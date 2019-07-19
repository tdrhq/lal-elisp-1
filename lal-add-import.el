
(provide 'lal-add-import)
(require 'cl)
(require 'eieio)
(require 'lal-strings)
(require 'lal-imports)
(require 'memoize)

(defun workspace-rebuild-index/make-empty ()
  (interactive)
  (workspace-set (ede-current-project) :src-files-listing nil)
  (let ((last-hash (workspace-get (ede-current-project) 'import-symbol-cache)))
    (unless last-hash
      (setq last-hash (workspace-set (ede-current-project) 'import-symbol-cache (make-hash-table :test 'equal))))
    (clrhash last-hash)))

(make-variable-buffer-local 'workspace-import-symbol-cache-cache)

(defun workspace-import-symbol-cache ()
  (if workspace-import-symbol-cache-cache
      workspace-import-symbol-cache-cache
    (setq workspace-import-symbol-cache-cache (workspace-get (ede-current-project) 'import-symbol-cache))
    workspace-import-symbol-cache-cache))

(defun workspace-dir-concat (a b)
  (if (string-prefix-p "/" b)
      b
    (concat a "/" b)))

(defun workspace-rebuild-index ()
  (interactive)
  "Unset all the caches, and do any rebuilding if required of symbol indexes"
  (workspace-rebuild-index/make-empty)
  (let ((hash (workspace-import-symbol-cache)))
    (mapc 'workspace-build-src-index (workspace-get-absolute-src-roots (ede-current-project)))
    (message "Going to build jar index")
    (let ((project-root (ede-project-root-directory (ede-current-project))))
      (loop for x in (ede-java-classpath (ede-current-project))
            do
            (message "Looking at '%s'" x)
            (workspace-build-jar-index (workspace-dir-concat project-root x))))))


(defun workspace-add-index-mapping (classname package)
  (puthash classname (cons
                      package
                      (gethash classname (workspace-import-symbol-cache)))
           (workspace-import-symbol-cache)))

(defun workspace-get-packages-for-class (classname)
  (unless (workspace-import-symbol-cache)
    (workspace-rebuild-index))
  (remove-if
   (lambda (x)
     (is-package-blacklisted
      (ede-current-project) x))
   (gethash classname (workspace-import-symbol-cache))))

(defmethod is-package-blacklisted (w package)
  (equal "junit.framework.Test" package))

(defun workspace-add-mapping-for-fqdn-class (classname)
  (workspace-add-index-mapping
   (lal-get-classname-from-import classname)
   classname))

(defun workspace-build-src-index (root)
  (mapc 'workspace-add-mapping-for-fqdn-class
        (noronha-dir-list root)))

(defun workspace-build-jar-index (jar)
  (message "Going through %s" jar)
  (mapc 'workspace-add-mapping-for-fqdn-class
        (noronha-jar-list jar)))


;; sexy code from previous version
(defmacro noronha-with-if-not-changed-unmark-buffer (&rest body)
  `(let ((old-string (buffer-string))  (old-buffer-modified-p (buffer-modified-p)) )
     ,@body
     (if (equal old-string (buffer-string))
         (set-buffer-modified-p old-buffer-modified-p))))

(defun lal-get-classname-from-import (import)
  (car (last (split-string import "\\."))))


;; same as above, but instead of packages, actual file names
(defun lal-filter-file-names-for-classname (classname-regex file-list)
  (remove-if-not
   (lambda (file-name)
     (string-match
       classname-regex
       (lal-file-name-sans-extension (file-name-nondirectory file-name))))
   file-list))

(defun lal-has-classname (file-name class-name)
  (equal class-name (lal-file-name-sans-extension (file-name-nondirectory file-name))))

(defun lal-trim-tag (tag)
  (replace-regexp-in-string "^@" "" tag))
(setq giit-history '())

(defun lal-add-import-s (tag)
  (interactive "sTag: ")
  "Import a 'known' package that has the same classname"
  (let ((tag (lal-trim-tag tag)))
    (let* ((matches (workspace-get-packages-for-class tag))
           (match (ido-completing-read "Choose import: " matches nil nil nil 'giit-history)))
      (lal-add-import match))))

(defun lal-add-import-t ()
  (interactive)
  (lal-add-import-s (current-word)))

(global-set-key "\C-ci" 'lal-add-import-t)

(defmacro setq-if-nil (sym val)
  `(when (not (boundp (quote ,sym)))
       (setq ,sym ,val)))

(setq-if-nil *lal-load-jars* ())

(defun lal-load-jars ()
  (if (current-workspace)
      (ede-java-classpath (ede-current-project))
    (*lal-load-jars*)))




(defun lal-find-file-for-classname-in-dir (classname-regex dir)
  (let ((dir-listing (noronha-dir-list-files dir)))
    (lal-filter-file-names-for-classname classname-regex dir-listing)))

(defun lal-project-dir ()
  (oref (ede-current-project) :directory))

(defun lal-find-file-for-classname (classname)
  (remove-if-not (lambda (x) (lal-has-classname x classname)) (lal-list-all-src-files)))

(defun lal-list-all-src-files (&optional default-workspace)
  (let* ((w (or default-workspace (current-workspace)))
         (cached-result (workspace-get w :src-files-listing)))
    (or
     cached-result
     (workspace-set w :src-files-listing
      (loop for x in (workspace-get-absolute-src-roots w)
           append (mapcar
                   (lambda (y) (concat x "/" y))
                   (noronha-dir-list-files x)))))))

(defun lal-find-file-for-classname-regex (classname-regex)
  (lal-filter-file-names-for-classname classname-regex (lal-list-all-src-files)))

(defun lal-list-classes (workspace)
  (let ((ret (loop for x in (lal-list-all-src-files workspace)
                   collect (lal-file-name-sans-extension (file-name-nondirectory x)))))
    (remove-duplicates-from-sorted (setf ret (sort ret 'string-lessp)))))

(defun remove-duplicates-from-sorted (l)
  (if (not (and l (cdr l)))
      (if (equal (car l) (car (cdr l)))
          (cdr l)
        (cons (car l) (remove-duplicates-from-sorted (cdr l))))
    l))


(defun lal-canonicalize-classname (classname)
  (if classname
      (let ((case-fold-search nil))
        (if (string-match "^m[A-Z]" classname)
            (lal-canonicalize-classname (substring classname 1))
          (if (string-match "[a-z]" classname)
              ;; capitalize the first letter
              (let ((cn (string-to-list classname)))
                (concat (cons (capitalize (first cn)) (rest cn)) )))))))


(defun lal-file-name-sans-extension (filename)
  ;; file-name-sans-extension is slow, this is a faster version that works for limited cases
  (let ((len (length filename)))
    (if (and (> len 5) (eq (aref filename (- len 5)) ?.))
        (substring filename 0 (- len 5))
      (file-name-sans-extension filename))))

(defun lal-classnames-for-classname-regex (regex)
  (let ((files (lal-find-file-for-classname-regex regex)))
    (mapcar
     (lambda (filename) (lal-file-name-sans-extension (file-name-nondirectory filename)))
     files)))

(setq  lal-read-classname-history ())
(defun lal-read-classname ()
  "Read a classname interactively and return it"
  (ido-completing-read
   "Classname: (%s)" ;; prompt
   (lal-list-classes (current-workspace)) ;; choices
   nil ;; predicate
   t   ;; require-match
   (lal-canonicalize-classname (thing-at-point 'word)))) ;; initial-input

;; (defun lal-list-all-basenames ((w workspace))
;;  lal-read-classname-cache


(setq lal-find-file-history ())
(defun lal-find-file-for-classname-interactive (classname)
  (interactive (list (lal-read-classname)))
  (find-file (ido-completing-read "Choose file: " (lal-find-file-for-classname classname) nil nil nil 'lal-find-file-history)))

(global-set-key "\C-cg" 'lal-find-file-for-classname-interactive)


(defun lal-src-to-import-hash (workspace)
  (or
   (import-tag-hash workspace)
   (set-import-tag-hash workspace
                        (let ((hash (makehash 'equal)))
                          ;; get the list of all files
                          (mapc
                           (lambda (file)
                             (let*
                                 ((import (lal-get-classname-from-import file))
                                  (oldhash (or (gethash import hash) '())))
                               (puthash import (cons file oldhash) hash)))
                           (let ((src-roots (workspace-get-absolute-src-roots workspace)))
                             (apply 'append (mapcar 'noronha-dir-list src-roots))))
                          hash))))


(defun lal-src-find-for-classname (classname)
  (gethash classname (or (lal-src-to-import-hash (current-workspace)) '())))




;; find by classname
(defun lal-find-by-classname (classname)
  (append
   (lal-filter-imports-for-classname classname lal-safe-packages)
   (lal-src-find-for-classname  classname)
   (lal-jars-find-for-classname  (lal-load-jars) classname)))



;; jar file parsers

(defun noronha-get-canonical-package (package)
  (subst-char-in-string
   ?/ ?.
   (dolist (suffix '(".class" ".java" ".kt") package)
     (when (string-suffix-p suffix package)
       (setf package (substring package 0 (- (length suffix))))))))

(defun mret (a)
  (message "got %s" a)
  a)

(defvar *noronha-archive-cache* nil)

(defun noronha-list-classes-in-archive (file)
  (flet ((get-uncached ()
                       (let ((prefix (concat "cat " file)))
                         (when (string-suffix-p ".aar" file)
                           (setf prefix (concat "unzip -p " file " classes.jar")))
                         (split-string (shell-command-to-string (concat prefix " | jar -t "))))))
    (let ((cached-element (assoc file *noronha-archive-cache* 'equal))
          (mod-time (file-attribute-modification-time (file-attributes file))))
      (cond
       ((equal mod-time (cadr cached-element))
        (caddr cached-element))
       (cached-element
        (setf (caddr cached-element) (get-uncached)))
       (t
        (let ((ret (get-uncached)))
          (push (list file mod-time ret) *noronha-archive-cache*)
          ret))))))

(defvar *noronha-jar-list-cache* nil)

(defun noronha-jar-list (file)
  (message "listing jar %s" file)
  (unless (file-exists-p file)
    (error "File %s does not exist" file))
  (loop for file in (noronha-list-classes-in-archive file)
        unless (string-suffix-p "/" file)
        collect (noronha-get-canonical-package file)))

(defun noronha-dir-list (dir)
  "Get a list of all top level classes in the given source directory"
  (message "listing dir %s" dir)
  (mapcar 'noronha-get-canonical-package (noronha-dir-list-files dir)))


(defun noronha-jars-list (file-list)
  (loop for x in file-list
        concatenate (noronha-jar-list x)))


;; A list of interesting jar files.
(defun lal-jar-find-for-classname (jar classname)
  (message "finding %s in %s" classname jar)
  (lal-jars-find-for-classname (list jar) classname))

(defun lal-jars-find-for-classname (jar-list classname)
  (lal-filter-imports-for-classname classname
                                    (noronha-jars-list jar-list)))

(defun find-src-root-for-filename (filename &optional absolute-roots)
  (let ((fn (file-truename (or filename (buffer-file-name))))
        (srcroots (or absolute-roots (workspace-get-absolute-src-roots (current-workspace)))))
    (loop for sroot in srcroots
          if (starts-with fn sroot)
          return (file-relative-name fn sroot))))

(defun lal-expected-package-name-from-buffername (&optional filename absolute-roots)
  (let ((final (find-src-root-for-filename (or filename (buffer-file-name)) absolute-roots)))

    (message "Final filename is %s" final)

    (if (not final)
        "could-not-find-package"
      (replace-regexp-in-string "\\.[a-zA-Z0-9]*\\(\\.java\\|\\.kt\\)$" "" (replace-regexp-in-string "[/]" "." final)))))
