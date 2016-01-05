
(provide 'lal-add-import)

(require 'lal-strings)
(require 'lal-imports)
(require 'memoize)

(defun workspace-rebuild-index/make-empty ()
  (interactive)
  (workspace-set (ede-current-project) 'import-symbol-cache (makehash 'equal)))

(make-variable-buffer-local 'workspace-import-symbol-cache-cache)

(defun workspace-import-symbol-cache ()
  (if workspace-import-symbol-cache-cache
      workspace-import-symbol-cache-cache
    (setq workspace-import-symbol-cache-cache (workspace-get (ede-current-project) 'import-symbol-cache))
    workspace-import-symbol-cache-cache))

(defun workspace-dir-concat (a b)
  (if (starts-with b "/")
      b
    (concat a "/" b)))xo

(defun workspace-rebuild-index ()
  (interactive)
  "Unset all the caches, and do any rebuilding if required of symbol indexes"
  (workspace-rebuild-index/make-empty)
  (let ((hash (workspace-import-symbol-cache)))
    (mapc 'workspace-build-src-index (workspace-get-absolute-src-roots (ede-current-project)))
    (message "Going to build jar index")
    (let ((project-root (ede-project-root-directory (ede-current-project))))
      (mapc 'workspace-build-jar-index
            (mapcar '(lambda (x) (workspace-dir-concat project-root x))
                  (ede-java-classpath (ede-current-project)))))))

(defun workspace-add-index-mapping (classname package)
  (puthash classname (cons
                      package
                      (gethash classname (workspace-import-symbol-cache)))
           (workspace-import-symbol-cache)))

(defun workspace-get-packages-for-class (classname)
  (unless (workspace-import-symbol-cache)
    (workspace-rebuild-index))
  (gethash classname (workspace-import-symbol-cache)))

(defun workspace-add-mapping-for-fqdn-class (classname)
  (workspace-add-index-mapping
   (lal-get-classname-from-import classname)
   classname))

(defun workspace-build-src-index (root)
  (mapc 'workspace-add-mapping-for-fqdn-class
        (noronha-dir-list root)))

(defun workspace-build-jar-index (jar)
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
   '(lambda (file-name)
      (string-match
       classname-regex
       (file-name-sans-extension (file-name-nondirectory file-name))))
   file-list))

(ert-deftest lal-filter-file-names-for-classname ()
  (let ((files (list "a/b/C.java" "a/C.java" "a/B.java")))
    (should (equal '() (lal-filter-file-names-for-classname "Fooey" files)))
    (should (equal '("a/B.java") (lal-filter-file-names-for-classname "B" files)))
    (should (equal '("a/b/C.java" "a/C.java") (lal-filter-file-names-for-classname "C" files)))))

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

(ert-deftest lal-find-file-for-classname-in-dir ()
  (let ((fixtures (concat (lal-project-dir) "/fixtures")))
    (should (equal (list "One.java") (lal-find-file-for-classname-in-dir "^One$" fixtures)))
    (should (equal '() (lal-find-file-for-classname-in-dir "^DoesNotExist$" fixtures)))))

(defun lal-find-file-for-classname (classname)
  (lal-find-file-for-classname-regex (concat "^" classname "$")))

(defun lal-find-file-for-classname-regex (classname-regex)
  (noronha-flatten
   (remove-if-not 'identity
                  (append
                   (let* ((src-roots (workspace-get-absolute-src-roots (current-workspace))))
                     (mapcar
                      (lambda (x) (mapcar (lambda (y) (concat x "/" y)) (lal-find-file-for-classname-in-dir classname-regex x)))
                     src-roots))))))

(defun lal-canonicalize-classname (classname)
  (if classname
      (let ((case-fold-search nil))
        (if (string-match "^m[A-Z]" classname)
            (lal-canonicalize-classname (substring classname 1))
          (if (string-match "[a-z]" classname)
              ;; capitalize the first letter
              (let ((cn (string-to-list classname)))
                (concat (cons (capitalize (first cn)) (rest cn)) )))))))

(ert-deftest lal-canonicalize-classname ()
  (should (equal nil (lal-canonicalize-classname nil)))
  (should (equal "ArnoldNor" (lal-canonicalize-classname "ArnoldNor")))
  (should (equal "ArnoldNor" (lal-canonicalize-classname "mArnoldNor")))
  (should (equal "ArnoldNor" (lal-canonicalize-classname "arnoldNor"))))


(defun lal-classnames-for-classname-regex (regex)
  (let ((files (lal-find-file-for-classname-regex regex)))
    (mapcar
     '(lambda (filename) (file-name-sans-extension (file-name-nondirectory filename)))
     files)))

(setq  lal-read-classname-history ())
(defun lal-read-classname ()
  "Read a classname interactively and return it"
  (if t
      (read-string "Classname: (%s)" (lal-canonicalize-classname (thing-at-point 'word)))
    (ede-apply-project-local-variables)
    (let ((current-classname (lal-canonicalize-classname (thing-at-point 'word)))
          (all-classes
           (or
          (looking-at )l-read-classname-cache
          (lal-classnames-for-classname-regex ".*"))))
    (ede-make-project-local-variable lal-read-classname-cache)
    (ede-set-project-local-variable lal-read-classname-cache all-classes)
    (ido-completing-read
     "Classname: " ;; prompt
     all-classes   ;; choices
     nil           ;; predicate
     nil           ;; require match
     current-classname ;; initial-input
     lal-read-classname-history))))


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
  (replace-regexp-in-string
   "/" "."
   (replace-regexp-in-string "\\.class$\\|\\.java$\\|/$" "" package)))

(defun mret (a)
  (message "got %s" a)
  a)

(defun noronha-jar-list (file)
  (message "listing jar %s" file)
  (unless (file-exists-p file)
    (error "File %s does not exist" file))
  (mapcar 'noronha-get-canonical-package
          (remove-if '(lambda  (file) (string-match "/$" file))
                     (mret (split-string (shell-command-to-string (concat "jar -tf " file)))))))

(defun noronha-dir-list (dir)
  "Get a list of all top level classes in the given source directory"
  (message "listing dir %s" dir)
  (mapcar 'noronha-get-canonical-package (noronha-dir-list-files dir)))


(defun noronha-jars-list (file-list)
  (apply 'nconc (mapcar 'noronha-jar-list file-list)))


;; A list of interesting jar files.
(defun lal-jar-find-for-classname (jar classname)
  (message "finding %s in %s" classname jar)
  (lal-jars-find-for-classname (list jar) classname))

(defun lal-jars-find-for-classname (jar-list classname)
  (lal-filter-imports-for-classname classname
                                    (noronha-jars-list jar-list)))

(defun lal-expected-package-name-from-buffername ()
  (let ((fn (buffer-file-name))
        (final nil)
        (srcroots (workspace-get-absolute-src-roots (current-workspace))))

    (mapc (lambda (sroot)
            (if (starts-with fn sroot)
                (setq final (file-relative-name fn sroot)))) srcroots)

    (replace-regexp-in-string "\\.[a-zA-Z0-9]*\\.java" "" (replace-regexp-in-string "[/]" "." final))))
