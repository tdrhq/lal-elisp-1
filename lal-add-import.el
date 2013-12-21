
(provide 'lal-add-import)

(require 'lal-strings)
(require 'lal-imports)
(require 'memoize)

(defun workspace-rebuild-index/make-empty ()
  (interactive)
  (ede-set 'import-symbol-cache (makehash 'equal)))

(defun workspace-rebuild-index ()
  (interactive)
  "Unset all the caches, and do any rebuilding if required of symbol indexes"
  (workspace-rebuild-index/make-empty)
  (let ((hash import-symbol-cache))
    (mapc 'workspace-build-src-index (workspace-get-absolute-src-roots (ede-current-project)))
    (mapc 'workspace-build-jar-index (ede-java-classpath (ede-current-project)))))

(defun workspace-add-index-mapping (classname package)
  (puthash classname (cons
                      package
                      (gethash classname import-symbol-cache))
           import-symbol-cache))

(defun workspace-get-packages-for-class (classname)
  (unless (and (boundp 'import-symbol-cache) import-symbol-cache)
    (workspace-rebuild-index))
  (gethash classname import-symbol-cache))

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
(defun lal-filter-file-names-for-classname (classname file-list)
  (remove-if-not '(lambda (file-name)
                     (equal (file-name-nondirectory file-name)
                            (concat classname ".java")))
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




(defun lal-find-file-for-classname-in-dir (classname dir)
  (let ((dir-listing (noronha-dir-list-files dir)))
    (lal-filter-file-names-for-classname classname dir-listing)))

(ert-deftest lal-find-file-for-classname-in-dir ()
  (let ((fixtures (concat (lal-project-dir) "/fixtures")))
    (should (equal (list "One.java") (lal-find-file-for-classname-in-dir "One" fixtures)))
    (should (equal '() (lal-find-file-for-classname-in-dir "DoesNotExist" fixtures)))))

(defun lal-find-file-for-classname (classname)
  (remove-if-not 'identity
                 (append
                  (let* ((src-roots (workspace-get-absolute-src-roots (current-workspace))))
                    (mapcar
                     (lambda (x) (mapcar (lambda (y) (concat x "/" y)) (lal-find-file-for-classname-in-dir classname x)))
                     src-roots)))))

(setq lal-find-file-history ())
(defun lal-find-file-for-classname-interactive (classname)
  (interactive (list (read-string (format "Classname (%s): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
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

(defun noronha-jar-list (file)
  (message "listing jar %s" file)
  (mapcar 'noronha-get-canonical-package
          (remove-if '(lambda  (file) (string-match "/$" file))
                     (split-string (shell-command-to-string (concat "jar -tf " file))))))

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
