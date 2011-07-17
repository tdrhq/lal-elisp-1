
(provide 'lal-add-import)

(require 'lal-strings)
(require 'lal-imports)
(require 'memoize)

;; sexy code from previous version
(defmacro noronha-with-if-not-changed-unmark-buffer (&rest body)
  `(let ((old-string (buffer-string))  (old-buffer-modified-p (buffer-modified-p)) )
     ,@body
     (if (equal old-string (buffer-string))
         (set-buffer-modified-p old-buffer-modified-p))))


(setq lal-safe-packages
      (list "com.google.inject.Inject"
            "com.google.inject.Provider"
       "java.util.List"
       "java.util.Set"
       "java.util.EnumSet"
       "java.util.Comparator"
       "java.util.SortedSet"
       "java.util.Locale"
       "java.lang.IllegalStateException"
       "java.util.Arrays"))


(defun lal-filter-imports-for-classname (classname imports)
  "filter the given imports, for imports that have the given classname"
  (remove-if-not '(lambda (import)
                    (equal (car (last (split-string import "\\.")))
                           classname))
                 imports))

;; same as above, but instead of packages, actual file names
(defun lal-filter-file-names-for-classname (classname file-list)
  (remove-if-not '(lambda (file-name)
                     (equal (file-name-nondirectory file-name)
                            (concat classname ".java")))
                 file-list))

(defun lal-trim-tag (tag)
  (replace-regexp-in-string "^@" "" tag))

(defun lal-add-import-s (tag)
  (interactive "sTag: ")
  "Import a 'known' package that has the same classname"
  (let ((tag (lal-trim-tag tag)))
    (let* ((matches (lal-find-by-classname (upcase-initials tag)))
           (match (ido-completing-read "Choose import: " matches nil nil nil 'giit-history)))
      (lal-add-import match))))

(defun lal-add-import-t ()
  (interactive)
  (lal-add-import-s (current-word)))

(global-set-key "\C-ci" 'lal-add-import-t)

(setq-if-nil lal-android-jar "/home/opt/android/platforms/android-10/android.jar")

;; A list of load jars
(setq-if-nil lal-load-jars ())

;; find by classname
(defun lal-find-by-classname (classname)
  (or 
   (lal-filter-imports-for-classname classname lal-safe-packages)
   (lal-jar-find-for-classname lal-android-jar classname)
   (lal-jars-find-for-classname  lal-load-jars classname)))


(defmacro setq-if-nil (sym val)
  `(when (not (boundp (quote ,sym)))
       (setq ,sym ,val)))
  



;; jar file parsers

(defun noronha-get-canonical-package (package)
  (replace-regexp-in-string
                      "/" "."
                      (replace-regexp-in-string "\\.class$\\|\\.java$\\|.*java/+\\|/$" "" package)))

(defun noronha-jar-list (file)
  (message "listing jar %s" file)
  (mapcar 'noronha-get-canonical-package
          (remove-if '(lambda  (file) (string-match "/$" file))
                     (split-string (shell-command-to-string (concat "jar -tf " file))))))
(memoize 'noronha-jar-list)

(defun noronha-jars-list (file-list)
  (apply 'nconc (mapcar 'noronha-jar-list file-list)))


;; A list of interesting jar files.
(defun lal-jar-find-for-classname (jar classname)
  (message "finding %s in %s" classname jar)
  (lal-jars-find-for-classname (list jar) classname))

(defun lal-jars-find-for-classname (jar-list classname)
  (lal-filter-imports-for-classname classname
                                    (noronha-jars-list jar-list)))

(noronha-jars-list lal-load-jars)