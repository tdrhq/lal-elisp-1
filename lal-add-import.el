
(provide 'lal-add-import)

(require 'lal-strings)
(require 'lal-imports)

;; sexy code from previous version
(defmacro noronha-with-if-not-changed-unmark-buffer (&rest body)
  `(let ((old-string (buffer-string))  (old-buffer-modified-p (buffer-modified-p)) )
     ,@body
     (if (equal old-string (buffer-string))
         (set-buffer-modified-p old-buffer-modified-p))))


(setq lal-safe-packages
      (list "com.google.inject.Inject"
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

(defun lal-add-import-s (tag)
  (interactive "sTag: ")
  "Import a 'known' package that has the same classname"
  (let* ((matches (lal-find-by-classname (upcase-initials tag)))
         (match (ido-completing-read "Choose import: " matches nil nil nil 'giit-history)))
    (lal-add-import match)))

;; find by classname
(defun lal-find-by-classname (classname)
   (lal-filter-imports-for-classname classname lal-safe-packages))



