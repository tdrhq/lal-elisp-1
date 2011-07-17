(provide 'lal-strings)


;; from noronha-imports
(defun starts-with (str prefix)
  (and
   (< (length prefix) (length str))
   (equal (substring str 0 (length prefix)) prefix)))

(defun reverse-string (str)
  (apply 'string (reverse (string-to-list str))))

(defun ends-with (str prefix)
  (starts-with (reverse-string str) (reverse-string prefix)))

