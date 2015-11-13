
;; creates mechanisms for creating temporary directories for the
;; purpose of a test

(provide 'lal-test-setup)
(global-ede-mode 1)

(defun lal-project-dir ()
  "Get the project dir to use for this"
  (if load-file-name
      (file-name-directory load-file-name)
    "/home/arnold/builds/lal-elisp"))

(defun validate-dir (dir)
  (should (starts-with dir "/tmp"))) ;; verify we're not deleting something major

(defun lal-create-temp-project ()
  (make-temp-file "emacsproject" t))

(defun lal-delete-temp-project (dir)
  (validate-dir dir)
  (delete-directory dir t))

(defmacro with-temp-project (&rest body)
  `(let ((temp-project-dir (lal-create-temp-project)))
     (unwind-protect (progn ,@body)
       (lal-delete-temp-project temp-project-dir))))


;; touch a filename
(defun lal-touch (dir file)
  (validate-dir dir)
  (let ((abs (concat dir "/" file)))
    ;; create all parent directories
    (make-directory (file-name-directory abs) t)
    (write-region "" nil abs)
    abs))
