;; creates mechanisms for creating temporary directories for the
;; purpose of a test

(provide 'lal-test-setup)

(defun validate-dir (dir)
  (should (starts-with dir "/tmp"))) ;; verify we're not deleting something major

(defun lal-create-temp-project ()
  (make-temp-file "emacsproject" t))

(defun lal-delete-temp-project (dir)
  (validate-dir dir)
  (delete-directory dir t))

(defmacro with-temp-project (&rest body)
  `(let ((tmp (lal-create-temp-project)))
     (unwind-protect (progn ,@body)
       (lal-delete-temp-project tmp))))
  

;; touch a filename
(defun lal-touch (dir file)
  (validate-dir)
  (let ((abs (concat dir "/" file)))
    (write-region "" nil abs)))


    
