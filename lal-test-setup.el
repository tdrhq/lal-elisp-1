;; creates mechanisms for creating temporary directories for the
;; purpose of a test

(provide 'lal-test-setup)

(defun lal-create-temp-project ()
  (make-temp-file "emacsproject" t))

(defun lal-delete-temp-project (dir)
  (should (starts-with dir "/tmp")) ;; verify we're not deleting something major
  (delete-directory dir t))


    
