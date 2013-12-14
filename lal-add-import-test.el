
(defun lal-file-to-string (file)
  (with-temp-buffer
    (insert-file file)
    (buffer-string)))

(ert-deftest lal-add-import-test ()
  (with-temp-buffer 
    (insert-file "fixtures/One.java")
    (lal-add-import "com.foo.Foo")
    
    ;; verify that this is the same as the other buffer
    (should (equal (buffer-string) (lal-file-to-string "fixtures/OneResult.java")))))


(ert-deftest lal-reorder-import-with-from-package ()
  (with-temp-buffer 
    (insert-file "fixtures/ImportInPackage.java")
    (lal-reorder-imports)
    
    ;; verify that this is the same as the other buffer
    (should (equal (buffer-string) (lal-file-to-string "fixtures/ImportInPackageResult.java")))))
