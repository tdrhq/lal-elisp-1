(load-file "test-common.el")
(require 'lal-add-import)

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

;;(ert-deftest lal-find-file-for-classname-in-dir ()
;;  (let ((fixtures (concat (lal-project-dir) "/fixtures")))
;;    (should (equal (list "One.java") (lal-find-file-for-classname-in-dir "^One$" fixtures)))
;;    (should (equal '() (lal-find-file-for-classname-in-dir "^DoesNotExist$" fixtures)))))

(ert-deftest lal-canonicalize-classname ()
  (should (equal nil (lal-canonicalize-classname nil)))
  (should (equal "ArnoldNor" (lal-canonicalize-classname "ArnoldNor")))
  (should (equal "ArnoldNor" (lal-canonicalize-classname "mArnoldNor")))
  (should (equal "ArnoldNor" (lal-canonicalize-classname "arnoldNor"))))

(ert-deftest lal-filter-file-names-for-classname ()
  (let ((files (list "a/b/C.java" "a/C.java" "a/B.java")))
    (should (equal '() (lal-filter-file-names-for-classname "Fooey" files)))
    (should (equal '("a/B.java") (lal-filter-file-names-for-classname "B" files)))
    (should (equal '("a/b/C.java" "a/C.java") (lal-filter-file-names-for-classname "C" files)))))


(ert-deftest lal-expected-package-name-from-buffername ()
  (should (equal "com.foo.bar" (lal-expected-package-name-from-buffername "/androidTest/com/foo/bar/Blah.java" '("/androidTest")))))

(ert-run-tests-batch-and-exit)
