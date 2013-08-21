
(require 'lal-test-setup)

(ert-deftest test-setup-basics ()
  (lal-delete-temp-project 
   (lal-create-temp-project)))

(ert-deftest lal-with-temp-project-test ()
  (with-temp-project
   (should t)
   (should t)))

(ert-deftest lal-touch-test ()
  (with-temp-project
   (lal-touch temp-project-dir "b/F.java")
   (should (file-exists-p (concat temp-project-dir "/b/F.java")))
   (lal-touch temp-project-dir "b/c/D.java")
   (should (file-exists-p (concat temp-project-dir "/b/c/D.java")))
   (lal-touch temp-project-dir "F.java")
   (should (file-exists-p (concat temp-project-dir "/F.java")))))
