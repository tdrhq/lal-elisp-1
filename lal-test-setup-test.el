
(require 'lal-test-setup)

(ert-deftest test-setup-basics ()
  (lal-delete-temp-project 
   (lal-create-temp-project)))

(ert-deftest lal-with-temp-project-test ()
  (with-temp-project
   (should t)
   (should t)))
