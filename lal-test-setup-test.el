
(require 'lal-test-setup)

(ert-deftest test-setup-basics ()
  (lal-delete-temp-project 
   (lal-create-temp-project ())))
