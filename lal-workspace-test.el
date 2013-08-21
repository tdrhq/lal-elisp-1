
(require 'lal-workspace)
(require 'lal-test-setup)

(ert-deftest lal-workspace-simple-creation ()
  (workspace "foo"
             :name "soo"))
