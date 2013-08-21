#!/usr/bin/emacs --script
;; runs all the tests in the project

(add-to-list 'load-path (file-name-directory load-file-name))

(message "add-import")

(require 'lal-add-import)
(message "import")
(require 'lal-imports)

(message "workspace")
(require 'lal-workspace)

(ert-run-tests-batch)
