#!/usr/bin/emacs --script
;; runs all the tests in the project

(setq mydir (file-name-directory load-file-name))
(add-to-list 'load-path mydir)

(message "add-import")

(require 'lal-add-import)
(message "import")
(require 'lal-imports)

(message "workspace")
(require 'lal-workspace)

(defun safe-load (x)
  (unless (ends-with x "/test.el")
    (message (concat "Loading: " x))
    (load x)))
;; load all el files in the directory
(mapc 'safe-load
      (remove-if-not (lambda (x) (ends-with x ".el")) (directory-files mydir t)))

(ert-run-tests-batch)
