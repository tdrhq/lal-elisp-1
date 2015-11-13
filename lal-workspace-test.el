
(require 'lal-workspace)
(require 'lal-test-setup)

(defun temp-makefile ()
  (make-temp-file "foo"))

(ert-deftest lal-workspace-simple-creation ()
  (workspace "foo"
             ;;             :file (temp-makefile)
             ))

(ert-deftest lal-workspace-absolute-src-roots-test ()
  (with-temp-project
   (let ((ws (workspace
              ;; :file (temp-makefile)
                        :directory "/tmp/u"
                        :srcroot '("a/b" "/"))))
     (should (equal
              '("/tmp/u/a/b/" "/tmp/u/")
              (workspace-get-absolute-src-roots ws))))))


(ert-deftest lal-workspace-root ()
  (let* (
         (makefile (make-temp-file "foo"))
         (ws (workspace "foo"
                        ;; :file makefile
                       :directory "/tmp/u")))
    (should (equal "/tmp/u/" (workspace-root ws)))))



(ert-deftest lal-interesting-domains-test ()
  (let ((ws (workspace "foo" :interesting-domains '("java"))))
    (should (equal '("java") (interesting-domains ws)))))
