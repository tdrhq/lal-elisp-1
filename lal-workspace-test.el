
(require 'lal-workspace)
(require 'lal-test-setup)

(defun temp-makefile ()
  (make-temp-file "foo"))

(ert-deftest lal-workspace-simple-creation ()
  (workspace "foo"
             :file (temp-makefile)
             :name "soo"))

(ert-deftest lal-workspace-absolute-src-roots-test ()
  (with-temp-project
   (let ((ws (workspace "foo"
                        :file (temp-makefile)
                        :root "/tmp/u"
                        :srcroot '("a/b" "/"))))
     (should (equal
              '("/tmp/u/a/b/" "/tmp/u/")
              (workspace-get-absolute-src-roots ws))))))


(ert-deftest lal-workspace-add-exter-jar-test ()
  (let ((ws (workspace "foo"
                       :file (temp-makefile)
                       :root "/tmp/u")))
    (add-extern-jar ws "/tmp/a.jar")
    (should (equal '("/tmp/a.jar") (extern-jars ws)))))


(ert-deftest lal-workspace-root ()
  (let* (
         (makefile (make-temp-file "foo"))
         (ws (workspace "foo"
                       :file makefile
                       :root "/tmp/u")))
    (should (equal "/tmp/u/" (workspace-root ws)))))



(ert-deftest lal-interesting-domains-test ()
  (let ((ws (workspace "foo" :file (temp-makefile) :interesting-domains '("java"))))
    (should (equal '("java") (interesting-domains ws)))))
