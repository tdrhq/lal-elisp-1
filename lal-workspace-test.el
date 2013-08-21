
(require 'lal-workspace)
(require 'lal-test-setup)

(ert-deftest lal-workspace-simple-creation ()
  (workspace "foo"
             :name "soo"))

(ert-deftest lal-workspace-absolute-src-roots-test ()
  (with-temp-project 
   (let ((ws (workspace "foo" 
                        :root "/tmp/u"
                        :src-roots '("a/b" "/"))))
     (should (equal 
              '("/tmp/u/a/b/" "/tmp/u/")
              (workspace-get-absolute-src-roots ws))))))


(ert-deftest lal-workspace-add-exter-jar-test ()
  (let ((ws (workspace "foo"
                       :root "/tmp/u")))
    (add-extern-jar ws "/tmp/a.jar")
    (should (equal '("/tmp/a.jar") (extern-jars ws)))))

(ert-deftest lal-workdpsace-root ()
  (let ((ws (workspace "foo"
                       :root "/tmp/u")))
    (should (equal "/tmp/u/" (workspace-root ws)))))
  
                       

  
(ert-deftest lal-interesting-domains-test ()
  (let ((ws (workspace "foo" :interesting-domains '("java"))))
    (should (equal '("java") (interesting-domains ws)))))
