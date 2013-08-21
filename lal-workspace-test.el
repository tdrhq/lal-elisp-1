
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
                       

  
