(load-file "./test-common.el")

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

(ert-deftest lal-workspace-set-get-var ()
  (with-temp-project
   (let ((ws (workspace)))
     (workspace-set ws 'foo "bar")
     (should (equal "bar" (workspace-get ws 'foo))))))


(ert-deftest lal-workspace-root ()
  (let* (
         (makefile (make-temp-file "foo"))
         (ws (workspace "foo"
                        ;; :file makefile
                       :directory "/tmp/u")))
    (should (equal "/tmp/u/" (workspace-root ws)))))

;;(ert-deftest lal-can-access-root ()
;;  (let  ((ws (workspace nil :directory "/tmp/u")))
;;    (should (equal "/tmp/u/" (ede-project-root ws)))))

(ert-deftest lal-can-locate-project ()
  (let ((temp-project-dir (file-name-as-directory (expand-file-name "./fixtures"))))
    (message temp-project-dir)
    (let ((ws (workspace "foo" :directory temp-project-dir)) actual)
      (ede-add-project-to-global-list ws)
      (setq actual (ede-directory-get-open-project temp-project-dir))
      (should (not (equal nil (ede--inode-for-dir (concat temp-project-dir "ImportInPackage.java")))))
      (should (equal ws actual)))))

(ert-deftest lal-interesting-domains-test ()
  (let ((ws (workspace "foo" :interesting-domains '("java"))))
    (should (equal '("java") (interesting-domains ws)))))

(ert-run-tests-batch-and-exit)
