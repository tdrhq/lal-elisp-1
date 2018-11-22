(provide 'lal-buck-workspace)

(require 'lal-workspace)
(require 'lal-gradle-workspace)

(defclass buck-workspace (workspace)
  ((target-sdk :initarg :target-sdk)))

(cl-defun make-buck-workspace (name buck-dir &key (target-sdk "android-24"))
  (unless (file-exists-p buck-dir)
    (error "buck directory does not exist"))
  (let ((ret (buck-workspace name :directory buck-dir
                             :file (concat buck-dir "/.buckconfig")
                             :target-sdk target-sdk)))
    (oset ret :targets nil)
    (buck-update-workspace ret)
    ret))

(defun buck-update-workspace (&optional workspace)
  (let ((workspace (or workspace (ede-current-project))))
    (oset workspace :srcroot '("java/" "javatests/" "android_tests/"))
    (oset workspace :localclasspath
          (list
           (gradle-get-android-jar)))))

(oref (car ede-projects) :srcroot)
