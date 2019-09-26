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
  (interactive)
  (let ((workspace (or workspace (ede-current-project))))
    (oset workspace :srcroot '("java/" "javatests/" "android_tests/"))
    (oset workspace :localclasspath
          (cons
           (gradle-get-android-jar)
           (buck-read-third-party-jars (oref workspace :directory))))))

(defun buck-read-third-party-jars (dir)
  (directory-files-recursively (concat dir "/third-party") ".*.jar"))

(defmethod workspace-get-test ((ws buck-workspace) filename)
  (if (string-match (concat ".*/javatests/.*Test" (arnold-get-language-extension)) filename)
      (replace-regexp-in-string
       (concat "Test" (arnold-get-language-extension)) ".java"
       (replace-regexp-in-string "/javatests/" "/java/" filename))

    ;; else
    (replace-regexp-in-string
     (concat "\\" (arnold-get-language-extension)) (concat "Test" (arnold-get-language-extension))
     (replace-regexp-in-string
      "/java/" "/javatests/" filename))))

(defmethod workspace-get-test )

;; (message "%s" (oref *jippo-project* :localclasspath))
