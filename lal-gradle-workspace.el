;;; -*- lexical-binding: t -*-

(provide 'lal-gradle-workspace)

(require 'lal-workspace)


(defclass gradle-workspace (workspace)
  ((target-sdk :initarg :target-sdk)))

(defun gradle-get-android-sdk ()
  (or
   (getenv "ANDROID_SDK")
   (getenv "ANDROID_HOME")))

(cl-defun make-gradle-project (name gradle-dir &key (target-sdk "android-24"))
  (let ((ret (gradle-workspace name :directory gradle-dir
                               :file (concat gradle-dir "/build.gradle")
                               :target-sdk target-sdk)))
    (oset ret :targets '())
    (gradle-update-workspace ret)
    ret))

(defun gradle-update-workspace (&optional workspace)
  (interactive)
  (let ((workspace (or workspace (ede-current-project))))
    (unless workspace
      (error "No workspace on current file"))
    (let ((gradle-dir (oref workspace :directory))
          (target-sdk (oref workspace :target-sdk)))
      (oset workspace :srcroot (gradle-get-src-roots gradle-dir))
      (oset workspace :localclasspath (cons
                                       (concat (gradle-get-android-sdk) "/platforms/" target-sdk "/android.jar")
                                       (gradle-build-classpath gradle-dir))))))

(defun gradle-read-sub-projects (gradle-dir)
  (with-temp-buffer
    (insert-file-contents (concat gradle-dir "/settings.gradle"))
    (goto-char 0)
    (loop while (re-search-forward "':\\([^,]*\\)'" nil t)
      collect (match-string 1))))

(defun gradle-get-src-roots (gradle-dir)
  (loop for sub-p in (gradle-read-sub-projects gradle-dir)
        append (list
            (concat sub-p "/src/main/java")
            (concat sub-p "/src/androidTest/java"))))


(defun get-subproject-dependencies (subproject-dir)
  (with-temp-buffer
    (insert-file-contents (concat subproject-dir "/build.gradle"))
    (goto-char 0)
    (loop for prefix in '("implementation" "androidTestCompile")
          append
          (loop while (re-search-forward (concat prefix " '\\(.*:.*:.*\\)'") nil t)
                collect
                (match-string 1)))))

(defun gradle-get-cache-location (maven-package)
  (let* ((parts (split-string maven-package ":"))
         (dir (concat (getenv "HOME")
                      "/.gradle/caches/modules-2/files-2.1/"
                      (first parts) "/"
                      (second parts) "/"
                      (third parts))))
    ;; good news is, doesn't look like there are aar files in
    ;; here. So find all possible assets, and pull the one that's
    ;; not a pom or sources
    (loop for jar in (append
                      (directory-files-recursively dir ".*.jar")
                      (directory-files-recursively dir ".*.aar"))
          if (not (string-suffix-p "-sources.jar" jar))
          return  jar)))

(defun gradle-build-classpath (gradle-dir)
  (remove-if-not
   'identity
   (loop for subproject in (gradle-read-sub-projects gradle-dir)
         append
         (loop for dependencies in (get-subproject-dependencies (concat gradle-dir "/" subproject))
               collect (gradle-get-cache-location dependencies)))))



(defmethod workspace-get-test ((ws gradle-workspace) filename)
  (if (string-match (concat ".*/androidTest/.*Test" (arnold-get-language-extension)) filename)
      (replace-regexp-in-string
       (concat "Test" (arnold-get-language-extension)) ".java"
       (replace-regexp-in-string "/androidTest/" "/main/" filename))

    ;; else
    (replace-regexp-in-string
     (concat "\\" (arnold-get-language-extension)) (concat "Test" (arnold-get-language-extension))
     (replace-regexp-in-string
      "/main/" "/androidTest/" filename))))

(defun goto-test ()
  (interactive)
  (let ((workspace (ede-current-project)))
    (find-file (workspace-get-test workspace (buffer-file-name)))))
