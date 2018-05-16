;;; -*- lexical-binding: t -*-

(provide 'lal-gradle-workspace)

(require 'lal-workspace)


(defclass gradle-workspace (workspace)
  ())


(defun make-gradle-project (name gradle-dir)
  (let ((ret (gradle-workspace name :directory gradle-dir
                            :file (concat gradle-dir "/build.gradle"))))
    (gradle-update-workspace ret gradle-dir)
    (oset ret :targets '())
    ret))

(defun gradle-update-workspace (workspace gradle-dir)
  (oset workspace :srcroot (gradle-get-src-roots gradle-dir))
  (oset workspace :localclasspath (gradle-build-classpath gradle-dir)))

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
    (loop while (re-search-forward "implementation '\\(.*:.*:.*\\)'" nil t)
          collect
          (match-string 1))))

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
    (loop for jar in (directory-files-recursively dir ".*.jar")
          if (not (string-suffix-p "-sources.jar" jar))
          return (concat dir "/" jar))))

(defun gradle-build-classpath (gradle-dir)
  (remove-if-not
   'identity
   (loop for subproject in (gradle-read-sub-projects gradle-dir)
         append
         (loop for dependencies in (get-subproject-dependencies (concat gradle-dir "/" subproject))
               collect (gradle-get-cache-location dependencies)))))