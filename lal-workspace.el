;;; -*- lexical-binding: t -*-

(provide 'lal-workspace)

(require 'eieio)
(require 'lal-workspace-list)
(require 'ede/config)
;; (require 'ede/java-root)

(defclass workspace (ede-project ede-project-with-config-java)
  (
   ;; (name :initarg :name
   ;;       :initform ""
   ;;       :custom string
   ;;       :type string
   ;;       :documentation "the name of the workspace")
   (srcroot :initarg :srcroot
            :initform '())
   (localclasspath :initarg :localclasspath
                   :initform '())
   (classpath :initarg :classpath
              :accessor classpath
              :initform '())
   (interesting-domains :initarg :interesting-domains
                        :initform '()
                        :custom list
                        :type list
                        :accessor interesting-domains
                        :documentation  "each of this domains is stored into one separate section when ordering imports, in effect it's a style declaration")

   (open-files :initarg :open-files
               :initform '()
               :custom list
               :type list
               :accessor open-files
               :writer set-open-files
               :documentation "internally maintained list/cache of all the files from the workspace that were kept open, it will be restored the next time the file is loaded")
   (cache-map :initarg :cache-map
              :accessor cache-map
              :initform (make-hash-table))
   (last-compile-command  :accessor workspace-last-compile-command)
   ))

(defmethod workspace-set ((ws workspace) key value)
  (let ((map (oref ws :cache-map)))
    (puthash key value map)))

(defmethod workspace-get ((ws workspace) key)
  (let ((map (oref ws :cache-map)))
    (gethash key map)))

(defmethod workspace-root ((ws workspace))
  (file-name-as-directory (oref ws :directory)))

(cl-defmethod ede-find-subproject-for-directory ((proj workspace)
                                                 dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(cl-defmethod ede-project-root ((this workspace))
  this)

(cl-defmethod ede-project-root-directory ((this workspace))
  (oref this directory))

(defmethod workspace-set-root ((ws workspace) root)
  (oset ws :directory root))

(defmethod workspace-add-srcroot ((ws workspace) src)
  (oset ws :srcroot (cons src (oref ws :srcroot))))

(defun workspace-add-jar-impl (ws src)
  (oset ws :localclasspath (cons src (oref ws :localclasspath))))

(defun workspace-expand-wildcards (ws wildcard)
  (let ((default-directory (workspace-root ws)))
    (file-expand-wildcards wildcard)))

(defmethod workspace-add-jar ((ws workspace) src &optional wildcard)
  (workspace-add-jar-impl
   ws
   (if wildcard
       (let ((expansion (remove-if
                         (lambda (x) (string-suffix-p "sources.jar" x))
                         (workspace-expand-wildcards ws src))))
         (if (cdr expansion)
             (error "Too many matches for wildcard: %s" expansion)
           (car expansion)))
     (let ((default-directory (workspace-root ws)))
       (if (file-exists-p src)
           src
         (error "File %s does not exist" src))))))

(defun workspace-cleanup-jars (ws)
  (let ((jars (oref ws :localclasspath)))
    (oset ws :localclasspath
          (loop for jar in jars
                if (let ((default-directory (workspace-root ws)))
                     (file-exists-p jar))
                collect jar))))


(defmethod workspace-remove-jar ((ws workspace) src)
  (delete src (oref ws :localclasspath)))

(defmethod workspace-get-absolute-src-roots ((ws workspace))
  (mapcar 'expand-file-name
          (mapcar (lambda (x)
                    (concat (workspace-root ws) x "/"))
                  (oref ws :srcroot))))


(defun current-workspace ()
  "Deprecated wrapper over ede-current-project"
  (ede-current-project))

(defmethod workspace-get-buffers ((ws workspace))
  "Get all open buffers that we think belongs to the workspace"
  (remove-if-not
   '(lambda (a) (starts-with (buffer-file-name a) (workspace-root ws)))
   (buffer-list)))

(defmethod ede-java-classpath ((ws workspace))
  (oref ws :localclasspath))

(defmethod workspace-get-unsaved-buffers ((ws workspace))
   (remove-if-not 'buffer-modified-p (workspace-get-buffers ws)))


(defmethod workspace-switch ((ws workspace))
  "Switch the current workspace to ws along with all opened buffers of the old workspace"
  (if (workspace-get-unsaved-buffers (current-workspace))
      (message "There are unsaved buffers: %s" (workspace-get-unsaved-buffers (current-workspace)))
    ;; we've essentially decided we're good
    (let* ((buffers (workspace-get-buffers (current-workspace)))
           (old-files
            (mapcar
             (lambda (path) (substring (buffer-file-name path) (length (workspace-root (current-workspace)))))
             buffers)))
           (set-open-files (current-workspace) old-files)

           ;; close all the buffers
           (mapc 'kill-buffer buffers)

           ;; the files we want to open are all the old files, plus the files that were
           ;; already open in the other workspace

           (let* ((new-files (delete-dups (append old-files (open-files ws)))))
             (mapc 'find-file
                   (mapcar (lambda (x) (concat (workspace-root ws) x)) new-files)))
           )))

(defmethod workspace-is-file-managed ((ws workspace) file)
  t)


(defun noronha-flatten (list)
  (cond ((null list) nil)
        ((atom list) list)
        (t
         (let ((old list)
               (new ())
               item)
           (while old
             (if (atom old)             ; From consp with non-nil cdr.
                 (setq item old
                       old nil)
               (setq item (car old)
                     old (cdr old)))
             ;; Make item atomic.
             (while (consp item)
               (if (cdr item)
                   (setq old (cons (cdr item) old)))
               (setq item (car item)))
             (setq new (cons item new)))
           (reverse new)))))



(ert-deftest noronha-flatten-recursive ()
  (should (equal nil (noronha-flatten nil)))
  (should (equal (list nil) (noronha-flatten (list nil))))
  (should (equal nil (noronha-flatten '())))
  (noronha-flatten (loop for i from 1 to 1000 collect i)))


(defun noronha-dir-list-files (dir)
  (remove-if-not
   'identity
   (let ((dirs (noronha-flatten (list dir))))
     (noronha-flatten (mapcar (lambda (dir)
                                (mapcar (lambda (x) (substring x 2))
                                        (remove-if '(lambda  (file) (or (string-match "/$" file) (not (string-match ".java$" file))))
                                                   (split-string (shell-command-to-string (concat "cd " dir " && find ."))))))
                              dirs)))))

(ert-deftest noronha-dir-list-files ()
  (let ((fixtures (concat (lal-project-dir) "/fixtures")))
    (should (find "One.java" (noronha-dir-list-files fixtures) :test 'equal))
    (should (find "One.java" (noronha-dir-list-files (list fixtures)) :test 'equal))
    (should (find "One.java" (noronha-dir-list-files (list fixtures "/tmp")) :test 'equal))
    (should (equal nil (noronha-dir-list-files "/doesnotexist")))
    ))

(defun arnold-set-last-index (name newindent)
  (let ((a (assoc name c-offsets-alist)))
    (if (and (cdr a) (listp (cdr a)))
        (setcar (last (cdr a)) newindent)
      (setcdr a newindent))))

(defun  arnold-java-mode-hooks ()
  (interactive)
  (local-set-key (kbd "C-c h") 'search-android-docs)
  (arnold-set-last-index 'statement-cont '++)
  (arnold-set-last-index 'arglist-intro '++))

(add-hook 'java-mode-hook 'arnold-java-mode-hooks)

(defmethod project-compile-project ((ws workspace) &optional _command)
  (let ((last-compile-command (workspace-last-compile-command ws)))
    (when (and last-compile-command (equal last-compile-command ""))
      (setf last-compile-command nil))
    (let ((command (or _command
                       (compilation-read-command (or last-compile-command
                                                     (concat "cd " (oref ws :directory) " && "))))))
      (setf (workspace-last-compile-command ws) command)
      (compile command))))

(defun arnold-compile ()
  (interactive)
  (if (ede-current-project)
      (ede-compile-project)
    (compile)))

(global-set-key "\C-cc" 'arnold-compile)
