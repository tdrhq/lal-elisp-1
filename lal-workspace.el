;;; -*- lexical-binding: t -*-

(provide 'lal-workspace)

(require 'eieio)
(require 'lal-workspace-list)

(defclass workspace (ede-project)
  ((name :initarg :name
         :initform ""
         :custom string
         :type string
         :documentation "the name of the workspace")
   (root :initarg :root
         :initform ""
         :custom string
         :type string
         :accessor root
         :documentation "the root dir of the worksapce")
   (src-roots :initarg :src-roots
              :initform '()
              :accessor src-roots
              :custom list
              :type list
              :documentation "list of all root java directories relative to the root directory")
   (interesting-domains :initarg :interesting-domains
                        :initform '()
                        :custom list
                        :type list
                        :accessor interesting-domains
                        :documentation  "each of this domains is stored into one separate section when ordering imports, in effect it's a style declaration")

   (extern-jars :initarg :extern-jars
                :initform '()
                :custom list
                :type list
                :accessor extern-jars
                :writer set-extern-jars
                :documentation "list of jars that are used externally")
   (open-files :initarg :open-files
               :initform '()
               :custom list
               :type list
               :accessor open-files
               :writer set-open-files
               :documentation "internally maintained list/cache of all the files from the workspace that were kept open, it will be restored the next time the file is loaded")

   (import-tag-hash :initform nil
                    :accessor import-tag-hash
                    :writer set-import-tag-hash
                    :documentation "a table of tags to list of filenames that satisify that tag")
   ))

(setf *current-workspace* nil)

(defmethod workspace-root ((ws workspace))
  (file-name-as-directory (root ws)))

(defmethod workspace-get-absolute-src-roots ((ws workspace))
  (mapcar 'expand-file-name 
          (mapcar (lambda (x)
                    (concat (workspace-root ws) x "/"))
                  (src-roots ws))))


(defun current-workspace ()
  *current-workspace*)

(defmethod add-extern-jar  ((ws workspace) jar)
  "Convenience method to manipulate the extern-jars field"
  (set-extern-jars ws (cons jar (extern-jars ws))))

(defmethod workspace-get-buffers ((ws workspace))
  "Get all open buffers that we think belongs to the workspace"
  (remove-if-not
   '(lambda (a) (starts-with (buffer-file-name a) (workspace-root ws)))
   (buffer-list)))

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

           ;; after all is done, let's setf the state
           (setf *current-workspace* ws)
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

  
   
        
