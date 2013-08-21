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
                        :documentation
                        "each of this domains is stored into one separate section when ordering imports, in effect it's a style declaration")

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
               :documentation "internally maintained list/cache of all the files from the workspace that were kept open, it will be restored the next time the file is loaded")
   ))

(setf *current-workspace* nil)

(defmethod workspace-root ((ws workspace))
  (file-name-as-directory (oref ws :root)))

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
           (oset (current-workspace) :open-files old-files)
           
           ;; close all the buffers
           (mapc 'kill-buffer buffers)

           ;; the files we want to open are all the old files, plus the files that were 
           ;; already open in the other workspace

           (let* ((new-files (delete-dups (append old-files (oref ws :open-files)))))
             (mapc 'find-file 
                   (mapcar (lambda (x) (concat (workspace-root ws) x)) new-files)))

           ;; after all is done, let's setf the state
           (setf *current-workspace* ws)
           )))
    
    
   
        
