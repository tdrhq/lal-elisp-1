(provide 'lal-workspace)

(require 'eieio)

(defclass workspace () 
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
              :custom list
              :type list
              :documentation "list of all root java directories")
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
                :documentation "list of jars that are used externally")))

(setf *current-workspace* nil)

(defmethod workspace-root ((ws workspace))
  (file-name-as-directory (oref ws :root)))


(defun current-workspace ()
  *current-workspace*)

(defmethod add-extern-jar  ((ws workspace) jar)
  "Convenience method to manipulate the extern-jars field"
  (oset ws :extern-jars (cons jar (oref ws :extern-jars))))

(defmethod workspace-get-buffers ((ws workspace))
  "Get all open buffers that we think belongs to the workspace"
  (remove-if-not
   '(lambda (a) (starts-with (buffer-file-name a) (workspace-root ws)))
   (buffer-list)))

(defmethod workspace-get-unsaved-buffers ((ws workspace))
   (remove-if-not 'buffer-modified-p (workspace-get-buffers ws)))


(defmethod workspace-switch-if-required ((ws workspace) (old-ws workspace) buffer)
  "switch the given buffer to the new workspace if possible"
  (let (old-file-name (expand-file-name (buffer-file-name buffer)))
    ;; if the file is not from the workspace, skip it
    (unless (starts-with (workspace-root ws) old-file-name)
      ;; do stuff
      t
      )))
    
    

  
(defmethod workspace-switch ((ws workspace))
  "Switch the current workspace to ws along with all opened buffers of the old workspace"
  (mapcar (lambda (buffer) (workspace-switch-if-required ws buffer) (buffer-list))))
  


              
              
    
   
        
