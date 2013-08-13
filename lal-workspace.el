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
           (oset (current-workspace) :open-files old-files))))
    
    

  

  


              
              
    
   
        
