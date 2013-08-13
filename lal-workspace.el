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


(defun current-workspace ()
  *current-workspace*)


              
              
    
   
        
