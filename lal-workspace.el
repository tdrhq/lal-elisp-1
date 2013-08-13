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
   (extern-jars :initarg :extern-jars
                :initform '()
                :custom list
                :type list
                :documentation "list of jars that are used externally")))

(setf *current-workspace* nil)



              
              
    
   
        
