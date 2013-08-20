(provide 'lal-workspace-list)

(require 'cl)

;; manages list of workspaces

(setf *lal:workspace-list* nil)

(defun lal-add-workspace (workspace)
  (add-to-list '*lal:workspace-list* workspace))

(defun lal-workspace-as-strings ()
  (mapcar (lambda (ws) (oref ws :name))
          *lal:workspace-list*))

(defun lal-workspace-by-name (name)
  (car (remove-if-not (lambda (ws)
                   (equal name (oref ws :name)))
                 *lal:workspace-list*)))

(lal-workspace-as-strings)
(lal-workspace-by-name "second")


(defun lal-switch-workspace ()
  (interactive)
  "switch between workspaces"
  (let ((chosen (ido-completing-read "Select workspace: " (lal-workspace-as-strings))))
    (workspace-switch (lal-workspace-by-name chosen))))

(defun workspace-compile (cmd)
  (interactive "sCommand: ")
  "runs a compilation in the given workspace"
  (compile (concat "cd " (workspace-root (current-workspace)) " && " cmd)))
  
                       
  
  

