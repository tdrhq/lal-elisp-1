(provide 'lal-commit-editmsg)

(defun commit-edit-message-hook ()
  (when (equal (buffer-name) "COMMIT_EDITMSG")
    (message "we're int commit buffer")
    (goto-char (point-min))
    (insert-char ?\n 1)
    (goto-char (point-min))))


(add-hook 'find-file-hook 'commit-edit-message-hook t)
