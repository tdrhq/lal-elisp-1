(provide 'lal-import)

(defmacro lal-run-for-lines-c (&rest f)
  "run a command for each line in the buffer"
  `(save-excursion
    (beginning-of-buffer)
    (while (not (equal (point) (point-max)))
      (progn ,@f)
      (forward-line))))

(defmacro lal-run-for-lines (&rest f)
  "run a command for each line in the buffer and collect the return values"
  `(let ((lal-return-val ())) 
     (lal-run-for-lines-c
      (push (progn ,@f) lal-return-val))
     lal-return-val))


(defun import-on-line ()
  "what's the import on the current line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "import \\(.*\\)")
    (match-string 1)))

(defun imports-in-buffer ()
  "get the imports in the current buffer"
  (interactive)
  (mapcar 'remove-root-from-import (let ((imports (remove  nil (lal-run-for-lines
                 (if (equal "import" (thing-at-point 'word))
                     (import-on-line))))))
    imports)))


;; from noronha-imports
(defun starts-with (str prefix)
  (and
   (< (length prefix) (length str))
   (equal (substring str 0 (length prefix)) prefix)))

(defun reverse-string (str)
  (apply 'string (reverse (string-to-list str))))

(defun ends-with (str prefix)
  (starts-with (reverse-string str) (reverse-string prefix)))

(defun remove-root-from-import (import)
  (if (starts-with import "_root_.")
      (substring import 7)
    import))

(defun go-to-last-import ()
  (interactive)
  (while (re-search-forward "^import .*" nil t)))
    
(defun delete-all-imports ()
  "delete all imports from the file"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^import .*")
    (beginning-of-line)
    (let ((start (point)))
      ;; find the last line that's an import
      (go-to-last-import)
      (delete-region start (point)) )))



(defun add-a-numeric-prefix-for-domain (d)
  (message d)
  (let ((prefix
         (cond
          ((starts-with d "com.phonegap.lal") "000")
          ((starts-with d "java.") "100")
          (t "001"))))
    (concat prefix d)))

(defun add-newline-after-phonegap ()
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "^import com.phonegap.lal.*" nil t))

    ;; be careful
    (beginning-of-line)
    (if (re-search-forward "^import com.phonegap.lal" nil t)
        (progn (end-of-line)
               (insert-char ?\n 1)))))

(defun add-newline-before-java ()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "^import java.*" nil t)
        (progn (beginning-of-line)
               (insert-char ?\n 1)))))

(defun lal-import-lessp (imp1 imp2)
  (string-lessp
   (add-a-numeric-prefix-for-domain imp1)
   (add-a-numeric-prefix-for-domain imp2)))

(defun lal-reorder-imports ()
  (interactive)
  (save-excursion
    (let ((old-imports (remove-if-not 'import-used-p (imports-in-buffer))))
      (delete-all-imports) 
      (mapc (lambda (val) (insert-string (concat "import " val "\n")))
            (sort old-imports 'lal-import-lessp))

      (add-newline-after-phonegap)
      (add-newline-before-java))))



(defun symbol-from-import (import)
  (with-temp-buffer
    (insert import)
    (end-of-buffer)
    (re-search-backward ".")
    (forward-char)
    (thing-at-point 'word)))


(defmacro with-buffer-copy (&rest body)
  `(lexical-let ((buffer-string-copy (buffer-string)))
     (with-temp-buffer
       (insert buffer-string-copy)
       (beginning-of-buffer)
       (progn ,@body))))

(defun import-used-p (import)
  (if (or
       (ends-with import "_")
       (ends-with import "}"))
      t
    ;; else see if the import symbol is used somewhere
    (save-excursion
      (with-buffer-copy
       (delete-all-imports)
       (beginning-of-buffer)
       (re-search-forward (concat "\\W" (symbol-from-import import) "\\W") nil t)))))

