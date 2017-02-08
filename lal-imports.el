(provide 'lal-imports)

(require 'lal-strings)

(defmacro lal-run-for-lines-c (&rest f)
  "run a command for each line in the buffer"
  `(save-excursion
    (beginning-of-buffer)
    (while (not (equal (point) (point-max)))
      (progn ,@f)
      (forward-line))))

(defun lal-get-package ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "package ")
    (buffer-substring (point) (progn
                                (re-search-forward ";")
                                (backward-char)
                                (point)))))

(ert-deftest lal-get-package-test ()
  (with-temp-buffer
    (insert "package foo.bar;\n\n")
    (should (equal "foo.bar" (lal-get-package)))))


(defun lal-goto-first-import ()
  "goto the first line where we expect an import to be if there's no
  import go to the line after the package directive"
    (beginning-of-buffer)
    (re-search-forward "^package" nil t)
    (while (and
            (not (import-on-line))
            (not (eq (point-max) (point))))
      (forward-line))
    (when (eq (point-max) (point))
        (beginning-of-buffer)
      (re-search-forward "^package" nil t)
      (forward-line)))


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
    (when (equal (thing-at-point 'word) "import")
      (when (re-search-forward "import \\(.*\\);" nil t)
        (match-string 1)))))

(defun imports-in-buffer ()
  "get the imports in the current buffer"
  (interactive)
  (mapcar 'remove-root-from-import (let ((imports (remove  nil (lal-run-for-lines
                 (if (equal "import" (thing-at-point 'word))
                     (import-on-line))))))
    imports)))


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

(setq *lal:interesting-domains*
      '("javax" "java" ""))

(defun lal-interesting-domains ()
  (if (current-workspace)
      (interesting-domains (current-workspace))
    *lal:interesting-domains*))

(defun lal-domain-to-number (import)
  "Given a domain, tell me which numbered section it should be in"
  (number-to-string
   (+ 100
      (position t
                (mapcar (lambda (domain) (starts-with import domain)) (lal-interesting-domains) )))))

(defun add-a-numeric-prefix-for-domain (d)
  (let ((prefix
         (lal-domain-to-number d)))
    (concat prefix d)))

(defun import-on-next-line ()
  (save-excursion
    (forward-line)
    (import-on-line)))

(defun add-newlines-process-line ()
  (while (import-on-next-line)
    (when (not (equal
                (lal-domain-to-number (import-on-line))
                (lal-domain-to-number (import-on-next-line))))
      (forward-line)
      (insert-char ?\n 1))
    (forward-line)))
  ;; process the next line


(defun add-newlines-between-sections ()
  (interactive)
  "add a newline between each section of *lal:interesting-domains*"
  (save-excursion
    (lal-goto-first-import)
    (add-newlines-process-line)))

(defun lal-import-lessp (imp1 imp2)
  (string-lessp
   (add-a-numeric-prefix-for-domain imp1)
   (add-a-numeric-prefix-for-domain imp2)))

(defun lal-remove-multiple-empty-lines ()
  (if (and
       (eq ?\n (char-after))
       (eq ?\n (char-after (+ (point) 1))))
      (progn
        (delete-char 1)
        (lal-remove-multiple-empty-lines))))




(defun lal-reorder-imports ()
  (interactive)
  (save-excursion
    (lal-goto-first-import)
    (let ((old-imports (delete-dups (remove-if-not 'import-used-p (imports-in-buffer)))))
      (delete-all-imports)
      (mapc (lambda (val) (insert (concat "import " val ";\n")))
            (sort old-imports 'lal-import-lessp))

      (add-newlines-between-sections)
      (lal-remove-multiple-empty-lines)

      ;; if there's no newline before the first import, add it
      (lal-goto-first-import)
      (forward-line -1)
      (unless (eq (char-after) ?\n)
        (forward-line)
        (insert-char ?\n 1))
      )))




(defun symbol-from-import (import)
  (with-temp-buffer
    (insert import)
    (end-of-buffer)
    (re-search-backward "\\.")
    (forward-char)
    (thing-at-point 'word)))

(ert-deftest symbol-from-import-test ()
  (should (equal "FooBar" (symbol-from-import "com.foo.FooBar")))
  (should (equal "Foo" (symbol-from-import "com.foo.Bar.Foo"))))

(ert-deftest symbol-from-import-test-with-semicolon-and-space ()
  (should (equal "Foo" (symbol-from-import "com.foo.Foo ;"))))

(ert-deftest symbol-from-import-test-with-semicolon ()
  (should (equal "Foo" (symbol-from-import "com.foo.Foo;"))))


(defmacro with-buffer-copy (&rest body)
  `(lexical-let ((buffer-string-copy (buffer-string)))
     (with-temp-buffer
       (insert buffer-string-copy)
       (beginning-of-buffer)
       (progn ,@body))))

(defun lal-is-import-in-package (import package)
  (let ((lst (car (last (split-string import "\\.")))))
    (equal import (concat package "." lst))))

(ert-deftest lal-is-import-in-package-test ()
  (should (lal-is-import-in-package "a.b.C" "a.b"))
  (should (not (lal-is-import-in-package "a.b.C" "a"))))

(defun import-used-p (import)
  (if (lal-is-import-in-package import (lal-get-package))
      nil ;; not being used since it's from the current package
    (if (or
         (ends-with import "_")
         (ends-with import "}"))
        t
      ;; else see if the import symbol is used somewhere
      (save-excursion
        (with-buffer-copy
         (delete-all-imports)
         (beginning-of-buffer)
         (re-search-forward (concat "\\W" (symbol-from-import import) "\\W") nil t))))))

(defun lal-add-import (import)
  "add an import to the current file"
  (interactive "sPackage or import: ")
  (unless (and import (equal "" import))
    (save-excursion
      (lal-goto-first-import)
      (insert (concat "import " import ";\n")))
    (lal-reorder-imports)))
