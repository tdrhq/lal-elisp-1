(provide 'lal-strings)


;; from noronha-imports
(defun starts-with (str prefix)
  (and
   (<= (length prefix) (length str))
   (equal (substring str 0 (length prefix)) prefix)))


(ert-deftest starts-with-test ()
  (should (equal t (starts-with "foobar" "foo")))
  (should (equal t (starts-with "foo" "foo")))
  (should (equal t (starts-with "foo" "")))
  (should (equal t (starts-with "" "")))
  (should (equal nil (starts-with "foo" "bar")))
  (should (equal nil (starts-with "" "foo"))))
          
(defun reverse-string (str)
  (apply 'string (reverse (string-to-list str))))

(ert-deftest reverse-strin-test ()
  (should (equal "foo" (reverse-string "oof")))
  (should (equal "" (reverse-string "")))
  (should (equal "b" (reverse-string "b"))))

(defun ends-with (str prefix)
  (starts-with (reverse-string str) (reverse-string prefix)))

(ert-deftest ends-with-test ()
  (should (equal t (ends-with "foobar" "bar")))
  (should (equal t (ends-with "foo" "foo")))
  (should (equal t (ends-with "foo" "")))
  (should (equal t (ends-with "" "")))
  (should (equal nil (ends-with "foo" "bar")))
  (should (equal nil (ends-with "" "foo"))))


