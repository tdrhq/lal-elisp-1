(provide 'lal-package)

(require 'eieio)

(defclass lal-package ()
  ((name :initarg :name
         :custom string
         :type string
         :accessor lal-package-name
         :documentation "the name of the package")
   (exported-symbols :initarg :exported-symbols
            :custom list
            :initform nil
            :type list
            :accessor exported-symbols
            :documentation "a list of exported symbols stored in this")
   (symbols :initarg :symbols
            :initform nil
            :custom list
            :type list
            :accessor symbols
            :documentation "all symbols in this package")
   ))

(setf *f* (lal-package "dfd" :name "foo"))

(exported-symbols *f*)


(defmethod package-find-symbol ((package lal-package) sym-name)
  (let ((expected-sym (concat (lal-package-name package) "::" sym-name)))
    (car (remove-if-not 'identity (mapcar (lambda (s) 
                                            (when (equal (symbol-name s) expected-sym)
                                              s))
                                          (symbols package))))))

(defmethod package-intern ((package lal-package) sym-name)
  (if (package-find-symbol package sym-name)
      (progn 
        (message "fuck that")
        (package-find-symbol package sym-name))
    (let ((expected-sym (concat (lal-package-name package) "::" sym-name)))
      (message "in here")
      (oset package :symbols (cons (intern expected-sym) (symbols package)))
      (intern expected-sym))))

(symbols *f*)

(oref *f* :symbols)


(package-intern *f* "foo")
(package-intern *f* "bar")
(package-intern *f* "roo")

(package-find-symbol *f* "roor")

(symbols *f*)
