(push :ryos *features*)
#+:ryos
(print `(cl-markdown-qit package mode))

#-:ryos
(print `(no package mode))

#+:ryos
(make-package :cl-markdown-qit)

#+:ryos
(in-package :cl-markdown-qit)

(defun inner-func (arg)
  (print `(:inner-func ,arg)))

;(defparameter package *package*)

(defun my-test (func-name)
  (let ((x*package* (find-package 'cl-markdown-qit)))
    (let* ((func-symbol 
             (if (symbolp func-name) func-name (intern (string-upcase func-name) (string-upcase "cl-markdown-qit"))))
           ;(f (symbol-function func-symbol))
           (flag (fboundp func-symbol)))
      (print `(:fboundp ,func-name ,func-symbol ,(fboundp func-symbol) ,(if flag "hit" "NOOOOOO"))))))


(defparameter *func* (symbol-function 'inner-func))
;(print `(:symbol-function ,*package* ,(symbol-function 'inner-func)))

#+:ryos
(export 'my-test)
;(export 'inner-func)
(export '*func*)

#+:ryos
(in-package "COMMON-LISP-USER")

(setf test-func-name "fboundp")
(setf test-func-name (string-upcase "cl-markdown-qit::inner-func"))
(setf test-func-name "inner-func")

#+:ryos
(cl-markdown-qit:my-test test-func-name)
#+:ryos
(cl-markdown-qit:my-test 'cl-markdown-qit::inner-func)

#-:ryos
(my-test test-func-name)

;(print `(:*func* ,cl-markdown-qit:*func*))
;(print `(:symbol-function ,(symbol-function 'cl-markdown-qit:inner-func)))
;(print `(:symbol-function ,(symbol-function 'cl-markdown-qit::inner-func)))
;(print `(:symbol-function ,'cl-markdown-qit::inner-func))
