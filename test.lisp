#-:asdf (load "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
(require 'cl-ppcre)
(require 'cl-who)
(defun l () (load "test.lisp"))

(load "parser.lisp")
(print
  (with-open-file (in "test.txt")
    (interp-a-markdown in)))

(setf test-lst
  '("abc"
    "   " 
    "123" 
    "   abc" 
    "   abc 123" 
    "123 abc" 
    "abc = \"123\"" 
    "xxx abc = \"123\"" 
    "def abc = \"12a\\\"def\\\"3\"" ))

(setf tag-lst
  '((:keyword . ("def"))))

(setf count 0)
(dolist (i test-lst)

  (print `(,count ,i "=>" ,(python-line-parser i tag-lst)))
  (incf count)
  )

