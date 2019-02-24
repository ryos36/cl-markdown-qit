#-:asdf (load "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
(require 'cl-ppcre)
(require 'cl-who)
(defun mf (str) (multiple-value-bind (a b c d) (cl-ppcre:scan *pattern* str) `(,(subseq str a b) ,@(map 'list #'(lambda (x y) (subseq str x y)) c d))))
(defparameter *pattern* "^```([A-Za-z][^:]*):?([\\S]*)")
(mf "```python:hhh.py :hebereke")

(make-package 'cl-markdown-qit)
(load "parser.lisp")
(load "python-parser.lisp")

(in-package "CL-MARKDOWN-QIT")
#|
(lang-block-parser "```python:hhh.py :hebereke" nil)
(lang-block-parser "```python:sample.py" nil)
(lang-block-parser "```c:sample.c" nil)
(lang-block-parser "```c++:sample.cpp" nil)
(lang-block-parser "```common lisp:hebereke-dayo.lisp" nil)
(lang-block-parser "```common lisp:" nil)
|#

(defun nl () (string #\Newline))

#|
(print `(:new-markdown-stream 
          ,(with-input-from-string (in 
                                     (concatenate 'string
                                       "nandaroune" (nl)
                                       "hello" (nl)
                                       (nl)
                                       "nandarone" (nl)
                                       "good" " " "by"(nl)
                                       ))
             (new-markdown-stream in))))

(print :==============================================================)
(print `(:new-markdown ,(new-markdown "sect.txt" :tag-option '(:a))))
(print :==============================================================)
(print `(:new-markdown ,(new-markdown "test.txt" :tag-option '(:b))))

(with-input-from-string (in 
                            (concatenate 'string
                                 "def __init(self):" (nl)
                                 "    xpass" (nl)
                                 "    ypass" (nl)
                                 (nl)
                                 "abc = f" (nl)
                                 "good" " " "by"(nl)
                                 ))
  (labels ((do-it ()
            (let ((line (read-line in nil :eof)))
              (if (eq line :eof) t
                (progn
                  (print `(:line ,line))
                  (do-it))))))
    (do-it)))

(print `(:python 
          ,(with-input-from-string (in 
                            (concatenate 'string
                                 "def __init(self):" (nl)
                                 "    xpass" (nl)
                                 "    ypass" (nl)
                                 (nl)
                                 "abc = f" (nl)
                                 "good" " " "by"(nl)
                                 ))
             (python-parser in '((:current-line))))))

(setf test-list `(,(concatenate 'string
                                 "'def '__init(self):" (nl)
                                 )
             ,(concatenate 'string
                                 "'def '" (nl)
                                 )
             ,(concatenate 'string
                                 "'def " (nl)
                                 "xxx'def " (nl)
                                 )
             ,(concatenate 'string
                                 "'def \\" (nl)
                                 "xxx'def " (nl)
                                 )
             ,(concatenate 'string
                                 "'def \\" (nl)
                                 "yyy'" (nl)
                                 )))

(dolist (i test-list)
  (print `(:i ,i
           :python 
            ,(with-input-from-string (in i)
               (python-string-single-quote in nil (list (list :current-line)))))))

(setf test-list `(,(concatenate 'string
                                 "   '''" (nl)
                                 "   arere" (nl)
                                 "   '''" (nl)
                                 )
             ,(concatenate 'string
                                 "   '''" (nl)
                                 )
             ,(concatenate 'string
                                 "   '''" (nl)
                                 "   arere" (nl)
                                 )
             ,(concatenate 'string
                                 "   '''here" (nl)
                                 "   arere" (nl)
                                 "   '''" (nl)
                                 )
             ,(concatenate 'string
                                 "   '''here" (nl)
                                 "   arere" (nl)
                                 "   '''hhh" (nl)
                                 )
             ,(concatenate 'string
                                 "    	 '''here" (nl)
                                 "  arere" (nl)
                                 "    '''hhh" (nl)
                                 )))

(dolist (i test-list)
  (print `(:i ,i
           :python 
            ,(with-input-from-string (in i)
               (python-document-triple-single-quote in nil (list (list :current-line)))))))
|#

(print `(:python-lne-parser 
          ,(with-input-from-string (in 
                                     (concatenate 'string
                                       "def abc(x):" (nl)
                                       "   return x" (nl)
                                       (nl)
                                       "nandarone=abc(3)" (nl)
                                       "print('nandesuka:', nandesuka)" (nl)
                                       ))
             (python-lne-parser in (list (list :current-line))))))
