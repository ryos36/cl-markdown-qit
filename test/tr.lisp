#-:asdf (load "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
(with-output-to-string (*error-output*) 
  (require 'cl-ppcre)
  (require 'cl-who)
  (require 'cl-markdown-qit))

;(compile 'qiita:parse-python)

;(print `(:parse-python ,(qiita:parse-python "test/adpcm.py")))

(in-package :cl-markdown-qit)

(setf tlst '("abc" :nl "def" "abs"
             (:document-triple-single-quote . "doc abc")
             (:document-triple-single-quote "doc-xyz" "doc-rst" "doc-mno")
             (:document-triple-double-quote . "doc abc")
             (:document-triple-double-quote "dddoc-xyz" "dddoc-rst" "dddoc-mno")
             (:comment . "com abc")
             (:comment "cccom-xyz" "cccom-rst" "cccom-mno")
             (:nocomment . "no-cccom-xyz")
             (:nocomment "no-cccom-xyz" "no-cccom-rst" "no-cccom-mno")))

(print `(:python-tagged-list-to-who-style 
          ,(mapcar #'python-tagged-list-to-who-style tlst)))
