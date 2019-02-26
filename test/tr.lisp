#-:asdf (load "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
(with-output-to-string (*error-output*) 
  (require 'cl-ppcre)
  (require 'cl-who)
  (require 'cl-markdown-qit))

(compile 'qiita:parse-python)

(print `(:parse-python ,(qiita:parse-python "test/adpcm.py")))

