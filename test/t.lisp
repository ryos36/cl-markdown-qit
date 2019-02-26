#-:asdf (load "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
(require 'cl-ppcre)
(require 'cl-who)
(require 'cl-clasp)
(require 'cl-markdown-qit)

(print
  (with-open-file (in "/tmp/pp.txt")
    (cl-markdown-qit:interp-a-markdown in)))

;(print #'cl-markdown-qit:interp-a-markdown)
