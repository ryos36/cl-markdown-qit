;;; Sinby Corp. 2019

(defpackage :cl-markdown-qit
  (:use :cl))

(in-package :cl-markdown-qit)

(asdf:defsystem :cl-markdown-qit
  :version "0.1"
  :depends-on (:cl-who :cl-ppcre)
  :serial t
  :components
   ((:file "package")
    (:file "utils")
    (:file "python-parser")

    (:file "parser")))
