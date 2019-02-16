;;; Sinby Corp. 2019

(defpackage :cl-markdown-who
  (:use :cl :cl-who :cl-ppcre))

(in-package :cl-markdown-who)

(asdf:defsystem :cl-markdown-who
  :version "0.1"
  :depends-on (:cl-who :cl-ppcre)
  :serial t
  :components
   ((:file "package")
    (:file "parser")))
