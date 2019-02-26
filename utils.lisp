(in-package :cl-markdown-qit)

;----------------------------------------------------------------
(defun make-keyword (str)
  (if (string= str "common lisp") :common-lisp
    (intern (string-upcase str) :keyword)))

;----------------------------------------------------------------
(defun read-file-as-list (file-name)
  (with-open-file (in 
                    (asdf:system-relative-pathname :cl-markdown-qit file-name)
                    :direction :input)
    (labels ((read-all (rv)
              (let ((line (read-line in nil :eof)))
                (if (eq line :eof) (nreverse rv)
                  (read-all (push line rv))))))
      (read-all nil))))
