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

;----------------------------------------------------------------
; (list (list :current-line)) と同じだが
; 以後 assoc をつかうので 意味的に acons を使う
; もし :current-line が base-opt-lst にあれば重複するが
; 履歴を残すという意味でそのまま。assoc では手前が使われるので問題なし。
; 
(defun make-option-list ( &optional base-opt-lst )
  (acons :current-line nil (copy-tree base-opt-lst)))
