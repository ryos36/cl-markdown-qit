(defun one-markdown-to-func (line)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^([^0-9a-zA-Z]*) +(.*)" line)

    (let* ((flst
            (map 'list #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline) x))))
           (fname (car flst)))
      (if (fboundp fname)
        (eval flst)
        (cadr flst)))))
