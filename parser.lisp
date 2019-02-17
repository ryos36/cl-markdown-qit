
(defun |mw/*| (x) `(h1 ,x))
(defun |mw/**| (x) `(h2 ,x))
(defun |mw/***| (x) `(h3 ,x))

(defun one-markdown-to-func (strm)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^([^0-9a-zA-Z]*) *(.*)" line)

    (let* ((flstv
            (map 'vector #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline) x)) regs))
           (jgeil (print `(:jgeil ,flstv ,regs)))
           (fname (intern (string-concat "mw/" (elt flstv 0))))
           (an-arg (elt flstv 1))
           (flst (list fname an-arg)))

      (if (fboundp fname)
        (progn
          (print `(:flst ,flst))
          (eval flst))
        (cadr flst)))))

