(in-package :cl-markdown-qit)

;----------------------------------------------------------------
(defun python-continue-line-p (line)
  (let* ((line-len (length line))
         (last_1 (if (> line-len 1) (char line (- line-len 2))))
         (last (char line (- line-len 1))))
    (and (eq last #\\) (not (eq last_1 #\\)))))

;----------------------------------------------------------------
(defun python-string-quote (stream opt-lst rv0 quoted-char quoted-keyword)
  (labels ((parse-line (line)
             (multiple-value-bind (start end)
               (cl-ppcre:scan (format nil "[^\\\\]~a" quoted-char) line)
               (if end 
                 (let ((quoted-str-pair `(,(subseq line 0 end) . ,quoted-keyword))
                       (remain-str (subseq line end)))
                   (let ((has-remain-p (> (length remain-str) 0)))
                     (if has-remain-p
                       (push-back-line remain-str opt-lst))
                     (values quoted-str-pair (if (not has-remain-p) (list :nl)) :end)))
                 (values `(,line . ,quoted-keyword) 
                         (list :nl)
                         (if (python-continue-line-p line)
                         :continue
                         :error-end)))))

           (read-until-end-of-block (rv)
             (let ((line (nget-current-line stream opt-lst)))
               (if (eq line :eof) rv
                 (multiple-value-bind (one-rv nl end-parse)
                     (parse-line line)
                   (let* ((next-rv (cons one-rv rv))
                          (next-next-rv (if nl (cons nl next-rv) next-rv)))
                   (if (eq end-parse :continue)
                     (read-until-end-of-block next-next-rv)
                     next-next-rv)))))))

    (read-until-end-of-block rv0)))

;----------------------------------------------------------------
(defun python-string-double-quote (stream opt-lst &optional rv)
  (python-string-quote stream opt-lst #\" :string-double-quote))

;----------------------------------------------------------------
(defun python-string-single-quote (stream opt-lst &optional rv)
  (python-string-quote stream rv opt-lst #\' :string-single-quote))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defun python-document-quote (stream opt-lst rv0 quoted-str quoted-keyword)
  (labels ((parse-first-line(rv)
             (let ((line (nget-current-line stream opt-lst)))
               (multiple-value-bind (hit-str strv)
                 (cl-ppcre:scan-to-strings 
                   (format nil "(^\\s*)(~a)(.*)$" quoted-str) line)
                 (assert hit-str)
                 (cons 
                   `(,line . ,quoted-keyword)
                   rv))))

           (read-until-end-of-block (rv)
             (let ((line (nget-current-line stream opt-lst)))
               (if (eq line :eof) rv
                 (multiple-value-bind (hit-str strv)
                   (cl-ppcre:scan-to-strings 
                     (format nil "(^\\s*)(~a)(.*)$" quoted-str) line)
                   (if hit-str
                     (cons `(,line . ,quoted-keyword) rv)
                     (read-until-end-of-block
                       (cons `(,line . ,quoted-keyword) rv))))))))

    (read-until-end-of-block (parse-first-line rv0))))

;----------------------------------------------------------------
(defun python-document-triple-single-quote (stream opt-lst &optional rv)
  (python-document-quote stream opt-lst rv "'''" :document-triple-single-quote))
;----------------------------------------------------------------
(defun python-document-triple-double-quote (stream opt-lst &optional rv)
  (python-document-quote stream opt-lst rv "\"\"\"" :document-triple-double-quote))

