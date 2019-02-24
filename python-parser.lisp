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

    (print `(:python-string-single-quote ,opt-lst))
    (read-until-end-of-block rv0)))

;----------------------------------------------------------------
(defun python-string-double-quote (stream opt-lst &optional rv)
  (python-string-quote stream opt-lst rv #\" :string-double-quote))

;----------------------------------------------------------------
(defun python-string-single-quote (stream opt-lst &optional rv)
  (python-string-quote stream opt-lst rv #\' :string-single-quote))

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

;----------------------------------------------------------------
(defun python-document-triple-X-quote (stream opt-lst char-X &optional rv)
  (if (eq char-X #\') 
    (python-document-triple-single-quote stream opt-lst rv)
    (python-document-triple-double-quote stream opt-lst rv)))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defun python-line-parser (stream opt-lst &optional rv)
  (labels ((parse-one (parse-str line rv)
             (print `(:parse-one ,parse-str ,line))
             (multiple-value-bind (start end) 
                 (cl-ppcre:scan parse-str line)
               (print `(:start ,start :end ,end))
               (if (not end) (values line rv)
                 (let ((hit-str (subseq line start end))
                       (remain (if (= end (length line)) nil
                                 (subseq line end))))
                   (values remain (cons hit-str rv))))))

           (parse-line-nl (line rv)
              (if (= (length line) 0)
                (values nil (cons (list :nl) rv))
                (parse-line-space line rv)))

           (parse-line-space (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^\\s+" line rv)
                (if (null line0) (values nil (cons (list :nl) rv))
                  (if (and (>= (length line0) 3)
                           (let ((first-3-char (subseq line 0 3)))
                             (or (string-equal "'''" first-3-char)
                                 (string-equal "\"\"\"" first-3-char))))
                    (values list rv)

                    (parse-line-digit line0 rv0)))))

           (parse-line-digit (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^\\d+" line rv)
                (print `(:parse-line-digit ,line0 ,rv0))
                (if (null line0) (values nil (cons (list :nl) rv))
                  (parse-line-word line0 rv0))))

           (parse-line-word (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^\\w+" line rv)
                (print `(:parse-line-word ,line0 ,rv0))
                (if (null line0) (values nil (cons (list :nl) rv))
                  (parse-line-quote line0 rv0))))

           (parse-line-quote (line rv)
              (let ((first-char (char line 0)))
                (if (or (eq first-char #\")
                        (eq first-char #\'))
                  (values line rv)
                  (parse-line-comment line rv))))

           (parse-line-comment (line rv)
              (if (eq (char line 0) #\#)
                (values nil (cons (list :nl)
                                  (cons `(,list :comment) rv)))
                (parse-line-others line rv)))

           (parse-line-others (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^[^\"'#\\s]+" line rv)
                (if (null line0) (values nil (cons (list :nl) rv))
                  (parse-line-space line0 rv0)))))

    (let ((line (nget-current-line stream opt-lst)))
      (print `(:line ,line))
      (multiple-value-bind (remain updated-rv)
          (parse-line-nl line rv)
        (print `(:remain ,remain ,opt-lst))
        (if remain
          (push-back-line remain opt-lst))
        (let ((updated-updated-rv
                (if remain
                  (case (char remain 0)
                    (#\" (python-string-double-quote stream opt-lst updated-rv))
                    (#\' (python-string-single-quote stream opt-lst updated-rv))
                    (otherwise
                      (assert (cl-ppcre:scan "^\\s[\"']{3}" remain))
                      (let ((char-x (char remain 3)))
                        (python-document-triple-X-quote stream opt-lst char-x rv)))) updated-rv )))
          (let ((current-line (cdr (assoc :current-line opt-lst))))
            (if current-line
              (python-line-parser stream opt-lst updated-updated-rv)
              updated-updated-rv)))))))


;----------------------------------------------------------------
;----------------------------------------------------------------
