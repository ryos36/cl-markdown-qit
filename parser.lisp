(defparameter *tabstop* 4)

(defun |mw/*| (x stream) `(:h1 ,x))
(defun |mw/**| (x stream) `(:h2 ,x))
(defun |mw/***| (x stream) `(:h3 ,x))

(defun parse-decorations-for-lang (opt)
  (print `(:parse-decorations-for-lang ,opt))
  (let* ((optlst (cl-ppcre:split "\\s+" opt))
         (x (print `(:x ,opt ,optlst)))
         (lang-fname (cl-ppcre:split ":" (car optlst)))
         (lang (car lang-fname))
         (fname (cadr lang-fname)))
    (labels ((to-keyword (str)
               (let ((start-pos 
                       (if (char-equal (char str 0) #\:) 1 0)))
                 (intern (string-upcase (subseq str start-pos)) :keyword)))
             (take-two-by-two (lst rv)
               (if (null lst) rv
                 (let ((keyword (to-keyword (car lst)))
                       (value (cadr lst)))
                   (if (or (null value) (char-equal (char value 0) #\:))
                     (take-two-by-two (cdr lst)
                        (cons (list keyword t) rv))
                     (take-two-by-two (cddr lst)
                        (cons (list keyword value) rv))))))
             (enclose-list (cur-keyword cur-list remain rv)
                (if (null remain) (nreverse (cons (cons cur-keyword cur-list) rv))
                  (let ((next-key-value (car remain))
                        (next-remain (cdr remain)))
                    (let ((next-key (car next-key-value))
                          (next-value (cadr next-key-value)))
                    (if (eq cur-keyword next-key)
                      (enclose-list cur-keyword (cons next-value cur-list) next-remain rv)
                      (enclose-list next-key (list next-value) next-remain (cons (cons cur-keyword cur-list) rv)))))))
             (enclose-list0 (lst)
                (let ((first-item (car lst))
                      (remain (cdr lst)))
                  (enclose-list (car first-item) (cdr first-item) remain nil))))

      (print `(:two ,
          (enclose-list0 (sort
                    (take-two-by-two (cdr optlst) nil)
                    #'(lambda (a b) (string< (string (car a)) (string (car b))))))))
             
      (print `(:lang ,lang :filename ,fname))
      `((:lang ,lang) (:filename ,fname)
             ,@(enclose-list0 (sort
                    (take-two-by-two (cdr optlst) nil)
                    #'(lambda (a b) (string< (string (car a)) (string (car b))))))
             ))))

;----------------------------------------------------------------
(defun tab-to-space (line opt-lst)
  (let* ((opt-tabstop (cdr (assoc :tabstop opt-lst)))
         (tabstop-n (if opt-tabstop opt-tabstop *tabstop*))
         (replace-space (format nil "~V@{ ~}" tabstop-n :dummy)))
    (cl-ppcre:regex-replace-all "\\t" line replace-space)))

;----------------------------------------------------------------
;先頭から続くタブを空白に変換
(defun first-tab-to-space (line opt-lst)
  (multiple-value-bind (start-pos end-pos) (cl-ppcre:scan "^[\\s]*" line)
    (let ((first (subseq line start-pos end-pos))
          (remain (subseq line end-pos)))
      (if first
        (concatenate 'string (tab-to-space first opt-lst) remain)
        line))))

;----------------------------------------------------------------
(defun space-to-escaped-space (line)
    (cl-ppcre:regex-replace-all " " line "&#x20;"))

;----------------------------------------------------------------
(defun first-space-to-escaped-space (line)
  (multiple-value-bind (start-pos end-pos) (cl-ppcre:scan " *" line)
    (let ((first (subseq line start-pos end-pos))
          (remain (subseq line end-pos)))
      (if first
        (concatenate 'string (space-to-escaped-space first) remain)
        line))))

;----------------------------------------------------------------
(defun word-to-tagged-list (word opt-lst)
  (print `(:word-to-tagged-list ,word))
  word)

;----------------------------------------------------------------
(defun scan-strings-parser (line)
  (let ((first-char (char line 0)))

    (cl-ppcre:scan (format nil "[^\\\\]~a" first-char) line)))

;----------------------------------------------------------------
; Python 用なんちゃってパーザ
; 空白で区切る程度
; mode は :double-quote :quote :|triple-single-quote| :|triple-quote|

(defun python-line-parser (line opt-lst &optional rv)
  (if (or (null line) (= (length line) 0)) (values (nreverse rv) nil)
    (multiple-value-bind (sp-start-pos sp-end-pos)
        (cl-ppcre:scan "^\\s+" line)

      (if sp-start-pos
        (python-line-parser (subseq line sp-end-pos) opt-lst
              (cons (tab-to-space (subseq line sp-start-pos sp-end-pos) opt-lst) rv))

        (multiple-value-bind (pnum-start-pos pnum-end-pos)
            (cl-ppcre:scan "^\\d+" line)

          ;(print `(:plp ,line ,rv))
          (if pnum-start-pos 
            (python-line-parser (subseq line pnum-end-pos) opt-lst
                  (cons (subseq line pnum-start-pos pnum-end-pos) rv))

            (multiple-value-bind (w-start-pos w-end-pos)
                (cl-ppcre:scan "^\\w+" line)
          
                ;(print `(:plp2 ,line ,rv))
              (if w-start-pos 
                (python-line-parser (subseq line w-end-pos) opt-lst
                    (cons (word-to-tagged-list (subseq line w-start-pos w-end-pos) opt-lst) rv))

                (let ((line-len (length line))
                      (first-char (char line 0)))

                  (print `(:lf ,line-len ,first-char ,line))
                  (cond 
                    ((and (eq first-char #\")
                              (>= line-len 3)
                              (eq (char line 1) #\")
                              (eq (char line 2) #\"))

                     (values `((,line :|triple-quote|) ,@rv) :|triple-quote|))

                    ((and (eq first-char #\')
                              (>= line-len 3)
                              (eq (char line 1) #\')
                              (eq (char line 2) #\'))
                         (values `((,line :|triple-single-quote|) ,@rv) :|triple-single-quote|))

                    ((eq first-char #\#)
                     `((,line :comment) ,@rv))

                    ((eq first-char #\")
                     (if (= line-len 1)
                       (values `((,line :|id-double-quote|) ,@rv) :|id-double-quote|)
                       (multiple-value-bind (start0 end0)
                         (scan-strings-parser line)

                         (if end0
                           (python-line-parser 
                             (subseq line end0)
                             opt-lst
                             (cons `(,(subseq line 0 end0) :|id-double-quote|) rv))
                           (values `((,line :|id-double-quote|) ,@rv) :|id-double-quote|)))))

                    ((eq first-char #\')
                     (if (= line-len 1)
                       (values `((,line :|id-single-quote|) ,@rv) :|id-single-quote|)
                       (multiple-value-bind (start1 end1)
                         (scan-strings-parser line)
                         (if end0
                           (python-line-parser 
                             (subseq line end0)
                             opt-lst
                             (cons `(,(subseq line 0 end0) :|id-single-quote|) rv))
                           (values `((,line :|id-single-quote|) ,@rv) :|id-single-quote|)))))
                    (t (multiple-value-bind (start2 end2)
                                (cl-ppcre:scan "^[^\"'#\\s]+" line)
                              (print `(:line ,line ,start2, end2))
                              (python-line-parser (subseq line end2)
                                                  opt-lst
                                                  (cons 
                                                    (subseq line start2 end2)
                                                    rv)
                                                  )))))))))))))

;----------------------------------------------------------------
(defun text-line-to-div (line opt-lst)
  (print `(:text-line-to-div ,line))
  (first-space-to-escaped-space (first-tab-to-space line opt-lst)))

;----------------------------------------------------------------
(defun get-lang-option (opt-lst)
  opt-lst)

;----------------------------------------------------------------
(defun get-nolang-option (&options opt-lst)
  opt-lst)

;----------------------------------------------------------------
(defun |mw/```| (first-line-option stream) 
  (let ((decorate-option 
          (if (> (length first-line-option) 0)
            (get-lang-option
              (parse-decorations-for-lang first-line-option))
            (get-nolang-option))))

    (labels ((read-until-end-of-block (rv)
                (let ((line (read-line stream)))
                  (if (cl-ppcre:scan "^```[\\s]*" line) (nreverse rv)
                    (read-until-end-of-block (cons line rv))))))
      (print `(:|triple-single-quote| ,first-line-option ,decorate-option))
      (print  "decorate-option start")
      (map nil #'print (read-until-end-of-block nil))
      (print  "decorate-option END"))))

;----------------------------------------------------------------
(defun interp-a-markdown (stream)
  (let ((line (read-line stream)))
    (print `(:line ,line))
    (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^([^0-9a-zA-Z]*)[	 ]*(.*)" line)

      (let* ((flstv
               (map 'vector #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline) x)) regs))
             (jgeil (print `(:jgeil ,flstv ,regs)))
             (fname (intern (string-concat "mw/" (elt flstv 0))))
             (an-arg (elt flstv 1))
             (flst (list fname an-arg stream)))

        (print `(:fname ,fname))
        (if (fboundp fname)
          (progn
            (print `(:flst ,flst))
            (eval flst))
          (cadr flst))))))
