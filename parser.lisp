(defparameter *tabstop* 4)
(defparameter *lang-set*
  '((:pre-set
      (:name . :pre-set)
      (:parser . python-line-parser)
      (:word-split . python-word-split)
      (:keyword . first-space-to-escaped-space))
    (:python 
      (:name . :python)
      (:parser . python-line-parser)
      (:word-split . python-word-split)
      (:tag-to-span . python-tag-to-span)
      (:mode-set (:|triple-single-quote| . python-triple-single-quote)
                 (:|triple-double-quote| . python-triple-double-quote)))))

(defun make-keyword (str)
  (intern (string-upcase str) :keyword))

(defun |mw/*| (x stream) `(:h1 ,x))
(defun |mw/**| (x stream) `(:h2 ,x))
(defun |mw/***| (x stream) `(:h3 ,x))

(defun parse-decorations-for-lang (opt)
  ;(print `(:parse-decorations-for-lang ,opt))
  (let* ((optlst (cl-ppcre:split "\\s+" opt))
         ;(x (print `(:x ,opt ,optlst)))
         (lang-fname (cl-ppcre:split ":" (car optlst)))
         (lang (make-keyword (car lang-fname)))
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
                        (cons (list keyword) rv))
                     (take-two-by-two (cddr lst)
                        (let ((spliter (get-function :word-split *lang-set* :first-key :python)))
                          ;(print `(:xxx ,(python-word-split value)))
                          ;(print `(:xxx ,(funcall #'python-word-split value)))
                          (cons (cons keyword (funcall spliter value)) rv)))))))
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

      #+:debug+
      (print `(:two ,
          (enclose-list0 (sort
                    (take-two-by-two (cdr optlst) nil)
                    #'(lambda (a b) (string< (string (car a)) (string (car b))))))))
             
      #+:debug+
      (print `(:lang :find ,lang ,(assoc (intern (string-upcase lang) :keyword) *lang-set*) ,*lang-set* :filename ,fname))
      `((:lang . ,(cdr (assoc lang *lang-set*))) (:filename . ,fname)
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
(defun first-space-to-escaped-space (line opt-lst mode)
  (multiple-value-bind (start-pos end-pos) (cl-ppcre:scan " *" line)
    (let ((first (subseq line start-pos end-pos))
          (remain (subseq line end-pos)))
      (if first
        (concatenate 'string (space-to-escaped-space first) remain)
        line))))

;----------------------------------------------------------------
(defun word-to-tagged-list (word opt-lst)
  ;(print `(:tagged-list ,word ,opt-lst))
  (if (null opt-lst) word
    (let ((keyid (caar opt-lst))
          (keywords (cdar opt-lst))
          (remain (cdr opt-lst)))

      ;(print `(:key ,keyid ,keywords))
      (if (find word keywords :test #'(lambda (word x) (and (stringp x) (string-equal word x))))
        `(,word . ,keyid)
        (word-to-tagged-list word remain)))))

;----------------------------------------------------------------
(defun scan-strings-parser (line)
  (let ((first-char (char line 0)))

    (cl-ppcre:scan (format nil "[^\\\\]~a" first-char) line)))

;----------------------------------------------------------------
(defun python-word-split (line)
  (cl-ppcre:split "," line))

;----------------------------------------------------------------
(defun python-tag-to-span (line)
  (error "Not Implement Yet"))

;----------------------------------------------------------------
(defun python-triple-single-quote (line)
  (error "Not Implement Yet"))

;----------------------------------------------------------------
(defun python-triple-doub (line)
  (error "Not Implement Yet"))

;----------------------------------------------------------------
; Python 用なんちゃってパーザ
; 空白で区切る程度
; mode は :double-quote :quote :|triple-single-quote| :|triple-double-quote|

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

                  ;(print `(:lf ,line-len ,first-char ,line))
                  (cond 
                    ((and (eq first-char #\")
                              (>= line-len 3)
                              (eq (char line 1) #\")
                              (eq (char line 2) #\"))

                     (values `((,line . :|triple-double-quote|) ,@rv) :|triple-double-quote|))

                    ((and (eq first-char #\')
                              (>= line-len 3)
                              (eq (char line 1) #\')
                              (eq (char line 2) #\'))
                         (values `((,line . :|triple-single-quote|) ,@rv) :|triple-single-quote|))

                    ((eq first-char #\#)
                     `((,line :comment) ,@rv))

                    ((eq first-char #\")
                     (if (= line-len 1)
                       (values `((,line . :|id-double-quote|) ,@rv) :|id-double-quote|)
                       (multiple-value-bind (start0 end0)
                         (scan-strings-parser line)

                         (if end0
                           (python-line-parser 
                             (subseq line end0)
                             opt-lst
                             (cons `(,(subseq line 0 end0) . :|id-double-quote|) rv))
                           (values `((,line . :|id-double-quote|) ,@rv) :|id-double-quote|)))))

                    ((eq first-char #\')
                     (if (= line-len 1)
                       (values `((,line . :|id-single-quote|) ,@rv) :|id-single-quote|)
                       (multiple-value-bind (start1 end1)
                         (scan-strings-parser line)
                         (if end0
                           (python-line-parser 
                             (subseq line end0)
                             opt-lst
                             (cons `(,(subseq line 0 end0) . :|id-single-quote|) rv))
                           (values `((,line . :|id-single-quote|) ,@rv) :|id-single-quote|)))))
                    (t (multiple-value-bind (start2 end2)
                                (cl-ppcre:scan "^[^\"'#\\s]+" line)
                              ;(print `(:line ,line ,start2, end2))
                              (python-line-parser (subseq line end2)
                                                  opt-lst
                                                  (cons 
                                                    (subseq line start2 end2)
                                                    rv)
                                                  )))))))))))))

;----------------------------------------------------------------
(defun get-function (key opt-lst &key (first-key :lang))
  ;(print `(:get-function ,key ,opt-lst))
  (let* ((func (cdr (assoc key (cdr (assoc first-key opt-lst)))))
         (new-func (if func
                     (cdr (assoc key (cdr (assoc :pre-set *lang-set*)))))))
      (symbol-function new-func)))


;----------------------------------------------------------------
(defun merge-lang-option (opt-lst)
  opt-lst)

;----------------------------------------------------------------
(defun get-nolang-option (&options opt-lst)
  opt-lst)

;----------------------------------------------------------------
(defun |mw/```| (first-line-option stream) 
  (let* ((decorate-option 
          (if (> (length first-line-option) 0)
            (merge-lang-option
              (parse-decorations-for-lang first-line-option))
            (get-nolang-option)))
         (parser (get-function :parser decorate-option)))

    (labels ((read-until-end-of-block (rv mode)
                (let ((line (read-line stream)))
                  (if (cl-ppcre:scan "^```[\\s]*" line) (nreverse rv)
                    (multiple-value-bind
                        (a-rv new-mode) (funcall parser line decorate-option)
                      (print `(:mode ,mode :new-mode ,new-mode))
                      (read-until-end-of-block (cons a-rv rv) new-mode))))))
      ;(print `(:|triple-single-quote| ,first-line-option ,decorate-option))
      (print  "decorate-option start")
      (map nil #'print (read-until-end-of-block nil nil))
      (print  "decorate-option END"))))


;----------------------------------------------------------------
(defun interp-a-markdown (stream)
  (let ((line (read-line stream)))
    ;(print `(:line ,line))
    (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^([^0-9a-zA-Z]*)[	 ]*(.*)" line)

      (let* ((flstv
               (map 'vector #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline) x)) regs))
             ;(jgeil (print `(:jgeil ,flstv ,regs)))
             (fname (intern (string-concat "mw/" (elt flstv 0))))
             (an-arg (elt flstv 1))
             (flst (list fname an-arg stream)))

        ;(print `(:fname ,fname))
        (if (fboundp fname)
          (progn
            #+:debug+
            (print `(:flst ,flst))
            (eval flst))
          (cadr flst))))))
