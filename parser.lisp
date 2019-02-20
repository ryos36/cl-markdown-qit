(defparameter *cached-style-list* nil)
(defparameter *result* nil)
(defparameter *tabstop* 4)
(defparameter *lang-set*
  '((:pre-set
      (:name . :pre-set)
      (:parser . first-space-to-escaped-space)
      (:tag-to-span . python-tag-to-span) ; deprecated
      (:word-split . python-word-split))

    (:python 
      (:name . :python)
      (:parser . python-line-parser)
      (:word-split . python-word-split)
      (:tag-to-span . python-tag-to-span) ; deprecated
      (:style (:keyword . `(:span :class "python-keyword" ,arg)))
      (:mode-set (:|triple-single-quote| . python-triple-single-quote)
                 (:|triple-double-quote| . python-triple-double-quote)
                 (:|id-double-quote| . python-double-quote)))))

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
(defun python-triple-single-quote (line opt-lst &optional rv)
  (let ((new-mode (if (cl-ppcre:scan "^\s*'''" line) nil :|triple-single-quote|)))
    (values `((,line . :|triple-single-quote|)) new-mode)))

;----------------------------------------------------------------
(defun python-triple-double-quote (line opt-lst &optional rv)
  (let ((new-mode (if (cl-ppcre:scan "^\s*\"\"\"" line) nil :|triple-double-quote|)))
    (values `((,line . :|triple-single-quote|)) new-mode)))

;----------------------------------------------------------------
(defun python-quote (line opt-lst rv quoted-char quoted-keyword)
  (if (= (length line) 1)
    (values `((,line . ,quoted-keyword))
            (if (not (eq (char line 0) quoted-char)) quoted-keyword))
    (multiple-value-bind (start end)
        (cl-ppcre:scan (format nil "[^\\\\]~a" quoted-char) line)
      (if end
        (let ((quoted-str-pair `(,(subseq line 0 end) . ,quoted-keyword))
              (remain-str (subseq line end)))
          (if (> (length remain-str) 0)
              (multiple-value-bind (lst mode)
                (python-line-parser remain-str opt-lst)
                (values (cons quoted-str-pair lst) mode))
              (list quoted-str-pair)))
        (values `((,line . ,quoted-keyword)) quoted-keyword)))))

;----------------------------------------------------------------
(defun python-double-quote (line opt-lst &optional rv)
  (python-quote line opt-lst nil #\" :|id-double-quote|))

;----------------------------------------------------------------
(defun python-single-quote (line opt-lst &optional rv)
  (python-quote line opt-lst nil #\' :|id-single-quote|))

;----------------------------------------------------------------
(defun python-continue-line-p (line)
  (let* ((line-len (length line))
         (last_1 (if (> line-len 1) (char line (- line-len 2))))
         (last (char line (- line-len 1))))
    (and (eq last #\\) (not (eq last_1 #\\)))))

;----------------------------------------------------------------
; Python 用なんちゃってパーザ
; 空白で区切る程度
; mode は :|id-double-quote| :|id-single-quote] :|triple-single-quote| :|triple-double-quote|

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
                           (values (nreverse `((,line . :|id-double-quote|) ,@rv))
                                   (if (python-continue-line-p line) :|id-double-quote|))))))

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
                           (values (nreverse `((,line . :|id-single-quote|) ,@rv))
                                   (if (python-continue-line-p line) :|id-single-quote|))))))
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
         (new-func (if func func
                     (cdr (assoc key (cdr (assoc :pre-set *lang-set*)))))))
    ;(print `(:get-function ,new-func))
    (symbol-function new-func)))

;----------------------------------------------------------------
(defun get-mode-function (key opt-lst)
  (let* ((func (cdr (assoc key (cdr (assoc :mode-set (cdr (assoc :lang opt-lst)))))))
         (new-func (if func func
                     (cdr (assoc :parser (cdr (assoc :pre-set *lang-set*)))))))
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
         (main-parser (get-function :parser decorate-option))
         (tag-to-span (get-function :tag-to-span decorate-option)))

    (labels ((read-until-end-of-block (parser rv mode)
                (let ((line (read-line stream)))
                  (if (cl-ppcre:scan "^```[\\s]*" line) (nreverse rv)
                    (multiple-value-bind
                        (a-rv new-mode) (funcall parser line decorate-option nil )
                      ;(print `(:mode ,mode :new-mode ,new-mode))
                      (let ((new-parser (if new-mode (get-mode-function new-mode decorate-option) main-parser)))

                        (read-until-end-of-block new-parser (cons a-rv rv) new-mode)))))))
      `((:option . ,decorate-option)
        (:block .  ,(read-until-end-of-block main-parser nil nil))))))

;----------------------------------------------------------------
(defun get-tag-item (key-list opt-lst)
  (if (null key-list) opt-lst
    (get-tag-item (cdr key-list)
                  (cdr (assoc (car key-list) opt-lst)))))

;----------------------------------------------------------------
(defun escape-string (word)
  (if (cl-ppcre:scan "\\s+" word) (space-to-escaped-space word)
    (cl-who:escape-string word)))

;----------------------------------------------------------------
; `(:span :class "python-keyword" ,arg)))
; のような記述を ,arg などの , で始まるものをしらべて
; 引数にする lambda を生成する。
; 生成された lambda は funcall あるいは apply すればよい 

(defmacro make-style-lambda (style-desc) 
  `#'(lambda ,
       (remove-duplicates 
         (remove nil 
          (mapcar 
            #'(lambda (i) 
                (if (and (listp i) (eq (car i) 'SYSTEM::UNQUOTE)) (cadr i)))

            (cadr style-desc))) :from-end t) ,style-desc))

;
;----------------------------------------------------------------
;ryos
(defun expand-tagged-line-to-who (line-lst opt-lst)
  ;(print `(:line-lst ,line-lst))
  (let ((style-list (get-tag-item '(:lang :style) opt-lst)))
    ;(print `(:style ,style-list, opt-lst))
    `(:div ,@(mapcar #'(lambda (word-pair)
                (let* ((flag (listp word-pair))
                       (word (if flag (car word-pair) word-pair))
                       (tag (if flag (cdr word-pair)))
                       (style (cdr (assoc tag style-list)))
                       (style-func (if style (eval `(make-style-lambda ,style))))
                       (format-str (if style style "~a")))
                  ;(print `(:word ,word :style ,style))
                  (if style
                    (progn
                      (push `(,tag . ,style-func) *cached-style-list*)
                      ;(print `(:style-func ,style-func))
                      (push (funcall style-func word) *result*)
                      (funcall style-func word))
                    (escape-string word))))
            line-lst))))

;----------------------------------------------------------------
(defun expand-tagged-block-to-who (lst)
  (let ((opt-lst (cdr (assoc :option lst)))
        (blk (cdr (assoc :block lst))))
    `(:div ,@(mapcar #'(lambda (word) (expand-tagged-line-to-who word opt-lst)) blk))))

;----------------------------------------------------------------
(defun interp-a-markdown (stream)
  (labels ((interp-a-markdown-inner (rv)
            (let ((tlist (markdown-line-to-tagged-list stream)))
              (if (eq tlist :eof) (nreverse rv)
                (interp-a-markdown-inner 
                  (cons tlist rv))))))
    (mapcar #'expand-tagged-block-to-who (interp-a-markdown-inner nil))))

;----------------------------------------------------------------
(defun markdown-line-to-tagged-list (stream)
  (let ((line (read-line stream nil :eof)))
    (if (eq line :eof) :eof
      (multiple-value-bind (match regs)
        (cl-ppcre:scan-to-strings "^([^0-9a-zA-Z]*)[	 ]*(.*)" line)

        (let* ((flstv
                 (map 'vector #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline) x)) regs))
               (fname (intern (string-concat "mw/" (elt flstv 0))))
               (an-arg (elt flstv 1))
               (flst (list fname an-arg stream)))

          ;(print `(:fname ,fname))
          (if (fboundp fname)
            (progn
              #+:debug+
              (print `(:flst ,flst))
              (eval flst))
            (cadr flst)))))))
