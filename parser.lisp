(in-package :cl-markdown-qit)

(defparameter *cached-style-list* nil)
(defparameter *result* nil)
(defparameter *tabstop* 4)
(defparameter *lang-set*
  '((:pre-set
      (:name . :pre-set))
    (:python 
      (:name . :python)
      (:style-class-code . (:class "python-code"))
      (:style
        (:keyword ("def" "for" "range" "return") `(:span :class "python-keyword" ,arg))
        (:document-triple-single-quote ("") `(:span :class "python-document" ,arg))
        ))))

(defun make-keyword (str)
  (if (string= str "common lisp") :common-lisp
    (intern (string-upcase str) :keyword)))

;----------------------------------------------------------------
; 旧バージョンで ``` のあとのオプション設定に使っていた
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
    (cl-ppcre:regex-replace-all " " line "&nbsp;"))

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
(defun get-tag-item (key-list opt-lst)
  ; 何のための対応か不明。削除予定
  ;(if (atom (car opt-lst)) nil)
  ;(print `(:key-list ,key-list :opt-lst ,opt-lst))
  (if (null key-list) (prog1 opt-lst (setf *first-opt* nil))
    (get-tag-item (cdr key-list)
                  (cdr (assoc (car key-list) opt-lst)))))

;----------------------------------------------------------------
#|
(defun get-tag-item2 (key-list opt-lst)
  (if (null key-list) opt-lst
    (get-tag-item2 (cdr key-list)
                   (cdr (assoc (car key-list) opt-lst)))))
|#

;----------------------------------------------------------------
(defun escape-string (word)
  (if (cl-ppcre:scan "\\s+" word) (space-to-escaped-space word)
    (cl-who:escape-string-minimal-plus-quotes word)))

;----------------------------------------------------------------
; `(:span :class "python-keyword" ,arg)
; のような記述を ,arg などの , で始まるものをしらべて
; 引数にする lambda を生成する。
; macro なので関数的に使えないところに注意
; (style-func (if style (eval `(make-style-lambda ,style))))
;
; 生成された lambda は funcall あるいは apply すればよい 
; 使い方はこんな感じ

(defmacro make-style-lambda (style-desc) 
  `#'(lambda ,
       (remove-duplicates 
         (remove nil 
          (mapcar 
            #'(lambda (i) 
                (if (and (listp i) (eq (car i) 'SYSTEM::UNQUOTE)) (cadr i)))

            (cadr style-desc))) :from-end t) ,style-desc))

;----------------------------------------------------------------
(defun expand-tagged-line-to-who (line-lst opt-lst)
  (let ((style-list (cdr (assoc :style opt-lst)))
        ; なんか間違った記述の気がする。
        ; 必要ないことがわかったら削除
        (old-style-list '(get-tag-item '(:lang :style) opt-lst)))
    (mapcar #'(lambda (word-pair)
                (let* ((list-flag (listp word-pair))
                       (tagged-pair-flag (and list-flag (atom (cdr word-pair))))
                       (tag (if list-flag (car word-pair)))
                       (word (if list-flag (cdr word-pair) word-pair))
                       (x `(print `(:word ,word :tag ,tag)))
                       (style (if tag (cdr (assoc tag style-list))))
                       (style-func (if style (eval `(make-style-lambda ,style)))))
                  ;(print `(:x ,word-pair word ,word :style ,style ,word-pair))
                  (if (eq :nl word-pair) (list :br) ; NL に対する暫定処理
                  (if (eq :translated tag)
                    word
                    (if style
                      (progn
                        (push `(,tag . ,style-func) *cached-style-list*)
                        ;(print `(:style-func ,style-func))
                        (push (funcall style-func word) *result*)
                        (funcall style-func word))
                      (escape-string word))))))
            line-lst)))

;----------------------------------------------------------------
; lst が 
; (:translated なんとか)
; ((:div なんとか) (:div なんとか)) 
; ((:translated :tag なんとか) (:div なんとか))
; (:class "python-code" (:translated :tag なんとか) (:div なんとか))
; とか来ることを想定 最後の形式は skip-option で対応
; 結果は (:div (:div ... )) などの who 形式になる
;
; div-option-lst を外で作っていてソースが汚い、、、

(defun expand-tagged-block-to-who (lst)
  ;(print `(:expand-tagged-block-to-who ,lst))
  (let ((div-option-lst))
    (labels ((skip-option (lst)
               ;(print `(:skip-option ,lst))
               (let ((item (car lst))
                     (remain (cdr lst)))
                 (if (or (keywordp item) (stringp item))
                   (progn
                     (push item div-option-lst)
                     (skip-option remain))
                   (progn 
                     (setf div-option-lst (nreverse div-option-lst))
                     lst)))))

      (skip-option lst)

      (let ((opt-lst (cdr (assoc :option lst)))
            (blk (cdr (assoc :block lst))))
        (let ((is-translated (eq (car blk) :translated)))
          (if is-translated (cdr blk)
            (let ((skipped-option-blk (skip-option blk)))
              `(:div ,@div-option-lst ,@(expand-tagged-line-to-who skipped-option-blk opt-lst)))))))))

;----------------------------------------------------------------
;----------------------------------------------------------------
; opt-lst を変更することに注意 
(defun nget-current-line (stream opt-lst)
  (let* ((current-line-in-opt-lst (assoc :current-line opt-lst))
         (line (or (cdr current-line-in-opt-lst) (read-line stream nil :eof))))
    (if current-line-in-opt-lst (setf (cdr current-line-in-opt-lst) nil))
    line))

;(defun push-into-option-list (key value opt-lst)
;  (push `(,key . ,value) opt-lst))

(defun push-back-line (line opt-lst)
  (let ((current-line-in-opt-lst (assoc :current-line opt-lst)))
    (setf (cdr current-line-in-opt-lst) line)))

;----------------------------------------------------------------
(defun preset-line-parser-reverse (line stream opt-lst)
  `(,line))

;----------------------------------------------------------------
; 通常のパーザー
(defun preset-block-parser (stream opt-lst)
  (labels ((read-until-end-of-block (rv)
             (let ((line (nget-current-line stream opt-lst)))
               ;(print `(:preset-block-parser :line ,line ,opt-lst))
               (if (or (eq line :eof) (= (length line) 0)) (nreverse rv)
                 (let ((new-parser-p (block-parser-detector line)))
                   (if new-parser-p (nreverse rv)
                     (read-until-end-of-block 
                       (nconc (preset-line-parser-reverse line stream opt-lst) rv))))))))
    (let ((blk (read-until-end-of-block nil)))
      `((:option . ,opt-lst)
        (:block . ,blk)))))

;----------------------------------------------------------------
;----------------------------------------------------------------
(defun lang-to-parser (lang)
  (case lang
    (:python #'python-parser)
    (:common-lisp  #'common-lisp-parser)
    (otherwise  #'python-parser)))

;----------------------------------------------------------------
; ``` で始まる parser
(defun lang-block-parser (stream opt-lst)
  (labels ((nparse-first-line (line)
            (assert (cl-ppcre:scan "^```" line))
            (let ((start-pos (length "```")))
              (multiple-value-bind (hit-str argv)
                  (cl-ppcre:scan-to-strings "^([A-Za-z][^:]*):?(\\S*)" line :start start-pos)
                ;(print `(:argv ,argv :lang ,(make-keyword (elt argv 0))))
                (if argv
                  (let ((lang (make-keyword (elt argv 0)))
                        (file-name nil))
                    (push `(:lang ,lang) opt-lst)
                    (when (= (length argv) 2)
                      (setf file-name (elt argv 1))
                      (push `(:file-name ,file-name) opt-lst))
                    (values lang file-name))))))

           (add-file-name (blk lang file-name)
             (let ((div-option-lst (get-tag-item '(:python :style-class-code) *lang-set*)))
               ;(print `(:div-option-lst ,@div-option-lst ,(cadr div-option-lst)))
               `(,@div-option-lst 
                  (:translated :div :class "code-file-name" ,file-name)
                  ,@blk))))

    (multiple-value-bind (lang file-name)
        (nparse-first-line (nget-current-line stream opt-lst))
      ;(print `(:multiple-value-bind ,lang ,file-name))
      (let* ((parser (lang-to-parser lang))
             (blk (funcall parser stream opt-lst)))

        `((:block . ,(add-file-name blk lang file-name))
          (:option . ,opt-lst))))))

;----------------------------------------------------------------
(defun with-title-block-parser (stream opt-lst)
  (labels ((parse-first-line (line)
            (multiple-value-bind (start-pos end-pos)
                (cl-ppcre:scan "^#+\\s*" line)
              ;(print `(:line ,line))
              (assert (and start-pos end-pos))
              `(:translated :h1 ,(subseq line end-pos))))

           (read-until-end-of-block (rv)
            ;(print `(:read-until-end-of-block ,rv))
             (let ((line (nget-current-line stream opt-lst)))
               (if (or (eq line :eof) (= (length line) 0)) (nreverse rv)
                 (let ((new-parser-p (block-parser-detector line)))
                   (if new-parser-p 
                     (progn
                       (push-back-line line opt-lst)
                       (nreverse rv))
                     (read-until-end-of-block 
                       (nconc (preset-line-parser-reverse line stream opt-lst) rv))))))))

    (let ((blk (read-until-end-of-block (list 
                  (parse-first-line (nget-current-line stream opt-lst))))))
      ;(print `(:blk ,blk))
      `((:block . ,blk)
        (:option . ,opt-lst)))))

;----------------------------------------------------------------
; 行を見て block parser を決定する。
; block parser は ((:block . something)
;                  (:option . something))
; か ((:block :translated . "なにか"))
; か ((:block :translated :tag "なにか"))
; を返す関数。
; 判断にも使っているので、該当するものがなければ nil を返す

(defun block-parser-detector (line)
  (cond 
     ((cl-ppcre:scan "^```" line)
        #'lang-block-parser)
     ((cl-ppcre:scan "^#" line)
        #'with-title-block-parser)
     ((cl-ppcre:scan "^-" line)
        #'list-block-parser)
     (t nil)))


;----------------------------------------------------------------
(defun markdown-stream (stream &key (tag :section) (tag-option nil))
  (labels ((read-all-block (rv opt-lst)
             ;(print `(:read-all-block :rv ,rv :line ,opt-lst ,tag-option))
             (let ((line (nget-current-line stream opt-lst)))
               (push-back-line line opt-lst)
               ;(print `(:push-back-line :line ,line))
               (if (eq line :eof) (nreverse rv)
                 (let* ((parser (block-parser-detector line))
                        (new-parser (or parser #'preset-block-parser))
                        (tagged-blk (funcall new-parser stream opt-lst))
                        (blk-lst (expand-tagged-block-to-who tagged-blk)))
                   (read-all-block (push blk-lst rv)
                                   (list (assoc :current-line opt-lst))))))))
    (let ((current-line-init  (list (list :current-line))))
      `(,tag ,@tag-option ,@(read-all-block nil current-line-init)))))

;----------------------------------------------------------------
(defun markdown (file-name &key (tag :section) (tag-option nil))
  (with-open-file (in file-name)
    (markdown-stream in :tag tag :tag-option tag-option)))
