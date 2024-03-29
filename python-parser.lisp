(in-package :cl-markdown-qit)

;----------------------------------------------------------------
(defun load-python-keywords ()
  (read-file-as-list "python/python-keywords.txt"))

;----------------------------------------------------------------
(defun load-python-builtins ()
  (read-file-as-list "python/python-builtins.txt"))

;----------------------------------------------------------------
(defun python-continue-line-p (line)
  (let* ((line-len (length line))
         (last_1 (if (> line-len 1) (char line (- line-len 2))))
         (last (char line (- line-len 1))))
    (and (eq last #\\) (not (eq last_1 #\\)))))

;----------------------------------------------------------------
(defun python-string-quote (stream opt-lst rv0 quoted-char quoted-keyword)
  (labels ((make-tagged-list (lst)
             `(,quoted-keyword . ,lst))
           (parse-line (line)
             (multiple-value-bind (start end)
               (cl-ppcre:scan (format nil "[^\\\\]~a" quoted-char) line)
               (declare (ignore start))
               (if end 
                 (let ((quoted-str-pair (make-tagged-list (subseq line 0 end)))
                       (remain-str (subseq line end)))
                   (let ((has-remain-p (> (length remain-str) 0)))
                     (if has-remain-p
                       (push-back-line remain-str opt-lst))
                     (values quoted-str-pair (if (not has-remain-p) :nl) :end)))
                 (values (make-tagged-list line)
                         :nl
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

    ;(print `(:python-string-single-quote ,opt-lst))
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
  ;(print `(:python-document-quote ,opt-lst))
  (labels ((make-tagged-list (lst)
            ;(print `(:make-tagged-list ,lst))
             `(,quoted-keyword . ,lst))

           (parse-first-line (rv)
             (let ((line (nget-current-line stream opt-lst)))
               (multiple-value-bind (hit-str strv)
                 (cl-ppcre:scan-to-strings 
                   (format nil "(^\\s*)(~a)(.*)$" quoted-str) line)
                 (declare (ignore strv))
                 (assert hit-str)
                 (cons :nl
                       (cons 
                         (make-tagged-list line)
                         rv)))))

           (read-until-end-of-block (rv)
             ;(print :read-until-end-of-block)
             (let ((line (nget-current-line stream opt-lst)))
               ;(print `(:TTT ,line))
               (if (eq line :eof) rv
                 (multiple-value-bind (hit-str strv)
                   (cl-ppcre:scan-to-strings 
                     (format nil "^(\\s*)(~a)(.*)$" quoted-str) line)
                   (declare (ignore strv))
                   ;(print `(:hit-str ,hit-str , (format nil "^(\\s*)(~a)(.*)$" quoted-str)))
                   (if hit-str
                     (cons :nl
                           (cons (make-tagged-list line) rv))
                     (read-until-end-of-block
                       (cons :nl
                             (cons (make-tagged-list line) rv)))))))))

    (read-until-end-of-block (parse-first-line rv0))))

;----------------------------------------------------------------
(defun python-document-triple-single-quote (stream opt-lst &optional rv)
  (python-document-quote stream opt-lst rv "'''" :document-triple-single-quote))

;----------------------------------------------------------------
(defun python-document-triple-double-quote (stream opt-lst &optional rv)
  (python-document-quote stream opt-lst rv "\"\"\"" :document-triple-double-quote))

;----------------------------------------------------------------
; 
(defun python-document-triple-X-quote (stream opt-lst char-X &optional rv)
  (assert (or (eq char-X #\") (eq char-X #\')))
  (let ((quoted-str (make-string 3 :initial-element char-X))
        (quoted-keyword (if (eq char-X #\') :document-triple-single-quote :document-triple-single-quote)))

    (python-document-quote stream opt-lst rv quoted-str quoted-keyword)))

  ;(python-document-quote stream opt-lst rv quoted-str (if (eq char-X #\') :document-triple-single-quote :document-triple-single-quote))

;----------------------------------------------------------------
(defun is-triple-quote (line)
  (and (>= (length line) 3)
       (let ((first-3-char (subseq line 0 3)))
         (or (string-equal "'''" first-3-char)
             (string-equal "\"\"\"" first-3-char)))))

;----------------------------------------------------------------
;----------------------------------------------------------------

(defun python-line-parser (stream opt-lst &optional rv)
  (labels ((parse-one (parse-str line rv)
             ;(print `(:parse-one ,parse-str ,line))
             (multiple-value-bind (start end) 
                 (cl-ppcre:scan parse-str line)
               (if (not end) (values line rv)
                 (let ((hit-str (subseq line start end))
                       (remain (if (= end (length line)) nil
                                 (subseq line end))))
                   ;(print `(:hit-str ,hit-str :remain ,remain))
                   (values remain (cons hit-str rv))))))

           (parse-line-nl (line rv)
              ;(print `(:parse-line-nl ,line))
              (if (= (length line) 0)
                (values nil (cons :nl rv))
                (parse-line-space line rv)))

           (parse-line-space (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^\\s+" line rv)
                (if (null line0) (values nil (cons :nl rv0))
                  (if (is-triple-quote line0)
                    (values line rv)

                    (if (eq (char line0 0) #\#)
                      (values line rv)
                      (parse-line-digit line0 rv0))))))

           (parse-line-digit (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^\\d+" line rv)
                ;(print `(:parse-line-digit ,line0 ,rv0))
                (if (null line0) (values nil (cons :nl rv0))
                  (parse-line-word line0 rv0))))

           (parse-line-word (line rv)
              (multiple-value-bind (line0 rv0)
                  (parse-one "^\\w+" line rv)
                ;(print `(:parse-line-word ,line0 ,rv0))
                (if (null line0) (values nil (cons :nl rv0))
                  (parse-line-quote line0 rv0))))

           (parse-line-quote (line rv)
              (let ((first-char (char line 0)))
                (if (or (eq first-char #\")
                        (eq first-char #\'))
                  (values line rv)
                  (parse-line-comment line rv))))

           (parse-line-comment (line rv)
              (if (eq (char line 0) #\#)
                (values nil (cons :nl
                                  (cons `(:comment . ,line) rv)))
                (parse-line-others line rv)))

           (parse-line-others (line rv)
              ;(print `(:line ,line))
              (multiple-value-bind (line0 rv0)
                  (parse-one "^[^\"'#\\s]+" line rv)
                ;(print `(:line0 ,line0))
                (if (null line0) (values nil (cons :nl rv0))
                  (parse-line-space line0 rv0)))))

    (let ((line (nget-current-line stream opt-lst)))
      ;(print `(:line ,line))
      (multiple-value-bind (remain updated-rv)
          (parse-line-nl line rv)
        (if remain
          (push-back-line remain opt-lst))
        (let* ((trimmed-str (if remain (string-trim (format nil "~a~a" #\Space #\Tab) remain)))
               (first-char (if remain (char trimmed-str 0)))
               (x `(print `(:first-char ,remain ,first-char)))
               (updated-updated-rv
                 (if remain
                   (cond
                     ((is-triple-quote trimmed-str)
                      (python-document-triple-X-quote stream opt-lst first-char updated-rv))
                     ((eq first-char #\#)
                      (progn
                        (setf (cdr (assoc :current-line opt-lst)) nil)
                        (cons :nl (cons `(:comment . ,remain) updated-rv))))

                     ((eq first-char #\")
                      (python-string-double-quote stream opt-lst updated-rv))

                     ((eq first-char #\')
                      (python-string-single-quote stream opt-lst updated-rv))

                     (t
                       (assert nil)))

                   updated-rv))
               (current-line (cdr (assoc :current-line opt-lst))))
          ;(print `(:current-line ,current-line))
          (if current-line
            (python-line-parser stream opt-lst updated-updated-rv)
            updated-updated-rv))))))


;----------------------------------------------------------------
;----------------------------------------------------------------
; to-one-block で :nl がくるまで一塊にしている。
; to-block でさらにそれを一塊にしている
;
; 生成される who は :translated という余計な keyword が入っている
; expand-tagged-block-to-who でそれらをとることが出来る
;
(defun python-parser (stream opt-lst)
               ; ToDo
               ;   1):tab 対応
  (let ((lang-option (assoc :python *lang-set*)))
    (declare (ignore lang-option))
    (labels ((to-one-block (who-nl-list rv)
               (if (null who-nl-list) rv
                 (let ((word (car who-nl-list))
                       (remain-lst (cdr who-nl-list)))
                   (if (eq word :nl) (values rv remain-lst)
                     (to-one-block remain-lst 
                                   (push word rv))))))

             (to-block (who-nl-list rv)
               (if (null who-nl-list) rv
                 (multiple-value-bind (line-rv updated-who-nl-list)
                     (to-one-block who-nl-list nil)
                   ;(print `(:line-rv ,line-rv :updated-who-nl-list ,updated-who-nl-list))
                   (push (cons :translated (cons :div 
                                                 (or line-rv '((:br)))
                                                 )) rv)
                   (to-block updated-who-nl-list rv))))

             (make-return-value (lst)
               (mapcar #'python-tagged-list-to-who-style 
                      (concat-tagged-list lst)))
             ; RYOS TODO ryos todo ToDo
             ; nreverse を削除してうまく言ったつもり

             (escape-to (lst rv)
                ;(print `(:escape-to ,lst ,rv))
                (if (null lst) (nreverse rv)
                  (let ((target (car lst))
                        (remain (cdr lst)))
                    (cond
                      ((listp target)
                         (let ((first-right (car target))
                               (remain-or-left (cdr target)))
                           ;(print `(:first-right ,first-right ,remain-or-left))
                           (if (and (keywordp first-right) (stringp remain-or-left))
                             (escape-to
                               remain
                               (push
                                 (cons
                                   first-right
                                   (escape-string remain-or-left)) rv))
                             (escape-to
                               remain
                               (push (escape-to target nil) rv)))))
                      ((stringp target)
                         (escape-to
                           remain
                           (push (escape-string target) rv)))
                      (t 
                        (escape-to
                           remain
                           (push target rv)))))))

             (nread-until-end-of-block (rv)
               (let ((line (nget-current-line stream opt-lst)))
                 ;(print `(:nread-until-end-of-block ,line))
                 (if (or (eq line :eof)
                         (cl-ppcre:scan "^```[\\s]*" line))
                   rv

                   (progn
                     (push-back-line line opt-lst)
                       (let ((updated-rv
                               (python-line-parser stream opt-lst rv)))

                         (nread-until-end-of-block updated-rv)))))))

      (to-block
        (make-return-value
          (print 
          (escape-to
            (nread-until-end-of-block nil)
            nil)))
        nil))))

;----------------------------------------------------------------
(defun insert-item (lst item &key (reverse-func #'reverse))
  (cdr
   (reduce #'(lambda (x b) (cons item(cons b x))) (cons nil (funcall reverse-func lst)))))

;----------------------------------------------------------------
; 個々の 文字列 や targged-list を ほとんど who 形式にする。
; word はリストではないことに注意
; :nl がまざっているのでちょっと who とは違う。

(defun python-tagged-list-to-who-style (word &optional add-opt-lst)
  ;(print `(:tagged-list ,word ,(if (listp word) (length (cdr word)))))
  (let ((style-list 
          (nconc
            (copy-tree
              (cadr (assoc :style add-opt-lst)))
            (copy-tree
              (get-tag-item '(:python :style) *lang-set*)))))
    ;(print `(:style ,style-list))
    (labels ((find-style-by-string (word s-lst)
               (if (null s-lst) nil
                 (let* ((one-desc (car s-lst))
                        (key (car one-desc))
                        ;(x `(print `(:one-desc ,one-desc)))
                        (str-or-str-lst (cadr one-desc))
                        (style-desc (caddr one-desc))
                        (hit 
                          (if (stringp word)
                            (if (listp str-or-str-lst)
                              (find word str-or-str-lst :test #'string-equal)
                              (cl-ppcre:scan str-or-str-lst word)))))
                   (declare (ignore key))
                   (if hit style-desc
                     (find-style-by-string word (cdr s-lst))))))

             (find-style-by-key (tag s-lst)
               ;(print `(:find-style-by-key ,tag ,s-lst))
               (if (null s-lst) nil
                 (let* ((one-desc (car s-lst))
                        (key (car one-desc))
                        (style-desc-list (cddr one-desc)))
                   (if (eq tag key) (values-list style-desc-list)
                     (find-style-by-key tag (cdr s-lst))))))

             (get-type (word)
               (cond 
                 ((stringp word) :type-I)         ; "aaa"
                 ((eq :nl word) :type-IV)         ; :nl
                 ((stringp (cdr word)) :type-II)  ; (:key . "aaa")
                 ((>= (length word) 3) :type-III) ; (:key "aaa" "bbb" ...)
                 (t (assert (not "illegal format")))))

             (get-string-word (word type)
               (ecase type
                 (:type-I word)
                 (:type-II (cdr word))
                 (:type-III (cdr word))
                 (:type-IV word)))

             ;
             ; 両方のタイプに対応 後者は使わないかも
             ;(:document-triple-single-quote () `(:span :class "python-document" ,arg) `(:span :class "python-document" ,@arg) (:br))
             ;(:document-triple-single-quote () `(:span :class "python-document" ,arg) `(:span :class "python-document" ,@arg) (:br) `(:span ,arg))
             ;
             (get-style (type word style-list)
                (let ((key (and (not (stringp word)) 
                                (not (eq :nl word))
                                (car word))))
                  (ecase type
                    (:type-I (find-style-by-string word style-list))
                    (:type-II (find-style-by-key key style-list))
                    (:type-III (find-style-by-key key style-list))
                    (:type-IV nil))))

             (concat-list0 (word-lst delim span-style-desc)
               (let* ((style-lambda (if span-style-desc (eval `(make-style-lambda ,span-style-desc)) #'(lambda (x) x))))
                 (insert-item
                   (mapcar #'(lambda (word) (funcall style-lambda word)) word-lst) delim))))

      (let* ((type (get-type word))
             (word-str (get-string-word word type)))
        (multiple-value-bind (quote-style-desc splice-style-desc delim span-style-desc)
            (get-style type word style-list)

          (if (null quote-style-desc) 
            (if (listp word-str) 
              (apply #'concatenate 'string word-str)
              word-str)

            (let* ((style-desc (if (eq type :type-III) splice-style-desc quote-style-desc))
                   (style-lambda (eval `(make-style-lambda ,style-desc)))
                   (ins-word (if (eq type :type-III)
                               (concat-list0 word-str delim span-style-desc)
                               word-str)))
              (funcall style-lambda ins-word))))))))

;;----------------------------------------------------------------
(defun parse-python (file-name &optional base-opt-lst)
  (let ((opt-lst (make-option-list base-opt-lst)))
    (expand-tagged-line-to-who
      (with-open-file (in file-name)
        (python-parser in opt-lst)) opt-lst)))
