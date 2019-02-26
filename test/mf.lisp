(setf lst-with-unquote '`(:span :class "python-keyword" ,arg))
(setf lst-with-splice '`(:span :class "python-keyword" ,@arg))

(defmacro make-style-lambda (style-desc) 
  `#'(lambda ,
       (remove-duplicates 
         (remove nil 
          (mapcar 
            #'(lambda (i) 
                (if (listp i) 
                  (let ((qkey (car i))
                        (arg-sym (cadr i)))
                    (if (or (eq qkey 'SYSTEM::UNQUOTE)
                            (eq qkey 'SYSTEM::SPLICE))
                      arg-sym))))
            (cadr style-desc))) :from-end t) ,style-desc))

(print (eval `(make-style-lambda ,lst-with-unquote)))
(print (funcall (eval `(make-style-lambda ,lst-with-unquote)) "def"))
(print (funcall (eval `(make-style-lambda ,lst-with-unquote)) '("xyz" "abc")))
(print (eval `(make-style-lambda ,lst-with-splice)))
(print (funcall (eval `(make-style-lambda ,lst-with-splice)) '("xyz" "abc")))
(print (funcall (eval `(make-style-lambda ,lst-with-splice)) "xyz"))

#|
(setf lst '(
            "abc"
            (:comment . "# 0iji") :nl
            (:comment . "# 1iji") :nl
            (:comment . "# 2iji") :nl
            :nl
            "   " (:comment . "# 3iji") :nl
            "   " (:comment . "# 4iji") :nl
            "def" :nl
            (:comment . "# 4iji") :nl
            "def" (:string . "# 5iji") :nl
            (:string . "# 6iji") ">" "abc" :nl
            (:comment . "# 7iji") :nl
            ))

(setf lst 
 '((:COMMENT . "#&nbsp;+-----") :NL (:COMMENT . "#&nbsp;+-----") :NL
  (:COMMENT . "&nbsp;&nbsp;&nbsp;&nbsp;#xxx") :NL
  (:COMMENT . "&nbsp;&nbsp;&nbsp;&nbsp;#yyy") :NL "&nbsp;&nbsp;&nbsp;&nbsp;" "ih"
  "&nbsp;" "=" "&nbsp;" "input" "&nbsp;" "&gt;&gt;" "&nbsp;" "6" :NL))

(defun is-target-keyword (k)
  (case k
    (:comment :comment)
    (:string :string)
    (otherwise nil)))

(defun kari-matome0 (first-keyword remain-lst rv)
  (print `(:kari-matome0 ,first-keyword ,rv))
  (let ((target-first-word (car remain-lst))
        (target-second-word (cadr remain-lst))
        (new-remain-lst (cddr remain-lst)))

    (if (or (not (eq target-first-word :nl))
            (not (listp target-second-word))) (values (nreverse rv) remain-lst)

      (let ((target-second-keyword (car target-second-word))
            (target-second-str (cdr target-second-word)))
        (if (or (not (eq first-keyword target-second-keyword))
                (not (stringp target-second-str))) (values (nreverse rv) remain-lst)
          (kari-matome0 first-keyword new-remain-lst 
                        (cons target-second-str rv)))))))

(defun kari-matome (key first-lst remain-lst)
  ;(print `(:kari-matome ,first-lst ,remain-lst))
  (let ((first-keyword (is-target-keyword key)))
    (if (null first-keyword) nil
      (let ((first-str (cdr first-lst))
            (second-word (car remain-lst))
            (third-word (cadr remain-lst))
            (new-remain-lst (cddr remain-lst)))

        ;(assert (listp third-word))
        (if (or (not (stringp first-str))
                (not (eq second-word :nl))
                (not (listp third-word))
                (not (eq first-keyword (car third-word)))
                (not (stringp (cdr third-word)))) nil

          (kari-matome0 first-keyword new-remain-lst 
            (list (cdr third-word) first-str)))))))


(defun matome (lst &optional rv)
  (if (null lst) (nreverse rv)
    (let ((word (car lst))
          (remain (cdr lst)))

      (if (atom word) (matome remain (push word rv))
        (let ((key (car word)))
          (multiple-value-bind (new-rv new-remain)
              (kari-matome key word remain)
            ;(print `(:new-rv ,new-rv))
            (if new-rv (matome new-remain (push (cons key new-rv) rv))
              (matome remain (push word rv)))))))))

(print `(:matome ,(matome lst)))

(defun concat-tagged-list (lst &optional rv)
  (labels ((is-target-keyword (k)
             (case k
               (:comment :comment)
               (:string :string)
               (otherwise nil)))
   
           (concat-tagged-list-loop (first-keyword remain-lst rv)
             (print `(:concat-tagged-list-loop ,first-keyword ,rv))
             (let ((target-first-word (car remain-lst))
                   (target-second-word (cadr remain-lst))
                   (new-remain-lst (cddr remain-lst)))
   
               (if (or (not (eq target-first-word :nl))
                       (not (listp target-second-word))) (values (nreverse rv) remain-lst)
   
                 (let ((target-second-keyword (car target-second-word))
                       (target-second-str (cdr target-second-word)))
                   (if (or (not (eq first-keyword target-second-keyword))
                           (not (stringp target-second-str))) (values (nreverse rv) remain-lst)
                     (concat-tagged-list-loop first-keyword new-remain-lst 
                                          (cons target-second-str rv)))))))
   
           (concat-tagged-list0 (key first-lst remain-lst)
             (print `(:concat-tagged-list0 ,first-lst ,remain-lst))
             (let ((first-keyword (is-target-keyword key)))
               (if (null first-keyword) nil
                 (let ((first-str (cdr first-lst))
                       (second-word (car remain-lst))
                       (third-word (cadr remain-lst))
                       (new-remain-lst (cddr remain-lst)))
   
                   (print `(:here ,first-str ,(not (stringp first-str))
                             ,(not (eq second-word :nl))
                             ,(not (listp third-word))
                             ,(not (eq first-keyword (car third-word)))
                             ,(not (stringp (cdr third-word)))))
                   (if (or (not (stringp first-str))
                           (not (eq second-word :nl))
                           (not (listp third-word))
                           (not (eq first-keyword (car third-word)))
                           (not (stringp (cdr third-word)))) nil
   
                     (concat-tagged-list-loop
                       first-keyword new-remain-lst 
                       (list (cdr third-word) first-str))))))))
    (if (null lst) (nreverse rv)
      (let ((word (car lst))
            (remain (cdr lst)))

        (if (atom word) (matome remain (push word rv))
          (let ((key (car word)))
            (multiple-value-bind (new-rv new-remain)
              (concat-tagged-list0 key word remain)
              ;(print `(:new-rv ,new-rv))
              (let ((arg-remain (if new-rv new-remain remain))
                    (arg-rv (push (if new-rv (cons key new-rv) word) rv)))
                (concat-tagged-list arg-remain arg-rv)))))))))

(print `(:concat-tagged-list ,(concat-tagged-list lst)))
|#
