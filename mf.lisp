(setf x '`(:span :class "python-keyword" ,arg))

(defmacro make-style-lambda (style-desc)
  `#'(lambda ,
       (remove-duplicates
         (remove nil
          (mapcar
            #'(lambda (i)
                (if (and (listp i) (eq (car i) 'SYSTEM::UNQUOTE)) (cadr i)))

            (cadr style-desc))) :from-end t) ,style-desc))

(print (eval `(make-style-lambda ,x)))

(setf lst '(
            "abc"
            (:comment . "# 0iji") :nl
            (:comment . "# 1iji") :nl
            (:comment . "# 2iji") :nl
            :nl
            (:comment . "# 3iji") :nl
            (:comment . "# 4iji") :nl
            "def" :nl
            (:comment . "# 4iji") :nl
            "def" (:string . "# 5iji") :nl
            (:string . "# 6iji") ">" "abc" :nl
            (:comment . "# 7iji") :nl
            ))

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
