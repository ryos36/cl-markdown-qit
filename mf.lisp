(setf x '(:aaa . `(:span :class "python-keyword" ,arg)))

(defmacro make-style-lambda (style-desc)
  `#'(lambda ,
       (remove-duplicates
         (remove nil
          (mapcar
            #'(lambda (i)
                (if (and (listp i) (eq (car i) 'SYSTEM::UNQUOTE)) (cadr i)))

            (cadr style-desc))) :from-end t) ,style-desc))

(make-style-lambda x)
