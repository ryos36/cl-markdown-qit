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
      (print `(:|'''| ,first-line-option ,decorate-option))
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
