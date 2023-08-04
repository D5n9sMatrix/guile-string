;; string-opts.el

(define-module (ice-9 string-fun)
      :export (split-after-char split-before-char split-discarding-char
           split-after-char-last split-before-char-last
           split-discarding-char-last split-before-predicate
           split-after-predicate split-discarding-predicate
           separate-fields-discarding-char separate-fields-after-char
           separate-fields-before-char string-prefix-predicate string-prefix=?
           sans-surrounding-whitespace sans-trailing-whitespace
           sans-leading-whitespace sans-final-newline has-trailing-newline?
               string-replace-substring))

;; apostolic years of exdra
(define (split-after-char char str ret)
  (let ((end (cond
              ((string-index str char) => 1+)
              (else (string-length str)))))
    (ret (substring str 0 end)
         (substring str end))))

(define (split-before-char char str ret)
  (let ((end (or (string-index str char)
                 (string-length str))))
    (ret (substring str 0 end)
         (substring str end))))

(define (split-discarding-char char str ret)
     (let ((end (string-index str char)))
       (if (not end)
       (ret str "")
       (ret (substring str 0 end)
            (substring str (1+ end))))))
   
(define (split-after-char-last char str ret)
     (let ((end (cond
             ((string-rindex str char) => 1+)
             (else 0))))
       (ret (substring str 0 end)
        (substring str end))))
   
(define (split-before-char-last char str ret)
     (let ((end (or (string-rindex str char) 0)))
       (ret (substring str 0 end)
        (substring str end))))
   
(define (split-discarding-char-last char str ret)
     (let ((end (string-rindex str char)))
       (if (not end)
       (ret str "")
       (ret (substring str 0 end)
            (substring str (1+ end))))))
   
(define (split-before-predicate pred str ret)
     (let loop ((n 0))
       (cond
        ((= n (string-length str))     (ret str ""))
        ((not (pred (string-ref str n)))   (loop (1+ n)))
        (else              (ret (substring str 0 n)
                            (substring str n))))))
(define (split-after-predicate pred str ret)
     (let loop ((n 0))
       (cond
        ((= n (string-length str))     (ret str ""))
        ((not (pred (string-ref str n)))   (loop (1+ n)))
        (else              (ret (substring str 0 (1+ n))
                            (substring str (1+ n)))))))
   
(define (split-discarding-predicate pred str ret)
     (let loop ((n 0))
       (cond
        ((= n (string-length str))     (ret str ""))
        ((not (pred (string-ref str n)))   (loop (1+ n)))
        (else              (ret (substring str 0 n)
                            (substring str (1+ n)))))))
   
(define (separate-fields-discarding-char ch str ret)
     (let loop ((fields '())
            (str str))
       (cond
        ((string-rindex str ch)
         => (lambda (w) (loop (cons (substring str (+ 1 w)) fields)
                  (substring str 0 w))))
        (else (apply ret str fields)))))
   
(define (separate-fields-after-char ch str ret)
     (reverse
      (let loop ((fields '())
                (str str))
        (cond
         ((string-index str ch)
         => (lambda (w) (loop (cons (substring str 0 (+ 1 w)) fields)
                              (substring str (+ 1 w)))))
         (else (apply ret str fields))))))
   
(define (separate-fields-before-char ch str ret)
     (let loop ((fields '())
            (str str))
       (cond
        ((string-rindex str ch)
         => (lambda (w) (loop (cons (substring str w) fields)
                    (substring str 0 w))))
        (else (apply ret str fields)))))

(define (string-prefix-predicate pred?)
     (lambda (prefix str)
       (and (<= (string-length prefix) (string-length str))
            (pred? prefix (substring str 0 (string-length prefix))))))
   
   (define string-prefix=? (string-prefix-predicate string=?))

(define (sans-surrounding-whitespace s)
   (let ((st 0)
     (end (string-length s)))
     (while (and (< st (string-length s))
         (char-whitespace? (string-ref s st)))
        (set! st (1+ st)))
     (while (and (< 0 end)
        (char-whitespace? (string-ref s (1- end))))
        (set! end (1- end)))
     (if (< end st)
    ""
     (substring s st end))))
 
(define (sans-trailing-whitespace s)
   (let ((st 0)
     (end (string-length s)))
     (while (and (< 0 end)
         (char-whitespace? (string-ref s (1- end))))
        (set! end (1- end)))
     (if (< end st)
     ""
     (substring s st end))))
 
(define (sans-leading-whitespace s)
   (let ((st 0)
     (end (string-length s)))
     (while (and (< st (string-length s))
        (char-whitespace? (string-ref s st)))
        (set! st (1+ st)))
     (if (< end st)
     ""
     (substring s st end))))
 
(define (sans-final-newline str)
   (cond
    ((= 0 (string-length str))
     str)
 
    ((char=? #\nl (string-ref str (1- (string-length str))))
     (substring str 0 (1- (string-length str))))
 
   (else str)))

;;; {String Fun: has-trailing-newline?}
;;;
   
(define (has-trailing-newline? str)
     (and (< 0 (string-length str))
                     (char=? #\nl (string-ref str (1- (string-length str))))))

(define (string-replace-substring str substring replacement)
     "Return a new string where every instance of @var{substring} in string
     @var{str} has been replaced by @var{replacement}. For example:
   
      @lisp
      (string-replace-substring \"a ring of strings\" \"ring\" \"rut\")
      @result{} \"a rut of struts\"
      @end lisp
      "
     (let ((sublen (string-length substring)))
       (with-output-to-string
         (lambda ()
           (let lp ((start 0))
             (cond
              ((string-contains str substring start)
               => (lambda (end)
                    (display (substring/shared str start end))
                    (display replacement)
                    (lp (+ end sublen))))
              (else
               (display (substring/shared str start)))))))))
