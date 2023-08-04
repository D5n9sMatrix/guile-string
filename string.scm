;; string.scm

;; length life and live
(string-length "life")
(string-length "live")
;; reference life
(string-ref "life" 2)
;; by holy present
(substring "life" 0 0)
;; bealive acredit's
(string-take "life" 4)
(string-take "live" 4)
;; by holy present rain drop in
(string-drop "rain" 2)
;; open climatic refresh cold ice9
(string-take-right "ice" 3)
;; open climatic refresh rain cold ice drizzle
(string-drop-right "rain" 2)
;; roku channels string butterfly
(string-pad "butterfly" 9)
;; refresh cliamtic rain cold ice drizzle
(string-trim "ice")
;; refresh climatic rain cold ice drizzle
(string-trim-right "ice")
;; refresh climatic rain cold ice drizzle
(string-trim-both "ice"
                  (lambda (c)
                             (or (eqv? c #\x)
                                 (eqv? c #\y))))

;; string ambient ice climatic
(string? "ice")
;; string ambient ice null hot climatic
(string-null? "")
;; string any effect cliamtic refrence
(string #\j #\e #\s #\u #\s)
;; position any effect rain refrence cat
(list->string '(#\c #\a #\t))
;; position any effect rain refrence tac
(reverser-list->string '(#\c #\a #\t))
;; position any effect rain refrence make string
(make-string 27)
;; position any effect rain string list  #\i #\c #\e
(string->list "ice")
;; position any effect rain string true #\t
(string-split "any" #\t)
;; position any effect rain string length
(string-length "any")
;; position any effect rain string reference #\y
(string-ref "any" 2)
;; position any effect rain string set!
(string-set! '("any" 2 #\t))
;; loading output ...
#\t
;; start rain ice string define
(define any (string-copy "rain-ice"))
(substring-fill! any 1 3 #\r)
(substring-fill! any 2 3 #\r)




