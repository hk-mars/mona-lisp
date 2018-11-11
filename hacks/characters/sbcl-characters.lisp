
;;
;; Predicate functions of characters

* (setf z #\a)     

#\a
* (alpha-char-p z)

T
* (upper-case-p z)

NIL
* (lower-case-p z)

T
* (both-case-p z)

T
* (digit-char-p z)

NIL
* (alphanumericp z)   

T
* (char= z #\a)

T
* (char= z #\b)

NIL
* (char-equal z #\A)

T

;;
;; char-upcase char
;; char-downcase char
;;

* (char-upcase #\b)

#\B
* (char-downcase #\B)

#\b



;;
;; digit-char weight &optional (radix 10) (font 0)
;;

* (digit-char 7)

#\7
* (digit-char 12)

NIL
* (digit-char 12 16)

#\C
* (digit-char 6 2)

NIL
* (digit-char 1 2)

#\1
* (digit-char 0 2)

#\0


;;
;; char-int char
;;

* (char-int #\1)

49
* (char-int #\a)    

97


;;
;; char-name char
;; name-char name
;;

* (char-name #\a)

"LATIN_SMALL_LETTER_A"


* (name-char "LATIN_SMALL_LETTER_B")

#\b



