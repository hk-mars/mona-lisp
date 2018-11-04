
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
