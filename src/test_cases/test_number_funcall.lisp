

;;
;; number add
;;

;; syntax error
;; invalid argument, whic is not a number.
;;
;(+ t)
;(+ nil)
;(+ 'a)
;(+ #\a)
;(+ (list 1))
;(+ (cons 1 2))

(+ 1)
(+ 1 2)
(+ 0 1 2 3 4 5 6 7 8 9)

(+ (car (list 1 2)) (if (eq 1 2) 1 2))

;; 45
(setq x (+ 1 2 3 4 5 6 7 8 9))

;; 46
(+ x 1)

;; 90
(+ x x)

;; 46
(setq x (+ x 1))


;;
;; number compare
;;

;; syntax error
;(>)
;(> 1 t)
;(> 1 #\a)

(> 1 2)
(> 2 1)
(> 2 1 3)
(> 9 8 7 6 5 4 3 2 1 0)
(> 0 8 7 6 5 4 3 2 1)
(> 8 7 6 5 4 1 2 0)

(> (car (list 1 2)) (if (eq 1 2) 1 2))


