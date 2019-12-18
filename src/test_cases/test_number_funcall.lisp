

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



