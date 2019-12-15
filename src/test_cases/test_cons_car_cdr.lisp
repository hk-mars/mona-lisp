
(car (cons 1 2))
(car (cons (cons 1 2) 3))
(car (car (cons (cons 1 2) 3)))

(cdr (car (cons (cons 1 2) 3)))

(cons (car (cons (cons 1 2) 3)) 3)

(cons 0 (cdr (cons 0 (cons 1 2))))


;; (cons (car x) (cdr x)) == x
(cons (car (cons (cons 1 2) 3)) (cdr (cons (cons 1 2) 3)))
