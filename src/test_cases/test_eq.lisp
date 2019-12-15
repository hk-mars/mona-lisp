

(eq #\a #\a)
(eq #\a #\z)
(eq 1 1)
(eq 1 10)

(eq 1 #\a)

(eq 'a 'b)
(eq 'a 'a)
(eq 'abc 'abc)
(eq 'abc 'abC)

(eq nil nil)
(eq nil t)

;; syntax error
;(eq x y)

;(setq x #\b)
;(setq y #\b)
;(eq x y)

;(setq y #\c)
;(eq x y)

;(setq x #\c)
;(eq x y)

;(setq x 2)
;(setq y 2)
;(eq x y)

;(setq y 3)
;(eq x y)

;(setq x 3)
;(eq x y)

;(eq x 3)
;(eq 3 x)
;(eq 4 y)
;(eq y 4)

;(eq 1 (list 2))
;(eq (list 1) 2)
;(eq (list 1) nil)


(eq (cons 1 2) 3)
(eq (cons 1 2) (cons 1 2))

(eq (car (cons 1 2)) 1)
(eq (cdr (cons 1 2)) 2)
(eq (cdr (cons 1 (cons 2 3))) 4)
(eq (cdr (cons 1 (car (cons 2 3)))) 2)

(eq (car (cons (cons 1 2) 3)) (cons 1 2))


