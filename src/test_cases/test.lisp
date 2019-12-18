
;(setq x 1)

;(setq x #\a)

;(setq x nil)

;(setq x t)

;(setq x x)

;(setq x (list 1))

;(setq x (if t 1 0))


;(atom )
;(atom 1 2)
;(atom 1)
;(atom +1)
;(atom -0)
;(atom +0)
;(atom 9876543210)

;(atom #\a)
;(atom #\space)

;(atom nil)
;(atom t)
;(atom ())
;(atom (  ))
;(atom (
;       ))


;(atom "hello")
;(atom 'hello)

;(atom '(1))

;'(1)
;'  (1 2)

;'(1 (2 3))

;'(loop (setq x 1) (+ 3 x))

;(atom '(1))
;(atom '(1 2))
;'hello
;(atom 'hello)
;'"hello""
;(atom '"hello")
;(atom '(
;	))
;(atom )


;(eq #\a #\a)
;(eq #\a #\z)
;(eq 1 1)
;(eq 1 10)
;(eq 1 x)
;(eq x y)
;(eq 1 #\a)
;(eq 'a 'b)
;(eq 'a 'a)
;(eq 'abc 'abc)
;(eq 'abc 'abC)
;(eq (cons 1 2) (cons 1 2))

;; syntax error
;(cons )
;(cons 1)
;(cons 1 2 3)

;(cons 1 2)
;(cons 1 #\a)
;(cons 'a 'b)
;(cons (cons 1 2) 3)
;(cons 0 (cons 1 2))
;(cons (cons 1 2) (cons 3 4))


;(car (cons 1 2))
;(car (cons (cons 1 2) 3))
;(car (cons (cons (cons 1 2) 3) 4))

;; syntax error
;(car (cons 1))


;(cdr (cons 1 2))
;(cdr (cons 1 (cons 2 3)))
;(cdr '(1))

;; syntax error
;(cdr)
;(cdr 1)
;(cdr (cons 1 (cons 2)))

;(eq (car (cons 1 2)) 1)
;(eq (cdr (cons 1 2)) 2)
;(eq (cdr (cons 1 (cons 2 3))) 4)
;(eq (cdr (cons 1 #\a)) 2)

;(cons (cons 1 2) 3)
;(cons (cons 1 2) (cons 3 4))

;(eq (cons 1 2) 3)
;(eq (cons 1 2) (cons 1 2))
;(eq nil nil)
;(eq nil t)


;(atom (car (cons 1 2)))
;(atom (car (cons (cons 1 2) 3))) 
;(atom (cdr (cons 1 2)))
;(atom (cdr (cons 1 (cons 2 3))))



