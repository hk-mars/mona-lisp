

(eq #\a #\a)
(eq #\a #\z)
(eq 1 1)
(eq 1 10)

;; syntax error
;;(eq x y)

(setq x #\b)
(setq y #\b)
(eq x y)

(setq y #\c)
(eq x y)

(setq x #\c)
(eq x y)

(setq x 2)
(setq y 2)
(eq x y)

(setq y 3)
(eq x y)

(setq x 3)
(eq x y)

(eq x 3)
(eq 3 x)
(eq 4 y)
(eq y 4)

(eq 1 #\a)

(eq 'a 'b)
(eq 'a 'a)
(eq 'abc 'abc)
(eq 'abc 'abC)


(eq 1 (list 2))
(eq (list 1) 2)
(eq (list 1) nil)

;; TODO:
;; 1. failed when checking syntax
;; 2. Searching algorithm or ASG is invalid for this case
;(eq (list 1) (list 2))

