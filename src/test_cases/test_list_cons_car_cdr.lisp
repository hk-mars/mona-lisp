

;;
;; combine car, cdr, cons and list
;;

(car (list))
(cdr (list))
(car (list 1 2))
(car (list (list 1 2) 3))
(cdr (list 1 2))
(cdr (list 1 (list 2 3)))
(cons (car (list 1 2)) (car (cdr (list 1 2))))
(list (car (list 1 2)) (car (cdr (list 1 2))))

(list (cdr (list 1 2)))
(list (cdr nil) (car nil))
(list (cdr (list 1 2)) (car (list 1 2)))
(cdr (list 0 1 3))
(cdr (list 0 1 2 3 4 5 6 7 8 (cons 9 #\a)))
(car (list 1 2))
(car (cons 1 2))
(car (cdr (list 1 2)))
(car (cdr (list 5 6 7 8)))
(cdr (list 0 (car (list 1 2)) 3))
(list (car (cdr (list 1 2))))
(list (car (cdr (list 1 2))) (car (list 1 2)))




