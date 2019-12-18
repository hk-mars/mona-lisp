
;;
;; syntax error
;;
;(if )
;(if t )
;(if (eq 1 1))
;(if t t nil nil)

(if t t)
(if t nil)
(if nil t)
(if t t nil)
(if nil t nil)
(if 0 t nil)

(if (eq #\a #\b) (list t) (list nil))
(if (eq #\c #\c) (list t) (list nil))

(setq x #\b)
(if (eq x #\b) 1 0)
(setq x #\a)
(if (eq x #\b) 1 0)

(if (eq 1 1) 1 0)
(if (eq 1 2) t nil)

(setq y #\a)
(if (eq x y) #\a #\b)
(if (eq x y) x y)
(setq y #\b)
(if (eq x y) #\a #\b)
(if (eq x y) x y)

(if t x y)
(if nil x y)


(setq x t)

(if (eq x nil) nil x)
(if (eq x nil) nil (list x))
(if (eq x t) x nil)
(if (eq x t) (list x) nil)
(if (eq x t) (setq y 1) nil)
(setq y (if (eq x t) 2 nil))

(if (car (cons nil t)) 'end nil)
(if (cdr (cons nil 1)) 'end nil)

(if (car (list 1 2)) 'one 'unknown)

(if (eq (car (cdr (list 1 2))) 2) 'two 'unknown)

(setq num-name (if (eq (car (cdr (list 1 2))) 2) 'two 'unknown))


