

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


