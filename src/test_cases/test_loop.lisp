

(setq sum 0)
(setq n 1)


;; loop forever
;(loop
; (print sum))

(loop
 (return nil))


(loop
 (if (eq n 101) (return 1) nil)
 (setq sum (+ sum n))
 (setq n (+ n 1)))

(print sum)



