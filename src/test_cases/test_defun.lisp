

(defun sum (n m)
  (setq r 0)
  (loop
   (if (> n m) (return 0))
   (setq r (+ r n))
   (if (eq n m) (return r) nil)
   (setq n (+ n 1))))

(sum 1 10)

