
;'abc
;'\A\B\C

;"hello world"
;"\"\'\t"
;""
;"\"APL\\360?\" he cried."

;(+ 1 1)

;(+ 1b5000 777777q)

;(1.7J  -3/4+6.7J  12/25/83)

;(27^19 3^4/5 6//7 3.1.2.6 ^-43^)

;(3.141_592_653_589_793_238_4 -3.7+2.6i-6.17j+19.6k)

;; symbols but not likely numbers
;(/ /5 + 1+  1- foo+  ab.cd _ ^ ^/- )

;; list, number add
;(+ 1 2)

;(+ (* (/ +1 2) -34) (- 567 890))

;(- 1 2)

;(* 2 3)

;(/ 3 0)

;(* 1 (- 2 3))

;(* (/ 200 (+ 2 (- 3 1)) 10) 5 2 2)

;(+ (- 1 0) (+ (+ (+ 1 0) (+ 1 0)) 2) (+ 3 3))

;(setq x (+ 1 2))
;(setq x 1 y 2)
;(setq x (+ 1 2) y (* 2 3))

;(cons (car (list 1 2)) (cdr (list 3 4)))

;(list 1 2 3)

;(car (list 1 2 3))

;(cdr (list 1 2 3))

;(cons (list 4 5) (list 1 2 3))

;; wrong car syntax
;(car (list 1 2 3) (list 3 5))
;(list 1 2 3)

;(list (list 1 2 3) 4)

;(list (list 1 2) (list 3 4) 5)

;#\a
;#\'
;#\space

;(setq x #\a)

;(setq y #\tab)

;(setq z 'z)


;(car (list 1 2 3))

;(cdr (list 1 2 3))

;(car (cdr (list 1 2 3)))

;()

;(list x 2 3)

; y is unbound
;(list y 2 3)


;(defconstant myname
;  ; this is my english name
;  "Toby M.")

;(if (eq #\a #\b) (list t) (list nil))

;(eq #\a #\a)

;(eq #\a #\b)

;(eq #\a 2)

;(setq x #\a)

;(setq y #\a)

;(eq x y)

;(eq x #\a)
;(eq #\a y)

;(eq 2 2)

;(setq z 100)

;(eq z 123)

;(if (eq x #\b) 1 0)

;(if (eq z 123) (list 1 2) 0)

;(if z (if (eq z 123) (list 1 2) 3) 0)

;(if (eq z 100) 1 (list 2))

;(list (if t 1 0) 2)
;(list (if nil 1 0) 2)

; z is a self-evaluating-form in the if-form
;(list (if z z 0) 2)


;;
;; evaluate the formula "1+2+3+...+n"
;;
;(setq sum 0)
;(setq n 1)
;(loop
; (if (eq n 101) (return (list n sum)) nil)
; (setq sum (+ sum n))
; (setq n (+ n 1)))

;;
;; three loops
;;
;(loop
; (loop
;  (loop
;   (list 1)
;   (return 1))
;  (list 2)
;  (return 2))
; (list 3)
; (return 3))
;
;(setq x (list 1 2))

;;
;; function: sum
;; 1. evaluate the formula "n+(n+1)+(n+2)+...+m"
;;
;(defun sum (n m)
;  (setq r 0)
;  (loop
;   (if (> n m) (return 0))
;   (setq r (+ r n))
;   (if (eq n m) (return r) nil)
;   (setq n (+ n 1))))

;(sum 1 100)

;(+ 1 2)
;(< 1 2)
;(<= 1 2)
;(> 1 2)
;(>= 1 2)
;(= 1 1)
;(/= 1 2)
;(!= 1 2)

;(setq a (list 1 2))

;(setq z (setq y 0 x (list 1 2)))


;(defstruct person
;  (name age sex))


;;
;; @body are not evaluated before ",@body", but in a function call,
;; all the arguments are evaluated before the function is even invoked.
;; This macro could save the function calls and also the codes are more integrated.
;; I plan to implement this feature...
;;
;(defmacro while (test body)
;  `(do ()
;       ((not ,test))
;       ,@body))


(defmacro do (test do-one do-next)
  `(loop
       (if (eq ,test nil) (return nil))
       ,do-one ,do-next))


;(do (< x 100)
;  (print x)
;  (setq x (+ x 1)))


