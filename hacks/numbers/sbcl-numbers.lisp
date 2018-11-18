
;;
;; predicates functions of numbers
;;

* z    

3
* (zerop z)

NIL
* (plusp z)

T
* (minusp z)

NIL
* (oddp z)

T
* (evenp z)

NIL


;;
;; comparisions on numbers.
;;

* (= 3 3)

T
* (/= 3 3)

NIL
* (= 3 3 3)

T
* (= 3 3 4)

NIL
* (/= 1 2 3)

T
* (< 1 2 3)

T
* (> 4 3 2)

T
* (<= 1 1 2)

T
* (>= 3 3 4 5)

NIL
* (>= 4 4 3 2)

T


* (max 6 12)

12
* (min 6 12)

6
* (max -6 -12)

-6
* (max 5.0 2)

5.0
* (min 5.0 1)

1
* (max 1.0s0 7.0d0)

7.0d0
* (max 3 1 1.0s0 7.0d0)

7.0d0
* (min 3 1 1.0s0 7.0d0)

1


;;
;; arithmetic operations
;;

* (+ 1 2 3)

6
* (- 3 2)

1
* (- 3 2 1)

0
* (* 1 2 3)

6
* (* 1)

1
* (* 0)

0
* (* 1 3) 

3
* (/ 1 3)

1/3
* (/ 1 2 3)

1/6
* (/ -8)

-1/8
* (/ 1.0 2)

0.5
* (/ 1 2.0)

0.5
* (1+ 1)

2
* (1+ -1)

0
* (1- 1)

0
* (setq n 0)
0
* (incf n)

1
* (incf n 30)
31
* n   

31
* (decf n)

30
* (decf -5)

35
* (gcd 91 -49)

7
* (lcm 14 35)

70

