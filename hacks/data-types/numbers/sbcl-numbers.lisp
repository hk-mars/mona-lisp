
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

;;
;; type converstions and component extractions
;;

* (float 0)

0.0
* (float 1)

1.0
* (rational 3/9)

1/3
* (rational 2)

2
* (complex 1 2)

#C(1 2)
* (realpart (complex 1 2))

1


;;
;; logical operations on numbers
;;

 (logior 1 0)

1
* (logxor 1 1)

0
* (logand 1 1)            

1
* (logand 0 1)

0
* (logcount 15)

4



;;
;; random operations on numbers
;;

* (random 10)

5
* *random-state*

#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                     :INITIAL-CONTENTS
                                     '(0 2567483615 7 2601187879 3919438689
                                       2270374771 3254473187 705526435
                                       752899028 4259895275 1635503293
                                       287311810 3348146311 587101971
                                       1133963260 197444494 1569747226


* (make-random-state t)

#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                     :INITIAL-CONTENTS
                                     '(0 2567483615 624 2147483648 1475954278
                                       3325869972 334603751 3153596043
                                       628567236 3524313949 2634605248
                                       3638273735 3550264641 727190391

