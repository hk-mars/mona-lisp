

;;
;; eval form
;;

* (eval (list '+ 1 2))

3
```


;;
;; 1. eval car of list: (list 'cdr (car '((quote (a . b)) c))) => (cdr (quote (a . b)))
;; 2. eval cdr of list: (cdr (quote (a . b))) => (cdr (a . b)) => b
;;

* (eval (list 'cdr (car '((quote (a . b)) c))))

B

;;
;; quote treat the elements of a list as symbol but not value
;;
* (quote (a . b))

(A . B)

This is different with:

;;
;; 1. eval list: (list a b) => (10 2)
;; 2. eval cdr of list: (list 'cdr (quote (10 2)))) => (2)
;;
* (eval (list 'cdr (quote (list a b)))))

(2)  ; value of b is 2
