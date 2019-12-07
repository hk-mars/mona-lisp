


;; error: no argument
;(atom )

;; error: 2 arguments
;(atom 1 2)


;;
;; number object
;;
(atom 9)
(atom +1)
(atom -0)
(atom +0)
(atom 9876543210)


;;
;; character object
;;
(atom #\a)
(atom #\z)
(atom #\space)

;;
;; bool object
;;
(atom nil)
(atom t)

;;
;; empty list
;;
(atom ())
(atom (  ))
(atom (
       ))

;; string object
(atom "hello")

;;
;; s-expression as object
;;
(atom '1)
(atom 'hello)
(atom '(1))
(atom '(1 2))
(atom '())
(atom '"hello")



