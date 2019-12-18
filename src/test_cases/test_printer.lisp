
;; Standard Characters
;; ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? 
;; @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ 
;; ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~

;; char 0~9
(print #\0)
(print #\1)
(print #\2)
(print #\3)
(print #\4)
(print #\5)
(print #\6)
(print #\7)
(print #\8)
(print #\9)

;; a~z, A~Z
(print #\a)
(print #\b)
(print #\c)
(print #\d)
(print #\e)
(print #\f)
(print #\g)
(print #\h)
(print #\i)
(print #\j)
(print #\k)
(print #\l)
(print #\m)
(print #\n)
(print #\o)
(print #\p)
(print #\q)
(print #\r)
(print #\s)
(print #\t)
(print #\u)
(print #\v)
(print #\w)
(print #\x)
(print #\y)
(print #\z)

(print #\A)
(print #\B)
(print #\C)
(print #\D)
(print #\E)
(print #\F)
(print #\G)
(print #\H)
(print #\I)
(print #\J)
(print #\K)
(print #\L)
(print #\M)
(print #\N)
(print #\O)
(print #\P)
(print #\Q)
(print #\R)
(print #\S)
(print #\T)
(print #\U)
(print #\V)
(print #\W)
(print #\X)
(print #\Y)
(print #\Z)


;; " # $ % & ' ( ) * + , - . /
;; : ; < = > @ \ ^ _ `| ~ 
(print #\")
(print #\#)
(print #\$)
(print #\%)
(print #\&)
(print #\')
;(print #\()
;(print #\))
(print #\*)
(print #\+)
(print #\,)
(print #\-)
(print #\.)
(print #\/)
(print #\:)
(print #\;)
(print #\<)
(print #\=)
(print #\>)
(print #\@)
(print #\\)
(print #\^)
(print #\_)
(print #\`)
;(print #\|)
(print #\~)


;; characters not used in Common Lisp
;; [  ]  {  }  ?  !


;; semi-standard characters
;; #\Backspace  #\Tab  #\Linefeed  #\Page  #\Return  #\Rubout
(print #\space)
(print #\tab)
(print #\linefeed)
(print #\newline)


;;
;; print variable
;;
(setq x 100)
(print x)


;;
;; print const
;;
(print nil)
(print t)


;;
;; print integer number
;; maximum value of SINGED INT32: 2147483647 
;; minimum value of SINGED INT32: -2147483648 
;; maximum value of SINGED INT64: 9223372036854775807 
;; minimum value of SINGED INT64: -9223372036854775808
;; maximum value of fixnum of 64bits OS: 9223372036854775807 
;; minimum value of fixnum of 64bits OS: -9223372036854775808
;;
(print 0)
(print 1)
(print 2)
(print 3)
(print 4)
(print 5)
(print 6)
(print 7)
(print 8)
(print 9)
(print 2147483647)
(print 2147483648)
(print 9876543210)
(print 9223372036854775807)
(print -9223372036854775808)
;; TODO: big number
;(print -9223372036854775809)
;(print 9223372036854775808)


;;
;; print symbol
;;
(print 'hello)

;; TODO: the expression of quote-form which could be printed as a list
(print '(1 2))


(setq x 1)
(print x)
;; TODO: print a LIST
(setq x (list 1 2))
(print x)


;; TODO: print a CONS
;(setq x (cons 1 2))
;(print x)
