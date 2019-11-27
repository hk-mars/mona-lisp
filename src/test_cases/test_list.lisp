
;;
;; nil list
;;
()
(list )
(list )

;;
;; integer list
;;
(list 0)
(list 1)
(list 2)
(list 3)
(list 4)
(list 5)
(list 6)
(list 7)
(list 8)
(list 9)
(list 12 34 56 78 90)
(list 0 1 2 3 4 5 6 7 8 9 9876543210)

;;
;; character list
;;
(list #\a)
(list #\b)
(list #\c)
(list #\d)
(list #\e)
(list #\f)
(list #\g)
(list #\h)
(list #\i)
(list #\j)
(list #\k)
(list #\l)
(list #\m)
(list #\n)
(list #\o)
(list #\p)
(list #\q)
(list #\r)
(list #\s)
(list #\t)
(list #\u)
(list #\v)
(list #\w)
(list #\x)
(list #\y)
(list #\z)
(list #\A)
(list #\B)
(list #\C)
(list #\D)
(list #\E)
(list #\F)
(list #\G)
(list #\H)
(list #\I)
(list #\J)
(list #\K)
(list #\L)
(list #\M)
(list #\N)
(list #\O)
(list #\P)
(list #\Q)
(list #\R)
(list #\S)
(list #\T)
(list #\U)
(list #\V)
(list #\W)
(list #\X)
(list #\Y)
(list #\Z)
(list #\0)
(list #\1)
(list #\2)
(list #\3)
(list #\4)
(list #\5)
(list #\6)
(list #\7)
(list #\8)
(list #\9)

;; " # $ % & ' ( ) * + , - . /
;; : ; < = > @ \ ^ _ `| ~ 
(list #\")
(list #\#)
(list #\$)
(list #\%)
(list #\&)
(list #\')
;(list #\()
;(list #\))
(list #\*)
(list #\+)
(list #\,)
(list #\-)
(list #\.)
(list #\/)
(list #\:)
(list #\;)
(list #\<)
(list #\=)
(list #\>)
(list #\@)
(list #\\)
(list #\^)
(list #\_)
(list #\`)
;(list #\|)
(list #\~)

(list #\A #\B #\C)

;; semi-standard characters
;; #\Backspace  #\Tab  #\Linefeed  #\Page  #\Return  #\Rubout
(list #\space)
(list #\tab)
(list #\linefeed)
(list #\newline)

;;
;; integer & character
;;
(list 1 2 #\a #\space)

;;
;; symbol & interger & character
;;
(list 'a 'b 'abcdefg 12 #\b)


;;
;; nil and t
;;
(list nil)
(list t)
(list nil t)


;;
;; nested list
;;
;;

(list (list 1))
(list (list 2))
(list (list 1 2) 3)
(list (list 1 2) (list 3 4))
(list (list 1) (list 1 2) 3 (list 4 #\a) (list #\b 5 6))
