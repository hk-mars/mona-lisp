

* 
(read-from-string " 1 3 5" t nil :start 2)

3
5
* (read-from-string " 1 3 5" t nil :start 1)

1
3
* (read-from-string "(a b c d)")

(A B C D)
9
* (read-from-string "(a b c)")

(A B C)
7


