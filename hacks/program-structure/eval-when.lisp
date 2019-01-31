

;;
;; This causes the call to set-macro-character to be executed in the compiler's 
;; execution environment, thereby modifying its reader syntax table.
;;
* (eval-when (compile load eval) 
  (set-macro-character #\$ #'(lambda (stream char) 
                               (declare (ignore char)) 
                               (list 'dollar (read stream)))))
T




