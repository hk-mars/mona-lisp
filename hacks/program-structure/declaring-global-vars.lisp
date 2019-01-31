
* (defvar *avoid-registers* nil "Compilation control switch #43")

*AVOID-REGISTERS*

* (defvar *compiler-switch-number* 10)

*COMPILER-SWITCH-NUMBER*

;;
;; correct, because the call to **format** is evaluated at read time
;; (only at load time, or when interpreted but not compiled).
;;
* (defvar *avoid-registers* nil 
  #.(format nil "Compilation control switch #~D" 
            (incf *compiler-switch-number*)))

;;
;; erroneous because the all to format is not a literal string.
;;
*AVOID-REGISTERS*
* (defvar *avoid-registers* nil 
  (format nil "Compilation control switch #~D" 
          (incf *compiler-switch-number*)))

*AVOID-REGISTERS*


