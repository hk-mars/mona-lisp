
* (defvar *avoid-registers* nil "Compilation control switch #43")

*AVOID-REGISTERS*

* (defvar *compiler-switch-number* 10)

*COMPILER-SWITCH-NUMBER*
* (defvar *avoid-registers* nil 
  #.(format nil "Compilation control switch #~D" 
            (incf *compiler-switch-number*)))

*AVOID-REGISTERS*
* (defvar *avoid-registers* nil 
  (format nil "Compilation control switch #~D" 
          (incf *compiler-switch-number*)))

*AVOID-REGISTERS*


