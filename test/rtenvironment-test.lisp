;;
;;  ANSI COMMON LISP: 25. Environment
;;

;;
;;  Variable -
;;
(deftest prompt-minus.1
  (find-symbol "-" 'common-lisp)
  - :external)


;;
;;  Variable +, ++, +++
;;
(deftest prompt-plus.1
  (find-symbol "+" 'common-lisp)
  + :external)

(deftest prompt-plus.2
  (find-symbol "++" 'common-lisp)
  ++ :external)

(deftest prompt-plus.3
  (find-symbol "+++" 'common-lisp)
  +++ :external)


;;
;;  Variable *, **, ***
;;
(deftest prompt-asterisk.1
  (find-symbol "*" 'common-lisp)
  * :external)

(deftest prompt-asterisk.2
  (find-symbol "**" 'common-lisp)
  ** :external)

(deftest prompt-asterisk.3
  (find-symbol "***" 'common-lisp)
  *** :external)


;;
;;  Variable /, //, ///
;;
(deftest prompt-slash.1
  (find-symbol "/" 'common-lisp)
  / :external)

(deftest prompt-slash.2
  (find-symbol "//" 'common-lisp)
  // :external)

(deftest prompt-slash.3
  (find-symbol "///" 'common-lisp)
  /// :external)


;;
;;  Macro TRACE
;;
(deftest trace-list.1
  (lisp-system:specialp 'lisp-system::*trace-list*)
  t)

(deftest trace-list.2
  lisp-system::*trace-list*
  nil)

(defun trace-test-1 ()
  :hello)

(deftest trace.1
  (trace)
  nil)

(deftest trace.2
  (trace trace-test-1)
  (trace-test-1))

(deftest trace.3
  (with-open-stream (*trace-output* (make-broadcast-stream))
    (trace-test-1))
  :hello)

(deftest trace.4
  (trace)
  (trace-test-1))

(deftest trace.5
  (progn
    (untrace)
    lisp-system::*trace-list*)
  nil)

(deftest trace.6
  (progn
    (trace trace-test-1)
    (prog1 lisp-system::*trace-list*
      (untrace)))
  (trace-test-1))

(deftest trace-error.1
  (handler-bind ((warning #'muffle-warning))
    (untrace)
    (values
      (trace no-such-function-name)
      lisp-system::*trace-list*))
  nil nil)


;;
;;  Macro UNTRACE
;;
(defun untrace-test-1 ()
  :hello)

(deftest untrace.1
  (progn
    (untrace)
    (untrace))
  nil)

(deftest untrace.2
  lisp-system::*trace-list*
  nil)

(deftest untrace.3
  (trace)
  nil)

(deftest untrace.4
  (progn
    (trace untrace-test-1)
    (untrace untrace-test-1))
  (untrace-test-1))

(defun untrace-test-2 ()
  10)

(defun untrace-test-3 ()
  20)

(deftest untrace.5
  (progn
    (trace untrace-test-1 untrace-test-2 untrace-test-3)
    (prog1 (untrace untrace-test-2 untrace-test-3)
      (untrace)))
  (untrace-test-2 untrace-test-3))

(deftest untrace.6
  (progn
    (trace untrace-test-1 untrace-test-2 untrace-test-3)
    (untrace untrace-test-2 untrace-test-3)
    (prog1 lisp-system::*trace-list*
      (untrace)))
  (untrace-test-1))

(deftest untrace-error.1
  (handler-bind ((warning #'muffle-warning))
    (untrace)
    (values
      (untrace no-such-function-name)
      lisp-system::*trace-list*))
  nil nil)


;;
;;  Macro STEP
;;
(deftest-error! step-error.1
  (eval '(step)))

(deftest-error! step-error.2
  (eval '(step 10 20)))



;;
;;
;;
(deftest room.1
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (room))
  nil)

(deftest room.2
  (let ((x (with-output-to-string (*standard-output*) (room))))
    (and (stringp x)
         (< 0 (length x))))
  t)

(deftest room.3
  (let ((x (with-output-to-string (*standard-output*) (room :default))))
    (and (stringp x)
         (< 0 (length x))))
  t)

(deftest room.4
  (let ((x (with-output-to-string (*standard-output*) (room t))))
    (and (stringp x)
         (< 0 (length x))))
  t)

(deftest room.5
  (let ((x (with-output-to-string (*standard-output*) (room nil))))
    (and (stringp x)
         (< 0 (length x))))
  t)

(defun disassemble-test ()
  :hello)

(deftest disassemble.1
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (disassemble #'car))
  nil)

(deftest disassemble.2
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (disassemble #'disassemble-test))
  nil)

(deftest disassemble.3
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (disassemble 'car))
  nil)

(deftest disassemble.4
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (disassemble 'disassemble-test))
  nil)

(deftest disassemble.5
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (disassemble #'(setf car)))
  nil)

(deftest disassemble.6
  (null (search
          "LAMBDA"
          (with-output-to-string (*standard-output*)
            (disassemble 'disassemble-test))))
  nil)

