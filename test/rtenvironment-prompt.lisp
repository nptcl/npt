;;
;;  ANSI COMMON LISP: 25. Environment
;;
(deftest prompt-minus.1
  (find-symbol "-" 'common-lisp)
  - :external)

(deftest prompt-plus.1
  (find-symbol "+" 'common-lisp)
  + :external)

(deftest prompt-plus.2
  (find-symbol "++" 'common-lisp)
  ++ :external)

(deftest prompt-plus.3
  (find-symbol "+++" 'common-lisp)
  +++ :external)

(deftest prompt-asterisk.1
  (find-symbol "*" 'common-lisp)
  * :external)

(deftest prompt-asterisk.2
  (find-symbol "**" 'common-lisp)
  ** :external)

(deftest prompt-asterisk.3
  (find-symbol "***" 'common-lisp)
  *** :external)

(deftest prompt-slash.1
  (find-symbol "/" 'common-lisp)
  / :external)

(deftest prompt-slash.2
  (find-symbol "//" 'common-lisp)
  // :external)

(deftest prompt-slash.3
  (find-symbol "///" 'common-lisp)
  /// :external)

(deftest describe.1
  (< 1 (length
         (with-output-to-string (*standard-output*)
           (describe 100))))
  t)

(deftest describe.2
  (progn
    (defclass describe-test () ())
    (defmethod describe-object ((object describe-test) stream)
      (declare (ignore object))
      (format stream "Hello"))
    (with-output-to-string (*standard-output*)
      (describe (make-instance 'describe-test))))
  "Hello")

(deftest time.1
  (< 0 (length
         (with-output-to-string (*trace-output*)
           (time (values 10 20 30)))))
  t)

(deftest time.2
  (with-open-stream (*trace-output* (make-broadcast-stream))
    (time (values 10 20 30)))
  10 20 30)

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

(defun trace-test1 ()
  :hello)

(deftest trace.1
  (trace)
  nil)

(deftest trace.2
  (trace trace-test1)
  (trace-test1))

(deftest trace.3
  (with-open-stream (*trace-output* (make-broadcast-stream))
    (trace-test1))
  :hello)

(deftest trace.4
  (trace)
  (trace-test1))

(deftest untrace.1
  (progn
    (untrace)
    (untrace))
  nil)

(deftest untrace.2
  (trace)
  nil)

(deftest untrace.3
  (progn
    (trace trace-test1)
    (untrace trace-test1))
  (trace-test1))

(deftest untrace.4
  (trace)
  nil)

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

