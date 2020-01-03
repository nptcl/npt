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

