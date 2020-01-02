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

