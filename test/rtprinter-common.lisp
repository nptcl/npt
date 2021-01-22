;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  Function COPY-PPRINT-DISPATCH
;;
(deftest copy-pprint-dispatch.1
  (let ((x (copy-pprint-dispatch)))
    (values
      (typep x 'lisp-system::print-dispatch)
      (eq x *print-pprint-dispatch*)))
  t nil)

(deftest copy-pprint-dispatch.2
  (let ((x (copy-pprint-dispatch *print-pprint-dispatch*)))
    (values
      (typep x 'lisp-system::print-dispatch)
      (eq x *print-pprint-dispatch*)))
  t nil)

(deftest copy-pprint-dispatch.3
  (let ((x (copy-pprint-dispatch nil)))
    (values
      (typep x 'lisp-system::print-dispatch)
      (eq x *print-pprint-dispatch*)))
  t nil)

(defstruct copy-pprint-dispatch-1)

(deftest copy-pprint-dispatch.4
  (let ((x (copy-pprint-dispatch)))
    (set-pprint-dispatch
      'copy-pprint-dispatch-1
      (lambda (stream object)
        (declare (ignore object))
        (format stream "Hello"))
      0 x)
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch x)))
      (princ-to-string
        (make-copy-pprint-dispatch-1))))
  "Hello")

(deftest-error! copy-pprint-dispatch-error.1
  (eval '(copy-pprint-dispatch nil nil)))

(deftest-error! copy-pprint-dispatch-error.2
  (eval '(copy-pprint-dispatch 10))
  type-error)


;;
;;  Function PPRINT-DISPATCH
;;
(deftest pprint-dispatch.1
  (with-default-print
    (destructuring-bind (call callp)
      (multiple-value-list (pprint-dispatch 10))
      (values (typep call 'function) callp)))
  t nil)

(deftest pprint-dispatch.2
  (with-default-print
    (with-output-to-string (x)
      (funcall (pprint-dispatch 10) x 20)))
  "20")

(deftest pprint-dispatch.3
  (with-default-print
    (with-output-to-string (x)
      (funcall (pprint-dispatch :hello *print-pprint-dispatch*) x 'aaa)))
  "AAA")

(defstruct pprint-dispatch-1)

(deftest pprint-dispatch.4
  (with-default-print
    (let ((x (copy-pprint-dispatch))
          check)
      (set-pprint-dispatch
        'pprint-dispatch-1
        (lambda (stream object)
          (declare (ignore object))
          (format stream "Hello"))
        0 x)
      (values
        (with-output-to-string (stream)
          (let ((inst (make-pprint-dispatch-1)))
            (multiple-value-bind (call found) (pprint-dispatch inst x)
              (setq check found)
              (funcall call stream inst))))
        check)))
  "Hello" t)

(defstruct pprint-dispatch-2)

(deftest pprint-dispatch.5
  (with-default-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch))
          check)
      (declare (ignorable *print-pprint-dispatch*))
      (set-pprint-dispatch
        'pprint-dispatch-2
        (lambda (stream object)
          (declare (ignore object))
          (format stream "Hello")))
      (values
        (equal
          (with-output-to-string (stream)
            (let ((inst (make-pprint-dispatch-2)))
              (multiple-value-bind (call found) (pprint-dispatch inst nil)
                (setq check found)
                (funcall call stream inst))))
          "Hello")
        check)))
  nil nil)

(deftest-error! pprint-dispatch-error.1
  (eval '(pprint-dispatch)))

(deftest-error! pprint-dispatch-error.2
  (eval '(pprint-dispatch 10 *print-pprint-dispatch* nil)))

(deftest-error pprint-dispatch-error.3
  (eval '(pprint-dispatch 10 20))
  type-error)


;;
;;  Standard Generic Function PRINT-OBJECT
;;
(defclass print-object-test-1 ()
  ((value :initarg :value :initform nil)))

(defmethod print-object ((object print-object-test-1) stream)
  (with-slots (value) object
    (if *print-escape*
      (format stream "Hello1: ~A" value)
      (format stream "Hello2: ~A" value))))

(deftest print-object.1
  (with-output-to-string (stream)
    (let ((*print-escape* nil)
          (inst (make-instance 'print-object-test-1 :value 10)))
      (print-object inst stream)))
  "Hello2: 10")

(deftest print-object.2
  (princ-to-string
    (make-instance 'print-object-test-1 :value 20))
  "Hello2: 20")

(deftest print-object.3
  (prin1-to-string
    (make-instance 'print-object-test-1 :value 30))
  "Hello1: 30")

(defclass print-object-test-2 ()
  ((value :initarg :value :initform nil)))

(deftest print-object.4
  (subseq
    (prin1-to-string
      (make-instance 'print-object-test-2 :value 30))
    0 22)
  "#<PRINT-OBJECT-TEST-2 ")

(defstruct print-object-test-3 value)

(deftest print-object.5
  (prin1-to-string
    (make-print-object-test-3 :value 50))
  "#S(PRINT-OBJECT-TEST-3 :VALUE 50)")


;;
;;  Macro PRINT-UNREADABLE-OBJECT
;;
(deftest print-unreadable-object.1
  (with-output-to-string (stream)
    (print-unreadable-object (t stream)
      (princ "HELLO" stream)))
  "#<HELLO>")

(deftest print-unreadable-object.2
  (with-output-to-string (stream)
    (print-unreadable-object ('(a b) stream :type t)
      (princ "HELLO" stream)))
  "#<CONS HELLO>")

(deftest print-unreadable-object.3
  (remove-if
    (lambda (x)
      (or (char<= #\0 x #\9) (char<= #\a x #\f) (char<= #\A x #\F)))
    (with-output-to-string (stream)
      (print-unreadable-object ('(a b) stream :identity t)
        (princ "WXYZ" stream))))
  "#<WXYZ #x>")

(deftest print-unreadable-object.4
  (remove-if
    (lambda (x)
      (or (char<= #\0 x #\9) (char<= #\a x #\f) (char<= #\A x #\F)))
    (with-output-to-string (stream)
      (print-unreadable-object ('(a b) stream :identity t :type t)
        (princ "WXYZ" stream))))
  "#<ONS WXYZ #x>")

(deftest print-unreadable-object.5
  (remove-if
    (lambda (x)
      (or (char<= #\0 x #\9) (char<= #\a x #\f) (char<= #\A x #\F)))
    (with-output-to-string (stream)
      (print-unreadable-object
        ('(a b) stream :identity t :type t :identity nil :type nil)
        (princ "WXYZ" stream))))
  "#<ONS WXYZ #x>")

(deftest-error print-unreadable-object.6
  (let ((*print-readably* t))
    (with-output-to-string (stream)
      (print-unreadable-object (t stream)
        (princ "HELLO" stream))))
  print-not-readable)


;;
;;  Function SET-PPRINT-DISPATCH
;;
(defun set-pprint-dispatch-test (x)
  (lambda (s o)
    (declare (ignore o))
    (princ x s)))

(deftest set-pprint-dispatch.1
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (set-pprint-dispatch-test "Hello")))
        (set-pprint-dispatch 'integer x)
        (eq (pprint-dispatch 10) x))))
  t)

(deftest set-pprint-dispatch.2
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (set-pprint-dispatch-test "Hello")))
        (set-pprint-dispatch 'integer x)
        (eq (pprint-dispatch :hello) x))))
  nil)

(deftest set-pprint-dispatch.3
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (set-pprint-dispatch-test "Hello")))
        (set-pprint-dispatch 'integer x)
        (values
          (princ-to-string 200)
          (princ-to-string :aaa)))))
  "Hello" "AAA")

(deftest set-pprint-dispatch.4
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (set-pprint-dispatch-test "Hello")))
        (set-pprint-dispatch 'integer x)
        (values
          (princ-to-string 200)
          (princ-to-string :aaa)))))
  "Hello" "AAA")

(deftest set-pprint-dispatch.5
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((a (set-pprint-dispatch-test "AAA"))
            (b (set-pprint-dispatch-test "BBB"))
            (c (set-pprint-dispatch-test "CCC")))
        (set-pprint-dispatch 'integer a 5)
        (set-pprint-dispatch 'integer b 1)
        (set-pprint-dispatch 'integer c 3)
        (princ-to-string 200))))
  "AAA")

(deftest set-pprint-dispatch.6
  (with-pretty-print
    (let ((hello (copy-pprint-dispatch)))
      (let ((x (set-pprint-dispatch-test "AAA")))
        (set-pprint-dispatch 'integer x 0 hello)
        (princ-to-string 200))))
  "200")

(deftest set-pprint-dispatch.7
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch))
          (*print-pretty* nil))
      (let ((x (set-pprint-dispatch-test "AAA")))
        (set-pprint-dispatch 'integer x)
        (princ-to-string 200))))
  "200")

(deftest set-pprint-dispatch.8
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (set-pprint-dispatch-test "Hello")))
        (set-pprint-dispatch 'integer x)
        (set-pprint-dispatch 'integer nil)
        (eq (pprint-dispatch 10) x))))
  nil)

(deftest-error! set-pprint-dispatch-error.1
  (eval '(set-pprint-dispatch 'integer)))

(deftest-error! set-pprint-dispatch-error.2
  (eval '(set-pprint-dispatch 'integer nil 10 *print-pprint-dispatch* nil)))

(deftest-error! set-pprint-dispatch-error.3
  (eval '(set-pprint-dispatch 'integer nil "Hello")))


;;
;;  Function WRITE
;;
(deftest write.1
  (with-default-print
    (with-output-to-string (x)
      (write :hello :stream x)))
  ":HELLO")

(deftest write.2
  (with-default-print
    (with-open-stream (stream (make-broadcast-stream))
      (write :hello :stream stream)))
  :hello)

(deftest write.3
  (with-default-print
    (with-output-to-string (*standard-output*)
      (write 'hello)))
  "HELLO")

(deftest-error! write-error.1
  (eval '(write)))

(deftest-error write-error.2
  (eval '(write 'hello :hello)))

(deftest-error write-error.3
  (eval '(write 'hello :hello 20)))


;;
;;  Function PRIN1
;;
(deftest prin1.1
  (with-default-print
    (with-output-to-string (x)
      (prin1 :hello x)))
  ":HELLO")

(deftest prin1.2
  (with-default-print
    (with-output-to-string (*standard-output*)
      (prin1 :hello)))
  ":HELLO")

(deftest prin1.3
  (with-open-stream (stream (make-broadcast-stream))
    (prin1 :hello stream))
  :hello)

(deftest-error! prin1-error.1
  (eval '(prin1)))

(deftest-error! prin1-error.2
  (eval '(prin1 10 *standard-output* nil)))

(deftest-error prin1-error.3
  (eval '(prin1 10 20))
  type-error)


;;
;;  Function PRINC
;;
(deftest princ.1
  (with-default-print
    (with-output-to-string (x)
      (princ :hello x)))
  "HELLO")

(deftest princ.2
  (with-default-print
    (with-output-to-string (*standard-output*)
      (princ :hello)))
  "HELLO")

(deftest princ.3
  (with-open-stream (stream (make-broadcast-stream))
    (princ :hello stream))
  :hello)

(deftest-error! princ-error.1
  (eval '(princ)))

(deftest-error! princ-error.2
  (eval '(princ 10 *standard-output* nil)))

(deftest-error princ-error.3
  (eval '(princ 10 20))
  type-error)


;;
;;  Function PRINT
;;
(deftest print.1
  (with-default-print
    (with-output-to-string (x)
      (print :hello x)))
  #(#\newline #\: #\H #\E #\L #\L #\O #\space))

(deftest print.2
  (with-default-print
    (with-output-to-string (*standard-output*)
      (print :hello)))
  #(#\newline #\: #\H #\E #\L #\L #\O #\space))

(deftest print.3
  (with-open-stream (stream (make-broadcast-stream))
    (print :hello stream))
  :hello)

(deftest-error! print-error.1
  (eval '(print)))

(deftest-error! print-error.2
  (eval '(print 10 *standard-output* nil)))

(deftest-error print-error.3
  (eval '(print 10 20))
  type-error)


;;
;;  Function PPRINT
;;
(deftest pprint.1
  (with-default-print
    (with-output-to-string (x)
      (pprint :hello x)))
  #(#\newline #\: #\H #\E #\L #\L #\O))

(deftest pprint.2
  (with-default-print
    (with-output-to-string (*standard-output*)
      (pprint :hello)))
  #(#\newline #\: #\H #\E #\L #\L #\O))

(deftest pprint.3
  (with-open-stream (stream (make-broadcast-stream))
    (pprint :hello stream)))

(deftest pprint.4
  (let ((*print-pretty* nil)
        (*print-right-margin* 5)
        (*print-miser-width* nil))
    (with-output-to-string (x)
      (pprint '(10 20 30) x)))
  #.(mkstr #\Newline "(10" #\Newline " 20" #\Newline " 30)"))

(deftest-error! pprint-error.1
  (eval '(pprint)))

(deftest-error! pprint-error.2
  (eval '(pprint 10 *standard-output* nil)))

(deftest-error pprint-error.3
  (eval '(pprint 10 20))
  type-error)


;;
;;  Function WRITE-TO-STRING
;;
(deftest write-to-string.1
  (with-default-print
    (write-to-string :hello))
  ":HELLO")

(deftest-error! write-to-string-error.1
  (eval '(write-to-string)))

(deftest-error! write-to-string-error.2
  (eval '(write-to-string :hello :hello)))

(deftest-error! write-to-string-error.3
  (eval '(write-to-string :hello :hello 10)))


;;
;;  Function PRIN1-TO-STRING
;;
(deftest prin1-to-string.1
  (with-default-print
    (prin1-to-string :hello))
  ":HELLO")

(deftest prin1-to-string.2
  (prin1-to-string "abc")
  "\"abc\"")

(deftest-error! prin1-to-string-error.1
  (eval '(prin1-to-string)))

(deftest-error! prin1-to-string-error.2
  (eval '(prin1-to-string nil nil)))


;;
;;  Function PRINC-TO-STRING
;;
(deftest princ-to-string.1
  (with-default-print
    (princ-to-string :hello))
  "HELLO")

(deftest princ-to-string.2
  (princ-to-string "abc")
  "abc")

(deftest-error! princ-to-string-error.1
  (eval '(princ-to-string)))

(deftest-error! princ-to-string-error.2
  (eval '(princ-to-string nil nil)))


;;
;;  Variable *PRINT-PPRINT-DISPATCH*
;;
(deftest print-pprint-dispatch.1
  (typep *print-pprint-dispatch* 'lisp-system::print-dispatch)
  t)


;;
;;  Condition Type PRINT-NOT-READABLE
;;
(deftest print-not-readable.1
  (lisp-system:closp
    (find-class 'print-not-readable))
  t)

(deftest print-not-readable.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'print-not-readable)))
  (print-not-readable error serious-condition condition standard-object t))

(deftest print-not-readable.3
  (handler-case
    (let ((*print-readably* t))
      (with-output-to-string (stream)
        (print-unreadable-object (:hello stream)
          (princ "HELLO" stream))))
    (print-not-readable (c) (print-not-readable-object c)))
  :hello)


;;
;;  Function PRINT-NOT-READABLE-OBJECT
;;
(deftest print-not-readable-object.1
  (print-not-readable-object
    (make-condition 'print-not-readable :object 10))
  10)

(deftest-error! print-not-readable-object-error.1
  (eval '(print-not-readable-object)))

(deftest-error! print-not-readable-object-error.2
  (eval '(print-not-readable-object
           (make-condition 'print-not-readable :object 10)
           nil)))

(deftest-error print-not-readable-object-error.3
  (eval '(print-not-readable-object 10))
  type-error)


;;
;;  eastasian
;;
(deftest eastasian.1
  (let ((control (concatenate 'string '(#\u3042 #\u3044 #\u3046) "~10Tdef")))
    (coerce (format nil control) 'list))
  (#\u3042 #\u3044 #\u3046
   #\space #\space #\space #\space
   #\d #\e #\f))

