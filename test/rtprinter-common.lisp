;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  print function
;;
(deftest write.1
  (with-default-print
    (with-output-to-string (x)
      (write :hello :stream x)))
  ":HELLO")

(deftest write.2
  (with-default-print
    (write-to-string :hello))
  ":HELLO")

(deftest write.3
  (with-open-stream (stream (make-broadcast-stream))
    (write :hello :stream stream))
  :hello)

(deftest prin1.1
  (with-default-print
    (with-output-to-string (x)
      (prin1 :hello x)))
  ":HELLO")

(deftest prin1.2
  (with-default-print
    (with-output-to-string (x)
      (let ((*standard-output* x))
        (prin1 :hello))))
  ":HELLO")

(deftest prin1.3
  (with-open-stream (stream (make-broadcast-stream))
    (prin1 :hello stream))
  :hello)

(deftest prin1.4
  (with-default-print
    (prin1-to-string :hello))
  ":HELLO")

(deftest princ.1
  (with-default-print
    (with-output-to-string (x)
      (princ :hello x)))
  "HELLO")

(deftest princ.2
  (with-default-print
    (with-output-to-string (x)
      (let ((*standard-output* x))
        (princ :hello))))
  "HELLO")

(deftest princ.3
  (with-open-stream (stream (make-broadcast-stream))
    (princ :hello stream))
  :hello)

(deftest princ.4
  (with-default-print
    (princ-to-string :hello))
  "HELLO")

(deftest print.1
  (with-default-print
    (with-output-to-string (x)
      (print :hello x)))
  #(#\newline #\: #\H #\E #\L #\L #\O #\space))

(deftest print.2
  (with-default-print
    (with-output-to-string (x)
      (let ((*standard-output* x))
        (print :hello))))
  #(#\newline #\: #\H #\E #\L #\L #\O #\space))

(deftest print.3
  (with-open-stream (stream (make-broadcast-stream))
    (print :hello stream))
  :hello)


;;
;;  dispatch
;;
(deftest print-pprint-dispatch.1
  (typep *print-pprint-dispatch* 'lisp-system::print-dispatch)
  t)

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

(defun princ-lambda (x)
  (lambda (s o)
    (declare (ignore o))
    (princ x s)))

(deftest set-pprint-dispatch.1
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (princ-lambda "Hello")))
        (set-pprint-dispatch 'integer x)
        (eq (pprint-dispatch 10) x))))
  t)

(deftest set-pprint-dispatch.2
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (princ-lambda "Hello")))
        (set-pprint-dispatch 'integer x)
        (eq (pprint-dispatch :hello) x))))
  nil)

(deftest set-pprint-dispatch.3
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (princ-lambda "Hello")))
        (set-pprint-dispatch 'integer x)
        (values
          (princ-to-string 200)
          (princ-to-string :aaa)))))
  "Hello" "AAA")

(deftest set-pprint-dispatch.4
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((x (princ-lambda "Hello")))
        (set-pprint-dispatch 'integer x)
        (values
          (princ-to-string 200)
          (princ-to-string :aaa)))))
  "Hello" "AAA")

(deftest set-pprint-dispatch.5
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
      (let ((a (princ-lambda "AAA"))
            (b (princ-lambda "BBB"))
            (c (princ-lambda "CCC")))
        (set-pprint-dispatch 'integer a 5)
        (set-pprint-dispatch 'integer b 1)
        (set-pprint-dispatch 'integer c 3)
        (princ-to-string 200))))
  "AAA")

(deftest set-pprint-dispatch.6
  (with-pretty-print
    (let ((hello (copy-pprint-dispatch)))
      (let ((x (princ-lambda "AAA")))
        (set-pprint-dispatch 'integer x 0 hello)
        (princ-to-string 200))))
  "200")

(deftest set-pprint-dispatch.7
  (with-pretty-print
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch))
          (*print-pretty* nil))
      (let ((x (princ-lambda "AAA")))
        (set-pprint-dispatch 'integer x)
        (princ-to-string 200))))
  "200")

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


;;
;;  eastasian width
;;
(deftest eastasian.1
  (let ((control (concatenate 'string '(#\u3042 #\u3044 #\u3046) "~10Tdef")))
    (coerce (format nil control) 'list))
  (#\u3042 #\u3044 #\u3046
   #\space #\space #\space #\space
   #\d #\e #\f))

