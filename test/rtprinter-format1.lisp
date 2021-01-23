;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  Macro FORMATTER
;;
(deftest formatter.1
  (functionp (formatter "~S"))
  t)

(deftest formatter.2
  (with-output-to-string (stream)
    (funcall (formatter "<<~A>>") stream 10 20 30))
  "<<10>>")

(deftest formatter.3
  (with-open-stream (stream (make-string-output-stream))
    (funcall (formatter "<<~A>>") stream 10 20 30))
  (20 30))

(deftest formatter.4
  (format nil (formatter "<<~A>>") 10 20 30)
  "<<10>>")

(deftest formatter.5
  (functionp
    (formatter "Hello"))
  t)

(deftest formatter.6
  (let (result)
    (values
      (with-output-to-string (stream)
        (setq result (funcall (formatter "Hello") stream)))
      result))
  "Hello" nil)

(deftest formatter.7
  (let (result)
    (values
      (with-output-to-string (stream)
        (setq result (funcall (formatter "Hello") stream 10 20 30)))
      result))
  "Hello" (10 20 30))

(deftest formatter.8
  (let (result)
    (values
      (with-output-to-string (stream)
        (setq result (funcall (formatter "Hello: ~A") stream 10 20 30)))
      result))
  "Hello: 10" (20 30))

(deftest-error formatter-error.1
  (eval '(formatter)))

(deftest-error formatter-error.2
  (eval '(formatter (values "Hello"))))

(deftest-error formatter-error.3
  (eval '(formatter "Hello" nil)))

;;  ANSI Common Lisp
(deftest formatter-test.1
  (with-output-to-string (*standard-output*)
    (funcall (formatter "~&~A~A") *standard-output* 'a 'b 'c))
  "AB")

(deftest formatter-test.2
  (with-open-stream (*standard-output* (make-string-output-stream))
    (funcall (formatter "~&~A~A") *standard-output* 'a 'b 'c))
  (c))

(deftest formatter-test.3
  (with-output-to-string (*standard-output*)
    (format t (formatter "~&~A~A") 'a 'b 'c))
  "AB")

(deftest formatter-test.4
  (with-open-stream (*standard-output* (make-string-output-stream))
    (format t (formatter "~&~A~A") 'a 'b 'c))
  nil)


;;
;;  Function FORMAT
;;
(deftest format.1
  (with-output-to-string (*standard-output*)
    (format t "Hello: ~A" 10))
  "Hello: 10")

(deftest format.2
  (format nil "Hello: ~A" 10)
  "Hello: 10")

(deftest format.3
  (let ((value (make-array 10 :element-type 'character :fill-pointer 0 :adjustable t)))
    (format value "Hello: ~A" 10)
    value)
  "Hello: 10")

(deftest format.4
  (with-output-to-string (stream)
    (format stream "Hello: ~A" 10))
  "Hello: 10")

(deftest format.5
  (format nil (formatter "Hello: ~A") 10 20 30)
  "Hello: 10")

(deftest-error! format-error.1
  (eval '(format t)))

(deftest-error format-error.2
  (eval '(format 10 "Hello"))
  type-error)

