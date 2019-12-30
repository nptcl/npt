;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  formatter
;;
(deftest formatter.1
  (functionp
    (formatter "Hello"))
  t)

(deftest formatter.2
  (let (result)
    (values
      (with-output-to-string (stream)
        (setq result (funcall (formatter "Hello") stream)))
      result))
  "Hello" nil)

(deftest formatter.3
  (let (result)
    (values
      (with-output-to-string (stream)
        (setq result (funcall (formatter "Hello") stream 10 20 30)))
      result))
  "Hello" (10 20 30))

(deftest formatter.4
  (let (result)
    (values
      (with-output-to-string (stream)
        (setq result (funcall (formatter "Hello: ~A") stream 10 20 30)))
      result))
  "Hello: 10" (20 30))


;;
;;  format
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

