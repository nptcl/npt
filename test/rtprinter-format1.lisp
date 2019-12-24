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

