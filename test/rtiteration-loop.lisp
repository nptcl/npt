;;
;;  ANSI COMMON LISP: 6. Iteration
;;

;;
;;  simple
;;
(deftest loop-simple.1
  (let ((i 0) list)
    (loop (when (<= 10 i)
            (return (nreverse list)))
          (push i list)
          (incf i 1)))
  (0 1 2 3 4 5 6 7 8 9))


;;
;;  do
;;
(deftest loop-extend.1
  (loop do (return nil))
  nil)

(deftest loop-extend.2
  (loop do (return 100))
  100)

(deftest loop-extend.3
  (let (list)
    (values
      (loop do
            (push 10 list)
            (push 20 list)
            (return 30))
      (nreverse list)))
  30 (10 20))

(deftest loop-extend.4
  (let (list)
    (values
      (loop do
            (push 10 list)
            (push 20 list)
            (return 30)
            (push 40 list)
            (push 50 list))
      (nreverse list)))
  30 (10 20))


;;
;;  named
;;
(deftest loop-named.1
  (loop named hello
        do (return-from hello 100))
  100)

