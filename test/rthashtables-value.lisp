;;
;;  ANSI COMMON LISP: 18. Hash Tables
;;

;;
;;  array string
;;
(deftest hash-table-array.1
  (let ((table (make-hash-table :test 'equal))
        (x (make-array 5 :element-type 'character))
        (y (make-array 5 :element-type 'character)))
    (replace x "Hello")
    (replace y "Hello")
    (setf (gethash x table) 10)
    (setf (gethash y table) 20)
    (values x y (equal x y)
            (gethash x table)
            (gethash y table)))
  "Hello" "Hello" T 20 20)

(deftest hash-table-array.2
  (let ((table (make-hash-table :test 'equalp))
        (x (make-array 5 :element-type 'character))
        (y (make-array 5 :element-type 'character)))
    (replace x "hello")
    (replace y "HELLO")
    (setf (gethash x table) 10)
    (setf (gethash y table) 20)
    (values x y (equalp x y)
            (gethash x table)
            (gethash y table)))
  "hello" "HELLO" T 20 20)

(deftest hash-table-array.3
  (let ((x (make-array 5 :element-type 'character))
        (y (make-array 5 :element-type 'character)))
    (replace x "Hello")
    (replace y "Hello")
    (eql (sxhash x) (sxhash y)))
  t)

