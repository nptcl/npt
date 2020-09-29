;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  System Class SEQUENCE
;;
(deftest sequence-type.1
  (lisp-system:closp
    (find-class 'sequence))
  t)

(deftest sequence-type.2
  (flet ((findc (x) (lisp-system:closp
                      (find (find-class x)
                            (lisp-clos:class-precedence-list
                              (find-class 'sequence))))))
    (every #'findc '(sequence t)))
  t)

(deftest sequence-type.3
  (typep nil 'sequence)
  t)

(deftest sequence-type.4
  (typep #(10 20 30) 'sequence)
  t)

(deftest sequence-type.5
  (typep "Hello" 'sequence)
  t)

(deftest sequence-type.6
  (typep #2a((a b) (c d) (e f)) 'sequence)
  nil)

