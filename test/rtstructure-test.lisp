;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  Function COPY-STRUCTURE
;;
(deftest copy-structure.1
  (progn
    (defstruct copy-structure-1)
    (let* ((x (make-copy-structure-1))
           (y (copy-structure x)))
      (values
        (eq x y)
        (equal x y)
        (equalp x y)
        (copy-structure-1-p y))))
  nil nil t t)

(deftest copy-structure.2
  (progn
    (defstruct copy-structure-2 aaa bbb)
    (let* ((x (make-copy-structure-2 :aaa 10 :bbb 20))
           (y (copy-structure x)))
      (values
        (eq x y)
        (equal x y)
        (equalp x y)
        (copy-structure-2-aaa y)
        (copy-structure-2-bbb y)
        (copy-structure-2-p y))))
  nil nil t 10 20 t)

(deftest-error copy-structure-error.1
  (progn
    (defclass copy-structure-3 () ())
    (copy-structure
      (make-instance 'copy-structure-3))))

(deftest-error! copy-structure-error.2
  (eval '(copy-structure)))

(deftest-error! copy-structure-error.3
  (eval '(copy-structure 10)))

(deftest-error! copy-structure-error.4
  (eval '(copy-structure (make-copy-structure-2) 10)))

