;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Macro CASE
;;
(deftest case.1
  (case 10)
  nil)

(deftest case.2
  (case 10
    (10 111) (20 222) (30 333))
  111)

(deftest case.3
  (case 20
    (10 111) (20 1 2 3 222) (30 333))
  222)

(deftest case.4
  (case :hello
    (10 111) (20 222) (30 333))
  nil)

(deftest case.5
  (case 30
    (10 111) ((1 2 3) 222) (30 333))
  333)

(deftest case.6
  (case 3
    (10 111) ((1 2 3) 222) (30 333))
  222)

(deftest case.7
  (case 9
    (10 111) ((1 2 3) 222) (30 333) (t 999))
  999)

(deftest case.8
  (case 9
    (10 111) ((1 2 3) 222) (30 333) (otherwise 999))
  999)

(deftest case.9
  (case 3
    (10 111) ((1 2 3) 222) (30 333) (otherwise 999))
  222)

(deftest case.10
  (case 3
    (1 'aaa)
    (otherwise 'bbb))
  bbb)

(deftest case.11
  (case 3
    (1 'aaa)
    ((otherwise) 'bbb))
  nil)

(deftest case.12
  (case 'otherwise
    (1 'aaa)
    ((otherwise) 'bbb)
    (3 'ccc))
  bbb)

(deftest case.13
  (case 3
    (1 'aaa)
    (t 'bbb))
  bbb)

(deftest case.14
  (case 3
    (1 'aaa)
    ((t) 'bbb))
  nil)

(deftest case.15
  (case t
    (1 'aaa)
    ((t) 'bbb)
    (3 'ccc))
  bbb)

(deftest case.16
  (case 10 (10))
  nil)

(deftest case.17
  (case nil
    (nil 10)
    ((nil) 20)
    (t 30))
  20)

(deftest-error case-error.1
  (eval '(case)))

(deftest-error case-error.2
  (eval '(case 10 ())))


;;
;;  Macro ECASE
;;
(deftest ecase.1
  (ecase 10
    (10 :aaa))
  :aaa)

(deftest ecase.2
  (ecase 2
    (10 :aaa)
    ((1 2 3) :bbb)
    (30 :ccc))
  :bbb)

(deftest ecase.3
  (ecase 30
    (10 :aaa)
    ((1 2 3) :bbb)
    (30 :ccc))
  :ccc)

(deftest-error ecase.4
  (ecase 30
    (10 :aaa))
  type-error)

(deftest-error ecase.5
  (ecase 30
    (10 :aaa)
    (otherwise :bbb))
  type-error)

(deftest-error ecase.6
  (ecase 30
    (10 :aaa)
    (t :bbb))
  type-error)

(deftest ecase.7
  (ecase 10 (10))
  nil)

(deftest ecase.8
  (ecase nil
    (nil 10)
    ((nil) 20))
  20)

(deftest-error ecase-error.1
  (eval '(ecase)))

(deftest-error ecase-error.2
  (eval '(ecase 10 ())))


;;
;;  Macro CCASE
;;
(deftest ccase.1
  (let ((x 20))
    (ccase x (20)))
  nil)

(deftest ccase.2
  (let ((x 10))
    (ccase x
      (10 :aaa)))
  :aaa)

(deftest ccase.3
  (let ((x 2))
    (ccase x
      (10 :aaa)
      ((1 2 3) :bbb)
      (30 :ccc)))
  :bbb)

(deftest ccase.4
  (let ((x (cons 30 40)))
    (ccase (car x)
      (10 :aaa)
      ((1 2 3) :bbb)
      (30 :ccc)))
  :ccc)

(deftest-error ccase.5
  (let ((x 30))
    (ccase x
      (10 :aaa)
      (20 :bbb)))
  type-error)

(deftest-error ccase.6
  (let ((x 30))
    (ccase x
      (10 :aaa)
      (otherwise :bbb)))
  type-error)

(deftest-error ccase.7
  (let ((x 30))
    (ccase x
      (10 :aaa)
      (t :bbb)))
  type-error)

(deftest ccase.8
  (let ((x 30))
    (handler-bind ((type-error
                     (lambda (x)
                       (store-value 10 x))))
      (ccase x
        (10 :aaa)))
    x)
  10)

(deftest ccase.9
  (let ((x (cons 40 50)))
    (handler-bind ((type-error
                     (lambda (x)
                       (store-value 10 x))))
      (ccase (cdr x)
        (10 :aaa)))
    x)
  (40 . 10))

(deftest ccase.10
  (let (x)
    (ccase x
      (nil 10)
      ((nil) 20)))
  20)

(deftest ccase.11
  (let ((x 'hello))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ccase x (10 :aaa) (20 :bbb) (999 :ccc)))
      x))
  :ccc 999)

(deftest ccase.12
  (let ((x '(a b c d)))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ccase (car x) (10 :aaa) (20 :bbb) (999 :ccc)))
      x))
  :ccc (999 b c d))

(deftest-error ccase-error.1
  (eval '(ccase)))

(deftest-error ccase-error.2
  (eval '(ccase 10)))

(deftest-error ccase-error.3
  (eval '(ccase 10 ())))

(deftest-error ccase-error.4
  (eval '(let (x) (ccase x ()))))

;;  ANSI Common Lisp
(deftest case-test.1
  (let (list)
    (dolist (k '(1 2 3 :four #\v () t 'other))
      (push
        (format nil "~S"
                (case k ((1 2) 'clause1)
                  (3 'clause2)
                  (nil 'no-keys-so-never-seen)
                  ((nil) 'nilslot)
                  ((:four #\v) 'clause4)
                  ((t) 'tslot)
                  (otherwise 'others)))
        list))
    (nreverse list))
  ("CLAUSE1" "CLAUSE1" "CLAUSE2" "CLAUSE4" "CLAUSE4" "NILSLOT" "TSLOT" "OTHERS"))

(defun case-test-decode (x)
  (ccase x
    ((i uno) 1)
    ((ii dos) 2)
    ((iii tres) 3)
    ((iv cuatro) 4)))

(defun case-test-add-em (x)
  (apply #'+ (mapcar #'case-test-decode x)))

(deftest case-test.2
  (case-test-add-em '(uno iii))
  4)

(deftest case-test.3
  (handler-bind ((type-error
                   (lambda (c)
                     (store-value 'IV c))))
    (case-test-add-em '(uno iiii)))
  5)


;;
;;  Macro TYPECASE
;;
(deftest typecase.1
  (typecase 10)
  nil)

(deftest typecase.2
  (typecase 10
    (integer :aaa))
  :aaa)

(deftest typecase.3
  (typecase 10
    (integer :aaa)
    (string :bbb))
  :aaa)

(deftest typecase.4
  (typecase "Hello"
    (integer :aaa)
    (string :bbb))
  :bbb)

(deftest typecase.5
  (typecase #\a
    (integer :aaa)
    (string :bbb))
  nil)

(deftest typecase.6
  (typecase #\a
    (integer :aaa)
    (string :bbb)
    (otherwise :ccc))
  :ccc)

(deftest typecase.7
  (typecase #\a
    (integer :aaa)
    (string :bbb)
    (t :ccc))
  :ccc)

(deftest typecase.8
  (typecase "Hello"
    (integer :aaa)
    (string :bbb)
    (otherwise :ccc))
  :bbb)

(deftest typecase.9
  (typecase 100
    (integer))
  nil)

(deftest-error typecase-error.1
  (eval '(typecase)))

(deftest-error typecase-error.2
  (eval '(typecase 10 20)))


;;
;;  Macro ETYPECASE
;;
(deftest etypecase.1
  (etypecase 10
    (integer :aaa))
  :aaa)

(deftest etypecase.2
  (etypecase 10
    (integer :aaa)
    (string :bbb))
  :aaa)

(deftest etypecase.3
  (etypecase "Hello"
    (integer :aaa)
    (string :bbb))
  :bbb)

(deftest-error etypecase.4
  (etypecase :hello
    (integer :aaa)
    (string :bbb))
  type-error)

(deftest-error etypecase.5
  (etypecase :hello
    (integer :aaa)
    (string :bbb)
    (otherwise :ccc)))

(deftest etypecase.6
  (etypecase :hello
    (integer :aaa)
    (string :bbb)
    (t :ccc))
  :ccc)

(deftest etypecase.7
  (etypecase 10
    (integer))
  nil)

(deftest-error etypecase-error.1
  (eval '(etypecase)))

(deftest-error etypecase-error.2
  (eval '(etypecase 10 20)))


;;
;;  Macro CTYPECASE
;;
(deftest ctypecase.1
  (let ((x 10))
    (ctypecase x
      (integer :aaa)))
  :aaa)

(deftest ctypecase.2
  (let ((x (cons 10 "Hello")))
    (ctypecase (car x)
      (integer :aaa)
      (string :bbb)))
  :aaa)

(deftest ctypecase.3
  (let ((x (cons 10 "Hello")))
    (ctypecase (cdr x)
      (integer :aaa)
      (string :bbb)))
  :bbb)

(deftest ctypecase.4
  (let ((x "Hello"))
    (ctypecase x
      (integer :aaa)
      (string :bbb)))
  :bbb)

(deftest-error ctypecase.5
  (let ((x :hello))
    (ctypecase x
      (integer :aaa)
      (string :bbb)))
  type-error)

(deftest-error ctypecase.6
  (let ((x :hello))
    (ctypecase x
      (integer :aaa)
      (string :bbb)
      (otherwise :ccc))))

(deftest ctypecase.7
  (let ((x :hello))
    (ctypecase x
      (integer :aaa)
      (string :bbb)
      (t :ccc)))
  :ccc)

(deftest ctypecase.8
  (let ((x 10))
    (ctypecase x
      (integer)))
  nil)

(deftest ctypecase.9
  (let ((x 'hello))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ctypecase x (string :aaa) (integer :bbb) (float :ccc)))
      x))
  :bbb 999)

(deftest ctypecase.10
  (let ((x '(a b c d)))
    (values
      (handler-bind
        ((type-error
           (lambda (c)
             (store-value 999 c))))
        (ctypecase (car x) (string :aaa) (integer :bbb) (float :ccc)))
      x))
  :bbb (999 b c d))

(deftest-error ctypecase-error.1
  (eval '(ctypecase)))

(deftest-error ctypecase-error.2
  (eval '(ctypecase 10)))

(deftest-error ctypecase-error.3
  (eval '(ctypecase 10 ())))

(deftest-error ctypecase-error.4
  (eval '(let (x) (ctypecase x ()))))

;;  ANSI Common Lisp
(defun typecase-test-what-is-it (x)
  (format nil "~S is ~A."
          x (typecase x
              (float "a float")
              (null "a symbol, boolean false, or the empty list")
              (list "a list")
              (t (format nil "a(n) ~(~A~)" (type-of x))))))

(deftest typecase-test.1
  (map 'list #'typecase-test-what-is-it '(nil (a b) 7.0 7 box))
  ("NIL is a symbol, boolean false, or the empty list."
   "(A B) is a list."
   "7.0 is a float."
   "7 is a(n) (integer 7 7)."
   "BOX is a(n) symbol."))

(deftest typecase-test.2
  (let ((x 1/3))
    (handler-bind ((type-error
                     (lambda (c)
                       (store-value 12 c))))
      (ctypecase x
        (integer (* x 4))
        (symbol  (symbol-value x))))
    x)
  12)

