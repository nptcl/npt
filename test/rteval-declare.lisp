;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  Function PROCLAIM
;;
(deftest proclaim.1
  (proclaim nil)
  nil)

(deftest proclaim.2
  (progn
    (proclaim '(special proclaim-test2 proclaim-test2a proclaim-test2b))
    (lisp-system::specialp 'proclaim-test2))
  t)

(deftest proclaim.3
  (progn
    (proclaim '(type integer proclaim-test3))
    (eval '(setq proclaim-test3 100))
    (eval 'proclaim-test3))
  100)

(deftest proclaim.4
  (proclaim '(ftype function proclaim-test4))
  nil)

(deftest proclaim.5
  (proclaim '(inline proclaim-test5 (setf proclaim-test5a)))
  nil)

(deftest proclaim.6
  (proclaim '(notinline proclaim-test6 (setf proclaim-test6a)))
  nil)

(deftest proclaim.7
  (proclaim '(optimize (compilation-speed 0)
                       (debug 1)
                       (space 2)
                       (speed 3)
                       safety))
  nil)

(deftest proclaim.8
  (proclaim '(declaration hello-proclaim-test8))
  nil)

(deftest-error proclaim.9
  (progn
    (proclaim '(type integer proclaim-test9))
    (eval '(setq proclaim-test9 :hello)))
  type-error)

(deftest-error! proclaim-error.1
  (eval '(proclaim)))

(deftest-error! proclaim-error.2
  (eval '(proclaim '(special) nil)))

(deftest-error proclaim-error.3
  (eval '(proclaim 100)))


;;
;;  Function DECLAIM
;;
(deftest declaim.1
  (declaim)
  nil)

(deftest declaim.2
  (progn
    (declaim (special declaim-test2 declaim-test2a)
             (special declaim-test2b))
    (lisp-system::specialp 'declaim-test2))
  t)

(deftest declaim.3
  (progn
    (declaim (type integer declaim-test3))
    (eval '(setq declaim-test3 100))
    (eval 'declaim-test3))
  100)

(deftest declaim.4
  (declaim (ftype function declaim-test4))
  nil)

(deftest declaim.5
  (declaim (inline declaim-test5 (setf declaim-test5a)))
  nil)

(deftest declaim.6
  (declaim (notinline declaim-test6 (setf declaim-test6a)))
  nil)

(deftest declaim.7
  (declaim (optimize (compilation-speed 0)
                     (debug 1)
                     (space 2)
                     (speed 3)
                     safety))
  nil)

(deftest declaim.8
  (declaim (declaration hello-declaim-test8))
  nil)

(deftest-error declaim.9
  (progn
    (declaim (type integer declaim-test9))
    (eval '(setq declaim-test9 :hello)))
  type-error)

(deftest-error declaim-error.1
  (eval '(declaim 100)))


;;
;;  Special Operator LOCALLY
;;
(deftest locally.1
  (locally)
  nil)

(deftest locally.2
  (locally
    (values 10 20 30))
  10 20 30)

(deftest locally.3
  (locally
    (declare (special locally-test-1))
    (setq locally-test-1 100)
    (symbol-value 'locally-test-1))
  100)

;;  ANSI Common Lisp
(defun locally-sample-function-1 (y)
  (declare (special y))
  (let ((y t))
    (list y
          (locally (declare (special y))
                   y))))

(deftest locally-test.1
  (locally-sample-function-1 nil)
  (t nil))

(defun locally-sample-function-2 (y)
  (declare (special y))
  (let ((y t))
    (list y
          y
          (locally (declare (special y))
                   (list y y)))))

(deftest locally-test.2
  (locally-sample-function-2 nil)
  (t t (nil nil)))


;;
;;  Special Operator THE
;;
(deftest the.1
  (the integer 10)
  10)

(deftest-error the.2
  (eval '(the integer "Hello"))
  type-error)

(deftest the.3
  (the (values) :hello)
  :hello)

(deftest the.4
  (the (values integer integer) (values 10 20 30))
  10 20 30)

(deftest-error the.5
  (eval '(the (values integer integer) (values 10 #\a 30)))
  type-error)

(deftest-error the.6
  (eval '(the (values integer integer) (values 10)))
  type-error)

(deftest-error the.7
  (eval '(the (values integer integer) 10))
  type-error)

(deftest the.8
  (the (values integer (or null integer)) (values 10))
  10)

(deftest the.9
  (the (values integer (or null integer)) 10)
  10)

(deftest-error the-push.1
  (eval '(values
           (eval '(the (values integer integer) (values 10 #\a 30)))
           40))
  type-error)

(deftest the-push.2
  (values
    (eval '(the (values integer integer) (values 10 20 30)))
    40)
  10 40)

(deftest-error the-push.3
  (eval '(values
           (eval '(the (values integer integer) (values 10)))
           40))
  type-error)

(deftest-error the-push.4
  (eval '(values
           (eval '(the (values integer integer) 10))
           40))
  type-error)

(deftest the-push.5
  (values
    (the (values integer (or null integer)) (values 10))
    40)
  10 40)

(deftest the-push.6
  (values
    (the (values integer (or null integer)) 10)
    40)
  10 40)

(deftest-error the-error.1
  (eval '(the integer)))

(deftest-error the-error.2
  (eval '(the integer 10 20)))

(deftest-error the-error.3
  (eval '(the 10 20)))

;;  ANSI Common Lisp
(deftest the-test.1
  (null (symbol-package
          (the symbol (car (list (gensym))))))
  t)

(deftest the-test.2
  (the fixnum (+ 5 7))
  12)

(deftest the-test.3
  (the (values) (truncate 3.2 2))
  1 1.2)

(deftest the-test.4
  (the integer (truncate 3.2 2))
  1 1.2)

(deftest the-test.5
  (the (values integer) (truncate 3.2 2))
  1 1.2)

(deftest the-test.6
  (the (values integer float) (truncate 3.2 2))
  1 1.2)

(deftest the-test.7
  (the (values integer float symbol) (truncate 3.2 2))
  1 1.2)

(deftest the-test.8
  (the (values integer float symbol t null list)
       (truncate 3.2 2))
  1 1.2)

(deftest the-test.9
  (let ((i 100))
    (declare (fixnum i))
    (the fixnum (1+ i)))
  101)

(deftest the-test.10
  (let* ((x (list 'a 'b 'c))
         (y 5))
    (setf (the fixnum (car x)) y)
    x)
  (5 b c))


;;  Symbol DECLARE
;;  Declaration IGNORE
;;  Declaration IGNORABLE
;;  Declaration DYNAMIC-EXTENT
;;  Declaration TYPE
;;  Declaration INLINE
;;  Declaration NOTINLINE
;;  Declaration FTYPE
;;  Declaration DECLARATION
;;  Declaration OPTIMIZE
;;  Declaration SPECIAL

