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


;;
;;  Symbol DECLARE
;;
(deftest declare.1
  (locally
    (declare))
  nil)

(deftest declare.2
  (progn
    (defun declare-test-1 ()
      (declare)
      "hello"
      10)
    (documentation 'declare-test-1 'function))
  "hello")

(deftest declare.3
  (progn
    (defun declare-test-2 ()
      "abc"
      (declare)
      10)
    (documentation 'declare-test-2 'function))
  "abc")

(deftest declare.4
  (let (x)
    (declare (ignore x) (special x))
    (declare (special aaa))
    10)
  10)

(deftest-error declare.5
  (eval '(locally (declare hello))))

(deftest declare-lambda.1
  (functionp
    (lambda ()
      (declare (special aaa))
      :abc))
  t)

(deftest declare-defgeneric.1
  (functionp
    (defgeneric declare-defgeneric-1 () (declare)))
  t)

(deftest declare-defgeneric.2
  (functionp
    (defgeneric declare-defgeneric-2 (x y z) (declare (optimize speed))))
  t)

(deftest declare-define-compiler-macro.1
  (define-compiler-macro declare-define-compiler-macro-1 ()
    (declare (special aaa))
    :hello)
  declare-define-compiler-macro-1)

(deftest declare-define-method-combination.1
  (define-method-combination
    declare-define-method-combination-1 () ()
    (declare (special aaa))
    :hello)
  declare-define-method-combination-1)

(deftest declare-define-setf-expander.1
  (define-setf-expander declare-define-setf-expander-1 ()
    (declare (special aaa))
    :hello)
  declare-define-setf-expander-1)

(deftest declare-defmacro.1
  (defmacro declare-defmacro-1 (x)
    (declare (ignore x) (special aaa))
    :hello)
  declare-defmacro-1)

(deftest declare-defmethod.1
  (typep
    (defmethod declare-defgeneric-1 ()
      (declare (special aaa))
      :hello)
    'standard-method)
  t)

(deftest declare-defsetf.1
  (defsetf declare-defsetf-1 (x) (g)
    (declare (special x) (ignorable g))
    :hello)
  declare-defsetf-1)

(deftest declare-deftype.1
  (deftype declare-deftype-1 (x)
    (declare (special x))
    :hello)
  declare-deftype-1)

(deftest declare-defun.1
  (defun declare-defun-1 (x)
    (declare (special x))
    :hello)
  declare-defun-1)

(deftest declare-destructuring-bind.1
  (destructuring-bind (x) (list 10)
    (declare (special x))
    :hello)
  :hello)

(deftest declare-do.1
  (do (x)
    (t)
    (declare (special x)))
  nil)

(deftest declare-do*.2
  (do* (x)
    (t)
    (declare (special x)))
  nil)

(deftest declare-dolist.1
  (dolist (x '(1 2 3))
    (declare (special x))
    :hello)
  nil)

(deftest declare-dotimes.1
  (dotimes (i 3)
    (declare (special i))
    :hello)
  nil)

(deftest declare-do-symbols.1
  (functionp
    (lambda ()
      (do-symbols (x *package*)
        (declare (special x))
        :hello)))
  t)
(deftest declare-do-external-symbols.1
  (functionp
    (lambda ()
      (do-external-symbols (x *package*)
        (declare (special x))
        :hello)))
  t)

(deftest declare-do-all-symbols.1
  (functionp
    (lambda ()
      (do-all-symbols (x)
        (declare (special x))
        :hello)))
  t)

(deftest declare-flet.1
  (flet ((x () :hello))
    (declare (ignore #'x))
    :abc)
  :abc)

(deftest declare-flet.2
  (flet ((x (y) (declare (special y)) y))
    (x 10))
  10)

(deftest declare-labels.1
  (labels ((x () :hello))
    (declare (ignore #'x))
    :abc)
  :abc)

(deftest declare-labels.2
  (labels ((x (y) (declare (special y)) y))
    (x 10))
  10)

(deftest declare-macrolet.1
  (macrolet ((x () :hello))
    (declare (ignore #'x))
    :abc)
  :abc)

(deftest declare-macrolet.2
  (macrolet ((x (y) (declare (special y)) y))
    (x 10))
  10)

(deftest declare-let.1
  (let (x y z)
    (declare (special x y z aaa))
    :hello)
  :hello)

(deftest declare-let*.1
  (let* (x y z)
    (declare (special x y z aaa))
    :hello)
  :hello)

(deftest declare-locally.1
  (locally
    (declare (special aaa))
    :hello)
  :hello)

(deftest declare-handler-case.1
  (handler-case
    :hello
    (error (c) (declare (special c)) :error))
  :hello)

(deftest declare-restart-case.1
  (restart-case
    :hello
    (abc () (declare (special aaa)) :error))
  :hello)

(deftest declare-multiple-value-bind.1
  (multiple-value-bind (a) 10
    (declare (special a))
    :hello)
  :hello)

(deftest declare-pprint-logical-block.1
  (pprint-logical-block (nil nil)
    (declare (special aaa))
    11)
  nil)

(deftest declare-prog.1
  (prog (x)
    (declare (special x))
    22)
  nil)

(deftest declare-prog*.1
  (prog* (x)
    (declare (special x))
    22)
  nil)

(deftest declare-symbol-macrolet.1
  (symbol-macrolet ((a :hello))
    (declare (special aaa))
    a)
  :hello)

(defclass declare-with-accessors-1 ()
  ((aaa :accessor declare-with-accessors-1-aaa)
   (bbb :accessor declare-with-accessors-1-aaa)))

(deftest declare-with-accessors.1
  (with-accessors ((aaa declare-with-accessors-1-aaa)
                   (bbb declare-with-accessors-1-bbb))
    (make-instance 'declare-with-accessors-1)
    (declare (special ccc))
    33)
  33)

(deftest declare-with-hash-table-iterator.1
  (with-hash-table-iterator
    (var (make-hash-table))
    (declare (special aaa))
    :aaa)
  :aaa)

(deftest declare-with-package-iterator.1
  (with-package-iterator
    (var *package* :internal)
    (declare (special aaa))
    :bbb)
  :bbb)

(deftest declare-with-input-from-string.1
  (with-input-from-string (s "Hello")
    (declare (special aaa))
    :hello)
  :hello)

(deftest declare-with-output-to-string.1
  (with-output-to-string (s)
    (declare (special aaa))
    44)
  "")

(deftest declare-with-open-file.1
  (with-open-stream (file (lisp-system:make-memory-io-stream))
    (with-open-file (stream file)
      (declare (special aaa))
      55))
  55)

(deftest declare-with-open-stream.1
  (with-open-stream (stream (make-broadcast-stream))
    (declare (special aaa))
    66)
  66)

(defclass declare-with-slots-1 () (aaa bbb))

(deftest declare-with-slots.1
  (with-slots (aaa bbb) (make-instance 'declare-with-slots-1)
    (declare (special ccc))
    77)
  77)


;;
;;  Declaration IGNORE
;;
(deftest declaration-ignore.1
  (let ()
    (declare (ignore))
    10)
  10)

(deftest declaration-ignore.2
  (let (x y z)
    (declare (ignore x y z))
    10)
  10)

(deftest declaration-ignore.3
  (flet ((y () :hello))
    (declare (ignore #'y))
    20)
  20)

(deftest declaration-ignore.4
  (handler-case
    (eval '(let (x) 10))
    (style-warning () :warning))
  :warning)

(deftest declaration-ignore.5
  (handler-case
    (eval '(let (x) (declare (ignore x)) x))
    (style-warning () :warning))
  :warning)

(deftest-error declaration-ignore-error.1
  (eval '(let (x)
           (declare (ignore 10))
           10)))


;;
;;  Declaration IGNORABLE
;;
(deftest declaration-ignorable.1
  (let ()
    (declare (ignorable))
    10)
  10)

(deftest declaration-ignorable.2
  (let (x y z)
    (declare (ignorable x y z))
    10)
  10)

(deftest declaration-ignorable.3
  (let (x y z)
    (declare (ignorable x y z))
    z)
  nil)

(deftest declaration-ignorable.4
  (flet ((y () :hello))
    (declare (ignorable #'y))
    20)
  20)

(deftest-error declaration-ignorable-error.1
  (eval '(let (x)
           (declare (ignorable 10))
           10)))


;;
;;  Declaration DYNAMIC-EXTENT
;;
(deftest declaration-dynamic-extent.1
  (let ()
    (declare (dynamic-extent)))
  nil)

(deftest declaration-dynamic-extent.2
  (let (x y z)
    (declare (dynamic-extent x y))
    (values (and x y z)))
  nil)

(deftest declaration-dynamic-extent.3
  (flet ((aaa () :hello))
    (declare (dynamic-extent (function aaa)))
    (aaa))
  :hello)

(deftest-error declaration-dynamic-extent-error.1
  (eval '(locally (declare (dynamic-extent 10)))))


;;
;;  Declaration TYPE
;;
(deftest declaration-type.1
  (let ((x 10))
    (declare (type integer x))
    x)
  10)

(deftest declaration-type.2
  (let ((x 10))
    (declare (type integer))
    x)
  10)

(deftest-error declaration-type.3
  (eval '(let (x)
           (declare (type integer x))
           x))
  type-error)

(deftest-error declaration-type.4
  (eval '(let ((x 10))
           (declare (type integer x))
           (setq x :hello)))
  type-error)

(deftest declaration-type.5
  (let ((x 10) (y 20))
    (declare (type integer x y))
    (+ x y))
  30)

(deftest declaration-type.6
  (let ((x 10) (y 20))
    (declare (type integer x y))
    (+ x y))
  30)

(deftest declaration-type.7
  (let ((x 10) (y 20))
    (declare (integer x y))
    (+ x y))
  30)

(deftest declaration-type.8
  (let ((x 10) (y 20))
    (declare ((integer 0 100) x y))
    (+ x y))
  30)

;;  ANSI Common Lisp
(defun declaration-type-test-f1 (x y)
  (declare (type fixnum x y))
  (let ((z (+ x y)))
    (declare (type fixnum z))
    z))

(deftest declaration-type-test.1
  (declaration-type-test-f1 1 2)
  3)

(defun declaration-type-test-f2 (x y)
  (declare (fixnum x y))
  (the fixnum (+ x y)))

(deftest declaration-type-test.2
  (declaration-type-test-f2 1 2)
  3)

(defvar *declaration-type-test-one-array*)
(defvar *declaration-type-test-another-array*)

(defun declaration-type-test-frob1 (an-array)
  (declare (type (array (signed-byte 5) 1) an-array))
  (setf (aref an-array 1) 31)
  (setf (aref an-array 2) 127)
  (setf (aref an-array 3) (* 2 (aref an-array 3)))
  (let ((foo 0))
    (declare (type (signed-byte 5) foo))
    (setf foo (aref an-array 0))))

(deftest declaration-type-test.3
  (let ((*declaration-type-test-one-array*
          (make-array 10 :element-type '(signed-byte 5)))
        (*declaration-type-test-another-array*
          (make-array 10 :element-type '(signed-byte 8))))
    (declaration-type-test-frob1 *declaration-type-test-one-array*)
    (declaration-type-test-frob1 *declaration-type-test-another-array*)
    (values)))

(defun declaration-type-test-frob2 (an-array)
  (setf (the (signed-byte 5) (aref an-array 1)) 31)
  (setf (the (signed-byte 5) (aref an-array 2)) 127)
  (setf (the (signed-byte 5) (aref an-array 3))
        (* 2 (the (signed-byte 5) (aref an-array 3))))
  (let ((foo 0))
    (declare (type (signed-byte 5) foo))
    (setf foo (the (signed-byte 5) (aref an-array 0)))))

;;  error?
(deftest-error declaration-type-test.4
  (let ((*declaration-type-test-one-array*
          (make-array 10 :element-type '(signed-byte 5)))
        (*declaration-type-test-another-array*
          (make-array 10 :element-type '(signed-byte 8))))
    (declaration-type-test-frob2 *declaration-type-test-one-array*)
    (declaration-type-test-frob2 *declaration-type-test-another-array*)
    (values)))

(defun declaration-type-test-bump-counters (counters)
  (declare (type (array fixnum *) declaration-type-test-bump-counters))
  (dotimes (i (length counters))
    (incf (aref counters i))))


;;
;;  Declaration FTYPE
;;
(deftest declaration-ftype.1
  (locally
    (declare (ftype function))
    nil)
  nil)

(deftest declaration-ftype.2
  (labels ((aaa () :hello))
    (declare (ftype (function ()) aaa))
    (aaa))
  :hello)

(deftest declaration-ftype.3
  (labels ((aaa () :hello))
    (declare (ftype (function () symbol) aaa))
    (aaa))
  :hello)

(deftest-error declaration-ftype.4
  (eval '(labels ((aaa () :hello))
           (declare (ftype function (function aaa)))
           (aaa))))

(deftest-error declaration-ftype.5
  (eval '(labels ((aaa (x) x :hello))
           (declare (ftype (function (integer)) aaa))
           (aaa "Hello"))))

(deftest-error declaration-ftype-error.1
  (eval '(locally
           (declare (ftype)))))

(deftest declaration-ftype-test.1
  (locally
    (declare (ftype (function (integer list) t) ith)
             (ftype (function (number) float) sine cosine))
    :hello)
  :hello)


;;
;;  Declaration INLINE
;;
(deftest declaration-inline.1
  (locally
    (declare (inline)))
  nil)

(deftest declaration-inline.2
  (flet ((x ()) (y ()))
    (declare (inline x y))
    (x) (y))
  nil)

(deftest-error declaration-inline-error.1
  (eval '(flet ((aaa () :hello))
           (declare (inline (function aaa)))
           (aaa))))

(deftest-error declaration-inline-error.2
  (eval '(locally (declare (inline 10)))))


;;
;;  Declaration NOTINLINE
;;
(deftest declaration-notinline.1
  (locally
    (declare (notinline)))
  nil)

(deftest declaration-notinline.2
  (flet ((x ()) (y ()))
    (declare (notinline x y))
    (x) (y))
  nil)

(deftest-error declaration-notinline-error.1
  (eval '(flet ((aaa () :hello))
           (declare (notinline (function aaa)))
           (aaa))))

(deftest-error declaration-notinline-error.2
  (eval '(locally (declare (notinline 10)))))


;;
;;  Declaration DECLARATION
;;
(deftest declaration-declaration.1
  (progn
    (declaim (declaration))
    (values)))

(deftest declaration-declaration.2
  (progn
    (declaim (declaration declaration-test-1 declaration-test-2))
    (values)))

(deftest declaration-declaration.3
  (progn
    (declaim (declaration declaration-test-3))
    (declaim (declaration declaration-test-3))
    (declaim (declaration declaration-test-3))
    (values)))

(deftest declaration-declaration.4
  (progn
    (eval '(declaim (declaration declaration-test-4)))
    (eval '(declaim (declaration-test-4 "Hello")))
    (values)))

(deftest-error declaration-declaration-error.1
  (eval '(declaim (declaration 100))))

;;  ANSI Common Lisp
(deftest declaration-declaration-test.1
  (progn
    (eval '(declaim (declaration declaration-test-author
                                 declaration-target-language
                                 declaration-target-machine)))
    (eval '(declaim (declaration-target-language ada)))
    (eval '(declaim (declaration-target-machine IBM-650)))
    (eval '(defun declaration-test-strangep (x)
             (declare (declaration-test-author "Harry Tweeker"))
             (member x '(strange weird odd peculiar))))
    (values)))


;;
;;  Declaration OPTIMIZE
;;
(deftest declaration-optimize.1
  (locally
    (declare (optimize)))
  nil)

(deftest declaration-optimize.2
  (locally
    (declare (optimize compilation-speed))
    10)
  10)

(deftest declaration-optimize.3
  (locally
    (declare (optimize compilation-speed debug safety space speed))
    nil)
  nil)

(deftest declaration-optimize.4
  (locally
    (declare (optimize (compilation-speed 0)
                       (debug 1)
                       (safety 2)
                       (space 3)
                       (speed 0)))
    nil)
  nil)

(deftest declaration-optimize-error.5
  (handler-case
    (eval '(locally
             (declare (optimize no-such-optimization-quality))))
    (warning () :ok))
  :ok)

(deftest declaration-optimize-error.6
  (handler-case
    (eval '(locally
             (declare (optimize (no-such-optimization-quality 3)))))
    (warning () :ok))
  :ok)

(deftest-error declaration-optimize-error.1
  (eval '(locally
           (declare (optimize (compilation-speed -1))))))

(deftest-error declaration-optimize-error.2
  (eval '(locally
           (declare (optimize (compilation-speed 4))))))

;;  ANSI Common Lisp
(deftest declaration-optimize-test.
  (defun declaration-test-often-used-subroutine (x y)
    (declare (optimize (safety 2)))
    (error-check x y)
    (hairy-setup x)
    (do ((i 0 (+ i 1))
         (z x (cdr z)))
      ((null z))
      ;; This inner loop really needs to burn.
      (declare (optimize speed))
      (declare (fixnum i))))
  declaration-test-often-used-subroutine)


;;
;;  Declaration SPECIAL
;;
(deftest declaration-special.1
  (locally
    (declare (special)))
  nil)

(deftest declaration-special.2
  (locally
    (declare (special x y z)))
  nil)

(deftest declaration-special.3
  (locally
    (declare (special hello-symbol))
    (setq hello-symbol 10)
    (symbol-value 'hello-symbol))
  10)

(deftest declaration-special.4
  (let (hello-symbol)
    (declare (special hello-symbol))
    (setq hello-symbol 10)
    (symbol-value 'hello-symbol))
  10)

(deftest-error declaration-special-error.1
  (eval '(locally (declare (special 10)))))

;;  ANSI Common Lisp
(defun declaration-special-declare-eg (y)
  (declare (special y))
  (let ((y t))
    (list y
          (locally (declare (special y)) y))))

(deftest declaration-special-test.1
  (declaration-special-declare-eg nil)
  (t nil))

(declaim (special declaration-test-x))
(setf (symbol-value 'declaration-test-x) 6)
(defvar *declaration-test-2*)

(defun declaration-test-foo (declaration-test-x)
  (push declaration-test-x *declaration-test-2*)
  (let ((declaration-test-x (1+ declaration-test-x)))
    (declare (special declaration-test-x))
    (declaration-test-bar))
  (1+ declaration-test-x))

(defun declaration-test-bar ()
  (push (locally (declare (special declaration-test-x))
                 declaration-test-x)
        *declaration-test-2*))

(deftest declaration-special-test.2
  (let (*declaration-test-2*)
    (values
      (declaration-test-foo 10)
      (nreverse *declaration-test-2*)))
  11 (10 11))

(setf (symbol-value 'declaration-test-x) 6)
(defun declaration-test-bar2 (declaration-test-x y)
  (let ((old-x declaration-test-x)
        (declaration-test-x y))
    (declare (special declaration-test-x))
    (list old-x declaration-test-x)))

(deftest declaration-special-test.3
  (declaration-test-bar2 'first 'second)
  (first second))

(declaim (special declaration-test-prosp))
(setq declaration-test-prosp 1 declaration-test-reg 1)

(deftest declaration-special-test.4
  (let ((declaration-test-prosp 2) (declaration-test-reg 2))
    (set 'declaration-test-prosp 3)
    (set 'declaration-test-reg 3)
    (list declaration-test-prosp declaration-test-reg))
  (3 2))

(deftest declaration-special-test.5
  (list declaration-test-prosp declaration-test-reg)
  (1 3))

(deftest declaration-special-test.6
  (progn
    (declaim (special declaration-test-x))
    (defun declaration-special-example (declaration-test-x y)
      (declare (special y))
      (let ((y 3) (declaration-test-x (* declaration-test-x 2)))
        (print (+ y (locally (declare (special y)) y)))
        (let ((y 4)) (declare (special y)) (foo declaration-test-x)))))
  declaration-special-example)


;;
;;  Declaration OTHERS
;;
(deftest declaration-others.1
  (let ((x 10))
    (declare (integer x))
    x)
  10)

(deftest declaration-others.2
  (let ((x 10))
    (declare ((integer 0 100) x))
    x)
  10)

(deftest declaration-others.3
  (progn
    (eval '(declaim (declaration declaration-others-1)))
    (eval '(declaim (declaration-others-1 "Hello")))
    (values)))

(deftest declaration-others.4
  (progn
    (proclaim '(declaration declaration-others-2))
    (proclaim '(declaration-others-2 "Hello"))
    (values)))

(deftest declaration-others-error.1
  (eval '(progn
           (declaim (no-such-declare-name *declaration-others-error-1*))
           nil))
  nil)

(deftest-error declaration-others-error.2
  (eval '(progn
           (declaim (no-such-declare-name *declaration-others-error-1*))
           *declaration-others-error-1*)))

(deftest declaration-others-error.3
  (eval '(progn
           (proclaim '(no-such-declare-name *declaration-others-error-3*))
           nil))
  nil)

(deftest-error declaration-others-error.4
  (eval '(progn
           (proclaim '(no-such-declare-name *declaration-others-error-3*))
           *declaration-others-error-3*)))

(deftest declaration-others-error.5
  (eval '(locally
           (declare (no-such-declare-name *declaration-others-error-5*))))
  nil)

(deftest-error declaration-others-error.6
  (eval '(locally
           (declare (no-such-declare-name *declaration-others-error-5*))
           *declaration-others-error-5*)))


;;
;;  boundary
;;
(deftest declare-boundary.1
  (flet ((aaa (x) (let ((a 10))
                    (locally
                      (declare (type integer a))
                      (list x a)))))
    (aaa "Hello"))
  ("Hello" 10))

