;;
;;  ANSI COMMON LISP: 5. Data and Control Flow
;;

;;
;;  Function APPLY
;;
(deftest apply.1
  (apply #'list '(10 20 30))
  (10 20 30))

(deftest apply.2
  (apply #'list 10 '(20 30))
  (10 20 30))

(deftest apply.3
  (apply #'list nil)
  nil)

(deftest apply.4
  (apply 'list '(10 20 30))
  (10 20 30))

(deftest apply.5
  (apply 'list 10 20 30 nil)
  (10 20 30))

(deftest apply.6
  (apply (lambda () (values 10 20 30)) nil)
  10 20 30)

;;  setf
(deftest apply-setf.1
  (let ((v (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
    (setf (apply #'aref v 1 '(0)) 999))
  999)

(deftest apply-setf.2
  (let ((v (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
    (setf (apply #'aref v 1 '(0)) 999)
    v)
  #2a((1 2 3) (999 5 6)))

(deftest apply-setf.3
  (let ((v (make-array '(2 4) :element-type 'bit
                       :initial-contents '((0 0 0 1) (1 1 0 0)))))
    (setf (apply #'bit v 1 '(2)) 1))
  1)

(deftest apply-setf.4
  (let ((v (make-array '(2 4) :element-type 'bit
                       :initial-contents '((0 0 0 1) (1 1 0 0)))))
    (setf (apply #'bit v 1 '(2)) 1)
    v)
  #2a((0 0 0 1) (1 1 1 0)))

(deftest apply-setf.5
  (let ((v (make-array '(2 4) :element-type 'bit
                       :initial-contents '((0 0 0 1) (1 1 0 0)))))
    (setf (apply #'sbit v 1 '(2)) 1))
  1)

(deftest apply-setf.6
  (let ((v (make-array '(2 4) :element-type 'bit
                       :initial-contents '((0 0 0 1) (1 1 0 0)))))
    (setf (apply #'sbit v 1 '(2)) 1)
    v)
  #2a((0 0 0 1) (1 1 1 0)))

;;  error
(deftest-error apply-error.1
  (eval '(apply 10 nil))
  type-error)

(deftest-error apply-error.2
  (eval '(apply #'car 20))
  type-error)

(deftest-error apply-error.3
  (apply #'car 10 nil)
  type-error)

(deftest-error! apply-error.4
  (eval '(apply #'car)))

(deftest-error apply-error.5
  (apply #'car nil nil nil))

(deftest-error! apply-error.6
  (eval '(apply)))

(deftest-error apply-error.7
  (apply 'no-such-function-object '(10 20))
  undefined-function)

(deftest-error apply-error.8
  (apply 'dotimes '(i 20) nil nil)
  undefined-function)

;;  ANSI Common Lisp
(deftest apply-test.1
  (let ((f '+))
    (apply f '(1 2)))
  3)

(deftest apply-test.2
  (let ((f #'-))
    (apply f '(1 2)))
  -1)

(deftest apply-test.3
  (apply #'max 3 5 '(2 7 3))
  7)

(deftest apply-test.4
  (apply 'cons '((+ 2 3) 4))
  ((+ 2 3) . 4))

(deftest apply-test.5
  (apply #'+ '())
  0)

(defparameter *apply-list* '(a b c))
(defun apply-strange-test (&rest x)
  (eq x *apply-list*))

(deftest apply-test.6
  (apply #'apply-strange-test *apply-list*)
  nil)

(defun apply-foo (size &rest keys &key double &allow-other-keys)
  (let ((v (apply #'make-array size :allow-other-keys t keys)))
    (if double (concatenate 'vector v v) v)))

(deftest apply-test.7
  (apply-foo 4 :initial-contents '(a b c d) :double t)
  #(a b c d a b c d))


;;
;;  Function FUNCALL
;;
(deftest funcall.1
  (funcall #'list 10 20 30)
  (10 20 30))

(deftest funcall.2
  (funcall 'list 10 20 30)
  (10 20 30))

(deftest funcall.3
  (funcall 'list)
  nil)

(deftest funcall.4
  (funcall (lambda () (values 10 20 30)))
  10 20 30)

(deftest-error funcall-error.1
  (eval '(funcall 10))
  type-error)

(deftest-error! funcall-error.2
  (eval '(funcall)))

(deftest-error funcall-error.3
  (eval '(funcall #'car 10))
  type-error)

(deftest-error funcall-error.4
  (eval '(funcall #'car)))

(deftest-error funcall-error.5
  (funcall 'no-such-function-object 10 20)
  undefined-function)

(deftest-error funcall-error.6
  (funcall 'dotimes '(i 20) nil)
  undefined-function)

;;  ANSI Common Lisp
(deftest funcall-test.1
  (funcall #'+ 1 2 3)
  6)

(deftest funcall-test.2
  (funcall 'car '(1 2 3))
  1)

(deftest funcall-test.3
  (funcall 'position 1 '(1 2 3 2 1) :start 1)
  4)

(deftest funcall-test.4
  (cons 1 2)
  (1 . 2))

(deftest funcall-test.5
  (flet ((cons (x y) `(kons ,x ,y)))
    (let ((cons (symbol-function '+)))
      (funcall #'cons
               (funcall 'cons 1 2)
               (funcall cons 1 2))))
  (kons (1 . 2) 3))


;;
;;  Macro DEFUN
;;
(deftest defun.1
  (defun defun-test-1 ())
  defun-test-1)

(deftest defun.2
  (defun-test-1)
  nil)

(deftest defun.3
  (progn
    (defun defun-test-1 () :hello)
    (defun-test-1))
  :hello)

(deftest defun.4
  (progn
    (defun defun-test-2 (x y z)
      (+ x y z 10000))
    (defun-test-2 1 2 3))
  10006)

(deftest defun.5
  (progn
    (defun defun-test-3 ()
      "Hello"
      10)
    (documentation 'defun-test-3 'function))
  "Hello")

(deftest-error defun.6
  (progn
    (defun defun-test-4 (x)
      x)
    (eval '(defun-test-4 10 20))))

(deftest-error defun.7
  (eval '(defun-test-4)))

(deftest-error defun.8
  (progn
    (defun defun-test-5 (x)
      (declare (type integer x))
      x)
    (eval '(defun-test-5 :hello)))
  type-error)

(deftest defun.9
  (progn
    (defun defun-test-6 (x)
      (declare (special x))
      (symbol-value 'x))
    (progv '(x) '(100)
      (defun-test-6 200)))
  200)

(deftest defun.10
  (progn
    (defun defun-test-7 ()
      :hello)
    (functionp #'defun-test-7))
  t)

(deftest defun.11
  (progn
    (defun defun-test-8 ()
      (return-from defun-test-8 100)
      200)
    (defun-test-8))
  100)

(deftest defun-setf.1
  (defun (setf defun-test-setf-1) (v x)
    (rplacd x v)
    :hello)
  (setf defun-test-setf-1))

(deftest defun-setf.2
  (functionp #'(setf defun-test-setf-1))
  t)

(deftest defun-setf.3
  (let ((v (cons 'a 'b)))
    (setf (defun-test-setf-1 v) 100))
  :hello)

(deftest defun-setf.4
  (let ((v (cons 'a 'b)))
    (setf (defun-test-setf-1 v) 200)
    v)
  (a . 200))

(deftest-error defun-error.1
  (eval '(defun 100 () :hello)))

(deftest-error defun-error.2
  (eval '(defun defun-error-test-1 20 :hello)))

(deftest-error defun-error.3
  (eval '(defun defun-error-test-1)))

(deftest-error defun-error.4
  (eval '(defun)))

;;  ANSI Common Lisp
(deftest defun-test.1
  (defun defun-test-recur (x)
    (when (> x 0)
      (defun-test-recur (1- x))))
  defun-test-recur)

(deftest defun-test.2
  (defun defun-test-ex (a b &optional c (d 66) &rest keys &key test (start 0))
    (list a b c d keys test start))
  defun-test-ex)

(deftest defun-test.3
  (defun-test-ex 1 2)
  (1 2 nil 66 nil nil 0))

(deftest defun-test.4
  (defun-test-ex 1 2 3 4 :test 'equal :start 50)
  (1 2 3 4 (:test equal :start 50) equal 50))

(deftest defun-test.5
  (defun-test-ex :test 1 :start 2)
  (:test 1 :start 2 nil nil 0))

(defun defun-test-discriminant (a b c)
  (declare (number a b c))
  "Compute the discriminant for a quadratic equation."
  (- (* b b) (* 4 a c)))

(deftest defun-test.6
  (defun-test-discriminant 1 2/3 -2)
  76/9)

(defun defun-test-careful-discriminant (a b c)
  "Compute the discriminant for a quadratic equation."
  (check-type a number)
  (check-type b number)
  (check-type c number)
  (locally (declare (number a b c))
           (- (* b b) (* 4 a c))))

(deftest defun-test.7
  (defun-test-careful-discriminant 1 2/3 -2)
  76/9)


;;
;;  Accessor FDEFINITION
;;
(deftest fdefinition.1
  (functionp
    (fdefinition 'car))
  t)

(deftest-error fdefinition.2
  (fdefinition (gensym))
  undefined-function)

(deftest fdefinition.3
  (functionp
    (fdefinition '(setf car)))
  t)

(deftest-error fdefinition.4
  (fdefinition (list 'setf (gensym)))
  undefined-function)

(deftest fdefinition.5
  (functionp
    (fdefinition 'dotimes))
  t)

(deftest-error fdefinition.6
  (flet ((no-such-fdefinition-function () :hello))
    (declare (ignorable #'no-such-fdefinition-function))
    (fdefinition 'no-such-fdefinition-function))
  undefined-function)

(deftest-error fdefinition-error.1
  (eval '(fdefinition '(aaa)))
  type-error)

(deftest-error! fdefinition-error.2
  (eval '(fdefinition)))

(deftest-error! fdefinition-error.3
  (eval '(fdefinition 'car 'car)))


;;
;;  Accessor (SETF FDEFINITION)
;;
(deftest setf-fdefinition.1
  (progn
    (setf (fdefinition 'setf-fdefinition-test) (lambda () :hello))
    (setf-fdefinition-test))
  :hello)

(deftest setf-fdefinition.2
  (let (value)
    (setf (fdefinition '(setf bbb)) (lambda (v) (setq value v)))
    (setf (bbb) 100)
    value)
  100)

(deftest-error setf-fdefinition-error.1
  (eval '(setf (fdefinition 100) (lambda () :hello)))
  type-error)

(deftest-error setf-fdefinition-error.2
  (eval '(setf (fdefinition) (lambda () :hello))))

(deftest-error setf-fdefinition-error.3
  (eval '(setf (fdefinition 'setf-fdefinition-error 20) (lambda () :hello))))


;;
;;  Function FBOUNDP
;;
(deftest fboundp.1
  (fboundp 'car)
  t)

(deftest fboundp.2
  (fboundp 'no-such-fboundp-function)
  nil)

(deftest fboundp.3
  (fboundp '(setf car))
  t)

(deftest fboundp.4
  (fboundp '(setf no-such-fboundp-function))
  nil)

(deftest fboundp.5
  (fboundp 'dotimes)
  t)

(deftest-error fboundp-error.1
  (eval '(fboundp 10))
  type-error)

(deftest-error! fboundp-error.2
  (eval '(fboundp)))

(deftest-error! fboundp-error.3
  (eval '(fboundp 'car nil)))

;;  ANSI Common Lisp
(deftest fboundp-test.1
  (fboundp 'car)
  t)

(deftest fboundp-test.2
  (fboundp 'nth-value)
  t) ;; nil?

(deftest fboundp-test.3
  (fboundp 'with-open-file)
  t)

(deftest fboundp-test.4
  (fboundp 'unwind-protect)
  t)

(deftest fboundp-test.5
  (progn
    (defun fboundp-my-function (x) x)
    (fboundp 'fboundp-my-function))
  t)

(deftest fboundp-test.6
  (let ((saved-definition (symbol-function 'fboundp-my-function)))
    (unwind-protect (progn (fmakunbound 'fboundp-my-function)
                           (fboundp 'fboundp-my-function))
      (setf (symbol-function 'fboundp-my-function) saved-definition)))
  nil)

(deftest fboundp-test.7
  (fboundp 'fboundp-my-function)
  t)

(deftest fboundp-test.8
  (progn
    (defmacro fboundp-my-macro (x) `',x)
    (fboundp 'fboundp-my-macro))
  t)

(deftest fboundp-test.9
  (fmakunbound 'fboundp-my-function)
  fboundp-my-function)

(deftest fboundp-test.10
  (fboundp 'fboundp-my-function)
  nil)

(deftest fboundp-test.11
  (flet ((fboundp-my-function (x) x))
    (declare (ignorable #'fboundp-my-function))
    (fboundp 'fboundp-my-function))
  nil)


;;
;;  Function FMAKUNBOUND
;;
(deftest fmakunbound.1
  (fmakunbound 'hello)
  hello)

(deftest fmakunbound.2
  (progn
    (defun fmakunbound-2 ())
    (let ((result (fboundp 'fmakunbound-2)))
      (fmakunbound 'fmakunbound-2)
      (values result (fboundp 'fmakunbound-2))))
  t nil)

(deftest fmakunbound.3
  (fmakunbound '(setf hello))
  (setf hello))

(deftest fmakunbound.4
  (progn
    (setf (fdefinition '(setf fmakunbound-4)) (lambda (v) v))
    (let ((result (fboundp '(setf fmakunbound-4))))
      (fmakunbound '(setf fmakunbound-4))
      (values result (fboundp '(setf fmakunbound-4)))))
  t nil)

(deftest fmakunbound.5
  (progn
    (defmacro fmakunbound-5 () nil)
    (let ((result (fboundp 'fmakunbound-5)))
      (fmakunbound 'fmakunbound-5)
      (values result (fboundp 'fmakunbound-5))))
  t nil)

(deftest-error fmakunbound-error.1
  (eval '(fmakunbound 10))
  type-error)

(deftest-error! fmakunbound-error.2
  (eval '(fmakunbound)))

(deftest-error! fmakunbound-error.3
  (eval '(fmakunbound 'hello 'hello)))

;;  ANSI Common Lisp
(deftest fmakunbound-test.1
  (progn
    (defun fmakunbound-add-some (x) (+ x 19))
    (fboundp 'fmakunbound-add-some))
  t)

(deftest fmakunbound-test.2
  (flet ((fmakunbound-add-some (x) (+ x 37)))
    (fmakunbound 'fmakunbound-add-some)
    (fmakunbound-add-some 1))
  38)

(deftest fmakunbound-test.3
  (fboundp 'fmakunbound-add-some)
  nil)


;;
;;  Special Operator FLET
;;
(deftest flet.1
  (flet ())
  nil)

(deftest flet.2
  (flet () 100)
  100)

(deftest flet.3
  (flet ((aaa () :hello))
    (aaa))
  :hello)

(deftest flet.4
  (flet ((aaa () :aaa))
    (flet ((aaa () :bbb)
           (bbb () (aaa)))
      (values (aaa) (bbb))))
  :bbb :aaa)

(deftest flet.5
  (flet ((aaa () (return-from aaa 10) 20))
    (aaa))
  10)

(deftest flet.6
  (flet ((aaa () 10))
    (flet ((aaa () (1+ (aaa))))
      (aaa)))
  11)

(deftest flet.7
  (flet ((aaa () 10))
    (declare (ignore #'aaa))
    100)
  100)

(deftest flet.8
  (flet ((no-such-flet-function () 10))
    (declare (ignore #'no-such-flet-function))
    (fboundp 'no-such-flet-function))
  nil)

(deftest flet-setf.1
  (flet (((setf aaa) (value cons) (rplacd cons value) :hello))
    (let ((v (cons 10 20)))
      (setf (aaa v) 30)))
  :hello)

(deftest flet-setf.2
  (flet (((setf aaa) (value cons) (rplacd cons value) :hello))
    (let ((v (cons 10 20)))
      (setf (aaa v) 30)
      v))
  (10 . 30))

(deftest flet-setf.3
  (flet (((setf aaa) (value cons) (rplacd cons value) :hello))
    (declare (ignore #'(setf aaa)))
    (fboundp '(setf aaa)))
  nil)

(deftest flet-closure.1
  (let (v)
    (flet ((aaa () :hello))
      (setq v #'aaa))
    (funcall v))
  :hello)

(deftest flet-closure.2
  (let (v)
    (flet ((aaa () :hello))
      (setq v (lambda () (aaa))))
    (funcall v))
  :hello)

(deftest-error flet-error.1
  (eval '(flet)))

(deftest-error flet-error.2
  (eval '(flet (aaa))))

(deftest-error flet-error.3
  (eval '(flet ((aaa)))))

(deftest-error flet-error.4
  (eval '(flet ((aaa 10)))))

(deftest-error flet-error.5
  (eval '(flet ((aaa ()) 20))))



;;
;;  Special Operator LABELS
;;
(deftest labels.1
  (labels ())
  nil)

(deftest labels.2
  (labels () 100)
  100)

(deftest labels.3
  (labels ((aaa () :hello))
    (aaa))
  :hello)

(deftest labels.4
  (labels ((aaa () :aaa))
    (declare (ignorable #'aaa))
    (labels ((aaa () :bbb)
             (bbb () (aaa)))
      (values (aaa) (bbb))))
  :bbb :bbb)

(deftest labels.5
  (labels ((aaa () (return-from aaa 10) 20))
    (aaa))
  10)

(deftest labels.6
  (labels ((aaa (x) (if (<= x 1) x (* x (aaa (1- x))))))
    (aaa 10))
  3628800)

(deftest labels.7
  (labels ((aaa () 10))
    (declare (ignore #'aaa))
    100)
  100)

(deftest labels.8
  (labels ((no-such-labels-function () 10))
    (declare (ignore #'no-such-labels-function))
    (fboundp 'no-such-labels-function))
  nil)

(deftest labels-reference.1
  (labels ((aaa () 100)
           (bbb () (aaa)))
    (bbb))
  100)

(deftest labels-reference.2
  (labels ((aaa () (bbb))
           (bbb () 200))
    (aaa))
  200)

(deftest labels-setf.1
  (labels (((setf aaa) (value cons) (rplacd cons value) :hello))
    (let ((v (cons 10 20)))
      (setf (aaa v) 30)))
  :hello)

(deftest labels-setf.2
  (labels (((setf aaa) (value cons) (rplacd cons value) :hello))
    (let ((v (cons 10 20)))
      (setf (aaa v) 30)
      v))
  (10 . 30))

(deftest labels-setf.3
  (labels (((setf aaa) (value cons) (rplacd cons value) :hello))
    (declare (ignore #'(setf aaa)))
    (fboundp '(setf aaa)))
  nil)

(deftest labels-closure.1
  (let (v)
    (labels ((aaa () :hello))
      (setq v #'aaa))
    (funcall v))
  :hello)

(deftest labels-closure.2
  (let (v)
    (labels ((aaa () :hello))
      (setq v (lambda () (aaa))))
    (funcall v))
  :hello)

(deftest-error labels-error.1
  (eval '(labels)))

(deftest-error labels-error.2
  (eval '(labels (aaa))))

(deftest-error labels-error.3
  (eval '(labels ((aaa)))))

(deftest-error labels-error.4
  (eval '(labels ((aaa 10)))))

(deftest-error labels-error.5
  (eval '(labels ((aaa ()) 20))))


;;
;;  Special Operator MACROLET
;;
(deftest macrolet.1
  (macrolet ())
  nil)

(deftest macrolet.2
  (macrolet () :hello)
  :hello)

(deftest macrolet.3
  (macrolet ((aaa (x) `(list ,x)))
    (aaa 10))
  (10))

(deftest macrolet.4
  (macrolet ((aaa (x) (return-from aaa `(list ,x)) `nil))
    (aaa 10))
  (10))

(deftest macrolet.5
  (macrolet ((aaa (&body body) `(progn ,@body)))
    (aaa 10 20 30))
  30)

(deftest-error macrolet-error.1
  (eval '(macrolet)))

(deftest-error macrolet-error.2
  (eval '(macrolet (aaa))))

(deftest-error macrolet-error.3
  (eval '(macrolet ((aaa)))))

(deftest-error macrolet-error.4
  (eval '(macrolet ((aaa 10)))))

(deftest-error macrolet-error.5
  (eval '(macrolet ((aaa ()) 20))))

;;  ANSI Common Lisp
(defun macrolet-test-1 (x flag)
  (macrolet ((fudge (z)
                    ;The parameters x and flag are not accessible
                    ; at this point; a reference to flag would be to
                    ; the global variable of that name.
                    ` (if flag (* ,z ,z) ,z)))
    ;The parameters x and flag are accessible here.
    (+ x
       (fudge x)
       (fudge (+ x 1)))))

(defun macrolet-test-2 (x flag)
  (+ x
     (if flag (* x x) x)
     (if flag (* (+ x 1) (+ x 1)) (+ x 1))))

(deftest flet-test.1
  (values
    (eql (macrolet-test-1 10 t)
         (macrolet-test-2 10 t))
    (eql (macrolet-test-1 10 nil)
         (macrolet-test-2 10 nil)))
  t t)

(deftest flet-test.2
  (flet ((flet1 (n) (+ n n)))
    (flet ((flet1 (n) (+ 2 (flet1 n))))
      (flet1 2)))
  6)

(deftest flet-test.3
  (progn
    (defun flet-dummy-function () 'top-level)
    (funcall #'flet-dummy-function))
  top-level)

(deftest flet-test.4
  (flet ((flet-dummy-function () 'shadow))
    (funcall #'flet-dummy-function))
  shadow)

(deftest flet-test.5
  (eq (funcall #'flet-dummy-function) (funcall 'flet-dummy-function))
  t)

(deftest flet-test.6
  (flet ((flet-dummy-function () 'shadow))
    (eq (funcall #'flet-dummy-function)
        (funcall 'flet-dummy-function)))
  nil)

(deftest flet-test.7
  (progn
    (defun flet-recursive-times (k n)
      (labels ((temp (n)
                     (if (zerop n) 0 (+ k (temp (1- n))))))
        (temp n)))
    (flet-recursive-times 2 3))
  6)

(deftest flet-test.8
  (progn
    (defmacro flet-mlets (x &environment env)
      (let ((form `(babbit ,x)))
        (macroexpand form env)))
    (macrolet ((babbit (z) `(+ ,z ,z))) (flet-mlets 5)))
  10)

(deftest flet-test.9
  (flet ((safesqrt (x) (sqrt (abs x))))
    ;; The safesqrt function is used in two places.
    (< (abs (- (safesqrt (apply #'+ (map 'list #'safesqrt '(1 2 3 4 5 6))))
               3.291173))
       0.01))
  t)

(defun flet-integer-power (n k)
  (declare (integer n))
  (declare (type (integer 0 *) k))
  (labels ((expt0 (x k a)
                  (declare (integer x a) (type (integer 0 *) k))
                  (cond ((zerop k) a)
                        ((evenp k) (expt1 (* x x) (floor k 2) a))
                        (t (expt0 (* x x) (floor k 2) (* x a)))))
           (expt1 (x k a)
                  (declare (integer x a) (type (integer 0 *) k))
                  (cond ((evenp k) (expt1 (* x x) (floor k 2) a))
                        (t (expt0 (* x x) (floor k 2) (* x a))))))
    (expt0 n k 1)))

(deftest flet-test.10
  (flet-integer-power 2 10)
  1024)

(defun flet-example (y l)
  (flet ((attach (x)
                 (setq l (append l (list x)))))
    (declare (inline attach))
    (dolist (x y)
      (unless (null (cdr x))
        (attach x)))
    l))

(deftest flet-test.11
  (flet-example '((a apple apricot) (b banana) (c cherry) (d) (e))
                '((1) (2) (3) (4 2) (5) (6 3 2)))
  ((1) (2) (3) (4 2) (5) (6 3 2) (a apple apricot) (b banana) (c cherry)))


;;
;;
;;
(deftest function.1
  (functionp
    (function car))
  t)

(deftest function.2
  (functionp
    (function
      (lambda () :hello)))
  t)

(deftest function.3
  (functionp
    (function
      (setf car)))
  t)

(deftest function-lambda-expression.1
  (function-lambda-expression #'car)
  nil nil car)

(deftest function-lambda-expression.2
  (function-lambda-expression (lambda () :hello))
  (lambda () :hello) nil nil)

(deftest function-lambda-expression.3
  (progn
    (defun function-lambda-expression-test-3 () :hello)
    (function-lambda-expression #'function-lambda-expression-test-3))
  nil nil function-lambda-expression-test-3)

(deftest function-lambda-expression.4
  (progn
    (defun (setf function-lambda-expression-test-4) (v) v)
    (function-lambda-expression #'(setf function-lambda-expression-test-4)))
  nil nil (setf function-lambda-expression-test-4))

