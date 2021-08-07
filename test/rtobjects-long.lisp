;;
;;  ANSI COMMON LISP: 7. Objects
;;
(defvar *combination-value*)

;;
;;  define-method-combination long
;;
(deftest combination-long.1
  (define-method-combination combination-long-1 () nil)
  combination-long-1)

(deftest combination-long.2
  (null
    (method-combination-instance 'combination-long-1))
  nil)

(define-method-combination
  combination-test (&optional (order :most-specific-first))
  ((around (:aaa))
   (before (:bbb))
   (primary () :order order :required t)
   (after (:ccc)))
  (flet ((calls (s) (mapcar (lambda (m) `(call-method ,m)) s)))
    (let ((form (destructuring-bind (a . b) primary
                  (if (or before after b)
                    `(multiple-value-prog1
                       (progn ,@(calls before)
                              (call-method ,a ,b))
                       ,@(calls after))
                    `(call-method ,a)))))
      (if around
        (destructuring-bind (a . b) around
          `(call-method ,a (,@b (make-method ,form))))
        form))))

(defgeneric combination-long-2 (a) (:method-combination combination-test))
(defmethod combination-long-2 (a)
  (+ a 10))

(defgeneric combination-long-3 (a) (:method-combination standard))
(defmethod combination-long-3 (a)
  (+ a 20))

(deftest combination-long.3
  (combination-long-2 111)
  121)

(deftest combination-long.4
  (combination-long-2 2000)
  2010)

(deftest combination-long.5
  (combination-long-3 111)
  131)

(deftest combination-long.6
  (combination-long-3 2000)
  2020)


;;
;;  lambda-list
;;
(defgeneric combination-lambda-list-1 (a)
            (:method-combination
              combination-test :most-specific-first))
(defmethod combination-lambda-list-1 (a)
  (+ a 10))

(deftest combination-lambda-list.1
  (combination-lambda-list-1 111)
  121)

(defgeneric combination-lambda-list-2 (a)
            (:method-combination
              combination-test :most-specific-last))
(defmethod combination-lambda-list-2 (a)
  (+ a 10))

(deftest combination-lambda-list.2
  (combination-lambda-list-2 111)
  121)


;;
;;  next-method-p
;;
(defgeneric combination-next-method-p-1 (a) (:method-combination combination-test))
(defmethod combination-next-method-p-1 (a)
  (declare (ignore a))
  (setq *combination-value* :primary1)
  (next-method-p))

(defmethod combination-next-method-p-1 ((a integer))
  (declare (ignore a))
  (setq *combination-value* :primary2)
  (next-method-p))

(deftest combination-next-method-p.1
  (progn
    (setq *combination-value* nil)
    (values
      (combination-next-method-p-1 100)
      *combination-value*))
  t :primary2)

(deftest combination-next-method-p.2
  (progn
    (setq *combination-value* nil)
    (values
      (combination-next-method-p-1 "Hello")
      *combination-value*))
  nil :primary1)

(defgeneric combination-next-method-p-2 (a) (:method-combination standard))
(defmethod combination-next-method-p-2 (a)
  (declare (ignore a))
  (setq *combination-value* :primary1)
  (next-method-p))

(defmethod combination-next-method-p-2 ((a integer))
  (declare (ignore a))
  (setq *combination-value* :primary2)
  (next-method-p))

(deftest combination-next-method-p.3
  (progn
    (setq *combination-value* nil)
    (values
      (combination-next-method-p-2 100)
      *combination-value*))
  t :primary2)

(deftest combination-next-method-p.4
  (progn
    (setq *combination-value* nil)
    (values
      (combination-next-method-p-2 "Hello")
      *combination-value*))
  nil :primary1)

(defgeneric combination-next-method-p-3 (a)
            (:method-combination combination-test :most-specific-last))
(defmethod combination-next-method-p-3 (a)
  (declare (ignore a))
  (setq *combination-value* :primary1)
  (next-method-p))

(defmethod combination-next-method-p-3 ((a integer))
  (declare (ignore a))
  (setq *combination-value* :primary2)
  (next-method-p))

(deftest combination-next-method-p.5
  (progn
    (setq *combination-value* nil)
    (values
      (combination-next-method-p-3 100)
      *combination-value*))
  t :primary1)

(deftest combination-next-method-p.6
  (progn
    (setq *combination-value* nil)
    (values
      (combination-next-method-p-3 "Hello")
      *combination-value*))
  nil :primary1)


;;
;;  call-next-method
;;
(defgeneric combination-call-next-method-1 (a) (:method-combination combination-test))
(defmethod combination-call-next-method-1 (a)
  (declare (ignore a))
  (push :primary1 *combination-value*)
  :finish1)

(defmethod combination-call-next-method-1 ((a integer))
  (declare (ignore a))
  (push :primary2 *combination-value*)
  (call-next-method))

(deftest combination-call-next-method.1
  (progn
    (setq *combination-value* nil)
    (values
      (combination-call-next-method-1 100)
      *combination-value*))
  :finish1 (:primary1 :primary2))

(deftest combination-call-next-method.2
  (progn
    (setq *combination-value* nil)
    (values
      (combination-call-next-method-1 "Hello")
      *combination-value*))
  :finish1 (:primary1))

(defgeneric combination-call-next-method-2 (a) (:method-combination standard))
(defmethod combination-call-next-method-2 (a)
  (declare (ignore a))
  (push :primary1 *combination-value*)
  :finish1)

(defmethod combination-call-next-method-2 ((a integer))
  (declare (ignore a))
  (push :primary2 *combination-value*)
  (call-next-method))

(deftest combination-call-next-method.3
  (progn
    (setq *combination-value* nil)
    (values
      (combination-call-next-method-2 100)
      *combination-value*))
  :finish1 (:primary1 :primary2))

(deftest combination-call-next-method.4
  (progn
    (setq *combination-value* nil)
    (values
      (combination-call-next-method-2 "Hello")
      *combination-value*))
  :finish1 (:primary1))


;;
;;  around
;;
(defgeneric combination-around-1 (a) (:method-combination combination-test))
(defmethod combination-around-1 (a)
  (push :primary *combination-value*)
  (when (next-method-p)
    (call-next-method))
  (list :call1 a))

(defmethod combination-around-1 :aaa (a)
  (push :around *combination-value*)
  (when (next-method-p)
    (call-next-method))
  (list :call2 a))

(deftest combination-around.1
  (progn
    (setq *combination-value* nil)
    (values
      (combination-around-1 100)
      *combination-value*))
  (:call2 100) (:primary :around))

(defgeneric combination-around-2 (a) (:method-combination standard))
(defmethod combination-around-2 (a)
  (push :primary *combination-value*)
  (when (next-method-p)
    (call-next-method))
  (list :call1 a))

(defmethod combination-around-2 :around (a)
  (push :around *combination-value*)
  (when (next-method-p)
    (call-next-method))
  (list :call2 a))

(deftest combination-around.2
  (progn
    (setq *combination-value* nil)
    (values
      (combination-around-2 200)
      *combination-value*))
  (:call2 200) (:primary :around))


;;
;;  before after
;;
(defgeneric combination-before-after-1 (a) (:method-combination combination-test))
(defmethod combination-before-after-1 (a)
  (push :primary *combination-value*)
  (list :call1 a))

(defmethod combination-before-after-1 :bbb (a)
  (push :before *combination-value*)
  (list :call2 a))

(defmethod combination-before-after-1 :ccc (a)
  (push :after *combination-value*)
  (list :call3 a))

(deftest combination-before-after.1
  (progn
    (setq *combination-value* nil)
    (values
      (combination-before-after-1 111)
      *combination-value*))
  (:call1 111) (:after :primary :before))

(defgeneric combination-before-after-2 (a) (:method-combination standard))
(defmethod combination-before-after-2 (a)
  (push :primary *combination-value*)
  (list :call1 a))

(defmethod combination-before-after-2 :before (a)
  (push :before *combination-value*)
  (list :call2 a))

(defmethod combination-before-after-2 :after (a)
  (push :after *combination-value*)
  (list :call3 a))

(deftest combination-before-after.2
  (progn
    (setq *combination-value* nil)
    (values
      (combination-before-after-2 111)
      *combination-value*))
  (:call1 111) (:after :primary :before))


;;
;;  required
;;
(defgeneric combination-required-1 (a) (:method-combination combination-test))
(defmethod combination-required-1 :aaa (a)
  (list :call1 a))

(deftest-error combination-required.1
  (combination-required-1 100))

(defgeneric combination-required-2 (a) (:method-combination standard))
(defmethod combination-required-2 :around (a)
  (list :call1 a))

(deftest-error combination-required.2
  (combination-required-2 100))


;;
;;  no-generic
;;
(fmakunbound 'combination-no-generic)
(defmethod combination-no-generic (a b c)
  (+ a b c))

(deftest combination-no-generic.1
  (combination-no-generic 10 20 30)
  60)

(deftest combination-no-generic.2
  (closp #'combination-no-generic)
  t)


;;
;;  generic-function
;;
(define-method-combination
  combination-generic-1 ()
  ((primary () :required t))
  (:generic-function gen)
  `(progn
     (setq *combination-value* ,gen)
     (call-method ,(car primary) ,(cdr primary))))

(defgeneric combination-generic-2 (a) (:method-combination combination-generic-1))
(defmethod combination-generic-2 (a)
  (+ a 111))

(deftest combination-generic.1
  (progn
    (setq *combination-value* nil)
    (values
      (combination-generic-2 90000)
      (generic-function-name *combination-value*)))
  90111
  combination-generic-2)

(define-method-combination
  combination-generic-3 ()
  ((primary () :required t))
  (:generic-function gen)
  `(call-method ,(car primary) ,(cdr primary)))

(defgeneric combination-generic-4 (a) (:method-combination combination-generic-3))
(defmethod combination-generic-4 (a)
  (+ a 222))

(deftest combination-generic.2
  (combination-generic-4 90000)
  90222)


;;
;;  declare
;;
(define-method-combination
  combination-declare-1 (x y z)
  ((primary ()))
  (declare (special x y)
           (ignorable z))
  `(call-method ,(car primary) ,(cdr primary)))

(defgeneric combination-declare-1 ()
            (:method-combination
              combination-declare-1 10 20 30))
(defmethod combination-declare-1 ()
  :hello)

(deftest combination-declare.1
  (combination-declare-1)
  :hello)


;;
;;  documentation
;;
(define-method-combination
  combination-documentation-1 ()
  ((primary ()))
  "Hello"
  `(call-method ,(car primary) ,(cdr primary)))

(defgeneric combination-documentation-1 ()
            (:method-combination combination-documentation-1))
(defmethod combination-documentation-1 ()
  :hello)

(deftest combination-documentation.1
  (combination-documentation-1)
  :hello)

(deftest combination-documentation.2
  (documentation 'combination-documentation-1 'method-combination)
  "Hello")

(deftest combination-documentation.3
  (documentation
    (find-method-combination
      #'combination-documentation-1
      'combination-documentation-1
      nil)
    't)
  "Hello")


;;
;;  qualifiers
;;

;;  1. empty, ()
(define-method-combination
  combination-empty-1 ()
  ((primary ())
   (hello (:hello)))
  `(call-method ,(car primary) ,(cdr primary)))

(defgeneric combination-empty-2 ()
            (:method-combination combination-empty-1))
(defmethod combination-empty-2 :hello ()
  10)

(defmethod combination-empty-2 ()
  20)

(deftest combination-empty.1
  (combination-empty-2)
  20)


;;  2. any, *
(define-method-combination
  combination-any-1 ()
  ((primary *)
   (hello (:hello)))
  `(+ ,@(mapcar
          (lambda (x) `(call-method ,x))
          primary)))

(defgeneric combination-any-2 (x)
            (:method-combination combination-any-1))
(defmethod combination-any-2 :abc (x)
  (declare (ignore x))
  10)
(defmethod combination-any-2 ((x integer))
  (declare (ignore x))
  20)
(defmethod combination-any-2 ((x real))
  (declare (ignore x))
  30)
(deftest combination-any.1
  (combination-any-2 999)
  60)


;;  3. list, (a b c ...)
(define-method-combination
  combination-list-1 ()
  ((plus (aa bb cc))
   (minus (dd ee)))
  `(- (+ ,@(mapcar
             (lambda (x) `(call-method ,x))
             plus))
      ,@(mapcar
          (lambda (x) `(call-method ,x))
          minus)))

(defgeneric combination-list-2 (x)
            (:method-combination combination-list-1))

(deftest-error combination-list.1
  (defmethod combination-list-2 (x)
    (declare (ignore x))))

(defmethod combination-list-2 aa bb cc (x)
  (declare (ignore x))
  10)

(defmethod combination-list-2 aa bb cc ((x rational))
  (declare (ignore x))
  20)

(defmethod combination-list-2 dd ee ((x rational))
  (declare (ignore x))
  100)

(deftest combination-list.2
  (combination-list-2 999)
  -70)


;;  4. asterisk, (a b * c ...)
(define-method-combination
  combination-asterisk-1 ()
  ((plus (aa * cc))
   (minus (dd *)))
  `(- (+ ,@(mapcar
             (lambda (x) `(call-method ,x))
             plus))
      ,@(mapcar
          (lambda (x) `(call-method ,x))
          minus)))

(defgeneric combination-asterisk-2 (x)
            (:method-combination combination-asterisk-1))

(deftest-error combination-asterisk.1
  (defmethod combination-asterisk-2 (x)
    (declare (ignore x))))

(defmethod combination-asterisk-2 aa hello cc (x)
  (declare (ignore x))
  10)

(defmethod combination-asterisk-2 aa :zzzzz cc ((x rational))
  (declare (ignore x))
  20)

(defmethod combination-asterisk-2 dd :before ((x rational))
  (declare (ignore x))
  100)

(deftest combination-asterisk.2
  (combination-asterisk-2 999)
  -70)


;;  5. dotted, (a b c . *)
(define-method-combination
  combination-dotted-1 ()
  ((plus (aa bb . *))
   (minus (dd . *)))
  `(- (+ ,@(mapcar
             (lambda (x) `(call-method ,x))
             plus))
      ,@(mapcar
          (lambda (x) `(call-method ,x))
          minus)))

(defgeneric combination-dotted-2 (x)
            (:method-combination combination-dotted-1))

(deftest-error combination-dotted.1
  (defmethod combination-dotted-2 (x)
    (declare (ignore x))))

(defmethod combination-dotted-2 aa bb (x)
  (declare (ignore x))
  10)

(defmethod combination-dotted-2 aa bb cc dd ee ff gg ((x rational))
  (declare (ignore x))
  20)

(defmethod combination-dotted-2 dd :hello :around ((x rational))
  (declare (ignore x))
  100)

(deftest combination-dotted.2
  (combination-dotted-2 999)
  -70)


;;  6. predicate-function
(defun combination-predicate-2 (x)
  (equal x '(aa bb cc)))

(defun combination-predicate-3 (x)
  (equal x '(dd ee)))

(define-method-combination
  combination-predicate-1 ()
  ((plus combination-predicate-2)
   (minus combination-predicate-3))
  `(- (+ ,@(mapcar
             (lambda (x) `(call-method ,x))
             plus))
      ,@(mapcar
          (lambda (x) `(call-method ,x))
          minus)))

(defgeneric combination-predicate-4 (x)
            (:method-combination combination-predicate-1))

(deftest-error combination-predicate.1
  (defmethod combination-predicate-4 (x)
    (declare (ignore x))))

(defmethod combination-predicate-4 aa bb cc (x)
  (declare (ignore x))
  10)

(defmethod combination-predicate-4 aa bb cc ((x rational))
  (declare (ignore x))
  20)

(defmethod combination-predicate-4 dd ee ((x rational))
  (declare (ignore x))
  100)

(deftest combination-predicate.2
  (combination-predicate-4 999)
  -70)


;;  7. collision
(define-method-combination
  combination-collision-1 ()
  ((primary *)
   (hello (:hello)))
  `(+ ,@(mapcar
          (lambda (x) `(call-method ,x))
          primary)))

(deftest-error combination-collision.1
  (progn
    (defgeneric combination-collision-2 ()
                (:method-combination combination-collision-1))
    (defmethod combination-collision-2 :hello ()
      10)))

(deftest combination-collision.2
  (progn
    (defgeneric combination-collision-3 ()
                (:method-combination combination-collision-1))
    (defmethod combination-collision-3 ()
      10)
    (combination-collision-3))
  10)


;;  :order error
(define-method-combination
  combination-order-error-1 ()
  ((primary () :order :hello)
   (hello (:hello)))
  `(concatenate 'string ,@(mapcar
                            (lambda (x) `(call-method ,x))
                            primary)))

(defgeneric combination-order-error-2 (x)
            (:method-combination combination-order-error-1))

(defmethod combination-order-error-2 (x)
  (declare (ignore x))
  "AAA")
(defmethod combination-order-error-2 ((x integer))
  (declare (ignore x))
  "BBB")

(deftest-error combination-order-error.1
  (combination-order-error-2 100))


;;  :order evaluated
(defparameter *combination-order-evaluated-1* :most-specific-first)
(define-method-combination
  combination-order-evaluated-1 ()
  ((primary () :order *combination-order-evaluated-1*)
   (hello (:hello)))
  `(concatenate 'string ,@(mapcar
                            (lambda (x) `(call-method ,x))
                            primary)))

(defgeneric combination-order-evaluated-2 (x)
            (:method-combination combination-order-evaluated-1))

(defmethod combination-order-evaluated-2 (x)
  (declare (ignore x))
  "AAA")
(defmethod combination-order-evaluated-2 ((x integer))
  (declare (ignore x))
  "BBB")

(deftest combination-order-evaluated.1
  (combination-order-evaluated-2 100)
  "BBBAAA")


;;  :order :most-specific-first
(define-method-combination
  combination-order-first-1 ()
  ((primary () :order :most-specific-first)
   (hello (:hello)))
  `(concatenate 'string ,@(mapcar
                            (lambda (x) `(call-method ,x))
                            primary)))

(defgeneric combination-order-first-2 (x)
            (:method-combination combination-order-first-1))

(defmethod combination-order-first-2 (x)
  (declare (ignore x))
  "AAA")
(defmethod combination-order-first-2 ((x integer))
  (declare (ignore x))
  "BBB")

(deftest combination-order-first.1
  (combination-order-first-2 100)
  "BBBAAA")


;;  :order :most-specific-last
(define-method-combination
  combination-order-last-1 ()
  ((primary () :order :most-specific-last)
   (hello (:hello)))
  `(concatenate 'string ,@(mapcar
                            (lambda (x) `(call-method ,x))
                            primary)))

(defgeneric combination-order-last-2 (x)
            (:method-combination combination-order-last-1))

(defmethod combination-order-last-2 (x)
  (declare (ignore x))
  "AAA")
(defmethod combination-order-last-2 ((x integer))
  (declare (ignore x))
  "BBB")

(deftest combination-order-last.1
  (combination-order-last-2 100)
  "AAABBB")


;;
;;  arguments
;;
(define-method-combination
  combination-arguments-1 ()
  ((primary () :required t))
  (:arguments a b c)
  `(progn
     (setq *combination-value* (list ,a ,b ,c))
     (call-method ,(car primary) ,(cdr primary))))

;;  arguments var
(defgeneric combination-argument-2 (x y z)
            (:method-combination combination-arguments-1))

(defmethod combination-argument-2 (a b c)
  (+ a b c))

(deftest combination-arguments.1
  (values
    (combination-argument-2 10 20 30)
    *combination-value*)
  60 (10 20 30))


;;  arguments var, short
(defgeneric combination-argument-3 (x y)
            (:method-combination combination-arguments-1))

(defmethod combination-argument-3 (a b)
  (+ a b))

(deftest combination-arguments.2
  (values
    (combination-argument-3 10 20)
    *combination-value*)
  30 (10 20 nil))


;;  arguments var, long
(defgeneric combination-argument-4 (x y z w)
            (:method-combination combination-arguments-1))

(defmethod combination-argument-4 (a b c d)
  (+ a b c d))

(deftest combination-arguments.3
  (values
    (combination-argument-4 10 20 30 40)
    *combination-value*)
  100 (10 20 30))


;;  arguments opt
(define-method-combination
  combination-arguments-5 ()
  ((primary () :required t))
  (:arguments a b &optional c)
  `(progn
     (setq *combination-value* (list ,a ,b ,c))
     (call-method ,(car primary) ,(cdr primary))))

(defgeneric combination-arguments-6 (x &rest r)
            (:method-combination combination-arguments-5))

(defmethod combination-arguments-6 (x &rest r)
  (list x r))

(deftest combination-arguments.4
  (values
    (combination-arguments-6 10)
    *combination-value*)
  (10 nil) (10 nil nil))

(deftest combination-arguments.5
  (values
    (combination-arguments-6 10 20)
    *combination-value*)
  (10 (20)) (10 20 nil))


(deftest combination-arguments.6
  (values
    (combination-arguments-6 10 20 30 40)
    *combination-value*)
  (10 (20 30 40)) (10 20 30))


;;  arguments rest
(define-method-combination
  combination-arguments-7 ()
  ((primary () :required t))
  (:arguments a b &rest c)
  `(progn
     (setq *combination-value* (list ,a ,b ,c))
     (call-method ,(car primary) ,(cdr primary))))

(defgeneric combination-arguments-8 (&rest r)
            (:method-combination combination-arguments-7))

(defmethod combination-arguments-8 (&rest r)
  r)

(deftest combination-arguments.7
  (values
    (combination-arguments-8)
    *combination-value*)
  nil (nil nil nil))

(deftest combination-arguments.8
  (values
    (combination-arguments-8 10 20 30 40 50)
    *combination-value*)
  (10 20 30 40 50) (10 20 (30 40 50)))


;;  arguments key
(define-method-combination
  combination-arguments-key-1 ()
  ((primary () :required t))
  (:arguments a &key b c)
  `(progn
     (setq *combination-value* (list ,a ,b ,c))
     (call-method ,(car primary) ,(cdr primary))))

(defgeneric combination-arguments-key-2 (&rest r)
            (:method-combination combination-arguments-key-1))

(defmethod combination-arguments-key-2 (&rest r)
  r)

(deftest combination-arguments-key.1
  (values
    (combination-arguments-key-2)
    *combination-value*)
  nil (nil nil nil))

(deftest combination-arguments-key.2
  (values
    (combination-arguments-key-2 10 :b 20)
    *combination-value*)
  (10 :b 20) (10 20 nil))

(deftest combination-arguments-key.3
  (values
    (combination-arguments-key-2 10 :c 30 :b 20)
    *combination-value*)
  (10 :c 30 :b 20) (10 20 30))

(deftest combination-arguments-key.4
  (values
    (combination-arguments-key-2 10 :b 20 :hello 100)
    *combination-value*)
  (10 :b 20 :hello 100) (10 20 nil))


;;  arguments key &allow-other-keys
(define-method-combination
  combination-arguments-key-3 ()
  ((primary () :required t))
  (:arguments a &key b c &allow-other-keys)
  `(progn
     (setq *combination-value* (list ,a ,b ,c))
     (call-method ,(car primary) ,(cdr primary))))

(defgeneric combination-arguments-key-4 (&rest r)
            (:method-combination combination-arguments-key-3))

(defmethod combination-arguments-key-4 (&rest r)
  r)

(deftest combination-arguments-key.5
  (values
    (combination-arguments-key-4)
    *combination-value*)
  nil (nil nil nil))

(deftest combination-arguments-key.6
  (values
    (combination-arguments-key-4 10 :b 20 :hello 30)
    *combination-value*)
  (10 :b 20 :hello 30) (10 20 nil))


;;  arguments whole
(define-method-combination
  combination-arguments-whole-1 ()
  ((primary () :required t))
  (:arguments &whole a b c)
  `(progn
     (setq *combination-value* (list ,a ,b ,c))
     (call-method ,(car primary) ,(cdr primary))))

(defgeneric combination-arguments-whole-2 (&rest r)
            (:method-combination combination-arguments-whole-1))

(defmethod combination-arguments-whole-2 (&rest r)
  (cons :hello r))

(deftest combination-arguments-whole.1
  (values
    (combination-arguments-whole-2)
    *combination-value*)
  (:hello) (nil nil nil))

(deftest combination-arguments-whole.2
  (values
    (combination-arguments-whole-2 10 20)
    *combination-value*)
  (:hello 10 20) ((10 20) 10 20))

(deftest combination-arguments-whole.3
  (values
    (combination-arguments-whole-2 10 20 30 40 50)
    *combination-value*)
  (:hello 10 20 30 40 50) ((10 20 30 40 50) 10 20))


;;
;;  description
;;
;;  description
;;  description format
;;  invalid-method-error
;;  method-combination-error
(deftest combination-description.1
  (progn
    (define-method-combination
      combination-description-1 ()
      ((primary () :description "Hello primary"))
      `(call-method ,(car primary) ,(cdr primary)))
    (values)))

