;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function MAKE-RANDOM-STATE
;;
(deftest make-random-state.1
  (typep (make-random-state) 'random-state)
  t)

(deftest make-random-state.2
  (let ((a (make-random-state))
        (b (make-random-state)))
    (values (eq a b) (equal-random-state a b)))
  nil t)

(deftest make-random-state.3
  (let ((a (make-random-state nil))
        (b (make-random-state nil)))
    (values (eq a b) (equal-random-state a b)))
  nil t)

(deftest make-random-state.4
  (let ((a (make-random-state t))
        (b (make-random-state t)))
    (values (eq a b)
            (equal-random-state a b)
            (equal-random-state a *random-state*)))
  nil nil nil)

(deftest make-random-state.5
  (let ((a (make-random-state nil))
        (b (make-random-state t)))
    (values
      (eq *random-state* a)
      (eq *random-state* b)
      (equal-random-state *random-state* a)
      (equal-random-state *random-state* b)))
  nil nil t nil)

(deftest make-random-state.6
  (let* ((a (make-random-state t))
         (b (make-random-state a))
         (c (make-random-state a)))
    (values
      (eq a b)
      (eq a c)
      (equal-random-state a b)
      (equal-random-state a c)))
  nil nil t t)

(deftest make-random-state.7
  (let ((a (make-random-state))
        (b (make-random-state nil))
        (c *random-state*))
    (values
      (eq a b)
      (eq a c)
      (equal-random-state a b)
      (equal-random-state a c)))
  nil nil t t)

(deftest-error! make-random-state-error.1
  (eval '(make-random-state nil nil)))

(deftest-error! make-random-state-error.2
  (eval '(make-random-state 10))
  type-error)


;;
;;  Function RANDOM
;;
(deftest random.1
  (random 1)
  0)

(deftest random.2
  (and (<= 0 (random 3) 2)
       (<= 0 (random 3) 2)
       (<= 0 (random 3) 2)
       (<= 0 (random 3) 2)
       (<= 0 (random 3) 2)
       (<= 0 (random 3) 2)
       (<= 0 (random 3) 2))
  t)

(deftest random.3
  (and (<= 0.0 (random 0.5) 0.5)
       (<= 0.0 (random 0.5) 0.5)
       (<= 0.0 (random 0.5) 0.5)
       (<= 0.0 (random 0.5) 0.5)
       (<= 0.0 (random 0.5) 0.5)
       (<= 0.0 (random 0.5) 0.5))
  t)

(deftest random.4
  (let* ((state1 (make-random-state t))
         (state2 (make-random-state state1)))
    (values
      (equal (random 9999999999999999999999999999999999999999999999 state1)
             (random 9999999999999999999999999999999999999999999999 state2))
      (equal (random 1.0d5 state1)
             (random 1.0d5 state2))))
  t t)

(deftest random.5
  (values
    (integerp (random 10))
    (= (random 100000000)
       (random 100000000)
       (random 100000000)
       (random 100000000)
       (random 100000000)))
  t nil)

(deftest random.6
  (values
    (typep (random 2.0f0) 'single-float)
    (= (random 2.0f0)
       (random 2.0f0)
       (random 2.0f0)
       (random 2.0f0)
       (random 2.0f0)
       (random 2.0f0)))
  t nil)

(deftest random.7
  (values
    (typep (random 2.0d0) 'double-float)
    (= (random 2.0d0)
       (random 2.0d0)
       (random 2.0d0)
       (random 2.0d0)
       (random 2.0d0)
       (random 2.0d0)))
  t nil)

(deftest random.8
  (values
    (typep (random 2.0L0) 'long-float)
    (= (random 2.0L0)
       (random 2.0L0)
       (random 2.0L0)
       (random 2.0L0)
       (random 2.0L0)
       (random 2.0L0)))
  t nil)

(deftest random.9
  (let ((r (make-random-state t)))
    (equal
      (let ((*random-state* (make-random-state r)))
        (random 10000))
      (let ((*random-state* (make-random-state r)))
        (random 10000))))
  t)

(deftest-error! random-error.1
  (eval '(random)))

(deftest-error! random-error.2
  (eval '(random 0))
  type-error)

(deftest-error! random-error.3
  (eval '(random 0.0))
  type-error)

(deftest-error! random-error.4
  (eval '(random 100 nil))
  type-error)

(deftest-error! random-error.5
  (eval '(random 100 *random-state* nil)))

;;  ANSI Common Lisp
(deftest random-test.1
  (<= 0 (random 1000) 1000)
  t)

(deftest random-test.2
  (let ((state1 (make-random-state))
        (state2 (make-random-state)))
    (= (random 1000 state1) (random 1000 state2)))
  t)


;;
;;  Variable *RANDOM-STATE*
;;
(deftest random-state-variable.1
  (typep *random-state* 'random-state)
  t)

(deftest random-state-variable.2
  (random-state-p *random-state*)
  t)

(deftest random-state-variable.3
  (lisp-system:specialp '*random-state*)
  t)

(deftest random-state-variable.4
  (integerp
    (random 100 *random-state*))
  t)

