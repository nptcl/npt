;;
;;  ANSI COMMON LISP: 12. Numbers
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

(deftest random.1
  (and
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2)
    (<= 0 (random 3) 2))
  t)

(deftest random.2
  (and
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5)
    (<= 0.0 (random 0.5) 0.5))
  t)

(deftest random.3
  (values
    (integerp (random 10))
    (floatp (random 2.0)))
  t t)

(deftest random.4
  (let* ((state1 (make-random-state t))
         (state2 (make-random-state state1)))
    (values
      (equal (random 9999999999999999999999999999999999999999999999 state1)
             (random 9999999999999999999999999999999999999999999999 state2))
      (equal (random 1.0d5 state1)
             (random 1.0d5 state2))))
  t t)


