;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  round
;;

;;  ratio - fixnum
(deftest round-rf.1
  (round (make-ratio 0 1) 10)
  0 0)

(deftest round-rf.2
  (round (make-ratio 0 1) -10)
  0 0)

(deftest-error round-rf.3
  (round 10/3 0)
  division-by-zero)

(deftest round-rf.4a
  (round 10/3 10)
  0 10/3)

(deftest round-rf.pp.1
  (round 39/7 4)
  1 11/7)

(deftest round-rf.pp.2
  (round 43/7 4)
  2 -13/7)

(deftest round-rf.mp.1
  (round -39/7 4)
  -1 -11/7)

(deftest round-rf.mp.2
  (round -43/7 4)
  -2 13/7)

(deftest round-rf.pm.1
  (round 39/7 -4)
  -1 11/7)

(deftest round-rf.pm.2
  (round 43/7 -4)
  -2 -13/7)

(deftest round-rf.mm.1
  (round -39/7 -4)
  1 -11/7)

(deftest round-rf.mm.2
  (round -43/7 -4)
  2 13/7)

;;  ratio - bignum
(defun roundrb (a b)
  (round a (make-bignum b)))

(deftest round-rb.1
  (roundrb (make-ratio 0 1) 10)
  0 0)

(deftest round-rb.2
  (roundrb (make-ratio 0 1) -10)
  0 0)

(deftest-error round-rb.3
  (roundrb 10/3 0)
  division-by-zero)

(deftest round-rb.4a
  (roundrb 10/3 10)
  0 10/3)

(deftest round-rb.pp.1
  (roundrb 39/7 4)
  1 11/7)

(deftest round-rb.pp.2
  (roundrb 43/7 4)
  2 -13/7)

(deftest round-rb.mp.1
  (roundrb -39/7 4)
  -1 -11/7)

(deftest round-rb.mp.2
  (roundrb -43/7 4)
  -2 13/7)

(deftest round-rb.pm.1
  (roundrb 39/7 -4)
  -1 11/7)

(deftest round-rb.pm.2
  (roundrb 43/7 -4)
  -2 -13/7)

(deftest round-rb.mm.1
  (roundrb -39/7 -4)
  1 -11/7)

(deftest round-rb.mm.2
  (roundrb -43/7 -4)
  2 13/7)

;;  ratio - ratio
(deftest round-rr.1
  (round (make-ratio 0 1) 10/3)
  0 0)

(deftest round-rr.2
  (round (make-ratio 0 1) -10/3)
  0 0)

(deftest-error round-rr.3
  (round 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest round-rr.4a
  (round 3/4 5/6)
  1 -1/12)

(deftest round-rr.4b
  (round -3/4 5/6)
  -1 1/12)

(deftest round-rr.4c
  (round 3/4 -5/6)
  -1 -1/12)

(deftest round-rr.4d
  (round -3/4 -5/6)
  1 1/12)

(deftest round-rr.5a
  (round 11/4 7/6)
  2 5/12)

(deftest round-rr.5b
  (round -11/4 7/6)
  -2 -5/12)

(deftest round-rr.5c
  (round 11/4 -7/6)
  -2 5/12)

(deftest round-rr.5d
  (round -11/4 -7/6)
  2 -5/12)

;;  ratio - single-float
(defun round-check-rs (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-rs.1
  (round (make-ratio 0 1) 10.3f0)
  0 0.0f0)

(deftest round-rs.2
  (round (make-ratio 0 1) -10.3f0)
  0 0.0f0)

(deftest-error round-rs.3
  (round 10/3 0.0f0)
  division-by-zero)

(deftest round-rs.4a
  (round-check-rs 11/4 1.2f0    2 0.35f0)
  t)

(deftest round-rs.4b
  (round-check-rs -11/4 1.2f0    -2 -0.35f0)
  t)

(deftest round-rs.4c
  (round-check-rs 11/4 -1.2f0    -2 0.35f0)
  t)

(deftest round-rs.4d
  (round-check-rs -11/4 -1.2f0    2 -0.35f0)
  t)

(deftest round-rs.5a
  (round-check-rs 13/4 1.2f0    3 -0.35f0)
  t)

(deftest round-rs.5b
  (round-check-rs -13/4 1.2f0    -3 0.35f0)
  t)

(deftest round-rs.5c
  (round-check-rs 13/4 -1.2f0    -3 -0.35f0)
  t)

(deftest round-rs.5d
  (round-check-rs -13/4 -1.2f0    3 0.35f0)
  t)

;;  ratio - double-float
(defun round-check-rd (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest round-rd.1
  (round (make-ratio 0 1) 10.3d0)
  0 0.0d0)

(deftest round-rd.2
  (round (make-ratio 0 1) -10.3d0)
  0 0.0d0)

(deftest-error round-rd.3
  (round 10/3 0.0d0)
  division-by-zero)

(deftest round-rd.4a
  (round-check-rd 11/4 1.2d0    2 0.35d0)
  t)

(deftest round-rd.4b
  (round-check-rd -11/4 1.2d0    -2 -0.35d0)
  t)

(deftest round-rd.4c
  (round-check-rd 11/4 -1.2d0    -2 0.35d0)
  t)

(deftest round-rd.4d
  (round-check-rd -11/4 -1.2d0    2 -0.35d0)
  t)

(deftest round-rd.5a
  (round-check-rd 13/4 1.2d0    3 -0.35d0)
  t)

(deftest round-rd.5b
  (round-check-rd -13/4 1.2d0    -3 0.35d0)
  t)

(deftest round-rd.5c
  (round-check-rd 13/4 -1.2d0    -3 -0.35d0)
  t)

(deftest round-rd.5d
  (round-check-rd -13/4 -1.2d0    3 0.35d0)
  t)

;;  ratio - long-float
(defun round-check-rl (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'long-float))

(deftest round-rl.1
  (round (make-ratio 0 1) 10.3l0)
  0 0.0l0)

(deftest round-rl.2
  (round (make-ratio 0 1) -10.3l0)
  0 0.0l0)

(deftest-error round-rl.3
  (round 10/3 0.0l0)
  division-by-zero)

(deftest round-rl.4a
  (round-check-rl 11/4 1.2l0    2 0.35l0)
  t)

(deftest round-rl.4b
  (round-check-rl -11/4 1.2l0    -2 -0.35l0)
  t)

(deftest round-rl.4c
  (round-check-rl 11/4 -1.2l0    -2 0.35l0)
  t)

(deftest round-rl.4d
  (round-check-rl -11/4 -1.2l0    2 -0.35l0)
  t)

(deftest round-rl.5a
  (round-check-rl 13/4 1.2l0    3 -0.35l0)
  t)

(deftest round-rl.5b
  (round-check-rl -13/4 1.2l0    -3 0.35l0)
  t)

(deftest round-rl.5c
  (round-check-rl 13/4 -1.2l0    -3 -0.35l0)
  t)

(deftest round-rl.5d
  (round-check-rl -13/4 -1.2l0    3 0.35l0)
  t)

