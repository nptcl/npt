;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  fround
;;

;;  ratio - fixnum
(deftest fround-rf.1
  (fround (make-ratio 0 1) 10)
  0.0 0)

(deftest fround-rf.2
  (fround (make-ratio 0 1) -10)
  0.0 0)

(deftest-error fround-rf.3
  (fround 10/3 0)
  division-by-zero)

(deftest fround-rf.4a
  (fround 10/3 10)
  0.0 10/3)

(deftest fround-rf.pp.1
  (fround 39/7 4)
  1.0 11/7)

(deftest fround-rf.pp.2
  (fround 43/7 4)
  2.0 -13/7)

(deftest fround-rf.mp.1
  (fround -39/7 4)
  -1.0 -11/7)

(deftest fround-rf.mp.2
  (fround -43/7 4)
  -2.0 13/7)

(deftest fround-rf.pm.1
  (fround 39/7 -4)
  -1.0 11/7)

(deftest fround-rf.pm.2
  (fround 43/7 -4)
  -2.0 -13/7)

(deftest fround-rf.mm.1
  (fround -39/7 -4)
  1.0 -11/7)

(deftest fround-rf.mm.2
  (fround -43/7 -4)
  2.0 13/7)

;;  ratio - bignum
(defun froundrb (a b)
  (fround a (make-bignum b)))

(deftest fround-rb.1
  (froundrb (make-ratio 0 1) 10)
  0.0 0)

(deftest fround-rb.2
  (froundrb (make-ratio 0 1) -10)
  0.0 0)

(deftest-error fround-rb.3
  (froundrb 10/3 0)
  division-by-zero)

(deftest fround-rb.4a
  (froundrb 10/3 10)
  0.0 10/3)

(deftest fround-rb.pp.1
  (froundrb 39/7 4)
  1.0 11/7)

(deftest fround-rb.pp.2
  (froundrb 43/7 4)
  2.0 -13/7)

(deftest fround-rb.mp.1
  (froundrb -39/7 4)
  -1.0 -11/7)

(deftest fround-rb.mp.2
  (froundrb -43/7 4)
  -2.0 13/7)

(deftest fround-rb.pm.1
  (froundrb 39/7 -4)
  -1.0 11/7)

(deftest fround-rb.pm.2
  (froundrb 43/7 -4)
  -2.0 -13/7)

(deftest fround-rb.mm.1
  (froundrb -39/7 -4)
  1.0 -11/7)

(deftest fround-rb.mm.2
  (froundrb -43/7 -4)
  2.0 13/7)

;;  ratio - ratio
(deftest fround-rr.1
  (fround (make-ratio 0 1) 10/3)
  0.0 0)

(deftest fround-rr.2
  (fround (make-ratio 0 1) -10/3)
  0.0 0)

(deftest-error fround-rr.3
  (fround 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest fround-rr.4a
  (fround 3/4 5/6)
  1.0 -1/12)

(deftest fround-rr.4b
  (fround -3/4 5/6)
  -1.0 1/12)

(deftest fround-rr.4c
  (fround 3/4 -5/6)
  -1.0 -1/12)

(deftest fround-rr.4d
  (fround -3/4 -5/6)
  1.0 1/12)

(deftest fround-rr.5a
  (fround 11/4 7/6)
  2.0 5/12)

(deftest fround-rr.5b
  (fround -11/4 7/6)
  -2.0 -5/12)

(deftest fround-rr.5c
  (fround 11/4 -7/6)
  -2.0 5/12)

(deftest fround-rr.5d
  (fround -11/4 -7/6)
  2.0 -5/12)

;;  ratio - single-float
(defun fround-check-rs (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-rs.1
  (fround (make-ratio 0 1) 10.3f0)
  0.0f0 0.0f0)

(deftest fround-rs.2
  (fround (make-ratio 0 1) -10.3f0)
  0.0f0 0.0f0)

(deftest-error fround-rs.3
  (fround 10/3 0.0f0)
  division-by-zero)

(deftest fround-rs.4a
  (fround-check-rs 11/4 1.2f0    2.0f0 0.35f0)
  t)

(deftest fround-rs.4b
  (fround-check-rs -11/4 1.2f0    -2.0f0 -0.35f0)
  t)

(deftest fround-rs.4c
  (fround-check-rs 11/4 -1.2f0    -2.0f0 0.35f0)
  t)

(deftest fround-rs.4d
  (fround-check-rs -11/4 -1.2f0    2.0f0 -0.35f0)
  t)

(deftest fround-rs.5a
  (fround-check-rs 13/4 1.2f0    3.0f0 -0.35f0)
  t)

(deftest fround-rs.5b
  (fround-check-rs -13/4 1.2f0    -3.0f0 0.35f0)
  t)

(deftest fround-rs.5c
  (fround-check-rs 13/4 -1.2f0    -3.0f0 -0.35f0)
  t)

(deftest fround-rs.5d
  (fround-check-rs -13/4 -1.2f0    3.0f0 0.35f0)
  t)

;;  ratio - double-float
(defun fround-check-rd (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fround-rd.1
  (fround (make-ratio 0 1) 10.3d0)
  0.0d0 0.0d0)

(deftest fround-rd.2
  (fround (make-ratio 0 1) -10.3d0)
  0.0d0 0.0d0)

(deftest-error fround-rd.3
  (fround 10/3 0.0d0)
  division-by-zero)

(deftest fround-rd.4a
  (fround-check-rd 11/4 1.2d0    2.0d0 0.35d0)
  t)

(deftest fround-rd.4b
  (fround-check-rd -11/4 1.2d0    -2.0d0 -0.35d0)
  t)

(deftest fround-rd.4c
  (fround-check-rd 11/4 -1.2d0    -2.0d0 0.35d0)
  t)

(deftest fround-rd.4d
  (fround-check-rd -11/4 -1.2d0    2.0d0 -0.35d0)
  t)

(deftest fround-rd.5a
  (fround-check-rd 13/4 1.2d0    3.0d0 -0.35d0)
  t)

(deftest fround-rd.5b
  (fround-check-rd -13/4 1.2d0    -3.0d0 0.35d0)
  t)

(deftest fround-rd.5c
  (fround-check-rd 13/4 -1.2d0    -3.0d0 -0.35d0)
  t)

(deftest fround-rd.5d
  (fround-check-rd -13/4 -1.2d0    3.0d0 0.35d0)
  t)

;;  ratio - long-float
(defun fround-check-rl (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fround-rl.1
  (fround (make-ratio 0 1) 10.3l0)
  0.0l0 0.0l0)

(deftest fround-rl.2
  (fround (make-ratio 0 1) -10.3l0)
  0.0l0 0.0l0)

(deftest-error fround-rl.3
  (fround 10/3 0.0l0)
  division-by-zero)

(deftest fround-rl.4a
  (fround-check-rl 11/4 1.2l0    2.0l0 0.35l0)
  t)

(deftest fround-rl.4b
  (fround-check-rl -11/4 1.2l0    -2.0l0 -0.35l0)
  t)

(deftest fround-rl.4c
  (fround-check-rl 11/4 -1.2l0    -2.0l0 0.35l0)
  t)

(deftest fround-rl.4d
  (fround-check-rl -11/4 -1.2l0    2.0l0 -0.35l0)
  t)

(deftest fround-rl.5a
  (fround-check-rl 13/4 1.2l0    3.0l0 -0.35l0)
  t)

(deftest fround-rl.5b
  (fround-check-rl -13/4 1.2l0    -3.0l0 0.35l0)
  t)

(deftest fround-rl.5c
  (fround-check-rl 13/4 -1.2l0    -3.0l0 -0.35l0)
  t)

(deftest fround-rl.5d
  (fround-check-rl -13/4 -1.2l0    3.0l0 0.35l0)
  t)

