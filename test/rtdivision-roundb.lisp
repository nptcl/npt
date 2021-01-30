;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  round
;;

;;  bignum - fixnum
(deftest round-bf.1
  (roundb 0 10)
  0 0)

(deftest round-bf.2
  (roundb 0 -10)
  0 0)

(deftest-error round-bf.3
  (roundb 10 0)
  division-by-zero)

(deftest round-bf.4a
  (roundb 9 3)
  3 0)

(deftest round-bf.4b
  (roundb -9 3)
  -3 0)

(deftest round-bf.4c
  (roundb 9 -3)
  -3 0)

(deftest round-bf.4d
  (roundb -9 -3)
  3 0)

(deftest round-bf.pp.1
  (roundb 2 10)
  0 2)

(deftest round-bf.pp.2
  (roundb 37 7)
  5 2)

(deftest round-bf.pp.3
  (roundb 38 7)
  5 3)

(deftest round-bf.pp.4
  (roundb 39 7)
  6 -3)

(deftest round-bf.pp.5
  (roundb 40 7)
  6 -2)

(deftest round-bf.pp.6
  (roundb 44 8)
  6 -4)

(deftest round-bf.pp.7
  (roundb 52 8)
  6 4)

(deftest round-bf.pm.1
  (roundb 2 -10)
  0 2)

(deftest round-bf.pm.2
  (roundb 37 -7)
  -5 2)

(deftest round-bf.pm.3
  (roundb 38 -7)
  -5 3)

(deftest round-bf.pm.4
  (roundb 39 -7)
  -6 -3)

(deftest round-bf.pm.5
  (roundb 40 -7)
  -6 -2)

(deftest round-bf.pm.6
  (roundb 44 -8)
  -6 -4)

(deftest round-bf.pm.7
  (roundb 52 -8)
  -6 4)

(deftest round-bf.mp.1
  (roundb -2 10)
  0 -2)

(deftest round-bf.mp.2
  (roundb -37 7)
  -5 -2)

(deftest round-bf.mp.3
  (roundb -38 7)
  -5 -3)

(deftest round-bf.mp.4
  (roundb -39 7)
  -6 3)

(deftest round-bf.mp.5
  (roundb -40 7)
  -6 2)

(deftest round-bf.mp.6
  (roundb -44 8)
  -6 4)

(deftest round-bf.mp.7
  (roundb -52 8)
  -6 -4)

(deftest round-bf.mm.1
  (roundb -2 -10)
  0 -2)

(deftest round-bf.mm.2
  (roundb -37 -7)
  5 -2)

(deftest round-bf.mm.3
  (roundb -38 -7)
  5 -3)

(deftest round-bf.mm.4
  (roundb -39 -7)
  6 3)

(deftest round-bf.mm.5
  (roundb -40 -7)
  6 2)

(deftest round-bf.mm.6
  (roundb -44 -8)
  6 4)

(deftest round-bf.mm.7
  (roundb -52 -8)
  6 -4)

;;  bignum - bignum
(defun roundbb (a b)
  (round (make-bignum a) (make-bignum b)))

(deftest round-bb.1
  (roundbb 0 10)
  0 0)

(deftest round-bb.2
  (roundbb 0 -10)
  0 0)

(deftest-error round-bb.3
  (roundbb 10 0)
  division-by-zero)

(deftest round-bb.4a
  (roundbb 9 3)
  3 0)

(deftest round-bb.4b
  (roundbb -9 3)
  -3 0)

(deftest round-bb.4c
  (roundbb 9 -3)
  -3 0)

(deftest round-bb.4d
  (roundbb -9 -3)
  3 0)

(deftest round-bb.pp.1
  (roundbb 2 10)
  0 2)

(deftest round-bb.pp.2
  (roundbb 37 7)
  5 2)

(deftest round-bb.pp.3
  (roundbb 38 7)
  5 3)

(deftest round-bb.pp.4
  (roundbb 39 7)
  6 -3)

(deftest round-bb.pp.5
  (roundbb 40 7)
  6 -2)

(deftest round-bb.pp.6
  (roundbb 44 8)
  6 -4)

(deftest round-bb.pp.7
  (roundbb 52 8)
  6 4)

(deftest round-bb.pm.1
  (roundbb 2 -10)
  0 2)

(deftest round-bb.pm.2
  (roundbb 37 -7)
  -5 2)

(deftest round-bb.pm.3
  (roundbb 38 -7)
  -5 3)

(deftest round-bb.pm.4
  (roundbb 39 -7)
  -6 -3)

(deftest round-bb.pm.5
  (roundbb 40 -7)
  -6 -2)

(deftest round-bb.pm.6
  (roundbb 44 -8)
  -6 -4)

(deftest round-bb.pm.7
  (roundbb 52 -8)
  -6 4)

(deftest round-bb.mp.1
  (roundbb -2 10)
  0 -2)

(deftest round-bb.mp.2
  (roundbb -37 7)
  -5 -2)

(deftest round-bb.mp.3
  (roundbb -38 7)
  -5 -3)

(deftest round-bb.mp.4
  (roundbb -39 7)
  -6 3)

(deftest round-bb.mp.5
  (roundbb -40 7)
  -6 2)

(deftest round-bb.mp.6
  (roundbb -44 8)
  -6 4)

(deftest round-bb.mp.7
  (roundbb -52 8)
  -6 -4)

(deftest round-bb.mm.1
  (roundbb -2 -10)
  0 -2)

(deftest round-bb.mm.2
  (roundbb -37 -7)
  5 -2)

(deftest round-bb.mm.3
  (roundbb -38 -7)
  5 -3)

(deftest round-bb.mm.4
  (roundbb -39 -7)
  6 3)

(deftest round-bb.mm.5
  (roundbb -40 -7)
  6 2)

(deftest round-bb.mm.6
  (roundbb -44 -8)
  6 4)

(deftest round-bb.mm.7
  (roundbb -52 -8)
  6 -4)

;;  bignum - ratio
(deftest round-br.1
  (roundb 0 10/3)
  0 0)

(deftest round-br.2
  (roundb 0 -10/3)
  0 0)

(deftest-error round-br.3
  (roundb 10 (make-ratio 0 1))
  division-by-zero)

(deftest round-br.4a
  (roundb 5 1/2)
  10 0)

(deftest round-br.4b
  (roundb -5 1/2)
  -10 0)

(deftest round-br.4c
  (roundb 5 -1/2)
  -10 0)

(deftest round-br.4d
  (roundb -5 -1/2)
  10 0)

(deftest round-br.pp.1
  (roundb 3 4/7)  ;; less 1/2
  5 1/7)

(deftest round-br.pp.2
  (roundb 5 4/7)  ;; greater 1/2
  9 -1/7)

(deftest round-br.pp.3
  (roundb 6 4/7)  ;; equal even 1/2
  10 2/7)

(deftest round-br.pp.4
  (roundb 2 4/7)  ;; equal odd 1/2
  4 -2/7)

(deftest round-br.pp.5
  (roundb 2 5/11) ;; not-equal evel 1/2
  4 2/11)

(deftest round-br.pp.6
  (roundb 3 5/9)  ;; not-equal odd 1/2
  5 2/9)

(deftest round-br.mp.1
  (roundb -3 4/7)  ;; less 1/2
  -5 -1/7)

(deftest round-br.mp.2
  (roundb -5 4/7)  ;; greater 1/2
  -9 1/7)

(deftest round-br.mp.3
  (roundb -6 4/7)  ;; equal even 1/2
  -10 -2/7)

(deftest round-br.mp.4
  (roundb -2 4/7)  ;; equal odd 1/2
  -4 2/7)

(deftest round-br.mp.5
  (roundb -2 5/11) ;; not-equal evel 1/2
  -4 -2/11)

(deftest round-br.mp.6
  (roundb -3 5/9)  ;; not-equal odd 1/2
  -5 -2/9)

(deftest round-br.pm.1
  (roundb 3 -4/7)  ;; less 1/2
  -5 1/7)

(deftest round-br.pm.2
  (roundb 5 -4/7)  ;; greater 1/2
  -9 -1/7)

(deftest round-br.pm.3
  (roundb 6 -4/7)  ;; equal even 1/2
  -10 2/7)

(deftest round-br.pm.4
  (roundb 2 -4/7)  ;; equal odd 1/2
  -4 -2/7)

(deftest round-br.pm.5
  (roundb 2 -5/11) ;; not-equal evel 1/2
  -4 2/11)

(deftest round-br.pm.6
  (roundb 3 -5/9)  ;; not-equal odd 1/2
  -5 2/9)

(deftest round-br.mm.1
  (roundb -3 -4/7)  ;; less 1/2
  5 -1/7)

(deftest round-br.mm.2
  (roundb -5 -4/7)  ;; greater 1/2
  9 1/7)

(deftest round-br.mm.3
  (roundb -6 -4/7)  ;; equal even 1/2
  10 -2/7)

(deftest round-br.mm.4
  (roundb -2 -4/7)  ;; equal odd 1/2
  4 2/7)

(deftest round-br.mm.5
  (roundb -2 -5/11) ;; not-equal evel 1/2
  4 -2/11)

(deftest round-br.mm.6
  (roundb -3 -5/9)  ;; not-equal odd 1/2
  5 -2/9)

;;  bignum - single-float
(defun round-check-bs (a b c d)
  (roundb-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-bs.1
  (roundb 0 10.3f0)
  0 0.0f0)

(deftest round-bs.2
  (roundb 0 -10.3f0)
  0 0.0f0)

(deftest-error round-bs.3
  (roundb 10 0.0f0)
  division-by-zero)

(deftest round-bs.4a
  (roundb 5 0.5f0)
  10 0.0f0)

(deftest round-bs.4b
  (roundb -5 0.5f0)
  -10 0.0f0)

(deftest round-bs.4c
  (roundb 5 -0.5f0)
  -10 0.0f0)

(deftest round-bs.4d
  (roundb -5 -0.5f0)
  10 0.0f0)

(deftest round-bs.pp.1
  (round-check-bs 2 1.5f0    1 0.5f0)  ;; less 1/2
  t)

(deftest round-bs.pp.2
  (round-check-bs 4 1.5f0    3 -0.5f0)  ;; greater 1/2
  t)

(deftest round-bs.pp.3
  (round-check-bs 5 2.0f0    2 1.0f0)  ;; equal even 1/2
  t)

(deftest round-bs.pp.4
  (round-check-bs 7 2.0f0    4 -1.0f0)  ;; equal odd 1/2
  t)

(deftest round-bs.mp.1
  (round-check-bs -2 1.5f0    -1 -0.5f0)  ;; less 1/2
  t)

(deftest round-bs.mp.2
  (round-check-bs -4 1.5f0    -3 0.5f0)  ;; greater 1/2
  t)

(deftest round-bs.mp.3
  (round-check-bs -5 2.0f0    -2 -1.0f0)  ;; equal even 1/2
  t)

(deftest round-bs.mp.4
  (round-check-bs -7 2.0f0    -4 1.0f0)  ;; equal odd 1/2
  t)

(deftest round-bs.pm.1
  (round-check-bs 2 -1.5f0    -1 0.5f0)  ;; less 1/2
  t)

(deftest round-bs.pm.2
  (round-check-bs 4 -1.5f0    -3 -0.5f0)  ;; greater 1/2
  t)

(deftest round-bs.pm.3
  (round-check-bs 5 -2.0f0    -2 1.0f0)  ;; equal even 1/2
  t)

(deftest round-bs.pm.4
  (round-check-bs 7 -2.0f0    -4 -1.0f0)  ;; equal odd 1/2
  t)

(deftest round-bs.mm.1
  (round-check-bs -2 -1.5f0    1 -0.5f0)  ;; less 1/2
  t)

(deftest round-bs.mm.2
  (round-check-bs -4 -1.5f0    3 0.5f0)  ;; greater 1/2
  t)

(deftest round-bs.mm.3
  (round-check-bs -5 -2.0f0    2 -1.0f0)  ;; equal even 1/2
  t)

(deftest round-bs.mm.4
  (round-check-bs -7 -2.0f0    4 1.0f0)  ;; equal odd 1/2
  t)

;;  bignum - double-float
(defun round-check-bd (a b c d)
  (roundb-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest round-bd.1
  (roundb 0 10.3d0)
  0 0.0d0)

(deftest round-bd.2
  (roundb 0 -10.3d0)
  0 0.0d0)

(deftest-error round-bd.3
  (roundb 10 0.0d0)
  division-by-zero)

(deftest round-bd.4a
  (roundb 5 0.5d0)
  10 0.0d0)

(deftest round-bd.4b
  (roundb -5 0.5d0)
  -10 0.0d0)

(deftest round-bd.4c
  (roundb 5 -0.5d0)
  -10 0.0d0)

(deftest round-bd.4d
  (roundb -5 -0.5d0)
  10 0.0d0)

(deftest round-bd.pp.1
  (round-check-bd 2 1.5d0    1 0.5d0)  ;; less 1/2
  t)

(deftest round-bd.pp.2
  (round-check-bd 4 1.5d0    3 -0.5d0)  ;; greater 1/2
  t)

(deftest round-bd.pp.3
  (round-check-bd 5 2.0d0    2 1.0d0)  ;; equal even 1/2
  t)

(deftest round-bd.pp.4
  (round-check-bd 7 2.0d0    4 -1.0d0)  ;; equal odd 1/2
  t)

(deftest round-bd.mp.1
  (round-check-bd -2 1.5d0    -1 -0.5d0)  ;; less 1/2
  t)

(deftest round-bd.mp.2
  (round-check-bd -4 1.5d0    -3 0.5d0)  ;; greater 1/2
  t)

(deftest round-bd.mp.3
  (round-check-bd -5 2.0d0    -2 -1.0d0)  ;; equal even 1/2
  t)

(deftest round-bd.mp.4
  (round-check-bd -7 2.0d0    -4 1.0d0)  ;; equal odd 1/2
  t)

(deftest round-bd.pm.1
  (round-check-bd 2 -1.5d0    -1 0.5d0)  ;; less 1/2
  t)

(deftest round-bd.pm.2
  (round-check-bd 4 -1.5d0    -3 -0.5d0)  ;; greater 1/2
  t)

(deftest round-bd.pm.3
  (round-check-bd 5 -2.0d0    -2 1.0d0)  ;; equal even 1/2
  t)

(deftest round-bd.pm.4
  (round-check-bd 7 -2.0d0    -4 -1.0d0)  ;; equal odd 1/2
  t)

(deftest round-bd.mm.1
  (round-check-bd -2 -1.5d0    1 -0.5d0)  ;; less 1/2
  t)

(deftest round-bd.mm.2
  (round-check-bd -4 -1.5d0    3 0.5d0)  ;; greater 1/2
  t)

(deftest round-bd.mm.3
  (round-check-bd -5 -2.0d0    2 -1.0d0)  ;; equal even 1/2
  t)

(deftest round-bd.mm.4
  (round-check-bd -7 -2.0d0    4 1.0d0)  ;; equal odd 1/2
  t)

;;  bignum - long-float
(defun round-check-bl (a b c d)
  (roundb-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest round-bl.1
  (roundb 0 10.3l0)
  0 0.0l0)

(deftest round-bl.2
  (roundb 0 -10.3l0)
  0 0.0l0)

(deftest-error round-bl.3
  (roundb 10 0.0l0)
  division-by-zero)

(deftest round-bl.4a
  (roundb 5 0.5l0)
  10 0.0l0)

(deftest round-bl.4b
  (roundb -5 0.5l0)
  -10 0.0l0)

(deftest round-bl.4c
  (roundb 5 -0.5l0)
  -10 0.0l0)

(deftest round-bl.4d
  (roundb -5 -0.5l0)
  10 0.0l0)

(deftest round-bl.pp.1
  (round-check-bl 2 1.5l0    1 0.5l0)  ;; less 1/2
  t)

(deftest round-bl.pp.2
  (round-check-bl 4 1.5l0    3 -0.5l0)  ;; greater 1/2
  t)

(deftest round-bl.pp.3
  (round-check-bl 5 2.0l0    2 1.0l0)  ;; equal even 1/2
  t)

(deftest round-bl.pp.4
  (round-check-bl 7 2.0l0    4 -1.0l0)  ;; equal odd 1/2
  t)

(deftest round-bl.mp.1
  (round-check-bl -2 1.5l0    -1 -0.5l0)  ;; less 1/2
  t)

(deftest round-bl.mp.2
  (round-check-bl -4 1.5l0    -3 0.5l0)  ;; greater 1/2
  t)

(deftest round-bl.mp.3
  (round-check-bl -5 2.0l0    -2 -1.0l0)  ;; equal even 1/2
  t)

(deftest round-bl.mp.4
  (round-check-bl -7 2.0l0    -4 1.0l0)  ;; equal odd 1/2
  t)

(deftest round-bl.pm.1
  (round-check-bl 2 -1.5l0    -1 0.5l0)  ;; less 1/2
  t)

(deftest round-bl.pm.2
  (round-check-bl 4 -1.5l0    -3 -0.5l0)  ;; greater 1/2
  t)

(deftest round-bl.pm.3
  (round-check-bl 5 -2.0l0    -2 1.0l0)  ;; equal even 1/2
  t)

(deftest round-bl.pm.4
  (round-check-bl 7 -2.0l0    -4 -1.0l0)  ;; equal odd 1/2
  t)

(deftest round-bl.mm.1
  (round-check-bl -2 -1.5l0    1 -0.5l0)  ;; less 1/2
  t)

(deftest round-bl.mm.2
  (round-check-bl -4 -1.5l0    3 0.5l0)  ;; greater 1/2
  t)

(deftest round-bl.mm.3
  (round-check-bl -5 -2.0l0    2 -1.0l0)  ;; equal even 1/2
  t)

(deftest round-bl.mm.4
  (round-check-bl -7 -2.0l0    4 1.0l0)  ;; equal odd 1/2
  t)

