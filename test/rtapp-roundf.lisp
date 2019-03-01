;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  round
;;

;;  fixnum - fixnum
(deftest round-ff.1
  (round 0 10)
  0 0)

(deftest round-ff.2
  (round 0 -10)
  0 0)

(deftest-error round-ff.3
  (round 10 0)
  division-by-zero)

(deftest round-ff.4a
  (round 9 3)
  3 0)

(deftest round-ff.4b
  (round -9 3)
  -3 0)

(deftest round-ff.4c
  (round 9 -3)
  -3 0)

(deftest round-ff.4d
  (round -9 -3)
  3 0)

(deftest round-ff.pp.1
  (round 2 10)
  0 2)

(deftest round-ff.pp.2
  (round 37 7)
  5 2)

(deftest round-ff.pp.3
  (round 38 7)
  5 3)

(deftest round-ff.pp.4
  (round 39 7)
  6 -3)

(deftest round-ff.pp.5
  (round 40 7)
  6 -2)

(deftest round-ff.pp.6
  (round 44 8)
  6 -4)

(deftest round-ff.pp.7
  (round 52 8)
  6 4)

(deftest round-ff.pm.1
  (round 2 -10)
  0 2)

(deftest round-ff.pm.2
  (round 37 -7)
  -5 2)

(deftest round-ff.pm.3
  (round 38 -7)
  -5 3)

(deftest round-ff.pm.4
  (round 39 -7)
  -6 -3)

(deftest round-ff.pm.5
  (round 40 -7)
  -6 -2)

(deftest round-ff.pm.6
  (round 44 -8)
  -6 -4)

(deftest round-ff.pm.7
  (round 52 -8)
  -6 4)

(deftest round-ff.mp.1
  (round -2 10)
  0 -2)

(deftest round-ff.mp.2
  (round -37 7)
  -5 -2)

(deftest round-ff.mp.3
  (round -38 7)
  -5 -3)

(deftest round-ff.mp.4
  (round -39 7)
  -6 3)

(deftest round-ff.mp.5
  (round -40 7)
  -6 2)

(deftest round-ff.mp.6
  (round -44 8)
  -6 4)

(deftest round-ff.mp.7
  (round -52 8)
  -6 -4)

(deftest round-ff.mm.1
  (round -2 -10)
  0 -2)

(deftest round-ff.mm.2
  (round -37 -7)
  5 -2)

(deftest round-ff.mm.3
  (round -38 -7)
  5 -3)

(deftest round-ff.mm.4
  (round -39 -7)
  6 3)

(deftest round-ff.mm.5
  (round -40 -7)
  6 2)

(deftest round-ff.mm.6
  (round -44 -8)
  6 4)

(deftest round-ff.mm.7
  (round -52 -8)
  6 -4)

;;  fixnum - bignum
(defun roundfb (a b)
  (round a (make-bignum b)))

(deftest round-fb.1
  (roundfb 0 10)
  0 0)

(deftest round-fb.2
  (roundfb 0 -10)
  0 0)

(deftest-error round-fb.3
  (roundfb 10 0)
  division-by-zero)

(deftest round-fb.4a
  (roundfb 9 3)
  3 0)

(deftest round-fb.4b
  (roundfb -9 3)
  -3 0)

(deftest round-fb.4c
  (roundfb 9 -3)
  -3 0)

(deftest round-fb.4d
  (roundfb -9 -3)
  3 0)

(deftest round-fb.pp.1
  (roundfb 2 10)
  0 2)

(deftest round-fb.pp.2
  (roundfb 37 7)
  5 2)

(deftest round-fb.pp.3
  (roundfb 38 7)
  5 3)

(deftest round-fb.pp.4
  (roundfb 39 7)
  6 -3)

(deftest round-fb.pp.5
  (roundfb 40 7)
  6 -2)

(deftest round-fb.pp.6
  (roundfb 44 8)
  6 -4)

(deftest round-fb.pp.7
  (roundfb 52 8)
  6 4)

(deftest round-fb.pm.1
  (roundfb 2 -10)
  0 2)

(deftest round-fb.pm.2
  (roundfb 37 -7)
  -5 2)

(deftest round-fb.pm.3
  (roundfb 38 -7)
  -5 3)

(deftest round-fb.pm.4
  (roundfb 39 -7)
  -6 -3)

(deftest round-fb.pm.5
  (roundfb 40 -7)
  -6 -2)

(deftest round-fb.pm.6
  (roundfb 44 -8)
  -6 -4)

(deftest round-fb.pm.7
  (roundfb 52 -8)
  -6 4)

(deftest round-fb.mp.1
  (roundfb -2 10)
  0 -2)

(deftest round-fb.mp.2
  (roundfb -37 7)
  -5 -2)

(deftest round-fb.mp.3
  (roundfb -38 7)
  -5 -3)

(deftest round-fb.mp.4
  (roundfb -39 7)
  -6 3)

(deftest round-fb.mp.5
  (roundfb -40 7)
  -6 2)

(deftest round-fb.mp.6
  (roundfb -44 8)
  -6 4)

(deftest round-fb.mp.7
  (roundfb -52 8)
  -6 -4)

(deftest round-fb.mm.1
  (roundfb -2 -10)
  0 -2)

(deftest round-fb.mm.2
  (roundfb -37 -7)
  5 -2)

(deftest round-fb.mm.3
  (roundfb -38 -7)
  5 -3)

(deftest round-fb.mm.4
  (roundfb -39 -7)
  6 3)

(deftest round-fb.mm.5
  (roundfb -40 -7)
  6 2)

(deftest round-fb.mm.6
  (roundfb -44 -8)
  6 4)

(deftest round-fb.mm.7
  (roundfb -52 -8)
  6 -4)

;;  fixnum - ratio
(deftest round-fr.1
  (round 0 10/3)
  0 0)

(deftest round-fr.2
  (round 0 -10/3)
  0 0)

(deftest-error round-fr.3
  (round 10 (make-ratio 0 1))
  division-by-zero)

(deftest round-fr.4a
  (round 5 1/2)
  10 0)

(deftest round-fr.4b
  (round -5 1/2)
  -10 0)

(deftest round-fr.4c
  (round 5 -1/2)
  -10 0)

(deftest round-fr.4d
  (round -5 -1/2)
  10 0)

(deftest round-fr.pp.1
  (round 3 4/7)  ;; less 1/2
  5 1/7)

(deftest round-fr.pp.2
  (round 5 4/7)  ;; greater 1/2
  9 -1/7)

(deftest round-fr.pp.3
  (round 6 4/7)  ;; equal even 1/2
  10 2/7)

(deftest round-fr.pp.4
  (round 2 4/7)  ;; equal odd 1/2
  4 -2/7)

(deftest round-fr.pp.5
  (round 2 5/11) ;; not-equal evel 1/2
  4 2/11)

(deftest round-fr.pp.6
  (round 3 5/9)  ;; not-equal odd 1/2
  5 2/9)

(deftest round-fr.mp.1
  (round -3 4/7)  ;; less 1/2
  -5 -1/7)

(deftest round-fr.mp.2
  (round -5 4/7)  ;; greater 1/2
  -9 1/7)

(deftest round-fr.mp.3
  (round -6 4/7)  ;; equal even 1/2
  -10 -2/7)

(deftest round-fr.mp.4
  (round -2 4/7)  ;; equal odd 1/2
  -4 2/7)

(deftest round-fr.mp.5
  (round -2 5/11) ;; not-equal evel 1/2
  -4 -2/11)

(deftest round-fr.mp.6
  (round -3 5/9)  ;; not-equal odd 1/2
  -5 -2/9)

(deftest round-fr.pm.1
  (round 3 -4/7)  ;; less 1/2
  -5 1/7)

(deftest round-fr.pm.2
  (round 5 -4/7)  ;; greater 1/2
  -9 -1/7)

(deftest round-fr.pm.3
  (round 6 -4/7)  ;; equal even 1/2
  -10 2/7)

(deftest round-fr.pm.4
  (round 2 -4/7)  ;; equal odd 1/2
  -4 -2/7)

(deftest round-fr.pm.5
  (round 2 -5/11) ;; not-equal evel 1/2
  -4 2/11)

(deftest round-fr.pm.6
  (round 3 -5/9)  ;; not-equal odd 1/2
  -5 2/9)

(deftest round-fr.mm.1
  (round -3 -4/7)  ;; less 1/2
  5 -1/7)

(deftest round-fr.mm.2
  (round -5 -4/7)  ;; greater 1/2
  9 1/7)

(deftest round-fr.mm.3
  (round -6 -4/7)  ;; equal even 1/2
  10 -2/7)

(deftest round-fr.mm.4
  (round -2 -4/7)  ;; equal odd 1/2
  4 2/7)

(deftest round-fr.mm.5
  (round -2 -5/11) ;; not-equal evel 1/2
  4 -2/11)

(deftest round-fr.mm.6
  (round -3 -5/9)  ;; not-equal odd 1/2
  5 -2/9)

;;  fixnum - single-float
(defun round-check-fs (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-fs.1
  (round 0 10.3f0)
  0 0.0f0)

(deftest round-fs.2
  (round 0 -10.3f0)
  0 0.0f0)

(deftest-error round-fs.3
  (round 10 0.0f0)
  division-by-zero)

(deftest round-fs.4a
  (round 5 0.5f0)
  10 0.0f0)

(deftest round-fs.4b
  (round -5 0.5f0)
  -10 0.0f0)

(deftest round-fs.4c
  (round 5 -0.5f0)
  -10 0.0f0)

(deftest round-fs.4d
  (round -5 -0.5f0)
  10 0.0f0)

(deftest round-fs.pp.1
  (round-check-fs 2 1.5f0    1 0.5f0)  ;; less 1/2
  t)

(deftest round-fs.pp.2
  (round-check-fs 4 1.5f0    3 -0.5f0)  ;; greater 1/2
  t)

(deftest round-fs.pp.3
  (round-check-fs 5 2.0f0    2 1.0f0)  ;; equal even 1/2
  t)

(deftest round-fs.pp.4
  (round-check-fs 7 2.0f0    4 -1.0f0)  ;; equal odd 1/2
  t)

(deftest round-fs.mp.1
  (round-check-fs -2 1.5f0    -1 -0.5f0)  ;; less 1/2
  t)

(deftest round-fs.mp.2
  (round-check-fs -4 1.5f0    -3 0.5f0)  ;; greater 1/2
  t)

(deftest round-fs.mp.3
  (round-check-fs -5 2.0f0    -2 -1.0f0)  ;; equal even 1/2
  t)

(deftest round-fs.mp.4
  (round-check-fs -7 2.0f0    -4 1.0f0)  ;; equal odd 1/2
  t)

(deftest round-fs.pm.1
  (round-check-fs 2 -1.5f0    -1 0.5f0)  ;; less 1/2
  t)

(deftest round-fs.pm.2
  (round-check-fs 4 -1.5f0    -3 -0.5f0)  ;; greater 1/2
  t)

(deftest round-fs.pm.3
  (round-check-fs 5 -2.0f0    -2 1.0f0)  ;; equal even 1/2
  t)

(deftest round-fs.pm.4
  (round-check-fs 7 -2.0f0    -4 -1.0f0)  ;; equal odd 1/2
  t)

(deftest round-fs.mm.1
  (round-check-fs -2 -1.5f0    1 -0.5f0)  ;; less 1/2
  t)

(deftest round-fs.mm.2
  (round-check-fs -4 -1.5f0    3 0.5f0)  ;; greater 1/2
  t)

(deftest round-fs.mm.3
  (round-check-fs -5 -2.0f0    2 -1.0f0)  ;; equal even 1/2
  t)

(deftest round-fs.mm.4
  (round-check-fs -7 -2.0f0    4 1.0f0)  ;; equal odd 1/2
  t)

;;  fixnum - double-float
(defun round-check-fd (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest round-fd.1
  (round 0 10.3d0)
  0 0.0d0)

(deftest round-fd.2
  (round 0 -10.3d0)
  0 0.0d0)

(deftest-error round-fd.3
  (round 10 0.0d0)
  division-by-zero)

(deftest round-fd.4a
  (round 5 0.5d0)
  10 0.0d0)

(deftest round-fd.4b
  (round -5 0.5d0)
  -10 0.0d0)

(deftest round-fd.4c
  (round 5 -0.5d0)
  -10 0.0d0)

(deftest round-fd.4d
  (round -5 -0.5d0)
  10 0.0d0)

(deftest round-fd.pp.1
  (round-check-fd 2 1.5d0    1 0.5d0)  ;; less 1/2
  t)

(deftest round-fd.pp.2
  (round-check-fd 4 1.5d0    3 -0.5d0)  ;; greater 1/2
  t)

(deftest round-fd.pp.3
  (round-check-fd 5 2.0d0    2 1.0d0)  ;; equal even 1/2
  t)

(deftest round-fd.pp.4
  (round-check-fd 7 2.0d0    4 -1.0d0)  ;; equal odd 1/2
  t)

(deftest round-fd.mp.1
  (round-check-fd -2 1.5d0    -1 -0.5d0)  ;; less 1/2
  t)

(deftest round-fd.mp.2
  (round-check-fd -4 1.5d0    -3 0.5d0)  ;; greater 1/2
  t)

(deftest round-fd.mp.3
  (round-check-fd -5 2.0d0    -2 -1.0d0)  ;; equal even 1/2
  t)

(deftest round-fd.mp.4
  (round-check-fd -7 2.0d0    -4 1.0d0)  ;; equal odd 1/2
  t)

(deftest round-fd.pm.1
  (round-check-fd 2 -1.5d0    -1 0.5d0)  ;; less 1/2
  t)

(deftest round-fd.pm.2
  (round-check-fd 4 -1.5d0    -3 -0.5d0)  ;; greater 1/2
  t)

(deftest round-fd.pm.3
  (round-check-fd 5 -2.0d0    -2 1.0d0)  ;; equal even 1/2
  t)

(deftest round-fd.pm.4
  (round-check-fd 7 -2.0d0    -4 -1.0d0)  ;; equal odd 1/2
  t)

(deftest round-fd.mm.1
  (round-check-fd -2 -1.5d0    1 -0.5d0)  ;; less 1/2
  t)

(deftest round-fd.mm.2
  (round-check-fd -4 -1.5d0    3 0.5d0)  ;; greater 1/2
  t)

(deftest round-fd.mm.3
  (round-check-fd -5 -2.0d0    2 -1.0d0)  ;; equal even 1/2
  t)

(deftest round-fd.mm.4
  (round-check-fd -7 -2.0d0    4 1.0d0)  ;; equal odd 1/2
  t)

;;  fixnum - long-float
(defun round-check-fl (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest round-fl.1
  (round 0 10.3l0)
  0 0.0l0)

(deftest round-fl.2
  (round 0 -10.3l0)
  0 0.0l0)

(deftest-error round-fl.3
  (round 10 0.0l0)
  division-by-zero)

(deftest round-fl.4a
  (round 5 0.5l0)
  10 0.0l0)

(deftest round-fl.4b
  (round -5 0.5l0)
  -10 0.0l0)

(deftest round-fl.4c
  (round 5 -0.5l0)
  -10 0.0l0)

(deftest round-fl.4d
  (round -5 -0.5l0)
  10 0.0l0)

(deftest round-fl.pp.1
  (round-check-fl 2 1.5l0    1 0.5l0)  ;; less 1/2
  t)

(deftest round-fl.pp.2
  (round-check-fl 4 1.5l0    3 -0.5l0)  ;; greater 1/2
  t)

(deftest round-fl.pp.3
  (round-check-fl 5 2.0l0    2 1.0l0)  ;; equal even 1/2
  t)

(deftest round-fl.pp.4
  (round-check-fl 7 2.0l0    4 -1.0l0)  ;; equal odd 1/2
  t)

(deftest round-fl.mp.1
  (round-check-fl -2 1.5l0    -1 -0.5l0)  ;; less 1/2
  t)

(deftest round-fl.mp.2
  (round-check-fl -4 1.5l0    -3 0.5l0)  ;; greater 1/2
  t)

(deftest round-fl.mp.3
  (round-check-fl -5 2.0l0    -2 -1.0l0)  ;; equal even 1/2
  t)

(deftest round-fl.mp.4
  (round-check-fl -7 2.0l0    -4 1.0l0)  ;; equal odd 1/2
  t)

(deftest round-fl.pm.1
  (round-check-fl 2 -1.5l0    -1 0.5l0)  ;; less 1/2
  t)

(deftest round-fl.pm.2
  (round-check-fl 4 -1.5l0    -3 -0.5l0)  ;; greater 1/2
  t)

(deftest round-fl.pm.3
  (round-check-fl 5 -2.0l0    -2 1.0l0)  ;; equal even 1/2
  t)

(deftest round-fl.pm.4
  (round-check-fl 7 -2.0l0    -4 -1.0l0)  ;; equal odd 1/2
  t)

(deftest round-fl.mm.1
  (round-check-fl -2 -1.5l0    1 -0.5l0)  ;; less 1/2
  t)

(deftest round-fl.mm.2
  (round-check-fl -4 -1.5l0    3 0.5l0)  ;; greater 1/2
  t)

(deftest round-fl.mm.3
  (round-check-fl -5 -2.0l0    2 -1.0l0)  ;; equal even 1/2
  t)

(deftest round-fl.mm.4
  (round-check-fl -7 -2.0l0    4 1.0l0)  ;; equal odd 1/2
  t)

