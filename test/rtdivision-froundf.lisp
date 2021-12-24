;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  fround
;;

;;  fixnum - fixnum
(deftest fround-ff.1
  (fround 0 10)
  0.0 0)

(deftest fround-ff.2
  (fround 0 -10)
  0.0 0)

(deftest-error fround-ff.3
  (fround 10 0)
  division-by-zero)

(deftest fround-ff.4a
  (fround 9 3)
  3.0 0)

(deftest fround-ff.4b
  (fround -9 3)
  -3.0 0)

(deftest fround-ff.4c
  (fround 9 -3)
  -3.0 0)

(deftest fround-ff.4d
  (fround -9 -3)
  3.0 0)

(deftest fround-ff.pp.1
  (fround 2 10)
  0.0 2)

(deftest fround-ff.pp.2
  (fround 37 7)
  5.0 2)

(deftest fround-ff.pp.3
  (fround 38 7)
  5.0 3)

(deftest fround-ff.pp.4
  (fround 39 7)
  6.0 -3)

(deftest fround-ff.pp.5
  (fround 40 7)
  6.0 -2)

(deftest fround-ff.pp.6
  (fround 44 8)
  6.0 -4)

(deftest fround-ff.pp.7
  (fround 52 8)
  6.0 4)

(deftest fround-ff.pm.1
  (fround 2 -10)
  0.0 2)

(deftest fround-ff.pm.2
  (fround 37 -7)
  -5.0 2)

(deftest fround-ff.pm.3
  (fround 38 -7)
  -5.0 3)

(deftest fround-ff.pm.4
  (fround 39 -7)
  -6.0 -3)

(deftest fround-ff.pm.5
  (fround 40 -7)
  -6.0 -2)

(deftest fround-ff.pm.6
  (fround 44 -8)
  -6.0 -4)

(deftest fround-ff.pm.7
  (fround 52 -8)
  -6.0 4)

(deftest fround-ff.mp.1
  (fround -2 10)
  0.0 -2)

(deftest fround-ff.mp.2
  (fround -37 7)
  -5.0 -2)

(deftest fround-ff.mp.3
  (fround -38 7)
  -5.0 -3)

(deftest fround-ff.mp.4
  (fround -39 7)
  -6.0 3)

(deftest fround-ff.mp.5
  (fround -40 7)
  -6.0 2)

(deftest fround-ff.mp.6
  (fround -44 8)
  -6.0 4)

(deftest fround-ff.mp.7
  (fround -52 8)
  -6.0 -4)

(deftest fround-ff.mm.1
  (fround -2 -10)
  0.0 -2)

(deftest fround-ff.mm.2
  (fround -37 -7)
  5.0 -2)

(deftest fround-ff.mm.3
  (fround -38 -7)
  5.0 -3)

(deftest fround-ff.mm.4
  (fround -39 -7)
  6.0 3)

(deftest fround-ff.mm.5
  (fround -40 -7)
  6.0 2)

(deftest fround-ff.mm.6
  (fround -44 -8)
  6.0 4)

(deftest fround-ff.mm.7
  (fround -52 -8)
  6.0 -4)

;;  fixnum - bignum
(defun froundfb (a b)
  (fround a (make-bignum b)))

(deftest fround-fb.1
  (froundfb 0 10)
  0.0 0)

(deftest fround-fb.2
  (froundfb 0 -10)
  0.0 0)

(deftest-error fround-fb.3
  (froundfb 10 0)
  division-by-zero)

(deftest fround-fb.4a
  (froundfb 9 3)
  3.0 0)

(deftest fround-fb.4b
  (froundfb -9 3)
  -3.0 0)

(deftest fround-fb.4c
  (froundfb 9 -3)
  -3.0 0)

(deftest fround-fb.4d
  (froundfb -9 -3)
  3.0 0)

(deftest fround-fb.pp.1
  (froundfb 2 10)
  0.0 2)

(deftest fround-fb.pp.2
  (froundfb 37 7)
  5.0 2)

(deftest fround-fb.pp.3
  (froundfb 38 7)
  5.0 3)

(deftest fround-fb.pp.4
  (froundfb 39 7)
  6.0 -3)

(deftest fround-fb.pp.5
  (froundfb 40 7)
  6.0 -2)

(deftest fround-fb.pp.6
  (froundfb 44 8)
  6.0 -4)

(deftest fround-fb.pp.7
  (froundfb 52 8)
  6.0 4)

(deftest fround-fb.pm.1
  (froundfb 2 -10)
  -0.0 2)

(deftest fround-fb.pm.2
  (froundfb 37 -7)
  -5.0 2)

(deftest fround-fb.pm.3
  (froundfb 38 -7)
  -5.0 3)

(deftest fround-fb.pm.4
  (froundfb 39 -7)
  -6.0 -3)

(deftest fround-fb.pm.5
  (froundfb 40 -7)
  -6.0 -2)

(deftest fround-fb.pm.6
  (froundfb 44 -8)
  -6.0 -4)

(deftest fround-fb.pm.7
  (froundfb 52 -8)
  -6.0 4)

(deftest fround-fb.mp.1
  (froundfb -2 10)
  -0.0 -2)

(deftest fround-fb.mp.2
  (froundfb -37 7)
  -5.0 -2)

(deftest fround-fb.mp.3
  (froundfb -38 7)
  -5.0 -3)

(deftest fround-fb.mp.4
  (froundfb -39 7)
  -6.0 3)

(deftest fround-fb.mp.5
  (froundfb -40 7)
  -6.0 2)

(deftest fround-fb.mp.6
  (froundfb -44 8)
  -6.0 4)

(deftest fround-fb.mp.7
  (froundfb -52 8)
  -6.0 -4)

(deftest fround-fb.mm.1
  (froundfb -2 -10)
  0.0 -2)

(deftest fround-fb.mm.2
  (froundfb -37 -7)
  5.0 -2)

(deftest fround-fb.mm.3
  (froundfb -38 -7)
  5.0 -3)

(deftest fround-fb.mm.4
  (froundfb -39 -7)
  6.0 3)

(deftest fround-fb.mm.5
  (froundfb -40 -7)
  6.0 2)

(deftest fround-fb.mm.6
  (froundfb -44 -8)
  6.0 4)

(deftest fround-fb.mm.7
  (froundfb -52 -8)
  6.0 -4)

;;  fixnum - ratio
(deftest fround-fr.1
  (fround 0 10/3)
  0.0 0)

(deftest fround-fr.2
  (fround 0 -10/3)
  0.0 0)

(deftest-error fround-fr.3
  (fround 10 (make-ratio 0 1))
  division-by-zero)

(deftest fround-fr.4a
  (fround 5 1/2)
  10.0 0)

(deftest fround-fr.4b
  (fround -5 1/2)
  -10.0 0)

(deftest fround-fr.4c
  (fround 5 -1/2)
  -10.0 0)

(deftest fround-fr.4d
  (fround -5 -1/2)
  10.0 0)

(deftest fround-fr.pp.1
  (fround 3 4/7)  ;; less 1/2
  5.0 1/7)

(deftest fround-fr.pp.2
  (fround 5 4/7)  ;; greater 1/2
  9.0 -1/7)

(deftest fround-fr.pp.3
  (fround 6 4/7)  ;; equal even 1/2
  10.0 2/7)

(deftest fround-fr.pp.4
  (fround 2 4/7)  ;; equal odd 1/2
  4.0 -2/7)

(deftest fround-fr.pp.5
  (fround 2 5/11) ;; not-equal evel 1/2
  4.0 2/11)

(deftest fround-fr.pp.6
  (fround 3 5/9)  ;; not-equal odd 1/2
  5.0 2/9)

(deftest fround-fr.mp.1
  (fround -3 4/7)  ;; less 1/2
  -5.0 -1/7)

(deftest fround-fr.mp.2
  (fround -5 4/7)  ;; greater 1/2
  -9.0 1/7)

(deftest fround-fr.mp.3
  (fround -6 4/7)  ;; equal even 1/2
  -10.0 -2/7)

(deftest fround-fr.mp.4
  (fround -2 4/7)  ;; equal odd 1/2
  -4.0 2/7)

(deftest fround-fr.mp.5
  (fround -2 5/11) ;; not-equal evel 1/2
  -4.0 -2/11)

(deftest fround-fr.mp.6
  (fround -3 5/9)  ;; not-equal odd 1/2
  -5.0 -2/9)

(deftest fround-fr.pm.1
  (fround 3 -4/7)  ;; less 1/2
  -5.0 1/7)

(deftest fround-fr.pm.2
  (fround 5 -4/7)  ;; greater 1/2
  -9.0 -1/7)

(deftest fround-fr.pm.3
  (fround 6 -4/7)  ;; equal even 1/2
  -10.0 2/7)

(deftest fround-fr.pm.4
  (fround 2 -4/7)  ;; equal odd 1/2
  -4.0 -2/7)

(deftest fround-fr.pm.5
  (fround 2 -5/11) ;; not-equal evel 1/2
  -4.0 2/11)

(deftest fround-fr.pm.6
  (fround 3 -5/9)  ;; not-equal odd 1/2
  -5.0 2/9)

(deftest fround-fr.mm.1
  (fround -3 -4/7)  ;; less 1/2
  5.0 -1/7)

(deftest fround-fr.mm.2
  (fround -5 -4/7)  ;; greater 1/2
  9.0 1/7)

(deftest fround-fr.mm.3
  (fround -6 -4/7)  ;; equal even 1/2
  10.0 -2/7)

(deftest fround-fr.mm.4
  (fround -2 -4/7)  ;; equal odd 1/2
  4.0 2/7)

(deftest fround-fr.mm.5
  (fround -2 -5/11) ;; not-equal evel 1/2
  4.0 -2/11)

(deftest fround-fr.mm.6
  (fround -3 -5/9)  ;; not-equal odd 1/2
  5.0 -2/9)

;;  fixnum - single-float
(defun fround-check-fs (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-fs.1
  (fround 0 10.3f0)
  0.0f0 0.0f0)

(deftest fround-fs.2
  (fround 0 -10.3f0)
  0.0f0 0.0f0)

(deftest-error fround-fs.3
  (fround 10 0.0f0)
  division-by-zero)

(deftest fround-fs.4a
  (fround 5 0.5f0)
  10.0f0 0.0f0)

(deftest fround-fs.4b
  (fround -5 0.5f0)
  -10.0f0 0.0f0)

(deftest fround-fs.4c
  (fround 5 -0.5f0)
  -10.0f0 0.0f0)

(deftest fround-fs.4d
  (fround -5 -0.5f0)
  10.0f0 0.0f0)

(deftest fround-fs.pp.1
  (fround-check-fs 2 1.5f0    1.0f0 0.5f0)  ;; less 1/2
  t)

(deftest fround-fs.pp.2
  (fround-check-fs 4 1.5f0    3.0f0 -0.5f0)  ;; greater 1/2
  t)

(deftest fround-fs.pp.3
  (fround-check-fs 5 2.0f0    2.0f0 1.0f0)  ;; equal even 1/2
  t)

(deftest fround-fs.pp.4
  (fround-check-fs 7 2.0f0    4.0f0 -1.0f0)  ;; equal odd 1/2
  t)

(deftest fround-fs.mp.1
  (fround-check-fs -2 1.5f0    -1.0f0 -0.5f0)  ;; less 1/2
  t)

(deftest fround-fs.mp.2
  (fround-check-fs -4 1.5f0    -3.0f0 0.5f0)  ;; greater 1/2
  t)

(deftest fround-fs.mp.3
  (fround-check-fs -5 2.0f0    -2.0f0 -1.0f0)  ;; equal even 1/2
  t)

(deftest fround-fs.mp.4
  (fround-check-fs -7 2.0f0    -4.0f0 1.0f0)  ;; equal odd 1/2
  t)

(deftest fround-fs.pm.1
  (fround-check-fs 2 -1.5f0    -1.0f0 0.5f0)  ;; less 1/2
  t)

(deftest fround-fs.pm.2
  (fround-check-fs 4 -1.5f0    -3.0f0 -0.5f0)  ;; greater 1/2
  t)

(deftest fround-fs.pm.3
  (fround-check-fs 5 -2.0f0    -2.0f0 1.0f0)  ;; equal even 1/2
  t)

(deftest fround-fs.pm.4
  (fround-check-fs 7 -2.0f0    -4.0f0 -1.0f0)  ;; equal odd 1/2
  t)

(deftest fround-fs.mm.1
  (fround-check-fs -2 -1.5f0    1.0f0 -0.5f0)  ;; less 1/2
  t)

(deftest fround-fs.mm.2
  (fround-check-fs -4 -1.5f0    3.0f0 0.5f0)  ;; greater 1/2
  t)

(deftest fround-fs.mm.3
  (fround-check-fs -5 -2.0f0    2.0f0 -1.0f0)  ;; equal even 1/2
  t)

(deftest fround-fs.mm.4
  (fround-check-fs -7 -2.0f0    4.0f0 1.0f0)  ;; equal odd 1/2
  t)

;;  fixnum - double-float
(defun fround-check-fd (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fround-fd.1
  (fround 0 10.3d0)
  0.0d0 0.0d0)

(deftest fround-fd.2
  (fround 0 -10.3d0)
  0.0d0 0.0d0)

(deftest-error fround-fd.3
  (fround 10 0.0d0)
  division-by-zero)

(deftest fround-fd.4a
  (fround 5 0.5d0)
  10.0d0 0.0d0)

(deftest fround-fd.4b
  (fround -5 0.5d0)
  -10.0d0 0.0d0)

(deftest fround-fd.4c
  (fround 5 -0.5d0)
  -10.0d0 0.0d0)

(deftest fround-fd.4d
  (fround -5 -0.5d0)
  10.0d0 0.0d0)

(deftest fround-fd.pp.1
  (fround-check-fd 2 1.5d0    1.0d0 0.5d0)  ;; less 1/2
  t)

(deftest fround-fd.pp.2
  (fround-check-fd 4 1.5d0    3.0d0 -0.5d0)  ;; greater 1/2
  t)

(deftest fround-fd.pp.3
  (fround-check-fd 5 2.0d0    2.0d0 1.0d0)  ;; equal even 1/2
  t)

(deftest fround-fd.pp.4
  (fround-check-fd 7 2.0d0    4.0d0 -1.0d0)  ;; equal odd 1/2
  t)

(deftest fround-fd.mp.1
  (fround-check-fd -2 1.5d0    -1.0d0 -0.5d0)  ;; less 1/2
  t)

(deftest fround-fd.mp.2
  (fround-check-fd -4 1.5d0    -3.0d0 0.5d0)  ;; greater 1/2
  t)

(deftest fround-fd.mp.3
  (fround-check-fd -5 2.0d0    -2.0d0 -1.0d0)  ;; equal even 1/2
  t)

(deftest fround-fd.mp.4
  (fround-check-fd -7 2.0d0    -4.0d0 1.0d0)  ;; equal odd 1/2
  t)

(deftest fround-fd.pm.1
  (fround-check-fd 2 -1.5d0    -1.0d0 0.5d0)  ;; less 1/2
  t)

(deftest fround-fd.pm.2
  (fround-check-fd 4 -1.5d0    -3.0d0 -0.5d0)  ;; greater 1/2
  t)

(deftest fround-fd.pm.3
  (fround-check-fd 5 -2.0d0    -2.0d0 1.0d0)  ;; equal even 1/2
  t)

(deftest fround-fd.pm.4
  (fround-check-fd 7 -2.0d0    -4.0d0 -1.0d0)  ;; equal odd 1/2
  t)

(deftest fround-fd.mm.1
  (fround-check-fd -2 -1.5d0    1.0d0 -0.5d0)  ;; less 1/2
  t)

(deftest fround-fd.mm.2
  (fround-check-fd -4 -1.5d0    3.0d0 0.5d0)  ;; greater 1/2
  t)

(deftest fround-fd.mm.3
  (fround-check-fd -5 -2.0d0    2.0d0 -1.0d0)  ;; equal even 1/2
  t)

(deftest fround-fd.mm.4
  (fround-check-fd -7 -2.0d0    4.0d0 1.0d0)  ;; equal odd 1/2
  t)

;;  fixnum - long-float
(defun fround-check-fl (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest fround-fl.1
  (fround 0 10.3l0)
  0.0l0 0.0l0)

(deftest fround-fl.2
  (fround 0 -10.3l0)
  0.0l0 0.0l0)

(deftest-error fround-fl.3
  (fround 10 0.0l0)
  division-by-zero)

(deftest fround-fl.4a
  (fround 5 0.5l0)
  10.0l0 0.0l0)

(deftest fround-fl.4b
  (fround -5 0.5l0)
  -10.0l0 0.0l0)

(deftest fround-fl.4c
  (fround 5 -0.5l0)
  -10.0l0 0.0l0)

(deftest fround-fl.4d
  (fround -5 -0.5l0)
  10.0l0 0.0l0)

(deftest fround-fl.pp.1
  (fround-check-fl 2 1.5l0    1.0l0 0.5l0)  ;; less 1/2
  t)

(deftest fround-fl.pp.2
  (fround-check-fl 4 1.5l0    3.0l0 -0.5l0)  ;; greater 1/2
  t)

(deftest fround-fl.pp.3
  (fround-check-fl 5 2.0l0    2.0l0 1.0l0)  ;; equal even 1/2
  t)

(deftest fround-fl.pp.4
  (fround-check-fl 7 2.0l0    4.0l0 -1.0l0)  ;; equal odd 1/2
  t)

(deftest fround-fl.mp.1
  (fround-check-fl -2 1.5l0    -1.0l0 -0.5l0)  ;; less 1/2
  t)

(deftest fround-fl.mp.2
  (fround-check-fl -4 1.5l0    -3.0l0 0.5l0)  ;; greater 1/2
  t)

(deftest fround-fl.mp.3
  (fround-check-fl -5 2.0l0    -2.0l0 -1.0l0)  ;; equal even 1/2
  t)

(deftest fround-fl.mp.4
  (fround-check-fl -7 2.0l0    -4.0l0 1.0l0)  ;; equal odd 1/2
  t)

(deftest fround-fl.pm.1
  (fround-check-fl 2 -1.5l0    -1.0l0 0.5l0)  ;; less 1/2
  t)

(deftest fround-fl.pm.2
  (fround-check-fl 4 -1.5l0    -3.0l0 -0.5l0)  ;; greater 1/2
  t)

(deftest fround-fl.pm.3
  (fround-check-fl 5 -2.0l0    -2.0l0 1.0l0)  ;; equal even 1/2
  t)

(deftest fround-fl.pm.4
  (fround-check-fl 7 -2.0l0    -4.0l0 -1.0l0)  ;; equal odd 1/2
  t)

(deftest fround-fl.mm.1
  (fround-check-fl -2 -1.5l0    1.0l0 -0.5l0)  ;; less 1/2
  t)

(deftest fround-fl.mm.2
  (fround-check-fl -4 -1.5l0    3.0l0 0.5l0)  ;; greater 1/2
  t)

(deftest fround-fl.mm.3
  (fround-check-fl -5 -2.0l0    2.0l0 -1.0l0)  ;; equal even 1/2
  t)

(deftest fround-fl.mm.4
  (fround-check-fl -7 -2.0l0    4.0l0 1.0l0)  ;; equal odd 1/2
  t)

