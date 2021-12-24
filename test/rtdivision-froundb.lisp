;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  fround
;;

;;  bignum - fixnum
(deftest fround-bf.1
  (froundb 0 10)
  0.0 0)

(deftest fround-bf.2
  (froundb 0 -10)
  0.0 0)

(deftest-error fround-bf.3
  (froundb 10 0)
  division-by-zero)

(deftest fround-bf.4a
  (froundb 9 3)
  3.0 0)

(deftest fround-bf.4b
  (froundb -9 3)
  -3.0 0)

(deftest fround-bf.4c
  (froundb 9 -3)
  -3.0 0)

(deftest fround-bf.4d
  (froundb -9 -3)
  3.0 0)

(deftest fround-bf.pp.1
  (froundb 2 10)
  0.0 2)

(deftest fround-bf.pp.2
  (froundb 37 7)
  5.0 2)

(deftest fround-bf.pp.3
  (froundb 38 7)
  5.0 3)

(deftest fround-bf.pp.4
  (froundb 39 7)
  6.0 -3)

(deftest fround-bf.pp.5
  (froundb 40 7)
  6.0 -2)

(deftest fround-bf.pp.6
  (froundb 44 8)
  6.0 -4)

(deftest fround-bf.pp.7
  (froundb 52 8)
  6.0 4)

(deftest fround-bf.pm.1
  (froundb 2 -10)
  -0.0 2)

(deftest fround-bf.pm.2
  (froundb 37 -7)
  -5.0 2)

(deftest fround-bf.pm.3
  (froundb 38 -7)
  -5.0 3)

(deftest fround-bf.pm.4
  (froundb 39 -7)
  -6.0 -3)

(deftest fround-bf.pm.5
  (froundb 40 -7)
  -6.0 -2)

(deftest fround-bf.pm.6
  (froundb 44 -8)
  -6.0 -4)

(deftest fround-bf.pm.7
  (froundb 52 -8)
  -6.0 4)

(deftest fround-bf.mp.1
  (froundb -2 10)
  -0.0 -2)

(deftest fround-bf.mp.2
  (froundb -37 7)
  -5.0 -2)

(deftest fround-bf.mp.3
  (froundb -38 7)
  -5.0 -3)

(deftest fround-bf.mp.4
  (froundb -39 7)
  -6.0 3)

(deftest fround-bf.mp.5
  (froundb -40 7)
  -6.0 2)

(deftest fround-bf.mp.6
  (froundb -44 8)
  -6.0 4)

(deftest fround-bf.mp.7
  (froundb -52 8)
  -6.0 -4)

(deftest fround-bf.mm.1
  (froundb -2 -10)
  0.0 -2)

(deftest fround-bf.mm.2
  (froundb -37 -7)
  5.0 -2)

(deftest fround-bf.mm.3
  (froundb -38 -7)
  5.0 -3)

(deftest fround-bf.mm.4
  (froundb -39 -7)
  6.0 3)

(deftest fround-bf.mm.5
  (froundb -40 -7)
  6.0 2)

(deftest fround-bf.mm.6
  (froundb -44 -8)
  6.0 4)

(deftest fround-bf.mm.7
  (froundb -52 -8)
  6.0 -4)

;;  bignum - bignum
(defun froundbb (a b)
  (fround (make-bignum a) (make-bignum b)))

(deftest fround-bb.1
  (froundbb 0 10)
  0.0 0)

(deftest fround-bb.2
  (froundbb 0 -10)
  0.0 0)

(deftest-error fround-bb.3
  (froundbb 10 0)
  division-by-zero)

(deftest fround-bb.4a
  (froundbb 9 3)
  3.0 0)

(deftest fround-bb.4b
  (froundbb -9 3)
  -3.0 0)

(deftest fround-bb.4c
  (froundbb 9 -3)
  -3.0 0)

(deftest fround-bb.4d
  (froundbb -9 -3)
  3.0 0)

(deftest fround-bb.pp.1
  (froundbb 2 10)
  0.0 2)

(deftest fround-bb.pp.2
  (froundbb 37 7)
  5.0 2)

(deftest fround-bb.pp.3
  (froundbb 38 7)
  5.0 3)

(deftest fround-bb.pp.4
  (froundbb 39 7)
  6.0 -3)

(deftest fround-bb.pp.5
  (froundbb 40 7)
  6.0 -2)

(deftest fround-bb.pp.6
  (froundbb 44 8)
  6.0 -4)

(deftest fround-bb.pp.7
  (froundbb 52 8)
  6.0 4)

(deftest fround-bb.pm.1
  (froundbb 2 -10)
  -0.0 2)

(deftest fround-bb.pm.2
  (froundbb 37 -7)
  -5.0 2)

(deftest fround-bb.pm.3
  (froundbb 38 -7)
  -5.0 3)

(deftest fround-bb.pm.4
  (froundbb 39 -7)
  -6.0 -3)

(deftest fround-bb.pm.5
  (froundbb 40 -7)
  -6.0 -2)

(deftest fround-bb.pm.6
  (froundbb 44 -8)
  -6.0 -4)

(deftest fround-bb.pm.7
  (froundbb 52 -8)
  -6.0 4)

(deftest fround-bb.mp.1
  (froundbb -2 10)
  -0.0 -2)

(deftest fround-bb.mp.2
  (froundbb -37 7)
  -5.0 -2)

(deftest fround-bb.mp.3
  (froundbb -38 7)
  -5.0 -3)

(deftest fround-bb.mp.4
  (froundbb -39 7)
  -6.0 3)

(deftest fround-bb.mp.5
  (froundbb -40 7)
  -6.0 2)

(deftest fround-bb.mp.6
  (froundbb -44 8)
  -6.0 4)

(deftest fround-bb.mp.7
  (froundbb -52 8)
  -6.0 -4)

(deftest fround-bb.mm.1
  (froundbb -2 -10)
  0.0 -2)

(deftest fround-bb.mm.2
  (froundbb -37 -7)
  5.0 -2)

(deftest fround-bb.mm.3
  (froundbb -38 -7)
  5.0 -3)

(deftest fround-bb.mm.4
  (froundbb -39 -7)
  6.0 3)

(deftest fround-bb.mm.5
  (froundbb -40 -7)
  6.0 2)

(deftest fround-bb.mm.6
  (froundbb -44 -8)
  6.0 4)

(deftest fround-bb.mm.7
  (froundbb -52 -8)
  6.0 -4)

;;  bignum - ratio
(deftest fround-br.1
  (froundb 0 10/3)
  0.0 0)

(deftest fround-br.2
  (froundb 0 -10/3)
  0.0 0)

(deftest-error fround-br.3
  (froundb 10 (make-ratio 0 1))
  division-by-zero)

(deftest fround-br.4a
  (froundb 5 1/2)
  10.0 0)

(deftest fround-br.4b
  (froundb -5 1/2)
  -10.0 0)

(deftest fround-br.4c
  (froundb 5 -1/2)
  -10.0 0)

(deftest fround-br.4d
  (froundb -5 -1/2)
  10.0 0)

(deftest fround-br.pp.1
  (froundb 3 4/7)  ;; less 1/2
  5.0 1/7)

(deftest fround-br.pp.2
  (froundb 5 4/7)  ;; greater 1/2
  9.0 -1/7)

(deftest fround-br.pp.3
  (froundb 6 4/7)  ;; equal even 1/2
  10.0 2/7)

(deftest fround-br.pp.4
  (froundb 2 4/7)  ;; equal odd 1/2
  4.0 -2/7)

(deftest fround-br.pp.5
  (froundb 2 5/11) ;; not-equal evel 1/2
  4.0 2/11)

(deftest fround-br.pp.6
  (froundb 3 5/9)  ;; not-equal odd 1/2
  5.0 2/9)

(deftest fround-br.mp.1
  (froundb -3 4/7)  ;; less 1/2
  -5.0 -1/7)

(deftest fround-br.mp.2
  (froundb -5 4/7)  ;; greater 1/2
  -9.0 1/7)

(deftest fround-br.mp.3
  (froundb -6 4/7)  ;; equal even 1/2
  -10.0 -2/7)

(deftest fround-br.mp.4
  (froundb -2 4/7)  ;; equal odd 1/2
  -4.0 2/7)

(deftest fround-br.mp.5
  (froundb -2 5/11) ;; not-equal evel 1/2
  -4.0 -2/11)

(deftest fround-br.mp.6
  (froundb -3 5/9)  ;; not-equal odd 1/2
  -5.0 -2/9)

(deftest fround-br.pm.1
  (froundb 3 -4/7)  ;; less 1/2
  -5.0 1/7)

(deftest fround-br.pm.2
  (froundb 5 -4/7)  ;; greater 1/2
  -9.0 -1/7)

(deftest fround-br.pm.3
  (froundb 6 -4/7)  ;; equal even 1/2
  -10.0 2/7)

(deftest fround-br.pm.4
  (froundb 2 -4/7)  ;; equal odd 1/2
  -4.0 -2/7)

(deftest fround-br.pm.5
  (froundb 2 -5/11) ;; not-equal evel 1/2
  -4.0 2/11)

(deftest fround-br.pm.6
  (froundb 3 -5/9)  ;; not-equal odd 1/2
  -5.0 2/9)

(deftest fround-br.mm.1
  (froundb -3 -4/7)  ;; less 1/2
  5.0 -1/7)

(deftest fround-br.mm.2
  (froundb -5 -4/7)  ;; greater 1/2
  9.0 1/7)

(deftest fround-br.mm.3
  (froundb -6 -4/7)  ;; equal even 1/2
  10.0 -2/7)

(deftest fround-br.mm.4
  (froundb -2 -4/7)  ;; equal odd 1/2
  4.0 2/7)

(deftest fround-br.mm.5
  (froundb -2 -5/11) ;; not-equal evel 1/2
  4.0 -2/11)

(deftest fround-br.mm.6
  (froundb -3 -5/9)  ;; not-equal odd 1/2
  5.0 -2/9)

;;  bignum - single-float
(defun fround-check-bs (a b c d)
  (froundb-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-bs.1
  (froundb 0 10.3f0)
  0.0f0 0.0f0)

(deftest fround-bs.2
  (froundb 0 -10.3f0)
  0.0f0 0.0f0)

(deftest-error fround-bs.3
  (froundb 10 0.0f0)
  division-by-zero)

(deftest fround-bs.4a
  (froundb 5 0.5f0)
  10.0f0 0.0f0)

(deftest fround-bs.4b
  (froundb -5 0.5f0)
  -10.0f0 0.0f0)

(deftest fround-bs.4c
  (froundb 5 -0.5f0)
  -10.0f0 0.0f0)

(deftest fround-bs.4d
  (froundb -5 -0.5f0)
  10.0f0 0.0f0)

(deftest fround-bs.pp.1
  (fround-check-bs 2 1.5f0    1.0f0 0.5f0)  ;; less 1/2
  t)

(deftest fround-bs.pp.2
  (fround-check-bs 4 1.5f0    3.0f0 -0.5f0)  ;; greater 1/2
  t)

(deftest fround-bs.pp.3
  (fround-check-bs 5 2.0f0    2.0f0 1.0f0)  ;; equal even 1/2
  t)

(deftest fround-bs.pp.4
  (fround-check-bs 7 2.0f0    4.0f0 -1.0f0)  ;; equal odd 1/2
  t)

(deftest fround-bs.mp.1
  (fround-check-bs -2 1.5f0    -1.0f0 -0.5f0)  ;; less 1/2
  t)

(deftest fround-bs.mp.2
  (fround-check-bs -4 1.5f0    -3.0f0 0.5f0)  ;; greater 1/2
  t)

(deftest fround-bs.mp.3
  (fround-check-bs -5 2.0f0    -2.0f0 -1.0f0)  ;; equal even 1/2
  t)

(deftest fround-bs.mp.4
  (fround-check-bs -7 2.0f0    -4.0f0 1.0f0)  ;; equal odd 1/2
  t)

(deftest fround-bs.pm.1
  (fround-check-bs 2 -1.5f0    -1.0f0 0.5f0)  ;; less 1/2
  t)

(deftest fround-bs.pm.2
  (fround-check-bs 4 -1.5f0    -3.0f0 -0.5f0)  ;; greater 1/2
  t)

(deftest fround-bs.pm.3
  (fround-check-bs 5 -2.0f0    -2.0f0 1.0f0)  ;; equal even 1/2
  t)

(deftest fround-bs.pm.4
  (fround-check-bs 7 -2.0f0    -4.0f0 -1.0f0)  ;; equal odd 1/2
  t)

(deftest fround-bs.mm.1
  (fround-check-bs -2 -1.5f0    1.0f0 -0.5f0)  ;; less 1/2
  t)

(deftest fround-bs.mm.2
  (fround-check-bs -4 -1.5f0    3.0f0 0.5f0)  ;; greater 1/2
  t)

(deftest fround-bs.mm.3
  (fround-check-bs -5 -2.0f0    2.0f0 -1.0f0)  ;; equal even 1/2
  t)

(deftest fround-bs.mm.4
  (fround-check-bs -7 -2.0f0    4.0f0 1.0f0)  ;; equal odd 1/2
  t)

;;  bignum - double-float
(defun fround-check-bd (a b c d)
  (froundb-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fround-bd.1
  (froundb 0 10.3d0)
  0.0d0 0.0d0)

(deftest fround-bd.2
  (froundb 0 -10.3d0)
  0.0d0 0.0d0)

(deftest-error fround-bd.3
  (froundb 10 0.0d0)
  division-by-zero)

(deftest fround-bd.4a
  (froundb 5 0.5d0)
  10.0d0 0.0d0)

(deftest fround-bd.4b
  (froundb -5 0.5d0)
  -10.0d0 0.0d0)

(deftest fround-bd.4c
  (froundb 5 -0.5d0)
  -10.0d0 0.0d0)

(deftest fround-bd.4d
  (froundb -5 -0.5d0)
  10.0d0 0.0d0)

(deftest fround-bd.pp.1
  (fround-check-bd 2 1.5d0    1.0d0 0.5d0)  ;; less 1/2
  t)

(deftest fround-bd.pp.2
  (fround-check-bd 4 1.5d0    3.0d0 -0.5d0)  ;; greater 1/2
  t)

(deftest fround-bd.pp.3
  (fround-check-bd 5 2.0d0    2.0d0 1.0d0)  ;; equal even 1/2
  t)

(deftest fround-bd.pp.4
  (fround-check-bd 7 2.0d0    4.0d0 -1.0d0)  ;; equal odd 1/2
  t)

(deftest fround-bd.mp.1
  (fround-check-bd -2 1.5d0    -1.0d0 -0.5d0)  ;; less 1/2
  t)

(deftest fround-bd.mp.2
  (fround-check-bd -4 1.5d0    -3.0d0 0.5d0)  ;; greater 1/2
  t)

(deftest fround-bd.mp.3
  (fround-check-bd -5 2.0d0    -2.0d0 -1.0d0)  ;; equal even 1/2
  t)

(deftest fround-bd.mp.4
  (fround-check-bd -7 2.0d0    -4.0d0 1.0d0)  ;; equal odd 1/2
  t)

(deftest fround-bd.pm.1
  (fround-check-bd 2 -1.5d0    -1.0d0 0.5d0)  ;; less 1/2
  t)

(deftest fround-bd.pm.2
  (fround-check-bd 4 -1.5d0    -3.0d0 -0.5d0)  ;; greater 1/2
  t)

(deftest fround-bd.pm.3
  (fround-check-bd 5 -2.0d0    -2.0d0 1.0d0)  ;; equal even 1/2
  t)

(deftest fround-bd.pm.4
  (fround-check-bd 7 -2.0d0    -4.0d0 -1.0d0)  ;; equal odd 1/2
  t)

(deftest fround-bd.mm.1
  (fround-check-bd -2 -1.5d0    1.0d0 -0.5d0)  ;; less 1/2
  t)

(deftest fround-bd.mm.2
  (fround-check-bd -4 -1.5d0    3.0d0 0.5d0)  ;; greater 1/2
  t)

(deftest fround-bd.mm.3
  (fround-check-bd -5 -2.0d0    2.0d0 -1.0d0)  ;; equal even 1/2
  t)

(deftest fround-bd.mm.4
  (fround-check-bd -7 -2.0d0    4.0d0 1.0d0)  ;; equal odd 1/2
  t)

;;  bignum - long-float
(defun fround-check-bl (a b c d)
  (froundb-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest fround-bl.1
  (froundb 0 10.3l0)
  0.0l0 0.0l0)

(deftest fround-bl.2
  (froundb 0 -10.3l0)
  0.0l0 0.0l0)

(deftest-error fround-bl.3
  (froundb 10 0.0l0)
  division-by-zero)

(deftest fround-bl.4a
  (froundb 5 0.5l0)
  10.0l0 0.0l0)

(deftest fround-bl.4b
  (froundb -5 0.5l0)
  -10.0l0 0.0l0)

(deftest fround-bl.4c
  (froundb 5 -0.5l0)
  -10.0l0 0.0l0)

(deftest fround-bl.4d
  (froundb -5 -0.5l0)
  10.0l0 0.0l0)

(deftest fround-bl.pp.1
  (fround-check-bl 2 1.5l0    1.0l0 0.5l0)  ;; less 1/2
  t)

(deftest fround-bl.pp.2
  (fround-check-bl 4 1.5l0    3.0l0 -0.5l0)  ;; greater 1/2
  t)

(deftest fround-bl.pp.3
  (fround-check-bl 5 2.0l0    2.0l0 1.0l0)  ;; equal even 1/2
  t)

(deftest fround-bl.pp.4
  (fround-check-bl 7 2.0l0    4.0l0 -1.0l0)  ;; equal odd 1/2
  t)

(deftest fround-bl.mp.1
  (fround-check-bl -2 1.5l0    -1.0l0 -0.5l0)  ;; less 1/2
  t)

(deftest fround-bl.mp.2
  (fround-check-bl -4 1.5l0    -3.0l0 0.5l0)  ;; greater 1/2
  t)

(deftest fround-bl.mp.3
  (fround-check-bl -5 2.0l0    -2.0l0 -1.0l0)  ;; equal even 1/2
  t)

(deftest fround-bl.mp.4
  (fround-check-bl -7 2.0l0    -4.0l0 1.0l0)  ;; equal odd 1/2
  t)

(deftest fround-bl.pm.1
  (fround-check-bl 2 -1.5l0    -1.0l0 0.5l0)  ;; less 1/2
  t)

(deftest fround-bl.pm.2
  (fround-check-bl 4 -1.5l0    -3.0l0 -0.5l0)  ;; greater 1/2
  t)

(deftest fround-bl.pm.3
  (fround-check-bl 5 -2.0l0    -2.0l0 1.0l0)  ;; equal even 1/2
  t)

(deftest fround-bl.pm.4
  (fround-check-bl 7 -2.0l0    -4.0l0 -1.0l0)  ;; equal odd 1/2
  t)

(deftest fround-bl.mm.1
  (fround-check-bl -2 -1.5l0    1.0l0 -0.5l0)  ;; less 1/2
  t)

(deftest fround-bl.mm.2
  (fround-check-bl -4 -1.5l0    3.0l0 0.5l0)  ;; greater 1/2
  t)

(deftest fround-bl.mm.3
  (fround-check-bl -5 -2.0l0    2.0l0 -1.0l0)  ;; equal even 1/2
  t)

(deftest fround-bl.mm.4
  (fround-check-bl -7 -2.0l0    4.0l0 1.0l0)  ;; equal odd 1/2
  t)

