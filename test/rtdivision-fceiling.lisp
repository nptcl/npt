;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(defun fceiling-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float))
  (multiple-value-bind (e f) (fceiling a b)
    (or (and (typep e type)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "fceiling-equal error: (~S ~S) ~A, ~A, ~A" e f
          (typep e type)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  fceiling1
;;
(deftest fceiling1-integer.1
  (fceiling 0)
  0.0 0)

(deftest fceiling1-integer.2
  (fceiling -10)
  -10.0 0)

(deftest fceiling1-integer.3
  (fceiling -10)
  -10.0 0)

(deftest fceiling1-integer.4
  (fceiling 99999999999999999999)
  99999999999999999999.0 0)

(deftest fceiling1-integer.5
  (fceiling -99999999999999999999)
  -99999999999999999999.0 0)

(deftest fceiling1-ratio.1
  (fceiling 1/3)
  1.0 -2/3)

(deftest fceiling1-ratio.2
  (fceiling 10/3)
  4.0 -2/3)

(deftest fceiling1-ratio.3
  (fceiling -1/3)
  0.0 -1/3)

(deftest fceiling1-ratio.4
  (fceiling -10/3)
  -3.0 -1/3)

(deftest fceiling1-float.1
  (fceiling 10.0)
  10.0 0.0)

(deftest fceiling1-float.2
  (fceiling -10.0)
  -10.0 0.0)

(deftest fceiling1-float.3
  (fceiling 12.25)
  13.0 -0.75)

(deftest fceiling1-float.4
  (fceiling -12.25)
  -12.0 -0.25)

(deftest fceiling1-float.5
  (fceiling 12.25f0)
  13.0f0 -0.75f0)

(deftest fceiling1-float.6
  (fceiling -12.25f0)
  -12.0f0 -0.25f0)

(deftest fceiling1-float.7
  (fceiling 12.25d0)
  13.0d0 -0.75d0)

(deftest fceiling1-float.8
  (fceiling -12.25d0)
  -12.0d0 -0.25d0)

(deftest fceiling1-float.9
  (fceiling 12.25l0)
  13.0l0 -0.75l0)

(deftest fceiling1-float.10
  (fceiling -12.25l0)
  -12.0l0 -0.25l0)


;;
;;  fceiling
;;

;;  fixnum - fixnum
(deftest fceiling-ff.1
  (fceiling 0 10)
  0.0 0)

(deftest fceiling-ff.2
  (fceiling 0 -10)
  0.0 0)

(deftest-error fceiling-ff.3
  (fceiling 10 0)
  division-by-zero)

(deftest fceiling-ff.4a
  (fceiling 3 10)
  1.0 -7)

(deftest fceiling-ff.4b
  (fceiling -3 10)
  0.0 -3)

(deftest fceiling-ff.4c
  (fceiling 3 -10)
  0.0 3)

(deftest fceiling-ff.4d
  (fceiling -3 -10)
  1.0 7)

(deftest fceiling-ff.5a
  (fceiling 10 3)
  4.0 -2)

(deftest fceiling-ff.5b
  (fceiling -10 3)
  -3.0 -1)

(deftest fceiling-ff.5c
  (fceiling 10 -3)
  -3.0 1)

(deftest fceiling-ff.5d
  (fceiling -10 -3)
  4.0 2)

(deftest fceiling-ff.6a
  (fceiling 11 3)
  4.0 -1)

(deftest fceiling-ff.6b
  (fceiling -11 3)
  -3.0 -2)

(deftest fceiling-ff.6c
  (fceiling 11 -3)
  -3.0 2)

(deftest fceiling-ff.6d
  (fceiling -11 -3)
  4.0 1)


;; fixnum - bignum
(deftest fceiling-fb.1
  (fceiling 0 (make-bignum 10))
  0.0 0)

(deftest fceiling-fb.2
  (fceiling 0 (make-bignum -10))
  0.0 0)

(deftest-error fceiling-fb.3
  (fceiling 10 (make-bignum 0))
  division-by-zero)

(deftest fceiling-fb.4a
  (fceiling 3 (make-bignum 10))
  1.0 -7)

(deftest fceiling-fb.4b
  (fceiling -3 (make-bignum 10))
  -0.0 -3)

(deftest fceiling-fb.4c
  (fceiling 3 (make-bignum -10))
  -0.0 3)

(deftest fceiling-fb.4d
  (fceiling -3 (make-bignum -10))
  1.0 7)

(deftest fceiling-fb.5a
  (fceiling 10 (make-bignum 3))
  4.0 -2)

(deftest fceiling-fb.5b
  (fceiling -10 (make-bignum 3))
  -3.0 -1)

(deftest fceiling-fb.5c
  (fceiling 10 (make-bignum -3))
  -3.0 1)

(deftest fceiling-fb.5d
  (fceiling -10 (make-bignum -3))
  4.0 2)

(deftest fceiling-fb.6a
  (fceiling 11 (make-bignum 3))
  4.0 -1)

(deftest fceiling-fb.6b
  (fceiling -11 (make-bignum 3))
  -3.0 -2)

(deftest fceiling-fb.6c
  (fceiling 11 (make-bignum -3))
  -3.0 2)

(deftest fceiling-fb.6d
  (fceiling -11 (make-bignum -3))
  4.0 1)


;;  fixnum - ratio
(deftest fceiling-fr.1
  (fceiling 0 10/3)
  0.0 0)

(deftest fceiling-fr.2
  (fceiling 0 -10/3)
  0.0 0)

(deftest-error fceiling-fr.3
  (fceiling 10 (make-ratio 0 1))
  division-by-zero)

(deftest fceiling-fr.4a
  (fceiling 3 100/7)
  1.0 -79/7)

(deftest fceiling-fr.4b
  (fceiling -3 100/7)
  -0.0 -3)

(deftest fceiling-fr.4c
  (fceiling 3 -100/7)
  -0.0 3)

(deftest fceiling-fr.4d
  (fceiling -3 -100/7)
  1.0 79/7)

(deftest fceiling-fr.5a
  (fceiling 10 6/7)
  12.0 -2/7)

(deftest fceiling-fr.5b
  (fceiling -10 6/7)
  -11.0 -4/7)

(deftest fceiling-fr.5c
  (fceiling 10 -6/7)
  -11.0 4/7)

(deftest fceiling-fr.5d
  (fceiling -10 -6/7)
  12.0 2/7)

(deftest fceiling-fr.6a
  (fceiling 10 1/3)
  30.0 0)

(deftest fceiling-fr.6b
  (fceiling -10 1/3)
  -30.0 0)

(deftest fceiling-fr.6c
  (fceiling 10 -1/3)
  -30.0 0)

(deftest fceiling-fr.6d
  (fceiling -10 -1/3)
  30.0 0)

(deftest fceiling-fr.7a
  (fceiling 10 4/3)
  8.0 -2/3)


;;  fixnum - single-float
(defun fceiling-check-fs (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-fs.1
  (fceiling 0 10.0f0)
  0.0f0 0.0f0)

(deftest fceiling-fs.2
  (fceiling 0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error fceiling-fs.3
  (fceiling 10 0.0f0)
  division-by-zero)

(deftest fceiling-fs.4a
  (fceiling-check-fs 4 10.5f0    1.0f0 -6.5f0)
  t)

(deftest fceiling-fs.4b
  (fceiling-check-fs -4 10.5f0    0.0f0 -4.0f0)
  t)

(deftest fceiling-fs.4c
  (fceiling-check-fs 4 -10.5f0    0.0f0 4.0f0)
  t)

(deftest fceiling-fs.4d
  (fceiling-check-fs -4 -10.5f0    1.0f0 6.5f0)
  t)

(deftest fceiling-fs.5a
  (fceiling-check-fs 15 1.6f0    10.0f0 -1.0f0)
  t)

(deftest fceiling-fs.5b
  (fceiling-check-fs -15 1.6f0    -9.0f0 -0.6f0)
  t)

(deftest fceiling-fs.5c
  (fceiling-check-fs 15 -1.6f0    -9.0f0 0.6f0)
  t)

(deftest fceiling-fs.5d
  (fceiling-check-fs -15 -1.6f0    10.0f0 1.0f0)
  t)


;;  fixnum - double-float
(defun fceiling-check-fd (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fceiling-fd.1
  (fceiling 0 10.0d0)
  0.0d0 0.0d0)

(deftest fceiling-fd.2
  (fceiling 0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error fceiling-fd.3
  (fceiling 10 0.0d0)
  division-by-zero)

(deftest fceiling-fd.4a
  (fceiling-check-fd 4 10.5d0    1.0d0 -6.5d0)
  t)

(deftest fceiling-fd.4b
  (fceiling-check-fd -4 10.5d0    0.0d0 -4.0d0)
  t)

(deftest fceiling-fd.4c
  (fceiling-check-fd 4 -10.5d0    0.0d0 4.0d0)
  t)

(deftest fceiling-fd.4d
  (fceiling-check-fd -4 -10.5d0    1.0d0 6.5d0)
  t)

(deftest fceiling-fd.5a
  (fceiling-check-fd 15 1.6d0    10.0d0 -1.0d0)
  t)

(deftest fceiling-fd.5b
  (fceiling-check-fd -15 1.6d0    -9.0d0 -0.6d0)
  t)

(deftest fceiling-fd.5c
  (fceiling-check-fd 15 -1.6d0    -9.0d0 0.6d0)
  t)

(deftest fceiling-fd.5d
  (fceiling-check-fd -15 -1.6d0    10.0d0 1.0d0)
  t)


;;  fixnum - long-float
(defun fceiling-check-fl (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest fceiling-fl.1
  (fceiling 0 10.0l0)
  0.0l0 0.0l0)

(deftest fceiling-fl.2
  (fceiling 0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error fceiling-fl.3
  (fceiling 10 0.0l0)
  division-by-zero)

(deftest fceiling-fl.4a
  (fceiling-check-fl 4 10.5l0    1.0l0 -6.5l0)
  t)

(deftest fceiling-fl.4b
  (fceiling-check-fl -4 10.5l0    0.0l0 -4.0l0)
  t)

(deftest fceiling-fl.4c
  (fceiling-check-fl 4 -10.5l0    0.0l0 4.0l0)
  t)

(deftest fceiling-fl.4d
  (fceiling-check-fl -4 -10.5l0    1.0l0 6.5l0)
  t)

(deftest fceiling-fl.5a
  (fceiling-check-fl 15 1.6l0    10.0l0 -1.0l0)
  t)

(deftest fceiling-fl.5b
  (fceiling-check-fl -15 1.6l0    -9.0l0 -0.6l0)
  t)

(deftest fceiling-fl.5c
  (fceiling-check-fl 15 -1.6l0    -9.0l0 0.6l0)
  t)

(deftest fceiling-fl.5d
  (fceiling-check-fl -15 -1.6l0    10.0l0 1.0l0)
  t)


;;  bignum - fixnum
(defun fceilingb (a b)
  (fceiling (make-bignum a) b))

(deftest fceiling-bf.1
  (fceilingb 0 10)
  0.0 0)

(deftest fceiling-bf.2
  (fceilingb 0 -10)
  0.0 0)

(deftest-error fceiling-bf.3
  (fceilingb 10 0)
  division-by-zero)

(deftest fceiling-bf.4a
  (fceilingb 3 10)
  1.0 -7)

(deftest fceiling-bf.4b
  (fceilingb -3 10)
  -0.0 -3)

(deftest fceiling-bf.4c
  (fceilingb 3 -10)
  -0.0 3)

(deftest fceiling-bf.4d
  (fceilingb -3 -10)
  1.0 7)

(deftest fceiling-bf.5a
  (fceilingb 10 3)
  4.0 -2)

(deftest fceiling-bf.5b
  (fceilingb -10 3)
  -3.0 -1)

(deftest fceiling-bf.5c
  (fceilingb 10 -3)
  -3.0 1)

(deftest fceiling-bf.5d
  (fceilingb -10 -3)
  4.0 2)

(deftest fceiling-bf.6a
  (fceilingb 11 3)
  4.0 -1)

(deftest fceiling-bf.6b
  (fceilingb -11 3)
  -3.0 -2)

(deftest fceiling-bf.6c
  (fceilingb 11 -3)
  -3.0 2)

(deftest fceiling-bf.6d
  (fceilingb -11 -3)
  4.0 1)


;; bignum - bignum
(deftest fceiling-bb.1
  (fceilingb 0 (make-bignum 10))
  0.0 0)

(deftest fceiling-bb.2
  (fceilingb 0 (make-bignum -10))
  0.0 0)

(deftest-error fceiling-bb.3
  (fceilingb 10 (make-bignum 0))
  division-by-zero)

(deftest fceiling-bb.4a
  (fceilingb 3 (make-bignum 10))
  1.0 -7)

(deftest fceiling-bb.4b
  (fceilingb -3 (make-bignum 10))
  -0.0 -3)

(deftest fceiling-bb.4c
  (fceilingb 3 (make-bignum -10))
  -0.0 3)

(deftest fceiling-bb.4d
  (fceilingb -3 (make-bignum -10))
  1.0 7)

(deftest fceiling-bb.5a
  (fceilingb 10 (make-bignum 3))
  4.0 -2)

(deftest fceiling-bb.5b
  (fceilingb -10 (make-bignum 3))
  -3.0 -1)

(deftest fceiling-bb.5c
  (fceilingb 10 (make-bignum -3))
  -3.0 1)

(deftest fceiling-bb.5d
  (fceilingb -10 (make-bignum -3))
  4.0 2)

(deftest fceiling-bb.6a
  (fceilingb 11 (make-bignum 3))
  4.0 -1)

(deftest fceiling-bb.6b
  (fceilingb -11 (make-bignum 3))
  -3.0 -2)

(deftest fceiling-bb.6c
  (fceilingb 11 (make-bignum -3))
  -3.0 2)

(deftest fceiling-bb.6d
  (fceilingb -11 (make-bignum -3))
  4.0 1)


;;  bignum - ratio
(deftest fceiling-br.1
  (fceilingb 0 10/3)
  0.0 0)

(deftest fceiling-br.2
  (fceilingb 0 -10/3)
  0.0 0)

(deftest-error fceiling-br.3
  (fceilingb 10 (make-ratio 0 1))
  division-by-zero)

(deftest fceiling-br.4a
  (fceilingb 3 100/7)
  1.0 -79/7)

(deftest fceiling-br.4b
  (fceilingb -3 100/7)
  -0.0 -3)

(deftest fceiling-br.4c
  (fceilingb 3 -100/7)
  -0.0 3)

(deftest fceiling-br.4d
  (fceilingb -3 -100/7)
  1.0 79/7)

(deftest fceiling-br.5a
  (fceilingb 10 6/7)
  12.0 -2/7)

(deftest fceiling-br.5b
  (fceilingb -10 6/7)
  -11.0 -4/7)

(deftest fceiling-br.5c
  (fceilingb 10 -6/7)
  -11.0 4/7)

(deftest fceiling-br.5d
  (fceilingb -10 -6/7)
  12.0 2/7)

(deftest fceiling-br.6a
  (fceilingb 10 1/3)
  30.0 0)

(deftest fceiling-br.6b
  (fceilingb -10 1/3)
  -30.0 0)

(deftest fceiling-br.6c
  (fceilingb 10 -1/3)
  -30.0 0)

(deftest fceiling-br.6d
  (fceilingb -10 -1/3)
  30.0 0)


;;  bignum - single-float
(defun fceiling-check-bs (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-bs.1
  (fceilingb 0 10.0f0)
  0.0f0 0.0f0)

(deftest fceiling-bs.2
  (fceilingb 0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error fceiling-bs.3
  (fceilingb 10 0.0f0)
  division-by-zero)

(deftest fceiling-bs.4a
  (fceiling-check-bs 4 10.5f0    1.0f0 -6.5f0)
  t)

(deftest fceiling-bs.4b
  (fceiling-check-bs -4 10.5f0    0.0f0 -4.0f0)
  t)

(deftest fceiling-bs.4c
  (fceiling-check-bs 4 -10.5f0    0.0f0 4.0f0)
  t)

(deftest fceiling-bs.4d
  (fceiling-check-bs -4 -10.5f0    1.0f0 6.5f0)
  t)

(deftest fceiling-bs.5a
  (fceiling-check-bs 15 1.6f0    10.0f0 -1.0f0)
  t)

(deftest fceiling-bs.5b
  (fceiling-check-bs -15 1.6f0    -9.0f0 -0.6f0)
  t)

(deftest fceiling-bs.5c
  (fceiling-check-bs 15 -1.6f0    -9.0f0 0.6f0)
  t)

(deftest fceiling-bs.5d
  (fceiling-check-bs -15 -1.6f0    10.0f0 1.0f0)
  t)


;;  bignum - double-float
(defun fceiling-check-bd (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fceiling-bd.1
  (fceilingb 0 10.0d0)
  0.0d0 0.0d0)

(deftest fceiling-bd.2
  (fceilingb 0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error fceiling-bd.3
  (fceilingb 10 0.0d0)
  division-by-zero)

(deftest fceiling-bd.4a
  (fceiling-check-bd 4 10.5d0    1.0d0 -6.5d0)
  t)

(deftest fceiling-bd.4b
  (fceiling-check-bd -4 10.5d0    0.0d0 -4.0d0)
  t)

(deftest fceiling-bd.4c
  (fceiling-check-bd 4 -10.5d0    0.0d0 4.0d0)
  t)

(deftest fceiling-bd.4d
  (fceiling-check-bd -4 -10.5d0    1.0d0 6.5d0)
  t)

(deftest fceiling-bd.5a
  (fceiling-check-bd 15 1.6d0    10.0d0 -1.0d0)
  t)

(deftest fceiling-bd.5b
  (fceiling-check-bd -15 1.6d0    -9.0d0 -0.6d0)
  t)

(deftest fceiling-bd.5c
  (fceiling-check-bd 15 -1.6d0    -9.0d0 0.6d0)
  t)

(deftest fceiling-bd.5d
  (fceiling-check-bd -15 -1.6d0    10.0d0 1.0d0)
  t)


;;  bignum - long-float
(defun fceiling-check-bl (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest fceiling-bl.1
  (fceilingb 0 10.0l0)
  0.0l0 0.0l0)

(deftest fceiling-bl.2
  (fceilingb 0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error fceiling-bl.3
  (fceilingb 10 0.0l0)
  division-by-zero)

(deftest fceiling-bl.4a
  (fceiling-check-bl 4 10.5l0    1.0l0 -6.5l0)
  t)

(deftest fceiling-bl.4b
  (fceiling-check-bl -4 10.5l0    0.0l0 -4.0l0)
  t)

(deftest fceiling-bl.4c
  (fceiling-check-bl 4 -10.5l0    0.0l0 4.0l0)
  t)

(deftest fceiling-bl.4d
  (fceiling-check-bl -4 -10.5l0    1.0l0 6.5l0)
  t)

(deftest fceiling-bl.5a
  (fceiling-check-bl 15 1.6l0    10.0l0 -1.0l0)
  t)

(deftest fceiling-bl.5b
  (fceiling-check-bl -15 1.6l0    -9.0l0 -0.6l0)
  t)

(deftest fceiling-bl.5c
  (fceiling-check-bl 15 -1.6l0    -9.0l0 0.6l0)
  t)

(deftest fceiling-bl.5d
  (fceiling-check-bl -15 -1.6l0    10.0l0 1.0l0)
  t)


;;  ratio - fixnum
(deftest fceiling-rf.1
  (fceiling (make-ratio 0 1) 10)
  0.0 0)

(deftest fceiling-rf.2
  (fceiling (make-ratio 0 1) -10)
  0.0 0)

(deftest-error fceiling-rf.3
  (fceiling 10/3 0)
  division-by-zero)

(deftest fceiling-rf.4a
  (fceiling 2/3 5)
  1.0 -13/3)

(deftest fceiling-rf.4b
  (fceiling -2/3 5)
  -0.0 -2/3)

(deftest fceiling-rf.4c
  (fceiling 2/3 -5)
  -0.0 2/3)

(deftest fceiling-rf.4d
  (fceiling -2/3 -5)
  1.0 13/3)

(deftest fceiling-rf.5a
  (fceiling 20/3 5)
  2.0 -10/3)

(deftest fceiling-rf.5b
  (fceiling -20/3 5)
  -1.0 -5/3)

(deftest fceiling-rf.5c
  (fceiling 20/3 -5)
  -1.0 5/3)

(deftest fceiling-rf.5d
  (fceiling -20/3 -5)
  2.0 10/3)

(deftest fceiling-rf.6a
  (fceiling 53/3 8)
  3.0 -19/3)

(deftest fceiling-rf.6b
  (fceiling -53/3 8)
  -2.0 -5/3)

(deftest fceiling-rf.6c
  (fceiling 53/3 -8)
  -2.0 5/3)

(deftest fceiling-rf.6d
  (fceiling -53/3 -8)
  3.0 19/3)


;; ratio - bignum
(deftest fceiling-rb.1
  (fceiling (make-ratio 0 1) (make-bignum 10))
  0.0 0)

(deftest fceiling-rb.2
  (fceiling (make-ratio 0 1) (make-bignum -10))
  0.0 0)

(deftest-error fceiling-rb.3
  (fceiling 10/3 (make-bignum 0))
  division-by-zero)

(deftest fceiling-rb.4a
  (fceiling 2/3 (make-bignum 5))
  1.0 -13/3)

(deftest fceiling-rb.4b
  (fceiling -2/3 (make-bignum 5))
  -0.0 -2/3)

(deftest fceiling-rb.4c
  (fceiling 2/3 (make-bignum -5))
  -0.0 2/3)

(deftest fceiling-rb.4d
  (fceiling -2/3 (make-bignum -5))
  1.0 13/3)

(deftest fceiling-rb.5a
  (fceiling 20/3 (make-bignum 5))
  2.0 -10/3)

(deftest fceiling-rb.5b
  (fceiling -20/3 (make-bignum 5))
  -1.0 -5/3)

(deftest fceiling-rb.5c
  (fceiling 20/3 (make-bignum -5))
  -1.0 5/3)

(deftest fceiling-rb.5d
  (fceiling -20/3 (make-bignum -5))
  2.0 10/3)

(deftest fceiling-rb.6a
  (fceiling 53/3 (make-bignum 8))
  3.0 -19/3)

(deftest fceiling-rb.6b
  (fceiling -53/3 (make-bignum 8))
  -2.0 -5/3)

(deftest fceiling-rb.6c
  (fceiling 53/3 (make-bignum -8))
  -2.0 5/3)

(deftest fceiling-rb.6d
  (fceiling -53/3 (make-bignum -8))
  3.0 19/3)


;;  ratio - ratio
(deftest fceiling-rr.1
  (fceiling (make-ratio 0 1) 10/3)
  0.0 0)

(deftest fceiling-rr.2
  (fceiling (make-ratio 0 1) -10/3)
  0.0 0)

(deftest-error fceiling-rr.3
  (fceiling 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest fceiling-rr.4a
  (fceiling 1/2 33/5)
  1.0 -61/10)

(deftest fceiling-rr.4b
  (fceiling -1/2 33/5)
  -0.0 -1/2)

(deftest fceiling-rr.4c
  (fceiling 1/2 -33/5)
  -0.0 1/2)

(deftest fceiling-rr.4d
  (fceiling -1/2 -33/5)
  1.0 61/10)

(deftest fceiling-rr.5a
  (fceiling 79/3 2/7)
  93.0 -5/21)

(deftest fceiling-rr.5b
  (fceiling -79/3 2/7)
  -92.0 -1/21)

(deftest fceiling-rr.5c
  (fceiling 79/3 -2/7)
  -92.0 1/21)

(deftest fceiling-rr.5d
  (fceiling -79/3 -2/7)
  93.0 5/21)

(deftest fceiling-rr.6a
  (fceiling 4/5 8/20)
  2.0 0)

(deftest fceiling-rr.6b
  (fceiling -4/5 8/20)
  -2.0 0)

(deftest fceiling-rr.6c
  (fceiling 4/5 -8/20)
  -2.0 0)

(deftest fceiling-rr.6d
  (fceiling -4/5 -8/20)
  2.0 0)


;;  ratio - single-float
(defun fceiling-check-rs (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-rs.1
  (fceiling (make-ratio 0 1) 10.0f0)
  0.0f0 0.0f0)

(deftest fceiling-rs.2
  (fceiling (make-ratio 0 1) -10.0f0)
  0.0f0 0.0f0)

(deftest-error fceiling-rs.3
  (fceiling 10/4 0.0f0)
  division-by-zero)

(deftest fceiling-rs.4a
  (fceiling-check-rs 3/4 5.5f0    1.0f0 -4.75f0)
  t)

(deftest fceiling-rs.4b
  (fceiling-check-rs -3/4 5.5f0    0.0f0 -0.75f0)
  t)

(deftest fceiling-rs.4c
  (fceiling-check-rs 3/4 -5.5f0    0.0f0 0.75f0)
  t)

(deftest fceiling-rs.4d
  (fceiling-check-rs -3/4 -5.5f0    1.0f0 4.75f0)
  t)

(deftest fceiling-rs.5a
  (fceiling-check-rs 77/4 1.6f0    13.0f0 -1.55f0)
  t)

(deftest fceiling-rs.5b
  (fceiling-check-rs -77/4 1.6f0    -12.0f0 -0.05f0)
  t)

(deftest fceiling-rs.5c
  (fceiling-check-rs 77/4 -1.6f0    -12.0f0 0.05f0)
  t)

(deftest fceiling-rs.5d
  (fceiling-check-rs -77/4 -1.6f0    13.0f0 1.55f0)
  t)


;;  ratio - double-float
(defun fceiling-check-rd (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fceiling-rd.1
  (fceiling (make-ratio 0 1) 10.0d0)
  0.0d0 0.0d0)

(deftest fceiling-rd.2
  (fceiling (make-ratio 0 1) -10.0d0)
  0.0d0 0.0d0)

(deftest-error fceiling-rd.3
  (fceiling 10/4 0.0d0)
  division-by-zero)

(deftest fceiling-rd.4a
  (fceiling-check-rd 3/4 5.5d0    1.0d0 -4.75d0)
  t)

(deftest fceiling-rd.4b
  (fceiling-check-rd -3/4 5.5d0    0.0d0 -0.75d0)
  t)

(deftest fceiling-rd.4c
  (fceiling-check-rd 3/4 -5.5d0    0.0d0 0.75d0)
  t)

(deftest fceiling-rd.4d
  (fceiling-check-rd -3/4 -5.5d0    1.0d0 4.75d0)
  t)

(deftest fceiling-rd.5a
  (fceiling-check-rd 77/4 1.6d0    13.0d0 -1.55d0)
  t)

(deftest fceiling-rd.5b
  (fceiling-check-rd -77/4 1.6d0    -12.0d0 -0.05d0)
  t)

(deftest fceiling-rd.5c
  (fceiling-check-rd 77/4 -1.6d0    -12.0d0 0.05d0)
  t)

(deftest fceiling-rd.5d
  (fceiling-check-rd -77/4 -1.6d0    13.0d0 1.55d0)
  t)


;;  ratio - long-float
(defun fceiling-check-rl (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest fceiling-rl.1
  (fceiling (make-ratio 0 1) 10.0l0)
  0.0l0 0.0l0)

(deftest fceiling-rl.2
  (fceiling (make-ratio 0 1) -10.0l0)
  0.0l0 0.0l0)

(deftest-error fceiling-rl.3
  (fceiling 10/4 0.0l0)
  division-by-zero)

(deftest fceiling-rl.4a
  (fceiling-check-rl 3/4 5.5l0    1.0l0 -4.75l0)
  t)

(deftest fceiling-rl.4b
  (fceiling-check-rl -3/4 5.5l0    0.0l0 -0.75l0)
  t)

(deftest fceiling-rl.4c
  (fceiling-check-rl 3/4 -5.5l0    0.0l0 0.75l0)
  t)

(deftest fceiling-rl.4d
  (fceiling-check-rl -3/4 -5.5l0    1.0l0 4.75l0)
  t)

(deftest fceiling-rl.5a
  (fceiling-check-rl 77/4 1.6l0    13.0l0 -1.55l0)
  t)

(deftest fceiling-rl.5b
  (fceiling-check-rl -77/4 1.6l0    -12.0l0 -0.05l0)
  t)

(deftest fceiling-rl.5c
  (fceiling-check-rl 77/4 -1.6l0    -12.0l0 0.05l0)
  t)

(deftest fceiling-rl.5d
  (fceiling-check-rl -77/4 -1.6l0    13.0l0 1.55l0)
  t)


;; single-float - fixnum
(defun fceiling-check-sf (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-sf.1
  (fceiling 0.0f0 10)
  0.0f0 0.0f0)

(deftest fceiling-sf.2
  (fceiling 0.0f0 -10)
  0.0f0 0.0f0)

(deftest-error fceiling-sf.3
  (fceiling 10.4f0 0)
  division-by-zero)

(deftest fceiling-sf.4a
  (fceiling-check-sf 1.2f0 10    1.0f0 -8.8f0)
  t)

(deftest fceiling-sf.4b
  (fceiling-check-sf -1.2f0 10    0.0f0 -1.2f0)
  t)

(deftest fceiling-sf.4c
  (fceiling-check-sf 1.2f0 -10    0.0f0 1.2f0)
  t)

(deftest fceiling-sf.4d
  (fceiling-check-sf -1.2f0 -10    1.0f0 8.8f0)
  t)

(deftest fceiling-sf.5a
  (fceiling-check-sf 10.2f0 4    3.0f0 -1.8f0)
  t)

(deftest fceiling-sf.5b
  (fceiling-check-sf -10.2f0 4    -2.0f0 -2.2f0)
  t)

(deftest fceiling-sf.5c
  (fceiling-check-sf 10.2f0 -4    -2.0f0 2.2f0)
  t)

(deftest fceiling-sf.5d
  (fceiling-check-sf -10.2f0 -4    3.0f0 1.8f0)
  t)


;; single-float - bignum
(defun fceiling-check-sb (a b c d)
  (fceiling-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-sb.1
  (fceiling 0.0f0 (make-bignum 10))
  0.0f0 0.0f0)

(deftest fceiling-sb.2
  (fceiling 0.0f0 (make-bignum -10))
  0.0f0 0.0f0)

(deftest-error fceiling-sb.3
  (fceiling 10.4f0 (make-bignum 0))
  division-by-zero)

(deftest fceiling-sb.4a
  (fceiling-check-sb 1.2f0 10    1.0f0 -8.8f0)
  t)

(deftest fceiling-sb.4b
  (fceiling-check-sb -1.2f0 10    0.0f0 -1.2f0)
  t)

(deftest fceiling-sb.4c
  (fceiling-check-sb 1.2f0 -10    0.0f0 1.2f0)
  t)

(deftest fceiling-sb.4d
  (fceiling-check-sb -1.2f0 -10    1.0f0 8.8f0)
  t)

(deftest fceiling-sb.5a
  (fceiling-check-sb 10.2f0 4    3.0f0 -1.8f0)
  t)

(deftest fceiling-sb.5b
  (fceiling-check-sb -10.2f0 4    -2.0f0 -2.2f0)
  t)

(deftest fceiling-sb.5c
  (fceiling-check-sb 10.2f0 -4    -2.0f0 2.2f0)
  t)

(deftest fceiling-sb.5d
  (fceiling-check-sb -10.2f0 -4    3.0f0 1.8f0)
  t)


;; single-float - ratio
(defun fceiling-check-sr (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-sr.1
  (fceiling 0.0f0 10/3)
  0.0f0 0.0f0)

(deftest fceiling-sr.2
  (fceiling 0.0f0 -10/3)
  0.0f0 0.0f0)

(deftest-error fceiling-sr.3
  (fceiling 10.4f0 (make-ratio 0 1))
  division-by-zero)

(deftest fceiling-sr.4a
  (fceiling-check-sr 1.2f0 15/4    1.0f0 -2.55f0)
  t)

(deftest fceiling-sr.4b
  (fceiling-check-sr -1.2f0 15/4    0.0f0 -1.2f0)
  t)

(deftest fceiling-sr.4c
  (fceiling-check-sr 1.2f0 -15/4    0.0f0 1.2f0)
  t)

(deftest fceiling-sr.4d
  (fceiling-check-sr -1.2f0 -15/4    1.0f0 2.55f0)
  t)

(deftest fceiling-sr.5a
  (fceiling-check-sr 1.2f0 1/4    5.0f0 -0.05f0)
  t)

(deftest fceiling-sr.5b
  (fceiling-check-sr -1.2f0 1/4    -4.0f0 -0.2f0)
  t)

(deftest fceiling-sr.5c
  (fceiling-check-sr 1.2f0 -1/4    -4.0f0 0.2f0)
  t)

(deftest fceiling-sr.5d
  (fceiling-check-sr -1.2f0 -1/4    5.0f0 0.05f0)
  t)


;; single-float - single-float
(defun fceiling-check-ss (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fceiling-ss.1
  (fceiling 0.0f0 10.0f0)
  0.0f0 0.0f0)

(deftest fceiling-ss.2
  (fceiling 0.0f0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error fceiling-ss.3
  (fceiling 10.4f0 0.0f0)
  division-by-zero)

(deftest fceiling-ss.4a
  (fceiling-check-ss 1.2f0 3.7f0    1.0f0 -2.5f0)
  t)

(deftest fceiling-ss.4b
  (fceiling-check-ss -1.2f0 3.7f0    0.0f0 -1.2f0)
  t)

(deftest fceiling-ss.4c
  (fceiling-check-ss 1.2f0 -3.7f0    0.0f0 1.2f0)
  t)

(deftest fceiling-ss.4d
  (fceiling-check-ss -1.2f0 -3.7f0    1.0f0 2.5f0)
  t)

(deftest fceiling-ss.5a
  (fceiling-check-ss 12.3f0 3.7f0    4.0f0 -2.5f0)
  t)

(deftest fceiling-ss.5b
  (fceiling-check-ss -12.3f0 3.7f0    -3.0f0 -1.2f0)
  t)

(deftest fceiling-ss.5c
  (fceiling-check-ss 12.3f0 -3.7f0    -3.0f0 1.2f0)
  t)

(deftest fceiling-ss.5d
  (fceiling-check-ss -12.3f0 -3.7f0    4.0f0 2.5f0)
  t)


;; single-float - double-float
(defun fceiling-check-sd (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :type 'double-float))

(deftest fceiling-sd.1
  (fceiling 0.0f0 10.0d0)
  0.0d0 0.0d0)

(deftest fceiling-sd.2
  (fceiling 0.0f0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error fceiling-sd.3
  (fceiling 10.4f0 0.0d0)
  division-by-zero)

(deftest fceiling-sd.4a
  (fceiling-check-sd 1.2f0 3.7d0    1.0d0 -2.5d0)
  t)

(deftest fceiling-sd.4b
  (fceiling-check-sd -1.2f0 3.7d0    0.0d0 -1.2d0)
  t)

(deftest fceiling-sd.4c
  (fceiling-check-sd 1.2f0 -3.7d0    0.0d0 1.2d0)
  t)

(deftest fceiling-sd.4d
  (fceiling-check-sd -1.2f0 -3.7d0    1.0d0 2.5d0)
  t)

(deftest fceiling-sd.5a
  (fceiling-check-sd 12.3f0 3.7d0    4.0d0 -2.5d0)
  t)

(deftest fceiling-sd.5b
  (fceiling-check-sd -12.3f0 3.7d0    -3.0d0 -1.2d0)
  t)

(deftest fceiling-sd.5c
  (fceiling-check-sd 12.3f0 -3.7d0    -3.0d0 1.2d0)
  t)

(deftest fceiling-sd.5d
  (fceiling-check-sd -12.3f0 -3.7d0    4.0d0 2.5d0)
  t)


;; single-float - long-float
(defun fceiling-check-sl (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :type 'long-float))

(deftest fceiling-sl.1
  (fceiling 0.0f0 10.0l0)
  0.0l0 0.0l0)

(deftest fceiling-sl.2
  (fceiling 0.0f0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error fceiling-sl.3
  (fceiling 10.4f0 0.0l0)
  division-by-zero)

(deftest fceiling-sl.4a
  (fceiling-check-sl 1.2f0 3.7l0    1.0l0 -2.5l0)
  t)

(deftest fceiling-sl.4b
  (fceiling-check-sl -1.2f0 3.7l0    0.0l0 -1.2l0)
  t)

(deftest fceiling-sl.4c
  (fceiling-check-sl 1.2f0 -3.7l0    0.0l0 1.2l0)
  t)

(deftest fceiling-sl.4d
  (fceiling-check-sl -1.2f0 -3.7l0    1.0l0 2.5l0)
  t)

(deftest fceiling-sl.5a
  (fceiling-check-sl 12.3f0 3.7l0    4.0l0 -2.5l0)
  t)

(deftest fceiling-sl.5b
  (fceiling-check-sl -12.3f0 3.7l0    -3.0l0 -1.2l0)
  t)

(deftest fceiling-sl.5c
  (fceiling-check-sl 12.3f0 -3.7l0    -3.0l0 1.2l0)
  t)

(deftest fceiling-sl.5d
  (fceiling-check-sl -12.3f0 -3.7l0    4.0l0 2.5l0)
  t)


;; double-float - fixnum
(defun fceiling-check-df (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-14
    :type 'double-float))

(deftest fceiling-df.1
  (fceiling 0.0d0 10)
  0.0d0 0.0d0)

(deftest fceiling-df.2
  (fceiling 0.0d0 -10)
  0.0d0 0.0d0)

(deftest-error fceiling-df.3
  (fceiling 10.4d0 0)
  division-by-zero)

(deftest fceiling-df.4a
  (fceiling-check-df 1.2d0 10    1.0d0 -8.8d0)
  t)

(deftest fceiling-df.4b
  (fceiling-check-df -1.2d0 10    0.0f0 -1.2d0)
  t)

(deftest fceiling-df.4c
  (fceiling-check-df 1.2d0 -10    0.0f0 1.2d0)
  t)

(deftest fceiling-df.4d
  (fceiling-check-df -1.2d0 -10    1.0f0 8.8d0)
  t)

(deftest fceiling-df.5a
  (fceiling-check-df 10.2d0 4    3.0f0 -1.8d0)
  t)

(deftest fceiling-df.5b
  (fceiling-check-df -10.2d0 4    -2.0f0 -2.2d0)
  t)

(deftest fceiling-df.5c
  (fceiling-check-df 10.2d0 -4    -2.0f0 2.2d0)
  t)

(deftest fceiling-df.5d
  (fceiling-check-df -10.2d0 -4    3.0f0 1.8d0)
  t)


;; double-float - bignum
(defun fceiling-check-db (a b c d)
  (fceiling-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fceiling-db.1
  (fceiling 0.0d0 (make-bignum 10))
  0.0d0 0.0d0)

(deftest fceiling-db.2
  (fceiling 0.0d0 (make-bignum -10))
  0.0d0 0.0d0)

(deftest-error fceiling-db.3
  (fceiling 10.4d0 (make-bignum 0))
  division-by-zero)

(deftest fceiling-db.4a
  (fceiling-check-db 1.2d0 10    1.0d0 -8.8d0)
  t)

(deftest fceiling-db.4b
  (fceiling-check-db -1.2d0 10    0.0d0 -1.2d0)
  t)

(deftest fceiling-db.4c
  (fceiling-check-db 1.2d0 -10    0.0d0 1.2d0)
  t)

(deftest fceiling-db.4d
  (fceiling-check-db -1.2d0 -10    1.0d0 8.8d0)
  t)

(deftest fceiling-db.5a
  (fceiling-check-db 10.2d0 4    3.0d0 -1.8d0)
  t)

(deftest fceiling-db.5b
  (fceiling-check-db -10.2d0 4    -2.0d0 -2.2d0)
  t)

(deftest fceiling-db.5c
  (fceiling-check-db 10.2d0 -4    -2.0d0 2.2d0)
  t)

(deftest fceiling-db.5d
  (fceiling-check-db -10.2d0 -4    3.0d0 1.8d0)
  t)


;; double-float - ratio
(defun fceiling-check-dr (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fceiling-dr.1
  (fceiling 0.0d0 10/3)
  0.0d0 0.0d0)

(deftest fceiling-dr.2
  (fceiling 0.0d0 -10/3)
  0.0d0 0.0d0)

(deftest-error fceiling-dr.3
  (fceiling 10.4d0 (make-ratio 0 1))
  division-by-zero)

(deftest fceiling-dr.4a
  (fceiling-check-dr 1.2d0 15/4    1.0d0 -2.55d0)
  t)

(deftest fceiling-dr.4b
  (fceiling-check-dr -1.2d0 15/4    0.0d0 -1.2d0)
  t)

(deftest fceiling-dr.4c
  (fceiling-check-dr 1.2d0 -15/4    0.0d0 1.2d0)
  t)

(deftest fceiling-dr.4d
  (fceiling-check-dr -1.2d0 -15/4    1.0d0 2.55d0)
  t)

(deftest fceiling-dr.5a
  (fceiling-check-dr 1.2d0 1/4    5.0d0 -0.05d0)
  t)

(deftest fceiling-dr.5b
  (fceiling-check-dr -1.2d0 1/4    -4.0d0 -0.2d0)
  t)

(deftest fceiling-dr.5c
  (fceiling-check-dr 1.2d0 -1/4    -4.0d0 0.2d0)
  t)

(deftest fceiling-dr.5d
  (fceiling-check-dr -1.2d0 -1/4    5.0d0 0.05d0)
  t)


;; double-float - single-float
(defun fceiling-check-ds (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest fceiling-ds.1
  (fceiling 0.0d0 10.0f0)
  0.0d0 0.0d0)

(deftest fceiling-ds.2
  (fceiling 0.0d0 -10.0f0)
  0.0d0 0.0d0)

(deftest-error fceiling-ds.3
  (fceiling 10.4d0 0.0f0)
  division-by-zero)

(deftest fceiling-ds.4a
  (fceiling-check-ds 1.2d0 3.7f0    1.0d0 -2.5d0)
  t)

(deftest fceiling-ds.4b
  (fceiling-check-ds -1.2d0 3.7f0    0.0d0 -1.2d0)
  t)

(deftest fceiling-ds.4c
  (fceiling-check-ds 1.2d0 -3.7f0    0.0d0 1.2d0)
  t)

(deftest fceiling-ds.4d
  (fceiling-check-ds -1.2d0 -3.7f0    1.0d0 2.5d0)
  t)

(deftest fceiling-ds.5a
  (fceiling-check-ds 12.3d0 3.7f0    4.0d0 -2.5d0)
  t)

(deftest fceiling-ds.5b
  (fceiling-check-ds -12.3d0 3.7f0    -3.0d0 -1.2d0)
  t)

(deftest fceiling-ds.5c
  (fceiling-check-ds 12.3d0 -3.7f0    -3.0d0 1.2d0)
  t)

(deftest fceiling-ds.5d
  (fceiling-check-ds -12.3d0 -3.7f0    4.0d0 2.5d0)
  t)


;; double-float - double-float
(defun fceiling-check-dd (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fceiling-dd.1
  (fceiling 0.0d0 10.0d0)
  0.0d0 0.0d0)

(deftest fceiling-dd.2
  (fceiling 0.0d0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error fceiling-dd.3
  (fceiling 10.4d0 0.0d0)
  division-by-zero)

(deftest fceiling-dd.4a
  (fceiling-check-dd 1.2d0 3.7d0    1.0d0 -2.5d0)
  t)

(deftest fceiling-dd.4b
  (fceiling-check-dd -1.2d0 3.7d0    0.0d0 -1.2d0)
  t)

(deftest fceiling-dd.4c
  (fceiling-check-dd 1.2d0 -3.7d0    0.0d0 1.2d0)
  t)

(deftest fceiling-dd.4d
  (fceiling-check-dd -1.2d0 -3.7d0    1.0d0 2.5d0)
  t)

(deftest fceiling-dd.5a
  (fceiling-check-dd 12.3d0 3.7d0    4.0d0 -2.5d0)
  t)

(deftest fceiling-dd.5b
  (fceiling-check-dd -12.3d0 3.7d0    -3.0d0 -1.2d0)
  t)

(deftest fceiling-dd.5c
  (fceiling-check-dd 12.3d0 -3.7d0    -3.0d0 1.2d0)
  t)

(deftest fceiling-dd.5d
  (fceiling-check-dd -12.3d0 -3.7d0    4.0d0 2.5d0)
  t)


;; double-float - long-float
(defun fceiling-check-dl (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fceiling-dl.1
  (fceiling 0.0d0 10.0l0)
  0.0l0 0.0l0)

(deftest fceiling-dl.2
  (fceiling 0.0d0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error fceiling-dl.3
  (fceiling 10.4d0 0.0l0)
  division-by-zero)

(deftest fceiling-dl.4a
  (fceiling-check-dl 1.2d0 3.7l0    1.0l0 -2.5l0)
  t)

(deftest fceiling-dl.4b
  (fceiling-check-dl -1.2d0 3.7l0    0.0l0 -1.2l0)
  t)

(deftest fceiling-dl.4c
  (fceiling-check-dl 1.2d0 -3.7l0    0.0l0 1.2l0)
  t)

(deftest fceiling-dl.4d
  (fceiling-check-dl -1.2d0 -3.7l0    1.0l0 2.5l0)
  t)

(deftest fceiling-dl.5a
  (fceiling-check-dl 12.3d0 3.7l0    4.0l0 -2.5l0)
  t)

(deftest fceiling-dl.5b
  (fceiling-check-dl -12.3d0 3.7l0    -3.0l0 -1.2l0)
  t)

(deftest fceiling-dl.5c
  (fceiling-check-dl 12.3d0 -3.7l0    -3.0l0 1.2l0)
  t)

(deftest fceiling-dl.5d
  (fceiling-check-dl -12.3d0 -3.7l0    4.0l0 2.5l0)
  t)


;; long-float - fixnum
(defun fceiling-check-lf (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-14
    :type 'long-float))

(deftest fceiling-lf.1
  (fceiling 0.0l0 10)
  0.0l0 0.0l0)

(deftest fceiling-lf.2
  (fceiling 0.0l0 -10)
  0.0l0 0.0l0)

(deftest-error fceiling-lf.3
  (fceiling 10.4l0 0)
  division-by-zero)

(deftest fceiling-lf.4a
  (fceiling-check-lf 1.2l0 10    1.0l0 -8.8l0)
  t)

(deftest fceiling-lf.4b
  (fceiling-check-lf -1.2l0 10    0.0l0 -1.2l0)
  t)

(deftest fceiling-lf.4c
  (fceiling-check-lf 1.2l0 -10    0.0l0 1.2l0)
  t)

(deftest fceiling-lf.4d
  (fceiling-check-lf -1.2l0 -10    1.0l0 8.8l0)
  t)

(deftest fceiling-lf.5a
  (fceiling-check-lf 10.2l0 4    3.0l0 -1.8l0)
  t)

(deftest fceiling-lf.5b
  (fceiling-check-lf -10.2l0 4    -2.0l0 -2.2l0)
  t)

(deftest fceiling-lf.5c
  (fceiling-check-lf 10.2l0 -4    -2.0l0 2.2l0)
  t)

(deftest fceiling-lf.5d
  (fceiling-check-lf -10.2l0 -4    3.0l0 1.8l0)
  t)


;; long-float - bignum
(defun fceiling-check-lb (a b c d)
  (fceiling-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fceiling-lb.1
  (fceiling 0.0l0 (make-bignum 10))
  0.0l0 0.0l0)

(deftest fceiling-lb.2
  (fceiling 0.0l0 (make-bignum -10))
  0.0l0 0.0l0)

(deftest-error fceiling-lb.3
  (fceiling 10.4l0 (make-bignum 0))
  division-by-zero)

(deftest fceiling-lb.4a
  (fceiling-check-lb 1.2l0 10    1.0l0 -8.8l0)
  t)

(deftest fceiling-lb.4b
  (fceiling-check-lb -1.2l0 10    0.0l0 -1.2l0)
  t)

(deftest fceiling-lb.4c
  (fceiling-check-lb 1.2l0 -10    0.0l0 1.2l0)
  t)

(deftest fceiling-lb.4d
  (fceiling-check-lb -1.2l0 -10    1.0l0 8.8l0)
  t)

(deftest fceiling-lb.5a
  (fceiling-check-lb 10.2l0 4    3.0l0 -1.8l0)
  t)

(deftest fceiling-lb.5b
  (fceiling-check-lb -10.2l0 4    -2.0l0 -2.2l0)
  t)

(deftest fceiling-lb.5c
  (fceiling-check-lb 10.2l0 -4    -2.0l0 2.2l0)
  t)

(deftest fceiling-lb.5d
  (fceiling-check-lb -10.2l0 -4    3.0l0 1.8l0)
  t)


;; long-float - ratio
(defun fceiling-check-lr (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fceiling-lr.1
  (fceiling 0.0l0 10/3)
  0.0l0 0.0l0)

(deftest fceiling-lr.2
  (fceiling 0.0l0 -10/3)
  0.0l0 0.0l0)

(deftest-error fceiling-lr.3
  (fceiling 10.4l0 (make-ratio 0 1))
  division-by-zero)

(deftest fceiling-lr.4a
  (fceiling-check-lr 1.2l0 15/4    1.0l0 -2.55l0)
  t)

(deftest fceiling-lr.4b
  (fceiling-check-lr -1.2l0 15/4    0.0l0 -1.2l0)
  t)

(deftest fceiling-lr.4c
  (fceiling-check-lr 1.2l0 -15/4    0.0l0 1.2l0)
  t)

(deftest fceiling-lr.4d
  (fceiling-check-lr -1.2l0 -15/4    1.0l0 2.55l0)
  t)

(deftest fceiling-lr.5a
  (fceiling-check-lr 1.2l0 1/4    5.0l0 -0.05l0)
  t)

(deftest fceiling-lr.5b
  (fceiling-check-lr -1.2l0 1/4    -4.0l0 -0.2l0)
  t)

(deftest fceiling-lr.5c
  (fceiling-check-lr 1.2l0 -1/4    -4.0l0 0.2l0)
  t)

(deftest fceiling-lr.5d
  (fceiling-check-lr -1.2l0 -1/4    5.0l0 0.05l0)
  t)


;; long-float - single-float
(defun fceiling-check-ls (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest fceiling-ls.1
  (fceiling 0.0l0 10.0f0)
  0.0l0 0.0l0)

(deftest fceiling-ls.2
  (fceiling 0.0l0 -10.0f0)
  0.0l0 0.0l0)

(deftest-error fceiling-ls.3
  (fceiling 10.4l0 0.0f0)
  division-by-zero)

(deftest fceiling-ls.4a
  (fceiling-check-ls 1.2l0 3.7f0    1.0l0 -2.5l0)
  t)

(deftest fceiling-ls.4b
  (fceiling-check-ls -1.2l0 3.7f0    0.0l0 -1.2l0)
  t)

(deftest fceiling-ls.4c
  (fceiling-check-ls 1.2l0 -3.7f0    0.0l0 1.2l0)
  t)

(deftest fceiling-ls.4d
  (fceiling-check-ls -1.2l0 -3.7f0    1.0l0 2.5l0)
  t)

(deftest fceiling-ls.5a
  (fceiling-check-ls 12.3l0 3.7f0    4.0l0 -2.5l0)
  t)

(deftest fceiling-ls.5b
  (fceiling-check-ls -12.3l0 3.7f0    -3.0l0 -1.2l0)
  t)

(deftest fceiling-ls.5c
  (fceiling-check-ls 12.3l0 -3.7f0    -3.0l0 1.2l0)
  t)

(deftest fceiling-ls.5d
  (fceiling-check-ls -12.3l0 -3.7f0    4.0l0 2.5l0)
  t)


;; long-float - double-float
(defun fceiling-check-ld (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fceiling-ld.1
  (fceiling 0.0l0 10.0d0)
  0.0l0 0.0l0)

(deftest fceiling-ld.2
  (fceiling 0.0l0 -10.0d0)
  0.0l0 0.0l0)

(deftest-error fceiling-ld.3
  (fceiling 10.4l0 0.0d0)
  division-by-zero)

(deftest fceiling-ld.4a
  (fceiling-check-ld 1.2l0 3.7d0    1.0l0 -2.5l0)
  t)

(deftest fceiling-ld.4b
  (fceiling-check-ld -1.2l0 3.7d0    0.0l0 -1.2l0)
  t)

(deftest fceiling-ld.4c
  (fceiling-check-ld 1.2l0 -3.7d0    0.0l0 1.2l0)
  t)

(deftest fceiling-ld.4d
  (fceiling-check-ld -1.2l0 -3.7d0    1.0l0 2.5l0)
  t)

(deftest fceiling-ld.5a
  (fceiling-check-ld 12.3l0 3.7d0    4.0l0 -2.5l0)
  t)

(deftest fceiling-ld.5b
  (fceiling-check-ld -12.3l0 3.7d0    -3.0l0 -1.2l0)
  t)

(deftest fceiling-ld.5c
  (fceiling-check-ld 12.3l0 -3.7d0    -3.0l0 1.2l0)
  t)

(deftest fceiling-ld.5d
  (fceiling-check-ld -12.3l0 -3.7d0    4.0l0 2.5l0)
  t)


;; long-float - long-float
(defun fceiling-check-ll (a b c d)
  (fceiling-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fceiling-ll.1
  (fceiling 0.0l0 10.0l0)
  0.0l0 0.0l0)

(deftest fceiling-ll.2
  (fceiling 0.0l0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error fceiling-ll.3
  (fceiling 10.4l0 0.0l0)
  division-by-zero)

(deftest fceiling-ll.4a
  (fceiling-check-ll 1.2l0 3.7l0    1.0l0 -2.5l0)
  t)

(deftest fceiling-ll.4b
  (fceiling-check-ll -1.2l0 3.7l0    0.0l0 -1.2l0)
  t)

(deftest fceiling-ll.4c
  (fceiling-check-ll 1.2l0 -3.7l0    0.0l0 1.2l0)
  t)

(deftest fceiling-ll.4d
  (fceiling-check-ll -1.2l0 -3.7l0    1.0l0 2.5l0)
  t)

(deftest fceiling-ll.5a
  (fceiling-check-ll 12.3l0 3.7l0    4.0l0 -2.5l0)
  t)

(deftest fceiling-ll.5b
  (fceiling-check-ll -12.3l0 3.7l0    -3.0l0 -1.2l0)
  t)

(deftest fceiling-ll.5c
  (fceiling-check-ll 12.3l0 -3.7l0    -3.0l0 1.2l0)
  t)

(deftest fceiling-ll.5d
  (fceiling-check-ll -12.3l0 -3.7l0    4.0l0 2.5l0)
  t)

