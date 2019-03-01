;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(defun ceiling-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float) (call #'integerp))
  (multiple-value-bind (e f) (ceiling a b)
    (or (and (funcall call e)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "ceiling-equal error: (~S ~S) ~A, ~A, ~A" e f
          (funcall call e)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  ceiling1
;;
(deftest ceiling1-integer.1
  (ceiling 0)
  0 0)

(deftest ceiling1-integer.2
  (ceiling 10)
  10 0)

(deftest ceiling1-integer.3
  (ceiling -10)
  -10 0)

(deftest ceiling1-integer.4
  (ceiling 99999999999999999999)
  99999999999999999999 0)

(deftest ceiling1-integer.5
  (ceiling -99999999999999999999)
  -99999999999999999999 0)

(deftest ceiling1-ratio.1
  (ceiling 1/3)
  1 -2/3)

(deftest ceiling1-ratio.2
  (ceiling 10/3)
  4 -2/3)

(deftest ceiling1-ratio.3
  (ceiling -1/3)
  0 -1/3)

(deftest ceiling1-ratio.4
  (ceiling -10/3)
  -3 -1/3)

(deftest ceiling1-float.1
  (ceiling 10.0)
  10 0.0)

(deftest ceiling1-float.2
  (ceiling -10.0)
  -10 0.0)

(deftest ceiling1-float.3
  (ceiling 12.25)
  13 -0.75)

(deftest ceiling1-float.4
  (ceiling -12.25)
  -12 -0.25)

(deftest ceiling1-float.5
  (ceiling 12.25f0)
  13 -0.75f0)

(deftest ceiling1-float.6
  (ceiling -12.25f0)
  -12 -0.25f0)

(deftest ceiling1-float.7
  (ceiling 12.25d0)
  13 -0.75d0)

(deftest ceiling1-float.8
  (ceiling -12.25d0)
  -12 -0.25d0)

(deftest ceiling1-float.9
  (ceiling 12.25l0)
  13 -0.75l0)

(deftest ceiling1-float.10
  (ceiling -12.25l0)
  -12 -0.25l0)


;;
;;  ceiling
;;

;;  fixnum - fixnum
(deftest ceiling-ff.1
  (ceiling 0 10)
  0 0)

(deftest ceiling-ff.2
  (ceiling 0 -10)
  0 0)

(deftest-error ceiling-ff.3
  (ceiling 10 0)
  division-by-zero)

(deftest ceiling-ff.4a
  (ceiling 3 10)
  1 -7)

(deftest ceiling-ff.4b
  (ceiling -3 10)
  0 -3)

(deftest ceiling-ff.4c
  (ceiling 3 -10)
  0 3)

(deftest ceiling-ff.4d
  (ceiling -3 -10)
  1 7)

(deftest ceiling-ff.5a
  (ceiling 10 3)
  4 -2)

(deftest ceiling-ff.5b
  (ceiling -10 3)
  -3 -1)

(deftest ceiling-ff.5c
  (ceiling 10 -3)
  -3 1)

(deftest ceiling-ff.5d
  (ceiling -10 -3)
  4 2)

(deftest ceiling-ff.6a
  (ceiling 11 3)
  4 -1)

(deftest ceiling-ff.6b
  (ceiling -11 3)
  -3 -2)

(deftest ceiling-ff.6c
  (ceiling 11 -3)
  -3 2)

(deftest ceiling-ff.6d
  (ceiling -11 -3)
  4 1)


;; fixnum - bignum
(deftest ceiling-fb.1
  (ceiling 0 (make-bignum 10))
  0 0)

(deftest ceiling-fb.2
  (ceiling 0 (make-bignum -10))
  0 0)

(deftest-error ceiling-fb.3
  (ceiling 10 (make-bignum 0))
  division-by-zero)

(deftest ceiling-fb.4a
  (ceiling 3 (make-bignum 10))
  1 -7)

(deftest ceiling-fb.4b
  (ceiling -3 (make-bignum 10))
  0 -3)

(deftest ceiling-fb.4c
  (ceiling 3 (make-bignum -10))
  0 3)

(deftest ceiling-fb.4d
  (ceiling -3 (make-bignum -10))
  1 7)

(deftest ceiling-fb.5a
  (ceiling 10 (make-bignum 3))
  4 -2)

(deftest ceiling-fb.5b
  (ceiling -10 (make-bignum 3))
  -3 -1)

(deftest ceiling-fb.5c
  (ceiling 10 (make-bignum -3))
  -3 1)

(deftest ceiling-fb.5d
  (ceiling -10 (make-bignum -3))
  4 2)

(deftest ceiling-fb.6a
  (ceiling 11 (make-bignum 3))
  4 -1)

(deftest ceiling-fb.6b
  (ceiling -11 (make-bignum 3))
  -3 -2)

(deftest ceiling-fb.6c
  (ceiling 11 (make-bignum -3))
  -3 2)

(deftest ceiling-fb.6d
  (ceiling -11 (make-bignum -3))
  4 1)


;;  fixnum - ratio
(deftest ceiling-fr.1
  (ceiling 0 10/3)
  0 0)

(deftest ceiling-fr.2
  (ceiling 0 -10/3)
  0 0)

(deftest-error ceiling-fr.3
  (ceiling 10 (make-ratio 0 1))
  division-by-zero)

(deftest ceiling-fr.4a
  (ceiling 3 100/7)
  1 -79/7)

(deftest ceiling-fr.4b
  (ceiling -3 100/7)
  0 -3)

(deftest ceiling-fr.4c
  (ceiling 3 -100/7)
  0 3)

(deftest ceiling-fr.4d
  (ceiling -3 -100/7)
  1 79/7)

(deftest ceiling-fr.5a
  (ceiling 10 6/7)
  12 -2/7)

(deftest ceiling-fr.5b
  (ceiling -10 6/7)
  -11 -4/7)

(deftest ceiling-fr.5c
  (ceiling 10 -6/7)
  -11 4/7)

(deftest ceiling-fr.5d
  (ceiling -10 -6/7)
  12 2/7)

(deftest ceiling-fr.6a
  (ceiling 10 1/3)
  30 0)

(deftest ceiling-fr.6b
  (ceiling -10 1/3)
  -30 0)

(deftest ceiling-fr.6c
  (ceiling 10 -1/3)
  -30 0)

(deftest ceiling-fr.6d
  (ceiling -10 -1/3)
  30 0)

(deftest ceiling-fr.7a
  (ceiling 10 4/3)
  8 -2/3)


;;  fixnum - single-float
(defun ceiling-check-fs (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest ceiling-fs.1
  (ceiling 0 10.0f0)
  0 0.0f0)

(deftest ceiling-fs.2
  (ceiling 0 -10.0f0)
  0 0.0f0)

(deftest-error ceiling-fs.3
  (ceiling 10 0.0f0)
  division-by-zero)

(deftest ceiling-fs.4a
  (ceiling-check-fs 4 10.5f0    1 -6.5f0)
  t)

(deftest ceiling-fs.4b
  (ceiling-check-fs -4 10.5f0    0 -4.0f0)
  t)

(deftest ceiling-fs.4c
  (ceiling-check-fs 4 -10.5f0    0 4.0f0)
  t)

(deftest ceiling-fs.4d
  (ceiling-check-fs -4 -10.5f0    1 6.5f0)
  t)

(deftest ceiling-fs.5a
  (ceiling-check-fs 15 1.6f0    10 -1.0f0)
  t)

(deftest ceiling-fs.5b
  (ceiling-check-fs -15 1.6f0    -9 -0.6f0)
  t)

(deftest ceiling-fs.5c
  (ceiling-check-fs 15 -1.6f0    -9 0.6f0)
  t)

(deftest ceiling-fs.5d
  (ceiling-check-fs -15 -1.6f0    10 1.0f0)
  t)


;;  fixnum - double-float
(defun ceiling-check-fd (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest ceiling-fd.1
  (ceiling 0 10.0d0)
  0 0.0d0)

(deftest ceiling-fd.2
  (ceiling 0 -10.0d0)
  0 0.0d0)

(deftest-error ceiling-fd.3
  (ceiling 10 0.0d0)
  division-by-zero)

(deftest ceiling-fd.4a
  (ceiling-check-fd 4 10.5d0    1 -6.5d0)
  t)

(deftest ceiling-fd.4b
  (ceiling-check-fd -4 10.5d0    0 -4.0d0)
  t)

(deftest ceiling-fd.4c
  (ceiling-check-fd 4 -10.5d0    0 4.0d0)
  t)

(deftest ceiling-fd.4d
  (ceiling-check-fd -4 -10.5d0    1 6.5d0)
  t)

(deftest ceiling-fd.5a
  (ceiling-check-fd 15 1.6d0    10 -1.0d0)
  t)

(deftest ceiling-fd.5b
  (ceiling-check-fd -15 1.6d0    -9 -0.6d0)
  t)

(deftest ceiling-fd.5c
  (ceiling-check-fd 15 -1.6d0    -9 0.6d0)
  t)

(deftest ceiling-fd.5d
  (ceiling-check-fd -15 -1.6d0    10 1.0d0)
  t)


;;  fixnum - long-float
(defun ceiling-check-fl (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest ceiling-fl.1
  (ceiling 0 10.0l0)
  0 0.0l0)

(deftest ceiling-fl.2
  (ceiling 0 -10.0l0)
  0 0.0l0)

(deftest-error ceiling-fl.3
  (ceiling 10 0.0l0)
  division-by-zero)

(deftest ceiling-fl.4a
  (ceiling-check-fl 4 10.5l0    1 -6.5l0)
  t)

(deftest ceiling-fl.4b
  (ceiling-check-fl -4 10.5l0    0 -4.0l0)
  t)

(deftest ceiling-fl.4c
  (ceiling-check-fl 4 -10.5l0    0 4.0l0)
  t)

(deftest ceiling-fl.4d
  (ceiling-check-fl -4 -10.5l0    1 6.5l0)
  t)

(deftest ceiling-fl.5a
  (ceiling-check-fl 15 1.6l0    10 -1.0l0)
  t)

(deftest ceiling-fl.5b
  (ceiling-check-fl -15 1.6l0    -9 -0.6l0)
  t)

(deftest ceiling-fl.5c
  (ceiling-check-fl 15 -1.6l0    -9 0.6l0)
  t)

(deftest ceiling-fl.5d
  (ceiling-check-fl -15 -1.6l0    10 1.0l0)
  t)


;;  bignum - fixnum
(defun ceilingb (a b)
  (ceiling (make-bignum a) b))

(deftest ceiling-bf.1
  (ceilingb 0 10)
  0 0)

(deftest ceiling-bf.2
  (ceilingb 0 -10)
  0 0)

(deftest-error ceiling-bf.3
  (ceilingb 10 0)
  division-by-zero)

(deftest ceiling-bf.4a
  (ceilingb 3 10)
  1 -7)

(deftest ceiling-bf.4b
  (ceilingb -3 10)
  0 -3)

(deftest ceiling-bf.4c
  (ceilingb 3 -10)
  0 3)

(deftest ceiling-bf.4d
  (ceilingb -3 -10)
  1 7)

(deftest ceiling-bf.5a
  (ceilingb 10 3)
  4 -2)

(deftest ceiling-bf.5b
  (ceilingb -10 3)
  -3 -1)

(deftest ceiling-bf.5c
  (ceilingb 10 -3)
  -3 1)

(deftest ceiling-bf.5d
  (ceilingb -10 -3)
  4 2)

(deftest ceiling-bf.6a
  (ceilingb 11 3)
  4 -1)

(deftest ceiling-bf.6b
  (ceilingb -11 3)
  -3 -2)

(deftest ceiling-bf.6c
  (ceilingb 11 -3)
  -3 2)

(deftest ceiling-bf.6d
  (ceilingb -11 -3)
  4 1)


;; bignum - bignum
(deftest ceiling-bb.1
  (ceilingb 0 (make-bignum 10))
  0 0)

(deftest ceiling-bb.2
  (ceilingb 0 (make-bignum -10))
  0 0)

(deftest-error ceiling-bb.3
  (ceilingb 10 (make-bignum 0))
  division-by-zero)

(deftest ceiling-bb.4a
  (ceilingb 3 (make-bignum 10))
  1 -7)

(deftest ceiling-bb.4b
  (ceilingb -3 (make-bignum 10))
  0 -3)

(deftest ceiling-bb.4c
  (ceilingb 3 (make-bignum -10))
  0 3)

(deftest ceiling-bb.4d
  (ceilingb -3 (make-bignum -10))
  1 7)

(deftest ceiling-bb.5a
  (ceilingb 10 (make-bignum 3))
  4 -2)

(deftest ceiling-bb.5b
  (ceilingb -10 (make-bignum 3))
  -3 -1)

(deftest ceiling-bb.5c
  (ceilingb 10 (make-bignum -3))
  -3 1)

(deftest ceiling-bb.5d
  (ceilingb -10 (make-bignum -3))
  4 2)

(deftest ceiling-bb.6a
  (ceilingb 11 (make-bignum 3))
  4 -1)

(deftest ceiling-bb.6b
  (ceilingb -11 (make-bignum 3))
  -3 -2)

(deftest ceiling-bb.6c
  (ceilingb 11 (make-bignum -3))
  -3 2)

(deftest ceiling-bb.6d
  (ceilingb -11 (make-bignum -3))
  4 1)


;;  bignum - ratio
(deftest ceiling-br.1
  (ceilingb 0 10/3)
  0 0)

(deftest ceiling-br.2
  (ceilingb 0 -10/3)
  0 0)

(deftest-error ceiling-br.3
  (ceilingb 10 (make-ratio 0 1))
  division-by-zero)

(deftest ceiling-br.4a
  (ceilingb 3 100/7)
  1 -79/7)

(deftest ceiling-br.4b
  (ceilingb -3 100/7)
  0 -3)

(deftest ceiling-br.4c
  (ceilingb 3 -100/7)
  0 3)

(deftest ceiling-br.4d
  (ceilingb -3 -100/7)
  1 79/7)

(deftest ceiling-br.5a
  (ceilingb 10 6/7)
  12 -2/7)

(deftest ceiling-br.5b
  (ceilingb -10 6/7)
  -11 -4/7)

(deftest ceiling-br.5c
  (ceilingb 10 -6/7)
  -11 4/7)

(deftest ceiling-br.5d
  (ceilingb -10 -6/7)
  12 2/7)

(deftest ceiling-br.6a
  (ceilingb 10 1/3)
  30 0)

(deftest ceiling-br.6b
  (ceilingb -10 1/3)
  -30 0)

(deftest ceiling-br.6c
  (ceilingb 10 -1/3)
  -30 0)

(deftest ceiling-br.6d
  (ceilingb -10 -1/3)
  30 0)


;;  bignum - single-float
(defun ceiling-check-bs (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest ceiling-bs.1
  (ceilingb 0 10.0f0)
  0 0.0f0)

(deftest ceiling-bs.2
  (ceilingb 0 -10.0f0)
  0 0.0f0)

(deftest-error ceiling-bs.3
  (ceilingb 10 0.0f0)
  division-by-zero)

(deftest ceiling-bs.4a
  (ceiling-check-bs 4 10.5f0    1 -6.5f0)
  t)

(deftest ceiling-bs.4b
  (ceiling-check-bs -4 10.5f0    0 -4.0f0)
  t)

(deftest ceiling-bs.4c
  (ceiling-check-bs 4 -10.5f0    0 4.0f0)
  t)

(deftest ceiling-bs.4d
  (ceiling-check-bs -4 -10.5f0    1 6.5f0)
  t)

(deftest ceiling-bs.5a
  (ceiling-check-bs 15 1.6f0    10 -1.0f0)
  t)

(deftest ceiling-bs.5b
  (ceiling-check-bs -15 1.6f0    -9 -0.6f0)
  t)

(deftest ceiling-bs.5c
  (ceiling-check-bs 15 -1.6f0    -9 0.6f0)
  t)

(deftest ceiling-bs.5d
  (ceiling-check-bs -15 -1.6f0    10 1.0f0)
  t)


;;  bignum - double-float
(defun ceiling-check-bd (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest ceiling-bd.1
  (ceilingb 0 10.0d0)
  0 0.0d0)

(deftest ceiling-bd.2
  (ceilingb 0 -10.0d0)
  0 0.0d0)

(deftest-error ceiling-bd.3
  (ceilingb 10 0.0d0)
  division-by-zero)

(deftest ceiling-bd.4a
  (ceiling-check-bd 4 10.5d0    1 -6.5d0)
  t)

(deftest ceiling-bd.4b
  (ceiling-check-bd -4 10.5d0    0 -4.0d0)
  t)

(deftest ceiling-bd.4c
  (ceiling-check-bd 4 -10.5d0    0 4.0d0)
  t)

(deftest ceiling-bd.4d
  (ceiling-check-bd -4 -10.5d0    1 6.5d0)
  t)

(deftest ceiling-bd.5a
  (ceiling-check-bd 15 1.6d0    10 -1.0d0)
  t)

(deftest ceiling-bd.5b
  (ceiling-check-bd -15 1.6d0    -9 -0.6d0)
  t)

(deftest ceiling-bd.5c
  (ceiling-check-bd 15 -1.6d0    -9 0.6d0)
  t)

(deftest ceiling-bd.5d
  (ceiling-check-bd -15 -1.6d0    10 1.0d0)
  t)


;;  bignum - long-float
(defun ceiling-check-bl (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest ceiling-bl.1
  (ceilingb 0 10.0l0)
  0 0.0l0)

(deftest ceiling-bl.2
  (ceilingb 0 -10.0l0)
  0 0.0l0)

(deftest-error ceiling-bl.3
  (ceilingb 10 0.0l0)
  division-by-zero)

(deftest ceiling-bl.4a
  (ceiling-check-bl 4 10.5l0    1 -6.5l0)
  t)

(deftest ceiling-bl.4b
  (ceiling-check-bl -4 10.5l0    0 -4.0l0)
  t)

(deftest ceiling-bl.4c
  (ceiling-check-bl 4 -10.5l0    0 4.0l0)
  t)

(deftest ceiling-bl.4d
  (ceiling-check-bl -4 -10.5l0    1 6.5l0)
  t)

(deftest ceiling-bl.5a
  (ceiling-check-bl 15 1.6l0    10 -1.0l0)
  t)

(deftest ceiling-bl.5b
  (ceiling-check-bl -15 1.6l0    -9 -0.6l0)
  t)

(deftest ceiling-bl.5c
  (ceiling-check-bl 15 -1.6l0    -9 0.6l0)
  t)

(deftest ceiling-bl.5d
  (ceiling-check-bl -15 -1.6l0    10 1.0l0)
  t)


;;  ratio - fixnum
(deftest ceiling-rf.1
  (ceiling (make-ratio 0 1) 10)
  0 0)

(deftest ceiling-rf.2
  (ceiling (make-ratio 0 1) -10)
  0 0)

(deftest-error ceiling-rf.3
  (ceiling 10/3 0)
  division-by-zero)

(deftest ceiling-rf.4a
  (ceiling 2/3 5)
  1 -13/3)

(deftest ceiling-rf.4b
  (ceiling -2/3 5)
  0 -2/3)

(deftest ceiling-rf.4c
  (ceiling 2/3 -5)
  0 2/3)

(deftest ceiling-rf.4d
  (ceiling -2/3 -5)
  1 13/3)

(deftest ceiling-rf.5a
  (ceiling 20/3 5)
  2 -10/3)

(deftest ceiling-rf.5b
  (ceiling -20/3 5)
  -1 -5/3)

(deftest ceiling-rf.5c
  (ceiling 20/3 -5)
  -1 5/3)

(deftest ceiling-rf.5d
  (ceiling -20/3 -5)
  2 10/3)

(deftest ceiling-rf.6a
  (ceiling 53/3 8)
  3 -19/3)

(deftest ceiling-rf.6b
  (ceiling -53/3 8)
  -2 -5/3)

(deftest ceiling-rf.6c
  (ceiling 53/3 -8)
  -2 5/3)

(deftest ceiling-rf.6d
  (ceiling -53/3 -8)
  3 19/3)


;; ratio - bignum
(deftest ceiling-rb.1
  (ceiling (make-ratio 0 1) (make-bignum 10))
  0 0)

(deftest ceiling-rb.2
  (ceiling (make-ratio 0 1) (make-bignum -10))
  0 0)

(deftest-error ceiling-rb.3
  (ceiling 10/3 (make-bignum 0))
  division-by-zero)

(deftest ceiling-rb.4a
  (ceiling 2/3 (make-bignum 5))
  1 -13/3)

(deftest ceiling-rb.4b
  (ceiling -2/3 (make-bignum 5))
  0 -2/3)

(deftest ceiling-rb.4c
  (ceiling 2/3 (make-bignum -5))
  0 2/3)

(deftest ceiling-rb.4d
  (ceiling -2/3 (make-bignum -5))
  1 13/3)

(deftest ceiling-rb.5a
  (ceiling 20/3 (make-bignum 5))
  2 -10/3)

(deftest ceiling-rb.5b
  (ceiling -20/3 (make-bignum 5))
  -1 -5/3)

(deftest ceiling-rb.5c
  (ceiling 20/3 (make-bignum -5))
  -1 5/3)

(deftest ceiling-rb.5d
  (ceiling -20/3 (make-bignum -5))
  2 10/3)

(deftest ceiling-rb.6a
  (ceiling 53/3 (make-bignum 8))
  3 -19/3)

(deftest ceiling-rb.6b
  (ceiling -53/3 (make-bignum 8))
  -2 -5/3)

(deftest ceiling-rb.6c
  (ceiling 53/3 (make-bignum -8))
  -2 5/3)

(deftest ceiling-rb.6d
  (ceiling -53/3 (make-bignum -8))
  3 19/3)


;;  ratio - ratio
(deftest ceiling-rr.1
  (ceiling (make-ratio 0 1) 10/3)
  0 0)

(deftest ceiling-rr.2
  (ceiling (make-ratio 0 1) -10/3)
  0 0)

(deftest-error ceiling-rr.3
  (ceiling 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest ceiling-rr.4a
  (ceiling 1/2 33/5)
  1 -61/10)

(deftest ceiling-rr.4b
  (ceiling -1/2 33/5)
  0 -1/2)

(deftest ceiling-rr.4c
  (ceiling 1/2 -33/5)
  0 1/2)

(deftest ceiling-rr.4d
  (ceiling -1/2 -33/5)
  1 61/10)

(deftest ceiling-rr.5a
  (ceiling 79/3 2/7)
  93 -5/21)

(deftest ceiling-rr.5b
  (ceiling -79/3 2/7)
  -92 -1/21)

(deftest ceiling-rr.5c
  (ceiling 79/3 -2/7)
  -92 1/21)

(deftest ceiling-rr.5d
  (ceiling -79/3 -2/7)
  93 5/21)

(deftest ceiling-rr.6a
  (ceiling 4/5 8/20)
  2 0)

(deftest ceiling-rr.6b
  (ceiling -4/5 8/20)
  -2 0)

(deftest ceiling-rr.6c
  (ceiling 4/5 -8/20)
  -2 0)

(deftest ceiling-rr.6d
  (ceiling -4/5 -8/20)
  2 0)


;;  ratio - single-float
(defun ceiling-check-rs (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest ceiling-rs.1
  (ceiling (make-ratio 0 1) 10.0f0)
  0 0.0f0)

(deftest ceiling-rs.2
  (ceiling (make-ratio 0 1) -10.0f0)
  0 0.0f0)

(deftest-error ceiling-rs.3
  (ceiling 10/4 0.0f0)
  division-by-zero)

(deftest ceiling-rs.4a
  (ceiling-check-rs 3/4 5.5f0    1 -4.75f0)
  t)

(deftest ceiling-rs.4b
  (ceiling-check-rs -3/4 5.5f0    0 -0.75f0)
  t)

(deftest ceiling-rs.4c
  (ceiling-check-rs 3/4 -5.5f0    0 0.75f0)
  t)

(deftest ceiling-rs.4d
  (ceiling-check-rs -3/4 -5.5f0    1 4.75f0)
  t)

(deftest ceiling-rs.5a
  (ceiling-check-rs 77/4 1.6f0    13 -1.55f0)
  t)

(deftest ceiling-rs.5b
  (ceiling-check-rs -77/4 1.6f0    -12 -0.05f0)
  t)

(deftest ceiling-rs.5c
  (ceiling-check-rs 77/4 -1.6f0    -12 0.05f0)
  t)

(deftest ceiling-rs.5d
  (ceiling-check-rs -77/4 -1.6f0    13 1.55f0)
  t)


;;  ratio - double-float
(defun ceiling-check-rd (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest ceiling-rd.1
  (ceiling (make-ratio 0 1) 10.0d0)
  0 0.0d0)

(deftest ceiling-rd.2
  (ceiling (make-ratio 0 1) -10.0d0)
  0 0.0d0)

(deftest-error ceiling-rd.3
  (ceiling 10/4 0.0d0)
  division-by-zero)

(deftest ceiling-rd.4a
  (ceiling-check-rd 3/4 5.5d0    1 -4.75d0)
  t)

(deftest ceiling-rd.4b
  (ceiling-check-rd -3/4 5.5d0    0 -0.75d0)
  t)

(deftest ceiling-rd.4c
  (ceiling-check-rd 3/4 -5.5d0    0 0.75d0)
  t)

(deftest ceiling-rd.4d
  (ceiling-check-rd -3/4 -5.5d0    1 4.75d0)
  t)

(deftest ceiling-rd.5a
  (ceiling-check-rd 77/4 1.6d0    13 -1.55d0)
  t)

(deftest ceiling-rd.5b
  (ceiling-check-rd -77/4 1.6d0    -12 -0.05d0)
  t)

(deftest ceiling-rd.5c
  (ceiling-check-rd 77/4 -1.6d0    -12 0.05d0)
  t)

(deftest ceiling-rd.5d
  (ceiling-check-rd -77/4 -1.6d0    13 1.55d0)
  t)


;;  ratio - long-float
(defun ceiling-check-rl (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-rl.1
  (ceiling (make-ratio 0 1) 10.0l0)
  0 0.0l0)

(deftest ceiling-rl.2
  (ceiling (make-ratio 0 1) -10.0l0)
  0 0.0l0)

(deftest-error ceiling-rl.3
  (ceiling 10/4 0.0l0)
  division-by-zero)

(deftest ceiling-rl.4a
  (ceiling-check-rl 3/4 5.5l0    1 -4.75l0)
  t)

(deftest ceiling-rl.4b
  (ceiling-check-rl -3/4 5.5l0    0 -0.75l0)
  t)

(deftest ceiling-rl.4c
  (ceiling-check-rl 3/4 -5.5l0    0 0.75l0)
  t)

(deftest ceiling-rl.4d
  (ceiling-check-rl -3/4 -5.5l0    1 4.75l0)
  t)

(deftest ceiling-rl.5a
  (ceiling-check-rl 77/4 1.6l0    13 -1.55l0)
  t)

(deftest ceiling-rl.5b
  (ceiling-check-rl -77/4 1.6l0    -12 -0.05l0)
  t)

(deftest ceiling-rl.5c
  (ceiling-check-rl 77/4 -1.6l0    -12 0.05l0)
  t)

(deftest ceiling-rl.5d
  (ceiling-check-rl -77/4 -1.6l0    13 1.55l0)
  t)


;; single-float - fixnum
(defun ceiling-check-sf (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest ceiling-sf.1
  (ceiling 0.0f0 10)
  0 0.0f0)

(deftest ceiling-sf.2
  (ceiling 0.0f0 -10)
  0 0.0f0)

(deftest-error ceiling-sf.3
  (ceiling 10.4f0 0)
  division-by-zero)

(deftest ceiling-sf.4a
  (ceiling-check-sf 1.2f0 10    1 -8.8f0)
  t)

(deftest ceiling-sf.4b
  (ceiling-check-sf -1.2f0 10    0 -1.2f0)
  t)

(deftest ceiling-sf.4c
  (ceiling-check-sf 1.2f0 -10    0 1.2f0)
  t)

(deftest ceiling-sf.4d
  (ceiling-check-sf -1.2f0 -10    1 8.8f0)
  t)

(deftest ceiling-sf.5a
  (ceiling-check-sf 10.2f0 4    3 -1.8f0)
  t)

(deftest ceiling-sf.5b
  (ceiling-check-sf -10.2f0 4    -2 -2.2f0)
  t)

(deftest ceiling-sf.5c
  (ceiling-check-sf 10.2f0 -4    -2 2.2f0)
  t)

(deftest ceiling-sf.5d
  (ceiling-check-sf -10.2f0 -4    3 1.8f0)
  t)


;; single-float - bignum
(defun ceiling-check-sb (a b c d)
  (ceiling-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest ceiling-sb.1
  (ceiling 0.0f0 (make-bignum 10))
  0 0.0f0)

(deftest ceiling-sb.2
  (ceiling 0.0f0 (make-bignum -10))
  0 0.0f0)

(deftest-error ceiling-sb.3
  (ceiling 10.4f0 (make-bignum 0))
  division-by-zero)

(deftest ceiling-sb.4a
  (ceiling-check-sb 1.2f0 10    1 -8.8f0)
  t)

(deftest ceiling-sb.4b
  (ceiling-check-sb -1.2f0 10    0 -1.2f0)
  t)

(deftest ceiling-sb.4c
  (ceiling-check-sb 1.2f0 -10    0 1.2f0)
  t)

(deftest ceiling-sb.4d
  (ceiling-check-sb -1.2f0 -10    1 8.8f0)
  t)

(deftest ceiling-sb.5a
  (ceiling-check-sb 10.2f0 4    3 -1.8f0)
  t)

(deftest ceiling-sb.5b
  (ceiling-check-sb -10.2f0 4    -2 -2.2f0)
  t)

(deftest ceiling-sb.5c
  (ceiling-check-sb 10.2f0 -4    -2 2.2f0)
  t)

(deftest ceiling-sb.5d
  (ceiling-check-sb -10.2f0 -4    3 1.8f0)
  t)


;; single-float - ratio
(defun ceiling-check-sr (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest ceiling-sr.1
  (ceiling 0.0f0 10/3)
  0 0.0f0)

(deftest ceiling-sr.2
  (ceiling 0.0f0 -10/3)
  0 0.0f0)

(deftest-error ceiling-sr.3
  (ceiling 10.4f0 (make-ratio 0 1))
  division-by-zero)

(deftest ceiling-sr.4a
  (ceiling-check-sr 1.2f0 15/4    1 -2.55f0)
  t)

(deftest ceiling-sr.4b
  (ceiling-check-sr -1.2f0 15/4    0 -1.2f0)
  t)

(deftest ceiling-sr.4c
  (ceiling-check-sr 1.2f0 -15/4    0 1.2f0)
  t)

(deftest ceiling-sr.4d
  (ceiling-check-sr -1.2f0 -15/4    1 2.55f0)
  t)

(deftest ceiling-sr.5a
  (ceiling-check-sr 1.2f0 1/4    5 -0.05f0)
  t)

(deftest ceiling-sr.5b
  (ceiling-check-sr -1.2f0 1/4    -4 -0.2f0)
  t)

(deftest ceiling-sr.5c
  (ceiling-check-sr 1.2f0 -1/4    -4 0.2f0)
  t)

(deftest ceiling-sr.5d
  (ceiling-check-sr -1.2f0 -1/4    5 0.05f0)
  t)


;; single-float - single-float
(defun ceiling-check-ss (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest ceiling-ss.1
  (ceiling 0.0f0 10.0f0)
  0 0.0f0)

(deftest ceiling-ss.2
  (ceiling 0.0f0 -10.0f0)
  0 0.0f0)

(deftest-error ceiling-ss.3
  (ceiling 10.4f0 0.0f0)
  division-by-zero)

(deftest ceiling-ss.4a
  (ceiling-check-ss 1.2f0 3.7f0    1 -2.5f0)
  t)

(deftest ceiling-ss.4b
  (ceiling-check-ss -1.2f0 3.7f0    0 -1.2f0)
  t)

(deftest ceiling-ss.4c
  (ceiling-check-ss 1.2f0 -3.7f0    0 1.2f0)
  t)

(deftest ceiling-ss.4d
  (ceiling-check-ss -1.2f0 -3.7f0    1 2.5f0)
  t)

(deftest ceiling-ss.5a
  (ceiling-check-ss 12.3f0 3.7f0    4 -2.5f0)
  t)

(deftest ceiling-ss.5b
  (ceiling-check-ss -12.3f0 3.7f0    -3 -1.2f0)
  t)

(deftest ceiling-ss.5c
  (ceiling-check-ss 12.3f0 -3.7f0    -3 1.2f0)
  t)

(deftest ceiling-ss.5d
  (ceiling-check-ss -12.3f0 -3.7f0    4 2.5f0)
  t)


;; single-float - double-float
(defun ceiling-check-sd (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :call #'integerp
    :type 'double-float))

(deftest ceiling-sd.1
  (ceiling 0.0f0 10.0d0)
  0 0.0d0)

(deftest ceiling-sd.2
  (ceiling 0.0f0 -10.0d0)
  0 0.0d0)

(deftest-error ceiling-sd.3
  (ceiling 10.4f0 0.0d0)
  division-by-zero)

(deftest ceiling-sd.4a
  (ceiling-check-sd 1.2f0 3.7d0    1 -2.5d0)
  t)

(deftest ceiling-sd.4b
  (ceiling-check-sd -1.2f0 3.7d0    0 -1.2d0)
  t)

(deftest ceiling-sd.4c
  (ceiling-check-sd 1.2f0 -3.7d0    0 1.2d0)
  t)

(deftest ceiling-sd.4d
  (ceiling-check-sd -1.2f0 -3.7d0    1 2.5d0)
  t)

(deftest ceiling-sd.5a
  (ceiling-check-sd 12.3f0 3.7d0    4 -2.5d0)
  t)

(deftest ceiling-sd.5b
  (ceiling-check-sd -12.3f0 3.7d0    -3 -1.2d0)
  t)

(deftest ceiling-sd.5c
  (ceiling-check-sd 12.3f0 -3.7d0    -3 1.2d0)
  t)

(deftest ceiling-sd.5d
  (ceiling-check-sd -12.3f0 -3.7d0    4 2.5d0)
  t)


;; single-float - long-float
(defun ceiling-check-sl (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :call #'integerp
    :type 'long-float))

(deftest ceiling-sl.1
  (ceiling 0.0f0 10.0l0)
  0 0.0l0)

(deftest ceiling-sl.2
  (ceiling 0.0f0 -10.0l0)
  0 0.0l0)

(deftest-error ceiling-sl.3
  (ceiling 10.4f0 0.0l0)
  division-by-zero)

(deftest ceiling-sl.4a
  (ceiling-check-sl 1.2f0 3.7l0    1 -2.5l0)
  t)

(deftest ceiling-sl.4b
  (ceiling-check-sl -1.2f0 3.7l0    0 -1.2l0)
  t)

(deftest ceiling-sl.4c
  (ceiling-check-sl 1.2f0 -3.7l0    0 1.2l0)
  t)

(deftest ceiling-sl.4d
  (ceiling-check-sl -1.2f0 -3.7l0    1 2.5l0)
  t)

(deftest ceiling-sl.5a
  (ceiling-check-sl 12.3f0 3.7l0    4 -2.5l0)
  t)

(deftest ceiling-sl.5b
  (ceiling-check-sl -12.3f0 3.7l0    -3 -1.2l0)
  t)

(deftest ceiling-sl.5c
  (ceiling-check-sl 12.3f0 -3.7l0    -3 1.2l0)
  t)

(deftest ceiling-sl.5d
  (ceiling-check-sl -12.3f0 -3.7l0    4 2.5l0)
  t)


;; double-float - fixnum
(defun ceiling-check-df (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-14
    :call #'integerp
    :type 'double-float))

(deftest ceiling-df.1
  (ceiling 0.0d0 10)
  0 0.0d0)

(deftest ceiling-df.2
  (ceiling 0.0d0 -10)
  0 0.0d0)

(deftest-error ceiling-df.3
  (ceiling 10.4d0 0)
  division-by-zero)

(deftest ceiling-df.4a
  (ceiling-check-df 1.2d0 10    1 -8.8d0)
  t)

(deftest ceiling-df.4b
  (ceiling-check-df -1.2d0 10    0 -1.2d0)
  t)

(deftest ceiling-df.4c
  (ceiling-check-df 1.2d0 -10    0 1.2d0)
  t)

(deftest ceiling-df.4d
  (ceiling-check-df -1.2d0 -10    1 8.8d0)
  t)

(deftest ceiling-df.5a
  (ceiling-check-df 10.2d0 4    3 -1.8d0)
  t)

(deftest ceiling-df.5b
  (ceiling-check-df -10.2d0 4    -2 -2.2d0)
  t)

(deftest ceiling-df.5c
  (ceiling-check-df 10.2d0 -4    -2 2.2d0)
  t)

(deftest ceiling-df.5d
  (ceiling-check-df -10.2d0 -4    3 1.8d0)
  t)


;; double-float - bignum
(defun ceiling-check-db (a b c d)
  (ceiling-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest ceiling-db.1
  (ceiling 0.0d0 (make-bignum 10))
  0 0.0d0)

(deftest ceiling-db.2
  (ceiling 0.0d0 (make-bignum -10))
  0 0.0d0)

(deftest-error ceiling-db.3
  (ceiling 10.4d0 (make-bignum 0))
  division-by-zero)

(deftest ceiling-db.4a
  (ceiling-check-db 1.2d0 10    1 -8.8d0)
  t)

(deftest ceiling-db.4b
  (ceiling-check-db -1.2d0 10    0 -1.2d0)
  t)

(deftest ceiling-db.4c
  (ceiling-check-db 1.2d0 -10    0 1.2d0)
  t)

(deftest ceiling-db.4d
  (ceiling-check-db -1.2d0 -10    1 8.8d0)
  t)

(deftest ceiling-db.5a
  (ceiling-check-db 10.2d0 4    3 -1.8d0)
  t)

(deftest ceiling-db.5b
  (ceiling-check-db -10.2d0 4    -2 -2.2d0)
  t)

(deftest ceiling-db.5c
  (ceiling-check-db 10.2d0 -4    -2 2.2d0)
  t)

(deftest ceiling-db.5d
  (ceiling-check-db -10.2d0 -4    3 1.8d0)
  t)


;; double-float - ratio
(defun ceiling-check-dr (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest ceiling-dr.1
  (ceiling 0.0d0 10/3)
  0 0.0d0)

(deftest ceiling-dr.2
  (ceiling 0.0d0 -10/3)
  0 0.0d0)

(deftest-error ceiling-dr.3
  (ceiling 10.4d0 (make-ratio 0 1))
  division-by-zero)

(deftest ceiling-dr.4a
  (ceiling-check-dr 1.2d0 15/4    1 -2.55d0)
  t)

(deftest ceiling-dr.4b
  (ceiling-check-dr -1.2d0 15/4    0 -1.2d0)
  t)

(deftest ceiling-dr.4c
  (ceiling-check-dr 1.2d0 -15/4    0 1.2d0)
  t)

(deftest ceiling-dr.4d
  (ceiling-check-dr -1.2d0 -15/4    1 2.55d0)
  t)

(deftest ceiling-dr.5a
  (ceiling-check-dr 1.2d0 1/4    5 -0.05d0)
  t)

(deftest ceiling-dr.5b
  (ceiling-check-dr -1.2d0 1/4    -4 -0.2d0)
  t)

(deftest ceiling-dr.5c
  (ceiling-check-dr 1.2d0 -1/4    -4 0.2d0)
  t)

(deftest ceiling-dr.5d
  (ceiling-check-dr -1.2d0 -1/4    5 0.05d0)
  t)


;; double-float - single-float
(defun ceiling-check-ds (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'double-float))

(deftest ceiling-ds.1
  (ceiling 0.0d0 10.0f0)
  0 0.0d0)

(deftest ceiling-ds.2
  (ceiling 0.0d0 -10.0f0)
  0 0.0d0)

(deftest-error ceiling-ds.3
  (ceiling 10.4d0 0.0f0)
  division-by-zero)

(deftest ceiling-ds.4a
  (ceiling-check-ds 1.2d0 3.7f0    1 -2.5d0)
  t)

(deftest ceiling-ds.4b
  (ceiling-check-ds -1.2d0 3.7f0    0 -1.2d0)
  t)

(deftest ceiling-ds.4c
  (ceiling-check-ds 1.2d0 -3.7f0    0 1.2d0)
  t)

(deftest ceiling-ds.4d
  (ceiling-check-ds -1.2d0 -3.7f0    1 2.5d0)
  t)

(deftest ceiling-ds.5a
  (ceiling-check-ds 12.3d0 3.7f0    4 -2.5d0)
  t)

(deftest ceiling-ds.5b
  (ceiling-check-ds -12.3d0 3.7f0    -3 -1.2d0)
  t)

(deftest ceiling-ds.5c
  (ceiling-check-ds 12.3d0 -3.7f0    -3 1.2d0)
  t)

(deftest ceiling-ds.5d
  (ceiling-check-ds -12.3d0 -3.7f0    4 2.5d0)
  t)


;; double-float - double-float
(defun ceiling-check-dd (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest ceiling-dd.1
  (ceiling 0.0d0 10.0d0)
  0 0.0d0)

(deftest ceiling-dd.2
  (ceiling 0.0d0 -10.0d0)
  0 0.0d0)

(deftest-error ceiling-dd.3
  (ceiling 10.4d0 0.0d0)
  division-by-zero)

(deftest ceiling-dd.4a
  (ceiling-check-dd 1.2d0 3.7d0    1 -2.5d0)
  t)

(deftest ceiling-dd.4b
  (ceiling-check-dd -1.2d0 3.7d0    0 -1.2d0)
  t)

(deftest ceiling-dd.4c
  (ceiling-check-dd 1.2d0 -3.7d0    0 1.2d0)
  t)

(deftest ceiling-dd.4d
  (ceiling-check-dd -1.2d0 -3.7d0    1 2.5d0)
  t)

(deftest ceiling-dd.5a
  (ceiling-check-dd 12.3d0 3.7d0    4 -2.5d0)
  t)

(deftest ceiling-dd.5b
  (ceiling-check-dd -12.3d0 3.7d0    -3 -1.2d0)
  t)

(deftest ceiling-dd.5c
  (ceiling-check-dd 12.3d0 -3.7d0    -3 1.2d0)
  t)

(deftest ceiling-dd.5d
  (ceiling-check-dd -12.3d0 -3.7d0    4 2.5d0)
  t)


;; double-float - long-float
(defun ceiling-check-dl (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-dl.1
  (ceiling 0.0d0 10.0l0)
  0 0.0l0)

(deftest ceiling-dl.2
  (ceiling 0.0d0 -10.0l0)
  0 0.0l0)

(deftest-error ceiling-dl.3
  (ceiling 10.4d0 0.0l0)
  division-by-zero)

(deftest ceiling-dl.4a
  (ceiling-check-dl 1.2d0 3.7l0    1 -2.5l0)
  t)

(deftest ceiling-dl.4b
  (ceiling-check-dl -1.2d0 3.7l0    0 -1.2l0)
  t)

(deftest ceiling-dl.4c
  (ceiling-check-dl 1.2d0 -3.7l0    0 1.2l0)
  t)

(deftest ceiling-dl.4d
  (ceiling-check-dl -1.2d0 -3.7l0    1 2.5l0)
  t)

(deftest ceiling-dl.5a
  (ceiling-check-dl 12.3d0 3.7l0    4 -2.5l0)
  t)

(deftest ceiling-dl.5b
  (ceiling-check-dl -12.3d0 3.7l0    -3 -1.2l0)
  t)

(deftest ceiling-dl.5c
  (ceiling-check-dl 12.3d0 -3.7l0    -3 1.2l0)
  t)

(deftest ceiling-dl.5d
  (ceiling-check-dl -12.3d0 -3.7l0    4 2.5l0)
  t)


;; long-float - fixnum
(defun ceiling-check-lf (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-lf.1
  (ceiling 0.0l0 10)
  0 0.0l0)

(deftest ceiling-lf.2
  (ceiling 0.0l0 -10)
  0 0.0l0)

(deftest-error ceiling-lf.3
  (ceiling 10.4l0 0)
  division-by-zero)

(deftest ceiling-lf.4a
  (ceiling-check-lf 1.2l0 10    1 -8.8l0)
  t)

(deftest ceiling-lf.4b
  (ceiling-check-lf -1.2l0 10    0 -1.2l0)
  t)

(deftest ceiling-lf.4c
  (ceiling-check-lf 1.2l0 -10    0 1.2l0)
  t)

(deftest ceiling-lf.4d
  (ceiling-check-lf -1.2l0 -10    1 8.8l0)
  t)

(deftest ceiling-lf.5a
  (ceiling-check-lf 10.2l0 4    3 -1.8l0)
  t)

(deftest ceiling-lf.5b
  (ceiling-check-lf -10.2l0 4    -2 -2.2l0)
  t)

(deftest ceiling-lf.5c
  (ceiling-check-lf 10.2l0 -4    -2 2.2l0)
  t)

(deftest ceiling-lf.5d
  (ceiling-check-lf -10.2l0 -4    3 1.8l0)
  t)


;; long-float - bignum
(defun ceiling-check-lb (a b c d)
  (ceiling-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-lb.1
  (ceiling 0.0l0 (make-bignum 10))
  0 0.0l0)

(deftest ceiling-lb.2
  (ceiling 0.0l0 (make-bignum -10))
  0 0.0l0)

(deftest-error ceiling-lb.3
  (ceiling 10.4l0 (make-bignum 0))
  division-by-zero)

(deftest ceiling-lb.4a
  (ceiling-check-lb 1.2l0 10    1 -8.8l0)
  t)

(deftest ceiling-lb.4b
  (ceiling-check-lb -1.2l0 10    0 -1.2l0)
  t)

(deftest ceiling-lb.4c
  (ceiling-check-lb 1.2l0 -10    0 1.2l0)
  t)

(deftest ceiling-lb.4d
  (ceiling-check-lb -1.2l0 -10    1 8.8l0)
  t)

(deftest ceiling-lb.5a
  (ceiling-check-lb 10.2l0 4    3 -1.8l0)
  t)

(deftest ceiling-lb.5b
  (ceiling-check-lb -10.2l0 4    -2 -2.2l0)
  t)

(deftest ceiling-lb.5c
  (ceiling-check-lb 10.2l0 -4    -2 2.2l0)
  t)

(deftest ceiling-lb.5d
  (ceiling-check-lb -10.2l0 -4    3 1.8l0)
  t)


;; long-float - ratio
(defun ceiling-check-lr (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-lr.1
  (ceiling 0.0l0 10/3)
  0 0.0l0)

(deftest ceiling-lr.2
  (ceiling 0.0l0 -10/3)
  0 0.0l0)

(deftest-error ceiling-lr.3
  (ceiling 10.4l0 (make-ratio 0 1))
  division-by-zero)

(deftest ceiling-lr.4a
  (ceiling-check-lr 1.2l0 15/4    1 -2.55l0)
  t)

(deftest ceiling-lr.4b
  (ceiling-check-lr -1.2l0 15/4    0 -1.2l0)
  t)

(deftest ceiling-lr.4c
  (ceiling-check-lr 1.2l0 -15/4    0 1.2l0)
  t)

(deftest ceiling-lr.4d
  (ceiling-check-lr -1.2l0 -15/4    1 2.55l0)
  t)

(deftest ceiling-lr.5a
  (ceiling-check-lr 1.2l0 1/4    5 -0.05l0)
  t)

(deftest ceiling-lr.5b
  (ceiling-check-lr -1.2l0 1/4    -4 -0.2l0)
  t)

(deftest ceiling-lr.5c
  (ceiling-check-lr 1.2l0 -1/4    -4 0.2l0)
  t)

(deftest ceiling-lr.5d
  (ceiling-check-lr -1.2l0 -1/4    5 0.05l0)
  t)


;; long-float - single-float
(defun ceiling-check-ls (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'long-float))

(deftest ceiling-ls.1
  (ceiling 0.0l0 10.0f0)
  0 0.0l0)

(deftest ceiling-ls.2
  (ceiling 0.0l0 -10.0f0)
  0 0.0l0)

(deftest-error ceiling-ls.3
  (ceiling 10.4l0 0.0f0)
  division-by-zero)

(deftest ceiling-ls.4a
  (ceiling-check-ls 1.2l0 3.7f0    1 -2.5l0)
  t)

(deftest ceiling-ls.4b
  (ceiling-check-ls -1.2l0 3.7f0    0 -1.2l0)
  t)

(deftest ceiling-ls.4c
  (ceiling-check-ls 1.2l0 -3.7f0    0 1.2l0)
  t)

(deftest ceiling-ls.4d
  (ceiling-check-ls -1.2l0 -3.7f0    1 2.5l0)
  t)

(deftest ceiling-ls.5a
  (ceiling-check-ls 12.3l0 3.7f0    4 -2.5l0)
  t)

(deftest ceiling-ls.5b
  (ceiling-check-ls -12.3l0 3.7f0    -3 -1.2l0)
  t)

(deftest ceiling-ls.5c
  (ceiling-check-ls 12.3l0 -3.7f0    -3 1.2l0)
  t)

(deftest ceiling-ls.5d
  (ceiling-check-ls -12.3l0 -3.7f0    4 2.5l0)
  t)


;; long-float - double-float
(defun ceiling-check-ld (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-ld.1
  (ceiling 0.0l0 10.0d0)
  0 0.0l0)

(deftest ceiling-ld.2
  (ceiling 0.0l0 -10.0d0)
  0 0.0l0)

(deftest-error ceiling-ld.3
  (ceiling 10.4l0 0.0d0)
  division-by-zero)

(deftest ceiling-ld.4a
  (ceiling-check-ld 1.2l0 3.7d0    1 -2.5l0)
  t)

(deftest ceiling-ld.4b
  (ceiling-check-ld -1.2l0 3.7d0    0 -1.2l0)
  t)

(deftest ceiling-ld.4c
  (ceiling-check-ld 1.2l0 -3.7d0    0 1.2l0)
  t)

(deftest ceiling-ld.4d
  (ceiling-check-ld -1.2l0 -3.7d0    1 2.5l0)
  t)

(deftest ceiling-ld.5a
  (ceiling-check-ld 12.3l0 3.7d0    4 -2.5l0)
  t)

(deftest ceiling-ld.5b
  (ceiling-check-ld -12.3l0 3.7d0    -3 -1.2l0)
  t)

(deftest ceiling-ld.5c
  (ceiling-check-ld 12.3l0 -3.7d0    -3 1.2l0)
  t)

(deftest ceiling-ld.5d
  (ceiling-check-ld -12.3l0 -3.7d0    4 2.5l0)
  t)


;; long-float - long-float
(defun ceiling-check-ll (a b c d)
  (ceiling-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest ceiling-ll.1
  (ceiling 0.0l0 10.0l0)
  0 0.0l0)

(deftest ceiling-ll.2
  (ceiling 0.0l0 -10.0l0)
  0 0.0l0)

(deftest-error ceiling-ll.3
  (ceiling 10.4l0 0.0l0)
  division-by-zero)

(deftest ceiling-ll.4a
  (ceiling-check-ll 1.2l0 3.7l0    1 -2.5l0)
  t)

(deftest ceiling-ll.4b
  (ceiling-check-ll -1.2l0 3.7l0    0 -1.2l0)
  t)

(deftest ceiling-ll.4c
  (ceiling-check-ll 1.2l0 -3.7l0    0 1.2l0)
  t)

(deftest ceiling-ll.4d
  (ceiling-check-ll -1.2l0 -3.7l0    1 2.5l0)
  t)

(deftest ceiling-ll.5a
  (ceiling-check-ll 12.3l0 3.7l0    4 -2.5l0)
  t)

(deftest ceiling-ll.5b
  (ceiling-check-ll -12.3l0 3.7l0    -3 -1.2l0)
  t)

(deftest ceiling-ll.5c
  (ceiling-check-ll 12.3l0 -3.7l0    -3 1.2l0)
  t)

(deftest ceiling-ll.5d
  (ceiling-check-ll -12.3l0 -3.7l0    4 2.5l0)
  t)

