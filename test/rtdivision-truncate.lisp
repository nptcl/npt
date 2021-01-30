;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(defun truncate-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float) (call #'integerp))
  (multiple-value-bind (e f) (truncate a b)
    (or (and (funcall call e)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "truncate-equal error: (~S ~S) ~A, ~A, ~A" e f
          (funcall call e)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  truncate1
;;
(deftest truncate1-integer.1
  (truncate 0)
  0 0)

(deftest truncate1-integer.2
  (truncate 10)
  10 0)

(deftest truncate1-integer.3
  (truncate -10)
  -10 0)

(deftest truncate1-integer.4
  (truncate 99999999999999999999)
  99999999999999999999 0)

(deftest truncate1-integer.5
  (truncate -99999999999999999999)
  -99999999999999999999 0)

(deftest truncate1-ratio.1
  (truncate 1/3)
  0 1/3)

(deftest truncate1-ratio.2
  (truncate 10/3)
  3 1/3)

(deftest truncate1-ratio.3
  (truncate -1/3)
  0 -1/3)

(deftest truncate1-ratio.4
  (truncate -10/3)
  -3 -1/3)

(deftest truncate1-float.1
  (truncate 10.0)
  10 0.0)

(deftest truncate1-float.2
  (truncate -10.0)
  -10 0.0)

(deftest truncate1-float.3
  (truncate 12.25)
  12 0.25)

(deftest truncate1-float.4
  (truncate -12.25)
  -12 -0.25)

(deftest truncate1-float.5
  (truncate 12.25f0)
  12 0.25f0)

(deftest truncate1-float.6
  (truncate -12.25f0)
  -12 -0.25f0)

(deftest truncate1-float.7
  (truncate 12.25d0)
  12 0.25d0)

(deftest truncate1-float.8
  (truncate -12.25d0)
  -12 -0.25d0)

(deftest truncate1-float.9
  (truncate 12.25l0)
  12 0.25l0)

(deftest truncate1-float.10
  (truncate -12.25l0)
  -12 -0.25l0)


;;
;;  truncate
;;

;;  fixnum - fixnum
(deftest truncate-ff.1
  (truncate 0 10)
  0 0)

(deftest truncate-ff.2
  (truncate 0 -10)
  0 0)

(deftest-error truncate-ff.3
  (truncate 10 0)
  division-by-zero)

(deftest truncate-ff.4a
  (truncate 3 10)
  0 3)

(deftest truncate-ff.4b
  (truncate -3 10)
  0 -3)

(deftest truncate-ff.4c
  (truncate 3 -10)
  0 3)

(deftest truncate-ff.4d
  (truncate -3 -10)
  0 -3)

(deftest truncate-ff.5a
  (truncate 10 3)
  3 1)

(deftest truncate-ff.5b
  (truncate -10 3)
  -3 -1)

(deftest truncate-ff.5c
  (truncate 10 -3)
  -3 1)

(deftest truncate-ff.5d
  (truncate -10 -3)
  3 -1)

(deftest truncate-ff.6a
  (truncate 11 3)
  3 2)

(deftest truncate-ff.6b
  (truncate -11 3)
  -3 -2)

(deftest truncate-ff.6c
  (truncate 11 -3)
  -3 2)

(deftest truncate-ff.6d
  (truncate -11 -3)
  3 -2)


;; fixnum - bignum
(deftest truncate-fb.1
  (truncate 0 (make-bignum 10))
  0 0)

(deftest truncate-fb.2
  (truncate 0 (make-bignum -10))
  0 0)

(deftest-error truncate-fb.3
  (truncate 10 (make-bignum 0))
  division-by-zero)

(deftest truncate-fb.4a
  (truncate 3 (make-bignum 10))
  0 3)

(deftest truncate-fb.4b
  (truncate -3 (make-bignum 10))
  0 -3)

(deftest truncate-fb.4c
  (truncate 3 (make-bignum -10))
  0 3)

(deftest truncate-fb.4d
  (truncate -3 (make-bignum -10))
  0 -3)

(deftest truncate-fb.5a
  (truncate 10 (make-bignum 3))
  3 1)

(deftest truncate-fb.5b
  (truncate -10 (make-bignum 3))
  -3 -1)

(deftest truncate-fb.5c
  (truncate 10 (make-bignum -3))
  -3 1)

(deftest truncate-fb.5d
  (truncate -10 (make-bignum -3))
  3 -1)

(deftest truncate-fb.6a
  (truncate 11 (make-bignum 3))
  3 2)

(deftest truncate-fb.6b
  (truncate -11 (make-bignum 3))
  -3 -2)

(deftest truncate-fb.6c
  (truncate 11 (make-bignum -3))
  -3 2)

(deftest truncate-fb.6d
  (truncate -11 (make-bignum -3))
  3 -2)


;;  fixnum - ratio
(deftest truncate-fr.1
  (truncate 0 10/3)
  0 0)

(deftest truncate-fr.2
  (truncate 0 -10/3)
  0 0)

(deftest-error truncate-fr.3
  (truncate 10 (make-ratio 0 1))
  division-by-zero)

(deftest truncate-fr.4a
  (truncate 3 100/7)
  0 3)

(deftest truncate-fr.4b
  (truncate -3 100/7)
  0 -3)

(deftest truncate-fr.4c
  (truncate 3 -100/7)
  0 3)

(deftest truncate-fr.4d
  (truncate -3 -100/7)
  0 -3)

(deftest truncate-fr.5a
  (truncate 10 6/7)
  11 4/7)

(deftest truncate-fr.5b
  (truncate -10 6/7)
  -11 -4/7)

(deftest truncate-fr.5c
  (truncate 10 -6/7)
  -11 4/7)

(deftest truncate-fr.5d
  (truncate -10 -6/7)
  11 -4/7)

(deftest truncate-fr.6a
  (truncate 10 1/3)
  30 0)

(deftest truncate-fr.6b
  (truncate -10 1/3)
  -30 0)

(deftest truncate-fr.6c
  (truncate 10 -1/3)
  -30 0)

(deftest truncate-fr.6d
  (truncate -10 -1/3)
  30 0)

(deftest truncate-fr.7a
  (truncate 10 4/3)
  7 2/3)


;;  fixnum - single-float
(defun truncate-check-fs (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest truncate-fs.1
  (truncate 0 10.0f0)
  0 0.0f0)

(deftest truncate-fs.2
  (truncate 0 -10.0f0)
  0 0.0f0)

(deftest-error truncate-fs.3
  (truncate 10 0.0f0)
  division-by-zero)

(deftest truncate-fs.4a
  (truncate-check-fs 4 10.5f0    0 4.0f0)
  t)

(deftest truncate-fs.4b
  (truncate-check-fs -4 10.5f0    0 -4.0f0)
  t)

(deftest truncate-fs.4c
  (truncate-check-fs 4 -10.5f0    0 4.0f0)
  t)

(deftest truncate-fs.4d
  (truncate-check-fs -4 -10.5f0    0 -4.0f0)
  t)

(deftest truncate-fs.5a
  (truncate-check-fs 15 1.6f0    9 0.6f0)
  t)

(deftest truncate-fs.5b
  (truncate-check-fs -15 1.6f0    -9 -0.6f0)
  t)

(deftest truncate-fs.5c
  (truncate-check-fs 15 -1.6f0    -9 0.6f0)
  t)

(deftest truncate-fs.5d
  (truncate-check-fs -15 -1.6f0    9 -0.6f0)
  t)


;;  fixnum - double-float
(defun truncate-check-fd (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest truncate-fd.1
  (truncate 0 10.0d0)
  0 0.0d0)

(deftest truncate-fd.2
  (truncate 0 -10.0d0)
  0 0.0d0)

(deftest-error truncate-fd.3
  (truncate 10 0.0d0)
  division-by-zero)

(deftest truncate-fd.4a
  (truncate-check-fd 4 10.5d0    0 4.0d0)
  t)

(deftest truncate-fd.4b
  (truncate-check-fd -4 10.5d0    0 -4.0d0)
  t)

(deftest truncate-fd.4c
  (truncate-check-fd 4 -10.5d0    0 4.0d0)
  t)

(deftest truncate-fd.4d
  (truncate-check-fd -4 -10.5d0    0 -4.0d0)
  t)

(deftest truncate-fd.5a
  (truncate-check-fd 15 1.6d0    9 0.6d0)
  t)

(deftest truncate-fd.5b
  (truncate-check-fd -15 1.6d0    -9 -0.6d0)
  t)

(deftest truncate-fd.5c
  (truncate-check-fd 15 -1.6d0    -9 0.6d0)
  t)

(deftest truncate-fd.5d
  (truncate-check-fd -15 -1.6d0    9 -0.6d0)
  t)


;;  fixnum - long-float
(defun truncate-check-fl (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest truncate-fl.1
  (truncate 0 10.0l0)
  0 0.0l0)

(deftest truncate-fl.2
  (truncate 0 -10.0l0)
  0 0.0l0)

(deftest-error truncate-fl.3
  (truncate 10 0.0l0)
  division-by-zero)

(deftest truncate-fl.4a
  (truncate-check-fl 4 10.5l0    0 4.0l0)
  t)

(deftest truncate-fl.4b
  (truncate-check-fl -4 10.5l0    0 -4.0l0)
  t)

(deftest truncate-fl.4c
  (truncate-check-fl 4 -10.5l0    0 4.0l0)
  t)

(deftest truncate-fl.4d
  (truncate-check-fl -4 -10.5l0    0 -4.0l0)
  t)

(deftest truncate-fl.5a
  (truncate-check-fl 15 1.6l0    9 0.6l0)
  t)

(deftest truncate-fl.5b
  (truncate-check-fl -15 1.6l0    -9 -0.6l0)
  t)

(deftest truncate-fl.5c
  (truncate-check-fl 15 -1.6l0    -9 0.6l0)
  t)

(deftest truncate-fl.5d
  (truncate-check-fl -15 -1.6l0    9 -0.6l0)
  t)


;;  bignum - fixnum
(defun truncateb (a b)
  (truncate (make-bignum a) b))

(deftest truncate-bf.1
  (truncateb 0 10)
  0 0)

(deftest truncate-bf.2
  (truncateb 0 -10)
  0 0)

(deftest-error truncate-bf.3
  (truncateb 10 0)
  division-by-zero)

(deftest truncate-bf.4a
  (truncateb 3 10)
  0 3)

(deftest truncate-bf.4b
  (truncateb -3 10)
  0 -3)

(deftest truncate-bf.4c
  (truncateb 3 -10)
  0 3)

(deftest truncate-bf.4d
  (truncateb -3 -10)
  0 -3)

(deftest truncate-bf.5a
  (truncateb 10 3)
  3 1)

(deftest truncate-bf.5b
  (truncateb -10 3)
  -3 -1)

(deftest truncate-bf.5c
  (truncateb 10 -3)
  -3 1)

(deftest truncate-bf.5d
  (truncateb -10 -3)
  3 -1)

(deftest truncate-bf.6a
  (truncateb 11 3)
  3 2)

(deftest truncate-bf.6b
  (truncateb -11 3)
  -3 -2)

(deftest truncate-bf.6c
  (truncateb 11 -3)
  -3 2)

(deftest truncate-bf.6d
  (truncateb -11 -3)
  3 -2)


;; bignum - bignum
(deftest truncate-bb.1
  (truncateb 0 (make-bignum 10))
  0 0)

(deftest truncate-bb.2
  (truncateb 0 (make-bignum -10))
  0 0)

(deftest-error truncate-bb.3
  (truncateb 10 (make-bignum 0))
  division-by-zero)

(deftest truncate-bb.4a
  (truncateb 3 (make-bignum 10))
  0 3)

(deftest truncate-bb.4b
  (truncateb -3 (make-bignum 10))
  0 -3)

(deftest truncate-bb.4c
  (truncateb 3 (make-bignum -10))
  0 3)

(deftest truncate-bb.4d
  (truncateb -3 (make-bignum -10))
  0 -3)

(deftest truncate-bb.5a
  (truncateb 10 (make-bignum 3))
  3 1)

(deftest truncate-bb.5b
  (truncateb -10 (make-bignum 3))
  -3 -1)

(deftest truncate-bb.5c
  (truncateb 10 (make-bignum -3))
  -3 1)

(deftest truncate-bb.5d
  (truncateb -10 (make-bignum -3))
  3 -1)

(deftest truncate-bb.6a
  (truncateb 11 (make-bignum 3))
  3 2)

(deftest truncate-bb.6b
  (truncateb -11 (make-bignum 3))
  -3 -2)

(deftest truncate-bb.6c
  (truncateb 11 (make-bignum -3))
  -3 2)

(deftest truncate-bb.6d
  (truncateb -11 (make-bignum -3))
  3 -2)


;;  bignum - ratio
(deftest truncate-br.1
  (truncateb 0 10/3)
  0 0)

(deftest truncate-br.2
  (truncateb 0 -10/3)
  0 0)

(deftest-error truncate-br.3
  (truncateb 10 (make-ratio 0 1))
  division-by-zero)

(deftest truncate-br.4a
  (truncateb 3 100/7)
  0 3)

(deftest truncate-br.4b
  (truncateb -3 100/7)
  0 -3)

(deftest truncate-br.4c
  (truncateb 3 -100/7)
  0 3)

(deftest truncate-br.4d
  (truncateb -3 -100/7)
  0 -3)

(deftest truncate-br.5a
  (truncateb 10 6/7)
  11 4/7)

(deftest truncate-br.5b
  (truncateb -10 6/7)
  -11 -4/7)

(deftest truncate-br.5c
  (truncateb 10 -6/7)
  -11 4/7)

(deftest truncate-br.5d
  (truncateb -10 -6/7)
  11 -4/7)

(deftest truncate-br.6a
  (truncateb 10 1/3)
  30 0)

(deftest truncate-br.6b
  (truncateb -10 1/3)
  -30 0)

(deftest truncate-br.6c
  (truncateb 10 -1/3)
  -30 0)

(deftest truncate-br.6d
  (truncateb -10 -1/3)
  30 0)


;;  bignum - single-float
(defun truncate-check-bs (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest truncate-bs.1
  (truncateb 0 10.0f0)
  0 0.0f0)

(deftest truncate-bs.2
  (truncateb 0 -10.0f0)
  0 0.0f0)

(deftest-error truncate-bs.3
  (truncateb 10 0.0f0)
  division-by-zero)

(deftest truncate-bs.4a
  (truncate-check-bs 4 10.5f0    0 4.0f0)
  t)

(deftest truncate-bs.4b
  (truncate-check-bs -4 10.5f0    0 -4.0f0)
  t)

(deftest truncate-bs.4c
  (truncate-check-bs 4 -10.5f0    0 4.0f0)
  t)

(deftest truncate-bs.4d
  (truncate-check-bs -4 -10.5f0    0 -4.0f0)
  t)

(deftest truncate-bs.5a
  (truncate-check-bs 15 1.6f0    9 0.6f0)
  t)

(deftest truncate-bs.5b
  (truncate-check-bs -15 1.6f0    -9 -0.6f0)
  t)

(deftest truncate-bs.5c
  (truncate-check-bs 15 -1.6f0    -9 0.6f0)
  t)

(deftest truncate-bs.5d
  (truncate-check-bs -15 -1.6f0    9 -0.6f0)
  t)


;;  bignum - double-float
(defun truncate-check-bd (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest truncate-bd.1
  (truncateb 0 10.0d0)
  0 0.0d0)

(deftest truncate-bd.2
  (truncateb 0 -10.0d0)
  0 0.0d0)

(deftest-error truncate-bd.3
  (truncateb 10 0.0d0)
  division-by-zero)

(deftest truncate-bd.4a
  (truncate-check-bd 4 10.5d0    0 4.0d0)
  t)

(deftest truncate-bd.4b
  (truncate-check-bd -4 10.5d0    0 -4.0d0)
  t)

(deftest truncate-bd.4c
  (truncate-check-bd 4 -10.5d0    0 4.0d0)
  t)

(deftest truncate-bd.4d
  (truncate-check-bd -4 -10.5d0    0 -4.0d0)
  t)

(deftest truncate-bd.5a
  (truncate-check-bd 15 1.6d0    9 0.6d0)
  t)

(deftest truncate-bd.5b
  (truncate-check-bd -15 1.6d0    -9 -0.6d0)
  t)

(deftest truncate-bd.5c
  (truncate-check-bd 15 -1.6d0    -9 0.6d0)
  t)

(deftest truncate-bd.5d
  (truncate-check-bd -15 -1.6d0    9 -0.6d0)
  t)


;;  bignum - long-float
(defun truncate-check-bl (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest truncate-bl.1
  (truncateb 0 10.0l0)
  0 0.0l0)

(deftest truncate-bl.2
  (truncateb 0 -10.0l0)
  0 0.0l0)

(deftest-error truncate-bl.3
  (truncateb 10 0.0l0)
  division-by-zero)

(deftest truncate-bl.4a
  (truncate-check-bl 4 10.5l0    0 4.0l0)
  t)

(deftest truncate-bl.4b
  (truncate-check-bl -4 10.5l0    0 -4.0l0)
  t)

(deftest truncate-bl.4c
  (truncate-check-bl 4 -10.5l0    0 4.0l0)
  t)

(deftest truncate-bl.4d
  (truncate-check-bl -4 -10.5l0    0 -4.0l0)
  t)

(deftest truncate-bl.5a
  (truncate-check-bl 15 1.6l0    9 0.6l0)
  t)

(deftest truncate-bl.5b
  (truncate-check-bl -15 1.6l0    -9 -0.6l0)
  t)

(deftest truncate-bl.5c
  (truncate-check-bl 15 -1.6l0    -9 0.6l0)
  t)

(deftest truncate-bl.5d
  (truncate-check-bl -15 -1.6l0    9 -0.6l0)
  t)


;;  ratio - fixnum
(deftest truncate-rf.1
  (truncate (make-ratio 0 1) 10)
  0 0)

(deftest truncate-rf.2
  (truncate (make-ratio 0 1) -10)
  0 0)

(deftest-error truncate-rf.3
  (truncate 10/3 0)
  division-by-zero)

(deftest truncate-rf.4a
  (truncate 2/3 5)
  0 2/3)

(deftest truncate-rf.4b
  (truncate -2/3 5)
  0 -2/3)

(deftest truncate-rf.4c
  (truncate 2/3 -5)
  0 2/3)

(deftest truncate-rf.4d
  (truncate -2/3 -5)
  0 -2/3)

(deftest truncate-rf.5a
  (truncate 20/3 5)
  1 5/3)

(deftest truncate-rf.5b
  (truncate -20/3 5)
  -1 -5/3)

(deftest truncate-rf.5c
  (truncate 20/3 -5)
  -1 5/3)

(deftest truncate-rf.5d
  (truncate -20/3 -5)
  1 -5/3)

(deftest truncate-rf.6a
  (truncate 53/3 8)
  2 5/3)

(deftest truncate-rf.6b
  (truncate -53/3 8)
  -2 -5/3)

(deftest truncate-rf.6c
  (truncate 53/3 -8)
  -2 5/3)

(deftest truncate-rf.6d
  (truncate -53/3 -8)
  2 -5/3)


;; ratio - bignum
(deftest truncate-rb.1
  (truncate (make-ratio 0 1) (make-bignum 10))
  0 0)

(deftest truncate-rb.2
  (truncate (make-ratio 0 1) (make-bignum -10))
  0 0)

(deftest-error truncate-rb.3
  (truncate 10/3 (make-bignum 0))
  division-by-zero)

(deftest truncate-rb.4a
  (truncate 2/3 (make-bignum 5))
  0 2/3)

(deftest truncate-rb.4b
  (truncate -2/3 (make-bignum 5))
  0 -2/3)

(deftest truncate-rb.4c
  (truncate 2/3 (make-bignum -5))
  0 2/3)

(deftest truncate-rb.4d
  (truncate -2/3 (make-bignum -5))
  0 -2/3)

(deftest truncate-rb.5a
  (truncate 20/3 (make-bignum 5))
  1 5/3)

(deftest truncate-rb.5b
  (truncate -20/3 (make-bignum 5))
  -1 -5/3)

(deftest truncate-rb.5c
  (truncate 20/3 (make-bignum -5))
  -1 5/3)

(deftest truncate-rb.5d
  (truncate -20/3 (make-bignum -5))
  1 -5/3)

(deftest truncate-rb.6a
  (truncate 53/3 (make-bignum 8))
  2 5/3)

(deftest truncate-rb.6b
  (truncate -53/3 (make-bignum 8))
  -2 -5/3)

(deftest truncate-rb.6c
  (truncate 53/3 (make-bignum -8))
  -2 5/3)

(deftest truncate-rb.6d
  (truncate -53/3 (make-bignum -8))
  2 -5/3)


;;  ratio - ratio
(deftest truncate-rr.1
  (truncate (make-ratio 0 1) 10/3)
  0 0)

(deftest truncate-rr.2
  (truncate (make-ratio 0 1) -10/3)
  0 0)

(deftest-error truncate-rr.3
  (truncate 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest truncate-rr.4a
  (truncate 1/2 33/5)
  0 1/2)

(deftest truncate-rr.4b
  (truncate -1/2 33/5)
  0 -1/2)

(deftest truncate-rr.4c
  (truncate 1/2 -33/5)
  0 1/2)

(deftest truncate-rr.4d
  (truncate -1/2 -33/5)
  0 -1/2)

(deftest truncate-rr.5a
  (truncate 79/3 2/7)
  92 1/21)

(deftest truncate-rr.5b
  (truncate -79/3 2/7)
  -92 -1/21)

(deftest truncate-rr.5c
  (truncate 79/3 -2/7)
  -92 1/21)

(deftest truncate-rr.5d
  (truncate -79/3 -2/7)
  92 -1/21)

(deftest truncate-rr.6a
  (truncate 4/5 8/20)
  2 0)

(deftest truncate-rr.6b
  (truncate -4/5 8/20)
  -2 0)

(deftest truncate-rr.6c
  (truncate 4/5 -8/20)
  -2 0)

(deftest truncate-rr.6d
  (truncate -4/5 -8/20)
  2 0)


;;  ratio - single-float
(defun truncate-check-rs (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest truncate-rs.1
  (truncate (make-ratio 0 1) 10.0f0)
  0 0.0f0)

(deftest truncate-rs.2
  (truncate (make-ratio 0 1) -10.0f0)
  0 0.0f0)

(deftest-error truncate-rs.3
  (truncate 10/4 0.0f0)
  division-by-zero)

(deftest truncate-rs.4a
  (truncate-check-rs 3/4 5.5f0    0 0.75f0)
  t)

(deftest truncate-rs.4b
  (truncate-check-rs -3/4 5.5f0    0 -0.75f0)
  t)

(deftest truncate-rs.4c
  (truncate-check-rs 3/4 -5.5f0    0 0.75f0)
  t)

(deftest truncate-rs.4d
  (truncate-check-rs -3/4 -5.5f0    0 -0.75f0)
  t)

(deftest truncate-rs.5a
  (truncate-check-rs 77/4 1.6f0    12 0.05f0)
  t)

(deftest truncate-rs.5b
  (truncate-check-rs -77/4 1.6f0    -12 -0.05f0)
  t)

(deftest truncate-rs.5c
  (truncate-check-rs 77/4 -1.6f0    -12 0.05f0)
  t)

(deftest truncate-rs.5d
  (truncate-check-rs -77/4 -1.6f0    12 -0.05f0)
  t)


;;  ratio - double-float
(defun truncate-check-rd (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest truncate-rd.1
  (truncate (make-ratio 0 1) 10.0d0)
  0 0.0d0)

(deftest truncate-rd.2
  (truncate (make-ratio 0 1) -10.0d0)
  0 0.0d0)

(deftest-error truncate-rd.3
  (truncate 10/4 0.0d0)
  division-by-zero)

(deftest truncate-rd.4a
  (truncate-check-rd 3/4 5.5d0    0 0.75d0)
  t)

(deftest truncate-rd.4b
  (truncate-check-rd -3/4 5.5d0    0 -0.75d0)
  t)

(deftest truncate-rd.4c
  (truncate-check-rd 3/4 -5.5d0    0 0.75d0)
  t)

(deftest truncate-rd.4d
  (truncate-check-rd -3/4 -5.5d0    0 -0.75d0)
  t)

(deftest truncate-rd.5a
  (truncate-check-rd 77/4 1.6d0    12 0.05d0)
  t)

(deftest truncate-rd.5b
  (truncate-check-rd -77/4 1.6d0    -12 -0.05d0)
  t)

(deftest truncate-rd.5c
  (truncate-check-rd 77/4 -1.6d0    -12 0.05d0)
  t)

(deftest truncate-rd.5d
  (truncate-check-rd -77/4 -1.6d0    12 -0.05d0)
  t)


;;  ratio - long-float
(defun truncate-check-rl (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'integerp
    :type 'long-float))

(deftest truncate-rl.1
  (truncate (make-ratio 0 1) 10.0l0)
  0 0.0l0)

(deftest truncate-rl.2
  (truncate (make-ratio 0 1) -10.0l0)
  0 0.0l0)

(deftest-error truncate-rl.3
  (truncate 10/4 0.0l0)
  division-by-zero)

(deftest truncate-rl.4a
  (truncate-check-rl 3/4 5.5l0    0 0.75l0)
  t)

(deftest truncate-rl.4b
  (truncate-check-rl -3/4 5.5l0    0 -0.75l0)
  t)

(deftest truncate-rl.4c
  (truncate-check-rl 3/4 -5.5l0    0 0.75l0)
  t)

(deftest truncate-rl.4d
  (truncate-check-rl -3/4 -5.5l0    0 -0.75l0)
  t)

(deftest truncate-rl.5a
  (truncate-check-rl 77/4 1.6l0    12 0.05l0)
  t)

(deftest truncate-rl.5b
  (truncate-check-rl -77/4 1.6l0    -12 -0.05l0)
  t)

(deftest truncate-rl.5c
  (truncate-check-rl 77/4 -1.6l0    -12 0.05l0)
  t)

(deftest truncate-rl.5d
  (truncate-check-rl -77/4 -1.6l0    12 -0.05l0)
  t)


;; single-float - fixnum
(defun truncate-check-sf (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest truncate-sf.1
  (truncate 0.0f0 10)
  0 0.0f0)

(deftest truncate-sf.2
  (truncate 0.0f0 -10)
  0 0.0f0)

(deftest-error truncate-sf.3
  (truncate 10.4f0 0)
  division-by-zero)

(deftest truncate-sf.4a
  (truncate-check-sf 1.2f0 10    0 1.2f0)
  t)

(deftest truncate-sf.4b
  (truncate-check-sf -1.2f0 10    0 -1.2f0)
  t)

(deftest truncate-sf.4c
  (truncate-check-sf 1.2f0 -10    0 1.2f0)
  t)

(deftest truncate-sf.4d
  (truncate-check-sf -1.2f0 -10    0 -1.2f0)
  t)

(deftest truncate-sf.5a
  (truncate-check-sf 10.2f0 4    2 2.2f0)
  t)

(deftest truncate-sf.5b
  (truncate-check-sf -10.2f0 4    -2 -2.2f0)
  t)

(deftest truncate-sf.5c
  (truncate-check-sf 10.2f0 -4    -2 2.2f0)
  t)

(deftest truncate-sf.5d
  (truncate-check-sf -10.2f0 -4    2 -2.2f0)
  t)


;; single-float - bignum
(defun truncate-check-sb (a b c d)
  (truncate-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest truncate-sb.1
  (truncate 0.0f0 (make-bignum 10))
  0 0.0f0)

(deftest truncate-sb.2
  (truncate 0.0f0 (make-bignum -10))
  0 0.0f0)

(deftest-error truncate-sb.3
  (truncate 10.4f0 (make-bignum 0))
  division-by-zero)

(deftest truncate-sb.4a
  (truncate-check-sb 1.2f0 10    0 1.2f0)
  t)

(deftest truncate-sb.4b
  (truncate-check-sb -1.2f0 10    0 -1.2f0)
  t)

(deftest truncate-sb.4c
  (truncate-check-sb 1.2f0 -10    0 1.2f0)
  t)

(deftest truncate-sb.4d
  (truncate-check-sb -1.2f0 -10    0 -1.2f0)
  t)

(deftest truncate-sb.5a
  (truncate-check-sb 10.2f0 4    2 2.2f0)
  t)

(deftest truncate-sb.5b
  (truncate-check-sb -10.2f0 4    -2 -2.2f0)
  t)

(deftest truncate-sb.5c
  (truncate-check-sb 10.2f0 -4    -2 2.2f0)
  t)

(deftest truncate-sb.5d
  (truncate-check-sb -10.2f0 -4    2 -2.2f0)
  t)


;; single-float - ratio
(defun truncate-check-sr (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest truncate-sr.1
  (truncate 0.0f0 10/3)
  0 0.0f0)

(deftest truncate-sr.2
  (truncate 0.0f0 -10/3)
  0 0.0f0)

(deftest-error truncate-sr.3
  (truncate 10.4f0 (make-ratio 0 1))
  division-by-zero)

(deftest truncate-sr.4a
  (truncate-check-sr 1.2f0 15/4    0 1.2f0)
  t)

(deftest truncate-sr.4b
  (truncate-check-sr -1.2f0 15/4    0 -1.2f0)
  t)

(deftest truncate-sr.4c
  (truncate-check-sr 1.2f0 -15/4    0 1.2f0)
  t)

(deftest truncate-sr.4d
  (truncate-check-sr -1.2f0 -15/4    0 -1.2f0)
  t)

(deftest truncate-sr.5a
  (truncate-check-sr 1.2f0 1/4    4 0.2f0)
  t)

(deftest truncate-sr.5b
  (truncate-check-sr -1.2f0 1/4    -4 -0.2f0)
  t)

(deftest truncate-sr.5c
  (truncate-check-sr 1.2f0 -1/4    -4 0.2f0)
  t)

(deftest truncate-sr.5d
  (truncate-check-sr -1.2f0 -1/4    4 -0.2f0)
  t)


;; single-float - single-float
(defun truncate-check-ss (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest truncate-ss.1
  (truncate 0.0f0 10.0f0)
  0 0.0f0)

(deftest truncate-ss.2
  (truncate 0.0f0 -10.0f0)
  0 0.0f0)

(deftest-error truncate-ss.3
  (truncate 10.4f0 0.0f0)
  division-by-zero)

(deftest truncate-ss.4a
  (truncate-check-ss 1.2f0 3.7f0    0 1.2f0)
  t)

(deftest truncate-ss.4b
  (truncate-check-ss -1.2f0 3.7f0    0 -1.2f0)
  t)

(deftest truncate-ss.4c
  (truncate-check-ss 1.2f0 -3.7f0    0 1.2f0)
  t)

(deftest truncate-ss.4d
  (truncate-check-ss -1.2f0 -3.7f0    0 -1.2f0)
  t)

(deftest truncate-ss.5a
  (truncate-check-ss 12.3f0 3.7f0    3 1.2f0)
  t)

(deftest truncate-ss.5b
  (truncate-check-ss -12.3f0 3.7f0    -3 -1.2f0)
  t)

(deftest truncate-ss.5c
  (truncate-check-ss 12.3f0 -3.7f0    -3 1.2f0)
  t)

(deftest truncate-ss.5d
  (truncate-check-ss -12.3f0 -3.7f0    3 -1.2f0)
  t)


;; single-float - double-float
(defun truncate-check-sd (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :call #'integerp
    :type 'double-float))

(deftest truncate-sd.1
  (truncate 0.0f0 10.0d0)
  0 0.0d0)

(deftest truncate-sd.2
  (truncate 0.0f0 -10.0d0)
  0 0.0d0)

(deftest-error truncate-sd.3
  (truncate 10.4f0 0.0d0)
  division-by-zero)

(deftest truncate-sd.4a
  (truncate-check-sd 1.2f0 3.7d0    0 1.2d0)
  t)

(deftest truncate-sd.4b
  (truncate-check-sd -1.2f0 3.7d0    0 -1.2d0)
  t)

(deftest truncate-sd.4c
  (truncate-check-sd 1.2f0 -3.7d0    0 1.2d0)
  t)

(deftest truncate-sd.4d
  (truncate-check-sd -1.2f0 -3.7d0    0 -1.2d0)
  t)

(deftest truncate-sd.5a
  (truncate-check-sd 12.3f0 3.7d0    3 1.2d0)
  t)

(deftest truncate-sd.5b
  (truncate-check-sd -12.3f0 3.7d0    -3 -1.2d0)
  t)

(deftest truncate-sd.5c
  (truncate-check-sd 12.3f0 -3.7d0    -3 1.2d0)
  t)

(deftest truncate-sd.5d
  (truncate-check-sd -12.3f0 -3.7d0    3 -1.2d0)
  t)


;; single-float - long-float
(defun truncate-check-sl (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :call #'integerp
    :type 'long-float))

(deftest truncate-sl.1
  (truncate 0.0f0 10.0l0)
  0 0.0l0)

(deftest truncate-sl.2
  (truncate 0.0f0 -10.0l0)
  0 0.0l0)

(deftest-error truncate-sl.3
  (truncate 10.4f0 0.0l0)
  division-by-zero)

(deftest truncate-sl.4a
  (truncate-check-sl 1.2f0 3.7l0    0 1.2l0)
  t)

(deftest truncate-sl.4b
  (truncate-check-sl -1.2f0 3.7l0    0 -1.2l0)
  t)

(deftest truncate-sl.4c
  (truncate-check-sl 1.2f0 -3.7l0    0 1.2l0)
  t)

(deftest truncate-sl.4d
  (truncate-check-sl -1.2f0 -3.7l0    0 -1.2l0)
  t)

(deftest truncate-sl.5a
  (truncate-check-sl 12.3f0 3.7l0    3 1.2l0)
  t)

(deftest truncate-sl.5b
  (truncate-check-sl -12.3f0 3.7l0    -3 -1.2l0)
  t)

(deftest truncate-sl.5c
  (truncate-check-sl 12.3f0 -3.7l0    -3 1.2l0)
  t)

(deftest truncate-sl.5d
  (truncate-check-sl -12.3f0 -3.7l0    3 -1.2l0)
  t)


;; double-float - fixnum
(defun truncate-check-df (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-14
    :call #'integerp
    :type 'double-float))

(deftest truncate-df.1
  (truncate 0.0d0 10)
  0 0.0d0)

(deftest truncate-df.2
  (truncate 0.0d0 -10)
  0 0.0d0)

(deftest-error truncate-df.3
  (truncate 10.4d0 0)
  division-by-zero)

(deftest truncate-df.4a
  (truncate-check-df 1.2d0 10    0 1.2d0)
  t)

(deftest truncate-df.4b
  (truncate-check-df -1.2d0 10    0 -1.2d0)
  t)

(deftest truncate-df.4c
  (truncate-check-df 1.2d0 -10    0 1.2d0)
  t)

(deftest truncate-df.4d
  (truncate-check-df -1.2d0 -10    0 -1.2d0)
  t)

(deftest truncate-df.5a
  (truncate-check-df 10.2d0 4    2 2.2d0)
  t)

(deftest truncate-df.5b
  (truncate-check-df -10.2d0 4    -2 -2.2d0)
  t)

(deftest truncate-df.5c
  (truncate-check-df 10.2d0 -4    -2 2.2d0)
  t)

(deftest truncate-df.5d
  (truncate-check-df -10.2d0 -4    2 -2.2d0)
  t)


;; double-float - bignum
(defun truncate-check-db (a b c d)
  (truncate-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest truncate-db.1
  (truncate 0.0d0 (make-bignum 10))
  0 0.0d0)

(deftest truncate-db.2
  (truncate 0.0d0 (make-bignum -10))
  0 0.0d0)

(deftest-error truncate-db.3
  (truncate 10.4d0 (make-bignum 0))
  division-by-zero)

(deftest truncate-db.4a
  (truncate-check-db 1.2d0 10    0 1.2d0)
  t)

(deftest truncate-db.4b
  (truncate-check-db -1.2d0 10    0 -1.2d0)
  t)

(deftest truncate-db.4c
  (truncate-check-db 1.2d0 -10    0 1.2d0)
  t)

(deftest truncate-db.4d
  (truncate-check-db -1.2d0 -10    0 -1.2d0)
  t)

(deftest truncate-db.5a
  (truncate-check-db 10.2d0 4    2 2.2d0)
  t)

(deftest truncate-db.5b
  (truncate-check-db -10.2d0 4    -2 -2.2d0)
  t)

(deftest truncate-db.5c
  (truncate-check-db 10.2d0 -4    -2 2.2d0)
  t)

(deftest truncate-db.5d
  (truncate-check-db -10.2d0 -4    2 -2.2d0)
  t)


;; double-float - ratio
(defun truncate-check-dr (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest truncate-dr.1
  (truncate 0.0d0 10/3)
  0 0.0d0)

(deftest truncate-dr.2
  (truncate 0.0d0 -10/3)
  0 0.0d0)

(deftest-error truncate-dr.3
  (truncate 10.4d0 (make-ratio 0 1))
  division-by-zero)

(deftest truncate-dr.4a
  (truncate-check-dr 1.2d0 15/4    0 1.2d0)
  t)

(deftest truncate-dr.4b
  (truncate-check-dr -1.2d0 15/4    0 -1.2d0)
  t)

(deftest truncate-dr.4c
  (truncate-check-dr 1.2d0 -15/4    0 1.2d0)
  t)

(deftest truncate-dr.4d
  (truncate-check-dr -1.2d0 -15/4    0 -1.2d0)
  t)

(deftest truncate-dr.5a
  (truncate-check-dr 1.2d0 1/4    4 0.2d0)
  t)

(deftest truncate-dr.5b
  (truncate-check-dr -1.2d0 1/4    -4 -0.2d0)
  t)

(deftest truncate-dr.5c
  (truncate-check-dr 1.2d0 -1/4    -4 0.2d0)
  t)

(deftest truncate-dr.5d
  (truncate-check-dr -1.2d0 -1/4    4 -0.2d0)
  t)


;; double-float - single-float
(defun truncate-check-ds (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'double-float))

(deftest truncate-ds.1
  (truncate 0.0d0 10.0f0)
  0 0.0d0)

(deftest truncate-ds.2
  (truncate 0.0d0 -10.0f0)
  0 0.0d0)

(deftest-error truncate-ds.3
  (truncate 10.4d0 0.0f0)
  division-by-zero)

(deftest truncate-ds.4a
  (truncate-check-ds 1.2d0 3.7f0    0 1.2d0)
  t)

(deftest truncate-ds.4b
  (truncate-check-ds -1.2d0 3.7f0    0 -1.2d0)
  t)

(deftest truncate-ds.4c
  (truncate-check-ds 1.2d0 -3.7f0    0 1.2d0)
  t)

(deftest truncate-ds.4d
  (truncate-check-ds -1.2d0 -3.7f0    0 -1.2d0)
  t)

(deftest truncate-ds.5a
  (truncate-check-ds 12.3d0 3.7f0    3 1.2d0)
  t)

(deftest truncate-ds.5b
  (truncate-check-ds -12.3d0 3.7f0    -3 -1.2d0)
  t)

(deftest truncate-ds.5c
  (truncate-check-ds 12.3d0 -3.7f0    -3 1.2d0)
  t)

(deftest truncate-ds.5d
  (truncate-check-ds -12.3d0 -3.7f0    3 -1.2d0)
  t)


;; double-float - double-float
(defun truncate-check-dd (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest truncate-dd.1
  (truncate 0.0d0 10.0d0)
  0 0.0d0)

(deftest truncate-dd.2
  (truncate 0.0d0 -10.0d0)
  0 0.0d0)

(deftest-error truncate-dd.3
  (truncate 10.4d0 0.0d0)
  division-by-zero)

(deftest truncate-dd.4a
  (truncate-check-dd 1.2d0 3.7d0    0 1.2d0)
  t)

(deftest truncate-dd.4b
  (truncate-check-dd -1.2d0 3.7d0    0 -1.2d0)
  t)

(deftest truncate-dd.4c
  (truncate-check-dd 1.2d0 -3.7d0    0 1.2d0)
  t)

(deftest truncate-dd.4d
  (truncate-check-dd -1.2d0 -3.7d0    0 -1.2d0)
  t)

(deftest truncate-dd.5a
  (truncate-check-dd 12.3d0 3.7d0    3 1.2d0)
  t)

(deftest truncate-dd.5b
  (truncate-check-dd -12.3d0 3.7d0    -3 -1.2d0)
  t)

(deftest truncate-dd.5c
  (truncate-check-dd 12.3d0 -3.7d0    -3 1.2d0)
  t)

(deftest truncate-dd.5d
  (truncate-check-dd -12.3d0 -3.7d0    3 -1.2d0)
  t)


;; double-float - long-float
(defun truncate-check-dl (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest truncate-dl.1
  (truncate 0.0d0 10.0l0)
  0 0.0l0)

(deftest truncate-dl.2
  (truncate 0.0d0 -10.0l0)
  0 0.0l0)

(deftest-error truncate-dl.3
  (truncate 10.4d0 0.0l0)
  division-by-zero)

(deftest truncate-dl.4a
  (truncate-check-dl 1.2d0 3.7l0    0 1.2l0)
  t)

(deftest truncate-dl.4b
  (truncate-check-dl -1.2d0 3.7l0    0 -1.2l0)
  t)

(deftest truncate-dl.4c
  (truncate-check-dl 1.2d0 -3.7l0    0 1.2l0)
  t)

(deftest truncate-dl.4d
  (truncate-check-dl -1.2d0 -3.7l0    0 -1.2l0)
  t)

(deftest truncate-dl.5a
  (truncate-check-dl 12.3d0 3.7l0    3 1.2l0)
  t)

(deftest truncate-dl.5b
  (truncate-check-dl -12.3d0 3.7l0    -3 -1.2l0)
  t)

(deftest truncate-dl.5c
  (truncate-check-dl 12.3d0 -3.7l0    -3 1.2l0)
  t)

(deftest truncate-dl.5d
  (truncate-check-dl -12.3d0 -3.7l0    3 -1.2l0)
  t)


;; long-float - fixnum
(defun truncate-check-lf (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-14
    :call #'integerp
    :type 'long-float))

(deftest truncate-lf.1
  (truncate 0.0l0 10)
  0 0.0l0)

(deftest truncate-lf.2
  (truncate 0.0l0 -10)
  0 0.0l0)

(deftest-error truncate-lf.3
  (truncate 10.4l0 0)
  division-by-zero)

(deftest truncate-lf.4a
  (truncate-check-lf 1.2l0 10    0 1.2l0)
  t)

(deftest truncate-lf.4b
  (truncate-check-lf -1.2l0 10    0 -1.2l0)
  t)

(deftest truncate-lf.4c
  (truncate-check-lf 1.2l0 -10    0 1.2l0)
  t)

(deftest truncate-lf.4d
  (truncate-check-lf -1.2l0 -10    0 -1.2l0)
  t)

(deftest truncate-lf.5a
  (truncate-check-lf 10.2l0 4    2 2.2l0)
  t)

(deftest truncate-lf.5b
  (truncate-check-lf -10.2l0 4    -2 -2.2l0)
  t)

(deftest truncate-lf.5c
  (truncate-check-lf 10.2l0 -4    -2 2.2l0)
  t)

(deftest truncate-lf.5d
  (truncate-check-lf -10.2l0 -4    2 -2.2l0)
  t)


;; long-float - bignum
(defun truncate-check-lb (a b c d)
  (truncate-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest truncate-lb.1
  (truncate 0.0l0 (make-bignum 10))
  0 0.0l0)

(deftest truncate-lb.2
  (truncate 0.0l0 (make-bignum -10))
  0 0.0l0)

(deftest-error truncate-lb.3
  (truncate 10.4l0 (make-bignum 0))
  division-by-zero)

(deftest truncate-lb.4a
  (truncate-check-lb 1.2l0 10    0 1.2l0)
  t)

(deftest truncate-lb.4b
  (truncate-check-lb -1.2l0 10    0 -1.2l0)
  t)

(deftest truncate-lb.4c
  (truncate-check-lb 1.2l0 -10    0 1.2l0)
  t)

(deftest truncate-lb.4d
  (truncate-check-lb -1.2l0 -10    0 -1.2l0)
  t)

(deftest truncate-lb.5a
  (truncate-check-lb 10.2l0 4    2 2.2l0)
  t)

(deftest truncate-lb.5b
  (truncate-check-lb -10.2l0 4    -2 -2.2l0)
  t)

(deftest truncate-lb.5c
  (truncate-check-lb 10.2l0 -4    -2 2.2l0)
  t)

(deftest truncate-lb.5d
  (truncate-check-lb -10.2l0 -4    2 -2.2l0)
  t)


;; long-float - ratio
(defun truncate-check-lr (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest truncate-lr.1
  (truncate 0.0l0 10/3)
  0 0.0l0)

(deftest truncate-lr.2
  (truncate 0.0l0 -10/3)
  0 0.0l0)

(deftest-error truncate-lr.3
  (truncate 10.4l0 (make-ratio 0 1))
  division-by-zero)

(deftest truncate-lr.4a
  (truncate-check-lr 1.2l0 15/4    0 1.2l0)
  t)

(deftest truncate-lr.4b
  (truncate-check-lr -1.2l0 15/4    0 -1.2l0)
  t)

(deftest truncate-lr.4c
  (truncate-check-lr 1.2l0 -15/4    0 1.2l0)
  t)

(deftest truncate-lr.4d
  (truncate-check-lr -1.2l0 -15/4    0 -1.2l0)
  t)

(deftest truncate-lr.5a
  (truncate-check-lr 1.2l0 1/4    4 0.2l0)
  t)

(deftest truncate-lr.5b
  (truncate-check-lr -1.2l0 1/4    -4 -0.2l0)
  t)

(deftest truncate-lr.5c
  (truncate-check-lr 1.2l0 -1/4    -4 0.2l0)
  t)

(deftest truncate-lr.5d
  (truncate-check-lr -1.2l0 -1/4    4 -0.2l0)
  t)


;; long-float - single-float
(defun truncate-check-ls (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'long-float))

(deftest truncate-ls.1
  (truncate 0.0l0 10.0f0)
  0 0.0l0)

(deftest truncate-ls.2
  (truncate 0.0l0 -10.0f0)
  0 0.0l0)

(deftest-error truncate-ls.3
  (truncate 10.4l0 0.0f0)
  division-by-zero)

(deftest truncate-ls.4a
  (truncate-check-ls 1.2l0 3.7f0    0 1.2l0)
  t)

(deftest truncate-ls.4b
  (truncate-check-ls -1.2l0 3.7f0    0 -1.2l0)
  t)

(deftest truncate-ls.4c
  (truncate-check-ls 1.2l0 -3.7f0    0 1.2l0)
  t)

(deftest truncate-ls.4d
  (truncate-check-ls -1.2l0 -3.7f0    0 -1.2l0)
  t)

(deftest truncate-ls.5a
  (truncate-check-ls 12.3l0 3.7f0    3 1.2l0)
  t)

(deftest truncate-ls.5b
  (truncate-check-ls -12.3l0 3.7f0    -3 -1.2l0)
  t)

(deftest truncate-ls.5c
  (truncate-check-ls 12.3l0 -3.7f0    -3 1.2l0)
  t)

(deftest truncate-ls.5d
  (truncate-check-ls -12.3l0 -3.7f0    3 -1.2l0)
  t)


;; long-float - double-float
(defun truncate-check-ld (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest truncate-ld.1
  (truncate 0.0l0 10.0d0)
  0 0.0l0)

(deftest truncate-ld.2
  (truncate 0.0l0 -10.0d0)
  0 0.0l0)

(deftest-error truncate-ld.3
  (truncate 10.4l0 0.0d0)
  division-by-zero)

(deftest truncate-ld.4a
  (truncate-check-ld 1.2l0 3.7d0    0 1.2l0)
  t)

(deftest truncate-ld.4b
  (truncate-check-ld -1.2l0 3.7d0    0 -1.2l0)
  t)

(deftest truncate-ld.4c
  (truncate-check-ld 1.2l0 -3.7d0    0 1.2l0)
  t)

(deftest truncate-ld.4d
  (truncate-check-ld -1.2l0 -3.7d0    0 -1.2l0)
  t)

(deftest truncate-ld.5a
  (truncate-check-ld 12.3l0 3.7d0    3 1.2l0)
  t)

(deftest truncate-ld.5b
  (truncate-check-ld -12.3l0 3.7d0    -3 -1.2l0)
  t)

(deftest truncate-ld.5c
  (truncate-check-ld 12.3l0 -3.7d0    -3 1.2l0)
  t)

(deftest truncate-ld.5d
  (truncate-check-ld -12.3l0 -3.7d0    3 -1.2l0)
  t)


;; long-float - long-float
(defun truncate-check-ll (a b c d)
  (truncate-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest truncate-ll.1
  (truncate 0.0l0 10.0l0)
  0 0.0l0)

(deftest truncate-ll.2
  (truncate 0.0l0 -10.0l0)
  0 0.0l0)

(deftest-error truncate-ll.3
  (truncate 10.4l0 0.0l0)
  division-by-zero)

(deftest truncate-ll.4a
  (truncate-check-ll 1.2l0 3.7l0    0 1.2l0)
  t)

(deftest truncate-ll.4b
  (truncate-check-ll -1.2l0 3.7l0    0 -1.2l0)
  t)

(deftest truncate-ll.4c
  (truncate-check-ll 1.2l0 -3.7l0    0 1.2l0)
  t)

(deftest truncate-ll.4d
  (truncate-check-ll -1.2l0 -3.7l0    0 -1.2l0)
  t)

(deftest truncate-ll.5a
  (truncate-check-ll 12.3l0 3.7l0    3 1.2l0)
  t)

(deftest truncate-ll.5b
  (truncate-check-ll -12.3l0 3.7l0    -3 -1.2l0)
  t)

(deftest truncate-ll.5c
  (truncate-check-ll 12.3l0 -3.7l0    -3 1.2l0)
  t)

(deftest truncate-ll.5d
  (truncate-check-ll -12.3l0 -3.7l0    3 -1.2l0)
  t)

