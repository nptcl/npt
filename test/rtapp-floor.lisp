;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(defun floor-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float) (call #'integerp))
  (multiple-value-bind (e f) (floor a b)
    (or (and (funcall call e)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "floor-equal error: (~S ~S) ~A, ~A, ~A" e f
          (funcall call e)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  floor1
;;
(deftest floor1-integer.1
  (floor 0)
  0 0)

(deftest floor1-integer.2
  (floor 10)
  10 0)

(deftest floor1-integer.3
  (floor -10)
  -10 0)

(deftest floor1-integer.4
  (floor 99999999999999999999)
  99999999999999999999 0)

(deftest floor1-integer.5
  (floor -99999999999999999999)
  -99999999999999999999 0)

(deftest floor1-ratio.1
  (floor 1/3)
  0 1/3)

(deftest floor1-ratio.2
  (floor 10/3)
  3 1/3)

(deftest floor1-ratio.3
  (floor -1/3)
  -1 2/3)

(deftest floor1-ratio.4
  (floor -10/3)
  -4 2/3)

(deftest floor1-float.1
  (floor 10.0)
  10 0.0)

(deftest floor1-float.2
  (floor -10.0)
  -10 0.0)

(deftest floor1-float.3
  (floor 12.25)
  12 0.25)

(deftest floor1-float.4
  (floor -12.25)
  -13 0.75)

(deftest floor1-float.5
  (floor 12.25f0)
  12 0.25f0)

(deftest floor1-float.6
  (floor -12.25f0)
  -13 0.75f0)

(deftest floor1-float.7
  (floor 12.25d0)
  12 0.25d0)

(deftest floor1-float.8
  (floor -12.25d0)
  -13 0.75d0)

(deftest floor1-float.9
  (floor 12.25l0)
  12 0.25l0)

(deftest floor1-float.10
  (floor -12.25l0)
  -13 0.75l0)


;;
;;  floor
;;

;;  fixnum - fixnum
(deftest floor-ff.1
  (floor 0 10)
  0 0)

(deftest floor-ff.2
  (floor 0 -10)
  0 0)

(deftest-error floor-ff.3
  (floor 10 0)
  division-by-zero)

(deftest floor-ff.4a
  (floor 3 10)
  0 3)

(deftest floor-ff.4b
  (floor -3 10)
  -1 7)

(deftest floor-ff.4c
  (floor 3 -10)
  -1 -7)

(deftest floor-ff.4d
  (floor -3 -10)
  0 -3)

(deftest floor-ff.5a
  (floor 10 3)
  3 1)

(deftest floor-ff.5b
  (floor -10 3)
  -4 2)

(deftest floor-ff.5c
  (floor 10 -3)
  -4 -2)

(deftest floor-ff.5d
  (floor -10 -3)
  3 -1)

(deftest floor-ff.6a
  (floor 11 3)
  3 2)

(deftest floor-ff.6b
  (floor -11 3)
  -4 1)

(deftest floor-ff.6c
  (floor 11 -3)
  -4 -1)

(deftest floor-ff.6d
  (floor -11 -3)
  3 -2)


;; fixnum - bignum
(deftest floor-fb.1
  (floor 0 (make-bignum 10))
  0 0)

(deftest floor-fb.2
  (floor 0 (make-bignum -10))
  0 0)

(deftest-error floor-fb.3
  (floor 10 (make-bignum 0))
  division-by-zero)

(deftest floor-fb.4a
  (floor 3 (make-bignum 10))
  0 3)

(deftest floor-fb.4b
  (floor -3 (make-bignum 10))
  -1 7)

(deftest floor-fb.4c
  (floor 3 (make-bignum -10))
  -1 -7)

(deftest floor-fb.4d
  (floor -3 (make-bignum -10))
  0 -3)

(deftest floor-fb.5a
  (floor 10 (make-bignum 3))
  3 1)

(deftest floor-fb.5b
  (floor -10 (make-bignum 3))
  -4 2)

(deftest floor-fb.5c
  (floor 10 (make-bignum -3))
  -4 -2)

(deftest floor-fb.5d
  (floor -10 (make-bignum -3))
  3 -1)

(deftest floor-fb.6a
  (floor 11 (make-bignum 3))
  3 2)

(deftest floor-fb.6b
  (floor -11 (make-bignum 3))
  -4 1)

(deftest floor-fb.6c
  (floor 11 (make-bignum -3))
  -4 -1)

(deftest floor-fb.6d
  (floor -11 (make-bignum -3))
  3 -2)


;;  fixnum - ratio
(deftest floor-fr.1
  (floor 0 10/3)
  0 0)

(deftest floor-fr.2
  (floor 0 -10/3)
  0 0)

(deftest-error floor-fr.3
  (floor 10 (make-ratio 0 1))
  division-by-zero)

(deftest floor-fr.4a
  (floor 3 100/7)
  0 3)

(deftest floor-fr.4b
  (floor -3 100/7)
  -1 79/7)

(deftest floor-fr.4c
  (floor 3 -100/7)
  -1 -79/7)

(deftest floor-fr.4d
  (floor -3 -100/7)
  0 -3)

(deftest floor-fr.5a
  (floor 10 6/7)
  11 4/7)

(deftest floor-fr.5b
  (floor -10 6/7)
  -12 2/7)

(deftest floor-fr.5c
  (floor 10 -6/7)
  -12 -2/7)

(deftest floor-fr.5d
  (floor -10 -6/7)
  11 -4/7)

(deftest floor-fr.6a
  (floor 10 1/3)
  30 0)

(deftest floor-fr.6b
  (floor -10 1/3)
  -30 0)

(deftest floor-fr.6c
  (floor 10 -1/3)
  -30 0)

(deftest floor-fr.6d
  (floor -10 -1/3)
  30 0)

(deftest floor-fr.7a
  (floor 10 4/3)
  7 2/3)


;;  fixnum - single-float
(defun floor-check-fs (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest floor-fs.1
  (floor 0 10.0f0)
  0 0.0f0)

(deftest floor-fs.2
  (floor 0 -10.0f0)
  0 0.0f0)

(deftest-error floor-fs.3
  (floor 10 0.0f0)
  division-by-zero)

(deftest floor-fs.4a
  (floor-check-fs 4 10.5f0    0 4.0f0)
  t)

(deftest floor-fs.4b
  (floor-check-fs -4 10.5f0    -1 6.5f0)
  t)

(deftest floor-fs.4c
  (floor-check-fs 4 -10.5f0    -1 -6.5f0)
  t)

(deftest floor-fs.4d
  (floor-check-fs -4 -10.5f0    0 -4.0f0)
  t)

(deftest floor-fs.5a
  (floor-check-fs 15 1.6f0    9 0.6f0)
  t)

(deftest floor-fs.5b
  (floor-check-fs -15 1.6f0    -10 1.0f0)
  t)

(deftest floor-fs.5c
  (floor-check-fs 15 -1.6f0    -10 -1.0f0)
  t)

(deftest floor-fs.5d
  (floor-check-fs -15 -1.6f0    9 -0.6f0)
  t)


;;  fixnum - double-float
(defun floor-check-fd (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest floor-fd.1
  (floor 0 10.0d0)
  0 0.0d0)

(deftest floor-fd.2
  (floor 0 -10.0d0)
  0 0.0d0)

(deftest-error floor-fd.3
  (floor 10 0.0d0)
  division-by-zero)

(deftest floor-fd.4a
  (floor-check-fd 4 10.5d0    0 4.0d0)
  t)

(deftest floor-fd.4b
  (floor-check-fd -4 10.5d0    -1 6.5d0)
  t)

(deftest floor-fd.4c
  (floor-check-fd 4 -10.5d0    -1 -6.5d0)
  t)

(deftest floor-fd.4d
  (floor-check-fd -4 -10.5d0    0 -4.0d0)
  t)

(deftest floor-fd.5a
  (floor-check-fd 15 1.6d0    9 0.6d0)
  t)

(deftest floor-fd.5b
  (floor-check-fd -15 1.6d0    -10 1.0d0)
  t)

(deftest floor-fd.5c
  (floor-check-fd 15 -1.6d0    -10 -1.0d0)
  t)

(deftest floor-fd.5d
  (floor-check-fd -15 -1.6d0    9 -0.6d0)
  t)


;;  fixnum - long-float
(defun floor-check-fl (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest floor-fl.1
  (floor 0 10.0l0)
  0 0.0l0)

(deftest floor-fl.2
  (floor 0 -10.0l0)
  0 0.0l0)

(deftest-error floor-fl.3
  (floor 10 0.0l0)
  division-by-zero)

(deftest floor-fl.4a
  (floor-check-fl 4 10.5l0    0 4.0l0)
  t)

(deftest floor-fl.4b
  (floor-check-fl -4 10.5l0    -1 6.5l0)
  t)

(deftest floor-fl.4c
  (floor-check-fl 4 -10.5l0    -1 -6.5l0)
  t)

(deftest floor-fl.4d
  (floor-check-fl -4 -10.5l0    0 -4.0l0)
  t)

(deftest floor-fl.5a
  (floor-check-fl 15 1.6l0    9 0.6l0)
  t)

(deftest floor-fl.5b
  (floor-check-fl -15 1.6l0    -10 1.0l0)
  t)

(deftest floor-fl.5c
  (floor-check-fl 15 -1.6l0    -10 -1.0l0)
  t)

(deftest floor-fl.5d
  (floor-check-fl -15 -1.6l0    9 -0.6l0)
  t)


;;  bignum - fixnum
(defun floorb (a b)
  (floor (make-bignum a) b))

(deftest floor-bf.1
  (floorb 0 10)
  0 0)

(deftest floor-bf.2
  (floorb 0 -10)
  0 0)

(deftest-error floor-bf.3
  (floorb 10 0)
  division-by-zero)

(deftest floor-bf.4a
  (floorb 3 10)
  0 3)

(deftest floor-bf.4b
  (floorb -3 10)
  -1 7)

(deftest floor-bf.4c
  (floorb 3 -10)
  -1 -7)

(deftest floor-bf.4d
  (floorb -3 -10)
  0 -3)

(deftest floor-bf.5a
  (floorb 10 3)
  3 1)

(deftest floor-bf.5b
  (floorb -10 3)
  -4 2)

(deftest floor-bf.5c
  (floorb 10 -3)
  -4 -2)

(deftest floor-bf.5d
  (floorb -10 -3)
  3 -1)

(deftest floor-bf.6a
  (floorb 11 3)
  3 2)

(deftest floor-bf.6b
  (floorb -11 3)
  -4 1)

(deftest floor-bf.6c
  (floorb 11 -3)
  -4 -1)

(deftest floor-bf.6d
  (floorb -11 -3)
  3 -2)


;; bignum - bignum
(deftest floor-bb.1
  (floorb 0 (make-bignum 10))
  0 0)

(deftest floor-bb.2
  (floorb 0 (make-bignum -10))
  0 0)

(deftest-error floor-bb.3
  (floorb 10 (make-bignum 0))
  division-by-zero)

(deftest floor-bb.4a
  (floorb 3 (make-bignum 10))
  0 3)

(deftest floor-bb.4b
  (floorb -3 (make-bignum 10))
  -1 7)

(deftest floor-bb.4c
  (floorb 3 (make-bignum -10))
  -1 -7)

(deftest floor-bb.4d
  (floorb -3 (make-bignum -10))
  0 -3)

(deftest floor-bb.5a
  (floorb 10 (make-bignum 3))
  3 1)

(deftest floor-bb.5b
  (floorb -10 (make-bignum 3))
  -4 2)

(deftest floor-bb.5c
  (floorb 10 (make-bignum -3))
  -4 -2)

(deftest floor-bb.5d
  (floorb -10 (make-bignum -3))
  3 -1)

(deftest floor-bb.6a
  (floorb 11 (make-bignum 3))
  3 2)

(deftest floor-bb.6b
  (floorb -11 (make-bignum 3))
  -4 1)

(deftest floor-bb.6c
  (floorb 11 (make-bignum -3))
  -4 -1)

(deftest floor-bb.6d
  (floorb -11 (make-bignum -3))
  3 -2)


;;  bignum - ratio
(deftest floor-br.1
  (floorb 0 10/3)
  0 0)

(deftest floor-br.2
  (floorb 0 -10/3)
  0 0)

(deftest-error floor-br.3
  (floorb 10 (make-ratio 0 1))
  division-by-zero)

(deftest floor-br.4a
  (floorb 3 100/7)
  0 3)

(deftest floor-br.4b
  (floorb -3 100/7)
  -1 79/7)

(deftest floor-br.4c
  (floorb 3 -100/7)
  -1 -79/7)

(deftest floor-br.4d
  (floorb -3 -100/7)
  0 -3)

(deftest floor-br.5a
  (floorb 10 6/7)
  11 4/7)

(deftest floor-br.5b
  (floorb -10 6/7)
  -12 2/7)

(deftest floor-br.5c
  (floorb 10 -6/7)
  -12 -2/7)

(deftest floor-br.5d
  (floorb -10 -6/7)
  11 -4/7)

(deftest floor-br.6a
  (floorb 10 1/3)
  30 0)

(deftest floor-br.6b
  (floorb -10 1/3)
  -30 0)

(deftest floor-br.6c
  (floorb 10 -1/3)
  -30 0)

(deftest floor-br.6d
  (floorb -10 -1/3)
  30 0)


;;  bignum - single-float
(defun floor-check-bs (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest floor-bs.1
  (floorb 0 10.0f0)
  0 0.0f0)

(deftest floor-bs.2
  (floorb 0 -10.0f0)
  0 0.0f0)

(deftest-error floor-bs.3
  (floorb 10 0.0f0)
  division-by-zero)

(deftest floor-bs.4a
  (floor-check-bs 4 10.5f0    0 4.0f0)
  t)

(deftest floor-bs.4b
  (floor-check-bs -4 10.5f0    -1 6.5f0)
  t)

(deftest floor-bs.4c
  (floor-check-bs 4 -10.5f0    -1 -6.5f0)
  t)

(deftest floor-bs.4d
  (floor-check-bs -4 -10.5f0    0 -4.0f0)
  t)

(deftest floor-bs.5a
  (floor-check-bs 15 1.6f0    9 0.6f0)
  t)

(deftest floor-bs.5b
  (floor-check-bs -15 1.6f0    -10 1.0f0)
  t)

(deftest floor-bs.5c
  (floor-check-bs 15 -1.6f0    -10 -1.0f0)
  t)

(deftest floor-bs.5d
  (floor-check-bs -15 -1.6f0    9 -0.6f0)
  t)


;;  bignum - double-float
(defun floor-check-bd (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest floor-bd.1
  (floorb 0 10.0d0)
  0 0.0d0)

(deftest floor-bd.2
  (floorb 0 -10.0d0)
  0 0.0d0)

(deftest-error floor-bd.3
  (floorb 10 0.0d0)
  division-by-zero)

(deftest floor-bd.4a
  (floor-check-bd 4 10.5d0    0 4.0d0)
  t)

(deftest floor-bd.4b
  (floor-check-bd -4 10.5d0    -1 6.5d0)
  t)

(deftest floor-bd.4c
  (floor-check-bd 4 -10.5d0    -1 -6.5d0)
  t)

(deftest floor-bd.4d
  (floor-check-bd -4 -10.5d0    0 -4.0d0)
  t)

(deftest floor-bd.5a
  (floor-check-bd 15 1.6d0    9 0.6d0)
  t)

(deftest floor-bd.5b
  (floor-check-bd -15 1.6d0    -10 1.0d0)
  t)

(deftest floor-bd.5c
  (floor-check-bd 15 -1.6d0    -10 -1.0d0)
  t)

(deftest floor-bd.5d
  (floor-check-bd -15 -1.6d0    9 -0.6d0)
  t)


;;  bignum - long-float
(defun floor-check-bl (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'fixnump
    :type 'long-float))

(deftest floor-bl.1
  (floorb 0 10.0l0)
  0 0.0l0)

(deftest floor-bl.2
  (floorb 0 -10.0l0)
  0 0.0l0)

(deftest-error floor-bl.3
  (floorb 10 0.0l0)
  division-by-zero)

(deftest floor-bl.4a
  (floor-check-bl 4 10.5l0    0 4.0l0)
  t)

(deftest floor-bl.4b
  (floor-check-bl -4 10.5l0    -1 6.5l0)
  t)

(deftest floor-bl.4c
  (floor-check-bl 4 -10.5l0    -1 -6.5l0)
  t)

(deftest floor-bl.4d
  (floor-check-bl -4 -10.5l0    0 -4.0l0)
  t)

(deftest floor-bl.5a
  (floor-check-bl 15 1.6l0    9 0.6l0)
  t)

(deftest floor-bl.5b
  (floor-check-bl -15 1.6l0    -10 1.0l0)
  t)

(deftest floor-bl.5c
  (floor-check-bl 15 -1.6l0    -10 -1.0l0)
  t)

(deftest floor-bl.5d
  (floor-check-bl -15 -1.6l0    9 -0.6l0)
  t)


;;  ratio - fixnum
(deftest floor-rf.1
  (floor (make-ratio 0 1) 10)
  0 0)

(deftest floor-rf.2
  (floor (make-ratio 0 1) -10)
  0 0)

(deftest-error floor-rf.3
  (floor 10/3 0)
  division-by-zero)

(deftest floor-rf.4a
  (floor 2/3 5)
  0 2/3)

(deftest floor-rf.4b
  (floor -2/3 5)
  -1 13/3)

(deftest floor-rf.4c
  (floor 2/3 -5)
  -1 -13/3)

(deftest floor-rf.4d
  (floor -2/3 -5)
  0 -2/3)

(deftest floor-rf.5a
  (floor 20/3 5)
  1 5/3)

(deftest floor-rf.5b
  (floor -20/3 5)
  -2 10/3)

(deftest floor-rf.5c
  (floor 20/3 -5)
  -2 -10/3)

(deftest floor-rf.5d
  (floor -20/3 -5)
  1 -5/3)

(deftest floor-rf.6a
  (floor 53/3 8)
  2 5/3)

(deftest floor-rf.6b
  (floor -53/3 8)
  -3 19/3)

(deftest floor-rf.6c
  (floor 53/3 -8)
  -3 -19/3)

(deftest floor-rf.6d
  (floor -53/3 -8)
  2 -5/3)


;; ratio - bignum
(deftest floor-rb.1
  (floor (make-ratio 0 1) (make-bignum 10))
  0 0)

(deftest floor-rb.2
  (floor (make-ratio 0 1) (make-bignum -10))
  0 0)

(deftest-error floor-rb.3
  (floor 10/3 (make-bignum 0))
  division-by-zero)

(deftest floor-rb.4a
  (floor 2/3 (make-bignum 5))
  0 2/3)

(deftest floor-rb.4b
  (floor -2/3 (make-bignum 5))
  -1 13/3)

(deftest floor-rb.4c
  (floor 2/3 (make-bignum -5))
  -1 -13/3)

(deftest floor-rb.4d
  (floor -2/3 (make-bignum -5))
  0 -2/3)

(deftest floor-rb.5a
  (floor 20/3 (make-bignum 5))
  1 5/3)

(deftest floor-rb.5b
  (floor -20/3 (make-bignum 5))
  -2 10/3)

(deftest floor-rb.5c
  (floor 20/3 (make-bignum -5))
  -2 -10/3)

(deftest floor-rb.5d
  (floor -20/3 (make-bignum -5))
  1 -5/3)

(deftest floor-rb.6a
  (floor 53/3 (make-bignum 8))
  2 5/3)

(deftest floor-rb.6b
  (floor -53/3 (make-bignum 8))
  -3 19/3)

(deftest floor-rb.6c
  (floor 53/3 (make-bignum -8))
  -3 -19/3)

(deftest floor-rb.6d
  (floor -53/3 (make-bignum -8))
  2 -5/3)


;;  ratio - ratio
(deftest floor-rr.1
  (floor (make-ratio 0 1) 10/3)
  0 0)

(deftest floor-rr.2
  (floor (make-ratio 0 1) -10/3)
  0 0)

(deftest-error floor-rr.3
  (floor 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest floor-rr.4a
  (floor 1/2 33/5)
  0 1/2)

(deftest floor-rr.4b
  (floor -1/2 33/5)
  -1 61/10)

(deftest floor-rr.4c
  (floor 1/2 -33/5)
  -1 -61/10)

(deftest floor-rr.4d
  (floor -1/2 -33/5)
  0 -1/2)

(deftest floor-rr.5a
  (floor 79/3 2/7)
  92 1/21)

(deftest floor-rr.5b
  (floor -79/3 2/7)
  -93 5/21)

(deftest floor-rr.5c
  (floor 79/3 -2/7)
  -93 -5/21)

(deftest floor-rr.5d
  (floor -79/3 -2/7)
  92 -1/21)

(deftest floor-rr.6a
  (floor 4/5 8/20)
  2 0)

(deftest floor-rr.6b
  (floor -4/5 8/20)
  -2 0)

(deftest floor-rr.6c
  (floor 4/5 -8/20)
  -2 0)

(deftest floor-rr.6d
  (floor -4/5 -8/20)
  2 0)


;;  ratio - single-float
(defun floor-check-rs (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest floor-rs.1
  (floor (make-ratio 0 1) 10.0f0)
  0 0.0f0)

(deftest floor-rs.2
  (floor (make-ratio 0 1) -10.0f0)
  0 0.0f0)

(deftest-error floor-rs.3
  (floor 10/4 0.0f0)
  division-by-zero)

(deftest floor-rs.4a
  (floor-check-rs 3/4 5.5f0    0 0.75f0)
  t)

(deftest floor-rs.4b
  (floor-check-rs -3/4 5.5f0    -1 4.75f0)
  t)

(deftest floor-rs.4c
  (floor-check-rs 3/4 -5.5f0    -1 -4.75f0)
  t)

(deftest floor-rs.4d
  (floor-check-rs -3/4 -5.5f0    0 -0.75f0)
  t)

(deftest floor-rs.5a
  (floor-check-rs 77/4 1.6f0    12 0.05f0)
  t)

(deftest floor-rs.5b
  (floor-check-rs -77/4 1.6f0    -13 1.55f0)
  t)

(deftest floor-rs.5c
  (floor-check-rs 77/4 -1.6f0    -13 -1.55f0)
  t)

(deftest floor-rs.5d
  (floor-check-rs -77/4 -1.6f0    12 -0.05f0)
  t)


;;  ratio - double-float
(defun floor-check-rd (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest floor-rd.1
  (floor (make-ratio 0 1) 10.0d0)
  0 0.0d0)

(deftest floor-rd.2
  (floor (make-ratio 0 1) -10.0d0)
  0 0.0d0)

(deftest-error floor-rd.3
  (floor 10/4 0.0d0)
  division-by-zero)

(deftest floor-rd.4a
  (floor-check-rd 3/4 5.5d0    0 0.75d0)
  t)

(deftest floor-rd.4b
  (floor-check-rd -3/4 5.5d0    -1 4.75d0)
  t)

(deftest floor-rd.4c
  (floor-check-rd 3/4 -5.5d0    -1 -4.75d0)
  t)

(deftest floor-rd.4d
  (floor-check-rd -3/4 -5.5d0    0 -0.75d0)
  t)

(deftest floor-rd.5a
  (floor-check-rd 77/4 1.6d0    12 0.05d0)
  t)

(deftest floor-rd.5b
  (floor-check-rd -77/4 1.6d0    -13 1.55d0)
  t)

(deftest floor-rd.5c
  (floor-check-rd 77/4 -1.6d0    -13 -1.55d0)
  t)

(deftest floor-rd.5d
  (floor-check-rd -77/4 -1.6d0    12 -0.05d0)
  t)


;;  ratio - long-float
(defun floor-check-rl (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :call #'integerp
    :type 'long-float))

(deftest floor-rl.1
  (floor (make-ratio 0 1) 10.0l0)
  0 0.0l0)

(deftest floor-rl.2
  (floor (make-ratio 0 1) -10.0l0)
  0 0.0l0)

(deftest-error floor-rl.3
  (floor 10/4 0.0l0)
  division-by-zero)

(deftest floor-rl.4a
  (floor-check-rl 3/4 5.5l0    0 0.75l0)
  t)

(deftest floor-rl.4b
  (floor-check-rl -3/4 5.5l0    -1 4.75l0)
  t)

(deftest floor-rl.4c
  (floor-check-rl 3/4 -5.5l0    -1 -4.75l0)
  t)

(deftest floor-rl.4d
  (floor-check-rl -3/4 -5.5l0    0 -0.75l0)
  t)

(deftest floor-rl.5a
  (floor-check-rl 77/4 1.6l0    12 0.05l0)
  t)

(deftest floor-rl.5b
  (floor-check-rl -77/4 1.6l0    -13 1.55l0)
  t)

(deftest floor-rl.5c
  (floor-check-rl 77/4 -1.6l0    -13 -1.55l0)
  t)

(deftest floor-rl.5d
  (floor-check-rl -77/4 -1.6l0    12 -0.05l0)
  t)


;; single-float - fixnum
(defun floor-check-sf (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest floor-sf.1
  (floor 0.0f0 10)
  0 0.0f0)

(deftest floor-sf.2
  (floor 0.0f0 -10)
  0 0.0f0)

(deftest-error floor-sf.3
  (floor 10.4f0 0)
  division-by-zero)

(deftest floor-sf.4a
  (floor-check-sf 1.2f0 10    0 1.2f0)
  t)

(deftest floor-sf.4b
  (floor-check-sf -1.2f0 10    -1 8.8f0)
  t)

(deftest floor-sf.4c
  (floor-check-sf 1.2f0 -10    -1 -8.8f0)
  t)

(deftest floor-sf.4d
  (floor-check-sf -1.2f0 -10    0 -1.2f0)
  t)

(deftest floor-sf.5a
  (floor-check-sf 10.2f0 4    2 2.2f0)
  t)

(deftest floor-sf.5b
  (floor-check-sf -10.2f0 4    -3 1.8f0)
  t)

(deftest floor-sf.5c
  (floor-check-sf 10.2f0 -4    -3 -1.8f0)
  t)

(deftest floor-sf.5d
  (floor-check-sf -10.2f0 -4    2 -2.2f0)
  t)


;; single-float - bignum
(defun floor-check-sb (a b c d)
  (floor-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest floor-sb.1
  (floor 0.0f0 (make-bignum 10))
  0 0.0f0)

(deftest floor-sb.2
  (floor 0.0f0 (make-bignum -10))
  0 0.0f0)

(deftest-error floor-sb.3
  (floor 10.4f0 (make-bignum 0))
  division-by-zero)

(deftest floor-sb.4a
  (floor-check-sb 1.2f0 10    0 1.2f0)
  t)

(deftest floor-sb.4b
  (floor-check-sb -1.2f0 10    -1 8.8f0)
  t)

(deftest floor-sb.4c
  (floor-check-sb 1.2f0 -10    -1 -8.8f0)
  t)

(deftest floor-sb.4d
  (floor-check-sb -1.2f0 -10    0 -1.2f0)
  t)

(deftest floor-sb.5a
  (floor-check-sb 10.2f0 4    2 2.2f0)
  t)

(deftest floor-sb.5b
  (floor-check-sb -10.2f0 4    -3 1.8f0)
  t)

(deftest floor-sb.5c
  (floor-check-sb 10.2f0 -4    -3 -1.8f0)
  t)

(deftest floor-sb.5d
  (floor-check-sb -10.2f0 -4    2 -2.2f0)
  t)


;; single-float - ratio
(defun floor-check-sr (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest floor-sr.1
  (floor 0.0f0 10/3)
  0 0.0f0)

(deftest floor-sr.2
  (floor 0.0f0 -10/3)
  0 0.0f0)

(deftest-error floor-sr.3
  (floor 10.4f0 (make-ratio 0 1))
  division-by-zero)

(deftest floor-sr.4a
  (floor-check-sr 1.2f0 15/4    0 1.2f0)
  t)

(deftest floor-sr.4b
  (floor-check-sr -1.2f0 15/4    -1 2.55f0)
  t)

(deftest floor-sr.4c
  (floor-check-sr 1.2f0 -15/4    -1 -2.55f0)
  t)

(deftest floor-sr.4d
  (floor-check-sr -1.2f0 -15/4    0 -1.2f0)
  t)

(deftest floor-sr.5a
  (floor-check-sr 1.2f0 1/4    4 0.2f0)
  t)

(deftest floor-sr.5b
  (floor-check-sr -1.2f0 1/4    -5 0.05f0)
  t)

(deftest floor-sr.5c
  (floor-check-sr 1.2f0 -1/4    -5 -0.05f0)
  t)

(deftest floor-sr.5d
  (floor-check-sr -1.2f0 -1/4    4 -0.2f0)
  t)


;; single-float - single-float
(defun floor-check-ss (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'single-float))

(deftest floor-ss.1
  (floor 0.0f0 10.0f0)
  0 0.0f0)

(deftest floor-ss.2
  (floor 0.0f0 -10.0f0)
  0 0.0f0)

(deftest-error floor-ss.3
  (floor 10.4f0 0.0f0)
  division-by-zero)

(deftest floor-ss.4a
  (floor-check-ss 1.2f0 3.7f0    0 1.2f0)
  t)

(deftest floor-ss.4b
  (floor-check-ss -1.2f0 3.7f0    -1 2.5f0)
  t)

(deftest floor-ss.4c
  (floor-check-ss 1.2f0 -3.7f0    -1 -2.5f0)
  t)

(deftest floor-ss.4d
  (floor-check-ss -1.2f0 -3.7f0    0 -1.2f0)
  t)

(deftest floor-ss.5a
  (floor-check-ss 12.3f0 3.7f0    3 1.2f0)
  t)

(deftest floor-ss.5b
  (floor-check-ss -12.3f0 3.7f0    -4 2.5f0)
  t)

(deftest floor-ss.5c
  (floor-check-ss 12.3f0 -3.7f0    -4 -2.5f0)
  t)

(deftest floor-ss.5d
  (floor-check-ss -12.3f0 -3.7f0    3 -1.2f0)
  t)


;; single-float - double-float
(defun floor-check-sd (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :call #'integerp
    :type 'double-float))

(deftest floor-sd.1
  (floor 0.0f0 10.0d0)
  0 0.0d0)

(deftest floor-sd.2
  (floor 0.0f0 -10.0d0)
  0 0.0d0)

(deftest-error floor-sd.3
  (floor 10.4f0 0.0d0)
  division-by-zero)

(deftest floor-sd.4a
  (floor-check-sd 1.2f0 3.7d0    0 1.2d0)
  t)

(deftest floor-sd.4b
  (floor-check-sd -1.2f0 3.7d0    -1 2.5d0)
  t)

(deftest floor-sd.4c
  (floor-check-sd 1.2f0 -3.7d0    -1 -2.5d0)
  t)

(deftest floor-sd.4d
  (floor-check-sd -1.2f0 -3.7d0    0 -1.2d0)
  t)

(deftest floor-sd.5a
  (floor-check-sd 12.3f0 3.7d0    3 1.2d0)
  t)

(deftest floor-sd.5b
  (floor-check-sd -12.3f0 3.7d0    -4 2.5d0)
  t)

(deftest floor-sd.5c
  (floor-check-sd 12.3f0 -3.7d0    -4 -2.5d0)
  t)

(deftest floor-sd.5d
  (floor-check-sd -12.3f0 -3.7d0    3 -1.2d0)
  t)


;; single-float - long-float
(defun floor-check-sl (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :call #'integerp
    :type 'long-float))

(deftest floor-sl.1
  (floor 0.0f0 10.0l0)
  0 0.0l0)

(deftest floor-sl.2
  (floor 0.0f0 -10.0l0)
  0 0.0l0)

(deftest-error floor-sl.3
  (floor 10.4f0 0.0l0)
  division-by-zero)

(deftest floor-sl.4a
  (floor-check-sl 1.2f0 3.7l0    0 1.2l0)
  t)

(deftest floor-sl.4b
  (floor-check-sl -1.2f0 3.7l0    -1 2.5l0)
  t)

(deftest floor-sl.4c
  (floor-check-sl 1.2f0 -3.7l0    -1 -2.5l0)
  t)

(deftest floor-sl.4d
  (floor-check-sl -1.2f0 -3.7l0    0 -1.2l0)
  t)

(deftest floor-sl.5a
  (floor-check-sl 12.3f0 3.7l0    3 1.2l0)
  t)

(deftest floor-sl.5b
  (floor-check-sl -12.3f0 3.7l0    -4 2.5l0)
  t)

(deftest floor-sl.5c
  (floor-check-sl 12.3f0 -3.7l0    -4 -2.5l0)
  t)

(deftest floor-sl.5d
  (floor-check-sl -12.3f0 -3.7l0    3 -1.2l0)
  t)


;; double-float - fixnum
(defun floor-check-df (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-14
    :call #'integerp
    :type 'double-float))

(deftest floor-df.1
  (floor 0.0d0 10)
  0 0.0d0)

(deftest floor-df.2
  (floor 0.0d0 -10)
  0 0.0d0)

(deftest-error floor-df.3
  (floor 10.4d0 0)
  division-by-zero)

(deftest floor-df.4a
  (floor-check-df 1.2d0 10    0 1.2d0)
  t)

(deftest floor-df.4b
  (floor-check-df -1.2d0 10    -1 8.8d0)
  t)

(deftest floor-df.4c
  (floor-check-df 1.2d0 -10    -1 -8.8d0)
  t)

(deftest floor-df.4d
  (floor-check-df -1.2d0 -10    0 -1.2d0)
  t)

(deftest floor-df.5a
  (floor-check-df 10.2d0 4    2 2.2d0)
  t)

(deftest floor-df.5b
  (floor-check-df -10.2d0 4    -3 1.8d0)
  t)

(deftest floor-df.5c
  (floor-check-df 10.2d0 -4    -3 -1.8d0)
  t)

(deftest floor-df.5d
  (floor-check-df -10.2d0 -4    2 -2.2d0)
  t)


;; double-float - bignum
(defun floor-check-db (a b c d)
  (floor-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest floor-db.1
  (floor 0.0d0 (make-bignum 10))
  0 0.0d0)

(deftest floor-db.2
  (floor 0.0d0 (make-bignum -10))
  0 0.0d0)

(deftest-error floor-db.3
  (floor 10.4d0 (make-bignum 0))
  division-by-zero)

(deftest floor-db.4a
  (floor-check-db 1.2d0 10    0 1.2d0)
  t)

(deftest floor-db.4b
  (floor-check-db -1.2d0 10    -1 8.8d0)
  t)

(deftest floor-db.4c
  (floor-check-db 1.2d0 -10    -1 -8.8d0)
  t)

(deftest floor-db.4d
  (floor-check-db -1.2d0 -10    0 -1.2d0)
  t)

(deftest floor-db.5a
  (floor-check-db 10.2d0 4    2 2.2d0)
  t)

(deftest floor-db.5b
  (floor-check-db -10.2d0 4    -3 1.8d0)
  t)

(deftest floor-db.5c
  (floor-check-db 10.2d0 -4    -3 -1.8d0)
  t)

(deftest floor-db.5d
  (floor-check-db -10.2d0 -4    2 -2.2d0)
  t)


;; double-float - ratio
(defun floor-check-dr (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest floor-dr.1
  (floor 0.0d0 10/3)
  0 0.0d0)

(deftest floor-dr.2
  (floor 0.0d0 -10/3)
  0 0.0d0)

(deftest-error floor-dr.3
  (floor 10.4d0 (make-ratio 0 1))
  division-by-zero)

(deftest floor-dr.4a
  (floor-check-dr 1.2d0 15/4    0 1.2d0)
  t)

(deftest floor-dr.4b
  (floor-check-dr -1.2d0 15/4    -1 2.55d0)
  t)

(deftest floor-dr.4c
  (floor-check-dr 1.2d0 -15/4    -1 -2.55d0)
  t)

(deftest floor-dr.4d
  (floor-check-dr -1.2d0 -15/4    0 -1.2d0)
  t)

(deftest floor-dr.5a
  (floor-check-dr 1.2d0 1/4    4 0.2d0)
  t)

(deftest floor-dr.5b
  (floor-check-dr -1.2d0 1/4    -5 0.05d0)
  t)

(deftest floor-dr.5c
  (floor-check-dr 1.2d0 -1/4    -5 -0.05d0)
  t)

(deftest floor-dr.5d
  (floor-check-dr -1.2d0 -1/4    4 -0.2d0)
  t)


;; double-float - single-float
(defun floor-check-ds (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'double-float))

(deftest floor-ds.1
  (floor 0.0d0 10.0f0)
  0 0.0d0)

(deftest floor-ds.2
  (floor 0.0d0 -10.0f0)
  0 0.0d0)

(deftest-error floor-ds.3
  (floor 10.4d0 0.0f0)
  division-by-zero)

(deftest floor-ds.4a
  (floor-check-ds 1.2d0 3.7f0    0 1.2d0)
  t)

(deftest floor-ds.4b
  (floor-check-ds -1.2d0 3.7f0    -1 2.5d0)
  t)

(deftest floor-ds.4c
  (floor-check-ds 1.2d0 -3.7f0    -1 -2.5d0)
  t)

(deftest floor-ds.4d
  (floor-check-ds -1.2d0 -3.7f0    0 -1.2d0)
  t)

(deftest floor-ds.5a
  (floor-check-ds 12.3d0 3.7f0    3 1.2d0)
  t)

(deftest floor-ds.5b
  (floor-check-ds -12.3d0 3.7f0    -4 2.5d0)
  t)

(deftest floor-ds.5c
  (floor-check-ds 12.3d0 -3.7f0    -4 -2.5d0)
  t)

(deftest floor-ds.5d
  (floor-check-ds -12.3d0 -3.7f0    3 -1.2d0)
  t)


;; double-float - double-float
(defun floor-check-dd (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'double-float))

(deftest floor-dd.1
  (floor 0.0d0 10.0d0)
  0 0.0d0)

(deftest floor-dd.2
  (floor 0.0d0 -10.0d0)
  0 0.0d0)

(deftest-error floor-dd.3
  (floor 10.4d0 0.0d0)
  division-by-zero)

(deftest floor-dd.4a
  (floor-check-dd 1.2d0 3.7d0    0 1.2d0)
  t)

(deftest floor-dd.4b
  (floor-check-dd -1.2d0 3.7d0    -1 2.5d0)
  t)

(deftest floor-dd.4c
  (floor-check-dd 1.2d0 -3.7d0    -1 -2.5d0)
  t)

(deftest floor-dd.4d
  (floor-check-dd -1.2d0 -3.7d0    0 -1.2d0)
  t)

(deftest floor-dd.5a
  (floor-check-dd 12.3d0 3.7d0    3 1.2d0)
  t)

(deftest floor-dd.5b
  (floor-check-dd -12.3d0 3.7d0    -4 2.5d0)
  t)

(deftest floor-dd.5c
  (floor-check-dd 12.3d0 -3.7d0    -4 -2.5d0)
  t)

(deftest floor-dd.5d
  (floor-check-dd -12.3d0 -3.7d0    3 -1.2d0)
  t)


;; double-float - long-float
(defun floor-check-dl (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest floor-dl.1
  (floor 0.0d0 10.0l0)
  0 0.0l0)

(deftest floor-dl.2
  (floor 0.0d0 -10.0l0)
  0 0.0l0)

(deftest-error floor-dl.3
  (floor 10.4d0 0.0l0)
  division-by-zero)

(deftest floor-dl.4a
  (floor-check-dl 1.2d0 3.7l0    0 1.2l0)
  t)

(deftest floor-dl.4b
  (floor-check-dl -1.2d0 3.7l0    -1 2.5l0)
  t)

(deftest floor-dl.4c
  (floor-check-dl 1.2d0 -3.7l0    -1 -2.5l0)
  t)

(deftest floor-dl.4d
  (floor-check-dl -1.2d0 -3.7l0    0 -1.2l0)
  t)

(deftest floor-dl.5a
  (floor-check-dl 12.3d0 3.7l0    3 1.2l0)
  t)

(deftest floor-dl.5b
  (floor-check-dl -12.3d0 3.7l0    -4 2.5l0)
  t)

(deftest floor-dl.5c
  (floor-check-dl 12.3d0 -3.7l0    -4 -2.5l0)
  t)

(deftest floor-dl.5d
  (floor-check-dl -12.3d0 -3.7l0    3 -1.2l0)
  t)


;; long-float - fixnum
(defun floor-check-lf (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-14
    :call #'integerp
    :type 'long-float))

(deftest floor-lf.1
  (floor 0.0l0 10)
  0 0.0l0)

(deftest floor-lf.2
  (floor 0.0l0 -10)
  0 0.0l0)

(deftest-error floor-lf.3
  (floor 10.4l0 0)
  division-by-zero)

(deftest floor-lf.4a
  (floor-check-lf 1.2l0 10    0 1.2l0)
  t)

(deftest floor-lf.4b
  (floor-check-lf -1.2l0 10    -1 8.8l0)
  t)

(deftest floor-lf.4c
  (floor-check-lf 1.2l0 -10    -1 -8.8l0)
  t)

(deftest floor-lf.4d
  (floor-check-lf -1.2l0 -10    0 -1.2l0)
  t)

(deftest floor-lf.5a
  (floor-check-lf 10.2l0 4    2 2.2l0)
  t)

(deftest floor-lf.5b
  (floor-check-lf -10.2l0 4    -3 1.8l0)
  t)

(deftest floor-lf.5c
  (floor-check-lf 10.2l0 -4    -3 -1.8l0)
  t)

(deftest floor-lf.5d
  (floor-check-lf -10.2l0 -4    2 -2.2l0)
  t)


;; long-float - bignum
(defun floor-check-lb (a b c d)
  (floor-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest floor-lb.1
  (floor 0.0l0 (make-bignum 10))
  0 0.0l0)

(deftest floor-lb.2
  (floor 0.0l0 (make-bignum -10))
  0 0.0l0)

(deftest-error floor-lb.3
  (floor 10.4l0 (make-bignum 0))
  division-by-zero)

(deftest floor-lb.4a
  (floor-check-lb 1.2l0 10    0 1.2l0)
  t)

(deftest floor-lb.4b
  (floor-check-lb -1.2l0 10    -1 8.8l0)
  t)

(deftest floor-lb.4c
  (floor-check-lb 1.2l0 -10    -1 -8.8l0)
  t)

(deftest floor-lb.4d
  (floor-check-lb -1.2l0 -10    0 -1.2l0)
  t)

(deftest floor-lb.5a
  (floor-check-lb 10.2l0 4    2 2.2l0)
  t)

(deftest floor-lb.5b
  (floor-check-lb -10.2l0 4    -3 1.8l0)
  t)

(deftest floor-lb.5c
  (floor-check-lb 10.2l0 -4    -3 -1.8l0)
  t)

(deftest floor-lb.5d
  (floor-check-lb -10.2l0 -4    2 -2.2l0)
  t)


;; long-float - ratio
(defun floor-check-lr (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest floor-lr.1
  (floor 0.0l0 10/3)
  0 0.0l0)

(deftest floor-lr.2
  (floor 0.0l0 -10/3)
  0 0.0l0)

(deftest-error floor-lr.3
  (floor 10.4l0 (make-ratio 0 1))
  division-by-zero)

(deftest floor-lr.4a
  (floor-check-lr 1.2l0 15/4    0 1.2l0)
  t)

(deftest floor-lr.4b
  (floor-check-lr -1.2l0 15/4    -1 2.55l0)
  t)

(deftest floor-lr.4c
  (floor-check-lr 1.2l0 -15/4    -1 -2.55l0)
  t)

(deftest floor-lr.4d
  (floor-check-lr -1.2l0 -15/4    0 -1.2l0)
  t)

(deftest floor-lr.5a
  (floor-check-lr 1.2l0 1/4    4 0.2l0)
  t)

(deftest floor-lr.5b
  (floor-check-lr -1.2l0 1/4    -5 0.05l0)
  t)

(deftest floor-lr.5c
  (floor-check-lr 1.2l0 -1/4    -5 -0.05l0)
  t)

(deftest floor-lr.5d
  (floor-check-lr -1.2l0 -1/4    4 -0.2l0)
  t)


;; long-float - single-float
(defun floor-check-ls (a b c d)
  (floor-equal
    a b c d
    :eps 1.0e-6
    :call #'integerp
    :type 'long-float))

(deftest floor-ls.1
  (floor 0.0l0 10.0f0)
  0 0.0l0)

(deftest floor-ls.2
  (floor 0.0l0 -10.0f0)
  0 0.0l0)

(deftest-error floor-ls.3
  (floor 10.4l0 0.0f0)
  division-by-zero)

(deftest floor-ls.4a
  (floor-check-ls 1.2l0 3.7f0    0 1.2l0)
  t)

(deftest floor-ls.4b
  (floor-check-ls -1.2l0 3.7f0    -1 2.5l0)
  t)

(deftest floor-ls.4c
  (floor-check-ls 1.2l0 -3.7f0    -1 -2.5l0)
  t)

(deftest floor-ls.4d
  (floor-check-ls -1.2l0 -3.7f0    0 -1.2l0)
  t)

(deftest floor-ls.5a
  (floor-check-ls 12.3l0 3.7f0    3 1.2l0)
  t)

(deftest floor-ls.5b
  (floor-check-ls -12.3l0 3.7f0    -4 2.5l0)
  t)

(deftest floor-ls.5c
  (floor-check-ls 12.3l0 -3.7f0    -4 -2.5l0)
  t)

(deftest floor-ls.5d
  (floor-check-ls -12.3l0 -3.7f0    3 -1.2l0)
  t)


;; long-float - double-float
(defun floor-check-ld (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest floor-ld.1
  (floor 0.0l0 10.0d0)
  0 0.0l0)

(deftest floor-ld.2
  (floor 0.0l0 -10.0d0)
  0 0.0l0)

(deftest-error floor-ld.3
  (floor 10.4l0 0.0d0)
  division-by-zero)

(deftest floor-ld.4a
  (floor-check-ld 1.2l0 3.7d0    0 1.2l0)
  t)

(deftest floor-ld.4b
  (floor-check-ld -1.2l0 3.7d0    -1 2.5l0)
  t)

(deftest floor-ld.4c
  (floor-check-ld 1.2l0 -3.7d0    -1 -2.5l0)
  t)

(deftest floor-ld.4d
  (floor-check-ld -1.2l0 -3.7d0    0 -1.2l0)
  t)

(deftest floor-ld.5a
  (floor-check-ld 12.3l0 3.7d0    3 1.2l0)
  t)

(deftest floor-ld.5b
  (floor-check-ld -12.3l0 3.7d0    -4 2.5l0)
  t)

(deftest floor-ld.5c
  (floor-check-ld 12.3l0 -3.7d0    -4 -2.5l0)
  t)

(deftest floor-ld.5d
  (floor-check-ld -12.3l0 -3.7d0    3 -1.2l0)
  t)


;; long-float - long-float
(defun floor-check-ll (a b c d)
  (floor-equal
    a b c d
    :eps 1.0d-14
    :call #'integerp
    :type 'long-float))

(deftest floor-ll.1
  (floor 0.0l0 10.0l0)
  0 0.0l0)

(deftest floor-ll.2
  (floor 0.0l0 -10.0l0)
  0 0.0l0)

(deftest-error floor-ll.3
  (floor 10.4l0 0.0l0)
  division-by-zero)

(deftest floor-ll.4a
  (floor-check-ll 1.2l0 3.7l0    0 1.2l0)
  t)

(deftest floor-ll.4b
  (floor-check-ll -1.2l0 3.7l0    -1 2.5l0)
  t)

(deftest floor-ll.4c
  (floor-check-ll 1.2l0 -3.7l0    -1 -2.5l0)
  t)

(deftest floor-ll.4d
  (floor-check-ll -1.2l0 -3.7l0    0 -1.2l0)
  t)

(deftest floor-ll.5a
  (floor-check-ll 12.3l0 3.7l0    3 1.2l0)
  t)

(deftest floor-ll.5b
  (floor-check-ll -12.3l0 3.7l0    -4 2.5l0)
  t)

(deftest floor-ll.5c
  (floor-check-ll 12.3l0 -3.7l0    -4 -2.5l0)
  t)

(deftest floor-ll.5d
  (floor-check-ll -12.3l0 -3.7l0    3 -1.2l0)
  t)

