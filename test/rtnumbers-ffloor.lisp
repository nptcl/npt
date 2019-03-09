;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(defun ffloor-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float))
  (multiple-value-bind (e f) (ffloor a b)
    (or (and (typep e type)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "ffloor-equal error: (~S ~S) ~A, ~A, ~A" e f
          (typep e type)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  ffloor1
;;
(deftest ffloor1-integer.1
  (ffloor 0)
  0.0 0)

(deftest ffloor1-integer.2
  (ffloor -10)
  -10.0 0)

(deftest ffloor1-integer.3
  (ffloor -10)
  -10.0 0)

(deftest ffloor1-integer.4
  (ffloor 99999999999999999999)
  99999999999999999999.0 0)

(deftest ffloor1-integer.5
  (ffloor -99999999999999999999)
  -99999999999999999999.0 0)

(deftest ffloor1-ratio.1
  (ffloor 1/3)
  0.0 1/3)

(deftest ffloor1-ratio.2
  (ffloor 10/3)
  3.0 1/3)

(deftest ffloor1-ratio.3
  (ffloor -1/3)
  -1.0 2/3)

(deftest ffloor1-ratio.4
  (ffloor -10/3)
  -4.0 2/3)

(deftest ffloor1-float.1
  (ffloor 10.0)
  10.0 0.0)

(deftest ffloor1-float.2
  (ffloor -10.0)
  -10.0 0.0)

(deftest ffloor1-float.3
  (ffloor 12.25)
  12.0 0.25)

(deftest ffloor1-float.4
  (ffloor -12.25)
  -13.0 0.75)

(deftest ffloor1-float.5
  (ffloor 12.25f0)
  12.0f0 0.25f0)

(deftest ffloor1-float.6
  (ffloor -12.25f0)
  -13.0f0 0.75f0)

(deftest ffloor1-float.7
  (ffloor 12.25d0)
  12.0d0 0.25d0)

(deftest ffloor1-float.8
  (ffloor -12.25d0)
  -13.0d0 0.75d0)

(deftest ffloor1-float.9
  (ffloor 12.25l0)
  12.0l0 0.25l0)

(deftest ffloor1-float.10
  (ffloor -12.25l0)
  -13.0l0 0.75l0)


;;
;;  ffloor
;;

;;  fixnum - fixnum
(deftest ffloor-ff.1
  (ffloor 0 10)
  0.0 0)

(deftest ffloor-ff.2
  (ffloor 0 -10)
  0.0 0)

(deftest-error ffloor-ff.3
  (ffloor 10 0)
  division-by-zero)

(deftest ffloor-ff.4a
  (ffloor 3 10)
  0.0 3)

(deftest ffloor-ff.4b
  (ffloor -3 10)
  -1.0 7)

(deftest ffloor-ff.4c
  (ffloor 3 -10)
  -1.0 -7)

(deftest ffloor-ff.4d
  (ffloor -3 -10)
  0.0 -3)

(deftest ffloor-ff.5a
  (ffloor 10 3)
  3.0 1)

(deftest ffloor-ff.5b
  (ffloor -10 3)
  -4.0 2)

(deftest ffloor-ff.5c
  (ffloor 10 -3)
  -4.0 -2)

(deftest ffloor-ff.5d
  (ffloor -10 -3)
  3.0 -1)

(deftest ffloor-ff.6a
  (ffloor 11 3)
  3.0 2)

(deftest ffloor-ff.6b
  (ffloor -11 3)
  -4.0 1)

(deftest ffloor-ff.6c
  (ffloor 11 -3)
  -4.0 -1)

(deftest ffloor-ff.6d
  (ffloor -11 -3)
  3.0 -2)


;; fixnum - bignum
(deftest ffloor-fb.1
  (ffloor 0 (make-bignum 10))
  0.0 0)

(deftest ffloor-fb.2
  (ffloor 0 (make-bignum -10))
  0.0 0)

(deftest-error ffloor-fb.3
  (ffloor 10 (make-bignum 0))
  division-by-zero)

(deftest ffloor-fb.4a
  (ffloor 3 (make-bignum 10))
  0.0 3)

(deftest ffloor-fb.4b
  (ffloor -3 (make-bignum 10))
  -1.0 7)

(deftest ffloor-fb.4c
  (ffloor 3 (make-bignum -10))
  -1.0 -7)

(deftest ffloor-fb.4d
  (ffloor -3 (make-bignum -10))
  0.0 -3)

(deftest ffloor-fb.5a
  (ffloor 10 (make-bignum 3))
  3.0 1)

(deftest ffloor-fb.5b
  (ffloor -10 (make-bignum 3))
  -4.0 2)

(deftest ffloor-fb.5c
  (ffloor 10 (make-bignum -3))
  -4.0 -2)

(deftest ffloor-fb.5d
  (ffloor -10 (make-bignum -3))
  3.0 -1)

(deftest ffloor-fb.6a
  (ffloor 11 (make-bignum 3))
  3.0 2)

(deftest ffloor-fb.6b
  (ffloor -11 (make-bignum 3))
  -4.0 1)

(deftest ffloor-fb.6c
  (ffloor 11 (make-bignum -3))
  -4.0 -1)

(deftest ffloor-fb.6d
  (ffloor -11 (make-bignum -3))
  3.0 -2)


;;  fixnum - ratio
(deftest ffloor-fr.1
  (ffloor 0 10/3)
  0.0 0)

(deftest ffloor-fr.2
  (ffloor 0 -10/3)
  0.0 0)

(deftest-error ffloor-fr.3
  (ffloor 10 (make-ratio 0 1))
  division-by-zero)

(deftest ffloor-fr.4a
  (ffloor 3 100/7)
  0.0 3)

(deftest ffloor-fr.4b
  (ffloor -3 100/7)
  -1.0 79/7)

(deftest ffloor-fr.4c
  (ffloor 3 -100/7)
  -1.0 -79/7)

(deftest ffloor-fr.4d
  (ffloor -3 -100/7)
  0.0 -3)

(deftest ffloor-fr.5a
  (ffloor 10 6/7)
  11.0 4/7)

(deftest ffloor-fr.5b
  (ffloor -10 6/7)
  -12.0 2/7)

(deftest ffloor-fr.5c
  (ffloor 10 -6/7)
  -12.0 -2/7)

(deftest ffloor-fr.5d
  (ffloor -10 -6/7)
  11.0 -4/7)

(deftest ffloor-fr.6a
  (ffloor 10 1/3)
  30.0 0)

(deftest ffloor-fr.6b
  (ffloor -10 1/3)
  -30.0 0)

(deftest ffloor-fr.6c
  (ffloor 10 -1/3)
  -30.0 0)

(deftest ffloor-fr.6d
  (ffloor -10 -1/3)
  30.0 0)

(deftest ffloor-fr.7a
  (ffloor 10 4/3)
  7.0 2/3)


;;  fixnum - single-float
(defun ffloor-check-fs (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-fs.1
  (ffloor 0 10.0f0)
  0.0f0 0.0f0)

(deftest ffloor-fs.2
  (ffloor 0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error ffloor-fs.3
  (ffloor 10 0.0f0)
  division-by-zero)

(deftest ffloor-fs.4a
  (ffloor-check-fs 4 10.5f0    0.0f0 4.0f0)
  t)

(deftest ffloor-fs.4b
  (ffloor-check-fs -4 10.5f0    -1.0f0 6.5f0)
  t)

(deftest ffloor-fs.4c
  (ffloor-check-fs 4 -10.5f0    -1.0f0 -6.5f0)
  t)

(deftest ffloor-fs.4d
  (ffloor-check-fs -4 -10.5f0    0.0f0 -4.0f0)
  t)

(deftest ffloor-fs.5a
  (ffloor-check-fs 15 1.6f0    9.0f0 0.6f0)
  t)

(deftest ffloor-fs.5b
  (ffloor-check-fs -15 1.6f0    -10.0f0 1.0f0)
  t)

(deftest ffloor-fs.5c
  (ffloor-check-fs 15 -1.6f0    -10.0f0 -1.0f0)
  t)

(deftest ffloor-fs.5d
  (ffloor-check-fs -15 -1.6f0    9.0f0 -0.6f0)
  t)


;;  fixnum - double-float
(defun ffloor-check-fd (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ffloor-fd.1
  (ffloor 0 10.0d0)
  0.0d0 0.0d0)

(deftest ffloor-fd.2
  (ffloor 0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ffloor-fd.3
  (ffloor 10 0.0d0)
  division-by-zero)

(deftest ffloor-fd.4a
  (ffloor-check-fd 4 10.5d0    0.0d0 4.0d0)
  t)

(deftest ffloor-fd.4b
  (ffloor-check-fd -4 10.5d0    -1.0d0 6.5d0)
  t)

(deftest ffloor-fd.4c
  (ffloor-check-fd 4 -10.5d0    -1.0d0 -6.5d0)
  t)

(deftest ffloor-fd.4d
  (ffloor-check-fd -4 -10.5d0    0.0d0 -4.0d0)
  t)

(deftest ffloor-fd.5a
  (ffloor-check-fd 15 1.6d0    9.0d0 0.6d0)
  t)

(deftest ffloor-fd.5b
  (ffloor-check-fd -15 1.6d0    -10.0d0 1.0d0)
  t)

(deftest ffloor-fd.5c
  (ffloor-check-fd 15 -1.6d0    -10.0d0 -1.0d0)
  t)

(deftest ffloor-fd.5d
  (ffloor-check-fd -15 -1.6d0    9.0d0 -0.6d0)
  t)


;;  fixnum - long-float
(defun ffloor-check-fl (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest ffloor-fl.1
  (ffloor 0 10.0l0)
  0.0l0 0.0l0)

(deftest ffloor-fl.2
  (ffloor 0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ffloor-fl.3
  (ffloor 10 0.0l0)
  division-by-zero)

(deftest ffloor-fl.4a
  (ffloor-check-fl 4 10.5l0    0.0l0 4.0l0)
  t)

(deftest ffloor-fl.4b
  (ffloor-check-fl -4 10.5l0    -1.0l0 6.5l0)
  t)

(deftest ffloor-fl.4c
  (ffloor-check-fl 4 -10.5l0    -1.0l0 -6.5l0)
  t)

(deftest ffloor-fl.4d
  (ffloor-check-fl -4 -10.5l0    0.0l0 -4.0l0)
  t)

(deftest ffloor-fl.5a
  (ffloor-check-fl 15 1.6l0    9.0l0 0.6l0)
  t)

(deftest ffloor-fl.5b
  (ffloor-check-fl -15 1.6l0    -10.0l0 1.0l0)
  t)

(deftest ffloor-fl.5c
  (ffloor-check-fl 15 -1.6l0    -10.0l0 -1.0l0)
  t)

(deftest ffloor-fl.5d
  (ffloor-check-fl -15 -1.6l0    9.0l0 -0.6l0)
  t)


;;  bignum - fixnum
(defun ffloorb (a b)
  (ffloor (make-bignum a) b))

(deftest ffloor-bf.1
  (ffloorb 0 10)
  0.0 0)

(deftest ffloor-bf.2
  (ffloorb 0 -10)
  0.0 0)

(deftest-error ffloor-bf.3
  (ffloorb 10 0)
  division-by-zero)

(deftest ffloor-bf.4a
  (ffloorb 3 10)
  0.0 3)

(deftest ffloor-bf.4b
  (ffloorb -3 10)
  -1.0 7)

(deftest ffloor-bf.4c
  (ffloorb 3 -10)
  -1.0 -7)

(deftest ffloor-bf.4d
  (ffloorb -3 -10)
  0.0 -3)

(deftest ffloor-bf.5a
  (ffloorb 10 3)
  3.0 1)

(deftest ffloor-bf.5b
  (ffloorb -10 3)
  -4.0 2)

(deftest ffloor-bf.5c
  (ffloorb 10 -3)
  -4.0 -2)

(deftest ffloor-bf.5d
  (ffloorb -10 -3)
  3.0 -1)

(deftest ffloor-bf.6a
  (ffloorb 11 3)
  3.0 2)

(deftest ffloor-bf.6b
  (ffloorb -11 3)
  -4.0 1)

(deftest ffloor-bf.6c
  (ffloorb 11 -3)
  -4.0 -1)

(deftest ffloor-bf.6d
  (ffloorb -11 -3)
  3.0 -2)


;; bignum - bignum
(deftest ffloor-bb.1
  (ffloorb 0 (make-bignum 10))
  0.0 0)

(deftest ffloor-bb.2
  (ffloorb 0 (make-bignum -10))
  0.0 0)

(deftest-error ffloor-bb.3
  (ffloorb 10 (make-bignum 0))
  division-by-zero)

(deftest ffloor-bb.4a
  (ffloorb 3 (make-bignum 10))
  0.0 3)

(deftest ffloor-bb.4b
  (ffloorb -3 (make-bignum 10))
  -1.0 7)

(deftest ffloor-bb.4c
  (ffloorb 3 (make-bignum -10))
  -1.0 -7)

(deftest ffloor-bb.4d
  (ffloorb -3 (make-bignum -10))
  0.0 -3)

(deftest ffloor-bb.5a
  (ffloorb 10 (make-bignum 3))
  3.0 1)

(deftest ffloor-bb.5b
  (ffloorb -10 (make-bignum 3))
  -4.0 2)

(deftest ffloor-bb.5c
  (ffloorb 10 (make-bignum -3))
  -4.0 -2)

(deftest ffloor-bb.5d
  (ffloorb -10 (make-bignum -3))
  3.0 -1)

(deftest ffloor-bb.6a
  (ffloorb 11 (make-bignum 3))
  3.0 2)

(deftest ffloor-bb.6b
  (ffloorb -11 (make-bignum 3))
  -4.0 1)

(deftest ffloor-bb.6c
  (ffloorb 11 (make-bignum -3))
  -4.0 -1)

(deftest ffloor-bb.6d
  (ffloorb -11 (make-bignum -3))
  3.0 -2)


;;  bignum - ratio
(deftest ffloor-br.1
  (ffloorb 0 10/3)
  0.0 0)

(deftest ffloor-br.2
  (ffloorb 0 -10/3)
  0.0 0)

(deftest-error ffloor-br.3
  (ffloorb 10 (make-ratio 0 1))
  division-by-zero)

(deftest ffloor-br.4a
  (ffloorb 3 100/7)
  0.0 3)

(deftest ffloor-br.4b
  (ffloorb -3 100/7)
  -1.0 79/7)

(deftest ffloor-br.4c
  (ffloorb 3 -100/7)
  -1.0 -79/7)

(deftest ffloor-br.4d
  (ffloorb -3 -100/7)
  0.0 -3)

(deftest ffloor-br.5a
  (ffloorb 10 6/7)
  11.0 4/7)

(deftest ffloor-br.5b
  (ffloorb -10 6/7)
  -12.0 2/7)

(deftest ffloor-br.5c
  (ffloorb 10 -6/7)
  -12.0 -2/7)

(deftest ffloor-br.5d
  (ffloorb -10 -6/7)
  11.0 -4/7)

(deftest ffloor-br.6a
  (ffloorb 10 1/3)
  30.0 0)

(deftest ffloor-br.6b
  (ffloorb -10 1/3)
  -30.0 0)

(deftest ffloor-br.6c
  (ffloorb 10 -1/3)
  -30.0 0)

(deftest ffloor-br.6d
  (ffloorb -10 -1/3)
  30.0 0)


;;  bignum - single-float
(defun ffloor-check-bs (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-bs.1
  (ffloorb 0 10.0f0)
  0.0f0 0.0f0)

(deftest ffloor-bs.2
  (ffloorb 0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error ffloor-bs.3
  (ffloorb 10 0.0f0)
  division-by-zero)

(deftest ffloor-bs.4a
  (ffloor-check-bs 4 10.5f0    0.0f0 4.0f0)
  t)

(deftest ffloor-bs.4b
  (ffloor-check-bs -4 10.5f0    -1.0f0 6.5f0)
  t)

(deftest ffloor-bs.4c
  (ffloor-check-bs 4 -10.5f0    -1.0f0 -6.5f0)
  t)

(deftest ffloor-bs.4d
  (ffloor-check-bs -4 -10.5f0    0.0f0 -4.0f0)
  t)

(deftest ffloor-bs.5a
  (ffloor-check-bs 15 1.6f0    9.0f0 0.6f0)
  t)

(deftest ffloor-bs.5b
  (ffloor-check-bs -15 1.6f0    -10.0f0 1.0f0)
  t)

(deftest ffloor-bs.5c
  (ffloor-check-bs 15 -1.6f0    -10.0f0 -1.0f0)
  t)

(deftest ffloor-bs.5d
  (ffloor-check-bs -15 -1.6f0    9.0f0 -0.6f0)
  t)


;;  bignum - double-float
(defun ffloor-check-bd (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ffloor-bd.1
  (ffloorb 0 10.0d0)
  0.0d0 0.0d0)

(deftest ffloor-bd.2
  (ffloorb 0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ffloor-bd.3
  (ffloorb 10 0.0d0)
  division-by-zero)

(deftest ffloor-bd.4a
  (ffloor-check-bd 4 10.5d0    0.0d0 4.0d0)
  t)

(deftest ffloor-bd.4b
  (ffloor-check-bd -4 10.5d0    -1.0d0 6.5d0)
  t)

(deftest ffloor-bd.4c
  (ffloor-check-bd 4 -10.5d0    -1.0d0 -6.5d0)
  t)

(deftest ffloor-bd.4d
  (ffloor-check-bd -4 -10.5d0    0.0d0 -4.0d0)
  t)

(deftest ffloor-bd.5a
  (ffloor-check-bd 15 1.6d0    9.0d0 0.6d0)
  t)

(deftest ffloor-bd.5b
  (ffloor-check-bd -15 1.6d0    -10.0d0 1.0d0)
  t)

(deftest ffloor-bd.5c
  (ffloor-check-bd 15 -1.6d0    -10.0d0 -1.0d0)
  t)

(deftest ffloor-bd.5d
  (ffloor-check-bd -15 -1.6d0    9.0d0 -0.6d0)
  t)


;;  bignum - long-float
(defun ffloor-check-bl (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest ffloor-bl.1
  (ffloorb 0 10.0l0)
  0.0l0 0.0l0)

(deftest ffloor-bl.2
  (ffloorb 0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ffloor-bl.3
  (ffloorb 10 0.0l0)
  division-by-zero)

(deftest ffloor-bl.4a
  (ffloor-check-bl 4 10.5l0    0.0l0 4.0l0)
  t)

(deftest ffloor-bl.4b
  (ffloor-check-bl -4 10.5l0    -1.0l0 6.5l0)
  t)

(deftest ffloor-bl.4c
  (ffloor-check-bl 4 -10.5l0    -1.0l0 -6.5l0)
  t)

(deftest ffloor-bl.4d
  (ffloor-check-bl -4 -10.5l0    0.0l0 -4.0l0)
  t)

(deftest ffloor-bl.5a
  (ffloor-check-bl 15 1.6l0    9.0l0 0.6l0)
  t)

(deftest ffloor-bl.5b
  (ffloor-check-bl -15 1.6l0    -10.0l0 1.0l0)
  t)

(deftest ffloor-bl.5c
  (ffloor-check-bl 15 -1.6l0    -10.0l0 -1.0l0)
  t)

(deftest ffloor-bl.5d
  (ffloor-check-bl -15 -1.6l0    9.0l0 -0.6l0)
  t)


;;  ratio - fixnum
(deftest ffloor-rf.1
  (ffloor (make-ratio 0 1) 10)
  0.0 0)

(deftest ffloor-rf.2
  (ffloor (make-ratio 0 1) -10)
  0.0 0)

(deftest-error ffloor-rf.3
  (ffloor 10/3 0)
  division-by-zero)

(deftest ffloor-rf.4a
  (ffloor 2/3 5)
  0.0 2/3)

(deftest ffloor-rf.4b
  (ffloor -2/3 5)
  -1.0 13/3)

(deftest ffloor-rf.4c
  (ffloor 2/3 -5)
  -1.0 -13/3)

(deftest ffloor-rf.4d
  (ffloor -2/3 -5)
  0.0 -2/3)

(deftest ffloor-rf.5a
  (ffloor 20/3 5)
  1.0 5/3)

(deftest ffloor-rf.5b
  (ffloor -20/3 5)
  -2.0 10/3)

(deftest ffloor-rf.5c
  (ffloor 20/3 -5)
  -2.0 -10/3)

(deftest ffloor-rf.5d
  (ffloor -20/3 -5)
  1.0 -5/3)

(deftest ffloor-rf.6a
  (ffloor 53/3 8)
  2.0 5/3)

(deftest ffloor-rf.6b
  (ffloor -53/3 8)
  -3.0 19/3)

(deftest ffloor-rf.6c
  (ffloor 53/3 -8)
  -3.0 -19/3)

(deftest ffloor-rf.6d
  (ffloor -53/3 -8)
  2.0 -5/3)


;; ratio - bignum
(deftest ffloor-rb.1
  (ffloor (make-ratio 0 1) (make-bignum 10))
  0.0 0)

(deftest ffloor-rb.2
  (ffloor (make-ratio 0 1) (make-bignum -10))
  0.0 0)

(deftest-error ffloor-rb.3
  (ffloor 10/3 (make-bignum 0))
  division-by-zero)

(deftest ffloor-rb.4a
  (ffloor 2/3 (make-bignum 5))
  0.0 2/3)

(deftest ffloor-rb.4b
  (ffloor -2/3 (make-bignum 5))
  -1.0 13/3)

(deftest ffloor-rb.4c
  (ffloor 2/3 (make-bignum -5))
  -1.0 -13/3)

(deftest ffloor-rb.4d
  (ffloor -2/3 (make-bignum -5))
  0.0 -2/3)

(deftest ffloor-rb.5a
  (ffloor 20/3 (make-bignum 5))
  1.0 5/3)

(deftest ffloor-rb.5b
  (ffloor -20/3 (make-bignum 5))
  -2.0 10/3)

(deftest ffloor-rb.5c
  (ffloor 20/3 (make-bignum -5))
  -2.0 -10/3)

(deftest ffloor-rb.5d
  (ffloor -20/3 (make-bignum -5))
  1.0 -5/3)

(deftest ffloor-rb.6a
  (ffloor 53/3 (make-bignum 8))
  2.0 5/3)

(deftest ffloor-rb.6b
  (ffloor -53/3 (make-bignum 8))
  -3.0 19/3)

(deftest ffloor-rb.6c
  (ffloor 53/3 (make-bignum -8))
  -3.0 -19/3)

(deftest ffloor-rb.6d
  (ffloor -53/3 (make-bignum -8))
  2.0 -5/3)


;;  ratio - ratio
(deftest ffloor-rr.1
  (ffloor (make-ratio 0 1) 10/3)
  0.0 0)

(deftest ffloor-rr.2
  (ffloor (make-ratio 0 1) -10/3)
  0.0 0)

(deftest-error ffloor-rr.3
  (ffloor 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest ffloor-rr.4a
  (ffloor 1/2 33/5)
  0.0 1/2)

(deftest ffloor-rr.4b
  (ffloor -1/2 33/5)
  -1.0 61/10)

(deftest ffloor-rr.4c
  (ffloor 1/2 -33/5)
  -1.0 -61/10)

(deftest ffloor-rr.4d
  (ffloor -1/2 -33/5)
  0.0 -1/2)

(deftest ffloor-rr.5a
  (ffloor 79/3 2/7)
  92.0 1/21)

(deftest ffloor-rr.5b
  (ffloor -79/3 2/7)
  -93.0 5/21)

(deftest ffloor-rr.5c
  (ffloor 79/3 -2/7)
  -93.0 -5/21)

(deftest ffloor-rr.5d
  (ffloor -79/3 -2/7)
  92.0 -1/21)

(deftest ffloor-rr.6a
  (ffloor 4/5 8/20)
  2.0 0)

(deftest ffloor-rr.6b
  (ffloor -4/5 8/20)
  -2.0 0)

(deftest ffloor-rr.6c
  (ffloor 4/5 -8/20)
  -2.0 0)

(deftest ffloor-rr.6d
  (ffloor -4/5 -8/20)
  2.0 0)


;;  ratio - single-float
(defun ffloor-check-rs (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-rs.1
  (ffloor (make-ratio 0 1) 10.0f0)
  0.0f0 0.0f0)

(deftest ffloor-rs.2
  (ffloor (make-ratio 0 1) -10.0f0)
  0.0f0 0.0f0)

(deftest-error ffloor-rs.3
  (ffloor 10/4 0.0f0)
  division-by-zero)

(deftest ffloor-rs.4a
  (ffloor-check-rs 3/4 5.5f0    0.0f0 0.75f0)
  t)

(deftest ffloor-rs.4b
  (ffloor-check-rs -3/4 5.5f0    -1.0f0 4.75f0)
  t)

(deftest ffloor-rs.4c
  (ffloor-check-rs 3/4 -5.5f0    -1.0f0 -4.75f0)
  t)

(deftest ffloor-rs.4d
  (ffloor-check-rs -3/4 -5.5f0    0.0f0 -0.75f0)
  t)

(deftest ffloor-rs.5a
  (ffloor-check-rs 77/4 1.6f0    12.0f0 0.05f0)
  t)

(deftest ffloor-rs.5b
  (ffloor-check-rs -77/4 1.6f0    -13.0f0 1.55f0)
  t)

(deftest ffloor-rs.5c
  (ffloor-check-rs 77/4 -1.6f0    -13.0f0 -1.55f0)
  t)

(deftest ffloor-rs.5d
  (ffloor-check-rs -77/4 -1.6f0    12.0f0 -0.05f0)
  t)


;;  ratio - double-float
(defun ffloor-check-rd (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ffloor-rd.1
  (ffloor (make-ratio 0 1) 10.0d0)
  0.0d0 0.0d0)

(deftest ffloor-rd.2
  (ffloor (make-ratio 0 1) -10.0d0)
  0.0d0 0.0d0)

(deftest-error ffloor-rd.3
  (ffloor 10/4 0.0d0)
  division-by-zero)

(deftest ffloor-rd.4a
  (ffloor-check-rd 3/4 5.5d0    0.0d0 0.75d0)
  t)

(deftest ffloor-rd.4b
  (ffloor-check-rd -3/4 5.5d0    -1.0d0 4.75d0)
  t)

(deftest ffloor-rd.4c
  (ffloor-check-rd 3/4 -5.5d0    -1.0d0 -4.75d0)
  t)

(deftest ffloor-rd.4d
  (ffloor-check-rd -3/4 -5.5d0    0.0d0 -0.75d0)
  t)

(deftest ffloor-rd.5a
  (ffloor-check-rd 77/4 1.6d0    12.0d0 0.05d0)
  t)

(deftest ffloor-rd.5b
  (ffloor-check-rd -77/4 1.6d0    -13.0d0 1.55d0)
  t)

(deftest ffloor-rd.5c
  (ffloor-check-rd 77/4 -1.6d0    -13.0d0 -1.55d0)
  t)

(deftest ffloor-rd.5d
  (ffloor-check-rd -77/4 -1.6d0    12.0d0 -0.05d0)
  t)


;;  ratio - long-float
(defun ffloor-check-rl (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest ffloor-rl.1
  (ffloor (make-ratio 0 1) 10.0l0)
  0.0l0 0.0l0)

(deftest ffloor-rl.2
  (ffloor (make-ratio 0 1) -10.0l0)
  0.0l0 0.0l0)

(deftest-error ffloor-rl.3
  (ffloor 10/4 0.0l0)
  division-by-zero)

(deftest ffloor-rl.4a
  (ffloor-check-rl 3/4 5.5l0    0.0l0 0.75l0)
  t)

(deftest ffloor-rl.4b
  (ffloor-check-rl -3/4 5.5l0    -1.0l0 4.75l0)
  t)

(deftest ffloor-rl.4c
  (ffloor-check-rl 3/4 -5.5l0    -1.0l0 -4.75l0)
  t)

(deftest ffloor-rl.4d
  (ffloor-check-rl -3/4 -5.5l0    0.0l0 -0.75l0)
  t)

(deftest ffloor-rl.5a
  (ffloor-check-rl 77/4 1.6l0    12.0l0 0.05l0)
  t)

(deftest ffloor-rl.5b
  (ffloor-check-rl -77/4 1.6l0    -13.0l0 1.55l0)
  t)

(deftest ffloor-rl.5c
  (ffloor-check-rl 77/4 -1.6l0    -13.0l0 -1.55l0)
  t)

(deftest ffloor-rl.5d
  (ffloor-check-rl -77/4 -1.6l0    12.0l0 -0.05l0)
  t)


;; single-float - fixnum
(defun ffloor-check-sf (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-sf.1
  (ffloor 0.0f0 10)
  0.0f0 0.0f0)

(deftest ffloor-sf.2
  (ffloor 0.0f0 -10)
  0.0f0 0.0f0)

(deftest-error ffloor-sf.3
  (ffloor 10.4f0 0)
  division-by-zero)

(deftest ffloor-sf.4a
  (ffloor-check-sf 1.2f0 10    0.0f0 1.2f0)
  t)

(deftest ffloor-sf.4b
  (ffloor-check-sf -1.2f0 10    -1.0f0 8.8f0)
  t)

(deftest ffloor-sf.4c
  (ffloor-check-sf 1.2f0 -10    -1.0f0 -8.8f0)
  t)

(deftest ffloor-sf.4d
  (ffloor-check-sf -1.2f0 -10    0.0f0 -1.2f0)
  t)

(deftest ffloor-sf.5a
  (ffloor-check-sf 10.2f0 4    2.0f0 2.2f0)
  t)

(deftest ffloor-sf.5b
  (ffloor-check-sf -10.2f0 4    -3.0f0 1.8f0)
  t)

(deftest ffloor-sf.5c
  (ffloor-check-sf 10.2f0 -4    -3.0f0 -1.8f0)
  t)

(deftest ffloor-sf.5d
  (ffloor-check-sf -10.2f0 -4    2.0f0 -2.2f0)
  t)


;; single-float - bignum
(defun ffloor-check-sb (a b c d)
  (ffloor-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-sb.1
  (ffloor 0.0f0 (make-bignum 10))
  0.0f0 0.0f0)

(deftest ffloor-sb.2
  (ffloor 0.0f0 (make-bignum -10))
  0.0f0 0.0f0)

(deftest-error ffloor-sb.3
  (ffloor 10.4f0 (make-bignum 0))
  division-by-zero)

(deftest ffloor-sb.4a
  (ffloor-check-sb 1.2f0 10    0.0f0 1.2f0)
  t)

(deftest ffloor-sb.4b
  (ffloor-check-sb -1.2f0 10    -1.0f0 8.8f0)
  t)

(deftest ffloor-sb.4c
  (ffloor-check-sb 1.2f0 -10    -1.0f0 -8.8f0)
  t)

(deftest ffloor-sb.4d
  (ffloor-check-sb -1.2f0 -10    0.0f0 -1.2f0)
  t)

(deftest ffloor-sb.5a
  (ffloor-check-sb 10.2f0 4    2.0f0 2.2f0)
  t)

(deftest ffloor-sb.5b
  (ffloor-check-sb -10.2f0 4    -3.0f0 1.8f0)
  t)

(deftest ffloor-sb.5c
  (ffloor-check-sb 10.2f0 -4    -3.0f0 -1.8f0)
  t)

(deftest ffloor-sb.5d
  (ffloor-check-sb -10.2f0 -4    2.0f0 -2.2f0)
  t)


;; single-float - ratio
(defun ffloor-check-sr (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-sr.1
  (ffloor 0.0f0 10/3)
  0.0f0 0.0f0)

(deftest ffloor-sr.2
  (ffloor 0.0f0 -10/3)
  0.0f0 0.0f0)

(deftest-error ffloor-sr.3
  (ffloor 10.4f0 (make-ratio 0 1))
  division-by-zero)

(deftest ffloor-sr.4a
  (ffloor-check-sr 1.2f0 15/4    0.0f0 1.2f0)
  t)

(deftest ffloor-sr.4b
  (ffloor-check-sr -1.2f0 15/4    -1.0f0 2.55f0)
  t)

(deftest ffloor-sr.4c
  (ffloor-check-sr 1.2f0 -15/4    -1.0f0 -2.55f0)
  t)

(deftest ffloor-sr.4d
  (ffloor-check-sr -1.2f0 -15/4    0.0f0 -1.2f0)
  t)

(deftest ffloor-sr.5a
  (ffloor-check-sr 1.2f0 1/4    4.0f0 0.2f0)
  t)

(deftest ffloor-sr.5b
  (ffloor-check-sr -1.2f0 1/4    -5.0f0 0.05f0)
  t)

(deftest ffloor-sr.5c
  (ffloor-check-sr 1.2f0 -1/4    -5.0f0 -0.05f0)
  t)

(deftest ffloor-sr.5d
  (ffloor-check-sr -1.2f0 -1/4    4.0f0 -0.2f0)
  t)


;; single-float - single-float
(defun ffloor-check-ss (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ffloor-ss.1
  (ffloor 0.0f0 10.0f0)
  0.0f0 0.0f0)

(deftest ffloor-ss.2
  (ffloor 0.0f0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error ffloor-ss.3
  (ffloor 10.4f0 0.0f0)
  division-by-zero)

(deftest ffloor-ss.4a
  (ffloor-check-ss 1.2f0 3.7f0    0.0f0 1.2f0)
  t)

(deftest ffloor-ss.4b
  (ffloor-check-ss -1.2f0 3.7f0    -1.0f0 2.5f0)
  t)

(deftest ffloor-ss.4c
  (ffloor-check-ss 1.2f0 -3.7f0    -1.0f0 -2.5f0)
  t)

(deftest ffloor-ss.4d
  (ffloor-check-ss -1.2f0 -3.7f0    0.0f0 -1.2f0)
  t)

(deftest ffloor-ss.5a
  (ffloor-check-ss 12.3f0 3.7f0    3.0f0 1.2f0)
  t)

(deftest ffloor-ss.5b
  (ffloor-check-ss -12.3f0 3.7f0    -4.0f0 2.5f0)
  t)

(deftest ffloor-ss.5c
  (ffloor-check-ss 12.3f0 -3.7f0    -4.0f0 -2.5f0)
  t)

(deftest ffloor-ss.5d
  (ffloor-check-ss -12.3f0 -3.7f0    3.0f0 -1.2f0)
  t)


;; single-float - double-float
(defun ffloor-check-sd (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :type 'double-float))

(deftest ffloor-sd.1
  (ffloor 0.0f0 10.0d0)
  0.0d0 0.0d0)

(deftest ffloor-sd.2
  (ffloor 0.0f0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ffloor-sd.3
  (ffloor 10.4f0 0.0d0)
  division-by-zero)

(deftest ffloor-sd.4a
  (ffloor-check-sd 1.2f0 3.7d0    0.0d0 1.2d0)
  t)

(deftest ffloor-sd.4b
  (ffloor-check-sd -1.2f0 3.7d0    -1.0d0 2.5d0)
  t)

(deftest ffloor-sd.4c
  (ffloor-check-sd 1.2f0 -3.7d0    -1.0d0 -2.5d0)
  t)

(deftest ffloor-sd.4d
  (ffloor-check-sd -1.2f0 -3.7d0    0.0d0 -1.2d0)
  t)

(deftest ffloor-sd.5a
  (ffloor-check-sd 12.3f0 3.7d0    3.0d0 1.2d0)
  t)

(deftest ffloor-sd.5b
  (ffloor-check-sd -12.3f0 3.7d0    -4.0d0 2.5d0)
  t)

(deftest ffloor-sd.5c
  (ffloor-check-sd 12.3f0 -3.7d0    -4.0d0 -2.5d0)
  t)

(deftest ffloor-sd.5d
  (ffloor-check-sd -12.3f0 -3.7d0    3.0d0 -1.2d0)
  t)


;; single-float - long-float
(defun ffloor-check-sl (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :type 'long-float))

(deftest ffloor-sl.1
  (ffloor 0.0f0 10.0l0)
  0.0l0 0.0l0)

(deftest ffloor-sl.2
  (ffloor 0.0f0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ffloor-sl.3
  (ffloor 10.4f0 0.0l0)
  division-by-zero)

(deftest ffloor-sl.4a
  (ffloor-check-sl 1.2f0 3.7l0    0.0l0 1.2l0)
  t)

(deftest ffloor-sl.4b
  (ffloor-check-sl -1.2f0 3.7l0    -1.0l0 2.5l0)
  t)

(deftest ffloor-sl.4c
  (ffloor-check-sl 1.2f0 -3.7l0    -1.0l0 -2.5l0)
  t)

(deftest ffloor-sl.4d
  (ffloor-check-sl -1.2f0 -3.7l0    0.0l0 -1.2l0)
  t)

(deftest ffloor-sl.5a
  (ffloor-check-sl 12.3f0 3.7l0    3.0l0 1.2l0)
  t)

(deftest ffloor-sl.5b
  (ffloor-check-sl -12.3f0 3.7l0    -4.0l0 2.5l0)
  t)

(deftest ffloor-sl.5c
  (ffloor-check-sl 12.3f0 -3.7l0    -4.0l0 -2.5l0)
  t)

(deftest ffloor-sl.5d
  (ffloor-check-sl -12.3f0 -3.7l0    3.0l0 -1.2l0)
  t)


;; double-float - fixnum
(defun ffloor-check-df (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-14
    :type 'double-float))

(deftest ffloor-df.1
  (ffloor 0.0d0 10)
  0.0d0 0.0d0)

(deftest ffloor-df.2
  (ffloor 0.0d0 -10)
  0.0d0 0.0d0)

(deftest-error ffloor-df.3
  (ffloor 10.4d0 0)
  division-by-zero)

(deftest ffloor-df.4a
  (ffloor-check-df 1.2d0 10    0.0d0 1.2d0)
  t)

(deftest ffloor-df.4b
  (ffloor-check-df -1.2d0 10    -1.0d0 8.8d0)
  t)

(deftest ffloor-df.4c
  (ffloor-check-df 1.2d0 -10    -1.0d0 -8.8d0)
  t)

(deftest ffloor-df.4d
  (ffloor-check-df -1.2d0 -10    0.0d0 -1.2d0)
  t)

(deftest ffloor-df.5a
  (ffloor-check-df 10.2d0 4    2.0d0 2.2d0)
  t)

(deftest ffloor-df.5b
  (ffloor-check-df -10.2d0 4    -3.0d0 1.8d0)
  t)

(deftest ffloor-df.5c
  (ffloor-check-df 10.2d0 -4    -3.0d0 -1.8d0)
  t)

(deftest ffloor-df.5d
  (ffloor-check-df -10.2d0 -4    2.0d0 -2.2d0)
  t)


;; double-float - bignum
(defun ffloor-check-db (a b c d)
  (ffloor-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ffloor-db.1
  (ffloor 0.0d0 (make-bignum 10))
  0.0d0 0.0d0)

(deftest ffloor-db.2
  (ffloor 0.0d0 (make-bignum -10))
  0.0d0 0.0d0)

(deftest-error ffloor-db.3
  (ffloor 10.4d0 (make-bignum 0))
  division-by-zero)

(deftest ffloor-db.4a
  (ffloor-check-db 1.2d0 10    0.0d0 1.2d0)
  t)

(deftest ffloor-db.4b
  (ffloor-check-db -1.2d0 10    -1.0d0 8.8d0)
  t)

(deftest ffloor-db.4c
  (ffloor-check-db 1.2d0 -10    -1.0d0 -8.8d0)
  t)

(deftest ffloor-db.4d
  (ffloor-check-db -1.2d0 -10    0.0d0 -1.2d0)
  t)

(deftest ffloor-db.5a
  (ffloor-check-db 10.2d0 4    2.0d0 2.2d0)
  t)

(deftest ffloor-db.5b
  (ffloor-check-db -10.2d0 4    -3.0d0 1.8d0)
  t)

(deftest ffloor-db.5c
  (ffloor-check-db 10.2d0 -4    -3.0d0 -1.8d0)
  t)

(deftest ffloor-db.5d
  (ffloor-check-db -10.2d0 -4    2.0d0 -2.2d0)
  t)


;; double-float - ratio
(defun ffloor-check-dr (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ffloor-dr.1
  (ffloor 0.0d0 10/3)
  0.0d0 0.0d0)

(deftest ffloor-dr.2
  (ffloor 0.0d0 -10/3)
  0.0d0 0.0d0)

(deftest-error ffloor-dr.3
  (ffloor 10.4d0 (make-ratio 0 1))
  division-by-zero)

(deftest ffloor-dr.4a
  (ffloor-check-dr 1.2d0 15/4    0.0d0 1.2d0)
  t)

(deftest ffloor-dr.4b
  (ffloor-check-dr -1.2d0 15/4    -1.0d0 2.55d0)
  t)

(deftest ffloor-dr.4c
  (ffloor-check-dr 1.2d0 -15/4    -1.0d0 -2.55d0)
  t)

(deftest ffloor-dr.4d
  (ffloor-check-dr -1.2d0 -15/4    0.0d0 -1.2d0)
  t)

(deftest ffloor-dr.5a
  (ffloor-check-dr 1.2d0 1/4    4.0d0 0.2d0)
  t)

(deftest ffloor-dr.5b
  (ffloor-check-dr -1.2d0 1/4    -5.0d0 0.05d0)
  t)

(deftest ffloor-dr.5c
  (ffloor-check-dr 1.2d0 -1/4    -5.0d0 -0.05d0)
  t)

(deftest ffloor-dr.5d
  (ffloor-check-dr -1.2d0 -1/4    4.0d0 -0.2d0)
  t)


;; double-float - single-float
(defun ffloor-check-ds (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest ffloor-ds.1
  (ffloor 0.0d0 10.0f0)
  0.0d0 0.0d0)

(deftest ffloor-ds.2
  (ffloor 0.0d0 -10.0f0)
  0.0d0 0.0d0)

(deftest-error ffloor-ds.3
  (ffloor 10.4d0 0.0f0)
  division-by-zero)

(deftest ffloor-ds.4a
  (ffloor-check-ds 1.2d0 3.7f0    0.0d0 1.2d0)
  t)

(deftest ffloor-ds.4b
  (ffloor-check-ds -1.2d0 3.7f0    -1.0d0 2.5d0)
  t)

(deftest ffloor-ds.4c
  (ffloor-check-ds 1.2d0 -3.7f0    -1.0d0 -2.5d0)
  t)

(deftest ffloor-ds.4d
  (ffloor-check-ds -1.2d0 -3.7f0    0.0d0 -1.2d0)
  t)

(deftest ffloor-ds.5a
  (ffloor-check-ds 12.3d0 3.7f0    3.0d0 1.2d0)
  t)

(deftest ffloor-ds.5b
  (ffloor-check-ds -12.3d0 3.7f0    -4.0d0 2.5d0)
  t)

(deftest ffloor-ds.5c
  (ffloor-check-ds 12.3d0 -3.7f0    -4.0d0 -2.5d0)
  t)

(deftest ffloor-ds.5d
  (ffloor-check-ds -12.3d0 -3.7f0    3.0d0 -1.2d0)
  t)


;; double-float - double-float
(defun ffloor-check-dd (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ffloor-dd.1
  (ffloor 0.0d0 10.0d0)
  0.0d0 0.0d0)

(deftest ffloor-dd.2
  (ffloor 0.0d0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ffloor-dd.3
  (ffloor 10.4d0 0.0d0)
  division-by-zero)

(deftest ffloor-dd.4a
  (ffloor-check-dd 1.2d0 3.7d0    0.0d0 1.2d0)
  t)

(deftest ffloor-dd.4b
  (ffloor-check-dd -1.2d0 3.7d0    -1.0d0 2.5d0)
  t)

(deftest ffloor-dd.4c
  (ffloor-check-dd 1.2d0 -3.7d0    -1.0d0 -2.5d0)
  t)

(deftest ffloor-dd.4d
  (ffloor-check-dd -1.2d0 -3.7d0    0.0d0 -1.2d0)
  t)

(deftest ffloor-dd.5a
  (ffloor-check-dd 12.3d0 3.7d0    3.0d0 1.2d0)
  t)

(deftest ffloor-dd.5b
  (ffloor-check-dd -12.3d0 3.7d0    -4.0d0 2.5d0)
  t)

(deftest ffloor-dd.5c
  (ffloor-check-dd 12.3d0 -3.7d0    -4.0d0 -2.5d0)
  t)

(deftest ffloor-dd.5d
  (ffloor-check-dd -12.3d0 -3.7d0    3.0d0 -1.2d0)
  t)


;; double-float - long-float
(defun ffloor-check-dl (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ffloor-dl.1
  (ffloor 0.0d0 10.0l0)
  0.0l0 0.0l0)

(deftest ffloor-dl.2
  (ffloor 0.0d0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ffloor-dl.3
  (ffloor 10.4d0 0.0l0)
  division-by-zero)

(deftest ffloor-dl.4a
  (ffloor-check-dl 1.2d0 3.7l0    0.0l0 1.2l0)
  t)

(deftest ffloor-dl.4b
  (ffloor-check-dl -1.2d0 3.7l0    -1.0l0 2.5l0)
  t)

(deftest ffloor-dl.4c
  (ffloor-check-dl 1.2d0 -3.7l0    -1.0l0 -2.5l0)
  t)

(deftest ffloor-dl.4d
  (ffloor-check-dl -1.2d0 -3.7l0    0.0l0 -1.2l0)
  t)

(deftest ffloor-dl.5a
  (ffloor-check-dl 12.3d0 3.7l0    3.0l0 1.2l0)
  t)

(deftest ffloor-dl.5b
  (ffloor-check-dl -12.3d0 3.7l0    -4.0l0 2.5l0)
  t)

(deftest ffloor-dl.5c
  (ffloor-check-dl 12.3d0 -3.7l0    -4.0l0 -2.5l0)
  t)

(deftest ffloor-dl.5d
  (ffloor-check-dl -12.3d0 -3.7l0    3.0l0 -1.2l0)
  t)


;; long-float - fixnum
(defun ffloor-check-lf (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-14
    :type 'long-float))

(deftest ffloor-lf.1
  (ffloor 0.0l0 10)
  0.0l0 0.0l0)

(deftest ffloor-lf.2
  (ffloor 0.0l0 -10)
  0.0l0 0.0l0)

(deftest-error ffloor-lf.3
  (ffloor 10.4l0 0)
  division-by-zero)

(deftest ffloor-lf.4a
  (ffloor-check-lf 1.2l0 10    0.0l0 1.2l0)
  t)

(deftest ffloor-lf.4b
  (ffloor-check-lf -1.2l0 10    -1.0l0 8.8l0)
  t)

(deftest ffloor-lf.4c
  (ffloor-check-lf 1.2l0 -10    -1.0l0 -8.8l0)
  t)

(deftest ffloor-lf.4d
  (ffloor-check-lf -1.2l0 -10    0.0l0 -1.2l0)
  t)

(deftest ffloor-lf.5a
  (ffloor-check-lf 10.2l0 4    2.0l0 2.2l0)
  t)

(deftest ffloor-lf.5b
  (ffloor-check-lf -10.2l0 4    -3.0l0 1.8l0)
  t)

(deftest ffloor-lf.5c
  (ffloor-check-lf 10.2l0 -4    -3.0l0 -1.8l0)
  t)

(deftest ffloor-lf.5d
  (ffloor-check-lf -10.2l0 -4    2.0l0 -2.2l0)
  t)


;; long-float - bignum
(defun ffloor-check-lb (a b c d)
  (ffloor-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ffloor-lb.1
  (ffloor 0.0l0 (make-bignum 10))
  0.0l0 0.0l0)

(deftest ffloor-lb.2
  (ffloor 0.0l0 (make-bignum -10))
  0.0l0 0.0l0)

(deftest-error ffloor-lb.3
  (ffloor 10.4l0 (make-bignum 0))
  division-by-zero)

(deftest ffloor-lb.4a
  (ffloor-check-lb 1.2l0 10    0.0l0 1.2l0)
  t)

(deftest ffloor-lb.4b
  (ffloor-check-lb -1.2l0 10    -1.0l0 8.8l0)
  t)

(deftest ffloor-lb.4c
  (ffloor-check-lb 1.2l0 -10    -1.0l0 -8.8l0)
  t)

(deftest ffloor-lb.4d
  (ffloor-check-lb -1.2l0 -10    0.0l0 -1.2l0)
  t)

(deftest ffloor-lb.5a
  (ffloor-check-lb 10.2l0 4    2.0l0 2.2l0)
  t)

(deftest ffloor-lb.5b
  (ffloor-check-lb -10.2l0 4    -3.0l0 1.8l0)
  t)

(deftest ffloor-lb.5c
  (ffloor-check-lb 10.2l0 -4    -3.0l0 -1.8l0)
  t)

(deftest ffloor-lb.5d
  (ffloor-check-lb -10.2l0 -4    2.0l0 -2.2l0)
  t)


;; long-float - ratio
(defun ffloor-check-lr (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ffloor-lr.1
  (ffloor 0.0l0 10/3)
  0.0l0 0.0l0)

(deftest ffloor-lr.2
  (ffloor 0.0l0 -10/3)
  0.0l0 0.0l0)

(deftest-error ffloor-lr.3
  (ffloor 10.4l0 (make-ratio 0 1))
  division-by-zero)

(deftest ffloor-lr.4a
  (ffloor-check-lr 1.2l0 15/4    0.0l0 1.2l0)
  t)

(deftest ffloor-lr.4b
  (ffloor-check-lr -1.2l0 15/4    -1.0l0 2.55l0)
  t)

(deftest ffloor-lr.4c
  (ffloor-check-lr 1.2l0 -15/4    -1.0l0 -2.55l0)
  t)

(deftest ffloor-lr.4d
  (ffloor-check-lr -1.2l0 -15/4    0.0l0 -1.2l0)
  t)

(deftest ffloor-lr.5a
  (ffloor-check-lr 1.2l0 1/4    4.0l0 0.2l0)
  t)

(deftest ffloor-lr.5b
  (ffloor-check-lr -1.2l0 1/4    -5.0l0 0.05l0)
  t)

(deftest ffloor-lr.5c
  (ffloor-check-lr 1.2l0 -1/4    -5.0l0 -0.05l0)
  t)

(deftest ffloor-lr.5d
  (ffloor-check-lr -1.2l0 -1/4    4.0l0 -0.2l0)
  t)


;; long-float - single-float
(defun ffloor-check-ls (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest ffloor-ls.1
  (ffloor 0.0l0 10.0f0)
  0.0l0 0.0l0)

(deftest ffloor-ls.2
  (ffloor 0.0l0 -10.0f0)
  0.0l0 0.0l0)

(deftest-error ffloor-ls.3
  (ffloor 10.4l0 0.0f0)
  division-by-zero)

(deftest ffloor-ls.4a
  (ffloor-check-ls 1.2l0 3.7f0    0.0l0 1.2l0)
  t)

(deftest ffloor-ls.4b
  (ffloor-check-ls -1.2l0 3.7f0    -1.0l0 2.5l0)
  t)

(deftest ffloor-ls.4c
  (ffloor-check-ls 1.2l0 -3.7f0    -1.0l0 -2.5l0)
  t)

(deftest ffloor-ls.4d
  (ffloor-check-ls -1.2l0 -3.7f0    0.0l0 -1.2l0)
  t)

(deftest ffloor-ls.5a
  (ffloor-check-ls 12.3l0 3.7f0    3.0l0 1.2l0)
  t)

(deftest ffloor-ls.5b
  (ffloor-check-ls -12.3l0 3.7f0    -4.0l0 2.5l0)
  t)

(deftest ffloor-ls.5c
  (ffloor-check-ls 12.3l0 -3.7f0    -4.0l0 -2.5l0)
  t)

(deftest ffloor-ls.5d
  (ffloor-check-ls -12.3l0 -3.7f0    3.0l0 -1.2l0)
  t)


;; long-float - double-float
(defun ffloor-check-ld (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ffloor-ld.1
  (ffloor 0.0l0 10.0d0)
  0.0l0 0.0l0)

(deftest ffloor-ld.2
  (ffloor 0.0l0 -10.0d0)
  0.0l0 0.0l0)

(deftest-error ffloor-ld.3
  (ffloor 10.4l0 0.0d0)
  division-by-zero)

(deftest ffloor-ld.4a
  (ffloor-check-ld 1.2l0 3.7d0    0.0l0 1.2l0)
  t)

(deftest ffloor-ld.4b
  (ffloor-check-ld -1.2l0 3.7d0    -1.0l0 2.5l0)
  t)

(deftest ffloor-ld.4c
  (ffloor-check-ld 1.2l0 -3.7d0    -1.0l0 -2.5l0)
  t)

(deftest ffloor-ld.4d
  (ffloor-check-ld -1.2l0 -3.7d0    0.0l0 -1.2l0)
  t)

(deftest ffloor-ld.5a
  (ffloor-check-ld 12.3l0 3.7d0    3.0l0 1.2l0)
  t)

(deftest ffloor-ld.5b
  (ffloor-check-ld -12.3l0 3.7d0    -4.0l0 2.5l0)
  t)

(deftest ffloor-ld.5c
  (ffloor-check-ld 12.3l0 -3.7d0    -4.0l0 -2.5l0)
  t)

(deftest ffloor-ld.5d
  (ffloor-check-ld -12.3l0 -3.7d0    3.0l0 -1.2l0)
  t)


;; long-float - long-float
(defun ffloor-check-ll (a b c d)
  (ffloor-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ffloor-ll.1
  (ffloor 0.0l0 10.0l0)
  0.0l0 0.0l0)

(deftest ffloor-ll.2
  (ffloor 0.0l0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ffloor-ll.3
  (ffloor 10.4l0 0.0l0)
  division-by-zero)

(deftest ffloor-ll.4a
  (ffloor-check-ll 1.2l0 3.7l0    0.0l0 1.2l0)
  t)

(deftest ffloor-ll.4b
  (ffloor-check-ll -1.2l0 3.7l0    -1.0l0 2.5l0)
  t)

(deftest ffloor-ll.4c
  (ffloor-check-ll 1.2l0 -3.7l0    -1.0l0 -2.5l0)
  t)

(deftest ffloor-ll.4d
  (ffloor-check-ll -1.2l0 -3.7l0    0.0l0 -1.2l0)
  t)

(deftest ffloor-ll.5a
  (ffloor-check-ll 12.3l0 3.7l0    3.0l0 1.2l0)
  t)

(deftest ffloor-ll.5b
  (ffloor-check-ll -12.3l0 3.7l0    -4.0l0 2.5l0)
  t)

(deftest ffloor-ll.5c
  (ffloor-check-ll 12.3l0 -3.7l0    -4.0l0 -2.5l0)
  t)

(deftest ffloor-ll.5d
  (ffloor-check-ll -12.3l0 -3.7l0    3.0l0 -1.2l0)
  t)

