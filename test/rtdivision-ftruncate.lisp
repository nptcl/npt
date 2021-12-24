;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(defun ftruncate-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float))
  (multiple-value-bind (e f) (ftruncate a b)
    (or (and (typep e type)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "ftruncate-equal error: (~S ~S) ~A, ~A, ~A" e f
          (typep e type)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  ftruncate1
;;
(deftest ftruncate1-integer.1
  (ftruncate 0)
  0.0 0)

(deftest ftruncate1-integer.2
  (ftruncate -10)
  -10.0 0)

(deftest ftruncate1-integer.3
  (ftruncate -10)
  -10.0 0)

(deftest ftruncate1-integer.4
  (ftruncate 99999999999999999999)
  99999999999999999999.0 0)

(deftest ftruncate1-integer.5
  (ftruncate -99999999999999999999)
  -99999999999999999999.0 0)

(deftest ftruncate1-ratio.1
  (ftruncate 1/3)
  0.0 1/3)

(deftest ftruncate1-ratio.2
  (ftruncate 10/3)
  3.0 1/3)

(deftest ftruncate1-ratio.3
  (ftruncate -1/3)
  0.0 -1/3)

(deftest ftruncate1-ratio.4
  (ftruncate -10/3)
  -3.0 -1/3)

(deftest ftruncate1-float.1
  (ftruncate 10.0)
  10.0 0.0)

(deftest ftruncate1-float.2
  (ftruncate -10.0)
  -10.0 0.0)

(deftest ftruncate1-float.3
  (ftruncate 12.25)
  12.0 0.25)

(deftest ftruncate1-float.4
  (ftruncate -12.25)
  -12.0 -0.25)

(deftest ftruncate1-float.5
  (ftruncate 12.25f0)
  12.0f0 0.25f0)

(deftest ftruncate1-float.6
  (ftruncate -12.25f0)
  -12.0f0 -0.25f0)

(deftest ftruncate1-float.7
  (ftruncate 12.25d0)
  12.0d0 0.25d0)

(deftest ftruncate1-float.8
  (ftruncate -12.25d0)
  -12.0d0 -0.25d0)

(deftest ftruncate1-float.9
  (ftruncate 12.25l0)
  12.0l0 0.25l0)

(deftest ftruncate1-float.10
  (ftruncate -12.25l0)
  -12.0l0 -0.25l0)


;;
;;  ftruncate
;;

;;  fixnum - fixnum
(deftest ftruncate-ff.1
  (ftruncate 0 10)
  0.0 0)

(deftest ftruncate-ff.2
  (ftruncate 0 -10)
  0.0 0)

(deftest-error ftruncate-ff.3
  (ftruncate 10 0)
  division-by-zero)

(deftest ftruncate-ff.4a
  (ftruncate 3 10)
  0.0 3)

(deftest ftruncate-ff.4b
  (ftruncate -3 10)
  0.0 -3)

(deftest ftruncate-ff.4c
  (ftruncate 3 -10)
  0.0 3)

(deftest ftruncate-ff.4d
  (ftruncate -3 -10)
  0.0 -3)

(deftest ftruncate-ff.5a
  (ftruncate 10 3)
  3.0 1)

(deftest ftruncate-ff.5b
  (ftruncate -10 3)
  -3.0 -1)

(deftest ftruncate-ff.5c
  (ftruncate 10 -3)
  -3.0 1)

(deftest ftruncate-ff.5d
  (ftruncate -10 -3)
  3.0 -1)

(deftest ftruncate-ff.6a
  (ftruncate 11 3)
  3.0 2)

(deftest ftruncate-ff.6b
  (ftruncate -11 3)
  -3.0 -2)

(deftest ftruncate-ff.6c
  (ftruncate 11 -3)
  -3.0 2)

(deftest ftruncate-ff.6d
  (ftruncate -11 -3)
  3.0 -2)


;; fixnum - bignum
(deftest ftruncate-fb.1
  (ftruncate 0 (make-bignum 10))
  0.0 0)

(deftest ftruncate-fb.2
  (ftruncate 0 (make-bignum -10))
  0.0 0)

(deftest-error ftruncate-fb.3
  (ftruncate 10 (make-bignum 0))
  division-by-zero)

(deftest ftruncate-fb.4a
  (ftruncate 3 (make-bignum 10))
  0.0 3)

(deftest ftruncate-fb.4b
  (ftruncate -3 (make-bignum 10))
  -0.0 -3)

(deftest ftruncate-fb.4c
  (ftruncate 3 (make-bignum -10))
  -0.0 3)

(deftest ftruncate-fb.4d
  (ftruncate -3 (make-bignum -10))
  0.0 -3)

(deftest ftruncate-fb.5a
  (ftruncate 10 (make-bignum 3))
  3.0 1)

(deftest ftruncate-fb.5b
  (ftruncate -10 (make-bignum 3))
  -3.0 -1)

(deftest ftruncate-fb.5c
  (ftruncate 10 (make-bignum -3))
  -3.0 1)

(deftest ftruncate-fb.5d
  (ftruncate -10 (make-bignum -3))
  3.0 -1)

(deftest ftruncate-fb.6a
  (ftruncate 11 (make-bignum 3))
  3.0 2)

(deftest ftruncate-fb.6b
  (ftruncate -11 (make-bignum 3))
  -3.0 -2)

(deftest ftruncate-fb.6c
  (ftruncate 11 (make-bignum -3))
  -3.0 2)

(deftest ftruncate-fb.6d
  (ftruncate -11 (make-bignum -3))
  3.0 -2)


;;  fixnum - ratio
(deftest ftruncate-fr.1
  (ftruncate 0 10/3)
  0.0 0)

(deftest ftruncate-fr.2
  (ftruncate 0 -10/3)
  0.0 0)

(deftest-error ftruncate-fr.3
  (ftruncate 10 (make-ratio 0 1))
  division-by-zero)

(deftest ftruncate-fr.4a
  (ftruncate 3 100/7)
  0.0 3)

(deftest ftruncate-fr.4b
  (ftruncate -3 100/7)
  -0.0 -3)

(deftest ftruncate-fr.4c
  (ftruncate 3 -100/7)
  -0.0 3)

(deftest ftruncate-fr.4d
  (ftruncate -3 -100/7)
  0.0 -3)

(deftest ftruncate-fr.5a
  (ftruncate 10 6/7)
  11.0 4/7)

(deftest ftruncate-fr.5b
  (ftruncate -10 6/7)
  -11.0 -4/7)

(deftest ftruncate-fr.5c
  (ftruncate 10 -6/7)
  -11.0 4/7)

(deftest ftruncate-fr.5d
  (ftruncate -10 -6/7)
  11.0 -4/7)

(deftest ftruncate-fr.6a
  (ftruncate 10 1/3)
  30.0 0)

(deftest ftruncate-fr.6b
  (ftruncate -10 1/3)
  -30.0 0)

(deftest ftruncate-fr.6c
  (ftruncate 10 -1/3)
  -30.0 0)

(deftest ftruncate-fr.6d
  (ftruncate -10 -1/3)
  30.0 0)

(deftest ftruncate-fr.7a
  (ftruncate 10 4/3)
  7.0 2/3)


;;  fixnum - single-float
(defun ftruncate-check-fs (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-fs.1
  (ftruncate 0 10.0f0)
  0.0f0 0.0f0)

(deftest ftruncate-fs.2
  (ftruncate 0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error ftruncate-fs.3
  (ftruncate 10 0.0f0)
  division-by-zero)

(deftest ftruncate-fs.4a
  (ftruncate-check-fs 4 10.5f0    0.0f0 4.0f0)
  t)

(deftest ftruncate-fs.4b
  (ftruncate-check-fs -4 10.5f0    0.0f0 -4.0f0)
  t)

(deftest ftruncate-fs.4c
  (ftruncate-check-fs 4 -10.5f0    0.0f0 4.0f0)
  t)

(deftest ftruncate-fs.4d
  (ftruncate-check-fs -4 -10.5f0    0.0f0 -4.0f0)
  t)

(deftest ftruncate-fs.5a
  (ftruncate-check-fs 15 1.6f0    9.0f0 0.6f0)
  t)

(deftest ftruncate-fs.5b
  (ftruncate-check-fs -15 1.6f0    -9.0f0 -0.6f0)
  t)

(deftest ftruncate-fs.5c
  (ftruncate-check-fs 15 -1.6f0    -9.0f0 0.6f0)
  t)

(deftest ftruncate-fs.5d
  (ftruncate-check-fs -15 -1.6f0    9.0f0 -0.6f0)
  t)


;;  fixnum - double-float
(defun ftruncate-check-fd (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ftruncate-fd.1
  (ftruncate 0 10.0d0)
  0.0d0 0.0d0)

(deftest ftruncate-fd.2
  (ftruncate 0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ftruncate-fd.3
  (ftruncate 10 0.0d0)
  division-by-zero)

(deftest ftruncate-fd.4a
  (ftruncate-check-fd 4 10.5d0    0.0d0 4.0d0)
  t)

(deftest ftruncate-fd.4b
  (ftruncate-check-fd -4 10.5d0    0.0d0 -4.0d0)
  t)

(deftest ftruncate-fd.4c
  (ftruncate-check-fd 4 -10.5d0    0.0d0 4.0d0)
  t)

(deftest ftruncate-fd.4d
  (ftruncate-check-fd -4 -10.5d0    0.0d0 -4.0d0)
  t)

(deftest ftruncate-fd.5a
  (ftruncate-check-fd 15 1.6d0    9.0d0 0.6d0)
  t)

(deftest ftruncate-fd.5b
  (ftruncate-check-fd -15 1.6d0    -9.0d0 -0.6d0)
  t)

(deftest ftruncate-fd.5c
  (ftruncate-check-fd 15 -1.6d0    -9.0d0 0.6d0)
  t)

(deftest ftruncate-fd.5d
  (ftruncate-check-fd -15 -1.6d0    9.0d0 -0.6d0)
  t)


;;  fixnum - long-float
(defun ftruncate-check-fl (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest ftruncate-fl.1
  (ftruncate 0 10.0l0)
  0.0l0 0.0l0)

(deftest ftruncate-fl.2
  (ftruncate 0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ftruncate-fl.3
  (ftruncate 10 0.0l0)
  division-by-zero)

(deftest ftruncate-fl.4a
  (ftruncate-check-fl 4 10.5l0    0.0l0 4.0l0)
  t)

(deftest ftruncate-fl.4b
  (ftruncate-check-fl -4 10.5l0    0.0l0 -4.0l0)
  t)

(deftest ftruncate-fl.4c
  (ftruncate-check-fl 4 -10.5l0    0.0l0 4.0l0)
  t)

(deftest ftruncate-fl.4d
  (ftruncate-check-fl -4 -10.5l0    0.0l0 -4.0l0)
  t)

(deftest ftruncate-fl.5a
  (ftruncate-check-fl 15 1.6l0    9.0l0 0.6l0)
  t)

(deftest ftruncate-fl.5b
  (ftruncate-check-fl -15 1.6l0    -9.0l0 -0.6l0)
  t)

(deftest ftruncate-fl.5c
  (ftruncate-check-fl 15 -1.6l0    -9.0l0 0.6l0)
  t)

(deftest ftruncate-fl.5d
  (ftruncate-check-fl -15 -1.6l0    9.0l0 -0.6l0)
  t)


;;  bignum - fixnum
(defun ftruncateb (a b)
  (ftruncate (make-bignum a) b))

(deftest ftruncate-bf.1
  (ftruncateb 0 10)
  0.0 0)

(deftest ftruncate-bf.2
  (ftruncateb 0 -10)
  0.0 0)

(deftest-error ftruncate-bf.3
  (ftruncateb 10 0)
  division-by-zero)

(deftest ftruncate-bf.4a
  (ftruncateb 3 10)
  0.0 3)

(deftest ftruncate-bf.4b
  (ftruncateb -3 10)
  -0.0 -3)

(deftest ftruncate-bf.4c
  (ftruncateb 3 -10)
  -0.0 3)

(deftest ftruncate-bf.4d
  (ftruncateb -3 -10)
  0.0 -3)

(deftest ftruncate-bf.5a
  (ftruncateb 10 3)
  3.0 1)

(deftest ftruncate-bf.5b
  (ftruncateb -10 3)
  -3.0 -1)

(deftest ftruncate-bf.5c
  (ftruncateb 10 -3)
  -3.0 1)

(deftest ftruncate-bf.5d
  (ftruncateb -10 -3)
  3.0 -1)

(deftest ftruncate-bf.6a
  (ftruncateb 11 3)
  3.0 2)

(deftest ftruncate-bf.6b
  (ftruncateb -11 3)
  -3.0 -2)

(deftest ftruncate-bf.6c
  (ftruncateb 11 -3)
  -3.0 2)

(deftest ftruncate-bf.6d
  (ftruncateb -11 -3)
  3.0 -2)


;; bignum - bignum
(deftest ftruncate-bb.1
  (ftruncateb 0 (make-bignum 10))
  0.0 0)

(deftest ftruncate-bb.2
  (ftruncateb 0 (make-bignum -10))
  0.0 0)

(deftest-error ftruncate-bb.3
  (ftruncateb 10 (make-bignum 0))
  division-by-zero)

(deftest ftruncate-bb.4a
  (ftruncateb 3 (make-bignum 10))
  0.0 3)

(deftest ftruncate-bb.4b
  (ftruncateb -3 (make-bignum 10))
  -0.0 -3)

(deftest ftruncate-bb.4c
  (ftruncateb 3 (make-bignum -10))
  -0.0 3)

(deftest ftruncate-bb.4d
  (ftruncateb -3 (make-bignum -10))
  0.0 -3)

(deftest ftruncate-bb.5a
  (ftruncateb 10 (make-bignum 3))
  3.0 1)

(deftest ftruncate-bb.5b
  (ftruncateb -10 (make-bignum 3))
  -3.0 -1)

(deftest ftruncate-bb.5c
  (ftruncateb 10 (make-bignum -3))
  -3.0 1)

(deftest ftruncate-bb.5d
  (ftruncateb -10 (make-bignum -3))
  3.0 -1)

(deftest ftruncate-bb.6a
  (ftruncateb 11 (make-bignum 3))
  3.0 2)

(deftest ftruncate-bb.6b
  (ftruncateb -11 (make-bignum 3))
  -3.0 -2)

(deftest ftruncate-bb.6c
  (ftruncateb 11 (make-bignum -3))
  -3.0 2)

(deftest ftruncate-bb.6d
  (ftruncateb -11 (make-bignum -3))
  3.0 -2)


;;  bignum - ratio
(deftest ftruncate-br.1
  (ftruncateb 0 10/3)
  0.0 0)

(deftest ftruncate-br.2
  (ftruncateb 0 -10/3)
  0.0 0)

(deftest-error ftruncate-br.3
  (ftruncateb 10 (make-ratio 0 1))
  division-by-zero)

(deftest ftruncate-br.4a
  (ftruncateb 3 100/7)
  0.0 3)

(deftest ftruncate-br.4b
  (ftruncateb -3 100/7)
  -0.0 -3)

(deftest ftruncate-br.4c
  (ftruncateb 3 -100/7)
  -0.0 3)

(deftest ftruncate-br.4d
  (ftruncateb -3 -100/7)
  0.0 -3)

(deftest ftruncate-br.5a
  (ftruncateb 10 6/7)
  11.0 4/7)

(deftest ftruncate-br.5b
  (ftruncateb -10 6/7)
  -11.0 -4/7)

(deftest ftruncate-br.5c
  (ftruncateb 10 -6/7)
  -11.0 4/7)

(deftest ftruncate-br.5d
  (ftruncateb -10 -6/7)
  11.0 -4/7)

(deftest ftruncate-br.6a
  (ftruncateb 10 1/3)
  30.0 0)

(deftest ftruncate-br.6b
  (ftruncateb -10 1/3)
  -30.0 0)

(deftest ftruncate-br.6c
  (ftruncateb 10 -1/3)
  -30.0 0)

(deftest ftruncate-br.6d
  (ftruncateb -10 -1/3)
  30.0 0)


;;  bignum - single-float
(defun ftruncate-check-bs (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-bs.1
  (ftruncateb 0 10.0f0)
  0.0f0 0.0f0)

(deftest ftruncate-bs.2
  (ftruncateb 0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error ftruncate-bs.3
  (ftruncateb 10 0.0f0)
  division-by-zero)

(deftest ftruncate-bs.4a
  (ftruncate-check-bs 4 10.5f0    0.0f0 4.0f0)
  t)

(deftest ftruncate-bs.4b
  (ftruncate-check-bs -4 10.5f0    0.0f0 -4.0f0)
  t)

(deftest ftruncate-bs.4c
  (ftruncate-check-bs 4 -10.5f0    0.0f0 4.0f0)
  t)

(deftest ftruncate-bs.4d
  (ftruncate-check-bs -4 -10.5f0    0.0f0 -4.0f0)
  t)

(deftest ftruncate-bs.5a
  (ftruncate-check-bs 15 1.6f0    9.0f0 0.6f0)
  t)

(deftest ftruncate-bs.5b
  (ftruncate-check-bs -15 1.6f0    -9.0f0 -0.6f0)
  t)

(deftest ftruncate-bs.5c
  (ftruncate-check-bs 15 -1.6f0    -9.0f0 0.6f0)
  t)

(deftest ftruncate-bs.5d
  (ftruncate-check-bs -15 -1.6f0    9.0f0 -0.6f0)
  t)


;;  bignum - double-float
(defun ftruncate-check-bd (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ftruncate-bd.1
  (ftruncateb 0 10.0d0)
  0.0d0 0.0d0)

(deftest ftruncate-bd.2
  (ftruncateb 0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ftruncate-bd.3
  (ftruncateb 10 0.0d0)
  division-by-zero)

(deftest ftruncate-bd.4a
  (ftruncate-check-bd 4 10.5d0    0.0d0 4.0d0)
  t)

(deftest ftruncate-bd.4b
  (ftruncate-check-bd -4 10.5d0    0.0d0 -4.0d0)
  t)

(deftest ftruncate-bd.4c
  (ftruncate-check-bd 4 -10.5d0    0.0d0 4.0d0)
  t)

(deftest ftruncate-bd.4d
  (ftruncate-check-bd -4 -10.5d0    0.0d0 -4.0d0)
  t)

(deftest ftruncate-bd.5a
  (ftruncate-check-bd 15 1.6d0    9.0d0 0.6d0)
  t)

(deftest ftruncate-bd.5b
  (ftruncate-check-bd -15 1.6d0    -9.0d0 -0.6d0)
  t)

(deftest ftruncate-bd.5c
  (ftruncate-check-bd 15 -1.6d0    -9.0d0 0.6d0)
  t)

(deftest ftruncate-bd.5d
  (ftruncate-check-bd -15 -1.6d0    9.0d0 -0.6d0)
  t)


;;  bignum - long-float
(defun ftruncate-check-bl (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest ftruncate-bl.1
  (ftruncateb 0 10.0l0)
  0.0l0 0.0l0)

(deftest ftruncate-bl.2
  (ftruncateb 0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ftruncate-bl.3
  (ftruncateb 10 0.0l0)
  division-by-zero)

(deftest ftruncate-bl.4a
  (ftruncate-check-bl 4 10.5l0    0.0l0 4.0l0)
  t)

(deftest ftruncate-bl.4b
  (ftruncate-check-bl -4 10.5l0    0.0l0 -4.0l0)
  t)

(deftest ftruncate-bl.4c
  (ftruncate-check-bl 4 -10.5l0    0.0l0 4.0l0)
  t)

(deftest ftruncate-bl.4d
  (ftruncate-check-bl -4 -10.5l0    0.0l0 -4.0l0)
  t)

(deftest ftruncate-bl.5a
  (ftruncate-check-bl 15 1.6l0    9.0l0 0.6l0)
  t)

(deftest ftruncate-bl.5b
  (ftruncate-check-bl -15 1.6l0    -9.0l0 -0.6l0)
  t)

(deftest ftruncate-bl.5c
  (ftruncate-check-bl 15 -1.6l0    -9.0l0 0.6l0)
  t)

(deftest ftruncate-bl.5d
  (ftruncate-check-bl -15 -1.6l0    9.0l0 -0.6l0)
  t)


;;  ratio - fixnum
(deftest ftruncate-rf.1
  (ftruncate (make-ratio 0 1) 10)
  0.0 0)

(deftest ftruncate-rf.2
  (ftruncate (make-ratio 0 1) -10)
  0.0 0)

(deftest-error ftruncate-rf.3
  (ftruncate 10/3 0)
  division-by-zero)

(deftest ftruncate-rf.4a
  (ftruncate 2/3 5)
  0.0 2/3)

(deftest ftruncate-rf.4b
  (ftruncate -2/3 5)
  -0.0 -2/3)

(deftest ftruncate-rf.4c
  (ftruncate 2/3 -5)
  -0.0 2/3)

(deftest ftruncate-rf.4d
  (ftruncate -2/3 -5)
  0.0 -2/3)

(deftest ftruncate-rf.5a
  (ftruncate 20/3 5)
  1.0 5/3)

(deftest ftruncate-rf.5b
  (ftruncate -20/3 5)
  -1.0 -5/3)

(deftest ftruncate-rf.5c
  (ftruncate 20/3 -5)
  -1.0 5/3)

(deftest ftruncate-rf.5d
  (ftruncate -20/3 -5)
  1.0 -5/3)

(deftest ftruncate-rf.6a
  (ftruncate 53/3 8)
  2.0 5/3)

(deftest ftruncate-rf.6b
  (ftruncate -53/3 8)
  -2.0 -5/3)

(deftest ftruncate-rf.6c
  (ftruncate 53/3 -8)
  -2.0 5/3)

(deftest ftruncate-rf.6d
  (ftruncate -53/3 -8)
  2.0 -5/3)


;; ratio - bignum
(deftest ftruncate-rb.1
  (ftruncate (make-ratio 0 1) (make-bignum 10))
  0.0 0)

(deftest ftruncate-rb.2
  (ftruncate (make-ratio 0 1) (make-bignum -10))
  0.0 0)

(deftest-error ftruncate-rb.3
  (ftruncate 10/3 (make-bignum 0))
  division-by-zero)

(deftest ftruncate-rb.4a
  (ftruncate 2/3 (make-bignum 5))
  0.0 2/3)

(deftest ftruncate-rb.4b
  (ftruncate -2/3 (make-bignum 5))
  -0.0 -2/3)

(deftest ftruncate-rb.4c
  (ftruncate 2/3 (make-bignum -5))
  -0.0 2/3)

(deftest ftruncate-rb.4d
  (ftruncate -2/3 (make-bignum -5))
  0.0 -2/3)

(deftest ftruncate-rb.5a
  (ftruncate 20/3 (make-bignum 5))
  1.0 5/3)

(deftest ftruncate-rb.5b
  (ftruncate -20/3 (make-bignum 5))
  -1.0 -5/3)

(deftest ftruncate-rb.5c
  (ftruncate 20/3 (make-bignum -5))
  -1.0 5/3)

(deftest ftruncate-rb.5d
  (ftruncate -20/3 (make-bignum -5))
  1.0 -5/3)

(deftest ftruncate-rb.6a
  (ftruncate 53/3 (make-bignum 8))
  2.0 5/3)

(deftest ftruncate-rb.6b
  (ftruncate -53/3 (make-bignum 8))
  -2.0 -5/3)

(deftest ftruncate-rb.6c
  (ftruncate 53/3 (make-bignum -8))
  -2.0 5/3)

(deftest ftruncate-rb.6d
  (ftruncate -53/3 (make-bignum -8))
  2.0 -5/3)


;;  ratio - ratio
(deftest ftruncate-rr.1
  (ftruncate (make-ratio 0 1) 10/3)
  0.0 0)

(deftest ftruncate-rr.2
  (ftruncate (make-ratio 0 1) -10/3)
  0.0 0)

(deftest-error ftruncate-rr.3
  (ftruncate 10/3 (make-ratio 0 1))
  division-by-zero)

(deftest ftruncate-rr.4a
  (ftruncate 1/2 33/5)
  0.0 1/2)

(deftest ftruncate-rr.4b
  (ftruncate -1/2 33/5)
  -0.0 -1/2)

(deftest ftruncate-rr.4c
  (ftruncate 1/2 -33/5)
  -0.0 1/2)

(deftest ftruncate-rr.4d
  (ftruncate -1/2 -33/5)
  0.0 -1/2)

(deftest ftruncate-rr.5a
  (ftruncate 79/3 2/7)
  92.0 1/21)

(deftest ftruncate-rr.5b
  (ftruncate -79/3 2/7)
  -92.0 -1/21)

(deftest ftruncate-rr.5c
  (ftruncate 79/3 -2/7)
  -92.0 1/21)

(deftest ftruncate-rr.5d
  (ftruncate -79/3 -2/7)
  92.0 -1/21)

(deftest ftruncate-rr.6a
  (ftruncate 4/5 8/20)
  2.0 0)

(deftest ftruncate-rr.6b
  (ftruncate -4/5 8/20)
  -2.0 0)

(deftest ftruncate-rr.6c
  (ftruncate 4/5 -8/20)
  -2.0 0)

(deftest ftruncate-rr.6d
  (ftruncate -4/5 -8/20)
  2.0 0)


;;  ratio - single-float
(defun ftruncate-check-rs (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-rs.1
  (ftruncate (make-ratio 0 1) 10.0f0)
  0.0f0 0.0f0)

(deftest ftruncate-rs.2
  (ftruncate (make-ratio 0 1) -10.0f0)
  0.0f0 0.0f0)

(deftest-error ftruncate-rs.3
  (ftruncate 10/4 0.0f0)
  division-by-zero)

(deftest ftruncate-rs.4a
  (ftruncate-check-rs 3/4 5.5f0    0.0f0 0.75f0)
  t)

(deftest ftruncate-rs.4b
  (ftruncate-check-rs -3/4 5.5f0    0.0f0 -0.75f0)
  t)

(deftest ftruncate-rs.4c
  (ftruncate-check-rs 3/4 -5.5f0    0.0f0 0.75f0)
  t)

(deftest ftruncate-rs.4d
  (ftruncate-check-rs -3/4 -5.5f0    0.0f0 -0.75f0)
  t)

(deftest ftruncate-rs.5a
  (ftruncate-check-rs 77/4 1.6f0    12.0f0 0.05f0)
  t)

(deftest ftruncate-rs.5b
  (ftruncate-check-rs -77/4 1.6f0    -12.0f0 -0.05f0)
  t)

(deftest ftruncate-rs.5c
  (ftruncate-check-rs 77/4 -1.6f0    -12.0f0 0.05f0)
  t)

(deftest ftruncate-rs.5d
  (ftruncate-check-rs -77/4 -1.6f0    12.0f0 -0.05f0)
  t)


;;  ratio - double-float
(defun ftruncate-check-rd (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ftruncate-rd.1
  (ftruncate (make-ratio 0 1) 10.0d0)
  0.0d0 0.0d0)

(deftest ftruncate-rd.2
  (ftruncate (make-ratio 0 1) -10.0d0)
  0.0d0 0.0d0)

(deftest-error ftruncate-rd.3
  (ftruncate 10/4 0.0d0)
  division-by-zero)

(deftest ftruncate-rd.4a
  (ftruncate-check-rd 3/4 5.5d0    0.0d0 0.75d0)
  t)

(deftest ftruncate-rd.4b
  (ftruncate-check-rd -3/4 5.5d0    0.0d0 -0.75d0)
  t)

(deftest ftruncate-rd.4c
  (ftruncate-check-rd 3/4 -5.5d0    0.0d0 0.75d0)
  t)

(deftest ftruncate-rd.4d
  (ftruncate-check-rd -3/4 -5.5d0    0.0d0 -0.75d0)
  t)

(deftest ftruncate-rd.5a
  (ftruncate-check-rd 77/4 1.6d0    12.0d0 0.05d0)
  t)

(deftest ftruncate-rd.5b
  (ftruncate-check-rd -77/4 1.6d0    -12.0d0 -0.05d0)
  t)

(deftest ftruncate-rd.5c
  (ftruncate-check-rd 77/4 -1.6d0    -12.0d0 0.05d0)
  t)

(deftest ftruncate-rd.5d
  (ftruncate-check-rd -77/4 -1.6d0    12.0d0 -0.05d0)
  t)


;;  ratio - long-float
(defun ftruncate-check-rl (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14  ;; double -14
    :type 'long-float))

(deftest ftruncate-rl.1
  (ftruncate (make-ratio 0 1) 10.0l0)
  0.0l0 0.0l0)

(deftest ftruncate-rl.2
  (ftruncate (make-ratio 0 1) -10.0l0)
  0.0l0 0.0l0)

(deftest-error ftruncate-rl.3
  (ftruncate 10/4 0.0l0)
  division-by-zero)

(deftest ftruncate-rl.4a
  (ftruncate-check-rl 3/4 5.5l0    0.0l0 0.75l0)
  t)

(deftest ftruncate-rl.4b
  (ftruncate-check-rl -3/4 5.5l0    0.0l0 -0.75l0)
  t)

(deftest ftruncate-rl.4c
  (ftruncate-check-rl 3/4 -5.5l0    0.0l0 0.75l0)
  t)

(deftest ftruncate-rl.4d
  (ftruncate-check-rl -3/4 -5.5l0    0.0l0 -0.75l0)
  t)

(deftest ftruncate-rl.5a
  (ftruncate-check-rl 77/4 1.6l0    12.0l0 0.05l0)
  t)

(deftest ftruncate-rl.5b
  (ftruncate-check-rl -77/4 1.6l0    -12.0l0 -0.05l0)
  t)

(deftest ftruncate-rl.5c
  (ftruncate-check-rl 77/4 -1.6l0    -12.0l0 0.05l0)
  t)

(deftest ftruncate-rl.5d
  (ftruncate-check-rl -77/4 -1.6l0    12.0l0 -0.05l0)
  t)


;; single-float - fixnum
(defun ftruncate-check-sf (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-sf.1
  (ftruncate 0.0f0 10)
  0.0f0 0.0f0)

(deftest ftruncate-sf.2
  (ftruncate 0.0f0 -10)
  0.0f0 0.0f0)

(deftest-error ftruncate-sf.3
  (ftruncate 10.4f0 0)
  division-by-zero)

(deftest ftruncate-sf.4a
  (ftruncate-check-sf 1.2f0 10    0.0f0 1.2f0)
  t)

(deftest ftruncate-sf.4b
  (ftruncate-check-sf -1.2f0 10    0.0f0 -1.2f0)
  t)

(deftest ftruncate-sf.4c
  (ftruncate-check-sf 1.2f0 -10    0.0f0 1.2f0)
  t)

(deftest ftruncate-sf.4d
  (ftruncate-check-sf -1.2f0 -10    0.0f0 -1.2f0)
  t)

(deftest ftruncate-sf.5a
  (ftruncate-check-sf 10.2f0 4    2.0f0 2.2f0)
  t)

(deftest ftruncate-sf.5b
  (ftruncate-check-sf -10.2f0 4    -2.0f0 -2.2f0)
  t)

(deftest ftruncate-sf.5c
  (ftruncate-check-sf 10.2f0 -4    -2.0f0 2.2f0)
  t)

(deftest ftruncate-sf.5d
  (ftruncate-check-sf -10.2f0 -4    2.0f0 -2.2f0)
  t)


;; single-float - bignum
(defun ftruncate-check-sb (a b c d)
  (ftruncate-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-sb.1
  (ftruncate 0.0f0 (make-bignum 10))
  0.0f0 0.0f0)

(deftest ftruncate-sb.2
  (ftruncate 0.0f0 (make-bignum -10))
  0.0f0 0.0f0)

(deftest-error ftruncate-sb.3
  (ftruncate 10.4f0 (make-bignum 0))
  division-by-zero)

(deftest ftruncate-sb.4a
  (ftruncate-check-sb 1.2f0 10    0.0f0 1.2f0)
  t)

(deftest ftruncate-sb.4b
  (ftruncate-check-sb -1.2f0 10    0.0f0 -1.2f0)
  t)

(deftest ftruncate-sb.4c
  (ftruncate-check-sb 1.2f0 -10    0.0f0 1.2f0)
  t)

(deftest ftruncate-sb.4d
  (ftruncate-check-sb -1.2f0 -10    0.0f0 -1.2f0)
  t)

(deftest ftruncate-sb.5a
  (ftruncate-check-sb 10.2f0 4    2.0f0 2.2f0)
  t)

(deftest ftruncate-sb.5b
  (ftruncate-check-sb -10.2f0 4    -2.0f0 -2.2f0)
  t)

(deftest ftruncate-sb.5c
  (ftruncate-check-sb 10.2f0 -4    -2.0f0 2.2f0)
  t)

(deftest ftruncate-sb.5d
  (ftruncate-check-sb -10.2f0 -4    2.0f0 -2.2f0)
  t)


;; single-float - ratio
(defun ftruncate-check-sr (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-sr.1
  (ftruncate 0.0f0 10/3)
  0.0f0 0.0f0)

(deftest ftruncate-sr.2
  (ftruncate 0.0f0 -10/3)
  0.0f0 0.0f0)

(deftest-error ftruncate-sr.3
  (ftruncate 10.4f0 (make-ratio 0 1))
  division-by-zero)

(deftest ftruncate-sr.4a
  (ftruncate-check-sr 1.2f0 15/4    0.0f0 1.2f0)
  t)

(deftest ftruncate-sr.4b
  (ftruncate-check-sr -1.2f0 15/4    0.0f0 -1.2f0)
  t)

(deftest ftruncate-sr.4c
  (ftruncate-check-sr 1.2f0 -15/4    0.0f0 1.2f0)
  t)

(deftest ftruncate-sr.4d
  (ftruncate-check-sr -1.2f0 -15/4    0.0f0 -1.2f0)
  t)

(deftest ftruncate-sr.5a
  (ftruncate-check-sr 1.2f0 1/4    4.0f0 0.2f0)
  t)

(deftest ftruncate-sr.5b
  (ftruncate-check-sr -1.2f0 1/4    -4.0f0 -0.2f0)
  t)

(deftest ftruncate-sr.5c
  (ftruncate-check-sr 1.2f0 -1/4    -4.0f0 0.2f0)
  t)

(deftest ftruncate-sr.5d
  (ftruncate-check-sr -1.2f0 -1/4    4.0f0 -0.2f0)
  t)


;; single-float - single-float
(defun ftruncate-check-ss (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest ftruncate-ss.1
  (ftruncate 0.0f0 10.0f0)
  0.0f0 0.0f0)

(deftest ftruncate-ss.2
  (ftruncate 0.0f0 -10.0f0)
  0.0f0 0.0f0)

(deftest-error ftruncate-ss.3
  (ftruncate 10.4f0 0.0f0)
  division-by-zero)

(deftest ftruncate-ss.4a
  (ftruncate-check-ss 1.2f0 3.7f0    0.0f0 1.2f0)
  t)

(deftest ftruncate-ss.4b
  (ftruncate-check-ss -1.2f0 3.7f0    0.0f0 -1.2f0)
  t)

(deftest ftruncate-ss.4c
  (ftruncate-check-ss 1.2f0 -3.7f0    0.0f0 1.2f0)
  t)

(deftest ftruncate-ss.4d
  (ftruncate-check-ss -1.2f0 -3.7f0    0.0f0 -1.2f0)
  t)

(deftest ftruncate-ss.5a
  (ftruncate-check-ss 12.3f0 3.7f0    3.0f0 1.2f0)
  t)

(deftest ftruncate-ss.5b
  (ftruncate-check-ss -12.3f0 3.7f0    -3.0f0 -1.2f0)
  t)

(deftest ftruncate-ss.5c
  (ftruncate-check-ss 12.3f0 -3.7f0    -3.0f0 1.2f0)
  t)

(deftest ftruncate-ss.5d
  (ftruncate-check-ss -12.3f0 -3.7f0    3.0f0 -1.2f0)
  t)


;; single-float - double-float
(defun ftruncate-check-sd (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :type 'double-float))

(deftest ftruncate-sd.1
  (ftruncate 0.0f0 10.0d0)
  0.0d0 0.0d0)

(deftest ftruncate-sd.2
  (ftruncate 0.0f0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ftruncate-sd.3
  (ftruncate 10.4f0 0.0d0)
  division-by-zero)

(deftest ftruncate-sd.4a
  (ftruncate-check-sd 1.2f0 3.7d0    0.0d0 1.2d0)
  t)

(deftest ftruncate-sd.4b
  (ftruncate-check-sd -1.2f0 3.7d0    0.0d0 -1.2d0)
  t)

(deftest ftruncate-sd.4c
  (ftruncate-check-sd 1.2f0 -3.7d0    0.0d0 1.2d0)
  t)

(deftest ftruncate-sd.4d
  (ftruncate-check-sd -1.2f0 -3.7d0    0.0d0 -1.2d0)
  t)

(deftest ftruncate-sd.5a
  (ftruncate-check-sd 12.3f0 3.7d0    3.0d0 1.2d0)
  t)

(deftest ftruncate-sd.5b
  (ftruncate-check-sd -12.3f0 3.7d0    -3.0d0 -1.2d0)
  t)

(deftest ftruncate-sd.5c
  (ftruncate-check-sd 12.3f0 -3.7d0    -3.0d0 1.2d0)
  t)

(deftest ftruncate-sd.5d
  (ftruncate-check-sd -12.3f0 -3.7d0    3.0d0 -1.2d0)
  t)


;; single-float - long-float
(defun ftruncate-check-sl (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6  ;; single-float
    :type 'long-float))

(deftest ftruncate-sl.1
  (ftruncate 0.0f0 10.0l0)
  0.0l0 0.0l0)

(deftest ftruncate-sl.2
  (ftruncate 0.0f0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ftruncate-sl.3
  (ftruncate 10.4f0 0.0l0)
  division-by-zero)

(deftest ftruncate-sl.4a
  (ftruncate-check-sl 1.2f0 3.7l0    0.0l0 1.2l0)
  t)

(deftest ftruncate-sl.4b
  (ftruncate-check-sl -1.2f0 3.7l0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-sl.4c
  (ftruncate-check-sl 1.2f0 -3.7l0    0.0l0 1.2l0)
  t)

(deftest ftruncate-sl.4d
  (ftruncate-check-sl -1.2f0 -3.7l0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-sl.5a
  (ftruncate-check-sl 12.3f0 3.7l0    3.0l0 1.2l0)
  t)

(deftest ftruncate-sl.5b
  (ftruncate-check-sl -12.3f0 3.7l0    -3.0l0 -1.2l0)
  t)

(deftest ftruncate-sl.5c
  (ftruncate-check-sl 12.3f0 -3.7l0    -3.0l0 1.2l0)
  t)

(deftest ftruncate-sl.5d
  (ftruncate-check-sl -12.3f0 -3.7l0    3.0l0 -1.2l0)
  t)


;; double-float - fixnum
(defun ftruncate-check-df (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-14
    :type 'double-float))

(deftest ftruncate-df.1
  (ftruncate 0.0d0 10)
  0.0d0 0.0d0)

(deftest ftruncate-df.2
  (ftruncate 0.0d0 -10)
  0.0d0 0.0d0)

(deftest-error ftruncate-df.3
  (ftruncate 10.4d0 0)
  division-by-zero)

(deftest ftruncate-df.4a
  (ftruncate-check-df 1.2d0 10    0.0d0 1.2d0)
  t)

(deftest ftruncate-df.4b
  (ftruncate-check-df -1.2d0 10    0.0d0 -1.2d0)
  t)

(deftest ftruncate-df.4c
  (ftruncate-check-df 1.2d0 -10    0.0d0 1.2d0)
  t)

(deftest ftruncate-df.4d
  (ftruncate-check-df -1.2d0 -10    0.0d0 -1.2d0)
  t)

(deftest ftruncate-df.5a
  (ftruncate-check-df 10.2d0 4    2.0d0 2.2d0)
  t)

(deftest ftruncate-df.5b
  (ftruncate-check-df -10.2d0 4    -2.0d0 -2.2d0)
  t)

(deftest ftruncate-df.5c
  (ftruncate-check-df 10.2d0 -4    -2.0d0 2.2d0)
  t)

(deftest ftruncate-df.5d
  (ftruncate-check-df -10.2d0 -4    2.0d0 -2.2d0)
  t)


;; double-float - bignum
(defun ftruncate-check-db (a b c d)
  (ftruncate-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ftruncate-db.1
  (ftruncate 0.0d0 (make-bignum 10))
  0.0d0 0.0d0)

(deftest ftruncate-db.2
  (ftruncate 0.0d0 (make-bignum -10))
  0.0d0 0.0d0)

(deftest-error ftruncate-db.3
  (ftruncate 10.4d0 (make-bignum 0))
  division-by-zero)

(deftest ftruncate-db.4a
  (ftruncate-check-db 1.2d0 10    0.0d0 1.2d0)
  t)

(deftest ftruncate-db.4b
  (ftruncate-check-db -1.2d0 10    0.0d0 -1.2d0)
  t)

(deftest ftruncate-db.4c
  (ftruncate-check-db 1.2d0 -10    0.0d0 1.2d0)
  t)

(deftest ftruncate-db.4d
  (ftruncate-check-db -1.2d0 -10    0.0d0 -1.2d0)
  t)

(deftest ftruncate-db.5a
  (ftruncate-check-db 10.2d0 4    2.0d0 2.2d0)
  t)

(deftest ftruncate-db.5b
  (ftruncate-check-db -10.2d0 4    -2.0d0 -2.2d0)
  t)

(deftest ftruncate-db.5c
  (ftruncate-check-db 10.2d0 -4    -2.0d0 2.2d0)
  t)

(deftest ftruncate-db.5d
  (ftruncate-check-db -10.2d0 -4    2.0d0 -2.2d0)
  t)


;; double-float - ratio
(defun ftruncate-check-dr (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ftruncate-dr.1
  (ftruncate 0.0d0 10/3)
  0.0d0 0.0d0)

(deftest ftruncate-dr.2
  (ftruncate 0.0d0 -10/3)
  0.0d0 0.0d0)

(deftest-error ftruncate-dr.3
  (ftruncate 10.4d0 (make-ratio 0 1))
  division-by-zero)

(deftest ftruncate-dr.4a
  (ftruncate-check-dr 1.2d0 15/4    0.0d0 1.2d0)
  t)

(deftest ftruncate-dr.4b
  (ftruncate-check-dr -1.2d0 15/4    0.0d0 -1.2d0)
  t)

(deftest ftruncate-dr.4c
  (ftruncate-check-dr 1.2d0 -15/4    0.0d0 1.2d0)
  t)

(deftest ftruncate-dr.4d
  (ftruncate-check-dr -1.2d0 -15/4    0.0d0 -1.2d0)
  t)

(deftest ftruncate-dr.5a
  (ftruncate-check-dr 1.2d0 1/4    4.0d0 0.2d0)
  t)

(deftest ftruncate-dr.5b
  (ftruncate-check-dr -1.2d0 1/4    -4.0d0 -0.2d0)
  t)

(deftest ftruncate-dr.5c
  (ftruncate-check-dr 1.2d0 -1/4    -4.0d0 0.2d0)
  t)

(deftest ftruncate-dr.5d
  (ftruncate-check-dr -1.2d0 -1/4    4.0d0 -0.2d0)
  t)


;; double-float - single-float
(defun ftruncate-check-ds (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest ftruncate-ds.1
  (ftruncate 0.0d0 10.0f0)
  0.0d0 0.0d0)

(deftest ftruncate-ds.2
  (ftruncate 0.0d0 -10.0f0)
  0.0d0 0.0d0)

(deftest-error ftruncate-ds.3
  (ftruncate 10.4d0 0.0f0)
  division-by-zero)

(deftest ftruncate-ds.4a
  (ftruncate-check-ds 1.2d0 3.7f0    0.0d0 1.2d0)
  t)

(deftest ftruncate-ds.4b
  (ftruncate-check-ds -1.2d0 3.7f0    0.0d0 -1.2d0)
  t)

(deftest ftruncate-ds.4c
  (ftruncate-check-ds 1.2d0 -3.7f0    0.0d0 1.2d0)
  t)

(deftest ftruncate-ds.4d
  (ftruncate-check-ds -1.2d0 -3.7f0    0.0d0 -1.2d0)
  t)

(deftest ftruncate-ds.5a
  (ftruncate-check-ds 12.3d0 3.7f0    3.0d0 1.2d0)
  t)

(deftest ftruncate-ds.5b
  (ftruncate-check-ds -12.3d0 3.7f0    -3.0d0 -1.2d0)
  t)

(deftest ftruncate-ds.5c
  (ftruncate-check-ds 12.3d0 -3.7f0    -3.0d0 1.2d0)
  t)

(deftest ftruncate-ds.5d
  (ftruncate-check-ds -12.3d0 -3.7f0    3.0d0 -1.2d0)
  t)


;; double-float - double-float
(defun ftruncate-check-dd (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest ftruncate-dd.1
  (ftruncate 0.0d0 10.0d0)
  0.0d0 0.0d0)

(deftest ftruncate-dd.2
  (ftruncate 0.0d0 -10.0d0)
  0.0d0 0.0d0)

(deftest-error ftruncate-dd.3
  (ftruncate 10.4d0 0.0d0)
  division-by-zero)

(deftest ftruncate-dd.4a
  (ftruncate-check-dd 1.2d0 3.7d0    0.0d0 1.2d0)
  t)

(deftest ftruncate-dd.4b
  (ftruncate-check-dd -1.2d0 3.7d0    0.0d0 -1.2d0)
  t)

(deftest ftruncate-dd.4c
  (ftruncate-check-dd 1.2d0 -3.7d0    0.0d0 1.2d0)
  t)

(deftest ftruncate-dd.4d
  (ftruncate-check-dd -1.2d0 -3.7d0    0.0d0 -1.2d0)
  t)

(deftest ftruncate-dd.5a
  (ftruncate-check-dd 12.3d0 3.7d0    3.0d0 1.2d0)
  t)

(deftest ftruncate-dd.5b
  (ftruncate-check-dd -12.3d0 3.7d0    -3.0d0 -1.2d0)
  t)

(deftest ftruncate-dd.5c
  (ftruncate-check-dd 12.3d0 -3.7d0    -3.0d0 1.2d0)
  t)

(deftest ftruncate-dd.5d
  (ftruncate-check-dd -12.3d0 -3.7d0    3.0d0 -1.2d0)
  t)


;; double-float - long-float
(defun ftruncate-check-dl (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ftruncate-dl.1
  (ftruncate 0.0d0 10.0l0)
  0.0l0 0.0l0)

(deftest ftruncate-dl.2
  (ftruncate 0.0d0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ftruncate-dl.3
  (ftruncate 10.4d0 0.0l0)
  division-by-zero)

(deftest ftruncate-dl.4a
  (ftruncate-check-dl 1.2d0 3.7l0    0.0l0 1.2l0)
  t)

(deftest ftruncate-dl.4b
  (ftruncate-check-dl -1.2d0 3.7l0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-dl.4c
  (ftruncate-check-dl 1.2d0 -3.7l0    0.0l0 1.2l0)
  t)

(deftest ftruncate-dl.4d
  (ftruncate-check-dl -1.2d0 -3.7l0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-dl.5a
  (ftruncate-check-dl 12.3d0 3.7l0    3.0l0 1.2l0)
  t)

(deftest ftruncate-dl.5b
  (ftruncate-check-dl -12.3d0 3.7l0    -3.0l0 -1.2l0)
  t)

(deftest ftruncate-dl.5c
  (ftruncate-check-dl 12.3d0 -3.7l0    -3.0l0 1.2l0)
  t)

(deftest ftruncate-dl.5d
  (ftruncate-check-dl -12.3d0 -3.7l0    3.0l0 -1.2l0)
  t)


;; long-float - fixnum
(defun ftruncate-check-lf (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-14
    :type 'long-float))

(deftest ftruncate-lf.1
  (ftruncate 0.0l0 10)
  0.0l0 0.0l0)

(deftest ftruncate-lf.2
  (ftruncate 0.0l0 -10)
  0.0l0 0.0l0)

(deftest-error ftruncate-lf.3
  (ftruncate 10.4l0 0)
  division-by-zero)

(deftest ftruncate-lf.4a
  (ftruncate-check-lf 1.2l0 10    0.0l0 1.2l0)
  t)

(deftest ftruncate-lf.4b
  (ftruncate-check-lf -1.2l0 10    0.0l0 -1.2l0)
  t)

(deftest ftruncate-lf.4c
  (ftruncate-check-lf 1.2l0 -10    0.0l0 1.2l0)
  t)

(deftest ftruncate-lf.4d
  (ftruncate-check-lf -1.2l0 -10    0.0l0 -1.2l0)
  t)

(deftest ftruncate-lf.5a
  (ftruncate-check-lf 10.2l0 4    2.0l0 2.2l0)
  t)

(deftest ftruncate-lf.5b
  (ftruncate-check-lf -10.2l0 4    -2.0l0 -2.2l0)
  t)

(deftest ftruncate-lf.5c
  (ftruncate-check-lf 10.2l0 -4    -2.0l0 2.2l0)
  t)

(deftest ftruncate-lf.5d
  (ftruncate-check-lf -10.2l0 -4    2.0l0 -2.2l0)
  t)


;; long-float - bignum
(defun ftruncate-check-lb (a b c d)
  (ftruncate-equal
    a (make-bignum b) c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ftruncate-lb.1
  (ftruncate 0.0l0 (make-bignum 10))
  0.0l0 0.0l0)

(deftest ftruncate-lb.2
  (ftruncate 0.0l0 (make-bignum -10))
  0.0l0 0.0l0)

(deftest-error ftruncate-lb.3
  (ftruncate 10.4l0 (make-bignum 0))
  division-by-zero)

(deftest ftruncate-lb.4a
  (ftruncate-check-lb 1.2l0 10    0.0l0 1.2l0)
  t)

(deftest ftruncate-lb.4b
  (ftruncate-check-lb -1.2l0 10    0.0l0 -1.2l0)
  t)

(deftest ftruncate-lb.4c
  (ftruncate-check-lb 1.2l0 -10    0.0l0 1.2l0)
  t)

(deftest ftruncate-lb.4d
  (ftruncate-check-lb -1.2l0 -10    0.0l0 -1.2l0)
  t)

(deftest ftruncate-lb.5a
  (ftruncate-check-lb 10.2l0 4    2.0l0 2.2l0)
  t)

(deftest ftruncate-lb.5b
  (ftruncate-check-lb -10.2l0 4    -2.0l0 -2.2l0)
  t)

(deftest ftruncate-lb.5c
  (ftruncate-check-lb 10.2l0 -4    -2.0l0 2.2l0)
  t)

(deftest ftruncate-lb.5d
  (ftruncate-check-lb -10.2l0 -4    2.0l0 -2.2l0)
  t)


;; long-float - ratio
(defun ftruncate-check-lr (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ftruncate-lr.1
  (ftruncate 0.0l0 10/3)
  0.0l0 0.0l0)

(deftest ftruncate-lr.2
  (ftruncate 0.0l0 -10/3)
  0.0l0 0.0l0)

(deftest-error ftruncate-lr.3
  (ftruncate 10.4l0 (make-ratio 0 1))
  division-by-zero)

(deftest ftruncate-lr.4a
  (ftruncate-check-lr 1.2l0 15/4    0.0l0 1.2l0)
  t)

(deftest ftruncate-lr.4b
  (ftruncate-check-lr -1.2l0 15/4    0.0l0 -1.2l0)
  t)

(deftest ftruncate-lr.4c
  (ftruncate-check-lr 1.2l0 -15/4    0.0l0 1.2l0)
  t)

(deftest ftruncate-lr.4d
  (ftruncate-check-lr -1.2l0 -15/4    0.0l0 -1.2l0)
  t)

(deftest ftruncate-lr.5a
  (ftruncate-check-lr 1.2l0 1/4    4.0l0 0.2l0)
  t)

(deftest ftruncate-lr.5b
  (ftruncate-check-lr -1.2l0 1/4    -4.0l0 -0.2l0)
  t)

(deftest ftruncate-lr.5c
  (ftruncate-check-lr 1.2l0 -1/4    -4.0l0 0.2l0)
  t)

(deftest ftruncate-lr.5d
  (ftruncate-check-lr -1.2l0 -1/4    4.0l0 -0.2l0)
  t)


;; long-float - single-float
(defun ftruncate-check-ls (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest ftruncate-ls.1
  (ftruncate 0.0l0 10.0f0)
  0.0l0 0.0l0)

(deftest ftruncate-ls.2
  (ftruncate 0.0l0 -10.0f0)
  0.0l0 0.0l0)

(deftest-error ftruncate-ls.3
  (ftruncate 10.4l0 0.0f0)
  division-by-zero)

(deftest ftruncate-ls.4a
  (ftruncate-check-ls 1.2l0 3.7f0    0.0l0 1.2l0)
  t)

(deftest ftruncate-ls.4b
  (ftruncate-check-ls -1.2l0 3.7f0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-ls.4c
  (ftruncate-check-ls 1.2l0 -3.7f0    0.0l0 1.2l0)
  t)

(deftest ftruncate-ls.4d
  (ftruncate-check-ls -1.2l0 -3.7f0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-ls.5a
  (ftruncate-check-ls 12.3l0 3.7f0    3.0l0 1.2l0)
  t)

(deftest ftruncate-ls.5b
  (ftruncate-check-ls -12.3l0 3.7f0    -3.0l0 -1.2l0)
  t)

(deftest ftruncate-ls.5c
  (ftruncate-check-ls 12.3l0 -3.7f0    -3.0l0 1.2l0)
  t)

(deftest ftruncate-ls.5d
  (ftruncate-check-ls -12.3l0 -3.7f0    3.0l0 -1.2l0)
  t)


;; long-float - double-float
(defun ftruncate-check-ld (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ftruncate-ld.1
  (ftruncate 0.0l0 10.0d0)
  0.0l0 0.0l0)

(deftest ftruncate-ld.2
  (ftruncate 0.0l0 -10.0d0)
  0.0l0 0.0l0)

(deftest-error ftruncate-ld.3
  (ftruncate 10.4l0 0.0d0)
  division-by-zero)

(deftest ftruncate-ld.4a
  (ftruncate-check-ld 1.2l0 3.7d0    0.0l0 1.2l0)
  t)

(deftest ftruncate-ld.4b
  (ftruncate-check-ld -1.2l0 3.7d0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-ld.4c
  (ftruncate-check-ld 1.2l0 -3.7d0    0.0l0 1.2l0)
  t)

(deftest ftruncate-ld.4d
  (ftruncate-check-ld -1.2l0 -3.7d0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-ld.5a
  (ftruncate-check-ld 12.3l0 3.7d0    3.0l0 1.2l0)
  t)

(deftest ftruncate-ld.5b
  (ftruncate-check-ld -12.3l0 3.7d0    -3.0l0 -1.2l0)
  t)

(deftest ftruncate-ld.5c
  (ftruncate-check-ld 12.3l0 -3.7d0    -3.0l0 1.2l0)
  t)

(deftest ftruncate-ld.5d
  (ftruncate-check-ld -12.3l0 -3.7d0    3.0l0 -1.2l0)
  t)


;; long-float - long-float
(defun ftruncate-check-ll (a b c d)
  (ftruncate-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest ftruncate-ll.1
  (ftruncate 0.0l0 10.0l0)
  0.0l0 0.0l0)

(deftest ftruncate-ll.2
  (ftruncate 0.0l0 -10.0l0)
  0.0l0 0.0l0)

(deftest-error ftruncate-ll.3
  (ftruncate 10.4l0 0.0l0)
  division-by-zero)

(deftest ftruncate-ll.4a
  (ftruncate-check-ll 1.2l0 3.7l0    0.0l0 1.2l0)
  t)

(deftest ftruncate-ll.4b
  (ftruncate-check-ll -1.2l0 3.7l0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-ll.4c
  (ftruncate-check-ll 1.2l0 -3.7l0    0.0l0 1.2l0)
  t)

(deftest ftruncate-ll.4d
  (ftruncate-check-ll -1.2l0 -3.7l0    0.0l0 -1.2l0)
  t)

(deftest ftruncate-ll.5a
  (ftruncate-check-ll 12.3l0 3.7l0    3.0l0 1.2l0)
  t)

(deftest ftruncate-ll.5b
  (ftruncate-check-ll -12.3l0 3.7l0    -3.0l0 -1.2l0)
  t)

(deftest ftruncate-ll.5c
  (ftruncate-check-ll 12.3l0 -3.7l0    -3.0l0 1.2l0)
  t)

(deftest ftruncate-ll.5d
  (ftruncate-check-ll -12.3l0 -3.7l0    3.0l0 -1.2l0)
  t)

