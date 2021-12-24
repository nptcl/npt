;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  fround
;;

;;  single-float - fixnum
(defun fround-check-sf (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-sf.1
  (fround 0.0f0 10)
  0.0f0 0.0f0)

(deftest fround-sf.2
  (fround 0.0f0 -10)
  0.0f0 0.0f0)

(deftest-error fround-sf.3
  (fround 10.3f0 0)
  division-by-zero)

(deftest fround-sf.4a
  (fround 10.3f0 100)
  0.0f0 10.3f0)

(deftest fround-sf.4b
  (fround -10.3f0 100)
  -0.0f0 -10.3f0)

(deftest fround-sf.4c
  (fround 10.3f0 -100)
  -0.0f0 10.3f0)

(deftest fround-sf.4d
  (fround -10.3f0 -100)
  0.0f0 -10.3f0)

(deftest fround-sf.5a
  (fround-check-sf 12.3f0 3    4.0f0 0.3f0)
  t)

(deftest fround-sf.5b
  (fround-check-sf -12.3f0 3    -4.0f0 -0.3f0)
  t)

(deftest fround-sf.5c
  (fround-check-sf 12.3f0 -3    -4.0f0 0.3f0)
  t)

(deftest fround-sf.5d
  (fround-check-sf -12.3f0 -3    4.0f0 -0.3f0)
  t)

(deftest fround-sf.6a
  (fround-check-sf 14.3f0 3    5.0f0 -0.7f0)
  t)

(deftest fround-sf.6b
  (fround-check-sf -14.3f0 3    -5.0f0 0.7f0)
  t)

(deftest fround-sf.6c
  (fround-check-sf 14.3f0 -3    -5.0f0 -0.7f0)
  t)

(deftest fround-sf.6d
  (fround-check-sf -14.3f0 -3    5.0f0 0.7f0)
  t)


;;  single-float - bignum
(defun fround-sb (a b)
  (fround a (make-bignum b)))

(defun fround-check-sb (a b c d)
  (fround-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-sb.1
  (fround-sb 0.0f0 10)
  0.0f0 0.0f0)

(deftest fround-sb.2
  (fround-sb 0.0f0 -10)
  0.0f0 0.0f0)

(deftest-error fround-sb.3
  (fround-sb 10.3f0 0)
  division-by-zero)

(deftest fround-sb.4a
  (fround-sb 10.3f0 100)
  0.0f0 10.3f0)

(deftest fround-sb.4b
  (fround-sb -10.3f0 100)
  -0.0f0 -10.3f0)

(deftest fround-sb.4c
  (fround-sb 10.3f0 -100)
  -0.0f0 10.3f0)

(deftest fround-sb.4d
  (fround-sb -10.3f0 -100)
  0.0f0 -10.3f0)

(deftest fround-sb.5a
  (fround-check-sb 12.3f0 3    4.0f0 0.3f0)
  t)

(deftest fround-sb.5b
  (fround-check-sb -12.3f0 3    -4.0f0 -0.3f0)
  t)

(deftest fround-sb.5c
  (fround-check-sb 12.3f0 -3    -4.0f0 0.3f0)
  t)

(deftest fround-sb.5d
  (fround-check-sb -12.3f0 -3    4.0f0 -0.3f0)
  t)

(deftest fround-sb.6a
  (fround-check-sb 14.3f0 3    5.0f0 -0.7f0)
  t)

(deftest fround-sb.6b
  (fround-check-sb -14.3f0 3    -5.0f0 0.7f0)
  t)

(deftest fround-sb.6c
  (fround-check-sb 14.3f0 -3    -5.0f0 -0.7f0)
  t)

(deftest fround-sb.6d
  (fround-check-sb -14.3f0 -3    5.0f0 0.7f0)
  t)


;;  single-float - ratio
(defun fround-check-sr (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-sr.1
  (fround 0.0f0 10/3)
  0.0f0 0.0f0)

(deftest fround-sr.2
  (fround 0.0f0 -10/3)
  0.0f0 0.0f0)

(deftest-error fround-sr.3
  (fround 10.3f0 (make-ratio 0 1))
  division-by-zero)

(deftest fround-sr.4a
  (fround 10.3f0 100/3)
  0.0f0 10.3f0)

(deftest fround-sr.4b
  (fround -10.3f0 100/3)
  -0.0f0 -10.3f0)

(deftest fround-sr.4c
  (fround 10.3f0 -100/3)
  -0.0f0 10.3f0)

(deftest fround-sr.4d
  (fround -10.3f0 -100/3)
  0.0f0 -10.3f0)

(deftest fround-sr.5a
  (fround-check-sr 12.3f0 2/3    18.0f0 0.3f0)
  t)

(deftest fround-sr.5b
  (fround-check-sr -12.3f0 2/3    -18.0f0 -0.3f0)
  t)

(deftest fround-sr.5c
  (fround-check-sr 12.3f0 -2/3    -18.0f0 0.3f0)
  t)

(deftest fround-sr.5d
  (fround-check-sr -12.3f0 -2/3    18.0f0 -0.3f0)
  t)

(deftest fround-sr.6a
  (fround-check-sr 5.2f0 3/4   7.0f0 -0.05f0)
  t)

(deftest fround-sr.6b
  (fround-check-sr -5.2f0 3/4    -7.0f0 0.05f0)
  t)

(deftest fround-sr.6c
  (fround-check-sr 5.2f0 -3/4    -7.0f0 -0.05f0)
  t)

(deftest fround-sr.6d
  (fround-check-sr -5.2f0 -3/4    7.0f0 0.05f0)
  t)


;;  single-float - single-float
(defun fround-check-ss (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'single-float))

(deftest fround-ss.1
  (fround 0.0f0 10.3f0)
  0.0f0 0.0f0)

(deftest fround-ss.2
  (fround 0.0f0 10.3f0)
  0.0f0 0.0f0)

(deftest-error fround-ss.3
  (fround 10.3f0 0.0f0)
  division-by-zero)

(deftest fround-ss.4a
  (fround-check-ss 10.3f0 100.0f0    0.0f0 10.3f0)
  t)

(deftest fround-ss.4b
  (fround-check-ss -10.3f0 100.0f0    0.0f0 -10.3f0)
  t)

(deftest fround-ss.4c
  (fround-check-ss 10.3f0 -100.0f0    0.0f0 10.3f0)
  t)

(deftest fround-ss.4d
  (fround-check-ss -10.3f0 -100.0f0    0.0f0 -10.3f0)
  t)

(deftest fround-ss.5a
  (fround-check-ss 12.3f0 1.5f0    8.0f0 0.3f0)
  t)

(deftest fround-ss.5b
  (fround-check-ss -12.3f0 1.5f0    -8.0f0 -0.3f0)
  t)

(deftest fround-ss.5c
  (fround-check-ss 12.3f0 -1.5f0    -8.0f0 0.3f0)
  t)

(deftest fround-ss.5d
  (fround-check-ss -12.3f0 -1.5f0    8.0f0 -0.3f0)
  t)

(deftest fround-ss.6a
  (fround-check-ss 12.3f0 4.5f0    3.0f0 -1.2f0)
  t)

(deftest fround-ss.6b
  (fround-check-ss -12.3f0 4.5f0    -3.0f0 1.2f0)
  t)

(deftest fround-ss.6c
  (fround-check-ss 12.3f0 -4.5f0    -3.0f0 -1.2f0)
  t)

(deftest fround-ss.6d
  (fround-check-ss -12.3f0 -4.5f0    3.0f0 1.2f0)
  t)


;;  single-float - double-float
(defun fround-check-sd (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest fround-sd.1
  (fround 0.0f0 10.3d0)
  0.0d0 0.0d0)

(deftest fround-sd.2
  (fround 0.0f0 10.3d0)
  0.0d0 0.0d0)

(deftest-error fround-sd.3
  (fround 10.3f0 0.0d0)
  division-by-zero)

(deftest fround-sd.4a
  (fround-check-sd 10.3f0 100.0d0    0.0d0 10.3d0)
  t)

(deftest fround-sd.4b
  (fround-check-sd -10.3f0 100.0d0    0.0d0 -10.3d0)
  t)

(deftest fround-sd.4c
  (fround-check-sd 10.3f0 -100.0d0    0.0d0 10.3d0)
  t)

(deftest fround-sd.4d
  (fround-check-sd -10.3f0 -100.0d0    0.0d0 -10.3d0)
  t)

(deftest fround-sd.5a
  (fround-check-sd 12.3f0 1.5d0    8.0d0 0.3d0)
  t)

(deftest fround-sd.5b
  (fround-check-sd -12.3f0 1.5d0    -8.0d0 -0.3d0)
  t)

(deftest fround-sd.5c
  (fround-check-sd 12.3f0 -1.5d0    -8.0d0 0.3d0)
  t)

(deftest fround-sd.5d
  (fround-check-sd -12.3f0 -1.5d0    8.0d0 -0.3d0)
  t)

(deftest fround-sd.6a
  (fround-check-sd 12.3f0 4.5d0    3.0d0 -1.2d0)
  t)

(deftest fround-sd.6b
  (fround-check-sd -12.3f0 4.5d0    -3.0d0 1.2d0)
  t)

(deftest fround-sd.6c
  (fround-check-sd 12.3f0 -4.5d0    -3.0d0 -1.2d0)
  t)

(deftest fround-sd.6d
  (fround-check-sd -12.3f0 -4.5d0    3.0d0 1.2d0)
  t)


;;  single-float - long-float
(defun fround-check-sl (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest fround-sl.1
  (fround 0.0f0 10.3l0)
  0.0l0 0.0l0)

(deftest fround-sl.2
  (fround 0.0f0 10.3l0)
  0.0l0 0.0l0)

(deftest-error fround-sl.3
  (fround 10.3f0 0.0l0)
  division-by-zero)

(deftest fround-sl.4a
  (fround-check-sl 10.3f0 100.0l0    0.0l0 10.3l0)
  t)

(deftest fround-sl.4b
  (fround-check-sl -10.3f0 100.0l0    0.0l0 -10.3l0)
  t)

(deftest fround-sl.4c
  (fround-check-sl 10.3f0 -100.0l0    0.0l0 10.3l0)
  t)

(deftest fround-sl.4d
  (fround-check-sl -10.3f0 -100.0l0    0.0l0 -10.3l0)
  t)

(deftest fround-sl.5a
  (fround-check-sl 12.3f0 1.5l0    8.0l0 0.3l0)
  t)

(deftest fround-sl.5b
  (fround-check-sl -12.3f0 1.5l0    -8.0l0 -0.3l0)
  t)

(deftest fround-sl.5c
  (fround-check-sl 12.3f0 -1.5l0    -8.0l0 0.3l0)
  t)

(deftest fround-sl.5d
  (fround-check-sl -12.3f0 -1.5l0    8.0l0 -0.3l0)
  t)

(deftest fround-sl.6a
  (fround-check-sl 12.3f0 4.5l0    3.0l0 -1.2l0)
  t)

(deftest fround-sl.6b
  (fround-check-sl -12.3f0 4.5l0    -3.0l0 1.2l0)
  t)

(deftest fround-sl.6c
  (fround-check-sl 12.3f0 -4.5l0    -3.0l0 -1.2l0)
  t)

(deftest fround-sl.6d
  (fround-check-sl -12.3f0 -4.5l0    3.0l0 1.2l0)
  t)


;;  double-float - fixnum
(defun fround-check-df (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest fround-df.1
  (fround 0.0d0 10)
  0.0d0 0.0d0)

(deftest fround-df.2
  (fround 0.0d0 -10)
  0.0d0 0.0d0)

(deftest-error fround-df.3
  (fround 10.3d0 0)
  division-by-zero)

(deftest fround-df.4a
  (fround 10.3d0 100)
  0.0d0 10.3d0)

(deftest fround-df.4b
  (fround -10.3d0 100)
  -0.0d0 -10.3d0)

(deftest fround-df.4c
  (fround 10.3d0 -100)
  -0.0d0 10.3d0)

(deftest fround-df.4d
  (fround -10.3d0 -100)
  0.0d0 -10.3d0)

(deftest fround-df.5a
  (fround-check-df 12.3d0 3    4.0d0 0.3d0)
  t)

(deftest fround-df.5b
  (fround-check-df -12.3d0 3    -4.0d0 -0.3d0)
  t)

(deftest fround-df.5c
  (fround-check-df 12.3d0 -3    -4.0d0 0.3d0)
  t)

(deftest fround-df.5d
  (fround-check-df -12.3d0 -3    4.0d0 -0.3d0)
  t)

(deftest fround-df.6a
  (fround-check-df 14.3d0 3    5.0d0 -0.7d0)
  t)

(deftest fround-df.6b
  (fround-check-df -14.3d0 3    -5.0d0 0.7d0)
  t)

(deftest fround-df.6c
  (fround-check-df 14.3d0 -3    -5.0d0 -0.7d0)
  t)

(deftest fround-df.6d
  (fround-check-df -14.3d0 -3    5.0d0 0.7d0)
  t)


;;  double-float - bignum
(defun fround-db (a b)
  (fround a (make-bignum b)))

(defun fround-check-db (a b c d)
  (fround-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :type 'double-float))

(deftest fround-db.1
  (fround-db 0.0d0 10)
  0.0d0 0.0d0)

(deftest fround-db.2
  (fround-db 0.0d0 -10)
  0.0d0 0.0d0)

(deftest-error fround-db.3
  (fround-db 10.3d0 0)
  division-by-zero)

(deftest fround-db.4a
  (fround-db 10.3d0 100)
  0.0d0 10.3d0)

(deftest fround-db.4b
  (fround-db -10.3d0 100)
  -0.0d0 -10.3d0)

(deftest fround-db.4c
  (fround-db 10.3d0 -100)
  -0.0d0 10.3d0)

(deftest fround-db.4d
  (fround-db -10.3d0 -100)
  0.0d0 -10.3d0)

(deftest fround-db.5a
  (fround-check-db 12.3d0 3    4.0d0 0.3d0)
  t)

(deftest fround-db.5b
  (fround-check-db -12.3d0 3    -4.0d0 -0.3d0)
  t)

(deftest fround-db.5c
  (fround-check-db 12.3d0 -3    -4.0d0 0.3d0)
  t)

(deftest fround-db.5d
  (fround-check-db -12.3d0 -3    4.0d0 -0.3d0)
  t)

(deftest fround-db.6a
  (fround-check-db 14.3d0 3    5.0d0 -0.7d0)
  t)

(deftest fround-db.6b
  (fround-check-db -14.3d0 3    -5.0d0 0.7d0)
  t)

(deftest fround-db.6c
  (fround-check-db 14.3d0 -3    -5.0d0 -0.7d0)
  t)

(deftest fround-db.6d
  (fround-check-db -14.3d0 -3    5.0d0 0.7d0)
  t)


;;  double-float - ratio
(defun fround-check-dr (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest fround-dr.1
  (fround 0.0d0 10/3)
  0.0d0 0.0d0)

(deftest fround-dr.2
  (fround 0.0d0 -10/3)
  0.0d0 0.0d0)

(deftest-error fround-dr.3
  (fround 10.3d0 (make-ratio 0 1))
  division-by-zero)

(deftest fround-dr.4a
  (fround 10.3d0 100/3)
  0.0d0 10.3d0)

(deftest fround-dr.4b
  (fround -10.3d0 100/3)
  -0.0d0 -10.3d0)

(deftest fround-dr.4c
  (fround 10.3d0 -100/3)
  -0.0d0 10.3d0)

(deftest fround-dr.4d
  (fround -10.3d0 -100/3)
  0.0d0 -10.3d0)

(deftest fround-dr.5a
  (fround-check-dr 12.3d0 2/3    18.0d0 0.3d0)
  t)

(deftest fround-dr.5b
  (fround-check-dr -12.3d0 2/3    -18.0d0 -0.3d0)
  t)

(deftest fround-dr.5c
  (fround-check-dr 12.3d0 -2/3    -18.0d0 0.3d0)
  t)

(deftest fround-dr.5d
  (fround-check-dr -12.3d0 -2/3    18.0d0 -0.3d0)
  t)

(deftest fround-dr.6a
  (fround-check-dr 5.2d0 3/4   7.0d0 -0.05d0)
  t)

(deftest fround-dr.6b
  (fround-check-dr -5.2d0 3/4    -7.0d0 0.05d0)
  t)

(deftest fround-dr.6c
  (fround-check-dr 5.2d0 -3/4    -7.0d0 -0.05d0)
  t)

(deftest fround-dr.6d
  (fround-check-dr -5.2d0 -3/4    7.0d0 0.05d0)
  t)


;;  double-float - single-float
(defun fround-check-ds (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'double-float))

(deftest fround-ds.1
  (fround 0.0d0 10.3f0)
  0.0d0 0.0d0)

(deftest fround-ds.2
  (fround 0.0d0 10.3f0)
  0.0d0 0.0d0)

(deftest-error fround-ds.3
  (fround 10.3d0 0.0f0)
  division-by-zero)

(deftest fround-ds.4a
  (fround-check-ds 10.3d0 100.0f0    0.0d0 10.3d0)
  t)

(deftest fround-ds.4b
  (fround-check-ds -10.3d0 100.0f0    0.0d0 -10.3d0)
  t)

(deftest fround-ds.4c
  (fround-check-ds 10.3d0 -100.0f0    0.0d0 10.3d0)
  t)

(deftest fround-ds.4d
  (fround-check-ds -10.3d0 -100.0f0    0.0d0 -10.3d0)
  t)

(deftest fround-ds.5a
  (fround-check-ds 12.3d0 1.5f0    8.0d0 0.3d0)
  t)

(deftest fround-ds.5b
  (fround-check-ds -12.3d0 1.5f0    -8.0d0 -0.3d0)
  t)

(deftest fround-ds.5c
  (fround-check-ds 12.3d0 -1.5f0    -8.0d0 0.3d0)
  t)

(deftest fround-ds.5d
  (fround-check-ds -12.3d0 -1.5f0    8.0d0 -0.3d0)
  t)

(deftest fround-ds.6a
  (fround-check-ds 12.3d0 4.5f0    3.0d0 -1.2d0)
  t)

(deftest fround-ds.6b
  (fround-check-ds -12.3d0 4.5f0    -3.0d0 1.2d0)
  t)

(deftest fround-ds.6c
  (fround-check-ds 12.3d0 -4.5f0    -3.0d0 -1.2d0)
  t)

(deftest fround-ds.6d
  (fround-check-ds -12.3d0 -4.5f0    3.0d0 1.2d0)
  t)


;;  double-float - double-float
(defun fround-check-dd (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'double-float))

(deftest fround-dd.1
  (fround 0.0d0 10.3d0)
  0.0d0 0.0d0)

(deftest fround-dd.2
  (fround 0.0d0 10.3d0)
  0.0d0 0.0d0)

(deftest-error fround-dd.3
  (fround 10.3d0 0.0d0)
  division-by-zero)

(deftest fround-dd.4a
  (fround-check-dd 10.3d0 100.0d0    0.0d0 10.3d0)
  t)

(deftest fround-dd.4b
  (fround-check-dd -10.3d0 100.0d0    0.0d0 -10.3d0)
  t)

(deftest fround-dd.4c
  (fround-check-dd 10.3d0 -100.0d0    0.0d0 10.3d0)
  t)

(deftest fround-dd.4d
  (fround-check-dd -10.3d0 -100.0d0    0.0d0 -10.3d0)
  t)

(deftest fround-dd.5a
  (fround-check-dd 12.3d0 1.5d0    8.0d0 0.3d0)
  t)

(deftest fround-dd.5b
  (fround-check-dd -12.3d0 1.5d0    -8.0d0 -0.3d0)
  t)

(deftest fround-dd.5c
  (fround-check-dd 12.3d0 -1.5d0    -8.0d0 0.3d0)
  t)

(deftest fround-dd.5d
  (fround-check-dd -12.3d0 -1.5d0    8.0d0 -0.3d0)
  t)

(deftest fround-dd.6a
  (fround-check-dd 12.3d0 4.5d0    3.0d0 -1.2d0)
  t)

(deftest fround-dd.6b
  (fround-check-dd -12.3d0 4.5d0    -3.0d0 1.2d0)
  t)

(deftest fround-dd.6c
  (fround-check-dd 12.3d0 -4.5d0    -3.0d0 -1.2d0)
  t)

(deftest fround-dd.6d
  (fround-check-dd -12.3d0 -4.5d0    3.0d0 1.2d0)
  t)


;;  double-float - long-float
(defun fround-check-dl (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fround-dl.1
  (fround 0.0d0 10.3l0)
  0.0l0 0.0l0)

(deftest fround-dl.2
  (fround 0.0d0 10.3l0)
  0.0l0 0.0l0)

(deftest-error fround-dl.3
  (fround 10.3d0 0.0l0)
  division-by-zero)

(deftest fround-dl.4a
  (fround-check-dl 10.3d0 100.0l0    0.0l0 10.3l0)
  t)

(deftest fround-dl.4b
  (fround-check-dl -10.3d0 100.0l0    0.0l0 -10.3l0)
  t)

(deftest fround-dl.4c
  (fround-check-dl 10.3d0 -100.0l0    0.0l0 10.3l0)
  t)

(deftest fround-dl.4d
  (fround-check-dl -10.3d0 -100.0l0    0.0l0 -10.3l0)
  t)

(deftest fround-dl.5a
  (fround-check-dl 12.3d0 1.5l0    8.0l0 0.3l0)
  t)

(deftest fround-dl.5b
  (fround-check-dl -12.3d0 1.5l0    -8.0l0 -0.3l0)
  t)

(deftest fround-dl.5c
  (fround-check-dl 12.3d0 -1.5l0    -8.0l0 0.3l0)
  t)

(deftest fround-dl.5d
  (fround-check-dl -12.3d0 -1.5l0    8.0l0 -0.3l0)
  t)

(deftest fround-dl.6a
  (fround-check-dl 12.3d0 4.5l0    3.0l0 -1.2l0)
  t)

(deftest fround-dl.6b
  (fround-check-dl -12.3d0 4.5l0    -3.0l0 1.2l0)
  t)

(deftest fround-dl.6c
  (fround-check-dl 12.3d0 -4.5l0    -3.0l0 -1.2l0)
  t)

(deftest fround-dl.6d
  (fround-check-dl -12.3d0 -4.5l0    3.0l0 1.2l0)
  t)


;;  long-float - fixnum
(defun fround-check-lf (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest fround-lf.1
  (fround 0.0l0 10)
  0.0l0 0.0l0)

(deftest fround-lf.2
  (fround 0.0l0 -10)
  0.0l0 0.0l0)

(deftest-error fround-lf.3
  (fround 10.3l0 0)
  division-by-zero)

(deftest fround-lf.4a
  (fround 10.3l0 100)
  0.0l0 10.3l0)

(deftest fround-lf.4b
  (fround -10.3l0 100)
  -0.0l0 -10.3l0)

(deftest fround-lf.4c
  (fround 10.3l0 -100)
  -0.0l0 10.3l0)

(deftest fround-lf.4d
  (fround -10.3l0 -100)
  0.0l0 -10.3l0)

(deftest fround-lf.5a
  (fround-check-lf 12.3l0 3    4.0l0 0.3l0)
  t)

(deftest fround-lf.5b
  (fround-check-lf -12.3l0 3    -4.0l0 -0.3l0)
  t)

(deftest fround-lf.5c
  (fround-check-lf 12.3l0 -3    -4.0l0 0.3l0)
  t)

(deftest fround-lf.5d
  (fround-check-lf -12.3l0 -3    4.0l0 -0.3l0)
  t)

(deftest fround-lf.6a
  (fround-check-lf 14.3l0 3    5.0l0 -0.7l0)
  t)

(deftest fround-lf.6b
  (fround-check-lf -14.3l0 3    -5.0l0 0.7l0)
  t)

(deftest fround-lf.6c
  (fround-check-lf 14.3l0 -3    -5.0l0 -0.7l0)
  t)

(deftest fround-lf.6d
  (fround-check-lf -14.3l0 -3    5.0l0 0.7l0)
  t)


;;  long-float - bignum
(defun fround-lb (a b)
  (fround a (make-bignum b)))

(defun fround-check-lb (a b c d)
  (fround-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :type 'long-float))

(deftest fround-lb.1
  (fround-lb 0.0l0 10)
  0.0l0 0.0l0)

(deftest fround-lb.2
  (fround-lb 0.0l0 -10)
  0.0l0 0.0l0)

(deftest-error fround-lb.3
  (fround-lb 10.3l0 0)
  division-by-zero)

(deftest fround-lb.4a
  (fround-lb 10.3l0 100)
  0.0l0 10.3l0)

(deftest fround-lb.4b
  (fround-lb -10.3l0 100)
  -0.0l0 -10.3l0)

(deftest fround-lb.4c
  (fround-lb 10.3l0 -100)
  -0.0l0 10.3l0)

(deftest fround-lb.4d
  (fround-lb -10.3l0 -100)
  0.0l0 -10.3l0)

(deftest fround-lb.5a
  (fround-check-lb 12.3l0 3    4.0l0 0.3l0)
  t)

(deftest fround-lb.5b
  (fround-check-lb -12.3l0 3    -4.0l0 -0.3l0)
  t)

(deftest fround-lb.5c
  (fround-check-lb 12.3l0 -3    -4.0l0 0.3l0)
  t)

(deftest fround-lb.5d
  (fround-check-lb -12.3l0 -3    4.0l0 -0.3l0)
  t)

(deftest fround-lb.6a
  (fround-check-lb 14.3l0 3    5.0l0 -0.7l0)
  t)

(deftest fround-lb.6b
  (fround-check-lb -14.3l0 3    -5.0l0 0.7l0)
  t)

(deftest fround-lb.6c
  (fround-check-lb 14.3l0 -3    -5.0l0 -0.7l0)
  t)

(deftest fround-lb.6d
  (fround-check-lb -14.3l0 -3    5.0l0 0.7l0)
  t)


;;  long-float - ratio
(defun fround-check-lr (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest fround-lr.1
  (fround 0.0l0 10/3)
  0.0l0 0.0l0)

(deftest fround-lr.2
  (fround 0.0l0 -10/3)
  0.0l0 0.0l0)

(deftest-error fround-lr.3
  (fround 10.3l0 (make-ratio 0 1))
  division-by-zero)

(deftest fround-lr.4a
  (fround 10.3l0 100/3)
  0.0l0 10.3l0)

(deftest fround-lr.4b
  (fround -10.3l0 100/3)
  -0.0l0 -10.3l0)

(deftest fround-lr.4c
  (fround 10.3l0 -100/3)
  -0.0l0 10.3l0)

(deftest fround-lr.4d
  (fround -10.3l0 -100/3)
  0.0l0 -10.3l0)

(deftest fround-lr.5a
  (fround-check-lr 12.3l0 2/3    18.0l0 0.3l0)
  t)

(deftest fround-lr.5b
  (fround-check-lr -12.3l0 2/3    -18.0l0 -0.3l0)
  t)

(deftest fround-lr.5c
  (fround-check-lr 12.3l0 -2/3    -18.0l0 0.3l0)
  t)

(deftest fround-lr.5d
  (fround-check-lr -12.3l0 -2/3    18.0l0 -0.3l0)
  t)

(deftest fround-lr.6a
  (fround-check-lr 5.2l0 3/4   7.0l0 -0.05l0)
  t)

(deftest fround-lr.6b
  (fround-check-lr -5.2l0 3/4    -7.0l0 0.05l0)
  t)

(deftest fround-lr.6c
  (fround-check-lr 5.2l0 -3/4    -7.0l0 -0.05l0)
  t)

(deftest fround-lr.6d
  (fround-check-lr -5.2l0 -3/4    7.0l0 0.05l0)
  t)


;;  long-float - single-float
(defun fround-check-ls (a b c d)
  (fround-equal
    a b c d
    :eps 1.0e-6
    :type 'long-float))

(deftest fround-ls.1
  (fround 0.0l0 10.3f0)
  0.0l0 0.0l0)

(deftest fround-ls.2
  (fround 0.0l0 10.3f0)
  0.0l0 0.0l0)

(deftest-error fround-ls.3
  (fround 10.3l0 0.0f0)
  division-by-zero)

(deftest fround-ls.4a
  (fround-check-ls 10.3l0 100.0f0    0.0l0 10.3l0)
  t)

(deftest fround-ls.4b
  (fround-check-ls -10.3l0 100.0f0    0.0l0 -10.3l0)
  t)

(deftest fround-ls.4c
  (fround-check-ls 10.3l0 -100.0f0    0.0l0 10.3l0)
  t)

(deftest fround-ls.4d
  (fround-check-ls -10.3l0 -100.0f0    0.0l0 -10.3l0)
  t)

(deftest fround-ls.5a
  (fround-check-ls 12.3l0 1.5f0    8.0l0 0.3l0)
  t)

(deftest fround-ls.5b
  (fround-check-ls -12.3l0 1.5f0    -8.0l0 -0.3l0)
  t)

(deftest fround-ls.5c
  (fround-check-ls 12.3l0 -1.5f0    -8.0l0 0.3l0)
  t)

(deftest fround-ls.5d
  (fround-check-ls -12.3l0 -1.5f0    8.0l0 -0.3l0)
  t)

(deftest fround-ls.6a
  (fround-check-ls 12.3l0 4.5f0    3.0l0 -1.2l0)
  t)

(deftest fround-ls.6b
  (fround-check-ls -12.3l0 4.5f0    -3.0l0 1.2l0)
  t)

(deftest fround-ls.6c
  (fround-check-ls 12.3l0 -4.5f0    -3.0l0 -1.2l0)
  t)

(deftest fround-ls.6d
  (fround-check-ls -12.3l0 -4.5f0    3.0l0 1.2l0)
  t)


;;  long-float - double-float
(defun fround-check-ld (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fround-ld.1
  (fround 0.0l0 10.3d0)
  0.0l0 0.0l0)

(deftest fround-ld.2
  (fround 0.0l0 10.3d0)
  0.0l0 0.0l0)

(deftest-error fround-ld.3
  (fround 10.3l0 0.0d0)
  division-by-zero)

(deftest fround-ld.4a
  (fround-check-ld 10.3l0 100.0d0    0.0l0 10.3l0)
  t)

(deftest fround-ld.4b
  (fround-check-ld -10.3l0 100.0d0    0.0l0 -10.3l0)
  t)

(deftest fround-ld.4c
  (fround-check-ld 10.3l0 -100.0d0    0.0l0 10.3l0)
  t)

(deftest fround-ld.4d
  (fround-check-ld -10.3l0 -100.0d0    0.0l0 -10.3l0)
  t)

(deftest fround-ld.5a
  (fround-check-ld 12.3l0 1.5d0    8.0l0 0.3l0)
  t)

(deftest fround-ld.5b
  (fround-check-ld -12.3l0 1.5d0    -8.0l0 -0.3l0)
  t)

(deftest fround-ld.5c
  (fround-check-ld 12.3l0 -1.5d0    -8.0l0 0.3l0)
  t)

(deftest fround-ld.5d
  (fround-check-ld -12.3l0 -1.5d0    8.0l0 -0.3l0)
  t)

(deftest fround-ld.6a
  (fround-check-ld 12.3l0 4.5d0    3.0l0 -1.2l0)
  t)

(deftest fround-ld.6b
  (fround-check-ld -12.3l0 4.5d0    -3.0l0 1.2l0)
  t)

(deftest fround-ld.6c
  (fround-check-ld 12.3l0 -4.5d0    -3.0l0 -1.2l0)
  t)

(deftest fround-ld.6d
  (fround-check-ld -12.3l0 -4.5d0    3.0l0 1.2l0)
  t)


;;  long-float - long-float
(defun fround-check-ll (a b c d)
  (fround-equal
    a b c d
    :eps 1.0d-14
    :type 'long-float))

(deftest fround-ll.1
  (fround 0.0l0 10.3l0)
  0.0l0 0.0l0)

(deftest fround-ll.2
  (fround 0.0l0 10.3l0)
  0.0l0 0.0l0)

(deftest-error fround-ll.3
  (fround 10.3l0 0.0l0)
  division-by-zero)

(deftest fround-ll.4a
  (fround-check-ll 10.3l0 100.0l0    0.0l0 10.3l0)
  t)

(deftest fround-ll.4b
  (fround-check-ll -10.3l0 100.0l0    0.0l0 -10.3l0)
  t)

(deftest fround-ll.4c
  (fround-check-ll 10.3l0 -100.0l0    0.0l0 10.3l0)
  t)

(deftest fround-ll.4d
  (fround-check-ll -10.3l0 -100.0l0    0.0l0 -10.3l0)
  t)

(deftest fround-ll.5a
  (fround-check-ll 12.3l0 1.5l0    8.0l0 0.3l0)
  t)

(deftest fround-ll.5b
  (fround-check-ll -12.3l0 1.5l0    -8.0l0 -0.3l0)
  t)

(deftest fround-ll.5c
  (fround-check-ll 12.3l0 -1.5l0    -8.0l0 0.3l0)
  t)

(deftest fround-ll.5d
  (fround-check-ll -12.3l0 -1.5l0    8.0l0 -0.3l0)
  t)

(deftest fround-ll.6a
  (fround-check-ll 12.3l0 4.5l0    3.0l0 -1.2l0)
  t)

(deftest fround-ll.6b
  (fround-check-ll -12.3l0 4.5l0    -3.0l0 1.2l0)
  t)

(deftest fround-ll.6c
  (fround-check-ll 12.3l0 -4.5l0    -3.0l0 -1.2l0)
  t)

(deftest fround-ll.6d
  (fround-check-ll -12.3l0 -4.5l0    3.0l0 1.2l0)
  t)

