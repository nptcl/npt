;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  round
;;

;;  single-float - fixnum
(defun round-check-sf (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-sf.1
  (round 0.0f0 10)
  0 0.0f0)

(deftest round-sf.2
  (round 0.0f0 -10)
  0 0.0f0)

(deftest-error round-sf.3
  (round 10.3f0 0)
  division-by-zero)

(deftest round-sf.4a
  (round 10.3f0 100)
  0 10.3f0)

(deftest round-sf.4b
  (round -10.3f0 100)
  0 -10.3f0)

(deftest round-sf.4c
  (round 10.3f0 -100)
  0 10.3f0)

(deftest round-sf.4d
  (round -10.3f0 -100)
  0 -10.3f0)

(deftest round-sf.5a
  (round-check-sf 12.3f0 3    4 0.3f0)
  t)

(deftest round-sf.5b
  (round-check-sf -12.3f0 3    -4 -0.3f0)
  t)

(deftest round-sf.5c
  (round-check-sf 12.3f0 -3    -4 0.3f0)
  t)

(deftest round-sf.5d
  (round-check-sf -12.3f0 -3    4 -0.3f0)
  t)

(deftest round-sf.6a
  (round-check-sf 14.3f0 3    5 -0.7f0)
  t)

(deftest round-sf.6b
  (round-check-sf -14.3f0 3    -5 0.7f0)
  t)

(deftest round-sf.6c
  (round-check-sf 14.3f0 -3    -5 -0.7f0)
  t)

(deftest round-sf.6d
  (round-check-sf -14.3f0 -3    5 0.7f0)
  t)


;;  single-float - bignum
(defun round-sb (a b)
  (round a (make-bignum b)))

(defun round-check-sb (a b c d)
  (round-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-sb.1
  (round-sb 0.0f0 10)
  0 0.0f0)

(deftest round-sb.2
  (round-sb 0.0f0 -10)
  0 0.0f0)

(deftest-error round-sb.3
  (round-sb 10.3f0 0)
  division-by-zero)

(deftest round-sb.4a
  (round-sb 10.3f0 100)
  0 10.3f0)

(deftest round-sb.4b
  (round-sb -10.3f0 100)
  0 -10.3f0)

(deftest round-sb.4c
  (round-sb 10.3f0 -100)
  0 10.3f0)

(deftest round-sb.4d
  (round-sb -10.3f0 -100)
  0 -10.3f0)

(deftest round-sb.5a
  (round-check-sb 12.3f0 3    4 0.3f0)
  t)

(deftest round-sb.5b
  (round-check-sb -12.3f0 3    -4 -0.3f0)
  t)

(deftest round-sb.5c
  (round-check-sb 12.3f0 -3    -4 0.3f0)
  t)

(deftest round-sb.5d
  (round-check-sb -12.3f0 -3    4 -0.3f0)
  t)

(deftest round-sb.6a
  (round-check-sb 14.3f0 3    5 -0.7f0)
  t)

(deftest round-sb.6b
  (round-check-sb -14.3f0 3    -5 0.7f0)
  t)

(deftest round-sb.6c
  (round-check-sb 14.3f0 -3    -5 -0.7f0)
  t)

(deftest round-sb.6d
  (round-check-sb -14.3f0 -3    5 0.7f0)
  t)


;;  single-float - ratio
(defun round-check-sr (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-sr.1
  (round 0.0f0 10/3)
  0 0.0f0)

(deftest round-sr.2
  (round 0.0f0 -10/3)
  0 0.0f0)

(deftest-error round-sr.3
  (round 10.3f0 (make-ratio 0 1))
  division-by-zero)

(deftest round-sr.4a
  (round 10.3f0 100/3)
  0 10.3f0)

(deftest round-sr.4b
  (round -10.3f0 100/3)
  0 -10.3f0)

(deftest round-sr.4c
  (round 10.3f0 -100/3)
  0 10.3f0)

(deftest round-sr.4d
  (round -10.3f0 -100/3)
  0 -10.3f0)

(deftest round-sr.5a
  (round-check-sr 12.3f0 2/3    18 0.3f0)
  t)

(deftest round-sr.5b
  (round-check-sr -12.3f0 2/3    -18 -0.3f0)
  t)

(deftest round-sr.5c
  (round-check-sr 12.3f0 -2/3    -18 0.3f0)
  t)

(deftest round-sr.5d
  (round-check-sr -12.3f0 -2/3    18 -0.3f0)
  t)

(deftest round-sr.6a
  (round-check-sr 5.2f0 3/4   7 -0.05f0)
  t)

(deftest round-sr.6b
  (round-check-sr -5.2f0 3/4    -7 0.05f0)
  t)

(deftest round-sr.6c
  (round-check-sr 5.2f0 -3/4    -7 -0.05f0)
  t)

(deftest round-sr.6d
  (round-check-sr -5.2f0 -3/4    7 0.05f0)
  t)


;;  single-float - single-float
(defun round-check-ss (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'single-float))

(deftest round-ss.1
  (round 0.0f0 10.3f0)
  0 0.0f0)

(deftest round-ss.2
  (round 0.0f0 10.3f0)
  0 0.0f0)

(deftest-error round-ss.3
  (round 10.3f0 0.0f0)
  division-by-zero)

(deftest round-ss.4a
  (round-check-ss 10.3f0 100.0f0    0 10.3f0)
  t)

(deftest round-ss.4b
  (round-check-ss -10.3f0 100.0f0    0 -10.3f0)
  t)

(deftest round-ss.4c
  (round-check-ss 10.3f0 -100.0f0    0 10.3f0)
  t)

(deftest round-ss.4d
  (round-check-ss -10.3f0 -100.0f0    0 -10.3f0)
  t)

(deftest round-ss.5a
  (round-check-ss 12.3f0 1.5f0    8 0.3f0)
  t)

(deftest round-ss.5b
  (round-check-ss -12.3f0 1.5f0    -8 -0.3f0)
  t)

(deftest round-ss.5c
  (round-check-ss 12.3f0 -1.5f0    -8 0.3f0)
  t)

(deftest round-ss.5d
  (round-check-ss -12.3f0 -1.5f0    8 -0.3f0)
  t)

(deftest round-ss.6a
  (round-check-ss 12.3f0 4.5f0    3 -1.2f0)
  t)

(deftest round-ss.6b
  (round-check-ss -12.3f0 4.5f0    -3 1.2f0)
  t)

(deftest round-ss.6c
  (round-check-ss 12.3f0 -4.5f0    -3 -1.2f0)
  t)

(deftest round-ss.6d
  (round-check-ss -12.3f0 -4.5f0    3 1.2f0)
  t)


;;  single-float - double-float
(defun round-check-sd (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'double-float))

(deftest round-sd.1
  (round 0.0f0 10.3d0)
  0 0.0d0)

(deftest round-sd.2
  (round 0.0f0 10.3d0)
  0 0.0d0)

(deftest-error round-sd.3
  (round 10.3f0 0.0d0)
  division-by-zero)

(deftest round-sd.4a
  (round-check-sd 10.3f0 100.0d0    0 10.3d0)
  t)

(deftest round-sd.4b
  (round-check-sd -10.3f0 100.0d0    0 -10.3d0)
  t)

(deftest round-sd.4c
  (round-check-sd 10.3f0 -100.0d0    0 10.3d0)
  t)

(deftest round-sd.4d
  (round-check-sd -10.3f0 -100.0d0    0 -10.3d0)
  t)

(deftest round-sd.5a
  (round-check-sd 12.3f0 1.5d0    8 0.3d0)
  t)

(deftest round-sd.5b
  (round-check-sd -12.3f0 1.5d0    -8 -0.3d0)
  t)

(deftest round-sd.5c
  (round-check-sd 12.3f0 -1.5d0    -8 0.3d0)
  t)

(deftest round-sd.5d
  (round-check-sd -12.3f0 -1.5d0    8 -0.3d0)
  t)

(deftest round-sd.6a
  (round-check-sd 12.3f0 4.5d0    3 -1.2d0)
  t)

(deftest round-sd.6b
  (round-check-sd -12.3f0 4.5d0    -3 1.2d0)
  t)

(deftest round-sd.6c
  (round-check-sd 12.3f0 -4.5d0    -3 -1.2d0)
  t)

(deftest round-sd.6d
  (round-check-sd -12.3f0 -4.5d0    3 1.2d0)
  t)


;;  single-float - long-float
(defun round-check-sl (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'long-float))

(deftest round-sl.1
  (round 0.0f0 10.3l0)
  0 0.0l0)

(deftest round-sl.2
  (round 0.0f0 10.3l0)
  0 0.0l0)

(deftest-error round-sl.3
  (round 10.3f0 0.0l0)
  division-by-zero)

(deftest round-sl.4a
  (round-check-sl 10.3f0 100.0l0    0 10.3l0)
  t)

(deftest round-sl.4b
  (round-check-sl -10.3f0 100.0l0    0 -10.3l0)
  t)

(deftest round-sl.4c
  (round-check-sl 10.3f0 -100.0l0    0 10.3l0)
  t)

(deftest round-sl.4d
  (round-check-sl -10.3f0 -100.0l0    0 -10.3l0)
  t)

(deftest round-sl.5a
  (round-check-sl 12.3f0 1.5l0    8 0.3l0)
  t)

(deftest round-sl.5b
  (round-check-sl -12.3f0 1.5l0    -8 -0.3l0)
  t)

(deftest round-sl.5c
  (round-check-sl 12.3f0 -1.5l0    -8 0.3l0)
  t)

(deftest round-sl.5d
  (round-check-sl -12.3f0 -1.5l0    8 -0.3l0)
  t)

(deftest round-sl.6a
  (round-check-sl 12.3f0 4.5l0    3 -1.2l0)
  t)

(deftest round-sl.6b
  (round-check-sl -12.3f0 4.5l0    -3 1.2l0)
  t)

(deftest round-sl.6c
  (round-check-sl 12.3f0 -4.5l0    -3 -1.2l0)
  t)

(deftest round-sl.6d
  (round-check-sl -12.3f0 -4.5l0    3 1.2l0)
  t)


;;  double-float - fixnum
(defun round-check-df (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'double-float))

(deftest round-df.1
  (round 0.0d0 10)
  0 0.0d0)

(deftest round-df.2
  (round 0.0d0 -10)
  0 0.0d0)

(deftest-error round-df.3
  (round 10.3d0 0)
  division-by-zero)

(deftest round-df.4a
  (round 10.3d0 100)
  0 10.3d0)

(deftest round-df.4b
  (round -10.3d0 100)
  0 -10.3d0)

(deftest round-df.4c
  (round 10.3d0 -100)
  0 10.3d0)

(deftest round-df.4d
  (round -10.3d0 -100)
  0 -10.3d0)

(deftest round-df.5a
  (round-check-df 12.3d0 3    4 0.3d0)
  t)

(deftest round-df.5b
  (round-check-df -12.3d0 3    -4 -0.3d0)
  t)

(deftest round-df.5c
  (round-check-df 12.3d0 -3    -4 0.3d0)
  t)

(deftest round-df.5d
  (round-check-df -12.3d0 -3    4 -0.3d0)
  t)

(deftest round-df.6a
  (round-check-df 14.3d0 3    5 -0.7d0)
  t)

(deftest round-df.6b
  (round-check-df -14.3d0 3    -5 0.7d0)
  t)

(deftest round-df.6c
  (round-check-df 14.3d0 -3    -5 -0.7d0)
  t)

(deftest round-df.6d
  (round-check-df -14.3d0 -3    5 0.7d0)
  t)


;;  double-float - bignum
(defun round-db (a b)
  (round a (make-bignum b)))

(defun round-check-db (a b c d)
  (round-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'double-float))

(deftest round-db.1
  (round-db 0.0d0 10)
  0 0.0d0)

(deftest round-db.2
  (round-db 0.0d0 -10)
  0 0.0d0)

(deftest-error round-db.3
  (round-db 10.3d0 0)
  division-by-zero)

(deftest round-db.4a
  (round-db 10.3d0 100)
  0 10.3d0)

(deftest round-db.4b
  (round-db -10.3d0 100)
  0 -10.3d0)

(deftest round-db.4c
  (round-db 10.3d0 -100)
  0 10.3d0)

(deftest round-db.4d
  (round-db -10.3d0 -100)
  0 -10.3d0)

(deftest round-db.5a
  (round-check-db 12.3d0 3    4 0.3d0)
  t)

(deftest round-db.5b
  (round-check-db -12.3d0 3    -4 -0.3d0)
  t)

(deftest round-db.5c
  (round-check-db 12.3d0 -3    -4 0.3d0)
  t)

(deftest round-db.5d
  (round-check-db -12.3d0 -3    4 -0.3d0)
  t)

(deftest round-db.6a
  (round-check-db 14.3d0 3    5 -0.7d0)
  t)

(deftest round-db.6b
  (round-check-db -14.3d0 3    -5 0.7d0)
  t)

(deftest round-db.6c
  (round-check-db 14.3d0 -3    -5 -0.7d0)
  t)

(deftest round-db.6d
  (round-check-db -14.3d0 -3    5 0.7d0)
  t)


;;  double-float - ratio
(defun round-check-dr (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'double-float))

(deftest round-dr.1
  (round 0.0d0 10/3)
  0 0.0d0)

(deftest round-dr.2
  (round 0.0d0 -10/3)
  0 0.0d0)

(deftest-error round-dr.3
  (round 10.3d0 (make-ratio 0 1))
  division-by-zero)

(deftest round-dr.4a
  (round 10.3d0 100/3)
  0 10.3d0)

(deftest round-dr.4b
  (round -10.3d0 100/3)
  0 -10.3d0)

(deftest round-dr.4c
  (round 10.3d0 -100/3)
  0 10.3d0)

(deftest round-dr.4d
  (round -10.3d0 -100/3)
  0 -10.3d0)

(deftest round-dr.5a
  (round-check-dr 12.3d0 2/3    18 0.3d0)
  t)

(deftest round-dr.5b
  (round-check-dr -12.3d0 2/3    -18 -0.3d0)
  t)

(deftest round-dr.5c
  (round-check-dr 12.3d0 -2/3    -18 0.3d0)
  t)

(deftest round-dr.5d
  (round-check-dr -12.3d0 -2/3    18 -0.3d0)
  t)

(deftest round-dr.6a
  (round-check-dr 5.2d0 3/4   7 -0.05d0)
  t)

(deftest round-dr.6b
  (round-check-dr -5.2d0 3/4    -7 0.05d0)
  t)

(deftest round-dr.6c
  (round-check-dr 5.2d0 -3/4    -7 -0.05d0)
  t)

(deftest round-dr.6d
  (round-check-dr -5.2d0 -3/4    7 0.05d0)
  t)


;;  double-float - single-float
(defun round-check-ds (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'double-float))

(deftest round-ds.1
  (round 0.0d0 10.3f0)
  0 0.0d0)

(deftest round-ds.2
  (round 0.0d0 10.3f0)
  0 0.0d0)

(deftest-error round-ds.3
  (round 10.3d0 0.0f0)
  division-by-zero)

(deftest round-ds.4a
  (round-check-ds 10.3d0 100.0f0    0 10.3d0)
  t)

(deftest round-ds.4b
  (round-check-ds -10.3d0 100.0f0    0 -10.3d0)
  t)

(deftest round-ds.4c
  (round-check-ds 10.3d0 -100.0f0    0 10.3d0)
  t)

(deftest round-ds.4d
  (round-check-ds -10.3d0 -100.0f0    0 -10.3d0)
  t)

(deftest round-ds.5a
  (round-check-ds 12.3d0 1.5f0    8 0.3d0)
  t)

(deftest round-ds.5b
  (round-check-ds -12.3d0 1.5f0    -8 -0.3d0)
  t)

(deftest round-ds.5c
  (round-check-ds 12.3d0 -1.5f0    -8 0.3d0)
  t)

(deftest round-ds.5d
  (round-check-ds -12.3d0 -1.5f0    8 -0.3d0)
  t)

(deftest round-ds.6a
  (round-check-ds 12.3d0 4.5f0    3 -1.2d0)
  t)

(deftest round-ds.6b
  (round-check-ds -12.3d0 4.5f0    -3 1.2d0)
  t)

(deftest round-ds.6c
  (round-check-ds 12.3d0 -4.5f0    -3 -1.2d0)
  t)

(deftest round-ds.6d
  (round-check-ds -12.3d0 -4.5f0    3 1.2d0)
  t)


;;  double-float - double-float
(defun round-check-dd (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'double-float))

(deftest round-dd.1
  (round 0.0d0 10.3d0)
  0 0.0d0)

(deftest round-dd.2
  (round 0.0d0 10.3d0)
  0 0.0d0)

(deftest-error round-dd.3
  (round 10.3d0 0.0d0)
  division-by-zero)

(deftest round-dd.4a
  (round-check-dd 10.3d0 100.0d0    0 10.3d0)
  t)

(deftest round-dd.4b
  (round-check-dd -10.3d0 100.0d0    0 -10.3d0)
  t)

(deftest round-dd.4c
  (round-check-dd 10.3d0 -100.0d0    0 10.3d0)
  t)

(deftest round-dd.4d
  (round-check-dd -10.3d0 -100.0d0    0 -10.3d0)
  t)

(deftest round-dd.5a
  (round-check-dd 12.3d0 1.5d0    8 0.3d0)
  t)

(deftest round-dd.5b
  (round-check-dd -12.3d0 1.5d0    -8 -0.3d0)
  t)

(deftest round-dd.5c
  (round-check-dd 12.3d0 -1.5d0    -8 0.3d0)
  t)

(deftest round-dd.5d
  (round-check-dd -12.3d0 -1.5d0    8 -0.3d0)
  t)

(deftest round-dd.6a
  (round-check-dd 12.3d0 4.5d0    3 -1.2d0)
  t)

(deftest round-dd.6b
  (round-check-dd -12.3d0 4.5d0    -3 1.2d0)
  t)

(deftest round-dd.6c
  (round-check-dd 12.3d0 -4.5d0    -3 -1.2d0)
  t)

(deftest round-dd.6d
  (round-check-dd -12.3d0 -4.5d0    3 1.2d0)
  t)


;;  double-float - long-float
(defun round-check-dl (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'long-float))

(deftest round-dl.1
  (round 0.0d0 10.3l0)
  0 0.0l0)

(deftest round-dl.2
  (round 0.0d0 10.3l0)
  0 0.0l0)

(deftest-error round-dl.3
  (round 10.3d0 0.0l0)
  division-by-zero)

(deftest round-dl.4a
  (round-check-dl 10.3d0 100.0l0    0 10.3l0)
  t)

(deftest round-dl.4b
  (round-check-dl -10.3d0 100.0l0    0 -10.3l0)
  t)

(deftest round-dl.4c
  (round-check-dl 10.3d0 -100.0l0    0 10.3l0)
  t)

(deftest round-dl.4d
  (round-check-dl -10.3d0 -100.0l0    0 -10.3l0)
  t)

(deftest round-dl.5a
  (round-check-dl 12.3d0 1.5l0    8 0.3l0)
  t)

(deftest round-dl.5b
  (round-check-dl -12.3d0 1.5l0    -8 -0.3l0)
  t)

(deftest round-dl.5c
  (round-check-dl 12.3d0 -1.5l0    -8 0.3l0)
  t)

(deftest round-dl.5d
  (round-check-dl -12.3d0 -1.5l0    8 -0.3l0)
  t)

(deftest round-dl.6a
  (round-check-dl 12.3d0 4.5l0    3 -1.2l0)
  t)

(deftest round-dl.6b
  (round-check-dl -12.3d0 4.5l0    -3 1.2l0)
  t)

(deftest round-dl.6c
  (round-check-dl 12.3d0 -4.5l0    -3 -1.2l0)
  t)

(deftest round-dl.6d
  (round-check-dl -12.3d0 -4.5l0    3 1.2l0)
  t)


;;  long-float - fixnum
(defun round-check-lf (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'long-float))

(deftest round-lf.1
  (round 0.0l0 10)
  0 0.0l0)

(deftest round-lf.2
  (round 0.0l0 -10)
  0 0.0l0)

(deftest-error round-lf.3
  (round 10.3l0 0)
  division-by-zero)

(deftest round-lf.4a
  (round 10.3l0 100)
  0 10.3l0)

(deftest round-lf.4b
  (round -10.3l0 100)
  0 -10.3l0)

(deftest round-lf.4c
  (round 10.3l0 -100)
  0 10.3l0)

(deftest round-lf.4d
  (round -10.3l0 -100)
  0 -10.3l0)

(deftest round-lf.5a
  (round-check-lf 12.3l0 3    4 0.3l0)
  t)

(deftest round-lf.5b
  (round-check-lf -12.3l0 3    -4 -0.3l0)
  t)

(deftest round-lf.5c
  (round-check-lf 12.3l0 -3    -4 0.3l0)
  t)

(deftest round-lf.5d
  (round-check-lf -12.3l0 -3    4 -0.3l0)
  t)

(deftest round-lf.6a
  (round-check-lf 14.3l0 3    5 -0.7l0)
  t)

(deftest round-lf.6b
  (round-check-lf -14.3l0 3    -5 0.7l0)
  t)

(deftest round-lf.6c
  (round-check-lf 14.3l0 -3    -5 -0.7l0)
  t)

(deftest round-lf.6d
  (round-check-lf -14.3l0 -3    5 0.7l0)
  t)


;;  long-float - bignum
(defun round-lb (a b)
  (round a (make-bignum b)))

(defun round-check-lb (a b c d)
  (round-equal
    a (make-bignum b) c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'long-float))

(deftest round-lb.1
  (round-lb 0.0l0 10)
  0 0.0l0)

(deftest round-lb.2
  (round-lb 0.0l0 -10)
  0 0.0l0)

(deftest-error round-lb.3
  (round-lb 10.3l0 0)
  division-by-zero)

(deftest round-lb.4a
  (round-lb 10.3l0 100)
  0 10.3l0)

(deftest round-lb.4b
  (round-lb -10.3l0 100)
  0 -10.3l0)

(deftest round-lb.4c
  (round-lb 10.3l0 -100)
  0 10.3l0)

(deftest round-lb.4d
  (round-lb -10.3l0 -100)
  0 -10.3l0)

(deftest round-lb.5a
  (round-check-lb 12.3l0 3    4 0.3l0)
  t)

(deftest round-lb.5b
  (round-check-lb -12.3l0 3    -4 -0.3l0)
  t)

(deftest round-lb.5c
  (round-check-lb 12.3l0 -3    -4 0.3l0)
  t)

(deftest round-lb.5d
  (round-check-lb -12.3l0 -3    4 -0.3l0)
  t)

(deftest round-lb.6a
  (round-check-lb 14.3l0 3    5 -0.7l0)
  t)

(deftest round-lb.6b
  (round-check-lb -14.3l0 3    -5 0.7l0)
  t)

(deftest round-lb.6c
  (round-check-lb 14.3l0 -3    -5 -0.7l0)
  t)

(deftest round-lb.6d
  (round-check-lb -14.3l0 -3    5 0.7l0)
  t)


;;  long-float - ratio
(defun round-check-lr (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'long-float))

(deftest round-lr.1
  (round 0.0l0 10/3)
  0 0.0l0)

(deftest round-lr.2
  (round 0.0l0 -10/3)
  0 0.0l0)

(deftest-error round-lr.3
  (round 10.3l0 (make-ratio 0 1))
  division-by-zero)

(deftest round-lr.4a
  (round 10.3l0 100/3)
  0 10.3l0)

(deftest round-lr.4b
  (round -10.3l0 100/3)
  0 -10.3l0)

(deftest round-lr.4c
  (round 10.3l0 -100/3)
  0 10.3l0)

(deftest round-lr.4d
  (round -10.3l0 -100/3)
  0 -10.3l0)

(deftest round-lr.5a
  (round-check-lr 12.3l0 2/3    18 0.3l0)
  t)

(deftest round-lr.5b
  (round-check-lr -12.3l0 2/3    -18 -0.3l0)
  t)

(deftest round-lr.5c
  (round-check-lr 12.3l0 -2/3    -18 0.3l0)
  t)

(deftest round-lr.5d
  (round-check-lr -12.3l0 -2/3    18 -0.3l0)
  t)

(deftest round-lr.6a
  (round-check-lr 5.2l0 3/4   7 -0.05l0)
  t)

(deftest round-lr.6b
  (round-check-lr -5.2l0 3/4    -7 0.05l0)
  t)

(deftest round-lr.6c
  (round-check-lr 5.2l0 -3/4    -7 -0.05l0)
  t)

(deftest round-lr.6d
  (round-check-lr -5.2l0 -3/4    7 0.05l0)
  t)


;;  long-float - single-float
(defun round-check-ls (a b c d)
  (round-equal
    a b c d
    :eps 1.0e-6
    :call #'fixnump
    :type 'long-float))

(deftest round-ls.1
  (round 0.0l0 10.3f0)
  0 0.0l0)

(deftest round-ls.2
  (round 0.0l0 10.3f0)
  0 0.0l0)

(deftest-error round-ls.3
  (round 10.3l0 0.0f0)
  division-by-zero)

(deftest round-ls.4a
  (round-check-ls 10.3l0 100.0f0    0 10.3l0)
  t)

(deftest round-ls.4b
  (round-check-ls -10.3l0 100.0f0    0 -10.3l0)
  t)

(deftest round-ls.4c
  (round-check-ls 10.3l0 -100.0f0    0 10.3l0)
  t)

(deftest round-ls.4d
  (round-check-ls -10.3l0 -100.0f0    0 -10.3l0)
  t)

(deftest round-ls.5a
  (round-check-ls 12.3l0 1.5f0    8 0.3l0)
  t)

(deftest round-ls.5b
  (round-check-ls -12.3l0 1.5f0    -8 -0.3l0)
  t)

(deftest round-ls.5c
  (round-check-ls 12.3l0 -1.5f0    -8 0.3l0)
  t)

(deftest round-ls.5d
  (round-check-ls -12.3l0 -1.5f0    8 -0.3l0)
  t)

(deftest round-ls.6a
  (round-check-ls 12.3l0 4.5f0    3 -1.2l0)
  t)

(deftest round-ls.6b
  (round-check-ls -12.3l0 4.5f0    -3 1.2l0)
  t)

(deftest round-ls.6c
  (round-check-ls 12.3l0 -4.5f0    -3 -1.2l0)
  t)

(deftest round-ls.6d
  (round-check-ls -12.3l0 -4.5f0    3 1.2l0)
  t)


;;  long-float - double-float
(defun round-check-ld (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'long-float))

(deftest round-ld.1
  (round 0.0l0 10.3d0)
  0 0.0l0)

(deftest round-ld.2
  (round 0.0l0 10.3d0)
  0 0.0l0)

(deftest-error round-ld.3
  (round 10.3l0 0.0d0)
  division-by-zero)

(deftest round-ld.4a
  (round-check-ld 10.3l0 100.0d0    0 10.3l0)
  t)

(deftest round-ld.4b
  (round-check-ld -10.3l0 100.0d0    0 -10.3l0)
  t)

(deftest round-ld.4c
  (round-check-ld 10.3l0 -100.0d0    0 10.3l0)
  t)

(deftest round-ld.4d
  (round-check-ld -10.3l0 -100.0d0    0 -10.3l0)
  t)

(deftest round-ld.5a
  (round-check-ld 12.3l0 1.5d0    8 0.3l0)
  t)

(deftest round-ld.5b
  (round-check-ld -12.3l0 1.5d0    -8 -0.3l0)
  t)

(deftest round-ld.5c
  (round-check-ld 12.3l0 -1.5d0    -8 0.3l0)
  t)

(deftest round-ld.5d
  (round-check-ld -12.3l0 -1.5d0    8 -0.3l0)
  t)

(deftest round-ld.6a
  (round-check-ld 12.3l0 4.5d0    3 -1.2l0)
  t)

(deftest round-ld.6b
  (round-check-ld -12.3l0 4.5d0    -3 1.2l0)
  t)

(deftest round-ld.6c
  (round-check-ld 12.3l0 -4.5d0    -3 -1.2l0)
  t)

(deftest round-ld.6d
  (round-check-ld -12.3l0 -4.5d0    3 1.2l0)
  t)


;;  long-float - long-float
(defun round-check-ll (a b c d)
  (round-equal
    a b c d
    :eps 1.0d-14
    :call #'fixnump
    :type 'long-float))

(deftest round-ll.1
  (round 0.0l0 10.3l0)
  0 0.0l0)

(deftest round-ll.2
  (round 0.0l0 10.3l0)
  0 0.0l0)

(deftest-error round-ll.3
  (round 10.3l0 0.0l0)
  division-by-zero)

(deftest round-ll.4a
  (round-check-ll 10.3l0 100.0l0    0 10.3l0)
  t)

(deftest round-ll.4b
  (round-check-ll -10.3l0 100.0l0    0 -10.3l0)
  t)

(deftest round-ll.4c
  (round-check-ll 10.3l0 -100.0l0    0 10.3l0)
  t)

(deftest round-ll.4d
  (round-check-ll -10.3l0 -100.0l0    0 -10.3l0)
  t)

(deftest round-ll.5a
  (round-check-ll 12.3l0 1.5l0    8 0.3l0)
  t)

(deftest round-ll.5b
  (round-check-ll -12.3l0 1.5l0    -8 -0.3l0)
  t)

(deftest round-ll.5c
  (round-check-ll 12.3l0 -1.5l0    -8 0.3l0)
  t)

(deftest round-ll.5d
  (round-check-ll -12.3l0 -1.5l0    8 -0.3l0)
  t)

(deftest round-ll.6a
  (round-check-ll 12.3l0 4.5l0    3 -1.2l0)
  t)

(deftest round-ll.6b
  (round-check-ll -12.3l0 4.5l0    -3 1.2l0)
  t)

(deftest round-ll.6c
  (round-check-ll 12.3l0 -4.5l0    -3 -1.2l0)
  t)

(deftest round-ll.6d
  (round-check-ll -12.3l0 -4.5l0    3 1.2l0)
  t)

