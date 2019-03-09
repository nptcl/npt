;;
;;  ANSI COMMON LISP: 12. Numbers
;;
(import 'lisp-system::make-bignum)
(import 'lisp-system::make-ratio)
(import 'lisp-system::make-complex)
(import 'lisp-system::fixnump)
(import 'lisp-system::bignump)
(import 'lisp-system::ratiop)
(import 'lisp-system::equal-random-state)

(defun equal-eps (a b &key (eps 1.0e-6))
  (< (abs (- a b)) (abs eps)))

(defun equal-float2 (a1 b1 a2 b2 eps)
  (and (= a1 a2)
       (or (= b1 b2)
           (equal-eps b1 b2 :eps eps))))

(defun check-float-complex (value real imag type eps)
  (let* ((r (realpart value))
         (i (imagpart value))
         (a (typep r type))
         (b (or (zerop i) (typep i type)))
         (c (equal-eps r real :eps eps))
         (d (equal-eps i imag :eps eps)))
    (or (and a b c d)
        (format t "~&ERROR: ~A ~A ~A ~A~%" a b c d))))

(defun check-float-real (value real type eps)
  (let ((r (realpart value))
        (i (imagpart value)))
    (and (typep r type)
         (equal-eps r real :eps eps)
         (zerop i))))

(defmacro deftest-float (name expr real &optional imag (eps 1.0e-6))
  `(deftest ,name
     ,(if imag
        `(check-float-complex ,expr ,real ,imag 'float ,eps)
        `(check-float-real ,expr ,real 'float ,eps))
     t))

(defmacro deftest-single (name expr real &optional imag (eps 1.0e-6))
  `(deftest ,name
     ,(if imag
        `(check-float-complex ,expr ,real ,imag 'single-float ,eps)
        `(check-float-real ,expr ,real 'single-float ,eps))
     t))

(defmacro deftest-double (name expr real &optional imag (eps 1.0d-14))
  `(deftest ,name
     ,(if imag
        `(check-float-complex ,expr ,real ,imag 'double-float ,eps)
        `(check-float-real ,expr ,real 'double-float ,eps))
     t))

(defmacro deftest-long (name expr real &optional imag (eps 1.0l-14))
  `(deftest ,name
     ,(if imag
        `(check-float-complex ,expr ,real ,imag 'long-float ,eps)
        `(check-float-real ,expr ,real 'long-float ,eps))
     t))


;;
;;  round
;;
(defun round-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float) (call #'integerp))
  (multiple-value-bind (e f) (round a b)
    (or (and (funcall call e)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "round-equal error: (~S ~S) ~A, ~A, ~A" e f
          (funcall call e)
          (typep f type)
          (equal-float2 c d e f eps)))))

(defun roundb (a b)
  (round (make-bignum a) b))

(defun roundb-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float) (call #'integerp))
  (multiple-value-bind (e f) (roundb a b)
    (or (and (funcall call e)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "round-equal error: (~S ~S) ~A, ~A, ~A" e f
          (funcall call e)
          (typep f type)
          (equal-float2 c d e f eps)))))


;;
;;  fround
;;
(defun fround-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float))
  (multiple-value-bind (e f) (fround a b)
    (or (and (typep e type)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "fround-equal error: (~S ~S) ~A, ~A, ~A" e f
          (typep e type)
          (typep f type)
          (equal-float2 c d e f eps)))))

(defun froundb (a b)
  (fround (make-bignum a) b))

(defun froundb-equal
  (a b c d &key (eps 1.0e-6) (type 'single-float))
  (multiple-value-bind (e f) (froundb a b)
    (or (and (typep e type)
             (typep f type)
             (equal-float2 c d e f eps))
        (error "fround-equal error: (~S ~S) ~A, ~A, ~A" e f
          (typep e type)
          (typep f type)
          (equal-float2 c d e f eps)))))

