;;
;;  ANSI COMMON LISP: 12. Numbers
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

(load #p"test/rtapp-round1.lisp")
(load #p"test/rtapp-roundf.lisp")
(load #p"test/rtapp-roundb.lisp")
(load #p"test/rtapp-roundr.lisp")
(load #p"test/rtapp-rounds.lisp")

