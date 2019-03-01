;;
;;  ANSI COMMON LISP: 12. Numbers
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

(load #p"test/rtapp-fround1.lisp")
(load #p"test/rtapp-froundf.lisp")
(load #p"test/rtapp-froundb.lisp")
(load #p"test/rtapp-froundr.lisp")
(load #p"test/rtapp-frounds.lisp")

