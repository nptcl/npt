;;
;;  ANSI COMMON LISP: 17. Sequences
;;
(defun eqlf (v)
  (lambda (x) (eql v x)))

(defun noteqlf (v)
  (lambda (x) (not (eql v x))))

