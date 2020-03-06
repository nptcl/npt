;;
;;  optimize-scope
;;
(declaim (optimize (speed 1) (safety 1)))

(import 'lisp-system::optimize-check)

(deftest rtcode-scope-default
  (progn
    (declaim (optimize (speed 1) (safety 1)))
    (values)))

(defmacro scope-speed (&body body)
  `(locally
     (declare (optimize (safety 0) (speed 3)))
     ,@body))

(defmacro scope-safety (&body body)
  `(locally
     (declare (optimize (safety 3) (speed 0)))
     ,@body))

(deftest scope-speed.1
  (scope-speed
    (optimize-check scope))
  1)

(deftest scope-safety.1
  (scope-safety
    (optimize-check scope))
  0)

