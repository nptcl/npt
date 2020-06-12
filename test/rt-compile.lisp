;;
;;  compile-file
;;
(defconstant +compile-input+ #p"_debug_input.lisp")
(defconstant +compile-output+ #p"_debug_output.fasl")
(defvar *result*)

(defun test-compile-load (x)
  (with-open-file (stream +compile-input+ :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-right-margin* 70))
      (format stream "~S~%" x)))
  (compile-file +compile-input+ :output-file +compile-output+)
  (setq *result* nil)
  (load +compile-output+)
  (prog1 *result*
    (setq *result* nil)))

(defun call-compile (x)
  (unwind-protect
    (test-compile-load x)
    (flet ((del (x) (when (probe-file x)
                      (delete-file x))))
      (del +compile-input+)
      (del +compile-output+))))

(defmacro test-compile (x)
  `(call-compile ',x))


;;
;;  interface
;;
(deftest compile.1
  (test-compile nil)
  nil)

(deftest compile.2
  (test-compile t)
  nil)

(deftest compile.3
  (test-compile 10)
  nil)

(deftest compile-nil.1
  (test-compile
    (setq *result* nil))
  nil)

(deftest compile-t.1
  (test-compile
    (setq *result* t))
  t)

(deftest compile-cons.1
  (test-compile
    (setq *result* '(10 20 30)))
  (10 20 30))

(deftest compile-cons.2
  (test-compile
    (setq *result* '(10 (#\A #\b (40 50 "Hello")) . 70)))
  (10 (#\A #\b (40 50 "Hello")) . 70))

(deftest compile-vector.1
  (test-compile
    (setq *result* #()))
  #())

(deftest compile-vector.2
  (test-compile
    (setq *result* #(10 20 30)))
  #(10 20 30))

(deftest compile-vector.3
  (test-compile
    (setq *result* #(10 (#\A #\b (40 50 "Hello". 80)) 70)))
  #(10 (#\A #\b (40 50 "Hello" . 80)) 70))

(deftest compile-character.1
  (test-compile
    (setq *result* #\B))
  #\B)

(deftest compile-string.1
  (test-compile
    (setq *result* "Hello"))
  "Hello")

(deftest compile-symbol.1
  (test-compile
    (setq *result* 'hello))
  hello)

(deftest compile-symbol.2
  (test-compile
    (setq *result* :aaa))
  :aaa)

(deftest compile-fixnum.1
  (test-compile
    (setq *result* 111))
  111)

(deftest compile-bignum.1
  (test-compile
    (setq *result* #xFFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222))
  #xFFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222)

(deftest compile-bignum.2
  (test-compile
    (setq *result* #x-FFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222))
  #x-FFFFFFFFFFFFAAAAAAAAAAAAAA1111111111222222222222)

(deftest compile-ratio.1
  (test-compile
    (setq *result* 111/7))
  111/7)

(deftest compile-ratio.2
  (test-compile
    (setq *result* -111/7))
  -111/7)

(deftest compile-single-float.1
  (test-compile
    (setq *result* 10.4f0))
  10.4f0)

(deftest compile-double-float.1
  (test-compile
    (setq *result* -10.4d0))
  -10.4d0)

(deftest compile-long-float.1
  (test-compile
    (setq *result* 10.4L0))
  10.4L0)

(deftest compile-complex.1
  (test-compile
    (setq *result* #c(-10 20)))
  #c(-10 20))

(deftest compile-complex.2
  (test-compile
    (setq *result* #c(1.23 -4.56)))
  #c(1.23 -4.56))


;;
;;  do-tests
;;
(do-tests :test t)

