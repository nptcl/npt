;;
;;  compile-file
;;
(defconstant +compile-input+ #p"_debug_input.lisp")
(defconstant +compile-output+ #p"_debug_output.fasl")
(defvar *compile-result*)

(defun test-compile-load (x)
  (with-open-file (stream +compile-input+ :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-right-margin* 70))
      (format stream "~S~%" x)))
  (compile-file +compile-input+ :output-file +compile-output+)
  (setq *compile-result* nil)
  (load +compile-output+)
  (prog1 *compile-result*
    (setq *compile-result* nil)))

(defun test-compile (x)
  (unwind-protect
    (test-compile-load x)
    (flet ((del (x) (when (probe-file x)
                      (delete-file x))))
      (del +compile-input+)
      (del +compile-output+))))


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


;;
;;  do-tests
;;
(do-tests :test t)

