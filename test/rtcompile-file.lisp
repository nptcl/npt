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

(defmacro value-compile (x)
  `(call-compile '(setq *result* ,x)))


;;
;;  type
;;
(defun type-compile-load (x)
  (with-open-file (stream +compile-input+ :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-right-margin* 70))
      (format stream "(setq *result* #.(lisp-system::parse-type '~S))" x)))
  (compile-file +compile-input+ :output-file +compile-output+)
  (setq *result* nil)
  (load +compile-output+)
  (prog1 *result*
    (setq *result* nil)))

(defun calltype-compile (x)
  (unwind-protect
    (lisp-system::type-object
      (type-compile-load x))
    (flet ((del (x) (when (probe-file x)
                      (delete-file x))))
      (del +compile-input+)
      (del +compile-output+))))

(defmacro type-compile (x)
  `(calltype-compile ',x))


;;
;;  exec
;;
(defun exec-compile-load (x)
  (with-open-file (stream +compile-input+ :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-right-margin* 70))
      (format stream "(setq *result* #.~S)" x)))
  (compile-file +compile-input+ :output-file +compile-output+)
  (setq *result* nil)
  (load +compile-output+)
  (prog1 *result*
    (setq *result* nil)))

(defun exec-call-compile (x)
  (unwind-protect
    (exec-compile-load x)
    (flet ((del (x) (when (probe-file x)
                      (delete-file x))))
      (del +compile-input+)
      (del +compile-output+))))

(defmacro exec-compile (x)
  `(exec-call-compile ',x))

