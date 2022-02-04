;;
;;  Error
;;
(deftest if-nil.1
  (list 10 20 (when nil 'hello))
  (10 20 nil))

(deftest pprint-vector-error.1
  (let ((*print-pretty* t))
    (prin1-to-string #()))
  "#()")

(defconstant +load-readtable+
  (with-output-to-string (*standard-output*)
    (format t "(set-dispatch-macro-character~%")
    (format t "  #\\# #\\@~%")
    (format t "  (lambda (stream char number)~%")
    (format t "    (declare (ignore stream char number))~%")
    (format t "    :hello))")))

(deftest load-readtable.1
  (with-input-from-string (input +load-readtable+)
    (load input))
  t)

(deftest load-readtable.2
  (progn
    (with-input-from-string (input +load-readtable+)
      (load input))
    (values
      (read-from-string "  #@ 10 20 30")))
  :hello)

(deftest flet-error.1
  (functionp
    (locally
      (declare (optimize (speed 3) (safety 0)))
      (flet ((bbb () 10))
        #'bbb)))
  t)


;;
;;  symbol-package
;;
(deftest symbol-package-error.1
  (progn
    (eval '(defconstant symbol-package-error 10))
    (unintern 'symbol-package-error))
  t)

(deftest symbol-package-error.2
  (find-symbol "SYMBOL-PACKAGE-ERROR")
  nil nil)


;;
;;  symbol-plist
;;
(defconstant symbol-plist-error 10)
(deftest symbol-plist-error.1
  (setf (symbol-plist 'symbol-plist-error) nil)
  nil)

(deftest symbol-plist-error.2
  (progn
    (setf (get 'symbol-plist-error 'hello) 10)
    (get 'symbol-plist-error 'hello))
  10)

(deftest symbol-plist-error.3
  (progn
    (setf (get 'symbol-plist-error 'hello) 10)
    (remprop 'symbol-plist-error 'hello)
    (get 'symbol-plist-error 'hello))
  nil)


;;
;;  recursive handler call
;;
(defvar *handler-call* nil)

(deftest handler-call.1
  (progn
    (setq *handler-call* nil)
    (handler-case
      (handler-case
        (handler-case
          (progn
            (push 1 *handler-call*)
            (error "AAA"))
          (error ()
            (push 2 *handler-call*)
            (error "BBB")))
        (error ()
          (push 3 *handler-call*)
          (error "CCC")))
      (error ()
        (push 4 *handler-call*)))
    *handler-call*)
  (4 3 2 1))

(deftest handler-call.2
  (progn
    (setq *handler-call* nil)
    (handler-case
      (handler-bind
        ((error (lambda (c)
                  (declare (ignore c))
                  (push 3 *handler-call*)
                  (error "BBB"))))
        (handler-bind
          ((error (lambda (c)
                    (declare (ignore c))
                    (push 2 *handler-call*)
                    (error "CCC"))))
          (handler-bind
            ((error (lambda (c)
                      (declare (ignore c))
                      (push 1 *handler-call*)
                      (error "DDD"))))
            (error "AAA"))))
      (error ()
        (push 4 *handler-call*)))
    *handler-call*)
  (4 3 2 1))


;;
;;  define-symbol-macro
;;
(define-symbol-macro define-symbol-macro-error-1 (define-symbol-macro-error-2))

(deftest define-symbol-macro-error.1
  (macrolet ((define-symbol-macro-error-2 () :hello))
    define-symbol-macro-error-1)
  :hello)


;;
;;  scope type-check
;;
(deftype scope-type-check ()
  '(cons (eql queue) *))

(deftest scope-type-check.1
  (lisp-system:subtypep!
    '(cons (eql queue) *)
    '(or null (cons * (or null cons))))
  lisp-system::false)

(deftest scope-type-check.2
  (defun scope-type-check-1 (inst)
    (declare (type scope-type-check inst))
    (cadr inst))
  scope-type-check-1)

(deftest scope-type-check.3
  (scope-type-check-1 '(queue 10 20 30))
  10)

(deftest-error scope-type-check.4
  (scope-type-check-1 '(queue . 20))
  type-error)


;;
;;  typep function
;;
(defun type-function-1 (x)
  (declare (type no-such-function-type x))
  x)

(deftest typep-function.1
  (typep #'type-function-1 'function)
  t)


;;
;;  compile-file defconstant
;;
(deftest defconstant-compile.1
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (with-open-file (s input :direction :output)
        (format s "(defconstant defconstant-compile-1 10)~%")
        (format s "(deftype defconstant-compile-2 ()~%")
        (format s "  `(integer 0 ,defconstant-compile-1))~%")
        (format s "(defstruct defconstant-compile-3~%")
        (format s "  (defconstant-compile-4 0~%")
        (format s "    :type defconstant-compile-2))~%"))
      (file-position input :start)
      (compile-file input :output-file output))
    nil)
  nil)

