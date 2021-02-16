;;
;;  compile-define
;;

;;
;;  deftype
;;
(deftest compile-deftype.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (deftype compile-deftype-1 ()
        'integer)
      (eval-when (:compile-toplevel)
        (setq *result* (typep 100 'compile-deftype-1))))
    *result*)
  t)

(deftest compile-deftype.2
  (progn
    (setq *result* nil)
    (test-file-compile
      (deftype compile-deftype-2 ()
        'integer)
      (eval-when (:compile-toplevel :load-toplevel)
        (setq *result* (typep 100 'compile-deftype-2))))
    (setq *result* nil)
    (deftype compile-deftype-2 ()
      'string)
    (test-file-load)
    *result*)
  t)

(deftest-error compile-deftype.3
  (test-file-compile
    (let ()
      (deftype compile-deftype-3 ()
        'integer))
    (eval-when (:compile-toplevel)
      (setq *result* (typep 100 'compile-deftype-3)))))


;;
;;  defmacro
;;
(deftest compile-defmacro.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defmacro compile-defmacro-1 ()
        :hello)
      (eval-when (:compile-toplevel)
        (setq *result* (compile-defmacro-1))))
    *result*)
  :hello)

(deftest compile-defmacro.2
  (progn
    (setq *result* nil)
    (test-file-compile
      (defmacro compile-defmacro-2 ()
        :hello)
      (eval-when (:compile-toplevel :load-toplevel)
        (setq *result* (compile-defmacro-2))))
    (setq *result* nil)
    (test-file-load)
    *result*)
  :hello)

(deftest compile-defmacro.3
  (progn
    (setq *result* nil)
    (test-file-compile-string
      "(defmacro compile-defmacro-3 (x)"
      "  `(1+ ,x))"
      "(eval-when (:compile-toplevel)"
      "  (setq *result* (compile-defmacro-3 10)))")
    *result*)
  11)


;;
;;  define-modify-macro
;;
(deftest compile-define-modify-macro.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (define-modify-macro compile-define-modify-macro-1 (&rest args) append)
      (eval-when (:compile-toplevel)
        (let ((x (list 10 20 30)))
          (compile-define-modify-macro-1 x '(a b c))
          (setq *result* x))))
    *result*)
  (10 20 30 a b c))


;;
;;  defun
;;
(deftest-error compile-defun.1
  (test-file-compile
    (defun compile-defun-1 (x)
      (* x x))
    (eval-when (:compile-toplevel)
      (setq *result* (compile-defun-1 11)))))

(deftest compile-defun.2
  (progn
    (setq *result* nil)
    (test-file-compile
      (defun compile-defun-2 (x)
        (* x x))
      (setq *result* (compile-defun-2 11)))
    (test-file-load)
    *result*)
  121)


;;
;;  defvar
;;
(deftest compile-defvar.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defvar *compile-defvar-1* 100)
      (eval-when (:compile-toplevel)
        (setq *result* (boundp '*compile-defvar-1*))))
    *result*)
  nil)

(deftest compile-defvar.2
  (progn
    (test-file-compile
      (defvar *compile-defvar-2* 100)
      (setq *result* (symbol-value '*compile-defvar-2*)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  100)

(deftest compile-defvar.3
  (progn
    (test-file-compile
      (defvar *compile-defvar-3* 100)
      (let ((*compile-defvar-3* 200))
        (setq *result* (list *compile-defvar-3*
                             (symbol-value '*compile-defvar-3*)))))
    (setq *result* nil)
    (test-file-load)
    *result*)
  (200 200))


;;
;;  defparameter
;;
(deftest compile-defparameter.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defparameter *compile-defparameter-1* 100)
      (eval-when (:compile-toplevel)
        (setq *result* (boundp '*compile-defparameter-1*))))
    *result*)
  nil)

(deftest compile-defparameter.2
  (progn
    (test-file-compile
      (defparameter *compile-defparameter-2* 100)
      (setq *result* (symbol-value '*compile-defparameter-2*)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  100)

(deftest compile-defparameter.3
  (progn
    (test-file-compile
      (defparameter *compile-defparameter-3* 100)
      (let ((*compile-defparameter-3* 200))
        (setq *result* (list *compile-defparameter-3*
                             (symbol-value '*compile-defparameter-3*)))))
    (setq *result* nil)
    (test-file-load)
    *result*)
  (200 200))


;;
;;  defconstant
;;
(deftest compile-defconstant.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (eval-when (:compile-toplevel)
        (defconstant *compile-defconstant-1* 100)
        (setq *result* *compile-defconstant-1*)))
    *result*)
  100)

(deftest compile-defconstant.2
  (progn
    (setq *result* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (defconstant *compile-defconstant-2* 200)
        (setq *result* *compile-defconstant-2*)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  200)

(deftest compile-defconstant.3
  (progn
    (setq *result* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (defconstant *compile-defconstant-3* 300)
        (setq *result* *compile-defconstant-3*)))
    *result*)
  300)

(deftest compile-defconstant.4
  (progn
    (setq *result* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (defconstant *compile-defconstant-4* 400)
        (setq *result* *compile-defconstant-4*)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  400)


;;
;;  declaim
;;
(deftest compile-declaim.1
  (progn
    (test-file-compile
      (declaim (special *compile-declaim-1*))
      (setq *compile-declaim-1* 100)
      (setq *result* (symbol-value '*compile-declaim-1*)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  100)

(deftest compile-declaim.2
  (progn
    (test-file-compile
      (declaim (special *compile-declaim-2*))
      (setq *compile-declaim-2* 100)
      (let ((*compile-declaim-2* 200))
        (setq *result* (list *compile-declaim-2*
                             (symbol-value '*compile-declaim-2*)))))
    (setq *result* nil)
    (test-file-load)
    *result*)
  (200 200))


;;
;;  defsetf
;;
(deftest compile-defsetf.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defsetf compile-defsetf-1 set)
      (eval-when (:compile-toplevel)
        (let (x)
          (declare (special x) (ignorable x))
          (setf (compile-defsetf-1 'x) 10)
          (setq *result* x))))
    *result*)
  10)


;;
;;  define-setf-expander
;;
(deftest compile-define-setf-expander.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (define-setf-expander
        compile-define-setf-expander-1 (x)
        (let ((g (gensym)))
          (values nil nil `(,g) `(setf (cdr ,x) ,g) `(setq ,g (cdr ,x)))))
      (eval-when (:compile-toplevel)
        (let ((x (cons 10 20)))
          (setf (compile-define-setf-expander-1 x) 30)
          (setq *result* x))))
    *result*)
  (10 . 30))


;;
;;  defstruct
;;
(deftest compile-defstruct.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defstruct compile-defstruct-1 aaa)
      (eval-when (:compile-toplevel)
        (let ((x (make-compile-defstruct-1 :aaa 10)))
          (setq *result* (compile-defstruct-1-aaa x)))))
    *result*)
  10)


;;
;;  define-condition
;;
(deftest compile-define-condition.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (define-condition compile-define-condition-1 ()
        ((aaa :initarg :aaa)))
      (eval-when (:compile-toplevel)
        (let ((x (make-condition 'compile-define-condition-1 :aaa 20)))
          (setq *result* (slot-value x 'aaa)))))
    *result*)
  20)


;;
;;  defpackage
;;
(deftest compile-defpackage.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defpackage compile-defpackage-1 (:use common-lisp))
      (eval-when (:compile-toplevel)
        (setq *result* (package-name (find-package 'compile-defpackage-1)))))
    *result*)
  "COMPILE-DEFPACKAGE-1")

(deftest compile-defpackage.2
  (progn
    (setq *result* nil)
    (test-file-compile-string
      "(defpackage compile-defpackage-2 (:use common-lisp))"
      "(in-package compile-defpackage-2)"
      "(eval-when (:compile-toplevel)"
      "  (setq common-lisp-user::*result* (package-name *package*)))")
    *result*)
  "COMPILE-DEFPACKAGE-2")


;;
;;  defclass
;;
(deftest compile-defclass.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defclass compile-defclass-1 ()
        ((aaa :initarg :aaa)))
      (eval-when (:compile-toplevel)
        (let ((x (make-instance 'compile-defclass-1 :aaa 111)))
          (setq *result* (slot-value x 'aaa)))))
    *result*)
  111)


;;
;;  defgeneric
;;
(deftest compile-defgeneric.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defgeneric compile-defgeneric-1 ())
      (eval-when (:compile-toplevel)
        (setq *result* (fboundp 'compile-defgeneric-1))))
    *result*)
  nil)


;;
;;  defmethod
;;
(deftest compile-defmethod.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defgeneric compile-defmethod-1 ())
      (defmethod compile-defmethod-1 ()
        :hello)
      (eval-when (:compile-toplevel)
        (setq *result* (fboundp 'compile-defmethod-1))))
    *result*)
  nil)

(deftest compile-defmethod.2
  (progn
    (test-file-compile
      (defgeneric compile-defmethod-2 (x y))
      (defmethod compile-defmethod-2 (x y)
        (+ x y))
      (setq *result* (compile-defmethod-2 10 20)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  30)


;;
;;  define-method-combination
;;
(deftest compile-define-method-combination.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (defun compile-define-method-combination-1 (&rest args)
        (apply #'+ args))
      (define-method-combination compile-define-method-combination-1)
      (defgeneric compile-define-method-combination-generic-1 (x)
                  (:method-combination compile-define-method-combination-1))
      (eval-when (:compile-toplevel)
        (setq *result* (fboundp 'compile-define-method-combination-generic-1))))
    *result*)
  nil)

(deftest compile-define-method-combination.2
  (progn
    (setq *result* nil)
    (test-file-compile
      (defun compile-define-method-combination-2 (&rest args)
        (apply #'+ args))
      (define-method-combination compile-define-method-combination-2)
      (defgeneric compile-define-method-combination-generic-2 (x)
                  (:method-combination compile-define-method-combination-2))
      (setq *result* (fboundp 'compile-define-method-combination-generic-2)))
    (setq *result* nil)
    (test-file-load)
    *result*)
  t)


;;
;;  define-compiler-macro
;;
(deftest compile-define-compiler-macro.1
  (expr-compile
    (define-symbol-macro *cl-package* (find-package "COMMON-LISP")))
  *cl-package*)

