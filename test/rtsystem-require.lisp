;;
;;  ANSI COMMON LISP: 24. System Construction
;;

;;
;;  Variable *MODULES*
;;
(deftest special-modules.1
  (lisp-system:specialp '*modules*)
  t)

(deftest special-modules.2
  (listp *modules*)
  t)


;;
;;  Function PROVIDE
;;
(deftest provide.1
  (let ((*modules* nil))
    (provide "Hello"))
  nil)

(deftest provide.2
  (let ((*modules* nil))
    (provide "Hello")
    *modules*)
  ("Hello"))

(deftest provide.3
  (let ((*modules* nil))
    (provide "Hello")
    (provide "Hello")
    *modules*)
  ("Hello"))

(deftest provide.4
  (let ((*modules* nil))
    (provide "Hello")
    (provide "HELLO")
    (provide "Hello")
    *modules*)
  ("HELLO" "Hello"))

(deftest provide.5
  (let ((*modules* nil))
    (provide 'hello)
    (provide :test)
    *modules*)
  ("TEST" "HELLO"))

(deftest-error! provide-error.1
  (eval '(provide)))

(deftest-error! provide-error.2
  (eval '(provide 10))
  type-error)

(deftest-error! provide-error.3
  (eval '(provide "Hello" nil)))


;;
;;  Function REQUIRE
;;
(deftest require.1
  (let ((*modules* nil)
        (lisp-system::*module-provider-functions*
          (list (constantly t))))
    (require "HELLO" nil))
  nil)

(deftest require.2
  (let ((*modules* nil)
        (lisp-system::*module-provider-functions*
          (list (constantly t))))
    (require "HELLO" nil)
    *modules*)
  ("HELLO"))

(deftest require.3
  (let ((*modules* nil)
        (lisp-system::*module-provider-functions*
          (list (constantly nil)
                (constantly nil)
                (constantly nil)
                (constantly t)
                (constantly nil)
                (constantly nil))))
    (require "HELLO" nil)
    *modules*)
  ("HELLO"))

(deftest-error require.4
  (let ((*modules* nil)
        (lisp-system::*module-provider-functions*
          (list (constantly nil)
                (constantly nil)
                (constantly nil)
                (constantly nil)
                (constantly nil))))
    (require "HELLO" nil)))

(deftest-error require.5
  (let ((*modules* nil)
        (lisp-system::*module-provider-functions*
          (list (constantly nil)
                (constantly nil)
                (constantly nil)
                (constantly nil)
                (constantly nil))))
    (require "HELLO" nil)))

(defun require-call ()
  nil)

(deftest-error require.6
  (let ((*modules* nil)
        (lisp-system::*module-provider-functions*
          (list (constantly nil)
                #'require-call
                'require-call
                (constantly nil)
                (constantly nil)
                (constantly nil))))
    (require "HELLO" nil)))

(defvar *require-test1*)
(defvar *require-test2*)

(deftest require-load.1
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-require1.lisp")))
  nil)

(deftest require-load.2
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-require1.lisp"))
    *modules*)
  ("HELLO"))

(deftest require-load.3
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-require1.lisp"))
    *require-test1*)
  t)

(deftest require-load.4
  (let ((*modules* nil)
        (*require-test1* nil)
        (*require-test2* nil))
    (require "HELLO" '("test/rtsystem-require1.lisp"
                       "test/rtsystem-require2.lisp"))
    (values *require-test1* *require-test2*))
  t t)

(deftest require-load.5
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-require1.lisp"))
    (setq *require-test1* nil)
    (require "HELLO" '("test/rtsystem-require1.lisp"))
    *require-test1*)
  t)

(deftest require-load.6
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" "test/rtsystem-require1.lisp"))
  nil)

(deftest require-load.7
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" "test/rtsystem-require1.lisp")
    *modules*)
  ("HELLO"))

(deftest require-load.8
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" #p"test/rtsystem-require1.lisp"))
  nil)

(deftest require-load.9
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" #p"test/rtsystem-require1.lisp")
    *modules*)
  ("HELLO"))

(deftest-error require-load.10
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" #p"test/no-such-require-file.lisp"))
  file-error)

(deftest-error require-load.11
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" #p"test/*.lisp"))
  file-error)

(deftest-error! require-error.1
  (eval '(require)))

(deftest-error! require-error.2
  (eval '(require 10))
  type-error)

(deftest-error! require-error.3
  (eval '(require "HELLO" nil nil)))

