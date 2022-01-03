;;
;;  ANSI COMMON LISP: 24. System Construction
;;

;;
;;  provide
;;
(deftest provide.1
  *modules*
  nil)

(deftest provide.2
  (let ((*modules* nil))
    (provide "Hello"))
  nil)

(deftest provide.3
  (let ((*modules* nil))
    (provide "Hello")
    *modules*)
  ("Hello"))

(deftest provide.4
  (let ((*modules* nil))
    (provide "Hello")
    (provide "Hello")
    *modules*)
  ("Hello"))

(deftest provide.5
  (let ((*modules* nil))
    (provide "Hello")
    (provide "HELLO")
    (provide "Hello")
    *modules*)
  ("HELLO" "Hello"))

(deftest provide.6
  (let ((*modules* nil))
    (provide 'hello)
    (provide :test)
    *modules*)
  ("TEST" "HELLO"))


;;
;;  require
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

(defvar *require-test1*)
(defvar *require-test2*)

(deftest require.6
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-file2.lisp")))
  nil)

(deftest require.7
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-file2.lisp"))
    *modules*)
  ("HELLO"))

(deftest require.8
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-file2.lisp"))
    *require-test1*)
  t)

(deftest require.9
  (let ((*modules* nil)
        (*require-test1* nil)
        (*require-test2* nil))
    (require "HELLO" '("test/rtsystem-file2.lisp"
                       "test/rtsystem-file3.lisp"))
    (values *require-test1* *require-test2*))
  t t)

(deftest require.10
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" '("test/rtsystem-file2.lisp"))
    (setq *require-test1* nil)
    (require "HELLO" '("test/rtsystem-file2.lisp"))
    *require-test1*)
  t)

(deftest require.11
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" "test/rtsystem-file2.lisp"))
  nil)

(deftest require.12
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" "test/rtsystem-file2.lisp")
    *modules*)
  ("HELLO"))

(deftest require.13
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" #p"test/rtsystem-file2.lisp"))
  nil)

(deftest require.14
  (let ((*modules* nil)
        (*require-test1* nil))
    (require "HELLO" #p"test/rtsystem-file2.lisp")
    *modules*)
  ("HELLO"))

