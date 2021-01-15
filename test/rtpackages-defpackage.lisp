;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Macro DEFPACKAGE
;;
(deftest defpackage.1
  (packagep
    (defpackage defpackage-1))
  t)

(deftest defpackage.2
  (package-name
    (defpackage defpackage-2))
  "DEFPACKAGE-2")

(deftest defpackage.3
  (progn
    (defpackage defpackage-3)
    (packagep (find-package 'defpackage-3)))
  t)


;;
;;  name
;;
(deftest defpackage-name.1
  (package-name
    (defpackage defpackage-name-1))
  "DEFPACKAGE-NAME-1")

(deftest defpackage-name.2
  (package-name
    (defpackage "DEFPACKAGE-NAME-2"))
  "DEFPACKAGE-NAME-2")

(deftest defpackage-name.3
  (package-name
    (defpackage :defpackage-name-3))
  "DEFPACKAGE-NAME-3")

(deftest defpackage-name.4
  (package-name
    (defpackage #:defpackage-name-4))
  "DEFPACKAGE-NAME-4")

(deftest defpackage-name.5
  (prog1 (package-name
           (defpackage #\a))
    (delete-package #\a))
  "a")

(deftest-error defpackage-name.6
  (eval '(defpackage 100))
  type-error)

(deftest defpackage-name.7
  (package-name
    (lisp-system::defpackage 'defpackage-name-7))
  "DEFPACKAGE-NAME-7")

(deftest defpackage-name.8
  (package-name
    (lisp-system::defpackage "DEFPACKAGE-NAME-8"))
  "DEFPACKAGE-NAME-8")

(deftest-error defpackage-name.9
  (eval '(lisp-system::defpackage 100))
  type-error)


;;
;;  nicknames
;;
(deftest defpackage-nicknames.1
  ;; make
  (progn
    (defpackage defpackage-nicknames-1
      (:nicknames defpackage-nicknames-2 defpackage-nicknames-3))
    (values
      (packagep (find-package 'defpackage-nicknames-1))
      (packagep (find-package 'defpackage-nicknames-2))
      (packagep (find-package 'defpackage-nicknames-3))))
  t t t)

(deftest defpackage-nicknames.2
  ;; update
  (progn
    (defpackage defpackage-nicknames-1
      (:nicknames defpackage-nicknames-2
                  defpackage-nicknames-3
                  defpackage-nicknames-4))
    (values
      (packagep (find-package 'defpackage-nicknames-1))
      (packagep (find-package 'defpackage-nicknames-2))
      (packagep (find-package 'defpackage-nicknames-3))
      (packagep (find-package 'defpackage-nicknames-4))))
  t t t t)

(deftest-error defpackage-nicknames.3
  (defpackage defpackage-nicknames-5
    (:nicknames common-lisp-user))
  package-error)

(deftest defpackage-nicknames.4
  (packagep
    (defpackage defpackage-nicknames-6
      (:nicknames "DEFPACKAGE-NICKNAMES-7")))
  t)

(deftest-error defpackage-nicknames.5
  (eval '(defpackage defpackage-nicknames-8
           (:nicknames 100)))
  type-error)

(deftest-error defpackage-nicknames.6
  (lisp-system::defpackage 'defpackage-nicknames-8 :nicknames '(100))
  type-error)

(deftest-error defpackage-nicknames.7
  (eval '(lisp-system::defpackage 'defpackage-nicknames-8 :nicknames 100))
  type-error)


;;
;;  documentation
;;
(deftest defpackage-documentation.1
  (documentation
    (defpackage defpackage-documentation-1 (:documentation "Hello"))
    t)
  "Hello")

(deftest-error defpackage-documentation.2
  (eval '(defpackage defpackage-documentation-2 (:documentation nil)))
  type-error)

(deftest-error defpackage-documentation.3
  (eval '(defpackage defpackage-documentation-3 (:documentation 10)))
  type-error)

(deftest-error defpackage-documentation.4
  (eval '(defpackage defpackage-documentation-4
           (:documentation "Hello")
           (:documentation "Error")))
  program-error)

(deftest defpackage-documentation.5
  (documentation
    (lisp-system::defpackage
      'defpackage-documentation-5
      :documentation "Hello")
    t)
  "Hello")

(deftest-error defpackage-documentation.6
  (eval '(lisp-system::defpackage
           'defpackage-documentation-6
           :documentation nil))
  type-error)

(deftest-error defpackage-documentation.7
  (eval '(lisp-system::defpackage
           'defpackage-documentation-7
           :documentation 10))
  type-error)

(deftest defpackage-documentation.8
  (documentation
    (lisp-system::defpackage
      'defpackage-documentation-8
      :documentation "Hello"
      :documentation "OK")
    t)
  "Hello")


;;
;;  use
;;
(deftest defpackage-use.1
  ;; make
  (progn
    (defpackage defpackage-use-1 (:export aaa))
    (defpackage defpackage-use-2 (:use defpackage-use-1 common-lisp))
    (values
      (find-symbol-list "AAA" 'defpackage-use-2)
      (find-symbol-list "CAR" 'defpackage-use-2)))
  ("DEFPACKAGE-USE-1" "AAA" :inherited)
  ("COMMON-LISP" "CAR" :inherited))

(deftest defpackage-use.2
  ;; update
  (progn
    (defpackage defpackage-use-1 (:export aaa))
    (defpackage defpackage-use-2 (:use defpackage-use-1 common-lisp))
    (values
      (find-symbol-list "AAA" 'defpackage-use-2)
      (find-symbol-list "CAR" 'defpackage-use-2)))
  ("DEFPACKAGE-USE-1" "AAA" :inherited)
  ("COMMON-LISP" "CAR" :inherited))

(deftest defpackage-use.3
  (sort (mapcar #'package-name (package-use-list 'defpackage-use-2))
        #'string<)
  ("COMMON-LISP" "DEFPACKAGE-USE-1"))

(deftest defpackage-use.4
  (packagep
    (defpackage defpackage-use-3 (:use "COMMON-LISP")))
  t)

(deftest defpackage-use.5
  (packagep
    (defpackage defpackage-use-3 (:use #:common-lisp)))
  t)

(deftest-error defpackage-use.6
  (eval '(defpackage defpackage-use-3 (:use 100)))
  type-error)


;;
;;  shadow
;;
(deftest defpackage-shadow.1
  (progn
    (defpackage defpackage-shadow-1 (:shadow aaa bbb ccc))
    (package-shadowing-symbols-name 'defpackage-shadow-1))
  ("AAA" "BBB" "CCC"))

(deftest defpackage-shadow.2
  (progn
    (defpackage defpackage-shadow-2 (:shadow aaa #:aaa :aaa))
    (package-shadowing-symbols-name 'defpackage-shadow-2))
  ("AAA"))

(deftest-error defpackage-shadow.3
  (eval '(defpackage defpackage-shadow-3 (:shadow 100)))
  type-error)

(deftest-error! defpackage-shadow.4
  (eval '(defpackage defpackage-shadow-3
           (:shadow aaa)
           (:intern aaa)))
  package-error)

(deftest-error! defpackage-shadow.5
  (eval '(defpackage defpackage-shadow-3
           (:shadow aaa)
           (:import-from hello aaa)))
  package-error)

(deftest-error! defpackage-shadow.6
  (eval '(defpackage defpackage-shadow-3
           (:shadow aaa)
           (:shadowing-import-from hello aaa)))
  package-error)


;;
;;  shadowing-import-from
;;
(deftest defpackage-shadowing.1
  (progn
    (defpackage defpackage-shadowing-1 (:intern aaa bbb ccc))
    (defpackage defpackage-shadowing-2
      (:shadowing-import-from defpackage-shadowing-1 bbb ccc))
    (package-shadowing-symbols-name 'defpackage-shadowing-2))
  ("BBB" "CCC"))

(deftest-error defpackage-shadowing.2
  (eval '(defpackage defpackage-shadowing-3
           (:shadowing-import-from 100 aaa)
           (:import-from hello2 aaa)))
  type-error)

(deftest-error defpackage-shadowing.3
  (eval '(defpackage defpackage-shadowing-3
           (:shadowing-import-from hello1 aaa 200 ccc)
           (:import-from hello2 aaa)))
  type-error)

(deftest-error! defpackage-shadowing.4
  (eval '(defpackage defpackage-shadowing-3
           (:shadowing-import-from hello1 aaa)
           (:import-from hello2 aaa)))
  package-error)

(deftest-error! defpackage-shadowing.5
  (eval '(defpackage defpackage-shadowing-3
           (:shadowing-import-from hello1 aaa)
           (:intern aaa bbb ccc)))
  package-error)

(deftest-error defpackage-shadowing.6
  (progn
    (defpackage defpackage-shadowing-4 (:intern aaa bbb ccc))
    (defpackage defpackage-shadowing-5
      (:shadowing-import-from defpackage-shadowing-4 error)))
  package-error)

(deftest defpackage-shadowing.7
  (handler-bind ((package-error #'continue))
    (defpackage defpackage-shadowing-6 (:intern aaa bbb ccc))
    (defpackage defpackage-shadowing-7
      (:shadowing-import-from defpackage-shadowing-6 error))
    (find-symbol-list "ERROR" 'defpackage-shadowing-6))
  ("DEFPACKAGE-SHADOWING-6" "ERROR" :internal))


;;
;;  import-from
;;
(deftest defpackage-import.1
  (progn
    (defpackage defpackage-import-1 (:intern aaa bbb ccc))
    (defpackage defpackage-import-2 (:import-from defpackage-import-1 ccc))
    (find-symbol-list "CCC" 'defpackage-import-2))
  ("DEFPACKAGE-IMPORT-1" "CCC" :internal))

(deftest-error defpackage-import.2
  (progn
    (defpackage defpackage-import-3 (:intern aaa bbb ccc))
    (defpackage defpackage-import-4 (:import-from defpackage-import-3 error)))
  package-error)

(deftest defpackage-import.3
  (handler-bind ((package-error #'continue))
    (defpackage defpackage-import-5 (:intern aaa bbb ccc))
    (defpackage defpackage-import-6 (:import-from defpackage-import-5 error))
    (find-symbol-list "ERROR" 'defpackage-import-5))
  ("DEFPACKAGE-IMPORT-5" "ERROR" :internal))

(deftest-error defpackage-import.4
  (eval '(defpackage defpackage-import-7 (:import-from 100 aaa)))
  type-error)

(deftest-error defpackage-import.5
  (eval '(defpackage defpackage-import-7
           (:import-from defpackage-import-3 200)))
  type-error)

(deftest-error! defpackage-import.6
  (eval '(defpackage defpackage-import-7
           (:import-from hello aaa)
           (:intern aaa)))
  package-error)


;;
;;  export
;;
(deftest defpackage-export.1
  (progn
    (defpackage defpackage-export-1 (:export aaa bbb))
    (find-symbol-list "BBB" 'defpackage-export-1))
  ("DEFPACKAGE-EXPORT-1" "BBB" :external))

(deftest-error defpackage-export.2
  (eval '(defpackage defpackage-export-2 (:export aaa 200 ccc)))
  type-error)

(deftest-error! defpackage-export.3
  (eval '(defpackage defpackage-export-3
           (:export aaa bbb ccc)
           (:intern ccc)))
  package-error)


;;
;;  intern
;;
(deftest defpackage-intern.1
  (progn
    (defpackage defpackage-intern-1 (:intern aaa bbb))
    (defpackage defpackage-intern-2 (:import-from defpackage-intern-1 aaa))
    (find-symbol-list "AAA" 'defpackage-intern-2))
  ("DEFPACKAGE-INTERN-1" "AAA" :internal))

(deftest defpackage-intern.2
  (progn
    (defpackage defpackage-intern-3 (:intern aaa))
    (find-symbol-list "AAA" 'defpackage-intern-3))
  ("DEFPACKAGE-INTERN-3" "AAA" :internal))


;;  size
(deftest defpackage-size.1
  (packagep
    (defpackage defpackage-size-1 (:size 10)))
  t)

(deftest-error defpackage-size.2
  (eval '(defpackage defpackage-size-1 (:size nil)))
  type-error)

(deftest defpackage-size.3
  (packagep
    (defpackage defpackage-size-1 (:size 0)))
  t)

(deftest-error defpackage-size.4
  (eval '(defpackage defpackage-size-1 (:size -1))))

(deftest-error defpackage-size.5
  (eval '(defpackage defpackage-size-1
           (:size 100)
           (:size 200)))
  program-error)


;;  error
(deftest-error defpackage-error.1
  (eval '(defpackage)))

(deftest-error defpackage-error.2
  (eval '(defpackage defpackage-error-1 hello))
  program-error)

(deftest-error defpackage-error.3
  (eval '(defpackage defpackage-error-1 (:hello 10 20 30)))
  program-error)

(deftest defpackage-error.4
  (progn
    (defpackage defpackage-shadow-error-1
      (:use :cl)
      (:shadow #:debug)
      (:export #:debug))
    (defpackage defpackage-shadow-error-2
      (:use :cl defpackage-shadow-error-1)
      (:shadowing-import-from defpackage-shadow-error-1 #:debug))
    (values
      (packagep (find-package 'defpackage-shadow-error-1))
      (packagep (find-package 'defpackage-shadow-error-2))
      (package-name
        (symbol-package
          (find-symbol "DEBUG" 'defpackage-shadow-error-1)))
      (package-name
        (symbol-package
          (find-symbol "DEBUG" 'defpackage-shadow-error-2)))))
  t t
  "DEFPACKAGE-SHADOW-ERROR-1"
  "DEFPACKAGE-SHADOW-ERROR-1")

(deftest defpackage-error.5
  (progn
    (defpackage defpackage-use-error-1
      (:use :cl)
      (:export #:char-code))
    (defpackage defpackage-use-error-2
      (:use :cl defpackage-use-error-1))
    (values
      (packagep (find-package 'defpackage-use-error-1))
      (packagep (find-package 'defpackage-use-error-2))))
  t t)


;;  test
(deftest defpackage-test.1
  ;; make
  (progn
    (defpackage defpackage-test-1 (:intern import1 import2))
    (packagep
      (defpackage defpackage-test-2
        (:nicknames defpackage-test-3 defpackage-test-4)
        (:use common-lisp)
        (:export :hello #:abc)
        (:intern aaa bbb "CCC")
        (:shadow ddd eee fff)
        (:import-from defpackage-test-1 import1)
        (:shadowing-import-from defpackage-test-1 import2)
        (:documentation "Hello")
        (:size 4))))
  t)

(deftest defpackage-test.2
  ;; update
  (progn
    (defpackage defpackage-test-1 (:intern import1 import2))
    (packagep
      (defpackage defpackage-test-2
        (:nicknames defpackage-test-3 defpackage-test-4)
        (:use common-lisp)
        (:export :hello #:abc)
        (:intern aaa bbb "CCC")
        (:shadow ddd eee fff)
        (:import-from defpackage-test-1 import1)
        (:shadowing-import-from defpackage-test-1 import2)
        (:documentation "Hello")
        (:size 4))))
  t)

