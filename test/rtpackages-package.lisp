;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Function FIND-PACKAGE
;;
(deftest find-package.1
  (packagep
    (find-package 'common-lisp))
  t)

(deftest find-package.2
  (package-name
    (find-package "COMMON-LISP-USER"))
  "COMMON-LISP-USER")

(deftest find-package.3
  (find-package "NO-SUCH-PACKAGE")
  nil)

(deftest-error find-package-error.1
  (eval '(find-package 10))
  type-error)

(deftest-error! find-package-error.2
  (eval '(find-package)))

(deftest-error! find-package-error.3
  (eval '(find-package 'hello nil)))


;;
;;  Function LIST-ALL-PACKAGES
;;
(deftest list-all-packages.1
  (listp
    (list-all-packages))
  t)

(deftest list-all-packages.2
  (package-name
    (find "COMMON-LISP" (list-all-packages) :key #'package-name :test 'equal))
  "COMMON-LISP")

(deftest list-all-packages.3
  (let ((before (list-all-packages)))
    (make-package 'list-all-package-1)
    (mapcar
      #'package-name
      (set-difference (list-all-packages) before)))
  ("LIST-ALL-PACKAGE-1"))

(deftest-error! list-all-packages-error.1
  (eval '(list-all-package 10)))


;;
;;  Function RENAME-PACKAGE
;;
(deftest rename-package.1
  (let ((result (rename-package 'test3 'test3-rename)))
    (rename-package 'test3-rename 'test3)
    (packagep result))
  t)

(deftest rename-package.2
  (let ((result (rename-package "TEST3" "TEST3-RENAME")))
    (rename-package 'test3-rename 'test3)
    (packagep result))
  t)

(deftest rename-package.3
  (let ((package (find-package 'test3)))
    (rename-package 'test3 'test3-rename)
    (prog1 (package-name package)
      (rename-package 'test3-rename 'test3)))
  "TEST3-RENAME")

(deftest rename-package.4
  (progn
    (rename-package 'test3 'test3-rename '(test3-aaa test3-bbb test3-ccc))
    (let ((result1 (find-package 'test3-bbb)))
      (rename-package 'test3-rename 'test3)
      (let ((result2 (find-package 'test3-bbb)))
        (values (packagep result1) (packagep result2)))))
  t nil)

(deftest rename-package.5
  (progn
    (rename-package 'test3 'test3-rename '(test3-aaa "TEST3-BBB" test3-ccc))
    (let ((result1 (find-package 'test3-bbb)))
      (rename-package 'test3-rename 'test3)
      (let ((result2 (find-package 'test3-bbb)))
        (values (packagep result1) (packagep result2)))))
  t nil)

(deftest-error rename-package-error.1
  (eval '(rename-package 10 'hello))
  type-error)

(deftest-error rename-package-error.2
  (eval '(rename-package 'hello 20))
  type-error)

(deftest-error rename-package-error.3
  (rename-package 'no-such-package-name 'hello))

(deftest-error rename-package-error.4
  (eval '(rename-package 'test3 'test3-rename 10)))

(deftest-error rename-package-error.5
  (rename-package 'test3 'test3-rename '(10 20 30)))

(deftest-error rename-package-error.6
  (rename-package 'test3 'test2))

(deftest-error rename-package-error.7
  (progn
    (rename-package 'test2 'test2 '(test2-hello))
    (rename-package 'test3 'test2-hello)))

(deftest-error rename-package-error.8
  (rename-package 'test3 'test3 '(test2-hello)))

(deftest rename-package-error.9
  (progn
    (rename-package 'test2 'test2)
    (values)))

;;  ANSI Common Lisp
(deftest rename-package-test.1
  (progn
    (make-package 'rename-pacakge-temporary :nicknames '("RENAME-PACKAGE-TEMP"))
    (rename-package 'rename-package-temp 'rename-package-ephemeral)
    (package-nicknames (find-package 'rename-package-ephemeral)))
  nil)

(deftest rename-package-test.2
  (find-package 'rename-package-temporary)
  nil)

(deftest rename-package-test.3
  (progn
    (rename-package 'rename-package-ephemeral 'rename-package-temporary
                    '(rename-package-temp rename-package-fleeting))
    (sort (package-nicknames (find-package 'rename-package-temp))
          #'string<))
  ("RENAME-PACKAGE-FLEETING" "RENAME-PACKAGE-TEMP"))


;;
;;  Function DELETE-PACKAGE
;;
(deftest delete-package.1
  (progn
    (make-package 'delete-package-1)
    (delete-package "DELETE-PACKAGE-1"))
  t)

(deftest delete-package.2
  (let ((package (make-package 'delete-package-2)))
    (delete-package package)
    (values (packagep package) (package-name package)))
  t nil)

(deftest delete-package.3
  (let ((package (make-package 'delete-package-3)))
    (delete-package package)
    (delete-package package))
  nil)

(deftest delete-package.4
  (progn
    (make-package
      'delete-package-4 :nicknames '(delete-package-4-1 delete-package-4-2))
    (delete-package 'delete-package-4-1)
    (find-package 'delete-package-4))
  nil)

(deftest delete-package.5
  (progn
    (make-package 'delete-package-5)
    (let ((x (intern "HELLO" 'delete-package-5)))
      (delete-package 'delete-package-5)
      (symbol-package x)))
  nil)


(deftest-error delete-package-name.1
  (delete-package 'no-such-package-name)
  package-error)

(deftest delete-package-name.2
  (handler-bind ((package-error #'continue))
    (delete-package 'no-such-package-name))
  nil)

(defun delete-package-if (&rest args)
  (dolist (x args)
    (if (find-package x)
      (delete-package x))))

(deftest-error delete-package-used.1
  (progn
    (make-package 'delete-package-used-1)
    (make-package 'delete-package-used-2 :use '(delete-package-used-1))
    (make-package 'delete-package-used-3 :use '(delete-package-used-1))
    (delete-package 'delete-package-used-1))
  package-error)

(deftest delete-package-used.2
  (handler-bind ((package-error #'continue))
    (make-package 'delete-package-used-4)
    (make-package 'delete-package-used-5 :use '(delete-package-used-4))
    (make-package 'delete-package-used-6 :use '(delete-package-used-4))
    (delete-package 'delete-package-used-4))
  t)

(deftest-error delete-package-error.1
  (eval '(delete-package 10))
  type-error)

(deftest-error! delete-package-error.2
  (eval '(delete-package)))

(deftest-error! delete-package-error.3
  (eval '(delete-package 'hello 'hello)))

;;  ANSI Common Lisp
(defvar *delete-package-foo-package*)
(defvar *delete-package-foo-symbol*)
(defvar *delete-package-bar-package*)
(defvar *delete-package-bar-symbol*)
(defvar *delete-pacakge-baz-package*)

(deftest delete-package-test.1
  (progn
    (setq *delete-package-foo-package*
          (make-package "DELETE-PACKAGE-FOO" :use nil))
    (setq *delete-package-foo-symbol*
          (intern "FOO" *delete-package-foo-package*))
    (export *delete-package-foo-symbol* *delete-package-foo-package*)

    (setq *delete-package-bar-package*
          (make-package "DELETE-PACKAGE-BAR" :use '("DELETE-PACKAGE-FOO")))
    (setq *delete-package-bar-symbol*
          (intern "BAR" *delete-package-bar-package*))
    (export *delete-package-foo-symbol* *delete-package-bar-package*)
    (export *delete-package-bar-symbol* *delete-package-bar-package*)

    (setq *delete-pacakge-baz-package*
          (make-package "DELETE-PACKAGE-BAZ" :use '("DELETE-PACKAGE-BAR")))

    (package-name
      (symbol-package *delete-package-foo-symbol*)))
  "DELETE-PACKAGE-FOO")

(deftest delete-package-test.2
  (package-name
    (symbol-package *delete-package-bar-symbol*))
  "DELETE-PACKAGE-BAR")

(deftest delete-package-test.3
  (prin1-to-string *delete-package-foo-symbol*)
  "DELETE-PACKAGE-FOO:FOO")

(deftest delete-package-test.4
  (prin1-to-string *delete-package-bar-symbol*)
  "DELETE-PACKAGE-BAR:BAR")

(deftest delete-package-test.5
  (multiple-value-bind (x y)
    (find-symbol "FOO" *delete-package-bar-package*)
    (values (prin1-to-string x) y))
  "DELETE-PACKAGE-FOO:FOO" :external)

(deftest delete-package-test.6
  (multiple-value-bind (x y)
    (find-symbol "FOO" *delete-pacakge-baz-package*)
    (values (prin1-to-string x) y))
  "DELETE-PACKAGE-FOO:FOO" :inherited)

(deftest delete-package-test.7
  (multiple-value-bind (x y)
    (find-symbol "BAR" *delete-pacakge-baz-package*)
    (values (prin1-to-string x) y))
  "DELETE-PACKAGE-BAR:BAR" :inherited)

(deftest delete-package-test.8
  (values
    (packagep *delete-package-foo-package*)
    (packagep *delete-package-bar-package*)
    (packagep *delete-pacakge-baz-package*))
  t t t)

(deftest delete-package-test.9
  (values
    (package-name *delete-package-foo-package*)
    (package-name *delete-package-bar-package*)
    (package-name *delete-pacakge-baz-package*))
  "DELETE-PACKAGE-FOO"
  "DELETE-PACKAGE-BAR"
  "DELETE-PACKAGE-BAZ")

(deftest delete-package-test.10
  (values
    (mapcar #'package-name
            (package-use-list *delete-package-foo-package*))
    (mapcar #'package-name
            (package-use-list *delete-package-bar-package*))
    (mapcar #'package-name
            (package-use-list *delete-pacakge-baz-package*)))
  () ("DELETE-PACKAGE-FOO") ("DELETE-PACKAGE-BAR"))

(deftest delete-package-test.11
  (values
    (mapcar #'package-name
            (package-used-by-list *delete-package-foo-package*))
    (mapcar #'package-name
            (package-used-by-list *delete-package-bar-package*))
    (mapcar #'package-name
            (package-used-by-list *delete-pacakge-baz-package*)))
  ("DELETE-PACKAGE-BAR") ("DELETE-PACKAGE-BAZ") ())

(deftest-error delete-package-test.12
  (delete-package *delete-package-bar-package*))

(deftest delete-package-test.13
  (handler-bind ((package-error #'continue))
    (delete-package *delete-package-bar-package*))
  t)

(deftest delete-package-test.14
  (package-name
    (symbol-package *delete-package-foo-symbol*))
  "DELETE-PACKAGE-FOO")

(deftest delete-package-test.15
  (symbol-package *delete-package-bar-symbol*)
  nil)

(deftest delete-package-test.16
  (prin1-to-string *delete-package-foo-symbol*)
  "DELETE-PACKAGE-FOO:FOO")

(deftest delete-package-test.17
  (prin1-to-string *delete-package-bar-symbol*)
  "#:BAR")

(deftest delete-package-test.18
  (find-symbol "DELETE-PACKAGE-FOO" *delete-package-bar-package*)
  nil nil)

(deftest delete-package-test.19
  (find-symbol "DELETE-PACKAGE-FOO" *delete-pacakge-baz-package*)
  nil nil)

(deftest delete-package-test.20
  (find-symbol "DELETE-PACKAGE-BAR" *delete-pacakge-baz-package*)
  nil nil)

(deftest delete-package-test.21
  (values
    (packagep *delete-package-foo-package*)
    (packagep *delete-package-bar-package*)
    (packagep *delete-pacakge-baz-package*))
  t t t)

(deftest delete-package-test.22
  (values
    (package-name *delete-package-foo-package*)
    (package-name *delete-package-bar-package*)
    (package-name *delete-pacakge-baz-package*))
  "DELETE-PACKAGE-FOO"
  nil
  "DELETE-PACKAGE-BAZ")

(deftest delete-package-test.23
  (values
    (package-use-list *delete-package-foo-package*)
    (package-use-list *delete-package-bar-package*)
    (package-use-list *delete-pacakge-baz-package*))
  nil nil nil)

(deftest delete-package-test.24
  (values
    (package-used-by-list *delete-package-foo-package*)
    (package-used-by-list *delete-package-bar-package*)
    (package-used-by-list *delete-pacakge-baz-package*))
  nil nil nil)


;;
;;
;;
(deftest make-package.1
  (progn
    (make-package 'make-package-1)
    (let ((result (packagep (find-package 'make-package-1))))
      (delete-package 'make-package-1)
      result))
  t)

(deftest make-package.2
  (progn
    (make-package
      'make-package-2 :nicknames
      '(make-package-2-1 make-package-2-2 make-package-2-3 make-package-2-4))
    (let ((result (packagep (find-package 'make-package-2-2))))
      (delete-package 'make-package-2)
      result))
  t)

(deftest make-package.3
  (let ((p1 (make-package 'make-package-3-1)))
    (export (intern "HELLO" p1) p1)
    (let ((p2 (make-package 'make-package-3-2 :use (list p1))))
      (multiple-value-bind (symbol status) (find-symbol "HELLO" p2)
        (values (symbol-name symbol)
                (package-name (symbol-package symbol))
                status))))
  "HELLO" "MAKE-PACKAGE-3-1" :inherited)

(deftest with-package-iterator.1
  (with-package-iterator
    (call *package* :internal)
    (multiple-value-bind (check symbol status package) (call)
      (values check (symbolp symbol) status (eq package *package*))))
  t t :internal t)

(deftest in-package.1
  (let (name)
    (in-package test1)
    (setq name (package-name *package*))
    (in-package common-lisp-user)
    name)
  "TEST1")

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
    (packagep (find-package 'defpackage-1)))
  t)

(deftest defpackage.4
  (progn
    (defpackage defpackage-4 (:nicknames defpackage-4-1 defpackage-4-2))
    (values
      (packagep (find-package 'defpackage-4))
      (packagep (find-package 'defpackage-4-1))
      (packagep (find-package 'defpackage-4-2))))
  t t t)

(deftest defpackage.5
  (progn
    (defpackage defpackage-5 (:shadow aaa))
    (symbol-name
      (car (package-shadowing-symbols 'defpackage-5))))
  "AAA")

(deftest defpackage.6
  (progn
    (defpackage defpackage-6)
    (intern "BBB" 'defpackage-6)
    (defpackage defpackage-6-1 (:shadowing-import-from defpackage-6 bbb))
    (symbol-name
      (car (package-shadowing-symbols 'defpackage-6-1))))
  "BBB")

(deftest defpackage.7
  (progn
    (defpackage defpackage-7 (:export aaa))
    (defpackage defpackage-7-1 (:use defpackage-7 common-lisp))
    (multiple-value-bind (symbol check) (find-symbol "AAA" 'defpackage-7-1)
      (values
        (symbol-name symbol)
        (package-name (symbol-package symbol))
        check)))
  "AAA" "DEFPACKAGE-7" :inherited)

(deftest defpackage.8
  (progn
    (defpackage defpackage-8 (:export aaa bbb))
    (multiple-value-bind (symbol check) (find-symbol "BBB" 'defpackage-8)
      (values
        (symbol-name symbol)
        (package-name (symbol-package symbol))
        check)))
  "BBB" "DEFPACKAGE-8" :external)

(deftest defpackage.9
  (progn
    (defpackage defpackage-9 (:intern aaa bbb))
    (defpackage defpackage-9-1 (:import-from defpackage-9 aaa))
    (multiple-value-bind (symbol check) (find-symbol "AAA" 'defpackage-9-1)
      (values
        (symbol-name symbol)
        (package-name (symbol-package symbol))
        check)))
  "AAA" "DEFPACKAGE-9" :internal)

(deftest defpackage.10
  (progn
    (defpackage defpackage-10 (:intern aaa))
    (multiple-value-bind (symbol check) (find-symbol "AAA" 'defpackage-10)
      (values
        (symbol-name symbol)
        (package-name (symbol-package symbol))
        check)))
  "AAA" "DEFPACKAGE-10" :internal)

(deftest defpackage.11
  (packagep
    (defpackage defpackage-11 (:size 10) (:documentation "Hello")))
  t)

