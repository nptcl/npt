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


;;  nicknames
(deftest defpackage.4
  (progn
    (defpackage defpackage-4 (:nicknames defpackage-4-1 defpackage-4-2))
    (values
      (packagep (find-package 'defpackage-4))
      (packagep (find-package 'defpackage-4-1))
      (packagep (find-package 'defpackage-4-2))))
  t t t)


;;  shadow
(deftest defpackage.5
  (progn
    (defpackage defpackage-5 (:shadow aaa))
    (symbol-name
      (car (package-shadowing-symbols 'defpackage-5))))
  "AAA")


;;  shadowing-import-from
(deftest defpackage.6
  (progn
    (defpackage defpackage-6)
    (intern "BBB" 'defpackage-6)
    (defpackage defpackage-6-1 (:shadowing-import-from defpackage-6 bbb))
    (symbol-name
      (car (package-shadowing-symbols 'defpackage-6-1))))
  "BBB")


;;  use
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


;;  export
(deftest defpackage.8
  (progn
    (defpackage defpackage-8 (:export aaa bbb))
    (multiple-value-bind (symbol check) (find-symbol "BBB" 'defpackage-8)
      (values
        (symbol-name symbol)
        (package-name (symbol-package symbol))
        check)))
  "BBB" "DEFPACKAGE-8" :external)


;;  intern
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


;;  size
;;  documentation
(deftest defpackage.11
  (packagep
    (defpackage defpackage-11 (:size 10) (:documentation "Hello")))
  t)

