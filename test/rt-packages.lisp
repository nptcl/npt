;;
;;  ANSI COMMON LISP: 11. Packages
;;
(make-package 'test1)
(make-package 'test2)
(make-package 'test3)

(deftest export.1
  (let ((symbol (intern "EXPORT1" 'test1)))
    (export symbol 'test1))
  t)

(deftest export.2
  (let ((symbol (intern "EXPORT2" 'test1)))
    (export symbol 'test1)
    (multiple-value-bind (symbol status) (find-symbol "EXPORT2" 'test1)
      (values (symbol-name symbol) status)))
  "EXPORT2" :external)

(deftest export.3
  (progn
    (intern "EXPORT3" 'test1)
    (multiple-value-bind (symbol status) (find-symbol "EXPORT3" 'test1)
      (values (symbol-name symbol) status)))
  "EXPORT3" :internal)

(deftest export.4
  (progn
    (unintern (intern "EXPORT4"))
    (export (intern "EXPORT4")))
  t)

(deftest export.5
  (progn
    (unintern (intern "EXPORT5"))
    (export (list (intern "EXPORT5")))
    (multiple-value-bind (symbol status) (find-symbol "EXPORT5")
      (values (symbol-name symbol) status)))
  "EXPORT5" :external)

(deftest find-symbol.1
  (find-symbol "CAR")
  car :inherited)

(deftest find-symbol.2
  (find-symbol "NO-SUCH-SYMBOL")
  nil nil)

(deftest find-symbol.3
  (let ((symbol (intern "FIND-SYMBOL-3" 'test1)))
    (eq (find-symbol "FIND-SYMBOL-3" 'test1) symbol))
  t)

(deftest find-symbol.4
  (progn
    (intern "FIND-SYMBOL-4" 'test1)
    (multiple-value-bind (symbol status) (find-symbol "FIND-SYMBOL-4" 'test1)
      (values (symbol-name symbol) status)))
  "FIND-SYMBOL-4" :internal)

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

(deftest find-all-symbols.1
  (set-exclusive-or
    (find-all-symbols "COMMON-LISP")
    '(common-lisp :common-lisp))
  nil)

(deftest find-all-symbols.2
  (find-all-symbols "NO-SUCH-SYMBOL-ALL-TEST")
  nil)

(deftest import.1
  (let ((symbol (intern "IMPORT1" 'test1)))
    (import symbol 'test2))
  t)

(deftest import.2
  (progn
    (import (intern "IMPORT2" 'test1) 'test2)
    (multiple-value-bind (symbol status) (find-symbol "IMPORT2" 'test2)
      (values (symbol-name symbol)
              (package-name
                (symbol-package symbol))
              status)))
  "IMPORT2" "TEST1" :internal)

(deftest import.3
  (progn
    (import (intern "IMPORT3" 'test1))
    (multiple-value-bind (symbol status) (find-symbol "IMPORT3")
      (values (symbol-name symbol)
              (package-name
                (symbol-package symbol))
              status)))
  "IMPORT3" "TEST1" :internal)

(deftest import.4
  (progn
    (import (list (intern "IMPORT4" 'test1)))
    (multiple-value-bind (symbol status) (find-symbol "IMPORT4")
      (values (symbol-name symbol)
              (package-name
                (symbol-package symbol))
              status)))
  "IMPORT4" "TEST1" :internal)

(deftest list-all-packages.1
  (listp
    (list-all-packages))
  t)

(deftest list-all-packages.2
  (null
    (member "COMMON-LISP" (list-all-packages) :key #'package-name :test 'equal))
  nil)

(deftest rename-package.1
  (progn
    (find-package 'test3)
    (let ((result (rename-package 'test3 'test3-rename)))
      (rename-package 'test3-rename 'test3)
      (packagep result)))
  t)

(deftest rename-package.2
  (let ((package (find-package 'test3)))
    (rename-package 'test3 'test3-rename)
    (let ((name (package-name package)))
      (rename-package 'test3-rename 'test3)
      name))
  "TEST3-RENAME")

(deftest rename-package.3
  (progn
    (rename-package 'test3 'test3-rename '(test3-aaa test3-bbb test3-ccc))
    (let ((result1 (find-package 'test3-bbb)))
      (rename-package 'test3-rename 'test3)
      (let ((result2 (find-package 'test3-bbb)))
        (values (packagep result1) (packagep result2)))))
  t nil)

(deftest shadow.1
  (shadow "SHADOW1" 'test1)
  t)

(deftest shadow.2
  (shadow '("SHADOW2" "SHADOW2-2") 'test1)
  t)

(deftest shadow.3
  (shadow "SHADOW3")
  t)

(deftest shadow.4
  (let ((symbol (intern "SHADOW4")))
    (shadow "SHADOW4")
    (eq (car (member symbol (package-shadowing-symbols *package*)))
        symbol))
  t)

(deftest shadowing-import.1
  (shadowing-import (intern "SHADOWING-IMPORT1" 'test1) 'test2)
  t)

(deftest shadowing-import.2
  (shadowing-import
    (list (intern "SHADOWING-IMPORT2" 'test1) 'shadowing-import3)
    'test2)
  t)

(deftest shadowing-import.3
  (let ((symbol (intern "SHADOWING-IMPORT3" 'test1)))
    (shadowing-import (intern "SHADOWING-IMPORT3" 'test1) 'test2)
    (eq (car (member symbol (package-shadowing-symbols 'test2)))
        symbol))
  t)

(deftest shadowing-import.4
  (progn
    (shadowing-import (intern "SHADOWING-IMPORT4" 'test1) 'test2)
    (multiple-value-bind (symbol status) (find-symbol  "SHADOWING-IMPORT4" 'test2)
      (values (symbol-name symbol)
              (package-name (symbol-package symbol))
              status)))
  "SHADOWING-IMPORT4" "TEST1" :internal)

(deftest shadowing-import.5
  (shadowing-import (intern "SHADOWING-IMPORT5" 'test1))
  t)

(deftest shadowing-import.6
  (let ((symbol (intern "SHADOWING-IMPORT6" 'test1)))
    (shadowing-import (intern "SHADOWING-IMPORT6" 'test1))
    (eq (car (member symbol (package-shadowing-symbols *package*)))
        symbol))
  t)

(deftest shadowing-import.7
  (progn
    (shadowing-import (intern "SHADOWING-IMPORT7" 'test1))
    (multiple-value-bind (symbol status) (find-symbol  "SHADOWING-IMPORT7")
      (values (symbol-name symbol)
              (package-name (symbol-package symbol))
              status)))
  "SHADOWING-IMPORT7" "TEST1" :internal)

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

(deftest unexport.1
  (unexport (intern "UNEXPORT1" 'test1) 'test1)
  t)

(deftest unexport.2
  (unexport (list (intern "UNEXPORT2-1" 'test1)
                  (intern "UNEXPORT2-2" 'test1)) 'test1)
  t)

(deftest unexport.3
  (progn
    (make-package 'unexport3)
    (let ((symbol (intern "UNEXPORT3-1" 'unexport3)))
      (export symbol 'unexport3)
      (make-package 'unexport3-2 :use '(unexport3))
      (multiple-value-bind (symbol check)
        (find-symbol "UNEXPORT3-1" 'unexport3)
        (declare (ignore symbol))
        (unless (eq check :external)
          (error "find-symbol error")))
      (unexport symbol 'unexport3)
      (multiple-value-bind (symbol check)
        (find-symbol "UNEXPORT3-1" 'unexport3)
        (declare (ignore symbol))
        (eq check :external))))
  nil)

(deftest unexport.4
  (progn
    (export (intern "UNEXPORT4"))
    (unexport (intern "UNEXPORT4")))
  t)

(deftest unintern.1
  (unintern (intern "UNINTERN1" 'test1) 'test1)
  t)

(deftest unintern.2
  (let ((symbol (intern "UNINTERN2" 'test2)))
    (import symbol 'test1)
    (unintern symbol 'test1))
  t)

(deftest unintern.3
  (let ((symbol (intern "UNINTERN2" 'test2)))
    (unintern symbol 'test1))
  nil)

(deftest unintern.4
  (let ((symbol (intern "UNINTERN4" 'test1)))
    (unintern symbol 'test1)
    (find-symbol "UNINTERN4" 'test1))
  nil nil)

(deftest in-package.1
  (let (name)
    (in-package test1)
    (setq name (package-name *package*))
    (in-package common-lisp-user)
    name)
  "TEST1")

(deftest use-package.1
  (use-package 'test2 'test1)
  t)

(deftest use-package.2
  (progn
    (use-package 'test2 'test1)
    (use-package '(test2) 'test1))
  t)

(deftest use-package.3
  (progn
    (unuse-package 'test2 'test1)
    (let ((symbol (intern "USE-PACKAGE3" 'test2)))
      (export symbol 'test2))
    (use-package 'test2 'test1)
    (multiple-value-bind (symbol status) (find-symbol "USE-PACKAGE3" 'test2)
      (values (symbol-name symbol) status)))
  "USE-PACKAGE3" :external)

(deftest use-package.4
  (progn
    (unuse-package 'test2 'test1)
    (use-package 'test2 'test1)
    (car (member
           "TEST2"
           (mapcar #'package-name (package-use-list 'test1))
           :test 'equal)))
  "TEST2")

(deftest unuse-package.1
  (unuse-package 'test2 'test1)
  t)

(deftest unuse-package.2
  (unuse-package '(test2) 'test1)
  t)

(deftest unuse-package.3
  (progn
    (unuse-package 'test2 'test1)
    (use-package 'test2 'test1)
    (unuse-package 'test2 'test1)
    (car (member
           "TEST2"
           (mapcar #'package-name (package-use-list 'test1))
           :test 'equal)))
  nil)

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

(deftest do-symbols.1
  (let (a)
    (do-symbols (v)
      (declare (ignore v))
      (setq a t))
    a)
  t)

(deftest do-symbols.2
  (let ((a :hello))
    (do-symbols (v 'common-lisp-user a)
      (declare (ignore v))))
  :hello)

(deftest do-symbols.3
  (progn
    (defpackage do-symbols-3 (:use))
    (intern "AAA" 'do-symbols-3)
    (intern "BBB" 'do-symbols-3)
    (intern "CCC" 'do-symbols-3)
    (let ((count 0))
      (do-symbols
        (v 'do-symbols-3)
        (declare (ignorable v))
        (setq count (1+ count)))
      count))
  3)

(deftest do-symbols.4
  (progn
    (defpackage do-symbols-4 (:use))
    (intern "AAA" 'do-symbols-4)
    (intern "BBB" 'do-symbols-4)
    (intern "CCC" 'do-symbols-4)
    (let ((count 0))
      (do-symbols
        (v 'do-symbols-4 :hello)
        (declare (ignorable v))
        (setq count (1+ count)))))
  :hello)

(deftest do-external-symbols.1
  (let (a)
    (do-external-symbols (v)
      (declare (ignore v))
      (setq a t))
    a)
  t)

(deftest do-external-symbols.2
  (let ((a :hello))
    (do-external-symbols (v 'common-lisp-user a)
      (declare (ignore v))))
  :hello)

(deftest do-external-symbols.3
  (progn
    (defpackage do-external-symbols-3 (:use))
    (intern "AAA" 'do-external-symbols-3)
    (intern "BBB" 'do-external-symbols-3)
    (intern "CCC" 'do-external-symbols-3)
    (let ((count 0))
      (do-external-symbols
        (v 'do-external-symbols-3)
        (declare (ignorable v))
        (setq count (1+ count)))
      count))
  0)

(deftest do-external-symbols.4
  (progn
    (defpackage do-external-symbols-4 (:use))
    (intern "AAA" 'do-external-symbols-4)
    (intern "BBB" 'do-external-symbols-4)
    (intern "CCC" 'do-external-symbols-4)
    (export (intern "BBB" 'do-external-symbols-4) 'do-external-symbols-4)
    (export (intern "CCC" 'do-external-symbols-4) 'do-external-symbols-4)
    (let ((count 0))
      (do-external-symbols
        (v 'do-external-symbols-4)
        (declare (ignorable v))
        (setq count (1+ count)))
      count))
  2)

(deftest do-external-symbols.5
  (progn
    (defpackage do-external-symbols-5 (:use))
    (intern "AAA" 'do-external-symbols-5)
    (intern "BBB" 'do-external-symbols-5)
    (intern "CCC" 'do-external-symbols-5)
    (export (intern "BBB" 'do-external-symbols-5) 'do-external-symbols-5)
    (export (intern "CCC" 'do-external-symbols-5) 'do-external-symbols-5)
    (let ((count 0))
      (do-external-symbols
        (v 'do-external-symbols-5 :hello)
        (declare (ignorable v))
        (setq count (1+ count)))))
  :hello)

(deftest do-all-symbols.1
  (do-all-symbols (v)
    (declare (ignore v)))
  nil)

(deftest do-all-symbols.2
  (let ((a :hello))
    (do-all-symbols (v a)
      (declare (ignore v))))
  :hello)

(deftest package-error-package.1
  "TODO"
  "TODO")


;;
;;  do-tests
;;
(do-tests :test t)

