;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Function EXPORT
;;
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

(deftest intern-error.1
  (let ((x (intern "KEYWORD-TEST" "KEYWORD")))
    (values
      (symbol-name
        (symbol-value x))
      (package-name
        (symbol-package x))))
  "KEYWORD-TEST" "KEYWORD")

