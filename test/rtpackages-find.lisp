;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Function FIND-SYMBOL
;;
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

(deftest find-symbol.5
  (progn
    (export (intern "FIND-SYMBOL-5" 'test1) 'test1)
    (multiple-value-bind (symbol status) (find-symbol "FIND-SYMBOL-5" 'test1)
      (values (symbol-name symbol) status)))
  "FIND-SYMBOL-5" :external)

(deftest-error find-symbol-error.1
  (eval '(find-symbol 10))
  type-error)

(deftest-error find-symbol-error.2
  (eval '(find-symbol "HELLO" 20))
  type-error)

(deftest-error! find-symbol-error.3
  (eval '(find-symbol)))

(deftest-error! find-symbol-error.4
  (eval '(find-symbol "HELLO" *package* 30)))

(deftest-error find-symbol-error.5
  (eval '(find-symbol "HELLO" 'no-such-package-name)))

;;  ANSI Common Lisp
(deftest find-symbol-test.1
  (find-symbol "NEVER-BEFORE-USED")
  nil nil)

(deftest find-symbol-test.2
  (find-symbol "NEVER-BEFORE-USED")
  nil nil)

(deftest find-symbol-test.3
  (multiple-value-bind (x y) (intern "NEVER-BEFORE-USED")
    (values (symbol-name x) y))
  "NEVER-BEFORE-USED" nil)

(deftest find-symbol-test.4
  (multiple-value-bind (x y) (intern "NEVER-BEFORE-USED")
    (values (symbol-name x) y))
  "NEVER-BEFORE-USED" :internal)

(deftest find-symbol-test.5
  (multiple-value-bind (x y) (find-symbol "NEVER-BEFORE-USED")
    (values (symbol-name x) y))
  "NEVER-BEFORE-USED" :internal)

(deftest find-symbol-test.6
  (find-symbol "never-before-used")
  nil nil)

(deftest find-symbol-test.7
  (find-symbol "CAR" 'common-lisp-user)
  car :inherited)

(deftest find-symbol-test.8
  (find-symbol "CAR" 'common-lisp)
  car :external)

(deftest find-symbol-test.9
  (find-symbol "NIL" 'common-lisp-user)
  nil :inherited)

(deftest find-symbol-test.10
  (find-symbol "NIL" 'common-lisp)
  nil :external)

(deftest find-symbol-test.11
  (multiple-value-bind (x y)
    (find-symbol "NIL" (prog1 (make-package "FIND-SYMBOL-JUST-TESTING" :use '())
                         (intern "NIL" "FIND-SYMBOL-JUST-TESTING")))
    (values (package-name (symbol-package x))
            (symbol-name x)
            y))
  "FIND-SYMBOL-JUST-TESTING" "NIL" :internal)

(deftest find-symbol-test.12
  (progn
    (export (intern "NIL" "FIND-SYMBOL-JUST-TESTING")
            'find-symbol-just-testing)
    (multiple-value-bind (x y) (find-symbol "NIL" 'find-symbol-just-testing)
      (values (package-name (symbol-package x))
              (symbol-name x)
              y)))
  "FIND-SYMBOL-JUST-TESTING" "NIL" :external)

(deftest find-symbol-test.13
  (let ((x (find-symbol-list "NIL" "KEYWORD")))
    (or (null x)
        (equal x '("KEYWORD" "NIL" :external))))
  t)

(deftest find-symbol-test.14
  (progn
    (read-from-string ":nil")
    (multiple-value-bind (x y) (find-symbol "NIL" "KEYWORD")
      (values (package-name (symbol-package x))
              (symbol-name x)
              y)))
  "KEYWORD" "NIL" :external)


;;
;;  Function FIND-ALL-SYMBOLS
;;
(deftest find-all-symbols.1
  (set-exclusive-or
    (find-all-symbols "COMMON-LISP")
    '(common-lisp :common-lisp))
  nil)

(deftest find-all-symbols.2
  (find-all-symbols "NO-SUCH-SYMBOL-ALL-TEST")
  nil)

(deftest find-all-symbols.3
  (null
    (find-all-symbols "CAR"))
  nil)

(deftest find-all-symbols.4
  (symbolp
    (car (find-all-symbols "CAR")))
  t)

(deftest find-all-symbols.5
  (null
    (find-all-symbols 'car))
  nil)

(deftest find-all-symbols.6
  (null
    (find-all-symbols (intern "CDR" "KEYWORD")))
  nil)

(deftest find-all-symbols.7
  (let ((x (intern "CAR" (make-package 'temp1 :use nil))))
    (null
      (find x (find-all-symbols 'car) :test 'eq)))
  nil)

(deftest-error find-all-symbols-error.1
  (eval '(find-all-symbols 10))
  type-error)

(deftest-error! find-all-symbols-error.2
  (eval '(find-all-symbols)))

(deftest-error! find-all-symbols-error.3
  (eval '(find-all-symbols 'hello 'hello)))


;;
;;
;;
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
    (make-package 'aaa)
    (export (intern "X" 'aaa) 'aaa)
    (do-external-symbols (v 'aaa)
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

