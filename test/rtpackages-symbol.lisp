;;
;;  ANSI COMMON LISP: 11. Packages
;;
(defun find-symbol-list (x &optional (y *package*))
  (multiple-value-bind (symbol status) (find-symbol x y)
    (when status
      (list (package-name
              (symbol-package symbol))
            (symbol-name symbol)
            status))))

(defun package-shadowing-symbols-list (package)
  (mapcar
    (lambda (x)
      (list (package-name (symbol-package x))
            (symbol-name x)))
    (package-shadowing-symbols package)))


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
    (find-symbol-list "EXPORT2" 'test1))
  ("TEST1" "EXPORT2" :external))

(deftest export.3
  (progn
    (intern "EXPORT3" 'test1)
    (find-symbol-list "EXPORT3" 'test1))
  ("TEST1" "EXPORT3" :internal))

(deftest export.4
  (let ((x (intern "EXPORT4" 'test1)))
    (export x 'test1)
    (export x 'test1)
    (export x 'test1))
  t)

(deftest export.5
  (let ((x (intern "EXPORT5" 'test1)))
    (export x 'test1)
    (export x 'test1)
    (export x 'test1)
    (find-symbol-list "EXPORT5" 'test1))
  ("TEST1" "EXPORT5" :external))

;;  list
(deftest export-list.1
  (let ((x (list (intern "EXPORT-LIST-1A" 'test1)
                 (intern "EXPORT-LIST-1B" 'test1)
                 (intern "EXPORT-LIST-1C" 'test1))))
    (export x 'test1))
  t)

(deftest export-list.2
  (let ((x (list (intern "EXPORT-LIST-2A" 'test1)
                 (intern "EXPORT-LIST-2B" 'test1)
                 (intern "EXPORT-LIST-2C" 'test1))))
    (export x 'test1)
    (mapcar
      (lambda (symbol)
        (find-symbol-list (symbol-name symbol) 'test1))
      x))
  (("TEST1" "EXPORT-LIST-2A" :external)
   ("TEST1" "EXPORT-LIST-2B" :external)
   ("TEST1" "EXPORT-LIST-2C" :external)))

;;  package
(deftest export-package.1
  (progn
    (let ((*package* (find-package 'test2)))
      (export (intern "EXPORT-PACKAGE-1")))
    (find-symbol-list "EXPORT-PACKAGE-1" 'test2))
  ("TEST2" "EXPORT-PACKAGE-1" :external))

(deftest export-package.2
  (progn
    (export (intern "EXPORT-PACKAGE-2" 'test2) 'test2)
    (find-symbol-list "EXPORT-PACKAGE-2" 'test2))
  ("TEST2" "EXPORT-PACKAGE-2" :external))


;;  accessible
(deftest-error export-access.1
  (export (intern "EXPORT-ACCESS-1" 'test1) 'test2)
  package-error)

(deftest export-access.2
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'lisp-system::ignore c))))
    (export (intern "EXPORT-ACCESS-2" 'test1) 'test2))
  t)

(deftest export-access.3
  (values
    (find-symbol-list "EXPORT-ACCESS-2" 'test1)
    (find-symbol-list "EXPORT-ACCESS-2" 'test2))
  ("TEST1" "EXPORT-ACCESS-2" :internal)
  nil)

(deftest export-access.4
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'import c))))
    (export (intern "EXPORT-ACCESS-4" 'test1) 'test2))
  t)

(deftest export-access.5
  (values
    (find-symbol-list "EXPORT-ACCESS-4" 'test1)
    (find-symbol-list "EXPORT-ACCESS-4" 'test2))
  ("TEST1" "EXPORT-ACCESS-4" :internal)
  ("TEST1" "EXPORT-ACCESS-4" :external))


;;  conflict
(deftest-error export-conflict.1
  (progn
    (make-package 'export-conflict-1a)
    (make-package 'export-conflict-1b)
    (intern "X" 'export-conflict-1a)
    (intern "X" 'export-conflict-1b)
    (use-package 'export-conflict-1a 'export-conflict-1b)
    (export (intern "X" 'export-conflict-1a) 'export-conflict-1a))
  package-error)

(deftest export-conflict.2
  (progn
    (handler-bind ((package-error
                     (lambda (c)
                       (invoke-restart 'lisp-system::ignore c))))
      (make-package 'export-conflict-2a)
      (make-package 'export-conflict-2b)
      (intern "Y" 'export-conflict-2a)
      (intern "Y" 'export-conflict-2b)
      (use-package 'export-conflict-2a 'export-conflict-2b)
      (export (intern "Y" 'export-conflict-2a) 'export-conflict-2a))
    (values (find-symbol-list "Y" 'export-conflict-2a)
            (find-symbol-list "Y" 'export-conflict-2b)))
  ("EXPORT-CONFLICT-2A" "Y" :internal)
  ("EXPORT-CONFLICT-2B" "Y" :internal))

(deftest export-conflict.3
  (progn
    (handler-bind ((package-error
                     (lambda (c)
                       (invoke-restart 'shadow c))))
      (make-package 'export-conflict-3a)
      (make-package 'export-conflict-3b)
      (intern "Y" 'export-conflict-3a)
      (intern "Y" 'export-conflict-3b)
      (use-package 'export-conflict-3a 'export-conflict-3b)
      (export (intern "Y" 'export-conflict-3a) 'export-conflict-3a))
    (values (find-symbol-list "Y" 'export-conflict-3a)
            (find-symbol-list "Y" 'export-conflict-3b)
            (package-shadowing-symbols-list 'export-conflict-3a)
            (package-shadowing-symbols-list 'export-conflict-3b)))
  ("EXPORT-CONFLICT-3A" "Y" :external)
  ("EXPORT-CONFLICT-3B" "Y" :internal)
  nil
  (("EXPORT-CONFLICT-3B" "Y")))

(deftest export-conflict.4
  (progn
    (handler-bind ((package-error
                     (lambda (c)
                       (invoke-restart 'unintern c))))
      (make-package 'export-conflict-4a)
      (make-package 'export-conflict-4b)
      (intern "Y" 'export-conflict-4a)
      (intern "Y" 'export-conflict-4b)
      (use-package 'export-conflict-4a 'export-conflict-4b)
      (export (intern "Y" 'export-conflict-4a) 'export-conflict-4a))
    (values (find-symbol-list "Y" 'export-conflict-4a)
            (find-symbol-list "Y" 'export-conflict-4b)
            (package-shadowing-symbols-list 'export-conflict-4a)
            (package-shadowing-symbols-list 'export-conflict-4b)))
  ("EXPORT-CONFLICT-4A" "Y" :external)
  ("EXPORT-CONFLICT-4A" "Y" :inherited)
  nil
  nil)

;;  error
(deftest-error export-error.1
  (eval '(export 10))
  type-error)

(deftest-error export-error.2
  (eval '(export 'hello 20))
  type-error)

(deftest-error export-error.3
  (eval '(export '(10 20 30)))
  type-error)

(deftest-error! export-error.4
  (eval '(export)))

(deftest-error! export-error.5
  (eval '(export 'hello *package* 40)))

(deftest-error! export-error.6
  (eval '(export 'hello *package* 40)))

(deftest-error! export-error.7
  (eval '(export 'hello 'no-such-package-name)))


;;  ANSI Common Lisp
(deftest export-test.1
  (progn
    (make-package 'export-test-0 :use nil)
    (package-name
      (make-package 'export-test-1 :use nil)))
  "EXPORT-TEST-1")

(deftest export-test.2
  (use-package 'export-test-1 'export-test-0)
  t)

(deftest export-test.3
  (multiple-value-bind (x y) (intern "TEMP-SYM" 'export-test-1)
    (values (package-name (symbol-package x))
            (symbol-name x)
            y))
  "EXPORT-TEST-1" "TEMP-SYM" nil)

(deftest export-test.4
  (find-symbol "TEMP-SYM")
  nil nil)

(deftest export-test.5
  (find-symbol "TEMP-SYM" 'export-test-0)
  nil nil)

(deftest export-test.6
  (export (find-symbol "TEMP-SYM" 'export-test-1) 'export-test-1)
  t)

(deftest export-test.7
  (multiple-value-bind (x y) (find-symbol "TEMP-SYM" 'export-test-0)
    (values (symbolp x) y))
  t :inherited)



;;
;;
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

(deftest intern-error.1
  (let ((x (intern "KEYWORD-TEST" "KEYWORD")))
    (values
      (symbol-name
        (symbol-value x))
      (package-name
        (symbol-package x))))
  "KEYWORD-TEST" "KEYWORD")

