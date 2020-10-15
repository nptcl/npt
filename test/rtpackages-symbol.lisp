;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Function IMPORT
;;
(deftest import-init
  (init-test-package))

(deftest import.1
  (let ((symbol (intern "IMPORT1" 'test1)))
    (import symbol 'test2))
  t)

(deftest import.2
  (progn
    (import (intern "IMPORT2" 'test1) 'test2)
    (find-symbol-list "IMPORT2" 'test2))
  ("TEST1" "IMPORT2" :internal))

(deftest import.3
  (progn
    (import (intern "IMPORT3" 'test1))
    (find-symbol-list "IMPORT3"))
  ("TEST1" "IMPORT3" :internal))

(deftest import.4
  (progn
    (import (list (intern "IMPORT4" 'test1)))
    (find-symbol-list "IMPORT4"))
  ("TEST1" "IMPORT4" :internal))

(deftest import.5
  (progn
    (import (make-symbol "IMPORT5") 'test1)
    (find-symbol-list "IMPORT5" 'test1))
  ("TEST1" "IMPORT5" :internal))

(deftest import.6
  (progn
    (import (intern "IMPORT6" 'test2) 'test1)
    (export (intern "IMPORT6" 'test2) 'test1)
    (find-symbol-list "IMPORT6" 'test1))
  ("TEST2" "IMPORT6" :external))

(deftest import-list.1
  (let ((x (list (intern "IMPORT-LIST-1A" 'test1)
                 (intern "IMPORT-LIST-1B" 'test1)
                 (intern "IMPORT-LIST-1C" 'test1))))
    (import x 'test2))
  t)

(deftest import-list.2
  (let ((x (list (intern "IMPORT-LIST-2A" 'test1)
                 (intern "IMPORT-LIST-2B" 'test1)
                 (intern "IMPORT-LIST-2C" 'test1))))
    (import x 'test2)
    (mapcar
      (lambda (symbol)
        (find-symbol-list (symbol-name symbol) 'test2))
      x))
  (("TEST1" "IMPORT-LIST-2A" :internal)
   ("TEST1" "IMPORT-LIST-2B" :internal)
   ("TEST1" "IMPORT-LIST-2C" :internal)))

(deftest-error import-restart.1
  (progn
    (intern "IMPORT-RESTART-1" 'test1)
    (intern "IMPORT-RESTART-1" 'test2)
    (import (intern "IMPORT-RESTART-1" 'test2) 'test1))
  package-error)

(deftest import-restart.2
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'import c))))
    (intern "IMPORT-RESTART-2" 'test1)
    (intern "IMPORT-RESTART-2" 'test2)
    (import (intern "IMPORT-RESTART-2" 'test2) 'test1)
    (values
      (find-symbol-list "IMPORT-RESTART-2" 'test1)
      (find-symbol-list "IMPORT-RESTART-2" 'test2)))
  ("TEST2" "IMPORT-RESTART-2" :internal)
  ("TEST2" "IMPORT-RESTART-2" :internal))

(deftest import-restart.3
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'ignore c))))
    (intern "IMPORT-RESTART-3" 'test1)
    (intern "IMPORT-RESTART-3" 'test2)
    (import (intern "IMPORT-RESTART-3" 'test2) 'test1)
    (values
      (find-symbol-list "IMPORT-RESTART-3" 'test1)
      (find-symbol-list "IMPORT-RESTART-3" 'test2)))
  ("TEST1" "IMPORT-RESTART-3" :internal)
  ("TEST2" "IMPORT-RESTART-3" :internal))

(deftest-error import-restart.4
  (progn
    (shadow "IMPORT-RESTART-4" 'test1)
    (shadow "IMPORT-RESTART-4" 'test2)
    (import (intern "IMPORT-RESTART-4" 'test2) 'test1))
  package-error)

(deftest-error import-restart.5
  (progn
    (make-package 'import-restart-5a)
    (make-package 'import-restart-5b)
    (make-package 'import-restart-5c)
    (export (intern "X" 'import-restart-5a) 'import-restart-5a)
    (use-package 'import-restart-5a 'import-restart-5b)
    (import (intern "X" 'import-restart-5c) 'import-restart-5b))
  package-error)

(deftest-error import-error.1
  (eval '(import 10))
  type-error)

(deftest-error import-error.2
  (eval '(import 'hello 20))
  type-error)

(deftest-error import-error.3
  (eval '(import '(10 20 30)))
  type-error)

(deftest-error! import-error.4
  (eval '(import)))

(deftest-error! import-error.5
  (eval '(import 'hello *package* 40)))

(deftest-error! import-error.6
  (eval '(import 'hello *package* 40)))

(deftest-error! import-error.7
  (eval '(import 'hello 'no-such-package-name)))

;;  ANSI Common Lisp
(deftest import-test.1
  (import 'common-lisp::car (make-package 'import-test-1 :use nil))
  t)

(deftest import-test.2
  (find-symbol "CAR" 'import-test-1)
  car :internal)

(deftest import-test.3
  (find-symbol "CDR" 'import-test-1)
  nil nil)


;;
;;  Function SHADOW
;;
(deftest shadow-init
  (init-test-package))

(deftest shadow.1
  (shadow "SHADOW1" 'test1)
  t)

(deftest shadow.2
  (find-symbol-list "SHADOW1" 'test1)
  ("TEST1" "SHADOW1" :internal))

(deftest shadow.3
  (count-shadowing-symbols "SHADOW1" 'test1)
  1)

(deftest shadow.4
  (progn
    (shadow 'shadow4 'test1)
    (values
      (find-symbol-list "SHADOW4" 'test1)
      (count-shadowing-symbols "SHADOW4" 'test1)))
  ("TEST1" "SHADOW4" :internal)
  1)

(deftest shadow.5
  (progn
    (intern "SHADOW5" 'test1)
    (shadow "SHADOW5" 'test1))
  t)

(deftest-error shadow.6
  (shadow (make-array 5 :initial-contents "Hello"))
  type-error)

(deftest shadow.7
  (progn
    (make-package 'shadow-1)
    (make-package 'shadow-2)
    (export (intern "X" 'shadow-1) 'shadow-1)
    (use-package 'shadow-1 'shadow-2)
    (multiple-value-bind (x y) (find-symbol "X" 'shadow-2)
      (declare (ignore x))
      (shadow "X" 'shadow-2)
      (values
        y
        (find-symbol-list "X" 'shadow-2))))
  :inherited
  ("SHADOW-2" "X" :internal))

(deftest shadow.8
  (shadow "SHADOW8")
  t)

(deftest shadow.9
  (let ((symbol (intern "SHADOW9")))
    (shadow "SHADOW9")
    (eq (car (member symbol (package-shadowing-symbols *package*)))
        symbol))
  t)

(deftest shadow-list.1
  (shadow '("SHADOW-LIST-1" "SHADOW-LIST-2") 'test1)
  t)

(deftest shadow-list.2
  (values
    (find-symbol-list "SHADOW-LIST-1" 'test1)
    (find-symbol-list "SHADOW-LIST-2" 'test1)
    (count-shadowing-symbols "SHADOW-LIST-1" 'test1)
    (count-shadowing-symbols "SHADOW-LIST-2" 'test1))
  ("TEST1" "SHADOW-LIST-1" :internal)
  ("TEST1" "SHADOW-LIST-2" :internal)
  1 1)

(deftest-error shadow-error.1
  (eval '(shadow 10))
  type-error)

(deftest-error shadow-error.2
  (eval '(shadow 'hello 20))
  type-error)

(deftest-error shadow-error.3
  (eval '(shadow '(10 20 30)))
  type-error)

(deftest-error! shadow-error.4
  (eval '(shadow)))

(deftest-error! shadow-error.5
  (eval '(shadow 'hello *package* 40)))

(deftest-error! shadow-error.6
  (eval '(shadow 'hello *package* 40)))

(deftest-error! shadow-error.7
  (eval '(shadow 'hello 'no-such-package-name)))

;;  ANSI Common Lisp
(deftest shadow-test.1
  (package-shadowing-symbols
    (make-package 'shadow-test-1))
  nil)

(deftest shadow-test.2
  (find-symbol "CAR" 'shadow-test-1)
  car :inherited)

(deftest shadow-test.3
  (shadow 'car 'shadow-test-1)
  t)

(deftest shadow-test.4
  (find-symbol-list "CAR" 'shadow-test-1)
  ("SHADOW-TEST-1" "CAR" :internal))

(deftest shadow-test.5
  (package-shadowing-symbols-list 'shadow-test-1)
  (("SHADOW-TEST-1" "CAR")))

(deftest shadow-test.6
  (progn
    (make-package 'shadow-test-2)
    (shadow
      (intern "TEST" (find-package 'shadow-test-2))
      (find-package 'shadow-test-2)))
  t)

(deftest shadow-test.7
  (shadow 'TEST (find-package 'shadow-test-2))
  t)

(deftest shadow-test.8
  (package-shadowing-symbols-list 'shadow-test-2)
  (("SHADOW-TEST-2" "TEST")))

(deftest shadow-test.9
  (progn
    (make-package 'shadow-test-3)
    (intern "TEST" (find-package 'shadow-test-3))
    (export (intern "TEST" 'shadow-test-3) (find-package 'shadow-test-3))
    (use-package 'shadow-test-3 (find-package 'shadow-test-2)))
  t)


;;
;;  Function SHADOWING-IMPORT
;;
(deftest shadowing-import-init
  (init-test-package))

(deftest shadowing-import.1
  (shadowing-import
    (intern "SHADOWING1" 'test1) 'test2)
  t)

(deftest shadowing-import.2
  (let ((x (intern "SHADOWING3" 'test1)))
    (shadowing-import x 'test2)
    (count-shadowing-symbols x 'test2))
  1)

(deftest shadowing-import.3
  (progn
    (shadowing-import (intern "SHADOWING3" 'test1) 'test2)
    (find-symbol-list "SHADOWING3" 'test2))
  ("TEST1" "SHADOWING3" :internal))

(deftest shadowing-import.4
  (shadowing-import (intern "SHADOWING4" 'test1))
  t)

(deftest shadowing-import.5
  (let ((x (intern "SHADOWING5" 'test1)))
    (shadowing-import x)
    (count-shadowing-symbols x *package*))
  1)

(deftest shadowing-import.6
  (progn
    (shadowing-import (intern "SHADOWING6" 'test1))
    (find-symbol-list "SHADOWING6" *package*))
  ("TEST1" "SHADOWING6" :internal))

(deftest shadowing-import-list.1
  (shadowing-import
    (list (intern "SHADOWING-LIST-1A" 'test1) 'shadowing-list-1b)
    'test1)
  t)

(deftest shadowing-import-list.2
  (values
    (find-symbol-list "SHADOWING-LIST-1A" 'test1)
    (find-symbol-list "SHADOWING-LIST-1B" *package*)
    (count-shadowing-symbols "SHADOWING-LIST-1A" 'test1)
    (count-shadowing-symbols "SHADOWING-LIST-1B" 'test1))
  ("TEST1" "SHADOWING-LIST-1A" :internal)
  ("COMMON-LISP-USER" "SHADOWING-LIST-1B" :internal)
  1 1)

(deftest shadowing-import-conflict.1
  (let ((x (intern "SHADOWING-CONFLICT-1" 'test2)))
    (intern "SHADOWING-CONFLICT-1" 'test1)
    (shadowing-import x 'test1))
  t)

(deftest shadowing-import-conflict.2
  (values
    (find-symbol-list "SHADOWING-CONFLICT-1" 'test1)
    (find-symbol-list "SHADOWING-CONFLICT-1" 'test2)
    (count-shadowing-symbols "SHADOWING-CONFLICT-1" 'test1)
    (count-shadowing-symbols "SHADOWING-CONFLICT-1" 'test2))
  ("TEST2" "SHADOWING-CONFLICT-1" :internal)
  ("TEST2" "SHADOWING-CONFLICT-1" :internal)
  1 0)

(deftest shadowing-import-conflict.3
  (let ((x (intern "SHADOWING-CONFLICT-3" 'test2)))
    (intern "SHADOWING-CONFLICT-3" 'test1)
    (shadowing-import x 'test1)
    (values
      (eq x (find-symbol "SHADOWING-CONFLICT-3" 'test1))
      (eq x (find-symbol "SHADOWING-CONFLICT-3" 'test2))))
  t t)

(deftest shadowing-import-conflict.4
  (let ((x (intern "SHADOWING-CONFLICT-4" 'test2))
        (y (intern "SHADOWING-CONFLICT-4" 'test3)))
    (import x 'test1)
    (shadowing-import y 'test1)
    (values
      (eq y (find-symbol "SHADOWING-CONFLICT-4" 'test1))
      (eq y (find-symbol "SHADOWING-CONFLICT-4" 'test2))
      (eq y (find-symbol "SHADOWING-CONFLICT-4" 'test3))
      (count-shadowing-symbols "SHADOWING-CONFLICT-4" 'test1)
      (count-shadowing-symbols "SHADOWING-CONFLICT-4" 'test2)
      (count-shadowing-symbols "SHADOWING-CONFLICT-4" 'test3)))
  t nil t
  1 0 0)

(deftest shadowing-import-conflict.5
  (progn
    (shadow "SHADOWING-CONFLICT-5" 'test1)
    (export (intern "SHADOWING-CONFLICT-5" 'test2) 'test2)
    (export (intern "SHADOWING-CONFLICT-5" 'test3) 'test3)
    (use-package 'test2 'test1)
    (use-package 'test3 'test1)
    ;;  unintern error
    ;; (unintern (intern "SHADOWING-CONFLICT-5" 'test1) 'test1)
    (shadowing-import
      (intern "SHADOWING-CONFLICT-5" 'common-lisp-user)
      'test1)
    (values
      (find-symbol-list "SHADOWING-CONFLICT-5" 'test1)
      (count-shadowing-symbols "SHADOWING-CONFLICT-5" 'test1)))
  ("COMMON-LISP-USER" "SHADOWING-CONFLICT-5" :internal)
  1)

(deftest shadowing-import-conflict.6
  (progn
    (unuse-package 'test3 'test1)
    (unuse-package 'test2 'test1)
    (values)))

(deftest-error shadowing-import-error.1
  (eval '(shadowing-import 10))
  type-error)

(deftest-error shadowing-import-error.2
  (eval '(shadowing-import 'hello 20))
  type-error)

(deftest-error shadowing-import-error.3
  (eval '(shadowing-import '(10 20 30)))
  type-error)

(deftest-error! shadowing-import-error.4
  (eval '(shadowing-import)))

(deftest-error! shadowing-import-error.5
  (eval '(shadowing-import 'hello *package* 40)))

(deftest-error! shadowing-import-error.6
  (eval '(shadowing-import 'hello *package* 40)))

(deftest-error! shadowing-import-error.7
  (eval '(shadowing-import 'hello 'no-such-package-name)))

;;  ANSI Common Lisp
(deftest shadowing-import-test.1
  (package-name
    (in-package "COMMON-LISP-USER"))
  "COMMON-LISP-USER")

(defvar shadowing-import-test-sym)
(deftest shadowing-import-test.2
  (symbolp
    (setq shadowing-import-test-sym (intern "SHADOWING-IMPORT-TEST-CONFLICT")))
  t)

(deftest shadowing-import-test.3
  (multiple-value-bind (x y)
    (intern "SHADOWING-IMPORT-TEST-CONFLICT"
            (make-package 'shadowing-import-test-temp))
    (values
      (package-name (symbol-package x))
      (symbol-name x)
      y))
  "SHADOWING-IMPORT-TEST-TEMP"
  "SHADOWING-IMPORT-TEST-CONFLICT"
  nil)

(deftest shadowing-import-test.4
  (package-shadowing-symbols 'shadowing-import-test-temp)
  nil)

(deftest shadowing-import-test.5
  (shadowing-import shadowing-import-test-sym 'shadowing-import-test-temp)
  t)

(deftest shadowing-import-test.6
  (count-shadowing-symbols
    "SHADOWING-IMPORT-TEST-CONFLICT"
    'shadowing-import-test-temp)
  1)


;;
;;
;;
(deftest intern-error.1
  (let ((x (intern "KEYWORD-TEST" "KEYWORD")))
    (values
      (symbol-name
        (symbol-value x))
      (package-name
        (symbol-package x))))
  "KEYWORD-TEST" "KEYWORD")

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

