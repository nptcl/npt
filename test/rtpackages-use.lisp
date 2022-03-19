;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Function USE-PACKAGE
;;
(deftest use-package-init
  (init-test-package))

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
    (export (intern "USE-PACKAGE3" 'test2) 'test2)
    (use-package 'test2 'test1)
    (values
      (find-symbol-list "USE-PACKAGE3" 'test1)
      (find-symbol-list "USE-PACKAGE3" 'test2)))
  ("TEST2" "USE-PACKAGE3" :inherited)
  ("TEST2" "USE-PACKAGE3" :external))

(deftest use-package.4
  (progn
    (unuse-package 'test2 'test1)
    (use-package 'test2 'test1)
    (car (member
           "TEST2"
           (mapcar #'package-name (package-use-list 'test1))
           :test 'equal)))
  "TEST2")

(deftest-error use-package.5
  (use-package 'test1 'keyword))

(deftest-error use-package.6
  (use-package '(keywrod) 'test1))

(deftest use-package-readonly.1
  (let ((x (make-package 'use-package-readonly-1))
        (y (make-package 'use-package-readonly-2)))
    (package-readonly x t)
    (use-package x y))
  t)

(deftest-error use-package-readonly.2
  (let ((x (make-package 'use-package-readonly-3))
        (y (make-package 'use-package-readonly-4)))
    (package-readonly y t)
    (use-package x y)))

(deftest use-package-uninit.6
  (progn
    (unuse-package 'test1 'test2)
    (unuse-package 'test2 'test1))
  t)

(deftest-error use-package-conflict.1
  (progn
    (make-package 'use-package-conflict-1)
    (make-package 'use-package-conflict-2)
    (export (intern "X" 'use-package-conflict-1) 'use-package-conflict-1)
    (intern "X" 'use-package-conflict-2)
    (use-package 'use-package-conflict-1 'use-package-conflict-2))
  package-error)

(deftest use-package-conflict.2
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'shadow c))))
    (use-package 'use-package-conflict-1 'use-package-conflict-2))
  t)

(deftest use-package-conflict.3
  (count-shadowing-symbols "X" 'use-package-conflict-2)
  1)

(deftest use-package-conflict.4
  (find-symbol-list "X" 'use-package-conflict-2)
  ("USE-PACKAGE-CONFLICT-2" "X" :internal))

(deftest-error use-package-group.1
  (progn
    (make-package 'use-package-group-1)
    (make-package 'use-package-group-2)
    (make-package 'use-package-group-3)
    (export (intern "X" 'use-package-group-1) 'use-package-group-1)
    (export (intern "X" 'use-package-group-2) 'use-package-group-2)
    (use-package '(use-package-group-1 use-package-group-2) 'use-package-group-3))
  package-error)

(deftest use-package-group.2
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'shadow c))))
    (use-package '(use-package-group-1 use-package-group-2) 'use-package-group-3))
  t)

(deftest use-package-group.3
  (count-shadowing-symbols "X" 'use-package-group-3)
  1)

(deftest use-package-group.4
  (progn
    (make-package 'use-package-group-4)
    (make-package 'use-package-group-5)
    (make-package 'use-package-group-6)
    (let ((x (intern "X" 'use-package-group-6)))
      (import x 'use-package-group-4)
      (import x 'use-package-group-5)
      (export x 'use-package-group-4)
      (export x 'use-package-group-5))
    (use-package '(use-package-group-4 use-package-group-5) 'use-package-group-6))
  t)


;;  error
(deftest-error use-package-error.1
  (eval '(use-package 10)))

(deftest-error use-package-error.2
  (eval '(use-package 'hello 20)))

(deftest-error! use-package-error.3
  (eval '(use-package)))

(deftest-error! use-package-error.4
  (eval '(use-package 'hello 'aaa 'bbb)))

(deftest-error use-package-error.5
  (eval '(use-package '(10 20 30) 'test2)))

(deftest-error use-package-error.6
  (eval '(use-package 'keyword)))

(deftest-error use-package-error.7
  (eval '(use-package 'test1 'keyword)))


;;  ANSI Common Lisp
(deftest use-package-test.1
  (export
    (intern "LAND-FILL" (make-package 'use-package-test-1))
    'use-package-test-1)
  t)

(deftest use-package-test.2
  (find-symbol "LAND-FILL" (make-package 'use-package-test-2))
  nil nil)

(deftest use-package-test.3
  (mapcar #'package-name (package-use-list 'use-package-test-2))
  ("COMMON-LISP")) ;; Implementation dependency, ("USE-PACKAGE-TEST-2")

(deftest use-package-test.4
  (use-package 'use-package-test-1 'use-package-test-2)
  t)

(deftest use-package-test.5
  (count "USE-PACKAGE-TEST-1"
         (package-use-list 'use-package-test-2)
         :key #'package-name
         :test #'equal)
  1)

(deftest use-package-test.6
  (find-symbol-list "LAND-FILL" 'use-package-test-2)
  ("USE-PACKAGE-TEST-1" "LAND-FILL" :inherited))


;;
;;  Function UNUSE-PACKAGE
;;
(deftest unuse-package-init
  (init-test-package))

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

(deftest unuse-package.4
  (progn
    (use-package '(test1 test2) 'test3)
    (unuse-package '(test1 test2) 'test3))
  t)

(deftest unuse-package.5
  (progn
    (make-package 'unuse-package-1)
    (make-package 'unuse-package-2 :use '(unuse-package-1))
    (export (intern "X" 'unuse-package-1) 'unuse-package-1)
    (unuse-package 'unuse-package-1 'unuse-package-2)
    (find-symbol "X" 'unuse-package-2))
  nil nil)

(deftest unuse-package-readonly.1
  (let ((x (make-package 'unuse-package-readonly-1))
        (y (make-package 'unuse-package-readonly-2)))
    (use-package x y)
    (package-readonly x t)
    (unuse-package x y))
  t)

(deftest-error unuse-package-readonly.2
  (let ((x (make-package 'unuse-package-readonly-3))
        (y (make-package 'unuse-package-readonly-4)))
    (use-package x y)
    (package-readonly y t)
    (unuse-package x y)))

;;  error
(deftest-error unuse-package-error.1
  (eval '(unuse-package 10)))

(deftest-error unuse-package-error.2
  (eval '(unuse-package 'hello 20)))

(deftest-error! unuse-package-error.3
  (eval '(unuse-package)))

(deftest-error! unuse-package-error.4
  (eval '(unuse-package 'hello 'aaa 'bbb)))

(deftest-error unuse-package-error.5
  (eval '(unuse-package '(10 20 30) 'test2)))


;;  ANSI Common Lisp
(deftest unuse-package-test.1
  (package-name
    (in-package "COMMON-LISP-USER"))
  "COMMON-LISP-USER")

(deftest unuse-package-test.2
  (export
    (intern "UNUSE-PACKAGE-SYMBOL-1" (make-package 'unuse-package-test-1))
    'unuse-package-test-1)
  t)

(deftest unuse-package-test.3
  (find-symbol "UNUSE-PACKAGE-SYMBOL-1")
  nil nil)

(deftest unuse-package-test.4
  (use-package 'unuse-package-test-1)
  t)

(deftest unuse-package-test.5
  (find-symbol-list "UNUSE-PACKAGE-SYMBOL-1")
  ("UNUSE-PACKAGE-TEST-1" "UNUSE-PACKAGE-SYMBOL-1" :inherited))

(deftest unuse-package-test.6
  (package-name
    (find (find-package 'unuse-package-test-1)
          (package-use-list 'common-lisp-user)))
  "UNUSE-PACKAGE-TEST-1")

(deftest unuse-package-test.7
  (unuse-package 'unuse-package-test-1)
  t)

(deftest unuse-package-test.8
  (find-symbol "UNUSE-PACKAGE-SYMBOL-1")
  nil nil)

