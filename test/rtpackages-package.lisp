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
    (make-package 'rename-package-temporary :nicknames '("RENAME-PACKAGE-TEMP"))
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
(defvar *delete-package-baz-package*)

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

    (setq *delete-package-baz-package*
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
    (find-symbol "FOO" *delete-package-baz-package*)
    (values (prin1-to-string x) y))
  "DELETE-PACKAGE-FOO:FOO" :inherited)

(deftest delete-package-test.7
  (multiple-value-bind (x y)
    (find-symbol "BAR" *delete-package-baz-package*)
    (values (prin1-to-string x) y))
  "DELETE-PACKAGE-BAR:BAR" :inherited)

(deftest delete-package-test.8
  (values
    (packagep *delete-package-foo-package*)
    (packagep *delete-package-bar-package*)
    (packagep *delete-package-baz-package*))
  t t t)

(deftest delete-package-test.9
  (values
    (package-name *delete-package-foo-package*)
    (package-name *delete-package-bar-package*)
    (package-name *delete-package-baz-package*))
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
            (package-use-list *delete-package-baz-package*)))
  () ("DELETE-PACKAGE-FOO") ("DELETE-PACKAGE-BAR"))

(deftest delete-package-test.11
  (values
    (mapcar #'package-name
            (package-used-by-list *delete-package-foo-package*))
    (mapcar #'package-name
            (package-used-by-list *delete-package-bar-package*))
    (mapcar #'package-name
            (package-used-by-list *delete-package-baz-package*)))
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
  (find-symbol "DELETE-PACKAGE-FOO" *delete-package-baz-package*)
  nil nil)

(deftest delete-package-test.20
  (find-symbol "DELETE-PACKAGE-BAR" *delete-package-baz-package*)
  nil nil)

(deftest delete-package-test.21
  (values
    (packagep *delete-package-foo-package*)
    (packagep *delete-package-bar-package*)
    (packagep *delete-package-baz-package*))
  t t t)

(deftest delete-package-test.22
  (values
    (package-name *delete-package-foo-package*)
    (package-name *delete-package-bar-package*)
    (package-name *delete-package-baz-package*))
  "DELETE-PACKAGE-FOO"
  nil
  "DELETE-PACKAGE-BAZ")

(deftest delete-package-test.23
  (values
    (package-use-list *delete-package-foo-package*)
    (package-use-list *delete-package-bar-package*)
    (package-use-list *delete-package-baz-package*))
  nil nil nil)

(deftest delete-package-test.24
  (values
    (package-used-by-list *delete-package-foo-package*)
    (package-used-by-list *delete-package-bar-package*)
    (package-used-by-list *delete-package-baz-package*))
  nil nil nil)


;;
;;  Function MAKE-PACKAGE
;;
(deftest make-package.1
  (progn
    (make-package 'make-package-1)
    (prog1 (packagep (find-package 'make-package-1))
      (delete-package 'make-package-1)))
  t)

(deftest make-package.2
  (progn
    (make-package
      'make-package-2 :nicknames
      '(make-package-2-1 make-package-2-2 make-package-2-3 make-package-2-4))
    (prog1 (packagep (find-package 'make-package-2-2))
      (delete-package 'make-package-2)))
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

(deftest-error make-package-name.1
  (progn
    (make-package 'make-package-4)
    (make-package 'make-package-4))
  package-error)

(deftest-error make-package-name.2
  (progn
    (make-package 'make-package-name-2
                  :nicknames '(make-package-name-2a make-package-name-2b))
    (make-package 'make-package-name-2a))
  package-error)

(deftest make-package-name.3
  (packagep
    (handler-bind ((package-error #'continue))
      (make-package 'make-package-name-3)
      (make-package 'make-package-name-3)))
  t)

(deftest make-package-name.4
  (packagep
    (handler-bind ((package-error #'continue))
      (make-package 'make-package-name-4
                    :nicknames '(make-package-name-4a make-package-name-4b))
      (make-package 'make-package-name-4a)))
  t)

(deftest make-package-nicknames.1
  (packagep
    (make-package 'make-package-nicknames-1
                  :nicknames '(make-package-nicknames-1a)))
  t)

(deftest make-package-nicknames.2
  (progn
    (make-package 'make-package-nicknames-2
                  :nicknames '(make-package-nicknames-2a))
    (packagep
      (find-package 'make-package-nicknames-2a)))
  t)

(deftest make-package-nicknames.3
  (packagep
    (make-package 'make-package-nicknames-3
                  :nicknames '(make-package-nicknames-3)))
  t)

(deftest make-package-nicknames.4
  (progn
    (make-package 'make-package-nicknames-4
                  :nicknames '(make-package-nicknames-4a
                                make-package-nicknames-4b))
    (values
      (packagep (find-package 'make-package-nicknames-4))
      (packagep (find-package 'make-package-nicknames-4a))
      (packagep (find-package 'make-package-nicknames-4b))))
  t t t)

(deftest-error make-package-nicknames.5
  (progn
    (make-package 'make-package-nicknames-5a)
    (make-package 'make-package-nicknames-5b
                  :nicknames '(make-package-nicknames-5a)))
  package-error)

(deftest-error make-package-nicknames.6
  (progn
    (make-package 'make-package-nicknames-6a
                  :nicknames '(make-package-nicknames-6b))
    (make-package 'make-package-nicknames-6c
                  :nicknames '(make-package-nicknames-6b)))
  package-error)

(deftest make-package-nicknames.7
  (package-name
    (handler-bind ((package-error #'continue))
      (make-package 'make-package-nicknames-7)
      (make-package 'make-package-nicknames-7a
                    :nicknames '(make-package-nicknames-7))))
  "MAKE-PACKAGE-NICKNAMES-7A")

(deftest make-package-nicknames.8
  (package-nicknames
    (handler-bind ((package-error #'continue))
      (make-package 'make-package-nicknames-8)
      (make-package 'make-package-nicknames-8a
                    :nicknames '(make-package-nicknames-8b
                                  make-package-nicknames-7
                                  make-package-nicknames-8c))))
  ("MAKE-PACKAGE-NICKNAMES-8B" "MAKE-PACKAGE-NICKNAMES-8C"))

(deftest make-package-use.1
  (packagep
    (progn
      (make-package 'make-package-use-1a)
      (make-package 'make-package-use-1b :use '(make-package-use-1a))))
  t)

(deftest-error make-package-use.2
  (packagep
    (progn
      (make-package 'make-package-use-2a)
      (make-package 'make-package-use-2b :use '(20)))))

(deftest make-package-use.3
  (mapcar #'package-name
          (package-use-list
            (make-package 'make-package-use-3)))
  ("COMMON-LISP"))

(deftest make-package-use.4
  (package-used-by-list
    (make-package 'make-package-use-4))
  nil)

(deftest make-package-use.5
  (progn
    (make-package 'make-package-use-5a :use nil)
    (make-package 'make-package-use-5b :use '(make-package-use-5a))
    (values
      (mapcar #'package-name (package-use-list 'make-package-use-5a))
      (mapcar #'package-name (package-used-by-list 'make-package-use-5a))
      (mapcar #'package-name (package-use-list 'make-package-use-5b))
      (mapcar #'package-name (package-used-by-list 'make-package-use-5b))))
  () ("MAKE-PACKAGE-USE-5B")
  ("MAKE-PACKAGE-USE-5A") ())

(deftest-error make-package-use.6
  (make-package 'make-package-use-6 :use '(no-such-package-name)))

(deftest-error make-package-use.7
  (progn
    (make-package 'make-package-use-7a)
    (make-package 'make-package-use-7b)
    (export (intern "X" 'make-package-use-7a) 'make-package-use-7a)
    (export (intern "X" 'make-package-use-7b) 'make-package-use-7b)
    (make-package 'make-package-use-7c
                  :use '(make-package-use-7a make-package-use-7b)))
  package-error)

(deftest make-package-use.8
  (handler-bind ((package-error #'continue))
    (make-package 'make-package-use-8a)
    (make-package 'make-package-use-8b)
    (export (intern "X" 'make-package-use-8a) 'make-package-use-8a)
    (export (intern "X" 'make-package-use-8b) 'make-package-use-8b)
    (make-package 'make-package-use-8c :use '(make-package-use-8a make-package-use-8b))
    (mapcar #'package-name (package-use-list 'make-package-use-8c)))
  nil)

(deftest make-package-use.9
  (handler-bind ((package-error
                   (lambda (c)
                     (invoke-restart 'shadow c))))
    (make-package 'make-package-use-9a)
    (make-package 'make-package-use-9b)
    (export (intern "X" 'make-package-use-9a) 'make-package-use-9a)
    (export (intern "X" 'make-package-use-9b) 'make-package-use-9b)
    (make-package 'make-package-use-9c :use '(make-package-use-9a make-package-use-9b))
    (sort (mapcar #'package-name (package-use-list 'make-package-use-9c))
          #'string<))
  ("MAKE-PACKAGE-USE-9A" "MAKE-PACKAGE-USE-9B"))

(deftest make-package-use.10
  (mapcar #'symbol-name (package-shadowing-symbols 'make-package-use-9c))
  ("X"))

(deftest-error make-package-error.1
  (eval '(make-package 10))
  type-error)

(deftest-error! make-package-error.2
  (eval '(make-package)))

(deftest-error make-package-error.3
  (eval '(make-package 'make-package-error-3 :nicknames)))

(deftest-error make-package-error.4
  (eval '(make-package 'make-package-error-4 :nicknames 20)))

(deftest-error make-package-error.5
  (eval '(make-package 'make-package-error-5 :hello 20)))

(deftest-error make-package-error.6
  (eval '(make-package 'make-package-error-6 :use 20)))

(deftest-error make-package-error.7
  (eval '(make-package 'make-package-error-7 :nicknames '(10))))

(deftest-error make-package-error.8
  (eval '(make-package 'make-package-error-8 :use '(10))))

;;  ANSI Common Lisp
(deftest make-package-test.1
  (packagep
    (make-package 'make-package-test-1
                  :nicknames '("MAKE-PACKAGE-TEST-2" "MAKE-PACKAGE-TEST-3")))
  t)

(deftest make-package-test.2
  (packagep
    (make-package "MAKE-PACKAGE-TEST-4" :use '("MAKE-PACKAGE-TEST-3")))
  t)

(deftest make-package-test.3
  (mapcar #'package-name (package-used-by-list 'make-package-test-3))
  ("MAKE-PACKAGE-TEST-4"))

(deftest make-package-test.4
  (mapcar #'package-name (package-use-list 'make-package-test-4))
  ("MAKE-PACKAGE-TEST-1"))


;;
;;  Macro WITH-PACKAGE-ITERATOR
;;
(deftest with-package-iterator.1
  (with-package-iterator
    (call *package* :internal)
    (multiple-value-bind (check symbol status package) (call)
      (values check (symbolp symbol) status (eq package *package*))))
  t t :internal t)

(deftest with-package-iterator.2
  (with-package-iterator
    (call (list *package*) :internal)
    (multiple-value-bind (check symbol status package) (call)
      (values check (symbolp symbol) status (eq package *package*))))
  t t :internal t)

(deftest with-package-iterator.3
  (let ((x (make-package 'with-package-iterator-3 :use nil)))
    (with-package-iterator
      (call x :internal :external :inherited)
      (call)))
  nil)

(defun with-package-iterator-loop (call)
  (do (list) (nil)
    (multiple-value-bind (check x y z) (funcall call)
      (unless check
        (return list))
      (push (list x y z) list))))

(defun with-package-iterator-string (call)
  (mapcar (lambda (list)
            (destructuring-bind (x y z) list
              (list
                (package-name
                  (symbol-package x))
                (symbol-name x)
                y
                (package-name z))))
          (with-package-iterator-loop call)))

(defun with-package-iterator-list (call)
  (sort (with-package-iterator-string call)
        (lambda (x y)
          (or (string< (car x) (car y))
              (string< (cadr x) (cadr y))))))

(deftest with-package-iterator.4
  (progn
    (make-package 'with-package-iterator-4a)
    (make-package 'with-package-iterator-4b)
    (make-package 'with-package-iterator-4c)
    (intern "A" 'with-package-iterator-4a)
    (intern "B" 'with-package-iterator-4a)
    (intern "C" 'with-package-iterator-4b)
    (intern "D" 'with-package-iterator-4b)
    (intern "E" 'with-package-iterator-4c)
    (let ((x '(with-package-iterator-4a
                with-package-iterator-4b
                with-package-iterator-4c)))
      (with-package-iterator
        (call x :internal)
        (with-package-iterator-list (lambda () (call))))))
  (("WITH-PACKAGE-ITERATOR-4A" "A" :internal "WITH-PACKAGE-ITERATOR-4A")
   ("WITH-PACKAGE-ITERATOR-4A" "B" :internal "WITH-PACKAGE-ITERATOR-4A")
   ("WITH-PACKAGE-ITERATOR-4B" "C" :internal "WITH-PACKAGE-ITERATOR-4B")
   ("WITH-PACKAGE-ITERATOR-4B" "D" :internal "WITH-PACKAGE-ITERATOR-4B")
   ("WITH-PACKAGE-ITERATOR-4C" "E" :internal "WITH-PACKAGE-ITERATOR-4C")))

(deftest-error with-package-iterator-error.1
  (eval '(with-package-iterator
           (10 *package* :internal :external :inherited)
           :hello)))

(deftest-error with-package-iterator-error.2
  (eval '(with-package-iterator
           (call 20 :internal :external :inherited)
           :hello)))

(deftest-error with-package-iterator-error.3
  (eval '(with-package-iterator
           (call '(10 20 30) :internal :external :inherited)
           :hello)))

(deftest-error with-package-iterator-error.4
  (eval '(with-package-iterator
           (call *package*)
           :hello))
  package-error)

(deftest-error with-package-iterator-error.5
  (eval '(with-package-iterator
           (call *package* :hello)
           :hello))
  package-error)

(deftest-error with-package-iterator-error.6
  (eval '(with-package-iterator
           (call)
           :hello)))

(deftest-error with-package-iterator-error.7
  (eval '(with-package-iterator
           (call *package* :internal :external :inherited)
           (call 10))))


;;
;;  Macro IN-PACKAGE
;;
(deftest in-package.1
  (let (name)
    (in-package test1)
    (setq name (package-name *package*))
    (in-package common-lisp-user)
    name)
  "TEST1")

(deftest-error in-package.2
  (in-package no-such-package-name)
  package-error)

(deftest in-package.3
  (package-name
    (handler-bind ((package-error
                     (lambda (x)
                       (use-value 'common-lisp-user x ))))
      (in-package no-such-package-name)))
  "COMMON-LISP-USER")

(deftest-error in-package.4
  (eval '(in-package 10)))

(deftest-error in-package.5
  (eval '(in-package)))

(deftest-error in-package.6
  (eval '(in-package common-lisp-user common-lisp-user)))

