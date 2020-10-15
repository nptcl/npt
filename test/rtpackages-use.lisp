;;
;;  ANSI COMMON LISP: 11. Packages
;;

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

