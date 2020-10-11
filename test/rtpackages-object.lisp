;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  System Class PACKAGE
;;
(deftest package-type.1
  (lisp-system:closp
    (find-class 'package))
  t)

(deftest package-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'package)))
  (package t))

(deftest package-type.3
  (typep *package* 'package)
  t)

(deftest package-type.4
  (typep 'hello 'package)
  nil)


;;
;;  Function PACKAGE-NAME
;;
(deftest package-name.1
  (package-name
    (find-package 'common-lisp))
  "COMMON-LISP")

(deftest package-name.2
  (let ((x (make-package 'package-name-test)))
    (delete-package 'package-name-test)
    (package-name x))
  nil)

(deftest package-name.3
  (package-name 'common-lisp-user)
  "COMMON-LISP-USER")

(deftest package-name.4
  (stringp
    (package-name *package*))
  t)

(deftest-error package-name-error.1
  (package-name 'no-such-package-name))

(deftest-error package-name-error.2
  (eval '(package-name 10))
  type-error)

(deftest-error! package-name-error.3
  (eval '(package-name)))

(deftest-error! package-name-error.4
  (eval '(package-name 'common-lisp 20)))

;;  ANSI Common Lisp
(deftest package-name-test.1
  (let ((*package* *package*))
    (in-package "COMMON-LISP-USER")
    (package-name *package*))
  "COMMON-LISP-USER")

(deftest package-name-test.2
  (package-name (symbol-package :test))
  "KEYWORD")

(deftest package-name-test.3
  (package-name (find-package 'common-lisp))
  "COMMON-LISP")

(deftest package-name-test.4
  (let ((package (make-package "PACKAGE-NAME-TEST-PACKAGE")))
    (rename-package "PACKAGE-NAME-TEST-PACKAGE" "PACKAGE-NAME-TEST-PACKAGE0")
    (package-name package))
  "PACKAGE-NAME-TEST-PACKAGE0")


;;
;;  Function PACKAGE-NICKNAMES
;;
(deftest package-nicknames.1
  (let ((x (make-package 'package-nicknames-1 :nicknames ())))
    (package-nicknames x))
  nil)

(deftest package-nicknames.2
  (progn
    (make-package 'package-nicknames-2 :nicknames '("AAA" "BBB" "CCC"))
    (prog1
      (sort (package-nicknames 'package-nicknames-2) #'string<)
      (delete-package 'package-nicknames-2)))
  ("AAA" "BBB" "CCC"))

(deftest-error package-nicknames-error.1
  (package-nicknames 'no-such-package-name))

(deftest-error package-nicknames-error.2
  (eval '(package-nicknames 10))
  type-error)

(deftest-error! package-nicknames-error.3
  (eval '(package-nicknames)))

(deftest-error! package-nicknames-error.4
  (eval '(package-nicknames 'common-lisp 20)))


;;
;;  Function PACKAGE-SHADOWING-SYMBOLS
;;
(deftest package-shadowing-symbols.1
  (let ((x (make-package 'package-shadowning-symbols-1)))
    (package-shadowing-symbols x))
  nil)

(deftest package-shadowing-symbols.2
  (progn
    (make-package 'package-shadowning-symbols-2)
    (package-shadowing-symbols 'package-shadowning-symbols-2))
  nil)

(deftest package-shadowing-symbols.3
  (progn
    (make-package 'package1)
    (shadow "AAA" 'package1)
    (let ((x (mapcar
               #'symbol-name
               (package-shadowing-symbols 'package1))))
      (delete-package 'package1)
      x))
  ("AAA"))

(deftest package-shadowing-symbols.4
  (progn
    (make-package 'package2)
    (shadow "AAA" 'package2)
    (shadow "BBB" 'package2)
    (shadow "CCC" 'package2)
    (let ((x (mapcar
               #'symbol-name
               (package-shadowing-symbols 'package2))))
      (delete-package 'package2)
      (sort x #'string<)))
  ("AAA" "BBB" "CCC"))

(deftest package-shadowing-symbols.5
  (progn
    (make-package 'package4)
    (make-package 'package5)
    (let ((x (intern "AAA" 'package5))
          (y (intern "BBB" 'package5)))
      (shadowing-import x 'package4)
      (shadowing-import y 'package4)
      (let ((list (mapcar
                    #'symbol-name
                    (package-shadowing-symbols 'package4))))
        (delete-package 'package4)
        (delete-package 'package5)
        (sort list #'string<))))
  ("AAA" "BBB"))

(deftest-error package-shadowing-symbols-error.1
  (package-shadowing-symbols 'no-such-package-name))

(deftest-error package-shadowing-symbols-error.2
  (eval '(package-shadowing-symbols 10))
  type-error)

(deftest-error! package-shadowing-symbols-error.3
  (eval '(package-shadowing-symbols)))

(deftest-error! package-shadowing-symbols-error.4
  (eval '(package-shadowing-symbols 'common-lisp 20)))

;;  ANSI Common Lisp
(deftest package-shadowing-symbols-test.1
  (package-shadowing-symbols
    (make-package 'package-test-1))
  ())

(deftest package-shadowing-symbols-test.2
  (shadow 'cdr 'package-test-1)
  t)

(deftest package-shadowing-symbols-test.3
  (let ((x (package-shadowing-symbols 'package-test-1)))
    (values
      (length x)
      (package-name (symbol-package (car x)))
      (symbol-name (car x))))
  1 "PACKAGE-TEST-1" "CDR")

(deftest package-shadowing-symbols-test.5
  (progn
    (intern "PILL" 'package-test-1)
    (shadowing-import 'pill 'package-test-1)
    (length
      (package-shadowing-symbols 'package-test-1)))
  2)


;;
;;  Function PACKAGE-USE-LIST
;;
(deftest package-use-list.1
  (let ((x (make-package 'package-use-list-1 :use nil)))
    (prog1
      (package-use-list x)
      (delete-package x)))
  nil)

(deftest package-use-list.2
  (let ((x (make-package 'package-use-list-2)))
    (prog1
      (mapcar
        #'package-name
        (package-use-list 'package-use-list-2))
      (delete-package x)))
  ("COMMON-LISP"))

(deftest package-use-list.3
  (progn
    (make-package 'package-use-list-1 :use nil)
    (make-package 'package-use-list-2)
    (make-package 'package-use-list-3)
    (make-package 'package-use-list-4)
    (use-package
      '(package-use-list-2 package-use-list-3 package-use-list-4)
      'package-use-list-1)
    (let ((x (mapcar
               #'package-name
               (package-use-list 'package-use-list-1))))
      (delete-package 'package-use-list-1)
      (delete-package 'package-use-list-2)
      (delete-package 'package-use-list-3)
      (delete-package 'package-use-list-4)
      (sort x #'string<)))
  ("PACKAGE-USE-LIST-2" "PACKAGE-USE-LIST-3" "PACKAGE-USE-LIST-4"))

(deftest-error package-use-list-error.1
  (package-use-list 'no-such-package-use-list))

(deftest-error package-use-list-error.2
  (eval '(package-use-list 10))
  type-error)

(deftest-error! package-use-list-error.3
  (eval '(package-use-list)))

(deftest-error! package-use-list-error.4
  (eval '(package-use-list 'common-lisp 20)))

;;  ANSI Common Lisp
(deftest package-use-list-test.1
  (mapcar #'package-name
          (package-use-list
            (make-package 'package1)))
  ("COMMON-LISP"))

(deftest package-use-list-test.2
  (progn
    (use-package 'common-lisp-user 'package1)
    (prog1
      (sort (mapcar #'package-name (package-use-list 'package1))
            #'string<)
      (delete-package 'package1)))
  ("COMMON-LISP" "COMMON-LISP-USER"))


;;
;;  Function PACKAGE-USED-BY-LIST
;;
(deftest package-used-by-list.1
  (let ((x (make-package 'package-used-by-list-1)))
    (prog1 (package-used-by-list x)
      (delete-package x)))
  nil)

(deftest package-used-by-list.2
  (let ((x (make-package 'package-used-by-list-2)))
    (prog1 (package-used-by-list 'package-used-by-list-2)
      (delete-package x)))
  nil)

(deftest package-used-by-list.3
  (let ((x (make-package 'package10))
        (y (make-package 'package11))
        (z (make-package 'package12)))
    (use-package x y)
    (use-package x z)
    (prog1 (sort (mapcar #'package-name
                         (package-used-by-list x))
                 #'string<)
      (delete-package y)
      (delete-package z)
      (delete-package x)))
  ("PACKAGE11" "PACKAGE12"))

(deftest-error package-used-by-list-error.1
  (package-used-by-list 'no-such-package-used-by-list))

(deftest-error package-used-by-list-error.2
  (eval '(package-used-by-list 10))
  type-error)

(deftest-error! package-used-by-list-error.3
  (eval '(package-used-by-list)))

(deftest-error! package-used-by-list-error.4
  (eval '(package-used-by-list 'common-lisp 20)))

;;  ANSI Common Lisp
(deftest package-used-by-list-test.1
  (package-used-by-list
    (make-package 'package-used-by-list-1))
  ())

(deftest package-used-by-list-test.2
  (progn
    (make-package 'package-used-by-list-2 :use '(package-used-by-list-1))
    (prog1 (mapcar #'package-name
                   (package-used-by-list 'package-used-by-list-1))
      (delete-package 'package-used-by-list-2)
      (delete-package 'package-used-by-list-1)))
  ("PACKAGE-USED-BY-LIST-2"))


;;
;;  Function PACKAGEP
;;
(deftest packagep.1
  (packagep *package*)
  t)

(deftest packagep.2
  (packagep 'common-lisp)
  nil)

(deftest packagep.3
  (packagep
    (find-package 'common-lisp))
  t)

(deftest-error! packagep-error.1
  (eval '(packagep)))

(deftest-error! packagep-error.2
  (eval '(packagep 10 20)))


;;
;;  Variable *PACKAGE*
;;
(deftest package-special.1
  (packagep *package*)
  t)

(deftest package-special.2
  (eq *package* (find-package 'common-lisp-user))
  t)

(deftest package-special.3
  (let ((*package* *package*))
    (in-package "COMMON-LISP-USER")
    (package-name *package*))
  "COMMON-LISP-USER")

(deftest package-special.4
  (let ((*package* *package*))
    (in-package "COMMON-LISP")
    (package-name *package*))
  "COMMON-LISP")

(deftest package-special.5
  (progn
    (make-package "PACKAGE-SPECIAL" :use '("COMMON-LISP"))
    (list
      (package-name
        (symbol-package
          (let ((*package* (find-package 'package-special)))
            (read-from-string "just-testing"))))
      (package-name
        *package*)))
  ("PACKAGE-SPECIAL" "COMMON-LISP-USER"))

(deftest package-special.6
  (list
    (package-name
      (symbol-package
        (read-from-string "just-testing")))
    (package-name
      *package*))
  ("COMMON-LISP-USER" "COMMON-LISP-USER"))

(deftest package-special.7
  (eq 'foo (intern "FOO"))
  t)

(deftest package-special.8
  (eq 'foo (let ((*package* (find-package 'package-special)))
             (intern "FOO")))
  nil)


;;
;;  Condition Type PACKAGE-ERROR
;;
(deftest package-error.1
  (lisp-system:closp
    (find-class 'package-error))
  t)

(deftest package-error.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'package-error)))
  (package-error error serious-condition condition standard-object t))

(deftest package-error.3
  (handler-case
    (error (make-condition 'package-error
                           :package (find-package 'common-lisp-user)))
    (package-error (c) (declare (ignore c)) :hello))
  :hello)


;;
;;  Function PACKAGE-ERROR-PACKAGE
;;
(deftest package-error-package.1
  (package-name
    (package-error-package
      (make-condition 'package-error :package (find-package 'common-lisp))))
  "COMMON-LISP")

(deftest package-error-package.2
  (handler-case
    (error (make-condition 'package-error
                           :package (find-package 'common-lisp-user)))
    (package-error (c) (values
                         (packagep
                           (package-error-package c))
                         (package-name
                           (package-error-package c)))))
  t "COMMON-LISP-USER")

(deftest-error package-error-package-error.1
  (eval '(package-error-package 10))
  type-error)

(deftest-error! package-error-package-error.2
  (eval '(package-error-package)))

(deftest-error! package-error-package-error.3
  (eval '(package-error-package
           (make-condition 'package-error :package (find-package 'common-lisp))
           nil)))

