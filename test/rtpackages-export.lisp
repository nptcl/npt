;;
;;  ANSI COMMON LISP: 11. Packages
;;

;;
;;  Function EXPORT
;;
(deftest export-init
  (init-test-package))

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

(deftest export.6
  (let ((x (intern "EXPORT6" 'test1)))
    (export x 'test1)
    (count "EXPORT6" (package-export-list 'test1) :test #'equal))
  1)

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
                     (invoke-restart 'ignore c))))
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
                       (invoke-restart 'ignore c))))
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
;;  Function UNEXPORT
;;
(deftest unexport-init
  (init-test-package))

(deftest unexport.1
  (unexport (intern "UNEXPORT1" 'test1) 'test1)
  t)

(deftest unexport.2
  (unexport (list (intern "UNEXPORT2-1" 'test1)
                  (intern "UNEXPORT2-2" 'test1)) 'test1)
  t)

(deftest unexport.3
  (export (intern "UNEXPORT3" 'test1) 'test1)
  t)

(deftest unexport.4
  (find-symbol-list "UNEXPORT3" 'test1)
  ("TEST1" "UNEXPORT3" :external))

(deftest unexport.5
  (unexport (intern "UNEXPORT3" 'test1) 'test1)
  t)

(deftest unexport.6
  (find-symbol-list "UNEXPORT3" 'test1)
  ("TEST1" "UNEXPORT3" :internal))

(deftest unexport.7
  (progn
    (unexport (intern "UNEXPORT3" 'test1) 'test1)
    (find-symbol-list "UNEXPORT3" 'test1))
  ("TEST1" "UNEXPORT3" :internal))

(deftest-error unexport.8
  (unexport (intern "NO-SUCH-SYMBOL-NAME" 'test2) 'test1)
  package-error)

(deftest-error unexport.9
  (unexport (intern "UNEXPORT3" 'test2) 'test1)
  package-error)

(deftest unexport.10
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

(deftest unexport.11
  (progn
    (export (intern "UNEXPORT4"))
    (unexport (intern "UNEXPORT4")))
  t)

(deftest unexport.12
  (let ((x (intern "EXPORT12" 'test1)))
    (export x 'test1)
    (unexport x 'test1)
    (count "EXPORT12" (package-export-list 'test1) :test #'equal))
  0)

(deftest unexport.13
  (progn
    (make-package 'unexport-13a)
    (make-package 'unexport-13b)
    (export (intern "X" 'unexport-13a) 'unexport-13a)
    (use-package 'unexport-13a 'unexport-13b)
    (let ((x (find-symbol-list "X" 'unexport-13b)))
      (unexport (intern "X" 'unexport-13a) 'unexport-13a)
      (values x (find-symbol-list "X" 'unexport-13b))))
  ("UNEXPORT-13A" "X" :inherited)
  nil)

(deftest unexport-list.1
  (let ((x (list (intern "UNEXPORT-LIST-1" 'test1)
                 (intern "UNEXPORT-LIST-2" 'test1))))
    (export x 'test1)
    (unexport x 'test1))
  t)

(deftest unexport-list.2
  (values
    (find-symbol-list "UNEXPORT-LIST-1" 'test1)
    (find-symbol-list "UNEXPORT-LIST-2" 'test1))
  ("TEST1" "UNEXPORT-LIST-1" :internal)
  ("TEST1" "UNEXPORT-LIST-2" :internal))

(deftest unexport-inherit.1
  (progn
    (make-package 'unexport-inherit-1)
    (make-package 'unexport-inherit-2 :use '(unexport-inherit-1))
    (export (intern "X" 'unexport-inherit-1) 'unexport-inherit-1)
    (unexport (intern "X" 'unexport-inherit-1) 'unexport-inherit-1)
    (find-symbol-list "X" 'unexport-inherit-2))
  nil)

(deftest unexport-inherit.2
  (find "X" (package-export-list 'unexport-inherit-1) :test #'equal)
  nil)


;;  error
(deftest-error unexport-error.1
  (eval '(unexport 10))
  type-error)

(deftest-error unexport-error.2
  (eval '(unexport 'hello 20))
  type-error)

(deftest-error unexport-error.3
  (eval '(unexport '(10 20 30)))
  type-error)

(deftest-error! unexport-error.4
  (eval '(unexport)))

(deftest-error! unexport-error.5
  (eval '(unexport 'hello *package* 40)))

(deftest-error! unexport-error.6
  (eval '(unexport 'hello *package* 40)))

(deftest-error! unexport-error.7
  (eval '(unexport 'hello 'no-such-package-name)))


;;  ANSI Common Lisp
(deftest unexport-test.1
  (package-name
    (in-package "COMMON-LISP-USER"))
  "COMMON-LISP-USER")

(deftest unexport-test.2
  (export (intern "UNEXPORT-TEST-CONTRABAND"
                  (make-package 'unexport-test-temp))
          'unexport-test-temp)
  t)

(deftest unexport-test.3
  (find-symbol "UNEXPORT-TEST-CONTRABAND")
  nil nil)

(deftest unexport-test.4
  (use-package 'unexport-test-temp)
  t)

(deftest unexport-test.5
  (find-symbol-list "UNEXPORT-TEST-CONTRABAND")
  ("UNEXPORT-TEST-TEMP" "UNEXPORT-TEST-CONTRABAND" :inherited))

(deftest unexport-test.6
  (unexport
    (intern "UNEXPORT-TEST-CONTRABAND" 'unexport-test-temp)
    'unexport-test-temp)
  t)

(deftest unexport-test.7
  (find-symbol "UNEXPORT-TEST-CONTRABAND")
  nil nil)

(deftest unexport-test.8
  (unuse-package 'unexport-test-temp)
  t)

