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
;;  Macro DO-SYMBOLS
;;
(deftest do-symbols.1
  (progn
    (make-package 'do-symbols-1 :use ())
    (intern "X" 'do-symbols-1)
    (intern "Y" 'do-symbols-1)
    (export (intern "Z" 'do-symbols-1) 'do-symbols-1)
    (let ((*package* (find-package 'do-symbols-1))
          value)
      (do-symbols (x)
        (declare (ignore x))
        (setq value t))
      value))
  t)

(deftest do-symbols.2
  (let ((value 0))
    (do-symbols (x 'do-symbols-1)
      (declare (ignore x))
      (incf value 1))
    value)
  3)

(deftest do-symbols.3
  (let ((value :hello))
    (do-symbols (v 'do-symbols-1 value)
      (declare (ignore v))))
  :hello)

(deftest do-symbols.4
  (let ((value 0))
    (do-symbols (x 'do-symbols-1 :hello)
      (declare (ignore x))
      (incf value 1)
      (return value)))
  1)

(deftest do-symbols.5
  (let ((value 0))
    (do-symbols (x 'do-symbols-1 value)
      (declare (ignore x))
      (go label)
      (incf value 1)
      label))
  0)

(deftest do-symbols.6
  (let (list)
    (do-symbols (x 'do-symbols-1)
      (push x list))
    (sort (mapcar #'symbol-name list) #'string<))
  ("X" "Y" "Z"))

(deftest-error do-symbols-error.1
  (eval '(do-symbols (10))))

(deftest-error do-symbols-error.2
  (eval '(do-symbols (x 20) (symbolp x))))

(deftest-error do-symbols-error.3
  (eval '(do-symbols ())))

(deftest-error do-symbols-error.4
  (eval '(do-symbols (x *package* 30 40) (symbolp x))))


;;
;;  Macro DO-EXTERNAL-SYMBOLS
;;
(deftest do-external-symbols.1
  (progn
    (make-package 'do-external-symbols-1 :use ())
    (intern "W" 'do-external-symbols-1)
    (export (intern "X" 'do-external-symbols-1) 'do-external-symbols-1)
    (export (intern "Y" 'do-external-symbols-1) 'do-external-symbols-1)
    (export (intern "Z" 'do-external-symbols-1) 'do-external-symbols-1)
    (let ((*package* (find-package 'do-external-symbols-1))
          value)
      (do-external-symbols (x)
        (declare (ignore x))
        (setq value t))
      value))
  t)

(deftest do-external-symbols.2
  (let ((value 0))
    (do-external-symbols (x 'do-external-symbols-1)
      (declare (ignore x))
      (incf value 1))
    value)
  3)

(deftest do-external-symbols.3
  (let ((value :hello))
    (do-external-symbols (v 'do-external-symbols-1 value)
      (declare (ignore v))))
  :hello)

(deftest do-external-symbols.4
  (let ((value 0))
    (do-external-symbols (x 'do-external-symbols-1 :hello)
      (declare (ignore x))
      (incf value 1)
      (return value)))
  1)

(deftest do-external-symbols.5
  (let ((value 0))
    (do-external-symbols (x 'do-external-symbols-1 value)
      (declare (ignore x))
      (go label)
      (incf value 1)
      label))
  0)

(deftest do-external-symbols.6
  (let (list)
    (do-external-symbols (x 'do-external-symbols-1)
      (push x list))
    (sort (mapcar #'symbol-name list) #'string<))
  ("X" "Y" "Z"))

(deftest-error do-external-symbols-error.1
  (eval '(do-external-symbols (10))))

(deftest-error do-external-symbols-error.2
  (eval '(do-external-symbols (x 20) (symbolp x))))

(deftest-error do-external-symbols-error.3
  (eval '(do-external-symbols ())))

(deftest-error do-external-symbols-error.4
  (eval '(do-external-symbols (x *package* 30 40) (symbolp x))))


;;
;;  Macro DO-ALL-SYMBOLS
;;
#-force-gc
(deftest do-all-symbols.1
  (do-all-symbols (v)
    (declare (ignore v)))
  nil)

#-force-gc
(deftest do-all-symbols.2
  (let ((value 0))
    (do-all-symbols (x)
      (declare (ignore x))
      (incf value 1))
    (< 1 value))
  t)

#-force-gc
(deftest do-all-symbols.3
  (let ((value :hello))
    (do-all-symbols (v value)
      (declare (ignore v))))
  :hello)

(deftest do-all-symbols.4
  (let ((value 0))
    (do-all-symbols (x :hello)
      (declare (ignore x))
      (incf value 1)
      (return value)))
  1)

#-force-gc
(deftest do-all-symbols.5
  (let ((value 0))
    (do-all-symbols (x value)
      (declare (ignore x))
      (go label)
      (incf value 1)
      label))
  0)

(deftest-error do-all-symbols-error.1
  (eval '(do-all-symbols (10))))

(deftest-error do-all-symbols-error.2
  (eval '(do-all-symbols ())))

(deftest-error do-all-symbols-error.3
  (eval '(do-all-symbols (x 30 40) (symbolp x))))


;;  ANSI Common Lisp
#-force-gc
(deftest do-symbols-test.1
  (package-name
    (make-package 'do-symbols-test-1 :use nil))
  "DO-SYMBOLS-TEST-1")

#-force-gc
(deftest do-symbols-test.2
  (multiple-value-bind (x y) (intern "SHY" 'do-symbols-test-1)
    (values (package-name (symbol-package x))
            (symbol-name x)
            y))
  "DO-SYMBOLS-TEST-1" "SHY" nil)

#-force-gc
(deftest do-symbols-test.3
  (export (intern "BOLD" 'do-symbols-test-1) 'do-symbols-test-1)
  t)

#-force-gc
(deftest do-symbols-test.4
  (let ((lst ()))
    (do-symbols (s (find-package 'do-symbols-test-1))
      (push s lst))
    (mapcar
      (lambda (x)
        (list (package-name (symbol-package x))
              (symbol-name x)))
      (sort lst #'string< :key #'symbol-name)))
  (("DO-SYMBOLS-TEST-1" "BOLD")
   ("DO-SYMBOLS-TEST-1" "SHY")))

#-force-gc
(deftest do-symbols-test.5
  (let ((lst ()))
    (do-external-symbols (s (find-package 'do-symbols-test-1) lst)
      (push s lst))
    (mapcar
      (lambda (x)
        (list (package-name (symbol-package x))
              (symbol-name x)))
      (sort lst #'string< :key #'symbol-name)))
  (("DO-SYMBOLS-TEST-1" "BOLD")))

#-force-gc
(deftest do-symbols-test.6
  (let ((lst ()))
    (do-all-symbols (s lst)
      (when (eq (find-package 'do-symbols-test-1) (symbol-package s))
        (push s lst)))
    (mapcar
      (lambda (x)
        (list (package-name (symbol-package x))
              (symbol-name x)))
      (sort lst #'string< :key #'symbol-name)))
  (("DO-SYMBOLS-TEST-1" "BOLD")
   ("DO-SYMBOLS-TEST-1" "SHY")))

