;;
;;  ANSI COMMON LISP: 10. Symbols
;;

;;
;;  System Class SYMBOL
;;
(deftest symbol-type.1
  (lisp-system:closp
    (find-class 'symbol))
  t)

(deftest symbol-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'symbol)))
  (symbol t))

(deftest symbol-type.3
  (typep 'hello 'symbol)
  t)

(deftest symbol-type.4
  (typep :hello 'symbol)
  t)

(deftest symbol-type.5
  (typep 10'symbol)
  nil)


;;
;;  Type KEYWORD
;;
(deftest keyword-type.1
  (lisp-system:closp
    (find-class 'keyword))
  t)

(deftest keyword-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'keyword)))
  (keyword symbol t))

(deftest keyword-type.3
  (subtypep 'keyword 'symbol)
  t t)

(deftest keyword-type.4
  (typep :hello 'keyword)
  t)

(deftest keyword-type.5
  (typep 'hello 'keyword)
  nil)

(deftest keyword-type.6
  (typep 10 'keyword)
  nil)

(deftest keyword-type.7
  :hello
  :hello)

(deftest-error keyword-type.8
  (eval '(setq :hello 10)))


;;
;;  Function SYMBOLP
;;
(deftest symbolp.1
  (symbolp 'elephant)
  t)

(deftest symbolp.2
  (symbolp 12)
  nil)

(deftest symbolp.3
  (symbolp nil)
  t)

(deftest symbolp.4
  (symbolp '())
  t)

(deftest symbolp.5
  (symbolp :test)
  t)

(deftest symbolp.6
  (symbolp "hello")
  nil)

(deftest symbolp.7
  (symbolp '#:hello-gensym)
  t)

(deftest-error! symbolp-error.1
  (eval '(symbolp)))

(deftest-error! symbolp-error.2
  (eval '(symbolp nil nil)))


;;
;;  Function KEYWORDP
;;
(deftest keywordp.1
  (keywordp 'elephant)
  nil)

(deftest keywordp.2
  (keywordp 12)
  nil)

(deftest keywordp.3
  (keywordp :test)
  t)

(deftest keywordp.4
  (keywordp ':test)
  t)

(deftest keywordp.5
  (keywordp nil)
  nil)

(deftest keywordp.6
  (keywordp :nil)
  t)

(deftest keywordp.7
  (keywordp '(:test))
  nil)

(deftest keywordp.8
  (keywordp "hello")
  nil)

(deftest keywordp.9
  (keywordp ":hello")
  nil)

(deftest keywordp.10
  (keywordp '&optional)nil
  )

(deftest keywordp.11
  (keywordp '#:hello-gensym)
  nil)

(deftest-error! keywordp-error.1
  (eval '(keywordp)))

(deftest-error! keywordp-error.2
  (eval '(keywordp nil nil)))

