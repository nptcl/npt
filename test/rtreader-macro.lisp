;;
;;  ANSI COMMON LISP: 23. Reader
;;

;;
;;  Function MAKE-DISPATCH-MACRO-CHARACTER
;;
(deftest make-dispatch-macro-character.1
  (let ((*readtable* (copy-readtable)))
    (values
      (get-macro-character #\$)
      (make-dispatch-macro-character #\$)
      (not (get-macro-character #\$))))
  nil t nil)

(deftest make-dispatch-macro-character.2
  (let ((x (copy-readtable)))
    (values
      (get-macro-character #\$)
      (get-macro-character #\$ x)
      (make-dispatch-macro-character #\$ nil x)
      (not (get-macro-character #\$))
      (not (get-macro-character #\$ x))))
  nil nil t t nil)

(deftest make-dispatch-macro-character.3
  (let ((*readtable* (copy-readtable)))
    (make-dispatch-macro-character #\$ nil)
    (set-dispatch-macro-character
      #\$ #\-
      (lambda (stream char value)
        (declare (ignore char value))
        (format nil "<<~A>>" (read stream t nil t))))
    (with-input-from-string (stream "aaa$-bbb")
      (values
        (read stream nil nil)
        (read stream nil nil)
        (read stream nil nil))))
  aaa "<<BBB>>" nil)

(deftest make-dispatch-macro-character.4
  (let ((*readtable* (copy-readtable)))
    (make-dispatch-macro-character #\$ t)
    (set-dispatch-macro-character
      #\$ #\-
      (lambda (stream char value)
        (declare (ignore char value))
        (format nil "<<~A>>" (read stream t nil t))))
    (with-input-from-string (stream "aaa$-bbb")
      (values
        (read stream nil nil)
        (read stream nil nil)
        (read stream nil nil))))
  AAA$-BBB nil nil)

(deftest-error! make-dispatch-macro-character-error.1
  (eval '(make-dispatch-macro-character)))

(deftest-error! make-dispatch-macro-character-error.2
  (eval '(make-dispatch-macro-character #\$ t nil)))

(deftest-error! make-dispatch-macro-character-error.3
  (eval '(make-dispatch-macro-character #\$ t *readtable* nil)))

(deftest-error make-dispatch-macro-character-error.4
  (eval '(make-dispatch-macro-character 10))
  type-error)

;;  ANSI Common Lisp
(deftest make-dispatch-macro-character-test.1
  (let ((*readtable* (copy-readtable)))
    (values
      (multiple-value-list (get-macro-character #\{))
      (make-dispatch-macro-character #\{)
      (not (get-macro-character #\{))))
  (nil nil) t nil)


;;
;;  Function SET-DISPATCH-MACRO-CHARACTER
;;
(deftest set-dispatch-macro-character.1
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\$ (constantly :hello))
    (values
      (read-from-string "   #$ ")))
  :hello)

(deftest set-dispatch-macro-character.2
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\$ (constantly :hello)))
  t)

(deftest-error set-dispatch-macro-character.3
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\$ #\# (constantly :hello))))

(deftest set-dispatch-macro-character.4
  (let ((*readtable* (copy-readtable nil)))
    (make-dispatch-macro-character #\$)
    (set-dispatch-macro-character #\$ #\# (constantly :hello))
    (values
      (read-from-string "   $# ")))
  :hello)

(deftest set-dispatch-macro-character.5
  (let ((x (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\% (constantly :hello) x)
    (let ((*readtable* x))
      (values
        (read-from-string "   #% "))))
  :hello)

(deftest set-dispatch-macro-character.6
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\$ (constantly :hello))
    (set-dispatch-macro-character #\# #\$ (constantly :abc))
    (values
      (read-from-string "   #$ ")))
  :abc)

(defun set-dispatch-macro-character-call (&rest args)
  (declare (ignore args))
  :zzz)

(deftest set-dispatch-macro-character.7
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\$ 'set-dispatch-macro-character-call)
    (values
      (read-from-string "   #$ ")))
  :zzz)

(deftest-error! set-dispatch-macro-character-error.1
  (eval '(set-dispatch-macro-character #\# #\$)))

(deftest-error! set-dispatch-macro-character-error.2
  (eval '(set-dispatch-macro-character #\# #\$ (constantly 10) *readtable* nil)))

(deftest-error! set-dispatch-macro-character-error.3
  (eval '(set-dispatch-macro-character #\# #\$ nil *readtable*))
  undefined-function)


;;
;;  Function GET-DISPATCH-MACRO-CHARACTER
;;
(deftest get-dispatch-macro-character.1
  (get-dispatch-macro-character #\# #\$)
  nil)

(deftest-error get-dispatch-macro-character.2
  (get-dispatch-macro-character #\$ #\$))

(deftest get-dispatch-macro-character.3
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\$ (constantly :hello))
    (functionp
      (get-dispatch-macro-character #\# #\$)))
  t)

(deftest get-dispatch-macro-character.4
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\$ (constantly :hello))
    (funcall
      (get-dispatch-macro-character #\# #\$)
      nil nil nil))
  :hello)

(deftest-error! get-dispatch-macro-character-error.1
  (eval '(get-dispatch-macro-character #\#)))

(deftest-error! get-dispatch-macro-character-error.2
  (eval '(get-dispatch-macro-character #\# #\$ *readtable* nil)))

(deftest-error get-dispatch-macro-character-error.3
  (eval '(get-dispatch-macro-character #\# 10))
  type-error)

;;  ANSI Common Lisp
(deftest get-dispatch-macro-test.1
  (let ((*readtable* (copy-readtable nil)))
    (get-dispatch-macro-character #\# #\{))
  nil)

(deftest get-dispatch-macro-test.2
  (let ((*readtable* (copy-readtable nil)))
    (values
      (set-dispatch-macro-character
        #\# #\{        ;dispatch on #{
        #'(lambda(s c n)
            (declare (ignore c))
            (let ((list (read s nil (values) t)))  ;list is object after #n{
              (when (consp list)                   ;return nth element of list
                (unless (and n (< 0 n (length list))) (setq n 0))
                (setq list (nth n list)))
              list)))
      (read-from-string "#{(1 2 3 4)")
      (read-from-string "#3{(0 1 2 3)")
      (read-from-string "#{123")))
  t 1 3 123)


;;
;;  Function SET-MACRO-CHARACTER
;;
(deftest set-macro-character.1
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123)))
  t)

(deftest set-macro-character.2
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123))
    (values
      (read-from-string "$5678")))
  123)

(deftest set-macro-character.3
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123))
    (values
      (read-from-string "hello$5678")))
  hello)

(deftest set-macro-character.4
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123) nil)
    (values
      (read-from-string "hello$5678")))
  hello)

(deftest set-macro-character.5
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123) t)
    (values
      (read-from-string "$")))
  123)

(deftest set-macro-character.6
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123) t)
    (values
      (read-from-string "hello$5678")))
  hello$5678)

(deftest set-macro-character.7
  (let ((x (copy-readtable nil)))
    (set-macro-character #\$ (constantly 123) t x)
    (let ((*readtable* x))
      (values
        (read-from-string "$"))))
  123)

(defun set-macro-character-call (&rest args)
  (declare (ignore args))
  :hello-macro)

(deftest set-macro-character.8
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ 'set-macro-character-call)
    (values
      (read-from-string "$")))
  :hello-macro)

(deftest-error! set-macro-character-error.1
  (eval '(set-macro-character)))

(deftest-error! set-macro-character-error.2
  (eval '(set-macro-character #\$ (constantly :hello) nil *readtable* nil)))

(deftest-error set-macro-character-error.3
  (eval '(set-macro-character #\$ nil))
  undefined-function)


;;
;;  Function GET-MACRO-CHARACTER
;;
(deftest get-macro-character.1
  (let ((*readtable* (copy-readtable nil)))
    (get-macro-character #\$))
  nil nil)

(deftest get-macro-character.2
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly :hello))
    (funcall
      (get-macro-character #\$)))
  :hello)

(deftest get-macro-character.3
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly :hello))
    (multiple-value-bind (x y) (get-macro-character #\$)
      (values (functionp x) y)))
  t nil)

(deftest get-macro-character.4
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\$ (constantly :hello) t)
    (multiple-value-bind (x y) (get-macro-character #\$)
      (values (functionp x) y)))
  t t)

(deftest get-macro-character.5
  (let ((x (copy-readtable nil)))
    (set-macro-character #\$ (constantly :hello) nil x)
    (funcall
      (get-macro-character #\$ x)))
  :hello)

(deftest-error! get-macro-character-error.1
  (eval '(get-macro-character)))

(deftest-error! get-macro-character-error.2
  (eval '(get-macro-character #\$ *readtable* nil)))

(deftest-error! get-macro-character-error.3
  (eval '(get-macro-character 10))
  type-error)

;;  ANSI Common Lisp
(deftest get-macro-character-test.1
  (get-macro-character #\{)
  nil nil)

(deftest get-macro-character-test.2
  (not (get-macro-character #\;))
  nil)

(defun get-macro-character-single-quote-reader (stream char)
  (declare (ignore char))
  (list 'quote (read stream t nil t)))

(deftest get-macro-character-test.3
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\' #'get-macro-character-single-quote-reader))
  t)


;;
;;  Function SET-SYNTAX-FROM-CHAR
;;
(deftest set-syntax-from-char.1
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;))
  t)

(deftest set-syntax-from-char.2
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;)
    (values
      (read-from-string "123579")))
  1235)

(deftest set-syntax-from-char.3
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;)
    (values
      (read-from-string "123579")))
  1235)

(deftest set-syntax-from-char.4
  (let ((x (copy-readtable nil)))
    (set-syntax-from-char #\7 #\; x)
    (let ((*readtable* x))
      (values
        (read-from-string "123579"))))
  1235)

(deftest set-syntax-from-char.5
  (let ((*readtable* (copy-readtable nil))
        (x (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;)
    (set-syntax-from-char #\5 #\7 x *readtable*)
    (let ((*readtable* x))
      (values
        (read-from-string "123579"))))
  123)

(deftest set-syntax-from-char.6
  (let ((*readtable* (copy-readtable nil))
        (x (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;)
    (set-syntax-from-char #\5 #\7 x)
    (let ((*readtable* x))
      (values
        (read-from-string "123579"))))
  123579)

(deftest set-syntax-from-char.7
  (let ((*readtable* (copy-readtable nil))
        (x (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;)
    (set-syntax-from-char #\5 #\7 x nil)
    (let ((*readtable* x))
      (values
        (read-from-string "123579"))))
  123579)

(deftest-error! set-syntax-from-char-error.1
  (eval '(set-syntax-from-char #\7)))

(deftest-error! set-syntax-from-char-error.2
  (eval '(set-syntax-from-char #\7 #\; *readtable* nil nil)))

(deftest-error! set-syntax-from-char-error.3
  (eval '(set-syntax-from-char 10 20))
  type-error)


;;
;;  Macro WITH-STANDARD-IO-SYNTAX
;;
(deftest with-standard-io-syntax.1
  (with-standard-io-syntax
    (values 10 20 30))
  10 20 30)

(deftest with-standard-io-syntax.2
  (with-standard-io-syntax
    10 20 30)
  30)

(deftest with-standard-io-syntax.3
  (let ((*print-array* nil)
        (*print-base* 16)
        (*print-case* :downcase)
        (*print-circle* t)
        (*print-escape* nil)
        (*print-gensym* nil)
        (*print-length* 10)
        (*print-level* 20)
        (*print-lines* 5)
        (*print-miser-width* 40)
        (*print-pretty* t)
        (*print-radix* t)
        (*print-readably* nil)
        (*print-right-margin* 11)
        (*read-base* 8)
        (*read-default-float-format* 'double-float)
        (*read-eval* nil)
        (*read-suppress* t))
    (with-standard-io-syntax
      (values
        *print-array*
        *print-base*
        *print-case*
        *print-circle*
        *print-escape*
        *print-gensym*
        *print-length*
        *print-level*
        *print-lines*
        *print-miser-width*
        *print-pretty*
        *print-radix*
        *print-readably*
        *print-right-margin*
        *read-base*
        *read-default-float-format*
        *read-eval*
        *read-suppress*)))
  t 10 :upcase nil t t nil nil nil nil nil nil t nil
  10 single-float t nil)

(deftest with-standard-io-syntax.4
  (let ((*package* (find-package 'common-lisp)))
    (with-standard-io-syntax
      (package-name *package*)))
  "COMMON-LISP-USER")

(deftest with-standard-io-syntax.5
  (let ((x (copy-pprint-dispatch *print-pprint-dispatch*))
        (*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (with-standard-io-syntax
      (eq *print-pprint-dispatch* x)))
  nil)

(deftest with-standard-io-syntax.6
  (let ((*readtable* (copy-readtable nil)))
    (set-syntax-from-char #\7 #\;)
    (with-standard-io-syntax
      (values
        (read-from-string "123579"))))
  123579)


;;
;;  dispatch
;;
(deftest dispatch-block-comment.1
  (values
    (read-from-string "#| hello |# 10"))
  10)

(deftest dispatch-block-comment.2
  (values
    (read-from-string "#|| hello ||# 10"))
  10)

