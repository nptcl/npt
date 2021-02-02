;;
;;  subtypep atomic-not
;;
(deftest subtypep3-initialize
  (progn
    (setq *subtypep!* 'subtypep-atomic-not)
    (subtypep! nil nil nil t))
  subtypep-atomic-not)


;;
;;  atomic-not
;;
(deftest subtypep-not-normal.1
  (subtypep! 'integer 'integer)
  include)

(deftest subtypep-not-normal.2
  (subtypep! 'cons 'integer)
  exclude)

(deftest subtypep-not-normal.3
  (subtypep! 'real 'integer)
  false)

;;  left
(deftest subtypep-not-left.1
  (subtypep! '(not integer) 'integer)
  exclude)

(deftest subtypep-not-left.2
  (subtypep! '(not real) 'integer)
  exclude)

(deftest subtypep-not-left.3
  (subtypep! '(not integer) 'real)
  false)

(deftest subtypep-not-left.4
  (subtypep! '(not cons) 'integer)
  false)

;;  right
(deftest subtypep-not-right.1
  (subtypep! 'integer '(not integer))
  exclude)

(deftest subtypep-not-right.2
  (subtypep! 'integer '(not real))
  exclude)

(deftest subtypep-not-right.3
  (subtypep! 'real '(not integer))
  false)

(deftest subtypep-not-right.4
  (subtypep! 'real '(not cons))
  include)

;;  both
(deftest subtypep-not-both.1
  (subtypep! '(not integer) '(not integer))
  include)

(deftest subtypep-not-both.2
  (subtypep! '(not real) '(not integer))
  include)

(deftest subtypep-not-both.3
  (subtypep! '(not integer) '(not real))
  false)

(deftest subtypep-not-both.4
  (subtypep! '(not cons) '(not integer))
  false)


;;
;;  table
;;
(deftest subtypep3-compound-initialize
  (progn
    (setq *subtypep!* 'subtypep-compound)
    (subtypep! nil nil nil t))
  subtypep-compound)


;;
;;  cons
;;
(deftest subtypep-cons.1
  (subtypep! 'cons 'cons)
  include)

(deftest subtypep-cons.2
  (subtypep! 'sequence 'cons)
  false)

(deftest subtypep-cons.3
  (subtypep! 'null 'cons)
  exclude)

(deftest subtypep-cons.4
  (subtypep! '(cons integer cons) '(cons * *))
  include)

(deftest subtypep-cons.5
  (subtypep! '(cons integer cons) '(cons real *))
  include)

(deftest subtypep-cons.6
  (subtypep! '(cons integer cons) '(cons * cons))
  include)

(deftest subtypep-cons.7
  (subtypep! '(cons integer cons) '(cons character cons))
  false)

(deftest subtypep-cons.8
  (subtypep! '(cons integer real) '(cons t integer))
  false)

(deftest subtypep-cons.9
  (subtypep! '(cons integer real) '(cons t t))
  include)

(deftest subtypep-cons.10
  (subtypep! '(cons * real) '(cons real t))
  false)

(deftest subtypep-cons.11
  (subtypep! '(cons integer *) '(cons t real))
  false)


;;
;;  complex
;;
(deftest subtypep-complex.1
  (subtypep! 'complex 'complex)
  include)

(deftest subtypep-complex.2
  (subtypep! 'number 'complex)
  false)

(deftest subtypep-complex.3
  (subtypep! 'complex '(complex *))
  include)

(deftest-error subtypep-complex.4
  (subtypep! 'complex '(complex t)))

(deftest subtypep-complex.5
  (subtypep! '(complex integer) '(complex *))
  include)

(deftest subtypep-complex.6
  (subtypep! '(complex integer) '(complex single-float))
  false)

(deftest subtypep-complex.7
  (subtypep! '(complex *) '(complex single-float))
  false)

(deftest subtypep-complex.8
  (subtypep! 'cons 'complex)
  exclude)


;;
;;  function
;;
(deftest subtypep-function.1
  (subtypep! 'function 'function)
  include)

(deftest subtypep-function.2
  (subtypep! 'compiled-function 'function)
  include)

(deftest subtypep-function.3
  (subtypep! 'real 'function)
  exclude)

;;  lambda-list
(deftest subtypep-function-args.1
  (subtypep! '(function (integer) *) '(function * *))
  include)

(deftest subtypep-function-args.2
  (subtypep! '(function * *) '(function (real) *))
  false)

;;  var
(deftest subtypep-function-var.1
  (subtypep! '(function (real) *) '(function (real) *))
  include)

(deftest subtypep-function-var.2
  (subtypep! '(function (real real) *) '(function (real) *))
  false)

(deftest subtypep-function-var.3
  (subtypep! '(function (real) *) '(function (real real) *))
  false)

(deftest subtypep-function-var.4
  (subtypep! '(function (real integer) *) '(function (real real) *))
  include)

(deftest subtypep-function-var.5
  (subtypep! '(function (real cons) *) '(function (real real) *))
  false)

;;  &optional
(deftest subtypep-function-opt.1
  (subtypep! '(function (real &optional integer))
             '(function (real &optional t)))
  include)

(deftest subtypep-function-opt.2
  (subtypep! '(function (real &optional integer))
             '(function (real &optional cons)))
  false)

(deftest subtypep-function-opt.3
  (subtypep! '(function (real integer))
             '(function (real &optional real)))
  include)

(deftest subtypep-function-opt.4
  (subtypep! '(function (real integer))
             '(function (real &optional cons)))
  false)

(deftest subtypep-function-opt.5
  (subtypep! '(function (real &optional integer))
             '(function (real real)))
  false)

(deftest subtypep-function-opt.6
  (subtypep! '(function (&optional integer))
             '(function (real real)))
  false)

(deftest subtypep-function-opt.7
  (subtypep! '(function (real integer))
             '(function (&optional t)))
  false)

;;  &rest
(deftest subtypep-function-rest.1
  (subtypep! '(function (integer &rest integer))
             '(function (real &rest rational)))
  include)

(deftest subtypep-function-rest.2
  (subtypep! '(function (integer &rest cons))
             '(function (real &rest rational)))
  false)

(deftest subtypep-function-rest.3
  (subtypep! '(function (integer &optional ratio))
             '(function (&rest real)))
  include)

(deftest subtypep-function-rest.4
  (subtypep! '(function (&rest number))
             '(function (integer &optional integer)))
  false)

;;  &key
(deftest subtypep-function-key.1
  (subtypep! '(function (integer &key (hello real)))
             '(function (real &key (hello t))))
  include)

(deftest subtypep-function-key.2
  (subtypep! '(function (integer &key (hello real)))
             '(function (real &key (hello cons))))
  false)

(deftest subtypep-function-key.3
  (subtypep! '(function (integer &key (aaa real)))
             '(function (real &key (hello t))))
  false)

(deftest subtypep-function-key.4
  (subtypep! '(function (integer &key (aaa real)))
             '(function (real &key (hello t) (aaa number))))
  include)

(deftest subtypep-function-key.5
  (subtypep! '(function (integer &key (aaa real)))
             '(function (&rest t)))
  include)

(deftest subtypep-function-key.6
  (subtypep! '(function (&rest t))
             '(function (real &key (hello t) (aaa number))))
  false)

(deftest subtypep-function-key.7
  (subtypep! '(function (real &key (aaa symbol)))
             '(function (real &rest symbol)))
  include)

(deftest subtypep-function-key.8
  (subtypep! '(function (real &key (aaa symbol) (bbb integer)))
             '(function (real &rest symbol)))
  false)

;;  values
(deftest subtypep-function-values.1
  (subtypep! '(function * integer) '(function * integer))
  include)

(deftest subtypep-function-values.2
  (subtypep! '(function * integer) '(function * real))
  include)

(deftest subtypep-function-values.3
  (subtypep! '(function * real) '(function * cons))
  false)

(deftest subtypep-function-values.4
  (subtypep! '(function * real) '(function * (values)))
  include)

(deftest subtypep-function-values.5
  (subtypep! '(function * (values)) '(function * cons))
  false)

;;  var
(deftest subtypep-function-values-var.1
  (subtypep! '(function * (values integer))
             '(function * (values real)))
  include)

(deftest subtypep-function-values-var.2
  (subtypep! '(function * (values integer real cons))
             '(function * (values real t t)))
  include)

(deftest subtypep-function-values-var.3
  (subtypep! '(function * (values integer real cons))
             '(function * (values real)))
  include)

(deftest subtypep-function-values-var.4
  (subtypep! '(function * (values integer))
             '(function * (values real real cons)))
  false)

(deftest subtypep-function-values-var.5
  (subtypep! '(function * (values t))
             '(function * (values t t t)))
  false)

(deftest subtypep-function-values-var.6
  (subtypep! '(function * (values t t t))
             '(function * (values t)))
  include)

;;  &optional
(deftest subtypep-function-values-opt.1
  (subtypep! '(function * (values integer &optional integer))
             '(function * (values real &optional real)))
  include)

(deftest subtypep-function-values-opt.2
  (subtypep! '(function * (values integer &optional integer))
             '(function * (values real &optional real)))
  include)

(deftest subtypep-function-values-opt.3
  (subtypep! '(function * (values integer integer))
             '(function * (values &optional real rational)))
  include)

(deftest subtypep-function-values-opt.4
  (subtypep! '(function * (values &optional integer integer))
             '(function * (values real rational)))
  false)

(deftest subtypep-function-values-opt.5
  (subtypep! '(function * (values &optional integer integer))
             '(function * (values)))
  include)

(deftest subtypep-function-values-opt.6
  (subtypep! '(function * (values integer))
             '(function * (values real &optional integer integer)))
  false)

;;  &rest
(deftest subtypep-function-values-rest.1
  (subtypep! '(function * (values &rest integer))
             '(function * (values)))
  include)

(deftest subtypep-function-values-rest.2
  (subtypep! '(function * (values real &rest integer))
             '(function * (values number &rest number)))
  include)

(deftest subtypep-function-values-rest.3
  (subtypep! '(function * (values real &optional ratio &rest integer))
             '(function * (values number &rest number)))
  include)

(deftest subtypep-function-values-rest.4
  (subtypep! '(function * (values real &optional cons &rest integer))
             '(function * (values number &rest number)))
  false)

(deftest subtypep-function-values-rest.5
  (subtypep! '(function * (values real &rest integer))
             '(function * (values number &optional integer &rest number)))
  include)

(deftest subtypep-function-values-rest.6
  (subtypep! '(function * (values real &rest integer))
             '(function * (values number &optional cons &rest number)))
  false)

;;  all
(deftest subtypep-function-all.1
  (subtypep! '(function (integer &optional cons &rest symbol &key (hello integer))
                        (values t &optional ratio &rest symbol))
             '(function (real &optional t &rest symbol &key (hello t) (aaa integer))
                        (values t &optional rational &rest t)))
  include)

(deftest subtypep-function-all.2
  (subtypep! '(function (integer &optional cons &rest symbol &key (hello integer))
                        (values t &optional ratio &rest symbol))
             '(function (real &optional t &rest function &key (hello t) (aaa integer))
                        (values t &optional rational &rest t)))
  false)

;;
;;  compiled-function
;;
(deftest subtypep-compiled-function.1
  (subtypep! 'function 'compiled-function)
  false)

(deftest subtypep-compiled-function.2
  (subtypep! '(compiled-function (integer) cons)
             '(compiled-function (real) (values)))
  include)

(deftest subtypep-compiled-function.3
  (subtypep! '(compiled-function (integer) cons)
             '(compiled-function (real) (values symbol)))
  false)

(deftest subtypep-compiled-function.4
  (subtypep! 'symbol 'compiled-function)
  exclude)


;;
;;  nil
;;
(deftest subtypep-compound-nil.1
  (subtypep! 'integer nil)
  exclude)

(deftest subtypep-compound-nil.2
  (subtypep! nil nil)
  include)

(deftest subtypep-compound-nil.3
  (subtypep! t nil)
  exclude)


;;
;;  t
;;
(deftest subtypep-compound-t.1
  (subtypep! 'real t)
  include)

(deftest subtypep-compound-t.2
  (subtypep! nil t)
  include)

(deftest subtypep-compound-t.3
  (subtypep! t t)
  include)


;;
;;  eql
;;
(deftest subtypep-eql-type.1
  (subtypep! '(eql 10) 'integer)
  include)

(deftest subtypep-eql-type.2
  (subtypep! '(eql 10) 'symbol)
  exclude)

(deftest subtypep-eql-type.3
  (subtypep! '(eql #\A) '(satisfies hello))
  invalid)

(deftest subtypep-type-eql.1
  (subtypep! 'integer '(eql 10))
  false)

(deftest subtypep-type-eql.2
  (subtypep! 'symbol '(eql 10))
  exclude)

(deftest subtypep-type-eql.3
  (subtypep! '(satisfies hello) '(eql 10))
  invalid)

(deftest subtypep-eql-eql.1
  (subtypep! '(eql #\A) '(eql #\A))
  include)

(deftest subtypep-eql-eql.2
  (subtypep! '(eql #\A) '(eql #\a))
  exclude)

(deftest subtypep-eql-eql.3
  (subtypep! '(eql #\A) '(not (eql #\a)))
  include)


;;
;;  satisfies
;;
(deftest subtypep-satisfies.1
  (subtypep! 'integer '(satisfies hello))
  invalid)

(deftest subtypep-satisfies.2
  (subtypep! nil '(satisfies hello))
  include)

(deftest subtypep-satisfies.3
  (subtypep! '(satisfies hello) 'keyword)
  invalid)

(deftest subtypep-satisfies.4
  (subtypep! '(satisfies hello) t)
  include)

