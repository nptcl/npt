;;
;;  typep / subtypep
;;

(deftest subtypep-and-right.1
  (subtypep-result 'fixnum '(and real rational integer))
  include)

(deftest subtypep-and-right.2
  (subtypep-result 'fixnum '(and real string integer))
  exclude)

(deftest subtypep-and-right.3
  (subtypep-result 'fixnum '(and real (integer 0 *) integer))
  false)

(deftest subtypep-and-right.4
  (subtypep-result 'fixnum '(and (satisfies hello) cons fixnum))
  exclude)

(deftest subtypep-and-right.5
  (subtypep-result 'fixnum '(and (satisfies hello) (integer 0 *)))
  invalid)

(deftest subtypep-or-right.1
  (subtypep-result 'integer '(or real string cons))
  include)

(deftest subtypep-or-right.2
  (subtypep-result 'integer '(or pathname string cons))
  exclude)

(deftest subtypep-or-right.3
  (subtypep-result 'integer '(or pathname (integer * 0) cons))
  false)

(deftest subtypep-or-right.4
  (subtypep-result 'integer '(or (satisfies hello) real cons))
  include)

(deftest subtypep-or-right.5
  (subtypep-result 'integer '(or (satisfies hello) string cons))
  invalid)

(deftest subtypep-and-left.1
  (subtypep-result '(and integer rational float) 'real)
  include)

(deftest subtypep-and-left.2
  (subtypep-result '(and string rational float) 'real)
  include)

(deftest subtypep-and-left.3
  (subtypep-result '(and (integer 0 *) rational float) 'real)
  include)

(deftest subtypep-and-left.4
  (subtypep-result '(and real rational number) 'integer)
  false)

(deftest subtypep-and-left.5
  (subtypep-result '(and string rational number) 'integer)
  include)

(deftest subtypep-or-left.1
  (subtypep-result '(or integer ratio float) 'real)
  include)

(deftest subtypep-or-left.2
  (subtypep-result '(or integer ratio string) 'real)
  false)

(deftest subtypep-or-left.3
  (subtypep-result '(or pathname package string) 'real)
  exclude)

(deftest subtypep-or-left.4
  (subtypep-result '(or number package string) 'real)
  false)

(deftest subtypep-or-left.5
  (subtypep-result '(or (satisfies hello) number) 'real)
  false)

(deftest subtypep-or-or.1
  (subtypep-result '(or integer symbol) '(or real symbol))
  include)


;;
;;  Error
;;
'(deftest error-subtypep.1
   (subtypep 'real '(not integer))
   nil t)

'(deftest error-subtypep.2
   (subtypep
     '(complex (or (integer 0 0) (integer 1 1)))
     'complex)
   t t)

