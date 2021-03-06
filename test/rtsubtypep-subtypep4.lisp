;;
;;  subtypep
;;
(deftest subtypep4-initialize
  (progn
    (setq *subtypep!* 'subtypep-normal)
    (subtypep! nil nil nil t))
  subtypep-normal)


;;
;;  and-right
;;
(deftest subtypep-and-right.1
  (subtypep! 'integer '(and))
  include)

(deftest subtypep-and-right.2
  (subtypep! 'integer '(and real cons integer))
  exclude)

(deftest subtypep-and-right.3
  (subtypep! 'rational '(and invalid real integer))
  invalid)

(deftest subtypep-and-right.4
  (subtypep! 'integer '(and real rational integer))
  include)

(deftest subtypep-and-right.5
  (subtypep! 'integer '(and real (integer 0 *) integer))
  false)

(deftest subtypep-and-right.6
  (subtypep! 'integer '(and invalid cons integer))
  exclude)

(deftest subtypep-and-right.7
  (subtypep! 'integer '(and invalid (integer 0 *)))
  invalid)


;;
;;  or-right
;;
(deftest subtypep-or-right.1
  (subtypep! 'integer '(or))
  exclude)

(deftest subtypep-or-right.2
  (subtypep! 'integer '(or real readtable cons))
  include)

(deftest subtypep-or-right.3
  (subtypep! 'integer '(or invalid real cons))
  include)

(deftest subtypep-or-right.4
  (subtypep! 'integer '(or invalid readtable cons))
  invalid)

(deftest subtypep-or-right.5
  (subtypep! 'integer '(or pathname readtable cons))
  exclude)

(deftest subtypep-or-right.6
  (subtypep! 'integer '(or pathname (integer * 0) cons))
  false)


;;
;;  and-left
;;
(deftest subtypep-and-left.1
  (subtypep! '(and) 'real)
  false)

(deftest subtypep-and-left.2
  (subtypep! '(and integer rational float) 'real)
  include)

(deftest subtypep-and-left.3
  (subtypep! '(and cons rational float) 'real)
  include)

(deftest subtypep-and-left.4
  (subtypep! '(and (integer 0 *) rational float) 'real)
  include)

(deftest subtypep-and-left.5
  (subtypep! '(and integer cons invalid) 'real)
  include)

(deftest subtypep-and-left.6
  (subtypep! '(and integer rational) 'cons)
  exclude)

(deftest subtypep-and-left.7
  (subtypep! '(and integer rational invalid) 'cons)
  exclude)

(deftest subtypep-and-left.8
  (subtypep! '(and invalid invalid invalid) 'integer)
  invalid)

(deftest subtypep-and-left.9
  (subtypep! '(and rational real) 'integer)
  false)


;;
;;  or-left
;;
(deftest subtypep-or-left.1
  (subtypep! '(or integer ratio float) 'real)
  include)

(deftest subtypep-or-left.2
  (subtypep! '(or integer ratio cons) 'real)
  false)

(deftest subtypep-or-left.3
  (subtypep! '(or pathname package cons) 'real)
  exclude)

(deftest subtypep-or-left.4
  (subtypep! '(or number package cons) 'real)
  false)

(deftest subtypep-or-left.5
  (subtypep! '(or (satisfies hello) number) 'real)
  invalid)


;;
;;  or-or
;;
(deftest subtypep-or-or.1
  (subtypep! '(or integer symbol) '(or real symbol))
  include)


;;
;;  number
;;
(deftest subtypep-number-integer.1
  (subtypep-number t)
  t)

(deftest subtypep-number-integer.2
  (subtypep-number 'integer)
  (or integer
      (and (not real)
           integer)))

(deftest subtypep-number-integer.3
  (subtypep-number '(or (integer 10 40) (integer 30 60)))
  (or (integer 10 60)
      (and (not real)
           (or (integer 10 40) (integer 30 60)))))

(deftest subtypep-number-integer.4
  (subtypep! '(integer 20 50)
             '(or (integer 10 40) (integer 30 60)))
  include)

(deftest subtypep-number-integer.5
  (subtypep! '(integer 20 50)
             '(or (integer 10 40) (integer 30 60))
             nil 'subtypep-compound)
  false)

(deftest subtypep-number-rational.1
  (subtypep-number '(rational 10 20))
  (or (integer 10 20)
      (rational 10 20)
      (and (not real)
           (rational 10 20))))

(deftest subtypep-number-rational.2
  (subtypep-number '(or (integer 10 40)
                        (rational 30 60)))
  (or (integer 10 60)
      (rational 30 60)
      (and (not real)
           (or (integer 10 40)
               (rational 30 60)))))

(deftest subtypep-number-real.1
  (subtypep-number '(or (real 10 40)
                        (single-float 20.0f0 50.0f0)))
  (or (integer 10 40)
      (rational 10 40)
      (short-float 10.0s0 40.0s0)
      (single-float 10.0f0 50.0f0)
      (double-float 10.0d0 40.0d0)
      (long-float 10.0L0 40.0L0)
      (and (not real)
           (or (real 10 40)
               (single-float 20.0f0 50.0f0)))))

