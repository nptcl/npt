;;
;;  subtypep
;;
(deftest subtypep5-initialize
  (progn
    (setq *subtypep!* 'subtypep-normal)
    (subtypep! nil nil nil t))
  subtypep-normal)


;;
;;  sequence
;;    -> (or null cons (array * 1))
;;
(deftest subtypep-sequence.1
  (subtypep! 'sequence 'sequence)
  include)

(deftest subtypep-sequence.2
  (subtypep! 'null 'sequence)
  include)

(deftest subtypep-sequence.3
  (subtypep! 'cons 'sequence)
  include)

(deftest subtypep-sequence.4
  (subtypep! 'array 'sequence)
  false)

(deftest subtypep-sequence.5
  (subtypep! '(array t 1) 'sequence)
  include)

(deftest subtypep-sequence.6
  (subtypep! '(array t 4) 'sequence)
  false)

(deftest subtypep-sequence.7
  (subtypep! '(array t (4)) 'sequence)
  include)

(deftest subtypep-sequence.8
  (subtypep! '(array t (4 5 6)) 'sequence)
  false)

(deftest subtypep-sequence.9
  (subtypep! '(simple-array bit 1) 'sequence)
  include)

(deftest subtypep-sequence.10
  (subtypep! '(simple-array bit 4) 'sequence)
  false)

(deftest subtypep-sequence.11
  (subtypep! '(simple-array bit (4)) 'sequence)
  include)

(deftest subtypep-sequence.12
  (subtypep! '(simple-array bit (4 5 6)) 'sequence)
  false)

(deftest subtypep-sequence.13
  (subtypep! 'complex 'sequence)
  exclude)

(deftest subtypep-sequence-error.1
  (subtypep 'sequence 'array)
  nil t)

(deftest subtypep-sequence-error.2
  (subtypep 'array 'sequence)
  nil t)

(deftest subtypep-sequence-error.3
  (subtypep 'sequence '(array * 1))
  nil t)

(deftest subtypep-sequence-error.4
  (subtypep '(and sequence array) 'array)
  t t)

(deftest subtypep-sequence-error.5
  (subtypep '(and (or null cons (array * 1)) array) '(array * 1))
  t t)

(deftest subtypep-sequence-error.6
  (subtypep '(and sequence array) '(array * 1))
  t t)

(deftest subtypep-sequence-error.7
  (subtypep '(and (or sequence integer) array) '(array * 1))
  t t)

(deftest subtypep-sequence-error.8
  (subtypep! 'sequence 'null)
  false)

(deftest subtypep-sequence-error.9
  (subtypep! 'null 'sequence)
  include)

(deftest subtypep-sequence-error.10
  (subtypep! 'sequence 'array)
  false)

(deftest subtypep-sequence-error.11
  (subtypep! 'sequence 'simple-array)
  false)

(deftest subtypep-sequence-error.12
  (subtypep! 'sequence 'cons)
  false)

