;;
;;  subtypep
;;
(deftest subtypep5-initialize
  (progn
    (setq *subtypep!* 'subtypep-normal)
    (subtypep! nil nil nil t))
  subtypep-normal)


;;
;;  member
;;    -> (or (eql x) (eql y) ...)
;;
(deftest subtypep-member.1
  (subtypep! '(member 10 20 30) '(member 10 20 30))
  include)

(deftest subtypep-member.2
  (subtypep! '(eql 10) '(member 10 20 30))
  include)

(deftest subtypep-member.3
  (subtypep! 'integer '(member 10 20 30))
  false)
(deftest subtypep-member.4
  (subtypep! '(member 10 20 30) 'integer)
  include)

(deftest subtypep-member.5
  (subtypep! '(member 10 111) '(member 10 20 30))
  false)

(deftest subtypep-member.6
  (subtypep! '(member 10 20) '(member 10 20 30))
  include)

(deftest subtypep-member.7
  (subtypep! '(member 10 20 30) '(member 10 20))
  false)

(deftest subtypep-member.8
  (subtypep! '(member #\A 20 30) '(member 40 50))
  exclude)

(deftest subtypep-member.9
  (subtypep! 'cons '(member 40 50))
  exclude)


;;
;;  mod
;;    -> (integer 0 (value))
;;
(deftest subtypep-mod.1
  (subtypep! '(mod 100) '(mod 100))
  include)

(deftest subtypep-mod.2
  (subtypep! '(integer 0 (100)) '(mod 100))
  include)

(deftest subtypep-mod.3
  (subtypep! '(integer 0 100) '(mod 100))
  false)

(deftest subtypep-mod.4
  (subtypep! '(integer -1 10) '(mod 100))
  false)

(deftest subtypep-mod.5
  (subtypep! '(mod 100) 'integer)
  include)

(deftest subtypep-mod.6
  (subtypep! '(mod 100) '(mod 200))
  include)

(deftest subtypep-mod.7
  (subtypep! '(integer 300 *) '(mod 200))
  exclude)

(deftest subtypep-mod.8
  (subtypep! 'cons '(mod 200))
  exclude)


;;
;;  atom
;;    -> (not cons)
;;
(deftest subtypep-atom.1
  (subtypep! 'integer 'atom)
  include)

(deftest subtypep-atom.2
  (subtypep! 'atom 'atom)
  include)

(deftest subtypep-atom.3
  (subtypep! 'cons 'atom)
  exclude)

(deftest subtypep-atom.4
  (subtypep! 'null 'atom)
  include)


;;
;;  list
;;    -> (or null cons)
;;
(deftest subtypep-list.1
  (subtypep! 'list 'list)
  include)

(deftest subtypep-list.2
  (subtypep! 'null 'list)
  include)

(deftest subtypep-list.3
  (subtypep! 'cons 'list)
  include)

(deftest subtypep-list.4
  (subtypep! 'integer 'list)
  exclude)


;;
;;  boolean
;;    -> (or null (eql t))
;;
(deftest subtypep-boolean.1
  (subtypep! 'null 'boolean)
  include)

(deftest subtypep-boolean.2
  (subtypep! 'boolean 'boolean)
  include)

(deftest subtypep-boolean.3
  (subtypep! nil 'boolean)
  include)

(deftest subtypep-boolean.4
  (subtypep! t 'boolean)
  false)

(deftest subtypep-boolean.5
  (subtypep! '(eql t) 'boolean)
  include)

(deftest subtypep-boolean.6
  (subtypep! '(eql nil) 'boolean)
  include)

(deftest subtypep-boolean.7
  (subtypep! 'cons 'boolean)
  exclude)


;;
;;  sequence
;;    -> (or list (array * 1))
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
  exclude)

(deftest subtypep-sequence.7
  (subtypep! '(array t (4)) 'sequence)
  include)

(deftest subtypep-sequence.8
  (subtypep! '(array t (4 5 6)) 'sequence)
  exclude)

(deftest subtypep-sequence.9
  (subtypep! '(simple-array bit 1) 'sequence)
  include)

(deftest subtypep-sequence.10
  (subtypep! '(simple-array bit 4) 'sequence)
  exclude)

(deftest subtypep-sequence.11
  (subtypep! '(simple-array bit (4)) 'sequence)
  include)

(deftest subtypep-sequence.12
  (subtypep! '(simple-array bit (4 5 6)) 'sequence)
  exclude)

(deftest subtypep-sequence.13
  (subtypep! 'complex 'sequence)
  exclude)

(deftest subtypep-sequence.14
  (subtypep! '(array t (*)) 'sequence)
  include)

(deftest subtypep-sequence.15
  (subtypep! '(array t (* *)) 'sequence)
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


;;
;;  vector
;;    -> (array type (size))
;;
(deftest subtypep-vector.1
  (subtypep! 'vector 'vector)
  include)

(deftest subtypep-vector.2
  (subtypep! '(array * 1) 'vector)
  include)

(deftest subtypep-vector.3
  (subtypep! '(array bit 1) '(vector bit))
  include)

(deftest subtypep-vector.4
  (subtypep! '(array t 1) '(vector bit))
  exclude)

(deftest subtypep-vector.5
  (subtypep! '(array character (5)) '(vector character 5))
  include)

(deftest subtypep-vector.6
  (subtypep! '(array character (*)) '(vector character 5))
  false)

(deftest subtypep-vector.7
  (subtypep! '(array character (5)) '(vector character))
  include)

(deftest subtypep-vector.8
  (subtypep! 'cons '(vector character))
  exclude)


;;
;;  simple-vector
;;    -> (simple-array t (size))
;;
(deftest subtypep-simple-vector.1
  (subtypep! 'simple-vector 'simple-vector)
  include)

(deftest subtypep-simple-vector.2
  (subtypep! '(simple-array t 1) 'simple-vector)
  include)

(deftest subtypep-simple-vector.3
  (subtypep! '(simple-array t 1) '(simple-vector 10))
  false)

(deftest subtypep-simple-vector.4
  (subtypep! '(simple-array t 2) '(simple-vector 10))
  exclude)

(deftest subtypep-simple-vector.5
  (subtypep! '(simple-array t (*)) '(simple-vector 10))
  false)

(deftest subtypep-simple-vector.6
  (subtypep! '(simple-array t (10)) '(simple-vector *))
  include)

(deftest subtypep-simple-vector.7
  (subtypep! 'cons '(simple-vector *))
  exclude)


;;
;;  bit-vector
;;    -> (array bit (size))
;;
(deftest subtypep-bit-vector.1
  (subtypep! 'bit-vector 'bit-vector)
  include)

(deftest subtypep-bit-vector.2
  (subtypep! '(array bit 1) 'bit-vector)
  include)

(deftest subtypep-bit-vector.3
  (subtypep! '(array bit 1) '(bit-vector 10))
  false)

(deftest subtypep-bit-vector.4
  (subtypep! '(array bit 2) '(bit-vector 10))
  exclude)

(deftest subtypep-bit-vector.5
  (subtypep! '(array bit (*)) '(bit-vector 10))
  false)

(deftest subtypep-bit-vector.6
  (subtypep! '(array bit (10)) '(bit-vector *))
  include)

(deftest subtypep-bit-vector.7
  (subtypep! 'cons '(bit-vector *))
  exclude)


;;
;;  simple-bit-vector
;;    -> (simple-array bit (size))
;;
(deftest subtypep-simple-bit-vector.1
  (subtypep! 'simple-bit-vector 'simple-bit-vector)
  include)

(deftest subtypep-simple-bit-vector.2
  (subtypep! '(simple-array bit 1) 'simple-bit-vector)
  include)

(deftest subtypep-simple-bit-vector.3
  (subtypep! '(simple-array bit 1) '(simple-bit-vector 10))
  false)

(deftest subtypep-simple-bit-vector.4
  (subtypep! '(simple-array bit 2) '(simple-bit-vector 10))
  exclude)

(deftest subtypep-simple-bit-vector.5
  (subtypep! '(simple-array bit (*)) '(simple-bit-vector 10))
  false)

(deftest subtypep-simple-bit-vector.6
  (subtypep! '(simple-array bit (10)) '(simple-bit-vector *))
  include)

(deftest subtypep-simple-bit-vector.7
  (subtypep! 'cons '(simple-bit-vector *))
  exclude)


;;
;;  extended-char
;;    -> (and character (not base-char))
;;
(deftest subtypep-extended-char.1
  (subtypep! 'extended-char 'extended-char)
  include)

(deftest subtypep-extended-char.2
  (subtypep! 'character 'extended-char)
  false)

(deftest subtypep-extended-char.3
  (subtypep! 'base-char 'extended-char)
  exclude)

(deftest subtypep-extended-char.4
  (subtypep! 'standard-char 'extended-char)
  exclude)

(deftest subtypep-extended-char.5
  (subtypep! 'integer 'extended-char)
  exclude)


;;
;;  string
;;    -> (array character (size))
;;
(deftest subtypep-string.1
  (subtypep! 'string 'string)
  include)

(deftest subtypep-string.2
  (subtypep! '(array character 1) 'string)
  include)

(deftest subtypep-string.3
  (subtypep! '(array character 1) '(string 10))
  false)

(deftest subtypep-string.4
  (subtypep! '(array character 2) '(string 10))
  exclude)

(deftest subtypep-string.5
  (subtypep! '(array character (*)) '(string 10))
  false)

(deftest subtypep-string.6
  (subtypep! '(array character (10)) '(string *))
  include)

(deftest subtypep-string.7
  (subtypep! 'cons '(string *))
  exclude)


;;
;;  base-string
;;    -> (array base-char (size))
;;
(deftest subtypep-base-string.1
  (subtypep! 'base-string 'base-string)
  include)

(deftest subtypep-base-string.2
  (subtypep! '(array base-char 1) 'base-string)
  include)

(deftest subtypep-base-string.3
  (subtypep! '(array base-char 1) '(base-string 10))
  false)

(deftest subtypep-base-string.4
  (subtypep! '(array base-char 2) '(base-string 10))
  exclude)

(deftest subtypep-base-string.5
  (subtypep! '(array base-char (*)) '(base-string 10))
  false)

(deftest subtypep-base-string.6
  (subtypep! '(array base-char (10)) '(base-string *))
  include)

(deftest subtypep-base-string.7
  (subtypep! 'cons '(base-string *))
  exclude)


;;
;;  simple-string
;;    -> (simple-array character (size))
;;
(deftest subtypep-simple-string.1
  (subtypep! 'simple-string 'simple-string)
  include)

(deftest subtypep-simple-string.2
  (subtypep! '(simple-array character 1) 'simple-string)
  include)

(deftest subtypep-simple-string.3
  (subtypep! '(simple-array character 1) '(simple-string 10))
  false)

(deftest subtypep-simple-string.4
  (subtypep! '(simple-array character 2) '(simple-string 10))
  exclude)

(deftest subtypep-simple-string.5
  (subtypep! '(simple-array character (*)) '(simple-string 10))
  false)

(deftest subtypep-simple-string.6
  (subtypep! '(simple-array character (10)) '(simple-string *))
  include)

(deftest subtypep-simple-string.7
  (subtypep! 'cons '(simple-string *))
  exclude)


;;
;;  simple-base-string
;;    -> (simple-array base-char (size))
;;
(deftest subtypep-simple-base-string.1
  (subtypep! 'simple-base-string 'simple-base-string)
  include)

(deftest subtypep-simple-base-string.2
  (subtypep! '(simple-array base-char 1) 'simple-base-string)
  include)

(deftest subtypep-simple-base-string.3
  (subtypep! '(simple-array base-char 1) '(simple-base-string 10))
  false)

(deftest subtypep-simple-base-string.4
  (subtypep! '(simple-array base-char 2) '(simple-base-string 10))
  exclude)

(deftest subtypep-simple-base-string.5
  (subtypep! '(simple-array base-char (*)) '(simple-base-string 10))
  false)

(deftest subtypep-simple-base-string.6
  (subtypep! '(simple-array base-char (10)) '(simple-base-string *))
  include)

(deftest subtypep-simple-base-string.7
  (subtypep! 'cons '(simple-base-string *))
  exclude)

;;
;;  signed-byte
;;    -> (integer (lower) upper)
;;
(deftest subtypep-signed-byte.1
  (subtypep! 'signed-byte 'signed-byte)
  include)

(deftest subtypep-signed-byte.2
  (subtypep! '(signed-byte 10) '(signed-byte 10))
  include)

(deftest subtypep-signed-byte.3
  (subtypep! '(signed-byte 10) '(signed-byte 20))
  include)

(deftest subtypep-signed-byte.4
  (subtypep! '(signed-byte 20) '(signed-byte 10))
  false)

(deftest subtypep-signed-byte.5
  (subtypep! '(integer -512 511) '(signed-byte 10))
  include)

(deftest subtypep-signed-byte.6
  (subtypep! '(integer -512 (512)) '(signed-byte 10))
  include)

(deftest subtypep-signed-byte.7
  (subtypep! '(integer -513 (512)) '(signed-byte 10))
  false)

(deftest subtypep-signed-byte.8
  (subtypep! '(integer -512 (513)) '(signed-byte 10))
  false)

(deftest subtypep-signed-byte.9
  (subtypep! 'cons '(signed-byte 10))
  exclude)


;;
;;  unsigned-byte
;;    -> (integer 0 upper)
;;
(deftest subtypep-unsigned-byte.1
  (subtypep! 'unsigned-byte 'unsigned-byte)
  include)

(deftest subtypep-unsigned-byte.2
  (subtypep! '(unsigned-byte 10) '(unsigned-byte 10))
  include)

(deftest subtypep-unsigned-byte.3
  (subtypep! '(unsigned-byte 10) '(unsigned-byte 20))
  include)

(deftest subtypep-unsigned-byte.4
  (subtypep! '(unsigned-byte 20) '(unsigned-byte 10))
  false)

(deftest subtypep-unsigned-byte.5
  (subtypep! '(integer 0 (1024)) '(unsigned-byte 10))
  include)

(deftest subtypep-unsigned-byte.6
  (subtypep! '(integer -1 (1024)) '(unsigned-byte 10))
  false)

(deftest subtypep-unsigned-byte.7
  (subtypep! '(integer 0 1024) '(unsigned-byte 10))
  false)

(deftest subtypep-unsigned-byte.8
  (subtypep! 'cons '(unsigned-byte 10))
  exclude)


;;
;;  bit
;;    -> (integer 0 1)
;;
(deftest subtypep-bit.1
  (subtypep! 'bit 'bit)
  include)

(deftest subtypep-bit.2
  (subtypep! '(eql 0) 'bit)
  include)

(deftest subtypep-bit.3
  (subtypep! '(integer 1 1) 'bit)
  include)

(deftest subtypep-bit.4
  (subtypep! '(integer 1 100) 'bit)
  false)

(deftest subtypep-bit.5
  (subtypep! 'list 'bit)
  exclude)


;;
;;  fixnum
;;    -> (integer fixnum-lower fixnum-upper)
;;
(deftest subtypep-fixnum.1
  (subtypep! 'fixnum 'fixnum)
  include)

(deftest subtypep-fixnum.2
  (subtypep! 'integer 'fixnum)
  false)

(deftest subtypep-fixnum.3
  (subtypep! '(integer -20 10) 'fixnum)
  include)

(deftest subtypep-fixnum.4
  (subtypep! '(integer 0 1000000000000000000000000000000000000000) 'fixnum)
  false)

(deftest subtypep-fixnum.5
  (subtypep! 'fixnum 'signed-byte)
  include)

(deftest subtypep-fixnum.6
  (subtypep! 'fixnum 'unsigned-byte)
  false)

(deftest subtypep-fixnum.7
  (subtypep! 'cons 'fixnum)
  exclude)


;;
;;  bignum
;;    -> (and integer (not fixnum))
;;
(deftest subtypep-bignum.1
  (subtypep! 'bignum 'bignum)
  include)

(deftest subtypep-bignum.2
  (subtypep! 'fixnum 'bignum)
  exclude)

(deftest subtypep-bignum.3
  (subtypep! 'integer 'bignum)
  false)

(deftest subtypep-bignum.4
  (subtypep! 'cons 'bignum)
  exclude)

