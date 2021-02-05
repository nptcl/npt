;;
;;  subtypep atomic
;;
(deftest subtypep1-initialize
  (progn
    (setq *subtypep!* 'subtypep-atomic)
    (subtypep! nil nil nil t))
  subtypep-atomic)


;;
;;  invalid
;;
(deftest subtypep-invalid.1
  (subtypep! 'integer 'invalid)
  invalid)


;;
;;  clos
;;
(deftest subtypep-clos.1
  (subtypep! 'integer (find-class 'class))
  exclude)

(defclass subtypep-clos-1 () ())
(defclass subtypep-clos-2 (subtypep-clos-1) ())

(deftest subtypep-clos.2
  (subtypep! 'subtypep-clos-1 'class)
  false)

(deftest subtypep-clos.3
  (subtypep! 'subtypep-clos-1 'subtypep-clos-2)
  false)

(deftest subtypep-clos.4
  (subtypep! 'subtypep-clos-2 'subtypep-clos-1)
  include)


;;
;;  asterisk
;;
(deftest-error subtypep-asterisk.1
  (subtypep 'integer '*))


;;
;;  nil
;;
(deftest subtypep-nil.1
  (subtypep! 'integer nil)
  exclude)

(deftest subtypep-nil.2
  (subtypep! nil nil)
  include)

(deftest subtypep-nil.3
  (subtypep! t nil)
  exclude)


;;
;;  t
;;
(deftest subtypep-t.1
  (subtypep! 'real t)
  include)

(deftest subtypep-t.2
  (subtypep! nil t)
  include)

(deftest subtypep-t.3
  (subtypep! t t)
  include)


;;
;;  null
;;
(deftest subtypep-null.1
  (subtypep! 'null 'null)
  include)

(deftest subtypep-null.2
  (subtypep! 'cons 'null)
  exclude)

(deftest subtypep-null.3
  (subtypep! 'integer 'null)
  exclude)

(deftest subtypep-null.4
  (subtypep! 'nil 'null)
  exclude)


;;
;;  hash-table
;;
(deftest subtypep-hash-table.1
  (subtypep! 'hash-table 'hash-table)
  include)

(deftest subtypep-hash-table.2
  (subtypep! 'real 'hash-table)
  exclude)


;;
;;  symbol
;;
(deftest subtypep-symbol.1
  (subtypep! 'symbol 'symbol)
  include)

(deftest subtypep-symbol.2
  (subtypep! 'null 'symbol)
  include)

(deftest subtypep-symbol.3
  (subtypep! 'keyword 'symbol)
  include)

(deftest subtypep-symbol.4
  (subtypep! 'integer 'symbol)
  exclude)


;;
;;  keyword
;;
(deftest subtypep-keyword.1
  (subtypep! 'keyword 'keyword)
  include)

(deftest subtypep-keyword.2
  (subtypep! 'null 'keyword)
  exclude)

(deftest subtypep-keyword.3
  (subtypep! 'integer 'keyword)
  exclude)

(deftest subtypep-keyword.4
  (subtypep! 'keyword 'symbol)
  include)


;;
;;  package
;;
(deftest subtypep-package.1
  (subtypep! 'package 'package)
  include)

(deftest subtypep-package.2
  (subtypep! 'real 'package)
  exclude)


;;
;;  random-state
;;
(deftest subtypep-random-state.1
  (subtypep! 'random-state 'random-state)
  include)

(deftest subtypep-random-state.2
  (subtypep! 'real 'random-state)
  exclude)


;;
;;  readtable
;;
(deftest subtypep-readtable.1
  (subtypep! 'readtable 'readtable)
  include)

(deftest subtypep-readtable.2
  (subtypep! 'real 'readtable)
  exclude)


;;
;;  pathname
;;
(deftest subtypep-pathname.1
  (subtypep! 'pathname 'pathname)
  include)

(deftest subtypep-pathname.2
  (subtypep! 'logical-pathname 'pathname)
  include)

(deftest subtypep-pathname.3
  (subtypep! 'cons 'pathname)
  exclude)


;;
;;  logical-pathname
;;
(deftest subtypep-logical-pathname.1
  (subtypep! 'logical-pathname 'logical-pathname)
  include)

(deftest subtypep-logical-pathname.2
  (subtypep! 'pathname 'logical-pathname)
  false)

(deftest subtypep-logical-pathname.3
  (subtypep! 'cons 'logical-pathname)
  exclude)


;;
;;  array
;;
(deftest subtypep-array.1
  (subtypep! 'array 'array)
  include)

(deftest subtypep-array.2
  (subtypep! 'simple-array 'array)
  include)

(deftest subtypep-array-element-type.1
  (subtypep! '(array bit) '(array *))
  include)

(deftest subtypep-array-element-type.2
  (subtypep! '(array *) '(array bit))
  false)

(deftest subtypep-array-element-type.3
  (subtypep! '(array bit) '(array *))
  include)

(deftest subtypep-array-element-type.4
  (subtypep! '(array t) '(array bit))
  exclude)

(deftest subtypep-array-element-type.5
  (subtypep! '(simple-array t) '(array integer))  ;; upgraded integer -> t
  include)

(deftest subtypep-array-dimension-fixnum.1
  (subtypep! '(array * 1) '(array * *))
  include)

(deftest subtypep-array-dimension-fixnum.2
  (subtypep! '(array t 4) '(array * 4))
  include)

(deftest subtypep-array-dimension-fixnum.3
  (subtypep! '(array * 4) '(array t 4))
  false)

(deftest subtypep-array-dimension-fixnum.4
  (subtypep! '(array * 3) '(array * 4))
  exclude)

(deftest subtypep-array-dimension-fixnum.5
  (subtypep! '(array * (1 2 3)) '(array * 4))
  exclude)

(deftest subtypep-array-dimension-fixnum.6
  (subtypep! '(array * (1 2 3 4)) '(array * 4))
  include)

(deftest subtypep-array-dimension-vector.1
  (subtypep! '(array * (2 3 4)) '(array * (2 3 4)))
  include)

(deftest subtypep-array-dimension-vector.2
  (subtypep! '(array * (2 3)) '(array * (2 3 4)))
  exclude)

(deftest subtypep-array-dimension-vector.3
  (subtypep! '(array * (2 3 4 5)) '(array * (2 3 4)))
  exclude)

(deftest subtypep-array-dimension-vector.4
  (subtypep! '(array * (2 8 4)) '(array * (2 3 4)))
  exclude)

(deftest subtypep-array-dimension-vector.5
  (subtypep! '(array * (2 * 4)) '(array * (2 3 4)))
  false)

(deftest subtypep-array-dimension-vector.6
  (subtypep! '(array * (2 3 4)) '(array * (2 * 4)))
  include)

(deftest subtypep-array-dimension-vector.7
  (subtypep! '(array * (2 * 4)) '(array * (2 * *)))
  include)

(deftest subtypep-array-error.1
  (subtypep! '(array t (*)) '(array t (10)))
  false)

'(deftest subtypep-array-error.2
   (subtypep! '(array t (*)) '(not (array t (10))))
   false)


;;
;;  simple-array
;;
(deftest subtypep-simple-array.1
  (subtypep! 'simple-array 'simple-array)
  include)

(deftest subtypep-simple-array.2
  (subtypep! 'array 'simple-array)
  false)

(deftest subtypep-simple-array.3
  (subtypep! '(array t) '(simple-array bit))
  exclude)

(deftest subtypep-simple-array.4
  (subtypep! '(array *) '(simple-array t))
  false)

(deftest subtypep-simple-array.5
  (subtypep! '(array t) '(simple-array *))
  false)

(deftest subtypep-simple-array-element-type.1
  (subtypep! '(simple-array bit) '(simple-array *))
  include)

(deftest subtypep-simple-array-element-type.2
  (subtypep! '(simple-array *) '(simple-array bit))
  false)

(deftest subtypep-simple-array-element-type.3
  (subtypep! '(simple-array bit) '(simple-array *))
  include)

(deftest subtypep-simple-array-element-type.4
  (subtypep! '(simple-array t) '(simple-array bit))
  exclude)

(deftest subtypep-simple-array-element-type.5
  (subtypep! '(simple-array t) '(simple-array integer))  ;; upgraded integer -> t
  include)

(deftest subtypep-simple-array-dimension-fixnum.1
  (subtypep! '(simple-array * 1) '(simple-array * *))
  include)

(deftest subtypep-simple-array-dimension-fixnum.2
  (subtypep! '(simple-array t 4) '(simple-array * 4))
  include)

(deftest subtypep-simple-array-dimension-fixnum.3
  (subtypep! '(simple-array * 4) '(simple-array t 4))
  false)

(deftest subtypep-simple-array-dimension-fixnum.4
  (subtypep! '(simple-array * 3) '(simple-array * 4))
  exclude)

(deftest subtypep-simple-array-dimension-fixnum.5
  (subtypep! '(simple-array * (1 2 3)) '(simple-array * 4))
  exclude)

(deftest subtypep-simple-array-dimension-fixnum.6
  (subtypep! '(simple-array * (1 2 3 4)) '(simple-array * 4))
  include)

(deftest subtypep-simple-array-dimension-vector.1
  (subtypep! '(simple-array * (2 3 4)) '(simple-array * (2 3 4)))
  include)

(deftest subtypep-simple-array-dimension-vector.2
  (subtypep! '(simple-array * (2 3)) '(simple-array * (2 3 4)))
  exclude)

(deftest subtypep-simple-array-dimension-vector.3
  (subtypep! '(simple-array * (2 3 4 5)) '(simple-array * (2 3 4)))
  exclude)

(deftest subtypep-simple-array-dimension-vector.4
  (subtypep! '(simple-array * (2 8 4)) '(simple-array * (2 3 4)))
  exclude)

(deftest subtypep-simple-array-dimension-vector.5
  (subtypep! '(simple-array * (2 * 4)) '(simple-array * (2 3 4)))
  false)

(deftest subtypep-simple-array-dimension-vector.6
  (subtypep! '(simple-array * (2 3 4)) '(simple-array * (2 * 4)))
  include)

(deftest subtypep-simple-array-dimension-vector.7
  (subtypep! '(simple-array * (2 * 4)) '(array * (2 * *)))
  include)


;;
;;  character
;;
(deftest subtypep-character.1
  (subtypep! 'character 'character)
  include)

(deftest subtypep-character.2
  (subtypep! 'base-char 'character)
  include)

(deftest subtypep-character.3
  (subtypep! 'standard-char 'character)
  include)

(deftest subtypep-character.4
  (subtypep! 'integer 'character)
  exclude)


;;
;;  base-char
;;
(deftest subtypep-base-char.1
  (subtypep! 'base-char 'base-char)
  include)

(deftest subtypep-base-char.2
  (subtypep! 'character 'base-char)
  false)

(deftest subtypep-base-char.3
  (subtypep! 'standard-char 'base-char)
  include)

(deftest subtypep-base-char.4
  (subtypep! 'integer 'base-char)
  exclude)


;;
;;  standard-char
;;
(deftest subtypep-standard-char.1
  (subtypep! 'standard-char 'standard-char)
  include)

(deftest subtypep-standard-char.2
  (subtypep! 'character 'standard-char)
  false)

(deftest subtypep-standard-char.3
  (subtypep! 'base-char 'standard-char)
  false)

(deftest subtypep-standard-char.4
  (subtypep! 'integer 'standard-char)
  exclude)


;;
;;  integer
;;
(deftest subtypep-integer.1
  (subtypep! 'integer 'integer)
  include)

(deftest subtypep-integer.2
  (subtypep! '(integer * *) '(integer))
  include)

(deftest subtypep-integer.3
  (subtypep! '(integer 2 3) '(integer 4 5))
  exclude)

(deftest subtypep-integer.4
  (subtypep! '(integer 2 100) '(integer 4 5))
  false)

(deftest subtypep-integer.5
  (subtypep! '(integer 2 100) '(integer 0 *))
  include)

(deftest subtypep-integer.6
  (subtypep! '(rational 2 3) 'integer)
  false)

(deftest subtypep-integer.7
  (subtypep! '(rational 2 3) '(integer 4 5))
  exclude)

(deftest subtypep-integer.8
  (subtypep! 'real 'integer)
  false)

(deftest subtypep-integer.9
  (subtypep! '(real 2 3) '(integer 4 5))
  exclude)

(deftest subtypep-integer.10
  (subtypep! 'number '(integer 4 5))
  false)

(deftest subtypep-integer.11
  (subtypep! 'cons 'integer)
  exclude)


;;
;;  rational
;;
(deftest subtypep-rational.1
  (subtypep! 'rational 'rational)
  include)

(deftest subtypep-rational.2
  (subtypep! '(rational) '(rational * *))
  include)

(deftest subtypep-rational.3
  (subtypep! '(rational 10 20) '(rational * 15))
  false)

(deftest subtypep-rational.4
  (subtypep! '(rational 10 20) '(rational * 30))
  include)

(deftest subtypep-rational.5
  (subtypep! '(rational 40 *) '(rational * 30))
  exclude)

(deftest subtypep-rational.6
  (subtypep! 'integer 'rational)
  include)

(deftest subtypep-rational.7
  (subtypep! '(integer 10 20) '(rational * 15))
  false)

(deftest subtypep-rational.8
  (subtypep! '(integer 10 20) '(rational * 30))
  include)

(deftest subtypep-rational.9
  (subtypep! '(integer 40 *) '(rational * 30))
  exclude)

(deftest subtypep-rational.10
  (subtypep! '(real 10 20) '(rational * 30))
  false)

(deftest subtypep-rational.11
  (subtypep! '(real 10 20) '(rational * 0))
  exclude)

(deftest subtypep-rational.12
  (subtypep! 'ratio 'rational)
  include)

(deftest subtypep-rational.13
  (subtypep! 'ratio '(rational 10 20))
  false)

(deftest subtypep-rational.14
  (subtypep! 'number 'rational)
  false)


;;
;;  real
;;
(deftest subtypep-real.1
  (subtypep! 'real 'real)
  include)

(deftest subtypep-real.2
  (subtypep! '(real * *) 'real)
  include)

(deftest subtypep-real.3
  (subtypep! '(real 10 20) '(real 30 40))
  exclude)

(deftest subtypep-real.4
  (subtypep! '(real 10 *) '(real 30 40))
  false)

(deftest subtypep-real.5
  (subtypep! 'integer 'real)
  include)

(deftest subtypep-real.6
  (subtypep! 'integer '(real 0))
  false)

(deftest subtypep-real.7
  (subtypep! 'rational 'real)
  include)

(deftest subtypep-real.8
  (subtypep! 'rational 'real)
  include)

(deftest subtypep-real.9
  (subtypep! 'short-float 'real)
  include)

(deftest subtypep-real.10
  (subtypep! 'single-float 'real)
  include)

(deftest subtypep-real.11
  (subtypep! 'double-float 'real)
  include)

(deftest subtypep-real.12
  (subtypep! 'long-float 'real)
  include)

(deftest subtypep-real.13
  (subtypep! 'float 'real)
  include)

(deftest subtypep-real.14
  (subtypep! 'complex 'real)
  exclude)


;;
;;  float
;;
(deftest subtypep-float.1
  (subtypep! 'float 'float)
  include)

(deftest subtypep-float.2
  (subtypep! 'float '(float * *))
  include)

(deftest subtypep-float.3
  (subtypep! '(float 10.0 20.0) '(float 0.0 40.0))
  include)

(deftest subtypep-float.4
  (subtypep! '(float * 20.0) '(float 0.0 40.0))
  false)

(deftest subtypep-float.5
  (subtypep! '(float 10.0 20.0) '(float 30.0 40.0))
  exclude)

(deftest subtypep-float.6
  (subtypep! '(real 10 200) '(float 30.0 40.0))
  false)

(deftest subtypep-float.7
  (subtypep! '(real 10 20) '(float 30.0 40.0))
  exclude)

(deftest subtypep-float.8
  (subtypep! '(single-float 0.0f0 100.0f0) '(float 30.0 40.0))
  false)

(deftest subtypep-float.9
  (subtypep! '(double-float 0.0d0 100.0d0) '(float 30.0 40.0))
  false)

(deftest subtypep-float.10
  (subtypep! '(long-float 0.0L0 100.0L0) '(float 30.0 40.0))
  false)

(deftest subtypep-float.11
  (subtypep! 'number '(float 30.0 40.0))
  false)

(deftest subtypep-float.12
  (subtypep! 'cons 'float)
  exclude)


;;
;;  single-float
;;
(deftest subtypep-single-float.1
  (subtypep! 'single-float 'single-float)
  include)

(deftest subtypep-single-float.2
  (subtypep! '(single-float * *) 'single-float)
  include)

(deftest subtypep-single-float.3
  (subtypep! '(single-float 10.0f0 20.0f0) '(single-float 0.0f0 100.0f0))
  include)

(deftest subtypep-single-float.4
  (subtypep! '(single-float 10.0f0 20.0f0) '(single-float 50.0f0 100.0f0))
  exclude)

(deftest subtypep-single-float.5
  (subtypep! '(single-float 10.0f0 *) '(single-float 50.0f0 100.0f0))
  false)

(deftest subtypep-single-float.6
  (subtypep! '(real 10.0f0 *) '(single-float 50.0f0 *))
  false)

(deftest subtypep-single-float.7
  (subtypep! '(float 10.0f0 *) '(single-float 50.0f0 *))
  false)

(deftest subtypep-single-float.8
  (subtypep! 'number 'single-float)
  false)

(deftest subtypep-single-float.9
  (subtypep! 'package 'single-float)
  exclude)


;;
;;  double-float
;;
(deftest subtypep-double-float.1
  (subtypep! 'double-float 'double-float)
  include)

(deftest subtypep-double-float.2
  (subtypep! '(double-float * *) 'double-float)
  include)

(deftest subtypep-double-float.3
  (subtypep! '(double-float 10.0d0 20.0d0) '(double-float 0.0d0 100.0d0))
  include)

(deftest subtypep-double-float.4
  (subtypep! '(double-float 10.0d0 20.0d0) '(double-float 50.0d0 100.0d0))
  exclude)

(deftest subtypep-double-float.5
  (subtypep! '(double-float 10.0d0 *) '(double-float 50.0d0 100.0d0))
  false)

(deftest subtypep-double-float.6
  (subtypep! '(real 10.0d0 *) '(double-float 50.0d0 *))
  false)

(deftest subtypep-double-float.7
  (subtypep! '(float 10.0d0 *) '(double-float 50.0d0 *))
  false)

(deftest subtypep-double-float.8
  (subtypep! 'number 'double-float)
  false)

(deftest subtypep-double-float.9
  (subtypep! 'package 'double-float)
  exclude)


;;
;;  long-float
;;
(deftest subtypep-long-float.1
  (subtypep! 'long-float 'long-float)
  include)

(deftest subtypep-long-float.2
  (subtypep! '(long-float * *) 'long-float)
  include)

(deftest subtypep-long-float.3
  (subtypep! '(long-float 10.0L0 20.0L0) '(long-float 0.0L0 100.0L0))
  include)

(deftest subtypep-long-float.4
  (subtypep! '(long-float 10.0L0 20.0L0) '(long-float 50.0L0 100.0L0))
  exclude)

(deftest subtypep-long-float.5
  (subtypep! '(long-float 10.0L0 *) '(long-float 50.0L0 100.0L0))
  false)

(deftest subtypep-long-float.6
  (subtypep! '(real 10.0L0 *) '(long-float 50.0L0 *))
  false)

(deftest subtypep-long-float.7
  (subtypep! '(float 10.0L0 *) '(long-float 50.0L0 *))
  false)

(deftest subtypep-long-float.8
  (subtypep! 'number 'long-float)
  false)

(deftest subtypep-long-float.9
  (subtypep! 'package 'long-float)
  exclude)


;;
;;  number
;;
(deftest subtypep-number.1
  (subtypep! 'number 'number)
  include)

(deftest subtypep-number.2
  (subtypep! 'complex 'number)
  include)

(deftest subtypep-number.3
  (subtypep! 'ratio 'number)
  include)

(deftest subtypep-number.4
  (subtypep! 'integer 'number)
  include)

(deftest subtypep-number.5
  (subtypep! 'rational 'number)
  include)

(deftest subtypep-number.6
  (subtypep! 'real 'number)
  include)

(deftest subtypep-number.7
  (subtypep! 'float 'number)
  include)

(deftest subtypep-number.8
  (subtypep! 'short-float 'number)
  include)

(deftest subtypep-number.9
  (subtypep! 'single-float 'number)
  include)

(deftest subtypep-number.10
  (subtypep! 'double-float 'number)
  include)

(deftest subtypep-number.11
  (subtypep! 'long-float 'number)
  include)

(deftest subtypep-number.12
  (subtypep! '(real 10 20) 'number)
  include)

(deftest subtypep-number.13
  (subtypep! 'cons 'number)
  exclude)


;;
;;  ratio
;;
(deftest subtypep-ratio.1
  (subtypep! 'ratio 'ratio)
  include)

(deftest subtypep-ratio.2
  (subtypep! 'number 'ratio)
  false)

(deftest subtypep-ratio.3
  (subtypep! 'real 'ratio)
  false)

(deftest subtypep-ratio.4
  (subtypep! 'rational 'ratio)
  false)

(deftest subtypep-ratio.5
  (subtypep! 'integer 'ratio)
  exclude)


;;
;;  stream
;;
(deftest subtypep-stream.1
  (subtypep! 'stream 'stream)
  include)

(deftest subtypep-stream.2
  (subtypep! 'broadcast-stream 'stream)
  include)

(deftest subtypep-stream.3
  (subtypep! 'concatenated-stream 'stream)
  include)

(deftest subtypep-stream.4
  (subtypep! 'echo-stream 'stream)
  include)

(deftest subtypep-stream.5
  (subtypep! 'file-stream 'stream)
  include)

(deftest subtypep-stream.6
  (subtypep! 'string-stream 'stream)
  include)

(deftest subtypep-stream.7
  (subtypep! 'synonym-stream 'stream)
  include)

(deftest subtypep-stream.8
  (subtypep! 'two-way-stream 'stream)
  include)

(deftest subtypep-stream.9
  (subtypep! 'cons 'stream)
  exclude)


;;
;;  broadcast-stream
;;
(deftest subtypep-broadcast-stream.1
  (subtypep! 'broadcast-stream 'broadcast-stream)
  include)

(deftest subtypep-broadcast-stream.2
  (subtypep! 'stream 'broadcast-stream)
  false)

(deftest subtypep-broadcast-stream.3
  (subtypep! 'integer 'broadcast-stream)
  exclude)


;;
;;  concatenated-stream
;;
(deftest subtypep-concatenated-stream.1
  (subtypep! 'concatenated-stream 'concatenated-stream)
  include)

(deftest subtypep-concatenated-stream.2
  (subtypep! 'stream 'concatenated-stream)
  false)

(deftest subtypep-concatenated-stream.3
  (subtypep! 'integer 'concatenated-stream)
  exclude)


;;
;;  echo-stream
;;
(deftest subtypep-echo-stream.1
  (subtypep! 'echo-stream 'echo-stream)
  include)

(deftest subtypep-echo-stream.2
  (subtypep! 'stream 'echo-stream)
  false)

(deftest subtypep-echo-stream.3
  (subtypep! 'integer 'echo-stream)
  exclude)


;;
;;  file-stream
;;
(deftest subtypep-file-stream.1
  (subtypep! 'file-stream 'file-stream)
  include)

(deftest subtypep-file-stream.2
  (subtypep! 'stream 'file-stream)
  false)

(deftest subtypep-file-stream.3
  (subtypep! 'integer 'file-stream)
  exclude)


;;
;;  string-stream
;;
(deftest subtypep-string-stream.1
  (subtypep! 'string-stream 'string-stream)
  include)

(deftest subtypep-string-stream.2
  (subtypep! 'stream 'string-stream)
  false)

(deftest subtypep-string-stream.3
  (subtypep! 'integer 'string-stream)
  exclude)


;;
;;  synonym-stream
;;
(deftest subtypep-synonym-stream.1
  (subtypep! 'synonym-stream 'synonym-stream)
  include)

(deftest subtypep-synonym-stream.2
  (subtypep! 'stream 'synonym-stream)
  false)

(deftest subtypep-synonym-stream.3
  (subtypep! 'integer 'synonym-stream)
  exclude)


;;
;;  two-way-stream
;;
(deftest subtypep-two-way-stream.1
  (subtypep! 'two-way-stream 'two-way-stream)
  include)

(deftest subtypep-two-way-stream.2
  (subtypep! 'stream 'two-way-stream)
  false)

(deftest subtypep-two-way-stream.3
  (subtypep! 'broadcast-stream 'two-way-stream)
  exclude)

(deftest subtypep-two-way-stream.4
  (subtypep! 'integer 'two-way-stream)
  exclude)

