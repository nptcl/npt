;;
;;  compile-type
;;

;;
;;  system-type
;;
(deftest compile-type-type.1
  (type-compile lisp-system::type)
  lisp-system::type)

(deftest compile-type-clos.1
  (type-compile standard-class)
  standard-class)

(deftest compile-type-asterisk.1
  (type-compile *)
  *)


;;
;;  conpound-type
;;
(deftest compile-type-and.1
  (type-compile (and))
  (and))

(deftest compile-type-and.2
  (type-compile (and integer string))
  (and integer string))

(deftest compile-type-or.1
  (type-compile (or))
  (or))

(deftest compile-type-or.2
  (type-compile (or integer string))
  (or integer string))

(deftest compile-type-eql.1
  (type-compile (eql #\A))
  (eql #\A))

(deftest compile-type-member.1
  (type-compile (member))
  (member))

(deftest compile-type-member.2
  (type-compile (member 10 20 #\z))
  (member 10 20 #\z))

(deftest compile-type-mod.1
  (type-compile (mod 256))
  (mod 256))

(deftest compile-type-not.1
  (type-compile (not integer))
  (not integer))

(deftest compile-type-satisfies.1
  (type-compile (satisfies atom))
  (satisfies atom))


;;
;;  extract-type
;;
(deftest compile-type-atom.1
  (type-compile atom)
  atom)

(deftest compile-type-list.1
  (type-compile list)
  list)

(deftest compile-type-boolean.1
  (type-compile boolean)
  boolean)

(deftest compile-type-vector.1
  (type-compile vector)
  vector)

(deftest compile-type-vector.2
  (type-compile (vector t 10))
  (vector t 10))

(deftest compile-type-simple-vector.1
  (type-compile simple-vector)
  simple-vector)

(deftest compile-type-simple-vector.2
  (type-compile (simple-vector 10))
  (simple-vector 10))

(deftest compile-type-bit-vector.1
  (type-compile bit-vector)
  bit-vector)

(deftest compile-type-bit-vector.2
  (type-compile (bit-vector 10))
  (bit-vector 10))

(deftest compile-type-simple-bit-vector.1
  (type-compile simple-bit-vector)
  simple-bit-vector)

(deftest compile-type-simple-bit-vector.2
  (type-compile (simple-bit-vector 10))
  (simple-bit-vector 10))

(deftest compile-type-extended-char.1
  (type-compile extended-char)
  extended-char)

(deftest compile-type-string.1
  (type-compile string)
  string)

(deftest compile-type-string.2
  (type-compile (string 11))
  (string 11))

(deftest compile-type-base-string.1
  (type-compile base-string)
  base-string)

(deftest compile-type-base-string.2
  (type-compile (base-string 11))
  (base-string 11))

(deftest compile-type-simple-string.1
  (type-compile simple-string)
  simple-string)

(deftest compile-type-simple-string.2
  (type-compile (simple-string 11))
  (simple-string 11))

(deftest compile-type-simple-base-string.1
  (type-compile simple-base-string)
  simple-base-string)

(deftest compile-type-simple-base-string.2
  (type-compile (simple-base-string 11))
  (simple-base-string 11))

(deftest compile-type-signed-byte.1
  (type-compile signed-byte)
  signed-byte)

(deftest compile-type-signed-byte.2
  (type-compile (signed-byte 20))
  (signed-byte 20))

(deftest compile-type-unsigned-byte.1
  (type-compile unsigned-byte)
  unsigned-byte)

(deftest compile-type-unsigned-byte.2
  (type-compile (unsigned-byte 20))
  (unsigned-byte 20))

(deftest compile-type-bit.1
  (type-compile bit)
  bit)

(deftest compile-type-fixnum.1
  (type-compile fixnum)
  fixnum)

(deftest compile-type-bignum.1
  (type-compile bignum)
  bignum)


;;
;;  atomic-type
;;
(deftest compile-type-nil.1
  (type-compile nil)
  nil)

(deftest compile-type-t.1
  (type-compile t)
  t)

(deftest compile-type-null.1
  (type-compile null)
  null)

(deftest compile-type-cons.1
  (type-compile cons)
  cons)

(deftest compile-type-cons.2
  (type-compile (cons atom t))
  (cons atom t))

(deftest compile-type-hash-table.1
  (type-compile hash-table)
  hash-table)

(deftest compile-type-symbol.1
  (type-compile symbol)
  symbol)

(deftest compile-type-keyword.1
  (type-compile keyword)
  keyword)

(deftest compile-type-package.1
  (type-compile package)
  package)

(deftest compile-type-random-state.1
  (type-compile random-state)
  random-state)

(deftest compile-type-readtable.1
  (type-compile readtable)
  readtable)

(deftest compile-type-function.1
  (type-compile function)
  function)

(deftest compile-type-function.2
  (type-compile (function (integer string) (values t t nil &rest t)))
  (function (integer string) (values t t nil &rest t)))

(deftest compile-type-compiled-function.1
  (type-compile compiled-function)
  compiled-function)

(deftest compile-type-compiled-function.2
  (type-compile (compiled-function (integer string) (values t t nil &rest t)))
  (compiled-function (integer string) (values t t nil &rest t)))

(deftest compile-type-pathname.1
  (type-compile pathname)
  pathname)

(deftest compile-type-logical-pathname.1
  (type-compile logical-pathname)
  logical-pathname)

(deftest compile-type-sequence.1
  (type-compile sequence)
  sequence)

(deftest compile-type-array.1
  (type-compile array)
  array)

(deftest compile-type-array.2
  (type-compile (array t 4))
  (array t 4))

(deftest compile-type-array.3
  (type-compile (array character (3 4 5)))
  (array character (3 4 5)))

(deftest compile-type-simple-array.1
  (type-compile simple-array)
  simple-array)

(deftest compile-type-simple-array.2
  (type-compile (simple-array t 4))
  (simple-array t 4))

(deftest compile-type-simple-array.3
  (type-compile (simple-array character (3 4 5)))
  (simple-array character (3 4 5)))

(deftest compile-type-character.1
  (type-compile character)
  character)

(deftest compile-type-base-char.1
  (type-compile base-char)
  base-char)

(deftest compile-type-standard-char.1
  (type-compile standard-char)
  standard-char)

(deftest compile-type-number.1
  (type-compile number)
  number)

(deftest compile-type-real.1
  (type-compile real)
  real)

(deftest compile-type-real.2
  (type-compile (real * 4))
  (real * 4))

(deftest compile-type-real.3
  (type-compile (real (-3.5) 7/6))
  (real (-3.5) 7/6))

(deftest compile-type-rational.1
  (type-compile rational)
  rational)

(deftest compile-type-rational.2
  (type-compile (rational 3 (4)))
  (rational 3 (4)))

(deftest compile-type-ratio.1
  (type-compile ratio)
  ratio)

(deftest compile-type-integer.1
  (type-compile integer)
  integer)

(deftest compile-type-integer.2
  (type-compile (integer (5) 6))
  (integer (5) 6))

(deftest compile-type-complex.1
  (type-compile complex)
  complex)

(deftest compile-type-complex.2
  (type-compile (complex double-float))
  (complex double-float))

(deftest compile-type-float.1
  (type-compile float)
  float)

(deftest compile-type-float.2
  (type-compile (float -4.5 (6.7)))
  (float -4.5 (6.7)))

(deftest compile-type-single-float.1
  (type-compile single-float)
  single-float)

(deftest compile-type-single-float.2
  (type-compile (single-float -4.5 (6.7)))
  (single-float -4.5 (6.7)))

(deftest compile-type-double-float.1
  (type-compile double-float)
  double-float)

(deftest compile-type-double-float.2
  (type-compile (double-float -4.5d0 (6.7d0)))
  (double-float -4.5d0 (6.7d0)))

(deftest compile-type-long-float.1
  (type-compile long-float)
  long-float)

(deftest compile-type-long-float.2
  (type-compile (long-float -4.5L0 (6.7L0)))
  (long-float -4.5L0 (6.7L0)))

(deftest compile-type-restart.1
  (type-compile restart)
  restart)

(deftest compile-type-stream.1
  (type-compile stream)
  stream)

(deftest compile-type-broadcast-stream.1
  (type-compile broadcast-stream)
  broadcast-stream)

(deftest compile-type-concatenated-stream.1
  (type-compile concatenated-stream)
  concatenated-stream)

(deftest compile-type-echo-stream.1
  (type-compile echo-stream)
  echo-stream)

(deftest compile-type-file-stream.1
  (type-compile file-stream)
  file-stream)

(deftest compile-type-string-stream.1
  (type-compile string-stream)
  string-stream)

(deftest compile-type-synonym-stream.1
  (type-compile synonym-stream)
  synonym-stream)

(deftest compile-type-two-way-stream.1
  (type-compile two-way-stream)
  two-way-stream)

