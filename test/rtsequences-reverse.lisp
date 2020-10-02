;;
;;  ANSI COMMON LISP: 17. Sequences
;;

;;
;;  Function REVERSE
;;
(deftest reverse-list.1
  (reverse nil)
  nil)

(deftest reverse-list.2
  (reverse '(10 20 30 40))
  (40 30 20 10))

(deftest reverse-list.3
  (let ((a '(10 20 30 40)))
    (reverse a)
    (equal a '(10 20 30 40)))
  t)

(deftest reverse-vector.1
  (reverse #())
  #())

(deftest reverse-vector.2
  (reverse #(a))
  #(a))

(deftest reverse-vector.3
  (reverse #(a b c d))
  #(d c b a))

(deftest reverse-vector.4
  (reverse #(a b c d e))
  #(e d c b a))

(deftest reverse-vector.5
  (let ((a #(a b c d e)))
    (reverse a)
    (equalrt a #(a b c d e)))
  t)

(deftest reverse-string.1
  (reverse "")
  "")

(deftest reverse-string.2
  (reverse "a")
  "a")

(deftest reverse-string.3
  (reverse "abcd")
  "dcba")

(deftest reverse-string.4
  (reverse "abcde")
  "edcba")

(deftest reverse-string.5
  (let ((a "abcde"))
    (reverse a)
    (equalrt a "abcde"))
  t)

(deftest reverse-bitvector.1
  (reverse #*)
  #*)

(deftest reverse-bitvector.2
  (reverse #*1)
  #*1)

(deftest reverse-bitvector.3
  (reverse #*1110)
  #*0111)

(deftest reverse-bitvector.4
  (reverse #*11001)
  #*10011)

(deftest reverse-bitvector.5
  (let ((a #*11001))
    (reverse a)
    (equalrt a #*11001))
  t)

(deftest reverse-array.1
  (reverse #1a())
  #())

(deftest reverse-array.2
  (reverse #1a(a))
  #(a))

(deftest reverse-array.3
  (reverse #1a(a b c d))
  #1a(d c b a))

(deftest reverse-array.4
  (reverse #1a(a b c d e))
  #1a(e d c b a))

(deftest reverse-array.5
  (let ((a #1a(a b c d e)))
    (reverse a)
    (equalrt a #1a(a b c d e)))
  t)

(deftest reverse.1
  (let ((x '(1 2 3)))
    (eq (reverse x) x))
  nil)

(deftest reverse.2
  (let ((x #(1 2 3)))
    (eq (reverse x) x))
  nil)

(deftest reverse.3
  (let ((x #(1 2 3)))
    (eq (reverse x) x))
  nil)

(deftest reverse.4
  (let ((x "Hello"))
    (eq (reverse x) x))
  nil)

(deftest reverse.5
  (let ((x #*1000111))
    (eq (reverse x) x))
  nil)

(deftest reverse.6
  (let ((x #1a(a b c)))
    (eq (reverse x) x))
  nil)

(deftest-error reverse-error.1
  (reverse '(a b c . d)))

(deftest-error reverse-error.2
  (eval '(reverse 10))
  type-error)

(deftest-error! reverse-error.3
  (eval '(reverse)))

(deftest-error! reverse-error.4
  (eval '(reverse nil nil)))


;;
;;  Function NREVERSE
;;
(deftest nreverse-list.1
  (nreverse nil)
  nil)

(deftest nreverse-list.2
  (nreverse '(10 20 30 40))
  (40 30 20 10))

(deftest nreverse-list.3
  (let ((a '(10 20 30 40)))
    (nreverse a)
    (equal a '(10 20 30 40)))
  nil)

(deftest nreverse-vector.1
  (nreverse #())
  #())

(deftest nreverse-vector.2
  (nreverse #(a))
  #(a))

(deftest nreverse-vector.3
  (nreverse #(a b c d))
  #(d c b a))

(deftest nreverse-vector.4
  (nreverse #(a b c d e))
  #(e d c b a))

(deftest nreverse-vector.5
  (let ((a #(a b c d e)))
    (nreverse a)
    (equalrt a #(a b c d e)))
  nil)

(deftest nreverse-string.1
  (nreverse "")
  "")

(deftest nreverse-string.2
  (nreverse "a")
  "a")

(deftest nreverse-string.3
  (nreverse "abcd")
  "dcba")

(deftest nreverse-string.4
  (nreverse "abcde")
  "edcba")

(deftest nreverse-string.5
  (let ((a "abcde"))
    (nreverse a)
    (equalrt a "abcde"))
  nil)

(deftest nreverse-bitvector.1
  (nreverse #*)
  #*)

(deftest nreverse-bitvector.2
  (nreverse #*1)
  #*1)

(deftest nreverse-bitvector.3
  (nreverse #*1110)
  #*0111)

(deftest nreverse-bitvector.4
  (nreverse #*11001)
  #*10011)

(deftest nreverse-bitvector.5
  (let ((a #*11001))
    (nreverse a)
    (equalrt a #*11001))
  nil)

(deftest nreverse-array.1
  (nreverse #1a())
  #1a())

(deftest nreverse-array.2
  (nreverse #1a(a))
  #1a(a))

(deftest nreverse-array.3
  (nreverse #1a(a b c d))
  #1a(d c b a))

(deftest nreverse-array.4
  (nreverse #1a(a b c d e))
  #1a(e d c b a))

(deftest nreverse-array.5
  (let ((a #1a(a b c d e)))
    (nreverse a)
    (equalrt a #1a(a b c d e)))
  nil)

(deftest nreverse.1
  (let* ((x '(1 2 3))
         (y (nreverse x)))
    (values
      (eq x y)
      x
      y))
  nil (1) (3 2 1))

(deftest nreverse.2
  (let ((x #(1 2 3)))
    (eq (nreverse x) x))
  t)

(deftest nreverse.3
  (let ((x #(1 2 3)))
    (eq (nreverse x) x))
  t)

(deftest nreverse.4
  (let ((x "Hello"))
    (eq (nreverse x) x))
  t)

(deftest nreverse.5
  (let ((x #*1000111))
    (eq (nreverse x) x))
  t)

(deftest nreverse.6
  (let ((x #1a(a b c)))
    (eq (nreverse x) x))
  t)

(deftest-error nreverse-error.1
  (nreverse '(a b c . d)))

(deftest-error nreverse-error.2
  (eval '(nreverse 10))
  type-error)

(deftest-error! nreverse-error.3
  (eval '(nreverse)))

(deftest-error! nreverse-error.4
  (eval '(nreverse nil nil)))

;; ANSI Common Lisp
(defvar *reverse-object*)

(deftest reverse-test.1
  (progn
    (setq *reverse-object* "abc")
    (reverse *reverse-object*))
  "cba")

(deftest reverse-test.2
  *reverse-object*
  "abc")

(deftest reverse-test.3
  (progn
    (setq *reverse-object* (copy-seq *reverse-object*))
    (nreverse *reverse-object*))
  "cba")

(deftest reverse-test.4
  *reverse-object*
  "cba")

(deftest reverse-test.5
  (progn
    (setq *reverse-object* (list 1 2 3))
    (nreverse *reverse-object*))
  (3 2 1))

(deftest reverse-test.6
  *reverse-object*
  (1))

