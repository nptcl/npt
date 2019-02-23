;;
;;  ANSI COMMON LISP: 17. Sequences
;;
(deftest remove-duplicates.1
  (remove-duplicates nil)
  nil)

(deftest remove-duplicates.2
  (remove-duplicates '(a a))
  (a))

(deftest remove-duplicates.3
  (remove-duplicates '(a a b a))
  (a b))

(deftest remove-duplicates.4
  (remove-duplicates '(a a b a c b d d))
  (a b c d))

(deftest remove-duplicates.5
  (remove-duplicates '(a a b a c b d d) :from-end t)
  (a c b d))

(deftest remove-duplicates.6
  (remove-duplicates '(a b c d e f))
  (a b c d e f))

(deftest remove-duplicates.7
  (remove-duplicates '(a b c d e f) :from-end t)
  (a b c d e f))

(deftest remove-duplicates.8
  (remove-duplicates #())
  #())

(deftest remove-duplicates.9
  (remove-duplicates #(a a))
  #(a))

(deftest remove-duplicates.10
  (remove-duplicates #(a a b a))
  #(a b))

(deftest remove-duplicates.11
  (remove-duplicates #(a a b a c b d d))
  #(a b c d))

(deftest remove-duplicates.12
  (remove-duplicates #(a a b a c b d d) :from-end t)
  #(a c b d))

(deftest remove-duplicates.13
  (remove-duplicates #(a b c d e f))
  #(a b c d e f))

(deftest remove-duplicates.14
  (remove-duplicates #(a b c d e f) :from-end t)
  #(a b c d e f))

(deftest delete-duplicates.1
  (delete-duplicates nil)
  nil)

(deftest delete-duplicates.2
  (delete-duplicates '(a a))
  (a))

(deftest delete-duplicates.3
  (delete-duplicates '(a a b a))
  (a b))

(deftest delete-duplicates.4
  (delete-duplicates '(a a b a c b d d))
  (a b c d))

(deftest delete-duplicates.5
  (delete-duplicates '(a a b a c b d d) :from-end t)
  (a c b d))

(deftest delete-duplicates.6
  (delete-duplicates '(a b c d e f))
  (a b c d e f))

(deftest delete-duplicates.7
  (delete-duplicates '(a b c d e f) :from-end t)
  (a b c d e f))

(deftest delete-duplicates.8
  (delete-duplicates #())
  #())

(deftest delete-duplicates.9
  (delete-duplicates #(a a))
  #(a))

(deftest delete-duplicates.10
  (delete-duplicates #(a a b a))
  #(a b))

(deftest delete-duplicates.11
  (delete-duplicates #(a a b a c b d d))
  #(a b c d))

(deftest delete-duplicates.12
  (delete-duplicates #(a a b a c b d d) :from-end t)
  #(a c b d))

(deftest delete-duplicates.13
  (delete-duplicates #(a b c d e f))
  #(a b c d e f))

(deftest delete-duplicates.14
  (delete-duplicates #(a b c d e f) :from-end t)
  #(a b c d e f))

