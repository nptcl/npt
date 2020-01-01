;;
;;  ANSI COMMON LISP: 25. Environment
;;
(deftest encode-universal-time.1
  (encode-universal-time 0 0 0 1 1 1900 0)
  0)


(deftest decode-universal-time.1
  (decode-universal-time 0 0)
  0 0 0 1 1 1900 0 nil 0)

(deftest encode-universal-time.2
  (encode-universal-time 0 0 0 1 1 1970 0)
  2208988800)

(deftest decode-universal-time.2
  (decode-universal-time 2208988800 0)
  0 0 0 1 1 1970 3 nil 0)

(deftest encode-universal-time.3
  (encode-universal-time 0 0 0 1 1 1970 4)
  2209003200)

(deftest decode-universal-time.3a
  (decode-universal-time 2209003200 4)
  0 0 0 1 1 1970 3 nil 4)

(deftest decode-universal-time.3b
  (decode-universal-time 2209003200 0)
  0 0 4 1 1 1970 3 nil 0)

(deftest encode-universal-time.4
  (encode-universal-time 0 0 0 1 1 1970 -9)
  2208956400)

(deftest encode-universal-time.4a
  (decode-universal-time 2208956400 -9)
  0 0 0 1 1 1970 3 nil -9)

(deftest encode-universal-time.4b
  (decode-universal-time 2208956400 0)
  0 0 15 31 12 1969 2 nil 0)

(deftest encode-universal-time.5
  (encode-universal-time 1 2 3 4 5 2020 0)
  3797550121)

(deftest decode-universal-time.5
  (decode-universal-time 3797550121 0)
  1 2 3 4 5 2020 0 nil 0)

(deftest encode-universal-time.6
  (integerp
    (encode-universal-time
      1 2 3 4 5 2020))
  t)

(deftest decode-universal-time.6
  (values-list
    (subseq
      (multiple-value-list
        (decode-universal-time
          (encode-universal-time
            1 2 3 4 5 2020)))
      0 6))
  1 2 3 4 5 2020)

(deftest get-universal-time.1
  (integerp
    (get-universal-time))
  t)

(deftest get-universal-time.2
  (< (encode-universal-time 0 0 0 1 1 2020 0)
     (get-universal-time))
  t)

(deftest get-decoded-time.1
  (length
    (multiple-value-list
      (get-decoded-time)))
  9)


;;
;;  do-tests
;;
(do-tests :test t)

