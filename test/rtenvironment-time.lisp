;;
;;  ANSI COMMON LISP: 25. Environment
;;

;;
;;  universal-time
;;
(deftest universal-time.1
  (encode-universal-time 0 0 0 1 1 1900 0)
  0)

(deftest universal-time.2
  (decode-universal-time 0 0)
  0 0 0 1 1 1900 0 nil 0)

(deftest universal-time.3
  (encode-universal-time 0 0 0 1 1 1970 0)
  2208988800)

(deftest universal-time.4
  (decode-universal-time 2208988800 0)
  0 0 0 1 1 1970 3 nil 0)

(deftest universal-time.5
  (encode-universal-time 0 0 0 1 1 1970 4)
  2209003200)

(deftest universal-time.6
  (decode-universal-time 2209003200 4)
  0 0 0 1 1 1970 3 nil 4)

(deftest universal-time.7
  (decode-universal-time 2209003200 0)
  0 0 4 1 1 1970 3 nil 0)

(deftest universal-time.8
  (encode-universal-time 0 0 0 1 1 1970 -9)
  2208956400)

(deftest universal-time.9
  (decode-universal-time 2208956400 -9)
  0 0 0 1 1 1970 3 nil -9)

(deftest universal-time.10
  (decode-universal-time 2208956400 0)
  0 0 15 31 12 1969 2 nil 0)

(deftest universal-time.11
  (encode-universal-time 1 2 3 4 5 2020 0)
  3797550121)

(deftest universal-time.12
  (decode-universal-time 3797550121 0)
  1 2 3 4 5 2020 0 nil 0)

(deftest universal-time.13
  (integerp
    (encode-universal-time
      1 2 3 4 5 2020))
  t)

(deftest universal-time.14
  (values-list
    (subseq
      (multiple-value-list
        (decode-universal-time
          (encode-universal-time
            1 2 3 4 5 2020)))
      0 6))
  1 2 3 4 5 2020)

(deftest universal-time.15
  (multiple-value-bind (s mi h d m y)
    (decode-universal-time
      (encode-universal-time 1 2 3 4 5 30))
    (values s mi h d m (mod y 100) (<= 1900 y)))
  1 2 3 4 5 30 t)

(deftest universal-time.16
  (multiple-value-bind (s mi h d m y)
    (decode-universal-time
      (encode-universal-time 1 2 3 4 5 30 0)
      0)
    (values s mi h d m (mod y 100) (<= 1900 y)))
  1 2 3 4 5 30 t)

(deftest universal-time.17
  (multiple-value-bind (s mi h d m y)
    (decode-universal-time
      (encode-universal-time 1 2 3 4 5 70))
    (values s mi h d m (mod y 100) (<= 1900 y)))
  1 2 3 4 5 70 t)

(deftest universal-time.18
  (multiple-value-bind (s mi h d m y)
    (decode-universal-time
      (encode-universal-time 1 2 3 4 5 70 0)
      0)
    (values s mi h d m (mod y 100) (<= 1900 y)))
  1 2 3 4 5 70 t)


;;
;;  Function DECODE-UNIVERSAL-TIME
;;
(deftest-error! decode-universal-time-error.1
  (eval '(decode-universal-time)))

(deftest-error! decode-universal-time-error.2
  (eval '(decode-universal-time #\a))
  type-error)

(deftest-error! decode-universal-time-error.3
  (eval '(decode-universal-time 10 20 30)))

(deftest decode-universal-time-test.1
  (decode-universal-time 0 0)
  0 0 0 1 1 1900 0 nil 0)

(deftest decode-universal-time-test.2
  (decode-universal-time 2414296800 5)
  0 0 1 4 7 1976 6 nil 5)


;;
;;  Function ENCODE-UNIVERSAL-TIME
;;
(deftest encode-universal-time.1
  (integerp
    (encode-universal-time 1 2 3 4 5 6))
  t)

(deftest encode-universal-time.2
  (integerp
    (encode-universal-time 1 2 3 4 5 6 7))
  t)

(deftest-error! encode-universal-time-error.1
  (eval '(encode-universal-time 1 2 3 4 5)))

(deftest-error! encode-universal-time-error.2
  (eval '(encode-universal-time 1 2 3 400 5 6))
  type-error)

(deftest-error! encode-universal-time-error.3
  (eval '(encode-universal-time 1 2 3 4 5 6 0 nil)))

(deftest encode-universal-time-test.1
  (encode-universal-time 0 0 0 1 1 1900 0)
  0)

(deftest encode-universal-time-test.2
  (encode-universal-time 0 0 1 4 7 1976 5)
  2414296800)


;;
;;  Function GET-UNIVERSAL-TIME
;;
(deftest get-universal-time.1
  (integerp
    (get-universal-time))
  t)

(deftest get-universal-time.2
  (< (encode-universal-time 0 0 0 1 1 2020 0)
     (get-universal-time))
  t)

(deftest-error! get-universal-time-error.1
  (eval '(get-universal-time nil)))


;;
;;  Function GET-DECODED-TIME
;;
(deftest get-decoded-time.1
  (length
    (multiple-value-list
      (get-decoded-time)))
  9)

(deftest get-decoded-time.2
  (multiple-value-bind (s mi h d m y d2 dp z) (get-decoded-time)
    (declare (ignorable s mi h d m y d2 dp z))
    (< 2020 y))
  t)

(deftest-error! get-decoded-time-error.1
  (eval '(get-decoded-time nil)))


;;
;;  Function SLEEP
;;
(deftest sleep.1
  (sleep 0)
  nil)

(deftest sleep.2
  (sleep 3/667)
  nil)

(deftest sleep.3
  (sleep 1.0e-2)
  nil)

(deftest-error! sleep-error.1
  (eval '(sleep)))

(deftest-error! sleep-error.2
  (eval '(sleep -1))
  type-error)

(deftest-error! sleep-error.3
  (eval '(sleep 10 20)))


;;
;;  Constant Variable INTERNAL-TIME-UNITS-PER-SECOND
;;
(deftest internal-time-units-per-second.1
  (integerp internal-time-units-per-second)
  t)

(deftest internal-time-units-per-second.2
  (< 0 internal-time-units-per-second)
  t)


;;
;;  Function GET-INTERNAL-REAL-TIME
;;
(deftest get-internal-real-time.1
  (integerp
    (get-internal-real-time))
  t)

(deftest get-internal-real-time.2
  (< 0 (get-internal-real-time))
  t)

(deftest-error! get-internal-real-time.3
  (eval '(get-internal-real-time nil)))


;;
;;  Function GET-INTERNAL-RUN-TIME
;;
(deftest get-internal-run-time.1
  (integerp
    (get-internal-run-time))
  t)

(deftest get-internal-run-time.2
  (< 0 (get-internal-run-time))
  t)

(deftest-error! get-internal-run-time.3
  (eval '(get-internal-run-time nil)))


;;
;;  Macro TIME
;;
(deftest time.1
  (< 0 (length
         (with-output-to-string (*trace-output*)
           (time (values 10 20 30)))))
  t)

(deftest time.2
  (with-open-stream (*trace-output* (make-broadcast-stream))
    (time (values 10 20 30)))
  10 20 30)

(deftest-error! time-error.1
  (eval '(time)))

(deftest-error! time-error.2
  (eval '(time 10 20)))

