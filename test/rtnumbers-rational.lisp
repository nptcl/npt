;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function GCD
;;
(deftest gcd.1
  (gcd)
  0)

(deftest gcd.2
  (gcd 10)
  10)

(deftest gcd.3
  (gcd -20)
  20)

(deftest gcd.4
  (gcd 4444444444444444444444444444444444444444444444444444)
  4444444444444444444444444444444444444444444444444444)

(deftest gcd.5
  (gcd -4444444444444444444444444444444444444444444444444444)
  4444444444444444444444444444444444444444444444444444)

(deftest gcd.6
  (gcd 6 4)
  2)

(deftest gcd.7
  (gcd 1200 4500 125)
  25)

(deftest gcd.8
  (gcd 4 5 6 7 8 9 10 11 12 87 86 85 84 83 82 81)
  1)

(deftest gcd-test.1
  (gcd)
  0)

(deftest gcd-test.2
  (gcd 60 42)
  6)

(deftest gcd-test.3
  (gcd 3333 -33 101)
  1)

(deftest gcd-test.4
  (gcd 3333 -33 1002001)
  11)

(deftest gcd-test.5
  (gcd 91 -49)
  7)

(deftest gcd-test.6
  (gcd 63 -42 35)
  7)

(deftest gcd-test.7
  (gcd 5)
  5)

(deftest gcd-test.8
  (gcd -4)
  4)

(deftest-error! gcd-error.1
  (eval '(gcd 4/5))
  type-error)

(deftest-error! gcd-error.2
  (eval '(gcd 10 20 4/5))
  type-error)


;;
;;  Function LCM
;;
(deftest lcm.1
  (lcm)
  1)

(deftest lcm.2
  (lcm 10)
  10)

(deftest lcm.3
  (lcm -20)
  20)

(deftest lcm.4
  (lcm 4444444444444444444444444444444444444444444444444444)
  4444444444444444444444444444444444444444444444444444)

(deftest lcm.5
  (lcm -4444444444444444444444444444444444444444444444444444)
  4444444444444444444444444444444444444444444444444444)

(deftest lcm.6
  (lcm 10 0)
  0)

(deftest lcm.7
  (lcm 0 10)
  0)

(deftest lcm-test.1
  (lcm 10)
  10)

(deftest lcm-test.2
  (lcm 25 30)
  150)

(deftest lcm-test.3
  (lcm -24 18 10)
  360)

(deftest lcm-test.4
  (lcm 14 35)
  70)

(deftest lcm-test.5
  (lcm 0 5)
  0)

(deftest lcm-test.6
  (lcm 1 2 3 4 5 6)
  60)

(deftest-error! lcm-error.1
  (eval '(lcm 4/5))
  type-error)

(deftest-error! lcm-error.2
  (eval '(lcm 10 20 4/5))
  type-error)



;;
;;
;;
(deftest numerator.1
  (numerator 0)
  0)

(deftest numerator.2
  (numerator 10)
  10)

(deftest numerator.3
  (numerator -20)
  -20)

(deftest numerator.4
  (numerator 4/6)
  2)

(deftest numerator.5
  (numerator 123/541)
  123)

(deftest denominator.1
  (denominator 0)
  1)

(deftest denominator.2
  (denominator 10)
  1)

(deftest denominator.3
  (denominator -20)
  1)

(deftest denominator.4
  (denominator 4/6)
  3)

(deftest denominator.5
  (denominator 123/541)
  541)

(deftest integer-length.1
  (values
    (integer-length 0)
    (integer-length 1)
    (integer-length 2)
    (integer-length 3)
    (integer-length 4)
    (integer-length 7)
    (integer-length 8)
    (integer-length 15)
    (integer-length 16))
  0 1 2 2 3 3 4 4 5)

(deftest integer-length.2
  (values
    (integer-length -1)
    (integer-length -2)
    (integer-length -3)
    (integer-length -4)
    (integer-length -5)
    (integer-length -8)
    (integer-length -9)
    (integer-length -16)
    (integer-length -17))
  0 1 2 2 3 3 4 4 5)

(deftest integer-length.3
  (values
    (integer-length #xFFFFFFFFFFFFFFFF)
    (integer-length #x-FFFFFFFFFFFFFFFF))
  64 64)

(deftest integer-length.4
  (values
    (integer-length #x10000000000000000)
    (integer-length #x-10000000000000000))
  65 64)

(deftest integer-length.5
  (values
    (integer-length #x10000000000000001)
    (integer-length #x-10000000000000001))
  65 65)

(deftest integer-length.6
  (values
    (integer-length #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
    (integer-length #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))
  128 128)

(deftest integer-length.7
  (values
    (integer-length #x100000000000000000000000000000000)
    (integer-length #x-100000000000000000000000000000000))
  129 128)

(deftest integer-length.8
  (values
    (integer-length #x100000000000000000000000000000001)
    (integer-length #x-100000000000000000000000000000001))
  129 129)

(deftest parse-integer.1
  (parse-integer "10")
  10 2)

(deftest parse-integer.2
  (parse-integer "   10")
  10 5)

(deftest parse-integer.3
  (parse-integer "   10   ")
  10 8)

(deftest parse-integer.4
  (parse-integer "   10   " :junk-allowed t)
  10 5)

(deftest parse-integer.5
  (parse-integer "123456789" :start 3 :end 5)
  45 5)

(deftest-error parse-integer.6
  (parse-integer "ABCD"))

(deftest parse-integer.7
  (parse-integer "ABCD" :radix 16)
  #xABCD 4)

(deftest parse-integer.8
  (parse-integer "efghi" :radix 16 :junk-allowed :hello)
  #xEF 2)

(deftest parse-integer.9
  (parse-integer " +10 ")
  10 5)

(deftest parse-integer.10
  (parse-integer " -10 " :junk-allowed t)
  -10 4)

(deftest-error parse-integer.11
  (parse-integer "  HHHH" :junk-allowed nil))

(deftest parse-integer.12
  (parse-integer "  HHHH" :junk-allowed t)
  nil 2)

