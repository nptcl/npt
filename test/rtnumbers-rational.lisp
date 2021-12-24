;;
;;  ANSI COMMON LISP: 12. Numbers
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

(deftest gcd.1
  (values
    (gcd)
    (gcd 60 42)
    (gcd 3333 -33 101)
    (gcd 3333 -33 1002001)
    (gcd 91 -49)
    (gcd 63 -42 35)
    (gcd 5)
    (gcd -4))
  0 6 1 11 7 7 5 4)

(deftest lcm.1
  (values
    (lcm 10)
    (lcm 25 30)
    (lcm -24 18 10)
    (lcm 14 35)
    (lcm 0 5)
    (lcm 1 2 3 4 5 6))
  10 150 360 70 0 60)

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

