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
;;  Function NUMERATOR
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

(deftest numerator.6
  (numerator -123/541)
  -123)

(deftest-error! numerator-error.1
  (eval '(numerator)))

(deftest-error! numerator-error.2
  (eval '(numerator #\A))
  type-error)

(deftest-error! numerator-error.3
  (eval '(numerator 10 20)))


;;
;;  Function DENOMINATOR
;;
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

(deftest denominator.6
  (denominator -123/541)
  541)

(deftest-error! denominator-error.1
  (eval '(denominator)))

(deftest-error! denominator-error.2
  (eval '(denominator #\A))
  type-error)

(deftest-error! denominator-error.3
  (eval '(denominator 10 20)))

;;  ANSI Common Lisp
(deftest numerator-test.1
  (numerator 1/2)
  1)

(deftest numerator-test.2
  (denominator 12/36)
  3)

(deftest numerator-test.3
  (numerator -1)
  -1)

(deftest numerator-test.4
  (denominator (/ -33))
  33)

(deftest numerator-test.5
  (numerator (/ 8 -6))
  -4)

(deftest numerator-test.6
  (denominator (/ 8 -6))
  3)

(deftest numerator-test.7
  (values
    (gcd (numerator 6/7) (denominator 6/7))
    (gcd (numerator -123/456) (denominator -123/456))
    (gcd (numerator 7/11) (denominator 7/11))
    (gcd (numerator 3) (denominator 3)))
  1 1 1 1)


;;
;;  Function RATIONAL
;;
(deftest rational.1
  (rational 0)
  0)

(deftest rational.2
  (rational -12)
  -12)

(deftest rational.3
  (rational 3/4)
  3/4)

(deftest rational-single.1
  (rational 1.0f0)
  1)

(deftest rational-single.2
  (rational 0.5f0)
  1/2)

(deftest rational-single.3
  (rational -0.125f0)
  -1/8)

(deftest rational-single.4
  (rational 0.1f0)
  13421773/134217728)

(deftest rational-single.5
  (rational -1.23f5)
  -123000)

(deftest rational-single.6
  (rational 4.56f-2)
  12240657/268435456)

(deftest rational-single.7
  (rational +0.0f0)
  0)

(deftest rational-single.8
  (rational -0.0f0)
  0)

(deftest rational-single.9
  (eql (float (rational 4.56f-2)) 4.56f-2)
  t)

(deftest rational-double.1
  (rational 1.0d0)
  1)

(deftest rational-double.2
  (rational 0.5d0)
  1/2)

(deftest rational-double.3
  (rational -0.125d0)
  -1/8)

(deftest rational-double.4
  (rational 0.1d0)
  3602879701896397/36028797018963968)

(deftest rational-double.5
  (rational -1.23d5)
  -123000)

(deftest rational-double.6
  (rational 4.57d-2)
  6586064095066613/144115188075855872)

(deftest rational-double.7
  (rational +0.0d0)
  0)

(deftest rational-double.8
  (rational -0.0d0)
  0)

(deftest rational-long.1
  (rational 1.0d0)
  1)

(deftest rational-long.2
  (rational 0.5L0)
  1/2)

(deftest rational-long.3
  (rational -0.125L0)
  -1/8)

#+long-float-64
(deftest rational-long.4
  (rational 0.1L0)
  3602879701896397/36028797018963968)

#+long-float-80
(deftest rational-long.4
  (rational 0.1L0)
  14757395258967641293/147573952589676412928)

(deftest rational-long.5
  (rational -1.23L5)
  -123000)

#+long-float-64
(deftest rational-long.6
  (rational 4.56L-2)
  1642913144064757/36028797018963968)

#+long-float-80
(deftest rational-long.6
  (rational 4.56L-2)
  13458744476178488859/295147905179352825856)

(deftest rational-long.7
  (rational 0.0L0)
  0)

(deftest rational-long.8
  (rational -0.0L0)
  0)

(deftest-error! rational-error.1
  (eval '(rational)))

(deftest-error! rational-error.2
  (eval '(rational "Hello"))
  type-error)

(deftest-error! rational-error.3
  (eval '(rational 10 20)))


;;
;;  Function RATIONALIZE
;;
(deftest rationalize.1
  (rationalize 0)
  0)

(deftest rationalize.2
  (rationalize -12)
  -12)

(deftest rationalize.3
  (rationalize 3/4)
  3/4)

(deftest rationalize-single.1
  (rationalize 1.0f0)
  1)

(deftest rationalize-single.2
  (rationalize 0.5f0)
  1/2)

(deftest rationalize-single.3
  (rationalize -0.125f0)
  -1/8)

(deftest rationalize-single.4
  (rationalize 0.1f0)
  1/10)

(deftest rationalize-single.5
  (rationalize -1.23f5)
  -123000)

(deftest rationalize-single.6
  (rationalize 4.56f-2)
  57/1250)

(deftest rationalize-single.7
  (rationalize +0.0f0)
  0)

(deftest rationalize-single.8
  (rationalize -0.0f0)
  0)

(deftest rationalize-single.9
  (eql (float (rationalize 4.56f-2)) 4.56f-2)
  t)

(deftest rationalize-double.1
  (rationalize 1.0d0)
  1)

(deftest rationalize-double.2
  (rationalize 0.5d0)
  1/2)

(deftest rationalize-double.3
  (rationalize -0.125d0)
  -1/8)

(deftest rationalize-double.4
  (rationalize 0.1d0)
  1/10)

(deftest rationalize-double.5
  (rationalize -1.23d5)
  -123000)

(deftest rationalize-double.6
  (rationalize 4.57d-2)
  457/10000)

(deftest rationalize-double.7
  (rationalize +0.0d0)
  0)

(deftest rationalize-double.8
  (rationalize -0.0d0)
  0)

(deftest rationalize-long.1
  (rationalize 1.0d0)
  1)

(deftest rationalize-long.2
  (rationalize 0.5L0)
  1/2)

(deftest rationalize-long.3
  (rationalize -0.125L0)
  -1/8)

(deftest rationalize-long.4
  (rationalize 0.1L0)
  1/10)

(deftest rationalize-long.5
  (rationalize -1.23L5)
  -123000)

(deftest rationalize-long.6
  (rationalize 4.56L-2)
  57/1250)

(deftest rationalize-long.7
  (rationalize +0.0L0)
  0)

(deftest rationalize-long.8
  (rationalize -0.0L0)
  0)

(deftest-error! rationalize-error.1
  (eval '(rationalize)))

(deftest-error! rationalize-error.2
  (eval '(rationalize "Hello"))
  type-error)

(deftest-error! rationalize-error.3
  (eval '(rationalize 10 20)))

;;  ANSI Common Lisp
(deftest rational-test.1
  (rational 0)
  0)

(deftest rational-test.2
  (rationalize -11/100)
  -11/100)

(deftest rational-test.3
  (rational .1)
  13421773/134217728)

(deftest rational-test.4
  (rationalize .1)
  1/10)



;;
;;
;;
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

