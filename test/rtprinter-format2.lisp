;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  Output
;;
(deftest format-output.1
  (format nil "")
  "")

(deftest format-output.2
  (format nil "Hello")
  "Hello")

(deftest format-output.3
  (format nil "  1234   ")
  "  1234   ")

;;
;;  Aethetic
;;
(deftest format-aethetic.1
  (format nil "~A" 10)
  "10")

(deftest format-aethetic.2
  (format nil "~a" #\A)
  "A")

(deftest format-aethetic.3
  (format nil "Hello~AXYZ" 10)
  "Hello10XYZ")

(deftest format-aethetic.4
  (format nil "~A~A~A" #\A #\b 200)
  "Ab200")

(deftest format-aethetic.5
  (format nil "~A" nil)
  "NIL")

(deftest format-aethetic.6
  (format nil "~:A" nil)
  "()")

(deftest format-aethetic.7
  (format nil "~:@A" 22)
  "22")

(deftest-error format-aethetic.8
  (format nil "~-1A" 22))

(deftest format-aethetic.9
  (format nil "~0A" 33)
  "33")

(deftest format-aethetic.10
  (format nil "[~10A]" 33)
  "[33        ]")

(deftest format-aethetic.11
  (format nil "~+5A" 33)
  "33   ")

(deftest format-aethetic.12
  (format nil "~10,4A" 12345)
  "12345        ")

(deftest format-aethetic.13
  (format nil "~10,4A" 12345)
  "12345        ")

(deftest-error format-aethetic.14
  (format nil "~10,0A" 12345))

(deftest format-aethetic.15
  (format nil "~,,5A" 12345)
  "12345     ")

(deftest format-aethetic.16
  (format nil "~11,,,'*@A" "Hello")
  "******Hello")

(deftest format-aethetic.17
  (format nil "~10,4,6,'*@:A" "Hello")
  "******Hello")

(deftest-error format-aethetic.18
  (format nil "~,,-1:A" "Hello"))

(deftest-error format-aethetic.19
  (format nil "~,,,50:A" "Hello"))

(deftest-error format-aethetic.20
  (format nil "~,,,,10:A" "Hello"))

(deftest format-aethetic.21
  (format nil "~A" 12.3)
  "12.3")

(deftest format-aethetic.22
  (format nil "~A" -12.3)
  "-12.3")

(deftest format-aethetic.23
  (let ((x (copy-readtable))
        (y (copy-readtable)))
    (equal (format nil "~A" x)
           (format nil "~A" y)))
  nil)

(deftest format-aethetic.24
  (format nil "~A" 0.0003)
  "3.0E-4")

(deftest format-aethetic.25
  (format nil "~A" -0.0003)
  "-3.0E-4")

(deftest format-aethetic.26
  (format nil "~:@A" 22)
  "22")


;;
;;  Standard
;;
(deftest format-standard.1
  (format nil "~S" 10)
  "10")

(deftest format-standard.2
  (format nil "~s" #\A)
  "#\\A")

(deftest format-standard.3
  (format nil "Hello~SXYZ" 10)
  "Hello10XYZ")

(deftest format-standard.4
  (format nil "~S~S~S" #\A #\b 200)
  "#\\A#\\b200")

(deftest format-standard.5
  (format nil "~S" nil)
  "NIL")

(deftest format-standard.6
  (format nil "~:S" nil)
  "()")

(deftest format-standard.7
  (format nil "~:@S" 22)
  "22")

(deftest-error format-standard.8
  (format nil "~-1S" 22))

(deftest format-standard.9
  (format nil "~0S" 33)
  "33")

(deftest format-standard.10
  (format nil "[~10S]" 33)
  "[33        ]")

(deftest format-standard.11
  (format nil "~+5S" 33)
  "33   ")

(deftest format-standard.12
  (format nil "~10,4S" 12345)
  "12345        ")

(deftest format-standard.13
  (format nil "~10,4S" 12345)
  "12345        ")

(deftest-error format-standard.14
  (format nil "~10,0S" 12345))

(deftest format-standard.15
  (format nil "~,,5S" 12345)
  "12345     ")

(deftest format-standard.16
  (format nil "~11,,,'*@S" "Hello")
  "****\"Hello\"")

(deftest format-standard.17
  (format nil "~10,4,6,'*@:S" "Hello")
  "******\"Hello\"")

(deftest-error format-standard.18
  (format nil "~,,-1:S" "Hello"))

(deftest-error format-standard.19
  (format nil "~,,,50:S" "Hello"))

(deftest-error format-standard.20
  (format nil "~,,,,10:S" "Hello"))

(deftest format-standard.21
  (format nil "~S" 12.3)
  "12.3")

(deftest format-standard.22
  (format nil "~S" -12.3)
  "-12.3")

(deftest format-standard.23
  (let ((x (copy-readtable))
        (y (copy-readtable)))
    (equal (format nil "~S" x)
           (format nil "~S" y)))
  nil)

(deftest format-standard.24
  (format nil "~S" 0.0003)
  "3.0E-4")

(deftest format-standard.25
  (format nil "~S" -0.0003)
  "-3.0E-4")

(deftest format-standard.26
  (format nil "~:@S" 22)
  "22")


;;
;;  Binary
;;
(deftest format-binary.1
  (format nil "~B" #b10111)
  "10111")

(deftest format-binary.2
  (format nil "~b" #b-10111)
  "-10111")

(deftest format-binary.3
  (format nil "~B" 3/4)
  "11/100")

(deftest format-binary.4
  (format nil "~B" 19.0f0)
  "19.0")

(deftest format-binary.5
  (format nil "~@B" #b10111)
  "+10111")

(deftest format-binary.6
  (format nil "~@B" #b-10111)
  "-10111")

(deftest format-binary.7
  (format nil "~:B" #b10111)
  "10,111")

(deftest format-binary.8
  (format nil "~12B" #b10111)
  "       10111")

(deftest format-binary.9
  (format nil "~12,'*B" #b10111)
  "*******10111")

(deftest format-binary.10
  (format nil "~,,'-:B" #b10111001)
  "10-111-001")

(deftest format-binary.11
  (format nil "~,,'-,2:B" #b10111001)
  "10-11-10-01")

(deftest-error format-binary.12
  (format nil "~-1B" #b10111001))

(deftest-error format-binary.13
  (format nil "~,10B" #b10111001))

(deftest-error format-binary.14
  (format nil "~,,20B" #b10111001))

(deftest-error format-binary.15
  (format nil "~,,,0B" #b10111001))

(deftest-error format-binary.16
  (format nil "~,,,,10B" #b10111001))

(deftest format-binary.17
  (format nil "~B" :test)
  "TEST")


;;
;;  Octal
;;
(deftest format-octal.1
  (format nil "~O" #o13476)
  "13476")

(deftest format-octal.2
  (format nil "~o" #o-13476)
  "-13476")

(deftest format-octal.3
  (format nil "~O" 123/5678)
  "173/13056")

(deftest format-octal.4
  (format nil "~O" 19.0f0)
  "19.0")

(deftest format-octal.5
  (format nil "~@O" #o13476)
  "+13476")

(deftest format-octal.6
  (format nil "~@O" #o-13476)
  "-13476")

(deftest format-octal.7
  (format nil "~:O" #o13476)
  "13,476")

(deftest format-octal.8
  (format nil "~12O" #o13476)
  "       13476")

(deftest format-octal.9
  (format nil "~12,'*O" #o13476)
  "*******13476")

(deftest format-octal.10
  (format nil "~,,'-:O" #o12345670)
  "12-345-670")

(deftest format-octal.11
  (format nil "~,,'-,2:O" #o12345670)
  "12-34-56-70")

(deftest-error format-octal.12
  (format nil "~-1O" #o12345670))

(deftest-error format-octal.13
  (format nil "~,10O" #o12345670))

(deftest-error format-octal.14
  (format nil "~,,20O" #o12345670))

(deftest-error format-octal.15
  (format nil "~,,,0O" #o12345670))

(deftest-error format-octal.16
  (format nil "~,,,,10O" #o12345670))

(deftest format-octal.17
  (format nil "~O" :test)
  "TEST")


;;
;;  Decimal
;;
(deftest format-decimal.1
  (format nil "~D" 13476)
  "13476")

(deftest format-decimal.2
  (format nil "~d" -13476)
  "-13476")

(deftest format-decimal.3
  (format nil "~D" 123/5678)
  "123/5678")

(deftest format-decimal.4
  (format nil "~D" 19.0f0)
  "19.0")

(deftest format-decimal.5
  (format nil "~@D" 98765)
  "+98765")

(deftest format-decimal.6
  (format nil "~@D" -98765)
  "-98765")

(deftest format-decimal.7
  (format nil "~:D" 98765)
  "98,765")

(deftest format-decimal.8
  (format nil "~12D" 98765)
  "       98765")

(deftest format-decimal.9
  (format nil "~12,'*D" 98765)
  "*******98765")

(deftest format-decimal.10
  (format nil "~,,'-:D" 987654321)
  "987-654-321")

(deftest format-decimal.11
  (format nil "~,,'-,2:D" 987654321)
  "9-87-65-43-21")

(deftest-error format-decimal.12
  (format nil "~-1D" 12345))

(deftest-error format-decimal.13
  (format nil "~,10D" 12345))

(deftest-error format-decimal.14
  (format nil "~,,20D" 12345))

(deftest-error format-decimal.15
  (format nil "~,,,0D" 12345))

(deftest-error format-decimal.16
  (format nil "~,,,,10D" 12345))

(deftest format-decimal.17
  (format nil "~D" :test)
  "TEST")


;;
;;  Hexadecimal
;;
(deftest format-hexadecimal.1
  (format nil "~X" #x9ABCDEF)
  "9ABCDEF")

(deftest format-hexadecimal.2
  (format nil "~x" #x-9ABCDEF)
  "-9ABCDEF")

(deftest format-hexadecimal.3
  (format nil "~X" 123/5678)
  "7B/162E")

(deftest format-hexadecimal.4
  (format nil "~X" 19.0f0)
  "19.0")

(deftest format-hexadecimal.5
  (format nil "~@X" #x9ABCDEF)
  "+9ABCDEF")

(deftest format-hexadecimal.6
  (format nil "~@X" #x-9ABCDEF)
  "-9ABCDEF")

(deftest format-hexadecimal.7
  (format nil "~:X" #x9ABCDEF)
  "9,ABC,DEF")

(deftest format-hexadecimal.8
  (format nil "~12X" #x9ABCD)
  "       9ABCD")

(deftest format-hexadecimal.9
  (format nil "~12,'*X" #x9ABCD)
  "*******9ABCD")

(deftest format-hexadecimal.10
  (format nil "~,,'-:X" #x789ABCDEF)
  "789-ABC-DEF")

(deftest format-hexadecimal.11
  (format nil "~,,'-,2:X" #x789ABCDEF)
  "7-89-AB-CD-EF")

(deftest-error format-hexadecimal.12
  (format nil "~-1X" #x12345))

(deftest-error format-hexadecimal.13
  (format nil "~,10X" #x12345))

(deftest-error format-hexadecimal.14
  (format nil "~,,20X" #x12345))

(deftest-error format-hexadecimal.15
  (format nil "~,,,0X" #x12345))

(deftest-error format-hexadecimal.16
  (format nil "~,,,,10X" #x12345))

(deftest format-hexadecimal.17
  (format nil "~X" :test)
  "TEST")


;;
;;  Radix
;;
(deftest format-radix.1
  (format nil "~16R" #x9ABCDEF)
  "9ABCDEF")

(deftest format-radix.2
  (format nil "~2r" #b-10110011)
  "-10110011")

(deftest format-radix.3
  (format nil "~10R" 123/5678)
  "123/5678")

(deftest format-radix.4
  (format nil "~13R" 19.0f0)
  "19.0")

(deftest format-radix.5
  (format nil "~15@R" #15r9ABCDE0)
  "+9ABCDE0")

(deftest format-radix.6
  (format nil "~15@R" #15r-9ABCDE0)
  "-9ABCDE0")

(deftest format-radix.7
  (format nil "~36:R" #36rHIJKLMN)
  "H,IJK,LMN")

(deftest format-radix.8
  (format nil "~7,12R" #7r12345)
  "       12345")

(deftest format-radix.9
  (format nil "~7,12,'*R" #7r12345)
  "*******12345")

(deftest format-radix.10
  (format nil "~11,,,'-:R" #11rA98765432)
  "A98-765-432")

(deftest format-radix.11
  (format nil "~11,,,'-,2:R" #11rA98765432)
  "A-98-76-54-32")

(deftest-error format-radix.12
  (format nil "~1R" #x12345))

(deftest-error format-radix.13
  (format nil "~37R" #x12345))

(deftest-error format-radix.14
  (format nil "~10,-1R" #x12345))

(deftest-error format-radix.15
  (format nil "~10,,10R" #x12345))

(deftest-error format-radix.16
  (format nil "~10,,,20R" #x12345))

(deftest-error format-radix.17
  (format nil "~10,,,,0R" #x12345))

(deftest-error format-radix.18
  (format nil "~10,,,,,10R" #x12345))

(deftest format-radix.19
  (format nil "~11R" :test)
  "TEST")


;;
;;  RadixText
;;
(deftest format-radix-english.1
  (format nil "~R" 0)
  "zero")

(deftest format-radix-english.2
  (format nil "~R" 1)
  "one")

(deftest format-radix-english.3
  (format nil "~R" -1)
  "minus one")

(deftest format-radix-english.4
  (format nil "~R" 1000)
  "one thousand")

(deftest format-radix-english.5
  (format nil "~r" 20000)
  "twenty thousand")

(deftest format-radix-english.7
  (format nil "~R" 12345678)
  "twelve million, three hundred and forty-five thousand, six hundred and seventy-eight")

(deftest format-radix-english.8
  (format nil "~R" 10000008)
  "ten million, eight")

(deftest format-radix-english.9
  (format nil "~R" (ash 1 200))
  "one novendecillion, six hundred and six octodecillion, nine hundred and thirty-eight septendecillion, forty-four sedecillion, two hundred and fifty-eight quindecillion, nine hundred and ninety quattuordecillion, two hundred and seventy-five tredecillion, five hundred and forty-one duodecillion, nine hundred and sixty-two undecillion, ninety-two decillion, three hundred and forty-one nonillion, one hundred and sixty-two octillion, six hundred and two septillion, five hundred and twenty-two sextillion, two hundred and two quintillion, nine hundred and ninety-three quadrillion, seven hundred and eighty-two trillion, seven hundred and ninety-two billion, eight hundred and thirty-five million, three hundred and one thousand, three hundred and seventy-six")

(deftest format-radix-english.10
  (format nil "~:R" 0)
  "zeroth")

(deftest format-radix-english.11
  (format nil "~:R" 1)
  "first")

(deftest format-radix-english.12
  (format nil "~:R" -1)
  "minus first")

(deftest format-radix-english.13
  (format nil "~:R" 1000)
  "one thousandth")

(deftest format-radix-english.14
  (format nil "~:R" -12345)
  "minus twelve thousand, three hundred and forty-fifth")

(deftest-error format-radix-english.15
  (format nil "~R" 12/34))

(deftest-error format-radix-english.16
  (format nil "~,10R" 10))

(deftest-error format-radix-english.17
  (format nil "~R" :test))

(deftest format-radix-roma.1
  (format nil "~@R" 1)
  "I")

(deftest format-radix-roma.2
  (format nil "~@R" 4)
  "IV")

(deftest format-radix-roma.3
  (format nil "~:@R" 4)
  "IIII")

(deftest format-radix-roma.4
  (format nil "~@R" 5)
  "V")

(deftest format-radix-roma.5
  (format nil "~:@R" 5)
  "V")

(deftest format-radix-roma.6
  (format nil "~@R" 1234)
  "MCCXXXIV")

(deftest format-radix-roma.7
  (format nil "~:@R" 1234)
  "MCCXXXIIII")

(deftest-error format-radix-roma.8
  (format nil "~@R" 0))

(deftest-error format-radix-roma.9
  (format nil "~@R" 4000))

(deftest-error format-radix-roma.10
  (format nil "~,10@R" 10))

(deftest-error format-radix-roma.11
  (format nil "~@R" :test))


;;
;;  Plural
;;
(deftest format-plural.1
  (format nil "hello~P" 1)
  "hello")

(deftest format-plural.2
  (format nil "hello~P" 0)
  "hellos")

(deftest format-plural.3
  (format nil "hello~P" 100)
  "hellos")

(deftest format-plural.4
  (format nil "hello~P" 1.0)
  "hellos")

(deftest format-plural.5
  (format nil "hello~P" :test)
  "hellos")

(deftest format-plural.6
  (format nil "hello~P" 1.0)
  "hellos")

(deftest format-plural.7
  (format nil "miff~@P" 1)
  "miffy")

(deftest format-plural.8
  (format nil "miff~@P" 3)
  "miffies")

(deftest format-plural.9
  (format nil "~D hello~:P" 1)
  "1 hello")

(deftest format-plural.10
  (format nil "~D hello~:P" 3)
  "3 hellos")

(deftest format-plural.11
  (format nil "~D miff~:@P" 1)
  "1 miffy")

(deftest format-plural.12
  (format nil "~D miff~:@P" 3)
  "3 miffies")

(deftest-error format-plural.13
  (format nil "~10P" 3))


;;
;;  Character
;;
(deftest format-character.1
  (format nil "~C" #\A)
  "A")

(deftest format-character.2
  (format nil "~@C" #\A)
  "#\\A")

(deftest format-character.3
  (format nil "~:C" #\A)
  "A")

(deftest format-character.4
  (format nil "~:@C" #\A)
  "A")

(deftest format-character.5
  (format nil "~C" #\Space)
  " ")

(deftest format-character.6
  (format nil "~@C" #\Space)
  "#\\Space")

(deftest format-character.7
  (format nil "~:C" #\Space)
  "Space")

(deftest format-character.8
  (format nil "~:@C" #\Space)
  "Space")

(deftest-error format-character.9
  (format nil "~10C" #\Space))

(deftest-error format-character.10
  (format nil "~C" 100))


;;
;;  Fixed
;;
(deftest format-fixed.1
  (format nil "~F" 12.3)
  "12.3")

(deftest format-fixed.2
  (format nil "~f" 10)
  "10.0")

(deftest format-fixed.3
  (format nil "~F" -1.25)
  "-1.25")

(deftest format-fixed.4
  (format nil "~@F" 1.25)
  "+1.25")

(deftest format-fixed.5
  (format nil "~10F" 1.25)
  "      1.25")
(deftest format-fixed.6
  (format nil "~10,,,,'*F" 1.25)
  "******1.25")

(deftest format-fixed.7
  (format nil "~5,,5F" 1.25)
  "125000.")

(deftest format-fixed.8
  (format nil "~5,,5,'=F" 1.25)
  "=====")

(deftest-error format-fixed.9
  (format nil "~F" :test))

(deftest-error format-fixed.10
  (format nil "~-1F" 1.25))

(deftest-error format-fixed.11
  (format nil "~,-1F" 1.25))

(deftest-error format-fixed.12
  (format nil "~,,'*F" 1.25))

(deftest-error format-fixed.13
  (format nil "~,,,10F" 1.25))

(deftest-error format-fixed.14
  (format nil "~,,,,20F" 1.25))

(deftest-error format-fixed.15
  (format nil "~,,,,,30F" 1.25))

(deftest-error format-fixed.16
  (format nil "~:F" 1.25))


;;
;;  Exponential
;;
(deftest format-exponential.1
  (format nil "~E" 12.3)
  "1.23E+1")

(deftest format-exponential.2
  (format nil "~e" 10)
  "1.0E+1")

(deftest format-exponential.3
  (format nil "~E" -1.25)
  "-1.25E+0")

(deftest format-exponential.4
  (format nil "~@E" 1.25)
  "+1.25E+0")

(deftest format-exponential.5
  (format nil "~10E" 1.25)
  "   1.25E+0")

(deftest format-exponential.6
  (format nil "~10,,,,,'*E" 1.25)
  "***1.25E+0")

(deftest format-exponential.7
  (format nil "~10,,,10E" 1.25)
  "1250000000.E-9")

(deftest format-exponential.8
  (format nil "~10,,,10,'=E" 1.25)
  "==========")

(deftest format-exponential.9
  (format nil "~,,,,,,'?E" -1.25)
  "-1.25?+0")

(deftest-error format-exponential.10
  (format nil "~E" :test))

(deftest-error format-exponential.11
  (format nil "~-1E" 1.25))

(deftest-error format-exponential.12
  (format nil "~,-1E" 1.25))

(deftest-error format-exponential.13
  (format nil "~,,'*E" 1.25))

(deftest-error format-exponential.14
  (format nil "~,,,'*E" 1.25))

(deftest-error format-exponential.15
  (format nil "~,,,,20E" 1.25))

(deftest-error format-exponential.16
  (format nil "~,,,,,30E" 1.25))

(deftest-error format-exponential.17
  (format nil "~,,,,,,40E" 1.25))

(deftest-error format-exponential.18
  (format nil "~:E" 1.25))

(deftest format-exponential.19
  (let ((*read-default-float-format* 'single-float))
    (format nil "~E" 1.25d0))
  "1.25D+0")

(deftest format-exponential.20
  (let ((*read-default-float-format* 'double-float))
    (format nil "~E" 1.25d0))
  "1.25E+0")

(deftest format-exponential.21
  (values
    (format nil "~E" 1.0f-4)
    (format nil "~E" 1.0f+8))
  "1.0E-4"
  "1.0E+8")

(deftest format-exponential.22
  (values
    (prin1-to-string 1.0f-4)
    (prin1-to-string 1.0f+8))
  "1.0E-4"
  "1.0E8")


;;
;;  General
;;
(deftest format-general.1
  (format nil "~G" 12.3)
  "12.3    ")

(deftest format-general.2
  (format nil "~g" 10)
  "10.0    ")

(deftest format-general.3
  (format nil "~G" -1.25)
  "-1.25    ")

(deftest format-general.4
  (format nil "~@G" 1.25)
  "+1.25    ")

(deftest format-general.5
  (format nil "~10G" 1.25)
  "  1.25    ")

(deftest format-general.6
  (format nil "~10,,,,,'*G" 1.25)
  "**1.25    ")

(deftest format-general.7
  (format nil "~G" 0.01)
  "1.0E-2")

(deftest format-general.8
  (format nil "~10,,,10G" 0.01)
  "1000000000.E-11")

(deftest format-general.9
  (format nil "~10,,,10,'=G" 0.01)
  "==========")

(deftest format-general.10
  (format nil "~,,,,,,'?G" -0.01)
  "-1.0?-2")

(deftest-error format-general.11
  (format nil "~G" :test))

(deftest-error format-general.12
  (format nil "~-1G" 1.25))

(deftest-error format-general.13
  (format nil "~,-1G" 1.25))

(deftest-error format-general.14
  (format nil "~,,'*G" 1.25))

(deftest-error format-general.15
  (format nil "~,,,'*G" 1.25))

(deftest-error format-general.16
  (format nil "~,,,,20G" 1.25))

(deftest-error format-general.17
  (format nil "~,,,,,30G" 1.25))

(deftest-error format-general.18
  (format nil "~,,,,,,40G" 1.25))

(deftest-error format-general.19
  (format nil "~:G" 1.25))


;;
;;  Monetary
;;
(deftest format-monetary.1
  (format nil "~$" 10)
  "10.00")

(deftest format-monetary.2
  (format nil "~$" -765)
  "-765.00")

(deftest format-monetary.3
  (format nil "~$" 1234.56789)
  "1234.57")

(deftest format-monetary.4
  (format nil "~3$" -1234.56789)
  "-1234.568")

(deftest format-monetary.5
  (format nil "~3,10$" 1234.56789)
  "0000001234.568")

(deftest format-monetary.6
  (format nil "~3,10$" -1234.56789)
  "-0000001234.568")

(deftest format-monetary.7
  (format nil "~,,10$" 1234.56789)
  "   1234.57")

(deftest format-monetary.8
  (format nil "~,,10$" -1234.56789)
  "  -1234.57")

(deftest format-monetary.9
  (format nil "~,,10@$" 1234.56789)
  "  +1234.57")

(deftest format-monetary.10
  (format nil "~,,10,'=@$" 1234.56789)
  "==+1234.57")

(deftest format-monetary.11
  (format nil "~,,10:$" -1234.56789)
  "-  1234.57")

(deftest format-monetary.12
  (format nil "~,,10:$" 1234.56789)
  "   1234.57")

(deftest format-monetary.13
  (format nil "~,,10:@$" 1234.56789)
  "+  1234.57")

(deftest format-monetary.14
  (format nil "~$" 100)
  "100.00")

(deftest format-monetary.15
  (format nil "~$" :test)
  "TEST")

(deftest format-monetary.16
  (format nil "~,,20,'=$" #c(10 20))
  "           #C(10 20)")

(deftest-error format-monetary.17
  (format nil "~-1$" 123))

(deftest-error format-monetary.18
  (format nil "~,-1$" 123))

(deftest-error format-monetary.19
  (format nil "~,,-1$" 123))

(deftest-error format-monetary.20
  (format nil "~,,,10$" 123))

(deftest-error format-monetary.21
  (format nil "~,,,,20$" 123))


;;
;;  Newline
;;
(deftest format-newline.1
  (format nil "~%")
  #.(mkstr #\newline))

(deftest format-newline.2
  (format nil "~%~%~%")
  #.(mkstr #\newline #\newline #\newline))

(deftest format-newline.3
  (format nil "~3%")
  #.(mkstr #\newline #\newline #\newline))

(deftest format-newline.4
  (format nil "~0%")
  "")

(deftest-error format-newline.5
  (format nil "~-1%"))

(deftest-error format-newline.6
  (format nil "~,1%"))


;;
;;  FreshLine
;;
(deftest format-fresh-line.1
  (format nil "~&")
  "")

(deftest format-fresh-line.2
  (format nil "aaa~&")
  #.(mkstr "aaa" #\newline))

(deftest format-fresh-line.3
  (format nil "aaa~&~&~&")
  #.(mkstr "aaa" #\newline))

(deftest format-fresh-line.4
  (format nil "~3&")
  #.(mkstr #\newline #\newline))

(deftest format-fresh-line.5
  (format nil "aaa~3&")
  #.(mkstr "aaa" #\newline #\newline #\newline))

(deftest format-fresh-line.6
  (format nil "~0&")
  "")

(deftest format-fresh-line.7
  (format nil "aaa~0&")
  "aaa")

(deftest-error format-fresh-line.8
  (format nil "~-1&"))

(deftest-error format-fresh-line.9
  (format nil "~,1&"))


;;
;;  Page
;;
(deftest format-page.1
  (format nil "~|")
  #.(mkstr #\page))

(deftest format-page.2
  (format nil "~|~|~|")
  #.(mkstr #\page #\page #\page))

(deftest format-page.3
  (format nil "~3|")
  #.(mkstr #\page #\page #\page))

(deftest format-page.4
  (format nil "~0|")
  "")

(deftest-error format-page.5
  (format nil "~-1|"))

(deftest-error format-page.6
  (format nil "~,1|"))


;;
;;  Tilde
;;
(deftest format-tilde.1
  (format nil "~~")
  "~")

(deftest format-tilde.2
  (format nil "~~~~~~")
  "~~~")

(deftest format-tilde.3
  (format nil "~3~")
  "~~~")

(deftest format-tilde.4
  (format nil "~0~")
  "")

(deftest-error format-tilde.5
  (format nil "~-1~"))

(deftest-error format-tilde.6
  (format nil "~,1~"))


;;
;;  IgnoredNewline
;;
(deftest format-ignored-newline.1
  (format nil (mkstr "ABC~" #\newline "DEC"))
  #.(mkstr "ABCDEC"))

(deftest format-ignored-newline.2
  (format nil (mkstr "ABC~" #\newline "    DEC"))
  #.(mkstr "ABCDEC"))

(deftest format-ignored-newline.3
  (format nil (mkstr "ABC~" #\newline "    DEC"))
  #.(mkstr "ABCDEC"))

(deftest format-ignored-newline.4
  (format nil (mkstr "ABC~:" #\newline "    DEC"))
  #.(mkstr "ABC    DEC"))

(deftest format-ignored-newline.5
  (format nil (mkstr "ABC~@" #\newline "    DEC"))
  #.(mkstr "ABC" #\newline "DEC"))

(deftest format-ignored-newline.6
  (format nil (mkstr "ABC~:@" #\newline "    DEC"))
  #.(mkstr "ABC" #\newline "    DEC"))

(deftest-error format-ignored-newline.7
  (format nil (mkstr "ABC~10:@" #\newline "    DEC")))


;;
;;  Tabulate
;;
(deftest format-tabulate.1
  (format nil "~T")
  " ")

(deftest format-tabulate.2
  (format nil "~5T")
  "     ")

(deftest format-tabulate.3
  (format nil "123~4T")
  "123 ")

(deftest format-tabulate.4
  (format nil "1234~4T")
  "1234 ")

(deftest format-tabulate.5
  (format nil "12345~4T")
  "12345 ")

(deftest format-tabulate.6
  (format nil "123~4,5T")
  "123 ")

(deftest format-tabulate.7
  (format nil "1234~4,5T")
  "1234     ")

(deftest format-tabulate.8
  (format nil "12345~4,5T")
  "12345    ")

(deftest format-tabulate.9
  (format nil "~0T")
  " ")

(deftest format-tabulate.10
  (format nil "~0,0T")
  "")

(deftest format-tabulate.11
  (format nil "1234~3,0T")
  "1234")

(deftest format-tabulate.12
  (format nil "1234~4,0T")
  "1234")

(deftest format-tabulate.13
  (format nil "1234~5,0T")
  "1234 ")

(deftest format-tabulate.14
  (format nil "1234567890~14,3TABC")
  "1234567890    ABC")

(deftest format-tabulate.15
  (format nil "1234567890~2,7TABC")
  "1234567890      ABC")

(deftest format-tabulate.16
  (format nil "~@T")
  " ")

(deftest format-tabulate.17
  (format nil "~4@T")
  "    ")

(deftest format-tabulate.18
  (format nil "123~4@T")
  "123    ")

(deftest format-tabulate.19
  (format nil "123~3,4@T")
  "123     ")

(deftest format-tabulate.20
  (format nil "123~3,0@T")
  "123   ")

(deftest-error format-tabulate.21
  (format nil "~-1T"))

(deftest-error format-tabulate.22
  (format nil "~,-1T"))

(deftest-error format-tabulate.23
  (format nil "~,,1T"))


;;
;;  ConditionalNewline
;;
(deftest format-conditional-newline.1
  (format nil "~_")
  "")

(deftest format-conditional-newline.2
  (format nil "~:_")
  "")

(deftest format-conditional-newline.3
  (format nil "~@_")
  "")

(deftest format-conditional-newline.4
  (format nil "~:@_")
  "")

(deftest-error format-conditional-newline.5
  (format nil "~10_"))


;;
;;  Write
;;
(deftest format-write.1
  (with-default-print
    (format nil "~W" #\A))
  "#\\A")

(deftest format-write.2
  (with-default-print
    (let ((*print-escape* nil))
      (format nil "~W" #\A)))
  "A")

(deftest format-write.3
  (with-default-print
    (let ((*print-level* 1)
          (*print-length* 1))
      (format nil "~W" '(((((10)))) 20 30))))
  "(# ...)")

(deftest format-write.4
  (with-default-print
    (let ((*print-level* 1)
          (*print-length* 1))
      (format nil "~@W" '(((((10)))) 20 30))))
  "(((((10)))) 20 30)")

(deftest format-write.5
  (with-default-print
    (let ((*print-pretty* nil)
          (*print-right-margin* 5))
      (format nil "~:W" '(10 20 30 40))))
  #.(mkstr "(10" #\newline " 20" #\newline " 30" #\newline " 40)"))

(deftest format-write.6
  (with-default-print
    (let ((*print-pretty* nil)
          (*print-level* 1)
          (*print-length* 1)
          (*print-right-margin* 5))
      (format nil "~:@W" '((((10))) 20 30 40))))
  #.(mkstr "((((10)))" #\newline " 20" #\newline " 30" #\newline " 40)"))


;;
;;  Indent
;;
(deftest format-indent.1
  (format nil "~I")
  "")

(deftest format-indent.2
  (format nil "~10I")
  "")

(deftest format-indent.3
  (format nil "~:I")
  "")

(deftest format-indent.4
  (format nil "~10:I")
  "")

(deftest format-indent.5
  (format nil "~-1I")
  "")

(deftest format-indent.6
  (format nil "~-1:I")
  "")

(deftest-error format-indent.7
  (format nil "~,1I"))

(deftest-error format-indent.8
  (format nil "~,1:I"))


;;
;;  CallFunction
;;
(defun format-call-test (stream v colon atsign &rest args)
  (princ (list* v colon atsign args) stream))

(defpackage format-package)
(defun format-package::call (stream v colon atsign &rest args)
  (princ (list* :call v colon atsign args) stream))

(deftest format-call-function.1
  (format nil "~/format-call-test/" 100)
  "(100 NIL NIL)")

(deftest format-call-function.2
  (format nil "~/format-package::call/" 100)
  "(CALL 100 NIL NIL)")

(deftest format-call-function.3
  (format nil "~:/FORMAT-call-TEST/" 100)
  "(100 T NIL)")

(deftest format-call-function.4
  (format nil "~1,2,'=@/format-call-test/" 100)
  "(100 NIL T 1 2 =)")

