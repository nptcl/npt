;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  format ~R
;;
(deftest format-r.1
  (format nil "~R" 0)
  "zero")

(deftest format-r.2
  (format nil "~R" 1)
  "one")

(deftest format-r.3
  (format nil "~R" -1)
  "minus one")

(deftest format-r.4
  (format nil "~R" 1000)
  "one thousand")

(deftest format-r.5
  (format nil "~r" 20000)
  "twenty thousand")

(deftest format-r.6
  (format nil "~r" 20000)
  "twenty thousand")

(deftest format-r.7
  (format nil "~R" 12345678)
  "twelve million, three hundred and forty-five thousand, six hundred and seventy-eight")

(deftest format-r.8
  (format nil "~R" 10000008)
  "ten million, eight")

(deftest format-r.9
  (format nil "~R" 1606938044258990275541962092341162602522202993782792835301376)
  "one novendecillion, six hundred and six octodecillion, nine hundred and thirty-eight septendecillion, forty-four sedecillion, two hundred and fifty-eight quindecillion, nine hundred and ninety quattuordecillion, two hundred and seventy-five tredecillion, five hundred and forty-one duodecillion, nine hundred and sixty-two undecillion, ninety-two decillion, three hundred and forty-one nonillion, one hundred and sixty-two octillion, six hundred and two septillion, five hundred and twenty-two sextillion, two hundred and two quintillion, nine hundred and ninety-three quadrillion, seven hundred and eighty-two trillion, seven hundred and ninety-two billion, eight hundred and thirty-five million, three hundred and one thousand, three hundred and seventy-six")

