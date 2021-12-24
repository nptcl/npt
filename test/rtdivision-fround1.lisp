;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  fround1
;;
(deftest fround1-integer.1
  (fround 0)
  0.0 0)

(deftest fround1-integer.2
  (fround 10)
  10.0 0)

(deftest fround1-integer.3
  (fround -10)
  -10.0 0)

(deftest fround1-integer.4
  (fround 99999999999999999999)
  99999999999999999999.0 0)

(deftest fround1-integer.5
  (fround -99999999999999999999)
  -99999999999999999999.0 0)

(deftest fround1-ratio.1
  (fround 1/3)
  0.0 1/3)

(deftest fround1-ratio.2
  (fround 1/2)
  0.0 1/2)

(deftest fround1-ratio.3
  (fround 2/3)
  1.0 -1/3)

(deftest fround1-ratio.4
  (fround -1/3)
  0.0 -1/3)

(deftest fround1-ratio.5
  (fround -1/2)
  0.0 -1/2)

(deftest fround1-ratio.6
  (fround -2/3)
  -1.0 1/3)

(deftest fround1-ratio.7
  (fround 16/3)
  5.0 1/3)

(deftest fround1-ratio.8
  (fround 11/2)
  6.0 -1/2)

(deftest fround1-ratio.9
  (fround 17/3)
  6.0 -1/3)

(deftest fround1-ratio.10
  (fround 19/3)
  6.0 1/3)

(deftest fround1-ratio.11
  (fround 13/2)
  6.0 1/2)

(deftest fround1-ratio.12
  (fround 20/3)
  7.0 -1/3)

(deftest fround1-ratio.13
  (fround -16/3)
  -5.0 -1/3)

(deftest fround1-ratio.14
  (fround -11/2)
  -6.0 1/2)

(deftest fround1-ratio.15
  (fround -17/3)
  -6.0 1/3)

(deftest fround1-ratio.16
  (fround -19/3)
  -6.0 -1/3)

(deftest fround1-ratio.17
  (fround -13/2)
  -6.0 -1/2)

(deftest fround1-ratio.18
  (fround -20/3)
  -7.0 1/3)

(deftest fround1-float.1
  (fround 10.0)
  10.0 0.0)

(deftest fround1-float.2
  (fround -10.0)
  -10.0 0.0)

(deftest fround1-float.3
  (fround 0.25)
  0.0 0.25)

(deftest fround1-float.4
  (fround 0.5)
  0.0 0.5)

(deftest fround1-float.5
  (fround 0.75)
  1.0 -0.25)

(deftest fround1-float.6
  (fround -0.25)
  -0.0 -0.25)

(deftest fround1-float.7
  (fround -0.5)
  -0.0 -0.5)

(deftest fround1-float.8
  (fround -0.75)
  -1.0 0.25)

(deftest fround1-float.9
  (fround 12.25)
  12.0 0.25)

(deftest fround1-float.10
  (fround 12.5)
  12.0 0.5)

(deftest fround1-float.11
  (fround 12.75)
  13.0 -0.25)

(deftest fround1-float.12
  (fround -12.25)
  -12.0 -0.25)

(deftest fround1-float.13
  (fround -12.5)
  -12.0 -0.5)

(deftest fround1-float.14
  (fround -12.75)
  -13.0 0.25)

(deftest fround1-float.15
  (fround 13.25)
  13.0 0.25)

(deftest fround1-float.16
  (fround 13.5)
  14.0 -0.5)

(deftest fround1-float.17
  (fround 13.75)
  14.0 -0.25)

(deftest fround1-float.18
  (fround -13.25)
  -13.0 -0.25)

(deftest fround1-float.19
  (fround -13.5)
  -14.0 0.5)

(deftest fround1-float.20
  (fround -13.75)
  -14.0 0.25)

(deftest fround1-single.1
  (fround 10.0f0)
  10.0f0 0.0f0)

(deftest fround1-single.2
  (fround -10.0f0)
  -10.0f0 0.0f0)

(deftest fround1-single.3
  (fround 0.25f0)
  0.0f0 0.25f0)

(deftest fround1-single.4
  (fround 0.5f0)
  0.0f0 0.5f0)

(deftest fround1-single.5
  (fround 0.75f0)
  1.0f0 -0.25f0)

(deftest fround1-single.6
  (fround -0.25f0)
  -0.0f0 -0.25f0)

(deftest fround1-single.7
  (fround -0.5f0)
  -0.0f0 -0.5f0)

(deftest fround1-single.8
  (fround -0.75f0)
  -1.0f0 0.25f0)

(deftest fround1-single.9
  (fround 12.25f0)
  12.0f0 0.25f0)

(deftest fround1-single.10
  (fround 12.5f0)
  12.0f0 0.5f0)

(deftest fround1-single.11
  (fround 12.75f0)
  13.0f0 -0.25f0)

(deftest fround1-single.12
  (fround -12.25f0)
  -12.0f0 -0.25f0)

(deftest fround1-single.13
  (fround -12.5f0)
  -12.0f0 -0.5f0)

(deftest fround1-single.14
  (fround -12.75f0)
  -13.0f0 0.25f0)

(deftest fround1-single.15
  (fround 13.25f0)
  13.0f0 0.25f0)

(deftest fround1-single.16
  (fround 13.5f0)
  14.0f0 -0.5f0)

(deftest fround1-single.17
  (fround 13.75f0)
  14.0f0 -0.25f0)

(deftest fround1-single.18
  (fround -13.25f0)
  -13.0f0 -0.25f0)

(deftest fround1-single.19
  (fround -13.5f0)
  -14.0f0 0.5f0)

(deftest fround1-single.20
  (fround -13.75f0)
  -14.0f0 0.25f0)

(deftest fround1-double.1
  (fround 10.0d0)
  10.0d0 0.0d0)

(deftest fround1-double.2
  (fround -10.0d0)
  -10.0d0 0.0d0)

(deftest fround1-double.3
  (fround 0.25d0)
  0.0d0 0.25d0)

(deftest fround1-double.4
  (fround 0.5d0)
  0.0d0 0.5d0)

(deftest fround1-double.5
  (fround 0.75d0)
  1.0d0 -0.25d0)

(deftest fround1-double.6
  (fround -0.25d0)
  -0.0d0 -0.25d0)

(deftest fround1-double.7
  (fround -0.5d0)
  -0.0d0 -0.5d0)

(deftest fround1-double.8
  (fround -0.75d0)
  -1.0d0 0.25d0)

(deftest fround1-double.9
  (fround 12.25d0)
  12.0d0 0.25d0)

(deftest fround1-double.10
  (fround 12.5d0)
  12.0d0 0.5d0)

(deftest fround1-double.11
  (fround 12.75d0)
  13.0d0 -0.25d0)

(deftest fround1-double.12
  (fround -12.25d0)
  -12.0d0 -0.25d0)

(deftest fround1-double.13
  (fround -12.5d0)
  -12.0d0 -0.5d0)

(deftest fround1-double.14
  (fround -12.75d0)
  -13.0d0 0.25d0)

(deftest fround1-double.15
  (fround 13.25d0)
  13.0d0 0.25d0)

(deftest fround1-double.16
  (fround 13.5d0)
  14.0d0 -0.5d0)

(deftest fround1-double.17
  (fround 13.75d0)
  14.0d0 -0.25d0)

(deftest fround1-double.18
  (fround -13.25d0)
  -13.0d0 -0.25d0)

(deftest fround1-double.19
  (fround -13.5d0)
  -14.0d0 0.5d0)

(deftest fround1-double.20
  (fround -13.75d0)
  -14.0d0 0.25d0)

(deftest fround1-long.1
  (fround 10.0l0)
  10.0l0 0.0l0)

(deftest fround1-long.2
  (fround -10.0l0)
  -10.0l0 0.0l0)

(deftest fround1-long.3
  (fround 0.25l0)
  0.0l0 0.25l0)

(deftest fround1-long.4
  (fround 0.5l0)
  0.0l0 0.5l0)

(deftest fround1-long.5
  (fround 0.75l0)
  1.0l0 -0.25l0)

(deftest fround1-long.6
  (fround -0.25l0)
  -0.0l0 -0.25l0)

(deftest fround1-long.7
  (fround -0.5l0)
  -0.0l0 -0.5l0)

(deftest fround1-long.8
  (fround -0.75l0)
  -1.0l0 0.25l0)

(deftest fround1-long.9
  (fround 12.25l0)
  12.0l0 0.25l0)

(deftest fround1-long.10
  (fround 12.5l0)
  12.0l0 0.5l0)

(deftest fround1-long.11
  (fround 12.75l0)
  13.0l0 -0.25l0)

(deftest fround1-long.12
  (fround -12.25l0)
  -12.0l0 -0.25l0)

(deftest fround1-long.13
  (fround -12.5l0)
  -12.0l0 -0.5l0)

(deftest fround1-long.14
  (fround -12.75l0)
  -13.0l0 0.25l0)

(deftest fround1-long.15
  (fround 13.25l0)
  13.0l0 0.25l0)

(deftest fround1-long.16
  (fround 13.5l0)
  14.0l0 -0.5l0)

(deftest fround1-long.17
  (fround 13.75l0)
  14.0l0 -0.25l0)

(deftest fround1-long.18
  (fround -13.25l0)
  -13.0l0 -0.25l0)

(deftest fround1-long.19
  (fround -13.5l0)
  -14.0l0 0.5l0)

(deftest fround1-long.20
  (fround -13.75l0)
  -14.0l0 0.25l0)

