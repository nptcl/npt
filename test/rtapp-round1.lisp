;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  round1
;;
(deftest round1-integer.1
  (round 0)
  0 0)

(deftest round1-integer.2
  (round 10)
  10 0)

(deftest round1-integer.3
  (round -10)
  -10 0)

(deftest round1-integer.4
  (round 99999999999999999999)
  99999999999999999999 0)

(deftest round1-integer.5
  (round -99999999999999999999)
  -99999999999999999999 0)

(deftest round1-ratio.1
  (round 1/3)
  0 1/3)

(deftest round1-ratio.2
  (round 1/2)
  0 1/2)

(deftest round1-ratio.3
  (round 2/3)
  1 -1/3)

(deftest round1-ratio.4
  (round -1/3)
  0 -1/3)

(deftest round1-ratio.5
  (round -1/2)
  0 -1/2)

(deftest round1-ratio.6
  (round -2/3)
  -1 1/3)

(deftest round1-ratio.7
  (round 16/3)
  5 1/3)

(deftest round1-ratio.8
  (round 11/2)
  6 -1/2)

(deftest round1-ratio.9
  (round 17/3)
  6 -1/3)

(deftest round1-ratio.10
  (round 19/3)
  6 1/3)

(deftest round1-ratio.11
  (round 13/2)
  6 1/2)

(deftest round1-ratio.12
  (round 20/3)
  7 -1/3)

(deftest round1-ratio.13
  (round -16/3)
  -5 -1/3)

(deftest round1-ratio.14
  (round -11/2)
  -6 1/2)

(deftest round1-ratio.15
  (round -17/3)
  -6 1/3)

(deftest round1-ratio.16
  (round -19/3)
  -6 -1/3)

(deftest round1-ratio.17
  (round -13/2)
  -6 -1/2)

(deftest round1-ratio.18
  (round -20/3)
  -7 1/3)

(deftest round1-float.1
  (round 10.0)
  10 0.0)

(deftest round1-float.2
  (round -10.0)
  -10 0.0)

(deftest round1-float.3
  (round 0.25)
  0 0.25)

(deftest round1-float.4
  (round 0.5)
  0 0.5)

(deftest round1-float.5
  (round 0.75)
  1 -0.25)

(deftest round1-float.6
  (round -0.25)
  0 -0.25)

(deftest round1-float.7
  (round -0.5)
  0 -0.5)

(deftest round1-float.8
  (round -0.75)
  -1 0.25)

(deftest round1-float.9
  (round 12.25)
  12 0.25)

(deftest round1-float.10
  (round 12.5)
  12 0.5)

(deftest round1-float.11
  (round 12.75)
  13 -0.25)

(deftest round1-float.12
  (round -12.25)
  -12 -0.25)

(deftest round1-float.13
  (round -12.5)
  -12 -0.5)

(deftest round1-float.14
  (round -12.75)
  -13 0.25)

(deftest round1-float.15
  (round 13.25)
  13 0.25)

(deftest round1-float.16
  (round 13.5)
  14 -0.5)

(deftest round1-float.17
  (round 13.75)
  14 -0.25)

(deftest round1-float.18
  (round -13.25)
  -13 -0.25)

(deftest round1-float.19
  (round -13.5)
  -14 0.5)

(deftest round1-float.20
  (round -13.75)
  -14 0.25)

(deftest round1-single.1
  (round 10.0f0)
  10 0.0f0)

(deftest round1-single.2
  (round -10.0f0)
  -10 0.0f0)

(deftest round1-single.3
  (round 0.25f0)
  0 0.25f0)

(deftest round1-single.4
  (round 0.5f0)
  0 0.5f0)

(deftest round1-single.5
  (round 0.75f0)
  1 -0.25f0)

(deftest round1-single.6
  (round -0.25f0)
  0 -0.25f0)

(deftest round1-single.7
  (round -0.5f0)
  0 -0.5f0)

(deftest round1-single.8
  (round -0.75f0)
  -1 0.25f0)

(deftest round1-single.9
  (round 12.25f0)
  12 0.25f0)

(deftest round1-single.10
  (round 12.5f0)
  12 0.5f0)

(deftest round1-single.11
  (round 12.75f0)
  13 -0.25f0)

(deftest round1-single.12
  (round -12.25f0)
  -12 -0.25f0)

(deftest round1-single.13
  (round -12.5f0)
  -12 -0.5f0)

(deftest round1-single.14
  (round -12.75f0)
  -13 0.25f0)

(deftest round1-single.15
  (round 13.25f0)
  13 0.25f0)

(deftest round1-single.16
  (round 13.5f0)
  14 -0.5f0)

(deftest round1-single.17
  (round 13.75f0)
  14 -0.25f0)

(deftest round1-single.18
  (round -13.25f0)
  -13 -0.25f0)

(deftest round1-single.19
  (round -13.5f0)
  -14 0.5f0)

(deftest round1-single.20
  (round -13.75f0)
  -14 0.25f0)

(deftest round1-double.1
  (round 10.0d0)
  10 0.0d0)

(deftest round1-double.2
  (round -10.0d0)
  -10 0.0d0)

(deftest round1-double.3
  (round 0.25d0)
  0 0.25d0)

(deftest round1-double.4
  (round 0.5d0)
  0 0.5d0)

(deftest round1-double.5
  (round 0.75d0)
  1 -0.25d0)

(deftest round1-double.6
  (round -0.25d0)
  0 -0.25d0)

(deftest round1-double.7
  (round -0.5d0)
  0 -0.5d0)

(deftest round1-double.8
  (round -0.75d0)
  -1 0.25d0)

(deftest round1-double.9
  (round 12.25d0)
  12 0.25d0)

(deftest round1-double.10
  (round 12.5d0)
  12 0.5d0)

(deftest round1-double.11
  (round 12.75d0)
  13 -0.25d0)

(deftest round1-double.12
  (round -12.25d0)
  -12 -0.25d0)

(deftest round1-double.13
  (round -12.5d0)
  -12 -0.5d0)

(deftest round1-double.14
  (round -12.75d0)
  -13 0.25d0)

(deftest round1-double.15
  (round 13.25d0)
  13 0.25d0)

(deftest round1-double.16
  (round 13.5d0)
  14 -0.5d0)

(deftest round1-double.17
  (round 13.75d0)
  14 -0.25d0)

(deftest round1-double.18
  (round -13.25d0)
  -13 -0.25d0)

(deftest round1-double.19
  (round -13.5d0)
  -14 0.5d0)

(deftest round1-double.20
  (round -13.75d0)
  -14 0.25d0)

(deftest round1-long.1
  (round 10.0l0)
  10 0.0l0)

(deftest round1-long.2
  (round -10.0l0)
  -10 0.0l0)

(deftest round1-long.3
  (round 0.25l0)
  0 0.25l0)

(deftest round1-long.4
  (round 0.5l0)
  0 0.5l0)

(deftest round1-long.5
  (round 0.75l0)
  1 -0.25l0)

(deftest round1-long.6
  (round -0.25l0)
  0 -0.25l0)

(deftest round1-long.7
  (round -0.5l0)
  0 -0.5l0)

(deftest round1-long.8
  (round -0.75l0)
  -1 0.25l0)

(deftest round1-long.9
  (round 12.25l0)
  12 0.25l0)

(deftest round1-long.10
  (round 12.5l0)
  12 0.5l0)

(deftest round1-long.11
  (round 12.75l0)
  13 -0.25l0)

(deftest round1-long.12
  (round -12.25l0)
  -12 -0.25l0)

(deftest round1-long.13
  (round -12.5l0)
  -12 -0.5l0)

(deftest round1-long.14
  (round -12.75l0)
  -13 0.25l0)

(deftest round1-long.15
  (round 13.25l0)
  13 0.25l0)

(deftest round1-long.16
  (round 13.5l0)
  14 -0.5l0)

(deftest round1-long.17
  (round 13.75l0)
  14 -0.25l0)

(deftest round1-long.18
  (round -13.25l0)
  -13 -0.25l0)

(deftest round1-long.19
  (round -13.5l0)
  -14 0.5l0)

(deftest round1-long.20
  (round -13.75l0)
  -14 0.25l0)

