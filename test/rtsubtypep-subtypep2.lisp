;;
;;  subtypep range
;;
(deftest subtypep2-initialize
  (progn
    (setq *subtypep!* 'subtypep-atomic)
    (subtypep! nil nil nil t))
  subtypep-atomic)


;;
;;  range
;;
(deftest subtypep-range.1
  (subtypep! '(real 10 20) 'real)
  include)

(deftest subtypep-range.2
  (subtypep! 'real '(real 10))
  false)

(deftest subtypep-range.3
  (subtypep! '(real 2 *) '(real * 5))
  false)

(deftest subtypep-range.4
  (subtypep! '(real 2 3) '(real * 5))
  include)


;;
;;  (left * *)
;;
(deftest subtypep-range-aa.1
  (subtypep! 'real 'real)
  include)

(deftest subtypep-range-aa.2
  (subtypep! 'real '(real 10 *))
  false)

(deftest subtypep-range-aa.3
  (subtypep! 'real '(real * 20))
  false)

(deftest subtypep-range-aa.4
  (subtypep! 'real '(real 10 20))
  false)


;;
;;  (left v *)
;;

;;  (right * *)
(deftest subtypep-range-va-aa.1
  (subtypep! '(real 10 *) 'real)
  include)

(deftest subtypep-range-va-aa.2
  (subtypep! '(real (10) *) 'real)
  include)

;;  (right v *)
(deftest subtypep-range-va-va.1
  (subtypep! '(real 10 *) '(real 9 *))
  include)

(deftest subtypep-range-va-va.2
  (subtypep! '(real 10 *) '(real 10 *))
  include)

(deftest subtypep-range-va-va.3
  (subtypep! '(real 10 *) '(real 11 *))
  false)

(deftest subtypep-range-va-va.4
  (subtypep! '(real 10 *) '(real (9) *))
  include)

(deftest subtypep-range-va-va.5
  (subtypep! '(real 10 *) '(real (10) *))
  false)

(deftest subtypep-range-va-va.6
  (subtypep! '(real 10 *) '(real (11) *))
  false)

(deftest subtypep-range-va-va.7
  (subtypep! '(real (10) *) '(real 9 *))
  include)

(deftest subtypep-range-va-va.8
  (subtypep! '(real (10) *) '(real 10 *))
  include)

(deftest subtypep-range-va-va.9
  (subtypep! '(real (10) *) '(real 11 *))
  false)

(deftest subtypep-range-va-va.10
  (subtypep! '(real (10) *) '(real (9) *))
  include)

(deftest subtypep-range-va-va.11
  (subtypep! '(real (10) *) '(real (10) *))
  include)

(deftest subtypep-range-va-va.12
  (subtypep! '(real (10) *) '(real (11) *))
  false)

;;  (right * v)
(deftest subtypep-range-va-av.1
  (subtypep! '(real 10 *) '(real * 9))
  exclude)

(deftest subtypep-range-va-av.2
  (subtypep! '(real 10 *) '(real * 10))
  false)

(deftest subtypep-range-va-av.3
  (subtypep! '(real 10 *) '(real * 11))
  false)

(deftest subtypep-range-va-av.4
  (subtypep! '(real 10 *) '(real * (9)))
  exclude)

(deftest subtypep-range-va-av.5
  (subtypep! '(real 10 *) '(real * (10)))
  exclude)

(deftest subtypep-range-va-av.6
  (subtypep! '(real 10 *) '(real * (11)))
  false)

(deftest subtypep-range-va-av.7
  (subtypep! '(real (10) *) '(real * 9))
  exclude)

(deftest subtypep-range-va-av.8
  (subtypep! '(real (10) *) '(real * 10))
  exclude)

(deftest subtypep-range-va-av.9
  (subtypep! '(real (10) *) '(real * 11))
  false)

(deftest subtypep-range-va-av.10
  (subtypep! '(real (10) *) '(real * (9)))
  exclude)

(deftest subtypep-range-va-av.11
  (subtypep! '(real (10) *) '(real * (10)))
  exclude)

(deftest subtypep-range-va-av.12
  (subtypep! '(real (10) *) '(real * (11)))
  false)

;;  (right v v)
(deftest subtypep-range-va-vv.1
  (subtypep! '(real 10 *) '(real 6 9))
  exclude)

(deftest subtypep-range-va-vv.2
  (subtypep! '(real 10 *) '(real 6 10))
  false)

(deftest subtypep-range-va-vv.3
  (subtypep! '(real 10 *) '(real 6 11))
  false)

(deftest subtypep-range-va-vv.4
  (subtypep! '(real 10 *) '(real 9 20))
  false)

(deftest subtypep-range-va-vv.5
  (subtypep! '(real 10 *) '(real 10 20))
  false)

(deftest subtypep-range-va-vv.6
  (subtypep! '(real 10 *) '(real 11 20))
  false)

(deftest subtypep-range-va-vv.7
  (subtypep! '(real 10 *) '(real 6 (9)))
  exclude)

(deftest subtypep-range-va-vv.8
  (subtypep! '(real 10 *) '(real 6 (10)))
  exclude)

(deftest subtypep-range-va-vv.9
  (subtypep! '(real 10 *) '(real 6 (11)))
  false)

(deftest subtypep-range-va-vv.10
  (subtypep! '(real 10 *) '(real (9) 20))
  false)

(deftest subtypep-range-va-vv.11
  (subtypep! '(real 10 *) '(real (10) 20))
  false)

(deftest subtypep-range-va-vv.12
  (subtypep! '(real 10 *) '(real (11) 20))
  false)

(deftest subtypep-range-va-vv.13
  (subtypep! '(real (10) *) '(real 6 9))
  exclude)

(deftest subtypep-range-va-vv.14
  (subtypep! '(real (10) *) '(real 6 10))
  exclude)

(deftest subtypep-range-va-vv.15
  (subtypep! '(real (10) *) '(real 6 11))
  false)

(deftest subtypep-range-va-vv.16
  (subtypep! '(real (10) *) '(real 9 20))
  false)

(deftest subtypep-range-va-vv.17
  (subtypep! '(real (10) *) '(real 10 20))
  false)

(deftest subtypep-range-va-vv.18
  (subtypep! '(real (10) *) '(real 11 20))
  false)

(deftest subtypep-range-va-vv.19
  (subtypep! '(real (10) *) '(real 6 (9)))
  exclude)

(deftest subtypep-range-va-vv.20
  (subtypep! '(real (10) *) '(real 6 (10)))
  exclude)

(deftest subtypep-range-va-vv.21
  (subtypep! '(real (10) *) '(real 6 (11)))
  false)

(deftest subtypep-range-va-vv.22
  (subtypep! '(real (10) *) '(real (9) 20))
  false)

(deftest subtypep-range-va-vv.23
  (subtypep! '(real (10) *) '(real (10) 20))
  false)

(deftest subtypep-range-va-vv.24
  (subtypep! '(real (10) *) '(real (11) 20))
  false)


;;
;;  (left * v)
;;

;;  (right * *)
(deftest subtypep-range-av-aa.1
  (subtypep! '(real * 10) 'real)
  include)

(deftest subtypep-range-av-aa.2
  (subtypep! '(real * (10)) 'real)
  include)

;;  (right v *)
(deftest subtypep-range-av-va.1
  (subtypep! '(real * 10) '(real 9 *))
  false)

(deftest subtypep-range-av-va.2
  (subtypep! '(real * 10) '(real 10 *))
  false)

(deftest subtypep-range-av-va.3
  (subtypep! '(real * 10) '(real 11 *))
  exclude)

(deftest subtypep-range-av-va.4
  (subtypep! '(real * 10) '(real (9) *))
  false)

(deftest subtypep-range-av-va.5
  (subtypep! '(real * 10) '(real (10) *))
  exclude)

(deftest subtypep-range-av-va.6
  (subtypep! '(real * 10) '(real (11) *))
  exclude)

(deftest subtypep-range-av-va.7
  (subtypep! '(real * (10)) '(real 9 *))
  false)

(deftest subtypep-range-av-va.8
  (subtypep! '(real * (10)) '(real 10 *))
  exclude)

(deftest subtypep-range-av-va.9
  (subtypep! '(real * (10)) '(real 11 *))
  exclude)

(deftest subtypep-range-av-va.10
  (subtypep! '(real * (10)) '(real (9) *))
  false)

(deftest subtypep-range-av-va.11
  (subtypep! '(real * (10)) '(real (10) *))
  exclude)

(deftest subtypep-range-av-va.12
  (subtypep! '(real * (10)) '(real (11) *))
  exclude)

;;  (right * v)
(deftest subtypep-range-av-av.1
  (subtypep! '(real * 10) '(real * 9))
  false)

(deftest subtypep-range-av-av.2
  (subtypep! '(real * 10) '(real * 10))
  include)

(deftest subtypep-range-av-av.3
  (subtypep! '(real * 10) '(real * 11))
  include)

(deftest subtypep-range-av-av.4
  (subtypep! '(real * 10) '(real * (9)))
  false)

(deftest subtypep-range-av-av.5
  (subtypep! '(real * 10) '(real * (10)))
  false)

(deftest subtypep-range-av-av.6
  (subtypep! '(real * 10) '(real * (11)))
  include)

(deftest subtypep-range-av-av.7
  (subtypep! '(real * (10)) '(real * 9))
  false)

(deftest subtypep-range-av-av.8
  (subtypep! '(real * (10)) '(real * 10))
  include)

(deftest subtypep-range-av-av.9
  (subtypep! '(real * (10)) '(real * 11))
  include)

(deftest subtypep-range-av-av.10
  (subtypep! '(real * (10)) '(real * (9)))
  false)

(deftest subtypep-range-av-av.11
  (subtypep! '(real * (10)) '(real * (10)))
  include)

(deftest subtypep-range-av-av.12
  (subtypep! '(real * (10)) '(real * (11)))
  include)

;;  (right v v)
(deftest subtypep-range-av-vv.1
  (subtypep! '(real * 10) '(real 6 9))
  false)

(deftest subtypep-range-av-vv.2
  (subtypep! '(real * 10) '(real 6 10))
  false)

(deftest subtypep-range-av-vv.3
  (subtypep! '(real * 10) '(real 6 11))
  false)

(deftest subtypep-range-av-vv.4
  (subtypep! '(real * 10) '(real 9 20))
  false)

(deftest subtypep-range-av-vv.5
  (subtypep! '(real * 10) '(real 10 20))
  false)

(deftest subtypep-range-av-vv.6
  (subtypep! '(real * 10) '(real 11 20))
  exclude)

(deftest subtypep-range-av-vv.7
  (subtypep! '(real * 10) '(real 6 (9)))
  false)

(deftest subtypep-range-av-vv.8
  (subtypep! '(real * 10) '(real 6 (10)))
  false)

(deftest subtypep-range-av-vv.9
  (subtypep! '(real * 10) '(real 6 (11)))
  false)

(deftest subtypep-range-av-vv.10
  (subtypep! '(real * 10) '(real (9) 20))
  false)

(deftest subtypep-range-av-vv.11
  (subtypep! '(real * 10) '(real (10) 20))
  exclude)

(deftest subtypep-range-av-vv.12
  (subtypep! '(real * 10) '(real (11) 20))
  exclude)

(deftest subtypep-range-av-vv.13
  (subtypep! '(real * (10)) '(real 6 9))
  false)

(deftest subtypep-range-av-vv.14
  (subtypep! '(real * (10)) '(real 6 10))
  false)

(deftest subtypep-range-av-vv.15
  (subtypep! '(real * (10)) '(real 6 11))
  false)

(deftest subtypep-range-av-vv.16
  (subtypep! '(real * (10)) '(real 9 20))
  false)

(deftest subtypep-range-av-vv.17
  (subtypep! '(real * (10)) '(real 10 20))
  exclude)

(deftest subtypep-range-av-vv.18
  (subtypep! '(real * (10)) '(real 11 20))
  exclude)

(deftest subtypep-range-av-vv.19
  (subtypep! '(real * (10)) '(real 6 (9)))
  false)

(deftest subtypep-range-av-vv.20
  (subtypep! '(real * (10)) '(real 6 (10)))
  false)

(deftest subtypep-range-av-vv.21
  (subtypep! '(real * (10)) '(real 6 (11)))
  false)

(deftest subtypep-range-av-vv.22
  (subtypep! '(real * (10)) '(real (9) 20))
  false)

(deftest subtypep-range-av-vv.23
  (subtypep! '(real * (10)) '(real (10) 20))
  exclude)

(deftest subtypep-range-av-vv.24
  (subtypep! '(real * (10)) '(real (11) 20))
  exclude)


;;
;;  (left v v)
;;

;;  (right * *)
(deftest subtypep-range-vv-aa.1
  (subtypep! '(real 10 20) 'real)
  include)

(deftest subtypep-range-vv-aa.2
  (subtypep! '(real (10) 20) 'real)
  include)

(deftest subtypep-range-vv-aa.3
  (subtypep! '(real 10 (20)) 'real)
  include)

(deftest subtypep-range-vv-aa.4
  (subtypep! '(real (10) (20)) 'real)
  include)

;;  (right v *)
(deftest subtypep-range-vv1-va.1
  (subtypep! '(real 10 20) '(real 9 *))
  include)

(deftest subtypep-range-vv1-va.2
  (subtypep! '(real 10 20) '(real 10 *))
  include)

(deftest subtypep-range-vv1-va.3
  (subtypep! '(real 10 20) '(real 11 *))
  false)

(deftest subtypep-range-vv1-va.4
  (subtypep! '(real 10 20) '(real (9) *))
  include)

(deftest subtypep-range-vv1-va.5
  (subtypep! '(real 10 20) '(real (10) *))
  false)

(deftest subtypep-range-vv1-va.6
  (subtypep! '(real 10 20) '(real (11) *))
  false)

(deftest subtypep-range-vv2-va.1
  (subtypep! '(real (10) 20) '(real 9 *))
  include)

(deftest subtypep-range-vv2-va.2
  (subtypep! '(real (10) 20) '(real 10 *))
  include)

(deftest subtypep-range-vv2-va.3
  (subtypep! '(real (10) 20) '(real 11 *))
  false)

(deftest subtypep-range-vv2-va.4
  (subtypep! '(real (10) 20) '(real (9) *))
  include)

(deftest subtypep-range-vv2-va.5
  (subtypep! '(real (10) 20) '(real (10) *))
  include)

(deftest subtypep-range-vv2-va.6
  (subtypep! '(real (10) 20) '(real (11) *))
  false)

(deftest subtypep-range-vv3-va.1
  (subtypep! '(real 10 20) '(real 19 *))
  false)

(deftest subtypep-range-vv3-va.2
  (subtypep! '(real 10 20) '(real 20 *))
  false)

(deftest subtypep-range-vv3-va.3
  (subtypep! '(real 10 20) '(real 21 *))
  exclude)

(deftest subtypep-range-vv3-va.4
  (subtypep! '(real 10 20) '(real (19) *))
  false)

(deftest subtypep-range-vv3-va.5
  (subtypep! '(real 10 20) '(real (20) *))
  exclude)

(deftest subtypep-range-vv3-va.6
  (subtypep! '(real 10 20) '(real (21) *))
  exclude)

(deftest subtypep-range-vv4-va.1
  (subtypep! '(real 10 (20)) '(real 19 *))
  false)

(deftest subtypep-range-vv4-va.2
  (subtypep! '(real 10 (20)) '(real 20 *))
  exclude)

(deftest subtypep-range-vv4-va.3
  (subtypep! '(real 10 (20)) '(real 21 *))
  exclude)

(deftest subtypep-range-vv4-va.4
  (subtypep! '(real 10 (20)) '(real (19) *))
  false)

(deftest subtypep-range-vv4-va.5
  (subtypep! '(real 10 (20)) '(real (20) *))
  exclude)

(deftest subtypep-range-vv4-va.6
  (subtypep! '(real 10 (20)) '(real (21) *))
  exclude)

;;  (right * v)
(deftest subtypep-range-vv1-av.1
  (subtypep! '(real 10 20) '(real * 9))
  exclude)

(deftest subtypep-range-vv1-av.2
  (subtypep! '(real 10 20) '(real * 10))
  false)

(deftest subtypep-range-vv1-av.3
  (subtypep! '(real 10 20) '(real * 11))
  false)

(deftest subtypep-range-vv1-av.4
  (subtypep! '(real 10 20) '(real * (9)))
  exclude)

(deftest subtypep-range-vv1-av.5
  (subtypep! '(real 10 20) '(real * (10)))
  exclude)

(deftest subtypep-range-vv1-av.6
  (subtypep! '(real 10 20) '(real * (11)))
  false)

(deftest subtypep-range-vv2-av.1
  (subtypep! '(real (10) 20) '(real * 9))
  exclude)

(deftest subtypep-range-vv2-av.2
  (subtypep! '(real (10) 20) '(real * 10))
  exclude)

(deftest subtypep-range-vv2-av.3
  (subtypep! '(real (10) 20) '(real * 11))
  false)

(deftest subtypep-range-vv2-av.4
  (subtypep! '(real (10) 20) '(real * (9)))
  exclude)

(deftest subtypep-range-vv2-av.5
  (subtypep! '(real (10) 20) '(real * (10)))
  exclude)

(deftest subtypep-range-vv2-av.6
  (subtypep! '(real (10) 20) '(real * (11)))
  false)

(deftest subtypep-range-vv3-av.1
  (subtypep! '(real 10 20) '(real * 19))
  false)

(deftest subtypep-range-vv3-av.2
  (subtypep! '(real 10 20) '(real * 20))
  include)

(deftest subtypep-range-vv3-av.3
  (subtypep! '(real 10 20) '(real * 21))
  include)

(deftest subtypep-range-vv3-av.4
  (subtypep! '(real 10 20) '(real * (19)))
  false)

(deftest subtypep-range-vv3-av.5
  (subtypep! '(real 10 20) '(real * (20)))
  false)

(deftest subtypep-range-vv3-av.6
  (subtypep! '(real 10 20) '(real * (21)))
  include)

(deftest subtypep-range-vv4-av.1
  (subtypep! '(real 10 (20)) '(real * 19))
  false)

(deftest subtypep-range-vv4-av.2
  (subtypep! '(real 10 (20)) '(real * 20))
  include)

(deftest subtypep-range-vv4-av.3
  (subtypep! '(real 10 (20)) '(real * 21))
  include)

(deftest subtypep-range-vv4-av.4
  (subtypep! '(real 10 (20)) '(real * (19)))
  false)

(deftest subtypep-range-vv4-av.5
  (subtypep! '(real 10 (20)) '(real * (20)))
  include)

(deftest subtypep-range-vv4-av.6
  (subtypep! '(real 10 (20)) '(real * (21)))
  include)

;;  (right v v)
(deftest subtypep-range-vv1-vv.1
  (subtypep! '(real 10 20) '(real 5 9))
  exclude)

(deftest subtypep-range-vv1-vv.2
  (subtypep! '(real 10 20) '(real 5 10))
  false)

(deftest subtypep-range-vv1-vv.3
  (subtypep! '(real 10 20) '(real 5 11))
  false)

(deftest subtypep-range-vv1-vv.4
  (subtypep! '(real 10 20) '(real 5 (9)))
  exclude)

(deftest subtypep-range-vv1-vv.5
  (subtypep! '(real 10 20) '(real 5 (10)))
  exclude)

(deftest subtypep-range-vv1-vv.6
  (subtypep! '(real 10 20) '(real 5 (11)))
  false)

(deftest subtypep-range-vv2-vv.1
  (subtypep! '(real (10) 20) '(real 5 9))
  exclude)

(deftest subtypep-range-vv2-vv.2
  (subtypep! '(real (10) 20) '(real 5 10))
  exclude)

(deftest subtypep-range-vv2-vv.3
  (subtypep! '(real (10) 20) '(real 5 11))
  false)

(deftest subtypep-range-vv2-vv.4
  (subtypep! '(real (10) 20) '(real 5 (9)))
  exclude)

(deftest subtypep-range-vv2-vv.5
  (subtypep! '(real (10) 20) '(real 5 (10)))
  exclude)

(deftest subtypep-range-vv2-vv.6
  (subtypep! '(real (10) 20) '(real 5 (11)))
  false)

(deftest subtypep-range-vv3-vv.1
  (subtypep! '(real 10 20) '(real 9 15))
  false)

(deftest subtypep-range-vv3-vv.2
  (subtypep! '(real 10 20) '(real 10 15))
  false)

(deftest subtypep-range-vv3-vv.3
  (subtypep! '(real 10 20) '(real 11 15))
  false)

(deftest subtypep-range-vv3-vv.4
  (subtypep! '(real 10 20) '(real (9) 15))
  false)

(deftest subtypep-range-vv3-vv.5
  (subtypep! '(real 10 20) '(real (10) 15))
  false)

(deftest subtypep-range-vv3-vv.6
  (subtypep! '(real 10 20) '(real (11) 15))
  false)

(deftest subtypep-range-vv4-vv.1
  (subtypep! '(real 10 20) '(real 15 19))
  false)

(deftest subtypep-range-vv4-vv.2
  (subtypep! '(real 10 20) '(real 15 20))
  false)

(deftest subtypep-range-vv4-vv.3
  (subtypep! '(real 10 20) '(real 15 21))
  false)

(deftest subtypep-range-vv4-vv.4
  (subtypep! '(real 10 20) '(real 15 (19)))
  false)

(deftest subtypep-range-vv4-vv.5
  (subtypep! '(real 10 20) '(real 15 (20)))
  false)

(deftest subtypep-range-vv4-vv.6
  (subtypep! '(real 10 20) '(real 15 (21)))
  false)

(deftest subtypep-range-vv5-vv.1
  (subtypep! '(real 10 20) '(real 5 19))
  false)

(deftest subtypep-range-vv5-vv.2
  (subtypep! '(real 10 20) '(real 5 20))
  include)

(deftest subtypep-range-vv5-vv.3
  (subtypep! '(real 10 20) '(real 5 21))
  include)

(deftest subtypep-range-vv5-vv.4
  (subtypep! '(real 10 20) '(real 5 (19)))
  false)

(deftest subtypep-range-vv5-vv.5
  (subtypep! '(real 10 20) '(real 5 (20)))
  false)

(deftest subtypep-range-vv5-vv.6
  (subtypep! '(real 10 20) '(real 5 (21)))
  include)

(deftest subtypep-range-vv6-vv.1
  (subtypep! '(real 10 20) '(real 9 30))
  include)

(deftest subtypep-range-vv6-vv.2
  (subtypep! '(real 10 20) '(real 10 30))
  include)

(deftest subtypep-range-vv6-vv.3
  (subtypep! '(real 10 20) '(real 11 30))
  false)

(deftest subtypep-range-vv6-vv.4
  (subtypep! '(real 10 20) '(real (9) 30))
  include)

(deftest subtypep-range-vv6-vv.5
  (subtypep! '(real 10 20) '(real (10) 30))
  false)

(deftest subtypep-range-vv6-vv.6
  (subtypep! '(real 10 20) '(real (11) 30))
  false)

(deftest subtypep-range-vv7-vv.1
  (subtypep! '(real 10 20) '(real 19 30))
  false)

(deftest subtypep-range-vv7-vv.2
  (subtypep! '(real 10 20) '(real 20 30))
  false)

(deftest subtypep-range-vv7-vv.3
  (subtypep! '(real 10 20) '(real 21 30))
  exclude)

(deftest subtypep-range-vv7-vv.4
  (subtypep! '(real 10 20) '(real (19) 30))
  false)

(deftest subtypep-range-vv7-vv.5
  (subtypep! '(real 10 20) '(real (20) 30))
  exclude)

(deftest subtypep-range-vv7-vv.6
  (subtypep! '(real 10 20) '(real (21) 30))
  exclude)


;;
;;  Error
;;
(deftest subtypep-range-error.1
  (subtypep 'real '(not integer))
  nil t)

