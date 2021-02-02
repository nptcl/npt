;;
;;  subtypep atomic-not
;;
(deftest subtypep3-initialize
  (progn
    (setq *subtypep!* 'subtypep-atomic-not)
    (subtypep! nil nil nil t))
  subtypep-atomic-not)


;;
;;  left
;;
(deftest subtypep-not-normal.1
  (subtypep! 'integer 'integer)
  include)

(deftest subtypep-not-normal.2
  (subtypep! 'cons 'integer)
  exclude)

(deftest subtypep-not-normal.3
  (subtypep! 'real 'integer)
  false)

;;  left
(deftest subtypep-not-left.1
  (subtypep! '(not integer) 'integer)
  exclude)

(deftest subtypep-not-left.2
  (subtypep! '(not real) 'integer)
  exclude)

(deftest subtypep-not-left.3
  (subtypep! '(not integer) 'real)
  false)

(deftest subtypep-not-left.4
  (subtypep! '(not cons) 'integer)
  false)

;;  right
(deftest subtypep-not-right.1
  (subtypep! 'integer '(not integer))
  exclude)

(deftest subtypep-not-right.2
  (subtypep! 'integer '(not real))
  exclude)

(deftest subtypep-not-right.3
  (subtypep! 'real '(not integer))
  false)

(deftest subtypep-not-right.4
  (subtypep! 'real '(not cons))
  include)

;;  both
(deftest subtypep-not-both.1
  (subtypep! '(not integer) '(not integer))
  include)

(deftest subtypep-not-both.2
  (subtypep! '(not real) '(not integer))
  include)

(deftest subtypep-not-both.3
  (subtypep! '(not integer) '(not real))
  false)

(deftest subtypep-not-both.4
  (subtypep! '(not cons) '(not integer))
  false)

