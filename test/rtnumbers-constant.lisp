;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Constant Variable PI
;;
(deftest pi-constant.1
  (typep pi 'long-float)
  t)

(deftest pi-constant.2
  (< 3.141592 pi 3.141593)
  t)

(deftest pi-constant.3
  (cos pi)
  -1.0L0)


;;
;;  Constant Variable MOST-POSITIVE-FIXNUM
;;
(deftest most-positive-fixnum-constant.1
  (typep most-positive-fixnum 'fixnum)
  t)

(deftest most-positive-fixnum-constant.2
  (< 0 most-positive-fixnum)
  t)

(deftest most-positive-fixnum-constant.3
  (typep (1+ most-positive-fixnum) 'bignum)
  t)

(deftest most-positive-fixnum-constant.4
  (<= (1- (expt 2 15)) most-positive-fixnum)  ;; 32767
  t)

(deftest most-positive-fixnum-constant.5
  (<= array-dimension-limit most-positive-fixnum)
  t)

(deftest most-positive-fixnum-constant.6
  (values
    (typep most-positive-fixnum 'fixnum)
    (= most-positive-fixnum #+fixnum-64 #x7FFFFFFFFFFFFFFF #-fixnum-64 #x7FFFFFFF))
  t t)


;;
;;  Constant Variable MOST-NEGATIVE-FIXNUM
;;
(deftest most-negative-fixnum-constant.1
  (typep most-negative-fixnum 'fixnum)
  t)

(deftest most-negative-fixnum-constant.2
  (< most-negative-fixnum 0)
  t)

(deftest most-negative-fixnum-constant.3
  (typep (1- most-negative-fixnum) 'bignum)
  t)

(deftest most-negative-fixnum-constant.4
  (<= most-negative-fixnum (- (expt 2 15)))  ;; -32768
  t)

(deftest most-negative-fixnum-constant.5
  (values
    (typep most-negative-fixnum 'fixnum)
    (= most-negative-fixnum #+fixnum-64 #x-8000000000000000 #-fixnum-64 #x-80000000))
  t t)


;;
;;  Constant Variable MOST-POSITIVE-SHORT-FLOAT
;;
(deftest most-positive-short-float-constant.1
  (typep most-positive-short-float 'short-float)
  t)

(deftest most-positive-short-float-constant.2
  (< 0 most-positive-short-float)
  t)

(deftest most-positive-short-float-constant.3
  (fpclassify most-positive-short-float)
  lisp-system:fp-normal 1)

(deftest most-positive-short-float-constant.4
  (values
    (typep most-positive-short-float 'single-float)
    (plusp most-positive-short-float)
    (zerop most-positive-short-float)
    (< 10s0 most-positive-short-float))
  t t nil t)


;;
;;  Constant Variable LEAST-POSITIVE-SHORT-FLOAT
;;
(deftest least-positive-short-float-constant.1
  (typep least-positive-short-float 'short-float)
  t)

(deftest least-positive-short-float-constant.2
  (< 0 least-positive-short-float)
  t)

(deftest least-positive-short-float-constant.3
  (fpclassify least-positive-short-float)
  lisp-system:fp-subnormal 1)

(deftest least-positive-short-float-constant.4
  (values
    (typep least-positive-short-float 'single-float)
    (plusp least-positive-short-float)
    (zerop least-positive-short-float))
  t t nil)


;;
;;  Constant Variable LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
;;
(deftest least-positive-normalized-short-float-constant.1
  (typep least-positive-normalized-short-float 'short-float)
  t)

(deftest least-positive-normalized-short-float-constant.2
  (< 0 least-positive-normalized-short-float)
  t)

(deftest least-positive-normalized-short-float-constant.3
  (fpclassify least-positive-normalized-short-float)
  lisp-system:fp-normal 1)

(deftest least-positive-normalized-short-float-constant.4
  (values
    (typep least-positive-normalized-short-float 'single-float)
    (plusp least-positive-normalized-short-float)
    (zerop least-positive-normalized-short-float)
    (<= least-positive-short-float least-positive-normalized-short-float))
  t t nil t)


;;
;;  Constant Variable MOST-POSITIVE-SINGLE-FLOAT
;;
(deftest most-positive-single-float-constant.1
  (typep most-positive-single-float 'single-float)
  t)

(deftest most-positive-single-float-constant.2
  (< 0 most-positive-single-float)
  t)

(deftest most-positive-single-float-constant.3
  (fpclassify most-positive-single-float)
  lisp-system:fp-normal 1)

(deftest most-positive-single-float-constant.4
  (values
    (typep most-positive-single-float 'single-float)
    (plusp most-positive-single-float)
    (zerop most-positive-single-float)
    (< 10f0 most-positive-single-float))
  t t nil t)


;;
;;  Constant Variable LEAST-POSITIVE-SINGLE-FLOAT
;;
(deftest least-positive-single-float-constant.1
  (typep least-positive-single-float 'single-float)
  t)

(deftest least-positive-single-float-constant.2
  (< 0 least-positive-single-float)
  t)

(deftest least-positive-single-float-constant.3
  (fpclassify least-positive-single-float)
  lisp-system:fp-subnormal 1)

(deftest least-positive-single-float-constant.4
  (values
    (typep least-positive-single-float 'single-float)
    (plusp least-positive-single-float)
    (zerop least-positive-single-float))
  t t nil)


;;
;;  Constant Variable LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
;;
(deftest least-positive-normalized-single-float-constant.1
  (typep least-positive-normalized-single-float 'single-float)
  t)

(deftest least-positive-normalized-single-float-constant.2
  (< 0 least-positive-normalized-single-float)
  t)

(deftest least-positive-normalized-single-float-constant.3
  (fpclassify least-positive-normalized-single-float)
  lisp-system:fp-normal 1)

(deftest least-positive-normalized-single-float-constant.4
  (values
    (typep least-positive-normalized-single-float 'single-float)
    (plusp least-positive-normalized-single-float)
    (zerop least-positive-normalized-single-float)
    (<= least-positive-single-float least-positive-normalized-single-float))
  t t nil t)


;;
;;  Constant Variable MOST-POSITIVE-DOUBLE-FLOAT
;;
(deftest most-positive-double-float-constant.1
  (typep most-positive-double-float 'double-float)
  t)

(deftest most-positive-double-float-constant.2
  (< 0 most-positive-double-float)
  t)

(deftest most-positive-double-float-constant.3
  (fpclassify most-positive-double-float)
  lisp-system:fp-normal 1)

(deftest most-positive-double-float-constant.4
  (values
    (typep most-positive-double-float 'double-float)
    (plusp most-positive-double-float)
    (zerop most-positive-double-float)
    (< 10d0 most-positive-double-float))
  t t nil t)


;;
;;  Constant Variable LEAST-POSITIVE-DOUBLE-FLOAT
;;
(deftest least-positive-double-float-constant.1
  (typep least-positive-double-float 'double-float)
  t)

(deftest least-positive-double-float-constant.2
  (< 0 least-positive-double-float)
  t)

(deftest least-positive-double-float-constant.3
  (fpclassify least-positive-double-float)
  lisp-system:fp-subnormal 1)

(deftest least-positive-double-float-constant.4
  (values
    (typep least-positive-double-float 'double-float)
    (plusp least-positive-double-float)
    (zerop least-positive-double-float))
  t t nil)


;;
;;  Constant Variable LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
;;
(deftest least-positive-normalized-double-float-constant.1
  (typep least-positive-normalized-double-float 'double-float)
  t)

(deftest least-positive-normalized-double-float-constant.2
  (< 0 least-positive-normalized-double-float)
  t)

(deftest least-positive-normalized-double-float-constant.3
  (fpclassify least-positive-normalized-double-float)
  lisp-system:fp-normal 1)

(deftest least-positive-normalized-double-float-constant.4
  (values
    (typep least-positive-normalized-double-float 'double-float)
    (plusp least-positive-normalized-double-float)
    (zerop least-positive-normalized-double-float)
    (<= least-positive-double-float least-positive-normalized-double-float))
  t t nil t)


;;
;;  Constant Variable MOST-POSITIVE-LONG-FLOAT
;;
(deftest most-positive-long-float-constant.1
  (typep most-positive-long-float 'long-float)
  t)

(deftest most-positive-long-float-constant.2
  (< 0 most-positive-long-float)
  t)

(deftest most-positive-long-float-constant.3
  (fpclassify most-positive-long-float)
  lisp-system:fp-normal 1)

(deftest most-positive-long-float-constant.4
  (values
    (typep most-positive-long-float 'long-float)
    (plusp most-positive-long-float)
    (zerop most-positive-long-float)
    (< 10l0 most-positive-long-float))
  t t nil t)


;;
;;  Constant Variable LEAST-POSITIVE-LONG-FLOAT
;;
(deftest least-positive-long-float-constant.1
  (typep least-positive-long-float 'long-float)
  t)

(deftest least-positive-long-float-constant.2
  (< 0 least-positive-long-float)
  t)

(deftest least-positive-long-float-constant.3
  (fpclassify least-positive-long-float)
  lisp-system:fp-subnormal 1)

(deftest least-positive-long-float-constant.4
  (values
    (typep least-positive-long-float 'long-float)
    (plusp least-positive-long-float)
    (zerop least-positive-long-float))
  t t nil)


;;
;;  Constant Variable LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
;;
(deftest least-positive-normalized-long-float-constant.1
  (typep least-positive-normalized-long-float 'long-float)
  t)

(deftest least-positive-normalized-long-float-constant.2
  (< 0 least-positive-normalized-long-float)
  t)

(deftest least-positive-normalized-long-float-constant.3
  (fpclassify least-positive-normalized-long-float)
  lisp-system:fp-normal 1)

(deftest least-positive-normalized-long-float-constant.4
  (values
    (typep least-positive-normalized-long-float 'long-float)
    (plusp least-positive-normalized-long-float)
    (zerop least-positive-normalized-long-float)
    (<= least-positive-long-float least-positive-normalized-long-float))
  t t nil t)


;;
;;  Constant Variable MOST-NEGATIVE-SHORT-FLOAT
;;
(deftest most-negative-short-float-constant.1
  (typep most-negative-short-float 'short-float)
  t)

(deftest most-negative-short-float-constant.2
  (< most-negative-short-float 0)
  t)

(deftest most-negative-short-float-constant.3
  (fpclassify most-negative-short-float)
  lisp-system:fp-normal -1)

(deftest most-negative-short-float-constant.4
  (values
    (typep most-negative-short-float 'single-float)
    (minusp most-negative-short-float)
    (zerop most-negative-short-float)
    (> 10s0 most-negative-short-float))
  t t nil t)


;;
;;  Constant Variable LEAST-NEGATIVE-SHORT-FLOAT
;;
(deftest least-negative-short-float-constant.1
  (typep least-negative-short-float 'short-float)
  t)

(deftest least-negative-short-float-constant.2
  (< least-negative-short-float 0)
  t)

(deftest least-negative-short-float-constant.3
  (fpclassify least-negative-short-float)
  lisp-system:fp-subnormal -1)

(deftest least-negative-short-float-constant.4
  (values
    (typep least-negative-short-float 'single-float)
    (minusp least-negative-short-float)
    (zerop least-negative-short-float))
  t t nil)


;;
;;  Constant Variable LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
;;
(deftest least-negative-normalized-short-float-constant.1
  (typep least-negative-normalized-short-float 'short-float)
  t)

(deftest least-negative-normalized-short-float-constant.2
  (< least-negative-normalized-short-float 0)
  t)

(deftest least-negative-normalized-short-float-constant.3
  (fpclassify least-negative-normalized-short-float)
  lisp-system:fp-normal -1)

(deftest least-negative-normalized-short-float-constant.4
  (values
    (typep least-negative-normalized-short-float 'single-float)
    (minusp least-negative-normalized-short-float)
    (zerop least-negative-normalized-short-float)
    (>= least-negative-short-float least-negative-normalized-short-float))
  t t nil t)


;;
;;  Constant Variable MOST-NEGATIVE-SINGLE-FLOAT
;;
(deftest most-negative-single-float-constant.1
  (typep most-negative-single-float 'single-float)
  t)

(deftest most-negative-single-float-constant.2
  (< most-negative-single-float 0)
  t)

(deftest most-negative-single-float-constant.3
  (fpclassify most-negative-single-float)
  lisp-system:fp-normal -1)

(deftest most-negative-single-float-constant.4
  (values
    (typep most-negative-single-float 'single-float)
    (minusp most-negative-single-float)
    (zerop most-negative-single-float)
    (> 10f0 most-negative-single-float))
  t t nil t)


;;
;;  Constant Variable LEAST-NEGATIVE-SINGLE-FLOAT
;;
(deftest least-negative-single-float-constant.1
  (typep least-negative-single-float 'single-float)
  t)

(deftest least-negative-single-float-constant.2
  (< least-negative-single-float 0)
  t)

(deftest least-negative-single-float-constant.3
  (fpclassify least-negative-single-float)
  lisp-system:fp-subnormal -1)

(deftest least-negative-single-float-constant.4
  (values
    (typep least-negative-single-float 'single-float)
    (minusp least-negative-single-float)
    (zerop least-negative-single-float))
  t t nil)


;;
;;  Constant Variable LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
;;
(deftest least-negative-normalized-single-float-constant.1
  (typep least-negative-normalized-single-float 'single-float)
  t)

(deftest least-negative-normalized-single-float-constant.2
  (< least-negative-normalized-single-float 0)
  t)

(deftest least-negative-normalized-single-float-constant.3
  (fpclassify least-negative-normalized-single-float)
  lisp-system:fp-normal -1)

(deftest least-negative-normalized-single-float-constant.4
  (values
    (typep least-negative-normalized-single-float 'single-float)
    (minusp least-negative-normalized-single-float)
    (zerop least-negative-normalized-single-float)
    (>= least-negative-single-float least-negative-normalized-single-float))
  t t nil t)


;;
;;  Constant Variable MOST-NEGATIVE-DOUBLE-FLOAT
;;
(deftest most-negative-double-float-constant.1
  (typep most-negative-double-float 'double-float)
  t)

(deftest most-negative-double-float-constant.2
  (< most-negative-double-float 0)
  t)

(deftest most-negative-double-float-constant.3
  (fpclassify most-negative-double-float)
  lisp-system:fp-normal -1)

(deftest most-negative-double-float-constant.4
  (values
    (typep most-negative-double-float 'double-float)
    (minusp most-negative-double-float)
    (zerop most-negative-double-float)
    (> 10d0 most-negative-double-float))
  t t nil t)


;;
;;  Constant Variable LEAST-NEGATIVE-DOUBLE-FLOAT
;;
(deftest least-negative-double-float-constant.1
  (typep least-negative-double-float 'double-float)
  t)

(deftest least-negative-double-float-constant.2
  (< least-negative-double-float 0)
  t)

(deftest least-negative-double-float-constant.3
  (fpclassify least-negative-double-float)
  lisp-system:fp-subnormal -1)

(deftest least-negative-double-float-cnostant.4
  (values
    (typep least-negative-double-float 'double-float)
    (minusp least-negative-double-float)
    (zerop least-negative-double-float))
  t t nil)


;;
;;  Constant Variable LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
;;
(deftest least-negative-normalized-double-float-constant.1
  (typep least-negative-normalized-double-float 'double-float)
  t)

(deftest least-negative-normalized-double-float-constant.2
  (< least-negative-normalized-double-float 0)
  t)

(deftest least-negative-normalized-double-float-constant.3
  (fpclassify least-negative-normalized-double-float)
  lisp-system:fp-normal -1)

(deftest least-negative-normalized-double-float-constant.4
  (values
    (typep least-negative-normalized-double-float 'double-float)
    (minusp least-negative-normalized-double-float)
    (zerop least-negative-normalized-double-float)
    (>= least-negative-double-float least-negative-normalized-double-float))
  t t nil t)


;;
;;  Constant Variable MOST-NEGATIVE-LONG-FLOAT
;;
(deftest most-negative-long-float-constant.1
  (typep most-negative-long-float 'long-float)
  t)

(deftest most-negative-long-float-constant.2
  (< most-negative-long-float 0)
  t)

(deftest most-negative-long-float-constant.3
  (fpclassify most-negative-long-float)
  lisp-system:fp-normal -1)

(deftest most-negative-long-float-constant.4
  (values
    (typep most-negative-long-float 'long-float)
    (minusp most-negative-long-float)
    (zerop most-negative-long-float)
    (> 10l0 most-negative-long-float))
  t t nil t)


;;
;;  Constant Variable LEAST-NEGATIVE-LONG-FLOAT
;;
(deftest least-negative-long-float-constant.1
  (typep least-negative-long-float 'long-float)
  t)

(deftest least-negative-long-float-constant.2
  (< least-negative-long-float 0)
  t)

(deftest least-negative-long-float-constant.3
  (fpclassify least-negative-long-float)
  lisp-system:fp-subnormal -1)

(deftest least-negative-long-float-cnostant.4
  (values
    (typep least-negative-long-float 'long-float)
    (minusp least-negative-long-float)
    (zerop least-negative-long-float))
  t t nil)


;;
;;  Constant Variable LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
;;
(deftest least-negative-normalized-long-float-constant.1
  (typep least-negative-normalized-long-float 'long-float)
  t)

(deftest least-negative-normalized-long-float-constant.2
  (< least-negative-normalized-long-float 0)
  t)

(deftest least-negative-normalized-long-float-constant.3
  (fpclassify least-negative-normalized-long-float)
  lisp-system:fp-normal -1)

(deftest least-negative-normalized-long-float-cnostant.4
  (values
    (typep least-negative-normalized-long-float 'long-float)
    (minusp least-negative-normalized-long-float)
    (zerop least-negative-normalized-long-float)
    (>= least-negative-long-float least-negative-normalized-long-float))
  t t nil t)


;;
;;  Constant Variable SHORT-FLOAT-EPSILON
;;
(deftest short-float-epsilon-constant.1
  (typep short-float-epsilon 'short-float)
  t)

(deftest short-float-epsilon-constant.2
  (< 0 short-float-epsilon)
  t)

(deftest short-float-epsilon-constant.3
  (= (float 1 short-float-epsilon)
     (+ (float 1 short-float-epsilon)
        short-float-epsilon))
  nil)

(deftest short-float-epsilon-constant.4
  (values
    (typep short-float-epsilon 'single-float)
    (plusp short-float-epsilon)
    (zerop short-float-epsilon))
  t t nil)


;;
;;  Constant Variable SHORT-FLOAT-NEGATIVE-EPSILON
;;
(deftest short-float-negative-epsilon-constant.1
  (typep short-float-negative-epsilon 'short-float)
  t)

(deftest short-float-negative-epsilon-constant.2
  (< 0 short-float-negative-epsilon)
  t)

(deftest short-float-negative-epsilon-constant.3
  (= (float 1 short-float-negative-epsilon)
     (- (float 1 short-float-negative-epsilon)
        short-float-negative-epsilon))
  nil)

(deftest short-float-negative-epsilon-constant.4
  (values
    (typep short-float-negative-epsilon 'single-float)
    (plusp short-float-negative-epsilon)
    (zerop short-float-negative-epsilon))
  t t nil)


;;
;;  Constant Variable SINGLE-FLOAT-EPSILON
;;
(deftest single-float-epsilon-constant.1
  (typep single-float-epsilon 'single-float)
  t)

(deftest single-float-epsilon-constant.2
  (< 0 single-float-epsilon)
  t)

(deftest single-float-epsilon-constant.3
  (= (float 1 single-float-epsilon)
     (+ (float 1 single-float-epsilon)
        single-float-epsilon))
  nil)

(deftest single-float-epsilon-constant.4
  (values
    (typep single-float-epsilon 'single-float)
    (plusp single-float-epsilon)
    (zerop single-float-epsilon))
  t t nil)


;;
;;  Constant Variable SINGLE-FLOAT-NEGATIVE-EPSILON
;;
(deftest single-float-negative-epsilon-constant.1
  (typep single-float-negative-epsilon 'single-float)
  t)

(deftest single-float-negative-epsilon-constant.2
  (< 0 single-float-negative-epsilon)
  t)

(deftest single-float-negative-epsilon-constant.3
  (= (float 1 single-float-negative-epsilon)
     (- (float 1 single-float-negative-epsilon)
        single-float-negative-epsilon))
  nil)

(deftest single-float-negative-epsilon-constant.4
  (values
    (typep single-float-negative-epsilon 'single-float)
    (plusp single-float-negative-epsilon)
    (zerop single-float-negative-epsilon))
  t t nil)


;;
;;  Constant Variable DOUBLE-FLOAT-EPSILON
;;
(deftest double-float-epsilon-constant.1
  (typep double-float-epsilon 'double-float)
  t)

(deftest double-float-epsilon-constant.2
  (< 0 double-float-epsilon)
  t)

(deftest double-float-epsilon-constant.3
  (= (float 1 double-float-epsilon)
     (+ (float 1 double-float-epsilon)
        double-float-epsilon))
  nil)

(deftest double-float-epsilon-constant.4
  (values
    (typep double-float-epsilon 'double-float)
    (plusp double-float-epsilon)
    (zerop double-float-epsilon))
  t t nil)


;;
;;  Constant Variable DOUBLE-FLOAT-NEGATIVE-EPSILON
;;
(deftest double-float-negative-epsilon-constant.1
  (typep double-float-negative-epsilon 'double-float)
  t)

(deftest double-float-negative-epsilon-constant.2
  (< 0 double-float-negative-epsilon)
  t)

(deftest double-float-negative-epsilon-constant.3
  (= (float 1 double-float-negative-epsilon)
     (- (float 1 double-float-negative-epsilon)
        double-float-negative-epsilon))
  nil)

(deftest double-float-negative-epsilon-constant.4
  (values
    (typep double-float-negative-epsilon 'double-float)
    (plusp double-float-negative-epsilon)
    (zerop double-float-negative-epsilon))
  t t nil)


;;
;;  Constant Variable LONG-FLOAT-EPSILON
;;
(deftest long-float-epsilon-constant.1
  (typep long-float-epsilon 'long-float)
  t)

(deftest long-float-epsilon-constant.2
  (< 0 long-float-epsilon)
  t)

(deftest long-float-epsilon-constant.3
  (= (float 1 long-float-epsilon)
     (+ (float 1 long-float-epsilon)
        long-float-epsilon))
  nil)

(deftest long-float-epsilon-constant.4
  (values
    (typep long-float-epsilon 'long-float)
    (plusp long-float-epsilon)
    (zerop long-float-epsilon))
  t t nil)


;;
;;  Constant Variable LONG-FLOAT-NEGATIVE-EPSILON
;;
(deftest long-float-negative-epsilon-constant.1
  (typep long-float-negative-epsilon 'long-float)
  t)

(deftest long-float-negative-epsilon-constant.2
  (< 0 long-float-negative-epsilon)
  t)

(deftest long-float-negative-epsilon-constant.3
  (= (float 1 long-float-negative-epsilon)
     (- (float 1 long-float-negative-epsilon)
        long-float-negative-epsilon))
  nil)

(deftest long-float-negative-epsilon-constant.4
  (values
    (typep long-float-negative-epsilon 'long-float)
    (plusp long-float-negative-epsilon)
    (zerop long-float-negative-epsilon))
  t t nil)


;;
;;  Constant Variable boole
;;
(deftest boole-1-constant.1
  (integerp boole-1)
  t)

(deftest boole-2-constant.1
  (integerp boole-2)
  t)

(deftest boole-and-constant.1
  (integerp boole-and)
  t)

(deftest boole-andc1-constant.1
  (integerp boole-andc1)
  t)

(deftest boole-andc2-constant.1
  (integerp boole-andc2)
  t)

(deftest boole-c1-constant.1
  (integerp boole-c1)
  t)

(deftest boole-c2-constant.1
  (integerp boole-c2)
  t)

(deftest boole-clr-constant.1
  (integerp boole-clr)
  t)

(deftest boole-eqv-constant.1
  (integerp boole-eqv)
  t)

(deftest boole-ior-constant.1
  (integerp boole-ior)
  t)

(deftest boole-nand-constant.1
  (integerp boole-nand)
  t)

(deftest boole-nor-constant.1
  (integerp boole-nor)
  t)

(deftest boole-orc1-constant.1
  (integerp boole-orc1)
  t)

(deftest boole-orc2-constant.1
  (integerp boole-orc2)
  t)

(deftest boole-set-constant.1
  (integerp boole-set)
  t)

(deftest boole-xor-constant.1
  (integerp boole-xor)
  t)

(deftest boole-constant.1
  (/= boole-1 boole-2 boole-and boole-andc1 boole-andc2
      boole-c1 boole-c2 boole-clr boole-eqv boole-ior
      boole-nand boole-nor boole-orc1 boole-orc2 boole-set boole-xor)
  t)

