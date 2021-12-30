;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function BOOLE
;;
(deftest boole.1
  (boole boole-ior #xAB00 #x00CD)
  #xABCD)

(deftest boole.2
  (boole boole-ior #x-ABCD #x-1234)
  #x-201)

(deftest-error! boole-error.1
  (eval '(boole boole-1 10)))

(deftest-error! boole-error.2
  (eval '(boole boole-1 10 "Hello"))
  type-error)

(deftest-error! boole-error.3
  (eval '(boole boole-1 10 20 30)))

(deftest boole-test.1
  (boole boole-ior 1 16)
  17)

(deftest boole-test.2
  (boole boole-and -2 5)
  4)

(deftest boole-test.3
  (boole boole-eqv 17 15)
  -31)

(deftest boole-test.4
  (let (list)
    (push (format nil "Results of (BOOLE <op> #b0011 #b0101) ...") list)
    (push (format nil "---Op-------Decimal-----Binary----Bits---") list)
    (dolist (symbol '(boole-1 boole-2     boole-and  boole-andc1
                              boole-andc2 boole-c1   boole-c2   boole-clr
                              boole-eqv   boole-ior  boole-nand boole-nor
                              boole-orc1  boole-orc2 boole-set  boole-xor))
      (let ((result (boole (symbol-value symbol) #b0011 #b0101)))
        (push (format nil " ~A~13T~3,' D~23T~:*~5,' B~31T ...~4,'0B"
                      symbol result (logand result #b1111))
              list)))
    (nreverse list))
  ("Results of (BOOLE <op> #b0011 #b0101) ..."
   "---Op-------Decimal-----Binary----Bits---"
   " BOOLE-1       3          11    ...0011"
   " BOOLE-2       5         101    ...0101"
   " BOOLE-AND     1           1    ...0001"
   " BOOLE-ANDC1   4         100    ...0100"
   " BOOLE-ANDC2   2          10    ...0010"
   " BOOLE-C1     -4        -100    ...1100"
   " BOOLE-C2     -6        -110    ...1010"
   " BOOLE-CLR     0           0    ...0000"
   " BOOLE-EQV    -7        -111    ...1001"
   " BOOLE-IOR     7         111    ...0111"
   " BOOLE-NAND   -2         -10    ...1110"
   " BOOLE-NOR    -8       -1000    ...1000"
   " BOOLE-ORC1   -3         -11    ...1101"
   " BOOLE-ORC2   -5        -101    ...1011"
   " BOOLE-SET    -1          -1    ...1111"
   " BOOLE-XOR     6         110    ...0110"))

(defconstant boole-n-vector
  (vector boole-clr   boole-and  boole-andc1 boole-2
          boole-andc2 boole-1    boole-xor   boole-ior
          boole-nor   boole-eqv  boole-c1    boole-orc1
          boole-c2    boole-orc2 boole-nand  boole-set))

(defun boole-n (n integer &rest more-integers)
  (apply #'boole (elt boole-n-vector n) integer more-integers))

(deftest boole-test.5
  (boole-n #b0111 5 3)
  7)

(deftest boole-test.6
  (boole-n #b0001 5 3)
  1)

(deftest boole-test.7
  (boole-n #b1101 5 3)
  -3)

(deftest boole-test.8
  (loop for n from #b0000 to #b1111 collect (boole-n n 5 3))
  (0 1 2 3 4 5 6 7 -8 -7 -6 -5 -4 -3 -2 -1))


;;  constant
(deftest boole-1.1
  (boole boole-1 10 20)
  10)

(deftest boole-2.1
  (boole boole-2 10 20)
  20)

(deftest boole-and.1
  (boole boole-and
         #x4F7B9C0C7FF0742E5537ED43B7C32C52E336FB26A59F74B0369
         #x1BB47CEE9383B4CBC129966F98C74040F29B369AB0C893A3F62)
  #xB301C0C1380340A4121844390C30040E2123202A08810A0360)

(deftest boole-andc1.1
  (boole boole-andc1
         #x-393B346AD21E3DDA071EC4
         #x8A72E1DA8DBB4F44CE8D2A)
  #x832204A801A0D40060C02)

(deftest boole-andc2.1
  (boole boole-andc2
         #x-52FDFE24E59289EB088757
         #x348892E6AC6F7EA137B26A)
  #x-76FDFEE6EDFFFFEB3FB77F)

(deftest boole-c1.1
  (boole boole-c1
         #xBE6BBA61618AB915487855
         10)
  #x-BE6BBA61618AB915487856)

(deftest boole-c2.1
  (boole boole-c2
         20
         #x-D7F0FD5CB20EBC851D0764)
  #xD7F0FD5CB20EBC851D0763)

(deftest boole-clr.1
  (boole boole-clr 10 20)
  0)

(deftest boole-eqv.1
  (boole boole-eqv
         #x71C657D7522BBCA4301E44
         #xC251F31E815F2A86687E41)
  #x-B397A4C9D3749622586006)

(deftest boole-ior.1
  (boole boole-ior
         #x9D30D9178DF5F587B4A862
         #x-FE18764CA04F8ADEC8B296)
  #x-62082648200A0A58481296)

(deftest boole-nand.1
  (boole boole-nand
         #x-AA1355ACD3789E83205AE5
         #xDA43D15987AEBE4C7541C9)
  #x-504080510486204C55010A)


(deftest boole-nor.1
  (boole boole-nor
         #x-792A36F3B26AD04EEC191
         #x-1500DCF419194303258C5C)
  #x500806419000100248010)

(deftest boole-orc1.1
  (boole boole-orc1
         #x7C9A58ED632C20C71EB363
         #x52AABCA40F896CA4AA12A8)
  #x-2C1040496024004314A144)


(deftest boole-orc2.1
  (boole boole-orc2
         #x701EB39515992A5ED7B67F
         #x-B741C9877630D6CCA3A6CD)
  #xF75FFB9777B9FEDEF7B6FF)

(deftest boole-set.1
  (boole boole-set 10 20)
  -1)

(deftest boole-xor.1
  (boole boole-xor
         #x-59BD91DD857ED4CB3565B7
         #x-2B419DC0AC809248E00131)
  #x72FC0C1D29FE4683D56486)


;;
;;  Function LOGBITP
;;
(deftest logbitp.1
  (logbitp 1 1)
  nil)

(deftest logbitp.2
  (logbitp 0 1)
  t)

(deftest logbitp.3
  (logbitp 3 10)
  t)

(deftest logbitp.4
  (logbitp 1000000 -1)
  t)

(deftest logbitp.5
  (logbitp 2 6)
  t)

(deftest logbitp.6
  (logbitp 0 6)
  nil)

(deftest logbitp-fixnum.1
  (logbitp 1000000 1)
  nil)

(deftest logbitp-fixnum.2
  (let ((c #b1100000011111100000))
    (values
      (logbitp 0 c)
      (logbitp 9 c)
      (logbitp 10 c)
      (logbitp 11 c)))
  nil t t nil)

(deftest logbitp-fixnum.3
  (let ((c (- #b1100000011111100000)))
    (values
      (logbitp 0 c)
      (logbitp 1 c)
      (logbitp 4 c)
      (logbitp 5 c)
      (logbitp 10 c)
      (logbitp 11 c)))
  nil nil nil t nil t)

(deftest logbitp-bignum.1
  (let ((c #xFF000AA0000000000000000000000010000))
    (values
      (logbitp 0 c)
      (logbitp 16 c)
      (logbitp (+ (* 29 4) 0) c)
      (logbitp (+ (* 29 4) 1) c)
      (logbitp (+ (* 29 4) 2) c)
      (logbitp (+ (* 29 4) 3) c)
      (logbitp (+ (* 29 4) 4) c)
      (logbitp (+ (* 29 4) 5) c)))
  nil t nil t nil t nil nil)

(deftest logbitp-bignum.2
  (let ((c (- #xFF000AA0000000000000000000000010000)))
    (values
      (logbitp 0 c)
      (logbitp 16 c)
      (logbitp (+ (* 29 4) 0) c)
      (logbitp (+ (* 29 4) 1) c)
      (logbitp (+ (* 29 4) 2) c)
      (logbitp (+ (* 29 4) 3) c)
      (logbitp (+ (* 29 4) 4) c)
      (logbitp (+ (* 29 4) 5) c)))
  nil t t nil t nil t t)

(deftest logbitp-bignum.3
  (let ((c (- #xFF000AA0000000000000000000000000000)))
    (values
      (logbitp 0 c)
      (logbitp 16 c)
      (logbitp (+ (* 29 4) 0) c)
      (logbitp (+ (* 29 4) 1) c)
      (logbitp (+ (* 29 4) 2) c)
      (logbitp (+ (* 29 4) 3) c)
      (logbitp (+ (* 29 4) 4) c)
      (logbitp (+ (* 29 4) 5) c)))
  nil nil t nil t nil t t)

(deftest logbitp-bignum.4
  (let ((c (- #xFF000AAFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
    (values
      (logbitp 0 c)
      (logbitp 16 c)
      (logbitp (+ (* 29 4) 0) c)
      (logbitp (+ (* 29 4) 1) c)
      (logbitp (+ (* 29 4) 2) c)
      (logbitp (+ (* 29 4) 3) c)
      (logbitp (+ (* 29 4) 4) c)
      (logbitp (+ (* 29 4) 5) c)))
  t nil t nil t nil t t)

(deftest-error! logbitp-error.1
  (eval '(logbitp 10)))

(deftest-error! logbitp-error.2
  (eval '(logbitp -1 10))
  type-error)

(deftest-error! logbitp-error.3
  (eval '(logbitp 10 20 30)))


;;
;;  Function LOGCOUNT
;;
(deftest logcount.1
  (logcount #xABCD0011)
  12)

(deftest logcount.2
  (logcount #x-ABCD0011)
  11)

(deftest logcount.3
  (logcount #xFAB901DA6C9F81DD3C7273DBC22B5EE9806729)
  80)

(deftest logcount.4
  (logcount #x-FAB901DA6C9F81DD3C7273DBC22B5EE9806729)
  79)

(deftest-error! logcount-error.1
  (eval '(logcount)))

(deftest-error! logcount-error.2
  (eval '(logcount 3/4))
  type-error)

(deftest-error! logcount-error.3
  (eval '(logcount 10 20)))

;;  ANSI Common Lisp
(deftest logcount-test.1
  (logcount 0)
  0)

(deftest logcount-test.2
  (logcount -1)
  0)

(deftest logcount-test.3
  (logcount 7)
  3)

(deftest logcount-test.4
  (logcount  13)
  3)

(deftest logcount-test.5
  (logcount -13)
  2)

(deftest logcount-test.6
  (logcount  30)
  4)

(deftest logcount-test.7
  (logcount -30)
  4)

(deftest logcount-test.8
  (logcount (expt 2 100))
  1)

(deftest logcount-test.9
  (logcount (- (expt 2 100)))
  100)

(deftest logcount-test.10
  (logcount (- (1+ (expt 2 100))))
  1)

(deftest logcount-test.11
  (flet ((a (x) (logcount (- (+ x 1))))
         (b (x) (logcount (lognot x))))
    (values
      (= (logcount 10) (a 10) (b 10))
      (= (logcount -20) (a -20) (b -20))
      (= (logcount 3000) (a 3000) (b 3000))))
  t t t)


;;
;;  Function LOGTEST
;;
(deftest logtest.1
  (logtest 0 0)
  nil)

(deftest logtest.2
  (logtest 100 100)
  t)

(deftest logtest.3
  (logtest 0 -1)
  nil)

(deftest logtest.4
  (logtest 100 200)
  t)

(deftest logtest.5
  (logtest
    #x1110000000000000000000000000000000000000000000111
    #x0001000000000000000000000000000000000000000000000)
  nil)

(deftest logtest.6
  (logtest
    #x1110000000000000000000000000000000000000000000111
    #x000100000000000000000000000000000000000000000000F)
  t)

(deftest logtest.7
  (logtest
    #x1110000000000000000000000000000000000000000000111
    #x0F01000000000000000000000000000000000000000000000)
  t)

(deftest logtest.8
  (logtest
    #x1110000000000000000000000000000000000000000000111
    #x-0F01000000000000000000000000000000000000000000000)
  t)

(deftest-error! logtest-error.1
  (eval '(logtest 10)))

(deftest-error! logtest-error.2
  (eval '(logtest 10 3/4))
  type-error)

(deftest-error! logtest-error.3
  (eval '(logtest 10 20 30)))

;;  ANSI Common Lisp
(deftest logtest-test.1
  (logtest 1 7)
  t)

(deftest logtest-test.2
  (logtest 1 2)
  nil)

(deftest logtest-test.3
  (logtest -2 -1)
  t)

(deftest logtest-test.4
  (logtest 0 -1)
  nil)

