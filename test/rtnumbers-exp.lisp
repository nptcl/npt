;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;
;;  Function EXP
;;
(deftest-single exp.1
  (exp 0)
  1.0)

(deftest-float exp.2
  (exp 1)
  2.7182817)

(deftest-single exp.3
  (exp 2)
  7.389056)

(deftest-single exp.4
  (exp (make-bignum 2))
  7.389056)

(deftest-single exp.5
  (exp 3/4)
  2.117)

(deftest-single exp.6
  (exp 1.2f-1)
  1.1274968)

(deftest-double exp.7
  (exp -4.5d0)
  0.011108996538242306d0)

(deftest-long exp.8
  (exp 6.7l-1)
  1.9542373206359396d0)

(deftest-single exp.9
  (exp #c(1.2 3.4))
  -3.209883 -0.8484267)

(deftest-double exp.10
  (exp #c(5.6d-1 7.8d0))
  0.09445827102094172d0 1.748122375100993d0)

(deftest-single exp.11
  (exp #c(1 2))
  -1.1312044 2.4717267)

(deftest-double exp.12
  (exp #c(2 3.4d0))
  -7.143726081314396d0 -1.8882075384588168d0)

(deftest-error! exp-error.1
  (eval '(exp)))

(deftest-error! exp-error.2
  (eval '(exp 'hello))
  type-error)

(deftest-error! exp-error.3
  (eval '(exp 10 20)))

;;  ANSI Common Lisp
(deftest exp-test.1
  (exp 0)
  1.0)

(deftest-float exp-test.2
  (exp 1)
  2.718282)

(deftest exp-test.3
  (exp (log 5))
  5.0)


;;
;;  Function EXPT
;;
(deftest expt.1
  (expt 0 0)
  1)

(deftest expt.2
  (expt 10 0)
  1)

(deftest expt.3
  (expt 0 10)
  0)

(deftest expt.4
  (expt -10 0)
  1)

(deftest-error expt.5
  (expt 0 -10)
  division-by-zero)

(deftest expt-fixnum.1
  (expt 10 1)
  10)

(deftest expt-fixnum.2
  (expt 10 2)
  100)

(deftest expt-fixnum.3
  (expt 10 3)
  1000)

(deftest expt-fixnum.4
  (expt 10 4)
  10000)

(deftest expt-fixnum.5
  (expt 10 100)
  10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(deftest expt-fixnum.6
  (expt -2 8)
  256)

(deftest expt-fixnum.7
  (expt -2 9)
  -512)

(deftest expt-fixnum.8
  (expt 2 -8)
  1/256)

(deftest expt-fixnum.9
  (expt -2 -7)
  -1/128)

(deftest expt-fixnum.10
  (expt -2 -8)
  1/256)

(deftest expt-fixnum.11
  (expt 1234 123)
  170515806212727042875059727620626282654302313111068290470529619322183913834868007471366306717060598572641592314554345900570589670671499709086102539904846514793135617305563669993950104622035682027355757755070083238444147778396026387067042685700404003287042480639680696865587865016699383883388831980459159942845372414601809429717726107628595243406801014418529766279838067203562799104)

(deftest expt-fixnum.12
  (expt 7 -1234)
  1/70954734215028011240104595141498929070420289200365254864883100224459061531095964535961424752183918983389834714345883085722594630104327570484382813454950959834721299254363358098967399048133489131401375954827777487240689332659684949614116231197346793510695402301156840458057925504143947679213040737846060014251249690928324441226158708348216074516368113070768675494487327714367821662938661080440834970823376650216081882744143161592962823478851621834726159007802788609981541818975600270503709811145869971874252832721423453899821462438727851069669041332053765106227148570291872998008674128627485981069824533343176007316751497280494057995201432198045575691194828311578763776024467116028227768366827683004790939265568398432923069244698164607404132404210543306184313774389935234011995625090611727226388755028092996618090667295922698497975515840710550500258575437750076719351206138149202280453034232757583405474400100672191700332560347912741669538744924043142095605796598736664375513698661239721232174697654768024941266796370339939780641017369634940849)

(deftest expt-fixnum.13
  (expt 2 (make-bignum 8))
  256)

(deftest-float expt-fixnum.14
  (expt 2 3/4)
  1.6817926)

(deftest-float expt-fixnum.15
  (expt -2 3/4)
  -1.189207 1.1892072)

(deftest-float expt-fixnum.16
  (expt 3 -1.2f0)
  0.2675805)

(deftest-double expt-fixnum.17
  (expt -3 2.3d0)
  7.355252243329742d0 10.123636208224076d0)

(deftest-long expt-fixnum.18
  (expt -3 -1.9l0)
  0.11794402418366383989L0 0.038322336510242612396L0)

(deftest-single expt-fixnum.19
  (expt 3 #c(2 -3/4))
  6.1138897 -6.6045704)

(deftest-double expt-fixnum.20
  (expt 3 #c(1.2d0 3.4f0))
  -3.0976936660373564d0 -2.0906707336714394d0)

(deftest-single expt-fixnum.21
  (expt 3 #c(-2 3))
  -0.109791994 -0.01707037)

(deftest expt-bignum.1
  (expt (make-bignum 10) 1)
  10)

(deftest expt-bignum.2
  (expt (make-bignum 10) 2)
  100)

(deftest expt-bignum.3
  (expt (make-bignum 10) 3)
  1000)

(deftest expt-bignum.4
  (expt (make-bignum 10) 4)
  10000)

(deftest expt-bignum.5
  (expt (make-bignum 10) 100)
  10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(deftest expt-bignum.6
  (expt (make-bignum -2) 8)
  256)

(deftest expt-bignum.7
  (expt (make-bignum -2) 9)
  -512)

(deftest expt-bignum.8
  (expt (make-bignum 2) -8)
  1/256)

(deftest expt-bignum.9
  (expt (make-bignum -2) -7)
  -1/128)

(deftest expt-bignum.10
  (expt (make-bignum -2) -8)
  1/256)

(deftest expt-bignum.11
  (expt (make-bignum 9) 55)
  30432527221704537086371993251530170531786747066637049)

(deftest expt-bignum.12
  (expt (make-bignum 7) -88)
  1/233683216210633558353880137011125430143959282107856711392134007594290612801)

(deftest expt-bignum.13
  (expt (make-bignum 2) (make-bignum 8))
  256)

(deftest-float expt-bignum.14
  (expt (make-bignum 2) 3/4)
  1.6817926)

(deftest-float expt-bignum.15
  (expt (make-bignum -2) 3/4)
  -1.189207 1.1892072)

(deftest-float expt-bignum.16
  (expt (make-bignum 3) -1.2f0)
  0.2675805)

(deftest-double expt-bignum.17
  (expt (make-bignum -3) 2.3d0)
  7.355252243329742d0 10.123636208224076d0)

(deftest-long expt-bignum.18
  (expt (make-bignum -3) -1.9l0)
  0.11794402418366383989L0 0.038322336510242612396L0)

(deftest-single expt-bignum.19
  (expt (make-bignum 3) #c(2 -3/4))
  6.1138897 -6.6045704)

(deftest-double expt-bignum.20
  (expt (make-bignum 3) #c(1.2d0 3.4f0))
  -3.0976936660373564d0 -2.0906707336714394d0)

(deftest-single expt-bignum.21
  (expt (make-bignum 3) #c(-2 3))
  -0.109791994 -0.01707037)

(deftest expt-ratio.1
  (expt 2/3 8)
  256/6561)

(deftest expt-ratio.2
  (expt -2/3 (make-bignum -9))
  -19683/512)

(deftest-single expt-ratio.3
  (expt 2/3 5/7)
  0.7485495)

(deftest-single expt-ratio.4
  (expt -2/3 4.5f-2)
  0.97212356 0.13835368)

(deftest-double expt-ratio.5
  (expt -2/3 1.2d0)
  -0.4973339806908717d0 -0.3613342875941152d0)

(deftest-long expt-ratio.6
  (expt 2/3 -1.2L0)
  1.6267076567965479206L0)

(deftest-double expt-ratio.7
  (expt 2/3 #c(1.2f0 -3.4d1))
  0.21157379179848101d0 0.5771828754841491d0)

(deftest-single expt-ratio.8
  (expt 2/3 #c(1 2))
  0.45921627 -0.4832855)

(deftest-single expt-single.1
  (expt 2.3f0 -2)
  0.18903592)

(deftest-single expt-single.2
  (expt 2.3f0 (make-bignum -2))
  0.18903592)

(deftest-single expt-single.3
  (expt 2.3f0 -2/3)
  0.5739157)

(deftest-single expt-single.4
  (expt -2.3f0 1.2f0)
  -2.198017 -1.5969532)

(deftest-double expt-single.5
  (expt -2.3f0 -1.2d0)
  -0.297772270330278d0 0.21634421805565585d0)

(deftest-long expt-single.6
  (expt 2.3f0 -2.1L0)
  0.17392882509562813d0)

(deftest-double expt-single.7
  (expt 2.3f0 #c(1.2f0 -3.1d0))
  -2.302520072651371d0 -1.442199232632068d0)

(deftest-single expt-single.8
  (expt -2.3f0 #c(7 3))
  0.021991925 -0.016472045)

(deftest-double expt-double.1
  (expt -2.3d0 -2)
  0.18903591682419663d0 0.0d0)

(deftest-double expt-double.2
  (expt -2.3d0 (make-bignum -2))
  0.18903591682419663d0 0.0d0)

(deftest-double expt-double.3
  (expt 2.3d0 -2/3)
  0.5739157051286576d0)

(deftest-double expt-double.4
  (expt -2.3d0 1.2f0)
  -2.198016851951299d0 -1.5969532232935613d0)

(deftest-double expt-double.5
  (expt -2.3d0 -1.2d0)
  -0.29777226292216236d0 0.2163442126733448d0)

(deftest-long expt-double.6
  (expt 2.3d0 -2.1L0)
  0.1739288175232358d0)

(deftest-double expt-double.7
  (expt 2.3d0 #c(1.2f0 -3.1d0))
  -2.302520222623744d0 -1.4421991205302758d0)

(deftest-double expt-double.8
  (expt -2.3d0 #c(7 3))
  0.021991899471513303d0 -0.016472086611901793d0)

(deftest-long expt-long.1
  (expt -2.3L0 -2)
  0.18903591682419663L0 0.0L0)

(deftest-long expt-long.2
  (expt -2.3L0 (make-bignum -2))
  0.18903591682419663L0 0.0L0)

(deftest-long expt-long.3
  (expt 2.3L0 -2/3)
  0.5739157051286576L0)

(deftest-long expt-long.4
  (expt -2.3L0 1.2f0)
  -2.198016851951299L0 -1.5969532232935613L0)

(deftest-long expt-long.5
  (expt -2.3L0 -1.2L0)
  -0.29777226292216236L0 0.2163442126733448L0)

(deftest-long expt-long.6
  (expt 2.3L0 -2.1L0)
  0.1739288175232358L0)

(deftest-long expt-long.7
  (expt 2.3L0 #c(1.2f0 -3.1L0))
  -2.302520222623744L0 -1.4421991205302758L0)

(deftest-long expt-long.8
  (expt -2.3L0 #c(7 3))
  0.021991899471513303L0 -0.016472086611901793L0)

(deftest expt-complex.1
  (expt #c(10 -3/4) 3)
  #c(7865/8 -14373/64))

(deftest expt-complex.2
  (expt #c(10 -3/4) (make-bignum -3))
  #c(4026880/4165509529 919872/4165509529))

(deftest-single expt-complex.3
  (expt #c(10 -3/4) -6/7)
  0.13833067 0.00888827)

(deftest-single expt-complex.4
  (expt #c(2 -3/4) 1.2f0)
  2.2592518 -1.0375758)

(deftest-double expt-complex.5
  (expt #c(10 -3/4) -1.3d0)
  0.04970004219351855d0 0.0048520259958496435d0)

(deftest-long expt-complex.6
  (expt #c(-3 3/4) -1.3L0)
  -0.18704277584270710282L0 0.13466582318277132247L0)

(deftest-double expt-complex.7
  (expt #c(-1.2f0 3.4d0) -3)
  0.01815566699784148d0 0.011204368767686747d0)

(deftest-double expt-complex.8
  (expt #c(-1.2f0 3.4d0) (make-bignum -3))
  0.01815566699784148d0 0.011204368767686747d0)

(deftest-double expt-complex.9
  (expt #c(-1.2f0 3.4d0) -6/7)
  -0.02211023365124718d0 -0.3323815806407974d0)

(deftest-double expt-complex.10
  (expt #c(-1.2f0 3.4d0) 1.9d0)
  -10.10272275066296d0 -5.357261568911895d0)

(deftest-long expt-complex.11
  (expt #c(-1.2f0 3.4d0) 1.9L0)
  -10.10272275066296d0 -5.357261568911895d0)

(deftest-single expt-complex.12
  (expt #c(1 2) #c(-3 -4))
  7.250044 -1.906459)

(deftest-double expt-complex.13
  (expt #c(1.2d0 2) #c(-3 -4))
  4.766465047058875d0 -0.9439163516910051d0)

(deftest-long expt-complex.14
  (expt #c(1 2) #c(-3.9L0 -4))
  1.1333987462203081d0 -3.4521857102724227d0)

(deftest-single expt-complex.15
  (expt #c(1.2 3.4) #c(5.6 7.8))
  -0.032776006 -0.08229103)

(deftest-error! expt-error.1
  (eval '(expt)))

(deftest-error! expt-error.2
  (eval '(expt 'aaa)))

(deftest-error! expt-error.3
  (eval '(expt 10 20 30)))

;; ANSI Common Lisp
(deftest expt-test.1
  (expt 10 0)
  1)

(deftest expt-test.2
  (expt 10.0 0)
  1.0)

(deftest expt-test.3
  (expt 10.0L0 0)
  1.0L0)

(deftest expt-test.4
  (expt 2 8)
  256)

(deftest expt-test.5
  (expt 4 .5)
  2.0)

(deftest expt-test.6
  (expt #c(0 1) 2)
  -1)

(deftest expt-test.7
  (expt #c(2 2) 3)
  #c(-16 16))

(deftest expt-test.8
  (expt #c(2 2) 4)
  -64)

(deftest-float expt-test.9
  (let ((x (exp (/ (* 2 pi #c(0 1)) 3))))  ;; exp(2.pi.i/3)
    (expt x 3))
  1.0 0.0)

(deftest-float expt-test.10
  (let ((x (exp (/ (* 2 pi #c(0 1)) 3))))  ;; exp(2.pi.i/3)
    (sqrt (expt x 3)))
  1.0 0.0)

(deftest-float expt-test.11
  (let ((x (exp (/ (* 2 pi #c(0 1)) 3))))  ;; exp(2.pi.i/3)
    (expt x 3/2))
  -1.0 0.0)


;;
;;  Function LOG
;;

;;  log e
(deftest-single log-exp.1
  (log 1)
  0.0)

(deftest-error log-exp.2
  (log 0)
  arithmetic-error)

(deftest-single log-exp.3
  (log 2)
  0.6931472)

(deftest-single log-exp.4
  (log (make-bignum -3))
  1.0986123 3.1415927)

(deftest-single log-exp.5
  (log 4/5)
  -0.22314355)

(deftest-single log-exp.6
  (log -2.3f0)
  0.8329091 3.1415927)

(deftest-double log-exp.7
  (log 4d100)
  231.64480366052445d0)

(deftest-long log-exp.8
  (log -9.8l-2)
  -2.3227878003115651324L0 3.1415926535897932385L0)

(deftest-single log-exp.9
  (log #c(1 2))
  0.804719 1.1071488)

(deftest-double log-exp.10
  (log #c(1.2f0 3.4d0))
  1.2824746831323421d0 1.2315036998697262d0)

(deftest-long log-exp.11
  (log #c(1.2f0 3.4L0))
  1.2824746831323421d0 1.2315036998697262d0)

;;  log
(deftest-error log-fixnum.1
  (log 0 0)
  arithmetic-error)

(deftest-error log-fixnum.2
  (log 0 10)
  arithmetic-error)

(deftest-single log-fixnum.3
  (log 10 0)
  0.0)

(deftest-single log-fixnum.4
  (log 10 20)
  0.7686218)

(deftest-single log-fixnum.5
  (log 10 (make-bignum 20))
  0.7686218)

(deftest-single log-fixnum.6
  (log 10 -3/4)
  -0.066558294 -0.72684073)

(deftest-single log-fixnum.7
  (log 10 2.3f0)
  2.7645094)

(deftest-double log-fixnum.8
  (log -10 2.3d0)
  2.7645093919489354d0 3.7718312443488147d0)

(deftest-long log-fixnum.9
  (log -10 -2.3L0)
  1.115882504066917d0 -0.43708924951297623d0)

(deftest-single log-fixnum.10
  (log 10 #c(2 -3))
  1.1311495 0.86682934)

(deftest-double log-fixnum.11
  (log 10 #c(1.2d0 3.4))
  0.9340990086173372D0 -0.8969739643862817D0 1e-6)

(deftest-double log-fixnum.12
  (log 10 #c(1.2d0 3.4d0))
  0.9340990086173372D0 -0.8969739643862817D0)

(deftest-error log-bignum.1
  (log (make-bignum 0) 0)
  arithmetic-error)

(deftest-error log-bignum.2
  (log (make-bignum 0) 10)
  arithmetic-error)

(deftest-single log-bignum.3
  (log (make-bignum 10) 0)
  0.0)

(deftest-single log-bignum.4
  (log (make-bignum 10) 20)
  0.7686218)

(deftest-single log-bignum.5
  (log (make-bignum 10) (make-bignum 20))
  0.7686218)

(deftest-single log-bignum.6
  (log (make-bignum 10) -3/4)
  -0.066558294 -0.72684073)

(deftest-single log-bignum.7
  (log (make-bignum 10) 2.3f0)
  2.7645094)

(deftest-double log-bignum.8
  (log (make-bignum -10) 2.3d0)
  2.7645093919489354d0 3.7718312443488147d0)

(deftest-long log-bignum.9
  (log (make-bignum -10) -2.3L0)
  1.115882504066917d0 -0.43708924951297623d0)

(deftest-single log-bignum.10
  (log (make-bignum 10) #c(2 -3))
  1.1311495 0.86682934)

(deftest-double log-bignum.11
  (log (make-bignum 10) #c(1.2d0 3.4))
  0.9340990086173372D0 -0.8969739643862817D0 1e-6)

(deftest-double log-bignum.12
  (log (make-bignum 10) #c(1.2d0 3.4d0))
  0.9340990086173372D0 -0.8969739643862817D0)

(deftest-error log-ratio.1
  (log (make-ratio 0 1) 0)
  arithmetic-error)

(deftest-error log-ratio.2
  (log (make-ratio 0 1) 10)
  arithmetic-error)

(deftest-single log-ratio.3
  (log 10/11 0)
  0.0)

(deftest-single log-ratio.4
  (log 10/11 20)
  -0.0318153)

(deftest-single log-ratio.5
  (log 10/11 (make-bignum 20))
  -0.0318153)

(deftest-single log-ratio.6
  (log 11/6 -3/4)
  -0.0175209 -0.19133459)

(deftest-single log-ratio.7
  (log 5/6 2.3f0)
  -0.2188973)

(deftest-double log-ratio.8
  (log -5/6 2.3d0)
  -0.21889729836487837D0 3.7718312443488147D0)

(deftest-long log-ratio.9
  (log -5/6 -2.3L0)
  0.9199500598980037D0 0.30193486518496077D0)

(deftest-single log-ratio.10
  (log 5/6 #c(2 -3))
  -0.08956583 -0.068636626)

(deftest-double log-ratio.11
  (log 5/6 #c(1.2d0 3.4))
  -0.07396312286858601D0 0.07102351475567023D0)

(deftest-double log-ratio.12
  (log 5/6 #c(1.2d0 3.4d0))
  -0.07396312343417179D0 0.07102351617238363D0)

(deftest-error log-single.1
  (log 0.0f0 0)
  arithmetic-error)

(deftest-error log-single.2
  (log 0.0f0 10)
  arithmetic-error)

(deftest-single log-single.3
  (log 1.2f0 0)
  0.0)

(deftest-single log-single.4
  (log 1.2f0 20)
  0.060860444)

(deftest-single log-single.5
  (log 1.2f0 (make-bignum 20))
  0.060860444)

(deftest-single log-single.6
  (log 9.2f0 -3/4)
  -0.06414807 -0.7005202)

(deftest-single log-single.7
  (log 1.2f0 2.3f0)
  0.21889734)

(deftest-double log-single.8
  (log -1.6f0 2.3d0)
  0.5642916270272584D0 3.7718312443488147D0)

(deftest-long log-single.9
  (log -1.2f0 -2.3L0)
  0.9487018137260168D0 0.19348810176663522D0)

(deftest-single log-single.10
  (log 1.2f0 #c(2 -3))
  0.08956584 0.06863664)

(deftest-double log-single.11
  (log 1.2f0 #c(1.2d0 3.4))
  0.0739631389886238D0 -0.07102353023502937D0)

(deftest-double log-single.12
  (log 1.2f0 #c(1.2d0 3.4d0))
  0.0739631395542097D0 -0.07102353165174308D0)

(deftest-error log-double.1
  (log 0.0d0 0)
  arithmetic-error)

(deftest-error log-double.2
  (log 0.0d0 10)
  arithmetic-error)

(deftest-double log-double.3
  (log 1.2d0 0)
  0.0)

(deftest-double log-double.4
  (log 1.2d0 20)
  0.06086043082136214d0)

(deftest-double log-double.5
  (log 1.2d0 (make-bignum 20))
  0.06086043082136214d0)

(deftest-double log-double.6
  (log 9.2d0 -3/4)
  -0.06414807268141035D0 -0.7005202380542545D0)

(deftest-double log-double.7
  (log 1.2d0 2.3f0)
  0.2188973038134799D0)

(deftest-double log-double.8
  (log -1.6d0 2.3d0)
  0.5642916091367579D0 3.7718312443488147D0)

(deftest-long log-double.9
  (log -1.2d0 -2.3L0)
  0.9487018105928385D0 0.19348811358445517D0)

(deftest-double log-double.10
  (log 1.2d0 #c(2 -3))
  0.0895658309892637D0 0.06863662727500809D0)

(deftest-double log-double.11
  (log 1.2d0 #c(1.2d0 3.4))
  0.07396312286858601D0 -0.07102351475567023D0)

(deftest-double log-double.12
  (log 1.2d0 #c(1.2d0 3.4d0))
  0.07396312343417177D0 -0.07102351617238363D0)

(deftest-error log-long.1
  (log 0.0L0 0)
  arithmetic-error)

(deftest-error log-long.2
  (log 0.0L0 10)
  arithmetic-error)

(deftest-long log-long.3
  (log 1.2L0 0)
  0.0)

(deftest-long log-long.4
  (log 1.2L0 20)
  0.06086043082136214d0)

(deftest-long log-long.5
  (log 1.2L0 (make-bignum 20))
  0.06086043082136214d0)

(deftest-long log-long.6
  (log 9.2L0 -3/4)
  -0.06414807268141035D0 -0.7005202380542545D0)

(deftest-long log-long.7
  (log 1.2L0 2.3f0)
  0.2188973038134799D0)

(deftest-long log-long.8
  (log -1.6L0 2.3d0)
  0.5642916091367579D0 3.7718312443488147D0)

(deftest-long log-long.9
  (log -1.2L0 -2.3L0)
  0.9487018105928385D0 0.19348811358445517D0)

(deftest-long log-long.10
  (log 1.2L0 #c(2 -3))
  0.0895658309892637D0 0.06863662727500809D0)

(deftest-long log-long.11
  (log 1.2L0 #c(1.2d0 3.4))
  0.07396312286858601D0 -0.07102351475567023D0)

(deftest-long log-long.12
  (log 1.2L0 #c(1.2L0 3.4L0))
  0.07396312343417177D0 -0.07102351617238363D0)

(deftest-single log-complex.1
  (log #c(1.2 3.4) 0)
  0.0)

(deftest-single log-complex.2
  (log #c(1.2 3.4) 20)
  0.4281006 0.41108602)

(deftest-double log-complex.3
  (log #c(1.2d0 3.4d0) (make-bignum 20))
  0.42810056494444443D0 0.4110860383661241D0)

(deftest-single log-complex.4
  (log #c(1 2) -3/4)
  0.32622465 -0.28602305)

(deftest-single log-complex.5
  (log #c(1.2f0 3.4f0) 2.3f0)
  1.5397536 1.4785571)

(deftest-double log-complex.6
  (log #c(1 2) 2.3d0)
  0.9661545708387561D0 1.3292551219664739D0)

(deftest-long log-complex.7
  (log #c(-1 2.0f0) -2.3L0)
  0.6685054670590511D0 -0.0789136852889855D0)

(deftest-double log-complex.8
  (log #c(1.2d0 3.4) #c(2 -3))
  0.1664073694561802D0 1.0877780766231384D0)

(deftest-long log-complex.9
  (log #c(1 2) #c(1.2L0 3.4))
  0.7577442994547355D0 0.1356625536875226D0)

(deftest-error! log-error.1
  (eval '(log)))

(deftest-error! log-error.2
  (eval '(log 'aaa))
  type-error)

(deftest-error! log-error.3
  (eval '(log 10 20 30)))

;;  ANSI Common Lisp
(deftest log-test.1
  (eql (log -1.0) (complex 0.0 (float pi 0.0)))
  t)

(deftest log-test.2
  (log 8 2)
  3.0)

(deftest-float log-test.3
  (log 100 10)
  2.0)

(deftest-float log-test.4
  (log 100.0 10)
  2.0)

(deftest log-test.5
  (log #c(0 1) #c(0 -1))
  -1.0)

(deftest log-test.6
  (log 8.0 2)
  3.0)

(deftest-float log-test.7
  (log #c(-16 16) #c(2 2))
  3.0 0.0)

