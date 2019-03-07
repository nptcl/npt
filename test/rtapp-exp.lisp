;;
;;  ANSI COMMON LISP: 12. Numbers
;;

;;  exp
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


;;  expt
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

(deftest exptf.1
  (expt 10 1)
  10)

(deftest exptf.2
  (expt 10 2)
  100)

(deftest exptf.3
  (expt 10 3)
  1000)

(deftest exptf.4
  (expt 10 4)
  10000)

(deftest exptf.5
  (expt 10 100)
  10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(deftest exptf.6
  (expt -2 8)
  256)

(deftest exptf.7
  (expt -2 9)
  -512)

(deftest exptf.8
  (expt 2 -8)
  1/256)

(deftest exptf.9
  (expt -2 -7)
  -1/128)

(deftest exptf.10
  (expt -2 -8)
  1/256)

(deftest exptf.11
  (expt 1234 123)
  170515806212727042875059727620626282654302313111068290470529619322183913834868007471366306717060598572641592314554345900570589670671499709086102539904846514793135617305563669993950104622035682027355757755070083238444147778396026387067042685700404003287042480639680696865587865016699383883388831980459159942845372414601809429717726107628595243406801014418529766279838067203562799104)

(deftest exptf.12
  (expt 7 -1234)
  1/70954734215028011240104595141498929070420289200365254864883100224459061531095964535961424752183918983389834714345883085722594630104327570484382813454950959834721299254363358098967399048133489131401375954827777487240689332659684949614116231197346793510695402301156840458057925504143947679213040737846060014251249690928324441226158708348216074516368113070768675494487327714367821662938661080440834970823376650216081882744143161592962823478851621834726159007802788609981541818975600270503709811145869971874252832721423453899821462438727851069669041332053765106227148570291872998008674128627485981069824533343176007316751497280494057995201432198045575691194828311578763776024467116028227768366827683004790939265568398432923069244698164607404132404210543306184313774389935234011995625090611727226388755028092996618090667295922698497975515840710550500258575437750076719351206138149202280453034232757583405474400100672191700332560347912741669538744924043142095605796598736664375513698661239721232174697654768024941266796370339939780641017369634940849)

(deftest exptf.13
  (expt 2 (make-bignum 8))
  256)

(deftest-float exptf.14
  (expt 2 3/4)
  1.6817926)

(deftest-float exptf.15
  (expt -2 3/4)
  -1.189207 1.1892072)

(deftest-float exptf.16
  (expt 3 -1.2f0)
  0.2675805)

(deftest-double exptf.17
  (expt -3 2.3d0)
  7.355252243329742d0 10.123636208224076d0)

(deftest-long exptf.18
  (expt -3 -1.9l0)
  0.11794402418366383989L0 0.038322336510242612396L0)

(deftest-single exptf.19
  (expt 3 #c(2 -3/4))
  6.1138897 -6.6045704)

(deftest-double exptf.20
  (expt 3 #c(1.2d0 3.4f0))
  -3.0976936660373564d0 -2.0906707336714394d0)

(deftest-single exptf.21
  (expt 3 #c(-2 3))
  -0.109791994 -0.01707037)

(deftest expb.1
  (expt (make-bignum 10) 1)
  10)

(deftest expb.2
  (expt (make-bignum 10) 2)
  100)

(deftest expb.3
  (expt (make-bignum 10) 3)
  1000)

(deftest expb.4
  (expt (make-bignum 10) 4)
  10000)

(deftest expb.5
  (expt (make-bignum 10) 100)
  10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(deftest expb.6
  (expt (make-bignum -2) 8)
  256)

(deftest expb.7
  (expt (make-bignum -2) 9)
  -512)

(deftest expb.8
  (expt (make-bignum 2) -8)
  1/256)

(deftest expb.9
  (expt (make-bignum -2) -7)
  -1/128)

(deftest expb.10
  (expt (make-bignum -2) -8)
  1/256)

(deftest expb.11
  (expt (make-bignum 9) 55)
  30432527221704537086371993251530170531786747066637049)

(deftest expb.12
  (expt (make-bignum 7) -88)
  1/233683216210633558353880137011125430143959282107856711392134007594290612801)

(deftest expb.13
  (expt (make-bignum 2) (make-bignum 8))
  256)

(deftest-float expb.14
  (expt (make-bignum 2) 3/4)
  1.6817926)

(deftest-float expb.15
  (expt (make-bignum -2) 3/4)
  -1.189207 1.1892072)

(deftest-float expb.16
  (expt (make-bignum 3) -1.2f0)
  0.2675805)

(deftest-double expb.17
  (expt (make-bignum -3) 2.3d0)
  7.355252243329742d0 10.123636208224076d0)

(deftest-long expb.18
  (expt (make-bignum -3) -1.9l0)
  0.11794402418366383989L0 0.038322336510242612396L0)

(deftest-single expb.19
  (expt (make-bignum 3) #c(2 -3/4))
  6.1138897 -6.6045704)

(deftest-double expb.20
  (expt (make-bignum 3) #c(1.2d0 3.4f0))
  -3.0976936660373564d0 -2.0906707336714394d0)

(deftest-single exptb.21
  (expt (make-bignum 3) #c(-2 3))
  -0.109791994 -0.01707037)

(deftest expr.1
  (expt 2/3 8)
  256/6561)

(deftest expr.2
  (expt -2/3 (make-bignum -9))
  -19683/512)

(deftest-single expr.3
  (expt 2/3 5/7)
  0.7485495)

(deftest-single expr.4
  (expt -2/3 4.5f-2)
  0.97212356 0.13835368)

(deftest-double expr.5
  (expt -2/3 1.2d0)
  -0.4973339806908717d0 -0.3613342875941152d0)

(deftest-long expr.6
  (expt 2/3 -1.2L0)
  1.6267076567965479206L0)

(deftest-double expr.7
  (expt 2/3 #c(1.2f0 -3.4d1))
  0.21157379179848101d0 0.5771828754841491d0)

(deftest-single expr.8
  (expt 2/3 #c(1 2))
  0.45921627 -0.4832855)

(deftest-single exps.1
  (expt 2.3f0 -2)
  0.18903592)

(deftest-single exps.2
  (expt 2.3f0 (make-bignum -2))
  0.18903592)

(deftest-single exps.3
  (expt 2.3f0 -2/3)
  0.5739157)

(deftest-single exps.4
  (expt -2.3f0 1.2f0)
  -2.198017 -1.5969532)

(deftest-double exps.5
  (expt -2.3f0 -1.2d0)
  -0.297772270330278d0 0.21634421805565585d0)

(deftest-long exps.6
  (expt 2.3f0 -2.1L0)
  0.17392882509562813d0)

(deftest-double exps.7
  (expt 2.3f0 #c(1.2f0 -3.1d0))
  -2.302520072651371d0 -1.442199232632068d0)

(deftest-single exps.8
  (expt -2.3f0 #c(7 3))
  0.021991925 -0.016472045)

(deftest-double expd.1
  (expt -2.3d0 -2)
  0.18903591682419663d0 0.0d0)

(deftest-double expd.2
  (expt -2.3d0 (make-bignum -2))
  0.18903591682419663d0 0.0d0)

(deftest-double expd.3
  (expt 2.3d0 -2/3)
  0.5739157051286576d0)

(deftest-double expd.4
  (expt -2.3d0 1.2f0)
  -2.198016851951299d0 -1.5969532232935613d0)

(deftest-double expd.5
  (expt -2.3d0 -1.2d0)
  -0.29777226292216236d0 0.2163442126733448d0)

(deftest-long expd.6
  (expt 2.3d0 -2.1L0)
  0.1739288175232358d0)

(deftest-double expd.7
  (expt 2.3d0 #c(1.2f0 -3.1d0))
  -2.302520222623744d0 -1.4421991205302758d0)

(deftest-double expd.8
  (expt -2.3d0 #c(7 3))
  0.021991899471513303d0 -0.016472086611901793d0)

(deftest-long expl.1
  (expt -2.3L0 -2)
  0.18903591682419663L0 0.0L0)

(deftest-long expl.2
  (expt -2.3L0 (make-bignum -2))
  0.18903591682419663L0 0.0L0)

(deftest-long expl.3
  (expt 2.3L0 -2/3)
  0.5739157051286576L0)

(deftest-long expl.4
  (expt -2.3L0 1.2f0)
  -2.198016851951299L0 -1.5969532232935613L0)

(deftest-long expl.5
  (expt -2.3L0 -1.2L0)
  -0.29777226292216236L0 0.2163442126733448L0)

(deftest-long expl.6
  (expt 2.3L0 -2.1L0)
  0.1739288175232358L0)

(deftest-long expl.7
  (expt 2.3L0 #c(1.2f0 -3.1L0))
  -2.302520222623744L0 -1.4421991205302758L0)

(deftest-long expl.8
  (expt -2.3L0 #c(7 3))
  0.021991899471513303L0 -0.016472086611901793L0)

(deftest expc.1
  (expt #c(10 -3/4) 3)
  #c(7865/8 -14373/64))

(deftest expc.2
  (expt #c(10 -3/4) (make-bignum -3))
  #c(4026880/4165509529 919872/4165509529))

(deftest-single expc.3
  (expt #c(10 -3/4) -6/7)
  0.13833067 0.00888827)

(deftest-single expc.4
  (expt #c(2 -3/4) 1.2f0)
  2.2592518 -1.0375758)

(deftest-double expc.5
  (expt #c(10 -3/4) -1.3d0)
  0.04970004219351855d0 0.0048520259958496435d0)

(deftest-long expc.6
  (expt #c(-3 3/4) -1.3L0)
  -0.18704277584270710282L0 0.13466582318277132247L0)

(deftest-double expc.7
  (expt #c(-1.2f0 3.4d0) -3)
  0.01815566699784148d0 0.011204368767686747d0)

(deftest-double expc.8
  (expt #c(-1.2f0 3.4d0) (make-bignum -3))
  0.01815566699784148d0 0.011204368767686747d0)

(deftest-double expc.9
  (expt #c(-1.2f0 3.4d0) -6/7)
  -0.02211023365124718d0 -0.3323815806407974d0)

(deftest-double expc.10
  (expt #c(-1.2f0 3.4d0) 1.9d0)
  -10.10272275066296d0 -5.357261568911895d0)

(deftest-long expc.11
  (expt #c(-1.2f0 3.4d0) 1.9L0)
  -10.10272275066296d0 -5.357261568911895d0)

(deftest-single expc.12
  (expt #c(1 2) #c(-3 -4))
  7.250044 -1.906459)

(deftest-double expc.13
  (expt #c(1.2d0 2) #c(-3 -4))
  4.766465047058875d0 -0.9439163516910051d0)

(deftest-long expc.14
  (expt #c(1 2) #c(-3.9L0 -4))
  1.1333987462203081d0 -3.4521857102724227d0)

(deftest-single expc.15
  (expt #c(1.2 3.4) #c(5.6 7.8))
  -0.032776006 -0.08229103)

;;  log e
(deftest-single loge.1
  (log 1)
  0.0)

(deftest-error loge.2
  (log 0)
  arithmetic-error)

(deftest-single loge.3
  (log 2)
  0.6931472)

(deftest-single loge.4
  (log (make-bignum -3))
  1.0986123 3.1415927)

(deftest-single loge.5
  (log 4/5)
  -0.22314355)

(deftest-single loge.6
  (log -2.3f0)
  0.8329091 3.1415927)

(deftest-double loge.7
  (log 4d100)
  231.64480366052445d0)

(deftest-long loge.8
  (log -9.8l-2)
  -2.3227878003115651324L0 3.1415926535897932385L0)

(deftest-single loge.9
  (log #c(1 2))
  0.804719 1.1071488)

(deftest-double loge.10
  (log #c(1.2f0 3.4d0))
  1.2824746831323421d0 1.2315036998697262d0)

(deftest-long loge.11
  (log #c(1.2f0 3.4L0))
  1.2824746831323421d0 1.2315036998697262d0)

;;  log
(deftest-error logf.1
  (log 0 0)
  arithmetic-error)

(deftest-error logf.2
  (log 0 10)
  arithmetic-error)

(deftest-single logf.3
  (log 10 0)
  0.0)

(deftest-single logf.4
  (log 10 20)
  0.7686218)

(deftest-single logf.5
  (log 10 (make-bignum 20))
  0.7686218)

(deftest-single logf.6
  (log 10 -3/4)
  -0.066558294 -0.72684073)

(deftest-single logf.7
  (log 10 2.3f0)
  2.7645094)

(deftest-double logf.8
  (log -10 2.3d0)
  2.7645093919489354d0 3.7718312443488147d0)

(deftest-long logf.9
  (log -10 -2.3L0)
  1.115882504066917d0 -0.43708924951297623d0)

(deftest-single logf.10
  (log 10 #c(2 -3))
  1.1311495 0.86682934)

(deftest-double logf.11
  (log 10 #c(1.2d0 3.4))
  0.9340990086173372D0 -0.8969739643862817D0 1e-6)

(deftest-double logf.12
  (log 10 #c(1.2d0 3.4d0))
  0.9340990086173372D0 -0.8969739643862817D0)

(deftest-error logb.1
  (log (make-bignum 0) 0)
  arithmetic-error)

(deftest-error logb.2
  (log (make-bignum 0) 10)
  arithmetic-error)

(deftest-single logb.3
  (log (make-bignum 10) 0)
  0.0)

(deftest-single logb.4
  (log (make-bignum 10) 20)
  0.7686218)

(deftest-single logb.5
  (log (make-bignum 10) (make-bignum 20))
  0.7686218)

(deftest-single logb.6
  (log (make-bignum 10) -3/4)
  -0.066558294 -0.72684073)

(deftest-single logb.7
  (log (make-bignum 10) 2.3f0)
  2.7645094)

(deftest-double logb.8
  (log (make-bignum -10) 2.3d0)
  2.7645093919489354d0 3.7718312443488147d0)

(deftest-long logb.9
  (log (make-bignum -10) -2.3L0)
  1.115882504066917d0 -0.43708924951297623d0)

(deftest-single logb.10
  (log (make-bignum 10) #c(2 -3))
  1.1311495 0.86682934)

(deftest-double logb.11
  (log (make-bignum 10) #c(1.2d0 3.4))
  0.9340990086173372D0 -0.8969739643862817D0 1e-6)

(deftest-double logb.12
  (log (make-bignum 10) #c(1.2d0 3.4d0))
  0.9340990086173372D0 -0.8969739643862817D0)

(deftest-error logr.1
  (log (make-ratio 0 1) 0)
  arithmetic-error)

(deftest-error logr.2
  (log (make-ratio 0 1) 10)
  arithmetic-error)

(deftest-single logr.3
  (log 10/11 0)
  0.0)

(deftest-single logr.4
  (log 10/11 20)
  -0.0318153)

(deftest-single logr.5
  (log 10/11 (make-bignum 20))
  -0.0318153)

(deftest-single logr.6
  (log 11/6 -3/4)
  -0.0175209 -0.19133459)

(deftest-single logr.7
  (log 5/6 2.3f0)
  -0.2188973)

(deftest-double logr.8
  (log -5/6 2.3d0)
  -0.21889729836487837D0 3.7718312443488147D0)

(deftest-long logr.9
  (log -5/6 -2.3L0)
  0.9199500598980037D0 0.30193486518496077D0)

(deftest-single logr.10
  (log 5/6 #c(2 -3))
  -0.08956583 -0.068636626)

(deftest-double logr.11
  (log 5/6 #c(1.2d0 3.4))
  -0.07396312286858601D0 0.07102351475567023D0)

(deftest-double logr.12
  (log 5/6 #c(1.2d0 3.4d0))
  -0.07396312343417179D0 0.07102351617238363D0)

(deftest-error logs.1
  (log 0.0f0 0)
  arithmetic-error)

(deftest-error logs.2
  (log 0.0f0 10)
  arithmetic-error)

(deftest-single logs.3
  (log 1.2f0 0)
  0.0)

(deftest-single logs.4
  (log 1.2f0 20)
  0.060860444)

(deftest-single logs.5
  (log 1.2f0 (make-bignum 20))
  0.060860444)

(deftest-single logs.6
  (log 9.2f0 -3/4)
  -0.06414807 -0.7005202)

(deftest-single logs.7
  (log 1.2f0 2.3f0)
  0.21889734)

(deftest-double logs.8
  (log -1.6f0 2.3d0)
  0.5642916270272584D0 3.7718312443488147D0)

(deftest-long logs.9
  (log -1.2f0 -2.3L0)
  0.9487018137260168D0 0.19348810176663522D0)

(deftest-single logs.10
  (log 1.2f0 #c(2 -3))
  0.08956584 0.06863664)

(deftest-double logs.11
  (log 1.2f0 #c(1.2d0 3.4))
  0.0739631389886238D0 -0.07102353023502937D0)

(deftest-double logs.12
  (log 1.2f0 #c(1.2d0 3.4d0))
  0.0739631395542097D0 -0.07102353165174308D0)

(deftest-error logd.1
  (log 0.0d0 0)
  arithmetic-error)

(deftest-error logd.2
  (log 0.0d0 10)
  arithmetic-error)

(deftest-double logd.3
  (log 1.2d0 0)
  0.0)

(deftest-double logd.4
  (log 1.2d0 20)
  0.06086043082136214d0)

(deftest-double logd.5
  (log 1.2d0 (make-bignum 20))
  0.06086043082136214d0)

(deftest-double logd.6
  (log 9.2d0 -3/4)
  -0.06414807268141035D0 -0.7005202380542545D0)

(deftest-double logd.7
  (log 1.2d0 2.3f0)
  0.2188973038134799D0)

(deftest-double logd.8
  (log -1.6d0 2.3d0)
  0.5642916091367579D0 3.7718312443488147D0)

(deftest-long logd.9
  (log -1.2d0 -2.3L0)
  0.9487018105928385D0 0.19348811358445517D0)

(deftest-double logd.10
  (log 1.2d0 #c(2 -3))
  0.0895658309892637D0 0.06863662727500809D0)

(deftest-double logd.11
  (log 1.2d0 #c(1.2d0 3.4))
  0.07396312286858601D0 -0.07102351475567023D0)

(deftest-double logd.12
  (log 1.2d0 #c(1.2d0 3.4d0))
  0.07396312343417177D0 -0.07102351617238363D0)

(deftest-error logl.1
  (log 0.0L0 0)
  arithmetic-error)

(deftest-error logl.2
  (log 0.0L0 10)
  arithmetic-error)

(deftest-long logl.3
  (log 1.2L0 0)
  0.0)

(deftest-long logl.4
  (log 1.2L0 20)
  0.06086043082136214d0)

(deftest-long logl.5
  (log 1.2L0 (make-bignum 20))
  0.06086043082136214d0)

(deftest-long logl.6
  (log 9.2L0 -3/4)
  -0.06414807268141035D0 -0.7005202380542545D0)

(deftest-long logl.7
  (log 1.2L0 2.3f0)
  0.2188973038134799D0)

(deftest-long logl.8
  (log -1.6L0 2.3d0)
  0.5642916091367579D0 3.7718312443488147D0)

(deftest-long logl.9
  (log -1.2L0 -2.3L0)
  0.9487018105928385D0 0.19348811358445517D0)

(deftest-long logl.10
  (log 1.2L0 #c(2 -3))
  0.0895658309892637D0 0.06863662727500809D0)

(deftest-long logl.11
  (log 1.2L0 #c(1.2d0 3.4))
  0.07396312286858601D0 -0.07102351475567023D0)

(deftest-long logl.12
  (log 1.2L0 #c(1.2L0 3.4L0))
  0.07396312343417177D0 -0.07102351617238363D0)

(deftest-single logc.1
  (log #c(1.2 3.4) 0)
  0.0)

(deftest-single logc.2
  (log #c(1.2 3.4) 20)
  0.4281006 0.41108602)

(deftest-double logc.3
  (log #c(1.2d0 3.4d0) (make-bignum 20))
  0.42810056494444443D0 0.4110860383661241D0)

(deftest-single logc.4
  (log #c(1 2) -3/4)
  0.32622465 -0.28602305)

(deftest-single logc.5
  (log #c(1.2f0 3.4f0) 2.3f0)
  1.5397536 1.4785571)

(deftest-double logc.6
  (log #c(1 2) 2.3d0)
  0.9661545708387561D0 1.3292551219664739D0)

(deftest-long logc.7
  (log #c(-1 2.0f0) -2.3L0)
  0.6685054670590511D0 -0.0789136852889855D0)

(deftest-double logc.8
  (log #c(1.2d0 3.4) #c(2 -3))
  0.1664073694561802D0 1.0877780766231384D0)

(deftest-long logc.9
  (log #c(1 2) #c(1.2L0 3.4))
  0.7577442994547355D0 0.1356625536875226D0)

