;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;

;;
;;  optimizer off  [degrade]
;;
#+rt-degrade
(deftest rteval-optimize-check-degrade.1
  (lisp-system:optimize-check parse)
  0)

#+rt-degrade
(deftest rteval-optimize-check-degrade.2
  (lisp-system:optimize-check scope)
  0)


;;
;;  optimizer on  [load]
;;
#-rt-degrade
(deftest rteval-optimize-check-load.1
  (lisp-system:optimize-check parse)
  1)

#-rt-degrade
(deftest rteval-optimize-check-load.2
  (lisp-system:optimize-check scope)
  1)

