;;
;;  typep / subtypep
;;

;;  clos
(defclass type-clos-test-1 () ())
(defclass type-clos-test-2 (type-close-test-1) ())

(deftest typep-clos.1
  (typep (find-class 'type-clos-test-2) 'class)
  t)

