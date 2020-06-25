;;
;;  compile-toplevel
;;

;;
;;  eval-when
;;
(defvar *compile-eval-when* nil)

(deftest compile-eval-when.1
  (progn
    (expr-compile
      (eval-when (:compile-toplevel)
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  t)


