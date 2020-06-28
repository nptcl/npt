;;
;;  compile-define
;;

;;
;;  deftype
;;
(deftest compile-deftype.1
  (progn
    (setq *result* nil)
    (test-file-compile
      (deftype compile-deftype-1 ()
        'integer)
      (eval-when (:compile-toplevel)
        (setq *result* (typep 100 'compile-deftype-1))))
    *result*)
  t)

(deftest compile-deftype.2
  (progn
    (setq *result* nil)
    (test-file-compile
      (deftype compile-deftype-2 ()
        'integer)
      (eval-when (:compile-toplevel :load-toplevel)
        (setq *result* (typep 100 'compile-deftype-2))))
    (setq *result* nil)
    (deftype compile-deftype-2 ()
      'string)
    (test-file-load)
    *result*)
  t)

(deftest-error compile-deftype.3
  (test-file-compile
    (let ()
      (deftype compile-deftype-3 ()
        'integer))
    (eval-when (:compile-toplevel)
      (setq *result* (typep 100 'compile-deftype-3)))))


;;
;;  defmacro
;;
'(deftest compile-defmacro.1
   (progn
     (setq *result* nil)
     (test-file-compile
       (defmacro compile-defmacro-1 ()
         :hello)
       (eval-when (:compile-toplevel)
         (setq *result* (compile-defmacro-1))))
     *result*)
   :hello)

