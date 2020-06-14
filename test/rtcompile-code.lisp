;;
;;  compile-code
;;

(deftest compile.1
  (test-compile nil)
  nil)

(deftest compile.2
  (test-compile t)
  nil)

(deftest compile.3
  (test-compile 10)
  nil)

