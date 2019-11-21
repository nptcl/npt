;;
;;  Error
;;
(deftest if-nil.1
  (list 10 20 (when nil 'hello))
  (10 20 nil))


;;
;;  do-tests
;;
(do-tests :test t)

