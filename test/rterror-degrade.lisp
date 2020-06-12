;;
;;  Error
;;
(deftest if-nil.1
  (list 10 20 (when nil 'hello))
  (10 20 nil))

(deftest pprint-vector-error.1
  (let ((*print-pretty* t))
    (prin1-to-string #()))
  "#()")

