;;
;;  ANSI COMMON LISP: 7. Objects
;;


(deftest redefined.1
  (progn
    (defclass redefined1 ()
      (aaa bbb ccc))
    (defclass redefined1 ()
      (ddd fff ggg))
    (null
      (find-class 'redefined1)))
  nil)


;;  redefine
;;    slot-value
;;    slot-boundp
;;    slot-exists-p
;;    slot-makunbound
;;    reader
;;    writer
;;  change-class

