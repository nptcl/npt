;;
;;  System Call
;;
(deftest syscall-use-package.1
  (packagep
    (defpackage syscall-use-package-1 (:use common-lisp lisp-system)))
  t)

