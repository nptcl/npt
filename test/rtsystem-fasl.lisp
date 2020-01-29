;;
;;  ANSI COMMON LISP: 24. System Construction
;;
(defconstant +compile-lisp+ #p"test/rtsystem-file.lisp")
(defconstant +compile-fasl+ #p"test/rtsystem-file.fasl")

(defun write-compile-lisp (&rest args)
  (with-open-file (output +compile-lisp+ :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (x args)
      (write-line (string x) output))))

(defvar *compile-value* nil)

(deftest compile-file.1
  (let ((*compile-value* nil))
    (write-compile-lisp
      "(setq *compile-value* :hello)")
    (compile-file +compile-lisp+)
    (probe-file +compile-fasl+))
  t)

(deftest compile-file.2
  (let ((*compile-value* nil))
    (write-compile-lisp
      "(setq *compile-value* :hello)")
    (probe-file
      (compile-file +compile-lisp+)))
  t)


;;
;;  delete-file
;;
(deftest compile-file-close.1
  (progn
    (lisp-system::remove-file +compile-lisp+)
    (lisp-system::remove-file +compile-fasl+)
    (values)))

