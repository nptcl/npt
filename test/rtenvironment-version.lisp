;;
;;  ANSI COMMON LISP: 25. Environment
;;

;;
;;  Function LISP-IMPLEMENTATION-TYPE
;;
(deftest lisp-implementation-type.1
  (stringp
    (lisp-implementation-type))
  t)

(deftest-error! lisp-implementation-type.2
  (eval '(lisp-implementation-type nil)))


;;
;;  Function LISP-IMPLEMENTATION-VERSION
;;
(deftest lisp-implementation-version.1
  (stringp
    (lisp-implementation-version))
  t)

(deftest-error! lisp-implementation-version.2
  (eval '(lisp-implementation-version nil)))


;;
;;  Function SHORT-SITE-NAME
;;
(defun string-or-nil (x)
  (or (null x) (stringp x)))

(deftest short-site-name.1
  (string-or-nil
    (short-site-name))
  t)

(deftest-error! short-site-name.2
  (eval '(short-site-name nil)))


;;
;;  Function LONG-SITE-NAME
;;
(deftest long-site-name.1
  (string-or-nil
    (long-site-name))
  t)

(deftest-error! long-site-name.2
  (eval '(long-site-name nil)))



;;
;;  Function MACHINE-INSTANCE
;;
(deftest machine-instance.1
  (string-or-nil
    (machine-instance))
  t)

(deftest-error! machine-instance.2
  (eval '(machine-instance nil)))


;;
;;  Function MACHINE-TYPE
;;
(deftest machine-type.1
  (string-or-nil
    (machine-type))
  t)

(deftest-error! machine-type.2
  (eval '(machine-type nil)))


;;
;;  Function MACHINE-VERSION
;;
(deftest machine-version.1
  (string-or-nil
    (machine-version))
  t)

(deftest-error! machine-version.2
  (eval '(machine-version nil)))


;;
;;  Function SOFTWARE-TYPE
;;
(deftest software-type.1
  (string-or-nil
    (software-type))
  t)

(deftest-error! software-type.2
  (eval '(software-type nil)))


;;
;;  Function SOFTWARE-VERSION
;;
(deftest software-version.1
  (string-or-nil
    (software-version))
  t)

(deftest-error! software-version.2
  (eval '(software-version nil)))


;;
;;  Function USER-HOMEDIR-PATHNAME
;;
(defun user-homedir-pathname-p (x)
  (or (null x)
      (pathnamep x)))

(deftest user-homedir-pathname.1
  (user-homedir-pathname-p
    (user-homedir-pathname))
  t)

(deftest user-homedir-pathname.2
  (user-homedir-pathname-p
    (user-homedir-pathname "Hello"))
  t)

(deftest user-homedir-pathname.3
  (user-homedir-pathname-p
    (user-homedir-pathname '(10 20 30)))
  t)

(deftest user-homedir-pathname.4
  (user-homedir-pathname-p
    (user-homedir-pathname :unspecific))
  t)

(deftest-error! user-homedir-pathname-error.1
  (eval '(user-homedir-pathname 10))
  type-error)

(deftest-error! user-homedir-pathname-error.2
  (eval '(user-homedir-pathname :unspecific nil)))

