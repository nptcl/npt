;;
;;  ANSI COMMON LISP: 25. Environment
;;
(deftest lisp-implementation-type.1
  (stringp
    (lisp-implementation-type))
  t)

(deftest lisp-implementation-version.1
  (stringp
    (lisp-implementation-version))
  t)

(defun string-or-nil (x)
  (or (null x) (stringp x)))

(deftest short-site-name.1
  (string-or-nil
    (short-site-name))
  t)

(deftest long-site-name.1
  (string-or-nil
    (long-site-name))
  t)

(deftest machine-instance.1
  (string-or-nil
    (machine-instance))
  t)

(deftest machine-type.1
  (string-or-nil
    (machine-type))
  t)

(deftest machine-version.1
  (string-or-nil
    (machine-version))
  t)

(deftest software-type.1
  (string-or-nil
    (software-type))
  t)

(deftest software-version.1
  (string-or-nil
    (software-version))
  t)

(deftest user-homedir-pathname.1
  (let ((x (user-homedir-pathname)))
    (or (null x)
        (pathnamep x)))
  t)

