(defun aaa (x)
  (if (<= x 1)
    1
    (* x (aaa (1- x)))))

(defun bbb (x)
  (do ((x x (1- x))
       (r 1))
    ((<= x 1) r)
    (setq r (* r x))))

(let ((x 1000))
  (if (= (aaa x)
         (bbb x))
    (format t "OK~%")
    (error "ERROR")))

(defun rt-package-readonly (&rest args)
  (let* ((type (lisp-implementation-type))
         (name (format nil "~A-SYSTEM" type))
         (call (intern "SYSCTL" name)))
    (apply call 'package 'readonly args)))

(defun rt-rename-package (p x y)
  (let ((value (rt-package-readonly p)))
    (rt-package-readonly p nil)
    (rename-package p x y)
    (rt-package-readonly p value)))

(defun rt-package (symbol)
  (let* ((type (lisp-implementation-type))
         (name (format nil "~A-~A" type symbol))
         (lisp (format nil "~A-~A" 'lisp symbol)))
    (rt-rename-package name name (list lisp))))

(rt-package 'system)
(lisp-system:savecore #p"lisp.core")

