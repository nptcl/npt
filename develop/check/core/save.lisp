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

(defun rt-package (symbol &optional (type (lisp-implementation-type)))
  (let ((name (format nil "~A-~A" type symbol))
        (lisp (format nil "~A-~A" 'lisp symbol)))
    (rename-package name name (list lisp))))

(rt-package 'system)
(lisp-system:savecore #p"lisp.core")

