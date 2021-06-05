;;
;;  compile-toplevel
;;
(defvar *compile-eval-when* nil)
(defvar *compile-eval-when1* nil)
(defvar *compile-eval-when2* nil)

;;
;;  operator
;;
(deftest compile-toplevel.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (setq *compile-eval-when* t))
    *compile-eval-when*)
  nil)

(deftest compile-toplevel.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (setq *compile-eval-when* t))
    (test-file-load)
    *compile-eval-when*)
  t)

(deftest compile-toplevel.3
  (progn
    (setq *compile-eval-when* nil)
    (setq *compile-eval-when1* nil)
    (setq *compile-eval-when2* nil)
    (test-file-compile
      (setq *compile-eval-when* 10)
      (setq *compile-eval-when1* 20)
      (setq *compile-eval-when2* 30))
    (values
      *compile-eval-when*
      *compile-eval-when1*
      *compile-eval-when2*))
  nil nil nil)

(deftest compile-toplevel.4
  (progn
    (setq *compile-eval-when* nil)
    (setq *compile-eval-when1* nil)
    (setq *compile-eval-when2* nil)
    (test-file-compile
      (setq *compile-eval-when* 10)
      (setq *compile-eval-when1* 20)
      (setq *compile-eval-when2* 30))
    (test-file-load)
    (values
      *compile-eval-when*
      *compile-eval-when1*
      *compile-eval-when2*))
  10 20 30)


;;
;;  eval-when
;;
(deftest compile-eval-when.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel)
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  t)

(deftest compile-eval-when.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel)
        (setq *compile-eval-when* t)))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)

(deftest compile-eval-when.3
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  nil)

(deftest compile-eval-when.4
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (setq *compile-eval-when* t)))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  t)

(deftest compile-eval-when.5
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:execute)
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  nil)

(deftest compile-eval-when.6
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:execute)
        (setq *compile-eval-when* t)))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)

(deftest compile-eval-when.7
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  t)

(deftest compile-eval-when.8
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (setq *compile-eval-when* t)))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  t)

(deftest compile-eval-when.9
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when ()
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  nil)

(deftest compile-eval-when.10
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when ()
        (setq *compile-eval-when* t)))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)

(deftest compile-eval-when.11
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when ()
        (quote aaa bbb ccc)))
    *compile-eval-when*)
  nil)


;;
;;  compile-time
;;
(deftest compile-eval-compile-time-too.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (eval-when (:execute)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  t)

(deftest compile-eval-compile-time-too.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (eval-when (:execute)
          (setq *compile-eval-when* t))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)

(deftest compile-eval-compile-time-too.3
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (let ()
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    *compile-eval-when*)
  t)

(deftest compile-eval-compile-time-too.4
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (let ()
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  t)

(deftest compile-eval-compile-time-too.5
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    *compile-eval-when*)
  nil)

(deftest compile-eval-compile-time-too.6
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  t)


(deftest compile-eval-not-compile-time.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:execute)
        (setq *compile-eval-when* t)))
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:execute)
        (setq *compile-eval-when* t)))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.3
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:execute)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.4
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:execute)
          (setq *compile-eval-when* t))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  t)

(deftest compile-eval-not-compile-time.5
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (eval-when (:execute)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.6
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (eval-when (:execute)
          (setq *compile-eval-when* t))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.7
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (let ()
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.8
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (eval-when (:load-toplevel)
        (let ()
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  t)

(deftest compile-eval-not-compile-time.9
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:load-toplevel)
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    *compile-eval-when*)
  nil)

(deftest compile-eval-not-compile-time.10
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:load-toplevel)
          (eval-when (:execute)
            (setq *compile-eval-when* t)))))
    (setq *compile-eval-when* nil)
    (test-file-load)
    *compile-eval-when*)
  nil)


;;
;;  progn
;;
(deftest compile-eval-progn.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (progn
        (eval-when (:compile-toplevel)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  t)

(deftest compile-eval-progn.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (let ()
        (eval-when (:compile-toplevel)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  nil)


;;
;;  locally
;;
(deftest compile-eval-locally.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (locally
        (eval-when (:compile-toplevel)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  t)

(deftest compile-eval-locally.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (locally
        (declare (special *compile-eval-locally*))
        (eval-when (:compile-toplevel)
          (setq *compile-eval-locally* t))))
    (symbol-value '*compile-eval-locally*))
  t)


;;
;;  symbol-macrolet
;;
(deftest compile-eval-symbol-macrolet.1
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (symbol-macrolet
        nil
        (eval-when (:compile-toplevel)
          (setq *compile-eval-when* t))))
    *compile-eval-when*)
  t)

(deftest compile-eval-symbol-macrolet.2
  (progn
    (setq *compile-eval-when* nil)
    (test-file-compile
      (symbol-macrolet
        nil
        (eval-when (:compile-toplevel)
          (let ((x (list 10 20 30)))
            (symbol-macrolet
              ((y (car x)) (z (cadr x)))
              (setq y (1+ z) z (1+ y))
              (setq *compile-eval-when* (list x y z)))))))
    *compile-eval-when*)
  ((21 22 30) 21 22))


;;
;;  type dependency
;;
(deftest compile-eval-dependency.1
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (with-open-file (s input :direction :output)
        (format s "(defstruct compile-eval-dependency-1 aaa bbb)~%")
        (format s "(defun compile-eval-dependency-1-call (x)~%")
        (format s "  (declare (type compile-eval-dependency-1 x))~%")
        (format s "  x)~%"))
      (file-position input :start)
      (compile-file input :output-file output)
      nil))
  nil)

#|
(deftest compile-eval-dependency.2
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (with-open-file (s input :direction :output)
        (format s "(progn~%")
        (format s "  (defstruct compile-eval-dependency-2 aaa bbb)~%")
        (format s "  (defun compile-eval-dependency-2-call (x)~%")
        (format s "    (declare (type compile-eval-dependency-2 x))~%")
        (format s "    x))~%"))
      (file-position input :start)
      (compile-file input :output-file output)
      nil))
  nil)
|#

