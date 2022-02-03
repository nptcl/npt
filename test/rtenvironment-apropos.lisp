;;
;;  ANSI COMMON LISP: 25. Environment
;;

;;
;;  Function APROPOS
;;
(deftest apropos.1
  (with-open-stream (stream (make-string-output-stream))
    (let ((*standard-output* stream))
      (apropos "Hello"))))

(deftest apropos.2
  (with-open-stream (stream (make-string-output-stream))
    (let ((*standard-output* stream))
      (apropos "Hello" nil))))

(deftest apropos.3
  (< 0 (length
         (with-output-to-string (*standard-output*)
           (apropos "car"))))
  t)

(defpackage apropos-test)
(intern "CAR" 'apropos-test)

(defun apropos-search (str key)
  (let ((str (string str))
        (key (string key)))
    (and (search key str) t)))

(deftest apropos.4
  (let ((str (with-output-to-string (*standard-output*)
               (apropos 'car))))
    (values
      (apropos-search str "HELLO")
      (apropos-search str "MAPCAR")
      (apropos-search str "APROPOS-TEST")))
  nil t t)

(deftest apropos.5
  (let ((str (with-output-to-string (*standard-output*)
               (apropos 'car 'common-lisp))))
    (values
      (apropos-search str "HELLO")
      (apropos-search str "MAPCAR")
      (apropos-search str "APROPOS-TEST")))
  nil t nil)

(deftest apropos.6
  (let ((str (with-output-to-string (*standard-output*)
               (apropos 'car (find-package 'apropos-test)))))
    (values
      (apropos-search str "HELLO")
      (apropos-search str "MAPCAR")
      (apropos-search str "APROPOS-TEST")))
  nil nil t)

(deftest-error! apropos-error.1
  (eval '(apropos)))

(deftest-error! apropos-error.2
  (eval '(apropos 10))
  type-error)

(deftest-error! apropos-error.3
  (eval '(apropos 'car nil nil)))


;;
;;  Function APROPOS-LIST
;;
(deftest apropos-list.1
  (apropos-list "no-such-apropos-symbol")
  nil)

(deftest apropos-list.2
  (consp
    (apropos-list 'car nil))
  t)

(defun apropos-list-search (str key)
  (and (find key (apropos-list str)) t))

(deftest apropos-list.3
  (let ((x (apropos-list 'car)))
    (values
      (and (find 'common-lisp::car x) t)
      (and (find 'common-lisp::mapcar x) t)
      (and (find 'apropos-test::car x) t)))
  t t t)

(deftest apropos-list.4
  (let ((x (apropos-list 'car 'common-lisp)))
    (values
      (and (find 'common-lisp::car x) t)
      (and (find 'common-lisp::mapcar x) t)
      (and (find 'apropos-test::car x) t)))
  t t nil)

(deftest apropos-list.5
  (let ((x (apropos-list 'car (find-package 'apropos-test))))
    (values
      (and (find 'common-lisp::car x) t)
      (and (find 'common-lisp::mapcar x) t)
      (and (find 'apropos-test::car x) t)))
  nil nil t)

(deftest-error! apropos-list-error.1
  (eval '(apropos-list)))

(deftest-error! apropos-list-error.2
  (eval '(apropos-list 10))
  type-error)

(deftest-error! apropos-list-error.3
  (eval '(apropos-list 'car nil nil)))


;;
;;  Function DESCRIBE
;;
(deftest describe.1
  (< 1 (length
         (with-output-to-string (*standard-output*)
           (describe 100))))
  t)

(defclass describe-test () ())
(defmethod describe-object ((object describe-test) stream)
  (declare (ignore object))
  (format stream "Hello")
  999)

(deftest describe.2
  (with-output-to-string (*standard-output*)
    (describe (make-instance 'describe-test)))
  "Hello")

(deftest describe.3
  (with-open-stream (stream (make-string-output-stream))
    (describe (make-instance 'describe-test) stream)
    (get-output-stream-string stream))
  "Hello")

(deftest describe.4
  (with-open-stream (stream (make-string-output-stream))
    (describe (make-instance 'describe-test) stream)))

(deftest-error! describe-error.1
  (eval '(describe)))

(deftest-error! describe-error.2
  (eval '(describe 10 20))
  type-error)

(deftest-error! describe-error.3
  (eval '(describe 10 *standard-output* nil)))


;;
;;  Standard Generic Function DESCRIBE-OBJECT
;;
(deftest describe-object.1
  (functionp #'describe-object)
  t)

(deftest describe-object.2
  (typep #'describe-object 'standard-generic-function)
  t)

(deftest describe-object.3
  (with-output-to-string (stream)
    (describe-object
      (make-instance 'describe-test)
      stream))
  "Hello")

(deftest-error! describe-object-error.1
  (eval '(describe-object 10)))

(deftest-error! describe-object-error.2
  (eval '(describe-object 10 *standard-output* nil)))

(defclass describe-object-spaceship ()
  ((captain :initarg :captain :accessor spaceship-captain)
   (serial# :initarg :serial-number :accessor spaceship-serial-number)))

(defclass federation-starship (describe-object-spaceship) ())

(defmethod describe-object ((s describe-object-spaceship) stream)
  (with-slots (captain serial#) s
    (format stream "~&~S is a spaceship of type ~S,~
            ~%with ~A at the helm ~
            and with serial number ~D.~%"
            s (type-of s) captain serial#)))

(deftest describe-object-test.1
  (let ((x (make-instance 'federation-starship
                          :captain "Rachel Garrett"
                          :serial-number "NCC-1701-C")))
    (with-output-to-string (*standard-output*)
      (describe x))
    (values)))

(deftest describe-degrade.1
  (progn
    (with-output-to-string (*terminal-io*)
      (describe t t))
    nil)
  nil)

