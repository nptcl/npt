;;
;;  ANSI COMMON LISP: 9. Conditions
;;
(deftest handler-case.1
   (handler-case
     (error "Hello")
     (error () :hello))
   :hello)

(deftest handler-case.2
   (handler-case
     (handler-case
       (error "Hello")
       (warning () :warning))
     (error () :hello))
   :hello)

(deftest handler-case.3
   (handler-case
     (handler-case
       (error "Hello")
       (warning () :warning))
     (error () :hello))
   :hello)

(deftest handler-bind.1
  (let ((check :aaa))
    (handler-case
      (handler-bind
        ((error (lambda (c) (declare (ignore c))
                  (setq check t))))
        (error "Hello"))
      (error ()
        (values :error check))))
  :error t)

(deftest restart-bind.1
  (restart-bind
    ((hello (lambda () :aaa)))
    10)
  10)

(deftest restart-bind.2
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart
      (find-restart 'hello)))
  :aaa)

(deftest restart-bind.3
  (restart-bind
    ((hello (lambda () :aaa)))
    (invoke-restart
      (find-restart 'hello))
    :hello)
  :hello)

(deftest restart-case.1
  (restart-case
    20
    (hello () :aaa))
  20)

(deftest restart-case.2
  (restart-case
    (invoke-restart
      (find-restart 'hello))
    (hello () :aaa))
  :aaa)

(deftest restart-case.3
  (restart-case
    (progn
      (invoke-restart
        (find-restart 'hello))
      100)
    (hello () :aaa))
  :aaa)


;;
;;  do-tests
;;
(do-tests :test t)

