;;
;;  ANSI COMMON LISP: 24. System Construction
;;

;;
;;  compile-file-pathname
;;
(deftest compile-file-pathname.1
  (compile-file-pathname #p"/usr/local/bin/hello.lisp")
  #p"/usr/local/bin/hello.fasl")

(deftest compile-file-pathname.2
  (compile-file-pathname #p"/usr/local/bin/hello.lisp" :output-file "Hello.txt")
  #p"Hello.txt")

(deftest compile-file-pathname.3
  (compile-file-pathname
    #p"/usr/local/bin/hello.lisp"
    :output-file "/home/to/Hello.txt" :aaa 'bbb :ccc 'ddd)
  #p"/home/to/Hello.txt")


;;
;;  with-compilation-unit
;;
(deftest with-compilation-unit.1
  (with-compilation-unit ()
    :hello)
  :hello)

(deftest with-compilation-unit.2
  (with-compilation-unit (:override nil)
    10 20 30
    (values 40 50 60))
  40 50 60)

(deftest with-compilation-unit.3
  (with-compilation-unit (:override t)
    :hello)
  :hello)

(deftest with-compilation-unit.4
  (with-compilation-unit (:override 10)
    10 20 30
    (values 40 50 60))
  40 50 60)

(define-condition simple-delay-warning
  (simple-warning lisp-system::delay-warning) ())

(defun warn-delay-warning (control &rest args)
  (warn (make-condition 'simple-delay-warning
                        :format-control control
                        :format-arguments args)))

(defun mkstr (&rest args)
  (apply #'concatenate 'string (mapcar #'string args)))

(deftest with-compilation-unit.5
  (with-output-to-string (*error-output*)
    (warn "Hello1")
    (with-compilation-unit ()
      (warn "Hello2")
      (format *error-output* "Hello3~%")
      (warn "Hello4"))
    (format *error-output* "Hello5~%"))
  #.(format nil (mkstr "WARNING: Hello1~%"
                       "WARNING: Hello2~%"
                       "Hello3~%"
                       "WARNING: Hello4~%"
                       "Hello5~%")))

(deftest with-compilation-unit.6
  (with-output-to-string (*error-output*)
    (warn-delay-warning "Hello1")
    (with-compilation-unit ()
      (warn-delay-warning "Hello2")
      (format *error-output* "Hello3~%")
      (warn-delay-warning "Hello4"))
    (format *error-output* "Hello5~%"))
  #.(format nil (mkstr "WARNING: Hello1~%"
                       "Hello3~%"
                       "WARNING: Hello2~%"
                       "WARNING: Hello4~%"
                       "Hello5~%")))

(deftest with-compilation-unit.7
  (with-output-to-string (*error-output*)
    (handler-bind ((warning #'muffle-warning))
      (warn "AAA")
      (warn "BBB")
      (warn "CCC")))
  "")

(deftest with-compilation-unit.8
  (with-output-to-string (*error-output*)
    (warn-delay-warning "Hello1")
    (with-compilation-unit ()
      (warn-delay-warning "Hello2")
      (format *error-output* "Hello3~%")
      (with-compilation-unit ()
        (format *error-output* "Hello4~%")
        (warn-delay-warning "Hello5")
        (warn-delay-warning "Hello6")
        (format *error-output* "Hello7~%"))
      (format *error-output* "Hello8~%")))
  #.(format nil (mkstr "WARNING: Hello1~%"
                       "Hello3~%"
                       "Hello4~%"
                       "Hello7~%"
                       "Hello8~%"
                       "WARNING: Hello2~%"
                       "WARNING: Hello5~%"
                       "WARNING: Hello6~%")))

(deftest with-compilation-unit.9
  (with-output-to-string (*error-output*)
    (warn-delay-warning "Hello1")
    (with-compilation-unit ()
      (warn-delay-warning "Hello2")
      (format *error-output* "Hello3~%")
      (with-compilation-unit (:override t)
        (format *error-output* "Hello4~%")
        (warn-delay-warning "Hello5")
        (warn-delay-warning "Hello6")
        (format *error-output* "Hello7~%"))
      (format *error-output* "Hello8~%")))
  #.(format nil (mkstr "WARNING: Hello1~%"
                       "Hello3~%"
                       "Hello4~%"
                       "Hello7~%"
                       "WARNING: Hello5~%"
                       "WARNING: Hello6~%"
                       "Hello8~%"
                       "WARNING: Hello2~%")))

(deftest with-compilation-unit.10
  (with-compilation-unit (:override 10 :hello 'lisp)
    :hello)
  :hello)

