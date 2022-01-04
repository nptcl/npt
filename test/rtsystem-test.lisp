;;
;;  ANSI COMMON LISP: 24. System Construction
;;

;;
;;  Function COMPILE-FILE-PATHNAME
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

(setf (logical-pathname-translations "compile-file")
      '(("path;*.*.*" "/usr/local/path/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")
        ("aaa;bbb;*.fasl" "/usr/local/lisp/*.f")))

(deftest compile-file-pathname.4
  (equal (compile-file-pathname "compile-file:aaa;bbb;name.lisp")
         (parse-namestring "COMPILE-FILE:aaa;bbb;name.fasl"))
  t)

(deftest compile-file-pathname.5
  (equal (compile-file-pathname
           "compile-file:aaa;bbb;name.lisp"
           :output-file "compile-file:aaa;bbb;name.fasl")
         #p"/usr/local/lisp/name.f")
  t)

(deftest compile-file-pathname-stream.1
  (with-open-file (stream +compile-file1+)
    (compile-file-pathname stream))
  #p"test/rtsystem-compile1.fasl")

(deftest compile-file-pathname-stream.2
  (let ((stream (open +compile-file1+)))
    (close stream)
    (compile-file-pathname stream))
  #p"test/rtsystem-compile1.fasl")

(deftest-error compile-file-pathname-stream.3
  (let* ((lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file fasl nil)
    (with-open-stream (input (open lisp))
      (with-open-stream (output (open fasl :direction :output))
        (with-open-stream (stream (make-two-way-stream input output))
          (compile-file-pathname stream))))))

(deftest-error compile-file-pathname-stream.4
  (let* ((lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file fasl nil)
    (with-open-stream (input (open lisp))
      (with-open-stream (output (open fasl :direction :output))
        (with-open-stream (stream (make-echo-stream input output))
          (compile-file-pathname stream))))))

(deftest-error compile-file-pathname-stream.5
  (let* ((lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file fasl nil)
    (with-open-stream (output (open fasl :direction :output))
      (with-open-stream (stream (make-broadcast-stream output))
        (compile-file-pathname stream)))))

(deftest-error compile-file-pathname-stream.6
  (let* ((lisp +compile-file1+))
    (with-open-stream (input (open lisp))
      (with-open-stream (stream (make-concatenated-stream input))
        (compile-file-pathname stream)))))

(deftest-error compile-file-pathname-stream.7
  (with-open-stream (stream (make-string-input-stream "Hello"))
    (compile-file-pathname stream)))

(deftest-error compile-file-pathname-stream.8
  (with-open-stream (stream (make-string-output-stream))
    (compile-file-pathname stream)))

(deftest-error compile-file-pathname-error.1
  (compile-file-pathname "*.lisp")
  file-error)

(deftest-error compile-file-pathname-error.2
  (compile-file-pathname "name.lisp" :output-file "*.fasl")
  file-error)

(deftest-error! compile-file-pathname-error.3
  (eval '(compile-file-pathname)))

(deftest-error! compile-file-pathname-error.4
  (eval '(compile-file-pathname 10))
  type-error)

(deftest compile-file-pathname-delete.1
  (let* ((lisp +compile-file1+)
         (fasl (compile-file-pathname lisp)))
    (lisp-system:remove-file fasl nil)
    (values)))


;;
;;  Macro WITH-COMPILATION-UNIT
;;
(deftest with-compilation-unit.1
  (with-compilation-unit ())
  nil)

(deftest with-compilation-unit.2
  (with-compilation-unit ()
    :hello)
  :hello)

(deftest with-compilation-unit.3
  (with-compilation-unit (:override nil)
    10 20 30
    (values 40 50 60))
  40 50 60)

(deftest with-compilation-unit.4
  (with-compilation-unit (:override t)
    :hello)
  :hello)

(deftest with-compilation-unit.5
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

(deftest with-compilation-unit.6
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

(deftest with-compilation-unit.7
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

(deftest with-compilation-unit.8
  (with-output-to-string (*error-output*)
    (handler-bind ((warning #'muffle-warning))
      (warn "AAA")
      (warn "BBB")
      (warn "CCC")))
  "")

(deftest with-compilation-unit.9
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

(deftest with-compilation-unit.10
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

(deftest with-compilation-unit.11
  (with-compilation-unit (:override 10 :hello 'lisp)
    :hello)
  :hello)

(deftest-error! with-compilation-unit-error.1
  (eval '(with-compilation-unit)))

(deftest-error! with-compilation-unit-error.2
  (eval '(with-compilation-unit 10)))


;;
;;  Variable *FEATURES*
;;
(deftest special-features.1
  (lisp-system:specialp '*features*)
  t)

(deftest special-features.2
  (listp *features*)
  t)

(deftest special-features.3
  (find :ansi-cl *features* :test 'eq)
  :ansi-cl)

(deftest special-features.4
  (find :common-lisp *features* :test 'eq)
  :common-lisp)

(deftest special-features.5
  (values
    (read-from-string "#+common-lisp 10 #-common-lisp 20"))
  10)


;;
;;  Variable *COMPILE-FILE-PATHNAME*
;;
(deftest special-compile-file-pathname.1
  (lisp-system:specialp '*compile-file-pathname*)
  t)

(deftest special-compile-file-pathname.2
  *compile-file-pathname*
  nil)


;;
;;  Variable *COMPILE-FILE-TRUENAME*
;;
(deftest special-compile-file-truename.1
  (lisp-system:specialp '*compile-file-truename*)
  t)

(deftest special-compile-file-truename.2
  *compile-file-truename*
  nil)


;;
;;  Variable *LOAD-PATHNAME*
;;
(deftest special-load-pathname.1
  (lisp-system:specialp '*load-pathname*)
  t)

#+rt-degrade
(deftest special-load-pathname.2
  *load-pathname*
  nil)

(deftest special-load-pathname.3
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output :external-format 'utf16be)
      (format output "(setq *load-value* *load-pathname*)"))
    (let (*load-value*)
      (load lisp :external-format 'utf16be)
      (lisp-system:remove-file lisp)
      (pathname-name *load-value*)))
  "rtsystem-load2")


;;
;;  Variable *LOAD-TRUENAME*
;;
(deftest special-load-truename.1
  (lisp-system:specialp '*load-truename*)
  t)

#+rt-degrade
(deftest special-load-truename.2
  *load-truename*
  nil)

(deftest special-load-truename.3
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (with-open-file (output lisp :direction :output :external-format 'utf16be)
      (format output "(setq *load-value* *load-truename*)"))
    (let (*load-value*)
      (load lisp :external-format 'utf16be)
      (lisp-system:remove-file lisp)
      (pathname-name *load-value*)))
  "rtsystem-load2")

(deftest special-load-truename.4
  (let ((lisp +load-file2+))
    (lisp-system:remove-file lisp nil)
    (values)))


;;
;;  Variable *COMPILE-PRINT*
;;
(deftest special-compile-print.1
  (lisp-system:specialp '*compile-print*)
  t)

(deftest special-compile-print.2
  *compile-print*
  nil)


;;
;;  Variable *COMPILE-VERBOSE*
;;
(deftest special-compile-verbose.1
  (lisp-system:specialp '*compile-verbose*)
  t)

(deftest special-compile-verbose.2
  *compile-verbose*
  nil)


;;
;;  Variable *LOAD-PRINT*
;;
(deftest special-load-print.1
  (lisp-system:specialp '*load-print*)
  t)

(deftest special-load-print.2
  *load-print*
  nil)


;;
;;  Variable *LOAD-VERBOSE*
;;
(deftest special-load-verbose.1
  (lisp-system:specialp '*load-verbose*)
  t)

(deftest special-load-verbose.2
  *load-verbose*
  nil)

