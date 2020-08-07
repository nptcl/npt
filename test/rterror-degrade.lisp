;;
;;  Error
;;
(deftest if-nil.1
  (list 10 20 (when nil 'hello))
  (10 20 nil))

(deftest pprint-vector-error.1
  (let ((*print-pretty* t))
    (prin1-to-string #()))
  "#()")

(defconstant +load-readtable+
  (with-output-to-string (*standard-output*)
    (format t "(set-dispatch-macro-character~%")
    (format t "  #\\# #\\@~%")
    (format t "  (lambda (stream char number)~%")
    (format t "    (declare (ignore stream char number))~%")
    (format t "    :hello))")))

(deftest load-readtable.1
  (with-input-from-string (input +load-readtable+)
    (load input))
  t)

(deftest load-readtable.2
  (progn
    (with-input-from-string (input +load-readtable+)
      (load input))
    (values
      (read-from-string "  #@ 10 20 30")))
  :hello)

(deftest flet-error.1
  (functionp
    (locally
      (declare (optimize (speed 3) (safety 0)))
      (flet ((bbb () 10))
        #'bbb)))
  t)


;;
;;  symbol-package
;;
(deftest symbol-package-error.1
  (progn
    (eval '(defconstant symbol-package-error 10))
    (unintern 'symbol-package-error))
  t)

(deftest symbol-package-error.2
  (find-symbol "SYMBOL-PACKAGE-ERROR")
  nil nil)


;;
;;  symbol-plist
;;
(defconstant symbol-plist-error 10)
(deftest symbol-plist-error.1
  (setf (symbol-plist 'symbol-plist-error) nil)
  nil)

(deftest symbol-plist-error.2
  (progn
    (setf (get 'symbol-plist-error 'hello) 10)
    (get 'symbol-plist-error 'hello))
  10)

(deftest symbol-plist-error.3
  (progn
    (setf (get 'symbol-plist-error 'hello) 10)
    (remprop 'symbol-plist-error 'hello)
    (get 'symbol-plist-error 'hello))
  nil)

