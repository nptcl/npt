;;
;;  Error
;;
(defun function-var1 ()
  :hello)

(deftest variable-ordinary.1
  (function-var1)
  :hello)

(deftest-error variable-ordinary.2
  (function-var1 10))

(deftest variable-ordinary.3
  (funcall #'function-var1)
  :hello)

(deftest-error variable-ordinary.4
  (funcall #'function-var1 10))

(deftest variable-ordinary.5
  (apply #'function-var1 nil)
  :hello)

(deftest-error variable-ordinary.6
  (apply #'function-var1 10 nil))

(defmacro macro-var1 ()
  :hello)

(deftest variable-macro.1
  (macro-var1)
  :hello)

(deftest-error variable-macro.2
  (eval '(macro-var1 10)))

(deftest variable-macro.3
  (funcall (macro-function 'macro-var1) '(macro-var1) nil)
  :hello)

(deftest-error variable-macro.4
  (funcall (macro-function 'macro-var1) '(macro-var1 10) nil))

(defgeneric generic-var1 ())
(defmethod generic-var1 ()
  :generic)

(deftest variable-generic.1
  (generic-var1)
  :generic)

(deftest-error variable-generic.2
  (generic-var1 10))

(deftest variable-generic.3
  (funcall #'generic-var1)
  :generic)

(deftest-error variable-generic.4
  (funcall #'generic-var1 10))


;;
;;  :allow-other-keys t
;;
(defun function-allow1 (&key aaa bbb)
  (format nil "~A ~A" aaa bbb))

(deftest ordinary-allow-other-keys.1
  (function-allow1)
  "NIL NIL")

(deftest ordinary-allow-other-keys.2
  (function-allow1 :aaa 10 :bbb 20 :aaa 30)
  "10 20")

(deftest-error ordinary-allow-other-keys.3
  (function-allow1 :ccc 30))

(deftest ordinary-allow-other-keys.4
  (function-allow1 :ccc 30 :allow-other-keys t)
  "NIL NIL")

(deftest ordinary-allow-other-keys.5
  (function-allow1 :allow-other-keys t :ddd 50 :eee 60 :aaa 20)
  "20 NIL")

(deftest ordinary-allow-other-keys.6
  (funcall #'function-allow1)
  "NIL NIL")

(deftest ordinary-allow-other-keys.7
  (funcall #'function-allow1 :aaa 10 :bbb 20)
  "10 20")

(deftest-error ordinary-allow-other-keys.8
  (funcall #'function-allow1 :ccc 30))

(deftest ordinary-allow-other-keys.9
  (funcall #'function-allow1 :ccc 30 :allow-other-keys t)
  "NIL NIL")

(deftest ordinary-allow-other-keys.10
  (funcall #'function-allow1 :allow-other-keys t :ddd 50 :eee 60 :aaa 20)
  "20 NIL")

(defmacro macro-allow1 (&key aaa bbb)
  (format nil "~A ~A" aaa bbb))

(deftest macro-allow-other-keys.1
  (macro-allow1)
  "NIL NIL")

(deftest macro-allow-other-keys.2
  (macro-allow1 :bbb 10 :aaa 20 :bbb 30)
  "20 10")

(deftest-error macro-allow-other-keys.3
  (eval '(macro-allow1 :ccc 10)))

(deftest macro-allow-other-keys.4
  (macro-allow1 :ccc 30 :allow-other-keys t)
  "NIL NIL")

(deftest macro-allow-other-keys.5
  (macro-allow1 :allow-other-keys t :ddd 50 :eee 60 :aaa 20)
  "20 NIL")

(defgeneric generic-allow1 (&key aaa bbb))
(defmethod generic-allow1 (&key aaa bbb)
  (format nil "~A ~A" aaa bbb))

(deftest generic-allow-other-keys.1
  (generic-allow1)
  "NIL NIL")

(deftest generic-allow-other-keys.2
  (generic-allow1 :aaa 10 :bbb 20 :aaa 30)
  "10 20")

(deftest-error generic-allow-other-keys.3
  (generic-allow1 :ccc 30))

(deftest generic-allow-other-keys.4
  (generic-allow1 :ccc 30 :allow-other-keys t)
  "NIL NIL")

(deftest generic-allow-other-keys.5
  (generic-allow1 :allow-other-keys t :ddd 50 :eee 60 :aaa 20)
  "20 NIL")

(deftest generic-allow-other-keys.6
  (funcall #'generic-allow1)
  "NIL NIL")

(deftest generic-allow-other-keys.7
  (funcall #'generic-allow1 :aaa 10 :bbb 20)
  "10 20")

(deftest-error generic-allow-other-keys.8
  (funcall #'generic-allow1 :ccc 30))

(deftest generic-allow-other-keys.9
  (funcall #'generic-allow1 :ccc 30 :allow-other-keys t)
  "NIL NIL")

(deftest generic-allow-other-keys.10
  (funcall #'generic-allow1 :allow-other-keys t :ddd 50 :eee 60 :aaa 20)
  "20 NIL")

