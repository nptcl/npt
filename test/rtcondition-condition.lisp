;;
;;  ANSI COMMON LISP: 9. Conditions
;;
(import 'lisp-system:closp)

;;
;;  Condition Type CONDITION
;;
(deftest condition-condition.1
  (closp
    (make-condition 'condition))
  t)

(deftest condition-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'condition)))
  (condition standard-object t))

(deftest condition-condition.3
  (let ((inst (make-condition 'condition)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'standard-class)))
  t t nil)

(deftest condition-condition.4
  (closp
    (make-instance 'condition))
  t)


;;
;;  Condition Type WARNING
;;
(deftest warning-condition.1
  (closp
    (make-condition 'warning))
  t)

(deftest warning-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'warning)))
  (warning condition standard-object t))


;;
;;  Condition Type STYLE-WARNING
;;
(deftest style-warning-condition.1
  (closp
    (make-condition 'style-warning))
  t)

(deftest style-warning-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'style-warning)))
  (style-warning warning condition standard-object t))

(deftest style-warning-condition.3
  (handler-case
    (eval '(let (unused) (+ 10 20 30)))
    (style-warning () 'hit))
  hit)


;;
;;  Condition Type SERIOUS-CONDITION
;;
(deftest serious-condition-condition.1
  (closp
    (make-condition 'serious-condition))
  t)

(deftest serious-condition-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'serious-condition)))
  (serious-condition condition standard-object t))


;;
;;  Condition Type ERROR
;;
(deftest error-condition.1
  (closp
    (make-condition 'error))
  t)

(deftest error-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'error)))
  (error serious-condition condition standard-object t))


;;
;;  Condition Type CELL-ERROR
;;
(deftest cell-error-condition.1
  (closp
    (make-condition 'cell-error))
  t)

(deftest cell-error-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'cell-error)))
  (cell-error error serious-condition condition standard-object t))

(deftest cell-error-condition.3
  (cell-error-name
    (make-condition 'cell-error :name 'hello))
  hello)


;;
;;  Condition Type PARSE-ERROR
;;
(deftest parse-error-condition.1
  (closp
    (make-condition 'parse-error))
  t)

(deftest parse-error-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'parse-error)))
  (parse-error error serious-condition condition standard-object t))


;;
;;  Condition Type STORAGE-CONDITION
;;
(deftest storage-condition-condition.1
  (closp
    (make-condition 'storage-condition))
  t)

(deftest storage-condition-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'storage-condition)))
  (storage-condition serious-condition condition standard-object t))


;;
;;  Condition Type SIMPLE-ERROR
;;
(deftest simple-error-condition.1
  (closp
    (make-condition 'simple-error))
  t)

(deftest simple-error-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-error)))
  (simple-error simple-condition
    error serious-condition condition standard-object t))

(deftest simple-error-condition.3
  (let ((inst (make-condition
                'simple-error :format-control "aa" :format-arguments '(bb))))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)))
  "aa" (bb))

(deftest simple-error-condition.4
  (handler-case
    (error "AAA ~A" 10)
    (simple-error (c)
      (let ((x (simple-condition-format-control c))
            (y (simple-condition-format-arguments c)))
        (apply #'format nil x y))))
  "AAA 10")

(deftest simple-error-condition.5
  (handler-case
    (let (x)
      (cerror "AAA" "BBB" x))
    (simple-error () 'hit))
  hit)


;;
;;  Condition Type SIMPLE-CONDITION
;;
(deftest simple-condition-condition.1
  (closp
    (make-condition 'simple-condition))
  t)

(deftest simple-condition-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-condition)))
  (simple-condition condition standard-object t))

(deftest simple-condition-condition.3
  (let ((inst (make-condition
                'simple-condition :format-control "aa" :format-arguments '(bb))))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)))
  "aa" (bb))

(deftest simple-condition-condition.4
  (handler-case
    (signal "AAA" 10)
    (simple-condition (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "AAA" (10))


;;
;;  Condition Type SIMPLE-WARNING
;;
(deftest simple-warning-condition.1
  (closp
    (make-condition 'simple-warning))
  t)

(deftest simple-warning-condition.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-warning)))
  (simple-warning simple-condition warning condition standard-object t))

(deftest simple-warning-condition.3
  (let ((inst (make-condition
                'simple-warning :format-control "aa" :format-arguments '(bb))))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)))
  "aa" (bb))

(deftest simple-warning-condition.4
  (handler-case
    (warn "Hello" 10 20 30)
    (simple-warning (c)
      (values
        (simple-condition-format-control c)
        (simple-condition-format-arguments c))))
  "Hello" (10 20 30))


;;
;;  Conditions
;;
(deftest class-arithmetic-error.1
  (let ((inst (make-condition 'arithmetic-error)))
    (values
      (typep inst 't)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'arithmetic-error)))
  t t t t)

(deftest class-arithmetic-error.2
  (let ((inst (make-condition 'arithmetic-error :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest class-division-by-zero.1
  (let ((inst (make-condition 'division-by-zero)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'arithmetic-error)
      (typep inst 'division-by-zero)))
  t t t t t t)

(deftest class-division-by-zero.2
  (let ((inst (make-condition 'division-by-zero :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest class-floating-point-inexact.1
  (let ((inst (make-condition 'floating-point-inexact)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'arithmetic-error)
      (typep inst 'floating-point-inexact)))
  t t t t t t)

(deftest class-floating-point-inexact.2
  (let ((inst (make-condition 'floating-point-inexact :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest class-floating-point-invalid-operation.1
  (let ((inst (make-condition 'floating-point-invalid-operation)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'arithmetic-error)
      (typep inst 'floating-point-invalid-operation)))
  t t t t t t)

(deftest class-floating-point-invalid-operation.2
  (let ((inst (make-condition 'floating-point-invalid-operation
                              :operation 'aa :operands '(bb))))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa (bb))

(deftest class-floating-point-overflow.1
  (let ((inst (make-condition 'floating-point-overflow)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'arithmetic-error)
      (typep inst 'floating-point-overflow)))
  t t t t t t)

(deftest class-floating-point-overflow.2
  (let ((inst (make-condition 'floating-point-overflow :operation 'aa :operands 'bb)))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa bb)

(deftest class-floating-point-underflow.1
  (let ((inst (make-condition 'floating-point-underflow)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'arithmetic-error)
      (typep inst 'floating-point-underflow)))
  t t t t t t)

(deftest class-floating-point-underflow.2
  (let ((inst (make-condition 'floating-point-underflow :operation 'aa :operands 'bb)))
    (values
      (arithmetic-error-operation inst)
      (arithmetic-error-operands inst)))
  aa bb)

(deftest class-unbound-slot.1
  (let ((inst (make-condition 'unbound-slot)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'cell-error)
      (typep inst 'unbound-slot)))
  t t t t t t)

(deftest class-unbound-slot.2
  (let ((inst (make-condition 'unbound-slot :name 'aa :instance 'bb)))
    (values
      (cell-error-name inst)
      (unbound-slot-instance inst)))
  aa bb)

(deftest class-unbound-variable.1
  (let ((inst (make-condition 'unbound-variable)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'cell-error)
      (typep inst 'unbound-variable)))
  t t t t t t)

(deftest class-unbound-variable.2
  (cell-error-name
    (make-condition 'unbound-variable :name 'hello))
  hello)

(deftest class-undefined-function.1
  (let ((inst (make-condition 'undefined-function)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'cell-error)
      (typep inst 'undefined-function)))
  t t t t t t)

(deftest class-undefined-function.2
  (cell-error-name
    (make-condition 'undefined-function :name 'hello))
  hello)

(deftest class-control-error.1
  (let ((inst (make-condition 'control-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'control-error)))
  t t t t t)

(deftest class-stream-error.1
  (let ((inst (make-condition 'stream-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'stream-error)))
  t t t t t)

(deftest class-stream-error.2
  (stream-error-stream
    (make-condition 'stream-error :stream 'hello))
  hello)

(deftest class-end-of-file.1
  (let ((inst (make-condition 'end-of-file)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'stream-error)
      (typep inst 'end-of-file)))
  t t t t t t)

(deftest class-end-of-file.2
  (stream-error-stream
    (make-condition 'end-of-file :stream 'hello))
  hello)

(deftest class-file-error.1
  (let ((inst (make-condition 'file-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'file-error)))
  t t t t t)

(deftest class-file-error.2
  (file-error-pathname
    (make-condition 'file-error :pathname 'hello))
  hello)

(deftest class-package-error.1
  (let ((inst (make-condition 'package-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'package-error)))
  t t t t t)

(deftest class-package-error.2
  (package-error-package
    (make-condition 'package-error :package 'hello))
  hello)

(deftest class-print-not-readable.1
  (let ((inst (make-condition 'print-not-readable)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'print-not-readable)))
  t t t t t)

(deftest class-print-not-readable.2
  (print-not-readable-object
    (make-condition 'print-not-readable :object 'hello))
  hello)

(deftest class-program-error.1
  (let ((inst (make-condition 'program-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'program-error)))
  t t t t t)

(deftest class-reader-error.1
  (let ((inst (make-condition 'reader-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'stream-error)
      (typep inst 'parse-error)
      (typep inst 'reader-error)))
  t t t t t t t)

(deftest class-reader-error.2
  (stream-error-stream
    (make-condition 'reader-error :stream 'hello))
  hello)

(deftest class-type-error.1
  (let ((inst (make-condition 'type-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'type-error)))
  t t t t t)

(deftest class-type-error.2
  (let ((inst (make-condition 'type-error :datum 'aa :expected-type 'integer)))
    (values
      (type-error-datum inst)
      (type-error-expected-type inst)))
  aa integer)

(deftest class-simple-type-error.1
  (let ((inst (make-condition 'simple-type-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'type-error)
      (typep inst 'simple-condition)
      (typep inst 'simple-type-error)))
  t t t t t t t)

(deftest class-simple-type-error.2
  (let ((inst (make-condition 'simple-type-error
                              :format-control "aa" :format-arguments '(bb)
                              :datum 'cc :expected-type 'integer)))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)
      (type-error-datum inst)
      (type-error-expected-type inst)))
  "aa" (bb) cc integer)

