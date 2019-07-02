;;
;;  ANSI COMMON LISP: 9. Conditions
;;
(deftest class-condition.1
  (lisp-system::closp
    (make-condition 'condition))
  t)

(deftest class-condition.2
  (let ((inst (make-condition 'condition)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'standard-class)))
  t t nil)

(deftest class-condition.3
  (lisp-system::closp
    (make-instance 'condition))
  t)

(deftest class-condition.4
  (let ((inst (make-instance 'condition)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'standard-class)))
  t t nil)

(deftest class-serious-condition.1
  (let ((inst (make-condition 'serious-condition)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)))
  t t t)

(deftest class-error.1
  (let ((inst (make-condition 'error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)))
  t t t t)

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

(deftest class-cell-error.1
  (let ((inst (make-condition 'cell-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'cell-error)))
  t t t t t)

(deftest class-cell-error.2
  (cell-error-name
    (make-condition 'cell-error :name 'hello))
  hello)

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

(deftest class-parse-error.1
  (let ((inst (make-condition 'parse-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'error)
      (typep inst 'parse-error)))
  t t t t t)

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

(deftest class-simple-condition.1
  (let ((inst (make-condition 'simple-condition)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'simple-condition)))
  t t t)

(deftest class-simple-condition.2
  (let ((inst (make-condition
                'simple-condition :format-control "aa" :format-arguments '(bb))))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)))
  "aa" (bb))

(deftest class-simple-error.1
  (let ((inst (make-condition 'simple-error)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'simple-condition)
      (typep inst 'simple-error)))
  t t t t)

(deftest class-simple-error.2
  (let ((inst (make-condition
                'simple-error :format-control "aa" :format-arguments '(bb))))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)))
  "aa" (bb))

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

(deftest class-warning.1
  (let ((inst (make-condition 'warning)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'warning)))
  t t t)

(deftest class-simple-warning.1
  (let ((inst (make-condition 'simple-warning)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'warning)
      (typep inst 'simple-condition)
      (typep inst 'simple-warning)))
  t t t t t)

(deftest class-simple-warning.2
  (let ((inst (make-condition
                'simple-warning :format-control "aa" :format-arguments '(bb))))
    (values
      (simple-condition-format-control inst)
      (simple-condition-format-arguments inst)))
  "aa" (bb))

(deftest class-storage-condition.1
  (let ((inst (make-condition 'storage-condition)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'serious-condition)
      (typep inst 'storage-condition)))
  t t t t)

(deftest class-style-warning.1
  (let ((inst (make-condition 'style-warning)))
    (values
      (typep inst 't)
      (typep inst 'condition)
      (typep inst 'warning)
      (typep inst 'style-warning)))
  t t t t)

