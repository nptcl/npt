;;
;;  ANSI COMMON LISP: 3. Evaluation and Compilation
;;
(defvar *load-time-value-result*)
(defvar *load-time-value-test*)

;;
;;  Special Operator LOAD-TIME-VALUE
;;
(deftest load-time-value-eval.1
  (load-time-value 10)
  10)

(deftest load-time-value-eval.2
  (load-time-value "Hello" t)
  "Hello")

(deftest load-time-value-eval.3
  (load-time-value #\A nil)
  #\A)

(deftest load-time-value-eval.4
  (load-time-value (+ 10 20 30) :hello)
  60)

(deftest load-time-value-eval.5
  (+ (load-time-value 10)
     (load-time-value 20))
  30)

(deftest load-time-value-eval.6
  (values
    (eql (load-time-value 10) 10)
    (eql (load-time-value 20) 20)
    (eql (load-time-value 30) 30)
    (equal (load-time-value "Hello") "Hello")
    (equal (load-time-value (concatenate 'string "AA" "BB" "CC")) "AABBCC"))
  t t t t t)

(deftest load-time-value-eval.7
  (+ (eval '(load-time-value (+ 10 20)))
     (load-time-value 40))
  70)

(deftest-error load-time-value-eval.8
  (eval '(let ((x 10))
           (declare (ignorable x))
           (load-time-value (+ x 20)))))

(defvar *load-time-value-test-1* nil)
(deftest load-time-value-eval.9
  (eval '(progn
           (push (load-time-value
                   (progn
                     (push 10 *load-time-value-test-1*)
                     20))
                 *load-time-value-test-1*)
           *load-time-value-test-1*))
  (20 10))

(defvar *load-time-value-test-2* 10)
(deftest load-time-value-eval.10
  (eval '(let (x)
           (dotimes (i 20 x)
             (setq x (load-time-value
                       (incf *load-time-value-test-2* 1))))))
  11)

(deftest load-time-value-eval.11
  (progn
    (load #p"test/rteval-file1.lisp")
    (prog1
      (funcall *load-time-value-test*)
      (makunbound '*load-time-value-test*)))
  20)

(deftest-error load-time-value-error.1
  (eval '(load-time-value)))

(deftest-error load-time-value-error.2
  (eval '(load-time-value 10 t t)))


;;
;;  compile
;;
(deftest load-time-value-compile.1
  (funcall
    (compile nil '(lambda () (load-time-value 10))))
  10)

(deftest load-time-value-compile.2
  (funcall
    (compile nil '(lambda () (load-time-value (+ 10 20 30)))))
  60)


;;
;;  compile-file
;;
(defun compile-eval-execute (x input output &optional (call #'prin1))
  ;; write
  (with-open-file (stream input :direction :output)
    (funcall call x stream))
  (file-position input :start)
  ;; compile
  (compile-file input :output-file output)
  (file-position output :start)
  ;; load
  (let (*load-time-value-result*)
    (load output :type :fasl)
    *load-time-value-result*))

(defun compile-eval (x)
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (compile-eval-execute x input output))))

(defun compile-setq (x)
  (compile-eval
    `(setq *load-time-value-result* ,x)))

(defun compile-string (x)
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (compile-eval-execute
        (format nil "(setq *load-time-value-result* ~A)" x)
        input output #'princ))))


(defun compile-string-list-execute (list input output)
  ;; write
  (with-open-file (stream input :direction :output)
    (dolist (x list)
      (princ x stream)))
  (file-position input :start)
  ;; compile
  (compile-file input :output-file output)
  (file-position output :start)
  ;; load
  (let (*load-time-value-result*)
    (load output :type :fasl)
    *load-time-value-result*))

(defun compile-string-list (&rest args)
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (compile-string-list-execute args input output))))


;;
;;  compile-file
;;
(deftest load-time-value-load.1
  (compile-setq 999)
  999)

(deftest load-time-value-load.2
  (let ((x (compile-string "(quote #.(make-symbol \"HELLO\"))")))
    (values
      (symbolp x)
      (symbol-name x)
      (symbol-package x)))
  t "HELLO" nil)

(deftest load-time-value-load.3
  (let ((cons (compile-string
                "(quote (#.(make-symbol \"AAA\") #.(make-symbol \"BBB\")))")))
    (destructuring-bind (x y) cons
      (values
        (symbolp x)
        (symbolp y)
        (not (eq x y))
        (equal (symbol-name x) "AAA")
        (equal (symbol-name y) "BBB")
        (null (symbol-package x))
        (null (symbol-package y)))))
  t t t t t t t)

(deftest load-time-value-load.4
  (compile-setq '(load-time-value 11))
  11)

(deftest load-time-value-load.5
  (compile-setq '(load-time-value
                   (load-time-value 22)))
  22)

(deftest load-time-value-load.6
  (compile-string-list
    "(defvar *load-time-value-load-6* 10)"
    "(incf *load-time-value-load-6* 20)"
    "(setq *load-time-value-result* (+ *load-time-value-load-6* 30))")
  60)

(defstruct load-time-value-load-7 aaa bbb ccc)
(defmethod make-load-form ((x load-time-value-load-7) &optional env)
  (declare (ignore x env))
  `(make-load-time-value-load-7 :aaa 999))

(deftest load-time-value-load.7
  (typep
    (compile-string "#.(make-load-time-value-load-7)")
    'load-time-value-load-7)
  t)

(deftest load-time-value-load.8
  (slot-value
    (compile-string "#.(make-load-time-value-load-7)")
    'aaa)
  999)

(defstruct load-time-value-load-9 aaa bbb ccc)
(defmethod make-load-form ((x load-time-value-load-9) &optional env)
  (make-load-form-saving-slots x :environment env))

(deftest load-time-value-load.9
  (typep
    (compile-string "#.(make-load-time-value-load-9 :aaa 10 :bbb 20)")
    'load-time-value-load-9)
  t)

(deftest load-time-value-load.10
  (slot-value
    (compile-string "#.(make-load-time-value-load-9 :aaa 10 :bbb 20)")
    'aaa)
  10)


;;
;;  fasl
;;
(deftest load-time-value-fasl.1
  (with-open-stream (output (lisp-system:make-memory-io-stream))
    (compile-file #p"test/rteval-file2.lisp" :output-file output)
    (file-position output :start)
    (load output :type :fasl)
    (prog1
      (funcall *load-time-value-test*)
      (makunbound '*load-time-value-test*)))
  30)

(deftest load-time-value-fasl.2
  (compile-setq
    '(load-time-value 10))
  10)

(deftest load-time-value-fasl.3
  (compile-setq
    '(load-time-value (+ 10 20 30)))
  60)

(deftest load-time-value-fasl.4
  (compile-setq
    '(+ (load-time-value (+ 10 20))
        (load-time-value (and 10 20 30))
        100))
  160)

(deftest load-time-value-fasl.5
  (let (*load-time-value-result*)
    (declare (special *load-time-value-result*))
    (with-open-stream (input (lisp-system:make-memory-io-stream))
      (with-open-stream (output (lisp-system:make-memory-io-stream))
        (with-open-file (stream input :direction :output)
          (prin1 '(eval-when (:compile-toplevel :load-toplevel :execute)
                    (setq *load-time-value-result*
                          (+ (load-time-value (* 10 20 30)))))
                 stream))
        (file-position input :start)
        (compile-file input :output-file output))
      *load-time-value-result*))
  6000)

