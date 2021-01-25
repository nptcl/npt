;;
;;  SysCall
;;
(defun load-string (&rest args)
  (with-open-stream (memory (lisp-system:make-memory-io-stream))
    (with-open-file (output memory :direction :output)
      (dolist (x args)
        (write-line x output)
        (terpri output)))
    (with-open-file (input memory :direction :input)
      (load input))))

(defvar *load-memory-stream-test*)
(deftest load-memory-stream.1
  (progn
    (makunbound '*load-memory-stream-test*)
    (load-string
      "(setq *load-memory-stream-test* 100)"
      "(incf *load-memory-stream-test* 200)")
    *load-memory-stream-test*)
  300)

(defun ascii-memory-input-stream (&rest args)
  (let ((x (lisp-system:make-memory-io-stream)))
    (with-open-file (stream x :direction :output)
      (dolist (str args)
        (write-line str stream)
        (terpri stream)))
    (setf (lisp-system:memory-stream-p x) :input)
    (file-position x :start)
    x))

(deftest load-memory-stream.2
  (with-open-stream (input (ascii-memory-input-stream
                             "(setq *load-memory-stream-test* 300)"
                             "(incf *load-memory-stream-test* 400)"))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*load-memory-stream-test*)
      (compile-file input :output-file output)
      (with-open-file (stream output :direction :input
                              :element-type '(unsigned-byte 8))
        (load stream :type 'fasl)))
    *load-memory-stream-test*)
  700)

(deftest load-memory-stream.3
  (with-open-stream (input (ascii-memory-input-stream
                             "(setq *load-memory-stream-test* 300)"
                             "(incf *load-memory-stream-test* 400)"))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*load-memory-stream-test*)
      (compile-file input :output-file output)
      (file-position output :start)
      (load output :type 'fasl))
    *load-memory-stream-test*)
  700)


;;
;;  do-tests
;;
(do-tests :test t)

