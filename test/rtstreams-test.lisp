;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  Function INPUT-STREAM-P
;;
(deftest input-stream-p.1
  (input-stream-p
    *standard-input*)
  t)

(deftest input-stream-p.2
  (input-stream-p
    *error-output*)
  nil)

(deftest input-stream-p.3
  (input-stream-p
    *terminal-io*)
  t)

(deftest-error input-stream-p-error.1
  (eval '(input-stream-p 10))
  type-error)

(deftest-error! input-stream-p-error.2
  (eval '(input-stream-p)))

(deftest-error! input-stream-p-error.3
  (eval '(input-stream-p *standard-input* nil)))

;;  ANSI Common Lisp
(deftest input-stream-p-test.1
  (input-stream-p *standard-input*)
  t)

(deftest input-stream-p-test.2
  (input-stream-p *terminal-io*)
  t)

(deftest input-stream-p-test.3
  (input-stream-p (make-string-output-stream))
  nil)


;;
;;  Function OUTPUT-STREAM-P
;;
(deftest output-stream-p.1
  (output-stream-p
    *standard-output*)
  t)

(deftest output-stream-p.2
  (output-stream-p
    *standard-input*)
  nil)

(deftest output-stream-p.3
  (output-stream-p
    *terminal-io*)
  t)

(deftest-error output-stream-p-error.1
  (eval '(output-stream-p 10))
  type-error)

(deftest-error! output-stream-p-error.2
  (eval '(output-stream-p)))

(deftest-error! output-stream-p-error.3
  (eval '(output-stream-p *standard-output* nil)))

;;  ANSI Common Lisp
(deftest output-stream-p-test.1
  (output-stream-p *standard-output*)
  t)

(deftest output-stream-p-test.2
  (output-stream-p *terminal-io*)
  t)

(deftest output-stream-p-test.3
  (output-stream-p (make-string-input-stream "jr"))
  nil)


;;
;;  Function INTERACTIVE-STREAM-P
;;
(deftest interactive-stream-p.1
  (interactive-stream-p
    *standard-input*)
  t)

(deftest interactive-stream-p.2
  (interactive-stream-p
    *standard-output*)
  t)

(deftest interactive-stream-p.3
  (interactive-stream-p
    *error-output*)
  t)

(deftest-error interactive-stream-p-error.1
  (eval '(interactive-stream-p 10))
  type-error)

(deftest-error! interactive-stream-p-error.2
  (eval '(interactive-stream-p)))

(deftest-error! interactive-stream-p-error.3
  (eval '(interactive-stream-p *standard-output* nil)))


;;
;;  Function OPEN-STREAM-P
;;
(deftest open-stream-p.1
  (open-stream-p
    *standard-input*)
  t)

(deftest open-stream-p.2
  (open-stream-p
    *standard-output*)
  t)

(deftest open-stream-p.3
  (let (x)
    (with-output-to-string (stream)
      (setq x (open-stream-p stream)))
    x)
  t)

(deftest open-stream-p.4
  (let (x)
    (with-output-to-string (stream)
      (setq x stream))
    (open-stream-p x))
  nil)

(deftest open-stream-p.5
  (let (x)
    (with-temp-file
      (with-open-file (stream *file* :direction :input)
        (setq x (open-stream-p stream))))
    x)
  t)

(deftest open-stream-p.6
  (let (x)
    (with-temp-file
      (with-open-file (stream *file* :direction :input)
        (setq x stream)))
    (open-stream-p x))
  nil)

(deftest open-stream-p.7
  (with-input-from-string (stream "Hello")
    (open-stream-p stream))
  t)

(deftest open-stream-p.8
  (let (x)
    (with-input-from-string (stream "Hello")
      (setq x stream))
    (open-stream-p x))
  nil)

(deftest open-stream-p.9
  (with-open-stream (stream (make-broadcast-stream))
    (open-stream-p stream))
  t)

(deftest open-stream-p.10
  (let (x)
    (with-open-stream (stream (make-broadcast-stream))
      (setq x stream))
    (open-stream-p x))
  nil)

(deftest-error open-stream-p-error.1
  (eval '(open-stream-p 10))
  type-error)

(deftest-error! open-stream-p-error.2
  (eval '(open-stream-p)))

(deftest-error! open-stream-p-error.3
  (eval '(open-stream-p *standard-output* nil)))


;;
;;  Function STREAM-ELEMENT-TYPE
;;
(deftest stream-element-type.1
  (stream-element-type *standard-input*)
  character)

(deftest stream-element-type.2
  (stream-element-type *standard-output*)
  character)

(deftest stream-element-type.3
  (with-open-file (s *file* :element-type '(integer 0 1)
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (stream-element-type s))
  (unsigned-byte 8))

(deftest-error stream-element-type-error.1
  (eval '(stream-element-type 10))
  type-error)

(deftest-error! stream-element-type-error.2
  (eval '(stream-element-type)))

(deftest-error! stream-element-type-error.3
  (eval '(stream-element-type *standard-output* nil)))


;;
;;  Function STREAM-EXTERNAL-FORMAT
;;
(deftest stream-external-format.1
  (stream-external-format *standard-input*)
  lisp-system::utf-8)

(deftest stream-external-format.2
  (stream-external-format *standard-output*)
  lisp-system::utf-8)

(deftest stream-external-format.3
  (with-open-file (s *file* :element-type '(integer 0 1)
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
    (stream-external-format s))
  :default)

(deftest stream-external-format.4
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (s io :external-format 'utf-16le)
      (stream-external-format s)))
  lisp-system::utf-16le)

(deftest-error stream-external-format-error.1
  (eval '(stream-external-format 10))
  type-error)

(deftest-error! stream-external-format-error.2
  (eval '(stream-external-format)))

(deftest-error! stream-external-format-error.3
  (eval '(stream-external-format *standard-output* nil)))


;;
;;  Macro WITH-OPEN-STREAM
;;
(deftest with-open-stream.1
  (with-open-stream (x (make-string-input-stream "Hello"))
    (read-line x))
  "Hello" t)

(deftest with-open-stream.2
  (let (stream)
    (with-open-stream (x (make-string-input-stream "Hello"))
      (setq stream x))
    (values
      (streamp stream)
      (open-stream-p stream)
      (input-stream-p stream)))
  t nil t)

(deftest with-open-stream.3
  (with-open-stream (x (make-string-input-stream "Hello"))
    (declare (special x))
    (read-line (symbol-value 'x)))
  "Hello" t)

(deftest with-open-stream.4
  (with-open-stream (s (make-string-input-stream "1 2 3 4"))
    (+ (read s) (read s) (read s)))
  6)

(deftest-error with-open-stream-error.1
  (eval '(with-open-stream (10 (make-string-output-stream)))))

(deftest-error with-open-stream-error.2
  (eval '(with-open-stream (var))))

(deftest-error with-open-stream-error.3
  (eval '(with-open-stream (var (make-string-output-stream) "Hello"))))

(deftest-error with-open-stream-error.4
  (eval '(with-open-stream)))


;;
;;  variables
;;
(deftest special-standard-input.1
  (let ((stream *standard-input*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t t nil t)

(deftest special-standard-output.1
  (let ((stream *standard-output*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t nil t t)

(deftest special-error-output.1
  (let ((stream *error-output*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t nil t t)

(deftest special-trace-output.1
  (let ((stream *trace-output*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t t t t)

(deftest special-debug-io.1
  (let ((stream *debug-io*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t t t t)

(deftest special-query-io.1
  (let ((stream *query-io*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t t t t)

(deftest special-terminal-io.1
  (let ((stream *terminal-io*))
    (values
      (streamp stream)
      (input-stream-p stream)
      (output-stream-p stream)
      (interactive-stream-p stream)))
  t t t t)


;;
;;  special variables
;;
(deftest debug-io-special.1
  (lisp-system::specialp '*debug-io*)
  t)

(deftest error-output-special.1
  (lisp-system::specialp '*error-output*)
  t)

(deftest query-io-special.1
  (lisp-system::specialp '*query-io*)
  t)

(deftest standard-input-special.1
  (lisp-system::specialp '*standard-input*)
  t)

(deftest standard-output-special.1
  (lisp-system::specialp '*standard-output*)
  t)

(deftest trace-output-special.1
  (lisp-system::specialp '*trace-output*)
  t)

(deftest terminal-io-special.1
  (lisp-system::specialp '*terminal-io*)
  t)

