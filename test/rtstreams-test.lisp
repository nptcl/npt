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
;;
;;
(deftest make-synonym-stream.1
  (typep (make-synonym-stream 'hello) 'synonym-stream)
  t)

(deftest synonym-stream-symbol.1
  (synonym-stream-symbol
    (make-synonym-stream 'hello))
  hello)

(deftest make-broadcast-stream.1
  (typep (make-broadcast-stream) 'broadcast-stream)
  t)

(deftest make-broadcast-stream.2
  (typep (make-broadcast-stream
           *standard-output*
           *error-output*) 'broadcast-stream)
  t)

(deftest-error make-broadcast-stream.3
  (typep (make-broadcast-stream
           *standard-input*
           *standard-output*) 'broadcast-stream)
  type-error)

(deftest broadcast-stream-streams.1
  (broadcast-stream-streams
    (make-broadcast-stream))
  nil)

(deftest broadcast-stream-streams.2
  (mapcar
    #'output-stream-p
    (broadcast-stream-streams
      (make-broadcast-stream
        *standard-output*
        *error-output*)))
  (t t))

(deftest make-two-way-stream.1
  (typep (make-two-way-stream
           *standard-input*
           *standard-output*)
         'two-way-stream)
  t)

(deftest-error make-two-way-stream.2
  (make-two-way-stream *error-output* *standard-output*)
  type-error)

(deftest-error make-two-way-stream.3
  (make-two-way-stream *standard-input* *standard-input*)
  type-error)

(deftest two-way-stream-input-stream.1
  (eq *standard-input*
      (two-way-stream-input-stream
        (make-two-way-stream *standard-input* *standard-output*)))
  t)

(deftest two-way-stream-output-stream.1
  (eq *standard-output*
      (two-way-stream-output-stream
        (make-two-way-stream *standard-input* *standard-output*)))
  t)

(deftest make-echo-stream.1
  (typep (make-echo-stream
           *standard-input*
           *standard-output*)
         'echo-stream)
  t)

(deftest-error make-echo-stream.2
  (make-echo-stream *error-output* *standard-output*)
  type-error)

(deftest-error make-echo-stream.3
  (make-echo-stream *standard-input* *standard-input*)
  type-error)

(deftest echo-stream-input-stream.1
  (eq *standard-input*
      (echo-stream-input-stream
        (make-echo-stream *standard-input* *standard-output*)))
  t)

(deftest echo-stream-output-stream.1
  (eq *standard-output*
      (echo-stream-output-stream
        (make-echo-stream *standard-input* *standard-output*)))
  t)

(deftest make-concatenated-stream.1
  (typep (make-concatenated-stream) 'concatenated-stream)
  t)

(deftest make-concatenated-stream.2
  (typep (make-concatenated-stream
           *standard-input*
           *terminal-io*) 'concatenated-stream)
  t)

(deftest-error make-concatenated-stream.3
  (make-concatenated-stream *standard-input* *standard-output*)
  type-error)

(deftest concatenated-stream-streams.1
  (concatenated-stream-streams
    (make-concatenated-stream))
  nil)

(deftest concatenated-stream-streams.2
  (mapcar
    #'input-stream-p
    (concatenated-stream-streams
      (make-concatenated-stream
        *standard-input*
        *terminal-io*)))
  (t t))

(deftest make-string-input-stream.1
  (typep (make-string-input-stream "Hello") 'string-stream)
  t)

(deftest make-string-input-stream.2
  (let ((inst (make-string-input-stream "Hello")))
    (values
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)))
  #\H #\e #\l #\l #\o :eof)

(deftest make-string-input-stream.3
  (let ((inst (make-string-input-stream "Hello" :start 1)))
    (values
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)))
  #\e #\l #\l #\o :eof :eof)

(deftest make-string-input-stream.4
  (let ((inst (make-string-input-stream "Hello" :start 1 :end 3)))
    (values
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)))
  #\e #\l :eof :eof :eof :eof)

(deftest make-string-input-stream.5
  (read-char
    (make-string-input-stream "Hello" :start 5)
    nil :eof)
  :eof)

(deftest make-string-input-stream.6
  (read-char
    (make-string-input-stream "Hello" :start 5 :end 5)
    nil :eof)
  :eof)

(deftest-error make-string-input-stream.7
  (make-string-input-stream "Hello" :start 6))

(deftest-error make-string-input-stream.8
  (make-string-input-stream "Hello" :end 6))

(deftest-error make-string-input-stream.9
  (make-string-input-stream "Hello" :start 4 :end 3))

(deftest make-string-output-stream.1
  (typep (make-string-output-stream) 'string-stream)
  t)

(deftest make-string-output-stream.2
  (let ((inst (make-string-output-stream :element-type 'character)))
    (format inst "Hello")
    (get-output-stream-string inst))
  "Hello")

(deftest make-string-output-stream.3
  (let ((inst (make-string-output-stream :element-type 'character)))
    (get-output-stream-string inst))
  "")

(deftest make-string-output-stream.4
  (streamp
    (make-string-output-stream :element-type 'standard-char))
  t)

(deftest-error make-string-output-stream.5
  (make-string-output-stream :element-type 'symbol))

(deftest get-output-stream-string.1
  (with-open-stream (stream (make-string-output-stream))
    (format stream "Hello")
    (format stream "Hello")
    (get-output-stream-string stream))
  "HelloHello")

(deftest end-input-stream.1
  (let ((inst (make-string-input-stream "Hello")))
    (lisp-system::end-input-stream inst))
  0)

(deftest end-input-stream.2
  (let ((inst (make-string-input-stream "Hello")))
    (values
      (read-char inst)
      (read-char inst)
      (read-char inst)
      (lisp-system::end-input-stream inst)))
  #\H #\e #\l 3)

(deftest with-input-from-string.1
  (with-input-from-string (inst "Hello")
    (streamp inst))
  t)

(deftest with-input-from-string.2
  (with-input-from-string (inst "Hello")
    (values
      (read-char inst)
      (read-char inst)
      (read-char inst)))
  #\H #\e #\l)

(deftest with-input-from-string.3
  (with-input-from-string (inst "Hello" :start 2)
    (values
      (read-char inst)
      (read-char inst)
      (read-char inst)))
  #\l #\l #\o)

(deftest with-input-from-string.4
  (with-input-from-string (inst "Hello" :end 2)
    (values
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)))
  #\H #\e :eof)

(deftest with-input-from-string.5
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos)
        (streamp inst))
      pos))
  t 0)

(deftest with-input-from-string.6
  (let ((pos 'error))
    (with-input-from-string (inst "Hello" :index pos)
      (values (streamp inst) pos)))
  t error)

(deftest with-input-from-string.7
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos)
        (list
          (read-char inst)
          (read-char inst)
          (read-char inst)))
      pos))
  (#\H #\e #\l) 3)

(deftest with-input-from-string.8
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :start 2 :index pos)
        (list
          (read-char inst)
          (read-char inst)
          (read-char inst)))
      pos))
  (#\l #\l #\o) 5)

(deftest with-input-from-string.9
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos :end 2)
        (list
          (read-char inst nil :eof)
          (read-char inst nil :eof)
          (read-char inst nil :eof)))
      pos))
  (#\H #\e :eof) 2)

(deftest with-output-to-string.1
  (with-output-to-string (inst)
    (format inst "Hello"))
  "Hello")

(deftest with-output-to-string.2
  (with-output-to-string (inst nil :element-type 'character)
    (format inst "a")
    (format inst "b")
    (format inst "C"))
  "abC")

(deftest with-output-to-string.3
  (with-extend-to-string
    (inst array)
    (format inst "Hello")
    :aaa)
  :aaa)

(deftest with-output-to-string.4
  (with-extend-to-string
    (inst array)
    (format inst "Hello")
    array)
  "Hello")


;;
;;  open
;;
(deftest with-open-file.1
  (with-delete-temp-file
    (with-open-file (inst *file* :direction :output)
      (streamp inst)))
  t)

(deftest close.1
  (let ((inst (open *file* :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)))
    (format inst "Hello")
    (close inst :abort t)
    (probe-file *file*))
  nil)

(deftest close.2
  (let ((inst (open *file* :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)))
    (format inst "Hello")
    (close inst)
    (setq inst (open *file* :direction :input))
    (close inst :abort t)
    (probe-file *file*))
  t)


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

