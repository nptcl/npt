;;
;;  ANSI COMMON LISP: 21. Streams
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
(deftest open.1
  (with-delete-temp-file
    (let ((inst (open *file* :direction :output)))
      (prog1
        (streamp inst)
        (close inst))))
  t)

(deftest open.2
  (with-delete-temp-file
    (let ((inst (open *file* :direction :output)))
      (format inst "Hello")
      (close inst))
    (let ((inst (open *file* :direction :input)))
      (values (read-line inst))))
  "Hello")

(deftest open.3
  (with-delete-temp-file
    (let ((inst (open *file* :direction :output)))
      (format inst "Hello")
      (close inst))
    (let ((inst (open *file* :direction :output :if-exists :supersede)))
      (format inst "abc")
      (close inst))
    (let ((inst (open *file* :direction :input)))
      (values (read-line inst))))
  "abc")

(deftest with-open-file.1
  (with-delete-temp-file
    (with-open-file (inst *file* :direction :output)
      (streamp inst)))
  t)


;;
;;  input-stream-p
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

(deftest input-stream-p-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (input-stream-p stream)))
  t)

(deftest input-stream-p-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (input-stream-p stream)))
  nil)

(deftest input-stream-p-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (input-stream-p stream)))
  t)

(deftest input-stream-p-broadcast.1
  (with-open-stream (inst (make-broadcast-stream))
    (input-stream-p inst))
  nil)

(deftest input-stream-p-broadcast.2
  (with-open-stream (inst (make-broadcast-stream *terminal-io*))
    (input-stream-p inst))
  nil)

(deftest input-stream-p-broadcast.3
  (with-open-stream (inst (make-broadcast-stream *standard-output*))
    (input-stream-p inst))
  nil)

(deftest input-stream-p-concatenated.1
  (with-open-stream (inst (make-concatenated-stream))
    (input-stream-p inst))
  t)

(deftest input-stream-p-echo.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (input-stream-p inst))
  t)

(deftest input-stream-p-synonym.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (input-stream-p inst))))
  t)

(deftest input-stream-p-synonym.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (input-stream-p inst))))
  nil)

(deftest input-stream-p-two-way.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (input-stream-p inst))
  t)

(deftest input-stream-p-input-string.1
  (with-input-from-string (stream "Hello")
    (input-stream-p stream))
  t)

(deftest input-stream-p-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (input-stream-p stream)))
    result)
  nil)

(deftest input-stream-p-extend-string.1
  (with-extend-to-string
    (inst array)
    (input-stream-p inst))
  nil)


;;
;;  output-stream-p
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

(deftest output-stream-p-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (output-stream-p stream)))
  nil)

(deftest output-stream-p-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (output-stream-p stream)))
  t)

(deftest output-stream-p-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (output-stream-p stream)))
  t)

(deftest output-stream-p-broadcast.1
  (with-open-stream (inst (make-broadcast-stream))
    (output-stream-p inst))
  t)

(deftest output-stream-p-broadcast.2
  (with-open-stream (inst (make-broadcast-stream *terminal-io*))
    (output-stream-p inst))
  t)

(deftest output-stream-p-broadcast.3
  (with-open-stream (inst (make-broadcast-stream *standard-output*))
    (output-stream-p inst))
  t)

(deftest output-stream-p-concatenated.1
  (with-open-stream (inst (make-concatenated-stream))
    (output-stream-p inst))
  nil)

(deftest output-stream-p-echo.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (output-stream-p inst))
  t)

(deftest output-stream-p-synonym.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (output-stream-p inst))))
  nil)

(deftest output-stream-p-synonym.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (output-stream-p inst))))
  t)

(deftest output-stream-p-two-way.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (output-stream-p inst))
  t)

(deftest output-stream-p-input-string.1
  (with-input-from-string (stream "Hello")
    (output-stream-p stream))
  nil)

(deftest output-stream-p-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (output-stream-p stream)))
    result)
  t)

(deftest output-stream-p-extend-string.1
  (with-extend-to-string
    (inst array)
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
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

(deftest interactive-stream-p-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (interactive-stream-p stream)))
  nil)

(deftest interactive-stream-p-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (interactive-stream-p stream)))
  nil)

(deftest interactive-stream-p-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (interactive-stream-p stream)))
  nil)

(deftest interactive-stream-p-broadcast.1
  (with-open-stream (inst (make-broadcast-stream))
    (interactive-stream-p inst))
  nil)

(deftest interactive-stream-p-broadcast.2
  (with-open-stream (inst (make-broadcast-stream *query-io*))
    (interactive-stream-p inst))
  nil)

(deftest interactive-stream-p-broadcast.3
  (with-open-stream (inst (make-broadcast-stream *standard-output*))
    (interactive-stream-p inst))
  nil)

(deftest interactive-stream-p-concatenated.1
  (with-open-stream (inst (make-concatenated-stream))
    (interactive-stream-p inst))
  nil)

(deftest interactive-stream-p-echo.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (interactive-stream-p inst))
  nil)

(deftest interactive-stream-p-synonym.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (interactive-stream-p inst))))
  nil)

(deftest interactive-stream-p-synonym.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (interactive-stream-p inst))))
  nil)

(deftest interactive-stream-p-synonym.3
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (interactive-stream-p stream))
  t)

(deftest interactive-stream-p-two-way.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (interactive-stream-p inst))
  t)

(deftest interactive-stream-p-input-string.1
  (with-input-from-string (stream "Hello")
    (interactive-stream-p stream))
  nil)

(deftest interactive-stream-p-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (interactive-stream-p stream)))
    result)
  nil)

(deftest interactive-stream-p-extend-string.1
  (with-extend-to-string
    (inst array)
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
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
  (open-stream-p
    (open #p"test/empty.file" :direction :probe))
  nil)

(deftest open-stream-p-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (open-stream-p stream)))
  t)

(deftest open-stream-p-file.2
  (with-temp-file
    (let ((stream (open *file* :direction :input)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest open-stream-p-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (open-stream-p stream)))
  t)

(deftest open-stream-p-file.4
  (with-temp-file
    (let ((stream (open *file* :direction :output :if-exists :supersede)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest open-stream-p-file.5
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (open-stream-p stream)))
  t)

(deftest open-stream-p-file.6
  (with-temp-file
    (let ((stream (open *file* :direction :io :if-exists :supersede)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest open-stream-p-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (open-stream-p stream))
  t)

(deftest open-stream-p-broadcast.2
  (let ((stream (make-broadcast-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest open-stream-p-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (open-stream-p stream))
  t)

(deftest open-stream-p-concatenated.2
  (let ((stream (make-concatenated-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest open-stream-p-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (open-stream-p stream))
  t)

(deftest open-stream-p-echo.2
  (let ((stream (make-echo-stream *standard-input* *standard-output*)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest open-stream-p-synonym.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)))
  t t)

(deftest open-stream-p-synonym.2
  (let ((stream (make-synonym-stream '*standard-input*)))
    (close stream)
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)))
  nil t)

(deftest open-stream-p-two-way.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (open-stream-p stream))
  t)

(deftest open-stream-p-two-way.2
  (let ((stream (make-two-way-stream *standard-input* *standard-output*)))
    (close stream)
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)
      (open-stream-p *standard-output*)))
  nil t t)

(deftest open-stream-p-input-string.1
  (with-input-from-string (stream "Hello")
    (open-stream-p stream))
  t)

(deftest open-stream-p-input-string.2
  (let ((stream (make-string-input-stream "Hello")))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest open-stream-p-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (open-stream-p stream)))
    result)
  t)

(deftest open-stream-p-output-string.2
  (let ((stream (make-string-output-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest open-stream-p-extend-string.1
  (with-extend-to-string
    (inst array)
    (open-stream-p inst))
  t)

(deftest open-stream-p-extend-string.2
  (let (stream)
    (with-extend-to-string
      (inst array)
      (setq stream inst))
    (open-stream-p stream))
  nil)


;;
;;  streamp
;;
(deftest streamp.1
  (streamp *standard-input*)
  t)

(deftest streamp.2
  (streamp 1)
  nil)

(deftest streamp-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (streamp stream)))
  t)

(deftest streamp-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (streamp stream)))
  t)

(deftest streamp-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (streamp stream)))
  t)

(deftest streamp-file.4
  (with-temp-file
    (let ((stream (open *file* :direction :input)))
      (close stream)
      (streamp stream)))
  t)

(deftest streamp-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (streamp stream))
  t)

(deftest streamp-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (streamp stream))
  t)

(deftest streamp-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (streamp stream))
  t)

(deftest streamp-synonym.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (streamp stream))
  t)

(deftest streamp-two-way.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (streamp stream))
  t)
(deftest streamp-input-string.1
  (with-input-from-string (stream "Hello")
    (streamp stream))
  t)

(deftest streamp-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (streamp stream)))
    result)
  t)

(deftest streamp-extend-string.1
  (with-extend-to-string
    (inst array)
    (streamp inst))
  t)


;;
;;  stream-element-type
;;
(deftest stream-element-type.1
  (stream-element-type *standard-input*)
  character)

(deftest stream-element-type.2
  (stream-element-type *standard-output*)
  character)

(deftest stream-element-type-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (stream-element-type stream)))
  character)

(deftest stream-element-type-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest stream-element-type-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (stream-element-type stream)))
  character)

(deftest stream-element-type-file.4
  (with-temp-file
    (with-open-file (stream *file* :direction :output
                            :if-exists :supersede :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest stream-element-type-file.5
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (stream-element-type stream)))
  character)

(deftest stream-element-type-file.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io
                            :if-exists :supersede :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest stream-element-type-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (stream-element-type stream))
  t)

(deftest stream-element-type-broadcast.2
  (with-temp-file
    (with-open-file (output *file* :direction :io
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream *standard-output* output))
        (stream-element-type stream))))
  (unsigned-byte 8))

(deftest stream-element-type-broadcast.3
  (with-temp-file
    (with-open-file (output *file* :direction :io
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream output *standard-output*))
        (stream-element-type stream))))
  character)

(deftest stream-element-type-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (stream-element-type stream))
  nil)

(deftest stream-element-type-concatenated.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream *standard-input* input))
        (stream-element-type stream))))
  character)

(deftest stream-element-type-concatenated.3
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input *standard-input*))
        (stream-element-type stream))))
  (unsigned-byte 8))

(deftest stream-element-type-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (stream-element-type stream))
  character)

(deftest stream-element-type-echo.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-echo-stream input *standard-output*))
        (stream-element-type stream))))
  (or (unsigned-byte 8) character))

(deftest stream-element-type-echo.3
  (with-temp-file
    (with-open-file (output *file* :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-echo-stream *standard-input* output))
        (stream-element-type stream))))
  (or character (unsigned-byte 8)))

(deftest stream-element-type-synonym.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (stream-element-type stream))
  character)

(deftest stream-element-type-synonym.2
  (with-temp-file
    (with-open-file (hello *file* :direction :input :element-type 'unsigned-byte)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (stream-element-type stream))))
  (unsigned-byte 8))

(deftest stream-element-type-two-way.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (stream-element-type stream))
  character)

(deftest stream-element-type-two-way.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-two-way-stream input *standard-output*))
        (stream-element-type stream))))
  (or (unsigned-byte 8) character))

(deftest stream-element-type-two-way.3
  (with-temp-file
    (with-open-file (output *file* :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-two-way-stream *standard-input* output))
        (stream-element-type stream))))
  (or character (unsigned-byte 8)))

(deftest stream-element-type-input-string.1
  (with-input-from-string (stream "Hello")
    (stream-element-type stream))
  character)

(deftest stream-element-type-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (stream-element-type stream)))
    result)
  character)

(deftest stream-element-type-extend-string.1
  (with-extend-to-string
    (inst array)
    (stream-element-type inst))
  character)


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

