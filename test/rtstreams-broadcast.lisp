;;
;;  ANSI COMMON LISP: 21. Streams
;;
(deftest broadcast-stream.1
  (let ((x (make-broadcast-stream)))
    (values
      (streamp x)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (open-stream-p x)))
  t nil t nil t)


;;
;;  input-stream-p
;;
(deftest broadcast-input-stream-p.1
  (with-open-stream (inst (make-broadcast-stream))
    (input-stream-p inst))
  nil)


;;
;;  output-stream-p
;;
(deftest broadcast-output-stream-p.1
  (with-open-stream (inst (make-broadcast-stream))
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest broadcast-interactive-stream-p.1
  (with-open-stream (inst (make-broadcast-stream))
    (interactive-stream-p inst))
  nil)

(deftest broadcast-interactive-stream-p.2
  (with-open-stream (inst (make-broadcast-stream *query-io*))
    (interactive-stream-p inst))
  nil)

(deftest broadcast-interactive-stream-p.3
  (with-open-stream (inst (make-broadcast-stream *standard-output*))
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest broadcast-open-stream-p.1
  (with-open-stream (stream (make-broadcast-stream))
    (open-stream-p stream))
  t)

(deftest broadcast-open-stream-p.2
  (let ((stream (make-broadcast-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest broadcast-open-stream-p.3
  (let* ((str (make-string-output-stream))
         (stream (make-broadcast-stream str)))
    (close str)
    (open-stream-p stream))
  t)

(deftest broadcast-open-stream-p.4
  (let* ((str (make-string-output-stream))
         (stream (make-broadcast-stream str)))
    (close stream)
    (open-stream-p str))
  t)


;;
;;  stream-element-type
;;
(deftest broadcast-stream-element-type.1
  (with-open-stream (stream (make-broadcast-stream))
    (stream-element-type stream))
  t)

(deftest broadcast-stream-element-type.2
  (with-temp-file
    (with-open-file (output *file* :direction :io
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream *standard-output* output))
        (stream-element-type stream))))
  (unsigned-byte 8))

(deftest broadcast-stream-element-type.3
  (with-temp-file
    (with-open-file (output *file* :direction :io
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream output *standard-output*))
        (stream-element-type stream))))
  character)


;;
;;  read-byte
;;
(deftest-error broadcast-read-byte.1
  (with-temp-file
    (with-open-file (output *file* :direction :io :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream output))
        (read-byte stream nil :eof))))
  file-error)


;;
;;  write-byte
;;
(deftest broadcast-write-byte.1
  (with-open-stream (stream (make-broadcast-stream))
    (write-byte 70 stream))
  70)

(deftest broadcast-write-byte.2
  (with-temp-file1-file2
    (with-binary-output
      (output1 *file1*)
      (with-binary-output
        (output2 *file2*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (write-byte 70 stream)
          (write-byte 71 stream))))
    (let (result)
      (with-open-file (input *file1*)
        (push (read-char input nil :eof) result)
        (push (read-char input nil :eof) result)
        (push (read-char input nil :eof) result))
      (with-open-file (input *file2*)
        (push (read-char input nil :eof) result)
        (push (read-char input nil :eof) result)
        (push (read-char input nil :eof) result))
      (nreverse result)))
  (#\F #\G :eof #\F #\G :eof))

(deftest-error broadcast-write-byte.3
  (with-temp-file1-file2
    (with-overwrite-file (output *file*)
      (with-open-stream (stream (make-broadcast-stream output))
        (write-byte 70 stream)))))


;;
;;  read-char
;;
(deftest-error broadcast-read-char.1
  (with-open-stream (stream (make-broadcast-stream))
    (read-char stream nil)))


;;
;;  read-char-no-hang
;;
(deftest-error broadcast-read-char-no-hang.1
  (with-open-stream (stream (make-broadcast-stream))
    (read-char-no-hang stream nil)))


;;
;;  unread-char
;;
(deftest-error broadcast-unread-char.1
  (with-open-stream (stream (make-broadcast-stream))
    (unread-char #\Z stream)))


;;
;;  write-char
;;
(deftest broadcast-write-char.1
  (with-open-stream (stream (make-broadcast-stream))
    (write-char #\A stream))
  #\A)

(deftest broadcast-write-char.2
  (with-open-stream (output1 (make-string-output-stream))
    (with-open-stream (output2 (make-string-output-stream))
      (with-open-stream (stream (make-broadcast-stream output1 output2))
        (write-char #\Z output1)
        (write-char #\A stream)
        (write-char #\B stream))
      (values
        (get-output-stream-string output1)
        (get-output-stream-string output2))))
  "ZAB" "AB")


;;
;;  read-line
;;
(deftest-error broadcast-read-line.1
  (with-open-stream (stream (make-broadcast-stream))
    (read-line stream)))


;;
;;  file-length
;;
(deftest broadcast-file-length.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-length stream))
  0)

(deftest broadcast-file-length.2
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (input2 *file2* :direction :io :if-exists :overwrite)
          (with-open-stream (stream (make-broadcast-stream input1 input2))
            (file-length stream))))))
  4)


;;
;;  file-position
;;
(deftest broadcast-file-position.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-position stream))
  0)

(deftest broadcast-file-position.2
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdef")
      (with-open-file (input1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (input2 *file2* :direction :io :if-exists :overwrite)
          (read-char input1)
          (read-char input1)
          (read-char input2)
          (read-char input2)
          (read-char input2)
          (with-open-stream (stream (make-broadcast-stream input1 input2))
            (file-position stream))))))
  3)

(deftest broadcast-file-position-set.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-position stream :start))
  nil)

(deftest broadcast-file-position-set.2
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream :start)
              (read-char stream1)
              (read-char stream2)))))))
  t #\H #\a)

(deftest broadcast-file-position-set.3
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream :end)
              (read-char stream1 nil :eof)
              (read-char stream2 nil :eof)))))))
  t :eof :eof)

(deftest broadcast-file-position-set.4
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream 2)
              (read-char stream1)
              (read-char stream2)))))))
  t #\l #\c)


;;
;;  file-string-length
;;
(deftest broadcast-file-string-length.1
  (with-open-stream (stream (make-broadcast-stream))
    (values
      (file-string-length stream #\a)
      (file-string-length stream "abcd")))
  1 1)

(deftest broadcast-file-string-length.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file1* :external-format 'utf-8)
      (with-overwrite-file (output2 *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)


;;
;;  stream-external-format
;;
(deftest broadcast-stream-external-format.1
  (with-open-stream (stream (make-broadcast-stream))
    (stream-external-format stream))
  :default)

(deftest broadcast-stream-external-format.2
  (with-open-stream (a (make-memory-io-stream))
    (with-open-stream (b (make-memory-io-stream))
      (with-open-file (x a :direction :output :external-format 'utf8)
        (with-open-file (y b :direction :output :external-format 'ascii)
          (with-open-stream (z (make-broadcast-stream x y))
            (stream-external-format z))))))
  lisp-system::ascii)


;;
;;  close
;;
(deftest broadcast-close.1
  (with-open-stream (output (make-string-output-stream))
    (let ((stream (make-broadcast-stream output)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p output))))
  t t nil t nil t)

(deftest broadcast-close.2
  (with-open-stream (x (make-string-output-stream))
    (with-open-stream (stream (make-broadcast-stream x))
      (close stream)
      (values
        (open-stream-p x)
        (open-stream-p stream))))
  t nil)

(deftest broadcast-close.3
  (let ((x (make-broadcast-stream)))
    (values
      (close x)
      (open-stream-p x)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (streamp x)
      (close x)))
  t nil nil t nil t t)


;;
;;  listen
;;
(deftest-error broadcast-listen.1
  (with-open-stream (stream (make-broadcast-stream))
    (listen stream)))


;;
;;  clear-input
;;
(deftest-error broadcast-clear-input.1
  (with-open-stream (stream (make-broadcast-stream))
    (clear-input stream)))


;;
;;  finish-output
;;
(deftest broadcast-finish-output.1
  (with-open-stream (stream (make-broadcast-stream))
    (finish-output stream))
  nil)

(deftest broadcast-finish-output.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file1*)
      (with-overwrite-file (output2 *file2*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (finish-output stream)))))
  nil)


;;
;;  force-output
;;
(deftest broadcast-force-output.1
  (with-open-stream (stream (make-broadcast-stream))
    (force-output stream))
  nil)

(deftest broadcast-force-output.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file1*)
      (with-overwrite-file (output2 *file2*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (force-output stream)))))
  nil)


;;
;;  clear-output
;;
(deftest broadcast-clear-output.1
  (with-open-stream (stream (make-broadcast-stream))
    (clear-output stream))
  nil)

(deftest broadcast-clear-output.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file1*)
      (with-overwrite-file (output2 *file2*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (clear-output stream)))))
  nil)


;;
;;  Function MAKE-BROADCAST-STREAM
;;
(deftest make-broadcast-stream.1
  (typep (make-broadcast-stream) 'broadcast-stream)
  t)

(deftest make-broadcast-stream.2
  (typep (make-broadcast-stream
           *standard-output*
           *error-output*) 'broadcast-stream)
  t)

(deftest-error make-broadcast-stream-error.1
  (typep (make-broadcast-stream
           *standard-input*
           *standard-output*) 'broadcast-stream)
  type-error)

(deftest-error make-broadcast-stream-error.2
  (eval '(make-broadcast-stream 10))
  type-error)

;;  ANSI Common Lisp
(defvar *make-broadcast-stream-a*)
(defvar *make-broadcast-stream-b*)

(deftest make-broadcast-stream-test.1
  (progn
    (setq *make-broadcast-stream-a* (make-string-output-stream)
          *make-broadcast-stream-b* (make-string-output-stream))
    (format (make-broadcast-stream
              *make-broadcast-stream-a*
              *make-broadcast-stream-b*)
            "this will go to both streams")
    (values
      (get-output-stream-string *make-broadcast-stream-a*)
      (get-output-stream-string *make-broadcast-stream-b*)))
  "this will go to both streams"
  "this will go to both streams")


;;
;;  Function BROADCAST-STREAM-STREAMS
;;
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

(deftest-error broadcast-stream-streams-error.1
  (broadcast-stream-streams
    (make-string-output-stream))
  type-error)

(deftest-error broadcast-stream-streams-error.2
  (eval '(broadcast-stream-streams 10))
  type-error)

(deftest-error! broadcast-stream-streams-error.3
  (eval '(broadcast-stream-streams)))

(deftest-error! broadcast-stream-streams-error.4
  (eval '(broadcast-stream-streams
           (make-broadcast-stream)
           nil)))

