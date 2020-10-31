;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest two-way-input-stream-p.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (input-stream-p inst))
  t)


;;
;;  output-stream-p
;;
(deftest two-way-output-stream-p.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest two-way-interactive-stream-p.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (interactive-stream-p inst))
  t)

(deftest two-way-interactive-stream-p.2
  (with-open-stream (inst (make-two-way-stream
                            *standard-input*
                            (make-string-output-stream)))
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest two-way-open-stream-p.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (open-stream-p stream))
  t)

(deftest two-way-open-stream-p.2
  (let ((stream (make-two-way-stream *standard-input* *standard-output*)))
    (close stream)
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)
      (open-stream-p *standard-output*)))
  nil t t)

(deftest two-way-open-stream-p.3
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-two-way-stream x y)))
    (close stream)
    (values
      (open-stream-p x)
      (open-stream-p y)))
  t t)

(deftest two-way-open-stream-p.4
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-two-way-stream x y)))
    (close x)
    (close y)
    (open-stream-p stream))
  t)


;;
;;  stream-element-type
;;
(deftest two-way-stream-element-type.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (stream-element-type stream))
  character)

(deftest two-way-stream-element-type.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-two-way-stream input *standard-output*))
        (stream-element-type stream))))
  (or (unsigned-byte 8) character))

(deftest two-way-stream-element-type.3
  (with-temp-file
    (with-open-file (output *file* :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-two-way-stream *standard-input* output))
        (stream-element-type stream))))
  (or character (unsigned-byte 8)))


;;
;;  read-byte
;;
(deftest two-way-read-byte.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (read-byte stream)))))
  65)

(deftest two-way-read-byte.2
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (read-byte stream nil :eof)
            (read-byte stream nil :eof)
            (read-byte stream nil :eof)
            (read-byte stream nil :eof)
            (read-byte stream nil :eof))))))
  65 66 67 :eof :eof)

(deftest-error two-way-read-byte.3
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (read-byte stream)
          (read-byte stream)
          (read-byte stream)
          (read-byte stream)
          (read-byte stream)))))
  end-of-file)


;;
;;  write-byte
;;
(deftest two-way-write-byte.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (write-byte 70 stream)
          (write-byte 71 stream))))
    (with-open-file (input *file2*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\F #\G :eof)

(deftest-error two-way-write-byte.2
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (write-byte 70 stream))))))


;;
;;  read-char
;;
(deftest two-way-read-char.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (read-char stream)))))
  #\A)

(deftest two-way-read-char.2
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (read-char stream nil :eof)
            (read-char stream nil :eof)
            (read-char stream nil :eof)
            (read-char stream nil :eof)
            (read-char stream nil :eof))))))
  #\A #\B #\C :eof :eof)


;;
;;  read-char-no-hang
;;
(deftest two-way-read-char-no-hang.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (read-char-no-hang stream)))))
  #\A)

(deftest two-way-read-char-no-hang.2
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof))))))
  #\A #\B #\C :eof :eof)


;;
;;  unread-char
;;
(deftest two-way-unread-char.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (read-char stream)
        (unread-char #\Z stream)
        (values
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)))))
  #\Z #\B #\C nil nil)


;;
;;  write-char
;;
(deftest two-way-write-char.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (write-char #\A stream)
        (write-char #\B stream)
        (read-char stream)
        (read-char stream))
      (get-output-stream-string output)))
  "AB")


;;
;;  read-line
;;
(deftest two-way-read-line.1
  (with-input-from-string (input (format nil "aaa~%BBB"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (values
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)))))
  "aaa" "BBB" :eof)


;;
;;  file-length
;;
(deftest-error two-way-file-length.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input *file1*)
        (with-overwrite-file (output *file2*)
          (with-open-stream (stream (make-echo-stream input output))
            (file-length stream)))))))


;;
;;  file-position
;;
(deftest two-way-file-position.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream))))
  nil)

(deftest two-way-file-position-set.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream :start))))
  nil)

(deftest two-way-file-position-set.2
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream :end))))
  nil)

(deftest two-way-file-position-set.3
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream 3))))
  nil)


;;
;;  file-string-length
;;
(deftest two-way-file-string-length.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :external-format 'utf-8)
      (with-overwrite-file (output *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)


;;
;;  stream-external-format
;;
(deftest two-way-stream-external-format.1
  (with-open-stream (a (make-memory-io-stream))
    (with-open-stream (b (make-memory-io-stream))
      (with-open-file (x a :direction :input :external-format 'utf8)
        (with-open-file (y b :direction :output :external-format 'ascii)
          (with-open-stream (z (make-two-way-stream x y))
            (stream-external-format z))))))
  lisp-system::utf-8)


;;
;;  close
;;
(deftest two-way-close.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (let ((stream (make-two-way-stream input output)))
        (values
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (open-stream-p input)
          (open-stream-p output)))))
  t t nil t nil t t)

(deftest two-way-close.2
  (with-open-stream (x (make-string-input-stream "Hello"))
    (with-open-stream (y (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream x y))
        (close stream)
        (values
          (open-stream-p x)
          (open-stream-p y)
          (open-stream-p stream)))))
  t t nil)

(deftest two-way-close.3
  (with-open-stream (z (make-string-input-stream "Hello"))
    (with-open-stream (y (make-string-output-stream))
      (let ((x (make-two-way-stream z y)))
        (values
          (close x)
          (open-stream-p x)
          (input-stream-p x)
          (output-stream-p x)
          (interactive-stream-p x)
          (streamp x)
          (close x)))))
  t nil t t nil t t)


;;
;;  listen
;;
(deftest two-way-listen.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (listen stream))))
  t)


;;
;;  clear-input
;;
(deftest two-way-clear-input.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (clear-input stream))
  nil)


;;
;;  finish-output
;;
(deftest two-way-finish-output.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (write-char #\a stream)
          (finish-output stream)))))
  nil)


;;
;;  force-output
;;
(deftest two-way-force-output.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (write-char #\a stream)
          (force-output stream)))))
  nil)


;;
;;  clear-output
;;
(deftest two-way-clear-output.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (write-char #\a stream)
          (clear-output stream)))))
  nil)


;;
;;  Function MAKE-TWO-WAY-STREAM
;;
(deftest make-two-way-stream.1
  (typep (make-two-way-stream
           *standard-input*
           *standard-output*)
         'two-way-stream)
  t)

(deftest-error make-two-way-stream-error.1
  (make-two-way-stream *error-output* *standard-output*)
  type-error)

(deftest-error make-two-way-stream-error.2
  (make-two-way-stream *standard-input* *standard-input*)
  type-error)

(deftest-error make-two-way-stream-error.3
  (eval '(make-two-way-stream 10 *standard-output*))
  type-error)

(deftest-error make-two-way-stream-error.4
  (eval '(make-two-way-stream *standard-input* 20))
  type-error)

(deftest-error! make-two-way-stream-error.5
  (eval '(make-two-way-stream *standard-input*)))

(deftest-error! make-two-way-stream-error.6
  (eval '(make-two-way-stream *standard-input* *standard-output* nil)))

;;  ANSI Common Lisp
(deftest make-two-way-stream-test.1
  (let (what-is-read)
    (values
      (with-output-to-string (out)
        (with-input-from-string (in "input...")
          (let ((two (make-two-way-stream in out)))
            (format two "output...")
            (setq what-is-read (read two)))))
      what-is-read))
  "output..." input...)


;;
;;  Function TWO-WAY-STREAM-INPUT-STREAM
;;
(deftest two-way-stream-input-stream.1
  (eq *standard-input*
      (two-way-stream-input-stream
        (make-two-way-stream *standard-input* *standard-output*)))
  t)

(deftest-error two-way-stream-input-stream-error.1
  (eval '(two-way-stream-input-stream (make-string-input-stream "Hello")))
  type-error)

(deftest-error two-way-stream-input-stream-error.2
  (eval '(two-way-stream-input-stream 10))
  type-error)

(deftest-error! two-way-stream-input-stream-error.3
  (eval '(two-way-stream-input-stream)))

(deftest-error! two-way-stream-input-stream-error.4
  (eval '(two-way-stream-input-stream
           (make-two-way-stream *standard-input* *standard-output*)
           nil)))


;;
;;  Function TWO-WAY-STREAM-OUTPUT-STREAM
;;
(deftest two-way-stream-output-stream.1
  (eq *standard-output*
      (two-way-stream-output-stream
        (make-two-way-stream *standard-input* *standard-output*)))
  t)

(deftest-error two-way-stream-output-stream-error.1
  (eval '(two-way-stream-output-stream (make-string-output-stream)))
  type-error)

(deftest-error two-way-stream-output-stream-error.2
  (eval '(two-way-stream-output-stream 10))
  type-error)

(deftest-error! two-way-stream-output-stream-error.3
  (eval '(two-way-stream-output-stream)))

(deftest-error! two-way-stream-output-stream-error.4
  (eval '(two-way-stream-output-stream
           (make-two-way-stream *standard-input* *standard-output*)
           nil)))

