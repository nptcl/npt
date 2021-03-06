;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest echo-input-stream-p.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (input-stream-p inst))
  t)


;;
;;  output-stream-p
;;
(deftest echo-output-stream-p.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest echo-interactive-stream-p.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest echo-open-stream-p.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (open-stream-p stream))
  t)

(deftest echo-open-stream-p.2
  (let ((stream (make-echo-stream *standard-input* *standard-output*)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest echo-open-stream-p.3
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-echo-stream x y)))
    (close stream)
    (values
      (open-stream-p x)
      (open-stream-p y)))
  t t)

(deftest echo-open-stream-p.4
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-echo-stream x y)))
    (close x)
    (close y)
    (open-stream-p stream))
  t)


;;
;;  stream-element-type
;;
(deftest echo-stream-element-type.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (stream-element-type stream))
  character)

(deftest echo-stream-element-type.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-echo-stream input *standard-output*))
        (stream-element-type stream))))
  (or (unsigned-byte 8) character))

(deftest echo-stream-element-type.3
  (with-temp-file
    (with-open-file (output *file* :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-echo-stream *standard-input* output))
        (stream-element-type stream))))
  (or character (unsigned-byte 8)))


;;
;;  read-byte
;;
(deftest echo-read-byte.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-byte stream)))))
  65)

(deftest echo-read-byte.2
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-byte stream)))
      (with-open-file (check *file2* :direction :input)
        (values
          (read-char check nil :eof)
          (read-char check nil :eof)))))
  #\A :eof)

(deftest echo-read-byte.3
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (read-byte stream nil :eof)
            (read-byte stream nil :eof)
            (read-byte stream nil :eof)
            (read-byte stream nil :eof)
            (read-byte stream nil :eof))))))
  65 66 67 :eof :eof)

(deftest echo-read-byte.4
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)))
      (with-open-file (check *file2* :direction :input)
        (values
          (read-char check nil :eof)
          (read-char check nil :eof)
          (read-char check nil :eof)
          (read-char check nil :eof)
          (read-char check nil :eof)))))
  #\A #\B #\C :eof :eof)

(deftest-error echo-read-byte.5
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-byte stream)
          (read-byte stream)
          (read-byte stream)
          (read-byte stream)
          (read-byte stream)))))
  end-of-file)


;;
;;  write-byte
;;
(deftest echo-write-byte.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (write-byte 70 stream)
          (write-byte 71 stream))))
    (with-open-file (input *file2*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\F #\G :eof)

(deftest-error echo-write-byte.2
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (write-byte 70 stream))))))


;;
;;  read-char
;;
(deftest echo-read-char.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char stream)))))
  #\A)

(deftest echo-read-char.2
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char stream))))
    (with-open-file (input *file2*)
      (values
        (read-char input nil nil)
        (read-char input nil nil))))
  #\A nil)

(deftest echo-read-char.3
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (read-char stream nil :eof)
            (read-char stream nil :eof)
            (read-char stream nil :eof)
            (read-char stream nil :eof)
            (read-char stream nil :eof))))))
  #\A #\B #\C :eof :eof)

(deftest echo-read-char.4
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof))))
    (with-open-file (input *file2*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\A #\B #\C :eof :eof)


;;
;;  read-char-no-hang
;;
(deftest echo-read-char-no-hang.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char-no-hang stream)))))
  #\A)

(deftest echo-read-char-no-hang.2
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char-no-hang stream))))
    (with-open-file (input *file2*)
      (values
        (read-char-no-hang input nil nil)
        (read-char-no-hang input nil nil))))
  #\A nil)

(deftest echo-read-char-no-hang.3
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof))))))
  #\A #\B #\C :eof :eof)

(deftest echo-read-char-no-hang.4
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof))))
    (with-open-file (input *file2*)
      (values
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof))))
  #\A #\B #\C :eof :eof)


;;
;;  unread-char
;;
(deftest echo-unread-char.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (read-char stream)
        (unread-char #\Z stream)
        (values
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)))))
  #\Z #\B #\C nil nil)

(deftest echo-unread-char.2
  (with-input-from-string (input "Hello")
    (with-output-to-string (output)
      (with-open-stream (stream (make-echo-stream input output))
        (read-char stream)
        (read-char stream)
        (unread-char #\e stream)
        (read-char stream)
        (unread-char #\e stream)
        (read-char stream)
        (unread-char #\e stream)
        (read-char stream))))
  "He")

(deftest echo-unread-char.3
  (with-input-from-string (input "Hello")
    (with-output-to-string (output)
      (with-open-stream (stream (make-echo-stream input output))
        (read-char-no-hang stream)
        (read-char-no-hang stream)
        (unread-char #\e stream)
        (read-char-no-hang stream)
        (unread-char #\e stream)
        (read-char-no-hang stream)
        (unread-char #\e stream)
        (read-char-no-hang stream))))
  "He")


;;
;;  write-char
;;
(deftest echo-write-char.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (write-char #\Z stream)
        (read-char stream)
        (read-char stream))
      (get-output-stream-string output)))
  "ZAB")


;;
;;  readl-line
;;
(deftest echo-read-line.1
  (with-input-from-string (input (format nil "aaa~%BBB"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (values
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)))))
  "aaa" "BBB" :eof)

(deftest echo-read-line.2
  (with-input-from-string (input (format nil "aaa~%BBB"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (read-line stream nil :eof)
        (read-line stream nil :eof)
        (read-line stream nil :eof))
      (equal (format nil "aaa~%BBB")
             (get-output-stream-string output))))
  t)


;;
;;  file-length
;;
(deftest-error echo-file-length.1
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
(deftest echo-file-position.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream))))
  nil)

(deftest echo-file-position-set.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream :start))))
  nil)

(deftest echo-file-position-set.2
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream :end))))
  nil)

(deftest echo-file-position-set.3
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream 3))))
  nil)


;;
;;  file-string-length
;;
(deftest echo-file-string-length.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :external-format 'utf-8)
      (with-overwrite-file (output *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)


;;
;;  stream-external-format
;;
(deftest echo-stream-external-format.1
  (with-open-stream (a (make-memory-io-stream))
    (with-open-stream (b (make-memory-io-stream))
      (with-open-file (x a :direction :input :external-format 'utf8)
        (with-open-file (y b :direction :output :external-format 'ascii)
          (with-open-stream (z (make-echo-stream x y))
            (stream-external-format z))))))
  lisp-system::utf-8)


;;
;;  close
;;
(deftest echo-close.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (let ((stream (make-echo-stream input output)))
        (values
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (open-stream-p input)
          (open-stream-p output)))))
  t t nil t nil t t)

(deftest echo-close.2
  (with-open-stream (x (make-string-input-stream "Hello"))
    (with-open-stream (y (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream x y))
        (close stream)
        (values
          (open-stream-p x)
          (open-stream-p y)
          (open-stream-p stream)))))
  t t nil)

(deftest echo-close.3
  (with-open-stream (z (make-string-input-stream "Hello"))
    (with-open-stream (y (make-string-output-stream))
      (let ((x (make-echo-stream z y)))
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
(deftest echo-listen.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (listen stream))))
  t)


;;
;;  clear-input
;;
(deftest echo-clear-input.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (clear-input stream))
  nil)


;;
;;  finish-output
;;
(deftest echo-finish-output.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (write-char #\a stream)
          (finish-output stream)))))
  nil)


;;
;;  force-output
;;
(deftest echo-force-output.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (write-char #\a stream)
          (force-output stream)))))
  nil)


;;
;;  clear-output
;;
(deftest echo-clear-output.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (write-char #\a stream)
          (clear-output stream)))))
  nil)


;;
;;  Function MAKE-ECHO-STREAM
;;
(deftest make-echo-stream.1
  (typep (make-echo-stream
           *standard-input*
           *standard-output*)
         'echo-stream)
  t)

(deftest-error make-echo-stream-error.1
  (make-echo-stream *error-output* *standard-output*)
  type-error)

(deftest-error make-echo-stream-error.2
  (make-echo-stream *standard-input* *standard-input*)
  type-error)

(deftest-error make-echo-stream-error.3
  (eval '(make-echo-stream 10 *standard-output*))
  type-error)

(deftest-error make-echo-stream-error.4
  (eval '(make-echo-stream *standard-input* 20))
  type-error)

(deftest-error! make-echo-stream-error.5
  (eval '(make-echo-stream *standard-input*)))

(deftest-error! make-echo-stream-error.6
  (eval '(make-echo-stream *standard-input* *standard-output* nil)))

;;  ANSI Common Lisp
(deftest make-echo-stream-test.1
  (let ((out (make-string-output-stream)))
    (with-open-stream
      (s (make-echo-stream
           (make-string-input-stream "this-is-read-and-echoed")
           out))
      (read s)
      (format s " * this-is-direct-output")
      (get-output-stream-string out)))
  "this-is-read-and-echoed * this-is-direct-output")


;;
;;  Function ECHO-STREAM-INPUT-STREAM
;;
(deftest echo-stream-input-stream.1
  (eq *standard-input*
      (echo-stream-input-stream
        (make-echo-stream *standard-input* *standard-output*)))
  t)

(deftest-error echo-stream-input-stream-error.1
  (eval '(echo-stream-input-stream (make-string-input-stream "Hello")))
  type-error)

(deftest-error echo-stream-input-stream-error.2
  (eval '(echo-stream-input-stream 10))
  type-error)

(deftest-error! echo-stream-input-stream-error.3
  (eval '(echo-stream-input-stream)))

(deftest-error! echo-stream-input-stream-error.4
  (eval '(echo-stream-input-stream
           (make-echo-stream *standard-input* *standard-output*)
           nil)))


;;
;;  Function ECHO-STREAM-OUTPUT-STREAM
;;
(deftest echo-stream-output-stream.1
  (eq *standard-output*
      (echo-stream-output-stream
        (make-echo-stream *standard-input* *standard-output*)))
  t)

(deftest-error echo-stream-output-stream-error.1
  (eval '(echo-stream-output-stream (make-string-output-stream)))
  type-error)

(deftest-error echo-stream-output-stream-error.2
  (eval '(echo-stream-output-stream 10))
  type-error)

(deftest-error! echo-stream-output-stream-error.3
  (eval '(echo-stream-output-stream)))

(deftest-error! echo-stream-output-stream-error.4
  (eval '(echo-stream-output-stream
           (make-echo-stream *standard-input* *standard-output*)
           nil)))

