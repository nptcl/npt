;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest synonym-input-stream-p.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (input-stream-p inst))))
  t)

(deftest synonym-input-stream-p.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (input-stream-p inst))))
  nil)


;;
;;  output-stream-p
;;
(deftest synonym-output-stream-p.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (output-stream-p inst))))
  nil)

(deftest synonym-output-stream-p.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (output-stream-p inst))))
  t)


;;
;;  interactive-stream-p
;;
(deftest synonym-interactive-stream-p.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (interactive-stream-p inst))))
  nil)

(deftest synonym-interactive-stream-p.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (interactive-stream-p inst))))
  nil)

(deftest synonym-interactive-stream-p.3
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (interactive-stream-p stream))
  t)


;;
;;  open-stream-p
(deftest synonym-open-stream-p.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)))
  t t)

(deftest synonym-open-stream-p.2
  (let ((stream (make-synonym-stream '*standard-input*)))
    (close stream)
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)))
  nil t)

(deftest synonym-open-stream-p.3
  (let ((*x* (make-string-input-stream "Hello")))
    (declare (special *x*))
    (let ((stream (make-synonym-stream '*x*)))
      (close *x*)
      (open-stream-p stream)))
  t)

(deftest synonym-open-stream-p.4
  (let ((*x* (make-string-input-stream "Hello")))
    (declare (special *x*))
    (let ((stream (make-synonym-stream '*x*)))
      (close stream)
      (open-stream-p *x*)))
  t)


;;
;;  stream-element-type
;;
(deftest synonym-stream-element-type.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (stream-element-type stream))
  character)

(deftest synonym-stream-element-type.2
  (with-temp-file
    (with-open-file (hello *file* :direction :input :element-type 'unsigned-byte)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (stream-element-type stream))))
  (unsigned-byte 8))


;;
;;  read-byte
;;
(deftest synonym-read-byte.1
  (with-temp-file
    (with-open-file (*hello* *file* :element-type 'unsigned-byte)
      (declare (special *hello*))
      (with-open-stream (stream (make-synonym-stream '*hello*))
        (read-byte stream))))
  65)

(deftest synonym-read-byte.2
  (with-temp-file
    (with-open-file (*hello* *file* :element-type 'unsigned-byte)
      (declare (special *hello*))
      (with-open-stream (stream (make-synonym-stream '*hello*))
        (values
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)
          (read-byte stream nil :eof)))))
  65 66 67 :eof :eof)

(deftest-error synonym-read-byte.3
  (with-temp-file
    (with-open-file (*hello* *file* :element-type 'unsigned-byte)
      (declare (special *hello*))
      (with-open-stream (stream (make-synonym-stream '*hello*))
        (read-byte stream)
        (read-byte stream)
        (read-byte stream)
        (read-byte stream)
        (read-byte stream))))
  end-of-file)


;;
;;  write-byte
;;
(deftest synonym-write-byte.1
  (with-temp-file
    (with-binary-output
      (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (write-byte 70 stream)
        (write-byte 71 stream)))
    (with-open-file (input *file*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\F #\G :eof)

(deftest-error synonym-write-byte.2
  (with-temp-file
    (with-overwrite-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (write-byte 70 stream)))))


;;
;;  read-char
;;
(deftest synonym-read-char.1
  (with-temp-file
    (with-open-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (read-char stream))))
  #\A)

(deftest synonym-read-char.2
  (with-temp-file
    (with-open-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (values
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof)))))
  #\A #\B #\C :eof :eof)


;;
;;  read-char-no-hang
;;
(deftest synonym-read-char-no-hang.1
  (with-temp-file
    (with-open-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (read-char-no-hang stream))))
  #\A)

(deftest synonym-read-char-no-hang.2
  (with-temp-file
    (with-open-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (values
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)))))
  #\A #\B #\C :eof :eof)


;;
;;  unread-char
;;
(deftest synonym-unread-char.1
  (with-input-from-string (hello "ABC")
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (read-char stream)
      (unread-char #\Z stream)
      (values
        (read-char stream nil)
        (read-char stream nil)
        (read-char stream nil)
        (read-char stream nil)
        (read-char stream nil))))
  #\Z #\B #\C nil nil)


;;
;;  write-char
;;
(deftest synonym-write-char.1
  (with-open-stream (hello (make-string-output-stream))
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (write-char #\A stream)
      (write-char #\B stream))
    (get-output-stream-string hello))
  "AB")


;;
;;  read-line
;;
(deftest synonym-read-line.1
  (with-input-from-string (hello (format nil "aaa~%~%bbb~%"))
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (values
        (read-line stream nil :eof)
        (read-line stream nil :eof)
        (read-line stream nil :eof)
        (read-line stream nil :eof))))
  "aaa" "" "bbb" :eof)


;;
;;  file-length
;;
(deftest synonym-file-length.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (declare (special input))
      (with-open-stream (stream (make-synonym-stream 'input))
        (file-length stream))))
  4)


;;
;;  file-position
;;
(deftest synonym-file-position.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'hello))
      (file-position input)))
  2)

(deftest synonym-file-position-set.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream :start)
        (read-char stream))))
  t #\H)

(deftest synonym-file-position-set.2
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream :end)
        (read-char stream nil))))
  t nil)

(deftest synonym-file-position-set.3
  (with-input-from-string (input "abcdef")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream 3)
        (read-char stream))))
  t #\d)


;;
;;  file-string-length
;;
(deftest synonym-file-string-length.1
  (with-temp-file
    (with-overwrite-file (hello *file* :external-format 'utf-32)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (values
          (file-string-length stream #\a)
          (file-string-length stream "abcd")))))
  4 16)


;;
;;  stream-external-format
;;
(deftest synonym-stream-external-format.1
  (with-open-stream (x (make-memory-io-stream))
    (with-open-file (*stream* x :direction :output :external-format 'utf16le)
      (declare (special *stream*))
      (with-open-stream (z (make-synonym-stream '*stream*))
        (stream-external-format z))))
  lisp-system::utf-16le)


;;
;;  close
;;
(deftest synonym-close.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (let ((stream (make-synonym-stream 'input)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p input))))
  t t nil t nil t)

(deftest synonym-close.2
  (with-open-stream (output (make-string-output-stream))
    (declare (special output))
    (with-open-stream (x (make-synonym-stream 'output))
      (close x)
      (values
        (open-stream-p output)
        (open-stream-p x))))
  t nil)

(deftest synonym-close.3
  (with-open-stream (output (make-string-output-stream))
    (declare (special output))
    (let ((x (make-synonym-stream 'output)))
      (values
        (close x)
        (open-stream-p x)
        (input-stream-p x)
        (output-stream-p x)
        (interactive-stream-p x)
        (streamp x)
        (close x))))
  t nil nil t nil t t)


;;
;;  listen
;;
(deftest synonym-listen.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (with-open-stream (stream (make-synonym-stream 'input))
      (listen stream)))
  t)

