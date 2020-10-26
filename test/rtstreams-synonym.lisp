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

