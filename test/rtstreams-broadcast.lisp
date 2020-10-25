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

