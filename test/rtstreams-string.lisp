;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest string-input-stream-p.1
  (with-input-from-string (stream "Hello")
    (input-stream-p stream))
  t)

(deftest string-input-stream-p.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (input-stream-p stream)))
    result)
  nil)

(deftest string-input-stream-p.3
  (with-extend-to-string
    (inst array)
    (input-stream-p inst))
  nil)


;;
;;  output-stream-p
;;
(deftest string-output-stream-p.1
  (with-input-from-string (stream "Hello")
    (output-stream-p stream))
  nil)

(deftest string-output-stream-p.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (output-stream-p stream)))
    result)
  t)

(deftest string-output-stream-p.3
  (with-extend-to-string
    (inst array)
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest string-interactive-stream-p.1
  (with-input-from-string (stream "Hello")
    (interactive-stream-p stream))
  nil)

(deftest string-interactive-stream-p.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (interactive-stream-p stream)))
    result)
  nil)

(deftest string-interactive-stream-p.3
  (with-extend-to-string
    (inst array)
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest string-open-stream-p.1
  (with-input-from-string (stream "Hello")
    (open-stream-p stream))
  t)

(deftest string-open-stream-p.2
  (with-input-from-string (stream "x")
    (read-char stream nil nil)
    (read-char stream nil nil)
    (read-char stream nil nil)
    (read-char stream nil nil)
    (open-stream-p stream))
  t)

(deftest string-open-stream-p.3
  (let ((stream (make-string-input-stream "Hello")))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest string-open-stream-p.4
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (open-stream-p stream)))
    result)
  t)

(deftest string-open-stream-p.5
  (let ((stream (make-string-output-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest string-open-stream-p.6
  (with-extend-to-string
    (inst array)
    (open-stream-p inst))
  t)

(deftest string-open-stream-p.7
  (let (stream)
    (with-extend-to-string
      (inst array)
      (setq stream inst))
    (open-stream-p stream))
  nil)


;;
;;  stream-element-type
;;
(deftest string-stream-element-type.1
  (with-input-from-string (stream "Hello")
    (stream-element-type stream))
  character)

(deftest string-stream-element-type.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (stream-element-type stream)))
    result)
  character)

(deftest string-stream-element-type.3
  (with-extend-to-string
    (inst array)
    (stream-element-type inst))
  character)


;;
;;  read-byte
;;
(deftest-error string-read-byte.1
  (with-input-from-string (stream "Hello")
    (read-byte stream)))

(deftest-error string-read-byte.2
  (with-output-to-string (stream)
    (read-byte stream))
  type-error)

(deftest-error string-read-byte.3
  (with-extend-to-string
    (inst array)
    (read-byte inst))
  type-error)


;;
;;  write-byte
;;
(deftest-error string-write-byte.1
  (with-input-from-string (input "Hello")
    (write-byte 70 input)))

(deftest-error string-write-byte.2
  (with-output-to-string (output)
    (write-byte 70 output)))

(deftest-error string-write-byte.3
  (with-extend-to-string
    (output array)
    (write-byte 70 output)))


;;
;;  read-char
;;
(deftest string-read-char.1
  (with-input-from-string (stream "ABC")
    (read-char stream))
  #\A)

(deftest string-read-char.2
  (with-input-from-string (stream "ABC")
    (values
      (read-char stream nil :eof)
      (read-char stream nil :eof)
      (read-char stream nil :eof)
      (read-char stream nil :eof)
      (read-char stream nil :eof)))
  #\A #\B #\C :eof :eof)

(deftest-error string-read-char.3
  (with-output-to-string (stream)
    (read-char stream nil :eof)))

(deftest-error string-read-char.4
  (with-extend-to-string
    (stream array)
    (read-char stream nil :eof)))


;;
;;  read-char-no-hang
;;
(deftest string-read-char-no-hang.1
  (with-input-from-string (stream "ABC")
    (read-char-no-hang stream))
  #\A)

(deftest string-read-char-no-hang.2
  (with-input-from-string (stream "ABC")
    (values
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)))
  #\A #\B #\C :eof :eof)

(deftest-error string-read-char-no-hang.3
  (with-output-to-string (stream)
    (read-char-no-hang stream nil :eof)))

(deftest-error string-read-char-no-hang.4
  (with-extend-to-string
    (stream array)
    (read-char-no-hang stream nil :eof)))


;;
;;  unread-char
;;
(deftest string-unread-char.1
  (with-input-from-string (stream "ABC")
    (read-char stream)
    (unread-char #\Z stream)
    (values
      (read-char stream nil)
      (read-char stream nil)
      (read-char stream nil)
      (read-char stream nil)
      (read-char stream nil)))
  #\Z #\B #\C nil nil)

(deftest-error string-unread-char.2
  (with-output-to-string (stream)
    (unread-char #\Z stream)))

(deftest-error string-unread-char.3
  (with-extend-to-string
    (stream array)
    (unread-char #\Z stream)))


;;
;;  write-char
;;
(deftest-error string-write-char.1
  (with-open-stream (stream (make-string-input-stream "ABC"))
    (write-char #\A stream)))

(deftest string-write-char.2
  (with-output-to-string (stream)
    (write-char #\A stream)
    (write-char #\B stream))
  "AB")

(deftest string-write-char.3
  (with-extend-to-string
    (stream array)
    (write-char #\A stream)
    (write-char #\B stream)
    array)
  "AB")

