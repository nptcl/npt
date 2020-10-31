;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest concatenated-input-stream-p.1
  (with-open-stream (inst (make-concatenated-stream))
    (input-stream-p inst))
  t)


;;
;;  output-stream-p
;;
(deftest concatenated-output-stream-p.1
  (with-open-stream (inst (make-concatenated-stream))
    (output-stream-p inst))
  nil)


;;
;;  interactive-stream-p
;;
(deftest concatenated-interactive-stream-p.1
  (with-open-stream (inst (make-concatenated-stream))
    (interactive-stream-p inst))
  nil)

(deftest concatenated-interactive-stream-p.2
  (with-open-stream (inst (make-concatenated-stream *standard-input*))
    (interactive-stream-p inst))
  t)

(deftest concatenated-interactive-stream-p.3
  (with-temp-file
    (with-open-file
      (stream *file* :direction :input)
      (with-open-stream (inst (make-concatenated-stream stream *standard-input*))
        (interactive-stream-p inst))))
  nil)


;;
;;  open-stream-p
;;
(deftest concatenated-open-stream-p.1
  (with-open-stream (stream (make-concatenated-stream))
    (open-stream-p stream))
  t)

(deftest concatenated-open-stream-p.2
  (let ((stream (make-concatenated-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest concatenated-open-stream-p.3
  (let* ((str (make-string-input-stream "Hello"))
         (stream (make-concatenated-stream str)))
    (close str)
    (open-stream-p stream))
  t)

(deftest concatenated-open-stream-p.4
  (let* ((str (make-string-input-stream "Hello"))
         (stream (make-concatenated-stream str)))
    (close stream)
    (open-stream-p str))
  t)


;;
;;  stream-element-type
;;
(deftest concatenated-stream-element-type.1
  (with-open-stream (stream (make-concatenated-stream))
    (stream-element-type stream))
  nil)

(deftest concatenated-stream-element-type.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream *standard-input* input))
        (stream-element-type stream))))
  character)

(deftest concatenated-stream-element-type.3
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input *standard-input*))
        (stream-element-type stream))))
  (unsigned-byte 8))


;;
;;  read-byte
;;
(deftest concatenated-read-byte.1
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-byte input))))
  65)

(deftest-error concatenated-read-byte.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-byte input)
        (read-byte input)
        (read-byte input)
        (read-byte input))))
  end-of-file)

(deftest concatenated-read-byte.3
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input))
        (values
          (read-byte input nil :eof)
          (read-byte input nil :eof)
          (read-byte input nil :eof)
          (read-byte input nil :eof)
          (read-byte input nil :eof)))))
  65 66 67 :eof :eof)

(deftest concatenated-read-byte.4
  (with-make-file
    (*file1* "AB")
    (with-make-file
      (*file2* "ab")
      (with-open-file (input1 *file1* :element-type 'unsigned-byte)
        (with-open-file (input2 *file2* :element-type 'unsigned-byte)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (values
              (read-byte stream nil :eof)
              (read-byte stream nil :eof)
              (read-byte stream nil :eof)
              (read-byte stream nil :eof)
              (read-byte stream nil :eof)
              (read-byte stream nil :eof)))))))
  65 66 97 98 :eof :eof)

(deftest-error concatenated-read-byte.5
  (with-make-file
    (*file1* "AB")
    (with-make-file
      (*file2* "ab")
      (with-open-file (input1 *file1* :element-type 'unsigned-byte)
        (with-open-file (input2 *file2* :element-type 'unsigned-byte)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (values
              (read-byte stream)
              (read-byte stream)
              (read-byte stream)
              (read-byte stream)
              (read-byte stream)))))))
  end-of-file)


;;
;;  write-byte
;;
(deftest-error concatenated-write-byte.1
  (with-open-stream (stream (make-concatenated-stream))
    (write-byte 70 stream)))


;;
;;  read-char
;;
(deftest concatenated-read-char.1
  (with-open-stream (stream (make-concatenated-stream))
    (read-char stream nil))
  nil)

(deftest concatenated-read-char.2
  (with-make-file
    (*file1* "AB")
    (with-open-file (input *file1*)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-char stream))))
  #\A)

(deftest concatenated-read-char.3
  (with-make-file
    (*file1* "AB")
    (with-open-file (input *file1*)
      (with-open-stream (stream (make-concatenated-stream input))
        (values
          (read-char stream nil :eof)
          (read-char stream nil :eof)
          (read-char stream nil :eof)))))
  #\A #\B :eof)

(deftest concatenated-read-char.4
  (with-make-file
    (*file1* "AB")
    (with-make-file
      (*file2* "CD")
      (with-open-file (input1 *file1*)
        (with-open-file (input2 *file2*)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (values
              (read-char stream nil :eof)
              (read-char stream nil :eof)
              (read-char stream nil :eof)
              (read-char stream nil :eof)
              (read-char stream nil :eof)
              (read-char stream nil :eof)))))))
  #\A #\B #\C #\D :eof :eof)


;;
;;  read-char-no-hang
;;
(deftest concatenated-read-char-no-hang.1
  (with-open-stream (stream (make-concatenated-stream))
    (read-char-no-hang stream nil))
  nil)

(deftest concatenated-read-char-no-hang.2
  (with-make-file
    (*file1* "AB")
    (with-open-file (input *file1*)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-char-no-hang stream))))
  #\A)

(deftest concatenated-read-char-no-hang.3
  (with-make-file
    (*file1* "AB")
    (with-open-file (input *file1*)
      (with-open-stream (stream (make-concatenated-stream input))
        (values
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)))))
  #\A #\B :eof)

(deftest concatenated-read-char-no-hang.4
  (with-make-file
    (*file1* "AB")
    (with-make-file
      (*file2* "CD")
      (with-open-file (input1 *file1*)
        (with-open-file (input2 *file2*)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (values
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)))))))
  #\A #\B #\C #\D :eof :eof)


;;
;;  unread-char
;;
(deftest concatenated-unread-char.1
  (with-open-stream (stream (make-concatenated-stream))
    (unread-char #\Z stream))
  nil)

(deftest concatenated-unread-char.2
  (with-input-from-string (str "Hello")
    (with-open-stream (stream (make-concatenated-stream str))
      (read-char stream)
      (read-char stream)
      (read-char stream)
      (unread-char #\Z stream)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  #\Z #\l #\o :eof)


;;
;;  write-char
;;
(deftest-error concatenated-write-char.1
  (with-open-stream (stream (make-concatenated-stream))
    (write-char #\A stream)))


;;
;;  read-line
;;
(deftest concatenated-read-line.1
  (with-input-from-string (input1 (format nil "aaa~%bbb"))
    (with-input-from-string (input2 (format nil "ccc~%ddd~%"))
      (with-open-stream (stream (make-concatenated-stream input1 input2))
        (values
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)))))
  "aaa" "bbbccc" "ddd" :eof)


;;
;;  file-length
;;
(deftest-error concatenated-file-length.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input1 *file1*)
        (with-open-file (input2 *file2*)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (file-length stream)))))))


;;
;;  file-position
;;
(deftest concatenated-file-position.1
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream)))
  nil)

(deftest concatenated-file-position-set.1
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream :start)))
  nil)

(deftest concatenated-file-position-set.2
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream :end)))
  nil)

(deftest concatenated-file-position-set.3
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream 2)))
  nil)


;;
;;  file-string-length
;;
(deftest-error concatenated-file-string-length.1
  (with-open-stream (stream (make-concatenated-stream))
    (values
      (file-string-length stream #\a)
      (file-string-length stream "abcd"))))


;;
;;  stream-external-format
;;
(deftest concatenated-stream-external-format.1
  (with-open-stream (a (make-memory-io-stream))
    (with-open-stream (b (make-memory-io-stream))
      (with-open-file (x a :direction :input :external-format 'utf8)
        (with-open-file (y b :direction :input :external-format 'ascii)
          (with-open-stream (z (make-concatenated-stream x y))
            (stream-external-format z))))))
  lisp-system::utf-8)


;;
;;  close
;;
(deftest concatenated-close.1
  (with-input-from-string (input "Hello")
    (let ((stream (make-concatenated-stream input)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p input))))
  t t nil t nil t)

(deftest concatenated-close.2
  (with-open-stream (x (make-string-input-stream "Hello"))
    (with-open-stream (stream (make-concatenated-stream x))
      (close stream)
      (values
        (open-stream-p x)
        (open-stream-p stream))))
  t nil)

(deftest concatenated-close.3
  (let ((x (make-concatenated-stream)))
    (values
      (close x)
      (open-stream-p x)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (streamp x)
      (close x)))
  t nil t nil nil t t)


;;
;;  listen
;;
(deftest concatenated-listen.1
  (with-open-stream (stream (make-concatenated-stream))
    (listen stream))
  nil)

(deftest concatenated-listen.2
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (listen stream)))
  t)

(deftest concatenated-listen.3
  (with-input-from-string (input1 "Hello")
    (with-open-stream (input2 (make-concatenated-stream))
      (with-open-stream (stream (make-concatenated-stream input1 input2))
        (listen stream))))
  t)

(deftest concatenated-listen.4
  (with-input-from-string (input1 "Hello")
    (with-open-stream (input2 (make-concatenated-stream))
      (with-open-stream (stream (make-concatenated-stream input2 input1))
        (listen stream))))
  nil)


;;
;;  clear-input
;;
(deftest concatenated-clear-input.1
  (with-open-stream (stream (make-concatenated-stream))
    (clear-input stream))
  nil)

(deftest concatenated-clear-input.2
  (with-open-stream (stream (make-concatenated-stream *standard-input*))
    (clear-input stream))
  nil)


;;
;;  finish-output
;;
(deftest-error concatenated-finish-output.1
  (with-open-stream (stream (make-concatenated-stream))
    (finish-output output)))


;;
;;  force-output
;;
(deftest-error concatenated-force-output.1
  (with-open-stream (stream (make-concatenated-stream))
    (force-output output)))


;;
;;  clear-output
;;
(deftest-error concatenated-clear-output.1
  (with-open-stream (stream (make-concatenated-stream))
    (clear-output output)))


;;
;;  Function MAKE-CONCATENATED-STREAM
;;
(deftest make-concatenated-stream.1
  (typep (make-concatenated-stream) 'concatenated-stream)
  t)

(deftest make-concatenated-stream.2
  (typep (make-concatenated-stream
           *standard-input*
           *terminal-io*) 'concatenated-stream)
  t)

(deftest-error make-concatenated-stream-error.1
  (make-concatenated-stream *standard-input* *standard-output*)
  type-error)

(deftest-error make-concatenated-stream-error.2
  (eval '(make-concatenated-stream 10))
  type-error)

;;  ANSI Common Lisp
(deftest make-concatenated-stream-test.
  (read (make-concatenated-stream
          (make-string-input-stream "1")
          (make-string-input-stream "2")))
  12)


;;
;;  Function CONCATENATED-STREAM-STREAMS
;;
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

(deftest-error concatenated-stream-streams-error.1
  (concatenated-stream-streams
    (make-string-input-stream "Hello"))
  type-error)

(deftest-error concatenated-stream-streams-error.2
  (eval '(concatenated-stream-streams 10))
  type-error)

(deftest-error! concatenated-stream-streams-error.3
  (eval '(concatenated-stream-streams)))

(deftest-error! concatenated-stream-streams-error.4
  (eval '(concatenated-stream-streams
           (make-concatenated-stream)
           nil)))

