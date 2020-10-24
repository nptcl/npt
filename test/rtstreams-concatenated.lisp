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

