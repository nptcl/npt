;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest file-input-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (input-stream-p stream)))
  nil)

(deftest file-input-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.4
  (with-temp-file
    (with-open-file (stream *file* :direction :input
                            :element-type '(unsigned-byte 8))
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.5
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (input-stream-p stream)))
  nil)

(deftest file-input-stream-p.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.7
  (values
    (input-stream-p lisp-system::*standard-input*)
    (input-stream-p lisp-system::*standard-output*)
    (input-stream-p lisp-system::*standard-error*))
  t nil nil)


;;
;;  output-stream-p
;;
(deftest file-output-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (output-stream-p stream)))
  nil)

(deftest file-output-stream-p.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.4
  (with-temp-file
    (with-open-file (stream *file* :direction :input
                            :element-type '(unsigned-byte 8))
      (output-stream-p stream)))
  nil)

(deftest file-output-stream-p.5
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.7
  (values
    (output-stream-p lisp-system::*standard-input*)
    (output-stream-p lisp-system::*standard-output*)
    (output-stream-p lisp-system::*standard-error*))
  nil t t)


;;
;;  interactive-stream-p
;;
(deftest file-interactive-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (interactive-stream-p stream)))
  nil)

(deftest file-interactive-stream-p.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (interactive-stream-p stream)))
  nil)

(deftest file-interactive-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (interactive-stream-p stream)))
  nil)

(deftest file-interactive-stream-p.7
  (values
    (interactive-stream-p lisp-system::*standard-input*)
    (interactive-stream-p lisp-system::*standard-output*)
    (interactive-stream-p lisp-system::*standard-error*))
  t t t)


;;
;;  open-stream-p
;;
(deftest file-open-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (open-stream-p stream)))
  t)

(deftest file-open-stream-p.2
  (with-temp-file
    (let ((stream (open *file* :direction :input)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest file-open-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (open-stream-p stream)))
  t)

(deftest file-open-stream-p.4
  (with-temp-file
    (let ((stream (open *file* :direction :output :if-exists :supersede)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest file-open-stream-p.5
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (open-stream-p stream)))
  t)

(deftest file-open-stream-p.6
  (with-temp-file
    (let ((stream (open *file* :direction :io :if-exists :supersede)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest file-open-stream-p.7
  (open-stream-p
    (open #p"test/empty.file" :direction :probe))
  nil)


;;
;;  stream-element-type
;;
(deftest file-stream-element-type.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (stream-element-type stream)))
  character)

(deftest file-stream-element-type.2
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest file-stream-element-type.3
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (stream-element-type stream)))
  character)

(deftest file-stream-element-type.4
  (with-temp-file
    (with-open-file (stream *file* :direction :output
                            :if-exists :supersede :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest file-stream-element-type.5
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (stream-element-type stream)))
  character)

(deftest file-stream-element-type.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io
                            :if-exists :supersede :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(defun file-open-element-type (type)
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type type)
      (stream-element-type stream))))

(deftest file-stream-element-type.7
  (file-open-element-type '(unsigned-byte 3))
  (unsigned-byte 8))

(deftest file-stream-element-type.8
  (file-open-element-type '(unsigned-byte 8))
  (unsigned-byte 8))

(deftest file-stream-element-type.9
  (file-open-element-type '(unsigned-byte 16))
  (unsigned-byte 16))

(deftest file-stream-element-type.10
  (file-open-element-type '(unsigned-byte 32))
  (unsigned-byte 32))

#+64-bit
(deftest file-stream-element-type.11
  (file-open-element-type '(unsigned-byte 64))
  (unsigned-byte 64))

(deftest file-stream-element-type.12
  (file-open-element-type '(signed-byte 8))
  (signed-byte 8))

(deftest file-stream-element-type.13
  (file-open-element-type '(signed-byte 16))
  (signed-byte 16))

(deftest file-stream-element-type.14
  (file-open-element-type '(signed-byte 32))
  (signed-byte 32))

#+64-bit
(deftest file-stream-element-type.15
  (file-open-element-type '(signed-byte 64))
  (signed-byte 64))


;;
;;  read-byte
;;
(deftest file-read-byte.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)))
  65)

(deftest file-read-byte.2
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)))
  66)

(deftest-error file-read-byte.3
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)))
  end-of-file)

(deftest-error file-read-byte.4
  (with-temp-file
    (with-binary-output
      (stream *file*)
      (read-byte stream)))
  type-error)

(deftest-error file-read-byte.5
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (read-byte stream))))

(deftest-error file-read-byte.6
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)))
  end-of-file)

(deftest file-read-byte.7
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (values
        (read-byte stream nil :eof)
        (read-byte stream nil :eof)
        (read-byte stream nil :eof)
        (read-byte stream nil :eof)
        (read-byte stream nil :eof))))
  65 66 67 :eof :eof)


;;
;;  write-byte
;;
(deftest file-write-byte.1
  (with-temp-file
    (with-binary-output
      (output *file*)
      (write-byte 70 output)
      (write-byte 71 output))
    (with-open-file (input *file*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\F #\G :eof)

(deftest-error file-write-byte.2
  (with-temp-file
    (with-overwrite-file (output *file*)
      (write-byte 70 output))))


;;
;;  read-char
;;
(deftest file-read-char.1
  (with-temp-file
    (with-open-file (input *file*)
      (read-char input)))
  #\A)

(deftest file-read-char.2
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char input)
        (read-char input)
        (read-char input))))
  #\A #\B #\C)

(deftest-error file-read-char.3
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char input)
        (read-char input)
        (read-char input)
        (read-char input))))
  end-of-file)

(deftest file-read-char.4
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\A #\B #\C nil)

(deftest file-read-char.5
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\A #\B #\C :eof)

(deftest-error file-read-char.6
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (read-char input nil :eof))))

