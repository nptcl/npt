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

#+fixnum-64
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

#+fixnum-64
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


;;
;;  read-char-no-hang
;;
(deftest file-read-char-no-hang.1
  (with-temp-file
    (with-open-file (input *file*)
      (read-char-no-hang input)))
  #\A)

(deftest file-read-char-no-hang.2
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input)
        (read-char-no-hang input)
        (read-char-no-hang input))))
  #\A #\B #\C)

(deftest-error file-read-char-no-hang.3
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input)
        (read-char-no-hang input)
        (read-char-no-hang input)
        (read-char-no-hang input))))
  end-of-file)

(deftest file-read-char-no-hang.4
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input nil)
        (read-char-no-hang input nil)
        (read-char-no-hang input nil)
        (read-char-no-hang input nil))))
  #\A #\B #\C nil)

(deftest file-read-char-no-hang.5
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof))))
  #\A #\B #\C :eof)

(deftest-error file-read-char-no-hang.6
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (read-char-no-hang input nil :eof))))


;;
;;  unread-char
;;
(deftest file-unread-char.1
  (with-temp-file
    (with-open-file (input *file*)
      (read-char input)
      (unread-char #\Z input)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\Z #\B #\C nil)

(deftest file-unread-char.2
  (with-temp-file
    (with-open-file (input *file*)
      (unread-char #\Z input)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\Z #\A #\B #\C nil)

(deftest-error file-unread-char.3
  (with-temp-file
    (with-overwrite-file (output *file*)
      (read-char input))))

(deftest-error file-unread-char.4
  (with-temp-file
    (with-open-file (output *file* :element-type 'unsigned-byte)
      (read-char input))))


;;
;;  write-char
;;
(deftest file-write-char.1
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (write-char #\A stream)
      (write-char #\B stream))
    (with-open-file (input *file*)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\A #\B nil)

(deftest-error file-write-char.2
  (with-temp-file
    (with-open-file (input *file*)
      (write-char #\A stream))))


;;
;;  read-line
;;
(deftest file-read-line.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-line input)))
  "abcd" t)

(deftest file-read-line.2
  (with-make-file
    (*file* "abcd~%")
    (with-open-file (input *file*)
      (read-line input)))
  "abcd" nil)

(deftest-error file-read-line.3
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-line input)
      (read-line input)))
  end-of-file)

(deftest-error file-read-line.4
  (with-make-file
    (*file* "abcd~%")
    (with-open-file (input *file*)
      (read-line input)
      (read-line input)))
  end-of-file)

(deftest file-read-line.5
  (with-make-file
    (*file* "aaa~%~%bbb")
    (with-open-file (input *file*)
      (values
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof))))
  "aaa" "" "bbb" :eof)

(deftest file-read-line.6
  (with-make-file
    (*file* "aaa~%~%bbb~%")
    (with-open-file (input *file*)
      (values
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof))))
  "aaa" "" "bbb" :eof)

(deftest file-read-line.7
  (with-make-file
    (*file* "aaa~%~%bbb~%~%")
    (with-open-file (input *file*)
      (values
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof))))
  "aaa" "" "bbb" "" :eof)

(deftest-error file-read-line.8
  (with-make-file
    (*file* "aaa")
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (read-line input))))

(deftest-error file-read-line.9
  (with-make-file
    (*file* "aaa")
    (with-overwrite-file (input *file*)
      (read-line input))))


;;
;;  file-length
;;
(deftest file-file-length.1
  (with-temp-file
    (with-open-file (input *file*)
      (file-length input)))
  3)

(deftest file-file-length.2
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (file-length input)))
  6)

(deftest file-file-length.3
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (file-length input)))
  6)


;;
;;  file-position
;;
(deftest file-file-position.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (file-position input)))
  0)

(deftest file-file-position.2
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-char input)
      (file-position input)))
  1)

(deftest file-file-position.3
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-char input)
      (read-char input)
      (unread-char #\a input)
      (file-position input)))
  1)

(deftest file-file-position.4
  (with-temp-file
    (with-overwrite-file (output *file*)
      (file-position output)))
  0)

(deftest file-file-position.5
  (with-temp-file
    (with-overwrite-file (output *file*)
      (format output "Hello")
      (file-position output)))
  5)

(deftest file-file-position.6
  (with-temp-file
    (with-overwrite-file (output *file*)
      (let ((array (make-array 70000 :initial-element #\z)))
        (write-sequence array output)
        (file-position output))))
  70000)

(deftest file-file-position.7
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (read-char input)
      (read-char input)
      (file-position input)))
  2)

(deftest file-file-position-set.1
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-file-position-set.2
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (read-char input)
      (unread-char #\a input)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-file-position-set.3
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-file-position-set.4
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input :start)
        (read-char input))))
  t #\a)

(deftest file-file-position-set.5
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input :end)
        (read-char input nil :eof))))
  t :eof)

(deftest file-file-position-set.6
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :start)))
  t)

(deftest file-file-position-set.7
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :start)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "cdef" t)

(deftest file-file-position-set.8
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :end)))
  t)

(deftest file-file-position-set.9
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :end)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "abccdef" t)

(deftest file-file-position-set.10
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream 2)))
  t)

(deftest file-file-position-set.11
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream 2)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "abcdef" t)


;;
;;  file-string-length
;;
(deftest file-file-string-length.1
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (file-string-length stream #\a)))
  1)

(deftest file-file-string-length.2
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u7F)))
  1)

(deftest file-file-string-length.3
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u80)))
  2)

(deftest file-file-string-length.4
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u0800)))
  3)

(deftest file-file-string-length.5
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream #\uFFFF)))
  2)

(deftest file-file-string-length.6
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream #\u010000)))
  4)

(deftest file-file-string-length.7
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-32)
      (file-string-length stream #\A)))
  4)

(deftest file-file-string-length.8
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (file-string-length stream "abcd")))
  4)

(deftest file-file-string-length.9
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream (format nil "a~A~Ac" #\u80 #\u0811))))
  7)

(deftest file-file-string-length.10
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  10)


;;
;;  stream-external-format
;;
(deftest file-stream-external-format.1
  (with-temp-file
    (with-open-file (input *file*)
      (stream-external-format input)))
  lisp-system::utf-8)

(deftest file-stream-external-format.2
  (with-temp-file
    (with-overwrite-file (input *file* :external-format 'ascii)
      (stream-external-format input)))
  lisp-system::ascii)

(deftest file-stream-external-format.3
  (with-temp-file
    (with-open-file (input *file* :direction :io
                           :if-exists :overwrite
                           :element-type 'unsigned-byte)
      (stream-external-format input)))
  :default)


;;
;;  close
;;
(deftest file-close.1
  (with-temp-file
    (let ((stream (open *file*)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream))))
  t t nil t nil)

(deftest file-close.2
  (with-temp-file
    (let ((stream (open *file*)))
      (values
        (open-stream-p stream)
        (close stream :abort t)
        (open-stream-p stream)
        (close stream :abort t)
        (open-stream-p stream))))
  t t nil t nil)

(deftest file-close.3
  (with-temp-file
    (let ((x (open *file* :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)))
      (close x)
      (probe-file-boolean *file*)))
  t)

(deftest file-close.4
  (let ((x (open *file* :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create)))
    (close x :abort t)
    (probe-file-boolean *file*))
  nil)


;;  after
(deftest file-close-after.1
  (with-temp-file
    (let ((x (open *file*)))
      (values
        (close x)
        (open-stream-p x)
        (input-stream-p x)
        (output-stream-p x)
        (interactive-stream-p x)
        (streamp x)
        (close x))))
  t nil t nil nil t t)

(deftest file-close-after.2
  (with-temp-file
    (let* ((y (pathname *file*))
           (x (open y)))
      (close x)
      (values
        (pathnamep (pathname x))
        (pathnamep (truename x))
        (pathnamep (merge-pathnames x (user-homedir-pathname)))
        (pathnamep (merge-pathnames #p"Hello.txt" x))
        (equal (pathname-host x) (pathname-host y))
        (equal (pathname-device x) (pathname-device y))
        (equal (pathname-name x) (pathname-name y))
        (equal (pathname-directory x) (pathname-directory y))
        (equal (pathname-type x) (pathname-type y))
        (equal (pathname-version x) (pathname-version y))
        (equal (namestring x) (namestring y))
        (equal (file-namestring x) (file-namestring y))
        (equal (directory-namestring x) (directory-namestring y))
        (equal (host-namestring x) (host-namestring y))
        (equal (enough-namestring x) (enough-namestring y))
        (equal (directory x) (directory y))
        (probe-file-boolean x))))
  t t t t t t t t t t t t t t t t t)

(deftest file-close-after.3
  (with-temp-file
    (let ((x (open *file* :direction :output :if-exists :supersede)))
      (format x "Hello")
      (close x)
      (with-open-file (y x :direction :input)
        (read-line y))))
  "Hello" t)

(deftest file-close-after.4
  (with-open-stream (io (make-memory-io-stream))
    (let ((x (open io :direction :output :if-exists :supersede)))
      (format x "Hello")
      (close x)
      (with-open-file (y x :direction :input)
        (read-line y))))
  "Hello" t)


;;
;;  listen
;;
(deftest file-listen.1
  (with-temp-file
    (with-open-file (input *file*)
      (listen input)))
  t)

(deftest-error file-listen.2
  (with-temp-file
    (with-overwrite-file (output *file*)
      (listen output))))

(deftest file-listen.3
  (with-temp-file
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (listen input)))
  t)

(deftest file-listen.4
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (listen input)))
  t)

(deftest file-listen.5
  (with-temp-file
    (with-open-file (output *file* :direction :output :if-exists :supersede))
    (with-open-file (input *file*)
      (read-char input nil nil)
      (listen input)))
  nil)


;;
;;  clear-input
;;
(deftest file-clear-input.1
  (with-temp-file
    (with-open-file (input *file*)
      (clear-input input)))
  nil)

(deftest-error file-clear-input.2
  (with-temp-file
    (with-open-file (input *file* :direction :output :if-exists :overwrite)
      (clear-input input))))

(deftest file-clear-input.3
  (with-temp-file
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (clear-input input)))
  nil)

(deftest file-clear-input.4
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (clear-input input)))
  nil)

(deftest-error file-clear-input.5
  (with-temp-file
    (with-open-file (input *file* :direction :output
                           :if-exists :overwrite :element-type 'unsigned-byte)
      (clear-input input))))

(deftest file-clear-input.6
  (with-temp-file
    (with-open-file (input *file* :direction :io
                           :if-exists :overwrite :element-type 'unsigned-byte)
      (clear-input input)))
  nil)

(deftest file-clear-input.7
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (format x "Hello"))
    (with-open-file (x *file*)
      (values
        (read-char x)
        (clear-input x)
        (read-char x))))
  #\H nil #\e)


;;
;;  finish-output
;;
(deftest file-finish-output.1
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (finish-output x)))
  nil)

(deftest-error file-finish-output.2
  (with-temp-file
    (with-open-file (x *file*)
      (finish-output x))))

(deftest file-finish-output.3
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (format x "Hello")
      (finish-output x)
      (with-open-file (x *file*)
        (read-line x))))
  "Hello" t)


;;
;;  force-output
;;
(deftest file-force-output.1
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (force-output x)))
  nil)

(deftest-error file-force-output.2
  (with-temp-file
    (with-open-file (x *file*)
      (force-output x))))

(deftest file-force-output.3
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (format x "Hello")
      (force-output x)
      (with-open-file (x *file*)
        (read-line x))))
  "Hello" t)


;;
;;  clear-output
;;
(deftest file-clear-output.1
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (clear-output x)))
  nil)

(deftest-error file-clear-output.2
  (with-temp-file
    (with-open-file (x *file*)
      (clear-output x))))

(deftest file-clear-output.3
  (with-temp-file
    (with-open-file (x *file* :direction :output :if-exists :supersede)
      (format x "Hello")
      (clear-output x)
      (format x "ABC"))
    (read-line-1 *file*))
  "HelloABC")

