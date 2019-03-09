;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  read-byte
;;
(deftest read-byte-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)))
  65)

(deftest read-byte-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)))
  66)

(deftest-error read-byte-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)))
  end-of-file)

(deftest-error read-byte-file.4
  (with-temp-file
    (with-binary-output
      (stream *file*)
      (read-byte stream)))
  type-error)

(deftest-error read-byte-file.5
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (read-byte stream))))

(deftest-error read-byte-file.6
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)))
  end-of-file)

(deftest read-byte-file.7
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (values
        (read-byte stream nil :eof)
        (read-byte stream nil :eof)
        (read-byte stream nil :eof)
        (read-byte stream nil :eof)
        (read-byte stream nil :eof))))
  65 66 67 :eof :eof)

(deftest-error read-byte-broadcast.1
  (with-temp-file
    (with-open-file (output *file* :direction :io :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream output))
        (read-byte stream nil :eof))))
  file-error)

(deftest read-byte-concatenated.1
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-byte input))))
  65)

(deftest-error read-byte-concatenated.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-byte input)
        (read-byte input)
        (read-byte input)
        (read-byte input))))
  end-of-file)

(deftest read-byte-concatenated.3
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

(deftest read-byte-concatenated.4
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

(deftest-error read-byte-concatenated.5
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

(deftest read-byte-echo.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-byte stream)))))
  65)

(deftest read-byte-echo.2
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

(deftest read-byte-echo.3
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

(deftest read-byte-echo.4
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

(deftest-error read-byte-echo.5
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

(deftest read-byte-synonym.1
  (with-temp-file
    (with-open-file (*hello* *file* :element-type 'unsigned-byte)
      (declare (special *hello*))
      (with-open-stream (stream (make-synonym-stream '*hello*))
        (read-byte stream))))
  65)

(deftest read-byte-synonym.2
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

(deftest-error read-byte-synonym.3
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

(deftest read-byte-two-way.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-binary-output
        (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (read-byte stream)))))
  65)

(deftest read-byte-two-way.2
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

(deftest-error read-byte-two-way.3
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

(deftest-error read-byte-input-string.1
  (with-input-from-string (stream "Hello")
    (read-byte stream)))

(deftest-error read-byte-output-string.1
  (with-output-to-string (stream)
    (read-byte stream))
  type-error)

(deftest-error read-byte-extend-string.1
  (with-extend-to-string
    (inst array)
    (read-byte inst))
  type-error)


;;
;;  write-byte
;;
(deftest write-byte-file.1
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

(deftest-error write-byte-file.2
  (with-temp-file
    (with-overwrite-file (output *file*)
      (write-byte 70 output))))

(deftest write-byte-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (write-byte 70 stream))
  70)

(deftest write-byte-broadcast.2
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

(deftest-error write-byte-broadcast.3
  (with-temp-file1-file2
    (with-overwrite-file (output *file*)
      (with-open-stream (stream (make-broadcast-stream output))
        (write-byte 70 stream)))))

(deftest-error write-byte-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (write-byte 70 stream)))

(deftest write-byte-echo.1
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

(deftest-error write-byte-echo.2
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (write-byte 70 stream))))))

(deftest write-byte-synonym.1
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

(deftest-error write-byte-synonym.2
  (with-temp-file
    (with-overwrite-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (write-byte 70 stream)))))

(deftest write-byte-two-way.1
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

(deftest-error write-byte-two-way.2
  (with-temp-file1-file2
    (with-open-file (input *file1* :element-type 'unsigned-byte)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (write-byte 70 stream))))))

(deftest-error write-byte-input-string.1
  (with-input-from-string (input "Hello")
    (write-byte 70 input)))

(deftest-error write-byte-output-string.1
  (with-output-to-string (output)
    (write-byte 70 output)))

(deftest-error write-byte-extend-string.1
  (with-extend-to-string
    (output array)
    (write-byte 70 output)))

