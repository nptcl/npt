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


;;
;;  unsigned-byte
;;
(defmacro with-temp-write-element-type ((var type) &body body)
  `(with-open-file (,var *file* :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type ,type)
     ,@body))

(defmacro with-temp-read-element-type ((var type) &body body)
  `(with-open-file (,var *file* :direction :input :element-type ,type)
     ,@body))

(deftest read-byte-unsigned-8.1
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #xAB stream)
      (write-byte #x00 stream)
      (write-byte #x23 stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 8))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#xAB #x00 #x23 :eof))

(deftest-error read-byte-unsigned-8.2
  (with-temp-write-element-type
    (stream '(unsigned-byte 8))
    (write-byte #x0100 stream)))

(deftest-error read-byte-unsigned-8.3
  (with-temp-write-element-type
    (stream '(unsigned-byte 8))
    (write-byte -1 stream)))

(deftest read-byte-unsigned-16.1
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 16))
      (write-byte #xABCD stream)
      (write-byte #x0000 stream)
      (write-byte #x2345 stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 16))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#xABCD #x0000 #x2345 :eof))

(deftest read-byte-unsigned-16.2
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #xAB stream)
      (write-byte #xCD stream)
      (write-byte #xEF stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 16))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (mapcar (lambda (x)
              (if (eql x #xCDAB) #xABCD x))
            (nreverse list)))
  (#xABCD :eof :eof :eof))

(deftest-error read-byte-unsigned-16.3
  (with-temp-write-element-type
    (stream '(unsigned-byte 16))
    (write-byte #x010000 stream)))

(deftest-error read-byte-unsigned-16.4
  (with-temp-write-element-type
    (stream '(unsigned-byte 16))
    (write-byte -1 stream)))

(deftest read-byte-unsigned-32.1
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 32))
      (write-byte #xABCDEF01 stream)
      (write-byte #x00000000 stream)
      (write-byte #x23456789 stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 32))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#xABCDEF01 #x00000000 #x23456789 :eof))

(deftest read-byte-unsigned-32.2
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #xAB stream)
      (write-byte #xCD stream)
      (write-byte #xEF stream)
      (write-byte #x01 stream)
      (write-byte #x23 stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 32))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (mapcar (lambda (x)
              (if (eql x #x01EFCDAB) #xABCDEF01 x))
            (nreverse list)))
  (#xABCDEF01 :eof :eof :eof))

(deftest-error read-byte-unsigned-32.3
  (with-temp-write-element-type
    (stream '(unsigned-byte 32))
    (write-byte #x0100000000 stream)))

(deftest-error read-byte-unsigned-32.4
  (with-temp-write-element-type
    (stream '(unsigned-byte 32))
    (write-byte -1 stream)))

#+64-bit
(deftest read-byte-unsigned-64.1
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 64))
      (write-byte #xABCDEF0123456789 stream)
      (write-byte #x0000000000000000 stream)
      (write-byte #x23456789ABCDEF01 stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 64))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#xABCDEF0123456789 #x0000000000000000 #x23456789ABCDEF01 :eof))

#+64-bit
(deftest read-byte-unsigned-64.2
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #xAB stream)
      (write-byte #xCD stream)
      (write-byte #xEF stream)
      (write-byte #x01 stream)
      (write-byte #x23 stream)
      (write-byte #x45 stream)
      (write-byte #x67 stream)
      (write-byte #x89 stream)
      (write-byte #xAB stream))
    (with-temp-read-element-type
      (stream '(unsigned-byte 64))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (mapcar (lambda (x)
              (if (eql x #x8967452301EFCDAB) #xABCDEF01234567 x))
            (nreverse list)))
  (#xABCDEF01234567 :eof :eof :eof))

#+64-bit
(deftest-error read-byte-unsigned-64.3
  (with-temp-write-element-type
    (stream '(unsigned-byte 64))
    (write-byte #x010000000000000000 stream)))

#+64-bit
(deftest-error read-byte-unsigned-64.4
  (with-temp-write-element-type
    (stream '(unsigned-byte 64))
    (write-byte -1 stream)))


;;
;;  signed-byte
;;
(deftest read-byte-signed-8.1
  (let (list)
    (with-temp-write-element-type
      (stream '(signed-byte 8))
      (write-byte #x7F stream)
      (write-byte #x00 stream)
      (write-byte #x-80 stream))
    (with-temp-read-element-type
      (stream '(signed-byte 8))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#x7F #x00 #x-80 :eof))

(deftest-error read-byte-signed-8.2
  (with-temp-write-element-type
    (stream '(signed-byte 8))
    (write-byte #x80 stream)))

(deftest-error read-byte-signed-8.3
  (with-temp-write-element-type
    (stream '(signed-byte 8))
    (write-byte #x-81 stream)))

(deftest read-byte-signed-16.1
  (let (list)
    (with-temp-write-element-type
      (stream '(signed-byte 16))
      (write-byte #x7FFF stream)
      (write-byte #x0000 stream)
      (write-byte #x-8000 stream))
    (with-temp-read-element-type
      (stream '(signed-byte 16))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#x7FFF #x0000 #x-8000 :eof))

(deftest read-byte-signed-16.2
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #x7B stream)
      (write-byte #x7C stream)
      (write-byte #xEF stream))
    (with-temp-read-element-type
      (stream '(signed-byte 16))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (mapcar (lambda (x)
              (if (eql x #x7C7B) #x7B7C x))
            (nreverse list)))
  (#x7B7C :eof :eof :eof))

(deftest-error read-byte-signed-16.3
  (with-temp-write-element-type
    (stream '(signed-byte 16))
    (write-byte #x8000 stream)))

(deftest-error read-byte-signed-16.4
  (with-temp-write-element-type
    (stream '(signed-byte 16))
    (write-byte #x-8001 stream)))

(deftest read-byte-signed-32.1
  (let (list)
    (with-temp-write-element-type
      (stream '(signed-byte 32))
      (write-byte #x7FFFFFFF stream)
      (write-byte #x00000000 stream)
      (write-byte #x-80000000 stream))
    (with-temp-read-element-type
      (stream '(signed-byte 32))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#x7FFFFFFF #x00000000 #x-80000000 :eof))

(deftest read-byte-signed-32.2
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #x12 stream)
      (write-byte #x34 stream)
      (write-byte #x56 stream)
      (write-byte #x78 stream)
      (write-byte #x1A stream))
    (with-temp-read-element-type
      (stream '(signed-byte 32))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (mapcar (lambda (x)
              (if (eql x #x78563412) #x12345678 x))
            (nreverse list)))
  (#x12345678 :eof :eof :eof))

(deftest-error read-byte-signed-32.3
  (with-temp-write-element-type
    (stream '(signed-byte 32))
    (write-byte #x80000000 stream)))

(deftest-error read-byte-signed-32.4
  (with-temp-write-element-type
    (stream '(signed-byte 32))
    (write-byte #x-80000001 stream)))

#+64-bit
(deftest read-byte-signed-64.1
  (let (list)
    (with-temp-write-element-type
      (stream '(signed-byte 64))
      (write-byte #x7FFFFFFFFFFFFFFF stream)
      (write-byte #x0000000000000000 stream)
      (write-byte #x-8000000000000000 stream))
    (with-temp-read-element-type
      (stream '(signed-byte 64))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (#x7FFFFFFFFFFFFFFF #x0000000000000000 #x-8000000000000000 :eof))

#+64-bit
(deftest read-byte-signed-64.2
  (let (list)
    (with-temp-write-element-type
      (stream '(unsigned-byte 8))
      (write-byte #x12 stream)
      (write-byte #x34 stream)
      (write-byte #x56 stream)
      (write-byte #x78 stream)
      (write-byte #x1A stream)
      (write-byte #x2B stream)
      (write-byte #x3C stream)
      (write-byte #x4D stream)
      (write-byte #x5E stream))
    (with-temp-read-element-type
      (stream '(signed-byte 64))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (mapcar (lambda (x)
              (if (eql x #x4D3C2B1A78563412) #x123456781A2B3C4D x))
            (nreverse list)))
  (#x123456781A2B3C4D :eof :eof :eof))

#+64-bit
(deftest-error read-byte-signed-64.3
  (with-temp-write-element-type
    (stream '(signed-byte 64))
    (write-byte #x8000000000000000 stream)))

#+64-bit
(deftest-error read-byte-signed-64.4
  (with-temp-write-element-type
    (stream '(signed-byte 64))
    (write-byte #x-8000000000000001 stream)))

