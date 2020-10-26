;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  Function READ-SEQUENCE
;;
(deftest read-sequence.1
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "Hello a")
      (values
        (read-sequence array input)
        array)))
  7 #(#\H #\e #\l #\l #\o #\Space #\a :a :a :a))

(deftest read-sequence.2
  (let ((array (make-array 6 :element-type 'character :initial-element #\X)))
    (with-input-from-string (input "ABCDEFGHIJK")
      (values
        (read-sequence array input)
        array)))
  6 "ABCDEF")



(deftest read-sequence.3
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5)
        array)))
  8 #(:a :a :a :a :a #\a #\b #\c :a :a))

(deftest read-sequence.4
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5 :end 7)
        array)))
  7 #(:a :a :a :a :a #\a #\b :a :a :a))

(deftest read-sequence.5
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "abcdefg")
      (values
        (read-sequence array input :start 5)
        array)))
  10 #(:a :a :a :a :a #\a #\b #\c #\d #\e))


;;
;;  write-sequence
;;
(deftest write-sequence.1
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream))
  "abcdef")

(deftest write-sequence.2
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 4))
  "Zef")

(deftest write-sequence.3
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :end 3))
  "Zabc")

(deftest write-sequence.4
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 2 :end 3))
  "Zc")

(deftest write-sequence.5
  (with-open-stream (stream (make-string-output-stream))
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 2 :end 3))
  "abcdef")


;;
;;  file-length
;;
(deftest file-length-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (file-length input)))
  3)

(deftest file-length-file.2
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (file-length input)))
  6)

(deftest file-length-file.3
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (file-length input)))
  6)

(deftest file-length-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-length stream))
  0)

(deftest file-length-broadcast.2
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (input2 *file2* :direction :io :if-exists :overwrite)
          (with-open-stream (stream (make-broadcast-stream input1 input2))
            (file-length stream))))))
  4)

(deftest-error file-length-concatenated.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input1 *file1*)
        (with-open-file (input2 *file2*)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (file-length stream)))))))

(deftest-error file-length-echo.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input *file1*)
        (with-overwrite-file (output *file2*)
          (with-open-stream (stream (make-echo-stream input output))
            (file-length stream)))))))

(deftest file-length-synonym.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (declare (special input))
      (with-open-stream (stream (make-synonym-stream 'input))
        (file-length stream))))
  4)

(deftest-error file-length-two-way.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input *file1*)
        (with-overwrite-file (output *file2*)
          (with-open-stream (stream (make-echo-stream input output))
            (file-length stream)))))))

(deftest-error file-length-input-stream.1
  (with-input-from-string (stream "Hello")
    (file-length stream)))

(deftest-error file-length-output-stream.1
  (with-output-to-string (stream)
    (format stream "Hello")
    (file-length stream)))

(deftest-error file-length-extend-stream.1
  (with-extend-to-string
    (stream array)
    (format stream "Hello")
    (file-length stream)))


;;
;;  file-position
;;
(deftest file-position-file.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (file-position input)))
  0)

(deftest file-position-file.2
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-char input)
      (file-position input)))
  1)

(deftest file-position-file.3
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-char input)
      (read-char input)
      (unread-char #\a input)
      (file-position input)))
  1)

(deftest file-position-file.4
  (with-temp-file
    (with-overwrite-file (output *file*)
      (file-position output)))
  0)

(deftest file-position-file.5
  (with-temp-file
    (with-overwrite-file (output *file*)
      (format output "Hello")
      (file-position output)))
  5)

(deftest file-position-file.6
  (with-temp-file
    (with-overwrite-file (output *file*)
      (let ((array (make-array 70000 :initial-element #\z)))
        (write-sequence array output)
        (file-position output))))
  70000)

(deftest file-position-file.7
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (read-char input)
      (read-char input)
      (file-position input)))
  2)

(deftest file-position-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-position stream))
  0)

(deftest file-position-broadcast.2
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdef")
      (with-open-file (input1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (input2 *file2* :direction :io :if-exists :overwrite)
          (read-char input1)
          (read-char input1)
          (read-char input2)
          (read-char input2)
          (read-char input2)
          (with-open-stream (stream (make-broadcast-stream input1 input2))
            (file-position stream))))))
  3)

(deftest file-position-concatenated.1
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream)))
  nil)

(deftest file-position-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream))))
  nil)

(deftest file-position-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'hello))
      (file-position input)))
  2)

(deftest file-position-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream))))
  nil)

(deftest file-position-input-string.1
  (with-input-from-string (input "Hello")
    (read-char input)
    (read-char input)
    (read-char input)
    (file-position input))
  3)

(deftest file-position-output-string.1
  (with-open-stream (stream (make-string-output-stream))
    (format stream "abc")
    (file-position stream))
  3)

(deftest file-position-extend-string.1
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (file-position stream))
  3)


;;
;;  file-position set
;;
(deftest file-position-set-file.1
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-position-set-file.2
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

(deftest file-position-set-file.3
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-position-set-file.4
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input :start)
        (read-char input))))
  t #\a)

(deftest file-position-set-file.5
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input :end)
        (read-char input nil :eof))))
  t :eof)

(deftest file-position-set-file.6
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :start)))
  t)

(deftest file-position-set-file.7
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :start)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "cdef" t)

(deftest file-position-set-file.8
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :end)))
  t)

(deftest file-position-set-file.9
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :end)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "abccdef" t)

(deftest file-position-set-file.10
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream 2)))
  t)

(deftest file-position-set-file.11
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream 2)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "abcdef" t)

(deftest file-position-set-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-position stream :start))
  nil)

(deftest file-position-set-broadcast.2
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream :start)
              (read-char stream1)
              (read-char stream2)))))))
  t #\H #\a)

(deftest file-position-set-broadcast.3
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream :end)
              (read-char stream1 nil :eof)
              (read-char stream2 nil :eof)))))))
  t :eof :eof)

(deftest file-position-set-broadcast.4
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream 2)
              (read-char stream1)
              (read-char stream2)))))))
  t #\l #\c)

(deftest file-position-set-concatenated.1
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream :start)))
  nil)

(deftest file-position-set-concatenated.2
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream :end)))
  nil)

(deftest file-position-set-concatenated.3
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream 2)))
  nil)

(deftest file-position-set-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream :start))))
  nil)

(deftest file-position-set-echo.2
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream :end))))
  nil)

(deftest file-position-set-echo.3
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream 3))))
  nil)

(deftest file-position-set-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream :start)
        (read-char stream))))
  t #\H)

(deftest file-position-set-synonym.2
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream :end)
        (read-char stream nil))))
  t nil)

(deftest file-position-set-synonym.3
  (with-input-from-string (input "abcdef")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream 3)
        (read-char stream))))
  t #\d)

(deftest file-position-set-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream :start))))
  nil)

(deftest file-position-set-two-way.2
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream :end))))
  nil)

(deftest file-position-set-two-way.3
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream 3))))
  nil)

(deftest file-position-set-input-string.1
  (with-input-from-string (input "abcdef")
    (read-char input)
    (read-char input)
    (values
      (file-position input :start)
      (read-char input)))
  t #\a)

(deftest file-position-set-input-string.2
  (with-input-from-string (input "abcdef")
    (values
      (file-position input :end)
      (read-char input nil)))
  t nil)

(deftest file-position-set-input-string.3
  (with-input-from-string (input "abcdef")
    (values
      (file-position input 3)
      (read-char input)))
  t #\d)

(deftest file-position-set-output-string.1
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output :start)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "defg")

(deftest file-position-set-output-string.2
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output :end)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "abcdefg")

(deftest file-position-set-output-string.3
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output 2)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "abdefg")

(deftest file-position-set-extend-string.1
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream :start)
      (progn
        (format stream "defg")
        array)))
  t "defg")

(deftest file-position-set-extend-string.2
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream :end)
      (progn
        (format stream "defg")
        array)))
  t "abcdefg")

(deftest file-position-set-extend-string.3
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream 2)
      (progn
        (format stream "defg")
        array)))
  t "abdefg")


;;
;;  file-string-length
;;
(deftest file-string-length-file.1
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (file-string-length stream #\a)))
  1)

(deftest file-string-length-file.2
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u7F)))
  1)

(deftest file-string-length-file.3
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u80)))
  2)

(deftest file-string-length-file.4
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u0800)))
  3)

(deftest file-string-length-file.5
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream #\uFFFF)))
  2)

(deftest file-string-length-file.6
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream #\u010000)))
  4)

(deftest file-string-length-file.7
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-32)
      (file-string-length stream #\A)))
  4)

(deftest file-string-length-file.8
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (file-string-length stream "abcd")))
  4)

(deftest file-string-length-file.9
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream (format nil "a~A~Ac" #\u80 #\u0811))))
  7)

(deftest file-string-length-file.10
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  10)

(deftest file-string-length-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (values
      (file-string-length stream #\a)
      (file-string-length stream "abcd")))
  1 1)

(deftest file-string-length-broadcast.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file1* :external-format 'utf-8)
      (with-overwrite-file (output2 *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)

(deftest-error file-string-length-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (values
      (file-string-length stream #\a)
      (file-string-length stream "abcd"))))

(deftest file-string-length-echo.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :external-format 'utf-8)
      (with-overwrite-file (output *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)

(deftest file-string-length-synonym.1
  (with-temp-file
    (with-overwrite-file (hello *file* :external-format 'utf-32)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (values
          (file-string-length stream #\a)
          (file-string-length stream "abcd")))))
  4 16)

(deftest file-string-length-two-way.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :external-format 'utf-8)
      (with-overwrite-file (output *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)

(deftest-error file-string-length-input-string.1
  (with-input-from-string (stream "Hello")
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111)))))

(deftest file-string-length-output-string.1
  (with-open-stream (stream (make-string-output-stream))
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  1 4)

(deftest file-string-length-extend-string.1
  (with-extend-to-string
    (stream array)
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  1 4)

(deftest stream-external-format.1
  (with-temp-file
    (with-open-file (input *file*)
      (stream-external-format input)))
  lisp-system::utf-8)

(deftest stream-external-format.2
  (with-temp-file
    (with-overwrite-file (input *file* :external-format 'ascii)
      (stream-external-format input)))
  lisp-system::ascii)

(deftest stream-external-format.3
  (with-temp-file
    (with-open-file (input *file* :direction :io
                           :if-exists :overwrite
                           :element-type 'unsigned-byte)
      (stream-external-format input)))
  (unsigned-byte 8))


;;
;;  listen
;;
(deftest listen.1
  (listen *standard-input*)
  nil)

(deftest listen.2
  (prog2
    (unread-char #\a *standard-input*)
    (listen *standard-input*)
    (read-char *standard-input*))
  t)

(deftest-error listen.3
  (listen *standard-output*))

(deftest listen-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (listen input)))
  t)

(deftest-error listen-file.2
  (with-temp-file
    (with-overwrite-file (output *file*)
      (listen output))))

(deftest listen-file.3
  (with-temp-file
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (listen input)))
  t)

(deftest listen-file.4
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (listen input)))
  t)

(deftest-error listen-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (listen stream)))

(deftest listen-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (listen stream))
  nil)

(deftest listen-concatenated.2
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (listen stream)))
  t)

(deftest listen-concatenated.3
  (with-input-from-string (input1 "Hello")
    (with-open-stream (input2 (make-concatenated-stream))
      (with-open-stream (stream (make-concatenated-stream input1 input2))
        (listen stream))))
  t)

(deftest listen-concatenated.4
  (with-input-from-string (input1 "Hello")
    (with-open-stream (input2 (make-concatenated-stream))
      (with-open-stream (stream (make-concatenated-stream input2 input1))
        (listen stream))))
  nil)

(deftest listen-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (listen stream))))
  t)

(deftest listen-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (with-open-stream (stream (make-synonym-stream 'input))
      (listen stream)))
  t)

(deftest listen-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (listen stream))))
  t)

(deftest listen-input-string.1
  (with-input-from-string (stream "Hello")
    (listen stream))
  t)

(deftest-error listen-output-string.1
  (with-output-to-string (stream)
    (listen stream)))

(deftest-error listen-extend-string.1
  (with-extend-to-string
    (stream array)
    (listen stream)))


;;
;;  clear-input
;;
(deftest clear-input.1
  (clear-input *standard-input*)
  nil)

(deftest clear-input-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (clear-input input)))
  nil)

(deftest-error clear-input-file.2
  (with-temp-file
    (with-open-file (input *file* :direction :output :if-exists :overwrite)
      (clear-input input))))

(deftest clear-input-file.3
  (with-temp-file
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (clear-input input)))
  nil)

(deftest clear-input-file.4
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (clear-input input)))
  nil)

(deftest-error clear-input-file.5
  (with-temp-file
    (with-open-file (input *file* :direction :output
                           :if-exists :overwrite :element-type 'unsigned-byte)
      (clear-input input))))

(deftest clear-input-file.6
  (with-temp-file
    (with-open-file (input *file* :direction :io
                           :if-exists :overwrite :element-type 'unsigned-byte)
      (clear-input input)))
  nil)

(deftest-error clear-input-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (clear-input stream)))

(deftest clear-input-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (clear-input stream))
  nil)

(deftest clear-input-concatenated.2
  (with-open-stream (stream (make-concatenated-stream *standard-input*))
    (clear-input stream))
  nil)

(deftest clear-input-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (clear-input stream))
  nil)

(deftest clear-input-synonym.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (clear-input stream))
  nil)

(deftest clear-input-two-way.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (clear-input stream))
  nil)

(deftest clear-input-input-string.1
  (with-input-from-string (input "Hello")
    (read-char input)
    (unread-char #\H input)
    (values
      (clear-input input)
      (read-char input)))
  nil #\H)

(deftest-error clear-input-output-string.1
  (with-output-to-string (stream)
    (clear-input stream)))

(deftest-error clear-input-extend-string.1
  (with-extend-to-string
    (stream array)
    (clear-input stream)))


;;
;;  finish-output, force-output, clear-output
;;
(deftest finish-output-file.1
  (with-temp-file
    (with-overwrite-file (output *file*)
      (values
        (progn
          (write-char #\a output)
          (finish-output output))
        (progn
          (write-char #\b output)
          (force-output output))
        (progn
          (write-char #\c output)
          (clear-output output)))))
  nil nil nil)

(deftest finish-output-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (values
      (finish-output stream)
      (force-output stream)
      (clear-output stream)))
  nil nil nil)

(deftest finish-output-broadcast.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file*)
      (with-overwrite-file (output2 *file*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (values
            (progn
              (write-char #\a stream)
              (finish-output stream))
            (progn
              (write-char #\b stream)
              (force-output stream))
            (progn
              (write-char #\c stream)
              (clear-output stream)))))))
  nil nil nil)

(deftest-error finish-output-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (finish-output output)))

(deftest-error finish-output-concatenated.2
  (with-open-stream (stream (make-concatenated-stream))
    (force-output output)))

(deftest-error finish-output-concatenated.3
  (with-open-stream (stream (make-concatenated-stream))
    (clear-output output)))

(deftest finish-output-echo.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (progn
              (write-char #\a stream)
              (finish-output stream))
            (progn
              (write-char #\b stream)
              (force-output stream))
            (progn
              (write-char #\c stream)
              (clear-output stream)))))))
  nil nil nil)

(deftest finish-output-synonym.1
  (with-temp-file
    (with-overwrite-file (output *file*)
      (declare (special output))
      (with-open-stream (stream (make-synonym-stream 'output))
        (values
          (progn
            (write-char #\a stream)
            (finish-output stream))
          (progn
            (write-char #\b stream)
            (force-output stream))
          (progn
            (write-char #\c stream)
            (clear-output stream))))))
  nil nil nil)

(deftest finish-output-two-way.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (progn
              (write-char #\a stream)
              (finish-output stream))
            (progn
              (write-char #\b stream)
              (force-output stream))
            (progn
              (write-char #\c stream)
              (clear-output stream)))))))
  nil nil nil)

(deftest-error finish-output-input-stream.1
  (with-input-from-string (stream "Hello")
    (finish-output stream)))

(deftest-error finish-output-input-stream.2
  (with-input-from-string (stream "Hello")
    (force-output stream)))

(deftest-error finish-output-input-stream.3
  (with-input-from-string (stream "Hello")
    (clear-output stream)))

(deftest finish-output-output-stream.1
  (with-open-stream (stream (make-string-output-stream))
    (values
      (progn
        (write-char #\a stream)
        (finish-output stream))
      (progn
        (write-char #\b stream)
        (force-output stream))
      (progn
        (write-char #\c stream)
        (clear-output stream))))
  nil nil nil)

(deftest finish-output-extend-stream.1
  (with-extend-to-string
    (stream array)
    (values
      (progn
        (write-char #\a stream)
        (finish-output stream))
      (progn
        (write-char #\b stream)
        (force-output stream))
      (progn
        (write-char #\c stream)
        (clear-output stream))))
  nil nil nil)


;;
;;  close
;;
(deftest close-file.1
  (with-temp-file
    (let ((stream (open *file*)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream))))
  t t nil t nil)

(deftest close-file.2
  (with-temp-file
    (let ((stream (open *file*)))
      (values
        (open-stream-p stream)
        (close stream :abort t)
        (open-stream-p stream)
        (close stream :abort t)
        (open-stream-p stream))))
  t t nil t nil)

(deftest close-broadcast.1
  (with-open-stream (output (make-string-output-stream))
    (let ((stream (make-broadcast-stream output)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p output))))
  t t nil t nil t)

(deftest close-concatenated.1
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

(deftest close-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (let ((stream (make-echo-stream input output)))
        (values
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (open-stream-p input)
          (open-stream-p output)))))
  t t nil t nil t t)

(deftest close-synonym.1
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

(deftest close-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (let ((stream (make-two-way-stream input output)))
        (values
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (open-stream-p input)
          (open-stream-p output)))))
  t t nil t nil t t)

(deftest close-input-string.1
  (let ((stream (make-string-input-stream "Hello")))
    (values
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)))
  t t nil t nil)

(deftest close-input-string.2
  (let ((stream (make-string-output-stream)))
    (values
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)))
  t t nil t nil)

(deftest close-input-stream-p-file.1
  (with-temp-file
    (let ((input (open *file*)))
      (close input)
      (input-stream-p input)))
  t)

(deftest close-input-stream-p-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (close stream)
    (input-stream-p stream))
  nil)

(deftest close-input-stream-p-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (close stream)
    (input-stream-p stream))
  t)

(deftest close-output-stream-p.1
  (with-temp-file
    (let ((input (open *file* :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)))
      (close input)
      (output-stream-p input)))
  t)

(deftest close-interactive-stream-p.1
  (with-temp-file
    (let ((input (open *file*)))
      (close input)
      (interactive-stream-p input)))
  nil)

(deftest close-open-stream-p.1
  (with-temp-file
    (let ((input (open *file*)))
      (values
        (open-stream-p input)
        (progn
          (close input)
          (open-stream-p input)))))
  t nil)


;;  file-stream
;;  broadcast-stream
;;  concatenated-stream
;;  echo-stream
;;  synonym-stream
;;  two-way-stream
;;  input-string-stream
;;  output-string-stream
;;  extend-string-stream
;;  prompt-stream

