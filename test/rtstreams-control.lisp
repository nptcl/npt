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
  (let ((array (make-array 8 :element-type '(signed-byte 16) :initial-element -1)))
    (with-open-stream (io (make-memory-io-stream))
      (with-open-file (s io :direction :output :element-type '(signed-byte 16))
        (dolist (x '(10 20 30 -10 -20 -30))
          (write-byte x s)))
      (with-open-file (s io :direction :input :element-type '(signed-byte 16))
        (values
          (read-sequence array s)
          array))))
  6 #(10 20 30 -10 -20 -30 -1 -1))

(deftest read-sequence.4
  (let ((array (vector -1 -1 -1 -1 -1 -1 -1 -1)))
    (with-open-stream (io (make-memory-io-stream))
      (with-open-file (s io :direction :output :element-type '(signed-byte 16))
        (dolist (x '(10 20 30 -10 -20 -30))
          (write-byte x s)))
      (with-open-file (s io :direction :input :element-type '(signed-byte 16))
        (values
          (read-sequence array s)
          array))))
  6 #(10 20 30 -10 -20 -30 -1 -1))

;;  start
(deftest read-sequence-start.1
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 0)
        array)))
  3 #(#\a #\b #\c a a   a a a a a))

(deftest read-sequence-start.2
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5)
        array)))
  8 #(a a a a a   #\a #\b #\c a a))

(deftest read-sequence-start.3
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 8)
        array)))
  10 #(a a a a a   a a a #\a #\b))

(deftest read-sequence-start.4
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 10)
        array)))
  10 #(a a a a a   a a a a a))

(deftest-error read-sequence-start.5
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (read-sequence array input :start 11))))

(deftest-error read-sequence-start.6
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (read-sequence array input :start -1)))
  type-error)

(deftest read-sequence-start.7
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abcdefg")
      (values
        (read-sequence array input :start 5)
        array)))
  10 #(a a a a a #\a #\b #\c #\d #\e))

;;  end
(deftest-error read-sequence-end.1
  (let ((array (make-array 6 :initial-element 'a)))
    (with-input-from-string (input "abcdefghi")
      (read-sequence array input :end 7))))

(deftest read-sequence-end.2
  (let ((array (make-array 6 :initial-element 'a)))
    (with-input-from-string (input "abcdefghi")
      (values
        (read-sequence array input :end nil)
        array)))
  6 #(#\a #\b #\c #\d #\e #\f))

(deftest read-sequence-end.3
  (let ((array (make-array 6 :initial-element 'a)))
    (with-input-from-string (input "abcdefghi")
      (values
        (read-sequence array input :end 6)
        array)))
  6 #(#\a #\b #\c #\d #\e #\f))

(deftest read-sequence-end.4
  (let ((array (make-array 6 :initial-element 'a)))
    (with-input-from-string (input "abcdefghi")
      (values
        (read-sequence array input :end 5)
        array)))
  5 #(#\a #\b #\c #\d #\e a))

(deftest read-sequence-end.5
  (let ((array (make-array 6 :initial-element 'a)))
    (with-input-from-string (input "abcdefghi")
      (values
        (read-sequence array input :end 0)
        array)))
  0 #(a a a a a a))

(deftest-error read-sequence-end.6
  (let ((array (make-array 6 :initial-element 'a)))
    (with-input-from-string (input "abcdefghi")
      (read-sequence array input :end -1)))
  type-error)

;;  start-end
(deftest read-sequence-start-end.1
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5 :end 7)
        array)))
  7 #(a a a a a #\a #\b a a a))

(deftest read-sequence-start-end.2
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5 :end 5)
        array)))
  5 #(a a a a a   a a a a a))

(deftest-error read-sequence-start-end.3
  (let ((array (make-array 10 :initial-element 'a)))
    (with-input-from-string (input "abc")
      (read-sequence array input :start 6 :end 5))))

;;  error
(deftest-error read-sequence-error.1
  (eval '(read-sequence 10 *standard-input*))
  type-error)

(deftest-error read-sequence-error.2
  (eval '(read-sequence #(9 8 7) 20))
  type-error)

(deftest-error read-sequence-error.3
  (eval '(read-sequence #(9 8 7) *standard-output*))
  type-error)

(deftest-error! read-sequence-error.4
  (eval '(read-sequence #(9 8 7))))

(deftest-error read-sequence-error.5
  (eval '(read-sequence #(9 8 7) *standard-input* :start)))

(deftest-error read-sequence-error.6
  (eval '(read-sequence #(9 8 7) *standard-input* :start #\a)))

(deftest-error read-sequence-error.7
  (eval '(read-sequence #(9 8 7) *standard-input* :hello 10)))

;;  ANSI Common Lisp
(deftest read-sequence-test.1
  (let ((data (make-array 15 :initial-element nil)))
    (values
      (read-sequence data (make-string-input-stream "test string"))
      data))
  11 #(#\t #\e #\s #\t #\Space #\s #\t #\r #\i #\n #\g NIL NIL NIL NIL))


;;
;;  Function WRITE-SEQUENCE
;;
(deftest write-sequence.1
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream))
  "abcdef")

(deftest write-sequence.2
  (with-open-stream (stream (make-string-output-stream))
    (write-sequence "abcdef" stream))
  "abcdef")

(deftest write-sequence.3
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :output)
      (write-sequence "ABCDEFG" stream)
      (file-position stream :start)
      (write-sequence "abcd" stream))
    (read-line-1 io))
  "abcdEFG")

(defun read-byte-list (stream)
  (let ((v (read-byte stream nil nil)))
    (when v
      (cons v (read-byte-list stream)))))

(deftest write-sequence.4
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :output :element-type '(signed-byte 32))
      (write-sequence '(10 20 30 -40 -50 -60) stream))
    (with-open-file (stream io :direction :input :element-type '(signed-byte 32))
      (read-byte-list stream)))
  (10 20 30 -40 -50 -60))

;;  start
(deftest-error write-sequence-start.1
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream :start -1)))

(deftest write-sequence-start.2
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 0))
  "Zabcdef")

(deftest write-sequence-start.3
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 4))
  "Zef")

(deftest write-sequence-start.4
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 6))
  "Z")

(deftest-error write-sequence-start.5
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream :start 7)))

;;  end
(deftest-error write-sequence-end.1
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream :end -1)))

(deftest write-sequence-end.2
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :end 0))
  "Z")

(deftest write-sequence-end.3
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :end 3))
  "Zabc")

(deftest write-sequence-end.4
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :end 6))
  "Zabcdef")

(deftest-error write-sequence-end.5
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream :end 7)))

;;  start-end
(deftest write-sequence-start-end.1
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 2 :end 3))
  "Zc")

(deftest write-sequence-start-end.2
  (with-open-stream (stream (make-string-output-stream))
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 2 :end 3))
  "abcdef")

(deftest write-sequence-start-end.3
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 3 :end 3))
  "Z")

(deftest-error write-sequence-start-end.4
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream :start 4 :end 3)))

;;  error
(deftest-error write-sequence-error.1
  (eval '(write-sequence 10 *standard-output*))
  type-error)

(deftest-error write-sequence-error.2
  (eval '(write-sequence #(9 8 7) 20))
  type-error)

(deftest-error write-sequence-error.3
  (eval '(write-sequence #(9 8 7) *standard-input*))
  type-error)

(deftest-error! write-sequence-error.4
  (eval '(write-sequence #(9 8 7))))

(deftest-error write-sequence-error.5
  (eval '(write-sequence #(9 8 7) *standard-output* :start)))

(deftest-error write-sequence-error.6
  (eval '(write-sequence #(9 8 7) *standard-output* :start #\a)))

(deftest-error write-sequence-error.7
  (eval '(write-sequence #(9 8 7) *standard-output* :hello 10)))

;;  ANSI Common Lisp
(deftest write-sequence-test.1
  (with-output-to-string (stream)
    (write-sequence "bookworms" stream :end 4))
  "book")

(deftest write-sequence-test.2
  (with-open-stream (stream (make-string-output-stream))
    (write-sequence "bookworms" stream :end 4))
  "bookworms")


;;
;;  Function FILE-LENGTH
;;
(deftest file-length.1
  (with-open-stream (x (make-memory-input-stream #(1 2 3)))
    (file-length x))
  3)

(deftest file-length.2
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :output :element-type '(unsigned-byte 16))
      (write-byte 10 stream)
      (write-byte 20 stream)
      (write-byte 30 stream)
      (finish-output stream)
      (file-length stream)))
  3)

(deftest file-length.3
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :output :element-type '(signed-byte 32))
      (write-byte 10 stream)
      (write-byte 20 stream)
      (write-byte 30 stream)
      (finish-output stream)
      (file-length stream)))
  3)

(deftest-error file-length-error.1
  (eval '(file-length 10))
  type-error)

(deftest-error! file-length-error.2
  (eval '(file-length)))

(deftest-error! file-length-error.3
  (eval '(file-length *standard-input* 20)))

;;  ANSI Common Lisp
(deftest file-length-test.1
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (s io :direction :output)
      (princ "0123456789" s))
    (with-open-file (s io)
      (file-length s)))
  10)


;;
;;  Function FILE-POSITION
;;
(deftest file-position.1
  (with-open-stream (x (make-memory-input-stream #(11 22 33 44 55)))
    (file-position x))
  0)

(deftest file-position.2
  (with-open-stream (x (make-memory-input-stream #(11 22 33 44 55)))
    (values
      (read-byte x)
      (read-byte x)
      (file-position x)))
  11 22 2)

(deftest file-position.3
  (with-open-stream (x (make-memory-input-stream #(11 22 33 44 55)))
    (values
      (read-byte x)
      (read-byte x)
      (file-position x :start)
      (file-position x)))
  11 22 t 0)

(deftest file-position.4
  (with-open-stream (x (make-memory-input-stream #(11 22 33 44 55)))
    (values
      (read-byte x)
      (read-byte x)
      (file-position x :end)
      (file-position x)))
  11 22 t 5)

(deftest file-position.5
  (with-open-stream (x (make-memory-input-stream #(11 22 33 44 55)))
    (values
      (read-byte x)
      (read-byte x)
      (file-position x 1)
      (file-position x)
      (read-byte x)))
  11 22 t 1 22)

(deftest file-position-binary.1
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :output :element-type '(unsigned-byte 16))
      (dolist (x '(11 22 33 44 55))
        (write-byte x stream)))
    (with-open-file (stream io :element-type '(unsigned-byte 16))
      (values
        (read-byte stream)
        (read-byte stream)
        (file-position stream))))
  11 22 2)

(deftest file-position-binary.2
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :output :element-type '(signed-byte 16))
      (dolist (x '(11 22 33 44 55))
        (write-byte x stream)))
    (with-open-file (stream io :element-type '(signed-byte 16))
      (values
        (read-byte stream)
        (read-byte stream)
        (file-position stream 4)
        (read-byte stream))))
  11 22 t 55)

(deftest-error file-position-error.1
  (eval '(file-position 10))
  type-error)

(deftest-error file-position-error.2
  (eval '(file-position *standard-input* :hello))
  type-error)

(deftest-error! file-position-error.3
  (eval '(file-position)))

(deftest-error! file-position-error.4
  (eval '(file-position *standard-input* :end :end)))

;;  ANSI Common Lisp
(defun file-position-tester ()
  (with-open-stream (io (make-memory-io-stream))
    (let ((noticed '()))
      (flet ((notice (x) (push x noticed) x))
        (with-open-file (s io
                           :element-type '(unsigned-byte 8)
                           :direction :output)
          (notice (file-position s)) ;1
          (write-byte 5 s)
          (write-byte 6 s)
          (let ((p (file-position s)))
            (notice p) ;2
            (notice (when p (file-position s (1- p))))) ;3
          (write-byte 7 s)
          (notice (file-position s))) ;4
        (with-open-file (s io
                           :element-type '(unsigned-byte 8)
                           :direction :input)
          (notice (file-position s)) ;5
          (let ((length (file-length s)))
            (notice length) ;6
            (when length
              (dotimes (i length)
                (notice (read-byte s)))))) ;7,...
        (nreverse noticed)))))

(deftest file-position-test.1
  (file-position-tester)
  (0 2 t 2 0 2 5 7))


;;
;;  Function FILE-STRING-LENGTH
;;
(deftest file-string-length.1
  (file-string-length *standard-output* #\A)
  1)

(deftest file-string-length.2
  (file-string-length *standard-output* #\u3042)
  3)

(deftest file-string-length.3
  (file-string-length *terminal-io* "Hello")
  5)

(deftest file-string-length.4
  (file-string-length
    *terminal-io*
    (coerce '(#\u3042 #\u3044 #\u3046) 'string))
  9)

(deftest-error file-string-length-error.1
  (with-open-stream (io (make-memory-output-stream))
    (file-string-length io #\u3042))
  type-error)

(deftest-error file-string-length-error.2
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :input)
      (file-string-length stream #\u3042)))
  type-error)

(deftest-error file-string-length-error.3
  (eval '(file-string-length 10 #\A))
  type-error)

(deftest-error file-string-length-error.4
  (eval '(file-string-length *standard-output* 20))
  type-error)

(deftest-error! file-string-length-error.5
  (eval '(file-string-length *standard-output*)))

(deftest-error! file-string-length-error.6
  (eval '(file-string-length *standard-output* #\A #\B)))


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

