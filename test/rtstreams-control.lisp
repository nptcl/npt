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
  (eval '(let ((array (make-array 10 :initial-element 'a)))
           (with-input-from-string (input "abc")
             (read-sequence array input :start -1))))
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
  (eval '(let ((array (make-array 6 :initial-element 'a)))
           (with-input-from-string (input "abcdefghi")
             (read-sequence array input :end -1))))
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
  (eval '(with-output-to-string (stream)
           (write-sequence "abcdef" stream :start -1)))
  type-error)

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
  (eval '(with-output-to-string (stream)
           (write-sequence "abcdef" stream :end -1)))
  type-error)

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
;;  Function CLOSE
;;
(deftest close.1
  (close (make-string-output-stream))
  t)

(deftest close.2
  (let ((x (make-string-input-stream "Hello")))
    (close x)
    (open-stream-p x))
  nil)

(deftest close.3
  (let ((inst (open *file* :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)))
    (format inst "Hello")
    (close inst :abort t)
    (probe-file-boolean *file*))
  nil)

(deftest close.4
  (let ((inst (open *file* :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)))
    (format inst "Hello")
    (close inst)
    (setq inst (open *file* :direction :input))
    (close inst :abort t)
    (probe-file-boolean *file*))
  t)

(deftest-error close-error.1
  (eval '(close nil))
  type-error)

(deftest-error close-error.2
  (eval '(close (make-string-output-stream) :hello)))

(deftest-error close-error.3
  (eval '(close (make-string-output-stream) :hello 10)))

(deftest-error! close-error.4
  (eval '(close)))


;;
;;  Function LISTEN
;;
(deftest listen.1
  (dotimes (i 100 t)
    (clear-input)
    (unless (listen)
      (return nil)))
  nil)

(deftest listen.2
  (dotimes (i 100 t)
    (clear-input *standard-input*)
    (unless (listen *standard-input*)
      (return nil)))
  nil)

(deftest listen.3
  (dotimes (i 100 nil)
    (unread-char #\a *standard-input*)
    (when (listen)
      (clear-input *standard-input*)
      (return t))
    (clear-input *standard-input*))
  t)

(deftest listen.4
  (dotimes (i 100 nil)
    (unread-char #\a *standard-input*)
    (when (listen *standard-input*)
      (clear-input *standard-input*)
      (return t))
    (clear-input *standard-input*))
  t)

(deftest-error listen-error.1
  (eval '(listen *standard-output*))
  type-error)

(deftest-error listen-error.2
  (eval '(listen 10))
  type-error)

(deftest-error! listen-error.3
  (eval '(listen *standard-output* nil))
  type-error)


;;
;;  Function CLEAR-INPUT
;;
(deftest clear-input.1
  (clear-input)
  nil)

(deftest clear-input.2
  (clear-input *standard-input*)
  nil)

(deftest-error clear-input-error.1
  (eval '(clear-input 10))
  type-error)

(deftest-error clear-input-error.2
  (eval '(clear-input *standard-output*))
  type-error)

(deftest-error! clear-input-error.3
  (eval '(clear-input *standard-input* nil)))


;;
;;  Function FINISH-OUTPUT
;;
(deftest finish-output.1
  (finish-output)
  nil)

(deftest finish-output.2
  (finish-output *standard-output*)
  nil)

(deftest-error finish-output-error.1
  (eval '(finish-output 10))
  type-error)

(deftest-error finish-output-error.2
  (eval '(finish-output *standard-input*))
  type-error)

(deftest-error! finish-output-error.3
  (eval '(finish-output *standard-output* nil)))


;;
;;  Function FORCE-OUTPUT
;;
(deftest force-output.1
  (force-output)
  nil)

(deftest force-output.2
  (force-output *standard-output*)
  nil)

(deftest-error force-output-error.1
  (eval '(force-output 10))
  type-error)

(deftest-error force-output-error.2
  (eval '(force-output *standard-input*))
  type-error)

(deftest-error! force-output-error.3
  (eval '(force-output *standard-output* nil)))


;;
;;  Function CLEAR-OUTPUT
;;
(deftest clear-output.1
  (clear-output)
  nil)

(deftest clear-output.2
  (clear-output *standard-output*)
  nil)

(deftest-error clear-output-error.1
  (eval '(clear-output 10))
  type-error)

(deftest-error clear-output-error.2
  (eval '(clear-output *standard-input*))
  type-error)

(deftest-error! clear-output-error.3
  (eval '(clear-output *standard-output* nil)))

