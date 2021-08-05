;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  Function MAKE-MEMORY-INPUT-STREAM
;;
(deftest make-memory-input-stream.1
  (typep
    (make-memory-input-stream #())
    'stream)
  t)

(deftest make-memory-input-stream.2
  (typep
    (make-memory-input-stream
      (make-array 5 :element-type '(unsigned-byte 8)))
    'stream)
  t)

(deftest make-memory-input-stream.3
  (let ((x (make-memory-input-stream #(10 20 30))))
    (values
      (read-byte x nil :eof)
      (read-byte x nil :eof)
      (read-byte x nil :eof)
      (read-byte x nil :eof)))
  10 20 30 :eof)

(deftest-error make-memory-input-stream.4
  (let ((x (make-memory-input-stream #(10 20))))
    (read-byte x)
    (read-byte x)
    (read-byte x))
  end-of-file)

(deftest make-memory-input-stream.5
  (let ((x (make-memory-input-stream #(10 255))))
    (read-byte x)
    (read-byte x))
  255)

(deftest-error make-memory-input-stream.6
  (let ((x (make-memory-input-stream #(10 256))))
    (read-byte x)
    (read-byte x)))

(deftest-error make-memory-input-stream.7
  (let ((x (make-memory-input-stream #(10 -1))))
    (read-byte x)
    (read-byte x)))

(deftest make-memory-input-stream.8
  (let ((x (make-memory-input-stream
             (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents #(10 20 30)))))
    (values
      (read-byte x nil :eof)
      (read-byte x nil :eof)
      (read-byte x nil :eof)
      (read-byte x nil :eof)))
  10 20 30 :eof)

(deftest make-memory-input-stream.9
  (let ((x (make-memory-input-stream #(10 20 30))))
    (read-byte x nil :eof)
    (close x))
  t)

(deftest make-memory-input-stream-args.1
  (sysctl
    (make-memory-input-stream nil :size 10)
    'size)
  10)

(deftest make-memory-input-stream-args.2
  (sysctl
    (make-memory-input-stream nil :array 20)
    'array)
  20)


;;
;;  Function MAKE-MEMORY-OUTPUT-STREAM
;;
(deftest make-memory-output-stream.1
  (let ((x (make-memory-output-stream)))
    (write-byte 10 x)
    (write-byte 20 x)
    (write-byte 30 x)
    (close x))
  t)

(deftest make-memory-output-stream.2
  (let ((x (make-memory-output-stream)))
    (write-byte 10 x)
    (write-byte 20 x)
    (write-byte 30 x)
    (get-output-stream-memory x))
  #(10 20 30))

(deftest make-memory-output-stream.3
  (let ((x (make-memory-output-stream)))
    (dotimes (i 256)
      (write-byte i x))
    (let ((y (get-output-stream-memory x)))
      (dotimes (i 256)
        (unless (equal (elt y i) i)
          (error "error")))
      (length y)))
  256)

(deftest make-memory-output-stream-args.1
  (sysctl
    (make-memory-output-stream :size 10)
    'size)
  10)

(deftest make-memory-output-stream-args.2
  (sysctl
    (make-memory-output-stream :array 20)
    'array)
  20)


;;
;;  Function GET-OUTPUT-STREAM-MEMORY
;;
(deftest get-output-stream-memory.1
  (let ((x (make-memory-output-stream)))
    (get-output-stream-memory x))
  #())


;;
;;  Macro WITH-INPUT-FROM-MEMORY
;;
(deftest with-input-from-memory.1
  (let (list)
    (with-input-from-memory (stream #(10 20 30))
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list)
      (push (read-byte stream nil :eof) list))
    (nreverse list))
  (10 20 30 :eof))

(deftest with-input-from-memory.2
  (with-input-from-memory (stream #(10 20 30))
    999)
  999)

(deftest with-input-from-memory.3
  (let (list)
    (with-input-from-memory (stream #(10 20 30) :size 11 :array 22)
      (setq list (list (sysctl stream 'size)
                       (sysctl stream 'array))))
    list)
  (11 22))


;;
;;  Macro WITH-OUTPUT-TO-MEMORY
;;
(deftest with-output-to-memory.1
  (with-output-to-memory (stream)
    (write-byte 10 stream)
    (write-byte 20 stream)
    (write-byte 30 stream))
  #(10 20 30))

(deftest with-output-to-memory.2
  (with-output-to-memory (stream)
    100)
  #())

(deftest with-output-to-memory.3
  (let (list)
    (with-output-to-memory (stream :size 33 :array 44)
      (setq list (list (sysctl stream 'size)
                       (sysctl stream 'array))))
    list)
  (33 44))


;;
;;  open input
;;
(deftest open-input-memory.1
  (with-open-stream (input (make-memory-input-stream #()))
    (with-open-file (stream input)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  :eof :eof)

(deftest open-input-memory.2
  (with-open-stream (input (make-memory-input-stream #(65 66 67 68)))
    (with-open-file (stream input)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  #\A #\B #\C #\D :eof :eof)

(deftest open-input-memory.3
  (with-open-stream (input (make-memory-input-stream #(0 65 0 66 0 67 0 68)))
    (with-open-file (stream input :external-format 'utf16be)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  #\A #\B #\C #\D :eof :eof)


;;
;;  open output
;;
(deftest open-output-memory.1
  (with-open-stream (output (make-memory-output-stream))
    (with-open-file (stream output :direction :output)
      )
    (get-output-stream-memory output))
  #())

(deftest open-output-memory.2
  (with-open-stream (output (make-memory-output-stream))
    (with-open-file (stream output :direction :output)
      (write-char #\A stream)
      (write-char #\B stream)
      (write-char #\C stream))
    (get-output-stream-memory output))
  #(65 66 67))

(deftest open-output-memory.3
  (with-open-stream (output (make-memory-output-stream))
    (with-open-file (stream output :direction :output :external-format 'utf8bom)
      (write-char #\A stream)
      (write-char #\B stream)
      (write-char #\C stream))
    (get-output-stream-memory output))
  #(#xEF #xBB #xBF 65 66 67))


;;
;;  open io
;;
(deftest open-io-memory.1
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  :eof :eof)

(deftest open-io-memory.2
  (with-open-stream (io (make-memory-io-stream :input #(65 66 67 68)))
    (file-position io :start)
    (with-open-file (stream io)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  #\A #\B #\C #\D :eof :eof)

(deftest open-io-memory.3
  (with-open-stream (io (make-memory-io-stream :input #(0 65 0 66 0 67 0 68)))
    (file-position io :start)
    (with-open-file (stream io :external-format 'utf16be)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  #\A #\B #\C #\D :eof :eof)

(deftest open-io-memory.4
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :io)
      )
    (get-output-stream-memory io))
  #())

(deftest open-io-memory.5
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :io)
      (write-char #\A stream)
      (write-char #\B stream)
      (write-char #\C stream))
    (get-output-stream-memory io))
  #(65 66 67))

(deftest open-io-memory.6
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :io :external-format 'utf8bom)
      (write-char #\A stream)
      (write-char #\B stream)
      (write-char #\C stream))
    (get-output-stream-memory io))
  #(#xEF #xBB #xBF 65 66 67))

(deftest open-io-memory.7
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (stream io :direction :io :external-format 'utf8)
      (write-char #\A stream)
      (write-char #\B stream)
      (write-char #\C stream)
      (file-position stream :start)
      (values
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof)
        (read-char stream nil :eof))))
  #\A #\B #\C :eof)


;;
;;  listen
;;
(deftest memory-listen.1
  (with-open-stream (x (make-memory-input-stream #(1 2)))
    (listen x))
  t)

(deftest memory-listen.2
  (with-open-stream (x (make-memory-input-stream #(1 2)))
    (read-byte x nil nil)
    (read-byte x nil nil)
    (read-byte x nil nil)
    (listen x))
  nil)

