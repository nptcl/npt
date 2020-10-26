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

