;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  Function READ-BYTE
;;
(deftest read-byte.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)))
  65)

(deftest-error read-byte.2
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)
      (read-byte stream)))
  end-of-file)

(deftest read-byte.3
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream nil)
      (read-byte stream nil)
      (read-byte stream nil)
      (read-byte stream nil)
      (read-byte stream nil)))
  nil)

(deftest read-byte.4
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (read-byte stream nil :hello)
      (read-byte stream nil :hello)
      (read-byte stream nil :hello)
      (read-byte stream nil :hello)
      (read-byte stream nil :hello)))
  :hello)

(deftest-error read-byte-error.1
  (with-temp-file
    (with-open-file (stream *file*)
      (read-byte stream))))

(deftest-error read-byte-error.2
  (eval '(read-byte 10))
  type-error)

(deftest-error! read-byte-error.3
  (eval '(read-byte)))

(deftest-error! read-byte-error.4
  (eval '(read-byte *standard-input* nil :hello nil)))

;;  ANSI Common Lisp
(deftest read-byte-test.1
  (with-open-file (s *file* :direction :output :element-type 'unsigned-byte)
    (write-byte 101 s))
  101)

(deftest read-byte-test.2
  (with-open-file (s *file* :element-type 'unsigned-byte)
    (format nil "~S ~S" (read-byte s) (read-byte s nil 'eof)))
  "101 EOF")


;;
;;  Function WRITE-BYTE
;;
(deftest write-byte.1
  (with-open-file (stream *file* :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-byte #xAB stream)
    (write-byte #xCD stream))
  #xCD)

(deftest write-byte.2
  (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
    (values
      (read-byte stream nil :eof)
      (read-byte stream nil :eof)
      (read-byte stream nil :eof)))
  #xAB #xCD :eof)

(deftest-error write-byte-error.1
  (eval '(write-byte 10 20))
  type-error)

(deftest-error write-byte-error.2
  (eval '(write-byte *standard-output* :hello))
  type-error)

(deftest-error! write-byte-error.3
  (eval '(write-byte *standard-output*)))

(deftest-error! write-byte-error.4
  (eval '(write-byte *standard-output* 20 30)))

(deftest-error write-byte-error.5
  (with-temp-file
    (with-open-file (stream *file* :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-byte stream #xFF)))
  type-error)

(deftest-error write-byte-error.6
  (with-temp-file
    (with-open-file (stream *file* :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type 'unsigned-byte)
      (write-byte stream #xFFFF))))

;;  ANSI Common Lisp
(deftest write-byte-test.1
  (with-open-file (s *file* :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :element-type 'unsigned-byte)
    (write-byte 101 s))
  101)


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

#+fixnum-64
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

#+fixnum-64
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

#+fixnum-64
(deftest-error read-byte-unsigned-64.3
  (with-temp-write-element-type
    (stream '(unsigned-byte 64))
    (write-byte #x010000000000000000 stream)))

#+fixnum-64
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

#+fixnum-64
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

#+fixnum-64
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

#+fixnum-64
(deftest-error read-byte-signed-64.3
  (with-temp-write-element-type
    (stream '(signed-byte 64))
    (write-byte #x8000000000000000 stream)))

#+fixnum-64
(deftest-error read-byte-signed-64.4
  (with-temp-write-element-type
    (stream '(signed-byte 64))
    (write-byte #x-8000000000000001 stream)))

