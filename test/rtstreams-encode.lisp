;;
;;  ANSI COMMON LISP: 21. Streams
;;
(defconstant *encode-file* #p"_debug.txt")

(defun output-encode-file (vector)
  (with-open-file (file *encode-file* :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
    (write-sequence vector file)))

(defun read-char-times (stream times)
  (let (list)
    (dotimes (i times)
      (push (read-char stream nil nil) list))
    (values-list
      (nreverse list))))


;;
;;  ascii
;;
(deftest encode-ascii.1
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-ascii.2
  (with-open-stream (io (make-memory-input-stream #(#x41 #x42 #x43)))
    (with-open-file (file io :external-format 'ascii)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-ascii.3
  (progn
    (output-encode-file #(#x00 #x7E #x7F))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (read-char-times file 4)))
  #\u00 #\u7E #\u7F nil)

(deftest encode-ascii.4
  (with-open-stream (io (make-memory-input-stream #(#x00 #x7E #x7F)))
    (with-open-file (file io :external-format 'ascii)
      (read-char-times file 4)))
  #\u00 #\u7E #\u7F nil)

(deftest-error encode-ascii.5
  (progn
    (output-encode-file #(#x00 #x80 #x7F))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (read-char-times file 4))))

(deftest-error encode-ascii.6
  (with-open-stream (io (make-memory-input-stream #(#x00 #x80 #x7F)))
    (with-open-file (file io :external-format 'ascii)
      (read-char-times file 4))))


;;
;;  utf8
;;
(deftest encode-utf8.1
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf8.2
  (with-open-stream (io (make-memory-input-stream #(#x41 #x42 #x43)))
    (with-open-file (file io :external-format 'utf-8)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf8.3
  (progn
    (output-encode-file #(#xEF #xBB #xBF #x41 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 5)))
  #\A #\B #\u00 #\C nil)

(deftest encode-utf8.4
  (with-open-stream (io (make-memory-input-stream
                          #(#xEF #xBB #xBF #x41 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-8)
      (read-char-times file 5)))
  #\A #\B #\u00 #\C nil)

(deftest-error encode-utf8.5
  (progn
    (output-encode-file #(#xEF #xBF #x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 4))))

(deftest-error encode-utf8.6
  (with-open-stream (io (make-memory-input-stream #(#xEF #xBF #x41 #x42 #x43)))
    (with-open-file (file io :external-format 'utf-8)
      (read-char-times file 4))))

(deftest encode-utf8.7
  (progn
    (output-encode-file #(#xEF #xBB #xBF #x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf8.8
  (with-open-stream (io (make-memory-input-stream
                          #(#xEF #xBB #xBF #x41 #x42 #x43)))
    (with-open-file (file io :external-format 'utf-8-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf8.9
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8-bom)
      (read-char-times file 4))))

(deftest-error encode-utf8.10
  (with-open-stream (io (make-memory-input-stream #(#x41 #x42 #x43)))
    (with-open-file (file io :external-format 'utf-8-bom)
      (read-char-times file 4))))

(deftest encode-utf8.11
  (progn
    (output-encode-file
      #(#x41 #xE3 #x81 #x82 #xE3 #x81 #x84 #xE3 #x81 #x86 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 7)))
  #\A #\u3042 #\u3044 #\u3046 #\B #\C nil)

(deftest encode-utf8.12
  (with-open-stream (io (make-memory-input-stream
                          #(#x41 #xE3 #x81 #x82 #xE3 #x81 #x84 #xE3
                            #x81 #x86 #x42 #x43)))
    (with-open-file (file io :external-format 'utf-8)
      (read-char-times file 7)))
  #\A #\u3042 #\u3044 #\u3046 #\B #\C nil)


;;
;;  utf16
;;
(deftest encode-utf16.1
  (progn
    (output-encode-file #(#x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.2
  (with-open-stream (io (make-memory-input-stream #(#x00 #x41 #x00 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.3
  (progn
    (output-encode-file #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.4
  (with-open-stream (io (make-memory-input-stream
                          #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.5
  (progn
    (output-encode-file #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.6
  (with-open-stream (io (make-memory-input-stream
                          #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00)))
    (with-open-file (file io :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16be.1
  (progn
    (output-encode-file #(#x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16be.2
  (with-open-stream (io (make-memory-input-stream #(#x00 #x41 #x00 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-16be)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf16be.3
  (progn
    (output-encode-file #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be)
      (read-char-times file 4))))

(deftest-error encode-utf16be.4
  (with-open-stream (io (make-memory-input-stream
                          #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-16be)
      (read-char-times file 4))))

(deftest-error encode-utf16be.5
  (progn
    (output-encode-file #(#x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be-bom)
      (read-char-times file 4))))

(deftest-error encode-utf16be.6
  (with-open-stream (io (make-memory-input-stream
                          #(#x00 #x41 #x00 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-16be-bom)
      (read-char-times file 4))))

(deftest encode-utf16be.7
  (progn
    (output-encode-file #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16be.8
  (with-open-stream (io (make-memory-input-stream
                          #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-16be-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16le.1
  (progn
    (output-encode-file #(#x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16le.2
  (with-open-stream (io (make-memory-input-stream #(#x41 #x00 #x42 #x00 #x43 #x00)))
    (with-open-file (file io :external-format 'utf-16le)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf16le.3
  (progn
    (output-encode-file #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le)
      (read-char-times file 4))))

(deftest-error encode-utf16le.4
  (with-open-stream (io (make-memory-input-stream
                          #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00)))
    (with-open-file (file io :external-format 'utf-16le)
      (read-char-times file 4))))

(deftest-error encode-utf16le.5
  (progn
    (output-encode-file #(#x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le-bom)
      (read-char-times file 4))))

(deftest-error encode-utf16le.6
  (with-open-stream (io (make-memory-input-stream #(#x41 #x00 #x42 #x00 #x43 #x00)))
    (with-open-file (file io :external-format 'utf-16le-bom)
      (read-char-times file 4))))

(deftest encode-utf16le.7
  (progn
    (output-encode-file #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16le.8
  (with-open-stream (io (make-memory-input-stream
                          #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00)))
    (with-open-file (file io :external-format 'utf-16le-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)


;;
;;  utf32
;;
(deftest encode-utf32.1
  (progn
    (output-encode-file
      #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.2
  (with-open-stream
    (io (make-memory-input-stream
          #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.3
  (progn
    (output-encode-file
      #(#x00 #x00 #xFE #xFF
        #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.4
  (with-open-stream
    (io (make-memory-input-stream
          #(#x00 #x00 #xFE #xFF
            #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.5
  (progn
    (output-encode-file
      #(#xFF #xFE #x00 #x00
        #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.6
  (with-open-stream
    (io (make-memory-input-stream
          #(#xFF #xFE #x00 #x00
            #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32be.1
  (progn
    (output-encode-file
      #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32be.2
  (with-open-stream
    (io (make-memory-input-stream
          #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-32be)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf32be.3
  (progn
    (output-encode-file
      #(#x00 #x00 #xFE #xFF
        #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be)
      (read-char-times file 4))))

(deftest-error encode-utf32be.4
  (with-open-stream
    (io (make-memory-input-stream
          #(#x00 #x00 #xFE #xFF
            #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-32be)
      (read-char-times file 4))))

(deftest-error encode-utf32be.5
  (progn
    (output-encode-file
      #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be-bom)
      (read-char-times file 4))))

(deftest-error encode-utf32be.6
  (with-open-stream
    (io (make-memory-input-stream
          #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-32be-bom)
      (read-char-times file 4))))

(deftest encode-utf32be.7
  (progn
    (output-encode-file
      #(#x00 #x00 #xFE #xFF
        #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32be.8
  (with-open-stream
    (io (make-memory-input-stream
          #(#x00 #x00 #xFE #xFF
            #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43)))
    (with-open-file (file io :external-format 'utf-32be-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32le.1
  (progn
    (output-encode-file
      #(#x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32le.2
  (with-open-stream
    (io (make-memory-input-stream
          #(#x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32le)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf32le.3
  (progn
    (output-encode-file
      #(#xFF #xFE #x00 #x00
        #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le)
      (read-char-times file 4))))

(deftest-error encode-utf32le.4
  (with-open-stream
    (io (make-memory-input-stream
          #(#xFF #xFE #x00 #x00
            #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32le)
      (read-char-times file 4))))

(deftest-error encode-utf32le.5
  (progn
    (output-encode-file
      #(#x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le-bom)
      (read-char-times file 4))))

(deftest-error encode-utf32le.6
  (with-open-stream
    (io (make-memory-input-stream
          #(#x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32le-bom)
      (read-char-times file 4))))

(deftest encode-utf32le.7
  (progn
    (output-encode-file
      #(#xFF #xFE #x00 #x00
        #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32le.8
  (with-open-stream
    (io (make-memory-input-stream
          #(#xFF #xFE #x00 #x00
            #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32le-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)


;;
;;  external-format
;;
(deftest encode-external-format-ascii.1
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'ascii)
      (stream-external-format file)))
  lisp-system::ascii)

;;  utf8
(deftest encode-external-format-utf8.1
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf-8)
      (stream-external-format file)))
  lisp-system::utf-8)

(deftest encode-external-format-utf8.2
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format :default)
      (stream-external-format file)))
  lisp-system::utf-8)

(deftest encode-external-format-utf8.3
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format :utf8)
      (stream-external-format file)))
  lisp-system::utf-8)

(deftest encode-external-format-utf8.4
  (with-open-stream (io (make-memory-input-stream #(#xEF #xBB #xBF)))
    (with-open-file (file io :external-format 'utf-8)
      (stream-external-format file)))
  lisp-system::utf-8)

(deftest encode-external-format-utf8.5
  (with-open-stream (io (make-memory-input-stream #(#xEF #xBB #xBF)))
    (with-open-file (file io :external-format 'utf8bom)
      (stream-external-format file)))
  lisp-system::utf-8-bom)

(deftest encode-external-format-utf8.6
  (with-open-stream (io (make-memory-input-stream #(#xEF #xBB #xBF)))
    (with-open-file (file io :external-format 'utf-8-bom)
      (stream-external-format file)))
  lisp-system::utf-8-bom)


;;  utf16
(deftest encode-external-format-utf16.1
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf16)
      (stream-external-format file)))
  lisp-system::utf-16be)

(deftest encode-external-format-utf16.2
  (with-open-stream (io (make-memory-input-stream #(#xFE #xFF)))
    (with-open-file (file io :external-format 'utf-16)
      (stream-external-format file)))
  lisp-system::utf-16be)

(deftest encode-external-format-utf16.3
  (with-open-stream (io (make-memory-input-stream #(#xFF #xFE)))
    (with-open-file (file io :external-format 'utf-16)
      (stream-external-format file)))
  lisp-system::utf-16le)

(deftest encode-external-format-utf16.4
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf16be)
      (stream-external-format file)))
  lisp-system::utf-16be)

(deftest encode-external-format-utf16.5
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf-16be)
      (stream-external-format file)))
  lisp-system::utf-16be)

(deftest encode-external-format-utf16.6
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf16le)
      (stream-external-format file)))
  lisp-system::utf-16le)

(deftest encode-external-format-utf16.7
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf-16le)
      (stream-external-format file)))
  lisp-system::utf-16le)

(deftest encode-external-format-utf16.8
  (with-open-stream (io (make-memory-input-stream #(#xFE #xFF)))
    (with-open-file (file io :external-format 'utf16bebom)
      (stream-external-format file)))
  lisp-system::utf-16be-bom)

(deftest encode-external-format-utf16.9
  (with-open-stream (io (make-memory-input-stream #(#xFE #xFF)))
    (with-open-file (file io :external-format 'utf-16be-bom)
      (stream-external-format file)))
  lisp-system::utf-16be-bom)

(deftest encode-external-format-utf16.10
  (with-open-stream (io (make-memory-input-stream #(#xFF #xFE)))
    (with-open-file (file io :external-format 'utf16lebom)
      (stream-external-format file)))
  lisp-system::utf-16le-bom)

(deftest encode-external-format-utf16.11
  (with-open-stream (io (make-memory-input-stream #(#xFF #xFE)))
    (with-open-file (file io :external-format 'utf-16le-bom)
      (stream-external-format file)))
  lisp-system::utf-16le-bom)


;;  utf32
(deftest encode-external-format-utf32.1
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf32)
      (stream-external-format file)))
  lisp-system::utf-32be)

(deftest encode-external-format-utf32.2
  (with-open-stream (io (make-memory-input-stream #(#x00 #x00 #xFE #xFF)))
    (with-open-file (file io :external-format 'utf-32)
      (stream-external-format file)))
  lisp-system::utf-32be)

(deftest encode-external-format-utf32.3
  (with-open-stream (io (make-memory-input-stream #(#xFF #xFE #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32)
      (stream-external-format file)))
  lisp-system::utf-32le)

(deftest encode-external-format-utf32.4
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf32be)
      (stream-external-format file)))
  lisp-system::utf-32be)

(deftest encode-external-format-utf32.5
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf-32be)
      (stream-external-format file)))
  lisp-system::utf-32be)

(deftest encode-external-format-utf32.6
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf32le)
      (stream-external-format file)))
  lisp-system::utf-32le)

(deftest encode-external-format-utf32.7
  (with-open-stream (io (make-memory-input-stream #()))
    (with-open-file (file io :external-format 'utf-32le)
      (stream-external-format file)))
  lisp-system::utf-32le)

(deftest encode-external-format-utf32.8
  (with-open-stream (io (make-memory-input-stream #(#x00 #x00 #xFE #xFF)))
    (with-open-file (file io :external-format 'utf32bebom)
      (stream-external-format file)))
  lisp-system::utf-32be-bom)

(deftest encode-external-format-utf32.9
  (with-open-stream (io (make-memory-input-stream #(#x00 #x00 #xFE #xFF)))
    (with-open-file (file io :external-format 'utf-32be-bom)
      (stream-external-format file)))
  lisp-system::utf-32be-bom)

(deftest encode-external-format-utf32.10
  (with-open-stream (io (make-memory-input-stream #(#xFF #xFE #x00 #x00)))
    (with-open-file (file io :external-format 'utf32lebom)
      (stream-external-format file)))
  lisp-system::utf-32le-bom)

(deftest encode-external-format-utf32.11
  (with-open-stream (io (make-memory-input-stream #(#xFF #xFE #x00 #x00)))
    (with-open-file (file io :external-format 'utf-32le-bom)
      (stream-external-format file)))
  lisp-system::utf-32le-bom)

