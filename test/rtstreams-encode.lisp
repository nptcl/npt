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

(deftest encode-ascii.1
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-ascii.2
  (progn
    (output-encode-file #(#x00 #x7E #x7F))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (read-char-times file 4)))
  #\u00 #\u7E #\u7F nil)

(deftest-error encode-ascii.3
  (progn
    (output-encode-file #(#x00 #x80 #x7F))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (read-char-times file 4))))

(deftest encode-utf8.1
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf8.2
  (progn
    (output-encode-file #(#xEF #xBB #xBF #x41 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 5)))
  #\A #\B #\u00 #\C nil)

(deftest-error encode-utf8.3
  (progn
    (output-encode-file #(#xEF #xBF #x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 4))))

(deftest encode-utf8.4
  (progn
    (output-encode-file #(#xEF #xBB #xBF #x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf8.5
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8-bom)
      (read-char-times file 4))))

(deftest encode-utf8.6
  (progn
    (output-encode-file
      #(#x41 #xE3 #x81 #x82 #xE3 #x81 #x84 #xE3 #x81 #x86 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-8)
      (read-char-times file 7)))
  #\A #\あ #\い #\う #\B #\C nil)

(deftest encode-utf16.1
  (progn
    (output-encode-file #(#x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.2
  (progn
    (output-encode-file #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16.3
  (progn
    (output-encode-file #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16be.1
  (progn
    (output-encode-file #(#x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf16be.2
  (progn
    (output-encode-file #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be)
      (read-char-times file 4))))

(deftest-error encode-utf16be.3
  (progn
    (output-encode-file #(#x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be-bom)
      (read-char-times file 4))))

(deftest encode-utf16be.4
  (progn
    (output-encode-file #(#xFE #xFF #x00 #x41 #x00 #x42 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-16be-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf16le.1
  (progn
    (output-encode-file #(#x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf16le.2
  (progn
    (output-encode-file #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le)
      (read-char-times file 4))))

(deftest-error encode-utf16le.3
  (progn
    (output-encode-file #(#x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le-bom)
      (read-char-times file 4))))

(deftest encode-utf16le.4
  (progn
    (output-encode-file #(#xFF #xFE #x41 #x00 #x42 #x00 #x43 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-16le-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.1
  (progn
    (output-encode-file
      #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.2
  (progn
    (output-encode-file
      #(#x00 #x00 #xFE #xFF
        #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32.3
  (progn
    (output-encode-file
      #(#xFF #xFE #x00 #x00
        #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32be.1
  (progn
    (output-encode-file
      #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf32be.2
  (progn
    (output-encode-file
      #(#x00 #x00 #xFE #xFF
        #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be)
      (read-char-times file 4))))

(deftest-error encode-utf32be.3
  (progn
    (output-encode-file
      #(#x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be-bom)
      (read-char-times file 4))))

(deftest encode-utf32be.4
  (progn
    (output-encode-file
      #(#x00 #x00 #xFE #xFF
        #x00 #x00 #x00 #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43))
    (with-open-file (file *encode-file* :external-format 'utf-32be-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest encode-utf32le.1
  (progn
    (output-encode-file
      #(#x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le)
      (read-char-times file 4)))
  #\A #\B #\C nil)

(deftest-error encode-utf32le.2
  (progn
    (output-encode-file
      #(#xFF #xFE #x00 #x00
        #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le)
      (read-char-times file 4))))

(deftest-error encode-utf32le.3
  (progn
    (output-encode-file
      #(#x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le-bom)
      (read-char-times file 4))))

(deftest encode-utf32le.4
  (progn
    (output-encode-file
      #(#xFF #xFE #x00 #x00
        #x41 #x00 #x00 #x00 #x42 #x00 #x00 #x00 #x43 #x00 #x00 #x00))
    (with-open-file (file *encode-file* :external-format 'utf-32le-bom)
      (read-char-times file 4)))
  #\A #\B #\C nil)

