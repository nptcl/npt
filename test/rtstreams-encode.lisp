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

(deftest encode-ascii.1
  (progn
    (output-encode-file #(#x41 #x42 #x43))
    (with-open-file (file *encode-file* :external-format 'ascii)
      (values
        (read-char file nil nil)
        (read-char file nil nil)
        (read-char file nil nil)
        (read-char file nil nil))))
  #\A #\B #\C)

