;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  Function OPEN
;;
(defun close-values (x)
  (multiple-value-prog1
    (values
      (streamp x)
      (typep x 'file-stream)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (open-stream-p x)
      (stream-element-type x))
    (close x)))

(deftest open.1
  (with-temp-file
    (delete-temp-file)
    (let ((x (open *file* :direction :output)))
      (close-values x)))
  t t nil t nil t character)

(deftest open.2
  (with-open-stream (io (make-memory-io-stream))
    (let ((x (open io :direction :output)))
      (close-values x)))
  t t nil t nil t character)

(deftest open.3
  (with-temp-file
    (unless (probe-file-boolean *file*)
      (with-open-file (stream *file* :direction :output :if-does-not-exist :create)
        (write-char #\A stream)))
    (let ((x (open *file*)))
      (close-values x)))
  t t t nil nil t character)

(deftest open.4
  (with-open-stream (io (make-memory-io-stream :input #(65)))
    (let ((x (open io)))
      (close-values x)))
  t t t nil nil t character)

(deftest open.5
  (with-temp-file
    (let ((x (open *file* :direction :io
                   :if-exists :supersede
                   :if-does-not-exist :create)))
      (close-values x)))
  t t t t nil t character)

(deftest open.6
  (with-open-stream (io (make-memory-io-stream))
    (let ((x (open io :direction :io
                   :if-exists :supersede
                   :if-does-not-exist :create)))
      (close-values x)))
  t t t t nil t character)

(deftest open.7
  (with-temp-file
    (close-values
      (open *file* :direction :probe :if-does-not-exist :create)))
  t t nil nil nil nil character)

(deftest open.8
  (with-open-stream (io (make-memory-io-stream))
    (close-values
      (open io :direction :probe :if-does-not-exist :create)))
  t t nil nil nil nil character)

(deftest open.9
  (with-delete-temp-file
    (let ((inst (open *file* :direction :output)))
      (prog1
        (streamp inst)
        (close inst))))
  t)

(deftest open.10
  (with-delete-temp-file
    (let ((inst (open *file* :direction :output)))
      (format inst "Hello")
      (close inst))
    (read-line-1))
  "Hello")

(deftest open.11
  (with-delete-temp-file
    (let ((inst (open *file* :direction :output)))
      (format inst "Hello")
      (close inst))
    (let ((inst (open *file* :direction :output :if-exists :supersede)))
      (format inst "abc")
      (close inst))
    (read-line-1))
  "abc")

(deftest-error open-error.1
  (eval '(open 10))
  type-error)

(deftest-error open-error.2
  (eval '(open *file* :direction)))

(deftest-error open-error.3
  (eval '(open *file* :direction 20)))

(deftest-error open-error.4
  (eval '(open *file* :hello)))

(deftest-error open-error.5
  (eval '(open *file* :hello 30)))

(deftest-error! open-error.6
  (eval '(open)))

(deftest-error open-error.7
  (open #p"hello.*"))


;;
;;  open-input
;;
(deftest open-input.1
  (with-make-file
    (*file* "ABC")
    (with-open-stream (x (open *file*))
      (read-char x)))
  #\A)

(deftest open-input.2
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io))
      (read-char x)))
  #\A)

(deftest open-input.3
  (with-open-stream (io (make-memory-io-stream :input #(65 66 67)))
    (with-open-stream (x (open io))
      (read-char x)))
  #\A)

(deftest open-input.4
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (file-position io :end)
    (with-open-stream (x (open io))
      (read-char x)))
  #\A)

(deftest open-input-if-exists.1
  ;;  ignore if-exists
  (with-make-file
    (*file* "ABC")
    (with-open-stream (x (open *file* :direction :input :if-exists :error))
      (read-char x)))
  #\A)

(deftest open-input-if-exists.2
  ;;  ignore if-exists
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :direction :input :if-exists :error))
      (read-char x)))
  #\A)

(deftest-error open-input-if-does-not-exist.1
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :input :if-does-not-exist :error))
      (read-char x)))
  file-error)

(deftest open-input-if-does-not-exist.2
  (with-open-stream (io (make-memory-input-stream nil))
    (with-open-stream (x (open io :direction :input :if-does-not-exist :error))
      (streamp x)))
  t)

(deftest open-input-if-does-not-exist.3
  (progn
    (delete-temp-file)
    (prog1
      (with-open-stream (x (open *file* :direction :input :if-does-not-exist :create))
        (streamp x))
      (delete-temp-file)))
  t)

(deftest open-input-if-does-not-exist.4
  (with-open-stream (io (make-memory-input-stream nil))
    (with-open-stream (x (open io :direction :input :if-does-not-exist :create))
      (streamp x)))
  t)

(deftest open-input-if-does-not-exist.5
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :input :if-does-not-exist :create))
      (streamp x))
    (prog1
      (probe-file-boolean *file*)
      (delete-temp-file)))
  t)

(deftest open-input-if-does-not-exist.6
  (progn
    (delete-temp-file)
    (open *file* :direction :input :if-does-not-exist nil))
  nil)

(deftest open-input-if-does-not-exist.7
  (with-open-stream (io (make-memory-input-stream nil))
    (with-open-stream (x (open io :direction :input :if-does-not-exist nil))
      (streamp x)))
  t)

(deftest open-input-external-format.1
  (with-temp-file
    (with-open-stream (x (open *file* :direction :input :external-format :default))
      (streamp x)))
  t)

(deftest open-input-external-format.2
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :direction :input :external-format :default))
      (streamp x)))
  t)

(deftest-error open-input-external-format.3
  (with-temp-file
    (with-open-stream (x (open *file* :direction :input :external-format :hello))
      (streamp x))))

(deftest-error open-input-external-format.4
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :direction :input :external-format :hello))
      (streamp x))))


;;
;;  open-element-type
;;

;;  character
(deftest open-input-character.1
  (with-make-file
    (*file* "ABC")
    (with-open-stream (x (open *file* :element-type 'character))
      (values
        (stream-element-type x)
        (read-char x nil :eof)
        (read-char x nil :eof)
        (read-char x nil :eof)
        (read-char x nil :eof))))
  character #\A #\B #\C :eof)

(deftest open-input-character.2
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type 'character))
      (values
        (stream-element-type x)
        (read-char x nil :eof)
        (read-char x nil :eof)
        (read-char x nil :eof)
        (read-char x nil :eof))))
  character #\A #\B #\C :eof)

(deftest-error open-input-character.3
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type 'character))
      (read-byte x))))

(deftest open-input-character.4
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type :default))
      (stream-element-type x)))
  character)

(deftest-error open-input-character.5
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type nil))
      (stream-element-type x))))

(deftest-error open-input-character.6
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type t))
      (stream-element-type x))))

(deftest-error open-input-character.7
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type 'hello))
      (stream-element-type x))))

;;  (unsigned-byte 8)
(deftest open-input-unsigned-byte-8.1
  (with-make-file
    (*file* "ABC")
    (with-open-stream (x (open *file* :element-type 'unsigned-byte))
      (values
        (stream-element-type x)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof))))
  (unsigned-byte 8) 65 66 67 :eof)

(deftest open-input-unsigned-byte-8.2
  (with-open-stream (io (make-memory-input-stream #(#x01 #xBB #xFF)))
    (with-open-stream (x (open io :element-type 'unsigned-byte))
      (values
        (stream-element-type x)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof))))
  (unsigned-byte 8) #x01 #xBB #xFF :eof)

(deftest open-input-unsigned-byte-8.3
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type '(unsigned-byte 8)))
      (stream-element-type x)))
  (unsigned-byte 8))

(deftest open-input-unsigned-byte-8.4
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :element-type '(unsigned-byte 5)))
      (stream-element-type x)))
  (unsigned-byte 8))

;;  (unsigned-byte 16)
(deftest open-input-unsigned-byte-16.1
  (with-temp-file
    (open-unsigned8 #(#x11 #x22 #xEE #xFF))
    (with-open-stream (x (open *file* :element-type '(unsigned-byte 16)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22) a)
          (eql (byte-integer #xEE #xFF) b)
          c))))
  (unsigned-byte 16) t t :eof)

(deftest open-input-unsigned-byte-16.2
  (with-open-stream (io (make-memory-input-stream #(#x11 #x22 #xEE #xFF)))
    (with-open-stream (x (open io :element-type '(unsigned-byte 16)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22) a)
          (eql (byte-integer #xEE #xFF) b)
          c))))
  (unsigned-byte 16) t t :eof)

;;  (unsigned-byte 32)
(deftest open-input-unsigned-byte-32.1
  (with-temp-file
    (open-unsigned8 #(#x11 #x22 #x33 #x44 #xCC #xDD #xEE #xFF))
    (with-open-stream (x (open *file* :element-type '(unsigned-byte 32)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44) a)
          (eql (byte-integer #xCC #xDD #xEE #xFF) b)
          c))))
  (unsigned-byte 32) t t :eof)

(deftest open-input-unsigned-byte-32.2
  (with-open-stream (io (make-memory-input-stream
                          #(#x11 #x22 #x33 #x44 #xCC #xDD #xEE #xFF)))
    (with-open-stream (x (open io :element-type '(unsigned-byte 32)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44) a)
          (eql (byte-integer #xCC #xDD #xEE #xFF) b)
          c))))
  (unsigned-byte 32) t t :eof)

;;  (unsigned-byte 64)
#+64-bit
(deftest open-input-unsigned-byte-64.1
  (with-temp-file
    (open-unsigned8 #(#x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00
                      #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF))
    (with-open-stream (x (open *file* :element-type '(unsigned-byte 64)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00) a)
          (eql (byte-integer #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF) b)
          c))))
  (unsigned-byte 64) t t :eof)

#+64-bit
(deftest open-input-unsigned-byte-64.2
  (with-open-stream (io (make-memory-input-stream
                          #(#x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00
                            #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF)))
    (with-open-stream (x (open io :element-type '(unsigned-byte 64)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00) a)
          (eql (byte-integer #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF) b)
          c))))
  (unsigned-byte 64) t t :eof)

;;  (signed-byte 8)
(deftest open-input-signed-byte-8.1
  (with-make-file
    (*file* "ABC")
    (with-open-stream (x (open *file* :element-type '(signed-byte 8)))
      (values
        (stream-element-type x)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof))))
  (signed-byte 8) 65 66 67 :eof)

(deftest open-input-signed-byte-8.2
  (with-open-stream (io (make-memory-input-stream #(#x01 #xBB #xFF)))
    (with-open-stream (x (open io :element-type '(signed-byte 8)))
      (values
        (stream-element-type x)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof)
        (read-byte x nil :eof))))
  (signed-byte 8) #x01 #x-45 -1 :eof)

(deftest open-input-signed-byte-8.3
  (with-open-stream (io (make-memory-input-stream #(#x01 #xBB #xFF)))
    (with-open-stream (x (open io :element-type '(signed-byte 8)))
      (stream-element-type x)))
  (signed-byte 8))

(deftest open-input-signed-byte-8.4
  (with-open-stream (io (make-memory-input-stream #(#x01 #xBB #xFF)))
    (with-open-stream (x (open io :element-type '(signed-byte 5)))
      (stream-element-type x)))
  (signed-byte 8))

;;  (signed-byte 16)
(deftest open-input-signed-byte-16.1
  (with-temp-file
    (open-unsigned8 #(#x11 #x22 #xEE #xFF))
    (with-open-stream (x (open *file* :element-type '(signed-byte 16)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22) a)
          (eql (- (byte-integer #xEE #xFF) #x010000) b)
          c))))
  (signed-byte 16) t t :eof)

(deftest open-input-signed-byte-16.2
  (with-open-stream (io (make-memory-input-stream #(#x11 #x22 #xEE #xFF)))
    (with-open-stream (x (open io :element-type '(signed-byte 16)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22) a)
          (eql (- (byte-integer #xEE #xFF) #x010000) b)
          c))))
  (signed-byte 16) t t :eof)

;;  (signed-byte 32)
(deftest open-input-signed-byte-32.1
  (with-temp-file
    (open-unsigned8 #(#x11 #x22 #x33 #x44 #xCC #xDD #xEE #xFF))
    (with-open-stream (x (open *file* :element-type '(signed-byte 32)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44) a)
          (eql (- (byte-integer #xCC #xDD #xEE #xFF) #x0100000000) b)
          c))))
  (signed-byte 32) t t :eof)

(deftest open-input-signed-byte-32.2
  (with-open-stream (io (make-memory-input-stream
                          #(#x11 #x22 #x33 #x44 #xCC #xDD #xEE #xFF)))
    (with-open-stream (x (open io :element-type '(signed-byte 32)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44) a)
          (eql (- (byte-integer #xCC #xDD #xEE #xFF) #x0100000000) b)
          c))))
  (signed-byte 32) t t :eof)

;;  (signed-byte 64)
#+64-bit
(deftest open-input-signed-byte-64.1
  (with-temp-file
    (open-unsigned8 #(#x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00
                      #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF))
    (with-open-stream (x (open *file* :element-type '(signed-byte 64)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00) a)
          (eql (- (byte-integer #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF)
                  #x010000000000000000) b)
          c))))
  (signed-byte 64) t t :eof)

#+64-bit
(deftest open-input-signed-byte-64.2
  (with-open-stream (io (make-memory-input-stream
                          #(#x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00
                            #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF)))
    (with-open-stream (x (open io :element-type '(signed-byte 64)))
      (let ((a (read-byte x nil :eof))
            (b (read-byte x nil :eof))
            (c (read-byte x nil :eof)))
        (values
          (stream-element-type x)
          (eql (byte-integer #x11 #x22 #x33 #x44 #x55 #x66 #x77 #x00) a)
          (eql (- (byte-integer #xF1 #xE2 #xD3 #xC4 #xCC #xDD #xEE #xFF)
                  #x010000000000000000) b)
          c))))
  (signed-byte 64) t t :eof)


;;
;;  open-probe
;;
(deftest open-probe.1
  (progn
    (delete-temp-file)
    (open *file* :direction :probe))
  nil)

(deftest open-probe.2
  (with-temp-file
    (with-open-stream (x (open *file* :direction :probe))
      (values
        (streamp x)
        (open-stream-p x))))
  t nil)

(deftest open-probe.3
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :direction :probe))
      (values
        (streamp x)
        (open-stream-p x))))
  t nil)

(deftest open-probe.4
  (with-open-stream (io (make-memory-io-stream :input #(65 66 67)))
    (with-open-stream (x (open io :direction :probe))
      (values
        (streamp x)
        (open-stream-p x))))
  t nil)

(deftest open-probe-if-exists.1
  ;;  ignore if-exists
  (with-temp-file
    (with-open-stream (x (open *file* :direction :probe :if-exists :error))
      (streamp x)))
  t)

(deftest open-probe-if-exists.2
  ;;  ignore if-exists
  (with-open-stream (io (make-memory-input-stream #(65 66 67)))
    (with-open-stream (x (open io :direction :probe :if-exists :error))
      (streamp x)))
  t)

(deftest-error open-probe-if-does-not-exist.1
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :probe :if-does-not-exist :error))
      (streamp x)))
  file-error)

(deftest open-probe-if-does-not-exist.2
  (with-open-stream (io (make-memory-input-stream nil))
    (with-open-stream (x (open io :direction :probe :if-does-not-exist :error))
      (streamp x)))
  t)

(deftest open-probe-if-does-not-exist.3
  (progn
    (delete-temp-file)
    (prog1
      (with-open-stream (x (open *file* :direction :probe :if-does-not-exist :create))
        (streamp x))
      (delete-temp-file)))
  t)

(deftest open-probe-if-does-not-exist.4
  (with-open-stream (io (make-memory-input-stream nil))
    (with-open-stream (x (open io :direction :probe :if-does-not-exist :create))
      (streamp x)))
  t)

(deftest open-probe-if-does-not-exist.5
  (progn
    (delete-temp-file)
    (with-open-stream (x (open *file* :direction :probe :if-does-not-exist :create))
      (streamp x))
    (prog1
      (probe-file-boolean *file*)
      (delete-temp-file)))
  t)

(deftest open-probe-if-does-not-exist.6
  (progn
    (delete-temp-file)
    (open *file* :direction :probe :if-does-not-exist nil))
  nil)

(deftest open-probe-if-does-not-exist.7
  (with-open-stream (io (make-memory-input-stream nil))
    (with-open-stream (x (open io :direction :probe :if-does-not-exist nil))
      (streamp x)))
  t)


;;
;;  ANSI Common Lisp
;;
(deftest open-test.1
  (with-temp-file
    (open-stream-p
      (open *file* :direction :probe)))
  nil)

(deftest open-test.2
  (with-temp-file
    (delete-temp-file)
    (with-open-stream (x (open *file* :if-does-not-exist :create))
      (streamp x))
    (probe-file-boolean *file*))
  t)

(deftest open-test.3
  (with-temp-file
    (equal (pathname-name *file*)
           (pathname-name
             (truename
               (open *file* :direction :probe)))))
  t)

(deftest open-test.4
  (with-temp-file
    (open *file* :direction :output :if-exists nil))
  nil)


;;
;;  Macro WITH-OPEN-FILE
;;
(deftest with-open-file.1
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (file io :direction :input)
      (values
        (open-stream-p file)
        (input-stream-p file)
        (output-stream-p file)
        (streamp file))))
  t t nil t)

(deftest with-open-file.2
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (file io :direction :output)
      (values
        (open-stream-p file)
        (input-stream-p file)
        (output-stream-p file)
        (streamp file))))
  t nil t t)

(deftest with-open-file.3
  (let (stream)
    (with-open-stream (io (make-memory-io-stream))
      (with-open-file (file io :direction :input)
        (setq stream file))
      (values
        (open-stream-p stream)
        (input-stream-p stream)
        (output-stream-p stream)
        (streamp stream))))
  nil t nil t)

(deftest with-open-file.4
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (file io :direction :io)
      (format file "Hello")
      (file-position file :start)
      (values
        (read-line file))))
  "Hello")

(deftest with-open-file.5
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (file io :direction :io)
      ))
  nil)

(deftest-error with-open-file-error.1
  (eval '(with-open-file (10 "file.txt"))))

(deftest-error with-open-file-error.2
  (eval '(with-open-file (var 20))))

(deftest-error with-open-file-error.3
  (eval '(with-open-file (var))))

(deftest-error with-open-file-error.4
  (eval '(with-open-file)))

;;  ANSI Common Lisp
(deftest with-open-file-test.1
  (with-open-stream (p (make-memory-io-stream))
    (with-open-file (s p :direction :output :if-exists :supersede)
      (format s "Here are a couple~%of test data lines~%"))
    (let (list)
      (with-open-file (s p)
        (do ((l (read-line s) (read-line s nil 'eof)))
          ((eq l 'eof)
           (push "Reached end of file." list))
          (push (format nil "*** ~A" l) list)))
      (nreverse list)))
  ("*** Here are a couple"
   "*** of test data lines"
   "Reached end of file."))

(deftest with-open-file-test.2
  (with-open-stream (io (make-memory-io-stream))
    (with-open-file (x io :direction :output)
      (format x "Hello"))
    (with-open-file (*standard-input* io)
      (with-open-file (foo "with-open-file-no-such-file"
                           :if-does-not-exist nil)
        (read foo))))
  hello)

(deftest with-open-file-test.3
  (with-open-file (foo "with-opne-file-no-such-file"
                       :direction :output :if-does-not-exist nil)
    (format foo "Hello"))
  "Hello")

