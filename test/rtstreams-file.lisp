;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest file-input-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (input-stream-p stream)))
  nil)

(deftest file-input-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.4
  (with-temp-file
    (with-open-file (stream *file* :direction :input
                            :element-type '(unsigned-byte 8))
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.5
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (input-stream-p stream)))
  nil)

(deftest file-input-stream-p.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (input-stream-p stream)))
  t)

(deftest file-input-stream-p.7
  (values
    (input-stream-p lisp-system::*standard-input*)
    (input-stream-p lisp-system::*standard-output*)
    (input-stream-p lisp-system::*standard-error*))
  t nil nil)


;;
;;  output-stream-p
;;
(deftest file-output-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (output-stream-p stream)))
  nil)

(deftest file-output-stream-p.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.4
  (with-temp-file
    (with-open-file (stream *file* :direction :input
                            :element-type '(unsigned-byte 8))
      (output-stream-p stream)))
  nil)

(deftest file-output-stream-p.5
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (output-stream-p stream)))
  t)

(deftest file-output-stream-p.7
  (values
    (output-stream-p lisp-system::*standard-input*)
    (output-stream-p lisp-system::*standard-output*)
    (output-stream-p lisp-system::*standard-error*))
  nil t t)


;;
;;  interactive-stream-p
;;
(deftest file-interactive-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (interactive-stream-p stream)))
  nil)

(deftest file-interactive-stream-p.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (interactive-stream-p stream)))
  nil)

(deftest file-interactive-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (interactive-stream-p stream)))
  nil)

(deftest file-interactive-stream-p.7
  (values
    (interactive-stream-p lisp-system::*standard-input*)
    (interactive-stream-p lisp-system::*standard-output*)
    (interactive-stream-p lisp-system::*standard-error*))
  t t t)


;;
;;  open-stream-p
;;
(deftest file-open-stream-p.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (open-stream-p stream)))
  t)

(deftest file-open-stream-p.2
  (with-temp-file
    (let ((stream (open *file* :direction :input)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest file-open-stream-p.3
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (open-stream-p stream)))
  t)

(deftest file-open-stream-p.4
  (with-temp-file
    (let ((stream (open *file* :direction :output :if-exists :supersede)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest file-open-stream-p.5
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (open-stream-p stream)))
  t)

(deftest file-open-stream-p.6
  (with-temp-file
    (let ((stream (open *file* :direction :io :if-exists :supersede)))
      (close stream)
      (open-stream-p stream)))
  nil)

(deftest file-open-stream-p.7
  (open-stream-p
    (open #p"test/empty.file" :direction :probe))
  nil)


;;
;;  stream-element-type
;;
(deftest file-stream-element-type.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (stream-element-type stream)))
  character)

(deftest file-stream-element-type.2
  (with-temp-file
    (with-open-file (stream *file* :direction :input :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest file-stream-element-type.3
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (stream-element-type stream)))
  character)

(deftest file-stream-element-type.4
  (with-temp-file
    (with-open-file (stream *file* :direction :output
                            :if-exists :supersede :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

(deftest file-stream-element-type.5
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (stream-element-type stream)))
  character)

(deftest file-stream-element-type.6
  (with-temp-file
    (with-open-file (stream *file* :direction :io
                            :if-exists :supersede :element-type 'unsigned-byte)
      (stream-element-type stream)))
  (unsigned-byte 8))

