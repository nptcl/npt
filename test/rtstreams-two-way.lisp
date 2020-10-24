;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest two-way-input-stream-p.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (input-stream-p inst))
  t)


;;
;;  output-stream-p
;;
(deftest two-way-output-stream-p.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest two-way-interactive-stream-p.1
  (with-open-stream (inst (make-two-way-stream *standard-input* *standard-output*))
    (interactive-stream-p inst))
  t)

(deftest two-way-interactive-stream-p.2
  (with-open-stream (inst (make-two-way-stream
                            *standard-input*
                            (make-string-output-stream)))
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest two-way-open-stream-p.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (open-stream-p stream))
  t)

(deftest two-way-open-stream-p.2
  (let ((stream (make-two-way-stream *standard-input* *standard-output*)))
    (close stream)
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)
      (open-stream-p *standard-output*)))
  nil t t)

(deftest two-way-open-stream-p.3
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-two-way-stream x y)))
    (close stream)
    (values
      (open-stream-p x)
      (open-stream-p y)))
  t t)

(deftest two-way-open-stream-p.4
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-two-way-stream x y)))
    (close x)
    (close y)
    (open-stream-p stream))
  t)


;;
;;  stream-element-type
;;
(deftest two-way-stream-element-type.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (stream-element-type stream))
  character)

(deftest two-way-stream-element-type.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-two-way-stream input *standard-output*))
        (stream-element-type stream))))
  (or (unsigned-byte 8) character))

(deftest two-way-stream-element-type.3
  (with-temp-file
    (with-open-file (output *file* :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-two-way-stream *standard-input* output))
        (stream-element-type stream))))
  (or character (unsigned-byte 8)))

