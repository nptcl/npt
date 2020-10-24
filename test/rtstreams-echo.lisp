;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest echo-input-stream-p.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (input-stream-p inst))
  t)


;;
;;  output-stream-p
;;
(deftest echo-output-stream-p.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest echo-interactive-stream-p.1
  (with-open-stream (inst (make-echo-stream *standard-input* *standard-output*))
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest open-stream-p-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (open-stream-p stream))
  t)

(deftest open-stream-p-echo.2
  (let ((stream (make-echo-stream *standard-input* *standard-output*)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest open-stream-p-echo.3
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-echo-stream x y)))
    (close stream)
    (values
      (open-stream-p x)
      (open-stream-p y)))
  t t)

(deftest open-stream-p-echo.4
  (let* ((x (make-string-input-stream "Hello"))
         (y (make-string-output-stream))
         (stream (make-echo-stream x y)))
    (close x)
    (close y)
    (open-stream-p stream))
  t)


;;
;;  stream-element-type
;;
(deftest echo-stream-element-type.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (stream-element-type stream))
  character)

(deftest echo-stream-element-type.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-echo-stream input *standard-output*))
        (stream-element-type stream))))
  (or (unsigned-byte 8) character))

(deftest echo-stream-element-type.3
  (with-temp-file
    (with-open-file (output *file* :direction :output
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-echo-stream *standard-input* output))
        (stream-element-type stream))))
  (or character (unsigned-byte 8)))

