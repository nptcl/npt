;;
;;  ANSI COMMON LISP: 21. Streams
;;
(deftest broadcast-stream.1
  (let ((x (make-broadcast-stream)))
    (values
      (streamp x)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (open-stream-p x)))
  t nil t nil t)


;;
;;  input-stream-p
;;
(deftest broadcast-input-stream-p.1
  (with-open-stream (inst (make-broadcast-stream))
    (input-stream-p inst))
  nil)


;;
;;  output-stream-p
;;
(deftest broadcast-output-stream-p.1
  (with-open-stream (inst (make-broadcast-stream))
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest broadcast-interactive-stream-p.1
  (with-open-stream (inst (make-broadcast-stream))
    (interactive-stream-p inst))
  nil)

(deftest broadcast-interactive-stream-p.2
  (with-open-stream (inst (make-broadcast-stream *query-io*))
    (interactive-stream-p inst))
  nil)

(deftest broadcast-interactive-stream-p.3
  (with-open-stream (inst (make-broadcast-stream *standard-output*))
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest broadcast-open-stream-p.1
  (with-open-stream (stream (make-broadcast-stream))
    (open-stream-p stream))
  t)

(deftest broadcast-open-stream-p.2
  (let ((stream (make-broadcast-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest broadcast-open-stream-p.3
  (let* ((str (make-string-output-stream))
         (stream (make-broadcast-stream str)))
    (close str)
    (open-stream-p stream))
  t)

(deftest broadcast-open-stream-p.4
  (let* ((str (make-string-output-stream))
         (stream (make-broadcast-stream str)))
    (close stream)
    (open-stream-p str))
  t)


;;
;;  stream-element-type
;;
(deftest broadcast-stream-element-type.1
  (with-open-stream (stream (make-broadcast-stream))
    (stream-element-type stream))
  t)

(deftest broadcast-stream-element-type.2
  (with-temp-file
    (with-open-file (output *file* :direction :io
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream *standard-output* output))
        (stream-element-type stream))))
  (unsigned-byte 8))

(deftest broadcast-stream-element-type.3
  (with-temp-file
    (with-open-file (output *file* :direction :io
                            :if-exists :supersede
                            :element-type 'unsigned-byte)
      (with-open-stream (stream (make-broadcast-stream output *standard-output*))
        (stream-element-type stream))))
  character)

