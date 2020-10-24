;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest concatenated-input-stream-p.1
  (with-open-stream (inst (make-concatenated-stream))
    (input-stream-p inst))
  t)


;;
;;  output-stream-p
;;
(deftest concatenated-output-stream-p.1
  (with-open-stream (inst (make-concatenated-stream))
    (output-stream-p inst))
  nil)


;;
;;  interactive-stream-p
;;
(deftest concatenated-interactive-stream-p.1
  (with-open-stream (inst (make-concatenated-stream))
    (interactive-stream-p inst))
  nil)

(deftest concatenated-interactive-stream-p.2
  (with-open-stream (inst (make-concatenated-stream *standard-input*))
    (interactive-stream-p inst))
  t)

(deftest concatenated-interactive-stream-p.3
  (with-temp-file
    (with-open-file
      (stream *file* :direction :input)
      (with-open-stream (inst (make-concatenated-stream stream *standard-input*))
        (interactive-stream-p inst))))
  nil)


;;
;;  open-stream-p
;;
(deftest concatenated-open-stream-p.1
  (with-open-stream (stream (make-concatenated-stream))
    (open-stream-p stream))
  t)

(deftest concatenated-open-stream-p.2
  (let ((stream (make-concatenated-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest concatenated-open-stream-p.3
  (let* ((str (make-string-input-stream "Hello"))
         (stream (make-concatenated-stream str)))
    (close str)
    (open-stream-p stream))
  t)

(deftest concatenated-open-stream-p.4
  (let* ((str (make-string-input-stream "Hello"))
         (stream (make-concatenated-stream str)))
    (close stream)
    (open-stream-p str))
  t)


;;
;;  stream-element-type
;;
(deftest concatenated-stream-element-type.1
  (with-open-stream (stream (make-concatenated-stream))
    (stream-element-type stream))
  nil)

(deftest concatenated-stream-element-type.2
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream *standard-input* input))
        (stream-element-type stream))))
  character)

(deftest concatenated-stream-element-type.3
  (with-temp-file
    (with-open-file (input *file* :direction :input :element-type 'unsigned-byte)
      (with-open-stream (stream (make-concatenated-stream input *standard-input*))
        (stream-element-type stream))))
  (unsigned-byte 8))

