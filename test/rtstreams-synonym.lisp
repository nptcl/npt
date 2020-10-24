;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest synonym-input-stream-p.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (input-stream-p inst))))
  t)

(deftest synonym-input-stream-p.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (input-stream-p inst))))
  nil)


;;
;;  output-stream-p
;;
(deftest synonym-output-stream-p.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (output-stream-p inst))))
  nil)

(deftest synonym-output-stream-p.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (output-stream-p inst))))
  t)


;;
;;  interactive-stream-p
;;
(deftest synonym-interactive-stream-p.1
  (with-temp-file
    (with-open-file (hello *file* :direction :input)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (interactive-stream-p inst))))
  nil)

(deftest synonym-interactive-stream-p.2
  (with-temp-file
    (with-open-file (hello *file* :direction :output :if-exists :supersede)
      (declare (special hello))
      (with-open-stream (inst (make-synonym-stream 'hello))
        (interactive-stream-p inst))))
  nil)

(deftest synonym-interactive-stream-p.3
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (interactive-stream-p stream))
  t)


;;
;;  open-stream-p
(deftest synonym-open-stream-p.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)))
  t t)

(deftest synonym-open-stream-p.2
  (let ((stream (make-synonym-stream '*standard-input*)))
    (close stream)
    (values
      (open-stream-p stream)
      (open-stream-p *standard-input*)))
  nil t)

(deftest synonym-open-stream-p.3
  (let ((*x* (make-string-input-stream "Hello")))
    (declare (special *x*))
    (let ((stream (make-synonym-stream '*x*)))
      (close *x*)
      (open-stream-p stream)))
  t)

(deftest synonym-open-stream-p.4
  (let ((*x* (make-string-input-stream "Hello")))
    (declare (special *x*))
    (let ((stream (make-synonym-stream '*x*)))
      (close stream)
      (open-stream-p *x*)))
  t)


;;
;;  stream-element-type
;;
(deftest synonym-stream-element-type.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (stream-element-type stream))
  character)

(deftest synonym-stream-element-type.2
  (with-temp-file
    (with-open-file (hello *file* :direction :input :element-type 'unsigned-byte)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (stream-element-type stream))))
  (unsigned-byte 8))

