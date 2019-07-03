;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;  eof
;;  open
;;  with-open-file
;;  encode


;;
;;  listen
;;
(deftest listen.1
  (listen *standard-input*)
  nil)

(deftest listen.2
  (prog2
    (unread-char #\a *standard-input*)
    (listen *standard-input*)
    (read-char *standard-input*))
  t)

(deftest-error listen.3
  (listen *standard-output*))

(deftest listen-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (listen input)))
  t)

(deftest-error listen-file.2
  (with-temp-file
    (with-overwrite-file (output *file*)
      (listen output))))

(deftest listen-file.3
  (with-temp-file
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (listen input)))
  t)

(deftest listen-file.4
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (listen input)))
  t)

(deftest-error listen-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (listen stream)))

(deftest listen-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (listen stream))
  nil)

(deftest listen-concatenated.2
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (listen stream)))
  t)

(deftest listen-concatenated.3
  (with-input-from-string (input1 "Hello")
    (with-open-stream (input2 (make-concatenated-stream))
      (with-open-stream (stream (make-concatenated-stream input1 input2))
        (listen stream))))
  t)

(deftest listen-concatenated.4
  (with-input-from-string (input1 "Hello")
    (with-open-stream (input2 (make-concatenated-stream))
      (with-open-stream (stream (make-concatenated-stream input2 input1))
        (listen stream))))
  nil)

(deftest listen-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (listen stream))))
  t)

(deftest listen-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (with-open-stream (stream (make-synonym-stream 'input))
      (listen stream)))
  t)

(deftest listen-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (listen stream))))
  t)

(deftest listen-input-string.1
  (with-input-from-string (stream "Hello")
    (listen stream))
  t)

(deftest-error listen-output-string.1
  (with-output-to-string (stream)
    (listen stream)))

(deftest-error listen-extend-string.1
  (with-extend-to-string
    (stream array)
    (listen stream)))


;;
;;  clear-input
;;
(deftest clear-input.1
  (clear-input *standard-input*)
  nil)

(deftest clear-input-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (clear-input input)))
  nil)

(deftest-error clear-input-file.2
  (with-temp-file
    (with-open-file (input *file* :direction :output :if-exists :overwrite)
      (clear-input input))))

(deftest clear-input-file.3
  (with-temp-file
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (clear-input input)))
  nil)

(deftest clear-input-file.4
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (clear-input input)))
  nil)

(deftest-error clear-input-file.5
  (with-temp-file
    (with-open-file (input *file* :direction :output
                           :if-exists :overwrite :element-type 'unsigned-byte)
      (clear-input input))))

(deftest clear-input-file.6
  (with-temp-file
    (with-open-file (input *file* :direction :io
                           :if-exists :overwrite :element-type 'unsigned-byte)
      (clear-input input)))
  nil)

(deftest-error clear-input-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (clear-input stream)))

(deftest clear-input-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (clear-input stream))
  nil)

(deftest clear-input-concatenated.2
  (with-open-stream (stream (make-concatenated-stream *standard-input*))
    (clear-input stream))
  nil)

(deftest clear-input-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (clear-input stream))
  nil)

(deftest clear-input-synonym.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (clear-input stream))
  nil)

(deftest clear-input-two-way.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (clear-input stream))
  nil)

(deftest clear-input-input-string.1
  (with-input-from-string (input "Hello")
    (read-char input)
    (unread-char #\H input)
    (values
      (clear-input input)
      (read-char input)))
  nil #\H)

(deftest-error clear-input-output-string.1
  (with-output-to-string (stream)
    (clear-input stream)))

(deftest-error clear-input-extend-string.1
  (with-extend-to-string
    (stream array)
    (clear-input stream)))


;;
;;  finish-output, force-output, clear-output
;;
(deftest finish-output-file.1
  (with-temp-file
    (with-overwrite-file (output *file*)
      (values
        (progn
          (write-char #\a output)
          (finish-output output))
        (progn
          (write-char #\b output)
          (force-output output))
        (progn
          (write-char #\c output)
          (clear-output output)))))
  nil nil nil)

(deftest finish-output-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (values
      (finish-output stream)
      (force-output stream)
      (clear-output stream)))
  nil nil nil)

(deftest finish-output-broadcast.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file*)
      (with-overwrite-file (output2 *file*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (values
            (progn
              (write-char #\a stream)
              (finish-output stream))
            (progn
              (write-char #\b stream)
              (force-output stream))
            (progn
              (write-char #\c stream)
              (clear-output stream)))))))
  nil nil nil)

(deftest-error finish-output-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (finish-output output)))

(deftest-error finish-output-concatenated.2
  (with-open-stream (stream (make-concatenated-stream))
    (force-output output)))

(deftest-error finish-output-concatenated.3
  (with-open-stream (stream (make-concatenated-stream))
    (clear-output output)))

(deftest finish-output-echo.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (progn
              (write-char #\a stream)
              (finish-output stream))
            (progn
              (write-char #\b stream)
              (force-output stream))
            (progn
              (write-char #\c stream)
              (clear-output stream)))))))
  nil nil nil)

(deftest finish-output-synonym.1
  (with-temp-file
    (with-overwrite-file (output *file*)
      (declare (special output))
      (with-open-stream (stream (make-synonym-stream 'output))
        (values
          (progn
            (write-char #\a stream)
            (finish-output stream))
          (progn
            (write-char #\b stream)
            (force-output stream))
          (progn
            (write-char #\c stream)
            (clear-output stream))))))
  nil nil nil)

(deftest finish-output-two-way.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (progn
              (write-char #\a stream)
              (finish-output stream))
            (progn
              (write-char #\b stream)
              (force-output stream))
            (progn
              (write-char #\c stream)
              (clear-output stream)))))))
  nil nil nil)

(deftest-error finish-output-input-stream.1
  (with-input-from-string (stream "Hello")
    (finish-output stream)))

(deftest-error finish-output-input-stream.2
  (with-input-from-string (stream "Hello")
    (force-output stream)))

(deftest-error finish-output-input-stream.3
  (with-input-from-string (stream "Hello")
    (clear-output stream)))

(deftest finish-output-output-stream.1
  (with-open-stream (stream (make-string-output-stream))
    (values
      (progn
        (write-char #\a stream)
        (finish-output stream))
      (progn
        (write-char #\b stream)
        (force-output stream))
      (progn
        (write-char #\c stream)
        (clear-output stream))))
  nil nil nil)

(deftest finish-output-extend-stream.1
  (with-extend-to-string
    (stream array)
    (values
      (progn
        (write-char #\a stream)
        (finish-output stream))
      (progn
        (write-char #\b stream)
        (force-output stream))
      (progn
        (write-char #\c stream)
        (clear-output stream))))
  nil nil nil)


;;
;;  close
;;
(deftest close-file.1
  (with-temp-file
    (let ((stream (open *file*)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream))))
  t t nil t nil)

(deftest close-file.2
  (with-temp-file
    (let ((stream (open *file*)))
      (values
        (open-stream-p stream)
        (close stream :abort t)
        (open-stream-p stream)
        (close stream :abort t)
        (open-stream-p stream))))
  t t nil t nil)

(deftest close-broadcast.1
  (with-open-stream (output (make-string-output-stream))
    (let ((stream (make-broadcast-stream output)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p output))))
  t t nil t nil t)

(deftest close-concatenated.1
  (with-input-from-string (input "Hello")
    (let ((stream (make-concatenated-stream input)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p input))))
  t t nil t nil t)

(deftest close-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (let ((stream (make-echo-stream input output)))
        (values
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (open-stream-p input)
          (open-stream-p output)))))
  t t nil t nil t t)

(deftest close-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (let ((stream (make-synonym-stream 'input)))
      (values
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (close stream)
        (open-stream-p stream)
        (open-stream-p input))))
  t t nil t nil t)

(deftest close-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (let ((stream (make-two-way-stream input output)))
        (values
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (close stream)
          (open-stream-p stream)
          (open-stream-p input)
          (open-stream-p output)))))
  t t nil t nil t t)

(deftest close-input-string.1
  (let ((stream (make-string-input-stream "Hello")))
    (values
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)))
  t t nil t nil)

(deftest close-input-string.2
  (let ((stream (make-string-output-stream)))
    (values
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)))
  t t nil t nil)

(deftest close-input-stream-p-file.1
  (with-temp-file
    (let ((input (open *file*)))
      (close input)
      (input-stream-p input)))
  t)

(deftest close-input-stream-p-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (close stream)
    (input-stream-p stream))
  nil)

(deftest close-input-stream-p-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (close stream)
    (input-stream-p stream))
  t)

(deftest close-output-stream-p.1
  (with-temp-file
    (let ((input (open *file* :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)))
      (close input)
      (output-stream-p input)))
  t)

(deftest close-interactive-stream-p.1
  (with-temp-file
    (let ((input (open *file*)))
      (close input)
      (interactive-stream-p input)))
  nil)

(deftest close-open-stream-p.1
  (with-temp-file
    (let ((input (open *file*)))
      (values
        (open-stream-p input)
        (progn
          (close input)
          (open-stream-p input)))))
  t nil)


;;  file-stream
;;  broadcast-stream
;;  concatenated-stream
;;  echo-stream
;;  synonym-stream
;;  two-way-stream
;;  input-string-stream
;;  output-string-stream
;;  extend-string-stream
;;  prompt-stream

