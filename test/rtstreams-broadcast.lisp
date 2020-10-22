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

