;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  input-stream-p
;;
(deftest string-input-stream-p.1
  (with-input-from-string (stream "Hello")
    (input-stream-p stream))
  t)

(deftest string-input-stream-p.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (input-stream-p stream)))
    result)
  nil)

(deftest string-input-stream-p.3
  (with-extend-to-string
    (inst array)
    (input-stream-p inst))
  nil)


;;
;;  output-stream-p
;;
(deftest string-output-stream-p.1
  (with-input-from-string (stream "Hello")
    (output-stream-p stream))
  nil)

(deftest string-output-stream-p.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (output-stream-p stream)))
    result)
  t)

(deftest string-output-stream-p.3
  (with-extend-to-string
    (inst array)
    (output-stream-p inst))
  t)


;;
;;  interactive-stream-p
;;
(deftest string-interactive-stream-p.1
  (with-input-from-string (stream "Hello")
    (interactive-stream-p stream))
  nil)

(deftest string-interactive-stream-p.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (interactive-stream-p stream)))
    result)
  nil)

(deftest string-interactive-stream-p.3
  (with-extend-to-string
    (inst array)
    (interactive-stream-p inst))
  nil)


;;
;;  open-stream-p
;;
(deftest string-open-stream-p.1
  (with-input-from-string (stream "Hello")
    (open-stream-p stream))
  t)

(deftest string-open-stream-p.2
  (with-input-from-string (stream "x")
    (read-char stream nil nil)
    (read-char stream nil nil)
    (read-char stream nil nil)
    (read-char stream nil nil)
    (open-stream-p stream))
  t)

(deftest string-open-stream-p.3
  (let ((stream (make-string-input-stream "Hello")))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest string-open-stream-p.4
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (open-stream-p stream)))
    result)
  t)

(deftest string-open-stream-p.5
  (let ((stream (make-string-output-stream)))
    (close stream)
    (open-stream-p stream))
  nil)

(deftest string-open-stream-p.6
  (with-extend-to-string
    (inst array)
    (open-stream-p inst))
  t)

(deftest string-open-stream-p.7
  (let (stream)
    (with-extend-to-string
      (inst array)
      (setq stream inst))
    (open-stream-p stream))
  nil)


;;
;;  stream-element-type
;;
(deftest string-stream-element-type.1
  (with-input-from-string (stream "Hello")
    (stream-element-type stream))
  character)

(deftest string-stream-element-type.2
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (stream-element-type stream)))
    result)
  character)

(deftest string-stream-element-type.3
  (with-extend-to-string
    (inst array)
    (stream-element-type inst))
  character)


;;
;;  read-byte
;;
(deftest-error string-read-byte.1
  (with-input-from-string (stream "Hello")
    (read-byte stream)))

(deftest-error string-read-byte.2
  (with-output-to-string (stream)
    (read-byte stream))
  type-error)

(deftest-error string-read-byte.3
  (with-extend-to-string
    (inst array)
    (read-byte inst))
  type-error)


;;
;;  write-byte
;;
(deftest-error string-write-byte.1
  (with-input-from-string (input "Hello")
    (write-byte 70 input)))

(deftest-error string-write-byte.2
  (with-output-to-string (output)
    (write-byte 70 output)))

(deftest-error string-write-byte.3
  (with-extend-to-string
    (output array)
    (write-byte 70 output)))


;;
;;  read-char
;;
(deftest string-read-char.1
  (with-input-from-string (stream "ABC")
    (read-char stream))
  #\A)

(deftest string-read-char.2
  (with-input-from-string (stream "ABC")
    (values
      (read-char stream nil :eof)
      (read-char stream nil :eof)
      (read-char stream nil :eof)
      (read-char stream nil :eof)
      (read-char stream nil :eof)))
  #\A #\B #\C :eof :eof)

(deftest-error string-read-char.3
  (with-output-to-string (stream)
    (read-char stream nil :eof)))

(deftest-error string-read-char.4
  (with-extend-to-string
    (stream array)
    (read-char stream nil :eof)))


;;
;;  read-char-no-hang
;;
(deftest string-read-char-no-hang.1
  (with-input-from-string (stream "ABC")
    (read-char-no-hang stream))
  #\A)

(deftest string-read-char-no-hang.2
  (with-input-from-string (stream "ABC")
    (values
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)))
  #\A #\B #\C :eof :eof)

(deftest-error string-read-char-no-hang.3
  (with-output-to-string (stream)
    (read-char-no-hang stream nil :eof)))

(deftest-error string-read-char-no-hang.4
  (with-extend-to-string
    (stream array)
    (read-char-no-hang stream nil :eof)))


;;
;;  unread-char
;;
(deftest string-unread-char.1
  (with-input-from-string (stream "ABC")
    (read-char stream)
    (unread-char #\Z stream)
    (values
      (read-char stream nil)
      (read-char stream nil)
      (read-char stream nil)
      (read-char stream nil)
      (read-char stream nil)))
  #\Z #\B #\C nil nil)

(deftest-error string-unread-char.2
  (with-output-to-string (stream)
    (unread-char #\Z stream)))

(deftest-error string-unread-char.3
  (with-extend-to-string
    (stream array)
    (unread-char #\Z stream)))


;;
;;  write-char
;;
(deftest-error string-write-char.1
  (with-open-stream (stream (make-string-input-stream "ABC"))
    (write-char #\A stream)))

(deftest string-write-char.2
  (with-output-to-string (stream)
    (write-char #\A stream)
    (write-char #\B stream))
  "AB")

(deftest string-write-char.3
  (with-extend-to-string
    (stream array)
    (write-char #\A stream)
    (write-char #\B stream)
    array)
  "AB")


;;
;;  read-line
;;
(deftest string-read-line.1
  (with-input-from-string (stream (format nil "aaa~%BBB"))
    (values
      (read-line stream nil :eof)
      (read-line stream nil :eof)
      (read-line stream nil :eof)))
  "aaa" "BBB" :eof)

(deftest-error string-read-line.2
  (with-output-to-string (stream)
    (read-line stream)))

(deftest-error string-read-line.3
  (with-extend-to-string
    (stream array)
    (read-line stream)))


;;
;;  file-length
;;
(deftest-error string-file-length.1
  (with-input-from-string (stream "Hello")
    (file-length stream)))

(deftest-error string-file-length.2
  (with-output-to-string (stream)
    (format stream "Hello")
    (file-length stream)))

(deftest-error string-file-length.3
  (with-extend-to-string
    (stream array)
    (format stream "Hello")
    (file-length stream)))


;;
;;  file-position
;;
(deftest string-file-position.1
  (with-input-from-string (input "Hello")
    (read-char input)
    (read-char input)
    (read-char input)
    (file-position input))
  3)

(deftest string-file-position.2
  (with-open-stream (stream (make-string-output-stream))
    (format stream "abc")
    (file-position stream))
  3)

(deftest string-file-position.3
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (file-position stream))
  3)

(deftest string-file-position-set.1
  (with-input-from-string (input "abcdef")
    (read-char input)
    (read-char input)
    (values
      (file-position input :start)
      (read-char input)))
  t #\a)

(deftest string-file-position-set.2
  (with-input-from-string (input "abcdef")
    (values
      (file-position input :end)
      (read-char input nil)))
  t nil)

(deftest string-file-position-set.3
  (with-input-from-string (input "abcdef")
    (values
      (file-position input 3)
      (read-char input)))
  t #\d)

(deftest string-file-position-set.4
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output :start)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "defg")

(deftest string-file-position-set.5
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output :end)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "abcdefg")

(deftest string-file-position-set.6
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output 2)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "abdefg")

(deftest string-file-position-set.7
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream :start)
      (progn
        (format stream "defg")
        array)))
  t "defg")

(deftest string-file-position-set.8
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream :end)
      (progn
        (format stream "defg")
        array)))
  t "abcdefg")

(deftest string-file-position-set.9
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream 2)
      (progn
        (format stream "defg")
        array)))
  t "abdefg")

(deftest string-file-position-unread.1
  (with-input-from-string (x "abcdef")
    (values
      (read-char x)
      (read-char x)
      (read-char x)
      (file-position x)
      (unread-char #\c x)
      (file-position x)
      (read-char x)
      (file-position x)))
  #\a #\b #\c 3 nil 2 #\c 3)

(deftest string-file-position-unread.2
  (with-input-from-string (x "abcdef")
    (values
      (read-char x)
      (read-char x)
      (read-char x)
      (file-position x)
      (unread-char #\c x)
      (file-position x 1)
      (read-char x)
      (file-position x)))
  #\a #\b #\c 3 nil t #\b 2)



;;
;;  file-string-length
;;
(deftest-error string-file-string-length.1
  (with-input-from-string (stream "Hello")
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111)))))

(deftest string-file-string-length.2
  (with-open-stream (stream (make-string-output-stream))
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  1 4)

(deftest string-file-string-length.3
  (with-extend-to-string
    (stream array)
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  1 4)


;;
;;  stream-external-format
;;
(deftest string-stream-external-format.1
  (with-input-from-string (stream "Hello")
    (stream-external-format stream))
  :default)

(deftest string-stream-external-format.2
  (with-open-stream (stream (make-string-output-stream))
    (stream-external-format stream))
  :default)

(deftest string-stream-external-format.3
  (with-extend-to-string
    (stream array)
    (stream-external-format stream))
  :default)


;;
;;  close
;;
(deftest string-close.1
  (let ((stream (make-string-input-stream "Hello")))
    (values
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)))
  t t nil t nil)

(deftest string-close.2
  (let ((stream (make-string-output-stream)))
    (values
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)
      (close stream)
      (open-stream-p stream)))
  t t nil t nil)

(deftest string-close.3
  (let ((x (make-string-input-stream "Hello")))
    (values
      (close x)
      (open-stream-p x)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (streamp x)
      (close x)))
  t nil t nil nil t t)

(deftest string-close.4
  (let ((x (make-string-output-stream)))
    (values
      (close x)
      (open-stream-p x)
      (input-stream-p x)
      (output-stream-p x)
      (interactive-stream-p x)
      (streamp x)
      (close x)))
  t nil nil t nil t t)


;;
;;  listen
;;
(deftest string-listen.1
  (with-input-from-string (stream "Hello")
    (listen stream))
  t)

(deftest-error string-listen.2
  (with-output-to-string (stream)
    (listen stream)))

(deftest-error string-listen.3
  (with-extend-to-string
    (stream array)
    (listen stream)))

(deftest string-listen.4
  (let ((x (make-string-input-stream "Hello")))
    (read-line x nil nil)
    (read-line x nil nil)
    (listen x))
  nil)


;;
;;  clear-input
;;
(deftest string-clear-input.1
  (with-input-from-string (input "Hello")
    (clear-input input))
  nil)

(deftest-error string-clear-input.2
  (with-output-to-string (stream)
    (clear-input stream)))

(deftest-error string-clear-input.3
  (with-extend-to-string
    (stream array)
    (clear-input stream)))

(deftest string-clear-input.4
  (with-input-from-string (x "Hello")
    (values
      (read-char x)
      (clear-input x)
      (read-char x)))
  #\H nil #\e)


;;
;;  finish-output
;;
(deftest-error string-finish-output.1
  (with-input-from-string (stream "Hello")
    (finish-output stream)))

(deftest string-finish-output.2
  (with-open-stream (stream (make-string-output-stream))
    (write-char #\a stream)
    (finish-output stream))
  nil)

(deftest string-finish-output.3
  (with-extend-to-string
    (stream array)
    (write-char #\a stream)
    (finish-output stream))
  nil)


;;
;;  force-output
;;
(deftest-error string-force-output.1
  (with-input-from-string (stream "Hello")
    (force-output stream)))

(deftest string-force-output.2
  (with-open-stream (stream (make-string-output-stream))
    (write-char #\a stream)
    (force-output stream))
  nil)

(deftest string-force-output.3
  (with-extend-to-string
    (stream array)
    (write-char #\a stream)
    (force-output stream))
  nil)


;;
;;  clear-output
;;
(deftest-error string-clear-output.1
  (with-input-from-string (stream "Hello")
    (clear-output stream)))

(deftest string-clear-output.2
  (with-open-stream (stream (make-string-output-stream))
    (write-char #\a stream)
    (clear-output stream))
  nil)

(deftest string-clear-output.3
  (with-extend-to-string
    (stream array)
    (write-char #\a stream)
    (clear-output stream))
  nil)


;;
;;  Function MAKE-STRING-INPUT-STREAM
;;
(deftest make-string-input-stream.1
  (typep (make-string-input-stream "Hello") 'string-stream)
  t)

(deftest make-string-input-stream.2
  (with-open-stream (x (make-string-input-stream "Hello"))
    (values
      (read-char x nil :eof)
      (read-char x nil :eof)
      (read-char x nil :eof)
      (read-char x nil :eof)
      (read-char x nil :eof)
      (read-char x nil :eof)))
  #\H #\e #\l #\l #\o :eof)

;;  start
(deftest-error make-string-input-stream-start.1
  (eval '(make-string-input-stream "Hello" -1)))

(deftest make-string-input-stream-start.2
  (with-open-stream (x (make-string-input-stream "Hello" 0))
    (read-line x))
  "Hello" t)

(deftest make-string-input-stream-start.3
  (with-open-stream (x (make-string-input-stream "Hello" 1))
    (read-line x))
  "ello" t)

(deftest make-string-input-stream-start.4
  (with-open-stream (x (make-string-input-stream "Hello" 4))
    (read-line x))
  "o" t)

(deftest make-string-input-stream-start.5
  (with-open-stream (x (make-string-input-stream "Hello" 5))
    (read-line x nil :eof))
  :eof t)

(deftest-error make-string-input-stream-start.6
  (make-string-input-stream "Hello" 6))

;;  end
(deftest-error make-string-input-stream-end.1
  (eval '(make-string-input-stream "Hello" 0 -1)))

(deftest make-string-input-stream-end.2
  (with-open-stream (x (make-string-input-stream "Hello" 0 0))
    (read-line x nil :eof))
  :eof t)

(deftest make-string-input-stream-end.3
  (with-open-stream (x (make-string-input-stream "Hello" 1 1))
    (read-line x nil :eof))
  :eof t)

(deftest make-string-input-stream-end.4
  (with-open-stream (x (make-string-input-stream "Hello" 0 1))
    (read-line x))
  "H" t)

(deftest make-string-input-stream-end.5
  (with-open-stream (x (make-string-input-stream "Hello" 0 5))
    (read-line x))
  "Hello" t)

(deftest make-string-input-stream-end.6
  (with-open-stream (x (make-string-input-stream "Hello" 0 nil))
    (read-line x))
  "Hello" t)

(deftest-error make-string-input-stream-end.7
  (make-string-input-stream "Hello" 0 6))

;;  start-end
(deftest make-string-input-stream-start-end.1
  (with-open-stream (x (make-string-input-stream "Hello" 1 4))
    (read-line x))
  "ell" t)

(deftest make-string-input-stream-start-end.2
  (with-open-stream (x (make-string-input-stream "Hello" 3 3))
    (read-line x nil :eof))
  :eof t)

(deftest-error make-string-input-stream-start-end.3
  (with-open-stream (x (make-string-input-stream "Hello" 4 3))
    (read-line x nil :eof)))

(deftest-error make-string-input-stream-error.1
  (eval '(make-string-input-stream 10))
  type-error)

(deftest-error make-string-input-stream-error.2
  (eval '(make-string-input-stream "Hello" nil))
  type-error)

(deftest-error make-string-input-stream-error.3
  (eval '(make-string-input-stream "Hello" 0 t))
  type-error)

(deftest-error! make-string-input-stream-error.4
  (eval '(make-string-input-stream)))

(deftest-error! make-string-input-stream-error.5
  (eval '(make-string-input-stream "Hello" 0 nil nil)))

;;  ANSI Common Lisp
(deftest make-string-input-stream-test.1
  (let ((string-stream (make-string-input-stream "1 one ")))
    (list (read string-stream nil nil)
          (read string-stream nil nil)
          (read string-stream nil nil)))
  (1 one nil))

(deftest make-string-input-stream-test.2
  (read (make-string-input-stream "prefixtargetsuffix" 6 12))
  target)


;;
;;  Function MAKE-STRING-OUTPUT-STREAM
;;
(deftest make-string-output-stream.1
  (typep (make-string-output-stream) 'string-stream)
  t)

(deftest make-string-output-stream.2
  (let ((inst (make-string-output-stream :element-type 'character)))
    (format inst "Hello")
    (get-output-stream-string inst))
  "Hello")

(deftest make-string-output-stream.3
  (let ((inst (make-string-output-stream :element-type 'character)))
    (get-output-stream-string inst))
  "")

(deftest make-string-output-stream.4
  (streamp
    (make-string-output-stream :element-type 'standard-char))
  t)

(deftest-error make-string-output-stream-error.1
  (eval '(make-string-output-stream 10)))

(deftest-error make-string-output-stream-error.2
  (eval '(make-string-output-stream :hello 20)))

(deftest-error make-string-output-stream-error.3
  (make-string-output-stream :element-type 'symbol))

(deftest-error make-string-output-stream-error.4
  (eval '(make-string-output-stream :element-type 30))
  type-error)

;;  ANSI Common Lisp
(deftest make-string-output-stream-test.1
  (let ((s (make-string-output-stream)))
    (write-string "testing... " s)
    (prin1 1234 s)
    (get-output-stream-string s))
  "testing... 1234")


;;
;;  Function GET-OUTPUT-STREAM-STRING
;;
(deftest get-output-stream-string.1
  (with-open-stream (stream (make-string-output-stream))
    (get-output-stream-string stream))
  "")

(deftest get-output-stream-string.2
  (with-open-stream (stream (make-string-output-stream))
    (format stream "Hello")
    (format stream "Hello")
    (get-output-stream-string stream))
  "HelloHello")

(deftest-error get-output-stream-string-error.1
  (eval '(get-output-stream-string 10))
  type-error)

(deftest-error get-output-stream-string-error.2
  (eval '(get-output-stream-string (make-string-input-stream "Hello")))
  type-error)

(deftest-error! get-output-stream-string-error.3
  (eval '(get-output-stream-string)))

(deftest-error! get-output-stream-string-error.4
  (eval '(get-output-stream-string (make-string-output-stream) nil)))

;;  ANSI Common Lisp
(deftest get-output-stream-string-test.1
  (let ((a-stream (make-string-output-stream))
        (a-string "abcdefghijklm"))
    (write-string a-string a-stream)
    (values
      (get-output-stream-string a-stream)
      (get-output-stream-string a-stream)))
  "abcdefghijklm"
  "")


;;
;;  system::end-input-stream
;;
(deftest end-input-stream.1
  (let ((inst (make-string-input-stream "Hello")))
    (lisp-system::end-input-stream inst))
  0)

(deftest end-input-stream.2
  (let ((inst (make-string-input-stream "Hello")))
    (values
      (read-char inst)
      (read-char inst)
      (read-char inst)
      (lisp-system::end-input-stream inst)))
  #\H #\e #\l 3)


;;
;;  Macro WITH-INPUT-FROM-STRING
;;
(deftest with-input-from-string.1
  (with-input-from-string (inst "Hello")
    (streamp inst))
  t)

(deftest with-input-from-string.2
  (with-input-from-string (inst "Hello")
    (values
      (read-char inst)
      (read-char inst)
      (read-char inst)))
  #\H #\e #\l)


(deftest with-input-from-string.3
  (with-input-from-string (inst "Hello" :start 2)
    (values
      (read-char inst)
      (read-char inst)
      (read-char inst)))
  #\l #\l #\o)

(deftest with-input-from-string.4
  (with-input-from-string (inst "Hello" :end 2)
    (values
      (read-char inst nil :eof)
      (read-char inst nil :eof)
      (read-char inst nil :eof)))
  #\H #\e :eof)

(deftest with-input-from-string.5
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos)
        (streamp inst))
      pos))
  t 0)

(deftest with-input-from-string.6
  (let ((pos 'error))
    (with-input-from-string (inst "Hello" :index pos)
      (values (streamp inst) pos)))
  t error)

(deftest with-input-from-string.7
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos)
        (list
          (read-char inst)
          (read-char inst)
          (read-char inst)))
      pos))
  (#\H #\e #\l) 3)

(deftest with-input-from-string.8
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :start 2 :index pos)
        (list
          (read-char inst)
          (read-char inst)
          (read-char inst)))
      pos))
  (#\l #\l #\o) 5)

(deftest with-input-from-string.9
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos :end 2)
        (list
          (read-char inst nil :eof)
          (read-char inst nil :eof)
          (read-char inst nil :eof)))
      pos))
  (#\H #\e :eof) 2)

(deftest with-input-from-string.10
  (with-input-from-string (inst "Hello" :start 1 :end 4)
    (read-line inst))
  "ell" t)

(deftest with-input-from-string.11
  (let ((pos 'error))
    (values
      (with-input-from-string (inst "Hello" :index pos :start 1 :end 4)
        (list
          (read-char inst nil :eof)
          (read-char inst nil :eof)))
      pos))
  (#\e #\l) 3)

(deftest with-input-from-string.12
  (let ((pos (cons :a :b)))
    (values
      (with-input-from-string (inst "Hello" :index (car pos))
        (list
          (read-char inst)
          (read-char inst)
          (read-char inst)))
      pos))
  (#\H #\e #\l) (3 . :b))

(deftest-error with-input-from-string-error.1
  (eval '(with-input-from-string)))

(deftest-error with-input-from-string-error.2
  (eval '(with-input-from-string ())))

(deftest-error with-input-from-string-error.3
  (eval '(with-input-from-string ())))

(deftest-error with-input-from-string-error.4
  (eval '(with-input-from-string (10 "Hello"))))

(deftest-error with-input-from-string-error.5
  (eval '(with-input-from-string (x "Hello" :hello) x)))

(deftest-error with-input-from-string-error.6
  (eval '(with-input-from-string (x "Hello" :index 30) x)))

(deftest-error with-input-from-string-error.7
  (eval '(with-input-from-string (x "Hello" :hello 30) x)))

(deftest-error with-input-from-string-error.8
  (eval '(with-input-from-string (x "Hello" :start t) x)))

;;  ANSI Common Lisp
(deftest with-input-from-string-test.1
  (let (ind ans)
    (with-input-from-string (s "XXX1 2 3 4xxx"
                               :index ind
                               :start 3 :end 10)
      (setq ans (+ (read s) (read s) (read s))))
    (values ans ind))
  6 9)

(deftest with-input-from-string-test.2
  (let (j ans)
    (with-input-from-string (s "Animal Crackers" :index j :start 6)
      (setq ans (read s)))
    (values ans j))
  crackers 15)


;;
;;  Macro WITH-OUTPUT-TO-STRING
;;
(deftest with-output-to-string.1
  (with-output-to-string (inst)
    (format inst "Hello"))
  "Hello")

(deftest with-output-to-string.2
  (with-output-to-string (inst nil :element-type 'character)
    (format inst "a")
    (format inst "b")
    (format inst "C"))
  "abC")

(deftest with-output-to-string.3
  (with-extend-to-string
    (inst array)
    (format inst "Hello")
    :aaa)
  :aaa)

(deftest with-output-to-string.4
  (with-extend-to-string
    (inst array)
    (format inst "Hello")
    array)
  "Hello")

(deftest with-output-to-string.5
  (let ((fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (format s "here's some output")
      (input-stream-p s))
    fstr)
  "here's some output")

(deftest-error with-output-to-string-error.1
  (eval '(with-output-to-string)))

(deftest-error with-output-to-string-error.2
  (eval '(with-output-to-string nil)))

(deftest-error with-output-to-string-error.3
  (eval '(with-output-to-string (10))))

(deftest-error with-output-to-string-error.4
  (eval '(with-output-to-string (x 20) x)))

(deftest-error with-output-to-string-error.5
  (eval '(with-output-to-string (x nil :hello) x)))

(deftest-error with-output-to-string-error.6
  (eval '(with-output-to-string (x nil :hello) x)))

(deftest-error with-output-to-string-error.7
  (eval '(with-output-to-string (x nil :hello 30) x)))

(deftest-error with-output-to-string-error.8
  (eval '(with-output-to-string (x nil :element-type 'hello) x)))

(deftest-error with-output-to-string-error.9
  (eval '(with-output-to-string (x nil :element-type 40) x)))

