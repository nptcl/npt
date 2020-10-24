;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  Function PEEK-CHAR
;;
(deftest peek-char.1
  (with-temp-file
    (with-open-file (stream *file*)
      (values
        (peek-char nil stream)
        (peek-char nil stream))))
  #\A #\A)

(deftest peek-char.2
  (with-temp-file
    (with-open-file (stream *file*)
      (values
        (peek-char nil stream)
        (peek-char nil stream)
        (read-char stream)
        (read-char stream))))
  #\A #\A #\A #\B)

(deftest peek-char.3
  (with-make-file
    (*file* (format nil "   ~A~A  ABC" #\Return #\Newline))
    (with-open-file (stream *file*)
      (values
        (peek-char nil stream)
        (peek-char nil stream))))
  #\Space #\Space)

(deftest peek-char.4
  (with-make-file
    (*file* (format nil "   ~A~A  ABC" #\Return #\Newline))
    (with-open-file (stream *file*)
      (values
        (peek-char t stream)
        (peek-char t stream))))
  #\A #\A)

(deftest peek-char.5
  (with-make-file
    (*file* (format nil "   ~A~A  ABC" #\Return #\Newline))
    (with-open-file (stream *file*)
      (values
        (peek-char #\B stream nil :eof)
        (peek-char #\B stream nil :eof)
        (read-char stream)
        (peek-char #\B stream nil :eof))))
  #\B #\B #\B :eof)

(deftest-error peek-char.6
  (with-make-file
    (*file* (format nil "   ~A~A  ABC" #\Return #\Newline))
    (with-open-file (stream *file*)
      (values
        (peek-char #\B stream)
        (peek-char #\B stream)
        (read-char stream)
        (peek-char #\B stream))))
  end-of-file)

(deftest-error peek-char.7
  (with-make-file
    (*file* "Hello")
    (with-open-file (stream *file* :element-type 'unsigned-byte)
      (peek-char nil stream))))

(deftest peek-char.8
  (with-input-from-string (*standard-input* "ABC")
    (peek-char))
  #\A)

(deftest peek-char.9
  (with-input-from-string (*standard-input* "  ABC")
    (peek-char))
  #\Space)

(deftest-error peek-char.10
  (with-input-from-string (stream "  ")
    (peek-char t stream t nil t))
  end-of-file)

(deftest-error peek-char-error.1
  (eval '(peek-char 10))
  type-error)

(deftest-error peek-char-error.2
  (eval '(peek-char nil 20))
  type-error)

(deftest-error! peek-char-error.3
  (eval '(peek-char nil *standard-input* nil nil nil nil)))

;;  ANSI Common Lisp
(deftest peek-char-test.1
  (with-input-from-string (input-stream "    1 2 3 4 5")
    (values
      (peek-char t input-stream)
      (peek-char #\4 input-stream)
      (peek-char nil input-stream)))
  #\1 #\4 #\4)


;;
;;  Function READ-CHAR
;;
(deftest read-char.1
  (with-input-from-string (*standard-input* "ABC")
    (read-char))
  #\A)

(deftest-error read-char.2
  (with-input-from-string (*standard-input* "")
    (read-char))
  end-of-file)

(deftest-error read-char.3
  (with-input-from-string (stream "")
    (read-char stream t nil))
  end-of-file)

(deftest-error read-char.4
  (with-input-from-string (stream "")
    (read-char stream t nil t))
  end-of-file)

(deftest read-char.5
  (with-input-from-string (stream "")
    (read-char stream nil :hello t))
  :hello)

(deftest-error read-char.6
  (let ((stream (make-string-input-stream "Hello")))
    (close stream)
    (read-char stream)))

(deftest read-char.7
  (with-input-from-string (stream "ABC")
    (read-char stream)
    (unread-char #\Z stream)
    (read-char stream))
  #\Z)

(deftest-error read-char-error.1
  (eval '(read-char 10))
  type-error)

(deftest-error! read-char-error.2
  (eval '(read-char *standard-input* nil nil nil nil)))

;;  ANSI Common Lisp
(deftest read-char-test.1
  (with-input-from-string (is "0123")
    (do ((c (read-char is) (read-char is nil 'the-end))
         list)
      ((not (characterp c))
       (nreverse list))
      (push c list)))
  (#\0 #\1 #\2 #\3))


;;
;;  read-char-no-hang
;;
#-windows
(deftest read-char-no-hang.1
  (read-char-no-hang *standard-input*)
  nil)

#-windows
(deftest read-char-no-hang-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (read-char-no-hang input)))
  #\A)

#-windows
(deftest read-char-no-hang-file.2
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input)
        (read-char-no-hang input)
        (read-char-no-hang input))))
  #\A #\B #\C)

#-windows
(deftest-error read-char-no-hang-file.3
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input)
        (read-char-no-hang input)
        (read-char-no-hang input)
        (read-char-no-hang input))))
  end-of-file)

#-windows
(deftest read-char-no-hang-file.4
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input nil)
        (read-char-no-hang input nil)
        (read-char-no-hang input nil)
        (read-char-no-hang input nil))))
  #\A #\B #\C nil)

#-windows
(deftest read-char-no-hang-file.5
  (with-temp-file
    (with-open-file (input *file*)
      (values
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof))))
  #\A #\B #\C :eof)

#-windows
(deftest-error read-char-no-hang-file.6
  (with-temp-file
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (read-char-no-hang input nil :eof))))

(deftest-error read-char-no-hang-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (read-char-no-hang stream nil)))

(deftest read-char-no-hang-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (read-char-no-hang stream nil))
  nil)

(deftest read-char-no-hang-concatenated.2
  (with-make-file
    (*file1* "AB")
    (with-open-file (input *file1*)
      (with-open-stream (stream (make-concatenated-stream input))
        (read-char-no-hang stream))))
  #\A)

(deftest read-char-no-hang-concatenated.3
  (with-make-file
    (*file1* "AB")
    (with-open-file (input *file1*)
      (with-open-stream (stream (make-concatenated-stream input))
        (values
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)))))
  #\A #\B :eof)

(deftest read-char-no-hang-concatenated.4
  (with-make-file
    (*file1* "AB")
    (with-make-file
      (*file2* "CD")
      (with-open-file (input1 *file1*)
        (with-open-file (input2 *file2*)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (values
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)
              (read-char-no-hang stream nil :eof)))))))
  #\A #\B #\C #\D :eof :eof)

(deftest read-char-no-hang-echo.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char-no-hang stream)))))
  #\A)

(deftest read-char-no-hang-echo.2
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char-no-hang stream))))
    (with-open-file (input *file2*)
      (values
        (read-char-no-hang input nil nil)
        (read-char-no-hang input nil nil))))
  #\A nil)

(deftest read-char-no-hang-echo.3
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof))))))
  #\A #\B #\C :eof :eof)

(deftest read-char-no-hang-echo.4
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-echo-stream input output))
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof))))
    (with-open-file (input *file2*)
      (values
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof)
        (read-char-no-hang input nil :eof))))
  #\A #\B #\C :eof :eof)

(deftest read-char-no-hang-synonym.1
  (with-temp-file
    (with-open-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (read-char-no-hang stream))))
  #\A)

(deftest read-char-no-hang-synonym.2
  (with-temp-file
    (with-open-file (hello *file*)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (values
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)
          (read-char-no-hang stream nil :eof)))))
  #\A #\B #\C :eof :eof)

(deftest read-char-no-hang-two-way.1
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (read-char-no-hang stream)))))
  #\A)

(deftest read-char-no-hang-two-way.2
  (with-temp-file1-file2
    (with-open-file (input *file1*)
      (with-overwrite-file (output *file2*)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof)
            (read-char-no-hang stream nil :eof))))))
  #\A #\B #\C :eof :eof)

(deftest read-char-no-hang-input-string.1
  (with-input-from-string (stream "ABC")
    (read-char-no-hang stream))
  #\A)

(deftest read-char-no-hang-input-string.2
  (with-input-from-string (stream "ABC")
    (values
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)
      (read-char-no-hang stream nil :eof)))
  #\A #\B #\C :eof :eof)

(deftest-error read-char-no-hang-output-string.1
  (with-output-to-string (stream)
    (read-char-no-hang stream nil :eof)))

(deftest-error read-char-no-hang-extend-string.1
  (with-extend-to-string
    (stream array)
    (read-char-no-hang stream nil :eof)))


;;
;;  terpri
;;
(deftest terpri.1
  (map 'list
       #'char-code
       (with-output-to-string (stream)
         (terpri stream)
         (terpri stream)
         (terpri stream)))
  (10 10 10))


;;
;;  fresh-line
;;
(deftest fresh-line-file.1
  (with-temp-file
    (with-overwrite-file (output *file*)
      (format output "A")
      (fresh-line output)
      (fresh-line output)
      (fresh-line output)
      (format output "B")
      (fresh-line output)
      (fresh-line output)
      (fresh-line output))
    (with-open-file (input *file*)
      (values
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof)
        (read-char input nil :eof))))
  #\A #\Newline #\B #\Newline :eof)

(deftest fresh-line-broadcast.1
  (with-temp-file
    (with-open-stream (output1 (make-string-output-stream))
      (with-overwrite-file (output2 *file*)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (format stream "A")
          (fresh-line stream)
          (fresh-line stream)
          (fresh-line stream)))
      (let (result)
        (with-open-file (input *file*)
          (push (read-char input nil :eof) result)
          (push (read-char input nil :eof) result)
          (push (read-char input nil :eof) result))
        (values
          (nreverse result)
          (map 'list #'values (get-output-stream-string output1))))))
  (#\A #\Newline :eof) (#\A #\Newline))

(deftest fresh-line-broadcast.2
  (with-open-stream (stream (make-broadcast-stream))
    (fresh-line stream))
  nil)

(deftest-error fresh-line-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (fresh-line stream)))

(deftest fresh-line-echo.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (read-char stream)
        (fresh-line stream)
        (fresh-line stream)
        (fresh-line stream)
        (read-char stream)
        (map 'list #'values (get-output-stream-string output)))))
  (#\A #\Newline #\B))

(deftest fresh-line-synonym.1
  (with-open-stream (hello (make-string-output-stream))
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (format stream "A")
      (fresh-line stream)
      (fresh-line stream)
      (fresh-line stream))
    (map 'list #'values (get-output-stream-string hello)))
  (#\A #\Newline))

(deftest fresh-line-two-way.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (format stream "A")
        (fresh-line stream)
        (fresh-line stream)
        (fresh-line stream)
        (format stream "C")
        (map 'list #'values (get-output-stream-string output)))))
  (#\A #\Newline #\C))

(deftest-error fresh-line-input-string.1
  (with-input-from-string (stream "Hello")
    (fresh-line stream)))

(deftest fresh-line-output-stream.1
  (with-open-stream (stream (make-string-output-stream))
    (format stream "A")
    (fresh-line stream)
    (fresh-line stream)
    (fresh-line stream)
    (map 'list #'values (get-output-stream-string stream)))
  (#\A #\Newline))

(deftest fresh-line-extend-stream.1
  (with-extend-to-string
    (stream array)
    (format stream "A")
    (fresh-line stream)
    (fresh-line stream)
    (fresh-line stream)
    (map 'list #'values array))
  (#\A #\Newline))


;;
;;  unread-char
;;
(deftest unread-char-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (read-char input)
      (unread-char #\Z input)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\Z #\B #\C nil)

(deftest unread-char-file.2
  (with-temp-file
    (with-open-file (input *file*)
      (unread-char #\Z input)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\Z #\A #\B #\C nil)

(deftest-error unread-char-file.3
  (with-temp-file
    (with-overwrite-file (output *file*)
      (read-char input))))

(deftest-error unread-char-file.4
  (with-temp-file
    (with-open-file (output *file* :element-type 'unsigned-byte)
      (read-char input))))

(deftest-error unread-char-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (unread-char #\Z stream)))

(deftest unread-char-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (unread-char #\Z stream))
  nil)

(deftest unread-char-echo.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (read-char stream)
        (unread-char #\Z stream)
        (values
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)))))
  #\Z #\B #\C nil nil)

(deftest unread-char-echo.2
  (with-input-from-string (input "Hello")
    (with-output-to-string (output)
      (with-open-stream (stream (make-echo-stream input output))
        (read-char stream)
        (read-char stream)
        (unread-char #\e stream)
        (read-char stream)
        (unread-char #\e stream)
        (read-char stream)
        (unread-char #\e stream)
        (read-char stream))))
  "He")

(deftest unread-char-echo.3
  (with-input-from-string (input "Hello")
    (with-output-to-string (output)
      (with-open-stream (stream (make-echo-stream input output))
        (read-char-no-hang stream)
        (read-char-no-hang stream)
        (unread-char #\e stream)
        (read-char-no-hang stream)
        (unread-char #\e stream)
        (read-char-no-hang stream)
        (unread-char #\e stream)
        (read-char-no-hang stream))))
  "He")

(deftest unread-char-synonym.1
  (with-input-from-string (hello "ABC")
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (read-char stream)
      (unread-char #\Z stream)
      (values
        (read-char stream nil)
        (read-char stream nil)
        (read-char stream nil)
        (read-char stream nil)
        (read-char stream nil))))
  #\Z #\B #\C nil nil)

(deftest unread-char-two-way.2
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (read-char stream)
        (unread-char #\Z stream)
        (values
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)
          (read-char stream nil)))))
  #\Z #\B #\C nil nil)

(deftest unread-char-input-string.1
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

(deftest-error unread-char-output-string.1
  (with-output-to-string (stream)
    (unread-char #\Z stream)))

(deftest-error unread-char-extend-string.1
  (with-extend-to-string
    (stream array)
    (unread-char #\Z stream)))


;;
;;  write-char
;;
(deftest write-char-file.1
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (write-char #\A stream)
      (write-char #\B stream))
    (with-open-file (input *file*)
      (values
        (read-char input nil)
        (read-char input nil)
        (read-char input nil))))
  #\A #\B nil)

(deftest-error write-char-file.2
  (with-temp-file
    (with-open-file (input *file*)
      (write-char #\A stream))))

(deftest write-char-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (write-char #\A stream))
  #\A)

(deftest write-char-broadcast.2
  (with-open-stream (output1 (make-string-output-stream))
    (with-open-stream (output2 (make-string-output-stream))
      (with-open-stream (stream (make-broadcast-stream output1 output2))
        (write-char #\Z output1)
        (write-char #\A stream)
        (write-char #\B stream))
      (values
        (get-output-stream-string output1)
        (get-output-stream-string output2))))
  "ZAB" "AB")

(deftest-error write-char-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (write-char #\A stream)))

(deftest write-char-echo.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (write-char #\Z stream)
        (read-char stream)
        (read-char stream))
      (get-output-stream-string output)))
  "ZAB")

(deftest write-char-synonym.1
  (with-open-stream (hello (make-string-output-stream))
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (write-char #\A stream)
      (write-char #\B stream))
    (get-output-stream-string hello))
  "AB")

(deftest write-char-two-way.1
  (with-open-stream (input (make-string-input-stream "ABC"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (write-char #\A stream)
        (write-char #\B stream)
        (read-char stream)
        (read-char stream))
      (get-output-stream-string output)))
  "AB")

(deftest-error write-char-input-string.1
  (with-open-stream (stream (make-string-input-stream "ABC"))
    (write-char #\A stream)))

(deftest write-char-output-string.1
  (with-output-to-string (stream)
    (write-char #\A stream)
    (write-char #\B stream))
  "AB")

(deftest write-char-extend-string.1
  (with-extend-to-string
    (stream array)
    (write-char #\A stream)
    (write-char #\B stream)
    array)
  "AB")


;;
;;  read-line
;;
(deftest read-line-file.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-line input)))
  "abcd" t)

(deftest read-line-file.2
  (with-make-file
    (*file* "abcd~%")
    (with-open-file (input *file*)
      (read-line input)))
  "abcd" nil)

(deftest-error read-line-file.3
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-line input)
      (read-line input)))
  end-of-file)

(deftest-error read-line-file.4
  (with-make-file
    (*file* "abcd~%")
    (with-open-file (input *file*)
      (read-line input)
      (read-line input)))
  end-of-file)

(deftest read-line-file.5
  (with-make-file
    (*file* "aaa~%~%bbb")
    (with-open-file (input *file*)
      (values
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof))))
  "aaa" "" "bbb" :eof)

(deftest read-line-file.6
  (with-make-file
    (*file* "aaa~%~%bbb~%")
    (with-open-file (input *file*)
      (values
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof))))
  "aaa" "" "bbb" :eof)

(deftest read-line-file.7
  (with-make-file
    (*file* "aaa~%~%bbb~%~%")
    (with-open-file (input *file*)
      (values
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof)
        (read-line input nil :eof))))
  "aaa" "" "bbb" "" :eof)

(deftest-error read-line-file.8
  (with-make-file
    (*file* "aaa")
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (read-line input))))

(deftest-error read-line-file.9
  (with-make-file
    (*file* "aaa")
    (with-overwrite-file (input *file*)
      (read-line input))))

(deftest-error read-line-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (read-line stream)))

(deftest read-line-concatenated.1
  (with-input-from-string (input1 (format nil "aaa~%bbb"))
    (with-input-from-string (input2 (format nil "ccc~%ddd~%"))
      (with-open-stream (stream (make-concatenated-stream input1 input2))
        (values
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)))))
  "aaa" "bbbccc" "ddd" :eof)

(deftest read-line-echo.1
  (with-input-from-string (input (format nil "aaa~%BBB"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (values
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)))))
  "aaa" "BBB" :eof)

(deftest read-line-echo.2
  (with-input-from-string (input (format nil "aaa~%BBB"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (read-line stream nil :eof)
        (read-line stream nil :eof)
        (read-line stream nil :eof))
      (equal (format nil "aaa~%BBB")
             (get-output-stream-string output))))
  t)

(deftest read-line-synonym.1
  (with-input-from-string (hello (format nil "aaa~%~%bbb~%"))
    (declare (special hello))
    (with-open-stream (stream (make-synonym-stream 'hello))
      (values
        (read-line stream nil :eof)
        (read-line stream nil :eof)
        (read-line stream nil :eof)
        (read-line stream nil :eof))))
  "aaa" "" "bbb" :eof)

(deftest read-line-two-way.1
  (with-input-from-string (input (format nil "aaa~%BBB"))
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (values
          (read-line stream nil :eof)
          (read-line stream nil :eof)
          (read-line stream nil :eof)))))
  "aaa" "BBB" :eof)

(deftest read-line-input-string.1
  (with-input-from-string (stream (format nil "aaa~%BBB"))
    (values
      (read-line stream nil :eof)
      (read-line stream nil :eof)
      (read-line stream nil :eof)))
  "aaa" "BBB" :eof)

(deftest-error read-line-output-string.1
  (with-output-to-string (stream)
    (read-line stream)))

(deftest-error read-line-extend-string.1
  (with-extend-to-string
    (stream array)
    (read-line stream)))


;;
;;  write-string
;;
(deftest write-string-file.1
  (with-output-to-string (stream)
    (write-string "aaa" stream)
    (write-string "bbb" stream))
  "aaabbb")

(deftest write-string-file.2
  (with-output-to-string (stream)
    (write-string "abcdef" stream :start 2)
    (write-string "bbb" stream))
  "cdefbbb")

(deftest write-string-file.3
  (with-output-to-string (stream)
    (write-string "abcdef" stream :end 2)
    (write-string "bbb" stream))
  "abbbb")

(deftest write-string-file.4
  (with-output-to-string (stream)
    (write-string "abcdef" stream :start 3 :end 5)
    (write-string "bbb" stream))
  "debbb")


;;
;;  write-line
;;
(deftest write-line-file.1
  (equal (format nil "aaa~%bbb~%")
         (with-output-to-string (stream)
           (write-line "aaa" stream)
           (write-line "bbb" stream)))
  t)

(deftest write-line-file.2
  (equal (format nil "cdef~%bbb~%")
         (with-output-to-string (stream)
           (write-line "abcdef" stream :start 2)
           (write-line "bbb" stream)))
  t)

(deftest write-line-file.3
  (equal (format nil "ab~%bbb~%")
         (with-output-to-string (stream)
           (write-line "abcdef" stream :end 2)
           (write-line "bbb" stream)))
  t)

(deftest write-line-file.4
  (equal (format nil "de~%bbb~%")
         (with-output-to-string (stream)
           (write-line "abcdef" stream :start 3 :end 5)
           (write-line "bbb" stream)))
  t)


;;
;;  read-sequence
;;
(deftest read-sequence.1
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "Hello a")
      (values
        (read-sequence array input)
        array)))
  7 #(#\H #\e #\l #\l #\o #\Space #\a :a :a :a))

(deftest read-sequence.2
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5)
        array)))
  8 #(:a :a :a :a :a #\a #\b #\c :a :a))

(deftest read-sequence.3
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "abc")
      (values
        (read-sequence array input :start 5 :end 7)
        array)))
  7 #(:a :a :a :a :a #\a #\b :a :a :a))

(deftest read-sequence.4
  (let ((array (make-array 10 :initial-element :a)))
    (with-input-from-string (input "abcdefg")
      (values
        (read-sequence array input :start 5)
        array)))
  10 #(:a :a :a :a :a #\a #\b #\c #\d #\e))


;;
;;  write-sequence
;;
(deftest write-sequence.1
  (with-output-to-string (stream)
    (write-sequence "abcdef" stream))
  "abcdef")

(deftest write-sequence.2
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 4))
  "Zef")

(deftest write-sequence.3
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :end 3))
  "Zabc")

(deftest write-sequence.4
  (with-output-to-string (stream)
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 2 :end 3))
  "Zc")

(deftest write-sequence.5
  (with-open-stream (stream (make-string-output-stream))
    (write-char #\Z stream)
    (write-sequence "abcdef" stream :start 2 :end 3))
  "abcdef")


;;
;;  file-length
;;
(deftest file-length-file.1
  (with-temp-file
    (with-open-file (input *file*)
      (file-length input)))
  3)

(deftest file-length-file.2
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file* :element-type 'unsigned-byte)
      (file-length input)))
  6)

(deftest file-length-file.3
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (file-length input)))
  6)

(deftest file-length-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-length stream))
  0)

(deftest file-length-broadcast.2
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (input2 *file2* :direction :io :if-exists :overwrite)
          (with-open-stream (stream (make-broadcast-stream input1 input2))
            (file-length stream))))))
  4)

(deftest-error file-length-concatenated.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input1 *file1*)
        (with-open-file (input2 *file2*)
          (with-open-stream (stream (make-concatenated-stream input1 input2))
            (file-length stream)))))))

(deftest-error file-length-echo.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input *file1*)
        (with-overwrite-file (output *file2*)
          (with-open-stream (stream (make-echo-stream input output))
            (file-length stream)))))))

(deftest file-length-synonym.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (declare (special input))
      (with-open-stream (stream (make-synonym-stream 'input))
        (file-length stream))))
  4)

(deftest-error file-length-two-way.1
  (with-make-file
    (*file1* "abc")
    (with-make-file
      (*file2* "cdef")
      (with-open-file (input *file1*)
        (with-overwrite-file (output *file2*)
          (with-open-stream (stream (make-echo-stream input output))
            (file-length stream)))))))

(deftest-error file-length-input-stream.1
  (with-input-from-string (stream "Hello")
    (file-length stream)))

(deftest-error file-length-output-stream.1
  (with-output-to-string (stream)
    (format stream "Hello")
    (file-length stream)))

(deftest-error file-length-extend-stream.1
  (with-extend-to-string
    (stream array)
    (format stream "Hello")
    (file-length stream)))


;;
;;  file-position
;;
(deftest file-position-file.1
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (file-position input)))
  0)

(deftest file-position-file.2
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-char input)
      (file-position input)))
  1)

(deftest file-position-file.3
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file*)
      (read-char input)
      (read-char input)
      (unread-char #\a input)
      (file-position input)))
  1)

(deftest file-position-file.4
  (with-temp-file
    (with-overwrite-file (output *file*)
      (file-position output)))
  0)

(deftest file-position-file.5
  (with-temp-file
    (with-overwrite-file (output *file*)
      (format output "Hello")
      (file-position output)))
  5)

(deftest file-position-file.6
  (with-temp-file
    (with-overwrite-file (output *file*)
      (let ((array (make-array 70000 :initial-element #\z)))
        (write-sequence array output)
        (file-position output))))
  70000)

(deftest file-position-file.7
  (with-make-file
    (*file* "abcd")
    (with-open-file (input *file* :direction :io :if-exists :overwrite)
      (read-char input)
      (read-char input)
      (file-position input)))
  2)

(deftest file-position-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-position stream))
  0)

(deftest file-position-broadcast.2
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdef")
      (with-open-file (input1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (input2 *file2* :direction :io :if-exists :overwrite)
          (read-char input1)
          (read-char input1)
          (read-char input2)
          (read-char input2)
          (read-char input2)
          (with-open-stream (stream (make-broadcast-stream input1 input2))
            (file-position stream))))))
  3)

(deftest file-position-concatenated.1
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream)))
  nil)

(deftest file-position-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream))))
  nil)

(deftest file-position-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'hello))
      (file-position input)))
  2)

(deftest file-position-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream))))
  nil)

(deftest file-position-input-string.1
  (with-input-from-string (input "Hello")
    (read-char input)
    (read-char input)
    (read-char input)
    (file-position input))
  3)

(deftest file-position-output-string.1
  (with-open-stream (stream (make-string-output-stream))
    (format stream "abc")
    (file-position stream))
  3)

(deftest file-position-extend-string.1
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (file-position stream))
  3)


;;
;;  file-position set
;;
(deftest file-position-set-file.1
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-position-set-file.2
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (read-char input)
      (unread-char #\a input)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-position-set-file.3
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input 3)
        (read-char input)
        (read-char input))))
  t #\d #\e)

(deftest file-position-set-file.4
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input :start)
        (read-char input))))
  t #\a)

(deftest file-position-set-file.5
  (with-make-file
    (*file* "abcdef")
    (with-open-file (input *file*)
      (values
        (file-position input :end)
        (read-char input nil :eof))))
  t :eof)

(deftest file-position-set-file.6
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :start)))
  t)

(deftest file-position-set-file.7
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :start)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "cdef" t)

(deftest file-position-set-file.8
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :end)))
  t)

(deftest file-position-set-file.9
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream :end)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "abccdef" t)

(deftest file-position-set-file.10
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream 2)))
  t)

(deftest file-position-set-file.11
  (with-make-file
    (*file* "")
    (with-overwrite-file (stream *file*)
      (format stream "abc")
      (file-position stream 2)
      (format stream "cdef"))
    (with-open-file (input *file*)
      (read-line input)))
  "abcdef" t)

(deftest file-position-set-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (file-position stream :start))
  nil)

(deftest file-position-set-broadcast.2
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream :start)
              (read-char stream1)
              (read-char stream2)))))))
  t #\H #\a)

(deftest file-position-set-broadcast.3
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream :end)
              (read-char stream1 nil :eof)
              (read-char stream2 nil :eof)))))))
  t :eof :eof)

(deftest file-position-set-broadcast.4
  (with-make-file
    (*file1* "Hello")
    (with-make-file
      (*file2* "abcdefg")
      (with-open-file (stream1 *file1* :direction :io :if-exists :overwrite)
        (with-open-file (stream2 *file2* :direction :io :if-exists :overwrite)
          (read-char stream1)
          (read-char stream1)
          (read-char stream2)
          (read-char stream2)
          (read-char stream2)
          (with-open-stream (stream (make-broadcast-stream stream1 stream2))
            (values
              (file-position stream 2)
              (read-char stream1)
              (read-char stream2)))))))
  t #\l #\c)

(deftest file-position-set-concatenated.1
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream :start)))
  nil)

(deftest file-position-set-concatenated.2
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream :end)))
  nil)

(deftest file-position-set-concatenated.3
  (with-input-from-string (input "Hello")
    (with-open-stream (stream (make-concatenated-stream input))
      (file-position stream 2)))
  nil)

(deftest file-position-set-echo.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream :start))))
  nil)

(deftest file-position-set-echo.2
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream :end))))
  nil)

(deftest file-position-set-echo.3
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-echo-stream input output))
        (file-position stream 3))))
  nil)

(deftest file-position-set-synonym.1
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream :start)
        (read-char stream))))
  t #\H)

(deftest file-position-set-synonym.2
  (with-input-from-string (input "Hello")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream :end)
        (read-char stream nil))))
  t nil)

(deftest file-position-set-synonym.3
  (with-input-from-string (input "abcdef")
    (declare (special input))
    (read-char input)
    (read-char input)
    (with-open-stream (stream (make-synonym-stream 'input))
      (values
        (file-position stream 3)
        (read-char stream))))
  t #\d)

(deftest file-position-set-two-way.1
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream :start))))
  nil)

(deftest file-position-set-two-way.2
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream :end))))
  nil)

(deftest file-position-set-two-way.3
  (with-input-from-string (input "Hello")
    (with-open-stream (output (make-string-output-stream))
      (with-open-stream (stream (make-two-way-stream input output))
        (file-position stream 3))))
  nil)

(deftest file-position-set-input-string.1
  (with-input-from-string (input "abcdef")
    (read-char input)
    (read-char input)
    (values
      (file-position input :start)
      (read-char input)))
  t #\a)

(deftest file-position-set-input-string.2
  (with-input-from-string (input "abcdef")
    (values
      (file-position input :end)
      (read-char input nil)))
  t nil)

(deftest file-position-set-input-string.3
  (with-input-from-string (input "abcdef")
    (values
      (file-position input 3)
      (read-char input)))
  t #\d)

(deftest file-position-set-output-string.1
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output :start)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "defg")

(deftest file-position-set-output-string.2
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output :end)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "abcdefg")

(deftest file-position-set-output-string.3
  (with-open-stream (output (make-string-output-stream))
    (format output "abc")
    (values
      (file-position output 2)
      (progn
        (format output "defg")
        (get-output-stream-string output))))
  t "abdefg")

(deftest file-position-set-extend-string.1
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream :start)
      (progn
        (format stream "defg")
        array)))
  t "defg")

(deftest file-position-set-extend-string.2
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream :end)
      (progn
        (format stream "defg")
        array)))
  t "abcdefg")

(deftest file-position-set-extend-string.3
  (with-extend-to-string
    (stream array)
    (format stream "abc")
    (values
      (file-position stream 2)
      (progn
        (format stream "defg")
        array)))
  t "abdefg")


;;
;;  file-string-length
;;
(deftest file-string-length-file.1
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (file-string-length stream #\a)))
  1)

(deftest file-string-length-file.2
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u7F)))
  1)

(deftest file-string-length-file.3
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u80)))
  2)

(deftest file-string-length-file.4
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream #\u0800)))
  3)

(deftest file-string-length-file.5
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream #\uFFFF)))
  2)

(deftest file-string-length-file.6
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream #\u010000)))
  4)

(deftest file-string-length-file.7
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-32)
      (file-string-length stream #\A)))
  4)

(deftest file-string-length-file.8
  (with-temp-file
    (with-overwrite-file (stream *file*)
      (file-string-length stream "abcd")))
  4)

(deftest file-string-length-file.9
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-8)
      (file-string-length stream (format nil "a~A~Ac" #\u80 #\u0811))))
  7)

(deftest file-string-length-file.10
  (with-temp-file
    (with-overwrite-file (stream *file* :external-format 'utf-16)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  10)

(deftest file-string-length-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (values
      (file-string-length stream #\a)
      (file-string-length stream "abcd")))
  1 1)

(deftest file-string-length-broadcast.2
  (with-temp-file1-file2
    (with-overwrite-file (output1 *file1* :external-format 'utf-8)
      (with-overwrite-file (output2 *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-broadcast-stream output1 output2))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)

(deftest-error file-string-length-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (values
      (file-string-length stream #\a)
      (file-string-length stream "abcd"))))

(deftest file-string-length-echo.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :external-format 'utf-8)
      (with-overwrite-file (output *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-echo-stream input output))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)

(deftest file-string-length-synonym.1
  (with-temp-file
    (with-overwrite-file (hello *file* :external-format 'utf-32)
      (declare (special hello))
      (with-open-stream (stream (make-synonym-stream 'hello))
        (values
          (file-string-length stream #\a)
          (file-string-length stream "abcd")))))
  4 16)

(deftest file-string-length-two-way.1
  (with-temp-file1-file2
    (with-open-file (input *file1* :external-format 'utf-8)
      (with-overwrite-file (output *file2* :external-format 'utf-32)
        (with-open-stream (stream (make-two-way-stream input output))
          (values
            (file-string-length stream #\a)
            (file-string-length stream "abcd"))))))
  4 16)

(deftest-error file-string-length-input-string.1
  (with-input-from-string (stream "Hello")
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111)))))

(deftest file-string-length-output-string.1
  (with-open-stream (stream (make-string-output-stream))
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  1 4)

(deftest file-string-length-extend-string.1
  (with-extend-to-string
    (stream array)
    (values
      (file-string-length stream #\u10000)
      (file-string-length stream (format nil "ab~Ad" #\u011111))))
  1 4)

(deftest stream-external-format.1
  (with-temp-file
    (with-open-file (input *file*)
      (stream-external-format input)))
  lisp-system::utf-8)

(deftest stream-external-format.2
  (with-temp-file
    (with-overwrite-file (input *file* :external-format 'ascii)
      (stream-external-format input)))
  lisp-system::ascii)

(deftest stream-external-format.3
  (with-temp-file
    (with-open-file (input *file* :direction :io
                           :if-exists :overwrite
                           :element-type 'unsigned-byte)
      (stream-external-format input)))
  (unsigned-byte 8))

