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
;;  Function READ-CHAR-NO-HANG
;;
(deftest read-char-no-hang.1
  (dotimes (i 100 t)
    (unless (read-char-no-hang *standard-input*)
      (return nil)))
  nil)

(deftest read-char-no-hang.2
  (with-input-from-string (*standard-input* "ABC")
    (read-char-no-hang))
  #\A)

(deftest-error read-char-no-hang.3
  (with-input-from-string (*standard-input* "")
    (read-char-no-hang))
  end-of-file)

(deftest-error read-char-no-hang.4
  (with-input-from-string (stream "")
    (read-char-no-hang stream t nil))
  end-of-file)

(deftest-error read-char-no-hang.5
  (with-input-from-string (stream "")
    (read-char-no-hang stream t nil t))
  end-of-file)

(deftest read-char-no-hang.6
  (with-input-from-string (stream "")
    (read-char-no-hang stream nil :hello t))
  :hello)

(deftest-error read-char-no-hang.7
  (let ((stream (make-string-input-stream "Hello")))
    (close stream)
    (read-char-no-hang stream)))

(deftest read-char-no-hang.8
  (with-input-from-string (stream "ABC")
    (read-char-no-hang stream)
    (unread-char #\Z stream)
    (read-char-no-hang stream))
  #\Z)

(deftest-error read-char-no-hang-error.1
  (eval '(read-char-no-hang 10))
  type-error)

(deftest-error! read-char-no-hang-error.2
  (eval '(read-char-no-hang *standard-input* nil nil nil nil)))


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

(deftest terpri.2
  (map 'list
       #'char-code
       (with-output-to-string (*standard-output*)
         (terpri)))
  (10))

(deftest terpri.3
  (with-open-stream (stream (make-broadcast-stream))
    (terpri stream))
  nil)

(deftest terpri.4
  (map 'list
       #'char-code
       (with-output-to-string (stream)
         (terpri stream)
         (fresh-line stream)
         (fresh-line stream)
         (fresh-line stream)
         (terpri stream)
         (terpri stream)))
  (10 10 10))

(deftest-error terpri-error.1
  (eval '(terpri 10))
  type-error)

(deftest-error terpri-error.2
  (eval '(terpri *standard-input*))
  type-error)

(deftest-error! terpri-error.3
  (eval '(terpri *standard-output* nil)))

;;  ANSI Common Lisp
(deftest terpri-test.1
  (equal
    (with-output-to-string (s)
      (write-string "some text" s)
      (terpri s)
      (terpri s)
      (write-string "more text" s))
    (concatenate 'string "some text" '(#\newline #\newline) "more text"))
  t)


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

(deftest fresh-line.1
  (map 'list
       #'char-code
       (with-output-to-string (stream)
         (fresh-line stream)
         (fresh-line stream)
         (fresh-line stream)))
  nil)

(deftest fresh-line.2
  (map 'list
       #'char-code
       (with-output-to-string (*standard-output*)
         (write-char #\A)
         (fresh-line)
         (fresh-line)))
  (65 10))

(deftest fresh-line.3
  (with-open-stream (stream (make-broadcast-stream))
    (fresh-line stream))
  nil)

(deftest-error fresh-line-error.1
  (eval '(fresh-line 10))
  type-error)

(deftest-error fresh-line-error.2
  (eval '(fresh-line *standard-input*))
  type-error)

(deftest-error! fresh-line-error.3
  (eval '(fresh-line *standard-output* nil)))

(deftest fresh-line-test.1
  (equal
    (with-output-to-string (s)
      (write-string "some text" s)
      (fresh-line s)
      (fresh-line s)
      (write-string "more text" s))
    (concatenate 'string "some text" '(#\newline) "more text"))
  t)


;;
;;  Function UNREAD-CHAR
;;
(deftest unread-char.1
  (with-input-from-string (stream "ABCD")
    (read-char stream)  ;; A
    (read-char stream)  ;; B
    (read-char stream)  ;; C
    (unread-char #\Z stream)
    (values
      (read-char stream nil :eof)    ;; Z
      (read-char stream nil :eof)    ;; D
      (read-char stream nil :eof)))  ;; EOF
  #\Z #\D :eof)

(deftest unread-char.2
  (with-input-from-string (*standard-input* "ABCD")
    (read-char)
    (read-char)
    (unread-char #\Z)
    (read-char))
  #\Z)

(deftest unread-char.3
  (with-input-from-string (stream "ABCD")
    (read-char stream)
    (unread-char #\Z stream))
  nil)

(deftest-error unread-char.4
  (with-input-from-string (stream "ABCD")
    (read-char stream)
    (unread-char #\Z stream)
    (unread-char #\Z stream)))

(deftest-error unread-char-error.1
  (eval '(unread-char 10))
  type-error)

(deftest-error unread-char-error.2
  (eval '(unread-char #\a 20))
  type-error)

(deftest-error unread-char-error.3
  (eval '(unread-char #\a *standard-output*))
  type-error)

(deftest-error! unread-char-error.4
  (eval '(unread-char)))

(deftest-error! unread-char-error.5
  (eval '(unread-char #\A *standard-input* nil)))

;;  ANSI Common Lisp
(deftest unread-char-test.1
  (let (list)
    (with-input-from-string (is "0123")
      (dotimes (i 6)
        (let ((c (read-char is)))
          (if (evenp i)
            (push (list i c) list)
            (unread-char c is)))))
    (nreverse list))
  ((0 #\0) (2 #\1) (4 #\2)))


;;
;;  Function WRITE-CHAR
;;
(deftest write-char.1
  (with-output-to-string (*standard-output*)
    (write-char #\A))
  "A")

(deftest write-char.2
  (with-output-to-string (stream)
    (write-char #\A stream)
    (write-char #\B stream)
    (write-char #\C stream))
  "ABC")

(deftest-error write-char-error.1
  (eval '(write-char 10))
  type-error)

(deftest-error write-char-error.2
  (eval '(write-char #\A 20))
  type-error)

(deftest-error write-char-error.3
  (eval '(write-char #\A *standard-input*))
  type-error)

(deftest-error! write-char-error.4
  (eval '(write-char)))

(deftest-error! write-char-error.5
  (eval '(write-char #\Z *standard-output* nil)))

;;  ANSI Common Lisp
(deftest write-char-test.1
  (with-output-to-string (*standard-output*)
    (write-char #\a))
  "a")

(deftest write-char-test.2
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (write-char #\a))
  #\a)

(deftest write-char-test.3
  (with-output-to-string (s)
    (write-char #\a s)
    (write-char #\Space s)
    (write-char #\b s))
  "a b")


;;
;;  Function READ-LINE
;;
(deftest read-line.1
  (with-input-from-string (*standard-input* "Hello")
    (read-line))
  "Hello" t)

(deftest read-line.2
  (with-input-from-string (stream (format nil "Hello~%"))
    (read-line stream))
  "Hello" nil)

(deftest-error read-line.3
  (with-input-from-string (*standard-input* "Hello")
    (read-line)
    (read-line))
  end-of-file)

(deftest read-line.4
  (with-input-from-string (stream (format nil "Hello~%"))
    (read-line stream nil)
    (read-line stream nil))
  nil t)

(deftest read-line.5
  (with-input-from-string (stream (format nil "Hello~%"))
    (read-line stream nil :eof)
    (read-line stream nil :eof))
  :eof t)

(deftest-error read-line.6
  (with-input-from-string (stream "Hello")
    (read-line stream t nil t)
    (read-line stream t nil t))
  end-of-file)

(deftest read-line.7
  (with-input-from-string (stream (format nil "Hello~%"))
    (read-line stream nil :eof t)
    (read-line stream nil :eof t))
  :eof t)

(deftest-error read-line-error.1
  (eval '(read-line 10))
  type-error)

(deftest-error! read-line-error.2
  (eval '(read-line *standard-input* t nil t nil)))

(deftest-error read-line-error.3
  (with-temp-file
    (eval '(with-open-file (stream *file* :direction :input
                                   :element-type 'unsigned-byte)
             (read-line stream))))
  type-error)

;;  ANSI Common Lisp
(defvar *read-line-test*)

(deftest read-line-test.1
  (read-line
    (setq *read-line-test*
          (make-string-input-stream
            (concatenate 'string "line 1" '(#\newline) "line2"))))
  "line 1" nil)

(deftest read-line-test.2
  (read-line *read-line-test*)
  "line2" t)

(deftest read-line-test.3
  (read-line *read-line-test* nil nil)
  nil t)

;;  EOL mode
(deftest read-line-auto.1
  ;;  default value
  lisp-system::*end-of-line*
  lisp-system::auto)

(deftest read-line-auto.2
  (let ((lisp-system::*end-of-line* 'lisp-system::auto))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0A))
      (values
        (read-line stream)
        (read-line stream))))
  "ABC" "DEF")

(deftest read-line-auto.3
  (let ((lisp-system::*end-of-line* 'lisp-system::auto))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0D))
      (values
        (read-line stream)
        (read-line stream))))
  "ABC" "DEF")

(deftest read-line-auto.4
  (let ((lisp-system::*end-of-line* 'lisp-system::auto))
    (with-input-from-string
      (stream (format nil "ABC~A~ADEF" #\u0D #\u0A))
      (values
        (read-line stream)
        (read-line stream))))
  "ABC" "DEF")

(deftest read-line-auto.5
  (let ((lisp-system::*end-of-line* 'lisp-system::auto))
    (with-input-from-string
      (stream (format nil "ABC~A~ADEF" #\u0A #\u0D))
      (values
        (read-line stream)
        (read-line stream)
        (read-line stream))))
  "ABC" "" "DEF")

(deftest read-line-cr.1
  (let ((lisp-system::*end-of-line* 'lisp-system::cr))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0D))
      (values
        (read-line stream)
        (read-line stream))))
  "ABC" "DEF")

(deftest read-line-cr.2
  (let ((lisp-system::*end-of-line* 'lisp-system::cr))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0A))
      (equal (read-line stream)
             (format nil "ABC~ADEF" #\u0A))))
  t)

(deftest read-line-lf.1
  (let ((lisp-system::*end-of-line* 'lisp-system::lf))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0A))
      (values
        (read-line stream)
        (read-line stream))))
  "ABC" "DEF")

(deftest read-line-lf.2
  (let ((lisp-system::*end-of-line* 'lisp-system::lf))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0D))
      (equal (read-line stream)
             (format nil "ABC~ADEF" #\u0D))))
  t)

(deftest read-line-crlf.1
  (let ((lisp-system::*end-of-line* 'lisp-system::crlf))
    (with-input-from-string
      (stream (format nil "ABC~A~ADEF" #\u0D #\u0A))
      (values
        (read-line stream)
        (read-line stream))))
  "ABC" "DEF")

(deftest-error read-line-crlf.2
  (let ((lisp-system::*end-of-line* 'lisp-system::crlf))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0A))
      (read-line stream))))

(deftest-error read-line-crlf.3
  (let ((lisp-system::*end-of-line* 'lisp-system::crlf))
    (with-input-from-string
      (stream (format nil "ABC~ADEF" #\u0D))
      (read-line stream))))


;;
;;  Function WRITE-STRING
;;
(deftest write-string.1
  (with-output-to-string (*standard-output*)
    (write-string "aaa"))
  "aaa")

(deftest write-string.2
  (with-open-stream (stream (make-string-output-stream))
    (write-string "aaa" stream))
  "aaa")

(deftest write-string.3
  (with-output-to-string (stream)
    (write-string "aaa" stream)
    (write-string "bbb" stream))
  "aaabbb")

(deftest write-string-range.1
  (with-output-to-string (stream)
    (write-string "abc" stream :start 0))
  "abc")

(deftest write-string-range.2
  (with-output-to-string (stream)
    (write-string "abc" stream :start 1))
  "bc")

(deftest write-string-range.3
  (with-open-stream (stream (make-string-output-stream))
    (write-string "abc" stream :start 1))
  "abc")

(deftest write-string-range.4
  (with-output-to-string (stream)
    (write-string "abc" stream :start 3))
  "")

(deftest write-string-range.5
  (with-output-to-string (stream)
    (write-string "abc" stream :end 0))
  "")

(deftest write-string-range.6
  (with-output-to-string (stream)
    (write-string "abc" stream :end 1))
  "a")

(deftest write-string-range.7
  (with-output-to-string (stream)
    (write-string "abc" stream :end 3))
  "abc")

(deftest write-string-range.8
  (with-output-to-string (stream)
    (write-string "abc" stream :end nil))
  "abc")

(deftest write-string-range.9
  (with-output-to-string (stream)
    (write-string "abcdef" stream :start 2)
    (write-string "bbb" stream))
  "cdefbbb")

(deftest write-string-range.10
  (with-output-to-string (stream)
    (write-string "abcdef" stream :end 2)
    (write-string "bbb" stream))
  "abbbb")

(deftest write-string-range.11
  (with-output-to-string (stream)
    (write-string "abcdef" stream :start 3 :end 5)
    (write-string "bbb" stream))
  "debbb")

(deftest-error write-string-error.1
  (eval '(write-string 10))
  type-error)

(deftest-error write-string-error.2
  (eval '(write-string "Hello" 20))
  type-error)

(deftest-error write-string-error.3
  (eval '(write-string "Hello" *standard-output* :start)))

(deftest-error write-string-error.4
  (eval '(write-string "Hello" *standard-output* :start :hello)))

(deftest-error write-string-error.5
  (eval '(write-string "Hello" *standard-output* :hello 10)))

(deftest-error! write-string-error.6
  (eval '(write-string)))

(deftest-error! write-string-error.7
  (eval '(write-string "Hello" *standard-output* :start -1)))

(deftest-error! write-string-error.8
  (eval '(write-string "Hello" *standard-output* :start 6)))

(deftest-error! write-string-error.9
  (eval '(write-string "Hello" *standard-output* :end -1)))

(deftest-error! write-string-error.10
  (eval '(write-string "Hello" *standard-output* :end 6)))

(deftest-error! write-string-error.11
  (eval '(write-string "Hello" *standard-output* :start 4 :end 3)))


;;
;;  write-line
;;
(deftest write-line.1
  (equal (with-output-to-string (*standard-output*)
           (write-line "aaa"))
         (format nil "aaa~%"))
  t)

(deftest write-line.2
  (with-open-stream (stream (make-string-output-stream))
    (write-line "aaa" stream))
  "aaa")

(deftest write-line.3
  (equal (with-output-to-string (stream)
           (write-line "aaa" stream)
           (write-line "bbb" stream))
         (format nil "aaa~%bbb~%"))
  t)

(deftest write-line-range.1
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :start 0))
         (format nil "abc~%"))
  t)

(deftest write-line-range.2
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :start 1))
         (format nil "bc~%"))
  t)

(deftest write-line-range.3
  (with-open-stream (stream (make-string-output-stream))
    (write-line "abc" stream :start 1))
  "abc")

(deftest write-line-range.4
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :start 3))
         (format nil "~%"))
  t)

(deftest write-line-range.5
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :end 0))
         (format nil "~%"))
  t)

(deftest write-line-range.6
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :end 1))
         (format nil "a~%"))
  t)

(deftest write-line-range.7
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :end 3))
         (format nil "abc~%"))
  t)

(deftest write-line-range.8
  (equal (with-output-to-string (stream)
           (write-line "abc" stream :end nil))
         (format nil "abc~%"))
  t)

(deftest write-line-range.9
  (equal (format nil "cdef~%bbb~%")
         (with-output-to-string (stream)
           (write-line "abcdef" stream :start 2)
           (write-line "bbb" stream)))
  t)

(deftest write-line-range.10
  (equal (format nil "ab~%bbb~%")
         (with-output-to-string (stream)
           (write-line "abcdef" stream :end 2)
           (write-line "bbb" stream)))
  t)

(deftest write-line-range.11
  (equal (format nil "de~%bbb~%")
         (with-output-to-string (stream)
           (write-line "abcdef" stream :start 3 :end 5)
           (write-line "bbb" stream)))
  t)

(deftest-error write-line-error.1
  (eval '(write-line 10))
  type-error)

(deftest-error write-line-error.2
  (eval '(write-line "Hello" 20))
  type-error)

(deftest-error write-line-error.3
  (eval '(write-line "Hello" *standard-output* :start)))

(deftest-error write-line-error.4
  (eval '(write-line "Hello" *standard-output* :start :hello)))

(deftest-error write-line-error.5
  (eval '(write-line "Hello" *standard-output* :hello 10)))

(deftest-error! write-line-error.6
  (eval '(write-line)))

(deftest-error! write-line-error.7
  (eval '(write-line "Hello" *standard-output* :start -1)))

(deftest-error! write-line-error.8
  (eval '(write-line "Hello" *standard-output* :start 6)))

(deftest-error! write-line-error.9
  (eval '(write-line "Hello" *standard-output* :end -1)))

(deftest-error! write-line-error.10
  (eval '(write-line "Hello" *standard-output* :end 6)))

(deftest-error! write-line-error.11
  (eval '(write-line "Hello" *standard-output* :start 4 :end 3)))

;;  ANSI Common Lisp
(deftest write-string-test.1
  (with-output-to-string (*standard-output*)
    (prog1 (write-string "books" nil :end 4)
      (write-string "worms")))
  "bookworms")

(deftest write-string-test.2
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (prog1 (write-string "books" nil :end 4)
      (write-string "worms")))
  "books")

(deftest write-string-test.3
  (equal (with-output-to-string (*standard-output*)
           (progn (write-char #\*)
                  (write-line "test12" *standard-output* :end 5)
                  (write-line "*test2")
                  (write-char #\*)
                  nil))
         (format nil "*test1~%*test2~%*"))
  t)

