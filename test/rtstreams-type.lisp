;;
;;  ANSI COMMON LISP: 21. Streams
;;

;;
;;  System Class STREAM
;;
(deftest stream-type.1
  (lisp-system:closp
    (find-class 'stream))
  t)

(deftest stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'stream)))
  (stream t))

(deftest stream-type.3
  (typep *standard-input* 'stream)
  t)

(deftest stream-type.4
  (typep (make-broadcast-stream) 'stream)
  t)

(deftest stream-type.5
  (typep 'hello 'stream)
  nil)


;;
;;  System Class BROADCAST-STREAM
;;
(deftest broadcast-stream-type.1
  (lisp-system:closp
    (find-class 'broadcast-stream))
  t)

(deftest broadcast-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'broadcast-stream)))
  (broadcast-stream stream t))

(deftest broadcast-stream-type.3
  (typep (make-concatenated-stream) 'broadcast-stream)
  nil)

(deftest broadcast-stream-type.4
  (typep (make-broadcast-stream) 'broadcast-stream)
  t)

(deftest broadcast-stream-type.5
  (typep (make-broadcast-stream) 'stream)
  t)

(deftest broadcast-stream-type.6
  (typep 'hello 'broadcast-stream)
  nil)


;;
;;  System Class CONCATENATED-STREAM
;;
(deftest concatenated-stream-type.1
  (lisp-system:closp
    (find-class 'concatenated-stream))
  t)

(deftest concatenated-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'concatenated-stream)))
  (concatenated-stream stream t))

(deftest concatenated-stream-type.3
  (typep (make-echo-stream *standard-input* *standard-output*) 'concatenated-stream)
  nil)

(deftest concatenated-stream-type.4
  (typep (make-concatenated-stream) 'concatenated-stream)
  t)

(deftest concatenated-stream-type.5
  (typep (make-concatenated-stream) 'stream)
  t)

(deftest concatenated-stream-type.6
  (typep 'hello 'concatenated-stream)
  nil)


;;
;;  System Class ECHO-STREAM
;;
(deftest echo-stream-type.1
  (lisp-system:closp
    (find-class 'echo-stream))
  t)

(deftest echo-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'echo-stream)))
  (echo-stream stream t))

(deftest echo-stream-type.3
  (with-overwrite-and-delete (stream *file*)
    (typep stream 'echo-stream))
  nil)

(deftest echo-stream-type.4
  (typep (make-echo-stream *standard-input* *standard-output*) 'echo-stream)
  t)

(deftest echo-stream-type.5
  (typep (make-echo-stream *standard-input* *standard-output*) 'stream)
  t)

(deftest echo-stream-type.6
  (typep 'hello 'echo-stream)
  nil)


;;
;;  System Class FILE-STREAM
;;
(deftest file-stream-type.1
  (lisp-system:closp
    (find-class 'file-stream))
  t)

(deftest file-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'file-stream)))
  (file-stream stream t))

(deftest file-stream-type.3
  (typep (make-string-input-stream "Hello") 'file-stream)
  nil)

(deftest file-stream-type.4
  (with-overwrite-and-delete (stream *file*)
    (typep stream 'file-stream))
  t)

(deftest file-stream-type.5
  (with-overwrite-and-delete (stream *file*)
    (typep stream 'stream))
  t)

(deftest file-stream-type.6
  (typep 'hello 'file-stream)
  nil)


;;
;;  System Class STRING-STREAM
;;
(deftest string-stream-type.1
  (lisp-system:closp
    (find-class 'string-stream))
  t)

(deftest string-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'string-stream)))
  (string-stream stream t))

(deftest string-stream-type.3
  (typep (make-synonym-stream '*standard-input*) 'string-stream)
  nil)

(deftest string-stream-type.4
  (typep (make-string-input-stream "Hello") 'string-stream)
  t)

(deftest string-stream-type.5
  (typep (make-string-input-stream "Hello") 'stream)
  t)

(deftest string-stream-type.6
  (typep (make-string-output-stream) 'string-stream)
  t)

(deftest string-stream-type.7
  (typep (make-string-output-stream) 'stream)
  t)

(deftest string-stream-type.8
  (typep 'hello 'string-stream)
  nil)


;;
;;  System Class SYNONYM-STREAM
;;
(deftest synonym-stream-type.1
  (lisp-system:closp
    (find-class 'synonym-stream))
  t)

(deftest synonym-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'synonym-stream)))
  (synonym-stream stream t))

(deftest synonym-stream-type.3
  (typep (make-two-way-stream *standard-input* *standard-output*) 'synonym-stream)
  nil)

(deftest synonym-stream-type.4
  (typep (make-synonym-stream '*standard-output*) 'synonym-stream)
  t)

(deftest synonym-stream-type.5
  (typep (make-synonym-stream '*standard-output*) 'stream)
  t)

(deftest synonym-stream-type.6
  (typep 'hello 'synonym-stream)
  nil)


;;
;;  System Class TWO-WAY-STREAM
;;
(deftest two-way-stream-type.1
  (lisp-system:closp
    (find-class 'two-way-stream))
  t)

(deftest two-way-stream-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'two-way-stream)))
  (two-way-stream stream t))

(deftest two-way-stream-type.3
  (typep (make-broadcast-stream) 'two-way-stream)
  nil)

(deftest two-way-stream-type.4
  (typep (make-two-way-stream *standard-input* *standard-output*) 'two-way-stream)
  t)

(deftest two-way-stream-type.5
  (typep (make-two-way-stream *standard-input* *standard-output*) 'stream)
  t)

(deftest two-way-stream-type.6
  (typep 'hello 'two-way-stream)
  nil)


;;
;;  Function STREAMP
;;
(deftest streamp.1
  (streamp *standard-input*)
  t)

(deftest streamp.2
  (streamp *terminal-io*)
  t)

(deftest streamp.3
  (streamp 1)
  nil)

(deftest streamp.4
  (let ((x (make-broadcast-stream)))
    (close x)
    (streamp x))
  t)

(deftest streamp-file.1
  (with-temp-file
    (with-open-file (stream *file* :direction :input)
      (streamp stream)))
  t)

(deftest streamp-file.2
  (with-temp-file
    (with-open-file (stream *file* :direction :output :if-exists :supersede)
      (streamp stream)))
  t)

(deftest streamp-file.3
  (with-temp-file
    (with-open-file (stream *file* :direction :io :if-exists :supersede)
      (streamp stream)))
  t)

(deftest streamp-file.4
  (with-temp-file
    (let ((stream (open *file* :direction :input)))
      (close stream)
      (streamp stream)))
  t)

(deftest streamp-broadcast.1
  (with-open-stream (stream (make-broadcast-stream))
    (streamp stream))
  t)

(deftest streamp-concatenated.1
  (with-open-stream (stream (make-concatenated-stream))
    (streamp stream))
  t)

(deftest streamp-echo.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (streamp stream))
  t)

(deftest streamp-synonym.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (streamp stream))
  t)

(deftest streamp-two-way.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (streamp stream))
  t)
(deftest streamp-input-string.1
  (with-input-from-string (stream "Hello")
    (streamp stream))
  t)

(deftest streamp-output-string.1
  (let ((result 'error))
    (with-output-to-string (stream)
      (setq result (streamp stream)))
    result)
  t)

(deftest streamp-extend-string.1
  (with-extend-to-string
    (inst array)
    (streamp inst))
  t)

(deftest-error! streamp-error.1
  (eval '(streamp)))

(deftest-error! streamp-error.2
  (eval '(streamp nil nil)))


;;
;;  Condition Type STREAM-ERROR
;;
(deftest stream-error-type.1
  (lisp-system:closp
    (find-class 'stream-error))
  t)

(deftest stream-error-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'stream-error)))
  (stream-error error serious-condition condition standard-object t))


;;
;;  Condition Type END-OF-FILE
;;
(deftest end-of-file-type.1
  (lisp-system:closp
    (find-class 'end-of-file))
  t)

(deftest end-of-file-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'end-of-file)))
  (end-of-file stream-error error serious-condition condition standard-object t))


;;
;;  Function STREAM-ERROR-STREAM
;;
(deftest stream-error-stream.1
  (handler-case
    (with-input-from-string (stream "")
      (read stream)
      :aaa)
    (stream-error (c) (typep (stream-error-stream c) 'string-stream)))
  t)

(deftest stream-error-stream.2
  (handler-case
    (with-input-from-string (stream "")
      (read stream)
      :aaa)
    (end-of-file (c) (typep (stream-error-stream c) 'string-stream)))
  t)

(deftest stream-error-stream.3
  (with-input-from-string (s "(FOO")
    (handler-case (read s)
      (end-of-file (c) (format nil "~&End of file on ~A."
                               (subseq (princ-to-string
                                         (stream-error-stream c)) 0 2)))))
  "End of file on #<.")

(deftest-error stream-error-stream-error.1
  (eval '(stream-error-stream 10))
  type-error)

(deftest-error! stream-error-stream-error.2
  (eval '(stream-error-stream)))

(deftest-error! stream-error-stream-error.3
  (eval '(stream-error-stream
           (make-condition 'stream-error :stream nil)
           nil)))

