;;
;;  typep / subtypep
;;

;;
;;  nil
;;
(deftest typep-nil.1
  (typep nil nil)
  nil)

(deftest typep-nil.2
  (typep t nil)
  nil)


;;
;;  t
;;
(deftest typep-t.1
  (typep nil t)
  t)

(deftest typep-t.2
  (typep t t)
  t)


;;
;;  null
;;
(deftest typep-null.1
  (typep nil 'null)
  t)

(deftest typep-null.2
  (typep :hello 'null)
  nil)


;;
;;  cons
;;
(deftest typep-cons.1
  (typep '(10 20 30) 'cons)
  t)

(deftest typep-cons.2
  (typep nil 'cons)
  nil)

(deftest typep-cons.3
  (typep (list 10) '(cons))
  t)

(deftest typep-cons.4
  (typep (list nil) '(cons *))
  t)

(deftest typep-cons.5
  (typep (cons 10 20) '(cons * *))
  t)

(deftest typep-cons.6
  (typep (cons 10 20) '(cons integer real))
  t)

(deftest typep-cons.7
  (typep (cons 10 20) '(cons string real))
  nil)

(deftest typep-cons.8
  (typep (cons 10 20) '(cons * string))
  nil)

(deftest typep-cons.9
  (typep #*1011 'cons)
  nil)


;;
;;  hash-table
;;
(deftest typep-hash-table.1
  (typep (make-hash-table) 'hash-table)
  t)

(deftest typep-hash-table.2
  (typep 10 'hash-table)
  nil)


;;
;;  symbol
;;
(deftest type-symbol.1
  (typep 'hello 'symbol)
  t)

(deftest type-symbol.2
  (typep :hello 'symbol)
  t)

(deftest type-symbol.3
  (typep nil 'symbol)
  t)

(deftest type-symbol.4
  (typep t 'symbol)
  t)

(deftest type-symbol.5
  (typep (gensym) 'symbol)
  t)

(deftest type-symbol.6
  (typep "Hello" 'symbol)
  nil)


;;
;;  keyword
;;
(deftest type-keyword.1
  (typep 'hello 'keyword)
  nil)

(deftest type-keyword.2
  (typep :hello 'keyword)
  t)

(deftest type-keyword.3
  (typep nil 'keyword)
  nil)

(deftest type-keyword.4
  (typep t 'keyword)
  nil)

(deftest type-keyword.5
  (typep (gensym) 'keyword)
  nil)

(deftest type-keyword.6
  (typep "Hello" 'keyword)
  nil)


;;
;;  package
;;
(deftest typep-package.1
  (typep *package* 'package)
  t)

(deftest typep-package.2
  (typep '*package* 'package)
  nil)


;;
;;  random-state
;;
(deftest typep-random-state.1
  (typep *random-state* 'random-state)
  t)

(deftest typep-random-state.2
  (typep '*random-state* 'random-state)
  nil)

(deftest typep-random-state.3
  (typep (make-random-state t) 'random-state)
  t)


;;
;;  readtable
;;
(deftest typep-readtable.1
  (typep *readtable* 'readtable)
  t)

(deftest typep-readtable.2
  (typep '*readtable* 'readtable)
  nil)


;;
;;  function
;;
(deftest typep-function.1
  (typep #'car 'function)
  t)

(deftest typep-function.2
  (typep (lambda () :hello) 'function)
  t)

(deftest-error typep-function.3
  (typep #'car '(function)))

(deftest typep-function.4
  (typep 10 'function)
  nil)


;;
;;  compiled-function
;;
(deftest typep-compiled-function.1
  (typep #'car 'compiled-function)
  t)

(deftest typep-compiled-function.2
  (typep (lambda () :hello) 'compiled-function)
  nil)

(deftest-error typep-compiled-function.3
  (typep #'car '(compiled-function)))

(deftest typep-compiled-function.4
  (typep 10 'compiled-function)
  nil)


;;
;;  pathname
;;
(setf (logical-pathname-translations "typep")
      '(("path;*.*.*" "/path/to/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))

(deftest typep-pathname.1
  (typep #p"/usr/" 'pathname)
  t)

(deftest typep-pathname.2
  (typep #p"typep:path;to;" 'pathname)
  t)

(deftest typep-pathname.3
  (typep 11 'pathname)
  nil)


;;
;;  logical-pathname
;;
(deftest typep-logical-pathname.1
  (typep #p"/usr/" 'logical-pathname)
  nil)

(deftest typep-logical-pathname.2
  (typep #p"typep:path;to;" 'logical-pathname)
  t)

(deftest typep-logical-pathname.3
  (typep 11 'logical-pathname)
  nil)


;;
;;  character
;;
(deftest typep-character.1
  (typep #\A 'character)
  t)

(deftest typep-character.2
  (typep #\u3033 'character)
  t)

(deftest typep-character.3
  (typep (lisp-system:make-character #x80000000) 'character)
  t)

(deftest typep-character.4
  (typep 100 'character)
  nil)


;;
;;  base-char
;;
(deftest typep-base-char.1
  (typep #\A 'base-char)
  t)

(deftest typep-base-char.2
  (typep #\u3033 'base-char)
  t)

(deftest typep-base-char.3
  (typep (lisp-system:make-character #x80000000) 'base-char)
  nil)

(deftest typep-base-char.4
  (typep 100 'base-char)
  nil)


;;
;;  standard-char
;;
(deftest typep-standard-char.1
  (typep #\A 'standard-char)
  t)

(deftest typep-standard-char.2
  (typep #\u3033 'standard-char)
  nil)

(deftest typep-standard-char.3
  (typep (lisp-system:make-character #x80000000) 'standard-char)
  nil)

(deftest typep-standard-char.4
  (typep 100 'standard-char)
  nil)


;;
;;  restart
;;
(deftest typep-restart.1
  (restart-bind ((continue #'(lambda () :hello)))
    (typep (car (compute-restarts)) 'restart))
  t)

(deftest typep-restart.2
  (typep 100 'restart)
  nil)


;;
;;  environment
;;
(defmacro typep-environment-test-1 (&environment env)
  (typep env 'lisp-system::environment))

(deftest typep-environment.1
  (eval '(typep-environment-test-1))
  t)

(defmacro typep-environment-test-2 ()
  (typep 100 'lisp-system::environment))

(deftest typep-environment.2
  (eval '(typep-environment-test-2))
  nil)


;;
;;  stream
;;
(deftest typep-stream.1
  (typep *standard-input* 'stream)
  t)

(deftest typep-stream.2
  (with-open-stream (stream (lisp-system:make-memory-output-stream))
    (typep stream 'stream))
  t)

(deftest typep-stream.3
  (typep 100 'stream)
  nil)


;;
;;  broadcast-stream
;;
(deftest typep-broadcast-stream.1
  (with-open-stream (stream (make-broadcast-stream))
    (typep stream 'broadcast-stream))
  t)

(deftest typep-broadcast-stream.2
  (with-open-stream (stream (make-broadcast-stream))
    (typep stream 'stream))
  t)

(deftest typep-broadcast-stream.3
  (with-open-stream (stream (make-concatenated-stream))
    (typep stream 'broadcast-stream))
  nil)

(deftest typep-broadcast-stream.4
  (typep 100 'broadcast-stream)
  nil)


;;
;;  concatenated-stream
;;
(deftest typep-concatenated-stream.1
  (with-open-stream (stream (make-concatenated-stream))
    (typep stream 'concatenated-stream))
  t)

(deftest typep-concatenated-stream.2
  (with-open-stream (stream (make-concatenated-stream))
    (typep stream 'stream))
  t)

(deftest typep-concatenated-stream.3
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (typep stream 'concatenated-stream))
  nil)

(deftest typep-concatenated-stream.4
  (typep 100 'concatenated-stream)
  nil)


;;
;;  echo-stream
;;
(deftest typep-echo-stream.1
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (typep stream 'echo-stream))
  t)

(deftest typep-echo-stream.2
  (with-open-stream (stream (make-echo-stream *standard-input* *standard-output*))
    (typep stream 'stream))
  t)

(deftest typep-echo-stream.3
  (with-open-stream (io (lisp-system:make-memory-io-stream))
    (with-open-file (stream io)
      (typep stream 'echo-stream)))
  nil)

(deftest typep-echo-stream.4
  (typep 100 'echo-stream)
  nil)


;;
;;  file-stream
;;
(deftest typep-file-stream.1
  (with-open-stream (io (lisp-system:make-memory-io-stream))
    (with-open-file (stream io :direction :input)
      (typep stream 'file-stream)))
  t)

(deftest typep-file-stream.2
  (with-open-stream (io (lisp-system:make-memory-io-stream))
    (with-open-file (stream io :direction :output)
      (typep stream 'file-stream)))
  t)

(deftest typep-file-stream.3
  (with-open-stream (io (lisp-system:make-memory-io-stream))
    (with-open-file (stream io :direction :io)
      (typep stream 'file-stream)))
  t)

(deftest typep-file-stream.4
  (with-open-stream (io (lisp-system:make-memory-io-stream))
    (with-open-file (stream io :direction :probe)
      (typep stream 'file-stream)))
  t)

(deftest typep-file-stream.5
  (with-open-stream (io (lisp-system:make-memory-io-stream))
    (with-open-file (stream io)
      (typep stream 'stream)))
  t)

(deftest typep-file-stream.6
  (with-open-stream (stream (make-string-output-stream))
    (typep stream 'file-stream))
  nil)

(deftest typep-file-stream.7
  (typep 100 'file-stream)
  nil)


;;
;;  string-stream
;;
(deftest typep-string-stream.1
  (with-open-stream (stream (make-string-input-stream "Hello"))
    (typep stream 'string-stream))
  t)

(deftest typep-string-stream.2
  (with-open-stream (stream (make-string-output-stream))
    (typep stream 'string-stream))
  t)

(deftest typep-string-stream.3
  (with-open-stream (stream (make-string-input-stream "Hello"))
    (typep stream 'stream))
  t)

(deftest typep-string-stream.4
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (typep stream 'string-stream))
  nil)

(deftest typep-string-stream.5
  (typep 100 'string-stream)
  nil)


;;
;;  synonym-stream
;;
(deftest typep-synonym-stream.1
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (typep stream 'synonym-stream))
  t)

(deftest typep-synonym-stream.2
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (typep stream 'stream))
  t)

(deftest typep-synonym-stream.3
  (with-open-stream (stream (make-synonym-stream '*standard-input*))
    (typep stream 'two-way-stream))
  nil)

(deftest typep-synonym-stream.4
  (typep 100 'two-way-stream)
  nil)


;;
;;  two-way-stream
;;
(deftest typep-two-way-stream.1
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (typep stream 'two-way-stream))
  t)

(deftest typep-two-way-stream.2
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (typep stream 'stream))
  t)

(deftest typep-two-way-stream.3
  (with-open-stream (stream (make-two-way-stream *standard-input* *standard-output*))
    (typep stream 'broadcast-stream))
  nil)

(deftest typep-two-way-stream.4
  (typep 100 'two-way-stream)
  nil)

