;;
;;  ANSI COMMON LISP: 21. Streams
;;
(load #p"test/rtstreams.lisp")
(load #p"test/rtstreams-type.lisp")
(load #p"test/rtstreams-test.lisp")
(load #p"test/rtstreams-binary.lisp")
(load #p"test/rtstreams-character.lisp")
(load #p"test/rtstreams-control.lisp")
(load #p"test/rtstreams-encode.lisp")
(load #p"test/rtstreams-memory.lisp")
(load #p"test/rtstreams-open.lisp")
(load #p"test/rtstreams-output.lisp")
(load #p"test/rtstreams-io.lisp")

(load #p"test/rtstreams-broadcast.lisp")
(load #p"test/rtstreams-concatenated.lisp")
(load #p"test/rtstreams-echo.lisp")
(load #p"test/rtstreams-file.lisp")
(load #p"test/rtstreams-string.lisp")
(load #p"test/rtstreams-synonym.lisp")
(load #p"test/rtstreams-two-way.lisp")


;;
;;  do-tests
;;
(do-tests :test t)

