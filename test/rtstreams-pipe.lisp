;;
;;  ANSI COMMON LISP: 21. Streams
;;
(deftest pipe-stream.1
  (typep
    (lisp-system:sysctl 'stream 'pipe 'make 0)
    'stream)
  t)

(deftest pipe-stream.2
  (typep
    (lisp-system:sysctl 'stream 'pipe 'make 0)
    'lisp-system::pipe-stream)
  t)

(deftest pipe-stream.3
  (subtypep 'lisp-system::pipe-stream 'stream)
  t t)

(deftest pipe-stream.4
  (subtypep 'stream 'lisp-system::pipe-stream)
  nil t)

(deftest pipe-stream.5
  (subtypep 'string 'lisp-system::pipe-stream)
  nil t)

(deftest pipe-stream.6
  (let ((x (lisp-system:sysctl 'stream 'pipe 'make 1)))
    (type-of x))
  lisp-system::pipe-stream)

(deftest pipe-stream.7
  (lisp-system:sysctl 'stream 'pipe 'make 100000000)
  nil nil)

(deftest pipe-stream-type.1
  (let ((x (lisp-system:sysctl 'stream 'pipe 'make 0)))
    (lisp-system:sysctl 'stream 'pipe 'type x))
  0 t)

(deftest pipe-stream-type.2
  (let ((x (lisp-system:sysctl 'stream 'pipe 'make 1)))
    (lisp-system:sysctl 'stream 'pipe 'type x))
  1 t)

(deftest pipe-stream-type.3
  (let ((x (lisp-system:sysctl 'stream 'pipe 'make 0)))
    (lisp-system:sysctl 'stream 'pipe 'type x 2))
  2 t)

(deftest pipe-stream-type.4
  (let ((x (lisp-system:sysctl 'stream 'pipe 'make 0)))
    (lisp-system:sysctl 'stream 'pipe 'type x 2)
    (lisp-system:sysctl 'stream 'pipe 'type x))
  2 t)

