;;
;;  System Call: paper
;;

;;
;;  make-paper
;;
(deftest make-paper.1
  (typep
    (lisp-system:make-paper 0 0)
    'lisp-system::paper)
  t)

(deftest make-paper.2
  (let ((x (lisp-system:make-paper 10 0)))
    (values
      (lisp-system:info-paper x 'length nil)
      (lisp-system:info-paper x 'length t)))
  10 0)

(deftest make-paper.3
  (let ((x (lisp-system:make-paper 0 20)))
    (values
      (lisp-system:info-paper x 'length nil)
      (lisp-system:info-paper x 'length t)))
  0 20)

(deftest make-paper.4
  (let ((x (lisp-system:make-paper 10 20)))
    (values
      (lisp-system:info-paper x 'length nil)
      (lisp-system:info-paper x 'length t)))
  10 20)

(deftest make-paper.5
  (let ((x (lisp-system:make-paper 10 #x00FFFF)))
    (values
      (lisp-system:info-paper x 'length nil)
      (lisp-system:info-paper x 'length t)))
  10 #xFFFF)

(deftest-error make-paper.6
  (lisp-system:make-paper 10 #x010000))

(deftest-error make-paper.7
  (lisp-system:make-paper #x010000 20))

(deftest make-paper.8
  (let ((x (lisp-system:make-paper 3 0)))
    (values
      (lisp-system:info-paper x 'list nil)
      (lisp-system:info-paper x 'list t)))
  (nil nil nil) ())

(deftest make-paper.9
  (let ((x (lisp-system:make-paper 0 4)))
    (values
      (length (lisp-system:info-paper x 'vector nil))
      (length (lisp-system:info-paper x 'vector t))))
  0 4)

(deftest make-paper-fill.1
  (let ((x (lisp-system:make-paper 2 3 :fill nil)))
    (values
      (length (lisp-system:info-paper x 'list nil))
      (length (lisp-system:info-paper x 'list t))))
  2 3)

(deftest make-paper-fill.2
  (let ((x (lisp-system:make-paper 2 3 :fill t)))
    (values
      (lisp-system:info-paper x 'list nil)
      (lisp-system:info-paper x 'list t)))
  (nil nil) (0 0 0))

(deftest make-paper-fill.3
  (let ((x (lisp-system:make-paper 2 3 :fill #xCC)))
    (values
      (lisp-system:info-paper x 'list nil)
      (lisp-system:info-paper x 'list t)))
  (nil nil) (#xCC #xCC #xCC))

(deftest-error make-paper-fill.4
  (eval '(lisp-system:make-paper 2 3 :fill -1)))

(deftest-error make-paper-fill.5
  (eval '(lisp-system:make-paper 2 3 :fill #x0100)))

(deftest make-paper-type.1
  (let ((x (lisp-system:make-paper 2 3)))
    (lisp-system:info-paper x 'type))
  0)

(deftest make-paper-type.2
  (let ((x (lisp-system:make-paper 2 3 :type 10)))
    (lisp-system:info-paper x 'type))
  10)

(deftest-error make-paper-type.3
  (eval '(lisp-system:make-paper 2 3 :type nil)))

(deftest-error make-paper-type.4
  (eval '(lisp-system:make-paper 2 3 :type -1)))

(deftest-error make-paper-type.5
  (eval '(lisp-system:make-paper 2 3 :type #x0100)))


;;
;;  info-paper
;;
(deftest info-paper-length.1
  (let ((x (lisp-system:make-paper 2 3)))
    (lisp-system:info-paper x 'length))
  2)

(deftest info-paper-length.2
  (let ((x (lisp-system:make-paper 2 3)))
    (lisp-system:info-paper x 'length nil))
  2)

(deftest info-paper-length.3
  (let ((x (lisp-system:make-paper 2 3)))
    (lisp-system:info-paper x 'length #\A))
  3)

(deftest info-paper-length.4
  (let ((x (lisp-system:make-paper 0 3)))
    (values
      (lisp-system:info-paper x 'length nil)
      (lisp-system:info-paper x 'length t)))
  0 3)

(deftest info-paper-length.5
  (let ((x (lisp-system:make-paper 4 0)))
    (values
      (lisp-system:info-paper x 'length nil)
      (lisp-system:info-paper x 'length t)))
  4 0)

(deftest info-paper-list.1
  (let ((x (lisp-system:make-paper 4 3 :fill #xCC)))
    (lisp-system:info-paper x 'list))
  (nil nil nil nil))

(deftest info-paper-list.2
  (let ((x (lisp-system:make-paper 4 3 :fill #xCC)))
    (lisp-system:array-paper x 2 #\Z)
    (lisp-system:info-paper x 'list nil))
  (nil nil #\Z nil))

(deftest info-paper-list.3
  (let ((x (lisp-system:make-paper 4 3 :fill #xCC)))
    (lisp-system:info-paper x 'list t))
  (#xCC #xCC #xCC))

(deftest info-paper-vector.1
  (let ((x (lisp-system:make-paper 4 3 :fill #xCC)))
    (lisp-system:info-paper x 'vector))
  #(nil nil nil nil))

(deftest info-paper-vector.2
  (let ((x (lisp-system:make-paper 4 3 :fill #xCC)))
    (lisp-system:array-paper x 2 #\Z)
    (lisp-system:info-paper x 'vector nil))
  #(nil nil #\Z nil))

(deftest info-paper-vector.3
  (let ((x (lisp-system:make-paper 4 3 :fill #xCC)))
    (lisp-system:info-paper x 'vector t))
  #(#xCC #xCC #xCC))

(deftest info-paper-type.1
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:info-paper x 'type))
  0)

(deftest info-paper-type.2
  (let ((x (lisp-system:make-paper 4 3 :type #xDD)))
    (lisp-system:info-paper x 'type))
  #xDD)

(deftest info-paper-type.3
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:info-paper x 'type 123))
  123)

(deftest info-paper-type.4
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:info-paper x 'type 123)
    (lisp-system:info-paper x 'type))
  123)

(deftest-error info-paper-type.5
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:info-paper x 'type -1)))

(deftest-error info-paper-type.6
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:info-paper x 'type #x0100)))


;;
;;  array-paper
;;
(deftest array-paper.1
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:array-paper x 0))
  nil)

(deftest array-paper.2
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:array-paper x 1 'hello))
  hello)

(deftest array-paper.3
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:array-paper x 1 'hello)
    (lisp-system:array-paper x 1))
  hello)

(deftest array-paper.4
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:array-paper x 1 'hello)
    (lisp-system:info-paper x 'list))
  (nil hello nil nil))

(deftest-error array-paper.5
  (eval '(let ((x (lisp-system:make-paper 4 3)))
           (lisp-system:array-paper x -1))))

(deftest-error array-paper.6
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:array-paper x 4)))


;;
;;  body-paper
;;
(deftest body-paper.1
  (let ((x (lisp-system:make-paper 4 3 :fill 99)))
    (lisp-system:body-paper x 0))
  99)

(deftest body-paper.2
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:body-paper x 1 33))
  33)

(deftest body-paper.3
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:body-paper x 1 33)
    (lisp-system:body-paper x 1))
  33)

(deftest body-paper.4
  (let ((x (lisp-system:make-paper 4 3 :fill t)))
    (lisp-system:body-paper x 1 33)
    (lisp-system:info-paper x 'list t))
  (0 33 0))

(deftest-error body-paper.5
  (eval '(let ((x (lisp-system:make-paper 4 3)))
           (lisp-system:body-paper x -1))))

(deftest-error body-paper.6
  (let ((x (lisp-system:make-paper 4 3)))
    (lisp-system:body-paper x 3)))

(deftest-error body-paper.7
  (eval '(let ((x (lisp-system:make-paper 4 3)))
           (lisp-system:body-paper x 1 -1))))

(deftest-error body-paper.8
  (eval '(let ((x (lisp-system:make-paper 4 3)))
           (lisp-system:body-paper x 1 #x0100))))


;;
;;  print-write
;;
(deftest paper-write.1
  (let ((x (lisp-system:make-paper 2 3 :type 5)))
    (subseq
      (princ-to-string x)
      0 10))
  "#<PAPER 5 ")

(deftest paper-write.2
  (let ((x (lisp-system:make-paper 2 3 :type 5)))
    (subseq
      (prin1-to-string x)
      0 10))
  "#<PAPER 5 ")


;;
;;  compile-file
;;
(defvar *paper-value*)
(defvar *paper-input*)

(defun paper-compile-file ()
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (with-open-file (stream input :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (let ((*print-pretty* t)
              (*print-right-margin* 70))
          (format stream "(setq *paper-value* #.*paper-input*)")))
      (file-position input :start)
      (compile-file input :output-file output)
      (setq *paper-value* nil)
      (file-position output :start)
      (load output :type :fasl)
      *paper-value*)))

(deftest paper-compile-file.1
  (let ((x (lisp-system:make-paper 3 4))
        *paper-value* *paper-input*)
    (setq *paper-input* x)
    (paper-compile-file)
    (typep *paper-value* 'lisp-system::paper))
  t)

(deftest paper-compile-file.2
  (let ((x (lisp-system:make-paper 2 3 :type 123))
        *paper-value* *paper-input*)
    (lisp-system:array-paper x 0 'hello)
    (lisp-system:array-paper x 1 9876)
    (lisp-system:body-paper x 0 #x33)
    (lisp-system:body-paper x 1 #xBB)
    (lisp-system:body-paper x 2 #xCC)
    (setq *paper-input* x)
    (paper-compile-file)
    (values
      (lisp-system:info-paper *paper-value* 'type)
      (lisp-system:info-paper *paper-value* 'list nil)
      (lisp-system:info-paper *paper-value* 'list t)))
  123
  (hello 9876)
  (#x33 #xBB #xCC))


;;
;;  clos
;;
(deftest paper-clos.1
  (lisp-system:closp
    (find-class 'lisp-system::paper))
  t)

(deftest paper-clos.2
  (typep (find-class 'lisp-system::paper) 'built-in-class)
  t)

(deftest paper-clos.3
  (progn
    (defgeneric paper-clos-1 (x))
    (defmethod paper-clos-1 ((x lisp-system::paper))
      (declare (ignore x))
      100)
    (defmethod paper-clos-1 (x)
      (declare (ignore x))
      200)
    (let ((x (lisp-system:make-paper 0 0)))
      (values
        (paper-clos-1 x)
        (paper-clos-1 'hello))))
  100 200)

