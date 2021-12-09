;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  type
;;
(deftest type-list.1
  (progn
    (defstruct (type-list-1a (:type list))
      (aaa 10) (bbb 20))
    (defstruct (type-list-1b (:type list) (:include type-list-1a))
      (ccc 30) (ddd 40))
    (make-type-list-1b))
  (10 20 30 40))

(deftest-error type-list.2
  (progn
    (defstruct (type-list-2a (:type vector))
      (aaa 10) (bbb 20))
    (defstruct (type-list-2b (:type list) (:include type-list-2a))
      (ccc 30) (ddd 40))))

(deftest-error type-list.3
  (progn
    (defstruct (type-list-3a)
      (aaa 10) (bbb 20))
    (defstruct (type-list-3b (:type list) (:include type-list-3a))
      (ccc 30) (ddd 40))))

(deftest type-vector.1
  (progn
    (defstruct (type-vector-1a (:type vector))
      (aaa 10) (bbb 20))
    (defstruct (type-vector-1b (:type vector) (:include type-vector-1a))
      (ccc 30) (ddd 40))
    (make-type-vector-1b))
  #(10 20 30 40))

(deftest-error type-vector.2
  (progn
    (defstruct (type-vector-2a (:type (vector character)))
      (aaa 10) (bbb 20))
    (defstruct (type-vector-2b (:type vector) (:include type-vector-2a))
      (ccc 30) (ddd 40))
    (make-type-vector-2b)))

(deftest-error type-vector.3
  (progn
    (defstruct (type-vector-3a (:type vector))
      (aaa 10) (bbb 20))
    (defstruct (type-vector-3b (:type (vector (unsigned-byte 8)))
                               (:include type-vector-3a))
      (ccc 30) (ddd 40))
    (make-type-vector-3b)))


;;
;;  specialized write
;;
(deftest specialized-write.1
  (progn
    (defstruct (specialized-write-1 (:type (vector character)))
      aaa bbb)
    (make-specialized-write-1))
  #(#\u0 #\u0))

(deftest specialized-write.2
  (progn
    (defstruct (specialized-write-2 (:type (vector character)))
      aaa bbb)
    (let ((x (make-specialized-write-2)))
      (setf (specialized-write-2-aaa x) #\a)
      x))
  #(#\a #\u0))

(deftest-error specialized-write.3
  (progn
    (defstruct (specialized-write-3 (:type (vector character)))
      aaa bbb)
    (let ((x (make-specialized-write-3)))
      (setf (specialized-write-3-aaa x) 100)
      x)))

(deftest-error specialized-write.4
  (progn
    (defstruct (specialized-write-4 (:type (vector (unsigned-byte 8))))
      aaa bbb)
    (let ((x (make-specialized-write-4)))
      (setf (specialized-write-4-aaa x) 'hello)
      x)))

(deftest specialized-write.5
  (progn
    (defstruct (specialized-write-5 (:type (vector (unsigned-byte 8))))
      aaa bbb)
    (let ((x (make-specialized-write-5)))
      (setf (specialized-write-5-aaa x) 200)
      x))
  #(200 0))

(deftest-error specialized-write.6
  (progn
    (defstruct (specialized-write-6 (:type (vector (unsigned-byte 8))))
      (aaa 'hello))
    (make-specialized-write-6)))

(deftest-error specialized-write.7
  (progn
    (defstruct (specialized-write-7 (:type (vector character)))
      (aaa 100))
    (make-specialized-write-7)))


;;
;;  default value
;;
(deftest specialized-default.1
  (progn
    (defstruct (specialized-default-1 (:type vector))
      aaa)
    (make-specialized-default-1))
  #(nil))

(deftest specialized-default.2
  (progn
    (defstruct (specialized-default-2 (:type (vector (signed-byte 8))))
      aaa)
    (make-specialized-default-2))
  #(0))

(deftest specialized-default.3
  (progn
    (defstruct (specialized-default-3 (:type (vector (unsigned-byte 16))))
      aaa)
    (make-specialized-default-3))
  #(0))

(deftest specialized-default.4
  (progn
    (defstruct (specialized-default-4 (:type (vector single-float)))
      aaa)
    (make-specialized-default-4))
  #(0.0f0))

(deftest specialized-default.5
  (progn
    (defstruct (specialized-default-5 (:type (vector long-float)))
      aaa)
    (make-specialized-default-5))
  #(0.0L0))

(deftest specialized-default.6
  (progn
    (defstruct (specialized-default-6 (:type (vector character)))
      aaa)
    (make-specialized-default-6))
  #(#\u0))


;;
;;  list/vector shadow slot initform
;;
(deftest-error list-shadow.1
  (progn
    (defstruct (list-shadow-1a (:type list))
      (aaa 10) (bbb 20))
    (defstruct (list-shadow-1b (:type list) (:include list-shadow-1a))
      (aaa 30))))

(deftest list-shadow.2
  (progn
    (defstruct (list-shadow-2a (:type list))
      (aaa 10) (bbb 20))
    (defstruct (list-shadow-2b
                 (:type list)
                 (:include list-shadow-2a (aaa 30)))
      (ccc 40))
    (values
      (make-list-shadow-2a)
      (make-list-shadow-2b)))
  (10 20)
  (30 20 40))

(deftest list-shadow.3
  (progn
    (defstruct (list-shadow-3a (:type list))
      (aaa 10) (bbb 20))
    (defstruct (list-shadow-3b
                 (:type list)
                 (:include list-shadow-3a (aaa 30)))
      (ccc 40))
    (defstruct (list-shadow-3c
                 (:type list)
                 (:include list-shadow-3b (aaa 50)))
      (ddd 60))
    (values
      (make-list-shadow-3a)
      (make-list-shadow-3b)
      (make-list-shadow-3c)))
  (10 20)
  (30 20 40)
  (50 20 40 60))

(deftest-error vector-shadow.1
  (progn
    (defstruct (vector-shadow-1a (:type vector))
      (aaa 10) (bbb 20))
    (defstruct (vector-shadow-1b (:type vector) (:include vector-shadow-1a))
      (aaa 30))))

(deftest vector-shadow.2
  (progn
    (defstruct (vector-shadow-2a (:type vector))
      (aaa 10) (bbb 20))
    (defstruct (vector-shadow-2b
                 (:type vector)
                 (:include vector-shadow-2a (aaa 30)))
      (ccc 40))
    (values
      (make-vector-shadow-2a)
      (make-vector-shadow-2b)))
  #(10 20)
  #(30 20 40))

(deftest vector-shadow.3
  (progn
    (defstruct (vector-shadow-3a (:type vector))
      (aaa 10) (bbb 20))
    (defstruct (vector-shadow-3b
                 (:type vector)
                 (:include vector-shadow-3a (aaa 30)))
      (ccc 40))
    (defstruct (vector-shadow-3c
                 (:type vector)
                 (:include vector-shadow-3b (aaa 50)))
      (ddd 60))
    (values
      (make-vector-shadow-3a)
      (make-vector-shadow-3b)
      (make-vector-shadow-3c)))
  #(10 20)
  #(30 20 40)
  #(50 20 40 60))


;;
;;  Invalid type
;;
(deftest-error invalid-type.1
  (eval '(defstruct (invalid-type :type))))

(deftest-error invalid-type.2
  (eval '(defstruct (invalid-type (:type)))))

(deftest-error invalid-type.3
  (eval '(defstruct (invalid-type (:type symbol)))))

