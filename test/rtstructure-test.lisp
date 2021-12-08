;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  type
;;
(deftest structure-type.1
  (progn
    (defstruct structure-type-1 aaa)
    (lisp-system:sysctl 'structure 'type 'structure-type-1))
  class t)

(deftest structure-type.2
  (progn
    (defstruct (structure-type-2 (:type list)) aaa)
    (lisp-system:sysctl 'structure 'type 'structure-type-2))
  list t)


;;
;;  vector-type
;;
(deftest vector-type.1
  (progn
    (defstruct (vector-type-1 (:type (vector symbol))) aaa)
    (lisp-system:sysctl 'structure 'type 'vector-type-1))
  (vector t) t)

(deftest vector-type.2
  (progn
    (defstruct (vector-type-2 (:type (vector integer))
                              (:include vector-type-1)))
    (lisp-system:sysctl 'structure 'type 'vector-type-2))
  (vector t) t)

(deftest vector-type.3
  (progn
    (defstruct (vector-type-3 (:type (vector character))))
    (lisp-system:sysctl 'structure 'type 'vector-type-3))
  (vector character) t)

(deftest-error vector-type.4
  (defstruct (vector-type-4 (:type (vector character)) :named)))

(deftest-error vector-type.5
  (defstruct (vector-type-5 (:type (vector character))
                            (:include vector-type-1))))

(deftest vector-type.6
  (progn
    (defstruct (vector-type-6 (:type (vector (unsigned-byte 8)))))
    (lisp-system:sysctl 'structure 'type 'vector-type-6))
  (vector (unsigned-byte 8)) t)

(deftest-error vector-type.7
  (defstruct (vector-type-7 (:type (vector (unsigned-byte 16)))
                            (:include vector-type-6))))

(deftest-error vector-type.8
  (progn
    (defstruct (vector-type-8a (:type (vector (unsigned-byte 16)))))
    (defstruct (vector-type-8b (:type (vector (unsigned-byte 8)))
                               (:include vector-type-8a)))))

(deftest vector-type.9
  (progn
    (defstruct (vector-type-9a (:type (vector (unsigned-byte 16)))))
    (defstruct (vector-type-9b (:type (vector (unsigned-byte 16)))
                               (:include vector-type-9a)))
    (lisp-system:sysctl 'structure 'type 'vector-type-9b))
  (vector (unsigned-byte 16)) t)

(deftest vector-type.10
  (progn
    (defstruct (vector-type-10 (:type (vector (unsigned-byte 16))))
      (aaa 100) bbb)
    (make-vector-type-10))
  #(100 0))


;;
;;  documentation
;;
(deftest defstruct-documentation.1
  (progn
    (defstruct defstruct-documentation-1 "Hello" aaa bbb)
    (values
      (documentation 'defstruct-documentation-1 'structure)
      (documentation 'defstruct-documentation-1 'type)
      (documentation (find-class 'defstruct-documentation-1) t)))
  "Hello" "Hello" "Hello")

(deftest defstruct-documentation.2
  (progn
    (defstruct (defstruct-documentation-2 (:type list))
      "AAA" aaa bbb)
    (documentation 'defstruct-documentation-2 'structure))
  "AAA")


;;
;;  issues
;;
(deftest issues-sharp-6.1
  (defun issues-sharp-6-1-function ()
    (loop for issues-sharp-6-1-i from 0 to 3
          do (print issues-sharp-6-1-i)))
  issues-sharp-6-1-function)

(deftest issues-sharp-6-2
  (progn
    (defun issues-sharp-6-2-function ()
      (loop for issues-sharp-6-2-i from 0 to 3
            collect issues-sharp-6-2-i))
    (issues-sharp-6-2-function))
  (0 1 2 3))

(deftest issues-sharp-6-3
  (defun issues-sharp-6-3-function ()
    (loop for issues-sharp-6-3-i in '(0 1 2 3)
          do (print issues-sharp-6-3-i)))
  issues-sharp-6-3-function)

(deftest issues-sharp-6-4
  (progn
    (defun issues-sharp-6-4-function ()
      (loop for issues-sharp-6-4-i in '(0 1 2 3)
            collect issues-sharp-6-4-i))
    (issues-sharp-6-4-function))
  (0 1 2 3))

(deftest issues-sharp-6-5
  (defun issues-sharp-6-5-function ()
    (loop for (issues-sharp-6-5-a issues-sharp-6-5-b)
          in '((0 1) (2 3) (4 5))
          do (print issues-sharp-6-5-a)))
  issues-sharp-6-5-function)


(deftest issues-sharp-6-6
  (defun issues-sharp-6-6-function ()
    (loop for issues-sharp-6-6-i on '(0 1 2 3)
          do (print issues-sharp-6-6-i)))
  issues-sharp-6-6-function)

