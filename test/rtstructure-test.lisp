;;
;;  ANSI COMMON LISP: 8. Structures
;;

;;
;;  vector-type
;;
(defstruct (vector-type-a (:type (vector symbol))) aaa)
(deftest-error vector-type.1
  (defstruct (vector-type-1 (:type (vector integer))
                            (:include vector-type-a))))

(defstruct (vector-type-b (:type (vector integer))) aaa)
(deftest-error vector-type.2
  (defstruct (vector-type-2 (:type (vector real))
                            (:include vector-type-b))))

(deftest vector-type.3
  (defstruct (vector-type-3 (:type (vector fixnum))
                            (:include vector-type-b)))
  vector-type-3)

(deftest vector-type.4
  (let ((inst (make-vector-type-a)))
    (setf (vector-type-a-aaa inst) 'hello)
    (vector-type-a-aaa inst))
  hello)

(deftest-error vector-type.5
  (let ((inst (make-vector-type-a)))
    (setf (vector-type-a-aaa inst) 100)))


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

