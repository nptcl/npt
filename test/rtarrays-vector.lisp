;;
;;  ANSI COMMON LISP: 15. Arrays
;;

;;
;;  Accessor SVREF
;;
(deftest svref.1
  (let ((pos #(10 20 30)))
    (svref pos 1))
  20)

(deftest svref.2
  (let ((pos (make-array 10 :initial-contents '(10 20 30 40 50 0 1 2 3 4)
                         :fill-pointer nil
                         :adjustable nil
                         :displaced-to nil)))
    (svref pos 2))
  30)

(deftest-error svref.3
  (let ((pos (make-array 10 :fill-pointer t :adjustable t)))
    (svref pos 2))
  type-error)

(deftest-error svref.4
  (svref "Hello" 0)
  type-error)

(deftest-error svref.5
  (svref #*1000010100 0)
  type-error)

(deftest-error svref.6
  (let ((pos (make-array 10 :element-type 'character)))
    (svref pos 2))
  type-error)

(deftest-error svref.7
  (svref #(1 2 3 4 5) 5))

(deftest-error svref-error.1
  (eval '(svref 100 1))
  type-error)

(deftest-error svref-error.2
  (eval '(svref #(1 2 3) -1))
  type-error)

(deftest-error svref-error.3
  (eval '(svref #(1 2 3) nil))
  type-error)

(deftest-error! svref-error.4
  (eval '(svref #(1 2 3))))

(deftest-error! svref-error.5
  (eval '(svref #(1 2 3) 1 1)))


;;
;;  Accessor (SETF SVREF)
;;
(deftest setf-svref.1
  (let ((pos #(10 20 30)))
    (setf (svref pos 1) 999))
  999)

(deftest setf-svref.2
  (let ((pos #(10 20 30)))
    (setf (svref pos 1) 999)
    (svref pos 1))
  999)

(deftest setf-svref.3
  (let ((pos (make-array 10 :initial-contents '(10 20 30 40 50 0 1 2 3 4)
                         :fill-pointer nil
                         :adjustable nil
                         :displaced-to nil)))
    (setf (svref pos 2) 999)
    (svref pos 2))
  999)

(deftest-error setf-svref.4
  (setf (svref (make-array 10 :fill-pointer t :adjustable t) 2) :hello)
  type-error)

(deftest-error setf-svref.5
  (setf (svref "Hello" 0) #\a)
  type-error)

(deftest-error setf-svref.6
  (setf (svref #*1000010100 0) 1)
  type-error)

(deftest-error setf-svref.7
  (setf (svref (make-array 10 :element-type 'character) 2) #\A)
  type-error)

(deftest-error setf-svref.8
  (setf (svref #(1 2 3 4 5) 5) 100))

(deftest-error setf-svref-error.1
  (eval '(setf (svref 100 1) nil))
  type-error)

(deftest-error setf-svref-error.2
  (eval '(setf (svref #(1 2 3) -1) 1))
  type-error)

(deftest-error setf-svref-error.3
  (eval '(setf (svref #(1 2 3) nil) 1))
  type-error)

(deftest-error! setf-svref-error.4
  (eval '(setf (svref #(1 2 3)) 99)))

(deftest-error! setf-svref-error.5
  (eval '(setf (svref #(1 2 3) 1 1) 99)))

;;  ANSI Common Lisp
(defvar *svref-1*)

(deftest svref-test.1
  (simple-vector-p
    (setq *svref-1* (vector 1 2 'sirens)))
  t)

(deftest svref-test.2
  (svref *svref-1* 0)
  1)

(deftest svref-test.3
  (svref *svref-1* 2)
  SIRENS)

(deftest svref-test.4
  (setf (svref *svref-1* 1) 'newcomer)
  newcomer)

(deftest svref-test.5
  *svref-1*
  #(1 newcomer sirens))


;;
;;  Function VECTOR
;;
(deftest vector.1
  (array-total-size
    (vector 10 20 30 40))
  4)

(deftest vector.2
  (arrayp (vector 10 20 30 40))
  t)

(deftest vector.3
  (simple-vector-p (vector 10 20 30 40))
  t)

(deftest vector.4
  (array-total-size (vector))
  0)

(deftest vector.5
  (vector 10 nil t :hello)
  #(10 nil t :hello))

;;  ANSI Common Lisp
(defvar *vector-1*)

(deftest vector-test.1
  (arrayp (setq *vector-1* (vector 1 2 'sirens)))
  t)

(deftest vector-test.2
  (vectorp *vector-1*)
  t)

(deftest vector-test.3
  (simple-vector-p *vector-1*)
  t)

(deftest vector-test.4
  (length *vector-1*)
  3)


;;
;;  Function VECTOR-POP
;;
(deftest vector-pop.1
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer t)))
    (values
      (vector-pop pos)
      (vector-pop pos)
      (vector-pop pos)
      (fill-pointer pos)
      pos))
  5 6 7 2 #(9 8))

(deftest-error vector-pop.2
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer nil)))
    (vector-pop pos))
  type-error)

(deftest-error vector-pop.3
  (vector-pop #(10 20 30 40 50))
  type-error)

(deftest-error vector-pop.4
  (let ((pos (make-array 5 :initial-contents '(9 8 7 6 5) :fill-pointer 0)))
    (vector-pop pos))
  type-error)

(deftest vector-pop.5
  (let ((pos (make-array 5
                         :element-type 'character
                         :initial-contents "Hello"
                         :fill-pointer t)))
    (values
      (vector-pop pos)
      (vector-pop pos)
      (vector-pop pos)
      (fill-pointer pos)
      pos))
  #\o #\l #\l 2 "He")

(deftest-error vector-pop-error.1
  (eval '(vector-pop "Hello"))
  type-error)

(deftest-error vector-pop-error.2
  (eval '(vector-pop #(10 20 30)))
  type-error)

(deftest-error vector-pop-error.3
  (eval '(vector-pop #*10110))
  type-error)

(deftest-error vector-pop-error.4
  (eval '(vector-pop :hello))
  type-error)

(deftest-error vector-pop-error.5
  (eval '(vector-pop (make-array '(2 3))))
  type-error)

(deftest-error vector-pop-error.6
  (eval '(vector-pop (make-array 2)))
  type-error)

(deftest-error vector-pop-error.7
  (eval '(vector-pop :hello))
  type-error)

(deftest-error vector-pop-error.8
  (eval '(vector-pop (make-array 5 :fill-pointer 0)))
  type-error)

(deftest-error! vector-pop-error.9
  (eval '(vector-pop)))

(deftest-error! vector-pop-error.10
  (eval '(vector-pop (make-array 5 :fill-pointer t) nil)))

;;  ANSI Common Lisp
(defvar *vector-pop-1*)
(defvar *vector-pop-2*)

(deftest vector-pop-test.1
  (vector-push
    (setq *vector-pop-1* (list '*vector-pop-1*))
    (setq *vector-pop-2* (make-array 8 :fill-pointer 2 :initial-element 'sisyphus)))
  2)

(deftest vector-pop-test.2
  (fill-pointer *vector-pop-2*)
  3)

(deftest vector-pop-test.3
  (eq (vector-pop *vector-pop-2*) *vector-pop-1*)
  t)

(deftest vector-pop-test.4
  (vector-pop *vector-pop-2*)
  sisyphus)

(deftest vector-pop-test.5
  (fill-pointer *vector-pop-2*)
  1)


;;
;;  Function VECTOR-PUSH
;;
(deftest vector-push.1
  (let ((pos (make-array 4 :fill-pointer 0)))
    (vector-push 1 pos))
  0)

(deftest vector-push.2
  (let ((pos (make-array 4 :fill-pointer 0)))
    (values
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)
      (vector-push 1 pos)))
  0 1 2 3 nil nil)

(deftest vector-push.3
  (let ((pos (make-array 4 :fill-pointer 0)))
    (vector-push 1 pos)
    (vector-push 2 pos)
    (vector-push 3 pos)
    (vector-push 4 pos)
    (values
      (aref pos 0)
      (aref pos 1)
      (aref pos 2)
      (aref pos 3)
      (fill-pointer pos)))
  1 2 3 4 4)

(deftest-error vector-push.4
  (let ((pos (make-array 4 :fill-pointer nil)))
    (vector-push 1 pos))
  type-error)

(deftest-error vector-push.5
  (vector-push 1 #(10 20 30))
  type-error)

(deftest-error vector-push-error.1
  (eval '(vector-push 1 10))
  type-error)

(deftest-error! vector-push-error.2
  (eval '(vector-push 1)))

(deftest-error! vector-push-error.3
  (eval '(vector-push 1 (make-array 4 :fill-pointer t) nil)))


;;
;;  Function VECTOR-PUSH-EXTEND
;;
(deftest vector-push-extend.1
  (let ((pos (make-array 5 :fill-pointer 0 :adjustable t)))
    (values
      (vector-push-extend 'x pos)
      pos))
  0 #(x))

(deftest vector-push-extend.2
  (let ((pos (make-array 4 :fill-pointer 0 :adjustable nil)))
    (values
      (vector-push-extend 99 pos)
      (vector-push-extend 88 pos)
      (vector-push-extend 77 pos)
      (vector-push-extend 66 pos)
      pos))
  0 1 2 3 #(99 88 77 66))

(deftest-error vector-push-extend.3
  (let ((pos (make-array 4 :fill-pointer 0 :adjustable nil)))
    (vector-push-extend 99 pos)
    (vector-push-extend 88 pos)
    (vector-push-extend 77 pos)
    (vector-push-extend 66 pos)
    (vector-push-extend 55 pos))
  type-error)

(deftest vector-push-extend.4
  (let ((pos (make-array 5 :fill-pointer 4 :adjustable t :initial-element nil)))
    (values
      (vector-push-extend 'x pos)
      pos))
  4 #(nil nil nil nil x))

(deftest vector-push-extend.5
  (let ((pos (make-array 5 :fill-pointer 5 :adjustable t :initial-element nil)))
    (values
      (vector-push-extend 'x pos)
      pos))
  5 #(nil nil nil nil nil x))

(deftest vector-push-extend.6
  (let* ((a (make-array 3 :fill-pointer 3 :adjustable t :initial-element nil))
         (b (make-array 3 :fill-pointer 3 :adjustable t :displaced-to a)))
    (vector-push-extend 'x b)
    (values a b))
  #(nil nil nil)
  #(nil nil nil x))

(deftest vector-push-extend.7
  (let* ((a (make-array 3 :fill-pointer 3 :adjustable t :initial-element nil))
         (b (make-array 3 :fill-pointer 3 :adjustable t :displaced-to a)))
    (vector-push-extend 'x b)
    (vector-push-extend 'y b)
    (vector-push-extend 'z b)
    (vector-push-extend 'w a)
    (values a b))
  #(nil nil nil w)
  #(nil nil nil x y z))

(deftest vector-push-extend.8
  (let* ((a (make-array 10 :initial-contents '(a b c d e f g h i j)))
         (b (make-array 7 :displaced-to a :fill-pointer t :adjustable t)))
    (setf (aref a 1) :xx)
    (vector-push-extend :yy b)
    (setf (aref a 2) :zz)
    (values a b))
  #(a :xx :zz d e f g h i j)
  #(a :xx c d e f g :yy))

(deftest vector-push-extend.9
  (let ((x (make-array 5 :fill-pointer t :adjustable t)))
    (vector-push-extend 10 x 100)
    (array-total-size x))
  105)

(deftest-error vector-push-extend-error.1
  (eval '(vector-push-extend 10 20))
  type-error)

(deftest-error vector-push-extend-error.2
  (eval '(let ((x (make-array 10 :fill-pointer 0 :adjustable t)))
           (vector-push-extend 10 x :hello)))
  type-error)

(deftest-error! vector-push-extend-error.3
  (eval '(vector-push-extend 10)))

(deftest-error! vector-push-extend-error.4
  (eval '(let ((x (make-array 10 :fill-pointer 0 :adjustable t)))
           (vector-push-extend 10 x 30 40))))

;;  ANSI Common Lisp
(defvar *vector-push-1*)
(defvar *vector-push-2*)
(defvar *vector-push-3*)

(deftest vector-push-test.1
  (vector-push
    (setq *vector-push-1* (list '*vector-push-1*))
    (setq *vector-push-2* (make-array 8 :fill-pointer 2
                                      :initial-element 'first-one)))
  2)

(deftest vector-push-test.2
  (fill-pointer *vector-push-2*)
  3)

(deftest vector-push-test.3
  (eq (aref *vector-push-2* 2) *vector-push-1*)
  t)

(deftest vector-push-test.4
  (vector-push-extend
    #\X
    (setq *vector-push-3*
          (make-array 5 :element-type 'character
                      :adjustable t
                      :fill-pointer 3)))
  3)

(deftest vector-push-test.5
  (fill-pointer *vector-push-3*)
  4)

(deftest vector-push-test.6
  (vector-push-extend #\Y *vector-push-3* 4)
  4)

(deftest vector-push-test.7
  (<= 5 (array-total-size *vector-push-3*))
  t)

(deftest vector-push-test.8
  (vector-push-extend #\Z *vector-push-3* 4)
  5)

(deftest vector-push-test.9
  (<= 9 (array-total-size *vector-push-3*))
  t)

