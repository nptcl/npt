;;
;;  ANSI COMMON LISP: 14. Conses
;;

;;
;;  Function ACONS
;;
(deftest acons.1
  (acons 10 20 nil)
  ((10 . 20)))

(deftest acons.2
  (acons 10 20 '(a b c))
  ((10 . 20) a b c))

(defvar *alist-list* '())

(deftest acons.3
  (acons 1 "one" *alist-list*)
  ((1 . "one")))

(deftest acons.4
  *alist-list*
  nil)

(deftest acons.5
  (setq *alist-list* (acons 1 "one" (acons 2 "two" *alist-list*)))
  ((1 . "one") (2 . "two")))

(deftest acons.6
  (assoc 1 *alist-list*)
  (1 . "one"))

(deftest acons.7
  (setq *alist-list* (acons 1 "uno" *alist-list*))
  ((1 . "uno") (1 . "one") (2 . "two")))

(deftest acons.8
  (assoc 1 *alist-list*)
  (1 . "uno"))

(deftest-error acons-error.1
  (eval '(acons 10 20 30)))

(deftest-error! acons-error.2
  (eval '(acons 10 20)))

(deftest-error! acons-error.3
  (eval '(acons 10 20 nil 30)))


;;
;;  Function ASSOC
;;
(deftest assoc.1
  (assoc 10 nil)
  nil)

(deftest assoc.2
  (assoc 'y '((x . 10) (y . 20) (z . 3)))
  (y . 20))

(deftest assoc.3
  (assoc 'a '((x . 10) (y . 20) (z . 3)))
  nil)

(deftest assoc.4
  (assoc 21 '((10 . a) (20 . b) (30 . c)) :key #'1+ :test #'eql)
  (20 . b))

(deftest assoc.5
  (assoc '(10) '((10 . a) ((10) . b) (30 . c)) :test #'equal)
  ((10) . b))

(deftest assoc.6
  (assoc '10 '((10 . a) (10 . b) (30 . c)) :test-not #'eql)
  (30 . c))

(deftest assoc.7
  (assoc 'y '((x . 10) nil (y . 20) (z . 3)))
  (y . 20))

(deftest assoc.8
  (assoc nil '((a . b) nil (nil . c)))
  (nil . c))

(deftest-error assoc-error.1
  (assoc 'y '((x . 10) y  20 (z . 3))))

(deftest-error assoc-error.2
  (eval '(assoc 'y 20)))

(deftest-error! assoc-error.3
  (eval '(assoc t)))

(deftest-error! assoc-error.4
  (eval '(assoc t nil 10)))

(deftest-error! assoc-error.5
  (eval '(assoc t nil :key)))

(deftest-error! assoc-error.6
  (eval '(assoc t nil :key 10)))

(deftest-error! assoc-error.7
  (eval '(assoc t nil :hello 10)))

(deftest-error! assoc-error.8
  (eval '(assoc t nil :test (constantly t) :test-not (constantly t))))


;;
;;  Function ASSOC-IF
;;
(deftest assoc-if.1
  (assoc-if (lambda (x) (eql x 3)) '((1 . a) (2 . b) (3 . c)))
  (3 . c))

(deftest assoc-if.2
  (assoc-if (lambda (x) (eql x 3)) nil)
  nil)

(deftest assoc-if.3
  (assoc-if (lambda (x) (eql x 100)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest assoc-if.4
  (assoc-if #'null '((1 . a) nil (nil . b) (3 . c)))
  (nil . b))

(deftest-error assoc-if-error.1
  (assoc-if #'null '((x . 10) y  20 (z . 3))))

(deftest-error assoc-if-error.2
  (eval '(assoc-if #'null 20)))

(deftest-error! assoc-if-error.3
  (eval '(assoc-if #'null)))

(deftest-error! assoc-if-error.4
  (eval '(assoc-if #'null nil 10)))

(deftest-error! assoc-if-error.5
  (eval '(assoc-if #'null nil :key)))

(deftest-error! assoc-if-error.6
  (eval '(assoc-if #'null nil :key 10)))

(deftest-error! assoc-if-error.7
  (eval '(assoc-if #'null nil :hello 10)))


;;
;;  Function ASSOC-IF-NOT
;;
(deftest assoc-if-not.1
  (assoc-if-not (lambda (x) (eql x 1)) '((1 . a) (2 . b) (3 . c)))
  (2 . b))

(deftest assoc-if-not.2
  (assoc-if-not (lambda (x) (eql x 1)) nil)
  nil)

(deftest assoc-if-not.3
  (assoc-if-not (lambda (x) (numberp x)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest assoc-if-not.4
  (assoc-if-not #'identity '((1 . a) nil (nil . b) (3 . c)))
  (nil . b))

(deftest-error assoc-if-not-error.1
  (assoc-if-not #'ideftify '((x . 10) y  20 (z . 3))))

(deftest-error assoc-if-not-error.2
  (eval '(assoc-if-not #'ideftify 20)))

(deftest-error! assoc-if-not-error.3
  (eval '(assoc-if-not #'ideftify)))

(deftest-error! assoc-if-not-error.4
  (eval '(assoc-if-not #'ideftify nil 10)))

(deftest-error! assoc-if-not-error.5
  (eval '(assoc-if-not #'ideftify nil :key)))

(deftest-error! assoc-if-not-error.6
  (eval '(assoc-if-not #'ideftify nil :key 10)))

(deftest-error! assoc-if-not-error.7
  (eval '(assoc-if-not #'ideftify nil :hello 10)))

;; ANSI Common Lisp
(defparameter *assoc-list1* '((x . 100) (y . 200) (z . 50)))

(deftest assoc-test.1
  (assoc 'y *assoc-list1*)
  (y . 200))

(deftest assoc-test.2
  (rplacd (assoc 'y *assoc-list1*) 201)
  (y . 201))

(deftest assoc-test.3
  (assoc 'y *assoc-list1*)
  (y . 201))

(defparameter *assoc-list2* '((1 . "one") (2 . "two") (3 . "three")))

(deftest assoc-test.4
  (assoc 2 *assoc-list2*)
  (2 . "two"))

(deftest assoc-test.5
  (assoc-if #'evenp *assoc-list2*)
  (2 . "two"))

(deftest assoc-test.6
  (assoc-if-not #'(lambda(x) (< x 3)) *assoc-list2*)
  (3 . "three"))

(defparameter *assoc-list3* '(("one" . 1) ("two" . 2)))

(deftest assoc-test.7
  (assoc "one" *assoc-list3*)
  nil)

(deftest assoc-test.8
  (assoc "one" *assoc-list3* :test #'equalp)
  ("one" . 1))

(deftest assoc-test.9
  (assoc "two" *assoc-list3* :key #'(lambda(x) (char x 2)))
  nil)

(deftest assoc-test.10
  (assoc #\o *assoc-list3* :key #'(lambda(x) (char x 2)))
  ("two" . 2))

(deftest assoc-test.11
  (assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z)))
  (r . x))

(deftest assoc-test.12
  (assoc 'goo '((foo . bar) (zoo . goo)))
  nil)

(deftest assoc-test.13
  (assoc '2 '((1 a b c) (2 b c d) (-7 x y z)))
  (2 b c d))

(deftest assoc-test.14
  (setq *assoc-list3* '(("one" . 1) ("2" . 2) ("three" . 3)))
  (("one" . 1) ("2" . 2) ("three" . 3)))

(deftest assoc-test.15
  (assoc-if-not #'alpha-char-p *assoc-list3* :key #'(lambda (x) (char x 0)))
  ("2" . 2))


;;
;;  Function COPY-ALIST
;;
(deftest copy-alist.1
  (copy-alist nil)
  nil)

(deftest copy-alist.2
  (copy-alist '((a . b) (c . d)))
  ((a . b) (c . d)))

(deftest copy-alist.3
  (let ((a '((a . b) (c . d))))
    (eq (copy-alist a) a))
  nil)

(deftest copy-alist.4
  (let* ((a '(a . b))
         (b (list a '(c . d))))
    (eq (car (copy-alist b)) a))
  nil)

(deftest copy-alist.5
  (let* ((a '(a . b))
         (b (list a '(c . d))))
    (equal (car (copy-alist b)) a))
  t)

(deftest copy-alist.6
  (copy-alist '((a . b) nil (c . d)))
  ((a . b) nil (c . d)))

(defparameter *copy-alist1* (acons 1 "one" (acons 2 "two" '())))

(deftest copy-alist.7
  *copy-alist1*
  ((1 . "one") (2 . "two")))

(defparameter *copy-alist2* (copy-list *copy-alist1*))

(deftest copy-alist.8
  *copy-alist2*
  ((1 . "one") (2 . "two")))

(defparameter *copy-alist3* (copy-alist *copy-alist1*))

(deftest copy-alist.9
  *copy-alist3*
  ((1 . "one") (2 . "two")))

(deftest copy-alist.10
  (setf (cdr (assoc 2 *copy-alist3*)) "deux")
  "deux")

(deftest copy-alist.11
  *copy-alist3*
  ((1 . "one") (2 . "deux")))

(deftest copy-alist.12
  *copy-alist1*
  ((1 . "one") (2 . "two")))

(deftest copy-alist.13
  (setf (cdr (assoc 1 *copy-alist2*)) "uno")
  "uno")

(deftest copy-alist.14
  *copy-alist2*
  ((1 . "uno") (2 . "two")))

(deftest copy-alist.15
  *copy-alist1*
  ((1 . "uno") (2 . "two")))

(deftest-error copy-alist-error.1
  (copy-alist '((a . b) 10)))

(deftest-error copy-alist-error.2
  (eval '(copy-alist 10)))

(deftest-error! copy-alist-error.3
  (eval '(copy-alist)))

(deftest-error! copy-alist-error.4
  (eval '(copy-alist nil nil)))


;;
;;  Function PAIRLIS
;;
(deftest pairlis.1
  (pairlis nil nil)
  nil)

(deftest pairlis.2
  (pairlis nil nil nil)
  nil)

(deftest pairlis.3
  (pairlis nil nil '((c . 30)))
  ((c . 30)))

(deftest pairlis.4
  (pairlis '(a b) '(10 20))
  ((b . 20) (a . 10)))

(deftest pairlis.5
  (pairlis nil nil)
  nil)

(deftest pairlis.6
  (pairlis '(a b) '(10 20) '((c . 30)))
  ((b . 20) (a . 10) (c . 30)))

(deftest pairlis.7
  (pairlis '(one two) '(1 2) '((three . 3) (four . 19)))
  ((two . 2) (one . 1) (three . 3) (four . 19)))

(deftest pairlis.8
  (let ((keys '(1 2 3))
        (data '("one" "two" "three"))
        (alist '((4 . "four"))))
    (values
      (pairlis keys data)
      (pairlis keys data alist)
      alist))
  ((3 . "three") (2 . "two") (1 . "one"))
  ((3 . "three") (2 . "two") (1 . "one") (4 . "four"))
  ((4 . "four")))

(deftest-error pairlis-error.1
  (pairlis '(a b c) '(1 2)))

(deftest-error pairlis-error.2
  (pairlis '(a b c) '(1 2 3 4)))

(deftest-error pairlis-error.3
  (eval '(pairlis 10 nil nil))
  type-error)

(deftest-error pairlis-error.4
  (eval '(pairlis nil 20 nil))
  type-error)

(deftest-error pairlis-error.5
  (eval '(pairlis nil nil 30))
  type-error)

(deftest-error! pairlis-error.6
  (eval '(pairlis nil)))

(deftest-error! pairlis-error.7
  (eval '(pairlis nil nil nil nil)))


;;
;;  Function RASSOC
;;
(deftest rassoc.1
  (rassoc 10 nil)
  nil)

(deftest rassoc.2
  (rassoc '20 '((x . 10) (y . 20) (z . 3)))
  (y . 20))

(deftest rassoc.3
  (rassoc 'a '((x . 10) (y . 20) (z . 3)))
  nil)

(deftest rassoc.4
  (rassoc 21 '((a . 10) (b . 20) (c . 30)) :key #'1+ :test #'eql)
  (b . 20))

(deftest rassoc.5
  (rassoc '(10) '((a . 10) (b . (10)) (c . 30)) :test #'equal)
  (b . (10)))

(deftest rassoc.6
  (rassoc nil '((a . 10) nil (b . nil) (c . 30)))
  (b . nil))

(deftest rassoc.7
  (rassoc '20 '((x . 10) nil (y . 20) (z . 3)))
  (y . 20))

(deftest rassoc.8
  (rassoc nil '((a . b) nil (c . nil)))
  (c . nil))

(deftest-error rassoc-error.1
  (rassoc 'y '((x . 10) y  20 (z . 3))))

(deftest-error rassoc-error.2
  (eval '(rassoc 'y 20)))

(deftest-error! rassoc-error.3
  (eval '(rassoc t)))

(deftest-error! rassoc-error.4
  (eval '(rassoc t nil 10)))

(deftest-error! rassoc-error.5
  (eval '(rassoc t nil :key)))

(deftest-error! rassoc-error.6
  (eval '(rassoc t nil :key 10)))

(deftest-error! rassoc-error.7
  (eval '(rassoc t nil :hello 10)))

(deftest-error! rassoc-error.8
  (eval '(rassoc t nil :test (constantly t) :test-not (constantly t))))



;;
;;  Function RASSOC-IF
;;
(deftest rassoc-if.1
  (rassoc-if (lambda (x) (eql x 'c)) '((1 . a) (2 . b) (3 . c)))
  (3 . c))

(deftest rassoc-if.2
  (rassoc-if (lambda (x) (eql x 3)) nil)
  nil)

(deftest rassoc-if.3
  (rassoc-if (lambda (x) (eql x 100)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest rassoc-if.4
  (rassoc-if #'null '((1 . a) nil (2 . nil) (3 . c)))
  (2 . nil))

(deftest-error rassoc-if-error.1
  (rassoc-if #'null '((x . 10) y  20 (z . 3))))

(deftest-error rassoc-if-error.2
  (eval '(rassoc-if #'null 20)))

(deftest-error! rassoc-if-error.3
  (eval '(rassoc-if #'null)))

(deftest-error! rassoc-if-error.4
  (eval '(rassoc-if #'null nil 10)))

(deftest-error! rassoc-if-error.5
  (eval '(rassoc-if #'null nil :key)))

(deftest-error! rassoc-if-error.6
  (eval '(rassoc-if #'null nil :key 10)))

(deftest-error! rassoc-if-error.7
  (eval '(rassoc-if #'null nil :hello 10)))


;;
;;  Function RASSOC-IF-NOT
;;
(deftest rassoc-if-not.1
  (rassoc-if-not (lambda (x) (eql x 'a)) '((1 . a) (2 . b) (3 . c)))
  (2 . b))

(deftest rassoc-if-not.2
  (rassoc-if-not (lambda (x) (eql x 1)) nil)
  nil)

(deftest rassoc-if-not.3
  (rassoc-if-not (lambda (x) (symbolp x)) '((1 . a) (2 . b) (3 . c)))
  nil)

(deftest rassoc-if-not.4
  (rassoc-if-not #'identity '((1 . a) nil (2 . nil) (3 . c)))
  (2 . nil))

(deftest-error rassoc-if-not-error.1
  (rassoc-if-not #'ideftify '((x . 10) y  20 (z . 3))))

(deftest-error rassoc-if-not-error.2
  (eval '(rassoc-if-not #'ideftify 20)))

(deftest-error! rassoc-if-not-error.3
  (eval '(rassoc-if-not #'ideftify)))

(deftest-error! rassoc-if-not-error.4
  (eval '(rassoc-if-not #'ideftify nil 10)))

(deftest-error! rassoc-if-not-error.5
  (eval '(rassoc-if-not #'ideftify nil :key)))

(deftest-error! rassoc-if-not-error.6
  (eval '(rassoc-if-not #'ideftify nil :key 10)))

(deftest-error! rassoc-if-not-error.7
  (eval '(rassoc-if-not #'ideftify nil :hello 10)))

;; ANSI Common Lisp
(defparameter *rassoc-list* '((1 . "one") (2 . "two") (3 . 3)))

(deftest rassoc-test.1
  (rassoc 3 *rassoc-list*)
  (3 . 3))

(deftest rassoc-test.2
  (rassoc "two" *rassoc-list*)
  nil)

(deftest rassoc-test.3
  (rassoc "two" *rassoc-list* :test 'equal)
  (2 . "two"))

(deftest rassoc-test.4
  (rassoc 1 *rassoc-list* :key #'(lambda (x) (if (numberp x) (/ x 3))))
  (3 . 3))

(deftest rassoc-test.5
  (rassoc 'a '((a . b) (b . c) (c . a) (z . a)))
  (c . a))

(deftest rassoc-test.6
  (rassoc-if #'stringp *rassoc-list*)
  (1 . "one"))

(deftest rassoc-test.7
  (rassoc-if-not #'vectorp *rassoc-list*)
  (3 . 3))


;;
;;  Function GET-PROPERTIES
;;
(deftest get-properties.1
  (get-properties nil nil)
  nil nil nil)

(deftest get-properties.2
  (get-properties '(a b c d) nil)
  nil nil nil)

(deftest get-properties.3
  (get-properties '(a b c d) '(e f g))
  nil nil nil)

(deftest get-properties.4
  (get-properties '(a b c d e f) '(b))
  nil nil nil)

(deftest get-properties.5
  (get-properties '(a b c d e f) '(c))
  c d (c d e f))

(deftest get-properties.6
  (get-properties '(a b c d e f) '(c e))
  c d (c d e f))

(deftest get-properties.7
  (get-properties '(a b c d e f) '(e c))
  c d (c d e f))

(deftest get-properties.8
  (get-properties '(a b c d e f) '(z q))
  nil nil nil)

(deftest-error get-properties-error.1
  (eval '(get-properties 10 nil)))

(deftest-error get-properties-error.2
  (eval '(get-properties nil 20)))

(deftest-error! get-properties-error.3
  (eval '(get-properties nil)))

(deftest-error! get-properties-error.4
  (eval '(get-properties nil nil nil)))

(deftest-error! get-properties-error.5
  (eval '(get-properties '(a b c) nil)))

(defparameter *get-properties1* '())
(defparameter *get-properties2* '(prop1 prop2))

(deftest get-properties-test.1
  (getf *get-properties1* 'prop1)
  nil)

(deftest get-properties-test.2
  (setf (getf *get-properties1* 'prop1) 'val1)
  val1)

(deftest get-properties-test.3
  (eq (getf *get-properties1* 'prop1) 'val1)
  t)

(deftest get-properties-test.4
  (get-properties *get-properties1* *get-properties2*)
  prop1 val1 (prop1 val1))

(deftest get-properties-test.5
  *get-properties1*
  (prop1 val1))


;;
;;  Accessor GETF
;;
(deftest getf.1
  (getf nil nil)
  nil)

(deftest getf.2
  (getf nil 10)
  nil)

(deftest getf.3
  (getf '(a b c d) 'c)
  d)

(deftest getf.4
  (getf '(a b c d) 'z)
  nil)

(deftest getf.5
  (getf '(a b c d) 'z :hello)
  :hello)

(deftest getf.6
  (getf '(a b c d) 'a :hello)
  b)

(deftest getf.7
  (getf '(a b a d) 'a)
  b)

(deftest-error getf-error.1
  (eval '(getf 10 nil)))

(deftest-error! getf-error.2
  (eval '(getf nil)))

(deftest-error! getf-error.3
  (eval '(getf nil nil nil nil)))

(deftest-error! getf-error.4
  (eval '(getf '(a b c) 'c)))

;; ANSI Common Lisp
(defparameter *getf-list1* '())

(deftest getf-test.1
  (getf *getf-list1* 'prop1)
  nil)

(deftest getf-test.2
  (getf *getf-list1* 'prop1 7)
  7)

(deftest getf-test.3
  (getf *getf-list1* 'prop1)
  nil)

(deftest getf-test.4
  (setf (getf *getf-list1* 'prop1) 'val1)
  val1)

(deftest getf-test.5
  (eq (getf *getf-list1* 'prop1) 'val1)
  t)

(deftest getf-test.6
  (getf *getf-list1* 'prop1)
  val1)

(deftest getf-test.7
  (getf *getf-list1* 'prop1 7)
  val1)

(deftest getf-test.8
  *getf-list1*
  (prop1 val1))

(setq *getf-list2* (list 'a 'b 'c 'd 'e 'f))
(setq *getf-list3* (cddr *getf-list2*))

(deftest getf-test.9
  (remf *getf-list2* 'c)
  t)

(deftest getf-test.10
  *getf-list2*
  (a b e f))

(deftest getf-test.11
  *getf-list3*
  (c d e f))

(deftest getf-test.12
  (let ((plist '()))
    (incf (getf plist 'count 0))
    plist)
  (count 1))


;;
;;  Accessor (SETF GETF)
;;
(deftest setf-getf.1
  (let (x)
    (setf (getf x 'a) 10)
    x)
  (a 10))

(deftest setf-getf.2
  (let (x)
    (setf (getf x 'a) 10)
    (setf (getf x 'a) 20)
    x)
  (a 20))

(deftest setf-getf.3
  (let (x)
    (setf (getf x 'a) 10)
    (setf (getf x 'b) 20)
    x)
  (b 20 a 10))

(deftest setf-getf.4
  (let (x y)
    (setf (getf x 'a (setq y 999)) 10)
    x)
  (a 10))

(deftest setf-getf.5
  (let (x y)
    (setf (getf x 'a (setq y 999)) 10)
    y)
  999)

(deftest setf-getf.6
  (let ((x '(a b c d e f g h)))
    (values
      (setf (getf x 'a) 10)
      (setf (getf x 'e) 20)
      x))
  10 20 (a 10 c d e 20 g h))

(deftest setf-getf.7
  (let ((x '(a b)))
    (values
      (incf (getf x 'z 10))
      x))
  11 (z 11 a b))

(deftest setf-getf.8
  (let ((x '(a b c d e)))
    (setf (getf (cdr x) 'd) 20)
    x)
  (a b c d 20))

(deftest-error setf-getf-error.1
  (eval '(setf (getf 10 20) 30)))

(deftest-error setf-getf-error.2
  (eval '(let (x) (setf (getf x 'a 10 20) 30))))

(deftest-error setf-getf-error.3
  (eval '(let (x) (setf (getf x) 30))))


;;
;;  Macro REMF
;;
(deftest remf.1
  (let (x)
    (remf x 'a))
  nil)

(deftest remf.2
  (let ((x '(a b c d e f)))
    (remf x 'a))
  t)

(deftest remf.3
  (let ((x '(a b c d e f)))
    (remf x 'a)
    x)
  (c d e f))

(deftest remf.4
  (let ((x '(a b c d e f)))
    (remf x 'c))
  t)

(deftest remf.5
  (let ((x '(a b c d e f)))
    (remf x 'c)
    x)
  (a b e f))

(deftest remf.6
  (let ((x '(a b c d e f)))
    (remf x 'z))
  nil)

(deftest remf.7
  (let ((x '(a b c d e f)))
    (remf x 'z)
    x)
  (a b c d e f))

(deftest remf.8
  (let ((x '(a b c d e)))
    (values
      (remf (cdr x) 'd)
      x))
  t (a b c))

(deftest remf.9
  (let ((x (cons () ())))
    (values
      (setf (getf (car x) 'prop1) 'val1)
      (remf (car x) 'prop1)
      (remf (car x) 'prop1)))
  val1 t nil)

(deftest-error remf-error.1
  (eval '(remf 10 20)))

(deftest-error remf-error.2
  (eval '(let (x) (remf x))))

(deftest-error remf-error.3
  (eval '(let (x) (remf x 20 30))))

