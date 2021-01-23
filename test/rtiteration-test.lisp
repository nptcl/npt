;;
;;  ANSI COMMON LISP: 6. Iteration
;;
(deftest do.1
  (do () (t))
  nil)

(deftest do.2
  (do (a)
    (a a)
    (setq a 100))
  100)

(deftest do.3
  (let ((a 10))
    (do ((a 20)
         (b a))
      (t (values a b))))
  20 10)

(deftest do.4
  (do ((a 10 b)
       (b 20 a)
       c)
    (c (values a b))
    (setq c t))
  20 10)

(deftest do*.1
  (do* () (t))
  nil)

(deftest do*.2
  (do* (a)
    (a a)
    (setq a 100))
  100)

(deftest do*.3
  (let ((a 10))
    (declare (ignorable a))
    (do* ((a 20)
          (b a))
      (t (values a b))))
  20 20)

(deftest do*.4
  (do* ((a 10 b)
        (b 20 a)
        c)
    (c (values a b))
    (setq c t))
  20 20)

(deftest dotimes.1
  (dotimes (i 0))
  nil)

(deftest dotimes.2
  (dotimes (i 0 i))
  0)

(deftest dotimes.3
  (dotimes (i 4 (values 10 20)))
  10 20)

(deftest dotimes.4
  (let (a)
    (dotimes (i 0)
      (setq a i))
    a)
  nil)

(deftest dotimes.5
  (let (a)
    (dotimes (i 3 a)
      (push i a)))
  (2 1 0))

(deftest dotimes.6
  (let (a)
    (dotimes (i -1 a)
      (push i a)))
  nil)

(deftest dolist.1
  (dolist (i nil))
  nil)

(deftest dolist.2
  (dolist (i nil i))
  nil)

(deftest dolist.3
  (dolist (i nil 10))
  10)

(deftest dolist.4
  (dolist (i '(a b c) (values 10 20)))
  10 20)

(deftest dolist.5
  (let (a)
    (dolist (i nil)
      (setq a 100))
    a)
  nil)

(deftest dolist.6
  (let (a)
    (dolist (i '(a b c) a)
      (push i a)))
  (c b a))

'(deftest dolist.7
   (dolist (*print-case* '(:upcase :downcase :capitalize))
     :test))

