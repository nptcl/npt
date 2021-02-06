;;
;;  ANSI COMMON LISP: 6. Iteration
;;

;;
;;  Macro DO
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

(deftest do.5
  (do ((a 10 (1+ a)))
    ((eql a 20) a)
    (declare (type integer a))
    :hello)
  20)

(deftest-error do.6
  (eval '(do ((a 10 (1+ a)))
           ((eql a 20) a)
           (declare (type string a))
           :hello))
  type-error)

(deftest-error do.7
  (eval '(do ((a "Hello" 10))
           ((eql a 10) a)
           (declare (type string a))))
  type-error)

(deftest do.8
  (do (a) (t (setq a 10)
             (incf a 20)
             a))
  30)

(deftest do.9
  (do ((a 10))
    ((< 30 a) a)
    (incf a 10)
    (incf a 10)
    (incf a 10)
    (incf a 10))
  50)

(deftest do.10
  (do ((a 10 (1+ a))
       (b 0 (1+ a))
       (c 0 (1+ b)))
    ((< 0 c) (values a b c)))
  11 11 1)

(deftest do.11
  (do ((x 0) list) (nil)
    (unless (< x 5)
      (return (nreverse list)))
    (push x list)
    (incf x))
  (0 1 2 3 4))

(deftest do.12
  (do ((x 0) list) (nil :hello)
    (when (< x 5)
      (go next))
    (return (nreverse list))
    next
    (push x list)
    (incf x))
  (0 1 2 3 4))

(deftest-error do-error.1
  (eval '(do ())))

(deftest-error do-error.2
  (eval '(do (10) (t))))

(deftest-error do-error.3
  (eval '(do () ())))

(deftest-error do-error.4
  (eval '(do ((a 10 20 30)) ())))


;;
;;  Macro DO*
;;
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

(deftest do*.5
  (do* ((a 10 (1+ a)))
    ((eql a 20) a)
    (declare (type integer a))
    :hello)
  20)

(deftest-error do*.6
  (eval '(do* ((a 10 (1+ a)))
           ((eql a 20) a)
           (declare (type string a))
           :hello))
  type-error)

(deftest-error do*.7
  (eval '(do* ((a "Hello" 10))
           ((eql a 10) a)
           (declare (type string a))))
  type-error)

(deftest do*.8
  (do* (a) (t (setq a 10)
              (incf a 20)
              a))
  30)

(deftest do*.9
  (do* ((a 10))
    ((< 30 a) a)
    (incf a 10)
    (incf a 10)
    (incf a 10)
    (incf a 10))
  50)

(deftest do*.10
  (do* ((a 10 (1+ a))
        (b 0 (1+ a))
        (c 0 (1+ b)))
    ((< 0 c) (values a b c)))
  11 12 13)

(deftest do*.11
  (do* ((x 0) list) (nil)
    (unless (< x 5)
      (return (nreverse list)))
    (push x list)
    (incf x))
  (0 1 2 3 4))

(deftest do*.12
  (do* ((x 0) list) (nil :hello)
    (when (< x 5)
      (go next))
    (return (nreverse list))
    next
    (push x list)
    (incf x))
  (0 1 2 3 4))

(deftest-error do*-error.1
  (eval '(do* ())))

(deftest-error do*-error.2
  (eval '(do* (10) (t))))

(deftest-error do*-error.3
  (eval '(do* () ())))

(deftest-error do*-error.4
  (eval '(do* ((a 10 20 30)) ())))

;;  ANSI Common Lisp
(deftest do-test.1
  (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
    ((> (- temp-one temp-two) 5) temp-one))
  4)

(deftest do-test.2
  (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1+ temp-one)))
    ((= 3 temp-two) temp-one))
  3)

(deftest do-test.3
  (do* ((temp-one 1 (1+ temp-one))
        (temp-two 0 (1+ temp-one)))
    ((= 3 temp-two) temp-one))
  2)

(deftest do-test.4
  (let ((input '(banana (57 boxes) nil))
        list)
    (values
      (do ((j 0 (+ j 1)))
        (nil)
        (push (format nil "Input ~D:" j) list)
        (let ((item (pop input)))
          (if (null item) (return)
            (push (format nil "Output ~D: ~S" j item) list))))
      (nreverse list)))
  nil
  ("Input 0:"
   "Output 0: BANANA"
   "Input 1:"
   "Output 1: (57 BOXES)"
   "Input 2:"))

(deftest do-test.5
  (let ((a-vector (vector 1 nil 3 nil)))
    (values
      (do ((i 0 (+ i 1))
           (n (array-dimension a-vector 0)))
        ((= i n))
        (when (null (aref a-vector i))
          (setf (aref a-vector i) 0)))
      a-vector))
  nil #(1 0 3 0))


;;
;;  Macro DOTIMES
;;
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

(deftest dotimes.7
  (dotimes (i 10)
    (return 999))
  999)

(deftest dotimes.8
  (let (a)
    (dotimes (i 10)
      (go next)
      (setq a 10)
      next
      (setq a 20)
      (return a)))
  20)

(deftest dotimes.9
  (let (list)
    (dotimes (i 3 list)
      (declare (type integer i))
      (push i list)))
  (2 1 0))

(deftest-error dotimes.10
  (eval '(let (list)
           (dotimes (i 3 list)
             (declare (type string i))
             (push i list))))
  type-error)

(deftest-error dotimes-error.1
  (eval '(dotimes (i))))

(deftest-error dotimes-error.2
  (eval '(dotimes (i 20 nil nil))))

(deftest-error dotimes-error.3
  (eval '(dotimes)))

;;  ANSI Common Lisp
(deftest dotimes-test.1
  (dotimes (temp-one 10 temp-one))
  10)

(deftest dotimes-test.2
  (let ((temp-two 0))
    (values
      (dotimes (temp-one 10 t) (incf temp-two))
      temp-two))
  t 10)

;;; True if the specified subsequence of the string is a
;;; palindrome (reads the same forwards and backwards).
(defun dotimes-test-palindromep (string &optional
                                        (start 0)
                                        (end (length string)))
  (dotimes (k (floor (- end start) 2) t)
    (unless (char-equal (char string (+ start k))
                        (char string (- end k 1)))
      (return nil))))

(deftest dotimes-test.3
  (dotimes-test-palindromep "Able was I ere I saw Elba")
  t)

(deftest dotimes-test.4
  (dotimes-test-palindromep "A man, a plan, a canal--Panama!")
  nil)

(deftest dotimes-test.5
  (remove-if-not #'alpha-char-p
                 "A man, a plan, a canal--Panama!")
  "AmanaplanacanalPanama")

(deftest dotimes-test.6
  (dotimes-test-palindromep
    (remove-if-not #'alpha-char-p
                   "A man, a plan, a canal--Panama!"))
  t)

(deftest dotimes-test.7
  (dotimes-test-palindromep
    (remove-if-not
      #'alpha-char-p
      "Unremarkable was I ere I saw Elba Kramer, nu?"))
  t)

(deftest dotimes-test.8
  (dotimes-test-palindromep
    (remove-if-not
      #'alpha-char-p
      #.(format nil "~A~%~A"
                "A man, a plan, a cat, a ham, a yak,"
                "a yam, a hat, a canal--Panama!")))
  t)


;;
;;  Macro DOLIST
;;
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

(deftest dolist.7
  (dolist (i '(a b c))
    (return 999))
  999)

(deftest dolist.8
  (let (a)
    (dolist (i '(a b c) :hello)
      (go next)
      (setq a 10)
      next
      (setq a 20)
      (return a)))
  20)

(deftest dolist.9
  (dolist (i '(10 20 30))
    (declare (type integer i))
    (return i))
  10)

(deftest-error dolist.10
  (eval '(dolist (i '(10 20 30))
           (declare (type string i))
           (return i)))
  type-error)

(deftest-error dolist.11
  (eval '(dolist (i nil)
           (declare (type integer i))
           (return i)))
  type-error)

(deftest dolist.12
  (dolist (*print-case* '(:upcase :downcase :capitalize))
    :test)
  nil)

(deftest-error dolist-error.1
  (eval '(dolist (i))))

(deftest-error dolist-error.2
  (eval '(dolist (i 20 nil nil))))

(deftest-error dolist-error.3
  (eval '(dolist)))

;;  ANSI Common Lisp
(deftest dolist-test.1
  (let ((temp-two '()))
    (dolist (temp-one '(1 2 3 4) temp-two)
      (push temp-one temp-two)))
  (4 3 2 1))

(deftest dolist-test.2
  (let ((temp-two 0))
    (values
      (dolist (temp-one '(1 2 3 4))
        (incf temp-two))
      temp-two))
  nil 4)

(deftest dolist-test.3
  (with-output-to-string (stream)
    (dolist (x '(a b c d))
      (prin1 x stream)
      (princ " " stream)))
  "A B C D ")


;;
;;  Macro LOOP
;;
(deftest loop.1
  (loop (return 10))
  10)

(deftest loop.2
  (loop for n from 1 to 10
        when (oddp n)
        collect n)
  (1 3 5 7 9))

(deftest-error loop.3
  (eval '(loop (loop-finish))))


;;
;;  Local Macro LOOP-FINISH
;;
(deftest loop-finish.1
  (loop for i in '(1 2 3)
        do (loop-finish))
  nil)

(deftest-error loop-finish.2
  (eval '(loop for i in '(1 2 3)
               do (loop-finish 10))))

(deftest loop-finish-test.1
  (loop for i in '(1 2 3 stop-here 4 5 6)
        when (symbolp i) do (loop-finish)
        count i)
  3)

(deftest loop-finish-test.2
  (loop for i in '(1 2 3 stop-here 4 5 6)
        until (symbolp i)
        count i)
  3)

(defun loop-test-tokenize-sentence (string)
  (macrolet ((add-word (wvar svar)
                       `(when ,wvar
                          (push (coerce (nreverse ,wvar) 'string) ,svar)
                          (setq ,wvar nil))))
    (loop with word = '() and sentence = '() and endpos = nil
          for i below (length string)
          do (let ((char (aref string i)))
               (case char
                 (#\Space (add-word word sentence))
                 (#\. (setq endpos (1+ i)) (loop-finish))
                 (otherwise (push char word))))
          finally (add-word word sentence)
          (return (values (nreverse sentence) endpos)))))

(deftest loop-finish-test.3
  (loop-test-tokenize-sentence "this is a sentence. this is another sentence.")
  ("this" "is" "a" "sentence") 19)

(deftest loop-finish-test.4
  (loop-test-tokenize-sentence "this is a sentence")
  ("this" "is" "a" "sentence") nil)

