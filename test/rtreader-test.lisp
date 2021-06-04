;;
;;  ANSI COMMON LISP: 23. Reader
;;

;;
;;  Function COPY-READTABLE
;;
(deftest copy-readtable.1
  (readtablep
    (copy-readtable))
  t)

(deftest copy-readtable.2
  (eq (copy-readtable *readtable*)
      *readtable*)
  nil)

(deftest copy-readtable.3
  (let* ((x (copy-readtable))
         (y (copy-readtable))
         (z (copy-readtable x y)))
    (values
      (eq x y)
      (eq y z)))
  nil t)

(deftest copy-readtable.4
  (let ((x (copy-readtable))
        (call (lambda (stream value)
                (declare (ignore stream value))
                :hello)))
    (set-macro-character #\$ call nil x)
    (let ((y (copy-readtable x)))
      (values
        (eq (get-macro-character #\$ x)
            (get-macro-character #\$ y))
        (eq (get-macro-character #\$ y)
            (get-macro-character #\$ *readtable*)))))
  t nil)

(deftest copy-readtable.5
  (let ((x (copy-readtable))
        (call (lambda (stream char value)
                (declare (ignore stream char value))
                :hello)))
    (set-dispatch-macro-character #\# #\$ call x)
    (let ((y (copy-readtable x nil)))
      (values
        (eq (get-dispatch-macro-character #\# #\$ x)
            (get-dispatch-macro-character #\# #\$ y))
        (eq (get-dispatch-macro-character #\# #\$ y)
            (get-dispatch-macro-character #\# #\$ *readtable*)))))
  t nil)

(deftest copy-readtable.6
  (let ((x (copy-readtable))
        (y (copy-readtable))
        (call (lambda (stream value)
                (declare (ignore stream value))
                :hello)))
    (set-macro-character #\$ call nil x)
    (copy-readtable x y)
    (values
      (eq (get-macro-character #\$ x)
          (get-macro-character #\$ y))
      (eq (get-macro-character #\$ y)
          (get-macro-character #\$ *readtable*))))
  t nil)

(deftest copy-readtable.7
  (let ((x (copy-readtable))
        (y (copy-readtable))
        (call (lambda (stream char value)
                (declare (ignore stream char value))
                :hello)))
    (set-dispatch-macro-character #\# #\$ call x)
    (copy-readtable x y)
    (values
      (eq (get-dispatch-macro-character #\# #\$ x)
          (get-dispatch-macro-character #\# #\$ y))
      (eq (get-dispatch-macro-character #\# #\$ y)
          (get-dispatch-macro-character #\# #\$ *readtable*))))
  t nil)

(deftest copy-readtable.8
  (let ((x (copy-readtable)))
    (eq (get-macro-character #\# x)
        (get-macro-character #\# *readtable*)))
  t)

(deftest copy-readtable.9
  (let ((x (copy-readtable))
        (y (copy-readtable))
        (call (lambda (stream value)
                (declare (ignore stream value))
                :hello)))
    (set-macro-character #\$ call nil x)
    (let ((*readtable* x))
      (let ((z (copy-readtable nil)))
        (values
          (eq (get-macro-character #\$ x)
              (get-macro-character #\$ y))
          (eq (get-macro-character #\$ y)
              (get-macro-character #\$ z))))))
  nil t)

(deftest-error! copy-readtable-error.1
  (eval '(copy-readtable *readtable* nil nil)))

(deftest-error copy-readtable-error.2
  (eval '(copy-readtable 10))
  type-error)


;;
;;  Function READ
;;
(deftest read.1
  (with-input-from-string (*standard-input* ":hello")
    (read))
  :hello)

(deftest read.2
  (with-input-from-string (x "   :hello   ")
    (read x))
  :hello)

(deftest read.3
  (with-input-from-string (x ":hello 100")
    (values
      (read x)
      (read-char x)))
  :hello #\1)

(deftest-error read.4
  (with-input-from-string (x "   :hello   ")
    (read x)
    (read x))
  end-of-file)

(deftest-error read.5
  (with-input-from-string (x "   :hello   ")
    (read x t)
    (read x t))
  end-of-file)

(deftest read.6
  (with-input-from-string (x ":hello")
    (values
      (read x t)
      (read x nil)))
  :hello nil)

(deftest read.7
  (with-input-from-string (x ":hello   ")
    (values
      (read x nil 999)
      (read x nil 999)))
  :hello 999)

(deftest read.8
  (with-input-from-string (x ":hello   ")
    (values
      (read x nil 999)
      (read x nil 999)))
  :hello 999)

(deftest-error read.9
  (with-input-from-string (x ":hello")
    (read x nil 999 t)))

(deftest-error! read-error.1
  (eval '(read *standard-input* nil nil nil nil)))

(deftest-error read-error.2
  (eval '(read 10))
  type-error)


;;
;;  Function READ-PRESERVING-WHITESPACE
;;
(deftest read-preserving-whitespace.1
  (with-input-from-string (*standard-input* ":hello")
    (read-preserving-whitespace))
  :hello)

(deftest read-preserving-whitespace.2
  (with-input-from-string (x "   :hello   ")
    (read-preserving-whitespace x))
  :hello)

(deftest read-preserving-whitespace.3
  (with-input-from-string (x ":hello 100")
    (values
      (read-preserving-whitespace x)
      (read-char x)))
  :hello #\Space)

(deftest-error read-preserving-whitespace.4
  (with-input-from-string (x "   :hello   ")
    (read-preserving-whitespace x)
    (read-preserving-whitespace x))
  end-of-file)

(deftest-error read-preserving-whitespace.5
  (with-input-from-string (x "   :hello   ")
    (read-preserving-whitespace x t)
    (read-preserving-whitespace x t))
  end-of-file)

(deftest read-preserving-whitespace.6
  (with-input-from-string (x ":hello")
    (values
      (read-preserving-whitespace x t)
      (read-preserving-whitespace x nil)))
  :hello nil)

(deftest read-preserving-whitespace.7
  (with-input-from-string (x ":hello   ")
    (values
      (read-preserving-whitespace x nil 999)
      (read-preserving-whitespace x nil 999)))
  :hello 999)

(deftest read-preserving-whitespace.8
  (with-input-from-string (x ":hello   ")
    (values
      (read-preserving-whitespace x nil 999)
      (read-preserving-whitespace x nil 999)))
  :hello 999)

(deftest-error read-preserving-whitespace.9
  (with-input-from-string (x ":hello")
    (read-preserving-whitespace x nil 999 t)))

(deftest-error! read-preserving-whitespace-error.1
  (eval '(read-preserving-whitespace *standard-input* nil nil nil nil)))

(deftest-error read-preserving-whitespace-error.2
  (eval '(read-preserving-whitespace 10))
  type-error)

;;  ANSI Common Lisp
(deftest read-test.1
  (with-input-from-string (*standard-input* "'a")
    (read))
  (quote a))

(deftest read-test.2
  (with-input-from-string (is " ")
    (read is nil 'the-end))
  the-end)

(defun read-test-skip-then-read-char (s c n)
  (declare (ignore n))
  (if (char= c #\{)
    (read s t nil t)
    (read-preserving-whitespace s))
  (read-char-no-hang s))

(deftest read-test.3
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\{ #'read-test-skip-then-read-char)
    (set-dispatch-macro-character #\# #\} #'read-test-skip-then-read-char)
    (with-input-from-string (is "#{123 x #}123 y")
      (format nil "~S ~S" (read is) (read is))))
  "#\\x #\\Space")


;;
;;  Function READ-DELIMITED-LIST
;;
(deftest read-delimited-list.1
  (with-input-from-string (*standard-input* "10 20 30 40)")
    (read-delimited-list #\)))
  (10 20 30 40))

(deftest read-delimited-list.2
  (with-input-from-string (x "    10 20 30 40   +++")
    (read-delimited-list #\+ x))
  (10 20 30 40))

(deftest-error read-delimited-list.3
  (with-input-from-string (x "    10 20 30 40   +++")
    (read-delimited-list #\+ x t)))

(deftest read-delimited-list.4
  (with-input-from-string (x "10 20 30 40)abcde")
    (values
      (read-delimited-list #\) x)
      (read-char x)))
  (10 20 30 40) #\a)

(deftest-error read-delimited-list.5
  (with-input-from-string (x "")
    (read-delimited-list #\+ x))
  end-of-file)

(deftest read-delimited-list.6
  (with-input-from-string (x "+")
    (read-delimited-list #\+ x))
  nil)

(deftest-error! read-delimited-list-error.1
  (eval '(read-delimited-list)))

(deftest-error! read-delimited-list-error.2
  (eval '(read-delimited-list #\) *standard-input* nil nil)))

(deftest-error! read-delimited-list-error.3
  (eval '(read-delimited-list 10))
  type-error)


;;
;;  Function READ-FROM-STRING
;;
(deftest read-from-string.1
  (read-from-string "123")
  123 3)

(deftest read-from-string.2
  (read-from-string "  hello   ")
  hello 8)

(deftest-error read-from-string.3
  (read-from-string "  ")
  end-of-file)

(deftest read-from-string.4
  (read-from-string "    " nil)
  nil 4)

(deftest read-from-string.5
  (read-from-string "" nil 999)
  999 0)

(deftest read-from-string-start.1
  (read-from-string "1234567  " nil nil :start 0)
  1234567 8)

(deftest read-from-string-start.2
  (read-from-string "1234567  " nil nil :start 4)
  567 8)

(deftest read-from-string-start.3
  (read-from-string "1234567  " nil nil :start 9)
  nil 9)

(deftest-error read-from-string-start.4
  (read-from-string "1234567  " nil nil :start 10))

(deftest read-from-string-end.1
  (read-from-string "1234567  " nil nil :end nil)
  1234567 8)

(deftest read-from-string-end.2
  (read-from-string "1234567  " nil nil :end 0)
  nil 0)

(deftest read-from-string-end.3
  (read-from-string "1234567  " nil nil :end 4)
  1234 4)

(deftest read-from-string-end.4
  (read-from-string "1234567  " nil nil :end 9)
  1234567 8)

(deftest read-from-string-end.5
  (read-from-string "1234567" nil nil :end 7)
  1234567 7)

(deftest-error read-from-string-end.6
  (read-from-string "1234567" nil nil :end 8))

(deftest read-from-string-start-end.1
  (read-from-string "1234567" nil nil :start 3 :end 5)
  45 5)

(deftest read-from-string-start-end.2
  (read-from-string "1234567" nil nil :start 3 :end 3)
  nil 3)

(deftest-error read-from-string-start-end.3
  (read-from-string "1234567" nil nil :start 3 :end 2))

(deftest read-from-string-preserve-whitespace.1
  (read-from-string " 123 456 789 " nil nil :preserve-whitespace nil)
  123 5)

(deftest read-from-string-preserve-whitespace.2
  (read-from-string " 123 456 789 " nil nil :preserve-whitespace t)
  123 4)

(deftest-error! read-from-string-error.1
  (eval '(read-from-string)))

(deftest-error read-from-string-error.2
  (eval '(read-from-string "Hello" nil nil :hello)))

(deftest-error read-from-string-error.3
  (eval '(read-from-string "Hello" nil nil :hello 10)))

;;  ANSI Common Lisp
(deftest read-from-string-test.1
  (read-from-string " 1 3 5" t nil :start 2)
  3 5)

(deftest read-from-string-test.2
  (read-from-string "(a b c)")
  (a b c) 7)


;;
;;  Accessor READTABLE-CASE
;;
(deftest readtable-case.1
  (readtable-case *readtable*)
  :upcase)

(deftest readtable-case.2
  (let ((x (copy-readtable nil)))
    (readtable-case x))
  :upcase)

(deftest readtable-case.3
  (let ((x (copy-readtable nil)))
    (setf (readtable-case x) :downcase)
    (readtable-case x))
  :downcase)

(deftest readtable-case.4
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (values
      (read-from-string "HelLo")
      (readtable-case *readtable*)))
  hello :upcase)

(deftest readtable-case.5
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (values
      (read-from-string "HelLo")
      (readtable-case *readtable*)))
  |hello| :downcase)

(deftest readtable-case.6
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (values
      (read-from-string "HelLo")
      (readtable-case *readtable*)))
  |HelLo| :preserve)

(deftest readtable-case.7
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (values
      (read-from-string "HelLo")
      (readtable-case *readtable*)))
  |hELlO| :invert)

(deftest-error! readtable-case-error.1
  (eval '(readtable-case)))

(deftest-error! readtable-case-error.2
  (eval '(readtable-case *readtable* nil)))

(deftest-error readtable-case-error.3
  (eval '(readtable-case 10))
  type-error)


;;
;;  Accessor (SETF READTABLE-CASE)
;;
(deftest readtable-case-setf.1
  (let ((x (copy-readtable nil)))
    (setf (readtable-case x) :preserve))
  :preserve)

(deftest readtable-case-setf.2
  (let ((x (copy-readtable nil)))
    (setf (readtable-case x) :preserve)
    (readtable-case x))
  :preserve)

(deftest-error readtable-case-setf-error.1
  (eval '(setf (readtable-case) :preserve)))

(deftest-error readtable-case-setf-error.2
  (eval '(let ((x (copy-readtable nil)))
           (setf (readtable-case x nil) :preserve))))

(deftest-error readtable-case-setf-error.3
  (eval '(let ((x (copy-readtable nil)))
           (setf (readtable-case x) :hello))))


;;
;;  error
;;

;;  read colon
(defpackage read-intern-colon (:use cl)
  (:export aaa bbb ccc ddd))

(deftest read-intern-colon.1
  (eq (intern "BBB" "KEYWORD")
      (read-from-string ":bbb"))
  t)

(deftest read-intern-colon.2
  (eq (intern "BBB" "READ-INTERN-COLON")
      (read-from-string "read-intern-colon::bbb"))
  t)

(deftest read-intern-colon.3
  (eq (intern "BBB" "READ-INTERN-COLON")
      (read-from-string "read-intern-colon:bbb"))
  t)

(deftest read-intern-colon.4
  (eq (intern "EEE" "READ-INTERN-COLON")
      (read-from-string "read-intern-colon::eee"))
  t)

(deftest-error read-intern-colon.5
  (read-from-string "read-intern-colon:eee"))

(deftest read-intern-colon.6
  (let ((*package* (find-package 'read-intern-colon)))
    (eq (intern "EEE" "READ-INTERN-COLON")
        (read-from-string "read-intern-colon::eee")))
  t)

;;  double backquote
(deftest reader-quote-error.1
  (progn
    (read-from-string "`(block `(lambda 10))")
    (values)))


;;
;;  reader export
;;
(defpackage reader-export-1-a)
(defpackage reader-export-1-b)
(deftest reader-export.1
  (progn
    (intern "HELLO" 'reader-export-1-a)
    (use-package 'reader-export-1-b 'reader-export-1-a)
    (import (intern "HELLO" 'reader-export-1-a) 'reader-export-1-b)
    (export (intern "HELLO" 'reader-export-1-b) 'reader-export-1-b)
    (let ((x (read-from-string "reader-export-1-b:hello")))
      (values (package-name
                (symbol-package x))
              (symbol-name x))))
  "READER-EXPORT-1-A" "HELLO")

(defpackage reader-export-2-a)
(defpackage reader-export-2-b)
(deftest reader-export.2
  (progn
    (intern "HELLO" 'reader-export-2-a)
    (let ((x (intern "HELLO" 'reader-export-2-a)))
      (export x 'reader-export-2-a))
    (use-package 'reader-export-2-b 'reader-export-2-a)
    (import (intern "HELLO" 'reader-export-2-a) 'reader-export-2-b)
    (export (intern "HELLO" 'reader-export-2-b) 'reader-export-2-b)
    (let ((x (read-from-string "reader-export-2-b:hello")))
      (values (package-name
                (symbol-package x))
              (symbol-name x))))
  "READER-EXPORT-2-A" "HELLO")


(defpackage reader-export-3-a)
(defpackage reader-export-3-b)
(deftest reader-export.3
  (progn
    (intern "HELLO" 'reader-export-3-a)
    (import (intern "HELLO" 'reader-export-3-a) 'reader-export-3-b)
    (export (intern "HELLO" 'reader-export-3-b) 'reader-export-3-b)
    (let ((x (read-from-string "reader-export-3-b:hello")))
      (values (package-name
                (symbol-package x))
              (symbol-name x))))
  "READER-EXPORT-3-A" "HELLO")

