;;
;;  ANSI COMMON LISP: 16. Strings
;;

;;
;;  System Class STRING
;;
(deftest string-type.1
  (lisp-system:closp
    (find-class 'string))
  t)

(deftest string-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'string)))
  (string vector array sequence t))

(deftest string-type.3
  (typep "Hello" 'string)
  t)

(deftest string-type.4
  (typep "Hello" '(string *))
  t)

(deftest string-type.5
  (typep "Hello" '(string 5))
  t)

(deftest string-type.6
  (typep "Hello" '(string 4))
  nil)

(deftest string-type.7
  (subtypep '(string 5) '(vector character 5))
  t t)

(deftest string-type.8
  (subtypep '(vector character 5) '(string 5))
  t t)

(deftest string-type.9
  (typep :hello 'string)
  nil)


;;
;;  Type BASE-STRING
;;
(deftest base-string-type.1
  (lisp-system:closp
    (find-class 'base-string))
  t)

(deftest base-string-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'base-string)))
  (base-string string vector array sequence t))

(deftest base-string-type.3
  (subtypep 'base-string 'string)
  t t)

(deftest base-string-type.4
  (typep "Hello" 'base-string)
  t)

(deftest base-string-type.5
  (typep "Hello" '(base-string *))
  t)

(deftest base-string-type.6
  (typep "Hello" '(base-string 5))
  t)

(deftest base-string-type.7
  (typep "Hello" '(base-string 4))
  nil)

(deftest base-string-type.8
  (subtypep '(base-string 5) '(vector base-char 5))
  t t)

(deftest base-string-type.9
  (subtypep '(vector base-char 5) '(base-string 5))
  t t)

(deftest base-string-type.10
  (typep 10 'base-string)
  nil)


;;
;;  Type SIMPLE-STRING
;;
(deftest simple-string-type.1
  (lisp-system:closp
    (find-class 'simple-string))
  t)

(deftest simple-string-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-string)))
  (simple-string string vector simple-array array sequence t))

(deftest simple-string-type.3
  (subtypep 'simple-string 'string)
  t t)

(deftest simple-string-type.4
  (typep "Hello" 'simple-string)
  t)

(deftest simple-string-type.5
  (typep "Hello" '(simple-string *))
  t)

(deftest simple-string-type.6
  (typep "Hello" '(simple-string 5))
  t)

(deftest simple-string-type.7
  (typep "Hello" '(simple-string 4))
  nil)

(deftest simple-string-type.8
  (subtypep '(simple-string 5) '(simple-array character (5)))
  t t)

(deftest simple-string-type.9
  (subtypep '(simple-array character (5)) '(simple-string 5))
  t t)

(deftest simple-string-type.10
  (typep 10 'simple-string)
  nil)

(deftest simple-string-type.11
  (typep (make-array 5 :element-type 'character
                     :initial-contents "Hello")
         'simple-string)
  t)

(deftest simple-string-type.12
  (typep (make-array 5 :element-type 'character
                     :initial-contents "Hello"
                     :adjustable t)
         'simple-string)
  nil)


;;
;;  Type SIMPLE-BASE-STRING
;;
(deftest simple-base-string-type.1
  (lisp-system:closp
    (find-class 'simple-base-string))
  t)

(deftest simple-base-string-type.2
  (mapcar #'class-name
          (lisp-clos:class-precedence-list
            (find-class 'simple-base-string)))
  (simple-base-string
    base-string simple-string
    string vector simple-array array sequence t))

(deftest simple-base-string-type.3
  (subtypep 'simple-base-string 'string)
  t t)

(deftest simple-base-string-type.4
  (typep "Hello" 'simple-base-string)
  t)

(deftest simple-base-string-type.5
  (typep "Hello" '(simple-base-string *))
  t)

(deftest simple-base-string-type.6
  (typep "Hello" '(simple-base-string 5))
  t)

(deftest simple-base-string-type.7
  (typep "Hello" '(simple-base-string 4))
  nil)

(deftest simple-base-string-type.8
  (subtypep '(simple-base-string 5) '(simple-array base-char (5)))
  t t)

(deftest simple-base-string-type.9
  (subtypep '(simple-array base-char (5)) '(simple-base-string 5))
  t t)

(deftest simple-base-string-type.10
  (typep 10 'simple-base-string)
  nil)

(deftest simple-base-string-type.11
  (typep (make-array 5 :element-type 'base-char
                     :initial-contents "Hello")
         'simple-base-string)
  t)

(deftest simple-base-string-type.12
  (typep (make-array 5 :element-type 'base-char
                     :initial-contents "Hello"
                     :adjustable t)
         'simple-base-string)
  nil)


;;
;;  Function SIMPLE-STRING-P
;;
(deftest simple-string-p.1
  (simple-string-p "Hello")
  t)

(deftest simple-string-p.2
  (simple-string-p
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  t)

(deftest simple-string-p.3
  (simple-string-p
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable t
      :displaced-to nil))
  nil)

(deftest simple-string-p.4
  (simple-string-p
    (make-array
      10
      :element-type 't
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  nil)

(deftest simple-string-p.5
  (simple-string-p t)
  nil)

(deftest-error! simple-string-p-error.1
  (eval '(simple-string-p)))

(deftest-error! simple-string-p-error.2
  (eval '(simple-string-p 10 20)))

;;  ANSI Common Lisp
(deftest simple-string-p-test.1
  (simple-string-p "aaaaaa")
  t)

(deftest simple-string-p-test.2
  (simple-string-p (make-array 6 :element-type 'character :fill-pointer t))
  nil)


;;
;;  Function STRINGP
;;
(deftest stringp.1
  (stringp "Hello")
  t)

(deftest stringp.2
  (stringp
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  t)

(deftest stringp.3
  (stringp
    (make-array
      10
      :element-type 'character
      :fill-pointer nil
      :adjustable t
      :displaced-to nil))
  t)

(deftest stringp.4
  (stringp
    (make-array
      10
      :element-type 't
      :fill-pointer nil
      :adjustable nil
      :displaced-to nil))
  nil)

(deftest stringp.5
  (stringp t)
  nil)

(deftest-error! stringp-error.1
  (eval '(stringp)))

(deftest-error! stringp-error.2
  (eval '(stringp 10 20)))

;;  ANSI Common Lisp
(deftest stringp-test.1
  (stringp "aaaaaa")
  t)

(deftest stringp-test.2
  (stringp #\a)
  nil)

