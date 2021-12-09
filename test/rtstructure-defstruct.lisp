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
      (aaa 100) (bbb 0))
    (make-vector-type-10))
  #(100 0))

(deftest-error vector-type.11
  (progn
    (defstruct (vector-type-10 (:type (vector (unsigned-byte 16))))
      (aaa 100) bbb)
    (make-vector-type-10)))


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


;;
;;  ANSI Common Lisp
;;
(deftest defstruct-test.1
  (defstruct defstruct-person (name 007 :type string))
  defstruct-person)

(deftest defstruct-test.2
  (let ((x (make-defstruct-person :name "James")))
    (defstruct-person-name x))
  "James")

(deftest-error defstruct-test.3
  (let ((x (make-defstruct-person)))
    (defstruct-person-name x)))

(deftest defstruct-gensym.1
  (progn
    (defstruct defstructure-gensym-1
      (aaa (gensym)))
    (let ((x (make-defstructure-gensym-1))
          (y (make-defstructure-gensym-1)))
      (eq (defstructure-gensym-1-aaa x)
          (defstructure-gensym-1-aaa y))))
  nil)

(deftest defstruct-ship.1
  (progn
    (defvar *defstruct-ship* 1.23)
    (defstruct defstruct-ship
      (x-position 0.0 :type short-float)
      (y-position 0.0 :type short-float)
      (x-velocity 0.0 :type short-float)
      (y-velocity 0.0 :type short-float)
      (mass *defstruct-ship* :type short-float :read-only t)))
  defstruct-ship)

(deftest defstruct-ship.2
  (let ((x (make-defstruct-ship)))
    (defstruct-ship-mass x))
  1.23)

(deftest-error defstruct-ship.3
  (eval '(let ((x (make-defstruct-ship)))
           (setf (defstruct-ship-mass x) 100.0))))

(deftest defstruct-door.1
  (defstruct (defstruct-door (:conc-name defstruct-dr-))
    knob-color width material)
  defstruct-door)

(deftest defstruct-door.2
  (let ((my-door (make-defstruct-door :knob-color 'red :width 5.0)))
    (values
      (defstruct-dr-width my-door)
      (setf (defstruct-dr-width my-door) 43.7)
      (defstruct-dr-width my-door)))
  5.0 43.7 43.7)

(deftest defstruct-astronaut.1
  (defstruct defstruct-astro-person
    name age sex)
  defstruct-astro-person)

(deftest defstruct-astronaut.2
  (defstruct (defstruct-astronaut
               (:include defstruct-astro-person)
               (:conc-name defstruct-astro-))
    helmet-size
    (favorite-beverage 'tang))
  defstruct-astronaut)

(deftest defstruct-astronaut.3
  (let ((x (make-defstruct-astronaut
             :name 'buzz
             :age 45.
             :sex t
             :helmet-size 17.5)))
    (values
      (defstruct-astro-person-name x)
      (defstruct-astro-name x)
      (defstruct-astro-favorite-beverage x)
      (reduce #'+
              (list (make-defstruct-astronaut :age 10.)
                    (make-defstruct-astronaut :age 20.)
                    (make-defstruct-astronaut :age 30.))
              :key #'defstruct-astro-person-age)))
  buzz buzz tang 60)

(deftest defstruct-astronaut.4
  (let ((x (make-defstruct-astronaut :name 'aaa)))
    (defstruct-astro-name x))
  aaa)

(deftest-error defstruct-astronaut.5
  (let ((x (make-defstruct-astro-person :name 'aaa)))
    (defstruct-astro-name x)))

(deftest defstruct-astronaut.6
  (let ((x (make-defstruct-astronaut :name 'aaa)))
    (defstruct-astro-person-name x))
  aaa)

(deftest defstruct-astronaut.7
  (let ((x (make-defstruct-astro-person :name 'aaa)))
    (defstruct-astro-person-name x))
  aaa)

(deftest defstruct-astronaut.8
  (progn
    (defstruct (defstruct-astronaut2
                 (:include defstruct-astro-person (age 45)))
      helmet-size
      (favorite-beverage 'tang))
    (let ((x (make-defstruct-astronaut2)))
      (values
        (defstruct-astronaut2-age x)
        (defstruct-astronaut2-helmet-size x)
        (defstruct-astronaut2-favorite-beverage x))))
  45 nil tang)

(deftest defstruct-typep.1
  (progn
    (defstruct defstruct-typep-1)
    (typep (make-defstruct-typep-1) 'defstruct-typep-1))
  t)

(deftest defstruct-typep.2
  (progn
    (defstruct (defstruct-typep-2 (:include defstruct-typep-1)))
    (let ((x (make-defstruct-typep-1))
          (y (make-defstruct-typep-2)))
      (values
        (typep x 'defstruct-typep-1)
        (typep x 'defstruct-typep-2)
        (typep y 'defstruct-typep-1)
        (typep y 'defstruct-typep-2))))
  t nil t t)

(deftest defstruct-include-initform.1
  (progn
    (defstruct defstruct-include-initform-1
      (aaa 10) (bbb 20))
    (defstruct (defstruct-include-initform-2
                 (:include defstruct-include-initform-1 aaa)))
    (let ((x (make-defstruct-include-initform-2)))
      (values
        (defstruct-include-initform-1-aaa x)
        (defstruct-include-initform-2-aaa x)
        (defstruct-include-initform-1-bbb x)
        (defstruct-include-initform-2-bbb x))))
  nil nil 20 20)

(deftest defstruct-include-initform.2
  (progn
    (defstruct defstruct-include-initform-3
      (aaa 10) (bbb 20))
    (defstruct (defstruct-include-initform-4
                 (:include defstruct-include-initform-3 (aaa 30))))
    (let ((x (make-defstruct-include-initform-4)))
      (values
        (defstruct-include-initform-3-aaa x)
        (defstruct-include-initform-4-aaa x)
        (defstruct-include-initform-3-bbb x)
        (defstruct-include-initform-4-bbb x))))
  30 30 20 20)

(deftest defstruct-binop.1
  (defstruct (defstruct-binop (:type list) :named (:initial-offset 2))
    (operator '? :type symbol)
    operand-1
    operand-2)
  defstruct-binop)

(deftest defstruct-binop.2
  (defstruct (defstruct-annotated-binop
               (:type list)
               (:initial-offset 3)
               (:include defstruct-binop))
    commutative associative identity)
  defstruct-annotated-binop)

(deftest defstruct-binop.3
  (make-defstruct-annotated-binop
    :operator '*
    :operand-1 'x
    :operand-2 5
    :commutative t
    :associative t
    :identity 1)
  (nil nil defstruct-binop * x 5 nil nil nil t t 1))

(deftest defstruct-binop.4
  (defstruct (defstruct-binop-2 (:type list) (:initial-offset 2))
    (operator '? :type symbol)
    operand-1
    operand-2)
  defstruct-binop-2)

(deftest defstruct-binop.5
  (make-defstruct-binop-2 :operator '+ :operand-1 'x :operand-2 5)
  (nil nil + x 5))

(deftest defstruct-binop.6
  (make-defstruct-binop-2 :operand-2 4 :operator '*)
  (nil nil * nil 4))

(deftest defstruct-binop.7
  (defstruct (defstruct-binop-3 (:type list) :named (:initial-offset 2))
    (operator '? :type symbol)
    operand-1
    operand-2)
  defstruct-binop-3)

(deftest defstruct-binop.8
  (make-defstruct-binop-3 :operator '+ :operand-1 'x :operand-2 5)
  (nil nil defstruct-binop-3 + x 5))

(deftest defstruct-binop.9
  (make-defstruct-binop-3 :operand-2 4 :operator '*)
  (nil nil defstruct-binop-3 * nil 4))

(deftest defstruct-binop-named.1
  (defstruct (defstruct-binop-named-1 (:type list))
    (operator '? :type symbol)
    operand-1
    operand-2)
  defstruct-binop-named-1)

(deftest defstruct-binop-named.2
  (make-defstruct-binop-named-1 :operator '+ :operand-1 'x :operand-2 5)
  (+ x 5))

(deftest defstruct-binop-named.3
  (make-defstruct-binop-named-1 :operand-2 4 :operator '*)
  (* nil 4))

(deftest defstruct-binop-named.4
  (defstruct (defstruct-binop-named-2 (:type list) :named)
    (operator '? :type symbol)
    operand-1
    operand-2)
  defstruct-binop-named-2)

(deftest defstruct-binop-named.5
  (make-defstruct-binop-named-2 :operator '+ :operand-1 'x :operand-2 5)
  (defstruct-binop-named-2 + X 5))

(deftest defstruct-binop-named.6
  (make-defstruct-binop-named-2 :operand-2 4 :operator '*)
  (defstruct-binop-named-2 * NIL 4))

(deftest defstruct-type-of.1
  (progn
    (defstruct defstruct-type-of-1)
    (type-of (make-defstruct-type-of-1)))
  defstruct-type-of-1)

(deftest defstruct-type-of.2
  (progn
    (defstruct (defstruct-type-of-2 (:type list) :named))
    (type-of (make-defstruct-type-of-2)))
  cons)

(deftest defstruct-type-of.3
  (progn
    (defstruct (defstruct-type-of-3 (:type vector)))
    (type-of (make-defstruct-type-of-3)))
  (simple-vector 0))

(deftest defstruct-example.1
  (defstruct defstruct-example
    x-position
    y-position
    x-velocity
    y-velocity
    mass)
  defstruct-example)

(deftest defstruct-example.2
  (let ((x (make-defstruct-example :x-position 10)))
    (defstruct-example-x-position x))
  10)

(deftest defstruct-example.3
  (let ((x (make-defstruct-example :x-position 10)))
    (typep x 'defstruct-example))
  t)

(deftest defstruct-example.4
  (let ((x (make-defstruct-example :x-position 10)))
    (defstruct-example-p x))
  t)

(deftest defstruct-example.5
  (defstruct-example-p 10)
  nil)

(deftest defstruct-example.6
  (let* ((*default-ship-mass* 111)
         (x (make-defstruct-example
              :mass *default-ship-mass*
              :x-position 0
              :y-position 0)))
    (values
      (defstruct-example-mass x)
      (defstruct-example-x-position x)
      (defstruct-example-y-position x)))
  111 0 0)

(deftest defstruct-example.7
  (let* ((*default-ship-mass* 111)
         (x (make-defstruct-example
              :mass *default-ship-mass*
              :x-position 10
              :y-position 20))
         (y (copy-defstruct-example x)))
    (values
      (defstruct-example-mass y)
      (defstruct-example-x-position y)
      (defstruct-example-y-position y)))
  111 10 20)

(deftest defstruct-example.8
  (let ((x (make-defstruct-example :x-position 10)))
    (values
      (setf (defstruct-example-x-position x) 111)
      (defstruct-example-x-position x)))
  111 111)

;;  town
(deftest defstruct-town.1
  (defstruct defstruct-town
    area
    watertowers
    (firetrucks 1 :type fixnum)
    population
    (elevation 5128 :read-only t))
  defstruct-town)

(defvar defstruct-town-1)

(deftest defstruct-town.2
  (progn
    (setq defstruct-town-1 (make-defstruct-town :area 0 :watertowers 0))
    (defstruct-town-p defstruct-town-1))
  t)

(deftest defstruct-town.3
  (defstruct-town-area defstruct-town-1)
  0)

(deftest defstruct-town.4
  (defstruct-town-elevation defstruct-town-1)
  5128)

(deftest defstruct-town.5
  (setf (defstruct-town-population defstruct-town-1) 99)
  99)

(deftest defstruct-town.6
  (defstruct-town-population defstruct-town-1)
  99)

(defvar defstruct-town-2)
(deftest defstruct-town.7
  (progn
    (setq defstruct-town-2 (copy-defstruct-town defstruct-town-1))
    (= (defstruct-town-population defstruct-town-1)
       (defstruct-town-population defstruct-town-2)))
  t)

(deftest defstruct-town.8
  (let ((x (make-defstruct-town :area 0 :watertowers 3 :elevation 1200)))
    (defstruct-town-elevation x))
  1200)


;; clown
(deftest defstruct-clown.1
  (defstruct (defstruct-clown (:conc-name defstruct-bozo-))
    (nose-color 'red)
    frizzy-hair-p polkadots)
  defstruct-clown)

(deftest defstruct-clown.2
  (let ((funny-clown (make-defstruct-clown)))
    (defstruct-bozo-nose-color funny-clown))
  red)

(deftest defstruct-clown.3
  (defstruct (defstruct-klown
               (:constructor defstruct-make-up-klown)
               (:copier defstruct-clone-klown)
               (:predicate defstruct-is-a-bozo-p))
    nose-color frizzy-hair-p polkadots)
  defstruct-klown)

(deftest defstruct-clown.4
  (fboundp 'defstruct-make-up-klown)
  t)


;;  vehicle
(deftest defstruct-vehicle.1
  (defstruct defstruct-vehicle
    name year (diesel t :read-only t))
  defstruct-vehicle)

(deftest defstruct-vehicle.2
  (defstruct (defstruct-truck (:include defstruct-vehicle (year 79)))
    load-limit
    (axles 6))
  defstruct-truck)

(defvar defstruct-vehicle-x)
(deftest defstruct-vehicle.3
  (progn
    (setq defstruct-vehicle-x
          (make-defstruct-truck :name 'mac :diesel t :load-limit 17))
    (defstruct-vehicle-name defstruct-vehicle-x))
  mac)

(deftest defstruct-vehicle.4
  (defstruct-vehicle-year defstruct-vehicle-x)
  79)

(deftest defstruct-vehicle.5
  (defstruct (defstruct-pickup (:include defstruct-truck))
    camper long-bed four-wheel-drive)
  defstruct-pickup)

(deftest defstruct-vehicle.6
  (progn
    (setq defstruct-vehicle-x
          (make-defstruct-pickup :name 'king :long-bed t))
    (defstruct-pickup-year defstruct-vehicle-x))
  79)

;;  dfs-boa
(deftest defstruct-dfs-boa.1
  (defstruct (defstruct-dfs-boa
               (:constructor make-defstruct-dfs-boa (a b c))
               (:constructor defstruct-create-dfs-boa
                             (a &optional b (c 'cc) &rest d &aux e (f 'ff))))
    a b c d e f)
  defstruct-dfs-boa)

(defvar defstruct-dfs-boa-x)
(deftest defstruct-dfs-boa.2
  (progn
    (setq defstruct-dfs-boa-x (make-defstruct-dfs-boa 1 2 3))
    (defstruct-dfs-boa-a defstruct-dfs-boa-x))
  1)

(deftest defstruct-dfs-boa.3
  (progn
    (setq defstruct-dfs-boa-x (defstruct-create-dfs-boa 1 2))
    (defstruct-dfs-boa-b defstruct-dfs-boa-x))
  2)

(deftest defstruct-dfs-boa.4
  (eq (defstruct-dfs-boa-c defstruct-dfs-boa-x) 'cc)
  t)

(deftest defstruct-dfs-boa.5
  (progn
    (setq defstruct-dfs-boa-x (defstruct-create-dfs-boa 1 2 3 4 5 6))
    (defstruct-dfs-boa-d defstruct-dfs-boa-x))
  (4 5 6))

;;  program-error
(deftest-error defstruct-program-error.1
  (eval '(defstruct destruct-program-error-1 aaa aaa))
  program-error)

(deftest-error defstruct-program-error.2
  (progn
    (defstruct destruct-program-error-2 aaa)
    (eval '(defstruct (destruct-program-error-3
                        (:include destruct-program-error-2))
             aaa)))
  program-error)

(deftest-error defstruct-structure-type.1
  (progn
    (defclass defstruct-structure-type-1 () ())
    (defstruct (defstruct-structure-type-2
                 (:include defstruct-structure-type-1)))))

;;  #S(...)
(deftest defstruct-reader.1
  (progn
    (defstruct defstruct-reader-1 aaa bbb)
    (defstruct-reader-1-bbb
      (read-from-string "#S(defstruct-reader-1 :aaa 10 :bbb 20)")))
  20)

(deftest defstruct-reader.2
  (progn
    (defstruct (defstruct-reader-2 (:type list)) aaa bbb)
    (values
      (read-from-string "#S(defstruct-reader-2 :aaa 10 :bbb 20)")))
  (10 20))

(deftest defstruct-reader.3
  (progn
    (defstruct (defstruct-reader-3 (:type vector)) aaa bbb)
    (values
      (read-from-string "#S(defstruct-reader-3 :aaa 10 :bbb 20)")))
  #(10 20))

