;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Function CLASS-OF
;;
(deftest class-of.1
  (subtypep
    (class-of 'hello)
    'symbol)
  t t)

(deftest class-of.2
  (class-name
    (class-of
      (find-class 'standard-class)))
  standard-class)

(deftest class-of.3
  (class-name
    (class-of
      (find-class 'class)))
  standard-class)

(deftest class-of.4
  (progn
    (defclass class-of-1 () ())
    (defclass class-of-2 (class-of-1) ())
    (values)))

(deftest class-of.5
  (let ((inst (make-instance 'class-of-2)))
    (class-name
      (class-of inst)))
  class-of-2)

(deftest class-of.6
  (class-name
    (class-of
      (find-class 'class-of-2)))
  standard-class)

(deftest-error! class-of-error.1
  (eval '(class-of)))

(deftest-error! class-of-error.2
  (eval '(class-of 10 20)))

;;  ANSI Common Lisp
(deftest class-of-test.1
  (class-name
    (class-of 'fred))
  symbol)

(deftest class-of-test.2
  (class-name
    (class-of 2/3))
  ratio)

(deftest class-of-test.3
  (progn
    (defclass class-of-test-book () ())
    (class-name
      (class-of
        (make-instance 'class-of-test-book))))
  class-of-test-book)

(deftest class-of-test.4
  (progn
    (defclass class-of-test-novel (class-of-test-book) ())
    (class-name
      (class-of
        (make-instance 'class-of-test-novel))))
  class-of-test-novel)

(deftest class-of-test.5
  (progn
    (defstruct class-of-test-kons kar kdr)
    (class-name
      (class-of
        (make-class-of-test-kons :kar 3 :kdr 4))))
  class-of-test-kons)


;;
;;  Accessor FIND-CLASS
;;
(deftest find-class.1
  (closp
    (find-class 'standard-class))
  t)

(deftest find-class.2
  (closp
    (find-class 'standard-class nil))
  t)

(deftest find-class.3
  (closp
    (find-class 'standard-class nil nil))
  t)

(deftest-error find-class.4
  (find-class 'no-such-class-name))

(deftest find-class.5
  (find-class 'no-such-class-name nil)
  nil)

(deftest-error! find-class-error.1
  (eval '(find-class)))

(deftest-error! find-class-error.2
  (eval '(find-class 'standard-class nil nil nil)))

(deftest-error! find-class-error.3
  (eval '(find-class 10))
  type-error)


;;
;;  Accessor (SETF FIND-CLASS)
;;
(deftest setf-find-class.1
  (progn
    (defclass setf-find-class-1 () ())
    (defclass setf-find-class-2 () ())
    (defclass setf-find-class-3 () ())
    (values)))

(deftest setf-find-class.2
  (let ((inst (find-class 'setf-find-class-2)))
    (setf (find-class 'setf-find-class-1) inst)
    (class-name
      (find-class 'setf-find-class-1)))
  setf-find-class-2)

(deftest setf-find-class.3
  (let ((inst (find-class 'setf-find-class-3)))
    (setf (find-class 'setf-find-class-1 :hello-hello) inst)
    (class-name
      (find-class 'setf-find-class-1)))
  setf-find-class-3)

(deftest setf-find-class.4
  (progn
    (defclass setf-find-class-4 () ())
    (values
      (class-name
        (find-class 'setf-find-class-4))
      (setf (find-class 'setf-find-class-4) nil)
      (find-class 'setf-find-class-4 nil)))
  setf-find-class-4 nil nil)

(deftest-error setf-find-class.5
  (find-class 'setf-find-class-4))

(deftest-error! setf-find-class-error.1
  (eval '(setf (find-class) (find-class 'class))))

(deftest-error! setf-find-class-error.2
  (eval '(setf (find-class 'hello nil nil nil) (find-class 'class))))

(deftest-error! setf-find-class-error.3
  (eval '(setf (find-class 10) (find-class 'class)))
  type-error)


;;
;;  Standard Generic Function CHANGE-CLASS
;;
(defclass change-class-1 () (aaa bbb ccc))
(defclass change-class-2 () (ccc ddd))

(deftest change-class.1
  (let ((x (make-instance 'change-class-1)))
    (typep
      (change-class x 'change-class-2)
      'change-class-2))
  t)

(deftest change-class.2
  (let ((x (make-instance 'change-class-1)))
    (eq x (change-class x 'change-class-2)))
  t)

(deftest change-class.3
  (let ((x (make-instance 'change-class-1)))
    (change-class x 'change-class-2)
    (values
      (slot-exists-p x 'aaa)
      (slot-exists-p x 'bbb)
      (slot-exists-p x 'ccc)
      (slot-exists-p x 'ddd)))
  nil nil t t)

(deftest change-class.4
  (let ((x (make-instance 'change-class-1)))
    (setf (slot-value x 'bbb) 10)
    (setf (slot-value x 'ccc) 20)
    (change-class x 'change-class-2)
    (values
      (slot-value x 'ccc)
      (slot-boundp x 'ddd)))
  20 nil)

(deftest change-class.5
  (progn
    (defclass change-class-3 () (aaa bbb ccc))
    (defclass change-class-4 () (ccc ddd))
    (defmethod update-instance-for-different-class :before
      ((prev change-class-3) (inst change-class-4) &rest args &key &allow-other-keys)
      (declare (ignore args))
      (setf (slot-value inst 'ccc) (slot-value prev 'bbb))
      (setf (slot-value inst 'ddd) (slot-value prev 'aaa)))
    (values)))

(deftest change-class.6
  (let ((x (make-instance 'change-class-3)))
    (setf (slot-value x 'aaa) 10)
    (setf (slot-value x 'bbb) 20)
    (change-class x 'change-class-4)
    (values
      (slot-value x 'ccc)
      (slot-value x 'ddd)))
  20 10)

(deftest change-class.7
  (progn
    (defclass change-class-5 ()
      ((aaa :initarg :aaa :initform 10)
       (bbb :initarg :bbb :initform 20)
       (ccc :initarg :ccc :initform 30)))
    (defclass change-class-6 ()
      ((ccc :initarg :ccc :initform 40)
       (ddd :initarg :ddd :initform 50)))
    (values)))

(deftest change-class.8
  (let ((x (make-instance 'change-class-5)))
    (change-class x 'change-class-6)
    (values
      (slot-value x 'ccc)
      (slot-value x 'ddd)))
  30 50)

(deftest change-class.9
  (let ((x (make-instance 'change-class-5)))
    (slot-makunbound x 'ccc)
    (change-class x 'change-class-6)
    (values
      (slot-boundp x 'ccc)
      (slot-value x 'ddd)))
  nil 50)

(deftest change-class.10
  (let ((x (make-instance 'change-class-5)))
    (change-class x 'change-class-6 :ccc 111 :ddd 222)
    (values
      (slot-value x 'ccc)
      (slot-value x 'ddd)))
  111 222)

(deftest-error change-class.11
  (let ((x (make-instance 'change-class-5)))
    (change-class x 'change-class-6 :aaa 333)))

(deftest change-class.12
  (progn
    (defclass change-class-7 () (aaa bbb ccc))
    (defclass change-class-8 () (ccc ddd))
    (defmethod update-instance-for-different-class :before
      ((prev change-class-7) (inst change-class-8) &rest args &key &allow-other-keys)
      (declare (ignore args))
      (setf (slot-value inst 'ccc) (slot-value prev 'bbb))
      (setf (slot-value inst 'ddd) (slot-value prev 'aaa))
      (error "Hello"))
    (values)))

(deftest change-class.13
  (let ((x (make-instance 'change-class-7)))
    (setf (slot-value x 'aaa) 10)
    (setf (slot-value x 'bbb) 20)
    (handler-case
      (change-class x 'change-class-8)
      (error ()))
    (values
      (class-name (class-of x))
      (slot-value x 'aaa)
      (slot-value x 'bbb)))
  change-class-7 10 20)

(deftest-error! change-class-error.1
  (eval '(let ((x (make-instance 'change-class-1)))
           (change-class x))))

(deftest-error change-class-error.2
  (eval '(let ((x (make-instance 'change-class-1)))
           (change-class x 'change-class-2 :hello))))

(deftest-error change-class-error.3
  (eval '(change-class 10 'change-class-2)))

;;  ANSI Common Lisp
(deftest change-class-test.1
  (progn
    (defclass change-class-test-position () ())
    (defclass change-class-test-x-y-position (change-class-test-position)
      ((x :initform 0 :initarg :x)
       (y :initform 0 :initarg :y)))
    (defclass change-class-test-rho-theta-position (change-class-test-position)
      ((rho :initform 0)
       (theta :initform 0)))
    (defmethod update-instance-for-different-class :before
      ((old change-class-test-x-y-position)
       (new change-class-test-rho-theta-position)
       &key)
      (let ((x (slot-value old 'x))
            (y (slot-value old 'y)))
        (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
              (slot-value new 'theta) (atan y x))))
    (values)))

(deftest change-class-test.2
  (let ((p1 (make-instance 'change-class-test-x-y-position :x 2 :y 0)))
    (change-class p1 'change-class-test-rho-theta-position)
    (values)))


;;
;;  Standard Generic Function MAKE-INSTANCES-OBSOLETE
;;
(deftest make-instances-obsolete.1
  (progn
    (defclass make-instances-obsolete-1 () (aaa))
    (values)))

(deftest make-instances-obsolete.2
  (class-name
    (make-instances-obsolete 'make-instances-obsolete-1))
  make-instances-obsolete-1)

(deftest make-instances-obsolete.3
  (class-name
    (make-instances-obsolete
      (find-class 'make-instances-obsolete-1)))
  make-instances-obsolete-1)

(deftest-error! make-instances-obsolete-error.1
  (eval '(make-instances-obsolete)))

(deftest-error! make-instances-obsolete-error.2
  (eval '(make-instances-obsolete 'make-instances-obsolete-1 nil)))

(deftest-error make-instances-obsolete-error.3
  (eval '(make-instances-obsolete 100)))


;;
;;  Standard Generic Function UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
;;
(deftest update-instance-for-different-class.1
  (progn
    (defclass update-instance-for-different-class-1 () (aaa bbb ccc))
    (defclass update-instance-for-different-class-2 () (ccc ddd))
    (values)))

(deftest update-instance-for-different-class.2
  (progn
    (defmethod update-instance-for-different-class
      ((prev update-instance-for-different-class-1)
       (inst update-instance-for-different-class-2)
       &rest args &key &allow-other-keys)
      (declare (ignore args))
      (unless (slot-exists-p prev 'aaa) (error "error"))
      (unless (slot-exists-p prev 'bbb) (error "error"))
      (unless (slot-exists-p prev 'ccc) (error "error"))
      (when (slot-exists-p prev 'ddd) (error "error"))
      (when (slot-exists-p inst 'aaa) (error "error"))
      (when (slot-exists-p inst 'bbb) (error "error"))
      (unless (slot-exists-p inst 'ccc) (error "error"))
      (unless (slot-exists-p inst 'ddd) (error "error")))
    (change-class
      (make-instance 'update-instance-for-different-class-1)
      'update-instance-for-different-class-2)
    (values)))


;;
;;  Standard Generic Function UPDATE-INSTANCE-FOR-REDEFINED-CLASS
;;
(deftest update-instance-for-redefined-class.1
  (progn
    (defclass update-instance-for-redefined-class-1 () (aaa bbb ccc))
    (defmethod update-instance-for-redefined-class :before
      ((inst update-instance-for-redefined-class-1)
       add del prop &rest args &key &allow-other-keys)
      (declare (ignore inst prop args))
      (unless (equal '(ddd eee fff) (sort add #'string< :key #'string))
        (error "add error"))
      (unless (equal '(aaa bbb) (sort del #'string< :key #'string))
        (error "del error")))
    (let ((x (make-instance 'update-instance-for-redefined-class-1)))
      (defclass update-instance-for-redefined-class-1 () (ccc ddd eee fff))
      (values (slot-exists-p x 'fff))))
  t)

(deftest update-instance-for-redefined-class.2
  (progn
    (defclass update-instance-for-redefined-class-2 () (aaa bbb ccc))
    (defmethod update-instance-for-redefined-class :before
      ((inst update-instance-for-redefined-class-2)
       add del prop &rest args &key &allow-other-keys)
      (declare (ignore inst add del args))
      (when prop
        (error "prop error")))
    (let ((x (make-instance 'update-instance-for-redefined-class-2)))
      (defclass update-instance-for-redefined-class-2 () (ccc ddd eee fff))
      (values (slot-exists-p x 'fff))))
  t)

(deftest update-instance-for-redefined-class.3
  (progn
    (defclass update-instance-for-redefined-class-3 () (aaa bbb ccc))
    (defmethod update-instance-for-redefined-class :before
      ((inst update-instance-for-redefined-class-3)
       add del prop &rest args &key &allow-other-keys)
      (declare (ignore inst add del args))
      (unless (eql (getf prop 'aaa) 10)
        (error "aaa error"))
      (unless (eql (getf prop 'bbb) 20)
        (error "bbb error"))
      (unless (eql (getf prop 'ccc) 30)
        (error "ccc error")))
    (let ((x (make-instance 'update-instance-for-redefined-class-3)))
      (setf (slot-value x 'aaa) 10)
      (setf (slot-value x 'bbb) 20)
      (setf (slot-value x 'ccc) 30)
      (defclass update-instance-for-redefined-class-3 () (ccc ddd eee fff))
      (values (slot-exists-p x 'fff))))
  t)

;;  ANSI Common Lisp
(deftest update-instance-for-redefined-class-test.1
  (progn
    (defclass redefined-test-position () ())
    (values)))

(deftest update-instance-for-redefined-class-test.2
  (progn
    (defclass redefined-test-x-y-position (redefined-test-position)
      ((x :initform 0 :accessor redefined-test-position-x)
       (y :initform 0 :accessor redefined-test-position-y)))
    (values)))

(deftest update-instance-for-redefined-class-test.3
  (progn
    (defmethod update-instance-for-redefined-class :before
      ((pos redefined-test-x-y-position) added deleted plist &key)
      (declare (ignore added deleted))
      (let ((x (getf plist 'x))
            (y (getf plist 'y)))
        (setf (redefined-test-position-rho pos) (sqrt (+ (* x x) (* y y)))
              (redefined-test-position-theta pos) (atan y x))))
    (values)))

(deftest update-instance-for-redefined-class-test.4
  (progn
    (defclass redefined-test-x-y-position (redefined-test-position)
      ((rho :initform 0 :accessor redefined-test-position-rho)
       (theta :initform 0 :accessor redefined-test-position-theta)))
    (values)))

(deftest update-instance-for-redefined-class-test.5
  (progn
    (defmethod redefined-test-position-x ((pos redefined-test-x-y-position))
      (with-slots (rho theta) pos (* rho (cos theta))))
    (values)))

(deftest update-instance-for-redefined-class-test.6
  (progn
    (defmethod (setf redefined-test-position-x)
      (new-x (pos redefined-test-x-y-position))
      (with-slots (rho theta) pos
        (let ((y (redefined-test-position-y pos)))
          (setq rho (sqrt (+ (* new-x new-x) (* y y)))
                theta (atan y new-x))
          new-x)))
    (values)))

(deftest update-instance-for-redefined-class-test.7
  (progn
    (defmethod redefined-test-position-y
      ((pos redefined-test-x-y-position))
      (with-slots (rho theta) pos (* rho (sin theta))))
    (values)))

(deftest update-instance-for-redefined-class-test.8
  (progn
    (defmethod (setf redefined-test-position-y)
      (new-y (pos redefined-test-x-y-position))
      (with-slots (rho theta) pos
        (let ((x (redefined-test-position-x pos)))
          (setq rho (sqrt (+ (* x x) (* new-y new-y)))
                theta (atan new-y x))
          new-y)))
    (values)))


;;
;;  Standard Generic Function MAKE-LOAD-FORM
;;
(deftest-error make-load-form.1
  (progn
    (defclass make-load-form-1 () (aaa bbb ccc))
    (make-load-form
      (make-instance 'make-load-form-1))))

(deftest make-load-form.2
  (progn
    (defmethod make-load-form ((inst make-load-form-1) &optional env)
      (declare (ignore inst env))
      (values :hello :second))
    (make-load-form
      (make-instance 'make-load-form-1)))
  :hello :second)

(deftest-error make-load-form.3
  (progn
    (defstruct make-load-form-2 aaa bbb ccc)
    (make-load-form
      (make-instance 'make-load-form-2))))

(deftest make-load-form.4
  (progn
    (defmethod make-load-form ((inst make-load-form-2) &optional env)
      (declare (ignore inst env))
      (values :hello :second))
    (make-load-form
      (make-instance 'make-load-form-2)))
  :hello :second)

(deftest make-load-form.5
  (progn
    (defclass make-load-form-3 () (aaa bbb ccc))
    (defmethod make-load-form ((inst make-load-form-3) &optional env)
      (make-load-form-saving-slots inst :environment env))
    (multiple-value-bind (x y) (make-load-form
                                 (make-instance 'make-load-form-3))
      (values (consp x) (consp y))))
  t t)

(defvar *make-load-form-result*)
(deftest make-load-form.6
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*make-load-form-result*)
      (with-open-file (stream input :direction :output)
        (princ
          "(setq *make-load-form-result* #.(make-instance 'make-load-form-1))"
          stream))
      (file-position input :start)
      (compile-file input :output-file output)
      (file-position output :start)
      (load output :type :fasl)
      (eq *make-load-form-result* :hello)))
  t)

(deftest make-load-form.7
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*make-load-form-result*)
      (with-open-file (stream input :direction :output)
        (princ
          "(setq *make-load-form-result* #.(make-instance 'make-load-form-3))"
          stream))
      (file-position input :start)
      (compile-file input :output-file output)
      (file-position output :start)
      (load output :type :fasl)
      (typep *make-load-form-result* 'make-load-form-3)))
  t)

(deftest make-load-form.8
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*make-load-form-result*)
      (defstruct make-load-form-4 aaa bbb ccc)
      (defmethod make-load-form ((inst make-load-form-4) &optional env)
        (make-load-form-saving-slots inst :environment env))
      (with-open-file (stream input :direction :output)
        (princ
          "(setq *make-load-form-result* #.(make-make-load-form-4))"
          stream))
      (file-position input :start)
      (compile-file input :output-file output)
      (file-position output :start)
      (load output :type :fasl)
      (typep *make-load-form-result* 'make-load-form-4)))
  t)

(deftest make-load-form.9
  (equal (make-load-form (find-class 'make-load-form-1))
         '(find-class (quote make-load-form-1)))
  t)

(deftest make-load-form.10
  (equal (make-load-form (find-class 'program-error))
         '(find-class (quote program-error)))
  t)

;;  ANSI Common Lisp
(deftest make-load-form-test.1
  (progn
    (defclass make-load-form-test-obj ()
      ((x :initarg :x :reader obj-x)
       (y :initarg :y :reader obj-y)
       (dist :accessor make-load-form-test-obj-dist)))
    (values)))

(deftest make-load-form-test.2
  (progn
    (defmethod shared-initialize :after
      ((self make-load-form-test-obj) slot-names &rest keys)
      (declare (ignore slot-names keys))
      (unless (slot-boundp self 'dist)
        (setf (make-load-form-test-obj-dist self)
              (sqrt (+ (expt (obj-x self) 2) (expt (obj-y self) 2))))))
    (values)))

(deftest make-load-form-test.3
  (progn
    (defmethod make-load-form
      ((self make-load-form-test-obj) &optional environment)
      (declare (ignore environment))
      `(make-instance ',(class-name (class-of self))
                      :x ',(obj-x self) :y ',(obj-y self)))
    (values)))

(deftest make-load-form-test.4
  (let ((obj1 (make-instance 'make-load-form-test-obj :x 3.0 :y 4.0)))
    (values
      (make-load-form-test-obj-dist obj1)
      (make-load-form obj1)))
  5.0
  (make-instance 'make-load-form-test-obj :x '3.0 :y '4.0))


;;
;;  Function MAKE-LOAD-FORM-SAVING-SLOTS
;;
(deftest make-load-form-saving-slots.1
  (progn
    (defclass make-load-form-saving-slots-1 () (aaa bbb ccc))
    (multiple-value-bind (x y)
      (make-load-form-saving-slots
        (make-instance 'make-load-form-saving-slots-1))
      (values (consp x) (consp y))))
  t t)

(deftest make-load-form-saving-slots.2
  (progn
    (defclass make-load-form-saving-slots-1 () (aaa bbb ccc))
    (multiple-value-bind (x y)
      (make-load-form-saving-slots
        (make-instance 'make-load-form-saving-slots-1)
        :slot-names '(aaa))
      (values (consp x) (consp y))))
  t t)

(defvar *make-load-form-saving-slots-result*)
(deftest make-load-form-saving-slots.3
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*make-load-form-saving-slots-result*)
      (defclass make-load-form-saving-slots-2 ()
        ((aaa :initform 10)
         (bbb :initform 20)
         (ccc :initform 30)))
      (defmethod make-load-form ((inst make-load-form-saving-slots-2) &optional env)
        (make-load-form-saving-slots inst :environment env))
      (with-open-file (stream input :direction :output)
        (princ "(setq *make-load-form-saving-slots-result*" stream)
        (princ "   #.(make-instance 'make-load-form-saving-slots-2))" stream))
      (file-position input :start)
      (compile-file input :output-file output)
      (file-position output :start)
      (load output :type :fasl)
      (values
        (slot-value *make-load-form-saving-slots-result* 'aaa)
        (slot-value *make-load-form-saving-slots-result* 'bbb)
        (slot-value *make-load-form-saving-slots-result* 'ccc))))
  10 20 30)

(deftest make-load-form-saving-slots.4
  (with-open-stream (input (lisp-system:make-memory-io-stream))
    (with-open-stream (output (lisp-system:make-memory-io-stream))
      (makunbound '*make-load-form-saving-slots-result*)
      (defclass make-load-form-saving-slots-3 ()
        ((aaa :initform 10)
         (bbb :initform 20)
         (ccc :initform 30)))
      (defmethod make-load-form ((inst make-load-form-saving-slots-3) &optional env)
        (make-load-form-saving-slots
          inst :slot-names '(bbb ccc) :environment env))
      (with-open-file (stream input :direction :output)
        (princ "(setq *make-load-form-saving-slots-result*" stream)
        (princ "   #.(make-instance 'make-load-form-saving-slots-3))" stream))
      (file-position input :start)
      (compile-file input :output-file output)
      (file-position output :start)
      (load output :type :fasl)
      (values
        (slot-boundp *make-load-form-saving-slots-result* 'aaa)
        (slot-value *make-load-form-saving-slots-result* 'bbb)
        (slot-value *make-load-form-saving-slots-result* 'ccc))))
  nil 20 30)

(deftest-error! make-load-form-saving-slots-error.1
  (eval '(make-load-form-saving-slots)))

(deftest-error make-load-form-saving-slots-error.2
  (eval '(make-load-form-saving-slots 10)))

(deftest-error make-load-form-saving-slots-error.3
  (eval '(make-load-form-saving-slots (find-class 'class) :hello)))

