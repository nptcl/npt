;;
;;  ANSI COMMON LISP: 7. Objects
;;

;;
;;  Macro DEFINE-METHOD-COMBINATION
;;
(deftest define-method-combination.1
  (define-method-combination define-method-combination-1 :operator progn)
  define-method-combination-1)

(deftest define-method-combination.2
  (define-method-combination
    define-method-combination-2 ()
    ((primary ()))
    nil)
  define-method-combination-2)

(deftest-error define-method-combination-error.1
  (eval '(define-method-combination 100)))

(deftest-error define-method-combination-error.2
  (eval '(define-method-combination 100 ())))

(deftest-error define-method-combination-error.3
  (eval '(define-method-combination
           define-method-combination-error-1
           100)))

;;  ANSI Common Lisp
(deftest define-method-combination-test.1
  (define-method-combination
    define-method-combination-test-and-1
    :operator and
    :identity-with-one-argument t)
  define-method-combination-test-and-1)

(deftest define-method-combination-test.2
  (progn
    (defgeneric define-method-combination-test-1 (x y)
      (:method-combination define-method-combination-test-and-1))
    (typep
      (defmethod define-method-combination-test-1
        define-method-combination-test-and-1 ((x integer) y)
        (values x y))
      'standard-method))
  t)

(deftest define-method-combination-test.3
  (define-method-combination define-method-combination-test-and-2
    (&optional (order :most-specific-first))
    ((around (:around))
     (primary (and) :order order :required t))
    (let ((form (if (rest primary)
                  `(and ,@(mapcar #'(lambda (method)
                                      `(call-method ,method))
                                  primary))
                  `(call-method ,(first primary)))))
      (if around
        `(call-method ,(first around)
                      (,@(rest around)
                        (make-method ,form)))
        form)))
  define-method-combination-test-and-2)

(deftest define-method-combination-test.4
  (define-method-combination define-method-combination-test-standard ()
    ((around (:around))
     (before (:before))
     (primary () :required t)
     (after (:after)))
    (flet ((call-methods (methods)
                         (mapcar #'(lambda (method)
                                     `(call-method ,method))
                                 methods)))
      (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                       (progn ,@(call-methods before)
                              (call-method ,(first primary)
                                           ,(rest primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
        (if around
          `(call-method ,(first around)
                        (,@(rest around)
                          (make-method ,form)))
          form))))
  define-method-combination-test-standard)

(deftest define-method-combination-test.5
  (define-method-combination define-method-combination-test-or-1 ()
    ((methods (or)))
    `(or ,@(mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
  define-method-combination-test-or-1)

(deftest define-method-combination-test.6
  (define-method-combination define-method-combination-test-or-2
    (&optional (order ':most-specific-first))
    ((around (:around))
     (primary (or)))
    ;; Process the order argument
    (case order
      (:most-specific-first)
      (:most-specific-last (setq primary (reverse primary)))
      (otherwise (method-combination-error
                   "~S is an invalid order.~@
                   :most-specific-first and :most-specific-last are the possible values."
                   order)))
    ;; Must have a primary method
    (unless primary
      (method-combination-error "A primary method is required."))
    ;; Construct the form that calls the primary methods
    (let ((form (if (rest primary)
                  `(or ,@(mapcar #'(lambda (method)
                                     `(call-method ,method))
                                 primary))
                  `(call-method ,(first primary)))))
      ;; Wrap the around methods around that form
      (if around
        `(call-method ,(first around)
                      (,@(rest around)
                        (make-method ,form)))
        form)))
  define-method-combination-test-or-2)

(deftest define-method-combination-test.7
  (define-method-combination define-method-combination-test-or-3
    (&optional (order ':most-specific-first))
    ((around (:around))
     (primary (or) :order order :required t))
    (let ((form (if (rest primary)
                  `(or ,@(mapcar #'(lambda (method)
                                     `(call-method ,method))
                                 primary))
                  `(call-method ,(first primary)))))
      (if around
        `(call-method ,(first around)
                      (,@(rest around)
                        (make-method ,form)))
        form)))
  define-method-combination-test-or-3)

(deftest define-method-combination-test.8
  (define-method-combination
    define-method-combination-test-or-4
    :identity-with-one-argument t)
  define-method-combination-test-or-4)

(deftest define-method-combination-test.9
  (define-method-combination define-method-combination-test-example ()
    ((methods define-method-combination-test-example-p))
    `(progn ,@(mapcar #'(lambda (method)
                          `(call-method ,method))
                      (stable-sort methods #'<
                                   :key #'(lambda (method)
                                            (first (method-qualifiers method)))))))
  define-method-combination-test-example)

(defun define-method-combination-test-example-p (method-qualifiers)
  (and (= (length method-qualifiers) 1)
       (typep (first method-qualifiers) '(integer 0 *))))

(deftest define-method-combination-test.10
  (define-method-combination define-method-combination-test-progn ()
    ((methods ()))
    (:arguments object)
    `(unwind-protect
       (progn (lock (object-lock ,object))
              ,@(mapcar #'(lambda (method)
                            `(call-method ,method))
                        methods))
       (unlock (object-lock ,object))))
  define-method-combination-test-progn)


;;
;;  Local Macro CALL-METHOD
;;
(deftest call-method.1
  (progn
    (define-method-combination call-method-1 ()
      ((list ()))
      `(1+ (call-method ,(car list))))
    (defgeneric call-method-2 (x) (:method-combination call-method-1))
    (defmethod call-method-2 ((x integer))
      (declare (ignore x))
      100)
    (defmethod call-method-2 (x)
      (declare (ignore x))
      200)
    (call-method-2 999))
  101)

(deftest call-method.2
  (progn
    (define-method-combination call-method-3 ()
      ((list ()))
      `(call-method ,(car list)))
    (defgeneric call-method-4 (x) (:method-combination call-method-3))
    (defmethod call-method-4 ((x integer))
      (declare (ignore x))
      (values 100 (next-method-p)))
    (defmethod call-method-4 (x)
      (declare (ignore x))
      (values 200 (next-method-p)))
    (call-method-4 999))
  100 nil)

(deftest call-method.3
  (progn
    (define-method-combination call-method-5 ()
      ((list ()))
      `(call-method ,(car list) (,(cadr list))))
    (defgeneric call-method-6 (x) (:method-combination call-method-5))
    (defmethod call-method-6 ((x integer))
      (declare (ignore x))
      (values 100 (next-method-p)))
    (defmethod call-method-6 (x)
      (declare (ignore x))
      (values 200 (next-method-p)))
    (call-method-6 999))
  100 t)

(deftest call-method.4
  (progn
    (define-method-combination call-method-7 ()
      ((list ()))
      `(call-method ,(car list) (,(cadr list))))
    (defgeneric call-method-8 (x) (:method-combination call-method-7))
    (defmethod call-method-8 ((x integer))
      (declare (ignore x))
      (if (next-method-p)
        (call-next-method)
        100))
    (defmethod call-method-8 ((x rational))
      (declare (ignore x))
      (if (next-method-p)
        (call-next-method)
        200))
    (defmethod call-method-8 (x)
      (declare (ignore x))
      (if (next-method-p)
        (call-next-method)
        300))
    (call-method-8 999))
  200)

(deftest call-method.5
  (progn
    (define-method-combination call-method-9 ()
      ((list ()))
      `(call-method (make-method 100)))
    (defgeneric call-method-10 (x) (:method-combination call-method-9))
    (defmethod call-method-10 (x)
      (declare (ignore x))
      200)
    (call-method-10 999))
  100)

(deftest call-method.6
  (progn
    (define-method-combination call-method-11 ()
      ((list ()))
      `(call-method (make-method
                      (1+ (call-method ,(car list))))
                    ,(cdr list)))
    (defgeneric call-method-12 (x) (:method-combination call-method-11))
    (defmethod call-method-12 (x)
      (declare (ignore x))
      (if (next-method-p)
        (call-next-method)
        300))
    (call-method-12 999))
  301)

(deftest-error call-method-error.1
  (progn
    (define-method-combination call-method-error-1 ()
      ((list ()))
      `(call-method))
    (defgeneric call-method-error-2 (x) (:method-combination call-method-error-1))
    (defmethod call-method-error-2 (x)
      (declare (ignore x))
      100)
    (call-method-error-2 999)))

(deftest-error call-method-error.2
  (progn
    (define-method-combination call-method-error-3 ()
      ((list ()))
      `(call-method ,(car list) ,(cdr list) nil))
    (defgeneric call-method-error-4 (x) (:method-combination call-method-error-3))
    (defmethod call-method-error-4 (x)
      (declare (ignore x))
      100)
    (call-method-error-4 999)))

(deftest-error call-method-error.3
  (progn
    (define-method-combination call-method-error-5 ()
      ((list ()))
      `(call-method 999 ,(cdr list)))
    (defgeneric call-method-error-6 (x) (:method-combination call-method-error-5))
    (defmethod call-method-error-6 (x)
      (declare (ignore x))
      100)
    (call-method-error-6 999)))


;;
;;  Local Macro MAKE-METHOD
;;
(deftest-error make-method-error.1
  (progn
    (define-method-combination make-method-error-1 ()
      ((list ()))
      `(call-method
         (make-method)))
    (defgeneric make-method-error-2 (x) (:method-combination make-method-error-1))
    (defmethod make-method-error-2 (x)
      (declare (ignore x))
      100)
    (make-method-error-2 999)))

(deftest-error make-method-error.2
  (progn
    (define-method-combination make-method-error-3 ()
      ((list ()))
      `(call-method
         (make-method 200 300)))
    (defgeneric make-method-error-4 (x) (:method-combination make-method-error-3))
    (defmethod make-method-error-4 (x)
      (declare (ignore x))
      100)
    (make-method-error-4 999)))

