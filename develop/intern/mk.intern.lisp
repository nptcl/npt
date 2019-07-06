(defpackage make-intern-lisp (:use cl))
(in-package make-intern-lisp)

(defconstant +input+ #p"intern.lisp")
(defconstant +output-symbol-64+ #p"intern_symbol_64.h")
(defconstant +output-symbol-32+ #p"intern_symbol_32.h")
(defconstant +output-const+ #p"intern_const.h")
(defconstant +output-count+ #p"intern_count.h")
(make-package 'lisp-clos :use nil)
(make-package 'lisp-system :use nil)
(make-package 'lisp-code :use nil)
(make-package 'lisp-rt :use nil)
(defvar *bit*)
(defvar *list*)
(defvar *count*)
(defvar *exist*)

;;
;;  tools
;;
(defmacro with-overwrite-file ((file) &body body)
  `(with-open-file (*standard-output*
                     ,file :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
     ,@body))

(let ((g (gensym)))
  (defun read-list-stream (x)
    (do (result var) (nil)
      (setq var (read x nil g))
      (if (eq var g)
        (return (nreverse result))
        (push var result)))))

(defun read-list (x)
  (if (streamp x)
    (read-list-stream x)
    (with-open-file (stream x :direction :input)
      (read-list-stream stream))))


;;
;;  sxhash-string
;;
(defun sxhash-plus (a b size)
  (mod (+ a b) (ash 1 (1- (* size 8)))))

(defun sxhash-string (name size)
  (let ((value (length name))
        (p (make-array size :initial-element 0))
        (mask (ash 1 (1- (* size 8))))
        m)
    (dotimes (i value)
      (setq m (mod i size))
      (setf (elt p m)
            (sxhash-plus (elt p m) (char-code (char name i)) size)))
    (dotimes (i size)
      (setq value
            (sxhash-plus value (ash (elt p i) (* i 8)) size)))
    (mod value mask)))

(defun sxhash-value (name bit)
  (format nil (if (eql bit 64)
                "0x~16,'0XULL"
                "0x~8,'0XULL")
          (sxhash-string name (truncate bit 8))))


;;
;;  read-intern-lisp
;;
(defun hyphen-underline (str)
  (substitute #\_ #\- (princ-to-string str)))

(defun special-name (name)
  (remove #\* name :test #'eql))

(defun special-name-p (name)
  (let ((size (length name)))
    (and (< 2 size)
         (char= (char name 0) #\*)
         (char/= (char name 1) #\*)
         (char= (char name (1- size)) #\*))))

(defun symbol-package! (x)
  (let ((p (symbol-package x))
        (s (find-symbol (symbol-name x) 'common-lisp)))
    (if (eq x s)
      (find-package 'common-lisp)
      p)))

(defun intern-package-value (x)
  (package-name
    (symbol-package! x)))

(defun intern-package (x)
  (let ((x (intern-package-value x)))
    (cond ((string= x "COMMON-LISP") "COMMON")
          ((string= x "KEYWORD") "KEYWORD")
          ((string= x "LISP-SYSTEM") "SYSTEM")
          ((string= x "LISP-CLOS") "CLOS")
          ((string= x "LISP-CODE") "CODE")
          ((string= x "LISP-RT") "RT")
          (t (error "Invalid package name ~S." x)))))

(defun intern-constant-name (x)
  (when (symbolp x)
    (setq x (symbol-name x)))
  (hyphen-underline
    (special-name x)))

(defun intern-constant-value (name const)
  (format nil "CONSTANT_~A_~A"
          (intern-constant-name const)
          (intern-constant-name name)))

(defun intern-constant-package (x special)
  (let ((x (intern-package-value x)))
    (cond ((string= x "COMMON-LISP")
           (if special "SPECIAL" "COMMON"))
          ((string= x "KEYWORD") "KEYWORD")
          ((string= x "LISP-SYSTEM") "SYSTEM")
          ((string= x "LISP-CLOS") "CLOSNAME")
          ((string= x "LISP-CODE") "CODE")
          ((string= x "LISP-RT") "RT")
          (t (error "Invalid package name ~S." x)))))

(defun intern-constant-symbol (name x special)
  (intern-constant-value
    name
    (intern-constant-package x special)))

;; (list name package special const)
(defun intern-symbol (x &key (name nil name-p)
                        (constant nil constant-p)
                        (special nil special-p)
                        &aux package sym)
  ;; symbol
  (when (consp x)
    (destructuring-bind (a b) x
      (setq x (intern b a))))
  (setq sym (symbol-name x))
  ;; name
  (setq name (symbol-name
               (if name-p name x)))
  ;; package
  (setq package (intern-package x))
  ;; special
  (unless special-p
    (setq special (special-name-p sym)))
  ;; constant
  (setq constant (if constant-p
                   (intern-constant-value name constant)
                   (intern-constant-symbol name x special)))
  ;; list
  (list sym package (if special 1 0) constant))

(defun intern-special-p (x)
  (nth 2 x))

(defun intern-const (x)
  (nth 3 x))

(defun intern-sort (list)
  (stable-sort list #'< :key #'intern-special-p))

(defun read-intern-lisp (file &aux list)
  (dolist (x (read-list file))
    (if (symbolp x)
      (push (intern-symbol x) list)
      (push (apply #'intern-symbol x) list)))
  (setq *list* (intern-sort (nreverse list))))


;;
;;  output-symbol
;;
(defun count-package (name)
  (let ((value (gethash name *count*)))
    (if value
      (incf value 1)
      (setf value 1))
    (setf (gethash name *count*) value)))

(defun output-exist (package name)
  (let ((key (cons package name)))
    (if (shiftf (gethash key *exist*) t) 1 0)))

(defun output-list (name package special const)
  (let* ((size (length name))
         (hash (sxhash-value name *bit*))
         (find (output-exist package name)))
    (count-package package)
    (format t "{ ~A, ~A, ~S, ~A, ~A, ~A, ~A },~%"
            const package name size hash special find)))


(defun output-symbol (file bit)
  (let ((*bit* bit))
    (with-overwrite-file (file)
      (clrhash *count*)
      (clrhash *exist*)
      (dolist (x *list*)
        (apply #'output-list x)))))


;;
;;  output-const
;;
(defun output-const (file)
  (with-overwrite-file (file)
    (dolist (x *list*)
      (format t "~A,~%" (intern-const x)))))


;;
;;  output-count
;;
(defun hash-table-keys (hash &aux list)
  (maphash
    (lambda (k v)
      (declare (ignore v))
      (push k list))
    hash)
  list)

(defun output-count (file)
  (let ((list (hash-table-keys *count*)))
    (with-overwrite-file (file)
      (dolist (x (sort list #'string<))
        (format t "#define LISP_PACKAGE_COUNT_~A ~A~%" x (gethash x *count*))))))


;;
;;  main
;;
(defmacro with-intern-symbol (&body body)
  `(let ((*count* (make-hash-table :test 'equal))
         (*exist* (make-hash-table :test 'equal))
         (*list*))
     ,@body))

(defun main ()
  (with-intern-symbol
    (read-intern-lisp +input+)
    (output-symbol +output-symbol-64+ 64)
    (output-symbol +output-symbol-32+ 32)
    (output-const +output-const+)
    (output-count +output-count+)))
(main)

