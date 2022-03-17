(cl:in-package cl-user)
(defpackage document (:use cl))
(in-package document)

(defconstant +input+ #p"load.lisp")
(defconstant +output+ #p"../document_contents.c")
(defconstant +gensym+ (gensym))

(defvar *function* (make-hash-table :test #'equal))
(defvar *variable* (make-hash-table :test #'equal))
(defvar *type* (make-hash-table :test #'equal))

(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

(defmacro acond (&rest args)
  (when args
    (destructuring-bind ((expr . body) . cdr) args
      (let ((g (gensym)))
        `(let ((,g ,expr))
           (if ,g
             (let ((it ,g))
               (declare (ignorable it))
               ,@body)
             (acond ,@cdr)))))))


;;
;;  read
;;
(defun read-call (stream call)
  (do (x) (nil)
    (setq x (read stream nil +gensym+))
    (if (eq x +gensym+)
      (return nil)
      (funcall call x))))

(defun symbol-table (symbol)
  (ecase symbol
    (function *function*)
    (variable *variable*)
    (type *type*)))

(defun exist-check (key symbol table)
  (mvbind (ignore check) (gethash key table)
    (declare (ignore ignore))
    (when check
      (error "The key ~S already exist in ~S." key symbol))))

(defun add-document (package name list symbol)
  (setq package (symbol-name package))
  (setq name (symbol-name name))
  (unless (stringp package)
    (error "Invalid package name, ~S." package))
  (unless (stringp name)
    (error "Invalid name name, ~S." name))
  (let ((key (cons package name))
        (table (symbol-table symbol)))
    (exist-check key symbol table)
    (setf (gethash key table) list)))

(defun main-symbol-p (x name)
  (and (consp x)
       (eq (car x) name)
       name))

(defun main-documentation (x symbol)
  (dbind (package name . data) (cdr x)
    (unless (symbolp name)
      (error "Invalid documentation name, ~S." name))
    (add-document package name data symbol)))

(declaim (ftype function read-file))
(defun main-load (x)
  (dbind (file) (cdr x)
    (read-file file)))

(defun main-read (x)
  (acond ((main-symbol-p x 'function)
          (main-documentation x it))
         ((main-symbol-p x 'variable)
          (main-documentation x it))
         ((main-symbol-p x 'type)
          (main-documentation x it))
         ((main-symbol-p x 'load)
          (main-load x))
         (t (error "Invalid syntax, ~S." x))))

(defun read-file (file)
  (with-open-file (input file)
    (read-call input #'main-read)))


;;
;;  write
;;
(defun gather-package (table)
  (let (list)
    (maphash
      (lambda (key value)
        (declare (ignore value))
        (dbind (package . name) key
          (declare (ignore name))
          (pushnew package list :test #'equal)))
      table)
    (sort list #'string<)))

(defun symbol-variable (x)
  (setq x (substitute #\_ #\- x))
  (setq x (substitute #\A #\* x))
  x)

(defun gather-documentation (name table)
  (let (list)
    (maphash
      (lambda (key value)
        (declare (ignore value))
        (dbind (x . y) key
          (when (string= name x)
            (push y list))))
      table)
    (sort list #'string<)))

(defun write-header (s)
  (format s "#include \"build_define.h\"~%")
  (format s "#include \"document_contents.h\"~%")
  (format s "#include <stdio.h>~2%"))

(defun write-define (s symbol)
  (format s "/*~%")
  (format s " *  ~A~%" symbol)
  (format s " */~%")
  (format s "#ifdef LISP_DOCUMENTATION~%"))

(defun write-encode (text)
  (prog1 text
    (map nil (lambda (c)
               (when (<= #x80 (char-code c))
                 (error "Invalid documentation, ~S." text)))
         text)))

(defun varname-text (symbol x y)
  (let ((x1 (symbol-variable x))
        (y1 (symbol-variable y)))
    (format nil "Document_~A_~A_~A" symbol x1 y1)))

(defun write-text-variable (s key text)
  (format s "static const char ~A[] =~%" key)
  (format s "    ")
  (dolist (x text)
    (if x
      (format s "~S" (write-encode x))
      (format s " \"\\n\"~%    ")))
  (format s ";~2%"))

(defun write-text-reference-p (text)
  (and (consp text)
       (eq (car text) :reference)))

(defun write-text-reference (text)
  (dbind (type package name) (cdr text)
    (let* ((table (symbol-table type))
           (package (symbol-name package))
           (name (symbol-name name))
           (key (cons package name)))
      (mvbind (ignore check) (gethash key table)
        (declare (ignore ignore))
        (unless check
          (error "The key ~S is not exist in ~S." key type))))))

(defun write-text-body (s key text)
  (if (write-text-reference-p text)
    (write-text-reference text)
    (write-text-variable s key text)))

(defun write-text (s symbol table list)
  (dolist (x list)
    (dolist (y (gather-documentation x table))
      (let* ((text (gethash (cons x y) table))
             (key (varname-text symbol x y)))
        (write-text-body s key text)))))

(defun write-document-struct-p (table x y)
  (write-text-reference-p
    (gethash (cons x y) table)))

(defun write-document-struct2 (s symbol x y y0)
  (let ((str (varname-text symbol x y)))
    (format s "    { \"~A\", ~A },~%" y0 str)))

(defun write-document-struct1 (s table x y)
  (let ((text (gethash (cons x y) table)))
    (dbind (type package name) (cdr text)
      (write-document-struct2
        s type
        (symbol-name package)
        (symbol-name name)
        y))))

(defun write-document-struct (s symbol table x y)
  (if (write-document-struct-p table x y)
    (write-document-struct1 s table x y)
    (write-document-struct2 s symbol x y y)))

(defun varname-struct (symbol x)
  (let ((x1 (symbol-variable x)))
    (format nil "Document_~A_~A" symbol x1)))

(defun varname-size (symbol x)
  (let ((x1 (symbol-variable x)))
    (format nil "DocumentSize_~A_~A" symbol x1)))

(defun write-document-list (s symbol table x)
  (let ((v1 (varname-struct symbol x))
        (v2 (varname-size symbol x))
        (size 0))
    (format s "static struct DocumentStruct ~A[] = {~%" v1)
    (dolist (y (gather-documentation x table))
      (write-document-struct s symbol table x y)
      (incf size 1))
    (format s "    { NULL, NULL }~%")
    (format s "};~2%")
    (format s "#define ~A ~A~2%" v2 size)))

(defun write-document (s symbol table list)
  (dolist (x list)
    (write-document-list s symbol table x)))

(defun varname-package (x)
  (cond ((string= x "LISP-SYSTEM") (symbol-variable x))
        ((string= x "COMMON-LISP") "LISP_COMMON")
        (t (format nil "\"~A\"" x))))

(defun write-package (s symbol list)
  (format s "struct DocumentPackage Document_~A[] = {~%" symbol)
  (dolist (x list)
    (let ((v1 (varname-package x))
          (v2 (varname-struct symbol x))
          (v3 (varname-size symbol x)))
      (format s "    { ~A, ~A, ~A },~%" v1 v2 v3)))
  (format s "    { NULL, NULL, 0 }~%")
  (format s "};~%"))

(defun write-else (s symbol)
  (format s "#else~%")
  (format s "struct DocumentPackage Document_~A[] = {~%" symbol)
  (format s "    { NULL, NULL, 0 }~%")
  (format s "};~%")
  (format s "#endif~%"))

(defun write-symbol (stream symbol)
  (let* ((table (symbol-table symbol))
         (list (gather-package table)))
    (write-define stream symbol)
    (write-text stream symbol table list)
    (write-document stream symbol table list)
    (write-package stream symbol list)
    (write-else stream symbol)
    (terpri stream)
    (terpri stream)))

(defun write-call (stream)
  (write-header stream)
  (write-symbol stream 'function)
  (write-symbol stream 'variable)
  (write-symbol stream 'type))

(defun write-file (file)
  (with-open-file (output file :direction :output :if-exists :supersede)
    (write-call output)))


;;
;;  main
;;
(defun main ()
  (read-file +input+)
  (write-file +output+))
(main)

