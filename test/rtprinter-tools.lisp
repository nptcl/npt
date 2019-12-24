;;
;;  ANSI COMMON LISP: 22. Printer
;;
(defvar *default-print-dispatch* *print-pprint-dispatch*)
(defmacro with-default-print (&body body)
  `(let ((*print-array* t)
         (*print-base* 10)
         (*print-radix* nil)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-level* nil)
         (*print-length* nil)
         (*print-pprint-dispatch* *default-print-dispatch*)
         (*print-pretty* nil)
         (*print-readably* nil)
         (*print-right-margin* nil)
         (*read-default-float-format* 'single-float))
     ,@body))

(defmacro with-pretty-print (&body body)
  `(with-default-print
     (let ((*print-pretty* t))
       ,@body)))

(defun mkstr (&rest args)
  (apply #'concatenate 'string (mapcar #'string args)))

(defmacro pprint-logical-block-output
  ((margin &optional width &rest args &key &allow-other-keys) &body body)
  `(let ((*print-pretty* t)
         (*print-right-margin* ,margin)
         (*print-miser-width* ,width))
     (with-output-to-string (*standard-output*)
       (pprint-logical-block (nil nil ,@args)
         ,@body))))

(defmacro format-nil (str &rest args)
  ;;`(format (formatter ,str) ,@args)
  `(format nil ,str ,@args))

