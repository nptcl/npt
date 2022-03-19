;;
;;  MetaObject Protocol: Class
;;
(let* ((type (lisp-implementation-type))
       (closname (format nil "~A-CLOS" type)))
  (require closname))

