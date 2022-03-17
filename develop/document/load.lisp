;;
;; (symbol package name . body)
;;    symbol:  function, type, variable
;;      name:  name, (setf name)
;;      body:  text..., (:reference symbol package name)
;;

;;  4. Types and Classes
(load #p"document-types.lisp")
;;  5. Data and Control Flow
(load #p"document-data.lisp")
;;  7. Objects
(load #p"document-objects.lisp")
;;  14. Conses
(load #p"document-conses.lisp")
;;  LISP-SYSTEM
(load #p"document-external.lisp")

