;;
;;  COMMON-LISP: 5. Data and Control Flow
;;

;;
;;  Constant Variable CALL-ARGUMENTS-LIMIT
;;
(variable
  common-lisp call-arguments-limit
  "Constant Value: An integer not smaller than 50 and at least as great as the value of lambda-parameters-limit, the exact magnitude of which is implementation-dependent." nil
  "Description: The upper exclusive bound on the number of arguments that may be passed to a function." nil)


;;
;;  Constant Variable LAMBDA-LIST-KEYWORDS
;;
(variable
  common-lisp lambda-list-keywords
  "Constant Value: a list, the elements of which are implementation-dependent, but which must contain at least the symbols &allow-other-keys, &aux, &body, &environment, &key, &optional, &rest, and &whole." nil
  "Description: A list of all the lambda list keywords used in the implementation, including the additional ones used only by macro definition forms." nil)


;;
;;  Constant Variable LAMBDA-PARAMETERS-LIMIT
;;
(variable
  common-lisp lambda-parameters-limit
  "Constant Value: implementation-dependent, but not smaller than 50." nil
  "Description: A positive integer that is the upper exclusive bound on the number of parameter names that can appear in a single lambda list." nil)

