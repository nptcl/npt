;;
;;  COMMON-LISP: 14. Conses
;;

;;
;;  System Class LIST
;;
(type
  common-lisp list
  "Class Precedence List: list, sequence, t" nil
  "A list is a chain of conses in which the car of each cons is an element of the list, and the cdr of each cons is either the next link in the chain or a terminating atom." nil
  "A proper list is a chain of conses terminated by the empty list, (), which is itself a proper list. A dotted list is a list which has a terminating atom that is not the empty list. A circular list is a chain of conses that has no termination because some cons in the chain is the cdr of a later cons." nil
  "Dotted lists and circular lists are also lists, but usually the unqualified term ``list'' within this specification means proper list. Nevertheless, the type list unambiguously includes dotted lists and circular lists." nil
  "For each element of a list there is a cons. The empty list has no elements and is not a cons." nil
  "The types cons and null form an exhaustive partition of the type list." nil)


;;
;;  System Class NULL
;;
(type
  common-lisp null
  "Class Precedence List: null, symbol, list, sequence, t" nil
  "The only object of type null is nil, which represents the empty list and can also be notated ()." nil)


;;
;;  System Class CONS
;;
(type
  common-lisp cons
  "Class Precedence List cons, list, sequence, t" nil
  "A cons is a compound object having two components, called the car and cdr. These form a dotted pair. Each component can be any object." nil)


;;
;;  Type ATOM
;;
(type
  common-lisp atom
  "Supertypes: atom, t" nil
  "It is equivalent to (not cons)." nil)


;;
;;  Function CONS
;;
(function
  common-lisp cons
  "Syntax: cons object-1 object-2 => cons" nil
  "Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2." nil)


;;  Function CONSP
;;  Function ATOM
;;  Function RPLACA, RPLACD
;;  Accessor CAR, CDR, CAAR, CADR, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, CDAAR, CDADR, CDDAR, CDDDR, CAAAAR, CAAADR, CAADAR, CAADDR, CADAAR, CADADR, CADDAR, CADDDR, CDAAAR, CDAADR, CDADAR, CDADDR, CDDAAR, CDDADR, CDDDAR, CDDDDR
;;  Function COPY-TREE
;;  Function SUBLIS, NSUBLIS
;;  Function SUBST, SUBST-IF, SUBST-IF-NOT, NSUBST, NSUBST-IF, NSUBST-IF-NOT


;;
;;  Function TREE-EQUAL
;;
(function
  common-lisp tree-equal
  "Syntax: tree-equal tree-1 tree-2 &key test test-not => generalized-boolean" nil
  "tree-equal tests whether two trees are of the same shape and have the same leaves. tree-equal returns true if tree-1 and tree-2 are both atoms and satisfy the test, or if they are both conses and the car of tree-1 is tree-equal to the car of tree-2 and the cdr of tree-1 is tree-equal to the cdr of tree-2. Otherwise, tree-equal returns false." nil
  "tree-equal recursively compares conses but not any other objects that have components." nil
  "The first argument to the :test or :test-not function is tree-1 or a car or cdr of tree-1; the second argument is tree-2 or a car or cdr of tree-2." nil)


;;
;;  Function COPY-LIST
;;
(function
  common-lisp copy-list
  "Syntax: copy-list list => copy" nil
  "Returns a copy of list. If list is a dotted list, the resulting list will also be a dotted list." nil
  "Only the list structure of list is copied; the elements of the resulting list are the same as the corresponding elements of the given list." nil)


;;
;;  Function LIST
;;
(function
  common-lisp list
  "Syntax: list &rest objects => list" nil
  "Syntax: list* &rest objects+ => result" nil
  "list returns a list containing the supplied objects." nil
  "list* is like list except that the last argument to list becomes the car of the last cons constructed, while the last argument to list* becomes the cdr of the last cons constructed. Hence, any given call to list* always produces one fewer conses than a call to list with the same number of arguments." nil
  "If the last argument to list* is a list, the effect is to construct a new list which is similar, but which has additional elements added to the front corresponding to the preceding arguments of list*." nil
  "If list* receives only one object, that object is returned, regardless of whether or not it is a list." nil)


;;
;;  Function LIST*
;;
(function
  common-lisp list*
  :reference function common-lisp list)


;;  Function LIST-LENGTH
;;  Function LISTP
;;  Function MAKE-LIST


;;
;;  Macro PUSH
;;
(function
  common-lisp push
  "Syntax: push item place => new-place-value" nil
  "push prepends item to the list that is stored in place, stores the resulting list in place, and returns the list." nil)


;;
;;  Macro POP
;;
(function
  common-lisp pop
  "Syntax: pop place => element" nil
  "pop reads the value of place, remembers the car of the list which was retrieved, writes the cdr of the list back into the place, and finally yields the car of the originally retrieved list." nil)


;;  Accessor FIRST, SECOND, THIRD, FOURTH, FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH


;;
;;  Accessor NTH
;;
(function
  common-lisp nth
  "Syntax: nth n list => object" nil
  "Syntax: (setf (nth n list) new-object)" nil
  "nth locates the nth element of list, where the car of the list is the ``zeroth'' element. Specifically," nil
  "(nth n list) ==  (car (nthcdr n list))" nil
  "nth may be used to specify a place to setf. Specifically," nil
  "(setf (nth n list) new-object) ==  (setf (car (nthcdr n list)) new-object)" nil)

