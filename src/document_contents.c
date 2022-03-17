#include "build_define.h"
#include "document_contents.h"
#include <stdio.h>

/*
 *  FUNCTION
 */
#ifdef LISP_DOCUMENTATION
static const char Document_FUNCTION_COMMON_LISP_CONS[] =
    "Syntax: cons object-1 object-2 => cons" "\n"
    "Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_COPY_LIST[] =
    "Syntax: copy-list list => copy" "\n"
    "Returns a copy of list. If list is a dotted list, the resulting list will also be a dotted list." "\n"
    "Only the list structure of list is copied; the elements of the resulting list are the same as the corresponding elements of the given list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_FUNCTION_KEYWORDS[] =
    "Syntax: function-keywords method => keys, allow-other-keys-p" "\n"
    "Method Signatures: function-keywords (method standard-method)" "\n"
    "Returns the keyword parameter specifiers for a method." "\n"
    "Two values are returned: a list of the explicitly named keywords and a generalized boolean that states whether &allow-other-keys had been specified in the method definition." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_LIST[] =
    "Syntax: list &rest objects => list" "\n"
    "Syntax: list* &rest objects+ => result" "\n"
    "list returns a list containing the supplied objects." "\n"
    "list* is like list except that the last argument to list becomes the car of the last cons constructed, while the last argument to list* becomes the cdr of the last cons constructed. Hence, any given call to list* always produces one fewer conses than a call to list with the same number of arguments." "\n"
    "If the last argument to list* is a list, the effect is to construct a new list which is similar, but which has additional elements added to the front corresponding to the preceding arguments of list*." "\n"
    "If list* receives only one object, that object is returned, regardless of whether or not it is a list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_NTH[] =
    "Syntax: nth n list => object" "\n"
    "Syntax: (setf (nth n list) new-object)" "\n"
    "nth locates the nth element of list, where the car of the list is the ``zeroth'' element. Specifically," "\n"
    "(nth n list) ==  (car (nthcdr n list))" "\n"
    "nth may be used to specify a place to setf. Specifically," "\n"
    "(setf (nth n list) new-object) ==  (setf (car (nthcdr n list)) new-object)" "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_POP[] =
    "Syntax: pop place => element" "\n"
    "pop reads the value of place, remembers the car of the list which was retrieved, writes the cdr of the list back into the place, and finally yields the car of the originally retrieved list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_PUSH[] =
    "Syntax: push item place => new-place-value" "\n"
    "push prepends item to the list that is stored in place, stores the resulting list in place, and returns the list." "\n"
    ;

static const char Document_FUNCTION_COMMON_LISP_TREE_EQUAL[] =
    "Syntax: tree-equal tree-1 tree-2 &key test test-not => generalized-boolean" "\n"
    "tree-equal tests whether two trees are of the same shape and have the same leaves. tree-equal returns true if tree-1 and tree-2 are both atoms and satisfy the test, or if they are both conses and the car of tree-1 is tree-equal to the car of tree-2 and the cdr of tree-1 is tree-equal to the cdr of tree-2. Otherwise, tree-equal returns false." "\n"
    "tree-equal recursively compares conses but not any other objects that have components." "\n"
    "The first argument to the :test or :test-not function is tree-1 or a car or cdr of tree-1; the second argument is tree-2 or a car or cdr of tree-2." "\n"
    ;

static const char Document_FUNCTION_LISP_SYSTEM_DLCALL[] =
    "Dynamic Link Call.";

static const char Document_FUNCTION_LISP_SYSTEM_DLFILE[] =
    "Dynamic Link Control.";

static struct DocumentStruct Document_FUNCTION_COMMON_LISP[] = {
    { "CONS", Document_FUNCTION_COMMON_LISP_CONS },
    { "COPY-LIST", Document_FUNCTION_COMMON_LISP_COPY_LIST },
    { "FUNCTION-KEYWORDS", Document_FUNCTION_COMMON_LISP_FUNCTION_KEYWORDS },
    { "LIST", Document_FUNCTION_COMMON_LISP_LIST },
    { "LIST*", Document_FUNCTION_COMMON_LISP_LIST },
    { "NTH", Document_FUNCTION_COMMON_LISP_NTH },
    { "POP", Document_FUNCTION_COMMON_LISP_POP },
    { "PUSH", Document_FUNCTION_COMMON_LISP_PUSH },
    { "TREE-EQUAL", Document_FUNCTION_COMMON_LISP_TREE_EQUAL },
    { NULL, NULL }
};

#define DocumentSize_FUNCTION_COMMON_LISP 9

static struct DocumentStruct Document_FUNCTION_LISP_SYSTEM[] = {
    { "DLCALL", Document_FUNCTION_LISP_SYSTEM_DLCALL },
    { "DLFILE", Document_FUNCTION_LISP_SYSTEM_DLFILE },
    { NULL, NULL }
};

#define DocumentSize_FUNCTION_LISP_SYSTEM 2

struct DocumentPackage Document_FUNCTION[] = {
    { LISP_COMMON, Document_FUNCTION_COMMON_LISP, DocumentSize_FUNCTION_COMMON_LISP },
    { LISP_SYSTEM, Document_FUNCTION_LISP_SYSTEM, DocumentSize_FUNCTION_LISP_SYSTEM },
    { NULL, NULL, 0 }
};
#else
struct DocumentPackage Document_FUNCTION[] = {
    { NULL, NULL, 0 }
};
#endif


/*
 *  VARIABLE
 */
#ifdef LISP_DOCUMENTATION
static const char Document_VARIABLE_COMMON_LISP_CALL_ARGUMENTS_LIMIT[] =
    "Constant Value: An integer not smaller than 50 and at least as great as the value of lambda-parameters-limit, the exact magnitude of which is implementation-dependent." "\n"
    "Description: The upper exclusive bound on the number of arguments that may be passed to a function." "\n"
    ;

static const char Document_VARIABLE_COMMON_LISP_LAMBDA_LIST_KEYWORDS[] =
    "Constant Value: a list, the elements of which are implementation-dependent, but which must contain at least the symbols &allow-other-keys, &aux, &body, &environment, &key, &optional, &rest, and &whole." "\n"
    "Description: A list of all the lambda list keywords used in the implementation, including the additional ones used only by macro definition forms." "\n"
    ;

static const char Document_VARIABLE_COMMON_LISP_LAMBDA_PARAMETERS_LIMIT[] =
    "Constant Value: implementation-dependent, but not smaller than 50." "\n"
    "Description: A positive integer that is the upper exclusive bound on the number of parameter names that can appear in a single lambda list." "\n"
    ;

static struct DocumentStruct Document_VARIABLE_COMMON_LISP[] = {
    { "CALL-ARGUMENTS-LIMIT", Document_VARIABLE_COMMON_LISP_CALL_ARGUMENTS_LIMIT },
    { "LAMBDA-LIST-KEYWORDS", Document_VARIABLE_COMMON_LISP_LAMBDA_LIST_KEYWORDS },
    { "LAMBDA-PARAMETERS-LIMIT", Document_VARIABLE_COMMON_LISP_LAMBDA_PARAMETERS_LIMIT },
    { NULL, NULL }
};

#define DocumentSize_VARIABLE_COMMON_LISP 3

struct DocumentPackage Document_VARIABLE[] = {
    { LISP_COMMON, Document_VARIABLE_COMMON_LISP, DocumentSize_VARIABLE_COMMON_LISP },
    { NULL, NULL, 0 }
};
#else
struct DocumentPackage Document_VARIABLE[] = {
    { NULL, NULL, 0 }
};
#endif


/*
 *  TYPE
 */
#ifdef LISP_DOCUMENTATION
static const char Document_TYPE_COMMON_LISP_ATOM[] =
    "Supertypes: atom, t" "\n"
    "It is equivalent to (not cons)." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_CONS[] =
    "Class Precedence List cons, list, sequence, t" "\n"
    "A cons is a compound object having two components, called the car and cdr. These form a dotted pair. Each component can be any object." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_LIST[] =
    "Class Precedence List: list, sequence, t" "\n"
    "A list is a chain of conses in which the car of each cons is an element of the list, and the cdr of each cons is either the next link in the chain or a terminating atom." "\n"
    "A proper list is a chain of conses terminated by the empty list, (), which is itself a proper list. A dotted list is a list which has a terminating atom that is not the empty list. A circular list is a chain of conses that has no termination because some cons in the chain is the cdr of a later cons." "\n"
    "Dotted lists and circular lists are also lists, but usually the unqualified term ``list'' within this specification means proper list. Nevertheless, the type list unambiguously includes dotted lists and circular lists." "\n"
    "For each element of a list there is a cons. The empty list has no elements and is not a cons." "\n"
    "The types cons and null form an exhaustive partition of the type list." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_NULL[] =
    "Class Precedence List: null, symbol, list, sequence, t" "\n"
    "The only object of type null is nil, which represents the empty list and can also be notated ()." "\n"
    ;

static const char Document_TYPE_COMMON_LISP_STANDARD_CLASS[] =
    "Class Precedence List: standard-class, class, standard-object, t" "\n"
    "The class standard-class is the default class of classes defined by defclass." "\n"
    ;

static struct DocumentStruct Document_TYPE_COMMON_LISP[] = {
    { "ATOM", Document_TYPE_COMMON_LISP_ATOM },
    { "CONS", Document_TYPE_COMMON_LISP_CONS },
    { "LIST", Document_TYPE_COMMON_LISP_LIST },
    { "NULL", Document_TYPE_COMMON_LISP_NULL },
    { "STANDARD-CLASS", Document_TYPE_COMMON_LISP_STANDARD_CLASS },
    { NULL, NULL }
};

#define DocumentSize_TYPE_COMMON_LISP 5

struct DocumentPackage Document_TYPE[] = {
    { LISP_COMMON, Document_TYPE_COMMON_LISP, DocumentSize_TYPE_COMMON_LISP },
    { NULL, NULL, 0 }
};
#else
struct DocumentPackage Document_TYPE[] = {
    { NULL, NULL, 0 }
};
#endif


