#ifndef __LISP_EXTERN_TYPEDEF_HEADER__
#define __LISP_EXTERN_TYPEDEF_HEADER__

#include "typedef.h"

enum lisp_escape {
	lisp_escape_normal,
	lisp_escape_tagbody,
	lisp_escape_block,
	lisp_escape_catch,
	lisp_escape_handler_case,
	lisp_escape_restart_case
};

enum LispEastAsianType {
	LispEastAsianType_error,
	LispEastAsianType_N,
	LispEastAsianType_A,
	LispEastAsianType_H,
	LispEastAsianType_W,
	LispEastAsianType_F,
	LispEastAsianType_NA
};

typedef int (*lisp_calltype_macro)(addr form, addr env);
typedef int (*lisp_calltype_rest)(addr args);
typedef int (*lisp_calltype_dynamic)(addr args);
typedef int (*lisp_calltype_any)(void);
typedef int (*lisp_calltype_empty)(void);
typedef int (*lisp_calltype_var1)(addr);
typedef int (*lisp_calltype_var2)(addr, addr);
typedef int (*lisp_calltype_var3)(addr, addr, addr);
typedef int (*lisp_calltype_var4)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var5)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var6)(addr, addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_opt1)(addr);
typedef int (*lisp_calltype_opt2)(addr, addr);
typedef int (*lisp_calltype_opt3)(addr, addr, addr);
typedef int (*lisp_calltype_var1opt1)(addr, addr);
typedef int (*lisp_calltype_var1opt2)(addr, addr, addr);
typedef int (*lisp_calltype_var1opt3)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var2opt1)(addr, addr, addr);
typedef int (*lisp_calltype_var2opt2)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var2opt3)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt1)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt2)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt3)(addr, addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var1rest)(addr, addr);
typedef int (*lisp_calltype_var2rest)(addr, addr, addr);
typedef int (*lisp_calltype_var3rest)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var1dynamic)(addr, addr);
typedef int (*lisp_calltype_var2dynamic)(addr, addr, addr);
typedef int (*lisp_calltype_var3dynamic)(addr, addr, addr, addr);

#endif

