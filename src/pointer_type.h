#ifndef __POINTER_TYPE_HEADER__
#define __POINTER_TYPE_HEADER__

#include "execute.h"
#include "typedef.h"

typedef void *callbind_error;
typedef int (*callbind_code)(Execute, CodeValue);
typedef int (*callbind_macro)(Execute, addr, addr);
typedef int (*callbind_none)(void);
typedef int (*callbind_any)(Execute);
typedef int (*callbind_empty)(Execute);
typedef int (*callbind_rest)(Execute, addr);
typedef int (*callbind_dynamic)(Execute, addr);
typedef int (*callbind_var1)(Execute, addr);
typedef int (*callbind_var2)(Execute, addr, addr);
typedef int (*callbind_var3)(Execute, addr, addr, addr);
typedef int (*callbind_var4)(Execute, addr, addr, addr, addr);
typedef int (*callbind_var5)(Execute, addr, addr, addr, addr, addr);
typedef int (*callbind_var6)(Execute, addr, addr, addr, addr, addr, addr);
typedef int (*callbind_opt1)(Execute, addr);
typedef int (*callbind_opt2)(Execute, addr, addr);
typedef int (*callbind_opt3)(Execute, addr, addr, addr);
typedef int (*callbind_opt4)(Execute, addr, addr, addr, addr);
typedef int (*callbind_opt5)(Execute, addr, addr, addr, addr, addr);
typedef int (*callbind_var1opt1)(Execute, addr, addr);
typedef int (*callbind_var2opt1)(Execute, addr, addr, addr);
typedef int (*callbind_var3opt1)(Execute, addr, addr, addr, addr);
typedef int (*callbind_var4opt1)(Execute, addr, addr, addr, addr, addr);
typedef int (*callbind_var5opt1)(Execute, addr, addr, addr, addr, addr, addr);
typedef int (*callbind_var1opt2)(Execute, addr, addr, addr);
typedef int (*callbind_var2opt2)(Execute, addr, addr, addr, addr);
typedef int (*callbind_var2opt3)(Execute, addr, addr, addr, addr, addr);
typedef int (*callbind_var1rest)(Execute, addr, addr);
typedef int (*callbind_var2rest)(Execute, addr, addr, addr);
typedef int (*callbind_var3rest)(Execute, addr, addr, addr, addr);
typedef int (*callbind_var4rest)(Execute, addr, addr, addr, addr, addr);
typedef int (*callbind_opt1rest)(Execute, addr, addr);
typedef int (*callbind_var1dynamic)(Execute, addr, addr);
typedef int (*callbind_var2dynamic)(Execute, addr, addr, addr);
typedef int (*callbind_var3dynamic)(Execute, addr, addr, addr, addr);
typedef int (*callbind_var4dynamic)(Execute, addr, addr, addr, addr, addr);
typedef int (*callbind_opt1dynamic)(Execute, addr, addr);

typedef int (*callbind_extend_macro)(addr, addr);
typedef int (*callbind_extend_rest)(addr);
typedef int (*callbind_extend_dynamic)(addr);
typedef int (*callbind_extend_any)();
typedef int (*callbind_extend_empty)();
typedef int (*callbind_extend_var1)(addr);
typedef int (*callbind_extend_var2)(addr, addr);
typedef int (*callbind_extend_var3)(addr, addr, addr);
typedef int (*callbind_extend_var4)(addr, addr, addr, addr);
typedef int (*callbind_extend_var5)(addr, addr, addr, addr, addr);
typedef int (*callbind_extend_var6)(addr, addr, addr, addr, addr, addr);
typedef int (*callbind_extend_opt1)(addr);
typedef int (*callbind_extend_opt2)(addr, addr);
typedef int (*callbind_extend_opt3)(addr, addr, addr);
typedef int (*callbind_extend_var1opt1)(addr, addr);
typedef int (*callbind_extend_var1opt2)(addr, addr, addr);
typedef int (*callbind_extend_var1opt3)(addr, addr, addr, addr);
typedef int (*callbind_extend_var2opt1)(addr, addr, addr);
typedef int (*callbind_extend_var2opt2)(addr, addr, addr, addr);
typedef int (*callbind_extend_var2opt3)(addr, addr, addr, addr, addr);
typedef int (*callbind_extend_var3opt1)(addr, addr, addr, addr);
typedef int (*callbind_extend_var3opt2)(addr, addr, addr, addr, addr);
typedef int (*callbind_extend_var3opt3)(addr, addr, addr, addr, addr, addr);
typedef int (*callbind_extend_var1rest)(addr, addr);
typedef int (*callbind_extend_var2rest)(addr, addr, addr);
typedef int (*callbind_extend_var3rest)(addr, addr, addr, addr);
typedef int (*callbind_extend_var1dynamic)(addr, addr);
typedef int (*callbind_extend_var2dynamic)(addr, addr, addr);
typedef int (*callbind_extend_var3dynamic)(addr, addr, addr, addr);

enum CallBind_index {
	CallBind_error,
	CallBind_code,
	CallBind_macro,
	CallBind_none,
	CallBind_any,
	CallBind_empty,
	CallBind_rest,
	CallBind_dynamic,
	CallBind_var1,
	CallBind_var2,
	CallBind_var3,
	CallBind_var4,
	CallBind_var5,
	CallBind_var6,
	CallBind_opt1,
	CallBind_opt2,
	CallBind_opt3,
	CallBind_opt4,
	CallBind_opt5,
	CallBind_var1opt1,
	CallBind_var2opt1,
	CallBind_var3opt1,
	CallBind_var4opt1,
	CallBind_var5opt1,
	CallBind_var1opt2,
	CallBind_var2opt2,
	CallBind_var2opt3,
	CallBind_var1rest,
	CallBind_var2rest,
	CallBind_var3rest,
	CallBind_var4rest,
	CallBind_opt1rest,
	CallBind_var1dynamic,
	CallBind_var2dynamic,
	CallBind_var3dynamic,
	CallBind_var4dynamic,
	CallBind_opt1dynamic,

	CallBind_extend_macro,
	CallBind_extend_rest,
	CallBind_extend_dynamic,
	CallBind_extend_any,
	CallBind_extend_empty,
	CallBind_extend_var1,
	CallBind_extend_var2,
	CallBind_extend_var3,
	CallBind_extend_var4,
	CallBind_extend_var5,
	CallBind_extend_var6,
	CallBind_extend_opt1,
	CallBind_extend_opt2,
	CallBind_extend_opt3,
	CallBind_extend_var1opt1,
	CallBind_extend_var1opt2,
	CallBind_extend_var1opt3,
	CallBind_extend_var2opt1,
	CallBind_extend_var2opt2,
	CallBind_extend_var2opt3,
	CallBind_extend_var3opt1,
	CallBind_extend_var3opt2,
	CallBind_extend_var3opt3,
	CallBind_extend_var1rest,
	CallBind_extend_var2rest,
	CallBind_extend_var3rest,
	CallBind_extend_var1dynamic,
	CallBind_extend_var2dynamic,
	CallBind_extend_var3dynamic,

	CallBind_size
};

struct callbind_struct {
	enum CallBind_index type;
	union {
		callbind_error error;
		callbind_code code;
		callbind_macro macro;
		callbind_none none;
		callbind_any any;
		callbind_empty empty;
		callbind_rest rest;
		callbind_dynamic dynamic;
		callbind_var1 var1;
		callbind_var2 var2;
		callbind_var3 var3;
		callbind_var4 var4;
		callbind_var5 var5;
		callbind_var6 var6;
		callbind_opt1 opt1;
		callbind_opt2 opt2;
		callbind_opt3 opt3;
		callbind_opt4 opt4;
		callbind_opt5 opt5;
		callbind_var1opt1 var1opt1;
		callbind_var2opt1 var2opt1;
		callbind_var3opt1 var3opt1;
		callbind_var4opt1 var4opt1;
		callbind_var5opt1 var5opt1;
		callbind_var1opt2 var1opt2;
		callbind_var2opt2 var2opt2;
		callbind_var2opt3 var2opt3;
		callbind_var1rest var1rest;
		callbind_var2rest var2rest;
		callbind_var3rest var3rest;
		callbind_var4rest var4rest;
		callbind_opt1rest opt1rest;
		callbind_var1dynamic var1dynamic;
		callbind_var2dynamic var2dynamic;
		callbind_var3dynamic var3dynamic;
		callbind_var4dynamic var4dynamic;
		callbind_opt1dynamic opt1dynamic;

		callbind_extend_macro extend_macro;
		callbind_extend_rest extend_rest;
		callbind_extend_dynamic extend_dynamic;
		callbind_extend_any extend_any;
		callbind_extend_empty extend_empty;
		callbind_extend_var1 extend_var1;
		callbind_extend_var2 extend_var2;
		callbind_extend_var3 extend_var3;
		callbind_extend_var4 extend_var4;
		callbind_extend_var5 extend_var5;
		callbind_extend_var6 extend_var6;
		callbind_extend_opt1 extend_opt1;
		callbind_extend_opt2 extend_opt2;
		callbind_extend_opt3 extend_opt3;
		callbind_extend_var1opt1 extend_var1opt1;
		callbind_extend_var1opt2 extend_var1opt2;
		callbind_extend_var1opt3 extend_var1opt3;
		callbind_extend_var2opt1 extend_var2opt1;
		callbind_extend_var2opt2 extend_var2opt2;
		callbind_extend_var2opt3 extend_var2opt3;
		callbind_extend_var3opt1 extend_var3opt1;
		callbind_extend_var3opt2 extend_var3opt2;
		callbind_extend_var3opt3 extend_var3opt3;
		callbind_extend_var1rest extend_var1rest;
		callbind_extend_var2rest extend_var2rest;
		callbind_extend_var3rest extend_var3rest;
		callbind_extend_var1dynamic extend_var1dynamic;
		callbind_extend_var2dynamic extend_var2dynamic;
		callbind_extend_var3dynamic extend_var3dynamic;
		void *pvoid;
	} call;
};

#endif

