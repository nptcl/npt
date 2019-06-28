#ifndef __POINTER_TYPE_HEADER__
#define __POINTER_TYPE_HEADER__

#include "execute.h"
#include "typedef.h"

typedef void *callbind_error;
typedef void (*callbind_code)(Execute, addr);
typedef void (*callbind_macro)(Execute, addr, addr);
typedef void (*callbind_none)(void);
typedef void (*callbind_any)(Execute);
typedef void (*callbind_empty)(Execute);
typedef void (*callbind_dynamic)(Execute, addr);
typedef void (*callbind_rest)(Execute, addr);
typedef void (*callbind_var1)(Execute, addr);
typedef void (*callbind_var2)(Execute, addr, addr);
typedef void (*callbind_var3)(Execute, addr, addr, addr);
typedef void (*callbind_var4)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var5)(Execute, addr, addr, addr, addr, addr);
typedef void (*callbind_var6)(Execute, addr, addr, addr, addr, addr, addr);
typedef void (*callbind_opt1)(Execute, addr);
typedef void (*callbind_opt2)(Execute, addr, addr);
typedef void (*callbind_opt3)(Execute, addr, addr, addr);
typedef void (*callbind_opt4)(Execute, addr, addr, addr, addr);
typedef void (*callbind_opt5)(Execute, addr, addr, addr, addr, addr);
typedef void (*callbind_var1opt1)(Execute, addr, addr);
typedef void (*callbind_var2opt1)(Execute, addr, addr, addr);
typedef void (*callbind_var3opt1)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var4opt1)(Execute, addr, addr, addr, addr, addr);
typedef void (*callbind_var5opt1)(Execute, addr, addr, addr, addr, addr, addr);
typedef void (*callbind_var1opt2)(Execute, addr, addr, addr);
typedef void (*callbind_var2opt2)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var1rest)(Execute, addr, addr);
typedef void (*callbind_var2rest)(Execute, addr, addr, addr);
typedef void (*callbind_var1dynamic)(Execute, addr, addr);
typedef void (*callbind_var2dynamic)(Execute, addr, addr, addr);
typedef void (*callbind_var3dynamic)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var4dynamic)(Execute, addr, addr, addr, addr, addr);

enum CallBind_index {
       CallBind_error,
       CallBind_code,
       CallBind_macro,
       CallBind_none,
       CallBind_any,
       CallBind_empty,
       CallBind_dynamic,
       CallBind_rest,
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
       CallBind_var1rest,
       CallBind_var2rest,
       CallBind_var1dynamic,
       CallBind_var2dynamic,
       CallBind_var3dynamic,
       CallBind_var4dynamic,
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
		callbind_dynamic dynamic;
		callbind_rest rest;
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
		callbind_var1rest var1rest;
		callbind_var2rest var2rest;
		callbind_var1dynamic var1dynamic;
		callbind_var2dynamic var2dynamic;
		callbind_var3dynamic var3dynamic;
		callbind_var4dynamic var4dynamic;
	} call;
};

#endif

