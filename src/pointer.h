#ifndef __POINTER_HEADER__
#define __POINTER_HEADER__

#include "pointer_table.h"
#include "pointer_type.h"
#include "typedef.h"

#define SizePointer (p_size + LISP_POINTER_EXTEND)

__extern struct callbind_struct pointer_table[SizePointer];

#define ExecPointer(x, y) (pointer_table[x].call.y)

#define SetPointer(p, y, z) { \
	struct callbind_struct *__callbind = &(pointer_table[p]); \
	__callbind->type = CallBind_##y; \
	__callbind->call.y = z; \
}

#define GetPointer(p, y, z) { \
	struct callbind_struct *__callbind = &(pointer_table[p]); \
	Check(__callbind->type != CallBind_##y, "type error"); \
	(*z) = __callbind->call.y; \
}

#define SetPointerType(x, y)          SetPointer(p_##y, x, y)
#define SetPointerCall(x,y,z)         SetPointer_##y(p_##x##_##z, function_##z)
#define SetPointerSysCall(x,y,z)      SetPointer_##y(p_##x##_syscall_##z, syscall_##z)

#define SetPointer_code(p, x)         SetPointer(p, code, x)
#define GetPointer_code(p, x)         GetPointer(p, code, x)
#define SetPointer_macro(p, x)        SetPointer(p, macro, x)
#define GetPointer_macro(p, x)        GetPointer(p, macro, x)
#define SetPointer_none(p, x)         SetPointer(p, none, x)
#define GetPointer_none(p, x)         GetPointer(p, none, x)
#define SetPointer_any(p, x)          SetPointer(p, any, x)
#define GetPointer_any(p, x)          GetPointer(p, any, x)
#define SetPointer_empty(p, x)        SetPointer(p, empty, x)
#define GetPointer_empty(p, x)        GetPointer(p, empty, x)
#define SetPointer_dynamic(p, x)      SetPointer(p, dynamic, x)
#define GetPointer_dynamic(p, x)      GetPointer(p, dynamic, x)
#define SetPointer_rest(p, x)         SetPointer(p, rest, x)
#define GetPointer_rest(p, x)         GetPointer(p, rest, x)
#define SetPointer_var1(p, x)         SetPointer(p, var1, x)
#define GetPointer_var1(p, x)         GetPointer(p, var1, x)
#define SetPointer_var2(p, x)         SetPointer(p, var2, x)
#define GetPointer_var2(p, x)         GetPointer(p, var2, x)
#define SetPointer_var3(p, x)         SetPointer(p, var3, x)
#define GetPointer_var3(p, x)         GetPointer(p, var3, x)
#define SetPointer_var4(p, x)         SetPointer(p, var4, x)
#define GetPointer_var4(p, x)         GetPointer(p, var4, x)
#define SetPointer_var5(p, x)         SetPointer(p, var5, x)
#define GetPointer_var5(p, x)         GetPointer(p, var5, x)
#define SetPointer_var6(p, x)         SetPointer(p, var6, x)
#define GetPointer_var6(p, x)         GetPointer(p, var6, x)
#define SetPointer_opt1(p, x)         SetPointer(p, opt1, x)
#define GetPointer_opt1(p, x)         GetPointer(p, opt1, x)
#define SetPointer_opt2(p, x)         SetPointer(p, opt2, x)
#define GetPointer_opt2(p, x)         GetPointer(p, opt2, x)
#define SetPointer_opt3(p, x)         SetPointer(p, opt3, x)
#define GetPointer_opt3(p, x)         GetPointer(p, opt3, x)
#define SetPointer_opt4(p, x)         SetPointer(p, opt4, x)
#define GetPointer_opt4(p, x)         GetPointer(p, opt4, x)
#define SetPointer_opt5(p, x)         SetPointer(p, opt5, x)
#define GetPointer_opt5(p, x)         GetPointer(p, opt5, x)
#define SetPointer_var1opt1(p, x)     SetPointer(p, var1opt1, x)
#define GetPointer_var1opt1(p, x)     GetPointer(p, var1opt1, x)
#define SetPointer_var2opt1(p, x)     SetPointer(p, var2opt1, x)
#define GetPointer_var2opt1(p, x)     GetPointer(p, var2opt1, x)
#define SetPointer_var3opt1(p, x)     SetPointer(p, var3opt1, x)
#define GetPointer_var3opt1(p, x)     GetPointer(p, var3opt1, x)
#define SetPointer_var4opt1(p, x)     SetPointer(p, var4opt1, x)
#define GetPointer_var4opt1(p, x)     GetPointer(p, var4opt1, x)
#define SetPointer_var5opt1(p, x)     SetPointer(p, var5opt1, x)
#define GetPointer_var5opt1(p, x)     GetPointer(p, var5opt1, x)
#define SetPointer_var1opt2(p, x)     SetPointer(p, var1opt2, x)
#define GetPointer_var1opt2(p, x)     GetPointer(p, var1opt2, x)
#define SetPointer_var2opt2(p, x)     SetPointer(p, var2opt2, x)
#define GetPointer_var2opt2(p, x)     GetPointer(p, var2opt2, x)
#define SetPointer_var2opt3(p, x)     SetPointer(p, var2opt3, x)
#define GetPointer_var2opt3(p, x)     GetPointer(p, var2opt3, x)
#define SetPointer_var1rest(p, x)     SetPointer(p, var1rest, x)
#define GetPointer_var1rest(p, x)     GetPointer(p, var1rest, x)
#define SetPointer_var2rest(p, x)     SetPointer(p, var2rest, x)
#define GetPointer_var2rest(p, x)     GetPointer(p, var2rest, x)
#define SetPointer_opt1rest(p, x)     SetPointer(p, opt1rest, x)
#define GetPointer_opt1rest(p, x)     GetPointer(p, opt1rest, x)
#define SetPointer_var1dynamic(p, x)  SetPointer(p, var1dynamic, x)
#define GetPointer_var1dynamic(p, x)  GetPointer(p, var1dynamic, x)
#define SetPointer_var2dynamic(p, x)  SetPointer(p, var2dynamic, x)
#define GetPointer_var2dynamic(p, x)  GetPointer(p, var2dynamic, x)
#define SetPointer_var3dynamic(p, x)  SetPointer(p, var3dynamic, x)
#define GetPointer_var3dynamic(p, x)  GetPointer(p, var3dynamic, x)
#define SetPointer_var4dynamic(p, x)  SetPointer(p, var4dynamic, x)
#define GetPointer_var4dynamic(p, x)  GetPointer(p, var4dynamic, x)
#define SetPointer_opt1dynamic(p, x)  SetPointer(p, opt1dynamic, x)
#define GetPointer_opt1dynamic(p, x)  GetPointer(p, opt1dynamic, x)
#define SetPointer_extend_dynamic(p, x)  SetPointer(p, extend_dynamic, x)
#define GetPointer_extend_dynamic(p, x)  GetPointer(p, extend_dynamic, x)
#define SetPointer_extend_rest(p, x)  SetPointer(p, extend_rest, x)
#define GetPointer_extend_rest(p, x)  GetPointer(p, extend_rest, x)

_g void clear_pointer(void);

#endif

