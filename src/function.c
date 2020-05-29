#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "function.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "symbol.h"

static void alloc_function(LocalRoot local, addr *ret,
		addr name, addr code, int macro, int compiled)
{
	addr pos;
	struct function_struct *str;

	if (name != Nil) {
		if (GetType(name) != LISPTYPE_CALLNAME &&
				parse_callname_alloc(local, &name, name))
			fmte("Invalid function name ~S.", name, NULL);
	}
	alloc_smallsize(local, &pos,
			LISPTYPE_FUNCTION,
			FUNCTION_INDEX_SIZE,
			sizeoft(struct function_struct));
	SetCodeFunction_Low(pos, code);
	SetNameFunction_Low(pos, name);
	str = StructFunction_Low(pos);
	str->macro = macro;
	str->compiled = compiled;
	str->trace = 0;
	str->index = p_empty;
	*ret = pos;
}

_g void function_alloc(LocalRoot local, addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 0, 0);
}
_g void function_local(LocalRoot local, addr *ret, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 0, 0);
}
_g void function_heap(addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(NULL, ret, name, code, 0, 0);
}

_g void function_heap_for_develop(addr *ret, addr name)
{
	alloc_function(NULL, ret, name, Nil, 0, 0);
}

_g void macro_alloc(LocalRoot local, addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 1, 0);
}
_g void macro_local(LocalRoot local, addr *ret, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	alloc_function(local, ret, name, code, 1, 0);
}
_g void macro_heap(addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	alloc_function(NULL, ret, name, code, 1, 0);
}

_g void compiled_alloc(LocalRoot local, addr *ret, addr name)
{
	alloc_function(local, ret, name, Nil, 0, 1);
}
_g void compiled_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	alloc_function(local, ret, name, Nil, 0, 1);
}
_g void compiled_heap(addr *ret, addr name)
{
	alloc_function(NULL, ret, name, Nil, 0, 1);
}

_g void compiled_setf_heap(addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error.");
	setf_callname_heap(&symbol, symbol);
	alloc_function(NULL, ret, symbol, Nil, 0, 1);
}

_g void compiled_macro_alloc(LocalRoot local, addr *ret, addr name)
{
	alloc_function(local, ret, name, Nil, 1, 1);
}
_g void compiled_macro_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	alloc_function(local, ret, name, Nil, 1, 1);
}
_g void compiled_macro_heap(addr *ret, addr name)
{
	alloc_function(NULL, ret, name, Nil, 1, 1);
}

_g void setcompiled_code(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_code, "type error");
	StructFunction(pos)->index = p;
}

_g void getcompiled_code(addr pos, pointer *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	*ret = StructFunction(pos)->index;
	Check(pointer_table[*ret].type != CallBind_code, "type error");
}

_g void setcompiled_macro(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_macro, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_none(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_none, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_any(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_any, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_empty(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_empty, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var5, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var6(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var6, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt3, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt4, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt5, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var3opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var4opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var5opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var5opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1opt2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt3, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var3dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var4dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_extend_dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_extend_rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_extend_rest, "type error");
	StructFunction(pos)->index = p;
}

_g struct function_struct *structfunction(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	return StructFunction_Low(pos);
}

_g void getcodefunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetCodeFunction_Low(pos, ret);
}

_g void setcodefunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCodeFunction_Low(pos, value);
}

_g void getnamefunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetNameFunction_Low(pos, ret);
}

_g void setnamefunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	CheckType(value, LISPTYPE_CALLNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameFunction_Low(pos, value);
}

_g void getdatafunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetDataFunction_Low(pos, ret);
}

_g void setdatafunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDataFunction_Low(pos, value);
}

static void getplist_function(addr pos, constindex index, addr *ret)
{
	addr type;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetTableFunction_Low(pos, &pos);
	GetConstant(index, &type);
	getplist(pos, type, ret);
}
_g void gettype_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_SYSTEM_TYPE_FUNCTION, ret);
}

static void setplist_function(addr pos, constindex index, addr value)
{
	addr table, type;

	CheckType(pos, LISPTYPE_FUNCTION);
	CheckReadOnly(pos);
	GetTableFunction_Low(pos, &table);
	GetConstant(index, &type);
	if (setplist_heap(table, type, value, &table))
		SetTableFunction_Low(pos, table);
}
_g void settype_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_SYSTEM_TYPE_FUNCTION, value);
}

_g void getdocumentation_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_DOCUMENTATION, ret);
}

_g void setdocumentation_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_DOCUMENTATION, value);
}

_g void getlambda_expression_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_LAMBDA, ret);
}

_g void setlambda_expression_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_LAMBDA, value);
}

_g void getdefunform_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_DEFUN, ret);
}

_g void setdefunform_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_DEFUN, value);
}

_g int functionp(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION;
}

_g int funcall_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->macro == 0;
}

_g int macro_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->macro;
}

_g int interpreted_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->compiled == 0;
}

_g int interpreted_funcall_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled == 0 && str->macro == 0;
}

_g int interpreted_macro_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled == 0 && str->macro;
}

_g int compiled_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->compiled;
}

_g int compiled_funcall_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled && str->macro == 0;
}

_g int compiled_macro_function_p(addr pos)
{
	struct function_struct *str;

	if (GetType(pos) != LISPTYPE_FUNCTION)
		return 0;
	str = StructFunction_Low(pos);

	return str->compiled && str->macro;
}

_g void settrace_function(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	StructFunction_Low(pos)->trace = 1;
}

_g int tracep_function(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->trace;
}

