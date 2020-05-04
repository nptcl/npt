#include "code_object.h"
#include "copy.h"
#include "function.h"
#include "memory.h"
#include "symbol.h"

static void alloc_code(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret, LISPTYPE_CODE,
			Code_Size, sizeoft(struct code_struct));
}

static void code_call_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_body4(local, ret, LISPSYSTEM_CODE, sizeoft(pointer) * size);
}

static void make_code_call(LocalRoot local, addr codeA4, size_t size,
		addr *retcall, addr *retargs, const pointer **retptr)
{
	size_t i;
	addr left, right, call, args;
	pointer *ptr;

	code_call_alloc(local, &call, size + 1);
	vector4_alloc(local, &args, size + 1);
	ptr = PtrCallCode(call);
	for (i = 0; i < size; i++) {
		GetArrayA4(codeA4, i, &left);
		copylocal_object(local, &left, left);
		GetCons(left, &left, &right);
		GetFunctionSymbol(left, &left);
		/* call */
		Check(left == NULL || left == Unbound, "unbound error");
		Check(! compiled_funcall_function_p(left), "type error");
		ptr[i] = StructFunction(left)->index;
		/* args */
		SetArgumentCall(args, i, right);
	}
	ptr[i] = 0; /* p_empty */
	*retcall = call;
	*retargs = args;
	*retptr = ptr;
}

static void code_alloc(LocalRoot local, addr *ret, addr codeA4)
{
	addr pos, call, args;
	struct code_struct *ptr;
	const pointer *pp;
	size_t size;

	alloc_code(local, &pos);
	ptr = StructCode(pos);
	clearpoint(ptr);
	CheckType(pos, LISPTYPE_CODE);
	CheckType(codeA4, LISPTYPE_VECTOR);
	LenArrayA4(codeA4, &size);
	make_code_call(local, codeA4, size, &call, &args, &pp);
	SetArrayCode(pos, Code_Array, codeA4);
	SetArrayCode(pos, Code_Call, call);
	SetArrayCode(pos, Code_Argument, args);
	ptr->call = pp;
	ptr->size = size;
	*ret = pos;
}

_g void code_heap(addr *ret, addr codeA4)
{
	code_alloc(NULL, ret, codeA4);
}

_g void code_empty_heap(addr *ret)
{
	addr pos;
	vector4_heap(&pos, 0);
	code_heap(ret, pos);
}

_g void function_empty_heap(addr *ret, addr name)
{
	addr pos;
	code_empty_heap(&pos);
	function_heap(ret, name, pos);
}

_g const pointer *getcalltype_code(addr pos)
{
	CheckType(pos, LISPTYPE_CODE);
	return StructCode(pos)->call;
}

_g void getarray_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Array, ret);
}

_g void getargs_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Argument, ret);
}

