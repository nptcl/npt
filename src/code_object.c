#include "code_object.h"
#include "copy.h"
#include "function.h"
#include "memory.h"
#include "symbol.h"

/*
 *  code
 */
static void alloc_code(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret, LISPTYPE_CODE,
			Code_Size, sizeoft(struct code_struct));
}

static void code_call_alloc(LocalRoot local, addr *ret, size_t size)
{
	addr pos;
	size_t allsize, *ptr;

	allsize = sizeoft(size_t)
		+ (size + 1) * (sizeoft(pointer) + sizeoft(callbind_code));
	alloc_body4(local, &pos, LISPSYSTEM_CODE, allsize);
	ptr = (size_t *)PtrCallCode(pos);
#ifdef LISP_DEBUG
	aamemory(ptr, allsize);
#endif
	*ptr = size;
	*ret = pos;
}

static pointer *pointer_code_call(addr pos)
{
	byte *ptr;
	size_t size;

	CheckType(pos, LISPSYSTEM_CODE);
	ptr = (byte *)PtrCallCode(pos);
	size = *(size_t *)ptr;
	ptr += sizeoft(size_t);
	return (pointer *)(ptr + (size + 1) * sizeoft(callbind_code));
}

static void make_code_call(LocalRoot local, addr codeA4, size_t size,
		addr *rcall, addr *rargs)
{
	size_t i;
	addr left, right, call, args;
	pointer *ptr;

	code_call_alloc(local, &call, size);
	vector4_alloc(local, &args, size + 1);
	ptr = pointer_code_call(call);
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
	*rcall = call;
	*rargs = args;
}

static void code_alloc(LocalRoot local, addr *ret, addr codeA4)
{
	addr pos, call, args;
	struct code_struct *ptr;
	size_t size;

	alloc_code(local, &pos);
	ptr = StructCode(pos);
	clearpoint(ptr);
	CheckType(codeA4, LISPTYPE_VECTOR);
	LenArrayA4(codeA4, &size);
	make_code_call(local, codeA4, size, &call, &args);
	SetArrayCode(pos, Code_Array, codeA4);
	SetArrayCode(pos, Code_Call, call);
	SetArrayCode(pos, Code_Argument, args);
	ptr->size = size;
	allpointer_code(pos);
	*ret = pos;
}

_g void code_heap(addr *ret, addr codeA4)
{
	code_alloc(NULL, ret, codeA4);
}

_g void function_empty_heap(addr *ret, addr name)
{
	addr pos;

	vector4_heap(&pos, 0);
	code_heap(&pos, pos);
	function_heap(ret, name, pos);
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


/*
 *  setpointer
 */
static callbind_code *system_code_call(addr pos)
{
	byte *ptr;

	CheckType(pos, LISPSYSTEM_CODE);
	ptr = (byte *)PtrCallCode(pos);
	return (callbind_code *)(ptr + sizeoft(size_t));
}

_g void setpointer_code(addr code)
{
	addr pos;
	struct code_struct *ptr;

	CheckType(code, LISPTYPE_CODE);
	ptr = StructCode(code);
	/* sys */
	GetArrayCode(code, Code_Call, &pos);
	ptr->sys = system_code_call(pos);
	/* args */
	GetArrayCode(code, Code_Argument, &pos);
	ptr->args = (addr *)PtrArrayA4(pos);
}

_g void setpointer_code_call(addr code)
{
	byte *ptr;
	callbind_code *sys, bind;
	pointer *pp, id;
	size_t i, size;

	CheckType(code, LISPSYSTEM_CODE);
	ptr = (byte *)PtrCallCode(code);
	size = *(size_t *)ptr;
	ptr += sizeoft(size_t);
	sys = (callbind_code *)ptr;
	ptr += (size + 1) * sizeoft(callbind_code);
	pp = (pointer *)ptr;

	for (i = 0; i < size; i++) {
		id = pp[i];
		GetPointer_code(id, &bind);
		sys[i] = bind;
	}
	pp[i] = 0;
	sys[i] = 0;
}

_g void allpointer_code(addr code)
{
	setpointer_code(code);
	GetArrayCode(code, Code_Call, &code);
	setpointer_code_call(code);
}

