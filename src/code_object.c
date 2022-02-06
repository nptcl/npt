#include "character.h"
#include "code_init.h"
#include "code_object.h"
#include "copy.h"
#include "function.h"
#include "heap.h"
#include "memory.h"
#include "pointer.h"
#include "symbol.h"

/*
 *  code
 */
static void alloc_code_heap(addr *ret)
{
	heap_smallsize(ret, LISPTYPE_CODE, Code_Size, sizeoft(struct code_struct));
}

static void alloc_code_system_heap(addr *ret, size_t size)
{
	addr pos;
	size_t allsize;
#ifdef LISP_DEBUG
	struct code_value *ptr;
#endif

	allsize = (size + 1) * sizeoft(struct code_value);
	heap_body4(&pos, LISPSYSTEM_CODE, allsize);
#ifdef LISP_DEBUG
	ptr = StructCallCode(pos);
	aamemory(ptr, allsize);
#endif
	*ret = pos;
}

void code_heap(addr *ret, addr codeA4)
{
	addr pos, call;
	struct code_struct *ptr;
	size_t size;

	alloc_code_heap(&pos);
	ptr = StructCode(pos);
	clearpoint(ptr);
	CheckType(codeA4, LISPTYPE_VECTOR);
	LenArrayA4(codeA4, &size);
	alloc_code_system_heap(&call, size);
	SetArrayCode(pos, Code_Array, codeA4);
	SetArrayCode(pos, Code_Call, call);
	ptr->size = size;
	update_code(pos);
	*ret = pos;
}

void getarray_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Array, ret);
}


/*
 *  update_code
 */
static CodeValue make_code_value(pointer id, addr pos)
{
	CodeValue ret;
	enum CodeValueType type;

	Check(p_size_code < id, "pointer error");
	GetCodeValueArray(id, &type);
	switch (type) {
		case CodeValueType_Addr:
			ret.pos = pos;
			break;

		case CodeValueType_Index:
			GetIndex(pos, &ret.index);
			break;

		case CodeValueType_Fixnum:
			GetFixnum(pos, &ret.value);
			break;

		case CodeValueType_FixnumNull:
			if (pos == Nil) {
				ret.pos = Nil;
			}
			else {
				GetFixnum(pos, &ret.value);
			}
			break;

		case CodeValueType_Character:
			GetCharacter(pos, &ret.character);
			break;

		case CodeValueType_Null:
			ret.pos = Nil;
			break;
	}

	return ret;
}

static void update_struct_code(struct code_value *ptr, addr list)
{
	addr symbol, pos;
	callbind_code bind;
	CodeValue value;
	pointer id;

	/* operator */
	GetCons(list, &symbol, &list);
	GetFunctionSymbol(symbol, &pos);
#ifdef LISP_DEBUG
	if (pos == Unbound)
		infoprint(symbol);
	Check(pos == Unbound, "unbound error");
#endif
	Check(! compiled_funcall_function_p(pos), "type error");
	id = StructFunction(pos)->index;
	GetPointer_code(id, &bind);
	value = make_code_value(id, list);

	/* result */
	ptr->call= bind;
	ptr->id = id;
	ptr->value = value;
}

void update_code(addr code)
{
	addr array, call, list;
	struct code_struct *str;
	struct code_value *ptr;
	size_t size, i;

	CheckType(code, LISPTYPE_CODE);
	GetArrayCode(code, Code_Array, &array);
	GetArrayCode(code, Code_Call, &call);
	str = StructCode(code);
	ptr = StructCallCode(call);
	size = str->size;

	for (i = 0; i < size; i++) {
		GetArgumentCall(array, i, &list);
		update_struct_code(ptr + i, list);
	}
	ptr[i].call = NULL;
	str->sys = ptr;
}

