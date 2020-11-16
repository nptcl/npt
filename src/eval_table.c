#include "callname.h"
#include "copy.h"
#include "equal.h"
#include "eval.h"
#include "eval_object.h"
#include "eval_table.h"
#include "execute_object.h"
#include "heap.h"
#include "local.h"
#include "object.h"
#include "parse.h"
#include "symbol.h"

/*
 *  tablevalue
 */
static void alloc_tablevalue(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEVALUE,
			TABLEVALUE_INDEX_SIZE,
			sizeoft(struct tablevalue));
}

void make_tablevalue(addr *ret, addr symbol)
{
	addr pos;
	struct tablevalue *ptr;

	alloc_tablevalue(NULL, &pos);
	ptr = StructTableValue(pos);
	clearpoint(ptr);
	SetEval(pos, TABLEVALUE_INDEX_NAME, symbol);
	*ret = pos;
}

void copy_tablevalue(addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableValue(pos);
	alloc_tablevalue(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_value(one), PtrBody_value(pos), size);

	for (i = 0; i < TABLEVALUE_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(NULL, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

void getname_tablevalue(addr pos, addr *ret)
{
	CheckTableValue(pos);
	GetEval(pos, TABLEVALUE_INDEX_NAME, ret);
}
void setname_tablevalue(addr pos, addr value)
{
	CheckTableValue(pos);
	SetEval(pos, TABLEVALUE_INDEX_NAME, value);
}

void gettype_tablevalue(addr pos, addr *ret)
{
	CheckTableValue(pos);
	GetEval(pos, TABLEVALUE_INDEX_TYPE, ret);
}
void settype_tablevalue(addr pos, addr value)
{
	CheckTableValue(pos);
	SetEval(pos, TABLEVALUE_INDEX_TYPE, value);
}

int getspecialp_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->specialp;
}
void setspecialp_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->specialp = (value != 0);
}

int getdynamic_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->dynamic;
}
void setdynamic_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->dynamic = (value != 0);
}

enum IgnoreType getignore_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->ignore;
}
void setignore_tablevalue(addr pos, enum IgnoreType value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->ignore = value;
}

int getreference_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->reference;
}
void setreference_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->reference = (value != 0);
}

int getcheck_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->check;
}
void setcheck_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->check = (value != 0);
}

size_t getlexical_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->lexical;
}
void setlexical_tablevalue(addr pos, size_t value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->lexical = value;
}

size_t getlet_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->let;
}
void setlet_tablevalue(addr pos, size_t value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->let = value;
}

int getclosurep_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->closurep;
}
void setclosurep_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->closurep = (value != 0);
}

int getbasep_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->basep;
}
void setbasep_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->basep = (value != 0);
}

int getglobalp_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->globalp;
}
void setglobalp_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->globalp = (value != 0);
}

size_t getclosure_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->closure;
}
void setclosure_tablevalue(addr pos, size_t value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->closure = value;
}

void getvalue_tablevalue(Execute ptr, addr pos, addr *ret)
{
	size_t index = getlexical_tablevalue(pos);
	get_lexical_control(ptr, index, ret);
}

void setvalue_tablevalue(Execute ptr, addr pos, addr value)
{
	size_t index = getlexical_tablevalue(pos);
	set_lexical_control(ptr, index, value);
}


/*
 *  tablefunction
 */
static void alloc_tablefunction(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEFUNCTION,
			TABLEFUNCTION_INDEX_SIZE,
			sizeoft(struct tablefunction));
}

void make_tablefunction(addr *ret, addr call)
{
	addr pos;
	struct tablefunction *ptr;

	alloc_tablefunction(NULL, &pos);
	ptr = StructTableFunction(pos);
	clearpoint(ptr);
	SetEval(pos, TABLEFUNCTION_INDEX_NAME, call);
	*ret = pos;
}

void copy_tablefunction(addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableFunction(pos);
	alloc_tablefunction(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_function(one), PtrBody_function(pos), size);

	for (i = 0; i < TABLEFUNCTION_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(NULL, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

void getname_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	GetEval(pos, TABLEFUNCTION_INDEX_NAME, ret);
}
void setname_tablefunction(addr pos, addr value)
{
	CheckTableFunction(pos);
	SetEval(pos, TABLEFUNCTION_INDEX_NAME, value);
}

void gettype_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	GetEval(pos, TABLEFUNCTION_INDEX_TYPE, ret);
}
void settype_tablefunction(addr pos, addr value)
{
	CheckTableFunction(pos);
	SetEval(pos, TABLEFUNCTION_INDEX_TYPE, value);
}

int getglobalp_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->globalp;
}
void setglobalp_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->globalp = (value != 0);
}

int getdynamic_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->dynamic;
}
void setdynamic_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->dynamic = (value != 0);
}

int getreference_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->reference;
}
void setreference_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->reference = (value != 0);
}

int getcheck_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->check;
}
void setcheck_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->check = (value != 0);
}

enum IgnoreType getignore_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->ignore;
}
void setignore_tablefunction(addr pos, enum IgnoreType value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->ignore = value;
}

enum InlineType getinline_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->Inline;
}
void setinline_tablefunction(addr pos, enum InlineType value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->Inline = value;
}

size_t getlexical_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->lexical;
}

void setlexical_tablefunction(addr pos, size_t value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->lexical = value;
}

int getclosurep_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->closurep;
}

void setclosurep_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->closurep = (value != 0);
}

size_t getclosure_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->closure;
}

void setclosure_tablefunction(addr pos, size_t value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->closure = value;
}

void getvalue_tablefunction(Execute ptr, addr pos, addr *ret)
{
	size_t index = getlexical_tablefunction(pos);
	getlow_lexical_control(ptr, index, ret);
}

void setvalue_tablefunction(Execute ptr, addr pos, addr value)
{
	size_t index = getlexical_tablefunction(pos);
	setlow_lexical_control(ptr, index, value);
}


/*
 *  tabletagbody
 */
static void alloc_tabletagbody(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLETAGBODY,
			TABLETAGBODY_INDEX_SIZE,
			sizeoft(struct tabletagbody));
}

void make_tabletagbody(addr *ret, addr tag)
{
	addr pos;
	struct tabletagbody *ptr;

	Check(! tagbody_tag_p(tag), "tag error");
	alloc_tabletagbody(NULL, &pos);
	ptr = StructTableTagBody(pos);
	clearpoint(ptr);
	SetEval(pos, TABLETAGBODY_INDEX_NAME, tag);
	*ret = pos;
}

void copy_tabletagbody(addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableTagBody(pos);
	alloc_tabletagbody(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_tagbody(one), PtrBody_tagbody(pos), size);

	for (i = 0; i < TABLETAGBODY_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(NULL, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

void getname_tabletagbody(addr pos, addr *ret)
{
	CheckTableTagBody(pos);
	GetEval(pos, TABLETAGBODY_INDEX_NAME, ret);
}
void setname_tabletagbody(addr pos, addr value)
{
	CheckTableTagBody(pos);
	Check(! tagbody_tag_p(value), "tag error");
	SetEval(pos, TABLETAGBODY_INDEX_NAME, value);
}

int getreference_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->reference;
}
void setreference_tabletagbody(addr pos, int value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->reference = (value != 0);
}

int equal_tabletagbody(addr left, addr right)
{
	CheckTableTagBody(left);
	CheckTableTagBody(right);
	GetEval(left, TABLETAGBODY_INDEX_NAME, &left);
	GetEval(right, TABLETAGBODY_INDEX_NAME, &right);
	return eql_function(left, right);
}

size_t getlexical_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->lexical;
}

void setlexical_tabletagbody(addr pos, size_t value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->lexical = value;
}

int getclosurep_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->closurep;
}

void setclosurep_tabletagbody(addr pos, int value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->closurep = (value != 0);
}

size_t getclosure_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->closure;
}

void setclosure_tabletagbody(addr pos, size_t value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->closure = value;
}

size_t getjump_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->jump;
}

void setjump_tabletagbody(addr pos, size_t value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->jump = value;
}


/*
 *  tableblock
 */
static void alloc_tableblock(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEBLOCK,
			TABLEBLOCK_INDEX_SIZE,
			sizeoft(struct tableblock));
}

void make_tableblock(addr *ret, addr tag)
{
	addr pos;
	struct tableblock *ptr;

	Check(! symbolp(tag), "tag error");
	alloc_tableblock(NULL, &pos);
	ptr = StructTableBlock(pos);
	clearpoint(ptr);
	SetEval(pos, TABLEBLOCK_INDEX_NAME, tag);
	*ret = pos;
}

void copy_tableblock(addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableBlock(pos);
	alloc_tableblock(NULL, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_block(one), PtrBody_block(pos), size);

	for (i = 0; i < TABLEBLOCK_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(NULL, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

void getname_tableblock(addr pos, addr *ret)
{
	CheckTableBlock(pos);
	GetEval(pos, TABLEBLOCK_INDEX_NAME, ret);
}
void setname_tableblock(addr pos, addr value)
{
	CheckTableBlock(pos);
	Check(! symbolp(value), "tag error");
	SetEval(pos, TABLEBLOCK_INDEX_NAME, value);
}

int getreference_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->reference;
}
void setreference_tableblock(addr pos, int value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->reference = (value != 0);
}

int equal_tableblock(addr left, addr right)
{
	CheckTableBlock(left);
	CheckTableBlock(right);
	GetEval(left, TABLEBLOCK_INDEX_NAME, &left);
	GetEval(right, TABLEBLOCK_INDEX_NAME, &right);
	return left == right;
}

size_t getlexical_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->lexical;
}

void setlexical_tableblock(addr pos, size_t value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->lexical = value;
}

int getclosurep_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->closurep;
}

void setclosurep_tableblock(addr pos, int value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->closurep = (value != 0);
}

size_t getclosure_tableblock(addr pos)
{
	CheckTableBlock(pos);
	return StructTableBlock(pos)->closure;
}

void setclosure_tableblock(addr pos, size_t value)
{
	CheckTableBlock(pos);
	StructTableBlock(pos)->closure = value;
}


/*
 *  evaltable
 */
static void evaltable_heap(addr *ret, enum EvalTable type, addr value)
{
	addr pos;
	struct eval_table *str;

	eval_heap(&pos, EVAL_TYPE_TABLE, 1, sizeoft(struct eval_table));
	SetEval(pos, 0, value);
	str = StructEvalTable(pos);
	str->type = type;
	*ret = pos;
}
void evaltable_value_heap(addr *ret, addr pos)
{
	CheckTableValue(pos);
	evaltable_heap(ret, EvalTable_Value, pos);
}
void evaltable_function_heap(addr *ret, addr pos)
{
	CheckTableFunction(pos);
	evaltable_heap(ret, EvalTable_Function, pos);
}
void evaltable_tagbody_heap(addr *ret, addr pos)
{
	CheckTableTagBody(pos);
	evaltable_heap(ret, EvalTable_TagBody, pos);
}
void evaltable_block_heap(addr *ret, addr pos)
{
	CheckTableBlock(pos);
	evaltable_heap(ret, EvalTable_Block, pos);
}

enum EvalTable gettype_evaltable(addr pos)
{
	CheckTableTable(pos);
	return StructEvalTable(pos)->type;
}

void get_evaltable(addr pos, addr *ret)
{
	CheckTableTable(pos);
	GetEval(pos, 0, ret);
}

int getvalue_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! symbolp(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_Value) {
			get_evaltable(var, &var);
			getname_tablevalue(var, &check);
			if (check == pos) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int getfunction_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! callnamep(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_Function) {
			get_evaltable(var, &var);
			getname_tablefunction(var, &check);
			if (equal_callname(check, pos)) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int gettagbody_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! tagbody_tag_p(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_TagBody) {
			get_evaltable(var, &var);
			getname_tabletagbody(var, &check);
			if (eql_function(check, pos)) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int getblock_evaltable(addr list, addr pos, addr *ret)
{
	addr var, check;

	Check(! symbolp(pos), "name error");
	while (list != Nil) {
		GetCons(list, &var, &list);
		if (gettype_evaltable(var) == EvalTable_Block) {
			get_evaltable(var, &var);
			getname_tableblock(var, &check);
			if (check == pos) {
				if (ret)
					*ret = var;
				return 1;
			}
		}
	}

	return 0;
}

int getclosurep_evaltable(addr table)
{
	addr pos;

	CheckTableTable(table);
	get_evaltable(table, &pos);
	switch (gettype_evaltable(table)) {
		case EvalTable_Value:
			return getclosurep_tablevalue(pos);

		case EvalTable_Function:
			return getclosurep_tablefunction(pos);

		case EvalTable_TagBody:
			return getclosurep_tabletagbody(pos);

		case EvalTable_Block:
			return getclosurep_tableblock(pos);

		default:
			Abort("getclosurep_evaltable error");
			return 0;
	}
}


/*
 *  tablecall
 */
static void alloc_tablecall(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLECALL,
			TABLECALL_INDEX_SIZE,
			sizeoft(struct tablecall));
}

void make_tablecall(addr *ret)
{
	addr pos;
	struct tablecall *ptr;

	alloc_tablecall(NULL, &pos);
	ptr = StructTableCall(pos);
	clearpoint(ptr);
	*ret = pos;
}

int getcheck_tablecall(addr pos)
{
	CheckTableCall(pos);
	return StructTableCall(pos)->check;
}
void setcheck_tablecall(addr pos, int value)
{
	CheckTableCall(pos);
	StructTableCall(pos)->check = (value != 0);
}

void getvalue_tablecall(addr pos, addr *ret)
{
	CheckTableCall(pos);
	GetEval(pos, TABLECALL_INDEX_VALUE, ret);
}
void setvalue_tablecall(addr pos, addr value)
{
	CheckTableCall(pos);
	SetEval(pos, TABLECALL_INDEX_VALUE, value);
}

void gettype_tablecall(addr pos, addr *ret)
{
	CheckTableCall(pos);
	GetEval(pos, TABLECALL_INDEX_TYPE, ret);
}
void settype_tablecall(addr pos, addr value)
{
	CheckTableCall(pos);
	SetEval(pos, TABLECALL_INDEX_TYPE, value);
}

