#include "eval_object.h"
#include "heap.h"
#include "local.h"

/*
 *  eval-object
 */
void eval_alloc(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	alloc_smallsize(local, ret, LISPTYPE_EVAL, array, body);
	SetEvalType(*ret, type);
}
void eval_heap(addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	eval_alloc(NULL, ret, type, array, body);
}
void eval_local(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	Check(local == NULL, "local error");
	eval_alloc(local, ret, type, array, body);
}

addr refeval(addr pos, size_t index)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEval_Low(pos, index);
}
void geteval(addr pos, size_t index, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEval_Low(pos, index, ret);
}
void seteval(addr pos, size_t index, addr value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEval_Low(pos, index, value);
}
enum EVAL_TYPE refevaltype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEvalType_Low(pos);
}
void getevaltype(addr pos, enum EVAL_TYPE *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEvalType_Low(pos, ret);
}
void setevaltype(addr pos, enum EVAL_TYPE value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEvalType_Low(pos, value);
}


/*
 *  check
 */
int eval_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL;
}
int eval_declare_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_DECLARE;
}
int eval_declare_nil_p(addr pos)
{
	return pos == Nil || eval_declare_p(pos);
}
int eval_parse_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_PARSE;
}
int eval_scope_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_SCOPE;
}
int eval_stack_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_STACK;
}
int eval_table_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLE;
}
int eval_tablevalue_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEVALUE;
}
int eval_tablefunction_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEFUNCTION;
}
int eval_tabletagbody_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLETAGBODY;
}
int eval_tableblock_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEBLOCK;
}
int eval_tablecall_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLECALL;
}
int eval_code_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_CODE;
}

