#include "eval_object.h"
#include "heap.h"
#include "local.h"

/*
 *  eval-object
 */
_g void eval_alloc(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	alloc_smallsize(local, ret, LISPTYPE_EVAL, array, body);
	SetEvalType(*ret, type);
}
_g void eval_heap(addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	eval_alloc(NULL, ret, type, array, body);
}
_g void eval_local(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	Check(local == NULL, "local error");
	eval_alloc(local, ret, type, array, body);
}

_g addr refeval(addr pos, size_t index)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEval_Low(pos, index);
}
_g void geteval(addr pos, size_t index, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEval_Low(pos, index, ret);
}
_g void seteval(addr pos, size_t index, addr value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEval_Low(pos, index, value);
}
_g enum EVAL_TYPE refevaltype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEvalType_Low(pos);
}
_g void getevaltype(addr pos, enum EVAL_TYPE *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEvalType_Low(pos, ret);
}
_g void setevaltype(addr pos, enum EVAL_TYPE value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEvalType_Low(pos, value);
}


/*
 *  check
 */
_g int eval_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL;
}
_g int eval_declare_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_DECLARE;
}
_g int eval_declare_nil_p(addr pos)
{
	return pos == Nil || eval_declare_p(pos);
}
_g int eval_parse_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_PARSE;
}
_g int eval_scope_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_SCOPE;
}
_g int eval_stack_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_STACK;
}
_g int eval_table_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLE;
}
_g int eval_tablevalue_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEVALUE;
}
_g int eval_tablefunction_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEFUNCTION;
}
_g int eval_tabletagbody_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLETAGBODY;
}
_g int eval_tableblock_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEBLOCK;
}
_g int eval_tablecall_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLECALL;
}
_g int eval_code_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_CODE;
}

