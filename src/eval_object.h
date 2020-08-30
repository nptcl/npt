#ifndef __EVAL_OBJECT_HEADER__
#define __EVAL_OBJECT_HEADER__

#include "local.h"
#include "memory.h"
#include "typedef.h"

#define eval_heap _n(eval_heap)
#define eval_local _n(eval_local)
#define eval_alloc _n(eval_alloc)
#define refeval _n(refeval)
#define geteval _n(geteval)
#define seteval _n(seteval)
#define refevaltype _n(refevaltype)
#define getevaltype _n(getevaltype)
#define setevaltype _n(setevaltype)
#define eval_p _n(eval_p)
#define eval_declare_p _n(eval_declare_p)
#define eval_declare_nil_p _n(eval_declare_nil_p)
#define eval_parse_p _n(eval_parse_p)
#define eval_scope_p _n(eval_scope_p)
#define eval_stack_p _n(eval_stack_p)
#define eval_table_p _n(eval_table_p)
#define eval_tablevalue_p _n(eval_tablevalue_p)
#define eval_tablefunction_p _n(eval_tablefunction_p)
#define eval_tabletagbody_p _n(eval_tabletagbody_p)
#define eval_tableblock_p _n(eval_tableblock_p)
#define eval_tablecall_p _n(eval_tablecall_p)
#define eval_code_p _n(eval_code_p)

enum EVAL_TYPE {
	EVAL_TYPE_DECLARE,
	EVAL_TYPE_PARSE,
	EVAL_TYPE_STACK,
	EVAL_TYPE_SCOPE,
	EVAL_TYPE_TABLE,
	EVAL_TYPE_TABLEVALUE,
	EVAL_TYPE_TABLEFUNCTION,
	EVAL_TYPE_TABLETAGBODY,
	EVAL_TYPE_TABLEBLOCK,
	EVAL_TYPE_TABLECALL,
	EVAL_TYPE_CODE,
	EVAL_TYPE_SIZE
};

#define RefEval_Low(p,i)		RefArraySS((p),(i))
#define GetEval_Low(p,i,v)		GetArraySS((p),(i),(v))
#define SetEval_Low(p,i,v)		SetArraySS((p),(i),(v))
#define RefEvalType_Low(p)		((enum EVAL_TYPE)GetUser(p))
#define GetEvalType_Low(p,v)	(*(v) = RefEvalType_Low(p))
#define SetEvalType_Low(p,v)	SetUser((p), (byte)(v))

#ifdef LISP_DEBUG
#define RefEval(p,i)		refeval(p,i)
#define GetEval(p,i,v)		geteval(p,i,v)
#define SetEval(p,i,v)		seteval(p,i,v)
#define RefEvalType(p)		refevaltype(p)
#define GetEvalType(p,v)	getevaltype(p,v)
#define SetEvalType(p,v)	setevaltype(p,v)
#else
#define RefEval(p,i)		RefEval_Low(p,i)
#define GetEval(p,i,v)		GetEval_Low(p,i,v)
#define SetEval(p,i,v)		SetEval_Low(p,i,v)
#define RefEvalType(p)		RefEvalType_Low(p)
#define GetEvalType(p,v)	GetEvalType_Low(p,v)
#define SetEvalType(p,v)	SetEvalType_Low(p,v)
#endif

#define LenBodyEval			LenBodySS
#define PtrEvalBody(x,y)	PtrBodySSa(x,y)
#define PtrEvalBodyAny(x)	PtrBodySS(x)

_g void eval_heap(addr *ret,
		enum EVAL_TYPE type, byte array, byte body);
_g void eval_local(LocalRoot local, addr *ret,
		enum EVAL_TYPE type, byte array, byte body);
_g void eval_alloc(LocalRoot local, addr *ret,
		enum EVAL_TYPE type, byte array, byte body);

_g addr refeval(addr pos, size_t index);
_g void geteval(addr pos, size_t index, addr *ret);
_g void seteval(addr pos, size_t index, addr value);
_g enum EVAL_TYPE refevaltype(addr pos);
_g void getevaltype(addr pos, enum EVAL_TYPE *ret);
_g void setevaltype(addr pos, enum EVAL_TYPE value);

_g int eval_p(addr pos);
_g int eval_declare_p(addr pos);
_g int eval_declare_nil_p(addr pos);
_g int eval_parse_p(addr pos);
_g int eval_scope_p(addr pos);
_g int eval_stack_p(addr pos);
_g int eval_table_p(addr pos);
_g int eval_tablevalue_p(addr pos);
_g int eval_tablefunction_p(addr pos);
_g int eval_tabletagbody_p(addr pos);
_g int eval_tableblock_p(addr pos);
_g int eval_tablecall_p(addr pos);
_g int eval_code_p(addr pos);

#endif

