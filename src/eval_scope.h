#ifndef __EVAL_SCOPE_HEADER__
#define __EVAL_SCOPE_HEADER__

#include "eval.h"
#include "eval_parse.h"

enum EVAL_SCOPE {
	EVAL_SCOPE_THE,
	EVAL_SCOPE_VALUE,
	EVAL_SCOPE_SIZE
};

enum EvalLambda_Index {
	EvalLambda_Call,
	EvalLambda_Table,
	EvalLambda_Args,
	EvalLambda_Decl,
	EvalLambda_Doc,
	EvalLambda_Cons,
	EvalLambda_Clos,
	EvalLambda_The,
	EvalLambda_Free,
	EvalLambda_Form,
	EvalLambda_Defun,
	EvalLambda_Size
};

enum EvalClosure_Index {
	EvalClosure_Value,
	EvalClosure_Function,
	EvalClosure_TagBody,
	EvalClosure_Block,
	EvalClosure_Size
};

struct eval_scope {
	enum EVAL_PARSE type;
};

#define StructEvalScope_Low(x)			((struct eval_scope *)PtrEvalBodyAny(x))
#define RefEvalScopeType_Low(x)			(StructEvalScope(x)->type)
#define GetEvalScopeType_Low(x,v)		(*(v) = RefEvalScopeType_Low(x))
#define SetEvalScopeType_Low(x,v)		(RefEvalScopeType_Low(x) = (v))
#define RefEvalScopeThe_Low(p)			RefEval((p),EVAL_SCOPE_THE)
#define GetEvalScopeThe_Low(p,v)		GetEval((p),EVAL_SCOPE_THE,(v))
#define SetEvalScopeThe_Low(p,v)		SetEval((p),EVAL_SCOPE_THE,(v))
#define RefEvalScopeValue_Low(p)		RefEval((p),EVAL_SCOPE_VALUE)
#define GetEvalScopeValue_Low(p,v)		GetEval((p),EVAL_SCOPE_VALUE,(v))
#define SetEvalScopeValue_Low(p,v)		SetEval((p),EVAL_SCOPE_VALUE,(v))
#define RefEvalScopeIndex_Low(p,i)		RefEval((p),(2UL+(i)))
#define GetEvalScopeIndex_Low(p,i,v)	GetEval((p),(2UL+(i)),(v))
#define SetEvalScopeIndex_Low(p,i,v)	SetEval((p),(2UL+(i)),(v))

#ifdef LISP_DEBUG
#define StructEvalScope(x)			structevalscope(x)
#define RefEvalScopeType(x)			refevalscopetype(x)
#define GetEvalScopeType(x,v)		getevalscopetype(x,v)
#define SetEvalScopeType(x,v)		setevalscopetype(x,v)
#define RefEvalScopeThe(p)			refevalscopethe(p)
#define GetEvalScopeThe(p,v)		getevalscopethe(p,v)
#define SetEvalScopeThe(p,v)		setevalscopethe(p,v)
#define RefEvalScopeValue(p)		refevalscopevalue(p)
#define GetEvalScopeValue(p,v)		getevalscopevalue(p,v)
#define SetEvalScopeValue(p,v)		setevalscopevalue(p,v)
#define RefEvalScopeIndex(p,i)		refevalscopeindex(p,i)
#define GetEvalScopeIndex(p,i,v)	getevalscopeindex(p,i,v)
#define SetEvalScopeIndex(p,i,v)	setevalscopeindex(p,i,v)
#else
#define StructEvalScope(x)			StructEvalScope_Low(x)
#define RefEvalScopeType(x)			RefEvalScopeType_Low(x)
#define GetEvalScopeType(x,v)		GetEvalScopeType_Low(x,v)
#define SetEvalScopeType(x,v)		SetEvalScopeType_Low(x,v)
#define RefEvalScopeThe(p)			RefEvalScopeThe_Low(p)
#define GetEvalScopeThe(p,v)		GetEvalScopeThe_Low(p,v)
#define SetEvalScopeThe(p,v)		SetEvalScopeThe_Low(p,v)
#define RefEvalScopeValue(p)		RefEvalScopeValue_Low(p)
#define GetEvalScopeValue(p,v)		GetEvalScopeValue_Low(p,v)
#define SetEvalScopeValue(p,v)		SetEvalScopeValue_Low(p,v)
#define RefEvalScopeIndex(p,i)		RefEvalScopeIndex_Low(p,i)
#define GetEvalScopeIndex(p,i,v)	GetEvalScopeIndex_Low(p,i,v)
#define SetEvalScopeIndex(p,i,v)	SetEvalScopeIndex_Low(p,i,v)
#endif

_g struct eval_scope *structevalscope(addr pos);
_g enum EVAL_PARSE refevalscopetype(addr pos);
_g void getevalscopetype(addr pos, enum EVAL_PARSE *ret);
_g void setevalscopetype(addr pos, enum EVAL_PARSE value);
_g addr refevalscopethe(addr pos);
_g void getevalscopethe(addr pos, addr *ret);
_g void setevalscopethe(addr pos, addr value);
_g addr refevalscopevalue(addr pos);
_g void getevalscopevalue(addr pos, addr *ret);
_g void setevalscopevalue(addr pos, addr value);
_g addr refevalscopeindex(addr pos, size_t size);
_g void getevalscopeindex(addr pos, size_t size, addr *ret);
_g void setevalscopeindex(addr pos, size_t size, addr value);

_g int eval_scope(Execute ptr, addr *ret, addr eval);

#endif

