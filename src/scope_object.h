#ifndef __SCOPE_OBJECT_HEADER__
#define __SCOPE_OBJECT_HEADER__

#include "declare.h"
#include "execute.h"
#include "hold.h"
#include "parse.h"
#include "scope_typedef.h"
#include "typedef.h"

#define eval_scope_heap_ _n(eval_scope_heap_)
#define eval_scope_size_ _n(eval_scope_size_)
#define make_eval_scope_ _n(make_eval_scope_)
#define structevalscope _n(structevalscope)
#define refevalscopetype _n(refevalscopetype)
#define getevalscopetype _n(getevalscopetype)
#define setevalscopetype _n(setevalscopetype)
#define refevalscopethe _n(refevalscopethe)
#define getevalscopethe _n(getevalscopethe)
#define setevalscopethe _n(setevalscopethe)
#define refevalscopevalue _n(refevalscopevalue)
#define getevalscopevalue _n(getevalscopevalue)
#define setevalscopevalue _n(setevalscopevalue)
#define refevalscopeindex _n(refevalscopeindex)
#define getevalscopeindex _n(getevalscopeindex)
#define setevalscopeindex _n(setevalscopeindex)
#define EvalScopeTable _n(EvalScopeTable)
#define scope_eval _n(scope_eval)
#define scope_allcons _n(scope_allcons)
#define localhold_scope_eval _n(localhold_scope_eval)
#define localhold_scope_allcons _n(localhold_scope_allcons)
#define scope_eval_lexical_ _n(scope_eval_lexical_)
#define scope_step_p _n(scope_step_p)

#define StructEvalScope_Low(x)			((struct scope_struct *)PtrEvalBodyAny(x))
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

int eval_scope_heap_(Execute ptr, addr *ret, size_t size);
int eval_scope_size_(Execute ptr, addr *ret, size_t size,
		EvalParse parse, addr type, addr value);
int make_eval_scope_(Execute ptr,
		addr *ret, EvalParse parse, addr type, addr value);

struct scope_struct *structevalscope(addr pos);
EvalParse refevalscopetype(addr pos);
void getevalscopetype(addr pos, EvalParse *ret);
void setevalscopetype(addr pos, EvalParse value);
addr refevalscopethe(addr pos);
void getevalscopethe(addr pos, addr *ret);
void setevalscopethe(addr pos, addr value);
addr refevalscopevalue(addr pos);
void getevalscopevalue(addr pos, addr *ret);
void setevalscopevalue(addr pos, addr value);
addr refevalscopeindex(addr pos, size_t size);
void getevalscopeindex(addr pos, size_t size, addr *ret);
void setevalscopeindex(addr pos, size_t size, addr value);

/* table */
typedef int (*eval_scope_calltype)(Execute, addr *, addr);
extern eval_scope_calltype EvalScopeTable[EVAL_PARSE_SIZE];

int scope_eval(Execute ptr, addr *ret, addr eval);
int scope_allcons(Execute ptr, addr *retcons, addr *rettype, addr cons);
int localhold_scope_eval(LocalHold hold, Execute ptr, addr *ret, addr eval);
int localhold_scope_allcons(LocalHold hold,
		Execute ptr, addr *retcons, addr *rettype, addr cons);
int scope_eval_lexical_(Execute ptr, addr *ret, addr eval);
int scope_step_p(addr pos);

#endif

