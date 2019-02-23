#ifndef __EVAL_HEADER__
#define __EVAL_HEADER__

#include "code.h"
#include "memory.h"
#include "eval_declare.h"

enum EVAL_TYPE {
	EVAL_TYPE_DECLARE,
	EVAL_TYPE_PARSE,
	EVAL_TYPE_STACK,
	EVAL_TYPE_SCOPE,
	EVAL_TYPE_TABLEVALUE,
	EVAL_TYPE_TABLEFUNCTION,
	EVAL_TYPE_TABLECALL,
	EVAL_TYPE_TABLETAGBODY,
	EVAL_TYPE_TABLEBLOCK,
	EVAL_TYPE_CODE,
	EVAL_TYPE_SIZE
};

enum EVAL_OPTIMIZE {
	EVAL_OPTIMIZE_COMPILATION = 0,
	EVAL_OPTIMIZE_DEBUG,
	EVAL_OPTIMIZE_SAFETY,
	EVAL_OPTIMIZE_SPACE,
	EVAL_OPTIMIZE_SPEED,
	EVAL_OPTIMIZE_SIZE
};

#define RefEval_Low(p,i)		RefArraySS((p),(i))
#define GetEval_Low(p,i,v)		GetArraySS((p),(i),(v))
#define SetEval_Low(p,i,v)		SetArraySS((p),(i),(v))
#define RefEvalType_Low(p)		((enum EVAL_TYPE)GetUser(p))
#define GetEvalType_Low(p,v)	(*(v) = RefEvalType_Low(p))
#define SetEvalType_Low(p,v)	SetUser((p), (byte)(v))

#if LISP_DEBUG
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

addr eval_heapr(enum EVAL_TYPE type, byte array, byte body);
addr eval_localr(LocalRoot local, enum EVAL_TYPE type, byte array, byte body);
addr eval_allocr(LocalRoot local, enum EVAL_TYPE type, byte array, byte body);
void eval_heap(addr *ret, enum EVAL_TYPE type, byte array, byte body);
void eval_local(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body);
void eval_alloc(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body);

addr refeval(addr pos, size_t index);
void geteval(addr pos, size_t index, addr *ret);
void seteval(addr pos, size_t index, addr value);
enum EVAL_TYPE refevaltype(addr pos);
void getevaltype(addr pos, enum EVAL_TYPE *ret);
void setevaltype(addr pos, enum EVAL_TYPE value);

int eval_p(addr pos);
int eval_declare_p(addr pos);
int eval_declare_nil_p(addr pos);
int eval_parse_p(addr pos);
int eval_scope_p(addr pos);
int eval_stack_p(addr pos);
int eval_tablevalue_p(addr pos);
int eval_tablefunction_p(addr pos);
int eval_tablecall_p(addr pos);
int eval_tabletagbody_p(addr pos);
int eval_tableblock_p(addr pos);
int eval_code_p(addr pos);

void symbol_evalwhen_eval(addr *ret);
void symbol_toplevel_eval(addr *ret);
void getevalwhen_eval(Execute ptr, addr *ret);
void setevalwhen_eval(Execute ptr, addr value);
void gettoplevel_eval(Execute ptr, addr *ret);
void settoplevel_eval(Execute ptr, addr value);
void push_toplevel_eval(Execute ptr, addr value);
void push_evalwhen_eval(Execute ptr);
void push_evalwhen_load(Execute ptr);
int toplevelp_eval(Execute ptr);

int eval_constantp(addr var, addr env, int *result);
int eval_constantp_stable(addr var);

int eval_execute(Execute ptr, addr pos);
int eval_stream(Execute ptr, addr stream);
int eval_object(Execute ptr, addr eval, addr *ret);
int eval_load(Execute ptr, int *result,
		addr file, addr verbose, addr print, int exist, addr external);

#endif

