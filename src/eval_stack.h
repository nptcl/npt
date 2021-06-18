#ifndef __EVAL_STACK_HEADER__
#define __EVAL_STACK_HEADER__

#include "declare.h"
#include "eval_object.h"
#include "local.h"

#define eval_stack_alloc _n(eval_stack_alloc)
#define eval_stack_local _n(eval_stack_local)
#define eval_stack_heap _n(eval_stack_heap)
#define structevalstack _n(structevalstack)
#define refevalstacktype _n(refevalstacktype)
#define getevalstacktype _n(getevalstacktype)
#define setevalstacktype _n(setevalstacktype)
#define getevalstacknext _n(getevalstacknext)
#define setevalstacknext _n(setevalstacknext)
#define getevalstacktable _n(getevalstacktable)
#define setevalstacktable _n(setevalstacktable)
#define getevalstackscope _n(getevalstackscope)
#define setevalstackscope _n(setevalstackscope)
#define getevalstacklexical _n(getevalstacklexical)
#define setevalstacklexical _n(setevalstacklexical)
#define eval_stack_lambda_lexical_p _n(eval_stack_lambda_lexical_p)
#define getstack_eval_ _n(getstack_eval_)
#define getglobal_eval_ _n(getglobal_eval_)
#define newstack_eval_ _n(newstack_eval_)
#define freestack_eval_ _n(freestack_eval_)
#define begin_eval_stack_ _n(begin_eval_stack_)
#define free_eval_stack _n(free_eval_stack)
#define globalp_stack_eval _n(globalp_stack_eval)
#define increment_stack_eval _n(increment_stack_eval)
#define getlexical_stack_eval _n(getlexical_stack_eval)
#define getlexical_index_heap _n(getlexical_index_heap)
#define apply_declaim_stack_ _n(apply_declaim_stack_)
#define apply_declare_stack _n(apply_declare_stack)
#define apply_declare_value_stack_ _n(apply_declare_value_stack_)
#define apply_declare_let_stack_ _n(apply_declare_let_stack_)
#define apply_declare_function_stack _n(apply_declare_function_stack)
#define getvalue_scope_evalstack _n(getvalue_scope_evalstack)
#define setvalue_scope_evalstack _n(setvalue_scope_evalstack)
#define getfunction_scope_evalstack _n(getfunction_scope_evalstack)
#define setfunction_scope_evalstack _n(setfunction_scope_evalstack)
#define gettagbody_scope_evalstack _n(gettagbody_scope_evalstack)
#define settagbody_scope_evalstack _n(settagbody_scope_evalstack)
#define getblock_scope_evalstack _n(getblock_scope_evalstack)
#define setblock_scope_evalstack _n(setblock_scope_evalstack)
#define setvalue_lexical_evalstack _n(setvalue_lexical_evalstack)
#define setfunction_lexical_evalstack _n(setfunction_lexical_evalstack)
#define settagbody_lexical_evalstack _n(settagbody_lexical_evalstack)
#define setblock_lexical_evalstack _n(setblock_lexical_evalstack)
#define find_plistlist_evalstack _n(find_plistlist_evalstack)
#define find_special_evalstack _n(find_special_evalstack)
#define find_global_special_evalstack _n(find_global_special_evalstack)
#define push_global_special_evalstack _n(push_global_special_evalstack)

enum EVAL_STACK {
	EVAL_STACK_NEXT,
	EVAL_STACK_TABLE,
	EVAL_STACK_SCOPE,
	EVAL_STACK_LEXICAL,
	EVAL_STACK_SIZE
};

enum EVAL_STACK_MODE {
	EVAL_STACK_MODE_NIL,
	EVAL_STACK_MODE_LEXICAL,
	EVAL_STACK_MODE_LAMBDA,
	EVAL_STACK_MODE_SIZE
};

struct eval_stack {
	unsigned globalp : 1;
	enum EVAL_STACK_MODE type;
	LocalStack stack;
	OptimizeType optimize[EVAL_OPTIMIZE_SIZE];
	size_t lexical;
};

#define PtrEvalStack(x)					PtrEvalBody(x, EVAL_STACK_SIZE)
#define StructEvalStack_Low(x)			((struct eval_stack *)PtrEvalStack(x))
#define RefEvalStackType_Low(x)			(StructEvalStack_Low(x)->type)
#define GetEvalStackType_Low(x,v)		(*(v) = RefEvalStackType_Low(x))
#define SetEvalStackType_Low(x,v)		(RefEvalStackType_Low(x) = (v))
#define GetEvalStackNext_Low(x,v)		GetEval((x),EVAL_STACK_NEXT,(v))
#define SetEvalStackNext_Low(x,v)		SetEval((x),EVAL_STACK_NEXT,(v))
#define GetEvalStackTable_Low(x,v)		GetEval((x),EVAL_STACK_TABLE,(v))
#define SetEvalStackTable_Low(x,v)		SetEval((x),EVAL_STACK_TABLE,(v))
#define GetEvalStackScope_Low(x,v)		GetEval((x),EVAL_STACK_SCOPE,(v))
#define SetEvalStackScope_Low(x,v)		SetEval((x),EVAL_STACK_SCOPE,(v))
#define GetEvalStackLexical_Low(x,v)	GetEval((x),EVAL_STACK_LEXICAL,(v))
#define SetEvalStackLexical_Low(x,v)	SetEval((x),EVAL_STACK_LEXICAL,(v))

#ifdef LISP_DEBUG
#define StructEvalStack(x)				structevalstack(x)
#define RefEvalStackType(x)				refevalstacktype(x)
#define GetEvalStackType(x,v)			getevalstacktype(x,v)
#define SetEvalStackType(x,v)			setevalstacktype(x,v)
#define GetEvalStackNext(x,v)			getevalstacknext(x,v)
#define SetEvalStackNext(x,v)			setevalstacknext(x,v)
#define GetEvalStackTable(x,v)			getevalstacktable(x,v)
#define SetEvalStackTable(x,v)			setevalstacktable(x,v)
#define GetEvalStackScope(x,v)			getevalstackscope(x,v)
#define SetEvalStackScope(x,v)			setevalstackscope(x,v)
#define GetEvalStackLexical(x,v)		getevalstacklexical(x,v)
#define SetEvalStackLexical(x,v)		setevalstacklexical(x,v)
#else
#define StructEvalStack(x)				StructEvalStack_Low(x)
#define RefEvalStackType(x)				RefEvalStackType_Low(x)
#define GetEvalStackType(x,v)			GetEvalStackType_Low(x,v)
#define SetEvalStackType(x,v)			SetEvalStackType_Low(x,v)
#define GetEvalStackNext(x,v)			GetEvalStackNext_Low(x,v)
#define SetEvalStackNext(x,v)			SetEvalStackNext_Low(x,v)
#define GetEvalStackTable(x,v)			GetEvalStackTable_Low(x,v)
#define SetEvalStackTable(x,v)			SetEvalStackTable_Low(x,v)
#define GetEvalStackScope(x,v)			GetEvalStackScope_Low(x,v)
#define SetEvalStackScope(x,v)			SetEvalStackScope_Low(x,v)
#define GetEvalStackLexical(x,v)		GetEvalStackLexical_Low(x,v)
#define SetEvalStackLexical(x,v)		SetEvalStackLexical_Low(x,v)
#endif

#define newstack_nil_(p,r) newstack_eval_((p), EVAL_STACK_MODE_NIL,(r))
#define newstack_lexical_(p,r) newstack_eval_((p), EVAL_STACK_MODE_LEXICAL,(r))
#define newstack_lambda_(p,r) newstack_eval_((p), EVAL_STACK_MODE_LAMBDA,(r))

void eval_stack_alloc(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type);
void eval_stack_local(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type);
void eval_stack_heap(addr *ret, enum EVAL_STACK_MODE type);

struct eval_stack *structevalstack(addr pos);
enum EVAL_STACK_MODE refevalstacktype(addr pos);
void getevalstacktype(addr pos, enum EVAL_STACK_MODE *ret);
void setevalstacktype(addr pos, enum EVAL_STACK_MODE value);
void getevalstacknext(addr pos, addr *ret);
void setevalstacknext(addr pos, addr value);
void getevalstacktable(addr pos, addr *ret);
void setevalstacktable(addr pos, addr value);
void getevalstackscope(addr pos, addr *ret);
void setevalstackscope(addr pos, addr value);
void getevalstacklexical(addr pos, addr *ret);
void setevalstacklexical(addr pos, addr value);

int eval_stack_lambda_lexical_p(addr stack);
int getstack_eval_(Execute ptr, addr *ret);
int getglobal_eval_(Execute ptr, addr *ret);
int newstack_eval_(Execute ptr, enum EVAL_STACK_MODE type, addr *ret);
int freestack_eval_(Execute ptr, addr scope);
int begin_eval_stack_(Execute ptr);
void free_eval_stack(Execute ptr);
int globalp_stack_eval(addr pos);
size_t increment_stack_eval(addr pos);
size_t getlexical_stack_eval(addr pos);
void getlexical_index_heap(addr stack, addr *ret);

int apply_declaim_stack_(Execute ptr, addr declare);
void apply_declare_stack(LocalRoot local, addr stack, addr declare);
int apply_declare_value_stack_(Execute ptr, addr stack, addr symbol, addr declare);
int apply_declare_let_stack_(Execute ptr, addr stack, addr symbol, addr declare);
void apply_declare_function_stack(LocalRoot local, addr stack, addr call, addr declare);

/* table */
int getvalue_scope_evalstack(addr stack, addr pos, addr *ret);
void setvalue_scope_evalstack(addr stack, addr pos);
int getfunction_scope_evalstack(addr stack, addr pos, addr *ret);
void setfunction_scope_evalstack(addr stack, addr pos);
int gettagbody_scope_evalstack(addr stack, addr pos, addr *ret);
void settagbody_scope_evalstack(addr stack, addr pos);
int getblock_scope_evalstack(addr stack, addr pos, addr *ret);
void setblock_scope_evalstack(addr stack, addr pos);

void setvalue_lexical_evalstack(addr stack, addr pos);
void setfunction_lexical_evalstack(addr stack, addr pos);
void settagbody_lexical_evalstack(addr stack, addr pos);
void setblock_lexical_evalstack(addr stack, addr pos);

int find_plistlist_evalstack(addr stack, addr key, addr symbol);
int find_special_evalstack(addr stack, addr symbol);

int find_global_special_evalstack(addr stack, addr symbol, addr *ret);
void push_global_special_evalstack(addr stack, addr value);

#endif

