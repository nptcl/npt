#ifndef __EVAL_STACK_HEADER__
#define __EVAL_STACK_HEADER__

#include "eval.h"
#include "eval_declare.h"
#include "local.h"

enum EVAL_STACK {
	EVAL_STACK_NEXT,
	EVAL_STACK_TABLE,
	EVAL_STACK_SIZE
};

enum EVAL_STACK_MODE {
	EVAL_STACK_MODE_NIL,
	EVAL_STACK_MODE_LAMBDA,
	EVAL_STACK_MODE_TAGBODY,
	EVAL_STACK_MODE_BLOCK,
	EVAL_STACK_MODE_SIZE
};

struct eval_stack {
	unsigned globalp : 1;
	enum EVAL_STACK_MODE type;
	LocalStack stack;
	OptimizeType optimize[EVAL_OPTIMIZE_SIZE];
};

#define PtrEvalStack(x)				PtrEvalBody(x, EVAL_STACK_SIZE)
#define StructEvalStack_Low(x)		((struct eval_stack *)PtrEvalStack(x))
#define RefEvalStackType_Low(x)		(StructEvalStack_Low(x)->type)
#define GetEvalStackType_Low(x,v)	(*(v) = RefEvalStackType_Low(x))
#define SetEvalStackType_Low(x,v)	(RefEvalStackType_Low(x) = (v))
#define GetEvalStackNext_Low(x,v)	GetEval((x),EVAL_STACK_NEXT,(v))
#define SetEvalStackNext_Low(x,v)	SetEval((x),EVAL_STACK_NEXT,(v))
#define GetEvalStackTable_Low(x,v)	GetEval((x),EVAL_STACK_TABLE,(v))
#define SetEvalStackTable_Low(x,v)	SetEval((x),EVAL_STACK_TABLE,(v))

#ifdef LISP_DEBUG
#define StructEvalStack(x)			structevalstack(x)
#define RefEvalStackType(x)			refevalstacktype(x)
#define GetEvalStackType(x,v)		getevalstacktype(x,v)
#define SetEvalStackType(x,v)		setevalstacktype(x,v)
#define GetEvalStackNext(x,v)		getevalstacknext(x,v)
#define SetEvalStackNext(x,v)		setevalstacknext(x,v)
#define GetEvalStackTable(x,v)		getevalstacktable(x,v)
#define SetEvalStackTable(x,v)		setevalstacktable(x,v)
#else
#define StructEvalStack(x)			StructEvalStack_Low(x)
#define RefEvalStackType(x)			RefEvalStackType_Low(x)
#define GetEvalStackType(x,v)		GetEvalStackType_Low(x,v)
#define SetEvalStackType(x,v)		SetEvalStackType_Low(x,v)
#define GetEvalStackNext(x,v)		GetEvalStackNext_Low(x,v)
#define SetEvalStackNext(x,v)		SetEvalStackNext_Low(x,v)
#define GetEvalStackTable(x,v)		GetEvalStackTable_Low(x,v)
#define SetEvalStackTable(x,v)		SetEvalStackTable_Low(x,v)
#endif

#define newstack_nil(p) newstack_eval((p), EVAL_STACK_MODE_NIL)
#define newstack_lambda(p) newstack_eval((p), EVAL_STACK_MODE_LAMBDA)
#define newstack_tagbody(p) newstack_eval((p), EVAL_STACK_MODE_TAGBODY)
#define newstack_block(p) newstack_eval((p), EVAL_STACK_MODE_BLOCK)

_g void eval_stack_alloc(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type);
_g void eval_stack_local(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type);
_g void eval_stack_heap(addr *ret, enum EVAL_STACK_MODE type);

_g struct eval_stack *structevalstack(addr pos);
_g enum EVAL_STACK_MODE refevalstacktype(addr pos);
_g void getevalstacktype(addr pos, enum EVAL_STACK_MODE *ret);
_g void setevalstacktype(addr pos, enum EVAL_STACK_MODE value);
_g void getevalstacknext(addr pos, addr *ret);
_g void setevalstacknext(addr pos, addr value);
_g void getevalstacktable(addr pos, addr *ret);
_g void setevalstacktable(addr pos, addr value);

_g void getstack_eval(Execute ptr, addr *ret);
_g void getglobal_eval(Execute ptr, addr *ret);
_g addr newstack_eval(Execute ptr, enum EVAL_STACK_MODE type);
_g void freestack_eval(Execute ptr, addr scope);
_g void init_eval_stack(Execute ptr);
_g void free_eval_stack(Execute ptr);
_g int globalp_stack_eval(addr pos);

_g void apply_declaim_stack(Execute ptr, addr declare);
_g void apply_declare_stack(LocalRoot local, addr stack, addr declare);
_g void apply_declare_value_stack(LocalRoot local, addr stack, addr symbol, addr declare);
_g void apply_declare_function_stack(LocalRoot local, addr stack, addr call, addr declare);

#endif

