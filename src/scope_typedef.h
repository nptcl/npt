#ifndef __SCOPE_TYPEDEF_HEADER__
#define __SCOPE_TYPEDEF_HEADER__

#include "declare.h"
#include "parse_typedef.h"
#include "typedef.h"

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
	EvalLambda_Code,
	EvalLambda_Self,
	EvalLambda_Lexical,
	EvalLambda_Size
};

struct scope_struct {
	EvalParse type;
	OptimizeType optimize[EVAL_OPTIMIZE_SIZE];
};

struct let_struct {
	addr stack, args, decl, doc, cons, free, the, allocate;
};

struct lambda_struct {
	addr stack, call, table, lexical;
	addr args, decl, doc, cons, clos, free, the;
	addr form, defun, body_the;
	unsigned globalp;
	EvalParse eval;
};

struct mvbind_struct {
	addr stack, args, decl, doc, cons, free, the, expr, allocate;
};

#endif

