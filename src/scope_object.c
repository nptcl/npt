#include <memory.h>
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "eval_object.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "heap.h"
#include "parse_object.h"
#include "scope_function.h"
#include "scope_lambda.h"
#include "scope_object.h"
#include "symbol.h"
#include "type_value.h"

/*
 *  memory
 */
static int optimize_eval_stack_(Execute ptr, OptimizeType *optimize)
{
	int i;
	addr stack;
	struct eval_stack *str;
	OptimizeType value;

	/* initial value */
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		optimize[i] = -1;

	/* stack */
	Return(getstack_eval_(ptr, &stack));
	while (stack != Nil) {
		str = StructEvalStack(stack);
		for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
			/* already set */
			if (0 <= optimize[i])
				continue;
			/* stack is not set */
			value = str->optimize[i];
			if (value < 0)
				continue;
			optimize[i] = value;
		}
		GetEvalStackNext(stack, &stack);
	}

	return 0;
}

int eval_scope_heap_(Execute ptr, addr *ret, size_t size)
{
	addr pos;
	struct scope_struct *a;

	Check(0xFF < sizeoft(struct scope_struct), "struct size error");
	Check(0xFF < 2UL + size, "size argument error");
	eval_heap(&pos, EVAL_TYPE_SCOPE,
			(byte)(2UL + size),
			(byte)sizeoft(struct scope_struct));
	a = StructEvalScope(pos);
	Return(optimize_eval_stack_(ptr, a->optimize));

	return Result(ret, pos);
}

int eval_scope_size_(Execute ptr, addr *ret, size_t size,
		EvalParse parse, addr type, addr value)
{
	addr pos;

	Return(eval_scope_heap_(ptr, &pos, size));
	SetEvalScopeType(pos, parse);
	SetEvalScopeThe(pos, type);
	SetEvalScopeValue(pos, value);

	return Result(ret, pos);
}

int make_eval_scope_(Execute ptr,
		addr *ret, EvalParse parse, addr type, addr value)
{
	return eval_scope_size_(ptr, ret, 0, parse, type, value);
}


/*
 *  eval-scope
 */
struct scope_struct *structevalscope(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return StructEvalScope_Low(pos);
}
EvalParse refevalscopetype(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeType_Low(pos);
}
void getevalscopetype(addr pos, EvalParse *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeType_Low(pos, ret);
}
void setevalscopetype(addr pos, EvalParse value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeType_Low(pos, value);
}
addr refevalscopethe(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeThe_Low(pos);
}
void getevalscopethe(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeThe_Low(pos, ret);
}
void setevalscopethe(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeThe_Low(pos, value);
}
addr refevalscopevalue(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeValue_Low(pos);
}
void getevalscopevalue(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue_Low(pos, ret);
}
void setevalscopevalue(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeValue_Low(pos, value);
}
addr refevalscopeindex(addr pos, size_t index)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeIndex_Low(pos, index);
}
void getevalscopeindex(addr pos, size_t index, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeIndex_Low(pos, index, ret);
}
void setevalscopeindex(addr pos, size_t index, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeIndex_Low(pos, index, value);
}


/*
 *  table
 */
eval_scope_calltype EvalScopeTable[EVAL_PARSE_SIZE];

int scope_eval(Execute ptr, addr *ret, addr eval)
{
	EvalParse type;
	eval_scope_calltype call;

	GetEvalParseType(eval, &type);
	call = EvalScopeTable[type];
	if (call == NULL)
		return fmte_("Invalid eval-parse type.", NULL);

	return (*call)(ptr, ret, eval);
}

int scope_allcons(Execute ptr, addr *retcons, addr *rettype, addr cons)
{
	addr root, expr;
	LocalHold hold;

	/* cons */
	hold = LocalHold_array(ptr, 1);
	for (root = expr = Nil; cons != Nil; ) {
		GetCons(cons, &expr, &cons);
		Return(scope_eval(ptr, &expr, expr));
		cons_heap(&root, expr, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(retcons, root);

	/* type */
	if (rettype) {
		if (expr == Nil)
			type_value_nil(rettype);
		else
			GetEvalScopeThe(expr, rettype);
	}

	return 0;
}

int localhold_scope_eval(LocalHold hold, Execute ptr, addr *ret, addr eval)
{
	Return(scope_eval(ptr, ret, eval));
	localhold_push(hold, *ret);
	return 0;
}

int localhold_scope_allcons(LocalHold hold,
		Execute ptr, addr *retcons, addr *rettype, addr cons)
{
	Return(scope_allcons(ptr, retcons, rettype, cons));
	if (rettype)
		localhold_pushva(hold, *retcons, *rettype, NULL);
	else
		localhold_push(hold, *retcons);

	return 0;
}


/*
 *  lexical
 */
static int scope_eval_lexical_object_(Execute ptr, addr stack, addr eval, addr *ret)
{
	addr type, pos;

	lambda_lexical_heap(stack, &pos);
	GetEvalScopeThe(eval, &type);
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_LEXICAL, type, eval));
	SetEvalScopeIndex(eval, 0, pos);

	return Result(ret, eval);
}

int scope_eval_lexical(Execute ptr, addr *ret, addr eval)
{
	addr stack;

	Return(newstack_lexical_(ptr, &stack));
	Return(scope_eval(ptr, &eval, eval));
	Return(scope_eval_lexical_object_(ptr, stack, eval, ret));

	return freestack_eval_(ptr, stack);
}

