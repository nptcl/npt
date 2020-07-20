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
_g void eval_scope_heap(Execute ptr, addr *ret, size_t size)
{
	addr pos, stack;
	struct eval_scope *a;
	struct eval_stack *b;

	Check(0xFF < sizeoft(struct eval_scope), "struct size error");
	Check(0xFF < 2UL + size, "size argument error");
	eval_heap(&pos, EVAL_TYPE_SCOPE,
			(byte)(2UL + size),
			(byte)sizeoft(struct eval_scope));
	getstack_eval(ptr, &stack);
	a = StructEvalScope(pos);
	b = StructEvalStack(stack);
	memcpy(a->optimize, b->optimize, sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
	*ret = pos;
}

_g void eval_scope_size(Execute ptr, addr *ret, size_t size,
		EvalParse parse, addr type, addr value)
{
	addr pos;

	eval_scope_heap(ptr, &pos, size);
	SetEvalScopeType(pos, parse);
	SetEvalScopeThe(pos, type);
	SetEvalScopeValue(pos, value);
	*ret = pos;
}

_g void make_eval_scope(Execute ptr,
		addr *ret, EvalParse parse, addr type, addr value)
{
	eval_scope_size(ptr, ret, 0, parse, type, value);
}


/*
 *  eval-scope
 */
_g struct eval_scope *structevalscope(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return StructEvalScope_Low(pos);

}
_g EvalParse refevalscopetype(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeType_Low(pos);
}
_g void getevalscopetype(addr pos, EvalParse *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeType_Low(pos, ret);
}
_g void setevalscopetype(addr pos, EvalParse value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeType_Low(pos, value);
}
_g addr refevalscopethe(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeThe_Low(pos);
}
_g void getevalscopethe(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeThe_Low(pos, ret);
}
_g void setevalscopethe(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeThe_Low(pos, value);
}
_g addr refevalscopevalue(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeValue_Low(pos);
}
_g void getevalscopevalue(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue_Low(pos, ret);
}
_g void setevalscopevalue(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeValue_Low(pos, value);
}
_g addr refevalscopeindex(addr pos, size_t index)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeIndex_Low(pos, index);
}
_g void getevalscopeindex(addr pos, size_t index, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeIndex_Low(pos, index, ret);
}
_g void setevalscopeindex(addr pos, size_t index, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeIndex_Low(pos, index, value);
}


/*
 *  table
 */
_g eval_scope_calltype EvalScopeTable[EVAL_PARSE_SIZE];

_g int scope_eval(Execute ptr, addr *ret, addr eval)
{
	EvalParse type;
	eval_scope_calltype call;

	GetEvalParseType(eval, &type);
	call = EvalScopeTable[type];
	if (call == NULL)
		return fmte_("Invalid eval-parse type.", NULL);

	return (*call)(ptr, ret, eval);
}

_g int scope_allcons(Execute ptr, addr *retcons, addr *rettype, addr cons)
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

_g int localhold_scope_eval(LocalHold hold, Execute ptr, addr *ret, addr eval)
{
	Return(scope_eval(ptr, ret, eval));
	localhold_push(hold, *ret);
	return 0;
}

_g int localhold_scope_allcons(LocalHold hold,
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
static void scope_eval_lexical_object(Execute ptr, addr stack, addr eval, addr *ret)
{
	addr type, pos;

	lambda_lexical_heap(stack, &pos);
	GetEvalScopeThe(eval, &type);
	eval_scope_size(ptr, &eval, 1, EVAL_PARSE_LEXICAL, type, eval);
	SetEvalScopeIndex(eval, 0, pos);
	*ret = eval;
}

_g int scope_eval_lexical(Execute ptr, addr *ret, addr eval)
{
	addr stack;

	stack = newstack_lexical(ptr);
	Return(scope_eval(ptr, &eval, eval));
	scope_eval_lexical_object(ptr, stack, eval, ret);
	freestack_eval(ptr, stack);

	return 0;
}

