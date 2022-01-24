#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "compile_eval.h"
#include "compile_file.h"
#include "constant.h"
#include "cons.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute_values.h"
#include "hold.h"
#include "load_instance.h"
#include "load_time_value.h"
#include "object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  make-load-form initialize
 */
static int make_load_form_lambda_(addr pos, addr g, addr value, addr *ret);
static int make_load_form_lambda_cons_(addr pos, addr g, addr value, addr *ret)
{
	addr car, cdr;

	GetCons(value, &car, &cdr);
	Return(make_load_form_lambda_(pos, g, car, &car));
	Return(make_load_form_lambda_(pos, g, cdr, &cdr));
	cons_heap(ret, car, cdr);

	return 0;
}

static int make_load_form_lambda_vector_(addr pos, addr g, addr value, addr *ret)
{
	addr vector, x;
	size_t size, i;

	lenarray(value, &size);
	vector_type_heap(&vector, value, size);
	for (i = 0; i < size; i++) {
		getarray(value, i, &x);
		Return(make_load_form_lambda_(pos, g, x, &x));
		setarray(vector, i, x);
	}

	return Result(ret, vector);
}

static int make_load_form_lambda_array_heap_(addr *ret, addr value)
{
	addr array, x;
	struct array_struct *str;

	array_empty_heap(&array);
	str = ArrayInfoStruct(array);
	*str = *ArrayInfoStruct(value);
	str->displaced = 0;
	str->simple = str->adjustable == 0 && str->fillpointer == 0;
	str->offset = 0;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &x);
	SetArrayInfo(array, ARRAY_INDEX_TYPE, x);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &x);
	SetArrayInfo(array, ARRAY_INDEX_DIMENSION, x);
	Return(array_allocate_(NULL, array, str));

	return Result(ret, array);
}

static int make_load_form_lambda_array_copy_(addr pos, addr g, addr array, addr value)
{
	addr x;
	size_t size, i;
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	size = str->front;
	for (i = 0; i < size; i++) {
		Return(array_get_t_(value, i, &x));
		Return(make_load_form_lambda_(pos, g, x, &x));
		Return(array_set_(array, i, x));
	}

	return 0;
}

static int make_load_form_lambda_array_(addr pos, addr g, addr value, addr *ret)
{
	addr array;

	if (array_system_specialized_p(value))
		return Result(ret, value);

	Return(make_load_form_lambda_array_heap_(&array, value));
	Return(make_load_form_lambda_array_copy_(pos, g, array, value));

	return Result(ret, array);
}

static int make_load_form_lambda_(addr pos, addr g, addr value, addr *ret)
{
	if (pos == value)
		return Result(ret, g);

	switch (GetType(value)) {
		case LISPTYPE_CONS:
			return make_load_form_lambda_cons_(pos, g, value, ret);

		case LISPTYPE_VECTOR:
			return make_load_form_lambda_vector_(pos, g, value, ret);

		case LISPTYPE_ARRAY:
			return make_load_form_lambda_array_(pos, g, value, ret);

		default:
			return Result(ret, value);
	}
}

static int make_load_form_replace_(Execute ptr, addr pos, addr init, addr *ret)
{
	addr g, lambda, declare, ignorable;

	if (init == Nil)
		return Result(ret, Nil);

	/* (lambda (g)
	 *   (declare (ignorable g))
	 *   [replace pos -> g])
	 */
	Return(make_gensym_(ptr, &g));
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);

	Return(make_load_form_lambda_(pos, g, init, &init));
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&g, g, NULL);
	list_heap(ret, lambda, g, declare, init, NULL);

	return 0;
}


/*
 *  make-load-form generic function
 */
static int make_load_form_generic_call_(
		Execute ptr, LocalHold hold, addr pos, addr *rexpr, addr *rinit)
{
	addr call, expr, init;

	GetConst(COMMON_MAKE_LOAD_FORM, &call);
	GetFunctionSymbol(call, &call);
	Return(funcall_control_(ptr, call, pos, NULL));
	/* result */
	getresult_control(ptr, &expr);
	localhold_set(hold, 0, expr);
	getvalues_control(ptr, 1, &init);
	if (init == Unbound)
		init = Nil;
	localhold_set(hold, 1, init);

	*rexpr = expr;
	*rinit = init;
	return 0;
}

static int make_load_form_generic_(Execute ptr, addr pos, addr *ret1, addr *ret2)
{
	addr control, expr, init;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	/* (make-load-form clos) */
	push_control(ptr, &control);
	expr = init = Nil;
	(void)make_load_form_generic_call_(ptr, hold, pos, &expr, &init);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	*ret1 = expr;
	return make_load_form_replace_(ptr, pos, init, ret2);
}


/*
 *  intern
 */
int intern_load_instance_(Execute ptr, addr pos)
{
	addr index, expr, init;

	Check(! eval_compile_p(ptr), "mode error");

	/* already interned */
	Return(get_load_table_(ptr, pos, &index));
	if (index != Nil)
		return 0;

	/* make-load-form */
	Return(make_load_form_generic_(ptr, pos, &expr, &init));
	return compile_instance_(ptr, pos, expr, init);
}

