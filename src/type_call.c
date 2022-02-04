#include "condition.h"
#include "function.h"
#include "subtypep.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "type_call.h"
#include "type_delay.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  typep-function
 */
static int typep_function_args_(Execute ptr, addr x, addr y, int *ret)
{
	GetArrayType(x, 0, &x);  /* args */
	GetArrayType(y, 0, &y);  /* args */
	if (type_asterisk_p(y))
		return Result(ret, 1);
	if (type_asterisk_p(x))
		return Result(ret, 0);

	/* subtypep */
	return Result(ret, -1);
}

static int typep_function_values_(Execute ptr, addr x, addr y, int *ret)
{
	GetArrayType(x, 1, &x);  /* values */
	GetArrayType(y, 1, &y);  /* values */
	if (type_asterisk_p(y))
		return Result(ret, 1);
	if (type_asterisk_p(x))
		return Result(ret, 0);

	/* subtypep */
	return Result(ret, -1);
}

static int typep_function_call_(Execute ptr, addr x, addr y, int *ret)
{
	LispDecl dx, dy;
	int notp, check;

	/* not */
	GetNotDecl(x, &notp);
	if (notp)
		goto subtypep;
	GetNotDecl(y, &notp);
	if (notp)
		goto subtypep;
	GetLispDecl(x, &dx);
	GetLispDecl(y, &dy);

	/* subtypep */
	if (dy == LISPDECL_SUBTYPEP) {
		get_type_subtypep(&y, y);
		return typep_function_call_(ptr, x, y, ret);
	}
	if (dx == LISPDECL_SUBTYPEP) {
		get_type_subtypep(&x, x);
		return typep_function_call_(ptr, x, y, ret);
	}

	/* optimized */
	if (dy == LISPDECL_OPTIMIZED) {
		get_type_optimized(&y, y);
		return typep_function_call_(ptr, x, y, ret);
	}
	if (dx == LISPDECL_OPTIMIZED) {
		get_type_optimized(&x, x);
		return typep_function_call_(ptr, x, y, ret);
	}

	/* delay */
	if (dy == LISPDECL_DELAY) {
		Return(get_delay_type_(ptr, y, &y));
		return typep_function_call_(ptr, y, y, ret);
	}
	if (dx == LISPDECL_DELAY) {
		Return(get_delay_type_(ptr, x, &x));
		return typep_function_call_(ptr, x, y, ret);
	}

	/* function */
	if (! decl_function_p(dy))
		goto subtypep;
	if (! decl_function_p(dx))
		goto subtypep;
	if (dx == LISPDECL_FUNCTION && dy == LISPDECL_COMPILED_FUNCTION)
		return Result(ret, 0);

	/* arguments */
	Return(typep_function_args_(ptr, x, y, &check));
	if (check < 0)
		goto subtypep;
	if (! check)
		return Result(ret, 0);

	/* values */
	Return(typep_function_values_(ptr, x, y, &check));
	if (check < 0)
		goto subtypep;
	return Result(ret, check);

subtypep:
	return subtypep_check_(ptr, x, y, Nil, ret, NULL);
}

static int typep_function_type_(Execute ptr, addr value, addr y, int *ret)
{
	addr x;

	gettype_function(value, &x);
	if (x == Nil) {
		if (compiled_function_p(value))
			GetTypeTable(&x, CompiledFunction);
		else
			GetTypeTable(&x, Function);
	}
	CheckType(x, LISPTYPE_TYPE);

	return typep_function_call_(ptr, x, y, ret);
}


/*
 *  function
 */
int typep_function_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (check == Nil) {
		*ret = 0;
		return fmte_("The cons type (FUNCTION ? ?) don't accept.", NULL);
	}
	if (! functionp(value))
		return Result(ret, 0);

	return typep_function_type_(ptr, value, type, ret);
}


/*
 *  compiled-function
 */
int typep_compiled_function_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (check == Nil) {
		*ret = 0;
		return fmte_("The cons type (COMPILED-FUNCTION ? ?) don't accept.", NULL);
	}
	if (! compiled_function_p(value))
		return Result(ret, 0);

	return typep_function_type_(ptr, value, type, ret);
}

