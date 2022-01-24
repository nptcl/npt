#include "condition.h"
#include "execute.h"
#include "type.h"
#include "type_delay.h"
#include "type_parse.h"
#include "type_memory.h"
#include "typedef.h"

static int get_delay_type_no_error(Execute ptr, addr pos, addr *ret)
{
	int notp;
	addr expr, type;

	CheckType(pos, LISPTYPE_TYPE);
	Check(RefLispDecl(pos) != LISPDECL_DELAY, "decl error");
	GetArrayType(pos, 1, &type);
	if (type != Nil)
		return Result(ret, type);

	/* parse-type */
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &expr);
	if (notp) {
		Return(parse_type_not(ptr, &type, expr, Nil));
	}
	else {
		Return(parse_type(ptr, &type, expr, Nil));
	}

	/* error check */
	if (type_delay_p(type))
		return Result(ret, Nil);

	/* Result */
	if (notp) {
		SetNotDecl(pos, 0);
	}
	SetArrayType(pos, 1, type);

	return Result(ret, type);
}

int get_delay_type_(Execute ptr, addr pos, addr *ret)
{
	addr check;

	Return(get_delay_type_no_error(ptr, pos, &check));
	if (check != Nil)
		return Result(ret, check);

	/* error */
	*ret = Nil;
	GetArrayType(pos, 0, &check);
	return call_type_error_va_(ptr, check, Nil, "Invalid type-spec ~S.", check, NULL);
}


/*
 *  check-delay-type
 */
typedef int (*type_delay_calltype)(Execute, addr, int, int *);
static type_delay_calltype TypeDelayCall[LISPDECL_SIZE];

static int check_delay_call_(Execute ptr, addr pos, int errorp, int *ret)
{
	LispDecl decl;
	type_delay_calltype call_;

	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl(pos, &decl);
	call_ = TypeDelayCall[decl];
	if (call_ == NULL)
		return Result(ret, 1);

	return (*call_)(ptr, pos, errorp ,ret);
}

static int check_delay_type_delay_(Execute ptr, addr pos, int errorp, int *ret)
{
	addr check;

	Return(get_delay_type_no_error(ptr, pos, &check));
	if (check != Nil)
		return Result(ret, 1);
	if (! errorp)
		return Result(ret, 0);

	/* type-error */
	*ret = 0;
	GetArrayType(pos, 0, &check);
	return call_type_error_va_(ptr, check, Nil, "Invalid type-spec ~S.", check, NULL);
}

static int check_delay_type_get1_(Execute ptr, addr pos, int errorp, int *ret)
{
	GetArrayType(pos, 0, &pos);
	return check_delay_call_(ptr, pos, errorp, ret);
}

static int check_delay_type_loop1_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr vector;
	size_t size, i;

	GetArrayType(pos, 0, &vector);
	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &pos);
		Return(check_delay_call_(ptr, pos, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_delay_type_cons_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* car */
	GetArrayType(pos, 0, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* cdr */
	GetArrayType(pos, 1, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int check_delay_type_loop_(Execute ptr, addr list, int errorp, int *ret)
{
	int check;
	addr x;

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(check_delay_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_delay_type_values_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* var */
	GetArrayType(pos, 0, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* opt */
	GetArrayType(pos, 1, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* rest */
	GetArrayType(pos, 2, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int check_delay_type_function_key_(Execute ptr, addr list, int errorp, int *ret)
{
	int check;
	addr x;

	if (list == T)
		return Result(ret, 1);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCdr(x, &x);
		Return(check_delay_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int check_delay_type_function_arguments_(Execute ptr,
		addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	if (type_astert_p(pos))
		return Result(ret, 1);

	/* var */
	GetArrayA2(pos, 0, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* opt */
	GetArrayA2(pos, 1, &x);
	Return(check_delay_type_loop_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* rest */
	GetArrayA2(pos, 2, &x);
	if (x != Nil) {
		Return(check_delay_call_(ptr, x, errorp, &check));
		if (! check)
			return Result(ret, 0);
	}

	/* key */
	GetArrayA2(pos, 3, &x);
	Return(check_delay_type_function_key_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return 0;
}

static int check_delay_type_function_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	addr x;

	/* arguments */
	GetArrayType(pos, 0, &x);
	Return(check_delay_type_function_arguments_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	/* values */
	GetArrayType(pos, 1, &x);
	Return(check_delay_call_(ptr, x, errorp, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, 1);
}

int check_delay_type_(Execute ptr, addr pos, int *ret)
{
	return check_delay_call_(ptr, pos, 0, ret);
}

int execute_delay_type_(Execute ptr, addr pos)
{
	int ignore;
	return check_delay_call_(ptr, pos, 1, &ignore);
}

void init_type_delay(void)
{
	cleartype(TypeDelayCall);
	TypeDelayCall[LISPDECL_DELAY] = check_delay_type_delay_;
	TypeDelayCall[LISPDECL_OPTIMIZED] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_SUBTYPEP] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_AND] = check_delay_type_loop1_;
	TypeDelayCall[LISPDECL_OR] = check_delay_type_loop1_;
	TypeDelayCall[LISPDECL_NOT] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_CONS] = check_delay_type_cons_;
	TypeDelayCall[LISPDECL_VECTOR] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_ARRAY] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_SIMPLE_ARRAY] = check_delay_type_get1_;
	TypeDelayCall[LISPDECL_VALUES] = check_delay_type_values_;
	TypeDelayCall[LISPDECL_FUNCTION] = check_delay_type_function_;
	TypeDelayCall[LISPDECL_COMPILED_FUNCTION] = check_delay_type_function_;
}

