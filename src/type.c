#include <stdarg.h>
#include "type_coerce.h"
#include "type_copy.h"
#include "type_delay.h"
#include "type_memory.h"
#include "type_name.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_symbol.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_upgraded.h"
#include "type_value.h"
#include "typedef.h"

/*
 *  check
 */
static int decl_function_p(LispDecl type)
{
	return type == LISPDECL_FUNCTION
		|| type == LISPDECL_COMPILED_FUNCTION;
}

static int decl_astert_p(LispDecl type)
{
	return type == LISPDECL_ASTERISK
		|| type == LISPDECL_T;
}

int decl_character_p(LispDecl type)
{
	return type == LISPDECL_CHARACTER
		|| type == LISPDECL_BASE_CHAR
		|| type == LISPDECL_STANDARD_CHAR
		|| type == LISPDECL_EXTENDED_CHAR;
}

int decl_float_p(LispDecl type)
{
	return type == LISPDECL_FLOAT
		|| type == LISPDECL_SINGLE_FLOAT
		|| type == LISPDECL_DOUBLE_FLOAT
		|| type == LISPDECL_LONG_FLOAT
		|| type == LISPDECL_SHORT_FLOAT;
}

int decl_range_p(LispDecl type)
{
	return type == LISPDECL_INTEGER
		|| type == LISPDECL_RATIONAL
		|| type == LISPDECL_REAL
		|| decl_float_p(type);
}

int decl_subtypep_real(LispDecl left, LispDecl right)
{
	switch (right) {
		case LISPDECL_INTEGER:
			return left == LISPDECL_INTEGER;

		case LISPDECL_RATIONAL:
			return left == LISPDECL_INTEGER
				|| left == LISPDECL_RATIONAL;

		case LISPDECL_REAL:
			return left == LISPDECL_INTEGER
				|| left == LISPDECL_RATIONAL
				|| left == LISPDECL_REAL
				|| decl_float_p(left);

		case LISPDECL_FLOAT:
			return decl_float_p(left);

		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return left == right;

		default:
			break;
	}

	return 0;
}

int type_delay_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && RefLispDecl(pos) == LISPDECL_DELAY;
}

int type_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && decl_function_p(LowLispDecl(pos));
}

int type_astert_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && decl_astert_p(LowLispDecl(pos));
}

int type_function_aster_p(addr pos)
{
	LispDecl type;
	addr args, values;

	if (GetType(pos) != LISPTYPE_TYPE)
		return 0;
	type = LowLispDecl(pos);
	if (type != LISPDECL_FUNCTION && type != LISPDECL_COMPILED_FUNCTION)
		return 0;
	GetArrayType(pos, 0, &args);
	GetArrayType(pos, 1, &values);
	return type_asterisk_p(args) && type_asterisk_p(values);
}

int type_asterisk_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && LowLispDecl(pos) == LISPDECL_ASTERISK;
}

int type_range_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return decl_range_p(LowLispDecl(pos));
}

int type_string_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (LowLispDecl(pos)) {
		case LISPDECL_STRING:
		case LISPDECL_BASE_STRING:
		case LISPDECL_SIMPLE_STRING:
		case LISPDECL_SIMPLE_BASE_STRING:
			return 1;

		default:
			return 0;
	}
}


/*
 *  init
 */
void init_type(void)
{
	init_type_coerce();
	init_type_copy();
	init_type_delay();
	init_type_name();
	init_type_object();
	init_type_parse();
	init_type_symbol();
	init_type_typep();
	init_type_value();
}

void build_type(void)
{
	build_type_table();
	build_type_constant();
	build_type_upgraded();
	build_type_symbol();
	build_type_parse();
}

