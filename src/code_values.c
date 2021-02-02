#include "code_values.h"
#include "condition_define.h"
#include "cons_list.h"
#include "execute_values.h"
#include "scope_let.h"
#include "subtypep.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  code
 */
static void get_type_values(addr type, size_t index, addr *ret)
{
	addr list, pos;
	size_t now;

	CheckType(type, LISPTYPE_TYPE);
	Check(RefLispDecl(type) != LISPDECL_VALUES, "decl error");
	now = 0;

	/* var */
	GetArrayType(type, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (index == now) {
			*ret = pos;
			return;
		}
		now++;
	}

	/* opt */
	GetArrayType(type, 1, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (index == now) {
			*ret = pos;
			return;
		}
		now++;
	}

	/* rest */
	GetArrayType(type, 2, ret);
}

static void get_length_values(addr type, size_t *ret)
{
	addr list;

	CheckType(type, LISPTYPE_TYPE);
	Check(RefLispDecl(type) != LISPDECL_VALUES, "decl error");
	/* var */
	GetArrayType(type, 0, &list);
	*ret = length_list_unsafe(list);
}

static int values_typep_values_(Execute ptr, addr type)
{
	addr value, check;
	size_t must, size, i;

	CheckType(type, LISPTYPE_TYPE);
	Check(RefLispDecl(type) != LISPDECL_VALUES, "decl error");

	get_length_values(type, &must);
	size = lengthvalues_control(ptr);
	size = (size < must)? must: size;
	for (i = 0; i < size; i++) {
		getvalues_control(ptr, i, &value);
		if (value == Unbound)
			value = Nil;
		get_type_values(type, i, &check);
		Return(call_typep_error_(ptr, value, check));
	}

	return 0;
}

int values_typep_error_(Execute ptr, addr type)
{
	enum LISPDECL decl;
	addr value;

	CheckType(type, LISPTYPE_TYPE);
	GetLispDecl(type, &decl);
	Check(decl == LISPDECL_SUBTYPEP, "type error, subtypep");
	Check(decl == LISPDECL_OPTIMIZED, "type error, optimized");
	if (decl == LISPDECL_VALUES)
		return values_typep_values_(ptr, type);

	getresult_control(ptr, &value);
	return call_typep_error_(ptr, value, type);
}

