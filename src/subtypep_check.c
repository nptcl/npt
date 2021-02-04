#include "subtypep_check.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "subtypep_range.h"
#include "type.h"

/*
 *  range
 */
static int subtypep_number_count(addr pos, int *count);

static int subtypep_number_count_optimized(addr pos, int *count)
{
	get_type_optimized(&pos, pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_subtypep(addr pos, int *count)
{
	get_type_subtypep(&pos, pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_not(addr pos, int *count)
{
	GetArrayType(pos, 0, &pos);
	return subtypep_number_count(pos, count);
}

static int subtypep_number_count_andor(addr pos, int *count)
{
	addr value;
	size_t size, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		if (subtypep_number_count(value, count))
			return 1;
	}

	return 0;
}

static int subtypep_number_count_range(addr pos, int *count)
{
	if (! range_asterisk_p(pos))
		(*count)++;

	return (2 <= *count);
}

static int subtypep_number_count(addr pos, int *count)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return subtypep_number_count_optimized(pos, count);

		case LISPDECL_SUBTYPEP:
			return subtypep_number_count_subtypep(pos, count);

		case LISPDECL_NOT:
			return subtypep_number_count_not(pos, count);

		case LISPDECL_AND:
		case LISPDECL_OR:
			return subtypep_number_count_andor(pos, count);

		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SHORT_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
			return subtypep_number_count_range(pos, count);

		default:
			return 0;
	}
}


/*
 *  or
 */
static int subtypep_number_begin(addr pos);

static int subtypep_number_begin_optimized(addr pos)
{
	get_type_optimized(&pos, pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_subtypep(addr pos)
{
	get_type_subtypep(&pos, pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_not(addr pos)
{
	GetArrayType(pos, 0, &pos);
	return subtypep_number_begin(pos);
}

static int subtypep_number_begin_or(addr pos)
{
	int count;
	addr value;
	size_t size, i;

	count = 0;
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		if (subtypep_number_count(value, &count))
			return 1;
	}

	return 0;
}

static int subtypep_number_begin(addr pos)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return subtypep_number_begin_optimized(pos);

		case LISPDECL_SUBTYPEP:
			return subtypep_number_begin_subtypep(pos);

		case LISPDECL_NOT:
			return subtypep_number_begin_not(pos);

		case LISPDECL_AND:
		case LISPDECL_OR:
			return subtypep_number_begin_or(pos);

		default:
			return 0;
	}
}


/*
 *  interface
 */
int subtypep_number_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return subtypep_number_begin(pos);
}

