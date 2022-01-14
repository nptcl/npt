#include "clos.h"
#include "constant.h"
#include "cons.h"
#include "execute.h"
#include "execute_values.h"
#include "integer.h"
#include "mop_generic.h"
#include "object.h"
#include "random_state.h"
#include "stream_memory.h"
#include "stream_object.h"
#include "strtype.h"
#include "structure.h"
#include "structure_change.h"
#include "structure_delete.h"
#include "symbol.h"
#include "sysctl.h"
#include "typedef.h"

/*
 *  recovery
 */
static int sysctl_recovery_no_applicable_method_(Execute ptr)
{
	addr pos;

	/* fmakunbound */
	GetConst(COMMON_NO_APPLICABLE_METHOD, &pos);
	SetFunctionSymbol(pos, Unbound);

	/* recovery */
	Return(defgeneric_no_applicable_method_mop_(ptr));
	setvalues_control(ptr, T, T, NULL);

	return 0;
}

static int sysctl_recovery_no_next_method_(Execute ptr)
{
	addr pos;

	/* fmakunbound */
	GetConst(COMMON_NO_NEXT_METHOD, &pos);
	SetFunctionSymbol(pos, Unbound);

	/* recovery */
	Return(defgeneric_no_next_method_mop_(ptr));
	setvalues_control(ptr, T, T, NULL);

	return 0;
}

static int sysctl_recovery_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* no-applicable-method */
	Return(string_designer_equalp_char_(pos, "no-applicable-method", &check));
	if (check)
		return sysctl_recovery_no_applicable_method_(ptr);

	/* no-next-method */
	Return(string_designer_equalp_char_(pos, "no-next-method", &check));
	if (check)
		return sysctl_recovery_no_next_method_(ptr);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  clos
 */
static int sysctl_clos_slots_(Execute ptr, addr pos)
{
	clos_getslots_heap(pos, &pos);
	setvalues_control(ptr, pos, T, NULL);
	return 0;
}

static int sysctl_clos_(Execute ptr, addr pos, addr args)
{
	int check;
	addr car;

	if (! consp_getcons(args, &car, &args))
		goto error;

	/* slots */
	Return(string_designer_equalp_char_(car, "slots", &check));
	if (check)
		return sysctl_clos_slots_(ptr, pos);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  memory-stream
 */
static int sysctl_memory_stream_size_(Execute ptr, addr pos)
{
	size_t size;

	getsize_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_memory_stream_array_(Execute ptr, addr pos)
{
	size_t size;

	getarray_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_memory_stream_cache_(Execute ptr, addr pos)
{
	int cache;

	cache = getcache_memory_stream(pos);
	setvalues_control(ptr, cache? T: Nil, T, NULL);

	return 0;
}

static int sysctl_memory_stream_(Execute ptr, addr pos, addr args)
{
	int check;
	addr car;

	if (! consp_getcons(args, &car, &args))
		goto error;

	/* size */
	Return(string_designer_equalp_char_(car, "size", &check));
	if (check)
		return sysctl_memory_stream_size_(ptr, pos);

	/* array */
	Return(string_designer_equalp_char_(car, "array", &check));
	if (check)
		return sysctl_memory_stream_array_(ptr, pos);

	/* cache */
	Return(string_designer_equalp_char_(car, "cache", &check));
	if (check)
		return sysctl_memory_stream_cache_(ptr, pos);

	/* error */
error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  structure
 */
static int sysctl_structure_check_(Execute ptr, addr pos)
{
	int check;

	check = structure_get_object(pos, &pos);
	setvalues_control(ptr, check? T: Nil, T, NULL);
	return 0;
}

static int sysctl_structure_delete_(Execute ptr, addr pos)
{
	int check;

	Return(structure_delete_(ptr, pos, &check));
	pos = check? T: Nil;
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_structure_type_(Execute ptr, addr pos)
{
	Return(structure_get_type_(pos, &pos));
	setvalues_control(ptr, pos, T, NULL);

	return 0;
}

static int sysctl_structure_(Execute ptr, addr args)
{
	int check;
	addr name, pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (! consp_getcons(args, &name, &args))
		goto error;

	/* check */
	Return(string_designer_equalp_char_(pos, "check", &check));
	if (check)
		return sysctl_structure_check_(ptr, name);

	/* delete */
	Return(string_designer_equalp_char_(pos, "delete", &check));
	if (check)
		return sysctl_structure_delete_(ptr, name);

	/* type */
	Return(string_designer_equalp_char_(pos, "type", &check));
	if (check)
		return sysctl_structure_type_(ptr, name);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  random
 */
static int sysctl_random_integer_(Execute ptr, addr args)
{
	addr value;

	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	Return(random_state_integer_(value, &value));
	setvalues_control(ptr, value, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_random_make_(Execute ptr, addr args)
{
	addr value;

	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	Return(random_state_make_(ptr->local, value, &value));
	setvalues_control(ptr, value, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_random_write_(Execute ptr, addr args)
{
	addr pos, value;

	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
	Return(random_state_write_(ptr->local, pos, value));
	setvalues_control(ptr, pos, T, NULL);
	return 0;

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

static int sysctl_random_(Execute ptr, addr args)
{
	int check;
	addr pos;

	if (! consp_getcons(args, &pos, &args))
		goto error;

	/* integer */
	Return(string_designer_equalp_char_(pos, "integer", &check));
	if (check)
		return sysctl_random_integer_(ptr, args);

	/* make */
	Return(string_designer_equalp_char_(pos, "make", &check));
	if (check)
		return sysctl_random_make_(ptr, args);

	/* write */
	Return(string_designer_equalp_char_(pos, "write", &check));
	if (check)
		return sysctl_random_write_(ptr, args);

error:
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}


/*
 *  sysctl
 */
int sysctl_values_(Execute ptr, addr pos, addr args)
{
	int check;

	/* memory stream */
	if (memory_stream_p(pos))
		return sysctl_memory_stream_(ptr, pos, args);

	/* clos object */
	if (closp(pos))
		return sysctl_clos_(ptr, pos, args);

	/* recovery */
	Return(string_designer_equalp_char_(pos, "recovery", &check));
	if (check)
		return sysctl_recovery_(ptr, args);

	/* structure */
	Return(string_designer_equalp_char_(pos, "structure", &check));
	if (check)
		return sysctl_structure_(ptr, args);

	/* random */
	Return(string_designer_equalp_char_(pos, "random", &check));
	if (check)
		return sysctl_random_(ptr, args);

	/* error */
	setvalues_control(ptr, Nil, Nil, NULL);
	return 0;
}

