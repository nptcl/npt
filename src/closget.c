#include "clos_variable.h"
#include "closget.h"
#include "typedef.h"

/*
 *  access
 */
#if 0
static int clos_access_version_get(addr clos, addr pos, fixnum *ret)
{
	addr vector;

	if (clos != Clos_standard_class)
		goto error;
	GetValueClos(pos, &pos);
	GetClosVector(pos, Clos_class_version, &pos);
	if (! fixnump(pos))
		goto error;
	GetFixnum(pos, ret);
	return 0;

error:
	*ret = 0;
	return 1;
}

static int clos_access_version(addr clos, addr pos, int *ret)
{
	ClosAccess r;
	addr check;
	fixnum x, y;

	GetVersionClos(pos, &x);
	if (clos_access_version_get(clos, pos, &y)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (x == y);
		return 0;
	}
}

static int clos_access_position(addr pos, addr key, size_t *ret)
{
	addr slots, check;
	size_t index;

	GetSlotsClos(pos, &slots);
	for (index = 0; ; index++) {
		if (! consp_getcons(slots, &check, &slots)) {
			*ret = 0;
			return 1;
		}
		if (check == key)
			break;
	}

	*ret = index;
	return 0;
}

static int clos_access_prototype(addr clos,
		addr pos, addr key, addr *ret, ClosAccess *rr)
{
	addr prototype;

	GetConst(CLOSNAME_PROTOTYPE, &prototype);
	if (clos_access(clos, prototype, &prototype, rr))
		return 1;
	if (clos_access(prototype, key, ret, rr))
		return 1;

	return 0;
}

static int clos_access_call(addr pos, addr key, addr *ret, ClosAccess *rr)
{
	int check;
	fixnum version;
	addr clos, vector, shared, value;
	size_t index;

	/* class-of */
	if (! closp(pos))
		goto error;
	GetClassOfClos(pos, &clos);
	if (! closp(clos))
		goto error;

	/* version */
	if (clos_access_version(clos, pos, &check))
		goto error;
	if (! check) {
		*rr = clos_access_update;
		return 1;
	}

	/* index */
	if (clos_access_position(pos, key, &index)) {
		*rr = clos_access_missing;
		return 1;
	}

	/* get */
	GetValueClos(pos, &vector);
	GetClosObjects(vector, index, &value);
	GetConst(CLOSNAME_SHARED, &shared);
	if (value == shared)
		return clos_access_prototype(clos, pos, key, ret, rr);
	if (value == Unbound) {
		*rr = clos_access_unbound;
		return 1;
	}

	/* value */
	*ret = value;
	*rr = clos_access_value;
	return 0;

error:
	*ret = Unbound;
	*rr = clos_access_error;
	return 1;
}

int clos_access(addr pos, addr key, addr *ret, ClosAccess *rr)
{
	ClosAccess reason;
	addr value;

	if (ret == NULL)
		ret = &value;
	if (rr == NULL)
		rr = &reason;

	return clos_access_call(pos, key, ret, rr);
}
#endif

