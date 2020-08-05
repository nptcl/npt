#include "build.h"
#include "clos_cache.h"
#include "memory.h"
#include "sxhash.h"

/*
 *  cache -> nil | (object1 ...)
 */
_g int hashindex_cache_(addr right, size_t size, size_t *ret)
{
	addr left;
	size_t value, index;
	fixnum check;

	value = 0;
	for (index = 0; right != Nil; index++) {
		Check(GetType(right) != LISPTYPE_CONS, "type error");
		GetCons(right, &left, &right);
		Return(sxhash_eq_(left, &check));
		value += (size_t)check + index;
	}

	return Result(ret, value % size);
}

_g int cache_equal_function_(addr right1, addr right2, int *ret)
{
	int check1, check2;
	addr left1, left2;

	check1 = (right1 == Nil);
	check2 = (right2 == Nil);
	if (check1 && check2)
		return Result(ret, 1);
	if (check1)
		return Result(ret, 0);
	if (check2)
		return Result(ret, 0);
	Check(GetType(right1) != LISPTYPE_CONS, "type right1 error");
	Check(GetType(right2) != LISPTYPE_CONS, "type right2 error");
	GetCons(right1, &left1, &right1);
	GetCons(right2, &left2, &right2);
	if (left1 != left2)
		return Result(ret, 0);

	return cache_equal_function_(right1, right2, ret);
}

_g int cache_equal_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(cache_equal_function_(left, right, &check));
	return check;
}

