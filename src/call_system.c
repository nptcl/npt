#include "call_system.h"
#include "cons_plist.h"
#include "constant.h"
#include "eval_load.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  load
 */
enum LoadType {
	LoadType_unbound,
	LoadType_lisp,
	LoadType_fasl
};

static int load_common_type_(Execute ptr, addr rest, enum LoadType *ret)
{
	int check;
	addr pos;

	if (GetKeyArgs(rest, KEYWORD_TYPE, &pos))
		goto unbound;
	if (! symbolp(pos))
		goto unbound;
	GetNameSymbol(pos, &pos);

	/* lisp */
	Return(string_equalp_char_(pos, "LISP", &check));
	if (check)
		return Result(ret, LoadType_lisp);

	/* fasl */
	Return(string_equalp_char_(pos, "FASL", &check));
	if (check)
		return Result(ret, LoadType_fasl);

	/* unbound */
unbound:
	return Result(ret, LoadType_unbound);
}

static int load_common_switch_(Execute ptr, addr file,
		enum LoadType type, addr x, addr y, int e, addr z, int *ret)
{
	switch (type) {
		case LoadType_lisp:
			return eval_load_force_lisp_(ptr, ret, file, x, y, e, z);

		case LoadType_fasl:
			return eval_load_force_fasl_(ptr, ret, file, x, y, e, z);

		case LoadType_unbound:
		default:
			return eval_load_(ptr, ret, file, x, y, e, z);
	}
}

int load_common_(Execute ptr, addr file, addr rest, int *ret)
{
	enum LoadType type;
	int e;
	addr x, y, z, pos;

	/* :verbose */
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &x))
		x = Unbound;

	/* :print */
	if (GetKeyArgs(rest, KEYWORD_PRINT, &y))
		y = Unbound;

	/* :if-does-not-exists */
	if (GetKeyArgs(rest, KEYWORD_IF_DOES_NOT_EXIST, &pos))
		pos = T;
	e = (pos != Nil);

	/* :external-format */
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &z))
		z = Unbound;

	/* :type */
	Return(load_common_type_(ptr, rest, &type));

	*ret = 0;
	return load_common_switch_(ptr, file, type, x, y, e, z, ret);
}

