#include "call_types.h"
#include "execute.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_typep.h"
#include "type_value.h"
#include "typedef.h"

/* type-of */
int type_of_common_(addr pos, addr *ret)
{
	Return(type_value_(&pos, pos));
	Return(type_object_(ret, pos));
	return 0;
}


/* typep */
int typep_common_(Execute ptr, addr x, addr y, addr env, addr *ret)
{
	int check;

	if (env == Unbound)
		env = Nil;
	Return(parse_type(ptr, &y, y, env));
	Return(typep_clang_(ptr, x, y, &check));
	*ret = check? T: Nil;

	return 0;
}

