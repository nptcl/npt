#include "compile_stream.h"
#include "condition.h"
#include "execute.h"
#include "typedef.h"

/*
 *  fixnum
 */
static int faslwrite_value_fixnum(Execute ptr, addr stream, addr pos)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	faslwrite_type(stream, FaslCode_fixnum);
	GetFixnum(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_fixnum_code(Execute ptr, addr stream, addr *ret)
{
	fixnum value;

	faslread_buffer(stream, &value, sizeoft(value));
	fixnum_heap(ret, value);

	return 0;
}


/*
 *  interface
 */
_g int faslwrite_value(Execute ptr, addr stream, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return faslwrite_value_fixnum(ptr, stream, pos);

		default:
			fmte("Invalid value ~S.", pos, NULL);
			return 0;
	}

	return 0;
}

