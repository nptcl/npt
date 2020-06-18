#include "clos.h"
#include "clos_class.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_value.h"
#include "compile_write.h"
#include "type.h"
#include "typedef.h"

/*
 *  clos
 */
static int faslwritetype_clos(Execute ptr, addr stream, addr pos)
{
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_ASTERISK, &pos);
	}
	else {
		stdget_class_name(pos, &pos);
	}

	return faslwrite_value_symbol(ptr, stream, pos);
}

static int faslreadtype_clos(Execute ptr, addr stream, addr pos)
{
	addr value, check;

	faslread_type_check(stream, FaslCode_symbol);
	Return(faslread_value_symbol(ptr, stream, &value));
	/* asterisk check */
	GetConst(COMMON_ASTERISK, &check);
	if (value != check)
		clos_find_class(value, &value);
	SetArrayType(pos, 0, value);

	return 0;
}


/*
 *  object
 */
static int faslwritetype_object(Execute ptr, addr stream, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &value);
		Return(faslwrite_value(ptr, stream, value));
	}

	return 0;
}

static int faslreadtype_object(Execute ptr, addr stream, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(faslread_value(ptr, stream, &value));
		SetArrayType(pos, i, value);
	}

	return 0;
}


/*
 *  interface
 */
_g int faslwrite_value_type(Execute ptr, addr stream, addr pos)
{
	size_t size;

	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType(pos, &size);
	faslwrite_type(stream, FaslCode_type);
	faslwrite_byte(stream, (byte)LispDecl(pos));
	faslwrite_buffer(stream, &size, IdxSize);

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			return faslwritetype_clos(ptr, stream, pos);

		default:
			return faslwritetype_object(ptr, stream, pos, size);
	}
}

_g int faslread_value_type(Execute ptr, addr stream, addr *ret)
{
	byte decl;
	addr pos;
	size_t size;

	faslread_byte(stream, &decl);
	faslread_buffer(stream, &size, IdxSize);
	type_heap(&pos, (enum LISPDECL)decl, size);

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			Return(faslreadtype_clos(ptr, stream, pos));
			break;

		default:
			Return(faslreadtype_object(ptr, stream, pos, size));
			break;
	}

	return Result(ret, pos);
}

