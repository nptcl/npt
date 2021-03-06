#include "clos.h"
#include "clos_class.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_type.h"
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
		Return(stdget_class_name_(pos, &pos));
	}

	return faslwrite_value_symbol(ptr, stream, pos);
}

static int faslreadtype_clos(Execute ptr, addr stream, addr pos)
{
	addr value, check;

	Return(faslread_type_check_(stream, FaslCode_symbol));
	Return(faslread_value_symbol(ptr, stream, &value));
	/* asterisk check */
	GetConst(COMMON_ASTERISK, &check);
	if (value != check) {
		Return(clos_find_class_(value, &value));
	}
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
int faslwrite_value_type(Execute ptr, addr stream, addr pos)
{
	size_t size;

	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType(pos, &size);
	Return(faslwrite_type_(stream, FaslCode_type));
	Return(faslwrite_byte_(stream, (byte)LowLispDecl(pos)));
	Return(faslwrite_buffer_(stream, &size, IdxSize));

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			return faslwritetype_clos(ptr, stream, pos);

		default:
			return faslwritetype_object(ptr, stream, pos, size);
	}
}

int faslread_value_type(Execute ptr, addr stream, addr *ret)
{
	byte decl;
	addr pos;
	size_t size;

	Return(faslread_byte_(stream, &decl));
	Return(faslread_buffer_(stream, &size, IdxSize));
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

