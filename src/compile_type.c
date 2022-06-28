#include "clos.h"
#include "closget_class.h"
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
static int faslwritetype_clos_(Execute ptr, addr stream, addr pos)
{
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_ASTERISK, &pos);
	}
	else {
		Return(stdget_class_name_(pos, &pos));
	}

	return faslwrite_value_symbol_(ptr, stream, pos);
}

static int faslreadtype_clos_(Execute ptr, addr stream, addr pos)
{
	addr value, check;

	Return(faslread_type_check_(stream, FaslCode_symbol));
	Return(faslread_value_symbol_(ptr, stream, &value));
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
static int faslwritetype_object_(Execute ptr, addr stream, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

static int faslreadtype_object_(Execute ptr, addr stream, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayType(pos, i, value);
	}

	return 0;
}


/*
 *  interface
 */
int faslwrite_value_type_(Execute ptr, addr stream, addr pos)
{
	size_t size;

	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType(pos, &size);
	Return(faslwrite_type_status_(stream, pos, FaslCode_type));
	Return(faslwrite_byte_(stream, (byte)LowLispDecl(pos)));
	Return(faslwrite_size_(stream, size));

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			return faslwritetype_clos_(ptr, stream, pos);

		default:
			return faslwritetype_object_(ptr, stream, pos, size);
	}
}

int faslread_value_type_(Execute ptr, addr stream, addr *ret)
{
	byte decl;
	FaslStatus status;
	addr pos;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &decl));
	Return(faslread_size_(stream, &size));
	type_heap(&pos, (enum LISPDECL)decl, size);

	switch (RefLispDecl(pos)) {
		case LISPDECL_CLOS:
			Return(faslreadtype_clos_(ptr, stream, pos));
			break;

		default:
			Return(faslreadtype_object_(ptr, stream, pos, size));
			break;
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

