#include "compile_stream.h"
#include "compile_type.h"
#include "condition.h"
#include "execute.h"
#include "type.h"
#include "typedef.h"

/*
 *  error
 */
static int faslwritetype_error(Execute ptr, addr stream, addr pos)
{
	fmte("Invalid type.", NULL);
	return 0;
}

static int faslreadtype_error(Execute ptr, addr stream, addr pos)
{
	fmte("Invalid type.", NULL);
	return 0;
}


/*
 *  type1
 */
static int faslwritetype_type1(Execute ptr, addr stream, addr pos)
{
	GetArrayType(pos, 0, &pos);
	return faslwrite_value_type(ptr, stream, pos);
}

static int faslreadtype_type1(Execute ptr, addr stream, addr pos)
{
	enum FaslCode type;
	addr value;

	faslread_type(stream, &type);
	Check(type != FaslCode_type, "type error.");
	Return(faslread_value_type(ptr, stream, &value));
	SetArrayType(pos, 0, value);
	return 0;
}


/*
 *  interface
 */
typedef int (*faslwritetype_call)(Execute, addr, addr);
typedef int (*faslreadtype_call)(Execute, addr, addr);
static faslwritetype_call faslwritetype_table[LISPDECL_SIZE];
static faslreadtype_call faslreadtype_table[LISPDECL_SIZE];

_g int faslwrite_value_type(Execute ptr, addr stream, addr pos)
{
	enum LISPDECL decl, type;
	faslwritetype_call call;
	size_t size;

	CheckType(pos, LISPTYPE_TYPE);
	/* type */
	faslwrite_type(stream, FaslCode_type);
	/* decl */
	decl = LispDecl(pos);
	GetLispDecl(pos, &type);
	faslwrite_byte(stream, (byte)decl);
	/* size */
	LenArrayType(pos, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	/* switch */
	call = faslwritetype_table[type];
	Check(call == NULL, "faslwrite_value_type error.");
	return (*call)(ptr, stream, pos);
}

_g int faslread_value_type(Execute ptr, addr stream, addr *ret)
{
	enum LISPDECL type;
	byte decl;
	addr pos;
	faslreadtype_call call;
	size_t size;

	/* decl */
	faslread_byte(stream, &decl);
	/* size */
	faslread_buffer(stream, &size, IdxSize);
	/* switch */
	type_heap(&pos, (enum LISPDECL)decl, size);
	GetLispDecl(pos, &type);
	call = faslreadtype_table[type];
	Check(call == NULL, "faslread_value_type error.");
	Return((*call)(ptr, stream, pos));

	return Result(ret, pos);
}


/*
 *  initialize
 */
#define defwrite(x,y) faslwritetype_table[LISPDECL_##x] = faslwritetype_##y
#define defread(x,y) faslreadtype_table[LISPDECL_##x] = faslreadtype_##y
_g void init_compile_type(void)
{
	defwrite(EMPTY, error);
	defwrite(OPTIMIZED, type1);
	defwrite(SUBTYPEP, type1);
	//defwrite(CLOS, clos);
	//defwrite(ASTERISK, asterisk);

	defread(EMPTY, error);
	defread(OPTIMIZED, type1);
	defread(SUBTYPEP, type1);
	//defread(CLOS, clos);
	//defread(ASTERISK, asterisk);
}
#undef defwrite
#undef defread

