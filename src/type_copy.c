#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "integer.h"
#include "sequence.h"
#include "type.h"
#include "type_copy.h"
#include "type_parse.h"

typedef void (*call_type_copy)(LocalRoot, addr *, addr);
static call_type_copy TypeCopyTable[LISPDECL_SIZE];

static void type_copy_error(LocalRoot local, addr *ret, addr type)
{
	infobit(type);
	Abort("Invalid type.");
}

static void getset_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetType(dst) != LISPTYPE_TYPE, "type left error");
	Check(GetType(src) != LISPTYPE_TYPE, "type right error");
	GetArrayType(src, index, &src);
	copylocal_object(local, &src, src);
	SetArrayType(dst, index, src);
}

static void getsettype_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetType(dst) != LISPTYPE_TYPE, "type left error");
	Check(GetType(src) != LISPTYPE_TYPE, "type right error");
	GetArrayType(src, index, &src);
	type_copy_alloc(local, &src, src);
	SetArrayType(dst, index, src);
}

static void getset_array4(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetStatusSize(dst) != LISPSIZE_ARRAY4, "size dst error");
	Check(GetStatusSize(src) != LISPSIZE_ARRAY4, "size src error");
	GetArrayA4(src, index, &src);
	copylocal_object(local, &src, src);
	SetArrayA4(dst, index, src);
}

static void getsettype_array4(LocalRoot local, addr dst, addr src, size_t index)
{
	Check(GetStatusSize(dst) != LISPSIZE_ARRAY4, "size dst error");
	Check(GetStatusSize(src) != LISPSIZE_ARRAY4, "size src error");
	GetArrayA4(src, index, &src);
	type_copy_alloc(local, &src, src);
	SetArrayA4(dst, index, src);
}

static void typecopy_empty(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;

	GetLispDecl(type, &decl);
	Check(lenarrayr(type) != 0, "length error");
	type0_alloc(local, decl, ret);
}

static void typecopy_allobject(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	LenArrayType(type, &size);
	type_alloc(local, &pos, decl, (byte)size);
	for (i = 0; i < size; i++)
		getset_arraytype(local, pos, type, i);
	*ret = pos;
}

static void typecopy_alltype(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	LenArrayType(type, &size);
	type_alloc(local, &pos, decl, (byte)size);
	for (i = 0; i < size; i++)
		getsettype_arraytype(local, pos, type, i);
	*ret = pos;
}

static void typecopy_vector_alltype(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr src, dst, pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	GetArrayType(type, 0, &src);
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++)
		getsettype_array4(local, dst, src, i);
	type_alloc(local, &pos, decl, 1);
	SetArrayType(pos, 0, dst);
	*ret = pos;
}

static void typecopy_clos(LocalRoot local, addr *ret, addr type)
{
	addr check;

	if (local == NULL && GetStatusDynamic(type)) {
		GetArrayType(type, 0, &check);
		if (GetStatusDynamic(check))
			Abort("dynamic scope error");
	}
	GetArrayType(type, 0, &type);
	type1_alloc(local, LISPDECL_CLOS, type, &type);
	*ret = type;
}

static void typecopy_eql(LocalRoot local, addr *ret, addr type)
{
	GetArrayType(type, 0, &type);
	copylocal_object(local, &type, type);
	type1_alloc(local, LISPDECL_EQL, type, &type);
	*ret = type;
}

static void typecopy_member(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr src, dst, pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	GetArrayType(type, 0, &src);
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++)
		getset_array4(local, dst, src, i);
	type_alloc(local, &pos, decl, 1);
	SetArrayType(pos, 0, dst);
	*ret = pos;
}


static void copylist_type(LocalRoot local, addr *ret, addr cons)
{
	addr root, child;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &child, &cons);
		type_copy_alloc(local, &child, child);
		cons_alloc(local, &root, child, root);
	}
	nreverse(ret, root);
}

static void copylist_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	GetArrayType(src, index, &src);
	copylist_type(local, &src, src);
	SetArrayType(dst, index, src);
}

static void typecopy_values(LocalRoot local, addr *ret, addr type)
{
	addr pos, check;

	type_alloc(local, &pos, LISPDECL_VALUES, 4);
	/* var */
	copylist_arraytype(local, pos, type, 0);
	/* opt */
	copylist_arraytype(local, pos, type, 1);
	/* rest */
	GetArrayType(type, 2, &check);
	if (check != Nil) {
		type_copy_alloc(local, &check, check);
		SetArrayType(pos, 2, check);
	}
	/* allow */
	getset_arraytype(local, pos, type, 3);
	*ret = pos;
}

static void typecopy_vector(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 2);
	/* type */
	getsettype_arraytype(local, pos, type, 0);
	/* dimension */
	GetArrayType(type, 1, &child);
	if (GetType(child) == LISPTYPE_FIXNUM)
		copylocal_object(local, &child, child);
	else
		type_copy_alloc(local, &child, child);
	SetArrayType(pos, 1, child);
	*ret = pos;
}

static void typecopy_size(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 1);

	GetArrayType(type, 0, &child);
	if (integerp(child))
		copylocal_object(local, &child, child);
	else
		type_copy_alloc(local, &child, child);
	SetArrayType(pos, 0, child);
	*ret = pos;
}

static void typecopy_function_key(LocalRoot local, addr *ret, addr list)
{
	addr root, name, type;

	if (list == T) {
		*ret = T;
		return;
	}
	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		GetCons(name, &name, &type);
		type_copy_alloc(local, &type, type);
		cons_alloc(local, &name, name, type);
		cons_alloc(local, &root, name, root);
	}
	nreverse(ret, root);
}

static void typecopy_function_arguments(LocalRoot local, addr *ret, addr src)
{
	addr dst, pos;

	if (type_asterisk_p(src)) {
		type_copy_alloc(local, ret, src);
		return;
	}
	vector2_alloc(local, &dst, 4);
	/* var */
	GetArrayA2(src, 0, &pos);
	copylist_type(local, &pos, pos);
	SetArrayA2(dst, 0, pos);
	/* opt */
	GetArrayA2(src, 1, &pos);
	copylist_type(local, &pos, pos);
	SetArrayA2(dst, 1, pos);
	/* rest */
	GetArrayA2(src, 2, &pos);
	if (pos != Nil)
		type_copy_alloc(local, &pos, pos);
	SetArrayA2(dst, 2, pos);
	/* key */
	GetArrayA2(src, 3, &pos);
	typecopy_function_key(local, &pos, pos);
	SetArrayA2(dst, 3, pos);
	*ret = dst;
}

static void typecopy_function(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 3);
	/* arguments */
	GetArrayType(type, 0, &child);
	typecopy_function_arguments(local, &child, child);
	SetArrayType(pos, 0, child);
	/* result */
	getsettype_arraytype(local, pos, type, 1);
	/* check */
	getset_arraytype(local, pos, type, 2);
	*ret = pos;
}

static void typecopy_array_dimension(LocalRoot local, addr *ret, addr src)
{
	addr dst, pos;
	size_t size, i;

	if (type_asterisk_p(src)) {
		type_copy_alloc(local, ret, src);
		return;
	}
	if (GetType(src) == LISPTYPE_FIXNUM) {
		copylocal_object(local, ret, src);
		return;
	}
	Check(GetType(src) != LISPTYPE_VECTOR, "type error");
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &pos);
		if (GetType(pos) == LISPTYPE_TYPE)
			type_copy_alloc(local, &pos, pos);
		else
			copylocal_object(local, &pos, pos);
		SetArrayA4(dst, i, pos);
	}
	*ret = dst;
}

static void typecopy_array(LocalRoot local, addr *ret, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 2);
	/* arguments */
	getsettype_arraytype(local, pos, type, 0);
	/* dimension */
	GetArrayType(type, 1, &child);
	typecopy_array_dimension(local, &child, child);
	SetArrayType(pos, 1, child);
	*ret = pos;
}

void init_type_copy(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeCopyTable[i] = type_copy_error;

	TypeCopyTable[LISPDECL_TYPE] = typecopy_empty;
	TypeCopyTable[LISPDECL_CLOS] = typecopy_clos;
	TypeCopyTable[LISPDECL_ASTERISK] = typecopy_allobject;
	TypeCopyTable[LISPDECL_OPTIMIZED] = typecopy_alltype;
	TypeCopyTable[LISPDECL_SUBTYPEP] = typecopy_alltype;
	/* Compound-type */
	TypeCopyTable[LISPDECL_AND] = typecopy_vector_alltype;
	TypeCopyTable[LISPDECL_OR] = typecopy_vector_alltype;
	TypeCopyTable[LISPDECL_EQL] = typecopy_eql;
	TypeCopyTable[LISPDECL_MEMBER] = typecopy_member;
	TypeCopyTable[LISPDECL_MOD] = typecopy_allobject;
	TypeCopyTable[LISPDECL_NOT] = typecopy_alltype;
	TypeCopyTable[LISPDECL_SATISFIES] = typecopy_allobject;
	TypeCopyTable[LISPDECL_VALUES] = typecopy_values;
	/* Extract-type */
	TypeCopyTable[LISPDECL_ATOM] = typecopy_empty;
	TypeCopyTable[LISPDECL_LIST] = typecopy_empty;
	TypeCopyTable[LISPDECL_BOOLEAN] = typecopy_empty;
	TypeCopyTable[LISPDECL_VECTOR] = typecopy_vector;
	TypeCopyTable[LISPDECL_SIMPLE_VECTOR] = typecopy_size;
	TypeCopyTable[LISPDECL_BIT_VECTOR] = typecopy_size;
	TypeCopyTable[LISPDECL_SIMPLE_BIT_VECTOR] = typecopy_size;
	TypeCopyTable[LISPDECL_EXTENDED_CHAR] = typecopy_empty;
	TypeCopyTable[LISPDECL_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_BASE_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_SIMPLE_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_SIMPLE_BASE_STRING] = typecopy_size;
	TypeCopyTable[LISPDECL_SIGNED_BYTE] = typecopy_size;
	TypeCopyTable[LISPDECL_UNSIGNED_BYTE] = typecopy_size;
	TypeCopyTable[LISPDECL_BIT] = typecopy_empty;
	TypeCopyTable[LISPDECL_FIXNUM] = typecopy_empty;
	TypeCopyTable[LISPDECL_BIGNUM] = typecopy_empty;
	/* Atomic-type */
	TypeCopyTable[LISPDECL_NIL] = typecopy_empty;
	TypeCopyTable[LISPDECL_T] = typecopy_empty;
	TypeCopyTable[LISPDECL_NULL] = typecopy_empty;
	TypeCopyTable[LISPDECL_CONS] = typecopy_alltype;
	TypeCopyTable[LISPDECL_HASH_TABLE] = typecopy_empty;
	TypeCopyTable[LISPDECL_SYMBOL] = typecopy_empty;
	TypeCopyTable[LISPDECL_KEYWORD] = typecopy_empty;
	TypeCopyTable[LISPDECL_PACKAGE] = typecopy_empty;
	TypeCopyTable[LISPDECL_RANDOM_STATE] = typecopy_empty;
	TypeCopyTable[LISPDECL_READTABLE] = typecopy_empty;
	TypeCopyTable[LISPDECL_FUNCTION] = typecopy_function;
	TypeCopyTable[LISPDECL_COMPILED_FUNCTION] = typecopy_function;
	TypeCopyTable[LISPDECL_PATHNAME] = typecopy_empty;
	TypeCopyTable[LISPDECL_LOGICAL_PATHNAME] = typecopy_empty;
	TypeCopyTable[LISPDECL_SEQUENCE] = typecopy_empty;
	TypeCopyTable[LISPDECL_ARRAY] = typecopy_array;
	TypeCopyTable[LISPDECL_SIMPLE_ARRAY] = typecopy_array;
	TypeCopyTable[LISPDECL_CHARACTER] = typecopy_empty;
	TypeCopyTable[LISPDECL_BASE_CHAR] = typecopy_empty;
	TypeCopyTable[LISPDECL_STANDARD_CHAR] = typecopy_empty;
	TypeCopyTable[LISPDECL_NUMBER] = typecopy_empty;
	TypeCopyTable[LISPDECL_REAL] = typecopy_allobject;
	TypeCopyTable[LISPDECL_RATIO] = typecopy_allobject;
	TypeCopyTable[LISPDECL_INTEGER] = typecopy_allobject;
	TypeCopyTable[LISPDECL_RATIONAL] = typecopy_allobject;
	TypeCopyTable[LISPDECL_COMPLEX] = typecopy_alltype;
	TypeCopyTable[LISPDECL_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_SHORT_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_SINGLE_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_DOUBLE_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_LONG_FLOAT] = typecopy_allobject;
	TypeCopyTable[LISPDECL_RESTART] = typecopy_empty;
	TypeCopyTable[LISPDECL_ENVIRONMENT] = typecopy_empty;
	TypeCopyTable[LISPDECL_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_BROADCAST_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_CONCATENATED_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_ECHO_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_FILE_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_STRING_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_SYNONYM_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_TWO_WAY_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_PROMPT_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_PRETTY_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_MEMORY_STREAM] = typecopy_empty;
	TypeCopyTable[LISPDECL_QUOTE] = typecopy_empty;
	TypeCopyTable[LISPDECL_BYTESPEC] = typecopy_empty;
	TypeCopyTable[LISPDECL_PRINT_DISPATCH] = typecopy_empty;
	TypeCopyTable[LISPDECL_EVAL] = typecopy_empty;
}


/*
 *  type-copy
 */
void type_copy_alloc(LocalRoot local, addr *ret, addr type)
{
	call_type_copy call;
	addr pos;

	CheckType(type, LISPTYPE_TYPE);
	call = TypeCopyTable[(int)RefLispDecl(type)];
	Check(call == NULL, "build error");
	call(local, &pos, type);
	type_setnotobject(pos, type);
	*ret = pos;
}
void type_copy_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	type_copy_alloc(local, ret, type);
}
void type_copy_heap(addr *ret, addr type)
{
	type_copy_alloc(NULL, ret, type);
}


/*
 *  type-throw
 */
void type_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (local)
		type_throw_local(local, pos, ret);
	else
		type_throw_heap(pos, ret);
}
void type_throw_local(LocalRoot local, addr pos, addr *ret)
{
	CheckLocal(local);
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		type_copy_local(local, ret, pos);
}
void type_throw_heap(addr pos, addr *ret)
{
	if (GetStatusDynamic(pos))
		type_copy_heap(ret, pos);
	else
		*ret = pos;
}

