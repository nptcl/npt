#include "condition.h"
#include "cons.h"
#include "copy.h"
#include "integer.h"
#include "sequence.h"
#include "type_copy.h"
#include "type_parse.h"

typedef addr (*call_type_copy)(LocalRoot, addr);
static call_type_copy TypeCopy[LISPDECL_SIZE];

static addr type_copy_error(LocalRoot local, addr type)
{
	fmte("Invalid type ~S.", type, NULL);
	return NULL;
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
	src = type_copy_allocr(local, src);
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
	src = type_copy_allocr(local, src);
	SetArrayA4(dst, index, src);
}

static addr copy_allobject(LocalRoot local, addr type)
{
	enum LISPDECL decl;
	addr pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	LenArrayType(type, &size);
	type_alloc(local, &pos, decl, (byte)size);
	for (i = 0; i < size; i++)
		getset_arraytype(local, pos, type, i);

	return pos;
}

static addr copy_alltype(LocalRoot local, addr type)
{
	enum LISPDECL decl;
	addr pos;
	size_t size, i;

	GetLispDecl(type, &decl);
	LenArrayType(type, &size);
	type_alloc(local, &pos, decl, (byte)size);
	for (i = 0; i < size; i++)
		getsettype_arraytype(local, pos, type, i);

	return pos;
}

static addr copy_vector_alltype(LocalRoot local, addr type)
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

	return pos;
}

static addr copy_clos(LocalRoot local, addr type)
{
	addr check;

	if (local == NULL && GetStatusDynamic(type)) {
		GetArrayType(type, 0, &check);
		if (GetStatusDynamic(check))
			fmte("dynamic scope error", NULL);
	}
	GetArrayType(type, 0, &type);
	type_object1(local, LISPDECL_CLOS, type, &type);
	return type;
}

static addr copy_eql(LocalRoot local, addr type)
{
	GetArrayType(type, 0, &type);
	copylocal_object(local, &type, type);
	type_object1(local, LISPDECL_EQL, type, &type);
	return type;
}

static addr copy_member(LocalRoot local, addr type)
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

	return pos;
}


static addr copylist_type(LocalRoot local, addr cons)
{
	addr root, child;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &child, &cons);
		child = type_copy_allocr(local, child);
		cons_alloc(local, &root, child, root);
	}
	nreverse_list_unsafe(&root, root);

	return root;
}

static void copylist_arraytype(LocalRoot local, addr dst, addr src, size_t index)
{
	GetArrayType(src, index, &src);
	src = copylist_type(local, src);
	SetArrayType(dst, index, src);
}

static addr copy_values(LocalRoot local, addr type)
{
	addr pos, check;

	type_alloc(local, &pos, LISPDECL_VALUES, 4);
	/* var */
	copylist_arraytype(local, pos, type, 0);
	/* opt */
	copylist_arraytype(local, pos, type, 1);
	/* rest */
	GetArrayType(type, 2, &check);
	if (check != Nil)
		SetArrayType(pos, 2, type_copy_allocr(local, check));
	/* allow */
	getset_arraytype(local, pos, type, 3);

	return pos;
}

static addr copy_vector(LocalRoot local, addr type)
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
		child = type_copy_allocr(local, child);
	SetArrayType(pos, 1, child);

	return pos;
}

static addr copy_size(LocalRoot local, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 1);

	GetArrayType(type, 0, &child);
	if (integerp(child))
		copylocal_object(local, &child, child);
	else
		child = type_copy_allocr(local, child);
	SetArrayType(pos, 0, child);

	return pos;
}

static addr copy_function_key(LocalRoot local, addr list)
{
	addr root, name, type;

	if (list == T) return T;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		GetCons(name, &name, &type);
		type = type_copy_allocr(local, type);
		cons_alloc(local, &name, name, type);
		cons_alloc(local, &root, name, root);
	}
	nreverse_list_unsafe(&root, root);

	return root;
}

static addr copy_function_arguments(LocalRoot local, addr src)
{
	addr dst, pos;

	if (asterisk_p(src))
		return type_copy_allocr(local, src);
	vector2_alloc(local, &dst, 4);
	/* var */
	GetArrayA2(src, 0, &pos);
	pos = copylist_type(local, pos);
	SetArrayA2(dst, 0, pos);
	/* opt */
	GetArrayA2(src, 1, &pos);
	pos = copylist_type(local, pos);
	SetArrayA2(dst, 1, pos);
	/* rest */
	GetArrayA2(src, 2, &pos);
	if (pos != Nil)
		pos = type_copy_allocr(local, pos);
	SetArrayA2(dst, 2, pos);
	/* key */
	GetArrayA2(src, 3, &pos);
	pos = copy_function_key(local, pos);
	SetArrayA2(dst, 3, pos);

	return dst;
}

static addr copy_function(LocalRoot local, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 3);
	/* arguments */
	GetArrayType(type, 0, &child);
	child = copy_function_arguments(local, child);
	SetArrayType(pos, 0, child);
	/* result */
	getsettype_arraytype(local, pos, type, 1);
	/* check */
	getset_arraytype(local, pos, type, 2);

	return pos;
}

static addr copy_array_dimension(LocalRoot local, addr src)
{
	addr dst, pos;
	size_t size, i;

	if (asterisk_p(src))
		return type_copy_allocr(local, src);
	if (GetType(src) == LISPTYPE_FIXNUM) {
		copylocal_object(local, &src, src);
		return src;
	}
	Check(GetType(src) != LISPTYPE_VECTOR, "type error");
	LenArrayA4(src, &size);
	vector4_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &pos);
		if (GetType(pos) == LISPTYPE_TYPE)
			pos = type_copy_allocr(local, pos);
		else
			copylocal_object(local, &pos, pos);
		SetArrayA4(dst, i, pos);
	}

	return dst;
}

static addr copy_array(LocalRoot local, addr type)
{
	enum LISPDECL decl;
	addr pos, child;

	GetLispDecl(type, &decl);
	type_alloc(local, &pos, decl, 2);
	/* arguments */
	getsettype_arraytype(local, pos, type, 0);
	/* dimension */
	GetArrayType(type, 1, &child);
	child = copy_array_dimension(local, child);
	SetArrayType(pos, 1, child);

	return pos;
}

void init_type_copy(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeCopy[i] = type_copy_error;

	TypeCopy[LISPDECL_TYPE] = copy_allobject;
	TypeCopy[LISPDECL_CLOS] = copy_clos;
	TypeCopy[LISPDECL_ASTERISK] = copy_allobject;
	TypeCopy[LISPDECL_OPTIMIZED] = copy_alltype;
	TypeCopy[LISPDECL_SUBTYPEP] = copy_alltype;
	/* Compound-type */
	TypeCopy[LISPDECL_AND] = copy_vector_alltype;
	TypeCopy[LISPDECL_OR] = copy_vector_alltype;
	TypeCopy[LISPDECL_EQL] = copy_eql;
	TypeCopy[LISPDECL_MEMBER] = copy_member;
	TypeCopy[LISPDECL_MOD] = copy_allobject;
	TypeCopy[LISPDECL_NOT] = copy_alltype;
	TypeCopy[LISPDECL_SATISFIES] = copy_allobject;
	TypeCopy[LISPDECL_VALUES] = copy_values;
	/* Extract-type */
	TypeCopy[LISPDECL_ATOM] = copy_allobject;
	TypeCopy[LISPDECL_LIST] = copy_allobject;
	TypeCopy[LISPDECL_BOOLEAN] = copy_allobject;
	TypeCopy[LISPDECL_VECTOR] = copy_vector;
	TypeCopy[LISPDECL_SIMPLE_VECTOR] = copy_size;
	TypeCopy[LISPDECL_BIT_VECTOR] = copy_size;
	TypeCopy[LISPDECL_SIMPLE_BIT_VECTOR] = copy_size;
	TypeCopy[LISPDECL_EXTENDED_CHAR] = copy_allobject;
	TypeCopy[LISPDECL_STRING] = copy_size;
	TypeCopy[LISPDECL_BASE_STRING] = copy_size;
	TypeCopy[LISPDECL_SIMPLE_STRING] = copy_size;
	TypeCopy[LISPDECL_SIMPLE_BASE_STRING] = copy_size;
	TypeCopy[LISPDECL_SIGNED_BYTE] = copy_size;
	TypeCopy[LISPDECL_UNSIGNED_BYTE] = copy_size;
	TypeCopy[LISPDECL_BIT] = copy_allobject;
	TypeCopy[LISPDECL_FIXNUM] = copy_allobject;
	TypeCopy[LISPDECL_BIGNUM] = copy_allobject;
	/* Atomic-type */
	TypeCopy[LISPDECL_NIL] = copy_allobject;
	TypeCopy[LISPDECL_T] = copy_allobject;
	TypeCopy[LISPDECL_NULL] = copy_allobject;
	TypeCopy[LISPDECL_CONS] = copy_alltype;
	TypeCopy[LISPDECL_HASH_TABLE] = copy_allobject;
	TypeCopy[LISPDECL_SYMBOL] = copy_allobject;
	TypeCopy[LISPDECL_KEYWORD] = copy_allobject;
	TypeCopy[LISPDECL_PACKAGE] = copy_allobject;
	TypeCopy[LISPDECL_RANDOM_STATE] = copy_allobject;
	TypeCopy[LISPDECL_READTABLE] = copy_allobject;
	TypeCopy[LISPDECL_FUNCTION] = copy_function;
	TypeCopy[LISPDECL_COMPILED_FUNCTION] = copy_function;
	TypeCopy[LISPDECL_PATHNAME] = copy_allobject;
	TypeCopy[LISPDECL_LOGICAL_PATHNAME] = copy_allobject;
	TypeCopy[LISPDECL_SEQUENCE] = copy_allobject;
	TypeCopy[LISPDECL_ARRAY] = copy_array;
	TypeCopy[LISPDECL_SIMPLE_ARRAY] = copy_array;
	TypeCopy[LISPDECL_CHARACTER] = copy_allobject;
	TypeCopy[LISPDECL_BASE_CHAR] = copy_allobject;
	TypeCopy[LISPDECL_STANDARD_CHAR] = copy_allobject;
	TypeCopy[LISPDECL_NUMBER] = copy_allobject;
	TypeCopy[LISPDECL_REAL] = copy_allobject;
	TypeCopy[LISPDECL_RATIO] = copy_allobject;
	TypeCopy[LISPDECL_INTEGER] = copy_allobject;
	TypeCopy[LISPDECL_RATIONAL] = copy_allobject;
	TypeCopy[LISPDECL_COMPLEX] = copy_alltype;
	TypeCopy[LISPDECL_FLOAT] = copy_allobject;
	TypeCopy[LISPDECL_SHORT_FLOAT] = copy_allobject;
	TypeCopy[LISPDECL_SINGLE_FLOAT] = copy_allobject;
	TypeCopy[LISPDECL_DOUBLE_FLOAT] = copy_allobject;
	TypeCopy[LISPDECL_LONG_FLOAT] = copy_allobject;
	TypeCopy[LISPDECL_RESTART] = copy_allobject;
	TypeCopy[LISPDECL_ENVIRONMENT] = copy_allobject;
	TypeCopy[LISPDECL_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_BROADCAST_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_CONCATENATED_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_ECHO_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_FILE_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_STRING_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_SYNONYM_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_TWO_WAY_STREAM] = copy_allobject;
	TypeCopy[LISPDECL_BYTESPEC] = copy_allobject;
}

addr type_copy_allocr(LocalRoot local, addr type)
{
	call_type_copy call;
	addr pos;

	Check(GetType(type) != LISPTYPE_TYPE, "type error");
	call = TypeCopy[(int)RefLispDecl(type)];
	Check(call == NULL, "build error");
	pos = call(local, type);
	setnotdecl_object(pos, type);

	return pos;
}
addr type_copy_localr(LocalRoot local, addr type)
{
	Check(local == NULL, "local error");
	return type_copy_allocr(local, type);
}
addr type_copy_heapr(addr type)
{
	return type_copy_allocr(NULL, type);
}
void type_copy_alloc(LocalRoot local, addr *ret, addr type)
{
	*ret = type_copy_allocr(local, type);
}
void type_copy_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	*ret = type_copy_allocr(local, type);
}
void type_copy_heap(addr *ret, addr type)
{
	*ret = type_copy_allocr(NULL, type);
}

