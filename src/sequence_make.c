#include "array_vector.h"
#include "bit.h"
#include "character.h"
#include "condition.h"
#include "cons_plist.h"
#include "hold.h"
#include "integer.h"
#include "local.h"
#include "sequence.h"
#include "sequence_make.h"
#include "strvect.h"
#include "type.h"
#include "type_parse.h"
#include "type_upgraded.h"
#include "typedef.h"

/*
 *  make-sequence
 */
static int list_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;
	addr root;
	size_t i;

	/* type check */
	if (value == Unbound)
		value = Nil;
	decl = LowLispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return Result(ret, Unbound);

	/* make-sequence */
	root = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&root, value, root);

	return Result(ret, root);
}

static int alloc_t_make_sequence_(addr *ret, size_t size, addr value)
{
	addr pos;
	size_t i;

	if (value == Unbound)
		value = Nil;
	vector_heap(&pos, size);
	for (i = 0; i < size; i++)
		setarray(pos, i, value);

	return Result(ret, pos);
}

static int alloc_bitvector_make_sequence_(addr *ret, size_t size, addr value)
{
	int bit;

	if (value == Unbound)
		bit = 0;
	else if (bit_getint(value, &bit))
		return TypeError_(value, BIT);
	bitmemory_unsafe(NULL, &value, size);
	bitmemory_memset(value, bit);

	return Result(ret, value);
}

static int alloc_string_make_sequence_(addr *ret, size_t size, addr value)
{
	unicode c;

	if (value == Unbound)
		c = 0;
	else if (characterp(value))
		GetCharacter(value, &c);
	else
		return TypeError_(value, CHARACTER);
	strvect_heap(&value, size);
	Return(strvect_setall_(value, c));

	return Result(ret, value);
}

static int vector_upgraded_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	Return(upgraded_array_value_(type, &upgraded, &upsize));
	switch (upgraded) {
		case ARRAY_TYPE_BIT:
			return alloc_bitvector_make_sequence_(ret, size, value);

		case ARRAY_TYPE_CHARACTER:
			return alloc_string_make_sequence_(ret, size, value);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return vector_signed_(ret, size, upgraded, upsize, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return vector_float_(ret, size, upgraded, value);

		default:
			return alloc_t_make_sequence_(ret, size, value);
	}
}

static int vector_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(ret, Unbound);
	/* vector size */
	Return(vector_check_sequence_(type, size));
	/* make-sequence */
	return vector_upgraded_make_sequence_(ret, type, size, value);
}

static int simple_vector_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(ret, Unbound);
	Return(simple_vector_check_sequence_(type, size));
	return alloc_t_make_sequence_(ret, size, value);
}

static int string_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	if (! type_string_p(type))
		return Result(ret, Unbound);
	Return(simple_vector_check_sequence_(type, size));
	return alloc_string_make_sequence_(ret, size, value);
}

static int array_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(ret, Unbound);

	/* dimension check */
	Return(array_check_sequence_(type, size));
	/* make-sequence */
	return vector_upgraded_make_sequence_(ret, type, size, value);
}

static int bitvector_make_sequence_(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(ret, Unbound);

	/* make-sequence */
	Return(simple_vector_check_sequence_(type, size));
	return alloc_bitvector_make_sequence_(ret, size, value);
}

static int sequence_make_sequence_(Execute ptr,
		addr *ret, addr type, size_t size, addr value)
{
	addr pos;

	/* list */
	Return(list_make_sequence_(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* vector */
	Return(vector_make_sequence_(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* simple-vector */
	Return(simple_vector_make_sequence_(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* string */
	Return(string_make_sequence_(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* array */
	Return(array_make_sequence_(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* bitvector */
	Return(bitvector_make_sequence_(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int make_sequence_common_(Execute ptr, addr *ret, addr type, addr size, addr rest)
{
	addr check, element;
	size_t index;
	LocalHold hold;

	if (GetKeyArgs(rest, KEYWORD_INITIAL_ELEMENT, &element))
		element = Unbound;
	if (GetIndex_integer(size, &index))
		return fmte_("Too large index ~S.", size, NULL);

	Return(parse_type_(ptr, &check, type, Nil));
	hold = LocalHold_local_push(ptr, check);

	Return(sequence_make_sequence_(ptr, &rest, check, index, element));
	localhold_push(hold, rest);

	Return(call_typep_asterisk_error_(ptr, rest, check));
	localhold_end(hold);

	return Result(ret, rest);
}

