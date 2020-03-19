#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "bignum.h"
#include "bit.h"
#include "build.h"
#include "cmpl.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "equal.h"
#include "hashtable.h"
#include "memory.h"
#include "number.h"
#include "object.h"
#include "pathname.h"
#include "ratio.h"
#include "strtype.h"
#include "strvect.h"
#include "structure.h"

_g int atom_function(addr pos)
{
	Check(pos == Unbound, "Unbound-variable");
	return GetType(pos) != LISPTYPE_CONS;
}

_g int eq_function(addr a, addr b)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");
	return a == b;
}


/*
 *  eql
 */
static int eql_function_fixnum(addr a, enum LISPTYPE type2, addr b)
{
	switch (type2) {
		case LISPTYPE_FIXNUM:
			return RefFixnum(a) == RefFixnum(b);

		case LISPTYPE_BIGNUM:
			return equal_fb_real(a, b);

		case LISPTYPE_RATIO:
			return equal_fr_real(a, b);

		default:
			return 0;
	}
}

static int eql_function_bignum(addr a, enum LISPTYPE type2, addr b)
{
	switch (type2) {
		case LISPTYPE_FIXNUM:
			return equal_bf_real(a, b);

		case LISPTYPE_BIGNUM:
			return equal_bb_real(a, b);

		case LISPTYPE_RATIO:
			return equal_br_real(a, b);

		default:
			return 0;
	}
}

static int eql_function_ratio(addr a, enum LISPTYPE type2, addr b)
{
	switch (type2) {
		case LISPTYPE_FIXNUM:
			return equal_rf_real(a, b);

		case LISPTYPE_BIGNUM:
			return equal_rb_real(a, b);

		case LISPTYPE_RATIO:
			return equal_rr_real(a, b);

		default:
			return 0;
	}
}

_g int eql_function(addr a, addr b)
{
	enum LISPTYPE type1, type2;

	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eq */
	if (a == b)
		return 1;

	/* eql */
	type1 = GetType(a);
	type2 = GetType(b);
	switch (type1) {
		case LISPTYPE_FIXNUM:
			return eql_function_fixnum(a, type2, b);

		case LISPTYPE_BIGNUM:
			return eql_function_bignum(a, type2, b);

		case LISPTYPE_RATIO:
			return eql_function_ratio(a, type2, b);

		default:
			break;
	}

	if (type1 != type2)
		return 0;
	switch (type1) {
		case LISPTYPE_CHARACTER:
			return character_equal(a, b);

		case LISPTYPE_SINGLE_FLOAT:
			return RefSingleFloat(a) == RefSingleFloat(b);

		case LISPTYPE_DOUBLE_FLOAT:
			return RefDoubleFloat(a) == RefDoubleFloat(b);

		case LISPTYPE_LONG_FLOAT:
			return RefLongFloat(a) == RefLongFloat(b);

		case LISPTYPE_COMPLEX:
			return eql_complex(a, b);

		default:
			return 0;
	}
}


/*
 *  equal
 */
static int equal_function_cons(addr a, addr b)
{
	addr car1, car2, cdr1, cdr2;

	if (! consp(b)) return 0;
	GetCons(a, &car1, &cdr1);
	GetCons(b, &car2, &cdr2);
	return equal_function(car1, car2)
		&& equal_function(cdr1, cdr2);
}

static int equal_function_string(addr a, addr b)
{
	if (stringp(b))
		return string_equal(a, b);
	else
		return 0;
}

static int equal_function_bitvector(addr a, addr b)
{
	if (bitvectorp(b))
		return bitvector_equal(a, b);
	else
		return 0;
}

static int equal_function_array(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_STRING:
			return equal_function_string(b, a);

		case LISPTYPE_BITVECTOR:
			return equal_function_bitvector(b, a);

		default:
			return 0;
	}
}

static int equal_function_pathname(addr a, addr b)
{
	return pathnamep(a) && pathnamep(b) && pathname_equal(a, b);
}

_g int equal_function(addr a, addr b)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eql */
	if (eql_function(a, b))
		return 1;

	/* equal */
	switch (GetType(a)) {
		case LISPTYPE_CONS:
			return equal_function_cons(a, b);

		case LISPTYPE_STRING:
			return equal_function_string(a, b);

		case LISPTYPE_BITVECTOR:
			return equal_function_bitvector(a, b);

		case LISPTYPE_ARRAY:
			return equal_function_array(a, b);

		case LISPTYPE_PATHNAME:
			return equal_function_pathname(a, b);

		default:
			return 0;
	}
}


/*
 *  equalp
 */
static int equalp_function_number(addr a, addr b)
{
	if (numberp(b))
		return equal_number(Local_Thread, a, b);
	else
		return 0;
}

static int equalp_function_character(addr a, addr b)
{
	if (characterp(b))
		return character_equalp(a, b);
	else
		return 0;
}

static int equalp_function_cons(addr a, addr b)
{
	addr car1, car2, cdr1, cdr2;

	if (! consp(b)) return 0;
	GetCons(a, &car1, &cdr1);
	GetCons(b, &car2, &cdr2);
	return equalp_function(car1, car2)
		&& equalp_function(cdr1, cdr2);
}

static int equalp_function_aa(addr a, addr b)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* rank, dimension */
	if (! array_equal_dimension(a, b))
		return 0;
	/* fill-pointer */
	array_get_rowlength(a, &size);
	array_get_rowlength(b, &i);
	if (size != i)
		return 0;
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		array_get(local, a, i, &c);
		array_get(local, b, i, &d);
		check = equalp_function(c, d);
		rollback_local(local, stack);
		if (! check)
			return 0;
	}

	return 1;
}

static int equalp_function_av(addr a, addr b)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* size check */
	if (! array_vector_p(a))
		return 0;
	array_get_rowlength(a, &size);
	lenarray(b, &i);
	if (size != i)
		return 0;
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		array_get(local, a, i, &c);
		getarray(b, i, &d);
		check = equalp_function(c, d);
		rollback_local(local, stack);
		if (! check)
			return 0;
	}

	return 1;
}

static int equalp_function_as(addr a, addr b)
{
	unicode c, d;
	size_t size, i;

	/* string */
	if (array_stringp(a))
		return string_equalp(a, b);
	/* size check */
	if (! array_vector_p(a))
		return 0;
	array_get_rowlength(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return 0;
	/* body */
	for (i = 0; i < size; i++) {
		if (array_type(a) != ARRAY_TYPE_CHARACTER)
			return 0;
		array_get_unicode(a, i, &c);
		strvect_getc(b, i, &d);
		if (toUpperUnicode(c) != toUpperUnicode(d))
			return 0;
	}

	return 1;
}

static int equalp_function_ab(addr a, addr b)
{
	int c, d;
	size_t size, i;

	/* string */
	if (array_bvarrayp(a))
		return bitvector_equal(a, b);
	/* size check */
	if (! array_vector_p(a))
		return 0;
	array_get_rowlength(a, &size);
	bitmemory_length(b, &i);
	if (size != i)
		return 0;
	/* body */
	for (i = 0; i < size; i++) {
		if (array_type(a) != ARRAY_TYPE_BIT)
			return 0;
		array_get_bit(a, i, &c);
		bitmemory_getint(b, i, &d);
		if (c != d)
			return 0;
	}

	return 1;
}

static int equalp_function_array(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_ARRAY:
			return equalp_function_aa(b, a);

		case LISPTYPE_VECTOR:
			return equalp_function_av(a, b);

		case LISPTYPE_STRING:
			return equalp_function_as(a, b);

		case LISPTYPE_BITVECTOR:
			return equalp_function_ab(a, b);

		default:
			return 0;
	}
}

static int equalp_function_vv(addr a, addr b)
{
	addr c, d;
	size_t size, i;

	lenarray(a, &size);
	lenarray(b, &i);
	if (size != i)
		return 0;
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		getarray(b, i, &d);
		if (! equalp_function(c, d))
			return 0;
	}

	return 1;
}

static int equalp_function_vs(addr a, addr b)
{
	addr c;
	unicode d, e;
	size_t size, i;

	lenarray(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return 0;
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		if (! characterp(c))
			return 0;
		GetCharacter(c, &d);
		strvect_getc(b, i, &e);
		if (toUpperUnicode(d) != toUpperUnicode(e))
			return 0;
	}

	return 1;
}

static int equalp_function_vb(addr a, addr b)
{
	int d, e;
	addr c;
	size_t size, i;

	lenarray(a, &size);
	bitmemory_length(b, &i);
	if (size != i)
		return 0;
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		if (bit_getint(c, &d))
			return 0;
		bitmemory_getint(b, i, &e);
		if (d != e)
			return 0;
	}

	return 1;
}

static int equalp_function_vector(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_VECTOR:
			return equalp_function_vv(a, b);

		case LISPTYPE_ARRAY:
			return equalp_function_av(b, a);

		case LISPTYPE_STRING:
			return equalp_function_vs(a, b);

		case LISPTYPE_BITVECTOR:
			return equalp_function_vb(a, b);

		default:
			return 0;
	}
}

static int equalp_function_string(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_STRING:
			return strvect_equalp(a, b);

		case LISPTYPE_VECTOR:
			return equalp_function_vs(b, a);

		case LISPTYPE_ARRAY:
			return equalp_function_as(b, a);

		default:
			return 0;
	}
}

static int equalp_function_bitvector(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_BITVECTOR:
			return bitmemory_equal(a, b);

		case LISPTYPE_VECTOR:
			return equalp_function_vb(b, a);

		case LISPTYPE_ARRAY:
			return equalp_function_ab(b, a);

		default:
			return 0;
	}
}

static int equalp_function_hashtable(addr a, addr b)
{
	if (hashtablep(b))
		return equalp_hashtable(a, b);
	else
		return 0;
}

static int equalp_function_structure(addr a, addr b)
{
	if (structure_instance_p(b))
		return equalp_structure(a, b);
	else
		return 0;
}

_g int equalp_function(addr a, addr b)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eq */
	if (a == b)
		return 1;

	/* number */
	if (numberp(a))
		return equalp_function_number(a, b);

	/* equalp */
	switch (GetType(a)) {
		case LISPTYPE_CHARACTER:
			return equalp_function_character(a, b);

		case LISPTYPE_CONS:
			return equalp_function_cons(a, b);

		case LISPTYPE_ARRAY:
			return equalp_function_array(a, b);

		case LISPTYPE_VECTOR:
			return equalp_function_vector(a, b);

		case LISPTYPE_STRING:
			return equalp_function_string(a, b);

		case LISPTYPE_BITVECTOR:
			return equalp_function_bitvector(a, b);

		case LISPTYPE_PATHNAME:
			return equal_function_pathname(a, b);

		case LISPTYPE_HASHTABLE:
			return equalp_function_hashtable(a, b);

		case LISPTYPE_CLOS:
			return equalp_function_structure(a, b);

		default:
			return 0;
	}
}


/*
 *  equal for rt
 */
static int equalrt_function_character(addr a, addr b)
{
	if (characterp(b))
		return character_equal(a, b);
	else
		return 0;
}

static int equalrt_function_cons(addr a, addr b)
{
	addr car1, car2, cdr1, cdr2;

	if (! consp(b)) return 0;
	GetCons(a, &car1, &cdr1);
	GetCons(b, &car2, &cdr2);
	return equalrt_function(car1, car2)
		&& equalrt_function(cdr1, cdr2);
}

static int equalrt_function_aa(addr a, addr b)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* rank, dimension */
	if (! array_equal_dimension(a, b))
		return 0;
	/* fill-pointer */
	array_get_rowlength(a, &size);
	array_get_rowlength(b, &i);
	if (size != i)
		return 0;
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		array_get(local, a, i, &c);
		array_get(local, b, i, &d);
		check = equalrt_function(c, d);
		rollback_local(local, stack);
		if (! check)
			return 0;
	}

	return 1;
}

static int equalrt_function_av(addr a, addr b)
{
	int check;
	addr c, d;
	LocalRoot local;
	LocalStack stack;
	size_t size, i;

	/* size check */
	if (! array_vector_p(a))
		return 0;
	array_get_rowlength(a, &size);
	lenarray(b, &i);
	if (size != i)
		return 0;
	/* body */
	local = Local_Thread;
	for (i = 0; i < size; i++) {
		push_local(local, &stack);
		array_get(local, a, i, &c);
		getarray(b, i, &d);
		check = equalrt_function(c, d);
		rollback_local(local, stack);
		if (! check)
			return 0;
	}

	return 1;
}

static int equalrt_function_as(addr a, addr b)
{
	unicode c, d;
	size_t size, i;

	/* string */
	if (array_stringp(a))
		return string_equalp(a, b);
	/* size check */
	if (! array_vector_p(a))
		return 0;
	array_get_rowlength(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return 0;
	/* body */
	for (i = 0; i < size; i++) {
		if (array_type(a) != ARRAY_TYPE_CHARACTER)
			return 0;
		array_get_unicode(a, i, &c);
		strvect_getc(b, i, &d);
		if (c != d)
			return 0;
	}

	return 1;
}

static int equalrt_function_array(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_ARRAY:
			return equalrt_function_aa(b, a);

		case LISPTYPE_VECTOR:
			return equalrt_function_av(a, b);

		case LISPTYPE_STRING:
			return equalrt_function_as(a, b);

		case LISPTYPE_BITVECTOR:
			return equalp_function_ab(a, b);

		default:
			return 0;
	}
}

static int equalrt_function_vv(addr a, addr b)
{
	addr c, d;
	size_t size, i;

	lenarray(a, &size);
	lenarray(b, &i);
	if (size != i)
		return 0;
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		getarray(b, i, &d);
		if (! equalrt_function(c, d))
			return 0;
	}

	return 1;
}

static int equalrt_function_vs(addr a, addr b)
{
	addr c;
	unicode d, e;
	size_t size, i;

	lenarray(a, &size);
	strvect_length(b, &i);
	if (size != i)
		return 0;
	for (i = 0; i < size; i++) {
		getarray(a, i, &c);
		if (! characterp(c))
			return 0;
		GetCharacter(c, &d);
		strvect_getc(b, i, &e);
		if (d != e)
			return 0;
	}

	return 1;
}

static int equalrt_function_vector(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_VECTOR:
			return equalrt_function_vv(a, b);

		case LISPTYPE_STRING:
			return equalrt_function_vs(a, b);

		case LISPTYPE_ARRAY:
			return equalrt_function_av(b, a);

		case LISPTYPE_BITVECTOR:
			return equalp_function_vb(a, b);

		default:
			return 0;
	}
}

static int equalrt_function_string(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_STRING:
			return strvect_equal(a, b);

		case LISPTYPE_VECTOR:
			return equalrt_function_vs(b, a);

		case LISPTYPE_ARRAY:
			return equalrt_function_as(b, a);

		default:
			return 0;
	}
}

static int equalrt_function_bitvector(addr a, addr b)
{
	switch (GetType(b)) {
		case LISPTYPE_BITVECTOR:
			return bitmemory_equal(a, b);

		case LISPTYPE_VECTOR:
			return equalp_function_vb(b, a);

		case LISPTYPE_ARRAY:
			return equalp_function_ab(b, a);

		default:
			return 0;
	}
}

static int equalrt_function_hashtable(addr a, addr b)
{
	if (hashtablep(b))
		return equalrt_hashtable(a, b);
	else
		return 0;
}

static int equalrt_function_structure(addr a, addr b)
{
	if (structure_instance_p(b))
		return equalrt_structure(a, b);
	else
		return 0;
}

_g int equalrt_function(addr a, addr b)
{
	Check(a == Unbound, "Unbound-variable");
	Check(b == Unbound, "Unbound-variable");

	/* eql */
	if (eql_function(a, b))
		return 1;

	/* equalrt */
	switch (GetType(a)) {
		case LISPTYPE_CHARACTER:
			return equalrt_function_character(a, b);

		case LISPTYPE_CONS:
			return equalrt_function_cons(a, b);

		case LISPTYPE_ARRAY:
			return equalrt_function_array(a, b);

		case LISPTYPE_VECTOR:
			return equalrt_function_vector(a, b);

		case LISPTYPE_STRING:
			return equalrt_function_string(a, b);

		case LISPTYPE_BITVECTOR:
			return equalrt_function_bitvector(a, b);

		case LISPTYPE_PATHNAME:
			return equal_function_pathname(a, b);

		case LISPTYPE_HASHTABLE:
			return equalrt_function_hashtable(a, b);

		case LISPTYPE_CLOS:
			return equalrt_function_structure(a, b);

		default:
			return 0;
	}
}

