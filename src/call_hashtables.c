#include "call_hashtables.h"
#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "real.h"
#include "sxhash.h"
#include "symbol.h"

/*
 *  make-hash-table
 */
static int make_hash_table_symbol_common(addr pos, enum HASHTABLE_TEST *ret)
{
	addr check;

	GetConst(COMMON_EQ, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQ;
		return 0;
	}
	GetConst(COMMON_EQL, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQL;
		return 0;
	}
	GetConst(COMMON_EQUAL, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQUAL;
		return 0;
	}
	GetConst(COMMON_EQUALP, &check);
	if (pos == check) {
		*ret = HASHTABLE_TEST_EQUALP;
		return 0;
	}

	return 1;
}

static int make_hash_table_test_common_(addr rest, enum HASHTABLE_TEST *ret)
{
	addr pos, check;

	if (GetKeyArgs(rest, KEYWORD_TEST, &pos)) {
		*ret = HASHTABLE_TEST_EQL;
		return 0;
	}
	if (symbolp(pos)) {
		if (make_hash_table_symbol_common(pos, ret))
			goto error;
		return 0;
	}
	if (functionp(pos)) {
		GetNameFunction(pos, &check);
		if (check == Nil)
			goto error;
		GetCallName(check, &check);
		if (make_hash_table_symbol_common(check, ret))
			goto error;
		return 0;
	}
error:
	*ret = HASHTABLE_TEST_EQL;
	return fmte_("Invalid hash-hable-test ~S.", pos, NULL);
}

static int make_hash_table_size_common_(addr rest, size_t *ret)
{
	addr pos;

	if (GetKeyArgs(rest, KEYWORD_SIZE, &pos)) {
		*ret = HASHTABLE_SIZE_DEFAULT;
		return 0;
	}
	if (! integerp(pos)) {
		*ret = 0;
		return TypeError_(pos, INTEGER);
	}
	if (GetIndex_integer(pos, ret)) {
		*ret = 0;
		return fmte_("Invalid hash size ~S.", pos, NULL);
	}

	return 0;
}

static int make_hash_table_rehash_size_common_(addr rest,
		int *floatp, double_float *rehashf, size_t *rehashi)
{
	addr pos;
	double_float valuef;
	size_t valuei;

	if (GetKeyArgs(rest, KEYWORD_REHASH_SIZE, &pos)) {
		*floatp = 1;
		*rehashf = HASHTABLE_REHASH_SIZE_DEFAULT;
		return 0;
	}
	if (integerp(pos)) {
		if (GetIndex_integer(pos, &valuei)) {
			*floatp = 0;
			*rehashf = 0;
			*rehashi = 0;
			return fmte_("Invalid rehash-size ~S.", pos, NULL);
		}
		if (valuei < 1UL) {
			*floatp = 0;
			*rehashf = 0;
			*rehashi = 0;
			return fmte_("rehash-size ~S must be greater than 1.", pos, NULL);
		}
		*floatp = 0;
		*rehashi = valuei;
		return 0;
	}
	else {
		Return(cast_double_float_unsafe_(pos, &valuef));
		if (valuef <= 1.0) {
			*floatp = 0;
			*rehashf = 0;
			*rehashi = 0;
			return fmte_("rehash-size ~S "
					"must be greater than equal to 1.0.", pos, NULL);
		}
		*floatp = 1;
		*rehashf = valuef;
		return 0;
	}

	return 0;
}

static int make_hash_table_rehash_threshold_common_(addr rest, double_float *ret)
{
	addr pos;
	double_float value;

	if (GetKeyArgs(rest, KEYWORD_REHASH_THRESHOLD, &pos)) {
		value = HASHTABLE_REHASH_THRESHOLD_DEFAULT;
	}
	else {
		Return(cast_double_float_unsafe_(pos, &value));
		if (value < 0.0 || 1.0 < value) {
			*ret = 0.0;
			return fmte_("rehash-threshold ~S "
					"must be a number between 0.0 and 1.0.", pos, NULL);
		}
	}
	*ret = value;

	return 0;
}

int make_hash_table_common_(addr rest, addr *ret)
{
	enum HASHTABLE_TEST test;
	int floatp;
	size_t size, rehashi;
	double_float rehashf, threshold;

	Return(make_hash_table_test_common_(rest, &test));
	Return(make_hash_table_size_common_(rest, &size));
	Return(make_hash_table_rehash_size_common_(rest, &floatp, &rehashf, &rehashi));
	Return(make_hash_table_rehash_threshold_common_(rest, &threshold));

	if (floatp)
		hashtable_full_heap(ret, test, size, rehashf, threshold);
	else
		hashtable_integer_heap(ret, test, size, rehashi, threshold);

	return 0;
}


/*
 *  hash-table-count
 */
void hash_table_count_common(addr var, addr *ret)
{
	size_t size;
	getcount_hashtable(var, &size);
	make_index_integer_heap(ret, size);
}


/*
 *  hash-table-rehash-size
 */
int hash_table_rehash_size_common_(addr var, addr *ret)
{
	double_float valuef;
	size_t valuei;

	if (getrehash_float_hashtable(var, &valuef)) {
		double_float_heap(ret, valuef);
		return 0;
	}
	if (getrehash_integer_hashtable(var, &valuei)) {
		make_index_integer_heap(ret, valuei);
		return 0;
	}

	return fmte_("Invalid hash-table structure ~S.", var, NULL);
}


/*
 *  hash-table-rehash-threshold
 */
void hash_table_rehash_threshold_common(addr var, addr *ret)
{
	double_float value;
	getrehash_threshold_hashtable(var, &value);
	double_float_heap(ret, value);
}


/*
 *  hash-table-size
 */
void hash_table_size_common(addr var, addr *ret)
{
	size_t size;
	getsize_hashtable(var, &size);
	make_index_integer_heap(ret, size);
}


/*
 *  hash-table-test
 */
void hash_table_test_common(addr var, addr *ret)
{
	enum HASHTABLE_TEST test;

	gettest_hashtable(var, &test);
	switch (test) {
		case HASHTABLE_TEST_EQ:
			GetConst(COMMON_EQ, ret);
			break;

		case HASHTABLE_TEST_EQL:
			GetConst(COMMON_EQL, ret);
			break;

		case HASHTABLE_TEST_EQUAL:
			GetConst(COMMON_EQUAL, ret);
			break;

		case HASHTABLE_TEST_EQUALP:
			GetConst(COMMON_EQUALP, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}


/*
 *  gethash
 */
int gethash_common_(addr key, addr table, addr value, addr *ret, addr *check)
{
	addr pos;

	Return(find_hashtable_(table, key, &pos));
	if (pos == Unbound) {
		*check = Nil;
		return Result(ret, (value == Unbound)? Nil: value);
	}
	else {
		*check = T;
		return Result(ret, pos);
	}
}


/*
 *  (setf gethash)
 */
int setf_gethash_common_(LocalRoot local, addr value, addr key, addr table)
{
	Return(intern_hashheap_(table, key, &table));
	SetCdr(table, value);
	return 0;
}


/*
 *  remhash
 */
int remhash_common_(addr key, addr table, addr *ret)
{
	int check;
	Return(delete_hashtable_(table, key, &check));
	return Result(ret, check? Nil: T);
}


/*
 *  maphash
 */
static int maphash_execute_common_(Execute ptr, addr call, addr key, addr value)
{
	addr control;

	push_control(ptr, &control);
	(void)funcall_control_(ptr, call, key, value, NULL);
	return pop_control_(ptr, control);
}

int maphash_common_(Execute ptr, addr call, addr table)
{
	addr cons, key, value;
	size_t i, size;

	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &cons);
		while (cons != Nil) {
			GetCons(cons, &key, &cons);
			GetCons(key, &key, &value);
			Return(maphash_execute_common_(ptr, call, key, value));
		}
	}

	return 0;
}


/*
 *  with-hash-table-iterator
 */
static int with_hash_table_iterator_expand_common_(Execute ptr,
		addr name, addr table, addr body, addr *ret)
{
	/* (let ((inst (make-hash-iterator table)))
	 *   (declare (ignorable inst))
	 *   (macrolet ((name () (list (quote next-hash-iterator) (quote inst))))
	 *     (declare (ignorable name))
	 *     . body))
	 */
	addr let, declare, ignorable, macrolet, list, quote, make, next;
	addr inst, let1, let2, let3;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &make);
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &next);
	Return(make_gensym_(ptr, &inst));

	list_heap(&let1, make, table, NULL);
	list_heap(&let1, inst, let1, NULL);
	conscar_heap(&let1, let1);
	list_heap(&let2, ignorable, inst, NULL);
	list_heap(&let2, declare, let2, NULL);
	list_heap(&let3, quote, inst, NULL);
	list_heap(&next, quote, next, NULL);
	list_heap(&let3, list, next, let3, NULL);
	list_heap(&let3, name, Nil, let3, NULL);
	conscar_heap(&let3, let3);
	list_heap(&name, ignorable, name, NULL);
	list_heap(&name, declare, name, NULL);
	lista_heap(&let3, macrolet, let3, name, body, NULL);
	list_heap(ret, let, let1, let2, let3, NULL);

	return 0;
}

int with_hash_table_iterator_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, table, check;

	/* args */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(name))
		goto error;
	GetCons(name, &name, &table);
	if (! consp(table))
		goto error;
	GetCons(table, &table, &check);
	if (check != Nil)
		goto error;
	/* ((name table) . args) */
	return with_hash_table_iterator_expand_common_(ptr, name, table, args, ret);

error:
	return fmte_("with-hash-table-iterator form ~S must be "
			"((name hash-table) &body body)" , form, NULL);
}


/*
 *  sxhash
 */
int sxhash_common_(addr var, addr *ret)
{
	fixnum value;

	Return(sxhash_equal_(var, &value));
	fixnum_heap(ret, value);

	return 0;
}

