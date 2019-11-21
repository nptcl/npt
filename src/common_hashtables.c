/*
 *  ANSI COMMON LISP: 18. Hash Tables
 */
#include "bignum.h"
#include "common_header.h"
#include "cons.h"
#include "hashtable.h"
#include "integer.h"
#include "number.h"
#include "sxhash.h"
#include "type_parse.h"

/* (defun make-hash-table
 *     (&key test size rehash-size rehash-threshold) ...) -> hashtable
 *   test              (or symbol function)  ;; eq eql equal equalp
 *     -> `(member eq eql equal equalp ,#'eq ,#'eql ,#'equal ,#'equalp)
 *   size              index
 *   rehash-size       (or (integer 1 *) (float (1.0) *))
 *   rehash-threshold  (real 0 1)
 */
static int make_hash_table_test_symbol(addr pos, enum HASHTABLE_TEST *ret)
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

static void make_hash_table_test(addr rest, enum HASHTABLE_TEST *ret)
{
	addr pos, check;

	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_TEST, &pos)) {
		*ret = HASHTABLE_TEST_EQL;
		return;
	}
	if (symbolp(pos)) {
		if (make_hash_table_test_symbol(pos, ret))
			goto error;
		return;
	}
	if (functionp(pos)) {
		GetNameFunction(pos, &check);
		GetCallName(check, &check);
		if (make_hash_table_test_symbol(check, ret))
			goto error;
		return;
	}
error:
	*ret = HASHTABLE_TEST_EQL;
	fmte("Invalid hash-hable-test ~S.", pos, NULL);
}

static void make_hash_table_size(addr rest, size_t *ret)
{
	addr pos;

	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_SIZE, &pos)) {
		*ret = HASHTABLE_SIZE_DEFAULT;
		return;
	}
	if (! integerp(pos)) {
		TypeError(pos, INTEGER);
	}
	if (getindex_integer(pos, ret)) {
		fmte("Invalid hash size ~S.", pos, NULL);
	}
}

static void make_hash_table_rehash_size(addr rest,
		int *floatp, double_float *rehashf, size_t *rehashi)
{
	addr pos;
	double_float valuef;
	size_t valuei;

	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_REHASH_SIZE, &pos)) {
		*floatp = 1;
		*rehashf = HASHTABLE_REHASH_SIZE_DEFAULT;
		return;
	}
	if (integerp(pos)) {
		if (getindex_integer(pos, &valuei))
			fmte("Invalid rehash-size ~S.", pos, NULL);
		if (valuei < 1UL)
			fmte("rehash-size ~S must be greater than 1.", pos, NULL);
		*floatp = 0;
		*rehashi = valuei;
		return;
	}
	else {
		valuef = cast_double_float_unsafe(pos);
		if (valuef <= 1.0)
			fmte("rehash-size ~S must be greater than equal to 1.0.", pos, NULL);
		*floatp = 1;
		*rehashf = valuef;
		return;
	}
}

static void make_hash_table_rehash_threshold(addr rest, double_float *ret)
{
	addr pos;
	double_float value;

	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_REHASH_THRESHOLD, &pos)) {
		value = HASHTABLE_REHASH_THRESHOLD_DEFAULT;
	}
	else {
		value = cast_double_float_unsafe(pos);
		if (value < 0.0 || 1.0 < value)
			fmte("rehash-threshold ~S must be a number between 0.0 and 1.0.", pos, NULL);
	}
	*ret = value;
}

static void function_make_hash_table(Execute ptr, addr rest)
{
	enum HASHTABLE_TEST test;
	int floatp;
	size_t size, rehashi;
	double_float rehashf, threshold;

	make_hash_table_test(rest, &test);
	make_hash_table_size(rest, &size);
	make_hash_table_rehash_size(rest, &floatp, &rehashf, &rehashi);
	make_hash_table_rehash_threshold(rest, &threshold);

	if (floatp)
		hashtable_full_heap(&rest, test, size, rehashf, threshold);
	else
		hashtable_integer_heap(&rest, test, size, rehashi, threshold);
	setresult_control(ptr, rest);
}

static void type_make_hash_table(addr *ret)
{
	addr arg, values, key, type;
	addr key1, key2, key3, key4;

	/* test */
	GetConst(KEYWORD_TEST, &key);
	GetTypeTable(&type, FunctionDesigner);
	cons_heap(&key1, key, type);
	/* size */
	GetConst(KEYWORD_SIZE, &key);
	GetTypeTable(&type, Index);
	cons_heap(&key2, key, type);
	/* rehash-size */
	GetConst(KEYWORD_REHASH_SIZE, &key);
	GetTypeTable(&type, RehashSize);
	cons_heap(&key3, key, type);
	/* rehash-threshold */
	GetConst(KEYWORD_REHASH_THRESHOLD, &key);
	GetTypeTable(&type, RehashThreshold);
	cons_heap(&key4, key, type);
	/* &key */
	list_heap(&arg, key1, key2, key3, key4, NULL);
	typeargs_key(&arg, arg);
	/* values */
	GetTypeTable(&values, Hashtable);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_hash_table(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_HASH_TABLE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_hash_table);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_hash_table(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-p (object) ...) -> boolean */
static void function_hash_table_p(Execute ptr, addr var)
{
	setbool_control(ptr, hashtablep(var));
}

static void defun_hash_table_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-count (hash-table) ...) -> count
 *   count  (integer 0 *)
 */
static void function_hash_table_count(Execute ptr, addr var)
{
	size_t size;

	getcount_hashtable(var, &size);
	make_index_integer_alloc(NULL, &var, size);
	setresult_control(ptr, var);
}

static void defun_hash_table_count(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_COUNT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_count);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, HashTableCount);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-rehash-size (hash-table) ...) -> rehash-size
 *    rehash-size  (or (integer 1 *) (float (1.0) *))
 */
static void function_hash_table_rehash_size(Execute ptr, addr var)
{
	addr pos;
	double_float valuef;
	size_t valuei;

	if (getrehash_float_hashtable(var, &valuef)) {
		double_float_heap(&pos, valuef);
		setresult_control(ptr, pos);
		return;
	}
	if (getrehash_integer_hashtable(var, &valuei)) {
		make_index_integer_alloc(NULL, &pos, valuei);
		setresult_control(ptr, pos);
		return;
	}
	fmte("Invalid hash-table structure ~S.", var, NULL);
}

static void type_hash_table_rehash_size(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Hashtable);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, RehashSize);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_hash_table_rehash_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_REHASH_SIZE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_rehash_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_hash_table_rehash_size(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-rehash-threshold (hash-table) ...) -> threshold
 *   threshold  (real 0 1)
 */
static void function_hash_table_rehash_threshold(Execute ptr, addr var)
{
	double_float value;

	getrehash_threshold_hashtable(var, &value);
	double_float_heap(&var, value);
	setresult_control(ptr, var);
}

static void type_hash_table_rehash_threshold(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Hashtable);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, RehashThreshold);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_hash_table_rehash_threshold(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_REHASH_THRESHOLD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_rehash_threshold);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_hash_table_rehash_threshold(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-size (hash-table) ...) -> size
 *   size  (integer 0 *)
 */
static void function_hash_table_size(Execute ptr, addr var)
{
	size_t size;

	getsize_hashtable(var, &size);
	make_index_integer_alloc(NULL, &var, size);
	setresult_control(ptr, var);
}

static void defun_hash_table_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_SIZE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, HashTableCount);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hash-table-test (hash-table) ...) -> symbol */
static void function_hash_table_test(Execute ptr, addr var)
{
	enum HASHTABLE_TEST test;

	gettest_hashtable(var, &test);
	switch (test) {
		case HASHTABLE_TEST_EQ:
			GetConst(COMMON_EQ, &var);
			break;

		case HASHTABLE_TEST_EQL:
			GetConst(COMMON_EQL, &var);
			break;

		case HASHTABLE_TEST_EQUAL:
			GetConst(COMMON_EQUAL, &var);
			break;

		case HASHTABLE_TEST_EQUALP:
			GetConst(COMMON_EQUALP, &var);
			break;

		default:
			var = Nil;
			break;
	}
	setresult_control(ptr, var);
}

static void type_hash_table_test(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Hashtable);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(arg, values, ret);
}

static void defun_hash_table_test(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HASH_TABLE_TEST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_hash_table_test);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_hash_table_test(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gethash (key hash-table &optional default) ...) -> value, boolean */
static void function_gethash(Execute ptr, addr key, addr table, addr value)
{
	addr result;

	if (value == Unbound) value = Nil;
	if (findvalue_hashtable(table, key, &result))
		setvalues_control(ptr, value, Nil, NULL);
	else
		setvalues_control(ptr, result, T, NULL);
}

static void type_gethash(addr *ret)
{
	addr arg, values, type1, type2;

	GetTypeTable(&arg, Hashtable);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Boolean);
	typeargs_var2opt1(&arg, type1, arg, type1);
	typevalues_values2(&values, type1, type2);
	type_compiled_heap(arg, values, ret);
}

static void defun_gethash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GETHASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_gethash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_gethash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf gethash) (value key hash-table) ...) -> value */
static void function_setf_gethash(Execute ptr,
		addr value, addr key, addr table, addr defvalue)
{
	addr cons;

	/* defvalue is ignored. */
	intern_hashtable(ptr->local, table, key, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void type_setf_gethash(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Hashtable);
	GetTypeTable(&values, T);
	typeargs_var3opt1(&arg, values, values, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_gethash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GETHASH, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_setf_gethash);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_gethash(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun remhash (key hash-table) ...) -> boolean */
static void function_remhash(Execute ptr, addr key, addr table)
{
	setbool_control(ptr, delete_hashtable(table, key) == 0);
}

static void type_remhash(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Hashtable);
	GetTypeTable(&values, T);
	typeargs_var2(&arg, values, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_remhash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMHASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_remhash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_remhash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun maphash (function hash-table) ...) -> null */
static void execute_maphash(Execute ptr, addr call, addr key, addr value)
{
	int check;
	addr control;

	push_close_control(ptr, &control);
	check = funcall_control(ptr, call, key, value, NULL);
	(void)free_check_control(ptr, control, check);
}

static void function_maphash(Execute ptr, addr call, addr table)
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
			execute_maphash(ptr, call, key, value);
		}
	}
	setresult_control(ptr, Nil);
}

static void type_maphash(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Hashtable);
	GetTypeTable(&values, Function);
	typeargs_var2(&arg, values, arg);
	GetTypeValues(&values, Null);
	type_compiled_heap(arg, values, ret);
}

static void defun_maphash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPHASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_maphash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_maphash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-hash-table-iterator ((name hash-table) &body body) ...) */
static void expand_with_hash_table_iterator(Execute ptr,
		addr name, addr table, addr body)
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
	make_gensym(ptr, &inst);

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
	list_heap(&let, let, let1, let2, let3, NULL);
	setresult_control(ptr, let);
}

static void function_with_hash_table_iterator(Execute ptr, addr form, addr env)
{
	addr args, name, table, check;

	/* args */
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &name, &args);
	if (! consp(name)) goto error;
	GetCons(name, &name, &table);
	if (! consp(table)) goto error;
	GetCons(table, &table, &check);
	if (check != Nil) goto error;
	/* ((name table) . args) */
	expand_with_hash_table_iterator(ptr, name, table, args);
	return;

error:
	fmte("with-hash-table-iterator form ~S must be "
			"((name hash-table) &body body)" , form, NULL);
}

static void defmacro_with_hash_table_iterator(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_HASH_TABLE_ITERATOR, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_hash_table_iterator);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun clrhash (hash-table) ...) -> hash-table */
static void function_clrhash(Execute ptr, addr var)
{
	clear_hashtable(var);
	setresult_control(ptr, var);
}

static void type_clrhash(addr *ret)
{
	addr arg, values;

	GetTypeTable(&values, Hashtable);
	typeargs_var1(&arg, values);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_clrhash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLRHASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_clrhash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_clrhash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sxhash (object) ...) -> hash-code
 *    hash-code  index  ;; fixnum
 */
static void function_sxhash(Execute ptr, addr var)
{
	fixnum_heap(&var, sxhash_equal(var));
	setresult_control(ptr, var);
}

static void type_sxhash(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, Index);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_sxhash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SXHASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_sxhash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_sxhash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_hashtables(void)
{
	SetPointerCall(defun, dynamic, make_hash_table);
	SetPointerCall(defun, var1, hash_table_p);
	SetPointerCall(defun, var1, hash_table_count);
	SetPointerCall(defun, var1, hash_table_rehash_size);
	SetPointerCall(defun, var1, hash_table_rehash_threshold);
	SetPointerCall(defun, var1, hash_table_size);
	SetPointerCall(defun, var1, hash_table_test);
	SetPointerCall(defun, var2opt1, gethash);
	SetPointerCall(defun, var3opt1, setf_gethash);
	SetPointerCall(defun, var2, remhash);
	SetPointerCall(defun, var2, maphash);
	SetPointerCall(defmacro, macro, with_hash_table_iterator);
	SetPointerCall(defun, var1, clrhash);
	SetPointerCall(defun, var1, sxhash);
}

_g void build_common_hashtables(void)
{
	defun_make_hash_table();
	defun_hash_table_p();
	defun_hash_table_count();
	defun_hash_table_rehash_size();
	defun_hash_table_rehash_threshold();
	defun_hash_table_size();
	defun_hash_table_test();
	defun_gethash();
	defun_setf_gethash();
	defun_remhash();
	defun_maphash();
	defmacro_with_hash_table_iterator();
	defun_clrhash();
	defun_sxhash();
}

