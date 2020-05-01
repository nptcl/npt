/*
 *  ANSI COMMON LISP: 18. Hash Tables
 */
#include "call_hashtables.h"
#include "common_header.h"
#include "cons.h"
#include "hashtable.h"

/* (defun make-hash-table
 *     (&key test size rehash-size rehash-threshold) ...) -> hashtable
 *   test              (or symbol function)  ;; eq eql equal equalp
 *     -> `(member eq eql equal equalp ,#'eq ,#'eql ,#'equal ,#'equalp)
 *   size              index
 *   rehash-size       (or (integer 1 *) (float (1.0) *))
 *   rehash-threshold  (real 0 1)
 */
static int function_make_hash_table(Execute ptr, addr rest)
{
	Return(make_hash_table_common(rest, &rest));
	setresult_control(ptr, rest);
	return 0;
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
static int function_hash_table_p(Execute ptr, addr var)
{
	setbool_control(ptr, hashtablep(var));
	return 0;
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
static int function_hash_table_count(Execute ptr, addr var)
{
	hash_table_count_common(var, &var);
	setresult_control(ptr, var);
	return 0;
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
static int function_hash_table_rehash_size(Execute ptr, addr var)
{
	Return(hash_table_rehash_size_common(var, &var));
	setresult_control(ptr, var);
	return 0;
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
static int function_hash_table_rehash_threshold(Execute ptr, addr var)
{
	hash_table_rehash_threshold_common(var, &var);
	setresult_control(ptr, var);
	return 0;
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
static int function_hash_table_size(Execute ptr, addr var)
{
	hash_table_size_common(var, &var);
	setresult_control(ptr, var);
	return 0;
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
static int function_hash_table_test(Execute ptr, addr var)
{
	hash_table_test_common(var, &var);
	setresult_control(ptr, var);
	return 0;
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
static int function_gethash(Execute ptr, addr key, addr table, addr value)
{
	gethash_common(key, table, value, &key, &value);
	setvalues_control(ptr, key, value, NULL);
	return 0;
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
static int function_setf_gethash(Execute ptr,
		addr value, addr key, addr table, addr defvalue)
{
	/* defvalue is ignored. */
	setf_gethash_common(ptr->local, value, key, table);
	setresult_control(ptr, value);
	return 0;
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
static int function_remhash(Execute ptr, addr key, addr table)
{
	remhash_common(key, table, &table);
	setresult_control(ptr, table);
	return 0;
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
static int function_maphash(Execute ptr, addr call, addr table)
{
	Return(maphash_common(ptr, call, table));
	setresult_control(ptr, Nil);
	return 0;
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
static int function_with_hash_table_iterator(Execute ptr, addr form, addr env)
{
	Return(with_hash_table_iterator_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
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
static int function_clrhash(Execute ptr, addr var)
{
	clear_hashtable(var);
	setresult_control(ptr, var);
	return 0;
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
static int function_sxhash(Execute ptr, addr var)
{
	sxhash_common(var, &var);
	setresult_control(ptr, var);
	return 0;
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

