/*
 *  ANSI COMMON LISP: 15. Arrays
 */
#include "array_sequence.h"
#include "array_vector.h"
#include "call_arrays.h"
#include "common_header.h"
#include "cons_list.h"
#include "sequence.h"
#include "type_upgraded.h"

/* (defun make-array (dimensions &key element-type initial-element
 *     initial-contents adjustable fill-pointer
 *     displaced-to displaced-index-offset) ...) -> array
 *   dimensions              (or index list) ;; array-dimensions
 *   element-type            type-specifier  ;; default t
 *   initial-element         t
 *   initial-contents        t
 *   adjustable              t  ;; boolean, default nil
 *   fill-pointer            (or index null (eql t))  ;; default nil
 *   displaced-to            (or array null)  ;; default nil
 *   displaced-index-offset  index  ;; default 0
 */
static int function_make_array(Execute ptr, addr var, addr rest)
{
	Return(make_array_common(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_array_key(addr *ret, int adjustable)
{
	addr keyword, type, type1, type2, type3, key, pos;

	/* element-type  type-specifier */
	GetConst(KEYWORD_ELEMENT_TYPE, &keyword);
	GetTypeTable(&type, TypeSpec);
	cons_heap(&pos, keyword, type);
	conscar_heap(&key, pos);
	/* initial-element  t */
	GetConst(KEYWORD_INITIAL_ELEMENT, &keyword);
	GetTypeTable(&type, T);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* initial-contents  t */
	GetConst(KEYWORD_INITIAL_CONTENTS, &keyword);
	GetTypeTable(&type, T);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* adjustable  t */
	if (adjustable) {
		GetConst(KEYWORD_ADJUSTABLE, &keyword);
		GetTypeTable(&type, T);
		cons_heap(&pos, keyword, type);
		cons_heap(&key, pos, key);
	}
	/* fill-pointer  (or index null (eql t)) */
	GetConst(KEYWORD_FILL_POINTER, &keyword);
	GetTypeTable(&type1, Index);
	GetTypeTable(&type2, Null);
	GetTypeTable(&type3, EqlT);
	type3or_heap(type1, type2, type3, &type);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* displaced-to  (or array null) */
	GetConst(KEYWORD_DISPLACED_TO, &keyword);
	GetTypeTable(&type1, Array);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &type);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* displaced-index-offset  index */
	GetConst(KEYWORD_DISPLACED_INDEX_OFFSET, &keyword);
	GetTypeTable(&type, Index);
	cons_heap(&pos, keyword, type);
	cons_heap(&key, pos, key);
	/* result */
	nreverse(ret, key);
}

static void type_make_array(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Index);
	GetTypeTable(&values, List);
	type2or_heap(arg, values, &arg);
	type_make_array_key(&values, 1);
	typeargs_var1key(&arg, arg, values);
	GetTypeValues(&values, Array);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_array(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_ARRAY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_array);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_array(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjust-array (array dimensions &key element-type initial-element
 *     initial-contents fill-pointer
 *     displaced-to displaced-index-offset) ...) -> array
 *   dimensions              (or index list) ;; array-dimensions
 *   element-type            type-specifier  ;; default t
 *   initial-element         t
 *   initial-contents        t
 *   fill-pointer            (or index null (eql t))  ;; default nil
 *   displaced-to            (or array null)  ;; default nil
 *   displaced-index-offset  index  ;; default 0
 */
static int function_adjust_array(Execute ptr, addr pos, addr dim, addr rest)
{
	Return(adjust_array_common(ptr, pos, dim, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_adjust_array(addr *ret)
{
	addr arg, values, key;

	GetTypeTable(&arg, Index);
	GetTypeTable(&values, List);
	type2or_heap(arg, values, &values);
	type_make_array_key(&key, 0);
	GetTypeTable(&arg, Array);
	typeargs_var2key(&arg, arg, values, key);
	GetTypeValues(&values, Array);
	type_compiled_heap(arg, values, ret);
}

static void defun_adjust_array(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ADJUST_ARRAY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_adjust_array);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_adjust_array(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjustable-array-p (array) ...) -> boolean */
static int function_adjustable_array_p(Execute ptr, addr array)
{
	int check;
	Return(adjustable_array_p_common(array, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_adjustable_array_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ADJUSTABLE_ARRAY_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_adjustable_array_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayBoolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun aref (array &rest args) ...) -> t */
static int function_aref(Execute ptr, addr var, addr rest)
{
	Return(aref_common(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_aref(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&arg, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_AREF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_aref);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_aref(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf aref) (value array &rest args) ...) -> value */
static int function_setf_aref(Execute ptr, addr value, addr var, addr rest)
{
	Return(setf_aref_common(value, var, rest));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_aref(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Index);
	typeargs_var2rest(&arg, type, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_AREF, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_setf_aref);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_aref(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun array-dimension (array axis) -> dimension
 *   axis       index
 *   dimension  index
 */
static int function_array_dimension(Execute ptr, addr var, addr axis)
{
	Return(array_dimension_common(var, axis, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_array_dimension(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Index);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, Index);
	type_compiled_heap(arg, values, ret);
}

static void defun_array_dimension(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_DIMENSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_array_dimension);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_dimension(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-dimensions (array) ...) -> (integer 0 *) */
static int function_array_dimensions(Execute ptr, addr var)
{
	Return(array_dimensions_common(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_array_dimensions(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_DIMENSIONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_dimensions);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayIndex);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-element-type (array) ...) -> typespec */
static int function_array_element_type(Execute ptr, addr var)
{
	Return(array_element_type_common(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_array_element_type(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Array);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_array_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_ELEMENT_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_element_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_element_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-has-fill-pointer-p (array) ...) -> boolean */
static int function_array_has_fill_pointer_p(Execute ptr, addr var)
{
	int check;
	Return(array_has_fill_pointer_p_common(var, &check));
	setbool_control(ptr, check);
	return 0;
}

static void defun_array_has_fill_pointer_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_HAS_FILL_POINTER_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_has_fill_pointer_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayBoolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-displacement (array) ...) -> (or nil array), index */
static int function_array_displacement(Execute ptr, addr pos)
{
	addr value, offset;
	Return(array_displacement_common(pos, &value, &offset));
	setvalues_control(ptr, value, offset, NULL);
	return 0;
}

static void type_array_displacement(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Array);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, Array);
	GetTypeTable(&type, Null);
	type2or_heap(values, type, &values);
	GetTypeTable(&type, Index);
	typevalues_values2(&values, values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_array_displacement(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_DISPLACEMENT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_displacement);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_displacement(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-in-bounds-p (array &rest args) ...) -> boolean */
static int function_array_in_bounds_p(Execute ptr, addr array, addr rest)
{
	int check;
	Return(array_in_bounds_p_common(array, rest, &check));
	setbool_control(ptr, check);
	return 0;
}

static void type_array_in_bounds_p(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Integer);
	typeargs_var1rest(&arg, arg, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_array_in_bounds_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_IN_BOUNDS_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_array_in_bounds_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_in_bounds_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-rank (array) ...) -> (intege 0 *) */
static int function_array_rank(Execute ptr, addr pos)
{
	Return(array_rank_common(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_array_rank(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_RANK, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_rank);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayIndex);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-row-major-index (array &rest args) ...) -> index */
static int function_array_row_major_index(Execute ptr, addr array, addr rest)
{
	Return(array_row_major_index_common(array, rest, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_array_row_major_index(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&arg, arg, values);
	GetTypeValues(&values, Index);
	type_compiled_heap(arg, values, ret);
}

static void defun_array_row_major_index(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_ROW_MAJOR_INDEX, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_array_row_major_index);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_array_row_major_index(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-total-size (array) ...) -> (integer 0 *) */
static int function_array_total_size(Execute ptr, addr array)
{
	Return(array_total_size_common(array, &array));
	setresult_control(ptr, array);
	return 0;
}

static void defun_array_total_size(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAY_TOTAL_SIZE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_array_total_size);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ArrayIndex);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun arrayp (object) ...) -> boolean */
static int function_arrayp(Execute ptr, addr var)
{
	setbool_control(ptr, arrayp_common(var));
	return 0;
}

static void defun_arrayp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARRAYP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_arrayp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fill-pointer (vector) ...) -> index */
static int function_fill_pointer(Execute ptr, addr array)
{
	Return(fill_pointer_common(ptr, array, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_fill_pointer(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Vector);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Index);
	type_compiled_heap(arg, values, ret);
}

static void defun_fill_pointer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILL_POINTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_fill_pointer);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fill_pointer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf fill-pointer) (value array) ...) -> value
 *   value  index
 */
static int function_setf_fill_pointer(Execute ptr, addr value, addr array)
{
	Return(setf_fill_pointer_common(ptr, value, array));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_fill_pointer(addr *ret)
{
	addr arg, values;

	GetTypeTable(&values, Index);
	GetTypeTable(&arg, Vector);
	typeargs_var2(&arg, values, arg);
	GetTypeValues(&values, Index);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_fill_pointer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILL_POINTER, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_fill_pointer);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_fill_pointer(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun row-major-aref (array index) ...) -> t */
static int function_row_major_aref(Execute ptr, addr array, addr index)
{
	Return(row_major_aref_common(array, index, &array));
	setresult_control(ptr, array);
	return 0;
}

static void type_row_major_aref(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Index);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_row_major_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROW_MAJOR_AREF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_row_major_aref);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_row_major_aref(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf row-major-aref) (value array index) ...) -> value */
static int function_setf_row_major_aref(Execute ptr,
		addr value, addr array, addr index)
{
	Return(setf_row_major_aref_common(value, array, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_row_major_aref(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&arg, Array);
	GetTypeTable(&values, Index);
	typeargs_var3(&arg, type, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_row_major_aref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROW_MAJOR_AREF, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_row_major_aref);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_row_major_aref(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun upgraded-array-element-type (typespec &optional environment) ...)
 *     -> typespec
 */
static int function_upgraded_array_element_type(Execute ptr, addr pos, addr env)
{
	Return(upgraded_array_common(ptr, env, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_upgraded_array_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPGRADED_ARRAY_ELEMENT_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_upgraded_array_element_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UpgradedType);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstnat array-dimension-limit FIXNUM_MAX) */
static void defconstant_array_dimension_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_ARRAY_DIMENSION_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstnat array-rank-limit FIXNUM_MAX) */
static void defconstant_array_rank_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_ARRAY_RANK_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstnat array-total-size-limit FIXNUM_MAX) */
static void defconstant_array_total_size_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_ARRAY_TOTAL_SIZE_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defun simple-vector-p (object) ...) -> boolean */
static int function_simple_vector_p(Execute ptr, addr var)
{
	setbool_control(ptr, simple_vector_p_common(var));
	return 0;
}

static void defun_simple_vector_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_VECTOR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_vector_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun svref (vector index) ...) -> t */
static int function_svref(Execute ptr, addr pos, addr index)
{
	Return(svref_common(pos, index, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_svref(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Vector);
	GetTypeTable(&values, Index);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_svref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SVREF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_svref);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_svref(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf svref) (value vector index) ...) -> t */
static int function_setf_svref(Execute ptr, addr value, addr pos, addr index)
{
	Return(setf_svref_common(value, pos, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_svref(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&arg, Vector);
	GetTypeTable(&values, Index);
	typeargs_var3(&arg, type, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_svref(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SVREF, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_svref);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_svref(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun vector (&rest args) ...) -> simple-vector */
static int function_vector(Execute ptr, addr rest)
{
	make_vector_from_list(&rest, rest);
	setresult_control(ptr, rest);
	return 0;
}

static void type_common_vector(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_rest(&arg, arg);
	GetTypeValues(&values, SimpleVector);
	type_compiled_heap(arg, values, ret);
}

static void defun_vector(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_vector);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_common_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vector-pop (vector) ...) -> t */
static int function_vector_pop(Execute ptr, addr pos)
{
	vector_pop_common(pos, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static void type_vector_pop(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Vector);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_vector_pop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR_POP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_vector_pop);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_vector_pop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vector-push (value vector) ...) -> index-null */
static int function_vector_push(Execute ptr, addr value, addr pos)
{
	vector_push_common(value, pos, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static void type_vector_push(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_vector_push(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR_PUSH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_vector_push);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_vector_push(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vector-push-extend (value vector &optional extension) ...)
 *    -> index
 *   extension  (integer 1 *)
 */
static int function_vector_push_extend(Execute ptr,
		addr value, addr pos, addr extension)
{
	vector_push_extend_common(value, pos, extension, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static void type_vector_push_extend(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, T);
	GetTypeTable(&values, Vector);
	type2integer_ab_heap(Nil, 1, &type);
	typeargs_var2opt1(&arg, arg, values, type);
	GetTypeValues(&values, Index);
	type_compiled_heap(arg, values, ret);
}

static void defun_vector_push_extend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTOR_PUSH_EXTEND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_vector_push_extend);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_vector_push_extend(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun vectorp (object) ...) -> boolean */
static int function_vectorp(Execute ptr, addr var)
{
	setbool_control(ptr, vectorp_common(var));
	return 0;
}

static void defun_vectorp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VECTORP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_vectorp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit (bit-array &rest args) ...) -> bit */
static int function_bit(Execute ptr, addr pos, addr rest)
{
	Return(bit_common(pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_bit_common(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, BitArray);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&arg, arg, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(arg, values, ret);
}

static void defun_bit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_bit);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_bit_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sbit (simple-bit-array &rest args) ...) -> bit */
static void type_sbit_common(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, SimpleBitArray);
	GetTypeTable(&values, Index);
	typeargs_var1rest(&arg, arg, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(arg, values, ret);
}

static void defun_sbit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SBIT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_bit);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_sbit_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf bit) (bit bit-array &rest args) ...) -> bit */
static int function_setf_bit(Execute ptr, addr value, addr pos, addr rest)
{
	Return(setf_bit_common(value, pos, rest));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_bit(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&type, Bit);
	GetTypeTable(&arg, BitArray);
	GetTypeTable(&values, Index);
	typeargs_var2rest(&arg, type, arg, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_bit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_setf_bit);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_bit(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun (setf sbit) (bit simple-bit-array &rest args) ...) -> bit */
static void type_setf_sbit(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&type, Bit);
	GetTypeTable(&arg, SimpleBitArray);
	GetTypeTable(&values, Index);
	typeargs_var2rest(&arg, type, arg, values);
	GetTypeValues(&values, Bit);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_sbit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SBIT, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_setf_bit);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_sbit(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun bit-vector-p (t) ...) -> boolean */
static int function_bit_vector_p(Execute ptr, addr var)
{
	setbool_control(ptr, bit_vector_p_common(var));
	return 0;
}

static void defun_bit_vector_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_VECTOR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_bit_vector_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-bit-vector-p (t) ...) -> boolean */
static int function_simple_bit_vector_p(Execute ptr, addr var)
{
	setbool_control(ptr, simple_bit_vector_p_common(var));
	return 0;
}

static void defun_simple_bit_vector_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_BIT_VECTOR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_bit_vector_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-and (bit-array1 bit-array2 &optional opt-arg) ...)
 *     -> bit-array
 *   opt-arg  (or boolean bit-array)
 */
static fixed bitcalc_and(fixed a, fixed b)
{
	return a & b;
}

static int function_bit_and(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_and);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_and(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_AND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_and);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-andc1 (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_andc1(fixed a, fixed b)
{
	return (~a) & b;
}

static int function_bit_andc1(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_andc1);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_andc1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ANDC1, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_andc1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-andc2 (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_andc2(fixed a, fixed b)
{
	return a & (~b);
}

static int function_bit_andc2(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_andc2);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_andc2(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ANDC2, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_andc2);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-eqv (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_eqv(fixed a, fixed b)
{
	return a ^ b;
}

static int function_bit_eqv(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_eqv);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_eqv(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_EQV, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_eqv);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-ior (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_ior(fixed a, fixed b)
{
	return a | b;
}

static int function_bit_ior(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_ior);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_ior(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_IOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_ior);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-nand (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_nand(fixed a, fixed b)
{
	return ~(a & b);
}

static int function_bit_nand(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_nand);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_nand(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_NAND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_nand);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-nor (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_nor(fixed a, fixed b)
{
	return ~(a | b);
}

static int function_bit_nor(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_nor);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_nor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_NOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_nor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-orc1 (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_orc1(fixed a, fixed b)
{
	return (~a) | b;
}

static int function_bit_orc1(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_orc1);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_orc1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ORC1, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_orc1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-orc2 (bit-array1 bit-array2 &optional opt-arg) ...) */
static fixed bitcalc_orc2(fixed a, fixed b)
{
	return a | (~b);
}

static int function_bit_orc2(Execute ptr, addr array1, addr array2, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitcalc(&array1, array1, array2, opt, bitcalc_orc2);
	setresult_control(ptr, array1);

	return 0;
}

static void defun_bit_orc2(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_ORC2, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_orc2);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-xor (bit-array1 bit-array2 &optional opt-arg) ...) */
static void defun_bit_xor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_XOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_bit_eqv);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, BitAnd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bit-not (bit-array1 bit-array2 &optional opt-arg) ...) */
static int function_bit_not(Execute ptr, addr array, addr opt)
{
	if (opt == Unbound)
		opt = Nil;
	array_bitnot(&array, array, opt);
	setresult_control(ptr, array);

	return 0;
}

static void type_bit_not(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, BitArray);
	GetTypeTable(&values, Boolean);
	type2or_heap(arg, values, &values);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, BitArray);
	type_compiled_heap(arg, values, ret);
}

static void defun_bit_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BIT_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_bit_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_bit_not(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_arrays(void)
{
	SetPointerCall(defun, var1dynamic, make_array);
	SetPointerCall(defun, var2dynamic, adjust_array);
	SetPointerCall(defun, var1, adjustable_array_p);
	SetPointerCall(defun, var1dynamic, aref);
	SetPointerCall(defun, var2dynamic, setf_aref);
	SetPointerCall(defun, var2, array_dimension);
	SetPointerCall(defun, var1, array_dimensions);
	SetPointerCall(defun, var1, array_element_type);
	SetPointerCall(defun, var1, array_has_fill_pointer_p);
	SetPointerCall(defun, var1, array_displacement);
	SetPointerCall(defun, var1dynamic, array_in_bounds_p);
	SetPointerCall(defun, var1, array_rank);
	SetPointerCall(defun, var1dynamic, array_row_major_index);
	SetPointerCall(defun, var1, array_total_size);
	SetPointerCall(defun, var1, arrayp);
	SetPointerCall(defun, var1, fill_pointer);
	SetPointerCall(defun, var2, setf_fill_pointer);
	SetPointerCall(defun, var2, row_major_aref);
	SetPointerCall(defun, var3, setf_row_major_aref);
	SetPointerCall(defun, var1opt1, upgraded_array_element_type);
	SetPointerCall(defun, var1, simple_vector_p);
	SetPointerCall(defun, var2, svref);
	SetPointerCall(defun, var3, setf_svref);
	SetPointerCall(defun, dynamic, vector);
	SetPointerCall(defun, var1, vector_pop);
	SetPointerCall(defun, var2, vector_push);
	SetPointerCall(defun, var2opt1, vector_push_extend);
	SetPointerCall(defun, var1, vectorp);
	SetPointerCall(defun, var1dynamic, bit);
	SetPointerCall(defun, var1dynamic, bit);
	SetPointerCall(defun, var2dynamic, setf_bit);
	SetPointerCall(defun, var2dynamic, setf_bit);
	SetPointerCall(defun, var1, bit_vector_p);
	SetPointerCall(defun, var1, simple_bit_vector_p);
	SetPointerCall(defun, var2opt1, bit_and);
	SetPointerCall(defun, var2opt1, bit_andc1);
	SetPointerCall(defun, var2opt1, bit_andc2);
	SetPointerCall(defun, var2opt1, bit_eqv);
	SetPointerCall(defun, var2opt1, bit_ior);
	SetPointerCall(defun, var2opt1, bit_nand);
	SetPointerCall(defun, var2opt1, bit_nor);
	SetPointerCall(defun, var2opt1, bit_orc1);
	SetPointerCall(defun, var2opt1, bit_orc2);
	SetPointerCall(defun, var2opt1, bit_eqv);
	SetPointerCall(defun, var1opt1, bit_not);
}

_g void build_common_arrays(void)
{
	defun_make_array();
	defun_adjust_array();
	defun_adjustable_array_p();
	defun_aref();
	defun_setf_aref();
	defun_array_dimension();
	defun_array_dimensions();
	defun_array_element_type();
	defun_array_has_fill_pointer_p();
	defun_array_displacement();
	defun_array_in_bounds_p();
	defun_array_rank();
	defun_array_row_major_index();
	defun_array_total_size();
	defun_arrayp();
	defun_fill_pointer();
	defun_setf_fill_pointer();
	defun_row_major_aref();
	defun_setf_row_major_aref();
	defun_upgraded_array_element_type();
	defconstant_array_dimension_limit();
	defconstant_array_rank_limit();
	defconstant_array_total_size_limit();
	defun_simple_vector_p();
	defun_svref();
	defun_setf_svref();
	defun_vector();
	defun_vector_pop();
	defun_vector_push();
	defun_vector_push_extend();
	defun_vectorp();
	defun_bit();
	defun_sbit();
	defun_setf_bit();
	defun_setf_sbit();
	defun_bit_vector_p();
	defun_simple_bit_vector_p();
	defun_bit_and();
	defun_bit_andc1();
	defun_bit_andc2();
	defun_bit_eqv();
	defun_bit_ior();
	defun_bit_nand();
	defun_bit_nor();
	defun_bit_orc1();
	defun_bit_orc2();
	defun_bit_xor();
	defun_bit_not();
}

