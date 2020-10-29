#include "array.h"
#include "bignum.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "callname.h"
#include "character.h"
#include "clos.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "core.h"
#include "declare.h"
#include "eastasian.h"
#include "execute.h"
#include "files.h"
#include "format_radix.h"
#include "integer.h"
#include "package_designer.h"
#include "package_object.h"
#include "pathname.h"
#include "process.h"
#include "question.h"
#include "random_state.h"
#include "ratio.h"
#include "sort.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_memory.h"
#include "stream_open.h"
#include "symbol.h"
#include "syscode_function.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "typedef.h"

/* hello */
_g int hello_syscode(Execute ptr)
{
	addr stream;

	Return(standard_output_stream_(ptr, &stream));
	Return(fresh_line_stream_(stream, NULL));
	Return(print_ascii_stream_(stream, "Hello"));
	Return(terpri_stream_(stream));

	return 0;
}


/* infobit */
_g void infobit_syscode(addr rest, addr *ret)
{
	addr x, y;

	for (y = Nil; rest != Nil; y = x) {
		GetCons(rest, &x, &rest);
		infobit(x);
	}
	*ret = y;
}


/* infoprint */
_g void infoprint_syscode(addr rest, addr *ret)
{
	addr x, y;

	for (y = Nil; rest != Nil; y = x) {
		GetCons(rest, &x, &rest);
		infoprint(x);
	}
	*ret = y;
}


/* gc */
_g void gc_syscode(addr rest)
{
	enum GcMode mode;

	if (GetKeyArgs(rest, KEYWORD_FULL, &rest))
		rest = Nil;
	mode = (rest == Nil)? GcMode_Default: GcMode_Full;
	gcstate_execute(mode);
}


/* savecore */
_g int savecore_syscode(Execute ptr, addr file)
{
	Return(pathname_designer_local_(ptr, file, &file));
	return savecore_execute_(ptr, file);
}


/* package-export-list */
_g int package_export_list_syscode_(addr var, addr *ret)
{
	Return(package_designer_(var, &var));
	getexport_package_unsafe(var, ret);
	return 0;
}


/* specialp */
_g void specialp_syscode(addr var, addr *ret)
{
	*ret = specialp_symbol(var)? T: Nil;
}


/* array-general-p */
_g void array_general_p_syscode(addr var, addr *ret)
{
	*ret = array_general_p(var)? T: Nil;
}


/* array-specialized-p */
_g void array_specialized_p_syscode(addr var, addr *ret)
{
	*ret = array_specialized_p(var)? T: Nil;
}


/* simple-sort */
_g int simple_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return simple_sort_sequence_(ptr, pos, call, key);
}


/* bubble-sort */
_g int bubble_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return bubble_sort_sequence_(ptr, pos, call, key);
}


/* quick-sort */
_g int quick_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return quick_sort_sequence_(ptr, pos, call, key);
}


/* merge-sort */
_g int merge_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return merge_sort_sequence_(ptr, pos, call, key);
}


/* exit */
_g int exit_syscode_(Execute ptr, addr code)
{
	fixnum value;

	/* default 0 */
	if (code == Unbound)
		fixnum_heap(&code, 0);

	/* value */
	if (GetFixnum_signed(code, &value))
		return fmte_("EXIT code ~S must be a integer type.", code, NULL);
	ptr->result = (int)value;

	/* invoke */
	return call_exit_condition_(ptr, code);
}


/* closp */
_g void closp_syscode(addr var, addr *ret)
{
	*ret = closp(var)? T: Nil;
}


/* fixnump */
_g void fixnump_syscode(addr var, addr *ret)
{
	*ret = fixnump(var)? T: Nil;
}


/* bignump */
_g void bignump_syscode(addr var, addr *ret)
{
	*ret = bignump(var)? T: Nil;
}


/* ratiop */
_g void ratiop_syscode(addr var, addr *ret)
{
	*ret = ratiop(var)? T: Nil;
}


/* short-float-p */
_g void short_float_p_syscode(addr var, addr *ret)
{
	*ret = (GetType(var) == LISPTYPE_SHORT_FLOAT)? T: Nil;
}


/* single-float-p */
_g void single_float_p_syscode(addr var, addr *ret)
{
	*ret = single_float_p(var)? T: Nil;
}


/* double-float-p */
_g void double_float_p_syscode(addr var, addr *ret)
{
	*ret = double_float_p(var)? T: Nil;
}


/* long-float-p */
_g void long_float_p_syscode(addr var, addr *ret)
{
	*ret = long_float_p(var)? T: Nil;
}


/* callnamep */
_g void callnamep_syscall(addr var, addr *ret)
{
	*ret = callnamep(var)? T: Nil;
}


/* large-number */
_g int large_number_syscode_(LocalRoot local, addr var, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = T;
	return english_unit_heap_(local, ret, var, opt != Nil);
}


/* make-character */
_g int make_character_syscode(addr var, addr *ret)
{
	unicode c;

	switch (GetType(var)) {
		case LISPTYPE_CHARACTER:
			GetCharacter(var, &c);
			make_character_heap(ret, c);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, CHARACTER);
	}

	return 0;
}


/* make-fixnum */
_g int make_fixnum_syscode(addr var, addr *ret)
{
	fixnum value;

	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(var, &value);
			make_fixnum_heap(ret, value);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, FIXNUM);
	}

	return 0;
}


/* make-bignum */
_g int make_bignum_syscode(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_heap(ret, var);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(var, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, INTEGER);
	}

	return 0;
}


/* make-ratio */
static int make_ratio_force_(addr *ret, addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_heap(ret, var);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_heap(ret, var);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, INTEGER);
	}

	return 0;
}

_g int make_ratio_syscode(addr numer, addr denom, addr *ret)
{
	int sign1, sign2;

	Return(make_ratio_force_(&numer, numer));
	Return(make_ratio_force_(&denom, denom));
	GetSignBignum(numer, &sign1);
	GetSignBignum(denom, &sign2);
	SetSignBignum(numer, SignPlus);
	SetSignBignum(denom, SignPlus);
	sign1 = SignMulti(sign1, sign2);
	make_ratio_alloc_unsafe(NULL, &numer, sign1, numer, denom);
	*ret = numer;

	return 0;
}


/* make-complex */
_g int make_complex_code_(addr real, addr imag, addr *ret)
{
	return complex_force_heap_(ret, real, imag, ComplexType_error);
}


/* equal-random-state */
_g void equal_random_state_syscode(addr left, addr right, addr *ret)
{
	*ret = equal_random_state_addr(left, right)? T: Nil;
}


/* subtypep-result */
_g int subtypep_result_syscode(Execute ptr, addr left, addr right, addr *ret)
{
	return subtypep_result_syscall(ptr, left, right, ret);
}


/* eastasian-set */
_g int eastasian_set_syscode_(addr var, addr value, addr errorp, addr *ret)
{
	return eastasian_set_syscall_(var, value, errorp, ret);
}


/* eastasian-get */
_g int eastasian_get_syscode_(addr var, addr *ret1, addr *ret2)
{
	return eastasian_get_syscall_(var, ret1, ret2);
}


/* eastasian-width */
_g int eastasian_width_syscode_(addr pos, addr *ret1, addr *ret2)
{
	return eastasian_width_syscall_(pos, ret1, ret2);
}


/* run-process */
_g int run_program_syscode_(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	return run_process_(local, var, args, rest, &var);
}


/* make-callname */
_g int make_callname_syscode_(addr var, addr *ret)
{
	return parse_callname_error_(ret, var);
}


/* remove-file */
_g int remove_file_syscode(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;
	Return(remove_file_common_(ptr, var, (opt != Nil), &check));
	return Result(ret, check? T: Nil);
}


/* remove-directory */
_g int remove_directory_syscode(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;
	Return(remove_directory_common_(ptr, var, (opt != Nil), &check));
	return Result(ret, check? T: Nil);
}


/* declare-parse */
static int declare_parse_value_(addr symbol, OptimizeType *ret)
{
	addr root, check;

	getroot_declare(&root);
	/* safety */
	GetConst(COMMON_SAFETY, &check);
	if (symbol == check)
		return Result(ret, get_optimize_safety_declare(root));
	/* speed */
	GetConst(COMMON_SPEED, &check);
	if (symbol == check)
		return Result(ret, get_optimize_speed_declare(root));
	/* space */
	GetConst(COMMON_SPACE, &check);
	if (symbol == check)
		return Result(ret, get_optimize_space_declare(root));
	/* debug */
	GetConst(COMMON_DEBUG, &check);
	if (symbol == check)
		return Result(ret, get_optimize_debug_declare(root));
	/* compilation */
	GetConst(COMMON_COMPILATION_SPEED, &check);
	if (symbol == check)
		return Result(ret, get_optimize_compilation_declare(root));

	/* error */
	*ret = 0;
	return fmte_("Invalid declare-parse argument ~S.", symbol, NULL);
}

_g int declare_parse_syscode(addr form, addr *ret)
{
	OptimizeType value;
	addr symbol, check;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &symbol, &check))
		goto error;
	if (check != Nil)
		goto error;
	Return(declare_parse_value_(symbol, &value));
	fixnum_heap(ret, (fixnum)value);
	return 0;

error:
	return fmte_("The declare-parse form ~S must be a (symbol).", form, NULL);
}


/* parse-type */
_g int parse_type_syscode(Execute ptr, addr var, addr *ret)
{
	return parse_type(ptr, ret, var, Nil);
}


/* type-object */
_g int type_object_syscode(addr var, addr *ret)
{
	return type_object_(ret, var);
}

/* upgraded-open-element-type */
_g int upgraded_open_element_type_syscode_(addr var, addr *ret)
{
	return upgrade_open_element_type_stream_(var, ret);
}


/* make-memory-input-stream */
static int getkeyindex_null_(addr list, constindex key, size_t *ret)
{
	addr pos;

	if (getplist_constant_safe(list, key, &pos)) {
		*ret = 0;
	}
	else if (pos == Nil) {
		*ret = 0;
	}
	else {
		Return(getindex_integer_(pos, ret));
	}

	return 0;
}

_g int make_memory_input_stream_syscode_(addr var, addr rest, addr *ret)
{
	size_t size, array;

	/* &key */
	Return(getkeyindex_null_(rest, CONSTANT_KEYWORD_SIZE, &size));
	Return(getkeyindex_null_(rest, CONSTANT_KEYWORD_ARRAY, &array));

	/* call */
	Return(open_input_memory_stream_(&var, var, size, array));
	return Result(ret, var);
}


/* make-memory-output-stream */
_g int make_memory_output_stream_syscode_(addr rest, addr *ret)
{
	size_t size, array;

	/* &key */
	Return(getkeyindex_null_(rest, CONSTANT_KEYWORD_SIZE, &size));
	Return(getkeyindex_null_(rest, CONSTANT_KEYWORD_ARRAY, &array));

	/* call */
	Return(open_output_memory_stream_(&rest, size, array));
	return Result(ret, rest);
}


/* make-memory-io-stream */
_g int make_memory_io_stream_syscode_(addr rest, addr *ret)
{
	addr input;
	size_t size, array;

	/* &key */
	Return(getkeyindex_null_(rest, CONSTANT_KEYWORD_SIZE, &size));
	Return(getkeyindex_null_(rest, CONSTANT_KEYWORD_ARRAY, &array));
	if (GetKeyArgs(rest, KEYWORD_INPUT, &input))
		input = Nil;

	/* call */
	Return(open_io_memory_stream_(&rest, input, size, array));
	return Result(ret, rest);
}


/* with-input-from-memory */
_g int with_input_from_memory_syscode_(Execute ptr, addr form, addr *ret)
{
	addr args, body, var, vector;
	addr let, make, unwind, progn, close, decl, pos;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &vector, &args))
		goto error;

	/* `(let ((,var (system::make-memory-input-stream ,vector)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (close ,var)))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_MEMORY_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	lista_heap(&make, make, vector, args, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse(ret, let);
	return 0;

error:
	return fmte_("WITH-INPUT-FROM-MEMORY form ~S must be a "
			"((var vector ...) &body body).", form, NULL);
}


/* with-output-to-memory */
_g int with_output_to_memory_syscode_(Execute ptr, addr form, addr *ret)
{
	addr args, var, body;
	addr let, make, unwind, progn, get, close, decl, pos;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;

	/* `(let ((,var (make-string-output-stream ...)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@body
	 *             (get-output-stream-string ,var))
	 *      (close ,var)))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_MEMORY_OUTPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(SYSTEM_GET_OUTPUT_STREAM_MEMORY, &get);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	list_heap(&get, get, var, NULL);
	conscar_heap(&progn, progn);
	while (body != Nil) {
		GetCons(body, &pos, &body);
		cons_heap(&progn, pos, progn);
	}
	cons_heap(&progn, get, progn);
	nreverse(&progn, progn);
	list_heap(&unwind, unwind, progn, close, NULL);
	cons_heap(&make, make, args);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse(ret, let);
	return 0;

error:
	return fmte_("WITH-OUTPUT-TO-MEMORY form ~S must be a "
			"((var) &body body).", form, NULL);
}


/* get-output-stream-memory */
_g int get_output_stream_memory_syscode_(addr var, addr *ret)
{
	return memory_stream_heap_(var, ret);
}


/* byte-integer */
static int byte_integer_endian_(addr list, int littlep, addr *ret)
{
	addr x, y, i, v8;
	LocalRoot local;

	local = Local_Thread;
	fixnum_heap(&x, 0);
	fixnum_heap(&i, 0);
	fixnum_heap(&v8, 8);
	while (list != Nil) {
		Return_getcons(list, &y, &list);
		if (littlep) {
			Return(ash_integer_common_(local, y, i, &y));
		}
		else {
			Return(ash_integer_common_(local, x, i, &x));
		}
		Return(plus_ii_real_common_(local, x, y, &x));
		Return(plus_ii_real_common_(local, i, v8, &i));
	}

	return Result(ret, x);
}

_g int byte_integer_syscode_(addr list, addr *ret)
{
	union byte_integer_union {
		uint16_t u16;
		uint8_t u8[2];
	} u;

	u.u16 = 1;
	return byte_integer_endian_(list, u.u8[0] != 0, ret);
}

/* question */
_g int question_syscode_(Execute ptr, addr var, addr args)
{
	return question_values_(ptr, var, args);
}

/* extension */
#ifdef LISP_EXTENSION
#include "ext_eval.h"
#endif
_g int extension_syscode(Execute ptr, addr var)
{
#ifdef LISP_EXTENSION
	return lisps_eval_(ptr, var);
#else
	return fmte_("The implementation is not supported.", NULL);
#endif
}

