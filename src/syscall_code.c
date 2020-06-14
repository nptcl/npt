#include "bigdata.h"
#include "bignum.h"
#include "callname.h"
#include "call_objects.h"
#include "clos.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "core.h"
#include "declare.h"
#include "eastasian.h"
#include "env_code.h"
#include "env_time.h"
#include "equal.h"
#include "eval.h"
#include "files.h"
#include "format.h"
#include "function.h"
#include "gc.h"
#include "hashtable.h"
#include "heap_memory.h"
#include "integer.h"
#include "lambda.h"
#include "local.h"
#include "loop_bind.h"
#include "package.h"
#include "pathname.h"
#include "print.h"
#include "print_pretty.h"
#include "print_write.h"
#include "process.h"
#include "radix.h"
#include "random_state.h"
#include "ratio.h"
#include "sequence.h"
#include "sort.h"
#include "stream.h"
#include "stream_pretty.h"
#include "stream_string.h"
#include "strtype.h"
#include "strvect.h"
#include "structure.h"
#include "symbol.h"
#include "syscall_code.h"
#include "type.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_subtypep.h"

/* hello */
_g void hello_syscode(Execute ptr)
{
	addr stream;

	standard_output_stream(ptr, &stream);
	fresh_line_stream(stream);
	print_ascii_stream(stream, "Hello");
	terpri_stream(stream);
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
_g void savecore_syscode(Execute ptr, addr file)
{
	pathname_designer_local(ptr, file, &file);
	savecore_execute(ptr, file);
}


/* redirect-restart */
_g int redirect_restart_syscode(Execute ptr, addr condition, addr list)
{
	addr pos;

	Check(! conditionp(condition), "type error");
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (GetType(pos) != LISPTYPE_RESTART)
			return fmte_("The argument ~S must be a restart.", pos, NULL);
		pushbind_restart_control(ptr, pos, 0);
	}
	reverse_restart_control(ptr);

	return 0;
}


/* defconstant */
_g int defconstant_syscode(addr symbol, addr value, addr doc)
{
	addr check;

	Check(! symbolp(symbol), "type symbol error");
	Check(doc != Nil && (! stringp(doc)), "type documentation error");
	GetValueSymbol(symbol, &check);
	if (check != Unbound && (! eql_function(check, value)))
		return fmte_("The defconstant cannot setq ~S value.", symbol, NULL);
	ResetStatusReadOnly(symbol);
	SetValueSymbol(symbol, value);
	setdocument_variable_symbol(symbol, doc);
	setspecial_symbol(symbol);
	SetStatusReadOnly(symbol);

	return 0;
}


/* in-package */
_g void in_package_syscode(Execute ptr, addr name, addr *ret)
{
	in_package(ptr, name, ret);
}


/* setplist */
_g void setplist_syscode(addr key, addr value, addr list, addr *ret)
{
	setplist_heap_safe(list, key, value, ret);
}


/* remplist */
_g void remplist_syscode(addr key, addr list, addr *ret1, addr *ret2)
{
	enum RemPlist check;

	check = remplist_safe(list, key, &list);
	*ret1 = list;
	*ret2 = (check != RemPlist_NotFound)? T: Nil;
}


/* make-hash-iterator */
_g void make_hash_iterator_syscode(addr pos, addr *ret)
{
	hash_iterator_heap(ret, pos);
}


/* next-hash-iterator */
_g void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3)
{
	int check;
	addr key, value;

	check = next_hash_iterator(pos, &key, &value);
	if (check) {
		*ret1 = T;
		*ret2 = key;
		*ret3 = value;
	}
	else {
		*ret1 = *ret2 = *ret3 = Nil;
	}
}


/* make-package-iterator */
_g void make_package_iterator_syscode(addr pos, addr a, addr b, addr c, addr *ret)
{
	package_iterator_heap(ret, pos, (a != Nil), (b != Nil), (c != Nil));
}


/* next-package-iterator */
_g void next_package_iterator_syscode(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4)
{
	enum PACKAGE_TYPE check;
	addr symbol, status, package;

	check = next_package_iterator(pos, &symbol, &package);
	if (check == PACKAGE_TYPE_NIL) {
		*ret1 = *ret2 = *ret3 = *ret4 = Nil;
	}
	else {
		keyword_packagetype(check, &status);
		*ret1 = T;
		*ret2 = symbol;
		*ret3 = status;
		*ret4 = package;
	}
}


/* defpackage */
_g int defpackage_syscode(Execute ptr, addr rest, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(defpackage_execute(ptr, rest, ret));
	rollback_local(local, stack);

	return 0;
}


/* do-symbols */
_g int do_symbols_syscode(Execute ptr, addr call, addr package)
{
	return do_symbols_package(ptr, call, package);
}


/* do-external-symbols */
_g int do_external_symbols_syscode(Execute ptr, addr call, addr package)
{
	return do_external_symbols_package(ptr, call, package);
}


/* do-all-symbols */
_g int do_all_symbols_syscode(Execute ptr, addr call)
{
	return do_all_symbols_package(ptr, call);
}


/* getdoc-variable */
_g void getdoc_variable_syscode(addr var, addr *ret)
{
	getdocument_variable_symbol(var, ret);
}


/* setdoc-variable */
_g void setdoc_variable_syscode(addr var, addr value)
{
	setdocument_variable_symbol(var, value);
}


/* specialp */
_g void specialp_syscode(addr var, addr *ret)
{
	*ret = specialp_symbol(var)? T: Nil;
}


/* ecase-error */
_g void ecase_error_syscode(addr value, addr list)
{
	make_vector4_from_list(&list, list);
	type1_heap(LISPDECL_MEMBER, list, &list);
	type_error(value, list);
}


/* etypecase-error */
_g void etypecase_error_syscode(addr value, addr list)
{
	make_vector4_from_list(&list, list);
	type1_heap(LISPDECL_OR, list, &list);
	type_error(value, list);
}


/* define-setf-expander */
_g void define_setf_expander_syscode(addr symbol, addr call)
{
	setsetfmacro_symbol(symbol, call);
}


/* defsetf-short */
_g int defsetf_short_syscode(Execute ptr,
		addr access, addr update, addr args, addr env,
		addr *r1, addr *r2, addr *r3, addr *r4, addr *r5)
{
	int check;
	addr a, b, g, w, r, pos, v;
	LocalHold hold;

	if (env == Unbound)
		env = Nil;
	make_gensym(ptr, &g);
	conscar_heap(&w, update);
	conscar_heap(&r, access);
	a = b = Nil;

	hold = LocalHold_array(ptr, 5);
	localhold_set(hold, 2, g);
	localhold_set(hold, 3, w);
	localhold_set(hold, 4, r);
	while (args != Nil) {
		if (! consp(args))
			return fmte_("Invalid call argument ~S.", args, NULL);
		GetCons(args, &pos, &args);
		if (eval_constantp(ptr, pos, env, &check))
			return 1;
		if (check) {
			cons_heap(&w, pos, w);
			cons_heap(&r, pos, r);
		}
		else {
			make_gensym(ptr, &v);
			cons_heap(&a, v, a);
			cons_heap(&b, pos, b);
			cons_heap(&w, v, w);
			cons_heap(&r, v, r);
			localhold_set(hold, 0, a);
			localhold_set(hold, 1, b);
		}
		localhold_set(hold, 3, w);
		localhold_set(hold, 4, r);
	}
	localhold_end(hold);

	cons_heap(&w, g, w);
	nreverse(&a, a);
	nreverse(&b, b);
	conscar_heap(&g, g);
	nreverse(&w, w);
	nreverse(&r, r);
	*r1 = a;
	*r2 = b;
	*r3 = g;
	*r4 = w;
	*r5 = r;

	return 0;
}


/* defsetf-long */
static void defsetf_push(addr array, int index, addr pos)
{
	addr root;
	GetArrayA2(array, index, &root);
	cons_heap(&root, pos, root);
	SetArrayA2(array, index, root);
}

static int defsetf_var_bind_(Execute ptr, addr *args, addr list, addr array)
{
	addr pos, gensym, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! consp(*args))
			return fmte_("The argument ~S must be list type.", *args, NULL);
		GetCons(*args, &value, args);
		make_gensym(ptr, &gensym);
		defsetf_push(array, 0, gensym);
		defsetf_push(array, 1, value);
		defsetf_push(array, 2, pos);
		defsetf_push(array, 3, gensym);
	}

	return 0;
}

static int defsetf_opt_bind_(Execute ptr, addr *args, addr list, addr array)
{
	int check;
	addr pos, var, init, sup, gensym;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &sup, NULL);
		check = (*args != Nil);
		if (check) {
			if (! consp(*args))
				return fmte_("The argument ~S must be list type.", *args, NULL);
			GetCons(*args, &init, args);
		}
		make_gensym(ptr, &gensym);
		defsetf_push(array, 0, gensym);
		defsetf_push(array, 1, init);
		defsetf_push(array, 2, var);
		defsetf_push(array, 3, gensym);
		if (sup != Nil) {
			make_gensym(ptr, &gensym);
			defsetf_push(array, 0, gensym);
			defsetf_push(array, 1, check? T: Nil);
			defsetf_push(array, 2, sup);
			defsetf_push(array, 3, gensym);
		}
	}

	return 0;
}

static int defsetf_store_bind_(Execute ptr, addr list, addr array, addr *ret)
{
	addr root, symbol, gensym;

	root = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("defsetf store ~S must be a list type.", list, NULL);
		GetCons(list, &symbol, &list);
		if (! symbolp(symbol))
			return fmte_("defsetf store ~S must be a symbol type.", symbol, NULL);
		make_gensym(ptr, &gensym);
		defsetf_push(array, 2, symbol);
		defsetf_push(array, 3, gensym);
		cons_heap(&root, gensym, root);
	}
	nreverse(ret, root);

	return 0;
}

static int defsetf_bind_(Execute ptr, addr args,
		addr lambda, addr store,
		addr *a, addr *b, addr *c, addr *d, addr *g)
{
	addr var, opt, rest, key, allow, env, array;

	List_bind(lambda, &var, &opt, &rest, &key, &allow, &env, NULL);
	vector2_heap(&array, 4);
	Return(defsetf_var_bind_(ptr, &args, var, array));
	Return(defsetf_opt_bind_(ptr, &args, opt, array));
	Return(defsetf_store_bind_(ptr, store, array, g));
	/* args */
	GetArrayA2(array, 0, a);
	GetArrayA2(array, 1, b);
	GetArrayA2(array, 2, c);
	GetArrayA2(array, 3, d);
	nreverse(a, *a);
	nreverse(b, *b);
	nreverse(c, *c);
	nreverse(d, *d);

	return 0;
}

static int defsetf_write_(Execute ptr, addr *ret, addr c, addr d, addr body)
{
	/*  (let ((c1 'd1)
	 *        (c2 'd2))
	 *    (declare (ignorable c...))
	 *    body...)
	 */
	addr root, x, y;
	addr quote, let, declare, ignorable;
	LocalHold hold;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);

	/* let-args */
	for (root = Nil; c != Nil; ) {
		Return_getcons(c, &x, &c);
		Return_getcons(d, &y, &d);
		list_heap(&y, quote, y, NULL);
		list_heap(&y, x, y, NULL);
		cons_heap(&root, y, root);
	}
	nreverse(&root, root);
	/* (declare ...) */
	cons_heap(&ignorable, ignorable, c);
	list_heap(&declare, declare, ignorable, NULL);
	/* let */
	lista_heap(&root, let, root, declare, body, NULL);
	hold = LocalHold_local_push(ptr, root);
	Return(eval_object(ptr, root, ret));
	localhold_end(hold);

	return 0;
}

_g int defsetf_long_syscode(Execute ptr, addr rest,
		addr *r1, addr *r2, addr *r3, addr *r4, addr *r5)
{
	addr access, lambda, store, body, args, env;
	addr quote, a, b, c, d, g, w, r;
	LocalHold hold;

	list_bind(rest, &access, &lambda, &store, &body, &args, &env, NULL);
	lambda_defsetf(ptr->local, &lambda, lambda);
	Return(defsetf_bind_(ptr, args, lambda, store, &a, &b, &c, &d, &g));
	/* (values 'a 'b 'g
	 *   `(let ((c1 'd1)
	 *          (c2 'd2))
	 *      (declare (ignorable c...))
	 *      ,@body...)
	 *   '(access d...))
	 */
	GetConst(COMMON_QUOTE, &quote);

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, c, d, g, NULL);
	Return(defsetf_write_(ptr, &w, c, d, body));
	localhold_end(hold);

	cons_heap(&r, access, d);
	*r1 = a;
	*r2 = b;
	*r3 = g;
	*r4 = w;
	*r5 = r;

	return 0;
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
	return simple_sort_sequence(ptr, pos, call, key);
}


/* bubble-sort */
_g int bubble_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return bubble_sort_sequence(ptr, pos, call, key);
}


/* quick-sort */
_g int quick_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return quick_sort_sequence(ptr, pos, call, key);
}


/* merge-sort */
_g int merge_sort_syscode(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key)) key = Nil;
	return merge_sort_sequence(ptr, pos, call, key);
}


/* exit */
_g int exit_syscode(addr code)
{
	int result;
	fixnum value;

	if (code == Unbound) {
		exit_execute(0);
		return 0;
	}
	if (! fixnump(code))
		return fmte_("Invalid code type ~S.", code, NULL);
	GetFixnum(code, &value);
	result = (int)value;
	if (value != (fixnum)result)
		return fmte_("The result code ~S must be a int type.", code, NULL);
	exit_execute(result);

	return 0;
}


/* end-input-stream */
_g void end_input_stream_syscode(addr var, addr *ret)
{
	size_t size;
	getindex_input_stream(var, &size);
	make_index_integer_heap(ret, size);
}


/* make-extend-output-stream */
_g void make_extend_output_stream_syscode(addr var, addr rest, addr *ret)
{
	/* ignore rest */
	open_extend_output_stream(ret, var);
}


/* prompt-for */
_g int prompt_for_syscode(Execute ptr, addr type, addr args, addr *ret)
{
	addr format;
	LocalHold hold;

	if (args == Nil) {
		strvect_char_heap(&format, "Input> ");
	}
	else {
		Return_getcons(args, &format, &args);
		Return(format_string_lisp(ptr, format, args, &format));
	}

	hold = LocalHold_local_push(ptr, format);
	Return(prompt_for_stream(ptr, type, format, &format));
	localhold_end(hold);
	*ret = format;

	return 0;
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
_g void large_number_syscode(LocalRoot local, addr var, addr opt, addr *ret)
{
	if (opt == Unbound) opt = T;
	english_unit_heap(local, ret, var, opt != Nil);
}


/* print-unreadable-call */
_g int print_unreadable_call_syscode(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body)
{
	int check1, check2;

	check1 = (type != Nil);
	check2 = (identity != Nil);
	return print_unreadable_common(ptr, stream, pos, check1, check2, body);
}


/* write-default */
_g int write_default_syscode(Execute ptr, addr stream, addr var, addr *ret)
{
	addr control;
	LocalHold hold;

	output_stream_designer(ptr, stream, &stream);
	push_new_control(ptr, &control);
	hold = LocalHold_local_push(ptr, stream);
	Return(write_default_print(ptr, stream, var));
	localhold_end(hold);
	*ret = var;

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
_g void make_complex_code(addr real, addr imag, addr *ret)
{
	complex_force_heap(ret, real, imag, ComplexType_error);
}


/* equal-random-state */
_g void equal_random_state_syscode(addr left, addr right, addr *ret)
{
	*ret = equal_random_state_addr(left, right)? T: Nil;
}


/* symbol-deftype */
_g void symbol_deftype_syscode(addr var, addr *ret)
{
	getdeftype_symbol(var, ret);
}


/* delete-deftype */
_g void delete_deftype_syscode(addr var, addr *ret)
{
	addr check;

	getdeftype_symbol(var, &check);
	if (check == Nil) {
		*ret = Nil;
	}
	else {
		remdeftype_symbol(var);
		*ret = T;
	}
}


/* subtypep-result */
_g int subtypep_result_syscode(Execute ptr, addr left, addr right, addr *ret)
{
	return subtypep_result_syscall(ptr, left, right, ret);
}


/* ensure-structure */
_g int ensure_structure_syscode_(Execute ptr, addr name, addr slots, addr rest)
{
	return ensure_structure_common_(ptr, name, slots, rest);
}


/* structure-constructor */
_g int structure_constructor_syscode(Execute ptr, addr symbol, addr rest, addr *ret)
{
	return structure_constructor_common(ptr, symbol, rest, ret);
}


/* loop-bind */
_g int loop_bind_syscode(Execute ptr, addr a, addr b, addr c, addr *ret)
{
	return loop_bind_common(ptr, a, b, c, ret);
}


/* make-pprint-stream */
_g void make_pprint_stream_syscode(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	open_pretty_stream(ptr, ret, stream, object, prefix, perline, suffix);
}


/* pprint-gensym */
_g void pprint_gensym_syscode(addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	gensym_pretty_stream(stream, ret);
}


/* pprint-exit */
_g int pprint_exit_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_exit_common(ptr, stream);
}


/* pprint-pop */
_g int pprint_pop_syscode(Execute ptr, addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_pop_common(ptr, stream, ret);
}


/* pprint-check */
_g int pprint_check_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return check_pretty_stream(ptr, stream);
}


/* pprint-close */
_g void pprint_close_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	close_pretty_stream(ptr, stream);
}


/* pprint-pretty */
_g int pprint_pretty_syscode(Execute ptr, addr stream, addr call)
{
	Check(! pretty_stream_p(stream), "type error");
	return call_pretty_stream(ptr, stream, call);
}



/* eastasian-set */
_g void eastasian_set_syscode(addr var, addr value, addr errorp, addr *ret)
{
	eastasian_set_syscall(var, value, errorp, ret);
}


/* eastasian-get */
_g void eastasian_get_syscode(addr var, addr *ret1, addr *ret2)
{
	eastasian_get_syscall(var, ret1, ret2);
}


/* eastasian-width */
_g void eastasian_width_syscode(addr pos, addr *ret1, addr *ret2)
{
	eastasian_width_syscall(pos, ret1, ret2);
}


/* timeinfo */
_g void timeinfo_syscode(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount)
{
	get_internal_real_time_common(local, rreal);
	get_internal_run_time_common(rrun);
	make_index_integer_heap(rsize, get_heap_object());
	make_index_integer_heap(rcount, get_heap_count());
}


/* ed-function */
_g void ed_function_syscode(Execute ptr, addr file)
{
	ed_process(ptr, file);
}


/* run-process */
_g void run_program_syscode(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	run_process(local, var, args, rest, &var);
}


/* make-callname */
_g void make_callname_syscode(addr var, addr *ret)
{
	parse_callname_error(ret, var);
}


/* trace-add */
_g void trace_add_syscode(Execute ptr, addr var, addr *ret)
{
	trace_add_common(ptr, var, ret);
}


/* trace-del */
_g void trace_del_syscode(Execute ptr, addr var, addr *ret)
{
	trace_del_common(ptr, var, ret);
}


/* set-slots */
_g void set_slots_syscode(addr var, addr slots, addr values)
{
	set_slots_syscall(var, slots, values);
}


/* remove-file */
_g void remove_file_syscode(Execute ptr, addr var, addr opt, addr *ret)
{
	*ret = remove_file_common(ptr, var, (opt != Nil))? T: Nil;
}


/* remove-directory */
_g void remove_directory_syscode(Execute ptr, addr var, addr opt, addr *ret)
{
	*ret = remove_directory_common(ptr, var, (opt != Nil))? T: Nil;
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
	type_object(ret, var);
	return 0;
}

