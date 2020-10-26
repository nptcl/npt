#include "call_eval.h"
#include "call_objects.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_object.h"
#include "control_operator.h"
#include "env_time.h"
#include "env_code.h"
#include "equal.h"
#include "eval_execute.h"
#include "execute.h"
#include "format.h"
#include "hashtable.h"
#include "heap_memory.h"
#include "hold.h"
#include "integer.h"
#include "lambda.h"
#include "loop_bind.h"
#include "package.h"
#include "package_common.h"
#include "package_defpackage.h"
#include "package_iterator.h"
#include "print.h"
#include "print_pretty.h"
#include "print_write.h"
#include "process.h"
#include "prompt_for.h"
#include "sequence.h"
#include "stream.h"
#include "stream_pretty.h"
#include "stream_string.h"
#include "strtype.h"
#include "structure.h"
#include "strvect.h"
#include "symbol.h"
#include "syscode_common.h"
#include "type.h"
#include "typedef.h"

/* redirect-restart */
_g int redirect_restart_syscode(Execute ptr, addr condition, addr list)
{
	addr pos;

	Check(! conditionp_debug(condition), "type error");
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
_g int in_package_syscode_(Execute ptr, addr name, addr *ret)
{
	return in_package_(ptr, name, ret);
}


/* setplist */
_g void setplist_syscode(addr key, addr value, addr list, addr *ret)
{
	setplist_heap_safe(list, key, value, ret);
}


/* remplist */
_g int remplist_syscode_(addr key, addr list, addr *ret1, addr *ret2)
{
	enum RemPlist check;

	Return(remplist_safe_(list, key, &list, &check));
	*ret1 = list;
	*ret2 = (check != RemPlist_NotFound)? T: Nil;
	return 0;
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
_g int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret)
{
	return package_iterator_heap_(ret, pos, (a != Nil), (b != Nil), (c != Nil));
}


/* next-package-iterator */
_g int next_package_iterator_syscode_(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4)
{
	enum PACKAGE_TYPE check;
	addr symbol, status, package;

	Return(next_package_iterator_(pos, &symbol, &package, &check));
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

	return 0;
}


/* defpackage */
_g int defpackage_syscode(Execute ptr, addr var, addr rest, addr *ret)
{
	return defpackage_execute(ptr, var, rest, ret);
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
_g int do_all_symbols_syscode_(Execute ptr, addr call)
{
	return do_all_symbols_package_(ptr, call);
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


/* ecase-error */
_g int ecase_error_syscode_(Execute ptr, addr value, addr list)
{
	Return(make_vector4_from_list_(&list, list));
	type1_heap(LISPDECL_MEMBER, list, &list);
	return call_type_error_(ptr, value, list);
}


/* etypecase-error */
_g int etypecase_error_syscode_(Execute ptr, addr value, addr list)
{
	Return(make_vector4_from_list_(&list, list));
	type1_heap(LISPDECL_OR, list, &list);
	return call_type_error_(ptr, value, list);
}


/* define-setf-expander */
_g int define_setf_expander_syscode_(addr symbol, addr call)
{
	return setsetfmacro_symbol_(symbol, call);
}


/* defsetf-short */
_g int defsetf_short_syscode(Execute ptr,
		addr access, addr update, addr args, addr env,
		addr *r1, addr *r2, addr *r3, addr *r4, addr *r5)
{
	addr check, a, b, g, w, r, pos, v;
	LocalHold hold;

	if (env == Unbound)
		env = Nil;
	Return(make_gensym_(ptr, &g));
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
		if (constantp_common(ptr, pos, env, &check))
			return 1;
		if (check != Nil) {
			cons_heap(&w, pos, w);
			cons_heap(&r, pos, r);
		}
		else {
			Return(make_gensym_(ptr, &v));
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
		Return(make_gensym_(ptr, &gensym));
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
		Return(make_gensym_(ptr, &gensym));
		defsetf_push(array, 0, gensym);
		defsetf_push(array, 1, init);
		defsetf_push(array, 2, var);
		defsetf_push(array, 3, gensym);
		if (sup != Nil) {
			Return(make_gensym_(ptr, &gensym));
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
		Return(make_gensym_(ptr, &gensym));
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

	Return(list_bind_(rest, &access, &lambda, &store, &body, &args, &env, NULL));
	Return(lambda_defsetf_(ptr->local, &lambda, lambda));
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


/* print-unreadable-call */
_g int print_unreadable_call_syscode(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body)
{
	int check1, check2;

	check1 = (type != Nil);
	check2 = (identity != Nil);
	return print_unreadable_common_(ptr, stream, pos, check1, check2, body);
}


/* write-default */
static int write_default_syscode_call_(Execute ptr, addr stream, addr var, addr *ret)
{
	LocalHold hold;

	Return(output_stream_designer_(ptr, stream, &stream));
	hold = LocalHold_local_push(ptr, stream);
	Return(write_default_print_(ptr, stream, var));
	localhold_end(hold);

	return Result(ret, var);
}

_g int write_default_syscode(Execute ptr, addr stream, addr var, addr *ret)
{
	addr control;

	Return(output_stream_designer_(ptr, stream, &stream));
	push_control(ptr, &control);
	(void)write_default_syscode_call_(ptr, stream, var, ret);
	return pop_control_(ptr, control);
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
_g int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	return open_pretty_stream_(ptr, ret, stream, object, prefix, perline, suffix);
}


/* pprint-gensym */
_g int pprint_gensym_syscode(addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return gensym_pretty_stream_(stream, ret);
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
_g int pprint_close_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return close_pretty_stream_(ptr, stream);
}


/* pprint-pretty */
_g int pprint_pretty_syscode(Execute ptr, addr stream, addr call)
{
	Check(! pretty_stream_p(stream), "type error");
	return call_pretty_stream(ptr, stream, call);
}


/* timeinfo */
_g int timeinfo_syscode_(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount)
{
	Return(get_internal_real_time_common_(local, rreal));
	get_internal_run_time_common(rrun);
	make_index_integer_heap(rsize, get_heap_object());
	make_index_integer_heap(rcount, get_heap_count());

	return 0;
}


/* ed-function */
_g int ed_function_syscode_(Execute ptr, addr file)
{
	return ed_process_(ptr, file);
}


/* trace-add */
_g int trace_add_syscode_(Execute ptr, addr var, addr *ret)
{
	return trace_add_common_(ptr, var, ret);
}


/* trace-del */
_g int trace_del_syscode_(Execute ptr, addr var, addr *ret)
{
	return trace_del_common_(ptr, var, ret);
}


/* set-slots */
_g int set_slots_syscode(addr var, addr slots, addr values)
{
	return set_slots_syscall(var, slots, values);
}

