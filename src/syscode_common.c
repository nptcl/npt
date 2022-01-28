#include "call_eval.h"
#include "call_objects.h"
#include "clos.h"
#include "clos_type.h"
#include "clos_defgeneric.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "env_time.h"
#include "env_code.h"
#include "equal.h"
#include "eval_load.h"
#include "execute.h"
#include "format.h"
#include "function.h"
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
#include "process_ed.h"
#include "prompt_for.h"
#include "restart.h"
#include "sequence.h"
#include "stream.h"
#include "stream_pretty.h"
#include "stream_string.h"
#include "strtype.h"
#include "structure.h"
#include "structure_define.h"
#include "strvect.h"
#include "symbol.h"
#include "syscode_common.h"
#include "type.h"
#include "typedef.h"

/* defconstant */
int defconstant_syscode(addr symbol, addr value, addr doc)
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
int in_package_syscode_(Execute ptr, addr name, addr *ret)
{
	return in_package_(ptr, name, ret);
}


/* setplist */
void setplist_syscode(addr key, addr value, addr list, addr *ret)
{
	setplist_heap_safe(list, key, value, ret);
}


/* remplist */
int remplist_syscode_(addr key, addr list, addr *ret1, addr *ret2)
{
	enum RemPlist check;

	Return(remplist_safe_(list, key, &list, &check));
	*ret1 = list;
	*ret2 = (check != RemPlist_NotFound)? T: Nil;
	return 0;
}


/* make-hash-iterator */
void make_hash_iterator_syscode(addr pos, addr *ret)
{
	hash_iterator_heap(ret, pos);
}


/* next-hash-iterator */
void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3)
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
int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret)
{
	return package_iterator_heap_(ret, pos, (a != Nil), (b != Nil), (c != Nil));
}


/* next-package-iterator */
int next_package_iterator_syscode_(Execute ptr, addr pos,
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
int defpackage_syscode(Execute ptr, addr var, addr rest, addr *ret)
{
	return defpackage_execute_(ptr, var, rest, ret);
}


/* do-symbols */
int do_symbols_syscode(Execute ptr, addr call, addr package)
{
	return do_symbols_package_(ptr, call, package);
}


/* do-external-symbols */
int do_external_symbols_syscode(Execute ptr, addr call, addr package)
{
	return do_external_symbols_package_(ptr, call, package);
}


/* do-all-symbols */
int do_all_symbols_syscode_(Execute ptr, addr call)
{
	return do_all_symbols_package_(ptr, call);
}


/* getdoc-variable */
void getdoc_variable_syscode(addr var, addr *ret)
{
	getdocument_variable_symbol(var, ret);
}


/* setdoc-variable */
void setdoc_variable_syscode(addr var, addr value)
{
	setdocument_variable_symbol(var, value);
}


/* ecase-error */
int ecase_error_syscode_(Execute ptr, addr value, addr list)
{
	Return(make_vector4_from_list_(&list, list));
	type1_heap(LISPDECL_MEMBER, list, &list);
	return call_type_error_(ptr, value, list);
}


/* etypecase-error */
int etypecase_error_syscode_(Execute ptr, addr value, addr list)
{
	Return(make_vector4_from_list_(&list, list));
	type1_heap(LISPDECL_OR, list, &list);
	return call_type_error_(ptr, value, list);
}


/* define-setf-expander */
int define_setf_expander_syscode_(addr symbol, addr call)
{
	return setsetfmacro_symbol_(symbol, call);
}


/* end-input-stream */
void end_input_stream_syscode(addr var, addr *ret)
{
	size_t size;
	getindex_input_stream(var, &size);
	make_index_integer_heap(ret, size);
}


/* make-extend-output-stream */
void make_extend_output_stream_syscode(addr var, addr rest, addr *ret)
{
	/* ignore rest */
	open_extend_output_stream(ret, var);
}


/* prompt-for */
int prompt_for_syscode(Execute ptr, addr type, addr args, addr *ret)
{
	addr format;
	LocalHold hold;

	if (args == Nil) {
		strvect_char_heap(&format, "Input> ");
	}
	else {
		Return_getcons(args, &format, &args);
		Return(format_string_lisp_(ptr, format, args, &format));
	}

	hold = LocalHold_local_push(ptr, format);
	Return(prompt_for_stream_(ptr, type, format, &format));
	localhold_end(hold);
	*ret = format;

	return 0;
}


/* print-unreadable-call */
int print_unreadable_call_syscode(Execute ptr,
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

int write_default_syscode(Execute ptr, addr stream, addr var, addr *ret)
{
	addr control;

	Return(output_stream_designer_(ptr, stream, &stream));
	push_control(ptr, &control);
	(void)write_default_syscode_call_(ptr, stream, var, ret);
	return pop_control_(ptr, control);
}


/* symbol-deftype */
void symbol_deftype_syscode(addr var, addr *ret)
{
	getdeftype_symbol(var, ret);
}


/* delete-deftype */
void delete_deftype_syscode(addr var, addr *ret)
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
int ensure_structure_syscode_(Execute ptr, addr name, addr slots, addr rest)
{
	return ensure_structure_common_(ptr, name, slots, rest);
}


/* structure-constructor */
int structure_constructor_syscode(Execute ptr, addr symbol, addr rest, addr *ret)
{
	return structure_constructor_common_(ptr, symbol, rest, ret);
}


/* loop-bind */
int loop_bind_syscode(Execute ptr, addr a, addr b, addr c, addr *ret)
{
	return loop_bind_common(ptr, a, b, c, ret);
}


/* make-pprint-stream */
int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	return open_pretty_stream_(ptr, ret, stream, object, prefix, perline, suffix);
}


/* pprint-gensym */
int pprint_gensym_syscode(addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return gensym_pretty_stream_(stream, ret);
}


/* pprint-exit */
int pprint_exit_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_exit_common_(ptr, stream);
}


/* pprint-pop */
int pprint_pop_syscode(Execute ptr, addr stream, addr *ret)
{
	Check(! pretty_stream_p(stream), "type error");
	return pprint_pop_common_(ptr, stream, ret);
}


/* pprint-check */
int pprint_check_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return check_pretty_stream_(ptr, stream);
}


/* pprint-close */
int pprint_close_syscode(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	return close_pretty_stream_(ptr, stream);
}


/* pprint-pretty */
int pprint_pretty_syscode(Execute ptr, addr stream, addr call)
{
	Check(! pretty_stream_p(stream), "type error");
	return call_pretty_stream(ptr, stream, call);
}


/* timeinfo */
int timeinfo_syscode_(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount)
{
	Return(get_internal_real_time_common_(local, rreal));
	get_internal_run_time_common(rrun);
	make_index_integer_heap(rsize, get_heap_object());
	make_index_integer_heap(rcount, get_heap_count());

	return 0;
}


/* ed-function */
int ed_function_syscode_(Execute ptr, addr file)
{
	return ed_process_(ptr, file);
}


/* trace-add */
int trace_add_syscode_(Execute ptr, addr var, addr *ret)
{
	return trace_add_common_(ptr, var, ret);
}


/* trace-del */
int trace_del_syscode_(Execute ptr, addr var, addr *ret)
{
	return trace_del_common_(ptr, var, ret);
}


/* set-slots */
int set_slots_syscode(addr var, addr slots, addr values)
{
	return set_slots_syscall(var, slots, values);
}


/* intern-eql-specializer */
int intern_eql_specializer_syscode(addr var, addr *ret)
{
	return clos_intern_specializer_(var, ret);
}


/* defgeneric */
int defgeneric_define_syscode_(Execute ptr, addr name, addr args, addr *ret)
{
	return system_generic_define_(ptr, name, args, ret);
}

int defgeneric_method_syscode_(addr inst, addr args)
{
	return system_generic_method_(inst, args);
}


/* condition-restarts-push */
static int condition_restarts_check_restarts_(addr list)
{
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! restartp(pos))
			return fmte_("The object ~S must be a restart type.", pos, NULL);
	}

	return 0;
}

int condition_restarts_push_syscode_(addr condition, addr restarts)
{
	addr pos, list;

	Return(condition_restarts_check_restarts_(restarts));
	while (restarts != Nil) {
		GetCons(restarts, &pos, &restarts);
		getassociated_restart(pos, &list);
		cons_heap(&list, condition, list);
		setassociated_restart(pos, list);
	}

	return 0;
}


/* condition-restarts-pop */
int condition_restarts_pop_syscode_(addr condition, addr restarts)
{
	addr pos, list, check;

	Return(condition_restarts_check_restarts_(restarts));
	while (restarts != Nil) {
		GetCons(restarts, &pos, &restarts);
		getassociated_restart(pos, &list);
		if (consp_getcons(list, &check, &list)) {
			if (check == condition)
				setassociated_restart(pos, list);
		}
	}

	return 0;
}


/* condition-restarts-make */
static int condition_restarts_make_name_(addr var, addr *ret)
{
	addr check;

	/* signal -> simple-condition */
	GetConst(COMMON_SIGNAL, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_CONDITION, ret);
		return 0;
	}

	/* error -> simple-error */
	GetConst(COMMON_ERROR, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_ERROR, ret);
		return 0;
	}

	/* cerror -> simple-error */
	GetConst(COMMON_CERROR, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_ERROR, ret);
		return 0;
	}

	/* warn -> simple-warning */
	GetConst(COMMON_WARN, &check);
	if (var == check) {
		GetConst(COMMON_SIMPLE_WARNING, ret);
		return 0;
	}

	/* error */
	*ret = Nil;
	return fmte_("Invalid argument, ~S.", var, NULL);
}

static int condition_restarts_make_string_(Execute ptr,
		addr var, addr car, addr cdr, addr *ret)
{
	addr key1, key2, make, list;

	Return(condition_restarts_make_name_(var, &var));
	GetConst(KEYWORD_FORMAT_CONTROL, &key1);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &key2);
	list_heap(&list, var, key1, car, key2, cdr, NULL);

	/* `(make-condition 'simple-condition
	 *    :format-control ',car
	 *    :format-arguments ',cdr)
	 */
	GetConst(COMMON_MAKE_CONDITION, &make);
	Return(getfunction_global_(make, &make));
	return apply1_control_(ptr, ret, make, list);
}

int condition_restarts_make_syscode_(Execute ptr, addr var, addr list, addr *ret)
{
	addr car, cdr, make;

	Return_getcons(list, &car, &cdr);

	/* string */
	if (stringp(car))
		return condition_restarts_make_string_(ptr, var, car, cdr, ret);

	/* instance */
	if (closp(car)) {
		if (cdr != Nil)
			return fmte_("Invalid condition form, ~S.", list, NULL);
		return Result(ret, car);
	}

	/* `(make-condition ,list) */
	GetConst(COMMON_MAKE_CONDITION, &make);
	Return(getfunction_global_(make, &make));
	return apply1_control_(ptr, ret, make, list);
}


/* make-restart */
int make_restart_syscode_(addr var, addr call, addr rest, addr *ret)
{
	addr inter, report, test, escape, pos;

	Check(! symbolp(var), "type error");
	Check(! functionp(call), "type error");
	if (GetKeyArgs(rest, KEYWORD_INTERACTIVE_FUNCTION, &inter))
		inter = Nil;
	if (GetKeyArgs(rest, KEYWORD_REPORT_FUNCTION, &report))
		report = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_FUNCTION, &test))
		test = Nil;
	if (GetKeyArgs(rest, KEYWORD_ESCAPE, &escape))
		escape = Nil;

	/* restart */
	restart_heap(&pos, var);
	setfunction_restart(pos, call);
	setinteractive_restart(pos, inter);
	setreport_restart(pos, report);
	settest_restart(pos, test);
	setescape_restart(pos, escape != Nil);

	return Result(ret, pos);
}

