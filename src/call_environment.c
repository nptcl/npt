#include "callname.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "format.h"
#include "function.h"
#include "heap.h"
#include "heap_memory.h"
#include "hold.h"
#include "integer.h"
#include "package.h"
#include "package_common.h"
#include "package_object.h"
#include "pathname_common.h"
#include "pathname_object.h"
#include "pathname.h"
#include "print.h"
#include "print_write.h"
#include "real_truncate.h"
#include "sort.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_echo.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  apropos-list
 */
static int list_all_packages_sort(Execute ptr, addr *ret)
{
	addr list, key, call;
	LocalHold hold;

	Return(list_all_packages_(&list));
	hold = LocalHold_local_push(ptr, list);
	/* key */
	GetConst(COMMON_PACKAGE_NAME, &key);
	getfunction_global(key, &key);
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	getfunction_global(call, &call);
	localhold_push(hold, call);
	/* sort */
	Return(quick_sort_sequence_(ptr, list, call, key));
	localhold_end(hold);

	return Result(ret, list);
}

static int apropos_symbol_p_(addr var, addr name, int *ret)
{
	size_t size, all, diff, x, y;
	unicode a, b;

	Check(! stringp(var), "type1 error");
	Check(! stringp(name), "type1 error");
	string_length(var, &size);
	string_length(name, &all);
	if (all < size)
		return Result(ret, 0);
	diff = (all - size) + 1;
	for (x = 0; x < diff; x++) {
		for (y = 0; y < size; y++) {
			Return(string_getc_(var, y, &a));
			Return(string_getc_(name, x + y, &b));
			if (toUpperUnicode(a) != toUpperUnicode(b))
				goto next;
		}
		return Result(ret, 1);
next:
		continue;
	}

	return Result(ret, 0);
}

static int apropos_symbol_common(Execute ptr, addr var, addr package, addr *ret)
{
	int check;
	addr list, symbol, x, key, call;
	LocalHold hold;

	/* list */
	Return(all_symbols_package_(package, &package));
	for (list = Nil; package != Nil; ) {
		GetCons(package, &symbol, &package);
		GetNameSymbol(symbol, &x);
		Return(apropos_symbol_p_(var, x, &check));
		if (check)
			cons_heap(&list, symbol, list);
	}

	/* sort */
	hold = LocalHold_local_push(ptr, list);
	/* key */
	GetConst(COMMON_SYMBOL_NAME, &key);
	getfunction_global(key, &key);
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	getfunction_global(call, &call);
	localhold_push(hold, call);
	/* sort */
	Return(quick_sort_sequence_(ptr, list, call, key));
	localhold_end(hold);

	return Result(ret, list);
}

_g int apropos_list_common(Execute ptr, addr var, addr package, addr *ret)
{
	addr list, root, x, y;
	LocalHold hold;

	Return(string_designer_heap_(&var, var, NULL));
	if (package != Nil)
		return apropos_symbol_common(ptr, var, package, ret);

	/* list-all-packages */
	hold = LocalHold_array(ptr, 1);
	Return(list_all_packages_sort(ptr, &list));
	localhold_push(hold, list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &package, &list);
		Return(apropos_symbol_common(ptr, var, package, &x));
		while (x != Nil) {
			GetCons(x, &y, &x);
			pushnew_heap(root, y, &root);
			localhold_set(hold, 0, root);
		}
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}


/*
 *  apropos
 */
_g int apropos_common(Execute ptr, addr var, addr package)
{
	addr stream, list, name;

	standard_output_stream(ptr, &stream);
	Return(apropos_list_common(ptr, var, package, &list));
	Return(fresh_line_stream_(stream, NULL));
	while (list != Nil) {
		GetCons(list, &var, &list);
		/* PACKAGE::NAME */
		GetPackageSymbol(var, &package);
		Return(getname_package_(package, &package));
		GetNameSymbol(var, &name);
		Return(print_string_stream_(stream, package));
		Return(print_ascii_stream_(stream, "::"));
		Return(print_string_stream_(stream, name));

		/* variable */
		getspecial_local(ptr, var, &name);
		if (name != Unbound) {
			Return(print_ascii_stream_(stream, ", Variable"));
		}
		/* function */
		GetFunctionSymbol(var, &name);
		if (name != Unbound) {
			Return(print_ascii_stream_(stream, ", Function"));
		}
		/* macro */
		getmacro_symbol(var, &name);
		if (name != Unbound) {
			Return(print_ascii_stream_(stream, ", Macro"));
		}
		/* terpri */
		Return(terpri_stream_(stream));
	}

	return 0;
}


/*
 *  time
 */
_g int time_common(addr form, addr env, addr *ret)
{
	/* (multiple-value-bind (real1 run1 space1 count1) (lisp-system::timeinfo)
	 *   (let ((list (multiple-value-list expr)))
	 *     (multiple-value-bind (real2 run2 space2 count2) (lisp-system::timeinfo)
	 *       (fresh-line)
	 *       (format *trace-output* "Real-Time~15T~A~%" (- real2 real1))
	 *       (format *trace-output* "Run-Time~15T~A~%" (- run2 run1))
	 *       (format *trace-output* "Heap-Space~15T~A~%" (- space2 space1))
	 *       (format *trace-output* "Heap-Count~15T~A~%" (- count2 count1)))
	 *     (values-list list)))
	 */
	addr expr, mvbind, mvlist, timeinfo, let, fresh, format, trace, minus, vlist;
	addr space1, space2, count1, count2, real1, real2, run1, run2;
	addr str1, str2, str3, str4;
	addr args1, args2, list;

	Return_getcdr(form, &args1);
	if (! consp_getcons(args1, &expr, &args1))
		goto error;
	if (args1 != Nil)
		goto error;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_MULTIPLE_VALUE_LIST, &mvlist);
	GetConst(SYSTEM_TIMEINFO, &timeinfo);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_FRESH_LINE, &fresh);
	GetConst(COMMON_FORMAT, &format);
	GetConst(SPECIAL_TRACE_OUTPUT, &trace);
	GetConst(COMMON_MINUS, &minus);
	GetConst(COMMON_VALUES_LIST, &vlist);
	strvect_char_heap(&str1, "Real-Time~15T~A~%");
	strvect_char_heap(&str2, "Run-Time~15T~A~%");
	strvect_char_heap(&str3, "Heap-Space~15T~A~%");
	strvect_char_heap(&str4, "Heap-Count~15T~A~%");
	make_symbolchar(&real1, "REAL1");
	make_symbolchar(&real2, "REAL2");
	make_symbolchar(&run1, "RUN1");
	make_symbolchar(&run2, "RUN2");
	make_symbolchar(&space1, "SPACE1");
	make_symbolchar(&space2, "SPACE2");
	make_symbolchar(&count1, "COUNT1");
	make_symbolchar(&count2, "COUNT2");
	make_symbolchar(&list, "LIST");
	list_heap(&args1, real1, run1, space1, count1, NULL);
	list_heap(&args2, real2, run2, space2, count2, NULL);
	list_heap(&real1, minus, real2, real1, NULL);
	list_heap(&run1, minus, run2, run1, NULL);
	list_heap(&space1, minus, space2, space1, NULL);
	list_heap(&count1, minus, count2, count1, NULL);
	list_heap(&real1, format, trace, str1, real1, NULL);
	list_heap(&run1, format, trace, str2, run1, NULL);
	list_heap(&space1, format, trace, str3, space1, NULL);
	list_heap(&count1, format, trace, str4, count1, NULL);
	list_heap(&timeinfo, timeinfo, NULL);
	list_heap(&fresh, fresh, NULL);
	list_heap(&args2, mvbind, args2, timeinfo,
			fresh, real1, run1, space1, count1, NULL);
	list_heap(&vlist, vlist, list, NULL);
	list_heap(&mvlist, mvlist, expr, NULL);
	list_heap(&list, list, mvlist, NULL);
	list_heap(&list, list, NULL);
	list_heap(&let, let, list, args2, vlist, NULL);
	list_heap(ret, mvbind, args1, timeinfo, let, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("Macro TIME ~S must be a (time form).", form, NULL);
}


/*
 *  room
 */
static int room_output_common(Execute ptr, addr stream)
{
	size_t size, object, space, percent;
	addr pos;

	/* heap memory */
	size = get_heap_size();
	object = get_heap_object();
	pos = intsizeh(size);
	Return(format_stream(ptr, stream, "Heap Size:~20T~A~40T[byte]~%", pos, NULL));
	pos = intsizeh(object);
	Return(format_stream(ptr, stream, "Object memory:~20T~A~40T[byte]~%", pos, NULL));
	pos = intsizeh(get_heap_count());
	Return(format_stream(ptr, stream, "Object count:~20T~A~40T[object]~%", pos, NULL));
	pos = intsizeh(get_heap_gc_count());
	Return(format_stream(ptr, stream, "GC count:~20T~A~40T[times]~%", pos, NULL));

	/* free space */
	space = size - object;
	pos = intsizeh(space);
	Return(format_stream(ptr, stream, "Free space:~20T~A~40T[byte]~%", pos, NULL));

	/* percent */
	percent = (space >> 16) * 100 / (size >> 16);
	pos = intsizeh(percent);
	Return(format_stream(ptr, stream, "Free percent:~20T~A~40T[percent]~%", pos, NULL));

	return 0;
}

static int room_default_common(Execute ptr, addr stream)
{
	Return(format_stream(ptr, stream, "Room default output.~%", NULL));
	return room_output_common(ptr, stream);
}

static int room_minimal_common(Execute ptr, addr stream)
{
	Return(format_stream(ptr, stream, "Room minimal output.~%", NULL));
	return room_output_common(ptr, stream);
}

static int room_maximal_common(Execute ptr, addr stream)
{
	Return(format_stream(ptr, stream, "Room maximal output.~%", NULL));
	return room_output_common(ptr, stream);
}

_g int room_common(Execute ptr, addr var)
{
	addr stream, check;

	standard_output_stream(ptr, &stream);
	Return(fresh_line_stream_(stream, NULL));
	if (var == Unbound)
		return room_default_common(ptr, stream);
	if (var == Nil)
		return room_minimal_common(ptr, stream);
	if (var == T)
		return room_maximal_common(ptr, stream);
	GetConst(KEYWORD_DEFAULT, &check);
	if (var == check)
		return room_default_common(ptr, stream);

	/* error */
	return fmte_("Invalid ROOM argument ~S.", var, NULL);
}


/*
 *  ed
 */
static int ed_execute_common(Execute ptr, addr file)
{
	addr call;

	GetConst(SYSTEM_ED_FUNCTION, &call);
	getspecialcheck_local(ptr, call, &call);
	return funcall_control(ptr, call, file, NULL);
}

static int ed_file_common(Execute ptr, addr var)
{
	int check;

	/* nil */
	if (var == Nil)
		return ed_execute_common(ptr, Nil);

	/* argument */
	Return(pathname_designer_heap_(ptr, var, &var));
	Return(wild_pathname_boolean_(var, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, var,
				"~ED can't use wild card pathname ~S.", var, NULL);
	}
	Return(physical_pathname_local_(ptr, var, &var));
	Return(namestring_pathname_(ptr, &var, var));
	return ed_execute_common(ptr, var);
}

static int ed_function_lambda(addr symbol, addr *ret)
{
	addr pos;

	Return(parse_callname_error_(&pos, symbol));
	getglobal_parse_callname(pos, &pos);
	if (pos == Unbound)
		goto error;
	getdefunform_function(pos, &pos);
	if (pos == Unbound)
		goto error;
	return Result(ret, pos);

error:
	*ret = Nil;
	return fmte_("Cannot edit ~S function.", symbol, NULL);
}

static void ed_function_name(Execute ptr, addr *ret)
{
	addr file;

	GetConst(SYSTEM_ED_TEMPFILE, &file);
	getspecial_local(ptr, file, &file);
	if (file == Unbound)
		strvect_char_heap(&file, "lisp_tempfile.lisp");
	*ret = file;
}

static int ed_function_write(Execute ptr, addr file, addr lambda)
{
	addr control;
	size_t width;

	push_new_control(ptr, &control);
	Return(open_stream_(ptr, &file, file,
				Stream_Open_Direction_Output,
				Stream_Open_Element_Character,
				Stream_Open_IfExists_Supersede,
				Stream_Open_IfDoesNot_Create,
				Stream_Open_External_Default));
	Return(right_margin_print_(ptr, file, &width));
	Return(push_right_margin_print_(ptr, width));
	Return(prin1_print(ptr, file, lambda));
	Return(close_stream_(file));

	return free_control_(ptr, control);
}

static int ed_function_common(Execute ptr, addr symbol)
{
	int result;
	addr file, lambda;

	Return(ed_function_lambda(symbol, &lambda));
	ed_function_name(ptr, &file);
	Return(ed_function_write(ptr, file, lambda));
	Return(ed_file_common(ptr, file));
	Return(eval_load(ptr, &result, file, Unbound, Unbound, 1, Unbound));

	return 0;
}

_g int ed_common(Execute ptr, addr var)
{
	if (var == Unbound)
		var = Nil;
	if (var == Nil)
		return ed_file_common(ptr, Nil);
	else if (function_name_p(var))
		return ed_function_common(ptr, var);
	else
		return ed_file_common(ptr, var);
}


/*
 *  dribble
 */
static int dribble_message_begin(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	Return(pathname_designer_heap_(ptr, file, &name));
	standard_output_stream(ptr, &stream);
	str = "~&;; DRIBBLE begin to write ~S.~%";
	return format_stream(ptr, stream, str, name, NULL);
}

static int dribble_message_end(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	Return(pathname_designer_heap_(ptr, file, &name));
	standard_output_stream(ptr, &stream);
	str = "~&;; DRIBBLE end to write ~S.~%";
	return format_stream(ptr, stream, str, name, NULL);
}

static int dribble_set_stream_(addr file)
{
	addr dfile, dinput, doutput, decho, dbroadcast, sinput, soutput;
	addr input, output, echo, broadcast;

	/*  *dribble*          -> file-stream
	 *  *standard-input*   -> echo-stream
	 *  *standard-output*  -> broadcast-stream
	 *  echo-stream
	 *    echo-input       -> STDIN
	 *    echo-output      -> file-stream
	 *  broadcast-stream
	 *    output1          -> STDOUT
	 *    output2          -> file-stream
	 */

	/* symbol */
	GetConst(SYSTEM_DRIBBLE_FILE, &dfile);
	GetConst(SYSTEM_DRIBBLE_INPUT, &dinput);
	GetConst(SYSTEM_DRIBBLE_OUTPUT, &doutput);
	GetConst(SYSTEM_DRIBBLE_ECHO, &decho);
	GetConst(SYSTEM_DRIBBLE_BROADCAST, &dbroadcast);
	GetConst(SPECIAL_STANDARD_INPUT, &sinput);
	GetConst(SPECIAL_STANDARD_OUTPUT, &soutput);

	/* stream */
	GetValueSymbol(sinput, &input);
	GetValueSymbol(soutput, &output);
	Return(exitpoint_stream_(output));
	Return(force_output_stream_(output));
	open_echo_stream(&echo, input, file);
	list_heap(&broadcast, output, file, NULL);
	open_broadcast_stream(&broadcast, broadcast);

	/* variable */
	SetValueSymbol(dfile, file);
	SetValueSymbol(dinput, input);
	SetValueSymbol(doutput, output);
	SetValueSymbol(decho, echo);
	SetValueSymbol(dbroadcast, broadcast);
	SetValueSymbol(sinput, echo);
	SetValueSymbol(soutput, broadcast);

	return 0;
}

static int dribble_open_file(Execute ptr, addr file)
{
	int check;

	Return(physical_pathname_heap_(ptr, file, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, file,
				"~DRIBBLE can't use wild card pathname ~S.", file, NULL);
	}
	Return(open_stream_(ptr, &file, file,
				Stream_Open_Direction_Output,
				Stream_Open_Element_Character,
				Stream_Open_IfExists_Supersede,
				Stream_Open_IfDoesNot_Create,
				Stream_Open_External_Default));
	Return(dribble_set_stream_(file));
	return dribble_message_begin(ptr, file);
}

static int dribble_open(Execute ptr, addr file)
{
	addr check;

	GetConst(SYSTEM_DRIBBLE_FILE, &check);
	GetValueSymbol(check, &check);
	if (check != Unbound) {
		Return(pathname_designer_heap_(ptr, check, &check));
		return fmtw_("DRIBBLE already open file ~S.", check, NULL);
	}
	else {
		return dribble_open_file(ptr, file);
	}
}

static int dribble_close_stream(Execute ptr)
{
	addr dfile, dinput, doutput, decho, dbroadcast, sinput, soutput;
	addr file, input, output, echo, broadcast;

	/* symbol */
	GetConst(SYSTEM_DRIBBLE_FILE, &dfile);
	GetConst(SYSTEM_DRIBBLE_INPUT, &dinput);
	GetConst(SYSTEM_DRIBBLE_OUTPUT, &doutput);
	GetConst(SYSTEM_DRIBBLE_ECHO, &decho);
	GetConst(SYSTEM_DRIBBLE_BROADCAST, &dbroadcast);
	GetConst(SPECIAL_STANDARD_INPUT, &sinput);
	GetConst(SPECIAL_STANDARD_OUTPUT, &soutput);

	/* variable */
	GetValueSymbol(dfile, &file);
	GetValueSymbol(dinput, &input);
	GetValueSymbol(doutput, &output);
	GetValueSymbol(decho, &echo);
	GetValueSymbol(dbroadcast, &broadcast);

	/* close */
	Return(dribble_message_end(ptr, file));
	SetValueSymbol(dfile, Unbound);
	SetValueSymbol(dinput, Unbound);
	SetValueSymbol(doutput, Unbound);
	SetValueSymbol(decho, Unbound);
	SetValueSymbol(dbroadcast, Unbound);
	SetValueSymbol(sinput, input);
	SetValueSymbol(soutput, output);
	Return(close_stream_(echo));
	Return(close_stream_(broadcast));
	Return(close_stream_(file));

	return 0;
}

static int dribble_close(Execute ptr)
{
	addr symbol, file;

	GetConst(SYSTEM_DRIBBLE_FILE, &symbol);
	GetValueSymbol(symbol, &file);
	if (file != Unbound) {
		Return(dribble_close_stream(ptr));
	}

	return 0;
}

_g int dribble_common(Execute ptr, addr file)
{
	if (file != Nil)
		return dribble_open(ptr, file);
	else
		return dribble_close(ptr);
}

