#include "callname.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval.h"
#include "format.h"
#include "function.h"
#include "heap.h"
#include "heap_memory.h"
#include "hold.h"
#include "integer.h"
#include "package.h"
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

	list_all_packages(&list);
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
	Return(quick_sort_sequence(ptr, list, call, key));
	localhold_end(hold);
	*ret = list;

	return 0;
}

static int apropos_symbol_p(addr var, addr name)
{
	size_t size, all, diff, x, y;
	unicode a, b;

	Check(! stringp(var), "type1 error");
	Check(! stringp(name), "type1 error");
	string_length(var, &size);
	string_length(name, &all);
	if (all < size)
		return 0;
	diff = (all - size) + 1;
	for (x = 0; x < diff; x++) {
		for (y = 0; y < size; y++) {
			string_getc(var, y, &a);
			string_getc(name, x + y, &b);
			if (toUpperUnicode(a) != toUpperUnicode(b))
				goto next;
		}
		return 1;
next:
		continue;
	}

	return 0;
}

static int apropos_symbol_common(Execute ptr, addr var, addr package, addr *ret)
{
	addr list, symbol, x, key, call;
	LocalHold hold;

	/* list */
	all_symbols_package(package, &package);
	for (list = Nil; package != Nil; ) {
		GetCons(package, &symbol, &package);
		GetNameSymbol(symbol, &x);
		if (apropos_symbol_p(var, x))
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
	Return(quick_sort_sequence(ptr, list, call, key));
	localhold_end(hold);
	*ret = list;

	return 0;
}

_g int apropos_list_common(Execute ptr, addr var, addr package, addr *ret)
{
	addr list, root, x, y;
	LocalHold hold;

	string_designer_heap(&var, var);
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
	fresh_line_stream(stream);
	while (list != Nil) {
		GetCons(list, &var, &list);
		/* PACKAGE::NAME */
		GetPackageSymbol(var, &package);
		getname_package(package, &package);
		GetNameSymbol(var, &name);
		print_string_stream(stream, package);
		print_ascii_stream(stream, "::");
		print_string_stream(stream, name);

		/* variable */
		getspecial_local(ptr, var, &name);
		if (name != Unbound)
			print_ascii_stream(stream, ", Variable");
		/* function */
		GetFunctionSymbol(var, &name);
		if (name != Unbound)
			print_ascii_stream(stream, ", Function");
		/* macro */
		getmacro_symbol(var, &name);
		if (name != Unbound)
			print_ascii_stream(stream, ", Macro");
		/* terpri */
		terpri_stream(stream);
	}

	return 0;
}


/*
 *  time
 */
_g void time_common(addr form, addr env, addr *ret)
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

	getcdr(form, &args1);
	if (! consp(args1))
		goto error;
	GetCons(args1, &expr, &args1);
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
	return;

error:
	fmte("Macro TIME ~S must be a (time form).", form, NULL);
	*ret = Nil;
}


/*
 *  room
 */
static void room_output_common(Execute ptr, addr stream)
{
	size_t size, object, space, percent;
	addr pos;

	/* heap memory */
	size = get_heap_size();
	object = get_heap_object();
	pos = intsizeh(size);
	format_stream(ptr, stream, "Heap Size:~20T~A~40T[byte]~%", pos, NULL);
	pos = intsizeh(object);
	format_stream(ptr, stream, "Object memory:~20T~A~40T[byte]~%", pos, NULL);
	pos = intsizeh(get_heap_count());
	format_stream(ptr, stream, "Object count:~20T~A~40T[object]~%", pos, NULL);
	pos = intsizeh(get_heap_gc_count());
	format_stream(ptr, stream, "GC count:~20T~A~40T[times]~%", pos, NULL);

	/* free space */
	space = size - object;
	pos = intsizeh(space);
	format_stream(ptr, stream, "Free space:~20T~A~40T[byte]~%", pos, NULL);

	/* percent */
	percent = (space >> 16) * 100 / (size >> 16);
	pos = intsizeh(percent);
	format_stream(ptr, stream, "Free percent:~20T~A~40T[percent]~%", pos, NULL);
}

static void room_default_common(Execute ptr, addr stream)
{
	format_stream(ptr, stream, "Room default output.~%", NULL);
	room_output_common(ptr, stream);
}

static void room_minimal_common(Execute ptr, addr stream)
{
	format_stream(ptr, stream, "Room minimal output.~%", NULL);
	room_output_common(ptr, stream);
}

static void room_maximal_common(Execute ptr, addr stream)
{
	format_stream(ptr, stream, "Room maximal output.~%", NULL);
	room_output_common(ptr, stream);
}

_g void room_common(Execute ptr, addr var)
{
	addr stream, check;

	standard_output_stream(ptr, &stream);
	fresh_line_stream(stream);
	if (var == Unbound) {
		room_default_common(ptr, stream);
		return;
	}
	if (var == Nil) {
		room_minimal_common(ptr, stream);
		return;
	}
	if (var == T) {
		room_maximal_common(ptr, stream);
		return;
	}
	GetConst(KEYWORD_DEFAULT, &check);
	if (var == check) {
		room_default_common(ptr, stream);
		return;
	}

	/* error */
	fmte("Invalid ROOM argument ~S.", var, NULL);
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
	/* nil */
	if (var == Nil)
		return ed_execute_common(ptr, Nil);

	/* argument */
	pathname_designer_heap(ptr, var, &var);
	if (wild_pathname_boolean(var, Nil)) {
		simple_file_error_stdarg(var,
				"~ED can't use wild card pathname ~S.", var, NULL);
		return 0;
	}
	physical_pathname_local(ptr, var, &var);
	namestring_pathname(ptr, &var, var);
	return ed_execute_common(ptr, var);
}

static void ed_function_lambda(addr symbol, addr *ret)
{
	addr pos;

	parse_callname_error(&pos, symbol);
	getglobal_parse_callname(pos, &pos);
	if (pos == Unbound)
		goto error;
	getdefunform_function(pos, &pos);
	if (pos == Unbound)
		goto error;
	*ret = pos;
	return;

error:
	fmte("Cannot edit ~S function.", symbol, NULL);
	*ret = Nil;
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
	open_stream(ptr, &file, file,
			Stream_Open_Direction_Output,
			Stream_Open_Element_Character,
			Stream_Open_IfExists_Supersede,
			Stream_Open_IfDoesNot_Create,
			Stream_Open_External_Default);
	right_margin_print(ptr, file, &width);
	push_right_margin_print(ptr, width);
	Return(prin1_print(ptr, file, lambda));
	close_stream(file);

	return free_control_(ptr, control);
}

static int ed_function_common(Execute ptr, addr symbol)
{
	int result;
	addr file, lambda;

	ed_function_lambda(symbol, &lambda);
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
static void dribble_message_begin(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	pathname_designer_heap(ptr, file, &name);
	standard_output_stream(ptr, &stream);
	str = "~&;; DRIBBLE begin to write ~S.~%";
	Error(format_stream(ptr, stream, str, name, NULL));
}

static void dribble_message_end(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	pathname_designer_heap(ptr, file, &name);
	standard_output_stream(ptr, &stream);
	str = "~&;; DRIBBLE end to write ~S.~%";
	Error(format_stream(ptr, stream, str, name, NULL));
}

static void dribble_set_stream(addr file)
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
	exitpoint_stream(output);
	force_output_stream(output);
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
}

static void dribble_open_file(Execute ptr, addr file)
{
	physical_pathname_heap(ptr, file, &file);
	if (wild_pathname_boolean(file, Nil)) {
		simple_file_error_stdarg(file,
				"~DRIBBLE can't use wild card pathname ~S.", file, NULL);
		return;
	}
	open_stream(ptr, &file, file,
			Stream_Open_Direction_Output,
			Stream_Open_Element_Character,
			Stream_Open_IfExists_Supersede,
			Stream_Open_IfDoesNot_Create,
			Stream_Open_External_Default);
	dribble_set_stream(file);
	dribble_message_begin(ptr, file);
}

static void dribble_open(Execute ptr, addr file)
{
	addr check;

	GetConst(SYSTEM_DRIBBLE_FILE, &check);
	GetValueSymbol(check, &check);
	if (check != Unbound) {
		pathname_designer_heap(ptr, check, &check);
		fmtw("DRIBBLE already open file ~S.", check, NULL);
	}
	else {
		dribble_open_file(ptr, file);
	}
}

static void dribble_close_stream(Execute ptr)
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
	dribble_message_end(ptr, file);
	SetValueSymbol(dfile, Unbound);
	SetValueSymbol(dinput, Unbound);
	SetValueSymbol(doutput, Unbound);
	SetValueSymbol(decho, Unbound);
	SetValueSymbol(dbroadcast, Unbound);
	SetValueSymbol(sinput, input);
	SetValueSymbol(soutput, output);
	close_stream(echo);
	close_stream(broadcast);
	close_stream(file);
}

static void dribble_close(Execute ptr)
{
	addr symbol, file;

	GetConst(SYSTEM_DRIBBLE_FILE, &symbol);
	GetValueSymbol(symbol, &file);
	if (file != Unbound)
		dribble_close_stream(ptr);
}

_g void dribble_common(Execute ptr, addr file)
{
	if (file != Nil)
		dribble_open(ptr, file);
	else
		dribble_close(ptr);
}

