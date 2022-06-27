#include "call_filenames.h"
#include "call_environment.h"
#include "callname.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "env_time.h"
#include "eval_load.h"
#include "format.h"
#include "function.h"
#include "heap.h"
#include "heap_memory.h"
#include "hold.h"
#include "integer.h"
#include "package.h"
#include "package_common.h"
#include "package_object.h"
#include "pathname_wildcard.h"
#include "pathname.h"
#include "print.h"
#include "print_write.h"
#include "real_truncate.h"
#include "sort.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_common.h"
#include "stream_echo.h"
#include "stream_function.h"
#include "stream_open.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"


/*
 *  decode-universal-time
 */
int decode_universal_time_common_(LocalRoot local, addr pos, addr zone,
		addr *rsecond, addr *rminute, addr *rhour,
		addr *rdate, addr *rmonth, addr *ryear,
		addr *rweek, addr *rdaylight, addr *rzone)
{
	struct universal_time_struct u;

	if (zone == Unbound)
		zone = Nil;
	Return(decode_universal_time_call_(local, &u, pos, zone));
	*rsecond = u.second;
	*rminute = u.minute;
	*rhour = u.hour;
	*rdate = u.date;
	*rmonth = u.month;
	*ryear = u.year;
	*rweek = u.week;
	*rdaylight = u.daylight_p;
	*rzone = u.zone;

	return 0;
}


/*
 *  encode-universal-time
 */
int encode_universal_time_common_(LocalRoot local, addr rest, addr *ret)
{
	addr s, mi, h, d, m, y, z;

	if (! consp_getcons(rest, &s, &rest))
		goto error;
	if (! consp_getcons(rest, &mi, &rest))
		goto error;
	if (! consp_getcons(rest, &h, &rest))
		goto error;
	if (! consp_getcons(rest, &d, &rest))
		goto error;
	if (! consp_getcons(rest, &m, &rest))
		goto error;
	if (! consp_getcons(rest, &y, &rest))
		goto error;
	if (! consp_getcons(rest, &z, &rest))
		z = Unbound;
	if (consp(rest))
		goto error;
	return encode_universal_time_call_(local, ret, s, mi, h, d, m, y, z);

error:
	return fmte_("Invalid argument ENCODE-UNIVERSAL-TIME.", NULL);
}


/*
 *  get-decoded-time
 */
int get_decoded_time_common_(LocalRoot local,
		addr *rsecond, addr *rminute, addr *rhour,
		addr *rdate, addr *rmonth, addr *ryear,
		addr *rweek, addr *rdaylight, addr *rzone)
{
	struct universal_time_struct u;

	Return(get_decoded_time_call_(local, &u));
	*rsecond = u.second;
	*rminute = u.minute;
	*rhour = u.hour;
	*rdate = u.date;
	*rmonth = u.month;
	*ryear = u.year;
	*rweek = u.week;
	*rdaylight = u.daylight_p;
	*rzone = u.zone;

	return 0;
}



/*
 *  apropos-list
 */
static int list_all_packages_sort_(Execute ptr, addr *ret)
{
	addr list, key, call;
	LocalHold hold;

	Return(list_all_packages_(&list));
	hold = LocalHold_local_push(ptr, list);
	/* key */
	GetConst(COMMON_PACKAGE_NAME, &key);
	Return(getfunction_global_(key, &key));
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	Return(getfunction_global_(call, &call));
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

	Check(! stringp(var), "type error");
	Check(! stringp(name), "type error");
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

static int apropos_symbol_common_(Execute ptr, addr var, addr package, addr *ret)
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
	Return(getfunction_global_(key, &key));
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	Return(getfunction_global_(call, &call));
	localhold_push(hold, call);
	/* sort */
	Return(quick_sort_sequence_(ptr, list, call, key));
	localhold_end(hold);

	return Result(ret, list);
}

int apropos_list_common_(Execute ptr, addr var, addr package, addr *ret)
{
	addr list, root, x, y;
	LocalHold hold;

	Return(string_designator_heap_(&var, var, NULL));
	if (package != Nil)
		return apropos_symbol_common_(ptr, var, package, ret);

	/* list-all-packages */
	hold = LocalHold_array(ptr, 1);
	Return(list_all_packages_sort_(ptr, &list));
	localhold_push(hold, list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &package, &list);
		Return(apropos_symbol_common_(ptr, var, package, &x));
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
int apropos_common_(Execute ptr, addr var, addr package)
{
	addr stream, list, name;

	Return(standard_output_stream_(ptr, &stream));
	Return(apropos_list_common_(ptr, var, package, &list));
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
int time_common_(addr form, addr env, addr *ret)
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
static int room_output_common_(Execute ptr, addr stream)
{
	size_t size, object, space, percent;
	addr pos;

	/* heap memory */
	size = get_heap_size();
	object = get_heap_object();
	pos = intsizeh(size);
	Return(format_stream_(ptr, stream, "Heap Size:~20T~A~40T[byte]~%", pos, NULL));
	pos = intsizeh(object);
	Return(format_stream_(ptr, stream, "Object memory:~20T~A~40T[byte]~%", pos, NULL));
	pos = intsizeh(get_heap_count());
	Return(format_stream_(ptr, stream, "Object count:~20T~A~40T[object]~%", pos, NULL));
	pos = intsizeh(get_heap_gc_count());
	Return(format_stream_(ptr, stream, "GC count:~20T~A~40T[times]~%", pos, NULL));

	/* free space */
	space = size - object;
	pos = intsizeh(space);
	Return(format_stream_(ptr, stream, "Free space:~20T~A~40T[byte]~%", pos, NULL));

	/* percent */
	percent = (space >> 16) * 100 / (size >> 16);
	pos = intsizeh(percent);
	Return(format_stream_(ptr, stream, "Free percent:~20T~A~40T[percent]~%", pos, NULL));

	return 0;
}

static int room_default_common_(Execute ptr, addr stream)
{
	Return(format_stream_(ptr, stream, "Room default output.~%", NULL));
	return room_output_common_(ptr, stream);
}

static int room_minimal_common_(Execute ptr, addr stream)
{
	Return(format_stream_(ptr, stream, "Room minimal output.~%", NULL));
	return room_output_common_(ptr, stream);
}

static int room_maximal_common_(Execute ptr, addr stream)
{
	Return(format_stream_(ptr, stream, "Room maximal output.~%", NULL));
	return room_output_common_(ptr, stream);
}

int room_common_(Execute ptr, addr var)
{
	addr stream, check;

	Return(standard_output_stream_(ptr, &stream));
	Return(fresh_line_stream_(stream, NULL));
	if (var == Unbound)
		return room_default_common_(ptr, stream);
	if (var == Nil)
		return room_minimal_common_(ptr, stream);
	if (var == T)
		return room_maximal_common_(ptr, stream);
	GetConst(KEYWORD_DEFAULT, &check);
	if (var == check)
		return room_default_common_(ptr, stream);

	/* error */
	return fmte_("Invalid ROOM argument ~S.", var, NULL);
}


/*
 *  ed
 */
static int ed_execute_common_(Execute ptr, addr file)
{
	addr call;

	GetConst(SYSTEM_ED_FUNCTION, &call);
	Return(getspecialcheck_local_(ptr, call, &call));
	return funcall_control_(ptr, call, file, NULL);
}

static int ed_file_common_(Execute ptr, addr var)
{
	int check;

	/* nil */
	if (var == Nil)
		return ed_execute_common_(ptr, Nil);

	/* argument */
	Return(pathname_designator_heap_(ptr, var, &var));
	Return(wild_pathname_boolean_(var, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, var,
				"~ED can't use wild card pathname ~S.", var, NULL);
	}
	Return(physical_pathname_local_(ptr, var, &var));
	Return(namestring_common_(ptr, &var, var));
	return ed_execute_common_(ptr, var);
}

static int ed_function_lambda_(addr symbol, addr *ret)
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

static int ed_function_write_call_(Execute ptr, addr file, addr lambda)
{
	enum Stream_Open_External external;
	size_t width;

	Return(open_external_format_(ptr, Unbound, &external));
	if (external == Stream_Open_External_Error)
		return fmte_("Invalid external-format ~S.", NULL);
	Return(open_stream_(ptr, &file, file,
				Stream_Open_Direction_Output,
				Stream_Open_Element_Character,
				Stream_Open_IfExists_Supersede,
				Stream_Open_IfDoesNot_Create,
				external));
	Return(right_margin_print_(ptr, file, &width));
	Return(push_right_margin_print_(ptr, width));
	Return(prin1_print_(ptr, file, lambda));
	Return(close_stream_(file, NULL));

	return 0;
}

static int ed_function_write_(Execute ptr, addr file, addr lambda)
{
	addr control;

	push_control(ptr, &control);
	(void)ed_function_write_call_(ptr, file, lambda);
	return pop_control_(ptr, control);
}

static int ed_function_common_(Execute ptr, addr symbol)
{
	int result;
	addr file, lambda;

	Return(ed_function_lambda_(symbol, &lambda));
	ed_function_name(ptr, &file);
	Return(ed_function_write_(ptr, file, lambda));
	Return(ed_file_common_(ptr, file));
	Return(eval_load_(ptr, &result, file, Unbound, Unbound, 1, Unbound));

	return 0;
}

int ed_common_(Execute ptr, addr var)
{
	if (var == Unbound)
		var = Nil;
	if (var == Nil)
		return ed_file_common_(ptr, Nil);
	else if (function_name_p(var))
		return ed_function_common_(ptr, var);
	else
		return ed_file_common_(ptr, var);
}


/*
 *  dribble
 */
static int dribble_message_begin_(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	Return(pathname_designator_heap_(ptr, file, &name));
	Return(standard_output_stream_(ptr, &stream));
	str = "~&;; DRIBBLE begin to write ~S.~%";
	return format_stream_(ptr, stream, str, name, NULL);
}

static int dribble_message_end_(Execute ptr, addr file)
{
	const char *str;
	addr name, stream;

	Return(pathname_designator_heap_(ptr, file, &name));
	Return(standard_output_stream_(ptr, &stream));
	str = "~&;; DRIBBLE end to write ~S.~%";
	return format_stream_(ptr, stream, str, name, NULL);
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
	Return(open_broadcast_stream_(&broadcast, broadcast));

	/* variable */
	Return(setvalue_symbol_(dfile, file));
	Return(setvalue_symbol_(dinput, input));
	Return(setvalue_symbol_(doutput, output));
	Return(setvalue_symbol_(decho, echo));
	Return(setvalue_symbol_(dbroadcast, broadcast));
	Return(setvalue_symbol_(sinput, echo));
	Return(setvalue_symbol_(soutput, broadcast));

	return 0;
}

static int dribble_open_file_(Execute ptr, addr file)
{
	enum Stream_Open_External external;
	int check;

	Return(physical_pathname_heap_(ptr, file, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, file,
				"~DRIBBLE can't use wild card pathname ~S.", file, NULL);
	}
	Return(open_external_format_(ptr, Unbound, &external));
	if (external == Stream_Open_External_Error)
		return fmte_("Invalid external-format.", NULL);
	Return(open_stream_(ptr, &file, file,
				Stream_Open_Direction_Output,
				Stream_Open_Element_Character,
				Stream_Open_IfExists_Supersede,
				Stream_Open_IfDoesNot_Create,
				external));
	Return(dribble_set_stream_(file));
	return dribble_message_begin_(ptr, file);
}

static int dribble_open_(Execute ptr, addr file)
{
	addr check;

	GetConst(SYSTEM_DRIBBLE_FILE, &check);
	GetValueSymbol(check, &check);
	if (check != Unbound) {
		Return(pathname_designator_heap_(ptr, check, &check));
		return fmtw_("DRIBBLE already open file ~S.", check, NULL);
	}
	else {
		return dribble_open_file_(ptr, file);
	}
}

static int dribble_close_stream_(Execute ptr)
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
	Return(dribble_message_end_(ptr, file));
	Return(setvalue_symbol_(dfile, Unbound));
	Return(setvalue_symbol_(dinput, Unbound));
	Return(setvalue_symbol_(doutput, Unbound));
	Return(setvalue_symbol_(decho, Unbound));
	Return(setvalue_symbol_(dbroadcast, Unbound));
	Return(setvalue_symbol_(sinput, input));
	Return(setvalue_symbol_(soutput, output));
	Return(close_stream_(echo, NULL));
	Return(close_stream_(broadcast, NULL));
	Return(close_stream_(file, NULL));

	return 0;
}

static int dribble_close_(Execute ptr)
{
	addr symbol, file;

	GetConst(SYSTEM_DRIBBLE_FILE, &symbol);
	GetValueSymbol(symbol, &file);
	if (file != Unbound) {
		Return(dribble_close_stream_(ptr));
	}

	return 0;
}

int dribble_common_(Execute ptr, addr file)
{
	if (file != Nil)
		return dribble_open_(ptr, file);
	else
		return dribble_close_(ptr);
}

