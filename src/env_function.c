#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "env_function.h"
#include "gc.h"
#include "package.h"
#include "sort.h"
#include "stream.h"
#include "strtype.h"
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
	getfunctioncheck_local(ptr, key, &key);
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	getfunctioncheck_local(ptr, call, &call);
	localhold_push(hold, call);
	/* sort */
	Return1(quick_sort_sequence(ptr, list, call, key));
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
	getfunctioncheck_local(ptr, key, &key);
	localhold_push(hold, key);
	/* call */
	GetConst(COMMON_STRING_LESS, &call);
	getfunctioncheck_local(ptr, call, &call);
	localhold_push(hold, call);
	/* sort */
	Return1(quick_sort_sequence(ptr, list, call, key));
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
	Return1(list_all_packages_sort(ptr, &list));
	localhold_push(hold, list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &package, &list);
		Return1(apropos_symbol_common(ptr, var, package, &x));
		while (x != Nil) {
			GetCons(x, &y, &x);
			pushnew_heap(root, y, &root);
			localhold_set(hold, 0, root);
		}
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);

	return 0;
}


/*
 *  apropos
 */
_g int apropos_common(Execute ptr, addr var, addr package)
{
	addr stream, list, name;

	standard_output_stream(ptr, &stream);
	Return1(apropos_list_common(ptr, var, package, &list));
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
_g void time_common(Execute ptr, addr form, addr env, addr *ret)
{
	/* (multiple-value-bind (real1 run1 space1 count1) (lisp-system::timeinfo)
	 *   (multiple-value-bind (real2 run2 space2 count2) (lisp-system::timeinfo)
	 *     (fresh-line)
	 *     (format *trace-output* "Result~15T~S~%" form)
	 *     (format *trace-output* "Real-Time~15T~A~%" (- real2 real1))
	 *     (format *trace-output* "Run-Time~15T~A~%" (- run2 run1))
	 *     (format *trace-output* "Heap-Space~15T~A~%" (- space2 space1))
	 *     (format *trace-output* "Heap-Count~15T~A~%" (- count2 count1))))
	 */
	addr expr, mvbind, timeinfo, fresh, format, trace, minus;
	addr space1, space2, count1, count2, real1, real2, run1, run2;
	addr str1, str2, str3, str4, str5;
	addr args1, args2;

	getcdr(form, &args1);
	if (! consp(args1))
		goto error;
	GetCons(args1, &expr, &args1);
	if (args1 != Nil)
		goto error;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_TIMEINFO, &timeinfo);
	GetConst(COMMON_FRESH_LINE, &fresh);
	GetConst(COMMON_FORMAT, &format);
	GetConst(SPECIAL_TRACE_OUTPUT, &trace);
	GetConst(COMMON_MINUS, &minus);
	strvect_char_heap(&str1, "Result~15T~S~%");
	strvect_char_heap(&str2, "Real-Time~15T~A~%");
	strvect_char_heap(&str3, "Run-Time~15T~A~%");
	strvect_char_heap(&str4, "Heap-Space~15T~A~%");
	strvect_char_heap(&str5, "Heap-Count~15T~A~%");
	make_symbolchar(&real1, "REAL1");
	make_symbolchar(&real2, "REAL2");
	make_symbolchar(&run1, "RUN1");
	make_symbolchar(&run2, "RUN2");
	make_symbolchar(&space1, "SPACE1");
	make_symbolchar(&space2, "SPACE2");
	make_symbolchar(&count1, "COUNT1");
	make_symbolchar(&count2, "COUNT2");
	list_heap(&args1, real1, run1, space1, count1, NULL);
	list_heap(&args2, real2, run2, space2, count2, NULL);
	list_heap(&real1, minus, real2, real1, NULL);
	list_heap(&run1, minus, run2, run1, NULL);
	list_heap(&space1, minus, space2, space1, NULL);
	list_heap(&count1, minus, count2, count1, NULL);
	list_heap(&expr, format, trace, str1, expr, NULL);
	list_heap(&real1, format, trace, str2, real1, NULL);
	list_heap(&run1, format, trace, str3, run1, NULL);
	list_heap(&space1, format, trace, str4, space1, NULL);
	list_heap(&count1, format, trace, str5, count1, NULL);
	list_heap(&timeinfo, timeinfo, NULL);
	list_heap(&fresh, fresh, NULL);
	list_heap(&args2, mvbind, args2, timeinfo,
			fresh, expr, real1, run1, space1, count1, NULL);
	list_heap(ret, mvbind, args1, timeinfo, args2, NULL);
	return;

error:
	fmte("Macro TIME ~S must be a (time form).", form, NULL);
	*ret = Nil;
}

