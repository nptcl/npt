#ifdef LISP_DEGRADE
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "eval.h"
#include "file.h"
#include "format.h"
#include "lisp.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"

/*
 *  Main
 */
static int rt_execute(Execute ptr, addr stream)
{
	addr pos;

	/* test execute */
	push_toplevel_eval(ptr, T);
	push_evalwhen_load(ptr);
	eval_stream(ptr, stream);
	close_stream(stream);
	/* result check */
	getresult_control(ptr, &pos);

	return pos != T;
}

static int rt_load(Execute ptr, addr file)
{
	int check;
	addr path, stream;

	/* load name */
	check = open_input_stream(ptr, &stream, file);
	if (! check)
		return rt_execute(ptr, stream);

	/* load "test/" name */
	parse_pathname_char_heap(ptr, "test/", &path);
	merge_pathnames_clang(ptr, file, path, Unbound, &file);
	name_pathname_heap(ptr, file, &file);
	open_input_stream_error(ptr, &stream, file); /* force */

	return rt_execute(ptr, stream);
}

static int load_rt_init(Execute ptr, const char *name)
{
	addr package, symbol, use, file;

	/* title */
	strvect_char_heap(&file, name);
	fmts(T, "~&[~A]~%", file, NULL);

	/* (let ((*package* (find-package 'common-lisp-user))) ...) */
	find_char_package(LISP_COMMON_USER, &package);
	GetConst(SPECIAL_PACKAGE, &symbol);
	pushspecial_control(ptr, symbol, package);
	/* (use-package 'lisp-rt *package*) */
	find_char_package(LISP_RT, &use);
	use_package(package, use);
	/* load-rt */
	parse_pathname_char_heap(ptr, name, &file);
	return rt_load(ptr, file);
}

static int load_rt_execute(Execute ptr, const char *name)
{
	int result;
	addr control;
	codejump jump;

	result = 1;
	begin_switch(ptr, &jump);
	push_close_control(ptr, &control);
	if (codejump_run_p(&jump)) {
		handler_warning(ptr);
		result = load_rt_init(ptr, name);
	}
	end_switch(&jump);
	free_control(ptr, control);
	throw_switch(&jump);

	return result;
}

static void load_rt_nickname(const char *str1, const char *str2)
{
	addr name1, name2;

	strvect_char_heap(&name1, str1);
	strvect_char_heap(&name2, str2);
	conscar_heap(&name2, name2);
	rename_package(name1, name1, name2, &name1);
}

static void load_rt_nicknames(void)
{
	load_rt_nickname(LISP_SYSTEM, "LISP-SYSTEM");
	load_rt_nickname(LISP_USER, "LISP-USER");
	load_rt_nickname(LISP_CLOS, "LISP-CLOS");
	load_rt_nickname(LISP_RT, "LISP-RT");
}

int load_rt_lisp(const char *name)
{
	int result;
	lispcode code;
	Execute ptr;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	result = 1;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		buildlisp(ptr);
		load_rt_nicknames();
		result = load_rt_execute(ptr, name);
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));

	return result;
}

#define load_rt_file(x) { \
	if (load_rt_lisp(x)) return 1; \
}

int load_rt(void)
{
	TITLE;

#if 0
#endif
	/* 3. Evaluation and Compilation */
	load_rt_file("rt-eval.lisp");
	/* 4. Types and Classes */
	load_rt_file("rt-types.lisp");
	/* 5. Data and Control Flow */
	load_rt_file("rt-data.lisp");
	/* 6. Iteration */
	load_rt_file("rt-iteration.lisp");
	/* 9. Conditions */
	load_rt_file("rt-conditions.lisp");
	/* 11. Packages */
	load_rt_file("rt-packages.lisp");
	/* 12. Number */
	load_rt_file("rt-numbers.lisp");
	/* 13. Characters */
	load_rt_file("rt-character.lisp");
	/* 14. Conses */
	load_rt_file("rt-conses.lisp");
	/* 15. Arrays */
	load_rt_file("rt-arrays.lisp");
	/* 16. Strings */
	load_rt_file("rt-strings.lisp");
	/* 17. Sequences */
	load_rt_file("rt-sequences.lisp");
	/* 18. Hash Tables */
	load_rt_file("rt-hashtables.lisp");
	/* 19. Filenames */
	load_rt_file("rt-filenames.lisp");
	/* 20. Files */
	load_rt_file("rt-files.lisp");
	/* 21. Streams */
	load_rt_file("rt-streams.lisp");
	/* 22. Printer */
	load_rt_file("rt-printer.lisp");
#if 0
#endif

	return 0;
}
#else
int load_rt(void)
{
	return 1;
}
#endif

