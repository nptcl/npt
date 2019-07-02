#ifndef __LISP_ARGV_HEADER__
#define __LISP_ARGV_HEADER__

#include <stddef.h>
#include "define.h"
#include "main_string.h"

enum lispargv_execute {
	lispargv_load,
	lispargv_eval,
	lispargv_script,
	lispargv_minusminus
};

struct lispargv_string {
	enum lispargv_execute type;
	lispstringu value;
};

struct lispargv_input {
	struct lispargv_string *data;
	size_t size;
};

struct lispargv {
	/* mode */
	unsigned mode_help : 1;
	unsigned mode_version : 1;
	unsigned mode_core : 1;
	unsigned mode_standalone : 1;
	unsigned mode_degrade : 1;
	/* args */
	unsigned version_script : 1;
	unsigned nocore : 1;
	unsigned noinit : 1;
	unsigned debugger : 1;
	unsigned interactive : 1;
	unsigned interactive_p : 1;
	unsigned quit : 1;
	size_t heap, local, index, start;
	lispstringu core;
	lispstringu init;
	lisparrayu argv;
	lisptableu env;
	struct lispargv_input *input;
};

void free_lispargv(struct lispargv *ptr);
struct lispargv *lispargv_main(int argc, char *argv[], char *env[]);
struct lispargv *lispargv_main_force(int argc, char *argv[], char *env[]);
#ifdef LISP_WINMAIN_WIDE
struct lispargv *lispargv_windows(void);
#endif

#endif

