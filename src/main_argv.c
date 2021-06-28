#include <stdlib.h>
#include "alloc.h"
#include "extern_init.h"
#include "main_argv.h"
#include "main_string.h"

#ifdef LISP_WINMAIN_WIDE
#include <windows.h>
#endif

/*
 *  constant
 */
#define LispArgv_MinusMinus     "--"
#define LispArgv_Help           "--help"
#define LispArgv_Version        "--version"
#define LispArgv_VersionScript  "--version-script"
#define LispArgv_Core           "--core"
#define LispArgv_Standalone     "--standalone"
#define LispArgv_Build          "--build"
#define LispArgv_Degrade        "--degrade"
#define LispArgv_Heap           "--heap"
#define LispArgv_Local          "--local"
#define LispArgv_Corefile       "--corefile"
#define LispArgv_Initfile       "--initfile"
#define LispArgv_Nocore         "--nocore"
#define LispArgv_Noinit         "--noinit"
#define LispArgv_Debugger       "--debugger"
#define LispArgv_Nodebugger     "--nodebugger"
#define LispArgv_Quit           "--quit"
#define LispArgv_Script         "--script"
#define LispArgv_Load           "--load"
#define LispArgv_Eval           "--eval"
#define LispArgv_Bright         "--bright"
#define LispArgv_Dark           "--dark"
#define LispArgv_Color          "--color"
#define LispArgv_Monochrome     "--monochrome"
#define LispArgv_equal(a,b) equalchar_stringu((a), LispArgv_##b)


/*
 *  inputs
 */
static int mainparse_inputs_string(lispstringu s, enum lispargv_execute *ret)
{
	if (LispArgv_equal(s, Load)) {
		*ret = lispargv_load;
		return 1;
	}
	if (LispArgv_equal(s, Eval)) {
		*ret = lispargv_eval;
		return 1;
	}
	if (LispArgv_equal(s, Script)) {
		*ret = lispargv_script;
		return 1;
	}
	if (LispArgv_equal(s, MinusMinus)) {
		*ret = lispargv_minusminus;
		return 1;
	}

	return 0;
}

static int mainparse_inputs_length(lisparrayu argv, size_t index, size_t *ret)
{
	enum lispargv_execute type;
	lispstringu s, *ptr;
	size_t allsize, size;

	allsize = argv->size;
	ptr = argv->ptr;
	for (size = 0; index < allsize; size++) {
		s = ptr[index++];
		if (! mainparse_inputs_string(s, &type)) {
			lisperror_noeol("Invalid argument.");
			output_stringu(s, lisperror_stream());
			lisperror(".");
			return 1;
		}
		if (type == lispargv_minusminus)
			break;
		if (allsize <= index) {
			lisperror_noeol("After ");
			output_stringu(s, lisperror_stream());
			lisperror(" must be at least one argument.");
			return 1;
		}
		index++;
	}
	*ret = size;

	return 0;
}

static int mainparse_inputs_copy(struct lispargv_input *input,
		lisparrayu argv, size_t index, size_t *ret)
{
	enum lispargv_execute type;
	lispstringu s, *ptr;
	struct lispargv_string *data;
	size_t allsize, size;

	data = input->data;
	allsize = argv->size;
	ptr = argv->ptr;
	for (size = 0; index < allsize; size++) {
		s = ptr[index];
		if (! mainparse_inputs_string(s, &type))
			return 1;
		if (type == lispargv_minusminus) {
			index++;
			break;
		}
		index++; /* key */
		if (allsize <= index)
			return 1;
		s = copy_stringu(ptr[index]);
		index++; /* value */
		if (s == NULL)
			return 1;
		data[size].type = type;
		data[size].value = s;
	}
	*ret = index;

	return 0;
}

static struct lispargv_input *make_lispargv_input(size_t size)
{
	struct lispargv_string *data;
	struct lispargv_input *ptr;

	ptr = (struct lispargv_input *)malloc(sizeoft(struct lispargv_input));
	if (ptr == NULL)
		return NULL;
	data = (struct lispargv_string *)malloc(sizeoft(struct lispargv_string) * size);
	if (data == NULL) {
		free(ptr);
		return NULL;
	}
	memset(data, 0, sizeoft(struct lispargv_string) * size);
	ptr->data = data;
	ptr->size = size;

	return ptr;
}

static int mainparse_inputs(struct lispargv *ptr)
{
	lisparrayu argv;
	struct lispargv_input *input;
	size_t index, size;

	argv = ptr->argv;
	index = ptr->index;
	/* length */
	if (mainparse_inputs_length(argv, index, &size))
		return 1;
	/* make input */
	input = make_lispargv_input(size);
	if (input == NULL)
		return 1;
	ptr->input = input;
	/* copy input */
	if (mainparse_inputs_copy(input, argv, index, &size))
		return 1;
	/* copy command */
	if (argv->size < size)
		return 1;
	ptr->start = size;

	return 0;
}


/*
 *  parse-arguments
 */
static int lispargs_heap(struct lispargv *ptr, size_t index)
{
	lisparrayu a;
	size_t size;

	a = ptr->argv;
	if (a->size <= index - 1UL) {
		lisperror("After --heap must be at least one argument.");
		return 1;
	}
	if (getsize_stringu(a->ptr[index + 1UL], &size)) {
		lisperror("Memory size format error.");
		return 1;
	}
	if (size < 1024UL * 1024UL) {
		lisperror("Memory size must be at least 1MByte.");
		return 1;
	}
	ptr->heap = size;

	return 0;
}

static int lispargs_local(struct lispargv *ptr, size_t index)
{
	lisparrayu a;
	size_t size;

	a = ptr->argv;
	if (a->size <= index - 1UL) {
		lisperror("After --local must be at least one argument.");
		return 1;
	}
	if (getsize_stringu(a->ptr[index + 1UL], &size)) {
		lisperror("Stack size format error.");
		return 1;
	}
	if (size < 1024UL * 1024UL) {
		lisperror("Stack size must be at least 1MByte.");
		return 1;
	}
	ptr->local = size;

	return 0;
}

static int lispargs_corefile(struct lispargv *ptr, size_t index)
{
	lispstringu s;
	lisparrayu a;

	a = ptr->argv;
	if (a->size <= index - 1UL) {
		lisperror("After --corefile must be at least one argument.");
		return 1;
	}
	if (ptr->mode_standalone) {
		lisperror("Cannot load a core file in standalone mode.");
		return 1;
	}
	s = copy_stringu(a->ptr[index + 1UL]);
	if (s == NULL) {
		lisperror("copy_string error.");
		return 1;
	}
	ptr->core = s;

	return 0;
}

static int lispargs_initfile(struct lispargv *ptr, size_t index)
{
	lispstringu s;
	lisparrayu a;

	a = ptr->argv;
	if (a->size <= index - 1UL) {
		lisperror("After --initfile must be at least one argument.");
		return 1;
	}
	s = copy_stringu(a->ptr[index + 1UL]);
	if (s == NULL) {
		lisperror("copy_string error.");
		return 1;
	}
	ptr->init = s;

	return 0;
}


/*
 *  mode
 */
enum LispMode {
	LispMode_Core,
	LispMode_Standalone,
	LispMode_Degrade,
	LispMode_Help,
	LispMode_Version,
	LispMode_Size
};

static void mainparse_mode(struct lispargv *ptr, enum LispMode mode)
{
	ptr->mode_help = 0;
	ptr->mode_version = 0;
	ptr->mode_core = 0;
	ptr->mode_standalone = 0;
	ptr->mode_degrade = 0;
	switch (mode) {
		case LispMode_Help:
			ptr->mode_help = 1;
			break;

		case LispMode_Version:
			ptr->mode_version = 1;
			break;

		case LispMode_Degrade:
			ptr->mode_degrade = 1;
			ptr->nocore = 0;
			ptr->noinit = 0;
			ptr->debugger = 1;
			ptr->debuggerp = 0;
			ptr->quit = 0;
			break;

		case LispMode_Core:
			ptr->mode_core = 1;
			ptr->nocore = 0;
			ptr->noinit = 0;
			ptr->debugger = 1;
			ptr->debuggerp = 0;
			ptr->quit = 0;
			break;

		case LispMode_Standalone:
		default:
			ptr->mode_standalone = 1;
			ptr->nocore = 1;
			ptr->noinit = 1;
			ptr->debugger = 1;
			ptr->debuggerp = 0;
			ptr->quit = 0;
			break;
	}
}

static int mainparse_initmode(struct lispargv *ptr)
{
#ifdef LISP_MODE_CORE
	mainparse_mode(ptr, LispMode_Core);
#endif
#ifdef LISP_MODE_STANDALONE
	mainparse_mode(ptr, LispMode_Standalone);
#endif
#ifdef LISP_MODE_DEGRADE
	mainparse_mode(ptr, LispMode_Degrade);
	if (ptr->argv->size == 1)
		return 1;
#endif
	return 0;
}

static int mainparse_modecheck(struct lispargv *ptr)
{
	lisparrayu a;
	lispstringu s;

	a = ptr->argv;
	if (a->size != 2)
		return 0;
	s = a->ptr[1];
	if (LispArgv_equal(s, Degrade)) {
		mainparse_mode(ptr, LispMode_Degrade);
		return 1;
	}
	if (LispArgv_equal(s, Help)) {
		mainparse_mode(ptr, LispMode_Help);
		return 1;
	}
	if (LispArgv_equal(s, Version)) {
		mainparse_mode(ptr, LispMode_Version);
		return 1;
	}
	if (LispArgv_equal(s, VersionScript)) {
		mainparse_mode(ptr, LispMode_Version);
		ptr->version_script = 1;
		return 1;
	}

	return 0;
}


/*
 *  mainparse_loop
 */
static int mainparse_loop(struct lispargv *ptr)
{
	lisparrayu a;
	lispstringu s, *argv;
	size_t i, size;

	a = ptr->argv;
	argv = a->ptr;
	size = a->size;
	for (i = 1; i < size; i++) {
		s = argv[i];
		if (LispArgv_equal(s, Heap)) {
			if (lispargs_heap(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Local)) {
			if (lispargs_local(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Corefile)) {
			if (lispargs_corefile(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Initfile)) {
			if (lispargs_initfile(ptr, i++))
				return 1;
			continue;
		}
		if (LispArgv_equal(s, Nocore)) {
			ptr->nocore = 1;
			continue;
		}
		if (LispArgv_equal(s, Noinit)) {
			ptr->noinit = 1;
			continue;
		}
		if (LispArgv_equal(s, Debugger)) {
			ptr->debugger = 1;
			ptr->debuggerp = 1;
			continue;
		}
		if (LispArgv_equal(s, Nodebugger)) {
			ptr->debugger = 0;
			ptr->debuggerp = 1;
			continue;
		}
		if (LispArgv_equal(s, Quit)) {
			ptr->quit = 1;
			continue;
		}
		if (LispArgv_equal(s, Bright)) {
			ptr->terme_bright = 1;
			ptr->terme_dark = 0;
			continue;
		}
		if (LispArgv_equal(s, Dark)) {
			ptr->terme_bright = 0;
			ptr->terme_dark = 1;
			continue;
		}
		if (LispArgv_equal(s, Color)) {
			ptr->terme_color = 1;
			ptr->terme_monochrome = 0;
			continue;
		}
		if (LispArgv_equal(s, Monochrome)) {
			ptr->terme_color = 0;
			ptr->terme_monochrome = 1;
			continue;
		}
		if (LispArgv_equal(s, Script)) {
			if (ptr->debuggerp == 0) {
				ptr->debuggerp = 1;
				ptr->debugger = 0;
			}
			ptr->quit = 1;
			goto inputs;
		}
		if (LispArgv_equal(s, Load) || LispArgv_equal(s, Eval) ||
				LispArgv_equal(s, MinusMinus)) {
			goto inputs;
		}
		if (LispArgv_equal(s, Core)) {
			if (i == 1) {
				mainparse_mode(ptr, LispMode_Core);
				continue;
			}
			lisperror("--core must be a first argument.");
			return 1;
		}
		if (LispArgv_equal(s, Standalone) || LispArgv_equal(s, Build)) {
			if (i == 1) {
				mainparse_mode(ptr, LispMode_Standalone);
				continue;
			}
			lisperror("--standalone must be a first argument.");
			return 1;
		}
		if (LispArgv_equal(s, Degrade)) {
			lisperror("--degrade must be only one argument.");
			return 1;
		}
		if (LispArgv_equal(s, Help)) {
			lisperror("--help must be only one argument.");
			return 1;
		}
		if (LispArgv_equal(s, Version)) {
			lisperror("--version must be only one argument.");
			return 1;
		}
		if (LispArgv_equal(s, VersionScript)) {
			lisperror("--version-script must be only one argument.");
			return 1;
		}
		lisperror("Illegal argument.");
		return 1;
	}
inputs:
	ptr->index = i;

	return 0;
}


/*
 *  mainparse
 */
static int mainparse(struct lispargv *ptr)
{
	/* default mode */
	if (mainparse_initmode(ptr))
		return 0;
	/* first argument */
	if (mainparse_modecheck(ptr))
		return 0;
	/* loop */
	if (mainparse_loop(ptr))
		return 1;
	/* inputs */
	if (mainparse_inputs(ptr))
		return 1;

	return 0;
}


/*
 *  argv, env
 */
static int argv_argv_main(lisparrayu *ret, int argc, char *argv[])
{
	lisparrayu a;

	a = arrayu_argv_utf8(argc, (const byte *const *)argv);
	if (a == NULL)
		return 1;
	*ret = a;

	return 0;
}

#ifdef LISP_WINMAIN_WIDE
static int argv_argv_windows(lisparrayu *ret)
{
	int argc;
	LPWSTR *ptr;
	lisparrayu a;

	ptr = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (ptr == NULL)
		return 1;
	a = arrayu_argv_utf16(argc, (const byte16 *const *)ptr);
	if (a == NULL) {
		(void)LocalFree((HLOCAL)ptr);
		return 1;
	}
	if (LocalFree((HLOCAL)ptr)) {
		free_arrayu(a);
		return 1;
	}
	*ret = a;

	return 0;
}
#endif


/*
 *  environment
 */
static int argv_env_main(lisptableu *ret, char *env[])
{
	lisptableu a;

	if (env == NULL) {
		*ret = NULL;
		return 0;
	}
	a = tableu_env_main((const byte *const *)env);
	if (a == NULL)
		return 1;
	*ret = a;

	return 0;
}

#ifdef LISP_WINMAIN_WIDE
static int argv_env_windows(lisptableu *ret)
{
	LPWCH ptr = NULL;
	lisptableu a;

	ptr = GetEnvironmentStringsW();
	if (ptr == NULL)
		return 1;
	a = tableu_env_windows((const byte16 *)ptr);
	FreeEnvironmentStringsW(ptr);
	if (a == NULL)
		return 1;
	*ret = a;

	return 0;
}
#endif


/*
 *  lispargv
 */
static struct lispargv *make_lispargv(void)
{
	struct lispargv *ptr;

	ptr = (struct lispargv *)malloc(sizeoft(struct lispargv));
	if (ptr == NULL)
		return NULL;
	memset(ptr, 0, sizeoft(struct lispargv));

	return ptr;
}

static void lispargv_free_input(struct lispargv_input *ptr)
{
	size_t i, size;
	struct lispargv_string *data;

	if (ptr) {
		size = ptr->size;
		data = ptr->data;
		for (i = 0; i < size; i++)
			free_stringu(data[i].value);
		free(data);
		ptr->data = NULL;
		free(ptr);
	}
}

void lispargv_free(struct lispargv *ptr)
{
	if (ptr) {
		free_stringu(ptr->core);
		free_stringu(ptr->init);
		free_arrayu(ptr->argv);
		free_tableu(ptr->env);
		lispargv_free_input(ptr->input);
		free(ptr);
	}
}


/*
 *  interface
 */
struct lispargv *lispargv_main_force(int argc, char *argv[], char *env[])
{
	lisparrayu a;
	lisptableu e;
	struct lispargv *ptr;

	/* object */
	ptr = make_lispargv();
	if (ptr == NULL) {
		lisperror("make_lispargv error.");
		return NULL;
	}

	/* argv */
	if (argv_argv_main(&a, argc, argv)) {
		lisperror("parse argv error");
		goto error;
	}
	ptr->argv = a;

	/* env */
	if (argv_env_main(&e, env)) {
		lisperror("parse environment error");
		goto error;
	}
	ptr->env = e;

	/* parse */
	if (argc && mainparse(ptr))
		goto error;
	return ptr;

error:
	lispargv_free(ptr);
	return NULL;
}

#ifdef LISP_WINMAIN_WIDE
struct lispargv *lispargv_windows(void)
{
	lisparrayu a;
	lisptableu e;
	struct lispargv *ptr;

	/* object */
	ptr = make_lispargv();
	if (ptr == NULL) {
		lisperror("make_lispargv error.");
		return NULL;
	}

	/* argv */
	if (argv_argv_windows(&a)) {
		lisperror("parse argv error");
		goto error;
	}
	ptr->argv = a;

	/* env */
	if (argv_env_windows(&e)) {
		lisperror("parse environment error");
		goto error;
	}
	ptr->env = e;

	/* parse */
	if (mainparse(ptr))
		goto error;
	return ptr;

error:
	lispargv_free(ptr);
	return NULL;
}
#endif

struct lispargv *lispargv_main(int argc, char *argv[], char *env[])
{
#ifdef LISP_WINMAIN_WIDE
	return lispargv_windows();
#else
	return lispargv_main_force(argc, argv, env);
#endif
}

