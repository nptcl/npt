#include <stdarg.h>
#include "bignum.h"
#include "build.h"
#include "condition.h"
#include "condition_debugger.h"
#include "constant.h"
#include "control_object.h"
#include "core.h"
#include "define.h"
#include "eval_main.h"
#include "execute_object.h"
#include "extern_init.h"
#include "extern_string.h"
#include "file.h"
#include "file_open.h"
#include "hashtable.h"
#include "main_init.h"
#include "pathname.h"
#include "prompt.h"
#include "strvect.h"
#include "symbol.h"

int lisp_code = 0;
int lisp_result = 0;

_g FILE *lisperror_stream(void)
{
	return stderr;
}

_g int lisperror_noeol(const char *fmt, ...)
{
	int check;
	FILE *file;
	va_list args;

	file = lisperror_stream();
	va_start(args, fmt);
	check = vfprintf(file, fmt, args);
	va_end(args);
	fflush(file);

	return check;
}

_g int lisperror(const char *fmt, ...)
{
	int check;
	FILE *file;
	va_list args;

	file = lisperror_stream();
	va_start(args, fmt);
	check = vfprintf(file, fmt, args);
	va_end(args);
	fprintf(file, "\n");
	fflush(file);

	return check;
}

void lisp_init(void)
{
	initlisp();
	lisp_code = 0;
	lisp_result = 1;
}

void lisp_free(void)
{
	freelisp();
	lisp_code = 0;
}

int lisp_alloc(size_t heap, size_t local)
{
	if (lisp_code) {
		lisperror("lisp internal error.");
		return 1;
	}
	if (alloclisp(heap, local)) {
		lisperror("alloclisp error.");
		lisp_code = 1;
	}
	lisp_result = 1;

	return lisp_code;
}


/*
 *  help
 */
static const char *lisp_main_help_message[] = {
	Lispname " -- ANSI Common Lisp Programming Language.",
	"",
	"USAGE:",
	"  " Lispname " [options] [inputs] [--] [arguments]",
	"",
	"OPTIONS:",
	"  --help             Print this message.",
	"  --version          Print the version infomation.",
	"  --core             Core mode.",
	"  --standalone       Standalone mode.",
#ifdef LISP_DEGRADE
	"  --degrade          Degrade mode.",
#endif
	"  --heap <size>      Heap memory size.",
	"  --local <size>     Local memory size.",
	"  --corefile <file>  Core file instead of default file used.",
	"  --initfile <file>  Init file instead of default file used.",
	"  --nocore           Don't load a default core file.",
	"  --noinit           Don't load a default init file.",
	"  --debugger         Enable debugger.",
	"  --nodebugger       Disable debugger.",
	"  --quit             Exit after load and eval processing.",
	"",
	"INPUTS:",
	"  --load <file>      Load source file.",
	"  --script <file>    Load script file.",
	"  --eval <cmd>       Execute command.",
	"",
	"If inputs aren't appeared, load from a standard-input.",
	"",
	NULL
};

int lisp_main_help(FILE *file)
{
	int i;
	const char *ptr;

	if (file == NULL) return 1;
	for (i = 0; ; i++) {
		ptr = lisp_main_help_message[i];
		if (ptr == NULL) break;
		fprintf(file, "%s\n", ptr);
	}
	lisp_result = 0;

	return 0;
}


/*
 *  version
 */
int lisp_main_version_text(FILE *file)
{
	if (file == NULL) return 1;
	fprintf(file, Lispname " Version %d.%d.%d\n",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	fprintf(file, "-----\n");
	fprintf(file, "%-20s %s\n", "Memory size", LISP_ARCH_MODE);
	fprintf(file, "%-20s %s\n", "Fixnum size", LISP_FIXNUM_MODE);
	fprintf(file, "%-20s %s\n", "Lisp mode", LISP_MODE);
	fprintf(file, "%-20s %s\n", "Thread mode", LISP_THREAD_MODE);
	fprintf(file, "%-20s %d.%d.%d\n", "Version",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	fprintf(file, "%-20s %s\n", "Build information", LISP_REVISION);
	fprintf(file, "-----\n");
	fprintf(file, "%-20s %s\n", "Execute mode", LISP_MODE_STRING);
	fprintf(file, "%-20s %s\n", "Release mode", LISP_DEBUG_STRING);
	fprintf(file, "%-20s %s\n", "Degrade mode", LISP_DEGRADE_STRING);
	fprintf(file, "%-20s %s\n", "Prompt mode", LISP_PROMPT_STRING);
#ifdef LISP_DEBUG_MEMORY
	fprintf(file, "%-20s %s\n", "Debug Memory", "true");
#endif
#ifdef LISP_DEBUG_FORCE_GC
	fprintf(file, "%-20s %s\n", "Force GC", "true");
#endif
#ifdef LISP_MEMORY_MALLOC
	fprintf(file, "%-20s %s\n", "Memory Malloc", "true");
#endif
	fprintf(file, "-----\n");

	lisp_result = 0;

	return 0;
}

int lisp_main_version_script(FILE *file)
{
	if (file == NULL) return 1;
	fprintf(file, "name\t" Lispname "\n");
	fprintf(file, "%s\t%s\n", "memory-size", LISP_ARCH_MODE);
	fprintf(file, "%s\t%s\n", "fixnum-size", LISP_FIXNUM_MODE);
	fprintf(file, "%s\t%s\n", "lisp-mode", LISP_MODE);
	fprintf(file, "%s\t%s\n", "thread-mode", LISP_THREAD_MODE);
	fprintf(file, "%s\t%d.%d.%d\n", "version",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	fprintf(file, "%s\t%s\n", "build-information", LISP_REVISION);
	fprintf(file, "%s\t%s\n", "execute-mode", LISP_MODE_STRING);
	fprintf(file, "%s\t%s\n", "release-mode", LISP_DEBUG_STRING);
	fprintf(file, "%s\t%s\n", "degrade-mode", LISP_DEGRADE_STRING);
	fprintf(file, "%s\t%s\n", "prompt-mode", LISP_PROMPT_STRING);
#ifdef LISP_DEBUG_MEMORY
	fprintf(file, "%s\t%s\n", "debug-memory", "true");
#else
	fprintf(file, "%s\t%s\n", "debug-memory", "false");
#endif
#ifdef LISP_DEBUG_FORCE_GC
	fprintf(file, "%s\t%s\n", "force-gc", "enable");
#else
	fprintf(file, "%s\t%s\n", "force-gc", "disable");
#endif
#ifdef LISP_AMALGAMATION
	fprintf(file, "%s\t%s\n", "amalgamation", "true");
#else
	fprintf(file, "%s\t%s\n", "amalgamation", "false");
#endif
#ifdef LISP_MEMORY_MALLOC
	fprintf(file, "%s\t%s\n", "memory-malloc", "true");
#else
	fprintf(file, "%s\t%s\n", "memory-malloc", "false");
#endif
	lisp_result = 0;

	return 0;
}

int lisp_main_version(struct lispargv *ptr, FILE *file)
{
	if (ptr->version_script)
		return lisp_main_version_script(stdout);
	else
		return lisp_main_version_text(stdout);
}

int lisp_main_degrade(struct lispargv *ptr)
{
	lisp_result = degradelisp();
	return lisp_result;
}


/*
 *  execute
 */
#define EnvHome "HOME"
#define EnvLispHome (LISPNAME "_HOME")
#define EnvLispUser (LISPNAME "_USER")
#ifdef LISP_WINDOWS_OR_ANSI
#define EnvUserProfile "USERPROFILE"
#define EnvProgramData "ProgramData"
#define EnvProgramFiles "PROGRAMFILES"
#define EnvProgramFilesx86 "ProgramFiles(x86)"
#endif

/* load core */
#ifndef LISP_WINDOWS_WIDE
static int lisp_argv_core_load(const char *name)
{
	int check;
	lispstringu file;

	file = char_stringu(name);
	if (file == NULL) {
		lisperror("char_stringu error.");
		return 1;
	}
	check = load_core(file->ptr, file->size);
	free_stringu(file);
	if (0 < check) {
		lisperror("load_core error.");
		return 1;
	}

	return check;
}
#define InitCoreLoad(x) { \
	int __result = lisp_argv_core_load(x); \
	if (__result == 0) { \
		return 0; \
	} \
	if (0 < __result) { \
		lisp_code = 1; \
		return 1; \
	} \
}
#endif

static int lisp_argv_core_env(lisptableu env, const char *key, const char *name)
{
	int check;
	lispstringu value, file;

	/* environment */
	value = findchar_tableu(env, key);
	if (value == NULL)
		return -1;
	/* load */
	file = concatchar_stringu(value, name);
	if (file == NULL) {
		lisperror("concatchar_stringu error.");
		return 1;
	}
	check = load_core(file->ptr, file->size);
	free_stringu(file);
	if (0 < check) {
		lisperror("load_core error.");
		return 1;
	}

	return check;
}
#define InitCoreEnv(p,x,y) { \
	int __result = lisp_argv_core_env((p),(x),(y)); \
	if (__result == 0) { \
		return 0; \
	} \
	if (0 < __result) { \
		lisp_code = 1; \
		return 1; \
	} \
}

static int lisp_argv_core_default(struct lispargv *ptr)
{
	lisptableu env;

	env = ptr->env;
#ifdef LISP_WINDOWS_WIDE
	InitCoreEnv(env, EnvLispHome, "\\" Lispname ".core");
	InitCoreEnv(env, EnvLispHome, "\\lib\\" Lispname ".core");
	InitCoreEnv(env, EnvUserProfile, "\\" Lispname ".core");
	InitCoreEnv(env, EnvProgramData, "\\" Lispname "\\" Lispname ".core");
	InitCoreEnv(env, EnvProgramFiles, "\\" Lispname "\\" Lispname ".core");
	InitCoreEnv(env, EnvProgramFilesx86, "\\" Lispname "\\" Lispname ".core");
#else
	InitCoreEnv(env, EnvLispHome, "/" Lispname ".core");
	InitCoreEnv(env, EnvLispHome, "/lib/" Lispname ".core");
	InitCoreEnv(env, EnvHome, "/." Lispname "/" Lispname ".core");
	InitCoreLoad("/usr/lib/" Lispname "/" Lispname ".core");
	InitCoreLoad("/usr/local/lib/" Lispname "/" Lispname ".core");
	InitCoreLoad("/opt/" Lispname "/" Lispname ".core");
	InitCoreLoad("/opt/lib/" Lispname "/" Lispname ".core");
#endif

	return 1;
}

static int lisp_argv_initcode(struct lispargv *ptr)
{
	lispstringu file;
	size_t heap, local;

	/* allocate */
	heap = ptr->heap;
	local = ptr->local;
	if (heap == 0)
		heap = DefaultHeapSize;
	if (local == 0)
		local = DefaultLocalSize;
	if (alloclisp(heap, local)) {
		lisperror("lisp initialize error.");
		return 1;
	}

	/* --nocore */
	if (ptr->nocore)
		return 0;

	/* --corefile */
	file = ptr->core;
	if (file) {
		if (load_core(file->ptr, file->size)) {
			lisperror("Cannot read corefile.");
			return 1;
		}
		return 0;
	}

	/* default corefile */
	if (lisp_argv_core_default(ptr)) {
		lisperror("Cannot read all default corefiles.");
		return 1;
	}

	return 0;
}

int lisp_argv_init(struct lispargv *ptr)
{
	/* error */
	if (lisp_code) {
		lisp_result = 1;
		return 1;
	}

	/* run */
	if (lisp_argv_initcode(ptr)) {
		lisp_code = lisp_result = 1;
		return 1;
	}

	return 0;
}


/*
 *  lisp_argv_execute
 */
static int lispstringu_heap_(addr *ret, lispstringu str)
{
	return strvect_sizeu_heap_(ret, str->ptr, str->size - 1UL);
}

static int lisp_argv_load_(Execute ptr, lispstringu name, int error, int *ret)
{
	addr file;

	Return(lispstringu_heap_(&file, name));
	Return(pathname_designer_heap_(ptr, file, &file));
	return eval_main_load_(ptr, file, error, ret);
}

static int lisp_argv_script_(Execute ptr, lispstringu name)
{
	addr file, stream;

	/* open */
	Return(lispstringu_heap_(&file, name));
	Return(pathname_designer_heap_(ptr, file, &file));
	Return(open_input_utf8_stream_(ptr, &stream, file));
	if (stream == NULL)
		return fmte_("Cannot open file ~S.", file, NULL);
	script_header(stream);
	/* load */
	return eval_main_load_(ptr, stream, 1, NULL);
}

#ifndef LISP_WINDOWS_WIDE
static int lisp_argv_file_load_(Execute ptr, int *ret, const char *name)
{
	int check;
	lispstringu file;

	file = char_stringu(name);
	if (file == NULL)
		return fmte_("char_stringu error.", NULL);
	check = lisp_argv_load_(ptr, file, 0, ret);
	free_stringu(file);

	return check;
}
#define InitFileLoad(p,a,x) { \
	Return(lisp_argv_file_load_((p),(a),(x))); \
	if (*(a) == 0) return 0; \
}
#endif

static int lisp_argv_file_env_(Execute ptr, lisptableu env, int *ret,
		const char *key, const char *name)
{
	int check;
	lispstringu value, file;

	/* environment */
	value = findchar_tableu(env, key);
	if (value == NULL)
		return Result(ret, 0); /* next */
	/* load */
	file = concatchar_stringu(value, name);
	if (file == NULL)
		return fmte_("concatchar_stringu error.", NULL);
	check = lisp_argv_load_(ptr, file, 0, ret);
	free_stringu(file);

	return check;
}
#define InitFileEnv(p,e,a,x,y) { \
	Return(lisp_argv_file_env_((p),(e),(a),(x),(y))); \
	if (*(a) == 0) return 0; \
}

static int lisp_argv_load_default_(Execute ptr, struct lispargv *argv, int *a)
{
	lisptableu env;

	env = argv->env;
#ifdef LISP_WINDOWS_WIDE
	InitFileEnv(ptr,env,a, EnvUserProfile, "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "\\lib\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvProgramData, "\\" Lispname "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvProgramFiles, "\\" Lispname "\\" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvProgramFilesx86, "\\" Lispname "\\" Lispname ".lisp");
#else
	InitFileEnv(ptr,env,a, EnvHome, "/." Lispname "/" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "/" Lispname ".lisp");
	InitFileEnv(ptr,env,a, EnvLispHome, "/lib/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/usr/lib/" Lispname "/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/usr/local/lib/" Lispname "/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/opt/" Lispname "/" Lispname ".lisp");
	InitFileLoad(ptr,a, "/opt/lib/" Lispname "/" Lispname ".lisp");
#endif

	return 0;
}

static int lisp_argv_loadinit_(Execute ptr, struct lispargv *argv, int *ret)
{
	lispstringu file;

	/* --noinit */
	if (argv->noinit)
		return Result(ret, 0);  /* success */

	/* --initfile */
	file = argv->init;
	if (file)
		return lisp_argv_load_(ptr, file, 1, ret);
	else
		return lisp_argv_load_default_(ptr, argv, ret);
}

static int lisp_argv_eval_(Execute ptr, lispstringu str)
{
	addr pos;
	Return(lispstringu_heap_(&pos, str));
	return eval_main_string_(ptr, pos);
}

static int lisp_argv_inputs_(Execute ptr, struct lispargv *argv)
{
	lispstringu name;
	struct lispargv_string *data;
	size_t i, size;

	data = argv->input->data;
	size = argv->input->size;
	for (i = 0; i < size; i++) {
		name = data[i].value;
		switch (data[i].type) {
			case lispargv_load:
				Return(lisp_argv_load_(ptr, name, 1, NULL));
				continue;

			case lispargv_eval:
				Return(lisp_argv_eval_(ptr, name));
				continue;

			case lispargv_script:
				Return(lisp_argv_script_(ptr, name));
				continue;

			default:
				return fmte_("Invalid input type.", NULL);
		}
	}

	return 0;
}

static int lisp_argv_debugger_(Execute ptr, struct lispargv *argv)
{
	int v;

	/* debugger */
	v = argv->debuggerp? argv->debugger: consolep_file();
	set_enable_debugger(ptr, v);
	/* eval-loop */
	return eval_main_loop_(ptr);
}

static int lisp_argv_execute_(Execute ptr, struct lispargv *argv)
{
	int check;

	/* load initialize */
	check = 0;
	Return(lisp_argv_loadinit_(ptr, argv, &check));
	if (check) {
		lisp_result = 1;
		return 0;
	}

	/* load / eval */
	if (argv->input) {
		Return(lisp_argv_inputs_(ptr, argv));
	}

	/* call */
	if (argv->call) {
		if ((argv->call)(argv->call_ptr)) {
			lisp_result = 1;
			return 0;
		}
	}

	/* debugger */
	setindex_prompt(ptr, 0);
	if (argv->quit == 0)
		return lisp_argv_debugger_(ptr, argv);

	return 0;
}


/*
 *  lisp_argv_run
 */
static void lisp_argv_intern(addr table, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	setspecial_symbol(pos);
	SetValueSymbol(pos, table);
}

static int lisp_argv_environment_(struct lispargv *argv)
{
	addr table, key, value, cons;
	lisptableu env;
	lispstringu k, v;
	struct lispkeyvalueu *kv;
	size_t size, i;

	/* make hashtable */
	env = argv->env;
	kv = env->table;
	size = env->size;
	hashtable_size_heap(&table, env->size);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);

	/* intern hashtable */
	for (i = 0; i < size; i++) {
		k = kv[i].key;
		v = kv[i].value;
		if (k->size == 0 || v->size == 0)
			return fmte_("lisp_argv_environment error.", NULL);
		Return(lispstringu_heap_(&key, k));
		Return(lispstringu_heap_(&value, v));
		Return(intern_hashheap_(table, key, &cons));
		SetCdr(cons, value);
	}
	lisp_argv_intern(table, CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT);

	return 0;
}

static int lisp_argv_arguments_copy_(addr array, size_t i, lispstringu str)
{
	addr pos;

	Return(lispstringu_heap_(&pos, str));
	setarray(array, i, pos);

	return 0;
}

static int lisp_argv_arguments_(struct lispargv *argv)
{
	addr pos;
	lisparrayu array;
	lispstringu *data;
	size_t size, comm, copy, i;

	array = argv->argv;
	data = array->ptr;
	size = array->size;
	comm = argv->start;
	if (size < comm)
		return fmte_("Invalid array size.", NULL);

	else if (size == 0) {
		vector_heap(&pos, 0);
	}
	else {
		copy = size - comm;
		vector_heap(&pos, copy + 1UL);
		Return(lisp_argv_arguments_copy_(pos, 0, data[0]));
		for (i = 1; i < copy; i++) {
			Return(lisp_argv_arguments_copy_(pos, i, data[comm + i]));
		}
	}
	lisp_argv_intern(pos, CONSTANT_SYSTEM_SPECIAL_ARGUMENTS);

	return 0;
}

/* savecore */
static void lisp_argv_makunbound(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	SetValueSymbol(symbol, Unbound);
}

static int lisp_argv_core_(Execute ptr)
{
	lisp_argv_makunbound(CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT);
	lisp_argv_makunbound(CONSTANT_SYSTEM_SPECIAL_ARGUMENTS);
	lisp_result = save_core(ptr);

	return 0;
}

static int lisp_argv_exit_(Execute ptr)
{
	lisp_result = (int)ptr->result;
	return 0;
}

static int lisp_argv_switch_call_(Execute ptr, struct lispargv *argv)
{
	push_prompt_info(ptr);
	Return(handler_warning_(ptr));
	Return(handler_savecore_(ptr));
	Return(handler_exit_(ptr));
	Return(lisp_argv_environment_(argv));
	Return(lisp_argv_arguments_(argv));
	Return(lisp_argv_execute_(ptr, argv));

	return 0;
}

static int lisp_argv_condition_p_(Execute ptr, constindex index, int *ret)
{
	addr pos, clos;

	if (ptr->throw_value != throw_handler_case)
		return Result(ret, 0);
	pos = ptr->throw_handler;
	GetConstant(index, &clos);
	GetNameHandler(pos, &pos);
	return Result(ret, pos == clos);
}

static int lisp_argv_savecore_p_(Execute ptr, int *ret)
{
	return lisp_argv_condition_p_(ptr, CONSTANT_CONDITION_SAVECORE, ret);
}

static int lisp_argv_exit_p_(Execute ptr, int *ret)
{
	return lisp_argv_condition_p_(ptr, CONSTANT_CONDITION_EXIT, ret);
}

static int lisp_argv_switch_(Execute ptr, struct lispargv *argv)
{
	int check;
	addr control;

	push_control(ptr, &control);
	if (lisp_argv_switch_call_(ptr, argv) == 0)
		return pop_control_(ptr, control);

	/* savecore */
	Return(lisp_argv_savecore_p_(ptr, &check));
	if (check) {
		normal_throw_control(ptr);
		Return(pop_control_(ptr, control));
		return lisp_argv_core_(ptr);
	}

	/* exit */
	Return(lisp_argv_exit_p_(ptr, &check));
	if (check) {
		normal_throw_control(ptr);
		Return(pop_control_(ptr, control));
		return lisp_argv_exit_(ptr);
	}

	return fmte_("Invalid result.", NULL);
}

/* runcode */
static int lisp_argv_code(struct lispargv *argv)
{
	Execute ptr;

	ptr = getexecute(0);
	Check(ptr == NULL, "getexecute error.");
	ptr->result = 0;

	/* execute */
	if (argv->nocore) {
		buildlisp(ptr);
	}
	if (lisp_argv_switch_(ptr, argv)) {
		abort_execute();
		return 1;
	}

	/* result */
	lisp_result = ptr->result;
	return 0;
}

int lisp_argv_run(struct lispargv *ptr)
{
	/* error */
	if (lisp_code) {
		lisp_result = 1;
		return 1;
	}

	/* runcode */
	if (lisp_argv_code(ptr)) {
		lisp_code = lisp_result = 1;
		return 1;
	}

	return 0;
}

