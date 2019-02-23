/*
 *  main - lisp command
 */
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <locale.h>
#include "character.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "core.h"
#include "encode.h"
#include "eval.h"
#include "eval_main.h"
#include "file.h"
#include "lisp.h"
#include "hashtable.h"
#include "package.h"
#include "pathname.h"
#include "prompt.h"
#include "strtype.h"
#include "symbol.h"

/* Structure */
enum LispMode {
	LispMode_Core,
	LispMode_Standalone,
	LispMode_Degrade,
	LispMode_Size
};

struct unimem {
	unicode *ptr;
	size_t size;
};

struct envmemory {
	struct unimem *ptr, *key, *value;
};

struct string_data {
	struct string_data *next;
	struct unimem *data;
};

struct string_list {
	struct string_data *root, *tail;
};

/* Input/Output parametes */
static int MainArgc = 0;
static char **MainArgv = NULL;
static char **MainEnv = NULL;
static size_t Argc = 0;
static size_t Envc = 0;
static struct unimem **Argv = NULL;
static struct envmemory **Envv = NULL;
static int Result = 1;

/* Arguments variables */
static size_t SizeHeap = (128UL * 1024UL * 1024UL);
static size_t SizeLocal = (1UL * 1024UL * 1024UL);
static int NoCore = 0;
static int NoInit = 0;
static int Debugger = 1;
static int Interactive = -1;
static int Quit = 0;
enum LispMode Mode = LispMode_Core;
static struct unimem *FileCore = NULL;
static struct unimem *FileInit = NULL;
static struct string_list *FileName_Core = NULL;
static struct string_list *FileName_Init = NULL;

/* Environment values */
static struct unimem *EnvHome = NULL;
static struct unimem *EnvLispHome = NULL;
static struct unimem *EnvLispUser = NULL;
#ifdef LISP_WINDOWS_OR_ANSI
static struct unimem *EnvUserProfile = NULL;
static struct unimem *EnvProgramData = NULL;
static struct unimem *EnvProgramFiles = NULL;
static struct unimem *EnvProgramFilesx86 = NULL;
#endif


/*
 *  error
 */
static int errormsg(const char *fmt, ...)
{
	int check;
	va_list args;

	va_start(args, fmt);
	check = vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	fflush(stderr);

	return check;
}


/*
 *  unimem
 */
static struct unimem *make_unimem(size_t size)
{
	unicode *u;
	struct unimem *ptr;

	ptr = (struct unimem *)malloc(sizeoft(struct unimem));
	if (ptr == NULL) {
		errormsg("malloc struct error.");
		return NULL;
	}
	u = (unicode *)malloc(sizeoft(unicode) * (size + 1UL));
	if (u == NULL) {
		errormsg("malloc body error.");
		free(ptr);
		return NULL;
	}
#ifdef LISP_DEBUG
	memset(u, 0xAA, sizeoft(unicode) * (size + 1UL));
#endif
	ptr->ptr = u;
	ptr->size = size;

	return ptr;
}

static void free_unimem(struct unimem *ptr)
{
	if (ptr) {
		free(ptr->ptr);
		free(ptr);
	}
}

#ifdef LISP_DEBUG
void output_unicode(const unicode *ptr, size_t size, int eol)
{
	unicode u;
	size_t i;

	if (ptr == NULL) {
		printf("NULL");
	}
	else {
		for (i = 0; i < size; i++) {
			u = ptr[i];
			if (u <= 0xFF && isgraph((int)u)) {
				printf("%c", (int)u);
			}
		}
	}
	if (eol)
		printf("\n");
	fflush(stdout);
}
void output_unimem(const struct unimem *ptr, int eol)
{
	if (ptr == NULL) {
		printf("null");
		if (eol)
			printf("\n");
		fflush(stdout);
		return;
	}
	output_unicode(ptr->ptr, ptr->size, eol);
}
#endif


/*
 *  string-list
 */
static struct string_list *make_string_list(void)
{
	struct string_list *ptr;

	ptr = (struct string_list *)malloc(sizeoft(struct string_list));
	if (ptr == NULL) {
		errormsg("malloc error.");
		return NULL;
	}
	memset(ptr, 0, sizeoft(struct string_list));

	return ptr;
}

static void free_string_list(struct string_list *ptr)
{
	struct string_data *left, *right;

	if (ptr) {
		for (left = ptr->root; left; left = right) {
			right = left->next;
			free_unimem(left->data);
			free(left);
		}
		free(ptr);
	}
}

static int push_string_list(struct string_list *ptr, struct unimem *u)
{
	struct string_data *data;

	/* make data */
	data = (struct string_data *)malloc(sizeoft(struct string_data));
	if (data == NULL) {
		errormsg("malloc error.");
		return 1;
	}
	data->data = u;
	data->next = NULL;

	/* push queue */
	if (ptr->root)
		ptr->tail = ptr->tail->next = data;
	else
		ptr->root = ptr->tail = data;

	return 0;
}


/*
 *  filename
 */
static void copy_unicode_char(unicode *dst, const char *src, size_t size)
{
	size_t i;
	const byte *ptr;

	ptr = (const byte *)src;
	for (i = 0; i < size; i++)
		dst[i] = (unicode)ptr[i];
}

static int filename_uc(struct string_list *list,
		const struct unimem *first, const char *tail)
{
	unicode *body;
	struct unimem *ptr;
	size_t size1, size2;

	if (first == NULL) return 0;
	size1 = first->size;
	size2 = strlen(tail);
	ptr = make_unimem(size1 + size2);
	if (ptr == NULL) {
		errormsg("make_unimem error.");
		return 1;
	}
	body = ptr->ptr;
	memcpy(body, first->ptr, sizeoft(unicode) * size1);
	copy_unicode_char(body + size1, tail, size2);
	body[size1 + size2] = 0;
	push_string_list(list, ptr);

	return 0;
}

#ifndef LISP_WINDOWS_WIDE
static int filename_cc(struct string_list *list,
		const char *first, const char *tail)
{
	unicode *body;
	struct unimem *ptr;
	size_t size1, size2;

	if (first == NULL) return 0;
	size1 = strlen(first);
	size2 = strlen(tail);
	ptr = make_unimem(size1 + size2);
	if (ptr == NULL) {
		errormsg("make_unimem error.");
		return 1;
	}
	body = ptr->ptr;
	copy_unicode_char(body, first, size1);
	copy_unicode_char(body + size1, tail, size2);
	body[size1 + size2] = 0;
	push_string_list(list, ptr);

	return 0;
}
#endif

#define corefile_uc(x, y) { \
	if (filename_uc(FileName_Core, x, y)) return 1; \
}
#define corefile_cc(x, y) { \
	if (filename_cc(FileName_Core, x, y)) return 1; \
}

static int filename_core(void)
{
	FileName_Core = make_string_list();
	if (FileName_Core == NULL) {
		errormsg("make_string_list error.");
		return 1;
	}

#ifdef LISP_WINDOWS_WIDE
	corefile_uc(EnvLispHome, "\\" Lispname ".core");
	corefile_uc(EnvLispHome, "\\lib\\" Lispname ".core");
	corefile_uc(EnvUserProfile, "\\" Lispname ".core");
	corefile_uc(EnvProgramData, "\\" Lispname "\\" Lispname ".core");
	corefile_uc(EnvProgramFiles, "\\" Lispname "\\" Lispname ".core");
	corefile_uc(EnvProgramFilesx86, "\\" Lispname "\\" Lispname ".core");
#else
	corefile_uc(EnvLispHome, "/" Lispname ".core");
	corefile_uc(EnvLispHome, "/lib/" Lispname ".core");
	corefile_uc(EnvHome, "/." Lispname "/" Lispname ".core");
	corefile_cc("/usr", "/lib/" Lispname "/" Lispname ".core");
	corefile_cc("/usr/local", "/lib/" Lispname "/" Lispname ".core");
	corefile_cc("/opt", "/" Lispname "/" Lispname ".core");
	corefile_cc("/opt", "/lib/" Lispname "/" Lispname ".core");
#endif
	if ((! NoCore) && FileName_Core->root == NULL) {
		errormsg("Cannot find all default core file.");
		return 1;
	}

	return 0;
}

#define initfile_uc(x, y) { \
	if (filename_uc(FileName_Init, x, y)) return 1; \
}
#define initfile_cc(x, y) { \
	if (filename_cc(FileName_Init, x, y)) return 1; \
}

static int filename_init(void)
{
	FileName_Init = make_string_list();
	if (FileName_Init == NULL) {
		errormsg("make_string_list error.");
		return 1;
	}

#ifdef LISP_WINDOWS_WIDE
	initfile_uc(EnvUserProfile, "\\" Lispname ".lisp");
	initfile_uc(EnvLispHome, "\\" Lispname ".lisp");
	initfile_uc(EnvLispHome, "\\lib\\" Lispname ".lisp");
	initfile_uc(EnvProgramData, "\\" Lispname "\\" Lispname ".lisp");
	initfile_uc(EnvProgramFiles, "\\" Lispname "\\" Lispname ".lisp");
	initfile_uc(EnvProgramFilesx86, "\\" Lispname "\\" Lispname ".lisp");
#else
	initfile_uc(EnvHome, "/." Lispname "/" Lispname ".lisp");
	initfile_uc(EnvLispHome, "/" Lispname ".lisp");
	initfile_uc(EnvLispHome, "/lib/" Lispname ".lisp");
	initfile_cc("/usr", "/lib/" Lispname "/" Lispname ".lisp");
	initfile_cc("/usr/local", "/lib/" Lispname "/" Lispname ".lisp");
	initfile_cc("/opt", "/" Lispname "/" Lispname ".lisp");
	initfile_cc("/opt", "/lib/" Lispname "/" Lispname ".lisp");
#endif
	if ((! NoInit) && FileName_Init->root == NULL) {
		errormsg("Cannot find all default init file.");
		return 1;
	}

	return 0;
}


/*
 *  command argument
 */
static int equal_unimem_char(const struct unimem *left, const char *right)
{
	const byte *b;
	const unicode *u;
	size_t i, size;

	size = strlen(right);
	if (left->size != size) return 0;
	u = left->ptr;
	b = (const byte *)right;
	for (i = 0; i < size; i++) {
		if (u[i] != (unicode)b[i]) return 0;
	}

	return 1;
}

static int args_argument(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--");
}
static int args_help(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--help");
}
static int args_version(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--version");
}
static int args_version_script(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--version-script");
}
static int args_core(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--core");
}
static int args_standalone(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--standalone");
}
static int args_build(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--build");
}
static int args_degrade(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--degrade");
}
static int args_heap(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--heap");
}
static int args_local(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--local");
}
static int args_corefile(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--corefile");
}
static int args_initfile(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--initfile");
}
static int args_nocore(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--nocore");
}
static int args_noinit(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--noinit");
}
static int args_debugger(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--debugger");
}
static int args_nodebugger(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--nodebugger");
}
static int args_interactive(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--interactive");
}
static int args_script(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--script");
}
static int args_quit(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--quit");
}
static int args_load(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--load");
}
static int args_eval(const struct unimem *ptr)
{
	return equal_unimem_char(ptr, "--eval");
}


/*
 *  loop
 */
static int mainlisp_load(Execute ptr,
		const struct unimem *name, int *abort, int error)
{
	int result;
	addr file;

	strvect_sizeu_heap(&file, name->ptr, name->size);
	pathname_designer_heap(ptr, file, &file);
	result = eval_main_load(ptr, file, 0, abort);
	if (error && result)
		fmte("Cannot open file ~S.", file, NULL);

	return result;
}

static int mainlisp_loadinit(Execute ptr, int *abort)
{
	int check;
	struct string_data *list;

	/* --noinit */
	if (NoInit) {
		return 0;  /* success */
	}

	/* --initfile */
	if (FileInit) {
		mainlisp_load(ptr, FileInit, abort, 1);
		return 0;
	}

	/* default initfile */
	for (list = FileName_Init->root; list; list = list->next) {
		check = mainlisp_load(ptr, list->data, abort, 0);
		if (check < 0) { /* File is not found. */
			continue;
		}
		if (check) { /* Invalid init file */
			errormsg("Cannot read default init file.");
			return 1;
		}
		else { /* success */
			return 0;
		}
	}

	/* error */
	errormsg("Cannot read all default init file.");
	return 1;
}

static void mainlisp_eval(Execute ptr, const struct unimem *str, int *abort)
{
	addr pos;
	strvect_sizeu_heap(&pos, str->ptr, str->size);
	eval_main_string(ptr, pos, abort);
}

static void mainlisp_args(Execute ptr, size_t index)
{
	int abort;
	const struct unimem *check;

	abort = 0;
	for (; index < Argc; index++) {
		if (abort) {
			break;
		}
		check = Argv[index];
		if (args_load(check)) {
			mainlisp_load(ptr, Argv[++index], &abort, 1);
			continue;
		}
		if (args_eval(check)) {
			mainlisp_eval(ptr, Argv[++index], &abort);
			continue;
		}
		break;
	}
}

static void mainlisp_interactive(Execute ptr)
{
	/* debugger */
	set_enable_debugger(Debugger);
	/* interactive */
	if (Interactive < 0)
		Interactive = consolep_file();
	set_enable_interactive(Interactive);
	/* eval-loop */
	eval_main_loop(ptr);
}

static void mainlisp_execute(Execute ptr, size_t index)
{
	int abort;

	/* load initialize */
	abort = 0;
	if (mainlisp_loadinit(ptr, &abort)) {
		Result = 1;
		return;
	}

	/* load / eval */
	if (abort == 0)
		mainlisp_args(ptr, index);

	/* interactive mode */
	setindex_prompt(ptr, 0);
	if (Quit == 0)
		mainlisp_interactive(ptr);
}

static void mainlisp_intern(addr table, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	setspecial_symbol(pos);
	SetValueSymbol(pos, table);
}

static void mainlisp_environment(void)
{
	addr table, key, value, cons;
	const struct envmemory *env;
	const struct unimem *ukey, *uvalue;
	size_t i;

	/* make hashtable */
	hashtable_size_heap(&table, Envc);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);

	/* intern hashtable */
	for (i = 0; i < Envc; i++) {
		env = Envv[i];
		ukey = env->key;
		uvalue = env->value;
		strvect_sizeu_heap(&key, ukey->ptr, ukey->size);
		strvect_sizeu_heap(&value, uvalue->ptr, uvalue->size);
		intern_hashheap(table, key, &cons);
		SetCdr(cons, value);
	}
	mainlisp_intern(table, CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT);
}

static void mainlisp_arguments(size_t index)
{
	const struct unimem *arg;
	addr pos, value;
	size_t size;

	/* check */
	for (; index < Argc; index++) {
		arg = Argv[index];
		if (args_argument(arg)) {
			index++;
			break;
		}
		if (args_load(arg) || args_eval(arg)) {
			index++;
			if (Argc <= index)
				fmte("Argument error.", NULL);
			continue;
		}
		fmte("Invalid argument.", NULL);
	}

	/* make vector */
	if (index < Argc) {
		vector_heap(&pos, (size_t)(Argc - index));
		for (size = 0; index < Argc; index++, size++) {
			arg = Argv[index];
			strvect_sizeu_heap(&value, arg->ptr, arg->size);
			setarray(pos, size, value);
		}
	}
	else {
		vector_heap(&pos, 0);
	}

	/* intern */
	mainlisp_intern(pos, CONSTANT_SYSTEM_SPECIAL_ARGUMENTS);
}

static void mainlisp_root(Execute ptr, size_t index)
{
	addr control;
	codejump jump;

	push_close_control(ptr, &control);
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		push_prompt_info(ptr);
		handler_warning(ptr);
		mainlisp_environment();
		mainlisp_arguments(index);
		mainlisp_execute(ptr, index);
	}
	end_switch(&jump);
	throw_switch(&jump);
	if (free_control(ptr, control)) {
		errormsg("free_control error.");
		exitexecute(ptr, LISPCODE_ABORT);
	}
}

static void makunbound_variable(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	SetValueSymbol(symbol, Unbound);
}

static int mainlisp_core(Execute ptr)
{
	makunbound_variable(CONSTANT_SYSTEM_SPECIAL_ENVIRONMENT);
	makunbound_variable(CONSTANT_SYSTEM_SPECIAL_ARGUMENTS);
	return make_core();
}

static int mainlisp_result(Execute ptr, lispcode code)
{
	switch (code) {
		case LISPCODE_EXECUTE:
		case LISPCODE_SUCCESS:
		case LISPCODE_EXIT:
			Result = ptr->result;
			return 0;

		case LISPCODE_SAVECORE:
			return mainlisp_core(ptr);

		case LISPCODE_ABORT:
		case LISPCODE_MEMORY:
		case LISPCODE_CONFLICT:
		case LISPCODE_CONTROL:
		case LISPCODE_ERROR:
		default:
			errormsg("ABORT: lisp error, interrupt.");
			return 1;
	}
}

static int mainlisp(size_t index)
{
	lispcode code;
	LocalRoot local;
	LocalStack stack;
	Execute ptr;

	ptr = getexecute(0);
	Check(ptr == NULL, "getexecute error");
	ptr->result = 0;
	local = ptr->local;
	push_local(local, &stack);
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		if (NoCore) {
			buildlisp(ptr);
		}
		mainlisp_root(ptr, index);
	}
	end_code(ptr);
	rollback_local(local, stack);

	return mainlisp_result(ptr, code);
}

static int mainargs_loadcore(void)
{
	int check;
	struct string_data *list;
	struct unimem *data;

	/* --corefile */
	if (FileCore) {
		if (load_core(FileCore->ptr, FileCore->size)) {
			errormsg("Cannot read core file.");
			return 1;
		}
		return 0;
	}

	/* Default corefile */
	for (list = FileName_Core->root; list; list = list->next) {
		data = list->data;
		check = load_core(data->ptr, data->size);
		if (check < 0) /* File is not found. */
			continue;
		if (check) { /* Invalid core file */
			errormsg("Cannot read default core file.");
			return 1;
		}
		else { /* success */
			return 0;
		}
	}

	/* error */
	errormsg("Cannot read all default core file.");
	return 1;
}

static int mainargs_core(size_t index)
{
	if (mainargs_loadcore()) {
		errormsg("mainargs_loadcore error.");
		return 1;
	}
	if (mainlisp(index)) {
		errormsg("mainlisp error.");
		return 1;
	}

	return 0;
}

static int make_filename_buffer(void)
{
	FileName_Core = NULL;
	FileName_Init = NULL;

	if (! NoCore) {
		if (filename_core()) {
			errormsg("filename_core error.");
			return 1;
		}
	}
	if (filename_init()) {
		errormsg("filename_init error.");
		return 1;
	}

	return 0;
}

static void free_filename_buffer(void)
{
	free_string_list(FileName_Core);
	free_string_list(FileName_Init);
	FileName_Core = NULL;
	FileName_Init = NULL;
}

static int mainargs_execute(size_t index)
{
	int check;

	if (alloclisp(SizeHeap, SizeLocal)) {
		errormsg("alloclisp error.");
		return 1;
	}
	check = NoCore?
		mainlisp(index):
		mainargs_core(index);
	freelisp();

	return check;
}

static int mainargs_filename(size_t index)
{
	int check;

	if (make_filename_buffer()) {
		errormsg("make_filename_buffer error.");
		return 1;
	}
	check = mainargs_execute(index);
	free_filename_buffer();
	if (check)
		errormsg("mainargs_execute error.");

	return check;
}


/*
 *  help
 */
static const char *help_message[] = {
	Lispname " -- Lisp Programming Language.",
	"",
	"USAGE:",
	"  " Lispname " [options] [inputs] [--] [arguments]",
	"",
	"OPTIONS:",
	"  --help             Print this message.",
	"  --version          Print the version infomation.",
#if 0
	"  --core             Core mode.",
#endif
	"  --standalone       Standalone mode.",
#ifdef LISP_DEGRADE
	"  --degrade          Degrade mode.",
#endif
	"  --heap <size>      Heap memory size.",
	"  --local <size>     Local memory size.",
#if 0
	"  --corefile <file>  Core file instead of default file used.",
#endif
	"  --initfile <file>  Init file instead of default file used.",
#if 0
	"  --nocore           Don't load a default core file.",
#endif
	"  --noinit           Don't load a default init file.",
	"  --debugger         Enable debugger.",
	"  --nodebugger       Disable debugger.",
	"  --interactive      Enable interactive mode.",
	"  --script           Disable interactive mode [script mode].",
	"  --quit             Exit after load and eval processing.",
	"",
	"INPUTS:",
	"  --load <file>      Load source file.",
	"  --eval <cmd>       Execute command.",
	"",
	"If inputs aren't appeared, load from a standard-input.",
	"",
	NULL
};

static void command_help(void)
{
	int i;
	const char *ptr;

	for (i = 0; ; i++) {
		ptr = help_message[i];
		if (ptr == NULL) break;
		printf("%s\n", ptr);
	}
}

static void command_version(void)
{
	printf(Lispname " Version %d.%d.%d\n",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	printf("-----\n");
	printf("%-20s %s\n", "Memory size", LISP_ARCH_MODE);
	printf("%-20s %s\n", "Fixnum size", LISP_FIXNUM_MODE);
	printf("%-20s %s\n", "Lisp mode", LISP_MODE);
	printf("%-20s %s\n", "Thread mode", LISP_THREAD_MODE);
	printf("%-20s %d.%d.%d\n", "Version",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	printf("%-20s %s\n", "Build information", LISP_REVISION);
	printf("-----\n");
	printf("%-20s %s\n", "Execute mode", LISP_MODE_STRING);
	printf("%-20s %s\n", "Release mode", LISP_DEBUG_STRING);
	printf("%-20s %s\n", "Degrade mode", LISP_DEGRADE_STRING);
	printf("-----\n");
}

static void command_version_script(void)
{
	printf("name\t" Lispname "\n");
	printf("%s\t%s\n", "memory-size", LISP_ARCH_MODE);
	printf("%s\t%s\n", "fixnum-size", LISP_FIXNUM_MODE);
	printf("%s\t%s\n", "lisp-mode", LISP_MODE);
	printf("%s\t%s\n", "thread-mode", LISP_THREAD_MODE);
	printf("%s\t%d.%d.%d\n", "version",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C);
	printf("%s\t%s\n", "build-information", LISP_REVISION);
	printf("%s\t%s\n", "execute-mode", LISP_MODE_STRING);
	printf("%s\t%s\n", "release-mode", LISP_DEBUG_STRING);
	printf("%s\t%s\n", "degrade-mode", LISP_DEGRADE_STRING);
}


/*
 *  setmemory
 */
static int strtosize(const unicode *str, const unicode **endp, size_t *ret)
{
	unicode c;
	size_t value;

	/* trim space */
	for (;;) {
		c = *str;
		if (! isSpaceUnicode(c)) break;
		str++;
	}
	if (! isDigitCase(c)) return 1;

	/* parse-integer */
	value = 0;
	for (;;) {
		c = *str;
		if (! isDigitCase(c)) break;
		if (value) {
			if ((SIZE_MAX / 10) < value) {
				*ret = SIZE_MAX;
				return 2;
			}
			value *= 10;
		}
		if (value > (SIZE_MAX - c)) {
			*ret = SIZE_MAX;
			return 2;
		}
		value += c - '0';
		str++;
	}
	if (endp) *endp = str;
	*ret = value;

	return 0;
}

static int getunitstring(const unicode *str, int *type)
{
	unicode c, check;

	c = *(str++);
	if (c == 0) {
		*type = 0;
		return 0;
	}
	c = toUpperUnicode(c);
	if (c == 'K' || c == 'M' || c == 'G' || c == 'T' || c == 'P' || c == 'E') {
		for (;;) {
			check = *str;
			if (! isSpaceUnicode(check)) break;
			str++;
		}
		if (check != 0) return 1;
	}
	else {
		return 1;
	}
	*type = (int)c;

	return 0;
}

static int unitloop(size_t *value, int loop)
{
	int i;
	size_t v;

	v = *value;
	for (i = 0; i < loop; i++) {
		if ((SIZE_MAX / 1024UL) < v) {
			return 1;
		}
		v *= 1024UL;
	}
	*value = v;

	return 0;
}

static int getsizestring(const unicode *str, size_t *ret)
{
	int check;
	const unicode *next;
	size_t size;

	check = strtosize(str, &next, &size);
	if (check == 2) {
		errormsg("Number is too large.");
		return 1;
	}
	if (check) {
		errormsg("Invalid memory argument.");
		return 1;
	}
	if (size == 0) {
		errormsg("Memory size must not be a zero.");
		return 1;
	}
	if (getunitstring(next, &check)) {
		errormsg("Invalid unit string.");
		return 1;
	}
	switch (check) {
		case 'K': if (unitloop(&size, 1)) goto error; break;
		case 'M': if (unitloop(&size, 2)) goto error; break;
		case 'G': if (unitloop(&size, 3)) goto error; break;
		case 'T': if (unitloop(&size, 4)) goto error; break;
		case 'P': if (unitloop(&size, 5)) goto error; break;
		case 'E': if (unitloop(&size, 6)) goto error; break;
		default: break;
	}
	*ret = size;
	return 0;

error:
	errormsg("Number is too large.");
	return 1;
}

static int setheap(const struct unimem *arg)
{
	int check;
	size_t size;

	check = getsizestring(arg->ptr, &size);
	if (check) {
		errormsg("Memory size format error.");
		return 1;
	}
	if (size < 1024UL * 1024UL) {
		errormsg("Memory size must be at least 1MByte.");
		return 1;
	}
	SizeHeap = size;

	return 0;
}

static int setlocal(const struct unimem *arg)
{
	int check;
	size_t size;

	check = getsizestring(arg->ptr, &size);
	if (check) {
		errormsg("Stack size format error.");
		return 1;
	}
	if (size < 1024UL * 1024UL) {
		errormsg("Stack size must be at least 1MByte.");
		return 1;
	}
	SizeLocal = size;

	return 0;
}


/*
 *  Argument
 */
static int lispargs_heap(size_t index)
{
	if (Argc <= index - 1) {
		errormsg("After --heap must be at least one argument.");
		return 1;
	}
	if (setheap(Argv[++index])) {
		errormsg("Invalid heap size.");
		return 1;
	}

	return 0;
}

static int lispargs_local(size_t index)
{
	if (Argc <= index - 1) {
		errormsg("After --local must be at least one argument.");
		return 1;
	}
	if (setlocal(Argv[++index])) {
		errormsg("Invalid local size.");
		return 1;
	}

	return 0;
}

static int lispargs_corefile(size_t index)
{
	if (Argc <= index - 1) {
		errormsg("After --corefile must be at least one argument.");
		return 1;
	}
	if (Mode == LispMode_Standalone) {
		errormsg("Cannot load a core file in standalone mode.");
		return 1;
	}
	FileCore = Argv[++index];

	return 0;
}

static int lispargs_initfile(size_t index)
{
	if (Argc <= index - 1) {
		errormsg("After --initfile must be at least one argument.");
		return 1;
	}
	FileInit = Argv[++index];

	return 0;
}

static void mainargs_parameter(enum LispMode mode)
{
	Mode = mode;
	switch (Mode) {
		case LispMode_Standalone:
			NoCore = 1;
			NoInit = 1;
			Debugger = 1;
			Interactive = -1;
			Quit = 0;
			break;

		case LispMode_Degrade:
		case LispMode_Core:
		default:
			NoCore = 0;
			NoInit = 0;
			Debugger = 1;
			Interactive = -1;
			Quit = 0;
			break;
	}
}

static int mainargs_loop(size_t *ret)
{
	size_t index;
	const struct unimem *arg;

	for (index = 1; index < Argc; index++) {
		arg = Argv[index];
		if (args_heap(arg)) {
			if (lispargs_heap(index++)) return 1;
			continue;
		}
		if (args_local(arg)) {
			if (lispargs_local(index++)) return 1;
			continue;
		}
		if (args_corefile(arg)) {
			if (lispargs_corefile(index++)) return 1;
			continue;
		}
		if (args_initfile(arg)) {
			if (lispargs_initfile(index++)) return 1;
			continue;
		}
		if (args_nocore(arg)) {
			NoCore = 1;
			continue;
		}
		if (args_noinit(arg)) {
			NoInit = 1;
			continue;
		}
		if (args_debugger(arg)) {
			Debugger = 1;
			continue;
		}
		if (args_nodebugger(arg)) {
			Debugger = 0;
			continue;
		}
		if (args_interactive(arg)) {
			Interactive = 1;
			continue;
		}
		if (args_script(arg)) {
			Interactive = 0;
			continue;
		}
		if (args_quit(arg)) {
			Quit = 1;
			continue;
		}
		if (args_load(arg) || args_eval(arg) || args_argument(arg)) {
			goto inputs;
		}
		if (args_core(arg)) {
			if (index == 1) {
				mainargs_parameter(LispMode_Core);
				continue;
			}
			errormsg("--core must be a first argument.");
			return 1;
		}
		if (args_standalone(arg) || args_build(arg)) {
			if (index == 1) {
				mainargs_parameter(LispMode_Standalone);
				continue;
			}
			errormsg("--standalone must be a first argument.");
			return 1;
		}
		if (args_degrade(arg)) {
			errormsg("--degrade must be only one argument.");
			return 1;
		}
		if (args_help(arg)) {
			errormsg("--help must be only one argument.");
			return 1;
		}
		if (args_version(arg)) {
			errormsg("--version must be only one argument.");
			return 1;
		}
		if (args_version_script(arg)) {
			errormsg("--version-script must be only one argument.");
			return 1;
		}
		errormsg("Illegal argument.");
		return 1;
	}
inputs:
	*ret = index;

	return 0;
}

static int mainargs_init(void)
{
#ifdef LISP_MODE_DEFAULT
	mainargs_parameter(LispMode_Core);
#endif
#ifdef LISP_MODE_STANDALONE
	mainargs_parameter(LispMode_Standalone);
#endif
#ifdef LISP_MODE_DEGRADE
	mainargs_parameter(LispMode_Degrade);
	if (Argc == 1) {
		Result = degradelisp(MainArgc, MainArgv, MainEnv);
		return 1;
	}
#endif

	return 0;
}

static int mainargs_mode(void)
{
	const struct unimem *arg;

	if (Argc == 2) {
		arg = Argv[1];
		if (args_degrade(arg)) {
			mainargs_parameter(LispMode_Degrade);
			Result = degradelisp(MainArgc, MainArgv, MainEnv);
			return 1;
		}
		if (args_help(arg)) {
			command_help();
			Result = 0;
			return 1;
		}
		if (args_version(arg)) {
			command_version();
			Result = 0;
			return 1;
		}
		if (args_version_script(arg)) {
			command_version_script();
			Result = 0;
			return 1;
		}
	}

	return 0;
}

static int mainargs(void)
{
	size_t index;

	initlisp();
	if (mainargs_init()) {
		return 0;
	}
	if (mainargs_mode()) {
		return 0;
	}
	if (mainargs_loop(&index)) {
		Result = 1;
		return 0;
	}
	if (mainargs_filename(index)) {
		errormsg("mainexecute error.");
		return 1;
	}

	return 0;
}


/****************************************************************************
 *  unicode
 ****************************************************************************/
#ifdef LISP_WINDOWS_WIDE
typedef WCHAR ctype;
typedef byte16 utype;
#define UTF_null_strlen UTF16_null_strlen
#define UTF_null_makeunicode UTF16_null_makeunicode
#else
typedef char ctype;
typedef byte utype;
#define UTF_null_strlen UTF8_null_strlen
#define UTF_null_makeunicode UTF8_null_makeunicode
#endif

static struct unimem *make_unimem_ctype(const ctype *str)
{
	struct unimem *ptr;
	size_t size;

	UTF_null_strlen((const utype *)str, &size);
	ptr = make_unimem(size);
	if (ptr == NULL) {
		errormsg("make_unimem error.");
		return NULL;
	}
	UTF_null_makeunicode(ptr->ptr, (const utype *)str);
	ptr->ptr[size] = 0;

	return ptr;
}

static void split_equal_sign(struct unimem *body,
		struct unimem *key,
		struct unimem *value)
{
	unicode u;
	unicode *ptr;
	size_t i, size;

	size = body->size;
	ptr = body->ptr;
	for (i = 0; i < size; i++) {
		u = ptr[i];
		if (u == '=' || u == 0) break;
	}
	ptr[i] = 0;

	/* key */
	key->ptr = body->ptr;
	key->size = i;
	/* value */
	if (i != size) {
		value->ptr = body->ptr + i + 1;
		value->size = body->size - i - 1;
	}
	else {
		value->ptr = body->ptr + i;
		value->size = 0;
	}
}

static struct envmemory *make_envmemory(const ctype *str)
{
	struct envmemory *ptr;
	struct unimem *u, *key, *value;

	ptr = NULL;
	u = key = value = NULL;
	/* ptr */
	ptr = (struct envmemory *)malloc(sizeoft(struct envmemory));
	if (ptr == NULL) {
		errormsg("malloc body error.");
		goto error;
	}
	/* key */
	key = (struct unimem *)malloc(sizeoft(struct unimem));
	if (key == NULL) {
		errormsg("malloc key error.");
		goto error;
	}
	/* value */
	value = (struct unimem *)malloc(sizeoft(struct unimem));
	if (value == NULL) {
		errormsg("malloc value error.");
		goto error;
	}
	/* unicode */
	u = make_unimem_ctype(str);
	if (u == NULL) {
		errormsg("make_unimem_ctype error.");
		goto error;
	}
	split_equal_sign(u, key, value);
	ptr->ptr = u;
	ptr->key = key;
	ptr->value = value;
	return ptr;

error:
	free(ptr);
	free(key);
	free(value);
	return NULL;
}

static void free_envmemory(struct envmemory *ptr)
{
	if (ptr) {
		free_unimem(ptr->ptr);
		free(ptr->key); /* Don't use free_unimem */
		free(ptr->value); /* Don't use free_unimem */
		free(ptr);
	}
}

static void free_argv_array(size_t size, struct unimem **ptr)
{
	size_t i;

	if (ptr) {
		for (i = 0; i < size; i++)
			free_unimem(ptr[i]);
		free(ptr);
	}
}

static struct unimem **make_argv_array(size_t size, ctype *argv[])
{
	struct unimem **ptr, *u;
	size_t i, allsize;

	allsize = sizeoft(struct unimem *) * size;
	ptr = (struct unimem **)malloc(allsize);
	if (ptr == NULL) {
		errormsg("malloc error.");
		return NULL;
	}
	memset(ptr, 0, allsize);

	for (i = 0; i < size; i++) {
		u = make_unimem_ctype(argv[i]);
		if (u == NULL) goto error;
		ptr[i] = u;
	}
	return ptr;

error:
	free_argv_array(size, ptr);
	return NULL;
}

static int main_argv(int argc, ctype *argv[])
{
	int check;
	struct unimem **ptr;
	size_t size;

	size = (size_t)argc;
	ptr = make_argv_array(size, argv);
	if (ptr == NULL) {
		errormsg("make_argv_array error.");
		return 1;
	}
	Argc = size;
	Argv = ptr;

	check = mainargs();
	if (check)
		errormsg("mainargs error.");
	free_argv_array(Argc, Argv);
	Argc = 0;
	Argv = NULL;

	return check;
}


/****************************************************************************
 *  main
 ****************************************************************************/
static void envfront(const struct envmemory *env,
		const char *name,
		struct unimem **var)
{
	if (equal_unimem_char(env->key, name))
		*var = env->value;
}

static void setenvvar(void)
{
	size_t i;
	struct envmemory *ptr;

	EnvHome = EnvLispHome = EnvLispUser = NULL;
#ifdef LISP_WINDOWS_OR_ANSI
	EnvUserProfile = EnvProgramData = EnvProgramFiles = EnvProgramFilesx86 = NULL;
#endif
	for (i = 0; i < Envc; i++) {
		ptr = Envv[i];
		envfront(ptr, "HOME", &EnvHome);
		envfront(ptr, LISPNAME "_HOME", &EnvLispHome);
		envfront(ptr, LISPNAME "_USER", &EnvLispUser);
#ifdef LISP_WINDOWS_OR_ANSI
		envfront(ptr, "USERPROFILE", &EnvUserProfile);
		envfront(ptr, "ProgramData", &EnvProgramData);
		envfront(ptr, "PROGRAMFILES", &EnvProgramFiles);
		envfront(ptr, "ProgramFiles(x86)", &EnvProgramFilesx86);
#endif
	}
}

static void free_envvar_variable(struct unimem **env)
{
	*env = NULL;
}

static void free_envvar(void)
{
	free_envvar_variable(&EnvHome);
	free_envvar_variable(&EnvLispHome);
	free_envvar_variable(&EnvLispUser);
#ifdef LISP_WINDOWS_OR_ANSI
	free_envvar_variable(&EnvUserProfile);
	free_envvar_variable(&EnvProgramData);
	free_envvar_variable(&EnvProgramFiles);
	free_envvar_variable(&EnvProgramFilesx86);
#endif
}

static void free_envarray(size_t size, struct envmemory **ptr)
{
	size_t i;

	if (ptr) {
		for (i = 0; i < size; i++)
			free_envmemory(ptr[i]);
		free(ptr);
	}
}


#ifdef LISP_WINDOWS_WIDE
/*
 *  main_wchar
 */
static struct envmemory **make_envarray_windows(const ctype *env, size_t *ret)
{
	struct envmemory **ptr, *e;
	const ctype *pos;
	size_t i, size, allsize;

	/* length */
	for (size = 0, pos = env; *pos; pos++, size++) {
		while (*pos) pos++;
	}

	/* allocate */
	allsize = sizeoft(struct envmemory *) * size;
	ptr = (struct envmemory **)malloc(allsize);
	if (ptr == NULL) {
		errormsg("malloc error.");
		return NULL;
	}
	memset(ptr, 0, allsize);

	pos = env;
	for (i = 0; i < size; i++) {
		e = make_envmemory(pos);
		if (e == NULL) goto error;
		ptr[i] = e;
		while (*pos) pos++;
	}
	*ret = size;
	return ptr;

error:
	free_envarray(size, ptr);
	return NULL;
}

static int main_windows(int argc, ctype *argv[], const ctype *env)
{
	int check;
	struct envmemory **ptr;
	size_t size;

	ptr = make_envarray_windows(env, &size);
	if (ptr == NULL) {
		errormsg("make_envarray_windows error.");
		return 1;
	}
	Envc = size;
	Envv = ptr;
	setenvvar();

	check = main_argv(argc, argv);
	if (check)
		errormsg("main_argv error.");
	free_envvar();
	free_envarray(Envc, Envv);
	Envc = 0;
	Envv = NULL;

	return check;
}

static int main_wchar_argv(const ctype *env)
{
	LPWSTR *argv;
	int argc, check;

	argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (argv == NULL) {
		errormsg("CommandLineToArgvW error.");
		return 1;
	}

	check = main_windows(argc, argv, env);
	if (check)
		errormsg("main_windows error.");
	if (LocalFree((HLOCAL)argv)) {
		errormsg("LocalFree error.");
		return 1;
	}

	return check;
}

static int main_wchar(void)
{
	int check;
	LPVOID ptr;

	ptr = GetEnvironmentStringsW();
	if (ptr == NULL) {
		errormsg("GetEnvironmentStringsW error.");
		return 1;
	}
	check = main_wchar_argv((const ctype *)ptr);
	if (check)
		errormsg("main_wchar_argv error.");
	if (FreeEnvironmentStrings(ptr) == FALSE) {
		errormsg("FreeEnvironmentStrings error.");
		return 1;
	}

	return check;
}


#else
/*
 *  main_char
 */
static struct envmemory **make_envarray(char *env[], size_t *ret)
{
	struct envmemory **ptr, *e;
	size_t i, size, allsize;

	/* length */
	for (size = 0; env[size]; size++)
		continue;

	/* allocate */
	allsize = sizeoft(struct envmemory *) * size;
	ptr = (struct envmemory **)malloc(allsize);
	if (ptr == NULL) {
		errormsg("malloc error.");
		return NULL;
	}
	memset(ptr, 0, allsize);

	for (i = 0; i < size; i++) {
		e = make_envmemory(env[i]);
		if (e == NULL) goto error;
		ptr[i] = e;
	}
	*ret = size;
	return ptr;

error:
	free_envarray(size, ptr);
	return NULL;
}

static int main_char(int argc, char *argv[], char *env[])
{
	int check;
	struct envmemory **ptr;
	size_t size;

	ptr = make_envarray(env, &size);
	if (ptr == NULL) {
		errormsg("make_envarray error.");
		return 1;
	}
	Envc = size;
	Envv = ptr;
	setenvvar();

	check = main_argv(argc, argv);
	if (check)
		errormsg("main_argv error.");
	free_envvar();
	free_envarray(Envc, Envv);
	Envc = 0;
	Envv = NULL;

	return check;
}
#endif


/*
 *  main_setbinary
 */
#ifdef LISP_WINDOWS_WIDE
#include <io.h>
#include <fcntl.h>

static int setbinary_FILE(FILE *file)
{
	int check;

	if (fflush(file)) {
		errormsg("fflush error (first).");
		return 1;
	}
	check = _fileno(file);
	if (check < 0) {
		errormsg("_fileno error: %d (%d).", check, errno);
		return 1;
	}
	check = _setmode(check, _O_BINARY);
	if (check < 0) {
		errormsg("_setmode error: %d (%d).", check, errno);
		return 1;
	}
	if (fflush(file)) {
		errormsg("fflush error (second).");
		return 1;
	}

	return 0;
}

static int main_setbinary(void)
{
	if (setbinary_FILE(stdin)) {
		errormsg("setbinary error: stdin.");
		return 1;
	}
	if (setbinary_FILE(stdout)) {
		errormsg("setbinary error: stdout.");
		return 1;
	}
	if (setbinary_FILE(stderr)) {
		errormsg("setbinary error: stderr.");
		return 1;
	}

	return 0;
}
#endif


/*
 *  main
 */
#ifdef LISP_WINMAIN
/* ARGSUSED0 */
int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR lpCmd, int nShow)
{
	if (setlocale(LC_ALL, "") == NULL) {
		errormsg("setlocale error.");
		return 1;
	}
	if (main_setbinary()) {
		errormsg("main_setbinary error.");
		return 1;
	}
	if (main_wchar()) {
		errormsg("main_wchar error.");
		return 1;
	}

	return Result;
}

#else

/* ARGSUSED0 */
int main(int argc, char *argv[], char *env[])
{
	MainArgc = argc;
	MainArgv = argv;
	MainEnv = env;

	if (setlocale(LC_ALL, "") == NULL) {
		errormsg("setlocale error.");
		return 1;
	}
#ifdef LISP_WINDOWS_WIDE
	if (main_setbinary()) {
		errormsg("main_setbinary error.");
		return 1;
	}
	if (main_wchar()) {
		errormsg("main_wchar error.");
		return 1;
	}
#else
	if (main_char(argc, argv, env)) {
		errormsg("main_char error.");
		return 1;
	}
#endif

	return Result;
}
#endif

