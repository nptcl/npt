#include "condition.h"
#include "core.h"
#include "execute.h"
#include "file_memory.h"
#include "files.h"
#include "format.h"
#include "gc.h"
#include "main_string.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

#define CoreHeader (LISPNAME "CORE\0")
#define CoreHeaderSize (LISPNAMESIZE + 5)


/*
 *  save/load coreheader
 */
struct lisp_core_header {
	byte magic[CoreHeaderSize];
	uint16_t endian, a, b, c, arch;
};

static int savecore_header(filestream fm)
{
	struct lisp_core_header header;

	cleartype(header);
	memcpy(header.magic, CoreHeader, CoreHeaderSize);
	header.endian = 1;
	header.a = LISP_VERSION_A;
	header.b = LISP_VERSION_B;
	header.c = LISP_VERSION_C;
#ifdef LISP_64BIT
	header.arch = 1;
#else
	header.arch = 0;
#endif
	if (writecheck_filememory(fm, &header, sizeoft(header))) {
		Debug("writecheck error: header");
		return 1;
	}

	return 0;
}
static int loadcore_header(filestream fm)
{
	struct lisp_core_header header;

	if (readcheck_filememory(fm, &header, sizeoft(header))) {
		Debug("readcheck error: header");
		return 1;
	}
	if (memcmp(header.magic, CoreHeader, CoreHeaderSize) != 0) {
		Debug("loadcore_header: magic error");
		return 1;
	}
	if (header.endian != 1) {
		Debug("loadcore_header: endian error");
		return 1;
	}
	if (header.a != LISP_VERSION_A ||
			header.b != LISP_VERSION_B ||
			header.c != LISP_VERSION_C) {
		Debug("loadcore_header: version error");
		return 1;
	}
#ifdef LISP_64BIT
	if (header.arch != 1) {
		Debug("loadcore_header: arch error");
		return 1;
	}
#else
	if (header.arch != 0) {
		Debug("loadcore_header: arch error");
		return 1;
	}
#endif

	return 0;
}

/* save/load corefile */
static int savecore_file(filestream fm)
{
	if (savecore_header(fm)) {
		Debug("savecore_header error.");
		return 1;
	}
	if (save_lisp(fm)) {
		Debug("save_lisp error.");
		return 1;
	}

	return 0;
}
static int loadcore_file(filestream fm)
{
	if (loadcore_header(fm)) {
		Debug("loadcore_header error.");
		return 1;
	}
	if (load_lisp(fm)) {
		Debug("load_lisp error.");
		return 1;
	}

	return 0;
}


/* save/load root */
static int savecore_root(filestream fm)
{
	unsigned i;

	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (writeaddr_filememory(fm, lisp_root[i])) {
			Debug("writeaddr error: root");
			return 1;
		}
	}

	return 0;
}
static int loadcore_root(filestream fm)
{
	unsigned i;

	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (readaddr_filememory(fm, &(lisp_root[i]))) {
			Debug("readaddr error: root");
			return 1;
		}
	}

	/* nil, t */
	lisp_nil_object = lisp_root[LISPINDEX_NIL];
	lisp_t_object = lisp_root[LISPINDEX_T];
	Execute_Thread->control = lisp_nil_object;

	return 0;
}


/*
 *  make-core
 */
int savecore_execute_(Execute ptr, addr output, addr input, int exitp)
{
	addr symbol, file;

	if (Index_Thread != 0)
		return fmte_("Thread Index must be 0.", NULL);
	if (count_execute() != 1)
		return fmte_("Any child thread must be destroyed.", NULL);

	/* (setq system::*core-output* output) */
	if (output != Nil) {
		Return(name_physical_heap_(ptr, output, &file));
		Return(strvect_value_heap_(&file, file));
	}
	else {
		file = Nil;
	}
	GetConst(SYSTEM_CORE_OUTPUT, &symbol);
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, file);

	/* (setq system::*core-input* input) */
	if (input != Nil) {
		Return(name_physical_heap_(ptr, input, &file));
		Return(strvect_value_heap_(&file, file));
	}
	else {
		file = exitp? Nil: T;
	}
	GetConst(SYSTEM_CORE_INPUT, &symbol);
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, file);

	/* input check */
	if (input != Nil) {
		Return(probe_file_files_(ptr, &file, input));
		if (file == Nil)
			return fmte_("File is not found, ~S.", input, NULL);
	}

	/* invoke */
	return call_savecore_condition_(ptr);
}

static int savecore_stream(Execute ptr, filestream fm)
{
	int check;
	addr pos;

	GetConst(SYSTEM_CORE_OUTPUT, &pos);
	GetValueSymbol(pos, &pos);
	if (pos == Nil)
		return 1;
	if (open_output_filememory_(ptr->local, fm, pos, FileOutput_supersede, &check))
		Abort("file open error.");
	if (check)
		Abort("file open error.");

	return 0;
}

static void savecore_result(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_CORE_OUTPUT, &pos);
	GetValueSymbol(pos, &pos);
	format_stdout_(ptr, "~&Core file: ~A~%", pos, NULL);
}

static int savecore_open(Execute ptr, filestream fm)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	check = savecore_stream(ptr, fm);
	rollback_local(local, stack);

	return check;
}

int save_core(Execute ptr)
{
	struct filememory fm;

	/* open file */
	if (savecore_open(ptr, &fm))
		return 0;

	/* write file */
	gcexec(GcMode_Full);
	if (savecore_file(&fm)) {
		Debug("savecore_file error.");
		goto error;
	}

	/* root */
	if (savecore_root(&fm)) {
		Debug("savecore_root error.");
		goto error;
	}

	/* close file */
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
		return 1;
	}

	/* result output */
	savecore_result(ptr);
	return 0;

error:
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
	}
	return 1;
}

int load_core(const unicode *name, size_t size)
{
	struct filememory fm;

	if (input_unicode_filememory(&fm, name, size)) {
		/* Read error, try next core file */
		return -1;
	}
	if (loadcore_file(&fm)) {
		Debug("loadcore_file error.");
		goto error;
	}

	/* root */
	if (loadcore_root(&fm)) {
		Debug("loadcore_root error.");
		goto error;
	}

	/* stream */
	if (update_standard_stream()) {
		Debug("update_standard_stream error.");
		return 1;
	}

	/* close file */
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
		return 1;
	}
	return 0;

error:
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
	}
	return 1;
}

static int loadcore_reload_(Execute ptr, struct lispargv *argv)
{
	addr symbol, input;
	lispstringu file;
	unicode *str, c;
	size_t size, i;

	GetConst(SYSTEM_CORE_INPUT, &symbol);
	GetValueSymbol(symbol, &input);
	if (input == Unbound || input == Nil)
		return 0;
	if (input == T) {
		argv->reload = 1;
		argv->reload_core = NULL;
		return 0;
	}

	/* make lispstringu */
	if (! strvectp(input))
		return fmte_("The object ~S must be a strvect type.", input, NULL);
	strvect_length(input, &size);
	file = make_stringu(size + 1UL);
	if (file == NULL)
		return fmte_("make_stringu error.", NULL);
	str = file->ptr;
	for (i = 0; i < size; i++) {
		strvect_getc(input, i, &c);
		str[i] = c;
	}
	str[i] = 0;

	/* argv */
	argv->reload = 1;
	argv->reload_core = file;

	return 0;
}

int save_and_load_core_(Execute ptr, struct lispargv *argv, int *ret)
{
	Return(loadcore_reload_(ptr, argv));
	*ret = save_core(ptr);
	return 0;
}

