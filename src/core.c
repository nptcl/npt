#include "condition.h"
#include "core.h"
#include "execute.h"
#include "file_memory.h"
#include "format.h"
#include "gc.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
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

static int save_coreheader(struct filememory *fm)
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
static int load_coreheader(struct filememory *fm)
{
	struct lisp_core_header header;

	if (readcheck_filememory(fm, &header, sizeoft(header))) {
		Debug("readcheck error: header");
		return 1;
	}
	if (memcmp(header.magic, CoreHeader, CoreHeaderSize) != 0) {
		Debug("load_coreheader: magic error");
		return 1;
	}
	if (header.endian != 1) {
		Debug("load_coreheader: endian error");
		return 1;
	}
	if (header.a != LISP_VERSION_A ||
			header.b != LISP_VERSION_B ||
			header.c != LISP_VERSION_C) {
		Debug("load_coreheader: version error");
		return 1;
	}
#ifdef LISP_64BIT
	if (header.arch != 1) {
		Debug("load_coreheader: arch error");
		return 1;
	}
#else
	if (header.arch != 0) {
		Debug("load_coreheader: arch error");
		return 1;
	}
#endif

	return 0;
}

/* save/load corefile */
static int save_corefile(struct filememory *fm)
{
	if (save_coreheader(fm)) {
		Debug("save_coreheader error.");
		return 1;
	}
	if (save_lisp(fm)) {
		Debug("save_lisp error.");
		return 1;
	}

	return 0;
}
static int load_corefile(struct filememory *fm)
{
	if (load_coreheader(fm)) {
		Debug("load_coreheader error.");
		return 1;
	}
	if (load_lisp(fm)) {
		Debug("load_lisp error.");
		return 1;
	}

	return 0;
}


/* save/load root */
static int save_root(struct filememory *fm)
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
static int load_root(struct filememory *fm)
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
_g int savecore_execute_(Execute ptr, addr file)
{
	addr symbol;

	if (Index_Thread != 0)
		return fmte_("Thread Index must be 0.", NULL);
	if (count_execute() != 1)
		return fmte_("Any child thread must be destroyed.", NULL);
	Return(pathname_designer_heap_(ptr, file, &file));
	Return(name_pathname_heap_(ptr, file, &file));
	GetConst(SYSTEM_SAVECORE_VALUE, &symbol);
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, file);
	exitthis(LISPCODE_SAVECORE);

	return 0;
}

static void save_core_stream(Execute ptr, struct filememory *fm)
{
	int check;
	addr pos;

	GetConst(SYSTEM_SAVECORE_VALUE, &pos);
	GetValueSymbol(pos, &pos);
	if (open_output_filememory_(ptr->local, fm, pos, FileOutput_supersede, &check))
		Abort("file open error.");
	if (check)
		Abort("file open error.");
}

static void save_core_result(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_SAVECORE_VALUE, &pos);
	GetValueSymbol(pos, &pos);
	format_stdout(ptr, "~&Core file: ~A~%", pos, NULL);
}

static int open_corefile(Execute ptr, struct filememory *fm)
{
	lispcode code;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	begin_setjmp(ptr, &code);
	if (code_run_p(code))
		save_core_stream(ptr, fm);
	end_setjmp(ptr);
	rollback_local(local, stack);
	if (code_error_p(code)) {
		Debug("save_core error, interrupt.");
		return 1;
	}

	return 0;
}

_g int save_core(Execute ptr)
{
	struct filememory fm;

	/* open file */
	if (open_corefile(ptr, &fm)) {
		Debug("open_corefile error.");
		return 1;
	}

	/* write file */
	gcexec(GcMode_Full);
	if (save_corefile(&fm)) {
		Debug("save_corefile error.");
		goto error;
	}

	/* root */
	if (save_root(&fm)) {
		Debug("save_root error.");
		goto error;
	}

	/* close file */
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
		return 1;
	}

	/* result output */
	save_core_result(ptr);
	return 0;

error:
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
	}
	return 1;
}

_g int load_core(const unicode *name, size_t size)
{
	struct filememory fm;

	if (input_unicode_filememory(&fm, name, size)) {
		/* Read error, try next core file */
		return -1;
	}
	if (load_corefile(&fm)) {
		Debug("load_corefile error.");
		goto error;
	}

	/* root */
	if (load_root(&fm)) {
		Debug("load_root error.");
		goto error;
	}

	/* stream */
	update_standard_stream();

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

