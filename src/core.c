#include "condition.h"
#include "core.h"
#include "execute.h"
#include "file_memory.h"
#include "gc.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "symbol.h"

#define CoreHeader (LISPNAME "CORE\0")
#define CoreHeaderSize (LISPNAMESIZE + 5)

/*
 *  write-corefile
 */
#define writesize(fm, value) writecheck_filememory(fm, &(value), sizeoft(value))

static int write_coreheader(struct filememory *fm)
{
	byte buffer[64];
	uint16_t v;

	/* magic number */
	if (writecheck_filememory(fm, CoreHeader, CoreHeaderSize)) return 1;
	/* endian check */
	v = 1;
	if (writesize(fm, v)) return 1;
	/* version */
	v = LISP_VERSION_A;
	if (writesize(fm, v)) return 1;
	v = LISP_VERSION_B;
	if (writesize(fm, v)) return 1;
	v = LISP_VERSION_C;
	if (writesize(fm, v)) return 1;
	/* arch */
#ifdef LISP_64BIT
	v = 1;
#else
	v = 0;
#endif
	if (writesize(fm, v)) return 1;
	/* padding */
	memset(buffer, 0, 14);
	if (writecheck_filememory(fm, buffer, 14)) return 1;

	return 0;
}

static int write_corefile(struct filememory *fm)
{
	if (write_coreheader(fm)) {
		Debug("write_coreheader error.");
		return 1;
	}
	if (save_lisp(fm)) {
		Debug("save_lisp error.");
		return 1;
	}

	return 1;
}


/*
 *  make-core
 */
void savecore_execute(addr path)
{
	addr symbol;
	Execute ptr;

	if (Index_Thread != 0)
		fmte("Thread Index must be 0.", NULL);
	if (count_execute() != 1)
		fmte("Any child thread must be destroyed.", NULL);
	ptr = Execute_Thread;
	pathname_designer_heap(ptr, path, &path);
	internchar(LISP_SYSTEM, "*SAVECORE*", &symbol);
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, path);
	exitthis(LISPCODE_SAVECORE);
}

static void make_corefile(Execute ptr, struct filememory *fm)
{
	addr symbol;

	/* open output */
	internchar(LISP_SYSTEM, "*SAVECORE*", &symbol);
	GetValueSymbol(symbol, &symbol);
	name_pathname_heap(ptr, symbol, &symbol);
	if (open_output_filememory(ptr, fm, symbol, FileOutput_supersede))
		fmte("file open error.", NULL);

	/* clean heap */
	SetValueSymbol(symbol, Nil);
	interncommon("*FEATURES*", &symbol);
	SetValueSymbol(symbol, Nil);
}

static int open_corefile(struct filememory *fm)
{
	lispcode code;
	LocalRoot local;
	LocalStack stack;
	Execute ptr;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	begin_code(ptr, &code);
	if (code_run_p(code))
		make_corefile(ptr, fm);
	end_code(ptr);
	rollback_local(local, stack);
	if (code_error_p(code)) {
		Debug("make_core error, interrupt.");
		return 1;
	}

	return 0;
}

int make_core(void)
{
	int result;
	struct filememory fm;

	/* open file */
	if (open_corefile(&fm)) {
		Debug("open_corefile error.");
		return 1;
	}

	/* write file */
	gcexec();
	result = write_corefile(&fm);
	if (result) {
		Debug("write_corefile error.");
		result = 1;
	}

	/* close file */
	close_filememory(&fm);

	return result;
}


/*
 *  load-core
 */
static inline int readcheck(struct filememory *fm, void *pos, size_t size)
{
	int check;
	size_t result;

	check = readforce_filememory(fm, pos, size, &result);
	if (check) return 1;
	if (size != result) return 1;

	return 0;
}

#define readsize(fm, value) readcheck(fm, &(value), sizeoft(value))

static int headercheck(struct filememory *fm)
{
	byte buffer[64];
	uint16_t v, a, b, c;

	/* magic number */
	if (readcheck(fm, buffer, CoreHeaderSize)) return 1;
	if (memcmp(buffer, CoreHeader, CoreHeaderSize) != 0) return 1;
	/* endian check */
	if (readsize(fm, v)) return 1;
	if (v != 1) {
		Debug("endian error.");
		return 1;
	}
	/* version */
	if (readsize(fm, a)) return 1;
	if (readsize(fm, b)) return 1;
	if (readsize(fm, c)) return 1;
	if (end_filememory(fm)) return 1;
	if (a != LISP_VERSION_A || b != LISP_VERSION_B || c != LISP_VERSION_C) return 1;
	/* arch */
	if (readsize(fm, v)) return 1;
#ifdef LISP_64BIT
	if (v != 1) {
		Debug("This core file is not 64bit arch.");
		return 1;
	}
#else
	if (v != 0) {
		Debug("This core file is not 32bit arch.");
		return 1;
	}
#endif
	if (end_filememory(fm)) return 1;
	/* padding */
	if (readcheck(fm, buffer, 14)) return 1;
	if (end_filememory(fm)) return 1;

	return 0;
}

int load_corefile(struct filememory *fm)
{
	if (headercheck(fm)) {
		Debug("headercheck error.");
		return 1;
	}
	if (load_lisp(fm)) {
		Debug("load_lisp error.");
		return 1;
	}

	return 1;
}

int load_core(const void *name, size_t size)
{
	int result;
	struct filememory fm;

	if (input_lowlevel_filememory(&fm, name)) {
		/* Read error, try next core file */
		return -1;
	}
	result = load_corefile(&fm);
	if (result) {
		Debug("load_corefile error.");
		result = 1;
	}
	if (close_filememory(&fm)) {
		Debug("close_filememory error.");
		result = 1;
	}

	return result;
}

