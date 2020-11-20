#ifndef __FILE_TYPE_HEADER__
#define __FILE_TYPE_HEADER__

#include <stdio.h>
#include "define.h"
#include "typedef.h"

enum EncodeType {
	EncodeType_binary,
	EncodeType_unsigned16,
	EncodeType_unsigned32,
	EncodeType_unsigned64,
	EncodeType_signed8,
	EncodeType_signed16,
	EncodeType_signed32,
	EncodeType_signed64,
	EncodeType_ascii,
	EncodeType_utf8,
	EncodeType_utf16le,
	EncodeType_utf16be,
	EncodeType_utf32le,
	EncodeType_utf32be,
	EncodeType_windows,
	EncodeType_size
};

enum EncodeBom {
	EncodeBom_auto,
	EncodeBom_empty,
	EncodeBom_exist
};

enum FileOutput {
	FileOutput_supersede,
	FileOutput_append,
	FileOutput_overwrite
};

struct FileEncode {
	unsigned error : 1;
	unsigned create : 1;
	unsigned bom : 2;
	enum EncodeType type : 5;
	unicode code;
};


/*
 *  Mode
 */
#if defined LISP_ANSIC
#include <stdio.h>
typedef FILE *file_type;
#elif defined LISP_POSIX
typedef int file_type;
#elif defined LISP_WINDOWS
#include <windows.h>
typedef HANDLE file_type;
#else
/* default -> LISP_ANSIC */
#include <stdio.h>
typedef FILE *file_type;
#endif


/*
 *  filememory
 */
#ifdef LISP_DEBUG
#define FILEMEMORY_SIZE     8
#else
#define FILEMEMORY_SIZE     4096
#endif
#define FILEMEMORY_UNGETC_SIZE  16

enum filememory_system {
	filememory_stream,
	filememory_stdin,
	filememory_stdout,
	filememory_stderr
};

enum filememory_mode {
	filememory_normal,
	filememory_end,
	filememory_close,
	filememory_error
};

enum filememory_direct {
	filememory_input,
	filememory_output,
	filememory_io
};

struct filememory {
	unsigned cache : 1;
	unsigned readio : 1;
	unsigned redirect : 1;
	enum filememory_system system : 4;
	enum filememory_mode mode : 4;
	enum filememory_direct direct : 4;
	uint8_t ungetc;
	addr pos;
	file_type file;
	size_t index, size, now;
	struct FileEncode encode;
	byte ungetc_value[FILEMEMORY_UNGETC_SIZE];
	byte buffer[FILEMEMORY_SIZE];
};

typedef struct filememory *filestream;

#endif

