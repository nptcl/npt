#include "condition.h"
#include "encode.h"
#include "file.h"
#include "file_memory.h"
#include "memory.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"

#define PtrFileMemory(stream) ((struct filememory *)PtrDataStream(stream))


/*
 *  Common Function
 */
static void standard_constant_stream(addr *stream,
		enum StreamType type,
		void (*call)(struct filememory *))
{
	addr pos;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	call(fm);
	force_open_stream(pos, stream);
}

static void encode_standard_stream(addr pos)
{
	struct filememory *fm;
	struct FileEncode *encode;

	fm = PtrFileMemory(pos);
	encode = &(fm->encode);
	encode->type = EncodeType_utf8;
	encode->bom = EncodeBom_empty;
	encode->error = 0;
}

_g void make_standard_input(addr *stream)
{
	standard_constant_stream(stream,
			StreamType_BincharInput, standard_input_filememory);
	encode_standard_stream(*stream);
}

_g void make_standard_output(addr *stream)
{
	standard_constant_stream(stream,
			StreamType_BincharOutput, standard_output_filememory);
	encode_standard_stream(*stream);
}

_g void make_standard_error(addr *stream)
{
	standard_constant_stream(stream,
			StreamType_BincharOutput, standard_error_filememory);
	encode_standard_stream(*stream);
}

_g void update_standard_input(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	update_standard_input_filememory(PtrFileMemory(stream));
}

_g void update_standard_output(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	update_standard_output_filememory(PtrFileMemory(stream));
}

_g void update_standard_error(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	update_standard_error_filememory(PtrFileMemory(stream));
}

_g int script_header(addr stream)
{
	int check;
	byte a, b;
	struct filememory *fm;

	CheckType(stream, LISPTYPE_STREAM);
	fm = PtrFileMemory(stream);
	/* read UTF-8 BOM */
	if (readbom8_encode(fm) < 0)
		return end_filememory(fm);
	/* #\# */
	check = getc_filememory(fm, &a);
	if (check)
		return check;
	if (a != '#')
		return ungetc_filememory(fm, a);
	/* #\! */
	check = getc_filememory(fm, &b);
	if (check)
		return check;
	if (b != '!') {
		ungetc_filememory(fm, b);
		return ungetc_filememory(fm, a);
	}
	/* ... \n */
	for (;;) {
		check = getc_filememory(fm, &a);
		if (check)
			return check;
		if (a == 0x0A || a == 0x0D)
			break;
	}

	return 0;
}


/*
 *  input
 */
_g int open_input_stream_external(Execute ptr, addr *stream, addr file, addr format)
{
	addr check;

	/* ascii */
	GetConst(SYSTEM_ASCII, &check);
	if (format == check)
		return open_input_ascii_stream(ptr, stream, file);

	/* utf-8 */
	GetConst(SYSTEM_UTF_8, &check);
	if (format == check)
		return open_input_utf8_stream(ptr, stream, file);

	/* utf-8-bom */
	GetConst(SYSTEM_UTF_8_BOM, &check);
	if (format == check)
		return open_input_utf8bom_stream(ptr, stream, file);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &check);
	if (format == check)
		return open_input_utf16_stream(ptr, stream, file);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &check);
	if (format == check)
		return open_input_utf16le_stream(ptr, stream, file);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &check);
	if (format == check)
		return open_input_utf16be_stream(ptr, stream, file);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &check);
	if (format == check)
		return open_input_utf16lebom_stream(ptr, stream, file);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &check);
	if (format == check)
		return open_input_utf16bebom_stream(ptr, stream, file);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &check);
	if (format == check)
		return open_input_utf32_stream(ptr, stream, file);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &check);
	if (format == check)
		return open_input_utf32le_stream(ptr, stream, file);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &check);
	if (format == check)
		return open_input_utf32be_stream(ptr, stream, file);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &check);
	if (format == check)
		return open_input_utf32lebom_stream(ptr, stream, file);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &check);
	if (format == check)
		return open_input_utf32bebom_stream(ptr, stream, file);

	/* others */
	fmte("Invalid *external-format* value ~S.", format, NULL);
	return 1;
}

_g int open_input_stream(Execute ptr, addr *stream, addr file)
{
	addr format;

	GetConst(SYSTEM_EXTERNAL_FORMAT, &format);
	getspecialcheck_local(ptr, format, &format);

	return open_input_stream_external(ptr, stream, file, format);
}

_g void open_input_stream_error(Execute ptr, addr *stream, addr file)
{
	if (open_input_stream(ptr, stream, file))
		file_error(file);
}

static inline int inputstream(Execute ptr,
		addr *stream, addr file, enum StreamType type)
{
	addr pos;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	if (open_input_filememory(ptr, fm, file)) return 1;
	SetPathnameStream(pos, file);
	*stream = pos;

	return 0;
}

_g int open_input_binary_stream(Execute ptr, addr *stream, addr file)
{
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_BinaryInput))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_ascii_stream(Execute ptr, addr *stream, addr file)
{
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf8_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom8_encode(fm);
	if (check < 0) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf8bom_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom8_encode(fm);
	if (check < 0 || check == 0) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf16_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check < 0) {
		close_filememory(fm);
		return 1;
	}
	if (check == 1)
		fm->encode.type = EncodeType_utf16le;
	else
		fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf16le_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf16be_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf16lebom_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 1) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf16bebom_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 2) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf32_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check < 0) {
		close_filememory(fm);
		return 1;
	}
	if (check == 1)
		fm->encode.type = EncodeType_utf32le;
	else
		fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf32le_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf32be_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf32lebom_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 1) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_input_utf32bebom_stream(Execute ptr, addr *stream, addr file)
{
	int check;
	struct filememory *fm;

	if (inputstream(ptr, &file, file, StreamType_CharacterInput))
		return 1;
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 2) {
		close_filememory(fm);
		return 1;
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}


/*
 *  output
 */
_g int open_output_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	addr check, value;

	/* symbol */
	GetConst(SYSTEM_EXTERNAL_FORMAT, &value);
	getspecialcheck_local(ptr, value, &value);

	/* ascii */
	GetConst(SYSTEM_ASCII, &check);
	if (value == check)
		return open_output_ascii_stream(ptr, stream, file, mode);

	/* utf-8 */
	GetConst(SYSTEM_UTF_8, &check);
	if (value == check)
		return open_output_utf8_stream(ptr, stream, file, mode, 0);

	/* utf-8-bom */
	GetConst(SYSTEM_UTF_8_BOM, &check);
	if (value == check)
		return open_output_utf8_stream(ptr, stream, file, mode, 1);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &check);
	if (value == check)
		return open_output_utf16be_stream(ptr, stream, file, mode, 0);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &check);
	if (value == check)
		return open_output_utf16le_stream(ptr, stream, file, mode, 0);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &check);
	if (value == check)
		return open_output_utf16be_stream(ptr, stream, file, mode, 0);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &check);
	if (value == check)
		return open_output_utf16le_stream(ptr, stream, file, mode, 1);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &check);
	if (value == check)
		return open_output_utf16be_stream(ptr, stream, file, mode, 1);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &check);
	if (value == check)
		return open_output_utf32be_stream(ptr, stream, file, mode, 0);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &check);
	if (value == check)
		return open_output_utf32le_stream(ptr, stream, file, mode, 0);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &check);
	if (value == check)
		return open_output_utf32be_stream(ptr, stream, file, mode, 0);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &check);
	if (value == check)
		return open_output_utf32le_stream(ptr, stream, file, mode, 1);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &check);
	if (value == check)
		return open_output_utf32be_stream(ptr, stream, file, mode, 1);

	/* others */
	fmte("Invalid *external-format* value ~S.", value, NULL);
	return 1;
}

static inline int outputstream(Execute ptr,
		addr *stream, addr file, enum StreamType type, enum FileOutput mode)
{
	addr pos;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	if (open_output_filememory(ptr, fm, file, mode)) return 1;
	SetPathnameStream(pos, file);
	*stream = pos;

	return 0;
}

_g int open_output_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_BinaryOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_output_ascii_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_CharacterOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_output_utf8_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_CharacterOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_output_utf16le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_CharacterOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_output_utf16be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_CharacterOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_output_utf32le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_CharacterOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_output_utf32be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	if (outputstream(ptr, &file, file, StreamType_CharacterOutput, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}


/*
 *  io
 */
_g int open_io_stream(Execute ptr, addr *stream, addr file, enum FileOutput mode)
{
	addr check, value;

	/* symbol */
	GetConst(SYSTEM_EXTERNAL_FORMAT, &value);
	getspecialcheck_local(ptr, value, &value);

	/* ascii */
	GetConst(SYSTEM_ASCII, &check);
	if (value == check)
		return open_io_ascii_stream(ptr, stream, file, mode);

	/* utf-8 */
	GetConst(SYSTEM_UTF_8, &check);
	if (value == check)
		return open_io_utf8_stream(ptr, stream, file, mode);

	/* utf-8-bom */
	GetConst(SYSTEM_UTF_8_BOM, &check);
	if (value == check)
		return open_io_utf8bom_stream(ptr, stream, file, mode);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &check);
	if (value == check)
		return open_io_utf16_stream(ptr, stream, file, mode);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &check);
	if (value == check)
		return open_io_utf16le_stream(ptr, stream, file, mode);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &check);
	if (value == check)
		return open_io_utf16be_stream(ptr, stream, file, mode);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &check);
	if (value == check)
		return open_io_utf16lebom_stream(ptr, stream, file, mode);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &check);
	if (value == check)
		return open_io_utf16bebom_stream(ptr, stream, file, mode);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &check);
	if (value == check)
		return open_io_utf32_stream(ptr, stream, file, mode);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &check);
	if (value == check)
		return open_io_utf32le_stream(ptr, stream, file, mode);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &check);
	if (value == check)
		return open_io_utf32be_stream(ptr, stream, file, mode);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &check);
	if (value == check)
		return open_io_utf32lebom_stream(ptr, stream, file, mode);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &check);
	if (value == check)
		return open_io_utf32bebom_stream(ptr, stream, file, mode);

	/* others */
	fmte("Invalid *external-format* value ~S.", value, NULL);
	return 1;
}

static inline int iostream(Execute ptr,
		addr *stream, addr file, enum StreamType type, enum FileOutput mode)
{
	addr pos;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	if (open_io_filememory(ptr, fm, file, mode)) return 1;
	SetPathnameStream(pos, file);
	*stream = pos;

	return 0;
}

_g int open_io_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_BinaryIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_ascii_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf8_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf8bom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf16_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf16le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf16be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf16lebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf16bebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf32_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf32le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf32be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf32lebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}

_g int open_io_utf32bebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	if (iostream(ptr, &file, file, StreamType_CharacterIO, mode))
		return 1;
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return 1;
	}
	force_open_stream(file, stream);

	return 0;
}


/*
 *  stream function
 */
#define CheckFileStream(stream) Check(! file_stream_p(stream), "type error")

_g int close_stream_file(addr stream, int abort)
{
	struct filememory *fm;

	CheckFileStream(stream);
	if (open_stream_p(stream)) {
		fm = PtrFileMemory(stream);
		if (close_filememory(fm))
			fmte("close error", NULL);
		/* TODO: abort */
	}

	return 1;
}

_g int read_binary_file(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = read_filememory(fm, pos, size, ret);
	if (check < 0)
		fmte("read error", NULL);

	return check;
}

_g int readforce_binary_file(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = readforce_filememory(fm, pos, size, ret);
	if (check < 0)
		fmte("read error", NULL);

	return check;
}

_g int read_byte_file(addr stream, byte *c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = getc_filememory(fm, c);
	if (check < 0)
		fmte("getc error", NULL);

	return check;
}

_g int unread_byte_file(addr stream, byte c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = ungetc_filememory(fm, c);
	if (check < 0)
		fmte("unread_byte error", NULL);

	return check;
}

_g int write_binary_file(addr stream, const void *pos, size_t size, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = write_filememory(fm, pos, size, ret);
	if (check < 0)
		fmte("write error", NULL);

	return check;
}

_g int write_byte_file(addr stream, byte c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = putc_filememory(fm, c);
	if (check < 0)
		fmte("write_byte error", NULL);

	return check? -1: 0;
}


/*
 *  character
 */
_g int read_char_file(addr stream, unicode *c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = read_char_encode(fm, c);
	if (check < 0)
		fmte("read_char_encode error", NULL);

	return check? 1: 0;
}

_g int read_hang_file(addr stream, unicode *c, int *hang)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = read_hang_encode(fm, c, hang);
	if (check < 0)
		fmte("read_hang_encode error", NULL);

	return check? 1: 0;
}

_g void write_char_file(addr stream, unicode c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = write_char_encode(fm, c);
	if (check < 0)
		fmte("write_char_encode error", NULL);
}

_g int file_length_file(addr stream, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm))
		fmte("flush error.", NULL);
	check = file_length_filememory(fm, ret);
	if (check < 0)
		fmte("file-length error.", NULL);

	return check != 0;
}

_g int file_position_file(addr stream, size_t *ret)
{
	int check;
	struct filememory *fm;
	struct StructStream *ptr;
	size_t size, unread;

	/* file-memory position */
	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = file_position_filememory(fm, &size);
	if (check < 0)
		fmte("file-position error.", NULL);
	if (check)
		return 1;

	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		check = length_char_encode(fm, ptr->unread);
		if (check < 0)
			fmte("Invalid unread character ~S.", character_heapr(ptr->unread), NULL);
		unread = (size_t)check;
		if (size < unread)
			fmte("The stream ~S position is a minus value.", stream, NULL);
		size -= unread;
	}
	*ret = size;

	return 0;
}

_g int file_position_start_file(addr stream)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm))
		fmte("flush error.", NULL);
	check = file_position_start_filememory(fm);
	if (check < 0)
		fmte("file-position-start error.", NULL);
	if (check == 0)
		PtrStructStream(stream)->unread_check = 0;

	return check != 0;
}

_g int file_position_end_file(addr stream)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm))
		fmte("flush error.", NULL);
	check = file_position_end_filememory(fm);
	if (check < 0)
		fmte("file-position-end error.", NULL);
	if (check == 0)
		PtrStructStream(stream)->unread_check = 0;

	return check != 0;
}

_g int file_position_set_file(addr stream, size_t pos)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm))
		fmte("flush error.", NULL);
	check = file_position_set_filememory(fm, pos);
	if (check < 0)
		fmte("file-position-end error.", NULL);
	if (check == 0)
		PtrStructStream(stream)->unread_check = 0;

	return check != 0;
}

_g int file_character_length_file(addr stream, unicode u, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = length_char_encode(fm, u);
	if (check < 0)
		return 1;
	*ret = (size_t)check;

	return 0;
}

_g int file_string_length_file(addr stream, addr pos, size_t *ret)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	return length_string_encode(fm, pos, ret);
}

_g void external_format_file(addr stream, addr *ret)
{
	enum EncodeBom bom;
	enum EncodeType type;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	type = (enum EncodeType)fm->encode.type;
	bom = (enum EncodeBom)fm->encode.bom;
	switch (type) {
		case EncodeType_binary:
			GetConst(STREAM_BINARY_TYPE, ret);
			break;

		case EncodeType_ascii:
			GetConst(SYSTEM_ASCII, ret);
			break;

		case EncodeType_utf8:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_8_BOM, ret);
			else
				GetConst(SYSTEM_UTF_8, ret);
			break;

		case EncodeType_utf16le:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_16LE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_16LE, ret);
			break;

		case EncodeType_utf16be:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_16BE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_16BE, ret);
			break;

		case EncodeType_utf32le:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_32LE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_32LE, ret);
			break;

		case EncodeType_utf32be:
			if (bom == EncodeBom_exist)
				GetConst(SYSTEM_UTF_32BE_BOM, ret);
			else
				GetConst(SYSTEM_UTF_32BE, ret);
			break;

		case EncodeType_windows:
			GetConst(SYSTEM_WINDOWS, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}

_g int listen_file(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	if (PtrStructStream(stream)->unread_check)
		return 1;
	fm = PtrFileMemory(stream);
	return fm->cache;
}

_g void clear_input_file(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	fm = PtrFileMemory(stream);
	if (clear_input_filememory(fm))
		fmte("clear-input error.", NULL);
}

_g void finish_output_file(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm))
		fmte("flush-filememory error.", NULL);
}

_g void force_output_file(addr stream)
{
	finish_output_file(stream);
}

_g void clear_output_file(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (clear_output_filememory(fm))
		fmte("clear-output error.", NULL);
}

_g void exitpoint_file(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	exitpoint_filememory(fm);
}


/*
 *  core
 */
_g int save_stream_file(addr pos)
{
	struct filememory *fm;
	struct StructStream *ptr;

	fm = PtrFileMemory(pos);
	close_filememory(fm);
	ptr = PtrStructStream(pos);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;

	return 0;
}

_g int save_stream_system(addr pos)
{
	if (PtrFileMemory(pos)->system == filememory_stream)
		return save_stream_file(pos);
	else
		return 0;
}

