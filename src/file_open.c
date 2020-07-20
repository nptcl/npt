#include "condition.h"
#include "constant.h"
#include "encode.h"
#include "execute.h"
#include "file.h"
#include "file_memory.h"
#include "file_open.h"
#include "file_type.h"
#include "pathname.h"
#include "stream.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  input
 */
static inline int inputstream_(Execute ptr,
		addr *ret, addr file, enum StreamType type)
{
	addr pos, name;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	Return(name_pathname_local_(ptr, file, &name));
	if (open_input_filememory(ptr->local, fm, name))
		return Result(ret, NULL);
	SetPathnameStream(pos, file);

	return Result(ret, pos);
}

_g int open_input_binary_stream_(Execute ptr, addr *ret, addr file)
{
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_ascii_stream_(Execute ptr, addr *ret, addr file)
{
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf8_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom8_encode(fm);
	if (check < 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf8bom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom8_encode(fm);
	if (check < 0 || check == 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf16_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check < 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	if (check == 1)
		fm->encode.type = EncodeType_utf16le;
	else
		fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf16le_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf16be_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf16lebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 1) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf16bebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(fm);
	if (check != 2) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf32_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check < 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	if (check == 1)
		fm->encode.type = EncodeType_utf32le;
	else
		fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf32le_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf32be_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 0) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf32lebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 1) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_utf32bebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	struct filememory *fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(fm);
	if (check != 2) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_input_stream_external_(Execute ptr, addr *ret, addr file, addr format)
{
	addr check;

	/* ascii */
	GetConst(SYSTEM_ASCII, &check);
	if (format == check)
		return open_input_ascii_stream_(ptr, ret, file);

	/* utf-8 */
	GetConst(SYSTEM_UTF_8, &check);
	if (format == check)
		return open_input_utf8_stream_(ptr, ret, file);

	/* utf-8-bom */
	GetConst(SYSTEM_UTF_8_BOM, &check);
	if (format == check)
		return open_input_utf8bom_stream_(ptr, ret, file);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &check);
	if (format == check)
		return open_input_utf16_stream_(ptr, ret, file);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &check);
	if (format == check)
		return open_input_utf16le_stream_(ptr, ret, file);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &check);
	if (format == check)
		return open_input_utf16be_stream_(ptr, ret, file);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &check);
	if (format == check)
		return open_input_utf16lebom_stream_(ptr, ret, file);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &check);
	if (format == check)
		return open_input_utf16bebom_stream_(ptr, ret, file);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &check);
	if (format == check)
		return open_input_utf32_stream_(ptr, ret, file);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &check);
	if (format == check)
		return open_input_utf32le_stream_(ptr, ret, file);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &check);
	if (format == check)
		return open_input_utf32be_stream_(ptr, ret, file);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &check);
	if (format == check)
		return open_input_utf32lebom_stream_(ptr, ret, file);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &check);
	if (format == check)
		return open_input_utf32bebom_stream_(ptr, ret, file);

	/* others */
	return fmte_("Invalid *external-format* value ~S.", format, NULL);
}

_g int open_input_stream_(Execute ptr, addr *ret, addr file)
{
	addr format;

	GetConst(SYSTEM_EXTERNAL_FORMAT, &format);
	getspecialcheck_local(ptr, format, &format);

	return open_input_stream_external_(ptr, ret, file, format);
}

_g int open_input_stream_error_(Execute ptr, addr *ret, addr file)
{
	addr pos;

	Return(open_input_stream_(ptr, &pos, file));
	if (pos == NULL)
		return call_file_error_(ptr, file);

	return Result(ret, pos);
}


/*
 *  output
 */
static inline int outputstream_(Execute ptr,
		addr *ret, addr file, enum StreamType type, enum FileOutput mode)
{
	addr pos, name;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	Return(name_pathname_local_(ptr, file, &name));
	if (open_output_filememory(ptr->local, fm, name, mode))
		return Result(ret, NULL);
	SetPathnameStream(pos, file);

	return Result(ret, pos);;
}

_g int open_output_binary_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_ascii_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_utf8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_utf16le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_utf16be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_utf32le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_utf32be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	struct filememory *fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	if (bomp && writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_output_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	addr check, value;

	/* symbol */
	GetConst(SYSTEM_EXTERNAL_FORMAT, &value);
	getspecialcheck_local(ptr, value, &value);

	/* ascii */
	GetConst(SYSTEM_ASCII, &check);
	if (value == check)
		return open_output_ascii_stream_(ptr, ret, file, mode);

	/* utf-8 */
	GetConst(SYSTEM_UTF_8, &check);
	if (value == check)
		return open_output_utf8_stream_(ptr, ret, file, mode, 0);

	/* utf-8-bom */
	GetConst(SYSTEM_UTF_8_BOM, &check);
	if (value == check)
		return open_output_utf8_stream_(ptr, ret, file, mode, 1);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &check);
	if (value == check)
		return open_output_utf16be_stream_(ptr, ret, file, mode, 0);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &check);
	if (value == check)
		return open_output_utf16le_stream_(ptr, ret, file, mode, 0);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &check);
	if (value == check)
		return open_output_utf16be_stream_(ptr, ret, file, mode, 0);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &check);
	if (value == check)
		return open_output_utf16le_stream_(ptr, ret, file, mode, 1);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &check);
	if (value == check)
		return open_output_utf16be_stream_(ptr, ret, file, mode, 1);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &check);
	if (value == check)
		return open_output_utf32be_stream_(ptr, ret, file, mode, 0);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &check);
	if (value == check)
		return open_output_utf32le_stream_(ptr, ret, file, mode, 0);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &check);
	if (value == check)
		return open_output_utf32be_stream_(ptr, ret, file, mode, 0);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &check);
	if (value == check)
		return open_output_utf32le_stream_(ptr, ret, file, mode, 1);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &check);
	if (value == check)
		return open_output_utf32be_stream_(ptr, ret, file, mode, 1);

	/* others */
	return fmte_("Invalid *external-format* value ~S.", value, NULL);
}


/*
 *  io
 */
static inline int iostream_(Execute ptr,
		addr *ret, addr file, enum StreamType type, enum FileOutput mode)
{
	addr pos, name;
	struct filememory *fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	Return(name_pathname_local_(ptr, file, &name));
	if (open_io_filememory(ptr->local, fm, name, mode))
		return Result(ret, NULL);
	SetPathnameStream(pos, file);

	return Result(ret, pos);;
}

_g int open_io_binary_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_binary;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_ascii_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf8bom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf16le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf16be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf16lebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf16bebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf32le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf32be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf32lebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_utf32bebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	struct filememory *fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	if (writebom_encode(fm)) {
		close_filememory(fm);
		return Result(ret, NULL);
	}
	force_open_stream(file);

	return Result(ret, file);
}

_g int open_io_stream_(Execute ptr, addr *ret, addr file, enum FileOutput mode)
{
	addr check, value;

	/* symbol */
	GetConst(SYSTEM_EXTERNAL_FORMAT, &value);
	getspecialcheck_local(ptr, value, &value);

	/* ascii */
	GetConst(SYSTEM_ASCII, &check);
	if (value == check)
		return open_io_ascii_stream_(ptr, ret, file, mode);

	/* utf-8 */
	GetConst(SYSTEM_UTF_8, &check);
	if (value == check)
		return open_io_utf8_stream_(ptr, ret, file, mode);

	/* utf-8-bom */
	GetConst(SYSTEM_UTF_8_BOM, &check);
	if (value == check)
		return open_io_utf8bom_stream_(ptr, ret, file, mode);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &check);
	if (value == check)
		return open_io_utf16_stream_(ptr, ret, file, mode);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &check);
	if (value == check)
		return open_io_utf16le_stream_(ptr, ret, file, mode);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &check);
	if (value == check)
		return open_io_utf16be_stream_(ptr, ret, file, mode);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &check);
	if (value == check)
		return open_io_utf16lebom_stream_(ptr, ret, file, mode);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &check);
	if (value == check)
		return open_io_utf16bebom_stream_(ptr, ret, file, mode);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &check);
	if (value == check)
		return open_io_utf32_stream_(ptr, ret, file, mode);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &check);
	if (value == check)
		return open_io_utf32le_stream_(ptr, ret, file, mode);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &check);
	if (value == check)
		return open_io_utf32be_stream_(ptr, ret, file, mode);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &check);
	if (value == check)
		return open_io_utf32lebom_stream_(ptr, ret, file, mode);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &check);
	if (value == check)
		return open_io_utf32bebom_stream_(ptr, ret, file, mode);

	/* others */
	return fmte_("Invalid *external-format* value ~S.", value, NULL);
}
