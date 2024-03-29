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
#include "stream_function.h"
#include "stream_memory.h"
#include "stream_open.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  input
 */
static inline int inputstream_(Execute ptr,
		addr *ret, addr file, enum StreamType type)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		open_input_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_input_filememory_(ptr->local, fm, name, &check));
	if (check)
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_input_binary_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

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

int open_input_unsigned16_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_unsigned32_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed8_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed16_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed32_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

#ifdef LISP_64BIT
int open_input_unsigned64_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_signed64_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_BinaryInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}
#endif

int open_input_ascii_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

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

static void open_input_close_filememory(addr file)
{
	filestream fm;
	addr prev, mem;

	fm = PtrFileMemory(file);
	if (fm->redirect == 0) {
		close_filememory(fm);
		return;
	}

	/* redirect */
	GetPathnameStream(file, &mem);
	Check(! memory_stream_p(mem), "type error");
	prev = fm->pos;
	fm->pos = mem;
	close_filememory(fm);
	fm->pos = prev;
}

int open_input_utf8_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom8_encode(file);
	if (check < 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf8bom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom8_encode(file);
	if (check < 0 || check == 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check < 0) {
		open_input_close_filememory(file);
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

int open_input_utf16le_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16be_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16lebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 1) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf16bebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom16_encode(file);
	if (check != 2) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check < 0) {
		open_input_close_filememory(file);
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

int open_input_utf32le_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32be_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 0) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32lebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 1) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_utf32bebom_stream_(Execute ptr, addr *ret, addr file)
{
	int check;
	filestream fm;

	Return(inputstream_(ptr, &file, file, StreamType_CharacterInput));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	check = readbom32_encode(file);
	if (check != 2) {
		open_input_close_filememory(file);
		return Result(ret, NULL);
	}
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_input_stream_(Execute ptr, addr *ret, addr file, addr format)
{
	enum Stream_Open_External type;

	Return(open_external_format_(ptr, format, &type));
	switch (type) {
		case Stream_Open_External_Default:
		case Stream_Open_External_Utf8:
			return open_input_utf8_stream_(ptr, ret, file);

		case Stream_Open_External_Utf8Bom:
			return open_input_utf8bom_stream_(ptr, ret, file);

		case Stream_Open_External_Ascii:
			return open_input_ascii_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16:
			return open_input_utf16_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16Le:
			return open_input_utf16le_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16Be:
			return open_input_utf16be_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16LeBom:
			return open_input_utf16lebom_stream_(ptr, ret, file);

		case Stream_Open_External_Utf16BeBom:
			return open_input_utf16bebom_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32:
			return open_input_utf32_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32Le:
			return open_input_utf32le_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32Be:
			return open_input_utf32be_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32LeBom:
			return open_input_utf32lebom_stream_(ptr, ret, file);

		case Stream_Open_External_Utf32BeBom:
			return open_input_utf32bebom_stream_(ptr, ret, file);

		case Stream_Open_External_Error:
		default:
			*ret = Nil;
			return fmte_("Invalid external-format ~S.", format, NULL);
	};
}

int open_input_stream_error_(Execute ptr, addr *ret, addr file, addr format)
{
	addr pos;

	Return(open_input_stream_(ptr, &pos, file, format));
	if (pos == NULL) {
		*ret = Nil;
		return call_simple_file_error_va_(ptr, file,
				"Cannot open file, ~S.", file, NULL);
	}

	return Result(ret, pos);
}


/*
 *  output
 */
static int open_redirect_supersede_(addr file, enum FileOutput mode)
{
	int check;

	switch (mode) {
		case FileOutput_supersede:
			return clear_memory_stream_(file);

		case FileOutput_append:
			Return(file_position_end_stream_(file, &check));
			break;

		case FileOutput_overwrite:
			Return(file_position_start_stream_(file, &check));
			break;

		default:
			return clear_memory_stream_(file);
	}
	if (check)
		return fmte_("Cannot set a file-position ~S.", file, NULL);

	return 0;
}

static inline int outputstream_(Execute ptr,
		addr *ret, addr file, enum StreamType type, enum FileOutput mode)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		Return(open_redirect_supersede_(file, mode));
		open_output_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_output_filememory_(ptr->local, fm, name, mode, &check));
	if (check)
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_output_binary_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_output_unsigned16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_unsigned32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

#ifdef LISP_64BIT
int open_output_unsigned64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_output_signed64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_BinaryOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}
#endif

int open_output_ascii_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_output_utf8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf16le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf16be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf32le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}

int open_output_utf32be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode, int bomp)
{
	filestream fm;

	Return(outputstream_(ptr, &file, file, StreamType_CharacterOutput, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = bomp? EncodeBom_exist: EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	if (bomp) {
		Return(writebom_encode_(file));
	}

	return Result(ret, file);
}


/*
 *  io
 */
static inline int iostream_(Execute ptr,
		addr *ret, addr file, enum StreamType type, enum FileOutput mode)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		Return(open_redirect_supersede_(file, mode));
		open_io_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_io_filememory_(ptr->local, fm, name, mode, &check));
	if (check)
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_io_binary_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_unsigned16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_unsigned32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed8;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed16;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed32;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

#ifdef LISP_64BIT
int open_io_unsigned64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_unsigned64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}

int open_io_signed64_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_BinaryIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_signed64;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);

	return Result(ret, file);
}
#endif

int open_io_ascii_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf8_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf8bom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_exist;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf16_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf16le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf16be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf16lebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf16bebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf32_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf32le_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf32be_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

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

int open_io_utf32lebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}

int open_io_utf32bebom_stream_(Execute ptr, addr *ret,
		addr file, enum FileOutput mode)
{
	filestream fm;

	Return(iostream_(ptr, &file, file, StreamType_CharacterIO, mode));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	force_open_stream(file);
	Return(writebom_encode_(file));

	return Result(ret, file);
}


/*
 *  probe
 */
static inline int probestream_(Execute ptr, addr *ret, addr file, enum StreamType type)
{
	int check;
	addr pos, name;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	SetPathnameStream(pos, file);
	fm = PtrFileMemory(pos);

	/* stream */
	if (memory_stream_p(file)) {
		open_input_redirect_filememory_(fm, file);
		return Result(ret, pos);
	}

	/* pathname */
	Return(name_physical_local_(ptr, file, &name));
	Return(open_input_filememory_(ptr->local, fm, name, &check));
	if (check)
		return Result(ret, NULL);
	if (close_filememory(fm))
		return Result(ret, NULL);

	return Result(ret, pos);
}

int open_probe_stream_(Execute ptr, addr *ret, addr file)
{
	filestream fm;

	Return(probestream_(ptr, &file, file, StreamType_Probe));
	if (file == NULL)
		return Result(ret, NULL);
	fm = PtrFileMemory(file);
	fm->encode.type = EncodeType_ascii;
	fm->encode.bom = EncodeBom_empty;
	fm->encode.error = 1;
	/* close */
	fm->mode = filememory_close;
	force_close_stream(file);

	return Result(ret, file);
}

