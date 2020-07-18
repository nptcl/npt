#include "character.h"
#include "condition.h"
#include "constant.h"
#include "console.h"
#include "encode.h"
#include "execute.h"
#include "file.h"
#include "file_type.h"
#include "file_memory.h"
#include "files.h"
#include "pathname.h"
#include "stream.h"
#include "symbol.h"
#include "typedef.h"

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
	force_open_stream(pos);
	*stream = pos;
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
 *  stream function
 */
#define CheckFileStream(stream) Check(! file_stream_p(stream), "type error")

_g void force_close_stream_file(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	if (open_stream_p(stream)) {
		fm = PtrFileMemory(stream);
		(void)close_filememory(fm);
		force_close_stream(stream);
	}
}

static int close_stream_abort(addr stream)
{
	addr check;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SYSTEM_CLOSE_ABORT, &check);
	getspecial_local(ptr, check, &check);
	if (check == Unbound || check == Nil)
		return 0;
	/* :abort t */
	pathname_designer_heap(ptr, stream, &check);
	return delete_file_files_(ptr, check);
}

_g int close_stream_file_(addr stream, addr *ret)
{
	int outputp;
	struct filememory *fm;

	CheckFileStream(stream);
	if (open_stream_p(stream)) {
		fm = PtrFileMemory(stream);
		outputp = (fm->direct == filememory_output);
		if (close_filememory(fm))
			return fmte_("close error", NULL);
		if (outputp)
			Return(close_stream_abort(stream));
	}

	return Result(ret, T);
}

_g int read_binary_file_(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = read_filememory(fm, pos, size, ret);
	if (check < 0)
		return fmte_("read error", NULL);
	if (check)
		return Result(ret, 0);

	return 0;
}

_g int readf_binary_file_(addr stream, void *pos, size_t size, size_t *ret)
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

_g int read_byte_file_(addr stream, byte *c, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = getc_filememory(fm, c);
	if (check < 0)
		return fmte_("getc error", NULL);

	return Result(ret, check);
}

_g int unread_byte_file_(addr stream, byte c)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (ungetc_filememory(fm, c))
		return fmte_("unread_byte error", NULL);

	return 0;
}

_g int write_binary_file_(addr stream, const void *pos, size_t size, size_t *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = write_filememory(fm, pos, size, ret);
	if (check)
		return fmte_("write error", NULL);

	return 0;
}

_g int write_byte_file_(addr stream, byte c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = putc_filememory(fm, c);
	if (check)
		return fmte_("write_byte error", NULL);

	return 0;
}


/*
 *  character
 */
_g int read_char_file_(addr stream, unicode *c, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	Return(read_char_encode_(fm, c, &check));
	if (check < 0)
		return fmte_("read_char_encode error", NULL);

	return Result(ret, check? 1: 0);
}

_g int read_hang_file_(addr stream, unicode *c, int *hang, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	Return(read_hang_encode_(fm, c, hang, &check));
	if (check < 0)
		return fmte_("read_hang_encode error", NULL);

	return Result(ret, check? 1: 0);
}

_g int write_char_file_(addr stream, unicode c)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = write_char_encode(fm, c);
	if (check)
		return fmte_("write_char_encode error", NULL);

	return 0;
}

_g int file_length_file_(addr stream, size_t *value, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*value = 0;
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_length_filememory(fm, value);
	if (check < 0) {
		*value = 0;
		*ret = 0;
		return fmte_("file-length error.", NULL);
	}

	return Result(ret, check);
}

_g int file_position_file_(addr stream, size_t *value, int *ret)
{
	int check;
	struct filememory *fm;
	struct StructStream *ptr;
	size_t size, unread;

	/* file-memory position */
	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = file_position_filememory(fm, &size);
	if (check < 0) {
		*value = 0;
		*ret = 0;
		return fmte_("file-position error.", NULL);
	}
	if (check) {
		*value = 0;
		return Result(ret, 1);
	}

	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		check = length_char_encode(fm, ptr->unread);
		if (check < 0) {
			*value = 0;
			*ret = 0;
			character_heap(&stream, ptr->unread);
			return fmte_("Invalid unread character ~S.", stream, NULL);
		}
		unread = (size_t)check;
		if (size < unread) {
			*value = 0;
			*ret = 0;
			return fmte_("The stream ~S position is a minus value.", stream, NULL);
		}
		size -= unread;
	}
	*value = size;
	return Result(ret, 0);
}

_g int file_position_start_file_(addr stream, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_position_start_filememory(fm);
	if (check < 0) {
		*ret = 0;
		return fmte_("file-position-start error.", NULL);
	}
	if (check == 0) {
		PtrStructStream(stream)->unread_check = 0;
	}

	return Result(ret, check);
}

_g int file_position_end_file_(addr stream, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_position_end_filememory(fm);
	if (check < 0) {
		*ret = 0;
		return fmte_("file-position-end error.", NULL);
	}
	if (check == 0) {
		PtrStructStream(stream)->unread_check = 0;
	}

	return Result(ret, check);
}

_g int file_position_set_file_(addr stream, size_t value, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	check = file_position_set_filememory(fm, value);
	if (check < 0) {
		*ret = 0;
		return fmte_("file-position-end error.", NULL);
	}
	if (check == 0) {
		PtrStructStream(stream)->unread_check = 0;
	}

	return Result(ret, check);
}

_g int file_charlen_file_(addr stream, unicode u, size_t *value, int *ret)
{
	int check;
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	check = length_char_encode(fm, u);
	if (check < 0)
		return Result(ret, 1);
	*value = (size_t)check;

	return Result(ret, 0);
}

_g int file_strlen_file_(addr stream, addr pos, size_t *value, int *ret)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	*ret = length_string_encode(fm, pos, value);
	return 0;
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

_g int listen_file_(addr stream, int *ret)
{
	struct filememory *fm;

	CheckFileStream(stream);
	if (PtrStructStream(stream)->unread_check)
		return Result(ret, 1);
	fm = PtrFileMemory(stream);
	return Result(ret, fm->cache);
}

_g int clear_input_file_(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	fm = PtrFileMemory(stream);
	if (clear_input_filememory(fm))
		return fmte_("clear-input error.", NULL);
	
	return 0;
}

_g int finish_output_file_(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm))
		return fmte_("flush-filememory error.", NULL);
	
	return 0;
}

_g int force_output_file_(addr stream)
{
	return finish_output_file_(stream);
}

_g int clear_output_file_(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (clear_output_filememory(fm))
		return fmte_("clear-output error.", NULL);

	return 0;
}

_g int exitpoint_file_(addr stream)
{
	struct filememory *fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	exitpoint_filememory(fm);
	return 0;
}

_g int termsize_file_(addr stream, size_t *value, int *ret)
{
	*ret = getwidth_console(value);
	return 0;
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

