#include "bignum.h"
#include "bignum_object.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "console.h"
#include "encode.h"
#include "execute.h"
#include "file.h"
#include "file_buffering.h"
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
		void (*call)(filestream))
{
	addr pos;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	call(fm);
	force_open_stream(pos);
	*stream = pos;
}

static void encode_standard_stream(addr pos)
{
	filestream fm;
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
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	Check(fm->redirect, "redirect error");
	/* read UTF-8 BOM */
	if (readbom8_encode(stream) < 0)
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
_g void force_close_stream_file(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	if (! open_stream_p(stream))
		return;

	fm = PtrFileMemory(stream);
	(void)close_filememory(fm);
	force_close_stream(stream);
}

static int close_stream_abort_(addr stream)
{
	addr check;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(SYSTEM_CLOSE_ABORT, &check);
	getspecial_local(ptr, check, &check);
	if (check == Unbound || check == Nil)
		return 0;
	/* :abort t */
	Return(pathname_designer_heap_(ptr, stream, &check));
	return delete_file_files_(ptr, check);
}

_g int close_stream_file_(addr stream, addr *ret)
{
	int outputp;
	filestream fm;

	CheckFileStream(stream);
	if (! open_stream_p(stream))
		return Result(ret, T);

	/* buffering */
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return close_stream_buffering_(stream, ret);

	/* filememory */
	outputp = (fm->direct == filememory_output);
	if (close_filememory(fm)) {
		*ret = Nil;
		return fmte_("close error", NULL);
	}
	if (outputp) {
		Return(close_stream_abort_(stream));
	}

	return Result(ret, T);
}

_g int read_binary_file_(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_binary_buffering_(stream, pos, size, ret);

	check = readf_filememory(fm, pos, size, ret);
	if (check < 0)
		return fmte_("read error", NULL);

	return 0;
}


/* read-byte */
static int read_byte_file_u8(filestream fm, addr *ret)
{
	int check;
	byte v;

	check = getc_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_u16(filestream fm, addr *ret)
{
	int check;
	uint16_t v;

	check = read_u16_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_u32(filestream fm, addr *ret)
{
	int check;
	uint32_t v;

	check = read_u32_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
#ifdef LISP_64BIT
	fixnum_heap(ret, (fixnum)v);
#else
	integer_fixed_heap(ret, signplus_bignum, (fixed)v);
#endif

	return 0;
}

#ifdef LISP_64BIT
static int read_byte_file_u64(filestream fm, addr *ret)
{
	int check;
	uint64_t v;

	check = read_u64_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	integer_fixed_heap(ret, signplus_bignum, (fixed)v);

	return 0;
}
#endif

static int read_byte_file_s8(filestream fm, addr *ret)
{
	int check;
	signed char v;

	check = getc_filememory(fm, (byte *)&v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_s16(filestream fm, addr *ret)
{
	int check;
	int16_t v;

	check = read_s16_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

static int read_byte_file_s32(filestream fm, addr *ret)
{
	int check;
	int32_t v;

	check = read_s32_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}

#ifdef LISP_64BIT
static int read_byte_file_s64(filestream fm, addr *ret)
{
	int check;
	int64_t v;

	check = read_s64_filememory(fm, &v);
	if (check) {
		*ret = Nil;
		return check;
	}
	fixnum_heap(ret, (fixnum)v);

	return 0;
}
#endif

_g int read_byte_file_type(addr stream, addr *ret)
{
	filestream fm;

	fm = PtrFileMemory(stream);
	switch (fm->encode.type) {
		case EncodeType_binary:
			return read_byte_file_u8(fm, ret);

		case EncodeType_unsigned16:
			return read_byte_file_u16(fm, ret);

		case EncodeType_unsigned32:
			return read_byte_file_u32(fm, ret);

		case EncodeType_signed8:
			return read_byte_file_s8(fm, ret);

		case EncodeType_signed16:
			return read_byte_file_s16(fm, ret);

		case EncodeType_signed32:
			return read_byte_file_s32(fm, ret);

#ifdef LISP_ARCH_64BIT
		case EncodeType_unsigned64:
			return read_byte_file_u64(fm, ret);

		case EncodeType_signed64:
			return read_byte_file_s64(fm, ret);
#endif

		default:
			*ret = Nil;
			return -1;
	}
}

_g int read_byte_file_(addr stream, addr *value, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_byte_buffering_(stream, value, ret);

	check = read_byte_file_type(stream, value);
	if (check < 0)
		return fmte_("read-byte-file error", NULL);

	return Result(ret, check);
}

_g int unread_byte_file_(addr stream, byte c)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (ungetc_filememory(fm, c))
		return fmte_("unread_byte error", NULL);

	return 0;
}

_g int write_binary_file_(addr stream, const void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_binary_buffering_(stream, pos, size, ret);

	check = write_filememory(fm, pos, size, ret);
	if (check)
		return fmte_("write error", NULL);

	return 0;
}

/* write-byte */
static int write_byte_file_u8_(filestream fm, addr pos)
{
	fixnum v;

	if (GetFixnum_unsigned(pos, &v))
		goto error;
	if (0xFF < v)
		goto error;
	if (putc_filememory(fm, (byte)v))
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

static int write_byte_file_u16_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	uint16_t u16;
	size_t size;

	if (GetFixnum_unsigned(pos, &v))
		goto error;
	if (0xFFFF < v)
		goto error;

	u16 = (uint16_t)v;
	check = write_filememory(fm, (const void *)&u16, 2, &size);
	if (check)
		goto error;
	if (size != 2)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

#ifdef LISP_64BIT
static int write_byte_file_u32_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	uint32_t u32;
	size_t size;

	if (GetFixnum_unsigned(pos, &v))
		goto error;
	if (0xFFFFFFFF < v)
		goto error;

	u32 = (uint32_t)v;
	check = write_filememory(fm, (const void *)&u32, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#else
static int write_byte_file_u32_(filestream fm, addr pos)
{
	int check;
	fixed v;
	size_t size;

	if (getfixed1_integer(pos, &check, &v))
		goto error;
	if (check != signplus_bignum)
		goto error;

	check = write_filememory(fm, (const void *)&v, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

#ifdef LISP_64BIT
static int write_byte_file_u64_(filestream fm, addr pos)
{
	int check;
	fixed v;
	size_t size;

	if (getfixed1_integer(pos, &check, &v))
		goto error;
	if (check != signplus_bignum)
		goto error;

	check = write_filememory(fm, (const void *)&v, 8, &size);
	if (check)
		goto error;
	if (size != 8)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

static int write_byte_file_s8_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	int8_t u8;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (v < -0x80 || 0x7F < v)
		goto error;

	u8 = (int8_t)v;
	check = write_filememory(fm, (const void *)&u8, 1, &size);
	if (check)
		goto error;
	if (size != 1)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

static int write_byte_file_s16_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	int16_t u16;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (v < -0x8000 || 0x7FFF < v)
		goto error;

	u16 = (int16_t)v;
	check = write_filememory(fm, (const void *)&u16, 2, &size);
	if (check)
		goto error;
	if (size != 2)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}

#ifdef LISP_64BIT
static int write_byte_file_s32_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	int32_t u32;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;
	if (v < -0x80000000LL || 0x7FFFFFFFLL < v)
		goto error;

	u32 = (int32_t)v;
	check = write_filememory(fm, (const void *)&u32, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#else
static int write_byte_file_s32_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;

	check = write_filememory(fm, (const void *)&v, 4, &size);
	if (check)
		goto error;
	if (size != 4)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

#ifdef LISP_64BIT
static int write_byte_file_s64_(filestream fm, addr pos)
{
	int check;
	fixnum v;
	size_t size;

	if (GetFixnum_signed(pos, &v))
		goto error;

	check = write_filememory(fm, (const void *)&v, 8, &size);
	if (check)
		goto error;
	if (size != 8)
		goto error;
	return 0;

error:
	return fmte_("Cannot write the value ~S.", pos, NULL);
}
#endif

_g int write_byte_file_type_(filestream fm, addr pos)
{
	switch (fm->encode.type) {
		case EncodeType_binary:
			return write_byte_file_u8_(fm, pos);

		case EncodeType_unsigned16:
			return write_byte_file_u16_(fm, pos);

		case EncodeType_unsigned32:
			return write_byte_file_u32_(fm, pos);

		case EncodeType_signed8:
			return write_byte_file_s8_(fm, pos);

		case EncodeType_signed16:
			return write_byte_file_s16_(fm, pos);

		case EncodeType_signed32:
			return write_byte_file_s32_(fm, pos);

#ifdef LISP_ARCH_64BIT
		case EncodeType_unsigned64:
			return write_byte_file_u64_(fm, pos);

		case EncodeType_signed64:
			return write_byte_file_s64_(fm, pos);
#endif

		default:
			return fmte_("Invalid stream type.", NULL);
	}
}

_g int write_byte_file_(addr stream, addr pos)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_byte_buffering_(stream, pos);

	return write_byte_file_type_(fm, pos);
}


/*
 *  character
 */
_g int read_char_file_(addr stream, unicode *c, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_char_buffering_(stream, c, ret);

	Return(read_char_encode_(fm, c, &check));
	if (check < 0)
		return fmte_("read_char_encode error", NULL);

	return Result(ret, check? 1: 0);
}

_g int read_hang_file_(addr stream, unicode *c, int *hang, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return read_hang_buffering_(stream, c, hang, ret);

	Return(read_hang_encode_(fm, c, hang, &check));
	if (check < 0)
		return fmte_("read_hang_encode error", NULL);

	return Result(ret, check? 1: 0);
}

_g int write_char_file_(addr stream, unicode c)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_char_buffering_(stream, c);

	return write_char_encode_(fm, c);
}

_g int element_type_file_(addr stream, addr *ret)
{
	external_format_file(stream, ret);
	return 0;
}

_g int file_length_file_type_(filestream fm, size_t *value, int *ret)
{
	int check;

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

_g int file_length_file_(addr stream, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_length_buffering_(stream, value, ret);

	return file_length_file_type_(fm, value, ret);
}

_g int file_position_file_type_(addr stream, size_t *value, int *ret)
{
	int check;
	filestream fm;
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

_g int file_position_file_(addr stream, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_buffering_(stream, value, ret);

	return file_position_file_type_(stream, value, ret);
}

_g int file_position_start_file_type_(addr stream, int *ret)
{
	int check;
	filestream fm;

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

_g int file_position_start_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_start_buffering_(stream, ret);

	return file_position_start_file_type_(stream, ret);
}

_g int file_position_end_file_type_(addr stream, int *ret)
{
	int check;
	filestream fm;

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

_g int file_position_end_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_end_buffering_(stream, ret);

	return file_position_end_file_type_(stream, ret);
}

_g int file_position_set_file_type_(addr stream, size_t value, int *ret)
{
	int check;
	filestream fm;

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

_g int file_position_set_file_(addr stream, size_t value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_set_buffering_(stream, value, ret);

	return file_position_set_file_type_(stream, value, ret);
}

_g int file_charlen_file_(addr stream, unicode u, size_t *value, int *ret)
{
	int check;
	filestream fm;

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
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	return length_string_encode_(fm, pos, value, ret);
}

static void external_format_unsigned_byte(fixnum v, addr *ret)
{
	addr type, value;

	GetConst(COMMON_UNSIGNED_BYTE, &type);
	fixnum_heap(&value, v);
	list_heap(ret, type, value, NULL);
}

static void external_format_signed_byte(fixnum v, addr *ret)
{
	addr type, value;

	GetConst(COMMON_SIGNED_BYTE, &type);
	fixnum_heap(&value, v);
	list_heap(ret, type, value, NULL);
}

_g void external_format_file(addr stream, addr *ret)
{
	enum EncodeBom bom;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	bom = (enum EncodeBom)fm->encode.bom;
	switch (fm->encode.type) {
		case EncodeType_binary:
			external_format_unsigned_byte(8, ret);
			break;

		case EncodeType_unsigned16:
			external_format_unsigned_byte(16, ret);
			break;

		case EncodeType_unsigned32:
			external_format_unsigned_byte(32, ret);
			break;

		case EncodeType_unsigned64:
			external_format_unsigned_byte(64, ret);
			break;

		case EncodeType_signed8:
			external_format_signed_byte(8, ret);
			break;

		case EncodeType_signed16:
			external_format_signed_byte(16, ret);
			break;

		case EncodeType_signed32:
			external_format_signed_byte(32, ret);
			break;

		case EncodeType_signed64:
			external_format_signed_byte(64, ret);
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
	filestream fm;

	CheckFileStream(stream);
	if (PtrStructStream(stream)->unread_check)
		return Result(ret, 1);
	fm = PtrFileMemory(stream);
	return Result(ret, fm->cache);
}

_g int clear_input_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	fm = PtrFileMemory(stream);
	if (clear_input_filememory(fm))
		return fmte_("clear-input error.", NULL);

	return 0;
}

_g int finish_output_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return finish_output_buffering_(stream);

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
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (clear_output_filememory(fm))
		return fmte_("clear-output error.", NULL);

	return 0;
}

_g int exitpoint_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return exitpoint_buffering_(stream);

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
	filestream fm;
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

