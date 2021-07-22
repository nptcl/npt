#include "arch.h"
#include "bignum.h"
#include "bignum_object.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
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
static int standard_constant_stream(addr *stream,
		enum StreamType type,
		int (*call)(filestream))
{
	addr pos;
	filestream fm;

	stream_heap(&pos, type, sizeoft(struct filememory));
	fm = PtrFileMemory(pos);
	if ((*call)(fm))
		return 1;
	force_open_stream(pos);
	*stream = pos;

	return 0;
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

int make_standard_input(addr *stream)
{
	if (standard_constant_stream(stream,
				StreamType_BincharInput,
				standard_input_filememory))
		return 1;
	encode_standard_stream(*stream);

	return 0;
}

int make_standard_output(addr *stream)
{
	if (standard_constant_stream(stream,
				StreamType_BincharOutput,
				standard_output_filememory))
		return 1;
	encode_standard_stream(*stream);

	return 0;
}

int make_standard_error(addr *stream)
{
	if (standard_constant_stream(stream,
				StreamType_BincharOutput,
				standard_error_filememory))
		return 1;
	encode_standard_stream(*stream);

	return 0;
}

int update_standard_input(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return update_standard_input_filememory(PtrFileMemory(stream));
}

int update_standard_output(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return update_standard_output_filememory(PtrFileMemory(stream));
}

int update_standard_error(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return update_standard_error_filememory(PtrFileMemory(stream));
}

int script_header(addr stream)
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
void force_close_stream_file(addr stream)
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

int close_stream_file_(addr stream, addr *ret)
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

int read_binary_file_(addr stream, void *pos, size_t size, size_t *ret)
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

int read_byte_file_type(addr stream, addr *ret)
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

int read_byte_file_(addr stream, addr *value, int *ret)
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

int unread_byte_file_(addr stream, byte c)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (ungetc_filememory(fm, c))
		return fmte_("unread_byte error", NULL);

	return 0;
}

int write_binary_file_(addr stream, const void *pos, size_t size, size_t *ret)
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

int write_byte_file_type_(filestream fm, addr pos)
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

int write_byte_file_(addr stream, addr pos)
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
int read_char_file_(addr stream, unicode *c, int *ret)
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

int read_hang_file_(addr stream, unicode *c, int *hang, int *ret)
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

int write_char_file_(addr stream, unicode c)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return write_char_buffering_(stream, c);

	return write_char_encode_(fm, c);
}

static void element_type_unsigned_byte(fixnum v, addr *ret)
{
	addr type, value;

	GetConst(COMMON_UNSIGNED_BYTE, &type);
	fixnum_heap(&value, v);
	list_heap(ret, type, value, NULL);
}

static void element_type_signed_byte(fixnum v, addr *ret)
{
	addr type, value;

	GetConst(COMMON_SIGNED_BYTE, &type);
	fixnum_heap(&value, v);
	list_heap(ret, type, value, NULL);
}

int element_type_file_(addr stream, addr *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	switch (fm->encode.type) {
		case EncodeType_binary:
			element_type_unsigned_byte(8, ret);
			break;

		case EncodeType_unsigned16:
			element_type_unsigned_byte(16, ret);
			break;

		case EncodeType_unsigned32:
			element_type_unsigned_byte(32, ret);
			break;

		case EncodeType_unsigned64:
			element_type_unsigned_byte(64, ret);
			break;

		case EncodeType_signed8:
			element_type_signed_byte(8, ret);
			break;

		case EncodeType_signed16:
			element_type_signed_byte(16, ret);
			break;

		case EncodeType_signed32:
			element_type_signed_byte(32, ret);
			break;

		case EncodeType_signed64:
			element_type_signed_byte(64, ret);
			break;

		default:
			GetConst(COMMON_CHARACTER, ret);
			break;
	}

	return 0;
}

int external_format_file_(addr stream, addr *ret)
{
	enum EncodeBom bom;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	bom = (enum EncodeBom)fm->encode.bom;
	switch (fm->encode.type) {
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
			GetConst(KEYWORD_DEFAULT, ret);
			break;
	}

	return 0;
}

static int file_length_file_value_(filestream fm, size_t *value, int *ret)
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

static size_t file_length_division(filestream fm, size_t size)
{
	switch (fm->encode.type) {
		case EncodeType_unsigned16:
		case EncodeType_signed16:
			return size / 2;

		case EncodeType_unsigned32:
		case EncodeType_signed32:
			return size / 4;

		case EncodeType_unsigned64:
		case EncodeType_signed64:
			return size / 8;

		default:
			return size;
	}
}

static size_t file_length_multiple(filestream fm, size_t size)
{
	switch (fm->encode.type) {
		case EncodeType_unsigned16:
		case EncodeType_signed16:
			return size * 2;

		case EncodeType_unsigned32:
		case EncodeType_signed32:
			return size * 4;

		case EncodeType_unsigned64:
		case EncodeType_signed64:
			return size * 8;

		default:
			return size;
	}
}

int file_length_file_type_(filestream fm, size_t *value, int *ret)
{
	int check;
	size_t size;

	Return(file_length_file_value_(fm, &size, &check));
	if (check) {
		*value = 0;
		return Result(ret, check);
	}

	*value = file_length_division(fm, size);
	return Result(ret, 0);
}

int file_length_file_(addr stream, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_length_buffering_(stream, value, ret);

	return file_length_file_type_(fm, value, ret);
}

static int file_position_file_unread_(addr stream, size_t *ret)
{
	int check;
	struct StructStream *ptr;
	filestream fm;
	addr pos;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check == 0)
		return Result(ret, 0);

	/* unread */
	fm = PtrFileMemory(stream);
	check = length_char_encode(fm, ptr->unread);
	if (check < 0) {
		*ret = 0;
		character_heap(&pos, ptr->unread);
		return fmte_("Invalid unread character ~S.", pos, NULL);
	}

	return Result(ret, (size_t)check);
}

int file_position_file_type_(addr stream, size_t *value, int *ret)
{
	int check;
	filestream fm;
	size_t size, unread;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	*value = 0;
	*ret = 0;

	/* file-memory position */
	check = file_position_filememory(fm, &size);
	if (check < 0)
		return fmte_("file-position error.", NULL);
	if (check)
		return Result(ret, 1);

	/* unread */
	Return(file_position_file_unread_(stream, &unread));
	size = file_length_division(fm, size);

	/* result */
	if (size < unread)
		return fmte_("The stream ~S position is a minus value.", stream, NULL);
	*value = size - unread;
	return Result(ret, 0);
}

int file_position_file_(addr stream, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_buffering_(stream, value, ret);

	return file_position_file_type_(stream, value, ret);
}

int file_position_start_file_type_(addr stream, int *ret)
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

int file_position_start_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_start_buffering_(stream, ret);

	return file_position_start_file_type_(stream, ret);
}

int file_position_end_file_type_(addr stream, int *ret)
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

int file_position_end_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_end_buffering_(stream, ret);

	return file_position_end_file_type_(stream, ret);
}

int file_position_set_file_type_(addr stream, size_t value, int *ret)
{
	int check;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (flush_filememory(fm)) {
		*ret = 0;
		return fmte_("flush error.", NULL);
	}
	value = file_length_multiple(fm, value);
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

int file_position_set_file_(addr stream, size_t value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return file_position_set_buffering_(stream, value, ret);

	return file_position_set_file_type_(stream, value, ret);
}

int file_charlen_file_(addr stream, unicode u, size_t *value, int *ret)
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

int file_strlen_file_(addr stream, addr pos, size_t *value, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	return length_string_encode_(fm, pos, value, ret);
}

int listen_file_(addr stream, int *ret)
{
	filestream fm;

	CheckFileStream(stream);
	if (PtrStructStream(stream)->unread_check)
		return Result(ret, 1);
	fm = PtrFileMemory(stream);

	/* eof */
	if (fm->mode ==  filememory_end)
		return Result(ret, 0);

	return Result(ret, fm->cache);
}

int clear_input_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	fm = PtrFileMemory(stream);
	if (clear_input_filememory(fm))
		return fmte_("clear-input error.", NULL);

	return 0;
}

int finish_output_file_(addr stream)
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

int force_output_file_(addr stream)
{
	return finish_output_file_(stream);
}

int clear_output_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (clear_output_filememory(fm))
		return fmte_("clear-output error.", NULL);

	return 0;
}

int exitpoint_file_(addr stream)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	if (fm->redirect)
		return exitpoint_buffering_(stream);

	exitpoint_filememory(fm);
	return 0;
}

int termsize_file_(addr stream, size_t *value, int *ret)
{
	unsigned width;

	*ret = getwidth_arch(&width, NULL);
	*value = (size_t)width;

	return 0;
}


/*
 *  core
 */
int save_stream_file(addr pos)
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

int save_stream_system(addr pos)
{
	if (PtrFileMemory(pos)->system == filememory_stream)
		return save_stream_file(pos);
	else
		return 0;
}

