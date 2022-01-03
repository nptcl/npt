#include "bignum_output.h"
#include "character_queue.h"
#include "condition.h"
#include "condition_define.h"
#include "constant.h"
#include "file_open.h"
#include "files.h"
#include "integer.h"
#include "local.h"
#include "pathname_object.h"
#include "stream_function.h"
#include "stream_object.h"
#include "stream_open.h"
#include "strtype.h"
#include "subtypep.h"
#include "type_parse.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  upgraded-open-element-type
 */
int upgrade_open_element_type_stream_(addr var, addr *ret)
{
	*ret = Nil;
	return 0;
}

int open_element_stream_(Execute ptr, addr value, enum Stream_Open_Element *ret)
{
	int result;
	addr check, type;

	/* default */
	if (value == Unbound)
		return Result(ret, Stream_Open_Element_Character);

	/* :default */
	GetConst(KEYWORD_DEFAULT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Character);

	/* unsigned-byte */
	GetConst(COMMON_UNSIGNED_BYTE, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Unsigned8);

	/* character */
	GetConst(COMMON_CHARACTER, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Character);

	/* type */
	if (value == Nil)
		goto error;
	if (parse_type(ptr, &check, value, Nil))
		goto error;

	/* Unicode */
	GetTypeTable(&type, Character);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Character);

	/* bit */
	GetTypeTable(&type, Bit);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned8);

	/* (unsigned-byte 8) */
	GetTypeTable(&type, Unsigned8);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned8);

	/* (unsigned-byte 16) */
	GetTypeTable(&type, Unsigned16);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned16);

	/* (unsigned-byte 32) */
	GetTypeTable(&type, Unsigned32);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned32);

#ifdef LISP_ARCH_64BIT
	/* (unsigned-byte 64) 64-bit only */
	GetTypeTable(&type, Unsigned64);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Unsigned64);
#endif

	/* (signed-byte 8) */
	GetTypeTable(&type, Signed8);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed8);

	/* (signed-byte 16) */
	GetTypeTable(&type, Signed16);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed16);

	/* (signed-byte 32) */
	GetTypeTable(&type, Signed32);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed32);

#ifdef LISP_ARCH_64BIT
	/* (signed-byte 64) 64-bit only */
	GetTypeTable(&type, Signed64);
	Return(subtypep_check_(ptr, check, type, Nil, &result, NULL));
	if (result)
		return Result(ret, Stream_Open_Element_Signed64);
#endif

	/* error */
error:
	*ret = Stream_Open_Element_Character;
	return fmte_("Invalid :element-type value ~S.", value, NULL);
}


/*
 *  open
 */
static int open_make_empty_stream_(Execute ptr, addr pos)
{
	int ignore;
	addr stream;

	/* memory-stream */
	if (streamp(pos))
		return file_position_start_stream_(pos, &ignore);

	/* pathname */
	Return(open_output_binary_stream_(ptr, &stream, pos, FileOutput_supersede));
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot create file, ~S.", pos, NULL);
	}

	return close_stream_(stream, NULL);
}

static int open_probe_file_stream_(Execute ptr, addr pos, int *ret)
{
	/* memory-stream */
	if (streamp(pos))
		return Result(ret, 1); /* always exist */

	/* pathname */
	Return(probe_file_files_(ptr, &pos, pos));
	return Result(ret, pos != Nil);
}

static int open_if_does_not_exist_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfDoesNot value, int createp, int *existp)
{
	int check;

	Return(open_probe_file_stream_(ptr, pos, &check));
	if (check)
		return Result(existp, 0);

	switch (value) {
		case Stream_Open_IfDoesNot_Create:
			if (createp) {
				Return(open_make_empty_stream_(ptr, pos));
			}
			return Result(existp, 0);

		case Stream_Open_IfDoesNot_Error:
			return call_simple_file_error_va_(ptr, pos,
					"File ~S is not exist.", pos, NULL);

		case Stream_Open_IfDoesNot_Nil:
			*ret = Nil;
			return Result(existp, 1);

		default:
			return fmte_("Invalid :if-does-not-exist value.", NULL);
	}
}

static int open_if_exists_pathname_stream_(LocalRoot local,
		addr pos, size_t i, addr *ret)
{
	addr queue, type;

	copy_pathname_alloc(local, &pos, pos);
	charqueue_local(local, &queue, 0);
	GetTypePathname(pos, &type);
	if (stringp(type)) {
		Return(pushstring_charqueue_local_(local, queue, type));
		Return(pushchar_charqueue_local_(local, queue, "."));
	}
	make_index_integer_local(local, &type, i);
	Return(decimal_charqueue_integer_local_(local, type, queue));
	make_charqueue_local(local, queue, &type);
	SetTypePathname(pos, type);

	return Result(ret, pos);
}

static int open_if_exists_rename_stream_(Execute ptr, addr pos)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, ret1, ret2, ret3;
	size_t i;

	/* memory-stream */
	if (streamp(pos))
		return 0;

	/* pathname */
	Return(open_probe_file_stream_(ptr, pos, &check));
	if (! check)
		return 0;

	/* make pathname */
	local = ptr->local;
	push_local(local, &stack);
	for (i = 0; ; i++) {
		Return(open_if_exists_pathname_stream_(local, pos, i, &path));
		Return(open_probe_file_stream_(ptr, path, &check));
		if (! check)
			break;
	}

	/* rename */
	Return(rename_file_files_(ptr, &ret1, &ret2, &ret3, pos, path));
	rollback_local(local, stack);
	return 0;
}

static int open_if_exists_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfExists value,
		enum FileOutput *mode,
		int *existp)
{
	int check;

	Return(open_probe_file_stream_(ptr, pos, &check));
	if (! check) {
		*mode = FileOutput_supersede;
		return Result(existp, 0);
	}
	switch (value) {
		case Stream_Open_IfExists_Error:
			*mode = FileOutput_supersede;
			return call_simple_file_error_va_(ptr, pos,
					"File ~S already exists.", pos, NULL);

		case Stream_Open_IfExists_RenameAndDelete:
		case Stream_Open_IfExists_NewVersion:
		case Stream_Open_IfExists_Supersede:
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Rename:
			Return(open_if_exists_rename_stream_(ptr, pos));
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Overwrite:
			*mode = FileOutput_overwrite;
			break;

		case Stream_Open_IfExists_Append:
			*mode = FileOutput_append;
			break;

		case Stream_Open_IfExists_Nil:
			*ret = Nil;
			return Result(existp, 1);

		default:
			*mode = FileOutput_supersede;
			*existp = 0;
			return fmte_("Invalid :if-exist value.", NULL);
	}

	return Result(existp, 0);
}

static int open_external_input_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Ascii:
			return open_input_ascii_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf8:
			return open_input_utf8_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf8Bom:
			return open_input_utf8bom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16:
			return open_input_utf16_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16Le:
			return open_input_utf16le_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16Be:
			return open_input_utf16be_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16LeBom:
			return open_input_utf16lebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16BeBom:
			return open_input_utf16bebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32:
			return open_input_utf32_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32Le:
			return open_input_utf32le_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32Be:
			return open_input_utf32be_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32LeBom:
			return open_input_utf32lebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32BeBom:
			return open_input_utf32bebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Default:
		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_input_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	addr stream;

	/* rewind */
	if (memory_stream_p(pos)) {
		Return(file_position_start_stream_(pos, &check));
		if (check) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot move file-pointer, ~S.", pos, NULL);
		}
	}

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 1, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Character:
			Return(open_external_input_stream_(ptr, &stream, pos, ext));
			break;

		case Stream_Open_Element_Unsigned8:
			Return(open_input_binary_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Unsigned16:
			Return(open_input_unsigned16_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Unsigned32:
			Return(open_input_unsigned32_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed8:
			Return(open_input_signed8_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed16:
			Return(open_input_signed16_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed32:
			Return(open_input_signed32_stream_(ptr, &stream, pos));
			break;

#ifdef LISP_64BIT
		case Stream_Open_Element_Unsigned64:
			Return(open_input_unsigned64_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Signed64:
			Return(open_input_signed64_stream_(ptr, &stream, pos));
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot open file, ~S.", pos, NULL);
	}

	return Result(ret, stream);
}

static int open_external_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Ascii:
			return open_output_ascii_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_output_utf8_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf8Bom:
			return open_output_utf8_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Le:
			return open_output_utf16le_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Be:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16LeBom:
			return open_output_utf16le_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16BeBom:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Le:
			return open_output_utf32le_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Be:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32LeBom:
			return open_output_utf32le_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32BeBom:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Default:
		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 0, &check));
	if (check)
		return 0;

	/* :if-exists */
	Return(open_if_exists_stream_(ptr, ret, pos, if1, &mode, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Character:
			Return(open_external_output_stream_(ptr, &stream, pos, ext, mode));
			break;

		case Stream_Open_Element_Unsigned8:
			Return(open_output_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned16:
			Return(open_output_unsigned16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned32:
			Return(open_output_unsigned32_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed8:
			Return(open_output_signed8_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed16:
			Return(open_output_signed16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed32:
			Return(open_output_signed32_stream_(ptr, &stream, pos, mode));
			break;

#ifdef LISP_64BIT
		case Stream_Open_Element_Unsigned64:
			Return(open_output_unsigned64_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed64:
			Return(open_output_signed64_stream_(ptr, &stream, pos, mode));
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot open file, ~S.", pos, NULL);
	}

	return Result(ret, stream);
}

static int open_external_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Ascii:
			return open_io_ascii_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_io_utf8_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8Bom:
			return open_io_utf8bom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16:
			return open_io_utf16_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Le:
			return open_io_utf16le_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Be:
			return open_io_utf16be_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16LeBom:
			return open_io_utf16lebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16BeBom:
			return open_io_utf16bebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32:
			return open_io_utf32_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Le:
			return open_io_utf32le_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Be:
			return open_io_utf32be_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32LeBom:
			return open_io_utf32lebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32BeBom:
			return open_io_utf32bebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Default:
		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 0, &check));
	if (check)
		return 0;

	/* :if-exists */
	Return(open_if_exists_stream_(ptr, ret, pos, if1, &mode, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Character:
			Return(open_external_io_stream_(ptr, &stream, pos, ext, mode));
			break;

		case Stream_Open_Element_Unsigned8:
			Return(open_io_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned16:
			Return(open_io_unsigned16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Unsigned32:
			Return(open_io_unsigned32_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed8:
			Return(open_io_signed8_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed16:
			Return(open_io_signed16_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed32:
			Return(open_io_signed32_stream_(ptr, &stream, pos, mode));
			break;

#ifdef LISP_64BIT
		case Stream_Open_Element_Unsigned64:
			Return(open_io_unsigned64_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Signed64:
			Return(open_io_signed64_stream_(ptr, &stream, pos, mode));
			break;
#endif

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot open file, ~S.", pos, NULL);
	}

	return Result(ret, stream);
}

static int open_direct_probe_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ignore)
{
	int check;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 1, &check));
	if (check)
		return 0;

	/* :element-type */
	Return(open_probe_stream_(ptr, &stream, pos));

	/* error check */
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot open file, ~S.", pos, NULL);
	}

	return Result(ret, stream);
}

int open_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	/* :direction */
	switch (direction) {
		case Stream_Open_Direction_Input:
			return open_direct_input_stream_(ptr, ret, pos, type, if2, ext);

		case Stream_Open_Direction_Output:
			return open_direct_output_stream_(ptr, ret, pos, type, if1, if2, ext);

		case Stream_Open_Direction_Io:
			return open_direct_io_stream_(ptr, ret, pos, type, if1, if2, ext);

		case Stream_Open_Direction_Probe:
			return open_direct_probe_stream_(ptr, ret, pos, type, if2, ext);

		default:
			*ret = Nil;
			return fmte_("Invalid direction.", NULL);
	}
}

