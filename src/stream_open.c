#include "bignum_output.h"
#include "character_queue.h"
#include "condition.h"
#include "condition_define.h"
#include "file_open.h"
#include "files.h"
#include "integer.h"
#include "local.h"
#include "pathname_object.h"
#include "stream_function.h"
#include "stream_open.h"
#include "strtype.h"
#include "typedef.h"

static int open_make_empty_stream_(Execute ptr, addr file)
{
	addr stream;

	Return(open_output_binary_stream_(ptr, &stream, file, FileOutput_supersede));
	if (stream == NULL)
		return call_file_error_(ptr, file);

	return close_stream_(stream, NULL);
}

static int open_probe_file_stream_(Execute ptr, addr file, int *ret)
{
#ifdef LISP_ANSI
	return Result(ret, 1);
#else
	Return(probe_file_files_(ptr, &file, file));
	return Result(ret, file != Nil);
#endif
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
			return call_file_error_(ptr, pos);

		case Stream_Open_IfDoesNot_Nil:
			*ret = Nil;
			return Result(existp, 1);

		default:
			return fmte_("Invalid :if-does-not-exist value.", NULL);
	}
}

static int open_if_exists_rename_stream_(Execute ptr, addr pos)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, type, queue, ret1, ret2, ret3;
	size_t i;

	Return(open_probe_file_stream_(ptr, pos, &check));
	if (! check)
		return 0;
	/* rename */
	local = ptr->local;
	for (i = 0; ; i++) {
		push_local(local, &stack);
		/* make filename */
		copy_pathname_alloc(local, &path, pos);
		charqueue_local(local, &queue, 0);
		GetTypePathname(path, &type);
		if (stringp(type)) {
			Return(pushstring_charqueue_local_(local, queue, type));
			Return(pushchar_charqueue_local_(local, queue, "."));
		}
		make_index_integer_alloc(local, &type, i);
		Return(decimal_charqueue_integer_local_(local, type, queue));
		make_charqueue_local(local, queue, &type);
		SetTypePathname(path, type);
		/* check */
		Return(open_probe_file_stream_(ptr, path, &check));
		if (! check) {
			Return(rename_file_files_(ptr, &ret1, &ret2, &ret3, pos, path));
			rollback_local(local, stack);
			return 0;
		}
		rollback_local(local, stack);
	}
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
			return call_file_error_(ptr, pos);

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
		case Stream_Open_External_Default:
			return open_input_stream_(ptr, ret, pos);

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

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 1, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			Return(open_input_binary_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Character:
			Return(open_external_input_stream_(ptr, &stream, pos, ext));
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_external_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_output_stream_(ptr, ret, pos, mode);

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
		case Stream_Open_Element_Binary:
			Return(open_output_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Character:
			Return(open_external_output_stream_(ptr, &stream, pos, ext, mode));
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_external_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_io_stream_(ptr, ret, pos, mode);

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
		case Stream_Open_Element_Binary:
			Return(open_io_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Character:
			Return(open_external_io_stream_(ptr, &stream, pos, ext, mode));
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_direct_probe_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	Return(open_direct_input_stream_(ptr, &pos, pos, type, if2, ext));
	if (pos != Nil) {
		Return(close_stream_(pos, NULL));
	}
	return Result(ret, pos);
}

_g int open_stream_(Execute ptr, addr *ret, addr pos,
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

