#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "eastasian_unicode.h"
#include "equal.h"
#include "file.h"
#include "integer.h"
#include "stream.h"
#include "stream_default.h"
#include "stream_function.h"
#include "stream_object.h"
#include "typedef.h"

/*
 *  default call
 */
int close_default_stream(addr stream, addr *ret)
{
	return Result(ret, T);
}

int read_char_default_stream(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_char_file_(stream, c, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	return Result(ret, 0);
}

int read_hang_default_stream(addr stream, unicode *c, int *hang, int *ret)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_hang_file_(stream, c, hang, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	*hang = 0;
	return Result(ret, 0);
}

int unread_char_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return fmte_("unread already exists.", NULL);
	ptr->unread = c;
	ptr->unread_check = 1;

	return 0;
}

int write_char_default_stream(addr stream, unicode c)
{
	Return(write_char_file_(stream, c));
	charleft_default_stream(stream, c);
	return 0;
}

int getleft_default_stream(addr stream, size_t *ret)
{
	*ret = PtrStructStream(stream)->terpri;
	return 0;
}

int setleft_default_stream(addr stream, size_t value)
{
	PtrStructStream(stream)->terpri = value;
	return 0;
}

void charleft_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (c == '\n' || c == '\f')
		ptr->terpri = 0;
	else
		ptr->terpri += eastasian_width(c);
}

int file_length_default_stream(addr stream, addr *ret)
{
	int check;
	addr pos;
	size_t size;

	Return(file_length_file_(stream, &size, &check));
	if (check) {
		return Result(ret, Nil);
	}
	else {
		make_index_integer_heap(&pos, size);
		return Result(ret, pos);
	}
}

int file_position_default_stream(addr stream, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

int file_position_start_default_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

int file_position_end_default_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

int file_position_set_default_stream(addr stream, size_t value, int *ret)
{
	return Result(ret, 1);
}

int finish_output_default_stream(addr stream)
{
	return exitpoint_stream_(stream);
}

int force_output_default_stream(addr stream)
{
	return exitpoint_stream_(stream);
}

int clear_output_default_stream(addr stream)
{
	return 0;
}

int exitpoint_default_stream(addr stream)
{
	return 0;
}

int termsize_default_stream(addr stream, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

int checkp_true_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}


/*
 *  default value
 */
int checkp_false_stream(addr stream, int *ret)
{
	return Result(ret, 0);
}

int element_type_character_stream(addr stream, addr *ret)
{
	GetConst(COMMON_CHARACTER, ret);
	return 0;
}

int element_type_io_stream(addr stream, addr *ret)
{
	int check;
	addr input, output;

	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(element_type_stream_(input, &input));
	Return(element_type_stream_(output, &output));
	Return(equal_function_(input, output, &check));
	if (check) {
		return Result(ret, input);
	}
	else {
		GetConst(COMMON_OR, &stream);
		list_heap(ret, stream, input, output, NULL);
		return 0;
	}
}

int external_format_default_stream(addr stream, addr *ret)
{
	GetConst(KEYWORD_DEFAULT, ret);
	return 0;
}

