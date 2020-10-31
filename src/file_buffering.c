#include "buffering.h"
#include "build.h"
#include "condition.h"
#include "encode.h"
#include "file.h"
#include "file_buffering.h"
#include "stream_memory.h"
#include "stream_object.h"
#include "typedef.h"

/*
 *  file-memory
 */
_g int read_low_buffering(filestream fm, byte *pos, size_t size, size_t *ret)
{
	byte c;
	int check;
	addr stream;
	size_t i;

	stream = fm->pos;
	if (! read_memory_stream_p(stream)) {
		i = 0;
		goto error;
	}

	for (i = 0; i < size; i++) {
		if (read_byte_memory_stream(stream, &c, &check))
			goto error;
		if (check) {
			*ret = i;
			return i == 0; /* Normal or EOF */
		}
		pos[i] = c;
	}
	*ret = i;
	return 0;

error:
	*ret = i;
	return -1;
}

_g int write_low_buffering(filestream fm, const byte *pos, size_t size, size_t *ret)
{
	addr stream;
	size_t i;

	stream = fm->pos;
	if (! write_memory_stream_p(stream)) {
		*ret = 0;
		return 1;
	}

	for (i = 0; i < size; i++) {
		if (write_byte_memory_stream(stream, pos[i]))
			goto error;
	}
	*ret = i;
	return 0;

error:
	*ret = i;
	return 1;
}

_g int close_low_buffering(filestream fm)
{
	return 0;
}

_g int flush_low_buffering(filestream fm)
{
	return 0;
}

_g int read_ready_low_buffering(filestream fm)
{
	return 1; /* ready */
}

_g int file_length_low_buffering(filestream fm, size_t *ret)
{
	if (file_length_memory_stream(fm->pos, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

_g int file_position_low_buffering(filestream fm, size_t *ret)
{
	if (file_position_memory_stream(fm->pos, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

_g int file_position_start_low_buffering(filestream fm)
{
	return file_position_start_memory_stream(fm->pos);
}

_g int file_position_end_low_buffering(filestream fm)
{
	return file_position_end_memory_stream(fm->pos);
}

_g int file_position_set_low_buffering(filestream fm, size_t pos)
{
	return file_position_set_memory_stream(fm->pos, pos);
}


/*
 *  file
 */
static filestream begin_buffering_filememory(addr stream, addr *ret)
{
	addr mem;
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	Check(fm->redirect == 0, "redirect error");
	GetPathnameStream(stream, &mem);
	Check(! memory_stream_p(mem), "type error");

	*ret = fm->pos;
	fm->pos = mem;

	return fm;
}

static void end_buffering_filememory(addr stream, addr prev)
{
	filestream fm;

	CheckFileStream(stream);
	fm = PtrFileMemory(stream);
	fm->pos = prev;
}

_g int close_stream_buffering_(addr stream, addr *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = close_filememory(fm);
	end_buffering_filememory(stream, prev);

	if (check) {
		*ret = Nil;
		return fmte_("close error.", NULL);
	}
	force_close_stream(stream);

	return Result(ret, T);
}

_g int read_binary_buffering_(addr stream, void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = readf_filememory(fm, pos, size, ret);
	end_buffering_filememory(stream, prev);

	if (check < 0)
		return fmte_("read error", NULL);

	return 0;
}

_g int read_byte_buffering_(addr stream, addr *value, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = read_byte_file_type(stream, value);
	end_buffering_filememory(stream, prev);

	if (check < 0)
		return fmte_("read-byte-file error", NULL);

	return Result(ret, check);
}

_g int write_binary_buffering_(addr stream, const void *pos, size_t size, size_t *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = write_filememory(fm, pos, size, ret);
	end_buffering_filememory(stream, prev);

	if (check)
		return fmte_("write error", NULL);

	return 0;
}

_g int write_byte_buffering_(addr stream, addr pos)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = write_byte_file_type_(fm, pos);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int read_char_buffering_(addr stream, unicode *c, int *ret)
{
	int check, escape;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	escape = read_char_encode_(fm, c, &check);
	end_buffering_filememory(stream, prev);

	if (escape)
		return 1;
	if (check < 0)
		return fmte_("read_char_encode error", NULL);

	return Result(ret, check? 1: 0);
}

_g int read_hang_buffering_(addr stream, unicode *c, int *hang, int *ret)
{
	int check, escape;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	escape = read_hang_encode_(fm, c, hang, &check);
	end_buffering_filememory(stream, prev);

	if (escape)
		return 1;
	if (check < 0)
		return fmte_("read_hang_encode error", NULL);

	return Result(ret, check? 1: 0);
}

_g int write_char_buffering_(addr stream, unicode c)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = write_char_encode_(fm, c);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int file_length_buffering_(addr stream, size_t *value, int *ret)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = file_length_file_type_(fm, value, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int file_position_buffering_(addr stream, size_t *value, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_file_type_(stream, value, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int file_position_start_buffering_(addr stream, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_start_file_type_(stream, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int file_position_end_buffering_(addr stream, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_end_file_type_(stream, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int file_position_set_buffering_(addr stream, size_t value, int *ret)
{
	int check;
	addr prev;

	(void)begin_buffering_filememory(stream, &prev);
	check = file_position_set_file_type_(stream, value, ret);
	end_buffering_filememory(stream, prev);

	return check;
}

_g int finish_output_buffering_(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	check = flush_filememory(fm);
	end_buffering_filememory(stream, prev);

	if (check)
		return fmte_("flush-filememory error.", NULL);

	return check;
}

_g int exitpoint_buffering_(addr stream)
{
	filestream fm;
	addr prev;

	fm = begin_buffering_filememory(stream, &prev);
	exitpoint_filememory(fm);
	end_buffering_filememory(stream, prev);

	return 0;
}

