#ifndef __FILE_ARCH_HEADER__
#define __FILE_ARCH_HEADER__

#include <stdio.h>
#include "file_buffering.h"
#include "file_type.h"
#include "typedef.h"

static inline int read_low(filestream fm, void *pos, size_t size, size_t *ret)
{
	if (fm->redirect)
		return read_low_buffering(fm, (byte *)pos, size, ret);
	else
		return read_arch(fm->file, pos, size, ret);
}

static inline int write_low(filestream fm, const void *pos, size_t size, size_t *ret)
{
	if (fm->redirect)
		return write_low_buffering(fm, (const byte *)pos, size, ret);
	else
		return write_arch(fm->file, pos, size, ret);
}

static inline int close_low(filestream fm)
{
	if (fm->redirect)
		return close_low_buffering(fm);
	else
		return close_arch(fm->file);
}

static inline int flush_low(filestream fm)
{
	if (fm->redirect)
		return flush_low_buffering(fm);
	else
		return flush_arch(fm->file);
}

static inline int read_ready_low(filestream fm)
{
	if (fm->redirect)
		return read_ready_low_buffering(fm);
	else
		return read_ready_arch(fm->file);
}

static inline int file_length_low(filestream fm, size_t *ret)
{
	if (fm->redirect)
		return file_length_low_buffering(fm, ret);
	else
		return file_length_arch(fm->file, ret);
}

static inline int file_position_low(filestream fm, size_t *ret)
{
	if (fm->redirect)
		return file_position_low_buffering(fm, ret);
	else
		return file_position_arch(fm->file, ret);
}

static inline int file_position_start_low(filestream fm)
{
	if (fm->redirect)
		return file_position_start_low_buffering(fm);
	else
		return file_position_start_arch(fm->file);
}

static inline int file_position_end_low(filestream fm)
{
	if (fm->redirect)
		return file_position_end_low_buffering(fm);
	else
		return file_position_end_arch(fm->file);
}

static inline int file_position_set_low(filestream fm, size_t pos)
{
	if (fm->redirect)
		return file_position_set_low_buffering(fm, pos);
	else
		return file_position_set_arch(fm->file, pos);
}
#endif

