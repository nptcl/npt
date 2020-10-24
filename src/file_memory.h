#ifndef __FILE_MEMORY_HEADER__
#define __FILE_MEMORY_HEADER__

#include "define.h"
#include "execute.h"
#include "file_type.h"
#include "local.h"
#include "typedef.h"

#define input_unicode_filememory _n(input_unicode_filememory)
#define update_standard_input_filememory _n(update_standard_input_filememory)
#define update_standard_output_filememory _n(update_standard_output_filememory)
#define update_standard_error_filememory _n(update_standard_error_filememory)
#define standard_input_filememory _n(standard_input_filememory)
#define standard_output_filememory _n(standard_output_filememory)
#define standard_error_filememory _n(standard_error_filememory)
#define open_input_filememory_ _n(open_input_filememory_)
#define open_output_filememory_ _n(open_output_filememory_)
#define open_io_filememory_ _n(open_io_filememory_)
#define close_filememory _n(close_filememory)
#define end_filememory _n(end_filememory)
#define error_filememory _n(error_filememory)
#define read_filememory _n(read_filememory)
#define readforce_filememory _n(readforce_filememory)
#define read_nonblocking_filememory _n(read_nonblocking_filememory)
#define getc_filememory _n(getc_filememory)
#define getc_nonblocking_filememory _n(getc_nonblocking_filememory)
#define ungetc_filememory _n(ungetc_filememory)
#define putc_filememory _n(putc_filememory)
#define write_filememory _n(write_filememory)

#define read_s16_filememory _n(read_s16_filememory)
#define read_s32_filememory _n(read_s32_filememory)
#define read_u16_filememory _n(read_u16_filememory)
#define read_u32_filememory _n(read_u32_filememory)
#define write_s16_filememory _n(write_s16_filememory)
#define write_s32_filememory _n(write_s32_filememory)
#define write_u16_filememory _n(write_u16_filememory)
#define write_u32_filememory _n(write_u32_filememory)
#ifdef LISP_ARCH_64BIT
#define read_s64_filememory _n(read_s64_filememory)
#define read_u64_filememory _n(read_u64_filememory)
#define write_s64_filememory _n(write_s64_filememory)
#define write_u64_filememory _n(write_u64_filememory)
#endif

#define flush_filememory _n(flush_filememory)
#define exitpoint_filememory _n(exitpoint_filememory)
#define end_filememory _n(end_filememory)
#define error_filememory _n(error_filememory)
#define listen_filememory _n(listen_filememory)
#define clear_input_filememory _n(clear_input_filememory)
#define clear_output_filememory _n(clear_output_filememory)
#define file_length_filememory _n(file_length_filememory)
#define file_position_filememory _n(file_position_filememory)
#define file_position_start_filememory _n(file_position_start_filememory)
#define file_position_end_filememory _n(file_position_end_filememory)
#define file_position_set_filememory _n(file_position_set_filememory)
#define readcheck_filememory _n(readcheck_filememory)
#define writecheck_filememory _n(writecheck_filememory)
#define readptr_filememory _n(readptr_filememory)
#define writeptr_filememory _n(writeptr_filememory)
#define readaddr_filememory _n(readaddr_filememory)
#define writeaddr_filememory _n(writeaddr_filememory)
#define readsize_filememory _n(readsize_filememory)
#define writesize_filememory _n(writesize_filememory)

_g int input_unicode_filememory(struct filememory *fm, const unicode *name, size_t size);
_g void update_standard_input_filememory(struct filememory *fm);
_g void update_standard_output_filememory(struct filememory *fm);
_g void update_standard_error_filememory(struct filememory *fm);
_g void standard_input_filememory(struct filememory *fm);
_g void standard_output_filememory(struct filememory *fm);
_g void standard_error_filememory(struct filememory *fm);
_g int open_input_filememory_(LocalRoot local,
		struct filememory *fm, addr name, int *ret);
_g int open_output_filememory_(LocalRoot local,
		struct filememory *fm, addr name, enum FileOutput mode, int *ret);
_g int open_io_filememory_(LocalRoot local,
		struct filememory *fm, addr name, enum FileOutput mode, int *ret);
_g int close_filememory(struct filememory *fm);
_g int end_filememory(struct filememory *fm);
_g int error_filememory(struct filememory *fm);
_g int read_filememory(struct filememory *fm, void *dst, size_t size, size_t *ret);
_g int readforce_filememory(struct filememory *fm, void *dst, size_t size, size_t *ret);
_g int read_nonblocking_filememory(struct filememory *fm,
		void *dst, size_t size, size_t *ret);
_g int getc_filememory(struct filememory *fm, byte *pos);
_g int getc_nonblocking_filememory(struct filememory *fm, byte *pos, size_t *ret);
_g int ungetc_filememory(struct filememory *fm, byte c);
_g int putc_filememory(struct filememory *fm, byte c);
_g int write_filememory(struct filememory *fm,
		const void *dst, size_t size, size_t *ret);

_g int read_s16_filememory(struct filememory *fm, int16_t *ret);
_g int read_s32_filememory(struct filememory *fm, int32_t *ret);
_g int read_u16_filememory(struct filememory *fm, uint16_t *ret);
_g int read_u32_filememory(struct filememory *fm, uint32_t *ret);
_g int write_s16_filememory(struct filememory *fm, int16_t c);
_g int write_s32_filememory(struct filememory *fm, int32_t c);
_g int write_u16_filememory(struct filememory *fm, uint16_t c);
_g int write_u32_filememory(struct filememory *fm, uint32_t c);
#ifdef LISP_ARCH_64BIT
_g int read_s64_filememory(struct filememory *fm, int64_t *ret);
_g int read_u64_filememory(struct filememory *fm, uint64_t *ret);
_g int write_s64_filememory(struct filememory *fm, int64_t c);
_g int write_u64_filememory(struct filememory *fm, uint64_t c);
#endif

_g int flush_filememory(struct filememory *fm);
_g void exitpoint_filememory(struct filememory *fm);
_g int end_filememory(struct filememory *fm);
_g int error_filememory(struct filememory *fm);
_g int listen_filememory(struct filememory *fm);
_g int clear_input_filememory(struct filememory *fm);
_g int clear_output_filememory(struct filememory *fm);
_g int file_length_filememory(struct filememory *fm, size_t *ret);
_g int file_position_filememory(struct filememory *fm, size_t *ret);
_g int file_position_start_filememory(struct filememory *fm);
_g int file_position_end_filememory(struct filememory *fm);
_g int file_position_set_filememory(struct filememory *fm, size_t pos);

/* core */
_g int readcheck_filememory(struct filememory *fm, void *dst, size_t size);
_g int writecheck_filememory(struct filememory *fm, const void *dst, size_t size);
_g int readptr_filememory(struct filememory *fm, void **pos);
_g int writeptr_filememory(struct filememory *fm, const void *pos);
_g int readaddr_filememory(struct filememory *fm, addr *ret);
_g int writeaddr_filememory(struct filememory *fm, addr pos);
_g int readsize_filememory(struct filememory *fm, size_t *ret);
_g int writesize_filememory(struct filememory *fm, size_t pos);

#endif

