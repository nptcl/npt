#ifndef __FILE_MEMORY_HEADER__
#define __FILE_MEMORY_HEADER__

#include "define.h"
#include "execute.h"
#include "file_type.h"
#include "local.h"
#include "typedef.h"

_g int input_unicode_filememory(struct filememory *fm, const unicode *name, size_t size);
_g void update_standard_input_filememory(struct filememory *fm);
_g void update_standard_output_filememory(struct filememory *fm);
_g void update_standard_error_filememory(struct filememory *fm);
_g void standard_input_filememory(struct filememory *fm);
_g void standard_output_filememory(struct filememory *fm);
_g void standard_error_filememory(struct filememory *fm);
_g int open_input_filememory(Execute ptr, struct filememory *fm, addr name);
_g int open_output_filememory(Execute ptr, struct filememory *fm,
		addr name, enum FileOutput mode);
_g int open_io_filememory(Execute ptr, struct filememory *fm,
		addr name, enum FileOutput mode);
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

