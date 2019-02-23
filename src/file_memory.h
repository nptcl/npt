#ifndef __FILE_MEMORY_HEADER__
#define __FILE_MEMORY_HEADER__

#include "define.h"
#include "execute.h"
#include "file_type.h"
#include "local.h"
#include "typedef.h"

/* low level open */
int input_lowlevel_filememory(struct filememory *fm, const void *name);

/* proto-type declaration */
void standard_input_filememory(struct filememory *fm);
void standard_output_filememory(struct filememory *fm);
void standard_error_filememory(struct filememory *fm);
int open_input_filememory(Execute ptr, struct filememory *fm, addr name);
int open_output_filememory(Execute ptr, struct filememory *fm,
		addr name, enum FileOutput mode);
int open_io_filememory(Execute ptr, struct filememory *fm,
		addr name, enum FileOutput mode);
int close_filememory(struct filememory *fm);
int end_filememory(struct filememory *fm);
int error_filememory(struct filememory *fm);
int read_filememory(struct filememory *fm, void *dst, size_t size, size_t *ret);
int readforce_filememory(struct filememory *fm, void *dst, size_t size, size_t *ret);
int readcheck_filememory(struct filememory *fm, void *dst, size_t size);
int read_nonblocking_filememory(struct filememory *fm,
		void *dst, size_t size, size_t *ret);
int getc_filememory(struct filememory *fm, byte *pos);
int getc_nonblocking_filememory(struct filememory *fm, byte *pos, size_t *ret);
int ungetc_filememory(struct filememory *fm, byte c);
int putc_filememory(struct filememory *fm, byte c);
int write_filememory(struct filememory *fm,
		const void *dst, size_t size, size_t *ret);
int writecheck_filememory(struct filememory *fm, const void *dst, size_t size);
int flush_filememory(struct filememory *fm);
int end_filememory(struct filememory *fm);
int error_filememory(struct filememory *fm);
int listen_filememory(struct filememory *fm);
int clear_input_filememory(struct filememory *fm);
int clear_output_filememory(struct filememory *fm);
int file_length_filememory(struct filememory *fm, size_t *ret);
int file_position_filememory(struct filememory *fm, size_t *ret);
int file_position_start_filememory(struct filememory *fm);
int file_position_end_filememory(struct filememory *fm);
int file_position_set_filememory(struct filememory *fm, size_t pos);

/* core */
int readaddr_filememory(struct filememory *fm, addr *ret);
int writeaddr_filememory(struct filememory *fm, addr pos);

#endif

