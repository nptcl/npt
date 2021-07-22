#ifndef __STREAM_HEADER__
#define __STREAM_HEADER__

#include "build.h"
#include "file_type.h"
#include "file_memory.h"
#include "memory.h"
#include "stream_init.h"
#include "stream_object.h"
#include "typedef_stream.h"

#define open_stream_p _n(open_stream_p)
#define copyleft_stream_ _n(copyleft_stream_)
#define pageout_stream_ _n(pageout_stream_)
#define print_ascii_stream_ _n(print_ascii_stream_)
#define print_unicode_stream_ _n(print_unicode_stream_)
#define print_string_stream_ _n(print_string_stream_)
#define input_stream_designer_ _n(input_stream_designer_)
#define output_stream_designer_ _n(output_stream_designer_)

#define standard_input_stream_ _n(standard_input_stream_)
#define standard_output_stream_ _n(standard_output_stream_)
#define error_output_stream_ _n(error_output_stream_)
#define trace_output_stream_ _n(trace_output_stream_)
#define terminal_io_stream_ _n(terminal_io_stream_)
#define debug_io_stream_ _n(debug_io_stream_)
#define query_io_stream_ _n(query_io_stream_)
#define read_unsigned8_stream_ _n(read_unsigned8_stream_)
#define write_unsigned8_stream_ _n(write_unsigned8_stream_)
#define redirect_unsigned8_stream_ _n(redirect_unsigned8_stream_)
#define close_stream_unwind_protect_ _n(close_stream_unwind_protect_)

#define update_standard_stream _n(update_standard_stream)
#define save_stream _n(save_stream)

int open_stream_p(addr stream);
int copyleft_stream_(addr stream, addr src);
int pageout_stream_(addr stream);
int print_ascii_stream_(addr stream, const char *data);
int print_unicode_stream_(addr stream, const unicode *data);
int print_string_stream_(addr stream, addr pos);
int input_stream_designer_(Execute ptr, addr stream, addr *ret);
int output_stream_designer_(Execute ptr, addr stream, addr *ret);

/* special variable */
int standard_input_stream_(Execute ptr, addr *ret);
int standard_output_stream_(Execute ptr, addr *ret);
int error_output_stream_(Execute ptr, addr *ret);
int trace_output_stream_(Execute ptr, addr *ret);
int terminal_io_stream_(Execute ptr, addr *ret);
int debug_io_stream_(Execute ptr, addr *ret);
int query_io_stream_(Execute ptr, addr *ret);

/* wrapper */
int read_unsigned8_stream_(addr stream, byte *value, int *ret);
int write_unsigned8_stream_(addr stream, byte value);
int redirect_unsigned8_stream_(Execute ptr, addr src, addr dst);
int close_stream_unwind_protect_(Execute ptr, addr stream);

/* core */
int update_standard_stream(void);
int save_stream(addr pos);

#endif

