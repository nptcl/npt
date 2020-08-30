#ifndef __FILE_OPEN_HEADER__
#define __FILE_OPEN_HEADER__

#include "execute.h"
#include "file_type.h"
#include "typedef.h"

#define open_input_binary_stream_ _n(open_input_binary_stream_)
#define open_input_ascii_stream_ _n(open_input_ascii_stream_)
#define open_input_utf8_stream_ _n(open_input_utf8_stream_)
#define open_input_utf8bom_stream_ _n(open_input_utf8bom_stream_)
#define open_input_utf16_stream_ _n(open_input_utf16_stream_)
#define open_input_utf16le_stream_ _n(open_input_utf16le_stream_)
#define open_input_utf16be_stream_ _n(open_input_utf16be_stream_)
#define open_input_utf16lebom_stream_ _n(open_input_utf16lebom_stream_)
#define open_input_utf16bebom_stream_ _n(open_input_utf16bebom_stream_)
#define open_input_utf32_stream_ _n(open_input_utf32_stream_)
#define open_input_utf32le_stream_ _n(open_input_utf32le_stream_)
#define open_input_utf32be_stream_ _n(open_input_utf32be_stream_)
#define open_input_utf32lebom_stream_ _n(open_input_utf32lebom_stream_)
#define open_input_utf32bebom_stream_ _n(open_input_utf32bebom_stream_)
#define open_input_stream_external_ _n(open_input_stream_external_)
#define open_input_stream_ _n(open_input_stream_)
#define open_input_stream_error_ _n(open_input_stream_error_)
#define open_output_binary_stream_ _n(open_output_binary_stream_)
#define open_output_ascii_stream_ _n(open_output_ascii_stream_)
#define open_output_utf8_stream_ _n(open_output_utf8_stream_)
#define open_output_utf16le_stream_ _n(open_output_utf16le_stream_)
#define open_output_utf16be_stream_ _n(open_output_utf16be_stream_)
#define open_output_utf32le_stream_ _n(open_output_utf32le_stream_)
#define open_output_utf32be_stream_ _n(open_output_utf32be_stream_)
#define open_output_stream_ _n(open_output_stream_)
#define open_io_binary_stream_ _n(open_io_binary_stream_)
#define open_io_ascii_stream_ _n(open_io_ascii_stream_)
#define open_io_utf8_stream_ _n(open_io_utf8_stream_)
#define open_io_utf8bom_stream_ _n(open_io_utf8bom_stream_)
#define open_io_utf16_stream_ _n(open_io_utf16_stream_)
#define open_io_utf16le_stream_ _n(open_io_utf16le_stream_)
#define open_io_utf16be_stream_ _n(open_io_utf16be_stream_)
#define open_io_utf16lebom_stream_ _n(open_io_utf16lebom_stream_)
#define open_io_utf16bebom_stream_ _n(open_io_utf16bebom_stream_)
#define open_io_utf32_stream_ _n(open_io_utf32_stream_)
#define open_io_utf32le_stream_ _n(open_io_utf32le_stream_)
#define open_io_utf32be_stream_ _n(open_io_utf32be_stream_)
#define open_io_utf32lebom_stream_ _n(open_io_utf32lebom_stream_)
#define open_io_utf32bebom_stream_ _n(open_io_utf32bebom_stream_)
#define open_io_stream_ _n(open_io_stream_)

/* input */
_g int open_input_binary_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_ascii_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf8_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf8bom_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf16_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf16le_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf16be_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf16lebom_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf16bebom_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf32_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf32le_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf32be_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf32lebom_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_utf32bebom_stream_(Execute ptr, addr *stream, addr file);

_g int open_input_stream_external_(Execute ptr, addr *stream, addr file, addr format);
_g int open_input_stream_(Execute ptr, addr *stream, addr file);
_g int open_input_stream_error_(Execute ptr, addr *ret, addr file);

/* output */
_g int open_output_binary_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_output_ascii_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_output_utf8_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf16le_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf16be_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf32le_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf32be_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);

/* io */
_g int open_io_binary_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_ascii_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf8_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf8bom_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16le_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16be_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16lebom_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16bebom_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32le_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32be_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32lebom_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32bebom_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_stream_(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);

#endif

