#ifndef __STREAM_ECHO_HEADER__
#define __STREAM_ECHO_HEADER__

#include "typedef.h"

#define open_echo_stream _n(open_echo_stream)
#define get_echo_input_stream _n(get_echo_input_stream)
#define set_echo_input_stream _n(set_echo_input_stream)
#define get_echo_output_stream _n(get_echo_output_stream)
#define set_echo_output_stream _n(set_echo_output_stream)
#define init_stream_echo _n(init_stream_echo)

void open_echo_stream(addr *stream, addr input, addr output);
void get_echo_input_stream(addr stream, addr *ret);
void set_echo_input_stream(addr stream, addr input);
void get_echo_output_stream(addr stream, addr *ret);
void set_echo_output_stream(addr stream, addr output);
void init_stream_echo(void);

#endif

