#ifndef __STREAM_ECHO_HEADER__
#define __STREAM_ECHO_HEADER__

#include "typedef.h"

void open_echo_stream(addr *stream, addr input, addr output);
void get_echo_input_stream(addr stream, addr *ret);
void set_echo_input_stream(addr stream, addr input);
void get_echo_output_stream(addr stream, addr *ret);
void set_echo_output_stream(addr stream, addr output);
void init_stream_echo(void);

#endif

