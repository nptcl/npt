#ifndef __STREAM_ECHO_HEADER__
#define __STREAM_ECHO_HEADER__

#include "typedef.h"

_g void open_echo_stream(addr *stream, addr input, addr output);
_g void get_echo_input_stream(addr stream, addr *ret);
_g void set_echo_input_stream(addr stream, addr input);
_g void get_echo_output_stream(addr stream, addr *ret);
_g void set_echo_output_stream(addr stream, addr output);
_g void init_stream_echo(void);

#endif

