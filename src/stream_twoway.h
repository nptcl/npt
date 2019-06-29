#ifndef __STREAM_TWOWAY_HEADER__
#define __STREAM_TWOWAY_HEADER__

#include "typedef.h"

_g void open_twoway_stream(addr *stream, addr input, addr output);
_g void get_twoway_input_stream(addr stream, addr *ret);
_g void set_twoway_input_stream(addr stream, addr input);
_g void get_twoway_output_stream(addr stream, addr *ret);
_g void set_twoway_output_stream(addr stream, addr output);
_g void init_stream_twoway(void);

#endif

