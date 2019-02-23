#ifndef __STREAM_TWOWAY_HEADER__
#define __STREAM_TWOWAY_HEADER__

#include "typedef.h"

void open_twoway_stream(addr *stream, addr input, addr output);
void get_twoway_input_stream(addr stream, addr *ret);
void set_twoway_input_stream(addr stream, addr input);
void get_twoway_output_stream(addr stream, addr *ret);
void set_twoway_output_stream(addr stream, addr output);
void init_stream_twoway(void);

#endif

