#ifndef __STREAM_TWOWAY_HEADER__
#define __STREAM_TWOWAY_HEADER__

#include "typedef.h"

#define open_twoway_stream _n(open_twoway_stream)
#define get_twoway_input_stream _n(get_twoway_input_stream)
#define set_twoway_input_stream _n(set_twoway_input_stream)
#define get_twoway_output_stream _n(get_twoway_output_stream)
#define set_twoway_output_stream _n(set_twoway_output_stream)
#define init_stream_twoway _n(init_stream_twoway)

void open_twoway_stream(addr *stream, addr input, addr output);
void get_twoway_input_stream(addr stream, addr *ret);
void set_twoway_input_stream(addr stream, addr input);
void get_twoway_output_stream(addr stream, addr *ret);
void set_twoway_output_stream(addr stream, addr output);
void init_stream_twoway(void);

#endif

