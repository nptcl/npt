#ifndef __TERME_OUTPUT_HEADER__
#define __TERME_OUTPUT_HEADER__

#include "typedef.h"

#define terme_output_init _n(terme_output_init)
#define terme_finish_output _n(terme_finish_output)
#define terme_write_byte _n(terme_write_byte)
#define terme_write_char _n(terme_write_char)
#define terme_terpri _n(terme_terpri)
#define terme_fresh_line _n(terme_fresh_line)

void terme_output_init(void);
int terme_finish_output(void);
int terme_write_byte(byte c);
int terme_write_char(unicode c, unsigned width);
int terme_terpri(void);
int terme_fresh_line(void);

#endif

