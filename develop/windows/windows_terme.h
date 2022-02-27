#ifndef __WINDOWS_TERME_HEADER__
#define __WINDOWS_TERME_HEADER__

#include "typedef.h"

#define terme_windows_init _n(terme_windows_init)
#define terme_windows_begin _n(terme_windows_begin)
#define terme_windows_end _n(terme_windows_end)
#define terme_windows_textmode _n(terme_windows_textmode)
#define terme_windows_rawmode _n(terme_windows_rawmode)
#define terme_windows_build _n(terme_windows_build)
#define terme_windows_select _n(terme_windows_select)
#define terme_windows_wait_integer _n(terme_windows_wait_integer)
#define terme_windows_wait_float _n(terme_windows_wait_float)
#define terme_windows_read _n(terme_windows_read)
#define terme_windows_write _n(terme_windows_write)
#define terme_windows_escape_begin _n(terme_windows_escape_begin)
#define terme_windows_escape_end _n(terme_windows_escape_end)
#define terme_windows_begin_default_ _n(terme_windows_begin_default_)
#define terme_windows_begin_rawmode_ _n(terme_windows_begin_rawmode_)
#define terme_windows_restore_ _n(terme_windows_restore_)

int terme_windows_init(void);
int terme_windows_begin(void);
int terme_windows_end(void);
int terme_windows_textmode(void);
int terme_windows_rawmode(void);
void terme_windows_build(void);
int terme_windows_select(int *ret);
int terme_windows_wait_integer(int *ret, int value);
int terme_windows_wait_float(int *ret, double value);
int terme_windows_read(void *data, size_t size, size_t *ret);
int terme_windows_write(const void *data, size_t size, size_t *ret);
int terme_windows_escape_begin(void);
int terme_windows_escape_end(int *ret);
int terme_windows_begin_default_(addr *ret);
int terme_windows_begin_rawmode_(addr *ret);
int terme_windows_restore_(addr pos);

#endif

