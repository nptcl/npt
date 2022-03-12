#ifndef __TERME_ARCH_HEADER__
#define __TERME_ARCH_HEADER__

#include "execute.h"
#include "print_font.h"
#include "typedef.h"

#define terme_arch_init _n(terme_arch_init)
#define terme_arch_size_update _n(terme_arch_size_update)
#define terme_arch_size_get _n(terme_arch_size_get)
#define terme_arch_signal_p_ _n(terme_arch_signal_p_)
#define terme_arch_begin _n(terme_arch_begin)
#define terme_arch_end _n(terme_arch_end)
#define terme_arch_textmode _n(terme_arch_textmode)
#define terme_arch_rawmode _n(terme_arch_rawmode)
#define terme_arch_build _n(terme_arch_build)
#define terme_arch_select _n(terme_arch_select)
#define terme_arch_wait_integer _n(terme_arch_wait_integer)
#define terme_arch_wait_float _n(terme_arch_wait_float)
#define terme_arch_read _n(terme_arch_read)
#define terme_arch_write _n(terme_arch_write)
#define terme_arch_terminal_stop_ _n(terme_arch_terminal_stop_)
#define terme_arch_terminal_sigint_ _n(terme_arch_terminal_sigint_)
#define terme_arch_enable _n(terme_arch_enable)
#define terme_arch_escape_begin _n(terme_arch_escape_begin)
#define terme_arch_escape_end _n(terme_arch_escape_end)
#define text_color_arch_terme _n(text_color_arch_terme)
#define terme_arch_begin_default_ _n(terme_arch_begin_default_)
#define terme_arch_begin_rawmode_ _n(terme_arch_begin_rawmode_)
#define terme_arch_restore_ _n(terme_arch_restore_)

int terme_arch_init(void);
int terme_arch_size_update(void);
void terme_arch_size_get(unsigned *ret_x, unsigned *ret_y);
int terme_arch_signal_p_(int *ret);
int terme_arch_begin(void);
int terme_arch_end(void);
int terme_arch_textmode(int *ret);
int terme_arch_rawmode(int *ret);
void terme_arch_build(void);

int terme_arch_select(int *ret);
int terme_arch_wait_integer(int *ret, int value);
int terme_arch_wait_float(int *ret, double value);
int terme_arch_read(void *data, size_t size, size_t *ret);
int terme_arch_write(const void *data, size_t size, size_t *ret);
int terme_arch_terminal_stop_(void);
int terme_arch_terminal_sigint_(void);
int terme_arch_enable(void);
int terme_arch_escape_begin(void);
int terme_arch_escape_end(int *ret);

int text_color_arch_terme(Execute ptr, PrintColor value);

int terme_arch_begin_default_(addr *ret);
int terme_arch_begin_rawmode_(addr *ret);
int terme_arch_restore_(addr pos);

#endif

