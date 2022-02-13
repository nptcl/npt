#ifndef __TERME_ARCH_HEADER__
#define __TERME_ARCH_HEADER__

#include "execute.h"
#include "print_font.h"
#include "typedef.h"

#define terme_arch_init _n(terme_arch_init)
#define terme_arch_size_update _n(terme_arch_size_update)
#define terme_arch_size_get _n(terme_arch_size_get)
#define terme_arch_signal_p _n(terme_arch_signal_p)
#define terme_arch_signal_clear _n(terme_arch_signal_clear)
#define terme_arch_begin _n(terme_arch_begin)
#define terme_arch_end _n(terme_arch_end)
#define terme_arch_textmode _n(terme_arch_textmode)
#define terme_arch_rawmode _n(terme_arch_rawmode)
#define terme_arch_build _n(terme_arch_build)

#define terme_arch_select _n(terme_arch_select)
#define terme_arch_wait_integer _n(terme_arch_wait_integer)
#define terme_arch_wait_float _n(terme_arch_wait_float)
#define terme_arch_wait _n(terme_arch_wait)
#define terme_arch_read _n(terme_arch_read)
#define terme_arch_write _n(terme_arch_write)
#define terme_arch_terminal_stop_ _n(terme_arch_terminal_stop_)
#define terme_arch_terminal_sigint_ _n(terme_arch_terminal_sigint_)
#define terme_arch_enable _n(terme_arch_enable)

#define font_arch_terme _n(font_arch_terme)
#define text_color_arch_terme _n(text_color_arch_terme)
#define back_color_arch_terme _n(back_color_arch_terme)

int terme_arch_init(void);
int terme_arch_size_update(void);
void terme_arch_size_get(unsigned *ret_x, unsigned *ret_y);
int terme_arch_signal_p(void);
void terme_arch_signal_clear(void);
int terme_arch_begin(void);
int terme_arch_end(void);
int terme_arch_textmode(int *ret);
int terme_arch_rawmode(int *ret);
void terme_arch_build(void);

int terme_arch_select(int *ret);
int terme_arch_wait_integer(int *ret, int value);
int terme_arch_wait_float(int *ret, double value);
int terme_arch_wait(void);
int terme_arch_read(void *data, size_t size, size_t *ret);
int terme_arch_write(const void *data, size_t size, size_t *ret);
int terme_arch_terminal_stop_(void);
int terme_arch_terminal_sigint_(void);
int terme_arch_enable(void);

int font_arch_terme(Execute ptr, PrintFont value);
int text_color_arch_terme(Execute ptr, PrintColor value);
int back_color_arch_terme(Execute ptr, PrintColor value);

#endif

