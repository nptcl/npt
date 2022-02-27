#ifndef __WINDOWS_OUTPUT_HEADER__
#define __WINDOWS_OUTPUT_HEADER__

#include "typedef.h"

#define windows_output_init _n(windows_output_init)
#define windows_output_write _n(windows_output_write)
#define windows_output_clear _n(windows_output_clear)

void windows_output_init(void);
int windows_output_write(const void *data, size_t size, size_t *ret);
void windows_output_clear(void);

#endif
