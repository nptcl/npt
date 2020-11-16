#ifndef __LISP_MAIN_STRING_HEADER__
#define __LISP_MAIN_STRING_HEADER__

#include <stdio.h>
#include "main_typedef.h"
#include "typedef.h"

#define getsize_stringu _n(getsize_stringu)
#define char_stringu _n(char_stringu)
#define copy_stringu _n(copy_stringu)
#define concatchar_stringu _n(concatchar_stringu)
#define output_stringu _n(output_stringu)
#define free_stringu _n(free_stringu)
#define equal_stringu _n(equal_stringu)
#define equalchar_stringu _n(equalchar_stringu)
#define arrayu_argv_utf8 _n(arrayu_argv_utf8)
#define arrayu_argv_utf16 _n(arrayu_argv_utf16)
#define free_arrayu _n(free_arrayu)
#define free_tableu _n(free_tableu)
#define tableu_env_main _n(tableu_env_main)
#define tableu_env_windows _n(tableu_env_windows)
#define findchar_tableu _n(findchar_tableu)

/* lispstringu */
int getsize_stringu(lispstringu str, size_t *ret);
lispstringu char_stringu(const char *str);
lispstringu copy_stringu(lispstringu ptr);
lispstringu concatchar_stringu(lispstringu a, const char *b);
void output_stringu(lispstringu ptr, FILE *file);
void free_stringu(lispstringu ptr);
int equal_stringu(lispstringu a, lispstringu b);
int equalchar_stringu(lispstringu a, const char *b);
/* lisparrayu */
lisparrayu arrayu_argv_utf8(int argc, const byte *const *argv);
lisparrayu arrayu_argv_utf16(int argc, const byte16 *const *argv);
void free_arrayu(lisparrayu ptr);
/* lisptableu */
void free_tableu(lisptableu ptr);
lisptableu tableu_env_main(const byte *const *env);
lisptableu tableu_env_windows(const byte16 *env);
lispstringu findchar_tableu(lisptableu env, const char *key);

#endif

