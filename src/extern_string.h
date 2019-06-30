#ifndef __EXTERN_STRING_HEADER__
#define __EXTERN_STRING_HEADER__

#include <stdio.h>
#include "main_string.h"
#include "typedef.h"

/* lispstringu */
_g int getsize_stringu(lispstringu str, size_t *ret);
_g lispstringu char_stringu(const char *str);
_g lispstringu copy_stringu(lispstringu ptr);
_g lispstringu concatchar_stringu(lispstringu a, const char *b);
_g void output_stringu(lispstringu ptr, FILE *file);
_g void free_stringu(lispstringu ptr);
_g int equal_stringu(lispstringu a, lispstringu b);
_g int equalchar_stringu(lispstringu a, const char *b);
/* lisparrayu */
_g lisparrayu arrayu_argv_utf8(int argc, const byte *const *argv);
_g lisparrayu arrayu_argv_utf16(int argc, const byte16 *const *argv);
_g void free_arrayu(lisparrayu ptr);
/* lisptableu */
_g void free_tableu(lisptableu ptr);
_g lisptableu tableu_env_main(const byte *const *env);
_g lisptableu tableu_env_windows(const byte16 *env);
_g lispstringu findchar_tableu(lisptableu env, const char *key);

#endif

