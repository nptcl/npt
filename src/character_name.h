#ifndef __CHARACTER_NAME_HEADER__
#define __CHARACTER_NAME_HEADER__

#include "typedef.h"

_g int findtable_unicode_name(addr *ret, unicode u);
_g int findtable_char_name(addr *ret, addr pos);
_g int findtable_name_char(addr *ret, addr name);
_g int find_name_char(addr *ret, addr name);

_g void build_character_name(void);

#endif

