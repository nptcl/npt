#ifndef __CHARACTER_NAME_HEADER__
#define __CHARACTER_NAME_HEADER__

#include "typedef.h"

#define findtable_unicode_name_ _n(findtable_unicode_name_)
#define findtable_char_name_ _n(findtable_char_name_)
#define findtable_name_char_ _n(findtable_name_char_)
#define find_name_char_ _n(find_name_char_)
#define build_character_name _n(build_character_name)

int findtable_unicode_name_(addr *ret, unicode u);
int findtable_char_name_(addr *ret, addr pos);
int findtable_name_char_(addr *ret, addr name);
int find_name_char_(addr *ret, addr name);

void build_character_name(void);

#endif

