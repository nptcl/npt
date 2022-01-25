#ifndef __TYPE_PARSE_HEADER__
#define __TYPE_PARSE_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_type_parse _n(init_type_parse)
#define build_type_parse _n(build_type_parse)
#define parse_type_ _n(parse_type_)
#define parse_type_not_ _n(parse_type_not_)
#define parse_type_noaster_ _n(parse_type_noaster_)
#define parse_type_unsafe _n(parse_type_unsafe)
#define parse_type_values_ _n(parse_type_values_)

void init_type_parse(void);
void build_type_parse(void);
int parse_type_(Execute ptr, addr *ret, addr pos, addr env);
int parse_type_not_(Execute ptr, addr *ret, addr pos, addr env);
int parse_type_noaster_(Execute ptr, addr *ret, addr pos, addr env);
void parse_type_unsafe(addr *ret, addr pos);

/* debug */
int parse_type_values_(Execute ptr, addr *ret, addr type, addr env);

#endif

