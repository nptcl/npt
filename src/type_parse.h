#ifndef __TYPE_PARSE_HEADER__
#define __TYPE_PARSE_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_type_parse _n(init_type_parse)
#define build_type_parse _n(build_type_parse)
#define parse_type _n(parse_type)
#define parse_type_not _n(parse_type_not)
#define parse_type_noaster _n(parse_type_noaster)
#define parse_type_unsafe _n(parse_type_unsafe)
#define parse_type_values _n(parse_type_values)

void init_type_parse(void);
void build_type_parse(void);
int parse_type(Execute ptr, addr *ret, addr pos, addr env);
int parse_type_not(Execute ptr, addr *ret, addr pos, addr env);
int parse_type_noaster(Execute ptr, addr *ret, addr pos, addr env);
void parse_type_unsafe(addr *ret, addr pos);

/* debug */
int parse_type_values(Execute ptr, addr *ret, addr type, addr env);

#endif

