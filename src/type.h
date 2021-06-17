#ifndef __TYPE_HEADER__
#define __TYPE_HEADER__

#include "type_memory.h"
#include "typedef.h"

#define decl_character_p _n(decl_character_p)
#define decl_float_p _n(decl_float_p)
#define decl_range_p _n(decl_range_p)
#define decl_subtypep_real _n(decl_subtypep_real)
#define type_error_p _n(type_error_p)
#define type_function_p _n(type_function_p)
#define type_astert_p _n(type_astert_p)
#define type_function_aster_p _n(type_function_aster_p)
#define type_asterisk_p _n(type_asterisk_p)
#define type_range_p _n(type_range_p)
#define type_string_p _n(type_string_p)

#define init_type _n(init_type)
#define build_type _n(build_type)

int decl_character_p(LispDecl type);
int decl_float_p(LispDecl type);
int decl_range_p(LispDecl type);
int decl_subtypep_real(LispDecl left, LispDecl right);
int type_error_p(addr pos);
int type_function_p(addr pos);
int type_astert_p(addr pos);
int type_function_aster_p(addr pos);
int type_asterisk_p(addr pos);
int type_range_p(addr pos);
int type_string_p(addr pos);

void init_type(void);
void build_type(void);

#endif

