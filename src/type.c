#include "type.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_typep.h"
#include "type_value.h"

void init_type(void)
{
	init_type_class();
	init_type_copy();
	init_type_value();
	init_type_subtypep();
	init_type_typep();
}

