#include "clos_variable.h"
#include "typedef.h"

addr Clos_standard_direct_slot_definition;
addr Clos_standard_effective_slot_definition;
addr Clos_standard_class;
addr Clos_standard_generic_function;
addr Clos_standard_method;
addr Clos_long_method_combination;
addr Clos_short_method_combination;
addr Clos_define_long_method_combination;
addr Clos_define_short_method_combination;
addr Clos_eql_specializer;
addr Clos_structure_class;

void init_clos_variable(void)
{
	Clos_standard_direct_slot_definition = 0;
	Clos_standard_effective_slot_definition = 0;
	Clos_standard_class = 0;
	Clos_standard_generic_function = 0;
	Clos_standard_method = 0;
	Clos_long_method_combination = 0;
	Clos_short_method_combination = 0;
	Clos_define_long_method_combination = 0;
	Clos_define_short_method_combination = 0;
	Clos_eql_specializer = 0;
	Clos_structure_class = 0;
}

