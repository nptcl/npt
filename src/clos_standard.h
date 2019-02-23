#ifndef __CLOS_STANDARD_HEADER__
#define __CLOS_STANDARD_HEADER__

#include "constant.h"
#include "execute.h"

void default_slot_name(addr pos, int index, enum CONSTANT_INDEX cons);
void set_slots_localtion(addr slots);
void make_type_class(Execute ptr,
		addr *ret, addr metaclass, addr name, addr supers);
void make_type_class_constant(Execute ptr,
		addr metaclass, enum CONSTANT_INDEX index, ...);
void make_type_class_slots_constant(Execute ptr,
		addr metaclass, addr slots,
		enum CONSTANT_INDEX index_name,
		enum CONSTANT_INDEX index_super);
void std_update_class_of(addr instance, addr class_of);
void getdocument_standard_class(addr instance, addr *ret);
void setdocument_standard_class(addr instance, addr value);
int classp(addr clos);

int eql_specializer_p(addr clos);
void intern_eql_specializer(addr value, addr *ret);
int std_subclass_p(addr clos, addr super);
int std_subtype_p(addr clos, addr super);
int funcallablep(addr pos);
void find_class_constant(enum CONSTANT_INDEX index, enum CONSTANT_INDEX clos);

void build_clos_standard(Execute ptr);

#endif

