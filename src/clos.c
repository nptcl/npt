#include "callname.h"
#include "clos.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_instance.h"
#include "clos_make.h"
#include "clos_slot.h"
#include "clos_object.h"
#include "clos_type.h"
#include "closget.h"
#include "closget_slot.h"
#include "condition.h"
#include "cons_list.h"
#include "constant.h"
#include "function.h"
#include "hashtable.h"
#include "package.h"
#include "symbol.h"

static int Clos_standard_ignore = 0;

/*
 *  access
 */
void clos_standard_ignore(int value)
{
	Clos_standard_ignore = value;
}

int clos_standard_class_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_class_p_Low(pos);
}

int clos_standard_generic_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_generic_p_Low(pos);
}

int clos_standard_method_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_method_p_Low(pos);
}

int clos_standard_combination_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_combination_p_Low(pos);
}

int clos_standard_specializer_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_specializer_p_Low(pos);
}


/*
 *  table
 */
/* clos */
void clos_find_class_nil(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	getclass_symbol(name, ret);
}

int clos_find_class_(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	clos_find_class_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No class named ~S.", name, NULL);

	return 0;
}

void clos_define_class(addr name, addr value)
{
	Check(! symbolp(name), "type name error");
	Check((! closp(value)) && (value != Nil), "type value error");
	if (value == Nil)
		remclass_symbol(name);
	else
		setclass_symbol(name, value);
}

/* generic */
void clos_find_generic_nil(addr name, addr *ret)
{
	Check(! function_name_p(name), "type error");

	getglobal_parse_callname(name, &name);
	if (name == Unbound || (! closp(name)))
		*ret = Nil;
	else
		*ret = name;
}

int clos_find_generic_(addr name, addr *ret)
{
	clos_find_generic_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No generic function named ~S.", name, NULL);

	return 0;
}

int clos_define_generic_(addr name, addr value)
{
	Check(! function_name_p(name), "type name error");
	return setglobal_parse_callname_(name, value);
}

/* method-combination */
void clos_find_combination_nil(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	getcombination_symbol(name, ret);
}

int clos_find_combination_(addr name, addr *ret)
{
	clos_find_combination_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No method combination named ~S.", name, NULL);

	return 0;
}

void clos_define_combination(addr name, addr value)
{
	Check(! symbolp(name), "type name error");
	setcombination_symbol(name, value);
}

/* eql-specializser */
static void clos_table_specializer(addr *ret)
{
	*ret = LispRoot(SPECIALIZER);
}

int clos_find_specializer_nil_(addr name, addr *ret)
{
	addr table;
	clos_table_specializer(&table);
	return findnil_hashtable_(table, name, ret);
}

int clos_find_specializer_(addr name, addr *ret)
{
	Return(clos_find_specializer_nil_(name, ret));
	if (*ret == Nil)
		return fmte_("No method eql-specializer named ~S.", name, NULL);

	return 0;
}

int clos_define_specializer_(addr name, addr value)
{
	addr table;

	clos_table_specializer(&table);
	Return(intern_hashheap_(table, name, &name));
	SetCdr(name, value);

	return 0;
}

void clos_forget_all_specializer_unsafe(void)
{
	addr table;
	clos_table_specializer(&table);
	clear_hashtable_heap(table);
}


/*
 *  build
 */
void init_clos(void)
{
	init_clos_generic();
	init_clos_make();
	init_clos_type();
}

static void build_clos_table(Execute ptr)
{
	addr pos;

	/* eql-specializer */
	hashtable_size_heap(&pos, CLOS_TABLE_SPECIALIZER_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	SetLispRoot(SPECIALIZER, pos);
}

void build_clos(Execute ptr)
{
	/* Variable */
	Clos_standard_class = 0;
	Clos_standard_generic = 0;
	Clos_standard_method = 0;
	Clos_standard_combination = 0;
	Clos_standard_specializer = 0;
	Clos_standard_ignore = 0;
	/* build */
	build_clos_table(ptr);
	build_clos_class(ptr->local);
	build_clos_combination();
}

