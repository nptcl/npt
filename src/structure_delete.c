#include "clos.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "structure.h"
#include "structure_access.h"
#include "structure_delete.h"
#include "structure_object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  name
 */
static int structure_delete1_instance_(addr name)
{
	if (! symbolp(name))
		return TypeError_(name, SYMBOL);
	clos_define_class(name, Nil);

	return 0;
}

static int structure_delete2_instance_(addr name)
{
	if (! symbolp(name))
		return TypeError_(name, SYMBOL);
	remstructure_symbol(name);

	return 0;
}

static int structure_delete3_instance_(addr name)
{
	return structure_delete2_instance_(name);
}


/*
 *  call
 */
int structure_delete1_call_(addr instance)
{
	addr list, pos;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_access_(instance, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos);
		/* reader */
		SetFunctionSymbol(pos, Unbound);
		/* writer (setf ...) */
		remsetf_symbol(pos);
	}

	return 0;
}

int structure_delete2_call_(addr instance)
{
	addr list, pos;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetAccessStructure(instance, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos);
		/* reader */
		SetFunctionSymbol(pos, Unbound);
		/* writer (setf ...) */
		remsetf_symbol(pos);
	}

	return 0;
}

int structure_delete3_call_(addr instance)
{
	return structure_delete2_call_(instance);
}


/*
 *  copier
 */
int structure_delete1_copier_(addr instance)
{
	addr symbol;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_copier_(instance, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete2_copier_(addr instance)
{
	addr symbol;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetCopierStructure(instance, &symbol);
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete3_copier_(addr instance)
{
	return structure_delete2_copier_(instance);
}


/*
 *  predicate
 */
int structure_delete1_predicate_(addr instance)
{
	addr symbol;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_predicate_(instance, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete2_predicate_(addr instance)
{
	addr symbol;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetPredicateStructure(instance, &symbol);
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete3_predicate_(addr instance)
{
	return structure_delete2_predicate_(instance);
}


/*
 *  constructor
 */
int structure_delete1_constructor_(addr instance)
{
	addr list, symbol;

	Check(! structure_class_p_debug(instance), "type error");
	Return(stdget_structure_constructor_(instance, &list));
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete2_constructor_(addr instance)
{
	addr list, symbol;

	CheckType(instance, LISPSYSTEM_STRUCTURE);
	GetConstructorStructure(instance, &list);
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		SetFunctionSymbol(symbol, Unbound);
	}

	return 0;
}

int structure_delete3_constructor_(addr instance)
{
	return structure_delete2_constructor_(instance);
}


/*
 *  print-object
 */
static int structure_delete1_method_(Execute ptr, addr instance, addr gen, addr *ret)
{
	addr list, type1, type2;

	/* (structure T) */
	type1 = instance;
	GetConst(CLOS_T, &type2);
	list_heap(&list, type1, type2, NULL);

	/* find-method */
	return method_find_method_nil_(ptr, gen, Nil, list, ret);
}

int structure_delete1_print_(Execute ptr, addr instance)
{
	addr gen, method;

	Check(! structure_class_p_debug(instance), "type error");
	GetConst(COMMON_PRINT_OBJECT, &gen);
	GetFunctionSymbol(gen, &gen);
	Return(structure_delete1_method_(ptr, instance, gen, &method));
	if (method != Nil) {
		Return(method_remove_method_(ptr, gen, method));
	}

	return 0;
}


/*
 *  delete
 */
static int structure_delete1_(Execute ptr, addr name, addr instance)
{
	Return(structure_delete1_instance_(name));
	Return(structure_delete1_call_(instance));
	Return(structure_delete1_copier_(instance));
	Return(structure_delete1_predicate_(instance));
	Return(structure_delete1_constructor_(instance));
	Return(structure_delete1_print_(ptr, instance));

	return 0;
}

static int structure_delete2_(Execute ptr, addr name, addr instance)
{
	Return(structure_delete2_instance_(name));
	Return(structure_delete2_call_(instance));
	Return(structure_delete2_copier_(instance));
	Return(structure_delete2_predicate_(instance));
	Return(structure_delete2_constructor_(instance));

	return 0;
}

static int structure_delete3_(Execute ptr, addr name, addr instance)
{
	Return(structure_delete3_instance_(name));
	Return(structure_delete3_call_(instance));
	Return(structure_delete3_copier_(instance));
	Return(structure_delete3_predicate_(instance));
	Return(structure_delete3_constructor_(instance));

	return 0;
}

int structure_delete_(Execute ptr, addr name, int *ret)
{
	addr pos;

	/* type */
	if (! symbolp(name))
		return TypeError_(name, SYMBOL);

	/* class */
	if (structure_get_class(name, &pos)) {
		Return(structure_delete1_(ptr, name, pos));
		return Result(ret, 1);
	}

	/* object */
	if (structure_get_object(name, &pos)) {
		if (structure_list_p(pos)) {
			Return(structure_delete2_(ptr, name, pos));
		}
		if (structure_vector_p(pos)) {
			Return(structure_delete3_(ptr, name, pos));
		}
		return Result(ret, 1);
	}

	/* error */
	return Result(ret, 0);
}

