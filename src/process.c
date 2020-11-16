#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "pathname.h"
#include "process.h"
#include "process_arch.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  defclass lisp-system::process
 */
static void process_defclass_slot(addr slots, size_t n, constindex index)
{
	addr slot, pos;

	slot_heap(&slot);
	GetConstant(index, &pos);
	Check(! symbolp(pos), "type error");
	SetNameSlot(slot, pos);
	SetSlotVector(slots, n, slot);
}

static void process_defclass_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 14);
	process_defclass_slot(slots, 0, CONSTANT_KEYWORD_PROGRAM);
	process_defclass_slot(slots, 1, CONSTANT_KEYWORD_ARGS);
	process_defclass_slot(slots, 2, CONSTANT_KEYWORD_ENVIRONMENT);
	process_defclass_slot(slots, 3, CONSTANT_KEYWORD_WAIT);
	process_defclass_slot(slots, 4, CONSTANT_KEYWORD_SEARCH);
	process_defclass_slot(slots, 5, CONSTANT_KEYWORD_ELEMENT_TYPE);
	process_defclass_slot(slots, 6, CONSTANT_KEYWORD_EXTERNAL_FORMAT);
	process_defclass_slot(slots, 7, CONSTANT_KEYWORD_DIRECTORY);
	process_defclass_slot(slots, 8, CONSTANT_KEYWORD_INPUT);
	process_defclass_slot(slots, 9, CONSTANT_KEYWORD_OUTPUT);
	process_defclass_slot(slots, 10, CONSTANT_KEYWORD_ERROR);
	process_defclass_slot(slots, 11, CONSTANT_KEYWORD_IF_INPUT_DOES_NOT_EXIST);
	process_defclass_slot(slots, 12, CONSTANT_KEYWORD_IF_OUTPUT_EXISTS);
	process_defclass_slot(slots, 13, CONSTANT_KEYWORD_IF_ERROR_EXISTS);
	slotvector_set_location(slots);
	*ret = slots;
}

static int process_defclass_class_(LocalRoot local, addr slots)
{
	addr name, supers, metaclass, instance;

	/* name */
	GetConst(SYSTEM_PROCESS, &name);
	Check(! symbolp(name), "type error");
	/* supers */
	GetConst(CLOS_STANDARD_OBJECT, &supers);
	CheckType(supers, LISPTYPE_CLOS);
	conscar_heap(&supers, supers);
	/* metaclass */
	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* defclass */
	return clos_stdclass_supers_(local, &instance, metaclass, name, slots, supers);
}

static int process_defclass_(LocalRoot local)
{
	addr pos, slots;

	/* class check */
	GetConst(SYSTEM_PROCESS, &pos);
	clos_find_class_nil(pos, &pos);
	if (pos != Nil)
		return 0;

	/* defclass */
	process_defclass_slots(&slots);
	return process_defclass_class_(local, slots);
}


/*
 *  make-instance
 */
static int process_instance_environment_(addr pos, addr value)
{
	addr list, x;

	if (value == Unbound)
		return 0;
	if (! listp(value))
		return fmte_(":ENVIRONMENT argument ~S must be a list type.", value, NULL);

	/* string check */
	list = value;
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		if (! stringp(x))
			return fmte_(":ENVIRONMENT value ~S must be a string type.", x, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_ENVIRONMENT, value);
}

static int process_instance_wait_(addr pos, addr value)
{
	addr check;

	if (value == Unbound)
		value = T;
	GetConst(KEYWORD_PIPE, &check);
	if (value != T && value == check)
		return fmte_(":WAIT argument ~S must be a T or :PIPE.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_WAIT, value);
}

static int process_instance_search_(addr pos, addr value)
{
	if (value == Unbound)
		value = Nil;
	if (value != Nil)
		value = T;

	return ClosSetConst_(pos, KEYWORD_SEARCH, value);
}

static int process_instance_element_type_(addr pos, addr value)
{
	addr key1, key2;

	GetConst(COMMON_CHARACTER, &key1);
	GetConst(COMMON_UNSIGNED_BYTE, &key2);
	if (value == Unbound) {
		value = key1;
	}
	if (value != key1 && value != key2) {
		return fmte_(":ELEMENT-TYPE argument ~S "
				"must be a CHARACTER or UNSIGNED-BYTE.", value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_ELEMENT_TYPE, value);
}

static int process_instance_external_format_(addr pos, addr value)
{
	if (value == Unbound) {
		GetConst(KEYWORD_DEFAULT, &value);
	}

	return ClosSetConst_(pos, KEYWORD_EXTERNAL_FORMAT, value);
}

static int process_instance_directory_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound)
		return 0;

	Return(physical_pathname_heap_(ptr, value, &value));
	return ClosSetConst_(pos, KEYWORD_DIRECTORY, value);
}

static int process_instance_input_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound) {
		value = Nil;
	}
	if (value == T) {
		Return(standard_input_stream_(ptr, &value));
	}
	if ((! streamp(value)) && value != Nil)
		return fmte_(":INPUT argument ~S must be a NIL or T or stream.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_INPUT, value);
}

static int process_instance_output_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound) {
		value = Nil;
	}
	if (value == T) {
		Return(standard_output_stream_(ptr, &value));
	}
	if ((! streamp(value)) && value != Nil)
		return fmte_(":OUTPUT argument ~S must be a NIL or T or stream.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_OUTPUT, value);
}

static int process_instance_error_(Execute ptr, addr pos, addr value)
{
	if (value == Unbound) {
		value = Nil;
	}
	if (value == T) {
		Return(error_output_stream_(ptr, &value));
	}
	if ((! streamp(value)) && value != Nil)
		return fmte_(":ERROR argument ~S must be a NIL or T or stream.", value, NULL);

	return ClosSetConst_(pos, KEYWORD_ERROR, value);
}

static int process_instance_if_input_does_not_exist_(addr pos, addr value)
{
	addr key1, key2;

	GetConst(KEYWORD_ERROR, &key1);
	GetConst(KEYWORD_CREATE, &key2);
	if (value == Unbound) {
		value = Nil;
	}
	if (value != Nil && value != key1 && value != key2) {
		return fmte_(":IF-INPUT-DOES-NOT-EXIST argument ~S "
				"must be a :ERROR or :CREATE or NIL.", value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_IF_INPUT_DOES_NOT_EXIST, value);
}

static int process_instance_if_output_exists_(addr pos, addr value)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_ERROR, &key1);
	GetConst(KEYWORD_SUPERSEDE, &key2);
	GetConst(KEYWORD_APPEND, &key3);
	if (value == Unbound) {
		value = key1;
	}
	if (value != Nil && value != key1 && value != key2 && value != key3) {
		return fmte_(":IF-OUTPUT-EXISTS argument ~S "
				"must be a :ERROR, :SUPERSEDE, :APPEND or NIL.",
				value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_IF_OUTPUT_EXISTS, value);
}

static int process_instance_if_error_exists_(addr pos, addr value)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_ERROR, &key1);
	GetConst(KEYWORD_SUPERSEDE, &key2);
	GetConst(KEYWORD_APPEND, &key3);
	if (value == Unbound) {
		value = key1;
	}
	if (value != Nil && value != key1 && value != key2 && value != key3) {
		return fmte_(":IF-ERROR-EXISTS argument ~S "
				"must be a :ERROR, :SUPERSEDE, :APPEND or NIL.",
				value, NULL);
	}

	return ClosSetConst_(pos, KEYWORD_IF_ERROR_EXISTS, value);
}

static int process_eq_constant(addr key, addr value, addr *ret, constindex index)
{
	addr check;

	GetConstant(index, &check);
	if (key != check)
		return 0;
	if (*ret == Unbound)
		*ret = value;

	return 1;
}
#define ProcessEqConst(a,b,c,d) { \
	if (process_eq_constant((a),(b),(c),CONSTANT_KEYWORD_##d)) { \
		continue; \
	} \
}

static int process_instance_rest_(Execute ptr, addr pos, addr rest)
{
	addr key, value;
	addr env, wait, search, element, external, directory;
	addr input, output, error, ifinput, ifoutput, iferror;

	env = wait = search = element = external = directory = Unbound;
	input = output = error = ifinput = ifoutput = iferror = Unbound;
	while (rest != Nil) {
		Return_getcons(rest, &key, &rest);
		Return_getcons(rest, &value, &rest);
		ProcessEqConst(key, value, &env, ENVIRONMENT);
		ProcessEqConst(key, value, &wait, WAIT);
		ProcessEqConst(key, value, &search, SEARCH);
		ProcessEqConst(key, value, &element, ELEMENT_TYPE);
		ProcessEqConst(key, value, &external, EXTERNAL_FORMAT);
		ProcessEqConst(key, value, &directory, DIRECTORY);
		ProcessEqConst(key, value, &input, INPUT);
		ProcessEqConst(key, value, &output, OUTPUT);
		ProcessEqConst(key, value, &error, ERROR);
		ProcessEqConst(key, value, &ifinput, IF_INPUT_DOES_NOT_EXIST);
		ProcessEqConst(key, value, &ifoutput, IF_OUTPUT_EXISTS);
		ProcessEqConst(key, value, &iferror, IF_ERROR_EXISTS);
		return fmte_("Invalid key argument ~S.", key, NULL);
	}
	Return(process_instance_environment_(pos, env));
	Return(process_instance_wait_(pos, wait));
	Return(process_instance_search_(pos, search));
	Return(process_instance_element_type_(pos, element));
	Return(process_instance_external_format_(pos, external));
	Return(process_instance_directory_(ptr, pos, directory));
	Return(process_instance_input_(ptr, pos, input));
	Return(process_instance_output_(ptr, pos, output));
	Return(process_instance_error_(ptr, pos, error));
	Return(process_instance_if_input_does_not_exist_(pos, ifinput));
	Return(process_instance_if_output_exists_(pos, ifoutput));
	Return(process_instance_if_error_exists_(pos, iferror));

	return 0;
}

static int process_instance_(Execute ptr, addr var, addr args, addr rest, addr *ret)
{
	addr pos;

	/* make-instance */
	GetConst(SYSTEM_PROCESS, &pos);
	Return(clos_find_class_(pos, &pos));
	Return(clos_instance_heap_(pos, &pos));
	/* setf slot */
	Return(ClosSetConst_(pos, KEYWORD_PROGRAM, var));
	Return(ClosSetConst_(pos, KEYWORD_ARGS, args));
	/* rest */
	Return(process_instance_rest_(ptr, pos, rest));

	return Result(ret, pos);
}


/*
 *  run-process
 */
int run_process_(Execute ptr, addr var, addr args, addr rest, addr *ret)
{
	Return(process_defclass_(ptr->local));
	Return(process_instance_(ptr, var, args, rest, &var));
	return run_process_arch_(ptr, var, ret);
}

