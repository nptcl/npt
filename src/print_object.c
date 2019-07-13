#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "constant.h"
#include "control.h"
#include "function.h"
#include "mop.h"
#include "print.h"
#include "print_object.h"
#include "stream.h"
#include "structure.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  t
 */
static void method_print_object_t(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	struct PrintFormat format;

	format_print(ptr, &format);
	if (print_unreadable_object(&format, stream, pos, 1, 1, NULL))
		return;
	setresult_control(ptr, pos);
}


/*
 *  class
 */
static int write_clos_body(struct PrintFormat *format, addr stream, addr pos)
{
	Check(! clos_class_p(pos), "type error");
	stdget_class_name(pos, &pos);
	return write_print(format, stream, pos);
}

static void method_print_object_class(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	struct PrintFormat format;

	format_print(ptr, &format);
	if (print_unreadable_object(&format, stream, pos, 1, 0, write_clos_body))
		return;
	setresult_control(ptr, pos);
}


/*
 *  structure-object
 */
static int write_structure(struct PrintFormat *format, addr stream, addr pos)
{
	addr x, y, z;
	size_t size, i;

	/* class name */
	GetClassOfClos(pos, &x);
	if (x == Unbound || (! structure_class_p(x))) {
		print_ascii_stream(stream, "#S(INVALID)");
		return 0;
	}
	stdget_structure_name(x, &x);
	print_ascii_stream(stream, "#S(");
	if (write_print(format, stream, x)) return 1;
	/* slot */
	GetSlotClos(pos, &x);
	GetValueClos(pos, &y);
	LenSlotVector(x, &size);
	for (i = 0; i < size; i++) {
		write_char_stream(stream, ' ');
		GetSlotVector(x, i, &z);
		GetNameSlot(z, &z);
		GetNameSymbol(z, &z);
		write_char_stream(stream, ':');
		if (princ_print(format->ptr, stream, z)) return 1;
		write_char_stream(stream, ' ');
		GetClosValue(y, i, &z);
		if (z == Unbound)
			print_ascii_stream(stream, "#<UNBOUND>");
		else
			if (write_print(format, stream, z)) return 1;
	}
	write_char_stream(stream, ')');

	return 0;
}

static void method_print_object_structure_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	if (print_structure(ptr, stream, pos))
		return;
	setresult_control(ptr, pos);
}

_g int print_structure(Execute ptr, addr stream, addr pos)
{
	struct PrintFormat format;

	format_print(ptr, &format);
	return write_structure(&format, stream, pos);
}


/*
 *  defmethod
 */
static void method_type_print_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Stream);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_print_object(Execute ptr, addr name, addr gen,
		pointer p, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p);
	method_type_print_object(&type);
	settype_function(call, type);
	/* method */
	GetConstant(index, &pos);
	mop_argument_method_print_object(&pos, pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}
#define DefMethod_PrintObject(ptr, name, gen, p, c) { \
	defmethod_print_object((ptr), (name), (gen), \
			p_method_print_object_##p, CONSTANT_CLOS_##c); \
}


/*
 *  defgeneric
 */
_g void init_print_object(void)
{
	SetPointerType(var4, method_print_object_t);
	SetPointerType(var4, method_print_object_class);
	SetPointerType(var4, method_print_object_structure_object);
}

static void build_print_object_method(Execute ptr, addr name, addr gen)
{
	DefMethod_PrintObject(ptr, name, gen, t, T);
	DefMethod_PrintObject(ptr, name, gen, class, CLASS);
	DefMethod_PrintObject(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
}

_g void build_print_object(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_PRINT_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	build_print_object_method(ptr, name, gen);
	common_method_finalize(gen);
}

