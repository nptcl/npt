#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "constant.h"
#include "control_operator.h"
#include "function.h"
#include "mop.h"
#include "print.h"
#include "print_object.h"
#include "print_write.h"
#include "stream.h"
#include "stream_function.h"
#include "structure.h"
#include "structure_object.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  t
 */
static int method_print_clos_name_(Execute ptr, addr stream, addr pos)
{
	if (pos == Unbound)
		goto unbound;
	if (! closp(pos))
		goto unbound;
	stdget_class_name_check(pos, &pos);
	if (pos == Unbound)
		goto unbound;
	return princ_print(ptr, stream, pos);
unbound:
	return print_ascii_stream_(stream, "Unbound");
}

static int method_print_clos_class_of_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetClassOfClos(pos, &pos);
	return method_print_clos_name_(ptr, stream, pos);
}

static int method_print_object_t_body(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	return method_print_clos_class_of_(ptr, stream, pos);
}

static int method_print_object_t(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(print_unreadable_object_(ptr,
				stream, pos, 0, 1, method_print_object_t_body));
	setresult_control(ptr, pos);
	return 0;
}


/*
 *  class
 */
static int method_print_object_class(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	/* #<CLASS-OF CLASS-NAME> */
	Return(print_ascii_stream_(stream, "#<"));
	Return(method_print_clos_class_of_(ptr, stream, pos));
	Return(write_char_stream_(stream, ' '));
	Return(method_print_clos_name_(ptr, stream, pos));
	Return(write_char_stream_(stream, '>'));
	/* result */
	setresult_control(ptr, pos);

	return 0;
}


/*
 *  structure-object
 */
static int write_structure(Execute ptr, addr stream, addr pos)
{
	int check;
	addr x, y, z;
	size_t size, i;

	/* class name */
	GetClassOfClos(pos, &x);
	if (x == Unbound)
		return print_ascii_stream_(stream, "#S(INVALID)");
	Return(structure_class_p_(x, &check));
	if (! check)
		return print_ascii_stream_(stream, "#S(INVALID)");
	Return(stdget_structure_name_(x, &x));
	Return(print_ascii_stream_(stream, "#S("));
	Return(write_print(ptr, stream, x));
	/* slot */
	GetSlotClos(pos, &x);
	GetValueClos(pos, &y);
	LenSlotVector(x, &size);
	for (i = 0; i < size; i++) {
		Return(write_char_stream_(stream, ' '));
		GetSlotVector(x, i, &z);
		GetNameSlot(z, &z);
		GetNameSymbol(z, &z);
		Return(write_char_stream_(stream, ':'));
		Return(princ_print(ptr, stream, z));
		Return(write_char_stream_(stream, ' '));
		GetClosValue(y, i, &z);
		if (z == Unbound) {
			Return(print_ascii_stream_(stream, "#<UNBOUND>"));
		}
		else {
			Return(write_print(ptr, stream, z));
		}
	}
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int method_print_object_structure_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(print_structure(ptr, stream, pos));
	setresult_control(ptr, pos);
	return 0;
}

int print_structure(Execute ptr, addr stream, addr pos)
{
	return write_structure(ptr, stream, pos);
}


/*
 *  generic_function
 */
static int method_print_object_generic_function(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	addr class_of, call, name;

	Return(clos_class_of_(pos, &class_of));
	Return(stdget_class_name_(class_of, &class_of));
	Return(stdget_generic_name_(pos, &call));
	/* #<CLASS-OF CLASS-NAME> */
	Return(print_ascii_stream_(stream, "#<"));
	Return(princ_print(ptr, stream, class_of));
	Return(write_char_stream_(stream, ' '));
	GetCallName(call, &name);
	if (setfp_callname(call)) {
		Return(print_ascii_stream_(stream, "(SETF "));
		Return(prin1_print(ptr, stream, name));
		Return(write_char_stream_(stream, ')'));
	}
	else {
		Return(princ_print(ptr, stream, name));
	}
	Return(write_char_stream_(stream, '>'));
	/* result */
	setresult_control(ptr, pos);

	return 0;
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

static int defmethod_print_object_(Execute ptr, addr name, addr gen,
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
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  defgeneric
 */
void init_print_object(void)
{
	SetPointerType(var4, method_print_object_t);
	SetPointerType(var4, method_print_object_class);
	SetPointerType(var4, method_print_object_structure_object);
	SetPointerType(var4, method_print_object_generic_function);
}

#define DefMethod_PrintObject(ptr, name, gen, p, c) { \
	Return(defmethod_print_object_((ptr), (name), (gen), \
				p_method_print_object_##p, CONSTANT_CLOS_##c)); \
}
static int build_print_object_method_(Execute ptr, addr name, addr gen)
{
	DefMethod_PrintObject(ptr, name, gen, t, T);
	DefMethod_PrintObject(ptr, name, gen, class, CLASS);
	DefMethod_PrintObject(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
	DefMethod_PrintObject(ptr, name, gen, generic_function, GENERIC_FUNCTION);

	return 0;
}

int build_print_object_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_PRINT_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(build_print_object_method_(ptr, name, gen));
	return common_method_finalize_(gen);
}

