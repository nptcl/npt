#include "clos.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "clos_type.h"
#include "condition.h"
#include "constant.h"
#include "object.h"
#include "package.h"
#include "readlite.h"
#include "readtable.h"
#include "sequence.h"

/*
 *  defgeneric
 */
static void defgeneric_class_name(Execute ptr)
{
	addr generic, name, lambda;

	GetConst(COMMON_CLASS_NAME, &name);
	readlite_package_heap(&lambda, LISP_CLOS, "(value)");
	generic_function_instance(ptr, &generic, name, lambda);
}

static void defmethod_class_name_object(Execute ptr)
{
}

static void defgeneric_setf_class_name(Execute ptr)
{
}

void build_defgeneric(Execute ptr)
{
	/* class-name */
	defgeneric_class_name(ptr);
	defmethod_class_name_object(ptr);
	defgeneric_setf_class_name(ptr);
}

