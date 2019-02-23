/*
 *  ANSI COMMON LISP: 7. Objects
 */
#include "common_header.h"
#include "cons.h"

/*
 *  (defmacro defclass (name ({superclass}*) ({slot}*) &rest option) -> class
 *    name        (or symbol (not null))  ;; nonnil-symbol
 *    superclass  (or symbol (not null))  ;; nonnil-symbol
 *    slot        (or symbol cons)
 *    option      &rest cons
 *    class       class
 */
static void function_defclass(Execute ptr, addr form, addr env)
{
	/*
	 *  `(ensure-class ',name
	 *     :direct-superclasses ,classes
	 *     :direct-slots ,slots
	 *     ,@class-options)
	 */
	addr args, name, classes; //, slots, symbol;

	getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &classes, &args);
	if (! consp(args))
		goto error;

	/* expand */

	return;

error:
	fmte("The defclass ~S must be a "
			"(defclass name (superclasses) (slots) ...) form.", form, NULL);
}

static void defmacro_defclass(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCLASS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_defclass);
	SetMacroCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_MacroFunction);
	settype_function(pos, type);
}


/*
 *  intern
 */
void intern_common_objects(void)
{
	defmacro_defclass();
}

