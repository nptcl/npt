#include "call_objects.h"
#include "callname.h"
#include "clos.h"
#include "clos_combination.h"
#include "clos_type.h"
#include "common_header.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "declare.h"
#include "function.h"
#include "hold.h"
#include "lambda.h"
#include "strtype.h"
#include "symbol.h"
#include "type_parse.h"

/*
 *  defclass
 */
static int defclass_parse_superclasses(addr args, int defclass, addr *ret)
{
	addr root, pos, quote, find, refer, list;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(CLOSNAME_REFERENCED_CLASS, &refer);
	GetConst(COMMON_LIST, &list);

	/* () -> (list (find-class (quote standard-object))) */
	if (args == Nil) {
		if (defclass)
			GetConst(COMMON_STANDARD_OBJECT, &pos);
		else
			GetConst(COMMON_CONDITION, &pos);
		list_heap(&pos, quote, pos, NULL);
		list_heap(&pos, find, pos, NULL);
		list_heap(ret, list, pos, NULL);
		return 0;
	}

	/* (a b ...) ->
	 *   (list (referenced-class (quote a))
	 *         (referenced-class (quote b))
	 *         ...)
	 */
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		list_heap(&pos, quote, pos, NULL);
		list_heap(&pos, refer, pos, NULL);
		cons_heap(&root, pos, root);
	}
	/* result */
	nreverse(&root, root);
	cons_heap(ret, list, root);

	return 0;
}

static int defclass_eqconst(addr left, constindex index)
{
	addr right;
	GetConstant(index, &right);
	return left == right;
}
#define DefClassEqConst(x,y) defclass_eqconst((x), CONSTANT_KEYWORD_##y)

static int non_nil_symbol_p(addr pos)
{
	return (pos != Nil) && symbolp(pos);
}

static int defclass_allocation_p(addr pos)
{
	return DefClassEqConst(pos, INSTANCE) || DefClassEqConst(pos, CLASS);
}

static int defclass_parse_slotlist(Execute ptr, addr env, addr list, addr *ret)
{
	addr name, key, value, pos, root, quote;
	addr readers, writers, allocation, initargs, initform, initfunction;
	addr type, doc, others;
	LocalHold hold;

	/* name */
	if (! consp(list))
		return fmte_("SLOT-SPECIFIER ~S must be a (name . tail) form.", list, NULL);
	GetCons(list, &name, &list);
	if (! symbolp(name))
		return fmte_("SLOT-NAME ~S must be a symbol.", name, NULL);

	/* arguments */
	readers = Nil;
	writers = Nil;
	allocation = Nil;
	initargs = Nil;
	initform = Unbound;
	initfunction = Nil;
	type = Nil;
	doc = Nil;
	others = Nil;

	hold = LocalHold_array(ptr, 6);
	while (list != Nil) {
		/* key - value */
		if (! consp(list))
			return fmte_("Invalid slot-specifier value ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			return fmte_("SLOT-SPECIFIER ~S must be a key-value form", list, NULL);
		GetCons(list, &value, &list);

		/* :reader */
		if (DefClassEqConst(key, READER)) {
			if (! non_nil_symbol_p(value))
				return fmte_(":READER ~S must be a non-nil symbol.", value, NULL);
			pushnew_heap(readers, value, &readers);
			localhold_set(hold, 0, readers);
			continue;
		}

		/* :writer */
		if (DefClassEqConst(key, WRITER)) {
			if (! function_name_p(value))
				return fmte_(":WRITER ~S must be function name.", value, NULL);
			Return(pushnew_equal_heap_(writers, value, &writers));
			localhold_set(hold, 1, writers);
			continue;
		}

		/* :accessor */
		if (DefClassEqConst(key, ACCESSOR)) {
			if (! non_nil_symbol_p(value))
				return fmte_(":ACCESSOR ~S must be a non-nil symbol.", value, NULL);
			pushnew_heap(readers, value, &readers);
			localhold_set(hold, 0, readers);

			GetConst(COMMON_SETF, &pos);
			list_heap(&value, pos, value, NULL);
			Return(pushnew_equal_heap_(writers, value, &writers));
			localhold_set(hold, 1, writers);
			continue;
		}

		/* :allocation */
		if (DefClassEqConst(key, ALLOCATION)) {
			if (! defclass_allocation_p(value)) {
				return fmte_(":ALLOCATION ~S "
						"must be a :instance or :class.", value, NULL);
			}
			if (allocation != Nil) {
				return call_simple_program_error_va_(NULL,
						":ALLOCATION is already exist.", NULL);
			}
			allocation = value;
			continue;
		}

		/* :initarg */
		if (DefClassEqConst(key, INITARG)) {
			if (! symbolp(value))
				return fmte_(":INITARG ~S must be a symbol.", value, NULL);
			pushnew_heap(initargs, value, &initargs);
			localhold_set(hold, 2, initargs);
			continue;
		}

		/* :initform */
		if (DefClassEqConst(key, INITFORM)) {
			if (initfunction != Nil) {
				return call_simple_program_error_va_(NULL,
						":INITFORM is already exist.", NULL);
			}
			initform = value;
			GetConst(COMMON_LAMBDA, &pos);
			list_heap(&initfunction, pos, Nil, value, NULL);
			localhold_set(hold, 3, initfunction);
			continue;
		}

		/* :type */
		if (DefClassEqConst(key, TYPE)) {
			if (type != Nil) {
				return call_simple_program_error_va_(NULL,
						":TYPE is already exist.", NULL);
			}
			type = value;
			localhold_set(hold, 4, type);
			continue;
		}

		/* :document */
		if (DefClassEqConst(key, DOCUMENTATION)) {
			if (doc != Nil) {
				return call_simple_program_error_va_(NULL,
						":DOCUMENTATION is already exist.", NULL);
			}
			if (! stringp(value))
				return fmte_(":DOCUMENTATION ~S must be a string.", value, NULL);
			doc = value;
			continue;
		}

		/* error */
		return call_simple_program_error_va_(NULL,
				"Invalid slot option ~S.", key, NULL);
	}
	localhold_end(hold);

	root = Nil;
	GetConst(COMMON_QUOTE, &quote);
	/* list :name (quote name) */
	GetConst(COMMON_LIST, &pos);
	cons_heap(&root, pos, root);
	GetConst(KEYWORD_NAME, &pos);
	cons_heap(&root, pos, root);
	list_heap(&pos, quote, name, NULL);
	cons_heap(&root, pos, root);
	/* :readers (quote (a b c ...)) */
	if (readers != Nil) {
		GetConst(CLOSKEY_READERS, &pos);
		cons_heap(&root, pos, root);
		nreverse(&readers, readers);
		list_heap(&pos, quote, readers, NULL);
		cons_heap(&root, pos, root);
	}
	/* :writers (quote (a b c ...)) */
	if (writers != Nil) {
		GetConst(CLOSKEY_WRITERS, &pos);
		cons_heap(&root, pos, root);
		nreverse(&writers, writers);
		list_heap(&pos, quote, writers, NULL);
		cons_heap(&root, pos, root);
	}
	/* :allocation v */
	if (allocation != Nil) {
		GetConst(KEYWORD_ALLOCATION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, allocation, root);
	}
	/* :initargs (quote (a b c ...)) */
	if (initargs != Nil) {
		GetConst(CLOSKEY_INITARGS, &pos);
		cons_heap(&root, pos, root);
		nreverse(&initargs, initargs);
		list_heap(&pos, quote, initargs, NULL);
		cons_heap(&root, pos, root);
	}
	/* :initform (quote a) :initfunction b */
	if (initfunction != Nil) {
		GetConst(KEYWORD_INITFORM, &pos);
		cons_heap(&root, pos, root);
		list_heap(&pos, quote, initform, NULL);
		cons_heap(&root, pos, root);
		GetConst(CLOSKEY_INITFUNCTION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, initfunction, root);
	}
	/* :type (quote a) */
	if (type != Nil) {
		GetConst(KEYWORD_TYPE, &pos);
		cons_heap(&root, pos, root);
		list_heap(&pos, quote, type, NULL);
		cons_heap(&root, pos, root);
	}
	/* :documentation a */
	if (doc != Nil) {
		GetConst(KEYWORD_DOCUMENTATION, &pos);
		cons_heap(&root, pos, root);
		list_heap(&pos, quote, doc, NULL);
		cons_heap(&root, pos, root);
	}
	/* others */
	if (others != Nil) {
		nreverse(&others, others);
		while (others != Nil) {
			GetCons(others, &pos, &others);
			cons_heap(&root, pos, root);
		}
	}
	/* result */
	nreverse(ret, root);

	return 0;
}

static int defclass_parse_slot(Execute ptr, addr env, addr list, addr *ret)
{
	addr symbol, keyword, quote;

	/* symbol */
	if (symbolp(list)) {
		/* (list :name (quote value)) */
		GetConst(COMMON_QUOTE, &quote);
		GetConst(COMMON_LIST, &symbol);
		GetConst(CLOSKEY_NAME, &keyword);
		list_heap(&list, quote, list, NULL);
		list_heap(ret, symbol, keyword, list, NULL);
		return 0;
	}

	/* list */
	if (consp(list))
		return defclass_parse_slotlist(ptr, env, list, ret);

	/* error */
	*ret = Nil;
	return fmte_("DEFCLASS slot-specifier ~S must be a list or a symbol,", list, NULL);
}

static int defclass_parse_slots(Execute ptr, addr env, addr list, addr *ret)
{
	addr root, pos;
	LocalHold hold;

	/* check */
	if (list == Nil)
		return Result(ret, Nil);
	/* hold */
	hold = LocalHold_array(ptr, 1);
	localhold_pushva_force(hold, env, list, NULL);
	/* (list ,@mapcar) */
	GetConst(COMMON_LIST, &root);
	conscar_heap(&root, root);
	localhold_set(hold, 0, root);
	while (list != Nil) {
		if (! consp(list))
			return fmte_("DEFCLASS slot-specifier ~S must be a list.", list, NULL);
		GetCons(list, &pos, &list);
		Return(defclass_parse_slot(ptr, env, pos, &pos));
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	/* result */
	nreverse(ret, root);

	return 0;
}

static int defclass_parse_initargs(addr args, addr *ret)
{
	/* (:aaa 100 bbb (hello))
	 * -> (list (list (quote :aaa) (quote 100) (lambda () 100))
	 *          (list (quote bbb) (quote (hello)) (lambda () (hello))))
	 */
	addr root, key, value, list, quote, lambda, a, b, c;

	/* parse */
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LAMBDA, &lambda);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &key, &args);
		Return_getcons(args, &value, &args);
		list_heap(&a, quote, key, NULL);
		list_heap(&b, quote, value, NULL);
		list_heap(&c, lambda, Nil, value, NULL);
		list_heap(&value, list, a, b, c, NULL);
		cons_heap(&root, value, root);
	}

	/* result */
	if (root == Nil)
		*ret = Nil;
	else {
		nreverse(&root, root);
		cons_heap(ret, list, root);
	}

	return 0;
}

static int defclass_parse_options(addr list, int defclass, addr *ret, addr *report)
{
	addr root, key, value;

	*report = NULL;
	for (root = Nil; list != Nil; ) {
		if (! consp(list))
			return fmte_("DEFCLASS options ~S don't allow dotted list.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(key))
			return fmte_("DEFCLASS option ~S must be a cons.", key, NULL);
		GetCons(key, &key, &value);

		/* :metaclass */
		if (DefClassEqConst(key, METACLASS)) {
			if (! defclass)
				return fmte_(":METACLASS is not supported in DEFINE-CONBINATION.", NULL);
			if (! singlep(value))
				return fmte_("Invalid :METACLASS ~S.", value, NULL);
			GetCar(value, &value);
			if (! non_nil_symbol_p(value))
				return fmte_(":METACLASS ~S must be a non-nil symbol.", value, NULL);
			/* :metaclass (find-class (quote value)) */
			cons_heap(&root, key, root);
			GetConst(COMMON_QUOTE, &key);
			list_heap(&value, key, value, NULL);
			GetConst(COMMON_FIND_CLASS, &key);
			list_heap(&value, key, value, NULL);
			cons_heap(&root, value, root);
			continue;
		}

		/* :default-initargs */
		if (DefClassEqConst(key, DEFAULT_INITARGS)) {
			Return(defclass_parse_initargs(value, &value));
			GetConst(CLOSKEY_DIRECT_DEFAULT_INITARGS, &key);
			cons_heap(&root, key, root);
			cons_heap(&root, value, root);
			continue;
		}

		/* :documentation */
		if (DefClassEqConst(key, DOCUMENTATION)) {
			if (! singlep(value))
				return fmte_("Invalid :DOCUMENTATION ~S.", value, NULL);
			GetCar(value, &value);
			if (! stringp(value))
				return fmte_(":DOCUMENTATION ~S must be a string.", value, NULL);
			/* :documentation value */
			cons_heap(&root, key, root);
			cons_heap(&root, value, root);
			continue;
		}

		if (DefClassEqConst(key, REPORT)) {
			if (defclass)
				return fmte_(":REPORT is not supported in DEFCLASS.", NULL);
			if (! singlep(value))
				return fmte_("Invalid :REPORT ~S.", value, NULL);
			GetCar(value, &value);
			/* :report -> defmethod */
			*report = value;
			continue;
		}

		/* error */
		return call_simple_program_error_va_(NULL,
				"Invalid class option ~S.", key, NULL);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static void define_condition_result(addr *ret, addr args, addr name, addr report)
{
	addr defmethod, prog1, print, declare, ignore, write, funcall, fsymbol;
	addr form, inst, stream, x;

	/* (class-name
	 *   (ensure-class ...))
	 */
	GetConst(COMMON_CLASS_NAME, &form);
	list_heap(&form, form, args, NULL);

	/* (prog1
	 *   [define-condition]
	 *   (defmethod print-object ((inst name) stream)
	 *     ...))
	 */
	if (report) {
		GetConst(COMMON_PROG1, &prog1);
		GetConst(COMMON_DEFMETHOD, &defmethod);
		GetConst(COMMON_PRINT_OBJECT, &print);
		make_symbolchar(&inst, "INST");
		make_symbolchar(&stream, "STREAM");
		list_heap(&x, inst, name, NULL);
		list_heap(&x, x, stream, NULL);

		if (stringp(report)) {
			/* (defmethod print-object ((inst name) stream)
			 *   (declare (ignore inst))
			 *   (write-string report stream))
			 */
			GetConst(COMMON_DECLARE, &declare);
			GetConst(COMMON_IGNORE, &ignore);
			GetConst(COMMON_WRITE_STRING, &write);
			list_heap(&write, write, report, stream, NULL);
			list_heap(&ignore, ignore, inst, NULL);
			list_heap(&declare, declare, ignore, NULL);
			list_heap(&x, defmethod, print, x, declare, write, NULL);
		}
		else {
			/* (defmethod print-object ((inst name) stream)
			 *   (funcall (function report) inst stream))
			 */
			GetConst(COMMON_FUNCALL, &funcall);
			GetConst(COMMON_FUNCTION, &fsymbol);
			list_heap(&fsymbol, fsymbol, report, NULL);
			list_heap(&funcall, funcall, fsymbol, inst, stream, NULL);
			list_heap(&x, defmethod, print, x, funcall, NULL);
		}
		list_heap(&form, prog1, form, x, NULL);
	}

	/* result */
	*ret = form;
}

static int defclass_define_condition(Execute ptr,
		addr form, addr env, addr *ret, int defclass)
{
	/* `(system::ensure-class ',name
	 *     :direct-superclasses ,supers
	 *     :direct-slots ,slots
	 *     ,@class-options)
	 */
	addr first, args, name, nameq, supers, slots, options, ensure;
	addr key1, key2, report;
	LocalHold hold;

	/* destructuring-bind */
	Return_getcons(form, &first, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp_getcons(args, &supers, &args))
		goto error;
	if (! consp_getcons(args, &slots, &options))
		goto error;

	hold = LocalHold_local(ptr);
	/* name */
	GetConst(COMMON_QUOTE, &key1);
	list_heap(&nameq, key1, name, NULL);
	localhold_push(hold, nameq);

	/* parse */
	Return(defclass_parse_superclasses(supers, defclass, &supers));
	localhold_push(hold, supers);
	Return(defclass_parse_slots(ptr, env, slots, &slots));
	Return(defclass_parse_options(options, defclass, &options, &report));

	/* make */
	GetConst(CLOSNAME_ENSURE_CLASS, &ensure);
	GetConst(CLOSKEY_DIRECT_SUPERCLASSES, &key1);
	GetConst(CLOSKEY_DIRECT_SLOTS, &key2);
	lista_heap(&args, ensure, nameq, key1, supers, key2, slots, options, NULL);
	if (! defclass)
		define_condition_result(&args, args, name, report);
	return Result(ret, args);

error:
	*ret = Nil;
	return fmte_("The ~S ~S must be a "
			"(~S name (superclasses) (slots) ...) form.", first, form, first, NULL);
}

int defclass_common(Execute ptr, addr form, addr env, addr *ret)
{
	return defclass_define_condition(ptr, form, env, ret, 1);
}

int define_condition_common(Execute ptr, addr form, addr env, addr *ret)
{
	return defclass_define_condition(ptr, form, env, ret, 0);
}


/*
 *  find-class
 */
int find_class_common_(addr pos, int errorp, addr env_ignore, addr *ret)
{
	Check(! symbolp(pos), "type error");
	if (errorp) {
		Return(clos_find_class_(pos, ret));
	}
	else {
		clos_find_class_nil(pos, ret);
	}

	return 0;
}


/*
 *  (setf find-class)
 */
void setf_find_class_common(addr pos, addr name, addr env_ignore)
{
	Check((! closp(pos)) && (pos != Nil), "type error");
	Check(! symbolp(name), "type error");
	clos_define_class(name, pos);
}


/*
 *  with-accessors
 */
static int with_accessors_arguments(addr args, addr g, addr *ret)
{
	addr root, var, name, temp;

	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &var, &args))
			goto error;
		/* parse */
		if (! consp_getcons(var, &var, &temp))
			goto error;
		if (! consp_getcons(temp, &name, &temp))
			goto error;
		if (temp != Nil)
			goto error;
		if (! symbolp(var))
			return fmte_("WITH-ACCESSORS argument ~S must be a symbol.", var, NULL);
		if (! symbolp(name))
			return fmte_("WITH-ACCESSORS argument ~S must be a symbol.", name, NULL);
		/* expand */
		list_heap(&name, name, g, NULL);
		list_heap(&var, var, name, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-ACCESSORS arguments ~S "
			"must be a (var name) form.", args, NULL);
}

int with_accessors_common(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(let ((,g ,expr))
	 *    (declare (ignorable g))
	 *    (symbol-macrolet ((,var1 (,name1 ,g))
	 *                      (,varN (,nameN ,g)))
	 *      ,@args))
	 */
	addr args, var, expr, g, let, declare, ignorable, symm;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_SYMBOL_MACROLET, &symm);
	Return(make_gensym_(ptr, &g));
	Return(with_accessors_arguments(var, g, &var));
	lista_heap(&symm, symm, var, args, NULL);
	list_heap(&expr, g, expr, NULL);
	list_heap(&expr, expr, NULL);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, expr, declare, symm, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-ACCESSORS argument ~S must be a "
			"((var ...) &body form) form.", form, NULL);
}


/*
 *  with-slots
 */
static int with_slots_arguments(addr args, addr g, addr *ret)
{
	addr slot, quote, root, var, name, temp;

	GetConst(COMMON_SLOT_VALUE, &slot);
	GetConst(COMMON_QUOTE, &quote);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &var, &args);
		/* parse */
		if (symbolp(var)) {
			name = var;
		}
		else {
			if (! consp_getcons(var, &var, &temp))
				goto error;
			if (! consp_getcons(temp, &name, &temp))
				goto error;
			if (temp != Nil)
				goto error;
			if (! symbolp(var))
				return fmte_("WITH-SLOTS argument ~S must be a symbol.", var, NULL);
			if (! symbolp(name))
				return fmte_("WITH-SLOTS argument ~S must be a symbol.", name, NULL);
		}
		/* expand */
		list_heap(&name, quote, name, NULL);
		list_heap(&name, slot, g, name, NULL);
		list_heap(&var, var, name, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-SLOTS arguments ~S must be "
			"a symbol or (var name) form.", args, NULL);
}

int with_slots_common(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(let ((,g ,expr))
	 *    (declare (ignorable g))
	 *    (symbol-macrolet ((,var1 (slot-value ,g ',name1))
	 *                      (,varN (slot-value ,g ',nameN)))
	 *      ,@args))
	 */
	addr args, var, expr, g, let, declare, ignorable, symm;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_SYMBOL_MACROLET, &symm);
	Return(make_gensym_(ptr, &g));
	Return(with_slots_arguments(var, g, &var));
	lista_heap(&symm, symm, var, args, NULL);
	list_heap(&expr, g, expr, NULL);
	list_heap(&expr, expr, NULL);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, expr, declare, symm, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("WITH-SLOTS argument ~S "
			"must be a ((var ...) &body form) form.", form, NULL);
}


/*
 *  (defmacro defgeneric (name lambda &rest args) ...) -> generic-function
 *  args ->
 *    (:argument-precedence-order parameter-name+)
 *    (declare gf-declaration+)
 *    (:documentation gf-documentation)
 *    (:method-combination method-combination method-combination-argument*)
 *    (:generic-function-class generic-function-class)
 *    (:method-class method-class)
 *    (:method qualifiers* lambda declare* document* form*)*
 */
static int defgeneric_parse_order(addr list, addr *ret)
{
	addr check;

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &check, &list);
		if (! symbolp(check)) {
			return fmte_(":ARGUMENT-PRECEDENCE-ORDER ~S "
					"must be a symbol.", check, NULL);
		}
	}

	return 0;
}

static int defgeneric_parse_declare(addr list, addr *ret)
{
	int check;
	addr decl;

	Return(parse_optimize_heap_(list, &decl, &check));
	if (check)
		return fmte_(":DECLARE accept only OPTIMIZE but ~S.", list, NULL);

	return Result(ret, decl);
}

static int defgeneric_parse_document(addr list, addr *ret)
{
	addr pos, doc;

	if (! consp_getcons(list, &doc, &pos))
		goto error;
	if (pos != Nil)
		goto error;
	if (! stringp(doc))
		return fmte_(":DOCUMENTATION ~S must be a string.", doc, NULL);
	return Result(ret, doc);

error:
	*ret = Nil;
	return fmte_(":DOCUMENTATION ~S must be a (string) form.", list, NULL);
}

static int defgeneric_parse_method_combination(addr list, addr *ret)
{
	addr pos, check;

	if (! consp_getcons(list, &check, &pos))
		goto error;
	if (! symbolp(check))
		goto error;
	return Result(ret, list);

error:
	*ret = Nil;
	return fmte_(":METHOD-COMBINATION ~S must be a "
			"(symbol arguments*) form.", list, NULL);
}

static int defgeneric_parse_generic(addr list, addr *ret)
{
	addr name, tail, find, quote;

	/* (find-class (quote name)) */
	if (! consp_getcons(list, &name, &tail))
		return fmte_(":GENERIC-FUNCTION-CLASS ~S must be a (symbol) form.", list, NULL);
	if (name == Nil)
		return fmte_(":GENERIC-FUNCTION-CLASS must be a non-nil symbol.", NULL);
	if (! symbolp(name))
		return fmte_(":GENERIC-FUNCTION-CLASS ~S must be a symbol.", name, NULL);
	if (tail != Nil)
		return fmte_(":GENERIC-FUNCTION-CLASS ~S must be (symbol) form.", list, NULL);

	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, find, name, NULL);

	return 0;
}

static int defgeneric_parse_method(addr list, addr *ret)
{
	addr name, tail, find, quote;

	/* (find-class (quote name)) */
	if (! consp_getcons(list, &name, &tail))
		return fmte_(":METHOD-CLASS ~S must be a (symbol) form.", list, NULL);
	if (name == Nil)
		return fmte_(":METHOD-CLASS must be a non-nil symbol.", NULL);
	if (! symbolp(name))
		return fmte_(":METHOD-CLASS ~S must be a symbol.", name, NULL);
	if (tail != Nil)
		return fmte_(":METHOD-CLASS ~S must be a (symbol) form.", list, NULL);

	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, find, name, NULL);

	return 0;
}

static void defgeneric_parse_form(addr root, addr name, addr list, addr *ret)
{
	addr pos;

	GetConst(COMMON_DEFMETHOD, &pos);
	lista_heap(&pos, pos, name, list, NULL);
	cons_heap(ret, pos, root);
}

static int defgeneric_parse_options(addr name, addr args,
		addr *rorder, addr *rdecl, addr *rdoc, addr *rcomb,
		addr *rgen, addr *rmethod, addr *rcode)
{
	addr pos, tail, type, check;
	addr order, decl, doc, comb, gen, method, code;

	decl = doc = comb = gen = method = Unbound;
	order = code = Nil;
	while (args != Nil) {
		if (! consp_getcons(args, &pos, &args))
			return fmte_("Invalid defgeneric argument ~S.", args, NULL);
		if (! consp_getcons(pos, &type, &tail))
			return fmte_("Invalid defgeneric argument ~S.", pos, NULL);

		/* :argument-precedence-order */
		GetConst(KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &check);
		if (type == check) {
			Return(defgeneric_parse_order(tail, &order));
			continue;
		}
		/* :declare */
		GetConst(COMMON_DECLARE, &check);
		if (type == check) {
			Return(defgeneric_parse_declare(tail, &decl));
			continue;
		}
		/* :documentation */
		GetConst(KEYWORD_DOCUMENTATION, &check);
		if (type == check) {
			Return(defgeneric_parse_document(tail, &doc));
			continue;
		}
		/* :method-combination */
		GetConst(KEYWORD_METHOD_COMBINATION, &check);
		if (type == check) {
			Return(defgeneric_parse_method_combination(tail, &comb));
			continue;
		}
		/* :generic-function-class */
		GetConst(KEYWORD_GENERIC_FUNCTION_CLASS, &check);
		if (type == check) {
			Return(defgeneric_parse_generic(tail, &gen));
			continue;
		}
		/* :method-class */
		GetConst(KEYWORD_METHOD_CLASS, &check);
		if (type == check) {
			Return(defgeneric_parse_method(tail, &method));
			continue;
		}
		/* :method */
		GetConst(KEYWORD_METHOD, &check);
		if (type == check) {
			defgeneric_parse_form(code, name, tail, &code);
			continue;
		}
		/* error */
		return fmte_("Invalid defgeneric option ~S.", pos, NULL);
	}
	*rorder = order;
	*rdecl = decl;
	*rdoc = doc;
	*rcomb = comb;
	*rgen = gen;
	*rmethod = method;
	*rcode = code;
	return 0;
}

static void defgeneric_push_value(addr *ret, addr key, addr value, addr root)
{
	cons_heap(&root, key, root);
	cons_heap(ret, value, root);
}

static void defgeneric_push_quote(addr *ret, addr key, addr value, addr root)
{
	addr quote;

	GetConst(COMMON_QUOTE, &quote);
	list_heap(&value, quote, value, NULL);
	defgeneric_push_value(ret, key, value, root);
}

static int defgeneric_check_function_(addr name)
{
	addr check;

	/* name error */
	if (! function_name_p(name))
		return fmte_("Invalid function name ~S.", name, NULL);

	/* symbol */
	if (symbolp(name)) {
		GetFunctionSymbol(name, &check);
		if (check != Unbound && (! closp(check))) {
			return call_simple_program_error_va_(NULL,
					"Cannot update the function because "
					"~S is not a generic-function.", name, NULL);
		}
	}

	/* setf */
	if (consp(name)) {
		Return(parse_callname_error_(&check, name));
		getglobal_callname(check, &check);
		if (check != Unbound && (! closp(check))) {
			return call_simple_program_error_va_(NULL,
					"Cannot update the function because "
					"~S is not a generic-function.", name, NULL);
		}
		return 0;
	}

	/* macro */
	getmacro_symbol(name, &check);
	if (check != Unbound) {
		return call_simple_program_error_va_(NULL,
				"Cannot make the generic-function because "
				"~S is a macro-function.", name, NULL);
	}

	/* special-operator */
	if (get_special_operator(name)) {
		return call_simple_program_error_va_(NULL,
				"Cannot make the generic-function because "
				"~S is a special operator.", name, NULL);
	}

	return 0;
}

static int defgeneric_method_common_(Execute ptr,
		addr name, addr ensure, addr code, addr *ret)
{
	/*  (let ((g (defgeneric-define ...)))
	 *    (system::defgeneric-method g (defmethod ...))
	 *    (system::defgeneric-method g (defmethod ...))
	 *    g)
	 */
	addr g, let, method, args, list, pos;

	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_DEFGENERIC_METHOD, &method);
	Return(make_gensym_(ptr, &g));

	/* let */
	list_heap(&args, g, ensure, NULL);
	list_heap(&args, args, NULL);

	/* list */
	list = Nil;
	cons_heap(&list, let, list);
	cons_heap(&list, args, list);
	while (code != Nil) {
		GetCons(code, &pos, &code);
		list_heap(&pos, method, g, pos, NULL);
		cons_heap(&list, pos, list);
	}
	cons_heap(&list, g, list);
	nreverse(ret, list);

	return 0;
}

int defgeneric_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, lambda, order, decl, doc, comb, gen, method, code, key;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp_getcons(args, &lambda, &args))
		goto error;
	/* options */
	order = decl = doc = comb = gen = method = code = Nil;
	Return(defgeneric_parse_options(name, args,
				&order, &decl, &doc, &comb, &gen, &method, &code));
	/* check */
	Return(defgeneric_check_function_(name));
	/* expand */
	args = Nil;
	GetConst(SYSTEM_DEFGENERIC_DEFINE, &key);
	defgeneric_push_quote(&args, key, name, args);
	GetConst(KEYWORD_LAMBDA_LIST, &key);
	defgeneric_push_quote(&args, key, lambda, args);
	if (order != Nil) {
		GetConst(KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &key);
		defgeneric_push_quote(&args, key, order, args);
	}
	if (decl != Unbound) {
		GetConst(KEYWORD_DECLARE, &key);
		defgeneric_push_quote(&args, key, decl, args);
	}
	if (doc != Unbound) {
		GetConst(KEYWORD_DOCUMENTATION, &key);
		defgeneric_push_value(&args, key, doc, args);
	}
	if (comb != Unbound) {
		GetConst(KEYWORD_METHOD_COMBINATION, &key);
		defgeneric_push_quote(&args, key, comb, args);
	}
	if (gen != Unbound) {
		GetConst(KEYWORD_GENERIC_FUNCTION_CLASS, &key);
		defgeneric_push_value(&args, key, gen, args);
	}
	if (method != Unbound) {
		GetConst(KEYWORD_METHOD_CLASS, &key);
		defgeneric_push_value(&args, key, method, args);
	}
	nreverse(&args, args);
	if (code != Nil) {
		Return(defgeneric_method_common_(ptr, name, args, code, &args));
	}
	return Result(ret, args);

error:
	*ret = Nil;
	return fmte_("DEFGENERIC argument ~S must be a "
			"(name lambda-list &rest args) form.", form, NULL);
}


/*
 *  defmethod
 */
static int defmethod_parse_qualifiers(addr list, addr *qua, addr *args, addr *body)
{
	addr root, lambda;

	root = Nil;
	for (;;) {
		if (! consp(list))
			return 1;
		GetCons(list, &lambda, &list);
		if (listp(lambda))
			break;
		cons_heap(&root, lambda, root);
	}
	nreverse(qua, root);
	*args = lambda;
	*body = list;

	return 0;
}

static int defmethod_parse_specializers(addr pos, addr *ret)
{
	addr list, var, type, root, quote;

	GetArgument(pos, ArgumentIndex_var, &pos);
	GetConst(COMMON_QUOTE, &quote);
	for (root = Nil; pos != Nil; ) {
		GetCons(pos, &list, &pos);
		Return(list_bind_(list, &var, &type, NULL));
		if (consp(type)) {
			Return_getcdr(type, &type);
			Return_getcar(type, &type);
			/* (intern-eql-specializer (quote value)) */
			GetConst(SYSTEM_INTERN_EQL_SPECIALIZER, &var);
		}
		else {
			/* (find-class (quote value)) */
			GetConst(COMMON_FIND_CLASS, &var);
		}
		list_heap(&type, quote, type, NULL);
		list_heap(&type, var, type, NULL);
		cons_heap(&root, type, root);
	}
	nreverse(&root, root);
	/* result */
	if (root != Nil) {
		GetConst(COMMON_LIST, &list);
		cons_heap(&root, list, root);
	}

	return Result(ret, root);
}

static int defmethod_parse_documentation_(addr form, addr *ret)
{
	addr decl, body;
	return split_decl_body_doc_(form, ret, &decl, &body);
}

static int defmethod_parse_function_(Execute ptr,
		addr env, addr form, addr ord, addr *ret)
{
	/* `(lambda (,method ,next &rest ,args)
	 *    (flet ((next-method-p ()
	 *             (clos::flet-method-p ,next))
	 *           (call-next-method (&rest ,rest)
	 *             (clos::flet-next-method ,method ,next ,args ,rest)))
	 *      (declare (ignorable #'next-method-p #'call-next-method))
	 *      "Documentation"
	 *      (apply (lambda ,lambda-list ,@form) ,args)))
	 */
	addr lambda, apply, next1, next2, call1, call2, a, b, c, doc;
	addr method, next, args, rest, ignorable, declare, arest, flet;

	/* gensym */
	Return(make_gensym_(ptr, &method));
	Return(make_gensym_(ptr, &next));
	Return(make_gensym_(ptr, &args));
	Return(make_gensym_(ptr, &rest));
	/* constant */
	GetConst(COMMON_NEXT_METHOD_P, &next1);
	GetConst(CLOSNAME_FLET_METHOD_P, &next2);
	GetConst(COMMON_CALL_NEXT_METHOD, &call1);
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &call2);
	/* lambda */
	GetConst(COMMON_LAMBDA, &lambda);
	Return(argument_method_lambda_heap_(&ord, ord));
	Return(defmethod_parse_documentation_(form, &doc));
	lista_heap(&ord, lambda, ord, form, NULL);
	/* apply */
	GetConst(COMMON_APPLY, &apply);
	list_heap(&apply, apply, ord, args, NULL);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_FUNCTION, &a);
	list_heap(&b, a, next1, NULL);
	list_heap(&c, a, call1, NULL);
	list_heap(&ignorable, ignorable, b, c, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	/* next-method-p */
	list_heap(&next2, next2, next, NULL);
	list_heap(&next1, next1, Nil, next2, NULL);
	/* call-next-method */
	list_heap(&call2, call2, method, next, args, rest, NULL);
	GetConst(AMPERSAND_REST, &arest);
	list_heap(&rest, arest, rest, NULL);
	list_heap(&call1, call1, rest, call2, NULL);
	/* flet */
	list_heap(&next1, next1, call1, NULL);
	GetConst(COMMON_FLET, &flet);
	list_heap(&flet, flet, next1, declare, apply, NULL);
	/* lambda */
	list_heap(&method, method, next, arest, args, NULL);
	list_heap(ret, lambda, method, doc, flet, NULL);

	return 0;
}

int defmethod_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, qua, spec, lambda, list, quote;
	addr key1, key2, key3, key4, key5;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! function_name_p(name))
		return fmte_("Invalid function name ~S.", name, NULL);
	if (defmethod_parse_qualifiers(args, &qua, &lambda, &args))
		goto error;
	Return(argument_method_heap_(ptr->local, &list, lambda));
	Return(defmethod_parse_specializers(list, &spec));
	Return(defmethod_parse_function_(ptr, env, args, list, &args));

	/* name qua lambda doc decl args */
	GetConst(COMMON_QUOTE, &quote);
	GetConst(CLOSNAME_ENSURE_METHOD, &key1);
	GetConst(CLOSKEY_LAMBDA_LIST, &key2);
	GetConst(CLOSKEY_QUALIFIERS, &key3);
	GetConst(CLOSKEY_SPECIALIZERS, &key4);
	GetConst(CLOSKEY_FUNCTION, &key5);
	list_heap(&name, quote, name, NULL);
	list_heap(&lambda, quote, lambda, NULL);
	if (qua != Nil)
		list_heap(&qua, quote, qua, NULL);
	list_heap(ret, key1, name, key2, lambda, key3, qua, key4, spec, key5, args, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("DEFMETHOD argument ~S must be a "
			"(name qualifier* lambda-list &body form).", form, NULL);
}


/*
 *  define-method-combination
 */
static int defcomb_short(addr *ret, addr list, addr name)
{
	addr doc, ident, oper, key, value;
	addr kdoc, kident, koper, quote;

	/* parse */
	GetConst(KEYWORD_DOCUMENTATION, &kdoc);
	GetConst(KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, &kident);
	GetConst(KEYWORD_OPERATOR, &koper);
	doc = ident = oper = Unbound;
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == kdoc) {
			if (! stringp(value))
				return fmte_(":DOCUMENTATION ~S must be a symbol.", value, NULL);
			doc = value;
			continue;
		}
		if (key == kident) {
			ident = (value != Nil)? T: Nil;
			continue;
		}
		if (key == koper) {
			if (! symbolp(value))
				return fmte_(":OPERATOR ~S must be a symbol.", value, NULL);
			oper = value;
			continue;
		}
		return fmte_("Invalid argument ~S ~S.", key, value, NULL);
	}

	/* `(ensure-method-combination-short
	 *    (quote ,name)
	 *    :documentation ,doc
	 *    :identity-with-one-argument ,ident
	 *    :operator (quote ,oper))
	 */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, &key);
	conscar_heap(&list, key);
	GetConst(COMMON_QUOTE, &quote);
	quotelist_heap(&name, name);
	cons_heap(&list, name, list);
	/* documentation */
	if (doc != Unbound)
		pushva_heap(&list, kdoc, doc, NULL);
	/* identity-with-one-argument */
	if (ident != Unbound)
		pushva_heap(&list, kident, ident, NULL);
	/* operator */
	if (oper != Unbound) {
		quotelist_heap(&oper, oper);
		pushva_heap(&list, koper, oper, NULL);
	}
	/* result */
	nreverse(ret, list);
	return 0;
}

static int defcomb_split_body(addr list, addr *rargs, addr *rgen, addr *rbody)
{
	addr next, args, gen, a, b, c, kargs, kgen;

	GetConst(KEYWORD_ARGUMENTS, &kargs);
	GetConst(KEYWORD_GENERIC_FUNCTION, &kgen);
	gen = Unbound;
	args = Nil;
	while (list != Nil) {
		Return_getcons(list, &a, &next);
		if (! consp_getcons(a, &a, &b))
			break;
		/* (:arguments . args) */
		if (a == kargs) {
			if (args == Nil)
				args = b;
			list = next;
			continue;
		}
		/* (:generic-function gen) */
		if (a == kgen) {
			if (! consp(b))
				return fmte_(":GENERIC-FUNCTION ~S must be a cons form.", b, NULL);
			GetCons(b, &a, &c);
			if (c != Nil)
				return fmte_("Invalid :GENERIC-FUNCTION form ~S.", b, NULL);
			if (! symbolp(a))
				return fmte_(":GENERIC-FUNCTION ~S must be a symbol.", a, NULL);
			if (gen == Unbound)
				gen = a;
			list = next;
			continue;
		}
		/* form */
		break;
	}
	/* result */
	*rargs = args;
	*rgen = gen;
	*rbody = list;
	return 0;
}

static int defcomb_long_specifiers(addr *ret, addr list)
{
	addr root, name, spec, tail, key, value, check, order, req, desc;

	if (! listp(list)) {
		return fmte_("DEFINE-METHOD-COMBINATION specifiers ~S "
				"must be a list.", list, NULL);
	}
	for (root = Nil; list != Nil; ) {
		/* (name spec &key order required description) */
		Return_getcons(list, &spec, &list);
		Return_getcons(spec, &name, &spec);
		Return_getcons(spec, &spec, &tail);
		if ((! symbolp(spec)) && (! listp(spec))) {
			return fmte_("The qualifiers pattern ~S "
					"must be a symbol or list.", spec, NULL);
		}
		/* &key */
		order = req = desc = Unbound;
		while (tail != Nil) {
			Return_getcons(tail, &key, &tail);
			Return_getcons(tail, &value, &tail);
			/* order */
			GetConst(KEYWORD_ORDER, &check);
			if (key == check) {
				if (order == Unbound)
					order = value;
				continue;
			}
			/* required */
			GetConst(KEYWORD_REQUIRED, &check);
			if (key == check) {
				if (req == Unbound)
					req = value;
				continue;
			}
			/* desc */
			GetConst(KEYWORD_DESCRIPTION, &check);
			if (key == check) {
				if (desc == Unbound)
					desc = value;
				continue;
			}
			/* error */
			return fmte_("Invalid specifiers keyword ~S.", key, NULL);
		}
		if (order == Unbound)
			GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &order);
		if (req == Unbound)
			req = Nil;
		if (desc == Unbound)
			desc = Nil;
		list_heap(&name, name, spec, order, req, desc, NULL);
		cons_heap(&root, name, root);
	}
	nreverse(ret, root);
	return 0;
}

static int defcomb_long(LocalRoot local, addr form, addr env, addr *ret,
		addr list, addr name)
{
	addr pos, lambda, spec, args, gen, doc, body, decl;

	/* long form */
	GetCons(list, &lambda, &list);
	if (! consp_getcons(list, &spec, &list))
		goto error;
	args = gen = Nil;
	Return(defcomb_split_body(list, &args, &gen, &list));
	/* parser */
	Return(defcomb_long_specifiers(&spec, spec));
	Return(split_decl_body_doc_(list, &doc, &decl, &body));

	/* `(ensure-method-combination-long
	 *    (quote ,name)
	 *    (quote ,lambda)
	 *    (quote ,spec)
	 *    :arguments (quote ,args)
	 *    :generic-function (quote ,gen)
	 *    :documentation ,doc
	 *    :form (lambda ...)
	 */
	list = Nil;
	PushConst(&list, CLOSNAME_ENSURE_METHOD_COMBINATION_LONG);
	/* name */
	quotelist_heap(&pos, name);
	pushva_heap(&list, pos, NULL);
	/* lambda-list */
	Return(argument_ordinary_heap_(local, &pos, lambda));
	quotelist_heap(&pos, pos);
	pushva_heap(&list, pos, NULL);
	/* specifiers */
	quotelist_heap(&pos, spec);
	pushva_heap(&list, pos, NULL);
	/* arguments */
	if (args != Nil) {
		Return(argument_combination_heap_(local, &args, args));
		PushConst(&list, KEYWORD_ARGUMENTS);
		quotelist_heap(&pos, args);
		pushva_heap(&list, pos, NULL);
	}
	/* generic-function */
	if (gen != Unbound) {
		PushConst(&list, KEYWORD_GENERIC_FUNCTION);
		quotelist_heap(&pos, gen);
		pushva_heap(&list, pos, NULL);
	}
	/* documentation */
	if (doc != Nil) {
		PushConst(&list, KEYWORD_DOCUMENTATION);
		pushva_heap(&list, doc, NULL);
	}
	/* body */
	PushConst(&list, CLOSKEY_FORM);
	Return(comb_longmacro_(&body, lambda, spec, args, gen, decl, body));
	pushva_heap(&list, body, NULL);
	/* result */
	nreverse(ret, list);
	return 0;

error:
	*ret = Nil;
	return fmte_("Invalid DEFINE-METHOD-COMBINATION form ~S.", form, NULL);
}

int define_method_combination_common(
		LocalRoot local, addr form, addr env, addr *ret)
{
	addr list, name, check;

	/* arguments */
	Return_getcdr(form, &list);
	if (! consp_getcons(list, &name, &list))
		goto error;
	if (! symbolp(name))
		return fmte_("DEFINE-METHOD-COMBINATION name ~S must be a symbol.", name, NULL);
	if (list == Nil)
		return defcomb_short(ret, list, name);
	if (! consp_getcar(list, &check))
		goto error;
	if (keywordp(check))
		return defcomb_short(ret, list, name);
	return defcomb_long(local, form, env, ret, list, name);

error:
	*ret = Nil;
	return fmte_("Invalid DEFINE-METHOD-COMBINATION form ~S.", form, NULL);
}


/*
 *  make-load-form-saving-slots
 */
static void make_load_form_saving_slots_list(addr var, addr *ret)
{
	addr vector, list, x;
	size_t size, i;

	GetSlotClos(var, &vector);
	LenSlotVector(vector, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(vector, i, &x);
		GetNameSlot(x, &x);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);
}

int make_load_form_saving_slots_common(Execute ptr,
		addr var, addr list, addr env, addr *ret1, addr *ret2)
{
	/* (allocate-instance
	 *   (find-class
	 *     (quote class-name)))
	 * (lisp-system::set-slots var
	 *   (quote (slot1  slot2  ...))
	 *   (quote (value1 value2 ...)))
	 */
	addr alloc, find, call, name;
	addr set, root, values, x, y;

	/* type check */
	if (! closp(var)) {
		*ret1 = *ret2 = Nil;
		return fmte_("The object ~S must be a clos-object.", var, NULL);
	}

	/* first */
	GetConst(COMMON_ALLOCATE_INSTANCE, &alloc);
	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_CLASS_NAME, &call);
	Return(clos_class_of_(var, &name));
	Return(getfunction_global_(call, &call));
	Return(callclang_funcall(ptr, &name, call, name, NULL));
	quotelist_heap(&name, name);
	list_heap(&find, find, name, NULL);
	list_heap(&alloc, alloc, find, NULL);

	/* second */
	GetConst(SYSTEM_SET_SLOTS, &set);
	if (list == Unbound)
		make_load_form_saving_slots_list(var, &list);
	values = Nil;
	root = list;
	while (root != Nil) {
		Return_getcons(root, &x, &root);
		Return(clos_get_(var, x, &y));
		if (y == Unbound)
			GetConst(SYSTEM_UNBOUND_VALUE, &y);
		cons_heap(&values, y, values);
	}
	nreverse(&values, values);
	quotelist_heap(&list, list);
	quotelist_heap(&values, values);
	list_heap(&set, set, var, list, values, NULL);

	/* result */
	*ret1 = alloc;
	*ret2 = set;
	return 0;
}

int set_slots_syscall(addr var, addr slots, addr values)
{
	addr x, y, unbound;

	GetConst(SYSTEM_UNBOUND_VALUE, &unbound);
	while (slots != Nil || values != Nil) {
		Return_getcons(slots, &x, &slots);
		Return_getcons(values, &y, &values);
		if (y == unbound)
			y = Unbound;
		Return(clos_set_(var, x, y));
	}

	return 0;
}

