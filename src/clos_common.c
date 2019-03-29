#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "function.h"
#include "strtype.h"
#include "symbol.h"
#include "type_parse.h"

/*
 *  slot-boundp
 */
void slot_boundp_common(addr pos, addr name, addr *ret)
{
	fmte("TODO", NULL);
}


/*
 *  defclass
 */
static void defclass_parse_superclasses(addr args, addr *ret)
{
	addr root, pos, quote, find, list;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_FIND_CLASS, &find);
	GetConst(COMMON_LIST, &list);

	/* () -> (list (find-class (quote standard-object))) */
	if (args == Nil) {
		GetConst(COMMON_STANDARD_OBJECT, &pos);
		list_heap(&pos, quote, pos, NULL);
		list_heap(&pos, find, pos, NULL);
		list_heap(ret, list, pos, NULL);
		return;
	}

	/* (a b ...) ->
	 *   (list (find-class (quote a))
	 *         (find-class (quote b))
	 *         ...)
	 */
	for (root = Nil; args != Nil; ) {
		getcons(args, &pos, &args);
		list_heap(&pos, quote, pos, NULL);
		list_heap(&pos, find, pos, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(&root, root);
	cons_heap(ret, list, root);
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

	/* name */
	if (! consp(list))
		fmte("SLOT-SPECIFIER ~S must be a (name . tail) form.", list, NULL);
	GetCons(list, &name, &list);
	if (! symbolp(name))
		fmte("SLOT-NAME ~S must be a symbol.", name, NULL);

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
	while (list != Nil) {
		/* key - value */
		if (! consp(list))
			fmte("Invalid slot-specifier value ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			fmte("SLOT-SPECIFIER ~S must be a key-value form", list, NULL);
		GetCons(list, &value, &list);

		/* :reader */
		if (DefClassEqConst(key, READER)) {
			if (! non_nil_symbol_p(value))
				fmte(":READER ~S must be a non-nil symbol.", value, NULL);
			pushnew_heap(readers, value, &readers);
			continue;
		}

		/* :writer */
		if (DefClassEqConst(key, WRITER)) {
			if (! function_name_p(value))
				fmte(":WRITER ~S must be function name.", value, NULL);
			pushnew_equal_heap(writers, value, &writers);
			continue;
		}

		/* :accessor */
		if (DefClassEqConst(key, ACCESSOR)) {
			if (! non_nil_symbol_p(value))
				fmte(":ACCESSOR ~S must be a non-nil symbol.", value, NULL);
			pushnew_heap(readers, value, &readers);
			GetConst(COMMON_SETF, &pos);
			list_heap(&value, pos, value, NULL);
			pushnew_equal_heap(writers, value, &writers);
			continue;
		}

		/* :allocation */
		if (DefClassEqConst(key, ALLOCATION)) {
			if (! defclass_allocation_p(value))
				fmte(":ALLOCATION ~S must be a :instance or :class.", value, NULL);
			if (allocation != Nil)
				fmte(":ALLOCATION is already exist.", NULL);
			continue;
		}

		/* :initarg */
		if (DefClassEqConst(key, INITARG)) {
			if (! symbolp(value))
				fmte(":INITARG ~S must be a symbol.", value, NULL);
			pushnew_heap(initargs, value, &initargs);
			continue;
		}

		/* :initform */
		if (DefClassEqConst(key, INITFORM)) {
			if (initfunction != Nil)
				fmte(":INITFORM is already exist.", NULL);
			initform = value;
			GetConst(COMMON_LAMBDA, &pos);
			list_heap(&initfunction, pos, Nil, value, NULL);
			continue;
		}

		/* :type */
		if (DefClassEqConst(key, TYPE)) {
			if (type != Nil)
				fmte(":TYPE is already exist.", NULL);
			if (parse_type(ptr, &type, value, env))
				return 1;
			continue;
		}

		/* :document */
		if (DefClassEqConst(key, DOCUMENTATION)) {
			if (doc != Nil)
				fmte(":DOCUMENTATION is already exist.", NULL);
			if (! symbolp(value))
				fmte(":DOCUMENTATION ~S must be a symbol.", value, NULL);
			doc = value;
			continue;
		}

		/* otherwise */
		GetConst(COMMON_QUOTE, &pos);
		list_heap(&key, pos, key, NULL);
		list_heap(&value, pos, value, NULL);
		cons_heap(&others, key, others);
		cons_heap(&others, value, others);
	}

	/* list :name (quote name) */
	GetConst(COMMON_QUOTE, &quote);
	root = Nil;
	GetConst(COMMON_LIST, &pos);
	cons_heap(&root, pos, root);
	GetConst(KEYWORD_NAME, &pos);
	cons_heap(&root, pos, root);
	list_heap(&pos, pos, name, NULL);
	cons_heap(&root, pos, root);
	/* :readers (quote (a b c ...)) */
	if (readers != Nil) {
		GetConst(CLOSKEY_READERS, &pos);
		cons_heap(&root, pos, root);
		nreverse_list_unsafe(&readers, readers);
		list_heap(&pos, quote, readers, NULL);
		cons_heap(&root, pos, root);
	}
	/* :writers (quote (a b c ...)) */
	if (writers != Nil) {
		GetConst(CLOSKEY_WRITERS, &pos);
		cons_heap(&root, pos, root);
		nreverse_list_unsafe(&writers, writers);
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
		nreverse_list_unsafe(&initargs, initargs);
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
		nreverse_list_unsafe(&others, others);
		while (others != Nil) {
			GetCons(others, &pos, &others);
			cons_heap(&root, pos, root);
		}
	}
	/* result */
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int defclass_parse_slot(Execute ptr, addr env, addr list, addr *ret)
{
	addr symbol, keyword;

	/* symbol */
	if (symbolp(list)) {
		/* (list :name value) */
		GetConst(COMMON_LIST, &symbol);
		GetConst(CLOSKEY_NAME, &keyword);
		list_heap(ret, symbol, keyword, list, NULL);
		return 0;
	}

	/* list */
	if (consp(list))
		return defclass_parse_slotlist(ptr, env, list, ret);

	/* error */
	fmte("DEFCLASS slot-specifier ~S must be a list or a symbol,", list, NULL);
	return 0;
}

static int defclass_parse_slots(Execute ptr, addr env, addr list, addr *ret)
{
	addr root, pos;

	/* check */
	if (list == Nil) {
		*ret = Nil;
		return 0;
	}
	/* (list ,@mapcar) */
	GetConst(COMMON_LIST, &root);
	conscar_heap(&root, root);
	while (list != Nil) {
		if (! consp(list))
			fmte("DEFCLASS slot-specifier ~S must be a list.", list, NULL);
		GetCons(list, &pos, &list);
		if (defclass_parse_slot(ptr, env, pos, &pos))
			return 1;
		cons_heap(&root, pos, root);
	}
	/* result */
	nreverse_list_unsafe(ret, root);

	return 0;
}

static void defclass_parse_options(addr list, addr *ret)
{
	addr root, key, value;

	for (root = Nil; list != Nil; ) {
		if (! consp(list))
			fmte("DEFCLASS options ~S don't allow dotted list.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(key))
			fmte("DEFCLASS option ~S must be a cons.", key, NULL);
		GetCons(key, &key, &value);

		/* :metaclass */
		if (DefClassEqConst(key, METACLASS)) {
			if (! singlep(value))
				fmte("Invalid :METACLASS ~S.", value, NULL);
			GetCar(value, &value);
			if (! non_nil_symbol_p(value))
				fmte(":METACLASS ~S must be a non-nil symbol.", value, NULL);
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
			cons_heap(&root, key, root);
			cons_heap(&root, value, root);
			continue;
		}

		/* :documentation */
		if (DefClassEqConst(key, DOCUMENTATION)) {
			if (! singlep(value))
				fmte("Invalid :DOCUMENTATION ~S.", value, NULL);
			GetCar(value, &value);
			if (! stringp(value))
				fmte(":DOCUMENTATION ~S must be a string.", value, NULL);
			/* :documentation value */
			cons_heap(&root, key, root);
			cons_heap(&root, value, root);
			continue;
		}

		/* otherwise */
		if (! singlep(value))
			fmte("Invalid option ~S.", value, NULL);
		GetCar(value, &value);
		/* key value */
		cons_heap(&root, key, root);
		cons_heap(&root, value, root);
	}

	/* result */
	nreverse_list_unsafe(ret, root);
}

int defclass_common(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(system::ensure-class ',name
	 *     :direct-superclasses ,supers
	 *     :direct-slots ,slots
	 *     ,@class-options)
	 */
	addr args, name, supers, slots, options, ensure, key1, key2;

	/* destructuring-bind */
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &name, &args);
	if (! consp(args)) goto error;
	GetCons(args, &supers, &args);
	if (! consp(args)) goto error;
	GetCons(args, &slots, &options);

	/* parse */
	defclass_parse_superclasses(supers, &supers);
	if (defclass_parse_slots(ptr, env, slots, &slots)) return 1;
	defclass_parse_options(options, &options);

	/* make */
	GetConst(CLOSNAME_ENSURE_CLASS, &ensure);
	GetConst(CLOSKEY_DIRECT_SUPERCLASSES, &key1);
	GetConst(CLOSKEY_DIRECT_SLOTS, &key2);
	lista_heap(ret, ensure, name, key1, supers, key2, slots, options, NULL);
	return 0;

error:
	fmte("The defclass ~S must be a "
			"(defclass name (superclasses) (slots) ...) form.", form, NULL);
	return 0;
}


/*
 *  ensure-class
 */
void ensure_class_common(Execute ptr, addr name, addr args, addr *ret)
{
}


/*
 *  find-class
 *    TODO: environment
 */
void find_class_common(addr pos, int errorp, addr env, addr *ret)
{
	Check(! symbolp(pos), "type error");
	if (errorp)
		clos_find_class(pos, ret);
	else
		clos_find_class_nil(pos, ret);
}

