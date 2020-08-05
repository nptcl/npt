#include "call_structures.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "gc.h"
#include "integer.h"
#include "lambda.h"
#include "package_symbol.h"
#include "structure.h"
#include "strtype.h"
#include "symbol.h"
#include "type_parse.h"

/* defstruct-slots */
static int defstruct_parse_slot(struct defstruct *str, addr pos,
		addr *rname, addr *rinit, addr *rtype, addr *rreadonly)
{
	addr gensym, list, name, init, type, readonly, key, value, key1, key2;

	/* name */
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	name = init = type = readonly = gensym;
	if (! consp(pos)) {
		name = pos;
		goto finish;
	}

	/* (name) */
	GetCons(pos, &name, &list);
	if (! symbolp(name))
		return fmte_("DEFSTRUCT slot-name ~S must be a symbol.", name, NULL);
	if (list == Nil)
		goto finish;

	/* (name init) */
	if (! consp(list))
		return fmte_("Invalid DEFSTRUCT slot-option ~S.", pos, NULL);
	GetCons(list, &init, &list);
	if (list == Nil)
		goto finish;

	/* options */
	GetConst(KEYWORD_TYPE, &key1);
	GetConst(KEYWORD_READ_ONLY, &key2);
	while (list != Nil) {
		if (! consp(list))
			return fmte_("Invalid DEFSTRUCT slot-option key ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			return fmte_("Invalid DEFSTRUCT slot-option value ~S.", list, NULL);
		GetCons(list, &value, &list);
		/* :type */
		if (key == key1) {
			if (type == gensym) {
				Return(parse_type(str->ptr, &type, value, str->env));
			}
			continue;
		}
		/* :read-only */
		if (key == key2) {
			if (readonly == gensym) {
				readonly = (value == Nil)? Nil: T;
			}
			continue;
		}
		/* error */
		return fmte_("Invalid DEFSTRUCT slot-option ~S.", key, NULL);
	}

finish:
	if (! symbolp(name))
		return fmte_("DEFSTRUCT slot-name ~S must be a symbol.", name, NULL);
	*rname = name;
	*rinit = init;
	*rtype = type;
	*rreadonly = readonly;
	return 0;
}

static int defstruct_parse_slots_result(struct defstruct *str, addr list, addr *ret)
{
	addr root, pos, name, init, type, readonly;
	LocalHold hold;

	name = init = type = readonly = NULL;
	hold = LocalHold_array(str->ptr, 1);
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(defstruct_parse_slot(str, pos, &name, &init, &type, &readonly));
		list_heap(&pos, name, init, type, readonly, NULL);
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int defstruct_parse_slots(struct defstruct *str, addr list)
{
	Return(defstruct_parse_slots_result(str, list, &list));
	str->slots = list;
	return 0;
}


/* defstruct-name */
static int defstruct_parse_name_option1(constindex index, addr option, addr *ret)
{
	addr key, check, tail;

	GetConstant(index, &key);
	if (key == option) {
		/* :keyword */
		return Result(ret, Unbound);
	}
	if (! consp(option)) {
		/* :others */
		return Result(ret, NULL);
	}
	GetCons(option, &check, &tail);
	if (key != check) {
		/* (:others ...) */
		return Result(ret, NULL);
	}
	if (tail == Nil) {
		/* (:keyword) */
		return Result(ret, Unbound);
	}
	if (! consp(tail)) {
		/* (:keyword . xxx) */
		goto error;
	}
	GetCons(tail, &check, &tail);
	if (tail != Nil) {
		/* (:keyword name . xxx) */
		goto error;
	}
	/* (:keyword value) */
	return Result(ret, check);

error:
	*ret = NULL;
	return fmte_("Invalid DEFSTRUCT option ~S.", option, NULL);
}
#define defstruct_option1(x,y,z) \
	defstruct_parse_name_option1(CONSTANT_KEYWORD_##x,(y),(z))

static int defstruct_parse_conc_name(struct defstruct *str, addr pos, int *ret)
{
	int check;

	Return(defstruct_option1(CONC_NAME, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (str->conc_name_p)
		return fmte_("DEFSTRUCT :CONC-NAME is already exist.", NULL);
	str->conc_name_p = 1;
	if (pos == Unbound || pos == Nil) {
		str->conc_name = Nil;
		return Result(ret, 1);
	}
	Return(string_designer_heap_(&pos, pos, &check));
	if (check) {
		str->conc_name = pos;
		return Result(ret, 1);
	}

	return fmte_("DEFSTRUCT :CONC-NAME ~S must be a string-designer.", pos, NULL);
}

static int defstruct_parse_copier(struct defstruct *str, addr pos, int *ret)
{
	int check;

	Return(defstruct_option1(COPIER, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (str->copier_p)
		return fmte_("DEFSTRUCT :COPIER is already exist.", NULL);
	if (pos == Unbound) {
		pos = T;
		goto store;
	}
	if (pos == Nil)
		goto store;
	Return(string_designer_heap_(&pos, pos, &check));
	if (check)
		goto store;
	return fmte_("DEFSTRUCT :COPIER ~S must be a symbol.", pos, NULL);

store:
	str->copier_p = 1;
	str->copier = pos;
	return Result(ret, 1);
}

static int defstruct_parse_predicate(struct defstruct *str, addr pos, int *ret)
{
	int check;

	Return(defstruct_option1(PREDICATE, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (str->predicate_p)
		return fmte_("DEFSTRUCT :PREDICATE is already exist.", NULL);
	if (pos == Unbound) {
		pos = T;
		goto store;
	}
	if (pos == Nil)
		goto store;
	Return(string_designer_heap_(&pos, pos, &check));
	if (check)
		goto store;
	return fmte_("DEFSTRUCT :PREDICATE ~S must be a symbol.", pos, NULL);

store:
	str->predicate_p = 1;
	str->predicate = pos;
	return Result(ret, 1);
}

static int defstruct_parse_constructor2(addr option, addr *ret1, addr *ret2)
{
	addr key, pos1, pos2, tail;

	GetConstant(CONSTANT_KEYWORD_CONSTRUCTOR, &key);
	if (key == option) {
		/* :constructor */
		*ret1 = Unbound;
		*ret2 = Unbound;
		return 0;
	}
	if (! consp(option)) {
		/* :others */
		return Result(ret1, NULL);
	}
	GetCons(option, &pos1, &tail);
	if (key != pos1) {
		/* (:others ...) */
		return Result(ret1, NULL);
	}
	if (tail == Nil) {
		/* (:constructor) */
		*ret1 = Unbound;
		*ret2 = Unbound;
		return 0;
	}
	if (! consp(tail)) {
		/* (:constructor . xxx) */
		goto error;
	}
	GetCons(tail, &pos1, &tail);
	if (tail == Nil) {
		/* (:constructor ret1) */
		*ret1 = pos1;
		*ret2 = Unbound;
		return 0;
	}
	if (! consp(tail)) {
		/* (:constructor pos1 . xxx) */
		goto error;
	}
	GetCons(tail, &pos2, &tail);
	if (tail != Nil) {
		/* (:constructor name . xxx) */
		goto error;
	}
	/* (:constructor pos1 pos2) */
	*ret1 = pos1;
	*ret2 = pos2;
	return 0;

error:
	*ret1 = NULL;
	return fmte_("Invalid DEFSTRUCT option ~S.", option, NULL);
}

static int defstruct_parse_constructor(struct defstruct *str, addr pos, int *ret)
{
	addr args, g;

	Return(defstruct_parse_constructor2(pos, &pos, &args));
	if (pos == NULL)
		return Result(ret, 0);
	if (pos == Unbound) {
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
	}
	if (! symbolp(pos))
		return fmte_(":CONSTRUCTOR name ~S must be a symbol.", pos, NULL);
	str->constructor_p = 1;
	if (pos == Nil) {
		str->constructor = Nil;
		return Result(ret, 1);
	}
	if (args != Unbound) {
		GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
		quotelist_heap(&g, g);
		Return(argument_boa_heap_(str->ptr->local, &args, args, g));
		cons_heap(&pos, pos, args);
	}
	cons_heap(&(str->constructor), pos, str->constructor);

	return Result(ret, 1);
}

static int defstruct_parse_include2(addr option, addr *ret1, addr *ret2)
{
	addr key, check, tail;

	if (! consp(option)) {
		/* :others */
		return Result(ret1, NULL);
	}
	GetConstant(CONSTANT_KEYWORD_INCLUDE, &key);
	GetCons(option, &check, &tail);
	if (key != check) {
		/* (:others ...) */
		return Result(ret1, NULL);
	}
	if (! consp(tail)) {
		/* (:include . xxx) */
		goto error;
	}
	GetCons(tail, ret1, ret2);
	return 0;

error:
	*ret1 = *ret2 = NULL;
	return fmte_("DEFSTRUCT :INCLUDE option ~S "
			"must be a (:include name . slots) form.", option, NULL);
}

static int defstruct_parse_include(struct defstruct *str, addr pos, int *ret)
{
	addr args;

	Return(defstruct_parse_include2(pos, &pos, &args));
	if (pos == NULL)
		return Result(ret, 0);
	if (! symbolp(pos))
		return fmte_(":INCLUDE name ~S must be a symbol.", pos, NULL);
	if (str->include_p)
		return fmte_("DEFSTRUCT :INCLUDE is already exist.", NULL);
	str->include_p = 1;
	str->iname = pos;
	str->iargs = args;

	return Result(ret, 1);
}

static int defstruct_parse_print_object1(constindex index, addr option, addr *ret)
{
	addr key, check, tail;

	GetConstant(index, &key);
	if (key == option) {
		/* :option */
		return Result(ret, Unbound);
	}
	if (! consp(option)) {
		/* :others */
		return Result(ret, NULL);
	}
	GetCons(option, &check, &tail);
	if (key != check) {
		/* (:others ...) */
		return Result(ret, NULL);
	}
	if (tail == Nil) {
		/* (:print-object) */
		return Result(ret, Unbound);
	}
	if (! consp(tail)) {
		/* (:print-object . xxx) */
		goto error;
	}
	GetCons(tail, &check, &tail);
	if (tail != Nil) {
		/* (:print-object name . xxx) */
		goto error;
	}
	/* (:print-object value) */
	return Result(ret, check);

error:
	return fmte_("Invalid DEFSTRUCT option ~S.", option, NULL);
}

static int defstruct_parse_print_object(struct defstruct *str, addr pos, int *ret)
{
	Return(defstruct_parse_print_object1(CONSTANT_KEYWORD_PRINT_OBJECT, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (pos == Unbound)
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
	if (str->print_object_p)
		return fmte_("DEFSTRUCT :PRINT-OBJECT is already exist.", NULL);
	if (str->print_function_p)
		return fmte_("DEFSTRUCT :PRINT-FUNCTION is already exist.", NULL);
	str->print_object_p = 1;
	str->print_object = pos;

	return Result(ret, 1);
}

static int defstruct_parse_print_function(struct defstruct *str, addr pos, int *ret)
{
	Return(defstruct_parse_print_object1(CONSTANT_KEYWORD_PRINT_FUNCTION, pos, &pos));
	if (pos == NULL)
		return Result(ret, 0);
	if (pos == Unbound)
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
	if (str->print_object_p)
		return fmte_("DEFSTRUCT :PRINT-OBJECT is already exist.", NULL);
	if (str->print_function_p)
		return fmte_("DEFSTRUCT :PRINT-FUNCTION is already exist.", NULL);
	str->print_function_p = 1;
	str->print_function = pos;

	return Result(ret, 1);
}

static int defstruct_parse_type(struct defstruct *str, addr option, int *ret)
{
	addr key, check, pos, a, b;

	/* parse */
	if (! consp(option))
		return Result(ret, 0);
	GetCons(option, &check, &pos);
	GetConst(KEYWORD_TYPE, &key);
	if (key != check)
		return Result(ret, 0);
	if (! consp(pos))
		goto error;
	GetCons(pos, &pos, &check);
	if (check != Nil)
		goto error;
	if (str->type_p)
		return fmte_("DEFSTRUCT :TYPE already exists.", NULL);
	/* list */
	GetConst(COMMON_LIST, &check);
	if (pos == check) {
		str->type_p = 1;
		str->type_list_p = 1;
		return Result(ret, 1);
	}
	/* vector */
	GetConst(COMMON_VECTOR, &check);
	if (pos == check) {
		str->type_p = 1;
		str->type_vector_p = 1;
		str->type_vector = T;
		return Result(ret, 1);
	}
	/* (vector type) */
	if (! consp(pos))
		goto type_error;
	GetCons(pos, &a, &b);
	GetConst(COMMON_VECTOR, &check);
	if (a != check)
		goto type_error;
	if (! consp(b))
		goto type_error;
	GetCons(b, &a, &b);
	if (b != Nil)
		goto type_error;
	str->type_p = 1;
	str->type_vector_p = 1;
	str->type_vector = a;
	return Result(ret, 1);

error:
	return fmte_("DEFSTRUCT :TYPE must be a (:type type) form.", option, NULL);

type_error:
	return fmte_("Invalid :TYPE argument ~S.", pos, NULL);
}

static int defstruct_parse_named(struct defstruct *str, addr option, int *ret)
{
	addr key;

	GetConst(KEYWORD_NAMED, &key);
	if (option != key)
		return Result(ret, 0);
	if (str->named_p)
		return fmte_("DEFSTRUCT :named already exists.", NULL);
	str->named_p = 1;

	return Result(ret, 1);
}

static int defstruct_parse_initial_offset(struct defstruct *str, addr option, int *ret)
{
	addr key, check, pos;
	size_t size;

	/* parse */
	if (! consp(option))
		return Result(ret, 0);
	GetCons(option, &check, &pos);
	GetConst(KEYWORD_INITIAL_OFFSET, &key);
	if (key != check)
		return Result(ret, 0);
	if (! consp(pos))
		goto error;
	GetCons(pos, &check, &pos);
	if (pos != Nil)
		goto error;
	if (str->initial_offset_p)
		return fmte_("DEFSTRUCT :INITIAL-OFFSET already exists.", NULL);
	getindex_integer(check, &size);
	str->initial_offset_p = 1;
	str->initial_offset = check;

	return Result(ret, 1);

error:
	return fmte_("DEFSTRUCT :INITIAL-OFFSET must be a "
			"(:initial-offset offset) form.", option, NULL);
}

static int defstruct_parse_name(struct defstruct *str, addr name)
{
	int check;
	addr list, pos;
	LocalHold hold;

	if (symbolp(name)) {
		str->name = name;
		return 0;
	}
	if (! consp(name))
		return fmte_("DEFSTRUCT name ~S must be symbol or list.", name, NULL);
	GetCons(name, &name, &list);
	if (! symbolp(name))
		return fmte_("DEFSTRUCT name ~S must be a symbol.", name, NULL);
	str->name = name;
	str->constructor = Nil;
	/* loop */
	while (list != Nil) {
		if (! consp(list))
			return fmte_("DEFSTRUCT name option ~S must be a list.", list, NULL);
		GetCons(list, &pos, &list);
		Return(defstruct_parse_conc_name(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_constructor(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_copier(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_predicate(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_include(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_print_object(str, pos, &check))
			if (check)
				continue;
		Return(defstruct_parse_print_function(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_type(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_named(str, pos, &check));
		if (check)
			continue;
		Return(defstruct_parse_initial_offset(str, pos, &check));
		if (check)
			continue;
		return fmte_("Invalid DEFSTRUCT option ~S.", pos, NULL);
	}

	/* parse slots */
	hold = LocalHold_local(str->ptr);
	localhold_destruct(hold, str);
	if (str->include_p) {
		Return(defstruct_parse_slots_result(str, str->iargs, &(str->iargs)));
	}

	/* parse-type */
	if (str->type_vector_p) {
		Return(parse_type(str->ptr, &(str->type_vector), str->type_vector, str->env));
	}
	localhold_end(hold);

	/* named check */
	if (str->named_p && (! str->type_p))
		return fmte_("There is :NAMED option but no :TYPE option.", NULL);

	/* initial-offset check */
	if (str->initial_offset_p && (! str->type_p))
		return fmte_("There is :INITIAL-OFFSET option but no :TYPE option.", NULL);

	return 0;
}

static void defstruct_parse_document(struct defstruct *str, addr pos, addr *ret)
{
	addr a, b;

	if (! consp(pos)) {
		str->doc = NULL;
		*ret = pos;
		return;
	}
	GetCons(pos, &a, &b);
	if (! stringp(a)) {
		str->doc = NULL;
		*ret = pos;
		return;
	}
	str->doc = a;
	*ret = b;
}

static int defstruct_parse(struct defstruct *str, addr form)
{
	addr args, name;

	Return_getcdr(form, &args);
	if (! consp(args)) {
		return fmte_("DEFSTRUCT form ~S "
				"must be a (defstruct name [doc] {slot}*", form, NULL);
	}
	GetCons(args, &name, &args);
	Return(defstruct_parse_name(str, name));
	defstruct_parse_document(str, args, &args);
	return defstruct_parse_slots(str, args);
}

static void defstruct_slots_list(addr *ret, addr slots, addr first)
{
	addr root, pos, name, init, type, readonly;
	addr list, lambda, gensym;

	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	root = Nil;
	if (first != Unbound)
		cons_heap(&root, first, root);
	while (slots != Nil) {
		GetCons(slots, &pos, &slots);
		List_bind(pos, &name, &init, &type, &readonly, NULL);
		quotelist_heap(&name, name);
		quotelist_heap(&type, type);
		quotelist_heap(&readonly, readonly);
		if (init == gensym)
			quotelist_heap(&init, init);
		else
			list_heap(&init, lambda, Nil, init, NULL);
		list_heap(&pos, list, name, init, type, readonly, NULL);
		cons_heap(&root, pos, root);
	}
	if (root == Nil) {
		*ret = Nil;
	}
	else {
		nreverse(&root, root);
		cons_heap(ret, list, root);
	}
}

static int defstruct_constructor_body_(addr *ret, addr name, addr cons)
{
	addr root, symbol, keyword, package, call;

	/* (:slot1 slot1 :slot2 slot2 ...) */
	GetConst(PACKAGE_KEYWORD, &package);
	argument_boa_variables_heap(&cons, cons);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &symbol, &cons);
		GetNameSymbol(symbol, &keyword);
		Return(intern_package_(package, keyword, &keyword, NULL));
		cons_heap(&root, keyword, root);
		cons_heap(&root, symbol, root);
	}
	nreverse(&root, root);

	/* (lisp-system::structure-constructor 'name ...) */
	GetConst(SYSTEM_STRUCTURE_CONSTRUCTOR, &call);
	quotelist_heap(&name, name);
	lista_heap(ret, call, name, root, NULL);

	return 0;
}

static int defstruct_constructor_lambda_(addr *ret, addr cons, addr symbol)
{
	/* (lambda (...)
	 *   (lisp-system::structure-constructor 'name
	 *     :slot1 slot1 :slot2 slot2 ...))
	 */
	addr name, lambda, args, body, list;

	GetCons(cons, &name, &cons);
	argument_boa_lambda_heap(&args, cons);
	Return(defstruct_constructor_body_(&body, symbol, cons));
	GetConst(COMMON_LAMBDA, &lambda);
	list_heap(&args, lambda, args, body, NULL);
	/* (list 'name (lambda...)) */
	GetConst(COMMON_LIST, &list);
	quotelist_heap(&name, name);
	list_heap(ret, list, name, args, NULL);

	return 0;
}

static int defstruct_make_constructor_(struct defstruct *str, addr *ret, addr root)
{
	int check;
	addr list, symbol, keyword, pos;

	check = str->constructor_p;
	list = str->constructor;
	symbol = str->name;
	GetConst(KEYWORD_CONSTRUCTOR, &keyword);
	if (! check) {
		cons_heap(&root, keyword, root);
		GetConst(SYSTEM_STRUCTURE_GENSYM, &pos);
		quotelist_heap(&pos, pos);
		cons_heap(&root, pos, root);
		return Result(ret, root);
	}
	while (list != Nil) {
		GetCons(list, &pos, &list);
		cons_heap(&root, keyword, root);
		if (consp(pos)) {
			Return(defstruct_constructor_lambda_(&pos, pos, symbol));
		}
		else {
			quotelist_heap(&pos, pos);
		}
		cons_heap(&root, pos, root);
	}

	return Result(ret, root);
}

static void defstruct_make_print_object(addr *ret, addr pos)
{
	addr symbol, g;

	/* gensym */
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (pos == g) {
		quotelist_heap(ret, g);
		return;
	}

	/* symbol */
	if (symbolp(pos)) {
		GetConst(COMMON_FUNCTION, &symbol);
		list_heap(&pos, symbol, pos, NULL);
	}

	/* lambda */
	GetConst(COMMON_LAMBDA, &symbol);
	list_heap(ret, symbol, Nil, pos, NULL);
}

static int defstruct_make_(struct defstruct *str, addr *ret)
{
	/* `(ensure-structure
	 *    ',name
	 *    (list (list ',name (lambda () ,init) ',type ',readonly)
	 *          (list ...))
	 *    :documentation ,doc
	 *    :conc-name ',conc-name
	 */
	addr root, pos;

	/* ensure-structure */
	root = Nil;
	GetConst(SYSTEM_ENSURE_STRUCTURE, &pos);
	cons_heap(&root, pos, root);
	/* name, slots */
	quotelist_heap(&pos, str->name);
	cons_heap(&root, pos, root);
	defstruct_slots_list(&pos, str->slots, Unbound);
	cons_heap(&root, pos, root);
	/* :documentation */
	if (str->doc) {
		GetConst(KEYWORD_DOCUMENTATION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, str->doc, root);
	}
	/* :conc-name */
	if (str->conc_name_p) {
		GetConst(KEYWORD_CONC_NAME, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->conc_name);
		cons_heap(&root, pos, root);
	}
	/* :type */
	if (str->type_list_p) {
		GetConst(KEYWORD_TYPE, &pos);
		cons_heap(&root, pos, root);
		GetConst(COMMON_LIST, &pos);
		quotelist_heap(&pos, pos);
		cons_heap(&root, pos, root);
	}
	if (str->type_vector_p) {
		GetConst(KEYWORD_TYPE, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->type_vector);
		cons_heap(&root, pos, root);
	}
	/* :named */
	if (str->named_p) {
		GetConst(KEYWORD_NAMED, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, T, root);
	}
	/* :initial-offset */
	if (str->initial_offset_p) {
		GetConst(KEYWORD_INITIAL_OFFSET, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->initial_offset);
		cons_heap(&root, pos, root);
	}
	/* :copier */
	if (str->copier_p && str->copier != T) {
		GetConst(KEYWORD_COPIER, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->copier);
		cons_heap(&root, pos, root);
	}
	/* :predicate */
	if (str->predicate_p) {
		GetConst(KEYWORD_PREDICATE, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->predicate);
		cons_heap(&root, pos, root);
	}
	/* :include */
	if (str->include_p) {
		/* :include (list 'iname ...) */
		GetConst(KEYWORD_INCLUDE, &pos);
		cons_heap(&root, pos, root);
		quotelist_heap(&pos, str->iname);
		defstruct_slots_list(&pos, str->iargs, pos);
		cons_heap(&root, pos, root);
	}
	if (str->print_object_p) {
		/* :print-object ... */
		GetConst(KEYWORD_PRINT_OBJECT, &pos);
		cons_heap(&root, pos, root);
		defstruct_make_print_object(&pos, str->print_object);
		cons_heap(&root, pos, root);
	}
	if (str->print_function_p) {
		/* :print-function ... */
		GetConst(KEYWORD_PRINT_FUNCTION, &pos);
		cons_heap(&root, pos, root);
		defstruct_make_print_object(&pos, str->print_function);
		cons_heap(&root, pos, root);
	}
	/* :constructor */
	Return(defstruct_make_constructor_(str, &root, root));
	/* result */
	nreverse(ret, root);

	return 0;
}

_g int defstruct_common(Execute ptr, addr form, addr env, addr *ret)
{
	struct defstruct str;
	LocalHold hold;

	defstruct_clean(&str);
	str.ptr = ptr;
	str.env = env;
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	Return(defstruct_parse(&str, form));
	Return(defstruct_make_(&str, &form));
	localhold_end(hold);

	return Result(ret, form);
}

