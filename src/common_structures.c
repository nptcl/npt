/*
 *  ANSI COMMON LISP: 8. Structures
 */
#include "common_header.h"
#include "cons.h"
#include "lambda.h"
#include "strtype.h"
#include "structure.h"
#include "type_parse.h"

/*
 *  (defmacro defstruct (name [doc] slots*) ...) -> symbol
 */
struct defstruct_struct {
	unsigned conc_name_p : 1;
	unsigned constructor_p : 1;
	unsigned copier_p : 1;
	unsigned predicate_p : 1;
	unsigned include_p : 1;
	unsigned print_object_p : 1;
	unsigned print_function_p : 1;
	unsigned type_p : 1;
	unsigned named_p : 1;
	unsigned initial_offset_p : 1;
	Execute ptr;
	addr env, doc, slots;
	addr name, conc_name, copier, predicate;
	addr cname, cargs, iname, iargs;
	addr print_object, print_function;
	addr type, initial_offset;
};

static void defstruct_name_clean(struct defstruct_struct *ptr)
{
	clearpoint(ptr);
}


/* defstruct-slots */
static int defstruct_parse_slot(struct defstruct_struct *ptr, addr pos,
		addr *rname, addr *rinit, addr *rtype, addr *rreadonly)
{
	addr list, name, init, type, readonly, key, value, key1, key2;

	/* name */
	readonly = type = Unbound;
	name = init = Nil;
	if (! consp(pos)) {
		name = pos;
		goto finish;
	}

	/* (name) */
	GetCons(pos, &name, &list);
	if (! symbolp(name))
		fmte("DEFSTRUCT slot-name ~S must be a symbol.", name, NULL);
	if (list == Nil)
		goto finish;

	/* (name init) */
	if (! consp(list))
		fmte("Invalid DEFSTRUCT slot-option ~S.", pos, NULL);
	GetCons(list, &init, &list);
	if (list == Nil)
		goto finish;

	/* options */
	GetConst(KEYWORD_TYPE, &key1);
	GetConst(KEYWORD_READ_ONLY, &key2);
	while (list != Nil) {
		if (! consp(list))
			fmte("Invalid DEFSTRUCT slot-option key ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			fmte("Invalid DEFSTRUCT slot-option value ~S.", list, NULL);
		GetCons(list, &value, &list);
		/* :type */
		if (type == Unbound && key == key1) {
			if (parse_type(ptr->ptr, &type, value, ptr->env))
				return 1;
			continue;
		}
		/* :read-only */
		if (readonly == Unbound && key == key2) {
			readonly = (value == Nil)? Nil: T;
			continue;
		}
		/* error */
		fmte("Invalid DEFSTRUCT slot-option ~S.", key, NULL);
	}

finish:
	if (! symbolp(name))
		fmte("DEFSTRUCT slot-name ~S must be a symbol.", name, NULL);
	*rname = name;
	*rinit = init;
	*rtype = type;
	*rreadonly = (readonly == Unbound)? Nil: readonly;
	return 0;
}

static int defstruct_parse_slots_result(struct defstruct_struct *ptr,
		addr list, addr *ret)
{
	addr root, pos, name, init, type, readonly;

	for (root = Nil; list != Nil; ) {
		getcons(list, &pos, &list);
		if (defstruct_parse_slot(ptr, pos, &name, &init, &type, &readonly))
			return 1;
		list_heap(&pos, name, init, type, readonly, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int defstruct_parse_slots(struct defstruct_struct *ptr, addr list)
{
	if (defstruct_parse_slots_result(ptr, list, &list))
		return 1;
	ptr->slots = list;

	return 0;
}


/* defstruct-name */
static int defstruct_parse_name_option1(constindex index, addr option, addr *ret)
{
	addr key, check, tail;

	GetConstant(index, &key);
	if (key == option) {
		/* :keyword */
		*ret = Unbound;
		return 1;
	}
	if (! consp(option)) {
		/* :others */
		return 0;
	}
	GetCons(option, &check, &tail);
	if (key != check) {
		/* (:others ...) */
		return 0;
	}
	if (tail == Nil) {
		/* (:keyword) */
		*ret = Unbound;
		return 1;
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
	*ret = check;
	return 1;

error:
	fmte("Invalid DEFSTRUCT option ~S.", option, NULL);
	return 0;
}

static int defstruct_parse_conc_name(struct defstruct_struct *ptr, addr pos)
{
	if (! defstruct_parse_name_option1(CONSTANT_KEYWORD_CONC_NAME, pos, &pos)) {
		return 0;
	}
	if (! string_designer_p(pos)) {
		fmte("DEFSTRUCT :CONC-NAME ~S must be a string-designer.", pos, NULL);
		return 0;
	}
	if (ptr->conc_name_p) {
		fmte("DEFSTRUCT :CONC-NAME is already exist.", NULL);
		return 0;
	}
	ptr->conc_name_p = 1;
	ptr->conc_name = pos;
	return 1;
}

static int defstruct_parse_copier(struct defstruct_struct *ptr, addr pos)
{
	if (! defstruct_parse_name_option1(CONSTANT_KEYWORD_COPIER, pos, &pos)) {
		return 0;
	}
	if (! symbolp(pos)) {
		fmte("DEFSTRUCT :COPIER ~S must be a symbol.", pos, NULL);
		return 0;
	}
	if (ptr->copier_p) {
		fmte("DEFSTRUCT :COPIER is already exist.", NULL);
		return 0;
	}
	ptr->copier_p = 1;
	ptr->copier = pos;
	return 1;
}

static int defstruct_parse_predicate(struct defstruct_struct *ptr, addr pos)
{
	if (! defstruct_parse_name_option1(CONSTANT_KEYWORD_PREDICATE, pos, &pos)) {
		return 0;
	}
	if (! symbolp(pos)) {
		fmte("DEFSTRUCT :PREDICATE ~S must be a symbol.", pos, NULL);
		return 0;
	}
	if (ptr->predicate_p) {
		fmte("DEFSTRUCT :PREDICATE is already exist.", NULL);
		return 0;
	}
	ptr->predicate_p = 1;
	ptr->predicate = pos;
	return 1;
}

static int defstruct_parse_constructor2(addr option, addr *ret1, addr *ret2)
{
	addr key, pos1, pos2, tail;

	GetConstant(CONSTANT_KEYWORD_CONSTRUCTOR, &key);
	if (key == option) {
		/* :constructor */
		*ret1 = Unbound;
		*ret2 = Unbound;
		return 1;
	}
	if (! consp(option)) {
		/* :others */
		return 0;
	}
	GetCons(option, &pos1, &tail);
	if (key != pos1) {
		/* (:others ...) */
		return 0;
	}
	if (tail == Nil) {
		/* (:constructor) */
		*ret1 = Unbound;
		*ret2 = Unbound;
		return 1;
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
		return 1;
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
	return 1;

error:
	fmte("Invalid DEFSTRUCT option ~S.", option, NULL);
	return 0;
}

static int defstruct_parse_constructor(struct defstruct_struct *ptr, addr pos)
{
	addr args;

	if (! defstruct_parse_constructor2(pos, &pos, &args))
		return 0;
	if (! symbolp(pos)) {
		fmte(":CONSTRUCTOR name ~S must be a symbol.", pos, NULL);
		return 0;
	}
	if (ptr->constructor_p) {
		fmte("DEFSTRUCT :CONSTRUCTOR is already exist.", NULL);
		return 0;
	}
	argument_boa_heap(ptr->ptr->local, &args, args);
	ptr->constructor_p = 1;
	ptr->cname = pos;
	ptr->cargs = args;

	return 1;
}

static int defstruct_parse_include2(addr option, addr *ret1, addr *ret2)
{
	addr key, check, tail;

	if (! consp(option)) {
		/* :others */
		return 0;
	}
	GetConstant(CONSTANT_KEYWORD_INCLUDE, &key);
	GetCons(option, &check, &tail);
	if (key != check) {
		/* (:others ...) */
		return 0;
	}
	if (tail == Nil) {
		/* (:include) */
		goto error;
	}
	if (! consp(tail)) {
		/* (:include . xxx) */
		goto error;
	}
	GetCons(tail, ret1, ret2);
	return 1;

error:
	fmte("DEFSTRUCT :INCLUDE option ~S "
			"must be a (:include name . slots) form.", option, NULL);
	return 0;
}

static int defstruct_parse_include(struct defstruct_struct *ptr, addr pos)
{
	addr args;

	if (! defstruct_parse_include2(pos, &pos, &args))
		return 0;
	if (! symbolp(pos)) {
		fmte(":CONSTRUCTOR name ~S must be a symbol.", pos, NULL);
		return 0;
	}
	if (ptr->include_p) {
		fmte("DEFSTRUCT :INCLUDE is already exist.", NULL);
		return 0;
	}
	ptr->include_p = 1;
	ptr->iname = pos;
	ptr->iargs = args;

	return 1;
}

static int defstruct_parse_print_object1(constindex index, addr option, addr *ret)
{
	addr key, check, tail;

	GetConstant(index, &key);
	if (! consp(option)) {
		/* :others */
		return 0;
	}
	GetCons(option, &check, &tail);
	if (key != check) {
		/* (:others ...) */
		return 0;
	}
	if (tail == Nil) {
		/* (:print-object) */
		*ret = Unbound;
		return 1;
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
	*ret = check;
	return 1;

error:
	fmte("Invalid DEFSTRUCT option ~S.", option, NULL);
	return 0;
}

static int defstruct_parse_print_object(struct defstruct_struct *ptr, addr pos)
{
	if (! defstruct_parse_print_object1(CONSTANT_KEYWORD_PRINT_OBJECT, pos, &pos))
		return 0;
	if (! symbolp(pos)) {
		fmte(":PRINT-OBJECT name ~S must be a symbol.", pos, NULL);
		return 0;
	}
	if (ptr->print_object_p) {
		fmte("DEFSTRUCT :PRINT-OBJECT is already exist.", NULL);
		return 0;
	}
	ptr->print_object_p = 1;
	ptr->print_object = pos;

	return 1;
}

static int defstruct_parse_print_function(struct defstruct_struct *ptr, addr pos)
{
	if (! defstruct_parse_print_object1(CONSTANT_KEYWORD_PRINT_FUNCTION, pos, &pos))
		return 0;
	if (! symbolp(pos)) {
		fmte(":PRINT-FUNCTION name ~S must be a symbol.", pos, NULL);
		return 0;
	}
	if (ptr->print_function_p) {
		fmte("DEFSTRUCT :PRINT-FUNCTION is already exist.", NULL);
		return 0;
	}
	ptr->print_function_p = 1;
	ptr->print_function = pos;

	return 1;
}

static int defstruct_parse_type(struct defstruct_struct *ptr, addr option)
{
	addr key, check, pos;

	/* parse */
	if (! consp(option))
		return 0;
	GetCons(option, &check, &pos);
	GetConst(KEYWORD_TYPE, &key);
	if (key != check)
		return 0;
	if (! consp(pos))
		goto error;
	GetCons(pos, &check, &pos);
	if (pos != Nil)
		goto error;
	if (ptr->type_p) {
		fmte("DEFSTRUCT :TYPE already exists.", NULL);
		return 0;
	}
	ptr->type_p = 1;
	ptr->type = pos;

	return 1;

error:
	fmte("DEFSTRUCT :TYPE must be a (:type type) form.", option, NULL);
	return 0;
}

static int defstruct_parse_named(struct defstruct_struct *ptr, addr option)
{
	addr key;

	GetConst(KEYWORD_NAMED, &key);
	if (option != key)
		return 0;
	if (ptr->named_p) {
		fmte("DEFSTRUCT :named already exists.", NULL);
		return 0;
	}
	ptr->named_p = 1;

	return 1;
}

static int defstruct_parse_initial_offset(struct defstruct_struct *ptr, addr option)
{
	addr key, check, pos;

	/* parse */
	if (! consp(option))
		return 0;
	GetCons(option, &check, &pos);
	GetConst(KEYWORD_INITIAL_OFFSET, &key);
	if (key != check)
		return 0;
	if (! consp(pos))
		goto error;
	GetCons(pos, &check, &pos);
	if (pos != Nil)
		goto error;
	if (ptr->initial_offset_p) {
		fmte("DEFSTRUCT :INITIAL-OFFSET already exists.", NULL);
		return 0;
	}
	ptr->initial_offset_p = 1;
	ptr->initial_offset = pos;

	return 1;

error:
	fmte("DEFSTRUCT :INITIAL_OFFSET must be a "
			"(:initial_offset offset) form.", option, NULL);
	return 0;
}

static int defstruct_parse_name(struct defstruct_struct *ptr, addr name)
{
	addr list, pos;

	if (symbolp(name)) {
		ptr->name = name;
		return 0;
	}
	if (! consp(name)) {
		fmte("DEFSTRUCT name ~S must be symbol or list.", name, NULL);
		return 0;
	}
	GetCons(name, &name, &list);
	if (! symbolp(name)) {
		fmte("DEFSTRUCT name ~S must be a symbol.", name, NULL);
		return 0;
	}
	/* loop */
	while (list != Nil) {
		if (! consp(list)) {
			fmte("DEFSTRUCT name option ~S must be a list.", list, NULL);
			return 0;
		}
		GetCons(list, &pos, &list);
		if (defstruct_parse_conc_name(ptr, pos))
			continue;
		if (defstruct_parse_constructor(ptr, pos))
			continue;
		if (defstruct_parse_copier(ptr, pos))
			continue;
		if (defstruct_parse_predicate(ptr, pos))
			continue;
		if (defstruct_parse_include(ptr, pos))
			continue;
		if (defstruct_parse_print_object(ptr, pos))
			continue;
		if (defstruct_parse_print_function(ptr, pos))
			continue;
		if (defstruct_parse_type(ptr, pos))
			continue;
		if (defstruct_parse_named(ptr, pos))
			continue;
		if (defstruct_parse_initial_offset(ptr, pos))
			continue;
		fmte("Invalid DEFSTRUCT option ~S.", pos, NULL);
	}

	/* parse slots */
	if (ptr->include_p) {
		if (defstruct_parse_slots_result(ptr, ptr->iargs, &(ptr->iargs)))
			return 1;
	}

	/* parse-type */
	if (ptr->type_p) {
		if (parse_type(ptr->ptr, &(ptr->type), ptr->type, ptr->env))
			return 1;
	}

	return 0;
}

static void defstruct_parse_document(struct defstruct_struct *ptr, addr pos, addr *ret)
{
	addr a, b;

	if (! consp(pos)) {
		ptr->doc = NULL;
		*ret = pos;
		return;
	}
	GetCons(pos, &a, &b);
	if (! stringp(a)) {
		ptr->doc = NULL;
		*ret = pos;
		return;
	}
	ptr->doc = a;
	*ret = b;
}

static int defstruct_parse(struct defstruct_struct *ptr, addr form)
{
	addr args, name;

	getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (defstruct_parse_name(ptr, name))
		return 1;
	defstruct_parse_document(ptr, args, &args);
	return defstruct_parse_slots(ptr, args);

error:
	fmte("DEFSTRUCT form ~S must be a (defstruct name [doc] {slot}*", form, NULL);
	return 0;
}

static void defstruct_make(struct defstruct_struct *ptr, addr *ret)
{
	/* `(ensure-structure 
	 *    ',name
	 *    ',slots
	 *    :documentation ,doc
	 *    :conc-name ',conc-name
	 */
	addr root, pos;

	/* ensure-structure */
	root = Nil;
	GetConst(SYSTEM_ENSURE_STRUCTURE, &pos);
	cons_heap(&root, pos, root);
	/* name, slots */
	quotelist_heap(&pos, ptr->name);
	cons_heap(&root, pos, root);
	quotelist_heap(&pos, ptr->slots);
	cons_heap(&root, pos, root);
	/* :documentation */
	if (ptr->doc) {
		GetConst(KEYWORD_DOCUMENTATION, &pos);
		cons_heap(&root, pos, root);
		cons_heap(&root, ptr->doc, root);
	}
	/* result */
	nreverse_list_unsafe(ret, root);
}

static void function_defstruct(Execute ptr, addr form, addr env)
{
	struct defstruct_struct str;

	defstruct_name_clean(&str);
	str.ptr = ptr;
	str.env = env;
	if (defstruct_parse(&str, form))
		return;
	defstruct_make(&str, &form);
	setresult_control(ptr, form);
}

static void defmacro_defstruct(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFSTRUCT, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_defstruct);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* 
 *  (defun copy-structure (structure) ...) -> structure
 */
static void function_copy_structure(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
}

static void type_copy_structure(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_structure(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_STRUCTURE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_copy_structure);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_structure(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  intern
 */
void intern_common_structures(void)
{
	defmacro_defstruct();
	defun_copy_structure();
}

