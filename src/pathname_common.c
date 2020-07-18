#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "files.h"
#include "function.h"
#include "hashtable.h"
#include "input.h"
#include "integer.h"
#include "pathname_common.h"
#include "pathname_object.h"
#include "pathname_translate.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  common-lisp
 */
static void make_pathname_directory(addr *ret, addr list)
{
	int check;
	addr root, pos, absolute, relative, wild, wildi, up;

	/* :directory "Hello" */
	GetConst(KEYWORD_ABSOLUTE, &absolute);
	if (stringp(list)) {
		list_heap(ret, absolute, list, NULL);
		return;
	}

	/* check */
	if (! consp(list))
		fmte(":directory ~S must be a list or string type.", list, NULL);
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);
	GetConst(KEYWORD_UP, &up);
	getcons(list, &pos, &root);
	if (pos != absolute && pos != relative) {
		fmte("The firest argument of :directory ~S must be ~S or ~S.",
				pos, absolute, relative, NULL);
	}
	for (check = 1; root != Nil; ) {
		getcons(root, &pos, &root);
		if (stringp(pos)
				&& stringp_equal_char(pos, "*")
				&& stringp_equal_char(pos, "**")
				&& stringp_equal_char(pos, "..")) {
			check = 0;
		}
		if (! stringp(pos) && pos != wild && pos != wildi && pos != up) {
			fmte("Invalid :directory argument ~S.", pos, NULL);
		}
	}
	if (check) {
		*ret = list;
		return;
	}

	/* rebuild */
	GetCons(list, &pos, &list);
	conscar_heap(&root, pos);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (stringp_equal_char(pos, "*"))
			pos = wild;
		else if (stringp_equal_char(pos, "**"))
			pos = wildi;
		else if (stringp_equal_char(pos, ".."))
			pos = up;
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

enum PathnameType {
	PathnameType_Unix,
	PathnameType_Windows,
	PathnameType_Logical
};
static enum PathnameType pathname_environment(addr host)
{
	addr check;

	/* unix */
	GetConst(SYSTEM_UNIX, &check);
	if (host == check)
		return PathnameType_Unix;

	/* windows */
	GetConst(SYSTEM_WINDOWS, &check);
	if (host == check)
		return PathnameType_Windows;

	/* logical */
	if (stringp(host))
		return PathnameType_Logical;

	/* error */
	fmte("Invalid host value ~S.", host, NULL);

	return PathnameType_Unix;
}

static int make_pathname_upper_p(addr pos)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (! isUpperCase(c))
			return 0;
	}

	return 1;
}

static int make_pathname_lower_p(addr pos)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (! isUpperCase(c))
			return 0;
	}

	return 1;
}

static void make_pathname_upper(addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	strvect_heap(&one, size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		strvect_setc(one, toUpperUnicode(c), c);
	}
	*ret = one;
}

static void make_pathname_lower(addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	strvect_heap(&one, size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		strvect_setc(one, toUpperUnicode(c), c);
	}
	*ret = one;
}

static void make_pathname_string(addr *ret)
{
	addr pos;

	pos = *ret;
	if (stringp(pos)) {
		if (make_pathname_upper_p(pos))
			make_pathname_lower(ret, pos);
		else if (make_pathname_lower_p(pos))
			make_pathname_upper(ret, pos);
	}
}

static int make_pathname_check(addr pos)
{
	return stringp(pos) && (make_pathname_upper_p(pos) || make_pathname_lower_p(pos));
}

static int make_pathname_list_p(addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (make_pathname_check(pos))
			return 1;
	}

	return 0;
}

static void make_pathname_list(addr *ret, addr list)
{
	addr root, pos;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (make_pathname_check(pos))
			make_pathname_string(&pos);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

static void make_pathname_case(addr *directory, addr *name, addr *type)
{
	/* directory */
	if (make_pathname_list_p(*directory))
		make_pathname_list(directory, *directory);
	/* name, type */
	make_pathname_string(name);
	make_pathname_string(type);
}

_g void make_pathname(Execute ptr, addr *ret, addr rest)
{

	/* (defun make-pathname (&key host device directory name type version defaults case)
	 *     ...) -> pathname
	 *   host       (or string symbol)
	 *   device     (or string symbol)  ;; (eql :unspecific))
	 *   directory  (or cons (member :wild :unspecific))
	 *   name       (or string cons (member nil :wild))
	 *   type       (or string (member nil :wild :unspecific)))
	 *   version    (or (integer 1 *) (member nil :wild :unspecific :newest))
	 *   defaults   (or pathname null)  ;; default *default-pathname-defaults*
	 *   case       (member :common :local)
	 */
	enum PathnameType ptype;
	addr host, device, directory, name, type, version, defaults, keycase, check;

	if (GetKeyArgs(rest, KEYWORD_HOST, &host)) host = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DEVICE, &device)) device = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DIRECTORY, &directory)) directory = Unbound;
	if (GetKeyArgs(rest, KEYWORD_NAME, &name)) name = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TYPE, &type)) type = Unbound;
	if (GetKeyArgs(rest, KEYWORD_VERSION, &version)) version = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DEFAULTS, &defaults)) defaults = Unbound;
	if (GetKeyArgs(rest, KEYWORD_CASE, &keycase)) keycase = Unbound;

	/* *default-pathname-defaults* */
	defaults_pathname_heap(ptr, &defaults, defaults);

	/* default arguments */
	if (host == Unbound)
		GetHostPathname(defaults, &host);
	if (device == Unbound)
		GetDevicePathname(defaults, &device);
	if (directory == Unbound)
		GetDirectoryPathname(defaults, &directory);
	if (name == Unbound)
		GetNamePathname(defaults, &name);
	if (type == Unbound)
		GetTypePathname(defaults, &type);
	if (version == Unbound)
		GetVersionPathname(defaults, &version);

	/* format */
	make_pathname_directory(&directory, directory);

	/* case */
	ptype = pathname_environment(host);
	GetConst(KEYWORD_COMMON, &check);
	if (keycase == check &&
			(ptype == PathnameType_Unix || ptype == PathnameType_Logical)) {
		make_pathname_case(&directory, &name, &type);
	}

	/* pathname */
	if (ptype == PathnameType_Logical) {
		GetConst(KEYWORD_UNSPECIFIC, &check);
		if (version == check)
			GetConst(KEYWORD_NEWEST, &version);
		logical_pathname_heap(ret, host, directory, name, type, version);
	}
	else {
		pathname_heap(ret, host, device, directory, name, type);
	}
}


/*
 *  pathname accessor
 */
_g void pathname_host(addr pos, addr *ret, int localp)
{
	GetHostPathname(pos, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_device(addr pos, addr *ret, int localp)
{
	GetDevicePathname(pos, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_directory(addr pos, addr *ret, int localp)
{
	GetDirectoryPathname(pos, &pos);
	if (! localp) {
		if (make_pathname_list_p(pos))
			make_pathname_list(&pos, pos);
	}
	*ret = pos;
}

_g void pathname_name(addr pos, addr *ret, int localp)
{
	GetNamePathname(pos, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_type(addr pos, addr *ret, int localp)
{
	GetTypePathname(pos, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_version(addr pos, addr *ret)
{
	GetVersionPathname(pos, ret);
}


/*
 *  logical-pathname-translations
 */
_g void get_logical_pathname_translations(addr host, addr *ret)
{
	gethost_logical_pathname(host, ret);
}

static void list_logical_pathname_translations(Execute ptr,
		addr *ret, addr host, addr list)
{
	addr show, value, root, left, right, check;

	/* parse-list */
	show = list;
	root = Nil;
	while (list != Nil) {
		getcons(list, &right, &list);
		getcons(right, &left, &right);
		getcons(right, &right, &value);
		if (value != Nil)
			fmte("Invalid logical-pathname-translations arguments ~S.", show, NULL);

		/* left */
		if (stringp(left))
			parse_pathname_host_heap(ptr, left, host, &value);
		pathname_designer_heap(ptr, value, &value);
		if (! pathname_logical_p(value))
			fmte("The left argument ~S must be a logical-pathname.", left, NULL);
		GetHostPathname(value, &check);
		if (! string_equalp(host, check))
			fmte("The logical-pathname :HOST ~S must be ~S.", check, host, NULL);
		left = value;

		/* right */
		pathname_designer_heap(ptr, right, &value);
		if (! pathname_pathname_p(value))
			fmte("The right argument ~S must be a no-logical-pathname.", right, NULL);
		right = value;

		/* result */
		list_heap(&right, left, right, NULL);
		cons_heap(&root, right, root);
	}
	nreverse(ret, root);
}

static int function_set_logical_pathname_translations(Execute ptr, addr condition)
{
	addr table, pos;

	/* delete */
	table_logical_pathname(&table);
	getdata_control(ptr, &pos);
	delete_hashtable(table, pos);
	/* throw */
	error_function(condition);

	return 0;
}

static void set_logical_pathname_translations_code(Execute ptr,
		addr host, addr list, addr cons)
{
	list_logical_pathname_translations(ptr, &list, host, list);
	SetCdr(cons, list);
}

static int set_logical_pathname_translations_intern(Execute ptr,
		addr host, addr list, addr table)
{
	addr control, symbol, call, cons;

	/* push */
	push_new_control(ptr, &control);
	/* handler-case */
	GetConst(COMMON_ERROR, &symbol);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_set_logical_pathname_translations);
	pushhandler_common(ptr, symbol, call, 0);
	/* code */
	intern_hashheap(table, host, &cons);
	SetDataFunction(call, host);
	set_logical_pathname_translations_code(ptr, host, list, cons);
	/* free */
	return free_control_(ptr, control);
}

_g int set_logical_pathname_translations(Execute ptr, addr host, addr list)
{
	addr table, cons;

	table_logical_pathname(&table);
	if (findvalue_hashtable(table, host, &cons)) {
		set_logical_pathname_translations_code(ptr, host, list, cons);
	}
	else {
		Return(set_logical_pathname_translations_intern(ptr, host, list, table));
	}

	return 0;
}


/*
 *  logical-pathname
 */
_g void logical_pathname(Execute ptr, addr *ret, addr pos)
{
	addr value, type;

	pathname_designer_heap(ptr, pos, &value);
	if (! pathname_logical_p(value)) {
		GetConst(COMMON_LOGICAL_PATHNAME, &type);
		type_error_stdarg(value, type,
				"The pathname ~S is not logical-pathname.", pos, NULL);
	}
	*ret = value;
}


/*
 *  namestring
 */
_g int namestring_pathname_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		Return(name_pathname_heap_(ptr, pos, ret));
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		Return(name_pathname_heap_(ptr, pos, ret));
	}

	return 0;
}

_g int file_namestring_pathname_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		Return(file_name_pathname_heap_(local, pos, ret));
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		Return(file_name_pathname_heap_(local, pos, ret));
	}

	return 0;
}

_g void directory_namestring_pathname(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		directory_name_pathname_heap(local, pos, ret);
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		directory_name_pathname_heap(local, pos, ret);
	}
}

_g void host_namestring_pathname(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		GetHostPathname(pos, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			copylocal_object(NULL, ret, pos);
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		GetHostPathname(pos, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			*ret = pos;
	}
}

static void enough_copy_pathname(addr pos, addr cdr, addr *ret)
{
	addr one, value;

	make_pathname_alloc(NULL, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_DEVICE, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_NAME, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_TYPE, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_VERSION, one);
	/* directory */
	GetDirectoryPathname(pos, &pos);
	GetConst(KEYWORD_RELATIVE, &value);
	copylocal_object(NULL, &cdr, cdr);
	cons_heap(&value, value, cdr);
	SetDirectoryPathname(one, value);
	*ret = one;
}

static int enough_directory_pathname(LocalRoot local,
		addr pos, addr a, addr b, addr *ret)
{
	addr car1, car2, cdr1, cdr2, next;

	/* relative, absolute */
	getcons(a, &car1, &cdr1);
	getcons(b, &car2, &cdr2);
	if (car1 != car2)
		return 0;

	/* directory */
	while (cdr2 != Nil) {
		if (cdr1 == Nil)
			return 0;
		getcons(cdr1, &car1, &next);
		getcons(cdr2, &car2, &cdr2);
		if (! LispPathnameEqual(car1, car2))
			return 0;
		cdr1 = next;
	}

	/* result */
	enough_copy_pathname(pos, cdr1, ret);

	return 1;
}

static int enough_merge_pathname(LocalRoot local, addr a, addr b, addr *ret)
{
	int check1, check2;
	addr pos1, pos2;

	check1 = RefLogicalPathname(a);
	check2 = RefLogicalPathname(b);
	if (check1 != check2)
		return 0;
	/* host */
	GetHostPathname(a, &pos1);
	GetHostPathname(b, &pos2);
	if (! equalp_function(pos1, pos2))
		return 0;
	/* device */
	GetDevicePathname(a, &pos1);
	GetDevicePathname(b, &pos2);
	if (! LispPathnameEqual(pos1, pos2))
		return 0;
	/* directory */
	GetDirectoryPathname(a, &pos1);
	GetDirectoryPathname(b, &pos2);
	return enough_directory_pathname(local, a, pos1, pos2, ret);
}

_g int enough_namestring_pathname_(Execute ptr, addr *ret, addr pos, addr defaults)
{
	addr value;

	defaults_pathname_heap(ptr, &defaults, defaults);
	pathname_designer_heap(ptr, pos, &pos);
	if (enough_merge_pathname(ptr->local, pos, defaults, &value))
		pos = value;
	return name_pathname_heap_(ptr, pos, ret);
}


/*
 *  parse-namestring
 */
_g void parse_namestring(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk)
{
	addr check, type;
	size_t index1, index2;

	/* defaults */
	defaults_pathname_heap(ptr, &defaults, defaults);
	/* stream */
	if (streamp(thing))
		GetPathnameStream(thing, &thing);
	/* pathname */
	if (pathnamep(thing)) {
		GetHostPathname(thing, &check);
		if (host != Nil && ! equalp_function(host, check)) {
			GetConst(COMMON_PATHNAME, &type);
			type_error_stdarg(thing, type,
					":HOST ~S does not match a pathname host ~S.",
					host, check, NULL);
		}
		*ret = thing;
		*position = start;
		return;
	}
	/* string */
	if (stringp(thing)) {
		if (GetIndex_integer(start, &index1))
			fmte("Invalid :start value ~S.", start, NULL);
		if (end == Nil)
			string_length(thing, &index2);
		else if (GetIndex_integer(end, &index2))
			fmte("Invalid :start value ~S.", end, NULL);
		parse_pathname_full_heap(ptr, thing, host, defaults,
				index1, index2, junk != Nil, &thing, &index1);
		make_index_integer_alloc(NULL, position, index1);
		/* host check */
		if (host != Nil) {
			GetHostPathname(thing, &check);
			if (! equalp_function(host, check))
				fmte(":HOST ~S is not argument host ~S.", check, host, NULL);
		}
		/* result */
		*ret = thing;
		return;
	}

	GetConst(COMMON_PATHNAME, &type);
	type_error_stdarg(thing, type, "~S is not pathname-designer.", thing, NULL);
}


/*
 *  pathname-wild-p
 */
_g void wild_pathname_p(Execute ptr, addr *ret, addr file, addr field)
{
	int check;

	pathname_designer_heap(ptr, file, &file);
	if (field == Unbound) field = Nil;
	check = wild_pathname_boolean(file, field);
	*ret = check? T: Nil;
}


/*
 *  pathname-match-p
 */
_g void pathname_match_p(Execute ptr, addr *ret, addr a, addr b)
{
	int check;

	pathname_designer_heap(ptr, a, &a);
	pathname_designer_heap(ptr, b, &b);
	check = wildcard_pathname(a, b, 1);
	*ret = check? T: Nil;
}


/*
 *  translate-pathname
 */
_g void translate_pathname(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	translate_pathname_heap(ptr, ret, pos, from, to);
}


/*
 *  translate-logical-pathname
 */
_g void translate_logical_pathname(Execute ptr, addr *ret, addr pos)
{
	physical_pathname_heap(ptr, pos, ret);
}


/*
 *  merge-pathnames
 */
_g void merge_pathnames(Execute ptr, addr *ret, addr pos, addr defaults, addr version)
{
	merge_pathnames_clang(ptr, pos, defaults, version, ret);
}


/*
 *  load-logical-pathname-translations
 */
_g int load_logical_pathname_translations_common(Execute ptr, addr host, int *ret)
{
	addr defaults, file, x;

	/* pathname */
	GetConst(SYSTEM_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &defaults);
	getspecial_local(ptr, defaults, &defaults);
	defaults_pathname_heap(ptr, &defaults, defaults);
	pathname_heap(&file, Nil, Nil, Nil, host, Nil);
	merge_pathnames_clang(ptr, host, defaults, Unbound, &file);
	/* load */
	Return(probe_file_files_(ptr, &x, file));
	/* host file is not exist */
	if (x != T)
		return Result(ret, 0);
	/* read file */
	Return(readlist_input(ptr, file, &x));
	Return(set_logical_pathname_translations(ptr, host, x));
	return Result(ret, 1);
}


/*
 *  initialize
 */
_g void init_pathname_common(void)
{
	SetPointerCall(defun, var1, set_logical_pathname_translations);
}

