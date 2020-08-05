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
static int directory_keyword_p_(addr pos, int *ret)
{
	if (! stringp(pos))
		return Result(ret, 0);

	Return(string_equal_char_(pos, "*", ret));
	if (*ret)
		return 0;

	Return(string_equal_char_(pos, "**", ret));
	if (*ret)
		return 0;

	return string_equal_char_(pos, "..", ret);
}

static int make_pathname_directory_(addr *ret, addr list)
{
	int keywordp, check;
	addr root, pos, absolute, relative, wild, wildi, up;

	/* :directory "Hello" */
	GetConst(KEYWORD_ABSOLUTE, &absolute);
	if (stringp(list)) {
		list_heap(ret, absolute, list, NULL);
		return 0;
	}

	/* check */
	if (! consp(list)) {
		*ret = Nil;
		return fmte_(":directory ~S must be a list or string type.", list, NULL);
	}
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);
	GetConst(KEYWORD_UP, &up);
	Return_getcons(list, &pos, &root);
	if (pos != absolute && pos != relative) {
		*ret = Nil;
		return fmte_("The firest argument of :directory ~S must be ~S or ~S.",
				pos, absolute, relative, NULL);
	}
	for (keywordp = 1; root != Nil; ) {
		Return_getcons(root, &pos, &root);
		Return(directory_keyword_p_(pos, &check));
		if (check)
			keywordp = 0;
		if (! stringp(pos) && pos != wild && pos != wildi && pos != up) {
			*ret = Nil;
			return fmte_("Invalid :directory argument ~S.", pos, NULL);
		}
	}
	if (keywordp)
		return Result(ret, list);

	/* rebuild */
	GetCons(list, &pos, &list);
	conscar_heap(&root, pos);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		/* "*" */
		Return(stringp_equal_char_(pos, "*", &check));
		if (check) {
			cons_heap(&root, wild, root);
			continue;
		}
		/* "**" */
		Return(stringp_equal_char_(pos, "**", &check));
		if (check) {
			cons_heap(&root, wildi, root);
			continue;
		}
		/* ".." */
		Return(stringp_equal_char_(pos, "..", &check));
		if (check) {
			cons_heap(&root, up, root);
			continue;
		}
		/* else */
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

enum PathnameType {
	PathnameType_Unix,
	PathnameType_Windows,
	PathnameType_Logical
};
static int pathname_environment_(addr host, enum PathnameType *ret)
{
	addr check;

	/* unix */
	GetConst(SYSTEM_UNIX, &check);
	if (host == check)
		return Result(ret, PathnameType_Unix);

	/* windows */
	GetConst(SYSTEM_WINDOWS, &check);
	if (host == check)
		return Result(ret, PathnameType_Windows);

	/* logical */
	if (stringp(host))
		return Result(ret, PathnameType_Logical);

	/* error */
	*ret = PathnameType_Unix;
	return fmte_("Invalid host value ~S.", host, NULL);
}

static int make_pathname_upper_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (! isUpperCase(c))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int make_pathname_lower_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (! isUpperCase(c))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int make_pathname_upper_(addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	strvect_heap(&one, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(one, toUpperUnicode(c), c));
	}

	return Result(ret, one);
}

static int make_pathname_lower_(addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	strvect_heap(&one, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(one, toUpperUnicode(c), c));
	}

	return Result(ret, one);
}

static int make_pathname_string_(addr *ret)
{
	int check;
	addr pos;

	pos = *ret;
	if (! stringp(pos))
		return Result(ret, pos);

	Return(make_pathname_upper_p_(pos, &check));
	if (check)
		return make_pathname_lower_(ret, pos);

	Return(make_pathname_lower_p_(pos, &check));
	if (check)
		return make_pathname_upper_(ret, pos);

	return  Result(ret, pos);
}

static int make_pathname_check_(addr pos, int *ret)
{
	int check;

	if (! stringp(pos))
		return Result(ret, 0);
	Return(make_pathname_upper_p_(pos, &check));
	if (check)
		return Result(ret, 1);

	return make_pathname_lower_p_(pos, ret);
}

static int make_pathname_list_p_(addr list, int *ret)
{
	int check;
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(make_pathname_check_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int make_pathname_list_(addr *ret, addr list)
{
	int check;
	addr root, pos;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(make_pathname_check_(pos, &check));
		if (check) {
			Return(make_pathname_string_(&pos));
		}
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int make_pathname_case_(addr *directory, addr *name, addr *type)
{
	int check;

	/* directory */
	Return(make_pathname_list_p_(*directory, &check));
	if (check) {
		Return(make_pathname_list_(directory, *directory));
	}
	/* name, type */
	Return(make_pathname_string_(name));
	Return(make_pathname_string_(type));

	return 0;
}

_g int make_pathname_(Execute ptr, addr *ret, addr rest)
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

	if (GetKeyArgs(rest, KEYWORD_HOST, &host))
		host = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DEVICE, &device))
		device = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DIRECTORY, &directory))
		directory = Unbound;
	if (GetKeyArgs(rest, KEYWORD_NAME, &name))
		name = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TYPE, &type))
		type = Unbound;
	if (GetKeyArgs(rest, KEYWORD_VERSION, &version))
		version = Unbound;
	if (GetKeyArgs(rest, KEYWORD_DEFAULTS, &defaults))
		defaults = Unbound;
	if (GetKeyArgs(rest, KEYWORD_CASE, &keycase))
		keycase = Unbound;

	/* *default-pathname-defaults* */
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));

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
	Return(make_pathname_directory_(&directory, directory));

	/* case */
	Return(pathname_environment_(host, &ptype));
	GetConst(KEYWORD_COMMON, &check);
	if (keycase == check &&
			(ptype == PathnameType_Unix || ptype == PathnameType_Logical)) {
		Return(make_pathname_case_(&directory, &name, &type));
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

	return 0;
}


/*
 *  pathname accessor
 */
_g int pathname_host_(addr pos, addr *ret, int localp)
{
	GetHostPathname(pos, &pos);
	if (! localp) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

_g int pathname_device_(addr pos, addr *ret, int localp)
{
	GetDevicePathname(pos, &pos);
	if (! localp) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

_g int pathname_directory_(addr pos, addr *ret, int localp)
{
	int check;

	GetDirectoryPathname(pos, &pos);
	if (! localp) {
		Return(make_pathname_list_p_(pos, &check));
		if (check) {
			Return(make_pathname_list_(&pos, pos));
		}
	}

	return Result(ret, pos);
}

_g int pathname_name_(addr pos, addr *ret, int localp)
{
	GetNamePathname(pos, &pos);
	if (! localp) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

_g int pathname_type_(addr pos, addr *ret, int localp)
{
	GetTypePathname(pos, &pos);
	if (! localp) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

_g void pathname_version(addr pos, addr *ret)
{
	GetVersionPathname(pos, ret);
}


/*
 *  logical-pathname-translations
 */
_g int get_logical_pathname_translations_(addr host, addr *ret)
{
	return gethost_logical_pathname_(host, ret);
}

static int list_logical_pathname_translations_(Execute ptr,
		addr *ret, addr host, addr list)
{
	int check;
	addr show, value, root, left, right, temp;

	/* parse-list */
	show = list;
	root = Nil;
	while (list != Nil) {
		Return_getcons(list, &right, &list);
		Return_getcons(right, &left, &right);
		Return_getcons(right, &right, &value);
		if (value != Nil) {
			return fmte_("Invalid logical-pathname-translations "
					"arguments ~S.", show, NULL);
		}

		/* left */
		if (stringp(left)) {
			Return(parse_pathname_host_heap_(ptr, left, host, &value));
		}
		Return(pathname_designer_heap_(ptr, value, &value));
		if (! pathname_logical_p(value)) {
			return fmte_("The left argument ~S "
					"must be a logical-pathname.", left, NULL);
		}
		GetHostPathname(value, &temp);
		Return(string_equalp_(host, temp, &check));
		if (! check) {
			return fmte_("The logical-pathname :HOST ~S "
					"must be ~S.", temp, host, NULL);
		}
		left = value;

		/* right */
		Return(pathname_designer_heap_(ptr, right, &value));
		if (! pathname_pathname_p(value)) {
			return fmte_("The right argument ~S "
					"must be a no-logical-pathname.", right, NULL);
		}
		right = value;

		/* result */
		list_heap(&right, left, right, NULL);
		cons_heap(&root, right, root);
	}
	nreverse(ret, root);

	return 0;
}

static int function_set_logical_pathname_translations(Execute ptr, addr condition)
{
	int check;
	addr table, pos;

	/* delete */
	table_logical_pathname(&table);
	getdata_control(ptr, &pos);
	Return(delete_hashtable_(table, pos, &check));
	/* throw */
	return error_function_(ptr, condition);
}

static int set_logical_pathname_translations_code_(Execute ptr,
		addr host, addr list, addr cons)
{
	Return(list_logical_pathname_translations_(ptr, &list, host, list));
	SetCdr(cons, list);
	return 0;
}

static int set_logical_pathname_translations_intern_(Execute ptr,
		addr host, addr list, addr table)
{
	addr control, symbol, call, cons;

	/* push */
	push_new_control(ptr, &control);
	/* handler-case */
	GetConst(COMMON_ERROR, &symbol);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_set_logical_pathname_translations);
	Return(pushhandler_common_(ptr, symbol, call, 0));
	/* code */
	Return(intern_hashheap_(table, host, &cons));
	SetDataFunction(call, host);
	Return(set_logical_pathname_translations_code_(ptr, host, list, cons));
	/* free */
	return free_control_(ptr, control);
}

_g int set_logical_pathname_translations_(Execute ptr, addr host, addr list)
{
	addr table, cons;

	table_logical_pathname(&table);
	Return(find_hashtable_(table, host, &cons));
	if (cons != Unbound) {
		Return(set_logical_pathname_translations_code_(ptr, host, list, cons));
	}
	else {
		Return(set_logical_pathname_translations_intern_(ptr, host, list, table));
	}

	return 0;
}


/*
 *  logical-pathname
 */
_g int logical_pathname_(Execute ptr, addr *ret, addr pos)
{
	addr value, type;

	Return(pathname_designer_heap_(ptr, pos, &value));
	if (! pathname_logical_p(value)) {
		GetConst(COMMON_LOGICAL_PATHNAME, &type);
		return call_type_error_va_(ptr, value, type,
				"The pathname ~S is not logical-pathname.", pos, NULL);
	}

	return Result(ret, value);
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
		Return(pathname_designer_local_(ptr, pos, &pos));
		Return(name_pathname_heap_(ptr, pos, ret));
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designer_heap_(ptr, pos, &pos));
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
		Return(pathname_designer_local_(ptr, pos, &pos));
		Return(file_name_pathname_heap_(local, pos, ret));
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designer_heap_(ptr, pos, &pos));
		Return(file_name_pathname_heap_(local, pos, ret));
	}

	return 0;
}

_g int directory_namestring_pathname_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		Return(pathname_designer_local_(ptr, pos, &pos));
		Return(directory_name_pathname_heap_(local, pos, ret));
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designer_heap_(ptr, pos, &pos));
		Return(directory_name_pathname_heap_(local, pos, ret));
	}

	return 0;
}

_g int host_namestring_pathname_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		Return(pathname_designer_local_(ptr, pos, &pos));
		GetHostPathname(pos, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			copylocal_object(NULL, ret, pos);
		rollback_local(local, stack);
	}
	else {
		Return(pathname_designer_heap_(ptr, pos, &pos));
		GetHostPathname(pos, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			*ret = pos;
	}

	return 0;
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

static int enough_directory_pathname_(LocalRoot local,
		addr pos, addr a, addr b, addr *value, int *ret)
{
	int check;
	addr car1, car2, cdr1, cdr2, next;

	/* relative, absolute */
	Return_getcons(a, &car1, &cdr1);
	Return_getcons(b, &car2, &cdr2);
	if (car1 != car2)
		return Result(ret, 0);

	/* directory */
	while (cdr2 != Nil) {
		if (cdr1 == Nil)
			return Result(ret, 0);
		Return_getcons(cdr1, &car1, &next);
		Return_getcons(cdr2, &car2, &cdr2);
		Return(LispPathnameEqual_(car1, car2, &check));
		if (! check)
			return Result(ret, 0);
		cdr1 = next;
	}

	/* result */
	enough_copy_pathname(pos, cdr1, value);
	return Result(ret, 1);
}

static int enough_merge_pathname_(LocalRoot local,
		addr a, addr b, addr *value, int *ret)
{
	int check, check1, check2;
	addr pos1, pos2;

	check1 = RefLogicalPathname(a);
	check2 = RefLogicalPathname(b);
	if (check1 != check2)
		return Result(ret, 0);
	/* host */
	GetHostPathname(a, &pos1);
	GetHostPathname(b, &pos2);
	Return(equalp_function_(pos1, pos2, &check));
	if (! check)
		return Result(ret, 0);
	/* device */
	GetDevicePathname(a, &pos1);
	GetDevicePathname(b, &pos2);
	Return(LispPathnameEqual_(pos1, pos2, &check));
	if (! check)
		return Result(ret, 0);
	/* directory */
	GetDirectoryPathname(a, &pos1);
	GetDirectoryPathname(b, &pos2);
	return enough_directory_pathname_(local, a, pos1, pos2, value, ret);
}

_g int enough_namestring_pathname_(Execute ptr, addr *ret, addr pos, addr defaults)
{
	int check;
	addr value;

	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(enough_merge_pathname_(ptr->local, pos, defaults, &value, &check));
	if (check)
		pos = value;
	return name_pathname_heap_(ptr, pos, ret);
}


/*
 *  parse-namestring
 */
_g int parse_namestring_(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk)
{
	int check;
	addr value, type;
	size_t index1, index2;

	/* defaults */
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	/* stream */
	if (streamp(thing))
		GetPathnameStream(thing, &thing);
	/* pathname */
	if (pathnamep(thing)) {
		GetHostPathname(thing, &value);
		if (host != Nil) {
			Return(equalp_function_(host, value, &check));
			if (! check) {
				GetConst(COMMON_PATHNAME, &type);
				return call_type_error_va_(ptr, thing, type,
						":HOST ~S does not match a pathname host ~S.",
						host, value, NULL);
			}
		}
		*position = start;
		return Result(ret, thing);
	}
	/* string */
	if (stringp(thing)) {
		if (GetIndex_integer(start, &index1))
			return fmte_("Invalid :start value ~S.", start, NULL);
		if (end == Nil)
			string_length(thing, &index2);
		else if (GetIndex_integer(end, &index2))
			return fmte_("Invalid :start value ~S.", end, NULL);
		Return(parse_pathname_full_heap_(ptr, thing, host, defaults,
					index1, index2, junk != Nil, &thing, &index1));
		make_index_integer_alloc(NULL, position, index1);
		/* host check */
		if (host != Nil) {
			GetHostPathname(thing, &value);
			Return(equalp_function_(host, value, &check));
			if (! check)
				return fmte_(":HOST ~S is not argument host ~S.", value, host, NULL);
		}
		/* result */
		return Result(ret, thing);
	}

	GetConst(COMMON_PATHNAME, &type);
	return call_type_error_va_(ptr, thing, type,
			"~S is not pathname-designer.", thing, NULL);
}


/*
 *  pathname-wild-p
 */
_g int wild_pathname_p_(Execute ptr, addr *ret, addr file, addr field)
{
	int check;

	Return(pathname_designer_heap_(ptr, file, &file));
	if (field == Unbound)
		field = Nil;
	Return(wild_pathname_boolean_(file, field, &check));
	return Result(ret, check? T: Nil);
}


/*
 *  pathname-match-p
 */
_g int pathname_match_p_(Execute ptr, addr *ret, addr a, addr b)
{
	int check;

	Return(pathname_designer_heap_(ptr, a, &a));
	Return(pathname_designer_heap_(ptr, b, &b));
	Return(wildcard_pathname_(a, b, 1, &check));
	return Result(ret, check? T: Nil);
}


/*
 *  translate-pathname
 */
_g int translate_pathname_(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	return translate_pathname_heap_(ptr, ret, pos, from, to);
}


/*
 *  translate-logical-pathname
 */
_g int translate_logical_pathname_(Execute ptr, addr *ret, addr pos)
{
	return physical_pathname_heap_(ptr, pos, ret);
}


/*
 *  merge-pathnames
 */
_g int merge_pathnames_(Execute ptr, addr *ret, addr pos, addr defaults, addr version)
{
	return merge_pathnames_clang_(ptr, pos, defaults, version, ret);
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
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	pathname_heap(&file, Nil, Nil, Nil, host, Nil);
	Return(merge_pathnames_clang_(ptr, host, defaults, Unbound, &file));
	/* load */
	Return(probe_file_files_(ptr, &x, file));
	/* host file is not exist */
	if (x != T)
		return Result(ret, 0);
	/* read file */
	Return(readlist_input(ptr, file, &x));
	Return(set_logical_pathname_translations_(ptr, host, x));
	return Result(ret, 1);
}


/*
 *  initialize
 */
_g void init_pathname_common(void)
{
	SetPointerCall(defun, var1, set_logical_pathname_translations);
}

