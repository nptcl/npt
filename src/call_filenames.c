#include "character_check.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "call_filenames.h"
#include "common_header.h"
#include "control_object.h"
#include "copy.h"
#include "execute.h"
#include "execute_object.h"
#include "file_open.h"
#include "files.h"
#include "hashtable.h"
#include "hold.h"
#include "integer.h"
#include "pathname.h"
#include "pathname_object.h"
#include "pathname_translate.h"
#include "pathname_wildcard.h"
#include "reader.h"
#include "stream.h"
#include "stream_function.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

/* pathname */
_g int pathname_common_(Execute ptr, addr var, addr *ret)
{
	return pathname_designer_heap_(ptr, var, ret);
}

/* make-pathname */
static int pathname_case_local_p_(addr rest, int *ret)
{
	addr check;

	if (GetKeyArgs(rest, KEYWORD_CASE, &rest))
		return Result(ret, 1); /* default :local */
	GetConst(KEYWORD_LOCAL, &check);
	if (check == rest)
		return Result(ret, 1);
	GetConst(KEYWORD_COMMON, &check);
	if (check == rest)
		return Result(ret, 0);

	*ret = 0;
	return fmte_("Invalid :case value ~S.", rest, NULL);
}

_g int make_pathname_common_(Execute ptr, addr rest, addr *ret)
{
	int localp;
	addr host, device, directory, name, type, version, defaults;

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
	Return(pathname_case_local_p_(rest, &localp));

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

	/* make-pathname */
	return make_pathname_heap_(ret,
			host, device, directory, name, type, version,
			localp);
}


/* pathnamep */
_g void pathnamep_common(addr var, addr *ret)
{
	*ret = pathnamep(var)? T: Nil;
}


/* pathname-host */
_g int pathname_host_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_host_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-device */
_g int pathname_device_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_device_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-directory */
static int pathname_directory_nil(addr pos, addr *ret)
{
	addr check, key;

	/* (:relative) -> nil */
	GetDirectoryPathname(pos, &pos);
	if (! consp_getcons(pos, &check, &pos))
		return 0;
	if (pos != Nil)
		return 0;
	GetConst(KEYWORD_RELATIVE, &key);
	if (check != key)
		return 0;

	*ret = Nil;
	return 1;
}

_g int pathname_directory_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designer_heap_(ptr, pos, &pos));
	if (pathname_directory_nil(pos, &pos))
		return Result(ret, pos);
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_directory_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-name */
_g int pathname_name_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_name_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-type */
_g int pathname_type_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	int localp;

	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(pathname_case_local_p_(rest, &localp));
	Return(pathname_type_(pos, &pos, localp));

	return Result(ret, pos);
}


/* pathname-version */
_g int pathname_version_common_(Execute ptr, addr pos, addr *ret)
{
	Return(pathname_designer_heap_(ptr, pos, &pos));
	pathname_version(pos, &pos);

	return Result(ret, pos);
}


/* load-logical-pathname-translations */
static int load_logical_pathname_read_(Execute ptr,
		LocalHold hold, addr stream, addr *ret)
{
	addr list, pos;
	int result;

	list = Nil;
	for (;;) {
		Return(read_stream(ptr, stream, &result, &pos));
		if (result)
			break;
		cons_heap(&list, pos, list);
		localhold_set(hold, 1, list);
	}
	nreverse(ret, list);
	localhold_set(hold, 1, list);

	return 0;
}

static int load_logical_pathname_file_(Execute ptr, addr file, addr *ret)
{
	addr control, save;
	LocalHold hold;

	push_control(ptr, &control);
	hold = LocalHold_array(ptr, 2);
	/* open file */
	if (open_input_stream_error_(ptr, &file, file))
		goto escape;
	localhold_set(hold, 0, file);

	/* load file */
	(void)load_logical_pathname_read_(ptr, hold, file, ret);

	/* close */
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (close_stream_(file, NULL))
		goto escape;
	restore_execute_control(ptr, save);

escape:
	return pop_control_(ptr, control);
}

_g int load_logical_pathname_translations_common_(Execute ptr, addr host, addr *ret)
{
	addr defaults, file, pos;

	/* pathname */
	GetConst(SYSTEM_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &defaults);
	getspecial_local(ptr, defaults, &defaults);
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	pathname_heap(&file, Nil, Nil, Nil, host, Nil);
	Return(merge_pathnames_clang_(ptr, host, defaults, Unbound, &file));
	/* load */
	Return(probe_file_files_(ptr, &pos, file));
	/* host file is not exist */
	if (pos != T)
		return Result(ret, Nil);
	/* read file */
	Return(load_logical_pathname_file_(ptr, file, &pos));
	Return(setf_logical_pathname_translations_common_(ptr, host, pos));
	return Result(ret, T);
}


/* logical-pathname-translations */
_g int logical_pathname_translations_common_(addr host, addr *ret)
{
	return gethost_logical_pathname_(host, ret);
}


/* (setf logical-pathname-translations) */
static int setf_logical_pathname_translations_list_(Execute ptr,
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
			Return(parse_pathname_setf_heap_(ptr, left, host, &value));
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

static int setf_logical_pathname_translations_code_(Execute ptr,
		addr host, addr list, addr cons)
{
	Return(setf_logical_pathname_translations_list_(ptr, &list, host, list));
	SetCdr(cons, list);
	return 0;
}

static int setf_logical_pathname_translations_call_(Execute ptr,
		addr host, addr list, addr table)
{
	addr cons;

	Return(intern_hashheap_(table, host, &cons));
	return setf_logical_pathname_translations_code_(ptr, host, list, cons);
}

static int setf_logical_pathname_translations_delete_(Execute ptr, addr host)
{
	int check;
	addr table;

	table_logical_pathname(&table);
	return delete_hashtable_(table, host, &check);
}

static int setf_logical_pathname_translations_intern_(Execute ptr,
		addr host, addr list, addr table)
{
	int check;
	addr control, save;

	push_control(ptr, &control);
	check = setf_logical_pathname_translations_call_(ptr, host, list, table);
	if (check == 0)
		goto escape;

	/* delete hashtable */
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (setf_logical_pathname_translations_delete_(ptr, host))
		goto escape;
	restore_execute_control(ptr, save);

escape:
	return pop_control_(ptr, control);
}

_g int setf_logical_pathname_translations_common_(Execute ptr, addr host, addr list)
{
	addr table, cons;

	table_logical_pathname(&table);
	Return(find_hashtable_(table, host, &cons));
	if (cons != Unbound)
		return setf_logical_pathname_translations_code_(ptr, host, list, cons);
	else
		return setf_logical_pathname_translations_intern_(ptr, host, list, table);
}


/* logical-pathname */
_g int logical_pathname_common_(Execute ptr, addr *ret, addr pos)
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


/* *default-pathname-defaults* */
#ifdef LISP_WINDOWS
#define DEFAULT_PATHNAME_MODE CONSTANT_SYSTEM_WINDOWS
#else
#define DEFAULT_PATHNAME_MODE CONSTANT_SYSTEM_UNIX
#endif

_g void default_pathname_defaults_common(void)
{
	static const constindex index = DEFAULT_PATHNAME_MODE;
	addr symbol, value, type;

	/* value */
	GetConstant(index, &value);
	pathname_heap(&value, value, Nil, Nil, Nil, Nil);

	/* symbol */
	GetConst(SPECIAL_DEFAULT_PATHNAME_DEFAULTS, &symbol);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Pathname);
	settype_value_symbol(symbol, type);
}


/* namestring */
_g int namestring_common_(Execute ptr, addr *ret, addr pos)
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


/* file-namestring */
_g int file_namestring_common_(Execute ptr, addr *ret, addr pos)
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


/* directory-namestring */
_g int directory_namestring_common_(Execute ptr, addr *ret, addr pos)
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


/* host-namestring */
_g int host_namestring_common_(Execute ptr, addr *ret, addr pos)
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


/* enough-namestring */
static void enough_namestring_copy(addr pos, addr cdr, addr *ret)
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

static int enough_namestring_directory_(LocalRoot local,
		addr pos, addr a, addr b,
		lisp_equal_calltype equal,
		addr *value, int *ret)
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
		Return((*equal)(car1, car2, &check));
		if (! check)
			return Result(ret, 0);
		cdr1 = next;
	}

	/* result */
	enough_namestring_copy(pos, cdr1, value);
	return Result(ret, 1);
}

static int enough_namestring_merge_(LocalRoot local,
		addr a, addr b, addr *value, int *ret)
{
	int check, check1, check2;
	addr pos1, pos2;
	lisp_equal_calltype equal;

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
	equal = pathname_equal_function(a);

	/* device */
	GetDevicePathname(a, &pos1);
	GetDevicePathname(b, &pos2);
	Return((*equal)(pos1, pos2, &check));
	if (! check)
		return Result(ret, 0);
	/* directory */
	GetDirectoryPathname(a, &pos1);
	GetDirectoryPathname(b, &pos2);
	return enough_namestring_directory_(local, a, pos1, pos2, equal, value, ret);
}

_g int enough_namestring_common_(Execute ptr, addr *ret, addr pos, addr defaults)
{
	int check;
	addr value;

	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(enough_namestring_merge_(ptr->local, pos, defaults, &value, &check));
	if (check)
		pos = value;
	return name_pathname_heap_(ptr, pos, ret);
}


/* parse-namestring */
static int parse_namestring_call_(Execute ptr, addr *ret, addr *position,
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
		string_length(thing, &index2);
		Return(keyword_start_end_value_(index2, start, end, &index1, &index2));
		Return(parse_pathname_full_heap_(ptr, thing, host, defaults,
					index1, index2, junk != Nil, &thing, &index1));
		make_index_integer_heap(position, index1);
		/* junk-allowed */
		if (thing == Nil)
			return Result(ret, Nil);
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

_g int parse_namestring_common_(Execute ptr,
		addr thing, addr rest, addr *ret1, addr *ret2)
{
	addr host, defaults, start, end, junk;

	if (rest == Nil) {
		host = defaults = Nil;
		goto keyargs;
	}
	Return_getcons(rest, &host, &rest);
	if (rest == Nil) {
		defaults = Nil;
		goto keyargs;
	}
	Return_getcons(rest, &defaults, &rest);
keyargs:
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Nil;
	if (GetKeyArgs(rest, KEYWORD_JUNK_ALLOWED, &junk))
		junk = Nil;

	return parse_namestring_call_(ptr,
			ret1, ret2, thing, host, defaults, start, end, junk);
}

/* wild-pathname-p */
_g int wild_pathname_p_common_(Execute ptr, addr *ret, addr file, addr field)
{
	int check;

	Return(pathname_designer_heap_(ptr, file, &file));
	if (field == Unbound)
		field = Nil;
	Return(wild_pathname_boolean_(file, field, &check));
	return Result(ret, check? T: Nil);
}


/* pathname-match-p */
_g int pathname_match_p_common_(Execute ptr, addr *ret, addr a, addr b)
{
	int check;

	Return(pathname_designer_heap_(ptr, a, &a));
	Return(pathname_designer_heap_(ptr, b, &b));
	Return(wildcard_pathname_(a, b, 1, &check));
	return Result(ret, check? T: Nil);
}


/* translate-pathname */
_g int translate_pathname_common_(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	return translate_pathname_heap_(ptr, ret, pos, from, to);
}


/* translate-logical-pathname */
_g int translate_logical_pathname_common_(Execute ptr, addr *ret, addr pos)
{
	return physical_pathname_heap_(ptr, pos, ret);
}


/* merge-pathnames */
_g int merge_pathnames_common_(Execute ptr,
		addr *ret, addr pos, addr defaults, addr version)
{
	return merge_pathnames_clang_(ptr, pos, defaults, version, ret);
}

