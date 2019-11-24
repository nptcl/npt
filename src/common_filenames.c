/*
 *  ANSI COMMON LISP: 19. Filenames
 */
#include "common_header.h"
#include "cons.h"
#include "integer.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "type_parse.h"

/* (defun pathname (pathspec) ...) -> pathname */
static void function_pathname(Execute ptr, addr pos)
{
	pathname_designer_heap(ptr, pos, &pos);
	setresult_control(ptr, pos);
}

static void defun_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Pathname);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


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
static void function_make_pathname(Execute ptr, addr rest)
{
	make_pathname(ptr, &rest, rest);
	setresult_control(ptr, rest);
}

static void type_make_pathname(addr *ret)
{
	addr arg, values, name, value, common, keylocal;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8;

	KeyTypeTable(&key1, HOST, PathnameHost);
	KeyTypeTable(&key2, DEVICE, PathnameHost);
	KeyTypeTable(&key3, DIRECTORY, PathnameDirectory);
	KeyTypeTable(&key4, NAME, PathnameName);
	KeyTypeTable(&key5, TYPE, PathnameType);
	KeyTypeTable(&key6, VERSION, PathnameVersion);

	/* defaults   (or pathname null)  ;; default *default-pathname-defaults* */
	GetConst(KEYWORD_DEFAULTS, &name);
	GetTypeTable(&value, PathnameNull);
	cons_heap(&key7, name, value);
	/* case       (member :common :local) */
	GetConst(KEYWORD_CASE, &name);
	GetConst(KEYWORD_COMMON, &common);
	GetConst(KEYWORD_LOCAL, &keylocal);
	type_member_heap(&value, common, keylocal, NULL);
	cons_heap(&key8, name, value);
	/* key */
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);
	/* type */
	typeargs_key(&arg, key);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathnamep (object) ...) -> boolean */
static void function_pathnamep(Execute ptr, addr var)
{
	setbool_control(ptr, pathnamep(var));
}

static void defun_pathnamep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAMEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_pathnamep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-host (pathname &key case) ...) -> (or string symbol) */
static int pathname_case_local_p(addr rest)
{
	addr check;

	if (getkeyargs(rest, KEYWORD_CASE, &rest))
		return 1; /* default :local */
	GetConst(KEYWORD_LOCAL, &check);
	if (check == rest)
		return 1;
	GetConst(KEYWORD_COMMON, &check);
	if (check == rest)
		return 0;

	fmte("Invalid :case value ~S.", rest, NULL);
	return 1;
}

static void function_pathname_host(Execute ptr, addr pos, addr rest)
{
	pathname_designer_heap(ptr, pos, &pos);
	pathname_host(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_host(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PathnameCase);
	GetTypeTable(&values, PathnameHost);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_host(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_HOST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_host);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_host(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-device (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_device(Execute ptr, addr pos, addr rest)
{
	pathname_designer_heap(ptr, pos, &pos);
	pathname_device(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_device(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PathnameCase);
	GetTypeTable(&values, PathnameDevice);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_device(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_DEVICE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_device);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_device(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-directory (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_directory(Execute ptr, addr pos, addr rest)
{
	pathname_designer_heap(ptr, pos, &pos);
	pathname_directory(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_directory(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PathnameCase);
	GetTypeTable(&values, PathnameDirectory);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_DIRECTORY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_directory);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_directory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-name (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_name(Execute ptr, addr pos, addr rest)
{
	pathname_designer_heap(ptr, pos, &pos);
	pathname_name(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_name(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PathnameCase);
	GetTypeTable(&values, PathnameName);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-type (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_type(Execute ptr, addr pos, addr rest)
{
	pathname_designer_heap(ptr, pos, &pos);
	pathname_type(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_type(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PathnameCase);
	GetTypeTable(&values, PathnameType);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_pathname_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-version (pathname) ...) -> (or string symbol) */
static void function_pathname_version(Execute ptr, addr pos)
{
	pathname_designer_heap(ptr, pos, &pos);
	pathname_version(pos, &pos);
	setresult_control(ptr, pos);
}

static void type_pathname_version(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, PathnameVersion);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_VERSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_pathname_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_version(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun load-logical-pathname-translations (host) ...) -> boolean
 *   load-file:
 *     (merge-pathnames
 *       (make-pathname :name host)
 *       :defaults lisp-system::*load-logical-pathname-translations*)
 *   format:
 *     (logical-path1 physical-path1)
 *     (logical-path2 physical-path2)
 *     ...
 */
static void function_load_logical_pathname_translations(Execute ptr, addr pos)
{
	int check;
	if (load_logical_pathname_translations_common(ptr, pos, &check))
		return;
	setbool_control(ptr, check);
}

static void type_load_logical_pathname_translations(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, String);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_load_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_load_logical_pathname_translations);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_load_logical_pathname_translations(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun logical-pathname-translations (host) ...) -> list */
static void function_logical_pathname_translations(Execute ptr, addr host)
{
	get_logical_pathname_translations(host, &host);
	setresult_control(ptr, host);
}

static void type_logical_pathname_translations(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, String);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_logical_pathname_translations);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logical_pathname_translations(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf logical-pathname-translations) (value host) ...) -> list */
static void function_setf_logical_pathname_translations(Execute ptr,
		addr value, addr host)
{
	set_logical_pathname_translations(ptr, host, value);
	setresult_control(ptr, value);
}

static void type_setf_logical_pathname_translations(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, List);
	GetTypeTable(&values, String);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_logical_pathname_translations);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_logical_pathname_translations(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun logical-pathname (pathspec) ...) -> logical-pathname
 *   pathspec  (or logical-pathname string stream
 */
static void function_logical_pathname(Execute ptr, addr pos)
{
	logical_pathname(ptr, &pos, pos);
	setresult_control(ptr, pos);
}

static void type_logical_pathname(addr *ret)
{
	addr arg, values, pathname, string, stream;

	GetTypeTable(&pathname, LogicalPathname);
	GetTypeTable(&string, String);
	GetTypeTable(&stream, Stream);
	type3or_heap(pathname, string, stream, &arg);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, LogicalPathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_logical_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_logical_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_logical_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *default-pathname-defaults* [implementation-dependence]) */
#ifdef LISP_WINDOWS
#define DEFAULT_PATHNAME_MODE CONSTANT_SYSTEM_WINDOWS
#else
#define DEFAULT_PATHNAME_MODE CONSTANT_SYSTEM_UNIX
#endif

static void defvar_default_pathname_defaults(void)
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


/* (defun namestring (pathname) ...) -> namestring */
static void function_namestring(Execute ptr, addr pos)
{
	namestring_pathname(ptr, &pos, pos);
	setresult_control(ptr, pos);
}

static void defun_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-namestring (pathname) ...) -> namestring */
static void function_file_namestring(Execute ptr, addr pos)
{
	file_namestring_pathname(ptr, &pos, pos);
	setresult_control(ptr, pos);
}

static void defun_file_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun directory-namestring (pathname) ...) -> namestring */
static void function_directory_namestring(Execute ptr, addr pos)
{
	directory_namestring_pathname(ptr, &pos, pos);
	setresult_control(ptr, pos);
}

static void defun_directory_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIRECTORY_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_directory_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun host-namestring (pathname) ...) -> namestring */
static void function_host_namestring(Execute ptr, addr pos)
{
	host_namestring_pathname(ptr, &pos, pos);
	setresult_control(ptr, pos);
}

static void defun_host_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_HOST_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_host_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Namestring);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun enough-namestring (pathname &optional defaults) ...) -> string */
static void function_enough_namestring(Execute ptr, addr pos, addr defaults)
{
	enough_namestring_pathname(ptr, &pos, pos, defaults);
	setresult_control(ptr, pos);
}

static void type_enough_namestring(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var1opt1(&arg, arg, arg);
	GetTypeValues(&values, String);
	type_compiled_heap(arg, values, ret);
}

static void defun_enough_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENOUGH_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_enough_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_enough_namestring(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parse-namestring (thing
 *     &optional host defaults
 *     &key start end junk-allowed) ...) -> (values pathname position)
 *   thing         (or string stream pathname)  ;; pathname-designer
 *   host          (or string symbol)  ;; (or valid-pathname-host null)
 *   defaults      (or string stream pathname)  ;; pathname-designer
 *   start         keyword-start
 *   end           keyword-end
 *   junk-allowed  t  ;; boolean
 */
static void function_parse_namestring(Execute ptr, addr thing, addr rest)
{
	addr host, defaults, start, end, junk;

	if (rest == Nil) {
		host = defaults = Nil;
		goto keyargs;
	}
	getcons(rest, &host, &rest);
	if (rest == Nil) {
		defaults = Nil;
		goto keyargs;
	}
	getcons(rest, &defaults, &rest);
keyargs:
	if (getkeyargs(rest, KEYWORD_START, &start)) fixnum_heap(&start, 0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Nil;
	if (getkeyargs(rest, KEYWORD_JUNK_ALLOWED, &junk)) junk = Nil;

	parse_namestring(ptr, &thing, &start,
			thing, host, defaults, start, end, junk);
	setvalues_control(ptr, thing, start, NULL);
}

static void type_parse_namestring(addr *ret)
{
	addr arg, values, type, key, key1, key2, key3;

	/* key */
	KeyTypeTable(&key1, START, KeywordStart);
	KeyTypeTable(&key2, END, KeywordEnd);
	KeyTypeTable(&key3, JUNK_ALLOWED, T);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&arg, PathnameDesigner);
	GetTypeTable(&values, PathnameHost);
	typeargs_var1opt2key(&arg, arg, values, arg, key);
	GetTypeTable(&values, PathnameNull);
	GetTypeTable(&type, Index);
	typevalues_values2(&values, values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_parse_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PARSE_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_parse_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_parse_namestring(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun wild-pathname-p (path &optional field) ...) -> boolean
 *   path   pathname-designer
 *   field  (member :host :device :directory :name :type :version nil)
 */
static void function_wild_pathname_p(Execute ptr, addr pos, addr field)
{
	wild_pathname_p(ptr, &pos, pos, field);
	setresult_control(ptr, pos);
}

static void type_wild_pathname_p(addr *ret)
{
	addr arg, values, v1, v2, v3, v4, v5, v6;

	/* member */
	GetConst(KEYWORD_HOST, &v1);
	GetConst(KEYWORD_DEVICE, &v2);
	GetConst(KEYWORD_DIRECTORY, &v3);
	GetConst(KEYWORD_NAME, &v4);
	GetConst(KEYWORD_TYPE, &v5);
	GetConst(KEYWORD_VERSION, &v6);
	type_member_heap(&values, Nil, v1, v2, v3, v4, v5, v6, NULL);
	/* type */
	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_wild_pathname_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WILD_PATHNAME_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_wild_pathname_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_wild_pathname_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-match-p (path wildcard) ...) -> boolean
 *   path      pathname-desinger
 *   wildcard  pathname-designer
 */
static void function_pathname_match_p(Execute ptr, addr pos, addr wild)
{
	pathname_match_p(ptr, &pos, pos, wild);
	setresult_control(ptr, pos);
}

static void type_pathname_match_p(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var2(&arg, arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_match_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_MATCH_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_pathname_match_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_match_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun translate-pathname (source from to &key) ...) -> pathname
 *   source  pathname-designer
 *   to      pathname-designer
 *   from    pathname-designer
 */
static void function_translate_pathname(Execute ptr, addr pos, addr from, addr to)
{
	translate_pathname(ptr, &pos, pos, from, to);
	setresult_control(ptr, pos);
}

static void type_translate_pathname(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var3(&arg, arg, arg, arg);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_translate_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRANSLATE_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_translate_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_translate_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun translate-logical-pathname (pathname &key) ...) -> physical-pathname) */
static void function_translate_logical_pathname(Execute ptr, addr pos)
{
	translate_logical_pathname(ptr, &pos, pos);
	setresult_control(ptr, pos);
}

static void type_translate_logical_pathname(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_translate_logical_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRANSLATE_LOGICAL_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_translate_logical_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_translate_logical_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun merge-pathnames (pathname &optional defaults version) ...) -> pathname */
static void function_merge_pathnames(Execute ptr,
		addr pos, addr defaults, addr version)
{
	merge_pathnames(ptr, &pos, pos, defaults, version);
	setresult_control(ptr, pos);
}

static void type_merge_pathnames(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PathnameDesigner);
	GetTypeTable(&values, PathnameVersion);
	typeargs_var1opt2(&arg, arg, arg, values);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_merge_pathnames(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MERGE_PATHNAMES, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_merge_pathnames);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_merge_pathnames(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_filenames(void)
{
	SetPointerCall(defun, var1, pathname);
	SetPointerCall(defun, dynamic, make_pathname);
	SetPointerCall(defun, var1, pathnamep);
	SetPointerCall(defun, var1dynamic, pathname_host);
	SetPointerCall(defun, var1dynamic, pathname_device);
	SetPointerCall(defun, var1dynamic, pathname_directory);
	SetPointerCall(defun, var1dynamic, pathname_name);
	SetPointerCall(defun, var1dynamic, pathname_type);
	SetPointerCall(defun, var1, pathname_version);
	SetPointerCall(defun, var1, load_logical_pathname_translations);
	SetPointerCall(defun, var1, logical_pathname_translations);
	SetPointerCall(defun, var2, setf_logical_pathname_translations);
	SetPointerCall(defun, var1, logical_pathname);
	SetPointerCall(defun, var1, namestring);
	SetPointerCall(defun, var1, file_namestring);
	SetPointerCall(defun, var1, directory_namestring);
	SetPointerCall(defun, var1, host_namestring);
	SetPointerCall(defun, var1opt1, enough_namestring);
	SetPointerCall(defun, var1dynamic, parse_namestring);
	SetPointerCall(defun, var1opt1, wild_pathname_p);
	SetPointerCall(defun, var2, pathname_match_p);
	SetPointerCall(defun, var3, translate_pathname);
	SetPointerCall(defun, var1, translate_logical_pathname);
	SetPointerCall(defun, var1opt2, merge_pathnames);
}

_g void build_common_filenames(void)
{
	defun_pathname();
	defun_make_pathname();
	defun_pathnamep();
	defun_pathname_host();
	defun_pathname_device();
	defun_pathname_directory();
	defun_pathname_name();
	defun_pathname_type();
	defun_pathname_version();
	defun_load_logical_pathname_translations();
	defun_logical_pathname_translations();
	defun_setf_logical_pathname_translations();
	defun_logical_pathname();
	defvar_default_pathname_defaults();
	defun_namestring();
	defun_file_namestring();
	defun_directory_namestring();
	defun_host_namestring();
	defun_enough_namestring();
	defun_parse_namestring();
	defun_wild_pathname_p();
	defun_pathname_match_p();
	defun_translate_pathname();
	defun_translate_logical_pathname();
	defun_merge_pathnames();
}

