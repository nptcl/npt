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
	setcompiled_var1(pos, function_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Pathname);
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

	KeyCallType(&key1, HOST, PathnameHost);
	KeyCallType(&key2, DEVICE, PathnameHost);
	KeyCallType(&key3, DIRECTORY, PathnameDirectory);
	KeyCallType(&key4, NAME, PathnameName);
	KeyCallType(&key5, TYPE, PathnameType);
	KeyCallType(&key6, VERSION, PathnameVersion);

	/* defaults   (or pathname null)  ;; default *default-pathname-defaults* */
	GetConst(KEYWORD_DEFAULTS, &name);
	GetCallType(&value, PathnameNull);
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
	key_argtype(&arg, key);
	GetCallType(&values, Values_Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_make_pathname);
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
	setcompiled_var1(pos, function_pathnamep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
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
	pathname_host(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_host(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Args_PathnameCase);
	GetCallType(&values, PathnameHost);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_host(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_HOST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_pathname_host);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_host(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-device (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_device(Execute ptr, addr pos, addr rest)
{
	pathname_device(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_device(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Args_PathnameCase);
	GetCallType(&values, PathnameDevice);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_device(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_DEVICE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_pathname_device);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_device(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-directory (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_directory(Execute ptr, addr pos, addr rest)
{
	pathname_directory(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_directory(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Args_PathnameCase);
	GetCallType(&values, PathnameDirectory);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_DIRECTORY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_pathname_directory);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_directory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-name (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_name(Execute ptr, addr pos, addr rest)
{
	pathname_name(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_name(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Args_PathnameCase);
	GetCallType(&values, PathnameName);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_pathname_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-type (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_type(Execute ptr, addr pos, addr rest)
{
	pathname_type(pos, &pos, pathname_case_local_p(rest));
	setresult_control(ptr, pos);
}

static void type_pathname_type(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Args_PathnameCase);
	GetCallType(&values, PathnameType);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_pathname_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-version (pathname &key case) ...) -> (or string symbol) */
static void function_pathname_version(Execute ptr, addr pos)
{
	pathname_version(pos, &pos);
	setresult_control(ptr, pos);
}

static void type_pathname_version(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Pathname);
	var1_argtype(&arg, arg);
	GetCallType(&values, PathnameVersion);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_VERSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_pathname_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pathname_version(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun load-logical-pathname-translations (host) ...) -> boolean */
static void function_load_logical_pathname_translations(Execute ptr, addr pos)
{
	/* TODO */
	setresult_control(ptr, Nil);
}

static void type_load_logical_pathname_translations(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, String);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_load_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_load_logical_pathname_translations);
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

	GetCallType(&arg, String);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, ret);
}

static void defun_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_logical_pathname_translations);
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

	GetCallType(&arg, List);
	GetCallType(&values, String);
	var2_argtype(&arg, arg, values);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_logical_pathname_translations(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME_TRANSLATIONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_setf_logical_pathname_translations);
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

	GetCallType(&pathname, LogicalPathname);
	GetCallType(&string, String);
	GetCallType(&stream, Stream);
	type_or3(NULL, pathname, string, stream, &arg);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_LogicalPathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_logical_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOGICAL_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_logical_pathname);
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
	GetCallType(&type, Pathname);
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
	setcompiled_var1(pos, function_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Namestring);
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
	setcompiled_var1(pos, function_file_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Namestring);
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
	setcompiled_var1(pos, function_directory_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Namestring);
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
	setcompiled_var1(pos, function_host_namestring);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Namestring);
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

	GetCallType(&arg, PathnameDesigner);
	var1opt1_argtype(&arg, arg, arg);
	GetCallType(&values, Values_String);
	type_compiled_heap(arg, values, ret);
}

static void defun_enough_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENOUGH_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_enough_namestring);
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
	KeyCallType(&key1, START, KeywordStart);
	KeyCallType(&key2, END, KeywordEnd);
	KeyCallType(&key3, JUNK_ALLOWED, T);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetCallType(&arg, PathnameDesigner);
	GetCallType(&values, PathnameHost);
	var1opt2key_argtype(&arg, arg, values, arg, key);
	GetCallType(&values, PathnameNull);
	GetCallType(&type, Index);
	values2_valuestype(&values, values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_parse_namestring(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PARSE_NAMESTRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_parse_namestring);
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
	GetCallType(&arg, PathnameDesigner);
	var1opt1_argtype(&arg, arg, values);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_wild_pathname_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WILD_PATHNAME_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_wild_pathname_p);
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

	GetCallType(&arg, PathnameDesigner);
	var2_argtype(&arg, arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_pathname_match_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PATHNAME_MATCH_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_pathname_match_p);
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

	GetCallType(&arg, PathnameDesigner);
	var3_argtype(&arg, arg, arg, arg);
	GetCallType(&values, Values_Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_translate_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRANSLATE_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, function_translate_pathname);
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

	GetCallType(&arg, PathnameDesigner);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_translate_logical_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRANSLATE_LOGICAL_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_translate_logical_pathname);
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

	GetCallType(&arg, PathnameDesigner);
	GetCallType(&values, PathnameVersion);
	var1opt2_argtype(&arg, arg, arg, values);
	GetCallType(&values, Values_Pathname);
	type_compiled_heap(arg, values, ret);
}

static void defun_merge_pathnames(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MERGE_PATHNAMES, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, function_merge_pathnames);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_merge_pathnames(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  intern
 */
void intern_common_filenames(void)
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

