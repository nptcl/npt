/*
 *  ANSI COMMON LISP: 20. Files
 */
#include "call_files.h"
#include "common_header.h"
#include "cons.h"
#include "cons_plist.h"
#include "files.h"
#include "pathname.h"
#include "type_parse.h"

/* (defun directory (pathname &key) ...) -> list */
static int function_directory(Execute ptr, addr pos)
{
	Return(directory_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_directory(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIRECTORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_directory);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_directory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun probe-file (pathspec) ...) -> truename */
static int function_probe_file(Execute ptr, addr pos)
{
	Return(probe_file_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_probe_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PROBE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_probe_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Pathname);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ensure-directories-exist (pathspec &key verbose) ...)
 *     -> pathspec, created
 *   pathspec  pathname-designator
 *   verbose   t  ;; boolean
 *   created   boolean
 */
static int function_ensure_directories_exist(Execute ptr, addr pos, addr rest)
{
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &rest))
		rest = Nil;
	Return(ensure_directories_exist_files_(ptr, &pos, &rest, pos, rest != Nil));
	setvalues_control(ptr, pos, rest, NULL);

	return 0;
}

static void type_ensure_directories_exist(addr *ret)
{
	addr args, values, type;

	/* key */
	KeyTypeTable(&type, VERBOSE, T);
	conscar_heap(&type, type);
	/* type */
	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1key(&args, args, type);
	GetTypeTable(&values, Pathname);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_directories_exist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENSURE_DIRECTORIES_EXIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_directories_exist);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ensure_directories_exist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun truename (pathspec) ...) -> pathname */
static int function_truename(Execute ptr, addr pos)
{
	Return(truename_files_(ptr, pos, &pos, 1));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_truename(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRUENAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_truename);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Pathname);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-author (pathspec) ...) -> (or string null) */
static int function_file_author(Execute ptr, addr pos)
{
	Return(file_author_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_file_author(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_file_author(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_AUTHOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_author);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_author(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-write-date (pathspec) ...) -> (or integer null) */
static int function_file_write_date(Execute ptr, addr pos)
{
	Return(file_write_date_files_(ptr, &pos, pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_file_write_date(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeTable(&values, IntegerNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_file_write_date(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_WRITE_DATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_write_date);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_write_date(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rename-file (file rename) ...) -> new from to */
static int function_rename_file(Execute ptr, addr file1, addr file2)
{
	addr file3;

	Return(rename_file_files_(ptr, &file1, &file2, &file3, file1, file2));
	setvalues_control(ptr, file1, file2, file3, NULL);

	return 0;
}

static void type_rename_file(addr *ret)
{
	addr args, values;

	GetTypeTable(&values, PathnameDesignator);
	typeargs_var2(&args, values, values);
	typevalues_values3(&values, values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_rename_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RENAME_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rename_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_rename_file(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-file (file) ...) -> (eql t) */
static int function_delete_file(Execute ptr, addr pos)
{
	Return(delete_file_files_(ptr, pos));
	setresult_control(ptr, T);
	return 0;
}

static void type_delete_file(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, ret);
}

static void defun_delete_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_delete_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_delete_file(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-error-pathname (condition) ...) -> pathname-designator */
static int function_file_error_pathname(Execute ptr, addr var)
{
	Return(file_error_pathname_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_file_error_pathname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FileError);
	typeargs_var1(&args, args);
	GetTypeTable(&values, T);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_file_error_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_ERROR_PATHNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_error_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_error_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_files(void)
{
	SetPointerCall(defun, var1, directory);
	SetPointerCall(defun, var1, probe_file);
	SetPointerCall(defun, var1dynamic, ensure_directories_exist);
	SetPointerCall(defun, var1, truename);
	SetPointerCall(defun, var1, file_author);
	SetPointerCall(defun, var1, file_write_date);
	SetPointerCall(defun, var2, rename_file);
	SetPointerCall(defun, var1, delete_file);
	SetPointerCall(defun, var1, file_error_pathname);
}

void build_common_files(void)
{
	defun_directory();
	defun_probe_file();
	defun_ensure_directories_exist();
	defun_truename();
	defun_file_author();
	defun_file_write_date();
	defun_rename_file();
	defun_delete_file();
	defun_file_error_pathname();
}

