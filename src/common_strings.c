/*
 *  ANSI COMMON LISP: 16. Strings
 */
#include "call_strings.h"
#include "common_header.h"
#include "cons.h"
#include "strtype.h"

/* (defun stringp (object) ...) -> boolean */
static int function_stringp(Execute ptr, addr var)
{
	setbool_control(ptr, stringp(var));
	return 0;
}

static void defun_stringp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRINGP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_stringp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-string-p (object) ...) -> boolean */
static int function_simple_string_p(Execute ptr, addr var)
{
	simple_string_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_simple_string_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_STRING_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_string_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char (string index) ...) -> character
 *   string   string
 *   index    index
 */
static int function_char(Execute ptr, addr str, addr pos)
{
	Return(char_common_(str, pos, &str));
	setresult_control(ptr, str);
	return 0;
}

static void type_char(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, String);
	GetTypeTable(&type, Index);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun schar (string index) ...) -> character
 *   string   string
 *   index    index
 */
static void type_schar(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, SimpleString);
	GetTypeTable(&type, Index);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_schar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_schar(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf char) (character string index) ...) -> character */
static int function_setf_char(Execute ptr, addr value, addr pos, addr index)
{
	Return(setf_char_common_(value, pos, index));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_char(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, String);
	GetTypeTable(&type, Index);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_char);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_char(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun (setf schar) (character simple-string index) ...) -> character */
static void type_setf_schar(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, SimpleString);
	GetTypeTable(&type, Index);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_schar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCHAR, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_char);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_schar(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun string (x) ...) -> string
 *   x  (or string symbol character)  ;; string-designer
 */
static int function_string(Execute ptr, addr var)
{
	Return(string_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_string(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-upcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_string_upcase(Execute ptr, addr var, addr rest)
{
	Return(string_upcase_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_string_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_UPCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-downcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_string_downcase(Execute ptr, addr var, addr rest)
{
	Return(string_downcase_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_string_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_DOWNCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-capitalize (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_string_capitalize(Execute ptr, addr var, addr rest)
{
	Return(string_capitalize_common_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_string_capitalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_CAPITALIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_capitalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-upcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_nstring_upcase(Execute ptr, addr var, addr rest)
{
	Return(nstring_upcase_common_(var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nstring_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_UPCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-downcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_nstring_downcase(Execute ptr, addr var, addr rest)
{
	Return(nstring_downcase_common_(var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nstring_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_DOWNCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-capitalize (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static int function_nstring_capitalize(Execute ptr, addr var, addr rest)
{
	Return(nstring_capitalize_common_(var, rest));
	setresult_control(ptr, var);
	return 0;
}

static void defun_nstring_capitalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_CAPITALIZE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_capitalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-trim (sequence string) ...) -> string */
static int function_string_trim(Execute ptr, addr trim, addr pos)
{
	Return(string_trim_common_(trim, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_string_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_TRIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_string_left_trim(Execute ptr, addr trim, addr pos)
{
	Return(string_left_trim_common_(trim, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_string_left_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LEFT_TRIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_left_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_string_right_trim(Execute ptr, addr trim, addr pos)
{
	Return(string_right_trim_common_(trim, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_string_right_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_RIGHT_TRIM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_right_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string= (string1 string2 &key start1 end1 start2 end2) ...) -> boolean
 *   string1   (or string symbol character)  ;;string-designer
 *   string2   (or string symbol character)  ;;string-designer
 *   start1    keyword-start
 *   end1      keyword-end
 *   start2    keyword-start
 *   end2      keyword-end
 *   boolean   boolean
 *   mismatch  indexnull
 */
static int function_string_eql(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_eql_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringEqual);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string/= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_not_eql(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_eql_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string< (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_less(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_less_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_less(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_less);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string> (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_greater(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_greater_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_greater(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greater);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string<= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_less_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_less_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_less_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESS_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_less_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string>= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_greater_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_greater_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_greater_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATER_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greater_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-equal (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringEqual);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-not-equal (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_not_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_equal_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-lessp (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_lessp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_lessp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_lessp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_lessp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-greaterp (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_greaterp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_greaterp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_greaterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greaterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-not-graeter (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static int function_string_not_greaterp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_greaterp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_greaterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_GREATERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_greaterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string>= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int function_string_not_lessp(Execute ptr, addr var1, addr var2, addr rest)
{
	Return(string_not_lessp_common_(var1, var2, rest, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void defun_string_not_lessp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_LESSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_lessp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string (size &key initial-element element-type) ...) -> string
 *   size             index
 *   initial-elemnet  character
 *   element-type     t   ;; type-specifier
 *   string           simple-string
 */
static int function_make_string(Execute ptr, addr var, addr rest)
{
	Return(make_string_common_(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_string(addr *ret)
{
	/* (function (size &key
	 *             (initial-element character)
	 *             (element-type t))
	 *           (values simple-string &rest nil))
	 */
	addr args, values, symbol, type1, type2;

	/* args */
	GetTypeTable(&args, Index);
	GetTypeTable(&type1, Character);
	GetTypeTable(&type2, T);
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	cons_heap(&type1, symbol, type1);
	GetConst(KEYWORD_ELEMENT_TYPE, &symbol);
	cons_heap(&type2, symbol, type2);
	list_heap(&type1, type1, type2, NULL);
	typeargs_var1key(&args, args, type1);
	/* values */
	GetTypeValues(&values, SimpleString);
	type_compiled_heap(args, values, ret);
}

static void defun_make_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_strings(void)
{
	SetPointerCall(defun, var1, stringp);
	SetPointerCall(defun, var1, simple_string_p);
	SetPointerCall(defun, var2, char);
	SetPointerCall(defun, var3, setf_char);
	SetPointerCall(defun, var1, string);
	SetPointerCall(defun, var1dynamic, string_upcase);
	SetPointerCall(defun, var1dynamic, string_downcase);
	SetPointerCall(defun, var1dynamic, string_capitalize);
	SetPointerCall(defun, var1dynamic, nstring_upcase);
	SetPointerCall(defun, var1dynamic, nstring_downcase);
	SetPointerCall(defun, var1dynamic, nstring_capitalize);
	SetPointerCall(defun, var2, string_trim);
	SetPointerCall(defun, var2, string_left_trim);
	SetPointerCall(defun, var2, string_right_trim);
	SetPointerCall(defun, var2dynamic, string_eql);
	SetPointerCall(defun, var2dynamic, string_not_eql);
	SetPointerCall(defun, var2dynamic, string_less);
	SetPointerCall(defun, var2dynamic, string_greater);
	SetPointerCall(defun, var2dynamic, string_less_equal);
	SetPointerCall(defun, var2dynamic, string_greater_equal);
	SetPointerCall(defun, var2dynamic, string_equal);
	SetPointerCall(defun, var2dynamic, string_not_equal);
	SetPointerCall(defun, var2dynamic, string_lessp);
	SetPointerCall(defun, var2dynamic, string_greaterp);
	SetPointerCall(defun, var2dynamic, string_not_greaterp);
	SetPointerCall(defun, var2dynamic, string_not_lessp);
	SetPointerCall(defun, var1dynamic, make_string);
}

void build_common_strings(void)
{
	defun_stringp();
	defun_simple_string_p();
	defun_char();
	defun_schar();
	defun_setf_char();
	defun_setf_schar();
	defun_string();
	defun_string_upcase();
	defun_string_downcase();
	defun_string_capitalize();
	defun_nstring_upcase();
	defun_nstring_downcase();
	defun_nstring_capitalize();
	defun_string_trim();
	defun_string_left_trim();
	defun_string_right_trim();
	defun_string_eql();
	defun_string_not_eql();
	defun_string_less();
	defun_string_greater();
	defun_string_less_equal();
	defun_string_greater_equal();
	defun_string_equal();
	defun_string_not_equal();
	defun_string_lessp();
	defun_string_greaterp();
	defun_string_not_greaterp();
	defun_string_not_lessp();
	defun_make_string();
}

