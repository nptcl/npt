/*
 *  ANSI COMMON LISP: 13. Characters
 */
#include "character.h"
#include "common_header.h"
#include "call_characters.h"

/* (defconstnat char-code-limit UnicodeCount) */
static void defconstant_char_code_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_CHAR_CODE_LIMIT, &symbol);
	fixnum_heap(&value, UnicodeCount);
	defconstant_symbol(symbol, value);
}


/* (defun char= (character &rest character) ...) -> boolean */
static void defun_char_check(constindex index, pointer p)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CharEql);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_char_eql(Execute ptr, addr list)
{
	Return(char_eql_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_eql(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_EQL, p_defun_char_eql);
}


/* (defun char/= (character &rest character) ...) -> boolean */
static int function_char_not_eql(Execute ptr, addr list)
{
	Return(char_not_eql_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NOT_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_char_not_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CharEql);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char< (character &rest character) ...) -> boolean */
static int function_char_less(Execute ptr, addr list)
{
	Return(char_less_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_less(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESS, p_defun_char_less);
}


/* (defun char> (character &rest character) ...) -> boolean */
static int function_char_greater(Execute ptr, addr list)
{
	Return(char_greater_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_greater(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATER, p_defun_char_greater);
}


/* (defun char<= (character &rest character) ...) -> boolean */
static int function_char_less_equal(Execute ptr, addr list)
{
	Return(char_less_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_less_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESS_EQUAL, p_defun_char_less_equal);
}


/* (defun char>= (character &rest character) ...) -> boolean */
static int function_char_greater_equal(Execute ptr, addr list)
{
	Return(char_greater_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_greater_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATER_EQUAL, p_defun_char_greater_equal);
}


/* (defun char-equal (character &rest character) ...) -> boolean */
static int function_char_equal(Execute ptr, addr list)
{
	Return(char_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_EQUAL, p_defun_char_equal);
}


/* (defun char-not-equal (character &rest character) ...) -> boolean */
static int function_char_not_equal(Execute ptr, addr list)
{
	Return(char_not_equal_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NOT_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_char_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, CharEql);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-lessp (character &rest character) ...) -> boolean */
static int function_char_lessp(Execute ptr, addr list)
{
	Return(char_lessp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_lessp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESSP, p_defun_char_lessp);
}


/* (defun char-greaterp (character &rest character) ...) -> boolean */
static int function_char_greaterp(Execute ptr, addr list)
{
	Return(char_greaterp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_greaterp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATERP, p_defun_char_greaterp);
}


/* (defun char-not-lessp (character &rest character) ...) -> boolean */
static int function_char_not_lessp(Execute ptr, addr list)
{
	Return(char_not_lessp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_lessp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_NOT_LESSP, p_defun_char_not_lessp);
}


/* (defun char-not-greaterp (character &rest character) ...) -> boolean */
static int function_char_not_greaterp(Execute ptr, addr list)
{
	Return(char_not_greaterp_common_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_char_not_greaterp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_NOT_GREATERP, p_defun_char_not_greaterp);
}


/* (defun character (character) ...) -> character */
static int function_character(Execute ptr, addr var)
{
	Return(character_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_character_stringone(addr *ret)
{
	addr pos;
	fixnum_heap(&pos, 1);
	type1_heap(LISPDECL_SIMPLE_BASE_STRING, pos, ret);
}

static void type_character_arguments(addr *ret)
{
	addr type1, type2, type3, pos;

	/* (or character
	 *     (simple-base-string 1)
	 *     symbol)  ;; (= (length (symbol-name x)) 1)
	 */
	GetTypeTable(&type1, Character);
	type_character_stringone(&type2);
	GetTypeTable(&type3, Symbol);
	type3or_heap(type1, type2, type3, &pos);
	typeargs_var1(ret, pos);
}

static void type_character(addr *ret)
{
	addr args, values;

	type_character_arguments(&args);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun characterp (object) ...) -> boolean */
static int function_characterp(Execute ptr, addr var)
{
	setbool_control(ptr, characterp(var));
	return 0;
}

static void defun_characterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHARACTERP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_characterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun alpha-char-p (character) ...) -> boolean */
static int function_alpha_char_p(Execute ptr, addr var)
{
	alpha_char_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_character_boolean(addr *ret)
{
	addr args, values;

	/* (function (character) boolean) */
	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_alpha_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ALPHA_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_alpha_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun alphanumericp (character) ...) -> boolean */
static int function_alphanumericp(Execute ptr, addr var)
{
	alphanumericp_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_alphanumericp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ALPHANUMERICP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_alphanumericp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun digit-char (weight &optional radix) ...) -> result
 *   weight  (integer 0 *)
 *   radix   (integer 2 36), default 10
 *   result  (or null character)
 */
static int function_digit_char(Execute ptr, addr var, addr opt)
{
	digit_char_common(var, opt, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_digit_char(addr *ret)
{
	addr weight, radix, args, values;

	/* ((integer 0 *) &optional (integer 2 36)) */
	GetTypeTable(&weight, Intplus);
	GetTypeTable(&radix, RadixInteger);
	typeargs_var1opt1(&args, weight, radix);
	/* (or character null) */
	GetTypeValues(&values, CharacterNull);
	/* function */
	type_compiled_heap(args, values, ret);
}

static void defun_digit_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIGIT_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_digit_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_digit_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun digit-char-p (character &optional radix) ...) -> weight
 *   character  character
 *   radix      (integer 2 36), default 10
 *   weight     (or (integer 0 *) null)
 */
static int function_digit_char_p(Execute ptr, addr var, addr opt)
{
	digit_char_p_common(var, opt, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_digit_char_p(addr *ret)
{
	addr character, radix, args, values;

	/* (character &optional (integer 2 36)) */
	GetTypeTable(&character, Character);
	GetTypeTable(&radix, RadixInteger);
	typeargs_var1opt1(&args, character, radix);
	/* (or (integer 0 *) null) */
	GetTypeValues(&values, IntplusNull);
	/* function */
	type_compiled_heap(args, values, ret);
}

static void defun_digit_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIGIT_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_digit_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_digit_char_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun graphic-char-p (character) ...) -> boolean */
static int function_graphic_char_p(Execute ptr, addr var)
{
	graphic_char_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_graphic_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GRAPHIC_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_graphic_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun standard-char-p (character) ...) -> boolean */
static int function_standard_char_p(Execute ptr, addr var)
{
	standard_char_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_standard_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STANDARD_CHAR_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_standard_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-upcase (character) ...) -> character */
static int function_char_upcase(Execute ptr, addr var)
{
	char_upcase_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_character_character(addr *ret)
{
	addr args, values;

	/* (function (character) character) */
	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_char_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_UPCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-downcase (character) ...) -> character */
static int function_char_downcase(Execute ptr, addr var)
{
	char_downcase_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_char_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_DOWNCASE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun upper-case-p (character) ...) -> boolean */
static int function_upper_case_p(Execute ptr, addr var)
{
	upper_case_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_upper_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPPER_CASE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_upper_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lower-case-p (character) ...) -> boolean */
static int function_lower_case_p(Execute ptr, addr var)
{
	lower_case_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_lower_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOWER_CASE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_lower_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun both-case-p (character) ...) -> boolean */
static int function_both_case_p(Execute ptr, addr var)
{
	both_case_p_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_both_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BOTH_CASE_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_both_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-code (character) ...) -> code
 *   code  (integer 0 UnicodeCount)
 */
static int function_char_code(Execute ptr, addr var)
{
	char_code_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_char_code(addr *ret)
{
	addr args, values;

	/* (function (character) (values (integer 0 UnicodeCount))) */
	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	type4integer_heap(Nil, 0, Nil, (fixnum)UnicodeCount, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_char_code(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_CODE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_code);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_code(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void defun_char_int(void)
{
	/* char-code == char-int */
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_INT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_code);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_code(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun code-char (code) ...) -> char-p
 *   code    (integer 0 (UnicodeCount))
 *   char-p  (or character boolean)
 */
static int function_code_char(Execute ptr, addr var)
{
	code_char_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_code_char(addr *ret)
{
	addr args, values, type;

	/* (function
	 *   ((integer 0 UnicodeCount))
	 *   (values (or character boolean))) */
	type4integer_heap(Nil, 0, T, (fixnum)UnicodeCount, &args);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Character);
	GetTypeTable(&type, Boolean);
	type2or_heap(values, type, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_code_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CODE_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_code_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_code_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-name (character) ...) -> name
 *   name  (or string null)
 */
static int function_char_name(Execute ptr, addr var)
{
	Return(char_name_common_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_char_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Character);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_char_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun name-char (name) ...) -> char-p
 *   name    (or string symbol character)  ;; string-designator
 *   char-p  (or character null)
 */
static int function_name_char(Execute ptr, addr var)
{
	Return(name_char_common_(ptr->local, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_name_char(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesignator);
	typeargs_var1(&args, args);
	GetTypeValues(&values, CharacterNull);
	type_compiled_heap(args, values, ret);
}

static void defun_name_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NAME_CHAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_name_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_name_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_characters(void)
{
	SetPointerCall(defun, dynamic, char_eql);
	SetPointerCall(defun, dynamic, char_less);
	SetPointerCall(defun, dynamic, char_greater);
	SetPointerCall(defun, dynamic, char_less_equal);
	SetPointerCall(defun, dynamic, char_greater_equal);
	SetPointerCall(defun, dynamic, char_equal);
	SetPointerCall(defun, dynamic, char_lessp);
	SetPointerCall(defun, dynamic, char_greaterp);
	SetPointerCall(defun, dynamic, char_not_lessp);
	SetPointerCall(defun, dynamic, char_not_greaterp);
	SetPointerCall(defun, dynamic, char_not_eql);
	SetPointerCall(defun, dynamic, char_not_equal);
	SetPointerCall(defun, var1, character);
	SetPointerCall(defun, var1, characterp);
	SetPointerCall(defun, var1, alpha_char_p);
	SetPointerCall(defun, var1, alphanumericp);
	SetPointerCall(defun, var1opt1, digit_char);
	SetPointerCall(defun, var1opt1, digit_char_p);
	SetPointerCall(defun, var1, graphic_char_p);
	SetPointerCall(defun, var1, standard_char_p);
	SetPointerCall(defun, var1, char_upcase);
	SetPointerCall(defun, var1, char_downcase);
	SetPointerCall(defun, var1, upper_case_p);
	SetPointerCall(defun, var1, lower_case_p);
	SetPointerCall(defun, var1, both_case_p);
	SetPointerCall(defun, var1, char_code);
	SetPointerCall(defun, var1, char_code);
	SetPointerCall(defun, var1, code_char);
	SetPointerCall(defun, var1, char_name);
	SetPointerCall(defun, var1, name_char);
}

void build_common_characters(void)
{
	defconstant_char_code_limit();
	defun_char_eql();
	defun_char_not_eql();
	defun_char_less();
	defun_char_greater();
	defun_char_less_equal();
	defun_char_greater_equal();
	defun_char_equal();
	defun_char_not_equal();
	defun_char_lessp();
	defun_char_greaterp();
	defun_char_not_lessp();
	defun_char_not_greaterp();
	defun_character();
	defun_characterp();
	defun_alpha_char_p();
	defun_alphanumericp();
	defun_digit_char();
	defun_digit_char_p();
	defun_graphic_char_p();
	defun_standard_char_p();
	defun_char_upcase();
	defun_char_downcase();
	defun_upper_case_p();
	defun_lower_case_p();
	defun_both_case_p();
	defun_char_code();
	defun_char_int();
	defun_code_char();
	defun_char_name();
	defun_name_char();
}

