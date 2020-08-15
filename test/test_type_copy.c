#include "type_copy.c"
#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "degrade.h"
#include "equal.h"
#include "hashtable.h"
#include "package.h"
#include "package_symbol.h"
#include "pathname.h"
#include "random_state.h"
#include "ratio.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_table.h"

static int testlispdecl(addr pos, enum LISPDECL decl)
{
	if (GetType(pos) != LISPTYPE_TYPE) {
		degrade_printf("type error\n");
		return 0;
	}
	if (LispDecl(pos) != decl) {
		degrade_printf("lispdecl error\n");
		return 0;
	}
	return 1;
}

static void parse_type_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	if (parse_type(Execute_Thread, ret, *ret, Nil)) {
		Error(fmte_("parse-type error.", NULL));
	}
}

static int test_getset_arraytype(void)
{
	addr left, right;

	type4_heap(LISPDECL_INTEGER, Nil, Nil, Nil, Nil, &left);
	parse_type_string(&right, "(integer 10 20)");
	getset_arraytype(NULL, left, right, 1);
	GetArrayType(left, 1, &left);
	test(RefFixnum(left) == 10, "getset_arraytype1");

	RETURN;
}

static int test_getsettype_arraytype(void)
{
	addr type, left, right;

	parse_type_string(&type, "real");
	type4_heap(LISPDECL_INTEGER, Nil, Nil, Nil, Nil, &left);
	type4_heap(LISPDECL_INTEGER, Nil, Nil, Nil, Nil, &right);
	SetArrayType(right, 1, type);
	getsettype_arraytype(NULL, left, right, 1);
	GetArrayType(left, 1, &left);
	test(testlispdecl(left, LISPDECL_REAL), "getsettype_arraytype1");
	test(left != type, "getsettype_arraytype2");

	RETURN;
}

static int test_getset_array4(void)
{
	addr left, right, value;

	vector4_heap(&left, 4);
	vector4_heap(&right, 4);
	fixnum_heap(&value, 10);
	SetArrayA4(right, 1, value);
	getset_array4(NULL, left, right, 1);
	GetArrayA4(left, 1, &left);
	test(RefFixnum(left) == 10, "getset_array4-1");

	RETURN;
}

static int test_getsettype_array4(void)
{
	addr type, left, right;

	parse_type_string(&type, "real");
	vector4_heap(&left, 4);
	vector4_heap(&right, 4);
	SetArrayA4(right, 1, type);
	getsettype_array4(NULL, left, right, 1);
	GetArrayA4(left, 1, &left);
	test(testlispdecl(left, LISPDECL_REAL), "getsettype_array4-1");
	test(left != type, "getsettype_array4-2");

	RETURN;
}

static int test_typecopy_empty(void)
{
	addr left, right;

	type0_heap(LISPDECL_ATOM, &left);
	typecopy_empty(NULL, &right, left);
	test(testlispdecl(left, LISPDECL_ATOM), "typecopy_empty1");
	test(left != right, "typecopy_empty2");

	RETURN;
}

static int test_typecopy_allobject(void)
{
	addr left, right;
	size_t size1, size2;

	parse_type_string(&left, "(not integer)");
	typecopy_allobject(NULL, &right, left);
	test(left != right, "typecopy_allobject1");
	test(GetType(right) == LISPTYPE_TYPE, "typecopy_allobject2");
	test(RefLispDecl(left) == RefLispDecl(right), "typecopy_allobject3");
	LenArrayType(left, &size1);
	LenArrayType(right, &size2);
	test(size1 == size2, "typecopy_allobject4");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "typecopy_allobject5");

	RETURN;
}

static int test_typecopy_alltype(void)
{
	addr left, right;
	size_t size1, size2;

	parse_type_string(&left, "(not integer)");
	typecopy_alltype(NULL, &right, left);
	test(left != right, "typecopy_alltype1");
	test(GetType(right) == LISPTYPE_TYPE, "typecopy_alltype2");
	test(RefLispDecl(left) == RefLispDecl(right), "typecopy_alltype3");
	LenArrayType(left, &size1);
	LenArrayType(right, &size2);
	test(size1 == size2, "typecopy_alltype4");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "typecopy_alltype5");
	test(RefLispDecl(left) == RefLispDecl(right), "typecopy_alltype6");

	RETURN;
}


/*
 *  object
 */
static int test_typecopy_type(void)
{
	addr left, right;

	GetTypeTable(&left, Type);
	type_copy_heap(&right, left);
	test(testlispdecl(left, LISPDECL_TYPE), "typecopy_type1");
	test(left != right, "typecopy_type2");

	RETURN;
}

static int test_typecopy_clos(void)
{
	addr left, right, clos;

	Error(interncommon_("STANDARD-CLASS", &clos, NULL));
	clos_find_class_(clos, &clos);
	type_clos_heap(clos, &left);
	type_copy_heap(&right, left);
	test(left != right, "typecopy_clos1");
	test(testlispdecl(right, LISPDECL_CLOS), "typecopy_clos2");
	GetArrayType(right, 0, &right);
	test(right == clos, "typecopy_clos3");

	RETURN;
}

static int test_typecopy_asterisk(void)
{
	addr left, right;

	GetTypeTable(&left, Asterisk);
	type_copy_heap(&right, left);
	test(left != right, "typecopy_asterisk1");
	test(testlispdecl(right, LISPDECL_ASTERISK), "typecopy_asterisk2");

	RETURN;
}

static int test_typecopy_optimized(void)
{
	addr left, right, aster;

	GetTypeTable(&aster, Asterisk);
	type1_heap(LISPDECL_OPTIMIZED, aster, &left);
	type_copy_heap(&right, left);
	test(left != right, "typecopy_optimized1");
	test(testlispdecl(right, LISPDECL_OPTIMIZED), "typecopy_optimized2");
	GetArrayType(right, 0, &right);
	test(testlispdecl(right, LISPDECL_ASTERISK), "typecopy_optimized3");
	test(right != aster, "typecopy_optimized4");

	RETURN;
}

static int test_typecopy_subtypep(void)
{
	addr left, right, aster;

	GetTypeTable(&aster, Asterisk);
	type1_heap(LISPDECL_SUBTYPEP, aster, &left);
	type_copy_heap(&right, left);
	test(left != right, "typecopy_subtypep1");
	test(testlispdecl(right, LISPDECL_SUBTYPEP), "typecopy_subtypep2");
	GetArrayType(right, 0, &right);
	test(testlispdecl(right, LISPDECL_ASTERISK), "typecopy_subtypep3");
	test(right != aster, "typecopy_subtypep4");

	RETURN;
}

static int test_typecopy_empty_check(enum TypeTable type)
{
	enum LISPDECL decl1, decl2;
	addr left, right;

	gettypetable(type, &left);
	decl1 = LispDecl(left);
	type_copy_heap(&right, left);
	decl2 = LispDecl(right);
	if (left == right) {
		degrade_printf("copy error\n");
		return 0;
	}
	if (decl1 != decl2) {
		degrade_printf("decl error\n");
		return 0;
	}

	return 1;
}
#define TypeCopyEmptyCheck(x) test_typecopy_empty_check(TypeTable_##x)

static int test_typecopy_emptytype(void)
{
	test(TypeCopyEmptyCheck(Atom), "typecopy_atom");
	test(TypeCopyEmptyCheck(List), "typecopy_list");
	test(TypeCopyEmptyCheck(Boolean), "typecopy_boolean");
	test(TypeCopyEmptyCheck(ExtendedChar), "typecopy_extended_char");
	test(TypeCopyEmptyCheck(Bit), "typecopy_bit");
	test(TypeCopyEmptyCheck(Fixnum), "typecopy_fixnum");
	test(TypeCopyEmptyCheck(Bignum), "typecopy_bignum");
	test(TypeCopyEmptyCheck(Nil), "typecopy_nil");
	test(TypeCopyEmptyCheck(T), "typecopy_t");
	test(TypeCopyEmptyCheck(Null), "typecopy_null");
	test(TypeCopyEmptyCheck(Hashtable), "typecopy_hashtable");
	test(TypeCopyEmptyCheck(Symbol), "typecopy_symbol");
	test(TypeCopyEmptyCheck(Keyword), "typecopy_keyword");
	test(TypeCopyEmptyCheck(Package), "typecopy_package");
	test(TypeCopyEmptyCheck(RandomState), "typecopy_random_state");
	test(TypeCopyEmptyCheck(Readtable), "typecopy_readtable");
	test(TypeCopyEmptyCheck(Pathname), "typecopy_pathname");
	test(TypeCopyEmptyCheck(LogicalPathname), "typecopy_logical_pathname");
	test(TypeCopyEmptyCheck(Sequence), "typecopy_sequence");
	test(TypeCopyEmptyCheck(Character), "typecopy_character");
	test(TypeCopyEmptyCheck(BaseChar), "typecopy_base_char");
	test(TypeCopyEmptyCheck(StandardChar), "typecopy_standard_char");
	test(TypeCopyEmptyCheck(Number), "typecopy_number");
	test(TypeCopyEmptyCheck(Restart), "typecopy_restart");
	test(TypeCopyEmptyCheck(Stream), "typecopy_stream");
	test(TypeCopyEmptyCheck(BroadcastStream), "typecopy_broadcast_stream");
	test(TypeCopyEmptyCheck(ConcatenatedStream), "typecopy_concatenated_stream");
	test(TypeCopyEmptyCheck(EchoStream), "typecopy_echo_stream");
	test(TypeCopyEmptyCheck(FileStream), "typecopy_file_stream");
	test(TypeCopyEmptyCheck(StringStream), "typecopy_string_stream");
	test(TypeCopyEmptyCheck(SynonymStream), "typecopy_synonym_stream");
	test(TypeCopyEmptyCheck(TwoWayStream), "typecopy_two_way_stream");
	RETURN;
}


/*
 *  Compound-type
 */
static int test_typecopy_and(void)
{
	addr left, right;

	parse_type_string(&left, "(and integer character)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_and1");
	test(testlispdecl(right, LISPDECL_AND), "typecopy_and2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "typecopy_and3");
	test(LenArrayA4r(left) == LenArrayA4r(right), "typecopy_and4");
	GetArrayA4(left, 0, &left);
	GetArrayA4(right, 0, &right);
	test(left != right, "typecopy_and5");
	test(testlispdecl(left, LISPDECL_INTEGER), "typecopy_and6");
	test(testlispdecl(right, LISPDECL_INTEGER), "typecopy_and7");

	RETURN;
}

static int test_typecopy_or(void)
{
	addr left, right;

	parse_type_string(&left, "(or integer character)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_or1");
	test(testlispdecl(right, LISPDECL_OR), "typecopy_or2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "typecopy_or3");
	test(LenArrayA4r(left) == LenArrayA4r(right), "typecopy_or4");
	GetArrayA4(left, 0, &left);
	GetArrayA4(right, 0, &right);
	test(left != right, "typecopy_or5");
	test(testlispdecl(left, LISPDECL_INTEGER), "typecopy_or6");
	test(testlispdecl(right, LISPDECL_INTEGER), "typecopy_or7");

	RETURN;
}

static int test_typecopy_eql(void)
{
	addr left, right;

	parse_type_string(&left, "(eql 100)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_eql1");
	test(testlispdecl(right, LISPDECL_EQL), "typecopy_eql2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "typecopy_eql3");

	RETURN;
}

static int test_typecopy_member(void)
{
	addr left, right, check1, check2;

	parse_type_string(&left, "(member 10 20 30)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_member1");
	test(testlispdecl(right, LISPDECL_MEMBER), "typecopy_member2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(LenArrayA4r(left) == LenArrayA4r(right), "typecopy_member3");
	GetArrayA4(left, 0, &check1);
	GetArrayA4(right, 0, &check2);
	test(check1 == check2, "typecopy_member4");
	GetArrayA4(left, 2, &check1);
	GetArrayA4(right, 2, &check2);
	test(check1 == check2, "typecopy_member5");

	RETURN;
}

static int test_typecopy_mod(void)
{
	addr left, right;

	parse_type_string(&left, "(mod 256)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_mod1");
	test(testlispdecl(right, LISPDECL_MOD), "typecopy_mod2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "typecopy_mod3");

	RETURN;
}

static int test_typecopy_not(void)
{
	addr left, right;

	parse_type_string(&left, "(not integer)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_not1");
	test(testlispdecl(right, LISPDECL_NOT), "typecopy_not2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "typecopy_not3");
	test(testlispdecl(left, LISPDECL_INTEGER), "typecopy_not4");
	test(testlispdecl(right, LISPDECL_INTEGER), "typecopy_not5");

	RETURN;
}

static int test_typecopy_satisfies(void)
{
	addr left, right;

	parse_type_string(&left, "(satisfies hello)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_satisfies1");
	test(testlispdecl(right, LISPDECL_SATISFIES), "typecopy_satisfies2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left == right, "typecopy_satisfies3");

	RETURN;
}

static int test_typecopy_values(void)
{
	addr left, right;

	readstring_debug(&left, "(values integer)");
	parse_type_values(Execute_Thread, &left, left, Nil);
	type_copy_heap(&right, left);
	test(left != right, "typecopy_values1");
	test(testlispdecl(right, LISPDECL_VALUES), "typecopy_values2");
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	test(left != right, "typecopy_values3");
	GetCar(left, &left);
	GetCar(right, &right);
	test(left != right, "typecopy_values4");
	test(testlispdecl(left, LISPDECL_INTEGER), "typecopy_values5");
	test(testlispdecl(right, LISPDECL_INTEGER), "typecopy_values6");

	readstring_debug(&left, "(values integer &optional atom &rest t)");
	parse_type_values(Execute_Thread, &left, left, Nil);
	type_copy_heap(&right, left);
	test(left != right, "typecopy_values7");

	RETURN;
}


/*
 *  Extract-type
 */
static int test_typecopy_size_equal(addr left, addr right)
{
	enum LISPDECL check1, check2;

	if (left == right) {
		degrade_printf("copy error.\n");
		return 0;
	}
	check1 = LispDecl(left);
	check2 = LispDecl(right);
	if (check1 != check2) {
		degrade_printf("type error.\n");
		return 0;
	}
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	if (type_asterisk_p(left) && type_asterisk_p(right)) {
		return 1;
	}
	if (! equal_debug(left, right)) {
		degrade_printf("size error.\n");
		return 0;
	}

	return 1;
}

static int test_typecopy_size_check(enum LISPDECL type)
{
	addr pos, left, right;

	/* check1 */
	GetTypeTable(&pos, Asterisk);
	type1_heap(type, pos, &left);
	type_copy_heap(&right, left);
	if (! test_typecopy_size_equal(left, right)) {
		degrade_printf("check1 error.\n");
		return 0;
	}

	/* check2 */
	fixnum_heap(&pos, 10);
	type1_heap(type, pos, &left);
	type_copy_heap(&right, left);
	if (! test_typecopy_size_equal(left, right)) {
		degrade_printf("check2 error.\n");
		return 0;
	}

	return 1;
}
#define TypeCopySizeCheck(x) test_typecopy_size_check(LISPDECL_##x)

static int test_typecopy_size(void)
{
	test(TypeCopySizeCheck(SIMPLE_VECTOR), "typecopy_simple_vector");
	test(TypeCopySizeCheck(BIT_VECTOR), "typecopy_bit_vector");
	test(TypeCopySizeCheck(SIMPLE_BIT_VECTOR), "typecopy_simple_bit_vector");
	test(TypeCopySizeCheck(STRING), "typecopy_string");
	test(TypeCopySizeCheck(BASE_STRING), "typecopy_base_string");
	test(TypeCopySizeCheck(SIMPLE_STRING), "typecopy_simple_string");
	test(TypeCopySizeCheck(SIMPLE_BASE_STRING), "typecopy_simple_base_string");
	test(TypeCopySizeCheck(SIGNED_BYTE), "typecopy_signed_byte");
	test(TypeCopySizeCheck(UNSIGNED_BYTE), "typecopy_unsigned_byte");
	RETURN;
}

static int test_typecopy_vector(void)
{
	addr left, right, check;

	parse_type_string(&left, "(vector * *)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_vector1");
	test(testlispdecl(right, LISPDECL_VECTOR), "typecopy_vector2");
	GetArrayType(right, 0, &check);
	test(type_asterisk_p(check), "typecopy_vector3");
	GetArrayType(right, 1, &check);
	test(type_asterisk_p(check), "typecopy_vector4");

	parse_type_string(&left, "(vector character 3)");
	type_copy_heap(&right, left);
	test(left != right, "typecopy_vector5");
	test(testlispdecl(right, LISPDECL_VECTOR), "typecopy_vector6");
	GetArrayType(right, 0, &check);
	test(testlispdecl(check, LISPDECL_CHARACTER), "typecopy_vector7");
	GetArrayType(right, 1, &check);
	test(RefFixnum(check) == 3, "typecopy_vector8");

	RETURN;
}


/*
 *  Atomic-type
 */
static int test_typecopy_cons(void)
{
	addr left, right, check;

	parse_type_string(&left, "cons");
	type_copy_heap(&right, left);
	test(testlispdecl(right, LISPDECL_CONS), "typecopy_cons1");
	GetArrayType(right, 0, &check);
	test(type_asterisk_p(check), "typecopy_cons2");
	GetArrayType(right, 1, &check);
	test(type_asterisk_p(check), "typecopy_cons3");

	parse_type_string(&left, "(cons integer string)");
	type_copy_heap(&right, left);
	test(testlispdecl(right, LISPDECL_CONS), "typecopy_cons4");
	GetArrayType(right, 0, &check);
	test(testlispdecl(check, LISPDECL_INTEGER), "typecopy_cons5");
	GetArrayType(right, 1, &check);
	test(testlispdecl(check, LISPDECL_STRING), "typecopy_cons6");

	RETURN;
}

static int test_typecopy_function(void)
{
	addr x, y, check;
	const char *str;

	parse_type_string(&x, "function");
	type_copy_heap(&y, x);
	test(testlispdecl(y, LISPDECL_FUNCTION), "typecopy_function1");
	GetArrayType(y, 0, &check);
	test(type_asterisk_p(check), "typecopy_function2");
	GetArrayType(y, 1, &check);
	test(type_asterisk_p(check), "typecopy_function3");
	GetArrayType(y, 2, &check);
	test(type_asterisk_p(check), "typecopy_function4");

	parse_type_string(&x, "(function () *)");
	type_copy_heap(&y, x);
	test(testlispdecl(y, LISPDECL_FUNCTION), "typecopy_function5");
	GetArrayType(y, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "typecopy_function6");
	test(lenarrayr(check) == 4, "typecopy_function7");
	GetArrayType(y, 1, &check);
	test(type_asterisk_p(check), "typecopy_function8");
	GetArrayType(y, 2, &check);
	test(check == Nil, "typecopy_function9");

	str = "(function (atom list &optional symbol keyword "
		"&rest t &key (aaa integer) (bbb package)) "
		"(values integer &rest nil))";
	parse_type_string(&x, str);
	type_copy_heap(&x, x);
	type_object_(&x, x);
	parse_type_string(&y, str);
	type_object_(&y, y);
	test(equal_debug(x, y), "typecopy_function10");

	RETURN;
}

static int test_typecopy_char(const char *str)
{
	addr x, y;

	parse_type_string(&x, str);
	type_copy_heap(&x, x);
	type_object_(&x, x);
	parse_type_string(&y, str);
	type_object_(&y, y);

	return equal_debug(x, y);
}

static int test_typecopy_compiled_function(void)
{
	addr x, y, check;
	const char *str;

	parse_type_string(&x, "compiled-function");
	type_copy_heap(&y, x);
	test(testlispdecl(y, LISPDECL_COMPILED_FUNCTION), "typecopy_compiled_function1");
	GetArrayType(y, 0, &check);
	test(type_asterisk_p(check), "typecopy_compiled_function2");
	GetArrayType(y, 1, &check);
	test(type_asterisk_p(check), "typecopy_compiled_function3");
	GetArrayType(y, 2, &check);
	test(type_asterisk_p(check), "typecopy_compiled_function4");

	parse_type_string(&x, "(compiled-function () *)");
	type_copy_heap(&y, x);
	test(testlispdecl(y, LISPDECL_COMPILED_FUNCTION), "typecopy_compiled_function5");
	GetArrayType(y, 0, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "typecopy_compiled_function6");
	test(lenarrayr(check) == 4, "typecopy_compiled_function7");
	GetArrayType(y, 1, &check);
	test(type_asterisk_p(check), "typecopy_compiled_function8");
	GetArrayType(y, 2, &check);
	test(check == Nil, "typecopy_compiled_function9");

	str = "(compiled-function (atom list &optional symbol keyword "
		"&rest t &key (aaa integer) (bbb package)) "
		"(values integer &rest nil))";
	test(test_typecopy_char(str), "typecopy_compiled_function10");

	RETURN;
}

static int test_typecopy_array(void)
{
	addr x, y, check;
	const char *str;

	parse_type_string(&x, "array");
	type_copy_heap(&y, x);
	test(testlispdecl(y, LISPDECL_ARRAY), "typecopy_array1");
	GetArrayType(y, 0, &check);
	test(type_asterisk_p(check), "typecopy_array2");
	GetArrayType(y, 1, &check);
	test(type_asterisk_p(check), "typecopy_array3");

	str = "(array character 2)";
	test(test_typecopy_char(str), "typecopy_array4");

	str = "(array character (2 3 4 5))";
	test(test_typecopy_char(str), "typecopy_array5");

	RETURN;
}

static int test_typecopy_simple_array(void)
{
	addr x, y, check;
	const char *str;

	parse_type_string(&x, "simple-array");
	type_copy_heap(&y, x);
	test(testlispdecl(y, LISPDECL_SIMPLE_ARRAY), "typecopy_simple_array1");
	GetArrayType(y, 0, &check);
	test(type_asterisk_p(check), "typecopy_simple_array2");
	GetArrayType(y, 1, &check);
	test(type_asterisk_p(check), "typecopy_simple_array3");

	str = "(simple-array character 2)";
	test(test_typecopy_char(str), "typecopy_simple_array4");

	str = "(simple-array character (2 3 4 5))";
	test(test_typecopy_char(str), "typecopy_simple_array5");

	RETURN;
}

static int test_typecopy_real_check(constindex type, const char *str)
{
	addr x, y;

	GetConstant(type, &x);
	cons_heap(&x, x, readr_debug(str));
	parse_type_unsafe(&x, x);
	type_copy_heap(&y, x);
	type_object_(&x, x);
	type_object_(&y, y);

	return equal_debug(x, y);
}

#define TypeCopyRealCheck(x,y) test_typecopy_real_check(CONSTANT_COMMON_##x,(y))

static int test_typecopy_real(void)
{
	test(TypeCopyRealCheck(REAL, "(* *)"), "test_typecopy_real1");
	test(TypeCopyRealCheck(REAL, "(10 20)"), "test_typecopy_real2");
	test(TypeCopyRealCheck(REAL, "(* (20))"), "test_typecopy_real3");
	test(TypeCopyRealCheck(REAL, "((10) 20)"), "test_typecopy_real4");

	test(TypeCopyRealCheck(RATIONAL, "(* *)"), "test_typecopy_rational1");
	test(TypeCopyRealCheck(RATIONAL, "(10 20)"), "test_typecopy_rational2");
	test(TypeCopyRealCheck(RATIONAL, "(* (20))"), "test_typecopy_rational3");
	test(TypeCopyRealCheck(RATIONAL, "((10) 20)"), "test_typecopy_rational4");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_copy(void)
{
	TestBreak(test_getset_arraytype);
	TestBreak(test_getsettype_arraytype);
	TestBreak(test_getset_array4);
	TestBreak(test_getsettype_array4);
	TestBreak(test_typecopy_empty);
	TestBreak(test_typecopy_allobject);
	TestBreak(test_typecopy_alltype);
	/* object */
	TestBreak(test_typecopy_type);
	TestBreak(test_typecopy_clos);
	TestBreak(test_typecopy_asterisk);
	TestBreak(test_typecopy_optimized);
	TestBreak(test_typecopy_subtypep);
	TestBreak(test_typecopy_emptytype);
	/* Compound-type */
	TestBreak(test_typecopy_and);
	TestBreak(test_typecopy_or);
	TestBreak(test_typecopy_eql);
	TestBreak(test_typecopy_member);
	TestBreak(test_typecopy_mod);
	TestBreak(test_typecopy_not);
	TestBreak(test_typecopy_satisfies);
	TestBreak(test_typecopy_values);
	/* Extract-type */
	TestBreak(test_typecopy_size);
	TestBreak(test_typecopy_vector);
	/* Atomic-type */
	TestBreak(test_typecopy_cons);
	TestBreak(test_typecopy_function);
	TestBreak(test_typecopy_compiled_function);
	TestBreak(test_typecopy_array);
	TestBreak(test_typecopy_simple_array);
	TestBreak(test_typecopy_real);

	return 0;
}

int test_type_copy(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_reader();
		lisp_initialize = 1;
		result = testbreak_type_copy();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

