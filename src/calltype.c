#include "bignum.h"
#include "calltype.h"
#include "clos.h"
#include "clos_object.h"
#include "condition.h"
#include "constant.h"
#include "integer.h"
#include "type_parse.h"

/*
 *  interface
 */
void keycons_calltype(addr *ret, constindex name, enum CallType type)
{
	addr value1, value2;

	GetConstant(name, &value1);
	getcalltype(&value2, type);
	cons_heap(ret, value1, value2);
}

void clos_type(addr *ret, addr clos)
{
	Check(! conditionp(clos), "type error");
	type_object1(NULL, LISPDECL_CLOS, clos, ret);
}

void clos_type_constant(addr *ret, constindex name)
{
	addr clos;
	GetConstant(name, &clos);
	clos_type(ret, clos);
}

void nonnil_symbol_type(addr *ret)
{
	/* (and (not null) symbol) */
	addr null, symbol;

	type_empty_not(NULL, LISPDECL_NULL, &null);
	type_empty(NULL, LISPDECL_SYMBOL, &symbol);
	type_and(NULL, null, symbol, ret);
	SetStatusReadOnly(*ret);
}

void radix_integer_type(addr *ret)
{
	/* (integer 2 36) */
	type_intrange(Nil, 2, Nil, 36, ret);
	SetStatusReadOnly(*ret);
}

static void integer_plus_type(addr *ret)
{
	/* (integer 0 *) */
	type_intrange_left(Nil, 0, ret);
	SetStatusReadOnly(*ret);
}

static void package_designer_type(addr *ret)
{
	addr array, pos;

	/* (or package string symbol character) */
	vector4_heap(&array, 4);
	GetCallType(&pos, Package);
	SetArrayA4(array, 0, pos);
	GetCallType(&pos, String);
	SetArrayA4(array, 1, pos);
	GetCallType(&pos, Symbol);
	SetArrayA4(array, 2, pos);
	GetCallType(&pos, Character);
	SetArrayA4(array, 3, pos);
	type_object1(NULL, LISPDECL_OR, array, ret);
	SetStatusReadOnly(*ret);
}

static void function_designer_type(addr *ret)
{
	addr type1, type2;

	/* (or function symbol) */
	GetCallType(&type1, Function);
	GetCallType(&type2, Symbol);
	type_or(NULL, type1, type2, ret);
	SetStatusReadOnly(*ret);
}

static void restart_designer_type(addr *ret)
{
	addr type1, type2;

	/* (or restart (and symbol (not null))) */
	GetCallType(&type1, Restart);
	GetCallType(&type2, NonNilSymbol);
	type_or(NULL, type1, type2, ret);
	SetStatusReadOnly(*ret);
}

static void typesymbol_type(addr *ret)
{
	addr type1, type2;

	/* (or symbol (cons * *)) */
	GetCallType(&type1, Symbol);
	GetCallType(&type2, Cons);
	type_or(NULL, type1, type2, ret);
	SetStatusReadOnly(*ret);
}

static void typespec_type(addr *ret)
{
	addr array, pos;

	/* (or [type] symbol (cons * *)) */
	vector4_heap(&array, 3);
	GetCallType(&pos, Type);
	SetArrayA4(array, 0, pos);
	GetCallType(&pos, Cons);
	SetArrayA4(array, 1, pos);
	GetCallType(&pos, Symbol);
	SetArrayA4(array, 2, pos);
	type_object1(NULL, LISPDECL_OR, array, ret);
	SetStatusReadOnly(*ret);
}

static void environment_null_type(addr *ret)
{
	addr type1, type2;

	/* (or environment null) */
	type_empty(NULL, LISPDECL_ENVIRONMENT, &type1);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, ret);
	SetStatusReadOnly(*ret);
}

static void integer_index_type(addr *ret)
{
	addr left, right;

	/* (integer 0 SIZE_MAX) */
	fixnum_heap(&left, 0);
	make_indexmax_alloc(NULL, &right);
	type_object4(NULL, LISPDECL_INTEGER, Nil, left, Nil, right, ret);
	SetStatusReadOnly(*ret);
}

static void string_designer_type(addr *ret)
{
	addr array, pos;

	/* (or string symbol character) */
	vector4_heap(&array, 3);
	GetCallType(&pos, String);
	SetArrayA4(array, 0, pos);
	GetCallType(&pos, Symbol);
	SetArrayA4(array, 1, pos);
	GetCallType(&pos, Character);
	SetArrayA4(array, 2, pos);
	type_object1(NULL, LISPDECL_OR, array, ret);
	SetStatusReadOnly(*ret);
}

void empty_argtype(addr *ret)
{
	vector2_heap(ret, 4);
}

void full_argtype(addr *ret, addr var, addr opt, addr rest, addr key)
{
	addr pos;

	vector2_heap(&pos, 4);
	SetArrayA2(pos, 0, var);
	SetArrayA2(pos, 1, opt);
	SetArrayA2(pos, 2, rest);
	SetArrayA2(pos, 3, key);
	*ret = pos;
}

void var1_argtype(addr *ret, addr type)
{
	conscar_heap(&type, type);
	full_argtype(ret, type, Nil, Nil, Nil);
}

void var2_argtype(addr *ret, addr type1, addr type2)
{
	list_heap(&type1, type1, type2, NULL);
	full_argtype(ret, type1, Nil, Nil, Nil);
}

void var3_argtype(addr *ret, addr type1, addr type2, addr type3)
{
	list_heap(&type1, type1, type2, type3, NULL);
	full_argtype(ret, type1, Nil, Nil, Nil);
}

void var4_argtype(addr *ret, addr type1, addr type2, addr type3, addr type4)
{
	list_heap(&type1, type1, type2, type3, type4, NULL);
	full_argtype(ret, type1, Nil, Nil, Nil);
}

void var1key_argtype(addr *ret, addr var, addr key)
{
	conscar_heap(&var, var);
	full_argtype(ret, var, Nil, Nil, key);
}

void var2key_argtype(addr *ret, addr var1, addr var2, addr key)
{
	list_heap(&var1, var1, var2, NULL);
	full_argtype(ret, var1, Nil, Nil, key);
}

void var3key_argtype(addr *ret, addr var1, addr var2, addr var3, addr key)
{
	list_heap(&var1, var1, var2, var3, NULL);
	full_argtype(ret, var1, Nil, Nil, key);
}

void var4key_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4, addr key)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	full_argtype(ret, v1, Nil, Nil, key);
}

void opt1_argtype(addr *ret, addr type)
{
	conscar_heap(&type, type);
	full_argtype(ret, Nil, type, Nil, Nil);
}

void opt2_argtype(addr *ret, addr type1, addr type2)
{
	list_heap(&type1, type1, type2, NULL);
	full_argtype(ret, Nil, type1, Nil, Nil);
}

void opt4_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	full_argtype(ret, Nil, v1, Nil, Nil);
}

void opt5_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	full_argtype(ret, Nil, v1, Nil, Nil);
}

void var1opt1_argtype(addr *ret, addr var, addr opt)
{
	conscar_heap(&var, var);
	conscar_heap(&opt, opt);
	full_argtype(ret, var, opt, Nil, Nil);
}

void var1opt2_argtype(addr *ret, addr var, addr opt1, addr opt2)
{
	conscar_heap(&var, var);
	list_heap(&opt1, opt1, opt2, NULL);
	full_argtype(ret, var, opt1, Nil, Nil);
}

void var1opt2key_argtype(addr *ret, addr var, addr opt1, addr opt2, addr key)
{
	conscar_heap(&var, var);
	list_heap(&opt1, opt1, opt2, NULL);
	full_argtype(ret, var, opt1, Nil, key);
}

void var2opt1_argtype(addr *ret, addr var1, addr var2, addr opt)
{
	list_heap(&var1, var1, var2, NULL);
	conscar_heap(&opt, opt);
	full_argtype(ret, var1, opt, Nil, Nil);
}

void var2opt2_argtype(addr *ret, addr var1, addr var2, addr opt1, addr opt2)
{
	list_heap(&var1, var1, var2, NULL);
	list_heap(&opt1, opt1, opt2, NULL);
	full_argtype(ret, var1, opt1, Nil, Nil);
}

void var3opt1_argtype(addr *ret, addr var1, addr var2, addr var3, addr opt)
{
	list_heap(&var1, var1, var2, var3, NULL);
	conscar_heap(&opt, opt);
	full_argtype(ret, var1, opt, Nil, Nil);
}

void var1rest_argtype(addr *ret, addr var, addr rest)
{
	conscar_heap(&var, var);
	full_argtype(ret, var, Nil, rest, Nil);
}

void var2rest_argtype(addr *ret, addr var1, addr var2, addr rest)
{
	list_heap(&var1, var1, var2, NULL);
	full_argtype(ret, var1, Nil, rest, Nil);
}

void var3rest_argtype(addr *ret, addr var1, addr var2, addr var3, addr rest)
{
	list_heap(&var1, var1, var2, var3, NULL);
	full_argtype(ret, var1, Nil, rest, Nil);
}

void var4rest_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	full_argtype(ret, v1, Nil, rest, Nil);
}

void rest_argtype(addr *ret, addr rest)
{
	full_argtype(ret, Nil, Nil, rest, Nil);
}

void key_argtype(addr *ret, addr key)
{
	full_argtype(ret, Nil, Nil, Nil, key);
}

void result_valuestype(addr *ret, addr type)
{
	/* (values type &rest nil) */
	addr pos;
	GetCallType(&pos, Nil);
	conscar_heap(&type, type);
	type_object4(NULL, LISPDECL_VALUES, type, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void values2_valuestype(addr *ret, addr type1, addr type2)
{
	/* (values type1 type2 &rest nil) */
	addr pos;
	GetCallType(&pos, Nil);
	list_heap(&type1, type1, type2, NULL);
	type_object4(NULL, LISPDECL_VALUES, type1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void values3_valuestype(addr *ret, addr type1, addr type2, addr type3)
{
	/* (values type1 type2 type3 &rest nil) */
	addr pos;
	GetCallType(&pos, Nil);
	list_heap(&type1, type1, type2, type3, NULL);
	type_object4(NULL, LISPDECL_VALUES, type1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void values4_valuestype(addr *ret, addr type1, addr type2, addr type3, addr type4)
{
	/* (values type1 type2 type3 type4 &rest nil) */
	addr pos;
	GetCallType(&pos, Nil);
	list_heap(&type1, type1, type2, type3, type4, NULL);
	type_object4(NULL, LISPDECL_VALUES, type1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void values5_valuestype(addr *ret, addr t1, addr t2, addr t3, addr t4, addr t5)
{
	/* (values t1 t2 t3 t4 t5 &rest nil) */
	addr pos;
	GetCallType(&pos, Nil);
	list_heap(&t1, t1, t2, t3, t4, t5, NULL);
	type_object4(NULL, LISPDECL_VALUES, t1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

void rest_valuestype(addr *ret, addr type)
{
	type_object4(NULL, LISPDECL_VALUES, Nil, Nil, type, Nil, ret);
	SetStatusReadOnly(*ret);
}

void opt1conditionnull_argtype(addr *ret)
{
	addr pos;

	GetCallType(&pos, ConditionNull);
	opt1_argtype(ret, pos);
}

void char_rest_char_argtype(addr *ret)
{
	addr ctype;
	GetCallType(&ctype, Character);
	var1rest_argtype(ret, ctype, ctype);
}

static void string_one_type(addr *ret)
{
	addr pos;
	fixnum_heap(&pos, 1);
	type_object1(NULL, LISPDECL_SIMPLE_BASE_STRING, pos, ret);
	SetStatusReadOnly(*ret);
}

void character_designer_argtype(addr *ret)
{
	addr array, pos;

	/* (or character
	 *     (simple-base-string 1)
	 *     symbol)  ;; (= (length (symbol-name x)) 1)
	 */
	vector4_heap(&array, 3);
	GetCallType(&pos, Character);
	SetArrayA4(array, 0, pos);
	string_one_type(&pos);
	SetArrayA4(array, 1, pos);
	GetCallType(&pos, Symbol);
	SetArrayA4(array, 2, pos);
	type_object1(NULL, LISPDECL_OR, array, &pos);
	var1_argtype(ret, pos);
}

void restrestart_valuestype(addr *ret)
{
	addr pos;
	GetCallType(&pos, Restart);
	rest_valuestype(ret, pos);
}


/*
 *  build
 */
static void getroot_calltype(addr *ret)
{
	*ret = Root(LISPINDEX_CALLTYPE);
}

static void setroot_calltype(addr value)
{
	Root(LISPINDEX_CALLTYPE) = value;
}

static void makeroot_calltype(void)
{
	addr pos;

	vector4_heap(&pos, CallType_Size);
	setroot_calltype(pos);
}

static void setcalltype(enum CallType index, addr pos)
{
	addr table;

	getroot_calltype(&table);
	Check(GetType(table) != LISPTYPE_VECTOR, "table error");
	SetArrayA4(table, (size_t)index, pos);
	SetStatusReadOnly(pos);
}
#define SetCallType(x, y) setcalltype(CallType_##x, y)

static void calltype_asterisk(void)
{
	addr pos;
	type_asterisk_heap(&pos);
	SetCallType(Asterisk, pos);
}

static void calltype_nil(void)
{
	addr pos;
	type_nil_heap(&pos);
	SetCallType(Nil, pos);
}

static void calltype_t(void)
{
	addr pos;
	type_t_heap(&pos);
	SetCallType(T, pos);
}

static void calltype_type(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_TYPE, &pos);
	SetCallType(Type, pos);
}

static void calltype_character(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_CHARACTER, &pos);
	SetCallType(Character, pos);
}

static void calltype_symbol(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_SYMBOL, &pos);
	SetCallType(Symbol, pos);
}

static void calltype_keyword(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_KEYWORD, &pos);
	SetCallType(Keyword, pos);
}

static void calltype_null(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_NULL, &pos);
	SetCallType(Null, pos);
}

static void type_cons_carcdr(addr car, addr cdr, addr *ret)
{
	type_object2(NULL, LISPDECL_CONS, car, cdr, ret);
}

static void calltype_cons(void)
{
	addr pos, asterisk;

	GetCallType(&asterisk, Asterisk);
	type_cons_carcdr(asterisk, asterisk, &pos);
	SetCallType(Cons, pos);
}

static void calltype_cxr(void)
{
	/* (or null cons) */
	addr pos, type;
	GetCallType(&pos, Null);
	GetCallType(&type, Cons);
	type_or(NULL, pos, type, &pos);
	SetCallType(Cxr, pos);
}

static void type_cxr_carcdr(addr car, addr cdr, addr *ret)
{
	/* (or null (cons car cdr)) */
	addr pos, type;
	GetCallType(&pos, Null);
	type_cons_carcdr(car, cdr, &type);
	type_or(NULL, pos, type, ret);
}

static addr type_list_car(addr type)
{
	addr pos;
	GetCallType(&pos, Asterisk);
	type_cxr_carcdr(type, pos, &pos);
	return pos;
}

static addr type_list_cdr(addr type)
{
	addr pos;
	GetCallType(&pos, Asterisk);
	type_cxr_carcdr(pos, type, &pos);
	return pos;
}

static void calltype_car(void)
{
	addr cxr = refcalltype(CallType_Cxr);
	addr cxar = type_list_car(cxr);
	addr cxdr = type_list_cdr(cxr);
	addr cxaar = type_list_car(cxar);
	addr cxadr = type_list_cdr(cxar);
	addr cxdar = type_list_car(cxdr);
	addr cxddr = type_list_cdr(cxdr);
	addr cxaaar = type_list_car(cxaar);
	addr cxaadr = type_list_cdr(cxaar);
	addr cxadar = type_list_car(cxadr);
	addr cxaddr = type_list_cdr(cxadr);
	addr cxdaar = type_list_car(cxdar);
	addr cxdadr = type_list_cdr(cxdar);
	addr cxddar = type_list_car(cxddr);
	addr cxdddr = type_list_cdr(cxddr);
	addr fifth = type_list_cdr(cxdddr);
	addr sixth = type_list_cdr(fifth);
	addr seventh = type_list_cdr(sixth);
	addr eighth = type_list_cdr(seventh);
	addr ninth = type_list_cdr(eighth);
	addr tenth = type_list_cdr(ninth);
	SetCallType(Cxar, cxar);
	SetCallType(Cxdr, cxdr);
	SetCallType(Cxaar, cxaar);
	SetCallType(Cxadr, cxadr);
	SetCallType(Cxdar, cxdar);
	SetCallType(Cxddr, cxddr);
	SetCallType(Cxaaar, cxaaar);
	SetCallType(Cxaadr, cxaadr);
	SetCallType(Cxadar, cxadar);
	SetCallType(Cxaddr, cxaddr);
	SetCallType(Cxdaar, cxdaar);
	SetCallType(Cxdadr, cxdadr);
	SetCallType(Cxddar, cxddar);
	SetCallType(Cxdddr, cxdddr);
	SetCallType(Fifth, fifth);
	SetCallType(Sixth, sixth);
	SetCallType(Seventh, seventh);
	SetCallType(Eighth, eighth);
	SetCallType(Ninth, ninth);
	SetCallType(Tenth, tenth);
}

static addr type_cons_car(addr type)
{
	addr pos;
	GetCallType(&pos, Asterisk);
	type_cxr_carcdr(type, pos, &pos);
	return pos;
}

static addr type_cons_cdr(addr type)
{
	addr pos;
	GetCallType(&pos, Asterisk);
	type_cxr_carcdr(pos, type, &pos);
	return pos;
}

static void calltype_setf_car(void)
{
	addr cxr = refcalltype(CallType_Cons);
	addr cxar = type_cons_car(cxr);
	addr cxdr = type_cons_cdr(cxr);
	addr cxaar = type_cons_car(cxar);
	addr cxadr = type_cons_cdr(cxar);
	addr cxdar = type_cons_car(cxdr);
	addr cxddr = type_cons_cdr(cxdr);
	addr cxaaar = type_cons_car(cxaar);
	addr cxaadr = type_cons_cdr(cxaar);
	addr cxadar = type_cons_car(cxadr);
	addr cxaddr = type_cons_cdr(cxadr);
	addr cxdaar = type_cons_car(cxdar);
	addr cxdadr = type_cons_cdr(cxdar);
	addr cxddar = type_cons_car(cxddr);
	addr cxdddr = type_cons_cdr(cxddr);
	addr fifth = type_cons_cdr(cxdddr);
	addr sixth = type_cons_cdr(fifth);
	addr seventh = type_cons_cdr(sixth);
	addr eighth = type_cons_cdr(seventh);
	addr ninth = type_cons_cdr(eighth);
	addr tenth = type_cons_cdr(ninth);
	SetCallType(SetfCxar, cxar);
	SetCallType(SetfCxdr, cxdr);
	SetCallType(SetfCxaar, cxaar);
	SetCallType(SetfCxadr, cxadr);
	SetCallType(SetfCxdar, cxdar);
	SetCallType(SetfCxddr, cxddr);
	SetCallType(SetfCxaaar, cxaaar);
	SetCallType(SetfCxaadr, cxaadr);
	SetCallType(SetfCxadar, cxadar);
	SetCallType(SetfCxaddr, cxaddr);
	SetCallType(SetfCxdaar, cxdaar);
	SetCallType(SetfCxdadr, cxdadr);
	SetCallType(SetfCxddar, cxddar);
	SetCallType(SetfCxdddr, cxdddr);
	SetCallType(SetfFifth, fifth);
	SetCallType(SetfSixth, sixth);
	SetCallType(SetfSeventh, seventh);
	SetCallType(SetfEighth, eighth);
	SetCallType(SetfNinth, ninth);
	SetCallType(SetfTenth, tenth);
}

static void calltype_list(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_LIST, &pos);
	SetCallType(List, pos);
}

static void calltype_boolean(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_BOOLEAN, &pos);
	SetCallType(Boolean, pos);
}

static void calltype_string(void)
{
	addr pos;
	type_asterisk_heap(&pos);
	type_object1(NULL, LISPDECL_STRING, pos, &pos);
	SetCallType(String, pos);
}

static void calltype_stringnull(void)
{
	addr pos, type;
	GetCallType(&pos, String);
	GetCallType(&type, Null);
	type_or(NULL, pos, type, &pos);
	SetCallType(StringNull, pos);
}

static void calltype_simplestring(void)
{
	addr pos;
	type_asterisk_heap(&pos);
	type_object1(NULL, LISPDECL_SIMPLE_STRING, pos, &pos);
	SetCallType(SimpleString, pos);
}

static void calltype_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_STREAM, &pos);
	SetCallType(Stream, pos);
}

static void calltype_streamnull(void)
{
	addr type1, type2;
	GetCallType(&type1, Stream);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, &type1);
	SetCallType(StreamNull, type1);
}

static void calltype_file_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_FILE_STREAM, &pos);
	SetCallType(FileStream, pos);
}

static void calltype_synonym_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_SYNONYM_STREAM, &pos);
	SetCallType(SynonymStream, pos);
}

static void calltype_broadcast_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_BROADCAST_STREAM, &pos);
	SetCallType(BroadcastStream, pos);
}

static void calltype_two_way_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_TWO_WAY_STREAM, &pos);
	SetCallType(TwoWayStream, pos);
}

static void calltype_echo_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_ECHO_STREAM, &pos);
	SetCallType(EchoStream, pos);
}

static void calltype_concatenated_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_CONCATENATED_STREAM, &pos);
	SetCallType(ConcatenatedStream, pos);
}

static void calltype_string_stream(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_STRING_STREAM, &pos);
	SetCallType(StringStream, pos);
}

static void calltype_input_stream(void)
{
	addr pos;
	GetConst(COMMON_INPUT_STREAM_P, &pos);
	type_object1(NULL, LISPDECL_SATISFIES, pos, &pos);
	SetCallType(InputStream, pos);
}

static void calltype_output_stream(void)
{
	addr pos;
	GetConst(COMMON_OUTPUT_STREAM_P, &pos);
	type_object1(NULL, LISPDECL_SATISFIES, pos, &pos);
	SetCallType(OutputStream, pos);
}

static void calltype_function(void)
{
	addr pos;
	type_function_asterisk(NULL, &pos);
	SetCallType(Function, pos);
}

static void calltype_compiledfunction(void)
{
	addr pos;
	type_compiled_function_asterisk(NULL, &pos);
	SetCallType(CompiledFunction, pos);
}

static void calltype_package(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_PACKAGE, &pos);
	SetCallType(Package, pos);
}

static void calltype_sequence(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_SEQUENCE, &pos);
	SetCallType(Sequence, pos);
}

static void calltype_restart(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_RESTART, &pos);
	SetCallType(Restart, pos);
}

static void calltype_environment(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_ENVIRONMENT, &pos);
	SetCallType(Environment, pos);
}

static void calltype_readtable(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_READTABLE, &pos);
	SetCallType(Readtable, pos);
}

static void calltype_pathname(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_PATHNAME, &pos);
	SetCallType(Pathname, pos);
}

static void calltype_logical_pathname(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_LOGICAL_PATHNAME, &pos);
	SetCallType(LogicalPathname, pos);
}

static void calltype_hashtable(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_HASH_TABLE, &pos);
	SetCallType(Hashtable, pos);
}

static void calltype_randomstate(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_RANDOM_STATE, &pos);
	SetCallType(RandomState, pos);
}

static void calltype_integer(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_INTEGER, &pos);
	SetCallType(Integer, pos);
}

static void calltype_ratio(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_RATIO, &pos);
	SetCallType(Ratio, pos);
}

static void calltype_rational(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_RATIONAL, &pos);
	SetCallType(Rational, pos);
}

static void calltype_real(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_REAL, &pos);
	SetCallType(Real, pos);
}

static void calltype_float(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_FLOAT, &pos);
	SetCallType(Float, pos);
}

static void calltype_shortfloat(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_SHORT_FLOAT, &pos);
	SetCallType(ShortFloat, pos);
}

static void calltype_singlefloat(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_SINGLE_FLOAT, &pos);
	SetCallType(SingleFloat, pos);
}

static void calltype_doublefloat(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_DOUBLE_FLOAT, &pos);
	SetCallType(DoubleFloat, pos);
}

static void calltype_longfloat(void)
{
	addr pos;
	type_aster4(NULL, LISPDECL_LONG_FLOAT, &pos);
	SetCallType(LongFloat, pos);
}

static void calltype_number(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_NUMBER, &pos);
	SetCallType(Number, pos);
}

static void calltype_complex(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_COMPLEX, &pos);
	SetCallType(Complex, pos);
}

static void calltype_array(void)
{
	addr pos;
	type_aster2(NULL, LISPDECL_ARRAY, &pos);
	SetCallType(Array, pos);
}

static void calltype_vector(void)
{
	addr pos;
	type_aster2(NULL, LISPDECL_VECTOR, &pos);
	SetCallType(Vector, pos);
}

static void calltype_bit(void)
{
	addr pos;
	type_empty(NULL, LISPDECL_BIT, &pos);
	SetCallType(Bit, pos);
}

static void calltype_bit_vector(void)
{
	addr pos;
	type_aster1(NULL, LISPDECL_BIT_VECTOR, &pos);
	SetCallType(BitVector, pos);
}

static void calltype_simple_bit_vector(void)
{
	addr pos;
	type_aster1(NULL, LISPDECL_SIMPLE_BIT_VECTOR, &pos);
	SetCallType(SimpleBitVector, pos);
}

static void calltype_bit_array(void)
{
	addr pos, aster;

	GetCallType(&pos, Bit);
	GetCallType(&aster, Asterisk);
	type_object2(NULL, LISPDECL_ARRAY, pos, aster, &pos);
	SetCallType(BitArray, pos);
}

static void calltype_simple_bit_array(void)
{
	addr pos, aster;

	GetCallType(&pos, Bit);
	GetCallType(&aster, Asterisk);
	type_object2(NULL, LISPDECL_SIMPLE_ARRAY, pos, aster, &pos);
	SetCallType(SimpleBitArray, pos);
}

static void calltype_condition(void)
{
	addr pos;
	clos_type_constant(&pos, CONSTANT_CLOS_CONDITION);
	SetCallType(Condition, pos);
}

static void calltype_package_error(void)
{
	addr pos;
	ConditionType(&pos, PACKAGE_ERROR);
	SetCallType(PackageError, pos);
}

static void calltype_file_error(void)
{
	addr pos;
	ConditionType(&pos, FILE_ERROR);
	SetCallType(FileError, pos);
}

static void calltype_arithmetic_error(void)
{
	addr pos;
	ConditionType(&pos, ARITHMETIC_ERROR);
	SetCallType(ArithmeticError, pos);
}

static void calltype_conditionnull(void)
{
	addr type1, type2;

	GetCallType(&type1, Condition);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, &type1);
	SetCallType(ConditionNull, type1);
}

static void calltype_restartnull(void)
{
	addr type1, type2;

	GetCallType(&type1, Restart);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, &type1);
	SetCallType(RestartNull, type1);
}

static void calltype_index(void)
{
	addr pos;
	integer_index_type(&pos);
	SetCallType(Index, pos);
}

static void calltype_indexnull(void)
{
	addr type1, type2;

	GetCallType(&type1, Index);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, &type1);
	SetCallType(IndexNull, type1);
}

static void calltype_keywordstart(void)
{
	addr pos;
	GetCallType(&pos, Index);
	SetCallType(KeywordStart, pos);
}

static void calltype_keywordend(void)
{
	addr pos;
	GetCallType(&pos, IndexNull);
	SetCallType(KeywordEnd, pos);
}

static void calltype_intplus(void)
{
	addr pos;
	integer_plus_type(&pos);
	SetCallType(Intplus, pos);
}

static void calltype_intplusnull(void)
{
	addr type1, type2;

	GetCallType(&type1, Intplus);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, &type1);
	SetCallType(IntplusNull, type1);
}

static void calltype_nonnilsymbol(void)
{
	addr pos;
	nonnil_symbol_type(&pos);
	SetCallType(NonNilSymbol, pos);
}

static void calltype_stringdesigner(void)
{
	addr pos;
	string_designer_type(&pos);
	SetCallType(StringDesigner, pos);
}

static void calltype_packagedesigner(void)
{
	addr pos;
	package_designer_type(&pos);
	SetCallType(PackageDesigner, pos);
}

static void calltype_functiondesigner(void)
{
	addr pos;
	function_designer_type(&pos);
	SetCallType(FunctionDesigner, pos);
}

static void calltype_restartdesigner(void)
{
	addr pos;
	restart_designer_type(&pos);
	SetCallType(RestartDesigner, pos);
}

static void calltype_typesymbol(void)
{
	addr pos;
	typesymbol_type(&pos);
	SetCallType(TypeSymbol, pos);
}

static void calltype_typespec(void)
{
	addr pos;
	typespec_type(&pos);
	SetCallType(TypeSpec, pos);
}

static void calltype_functionnull(void)
{
	addr pos, null;

	GetCallType(&pos, Function);
	GetCallType(&null, Null);
	type_or(NULL, pos, null, &pos);
	SetCallType(FunctionNull, pos);
}

static void calltype_environmentnull(void)
{
	addr pos;
	environment_null_type(&pos);
	SetCallType(EnvironmentNull, pos);
}

static void calltype_integernull(void)
{
	addr pos, null;

	GetCallType(&pos, Integer);
	GetCallType(&null, Null);
	type_or(NULL, pos, null, &pos);
	SetCallType(IntegerNull, pos);
}

static void calltype_functionname(void)
{
	/* (or symbol (setf symbol)) */
	/* (or symbol (cons (eql setf) (cons symbol null))) */
	addr symbol, setf, pos, cons;

	/* (cons symbol null) */
	GetCallType(&symbol, Symbol);
	GetCallType(&pos, Null);
	type_cons_carcdr(symbol, pos, &cons);
	/* (cons (eql 'setf) [cons]) */
	GetConst(COMMON_SETF, &setf);
	type_object1(NULL, LISPDECL_EQL, setf, &setf);
	type_cons_carcdr(setf, cons, &pos);
	type_or(NULL, symbol, pos, &pos);
	SetCallType(FunctionName, pos);
}

static void calltype_radixinteger(void)
{
	addr pos;
	radix_integer_type(&pos);
	SetCallType(RadixInteger, pos);
}

static void calltype_floatsymbol(void)
{
	addr pos, float1, float2, float3, float4;

	/* (member short-float single-float double-float long-float) */
	GetConst(COMMON_SHORT_FLOAT, &float1);
	GetConst(COMMON_SINGLE_FLOAT, &float2);
	GetConst(COMMON_DOUBLE_FLOAT, &float3);
	GetConst(COMMON_LONG_FLOAT, &float4);
	vector4_heap(&pos, 4);
	SetArrayA4(pos, 0, float1);
	SetArrayA4(pos, 1, float2);
	SetArrayA4(pos, 2, float3);
	SetArrayA4(pos, 3, float4);
	type_object1(NULL, LISPDECL_MEMBER, pos, &pos);
	SetCallType(FloatSymbol, pos);
}

static void calltype_readtabledesigner(void)
{
	addr null, pos;

	GetCallType(&pos, Readtable);
	GetCallType(&null, Null);
	type_or(NULL, pos, null, &pos);
	SetCallType(ReadtableDesigner, pos);
}

static void calltype_pathnamedesigner(void)
{
	addr array, pos;

	/* (or pathname string stream) */
	vector4_heap(&array, 3);
	GetCallType(&pos, Pathname);
	SetArrayA4(array, 0, pos);
	GetCallType(&pos, String);
	SetArrayA4(array, 1, pos);
	GetCallType(&pos, Stream);
	SetArrayA4(array, 2, pos);
	type_object1(NULL, LISPDECL_OR, array, &pos);
	SetCallType(PathnameDesigner, pos);
}

static void calltype_streamdesigner(void)
{
	addr type1, type2;

	/* (or stream symbol) */
	GetCallType(&type1, Stream);
	GetCallType(&type2, String);
	type_or(NULL, type1, type2, &type1);
	SetCallType(StreamDesigner, type1);
}

static void calltype_eqlt(void)
{
	addr pos;
	type_object1(NULL, LISPDECL_EQL, T, &pos);
	SetCallType(EqlT, pos);
}

static void calltype_case_sensitivity(void)
{
	addr pos, sym1, sym2, sym3, sym4;

	GetConst(KEYWORD_UPCASE, &sym1);
	GetConst(KEYWORD_DOWNCASE, &sym2);
	GetConst(KEYWORD_PRESERVE, &sym3);
	GetConst(KEYWORD_INVERT, &sym4);
	list_heap(&pos, sym1, sym2, sym3, sym4, NULL);
	type_object1(NULL, LISPDECL_MEMBER, pos, &pos);
	SetCallType(CaseSensitivity, pos);
}

static void calltype_keytestlist(void)
{
	/* &key (:key      [function-designer])
	 *      (:test     [function-designer])
	 *      (:test-not [function-designer])
	 */
	addr key, test, test_not, type, list;

	GetConst(KEYWORD_KEY, &key);
	GetConst(KEYWORD_TEST, &test);
	GetConst(KEYWORD_TEST_NOT, &test_not);
	GetCallType(&type, FunctionDesigner);
	/* key */
	cons_heap(&key, key, type);
	cons_heap(&test, test, type);
	cons_heap(&test_not, test_not, type);
	list_heap(&list, key, test, test_not, NULL);
	/* result */
	SetCallType(KeyTestList, list);
}

static void calltype_rehashsize(void)
{
	addr type1, type2, type;
	/* rehash-size */
	type_intrange_left(Nil, 1, &type1);
	type_floatrange_left(T, 1.0f, &type2);
	type_or(NULL, type1, type2, &type);
	/* result */
	SetCallType(RehashSize, type);
}

static void calltype_rehashthreshold(void)
{
	addr pos;
	type_realrange_float(Nil, 0.0f, Nil, 1.0f, &pos);
	SetCallType(RehashThreshold, pos);
}

static void calltype_pathnamenull(void)
{
	addr type1, type2;
	GetCallType(&type1, Pathname);
	GetCallType(&type2, Null);
	type_or(NULL, type1, type2, &type1);
	SetCallType(PathnameNull, type1);
}

static void calltype_countkey(void)
{
	addr key, key1, key2, key3, key4, key5, key6;

	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, START, KeywordStart);
	KeyCallType(&key3, END, KeywordEnd);
	KeyCallType(&key4, KEY, FunctionDesigner);
	KeyCallType(&key5, TEST, FunctionDesigner);
	KeyCallType(&key6, TEST_NOT, FunctionDesigner);
	list_heap(&key, key1, key2, key3, key4, key5, key6, NULL);
	SetCallType(CountKey, key);
}

static void calltype_countifkey(void)
{
	addr key, key1, key2, key3, key4;

	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, START, KeywordStart);
	KeyCallType(&key3, END, KeywordEnd);
	KeyCallType(&key4, KEY, FunctionDesigner);
	list_heap(&key, key1, key2, key3, key4, NULL);
	SetCallType(CountIfKey, key);
}

static void calltype_pathnamehost(void)
{
	/* host       (or string symbol) */
	addr type1, type2;

	GetCallType(&type1, Symbol);
	GetCallType(&type2, String);
	type_or(NULL, type1, type2, &type1);
	SetCallType(PathnameHost, type1);
}

static void calltype_pathnamedevice(void)
{
	/* device     (or string symbol)  ;; (eql :unspecific)) */
	addr type;
	GetCallType(&type, PathnameHost);
	SetCallType(PathnameDevice, type);
}

static void calltype_pathnamedirectory(void)
{
	/* directory  (or cons (member :wild :unspecific)) */
	addr type, cons, wild, unspec;

	GetCallType(&cons, Cons);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, wild, unspec, NULL);
	type_or(NULL, cons, type, &type);
	SetCallType(PathnameDirectory, type);
}

static void calltype_pathnamename(void)
{
	/* name       (or string cons (member nil :wild)) */
	addr type, string, cons, wild;

	GetCallType(&string, String);
	GetCallType(&cons, Cons);
	GetConst(KEYWORD_WILD, &wild);
	type_member_heap(&type, Nil, wild, NULL);
	type_or3(NULL, string, cons, type, &type);
	SetCallType(PathnameName, type);
}

static void calltype_pathnametype(void)
{
	/* type       (or string (member nil :wild :unspecific))) */
	addr type, string, wild, unspec;

	GetCallType(&string, String);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, Nil, wild, unspec, NULL);
	type_or(NULL, string, type, &type);
	SetCallType(PathnameType, type);
}

static void calltype_pathnameversion(void)
{
	/* version    (or (integer 1 *) (member nil :wild :unspecific :newest)) */
	addr type, newest, wild, unspec, positive;

	GetConst(KEYWORD_NEWEST, &newest);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, Nil, wild, unspec, newest, NULL);
	type_intrange_left(Nil, 0, &positive);
	type_or(NULL, positive, type, &type);
	SetCallType(PathnameVersion, type);
}

static void callType_signed8(void)
{
	addr pos;
	type_signed_byte(NULL, &pos, 8);
	SetCallType(Signed8, pos);
}

static void callType_signed16(void)
{
	addr pos;
	type_signed_byte(NULL, &pos, 16);
	SetCallType(Signed16, pos);
}

static void callType_signed32(void)
{
	addr pos;
	type_signed_byte(NULL, &pos, 32);
	SetCallType(Signed32, pos);
}

#ifdef LISP_64BIT
static void callType_signed64(void)
{
	addr pos;
	type_signed_byte(NULL, &pos, 64);
	SetCallType(Signed64, pos);
}
#endif

static void callType_unsigned8(void)
{
	addr pos;
	type_unsigned_byte(NULL, &pos, 8);
	SetCallType(Unsigned8, pos);
}

static void callType_unsigned16(void)
{
	addr pos;
	type_unsigned_byte(NULL, &pos, 16);
	SetCallType(Unsigned16, pos);
}

static void callType_unsigned32(void)
{
	addr pos;
	type_unsigned_byte(NULL, &pos, 32);
	SetCallType(Unsigned32, pos);
}

#ifdef LISP_64BIT
static void callType_unsigned64(void)
{
	addr pos;
	type_unsigned_byte(NULL, &pos, 64);
	SetCallType(Unsigned64, pos);
}
#endif

static void calltype_opendirection(void)
{
	addr type, input, output, io, probe;

	GetConst(KEYWORD_INPUT, &input);
	GetConst(KEYWORD_OUTPUT, &output);
	GetConst(KEYWORD_IO, &io);
	GetConst(KEYWORD_PROBE, &probe);
	type_member_heap(&type, input, output, io, probe, NULL);
	SetCallType(OpenDirection, type);
}

static void calltype_openelementtype(void)
{
	addr type, keyword;

	GetCallType(&type, TypeSpec);
	GetConst(KEYWORD_DEFAULT, &keyword);
	type_object1(NULL, LISPDECL_EQL, keyword, &keyword);
	type_or(NULL, type, keyword, &type);
	SetCallType(OpenElementType, type);
}

static void calltype_openifexists(void)
{
	addr type, error, new_version, rename, rename_and_delete;
	addr overwrite, append, supersede;

	GetConst(KEYWORD_ERROR, &error);
	GetConst(KEYWORD_NEW_VERSION, &new_version);
	GetConst(KEYWORD_RENAME, &rename);
	GetConst(KEYWORD_RENAME_AND_DELETE, &rename_and_delete);
	GetConst(KEYWORD_OVERWRITE, &overwrite);
	GetConst(KEYWORD_APPEND, &append);
	GetConst(KEYWORD_SUPERSEDE, &supersede);
	type_member_heap(&type, error, new_version, rename, rename_and_delete,
			overwrite, append, supersede, Nil, NULL);
	SetCallType(OpenIfExists, type);
}

static void calltype_openifdoesnotexist(void)
{
	addr type, error, create;

	GetConst(KEYWORD_ERROR, &error);
	GetConst(KEYWORD_CREATE, &create);
	type_member_heap(&type, error, create, Nil, NULL);
	SetCallType(OpenIfDoesNotExist, type);
}

static void calltype_externalformat(void)
{
	addr type1, type2;

	GetCallType(&type1, Symbol);
	GetCallType(&type2, String);
	type_or(NULL, type1, type2, &type1);
	SetCallType(ExternalFormat, type1);
}

static void calltype_args_packagedesigner(void)
{
	addr pos;
	GetCallType(&pos, PackageDesigner);
	var1_argtype(&pos, pos);
	SetCallType(Args_PackageDesigner, pos);
}

static void calltype_args_pathnamecase(void)
{
	addr arg, key, symbol, common, keylocal;

	/* key */
	GetConst(KEYWORD_CASE, &symbol);
	GetConst(KEYWORD_COMMON, &common);
	GetConst(KEYWORD_LOCAL, &keylocal);
	type_member_heap(&key, common, keylocal, NULL);
	cons_heap(&key, symbol, key);
	conscar_heap(&key, key);
	/* type */
	GetCallType(&arg, PathnameDesigner);
	var1key_argtype(&arg, arg, key);
	SetCallType(Args_PathnameCase, arg);
}

static void callType_array_t(void)
{
	addr pos;
	GetCallType(&pos, T);
	SetCallType(Array_T, pos);
}

static void callType_array_bit(void)
{
	addr pos;
	GetCallType(&pos, Bit);
	SetCallType(Array_Bit, pos);
}

static void callType_array_character(void)
{
	addr pos;
	GetCallType(&pos, Character);
	SetCallType(Array_Character, pos);
}

static void callType_array_singlefloat(void)
{
	addr pos;
	GetCallType(&pos, SingleFloat);
	SetCallType(Array_SingleFloat, pos);
}

static void callType_array_doublefloat(void)
{
	addr pos;
	GetCallType(&pos, DoubleFloat);
	SetCallType(Array_DoubleFloat, pos);
}

static void callType_array_longfloat(void)
{
	addr pos;
	GetCallType(&pos, LongFloat);
	SetCallType(Array_LongFloat, pos);
}

static void calltype_values_nil(void)
{
	addr pos;
	GetCallType(&pos, Nil);
	rest_valuestype(&pos, pos);
	SetCallType(Values_Nil, pos);
}

static void calltype_values_t(void)
{
	addr pos;
	GetCallType(&pos, T);
	rest_valuestype(&pos, pos);
	SetCallType(Values_T, pos);
}

static void calltype_values_null(void)
{
	addr pos;
	GetCallType(&pos, Null);
	rest_valuestype(&pos, pos);
	SetCallType(Values_Null, pos);
}

static void calltype_values_cons(void)
{
	addr pos;
	GetCallType(&pos, Cons);
	result_valuestype(&pos, pos);
	SetCallType(Values_Cons, pos);
}

static void calltype_values_list(void)
{
	addr pos;
	GetCallType(&pos, List);
	result_valuestype(&pos, pos);
	SetCallType(Values_List, pos);
}

static void calltype_values_boolean(void)
{
	addr pos;
	GetCallType(&pos, Boolean);
	result_valuestype(&pos, pos);
	SetCallType(Values_Boolean, pos);
}

static void calltype_values_character(void)
{
	addr pos;
	GetCallType(&pos, Character);
	result_valuestype(&pos, pos);
	SetCallType(Values_Character, pos);
}

static void calltype_values_symbol(void)
{
	addr pos;
	GetCallType(&pos, Symbol);
	result_valuestype(&pos, pos);
	SetCallType(Values_Symbol, pos);
}

static void calltype_values_string(void)
{
	addr pos;
	GetCallType(&pos, String);
	result_valuestype(&pos, pos);
	SetCallType(Values_String, pos);
}

static void calltype_values_stringnull(void)
{
	addr pos;
	GetCallType(&pos, String);
	result_valuestype(&pos, pos);
	SetCallType(Values_StringNull, pos);
}

static void calltype_values_simplestring(void)
{
	addr pos;
	GetCallType(&pos, SimpleString);
	result_valuestype(&pos, pos);
	SetCallType(Values_SimpleString, pos);
}

static void calltype_values_stream(void)
{
	addr pos;
	GetCallType(&pos, Stream);
	result_valuestype(&pos, pos);
	SetCallType(Values_Stream, pos);
}

static void calltype_values_streamnull(void)
{
	addr pos;
	GetCallType(&pos, StreamNull);
	result_valuestype(&pos, pos);
	SetCallType(Values_StreamNull, pos);
}

static void calltype_values_function(void)
{
	addr pos;
	GetCallType(&pos, Function);
	result_valuestype(&pos, pos);
	SetCallType(Values_Function, pos);
}

static void calltype_values_eqlt(void)
{
	addr pos;
	GetCallType(&pos, EqlT);
	result_valuestype(&pos, pos);
	SetCallType(Values_EqlT, pos);
}

static void calltype_values_package(void)
{
	addr pos;
	GetCallType(&pos, Package);
	result_valuestype(&pos, pos);
	SetCallType(Values_Package, pos);
}

static void calltype_values_sequence(void)
{
	addr pos;
	GetCallType(&pos, Sequence);
	result_valuestype(&pos, pos);
	SetCallType(Values_Sequence, pos);
}

static void calltype_values_array(void)
{
	addr pos;
	GetCallType(&pos, Array);
	result_valuestype(&pos, pos);
	SetCallType(Values_Array, pos);
}

static void calltype_values_integer(void)
{
	addr pos;
	GetCallType(&pos, Integer);
	result_valuestype(&pos, pos);
	SetCallType(Values_Integer, pos);
}

static void calltype_values_ratio(void)
{
	addr pos;
	GetCallType(&pos, Ratio);
	result_valuestype(&pos, pos);
	SetCallType(Values_Ratio, pos);
}

static void calltype_values_rational(void)
{
	addr pos;
	GetCallType(&pos, Rational);
	result_valuestype(&pos, pos);
	SetCallType(Values_Rational, pos);
}

static void calltype_values_index(void)
{
	addr pos;
	GetCallType(&pos, Index);
	result_valuestype(&pos, pos);
	SetCallType(Values_Index, pos);
}

static void calltype_values_indexnull(void)
{
	addr pos;
	GetCallType(&pos, IndexNull);
	result_valuestype(&pos, pos);
	SetCallType(Values_IndexNull, pos);
}

static void calltype_values_intplus(void)
{
	addr pos;
	GetCallType(&pos, Intplus);
	result_valuestype(&pos, pos);
	SetCallType(Values_Intplus, pos);
}

static void calltype_values_intplusnull(void)
{
	addr pos;
	GetCallType(&pos, IntplusNull);
	result_valuestype(&pos, pos);
	SetCallType(Values_IntplusNull, pos);
}

static void calltype_values_bit(void)
{
	addr pos;
	GetCallType(&pos, Bit);
	result_valuestype(&pos, pos);
	SetCallType(Values_Bit, pos);
}

static void calltype_values_bitarray(void)
{
	addr pos;
	GetCallType(&pos, BitArray);
	result_valuestype(&pos, pos);
	SetCallType(Values_BitArray, pos);
}

static void calltype_values_pathname(void)
{
	addr pos;
	GetCallType(&pos, Pathname);
	result_valuestype(&pos, pos);
	SetCallType(Values_Pathname, pos);
}

static void calltype_values_pathnamenull(void)
{
	addr pos;
	GetCallType(&pos, PathnameNull);
	result_valuestype(&pos, pos);
	SetCallType(Values_PathnameNull, pos);
}

static void calltype_values_logical_pathname(void)
{
	addr pos;
	GetCallType(&pos, LogicalPathname);
	result_valuestype(&pos, pos);
	SetCallType(Values_LogicalPathname, pos);
}

static void calltype_values_float(void)
{
	addr pos;
	GetCallType(&pos, Float);
	result_valuestype(&pos, pos);
	SetCallType(Values_Float, pos);
}

static void calltype_values_real(void)
{
	addr pos;
	GetCallType(&pos, Real);
	result_valuestype(&pos, pos);
	SetCallType(Values_Real, pos);
}

static void calltype_values_number(void)
{
	addr pos;
	GetCallType(&pos, Number);
	result_valuestype(&pos, pos);
	SetCallType(Values_Number, pos);
}

static void calltype_values_complex(void)
{
	addr pos;
	GetCallType(&pos, Complex);
	result_valuestype(&pos, pos);
	SetCallType(Values_Complex, pos);
}

static void calltype_compiled_object_boolean(void)
{
	addr arg, values;

	GetCallType(&arg, Asterisk);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Object_Boolean, arg);
}

static void calltype_compiled_symbol_boolean(void)
{
	addr arg, values;

	GetCallType(&arg, Symbol);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Symbol_Boolean, arg);
}

static void calltype_compiled_stringcase(void)
{
	/* (function (string-designer &key (start keyword-start)
	 *                                 (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr arg, values, start, end, symbol, type;
	addr key1, key2;

	GetCallType(&start, KeywordStart);
	GetCallType(&end, KeywordEnd);
	/* :start1 */
	GetConst(KEYWORD_START, &symbol);
	cons_heap(&key1, symbol, start);
	/* :end1 */
	GetConst(KEYWORD_END, &symbol);
	cons_heap(&key2, symbol, end);
	/* &key ... */
	list_heap(&arg, key1, key2, NULL);
	GetCallType(&type, StringDesigner);
	var1key_argtype(&arg, type, arg);
	/* result */
	GetCallType(&values, Values_String);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_StringCase, arg);
}

static void calltype_compiled_nstringcase(void)
{
	/* (function (string &key (start keyword-start)
	 *                        (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr arg, values, start, end, symbol, type;
	addr key1, key2;

	GetCallType(&start, KeywordStart);
	GetCallType(&end, KeywordEnd);
	/* :start1 */
	GetConst(KEYWORD_START, &symbol);
	cons_heap(&key1, symbol, start);
	/* :end1 */
	GetConst(KEYWORD_END, &symbol);
	cons_heap(&key2, symbol, end);
	/* &key ... */
	list_heap(&arg, key1, key2, NULL);
	GetCallType(&type, String);
	var1key_argtype(&arg, type, arg);
	/* result */
	GetCallType(&values, Values_String);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_NStringCase, arg);
}

static void calltype_compiled_stringtrim(void)
{
	/* (function (sequence string-designer) (values string &rest nil)) */
	addr arg, values, type;

	GetCallType(&arg, Sequence);
	GetCallType(&type, StringDesigner);
	var2_argtype(&arg, arg, type);
	GetCallType(&values, Values_String);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_StringTrim, arg);
}

static void calltype_compiled_stringequal(void)
{
	/* (function (string-designer string-designer &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values boolean &rest null))
	 */
	addr arg, values, start, end, symbol, type;
	addr key1, key2, key3, key4;

	GetCallType(&start, KeywordStart);
	GetCallType(&end, KeywordEnd);
	/* :start1 */
	GetConst(KEYWORD_START1, &symbol);
	cons_heap(&key1, symbol, start);
	/* :end1 */
	GetConst(KEYWORD_END1, &symbol);
	cons_heap(&key2, symbol, end);
	/* :start2 */
	GetConst(KEYWORD_START2, &symbol);
	cons_heap(&key3, symbol, start);
	/* :end2 */
	GetConst(KEYWORD_END2, &symbol);
	cons_heap(&key4, symbol, end);
	/* &key ... */
	list_heap(&arg, key1, key2, key3, key4, NULL);
	GetCallType(&type, StringDesigner);
	var2key_argtype(&arg, type, type, arg);
	/* result */
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_StringEqual, arg);
}

static void calltype_compiled_stringmismatch(void)
{
	/* (function (string-designer string-designer &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values keyword-end &rest null))
	 */
	addr arg, values, start, end, symbol, type;
	addr key1, key2, key3, key4;

	GetCallType(&start, KeywordStart);
	GetCallType(&end, KeywordEnd);
	/* :start1 */
	GetConst(KEYWORD_START1, &symbol);
	cons_heap(&key1, symbol, start);
	/* :end1 */
	GetConst(KEYWORD_END1, &symbol);
	cons_heap(&key2, symbol, end);
	/* :start2 */
	GetConst(KEYWORD_START2, &symbol);
	cons_heap(&key3, symbol, start);
	/* :end2 */
	GetConst(KEYWORD_END2, &symbol);
	cons_heap(&key4, symbol, end);
	/* &key ... */
	list_heap(&arg, key1, key2, key3, key4, NULL);
	GetCallType(&type, StringDesigner);
	var2key_argtype(&arg, type, type, arg);
	/* result */
	result_valuestype(&values, end);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_StringMismatch, arg);
}

static void calltype_compiled_rplaca(void)
{
	/* (function (cons *) (values cons &rest nil)) */
	addr arg, values, type;

	GetCallType(&arg, Cons);
	GetCallType(&type, Asterisk);
	var2_argtype(&arg, arg, type);
	GetCallType(&values, Values_Cons);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Rplaca, arg);
}

static void calltype_compiled_list_list(void)
{
	/* (function (list) (values list &rest nil)) */
	addr arg, values;

	GetCallType(&arg, List);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_List_List, arg);
}

static void calltype_compiled_nth(void)
{
	/* (function (integer-plus list) (values t &rest nil)) */
	addr arg, values, type;

	GetCallType(&arg, Intplus);
	GetCallType(&type, List);
	var2_argtype(&arg, arg, type);
	GetCallType(&values, Values_T);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Nth, arg);
}

static void calltype_compiled_nconc(void)
{
	/* (function (&rest t) (values t &rest nil)) */
	addr arg, values;

	GetCallType(&arg, T);
	rest_argtype(&arg, arg);
	GetCallType(&values, Values_T);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Nconc, arg);
}

static void calltype_compiled_renconc(void)
{
	/* (function (list t) (values t &rest nil)) */
	addr arg, values, type;

	GetCallType(&arg, List);
	GetCallType(&type, T);
	var2_argtype(&arg, arg, type);
	GetCallType(&values, Values_T);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Nreconc, arg);
}

static void calltype_compiled_butlast(void)
{
	/* (function (list &optional index) (values list &rest nil)) */
	addr arg, values, type;

	GetCallType(&arg, List);
	GetCallType(&type, Index);
	var1opt1_argtype(&arg, arg, type);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_ButLast, arg);
}

static void calltype_compiled_macrofunction(void)
{
	/* (function (t (or null environment)) t) */
	addr arg, values, env;

	GetCallType(&arg, T);
	GetCallType(&env, EnvironmentNull);
	var2_argtype(&arg, arg, env);
	GetCallType(&values, Asterisk);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_MacroFunction, arg);
}

static void calltype_compiled_macroexpand(void)
{
	/* (function (t &optional (or null environment))
	 *           (values t boolean &rest nil))
	 */
	addr arg, values, pos1, pos2;

	GetCallType(&pos1, T);
	GetCallType(&pos2, EnvironmentNull);
	var1opt1_argtype(&arg, pos1, pos2);
	GetCallType(&pos2, Boolean);
	values2_valuestype(&values, pos1, pos2);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_MacroExpand, arg);
}

static void calltype_compiled_abort(void)
{
	addr arg, values;

	/* (function (&optional (or condition null)) nil) */
	opt1conditionnull_argtype(&arg);
	GetCallType(&values, Nil);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Abort, arg);
}

static void calltype_compiled_continue(void)
{
	addr arg, values;

	/* (function (&optional (or condition null)) null) */
	opt1conditionnull_argtype(&arg);
	GetCallType(&values, Values_Null);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Continue, arg);
}

static void calltype_compiled_macroreader(void)
{
	/* (function (stream character) *) */
	addr arg, values, type;

	GetCallType(&arg, Stream);
	GetCallType(&type, Character);
	var2_argtype(&arg, arg, type);
	GetCallType(&values, Asterisk);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_MacroReader, arg);
}

static void calltype_compiled_macrodispatch(void)
{
	/* (function (stream character intplus-null) *) */
	addr arg, values, type, intplus;

	GetCallType(&arg, Stream);
	GetCallType(&type, Character);
	GetCallType(&intplus, IntplusNull);
	var3_argtype(&arg, arg, type, intplus);
	GetCallType(&values, Asterisk);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_MacroDispatch, arg);
}

static void calltype_compiled_read(void)
{
	addr arg, values, type;

	/* (function (&optional stream t t t) (values t &rest nil)) */
	GetCallType(&arg, Stream);
	GetCallType(&type, T);
	list_heap(&arg, arg, type, type, type, NULL);
	full_argtype(&arg, Nil, arg, Nil, Nil);
	result_valuestype(&values, type);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Read, arg);
}

static void calltype_compiled_sublis(void)
{
	/* (function
	 *   (list list
	 *    &key (:key      (or (function (t &rest t) *)   symbol))
	 *         (:test     (or (function (t t &rest t) *) symbol))
	 *         (:test-not (or (function (t t &rest t) *) symbol)))
	 *   (values list &rest nil))
	 */
	addr arg, values, list;

	GetCallType(&list, List);
	GetCallType(&arg, KeyTestList);
	var2key_argtype(&arg, list, list, arg);
	result_valuestype(&values, list);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Sublis, arg);
}

static void calltype_compiled_subst(void)
{
	/* (function
	 *   (t t list
	 *    &key (:key      (or (function (t &rest t) *)   symbol))
	 *         (:test     (or (function (t t &rest t) *) symbol))
	 *         (:test-not (or (function (t t &rest t) *) symbol)))
	 *   (values list &rest nil))
	 */
	addr arg, values, type, list;

	GetCallType(&type, T);
	GetCallType(&list, List);
	GetCallType(&arg, KeyTestList);
	var3key_argtype(&arg, type, type, list, arg);
	result_valuestype(&values, list);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Subst, arg);
}

static void calltype_compiled_subst_if(void)
{
	/* (function
	 *   (t (or (function (t &rest t) *) symbol) list
	 *    &key (:key (or (function (t &rest t) *) symbol)))
	 *   (values list &rest nil))
	 */
	addr arg, values, type, call, list, key;

	GetCallType(&type, T);
	GetCallType(&call, FunctionDesigner);
	GetCallType(&list, List);
	GetConst(KEYWORD_KEY, &key);
	cons_heap(&key, key, call);
	conscar_heap(&key, key);
	var3key_argtype(&arg, type, call, list, key);
	result_valuestype(&values, list);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_SubstIf, arg);
}

static void calltype_compiled_eq(void)
{
	/* (function (t t) boolean) */
	addr arg, values;

	GetCallType(&arg, T);
	var2_argtype(&arg, arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Eq, arg);
}


static void calltype_compiled_every(void)
{
	/* (function (function-designer sequence &rest sequence)
	 *   (values boolean &rest nil))
	 */
	addr arg, values, call, sequence;

	GetCallType(&call, FunctionDesigner);
	GetCallType(&sequence, Sequence);
	var2rest_argtype(&arg, call, sequence, sequence);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Every, arg);
}

static void calltype_compiled_upgraded(void)
{
	/* (function (typespec &optional environment)
	 *   (values [typesymbol] &rest nil))
	 */
	addr arg, values, type;

	GetCallType(&arg, TypeSpec);
	GetCallType(&type, Environment);
	var1opt1_argtype(&arg, arg, type);
	GetCallType(&values, TypeSymbol);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Upgraded, arg);
}

static void calltype_compiled_number_equal(void)
{
	/* (function (number &rest number) (values boolean &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Number);
	var1rest_argtype(&arg, arg, arg);
	GetCallType(&values, Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Number_Equal, arg);
}

static void calltype_compiled_number_compare(void)
{
	/* (function (real &rest real) (values boolean &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Real);
	var1rest_argtype(&arg, arg, arg);
	GetCallType(&values, Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Number_Compare, arg);
}

static void calltype_compiled_max(void)
{
	/* (function (real &rest real) (values real &rest nil)) */
	addr arg, values;

	GetCallType(&values, Real);
	var1rest_argtype(&arg, values, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Max, arg);
}

static void calltype_compiled_minusp(void)
{
	/* (function (real) (values boolean &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Real);
	var1_argtype(&arg, arg);
	GetCallType(&values, Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Minusp, arg);
}

static void calltype_compiled_zerop(void)
{
	/* (function (number) (values boolean &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Number);
	var1_argtype(&arg, arg);
	GetCallType(&values, Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Zerop, arg);
}

static void calltype_compiled_plus(void)
{
	/* (function (&rest number) (values number &rest nil)) */
	addr arg, values;

	GetCallType(&values, Number);
	rest_argtype(&arg, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Plus, arg);
}

static void calltype_compiled_minus(void)
{
	/* (function (number &rest number) (values number &rest nil)) */
	addr arg, values;

	GetCallType(&values, Number);
	var1rest_argtype(&arg, values, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Minus, arg);
}

static void calltype_compiled_oneplus(void)
{
	/* (function (number) (values number &rest nil)) */
	addr arg, values;

	GetCallType(&values, Number);
	var1_argtype(&arg, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_OnePlus, arg);
}

static void calltype_compiled_hashtablecount(void)
{
	/* (function (hash-table) (values Index &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Hashtable);
	var1_argtype(&arg, arg);
	GetCallType(&values, Index);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_HashTableCount, arg);
}

static void calltype_compiled_evenp(void)
{
	/* (function (integer) (values boolean &res nil) */
	addr arg, values;

	GetCallType(&arg, Integer);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Evenp, arg);
}

static void calltype_compiled_export(void)
{
	/* (function
	 *   ((or list symbol) &optional package-designer)
	 *   (values (eql t) &rest nil))
	 */
	addr arg, values, type1, type2;

	GetCallType(&type1, List);
	GetCallType(&type2, Symbol);
	type_or(NULL, type1, type2, &arg);
	GetCallType(&values, PackageDesigner);
	var1opt1_argtype(&arg, arg, values);
	GetCallType(&values, Values_EqlT);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Export, arg);
}

static void calltype_compiled_usepackage(void)
{
	/*  (function
	 *    ((or list package-designer) &optional package-designer)
	 *    (values (eql t) &rest nil))
	 */
	addr arg, values;

	GetCallType(&arg, List);
	GetCallType(&values, PackageDesigner);
	type_or(NULL, arg, values, &arg);
	var1opt1_argtype(&arg, arg, values);
	GetCallType(&values, Values_EqlT);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_UsePackage, arg);
}

static void calltype_compiled_intern(void)
{
	/*  (function
	 *    (string &optional package-designer)
	 *    (values symbol (member :internal :external :inherited nil) &rest nil))
	 */
	addr arg, values, symbol, key1, key2, key3, status;

	/* arg */
	GetCallType(&arg, String);
	GetCallType(&values, PackageDesigner);
	var1opt1_argtype(&arg, arg, values);
	/* values */
	GetCallType(&symbol, Symbol);
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	type_member_heap(&status, key1, key2, key3, Nil, NULL);
	values2_valuestype(&values, symbol, status);
	/* result */
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Intern, arg);
}

static void calltype_compiled_packagenicknames(void)
{
	/*  (function
	 *    (package-designer)
	 *    (values list &rest nil))
	 */
	addr arg, values;

	GetCallType(&arg, Args_PackageDesigner);
	GetCallType(&values, List);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_PackageNicknames, arg);
}

static void calltype_compiled_prin1(void)
{
	/*  (function
	 *    (t &optional stream)
	 *    (values t &rest nil))
	 */
	addr arg, values;

	GetCallType(&arg, T);
	GetCallType(&values, Stream);
	var1opt1_argtype(&arg, arg, values);
	GetCallType(&values, Values_T);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Prin1, arg);
}


static void calltype_compiled_prin1tostring(void)
{
	/* (function (t) (values string &rest nil)) */
	addr arg, values;

	GetCallType(&arg, T);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_String);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Prin1ToString, arg);
}

static void calltype_compiled_reverse(void)
{
	/* (function (sequence) (values sequence &rest nil)) */
	addr arg, values;

	GetCallType(&values, Sequence);
	var1_argtype(&arg, values);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Reverse, arg);
}

static void calltype_compiled_member(void)
{
	/* (function (t list &key [KeyTestList]) (values list &rest nil)) */
	addr arg, values, key;

	GetCallType(&arg, T);
	GetCallType(&values, List);
	GetCallType(&key, KeyTestList);
	var2key_argtype(&arg, arg, values, key);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Member, arg);
}

static void calltype_compiled_memberif(void)
{
	/* (function (function-designer list &key (key function-designer))
	 *           (values list &rest nil))
	 */
	addr arg, values, type, key;

	GetCallType(&type, FunctionDesigner);
	GetCallType(&arg, List);
	GetConst(KEYWORD_KEY, &key);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	var2key_argtype(&arg, type, arg, key);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_MemberIf, arg);
}

static void calltype_compiled_mapc(void)
{
	/* (function (function-designer list &rest list)
	 *           (values list &rest nil))
	 */
	addr arg, values, type;

	GetCallType(&type, FunctionDesigner);
	GetCallType(&arg, List);
	var2rest_argtype(&arg, type, arg, arg);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Mapc, arg);
}

static void calltype_compiled_acons(void)
{
	/* (function (t t list) (values list &rest nil)) */
	addr arg, values, type;

	GetCallType(&arg, T);
	GetCallType(&type, List);
	var3_argtype(&arg, arg, arg, type);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Acons, arg);
}

static void calltype_compiled_intersection(void)
{
	/* (function (list list &key key test test-not) (values list &rest nil)) */
	addr arg, values;

	GetCallType(&arg, List);
	GetCallType(&values, KeyTestList);
	var2key_argtype(&arg, arg, arg, values);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Intersection, arg);
}


static void calltype_compiled_ecaseerror(void)
{
	/* (function (T list) (values &rest nil)) */
	addr arg, values;

	GetCallType(&arg, T);
	GetCallType(&values, List);
	var2_argtype(&arg, arg, values);
	GetCallType(&values, Values_Nil);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_EcaseError, arg);
}

static void calltype_compiled_dosymbols(void)
{
	/* (function (function package-designer) (values &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Function);
	GetCallType(&values, PackageDesigner);
	var2_argtype(&arg, arg, values);
	GetCallType(&values, Values_Nil);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_DoSymbols, arg);
}

static void calltype_compiled_arrayboolean(void)
{
	/* (function (array) (values boolean &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Array);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_ArrayBoolean, arg);
}

static void calltype_compiled_arrayindex(void)
{
	/* (function (array) (values index &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Array);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Index);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_ArrayIndex, arg);
}

static void calltype_compiled_bitand(void)
{
	/* (function (bit-array bit-array
	 *     &optional (or boolean bit-array))
	 *   (values bit-array &rest nil))
	 */
	addr arg, values;

	GetCallType(&arg, BitArray);
	GetCallType(&values, Boolean);
	type_or(NULL, arg, values, &values);
	var2opt1_argtype(&arg, arg, arg, values);
	GetCallType(&values, Values_BitArray);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_BitAnd, arg);
}

static void calltype_compiled_countif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values index &rest nil))
	 */
	addr arg, values, key;

	GetCallType(&arg, FunctionDesigner);
	GetCallType(&values, Sequence);
	GetCallType(&key, CountIfKey);
	var2key_argtype(&arg, arg, values, key);
	GetCallType(&values, Values_Index);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_CountIf, arg);
}

static void calltype_compiled_sort(void)
{
	/* (function (sequence call &key key) (values sequence &rest nil)) */
	addr arg, values, type, key;

	/* key */
	GetConst(KEYWORD_KEY, &key);
	GetCallType(&type, FunctionDesigner);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	/* type */
	GetCallType(&arg, Sequence);
	var2key_argtype(&arg, arg, type, key);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Sort, arg);
}

static void calltype_compiled_findif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values t &rest nil))
	 */
	addr arg, values, key;

	GetCallType(&arg, FunctionDesigner);
	GetCallType(&values, Sequence);
	GetCallType(&key, CountIfKey);
	var2key_argtype(&arg, arg, values, key);
	GetCallType(&values, Values_T);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_FindIf, arg);
}

static void calltype_compiled_positionif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values index-null &rest nil))
	 */
	addr arg, values, key;

	GetCallType(&arg, FunctionDesigner);
	GetCallType(&values, Sequence);
	GetCallType(&key, CountIfKey);
	var2key_argtype(&arg, arg, values, key);
	GetCallType(&values, Values_IndexNull);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_PositionIf, arg);
}

static void calltype_compiled_search(void)
{
	/* (function (sequence1 sequence2
	 *     &key from-end test test-not key start1 start2 end1 end2)
	 *     (values index-null &rest nil))
	 */
	addr arg, values, key;
	addr key1, key2, key3, key4, key5, key6, key7, key8;

	/* key */
	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, TEST, FunctionDesigner);
	KeyCallType(&key3, TEST_NOT, FunctionDesigner);
	KeyCallType(&key4, KEY, FunctionDesigner);
	KeyCallType(&key5, START1, KeywordStart);
	KeyCallType(&key6, START2, KeywordStart);
	KeyCallType(&key7, END1, KeywordEnd);
	KeyCallType(&key8, END2, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);

	/* type */
	GetCallType(&arg, Sequence);
	var2key_argtype(&arg, arg, arg, key);
	GetCallType(&values, Values_IndexNull);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Search, arg);
}

static void calltype_compiled_substitute(void)
{
	/* (function (t t sequence
	 *     &key from-end test test-not key start end count)
	 *     (values sequence &rest nil))
	 */
	addr arg, values, key;
	addr key1, key2, key3, key4, key5, key6, key7;

	/* key */
	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, TEST, FunctionDesigner);
	KeyCallType(&key3, TEST_NOT, FunctionDesigner);
	KeyCallType(&key4, KEY, FunctionDesigner);
	KeyCallType(&key5, START, KeywordStart);
	KeyCallType(&key6, END, KeywordEnd);
	KeyCallType(&key7, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);

	/* type */
	GetCallType(&arg, T);
	GetCallType(&values, Sequence);
	var3key_argtype(&arg, arg, arg, values, key);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Substitute, arg);
}

static void calltype_compiled_substituteif(void)
{
	/* (function (t call sequence
	 *     &key from-end key start end count)
	 *     (values sequence &rest nil))
	 */
	addr arg, values, type, key;
	addr key1, key2, key3, key4, key5;

	/* key */
	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, KEY, FunctionDesigner);
	KeyCallType(&key3, START, KeywordStart);
	KeyCallType(&key4, END, KeywordEnd);
	KeyCallType(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetCallType(&arg, T);
	GetCallType(&values, FunctionDesigner);
	GetCallType(&type, Sequence);
	var3key_argtype(&arg, arg, values, type, key);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_SubstituteIf, arg);
}

static void calltype_compiled_remove(void)
{
	/* (function (t sequence
	 *     &key from-end test test-not key start end count)
	 *     (values sequence &rest nil))
	 */
	addr arg, values, key;
	addr key1, key2, key3, key4, key5, key6, key7;

	/* key */
	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, TEST, FunctionDesigner);
	KeyCallType(&key3, TEST_NOT, FunctionDesigner);
	KeyCallType(&key4, KEY, FunctionDesigner);
	KeyCallType(&key5, START, KeywordStart);
	KeyCallType(&key6, END, KeywordEnd);
	KeyCallType(&key7, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);

	/* type */
	GetCallType(&arg, T);
	GetCallType(&values, Sequence);
	var2key_argtype(&arg, arg, values, key);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Remove, arg);
}

static void calltype_compiled_removeif(void)
{
	/* (function (call sequence
	 *     &key from-end key start end count)
	 *     (values sequence &rest nil))
	 */
	addr arg, values, key;
	addr key1, key2, key3, key4, key5;

	/* key */
	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, KEY, FunctionDesigner);
	KeyCallType(&key3, START, KeywordStart);
	KeyCallType(&key4, END, KeywordEnd);
	KeyCallType(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetCallType(&arg, FunctionDesigner);
	GetCallType(&values, Sequence);
	var2key_argtype(&arg, arg, values, key);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_RemoveIf, arg);
}

static void calltype_compiled_removeduplicates(void)
{
	/* (function (sequence
	 *     &key from-end test test-not start end key)
	 *     (values sequence &rest nil))
	 */
	addr arg, values, key;
	addr key1, key2, key3, key4, key5, key6;

	/* key */
	KeyCallType(&key1, FROM_END, T);
	KeyCallType(&key2, TEST, FunctionDesigner);
	KeyCallType(&key3, TEST_NOT, FunctionDesigner);
	KeyCallType(&key4, KEY, FunctionDesigner);
	KeyCallType(&key5, START, KeywordStart);
	KeyCallType(&key6, END, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetCallType(&arg, Sequence);
	var1key_argtype(&arg, arg, key);
	GetCallType(&values, Values_Sequence);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_RemoveDuplicates, arg);
}

static void calltype_compiled_namestring(void)
{
	/* (function (pathname-designer) (values string &rest nil)) */
	addr arg, values;

	GetCallType(&arg, PathnameDesigner);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_StringNull);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Namestring, arg);
}

static void calltype_compiled_pathname(void)
{
	/* (function (pathname-designer) (values pathname &rest nil)) */
	addr arg, values;

	GetCallType(&arg, PathnameDesigner);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Pathname);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Pathname, arg);
}

static void calltype_compiled_inputstreamp(void)
{
	/* (function (stream) (values boolean &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Stream);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_InputStreamP, arg);
}

static void calltype_compiled_exit(void)
{
	/* (function (&optional Intplus) (values &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Intplus);
	opt1_argtype(&arg, arg);
	GetCallType(&values, Values_Nil);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Exit, arg);
}

static void calltype_compiled_readchar(void)
{
	/* (function (&optional stream-designer t t t) (values t &rest nil)) */
	addr arg, values;

	GetCallType(&arg, StreamDesigner);
	GetCallType(&values, T);
	opt4_argtype(&arg, arg, values, values, values);
	GetCallType(&values, Values_T);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_ReadChar, arg);
}

static void calltype_compiled_writestring(void)
{
	/* (function (string &optional stream-designer &key start end)
	 *           (values string &rest nil))
	 */
	addr arg, values, var, opt, key, key1, key2;

	/* var */
	GetCallType(&var, String);
	conscar_heap(&var, var);
	/* opt */
	GetCallType(&opt, StreamDesigner);
	conscar_heap(&opt, opt);
	/* key */
	KeyCallType(&key1, START, KeywordStart);
	KeyCallType(&key2, END, KeywordEnd);
	list_heap(&key, key1, key2, NULL);
	/* arg */
	full_argtype(&arg, var, opt, Nil, key);
	/* values */
	GetCallType(&values, Values_String);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_WriteString, arg);
}

static void calltype_compiled_finishoutput(void)
{
	/* (function (&optional output-stream) (values null &rest nil)) */
	addr arg, values;

	GetCallType(&arg, OutputStream);
	opt1_argtype(&arg, arg);
	GetCallType(&values, Values_Null);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_FinishOutput, arg);
}

static void calltype_compiled_yesornop(void)
{
	/* (function (&optional (or string null) &rest t)
	 *           (values boolean &rest nil))
	 */
	addr arg, values;

	GetCallType(&arg, StringNull);
	conscar_heap(&arg, arg);
	GetCallType(&values, T);
	full_argtype(&arg, Nil, arg, values, Nil);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_YesOrNoP, arg);
}

static void calltype_compiled_floor(void)
{
	/* (function (real &optional real) (values integer real &rest nil) */
	addr arg, values, type;

	GetCallType(&type, Real);
	var1opt1_argtype(&arg, type, type);
	GetCallType(&values, Integer);
	values2_valuestype(&values, values, type);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Floor, arg);
}

static void calltype_compiled_ffloor(void)
{
	/* (function (real &optional real) (values real real &rest nil) */
	addr arg, values, type;

	GetCallType(&type, Real);
	var1opt1_argtype(&arg, type, type);
	values2_valuestype(&values, type, type);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Ffloor, arg);
}

static void calltype_compiled_envinfo(void)
{
	/* (function () (values (or string null) &rest nil)) */
	addr arg, values;

	empty_argtype(&arg);
	GetCallType(&values, Values_StringNull);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_EnvInfo, arg);
}

static void calltype_compiled_sin(void)
{
	/* (function (number) (values number &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Number);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Number);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Sin, arg);
}

static void calltype_compiled_realpart(void)
{
	/* (function (number) (values real &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Number);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Real);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_RealPart, arg);
}

static void calltype_compiled_gcd(void)
{
	/* (function (&rest integer) (values (integer 0 *) &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Integer);
	rest_argtype(&arg, arg);
	integer_plus_type(&values);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Gcd, arg);
}

static void calltype_compiled_mod(void)
{
	/* (function (real real) (values real &rest nil)) */
	addr arg, values;

	GetCallType(&arg, Real);
	var2_argtype(&arg, arg, arg);
	GetCallType(&values, Values_Real);
	type_compiled_heap(arg, values, &arg);
	SetCallType(Compiled_Mod, arg);
}


/*
 *  Interface
 */
addr refcalltype(enum CallType index)
{
	addr pos;
	getcalltype(&pos, index);
	return pos;
}

void getcalltype(addr *ret, enum CallType index)
{
	addr pos;

	getroot_calltype(&pos);
	Check(GetType(pos) != LISPTYPE_VECTOR, "table error");
	GetArrayA4(pos, (size_t)index, ret);
	Check(*ret == Nil, "type error");
}

void build_calltype(void)
{
	makeroot_calltype();
	calltype_asterisk();
	calltype_nil();
	calltype_t();
	calltype_type();
	calltype_character();
	calltype_symbol();
	calltype_keyword();
	calltype_null();
	calltype_cons();
	calltype_cxr();
	calltype_car();
	calltype_setf_car();
	calltype_list();
	calltype_boolean();
	calltype_string();
	calltype_stringnull();
	calltype_simplestring();
	calltype_stream();
	calltype_streamnull();
	calltype_file_stream();
	calltype_synonym_stream();
	calltype_broadcast_stream();
	calltype_two_way_stream();
	calltype_echo_stream();
	calltype_concatenated_stream();
	calltype_string_stream();
	calltype_input_stream();
	calltype_output_stream();
	calltype_function();
	calltype_compiledfunction();
	calltype_package();
	calltype_sequence();
	calltype_restart();
	calltype_environment();
	calltype_readtable();
	calltype_pathname();
	calltype_logical_pathname();
	calltype_hashtable();
	calltype_randomstate();
	calltype_integer();
	calltype_ratio();
	calltype_rational();
	calltype_real();
	calltype_float();
	calltype_shortfloat();
	calltype_singlefloat();
	calltype_doublefloat();
	calltype_longfloat();
	calltype_number();
	calltype_complex();
	calltype_array();
	calltype_vector();
	calltype_bit();
	calltype_bit_vector();
	calltype_simple_bit_vector();
	calltype_bit_array();
	calltype_simple_bit_array();

	calltype_condition();
	calltype_package_error();
	calltype_file_error();
	calltype_arithmetic_error();

	calltype_conditionnull();
	calltype_restartnull();
	calltype_index();
	calltype_indexnull();
	calltype_keywordstart();
	calltype_keywordend();
	calltype_intplus();
	calltype_intplusnull();
	calltype_nonnilsymbol();
	calltype_stringdesigner();
	calltype_packagedesigner();
	calltype_functiondesigner();
	calltype_restartdesigner();
	calltype_typesymbol();
	calltype_typespec();
	calltype_functionnull();
	calltype_environmentnull();
	calltype_integernull();
	calltype_functionname();
	calltype_radixinteger();
	calltype_floatsymbol();
	calltype_readtabledesigner();
	calltype_pathnamedesigner();
	calltype_streamdesigner();
	calltype_eqlt();
	calltype_case_sensitivity();
	calltype_keytestlist();
	calltype_rehashsize();
	calltype_rehashthreshold();
	calltype_pathnamenull();
	calltype_countkey();
	calltype_countifkey();
	calltype_pathnamehost();
	calltype_pathnamedevice();
	calltype_pathnamedirectory();
	calltype_pathnamename();
	calltype_pathnametype();
	calltype_pathnameversion();
	callType_signed8();
	callType_signed16();
	callType_signed32();
	callType_unsigned8();
	callType_unsigned16();
	callType_unsigned32();
#ifdef LISP_64BIT
	callType_signed64();
	callType_unsigned64();
#endif
	calltype_opendirection();
	calltype_openelementtype();
	calltype_openifexists();
	calltype_openifdoesnotexist();
	calltype_externalformat();
	calltype_args_packagedesigner();
	calltype_args_pathnamecase();
	callType_array_t();
	callType_array_bit();
	callType_array_character();
	callType_array_singlefloat();
	callType_array_doublefloat();
	callType_array_longfloat();
	calltype_values_nil();
	calltype_values_t();
	calltype_values_null();
	calltype_values_cons();
	calltype_values_list();
	calltype_values_boolean();
	calltype_values_character();
	calltype_values_symbol();
	calltype_values_string();
	calltype_values_stringnull();
	calltype_values_simplestring();
	calltype_values_stream();
	calltype_values_streamnull();
	calltype_values_function();
	calltype_values_eqlt();
	calltype_values_package();
	calltype_values_sequence();
	calltype_values_array();
	calltype_values_integer();
	calltype_values_ratio();
	calltype_values_rational();
	calltype_values_index();
	calltype_values_indexnull();
	calltype_values_intplus();
	calltype_values_intplusnull();
	calltype_values_bit();
	calltype_values_bitarray();
	calltype_values_pathname();
	calltype_values_pathnamenull();
	calltype_values_logical_pathname();
	calltype_values_float();
	calltype_values_real();
	calltype_values_number();
	calltype_values_complex();
	calltype_compiled_object_boolean();
	calltype_compiled_symbol_boolean();
	calltype_compiled_stringcase();
	calltype_compiled_nstringcase();
	calltype_compiled_stringtrim();
	calltype_compiled_stringequal();
	calltype_compiled_stringmismatch();
	calltype_compiled_rplaca();
	calltype_compiled_list_list();
	calltype_compiled_nth();
	calltype_compiled_nconc();
	calltype_compiled_renconc();
	calltype_compiled_butlast();
	calltype_compiled_macrofunction();
	calltype_compiled_macroexpand();
	calltype_compiled_abort();
	calltype_compiled_continue();
	calltype_compiled_macroreader();
	calltype_compiled_macrodispatch();
	calltype_compiled_read();
	calltype_compiled_sublis();
	calltype_compiled_subst();
	calltype_compiled_subst_if();
	calltype_compiled_eq();
	calltype_compiled_every();
	calltype_compiled_upgraded();
	calltype_compiled_number_equal();
	calltype_compiled_number_compare();
	calltype_compiled_max();
	calltype_compiled_minusp();
	calltype_compiled_zerop();
	calltype_compiled_plus();
	calltype_compiled_minus();
	calltype_compiled_oneplus();
	calltype_compiled_hashtablecount();
	calltype_compiled_evenp();
	calltype_compiled_export();
	calltype_compiled_usepackage();
	calltype_compiled_intern();
	calltype_compiled_packagenicknames();
	calltype_compiled_prin1();
	calltype_compiled_prin1tostring();
	calltype_compiled_reverse();
	calltype_compiled_member();
	calltype_compiled_memberif();
	calltype_compiled_mapc();
	calltype_compiled_acons();
	calltype_compiled_intersection();
	calltype_compiled_ecaseerror();
	calltype_compiled_dosymbols();
	calltype_compiled_arrayboolean();
	calltype_compiled_arrayindex();
	calltype_compiled_bitand();
	calltype_compiled_countif();
	calltype_compiled_sort();
	calltype_compiled_findif();
	calltype_compiled_positionif();
	calltype_compiled_search();
	calltype_compiled_substitute();
	calltype_compiled_substituteif();
	calltype_compiled_remove();
	calltype_compiled_removeif();
	calltype_compiled_removeduplicates();
	calltype_compiled_namestring();
	calltype_compiled_pathname();
	calltype_compiled_inputstreamp();
	calltype_compiled_exit();
	calltype_compiled_readchar();
	calltype_compiled_writestring();
	calltype_compiled_finishoutput();
	calltype_compiled_yesornop();
	calltype_compiled_floor();
	calltype_compiled_ffloor();
	calltype_compiled_envinfo();
	calltype_compiled_sin();
	calltype_compiled_realpart();
	calltype_compiled_gcd();
	calltype_compiled_mod();
}

