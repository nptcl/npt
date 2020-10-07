#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "integer.h"
#include "type.h"
#include "type_constant.h"
#include "type_table.h"

/*
 *  Atomic-Type
 */
static void typetable_type0(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type0_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type1(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type1aster_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type2(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type2aster_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type3(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type3aster_heap(type, &pos);
	settypetable(table, pos);
}

static void typetable_type4(enum LISPDECL type, enum TypeTable table)
{
	addr pos;
	type4aster_heap(type, &pos);
	settypetable(table, pos);
}

#define TypeTable0(a,b) typetable_type0(LISPDECL_##a, TypeTable_##b)
#define TypeTable1(a,b) typetable_type1(LISPDECL_##a, TypeTable_##b)
#define TypeTable2(a,b) typetable_type2(LISPDECL_##a, TypeTable_##b)
#define TypeTable3(a,b) typetable_type3(LISPDECL_##a, TypeTable_##b)
#define TypeTable4(a,b) typetable_type4(LISPDECL_##a, TypeTable_##b)

#define DefTypeTable(n,a,b) \
	static void typetable_##b(void) { TypeTable##n(a,b); }

DefTypeTable(0,  TYPE,                 Type                 );
DefTypeTable(0,  ASTERISK,             Asterisk             );
DefTypeTable(0,  ATOM,                 Atom                 );
DefTypeTable(0,  LIST,                 List                 );
DefTypeTable(0,  BOOLEAN,              Boolean              );
DefTypeTable(1,  CLOS,                 Clos                 );
DefTypeTable(2,  VECTOR,               Vector               );
DefTypeTable(1,  SIMPLE_VECTOR,        SimpleVector         );
DefTypeTable(1,  BIT_VECTOR,           BitVector            );
DefTypeTable(1,  SIMPLE_BIT_VECTOR,    SimpleBitVector      );
DefTypeTable(0,  EXTENDED_CHAR,        ExtendedChar         );
DefTypeTable(1,  STRING,               String               );
DefTypeTable(1,  BASE_STRING,          BaseString           );
DefTypeTable(1,  SIMPLE_STRING,        SimpleString         );
DefTypeTable(1,  SIMPLE_BASE_STRING,   SimpleBaseString     );
DefTypeTable(1,  SIGNED_BYTE,          SignedByte           );
DefTypeTable(1,  UNSIGNED_BYTE,        UnsignedByte         );
DefTypeTable(0,  BIT,                  Bit                  );
DefTypeTable(0,  FIXNUM,               Fixnum               );
DefTypeTable(0,  BIGNUM,               Bignum               );
DefTypeTable(2,  CONS,                 Cons                 );
DefTypeTable(0,  HASH_TABLE,           Hashtable            );
DefTypeTable(0,  SYMBOL,               Symbol               );
DefTypeTable(0,  KEYWORD,              Keyword              );
DefTypeTable(0,  PACKAGE,              Package              );
DefTypeTable(0,  RANDOM_STATE,         RandomState          );
DefTypeTable(0,  READTABLE,            Readtable            );
DefTypeTable(3,  FUNCTION,             Function             ); /* not 2 */
DefTypeTable(3,  COMPILED_FUNCTION,    CompiledFunction     ); /* not 2 */
DefTypeTable(0,  PATHNAME,             Pathname             );
DefTypeTable(0,  LOGICAL_PATHNAME,     LogicalPathname      );
DefTypeTable(0,  SEQUENCE,             Sequence             );
DefTypeTable(2,  ARRAY,                Array                );
DefTypeTable(2,  SIMPLE_ARRAY,         SimpleArray          );
DefTypeTable(0,  CHARACTER,            Character            );
DefTypeTable(0,  BASE_CHAR,            BaseChar             );
DefTypeTable(0,  STANDARD_CHAR,        StandardChar         );
DefTypeTable(0,  NUMBER,               Number               );
DefTypeTable(4,  REAL,                 Real                 );
DefTypeTable(4,  RATIONAL,             Rational             );
DefTypeTable(0,  RATIO,                Ratio                );
DefTypeTable(4,  INTEGER,              Integer              );
DefTypeTable(1,  COMPLEX,              Complex              );
DefTypeTable(4,  FLOAT,                Float                );
DefTypeTable(4,  SHORT_FLOAT,          ShortFloat           );
DefTypeTable(4,  SINGLE_FLOAT,         SingleFloat          );
DefTypeTable(4,  DOUBLE_FLOAT,         DoubleFloat          );
DefTypeTable(4,  LONG_FLOAT,           LongFloat            );
DefTypeTable(0,  RESTART,              Restart              );
DefTypeTable(0,  ENVIRONMENT,          Environment          );
DefTypeTable(0,  STREAM,               Stream               );
DefTypeTable(0,  BROADCAST_STREAM,     BroadcastStream      );
DefTypeTable(0,  CONCATENATED_STREAM,  ConcatenatedStream   );
DefTypeTable(0,  ECHO_STREAM,          EchoStream           );
DefTypeTable(0,  FILE_STREAM,          FileStream           );
DefTypeTable(0,  STRING_STREAM,        StringStream         );
DefTypeTable(0,  SYNONYM_STREAM,       SynonymStream        );
DefTypeTable(0,  TWO_WAY_STREAM,       TwoWayStream         );
DefTypeTable(0,  PROMPT_STREAM,        PromptStream         );
DefTypeTable(0,  PRETTY_STREAM,        PrettyStream         );
DefTypeTable(0,  QUOTE,                Quote                );
DefTypeTable(0,  BYTESPEC,             ByteSpec             );
DefTypeTable(0,  PRINT_DISPATCH,       PrintDispatch        );
DefTypeTable(0,  EVAL,                 Eval                 );

static void typetable_Nil(void)
{
	typetable_type0(LISPDECL_NIL, TypeTable_Nil);
}

static void typetable_T(void)
{
	typetable_type0(LISPDECL_T, TypeTable_T);
}

static void typetable_Null(void)
{
	typetable_type0(LISPDECL_NULL, TypeTable_Null);
}


/*
 *  Condition
 */
static void define_type_table_condition(constindex index, enum TypeTable type)
{
	addr pos;

	GetConstant(index, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	type_clos_heap(pos, &pos);
	settypetable(type, pos);
}
#define DefTypeTableCondition(x,y) \
	static void typetable_##y(void) { \
		define_type_table_condition(CONSTANT_CONDITION_##x, TypeTable_##y); \
	}

DefTypeTableCondition(ARITHMETIC_ERROR,    ArithmeticError);
DefTypeTableCondition(CELL_ERROR,          CellError);
DefTypeTableCondition(FILE_ERROR,          FileError);
DefTypeTableCondition(PACKAGE_ERROR,       PackageError);
DefTypeTableCondition(PRINT_NOT_READABLE,  PrintNotReadable);
DefTypeTableCondition(SIMPLE_CONDITION,    SimpleCondition);
DefTypeTableCondition(STREAM_ERROR,        StreamError);
DefTypeTableCondition(TYPE_ERROR,          TypeError);


/*
 *  Type
 */
static void typetable_cons2(addr car, addr cdr, addr *ret)
{
	type2_heap(LISPDECL_CONS, car, cdr, ret);
}

static void typetable_cxr(void)
{
	/* (or null cons) */
	addr pos, type;
	GetTypeTable(&pos, Null);
	GetTypeTable(&type, Cons);
	type2or_heap(pos, type, &pos);
	SetTypeTable(Cxr, pos);
}

static void type_cxr_carcdr(addr car, addr cdr, addr *ret)
{
	/* (or null (cons car cdr)) */
	addr pos, type;
	GetTypeTable(&pos, Null);
	typetable_cons2(car, cdr, &type);
	type2or_heap(pos, type, ret);
}

static addr type_list_car(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_cxr_carcdr(type, pos, &pos);
	return pos;
}

static addr type_list_cdr(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_cxr_carcdr(pos, type, &pos);
	return pos;
}

static void typetable_car(void)
{
	addr cxr = reftypetable(TypeTable_Cxr);
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
	SetTypeTable(Cxar, cxar);
	SetTypeTable(Cxdr, cxdr);
	SetTypeTable(Cxaar, cxaar);
	SetTypeTable(Cxadr, cxadr);
	SetTypeTable(Cxdar, cxdar);
	SetTypeTable(Cxddr, cxddr);
	SetTypeTable(Cxaaar, cxaaar);
	SetTypeTable(Cxaadr, cxaadr);
	SetTypeTable(Cxadar, cxadar);
	SetTypeTable(Cxaddr, cxaddr);
	SetTypeTable(Cxdaar, cxdaar);
	SetTypeTable(Cxdadr, cxdadr);
	SetTypeTable(Cxddar, cxddar);
	SetTypeTable(Cxdddr, cxdddr);
	SetTypeTable(Fifth, fifth);
	SetTypeTable(Sixth, sixth);
	SetTypeTable(Seventh, seventh);
	SetTypeTable(Eighth, eighth);
	SetTypeTable(Ninth, ninth);
	SetTypeTable(Tenth, tenth);
}

static void type_setf_cxr_carcdr(addr car, addr cdr, addr *ret)
{
	/* (cons car cdr) */
	typetable_cons2(car, cdr, ret);
}

static addr type_cons_car(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_setf_cxr_carcdr(type, pos, &pos);
	return pos;
}

static addr type_cons_cdr(addr type)
{
	addr pos;
	GetTypeTable(&pos, Asterisk);
	type_setf_cxr_carcdr(pos, type, &pos);
	return pos;
}

static void typetable_setf_car(void)
{
	addr cxr = reftypetable(TypeTable_Cons);
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
	SetTypeTable(SetfCxar, cxar);
	SetTypeTable(SetfCxdr, cxdr);
	SetTypeTable(SetfCxaar, cxaar);
	SetTypeTable(SetfCxadr, cxadr);
	SetTypeTable(SetfCxdar, cxdar);
	SetTypeTable(SetfCxddr, cxddr);
	SetTypeTable(SetfCxaaar, cxaaar);
	SetTypeTable(SetfCxaadr, cxaadr);
	SetTypeTable(SetfCxadar, cxadar);
	SetTypeTable(SetfCxaddr, cxaddr);
	SetTypeTable(SetfCxdaar, cxdaar);
	SetTypeTable(SetfCxdadr, cxdadr);
	SetTypeTable(SetfCxddar, cxddar);
	SetTypeTable(SetfCxdddr, cxdddr);
	SetTypeTable(SetfFifth, fifth);
	SetTypeTable(SetfSixth, sixth);
	SetTypeTable(SetfSeventh, seventh);
	SetTypeTable(SetfEighth, eighth);
	SetTypeTable(SetfNinth, ninth);
	SetTypeTable(SetfTenth, tenth);
}

static void typetable_ornull(enum TypeTable type, enum TypeTable typenull)
{
	addr pos, null;

	gettypetable(type, &pos);
	GetTypeTable(&null, Null);
	type2or_heap(pos, null, &pos);
	settypetable(typenull, pos);
}
#define SetTypeTableNull(a) typetable_ornull(TypeTable_##a, TypeTable_##a##Null)

static void typetable_characternull(void)
{
	SetTypeTableNull(Character);
}

static void typetable_stringnull(void)
{
	SetTypeTableNull(String);
}

static void typetable_streamnull(void)
{
	SetTypeTableNull(Stream);
}

static void typetable_condition(void)
{
	addr pos;

	GetConst(CLOS_CONDITION, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	type_clos_heap(pos, &pos);
	SetTypeTable(Condition, pos);
}

static void typetable_conditionnull(void)
{
	SetTypeTableNull(Condition);
}

static void typetable_restartnull(void)
{
	SetTypeTableNull(Restart);
}

static void typetable_functionnull(void)
{
	SetTypeTableNull(Function);
}

static void typetable_environmentnull(void)
{
	SetTypeTableNull(Environment);
}

static void typetable_integernull(void)
{
	SetTypeTableNull(Integer);
}

static void typetable_pathnamenull(void)
{
	SetTypeTableNull(Pathname);
}

static void typetable_packagenull(void)
{
	SetTypeTableNull(Package);
}

static void typetable_printdispatchnull(void)
{
	SetTypeTableNull(PrintDispatch);
}

static void typetable_stringdesigner(void)
{
	/* (or string symbol character) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Character);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(StringDesigner, pos);
}

static void typetable_packagedesigner(void)
{
	/* (or package string symbol character) */
	addr type1, type2, type3, type4, pos;

	GetTypeTable(&type1, Package);
	GetTypeTable(&type2, String);
	GetTypeTable(&type3, Symbol);
	GetTypeTable(&type4, Character);
	type4or_heap(type1, type2, type3, type4, &pos);
	SetTypeTable(PackageDesigner, pos);
}

static void typetable_packagedesignernull(void)
{
	SetTypeTableNull(PackageDesigner);
}

static void typetable_functiondesigner(void)
{
	/* (or function symbol) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Symbol);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(FunctionDesigner, pos);
}

static void typetable_restartdesigner(void)
{
	/* (or restart (and symbol (not null))) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Restart);
	GetTypeTable(&type2, Symbol);
	type0not_heap(LISPDECL_NULL, &type3);
	type2and_heap(type2, type3, &type2);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(RestartDesigner, pos);
}

static void typetable_pathnamedesigner(void)
{
	/* (or pathname string stream) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, Pathname);
	GetTypeTable(&type2, String);
	GetTypeTable(&type3, Stream);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(PathnameDesigner, pos);
}

static void typetable_streamdesigner(void)
{
	/* (or stream symbol) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, String);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(StreamDesigner, pos);
}

static void typetable_readtabledesigner(void)
{
	/* (or readtable null) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Readtable);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(ReadtableDesigner, pos);
}

static void typetable_conditiondesigner(void)
{
	/* (or string symbol condition) */
	addr type1, type2, type3, pos;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Condition);
	type3or_heap(type1, type2, type3, &pos);
	SetTypeTable(ConditionDesigner, pos);
}

static void typetable_index(void)
{
	/* (integer 0 SIZE_MAX) */
	addr left, right;

	fixnum_heap(&left, 0);
	GetConst(INDEX_MAX, &right);
	type4_heap(LISPDECL_INTEGER, Nil, left, Nil, right, &left);
	SetTypeTable(Index, left);
}

static void typetable_indexnull(void)
{
	SetTypeTableNull(Index);
}

static void typetable_intplus(void)
{
	addr pos;

	type2integer_ab_heap(Nil, 0, &pos);
	SetTypeTable(Intplus, pos);
}

static void typetable_intplusnull(void)
{
	SetTypeTableNull(Intplus);
}

static void typetable_input_stream(void)
{
	addr pos;

	GetConst(COMMON_INPUT_STREAM_P, &pos);
	type_satisfies_heap(pos, &pos);
	SetTypeTable(InputStream, pos);
}

static void typetable_output_stream(void)
{
	addr pos;

	GetConst(COMMON_OUTPUT_STREAM_P, &pos);
	type_satisfies_heap(pos, &pos);
	SetTypeTable(OutputStream, pos);
}

static void typetable_typespec(void)
{
	/* (or [type] symbol (cons * *)) */
	addr type1, type2, type3, type4, pos;

	GetTypeTable(&type1, Type);
	GetTypeTable(&type2, Symbol);
	GetTypeTable(&type3, Cons);
	GetTypeTable(&type4, Clos);
	type4or_heap(type1, type2, type3, type4, &pos);
	SetTypeTable(TypeSpec, pos);
}

static void typetable_typesymbol(void)
{
	/* (or symbol (cons * *)) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, Cons);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(TypeSymbol, pos);
}

static void typetable_bit_array(void)
{
	addr pos, aster;

	GetTypeTable(&pos, Bit);
	GetTypeTable(&aster, Asterisk);
	type2_heap(LISPDECL_ARRAY, pos, aster, &pos);
	SetTypeTable(BitArray, pos);
}

static void typetable_simple_bit_array(void)
{
	addr pos, aster;

	GetTypeTable(&pos, Bit);
	GetTypeTable(&aster, Asterisk);
	type2_heap(LISPDECL_SIMPLE_ARRAY, pos, aster, &pos);
	SetTypeTable(SimpleBitArray, pos);
}

static void typetable_keywordstart(void)
{
	addr pos;
	GetTypeTable(&pos, Index);
	SetTypeTable(KeywordStart, pos);
}

static void typetable_keywordend(void)
{
	addr pos;
	GetTypeTable(&pos, IndexNull);
	SetTypeTable(KeywordEnd, pos);
}

static void typetable_keystart1end1(void)
{
	/* &key (start keyword-start) (end keyword-end) */
	addr key, key1, key2, type;

	/* :start1 */
	GetConst(KEYWORD_START, &key1);
	GetTypeTable(&type, KeywordStart);
	cons_heap(&key1, key1, type);
	/* :end1 */
	GetConst(KEYWORD_END, &key2);
	GetTypeTable(&type, KeywordEnd);
	cons_heap(&key2, key2, type);
	/* &key ... */
	list_heap(&key, key1, key2, NULL);
	SetTypeTable(KeyStart1End1, key);
}

static void typetable_keystart2end2(void)
{
	/* &key (start1 keyword-start) (end1 keyword-end)
	 *      (start2 keyword-start) (end2 keyword-end))
	 */
	addr key, key1, key2, key3, key4, start, end;

	GetTypeTable(&start, KeywordStart);
	GetTypeTable(&end, KeywordEnd);
	/* :start1 */
	GetConst(KEYWORD_START1, &key1);
	cons_heap(&key1, key1, start);
	/* :end1 */
	GetConst(KEYWORD_END1, &key2);
	cons_heap(&key2, key2, end);
	/* :start2 */
	GetConst(KEYWORD_START2, &key3);
	cons_heap(&key3, key3, start);
	/* :end2 */
	GetConst(KEYWORD_END2, &key4);
	cons_heap(&key4, key4, end);
	/* &key ... */
	list_heap(&key, key1, key2, key3, key4, NULL);
	SetTypeTable(KeyStart2End2, key);
}

static void typetable_functionname(void)
{
	/* (or symbol (setf symbol)) */
	/* (or symbol (cons (eql setf) (cons symbol null))) */
	addr symbol, setf, pos, cons;

	/* (cons symbol null) */
	GetTypeTable(&symbol, Symbol);
	GetTypeTable(&pos, Null);
	typetable_cons2(symbol, pos, &cons);
	/* (cons (eql 'setf) [cons]) */
	GetConst(COMMON_SETF, &setf);
	type_eql_heap(setf, &setf);
	typetable_cons2(setf, cons, &pos);
	type2or_heap(symbol, pos, &pos);
	SetTypeTable(FunctionName, pos);
}

static void typetable_radixinteger(void)
{
	/* (integer 2 36) */
	addr pos;
	type4integer_heap(Nil, 2, Nil, 36, &pos);
	SetTypeTable(RadixInteger, pos);
}

static void typetable_floatsymbol(void)
{
	addr pos, v1, v2, v3, v4;

	/* (member short-float single-float double-float long-float) */
	GetConst(COMMON_SINGLE_FLOAT, &v1);
	GetConst(COMMON_DOUBLE_FLOAT, &v2);
	GetConst(COMMON_LONG_FLOAT, &v3);
	GetConst(COMMON_SHORT_FLOAT, &v4);
	type_member_heap(&pos, v1, v2, v3, v4, NULL);
	SetTypeTable(FloatSymbol, pos);
}

static void typetable_eqlt(void)
{
	addr pos;
	type_eql_heap(T, &pos);
	SetTypeTable(EqlT, pos);
}

static void typetable_case_sensitivity(void)
{
	addr pos, v1, v2, v3, v4;

	GetConst(KEYWORD_UPCASE, &v1);
	GetConst(KEYWORD_DOWNCASE, &v2);
	GetConst(KEYWORD_PRESERVE, &v3);
	GetConst(KEYWORD_INVERT, &v4);
	type_member_heap(&pos, v1, v2, v3, v4, NULL);
	SetTypeTable(CaseSensitivity, pos);
}

static void typetable_print_case(void)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_UPCASE, &key1);
	GetConst(KEYWORD_DOWNCASE, &key2);
	GetConst(KEYWORD_CAPITALIZE, &key3);
	type_member_heap(&key1, key1, key2, key3, NULL);
	SetTypeTable(PrintCase, key1);
}


static void typetable_keytestlist(void)
{
	/* &key (:key      [function-designer])
	 *      (:test     [function-designer])
	 *      (:test-not [function-designer])
	 */
	addr key, key1, key2, key3, type;

	GetConst(KEYWORD_KEY, &key1);
	GetConst(KEYWORD_TEST, &key2);
	GetConst(KEYWORD_TEST_NOT, &key3);
	GetTypeTable(&type, FunctionDesigner);
	/* key */
	cons_heap(&key1, key1, type);
	cons_heap(&key2, key2, type);
	cons_heap(&key3, key3, type);
	list_heap(&key, key1, key2, key3, NULL);
	/* result */
	SetTypeTable(KeyTestList, key);
}

static void typetable_rehashsize(void)
{
	/* (or (integer 1 *) (float (1.0f0) *)) */
	addr type1, type2, pos;

	type2integer_ab_heap(Nil, 1, &type1);
	type2float_ab_heap(T, 1.0f, &type2);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(RehashSize, pos);
}

static void typetable_rehashthreshold(void)
{
	/* (real 0.0 1.0) */
	addr pos;

	type4realf_heap(Nil, 0.0f, Nil, 1.0f, &pos);
	SetTypeTable(RehashThreshold, pos);
}

static void typetable_countkey(void)
{
	addr key, key1, key2, key3, key4, key5, key6;

	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, START, KeywordStart);
	KeyTypeTable(&key3, END, KeywordEnd);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, TEST, FunctionDesigner);
	KeyTypeTable(&key6, TEST_NOT, FunctionDesigner);
	list_heap(&key, key1, key2, key3, key4, key5, key6, NULL);
	SetTypeTable(CountKey, key);
}

static void typetable_countifkey(void)
{
	addr key, key1, key2, key3, key4;

	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, START, KeywordStart);
	KeyTypeTable(&key3, END, KeywordEnd);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	list_heap(&key, key1, key2, key3, key4, NULL);
	SetTypeTable(CountIfKey, key);
}

static void typetable_pathnamehost(void)
{
	/* host       (or string symbol) */
	addr type1, type2, pos;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, String);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(PathnameHost, pos);
}

static void typetable_pathnamedevice(void)
{
	/* device     (or string symbol)  ;; (eql :unspecific) */
	addr type;

	GetTypeTable(&type, PathnameHost);
	SetTypeTable(PathnameDevice, type);
}

static void typetable_pathnamedirectory(void)
{
	/* directory  (or cons (member :wild :unspecific)) */
	addr type, cons, wild, unspec;

	GetTypeTable(&cons, Cons);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, wild, unspec, NULL);
	type2or_heap(cons, type, &type);
	SetTypeTable(PathnameDirectory, type);
}

static void typetable_pathnamename(void)
{
	/* name       (or string cons (member nil :wild)) */
	addr type, string, cons, wild;

	GetTypeTable(&string, String);
	GetTypeTable(&cons, Cons);
	GetConst(KEYWORD_WILD, &wild);
	type_member_heap(&type, Nil, wild, NULL);
	type3or_heap(string, cons, type, &type);
	SetTypeTable(PathnameName, type);
}

static void typetable_pathnametype(void)
{
	/* type       (or string (member nil :wild :unspecific))) */
	addr type, string, wild, unspec;

	GetTypeTable(&string, String);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	type_member_heap(&type, Nil, wild, unspec, NULL);
	type2or_heap(string, type, &type);
	SetTypeTable(PathnameType, type);
}

static void typetable_pathnameversion(void)
{
	/* version    (or (integer 1 *) (member nil :wild :unspecific :newest)) */
	addr type1, type2, newest, wild, unspec, pos;

	type2integer_ab_heap(Nil, 0, &type1);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	GetConst(KEYWORD_NEWEST, &newest);
	type_member_heap(&type2, Nil, wild, unspec, newest, NULL);
	type2or_heap(type1, type2, &pos);
	SetTypeTable(PathnameVersion, pos);
}

static void typetable_signed8(void)
{
	addr pos;
	type_signed_heap(8, &pos);
	SetTypeTable(Signed8, pos);
}

static void typetable_signed16(void)
{
	addr pos;
	type_signed_heap(16, &pos);
	SetTypeTable(Signed16, pos);
}

static void typetable_signed32(void)
{
	addr pos;
	type_signed_heap(32, &pos);
	SetTypeTable(Signed32, pos);
}

static void typetable_unsigned8(void)
{
	addr pos;
	type_unsigned_heap(8, &pos);
	SetTypeTable(Unsigned8, pos);
}

static void typetable_unsigned16(void)
{
	addr pos;
	type_unsigned_heap(16, &pos);
	SetTypeTable(Unsigned16, pos);
}

static void typetable_unsigned32(void)
{
	addr pos;
	type_unsigned_heap(32, &pos);
	SetTypeTable(Unsigned32, pos);
}

#ifdef LISP_64BIT
static void typetable_signed64(void)
{
	addr pos;
	type_signed_heap(64, &pos);
	SetTypeTable(Signed64, pos);
}

static void typetable_unsigned64(void)
{
	addr pos;
	type_unsigned_heap(64, &pos);
	SetTypeTable(Unsigned64, pos);
}
#endif

static void typetable_opendirection(void)
{
	addr type, input, output, io, probe;

	GetConst(KEYWORD_INPUT, &input);
	GetConst(KEYWORD_OUTPUT, &output);
	GetConst(KEYWORD_IO, &io);
	GetConst(KEYWORD_PROBE, &probe);
	type_member_heap(&type, input, output, io, probe, NULL);
	SetTypeTable(OpenDirection, type);
}

static void typetable_openelementtype(void)
{
	addr type, keyword;

	GetTypeTable(&type, TypeSpec);
	GetConst(KEYWORD_DEFAULT, &keyword);
	type_eql_heap(keyword, &keyword);
	type2or_heap(type, keyword, &type);
	SetTypeTable(OpenElementType, type);
}

static void typetable_openifexists(void)
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
	SetTypeTable(OpenIfExists, type);
}

static void typetable_openifdoesnotexist(void)
{
	addr type, error, create;

	GetConst(KEYWORD_ERROR, &error);
	GetConst(KEYWORD_CREATE, &create);
	type_member_heap(&type, error, create, Nil, NULL);
	SetTypeTable(OpenIfDoesNotExist, type);
}

static void typetable_externalformat(void)
{
	addr type1, type2;

	GetTypeTable(&type1, Symbol);
	GetTypeTable(&type2, String);
	type2or_heap(type1, type2, &type1);
	SetTypeTable(ExternalFormat, type1);
}

static void typetable_pprint_newline(void)
{
	addr type, key1, key2, key3, key4;
	GetConst(KEYWORD_LINEAR, &key1);
	GetConst(KEYWORD_FILL, &key2);
	GetConst(KEYWORD_MISER, &key3);
	GetConst(KEYWORD_MANDATORY, &key4);
	type_member_heap(&type, key1, key2, key3, key4, NULL);
	SetTypeTable(PprintNewline, type);
}

static void typetable_pprint_tabular(void)
{
	addr type, key1, key2, key3, key4;
	GetConst(KEYWORD_LINE, &key1);
	GetConst(KEYWORD_SECTION, &key2);
	GetConst(KEYWORD_LINE_RELATIVE, &key3);
	GetConst(KEYWORD_SECTION_RELATIVE, &key4);
	type_member_heap(&type, key1, key2, key3, key4, NULL);
	SetTypeTable(PprintTabular, type);
}

static void typetable_format(void)
{
	addr type, null, eqlt, stream, string;

	/* (or null (eql t) stream string) */
	GetTypeTable(&null, Null);
	GetTypeTable(&eqlt, EqlT);
	GetTypeTable(&stream, Stream);
	GetTypeTable(&string, String);
	type4or_heap(null, eqlt, stream, string, &type);
	SetTypeTable(Format, type);
}

static void typetable_time_second(void)
{
	addr type;
	type4integer_heap(Nil, 0, Nil, 59, &type);
	SetTypeTable(TimeSecond, type);
}

static void typetable_time_hour(void)
{
	addr type;
	type4integer_heap(Nil, 0, Nil, 23, &type);
	SetTypeTable(TimeHour, type);
}

static void typetable_time_day(void)
{
	addr type;
	type4integer_heap(Nil, 1, Nil, 31, &type);
	SetTypeTable(TimeDay, type);
}

static void typetable_time_month(void)
{
	addr type;
	type4integer_heap(Nil, 1, Nil, 12, &type);
	SetTypeTable(TimeMonth, type);
}

static void typetable_time_zone(void)
{
	addr v1, v2, type;

	fixnum_heap(&v1, -24);
	fixnum_heap(&v2, 24);
	type4_heap(LISPDECL_RATIONAL, Nil, v1, Nil, v2, &type);
	SetTypeTable(TimeZone, type);
}

static void typetable_method(void)
{
	addr pos;

	/* method1 */
	GetConst(CLOS_METHOD, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(Method, pos);
	SetTypeTable(Method1, pos);

	/* method2 */
	GetTypeTable(&pos, List);
	SetTypeTable(Method2, pos);
}

static void typetable_class(void)
{
	addr pos;

	GetConst(CLOS_CLASS, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(Class, pos);
}

static void typetable_classnull(void)
{
	SetTypeTableNull(Class);
}

static void typetable_standardclass(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StandardClass, pos);
}

static void typetable_standardobject(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_OBJECT, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StandardObject, pos);
}

static void typetable_structureclass(void)
{
	addr pos;

	GetConst(CLOS_STRUCTURE_CLASS, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StructureClass, pos);
}

static void typetable_structureobject(void)
{
	addr pos;

	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StructureObject, pos);
}

static void typetable_standard_method(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_METHOD, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(StandardMethod, pos);
}

static void typetable_methodcombination(void)
{
	addr pos;

	GetConst(CLOS_METHOD_COMBINATION, &pos);
	type_clos_heap(pos, &pos);
	SetTypeTable(MethodCombination, pos);
}


/*
 *  Array
 */
static void typetable_array_t(void)
{
	addr pos;
	GetTypeTable(&pos, T);
	SetTypeTable(Array_T, pos);
}

static void typetable_array_bit(void)
{
	addr pos;
	GetTypeTable(&pos, Bit);
	SetTypeTable(Array_Bit, pos);
}

static void typetable_array_character(void)
{
	addr pos;
	GetTypeTable(&pos, Character);
	SetTypeTable(Array_Character, pos);
}

static void typetable_array_singlefloat(void)
{
	addr pos;
	GetTypeTable(&pos, SingleFloat);
	SetTypeTable(Array_SingleFloat, pos);
}

static void typetable_array_doublefloat(void)
{
	addr pos;
	GetTypeTable(&pos, DoubleFloat);
	SetTypeTable(Array_DoubleFloat, pos);
}

static void typetable_array_longfloat(void)
{
	addr pos;
	GetTypeTable(&pos, LongFloat);
	SetTypeTable(Array_LongFloat, pos);
}

static void typetable_array_signed8(void)
{
	addr pos;
	GetTypeTable(&pos, Signed8);
	SetTypeTable(Array_Signed8, pos);
}

static void typetable_array_signed16(void)
{
	addr pos;
	GetTypeTable(&pos, Signed16);
	SetTypeTable(Array_Signed16, pos);
}

static void typetable_array_signed32(void)
{
	addr pos;
	GetTypeTable(&pos, Signed32);
	SetTypeTable(Array_Signed32, pos);
}

static void typetable_array_unsigned8(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned8);
	SetTypeTable(Array_Unsigned8, pos);
}

static void typetable_array_unsigned16(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned16);
	SetTypeTable(Array_Unsigned16, pos);
}

static void typetable_array_unsigned32(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned32);
	SetTypeTable(Array_Unsigned32, pos);
}

#ifdef LISP_64BIT
static void typetable_array_signed64(void)
{
	addr pos;
	GetTypeTable(&pos, Signed64);
	SetTypeTable(Array_Signed64, pos);
}

static void typetable_array_unsigned64(void)
{
	addr pos;
	GetTypeTable(&pos, Unsigned64);
	SetTypeTable(Array_Unsigned64, pos);
}
#endif


/*
 *  Arguments
 */
static void typeargs_empty_constant(void)
{
	addr pos;
	typeargs_empty(&pos);
	SetTypeArgs(Empty, pos);
}

static void typeargs_optconditionnull(void)
{
	addr pos;

	GetTypeTable(&pos, ConditionNull);
	typeargs_opt1(&pos, pos);
	SetTypeArgs(OptConditionNull, pos);
}

static void typeargs_packagedesigner(void)
{
	addr pos;
	GetTypeTable(&pos, PackageDesigner);
	typeargs_var1(&pos, pos);
	SetTypeArgs(PackageDesigner, pos);
}

static void typeargs_pathnamecase(void)
{
	addr args, key, symbol, common, keylocal;

	/* key */
	GetConst(KEYWORD_CASE, &symbol);
	GetConst(KEYWORD_COMMON, &common);
	GetConst(KEYWORD_LOCAL, &keylocal);
	type_member_heap(&key, common, keylocal, NULL);
	cons_heap(&key, symbol, key);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1key(&args, args, key);
	SetTypeArgs(PathnameCase, args);
}

static void typeargs_error(void)
{
	addr args, type;

	GetTypeTable(&args, ConditionDesigner);
	GetTypeTable(&type, T);
	typeargs_var1rest(&args, args, type);
	SetTypeArgs(Error, args);
}


/*
 *  Values
 */
static void deftypevalues(enum TypeTable a, enum TypeTable b)
{
	addr pos;

	gettypetable(a, &pos);
	typevalues_result(&pos, pos);
	settypetable(b, pos);
}
#define TypeValues(a) deftypevalues(TypeTable_##a, TypeValues_##a)
#define DefTypeValues(a) static void typevalues_##a(void) { TypeValues(a); }

static void typevalues_Nil(void)
{
	addr pos;
	GetTypeTable(&pos, Nil);
	typevalues_rest(&pos, pos);
	SetTypeValues(Nil, pos);
}

static void typevalues_T(void)
{
	addr pos;
	GetTypeTable(&pos, T);
	typevalues_result(&pos, pos);
	SetTypeValues(T, pos);
}

DefTypeValues(Null);
DefTypeValues(Cons);
DefTypeValues(List);
DefTypeValues(Boolean);
DefTypeValues(Character);
DefTypeValues(CharacterNull);
DefTypeValues(Symbol);
DefTypeValues(Vector);
DefTypeValues(SimpleVector);
DefTypeValues(String);
DefTypeValues(StringNull);
DefTypeValues(SimpleString);
DefTypeValues(Stream);
DefTypeValues(StreamNull);
DefTypeValues(PrettyStream);
DefTypeValues(Function);
DefTypeValues(FunctionNull);
DefTypeValues(EqlT);
DefTypeValues(Package);
DefTypeValues(PackageNull);
DefTypeValues(Sequence);
DefTypeValues(Array);
DefTypeValues(Integer);
DefTypeValues(Ratio);
DefTypeValues(Rational);
DefTypeValues(Index);
DefTypeValues(IndexNull);
DefTypeValues(Intplus);
DefTypeValues(IntplusNull);
DefTypeValues(Bit);
DefTypeValues(BitArray);
DefTypeValues(Pathname);
DefTypeValues(PathnameNull);
DefTypeValues(LogicalPathname);
DefTypeValues(Float);
DefTypeValues(Real);
DefTypeValues(Number);
DefTypeValues(Complex);
DefTypeValues(TypeSymbol);
DefTypeValues(Class);
DefTypeValues(ClassNull);


/*
 *  Values
 */
static void typevalues_decode_universal_time(void)
{
	addr sec, hour, day, month, year, week, daylight, zone;
	addr values;

	GetTypeTable(&sec, TimeSecond);
	GetTypeTable(&hour, TimeHour);
	GetTypeTable(&day, TimeDay);
	GetTypeTable(&month, TimeMonth);
	GetTypeTable(&year, Intplus);
	type4integer_heap(Nil, 0, Nil, 6, &week);
	GetTypeTable(&daylight, Boolean);
	GetTypeTable(&zone, TimeZone);
	typevalues_values_va(&values,
			sec, sec, hour, day, month, year, week, daylight, zone, NULL);
	SetTypeValues(DecodeUniversalTime, values);
}

static void typevalues_empty(void)
{
	addr values;
	typevalues_rest(&values, Nil);
	SetTypeValues(Empty, values);
}


/*
 *  Compiled-Function
 */
static void typecompiled_object_boolean(void)
{
	addr args, values;

	GetTypeTable(&args, Asterisk);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Object_Boolean, args);
}

static void typecompiled_symbol_boolean(void)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Symbol_Boolean, args);
}

static void typecompiled_stringcase(void)
{
	/* (function (string-designer &key (start keyword-start)
	 *                                 (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, KeyStart1End1);
	typeargs_var1key(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringCase, args);
}

static void typecompiled_nstringcase(void)
{
	/* (function (string &key (start keyword-start)
	 *                        (end keyword-end))
	 *           (values string &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, KeyStart1End1);
	typeargs_var1key(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(NStringCase, args);
}

static void typecompiled_stringtrim(void)
{
	/* (function (sequence string-designer) (values string &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Sequence);
	GetTypeTable(&type, StringDesigner);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringTrim, args);
}

static void typecompiled_stringequal(void)
{
	/* (function (string-designer string-designer &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values boolean &rest null))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, KeyStart2End2);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringEqual, args);
}

static void typecompiled_stringmismatch(void)
{
	/* (function (string-designer string-designer &key
	 *             (start1 keyword-start)
	 *             (end1   keyword-end)
	 *             (start2 keyword-start)
	 *             (end2   keyword-end))
	 *           (values keyword-end &rest null))
	 */
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, KeyStart2End2);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StringMismatch, args);
}

static void typecompiled_rplaca(void)
{
	/* (function (cons *) (values cons &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Cons);
	GetTypeTable(&type, Asterisk);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Cons);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Rplaca, args);
}

static void typecompiled_list_list(void)
{
	/* (function (list) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(List_List, args);
}

static void typecompiled_nth(void)
{
	/* (function (integer-plus list) (values t &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, Intplus);
	GetTypeTable(&type, List);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Nth, args);
}

static void typecompiled_nconc(void)
{
	/* (function (&rest t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Nconc, args);
}

static void typecompiled_renconc(void)
{
	/* (function (list t) (values t &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Nreconc, args);
}

static void typecompiled_butlast(void)
{
	/* (function (list &optional intplus) (values list &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, Intplus);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ButLast, args);
}

static void typecompiled_macrofunction(void)
{
	/* (function (t (or null environment)) t) */
	addr args, values, env;

	GetTypeTable(&args, T);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2(&args, args, env);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroFunction, args);
}

static void typecompiled_macroexpand(void)
{
	/* (function (t &optional (or null environment))
	 *           (values t boolean &rest nil))
	 */
	addr args, values, pos1, pos2;

	GetTypeTable(&pos1, T);
	GetTypeTable(&pos2, EnvironmentNull);
	typeargs_var1opt1(&args, pos1, pos2);
	GetTypeTable(&pos2, Boolean);
	typevalues_values2(&values, pos1, pos2);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroExpand, args);
}

static void typecompiled_abort(void)
{
	addr args, values;

	/* (function (&optional (or condition null)) nil) */
	GetTypeArgs(&args, OptConditionNull);
	GetTypeTable(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Abort, args);
}

static void typecompiled_continue(void)
{
	addr args, values;

	/* (function (&optional (or condition null)) null) */
	GetTypeArgs(&args, OptConditionNull);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Continue, args);
}

static void typecompiled_storevalue(void)
{
	addr args, values;

	/* (function (t &optional (or condition null)) null) */
	GetTypeTable(&args, T);
	GetTypeTable(&values, ConditionNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(StoreValue, args);
}

static void typecompiled_macroreader(void)
{
	/* (function (stream character) *) */
	addr args, values, type;

	GetTypeTable(&args, Stream);
	GetTypeTable(&type, Character);
	typeargs_var2(&args, args, type);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroReader, args);
}

static void typecompiled_macrodispatch(void)
{
	/* (function (stream character intplus-null) *) */
	addr args, values, type, intplus;

	GetTypeTable(&args, Stream);
	GetTypeTable(&type, Character);
	GetTypeTable(&intplus, IntplusNull);
	typeargs_var3(&args, args, type, intplus);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MacroDispatch, args);
}

static void typecompiled_read(void)
{
	addr args, values, type;

	/* (function (&optional stream t t t) (values t &rest nil)) */
	GetTypeTable(&args, Stream);
	GetTypeTable(&type, T);
	list_heap(&args, args, type, type, type, NULL);
	typeargs_full(&args, Nil, args, Nil, Nil);
	typevalues_result(&values, type);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Read, args);
}

static void typecompiled_sublis(void)
{
	/* (function
	 *   (list list
	 *    &key (:key      (or (function (t &rest t) *)   symbol))
	 *         (:test     (or (function (t t &rest t) *) symbol))
	 *         (:test-not (or (function (t t &rest t) *) symbol)))
	 *   (values list &rest nil))
	 */
	addr args, values, list, tree;

	GetTypeTable(&list, List);
	GetTypeTable(&tree, T);
	GetTypeTable(&args, KeyTestList);
	typeargs_var2key(&args, list, tree, args);
	typevalues_result(&values, list);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Sublis, args);
}

static void typecompiled_subst(void)
{
	/* (function
	 *   (t t t
	 *    &key (:key      (or (function (t &rest t) *)   symbol))
	 *         (:test     (or (function (t t &rest t) *) symbol))
	 *         (:test-not (or (function (t t &rest t) *) symbol)))
	 *   (values t &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&type, T);
	GetTypeTable(&args, KeyTestList);
	typeargs_var3key(&args, type, type, type, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Subst, args);
}

static void typecompiled_subst_if(void)
{
	/* (function
	 *   (t (or (function (t &rest t) *) symbol) t
	 *    &key (:key (or (function (t &rest t) *) symbol)))
	 *   (values t &rest nil))
	 */
	addr args, values, type, call, key;

	GetTypeTable(&type, T);
	GetTypeTable(&call, FunctionDesigner);
	GetConst(KEYWORD_KEY, &key);
	cons_heap(&key, key, call);
	conscar_heap(&key, key);
	typeargs_var3key(&args, type, call, type, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SubstIf, args);
}

static void typecompiled_eq(void)
{
	/* (function (t t) boolean) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Eq, args);
}


static void typecompiled_every(void)
{
	/* (function (function-designer sequence &rest sequence)
	 *   (values boolean &rest nil))
	 */
	addr args, values, call, sequence;

	GetTypeTable(&call, FunctionDesigner);
	GetTypeTable(&sequence, Sequence);
	typeargs_var2rest(&args, call, sequence, sequence);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Every, args);
}

static void typecompiled_number_equal(void)
{
	/* (function (number &rest number) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Number_Equal, args);
}

static void typecompiled_number_compare(void)
{
	/* (function (real &rest real) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Number_Compare, args);
}

static void typecompiled_max(void)
{
	/* (function (real &rest real) (values real &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Real);
	typeargs_var1rest(&args, values, values);
	GetTypeValues(&values, Real);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Max, args);
}

static void typecompiled_minusp(void)
{
	/* (function (real) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Minusp, args);
}

static void typecompiled_zerop(void)
{
	/* (function (number) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Zerop, args);
}

static void typecompiled_plus(void)
{
	/* (function (&rest number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Number);
	typeargs_rest(&args, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Plus, args);
}

static void typecompiled_minus(void)
{
	/* (function (number &rest number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Number);
	typeargs_var1rest(&args, values, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Minus, args);
}

static void typecompiled_oneplus(void)
{
	/* (function (number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Number);
	typeargs_var1(&args, values);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(OnePlus, args);
}

static void typecompiled_hashtablecount(void)
{
	/* (function (hash-table) (values Index &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Index);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(HashTableCount, args);
}

static void typecompiled_evenp(void)
{
	/* (function (integer) (values boolean &res nil) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Evenp, args);
}

static void typecompiled_export(void)
{
	/* (function
	 *   ((or list symbol) &optional package-designer)
	 *   (values (eql t) &rest nil))
	 */
	addr args, values, type1, type2;

	GetTypeTable(&type1, List);
	GetTypeTable(&type2, Symbol);
	type2or_heap(type1, type2, &args);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Export, args);
}

static void typecompiled_usepackage(void)
{
	/*  (function
	 *    ((or list package-designer) &optional package-designer)
	 *    (values (eql t) &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, PackageDesigner);
	type2or_heap(args, values, &args);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(UsePackage, args);
}

static void typecompiled_intern(void)
{
	/*  (function
	 *    (string &optional package-designer)
	 *    (values symbol (member :internal :external :inherited nil) &rest nil))
	 */
	addr args, values, symbol, key1, key2, key3, status;

	/* args */
	GetTypeTable(&args, String);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&args, args, values);
	/* values */
	GetTypeTable(&symbol, Symbol);
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	type_member_heap(&status, key1, key2, key3, Nil, NULL);
	typevalues_values2(&values, symbol, status);
	/* result */
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Intern, args);
}

static void typecompiled_packagenicknames(void)
{
	/*  (function
	 *    (package-designer)
	 *    (values list &rest nil))
	 */
	addr args, values;

	GetTypeArgs(&args, PackageDesigner);
	GetTypeTable(&values, List);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PackageNicknames, args);
}

static void typecompiled_prin1(void)
{
	/*  (function
	 *    (t &optional stream-designer)
	 *    (values t &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Prin1, args);
}


static void typecompiled_prin1tostring(void)
{
	/* (function (t) (values string &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Prin1ToString, args);
}

static void typecompiled_reverse(void)
{
	/* (function (sequence) (values sequence &rest nil)) */
	addr args, values;

	GetTypeTable(&values, Sequence);
	typeargs_var1(&args, values);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Reverse, args);
}

static void typecompiled_member(void)
{
	/* (function (t list &key [KeyTestList]) (values list &rest nil)) */
	addr args, values, key;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&key, KeyTestList);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Member, args);
}

static void typecompiled_memberif(void)
{
	/* (function (function-designer list &key (key function-designer))
	 *           (values list &rest nil))
	 */
	addr args, values, type, key;

	GetTypeTable(&type, FunctionDesigner);
	GetTypeTable(&args, List);
	GetConst(KEYWORD_KEY, &key);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	typeargs_var2key(&args, type, args, key);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(MemberIf, args);
}

static void typecompiled_mapc(void)
{
	/* (function (function-designer list &rest list)
	 *           (values list &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&type, FunctionDesigner);
	GetTypeTable(&args, List);
	typeargs_var2rest(&args, type, args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Mapc, args);
}

static void typecompiled_acons(void)
{
	/* (function (t t list) (values list &rest nil)) */
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, List);
	typeargs_var3(&args, args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Acons, args);
}

static void typecompiled_intersection(void)
{
	/* (function (list list &key key test test-not) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, KeyTestList);
	typeargs_var2key(&args, args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Intersection, args);
}


static void typecompiled_ecaseerror(void)
{
	/* (function (T list) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(EcaseError, args);
}

static void typecompiled_dosymbols(void)
{
	/* (function (function package-designer) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Function);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(DoSymbols, args);
}

static void typecompiled_arrayboolean(void)
{
	/* (function (array) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Array);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ArrayBoolean, args);
}

static void typecompiled_arrayindex(void)
{
	/* (function (array) (values index &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Array);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ArrayIndex, args);
}

static void typecompiled_bitand(void)
{
	/* (function (bit-array bit-array
	 *     &optional (or boolean bit-array))
	 *   (values bit-array &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, BitArray);
	GetTypeTable(&values, Boolean);
	type2or_heap(args, values, &values);
	typeargs_var2opt1(&args, args, args, values);
	GetTypeValues(&values, BitArray);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(BitAnd, args);
}

static void typecompiled_countif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values index &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(CountIf, args);
}

static void typecompiled_sort(void)
{
	/* (function (sequence call &key key) (values sequence &rest nil)) */
	addr args, values, type, key;

	/* key */
	GetConst(KEYWORD_KEY, &key);
	GetTypeTable(&type, FunctionDesigner);
	cons_heap(&key, key, type);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var2key(&args, args, type, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Sort, args);
}

static void typecompiled_findif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values t &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FindIf, args);
}

static void typecompiled_positionif(void)
{
	/* (function (function-designer sequence
	 *     &key (:from-end t)
	 *          (:start (integer 0 *))
	 *          (:end (or (integer 0 *) null))
	 *          (:key function-designer))
	 *     (values index-null &rest nil))
	 */
	addr args, values, key;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	GetTypeTable(&key, CountIfKey);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PositionIf, args);
}

static void typecompiled_search(void)
{
	/* (function (sequence1 sequence2
	 *     &key from-end test test-not key start1 start2 end1 end2)
	 *     (values index-null &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7, key8;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START1, KeywordStart);
	KeyTypeTable(&key6, START2, KeywordStart);
	KeyTypeTable(&key7, END1, KeywordEnd);
	KeyTypeTable(&key8, END2, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);

	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var2key(&args, args, args, key);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Search, args);
}

static void typecompiled_substitute(void)
{
	/* (function (t t sequence
	 *     &key from-end test test-not key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START, KeywordStart);
	KeyTypeTable(&key6, END, KeywordEnd);
	KeyTypeTable(&key7, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	typeargs_var3key(&args, args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Substitute, args);
}

static void typecompiled_substituteif(void)
{
	/* (function (t call sequence
	 *     &key from-end key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, type, key;
	addr key1, key2, key3, key4, key5;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, KEY, FunctionDesigner);
	KeyTypeTable(&key3, START, KeywordStart);
	KeyTypeTable(&key4, END, KeywordEnd);
	KeyTypeTable(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, FunctionDesigner);
	GetTypeTable(&type, Sequence);
	typeargs_var3key(&args, args, values, type, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SubstituteIf, args);
}

static void typecompiled_remove(void)
{
	/* (function (t sequence
	 *     &key from-end test test-not key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START, KeywordStart);
	KeyTypeTable(&key6, END, KeywordEnd);
	KeyTypeTable(&key7, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);

	/* type */
	GetTypeTable(&args, T);
	GetTypeTable(&values, Sequence);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Remove, args);
}

static void typecompiled_removeif(void)
{
	/* (function (call sequence
	 *     &key from-end key start end count)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, KEY, FunctionDesigner);
	KeyTypeTable(&key3, START, KeywordStart);
	KeyTypeTable(&key4, END, KeywordEnd);
	KeyTypeTable(&key5, COUNT, IntegerNull);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);

	/* type */
	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&values, Sequence);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RemoveIf, args);
}

static void typecompiled_removeduplicates(void)
{
	/* (function (sequence
	 *     &key from-end test test-not start end key)
	 *     (values sequence &rest nil))
	 */
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6;

	/* key */
	KeyTypeTable(&key1, FROM_END, T);
	KeyTypeTable(&key2, TEST, FunctionDesigner);
	KeyTypeTable(&key3, TEST_NOT, FunctionDesigner);
	KeyTypeTable(&key4, KEY, FunctionDesigner);
	KeyTypeTable(&key5, START, KeywordStart);
	KeyTypeTable(&key6, END, KeywordEnd);
	list_heap(&key, key1, key2, key3, key4, key5, key6, NULL);

	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RemoveDuplicates, args);
}

static void typecompiled_namestring(void)
{
	/* (function (pathname-designer) (values string &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Namestring, args);
}

static void typecompiled_pathname(void)
{
	/* (function (pathname-designer) (values pathname &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Pathname, args);
}

static void typecompiled_inputstreamp(void)
{
	/* (function (stream) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Stream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(InputStreamP, args);
}

static void typecompiled_exit(void)
{
	/* (function (&optional Intplus) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Intplus);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Exit, args);
}

static void typecompiled_readchar(void)
{
	/* (function (&optional stream-designer t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_opt4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ReadChar, args);
}

static void typecompiled_writestring(void)
{
	/* (function (string &optional stream-designer &key start end)
	 *           (values string &rest nil))
	 */
	addr args, values, var, opt, key, key1, key2;

	/* var */
	GetTypeTable(&var, String);
	conscar_heap(&var, var);
	/* opt */
	GetTypeTable(&opt, StreamDesigner);
	conscar_heap(&opt, opt);
	/* key */
	KeyTypeTable(&key1, START, KeywordStart);
	KeyTypeTable(&key2, END, KeywordEnd);
	list_heap(&key, key1, key2, NULL);
	/* args */
	typeargs_full(&args, var, opt, Nil, key);
	/* values */
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(WriteString, args);
}

static void typecompiled_finishoutput(void)
{
	/* (function (&optional output-stream) (values null &rest nil)) */
	addr args, values;

	GetTypeTable(&args, OutputStream);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FinishOutput, args);
}

static void typecompiled_yesornop(void)
{
	/* (function (&optional (or string null) &rest t)
	 *           (values boolean &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StringNull);
	conscar_heap(&args, args);
	GetTypeTable(&values, T);
	typeargs_full(&args, Nil, args, values, Nil);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(YesOrNoP, args);
}

static void typecompiled_floor(void)
{
	/* (function (real &optional real) (values integer real &rest nil) */
	addr args, values, type;

	GetTypeTable(&type, Real);
	typeargs_var1opt1(&args, type, type);
	GetTypeTable(&values, Integer);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Floor, args);
}

static void typecompiled_ffloor(void)
{
	/* (function (real &optional real) (values real real &rest nil) */
	addr args, values, type;

	GetTypeTable(&type, Real);
	typeargs_var1opt1(&args, type, type);
	typevalues_values2(&values, type, type);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Ffloor, args);
}

static void typecompiled_envinfo(void)
{
	/* (function () (values (or string null) &rest nil)) */
	addr args, values;

	typeargs_empty(&args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(EnvInfo, args);
}

static void typecompiled_sin(void)
{
	/* (function (number) (values number &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Number);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Sin, args);
}

static void typecompiled_realpart(void)
{
	/* (function (number) (values real &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Number);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Real);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RealPart, args);
}

static void typecompiled_gcd(void)
{
	/* (function (&rest integer) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Gcd, args);
}

static void typecompiled_mod(void)
{
	/* (function (real real) (values real &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Real);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Mod, args);
}

static void typecompiled_float_digits(void)
{
	/* (function (float) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Float);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FloatDigits, args);
}

static void typecompiled_rational(void)
{
	/* (function (real) (values rational &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Rational);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Rational, args);
}

static void typecompiled_logand(void)
{
	/* (function (&rest integer) (values integer &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Logand, args);
}

static void typecompiled_logandc1(void)
{
	/* (function (integer integer) (values integer &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Logandc1, args);
}

static void typecompiled_byte_size(void)
{
	/* (function (byte) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, ByteSpec);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(ByteSize, args);
}

static void typecompiled_deposit_field(void)
{
	/* (function (integer byte integer) (values integer &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Integer);
	GetTypeTable(&values, ByteSpec);
	typeargs_var3(&args, args, values, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(DepositField, args);
}

static void typecompiled_ldb(void)
{
	/* (function (bytespec integer) (values (integer 0 *) &rest nil)) */
	addr args, values;

	GetTypeTable(&args, ByteSpec);
	GetTypeTable(&values, Integer);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Ldb, args);
}

static void typecompiled_upgraded_type(void)
{
	/* (function (typespec &optional environment) (values type &rest nil)) */
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, EnvironmentNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, TypeSymbol);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(UpgradedType, args);
}

static void typecompiled_slot_boundp(void)
{
	/* (function (clos symbol) (values boolean &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Clos);
	GetTypeTable(&values, Symbol);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SlotBoundp, args);
}

static void typecompiled_slot_boundp_method(void)
{
	/* (function (t t t t symbol) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var3(&args, args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(SlotBoundp_Method, args);
}

static void typecompiled_reader_method(void)
{
	/* (function (t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Reader_Method, args);
}

static void typecompiled_writer_method(void)
{
	/* (function (t t t t) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2(&args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Writer_Method, args);
}

static void typecompiled_signal(void)
{
	addr args, values;

	GetTypeArgs(&args, Error);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(Signal, args);
}

static void typecompiled_print_object_method(void)
{
	/* (function (t t t stream) (values t &rest nil)) */
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Stream);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PrintObject_Method, args);
}

static void typecompiled_pprint_fill(void)
{
	/* (function (output-stream-designer t &optional t t)
	 *           (values null &rest nil))
	 */
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_var2opt2(&args, args, values, values, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(PprintFill, args);
}

static void typecompiled_dispatch_function(void)
{
	/* (function (output-stream-designer t) (values T &rest nil)) */
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(DispatchFunction, args);
}

static void typecompiled_formatter_function(void)
{
	addr args, values;

	GetTypeTable(&args, Stream);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(FormatterFunction, args);
}

static void typecompiled_get_internal_real_time(void)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(GetInternalRealTime, args);
}

static void typecompiled_remove_file(void)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	GetTypeTable(&values, T);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(RemoveFile, args);
}

static void typecompiled_infobit(void)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(InfoBit, args);
}

static void typecompiled_chareql(void)
{
	addr args, values;

	GetTypeTable(&args, Character);
	typeargs_var1rest(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, &args);
	SetTypeCompiled(CharEql, args);
}


/*
 *  Interface
 */
_g void build_type_constant(void)
{
	/* Atomic-Type */
	typetable_Type();
	typetable_Asterisk();
	typetable_Atom();
	typetable_List();
	typetable_Boolean();
	typetable_Clos();
	typetable_Vector();
	typetable_SimpleVector();
	typetable_BitVector();
	typetable_ExtendedChar();
	typetable_SimpleBitVector();
	typetable_String();
	typetable_BaseString();
	typetable_SimpleString();
	typetable_SimpleBaseString();
	typetable_SignedByte();
	typetable_UnsignedByte();
	typetable_Bit();
	typetable_Fixnum();
	typetable_Bignum();
	typetable_Nil();
	typetable_T();
	typetable_Null();
	typetable_Cons();
	typetable_Hashtable();
	typetable_Symbol();
	typetable_Keyword();
	typetable_Package();
	typetable_RandomState();
	typetable_Readtable();
	typetable_Function();
	typetable_CompiledFunction();
	typetable_Pathname();
	typetable_LogicalPathname();
	typetable_Sequence();
	typetable_Array();
	typetable_SimpleArray();
	typetable_Character();
	typetable_BaseChar();
	typetable_StandardChar();
	typetable_Number();
	typetable_Real();
	typetable_Rational();
	typetable_Ratio();
	typetable_Integer();
	typetable_Complex();
	typetable_Float();
	typetable_ShortFloat();
	typetable_SingleFloat();
	typetable_DoubleFloat();
	typetable_LongFloat();
	typetable_Restart();
	typetable_Environment();
	typetable_Stream();
	typetable_BroadcastStream();
	typetable_ConcatenatedStream();
	typetable_EchoStream();
	typetable_FileStream();
	typetable_StringStream();
	typetable_SynonymStream();
	typetable_TwoWayStream();
	typetable_PromptStream();
	typetable_PrettyStream();
	typetable_Quote();
	typetable_ByteSpec();
	typetable_PrintDispatch();
	typetable_Eval();

	/* Condition */
	typetable_ArithmeticError();
	typetable_CellError();
	typetable_FileError();
	typetable_PackageError();
	typetable_PrintNotReadable();
	typetable_SimpleCondition();
	typetable_StreamError();
	typetable_TypeError();

	/* Type */
	typetable_cxr();
	typetable_car();
	typetable_setf_car();
	typetable_characternull();
	typetable_stringnull();
	typetable_streamnull();
	typetable_condition();
	typetable_conditionnull();
	typetable_restartnull();
	typetable_functionnull();
	typetable_environmentnull();
	typetable_integernull();
	typetable_pathnamenull();
	typetable_packagenull();
	typetable_printdispatchnull();
	typetable_stringdesigner();
	typetable_packagedesigner();
	typetable_packagedesignernull();
	typetable_functiondesigner();
	typetable_restartdesigner();
	typetable_pathnamedesigner();
	typetable_streamdesigner();
	typetable_readtabledesigner();
	typetable_conditiondesigner();
	typetable_index();
	typetable_indexnull();
	typetable_intplus();
	typetable_intplusnull();
	typetable_input_stream();
	typetable_output_stream();
	typetable_typespec();
	typetable_typesymbol();
	typetable_bit_array();
	typetable_simple_bit_array();
	typetable_keywordstart();
	typetable_keywordend();
	typetable_keystart1end1();
	typetable_keystart2end2();
	typetable_functionname();
	typetable_radixinteger();
	typetable_floatsymbol();
	typetable_eqlt();
	typetable_case_sensitivity();
	typetable_print_case();
	typetable_keytestlist();
	typetable_rehashsize();
	typetable_rehashthreshold();
	typetable_countkey();
	typetable_countifkey();
	typetable_pathnamehost();
	typetable_pathnamedevice();
	typetable_pathnamedirectory();
	typetable_pathnamename();
	typetable_pathnametype();
	typetable_pathnameversion();
	typetable_signed8();
	typetable_signed16();
	typetable_signed32();
	typetable_unsigned8();
	typetable_unsigned16();
	typetable_unsigned32();
#ifdef LISP_64BIT
	typetable_signed64();
	typetable_unsigned64();
#endif
	typetable_opendirection();
	typetable_openelementtype();
	typetable_openifexists();
	typetable_openifdoesnotexist();
	typetable_externalformat();
	typetable_pprint_newline();
	typetable_pprint_tabular();
	typetable_format();
	typetable_time_second();
	typetable_time_hour();
	typetable_time_day();
	typetable_time_month();
	typetable_time_zone();
	typetable_method();
	typetable_class();
	typetable_classnull();
	typetable_standardclass();
	typetable_standardobject();
	typetable_structureclass();
	typetable_structureobject();
	typetable_standard_method();
	typetable_methodcombination();

	/* Array */
	typetable_array_t();
	typetable_array_bit();
	typetable_array_character();
	typetable_array_singlefloat();
	typetable_array_doublefloat();
	typetable_array_longfloat();
	typetable_array_signed8();
	typetable_array_signed16();
	typetable_array_signed32();
	typetable_array_unsigned8();
	typetable_array_unsigned16();
	typetable_array_unsigned32();
#ifdef LISP_64BIT
	typetable_array_signed64();
	typetable_array_unsigned64();
#endif

	/* Arguments */
	typeargs_empty_constant();
	typeargs_optconditionnull();
	typeargs_packagedesigner();
	typeargs_pathnamecase();
	typeargs_error();

	/* Values */
	typevalues_Nil();
	typevalues_T();
	typevalues_Null();
	typevalues_Cons();
	typevalues_List();
	typevalues_Boolean();
	typevalues_Character();
	typevalues_CharacterNull();
	typevalues_Symbol();
	typevalues_Vector();
	typevalues_SimpleVector();
	typevalues_String();
	typevalues_StringNull();
	typevalues_SimpleString();
	typevalues_Stream();
	typevalues_StreamNull();
	typevalues_PrettyStream();
	typevalues_Function();
	typevalues_FunctionNull();
	typevalues_EqlT();
	typevalues_Package();
	typevalues_PackageNull();
	typevalues_Sequence();
	typevalues_Array();
	typevalues_Integer();
	typevalues_Ratio();
	typevalues_Rational();
	typevalues_Index();
	typevalues_IndexNull();
	typevalues_Intplus();
	typevalues_IntplusNull();
	typevalues_Bit();
	typevalues_BitArray();
	typevalues_Pathname();
	typevalues_PathnameNull();
	typevalues_LogicalPathname();
	typevalues_Float();
	typevalues_Real();
	typevalues_Number();
	typevalues_Complex();
	typevalues_TypeSymbol();
	typevalues_Class();
	typevalues_ClassNull();

	typevalues_decode_universal_time();
	typevalues_empty();

	/* Compiled-Function */
	typecompiled_object_boolean();
	typecompiled_symbol_boolean();
	typecompiled_stringcase();
	typecompiled_nstringcase();
	typecompiled_stringtrim();
	typecompiled_stringequal();
	typecompiled_stringmismatch();
	typecompiled_rplaca();
	typecompiled_list_list();
	typecompiled_nth();
	typecompiled_nconc();
	typecompiled_renconc();
	typecompiled_butlast();
	typecompiled_macrofunction();
	typecompiled_macroexpand();
	typecompiled_abort();
	typecompiled_continue();
	typecompiled_storevalue();
	typecompiled_macroreader();
	typecompiled_macrodispatch();
	typecompiled_read();
	typecompiled_sublis();
	typecompiled_subst();
	typecompiled_subst_if();
	typecompiled_eq();
	typecompiled_every();
	typecompiled_number_equal();
	typecompiled_number_compare();
	typecompiled_max();
	typecompiled_minusp();
	typecompiled_zerop();
	typecompiled_plus();
	typecompiled_minus();
	typecompiled_oneplus();
	typecompiled_hashtablecount();
	typecompiled_evenp();
	typecompiled_export();
	typecompiled_usepackage();
	typecompiled_intern();
	typecompiled_packagenicknames();
	typecompiled_prin1();
	typecompiled_prin1tostring();
	typecompiled_reverse();
	typecompiled_member();
	typecompiled_memberif();
	typecompiled_mapc();
	typecompiled_acons();
	typecompiled_intersection();
	typecompiled_ecaseerror();
	typecompiled_dosymbols();
	typecompiled_arrayboolean();
	typecompiled_arrayindex();
	typecompiled_bitand();
	typecompiled_countif();
	typecompiled_sort();
	typecompiled_findif();
	typecompiled_positionif();
	typecompiled_search();
	typecompiled_substitute();
	typecompiled_substituteif();
	typecompiled_remove();
	typecompiled_removeif();
	typecompiled_removeduplicates();
	typecompiled_namestring();
	typecompiled_pathname();
	typecompiled_inputstreamp();
	typecompiled_exit();
	typecompiled_readchar();
	typecompiled_writestring();
	typecompiled_finishoutput();
	typecompiled_yesornop();
	typecompiled_floor();
	typecompiled_ffloor();
	typecompiled_envinfo();
	typecompiled_sin();
	typecompiled_realpart();
	typecompiled_gcd();
	typecompiled_mod();
	typecompiled_float_digits();
	typecompiled_rational();
	typecompiled_logand();
	typecompiled_logandc1();
	typecompiled_byte_size();
	typecompiled_deposit_field();
	typecompiled_ldb();
	typecompiled_upgraded_type();
	typecompiled_slot_boundp();
	typecompiled_slot_boundp_method();
	typecompiled_reader_method();
	typecompiled_writer_method();
	typecompiled_signal();
	typecompiled_print_object_method();
	typecompiled_pprint_fill();
	typecompiled_dispatch_function();
	typecompiled_formatter_function();
	typecompiled_get_internal_real_time();
	typecompiled_remove_file();
	typecompiled_infobit();
	typecompiled_chareql();
}

