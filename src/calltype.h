#ifndef __CALLTYPE_HEADER__
#define __CALLTYPE_HEADER__

#include "constant.h"
#include "typedef.h"

enum CallType {
	CallType_Asterisk,
	CallType_Nil,
	CallType_T,
	CallType_Type,
	CallType_Character,
	CallType_Symbol,
	CallType_Keyword,
	CallType_Null,
	CallType_Cons,
	CallType_Cxr,
	CallType_Cxar,
	CallType_Cxdr,
	CallType_Cxaar,
	CallType_Cxadr,
	CallType_Cxdar,
	CallType_Cxddr,
	CallType_Cxaaar,
	CallType_Cxaadr,
	CallType_Cxadar,
	CallType_Cxaddr,
	CallType_Cxdaar,
	CallType_Cxdadr,
	CallType_Cxddar,
	CallType_Cxdddr,
	CallType_SetfCxar,
	CallType_SetfCxdr,
	CallType_SetfCxaar,
	CallType_SetfCxadr,
	CallType_SetfCxdar,
	CallType_SetfCxddr,
	CallType_SetfCxaaar,
	CallType_SetfCxaadr,
	CallType_SetfCxadar,
	CallType_SetfCxaddr,
	CallType_SetfCxdaar,
	CallType_SetfCxdadr,
	CallType_SetfCxddar,
	CallType_SetfCxdddr,
	CallType_Fifth,
	CallType_Sixth,
	CallType_Seventh,
	CallType_Eighth,
	CallType_Ninth,
	CallType_Tenth,
	CallType_SetfFifth,
	CallType_SetfSixth,
	CallType_SetfSeventh,
	CallType_SetfEighth,
	CallType_SetfNinth,
	CallType_SetfTenth,
	CallType_List,
	CallType_Boolean,
	CallType_String,
	CallType_StringNull,
	CallType_SimpleString,
	CallType_Stream,
	CallType_StreamNull,
	CallType_FileStream,
	CallType_SynonymStream,
	CallType_BroadcastStream,
	CallType_TwoWayStream,
	CallType_EchoStream,
	CallType_ConcatenatedStream,
	CallType_StringStream,
	CallType_InputStream,
	CallType_OutputStream,
	CallType_Function,
	CallType_CompiledFunction,
	CallType_Package,
	CallType_Sequence,
	CallType_Condition,
	CallType_PackageError,
	CallType_FileError,
	CallType_Restart,
	CallType_Environment,
	CallType_Readtable,
	CallType_Pathname,
	CallType_LogicalPathname,
	CallType_Hashtable,
	CallType_RandomState,
	CallType_Integer,
	CallType_Ratio,
	CallType_Rational,
	CallType_Real,
	CallType_Float,
	CallType_ShortFloat,
	CallType_SingleFloat,
	CallType_DoubleFloat,
	CallType_LongFloat,
	CallType_Number,
	CallType_Array,
	CallType_Vector,
	CallType_Bit,
	CallType_BitVector,
	CallType_SimpleBitVector,
	CallType_BitArray,
	CallType_SimpleBitArray,

	CallType_ArithmeticError,

	CallType_ConditionNull,
	CallType_RestartNull,
	CallType_Index,
	CallType_IndexNull,
	CallType_KeywordStart,
	CallType_KeywordEnd,
	CallType_Intplus,
	CallType_IntplusNull,
	CallType_NonNilSymbol,
	CallType_StringDesigner,
	CallType_PackageDesigner,
	CallType_FunctionDesigner,
	CallType_RestartDesigner,
	CallType_PathnameDesigner,
	CallType_StreamDesigner,
	CallType_TypeSpec,
	CallType_TypeSymbol,
	CallType_FunctionNull,
	CallType_EnvironmentNull,
	CallType_IntegerNull,
	CallType_FunctionName,
	CallType_RadixInteger,
	CallType_FloatSymbol,
	CallType_ReadtableDesigner,
	CallType_EqlT,
	CallType_CaseSensitivity,
	CallType_KeyTestList,
	CallType_RehashSize,
	CallType_RehashThreshold,
	CallType_PathnameNull,
	CallType_CountKey,
	CallType_CountIfKey,
	CallType_PathnameHost,
	CallType_PathnameDevice,
	CallType_PathnameDirectory,
	CallType_PathnameName,
	CallType_PathnameType,
	CallType_PathnameVersion,
	CallType_Signed8,
	CallType_Signed16,
	CallType_Signed32,
#ifdef LISP_64BIT
	CallType_Signed64,
#endif
	CallType_Unsigned8,
	CallType_Unsigned16,
	CallType_Unsigned32,
#ifdef LISP_64BIT
	CallType_Unsigned64,
#endif
	CallType_OpenDirection,
	CallType_OpenElementType,
	CallType_OpenIfExists,
	CallType_OpenIfDoesNotExist,
	CallType_ExternalFormat,

	CallType_Args_PackageDesigner,
	CallType_Args_PathnameCase,
	CallType_Array_T,
	CallType_Array_Bit,
	CallType_Array_Character,
	CallType_Array_SingleFloat,
	CallType_Array_DoubleFloat,
	CallType_Array_LongFloat,
	CallType_Values_Nil,
	CallType_Values_T,
	CallType_Values_Null,
	CallType_Values_Cons,
	CallType_Values_List,
	CallType_Values_Boolean,
	CallType_Values_Character,
	CallType_Values_Symbol,
	CallType_Values_String,
	CallType_Values_StringNull,
	CallType_Values_SimpleString,
	CallType_Values_Stream,
	CallType_Values_StreamNull,
	CallType_Values_Function,
	CallType_Values_EqlT,
	CallType_Values_Package,
	CallType_Values_Sequence,
	CallType_Values_Array,
	CallType_Values_Integer,
	CallType_Values_Index,
	CallType_Values_IndexNull,
	CallType_Values_Intplus,
	CallType_Values_IntplusNull,
	CallType_Values_Bit,
	CallType_Values_BitArray,
	CallType_Values_Pathname,
	CallType_Values_PathnameNull,
	CallType_Values_LogicalPathname,
	CallType_Values_Float,
	CallType_Compiled_Object_Boolean,
	CallType_Compiled_Symbol_Boolean,
	CallType_Compiled_StringCase,
	CallType_Compiled_NStringCase,
	CallType_Compiled_StringTrim,
	CallType_Compiled_StringEqual,
	CallType_Compiled_StringMismatch,
	CallType_Compiled_Rplaca,
	CallType_Compiled_List_List,
	CallType_Compiled_Nth,
	CallType_Compiled_Nconc,
	CallType_Compiled_Nreconc,
	CallType_Compiled_ButLast,
	CallType_Compiled_MacroFunction,
	CallType_Compiled_MacroExpand,
	CallType_Compiled_Abort,
	CallType_Compiled_Continue,
	CallType_Compiled_MacroReader,
	CallType_Compiled_MacroDispatch,
	CallType_Compiled_Read,
	CallType_Compiled_Sublis,
	CallType_Compiled_Subst,
	CallType_Compiled_SubstIf,
	CallType_Compiled_Eq,
	CallType_Compiled_Every,
	CallType_Compiled_Upgraded,
	CallType_Compiled_Number_Equal,
	CallType_Compiled_Number_Compare,
	CallType_Compiled_Max,
	CallType_Compiled_Minusp,
	CallType_Compiled_Zerop,
	CallType_Compiled_Plus,
	CallType_Compiled_Minus,
	CallType_Compiled_OnePlus,
	CallType_Compiled_HashTableCount,
	CallType_Compiled_Evenp,
	CallType_Compiled_Export,
	CallType_Compiled_UsePackage,
	CallType_Compiled_Intern,
	CallType_Compiled_PackageNicknames,
	CallType_Compiled_Prin1,
	CallType_Compiled_Prin1ToString,
	CallType_Compiled_Reverse,
	CallType_Compiled_Member,
	CallType_Compiled_MemberIf,
	CallType_Compiled_Mapc,
	CallType_Compiled_Acons,
	CallType_Compiled_Intersection,
	CallType_Compiled_EcaseError,
	CallType_Compiled_DoSymbols,
	CallType_Compiled_ArrayBoolean,
	CallType_Compiled_ArrayIndex,
	CallType_Compiled_BitAnd,
	CallType_Compiled_CountIf,
	CallType_Compiled_Sort,
	CallType_Compiled_FindIf,
	CallType_Compiled_PositionIf,
	CallType_Compiled_Search,
	CallType_Compiled_Substitute,
	CallType_Compiled_SubstituteIf,
	CallType_Compiled_Remove,
	CallType_Compiled_RemoveIf,
	CallType_Compiled_RemoveDuplicates,
	CallType_Compiled_Namestring,
	CallType_Compiled_Pathname,
	CallType_Compiled_InputStreamP,
	CallType_Compiled_Exit,
	CallType_Compiled_ReadChar,
	CallType_Compiled_WriteString,
	CallType_Compiled_FinishOutput,
	CallType_Compiled_YesOrNoP,
	CallType_Compiled_Floor,
	CallType_Compiled_Ffloor,
	CallType_Compiled_EnvInfo,
	CallType_Size
};

void keycons_calltype(addr *ret, constindex name, enum CallType type);
#define KeyCallType(r,a,b) keycons_calltype((r), CONSTANT_KEYWORD_##a, CallType_##b)
void clos_type(addr *ret, addr clos);
void clos_type_constant(addr *ret, constindex name);
#define ConditionType(r,n) clos_type_constant((r), CONSTANT_CONDITION_##n)

void nonnil_symbol_type(addr *ret);
void radix_integer_type(addr *ret);

void empty_argtype(addr *ret);
void full_argtype(addr *ret, addr var, addr opt, addr rest, addr key);
void var1_argtype(addr *ret, addr type);
void var2_argtype(addr *ret, addr type1, addr type2);
void var3_argtype(addr *ret, addr type1, addr type2, addr type3);
void var4_argtype(addr *ret, addr type1, addr type2, addr type3, addr type4);
void var1key_argtype(addr *ret, addr var, addr key);
void var2key_argtype(addr *ret, addr var1, addr var2, addr key);
void var3key_argtype(addr *ret, addr var1, addr var2, addr var3, addr key);
void var4key_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4, addr key);
void opt1_argtype(addr *ret, addr type);
void opt2_argtype(addr *ret, addr type1, addr type2);
void opt4_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4);
void opt5_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
void var1opt1_argtype(addr *ret, addr var, addr opt);
void var1opt2_argtype(addr *ret, addr var, addr opt1, addr opt2);
void var1opt2key_argtype(addr *ret, addr var, addr opt1, addr opt2, addr key);
void var2opt1_argtype(addr *ret, addr var1, addr var2, addr opt);
void var2opt2_argtype(addr *ret, addr var1, addr var2, addr opt1, addr opt2);
void var3opt1_argtype(addr *ret, addr var1, addr var2, addr var3, addr opt);
void var1rest_argtype(addr *ret, addr var, addr rest);
void var2rest_argtype(addr *ret, addr var1, addr var2, addr rest);
void var3rest_argtype(addr *ret, addr var1, addr var2, addr var3, addr rest);
void var4rest_argtype(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest);
void rest_argtype(addr *ret, addr rest);
void key_argtype(addr *ret, addr key);
void opt1conditionnull_argtype(addr *ret);
void char_rest_char_argtype(addr *ret);
void character_designer_argtype(addr *ret);

void result_valuestype(addr *ret, addr type);
void values2_valuestype(addr *ret, addr type1, addr type2);
void values3_valuestype(addr *ret, addr type1, addr type2, addr type3);
void values4_valuestype(addr *ret, addr type1, addr type2, addr type3, addr type4);
void values5_valuestype(addr *ret, addr t1, addr t2, addr t3, addr t4, addr t5);
void rest_valuestype(addr *ret, addr type);
void restrestart_valuestype(addr *ret);

/* build */
addr refcalltype(enum CallType index);
void getcalltype(addr *ret, enum CallType index);
#define GetCallType(a,b) getcalltype((a), CallType_##b)
void build_calltype(void);

#endif

