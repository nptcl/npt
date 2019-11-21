#include "bignum.h"
#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "integer.h"
#include "type_parse.h"
#include "type_table.h"


/*
 *  interface
 */
static void getroot_typetable(addr *ret)
{
	*ret = Root(LISPINDEX_TYPETABLE);
	CheckType(*ret, LISPTYPE_VECTOR);
}

_g addr reftypetable(enum TypeTable index)
{
	addr pos;
	gettypetable(index, &pos);
	return pos;
}

_g void gettypetable(enum TypeTable index, addr *ret)
{
	addr table;

	getroot_typetable(&table);
	GetArrayA4(table, (size_t)index, ret);
	Check(*ret == Nil, "type error");
}

_g void settypetable(enum TypeTable index, addr pos)
{
	addr table;

	getroot_typetable(&table);
	SetArrayA4(table, (size_t)index, pos);
	SetStatusReadOnly(pos);
}

_g void keytypetable(constindex name, enum TypeTable type, addr *ret)
{
	addr value1, value2;

	GetConstant(name, &value1);
	gettypetable(type, &value2);
	cons_heap(ret, value1, value2);
}

_g void build_type_table(void)
{
	addr pos;
	vector4_heap(&pos, TypeTable_Size);
	Root(LISPINDEX_TYPETABLE) = pos;
}


/*
 *  arguments
 */
_g void typeargs_empty(addr *ret)
{
	vector2_heap(ret, 4);
}

_g void typeargs_full(addr *ret, addr var, addr opt, addr rest, addr key)
{
	addr pos;

	vector2_heap(&pos, 4);
	SetArrayA2(pos, 0, var);
	SetArrayA2(pos, 1, opt);
	SetArrayA2(pos, 2, rest);
	SetArrayA2(pos, 3, key);
	*ret = pos;
}

_g void typeargs_var1(addr *ret, addr v1)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

_g void typeargs_var2(addr *ret, addr v1, addr v2)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

_g void typeargs_var3(addr *ret, addr v1, addr v2, addr v3)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

_g void typeargs_var4(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

_g void typeargs_var5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	typeargs_full(ret, v1, Nil, Nil, Nil);
}

_g void typeargs_var1key(addr *ret, addr v1, addr key)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, Nil, key);
}

_g void typeargs_var2key(addr *ret, addr v1, addr v2, addr key)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, Nil, key);
}

_g void typeargs_var3key(addr *ret, addr v1, addr v2, addr var3, addr key)
{
	list_heap(&v1, v1, v2, var3, NULL);
	typeargs_full(ret, v1, Nil, Nil, key);
}

_g void typeargs_var4key(addr *ret, addr v1, addr v2, addr v3, addr v4, addr key)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, Nil, key);
}

_g void typeargs_opt1(addr *ret, addr v1)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

_g void typeargs_opt2(addr *ret, addr v1, addr v2)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

_g void typeargs_opt3(addr *ret, addr v1, addr v2, addr v3)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}
_g void typeargs_opt4(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

_g void typeargs_opt5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	typeargs_full(ret, Nil, v1, Nil, Nil);
}

_g void typeargs_var1opt1(addr *ret, addr var1, addr opt1)
{
	conscar_heap(&var1, var1);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

_g void typeargs_var1opt2(addr *ret, addr var1, addr opt1, addr opt2)
{
	conscar_heap(&var1, var1);
	list_heap(&opt1, opt1, opt2, NULL);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

_g void typeargs_var1opt2key(addr *ret, addr var1, addr opt1, addr opt2, addr key)
{
	conscar_heap(&var1, var1);
	list_heap(&opt1, opt1, opt2, NULL);
	typeargs_full(ret, var1, opt1, Nil, key);
}

_g void typeargs_var2opt1(addr *ret, addr var1, addr var2, addr opt1)
{
	list_heap(&var1, var1, var2, NULL);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

_g void typeargs_var2opt2(addr *ret, addr var1, addr var2, addr opt1, addr opt2)
{
	list_heap(&var1, var1, var2, NULL);
	list_heap(&opt1, opt1, opt2, NULL);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

_g void typeargs_var2opt3(addr *ret, addr v1, addr v2, addr o1, addr o2, addr o3)
{
	list_heap(&v1, v1, v2, NULL);
	list_heap(&o1, o1, o2, o3, NULL);
	typeargs_full(ret, v1, o1, Nil, Nil);
}

_g void typeargs_var3opt1(addr *ret, addr var1, addr var2, addr var3, addr opt1)
{
	list_heap(&var1, var1, var2, var3, NULL);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, var1, opt1, Nil, Nil);
}

_g void typeargs_var4opt1(addr *ret, addr v1, addr v2, addr v3, addr v4, addr opt1)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, v1, opt1, Nil, Nil);
}

_g void typeargs_var1rest(addr *ret, addr v1, addr rest)
{
	conscar_heap(&v1, v1);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

_g void typeargs_var2rest(addr *ret, addr v1, addr v2, addr rest)
{
	list_heap(&v1, v1, v2, NULL);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

_g void typeargs_var3rest(addr *ret, addr v1, addr v2, addr v3, addr rest)
{
	list_heap(&v1, v1, v2, v3, NULL);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

_g void typeargs_var4rest(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest)
{
	list_heap(&v1, v1, v2, v3, v4, NULL);
	typeargs_full(ret, v1, Nil, rest, Nil);
}

_g void typeargs_opt1rest(addr *ret, addr opt1, addr rest)
{
	conscar_heap(&opt1, opt1);
	typeargs_full(ret, Nil, opt1, rest, Nil);
}

_g void typeargs_rest(addr *ret, addr rest)
{
	typeargs_full(ret, Nil, Nil, rest, Nil);
}

_g void typeargs_key(addr *ret, addr key)
{
	typeargs_full(ret, Nil, Nil, Nil, key);
}

_g void typeargs_method(addr pos)
{
	addr var, method1, method2;

	GetArrayA2(pos, 0, &var); /* var */
	GetTypeTable(&method1, Method1);
	GetTypeTable(&method2, Method2);
	lista_heap(&var, method1, method2, var, NULL);
	SetArrayA2(pos, 0, var); /* var */
}

_g void typeargs_methodkey(addr pos)
{
	addr rest;

	typeargs_method(pos);
	GetArrayA2(pos, 2, &rest); /* rest */
	if (rest == Nil) {
		GetTypeTable(&rest, T);
		SetArrayA2(pos, 2, rest); /* rest */
	}
}


/*
 *  values
 */
_g void typevalues_result(addr *ret, addr v1)
{
	/* (values v1 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	conscar_heap(&v1, v1);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

_g void typevalues_values2(addr *ret, addr v1, addr v2)
{
	/* (values v1 v2 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

_g void typevalues_values3(addr *ret, addr v1, addr v2, addr v3)
{
	/* (values v1 v2 v3 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, v3, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

_g void typevalues_values4(addr *ret, addr v1, addr v2, addr v3, addr v4)
{
	/* (values v1 v2 v3 v4 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, v3, v4, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

_g void typevalues_values5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5)
{
	/* (values v1 v2 v3 v4 v5 &rest nil) */
	addr pos;

	GetTypeTable(&pos, Nil);
	list_heap(&v1, v1, v2, v3, v4, v5, NULL);
	type_values_heap(v1, Nil, pos, Nil, ret);
	SetStatusReadOnly(*ret);
}

_g void typevalues_values_va(addr *ret, ...)
{
	/* (values ... &rest nil) */
	addr args, type;
	va_list va;

	/* args */
	va_start(va, ret);
	list_alloc_stdarg(NULL, &args, va);
	va_end(va);
	/* type */
	GetTypeTable(&type, Nil);
	type_values_heap(args, Nil, type, Nil, ret);
	SetStatusReadOnly(*ret);
}

_g void typevalues_rest(addr *ret, addr type)
{
	/* (values &rest type) */

	type_values_heap(Nil, Nil, type, Nil, ret);
	SetStatusReadOnly(*ret);
}

/* type asterisk */
_g void type1aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type1_alloc(local, type, aster, ret);
}

_g void type2aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type2_alloc(local, type, aster, aster, ret);
}

_g void type3aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type3_alloc(local, type, aster, aster, aster, ret);
}

_g void type4aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	CheckType(aster, LISPTYPE_TYPE);
	type4_alloc(local, type, aster, aster, aster, aster, ret);
}

_g void type1aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type1aster_alloc(local, type, ret);
}

_g void type2aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type2aster_alloc(local, type, ret);
}

_g void type3aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type3aster_alloc(local, type, ret);
}

_g void type4aster_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type4aster_alloc(local, type, ret);
}

_g void type1aster_heap(enum LISPDECL type, addr *ret)
{
	type1aster_alloc(NULL, type, ret);
}

_g void type2aster_heap(enum LISPDECL type, addr *ret)
{
	type2aster_alloc(NULL, type, ret);
}

_g void type3aster_heap(enum LISPDECL type, addr *ret)
{
	type3aster_alloc(NULL, type, ret);
}

_g void type4aster_heap(enum LISPDECL type, addr *ret)
{
	type4aster_alloc(NULL, type, ret);
}


/*
 *  and/or
 */
_g void type2and_alloc(LocalRoot local, addr a, addr b, addr *ret)
{
	enum LISPDECL decl;
	addr array;

	CheckType2(a, LISPTYPE_TYPE, "type left error");
	CheckType2(b, LISPTYPE_TYPE, "type right error");
	decl = LispDecl(a);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		*ret = b;
		return;
	}
	if (decl == LISPDECL_NIL) {
		GetTypeTable(ret, Nil);
		return;
	}
	decl = LispDecl(b);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		*ret = a;
		return;
	}
	if (decl == LISPDECL_NIL) {
		GetTypeTable(ret, Nil);
		return;
	}

	vector4_alloc(local, &array, 2);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	type1_alloc(local, LISPDECL_AND, array, ret);
}

_g void type2or_alloc(LocalRoot local, addr a, addr b, addr *ret)
{
	enum LISPDECL decl;
	addr array;

	CheckType2(a, LISPTYPE_TYPE, "type left error");
	CheckType2(b, LISPTYPE_TYPE, "type right error");
	decl = LispDecl(a);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		GetTypeTable(ret, T);
		return;
	}
	if (decl == LISPDECL_NIL) {
		*ret = b;
		return;
	}
	decl = LispDecl(b);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		GetTypeTable(ret, T);
		return;
	}
	if (decl == LISPDECL_NIL) {
		*ret = a;
		return;
	}

	vector4_alloc(local, &array, 2);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	type1_alloc(local, LISPDECL_OR, array, ret);
}

_g void type3or_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 3);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	type1_alloc(local, LISPDECL_OR, array, ret);
}

_g void type4or_alloc(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 4);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	SetArrayA4(array, 3, d);
	type1_alloc(local, LISPDECL_OR, array, ret);
}

_g void type2and_local(LocalRoot local, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2and_alloc(local, a, b, ret);
}

_g void type2or_local(LocalRoot local, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2or_alloc(local, a, b, ret);
}

_g void type3or_local(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3or_alloc(local, a, b, c, ret);
}

_g void type4or_local(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4or_alloc(local, a, b, c, d, ret);
}

_g void type2and_heap(addr a, addr b, addr *ret)
{
	type2and_alloc(NULL, a, b, ret);
}

_g void type2or_heap(addr a, addr b, addr *ret)
{
	type2or_alloc(NULL, a, b, ret);
}

_g void type3or_heap(addr a, addr b, addr c, addr *ret)
{
	type3or_alloc(NULL, a, b, c, ret);
}

_g void type4or_heap(addr a, addr b, addr c, addr d, addr *ret)
{
	type4or_alloc(NULL, a, b, c, d, ret);
}


/*
 *  range
 */
_g void type1real_heap(enum LISPDECL type, addr value, addr *ret)
{
	type4_heap(type, Nil, value, Nil, value, ret);
}

_g void type4integer_heap(addr a, fixnum b, addr c, fixnum d, addr *ret)
{
	addr x, y;

	Check(a != Nil && a != T, "left1 error");
	Check(c != Nil && c != T, "right1 error");
	fixnum_heap(&x, b);
	fixnum_heap(&y, d);
	type4_heap(LISPDECL_INTEGER, a, x, c, y, ret);
}

_g void type2integer_ab_heap(addr a, fixnum b, addr *ret)
{
	addr x, aster;

	Check(a != Nil && a != T, "left1 error");
	GetTypeTable(&aster, Asterisk);
	fixnum_heap(&x, b);
	type4_heap(LISPDECL_INTEGER, a, x, aster, aster, ret);
}

_g void type2integer_cd_heap(addr c, fixnum d, addr *ret)
{
	addr y, aster;

	Check(c != Nil && c != T, "right1 error");
	GetTypeTable(&aster, Asterisk);
	fixnum_heap(&y, d);
	type4_heap(LISPDECL_INTEGER, aster, aster, c, y, ret);
}

_g void type4declf_heap(enum LISPDECL type, addr a, float b, addr c, float d, addr *ret)
{
	addr x, y;

	Check(a != Nil && a != T, "left1 error");
	Check(c != Nil && c != T, "right1 error");
	single_float_heap(&x, b);
	single_float_heap(&y, d);
	type4_heap(type, a, x, c, y, ret);
}

_g void type2declf_ab_heap(enum LISPDECL type, addr a, float b, addr *ret)
{
	addr x, aster;

	Check(a != Nil && a != T, "left1 error");
	GetTypeTable(&aster, Asterisk);
	single_float_heap(&x, b);
	type4_heap(type, a, x, aster, aster, ret);
}

_g void type2declf_cd_heap(enum LISPDECL type, addr c, float d, addr *ret)
{
	addr y, aster;

	Check(c != Nil && c != T, "right1 error");
	GetTypeTable(&aster, Asterisk);
	single_float_heap(&y, d);
	type4_heap(type, aster, aster, c, y, ret);
}

_g void type4float_heap(addr a, float b, addr c, float d, addr *ret)
{
	type4declf_heap(LISPDECL_FLOAT, a, b, c, d, ret);
}

_g void type2float_ab_heap(addr a, float b, addr *ret)
{
	type2declf_ab_heap(LISPDECL_FLOAT, a, b, ret);
}

_g void type2float_cd_heap(addr c, float d, addr *ret)
{
	type2declf_cd_heap(LISPDECL_FLOAT, c, d, ret);
}

_g void type4realf_heap(addr a, float b, addr c, float d, addr *ret)
{
	type4declf_heap(LISPDECL_REAL, a, b, c, d, ret);
}

_g void type2realf_ab_heap(addr a, float b, addr *ret)
{
	type2declf_ab_heap(LISPDECL_REAL, a, b, ret);
}

_g void type2realf_cd_heap(addr c, float d, addr *ret)
{
	type2declf_cd_heap(LISPDECL_REAL, c, d, ret);
}


/*
 *  vector
 */
_g void type_vector1_heap(size_t size, addr *ret)
{
	addr first, second;

	GetTypeTable(&first, Asterisk);
	vector4_heap(&second, 1);
	SetArrayA4(second, 0, intsizeh(size));
	type2_heap(LISPDECL_ARRAY, first, second, ret);
}

