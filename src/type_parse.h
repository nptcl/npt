#ifndef __TYPE_PARSE_HEADER__
#define __TYPE_PARSE_HEADER__

#include "type.h"

addr type_allocr(LocalRoot local, enum LISPDECL type, byte size);
addr type_localr(LocalRoot local, enum LISPDECL type, byte size);
addr type_heapr(enum LISPDECL type, byte size);
void type_alloc(LocalRoot local, addr *ret, enum LISPDECL type, byte size);
void type_local(LocalRoot local, addr *ret, enum LISPDECL type, byte size);
void type_heap(addr *ret, enum LISPDECL type, byte size);

void copy_no_recursive_type_alloc(LocalRoot local, addr *ret, addr pos);
void copy_no_recursive_type_local(LocalRoot local, addr *ret, addr pos);
void copy_no_recursive_type_heap(addr *ret, addr pos);

void copy_no_recursive_typeonly_alloc(LocalRoot local, addr *ret, addr pos);
void copy_no_recursive_typeonly_local(LocalRoot local, addr *ret, addr pos);
void copy_no_recursive_typeonly_heap(addr *ret, addr pos);

void type_object1(LocalRoot local, enum LISPDECL type, addr a, addr *ret);
void type_object2(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret);
void type_object3(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr *ret);
void type_object4(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret);

void type_object1_not(LocalRoot local, enum LISPDECL type, addr a, addr *ret);
void type_object2_not(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret);
void type_object3_not(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr *ret);
void type_object4_not(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret);

void type_empty(LocalRoot local, enum LISPDECL type, addr *ret);
void type_empty_not(LocalRoot local, enum LISPDECL type, addr *ret);

addr type_asterisk_allocr(LocalRoot local);
addr type_asterisk_localr(LocalRoot local);
addr type_asterisk_heapr(void);
void type_asterisk_alloc(LocalRoot local, addr *ret);
void type_asterisk_local(LocalRoot local, addr *ret);
void type_asterisk_heap(addr *ret);
int function_decl_p(enum LISPDECL decl);
int function_type_p(addr pos);
int asterisk_or_t_decl_p(enum LISPDECL decl);
int asterisk_or_t_p(addr pos);
int function_asterisk_p(addr pos);
int asterisk_p(addr pos);

addr type_nil_allocr(LocalRoot local);
addr type_nil_localr(LocalRoot local);
addr type_nil_heapr(void);
void type_nil_alloc(LocalRoot local, addr *ret);
void type_nil_local(LocalRoot local, addr *ret);
void type_nil_heap(addr *ret);

addr type_t_allocr(LocalRoot local);
addr type_t_localr(LocalRoot local);
addr type_t_heapr(void);
void type_t_alloc(LocalRoot local, addr *ret);
void type_t_local(LocalRoot local, addr *ret);
void type_t_heap(addr *ret);

void type_bool_alloc(LocalRoot local, int value, addr *ret);
void type_bool_local(LocalRoot local, int value, addr *ret);
void type_bool_heap(int value, addr *ret);

void type_realvalue(LocalRoot local, enum LISPDECL type, addr value, addr *ret);
void type_aster1(LocalRoot local, enum LISPDECL type, addr *ret);
void type_aster2(LocalRoot local, enum LISPDECL type, addr *ret);
void type_aster3(LocalRoot local, enum LISPDECL type, addr *ret);
void type_aster4(LocalRoot local, enum LISPDECL type, addr *ret);

void type_intrange(addr left1, fixnum left2, addr right1, fixnum right2, addr *ret);
void type_intrange_left(addr left1, fixnum left2, addr *ret);
void type_intrange_right(addr right1, fixnum right2, addr *ret);
void type_fixnum_alloc(LocalRoot local, addr *ret);
void type_floatrange_left(addr left1, single_float left2, addr *ret);
void type_realrange_float(addr left1,
		single_float left2, addr right1, single_float right2, addr *ret);
void type_syscall(addr args, addr values, addr *ret);
void type_compiled_function_asterisk(LocalRoot local, addr *ret);
void type_function_asterisk(LocalRoot local, addr *ret);
void type_and(LocalRoot local, addr left, addr right, addr *ret);
void type_or(LocalRoot local, addr left, addr right, addr *ret);
void type_or3(LocalRoot local, addr a, addr b, addr c, addr *ret);
void type_or4(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
void type_function_heap(addr arg, addr values, addr *ret);
void type_compiled_heap(addr arg, addr values, addr *ret);
void type_member_heap(addr *ret, ...);

void setnotdecl_value(addr pos, int value);
void setnotdecl_object(addr left, addr right);
void reversenotdecl(addr pos);

int float_value_p(enum LISPDECL type);
int range_value_p(enum LISPDECL type);
int range_type_p(addr pos);
int subtype_real_p(enum LISPDECL left, enum LISPDECL right);

void type_signed_byte(LocalRoot local, addr *ret, int value);
void type_unsigned_byte(LocalRoot local, addr *ret, int value);

enum ARRAY_TYPE upgraded_array_direct(addr type, int *size);
void make_upgraded_array_object_heap(addr *ret, enum ARRAY_TYPE type, int size);
void upgraded_array_type_alloc(LocalRoot local, addr *ret, addr type);
void upgraded_array_object_heap(addr *ret, addr type);
void type_object2_array(LocalRoot local,
		enum LISPDECL type, addr pos1, addr pos2, addr *ret);
int equal_array_type(addr left, addr right);

void parse_type_values_alloc(LocalRoot local, addr *ret, addr type);
void parse_type_values_local(LocalRoot local, addr *ret, addr type);
void parse_type_values_heap(addr *ret, addr type);

void parse_type_alloc(LocalRoot local, addr *ret, addr type);
void parse_type_local(LocalRoot local, addr *ret, addr type);
void parse_type_heap(addr *ret, addr type);

void parse_type_not_alloc(LocalRoot local, addr *ret, addr type);
void parse_type_not_local(LocalRoot local, addr *ret, addr type);
void parse_type_not_heap(addr *ret, addr type);

void parse_type_no_asterisk_alloc(LocalRoot local, addr *ret, addr type);
void parse_type_no_asterisk_local(LocalRoot local, addr *ret, addr type);
void parse_type_no_asterisk_heap(addr *ret, addr type);

void type_throw_alloc(LocalRoot local, addr *ret, addr pos);
void type_throw_local(LocalRoot local, addr *ret, addr pos);
void type_throw_heap(addr *ret, addr pos);

enum CONSTANT_INDEX typedecl_classname(enum LISPDECL type);

void init_type_class(void);

#endif

