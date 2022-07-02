#ifndef __CLOSGET_HEADER__
#define __CLOSGET_HEADER__

#include "execute.h"
#include "typedef.h"

#define mop_value_get_ _n(mop_value_get_)
#define mop_value_set_ _n(mop_value_set_)
#define mop_value_boundp_ _n(mop_value_boundp_)
#define mop_value_exists_p_ _n(mop_value_exists_p_)
#define mop_value_makunbound_ _n(mop_value_makunbound_)
#define slot_value_get_ _n(slot_value_get_)
#define slot_value_set_ _n(slot_value_set_)
#define slot_boundp_ _n(slot_boundp_)
#define slot_exists_p_ _n(slot_exists_p_)
#define slot_makunbound_ _n(slot_makunbound_)
#define slot_value_get_common_ _n(slot_value_get_common_)
#define slot_value_set_common_ _n(slot_value_set_common_)
#define slot_boundp_common_ _n(slot_boundp_common_)
#define slot_exists_p_common_ _n(slot_exists_p_common_)
#define slot_makunbound_common_ _n(slot_makunbound_common_)
#define clos_elt_get_ _n(clos_elt_get_)
#define clos_elt_set_ _n(clos_elt_set_)
#define clos_elt_boundp_ _n(clos_elt_boundp_)
#define clos_elt_makunbound_ _n(clos_elt_makunbound_)
#define mop_value_structure_get_ _n(mop_value_structure_get_)
#define mop_value_structure_set_ _n(mop_value_structure_set_)
#define mop_value_structure_boundp_ _n(mop_value_structure_boundp_)
#define mop_value_structure_exists_p_ _n(mop_value_structure_exists_p_)
#define mop_value_structure_makunbound_ _n(mop_value_structure_makunbound_)

/*
 *  ClosAccess
 */
enum clos_access_result {
	clos_access_value,
	clos_access_unbound,
	clos_access_missing,
	clos_access_shared,
	clos_access_update,
	clos_access_error
};
typedef enum clos_access_result ClosAccess;
int clos_access_(addr pos, addr key, addr *ret, ClosAccess *rr);

/* MetaObject Protocol */
int mop_value_get_(Execute ptr, addr clos, addr pos, addr key);
int mop_value_set_(Execute ptr, addr clos, addr pos, addr key, addr value);
int mop_value_boundp_(Execute ptr, addr clos, addr pos, addr key);
int mop_value_exists_p_(Execute ptr, addr clos, addr pos, addr key);
int mop_value_makunbound_(Execute ptr, addr clos, addr pos, addr key);

/* access */
int slot_value_get_(Execute ptr, addr pos, addr key, addr *ret);
int slot_value_set_(Execute ptr, addr pos, addr key, addr value);
int slot_boundp_(Execute ptr, addr pos, addr key, int *ret);
int slot_exists_p_(Execute ptr, addr pos, addr key, int *ret);
int slot_makunbound_(Execute ptr, addr pos, addr key);

/* Common Lisp */
int slot_value_get_common_(Execute ptr, addr pos, addr key);
int slot_value_set_common_(Execute ptr, addr pos, addr key, addr value);
int slot_boundp_common_(Execute ptr, addr pos, addr key);
int slot_exists_p_common_(Execute ptr, addr pos, addr key);
int slot_makunbound_common_(Execute ptr, addr pos, addr key);

/* elt */
int clos_elt_get_(Execute ptr, addr clos, addr pos, addr key, size_t i, addr *ret);
int clos_elt_set_(Execute ptr, addr clos, addr pos, addr key, size_t i, addr value);
int clos_elt_boundp_(Execute ptr, addr clos, addr pos, addr key, size_t i, int *ret);
int clos_elt_makunbound_(Execute ptr, addr clos, addr pos, addr key, size_t i);

/* structure */
int mop_value_structure_get_(Execute ptr, addr pos, addr key, addr *ret);
int mop_value_structure_set_(Execute ptr, addr pos, addr key, addr value);
int mop_value_structure_boundp_(Execute ptr, addr pos, addr key, int *ret);
int mop_value_structure_exists_p_(Execute ptr, addr pos, addr key, int *ret);
int mop_value_structure_makunbound_(Execute ptr, addr pos, addr key);


/* old */
#include "closget_old.h"

#endif

