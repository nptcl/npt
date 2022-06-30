#ifndef __CLOS_SLOT_HEADER__
#define __CLOS_SLOT_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

struct slot_struct {
	size_t location, access;
};

#define struct_slot _n(struct_slot)
#define slotp _n(slotp)
#define slot_alloc_ _n(slot_alloc_)
#define slot_local_ _n(slot_local_)
#define slot_heap_ _n(slot_heap_)
#define slot_copy_alloc_ _n(slot_copy_alloc_)
#define slot_copy_local_ _n(slot_copy_local_)
#define slot_copy_heap_ _n(slot_copy_heap_)

#define slot_class_p_ _n(slot_class_p_)
#define slot_instance_p_ _n(slot_instance_p_)
#define slot_set_class_ _n(slot_set_class_)
#define slot_set_instance_ _n(slot_set_instance_)
#define slot_set_allocation_ _n(slot_set_allocation_)

struct slot_struct *struct_slot(addr pos);
int slotp(addr pos);
int slot_alloc_(Execute ptr, LocalRoot local, addr *ret);
int slot_local_(Execute ptr, addr *ret);
int slot_heap_(Execute ptr, addr *ret);
int slot_copy_alloc_(Execute ptr, LocalRoot local, addr *ret, addr slot);
int slot_copy_local_(Execute ptr, addr *ret, addr slot);
int slot_copy_heap_(Execute ptr, addr *ret, addr slot);

int slot_class_p_(Execute ptr, addr pos, int *ret);
int slot_instance_p_(Execute ptr, addr pos, int *ret);
int slot_set_class_(Execute ptr, addr pos);
int slot_set_instance_(Execute ptr, addr pos);
int slot_set_allocation_(Execute ptr, addr pos, addr value);

#endif

