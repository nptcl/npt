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
#define slot_alloc _n(slot_alloc)
#define slot_local _n(slot_local)
#define slot_heap _n(slot_heap)
#define slot_copy_alloc _n(slot_copy_alloc)
#define slot_copy_local _n(slot_copy_local)
#define slot_copy_heap _n(slot_copy_heap)

#define slot_class_p _n(slot_class_p)
#define slot_instance_p _n(slot_instance_p)
#define slot_set_class _n(slot_set_class)
#define slot_set_instance _n(slot_set_instance)
#define slot_set_allocation_ _n(slot_set_allocation_)

struct slot_struct *struct_slot(addr pos);
int slotp(addr pos);
void slot_alloc(LocalRoot local, addr *ret);
void slot_local(LocalRoot local, addr *ret);
void slot_heap(addr *ret);
void slot_copy_alloc(LocalRoot local, addr *ret, addr slot);
void slot_copy_local(LocalRoot local, addr *ret, addr slot);
void slot_copy_heap(addr *ret, addr slot);

int slot_class_p(addr pos);
int slot_instance_p(addr pos);
void slot_set_class(addr pos);
void slot_set_instance(addr pos);
int slot_set_allocation_(addr pos, addr value);

#endif

