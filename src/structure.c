#include "clos.h"
#include "clos_class.h"
#include "cons.h"
#include "constant.h"
#include "structure.h"
#include "symbol.h"

int structure_instance_p(addr pos)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_CONDITION, &super);
	return clos_subtype_p(pos, super);
}

int equalp_structure(addr left, addr right)
{
	return 0;
}

int equalrt_structure(addr left, addr right)
{
	return 0;
}


/*
 *  ensure-structure
 */
static void structure_slots(addr *ret, addr list)
{
	addr pos, name, init, type, readonly;

	while (list != Nil) {
		getcons(list, &pos, &list);
		list_bind(pos, &name, &init, &type, &readonly, NULL);
		slot_heap(&pos);
		SetNameSlot(pos, name);
		SetFormSlot(pos, init);
		SetTypeSlot(pos, type);
	}
}

static void structure_instance(addr *ret, addr name)
{
	addr clos, instance;

	GetConst(CLOS_STRUCTURE_CLASS, &clos);
	clos_instance_heap(clos, &instance);
	stdset_class_name(instance, name);
	SetClassOfClos(instance, clos);
	*ret = instance;
}

void ensure_structure_common(Execute ptr, addr name, addr slots, addr rest)
{
	addr instance, doc;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc)) doc = Unbound;

	/* make slots */
	structure_slots(&slots, slots);
	/* make clos */
	structure_instance(&instance, name);
	/* regist */
	clos_define_class(name, instance);
}


/*
 *  copy-structure
 */
void copy_structure_common(addr var, addr *ret)
{
	*ret = Nil;
}

