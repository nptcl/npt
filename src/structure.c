#include "clos_standard.h"
#include "constant.h"
#include "structure.h"

int structure_instance_p(addr pos)
{
	addr super;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_CONDITION, &super);
	return std_subtype_p(pos, super);
}

int equalp_structure(addr left, addr right)
{
	return 0;
}

int equalrt_structure(addr left, addr right)
{
	return 0;
}

