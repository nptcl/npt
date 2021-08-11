#include "condition.h"
#include "structure_change.h"
#include "typedef.h"

static int structure_change_check_slots_(struct defstruct *str)
{
	return fmtw_("The structure ~S already exists.", str->name, NULL);
}

int structure_change_(struct defstruct *str)
{
	Return(structure_change_check_slots_(str));

	return 0;
}

