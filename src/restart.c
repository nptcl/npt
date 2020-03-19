#include "heap.h"
#include "restart.h"
#include "restart_value.h"

enum Restart_Index {
	Restart_Name,
	Restart_Function,
	Restart_Interactive,
	Restart_Report,
	Restart_Test,
	Restart_Condition,
	Restart_Reference,
	Restart_Size
};

#define RefRestart		RefArrayA2
#define GetRestart		GetArrayA2
#define SetRestart		SetArrayA2

_g void restart_heap(addr *ret, addr name)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_RESTART, Restart_Size);
	SetUser(pos, 0);
	setenable_restart(pos, 1);
	SetRestart(pos, Restart_Name, name);

	*ret = pos;
}

_g void getname_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Name, ret);
}

_g void setname_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Name, value);
}

_g void getfunction_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Function, ret);
}

_g void setfunction_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Function, value);
}

_g void getinteractive_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Interactive, ret);
}

_g void setinteractive_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Interactive, value);
}

_g void getreport_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Report, ret);
}

_g void setreport_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Report, value);
}

_g void gettest_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Test, ret);
}

_g void settest_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Test, value);
}

_g void getcondition_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Condition, ret);
}

_g void setcondition_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Condition, value);
}

_g void getreference_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Reference, ret);
}

_g void setreference_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Reference, value);
}

_g void setescape_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 0, value);
	SetUser(pos, u);
}

_g int getescape_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 0);
}

_g void setenable_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 1, value);
	SetUser(pos, u);
}

_g int getenable_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 1);
}

_g void setredirect_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 2, value);
	SetUser(pos, u);
}

_g int getredirect_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 2);
}


/*
 *  initialize
 */
_g void init_restart(void)
{
	init_restart_value();
}

