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

int restartp(addr pos)
{
	return GetType(pos) == LISPTYPE_RESTART;
}

void restart_heap(addr *ret, addr name)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_RESTART, Restart_Size);
	SetUser(pos, 0);
	setenable_restart(pos, 1);
	SetRestart(pos, Restart_Name, name);

	*ret = pos;
}

void getname_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Name, ret);
}

void setname_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Name, value);
}

void getfunction_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Function, ret);
}

void setfunction_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Function, value);
}

void getinteractive_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Interactive, ret);
}

void setinteractive_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Interactive, value);
}

void getreport_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Report, ret);
}

void setreport_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Report, value);
}

void gettest_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Test, ret);
}

void settest_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Test, value);
}

void getcondition_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Condition, ret);
}

void setcondition_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Condition, value);
}

void getreference_restart(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RESTART);
	GetRestart(pos, Restart_Reference, ret);
}

void setreference_restart(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RESTART);
	SetRestart(pos, Restart_Reference, value);
}

void setescape_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 0, value);
	SetUser(pos, u);
}

int getescape_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 0);
}

void setenable_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 1, value);
	SetUser(pos, u);
}

int getenable_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 1);
}

void setredirect_restart(addr pos, int value)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);
	SetBitByte(u, 2, value);
	SetUser(pos, u);
}

int getredirect_restart(addr pos)
{
	byte u;

	CheckType(pos, LISPTYPE_RESTART);
	u = (byte)GetUser(pos);

	return GetBitByte(u, 2);
}


/*
 *  initialize
 */
void init_restart(void)
{
	init_restart_value();
}

