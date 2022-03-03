#include "condition.h"
#include "process_calltype.h"
#include "strtype.h"
#include "typedef.h"

#define ProcessCallType(pos, ret, name) { \
	int __check; \
	Return(string_designer_equalp_char_(pos, #name, &__check)); \
	if (__check) { \
		return Result(ret, CallBind_##name); \
	} \
}

int process_calltype_(addr pos, enum CallBind_index *ret)
{
	ProcessCallType(pos, ret, error);
	ProcessCallType(pos, ret, code);
	ProcessCallType(pos, ret, macro);
	ProcessCallType(pos, ret, none);
	ProcessCallType(pos, ret, any);
	ProcessCallType(pos, ret, empty);
	ProcessCallType(pos, ret, rest);
	ProcessCallType(pos, ret, dynamic);
	ProcessCallType(pos, ret, var1);
	ProcessCallType(pos, ret, var2);
	ProcessCallType(pos, ret, var3);
	ProcessCallType(pos, ret, var4);
	ProcessCallType(pos, ret, var5);
	ProcessCallType(pos, ret, var6);
	ProcessCallType(pos, ret, opt1);
	ProcessCallType(pos, ret, opt2);
	ProcessCallType(pos, ret, opt3);
	ProcessCallType(pos, ret, opt4);
	ProcessCallType(pos, ret, opt5);
	ProcessCallType(pos, ret, var1opt1);
	ProcessCallType(pos, ret, var2opt1);
	ProcessCallType(pos, ret, var3opt1);
	ProcessCallType(pos, ret, var4opt1);
	ProcessCallType(pos, ret, var5opt1);
	ProcessCallType(pos, ret, var1opt2);
	ProcessCallType(pos, ret, var2opt2);
	ProcessCallType(pos, ret, var2opt3);
	ProcessCallType(pos, ret, var1rest);
	ProcessCallType(pos, ret, var2rest);
	ProcessCallType(pos, ret, var3rest);
	ProcessCallType(pos, ret, var4rest);
	ProcessCallType(pos, ret, opt1rest);
	ProcessCallType(pos, ret, var1dynamic);
	ProcessCallType(pos, ret, var2dynamic);
	ProcessCallType(pos, ret, var3dynamic);
	ProcessCallType(pos, ret, var4dynamic);
	ProcessCallType(pos, ret, opt1dynamic);

	ProcessCallType(pos, ret, extend_macro);
	ProcessCallType(pos, ret, extend_rest);
	ProcessCallType(pos, ret, extend_dynamic);
	ProcessCallType(pos, ret, extend_any);
	ProcessCallType(pos, ret, extend_empty);
	ProcessCallType(pos, ret, extend_var1);
	ProcessCallType(pos, ret, extend_var2);
	ProcessCallType(pos, ret, extend_var3);
	ProcessCallType(pos, ret, extend_var4);
	ProcessCallType(pos, ret, extend_var5);
	ProcessCallType(pos, ret, extend_var6);
	ProcessCallType(pos, ret, extend_opt1);
	ProcessCallType(pos, ret, extend_opt2);
	ProcessCallType(pos, ret, extend_opt3);
	ProcessCallType(pos, ret, extend_var1opt1);
	ProcessCallType(pos, ret, extend_var1opt2);
	ProcessCallType(pos, ret, extend_var1opt3);
	ProcessCallType(pos, ret, extend_var2opt1);
	ProcessCallType(pos, ret, extend_var2opt2);
	ProcessCallType(pos, ret, extend_var2opt3);
	ProcessCallType(pos, ret, extend_var3opt1);
	ProcessCallType(pos, ret, extend_var3opt2);
	ProcessCallType(pos, ret, extend_var3opt3);
	ProcessCallType(pos, ret, extend_var1rest);
	ProcessCallType(pos, ret, extend_var2rest);
	ProcessCallType(pos, ret, extend_var3rest);
	ProcessCallType(pos, ret, extend_var1dynamic);
	ProcessCallType(pos, ret, extend_var2dynamic);
	ProcessCallType(pos, ret, extend_var3dynamic);

	/* error */
	*ret = CallBind_error;
	return fmte_("Invalid calltype, ~S.", pos, NULL);
}

