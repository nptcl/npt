#ifndef __TERME_HEADER__
#define __TERME_HEADER__

#include "execute.h"
#include "print_font.h"
#include "prompt.h"
#include "typedef.h"

#define init_terme _n(init_terme)
#define build_terme _n(build_terme)
#define begin_terme _n(begin_terme)
#define end_terme _n(end_terme)
#define prompt_terme_ _n(prompt_terme_)
#define readline_terme_ _n(readline_terme_)
#define clear_terme_ _n(clear_terme_)
#define font_terme _n(font_terme)
#define text_color_terme _n(text_color_terme)
#define back_color_terme _n(back_color_terme)

void init_terme(void);
void build_terme(void);
int begin_terme(void);
int end_terme(void);
int prompt_terme_(Execute ptr, addr pos, PromptMode mode);
int readline_terme_(Execute ptr, addr *ret);
int clear_terme_(Execute ptr);
int font_terme(Execute ptr, PrintFont value);
int text_color_terme(Execute ptr, PrintColor value);
int back_color_terme(Execute ptr, PrintColor value);

#endif

