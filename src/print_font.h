#ifndef __PRINT_COLOR_HEADER__
#define __PRINT_COLOR_HEADER__

#include "typedef.h"

enum print_font {
	print_font_reset,
	print_font_bold,
	print_font_faint,
	print_font_italic,
	print_font_underline,
	print_font_blink1,
	print_font_blink2,
	print_font_reverse,
	print_font_hide_in,
	print_font_hide_out
};

enum print_color {
	print_color_reset,
	print_color_black,
	print_color_red,
	print_color_green,
	print_color_yellow,
	print_color_blue,
	print_color_magenta,
	print_color_cyan,
	print_color_white,
	print_color_bright_black,
	print_color_bright_red,
	print_color_bright_green,
	print_color_bright_yellow,
	print_color_bright_blue,
	print_color_bright_magenta,
	print_color_bright_cyan,
	print_color_bright_white
};

typedef enum print_font PrintFont;
typedef enum print_color PrintColor;

#endif

