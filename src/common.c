/*
 *  ANSI COMMON LISP Function
 */
#include "common_header.h"

void init_common_eval(void);
void init_common_types(void);
void init_common_data(void);
void init_common_iteration(void);
void init_common_objects(void);
void init_common_structures(void);
void init_common_conditions(void);
void init_common_symbols(void);
void init_common_packages(void);
void init_common_numbers(void);
void init_common_characters(void);
void init_common_conses(void);
void init_common_arrays(void);
void init_common_strings(void);
void init_common_sequences(void);
void init_common_hashtables(void);
void init_common_filenames(void);
void init_common_files(void);
void init_common_streams(void);
void init_common_printer(void);
void init_common_reader(void);
void init_common_system(void);
void init_common_environment(void);

void build_common_eval(void);
void build_common_types(void);
void build_common_data(void);
void build_common_iteration(void);
void build_common_objects(void);
void build_common_structures(void);
void build_common_conditions(void);
void build_common_symbols(void);
void build_common_packages(void);
void build_common_numbers(void);
void build_common_characters(void);
void build_common_conses(void);
void build_common_arrays(void);
void build_common_strings(void);
void build_common_sequences(void);
void build_common_hashtables(void);
void build_common_filenames(void);
void build_common_files(void);
void build_common_streams(void);
void build_common_printer(void);
void build_common_reader(void);
void build_common_system(void);
void build_common_environment(void);

void init_common(void)
{
	/* 3. Evaluation and Compilation */
	init_common_eval();
	/* 4. Types and Classes */
	init_common_types();
	/* 5. Data and Control Flow */
	init_common_data();
	/* 6. Iteration */
	init_common_iteration();
	/* 7. Objects */
	init_common_objects();
	/* 8. Structures */
	init_common_structures();
	/* 9. Conditions */
	init_common_conditions();
	/* 10. Symbols */
	init_common_symbols();
	/* 11. Packages */
	init_common_packages();
	/* 12. Numbers */
	init_common_numbers();
	/* 13. Characters */
	init_common_characters();
	/* 14. Conses */
	init_common_conses();
	/* 15. Arrays */
	init_common_arrays();
	/* 16. Strings */
	init_common_strings();
	/* 17. Sequences */
	init_common_sequences();
	/* 18. Hash Tables */
	init_common_hashtables();
	/* 19. Filenames */
	init_common_filenames();
	/* 20. Files */
	init_common_files();
	/* 21. Streams */
	init_common_streams();
	/* 22. Printer */
	init_common_printer();
	/* 23. Reader */
	init_common_reader();
	/* 24. System Construction */
	init_common_system();
	/* 25. Environment */
	init_common_environment();
}

void build_common(void)
{
	/* 3. Evaluation and Compilation */
	build_common_eval();
	/* 4. Types and Classes */
	build_common_types();
	/* 5. Data and Control Flow */
	build_common_data();
	/* 6. Iteration */
	build_common_iteration();
	/* 7. Objects */
	build_common_objects();
	/* 8. Structures */
	build_common_structures();
	/* 9. Conditions */
	build_common_conditions();
	/* 10. Symbols */
	build_common_symbols();
	/* 11. Packages */
	build_common_packages();
	/* 12. Numbers */
	build_common_numbers();
	/* 13. Characters */
	build_common_characters();
	/* 14. Conses */
	build_common_conses();
	/* 15. Arrays */
	build_common_arrays();
	/* 16. Strings */
	build_common_strings();
	/* 17. Sequences */
	build_common_sequences();
	/* 18. Hash Tables */
	build_common_hashtables();
	/* 19. Filenames */
	build_common_filenames();
	/* 20. Files */
	build_common_files();
	/* 21. Streams */
	build_common_streams();
	/* 22. Printer */
	build_common_printer();
	/* 23. Reader */
	build_common_reader();
	/* 24. System Construction */
	build_common_system();
	/* 25. Environment */
	build_common_environment();
	/* After settings */
	build_common_after_settings();
}

