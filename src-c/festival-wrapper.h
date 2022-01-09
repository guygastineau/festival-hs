#ifndef FESTIVAL_WRAPPER_H
#define FESTIVAL_WRAPPER_H

#include "EST-wrapper.h"

void festival_c_initialize(int load_init_files, int heapsize);
void festival_c_init_lang(const char *language);
int festival_c_say_text(const char *text);
int festival_c_text_to_wave(const char *text, ESTWave *wave);
void festival_c_tidy_up();

#endif /* FESTIVAL_WRAPPER_H */
