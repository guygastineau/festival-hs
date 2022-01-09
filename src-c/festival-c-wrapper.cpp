#ifdef __cplusplus
#include <festival/festival.h>

extern "C" {
#endif

#include "festival-wrapper.h"

void festival_c_initialize(int load_init_files, int heapsize)
{
  festival_initialize(load_init_files, heapsize);
}

void festival_c_init_lang(const char *language)
{
  festival_init_lang(language);
}

int festival_c_say_text(const char *text)
{
  return festival_say_text(text);
}

int festival_c_text_to_wave(const char *text, ESTWave *wave)
{
  return festival_text_to_wave(text, *static_cast<EST_Wave*>((void *)wave));
}

void festival_c_tidy_up()
{
  festival_tidy_up();
}

#ifdef __cplusplus
}
#endif
