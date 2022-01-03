#ifdef __cplusplus
#include <festival/festival.h>

extern "C" {
#endif

void festival_c_initialize(int load_init_files, int heapsize)
{
  festival_initialize(load_init_files, heapsize);
}

int festival_c_say_text(const char *text)
{
  return festival_say_text(text);
}

#ifdef __cplusplus
}
#endif
