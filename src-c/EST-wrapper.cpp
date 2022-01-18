/* Wrap selected parts of the speech_tools EST library.  We really just want
 * EST_String.h and EST_Wave.h ... maybe EST_Wave_aux too?
 */

#ifdef __cplusplus
#include <speech_tools/EST_Wave.h>
#include <speech_tools/EST_String.h>
extern "C" {
#endif

#include "EST-wrapper.h"

ESTWave *newWave(void)
{
  return (ESTWave *)new EST_Wave();
}

void freeWave(ESTWave *wave)
{
  delete static_cast<EST_String*>((void *)wave);
}

int sampleRate(ESTWave *wave)
{
  return static_cast<EST_Wave*>((void *)wave)->sample_rate();
}

void setSampleRate(ESTWave *wave, int rate)
{
  static_cast<EST_Wave*>((void *)wave)->set_sample_rate(rate);
}

int sampleCount(ESTWave *wave)
{
  return static_cast<EST_Wave*>((void *)wave)->length();
}

void copySample(int n, short *buf, ESTWave *wave)
{
  /* I thought '1' was originally where the count went, but now I think
	 it is the channel number? */
  static_cast<EST_Wave*>((void *)wave)->copy_sample(0, buf,0 ,n);
}



#ifdef __cplusplus
}
#endif
