#ifndef EST_WRAPPER_H
#define EST_WRAPPER_H

/* This is dirtier than I wanted, but more pointers would be a silly waste. */
typedef void *ESTWave;

ESTWave *newWave(void);
void freeWave(ESTWave *wave);
int sampleRate(ESTWave *wave);
void setSampleRate(ESTWave *wave, int rate);
int sampleCount(ESTWave *wave);
void copySample(int n, short *buf, ESTWave *wave);

#endif /* EST_WRAPPER_H */
