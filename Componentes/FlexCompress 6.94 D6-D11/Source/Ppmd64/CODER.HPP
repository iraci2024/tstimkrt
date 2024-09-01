/****************************************************************************
 *  This file is part of PPMd project                                       *
 *  Contents: 'Carryless rangecoder' by Dmitry Subbotin                     *
 *  Comments: this implementation is claimed to be a public domain          *
 ****************************************************************************/
/**********************  Original text  *************************************
////////   Carryless rangecoder (c) 1999 by Dmitry Subbotin   ////////

typedef unsigned int  uint;
typedef unsigned char uc;

#define  DO(n)     for (int _=0; _<n; _++)
#define  TOP       (1<<24)
#define  BOT       (1<<16)


class RangeCoder
{
 uint  low, code, range, passed;
 FILE  *f;

 void OutByte (uc c)           { passed++; fputc(c,f); }
 uc   InByte ()                { passed++; return fgetc(f); }

public:

 uint GetPassed ()             { return passed; }
 void StartEncode (FILE *F)    { f=F; passed=low=0;  range= (uint) -1; }
 void FinishEncode ()          { DO(4)  OutByte(low>>24), low<<=8; }
 void StartDecode (FILE *F)    { passed=low=code=0;  range= (uint) -1;
                                 f=F; DO(4) code= code<<8 | InByte();
                               }

 void Encode (uint cumFreq, uint freq, uint totFreq) {
    assert(cumFreq+freq<totFreq && freq && totFreq<=BOT);
    low  += cumFreq * (range/= totFreq);
    range*= freq;
    while ((low ^ low+range)<TOP || range<BOT && ((range= -low & BOT-1),1))
       OutByte(low>>24), range<<=8, low<<=8;
 }

 uint GetFreq (uint totFreq) {
   uint tmp= (code-low) / (range/= totFreq);
   if (tmp >= totFreq)  throw ("Input data corrupt"); // or force it to return
   return tmp;                                         // a valid value :)
 }

 void Decode (uint cumFreq, uint freq, uint totFreq) {
    assert(cumFreq+freq<totFreq && freq && totFreq<=BOT);
    low  += cumFreq*range;
    range*= freq;
    while ((low ^ low+range)<TOP || range<BOT && ((range= -low & BOT-1),1))
       code= code<<8 | InByte(), range<<=8, low<<=8;
 }
};
*****************************************************************************/

static struct SUBRANGE {
    DWORD LowCount, HighCount, scale;
} SubRange;
//static DWORD TOP, BOT;
static DWORD low, code, range;
const DWORD TOP=1 << 24, BOT=1 << 15;

// returns symbol from buffer
inline int getSym(char *buffer, unsigned int &count, unsigned int &size)
{
 if (count >= size)
  return EOF;
 return 0x000000FF & (*(buffer+count++));
}
// puts symbol to buffer
inline void putSym(int c, char *buffer, unsigned int &count)
{
  *(buffer+count++) = (char)c;
}

inline void ariInitEncoder()
{
    low=0;
    range=DWORD(-1);
}
#define ARI_ENC_NORMALIZE(buffer,count) {                                         \
    while ((low ^ (low+range)) < TOP || range < BOT &&                      \
            ((range= -low & (BOT-1)),1)) {                                  \
        putSym(low >> 24,buffer,count);                                             \
        range <<= 8;                        low <<= 8;                      \
    }                                                                       \
}
inline void ariEncodeSymbol()
{
    low += SubRange.LowCount*(range /= SubRange.scale);
    range *= SubRange.HighCount-SubRange.LowCount;
}
inline void ariShiftEncodeSymbol(UINT SHIFT)
{
    low += SubRange.LowCount*(range >>= SHIFT);
    range *= SubRange.HighCount-SubRange.LowCount;
}
#define ARI_FLUSH_ENCODER(buffer,count) {                                         \
    for (int i=0;i < 4;i++) {                                               \
        putSym(low >> 24,buffer,count);             low <<= 8;                      \
    }                                                                       \
}
#define ARI_INIT_DECODER(buffer,count,size) {                                          \
    low=code=0;                             range=DWORD(-1);                \
    for (int i=0;i < 4;i++)                 code=(code << 8) | getSym(buffer,count,size);\
}
#define ARI_DEC_NORMALIZE(buffer,count,size) {                                         \
    while ((low ^ (low+range)) < TOP || range < BOT &&                      \
            ((range= -low & (BOT-1)),1)) {                                  \
        code=(code << 8) | getSym(buffer,count,size);                                    \
        range <<= 8;                        low <<= 8;                      \
    }                                                                       \
}
inline int ariGetCurrentCount() {
    return (code-low)/(range /= SubRange.scale);
}
inline UINT ariGetCurrentShiftCount(UINT SHIFT) {
    return (code-low)/(range >>= SHIFT);
}
inline void ariRemoveSubrange()
{
    low += range*SubRange.LowCount;
    range *= SubRange.HighCount-SubRange.LowCount;
}
