/****************************************************************************
 *  This file is part of PPMd project                                       *
 *  Written and distributed to public domain by Dmitry Shkarin 1997,        *
 *  1999-2000                                                               *
 *  Contents: compilation parameters and miscelaneous definitions           *
 *  Comments: system & compiler dependent file                              *
 ****************************************************************************/
#if !defined(_PPMDTYPE_H_)
#define _PPMDTYPE_H_

const __int32 MAX_O=16;                         /* maximum allowed model order */

//#define _WIN32_ENVIRONMENT_
//#include <wtypes.h>
typedef __int32   BOOL;
#define FALSE 0
#define TRUE  1
typedef unsigned char  BYTE;
typedef unsigned __int16 WORD;
typedef unsigned __int32  DWORD;
typedef unsigned __int32   UINT;
typedef signed __int32    LONG;

#define _FASTCALL __fastcall
#define _STDCALL  __stdcall

#define _PACK_ATTR

template <class TMP_TYPE>
inline TMP_TYPE CLAMP(const TMP_TYPE& X,const TMP_TYPE& LoX,const TMP_TYPE& HiX) { return (X >= LoX)?((X <= HiX)?(X):(HiX)):(LoX); }
template <class TMP_TYPE>
inline void SWAP(TMP_TYPE& t1,TMP_TYPE& t2) { TMP_TYPE tmp=t1; t1=t2; t2=tmp; }
#ifdef FOR_DELPHI
//extern void memcpy (void *dest, void *source, __int32 count);
//extern void memset (void *dest, BYTE b, __int32 count);
//extern BYTE* aa_malloc(__int32 count);
//extern void aa_free(char *buffer);
extern "C" {
	void _FASTCALL  memcpy (void *dest, void *source, __int32 count);
	void _FASTCALL  memset (void *dest, BYTE b, __int32 count);
	BYTE* _FASTCALL aa_malloc(__int32 count);
	void _FASTCALL  aa_free(char *buffer);
}
#endif
#endif /* !defined(_PPMDTYPE_H_) */
