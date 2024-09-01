/****************************************************************************
 *  This file is part of PPMd project                                       *
 *  Written and distributed to public domain by Dmitry Shkarin 1997,        *
 *  1999-2000                                                               *
 *  Contents: memory allocation routines                                    *
 ****************************************************************************/


#pragma hdrstop
//#include "SubAlloc.h"
const UINT N1=4, N2=4, N3=4, N4=(128+3-1*N1-2*N2-3*N3)/4;
const UINT /*UNIT_SIZE=12,*/ N_INDEXES=N1+N2+N3+N4;
UINT UNIT_SIZE=0;

static long SubAllocatorSize=0;
static BYTE Indx2Units[N_INDEXES], Units2Indx[128];
static BYTE* HeapStart, * LoUnit, * HiUnit, * LastBreath;
static struct NODE { NODE* next; } FreeList[N_INDEXES];

inline void InsertNode(void* p,int indx) {
    ((NODE*) p)->next=FreeList[indx].next;  FreeList[indx].next=(NODE*) p;
}
inline void* RemoveNode(int indx) {
    NODE* RetVal=FreeList[indx].next;       FreeList[indx].next=RetVal->next;
    return RetVal;
}
inline UINT I2B(int indx) { return UNIT_SIZE*Indx2Units[indx]; }
inline void SplitBlock(void* pv,int OldIndx,int NewIndx)
{
    int i, UDiff=Indx2Units[OldIndx]-Indx2Units[NewIndx];
    BYTE* p=((BYTE*) pv)+I2B(NewIndx);
    if (Indx2Units[i=Units2Indx[UDiff-1]] != UDiff) {
        InsertNode(p,--i);                  p += I2B(i);
        UDiff -= Indx2Units[i];
    }
    InsertNode(p,Units2Indx[UDiff-1]);
}

DWORD _STDCALL GetUsedMemory()
{
    DWORD i, k, RetVal=SubAllocatorSize-(HiUnit-LoUnit);
    for (k=i=0;i < N_INDEXES;i++, k=0) {
        for (NODE* pn=FreeList+i;(pn=pn->next) != NULL;k++)
                ;
        RetVal -= UNIT_SIZE*Indx2Units[i]*k;
    }
    if ( LastBreath )                       RetVal -= 128*128*UNIT_SIZE;
    return (RetVal >> 2);
}


void _STDCALL StopSubAllocator()
{

    if ( SubAllocatorSize ) {
        SubAllocatorSize=0;
#ifdef FOR_DELPHI
     aa_free((char *)HeapStart);
     HeapStart = NULL;
#else
        delete[] HeapStart;
#endif
    }

}

BOOL _STDCALL StartSubAllocator(int SASize)
{

    DWORD t=SASize<<20;
    if (SubAllocatorSize == t)
     return TRUE;
    StopSubAllocator();

#ifdef FOR_DELPHI
    HeapStart = aa_malloc(t);
	 if (HeapStart == NULL)
#else
    if ((HeapStart=new BYTE[t]) == NULL)
#endif
     return FALSE;
    SubAllocatorSize=t;
    return TRUE;

}
void InitSubAllocator()
{
    int i, k;
    memset(FreeList,0,sizeof(FreeList));
    HiUnit=(LoUnit=HeapStart)+UNIT_SIZE*(SubAllocatorSize/UNIT_SIZE);
    LastBreath=LoUnit;                      LoUnit += 128*128*UNIT_SIZE;
    for (i=0,k=1;i < N1     ;i++,k += 1)    Indx2Units[i]=k;
    for (k++;i < N1+N2      ;i++,k += 2)    Indx2Units[i]=k;
    for (k++;i < N1+N2+N3   ;i++,k += 3)    Indx2Units[i]=k;
    for (k++;i < N1+N2+N3+N4;i++,k += 4)    Indx2Units[i]=k;
    for (k=i=0;k < 128;k++) {
        i += (Indx2Units[i] < k+1);         Units2Indx[k]=i;
    }
}
void* _FASTCALL AllocUnitsRare(int NU)
{


    int i, indx=Units2Indx[NU-1];
    if ( FreeList[indx].next )
     return RemoveNode(indx);
    void* RetVal=LoUnit;
          LoUnit += I2B(indx);
    if (LoUnit <= HiUnit)
     return RetVal;
    if ( LastBreath )
    {
        for (i=0;i < 128;i++)
        {
            InsertNode(LastBreath,N_INDEXES-1);
            LastBreath += 128*UNIT_SIZE;
        }
        LastBreath=NULL;
    }
    LoUnit -= I2B(indx);                    i=indx;
    do {
        if (++i == N_INDEXES)
         return NULL;
    } while ( !FreeList[i].next );
    SplitBlock(RetVal=RemoveNode(i),i,indx);
    return RetVal;

}
void* AllocContext()
{
    if (HiUnit != LoUnit)
     return (HiUnit -= UNIT_SIZE);
    return AllocUnitsRare(1);
}
void* _FASTCALL ExpandUnits(void* OldPtr,int OldNU)
{
    int i0=Units2Indx[OldNU-1], i1=Units2Indx[OldNU-1+1];
    if (i0 == i1)                           return OldPtr;
    void* ptr=AllocUnitsRare(OldNU+1);
    if ( ptr ) {
        memcpy(ptr,OldPtr,I2B(i0));
                InsertNode(OldPtr,i0);
    }
    return ptr;
}
void* _FASTCALL ShrinkUnits(void* OldPtr,int OldNU,int NewNU)
{
    int i0=Units2Indx[OldNU-1], i1=Units2Indx[NewNU-1];
    if (i0 == i1)                           return OldPtr;
    if ( FreeList[i1].next ) {
        void* ptr=RemoveNode(i1);
                memcpy(ptr,OldPtr,I2B(i1));
        InsertNode(OldPtr,i0);              return ptr;
    } else {
        SplitBlock(OldPtr,i0,i1);           return OldPtr;
    }
}
void _FASTCALL FreeUnits(void* ptr,int OldNU)
{
    InsertNode(ptr,Units2Indx[OldNU-1]);
}