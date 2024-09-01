// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcAMQP_Helpers.pas' rev: 35.00 (Windows)

#ifndef Sgcamqp_helpersHPP
#define Sgcamqp_helpersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcAMQP_Classes.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcamqp_helpers
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPConnection(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPConnectionValue(const Sgcamqp_classes::TsgcAMQPMethod aValue);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPChannel(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPChannelValue(const Sgcamqp_classes::TsgcAMQPMethod aValue);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPExchange(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPExchangeValue(const Sgcamqp_classes::TsgcAMQPMethod aValue);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPQueue(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPQueueValue(const Sgcamqp_classes::TsgcAMQPMethod aValue);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPBasic(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPBasicValue(const Sgcamqp_classes::TsgcAMQPMethod aValue);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPTX(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPTXValue(const Sgcamqp_classes::TsgcAMQPMethod aValue);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPClass __fastcall sgcGetAMQPClass(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPClassValue(const Sgcamqp_classes::TsgcAMQPClass aValue);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall sgcReadAMQPBytes(System::Classes::TStream* const aStream, int aSize);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPBytes(System::Classes::TStream* const aStream, const System::DynamicArray<System::Byte> aBytes);
extern DELPHI_PACKAGE void __fastcall DoRaiseAMQPException(const int aCode);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPFrameType __fastcall sgcGetAMQPFrameType(const int aValue);
extern DELPHI_PACKAGE int __fastcall sgcGetAMQPFrameTypeValue(const Sgcamqp_classes::TsgcAMQPFrameType aValue);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcReadAMQPLongString(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcReadAMQPShortString(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall sgcReadAMQPLongStringAsBytes(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall sgcReadAMQPStringList(const System::DynamicArray<System::Byte> aBytes, int &aOffset, System::Classes::TStringList* &aList);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcReadAMQPFieldValue(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcReadAMQPFieldTable(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE bool __fastcall sgcReadAMQPBoolean(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::Word __fastcall sgcReadAMQPUInt16(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE unsigned __fastcall sgcReadAMQPUInt32(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE unsigned __int64 __fastcall sgcReadAMQPUInt64(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE double __fastcall sgcReadAMQPDecimal(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::Int8 __fastcall sgcReadAMQPInt8(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::Byte __fastcall sgcReadAMQPUInt8(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE short __fastcall sgcReadAMQPInt16(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE int __fastcall sgcReadAMQPInt32(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE __int64 __fastcall sgcReadAMQPInt64(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE float __fastcall sgcReadAMQPSingle(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::Byte __fastcall sgcReadAMQPByte(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE double __fastcall sgcReadAMQPDouble(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcReadAMQPFieldArray(const System::DynamicArray<System::Byte> aBytes, int &aOffset);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPStringList(System::Classes::TStringList* const aList, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPShortString(const System::UnicodeString aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPLongString(const System::UnicodeString aValue, System::DynamicArray<System::Byte> &aBytes)/* overload */;
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPUInt32(const unsigned aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPInt32(const int aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPInt8(const System::Int8 aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPUInt8(const System::Byte aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPInt16(const short aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPUInt16(const System::Word aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPInt64(const __int64 aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPUInt64(const unsigned __int64 aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPAuthentication __fastcall sgcGetAMQPAuthentication(const System::UnicodeString aValue);
extern DELPHI_PACKAGE System::UnicodeString __fastcall sgcGetAMQPAuthenticationValue(const Sgcamqp_classes::TsgcAMQPAuthentication aValue);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPByte(const System::Byte aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPAuthentications __fastcall sgcGetAMQPAuthentications(const System::UnicodeString aValue);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPBoolean(const bool aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE Sgcamqp_classes::TsgcAMQPMethod __fastcall sgcGetAMQPMethod(const Sgcamqp_classes::TsgcAMQPClass aClass, const int aValue);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPFieldTable(const System::UnicodeString aValue, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPLongString(const System::DynamicArray<System::Byte> aSource, System::DynamicArray<System::Byte> &aBytes)/* overload */;
extern DELPHI_PACKAGE void __fastcall sgcWriteAMQPBit(int aOffset, int aBifOffset, System::DynamicArray<System::Byte> &aBytes);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetAMQPExceptionMessage(const int aCode);
}	/* namespace Sgcamqp_helpers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCAMQP_HELPERS)
using namespace Sgcamqp_helpers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcamqp_helpersHPP
