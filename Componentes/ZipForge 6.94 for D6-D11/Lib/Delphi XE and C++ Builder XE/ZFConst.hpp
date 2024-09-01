// CodeGear C++Builder
// Copyright (c) 1995, 2010 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZFConst.pas' rev: 22.00

#ifndef ZfconstHPP
#define ZfconstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfconst
{
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::UnicodeString, 41> Zfconst__1;

//-- var, const, procedure ---------------------------------------------------
#define InternalCurrentVersion  (6.940000E+00)
extern PACKAGE System::UnicodeString internalCurrentVersionText;
static const System::Word DefaultSearchAttr = 0x28bf;
static const System::ShortInt ZF_MAX_IVECTOR_SIZE = 0x10;
static const System::ShortInt ZF_MAX_SAVED_IVECTOR_SIZE = 0x10;
extern PACKAGE System::ResourceString _SConfirmOverwrite;
#define Zfconst_SConfirmOverwrite System::LoadResourceString(&Zfconst::_SConfirmOverwrite)
extern PACKAGE System::ResourceString _SPasswordTitle;
#define Zfconst_SPasswordTitle System::LoadResourceString(&Zfconst::_SPasswordTitle)
extern PACKAGE System::ResourceString _SPasswordPrompt;
#define Zfconst_SPasswordPrompt System::LoadResourceString(&Zfconst::_SPasswordPrompt)
extern PACKAGE System::ResourceString _SOnRequestBlankDisk;
#define Zfconst_SOnRequestBlankDisk System::LoadResourceString(&Zfconst::_SOnRequestBlankDisk)
extern PACKAGE System::ResourceString _SOnRequestFirstDisk;
#define Zfconst_SOnRequestFirstDisk System::LoadResourceString(&Zfconst::_SOnRequestFirstDisk)
extern PACKAGE System::ResourceString _SOnRequestLastDisk;
#define Zfconst_SOnRequestLastDisk System::LoadResourceString(&Zfconst::_SOnRequestLastDisk)
extern PACKAGE System::ResourceString _SOnRequestMiddleDisk;
#define Zfconst_SOnRequestMiddleDisk System::LoadResourceString(&Zfconst::_SOnRequestMiddleDisk)
extern PACKAGE System::ResourceString _SOnDiskFull;
#define Zfconst_SOnDiskFull System::LoadResourceString(&Zfconst::_SOnDiskFull)
extern PACKAGE System::ResourceString _SOnProcessFileFailure;
#define Zfconst_SOnProcessFileFailure System::LoadResourceString(&Zfconst::_SOnProcessFileFailure)
extern PACKAGE System::ResourceString _SWrongDiskRequestLastDisk;
#define Zfconst_SWrongDiskRequestLastDisk System::LoadResourceString(&Zfconst::_SWrongDiskRequestLastDisk)
static const System::ShortInt ErUnknownError = 0x0;
static const System::ShortInt ErIndexOutOfBounds = 0x1;
static const System::ShortInt ErInvalidCompressionMode = 0x2;
static const System::ShortInt ErUnexpectedNil = 0x3;
static const System::ShortInt ErInvalidCheckSum = 0x4;
static const System::ShortInt ErBlankFileName = 0x5;
static const System::ShortInt ErFileNotFound = 0x6;
static const System::ShortInt ErArchiveIsNotOpen = 0x7;
static const System::ShortInt ErStubNotSpecified = 0x8;
static const System::ShortInt ErCannotCreateFile = 0x9;
static const System::ShortInt ErCannotCreateDir = 0xa;
static const System::ShortInt ErNotInUpdate = 0xb;
static const System::ShortInt ErCannotOpenFile = 0xc;
static const System::ShortInt ErInUpdate = 0xd;
static const System::ShortInt ErCannotDeleteFile = 0xe;
static const System::ShortInt ErInMemoryArchiveCanBeCreatedOnly = 0xf;
static const System::ShortInt ErFileIsInReadonlyMode = 0x10;
static const System::ShortInt ErInvalidCompressedSize = 0x11;
static const System::ShortInt ErInvalidFormat = 0x12;
static const System::ShortInt ErCannotCreateOutputFile = 0x13;
static const System::ShortInt ErArchiveIsOpen = 0x14;
static const System::ShortInt ErUnableToCreateDirectory = 0x15;
static const System::ShortInt ErUnableToFindZip64DirEnd = 0x16;
static const System::ShortInt ErHugeFileModeIsNotEnabled = 0x17;
static const System::ShortInt ErCannotOpenArchiveFile = 0x18;
static const System::ShortInt ErCannotWriteToStream = 0x19;
static const System::ShortInt ErCannotFitSFXStubOnVolume = 0x1a;
static const System::ShortInt ErDamagedArchive = 0x1b;
static const System::ShortInt ErMakeSFXIsNotAllowed = 0x1c;
static const System::ShortInt ErArchiveAlreadyHasSFXStub = 0x1d;
static const System::ShortInt ErMultiVolumeArchiveIsNotAllowed = 0x1e;
static const System::ShortInt ErSpanningModificationIsNotAllowed = 0x1f;
static const System::ShortInt ErDeflate64NotSupported = 0x20;
static const System::ShortInt ErMakeSFXError = 0x21;
static const System::ShortInt ErInvalidHashedNamesList = 0x22;
static const System::ShortInt ErCannotDeleteFolder = 0x23;
static const System::ShortInt ErFileNameTooLong = 0x24;
static const System::ShortInt ErMultipleTransactionsNotAllowedForMutlivolumeArchives = 0x25;
static const System::ShortInt ErInvalidIVIndex = 0x26;
static const System::ShortInt ErDiskFull = 0x27;
static const System::ShortInt ErInvalidPassword = 0x28;
static const System::ShortInt ZFMaxError = 0x28;
extern PACKAGE Zfconst__1 ZFErrorMessages;
static const System::ShortInt ZFMaxNativeError = 0x43;
extern PACKAGE System::StaticArray<System::StaticArray<int, 2>, 68> ZFNativeToErrorCode;
extern PACKAGE System::StaticArray<unsigned, 256> CRC32_TABLE;

}	/* namespace Zfconst */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Zfconst;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfconstHPP
