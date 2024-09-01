// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfconst.pas' rev: 20.00

#ifndef ZfconstHPP
#define ZfconstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Zfconst
{
//-- type declarations -------------------------------------------------------
typedef StaticArray<System::UnicodeString, 41> Zfconst__1;

//-- var, const, procedure ---------------------------------------------------
#define InternalCurrentVersion  (6.940000E+00)
extern PACKAGE System::UnicodeString internalCurrentVersionText;
static const Word DefaultSearchAttr = 0x28bf;
static const ShortInt ZF_MAX_IVECTOR_SIZE = 0x10;
static const ShortInt ZF_MAX_SAVED_IVECTOR_SIZE = 0x10;
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
static const ShortInt ErUnknownError = 0x0;
static const ShortInt ErIndexOutOfBounds = 0x1;
static const ShortInt ErInvalidCompressionMode = 0x2;
static const ShortInt ErUnexpectedNil = 0x3;
static const ShortInt ErInvalidCheckSum = 0x4;
static const ShortInt ErBlankFileName = 0x5;
static const ShortInt ErFileNotFound = 0x6;
static const ShortInt ErArchiveIsNotOpen = 0x7;
static const ShortInt ErStubNotSpecified = 0x8;
static const ShortInt ErCannotCreateFile = 0x9;
static const ShortInt ErCannotCreateDir = 0xa;
static const ShortInt ErNotInUpdate = 0xb;
static const ShortInt ErCannotOpenFile = 0xc;
static const ShortInt ErInUpdate = 0xd;
static const ShortInt ErCannotDeleteFile = 0xe;
static const ShortInt ErInMemoryArchiveCanBeCreatedOnly = 0xf;
static const ShortInt ErFileIsInReadonlyMode = 0x10;
static const ShortInt ErInvalidCompressedSize = 0x11;
static const ShortInt ErInvalidFormat = 0x12;
static const ShortInt ErCannotCreateOutputFile = 0x13;
static const ShortInt ErArchiveIsOpen = 0x14;
static const ShortInt ErUnableToCreateDirectory = 0x15;
static const ShortInt ErUnableToFindZip64DirEnd = 0x16;
static const ShortInt ErHugeFileModeIsNotEnabled = 0x17;
static const ShortInt ErCannotOpenArchiveFile = 0x18;
static const ShortInt ErCannotWriteToStream = 0x19;
static const ShortInt ErCannotFitSFXStubOnVolume = 0x1a;
static const ShortInt ErDamagedArchive = 0x1b;
static const ShortInt ErMakeSFXIsNotAllowed = 0x1c;
static const ShortInt ErArchiveAlreadyHasSFXStub = 0x1d;
static const ShortInt ErMultiVolumeArchiveIsNotAllowed = 0x1e;
static const ShortInt ErSpanningModificationIsNotAllowed = 0x1f;
static const ShortInt ErDeflate64NotSupported = 0x20;
static const ShortInt ErMakeSFXError = 0x21;
static const ShortInt ErInvalidHashedNamesList = 0x22;
static const ShortInt ErCannotDeleteFolder = 0x23;
static const ShortInt ErFileNameTooLong = 0x24;
static const ShortInt ErMultipleTransactionsNotAllowedForMutlivolumeArchives = 0x25;
static const ShortInt ErInvalidIVIndex = 0x26;
static const ShortInt ErDiskFull = 0x27;
static const ShortInt ErInvalidPassword = 0x28;
static const ShortInt ZFMaxError = 0x28;
extern PACKAGE Zfconst__1 ZFErrorMessages;
static const ShortInt ZFMaxNativeError = 0x43;
extern PACKAGE StaticArray<StaticArray<int, 2>, 68> ZFNativeToErrorCode;
extern PACKAGE StaticArray<unsigned, 256> CRC32_TABLE;

}	/* namespace Zfconst */
using namespace Zfconst;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZfconstHPP
