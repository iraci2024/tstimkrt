// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Zfconst.pas' rev: 10.00

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
typedef AnsiString ZFConst__1[41];

//-- var, const, procedure ---------------------------------------------------
#define InternalCurrentVersion  (6.940000E+00)
extern PACKAGE AnsiString internalCurrentVersionText;
static const Word DefaultSearchAttr = 0x28bf;
static const Shortint ZF_MAX_IVECTOR_SIZE = 0x10;
static const Shortint ZF_MAX_SAVED_IVECTOR_SIZE = 0x10;
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
static const Shortint ErUnknownError = 0x0;
static const Shortint ErIndexOutOfBounds = 0x1;
static const Shortint ErInvalidCompressionMode = 0x2;
static const Shortint ErUnexpectedNil = 0x3;
static const Shortint ErInvalidCheckSum = 0x4;
static const Shortint ErBlankFileName = 0x5;
static const Shortint ErFileNotFound = 0x6;
static const Shortint ErArchiveIsNotOpen = 0x7;
static const Shortint ErStubNotSpecified = 0x8;
static const Shortint ErCannotCreateFile = 0x9;
static const Shortint ErCannotCreateDir = 0xa;
static const Shortint ErNotInUpdate = 0xb;
static const Shortint ErCannotOpenFile = 0xc;
static const Shortint ErInUpdate = 0xd;
static const Shortint ErCannotDeleteFile = 0xe;
static const Shortint ErInMemoryArchiveCanBeCreatedOnly = 0xf;
static const Shortint ErFileIsInReadonlyMode = 0x10;
static const Shortint ErInvalidCompressedSize = 0x11;
static const Shortint ErInvalidFormat = 0x12;
static const Shortint ErCannotCreateOutputFile = 0x13;
static const Shortint ErArchiveIsOpen = 0x14;
static const Shortint ErUnableToCreateDirectory = 0x15;
static const Shortint ErUnableToFindZip64DirEnd = 0x16;
static const Shortint ErHugeFileModeIsNotEnabled = 0x17;
static const Shortint ErCannotOpenArchiveFile = 0x18;
static const Shortint ErCannotWriteToStream = 0x19;
static const Shortint ErCannotFitSFXStubOnVolume = 0x1a;
static const Shortint ErDamagedArchive = 0x1b;
static const Shortint ErMakeSFXIsNotAllowed = 0x1c;
static const Shortint ErArchiveAlreadyHasSFXStub = 0x1d;
static const Shortint ErMultiVolumeArchiveIsNotAllowed = 0x1e;
static const Shortint ErSpanningModificationIsNotAllowed = 0x1f;
static const Shortint ErDeflate64NotSupported = 0x20;
static const Shortint ErMakeSFXError = 0x21;
static const Shortint ErInvalidHashedNamesList = 0x22;
static const Shortint ErCannotDeleteFolder = 0x23;
static const Shortint ErFileNameTooLong = 0x24;
static const Shortint ErMultipleTransactionsNotAllowedForMutlivolumeArchives = 0x25;
static const Shortint ErInvalidIVIndex = 0x26;
static const Shortint ErDiskFull = 0x27;
static const Shortint ErInvalidPassword = 0x28;
static const Shortint ZFMaxError = 0x28;
extern PACKAGE AnsiString ZFErrorMessages[41];
static const Shortint ZFMaxNativeError = 0x43;
extern PACKAGE int ZFNativeToErrorCode[68][2];
extern PACKAGE unsigned CRC32_TABLE[256];

}	/* namespace Zfconst */
using namespace Zfconst;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Zfconst
