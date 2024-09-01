// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCConst.pas' rev: 4.00

#ifndef FXCConstHPP
#define FXCConstHPP

#pragma delphiheader begin
#pragma option push -w-
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fxcconst
{
//-- type declarations -------------------------------------------------------
typedef AnsiString FXCConst__1[41];

//-- var, const, procedure ---------------------------------------------------
#define InternalCurrentVersion  (6.940000E+00)
extern PACKAGE AnsiString internalCurrentVersionText;
static const Word DefaultSearchAttr = 0x28bf;
static const Shortint FXC_MAX_IVECTOR_SIZE = 0x10;
static const Shortint FXC_MAX_SAVED_IVECTOR_SIZE = 0x10;
extern PACKAGE System::ResourceString _SConfirmOverwrite;
#define Fxcconst_SConfirmOverwrite System::LoadResourceString(&Fxcconst::_SConfirmOverwrite)
extern PACKAGE System::ResourceString _SPasswordTitle;
#define Fxcconst_SPasswordTitle System::LoadResourceString(&Fxcconst::_SPasswordTitle)
extern PACKAGE System::ResourceString _SPasswordPrompt;
#define Fxcconst_SPasswordPrompt System::LoadResourceString(&Fxcconst::_SPasswordPrompt)
extern PACKAGE System::ResourceString _SOnRequestBlankDisk;
#define Fxcconst_SOnRequestBlankDisk System::LoadResourceString(&Fxcconst::_SOnRequestBlankDisk)
extern PACKAGE System::ResourceString _SOnRequestFirstDisk;
#define Fxcconst_SOnRequestFirstDisk System::LoadResourceString(&Fxcconst::_SOnRequestFirstDisk)
extern PACKAGE System::ResourceString _SOnRequestLastDisk;
#define Fxcconst_SOnRequestLastDisk System::LoadResourceString(&Fxcconst::_SOnRequestLastDisk)
extern PACKAGE System::ResourceString _SOnRequestMiddleDisk;
#define Fxcconst_SOnRequestMiddleDisk System::LoadResourceString(&Fxcconst::_SOnRequestMiddleDisk)
extern PACKAGE System::ResourceString _SOnDiskFull;
#define Fxcconst_SOnDiskFull System::LoadResourceString(&Fxcconst::_SOnDiskFull)
extern PACKAGE System::ResourceString _SOnProcessFileFailure;
#define Fxcconst_SOnProcessFileFailure System::LoadResourceString(&Fxcconst::_SOnProcessFileFailure)
	
extern PACKAGE System::ResourceString _SWrongDiskRequestLastDisk;
#define Fxcconst_SWrongDiskRequestLastDisk System::LoadResourceString(&Fxcconst::_SWrongDiskRequestLastDisk)
	
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
static const Shortint FXCMaxError = 0x28;
extern PACKAGE AnsiString FXCErrorMessages[41];
static const Shortint FXCMaxNativeError = 0x43;
extern PACKAGE int FXCNativeToErrorCode[68][2];
extern PACKAGE unsigned CRC32_TABLE[256];

}	/* namespace Fxcconst */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fxcconst;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FXCConst
