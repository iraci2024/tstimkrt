// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FXCConst.pas' rev: 31.00 (Windows)

#ifndef FxcconstHPP
#define FxcconstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fxcconst
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::UnicodeString, 41> Fxcconst__1;

//-- var, const, procedure ---------------------------------------------------
#define InternalCurrentVersion  (6.940000E+00)
extern DELPHI_PACKAGE System::UnicodeString internalCurrentVersionText;
static const System::Word DefaultSearchAttr = System::Word(0x28bf);
static const System::Int8 FXC_MAX_IVECTOR_SIZE = System::Int8(0x10);
static const System::Int8 FXC_MAX_SAVED_IVECTOR_SIZE = System::Int8(0x10);
extern DELPHI_PACKAGE System::ResourceString _SConfirmOverwrite;
#define Fxcconst_SConfirmOverwrite System::LoadResourceString(&Fxcconst::_SConfirmOverwrite)
extern DELPHI_PACKAGE System::ResourceString _SPasswordTitle;
#define Fxcconst_SPasswordTitle System::LoadResourceString(&Fxcconst::_SPasswordTitle)
extern DELPHI_PACKAGE System::ResourceString _SPasswordPrompt;
#define Fxcconst_SPasswordPrompt System::LoadResourceString(&Fxcconst::_SPasswordPrompt)
extern DELPHI_PACKAGE System::ResourceString _SOnRequestBlankDisk;
#define Fxcconst_SOnRequestBlankDisk System::LoadResourceString(&Fxcconst::_SOnRequestBlankDisk)
extern DELPHI_PACKAGE System::ResourceString _SOnRequestFirstDisk;
#define Fxcconst_SOnRequestFirstDisk System::LoadResourceString(&Fxcconst::_SOnRequestFirstDisk)
extern DELPHI_PACKAGE System::ResourceString _SOnRequestLastDisk;
#define Fxcconst_SOnRequestLastDisk System::LoadResourceString(&Fxcconst::_SOnRequestLastDisk)
extern DELPHI_PACKAGE System::ResourceString _SOnRequestMiddleDisk;
#define Fxcconst_SOnRequestMiddleDisk System::LoadResourceString(&Fxcconst::_SOnRequestMiddleDisk)
extern DELPHI_PACKAGE System::ResourceString _SOnDiskFull;
#define Fxcconst_SOnDiskFull System::LoadResourceString(&Fxcconst::_SOnDiskFull)
extern DELPHI_PACKAGE System::ResourceString _SOnProcessFileFailure;
#define Fxcconst_SOnProcessFileFailure System::LoadResourceString(&Fxcconst::_SOnProcessFileFailure)
extern DELPHI_PACKAGE System::ResourceString _SWrongDiskRequestLastDisk;
#define Fxcconst_SWrongDiskRequestLastDisk System::LoadResourceString(&Fxcconst::_SWrongDiskRequestLastDisk)
static const System::Int8 ErUnknownError = System::Int8(0x0);
static const System::Int8 ErIndexOutOfBounds = System::Int8(0x1);
static const System::Int8 ErInvalidCompressionMode = System::Int8(0x2);
static const System::Int8 ErUnexpectedNil = System::Int8(0x3);
static const System::Int8 ErInvalidCheckSum = System::Int8(0x4);
static const System::Int8 ErBlankFileName = System::Int8(0x5);
static const System::Int8 ErFileNotFound = System::Int8(0x6);
static const System::Int8 ErArchiveIsNotOpen = System::Int8(0x7);
static const System::Int8 ErStubNotSpecified = System::Int8(0x8);
static const System::Int8 ErCannotCreateFile = System::Int8(0x9);
static const System::Int8 ErCannotCreateDir = System::Int8(0xa);
static const System::Int8 ErNotInUpdate = System::Int8(0xb);
static const System::Int8 ErCannotOpenFile = System::Int8(0xc);
static const System::Int8 ErInUpdate = System::Int8(0xd);
static const System::Int8 ErCannotDeleteFile = System::Int8(0xe);
static const System::Int8 ErInMemoryArchiveCanBeCreatedOnly = System::Int8(0xf);
static const System::Int8 ErFileIsInReadonlyMode = System::Int8(0x10);
static const System::Int8 ErInvalidCompressedSize = System::Int8(0x11);
static const System::Int8 ErInvalidFormat = System::Int8(0x12);
static const System::Int8 ErCannotCreateOutputFile = System::Int8(0x13);
static const System::Int8 ErArchiveIsOpen = System::Int8(0x14);
static const System::Int8 ErUnableToCreateDirectory = System::Int8(0x15);
static const System::Int8 ErUnableToFindZip64DirEnd = System::Int8(0x16);
static const System::Int8 ErHugeFileModeIsNotEnabled = System::Int8(0x17);
static const System::Int8 ErCannotOpenArchiveFile = System::Int8(0x18);
static const System::Int8 ErCannotWriteToStream = System::Int8(0x19);
static const System::Int8 ErCannotFitSFXStubOnVolume = System::Int8(0x1a);
static const System::Int8 ErDamagedArchive = System::Int8(0x1b);
static const System::Int8 ErMakeSFXIsNotAllowed = System::Int8(0x1c);
static const System::Int8 ErArchiveAlreadyHasSFXStub = System::Int8(0x1d);
static const System::Int8 ErMultiVolumeArchiveIsNotAllowed = System::Int8(0x1e);
static const System::Int8 ErSpanningModificationIsNotAllowed = System::Int8(0x1f);
static const System::Int8 ErDeflate64NotSupported = System::Int8(0x20);
static const System::Int8 ErMakeSFXError = System::Int8(0x21);
static const System::Int8 ErInvalidHashedNamesList = System::Int8(0x22);
static const System::Int8 ErCannotDeleteFolder = System::Int8(0x23);
static const System::Int8 ErFileNameTooLong = System::Int8(0x24);
static const System::Int8 ErMultipleTransactionsNotAllowedForMutlivolumeArchives = System::Int8(0x25);
static const System::Int8 ErInvalidIVIndex = System::Int8(0x26);
static const System::Int8 ErDiskFull = System::Int8(0x27);
static const System::Int8 ErInvalidPassword = System::Int8(0x28);
static const System::Int8 FXCMaxError = System::Int8(0x28);
extern DELPHI_PACKAGE Fxcconst__1 FXCErrorMessages;
static const System::Int8 FXCMaxNativeError = System::Int8(0x43);
extern DELPHI_PACKAGE System::StaticArray<System::StaticArray<int, 2>, 68> FXCNativeToErrorCode;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 256> CRC32_TABLE;
}	/* namespace Fxcconst */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FXCCONST)
using namespace Fxcconst;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FxcconstHPP
