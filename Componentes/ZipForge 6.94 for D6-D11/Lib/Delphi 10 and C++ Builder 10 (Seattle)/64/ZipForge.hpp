﻿// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ZipForge.pas' rev: 30.00 (Windows)

#ifndef ZipforgeHPP
#define ZipforgeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <ZFFolderDialog.hpp>
#include <ZFPBKDF_2.hpp>
#include <ZFhmac.hpp>
#include <ZFHash.hpp>
#include <Winapi.Windows.hpp>
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <ZFZLib.hpp>
#include <ZFCipher.hpp>
#include <ZFConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zipforge
{
//-- forward type declarations -----------------------------------------------
struct TZFFileHeader;
struct TZFBlockHeader;
struct TZFZipFileHeader;
struct TZFZipCentralDir;
struct TZFZip64CentralDirEnd;
struct TZFZip64CentralDirEndLocator;
struct TZFZipCentralDirEnd;
struct TZFExtraFieldDataBlock;
struct TZFZip64ExtendedInfo;
struct TZFZipDataDescriptor;
struct TZFAESencryptionExtraField;
struct TInternalSearchRec;
struct TZFArchiveItem;
class DELPHICLASS TZFVolumeNumberInfo;
class DELPHICLASS TZFSpanningOptions;
class DELPHICLASS TZFOptions;
struct TDirItem;
struct THashItem;
class DELPHICLASS TZFStringHash;
class DELPHICLASS TZFHashedStringList;
class DELPHICLASS TZFDirArray;
class DELPHICLASS TZFDirManager;
class DELPHICLASS TZFBaseArchiver;
class DELPHICLASS TZFCryptoTransform;
class DELPHICLASS TAESCryptoTransform;
class DELPHICLASS TZipForge;
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString TZFString;

typedef System::Classes::TStrings TZFStrings;

typedef System::Classes::TStringList TZFStringList;

typedef System::Classes::TFileStream TZFFileStream;

typedef System::WideChar * TZFPChar;

typedef System::WideChar TZFChar;

typedef System::Sysutils::TSearchRec TZFSearchRec;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFFileHeader
{
public:
	int BlockSize;
	int CompSize;
	int UncompSize;
	unsigned FileCrc32;
	int NumBlocks;
	System::Byte CompressionAlgorithm;
	System::Byte CompressionMode;
	System::Word EncryptionAlgorithm;
	unsigned ControlBlockCrc32;
	System::StaticArray<char, 16> ControlBlock;
	System::Word extraLength;
	System::Byte UseInitVector;
	System::StaticArray<System::Byte, 3> reserved;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFBlockHeader
{
public:
	unsigned packedSize;
};
#pragma pack(pop)


enum DECLSPEC_DENUM TZFCompressionLevel : unsigned char { clNone, clFastest, clNormal, clMax };

enum DECLSPEC_DENUM TZFCompressionAlgorithm : unsigned char { caZLIB, caBZIP, caPPM };

enum DECLSPEC_DENUM TZFCryptoAlgorithm : unsigned char { caPkzipClassic, caAES_128, caAES_192, caAES_256 };

enum DECLSPEC_DENUM TZFProcessOperation : unsigned char { poAdd, poMove, poDelete, poUpdate, poExtract, poTest, poRename, poChangeAttr, poChangeComment, poMakeSFX, poMerge };

enum DECLSPEC_DENUM TZFProgressPhase : unsigned char { ppStart, ppProcess, ppEnd };

enum DECLSPEC_DENUM TZFAction : unsigned char { fxaRetry, fxaIgnore, fxaAbort };

typedef void __fastcall (__closure *TZFProgressEvent)(System::TObject* Sender, double Progress, TZFProcessOperation Operation, TZFProgressPhase ProgressPhase, bool &Cancel);

typedef void __fastcall (__closure *TZFFileProgressEvent)(System::TObject* Sender, System::UnicodeString FileName, double Progress, TZFProcessOperation Operation, TZFProgressPhase ProgressPhase, bool &Cancel);

typedef void __fastcall (__closure *TZFCopyTempFileProgressEvent)(System::TObject* Sender, double Progress, TZFProgressPhase ProgressPhase, bool &Cancel);

typedef void __fastcall (__closure *TZFConfirmOverwriteEvent)(System::TObject* Sender, System::UnicodeString SourceFileName, System::UnicodeString &DestFileName, bool &Confirm);

typedef void __fastcall (__closure *TZFConfirmProcessFileEvent)(System::TObject* Sender, System::UnicodeString FileName, TZFProcessOperation Operation, bool &Confirm);

typedef void __fastcall (__closure *TZFOnPasswordEvent)(System::TObject* Sender, System::UnicodeString FileName, System::AnsiString &NewPassword, bool &SkipFile);

typedef void __fastcall (__closure *TZFProcessFileFailureEvent)(System::TObject* Sender, System::UnicodeString FileName, TZFProcessOperation Operation, int NativeError, int ErrorCode, System::UnicodeString ErrorMessage, TZFAction &Action);

typedef void __fastcall (__closure *TZFOnRequestBlankVolumeEvent)(System::TObject* Sender, int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TZFOnRequestFirstVolumeEvent)(System::TObject* Sender, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TZFOnRequestLastVolumeEvent)(System::TObject* Sender, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TZFOnRequestMiddleVolumeEvent)(System::TObject* Sender, int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TZFOnDiskFullEvent)(System::TObject* Sender, int VolumeNumber, System::UnicodeString VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TZFProcessVolumeFailureEvent)(System::TObject* Sender, TZFProcessOperation Operation, int VolumeNumber, System::UnicodeString VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TZFOnStoreFileEvent)(System::TObject* Sender, System::UnicodeString &FileName, int &FileAttr, System::AnsiString &Comment, const System::UnicodeString OriginalFileName);

typedef void __fastcall (__closure *TZFOnExtractFileEvent)(System::TObject* Sender, System::UnicodeString &FileName, unsigned &FileAttr, const System::AnsiString Comment);

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZipFileHeader
{
public:
	unsigned signature;
	System::Word extractVersion;
	System::Word genPurposeFlag;
	System::Word compMethod;
	System::Word lastModTime;
	System::Word lastModDate;
	unsigned crc32;
	unsigned compSize;
	unsigned unCompSize;
	System::Word nameLength;
	System::Word extraLength;
};
#pragma pack(pop)


typedef TZFZipFileHeader *pZFZipFileHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZipCentralDir
{
public:
	unsigned signature;
	System::Word versionMadeBy;
	System::Word extractVersion;
	System::Word genPurposeFlag;
	System::Word compMethod;
	System::Word lastModTime;
	System::Word lastModDate;
	unsigned crc32;
	unsigned compSize;
	unsigned unCompSize;
	System::Word nameLength;
	System::Word extraLength;
	System::Word commentLength;
	System::Word diskNumberStart;
	System::Word internalAttr;
	unsigned externalAttr;
	unsigned relOffsetLH;
};
#pragma pack(pop)


typedef TZFZipCentralDir *pZFZipCentralDir;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZip64CentralDirEnd
{
public:
	unsigned signature;
	__int64 centralDirEndSize;
	System::Word versionMadeBy;
	System::Word versionNeededToExtract;
	unsigned diskNumber;
	unsigned startDiskNumber;
	__int64 entriesOnDisk;
	__int64 entriesCentralDir;
	__int64 centralDirSize;
	__int64 offsetStartDir;
};
#pragma pack(pop)


typedef TZFZip64CentralDirEnd *pZFZip64CentralDirEnd;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZip64CentralDirEndLocator
{
public:
	unsigned signature;
	unsigned startDiskNumber;
	__int64 offsetStartDirEnd;
	unsigned totalNumberOfDisks;
};
#pragma pack(pop)


typedef TZFZip64CentralDirEndLocator *pZFZip64CentralDirEndLocator;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZipCentralDirEnd
{
public:
	unsigned signature;
	System::Word diskNumber;
	System::Word startDiskNumber;
	System::Word entriesOnDisk;
	System::Word entriesCentralDir;
	unsigned centralDirSize;
	unsigned offsetStartDir;
	System::Word commentLength;
};
#pragma pack(pop)


typedef TZFZipCentralDirEnd *pZFZipCentralDirEnd;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFExtraFieldDataBlock
{
public:
	System::Word headerID;
	System::Word dataSize;
	System::Byte *pData;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZip64ExtendedInfo
{
public:
	__int64 uncompSize;
	__int64 compSize;
	__int64 relOffsetLH;
	unsigned diskNumberStart;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFZipDataDescriptor
{
public:
	int Signature;
	int crc32;
	int compressedSize;
	int uncompressedSize;
};
#pragma pack(pop)


typedef System::StaticArray<__int64, 3> TZipKey;

typedef System::StaticArray<System::Byte, 12> TZipKeyHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFAESencryptionExtraField
{
public:
	System::Word versionNumber;
	int keyLengthBits;
	System::Byte strength;
	System::Word compressionMethod;
	System::StaticArray<System::Byte, 3> vendorID;
};
#pragma pack(pop)


enum DECLSPEC_DENUM TZFOverwriteMode : unsigned char { omPrompt, omAlways, omNever, omIfNewer, omIfOlder };

enum DECLSPEC_DENUM TZFFileShareMode : unsigned char { smShareCompat, smShareExclusive, smShareDenyWrite, smShareDenyRead, smShareDenyNone };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TInternalSearchRec
{
public:
	int ItemNo;
	System::StaticArray<System::WideChar, 260> CFindMask;
	bool FWildCards;
	int FFindAttr;
	System::UnicodeString ExclusionMask;
	bool UseProperties;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TZFArchiveItem
{
public:
	System::UnicodeString FileName;
	System::UnicodeString StoredPath;
	__int64 CompressedSize;
	__int64 UncompressedSize;
	double CompressionRate;
	bool Encrypted;
	System::Word LastModFileDate;
	System::Word LastModFileTime;
	int CRC;
	unsigned ExternalFileAttributes;
	System::AnsiString Comment;
	TInternalSearchRec Handle;
};
#pragma pack(pop)


enum DECLSPEC_DENUM TZFStorePathMode : unsigned char { spNoPath, spRelativePath, spFullPath, spFullPathWithDrive };

enum DECLSPEC_DENUM TZip64Mode : unsigned char { zmDisabled, zmAuto, zmAlways };

enum DECLSPEC_DENUM TZFSpanningMode : unsigned char { smNone, smSpanning, smSplitting };

enum DECLSPEC_DENUM TZFVolumeSize : unsigned char { vsAutoDetect, vsCustom, vs1_44MB, vs100MB, vs200MB, vs250MB, vs600MB, vs650MB, vs700MB, vs4700MB };

enum DECLSPEC_DENUM TZFVolumeNumberKind : unsigned char { vnkFirst, vnkLast, vnkCustom, vnkUnknown };

class PASCALIMPLEMENTATION TZFVolumeNumberInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TZFVolumeNumberKind FVolumeKind;
	int FVolumeNumber;
	int FLastVolumeNumber;
	bool __fastcall GetFirstVolume(void);
	void __fastcall SetFirstVolume(bool Value);
	bool __fastcall GetLastVolume(void);
	void __fastcall SetLastVolume(bool Value);
	void __fastcall SetVolumeNumber(int Value);
	
public:
	__fastcall TZFVolumeNumberInfo(void);
	bool __fastcall IsEqualTo(TZFVolumeNumberInfo* VolumeInfo);
	void __fastcall Init(void);
	__property bool FirstVolume = {read=GetFirstVolume, write=SetFirstVolume, nodefault};
	__property bool LastVolume = {read=GetLastVolume, write=SetLastVolume, nodefault};
	__property int VolumeNumber = {read=FVolumeNumber, write=SetVolumeNumber, nodefault};
	__property int LastVolumeNumber = {read=FLastVolumeNumber, write=FLastVolumeNumber, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TZFVolumeNumberInfo(void) { }
	
};


class PASCALIMPLEMENTATION TZFSpanningOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FAdvancedNaming;
	__int64 FFirstVolumeSize;
	TZFVolumeSize FVolumeSize;
	__int64 FCustomVolumeSize;
	bool FSaveDirToFirstVolume;
	void __fastcall SetCustomVolumeSize(__int64 Value);
	
public:
	__fastcall TZFSpanningOptions(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__int64 __fastcall GetVolumeSize(TZFVolumeNumberInfo* VolumeNumberInfo);
	
__published:
	__property bool AdvancedNaming = {read=FAdvancedNaming, write=FAdvancedNaming, nodefault};
	__property __int64 FirstVolumeSize = {read=FFirstVolumeSize, write=FFirstVolumeSize};
	__property TZFVolumeSize VolumeSize = {read=FVolumeSize, write=FVolumeSize, nodefault};
	__property __int64 CustomVolumeSize = {read=FCustomVolumeSize, write=SetCustomVolumeSize};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TZFSpanningOptions(void) { }
	
};


class PASCALIMPLEMENTATION TZFOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TZFStorePathMode FStorePath;
	bool FRecurse;
	TZFFileShareMode FShareMode;
	TZFOverwriteMode FOverwriteMode;
	bool FCreateDirs;
	bool FReplaceReadOnly;
	bool FSetAttributes;
	int FSearchAttr;
	bool FFlushBuffers;
	bool FOEMFileNames;
	
public:
	__fastcall TZFOptions(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property bool Recurse = {read=FRecurse, write=FRecurse, default=1};
	__property TZFStorePathMode StorePath = {read=FStorePath, write=FStorePath, default=1};
	__property TZFFileShareMode ShareMode = {read=FShareMode, write=FShareMode, default=4};
	__property TZFOverwriteMode OverwriteMode = {read=FOverwriteMode, write=FOverwriteMode, default=1};
	__property bool CreateDirs = {read=FCreateDirs, write=FCreateDirs, default=1};
	__property bool ReplaceReadOnly = {read=FReplaceReadOnly, write=FReplaceReadOnly, default=1};
	__property bool SetAttributes = {read=FSetAttributes, write=FSetAttributes, default=1};
	__property int SearchAttr = {read=FSearchAttr, write=FSearchAttr, default=10431};
	__property bool FlushBuffers = {read=FFlushBuffers, write=FFlushBuffers, nodefault};
	__property bool OEMFileNames = {read=FOEMFileNames, write=FOEMFileNames, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TZFOptions(void) { }
	
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TDirItem
{
	
private:
	typedef System::DynamicArray<TZFExtraFieldDataBlock> _TDirItem__1;
	
	
public:
	TZFZipCentralDir CentralDir;
	System::UnicodeString Name;
	System::UnicodeString OldName;
	System::AnsiString Comment;
	_TDirItem__1 ExtraFields;
	TZFCryptoAlgorithm ZFEncryptionAlgorithm;
	bool bHugeFile;
	TZFZip64ExtendedInfo Zip64ExtInfo;
	bool Modified;
	System::AnsiString Password;
	System::Classes::TStream* Stream;
	bool bDestroyStream;
	int Position;
	bool Tagged;
	System::UnicodeString SrcFileName;
	System::Word CompressionMethod;
	System::Byte CompressionMode;
	TZFProcessOperation Operation;
	bool UseOldRijndael;
};
#pragma pack(pop)


typedef TDirItem *PDirItem;

typedef THashItem *PHashItem;

typedef PHashItem *PPHashItem;

struct DECLSPEC_DRECORD THashItem
{
public:
	THashItem *Next;
	System::UnicodeString Key;
	int Value;
};


class PASCALIMPLEMENTATION TZFStringHash : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<PHashItem> _TZFStringHash__1;
	
	
private:
	_TZFStringHash__1 Buckets;
	
protected:
	PPHashItem __fastcall Find(const System::UnicodeString Key);
	virtual unsigned __fastcall HashOf(const System::UnicodeString Key);
	
public:
	unsigned Size;
	__fastcall TZFStringHash(unsigned aSize);
	__fastcall virtual ~TZFStringHash(void);
	void __fastcall Add(const System::UnicodeString Key, int Value);
	void __fastcall Clear(void);
	void __fastcall Remove(const System::UnicodeString Key);
	bool __fastcall Modify(const System::UnicodeString Key, int Value);
	int __fastcall ValueOf(const System::UnicodeString Key);
};


class PASCALIMPLEMENTATION TZFHashedStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TZFStringHash* FValueHash;
	TZFStringHash* FNameHash;
	bool FValueHashValid;
	bool FNameHashValid;
	void __fastcall UpdateValueHash(void);
	
protected:
	void __fastcall UpdateNameHash(void);
	
public:
	__fastcall TZFHashedStringList(void);
	__fastcall virtual ~TZFHashedStringList(void);
	virtual int __fastcall Add(const System::UnicodeString S);
	void __fastcall Update(int Index, const System::UnicodeString NewValue);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Clear(void);
	virtual int __fastcall IndexOf(const System::UnicodeString S);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
};


class PASCALIMPLEMENTATION TZFDirArray : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TDirItem> _TZFDirArray__1;
	
	
private:
	_TZFDirArray__1 FItems;
	TZFHashedStringList* FItemNamesList;
	int AllocBy;
	int deAllocBy;
	int MaxAllocBy;
	int AllocItemCount;
	int ItemCount;
	void __fastcall SetItem(int Index, const TDirItem &Value);
	TDirItem __fastcall GetItem(int Index);
	PDirItem __fastcall GetItemPtr(int Index);
	void __fastcall SetCount(int Value);
	int __fastcall GetCount(void);
	
public:
	__fastcall TZFDirArray(void);
	__fastcall virtual ~TZFDirArray(void);
	void __fastcall Clear(void);
	void __fastcall Assign(TZFDirArray* da);
	void __fastcall Append(TZFDirArray* da);
	void __fastcall DeleteItem(int Index);
	void __fastcall ClearTags(void);
	bool __fastcall FileExists(const System::UnicodeString FileName, int &ItemNo);
	void __fastcall SetItemName(int Index, const System::UnicodeString Name);
	__property TDirItem Items[int Index] = {read=GetItem, write=SetItem};
	__property PDirItem ItemsPtr[int Index] = {read=GetItemPtr};
	__property int Count = {read=GetCount, write=SetCount, nodefault};
};


class PASCALIMPLEMENTATION TZFDirManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStream* FCompressedStream;
	bool FCreate;
	bool FIsCustomStream;
	TZFBaseArchiver* FArc;
	bool __fastcall FindSignature(System::Classes::TStream* Stream, __int64 &Offset, char * Signature, int SignatureLen);
	__int64 __fastcall FindLocalFileHeader(__int64 StartPosition);
	void __fastcall OpenVolume(TZFVolumeNumberInfo* VolumeNumberInfo);
	void __fastcall InitCentralDirEnd(void);
	bool __fastcall GetCentralDirEnd(TZFZipCentralDirEnd &DirEnd, __int64 &position, unsigned Signature);
	void __fastcall CalculateStubSize(__int64 DirEndPos);
	void __fastcall SaveSFXStub(void);
	
public:
	TZFDirArray* RBCDir;
	TZFDirArray* CDir;
	System::AnsiString ArchiveComment;
	TZFZip64CentralDirEndLocator Zip64CentralDirEndLocator;
	TZFZip64CentralDirEnd Zip64CentralDirEnd;
	TZFZipCentralDirEnd CentralDirEnd;
	__int64 StubSize;
	bool FZip64;
	bool Aborted;
	__int64 CentralDirOffset;
	__fastcall TZFDirManager(System::Classes::TStream* Stream, bool bCreate, TZFBaseArchiver* Arc);
	__fastcall virtual ~TZFDirManager(void);
	bool __fastcall IsEncrypted(void);
	void __fastcall SaveDir(bool RecreateCDirEnd, System::Classes::TStream* Stream = (System::Classes::TStream*)(0x0));
	void __fastcall LoadDir(void);
	void __fastcall ApplyStubOffset(void);
	bool __fastcall HasCentralDirEnd(void);
	bool __fastcall IsSFXArchive(void);
	void __fastcall MergeWith(TZFDirManager* DMHandleToAdd);
};


class PASCALIMPLEMENTATION TZFBaseArchiver : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FCurrentDir;
	int FUpdateCount;
	bool FActive;
	bool isZIPFormat;
	bool IsCustomStream;
	System::UnicodeString FFileName;
	System::UnicodeString VolumeFileName;
	TZFVolumeNumberInfo* FVolumeNumberInfo;
	System::Word FileOpenMode;
	System::UnicodeString FTempDir;
	System::UnicodeString FBaseDir;
	System::UnicodeString FSFXStub;
	TZFOptions* FOptions;
	System::Classes::TStrings* FFileMasks;
	System::Classes::TStrings* FExclusionMasks;
	System::Classes::TStrings* FNoCompressionMasks;
	bool FInMemory;
	int FProcessedFileNo;
	int FProcessedFileCount;
	__int64 FProcessedFilesTotalSize;
	__int64 FProcessedFilesSize;
	bool FProgressCancel;
	bool FProgressEnabled;
	bool FSkipFile;
	TZFSpanningMode FSpanningMode;
	TZFSpanningOptions* FSpanningOptions;
	bool FExtractCorruptedFiles;
	bool FOpenCorruptedArchives;
	bool FUnicodeFilenames;
	__int64 FProgress;
	__int64 FProgressMax;
	bool FNoProgress;
	TZFFileProgressEvent FOnFileProgress;
	TZFProgressEvent FOnOverallProgress;
	TZFCopyTempFileProgressEvent FOnCopyTempFileProgress;
	TZFConfirmOverwriteEvent FOnConfirmOverwrite;
	TZFConfirmProcessFileEvent FOnConfirmProcessFile;
	System::Classes::TNotifyEvent FAfterOpen;
	TZFOnPasswordEvent FOnPassword;
	TZFProcessFileFailureEvent FOnProcessFileFailure;
	TZFOnRequestFirstVolumeEvent FOnRequestFirstVolume;
	TZFOnRequestLastVolumeEvent FOnRequestLastVolume;
	TZFOnRequestMiddleVolumeEvent FOnRequestMiddleVolume;
	TZFOnRequestBlankVolumeEvent FOnRequestBlankVolume;
	TZFOnDiskFullEvent FOnDiskFull;
	TZFProcessVolumeFailureEvent FOnProcessVolumeFailure;
	TZFOnStoreFileEvent FOnStoreFile;
	TZFOnExtractFileEvent FOnExtractFile;
	bool FSuppressPasswordEvent;
	bool FUpdated;
	
protected:
	TZFDirManager* DMHandle;
	System::Word FCompressionMethod;
	TZFCompressionAlgorithm FCompressionAlgorithm;
	TZFCryptoAlgorithm FZFCryptoAlgorithm;
	System::Byte FCompressionMode;
	TZFCompressionLevel FCompressionLevel;
	System::AnsiString FPassword;
	System::Classes::TStream* FCompressedStream;
	TZip64Mode FZip64Mode;
	int Dummy1;
	int Dummy2;
	int Dummy3;
	int Dummy4;
	bool __fastcall GetInUpdate(void);
	void __fastcall SetInMemory(bool Value);
	System::AnsiString __fastcall GetFileComment(void);
	void __fastcall SetFileComment(const System::AnsiString Value);
	virtual void __fastcall SetPassword(const System::AnsiString Value);
	void __fastcall SetFileMasks(System::Classes::TStrings* Value);
	void __fastcall SetExclusionMasks(System::Classes::TStrings* Value);
	void __fastcall SetNoCompressionMasks(System::Classes::TStrings* Value);
	void __fastcall Lock(void);
	void __fastcall Unlock(void);
	void __fastcall CheckInactive(void);
	void __fastcall CheckInUpdate(void);
	void __fastcall CheckModifySpanning(void);
	void __fastcall SetActive(bool Value);
	bool __fastcall GetEncrypted(void);
	bool __fastcall GetExists(void);
	__int64 __fastcall GetArchiveSize(void);
	int __fastcall GetFileCount(void);
	void __fastcall SetCompressionLevel(TZFCompressionLevel newLevel);
	void __fastcall SetCompressionMode(System::Byte newMode);
	System::UnicodeString __fastcall GetCurrentVersionText(void);
	void __fastcall SetCurrentVersionText(System::UnicodeString s);
	void __fastcall SetSpanningMode(TZFSpanningMode Value);
	bool __fastcall InternalCompressFile(System::Classes::TStream* rfs, System::Classes::TStream* wfs, TDirItem &d, __int64 rfsSize);
	bool __fastcall ZipInitPassword(System::Classes::TStream* rfs, const System::AnsiString FPassword, System::Word genPurposeFlag, System::Word lastModTime, unsigned crc32, TZipKey &FKey);
	bool __fastcall InternalDecompressFile(System::Classes::TStream* rfs, System::Classes::TStream* wfs, TDirItem &d, int SizeToDecompress = 0x0, __int64 StartPosition = (__int64)(0LL));
	virtual void __fastcall SetCompMethod(void);
	int __fastcall ShareModeToInt(TZFFileShareMode ShareMode);
	void __fastcall InternalAddFromStream(const System::UnicodeString FileName, System::Classes::TStream* Stream, bool CopyToBuffer, bool DestroyStream, __int64 Position, __int64 Count, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ForceUpdate(void);
	void __fastcall FillDirItem(int ItemNo, const System::UnicodeString FileName, bool UseDiskFileData, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ExtractItem(int ItemNo, System::Classes::TStream* DestStream, int Count = 0x0, __int64 StartPosition = (__int64)(0LL));
	void __fastcall UpdateItem(int ItemNo, const System::UnicodeString SrcFileName);
	void __fastcall DeleteItem(int ItemNo);
	bool __fastcall CheckAttributesMatch(int FileAttr, int SearchAttr);
	bool __fastcall IsItemMatches(int ItemNo, TZFArchiveItem &F);
	bool __fastcall InternalFind(TZFArchiveItem &F);
	bool __fastcall IsInternalFileMatchMask(const System::UnicodeString FileName, const System::UnicodeString FileMask);
	bool __fastcall IsExternalFileMatchMask(const System::UnicodeString FileName, const System::UnicodeString FileMask, bool IsDir);
	System::UnicodeString __fastcall GetTempFileName(void);
	void __fastcall TagFiles(void)/* overload */;
	void __fastcall TagFiles(const System::UnicodeString FileMask, int SearchAttr, System::UnicodeString ExclusionMask="")/* overload */;
	void __fastcall ProcessTaggedFiles(TZFProcessOperation Operation, int TaggedFile = 0xffffffff);
	void __fastcall DeleteTaggedFile(int ItemNo);
	void __fastcall UpdateTaggedFile(int ItemNo, const System::UnicodeString FileName, const System::UnicodeString Path);
	void __fastcall ExtractTaggedFile(int ItemNo, const System::UnicodeString FileName, const System::UnicodeString Path);
	void __fastcall TestTaggedFile(int ItemNo, const System::UnicodeString FileName);
	System::UnicodeString __fastcall GetFullMask(const System::UnicodeString Mask, const System::UnicodeString BaseDir, bool &bRecurse);
	void __fastcall InternalAddFiles(System::Classes::TStrings* FileMasks, int SearchAttr, System::Classes::TStrings* ExclusionMasks, bool bMove, bool bRecurse);
	int __fastcall AddFileInit(const System::UnicodeString FileName, const System::UnicodeString BaseDir1, bool UseDiskFileData, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ReopenFileStream(const System::UnicodeString NewFileName);
	void __fastcall MakeDefaultVolumeName(System::UnicodeString &VolumeFileName, int VolumeNumber, bool CheckIfFileExists);
	__int64 __fastcall GetFreeDriveSpace(const System::UnicodeString VolumeFileName);
	int __fastcall WriteToStream(const void *Buffer, int Count, System::Classes::TStream* &Stream, TZFVolumeNumberInfo* VolumeNumberInfo, int RequiredFreeSpace = 0xffffffff, System::Classes::TStream* CacheStream = (System::Classes::TStream*)(0x0))/* overload */;
	int __fastcall WriteToStream(System::Classes::TStream* SrcStream, System::Classes::TStream* &DestStream, TZFVolumeNumberInfo* VolumeNumberInfo, bool &Cancel, __int64 Size = (__int64)(-1LL), int RequiredFreeSpace = 0xffffffff, System::Classes::TStream* CacheStream = (System::Classes::TStream*)(0x0), bool IsCopyTempFile = false)/* overload */;
	void __fastcall WriteBufferToStream(const void *Buffer, int Count, System::Classes::TStream* &Stream, TZFVolumeNumberInfo* VolumeNumberInfo, int RequiredFreeSpace = 0xffffffff, System::Classes::TStream* CacheStream = (System::Classes::TStream*)(0x0));
	void __fastcall OpenVolume(TZFVolumeNumberInfo* VolumeNumberInfo);
	bool __fastcall IsSFXArchive(const System::UnicodeString ArcFileName);
	void __fastcall InternalCreateArchive(void);
	void __fastcall InternalOpenNonSFXArchive(void);
	void __fastcall InternalOpenSFXArchive(void);
	void __fastcall DetectSpanning(void);
	void __fastcall InternalOpenArchive(void);
	void __fastcall DoAfterOpen(void);
	virtual void __fastcall DoOnOverallProgress(double Progress, TZFProcessOperation Operation, TZFProgressPhase ProgressPhase, bool &Cancel);
	virtual void __fastcall DoOnFileProgress(const System::UnicodeString FileName, double Progress, TZFProcessOperation Operation, TZFProgressPhase ProgressPhase, bool &Cancel);
	virtual void __fastcall DoOnCopyTempFileProgress(double Progress, TZFProgressPhase ProgressPhase, bool &Cancel);
	virtual void __fastcall DoOnConfirmOverwrite(System::UnicodeString SourceFileName, System::UnicodeString &DestFileName, bool &Confirm);
	virtual void __fastcall DoOnConfirmProcessFile(const System::UnicodeString FileName, TZFProcessOperation Operation, bool &Confirm);
	virtual void __fastcall DoOnPassword(const System::UnicodeString FileName, System::AnsiString &NewPassword, bool &SkipFile);
	virtual void __fastcall DoOnProcessFileFailure(const System::UnicodeString FileName, TZFProcessOperation Operation, int NativeError, int ErrorCode, const System::UnicodeString ErrorMessage, TZFAction &Action);
	virtual void __fastcall DoOnRequestBlankVolume(int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnRequestFirstVolume(System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnRequestLastVolume(System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnRequestMiddleVolume(int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnDiskFull(int VolumeNumber, const System::UnicodeString VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnProcessVolumeFailure(TZFProcessOperation Operation, int VolumeNumber, const System::UnicodeString VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnStoreFile(System::UnicodeString &FileName, int &FileAttr, System::AnsiString &Comment, const System::UnicodeString OriginalFileName);
	virtual void __fastcall DoOnExtractFile(System::UnicodeString &FileName, unsigned &FileAttr, const System::AnsiString Comment);
	bool __fastcall GetFileCendralDirItemNo(System::UnicodeString FileName, /* out */ int &ItemNo);
	bool __fastcall WriteToStreamWithOnDiskFull(System::Classes::TStream* Stream, void *Buffer, int Count);
	
public:
	__fastcall virtual TZFBaseArchiver(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TZFBaseArchiver(void);
	void __fastcall OpenArchive(void)/* overload */;
	void __fastcall OpenArchive(System::Word Mode)/* overload */;
	void __fastcall OpenArchive(System::Classes::TStream* Stream, bool Create)/* overload */;
	void __fastcall CloseArchive(void);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	void __fastcall CancelUpdate(void);
	void __fastcall AddFromStream(const System::UnicodeString FileName, System::Classes::TStream* Stream, bool CopyToBuffer = true, __int64 Position = (__int64)(0LL), __int64 Count = (__int64)(0LL), int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall AddFromBuffer(const System::UnicodeString FileName, const void *Buffer, int Count, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall AddFromString(const System::UnicodeString FileName, const System::UnicodeString Text, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ExtractToStream(const System::UnicodeString FileName, System::Classes::TStream* Stream);
	void __fastcall ExtractToBuffer(const System::UnicodeString FileName, void *Buffer, int Count, __int64 StartPosition);
	void __fastcall ExtractToString(const System::UnicodeString FileName, System::UnicodeString &Text);
	bool __fastcall FastFileAppend(System::Classes::TFileStream* const OuTZFFileStream, System::Classes::TFileStream* const InFileStream, const System::UnicodeString FileName);
	void __fastcall MakeSFX(const System::UnicodeString SFXFileName);
	bool __fastcall FindFirst(TZFArchiveItem &F)/* overload */;
	bool __fastcall FindFirst(const System::UnicodeString FileMask, TZFArchiveItem &F, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	bool __fastcall FindNext(TZFArchiveItem &F);
	void __fastcall RenameFile(const System::UnicodeString OldName, const System::UnicodeString NewName);
	void __fastcall ChangeFilesAttr(const System::UnicodeString FileMask, unsigned NewAttr);
	void __fastcall ChangeFilesIntAttr(const System::UnicodeString FileMask, unsigned NewAttr);
	void __fastcall ChangeFilesComment(const System::UnicodeString FileMask, const System::AnsiString NewComment);
	bool __fastcall IsFilePasswordValid(const System::UnicodeString FileName, const System::AnsiString Password);
	void __fastcall AddFiles(void)/* overload */;
	void __fastcall AddFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	void __fastcall MoveFiles(void)/* overload */;
	void __fastcall MoveFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	void __fastcall DeleteFiles(void)/* overload */;
	void __fastcall DeleteFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	void __fastcall UpdateFiles(void)/* overload */;
	void __fastcall UpdateFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	void __fastcall TestFiles(void)/* overload */;
	void __fastcall TestFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	void __fastcall RepairArchive(const System::UnicodeString OutputFileName = System::UnicodeString());
	void __fastcall ExtractFiles(void)/* overload */;
	void __fastcall ExtractFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = System::UnicodeString())/* overload */;
	bool __fastcall IsValidArchiveFile(void);
	__property TZFVolumeNumberInfo* VolumeNumberInfo = {read=FVolumeNumberInfo};
	__property __int64 Size = {read=GetArchiveSize};
	__property int FileCount = {read=GetFileCount, nodefault};
	__property bool Exists = {read=GetExists, nodefault};
	__property bool InUpdate = {read=GetInUpdate, nodefault};
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property System::AnsiString Comment = {read=GetFileComment, write=SetFileComment};
	__property bool OpenCorruptedArchives = {read=FOpenCorruptedArchives, write=FOpenCorruptedArchives, nodefault};
	
__published:
	__property bool ExtractCorruptedFiles = {read=FExtractCorruptedFiles, write=FExtractCorruptedFiles, nodefault};
	__property TZFCompressionLevel CompressionLevel = {read=FCompressionLevel, write=SetCompressionLevel, nodefault};
	__property System::Byte CompressionMode = {read=FCompressionMode, write=SetCompressionMode, nodefault};
	__property System::UnicodeString CurrentVersion = {read=GetCurrentVersionText, write=SetCurrentVersionText};
	__property System::AnsiString Password = {read=FPassword, write=SetPassword};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property System::UnicodeString BaseDir = {read=FBaseDir, write=FBaseDir};
	__property System::UnicodeString TempDir = {read=FTempDir, write=FTempDir};
	__property TZFSpanningMode SpanningMode = {read=FSpanningMode, write=SetSpanningMode, nodefault};
	__property TZFSpanningOptions* SpanningOptions = {read=FSpanningOptions, write=FSpanningOptions};
	__property System::UnicodeString SFXStub = {read=FSFXStub, write=FSFXStub};
	__property TZFOptions* Options = {read=FOptions, write=FOptions};
	__property System::Classes::TStrings* FileMasks = {read=FFileMasks, write=SetFileMasks};
	__property System::Classes::TStrings* ExclusionMasks = {read=FExclusionMasks, write=SetExclusionMasks};
	__property System::Classes::TStrings* NoCompressionMasks = {read=FNoCompressionMasks, write=SetNoCompressionMasks};
	__property bool InMemory = {read=FInMemory, write=SetInMemory, nodefault};
	__property TZFFileProgressEvent OnFileProgress = {read=FOnFileProgress, write=FOnFileProgress};
	__property TZFProgressEvent OnOverallProgress = {read=FOnOverallProgress, write=FOnOverallProgress};
	__property TZFCopyTempFileProgressEvent OnCopyTempFileProgress = {read=FOnCopyTempFileProgress, write=FOnCopyTempFileProgress};
	__property TZFConfirmOverwriteEvent OnConfirmOverwrite = {read=FOnConfirmOverwrite, write=FOnConfirmOverwrite};
	__property TZFConfirmProcessFileEvent OnConfirmProcessFile = {read=FOnConfirmProcessFile, write=FOnConfirmProcessFile};
	__property System::Classes::TNotifyEvent AfterOpen = {read=FAfterOpen, write=FAfterOpen};
	__property TZFOnPasswordEvent OnPassword = {read=FOnPassword, write=FOnPassword};
	__property TZFProcessFileFailureEvent OnProcessFileFailure = {read=FOnProcessFileFailure, write=FOnProcessFileFailure};
	__property TZFOnRequestBlankVolumeEvent OnRequestBlankVolume = {read=FOnRequestBlankVolume, write=FOnRequestBlankVolume};
	__property TZFOnRequestFirstVolumeEvent OnRequestFirstVolume = {read=FOnRequestFirstVolume, write=FOnRequestFirstVolume};
	__property TZFOnRequestLastVolumeEvent OnRequestLastVolume = {read=FOnRequestLastVolume, write=FOnRequestLastVolume};
	__property TZFOnRequestMiddleVolumeEvent OnRequestMiddleVolume = {read=FOnRequestMiddleVolume, write=FOnRequestMiddleVolume};
	__property TZFOnDiskFullEvent OnDiskFull = {read=FOnDiskFull, write=FOnDiskFull};
	__property TZFOnStoreFileEvent OnStoreFile = {read=FOnStoreFile, write=FOnStoreFile};
	__property TZFOnExtractFileEvent OnExtractFile = {read=FOnExtractFile, write=FOnExtractFile};
};


enum DECLSPEC_DENUM TCryptoTransformMode : unsigned char { Encryption, Decryption };

class PASCALIMPLEMENTATION TZFCryptoTransform : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCryptoTransformMode FTransformMode;
	
public:
	__fastcall TZFCryptoTransform(void);
	virtual void __fastcall GenerateKey(const System::AnsiString Password) = 0 ;
	virtual bool __fastcall CheckPassword(const System::AnsiString Password, const TDirItem &Item) = 0 ;
	virtual System::Sysutils::PByteArray __fastcall GetKey(void) = 0 ;
	virtual System::AnsiString __fastcall GetPassword(void) = 0 ;
	virtual void __fastcall Initialize(TCryptoTransformMode Mode, const TDirItem &Item);
	virtual void __fastcall Reset(void) = 0 ;
	virtual int __fastcall DecryptBuffer(const char * InputBuffer, int inputOffset, int InputCount, char * &OutputBuffer, int OutputOffset) = 0 ;
	virtual int __fastcall EncryptBuffer(const char * InputBuffer, int InputOffset, int InputCount, char * &OutputBuffer, int OutputOffset) = 0 ;
	virtual Zfzlib::PByte __fastcall GetFileStorageStartBlock(void) = 0 ;
	virtual void __fastcall LoadFileStorageStartBlock(System::Classes::TStream* Stream, __int64 Offset) = 0 ;
	virtual int __fastcall GetFileStorageStartBlockSize(void) = 0 ;
	virtual Zfzlib::PByte __fastcall GetFileStorageEndBlock(void) = 0 ;
	virtual void __fastcall LoadFileStorageEndBlock(System::Classes::TStream* Stream, __int64 Offset) = 0 ;
	virtual int __fastcall GetFileStorageEndBlockSize(void) = 0 ;
	virtual bool __fastcall IsDirItemCorrupted(const TDirItem &Item, unsigned Crc32) = 0 ;
	virtual TZFExtraFieldDataBlock __fastcall GetExtraFieldData(void) = 0 ;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TZFCryptoTransform(void) { }
	
};


class PASCALIMPLEMENTATION TAESCryptoTransform : public TZFCryptoTransform
{
	typedef TZFCryptoTransform inherited;
	
private:
	Zfpbkdf_2::TPBKDF2* FKeyGenerator;
	Zfhmac::THMAC_Context FHMAC_Context;
	Zfhash::THashDesc *FPHash;
	Zfcipher::TCipher_Rijndael* FCryptor;
	System::Word FKeyLengthBytes;
	System::Sysutils::TByteArray *FHashKey;
	System::Sysutils::TByteArray *FEncryptionKey;
	System::Sysutils::TByteArray *FPasswordVerificationValue;
	System::Sysutils::TByteArray *FSaltValue;
	System::Sysutils::TByteArray *FAuthenticationCode;
	System::AnsiString FPassword;
	System::Word FVersion;
	System::Word FActualCompressionMethod;
	int FAuthenticationCodeSize;
	
public:
	__fastcall TAESCryptoTransform(const int Strength, System::Word Crypt_version, System::Word ActualCompMethod);
	__fastcall virtual ~TAESCryptoTransform(void);
	TAESCryptoTransform* __fastcall CreateAESEncryption(const int KeyLengthBits, System::Word VersionNumber, System::Word CompressionMethod);
	virtual void __fastcall GenerateKey(const System::AnsiString Password);
	
private:
	void __fastcall UpdateKeys(void);
	
public:
	virtual bool __fastcall CheckPassword(const System::AnsiString Password, const TDirItem &Item);
	virtual bool __fastcall IsDirItemCorrupted(const TDirItem &Item, unsigned Crc32);
	virtual System::Sysutils::PByteArray __fastcall GetKey(void);
	virtual System::AnsiString __fastcall GetPassword(void);
	virtual void __fastcall Initialize(TCryptoTransformMode Mode, const TDirItem &Item);
	virtual void __fastcall Reset(void);
	virtual int __fastcall EncryptBuffer(const char * InputBuffer, int InputOffset, int InputCount, char * &OutputBuffer, int OutputOffset);
	virtual int __fastcall DecryptBuffer(const char * InputBuffer, int InputOffset, int InputCount, char * &OutputBuffer, int OutputOffset);
	virtual TZFExtraFieldDataBlock __fastcall GetExtraFieldData(void);
	virtual Zfzlib::PByte __fastcall GetFileStorageStartBlock(void);
	virtual void __fastcall LoadFileStorageStartBlock(System::Classes::TStream* Stream, __int64 Offset);
	virtual int __fastcall GetFileStorageStartBlockSize(void);
	virtual Zfzlib::PByte __fastcall GetFileStorageEndBlock(void);
	virtual void __fastcall LoadFileStorageEndBlock(System::Classes::TStream* Stream, __int64 Offset);
	virtual int __fastcall GetFileStorageEndBlockSize(void);
};


class PASCALIMPLEMENTATION TZipForge : public TZFBaseArchiver
{
	typedef TZFBaseArchiver inherited;
	
protected:
	virtual void __fastcall SetCompMethod(void);
	virtual void __fastcall SetPassword(const System::AnsiString Value);
	
public:
	__fastcall virtual TZipForge(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TZipForge(void);
	void __fastcall MergeWith(const System::UnicodeString ArchiveFileName);
	
__published:
	__property TZip64Mode Zip64Mode = {read=FZip64Mode, write=FZip64Mode, nodefault};
	__property bool UnicodeFilenames = {read=FUnicodeFilenames, write=FUnicodeFilenames, nodefault};
	__property TZFCryptoAlgorithm EncryptionMethod = {read=FZFCryptoAlgorithm, write=FZFCryptoAlgorithm, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const int DefaultMaxBlockSize = int(0x100000);
static const int BlockSizeForFastest = int(0x80000);
static const int BlockSizeForNormal = int(0x100000);
static const int BlockSizeForMax = int(0x180000);
static const int ZFBlockHeaderSize = int(0x4);
static const int ZFFileHeaderSize = int(0x32);
static const System::Int8 CustomHeaderSizeOffset = System::Int8(0x17);
static const System::Int8 ZFNormalFileAttr = System::Int8(0x20);
static const System::Int8 ZFDirectoryAttr = System::Int8(0x10);
static const int MinVolumeSize = int(0x10000);
extern DELPHI_PACKAGE System::UnicodeString ProductName;
static const int ZFZipFileHeaderSize = int(0x1e);
static const int ZFZipCentralDirSize = int(0x2e);
static const int ZFZipCentralDirEndSize = int(0x16);
static const int ZFZip64CentralDirEndSize = int(0x38);
static const int ZFZip64CentralDirEndLocatorSize = int(0x14);
static const int ZipFileHeaderSignature = int(0x4034b50);
static const int ZFFileHeaderSignature = int(0x5045c61);
static const int ZipCentralDirSignature = int(0x2014b50);
static const int ZFCentralDirSignature = int(0x3025c61);
static const int ZipDataDescriptorSignature = int(0x8074b50);
static const int ZipCentralDirEndSignature = int(0x6054b50);
static const int ZFCentralDirEndSignature = int(0x6054141);
static const int Zip64CentralDirEndSignature = int(0x6064b50);
static const int Zip64CentralDirEndLocatorSignature = int(0x7064b50);
static const System::Int8 ZipVersion = System::Int8(0x14);
static const System::Word ZipVersionNoOEM = System::Word(0xb14);
static const System::Word ZFVersion = System::Word(0x4114);
static const System::Int8 Zip64Version = System::Int8(0x2d);
static const System::Word ZF64Version = System::Word(0x412d);
static const System::Int8 ZipGenPurposeFlag = System::Int8(0x0);
static const System::Int8 ZIP_None = System::Int8(0x0);
static const System::Int8 ZIP_ZLIB = System::Int8(0x8);
static const System::Int8 ZIP_Deflate64 = System::Int8(0x9);
static const System::Byte ZF_None = System::Byte(0xff);
static const System::Byte ZF_ZLIB = System::Byte(0xfe);
static const System::Byte ZF_BZIP = System::Byte(0xfd);
static const System::Byte ZF_PPM = System::Byte(0xfc);
static const System::Word caNone = System::Word(0xffff);
extern DELPHI_PACKAGE System::Word AesExtraFieldHeaderID;
extern DELPHI_PACKAGE bool ForceBuildCentralDir;
extern DELPHI_PACKAGE bool __fastcall IsFloppyDrive(const System::UnicodeString FileName);
extern DELPHI_PACKAGE bool __fastcall IsRemovableDrive(const System::UnicodeString FileName);
extern DELPHI_PACKAGE bool __fastcall DelTreeIfNoFiles(const System::UnicodeString Directory);
extern DELPHI_PACKAGE System::Byte __fastcall ZipEncryptByte(System::Byte &c, TZipKey &Key);
extern DELPHI_PACKAGE void __fastcall ZipDecryptBuffer(Zfzlib::PByte &Buffer, int size, TZipKey &Key);
extern DELPHI_PACKAGE void __fastcall ZipEncryptBuffer(int size, int offset, Zfzlib::PByte &Buffer, TZipKey &Key);
extern DELPHI_PACKAGE void __fastcall ZipKeyInit(const System::AnsiString Password, TZipKey &Key, TZipKeyHeader &KeyHeader);
extern DELPHI_PACKAGE void __fastcall ZipEncryptKeyInit(const System::AnsiString Password, TZipKey &Key);
}	/* namespace Zipforge */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ZIPFORGE)
using namespace Zipforge;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ZipforgeHPP
