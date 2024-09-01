// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Flexcompress.pas' rev: 21.00

#ifndef FlexcompressHPP
#define FlexcompressHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Stdctrls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Fxcfolderdialog.hpp>	// Pascal unit
#include <Fxcpbkdf_2.hpp>	// Pascal unit
#include <Fxchmac.hpp>	// Pascal unit
#include <Fxchash.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Math.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Fxczlib.hpp>	// Pascal unit
#include <Fxcbzip2.hpp>	// Pascal unit
#include <Fxcppm.hpp>	// Pascal unit
#include <Fxccipher.hpp>	// Pascal unit
#include <Fxcrng.hpp>	// Pascal unit
#include <Fxcconst.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Flexcompress
{
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString TFXCString;

typedef Classes::TStrings TFXCStrings;

typedef Classes::TStringList TFXCStringList;

typedef Classes::TFileStream TFXCFileStream;

typedef System::WideChar * TFXCPChar;

typedef System::WideChar TFXCChar;

typedef Sysutils::TSearchRec TFXCSearchRec;

#pragma pack(push,1)
struct TFXCFileHeader
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
	StaticArray<char, 16> ControlBlock;
	System::Word extraLength;
	System::Byte UseInitVector;
	StaticArray<System::Byte, 3> reserved;
};
#pragma pack(pop)


#pragma pack(push,1)
struct TFXCBlockHeader
{
	
public:
	unsigned packedSize;
};
#pragma pack(pop)


#pragma option push -b-
enum TFXCCompressionLevel { clNone, clFastest, clNormal, clMax };
#pragma option pop

#pragma option push -b-
enum TFXCCompressionAlgorithm { caZLIB, caBZIP, caPPM };
#pragma option pop

#pragma option push -b-
enum TFXCCryptoAlgorithm { caRijndael_128, caRijndael_256, caDES_Single, caDES_Triple, caBlowfish, caTwofish_128, caTwofish_256, caSquare };
#pragma option pop

typedef StaticArray<System::Byte, 16> TInitVector;

#pragma option push -b-
enum TFXCProcessOperation { poAdd, poMove, poDelete, poUpdate, poExtract, poTest, poRename, poChangeAttr, poChangeComment, poMakeSFX, poMerge };
#pragma option pop

#pragma option push -b-
enum TFXCProgressPhase { ppStart, ppProcess, ppEnd };
#pragma option pop

#pragma option push -b-
enum TFXCAction { fxaRetry, fxaIgnore, fxaAbort };
#pragma option pop

typedef void __fastcall (__closure *TFXCProgressEvent)(System::TObject* Sender, double Progress, TFXCProcessOperation Operation, TFXCProgressPhase ProgressPhase, bool &Cancel);

typedef void __fastcall (__closure *TFXCFileProgressEvent)(System::TObject* Sender, System::UnicodeString FileName, double Progress, TFXCProcessOperation Operation, TFXCProgressPhase ProgressPhase, bool &Cancel);

typedef void __fastcall (__closure *TFXCCopyTempFileProgressEvent)(System::TObject* Sender, double Progress, TFXCProgressPhase ProgressPhase, bool &Cancel);

typedef void __fastcall (__closure *TFXCConfirmOverwriteEvent)(System::TObject* Sender, System::UnicodeString SourceFileName, System::UnicodeString &DestFileName, bool &Confirm);

typedef void __fastcall (__closure *TFXCConfirmProcessFileEvent)(System::TObject* Sender, System::UnicodeString FileName, TFXCProcessOperation Operation, bool &Confirm);

typedef void __fastcall (__closure *TFXCOnPasswordEvent)(System::TObject* Sender, System::UnicodeString FileName, System::AnsiString &NewPassword, bool &SkipFile);

typedef void __fastcall (__closure *TFXCProcessFileFailureEvent)(System::TObject* Sender, System::UnicodeString FileName, TFXCProcessOperation Operation, int NativeError, int ErrorCode, System::UnicodeString ErrorMessage, TFXCAction &Action);

typedef void __fastcall (__closure *TFXCOnRequestBlankVolumeEvent)(System::TObject* Sender, int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TFXCOnRequestFirstVolumeEvent)(System::TObject* Sender, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TFXCOnRequestLastVolumeEvent)(System::TObject* Sender, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TFXCOnRequestMiddleVolumeEvent)(System::TObject* Sender, int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TFXCOnDiskFullEvent)(System::TObject* Sender, int VolumeNumber, System::UnicodeString VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TFXCProcessVolumeFailureEvent)(System::TObject* Sender, TFXCProcessOperation Operation, int VolumeNumber, System::UnicodeString VolumeFileName, bool &Cancel);

typedef void __fastcall (__closure *TFXCOnStoreFileEvent)(System::TObject* Sender, System::UnicodeString &FileName, int &FileAttr, System::AnsiString &Comment, const System::UnicodeString OriginalFileName);

typedef void __fastcall (__closure *TFXCOnExtractFileEvent)(System::TObject* Sender, System::UnicodeString &FileName, unsigned &FileAttr, const System::AnsiString Comment);

#pragma pack(push,1)
struct TFXCZipFileHeader
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


typedef TFXCZipFileHeader *pFXCZipFileHeader;

#pragma pack(push,1)
struct TFXCZipCentralDir
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


typedef TFXCZipCentralDir *pFXCZipCentralDir;

#pragma pack(push,1)
struct TFXCZip64CentralDirEnd
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


typedef TFXCZip64CentralDirEnd *pFXCZip64CentralDirEnd;

#pragma pack(push,1)
struct TFXCZip64CentralDirEndLocator
{
	
public:
	unsigned signature;
	unsigned startDiskNumber;
	__int64 offsetStartDirEnd;
	unsigned totalNumberOfDisks;
};
#pragma pack(pop)


typedef TFXCZip64CentralDirEndLocator *pFXCZip64CentralDirEndLocator;

#pragma pack(push,1)
struct TFXCZipCentralDirEnd
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


typedef TFXCZipCentralDirEnd *pFXCZipCentralDirEnd;

#pragma pack(push,1)
struct TFXCExtraFieldDataBlock
{
	
public:
	System::Word headerID;
	System::Word dataSize;
	System::Byte *pData;
};
#pragma pack(pop)


#pragma pack(push,1)
struct TFXCZip64ExtendedInfo
{
	
public:
	__int64 uncompSize;
	__int64 compSize;
	__int64 relOffsetLH;
	unsigned diskNumberStart;
};
#pragma pack(pop)


#pragma pack(push,1)
struct TFXCZipDataDescriptor
{
	
public:
	int Signature;
	int crc32;
	int compressedSize;
	int uncompressedSize;
};
#pragma pack(pop)


typedef StaticArray<__int64, 3> TZipKey;

typedef StaticArray<System::Byte, 12> TZipKeyHeader;

#pragma pack(push,1)
struct TFXCAESencryptionExtraField
{
	
public:
	System::Word versionNumber;
	int keyLengthBits;
	System::Byte strength;
	System::Word compressionMethod;
	StaticArray<System::Byte, 3> vendorID;
};
#pragma pack(pop)


#pragma option push -b-
enum TFXCOverwriteMode { omPrompt, omAlways, omNever, omIfNewer, omIfOlder };
#pragma option pop

#pragma option push -b-
enum TFXCFileShareMode { smShareCompat, smShareExclusive, smShareDenyWrite, smShareDenyRead, smShareDenyNone };
#pragma option pop

#pragma pack(push,1)
struct TInternalSearchRec
{
	
public:
	int ItemNo;
	StaticArray<System::WideChar, 260> CFindMask;
	bool FWildCards;
	int FFindAttr;
	System::UnicodeString ExclusionMask;
	bool UseProperties;
};
#pragma pack(pop)


#pragma pack(push,1)
struct TFXCArchiveItem
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


#pragma option push -b-
enum TFXCStorePathMode { spNoPath, spRelativePath, spFullPath, spFullPathWithDrive };
#pragma option pop

#pragma option push -b-
enum TZip64Mode { zmDisabled, zmAuto, zmAlways };
#pragma option pop

#pragma option push -b-
enum TFXCSpanningMode { smNone, smSpanning, smSplitting };
#pragma option pop

#pragma option push -b-
enum TFXCVolumeSize { vsAutoDetect, vsCustom, vs1_44MB, vs100MB, vs200MB, vs250MB, vs600MB, vs650MB, vs700MB, vs4700MB };
#pragma option pop

#pragma option push -b-
enum TFXCVolumeNumberKind { vnkFirst, vnkLast, vnkCustom, vnkUnknown };
#pragma option pop

class DELPHICLASS TFXCVolumeNumberInfo;
class PASCALIMPLEMENTATION TFXCVolumeNumberInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFXCVolumeNumberKind FVolumeKind;
	int FVolumeNumber;
	int FLastVolumeNumber;
	bool __fastcall GetFirstVolume(void);
	void __fastcall SetFirstVolume(bool Value);
	bool __fastcall GetLastVolume(void);
	void __fastcall SetLastVolume(bool Value);
	void __fastcall SetVolumeNumber(int Value);
	
public:
	__fastcall TFXCVolumeNumberInfo(void);
	bool __fastcall IsEqualTo(TFXCVolumeNumberInfo* VolumeInfo);
	void __fastcall Init(void);
	__property bool FirstVolume = {read=GetFirstVolume, write=SetFirstVolume, nodefault};
	__property bool LastVolume = {read=GetLastVolume, write=SetLastVolume, nodefault};
	__property int VolumeNumber = {read=FVolumeNumber, write=SetVolumeNumber, nodefault};
	__property int LastVolumeNumber = {read=FLastVolumeNumber, write=FLastVolumeNumber, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFXCVolumeNumberInfo(void) { }
	
};


class DELPHICLASS TFXCSpanningOptions;
class PASCALIMPLEMENTATION TFXCSpanningOptions : public Classes::TPersistent
{
	typedef Classes::TPersistent inherited;
	
private:
	bool FAdvancedNaming;
	__int64 FFirstVolumeSize;
	TFXCVolumeSize FVolumeSize;
	__int64 FCustomVolumeSize;
	bool FSaveDirToFirstVolume;
	void __fastcall SetCustomVolumeSize(__int64 Value);
	
public:
	__fastcall TFXCSpanningOptions(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__int64 __fastcall GetVolumeSize(TFXCVolumeNumberInfo* VolumeNumberInfo);
	
__published:
	__property bool AdvancedNaming = {read=FAdvancedNaming, write=FAdvancedNaming, nodefault};
	__property __int64 FirstVolumeSize = {read=FFirstVolumeSize, write=FFirstVolumeSize};
	__property TFXCVolumeSize VolumeSize = {read=FVolumeSize, write=FVolumeSize, nodefault};
	__property __int64 CustomVolumeSize = {read=FCustomVolumeSize, write=SetCustomVolumeSize};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TFXCSpanningOptions(void) { }
	
};


class DELPHICLASS TFXCOptions;
class PASCALIMPLEMENTATION TFXCOptions : public Classes::TPersistent
{
	typedef Classes::TPersistent inherited;
	
private:
	TFXCStorePathMode FStorePath;
	bool FRecurse;
	TFXCFileShareMode FShareMode;
	TFXCOverwriteMode FOverwriteMode;
	bool FCreateDirs;
	bool FReplaceReadOnly;
	bool FSetAttributes;
	int FSearchAttr;
	bool FFlushBuffers;
	bool FOEMFileNames;
	
public:
	__fastcall TFXCOptions(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property bool Recurse = {read=FRecurse, write=FRecurse, default=1};
	__property TFXCStorePathMode StorePath = {read=FStorePath, write=FStorePath, default=1};
	__property TFXCFileShareMode ShareMode = {read=FShareMode, write=FShareMode, default=4};
	__property TFXCOverwriteMode OverwriteMode = {read=FOverwriteMode, write=FOverwriteMode, default=1};
	__property bool CreateDirs = {read=FCreateDirs, write=FCreateDirs, default=1};
	__property bool ReplaceReadOnly = {read=FReplaceReadOnly, write=FReplaceReadOnly, default=1};
	__property bool SetAttributes = {read=FSetAttributes, write=FSetAttributes, default=1};
	__property int SearchAttr = {read=FSearchAttr, write=FSearchAttr, default=10431};
	__property bool FlushBuffers = {read=FFlushBuffers, write=FFlushBuffers, nodefault};
	__property bool OEMFileNames = {read=FOEMFileNames, write=FOEMFileNames, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TFXCOptions(void) { }
	
};


#pragma pack(push,1)
struct TDirItem
{
	
private:
	typedef DynamicArray<TFXCExtraFieldDataBlock> _TDirItem__1;
	
	
public:
	TFXCZipCentralDir CentralDir;
	System::UnicodeString Name;
	System::UnicodeString OldName;
	System::AnsiString Comment;
	_TDirItem__1 ExtraFields;
	TFXCCryptoAlgorithm FXEncryptionAlgorithm;
	bool bHugeFile;
	TFXCZip64ExtendedInfo Zip64ExtInfo;
	bool Modified;
	System::AnsiString Password;
	Classes::TStream* Stream;
	bool bDestroyStream;
	int Position;
	bool Tagged;
	System::UnicodeString SrcFileName;
	System::Word CompressionMethod;
	System::Byte CompressionMode;
	TFXCProcessOperation Operation;
	bool UseOldRijndael;
};
#pragma pack(pop)


typedef TDirItem *PDirItem;

struct THashItem;
typedef THashItem *PHashItem;

typedef PHashItem *PPHashItem;

struct THashItem
{
	
public:
	THashItem *Next;
	System::UnicodeString Key;
	int Value;
};


class DELPHICLASS TFXCStringHash;
class PASCALIMPLEMENTATION TFXCStringHash : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef DynamicArray<PHashItem> _TFXCStringHash__1;
	
	
private:
	_TFXCStringHash__1 Buckets;
	
protected:
	PPHashItem __fastcall Find(const System::UnicodeString Key);
	virtual unsigned __fastcall HashOf(const System::UnicodeString Key);
	
public:
	unsigned Size;
	__fastcall TFXCStringHash(unsigned aSize);
	__fastcall virtual ~TFXCStringHash(void);
	void __fastcall Add(const System::UnicodeString Key, int Value);
	void __fastcall Clear(void);
	void __fastcall Remove(const System::UnicodeString Key);
	bool __fastcall Modify(const System::UnicodeString Key, int Value);
	int __fastcall ValueOf(const System::UnicodeString Key);
};


class DELPHICLASS TFXCHashedStringList;
class PASCALIMPLEMENTATION TFXCHashedStringList : public Classes::TStringList
{
	typedef Classes::TStringList inherited;
	
private:
	TFXCStringHash* FValueHash;
	TFXCStringHash* FNameHash;
	bool FValueHashValid;
	bool FNameHashValid;
	void __fastcall UpdateValueHash(void);
	
protected:
	void __fastcall UpdateNameHash(void);
	
public:
	__fastcall TFXCHashedStringList(void);
	__fastcall virtual ~TFXCHashedStringList(void);
	virtual int __fastcall Add(const System::UnicodeString S);
	void __fastcall Update(int Index, const System::UnicodeString NewValue);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Clear(void);
	virtual int __fastcall IndexOf(const System::UnicodeString S);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
};


class DELPHICLASS TFXCDirArray;
class PASCALIMPLEMENTATION TFXCDirArray : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef DynamicArray<TDirItem> _TFXCDirArray__1;
	
	
private:
	_TFXCDirArray__1 FItems;
	TFXCHashedStringList* FItemNamesList;
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
	__fastcall TFXCDirArray(void);
	__fastcall virtual ~TFXCDirArray(void);
	void __fastcall Clear(void);
	void __fastcall Assign(TFXCDirArray* da);
	void __fastcall Append(TFXCDirArray* da);
	void __fastcall DeleteItem(int Index);
	void __fastcall ClearTags(void);
	bool __fastcall FileExists(const System::UnicodeString FileName, int &ItemNo);
	void __fastcall SetItemName(int Index, const System::UnicodeString Name);
	__property TDirItem Items[int Index] = {read=GetItem, write=SetItem};
	__property PDirItem ItemsPtr[int Index] = {read=GetItemPtr};
	__property int Count = {read=GetCount, write=SetCount, nodefault};
};


class DELPHICLASS TFXCDirManager;
class DELPHICLASS TBaseArchiver;
class PASCALIMPLEMENTATION TFXCDirManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Classes::TStream* FCompressedStream;
	bool FCreate;
	bool FIsCustomStream;
	TBaseArchiver* FArc;
	bool __fastcall FindSignature(Classes::TStream* Stream, __int64 &Offset, char * Signature, int SignatureLen);
	__int64 __fastcall FindLocalFileHeader(__int64 StartPosition);
	void __fastcall OpenVolume(TFXCVolumeNumberInfo* VolumeNumberInfo);
	void __fastcall InitCentralDirEnd(void);
	bool __fastcall GetCentralDirEnd(TFXCZipCentralDirEnd &DirEnd, __int64 &position, unsigned Signature);
	void __fastcall CalculateStubSize(__int64 DirEndPos);
	void __fastcall SaveSFXStub(void);
	
public:
	TFXCDirArray* RBCDir;
	TFXCDirArray* CDir;
	System::AnsiString ArchiveComment;
	TFXCZip64CentralDirEndLocator Zip64CentralDirEndLocator;
	TFXCZip64CentralDirEnd Zip64CentralDirEnd;
	TFXCZipCentralDirEnd CentralDirEnd;
	__int64 StubSize;
	bool FZip64;
	bool Aborted;
	__int64 CentralDirOffset;
	__fastcall TFXCDirManager(Classes::TStream* Stream, bool bCreate, TBaseArchiver* Arc);
	__fastcall virtual ~TFXCDirManager(void);
	bool __fastcall IsEncrypted(void);
	void __fastcall SaveDir(bool RecreateCDirEnd, Classes::TStream* Stream = (Classes::TStream*)(0x0));
	void __fastcall LoadDir(void);
	void __fastcall ApplyStubOffset(void);
	bool __fastcall HasCentralDirEnd(void);
	bool __fastcall IsSFXArchive(void);
	void __fastcall MergeWith(TFXCDirManager* DMHandleToAdd);
};


class PASCALIMPLEMENTATION TBaseArchiver : public Classes::TComponent
{
	typedef Classes::TComponent inherited;
	
private:
	System::UnicodeString FCurrentDir;
	int FUpdateCount;
	bool FActive;
	bool isZIPFormat;
	bool IsCustomStream;
	System::UnicodeString FFileName;
	System::UnicodeString VolumeFileName;
	TFXCVolumeNumberInfo* FVolumeNumberInfo;
	System::Word FileOpenMode;
	System::UnicodeString FTempDir;
	System::UnicodeString FBaseDir;
	System::UnicodeString FSFXStub;
	TFXCOptions* FOptions;
	Classes::TStrings* FFileMasks;
	Classes::TStrings* FExclusionMasks;
	Classes::TStrings* FNoCompressionMasks;
	bool FInMemory;
	int FProcessedFileNo;
	int FProcessedFileCount;
	__int64 FProcessedFilesTotalSize;
	__int64 FProcessedFilesSize;
	bool FProgressCancel;
	bool FProgressEnabled;
	bool FSkipFile;
	TFXCSpanningMode FSpanningMode;
	TFXCSpanningOptions* FSpanningOptions;
	bool FExtractCorruptedFiles;
	bool FOpenCorruptedArchives;
	bool FUnicodeFilenames;
	__int64 FProgress;
	__int64 FProgressMax;
	bool FNoProgress;
	TFXCFileProgressEvent FOnFileProgress;
	TFXCProgressEvent FOnOverallProgress;
	TFXCCopyTempFileProgressEvent FOnCopyTempFileProgress;
	TFXCConfirmOverwriteEvent FOnConfirmOverwrite;
	TFXCConfirmProcessFileEvent FOnConfirmProcessFile;
	Classes::TNotifyEvent FAfterOpen;
	TFXCOnPasswordEvent FOnPassword;
	TFXCProcessFileFailureEvent FOnProcessFileFailure;
	TFXCOnRequestFirstVolumeEvent FOnRequestFirstVolume;
	TFXCOnRequestLastVolumeEvent FOnRequestLastVolume;
	TFXCOnRequestMiddleVolumeEvent FOnRequestMiddleVolume;
	TFXCOnRequestBlankVolumeEvent FOnRequestBlankVolume;
	TFXCOnDiskFullEvent FOnDiskFull;
	TFXCProcessVolumeFailureEvent FOnProcessVolumeFailure;
	TFXCOnStoreFileEvent FOnStoreFile;
	TFXCOnExtractFileEvent FOnExtractFile;
	bool FSuppressPasswordEvent;
	bool FUpdated;
	
protected:
	TFXCDirManager* DMHandle;
	System::Word FCompressionMethod;
	TFXCCompressionAlgorithm FCompressionAlgorithm;
	TFXCCryptoAlgorithm FFXCCryptoAlgorithm;
	System::Byte FCompressionMode;
	TFXCCompressionLevel FCompressionLevel;
	System::AnsiString FPassword;
	Classes::TStream* FCompressedStream;
	TZip64Mode FZip64Mode;
	TInitVector FInitVector;
	int FInitVectorMaxSize;
	bool FUseInitVector;
	int Dummy1;
	int Dummy2;
	int Dummy3;
	int Dummy4;
	bool __fastcall GetInUpdate(void);
	void __fastcall SetInMemory(bool Value);
	System::AnsiString __fastcall GetFileComment(void);
	void __fastcall SetFileComment(const System::AnsiString Value);
	virtual void __fastcall SetPassword(const System::AnsiString Value);
	void __fastcall SetFileMasks(Classes::TStrings* Value);
	void __fastcall SetExclusionMasks(Classes::TStrings* Value);
	void __fastcall SetNoCompressionMasks(Classes::TStrings* Value);
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
	void __fastcall SetCompressionLevel(TFXCCompressionLevel newLevel);
	void __fastcall SetCompressionMode(System::Byte newMode);
	int __fastcall GetMaxInitVectorSize(void);
	System::UnicodeString __fastcall GetCurrentVersionText(void);
	void __fastcall SetCurrentVersionText(System::UnicodeString s);
	void __fastcall SetSpanningMode(TFXCSpanningMode Value);
	bool __fastcall InternalCompressFile(Classes::TStream* rfs, Classes::TStream* wfs, TDirItem &d, __int64 rfsSize);
	bool __fastcall ZipInitPassword(Classes::TStream* rfs, const System::AnsiString FPassword, System::Word genPurposeFlag, System::Word lastModTime, unsigned crc32, __int64 *FKey);
	bool __fastcall InternalDecompressFile(Classes::TStream* rfs, Classes::TStream* wfs, TDirItem &d, int SizeToDecompress = 0x0, __int64 StartPosition = 0x000000000);
	virtual void __fastcall SetCompMethod(void);
	int __fastcall ShareModeToInt(TFXCFileShareMode ShareMode);
	void __fastcall InternalAddFromStream(const System::UnicodeString FileName, Classes::TStream* Stream, bool CopyToBuffer, bool DestroyStream, __int64 Position, __int64 Count, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ForceUpdate(void);
	void __fastcall FillDirItem(int ItemNo, const System::UnicodeString FileName, bool UseDiskFileData, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ExtractItem(int ItemNo, Classes::TStream* DestStream, int Count = 0x0, __int64 StartPosition = 0x000000000);
	void __fastcall UpdateItem(int ItemNo, const System::UnicodeString SrcFileName);
	void __fastcall DeleteItem(int ItemNo);
	bool __fastcall CheckAttributesMatch(int FileAttr, int SearchAttr);
	bool __fastcall IsItemMatches(int ItemNo, TFXCArchiveItem &F);
	bool __fastcall InternalFind(TFXCArchiveItem &F);
	bool __fastcall IsInternalFileMatchMask(const System::UnicodeString FileName, const System::UnicodeString FileMask);
	bool __fastcall IsExternalFileMatchMask(const System::UnicodeString FileName, const System::UnicodeString FileMask, bool IsDir);
	System::UnicodeString __fastcall GetTempFileName(void);
	void __fastcall TagFiles(void)/* overload */;
	void __fastcall TagFiles(const System::UnicodeString FileMask, int SearchAttr, System::UnicodeString ExclusionMask="")/* overload */;
	void __fastcall ProcessTaggedFiles(TFXCProcessOperation Operation, int TaggedFile = 0xffffffff);
	void __fastcall DeleteTaggedFile(int ItemNo);
	void __fastcall UpdateTaggedFile(int ItemNo, const System::UnicodeString FileName, const System::UnicodeString Path);
	void __fastcall ExtractTaggedFile(int ItemNo, const System::UnicodeString FileName, const System::UnicodeString Path);
	void __fastcall TestTaggedFile(int ItemNo, const System::UnicodeString FileName);
	System::UnicodeString __fastcall GetFullMask(const System::UnicodeString Mask, const System::UnicodeString BaseDir, bool &bRecurse);
	void __fastcall InternalAddFiles(Classes::TStrings* FileMasks, int SearchAttr, Classes::TStrings* ExclusionMasks, bool bMove, bool bRecurse);
	int __fastcall AddFileInit(const System::UnicodeString FileName, const System::UnicodeString BaseDir1, bool UseDiskFileData, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ReopenFileStream(const System::UnicodeString NewFileName);
	void __fastcall MakeDefaultVolumeName(System::UnicodeString &VolumeFileName, int VolumeNumber, bool CheckIfFileExists);
	__int64 __fastcall GetFreeDriveSpace(const System::UnicodeString VolumeFileName);
	int __fastcall WriteToStream(const void *Buffer, int Count, Classes::TStream* &Stream, TFXCVolumeNumberInfo* VolumeNumberInfo, int RequiredFreeSpace = 0xffffffff, Classes::TStream* CacheStream = (Classes::TStream*)(0x0))/* overload */;
	int __fastcall WriteToStream(Classes::TStream* SrcStream, Classes::TStream* &DestStream, TFXCVolumeNumberInfo* VolumeNumberInfo, bool &Cancel, __int64 Size = 0xffffffffffffffff, int RequiredFreeSpace = 0xffffffff, Classes::TStream* CacheStream = (Classes::TStream*)(0x0), bool IsCopyTempFile = false)/* overload */;
	void __fastcall WriteBufferToStream(const void *Buffer, int Count, Classes::TStream* &Stream, TFXCVolumeNumberInfo* VolumeNumberInfo, int RequiredFreeSpace = 0xffffffff, Classes::TStream* CacheStream = (Classes::TStream*)(0x0));
	void __fastcall OpenVolume(TFXCVolumeNumberInfo* VolumeNumberInfo);
	bool __fastcall IsSFXArchive(const System::UnicodeString ArcFileName);
	void __fastcall InternalCreateArchive(void);
	void __fastcall InternalOpenNonSFXArchive(void);
	void __fastcall InternalOpenSFXArchive(void);
	void __fastcall DetectSpanning(void);
	void __fastcall InternalOpenArchive(void);
	void __fastcall DoAfterOpen(void);
	virtual void __fastcall DoOnOverallProgress(double Progress, TFXCProcessOperation Operation, TFXCProgressPhase ProgressPhase, bool &Cancel);
	virtual void __fastcall DoOnFileProgress(const System::UnicodeString FileName, double Progress, TFXCProcessOperation Operation, TFXCProgressPhase ProgressPhase, bool &Cancel);
	virtual void __fastcall DoOnCopyTempFileProgress(double Progress, TFXCProgressPhase ProgressPhase, bool &Cancel);
	virtual void __fastcall DoOnConfirmOverwrite(System::UnicodeString SourceFileName, System::UnicodeString &DestFileName, bool &Confirm);
	virtual void __fastcall DoOnConfirmProcessFile(const System::UnicodeString FileName, TFXCProcessOperation Operation, bool &Confirm);
	virtual void __fastcall DoOnPassword(const System::UnicodeString FileName, System::AnsiString &NewPassword, bool &SkipFile);
	virtual void __fastcall DoOnProcessFileFailure(const System::UnicodeString FileName, TFXCProcessOperation Operation, int NativeError, int ErrorCode, const System::UnicodeString ErrorMessage, TFXCAction &Action);
	virtual void __fastcall DoOnRequestBlankVolume(int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnRequestFirstVolume(System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnRequestLastVolume(System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnRequestMiddleVolume(int VolumeNumber, System::UnicodeString &VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnDiskFull(int VolumeNumber, const System::UnicodeString VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnProcessVolumeFailure(TFXCProcessOperation Operation, int VolumeNumber, const System::UnicodeString VolumeFileName, bool &Cancel);
	virtual void __fastcall DoOnStoreFile(System::UnicodeString &FileName, int &FileAttr, System::AnsiString &Comment, const System::UnicodeString OriginalFileName);
	virtual void __fastcall DoOnExtractFile(System::UnicodeString &FileName, unsigned &FileAttr, const System::AnsiString Comment);
	bool __fastcall GetFileCendralDirItemNo(System::UnicodeString FileName, /* out */ int &ItemNo);
	bool __fastcall WriteToStreamWithOnDiskFull(Classes::TStream* Stream, void *Buffer, int Count);
	void __fastcall FillRandomBuffer(Sysutils::PByteArray buf, int BufSize);
	void __fastcall SetIVByte(int Index, const System::Byte Value);
	System::Byte __fastcall GetIVByte(int Index);
	
public:
	__fastcall virtual TBaseArchiver(Classes::TComponent* AOwner);
	__fastcall virtual ~TBaseArchiver(void);
	void __fastcall OpenArchive(void)/* overload */;
	void __fastcall OpenArchive(System::Word Mode)/* overload */;
	void __fastcall OpenArchive(Classes::TStream* Stream, bool Create)/* overload */;
	void __fastcall CloseArchive(void);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	void __fastcall CancelUpdate(void);
	void __fastcall AddFromStream(const System::UnicodeString FileName, Classes::TStream* Stream, bool CopyToBuffer = true, __int64 Position = 0x000000000, __int64 Count = 0x000000000, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall AddFromBuffer(const System::UnicodeString FileName, const void *Buffer, int Count, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall AddFromString(const System::UnicodeString FileName, const System::UnicodeString Text, int Attr = 0x20, System::TDateTime DateTime = 0.000000E+00);
	void __fastcall ExtractToStream(const System::UnicodeString FileName, Classes::TStream* Stream);
	void __fastcall ExtractToBuffer(const System::UnicodeString FileName, void *Buffer, int Count, __int64 StartPosition);
	void __fastcall ExtractToString(const System::UnicodeString FileName, System::UnicodeString &Text);
	bool __fastcall FastFileAppend(const Classes::TFileStream* OuTFXCFileStream, const Classes::TFileStream* InFileStream, const System::UnicodeString FileName);
	void __fastcall MakeSFX(const System::UnicodeString SFXFileName);
	bool __fastcall FindFirst(TFXCArchiveItem &F)/* overload */;
	bool __fastcall FindFirst(const System::UnicodeString FileMask, TFXCArchiveItem &F, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	bool __fastcall FindNext(TFXCArchiveItem &F);
	void __fastcall RenameFile(const System::UnicodeString OldName, const System::UnicodeString NewName);
	void __fastcall ChangeFilesAttr(const System::UnicodeString FileMask, unsigned NewAttr);
	void __fastcall ChangeFilesIntAttr(const System::UnicodeString FileMask, unsigned NewAttr);
	void __fastcall ChangeFilesComment(const System::UnicodeString FileMask, const System::AnsiString NewComment);
	bool __fastcall IsFilePasswordValid(const System::UnicodeString FileName, const System::AnsiString Password);
	void __fastcall AddFiles(void)/* overload */;
	void __fastcall AddFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	void __fastcall MoveFiles(void)/* overload */;
	void __fastcall MoveFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	void __fastcall DeleteFiles(void)/* overload */;
	void __fastcall DeleteFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	void __fastcall UpdateFiles(void)/* overload */;
	void __fastcall UpdateFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	void __fastcall TestFiles(void)/* overload */;
	void __fastcall TestFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	void __fastcall RepairArchive(const System::UnicodeString OutputFileName = L"");
	void __fastcall ExtractFiles(void)/* overload */;
	void __fastcall ExtractFiles(const System::UnicodeString FileMask, int SearchAttr = 0x28bf, const System::UnicodeString ExclusionMask = L"")/* overload */;
	bool __fastcall IsValidArchiveFile(void);
	__property bool UseInitVector = {read=FUseInitVector, write=FUseInitVector, nodefault};
	__property int MaxInitVectorSize = {read=GetMaxInitVectorSize, nodefault};
	__property System::Byte InitVector[int Index] = {read=GetIVByte, write=SetIVByte};
	void __fastcall GenerateIV(void);
	void __fastcall SetInitVector(void * IV, int Size);
	__property TFXCVolumeNumberInfo* VolumeNumberInfo = {read=FVolumeNumberInfo};
	__property __int64 Size = {read=GetArchiveSize};
	__property int FileCount = {read=GetFileCount, nodefault};
	__property bool Exists = {read=GetExists, nodefault};
	__property bool InUpdate = {read=GetInUpdate, nodefault};
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property System::AnsiString Comment = {read=GetFileComment, write=SetFileComment};
	__property bool OpenCorruptedArchives = {read=FOpenCorruptedArchives, write=FOpenCorruptedArchives, nodefault};
	
__published:
	__property bool ExtractCorruptedFiles = {read=FExtractCorruptedFiles, write=FExtractCorruptedFiles, nodefault};
	__property TFXCCompressionLevel CompressionLevel = {read=FCompressionLevel, write=SetCompressionLevel, nodefault};
	__property System::Byte CompressionMode = {read=FCompressionMode, write=SetCompressionMode, nodefault};
	__property System::UnicodeString CurrentVersion = {read=GetCurrentVersionText, write=SetCurrentVersionText};
	__property System::AnsiString Password = {read=FPassword, write=SetPassword};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property System::UnicodeString BaseDir = {read=FBaseDir, write=FBaseDir};
	__property System::UnicodeString TempDir = {read=FTempDir, write=FTempDir};
	__property TFXCSpanningMode SpanningMode = {read=FSpanningMode, write=SetSpanningMode, nodefault};
	__property TFXCSpanningOptions* SpanningOptions = {read=FSpanningOptions, write=FSpanningOptions};
	__property System::UnicodeString SFXStub = {read=FSFXStub, write=FSFXStub};
	__property TFXCOptions* Options = {read=FOptions, write=FOptions};
	__property Classes::TStrings* FileMasks = {read=FFileMasks, write=SetFileMasks};
	__property Classes::TStrings* ExclusionMasks = {read=FExclusionMasks, write=SetExclusionMasks};
	__property Classes::TStrings* NoCompressionMasks = {read=FNoCompressionMasks, write=SetNoCompressionMasks};
	__property bool InMemory = {read=FInMemory, write=SetInMemory, nodefault};
	__property TFXCFileProgressEvent OnFileProgress = {read=FOnFileProgress, write=FOnFileProgress};
	__property TFXCProgressEvent OnOverallProgress = {read=FOnOverallProgress, write=FOnOverallProgress};
	__property TFXCCopyTempFileProgressEvent OnCopyTempFileProgress = {read=FOnCopyTempFileProgress, write=FOnCopyTempFileProgress};
	__property TFXCConfirmOverwriteEvent OnConfirmOverwrite = {read=FOnConfirmOverwrite, write=FOnConfirmOverwrite};
	__property TFXCConfirmProcessFileEvent OnConfirmProcessFile = {read=FOnConfirmProcessFile, write=FOnConfirmProcessFile};
	__property Classes::TNotifyEvent AfterOpen = {read=FAfterOpen, write=FAfterOpen};
	__property TFXCOnPasswordEvent OnPassword = {read=FOnPassword, write=FOnPassword};
	__property TFXCProcessFileFailureEvent OnProcessFileFailure = {read=FOnProcessFileFailure, write=FOnProcessFileFailure};
	__property TFXCOnRequestBlankVolumeEvent OnRequestBlankVolume = {read=FOnRequestBlankVolume, write=FOnRequestBlankVolume};
	__property TFXCOnRequestFirstVolumeEvent OnRequestFirstVolume = {read=FOnRequestFirstVolume, write=FOnRequestFirstVolume};
	__property TFXCOnRequestLastVolumeEvent OnRequestLastVolume = {read=FOnRequestLastVolume, write=FOnRequestLastVolume};
	__property TFXCOnRequestMiddleVolumeEvent OnRequestMiddleVolume = {read=FOnRequestMiddleVolume, write=FOnRequestMiddleVolume};
	__property TFXCOnDiskFullEvent OnDiskFull = {read=FOnDiskFull, write=FOnDiskFull};
	__property TFXCOnStoreFileEvent OnStoreFile = {read=FOnStoreFile, write=FOnStoreFile};
	__property TFXCOnExtractFileEvent OnExtractFile = {read=FOnExtractFile, write=FOnExtractFile};
};


#pragma option push -b-
enum TCryptoTransformMode { Encryption, Decryption };
#pragma option pop

class DELPHICLASS TFXCCryptoTransform;
class PASCALIMPLEMENTATION TFXCCryptoTransform : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCryptoTransformMode FTransformMode;
	
public:
	__fastcall TFXCCryptoTransform(void);
	virtual void __fastcall GenerateKey(const System::AnsiString Password) = 0 ;
	virtual bool __fastcall CheckPassword(const System::AnsiString Password, const TDirItem &Item) = 0 ;
	virtual Sysutils::PByteArray __fastcall GetKey(void) = 0 ;
	virtual System::AnsiString __fastcall GetPassword(void) = 0 ;
	virtual void __fastcall Initialize(TCryptoTransformMode Mode, const TDirItem &Item);
	virtual void __fastcall Reset(void) = 0 ;
	virtual int __fastcall DecryptBuffer(const char * InputBuffer, int inputOffset, int InputCount, char * &OutputBuffer, int OutputOffset) = 0 ;
	virtual int __fastcall EncryptBuffer(const char * InputBuffer, int InputOffset, int InputCount, char * &OutputBuffer, int OutputOffset) = 0 ;
	virtual Fxczlib::PByte __fastcall GetFileStorageStartBlock(void) = 0 ;
	virtual void __fastcall LoadFileStorageStartBlock(Classes::TStream* Stream, __int64 Offset) = 0 ;
	virtual int __fastcall GetFileStorageStartBlockSize(void) = 0 ;
	virtual Fxczlib::PByte __fastcall GetFileStorageEndBlock(void) = 0 ;
	virtual void __fastcall LoadFileStorageEndBlock(Classes::TStream* Stream, __int64 Offset) = 0 ;
	virtual int __fastcall GetFileStorageEndBlockSize(void) = 0 ;
	virtual bool __fastcall IsDirItemCorrupted(const TDirItem &Item, unsigned Crc32) = 0 ;
	virtual TFXCExtraFieldDataBlock __fastcall GetExtraFieldData(void) = 0 ;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFXCCryptoTransform(void) { }
	
};


class DELPHICLASS TAESCryptoTransform;
class PASCALIMPLEMENTATION TAESCryptoTransform : public TFXCCryptoTransform
{
	typedef TFXCCryptoTransform inherited;
	
private:
	Fxcpbkdf_2::TPBKDF2* FKeyGenerator;
	Fxchmac::THMAC_Context FHMAC_Context;
	Fxchash::THashDesc *FPHash;
	Fxccipher::TCipher_Rijndael* FCryptor;
	System::Word FKeyLengthBytes;
	Sysutils::TByteArray *FHashKey;
	Sysutils::TByteArray *FEncryptionKey;
	Sysutils::TByteArray *FPasswordVerificationValue;
	Sysutils::TByteArray *FSaltValue;
	Sysutils::TByteArray *FAuthenticationCode;
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
	virtual Sysutils::PByteArray __fastcall GetKey(void);
	virtual System::AnsiString __fastcall GetPassword(void);
	virtual void __fastcall Initialize(TCryptoTransformMode Mode, const TDirItem &Item);
	virtual void __fastcall Reset(void);
	virtual int __fastcall EncryptBuffer(const char * InputBuffer, int InputOffset, int InputCount, char * &OutputBuffer, int OutputOffset);
	virtual int __fastcall DecryptBuffer(const char * InputBuffer, int InputOffset, int InputCount, char * &OutputBuffer, int OutputOffset);
	virtual TFXCExtraFieldDataBlock __fastcall GetExtraFieldData(void);
	virtual Fxczlib::PByte __fastcall GetFileStorageStartBlock(void);
	virtual void __fastcall LoadFileStorageStartBlock(Classes::TStream* Stream, __int64 Offset);
	virtual int __fastcall GetFileStorageStartBlockSize(void);
	virtual Fxczlib::PByte __fastcall GetFileStorageEndBlock(void);
	virtual void __fastcall LoadFileStorageEndBlock(Classes::TStream* Stream, __int64 Offset);
	virtual int __fastcall GetFileStorageEndBlockSize(void);
};


class DELPHICLASS TFlexCompress;
class PASCALIMPLEMENTATION TFlexCompress : public TBaseArchiver
{
	typedef TBaseArchiver inherited;
	
protected:
	virtual void __fastcall SetCompMethod(void);
	void __fastcall SetCompressionAlgorithm(TFXCCompressionAlgorithm newAlgorithm);
	virtual void __fastcall SetPassword(const System::AnsiString Value);
	
public:
	__fastcall virtual TFlexCompress(Classes::TComponent* AOwner);
	__fastcall virtual ~TFlexCompress(void);
	
__published:
	__property TFXCCompressionAlgorithm CompressionAlgorithm = {read=FCompressionAlgorithm, write=SetCompressionAlgorithm, nodefault};
	__property TFXCCryptoAlgorithm EncryptionAlgorithm = {read=FFXCCryptoAlgorithm, write=FFXCCryptoAlgorithm, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const int DefaultMaxBlockSize = 0x100000;
static const int BlockSizeForFastest = 0x80000;
static const int BlockSizeForNormal = 0x100000;
static const int BlockSizeForMax = 0x180000;
static const int FXCBlockHeaderSize = 0x4;
static const int FXCFileHeaderSize = 0x32;
static const ShortInt CustomHeaderSizeOffset = 0x17;
static const ShortInt FXCNormalFileAttr = 0x20;
static const ShortInt FXCDirectoryAttr = 0x10;
static const int MinVolumeSize = 0x10000;
extern PACKAGE System::UnicodeString ProductName;
static const int FXCZipFileHeaderSize = 0x1e;
static const int FXCZipCentralDirSize = 0x2e;
static const int FXCZipCentralDirEndSize = 0x16;
static const int FXCZip64CentralDirEndSize = 0x38;
static const int FXCZip64CentralDirEndLocatorSize = 0x14;
static const int ZipFileHeaderSignature = 0x4034b50;
static const int FXCFileHeaderSignature = 0x5045c61;
static const int ZipCentralDirSignature = 0x2014b50;
static const int FXCCentralDirSignature = 0x3025c61;
static const int ZipDataDescriptorSignature = 0x8074b50;
static const int ZipCentralDirEndSignature = 0x6054b50;
static const int FXCCentralDirEndSignature = 0x6054141;
static const int Zip64CentralDirEndSignature = 0x6064b50;
static const int Zip64CentralDirEndLocatorSignature = 0x7064b50;
static const ShortInt ZipVersion = 0x14;
static const Word ZipVersionNoOEM = 0xb14;
static const Word FXCVersion = 0x4114;
static const ShortInt Zip64Version = 0x2d;
static const Word FXC64Version = 0x412d;
static const ShortInt ZipGenPurposeFlag = 0x0;
static const ShortInt ZIP_None = 0x0;
static const ShortInt ZIP_ZLIB = 0x8;
static const ShortInt ZIP_Deflate64 = 0x9;
static const Byte FXC_None = 0xff;
static const Byte FXC_ZLIB = 0xfe;
static const Byte FXC_BZIP = 0xfd;
static const Byte FXC_PPM = 0xfc;
static const Word caNone = 0xffff;
extern PACKAGE System::Word AesExtraFieldHeaderID;
extern PACKAGE bool ForceBuildCentralDir;
extern PACKAGE bool __fastcall IsFloppyDrive(const System::UnicodeString FileName);
extern PACKAGE bool __fastcall IsRemovableDrive(const System::UnicodeString FileName);
extern PACKAGE bool __fastcall DelTreeIfNoFiles(const System::UnicodeString Directory);

}	/* namespace Flexcompress */
using namespace Flexcompress;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FlexcompressHPP
