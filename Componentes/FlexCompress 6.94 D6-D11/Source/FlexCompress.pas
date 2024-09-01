//==============================================================================
// Product name: FlexCompress
// Copyright 2002 ComponentAce.
//==============================================================================
unit FlexCompress;

{$I FXCVer.Inc}

{$DEFINE FC_VERSION}


{$DEFINE ENCRYPTION}
{$DEFINE ZLIB}
{$DEFINE PPMD}
{$DEFINE BZIP}
{$DEFINE DIALOGS}
{$DEFINE DELAY_ON}

{$IFDEF D6H}
  {$DEFINE FXCUNICODE}
{$ENDIF}

(*{$IFOPT WARNINGS}
 {$DEFINE FXC_SET_WARNINGS_ON}
 {$WARNINGS OFF}
{$ENDIF}
*)
interface

uses Classes,
{$IFDEF DIALOGS}

{$IFNDEF D16H}
  Dialogs, Forms, Controls, stdctrls, graphics,
{$ELSE}
  VCL.Dialogs, VCL.Forms, VCL.Controls, VCL.stdctrls, VCL.graphics,
{$ENDIF}
  {Author:  Poul Bak}
  {Copyright © 1999-2000 : BakSoft-Denmark (Poul Bak). All rights reserved.}
  {http://home11.inet.tele.dk/BakSoft/}
  {Mailto: baksoft-denmark@dk2net.dk}
  FXCFolderDialog, // directory open dialog by  
{$ENDIF}
  FXCPBKDF_2, FXCHMAC, FXChash,
  Windows, Math, SysUtils
{$IFDEF ZLIB}
  , FXCZlib
{$ENDIF}
{$IFDEF BZIP}
  , FXCBZip2
{$ENDIF}
{$IFDEF PPMD}
  , FXCPPM
{$ENDIF}

  , FXCCipher
{$IFDEF FC_VERSION}
  , FXCRng
{$ENDIF}


{$IFDEF FXCUNICODE}
{$IFNDEF D12H}
  {$IFNDEF D10H}
  , FXCUWideStrings
  {$ENDIF}
  , FXCUClasses
  , FXCUSysUtils
{$ENDIF}
{$ENDIF}

  , FXCConst;

type

  {$IFDEF UNICODE}
  TFXCString = UnicodeString;
  TFXCStrings = TStrings;
  TFXCStringList = TStringList;
  TFXCFileStream = TFileStream;
  TFXCPChar = PChar;
  TFXCChar = Char;
  TFXCSearchRec = TSearchRec;
  {$ELSE}
  {$IFDEF FXCUNICODE}
  TFXCString = widestring;
  TFXCStrings = TTntStrings;
  TFXCStringList = TTntStringList;
  TFXCFileStream = TTntFileStream;
  TFXCPChar = PWideChar;
  TFXCChar = WideChar;
  TFXCSearchRec = TSearchRecW;
  {$ELSE}
  TFXCString = string;
  TFXCStrings = TStrings;
  TFXCStringList = TStringList;
  TFXCFileStream = TFileStream;
  TFXCPChar = PChar;
  TFXCChar = Char;
  TFXCSearchRec = TSearchRec;
  {$ENDIF}
  {$ENDIF}



  TFXCFileHeader = packed record
    BlockSize:    integer; // block size
    CompSize:     integer; // compressed file size (excluding this header)
    UncompSize:   integer; // uncompressed file size
    FileCrc32:    longword; // file check sum
    NumBlocks:    integer; // number of blocks
    CompressionAlgorithm: byte; // compression algorithm
    CompressionMode: byte; // compression mode (0 - none, 9 - max)
    EncryptionAlgorithm: word; // encryption algorithm
    ControlBlockCrc32: longword; // check sum of control block
    ControlBlock: array [0..15] of AnsiChar; // control for encryption
    extraLength:  word; // extra data size (after header)
    UseInitVector: byte;  //reserved:     integer; // for future expanding
    reserved: array [0..2]  of byte;
  end; // 50 bytes

  TFXCBlockHeader = packed record
    packedSize: cardinal; // packed block size
  end; // 4


// FXC
const




  // maximum block size for stream classes, LoadFromStream / SaveToStream
  DefaultMaxBlockSize = 1024 * 1024; // 1.0 Mb for eclNone
  BlockSizeForFastest = 512 * 1024; // 0.5 Mb for fastest modes
  BlockSizeForNormal  = 1024 * 1024; // 1.0 Mb for normal modes
  BlockSizeForMax     = 1536 * 1024; // 1.5 Mb for max modes


  FXCBlockHeaderSize     = sizeof(TFXCBlockHeader); // size in bytes (16)
  FXCFileHeaderSize      = sizeof(TFXCFileHeader);  // size in bytes (128)
  CustomHeaderSizeOffset = 23;

  FXCNormalFileAttr = 32;
  FXCDirectoryAttr  = 16;

  MinVolumeSize = 65536;


 ProductName: string = 'FlexCompress';



type
  TFXCCompressionLevel     = (clNone, clFastest, clNormal, clMax);

  TFXCCompressionAlgorithm = (caZLIB, caBZIP, caPPM);
  {$IFDEF FC_VERSION}
  TFXCCryptoAlgorithm      = (
    caRijndael_128, caRijndael_256,
    caDES_Single, caDES_Triple,
    caBlowfish,
    caTwofish_128, caTwofish_256,
    caSquare);
  {$ENDIF}

  

  {$IFDEF FC_VERSION}
  TInitVector = array [0..FXC_MAX_IVECTOR_SIZE-1] of Byte;
  {$ENDIF} 

  // type of operation
  TFXCProcessOperation = (poAdd, poMove, poDelete, poUpdate, poExtract, poTest,
    poRename, poChangeAttr, poChangeComment, poMakeSFX, poMerge);

  // phase of operation progress
  TFXCProgressPhase = (ppStart, ppProcess, ppEnd);

  // retry | ignore | abort when error occured
  TFXCAction = (fxaRetry, fxaIgnore, fxaAbort);

  // progress event
  TFXCProgressEvent = procedure(Sender: TObject; Progress: double;
    Operation: TFXCProcessOperation; ProgressPhase: TFXCProgressPhase;
    var Cancel: boolean) of object;

  // file item processing progress event
  TFXCFileProgressEvent = procedure(Sender: TObject; FileName: TFXCString;
    // internal or external file depending on operation
    Progress: double; Operation: TFXCProcessOperation;
    ProgressPhase: TFXCProgressPhase; var Cancel: boolean) of object;

  // copying temp file event
  TFXCCopyTempFileProgressEvent = procedure(Sender: TObject;
    Progress: double; ProgressPhase: TFXCProgressPhase; var Cancel: boolean) of
    object;

  // overwrite file prompt event
  TFXCConfirmOverwriteEvent = procedure(Sender: TObject;
    SourceFileName: TFXCString; var DestFileName: TFXCString;
    var Confirm: boolean) of object;

  // confirm processing file item
  TFXCConfirmProcessFileEvent = procedure(Sender: TObject;
    FileName: TFXCString; Operation: TFXCProcessOperation;
    var Confirm: boolean) of object;

  // occurs when password needed
  TFXCOnPasswordEvent = procedure(Sender: TObject; FileName: TFXCString;
    var NewPassword: AnsiString; var SkipFile: boolean) of object;

  // on file processing failure
  TFXCProcessFileFailureEvent = procedure(Sender: TObject; FileName: TFXCString;
    Operation: TFXCProcessOperation; NativeError: integer;
    ErrorCode: integer; ErrorMessage: TFXCString; var Action: TFXCAction) of object;

  // on request blank volume
  TFXCOnRequestBlankVolumeEvent = procedure(Sender: TObject;
    VolumeNumber: integer; var VolumeFileName: TFXCString;
    var Cancel: boolean) of object;

  // on request first volume
  TFXCOnRequestFirstVolumeEvent = procedure(Sender: TObject;
    var VolumeFileName: TFXCString; var Cancel: boolean) of object;
  // on request last volume
  TFXCOnRequestLastVolumeEvent = procedure(Sender: TObject;
    var VolumeFileName: TFXCString; var Cancel: boolean) of object;
  // on request Middle volume
  TFXCOnRequestMiddleVolumeEvent = procedure(Sender: TObject;
    VolumeNumber: integer; var VolumeFileName: TFXCString;
    var Cancel: boolean) of object;

  // on disk full
  TFXCOnDiskFullEvent = procedure(Sender: TObject; VolumeNumber: integer;
    VolumeFileName: TFXCString; var Cancel: boolean) of object;

  // on save/load volume failure
  TFXCProcessVolumeFailureEvent = procedure(Sender: TObject;
    Operation: TFXCProcessOperation; VolumeNumber: integer;
    VolumeFileName: TFXCString; var Cancel: boolean) of object;

  // on store file into the archive
  TFXCOnStoreFileEvent = procedure(Sender: TObject; var FileName: TFXCString;
    var FileAttr: Integer; var Comment: AnsiString; const OriginalFileName: TFXCString) of
    object;

  // on extract file from the archive
  TFXCOnExtractFileEvent = procedure(Sender: TObject; var FileName: TFXCString;
    var FileAttr: longword; const Comment: AnsiString) of object;



type
  //--- ZIP headers
  // Local file header
  TFXCZipFileHeader = packed record
    signature:  longword;   // 4
    extractVersion: word;   // 2
    genPurposeFlag: word;   // 2
    compMethod: word;       // 2
    lastModTime: word;      // 2
    lastModDate: word;      // 2
    crc32:      longword;   // 4
    compSize:   longword;   // 4
    unCompSize: longword;   // 4
    nameLength: word;       // 2  ] = 22
    extraLength: word;      // 2  ] = 0
  end;
  pFXCZipFileHeader = ^TFXCZipFileHeader;

  // Central directory structure
  TFXCZipCentralDir = packed record
    signature:  longword;   // 4
    versionMadeBy: word;    // 2
    extractVersion: word;   // 2
    genPurposeFlag: word;   // 2
    compMethod: word;       // 2
    lastModTime: word;      // 2
    lastModDate: word;      // 2
    crc32:      longword;   // 4
    compSize:   longword;   // 4
    unCompSize: longword;   // 4
    nameLength: word;       // 2
    extraLength: word;      // 2
    commentLength: word;    // 2
    diskNumberStart: word;  // 2
    internalAttr: word;     // 2
    externalAttr: longword; // 4
    relOffsetLH: longword;  // 4
       // file name
       // extra field
       // file comment
  end; // 46
  pFXCZipCentralDir = ^TFXCZipCentralDir;

  // Zip64 end of central dir record
  TFXCZip64CentralDirEnd = packed record
    signature:      longword;
    centralDirEndSize: int64;
    versionMadeBy:  word;
    versionNeededToExtract: word;
    diskNumber:     longword;
    startDiskNumber: longword;
    entriesOnDisk:  int64;
    entriesCentralDir: int64;
    centralDirSize: int64;
    offsetStartDir: int64;
    // Zip64 extensible data sector
  end;
  pFXCZip64CentralDirEnd = ^TFXCZip64CentralDirEnd;

  // Zip64 end of central dir locator
  TFXCZip64CentralDirEndLocator = packed record
    signature: longword;
    startDiskNumber: longword;
    offsetStartDirEnd: int64;
    totalNumberOfDisks: longword;
  end;
  pFXCZip64CentralDirEndLocator = ^TFXCZip64CentralDirEndLocator;

  // Zip end of central directory record
  TFXCZipCentralDirEnd = packed record
    signature:      longword;
    diskNumber:     word;
    startDiskNumber: word;
    entriesOnDisk:  word;
    entriesCentralDir: word;
    centralDirSize: longword;
    offsetStartDir: longword;
    commentLength:  word;
       // Zip file comment
  end; // 22
  pFXCZipCentralDirEnd = ^TFXCZipCentralDirEnd;

  // Extra Field data block
  TFXCExtraFieldDataBlock = packed record
    headerID: word;  // type of data in block
    dataSize: word;  // size in bytes
    pData:    PByte; // data
  end;

  // Zip64 Extended Information Extra Field
  TFXCZip64ExtendedInfo = packed record
    //  HeaderID:       Word;     // type of data in block = 0x0001
    //  DataSize:       Word;     // size in bytes = 32?
    uncompSize:  int64;    // uncompressed file size
    compSize:    int64;    // compressed file size
    relOffsetLH: int64;    // compressed file size
    diskNumberStart: longword; // start disk No
  end;

  // used by zip with non-seekable streams
  TFXCZipDataDescriptor = packed record
    Signature: longint;
    crc32:     longint;
    compressedSize: longint;
    uncompressedSize: longint;
  end;

  TZipKey = array [0..2] of int64; // bug fix, was Integer
  TZipKeyHeader = array [0..11] of byte;

  TFXCAESencryptionExtraField = packed record
     versionNumber : word;
     keyLengthBits : longint;
     strength : Byte;
     compressionMethod : word;
     vendorID : array [0..2] of byte;
  end;

const
  // FXC Zip
  FXCZipFileHeaderSize      = sizeof(TFXCZipFileHeader);
  FXCZipCentralDirSize      = sizeof(TFXCZipCentralDir);
  FXCZipCentralDirEndSize   = sizeof(TFXCZipCentralDirEnd);
  FXCZip64CentralDirEndSize = sizeof(TFXCZip64CentralDirEnd);
  FXCZip64CentralDirEndLocatorSize = sizeof(TFXCZip64CentralDirEndLocator);

  ZipFileHeaderSignature     = $04034b50;
  FXCFileHeaderSignature     = $05045c61;
  ZipCentralDirSignature     = $02014b50;
  FXCCentralDirSignature     = $03025c61;
  ZipDataDescriptorSignature = $08074b50;

  ZipCentralDirEndSignature = $06054b50;
  FXCCentralDirEndSignature = $06054141;

  Zip64CentralDirEndSignature = $06064b50;
  Zip64CentralDirEndLocatorSignature = $07064b50;

  ZipVersion      = $0014;
  ZipVersionNoOEM = $0B14;  // no oem file name encoding
  FXCVersion      = $4114;  // for FXC format 2.0 or >

  //Zip64Version = 0045;
  Zip64Version = $002D;
  FXC64Version = $412D; // for FXC format 2.0 or >

  ZipGenPurposeFlag = $0000;
  ZIP_None = $0000;
  ZIP_ZLIB = $0008;
  ZIP_Deflate64 = $0009;
  FXC_None = $00FF;
  FXC_ZLIB = $00FE;
  FXC_BZIP = $00FD;
  FXC_PPM = $00FC;
  caNone  = $FFFF;

type
  TFXCOverwriteMode = (omPrompt, omAlways, omNever, omIfNewer, omIfOlder);
  TFXCFileShareMode = (smShareCompat, smShareExclusive, smShareDenyWrite,
    smShareDenyRead, smShareDenyNone);

  // internal record for search info
  TInternalSearchRec = packed record
    ItemNo:     integer;    // current item No
    CFindMask:  array [0..MAX_PATH - 1] of TFXCChar; // mask for FindNext
    FWildCards: boolean;    // wildcards in Find mask?
    FFindAttr:  integer;    // Attr for FinidFirst/Next
    ExclusionMask: TFXCString;  // files to exclude
    UseProperties: boolean; // use FileMasks, ExclusionMasks?
  end;

  TFXCArchiveItem = packed record
    FileName:TFXCString;
    StoredPath:TFXCString;
    CompressedSize: int64;
    UncompressedSize: int64;
    CompressionRate: double;
    Encrypted: boolean;
    LastModFileDate: word;
    LastModFileTime: word;
    CRC:     longint;
    ExternalFileAttributes: longword;
    Comment: AnsiString;
    Handle:  TInternalSearchRec;
  end;

  // how to store paths
  TFXCStorePathMode = (spNoPath, spRelativePath, spFullPath, spFullPathWithDrive);

  // Zip64 format mode
  TZip64Mode = (zmDisabled, zmAuto, zmAlways);

  // one volume, one volume on separate disks, multiple volumes one the same disk
  TFXCSpanningMode = (smNone, smSpanning, smSplitting);

  // auto-detect, custom, 1.44MB, ...
  TFXCVolumeSize = (vsAutoDetect, vsCustom, vs1_44MB, vs100MB, vs200MB, vs250MB, vs600MB,
    vs650MB, vs700MB, vs4700MB);

  // info to identify volume number
  TFXCVolumeNumberKind = (vnkFirst, vnkLast, vnkCustom, vnkUnknown);

  TFXCVolumeNumberInfo = class(TObject)
  private
    FVolumeKind:   TFXCVolumeNumberKind;
    FVolumeNumber: integer;
    FLastVolumeNumber: integer;

    function GetFirstVolume: boolean;
    procedure SetFirstVolume(Value: boolean);
    function GetLastVolume: boolean;
    procedure SetLastVolume(Value: boolean);
    procedure SetVolumeNumber(Value: integer);
  public
    constructor Create;
    function IsEqualTo(VolumeInfo: TFXCVolumeNumberInfo): boolean;
    procedure Init;

    property FirstVolume: boolean Read GetFirstVolume Write SetFirstVolume;
    property LastVolume: boolean Read GetLastVolume Write SetLastVolume;
    property VolumeNumber: integer Read FVolumeNumber Write SetVolumeNumber;
    property LastVolumeNumber: integer Read FLastVolumeNumber Write FLAstVolumeNumber;
  end;

  // multi-spanning options
  TFXCSpanningOptions = class(TPersistent)
  private
    FAdvancedNaming:  boolean;
    FFirstVolumeSize: int64;
    FVolumeSize:      TFXCVolumeSize;
    FCustomVolumeSize: int64;
    FSaveDirToFirstVolume: boolean;

    procedure SetCustomVolumeSize(Value: int64);

  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    // gets requested volume size
    function GetVolumeSize(VolumeNumberInfo: TFXCVolumeNumberInfo): int64;
  published
    // .ZIP, .Z00 | _01.ZIP
    property AdvancedNaming: boolean Read FAdvancedNaming Write FAdvancedNaming;
    // size of first volume (to reserve free space on first disk)
    property FirstVolumeSize: int64 Read FFirstVolumeSize Write FFirstVolumeSize;
    // auto-detect, custom, 1.44MB, ...
    property VolumeSize: TFXCVolumeSize Read FVolumeSize Write FVolumeSize;
    // if VolumeSize=Custom
    property CustomVolumeSize: int64 Read FCustomVolumeSize Write SetCustomVolumeSize;
    // store central dirs + cdirend in first volume (if necessary)?
    //   property SaveDirToFirstVolume: Boolean read FSaveDirToFirstVolume write FSaveDirToFirstVolume;
  end;

  // archive options
  TFXCOptions = class(TPersistent)
  private
    // if true files in zip archive will be stored with path info, like 'dir1/file1.txt'
    FStorePath:    TFXCStorePathMode;
    // if true then all files from specified subdirectories will be taken for current operation
    FRecurse:      boolean;
    // open sharing when adding files
    FShareMode:    TFXCFileShareMode;
    // how to overwrite
    FOverwriteMode: TFXCOverwriteMode;
    // create folders when extracting?
    FCreateDirs:   boolean;
    // overwrite read-only files?
    FReplaceReadOnly: boolean;
    // set attributes to extracted files
    FSetAttributes: boolean;
    // search attributes
    FSearchAttr:   integer;
    // flush file buffers
    FFlushBuffers: boolean;
    // file names stored in OEM character set
    FOEMFileNames: boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Recurse: boolean Read FRecurse Write FRecurse default True;
    property StorePath: TFXCStorePathMode
      Read FStorePath Write FStorePath default spRelativePath;
    property ShareMode: TFXCFileShareMode
      Read FShareMode Write FShareMode default smShareDenyNone;
    property OverwriteMode: TFXCOverwriteMode
      Read FOverwriteMode Write FOverwriteMode default omAlways;
    property CreateDirs: boolean Read FCreateDirs Write FCreateDirs default True;
    property ReplaceReadOnly: boolean Read FReplaceReadOnly
      Write FReplaceReadOnly default True;
    property SetAttributes: boolean
      Read FSetAttributes Write FSetAttributes default True;
    property SearchAttr: integer Read FSearchAttr Write FSearchAttr default
      DefaultSearchAttr;
    property FlushBuffers: boolean Read FFlushBuffers Write FFlushBuffers;
    property OEMFileNames: boolean Read FOEMFileNames Write FOEMFileNames;
  end;

type
  // each item describes one file (CentralDir+Name+Comments)
  TDirItem = packed record
    CentralDir: TFXCZipCentralDir;
    Name:     TFXCString;               // file or folder name/path
    OldName:  TFXCString;               // file or folder name/path before renaming
    Comment:   AnsiString;               // comment
    ExtraFields: array of TFXCExtraFieldDataBlock; // additional data
{$IFDEF FC_VERSION}
    FXEncryptionAlgorithm : TFXCCryptoAlgorithm;   //algorithm used to compress item
{$ENDIF}
    

    bHugeFile: boolean;              // is Zip64 format?
    Zip64ExtInfo: TFXCZip64ExtendedInfo; // extended info
    Modified:  boolean;              // is item new or updated
    Password:  AnsiString;               // password or ''
    Stream:    TStream;              // source data stream (when there is no file)
    bDestroyStream: boolean;         // destroy stream?
    Position:  integer;              // starting position in the stream
    Tagged:    boolean;              // for selection and group operations
    SrcFileName:TFXCString;             // external file name used for add/move/update
    CompressionMethod : word;        //real compression method
    CompressionMode: byte;           // applied compression level
    Operation: TFXCProcessOperation; // add/move/update
    UseOldRijndael : boolean;
  end;
  PDirItem = ^TDirItem;

  { TFXCStringHash - used internally to optimize searches. }
  PPHashItem = ^PHashItem;
  PHashItem  = ^THashItem;

  THashItem = record
    Next:  PHashItem;
    Key:   TFXCString;
    Value: integer;
  end;

  TFXCStringHash = class
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: TFXCString): PPHashItem;
    function HashOf(const Key: TFXCString): cardinal; virtual;
  public
    Size: cardinal;

    constructor Create(aSize: cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: TFXCString; Value: integer);
    procedure Clear;
    procedure Remove(const Key: TFXCString);
    function Modify(const Key: TFXCString; Value: integer): boolean;
    function ValueOf(const Key: TFXCString): integer;
  end;

  { TFXCHashedStringList - A TStringList that uses TFXCStringHash to improve the
    speed of Find }
  TFXCHashedStringList = class(TFXCStringList)
  private
    FValueHash:     TFXCStringHash;
    FNameHash:      TFXCStringHash;
    FValueHashValid: boolean;
    FNameHashValid: boolean;
    procedure UpdateValueHash;
  protected
    procedure UpdateNameHash;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: TFXCString): integer; override;
    procedure Update(Index: integer; const NewValue: TFXCString);
    procedure Delete(Index: integer); override;
    procedure Clear; override;
    function IndexOf(const S: TFXCString): integer; override;
{$IFDEF D6H}
    function IndexOfName(const Name: TFXCString): Integer; override;
{$ENDIF}
  end;

  // array of central dir
  TFXCDirArray = class(TObject)
  private
    FItems:     array of TDirItem; // DirItems
    FItemNamesList: TFXCHashedStringList;
    // A hashed TFXCStringList contains FItems.Name, for a quick find.
    AllocBy:    integer;
    deAllocBy:  integer;
    MaxAllocBy: integer;
    AllocItemCount: integer;
    ItemCount:  integer;

    // set/get item
    procedure SetItem(Index: integer; const Value: TDirItem);
    function GetItem(Index: integer): TDirItem;
    function GetItemPtr(Index: integer): PDirItem;

    // set/get size
    procedure SetCount(Value: integer);
    function GetCount: integer;

  public
    // instantiate object
    constructor Create;
    // destructor
    destructor Destroy; override;

    procedure Clear;

    // assign other array
    procedure Assign(da: TFXCDirArray);
    // append other array
    procedure Append(da: TFXCDirArray);
    // delete item (array is squeezed)
    procedure DeleteItem(Index: integer);
    // clear tags
    procedure ClearTags;
    function FileExists(const FileName: TFXCString; var ItemNo: integer): boolean;
    // update item name in hash list
    procedure SetItemName(Index: integer; const Name: TFXCString);

    property Items[Index: integer]: TDirItem Read GetItem Write SetItem;
    property ItemsPtr[Index: integer]: PDirItem Read GetItemPtr;

    property Count: integer Read GetCount Write SetCount;

  end;

  TBaseArchiver = class;

  // manages Central dirs and CDirEnd
  TFXCDirManager = class(TObject)
  private
    FCompressedStream: TStream;  // archive file stream
    FCreate: boolean;
    FIsCustomStream: boolean;
    FArc:    TBaseArchiver;

    function FindSignature(Stream: TStream; var Offset: int64;
      Signature: PAnsiChar; SignatureLen: integer): boolean;
    //find local file header in the compressed stream
    function FindLocalFileHeader(StartPosition: int64): int64;
    // save debug
    //   procedure SaveDebug;
    // process volume request
    procedure OpenVolume(VolumeNumberInfo: TFXCVolumeNumberInfo);
    // inits central dir end
    procedure InitCentralDirEnd;
    // returns central dir end
    function GetCentralDirEnd(var DirEnd: TFXCZipCentralDirEnd;
      var position: int64; Signature: longword): boolean;
    // detects stub and+ gets its size
    procedure CalculateStubSize(DirEndPos: int64);
    // saves SFX stub to empty archive
    procedure SaveSFXStub;

  public
    RBCDir:   TFXCDirArray; // rollback DirItems
    CDir:     TFXCDirArray; // current DirItems
    ArchiveComment: AnsiString;
    Zip64CentralDirEndLocator: TFXCZip64CentralDirEndLocator;
    Zip64CentralDirEnd: TFXCZip64CentralDirEnd;
    CentralDirEnd: TFXCZIPCentralDirEnd;
    StubSize: int64; // stub size if sfx archive is used
    FZip64:   boolean;
    Aborted:  boolean;
    CentralDirOffset: int64;


    // instantiate object
    constructor Create(Stream: TStream; bCreate: boolean; Arc: TBaseArchiver);
    // destructor
    destructor Destroy; override;
    // Is Encrypted
    function IsEncrypted: boolean;
    // save all dirs
    procedure SaveDir(RecreateCDirEnd: boolean; Stream: TStream = nil);
    // load all dirs
    procedure LoadDir;
    // add stub offset to relative offsets
    procedure ApplyStubOffset;
    // stream has CDirEnd within?
    function HasCentralDirEnd: boolean;
    // has SFX stub?
    function IsSFXArchive: boolean;
    // merge with central dir of another archive
    procedure MergeWith(DMHandleToAdd: TFXCDirManager);
  end;

  TBaseArchiver = class(TComponent)
  private
    FCurrentDir:  TFXCString;
    FUpdateCount: integer; // transactions in process?
    FActive:      boolean; // archive is open
    isZIPFormat:  boolean; // if true - TZipForge,
    // else TFlexCompress
    IsCustomStream: boolean; // File or custom stream?
    FFileName:    TFXCString;
    VolumeFileName: TFXCString;
    FVolumeNumberInfo: TFXCVolumeNumberInfo;
    FileOpenMode: word;    // fmCreate or fmShare...
    FTempDir:     TFXCString;
    FBaseDir:     TFXCString;
    FSFXStub:     TFXCString;      // SFX stub
    FOptions:     TFXCOptions; // store options
    FFileMasks:   TFXCStrings;    // list of files to exclude from operation
    FExclusionMasks: TFXCStrings;
    FNoCompressionMasks: TFXCStrings;   // list of files to not compress
    FInMemory:    boolean;           // in-memory archive?
    FProcessedFileNo: integer;       // cur No for total progress
    FProcessedFileCount: integer;    // count for total progress
    FProcessedFilesTotalSize: int64;  // total size of files to be procced
    FProcessedFilesSize: int64;      // total size of processed files
    FProgressCancel: boolean;        // progress cancelled?
    FProgressEnabled: boolean;       // call progress handlers?
    FSkipFile:    boolean;           // for OnPassword
    FSpanningMode: TFXCSpanningMode; // one volume, multiple-disks, multiple-volumes
    FSpanningOptions: TFXCSpanningOptions; // multi-part archive options
    FExtractCorruptedFiles: boolean; // extract corrupted files without exceptions
    FOpenCorruptedArchives: boolean;
    {$IFDEF FXCUNICODE}
    FUnicodeFilenames: boolean;
    {$ENDIF}
    // open corrupted aechive (don't request last volume)
    FProgress, FProgressMax: int64;
    FNoProgress:  boolean;
    FOnFileProgress: TFXCFileProgressEvent; // progress for bulk operations
    FOnOverallProgress: TFXCProgressEvent;  // progress for bulk operations
    FOnCopyTempFileProgress: TFXCCopyTempFileProgressEvent;
    // progress for bulk operations
    FOnConfirmOverwrite: TFXCConfirmOverwriteEvent; // confirm file overwrite
    FOnConfirmProcessFile: TFXCConfirmProcessFileEvent; // confirm file overwrite
    FAfterOpen:   TNotifyEvent; // occurs after archive opening
    FOnPassword:  TFXCOnPasswordEvent; // occurs when password needed
    FOnProcessFileFailure: TFXCProcessFileFailureEvent;
    // occurs when file processing failed
    FOnRequestFirstVolume: TFXCOnRequestFirstVolumeEvent; // on requesting first volume
    FOnRequestLastVolume: TFXCOnRequestLastVolumeEvent; // on requesting last volume
    FOnRequestMiddleVolume: TFXCOnRequestMiddleVolumeEvent;
    // on requesting Middle volume
    FOnRequestBlankVolume: TFXCOnRequestBlankVolumeEvent; // on requesting new volume
    FOnDiskFull:  TFXCOnDiskFullEvent;      // disk is full event
    FOnProcessVolumeFailure: TFXCProcessVolumeFailureEvent; // volume save failed
    FOnStoreFile: TFXCOnStoreFileEvent;     // file is added/updated
    FOnExtractFile: TFXCOnExtractFileEvent; // file is being extracted
    FSuppressPasswordEvent: boolean;

    FUpdated: boolean;

  protected
    DMHandle: TFXCDirManager; // Central dirs, CDirEnd, ...

    // none,ZLIB (ZIP-compatible), PPM good, PPM max
    FCompressionMethod: word;
    FCompressionAlgorithm: TFXCCompressionAlgorithm;
{$IFDEF FC_VERSION}
    FFXCCryptoAlgorithm: TFXCCryptoAlgorithm;     //Compression algorithm for FlexCompress
{$ENDIF}


    FCompressionMode: byte; // 0-9
    FCompressionLevel: TFXCCompressionLevel;
    FPassword:    AnsiString;
    FCompressedStream: TStream; // stream containing archive
    FZip64Mode: TZip64Mode;     // Zip64 | Zip32 ?

{$IFDEF FC_VERSION}
    FInitVector:        TInitVector;
    FInitVectorMaxSize: Integer;
    FUseInitVector:     boolean;
{$ENDIF}
    Dummy1:     integer;
    Dummy2:     integer;
    Dummy3:     integer;
    Dummy4:     integer;

    function GetInUpdate: boolean;
    procedure SetInMemory(Value: boolean);

    function GetFileComment: AnsiString;
    procedure SetFileComment(const Value: AnsiString);

    procedure SetPassword(const Value: AnsiString); virtual;

    procedure SetFileMasks(Value: TFXCStrings);
    procedure SetExclusionMasks(Value: TFXCStrings);
    procedure SetNoCompressionMasks(Value: TFXCStrings);

    procedure Lock;
    procedure Unlock;
    procedure CheckInactive;
    procedure CheckInUpdate;
    procedure CheckModifySpanning;
    procedure SetActive(Value: boolean);
    function GetEncrypted: boolean;
    function GetExists: boolean;
    function GetArchiveSize: int64;
    function GetFileCount: integer;
    procedure SetCompressionLevel(newLevel: TFXCCompressionLevel);
    procedure SetCompressionMode(newMode: byte);
{$IFDEF FC_VERSION}
    function GetMaxInitVectorSize: Integer;
{$ENDIF}
    function GetCurrentVersionText: TFXCString;
    procedure SetCurrentVersionText(s: TFXCString);
    procedure SetSpanningMode(Value: TFXCSpanningMode);
    // compress file from stream to stream
    function InternalCompressFile(rfs, wfs: TStream; var d: TDirItem; rfsSize: int64): boolean;
    // checks ZIP password
    function ZipInitPassword(rfs: TStream; const FPassword: AnsiString;
      genPurposeFlag: word; lastModTime: word; crc32: longword;
      var FKey: TZipKey): boolean;
    // decompress file from stream to stream
    function InternalDecompressFile(rfs, wfs: TStream; var d: TDirItem; SizeToDecompress: Integer=0; StartPosition: Int64=0): boolean;
    procedure SetCompMethod; virtual;
    // convert to standard const
    function ShareModeToInt(ShareMode: TFXCFileShareMode): integer;
    // used by AddFromStream,AddFromBuffer, AddFromString
    procedure InternalAddFromStream(const FileName: TFXCString;
      Stream: TStream; CopyToBuffer: Boolean; DestroyStream: Boolean; Position: Int64;
      Count: Int64; Attr: integer; DateTime: TDateTime);

    // forces save changes
    procedure ForceUpdate;
    // init dir structures
    procedure FillDirItem(ItemNo: integer; const FileName: TFXCString;
      UseDiskFileData: Boolean; Attr: Integer; DateTime: TDateTime);
    // extracts item
    procedure ExtractItem(ItemNo: integer; DestStream: TStream; Count: Integer = 0; StartPosition: Int64 = 0);
    // updates item
    procedure UpdateItem(ItemNo: integer; const SrcFileName: TFXCString);
    // deletes item
    procedure DeleteItem(ItemNo: integer);
    // check attributes for search condition
    function CheckAttributesMatch(FileAttr, SearchAttr: integer): boolean;
    // check item for search condition
    function IsItemMatches(ItemNo: integer; var F: TFXCArchiveItem): boolean;
    // find files
    function InternalFind(var F: TFXCArchiveItem): boolean;
    // is file within archive matches mask
    function IsInternalFileMatchMask(const FileName, FileMask: TFXCString): boolean;
    // is file on disk matches mask
    function IsExternalFileMatchMask(const FileName, FileMask: TFXCString;
      IsDir: boolean): boolean;
    // Get temp file name
    function GetTempFileName: TFXCString;
    // tag files
    procedure TagFiles; overload;
    procedure TagFiles(const FileMask: TFXCString; SearchAttr: integer;
      ExclusionMask: TFXCString); overload;
    // deletes, updates, extracts or tests tagged files
    procedure ProcessTaggedFiles(Operation: TFXCProcessOperation; TaggedFile : integer = -1);
    // delete tagged file
    procedure DeleteTaggedFile(ItemNo: integer);
    // update tagged file
    procedure UpdateTaggedFile(ItemNo: integer; const FileName, Path: TFXCString);
    // extract tagged file
    procedure ExtractTaggedFile(ItemNo: integer; const FileName, Path: TFXCString);
    // test tagged file
    procedure TestTaggedFile(ItemNo: integer; const FileName: TFXCString);
    // get full file mask
    function GetFullMask(const Mask, BaseDir: TFXCString; var bRecurse: boolean): TFXCString;
    // internal add files
    procedure InternalAddFiles(FileMasks: TFXCStrings; SearchAttr: integer;
      ExclusionMasks: TFXCStrings; bMove: boolean; bRecurse: boolean);

    // initialises data to add a file
    function AddFileInit(const FileName, BaseDir1: TFXCString;
  UseDiskFileData: Boolean; Attr: Integer; DateTime: TDateTime): integer;

    // reopen file stream (multi-spanning)
    procedure ReopenFileStream(const NewFileName: TFXCString);

    // prepare default volume name
    procedure MakeDefaultVolumeName(var VolumeFileName: TFXCString;
      VolumeNumber: integer; CheckIfFileExists: boolean);

    // gets free drive space
    function GetFreeDriveSpace(const VolumeFileName: TFXCString): int64;

    // writes to stream with request of new volume
    function WriteToStream(const Buffer; Count: longint; var Stream: TStream;
      VolumeNumberInfo: TFXCVolumeNumberInfo; RequiredFreeSpace: integer = -1;
      CacheStream: TStream = nil): longint; overload;

    // writes to stream with request of new volume
    function WriteToStream(SrcStream: TStream; var DestStream: TStream;
      VolumeNumberInfo: TFXCVolumeNumberInfo; var Cancel: boolean;
      Size: int64 = -1; RequiredFreeSpace: integer = -1;
      CacheStream: TStream = nil; IsCopyTempFile: boolean = False): longint;
      overload;

    // writes buffer to stream with request of new volume
    procedure WriteBufferToStream(const Buffer; Count: longint;
      var Stream: TStream; VolumeNumberInfo: TFXCVolumeNumberInfo;
      RequiredFreeSpace: integer = -1; CacheStream: TStream = nil);

    // process volume request
    procedure OpenVolume(VolumeNumberInfo: TFXCVolumeNumberInfo);

    // is ArcFileName EXE?
    function IsSFXArchive(const ArcFileName: TFXCString): boolean;

    // creates archive
    procedure InternalCreateArchive;

    // opens non-SFX archive
    procedure InternalOpenNonSFXArchive;

    // opens SFX archive
    procedure InternalOpenSFXArchive;

    // determines single file or splitting or spanning
    procedure DetectSpanning;

    // opens archive
    procedure InternalOpenArchive;
    // after archive open
    procedure DoAfterOpen;
    // on overall progress
    procedure DoOnOverallProgress(Progress: double;
      Operation: TFXCProcessOperation; ProgressPhase: TFXCProgressPhase;
      var Cancel: boolean); virtual;
    // on file progress
    procedure DoOnFileProgress(const FileName: TFXCString;
    // internal or external file depending on operation
      Progress: double; Operation: TFXCProcessOperation;
      ProgressPhase: TFXCProgressPhase; var Cancel: boolean); virtual;
    // on copy temp file progress
    procedure DoOnCopyTempFileProgress(Progress: double;
      ProgressPhase: TFXCProgressPhase; var Cancel: boolean); virtual;

    // confirm overwrite prompt
    procedure DoOnConfirmOverwrite(SourceFileName: TFXCString;
      var DestFileName: TFXCString; var Confirm: boolean); virtual;

    // confirm file processing
    procedure DoOnConfirmProcessFile(const FileName: TFXCString;
      Operation: TFXCProcessOperation; var Confirm: boolean); virtual;

    // occurs when password needed
    procedure DoOnPassword(const FileName: TFXCString; var NewPassword: AnsiString;
      var SkipFile: boolean); virtual;

    // on file processing failure
    procedure DoOnProcessFileFailure(const FileName: TFXCString;
      Operation: TFXCProcessOperation; NativeError: integer;
      ErrorCode: integer; const ErrorMessage: TFXCString;
      var Action: TFXCAction); virtual;

    // on request blank volume
    procedure DoOnRequestBlankVolume(VolumeNumber: integer;
      var VolumeFileName: TFXCString; var Cancel: boolean); virtual;

    // on request 1st volume
    procedure DoOnRequestFirstVolume(var VolumeFileName: TFXCString;
      var Cancel: boolean); virtual;

    // on request last volume
    procedure DoOnRequestLastVolume(var VolumeFileName: TFXCString;
      var Cancel: boolean); virtual;

    // on request Middle volume
    procedure DoOnRequestMiddleVolume(VolumeNumber: integer;
      var VolumeFileName: TFXCString; var Cancel: boolean); virtual;

    // on disk full
    procedure DoOnDiskFull(VolumeNumber: integer; const VolumeFileName: TFXCString;
      var Cancel: boolean); virtual;

    // on process volume failure
    procedure DoOnProcessVolumeFailure(Operation: TFXCProcessOperation;
      VolumeNumber: integer; const VolumeFileName: TFXCString;
      var Cancel: boolean); virtual;

    // on store file
    procedure DoOnStoreFile(var FileName: TFXCString; var FileAttr: Integer;
      var Comment: AnsiString; const OriginalFileName: TFXCString);
      virtual;

    // on extract file
    procedure DoOnExtractFile(var FileName: TFXCString; var FileAttr: longword;
      const Comment: AnsiString); virtual;

    function GetFileCendralDirItemNo(FileName: TFXCString; out ItemNo: integer): boolean;
    function WriteToStreamWithOnDiskFull(Stream: TStream; var Buffer; Count: Integer): Boolean;

{$IFDEF FC_VERSION}
    procedure FillRandomBuffer(buf: PByteArray; BufSize: Integer);

    procedure SetIVByte(Index: Integer; const Value: Byte);
    function GetIVByte(Index: Integer): Byte;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // opens archive
    procedure OpenArchive; overload;
    procedure OpenArchive(Mode: word); overload;
    procedure OpenArchive(Stream: TStream; Create: boolean); overload;
    // closes archive
    procedure CloseArchive;

    // starts transaction
    procedure BeginUpdate;
    // saves changes
    procedure EndUpdate;
    // cancels changes
    procedure CancelUpdate;

    // adds stream data to archive as a file
    procedure AddFromStream(const FileName: TFXCString; Stream: TStream;
      CopyToBuffer: boolean = True; Position: int64 = 0;
      Count: int64 = 0; Attr: integer = faArchive; DateTime: TDateTime = 0);
    // adds buffer data to archive as a file
    procedure AddFromBuffer(const FileName: TFXCString; const Buffer;
      Count: integer; Attr: integer = faArchive; DateTime: TDateTime = 0);
    // adds TFXCString data to archive as a file
    procedure AddFromString(const FileName: TFXCString; const Text: string;
      Attr: integer = faArchive; DateTime: TDateTime = 0);

    // extracts file from archive  to stream
    procedure ExtractToStream(const FileName: TFXCString; Stream: TStream);
    // extracts file from archive to buffer
    procedure ExtractToBuffer(const FileName: TFXCString; var Buffer; Count: integer; StartPosition: Int64);
    // extracts file from archive to TFXCString
    procedure ExtractToString(const FileName: TFXCString; var Text: string);

    // fast appending file
    function FastFileAppend(const OuTFXCFileStream, InFileStream: TFXCFileStream;
      const FileName: TFXCString): boolean;
    // makes SFX
    procedure MakeSFX(const SFXFileName: TFXCString);

    // finds a file
    function FindFirst(var F: TFXCArchiveItem): boolean; overload;
    function FindFirst(const FileMask: TFXCString; var F: TFXCArchiveItem;
      SearchAttr: integer = DefaultSearchAttr;
      const ExclusionMask: TFXCString = ''): boolean; overload;
    function FindNext(var F: TFXCArchiveItem): boolean;

    // renames file within archive
    procedure RenameFile(const OldName, NewName: TFXCString);

    // changes attributes
    procedure ChangeFilesAttr(const FileMask: TFXCString; NewAttr: longword);

    // changes internal attributes
    procedure ChangeFilesIntAttr(const FileMask: TFXCString; NewAttr: longword);

    // changes files comment
    procedure ChangeFilesComment(const FileMask: TFXCString; const NewComment: AnsiString);

    // checks whether the password is valid for specified file in archive
    function IsFilePasswordValid(const FileName: TFXCString;
      const Password: AnsiString): boolean;

    // adds files (uses list properties)
    procedure AddFiles; overload;
    // adds files (uses file mask, attributes and exclusion mask)
    procedure AddFiles(const FileMask: TFXCString; SearchAttr: integer = DefaultSearchAttr;
      const ExclusionMask: TFXCString = ''); overload;

    // moves files (uses list properties)
    procedure MoveFiles; overload;
    // moves files (uses file mask, attributes and exclusion mask)
    procedure MoveFiles(const FileMask: TFXCString; SearchAttr: integer = DefaultSearchAttr;
      const ExclusionMask: TFXCString = ''); overload;

    // deletes files (uses list properties)
    procedure DeleteFiles; overload;
    // deletes files (uses file mask, attributes and exclusion mask)
    procedure DeleteFiles(const FileMask: TFXCString;
      SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
      overload;

    // updates files (uses list properties)
    procedure UpdateFiles; overload;
    // updates files (uses file mask, attributes and exclusion mask)
    procedure UpdateFiles(const FileMask: TFXCString;
      SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
      overload;

    // tests files (uses list properties)
    procedure TestFiles; overload;
    // tests files (uses file mask, attributes and exclusion mask)
    procedure TestFiles(const FileMask: TFXCString; SearchAttr: integer = DefaultSearchAttr;
      const ExclusionMask: TFXCString = ''); overload;

    // input file is specified by FileName property
    // output file by default will be the same as input
    procedure RepairArchive(const OutputFileName: TFXCString = '');

    // extracts files (uses list properties)
    procedure ExtractFiles; overload;
    // extract files (uses file mask, attributes and exclusion mask)
    procedure ExtractFiles(const FileMask: TFXCString;
      SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
      overload;

    function IsValidArchiveFile: boolean;

{$IFDEF FC_VERSION}
    property UseInitVector: Boolean read FUseInitVector write FUseInitVector;
    property MaxInitVectorSize: Integer read GetMaxInitVectorSize;
    property InitVector[Index: Integer]: Byte read GetIVByte write SetIVByte;
    procedure GenerateIV;
    procedure SetInitVector(IV: Pointer; Size: Integer);
{$ENDIF}

    property VolumeNumberInfo: TFXCVolumeNumberInfo Read FVolumeNumberInfo;
    property Size: int64 Read GetArchiveSize;
    property FileCount: integer Read GetFileCount;
    property Exists: boolean Read GetExists;

    // is transaction processing?
    property InUpdate: boolean Read GetInUpdate;
    // is archive file open
    property Active: boolean Read FActive Write SetActive;
    // file comment
    property Comment: AnsiString Read GetFileComment Write SetFileComment;
    // open corrupted files
    property OpenCorruptedArchives: boolean
      Read FOpenCorruptedArchives Write FOpenCorruptedArchives;

  published
    // extract corrupted files without exceptions
    property ExtractCorruptedFiles: boolean
      Read FExtractCorruptedFiles Write FExtractCorruptedFiles;
    // compression levels
    property CompressionLevel: TFXCCompressionLevel
      Read FCompressionLevel Write SetCompressionLevel;
    // compression levels
    property CompressionMode: byte Read FCompressionMode Write SetCompressionMode;

    // ZIP file name
    property CurrentVersion: TFXCString Read GetCurrentVersionText
      Write SetCurrentVersionText;
    property Password: AnsiString Read FPassword Write SetPassword;
    property FileName: TFXCString Read FFileName Write FFileName;
    property BaseDir: TFXCString Read FBaseDir Write FBaseDir;
    property TempDir: TFXCString Read FTempDir Write FTempDir;
    // one volume, spanning, splitting?
    property SpanningMode: TFXCSpanningMode Read FSpanningMode Write SetSpanningMode;
    // multi-spanning options
    property SpanningOptions: TFXCSpanningOptions
      Read FSpanningOptions Write FSpanningOptions;
    // SFX stub file if any
    property SFXStub: TFXCString Read FSFXStub Write FSFXStub;
    // store options
    property Options: TFXCOptions Read FOptions Write FOptions;
    // list of files to operate with
    property FileMasks: TFXCStrings Read FFileMasks Write SetFileMasks;
    // list of files to exclude from operation
    property ExclusionMasks: TFXCStrings Read FExclusionMasks Write SetExclusionMasks;
    // list of files to not compress
    property NoCompressionMasks: TFXCStrings Read FNoCompressionMasks
      Write SetNoCompressionMasks;
    // In-memory mode
    property InMemory: boolean Read FInMemory Write SetInMemory;

    // File Progress Event
    property OnFileProgress: TFXCFileProgressEvent
      Read FOnFileProgress Write FOnFileProgress;
    // Total Progress Event
    property OnOverallProgress: TFXCProgressEvent
      Read FOnOverallProgress Write FOnOverallProgress;
    // Copy Temp File Progress Event
    property OnCopyTempFileProgress: TFXCCopyTempFileProgressEvent
      Read FOnCopyTempFileProgress Write FOnCopyTempFileProgress;
    // Overwrite file prompt Event
    property OnConfirmOverwrite: TFXCConfirmOverwriteEvent
      Read FOnConfirmOverwrite Write FOnConfirmOverwrite;
    // Process file prompt Event
    property OnConfirmProcessFile: TFXCConfirmProcessFileEvent
      Read FOnConfirmProcessFile Write FOnConfirmProcessFile;
    // After archive open Event
    property AfterOpen: TNotifyEvent Read FAfterOpen Write FAfterOpen;
    // On password needed Event
    property OnPassword: TFXCOnPasswordEvent Read FOnPassword Write FOnPassword;
    // On file processing failure Event
    property OnProcessFileFailure: TFXCProcessFileFailureEvent
      Read FOnProcessFileFailure Write FOnProcessFileFailure;
    // on request new blank volume Event
    property OnRequestBlankVolume: TFXCOnRequestBlankVolumeEvent
      Read FOnRequestBlankVolume Write FOnRequestBlankVolume;
    // on request first volume Event
    property OnRequestFirstVolume: TFXCOnRequestFirstVolumeEvent
      Read FOnRequestFirstVolume Write FOnRequestFirstVolume;
    // on request last volume Event
    property OnRequestLastVolume: TFXCOnRequestLastVolumeEvent
      Read FOnRequestLastVolume Write FOnRequestLastVolume;
    // on request Middle volume Event
    property OnRequestMiddleVolume: TFXCOnRequestMiddleVolumeEvent
      Read FOnRequestMiddleVolume Write FOnRequestMiddleVolume;
    // on disk full
    property OnDiskFull: TFXCOnDiskFullEvent Read FOnDiskFull Write FOnDiskFull;
    // on ProcessVolumeFailure Event
    //   property OnProcessVolumeFailure: TFXCProcessVolumeFailureEvent read FOnProcessVolumeFailure write FOnProcessVolumeFailure;
    // on store file
    property OnStoreFile: TFXCOnStoreFileEvent Read FOnStoreFile Write FOnStoreFile;
    // on extract file
    property OnExtractFile: TFXCOnExtractFileEvent
      Read FOnExtractFile Write FOnExtractFile;
  end;

  TCryptoTransformMode = (Encryption, Decryption);

  //Base class for all crypto transforms
  TFXCCryptoTransform = class (TObject)
     private
        FTransformMode : TCryptoTransformMode;

     public
        constructor Create;

        //Generate encryption key by given Password
        procedure GenerateKey(const Password : AnsiString); virtual; abstract;

        //check if the Password string is a correct password to decrypt the Item directory item
        function CheckPassword(const Password : AnsiString; Item : TDirItem) : Boolean; virtual; abstract;

        //Returns encryption key
        function GetKey() : PByteArray; virtual; abstract;

        //Returns current encryption password
        function GetPassword() : AnsiString; virtual; abstract;

        //Initializes an object to either decrypt or encrypt file
        procedure Initialize(Mode : TCryptoTransformMode; const Item : TDirItem); virtual;

        //resets inner variables of the crypter object
        procedure Reset(); virtual; abstract;

        //Decrypts buffer
        function DecryptBuffer(const InputBuffer : PAnsiChar; inputOffset : Longint; InputCount : Longint; var OutputBuffer : PAnsiChar; OutputOffset : Longint) : Longint; virtual; abstract;

        //Encrypts buffer
        function EncryptBuffer(const InputBuffer : PAnsiChar; InputOffset : Longint; InputCount : Longint; var OutputBuffer : PAnsiChar; OutputOffset : Longint) : Longint; virtual; abstract;

        //Data block that is written at the beginning of the file data block
        function GetFileStorageStartBlock() : PByte; virtual; abstract;

        //Load block of data that are stored at the beggining of the file data block
        procedure LoadFileStorageStartBlock(Stream : TStream; Offset : Int64); virtual; abstract;

        //Returns the size of the block of data that is stores at the beggining of the file data block
        function GetFileStorageStartBlockSize() : integer; virtual; abstract;

        //Data block that is written at the end of the file data block
        function GetFileStorageEndBlock() : PByte; virtual; abstract;

        //Load block of data that are stored at the end of the file data block
        procedure LoadFileStorageEndBlock(Stream : TStream; Offset : Int64); virtual; abstract;

        //Returns the size of the block of data that is stores at the end of the file data block
        function GetFileStorageEndBlockSize() : integer; virtual; abstract;

        //Check if the just decrypted item is corrupted or not
        function IsDirItemCorrupted(const Item : TDirItem; Crc32 : Cardinal) : Boolean; virtual; abstract;

        //Gets the extra field data for the encryption algorithm
        function GetExtraFieldData() : TFXCExtraFieldDataBlock; virtual; abstract;
   end;

const
     AesExtraFieldHeaderID: Word = $9901;

type

   TAESCryptoTransform  = class (TFXCCryptoTransform)
   private

        //Key generator
        FKeyGenerator : TPBKDF2;

        FHMAC_Context : THMAC_Context;

        FPHash : PHashDesc;

        //AES encrypter
        FCryptor : TCipher_Rijndael;

        //the length of the key = encryption strength
        FKeyLengthBytes : Word;

        //the key for HMACSHA1
        FHashKey : PByteArray;

        //the key for encryption
        FEncryptionKey : PByteArray;

        //2-bytes used to verify password
        FPasswordVerificationValue : PByteArray;

        //salt value
        FSaltValue : PByteArray;

        //authentication code
        FAuthenticationCode : PByteArray;

        //password
        FPassword : AnsiString;

        //1 - AE-1; 2 - AE-2
        FVersion : word;

        FActualCompressionMethod : word;

        FAuthenticationCodeSize : Integer;

   public

        // the strength value indicates the number of bits in the key
        //version: AES encryption version - AE-1, AE-2
        constructor Create(const Strength : integer; Crypt_version : word; ActualCompMethod : word);

        destructor Destroy; override;

        //returns new AESCryptoTransform object based on the information from the AES extra field
        function CreateAESEncryption(const KeyLengthBits : integer; VersionNumber : Word; CompressionMethod : Word) : TAESCryptoTransform;

        procedure GenerateKey(const Password : AnsiString); override;

   private
        procedure UpdateKeys();

   public
        function CheckPassword(const Password : AnsiString; Item : TDirItem) : Boolean; override;

        function IsDirItemCorrupted(const Item : TDirItem; Crc32 : Longword) : Boolean; override;

        function GetKey() : PByteArray; override;

        function GetPassword() : AnsiString; override;

        procedure Initialize(Mode : TCryptoTransformMode; const Item : TDirItem); override;

        procedure Reset(); override;

        function EncryptBuffer(const InputBuffer : PAnsiChar; InputOffset : Longint; InputCount : Longint; var OutputBuffer : PAnsiChar; OutputOffset : Longint) : Longint; override;

        function DecryptBuffer(const InputBuffer : PAnsiChar; InputOffset : Longint; InputCount : Integer; var OutputBuffer : PAnsiChar; OutputOffset : Longint) : Longint; override;

        function GetExtraFieldData() : TFXCExtraFieldDataBlock; override;

        function GetFileStorageStartBlock() : PByte; override;

        procedure LoadFileStorageStartBlock(Stream : TStream; Offset : Int64); override;

        function GetFileStorageStartBlockSize() : Longint; override;

        function GetFileStorageEndBlock() : PByte; override;

        procedure LoadFileStorageEndBlock(Stream : TStream; Offset : Int64); override;

        function GetFileStorageEndBlockSize() : Longint; override;

    end;




{$IFDEF FC_VERSION}
  // TFlexCompress provides multiple-files archive in ZIP format.
  // It supports 5 compression levels:
  //    - eclNone - store files without compression
  //    - eclFastest - ZLIB mode 1, ZIP mode 8 (Deflate)
  //    - eclNormal - ZLIB mode 5, ZIP mode 8 (Deflate)
  //    - eclGood - PPM variant G, model order 5, 20 Mb RAM requires
  //    - eclMax - PPM variant G, model order 16, 50 Mb RAM requires
  TFlexCompress = class(TBaseArchiver)
  protected
    procedure SetCompMethod; override;
    procedure SetCompressionAlgorithm(newAlgorithm: TFXCCompressionAlgorithm);
    procedure SetPassword(const Value : AnsiString); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // compression algorithm
    property CompressionAlgorithm: TFXCCompressionAlgorithm
      Read FCompressionAlgorithm Write SetCompressionAlgorithm;
    // crypto algorithm
    property EncryptionAlgorithm: TFXCCryptoAlgorithm
      Read FFXCCryptoAlgorithm Write FFXCCryptoAlgorithm;
  end;

{$ENDIF}



function IsFloppyDrive(const FileName: TFXCString): boolean;
function IsRemovableDrive(const FileName: TFXCString): boolean;
function DelTreeIfNoFiles(const Directory: TFXCString): boolean;

var
  // 5.06 addition - to force rebuilding central dir - to read corrupted archives better
  ForceBuildCentralDir: Boolean = False;

implementation

uses
{$IFDEF D12H}
 WideStrUtils,
{$ENDIF}
 FXCExcept, FXCStrFunc;

{$IFDEF FXCUNICODE}
const
  UnicodeAdditionalSignature: Cardinal =  $5843554E;
  UnicodeExtraFieldHeaderID: Word = $554E;
{$ENDIF}


type

{$IFDEF FXCUNICODE}
  TUnicodeExtendedInfo = class
  private
    FName:TFXCString;
    FAdditionalSignature: Integer;
  public
    property Name:TFXCString read FName write FName;
    function LoadFromStream(Stream: TStream): Boolean;
    function Load(Data: PByte; Size: Integer): Boolean;
    procedure SaveToStream(Stream: TStream);
  end;
{$ENDIF}

  TNullStream = class(TStream)
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF D6H}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF}
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$IFDEF D6H}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ENDIF}
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


var
  FCSect: TRTLCriticalSection;
  

function aaIncludeTrailingBackslash(const S: TFXCString): TFXCString;
begin
  Result := S;
  if not (Result[Length(Result)] = '\') then
     Result := Result + '\';
end;

function aaExcludeTrailingBackslash(const S: TFXCString): TFXCString;
begin
  Result := S;
  if (Result[Length(Result)] = '\') then
    SetLength(Result, Length(Result) - 1);
end;

{$IFNDEF FXCUNICODE}
function AnsiDirectoryExists(const Name: AnsiString): boolean;
var
  Code: integer;
begin
  Code   := GetFileAttributes(PAnsiChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}

function FXCDirectoryExists(Name: TFXCString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := DirectoryExists(Name);
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideDirectoryExists(Name);
  {$ELSE}
  Result := AnsiDirectoryExists(Name);
  {$ENDIF}
  {$ENDIF}
end;


{$IFNDEF FXCUNICODE}
function AnsiForceDirectories(Dir: string): boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    raise EFXCException.Create(00039);
  Dir := aaExcludeTrailingBackslash(Dir);
  if (Length(Dir) < 3) or FXCDirectoryExists(Dir) or (ExtractFilePath(Dir) = Dir) then
    Exit; // avoid 'xyz:\' problem.
  Result := AnsiForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;
{$ENDIF}


function FXCFileExists(FileName: TFXCString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := FileExists(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideFileExists(FileName)
  {$ELSE}
  Result := FileExists(FileName)
  {$ENDIF}
  {$ENDIF}
end;


function FXCForceDirectories(Dir: TFXCString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := ForceDirectories(Dir)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideForceDirectories(Dir);
  {$ELSE}
  Result := AnsiForceDirectories(Dir);
  {$ENDIF}
  {$ENDIF} 
end;



function FXCLowerCase(S: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := LowerCase(S);
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideLowerCase(S)
  {$ELSE}
  Result := AnsiLowerCase(S)
  {$ENDIF}
  {$ENDIF}
end;

function FXCUpperCase(S: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := UpperCase(S);
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideUpperCase(S)
  {$ELSE}
  Result := AnsiUpperCase(S)
  {$ENDIF}
  {$ENDIF}
end;


function FXCIsStrMatchPattern(StrPtr: TFXCPChar; PatternPtr: TFXCPChar;
  bIgnoreCase: boolean = True): boolean;
begin
  {$IFDEF UNICODE}
  Result := IsStrMatchPattern(StrPtr, PatternPtr, bIgnoreCase)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WIsStrMatchPattern(StrPtr, PatternPtr, bIgnoreCase)
  {$ELSE}
  Result := IsStrMatchPattern(StrPtr, PatternPtr, bIgnoreCase)
  {$ENDIF}
  {$ENDIF}
end;

function FXCStringReplace(S, OldPattern, NewPattern: TFXCString; Flags: TReplaceFlags): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := StringReplace(S, OldPattern, NewPattern, Flags)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := Tnt_WideStringReplace(S, OldPattern, NewPattern, Flags)
  {$ELSE}
  Result := StringReplace(S, OldPattern, NewPattern, Flags)
  {$ENDIF}
  {$ENDIF}
end;
// returns length of string in bytes
function ByteLength(S: TFXCString): Integer;
begin
  {$IFDEF FXCUNICODE}
  Result :=   Length(S) * 2;
  {$ELSE}
  Result := Length(S);
  {$ENDIF}
end;

function FXCDeleteFile(FileName: TFXCPChar): Boolean;
begin
  {$IFDEF UNICODE}
  Result := DeleteFile(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideDeleteFile(FileName);
  {$ELSE}
  Result := DeleteFile(FileName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCRenameFile(OldName, NewName: TFXCPChar): Boolean;
begin
  {$IFDEF UNICODE}
  Result := RenameFile(OldName, NewName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideRenameFile(OldName, NewName)
  {$ELSE}
  Result := RenameFile(OldName, NewName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCCopyFile(lpExistingFileName, lpNewFileName: TFXCPChar; bFailIfExists: BOOL): BOOL;
begin
  {$IFDEF UNICODE}
  Result := CopyFile(lpExistingFileName, lpNewFileName, bFailIfExists)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideCopyFile(lpExistingFileName, lpNewFileName, bFailIfExists)
  {$ELSE}
  Result := CopyFile(lpExistingFileName, lpNewFileName, bFailIfExists)
  {$ENDIF}
  {$ENDIF}
end;

function FXCFindFirst(const Path: TFXCString; Attr: Integer; var F: TFXCSearchRec): Integer;
begin
  {$IFDEF UNICODE}
  Result := SysUtils.FindFirst(Path, Attr, F)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideFindFirst(Path, Attr, F)
  {$ELSE}
  Result := SysUtils.FindFirst(Path, Attr, F)
  {$ENDIF}
  {$ENDIF}
end;

function FXCFindNext(var F: TFXCSearchRec): Integer;
begin
  {$IFDEF UNICODE}
  Result := SysUtils.FindNext(F)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideFindNext(F)
  {$ELSE}
  Result := SysUtils.FindNext(F)
  {$ENDIF}
  {$ENDIF}
end;

procedure FXCFindClose(var F: TFXCSearchRec);
begin
  {$IFDEF UNICODE}
  SysUtils.FindClose(F)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  WideFindClose(F)
  {$ELSE}
  SysUtils.FindClose(F)
  {$ENDIF}
  {$ENDIF}
end;



function FXCGetFileAttr(const FileName: TFXCString): Cardinal;
var temp : Cardinal;
begin
  {$IFDEF UNICODE}
  temp := FileGetAttr(FileName);
  {$ELSE}
  {$IFDEF FXCUNICODE}
  temp := WideFileGetAttr(FileName);
  {$ELSE}
  temp := FileGetAttr(FileName);
  {$ENDIF}
  {$ENDIF}
  if (temp = Cardinal(-1)) and (FXCFileExists(FileName)) then
      temp := faSysFile;
  Result := temp;
end;

function FXCSetFileAttr(const FileName: TFXCString; Attr: Cardinal): Boolean;
begin
  {$IFDEF UNICODE}
  Result := FileSetAttr(FileName, Attr) = 0;
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideFileSetAttr(FileName, Attr)
  {$ELSE}
  Result := FileSetAttr(FileName, Attr) = 0;
  {$ENDIF}
  {$ENDIF}
end;

function FXCExtractRelativePath(const BaseName, DestName: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := ExtractRelativePath(BaseName, DestName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideExtractRelativePath(BaseName, DestName)
  {$ELSE}
  Result := ExtractRelativePath(BaseName, DestName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCExtractFileName(FileName: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := ExtractFileName(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideExtractFileName(FileName)
  {$ELSE}
  Result := ExtractFileName(FileName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCExpandFileName(FileName: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := ExpandFileName(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideExpandFileName(FileName)
  {$ELSE}
  Result := ExpandFileName(FileName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCExtractFilePath(FileName: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := ExtractFilePath(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideExtractFilePath(FileName)
  {$ELSE}
  Result := ExtractFilePath(FileName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCExtractFileExt(FileName: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := ExtractFileExt(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideExtractFileExt(FileName)
  {$ELSE}
  Result := ExtractFileExt(FileName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCExtractFileDir(FileName: TFXCString): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := ExtractFileDir(FileName)
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideExtractFileDir(FileName)
  {$ELSE}
  Result := ExtractFileDir(FileName)
  {$ENDIF}
  {$ENDIF}
end;

function FXCGetCurrentDir(): TFXCString;
begin
  {$IFDEF UNICODE}
  Result := GetCurrentDir()
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideGetCurrentDir()
  {$ELSE}
  Result := GetCurrentDir()
  {$ENDIF}
  {$ENDIF}
end;

function FXCFileAge(FileName: TFXCString): Integer;
begin
  {$IFDEF UNICODE}
  Result := FileAge(FileName);
  {$ELSE}
  {$IFDEF FXCUNICODE}
  Result := WideFileAge(FileName);
  {$ELSE}
  Result := FileAge(FileName);
  {$ENDIF}
  {$ENDIF}
end;

{$IFNDEF D12H}
type
  RawByteString = type AnsiString;
  TEncodeType = (etUSASCII, etUTF8, etANSI);

{$IFNDEF D6H}
// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        wc := wc and $3F;
        if i > SourceBytes then Exit;           // incomplete multibyte char
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then  Exit;     // malformed trail byte or out of range char
          if i > SourceBytes then Exit;         // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
	if count >= MaxDestChars then count := MaxDestChars-1;
	Dest[count] := #0;
  end
  else
  begin
	while (i <= SourceBytes) do
	begin
	  c := Byte(Source[i]);
	  Inc(i);
	  if (c and $80) <> 0 then
	  begin
		if (c and $F0) = $F0 then Exit;  // too many bytes for UCS2
		if (c and $40) = 0 then Exit;    // malformed lead byte
		if i > SourceBytes then Exit;         // incomplete multibyte char

		if (Byte(Source[i]) and $C0) <> $80 then Exit;  // malformed trail byte
		Inc(i);
		if i > SourceBytes then Exit;         // incomplete multibyte char
		if ((c and $20) <> 0) and ((Byte(Source[i]) and $C0) <> $80) then Exit; // malformed trail byte
		Inc(i);
	  end;
	  Inc(count);
	end;
  end;
  Result := count+1;
end;

function Utf8Decode(const S: String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;
{$ENDIF}

function UTF8EncodeToShortString(const WS: WideString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(WS), Length(WS)) - 1));
end;

//----------------------------------------------------------
// Detect valid UTF8 sequence.
function DetectUTF8Encoding(const s: RawByteString): TEncodeType;
var
  c : Byte;
  P, EndPtr: PByte;
begin
  Result := etUSASCII;
  P := PByte(PAnsiChar(s));
  EndPtr := PByte(PAnsiChar(P) + Length(s));

  // skip leading US-ASCII part.
  while PAnsiChar(P) < PAnsiChar(EndPtr) do
  begin
    if P^ >= $80 then break;
    inc(P);
  end;

  // If all character is US-ASCII, done.
  if P = EndPtr then exit;

  while PAnsiChar(P) < PAnsiChar(EndPtr) do
  begin
    c := p^;
    case c of
      $00..$7F:
        inc(P);

      $C2..$DF:
        if (PAnsiChar(P)+1 < PAnsiChar(EndPtr))
            and ((PByte(PAnsiChar(P)+1))^ in [$80..$BF]) then
          Inc(P, 2)
        else
          break;

      $E0:
        if (PAnsiChar(P)+2 < PAnsiChar(EndPtr))
            and (PByte(PAnsiChar(P)+1)^ in [$A0..$BF])
            and (PByte(PAnsiChar(P)+2)^ in [$80..$BF]) then
          Inc(P, 3)
        else
          break;

      $E1..$EF:
        if (PAnsiChar(P)+2 < PAnsiChar(EndPtr))
            and (PByte(PAnsiChar(P)+1)^ in [$80..$BF])
            and (PByte(PAnsiChar(P)+2)^ in [$80..$BF]) then
          Inc(P, 3)
        else
          break;

      $F0:
        if (PAnsiChar(P)+3 < PAnsiChar(EndPtr))
            and (PByte(PAnsiChar(P)+1)^ in [$90..$BF])
            and (PByte(PAnsiChar(P)+2)^ in [$80..$BF])
            and (PByte(PAnsiChar(P)+3)^ in [$80..$BF]) then
          Inc(P, 4)
        else
          break;

      $F1..$F3:
        if (PAnsiChar(P)+3 < PAnsiChar(EndPtr))
            and (PByte(PAnsiChar(P)+1)^ in [$80..$BF])
            and (PByte(PAnsiChar(P)+2)^ in [$80..$BF])
            and (PByte(PAnsiChar(P)+3)^ in [$80..$BF]) then
          Inc(P, 4)
        else
          break;

      $F4:
        if (PAnsiChar(P)+3 < PAnsiChar(EndPtr))
            and (PByte(PAnsiChar(P)+1)^ in [$80..$8F])
            and (PByte(PAnsiChar(P)+2)^ in [$80..$BF])
            and (PByte(PAnsiChar(P)+3)^ in [$80..$BF]) then
          Inc(P, 4)
        else
          break;
    else
      break;
    end;
  end;

  if P = EndPtr then Result := etUTF8
  else Result := etANSI;
end;

// if string contain real UTF8 character, return true.
function IsUTF8String(const s: RawByteString): Boolean;
begin
  result := DetectUTF8Encoding(s) = etUTF8;
end;
{$ENDIF}

function PatchFileNameForWrite(srcFileName: WideString; var dstFileName: AnsiString): boolean;
begin
  Result := (WideString(AnsiString(srcFileName)) <> srcFileName);
  if Result then
    dstFileName := UTF8EncodeToShortString(srcFileName)
  else
    dstFileName := AnsiString(srcFileName);
end;


function PatchFileNameForRead(srcFileName: AnsiString; var dstFileName: WideString): boolean;
begin
  Result := IsUTF8String(srcFileName);
  if Result then
    dstFileName := UTF8Decode(srcFileName)
  else
    dstFileName := WideString(srcFileName);
end;


function GetFileSizeByName(FileName: TFXCString): int64;
var
  F: TFXCSearchRec;
begin
  if FXCDirectoryExists(FileName) then
    Result := 0
  else
  begin
    FXCFindFirst(FileName, faAnyFile + $80 + $800 + $2000, F);
    Int64Rec(Result).Lo := F.FindData.nFileSizeLow;
    Int64Rec(Result).Hi := F.FindData.nFileSizeHigh;
    FXCFindClose(F);
  end;
end;


function GetItemSize(DirItem: TDirItem): int64;
begin
  if Assigned(DirItem.Stream) then
    Result := DirItem.Stream.Size
  else
  begin
    Result := GetFileSizeByName(DirItem.SrcFileName);
  end;
end;





{ TFXCStringHash }

procedure TFXCStringHash.Add(const Key: TFXCString; Value: integer);
var
  Hash:   integer;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key   := Key;
  Bucket^.Value := Value;
  Bucket^.Next  := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TFXCStringHash.Clear;
var
  I:    integer;
  P, N: PHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TFXCStringHash.Create(aSize: cardinal);
begin
  inherited Create;
  SetLength(Buckets, aSize);
  Size := aSize;
end;

destructor TFXCStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFXCStringHash.Find(const Key: TFXCString): PPHashItem;
var
  Hash: integer;
begin
  Hash   := HashOf(Key) mod cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TFXCStringHash.HashOf(const Key: TFXCString): cardinal;
var
  I: integer;
  S: TFXCString;
begin
  S := Key;
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(S[I]);
end;

function TFXCStringHash.Modify(const Key: TFXCString; Value: integer): boolean;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result   := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TFXCStringHash.Remove(const Key: TFXCString);
var
  P:    PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key);
  P    := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TFXCStringHash.ValueOf(const Key: TFXCString): integer;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;


{ TFXCHashedStringList }

constructor TFXCHashedStringList.Create;
begin
  inherited Create;
  FValueHashValid := True;
end;

destructor TFXCHashedStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited Destroy;
end;

function TFXCHashedStringList.Add(const S: TFXCString): integer;
var
  I: integer;
begin
  I := inherited Add(S);
  if (FValueHash <> nil) and FValueHashValid then
  begin
{$IFDEF D6H}
      if not CaseSensitive then
        FValueHash.Add(FXCLowerCase(S), I)
      else
{$ENDIF}
    FValueHash.Add(S, I);
    // recreate hash table?
    if (Cardinal(Count) > FValueHash.Size * 5) then
    begin
      FValueHash.Free;
      FValueHash      := nil;
      FValueHashValid := False;
    end;
  end;
  Result := I;
end;


procedure TFXCHashedStringList.Update(Index: integer; const NewValue: TFXCString);
begin
  FValueHash.Remove(Self[Index]);
  Self[Index] := NewValue;
  FValueHash.Add(FXCLowerCase(NewValue), Index);
end;


procedure TFXCHashedStringList.Delete(Index: integer);
begin
  inherited Delete(Index);
  FValueHashValid := False;
  FNameHashValid  := False;
end;

procedure TFXCHashedStringList.Clear;
begin
  inherited Clear;
  if (FValueHash <> nil) then
    FValueHash.Clear;
end;

function TFXCHashedStringList.IndexOf(const S: TFXCString): integer;
begin
  UpdateValueHash;
{$IFDEF D6H}
  if not CaseSensitive then
    Result :=  FValueHash.ValueOf(FXCLowerCase(S))
  else
{$ENDIF}
  Result := FValueHash.ValueOf(S);
end;

{$IFDEF D6H}
function TFXCHashedStringList.IndexOfName(const Name: TFXCString): Integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(FXCLowerCase(Name))
  else
    Result := FNameHash.ValueOf(Name);
end;
{$ENDIF}

procedure TFXCHashedStringList.UpdateNameHash;
var
  I:   integer;
  P:   integer;
  Key: TFXCString;
begin
  if FNameHashValid then
    Exit;

  if FNameHash = nil then
    FNameHash := TFXCStringHash.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P   := Pos('=', Key);
    if P <> 0 then
    begin
{$IFDEF D6H}
      if not CaseSensitive then
        Key := FXCLowerCase(Copy(Key, 1, P - 1))
      else
{$ENDIF}
      Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure TFXCHashedStringList.UpdateValueHash;
var
  I: integer;
begin
  if FValueHash = nil then
    FValueHash := TFXCStringHash.Create(Max(Count, 1000));

  if FValueHashValid then
    Exit;

  FValueHash.Clear;
  for I := 0 to Count - 1 do
{$IFDEF D6H}
    if not CaseSensitive then
      FValueHash.Add(FXCLowerCase(Self[I]), I)
    else
{$ENDIF}
    FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;



function IsFloppyDrive(const FileName: TFXCString): boolean;
var
  FileDrive: TFXCString;
begin
  FileDrive := FXCUpperCase(ExtractFileDrive(FileName));
  Result    := (FileDrive = 'A:') or (FileDrive = 'B:');
end;

function IsRemovableDrive(const FileName: TFXCString): boolean;
var
  FileDrive: string;
begin
  FileDrive := ExtractFileDrive(FileName);
  if (IsFloppyDrive(FileName)) then
    Result := True
  else
    Result := (GetDriveType(PChar(FileDrive)) = DRIVE_REMOVABLE);
end;

function DelTreeIfNoFiles(const Directory: TFXCString): boolean;
var
  SearchRec: TFXCSearchRec;
begin
  if FXCFindFirst(Directory + '*', faAnyFile and not faVolumeID,
    SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          if (SearchRec.Attr and faDirectory <> 0) then
            if (not DelTreeIfNoFiles(Directory + SearchRec.Name + '\')) then
            begin
              Result := False;
              exit;
            end;
      until (FXCFindNext(SearchRec) <> 0);
    finally
      FXCFindClose(SearchRec);
    end;
  end;
  Result := SysUtils.RemoveDir(Directory);
  if (not Result) then
    Result := not FXCDirectoryExists(Directory);
end;



//------------------------------------------------------------------------------
// determines best block size for compression level
//------------------------------------------------------------------------------
function InternalGetBlockSize(CompressionMode: byte): integer;
begin
  Result := DefaultMaxBlockSize;
  if (CompressionMode = 0) then
    Exit;
  if (CompressionMode <= 3) then
    Result := BlockSizeForFastest
  else
  if (CompressionMode <= 6) then
    Result := BlockSizeForNormal
  else
  if (CompressionMode <= 9) then
    Result := BlockSizeForMax;
end;


//------------------------------------------------------------------------------
// returns compression level for this mode
//------------------------------------------------------------------------------
function InternalGetCompressionLevel(CompressionMode: byte): TFXCCompressionLevel;
begin
  Result := clNone;

  if (CompressionMode <= 0) then
    Result := clNone
  else
  if (CompressionMode <= 3) then
    Result := clFastest
  else
  if (CompressionMode <= 6) then
    Result := clNormal
  else
  if (CompressionMode <= 9) then
    Result := clMax;
end;


//------------------------------------------------------------------------------
// returns compression mode for this level
//------------------------------------------------------------------------------
function InternalGetCompressionMode(CompressionLevel: TFXCCompressionLevel): byte;
begin
  case CompressionLevel of
    clFastest: Result := 1;
    clNormal: Result  := 6;
    clMax: Result     := 9;
    else
      Result := 0;
  end;
end;


//------------------------------------------------------------------------------
// compresses buffer
// returns true if successful
// outBuf - pointer to compressed data
// outSize - size of compressed data
//------------------------------------------------------------------------------
function FXCInternalCompressBuffer(CompressionAlg: TFXCCompressionAlgorithm;
  CompressionMode: byte; inBuf: PByte; inSize: integer; out outBuf: PByte;
  out outSize: integer): boolean;
begin
  Result := False;
  if inSize = 0 then
    Exit;
  Result  := True;
  outSize := 0;
  case CompressionAlg of
{$IFDEF ZLIB}
    caZLIB:
    begin
      try
        ZLIBCompressBuf(inBuf, inSize, Pointer(outbuf), integer(outSize),
          CompressionMode);
      except
        Result := False;
      end;
      if (outSize <= 0) then
        Result := False;
    end
{$ENDIF}
{$IFDEF BZIP}
      ;
    caBZIP:
    begin
      try
        bzCompressBuf(inBuf, inSize, Pointer(outbuf), integer(outSize), CompressionMode)
      except
        Result := False;
      end;
      if (outSize <= 0) then
        Result := False;
    end
{$ENDIF}
{$IFDEF PPMD}
      ;
    caPPM:
    begin
      try
        PPMCompressBuf(inBuf, inSize, Pointer(outbuf), integer(outSize), CompressionMode);
      except
        Result := False;
      end;
      if (outSize <= 0) then
        Result := False;
    end
{$ENDIF}
      ;
    else
      Result := False;
  end;
end; // FXCInternalCompressBuffer;


//------------------------------------------------------------------------------
// decompresses buffer
// outsize must be set to uncompressed size
// returns true if successful
// outBuf - pointer to compressed data
// outSize - size of compressed data
//------------------------------------------------------------------------------
function FXCInternalDecompressBuffer(CompressionAlg: TFXCCompressionAlgorithm;
  inBuf: PByte; inSize: integer; out outBuf: PByte; out outSize: integer): boolean;
begin
  Result := False;
  if inSize = 0 then
    Exit;
  Result := True;
  case CompressionAlg of
{$IFDEF ZLIB}
    caZLIB:
    begin
      try
        ZLIBDecompressBuf(inBuf, inSize, OutSize, Pointer(outbuf), integer(outSize));
      except
        Result := False;
      end;
      if (outSize <= 0) then
        Result := False;
    end
{$ENDIF}
{$IFDEF BZIP}
      ;
    caBZIP:
    begin
      try
        bzDecompressBuf(inBuf, inSize, OutSize, Pointer(outbuf), integer(outSize));
      except
        Result := False;
      end;
      if (outSize <= 0) then
        Result := False;
    end
{$ENDIF}
{$IFDEF PPMD}
      ;
    caPPM:
    begin
      try
        PPMDecompressBuf(inBuf, inSize, Pointer(outbuf), integer(outSize));
      except
        Result := False;
      end;
      if (outSize <= 0) then
        Result := False;
    end
{$ENDIF}
      ;
    else
      Result := False;
  end; //case compression algorithm
end; // FXCInternalDecompressBuffer;

{$IFDEF FC_VERSION}

// encrypts specified buffer
function FXCInternalEncryptBuffer(CryptoAlg: TFXCCryptoAlgorithm;
  inBuf: PByte; Size: integer; Password: AnsiString; IVector: PByteArray = nil): boolean;
 {$IFDEF ENCRYPTION}
var
  crypto: TCipher;
  procedure InitKey;
  var
    iv: array [0..FXC_MAX_SAVED_IVECTOR_SIZE-1] of byte;
  begin
    if (IVector <> nil) then
      begin
        FillChar(iv, sizeof(iv), $00);
        Move(IVector^, iv, min(FXC_MAX_SAVED_IVECTOR_SIZE, FXC_MAX_IVECTOR_SIZE));
        crypto.InitKey(Password, @iv);
      end
    else
      crypto.InitKey(Password, crypto.Vector);
  end;
 {$ENDIF}
begin
  Result := False;
 {$IFDEF ENCRYPTION}
  try
    case CryptoAlg of
      caRijndael_128:
      begin
        crypto := TCipher_Rijndael.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caRijndael_256:
      begin
        crypto := TCipher_Rijndael.Create(Password, nil);
        crypto.HashClass := THash_RipeMD256;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caSquare:
      begin
        crypto := TCipher_Square.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caDES_Single:
      begin
        crypto := TCipher_1DES.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caDES_Triple:
      begin
        crypto := TCipher_3TDES.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caBlowfish:
      begin
        crypto := TCipher_Blowfish.Create(Password, nil);
        crypto.HashClass := THash_RipeMD256;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caTwofish_128:
      begin
        crypto := TCipher_Twofish.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
      caTwofish_256:
      begin
        crypto := TCipher_Twofish.Create(Password, nil);
        crypto.HashClass := THash_RipeMD256;
        InitKey;
        crypto.EncodeBuffer(inBuf^, inBuf^, Size);
        crypto.Free;
      end;
    end; // case
    Result := True;
  except
    Result := False; // encryption error
  end;
 {$ENDIF}
end; // FXCInternalEncryptBuffer

Function IsFilenameValid(const fn : TFXCString):boolean;
Const
  NonValidFileChars1 : set of byte = [0..32,34,42,46,58,60,62,63,92,124,128,129,141..144,157,158];
  NonValidFileChars2 : set of byte = [1..31,34,42,60,62,63,92,124,128,129,141..144,157,158];
  SpecialCharacters: set of AnsiChar = ['[',']','|','+'];
var i : integer;
begin
  result := true;
  for i := 1 to length(fn) do
    begin
     if i = 1 then
        result := not (ord(fn[i]) in NonValidFileChars1)
     else
        result := not (ord(fn[i]) in NonValidFileChars2);
     if not result then exit;
    end;
 // no invalid characters found, but it is still possible that new encryption algorithm was used
 // to encrypt file name
// if FXCExtractFileExt(fn) = '' then Result := False;
end;

// decrypts specified buffer
function FXCInternalDecryptBuffer(CryptoAlg: TFXCCryptoAlgorithm; inBuf: PByte; outBuf: PByte; Size: integer;
Password: AnsiString; UseOldRijndael : boolean; IVector: PByteArray = nil): boolean;
 {$IFDEF ENCRYPTION}
var
  crypto: TCipher;
  procedure InitKey;
  var
    iv: array [0..FXC_MAX_SAVED_IVECTOR_SIZE-1] of byte;
  begin
    if (IVector <> nil) then
      begin
        FillChar(iv, sizeof(iv), $00);
        Move(IVector^, iv, min(FXC_MAX_SAVED_IVECTOR_SIZE, FXC_MAX_IVECTOR_SIZE));
        crypto.InitKey(Password, @iv);
      end
    else
      crypto.InitKey(Password, crypto.Vector);
  end;
 {$ENDIF}
begin
  Result := False;
 {$IFDEF ENCRYPTION}
  try
    case CryptoAlg of
      caRijndael_128:
      begin
        if UseOldRijndael then
          crypto := TCipher_RijndaelFXC.Create(Password, nil)
        else
          crypto := TCipher_Rijndael.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caRijndael_256:
      begin
        if UseOldRijndael then
          crypto := TCipher_RijndaelFXC.Create('', nil)
        else
          crypto := TCipher_Rijndael.Create('', nil);
        crypto.HashClass := THash_RipeMD256;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caSquare:
      begin
        crypto := TCipher_Square.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caDES_Single:
      begin
        crypto := TCipher_1DES.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caDES_Triple:
      begin
        crypto := TCipher_3TDES.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caBlowfish:
      begin
        crypto := TCipher_Blowfish.Create(Password, nil);
        crypto.HashClass := THash_RipeMD256;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caTwofish_128:
      begin
        crypto := TCipher_Twofish.Create(Password, nil);
        crypto.HashClass := THash_RipeMD128;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
      caTwofish_256:
      begin
        crypto := TCipher_Twofish.Create(Password, nil);
        crypto.HashClass := THash_RipeMD256;
        InitKey;
        crypto.DecodeBuffer(inBuf^, outBuf^, Size);
        crypto.Free;
      end;
    end; // case
    Result := True;
  except
    Result := False; // decryption error
  end;
 {$ENDIF}
end; // FXCInternalDecryptBuffer

{$ENDIF}

procedure memset(P: Pointer; B: byte; Count: integer); cdecl;
begin
  FillChar(P^, Count, B);
end;

procedure memcpy(dest, Source: Pointer; Count: integer); cdecl;
begin
  Move(Source^, dest^, Count);
end;


function aa_malloc(Count: integer): PByte; cdecl;
begin
  Result := AllocMem(Count);
end;

procedure aa_free(buffer: PByte); cdecl;
begin
  FreeMem(buffer);
end;

////////////////////////////////////////////////////////////////////////////////

//   ZIP support

////////////////////////////////////////////////////////////////////////////////


function GetZlibStreamHeader(CompMode: integer): word;
begin
  Result := 0;
  case CompMode of
    0, 7, 8, 9: Result := $DA78;
    1, 2: Result := $0178;
    3, 4: Result := $5E78;
    5, 6: Result := $9C78;
  end;
end;

{function CRC32(CRC: longword; Data: Pointer; DataSize: longword): longword; assembler;
asm
  and    EDX,EDX
  JZ     @Exit
  and    ECX,ECX
  JLE    @Exit
  PUSH   EBX
  PUSH   EDI
  xor    EBX,EBX
  LEA    EDI,CS:[OFFSET @CRC32]
  @Start:  MOV    BL,AL
  shr    EAX,8
  xor    BL,[EDX]
  xor    EAX,[EDI + EBX * 4]
  INC    EDX
  DEC    ECX
  JNZ    @Start
  POP    EDI
  POP    EBX
  @Exit:   RET
  DB 0, 0, 0, 0, 0 // Align Table
  @CRC32:  DD 000000000h, 077073096h, 0EE0E612Ch, 0990951BAh
  DD 0076DC419h, 0706AF48Fh, 0E963A535h, 09E6495A3h
  DD 00EDB8832h, 079DCB8A4h, 0E0D5E91Eh, 097D2D988h
  DD 009B64C2Bh, 07EB17CBDh, 0E7B82D07h, 090BF1D91h
  DD 01DB71064h, 06AB020F2h, 0F3B97148h, 084BE41DEh
  DD 01ADAD47Dh, 06DDDE4EBh, 0F4D4B551h, 083D385C7h
  DD 0136C9856h, 0646BA8C0h, 0FD62F97Ah, 08A65C9ECh
  DD 014015C4Fh, 063066CD9h, 0FA0F3D63h, 08D080DF5h
  DD 03B6E20C8h, 04C69105Eh, 0D56041E4h, 0A2677172h
  DD 03C03E4D1h, 04B04D447h, 0D20D85FDh, 0A50AB56Bh
  DD 035B5A8FAh, 042B2986Ch, 0DBBBC9D6h, 0ACBCF940h
  DD 032D86CE3h, 045DF5C75h, 0DCD60DCFh, 0ABD13D59h
  DD 026D930ACh, 051DE003Ah, 0C8D75180h, 0BFD06116h
  DD 021B4F4B5h, 056B3C423h, 0CFBA9599h, 0B8BDA50Fh
  DD 02802B89Eh, 05F058808h, 0C60CD9B2h, 0B10BE924h
  DD 02F6F7C87h, 058684C11h, 0C1611DABh, 0B6662D3Dh
  DD 076DC4190h, 001DB7106h, 098D220BCh, 0EFD5102Ah
  DD 071B18589h, 006B6B51Fh, 09FBFE4A5h, 0E8B8D433h
  DD 07807C9A2h, 00F00F934h, 09609A88Eh, 0E10E9818h
  DD 07F6A0DBBh, 0086D3D2Dh, 091646C97h, 0E6635C01h
  DD 06B6B51F4h, 01C6C6162h, 0856530D8h, 0F262004Eh
  DD 06C0695EDh, 01B01A57Bh, 08208F4C1h, 0F50FC457h
  DD 065B0D9C6h, 012B7E950h, 08BBEB8EAh, 0FCB9887Ch
  DD 062DD1DDFh, 015DA2D49h, 08CD37CF3h, 0FBD44C65h
  DD 04DB26158h, 03AB551CEh, 0A3BC0074h, 0D4BB30E2h
  DD 04ADFA541h, 03DD895D7h, 0A4D1C46Dh, 0D3D6F4FBh
  DD 04369E96Ah, 0346ED9FCh, 0AD678846h, 0DA60B8D0h
  DD 044042D73h, 033031DE5h, 0AA0A4C5Fh, 0DD0D7CC9h
  DD 05005713Ch, 0270241AAh, 0BE0B1010h, 0C90C2086h
  DD 05768B525h, 0206F85B3h, 0B966D409h, 0CE61E49Fh
  DD 05EDEF90Eh, 029D9C998h, 0B0D09822h, 0C7D7A8B4h
  DD 059B33D17h, 02EB40D81h, 0B7BD5C3Bh, 0C0BA6CADh
  DD 0EDB88320h, 09ABFB3B6h, 003B6E20Ch, 074B1D29Ah
  DD 0EAD54739h, 09DD277AFh, 004DB2615h, 073DC1683h
  DD 0E3630B12h, 094643B84h, 00D6D6A3Eh, 07A6A5AA8h
  DD 0E40ECF0Bh, 09309FF9Dh, 00A00AE27h, 07D079EB1h
  DD 0F00F9344h, 08708A3D2h, 01E01F268h, 06906C2FEh
  DD 0F762575Dh, 0806567CBh, 0196C3671h, 06E6B06E7h
  DD 0FED41B76h, 089D32BE0h, 010DA7A5Ah, 067DD4ACCh
  DD 0F9B9DF6Fh, 08EBEEFF9h, 017B7BE43h, 060B08ED5h
  DD 0D6D6A3E8h, 0A1D1937Eh, 038D8C2C4h, 04FDFF252h
  DD 0D1BB67F1h, 0A6BC5767h, 03FB506DDh, 048B2364Bh
  DD 0D80D2BDAh, 0AF0A1B4Ch, 036034AF6h, 041047A60h
  DD 0DF60EFC3h, 0A867DF55h, 0316E8EEFh, 04669BE79h
  DD 0CB61B38Ch, 0BC66831Ah, 0256FD2A0h, 05268E236h
  DD 0CC0C7795h, 0BB0B4703h, 0220216B9h, 05505262Fh
  DD 0C5BA3BBEh, 0B2BD0B28h, 02BB45A92h, 05CB36A04h
  DD 0C2D7FFA7h, 0B5D0CF31h, 02CD99E8Bh, 05BDEAE1Dh
  DD 09B64C2B0h, 0EC63F226h, 0756AA39Ch, 0026D930Ah
  DD 09C0906A9h, 0EB0E363Fh, 072076785h, 005005713h
  DD 095BF4A82h, 0E2B87A14h, 07BB12BAEh, 00CB61B38h
  DD 092D28E9Bh, 0E5D5BE0Dh, 07CDCEFB7h, 00BDBDF21h
  DD 086D3D2D4h, 0F1D4E242h, 068DDB3F8h, 01FDA836Eh
  DD 081BE16CDh, 0F6B9265Bh, 06FB077E1h, 018B74777h
  DD 088085AE6h, 0FF0F6A70h, 066063BCAh, 011010B5Ch
  DD 08F659EFFh, 0F862AE69h, 0616BFFD3h, 0166CCF45h
  DD 0A00AE278h, 0D70DD2EEh, 04E048354h, 03903B3C2h
  DD 0A7672661h, 0D06016F7h, 04969474Dh, 03E6E77DBh
  DD 0AED16A4Ah, 0D9D65ADCh, 040DF0B66h, 037D83BF0h
  DD 0A9BCAE53h, 0DEBB9EC5h, 047B2CF7Fh, 030B5FFE9h
  DD 0BDBDF21Ch, 0CABAC28Ah, 053B39330h, 024B4A3A6h
  DD 0BAD03605h, 0CDD70693h, 054DE5729h, 023D967BFh
  DD 0B3667A2Eh, 0C4614AB8h, 05D681B02h, 02A6F2B94h
  DD 0B40BBE37h, 0C30C8EA1h, 05A05DF1Bh, 02D02EF8Dh
  DD 074726F50h, 0736E6F69h, 0706F4320h, 067697279h
  DD 028207468h, 031202963h, 020393939h, 048207962h
  DD 06E656761h, 064655220h, 06E616D64h, 06FBBA36Eh
end;}
function CRC32(CRC: longword; Data: Pointer; DataSize: longword): longword;
const
  crc_table : array[0..256-1] of longword = (
 $0,$77073096,$EE0E612C,$990951BA,
 $76DC419,$706AF48F,$E963A535,$9E6495A3,
 $EDB8832,$79DCB8A4,$E0D5E91E,$97D2D988,
 $9B64C2B,$7EB17CBD,$E7B82D07,$90BF1D91,
 $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,
 $1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
 $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,
 $14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
 $3B6E20C8,$4C69105E,$D56041E4,$A2677172,
 $3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
 $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,
 $32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
 $26D930AC,$51DE003A,$C8D75180,$BFD06116,
 $21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
 $2802B89E,$5F058808,$C60CD9B2,$B10BE924,
 $2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
 $76DC4190,$1DB7106,$98D220BC,$EFD5102A,
 $71B18589,$6B6B51F,$9FBFE4A5,$E8B8D433,
 $7807C9A2,$F00F934,$9609A88E,$E10E9818,
 $7F6A0DBB,$86D3D2D,$91646C97,$E6635C01,
 $6B6B51F4,$1C6C6162,$856530D8,$F262004E,
 $6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
 $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,
 $62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
 $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,
 $4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
 $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,
 $44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
 $5005713C,$270241AA,$BE0B1010,$C90C2086,
 $5768B525,$206F85B3,$B966D409,$CE61E49F,
 $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,
 $59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
 $EDB88320,$9ABFB3B6,$3B6E20C,$74B1D29A,
 $EAD54739,$9DD277AF,$4DB2615,$73DC1683,
 $E3630B12,$94643B84,$D6D6A3E,$7A6A5AA8,
 $E40ECF0B,$9309FF9D,$A00AE27,$7D079EB1,
 $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,
 $F762575D,$806567CB,$196C3671,$6E6B06E7,
 $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,
 $F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
 $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,
 $D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
 $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,
 $DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
 $CB61B38C,$BC66831A,$256FD2A0,$5268E236,
 $CC0C7795,$BB0B4703,$220216B9,$5505262F,
 $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,
 $C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
 $9B64C2B0,$EC63F226,$756AA39C,$26D930A,
 $9C0906A9,$EB0E363F,$72076785,$5005713,
 $95BF4A82,$E2B87A14,$7BB12BAE,$CB61B38,
 $92D28E9B,$E5D5BE0D,$7CDCEFB7,$BDBDF21,
 $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,
 $81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
 $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,
 $8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
 $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,
 $A7672661,$D06016F7,$4969474D,$3E6E77DB,
 $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,
 $A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
 $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,
 $BAD03605,$CDD70693,$54DE5729,$23D967BF,
 $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,
 $B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D);

var
 i: Integer;
begin
 Result:=CRC;
 for i:=0 to DataSize-1 do
 begin
   Result:=((Result shr 8) and $00FFFFFF) xor
           crc_table[(Result xor pByteArray(Data)[i]) and $000000FF];
 end;
end;

procedure UpdateCRC32(buf: PByte; Count: integer; var crc_32: longword);
begin
  crc_32 := CRC32(crc_32, buf, Count); // !!!
end; //UpdateCRC32

function UpdCRC(Octet: byte; Crc32: longword): longword;
var
  l: longword;
  w: array[1..4] of byte absolute l;
begin
  Result := CRC32_TABLE[byte(Crc32 xor longword(byte(Octet)))] xor
    ((Crc32 shr 8) and $00FFFFFF);
end; //UpdCRC


//------------------------------------------------------------------------------
// update zip keys
//------------------------------------------------------------------------------
procedure ZipUpdateKeys(symbol: byte; var Key: TZipKey);
begin
  Key[0] := UpdCRC(symbol, Key[0]);
  Key[1] := Key[1] + (Key[0] and $000000FF);
  Key[1] := longword(int64(Key[1]) * int64(134775813) + int64(1));
  Key[2] := UpdCrc(byte(Key[1] shr 24), Key[2]);
end;


//------------------------------------------------------------------------------
// decrypt byte
//------------------------------------------------------------------------------
function ZipDecryptByte(Key: TZipKey): byte;
var
  temp: int64; // bug-fix, was integer
begin
  temp   := Key[2] or 2;
  //temp   := word(temp);  // bug fix for overflow error, thanks to Danny Braun
  //temp   := word(int64(temp) * int64((temp xor 1)));
  //Result := byte(word(temp shr 8));
  temp   := word(int64(word(temp)) * int64((word(temp) xor 1)));
  Result := byte(word(word(temp) shr 8));

end;


//------------------------------------------------------------------------------
// encrypt byte
//------------------------------------------------------------------------------
function ZipEncryptByte(var c: byte; var Key: TZipKey): byte;
var
  t: integer;
begin
  t := ZipDecryptByte(Key);
  ZipUpdateKeys(c, Key);
  Result := t xor c;
end;


//------------------------------------------------------------------------------
// decrypt buffer
//------------------------------------------------------------------------------
procedure ZipDecryptBuffer(var Buffer: PByte; size: integer; var Key: TZipKey);
var
  temp, c: byte;
  i: integer;
begin
  for i := 0 to size - 1 do
  begin
    c    := pByte(PAnsiChar(Buffer) + i)^;
    temp := c xor ZipDecryptByte(Key);
    ZipUpdateKeys(temp, Key);
    pByte(PAnsiChar(Buffer) + i)^ := temp;
  end;
end;


//------------------------------------------------------------------------------
// encrypt buffer
//------------------------------------------------------------------------------
procedure ZipEncryptBuffer(size, offset: integer; var Buffer: PByte; var Key: TZipKey);
var
  i: integer;
  c: byte;
begin
  for i := offset to offset + size - 1 do
  begin
    c := pByte(PAnsiChar(Buffer) + i)^;
    pByte(PAnsiChar(Buffer) + i)^ := ZipEncryptByte(c, Key);
  end;
end;



//------------------------------------------------------------------------------
// init zip keys
//------------------------------------------------------------------------------
procedure ZipKeyInit(const Password: AnsiString; var Key: TZipKey;
  var KeyHeader: TZipKeyHeader);
var
  i: integer;
  c: byte;
begin
  Key[0] := 305419896;
  Key[1] := 591751049;
  Key[2] := 878082192;
  for i := 0 to Length(Password) - 1 do
    ZipUpdateKeys(pByte(PAnsiChar(password) + i)^, Key);
  for i := 0 to sizeof(TZipKeyHeader) - 1 do
  begin
    c := KeyHeader[i] xor ZipDecryptByte(Key);
    ZipUpdateKeys(c, Key);
    KeyHeader[i] := c;
  end;
end;


//------------------------------------------------------------------------------
// init encrypt zip keys
//------------------------------------------------------------------------------
procedure ZipEncryptKeyInit(const Password: AnsiString; var Key: TZipKey);
var
  i: integer;
begin
  Key[0] := 305419896;
  Key[1] := 591751049;
  Key[2] := 878082192;
  
  for i := 0 to Length(Password) - 1 do
    ZipUpdateKeys(pByte(PAnsiChar(password) + i)^, Key);
end;

//------------------------------------------------------------------------------
// get dir+'\'
//------------------------------------------------------------------------------
function GetSlashedDir(Dir, CurDir: TFXCString): TFXCString;
begin
  if (Dir = '') then
    Result := aaIncludeTrailingBackslash(CurDir)
  else
    Result := aaIncludeTrailingBackslash(Dir);
end;// GetSlashedDir


//------------------------------------------------------------------------------
// get full file name
//------------------------------------------------------------------------------
function GetFullFileName(BaseDir, FileName, CurDir: TFXCString): TFXCString;
var
  Dir, cDir: TFXCString;
begin
  if CurDir = '' then CurDir := FXCGetCurrentDir; // 2.76
  if (BaseDir = '') then
    Dir := aaIncludeTrailingBackslash(CurDir)
  else
    Dir := aaIncludeTrailingBackslash(BaseDir);

  if (Pos(':', FileName) <> 0) or (Pos('\\', FileName) = 1) then
    Result := FileName
  else
  if (FileName = '') then
  begin
    if (Pos(':', BaseDir) <> 0) then
      Result := BaseDir
    else
    begin
      // save current dir
      cDir := CurDir;
      if (FXCDirectoryExists(Dir) or (BaseDir = '')) then
      begin
        // dir1
        if (Dir <> '\') then
          Result := FXCExpandFileName(Dir)
        else
          Result := FXCExtractFilePath(FXCExpandFileName('.') + '\');
      end
      // non-existing dir? - keep original name
      else
        Result := BaseDir;
      // restore current dir
      CurDir := cDir;
    end;
  end
  else
  begin
    if (FileName[1] = '\') then
      Result := FXCExpandFileName(FileName)
    else
      Result := FXCExpandFileName(Dir + FileName);
  end;
end;// GetFullFileName


//------------------------------------------------------------------------------
// returns file name for zip archive
// 'c:\folder1\file1.txt' -> 'folder1/file1.txt'
//------------------------------------------------------------------------------
function GetZipFileName(FileName, BaseDir: TFXCString; StorePathMode: TFXCStorePathMode;
  CurDir: TFXCString): TFXCString;
var
  i, j, k, len: integer;
  sym:  TFXCChar;
  FFileName, s: TFXCString;
  BDir: TFXCString;
begin
  Result := '';
  BDir   := GetFullFileName(BaseDir, '', CurDir);
  FFileName := GetFullFileName(BDir, FileName, CurDir);
  // 'c:\data\' -> 'c:\data'
  if (FFileName <> '') then
    if ((FFileName[Length(FFileName)] = '\') or
      (FFileName[Length(FFileName)] = '/')) then
      FFileName := Copy(FFileName, 1, Length(FFileName) - 1);
  // 'c:\data.' -> 'c:\data'
  if (FFileName <> '') then
    if (FFileName[Length(FFileName)] = '.') then
      FFileName := Copy(FFileName, 1, Length(FFileName) - 1);

  // do store path and how?
  case StorePathMode of
    spNoPath:
      FFileName := FXCExtractFileName(FFileName);
    spRelativePath:
    begin
      s := FXCExtractRelativePath(GetSlashedDir(BDir, CurDir), FFileName);
      if (Pos('..\', s) = 0) and (Pos('../', s) = 0) then
        FFileName := s;
    end;
    spFullPath:
      FFileName := FFileName;
    spFullPathWithDrive:
      FFileName := FXCExpandFileName(FFileName);
  end;

  k   := 0;
  len := length(FFileName);

  if (StorePathMode <> spFullPathWithDrive) then
  begin
    // skip drive letter
    for i := 0 to len - 1 do
      if FFileName[I+1] = ':' then
      begin
        k := i + 1;
        break;
      end;
    if (k >= len) then
      Exit;

    // \\server\path -> path
    if (k = 0) then
    begin
      for i := 0 to len - 2 do
        if (FFileName[i + 1] = '/') or
          (FFileName[i + 1] = '\') then
          if (FFileName[i + 2] = '/') or
            (FFileName[i + 2] = '\') then
          begin
            k := i + 2;
            for j := k to len - 1 do
              if (FFileName[j + 1] = '/') or
                (FFileName[j + 1] = '\') then
              begin
                k := j + 1;
                break;
              end;
            break;
          end;
    end;
    if (FFileName[k + 1] = '\') or
      (FFileName[k + 1] = '/') then
      Inc(k);
  end;

  // fill result file name
  while (k < len) do
  begin
    sym := FFileName[k + 1];
    if (sym = '\') then
      sym := '/';
    Result := Result + sym;
    Inc(k);
  end;
end; // GetZipFileName


////////////////////////////////////////////////////////////////////////////////

//   TFXCVolumeNumberInfo

////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
// GetFirstVolume
//------------------------------------------------------------------------------
function TFXCVolumeNumberInfo.GetFirstVolume: boolean;
begin
  Result := (FVolumeNumber = 0);
end;// GetFirstVolume


//------------------------------------------------------------------------------
// SetFirstVolume
//------------------------------------------------------------------------------
procedure TFXCVolumeNumberInfo.SetFirstVolume(Value: boolean);
begin
  if (Value) then
  begin
    FVolumeNumber := 0;
    FVolumeKind   := vnkFirst;
  end;
end;// SetFirstVolume


//------------------------------------------------------------------------------
// GetLastVolume
//------------------------------------------------------------------------------
function TFXCVolumeNumberInfo.GetLastVolume: boolean;
begin
  if (FLastVolumeNumber <> -1) then
    Result := (FVolumeNumber = FLastVolumeNumber)
  else
    Result := (FVolumeKind = vnkLast);
end;// GetLastVolume


//------------------------------------------------------------------------------
// SetLastVolume
//------------------------------------------------------------------------------
procedure TFXCVolumeNumberInfo.SetLastVolume(Value: boolean);
begin
  if (Value) then
  begin
    FVolumeNumber := FLastVolumeNumber;
    FVolumeKind   := vnkLast;
  end;
end;// SetLastVolume


//------------------------------------------------------------------------------
// SetVolumeNumber
//------------------------------------------------------------------------------
procedure TFXCVolumeNumberInfo.SetVolumeNumber(Value: integer);
begin
  FVolumeKind   := vnkCustom;
  FVolumeNumber := Value;
end;// SetVolumeNumber


//------------------------------------------------------------------------------
// Create
//------------------------------------------------------------------------------
constructor TFXCVolumeNumberInfo.Create;
begin
  Init;
end;// Create


//------------------------------------------------------------------------------
// IsEqualTo
//------------------------------------------------------------------------------
function TFXCVolumeNumberInfo.IsEqualTo(VolumeInfo: TFXCVolumeNumberInfo): boolean;
begin
  if (LastVolume) then
    Result := VolumeInfo.LastVolume
  else
  if (FirstVolume) then
    Result := VolumeInfo.FirstVolume
  else
  if (FVolumeNumber >= 0) then
    Result := (FVolumeNumber = VolumeInfo.VolumeNumber)
  else
    Result := (FVolumeKind = VolumeInfo.FVolumeKind);
end;// IsEqualTo


//------------------------------------------------------------------------------
// Init
//------------------------------------------------------------------------------
procedure TFXCVolumeNumberInfo.Init;
begin
  FVolumeKind   := vnkUnknown;
  FVolumeNumber := -1;
  FLastVolumeNumber := -1;
end;// Init



////////////////////////////////////////////////////////////////////////////////
//   SpanningOptions
////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
// create
//------------------------------------------------------------------------------
constructor TFXCSpanningOptions.Create;
begin
  inherited Create;
  FAdvancedNaming  := False;
  FFirstVolumeSize := 0;
  FVolumeSize      := vsAutoDetect;
  FCustomVolumeSize := 0;
  FSaveDirToFirstVolume := False;
end;


//------------------------------------------------------------------------------
// set volume size
//------------------------------------------------------------------------------
procedure TFXCSpanningOptions.SetCustomVolumeSize(Value: int64);
begin
  // min volume size is 64 Kb
  if (Value < MinVolumeSize) then
    FCustomVolumeSize := MinVolumeSize
  else
    FCustomVolumeSize := Value;
end;// SetCustomVolumeSize


//------------------------------------------------------------------------------
// assign
//------------------------------------------------------------------------------
procedure TFXCSpanningOptions.Assign(Source: TPersistent);
var
  src: TFXCSpanningOptions;
begin
  if Source is TFXCSpanningOptions then
  begin
    src := TFXCSpanningOptions(Source);
    FAdvancedNaming := src.FAdvancedNaming;
    FFirstVolumeSize := src.FFirstVolumeSize;
    FVolumeSize := src.FVolumeSize;
    FCustomVolumeSize := src.FCustomVolumeSize;
    FSaveDirToFirstVolume := src.FSaveDirToFirstVolume;
  end
  else
    inherited Assign(Source);
end;


//------------------------------------------------------------------------------
// gets requested volume size (bytes)
//------------------------------------------------------------------------------
function TFXCSpanningOptions.GetVolumeSize(VolumeNumberInfo:
  TFXCVolumeNumberInfo): int64;
begin
  // first volume?
  if (VolumeNumberInfo.FirstVolume) and (FFirstVolumeSize > 0) then
    Result := FFirstVolumeSize
  else
  begin
    case FVolumeSize of
      vsAutoDetect:
        Result := -1;
      vsCustom:
        Result := FCustomVolumeSize;
      vs1_44MB:
        Result := 1457664; // floppy
      vs100MB:
        Result := 100 * 1024 * 1024;
      vs200MB:
        Result := 200 * 1024 * 1024;
      vs250MB:
        Result := 250 * 1024 * 1024;
      vs600MB:
        Result := 600 * 1024 * 1024;
      vs650MB:
        Result := 650 * 1024 * 1024;
      vs700MB:
        Result := 700 * 1024 * 1024;
      vs4700MB:
        Result := int64(4700) * int64(1024 * 1024);
      else
        Result := -1;
    end;
  end;
end;// GetVolumeSize



////////////////////////////////////////////////////////////////////////////////

//   Options

////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
// create
//------------------------------------------------------------------------------
constructor TFXCOptions.Create;
begin
  inherited Create;
  FStorePath    := spRelativePath;
  FRecurse      := True;
  FOverwriteMode := omAlways;
  FCreateDirs   := True;
  FReplaceReadOnly := True;
  FSetAttributes := True;
  FSearchAttr   := DefaultSearchAttr;
  FShareMode    := smShareDenyNone;
  FFlushBuffers := True;
  FOEMFileNames := True;
end;


//------------------------------------------------------------------------------
// assign
//------------------------------------------------------------------------------
procedure TFXCOptions.Assign(Source: TPersistent);
var
  src: TFXCOptions;
begin
  if Source is TFXCOptions then
  begin
    src      := TFXCOptions(Source);
    FStorePath := src.StorePath;
    FRecurse := src.Recurse;
    FOverwriteMode := src.OverwriteMode;
    FCreateDirs := src.CreateDirs;
    FReplaceReadOnly := src.ReplaceReadOnly;
    FSetAttributes := src.SetAttributes;
    FSearchAttr := src.SearchAttr;
    FFlushBuffers := src.FlushBuffers;
    FOEMFileNames := src.OEMFileNames;
  end
  else
    inherited Assign(Source);
end;




////////////////////////////////////////////////////////////////////////////////

//   TFXCDirArray

////////////////////////////////////////////////////////////////////////////////


constructor TFXCDirArray.Create;
begin
  ItemCount  := 0;
  AllocBy    := 500;   // default alloc
  deAllocBy  := 100;   // default dealloc
  MaxAllocBy := 10000; // max alloc
  AllocItemCount := 0;
  FItemNamesList := TFXCHashedStringList.Create;
end;// Create


//------------------------------------------------------------------------------
// destructor
//------------------------------------------------------------------------------
destructor TFXCDirArray.Destroy;
begin
  Clear;
  FItemNamesList.Free;
end;// Destroy


//------------------------------------------------------------------------------
// Clear
//------------------------------------------------------------------------------
procedure TFXCDirArray.Clear;
var
  i, j: integer;
begin
  for i := 0 to Count - 1 do
    if (Length(FItems[i].ExtraFields) <> 0) then
    begin
      for j := 0 to Length(FItems[i].ExtraFields) - 1 do
      begin
        FreeMem(FItems[i].ExtraFields[j].pData);
        FItems[i].ExtraFields[j].pData := nil;
      end;
      SetLength(FItems[i].ExtraFields, 0);
    end;
  ItemCount := 0;
  FItemNamesList.Clear;
end;


//------------------------------------------------------------------------------
// set item
//------------------------------------------------------------------------------
procedure TFXCDirArray.SetItem(Index: integer; const Value: TDirItem);
var
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then
    raise EFXCException.Create(00001);
  // free extra field
  if (FItems[Index].ExtraFields <> nil) then
    for i := 0 to Length(FItems[Index].ExtraFields) - 1 do
      if (FItems[Index].ExtraFields[i].pData <> nil) then
        FreeMem(FItems[Index].ExtraFields[i].pData);

  // add name to hashed list
  if (Index < FItemNamesList.Count) then
    FItemNamesList.Update(Index, FXCLowerCase(Value.Name))
  else
  if (Index = FItemNamesList.Count) then
    FItemNamesList.Add(FXCLowerCase(Value.Name))
  else
    raise EFXCException.Create(00058);

  // assign
  FItems[Index] := Value;

  // copy extra field
  if (FItems[Index].ExtraFields <> nil) then
  begin
    // copy values
    SetLength(FItems[Index].ExtraFields, Length(Value.ExtraFields));
    for i := 0 to Length(FItems[Index].ExtraFields) - 1 do
    begin
      FItems[Index].ExtraFields[i].headerID := Value.ExtraFields[i].headerID;
      FItems[Index].ExtraFields[i].dataSize := Value.ExtraFields[i].dataSize;
      FItems[Index].ExtraFields[i].pData    :=
        AllocMem(FItems[Index].ExtraFields[i].dataSize);
      if (Value.ExtraFields[i].pData = nil) then
      begin
        FItems[Index].ExtraFields[i].pData := nil;
      end else
        Move(Value.ExtraFields[i].pData^, FItems[Index].ExtraFields[i].pData^,
          FItems[Index].ExtraFields[i].dataSize);
    end;
  end;
end;// SetItem


//------------------------------------------------------------------------------
// get item
//------------------------------------------------------------------------------
function TFXCDirArray.GetItem(Index: integer): TDirItem;
begin
  if (Index < 0) or (Index >= Count) then
    raise EFXCException.Create(00002);
  Result := FItems[Index];
end;// SetItem


//------------------------------------------------------------------------------
// get item ptr
//------------------------------------------------------------------------------
function TFXCDirArray.GetItemPtr(Index: integer): PDirItem;
begin
  if (Index < 0) or (Index >= Count) then
    raise EFXCException.Create(00003);
  Result := @FItems[Index];
end;//GetItemPtr


//------------------------------------------------------------------------------
// set size
//------------------------------------------------------------------------------
procedure TFXCDirArray.SetCount(Value: integer);
var
  i, j, oldCount: integer;
begin
  oldCount := allocItemCount;
  // enlarge?
  if (Value > allocItemCount) then
  begin
    AllocBy := AllocBy * 2;
    if (AllocBy > MaxAllocBy) then
      AllocBy := MaxAllocBy;
    if (allocItemCount + AllocBy > Value) then
      allocItemCount := allocItemCount + AllocBy
    else
      allocItemCount := Value;
    SetLength(FItems, allocItemCount);
    for i := oldCount to allocItemCount - 1 do
    begin
      FItems[i].ExtraFields := nil;
      FItems[i].Password    := '';
      FItems[i].Modified    := False;
      FItems[i].Stream      := nil;
    end;
  end
  else
  if (Value = 0) then
  begin
    for i := Value to oldCount - 1 do
    begin
      if (Length(FItems[i].ExtraFields) <> 0) then
      begin
        for j := 0 to Length(FItems[i].ExtraFields) - 1 do
          FreeMem(FItems[i].ExtraFields[j].pData);
        FItems[i].ExtraFields := nil;
      end;
    end;
    SetLength(FItems, Value);
    allocItemCount := Value;
  end;
  ItemCount := Value;
end;// SetCount


//------------------------------------------------------------------------------
// get size
//------------------------------------------------------------------------------
function TFXCDirArray.GetCount: integer;
begin
  Result := ItemCount;
end;// GetCount


//------------------------------------------------------------------------------
// assign other array
//------------------------------------------------------------------------------
procedure TFXCDirArray.Assign(da: TFXCDirArray);
var
  i: integer;
begin
  Clear;
  Count := da.Count;
  for i := 0 to Count - 1 do
    Items[i] := da.Items[i];
end;// Assign


//------------------------------------------------------------------------------
// append other array
//------------------------------------------------------------------------------
procedure TFXCDirArray.Append(da: TFXCDirArray);
var
  i, oldCount: integer;
begin
  oldCount := Count;
  Count := oldCount+da.Count;
  for i := 0 to da.Count - 1 do
    Items[oldCount+i] := da.Items[i];
end;

//------------------------------------------------------------------------------
// delete item (array is squeezed)
//------------------------------------------------------------------------------
procedure TFXCDirArray.DeleteItem(Index: integer);
var
  i: integer;
begin
  if (Index < 0) or (Index >= Count) then
    raise EFXCException.Create(00004);

  // free item objects
  with Items[Index] do
  begin
    if (Stream <> nil) and (bDestroyStream) then
      Stream.Free;
    for i := 0 to Length(ExtraFields) - 1 do
      if (ExtraFields[i].pData <> nil) then
      begin
        FreeMem(ExtraFields[i].pData);
        ExtraFields[i].pData := nil;
      end;
    SetLength(ItemsPtr[Index].ExtraFields, 0);
  end;

  // delete name from hashed list
  i := FItemNamesList.IndexOf(FXCLowerCase(Items[Index].Name));
  if (i >= 0) then
    FItemNamesList.Delete(i);

  // shift items starting from Index
  for i := Index to Count - 2 do
    FItems[i] := FItems[i + 1];

  // shorten array
  Count := Count - 1;
end;// DeleteItem


//------------------------------------------------------------------------------
// clear tags
//------------------------------------------------------------------------------
procedure TFXCDirArray.ClearTags;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    FItems[i].Tagged := False;
end;// ClearTags


//------------------------------------------------------------------------------
// FileExists
//------------------------------------------------------------------------------
function TFXCDirArray.FileExists(const FileName: TFXCString; var ItemNo: integer): boolean;
begin
  ItemNo := FItemNamesList.IndexOf(FXCLowerCase(FileName));
  Result := (ItemNo >= 0);
end;// FileExists


//------------------------------------------------------------------------------
// SetItemName
//------------------------------------------------------------------------------
procedure TFXCDirArray.SetItemName(Index: integer; const Name: TFXCString);
var
  i: integer;
begin
  i := FItemNamesList.IndexOf(FXCLowerCase(FItems[Index].Name));
  if (i >= 0) then
    if (i = Index) then
      FItemNamesList.Update(i, FXCLowerCase(Name))
    else
      raise EFXCException.Create(00059)
  else
    FItemNamesList.Add(FXCLowerCase(Name));
  FItems[Index].Name := Name;
end;// SetItemName


////////////////////////////////////////////////////////////////////////////////

//   TFXCDirManager

////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
// instantiate object
//------------------------------------------------------------------------------
constructor TFXCDirManager.Create(Stream: TStream; bCreate: boolean; Arc: TBaseArchiver);
begin
  Aborted  := False;
  FCreate  := bCreate;
  FArc     := Arc;
  CDir     := TFXCDirArray.Create;
  RBCDir   := TFXCDirArray.Create;
  FCompressedStream := Stream;
  FIsCustomStream := not (FCompressedStream = FArc.FCompressedStream);
  FZip64   := False;
  StubSize := 0;
  InitCentralDirEnd;
  // fixed by Leo Martin - there is no need to load directory if file is being created
  if (not bCreate) then
  begin
    //   LoadDir;
  end
  else
  begin
    FArc.VolumeNumberInfo.FirstVolume := True;
    if (FArc.SFXStub <> '') then
      SaveSFXStub;
  end;
end;// Create


//------------------------------------------------------------------------------
// destructor
//------------------------------------------------------------------------------
destructor TFXCDirManager.Destroy;
begin
  CDir.Free;
  RBCDir.Free;
end;// Destroy


//------------------------------------------------------------------------------
// Is Encrypted
//------------------------------------------------------------------------------
function TFXCDirManager.IsEncrypted: boolean;
begin
  Result := False;
  if (CDir.Count > 0) then
    Result := ((CDir.Items[0].CentralDir.genPurposeFlag and $0001) = 1);
end;// IsEncrypted


//------------------------------------------------------------------------------
// returns central dir end
//------------------------------------------------------------------------------
function TFXCDirManager.GetCentralDirEnd(var DirEnd: TFXCZipCentralDirEnd;
  var position: int64; Signature: longword): boolean;
var
  sgn: array [0..3] of AnsiChar;
  size, i, j, k: int64;
  buf: PAnsiChar;

  function CheckCDirEnd(Pos: int64): boolean;
  var
    oldPos: int64;
  begin
    Result := False;
    oldPos := FCompressedStream.Position;
    FCompressedStream.Position := Pos;
    if (FCompressedStream.Size - FCompressedStream.Position >= sizeof(DirEnd)) then
    begin
      FCompressedStream.ReadBuffer(DirEnd, sizeof(DirEnd));
// fix in 5.06: thanks to Craig Peterson
//      if (DirEnd.commentLength = FCompressedStream.Size -
//        FCompressedStream.Position) then
//        Result := True
//      else
//        Result := False;
       Result :=
         (DirEnd.startDiskNumber <= DirEnd.diskNumber) and
         (DirEnd.entriesOnDisk <= DirEnd.entriesCentralDir) and
         ((DirEnd.centralDirSize = $FFFFFFFF) or (DirEnd.centralDirSize 
< FCompressedStream.Position)) and
         ((DirEnd.startDiskNumber < DirEnd.diskNumber) or
          (DirEnd.offsetStartDir = $FFFFFFFF) or
          (DirEnd.offsetStartDir < FCompressedStream.Position - 
DirEnd.centralDirSize)) and
         (DirEnd.commentLength <= FCompressedStream.Size - 
FCompressedStream.Position);
    end;
    FCompressedStream.Position := oldPos;
  end; //CheckCDirEnd
begin
  // repeat
  position := -1;
  Result   := False;
  ArchiveComment := '';
  FillChar(DirEnd, sizeof(DirEnd)*sizeof(AnsiChar), $00);
  i := Signature;
  Move(i, sgn, 4);
  size := Min($10040, FCompressedStream.Size);
  if (FCompressedStream.Size < size) then
    Exit;
  // searching central dir end
  buf := AllocMem(size);
  if not (FCompressedStream is TStringStream) then
    FCompressedStream.Seek(-size, soFromEnd)
  else
    FCompressedStream.Position := FCompressedStream.Size - size; // workaround TStringStream bug
  FCompressedStream.ReadBuffer(buf^, size);
  // searching signature from end of last block
  i := size - 1;
  k := -1;
  while (i >= 0) do
  begin
    if (PByte(PAnsiChar(Buf) + i)^ = Byte(sgn[3])) then
    begin
      j := 2;
      while j >= 0 do
      begin
        if ((i - (3 - j)) < 0) then
          break;
        if (PByte(PAnsiChar(Buf) + i - (3 - j))^ <> Byte(sgn[j])) then
          break;
        Dec(j);
        if (j < 0) then
          k := i - 3;
      end;
      // bug fix
      if (j < 0) then
        if (CheckCDirEnd(FCompressedStream.Size - size + k)) then
        begin
          position := FCompressedStream.Size - size + k;
          break;
        end;
    end;
    Dec(i);
  end; // end of searching central dir end
       // not found?
  if (k < 0) or (Position < 0) or (FCompressedStream.Size - k < sizeof(DirEnd)) then
    FreeMem(buf)
  else
  begin
    Move(PAnsiChar(Buf + k)^, DirEnd, sizeof(DirEnd));
    FreeMem(buf);

    // loading archive comment
    if (DirEnd.commentLength > 0) then
    begin
      FCompressedStream.Position := position + FXCZipCentralDirEndSize;
      SetLength(ArchiveComment, DirEnd.commentLength);
      FCompressedStream.Read(PAnsiChar(ArchiveComment)^, DirEnd.commentLength);
    end;
    Result := True;
  end;

end;// GetCentralDirEnd


//------------------------------------------------------------------------------
// detects stub and+ gets its size
//------------------------------------------------------------------------------
procedure TFXCDirManager.CalculateStubSize(DirEndPos: int64);
var
  offset, i: int64;
  LHeader: TFXCZipFileHeader;
  Dir: TFXCZipCentralDir;
  Name: TFXCString;
  name2: AnsiString;

  function CheckFile(headerPos: int64): boolean;
  var
    oldPos: int64;
  begin
    Result := False;
    oldPos := FCompressedStream.Position;
    FCompressedStream.Position := headerPos;
    if (FCompressedStream.Size - FCompressedStream.Position >= FXCZipFileHeaderSize) then
    begin
      FCompressedStream.ReadBuffer(LHeader, FXCZipFileHeaderSize);
      SetLength(name2, LHeader.nameLength);
      if (FCompressedStream.Read(PAnsiChar(name2)^, LHeader.nameLength) =
        LHeader.nameLength) then
        if ((LHeader.signature = ZipFileHeaderSignature) or
          (LHeader.signature = FXCFileHeaderSignature)) then
        begin
          if (headerPos = 0) or ((LHeader.crc32 = Dir.crc32) and
            (LHeader.extractVersion = Dir.extractVersion) and
            (LHeader.compMethod = Dir.compMethod) and
            (LHeader.lastModTime = Dir.lastModTime) and
            (LHeader.lastModDate = Dir.lastModDate) and
            (LHeader.compSize = Dir.compSize) and
            (LHeader.unCompSize = Dir.unCompSize)) then
            Result := True;
        end;

    end;
    FCompressedStream.Position := oldPos;
  end; //CheckFile

begin
  if (CentralDirEnd.entriesCentralDir = 0) then
  begin
    StubSize := DirEndPos;
    Exit;
  end;
  if (FArc.isZIPFormat) then
    i := ZipFileHeaderSignature
  else
    i := FXCFileHeaderSignature;
  Dir := CDir.Items[0].CentralDir;
  Name := CDir.Items[0].Name;
  if (Dir.relOffsetLH <> $FFFFFFFF) then
    offset := Dir.relOffsetLH
  else
    offset := CDir.Items[0].Zip64ExtInfo.relOffsetLH;

  FCompressedStream.Position := offset;
  // find local file header for first file in archive
  while FCompressedStream.Position < FCompressedStream.Size do
    if (not FindSignature(FCompressedStream, offset, @i, 4)) then
      break
    else
    begin
      FCompressedStream.Position := offset;
      if (CheckFile(offset)) then
      begin
        StubSize := offset;
        break;
      end
      else
        Inc(Offset);
    end;
end;// CalculateStubSize


//------------------------------------------------------------------------------
// saves SFX stub to empty archive
//------------------------------------------------------------------------------
procedure TFXCDirManager.SaveSFXStub;
var
  StubStream: TFXCFileStream;
  VolumeNumberInfo: TFXCVolumeNumberInfo;
begin
  // open stub
  StubStream := TFXCFileStream.Create(FArc.SFXStub, fmOpenRead or fmShareDenyNone);
  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  VolumeNumberInfo.FirstVolume := True;
  try
    if (FArc.SpanningMode <> smNone) and
      (FArc.SpanningOptions.GetVolumeSize(VolumeNumberInfo) <> -1) and
      (StubStream.Size > FArc.SpanningOptions.GetVolumeSize(VolumeNumberInfo)) then
      raise EFXCException.Create(00047, FArc);
    FCompressedStream.Size := 0;
    FCompressedStream.CopyFrom(StubStream, StubStream.Size);
    StubSize := StubStream.Size;
  finally
    VolumeNumberInfo.Free;
    StubStream.Free;
  end;
end;// SaveSFXStub


//------------------------------------------------------------------------------
// inits central dir end
//------------------------------------------------------------------------------
procedure TFXCDirManager.InitCentralDirEnd;
begin
  // fill central dir end
  CDir.Count := 0;
  FillChar(CentralDirEnd, FXCZipCentralDirEndSize, $00);
  if FArc.isZIPFormat then
    CentralDirEnd.signature := ZipCentralDirEndSignature
  else
    CentralDirEnd.signature := FXCCentralDirEndSignature;
  CentralDirEnd.diskNumber := $00;
  CentralDirEnd.startDiskNumber := $00;
end;// InitCentralDirEnd


//------------------------------------------------------------------------------
// find signature
//------------------------------------------------------------------------------
function TFXCDirManager.FindSignature(Stream: TStream; var Offset: int64;
  Signature: PAnsiChar; SignatureLen: integer): boolean;
var
  Buf:     PAnsiChar;
  BufSize: integer;
  WorkPos, OldPos: int64;

  function FindSignatureInBlock(FilePos: int64; var SignatureOffset: int64): boolean;
  var
    i: integer;
    SizeToCheck: integer;
  begin
    Result      := False;
    SizeToCheck := min(Stream.Size - Stream.Position, BufSize) - SignatureLen;
    Stream.Position := FilePos;
    Stream.Read(Buf^, BufSize);
    for i := 0 to SizeToCheck do
      if (StrLComp(PAnsiChar(Signature), Buf + i, SignatureLen) = 0) then
      begin
        Result := True;
        SignatureOffset := FilePos + i;
        break;
      end;
  end;

begin
  Result  := False;
  BufSize := 10000;
  OldPos  := Stream.Position;
  Buf     := AllocMem(BufSize);
  try
    WorkPos := Offset;
    while (WorkPos < Stream.Size) do
      if (FindSignatureInBlock(WorkPos, Offset)) then
      begin
        Result := True;
        break;
      end
      else
        WorkPos := WorkPos + BufSize - SignatureLen;
  finally
    FreeMem(Buf);
    Stream.Position := OldPos;
  end;
end;// FindSignature


//------------------------------------------------------------------------------
// find local file header in the compressed stream
//------------------------------------------------------------------------------
function TFXCDirManager.FindLocalFileHeader(StartPosition: int64): int64;
var
  size, pos, oldPos: int64;
  k, i, j: int64;
  buf:     PAnsiChar;
  sgn:     array [0..3] of AnsiChar;
begin
  Result := -1;
  oldPos := FCompressedStream.Position;
  FCompressedStream.Position := StartPosition;
  if (FArc.isZIPFormat) then
    i := ZipFileHeaderSignature
  else
    i := FXCFileHeaderSignature;
  Move(i, sgn, 4);
  buf := AllocMem($FFFF);
  // find local file header for first file in archive
  while FCompressedStream.Position < FCompressedStream.Size do
  begin
    pos  := FCompressedStream.Position;
    size := Min($FFFF, (FCompressedStream.Size - FCompressedStream.Position));
    FCompressedStream.ReadBuffer(buf^, size);
    // find local file header signature
    i := 0;
    k := -1;
    while (i < size) do
    begin
      k := -1;
      if (PAnsiChar(Buf + i)^ = sgn[0]) then
      begin
        j := 1;
        while j <= 3 do
        begin
          if ((i + j) >= size) then
            break;
          if (PAnsiChar(Buf + i + j)^ <> sgn[j]) then
            break;
          Inc(j);
          // signature found
          if (j > 3) then
            k := i;
        end; // check signature
        if (k >= 0) then
          break;
      end; // signature found
      Inc(i);
    end; // end of searching local header

    // calculate offset of LocalFileHeader relatively the beginning of the file
    if (k >= 0) then
    begin
      Result := k + pos;
      break;
    end;
    FCompressedStream.Position := pos + size - 4; // 4 = signature size
    if (FCompressedStream.Position <= pos) then
      break;
  end;
  if (Result >= 0) then
    if (FCompressedStream.Size - Result < FXCZipFileHeaderSize) then
      Result := -1; // local file header is cut off
  FCompressedStream.Position := oldPos;
  FreeMem(buf);
end;


//------------------------------------------------------------------------------
// load all dirs
//------------------------------------------------------------------------------
procedure TFXCDirManager.LoadDir;
var
  DirEnd:   TFXCZipCentralDirEnd;
  size, pos, loadedSize: int64;
  i, itemNo, offset: integer;
  Name:     TFXCString;
  buf, buf1:  array [0..65535] of AnsiChar;
  LFHeader: TFXCZipFileHeader;
  startDiskNumber, lastDiskNumber: integer;
  IsCentralDirLoaded: boolean;
  VolumeNumberInfo: TFXCVolumeNumberInfo;

  function FindDataDescriptor(filePos: int64;
  var ZipDataDescriptor: TFXCZipDataDescriptor): boolean;
  var
    size, pos: int64;
    k, i, j: int64;
    buf:    PAnsiChar;
    sgn:    array [0..3] of AnsiChar;
    offset: int64;
  begin
    Result := False;
    offset := 0;
    FCompressedStream.Position := filePos;
    i      := ZipDataDescriptorSignature;
    Move(i, sgn, 4);
    buf := AllocMem($FFFF);
    // find local file header for first file in archive
    while FCompressedStream.Position < FCompressedStream.Size do
    begin
      pos  := FCompressedStream.Position;
      size := Min($FFFF, (FCompressedStream.Size - FCompressedStream.Position));
      FCompressedStream.ReadBuffer(buf^, size);
      // find local file header signature
      i := 0;
      k := -1;
      while (i < size) do
      begin
        k := -1;
        if (PAnsiChar(Buf + i)^ = sgn[0]) then
        begin
          j := 1;
          while j <= 3 do
          begin
            if ((i + j) >= size) then
              break;
            if (PAnsiChar(Buf + i + j)^ <> sgn[j]) then
              break;
            Inc(j);
            // signature found
            if (j > 3) then
              k := i;
          end; // check signature
          if (k >= 0) then
            break;
        end; // signature found
        Inc(i);
      end; // end of searching descriptor

      // calculate offset of descriptor
      if (k >= 0) then
      begin
        Result := True;
        offset := k + pos;
        break;
      end;
      FCompressedStream.Position := pos + size - 4; // 4 = signature size
      if (FCompressedStream.Position <= pos) then
        break;
    end;
    if (Result) then
    begin
      FCompressedStream.Position := offset;
      if (FCompressedStream.Size - Offset >= sizeof(TFXCZipDataDescriptor)) then
        FCompressedStream.Read(ZipDataDescriptor, sizeof(ZipDataDescriptor));
    end;
    FreeMem(buf);
  end;// FindDataDescriptor

  procedure BuildCentralDirByLocalHeaders;
  var
    filePos: int64;
    ZipDataDescriptor: TFXCZipDataDescriptor;
    AnsiName: AnsiString;
    ms : TMemoryStream;
    vers : Word;
    vendor : Word;
    strength : Byte;
    comprMethod : Word;
  begin
    filePos := FindLocalFileHeader(0);
    if (filePos < 0) then
      raise EFXCException.Create(00035);

    while filePos >= 0 do
    begin
      // load local file header to central dir
      FCompressedStream.Position := filePos;
      FCompressedStream.ReadBuffer(LFHeader, FXCZipFileHeaderSize);


      if ((Int64(LFHeader.compSize) < FCompressedStream.Size) and
        (LFHeader.extraLength <= MAX_PATH)) then
      begin
        i := CDir.Count;
        CDir.SetCount(i + 1);
        CDir.ItemsPtr[i].CentralDir.signature  := LFHeader.signature;
        CDir.ItemsPtr[i].CentralDir.versionMadeBy := LFHeader.extractVersion;
        CDir.ItemsPtr[i].CentralDir.extractVersion := LFHeader.extractVersion;
        CDir.ItemsPtr[i].CentralDir.genPurposeFlag := LFHeader.genPurposeFlag;
        CDir.ItemsPtr[i].CentralDir.compMethod := LFHeader.compMethod;
        CDir.ItemsPtr[i].CompressionMethod := LFHeader.compMethod;

        CDir.ItemsPtr[i].CentralDir.lastModTime := LFHeader.lastModTime;
        CDir.ItemsPtr[i].CentralDir.lastModDate := LFHeader.lastModDate;
        CDir.ItemsPtr[i].CentralDir.crc32      := LFHeader.crc32;
        CDir.ItemsPtr[i].CentralDir.compSize   := LFHeader.compSize;
        CDir.ItemsPtr[i].CentralDir.unCompSize := LFHeader.unCompSize;
        CDir.ItemsPtr[i].CentralDir.nameLength := LFHeader.nameLength;
        CDir.ItemsPtr[i].CentralDir.extraLength := LFHeader.extraLength;
        CDir.ItemsPtr[i].CentralDir.commentLength := 0;
        CDir.ItemsPtr[i].CentralDir.diskNumberStart := 0;
        CDir.ItemsPtr[i].CentralDir.internalAttr := 0;
        if ((CDir.ItemsPtr[i].CentralDir.unCompSize = 0) and
          ((CDir.Items[i].CentralDir.genPurposeFlag and $0008) = 0)) then
          CDir.ItemsPtr[i].CentralDir.externalAttr := word(FXCDirectoryAttr)
        else
          CDir.ItemsPtr[i].CentralDir.externalAttr := word(FXCNormalFileAttr);
        CDir.ItemsPtr[i].CentralDir.relOffsetLH := filePos;

        

        // reading file name
        size := LFHeader.nameLength;
        if (size > Length(buf) - 1) then
          size := Length(buf) - 1;
        if (size > 0) and (FCompressedStream.Size -
          FCompressedStream.Position >= size) then
        begin
          SetLength(AnsiName, size);
          FCompressedStream.ReadBuffer(PAnsiChar(AnsiName)^, size);
          Move(AnsiName[1], Buf[0], Length(AnsiName) + 1);

          {$IFDEF FC_VERSION}

          // decrypt Name ?
          if (((CentralDirEnd.signature = FXCCentralDirEndSignature) or
            (CDir.Items[i].CentralDir.extractVersion = FXCVersion)) and
            ((CDir.Items[i].CentralDir.genPurposeFlag and $0001) = 1)) then
          begin
            CDir.ItemsPtr[i]^.UseOldRijndael := true;
            FXCInternalDecryptBuffer(caRijndael_256, @buf, @buf1, size, AnsiString(IntToStr(FXCCentralDirEndSignature)), CDir.ItemsPtr[i]^.UseOldRijndael);
            if (FArc.Options.OEMFileNames) then
               OEMToCharA(Buf1, Buf1);
            if not IsFilenameValid(string(Buf1)) then
            begin
              CDir.ItemsPtr[i]^.UseOldRijndael := false;
              FXCInternalDecryptBuffer(caRijndael_256, @buf, @buf1, size, AnsiString(IntToStr(FXCCentralDirEndSignature)), CDir.Items[i].UseOldRijndael);
              if (FArc.Options.OEMFileNames) then
                OEMToCharA(Buf1, Buf1);
            end;
            buf := buf1;
          end;
          {$ENDIF}
          
          CDir.SetItemName(i, TFXCString(Buf));
        end
        else
          CDir.SetItemName(i, '');

        // Reading extra fields
        size := CDir.Items[i].CentralDir.extraLength;
        if (size > 0) then
        begin
          loadedSize := 0;
          itemNo     := 0;
          CDir.ItemsPtr[i]^.bHugeFile := False;
          while (loadedSize <= size-4) do
          begin
            SetLength(CDir.ItemsPtr[i]^.ExtraFields, itemNo + 1);
            // read headerID, dataSize
            FCompressedStream.ReadBuffer(CDir.Items[i].ExtraFields[itemNo], 4);
            loadedSize := loadedSize + 4;
            // error: data exceeds remaining extra field
            if (CDir.Items[i].ExtraFields[itemNo].dataSize > size - loadedSize) then
              CDir.Items[i].ExtraFields[itemNo].dataSize := size - loadedSize;
            // alloc mem for extra field
            CDir.Items[i].ExtraFields[itemNo].pData :=
              AllocMem(CDir.Items[i].ExtraFields[itemNo].dataSize);
            // read extra field data
            FCompressedStream.ReadBuffer(CDir.Items[i].ExtraFields[itemNo].pData^,
              CDir.Items[i].ExtraFields[itemNo].dataSize);
            loadedSize := loadedSize + CDir.Items[i].ExtraFields[itemNo].dataSize;
            // if Zip64 extended info - copy it to record
            if (CDir.Items[i].ExtraFields[itemNo].headerID = $0001) then
            begin
              offset := 0;
              // uncompSize?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.uncompSize = $FFFFFFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.uncompSize :=
                    PInt64(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  Inc(offset, sizeof(int64));
                end;
              // compSize?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.compSize = $FFFFFFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.compSize :=
                    PInt64(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  Inc(offset, sizeof(int64));
                end;
              // relOffsetLH?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.relOffsetLH = $FFFFFFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.relOffsetLH :=
                    PInt64(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  Inc(offset, sizeof(int64));
                end;
              // diskNumberStart?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.diskNumberStart = $FFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.diskNumberStart :=
                    PWord(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  //            Inc(offset, sizeof(Word));
                end;

              CDir.ItemsPtr[i]^.bHugeFile := True;
            end;
            {$IFDEF FXCUNICODE}
            // unicode filename
            if (CDir.Items[i].ExtraFields[itemNo].headerID = UnicodeExtraFieldHeaderID) then
            begin
              with TUnicodeExtendedInfo.Create do
              begin
                try
                  if Load(CDir.Items[i].ExtraFields[itemNo].pData, CDir.Items[i].ExtraFields[itemNo].dataSize) then
                  begin
                    {$IFDEF FC_VERSION}
                    // decrypt Name ?
                    if (((CentralDirEnd.signature = FXCCentralDirEndSignature) or
                      (CDir.Items[i].CentralDir.extractVersion = FXCVersion)) and
                      ((CDir.Items[i].CentralDir.genPurposeFlag and $0001) = 1)) then
                    begin
                      FXCInternalDecryptBuffer(caRijndael_256, PByte(Name), PByte(Name),
                        Length(Name) * 2, AnsiString(IntToStr(FXCCentralDirEndSignature)), CDir.Items[i].UseOldRijndael);
                    end;
                    {$ENDIF}
                    CDir.SetItemName(i, Name);
                  end;
                finally
                  Free;
                end;
              end;
            end;
            {$ENDIF}

            

            // next item
            Inc(itemNo);
          end;
          if loadedSize < size then
            FCompressedStream.Position := FCompressedStream.Position + size - loadedSize;		  
        end;

        ///////////////////////////////////////////////////////////////////////

        // has data descriptor?
        if (CDir.Items[i].CentralDir.genPurposeFlag and $0008 <> 0) then
        begin
          if (FindDataDescriptor(filePos, ZipDataDescriptor)) then
          begin
            CDir.ItemsPtr[i].CentralDir.crc32      := ZipDataDescriptor.crc32;
            CDir.ItemsPtr[i].CentralDir.compSize   := ZipDataDescriptor.compressedSize;
            CDir.ItemsPtr[i].CentralDir.unCompSize := ZipDataDescriptor.uncompressedSize;
          end;
        end;
      end;
      filePos := FindLocalFileHeader(filePos + 4);
    end;
  end; //   BuildCentralDirByLocalHeaders;


  // check Zip64CentralDirEndLocator
  function GetZip64CentralDirEndLocator(var curPos: int64): boolean;
  var
    signature: longword;
    p1, p2:    PAnsiChar;
  begin
    Result := False;
    // file is large enough?
    if (curPos > FXCZip64CentralDirEndLocatorSize) then
    begin
      // seek to Zip64 CDir end locator
      FCompressedStream.Position := curPos - integer(FXCZip64CentralDirEndLocatorSize);
      FCompressedStream.ReadBuffer(Zip64CentralDirEndLocator,
        FXCZip64CentralDirEndLocatorSize);
      signature := Zip64CentralDirEndLocatorSignature;
      p1 := PAnsiChar(@Zip64CentralDirEndLocator.signature);
      p2 := PAnsiChar(@signature);
      if (StrLComp(p1, p2, sizeof(Zip64CentralDirEndLocator.signature)) = 0) then
      begin
        Result := True;
        curPos := curPos - integer(FXCZip64CentralDirEndLocatorSize);
      end;
    end;
  end;// GetZip64CentralDirEndLocator

  // check Zip64CentralDirEnd
  function GetZip64CentralDirEnd(var curPos: int64): boolean;
  var
    signature: longword;
    p1, p2:    PAnsiChar;
  begin
    Result := False;
    // file is large enough?
    if (curPos > FXCZip64CentralDirEndSize) then
    begin
      // seek to Zip64 CDir End
      FCompressedStream.Position := Zip64CentralDirEndLocator.offsetStartDirEnd;
      // read Zip64 CDir End
      FCompressedStream.ReadBuffer(Zip64CentralDirEnd, FXCZip64CentralDirEndSize);
      signature := Zip64CentralDirEndSignature;
      p1 := PAnsiChar(@Zip64CentralDirEnd.signature);
      p2 := PAnsiChar(@signature);
      if (StrLComp(p1, p2, sizeof(Zip64CentralDirEnd.signature)) = 0) then
      begin
        Result := True;
        curPos := curPos - integer(FXCZip64CentralDirEndSize);
      end;
    end;
  end;

  function IsStubOffsetApplied: boolean;
  var
    i: integer;
  begin
    Result := True;
    i      := 0;
    while (i < CDir.Count) do
      if (CDir.Items[i].CentralDir.compSize > 0) then
      begin
        Result := (CDir.Items[i].CentralDir.relOffsetLH >= longword(StubSize));
        break;
      end
      else
        Inc(i); // skip folders
  end;

  procedure TryToBuildCentralDir;
  begin
    CDir.Clear;
    // if single-file archive - try to auto-repair it
    if (FArc.SpanningMode = smNone) then
      // build central dir
      BuildCentralDirByLocalHeaders
    else
      raise EFXCException.Create(00044);
    CentralDirOffset := FCompressedStream.Size;
  end;

  procedure CheckIfCentralDirIsValid;
  var
    TotalCompressedSize: int64;
    i: integer;
  begin
    TotalCompressedSize := 0;
    for i := 0 to CDir.Count - 1 do
      if (CDir.Items[i].CentralDir.compSize = $FFFFFFFF) then
        TotalCompressedSize := TotalCompressedSize + CDir.Items[i].Zip64ExtInfo.compSize
      else
        TotalCompressedSize := TotalCompressedSize + CDir.Items[i].CentralDir.compSize;
        
    if (TotalCompressedSize > FCompressedStream.Size) then
      raise EFXCException.Create(00057);
  end;

var
  AnsiName: AnsiString;
  ws: WideString;
  version, vendor, compMethod : Word;
  strength : Byte;
  ms : TMemoryStream;

begin
  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  try
    // init CDirEnd record
    InitCentralDirEnd;
    StubSize := 0;

    // try to find CDirEnd
    // IsCentralDirLoaded := false;
    if FArc.isZIPFormat then
      IsCentralDirLoaded := GetCentralDirEnd(DirEnd, pos, ZipCentralDirEndSignature)
    else
      IsCentralDirLoaded := GetCentralDirEnd(DirEnd, pos, FXCCentralDirEndSignature) or
        GetCentralDirEnd(DirEnd, pos, ZipCentralDirEndSignature);
    if (not IsCentralDirLoaded) 
      // 5.06 addition
      or (ForceBuildCentralDir) then
    begin
      TryToBuildCentralDir;
      Exit;
    end;

    CentralDirEnd := DirEnd;

    //--- check Zip64 records
    // CDir end locator
    FZip64 := GetZip64CentralDirEndLocator(pos);
    if (FZip64) then
      // CDir end
      if not (GetZip64CentralDirEnd(pos)) then
        raise EFXCException.Create(00040, nil);

    // if not Zip64, but has > 65535 files
    if (not FZip64) and (CentralDirEnd.entriesCentralDir = $FFFF) then
    begin
      TryToBuildCentralDir;
      Exit;
    end;

    //--- load central dir
    // get start disk No, last disk No
    if (CentralDirEnd.startDiskNumber = $FFFF) then
      if (FZip64) then
        startDiskNumber := Zip64CentralDirEnd.startDiskNumber
      else
        startDiskNumber := 0
    else
      startDiskNumber := CentralDirEnd.startDiskNumber;
    if (CentralDirEnd.DiskNumber = $FFFF) then
      if (FZip64) then
        lastDiskNumber := Zip64CentralDirEndLocator.totalNumberOfDisks - 1
      else
        lastDiskNumber := 0
    else
      lastDiskNumber := CentralDirEnd.diskNumber;

    VolumeNumberInfo.LastVolumeNumber := lastDiskNumber;
    VolumeNumberInfo.VolumeNumber     := startDiskNumber;
    // request disk
    if (FArc.SpanningMode <> smNone) then
      OpenVolume(VolumeNumberInfo);

    // backward seek to the beginning of CDir
    if (FZip64) then
    begin
      if (FArc.SpanningMode = smNone) then
        FCompressedStream.Position := pos - Zip64CentralDirEnd.centralDirSize
      else
        FCompressedStream.Position := Zip64CentralDirEnd.offsetStartDir;
      CDir.Count := Zip64CentralDirEnd.entriesCentralDir;
    end
    else
    begin
      if (FArc.SpanningMode = smNone) then
        FCompressedStream.Position := pos - integer(DirEnd.centralDirSize)
      else
        FCompressedStream.Position := DirEnd.offsetStartDir;
      CDir.Count := 0; // free allocated memory
      CDir.Count := DirEnd.entriesCentralDir;
    end;

    try
      for i := 0 to CDir.Count - 1 do
      begin
        // request new volume?
        if (FCompressedStream.Size - FCompressedStream.Position <
          FXCZipCentralDirSize) and (FArc.SpanningMode <> smNone) then
        begin
          VolumeNumberInfo.VolumeNumber := VolumeNumberInfo.VolumeNumber + 1;
          OpenVolume(VolumeNumberInfo);
        end;

        // store offset
        if (i = 0) then
          CentralDirOffset := FCompressedStream.Position;

        // reading CentralDir element
        FCompressedStream.ReadBuffer(CDir.ItemsPtr[i]^.CentralDir, FXCZipCentralDirSize);
        
        

        CDir.ItemsPtr[i].CompressionMethod := CDir.ItemsPtr[i].CentralDir.compMethod;
        // reading file name
        size := CDir.Items[i].CentralDir.nameLength;
        if (size > Length(buf) - 1) then
          size := Length(buf) - 1;
        if (size > 0) then
        begin
          SetLength(AnsiName, size);
          FCompressedStream.ReadBuffer(PAnsiChar(AnsiName)^, size);
          
          memset(@Buf, 0, sizeof(Buf));
          memset(@Buf1, 0, sizeof(Buf1));
          Move(AnsiName[1], Buf[0], Length(AnsiName) + 1);

          {$IFDEF FC_VERSION}
          // decrypt Name ?
          if (((DirEnd.signature = FXCCentralDirEndSignature) or
            (CDir.Items[i].CentralDir.extractVersion = FXCVersion)) and
            ((CDir.Items[i].CentralDir.genPurposeFlag and $0001) = 1)) then
          begin
            CDir.ItemsPtr[i]^.UseOldRijndael := false;
            FXCInternalDecryptBuffer(caRijndael_256, @buf, @buf1, Length(AnsiName), AnsiString(IntToStr(FXCCentralDirEndSignature)), CDir.Items[i].UseOldRijndael);
            if (FArc.Options.OEMFileNames) then
              OEMToCharA(Buf1, Buf1);
            if not IsFilenameValid(string(Buf1)) then
            begin
              CDir.ItemsPtr[i]^.UseOldRijndael := True;
              FXCInternalDecryptBuffer(caRijndael_256, @buf, @buf1,
                Length(AnsiName), AnsiString(IntToStr(FXCCentralDirEndSignature)), CDir.ItemsPtr[i]^.UseOldRijndael);
              if (FArc.Options.OEMFileNames) then
                  OEMToCharA(Buf1, Buf1);
            end;
            buf := buf1;
          end;
          {$ENDIF}
          

          Move(Buf[0], AnsiName[1], Length(AnsiName) + 1);
          PatchFileNameForRead(AnsiName, ws);
          CDir.SetItemName(i, TFXCString(ws));
        end
        else
          CDir.SetItemName(i, '');

        // reading extra fields
        size := CDir.Items[i].CentralDir.extraLength;
        if (size > 0) then
        begin
          loadedSize := 0;
          itemNo     := 0;
          CDir.ItemsPtr[i]^.bHugeFile := False;
          while (loadedSize <= size-4) do
          begin
            SetLength(CDir.ItemsPtr[i]^.ExtraFields, itemNo + 1);
            // read headerID, dataSize
            FCompressedStream.ReadBuffer(CDir.Items[i].ExtraFields[itemNo], 4);
            loadedSize := loadedSize + 4;
            // error: data exceeds remaining extra field
            if (CDir.Items[i].ExtraFields[itemNo].dataSize > size - loadedSize) then
              CDir.Items[i].ExtraFields[itemNo].dataSize := size - loadedSize;
            // alloc mem for extra field
            CDir.Items[i].ExtraFields[itemNo].pData :=
              AllocMem(CDir.Items[i].ExtraFields[itemNo].dataSize);
            // read extra field data
            FCompressedStream.ReadBuffer(CDir.Items[i].ExtraFields[itemNo].pData^,
              CDir.Items[i].ExtraFields[itemNo].dataSize);
            loadedSize := loadedSize + CDir.Items[i].ExtraFields[itemNo].dataSize;
            // if Zip64 extended info - copy it to record
            if (CDir.Items[i].ExtraFields[itemNo].headerID = $0001) then
            begin
              offset := 0;
              // uncompSize?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.uncompSize = $FFFFFFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.uncompSize :=
                    PInt64(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  Inc(offset, sizeof(int64));
                end;
              // compSize?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.compSize = $FFFFFFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.compSize :=
                    PInt64(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  Inc(offset, sizeof(int64));
                end;
              // relOffsetLH?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.relOffsetLH = $FFFFFFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.relOffsetLH :=
                    PInt64(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                  Inc(offset, sizeof(int64));
                end;
              // diskNumberStart?
              if (offset < CDir.Items[i].ExtraFields[itemNo].dataSize) then
                if (CDir.Items[i].CentralDir.diskNumberStart = $FFFF) then
                begin
                  CDir.ItemsPtr[i]^.Zip64ExtInfo.diskNumberStart :=
                    PWord(PAnsiChar(CDir.Items[i].ExtraFields[itemNo].pData) + offset)^;
                end;

              CDir.ItemsPtr[i]^.bHugeFile := True;
            end;
            {$IFDEF FXCUNICODE}
            // unicode filename
            if (CDir.Items[i].ExtraFields[itemNo].headerID = UnicodeExtraFieldHeaderID) then
            begin
              with TUnicodeExtendedInfo.Create do
              begin
                try
                  if Load(CDir.Items[i].ExtraFields[itemNo].pData, CDir.Items[i].ExtraFields[itemNo].dataSize) then
                  begin
                    {$IFDEF FC_VERSION}
                    // decrypt Name ?
                    if (((CentralDirEnd.signature = FXCCentralDirEndSignature) or
                      (CDir.Items[i].CentralDir.extractVersion = FXCVersion)) and
                      ((CDir.Items[i].CentralDir.genPurposeFlag and $0001) = 1)) then
                    begin
                      FXCInternalDecryptBuffer(caRijndael_256, PByte(Name), PByte(Name),
                        Length(Name) * 2, AnsiString(IntToStr(FXCCentralDirEndSignature)), CDir.Items[i].UseOldRijndael);
                    end;
                    {$ENDIF}
                    CDir.SetItemName(i, Name);
                  end;
                finally
                  Free;
                end;
              end;
            end;
            {$ENDIF}

            
            // next item
            Inc(itemNo);
          end;
		  if loadedSize < size then
           FCompressedStream.Position := FCompressedStream.Position + size - loadedSize;
        end;
        // reading file comment
        size := CDir.Items[i].CentralDir.commentLength;
        if (size > 0) then
        begin
          SetLength(CDir.ItemsPtr[i]^.Comment, size);
          FCompressedStream.ReadBuffer(PAnsiChar(CDir.ItemsPtr[i]^.Comment)^, size);
        end
        else
          CDir.ItemsPtr[i]^.Comment := '';
      end;
    except
      if (FArc.FOpenCorruptedArchives) then
      begin
        TryToBuildCentralDir;
        Exit;
      end
      else
        raise;
    end;

    if (not FArc.FOpenCorruptedArchives) and (FArc.SpanningMode = smNone) then
      CheckIfCentralDirIsValid;

    if (lastDiskNumber = 0) then
      CalculateStubSize(pos);

    // SaveDebug;

    if (not IsStubOffsetApplied) then
      ApplyStubOffset;
  finally
    VolumeNumberInfo.Free;
  end;
end;// LoadDir


//------------------------------------------------------------------------------
// process volume request
//------------------------------------------------------------------------------
procedure TFXCDirManager.OpenVolume(VolumeNumberInfo: TFXCVolumeNumberInfo);
begin
  if (not FIsCustomStream) then
  begin
    FArc.OpenVolume(VolumeNumberInfo);
    FCompressedStream := FArc.FCompressedStream;
  end;
end;// OpenVolume


//------------------------------------------------------------------------------
// save all dirs
//------------------------------------------------------------------------------
procedure TFXCDirManager.SaveDir(RecreateCDirEnd: boolean; Stream: TStream = nil);
var
  i:   integer;
  pos, CDirSize: int64;
  CDirEnd: TFXCZipCentralDirEnd;
  buf: AnsiString;
  Comment: AnsiString;
  LastVolumeFileName: TFXCString;
  IsCentralDirLoaded: boolean;
  CacheStream: TStream;

  procedure SaveZip64CentralDirEnd(startCentralDir: int64);
  begin
    // store Zip64 CDir End start offset
    Zip64CentralDirEndLocator.offsetStartDirEnd := FCompressedStream.Position;
    // signature
    Zip64CentralDirEnd.signature      := Zip64CentralDirEndSignature;
    // central Dir End 64 Size
    Zip64CentralDirEnd.centralDirEndSize :=
      FXCZip64CentralDirEndSize - sizeof(Zip64CentralDirEnd.signature) -
      sizeof(Zip64CentralDirEnd.centralDirEndSize);
    // PKZip 4.5
    Zip64CentralDirEnd.versionMadeBy  := Zip64Version;
    Zip64CentralDirEnd.versionNeededToExtract := Zip64Version;
    // ?-th disk
    Zip64CentralDirEnd.diskNumber     := max(FArc.VolumeNumberInfo.VolumeNumber, 0);
    Zip64CentralDirEnd.startDiskNumber := max(FArc.VolumeNumberInfo.VolumeNumber, 0);
    // number of files in CDIR on this disk
    Zip64CentralDirEnd.entriesOnDisk  := CDir.Count;
    // total number of files in CDIR
    Zip64CentralDirEnd.entriesCentralDir := CDir.Count;
    // size of central dir
    Zip64CentralDirEnd.centralDirSize := FCompressedStream.Position - startCentralDir;
    // start of central dir
    Zip64CentralDirEnd.offsetStartDir := startCentralDir;

    // write Zip64 CDir End
    FCompressedStream.WriteBuffer(Zip64CentralDirEnd, FXCZip64CentralDirEndSize);
  end;

  procedure SaveZip64CentralDirEndLocator;
  begin
    // signature
    Zip64CentralDirEndLocator.signature := Zip64CentralDirEndLocatorSignature;
    // ?-th disk
    Zip64CentralDirEndLocator.startDiskNumber :=
      max(FArc.VolumeNumberInfo.VolumeNumber, 0);
    // ?+1 disks
    Zip64CentralDirEndLocator.totalNumberOfDisks :=
      max(FArc.VolumeNumberInfo.VolumeNumber, 0) + 1;

    // write Zip64 CDir End Locator
    FCompressedStream.WriteBuffer(Zip64CentralDirEndLocator,
      FXCZip64CentralDirEndLocatorSize);
  end;

  function GetZip64ExtraFieldLength(DirItemNo: integer): integer;
  begin
    Result := 0;
    if (CDir.ItemsPtr[DirItemNo]^.CentralDir.uncompSize = $FFFFFFFF) then
      Inc(Result, 8);
    if (CDir.ItemsPtr[DirItemNo]^.CentralDir.compSize = $FFFFFFFF) then
      Inc(Result, 8);
    if (CDir.Items[DirItemNo].CentralDir.relOffsetLH = $FFFFFFFF) then
      Inc(Result, 8);
    if (CDir.Items[DirItemNo].CentralDir.diskNumberStart = $FFFF) then
      Inc(Result, 4);
    if (Result > 0) then
      Inc(Result, 4); // headerID + size
  end;

  function GetExtraFieldsLength(DirItemNo: integer): integer;
  var
    i: integer;
  begin
    Result := GetZip64ExtraFieldLength(DirItemNo);
    for i := 0 to Length(CDir.ItemsPtr[DirItemNo]^.ExtraFields) - 1 do
      if (CDir.Items[DirItemNo].ExtraFields[i].headerID <> $0001) then
        Result := Result + CDir.ItemsPtr[DirItemNo]^.ExtraFields[i].dataSize + 4;
  end;

  procedure SaveExtraFields(DirItemNo: integer);
  var
    ExtraField: TFXCExtraFieldDataBlock;
    i: integer;
  begin
    if (GetZip64ExtraFieldLength(DirItemNo) > 0) then
    begin
      // headerID + size
      ExtraField.headerID := $0001; // zip64 ext info
      ExtraField.dataSize := GetZip64ExtraFieldLength(DirItemNo) - 4;
      FArc.WriteBufferToStream(ExtraField, 4,
        FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
      // uncompsize?
      if (CDir.ItemsPtr[DirItemNo]^.CentralDir.uncompSize = $FFFFFFFF) then
        FArc.WriteBufferToStream(CDir.ItemsPtr[DirItemNo]^.Zip64ExtInfo.uncompSize, 8,
          FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
      // compsize?
      if (CDir.ItemsPtr[DirItemNo]^.CentralDir.compSize = $FFFFFFFF) then
        FArc.WriteBufferToStream(CDir.ItemsPtr[DirItemNo]^.Zip64ExtInfo.compSize, 8,
          FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
      // reloffsetLH
      if (CDir.Items[DirItemNo].CentralDir.relOffsetLH = $FFFFFFFF) then
        FArc.WriteBufferToStream(CDir.ItemsPtr[DirItemNo]^.Zip64ExtInfo.relOffsetLH, 8,
          FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
      // diskNo
      if (CDir.Items[DirItemNo].CentralDir.diskNumberStart = $FFFF) then
        FArc.WriteBufferToStream(
          CDir.ItemsPtr[DirItemNo]^.Zip64ExtInfo.diskNumberStart, 4,
          FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
    end;
    for i := 0 to Length(CDir.ItemsPtr[DirItemNo]^.ExtraFields) - 1 do
      // if Zip64 extended info - don't copy it 2nd time
      if (CDir.Items[DirItemNo].ExtraFields[i].headerID <> $0001) then
      begin
        // write extra field header
        FArc.WriteBufferToStream(CDir.ItemsPtr[DirItemNo]^.ExtraFields[i], 4,
          FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
        // write extra field data
        if (CDir.ItemsPtr[DirItemNo]^.ExtraFields[i].pData <> nil) then
          FArc.WriteBufferToStream(CDir.ItemsPtr[DirItemNo]^.ExtraFields[i].pData^,
            CDir.ItemsPtr[DirItemNo]^.ExtraFields[i].dataSize,
            FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
      end;
  end;
var
  AnsiStr: AnsiString;
begin
  // passed from BaseArchiver?
  if (Stream <> nil) then
    FCompressedStream := Stream;
  Comment := ArchiveComment;

  IsCentralDirLoaded := False;
  if (not RecreateCDirEnd) then
    if FArc.isZIPFormat then
      IsCentralDirLoaded := GetCentralDirEnd(CDirEnd, pos, ZipCentralDirEndSignature)
    else
      IsCentralDirLoaded := GetCentralDirEnd(CDirEnd, pos, ZipCentralDirEndSignature) or
        GetCentralDirEnd(CDirEnd, pos, FXCCentralDirEndSignature);

  if (not IsCentralDirLoaded) then
  begin
    // empty archive - central dir was erased
    FCompressedStream.Position := FCompressedStream.Size;
  end
  else
    FCompressedStream.Position := pos - CentralDirEnd.centralDirSize;
  ArchiveComment := Comment;

  FCompressedStream.Size := FCompressedStream.Position;
  pos := FCompressedStream.Position;

  // store CDir start disk No
  if (FArc.VolumeNumberInfo.VolumeNumber < $FFFF) then
    CentralDirEnd.startDiskNumber := max(FArc.VolumeNumberInfo.VolumeNumber, 0)
  else
  begin
    CentralDirEnd.startDiskNumber      := $FFFF;
    Zip64CentralDirEnd.startDiskNumber := FArc.VolumeNumberInfo.VolumeNumber;
  end;

  CacheStream := TMemoryStream.Create;
  try
    CentralDirOffset := FCompressedStream.Position;

    // save central dir
    for i := 0 to CDir.Count - 1 do
    begin
      // calc extra field length
      CDir.ItemsPtr[i]^.CentralDir.extraLength := GetExtraFieldsLength(i);
      // central dir
      AnsiStr := AnsiString(CDir.ItemsPtr[i]^.Name);  // fix bug when AnsiStringLen > Length(WideFileName)

      if (PatchFileNameForWrite(CDir.ItemsPtr[i]^.Name, AnsiStr)) then
         CDir.ItemsPtr[i]^.CentralDir.genPurposeFlag :=
            CDir.ItemsPtr[i]^.CentralDir.genPurposeFlag OR $0800;

      CDir.ItemsPtr[i]^.CentralDir.nameLength := Length(AnsiStr);

      FArc.WriteBufferToStream(CDir.ItemsPtr[i]^.CentralDir, FXCZipCentralDirSize,
        FCompressedStream, FArc.VolumeNumberInfo,
        CDir.ItemsPtr[i]^.CentralDir.extraLength + Length(AnsiStr),
        CacheStream);
      // name
      ///Buf := CDir.ItemsPtr[i]^.Name;
      Buf := AnsiStr;
      UniqueString(Buf);
      
      //StrPCopy(Buf, CDir.ItemsPtr[i]^.Name);
      if (FArc.Options.OEMFileNames and (CDir.Items[i].CentralDir.genPurposeFlag AND $0800 = 0)) then
        CharToOEMA(PAnsiChar(buf), PAnsiChar(buf));
      // crypting Name ?
      {$IFDEF FC_VERSION}
      if (((CentralDirEnd.signature = FXCCentralDirEndSignature) or
        (CDir.Items[i].CentralDir.extractVersion = FXCVersion)) and
        ((CDir.Items[i].CentralDir.genPurposeFlag and $0001) = 1)) then
      begin
        FXCInternalEncryptBuffer(caRijndael_256, PByte(buf), Length(AnsiStr),
          AnsiString(IntToStr(FXCCentralDirEndSignature)));
      end;
      {$ENDIF}
      FArc.WriteBufferToStream(PAnsiChar(buf)^, Length(AnsiStr),
        FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
      // save extra fields
      SaveExtraFields(i);
      // comment
      FArc.WriteBufferToStream(PAnsiChar(CDir.ItemsPtr[i]^.Comment)^,
        CDir.ItemsPtr[i]^.CentralDir.commentLength,
        FCompressedStream, FArc.VolumeNumberInfo, -1, CacheStream);
    end;
  finally
    if (CacheStream.Size > 0) then
      FArc.WriteBufferToStream(PAnsiChar(TMemoryStream(CacheStream).Memory)^, CacheStream.Size,
        FCompressedStream, FArc.VolumeNumberInfo);
    CacheStream.Free;
  end;

  // size of central dir
  CDirSize := FCompressedStream.Position - pos;

  // init CentralDirEnd
  if (CDir.Count < $FFFF) then
    CentralDirEnd.entriesOnDisk := CDir.Count
  else
    CentralDirEnd.entriesOnDisk := $FFFF;
  if (CDir.Count < $FFFF) then
    CentralDirEnd.entriesCentralDir := CDir.Count
  else
    CentralDirEnd.entriesCentralDir := $FFFF;
  if (CDirSize < $FFFFFFFF) then
    CentralDirEnd.centralDirSize := CDirSize
  else
    CentralDirEnd.centralDirSize := $FFFFFFFF;
  if (pos < $FFFFFFFF) then
    CentralDirEnd.offsetStartDir := pos
  else
    CentralDirEnd.offsetStartDir := $FFFFFFFF;
  CentralDirEnd.commentLength := Length(ArchiveComment);
  // store CDirEnd disk No
  if (FArc.VolumeNumberInfo.VolumeNumber < $FFFF) then
    CentralDirEnd.diskNumber := max(FArc.VolumeNumberInfo.VolumeNumber, 0)
  else
  begin
    CentralDirEnd.diskNumber      := $FFFF;
    Zip64CentralDirEnd.diskNumber := FArc.VolumeNumberInfo.VolumeNumber;
  end;

  //--- Zip64 records?
  if ((FCompressedStream.Position >= $FFFFFFFF) or (CDir.Count >= $FFFF)) then
  begin
    // CDir end
    SaveZip64CentralDirEnd(pos);
    // CDir end locator
    SaveZip64CentralDirEndLocator;
  end;

  // save CentralDirEnd
  FCompressedStream.WriteBuffer(CentralDirEnd, FXCZipCentralDirEndSize);

  // save comment
  if (ArchiveComment <> '') then
    FCompressedStream.WriteBuffer(PAnsiChar(ArchiveComment)^, CentralDirEnd.commentLength);

  // multi-spanning?
  if (FArc.SpanningMode <> smNone) and (FArc.VolumeFileName <> FArc.FileName) then
  begin
    // rename last file to original FileName
    FCompressedStream.Free;
    LastVolumeFileName := FArc.FileName;
    FArc.MakeDefaultVolumeName(LastVolumeFileName, -1, False);
    FXCDeleteFile(TFXCPChar(LastVolumeFileName));
    FXCRenameFile(TFXCPChar(FArc.VolumeFileName), TFXCPChar(LastVolumeFileName));
    FArc.FCompressedStream :=
      TFXCFileStream.Create(LastVolumeFileName, fmOpenReadWrite or fmShareDenyNone);
    FCompressedStream      := FArc.FCompressedStream;
  end;
end;// SaveDir


//------------------------------------------------------------------------------
// add stub offset to relative offsets
//------------------------------------------------------------------------------
procedure TFXCDirManager.ApplyStubOffset;
var
  i: integer;
begin
  for i := 0 to CDir.Count - 1 do
    if ((CDir.ItemsPtr[i]^.CentralDir.externalAttr and faDirectory) = 0) then
      Inc(CDir.ItemsPtr[i]^.CentralDir.relOffsetLH, StubSize);
end;// ApplyStubOffset


//------------------------------------------------------------------------------
// stream has CDirEnd within?
//------------------------------------------------------------------------------
function TFXCDirManager.HasCentralDirEnd: boolean;
var
  pos: int64;
begin
  pos := 0;
  if (not FArc.FOpenCorruptedArchives) then
    if (FArc.isZIPFormat) then
      Result := GetCentralDirEnd(CentralDirEnd, pos, ZipCentralDirEndSignature)
    else
      Result := GetCentralDirEnd(CentralDirEnd, pos, FXCCentralDirEndSignature)
  else
  begin
    if (FArc.isZIPFormat) then
      GetCentralDirEnd(CentralDirEnd, pos, ZipCentralDirEndSignature)
    else
      GetCentralDirEnd(CentralDirEnd, pos, FXCCentralDirEndSignature);
    Result := True;
  end;
end;// HasCentralDirEnd


//------------------------------------------------------------------------------
// has SFX stub?
//------------------------------------------------------------------------------
function TFXCDirManager.IsSFXArchive: boolean;
begin
  Result := False;
  if (CDir.Count > 0) then
    if (CDir.Items[0].CentralDir.relOffsetLH > 0) then
      Result := True;
end;// IsSFXArchive


//------------------------------------------------------------------------------
// merge with central dir of another archive
//------------------------------------------------------------------------------
procedure TFXCDirManager.MergeWith(DMHandleToAdd: TFXCDirManager);
var
  i, oldCount: Integer;
  NewRelOffsetLH: Int64;
begin
  for i := 0 to CDir.Count - 1 do
    CDir.ItemsPtr[i]^.Tagged := false;
  oldCount := CDir.Count;
  CDir.Append(DMHandleToAdd.CDir);
  for i:= 0 to CDir.Count - 1 do
    if CDir.ItemsPtr[i]^.Tagged then
    begin
      CDir.ItemsPtr[i]^.Tagged := false;
      NewRelOffsetLH := CDir.ItemsPtr[i]^.CentralDir.relOffsetLH + CentralDirOffset;
      if (NewRelOffsetLH < $FFFFFFFF) then
       CDir.ItemsPtr[i]^.CentralDir.relOffsetLH := CDir.ItemsPtr[i]^.CentralDir.relOffsetLH + CentralDirOffset
      else
        begin
          CDir.ItemsPtr[i]^.bHugeFile := True;
          CDir.ItemsPtr[i]^.CentralDir.relOffsetLH := $FFFFFFFF;
          if (CDir.ItemsPtr[i]^.CentralDir.relOffsetLH <> $FFFFFFFF) then
            CDir.ItemsPtr[i]^.Zip64ExtInfo.relOffsetLH := NewRelOffsetLH
          else
            CDir.ItemsPtr[i]^.Zip64ExtInfo.relOffsetLH := CDir.ItemsPtr[i]^.Zip64ExtInfo.relOffsetLH + CentralDirOffset;
        end;
    end;
end;// MergeWith(DMHandleToAdd


////////////////////////////////////////////////////////////////////////////////

//   TBaseArchiver

////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
// Is Archive in transaction
//------------------------------------------------------------------------------
function TBaseArchiver.GetInUpdate;
begin
  Result := (FUpdateCount > 0);
end;


//------------------------------------------------------------------------------
// set property
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetInMemory(Value: boolean);
var
  f: TFXCFileStream;
begin
  if (FInMemory <> Value) then
  begin
    if (FActive) then
    begin
      // save memory to disk?
      if (FInMemory) then
      begin
        // blank file name?
        if (FFileName = '') then
          raise EFXCException.Create(00029, Self);
        f := TFXCFileStream.Create(FFileName, fmCreate);
        try
          FCompressedStream.Position := 0;
          f.CopyFrom(FCompressedStream, FCompressedStream.Size);
        finally
          f.Free;
        end;
        CloseArchive;
        FInMemory := False;
        OpenArchive;
      end
      else
      begin
        // close, open in-memory
        CloseArchive;
        FInMemory := True;
        OpenArchive;
      end;
    end;
    FInMemory := Value;
  end;
end;


//------------------------------------------------------------------------------
// get archive comment
//------------------------------------------------------------------------------
function TBaseArchiver.GetFileComment: AnsiString;
begin
  if (FActive) then
    Result := DMHandle.ArchiveComment
  else
    Result := '';
end;// GetFileComment


//------------------------------------------------------------------------------
// set archive comment
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetFileComment(const Value: AnsiString);
begin
  CheckInactive;
  CheckModifySpanning;
  DMHandle.ArchiveComment := Value;
  if ((not InUpdate) and (SpanningMode = smNone)) then
    DMHandle.SaveDir(False);
end;// SetFileComment


//------------------------------------------------------------------------------
// SetFileMasks
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetFileMasks(Value: TFXCStrings);
begin
  FFileMasks.Assign(Value);
end;// SetFileMasks


//------------------------------------------------------------------------------
// SetExclusionMasks
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetExclusionMasks(Value: TFXCStrings);
begin
  FExclusionMasks.Assign(Value);
end;// SetExclusionMasks


//------------------------------------------------------------------------------
// SetNoCompressionMasks
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetNoCompressionMasks(Value: TFXCStrings);
begin
  FNoCompressionMasks.Assign(Value);
end;// SetNoCompressionMasks


//------------------------------------------------------------------------------
// Lock
//------------------------------------------------------------------------------
procedure TBaseArchiver.Lock;
begin
  EnterCriticalSection(FCSect);
end;// Lock


//------------------------------------------------------------------------------
// Unlock
//------------------------------------------------------------------------------
procedure TBaseArchiver.Unlock;
begin
  LeaveCriticalSection(FCSect);
end;// Unlock


//------------------------------------------------------------------------------
// check inactive
//------------------------------------------------------------------------------
procedure TBaseArchiver.CheckInactive;
begin
  if (not FActive) then
    raise EFXCException.Create(00013, Self);
end;// CheckInactive


//------------------------------------------------------------------------------
// check in-update
//------------------------------------------------------------------------------
procedure TBaseArchiver.CheckInUpdate;
begin
  if (InUpdate) then
    raise EFXCException.Create(00026, Self);
end;


//------------------------------------------------------------------------------
// CheckModifySpanning
//------------------------------------------------------------------------------
procedure TBaseArchiver.CheckModifySpanning;
begin
  if ((not InUpdate) and (DMHandle.CDir.ItemCount > 0) and
    (SpanningMode <> smNone)) then
    raise EFXCException.Create(00052, Self);
end;// CheckModifySpanning


//------------------------------------------------------------------------------
// open/close archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetActive(Value: boolean);
begin
  if (Value <> FActive) then
    if (Value) then
      OpenArchive
    else
      CloseArchive;
end;// SetActive


//------------------------------------------------------------------------------
// returns encrypted
//------------------------------------------------------------------------------
function TBaseArchiver.GetEncrypted;
begin
  Result := (FPassword <> '');
  if (FActive) then
    if (DMHandle.CDir.Count > 0) then
      Result := DMHandle.IsEncrypted;
end;


//------------------------------------------------------------------------------
// check if archive file exists
//------------------------------------------------------------------------------
function TBaseArchiver.GetExists;
begin
  Result := FXCFileExists(FileName);
end;


//------------------------------------------------------------------------------
// get archive size
//------------------------------------------------------------------------------
function TBaseArchiver.GetArchiveSize: int64;
begin
  // check inactive
  CheckInactive;

  Result := FCompressedStream.Size;
end;


//------------------------------------------------------------------------------
// Gets File Count in archive
//------------------------------------------------------------------------------
function TBaseArchiver.GetFileCount: integer;
var
  FCount: integer;
  r:      TFXCArchiveItem;
  OldRecurse: boolean;
begin
  // determine whether archive is open
  CheckInactive;
  FCount     := 0;
  OldRecurse := FOptions.Recurse;
  try
    FOptions.Recurse := True;
    if (FindFirst('*.*', r, faDirectory)) then
    begin
      if ((r.ExternalFileAttributes and faDirectory) <> 0) then
        Inc(FCount);
      while (FindNext(r)) do
        if ((r.ExternalFileAttributes and faDirectory) <> 0) then
          Inc(FCount);
    end;
    FCount := DMHandle.CDir.Count - FCount;
  finally
    FOptions.Recurse := OldRecurse;
  end;
  Result := FCount;
end;


//------------------------------------------------------------------------------
// set compression level
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetCompressionLevel(newLevel: TFXCCompressionLevel);
begin
  FCompressionLevel := newLevel;
  FCompressionMode  := InternalGetCompressionMode(newLevel);
  SetCompMethod;
end;


//------------------------------------------------------------------------------
// set compression mode
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetCompressionMode(newMode: byte);
begin
  if (newMode > 9) then
    raise EFXCException.Create(00005, Self);
  FCompressionMode  := newMode;
  FCompressionLevel := InternalGetCompressionLevel(newMode);
  SetCompMethod;
end;

{$IFDEF FC_VERSION}
//------------------------------------------------------------------------------
// GetMaxInitVectorSize
//------------------------------------------------------------------------------
function TBaseArchiver.GetMaxInitVectorSize: Integer;
begin
  Result := FXC_MAX_IVECTOR_SIZE;
end;//GetMaxInitVectorSize
{$ENDIF}

//------------------------------------------------------------------------------
// sets encryption password
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetPassword (const Value: AnsiString);
begin
  Self.FPassword := Value;
end; // GetCurrentVersionText

//------------------------------------------------------------------------------
// returns current version text
//------------------------------------------------------------------------------
function TBaseArchiver.GetCurrentVersionText: TFXCString;
var
  c: char;
begin
  c      := {$ifndef D17H}DecimalSeparator{$else}FormatSettings.DecimalSeparator{$endif};
  {$ifndef D17H}DecimalSeparator{$else}FormatSettings.DecimalSeparator{$endif} := '.';
  Result := FloatToStrF(InternalCurrentVersion, ffFixed, 3, 2) + ' ' +
    internalCurrentVersionText;
  {$ifndef D17H}DecimalSeparator{$else}FormatSettings.DecimalSeparator{$endif} := c;
end; // GetCurrentVersionText


//------------------------------------------------------------------------------
// returns current version text
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetCurrentVersionText(s: TFXCString);
begin
  s := GetCurrentVersionText;
end; // GetCurrentVersionText


//------------------------------------------------------------------------------
// SetSpanningMode
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetSpanningMode(Value: TFXCSpanningMode);
begin
  if (Active) then
    raise Exception.Create('Cannot set spanning mode on open archive');
  FSpanningMode := Value;
end;// SetSpanningMode

//------------------------------------------------------------------------------
// compress file
//------------------------------------------------------------------------------
function TBaseArchiver.InternalCompressFile(rfs, wfs: TStream; var d: TDirItem; rfsSize: int64): boolean;
var
  Count, compSize, pos: int64;
  size, cSize, offset: int64;
  OutBytes, oldSize: int64;
  buf, buf1: PByte;
  strm:      TZStreamRec;
  FBlockSize: integer;
  FEncrypted: boolean;
  FCompressionMode: byte;
  FKey:      TZipKey;
  FKeyHeader: TZipKeyHeader;
  fcrc32:    longword;
  fileHeader: TFXCFileHeader;
  {$IFDEF FC_VERSION}
  blockHeader: TFXCBlockHeader;
  {$ENDIF}
  CentralDir: TFXCZipCentralDir;

  pEncrypter : ^TAESCryptoTransform;

  

{$IFDEF FC_VERSION}

  procedure FXCInitHeader;
  var
    i,m: integer;
  begin
    FillChar(fileHeader, sizeof(fileHeader), $00);
    if (FEncrypted) then
    begin
      if (UseInitVector) then
        begin
          m := min(sizeof(fileHeader.ControlBlock), MaxInitVectorSize);
          for i := 0 to m - 1 do
            fileHeader.ControlBlock[i] := AnsiChar(InitVector[i]);
          fileHeader.UseInitVector := 1;
        end
      else
        begin
          randomize;
          for i := 0 to sizeof(fileHeader.ControlBlock) - 1 do
            fileHeader.ControlBlock[i] := AnsiChar(Chr(Random(MAXINT) mod 256));
        end;

      fileHeader.ControlBlockCrc32 := fcrc32;
      UpdateCRC32(@fileHeader.ControlBlock,
        sizeof(fileHeader.ControlBlock),
        fileHeader.ControlBlockCrc32);
      fileHeader.ControlBlockCrc32 := not fileHeader.ControlBlockCrc32;
      FXCInternalEncryptBuffer(FFXCCryptoAlgorithm, @fileHeader.ControlBlock,
        sizeof(fileHeader.ControlBlock), d.Password);
      fileHeader.EncryptionAlgorithm := word(FFXCCryptoAlgorithm);
    end
    else
      fileHeader.EncryptionAlgorithm := caNone;
    fileHeader.BlockSize := FBlockSize;
    fileHeader.CompressionAlgorithm := byte(FCompressionAlgorithm);
    FileHeader.CompressionMode      := FCompressionMode;
  end;


  // encrypt buffer
  procedure FXCEncryptBuffer;
  begin
    if (UseInitVector) then
      FXCInternalEncryptBuffer(FFXCCryptoAlgorithm, buf, cSize, d.Password, @FInitVector)
    else
      FXCInternalEncryptBuffer(FFXCCryptoAlgorithm, buf, cSize, d.Password);
  end;


  // compresses buffer
  procedure CompressBuffer;
  var
    icSize: integer;
  begin
    offset := 0;
    if (FCompressionMode = 0) then
    begin
      buf   := buf1;
      cSize := size;
    end
    else
    begin
      icSize := cSize;
      FXCInternalCompressBuffer(FCompressionAlgorithm, FCompressionMode,
        buf1, size, buf, icSize);

      cSize := icSize;
    end;
  end;

  // compresses data in custom format
  procedure FXCCompress;
  {$IFDEF FC_VERSION}
  var
    headerPos: int64;
  {$ENDIF}
  begin
{$IFDEF FC_VERSION}
    // save start position
    headerPos := wfs.Position;
    // allocate memory for block
    buf1      := AllocMem(FBlockSize);
    // init header
    FXCInitHeader;
    // write empty compressed file header

    if not WriteToStreamWithOnDiskFull(wfs, FileHeader, FXCFileHeaderSize) then
      Exit;
    Inc(compSize, FXCFileHeaderSize);
    FProgressMax := rfsSize;
    while Count < rfsSize do
    begin
      FProgress := Count;
      // call progress handlers
      // don't call progress handlers?
      if (FProgressEnabled) then
      begin
        DoOnFileProgress(d.SrcFileName, FProgress / FProgressMax * 100.0, d.Operation,
          ppProcess, FProgressCancel);
        if (FProgressCancel) then
          break;
        DoOnOverallProgress(((FProcessedFileNo + FProgress / FProgressMax) /
          FProcessedFileCount) * 100.0, d.Operation,
          ppProcess, FProgressCancel);
        if (FProgressCancel) then
          break;
      end;
      if (rfsSize - Count > FBlockSize) then
        size := FBlockSize
      else
        size := rfsSize - Count;
      if (rfs.Read(buf1^, size) <> size) then
        break;
      try
        // count crc
        UpdateCRC32(buf1, size, fcrc32);
        // compress block
        CompressBuffer;
        // encrypt compressed buffer
        if (FEncrypted) then
          FXCEncryptBuffer;
      except
        FreeMem(buf1);
        buf1 := nil;
        if (buf <> nil) then
          FreeMem(buf);
        Exit;
      end;
      // write compressed file header
      BlockHeader.packedSize := cSize;
      if not WriteToStreamWithOnDiskFull(wfs, blockHeader, FXCBlockHeaderSize) then
      begin
        if (FCompressionMode <> 0) then FreeMem(buf);
        break;
      end;
      Inc(compSize, FXCBlockHeaderSize);

      // write compressed block
      if not WriteToStreamWithOnDiskFull(wfs, PByte(buf)^, cSize) then
      begin
        if (FCompressionMode <> 0) then
          FreeMem(buf);
        break;
      end;

      if (FCompressionMode <> 0) then
        FreeMem(buf);
      Inc(compSize, cSize);
      Inc(Count, size);
      Inc(fileHeader.NumBlocks);
    end; // compress all blocks
         // free allocated block
    FreeMem(buf1);
    // writing header
    wfs.Position := headerPos;
    fileHeader.CompSize := compSize;
    fileHeader.UncompSize := rfsSize;
    fileHeader.FileCrc32 := not fcrc32;
    // write compressed block header
    if not WriteToStreamWithOnDiskFull(wfs, FileHeader, FXCFileHeaderSize) then
      Exit;
    Result := True;
{$ENDIF}
  end; //FXCCompress
{$ENDIF}

  // InternalCompress
begin
  Result := True;
  CentralDir := d.centralDir;
  // DoOnFileProgress(0);
  // result := false;
  // initialisation
  rfs.Position := 0;
  Count      := 0;
  compSize   := 0;
  fcrc32     := $FFFFFFFF;
  FCompressionMode := d.CompressionMode;
  FBlockSize := InternalGetBlockSize(FCompressionMode);

  FEncrypted := (d.Password <> '');
  

  // process compression and encryption
  if (isZIPFormat) then
    
  else
  {$IFDEF FC_VERSION}
    try
      FXCCompress;
    except
    end
  {$ENDIF};


  // finalisation
  // if all data compressed successfully
  if ((Count = rfsSize) or ((rfsSize < 0) and (longword(rfsSize) = Count)))then
  begin
    Result := True;
    //compSize: zip64 format required?
    if (compSize < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
      centralDir.compSize := compSize
    else
    begin
      d.bHugeFile := True;
      centralDir.compSize := $FFFFFFFF;
      d.Zip64ExtInfo.compSize := compSize;
    end;
    //uncompSize: zip64 format required?
    if (rfsSize < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
      centralDir.unCompSize := rfsSize
    else
    begin
      d.bHugeFile := True;
      centralDir.uncompSize := $FFFFFFFF;
      d.Zip64ExtInfo.uncompSize := rfsSize;
    end;
    centralDir.crc32 := not fcrc32;

    // huge files enabled?
    if (d.bHugeFile and (FZip64Mode = zmDisabled)) then
      raise EFXCException.Create(00041, [d.SrcFileName], self);

    // added by Leo Martin - zero length file bug
    if (rfsSize = 0) then
    begin
      // empty files should be marked as not compressed
      if (isZIPFormat) then
      begin
        d.CompressionMethod := 0;
        centralDir.compMethod := 0;
      end;
    end;
  end
  else
  if not FProgressCancel then
    raise EFXCException.Create(00034, [rfsSize, Count], self)
  else
    Result := False;

  // DoOnFileProgress(100.0);
  d.centralDir := CentralDir;

  

end; //InternalCompressFile


//------------------------------------------------------------------------------
// check zip password
//------------------------------------------------------------------------------
function TBaseArchiver.ZipInitPassword(rfs: TStream; const FPassword: AnsiString;
  genPurposeFlag: word; lastModTime: word; crc32: longword;
  var FKey: TZipKey): boolean;
var
  CRCHighByte: byte;
  FKeyHeader:  TZipKeyHeader;
begin
  rfs.ReadBuffer(FKeyHeader, sizeof(FKeyHeader));
  ZipKeyInit(FPassword, FKey, FKeyHeader);

  if (genPurposeFlag and 8) <> 0 then
    CRCHighByte := HIBYTE(LOWORD(lastModTime))
  else
    CRCHighByte := HIBYTE(HIWORD(crc32));

  if (FKeyHeader[sizeof(FKeyHeader) - 1] <> CRCHighByte) then
    Result := False // invalid password
  else
    Result := True; // right password
end; // ZipInitPassword


//------------------------------------------------------------------------------
// compress file
//------------------------------------------------------------------------------
function TBaseArchiver.InternalDecompressFile(rfs, wfs: TStream;
  var d: TDirItem; SizeToDecompress: Integer; StartPosition: Int64): boolean;
var
  Count, decompSize: int64;
  size, cSize, i: int64;
  OutBytes, oldSize: int64;
  buf, buf1: PByte;
  strm:      TZStreamRec;
  fcrc32:    longword;
  CompSize:  int64;
  UncompSize : int64;
  LFHeader:  TFXCZipFileHeader;
  fileCompMethod: word;
  FBlockSize: integer;
  FEncrypted: boolean;

  pDecrypter : ^TAESCryptoTransform;

  FInvalidPass: boolean;
  FKey:      TZipKey;
  fileHeader: TFXCFileHeader;
  blockHeader: TFXCBlockHeader;
  centralDir: TFXCZipCentralDir;
  VolumeNumberInfo: TFXCVolumeNumberInfo;

  // read from rfs with request of new volumes if necessary
  function ReadFromStream(var Buffer; Count: longint): boolean;
  var
    size: longint;
  begin
    size := rfs.Read(Buffer, Count);
    // request next volume?
    while (size < Count) do
    begin
      VolumeNumberInfo.VolumeNumber := VolumeNumberInfo.VolumeNumber + 1;
      // request volume
      OpenVolume(VolumeNumberInfo);
      // assign reopened stream
      rfs  := FCompressedStream;
      rfs.Position := 0;
      // read next portion
      size := size + rfs.Read((PAnsiChar(@Buffer) + size)^, Count - size);
      // if nothing was read - error, exit
      if (rfs.Position = 0) then
        break;
    end;
    // all read?
    Result := (size = Count);
  end;

  function EstimatedOutBytes: int64;
  var
    ComprSize, UncomprSize: int64;
  begin
    if (centralDir.CompSize = $FFFFFFFF) then
      ComprSize := d.Zip64ExtInfo.compSize
    else
      ComprSize := centralDir.CompSize;
    if (centralDir.unCompSize = $FFFFFFFF) then
      UncomprSize := d.Zip64ExtInfo.uncompSize
    else
      UncomprSize := centralDir.unCompSize;
    Result := round(power(2, round(log2((1 + UncomprSize div ComprSize) *
      ((size + (size div 2) + 12) + 255) and not 255))));
  end;

  

{$IFDEF FC_VERSION}
  // decrypt buffer
  procedure FXCDecryptBuffer(item : TDirItem);
  begin
    if UseInitVector then
      FXCInternalDecryptBuffer(TFXCCryptoAlgorithm(FileHeader.EncryptionAlgorithm),
        PByte(buf1), PByte(buf1), size, FPassword, item.UseOldRijndael, @FInitVector)
    else
      FXCInternalDecryptBuffer(TFXCCryptoAlgorithm(FileHeader.EncryptionAlgorithm),
        PByte(buf1), PByte(buf1), size, FPassword, item.UseOldRijndael);
  end;

  // ccompresses buffer
  function FXCDecompressBuffer: boolean;
  var
    icSize: integer;
  begin
    Result := False;
    if (FileHeader.CompressionMode = 0) then
    begin
      buf := buf1;
    end
    else
    begin
      icSize := cSize;
      if (not FXCInternalDecompressBuffer(
        TFXCCompressionAlgorithm(FileHeader.CompressionAlgorithm),
        PByte(buf1), integer(size), PByte(buf), icSize)) then
      begin
        if (buf <> nil) then
        begin
          FreeMem(buf);
          buf := nil;
        end;
        cSize := icSize;
        Exit;
      end;
    end;
    Result := True;
  end;


  // Decompresses data in custom format
  function FXCDecompress (item : TDirItem): boolean;
  var
{$IFDEF DELAY_ON}
    time: cardinal;
{$ENDIF}
    fh:   TFXCFileHeader;
    i:    integer;
    wfsStartPosition: Int64;
  begin
    Result := False;
    // read file header
    if (not ReadFromStream(FileHeader, FXCFileHeaderSize)) then
      Exit;
    Inc(Count, FXCFileHeaderSize);
    if (FileHeader.EncryptionAlgorithm = caNone) then
      FEncrypted := False
    else
      FEncrypted := True;
    // check password
    if (FEncrypted) then
    begin
      try
        Move(FileHeader, fh, sizeof(FileHeader));
        begin
          FXCInternalDecryptBuffer(
            TFXCCryptoAlgorithm(FileHeader.EncryptionAlgorithm),
            @fh.ControlBlock,
            @fh.ControlBlock,
            sizeof(fh.ControlBlock),
            FPassword,
            item.UseOldRijndael);
        end;
        FSkipFile := False;
        repeat
          Move(FileHeader, fh, sizeof(FileHeader));
          // real decryption of control block
          FXCInternalDecryptBuffer(
            TFXCCryptoAlgorithm(fh.EncryptionAlgorithm),
            @fh.ControlBlock,
            @fh.ControlBlock,
            sizeof(fh.ControlBlock),
            FPassword,
            item.UseOldRijndael);
          // check crc
          fcrc32 := $FFFFFFFF;
          UpdateCRC32(
            @fh.ControlBlock,
            sizeof(fh.ControlBlock),
            fcrc32);
          FInvalidPass := ((not fcrc32) <> fh.ControlBlockCrc32);
          if (FInvalidPass) then
            DoOnPassword(d.Name, FPassword, FSkipFile)
          else
            Self.FPassword := FPassword;
        until ((not FInvalidPass) or FSkipFile);
        if (FSkipFile) then
          Exit;
        fcrc32 := $FFFFFFFF;
      except
        Exit;
      end;
    end; // decrypt control block and check password

    // update Initector
    UseInitVector := (fh.UseInitVector and 1)=1;
    if (UseInitVector) then
      SetInitVector(@fh.ControlBlock, sizeof(fh.ControlBlock));

    FBlockSize := FileHeader.BlockSize;

    // decompress data
    for i := 0 to FileHeader.NumBlocks - 1 do
    begin
      FProgress := Count;
      // call progress handlers
      // don't call progress handlers?
      if (FProgressEnabled) then
      begin
        DoOnFileProgress(d.Name, FProgress / FProgressMax * 100.0, d.Operation,
          ppProcess, FProgressCancel);
        if (FProgressCancel) then
          break;
        DoOnOverallProgress(((FProcessedFileNo + FProgress / FProgressMax) /
          FProcessedFileCount) * 100.0, d.Operation,
          ppProcess, FProgressCancel);
        if (FProgressCancel) then
          break;
      end;
      // read block header
      if (not ReadFromStream(BlockHeader, FXCBlockHeaderSize)) then
      begin
        FreeMem(buf1);
        Exit;
      end;
      Inc(Count, FXCBlockHeaderSize);

      size := BlockHeader.packedSize;


      // allocate memory for block
      buf1 := AllocMem(size);

      // try to read compressed block
      if (not ReadFromStream(buf1^, size)) then
      begin
        FreeMem(buf1);
        Exit;
      end;

      if (i = FileHeader.NumBlocks - 1) then

        // zip64?
        if (d.CentralDir.unCompSize < $FFFFFFFF) then
          cSize := cardinal(FileHeader.UncompSize) - decompSize
        else
          cSize := d.Zip64ExtInfo.uncompSize - decompSize
      else
        cSize := FBlockSize;
      try
        // decrypt compressed buffer
        if (FEncrypted) then
          FXCDecryptBuffer(d);
        // Decompress block
        if (not FXCDecompressBuffer) then
        begin
          if (buf1 <> nil) then
            FreeMem(buf1);
          buf1 := nil;
          if (buf <> nil) then
            FreeMem(buf);
          buf := nil;
          Exit;
        end;
        // count crc
        UpdateCRC32(PByte(buf), cSize, fcrc32);
      except
        FreeMem(buf1);
        buf1 := nil;
        if (buf <> nil) then
          FreeMem(buf);
        Exit;
      end;

      // write decompressed block
      //if (wfs.Write(PChar(buf)^, cSize) <> cSize) then
      if not WriteToStreamWithOnDiskFull(wfs, PByte(buf)^, cSize) then
      begin
        if (FileHeader.CompressionMode <> 0) then
          FreeMem(buf);
        FreeMem(buf1);
        Exit;
      end;
      if (FileHeader.CompressionMode <> 0) then
        FreeMem(buf);
      // free allocated block
      FreeMem(buf1);
      Inc(decompSize, cSize);
      Inc(Count, size);

      // if partial decompression (fixed count from from specified position)
      if (decompSize < StartPosition) then
        wfs.Position := 0
      else
      if (decompSize-cSize < StartPosition) then
        wfsStartPosition := cSize - (decompSize-StartPosition);

      if (sizeToDecompress > 0) and (decompSize >= StartPosition+sizeToDecompress) then
        begin
          if (StartPosition > 0) then
            wfs.Position := wfsStartPosition;
          break;
        end;

    end; // Decompress all blocks
    Result := True;
  end; //FXCDecompress

{$ENDIF}

  // InternalDecompress
begin
  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  Result := False;
  try
    // initialisation
    centralDir := d.CentralDir;
    FEncrypted := (centralDir.genPurposeFlag and 1) = 1;
    

    Count      := 0;
    decompSize := 0;
    // get volume number
    if (d.CentralDir.diskNumberStart = $FFFF) then
      if (DMHandle.FZip64) then
        VolumeNumberInfo.VolumeNumber := d.Zip64ExtInfo.diskNumberStart
      else
        VolumeNumberInfo.VolumeNumber := 0
    else
      VolumeNumberInfo.VolumeNumber := d.CentralDir.diskNumberStart;
    if (DMHandle.CentralDirEnd.diskNumber = $FFFF) then
      if (DMHandle.FZip64) then
        VolumeNumberInfo.LastVolumeNumber :=
          DMHandle.Zip64CentralDirEndLocator.totalNumberOfDisks - 1
      else
        VolumeNumberInfo.LastVolumeNumber := 0
    else
      VolumeNumberInfo.LastVolumeNumber := DMHandle.CentralDirEnd.diskNumber;

    rfs.ReadBuffer(LFHeader, FXCZipFileHeaderSize);
    rfs.Seek(LFHeader.nameLength + LFHeader.extraLength, soFromCurrent);

    fcrc32 := $FFFFFFFF;
    // Zip64?
    // Thanks to HEIKO: Start HEIKO 05.06.2018 - correct UnCompSize of a huge file
//    if (centralDir.CompSize = $FFFFFFFF) then
//    begin
//      CompSize := d.Zip64ExtInfo.compSize;
//      UncompSize := d.Zip64ExtInfo.uncompSize;
//    end
//    else
//    begin
//      CompSize := centralDir.CompSize;
//      UncompSize := centralDir.UncompSize;
//    end;
//
    if (centralDir.CompSize = $FFFFFFFF) then
      CompSize := d.Zip64ExtInfo.compSize
    else
      CompSize := centralDir.CompSize;
    if (centralDir.UnCompSize = $FFFFFFFF) then
      UncompSize := d.Zip64ExtInfo.uncompSize
    else
      UncompSize := centralDir.UncompSize;
    // End HEIKO

    // fix for wrong compressed size
    if (FSpanningMode = smNone) then
      if (rfs.Position + CompSize > DMHandle.CentralDirOffset) then
        CompSize := DMHandle.CentralDirOffset - rfs.Position;

    if (SizeToDecompress + StartPosition = 0) then
      FProgressMax := UncompSize
    else
      FProgressMax := SizeToDecompress + StartPosition;

    // fix for the problem with showing progress for empty files (division by zero)
    if (FProgressMax = 0) then
       FProgressMax := 2;
    fileCompMethod := d.CompressionMethod;
    FSkipFile      := False;

    // decompress
    if (fileCompMethod = ZIP_None) or (fileCompMethod = ZIP_ZLIB) then
    
    else
    if (fileCompMethod = ZIP_Deflate64) then
      raise EFXCException.Create(00055, nil)
    else
    if (centralDir.extractVersion > 10) then
    {$IFDEF FC_VERSION}
      Result := FXCDecompress(d)
    {$ENDIF}
    else
      Result := False;

    // bug-fix: check whether all data extracted
    // Zip64?
    if (sizeToDecompress = 0) then
      if (centralDir.unCompSize = $FFFFFFFF) then
      begin
        if (decompSize <> d.Zip64ExtInfo.unCompSize) then
          Result := False;
      end
      else
      if (decompSize <> centralDir.unCompSize) then
        Result := False;

    if (not Result) then
      Exit;
    // if all data decompressed successfully
    if (not FSkipFile) and (sizeToDecompress = 0) then
    begin

      if (d.CentralDir.extractVersion < FXCVersion) then
      
    end
    else
      Result := True;
  finally
    VolumeNumberInfo.Free;
    if (d.CentralDir.extractVersion < FXCVersion) then
    
  end;
end; //InternalDecompressFile




procedure TBaseArchiver.SetCompMethod;
begin
  Self.FCompressionMethod := $FFFF;
end;


//------------------------------------------------------------------------------
// convert to standard const
//------------------------------------------------------------------------------
function TBaseArchiver.ShareModeToInt(ShareMode: TFXCFileShareMode): integer;
begin
  Result := 0;
  case ShareMode of
    smShareCompat:
      Result := fmShareCompat;
    smShareExclusive:
      Result := fmShareExclusive;
    smShareDenyWrite:
      Result := fmShareDenyWrite;
    smShareDenyRead:
      Result := fmShareDenyRead;
    smShareDenyNone:
      Result := fmShareDenyNone;
  end;
end;// ShareModeToInt



//------------------------------------------------------------------------------
// force save changes
//------------------------------------------------------------------------------
procedure TBaseArchiver.ForceUpdate;
var
  i, chgItemNo: integer;
  bfs:    TFXCFileStream;
  TempFileName: TFXCString;
  BackupOffset: int64;
  FirstItemNo: integer;
  Action: TFXCAction;
  bOK:    boolean;
  Operation: TFXCProcessOperation;
  CacheStream: TMemoryStream;
  Cancel: boolean;

  function GetIndexOfFirstChange: integer;
  var
    i, Count: integer;
  begin
    if (DMHandle.RBCDir.Count < DMHandle.CDir.Count) then
      Count := DMHandle.RBCDir.Count
    else
      Count := DMHandle.CDir.Count;
    Result := Count;
    for i := 0 to Count - 1 do
      if (DMHandle.RBCDir.Items[i].Name <> DMHandle.CDir.Items[i].Name) or
        (DMHandle.CDir.Items[i].Modified) then
      begin
        Result := i;
        break;
      end;
  end;

  procedure BackupFileRest(ItemNo: integer);
  var
    Offset: int64;
  begin
    TempFileName := Self.GetTempFileName;

    // create temp file
    bfs := TFXCFileStream.Create(TempFileName, fmCreate);

    //-- calc position in original file to backup from
    // zip64?
    if (DMHandle.RBCDir.Items[ItemNo].CentralDir.relOffsetLH = $FFFFFFFF) then
      Offset := DMHandle.RBCDir.Items[ItemNo].Zip64ExtInfo.relOffsetLH
    else
      Offset := DMHandle.RBCDir.Items[ItemNo].CentralDir.relOffsetLH;
    // backup
    FCompressedStream.Seek(Offset, soFromBeginning);
    bfs.CopyFrom(FCompressedStream, FCompressedStream.Size - Offset);
  end;

  procedure DeleteFileRest;
  begin
    // close temp file
    if (bfs <> nil) then
    begin
      bfs.Free;
      bfs := nil;
    end;
    // delete temp file
    FXCDeleteFile(TFXCPChar(TempFileName));
  end;

  procedure RestoreFileRest(ItemNo: integer);
  begin
    if (bfs = nil) then
      EFXCException.Create(00006, Self);
    // seek to backup position
    FCompressedStream.Position := BackupOffset;
    // truncate
    FCompressedStream.Size := FCompressedStream.Position;
    // seek to 0 in backup file
    bfs.Position := 0;
    // restore
    FCompressedStream.CopyFrom(bfs, bfs.Size);
    // delete temp file
    DeleteFileRest;
  end;


  procedure AddFromFileRest(ItemNo: Integer; bfs: TStream; BackupOffset: int64);
  var
    i, RBItemNo, extraNo: integer;
    DataOffset: int64;
    extDataSize, DataSize: int64;
    LFHeader:    TFXCZipFileHeader;
    ArcFileName: TFXCString;
    AnsiFileName: AnsiString;
    FileNameLen: longword;
    toSeek: Word;
    ExtraField: TFXCExtraFieldDataBlock;
    buf: array [0..999] of AnsiChar;
    Int64Value: Int64;
  begin
    RBItemNo := -1;
    // get according ItemNo in RBCDir
    for i := 0 to DMHandle.RBCDir.Count - 1 do
      if ((DMHandle.RBCDir.Items[i].Name = DMHandle.CDir.Items[ItemNo].Name) or
        ((DMHandle.CDir.Items[ItemNo].Operation = poRename) and (DMHandle.CDir.Items[ItemNo].Modified) and
        (DMHandle.RBCDir.Items[i].Name = DMHandle.CDir.Items[ItemNo].OldName))) then
      begin
        RBItemNo := i;
        break;
      end;
    if (RBItemNo = -1) then
      // throw new  ZFException(00007, this);
      raise EFXCException.Create(00007, Self);

    // failed update?
    if (DMHandle.CDir.Items[ItemNo].Operation = poUpdate) then
      // restore original
      DMHandle.CDir.Items[ItemNo] := DMHandle.RBCDir.Items[RBItemNo];

    // calc offset in backup file
    if (DMHandle.RBCDir.Items[RBItemNo].CentralDir.relOffsetLH = $FFFFFFFF) then
      DataOffset := DMHandle.RBCDir.Items[RBItemNo].Zip64ExtInfo.relOffsetLH - BackupOffset
    else
      DataOffset := DMHandle.RBCDir.Items[RBItemNo].CentralDir.relOffsetLH - BackupOffset;

    // calc size   added by Michael Selkin
    DataSize := 0;
    for i := Low(DMHandle.RBCDir.Items[RBItemNo].ExtraFields) to High(DMHandle.RBCDir.Items[RBItemNo].ExtraFields) do
    begin
      DataSize := DataSize + DMHandle.RBCDir.Items[RBItemNo].ExtraFields[i].dataSize + 4; // adding header size of extra field (4 bytes)
    end;
    // load local header to check extra fileds that could be missed in central dir
    bfs.Seek(DataOffset, soFromBeginning);
    // read header in backup file
    bfs.Read(LFHeader, FXCZipFileHeaderSize);
//    if (LFHeader.extraLength > DataSize) then // 5.05 fix to avoid copying extra fields that exist only in central dir
      DataSize := LFHeader.extraLength;

    DataSize := FXCZipFileHeaderSize +
      DMHandle.RBCDir.Items[RBItemNo].CentralDir.nameLength +
      DataSize + // Modified: earlier the size of the central directory extra fields were used, however in central directory the Zip64 extra field is larger
      DMHandle.RBCDir.Items[RBItemNo].CentralDir.commentLength;

    // Zip64?
    if (DMHandle.RBCDir.Items[RBItemNo].CentralDir.compSize = $FFFFFFFF) then
    begin
      Inc(DataSize, DMHandle.RBCDir.Items[RBItemNo].Zip64ExtInfo.compSize);
    end
    else
      Inc(DataSize, DMHandle.RBCDir.Items[RBItemNo].CentralDir.compSize);
    // store offset

    // if huge file
    if (FCompressedStream.Position < $FFFFFFFF) then
    begin
      DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir.relOffsetLH := FCompressedStream.Position; //@TODO!

      // if Zip64
      if (DMHandle.RBCDir.Items[RBItemNo].CentralDir.uncompSize = $FFFFFFFF) then
      begin
        DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir.relOffsetLH := $FFFFFFFF;
        DMHandle.CDir.ItemsPtr[ItemNo]^.Zip64ExtInfo.relOffsetLH := FCompressedStream.Position;
      end;
    end
    else
    begin
      DMHandle.CDir.ItemsPtr[ItemNo]^.bHugeFile := true;
      DMHandle.CDir.ItemsPtr[ItemNo].CentralDir.relOffsetLH := $FFFFFFFF;
      DMHandle.CDir.ItemsPtr[ItemNo].Zip64ExtInfo.relOffsetLH := FCompressedStream.Position;
    end;

    // seek in backup file
    bfs.Seek(DataOffset, soFromBeginning);

    // copy data (or also modify it)
    if ((DMHandle.CDir.Items[ItemNo].Modified and
        (DMHandle.CDir.Items[ItemNo].Operation = poRename))) then
    begin
        // read header in backup file
        bfs.Read(LFHeader, FXCZipFileHeaderSize);
        // skip name in backup file
        bfs.Seek(DMHandle.RBCDir.Items[RBItemNo].CentralDir.nameLength, soFromCurrent);

        //----- skip old extra fields in backup file
        toSeek := LFHeader.extraLength;

        bfs.Seek(toSeek, soFromCurrent);

        // modify header (file name)
        ArcFileName := DMHandle.CDir.Items[ItemNo].Name;
        FileNameLen := Length(ArcFileName);
        LFHeader.nameLength := FileNameLen;
        // modify header (extras length)
        LFHeader.extraLength := 0;
        for i := Low(DMHandle.CDir.Items[ItemNo].ExtraFields) to High(DMHandle.CDir.Items[ItemNo].ExtraFields) do
        begin
          if DMHandle.CDir.Items[ItemNo].ExtraFields[i].headerID <> $0001 then
            LFHeader.extraLength :=  LFHeader.extraLength + DMHandle.CDir.Items[ItemNo].ExtraFields[i].dataSize + 4; // adding header size of extra field (4 bytes)
        end;

        if (DMHandle.RBCDir.Items[RBItemNo].CentralDir.uncompSize >= $FFFFFFFF) then
        begin
          LFHeader.extraLength := LFHeader.extraLength + 20; // Add the size of the zip64 extra field (20 for local header)
        end;

        // save modified header
        FCompressedStream.Write(LFHeader, FXCZipFileHeaderSize);
         // save new filename

        if (PatchFileNameForWrite(ArcFileName, AnsiFileName)) then
         DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir.genPurposeFlag :=
            DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir.genPurposeFlag OR $0800;
        StrPCopy(Buf, AnsiFileName);
        if (Options.OEMFileNames and (DMHandle.CDir.Items[ItemNo].CentralDir.genPurposeFlag AND $0800 = 0)) then
          CharToOEMA(buf, buf);
        // crypting Name ?
        {$IFDEF FC_VERSION}
        if (((DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature) or
          (DMHandle.CDir.Items[ItemNo].CentralDir.extractVersion = FXCVersion)) and
          ((DMHandle.CDir.Items[ItemNo].CentralDir.genPurposeFlag and $0001) = 1)) then
        begin
          FXCInternalEncryptBuffer(caRijndael_256, @buf, Length(AnsiFileName),
           AnsiString(IntToStr(FXCCentralDirEndSignature)));
        end;
        {$ENDIF}
        FCompressedStream.Write(buf[0], Length(AnsiFileName));
        ///// save extra fields /////
        // write Zip64 extra field
        if (DMHandle.RBCDir.Items[RBItemNo].CentralDir.uncompSize = $FFFFFFFF) then
        begin
          ExtraField.headerID := $0001; // zip64 ext info
          ExtraField.dataSize := 8 + 8; // uncomp size && compsize
          // header+size
          FCompressedStream.Write(ExtraField, 4);
          // uncomp size
          Int64Value := DMHandle.CDir.Items[ItemNo].Zip64ExtInfo.uncompSize;
          FCompressedStream.Write(Int64Value, 8);
          // comp size - its value is not defined yet - just reserve space
          if (DMHandle.CDir.Items[ItemNo].Zip64ExtInfo.compSize <> 0) then
             Int64Value := DMHandle.CDir.Items[ItemNo].Zip64ExtInfo.compSize
          else
             Int64Value := DMHandle.RBCDir.Items[RBItemNo].CentralDir.compSize;
          FCompressedStream.Write(Int64Value, 8);
        end;


        for i := 0 to Length(DMHandle.CDir.ItemsPtr[ItemNo]^.ExtraFields) - 1 do
        // if Zip64 extended info - don't copy it 2nd time
        if (DMHandle.CDir.Items[ItemNo].ExtraFields[i].headerID <> $0001) then
        begin
          // write extra field header
          FCompressedStream.Write(DMHandle.CDir.ItemsPtr[ItemNo]^.ExtraFields[i], 4);
          // write extra field data
          if (DMHandle.CDir.ItemsPtr[ItemNo]^.ExtraFields[i].pData <> nil) then
            FCompressedStream.Write(DMHandle.CDir.ItemsPtr[ItemNo]^.ExtraFields[i].pData^,
              DMHandle.CDir.ItemsPtr[ItemNo]^.ExtraFields[i].dataSize);
        end;

        // copy compressed file data from backup archive
        DataSize :=
            DMHandle.RBCDir.Items[RBItemNo].CentralDir.commentLength;
        // zip64?
        if (DMHandle.RBCDir.Items[RBItemNo].CentralDir.compSize = $FFFFFFFF) then
            Inc(DataSize, DMHandle.RBCDir.Items[RBItemNo].Zip64ExtInfo.compSize)
        else
            Inc(DataSize, DMHandle.RBCDir.Items[RBItemNo].CentralDir.compSize);
        // copy rest portion
        FCompressedStream.CopyFrom(bfs, DataSize);
    end
    else
        FCompressedStream.CopyFrom(bfs, DataSize);
  end;


  function AddFromNewSource(ItemNo: integer): boolean;
  var
    rfs:      TStream;
    rfsSize:  int64;
    LFHeader: TFXCZipFileHeader;
    ExtraField: TFXCExtraFieldDataBlock;
    ArcFileName: TFXCString;
    FileNameLen: longword;
    DirIndex: integer;
    OldPosition: int64;
    compSize, uncompSize: int64;
    buf: AnsiString;
    wStream:  TStream;
    LFHeaderSize: integer;
    tmpFileName: TFXCString;
    CurStreamPos: int64;

    procedure CloseFileStream;
    begin
      // close file / stream?
      if (DMHandle.CDir.ItemsPtr[DirIndex]^.Stream = nil) then
      begin
        if (rfs <> nil) then
        begin
          rfs.Free;
          rfs := nil;
        end;
      end
      else
      if (DMHandle.CDir.ItemsPtr[DirIndex]^.bDestroyStream) then
      begin
        DMHandle.CDir.ItemsPtr[DirIndex]^.Stream.Free;
        DMHandle.CDir.ItemsPtr[DirIndex]^.Stream := nil;
      end
      else
        rfs.Position := OldPosition;
    end;

  var
    AnsiStr: AnsiString;
    AesExtraLen : Integer;
    crypter : TAESCryptoTransform;

    AesExtraField : TFXCExtraFieldDataBlock;
  {$IFDEF FXCUNICODE}
    UnicodeExtra: TUnicodeExtendedInfo;
    UnicodeExtraLen: Integer;
    UnicodeDataLen: Word;
    UnicodeFileNameLen: Integer;
    UnicodeExtraFieldDataBlock: TFXCExtraFieldDataBlock;
    P: PByte;
    WBuf: WideString;
  {$ENDIF}
  begin
    Result := False;
    // setup variables
    SetCompMethod;
    DirIndex := ItemNo;
    // progress
    if ((DMHandle.CDir.Items[DirIndex].Operation = poAdd) or
      (DMHandle.CDir.Items[DirIndex].Operation = poMove)) then
      DoOnFileProgress(DMHandle.CDir.Items[DirIndex].SrcFileName, 0,
        DMHandle.CDir.Items[DirIndex].Operation, ppStart, FProgressCancel)
    else
      DoOnFileProgress(DMHandle.CDir.Items[DirIndex].Name, 0,
        DMHandle.CDir.Items[DirIndex].Operation, ppStart, FProgressCancel);
    if (FProgressCancel) then
      exit;

    try
      ArcFileName := DMHandle.CDir.ItemsPtr[DirIndex]^.Name;
      AnsiStr := AnsiString(ArcFileName);

       if (PatchFileNameForWrite(ArcFileName, AnsiStr)) then
         DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.genPurposeFlag :=
            DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.genPurposeFlag OR $0800;

      FileNameLen := Length(AnsiStr);
      DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile := False;

      OldPosition := 0;
      // open source
      if (DMHandle.CDir.ItemsPtr[DirIndex]^.Stream = nil) then
      begin
        if ((DMHandle.CDir.Items[ItemNo].CentralDir.externalAttr and
          faDirectory) = 0) then
        begin
          if (Length(DMHandle.CDir.Items[ItemNo].SrcFileName) < MAX_PATH) Or (Pos('\\?\', DMHandle.CDir.Items[ItemNo].SrcFileName) > 0) then
          begin
            try
              rfs := TFXCFileStream.Create(DMHandle.CDir.Items[ItemNo].SrcFileName,
                fmOpenRead or ShareModeToInt(FOptions.ShareMode));
              rfsSize := rfs.Size;
            except
              // cannot open file
              raise EFXCException.Create(00033,
                [DMHandle.CDir.Items[ItemNo].SrcFileName], Self)
            end;
          end
          else
          begin
            raise EFXCException.Create(00061,
              [DMHandle.CDir.Items[ItemNo].SrcFileName], Self);
          end;
        end
        else
          rfs := nil;
      end
      else
      begin
        rfs := DMHandle.CDir.ItemsPtr[DirIndex]^.Stream;
        rfsSize := rfs.Size;
        OldPosition := rfs.Position;
        rfs.Position := DMHandle.CDir.ItemsPtr[DirIndex]^.Position;
      end;

      // update central dir
      if (FCompressedStream.Position < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
        DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.relOffsetLH :=
          FCompressedStream.Position
      else
      begin
        DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile := True;
        DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.relOffsetLH := $FFFFFFFF;
        DMHandle.CDir.ItemsPtr[DirIndex]^.Zip64ExtInfo.relOffsetLH :=
          FCompressedStream.Position;
      end;

      // filling file header
      FillChar(LFHeader, FXCZipFileHeaderSize, $00);
      if DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature then
        LFHeader.signature := FXCFileHeaderSignature
      else
        LFHeader.signature := ZipFileHeaderSignature;
      if (DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile or
        ((rfs <> nil) and (rfsSize >= $FFFFFFFF))) then
      begin
        if DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature then
          LFHeader.extractVersion := FXC64Version
        else
          LFHeader.extractVersion := Zip64Version;
      end
      else
      if DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature then
        LFHeader.extractVersion := FXCVersion
      else
      if (FOptions.OEMFileNames) then
        LFHeader.extractVersion := ZipVersion
      else
        LFHeader.extractVersion := ZipVersionNoOEM;
      DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.extractVersion :=
        LFHeader.extractVersion;
      DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.versionMadeBy :=
        LFHeader.extractVersion;
      LFHeader.genPurposeFlag :=
        DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.genPurposeFlag;

      if ((DMHandle.CentralDirEnd.signature <> FXCCentralDirEndSignature) and (DMHandle.CDir.Items[DirIndex].CentralDir.extractVersion < FXCVersion)) then
      
            LFHeader.compMethod := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compMethod
      else
          LFHeader.compMethod := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compMethod;
      LFHeader.lastModTime := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.lastModTime;
      LFHeader.lastModDate := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.lastModDate;
      // LFHeader.crc32 := $FFFFFFFF;
      LFHeader.crc32    := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.crc32;
      LFHeader.compSize := 0;
      if (rfs <> nil) then
      begin
        // zip64 format required?
        if (rfsSize < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
          LFHeader.unCompSize := rfsSize
        else
        begin
          DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile := True;
          LFHeader.unCompSize := $FFFFFFFF;
        end;
        if (rfsSize >= 0) then
          uncompSize := rfsSize
        else
          uncompSize := longword(rfsSize);
      end
      else
      begin
        LFHeader.unCompSize := 0;
        uncompSize := 0;
      end;
      LFHeader.nameLength := FileNameLen;

      // Zip64 format disabled but file is too large?
      if (FZip64Mode = zmDisabled) and (DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile) then
      begin
        CloseFileStream;
        raise EFXCException.Create(00043, Self);
      end;

      // if multi-spanning then write to temp stream
      // 2.71 disable temp files creation for splitting
      if (FSpanningMode <> smNone) then
      begin
        tmpFileName := Self.GetTempFileName;
        wStream     := TFXCFileStream.Create(tmpFileName, fmCreate);
      end
      else
        wStream := FCompressedStream;
      try
        AnsiStr := AnsiString(ArcFileName);
       if (PatchFileNameForWrite(ArcFileName, AnsiStr)) then
         DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.genPurposeFlag :=
            DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.genPurposeFlag OR $0800;

        // write header with request of new volume if necessary
        wStream.Write(LFHeader, FXCZipFileHeaderSize);

        buf := AnsiStr;
        UniqueString(buf);

        if (FOptions.OEMFileNames and (DMHandle.CDir.Items[DirIndex].CentralDir.genPurposeFlag and $0800 = 0)) then
          CharToOEMA(PAnsiChar(buf), PAnsiChar(buf));

        {$IFDEF FC_VERSION}
        // crypting file Name ?
        if (((DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature) or
          (DMHandle.CDir.Items[DirIndex].CentralDir.extractVersion = FXCVersion)) and
          ((DMHandle.CDir.Items[DirIndex].CentralDir.genPurposeFlag and $0001) = 1)) then
        begin
          // encrypt file name
          FXCInternalEncryptBuffer(caRijndael_256, PByte(buf), Length(AnsiStr),
            AnsiString(IntToStr(FXCCentralDirEndSignature)));
        end;
        {$ENDIF}

        // write file name
        wStream.Write(PByte(buf)^, FileNameLen);
        
        // calc LFH size
        LFHeaderSize := FXCZipFileHeaderSize + FileNameLen;
        // reserve space for extra field
        if (LFHeader.unCompSize = $FFFFFFFF) then
        begin
          LFHeader.extraLength := 4 + 8 + 8;
          wStream.Seek(LFHeader.extraLength, soFromCurrent);
          Inc(LFHeaderSize, LFHeader.extraLength);
        end;

        {$IFDEF FXCUNICODE}
        // write Unicode file name extra field
	if (FUnicodeFilenames) then
        begin
          // header id + data size + signature + name length + filename
          UnicodeExtraLen := 2 + 2 + 4 + 4 + Length(ArcFileName) * 2;
          // update local header
          LFHeader.extraLength := LFHeader.extraLength + UnicodeExtraLen;
          LFHeaderSize := LFHeaderSize + UnicodeExtraLen;
          // header id + data size + signature + name length + filename
          wStream.Write(UnicodeExtraFieldHeaderID, SizeOf(UnicodeExtraFieldHeaderID));
          // data length = file name length + signature length + name length
          UnicodeDataLen := SizeOf(UnicodeAdditionalSignature) + SizeOf(UnicodeFileNameLen) + Length(ArcFileName) * 2;
          wStream.Write(UnicodeDataLen, SizeOf(UnicodeDataLen));
          wStream.Write(UnicodeAdditionalSignature, SizeOf(UnicodeAdditionalSignature));
          UnicodeFileNameLen := Length(ArcFileName);
          wStream.Write(UnicodeFileNameLen, SizeOf(UnicodeFileNameLen));
          WBuf := ArcFileName;
          UniqueString(WBuf);
          {$IFDEF FC_VERSION}
          // crypting file Name ? (unicode extra field)
          if (((DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature) or
            (DMHandle.CDir.Items[DirIndex].CentralDir.extractVersion = FXCVersion)) and
            ((DMHandle.CDir.Items[DirIndex].CentralDir.genPurposeFlag and $0001) = 1)) then
          begin
            // encrypt file name
            FXCInternalEncryptBuffer(caRijndael_256, PByte(WBuf), Length(WBuf) * 2,
              AnsiString(IntToStr(FXCCentralDirEndSignature)));

          end;
          {$ENDIF}

          wStream.Write(PWideChar(WBuf)^,  Length(WBuf)*2);
          // add unicode extra field to central dir
          SetLength(DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields, Length(DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields) + 1);
          DMHandle.CDir.Items[DirIndex].ExtraFields[Length(DMHandle.CDir.Items[DirIndex].ExtraFields) - 1].headerID := UnicodeExtraFieldHeaderID;
          DMHandle.CDir.Items[DirIndex].ExtraFields[Length(DMHandle.CDir.Items[DirIndex].ExtraFields) - 1].dataSize := UnicodeDataLen;
          DMHandle.CDir.Items[DirIndex].ExtraFields[Length(DMHandle.CDir.Items[DirIndex].ExtraFields) - 1].pData := AllocMem(SizeOf(Integer) + SizeOf(Integer) + Length(ArcFileName) * 2);
          P := DMHandle.CDir.Items[DirIndex].ExtraFields[Length(DMHandle.CDir.Items[DirIndex].ExtraFields) - 1].pData;
          Move(UnicodeAdditionalSignature, P^, SizeOf(UnicodeAdditionalSignature));
          Inc(P, 4);
          Move(UnicodeFileNameLen, P^, SizeOf(UnicodeFileNameLen));
          Inc(P, 4);
          Move(PWideChar(WBuf)^, P^, UnicodeFileNameLen*2);
        end;

        {$ENDIF}

        if ((DMHandle.CentralDirEnd.signature <> FXCCentralDirEndSignature) and (DMHandle.CDir.Items[DirIndex].CentralDir.extractVersion < FXCVersion)) then
        begin
        
        end;

        // write compressed data
        if (rfs <> nil) then
          if (InternalCompressFile(rfs, wStream, DMHandle.CDir.ItemsPtr[DirIndex]^, rfsSize)) then
          begin
            // file with size > 0
            LFHeader.crc32 := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.crc32;
            // zip64 format required?
            if (DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compSize < $FFFFFFFF) and
              (FZip64Mode <> zmAlways) then
              LFHeader.compSize := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compSize
            else
              LFHeader.compSize := $FFFFFFFF;
            if (DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compSize < $FFFFFFFF) then
              compSize := DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compSize
            else
              compSize := DMHandle.CDir.ItemsPtr[DirIndex]^.Zip64ExtInfo.compSize;
            // added by Leo Martin - zero length file bug
            if (rfsSize = 0) then
            begin
              if (isZIPFormat) then
                LFHeader.compMethod := 0;
            end;
            // seek to the start of LFHeader
            if (FSpanningMode <> smNone) then
              wStream.Position := 0
            else
            if (DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.relOffsetLH <
              $FFFFFFFF) then
              wStream.Position :=
                DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.relOffsetLH
            else
              wStream.Position :=
                DMHandle.CDir.ItemsPtr[DirIndex]^.Zip64ExtInfo.relOffsetLH;
            // rewrite header
            wStream.Write(LFHeader, FXCZipFileHeaderSize);
            // skip file name
            wStream.Seek(FileNameLen, soFromCurrent);
            // write zip64 extra field
            if (LFHeader.unCompSize = $FFFFFFFF) then
            begin
              ExtraField.headerID := $0001; // zip64 ext info
              ExtraField.dataSize := 8 + 8; // uncomp size and compsize
              // header+size
              wStream.Write(ExtraField, 4);
              // uncomp size
              wStream.Write(uncompSize, 8);
              // comp size - its value is not defined yet - just reserve space
              wStream.Write(compSize, 8);
            end;
          end;
        if FProgressCancel then
        begin
          Result := False;
          CloseFileStream;
          Exit;
        end;
        wStream.Seek(0, SoFromEnd);
        // write temp stream to volumes?
        if (FSpanningMode <> smNone) then
        begin
          // write LF header
          wStream.Position := 0;
          WriteToStream(wStream, FCompressedStream, VolumeNumberInfo, Cancel,
            LFHeaderSize, 0, CacheStream);
          // store start disk No
          DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.diskNumberStart :=
            VolumeNumberInfo.VolumeNumber;

          if (CacheStream = nil) then
            CurStreamPos := FCompressedStream.Position
          else
            CurStreamPos := CacheStream.Position;

          // store start offset
          if (CurStreamPos < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
            DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.relOffsetLH :=
              CurStreamPos - LFHeaderSize
          else
          begin
            DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile := True;
            DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.relOffsetLH := $FFFFFFFF;
            DMHandle.CDir.ItemsPtr[DirIndex]^.Zip64ExtInfo.relOffsetLH :=
              CurStreamPos - LFHeaderSize;
          end;
          // write data
          WriteToStream(wStream, FCompressedStream, VolumeNumberInfo,
            Cancel, -1, -1, CacheStream, True);
        end;

      finally
        //FreeMem(buf);
        // free temp stream
        if (FSpanningMode <> smNone) then
        begin
          wStream.Free;
          FXCDeleteFile(TFXCPChar(tmpFileName));
        end;
      end;

      CloseFileStream;

      // progress
      if ((DMHandle.CDir.Items[DirIndex].Operation = poAdd) or
        (DMHandle.CDir.Items[DirIndex].Operation = poMove)) then
        DoOnFileProgress(DMHandle.CDir.Items[DirIndex].SrcFileName, 100,
          DMHandle.CDir.Items[DirIndex].Operation, ppEnd, FProgressCancel)
      else
        DoOnFileProgress(DMHandle.CDir.Items[DirIndex].Name, 100,
          DMHandle.CDir.Items[DirIndex].Operation, ppEnd, FProgressCancel);

      // OK ?
      if (not Cancel) then
        Result := True
      else
        Action := fxaAbort;

    except
      on E: EFXCException do
      begin
        CloseFileStream;
        // disk was full and cancelled?
        if (FCompressedStream = nil) then
          Action := fxaAbort
        else
        if (DMHandle.CDir.ItemsPtr[DirIndex]^.Operation = poUpdate) then
          DoOnProcessFileFailure(DMHandle.CDir.ItemsPtr[DirIndex]^.Name,
            DMHandle.CDir.ItemsPtr[DirIndex]^.Operation,
            E.NativeError, E.ErrorCode, E.Message, Action)
        else
          DoOnProcessFileFailure(DMHandle.CDir.ItemsPtr[DirIndex]^.SrcFileName,
            DMHandle.CDir.ItemsPtr[DirIndex]^.Operation,
            E.NativeError, E.ErrorCode, E.Message, Action);
      end
      else
      begin
        CloseFileStream;
        raise;
      end;
    end;

  end;// AddFromNewSource

  procedure Rollback;
  begin
    // copy RBCDir to CDir
    DMHandle.CDir.Assign(DMHandle.RBCDir);
    // aborted multi-spanning - exit
    if (FCompressedStream <> nil) then
    begin
      // restore rest of the file
      if (bfs <> nil) then
        RestoreFileRest(chgItemNo)
      else
      begin
        FCompressedStream.Size := BackupOffset;
        DMHandle.FCompressedStream := FCompressedStream;
        DMHandle.SaveDir(True);
      end;
    end;
  end;

begin
  if (FUpdateCount > 0) then
  begin
    FUpdateCount := 0;
    bfs := nil;

    // for total/file progress
    FProcessedFileNo := 0;
    FProcessedFileCount := 0;
    FProgressCancel := False;
    FProgressEnabled := True;
    FirstItemNo := -1;
    FProcessedFilesTotalSize := 0;
    FProcessedFilesSize := 0;
    // calculate file count and total size
    for i := 0 to DMHandle.CDir.Count - 1 do
      if (DMHandle.CDir.Items[i].Modified) then
      begin
        if DMHandle.CDir.Items[i].bHugeFile then
          FProcessedFilesTotalSize :=
            FProcessedFilesTotalSize + DMHandle.CDir.Items[i].Zip64ExtInfo.uncompSize
        else
          FProcessedFilesTotalSize :=
            FProcessedFilesTotalSize + DMHandle.CDir.Items[i].CentralDir.unCompSize;
        Inc(FProcessedFileCount);
        if (FirstItemNo = -1) then
          FirstItemNo := i;
      end;

    // find first difference of RBCDir and CDir
    chgItemNo := GetIndexOfFirstChange;

    // start overall progress
    if (FirstItemNo >= 0) then
      Operation := DMHandle.CDir.Items[FirstItemNo].Operation
    else
      Operation := poDelete;
    DoOnOverallProgress(0, Operation, ppStart, FProgressCancel);

    if (FProgressCancel) then
      Exit;

    // store rest of a file to temp dir
    TempFileName := '';
    if (chgItemNo < DMHandle.RBCDir.Count) then
      BackupFileRest(chgItemNo);

    // seek to write position in original file
    if (chgItemNo < DMHandle.RBCDir.Count) then
      if (DMHandle.RBCDir.Items[chgItemNo].CentralDir.relOffsetLH <> $FFFFFFFF) then
        BackupOffset := DMHandle.RBCDir.Items[chgItemNo].CentralDir.relOffsetLH{+
              LongWord(DMHandle.StubSize)}
      else
        BackupOffset := DMHandle.RBCDir.Items[chgItemNo].Zip64ExtInfo.relOffsetLH
    else
    if (DMHandle.RBCDir.Count > 0) then
      if (DMHandle.CentralDirEnd.offsetStartDir <> $FFFFFFFF) then
        BackupOffset := DMHandle.CentralDirEnd.offsetStartDir
      else
        BackupOffset := DMHandle.Zip64CentralDirEnd.offsetStartDir
    else
      BackupOffset := DMHandle.StubSize;
    FCompressedStream.Position := BackupOffset;
    // truncate rest of a file
    FCompressedStream.Size     := BackupOffset;

    Action := fxaIgnore;
    // start volume No for writing is 0
    VolumeNumberInfo.FirstVolume := True;
    try
      if (FSpanningMode = smSpanning) and (IsFloppyDrive(FFileName)) then
        CacheStream := TMemoryStream.Create
      else
        CacheStream := nil;
      try
        // scan for changes
        i := chgItemNo;
        while ((i < DMHandle.CDir.Count) and (not FProgressCancel)) do
        begin
          // retry loop
          repeat
            Action := fxaIgnore;
            bOK    := True;
            if ((not DMHandle.CDir.Items[i].Modified) or
              (DMHandle.CDir.Items[i].Operation = poRename) or
              (DMHandle.CDir.Items[i].Operation = poChangeComment) or
              (DMHandle.CDir.Items[i].Operation = poChangeAttr)) then
              AddFromFileRest(i, bfs, BackupOffset) // changed by Michael Selkin
            else
              bOK := AddFromNewSource(i);
          until (Action <> fxaRetry);
          // aborted?
          if (Action = fxaAbort) then
            break;

          // cancelled?
          if (FProgressCancel) then
            break;

          // successful adding?
          if (bOK) then
          begin
            Inc(FProcessedFileNo);
            Inc(i);
          end
          else
          begin
            if (DMHandle.CDir.Items[i].Operation = poAdd) then
              // file is not added
              DMHandle.CDir.DeleteItem(i)
            else
            begin
              // keep old file
              AddFromFileRest(i, bfs, BackupOffset);  
              Inc(i);
            end;
          end;
        end;
      finally
        // save rest of cache to floppy?
        if (CacheStream <> nil) then
        begin
          if ((not FProgressCancel) and (Action <> fxaAbort)) then
            WriteBufferToStream(PByte(CacheStream.Memory)^, CacheStream.Size,
              FCompressedStream, VolumeNumberInfo);
          CacheStream.Free;
        end;
      end;
      if ((not FProgressCancel) and (Action <> fxaAbort)) then
      begin
        DMHandle.SaveDir(True, FCompressedStream);
        DeleteFileRest;
      end;
    except
      Rollback;
      raise;
    end;

    // delete src files if nececessary -  after Move operation
    if ((not FProgressCancel) and (Action <> fxaAbort)) then
      for i := 0 to DMHandle.CDir.Count - 1 do
        if (DMHandle.CDir.Items[i].Modified) then
          if (DMHandle.CDir.Items[i].Operation = poMove) then
          begin
            // retry loop
            repeat
              Action := fxaIgnore;
              try
                if ((DMHandle.CDir.Items[i].CentralDir.externalAttr and
                  faDirectory) = 0) then
                  if (not FXCDeleteFile(TFXCPChar(DMHandle.CDir.Items[i].SrcFileName))) then
                  begin
                    FXCSetFileAttr(DMHandle.CDir.Items[i].SrcFileName, 0);
                    if (not FXCDeleteFile(TFXCPChar(DMHandle.CDir.Items[i].SrcFileName))) then
                      raise EFXCException.Create(00027,
                        [DMHandle.CDir.Items[i].SrcFileName], Self);
                  end;
              except
                on E: EFXCException do
                  DoOnProcessFileFailure(DMHandle.CDir.ItemsPtr[i]^.SrcFileName,
                    DMHandle.CDir.ItemsPtr[i]^.Operation,
                    E.NativeError, E.ErrorCode, E.Message, Action)
              end;
            until (Action <> fxaRetry); // retry loop

            // aborted?
            if (Action = fxaAbort) then
              break;
          end;// for

    // delete src folders if nececessary -  after Move operation
    if ((not FProgressCancel) and (Action <> fxaAbort)) then
      for i := 0 to DMHandle.CDir.Count - 1 do
        if (DMHandle.CDir.Items[i].Modified) then
          if (DMHandle.CDir.Items[i].Operation = poMove) then
          begin
            // retry loop
            repeat
              Action := fxaIgnore;
              try
                if ((DMHandle.CDir.Items[i].CentralDir.externalAttr and
                  faDirectory) <> 0) then
                  if (not DelTreeIfNoFiles(DMHandle.CDir.Items[i].SrcFileName)) then
                    raise EFXCException.Create(00060,
                      [DMHandle.CDir.Items[i].SrcFileName], Self);
              except
                on E: EFXCException do
                  DoOnProcessFileFailure(DMHandle.CDir.ItemsPtr[i]^.SrcFileName,
                    DMHandle.CDir.ItemsPtr[i]^.Operation,
                    E.NativeError, E.ErrorCode, E.Message, Action)
              end;
            until (Action <> fxaRetry); // retry loop

            // aborted?
            if (Action = fxaAbort) then
              break;
          end;// for

    if ((not FProgressCancel) and (Action <> fxaAbort)) then
      DoOnOverallProgress(100, Operation, ppEnd, FProgressCancel);

    if ((FProgressCancel) or (Action = fxaAbort)) then
      Rollback
    else
    // flush changes
    if (FOptions.FlushBuffers and (FCompressedStream <> nil)) then
      if (FCompressedStream is TFXCFileStream) then
        FlushFileBuffers(TFXCFileStream(FCompressedStream).Handle);
  end;
end;// ForceUpdate


//------------------------------------------------------------------------------
// init structures
// Attr and DateTime must be specified if UseDiskFileData is False
// otherwise Attr and DateTime should be 0
//------------------------------------------------------------------------------
procedure TBaseArchiver.FillDirItem(ItemNo: integer; const FileName: TFXCString;
  UseDiskFileData: Boolean; Attr: Integer; DateTime: TDateTime);
var
  ArcFileName: TFXCString;
  FileNameLen: longword;
  fTime:    integer;
  fileDate: word;
  fileTime: word;
  // Attr:     integer;
  IsDirectory: boolean;
begin

  if UseDiskFileData {True} then // 5.05 fix to avoid conflicts on adding file from stream with disk files
  begin
    ArcFileName := GetZipFileName(FileName, FCurrentDir, FOptions.StorePath, FCurrentDir);
    IsDirectory := FXCDirectoryExists(FileName);
  end
  else begin
    ArcFileName := FXCStringReplace(aaExcludeTrailingBackslash(FileName), '\', '/', [rfReplaceAll]);
    IsDirectory := (Attr and faDirectory) > 0;
  end;

  if (ArcFileName = '') then  Exit;

  if (UseDiskFileData) then
  begin
    fTime := FXCFileAge(FileName);
    if (fTime = -1) then
      fTime := DateTimeToFileDate(Now);
    if (FXCFileExists(FileName) or IsDirectory) then
      Attr := FXCGetFileAttr(FileName)
    else
      Attr := faArchive;
    if (IsDirectory) then
    begin
      if (ArcFileName[Length(ArcFileName)] <> '/') then
        ArcFileName := ArcFileName + '/';
    end;
  end
  else
  begin
    fTime := DateTimeToFileDate(DateTime);
  end;
  fileDate    := LongRec(fTime).Hi;
  fileTime    := LongRec(fTime).Lo;
  FileNameLen := Length(ArcFileName);

  with DMHandle.CDir.ItemsPtr[ItemNo]^ do
  begin
    Name     := ArcFileName;
    Modified := True;
    Password := FPassword;
    if UseDiskFileData then
      SrcFileName := FileName
    else
      SrcFileName := '';
    if (not IsDirectory) then
      CompressionMode := FCompressionMode
    else
      CompressionMode := 0;
  end;

  // fill central dir
  FillChar(DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir, FXCZipCentralDirSize, $00);
  DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir.compMethod := FCompressionMethod;
  DMHandle.CDir.ItemsPtr[ItemNo]^.CompressionMethod := Self.FCompressionMethod;


  with DMHandle.CDir.ItemsPtr[ItemNo]^.CentralDir do
  begin
    if DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature then
    begin
      signature      := FXCCentralDirSignature;
      extractVersion := FXCVersion;
    end
    else
    begin
      signature := ZipCentralDirSignature;
      if (FOptions.OEMFileNames) then
        extractVersion := ZipVersion
      else
        extractVersion := ZipVersionNoOEM;
    end;
    if (FOptions.OEMFileNames) then
      versionMadeBy := ZipVersion
    else
      versionMadeBy := ZipVersionNoOEM;

    if (FPassword <> '') then
      genPurposeFlag := genPurposeFlag or 1;
    lastModTime := fileTime;
    lastModDate := fileDate;
    crc32      := $FFFFFFFF;
    compSize   := 0;
    unCompSize := 0;
    nameLength := FileNameLen;
    externalAttr := Attr;
    relOffsetLH := 0;
    if (IsDirectory) then
    begin
      compMethod := 0;
      crc32      := $0;
      genPurposeFlag := 2;
    end;
  end;
end;// FillDirItem


//------------------------------------------------------------------------------
// extracts file
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractItem(ItemNo: integer; DestStream: TStream; Count: Integer; StartPosition: Int64);
var
  VolumeNumberInfo: TFXCVolumeNumberInfo;
begin
  if (ItemNo < 0) or (ItemNo >= DMHandle.CDir.Count) then
    raise EFXCException.Create(00016, Self);

  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  try
    // get volume number
    if (DMHandle.CDir.Items[ItemNo].CentralDir.diskNumberStart = $FFFF) then
      if (DMHandle.FZip64) then
        VolumeNumberInfo.VolumeNumber :=
          DMHandle.CDir.Items[ItemNo].Zip64ExtInfo.diskNumberStart
      else
        VolumeNumberInfo.VolumeNumber := 0
    else
      VolumeNumberInfo.VolumeNumber :=
        DMHandle.CDir.Items[ItemNo].CentralDir.diskNumberStart;
    if (DMHandle.CentralDirEnd.diskNumber = $FFFF) then
      if (DMHandle.FZip64) then
        VolumeNumberInfo.LastVolumeNumber :=
          DMHandle.Zip64CentralDirEndLocator.totalNumberOfDisks - 1
      else
        VolumeNumberInfo.LastVolumeNumber := 0
    else
      VolumeNumberInfo.LastVolumeNumber := DMHandle.CentralDirEnd.diskNumber;

    // request volume
    OpenVolume(VolumeNumberInfo);

    // Zip64?
    if (DMHandle.CDir.Items[ItemNo].CentralDir.relOffsetLH = $FFFFFFFF) then
      FCompressedStream.Position := DMHandle.CDir.Items[ItemNo].Zip64ExtInfo.relOffsetLH
    else
      FCompressedStream.Position := DMHandle.CDir.Items[ItemNo].CentralDir.relOffsetLH;

    if (not InternalDecompressFile(FCompressedStream, DestStream,
      DMHandle.CDir.ItemsPtr[ItemNo]^, Count, StartPosition)) then
      if (not FProgressCancel) and (not FExtractCorruptedFiles) and (not FSkipFile) then
        raise EFXCException.Create(00008, Self);

  finally
    VolumeNumberInfo.Free;
  end;
end; // extract item


//------------------------------------------------------------------------------
// updates item
//------------------------------------------------------------------------------
procedure TBaseArchiver.UpdateItem(ItemNo: integer; const SrcFileName: TFXCString);
begin
  if (ItemNo < 0) or (ItemNo >= DMHandle.CDir.Count) then
    raise EFXCException.Create(00024, Self);

  if (not InUpdate) then
    raise EFXCException.Create(00025, Self);

  FillDirItem(ItemNo, SrcFileName, True, 0, 0);
  DMHandle.CDir.ItemsPtr[ItemNo]^.Operation := poUpdate;
end;// UpdateItem


//------------------------------------------------------------------------------
// Delete item from archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.DeleteItem(ItemNo: integer);
begin
  if (ItemNo < 0) or (ItemNo >= DMHandle.CDir.Count) then
    raise EFXCException.Create(00021, Self);

  if (not InUpdate) then
    raise EFXCException.Create(00022, Self);

  DMHandle.CDir.DeleteItem(ItemNo);
end;// DeleteFile


//------------------------------------------------------------------------------
// check attributes for search condition
//------------------------------------------------------------------------------
function TBaseArchiver.CheckAttributesMatch(FileAttr, SearchAttr: integer): boolean;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
var
  ExcludeAttr: integer;
begin
  ExcludeAttr := not SearchAttr and faSpecial;
  Result      := not (FileAttr and ExcludeAttr <> 0);
end;// CheckAttributesMatch


//------------------------------------------------------------------------------
// check item for search condition
//------------------------------------------------------------------------------
function TBaseArchiver.IsItemMatches(ItemNo: integer; var F: TFXCArchiveItem): boolean;
var
  FileName, FileMask, ExclusionMask: TFXCString;
  i, j: integer;
begin
  Result := False;
  if (F.Handle.UseProperties) then
  begin
    FileName := FXCStringReplace(DMHandle.CDir.Items[ItemNo].Name, '/',
      '\', [rfReplaceAll]);
    // loop all masks
    for i := 0 to FileMasks.Count - 1 do
    begin
      FileMask := FileMasks.Strings[i];
      if (IsInternalFileMatchMask(FileName, FileMask)) then
      begin
        Result := True;
        // compare with exclusion masks
        for j := 0 to ExclusionMasks.Count - 1 do
        begin
          ExclusionMask := ExclusionMasks.Strings[j];
          if (IsInternalFileMatchMask(FileName, ExclusionMask)) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    end;
  end
  else
  begin
    FileName := DMHandle.CDir.Items[ItemNo].Name;
    // 'data\' -> 'data'
    if (FileName <> '') then
      if ((FileName[Length(FileName)] = '\') or
        (FileName[Length(FileName)] = '/')) then
        FileName := Copy(FileName, 1, Length(FileName) - 1);

    FileMask := F.Handle.CFindMask;
    Result   := IsInternalFileMatchMask(FileName, FileMask) or
      IsInternalFileMatchMask(FXCExtractFileName(FXCStringReplace(FileName, '/', '\', [rfReplaceAll])), FileMask);
    if Result then
       if IsInternalFileMatchMask(FileName, F.Handle.ExclusionMask) then
           Result := false;
  end;
end;// IsItemMatches


//------------------------------------------------------------------------------
// find files
//------------------------------------------------------------------------------
function TBaseArchiver.InternalFind(var F: TFXCArchiveItem): boolean;
var
  i:      integer;
  bFound: boolean;
  FileName: TFXCString;
  s:      TFXCString;
begin
  bFound := False;
  (* added *)
  // if we need one file without any wildcards and recurse search
  // then we can speed up searching by look up a hash table
  if (not F.Handle.FWildCards) and (not FOptions.Recurse) then
  begin
    FileName := '';
    i := 0;
    // construct filename from mask
    while (F.Handle.CFindMask[i] <> #0) do
    begin
      FileName := FileName + F.Handle.CFindMask[i];
      inc(i);
    end;

    // look up hash table for FileName
    DMHandle.CDir.FileExists(FileName, i);

    // if we found it then turn bFound to true
    if (i >= 0) and (i >= F.Handle.ItemNo) then
    begin
      bFound := true;
    end;
  end
  else // usally search (not modified)
  begin
    // loop for all files
    for i := F.Handle.ItemNo to DMHandle.CDir.Count - 1 do
    begin
      // recursive search?
      if (not FOptions.Recurse) then
      begin
        // 'Dir/' -> 'Dir'
        FileName := DMHandle.CDir.Items[i].Name;
        if (FileName[Length(FileName)] = '/') then
          FileName := Copy(FileName, 1, Length(FileName) - 1);
        // scan only specified path
        if (FXCLowerCase(FXCExtractFilePath(FXCStringReplace(FileName, '/',
          '\', [rfReplaceAll]))) <> FXCLowerCase(
          ExtractFilePath(FXCStringReplace(F.Handle.CFindMask, '/', '\', [rfReplaceAll]))))
        then
          continue;
      end;
      bFound := IsItemMatches(i, f);
      if (bFound) then
        if (CheckAttributesMatch(DMHandle.CDir.Items[i].CentralDir.externalAttr,
          F.Handle.FFindAttr)) then
          break
        else
          bFound := False;
    end;
  end;

  if (bFound) then
  begin
    F.Handle.ItemNo := i;
    // fill structure
    s := aaExcludeTrailingBackslash(FXCStringReplace(DMHandle.CDir.Items[i].Name,
      '/', '\', [rfReplaceAll]));
    F.FileName := FXCExtractFileName(s);
    F.StoredPath := FXCExtractFilePath(s);
    // > 4Gb?
    if (DMHandle.CDir.Items[i].CentralDir.compSize < $FFFFFFFF) then
      F.CompressedSize := DMHandle.CDir.Items[i].CentralDir.compSize
    else
      F.CompressedSize := DMHandle.CDir.Items[i].Zip64ExtInfo.compSize;
    // > 4Gb?
    if (DMHandle.CDir.Items[i].CentralDir.unCompSize < $FFFFFFFF) then
      F.UncompressedSize := DMHandle.CDir.Items[i].CentralDir.unCompSize
    else
      F.UncompressedSize := DMHandle.CDir.Items[i].Zip64ExtInfo.unCompSize;
    if (F.UncompressedSize > 0) then
      F.CompressionRate := (1 - F.CompressedSize / F.UncompressedSize) * 100.0
    else
      F.CompressionRate := 100;
    if (F.CompressionRate < 0) then
      F.CompressionRate := 0;

    F.Encrypted := ((DMHandle.CDir.Items[i].CentralDir.genPurposeFlag and $0001) = 1);
    F.LastModFileDate := DMHandle.CDir.Items[i].CentralDir.lastModDate;
    F.LastModFileTime := DMHandle.CDir.Items[i].CentralDir.lastModTime;
    F.CRC     := DMHandle.CDir.Items[i].CentralDir.crc32;
    F.ExternalFileAttributes := DMHandle.CDir.Items[i].CentralDir.externalAttr;
    F.Comment := DMHandle.CDir.Items[i].Comment;
  end;
  Result := bFound;
end;// InternalFind


//------------------------------------------------------------------------------
// is file match mask
//------------------------------------------------------------------------------
function TBaseArchiver.IsInternalFileMatchMask(
  const FileName, FileMask: TFXCString): boolean;
var
  Mask:  TFXCString;
  FName: TFXCString;
begin
  if (FileMask = '') then
    Result := False
  else
  begin
    Mask := FXCStringReplace(FileMask, '*.*', '*', []);
    if (FileMask[Length(FileMask)] = '.') then
      Mask := Copy(Mask, 1, Length(Mask) - 1);
    if (FileName[Length(FileName)] = '.') then
    begin
      FName  := Copy(FileName, 1, Length(FileName) - 1);
      Result := FXCIsStrMatchPattern(TFXCPChar(FName), TFXCPChar(Mask));
    end
    else
      Result := FXCIsStrMatchPattern(TFXCPChar(FileName), TFXCPChar(Mask));

    if (Result) then
      if (FileMask[Length(FileMask)] = '.') then
        Result := ((FXCExtractFileExt(FileName) = '.') or
          (FXCExtractFileExt(FileName) = ''));
  end;
end;// IsInternalFileMatchMask


//------------------------------------------------------------------------------
// is file on disk matches mask
//------------------------------------------------------------------------------
function TBaseArchiver.IsExternalFileMatchMask(const FileName, FileMask: TFXCString;
  IsDir: boolean): boolean;


  function ExtractDirName(Path: TFXCString): TFXCString;
  begin
    if Path[Length(Path)] = '\' then
      SetLength(Path, Length(Path) - 1);
    Result := FXCExtractFileName(Path);
  end;

var
  Mask:     TFXCString;
  bRecurse: boolean;
  FName, FMask: TFXCString;
begin
  FName := FileName;
  FMask := FileMask;
  Result := False;

  if (FMask = '*.*') then
  begin
    Result := True;
    Exit;
  end;

  if (IsDir) then
  begin
    FName := GetSlashedDir(FileName, FCurrentDir);
    if (FMask <> '') then
      if (FMask[Length(FMask)] <> '*') then
        FMask := GetSlashedDir(FMask, FCurrentDir);
  end;
  // bug fix
  if (Pos(':', FMask) <= 0) then
    Mask := GetFullMask(FMask, FCurrentDir, bRecurse)
  else
  begin
    Mask     := FMask;
    bRecurse := False;
  end;
  // compare only file names?
  if (bRecurse) then
  begin
    FName := FXCExtractRelativePath(GetSlashedDir(FCurrentDir, FCurrentDir), FName);
    // compare only file name?
    if ((Pos('/', FMask) = 0) and (Pos('\', FMask) = 0)) then
      FName := FXCExtractFileName(FName);
    FMask := FXCExtractFileName(Mask);
    // 2.72
    if FName = '' then
      FName := ExtractDirName(FileName);       // directory
    Result := FXCIsStrMatchPattern(TFXCPChar(FName), TFXCPChar(FMask)) or FXCIsStrMatchPattern(
      TFXCPChar(FName), TFXCPChar(FXCExtractRelativePath(GetSlashedDir(FBaseDir, FCurrentDir), FMask)));
  end
  else
  begin
    FMask := Mask;
    // paths are the same?
    if (FXCLowerCase(FXCExtractFilePath(FName)) = FXCLowerCase(FXCExtractFilePath(FMask))) then
      Result := FXCIsStrMatchPattern(TFXCPChar(FName), TFXCPChar(FMask))
    else
    if (FXCExtractFileName(FMask) = '*.*') then
    begin
      if (Copy(FXCLowerCase(FXCExtractFilePath(FName)), 1, Length(FMask) - 3 {Length('*.*')} ) =
        FXCLowerCase(FXCExtractFilePath(FMask))) then
        Result := True;
    end
    else
      Result := False;
  end;
end;// IsExternalFileMatchMask


//------------------------------------------------------------------------------
// Get temp file name
//------------------------------------------------------------------------------
function TBaseArchiver.GetTempFileName: TFXCString;
var
  TempPath:   array[0..255] of char;
  Prefix:     array[0..10] of char;
  lpTempName: array [0..MAX_PATH] of char;
begin
  // get temp file name
  StrPCopy(Prefix, 'FC');
  if (FTempDir <> '') then
    StrPCopy(@TempPath, FTempDir)
  else
    GetTempPath(255, @TempPath);
  Windows.GetTempFileName(@TempPath, @Prefix, 0, lpTempName);
  Result := lpTempName;
end;// GetTempFileName


//------------------------------------------------------------------------------
// tag files (from lists)
//------------------------------------------------------------------------------
procedure TBaseArchiver.TagFiles;
var
  F: TFXCArchiveItem;
begin
  // clear tags
  DMHandle.CDir.ClearTags;

  // find first file
  if (FindFirst(F)) then
    repeat
      DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Tagged := True;
    until not FindNext(F);
end;// TagFiles (lists)


//------------------------------------------------------------------------------
// tag files (params)
//------------------------------------------------------------------------------
procedure TBaseArchiver.TagFiles(const FileMask: TFXCString; SearchAttr: integer;
  ExclusionMask: TFXCString);
var
  F: TFXCArchiveItem;
begin
  // clear tags
  DMHandle.CDir.ClearTags;

  // find first file
  if (FindFirst(FileMask, F, SearchAttr, ExclusionMask)) then
    repeat
      DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Tagged := True;
    until not FindNext(F);

end;// TagFiles (params)


//------------------------------------------------------------------------------
// deletes, updates, extracts or tests tagged files
// if TaggedFile <> -1 then process only one file with index TaggedFile
//------------------------------------------------------------------------------
procedure TBaseArchiver.ProcessTaggedFiles(Operation: TFXCProcessOperation; TaggedFile : integer = -1);
var
  i:      integer;
  FName, Path: TFXCString;
  ConfirmProcess: boolean;
  OldProgressCancel: boolean;
  Action: TFXCAction;
begin
  Lock;
  try
    // for total/file progress
    FProcessedFileNo    := 0;
    FProcessedFileCount := 0;
    FProgressCancel     := False;
    FProgressEnabled    := True;
    if TaggedFile <> -1 then
      inc(FProcessedFileCount)
    else
      for i := 0 to DMHandle.CDir.Count - 1 do
        if (DMHandle.CDir.Items[i].Tagged) then
          Inc(FProcessedFileCount);

    // start overall progress
    if ((Operation = poExtract) or (Operation = poTest)) then
      DoOnOverallProgress(0, Operation, ppStart, FProgressCancel);

    if (not FProgressCancel) then
    begin
      i := 0;
      if TaggedFile <> -1 then
        i := TaggedFile;
      while (i < DMHandle.CDir.Count) do
      begin
        if (DMHandle.CDir.Items[i].Tagged) then
        begin
          DMHandle.CDir.ItemsPtr[i]^.Operation := Operation;
          // retry loop
          repeat
            Action := fxaIgnore;
            try
              // get full dest file name
              FName := DMHandle.CDir.Items[i].Name;
              FName := FXCStringReplace(FName, '/', '\', [rfReplaceAll]);

              // confirm file processing
              DoOnConfirmProcessFile(FName, Operation, ConfirmProcess);
              if (not ConfirmProcess) then
                begin
                  if (Operation = poDelete) then Inc(i);
                  continue;
                end;
              // 3.0
              if FCurrentDir = '' then FCurrentDir := FXCGetCurrentDir;
              Path := GetSlashedDir(BaseDir, FCurrentDir);

              //--- process operation on each file
              // start file progress
              if ((Operation = poExtract) or (Operation = poTest)) then
                DoOnFileProgress(DMHandle.CDir.Items[i].Name, 0, Operation,
                  ppStart, FProgressCancel);
              if (FProgressCancel) then
                break;

              case Operation of
                poDelete:
                  DeleteTaggedFile(i);
                poUpdate:
                  UpdateTaggedFile(i, FName, Path);
                poExtract:
                  ExtractTaggedFile(i, FName, Path);
                poTest:
                  TestTaggedFile(i, FName);
              end;

              OldProgressCancel := FProgressCancel;
              // process operation on each file
              if ((Operation = poExtract) or (Operation = poTest)) then
                DoOnFileProgress(DMHandle.CDir.Items[i].Name, 100, Operation,
                  ppEnd, FProgressCancel);
              FProgressCancel := FProgressCancel or OldProgressCancel;
              if (FProgressCancel) then
                break;

              // processed
              Inc(FProcessedFileNo);
            except
              on E: EFXCException do
                DoOnProcessFileFailure(FName, Operation, E.NativeError, E.ErrorCode,
                  E.Message, Action)
            end;
          until (Action <> fxaRetry); // retry loop
          if ((Action = fxaAbort) or (FProgressCancel)) then
            break;
          if (Operation <> poDelete) then
            Inc(i);
        end
        else
          Inc(i);

        if TaggedFile <> -1 then
          break;
      end;
      // end overall progress
      OldProgressCancel := FProgressCancel;
      if ((Operation = poExtract) or (Operation = poTest)) then
        DoOnOverallProgress(100, Operation, ppEnd, FProgressCancel);
      FProgressCancel := FProgressCancel or OldProgressCancel;
    end;

    // cancel operation?
    if ((FProgressCancel) or (Action = fxaAbort)) then
      CancelUpdate;
  finally
    Unlock;
  end;
end;// ProcessTaggedFiles


//------------------------------------------------------------------------------
// delete tagged file
//------------------------------------------------------------------------------
procedure TBaseArchiver.DeleteTaggedFile(ItemNo: integer);
begin
  DeleteItem(ItemNo);
end;// DeleteTaggedFile


//------------------------------------------------------------------------------
// update tagged file
//------------------------------------------------------------------------------
procedure TBaseArchiver.UpdateTaggedFile(ItemNo: integer; const FileName, Path: TFXCString);
var
  fAge1, fAge2:     integer;
  FName, SrcFileName, oldDir: TFXCString;
  ConfirmOverwrite: boolean;
begin
  // file or directory?
  if ((DMHandle.CDir.Items[ItemNo].CentralDir.externalAttr and faDirectory) <> 0) then
    Exit;

  // source file name
  SrcFileName := GetFullFileName(Path, FileName, FCurrentDir);

  // file already exists?
  if (FXCFileExists(SrcFileName)) then
  begin
    // replace read-only?
    if (not FOptions.ReplaceReadOnly) then
    begin
      // get file attributes
      if ((DMHandle.CDir.Items[ItemNo].CentralDir.externalAttr and
        faReadOnly) <> 0) then
        Exit;
    end;

    // overwrite?
    case FOptions.OverwriteMode of
      omPrompt:
      begin
        // prompt to confirm
        FName := FileName;
        DoOnConfirmOverwrite(SrcFileName, FName, ConfirmOverwrite);
        if (not ConfirmOverwrite) then
          Exit;
      end;
      omNever:
        Exit;
      omIfNewer, omIfOlder:
      begin
        // get zip file date
        LongRec(fAge1).Hi := DMHandle.CDir.Items[ItemNo].CentralDir.lastModDate;
        LongRec(fAge1).Lo := DMHandle.CDir.Items[ItemNo].CentralDir.lastModTime;
        // get file date
        fAge2 := FXCFileAge(SrcFileName);
        if (((fAge1 >= fAge2) and (FOptions.OverwriteMode = omIfNewer)) or
          ((fAge1 <= fAge2) and (FOptions.OverwriteMode = omIfOlder))) then
          Exit;
      end;
    end;


    oldDir := FCurrentDir;
    try
      FCurrentDir := GetFullFileName(FBaseDir, '', FCurrentDir);
      // update data
      UpdateItem(ItemNo, SrcFileName);
    finally
      FCurrentDir := oldDir;
    end;
  end;
end;// UpdateTaggedFile


//------------------------------------------------------------------------------
// extract tagged file
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractTaggedFile(ItemNo: integer; const FileName, Path: TFXCString);
var
  fAge1, fAge2, fAttr: integer;
  newAttr: longword;
  f: TFXCFileStream;
  FPath, FName, DestFileName: TFXCString;
  ConfirmOverwrite: boolean;
  WasException: boolean;
begin
  FName   := FileName;
  FPath   := Path;
  newAttr := DMHandle.CDir.Items[ItemNo].CentralDir.externalAttr;
  DoOnExtractFile(FName, newAttr, DMHandle.CDir.Items[ItemNo].Comment);
  WasException := False;
  // if absolute path substituted, no need to apply base path
  if (Pos(':', FName) > 0) or (Pos('\\', FName) > 0) then
    FPath := '';

  // strip file name?
  if (not FOptions.CreateDirs) then
    FName := FXCExtractFileName(FName)
  else
  // force directories
  if (FOptions.CreateDirs) then
    try
      if (not FXCForceDirectories(FPath + FXCExtractFilePath(FName))) then
        raise Exception.Create('');
    except
      raise EFXCException.Create(00020, [FPath + ExtractFilePath(FName)], Self);
    end;
  // file or directory?
  if ((DMHandle.CDir.Items[ItemNo].CentralDir.externalAttr and faDirectory) <> 0) or
    (DMHandle.CDir.Items[ItemNo].Name[Length(DMHandle.CDir.Items[ItemNo].Name)] =
    '/') then
    Exit;

  // destination file name
  DestFileName := FPath + FName;

  // file already exists?
  if (FXCFileExists(DestFileName)) then
  begin
    // replace read-only?
    if (not FOptions.ReplaceReadOnly) then
    begin
      // get file attributes
      fAttr := FXCGetFileAttr(DestFileName);
      if ((fAttr and faReadOnly) <> 0) then
        Exit;
    end;

    // overwrite?
    case FOptions.OverwriteMode of
      omPrompt:
      begin
        // prompt to confirm
        DoOnConfirmOverwrite(FName, DestFileName, ConfirmOverwrite);
        if (not ConfirmOverwrite) then
          Exit;
      end;
      omNever:
        Exit;
      omIfNewer, omIfOlder:
      begin
        // get file date
        fAge1 := FXCFileAge(DestFileName);
        // get zip file date
        LongRec(fAge2).Hi := DMHandle.CDir.Items[ItemNo].CentralDir.lastModDate;
        LongRec(fAge2).Lo := DMHandle.CDir.Items[ItemNo].CentralDir.lastModTime;
        if (((fAge1 >= fAge2) and (FOptions.OverwriteMode = omIfNewer)) or
          ((fAge1 <= fAge2) and (FOptions.OverwriteMode = omIfOlder))) then
          Exit;
      end;
    end;
  end;

  // create file stream
  try
    // delete existing read-only files
    if (FXCFileExists(DestFileName)) then
    begin
      FXCSetFileAttr(DestFileName, 0);
      FXCDeleteFile(TFXCPChar(DestFileName));
    end;
    f := TFXCFileStream.Create(DestFileName, fmCreate);
  except
    raise EFXCException.Create(00019, [DestFileName], Self);
  end;

  // extract data
  try
    WasException := False;
    try
      ExtractItem(ItemNo, f);
    except
      WasException := True;
      raise;
    end;
  finally
    // set date/time
    LongRec(fAge2).Hi := DMHandle.CDir.Items[ItemNo].CentralDir.lastModDate;
    LongRec(fAge2).Lo := DMHandle.CDir.Items[ItemNo].CentralDir.lastModTime;
    FileSetDate(f.Handle, fAge2);
    // flush file buffers?
    if (FOptions.FlushBuffers and IsRemovableDrive(DestFileName)) then
      FlushFileBuffers(f.Handle);
    // close file
    f.Free;
    // if file is skipped - delete empty file
    if (FSkipFile or FProgressCancel or WasException) then
      FXCDeleteFile(TFXCPChar(DestFileName))
    else
      // set attributes?
      if (FOptions.SetAttributes) then
        FXCSetFileAttr(DestFileName, newAttr);
  end;
end;// ExtractTaggedFile


//------------------------------------------------------------------------------
// test tagged file
//------------------------------------------------------------------------------
procedure TBaseArchiver.TestTaggedFile(ItemNo: integer; const FileName: TFXCString);
var
  f: TStream;
begin
  // file or directory?
  if ((DMHandle.CDir.Items[ItemNo].CentralDir.externalAttr and faDirectory) <> 0) then
    Exit;
  // create null stream
  f := TNullStream.Create();
  try
    ExtractItem(ItemNo, f);
  finally
    // close file
    f.Free;
  end;
end;// TestTaggedFile


//------------------------------------------------------------------------------
// get full file mask
//------------------------------------------------------------------------------
function TBaseArchiver.GetFullMask(const Mask, BaseDir: TFXCString;
  var bRecurse: boolean): TFXCString;
begin
  if (Length(Mask) > 0) then
  begin
    if (Mask[1] = '\') or (Mask[1] = '/') then
    begin
      bRecurse := False;
      if (Length(Mask) > 1) then
        // not '\\server\...'
        if (Mask[2] <> '\') and (Mask[2] <> '/') then
          Result := aaIncludeTrailingBackslash(GetFullFileName(BaseDir, '',
            FCurrentDir)) + Copy(Mask, 2, Length(Mask) - 1)
        else
          Result := Mask
      else
        Result := GetFullFileName(BaseDir, '', FCurrentDir);
    end
    else
    if (Pos('.\', Mask) = 1) then
    begin
      bRecurse := False;
      Result   := aaIncludeTrailingBackslash(GetFullFileName(BaseDir, '', FCurrentDir)) +
        Copy(Mask, 3, Length(Mask) - 2);
    end
    else
    begin
      if (Pos(':', Mask) > 0) or (Pos('..\', Mask) > 0) or
        (Pos('../', Mask) > 0) then
      begin
        bRecurse := False;
        Result   := Mask;
      end
      else
      begin
         //+++ 
        if ExtractFileName(Mask) = Mask then
          bRecurse := True
        else
          bRecurse := False;
        //---
        //bRecurse := True;
        Result   := aaIncludeTrailingBackslash(GetFullFileName(
          BaseDir, '', FCurrentDir)) + Mask;
      end;
    end;
  end;
end;// GetFullMask


//------------------------------------------------------------------------------
// internal add files
//------------------------------------------------------------------------------
procedure TBaseArchiver.InternalAddFiles(FileMasks: TFXCStrings;
  SearchAttr: integer; ExclusionMasks: TFXCStrings; bMove: boolean; bRecurse: boolean);
var
  i: integer;
  oldDir, newDir: TFXCString;

  // add found file
  procedure AddFile(const FileName, BaseDir1: TFXCString; F: TFXCSearchRec);
  var
    DirIndex, i: integer;
    NoCompress, ConfirmProcess: boolean;
    Operation:   TFXCProcessOperation;
    SourceSize:  int64;
  begin
    if (bMove) then
      Operation := poMove
    else
      Operation := poAdd;

    // confirm file processing
    DoOnConfirmProcessFile(FileName, Operation, ConfirmProcess);
    if (not ConfirmProcess) then
      exit;

    DirIndex := AddFileInit(FileName, BaseDir1, True, 0, 0);
    if (DirIndex < 0) then
      exit;
    if (bMove) then
      DMHandle.CDir.ItemsPtr[DirIndex]^.Operation := poMove;

    // compress or store?
    NoCompress := False;
    for i := 0 to NoCompressionMasks.Count - 1 do
      if (IsExternalFileMatchMask(FileName, NoCompressionMasks.Strings[i],
        (F.Attr and faDirectory) <> 0)) then
      begin
        NoCompress := True;
        break;
      end;
    if (NoCompress) then
    begin
      DMHandle.CDir.ItemsPtr[DirIndex]^.CompressionMode := 0;
      if ((FCompressionMethod = ZIP_None) or (FCompressionMethod = ZIP_ZLIB)) then
      begin
        DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compMethod := ZIP_None;
        DMHandle.CDir.ItemsPtr[DirIndex]^.CompressionMethod := ZIP_None;
      end
      else
      begin
        DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.compMethod := FXC_None;
        DMHandle.CDir.ItemsPtr[DirIndex]^.CompressionMethod := FXC_None;
      end;
    end;
    // 2.74: store file size
    Int64Rec(SourceSize).Lo := F.FindData.nFileSizeLow;
    Int64Rec(SourceSize).Hi := F.FindData.nFileSizeHigh;
    if (SourceSize < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
      DMHandle.CDir.ItemsPtr[DirIndex]^.centralDir.unCompSize := SourceSize
    else
    begin
      DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile := True;
      DMHandle.CDir.ItemsPtr[DirIndex]^.centralDir.uncompSize := $FFFFFFFF;
      DMHandle.CDir.ItemsPtr[DirIndex]^.Zip64ExtInfo.uncompSize := SourceSize;
    end;

  end;

  procedure RecursiveProcess(const StartDir, BaseDir1: TFXCString;
  Mask: TFXCString; bRecurse: boolean; CurDir: TFXCString);
  var
    FileName, FullMask, dir: TFXCString;
    F: TFXCSearchRec;
    CurAttr, j: integer;
    bMatch, bMaskRecurse: boolean;
    Res : integer;

    function ProcessFile(var F: TFXCSearchRec; const StartDir, BaseDir1: TFXCString;
      CurDir: TFXCString): boolean;
    var
      j:    integer;
      sDir: TFXCString;
    begin
      sDir := GetFullFileName(StartDir, '', CurDir);
      if ((F.Attr and faDirectory) <> 0) then
        FileName := GetSlashedDir(sDir, CurDir) + GetSlashedDir(F.Name, CurDir)
      else
        FileName := GetSlashedDir(sDir, CurDir) + F.Name;

      bMatch := True;
      // compare with exclusion masks
      for j := 0 to ExclusionMasks.Count - 1 do
        if (IsExternalFileMatchMask(FileName, ExclusionMasks.Strings[j],
          (F.Attr and faDirectory) <> 0)) then
        begin
          bMatch := False;
          break;
        end;

      // store file / dir
      if (bMatch) then
        AddFile(FileName, BaseDir1, F);

      Result := bMatch;
    end;

  begin
    FullMask := GetFullMask(Mask, StartDir, bMaskRecurse);
    // if absolute path then don't search
    if ((Mask = FullMask) and (Pos('*', Mask) = 0) and (Pos('?', Mask) = 0)) then
    begin
        if (FXCFindFirst(FullMask, SearchAttr, F) = 0) then
          try
            ProcessFile(F, FXCExtractFilePath(FullMask), BaseDir1, CurDir);
          finally
            FXCFindClose(F);
          end;
    end
    else
    begin
      // search without dirs
      if ((SearchAttr and faDirectory) <> 0) then
        CurAttr := SearchAttr - faDirectory
      else
        CurAttr := SearchAttr;
      // find files by mask
      if (FXCFindFirst(FullMask, CurAttr, F) = 0) then
      begin
        try
          repeat
            if (((F.Attr and CurAttr) <> 0) or
              ((F.Attr = 0) and (((CurAttr and faArchive) <> 0) and
              ((CurAttr and faReadOnly) <> 0) and
              ((CurAttr and faHidden) <> 0) and
              ((CurAttr and faSysFile) <> 0)))) then
              ProcessFile(F, FXCExtractFilePath(FullMask), BaseDir1, CurDir);
          until (FXCFindNext(F) <> 0);
        finally
          FXCFindClose(F);
        end;
      end;
      // find dirs and scan their content
      if ((SearchAttr and faDirectory) <> 0) or (bRecurse) then
      begin

        Dir := ExtractFilePath(FullMask);
        if (Pos(StartDir, GetFullFileName(Dir, '', CurDir)) = 0) and (bMaskRecurse) then
          Dir := aaIncludeTrailingBackslash(StartDir);

        if (FXCFindFirst(Dir + '*', faDirectory or faHidden or
          faSysFile, F) = 0) then
        begin
          try
            repeat
              // skip files
              if ((F.Attr and faDirectory) = 0) then
                continue;
              // skip '.' and  '..'
              if ((F.Name = '.') or (F.Name = '..')) then
                continue;

              // compare with exclusion masks
              bMatch := True;
              for j := 0 to ExclusionMasks.Count - 1 do
                if (IsExternalFileMatchMask(Dir + F.Name,
                  ExclusionMasks.Strings[j], (F.Attr and faDirectory) <> 0)) then
                begin
                  bMatch := False;
                  break;
                end;
              if (not bMatch) then
                continue;

              // find dir by mask?
              if ((SearchAttr and faDirectory) <> 0) and
                (FXCIsStrMatchPattern(TFXCPChar(Dir + F.Name), TFXCPChar(FullMask)) or
                FXCIsStrMatchPattern(TFXCPChar(Dir + F.Name + '.'), TFXCPChar(FullMask))) then
              begin
                // add dir element
                if (ProcessFile(F, Dir, BaseDir1, CurDir)) then
                  // process all files in this dir
                begin
                  Mask := ExtractFileName(Mask);
                  RecursiveProcess(Dir + F.Name, BaseDir1, Mask, True, CurDir);  //'*.*', True, CurDir);
                end;
              end
              else
              // scan all subdirs
              if (bRecurse) then
              begin
                if ((Pos('/', Mask) > 0) or (Pos('\', Mask) > 0)) then
                  //             if (not bMaskRecurse) then
                begin
                  // absolute path in mask
                  // proceed with recurse only if mask includes '*' or '?'
                  if ((Pos('*', Mask) > 0) or (Pos('?', Mask) > 0)) then
                    RecursiveProcess(Dir + F.Name, BaseDir1,
                      FXCExtractFileName(FullMask), True, CurDir);
                end
                else
                  RecursiveProcess(Dir + F.Name, BaseDir1, Mask, True, CurDir);
              end;
            until (FXCFindNext(F) <> 0);
          finally
            FXCFindClose(F);
          end;
        end;
      end;
    end;
  end;

begin
  Lock;
  try
    oldDir := FCurrentDir;
    try
      newDir      := GetFullFileName(FBaseDir, '', FCurrentDir);
      FCurrentDir := newDir;
      for i := 0 to FileMasks.Count - 1 do
        RecursiveProcess(newDir, newDir, FileMasks.Strings[i], bRecurse, FCurrentDir);
    finally
      FCurrentDir := oldDir;
    end;
  finally
    Unlock;
  end;
end;// InternalAddFiles


//------------------------------------------------------------------------------
// initialises data to add a file
// if UseDiskFileDate is False, Attr and DateTime must be specified
// otherwise, Attr and DateTime must be set to 0
//------------------------------------------------------------------------------
function TBaseArchiver.AddFileInit(const FileName, BaseDir1: TFXCString;
  UseDiskFileData: Boolean; Attr: Integer; DateTime: TDateTime): integer;
var
  DirIndex:    integer;
  ItemNo, i:      integer;
  Proceed:     boolean;
  fAge1, fAge2: integer;
  ArcFileName: TFXCString;
  NewFileName: TFXCString;
  NewFileAttr: Integer;
  NewFileComment: AnsiString;
begin
  // 2.76
  if UseDiskFileData then
  begin
    ArcFileName := GetZipFileName(FileName, BaseDir1, FOptions.StorePath, FCurrentDir);
    if (FXCDirectoryExists(FileName) and (Length(ArcFileName) > 0)) then
      if (ArcFileName[Length(ArcFileName)] <> '/') then
        ArcFileName := ArcFileName + '/';
  end
  else begin
    ArcFileName := FXCStringReplace(aaExcludeTrailingBackslash(FileName), '\', '/', [rfReplaceAll]);
  end;

  NewFileName := ArcFileName;
  // 2.73
  // fix bug AttrLost.zip
  if UseDiskFileData then
  begin
    if (FXCFileExists(FileName) or FXCDirectoryExists(FileName)) then
      NewFileAttr := FXCGetFileAttr(FileName)
    else
      NewFileAttr := faArchive;
  end
  else begin
    NewFileAttr := Attr;
  end;

  NewFileComment := '';

  DoOnStoreFile(NewFileName, NewFileAttr, NewFileComment, FileName);

  // find file
  if (DMHandle.CDir.FileExists(NewFileName, ItemNo)) then
  begin
    // check if owerwrite existing item or not
    case FOptions.OverwriteMode of
      omNever: Proceed  := False;
      omAlways: Proceed := True;
      omPrompt: DoOnConfirmOverwrite(FileName, NewFileName, Proceed);
      omIfNewer, omIfOlder:
      begin
        // get zip file date
        LongRec(fAge1).Hi := DMHandle.CDir.Items[ItemNo].CentralDir.lastModDate;
        LongRec(fAge1).Lo := DMHandle.CDir.Items[ItemNo].CentralDir.lastModTime;
        // get file date
        if UseDiskFileData then
          fAge2 := FileAge(FileName)
        else
          fAge2 := DateTimeToFileDate(DateTime);

        if (((fAge1 >= fAge2) and (FOptions.OverwriteMode = omIfNewer)) or
          ((fAge1 <= fAge2) and (FOptions.OverwriteMode = omIfOlder))) then
          Proceed := False
        else
          Proceed := True;
      end;
    end;
    // owerwrite existing item
    if (Proceed) then
    begin
      DirIndex := ItemNo;
      DMHandle.CDir.ItemsPtr[DirIndex]^.Modified := True;
      DMHandle.CDir.ItemsPtr[DirIndex]^.SrcFileName := FileName;
      DMHandle.CDir.ItemsPtr[DirIndex]^.Operation := poUpdate;
    end
    else
      DirIndex := -1;
  end
  else
  begin
    // fill in new item
    DMHandle.CDir.Count := DMHandle.CDir.Count + 1;
    DirIndex := DMHandle.CDir.Count - 1;
    DMHandle.CDir.ItemsPtr[DirIndex]^.Operation := poAdd;
  end;
  if (DirIndex <> -1) then
  begin
    FillDirItem(DirIndex, FileName, UseDiskFileData, Attr, DateTime);
    DMHandle.CDir.ItemsPtr[DirIndex]^.Name := NewFileName;
    DMHandle.CDir.SetItemName(DirIndex, NewFileName);
    DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.NameLength :=
      Length(DMHandle.CDir.ItemsPtr[DirIndex]^.Name);
    DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.externalAttr := NewFileAttr;
    DMHandle.CDir.ItemsPtr[DirIndex]^.Comment := NewFileComment;
    DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir.commentLength :=
      Length(NewFileComment);
    DMHandle.CDir.ItemsPtr[DirIndex]^.UseOldRijndael := false;
    if (Length(DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields) <> 0)  then
    begin
        for i:=0 to Length(DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields)-1 do
          if (DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields[i].pData <> nil) then
            FreeMem(DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields[i].pData);
        SetLength(DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields, 0);
        DMHandle.CDir.ItemsPtr[DirIndex]^.ExtraFields := nil;
    end;
  end;
  Result := DirIndex;
end;// AddFileInit


//------------------------------------------------------------------------------
// reopen file stream (multi-spanning)
//------------------------------------------------------------------------------
procedure TBaseArchiver.ReopenFileStream(const NewFileName: TFXCString);
begin
  // close current file
  if (FCompressedStream <> nil) then
    FCompressedStream.Free;
  // open new volume
  FCompressedStream := TFXCFileStream.Create(NewFileName, FileOpenMode);
  // store used file name
  VolumeFileName    := NewFileName;
end;// ReopenFileStream


//------------------------------------------------------------------------------
// prepare default volume name
//------------------------------------------------------------------------------
procedure TBaseArchiver.MakeDefaultVolumeName(var VolumeFileName: TFXCString;
  VolumeNumber: integer; CheckIfFileExists: boolean);
var
  Path, FileName, FileExt: TFXCString;
  IsSFXArchive:  boolean;
  OldVolumeName: TFXCString;
begin
  // split volume file name into path, file name and file ext
  Path     := FXCExtractFilePath(VolumeFileName);
  FileExt  := FXCExtractFileExt(VolumeFileName);
  FileName := FXCExtractFileName(VolumeFileName);
  FileName := Copy(FileName, 1, Length(FileName) - Length(FileExt));
  IsSFXArchive := (DMHandle <> nil) and (DMHandle.StubSize <> 0) or
    (SFXStub <> '') or Self.IsSFXArchive(VolumeFileName);
  // if not last volume
  if (VolumeNumber <> -1) then
    // spanning or splitting?
    case (FSpanningMode) of
      smSpanning:
      begin
        // if SFX => use EXE extension
        if (IsSFXArchive) then
          FileExt := '.exe';
        // file_xxx.ZIP or file.ZIP
        if (FSpanningOptions.AdvancedNaming) then
          VolumeFileName := Path + FileName + '_' +
            Format('%.3u', [VolumeNumber + 1]) + FileExt
        else
          VolumeFileName := Path + FileName + FileExt;
        if (CheckIfFileExists) then
        begin
          if (not FXCFileExists(VolumeFileName)) then
          begin
            OldVolumeName := VolumeFileName;
            if (not FSpanningOptions.AdvancedNaming) then
              VolumeFileName :=
                Path + FileName + '_' + Format('%.3u',
                [VolumeNumber + 1]) + FileExt
            else
              VolumeFileName := Path + FileName + FileExt;
            if (not FXCFileExists(VolumeFileName)) then
              VolumeFileName := OldVolumeName;
          end;
        end;
      end;
      smSplitting:
      begin
        // if first volume and SFX => use EXE extension
        if (VolumeNumber = 0) and IsSFXArchive then
          VolumeFileName := Path + FileName + '.exe'
        else
        // file_xxx.ZIP or file.ZXX
        if (FSpanningOptions.AdvancedNaming) then
          VolumeFileName := Path + FileName + '_' +
            Format('%.3u', [VolumeNumber + 1]) + FileExt
        else
        // Zip?
        if (Self.FCompressionMethod < FXC_PPM) then
          VolumeFileName := Path + FileName + '.Z' + Format('%.2d', [VolumeNumber + 1])
        else
          VolumeFileName := Path + FileName + '.F' + Format('%.2d', [VolumeNumber + 1]);
      end;
    end
  else // last volume
  begin
    // if SFX and splitting then last volume should not be named as EXE
    if (IsSFXArchive) and (FSpanningMode = smSplitting) then
    begin
      if (Self.FCompressionMethod < FXC_PPM) then
        FileExt := '.ZIP'
      else
        FileExt := '.FXC';
      VolumeFileName := Path + FileName + FileExt;
    end;
  end;
end;// MakeDefaultVolumeName


//------------------------------------------------------------------------------
// gets free drive space
//------------------------------------------------------------------------------
function TBaseArchiver.GetFreeDriveSpace(const VolumeFileName: TFXCString): int64;
var
  FileDrive: string;
  Size1, Size2, Size3: int64;
begin
  FileDrive := ExtractFileDrive(VolumeFileName);
  FileDrive := FileDrive + '\';
  GetDiskFreeSpaceEx(PChar(FileDrive), Size1, Size2, @Size3);
  Result := Size1;
end;// GetFreeDriveSpace


//------------------------------------------------------------------------------
// writes to stream with request of new volume
//------------------------------------------------------------------------------
function TBaseArchiver.WriteToStream(const Buffer; Count: longint;
  var Stream: TStream; VolumeNumberInfo: TFXCVolumeNumberInfo;
  RequiredFreeSpace: integer = -1; CacheStream: TStream = nil): longint;
var
  size, freeSpace, allowedSize: int64;
begin
  // spanning?
  if (FSpanningMode <> smNone) then
  begin
    size := 0;
    // all is written?
    while (size < Count) do
    begin
      // how much space is free on drive?
      freeSpace := GetFreeDriveSpace(VolumeFileName);
      if (CacheStream <> nil) then
        freeSpace := freeSpace - CacheStream.Size;
      // allowed space left
      allowedSize := FSpanningOptions.GetVolumeSize(VolumeNumberInfo) - Stream.Size;
      if (CacheStream <> nil) then
        allowedSize := allowedSize - CacheStream.Size;
      // reduce free space quota to the allowed size
      if (FSpanningOptions.GetVolumeSize(VolumeNumberInfo) <> -1) then
        freeSpace := min(freeSpace, allowedSize);
      if (freeSpace < 0) then
        break;
      // write?
      if (RequiredFreeSpace = -1) or (Count - size + RequiredFreeSpace <=
        freeSpace) then
        if (min(Count - size, freeSpace) > 0) then
          if (CacheStream <> nil) then
            size := size + CacheStream.Write((PAnsiChar(@Buffer) + size)^,
              min(Count - size, freeSpace))
          else
            size := size + Stream.Write((PAnsiChar(@Buffer) + size)^,
              min(Count - size, freeSpace));
      // not all is written? - request new volume
      if (size < Count) then
      begin
        if (CacheStream <> nil) then
        begin
          Stream.CopyFrom(CacheStream, 0); // copy the whole stream
          CacheStream.Size := 0;
        end;
        // flush on removable devices?
        if (Stream is TFXCFileStream) and (SpanningMode = smSpanning) then
          FlushFileBuffers(TFXCFileStream(Stream).Handle);
        // next volume
        VolumeNumberInfo.VolumeNumber := VolumeNumberInfo.VolumeNumber + 1;
        // request volume
        OpenVolume(VolumeNumberInfo);
        // update stream var
        Stream := FCompressedStream;
      end;
    end;
  end
  else
    size := Stream.Write(Buffer, Count);

  // all written
  Result := size;
end;// WriteToStream


//------------------------------------------------------------------------------
// writes to stream with request of new volume
//------------------------------------------------------------------------------
function TBaseArchiver.WriteToStream(SrcStream: TStream; var DestStream: TStream;
  VolumeNumberInfo: TFXCVolumeNumberInfo; var Cancel: boolean;
  Size: int64 = -1; RequiredFreeSpace: integer = -1; CacheStream: TStream = nil;
  IsCopyTempFile: boolean = False): longint;
var
  pBuffer: PByte;
  sizeToWrite, readSize, totalSize, BlockSize, totalWrittenSize: int64;
begin
  Result := 0;
  // write from one stream to another by blocks
  BlockSize := 1000000;
  pBuffer   := AllocMem(BlockSize);
  try
    if (Size = -1) then
    begin
      totalSize := SrcStream.Size - SrcStream.Position;
      readSize  := BlockSize;
    end
    else
    begin
      totalSize := Size;
      readSize  := min(Size, BlockSize);
    end;
    totalWrittenSize := 0;
    if (IsCopyTempFile) then
    begin
      DoOnCopyTempFileProgress(0, ppStart, Cancel);
      if (Cancel) then
        exit;
    end;
    while (totalWrittenSize < totalSize) do
    begin
      sizeToWrite      := SrcStream.Read(pBuffer^, readSize);
      totalWrittenSize := totalWrittenSize + WriteToStream(pBuffer^,
        sizeToWrite, DestStream, VolumeNumberInfo, RequiredFreeSpace, CacheStream);
      if (IsCopyTempFile) then
      begin
        DoOnCopyTempFileProgress(totalWrittenSize / totalSize * 100, ppProcess, Cancel);
        if (Cancel) then
          break;
      end;
    end;
    if (IsCopyTempFile) then
    begin
      DoOnCopyTempFileProgress(100, ppEnd, Cancel);
      if (Cancel) then
        exit;
    end;
  finally
    FreeMem(pBuffer);
  end;
end;// WriteToStream


//------------------------------------------------------------------------------
// writes buffer to stream with request of new volume
//------------------------------------------------------------------------------
procedure TBaseArchiver.WriteBufferToStream(const Buffer; Count: longint;
  var Stream: TStream; VolumeNumberInfo: TFXCVolumeNumberInfo;
  RequiredFreeSpace: integer = -1; CacheStream: TStream = nil);
var
  WrittenToStream: integer;
begin
  WrittenToStream := WriteToStream(Buffer, Count, Stream, VolumeNumberInfo,
    RequiredFreeSpace, CacheStream);
  if (WrittenToStream <> Count) then
    raise EFXCException.Create(00046, Self);
end;// WriteBufferToStream


//------------------------------------------------------------------------------
// process volume request
//------------------------------------------------------------------------------
procedure TBaseArchiver.OpenVolume(VolumeNumberInfo: TFXCVolumeNumberInfo);
var
  Aborted:     boolean;
  PrevVolumeFileName: TFXCString;
  VolumeFileName: TFXCString;
  RequestedVolumeSize: int64;
  FreeDriveSpace: int64;
  VolumeLabel: string;
begin
  if (SpanningMode <> smNone) then
  begin
    //--- write on disks?
    if (FileOpenMode = fmCreate) then
    begin
      // close stream for current disk
      if (FCompressedStream <> nil) then
      begin
        FCompressedStream.Free;
        FCompressedStream := nil;
      end;
      // requested volume size (-1 = auto)
      RequestedVolumeSize := FSpanningOptions.GetVolumeSize(VolumeNumberInfo);
      // repeat until abort or non-full disk
      repeat
        VolumeFileName := FFileName;
        // request volume
        DoOnRequestBlankVolume(VolumeNumberInfo.VolumeNumber, VolumeFileName, Aborted);
        if (not Aborted) then
        begin
          // get free drive space
          FreeDriveSpace := GetFreeDriveSpace(VolumeFileName);
          // disk has not enough free space?
          if (FreeDriveSpace <= MinVolumeSize) or
            // added "(SpanningMode = smSpanning) and " to fix problem with splitting
            // when there is not enough place for whole volume
            ((RequestedVolumeSize > 0) and (SpanningMode = smSpanning) and
            (FreeDriveSpace < RequestedVolumeSize)) then
            DoOnDiskFull(VolumeNumberInfo.VolumeNumber, VolumeFileName, Aborted)
          else
            break;
        end;
      until (Aborted);
      // try to reopen file stream
      if (not Aborted) then
      begin
        ReopenFileStream(VolumeFileName);
        if (SpanningMode = smSpanning) then
        begin
          try
            if (VolumeNumberInfo.FirstVolume) then
              VolumeLabel := 'pkback# ' + Format('%.3d', [1])
            else
              VolumeLabel :=
                'pkback# ' + Format('%.3d', [VolumeNumberInfo.VolumeNumber + 1]);
            SetVolumeLabel(PChar(ExtractFileDrive(VolumeFileName)),
              PChar(VolumeLabel));
          except
          end;
        end;
      end;
    end
    else
    begin
      //--- read from disks
      // repeat until abort or existing file name
      // last used volume name
      PrevVolumeFileName := VolumeFileName;
      Aborted := False;
      if (not Self.VolumeNumberInfo.IsEqualTo(VolumeNumberInfo)) then
        repeat
          VolumeFileName := FileName;
          // request volume
          Aborted := True;
          if (VolumeNumberInfo.FirstVolume) then
            DoOnRequestFirstVolume(VolumeFileName, Aborted)
          else
          if (VolumeNumberInfo.LastVolume) then
            DoOnRequestLastVolume(VolumeFileName, Aborted)
          else
            DoOnRequestMiddleVolume(VolumeNumberInfo.VolumeNumber,
              VolumeFileName, Aborted);
          // aborted?
          if (not Aborted) then
          begin
            // new file was specified - reopen?
            if (VolumeFileName <> PrevVolumeFileName) or
              (FSpanningMode = smSpanning) then
            begin
              if (FXCFileExists(VolumeFileName) or (FileOpenMode = fmCreate)) then
              begin
                // file found - reopen it
                ReopenFileStream(VolumeFileName);
                Self.VolumeNumberInfo.VolumeNumber := VolumeNumberInfo.VolumeNumber;
                Self.VolumeNumberInfo.FVolumeKind  := VolumeNumberInfo.FVolumeKind;
                // if OK - exit
                break;
              end;
            end
            else
              // file is already opened
              break;
          end;
        until (Aborted);
    end;
  end
  else // no spanning
  begin
    if (FCompressedStream = nil) then
      ReopenFileStream(FileName);
    Self.VolumeNumberInfo.VolumeNumber := VolumeNumberInfo.VolumeNumber;
    Self.VolumeNumberInfo.FVolumeKind := VolumeNumberInfo.FVolumeKind;
    Aborted := False;
  end;

  if (Aborted) then
    raise EFXCException.Create(00045, Self);
end;// OpenVolume


//------------------------------------------------------------------------------
// is ArcFileName EXE?
//------------------------------------------------------------------------------
function TBaseArchiver.IsSFXArchive(const ArcFileName: TFXCString): boolean;
begin
  Result := (Pos('.exe', FXCLowerCase(ExtractFileExt(ArcFileName))) > 0);
end;// IsSFXArchive


//------------------------------------------------------------------------------
// creates archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.InternalCreateArchive;
var
  VolumeNumberInfo: TFXCVolumeNumberInfo;
begin
  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  try
    // open archive
    if (FInMemory) then
    begin
      FCompressedStream := TMemoryStream.Create;
      // create dir manager
      DMHandle := TFXCDirManager.Create(FCompressedStream, True, Self);
    end
    else
    begin
      VolumeNumberInfo.FirstVolume := True;
      // request & open volume
      OpenVolume(VolumeNumberInfo);
      // create dir manager
      DMHandle := TFXCDirManager.Create(FCompressedStream, True, Self);
    end;
  finally
    VolumeNumberInfo.Free;
  end;
end;// InternalCreateArchive


//------------------------------------------------------------------------------
// opens non-SFX archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.InternalOpenNonSFXArchive;
var
  VolumeNumberInfo: TFXCVolumeNumberInfo;
  HasCentralDirEnd: boolean;
  bOldOpenCorruptedArchives: boolean;
begin
  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  bOldOpenCorruptedArchives := FOpenCorruptedArchives;
  FOpenCorruptedArchives := False;
  try
    DMHandle := nil;
    repeat
      if (DMHandle <> nil) then
        DMHandle.Free;
      DMHandle := nil;
      Self.VolumeNumberInfo.Init;

      VolumeNumberInfo.LastVolume := True;
      if ((not FXCFileExists(FFileName)) and IsRemovableDrive(FFileName)) then
        FSpanningMode := smSpanning;
      // request & open volume
      OpenVolume(VolumeNumberInfo);

      // create dir manager
      DMHandle := TFXCDirManager.Create(FCompressedStream, False, Self);
      HasCentralDirEnd := DMHandle.HasCentralDirEnd;
      // detect spanning mode and advanced naming
      DetectSpanning;
    until (HasCentralDirEnd or (FSpanningMode = smNone));
  finally
    FOpenCorruptedArchives := bOldOpenCorruptedArchives;
    VolumeNumberInfo.Free;
  end;
end;// InternalOpenNonSFXArchive


//------------------------------------------------------------------------------
// opens SFX archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.InternalOpenSFXArchive;
var
  VolumeNumberInfo:   TFXCVolumeNumberInfo;
  HasCentralDirEnd:   boolean;
  TestVolumeFileName: TFXCString;
  bOldOpenCorruptedArchives: boolean;
begin
  VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  bOldOpenCorruptedArchives := FOpenCorruptedArchives;
  try
    VolumeNumberInfo.LastVolume := True;
    // open first volume (EXE)
    OpenVolume(VolumeNumberInfo);
    // create dir manager
    DMHandle := TFXCDirManager.Create(FCompressedStream, False, Self);
    FOpenCorruptedArchives := False;
    // if no CDirEnd on first volume - open last volume
    if (not DMHandle.HasCentralDirEnd) then
    begin
      if (DMHandle <> nil) then
        DMHandle.Free;
      DMHandle := nil;

      // try splitting
      FSpanningMode      := smSplitting;
      TestVolumeFileName := FFileName;
      MakeDefaultVolumeName(TestVolumeFileName, -1, False);
      if (FileExists(TestVolumeFileName)) then
      begin
        Self.VolumeNumberInfo.Init;
        VolumeNumberInfo.LastVolume := True;
        // request & open last volume
        OpenVolume(VolumeNumberInfo);
      end;
      FSpanningMode := smNone;

      // create dir manager
      DMHandle := TFXCDirManager.Create(FCompressedStream, False, Self);
      // try spanning
      repeat
        if (DMHandle <> nil) then
          DMHandle.Free;
        Self.VolumeNumberInfo.Init;

        VolumeNumberInfo.LastVolume := True;
        // request & open volume
        OpenVolume(VolumeNumberInfo);

        // create dir manager
        DMHandle := TFXCDirManager.Create(FCompressedStream, False, Self);
        HasCentralDirEnd := DMHandle.HasCentralDirEnd;
        DetectSpanning;
      until (HasCentralDirEnd or (FSpanningMode = smNone));
    end
    else
      DetectSpanning;
  finally
    FOpenCorruptedArchives := bOldOpenCorruptedArchives;
    VolumeNumberInfo.Free;
  end;
end;// InternalOpenSFXArchive


//------------------------------------------------------------------------------
// determines single file or splitting or spanning
//------------------------------------------------------------------------------
procedure TBaseArchiver.DetectSpanning;
var
  TestVolumeFileName: TFXCString;
begin
  // CentralDirEnd not loaded?
  if (DMHandle.CentralDirEnd.signature = 0) then
  begin
    if (IsRemovableDrive(FFileName)) then
      FSpanningMode := smSpanning
    else
      FSpanningMode := smNone;
  end
  else
  if (DMHandle.HasCentralDirEnd and (DMHandle.CentralDirEnd.diskNumber = 0)) then
  begin
    FSpanningMode := smNone;
  end
  else
  begin
    FSpanningMode      := smSplitting;
    TestVolumeFileName := FFileName;
    MakeDefaultVolumeName(TestVolumeFileName,
      DMHandle.CentralDirEnd.diskNumber - 1, False);
    if (not FXCFileExists(TestVolumeFileName)) then
    begin
      FSpanningOptions.AdvancedNaming := not FSpanningOptions.AdvancedNaming;
      TestVolumeFileName := FFileName;
      MakeDefaultVolumeName(TestVolumeFileName,
        DMHandle.CentralDirEnd.diskNumber - 1, False);
      if (not FXCFileExists(TestVolumeFileName)) then
      begin
        FSpanningOptions.AdvancedNaming := not FSpanningOptions.AdvancedNaming;
        if (Pos('.exe', FXCLowerCase(FFileName)) = 0) and
          ((DMHandle.CentralDirEnd.diskNumber > 0) or (IsRemovableDrive(FFileName))) then
          FSpanningMode := smSpanning
        else
          FSpanningMode := smNone;
      end;
    end
    else
    if (DMHandle.CentralDirEnd.diskNumber = 0) then
      FSpanningMode := smNone;
  end;
end;// DetectSpanning


//------------------------------------------------------------------------------
// opens archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.InternalOpenArchive;
begin
  if (FileOpenMode = fmCreate) or (FInMemory) then
    InternalCreateArchive
  else
  begin
    FSpanningMode := smNone; // remove? fixed in 2.41 to detect opening non-last volume
    VolumeNumberInfo.Init;
    if (IsSFXArchive(FileName)) then
      InternalOpenSFXArchive
    else
      InternalOpenNonSFXArchive;
    if (not DMHandle.HasCentralDirEnd) and (not FOpenCorruptedArchives) then
    begin
      if not IsCustomStream then
      begin
        FCompressedStream.Free;
        FCompressedStream := nil;
      end;
      raise EFXCException.Create(00048, Self);
    end;
    DMHandle.LoadDir;
  end;

  // indicate that archive is opened
  FActive := True;
  // after open
  DoAfterOpen;
end;// InternalOpenArchive


//------------------------------------------------------------------------------
// after archive open
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoAfterOpen;
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;// DoAfterOpen


//------------------------------------------------------------------------------
// on overall progress
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnOverallProgress(Progress: double;
  Operation: TFXCProcessOperation; ProgressPhase: TFXCProgressPhase;
  var Cancel: boolean);
begin
  if Assigned(FOnOverallProgress) then
    FOnOverallProgress(Self, Progress, Operation, ProgressPhase, Cancel)
  else
    Cancel := False;
end;// DoOnOverallProgress


//------------------------------------------------------------------------------
// on file progress
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnFileProgress(const FileName: TFXCString;
  // internal or external file depending on operation
  Progress: double; Operation: TFXCProcessOperation; ProgressPhase: TFXCProgressPhase;
  var Cancel: boolean);
var
  FName: TFXCString;
begin
  if Assigned(FOnFileProgress) then
  begin
    FName := FXCStringReplace(FileName, '/', '\', [rfReplaceAll]);
    FOnFileProgress(Self, FName, Progress, Operation, ProgressPhase, Cancel);
  end
  else
    Cancel := False;
end;// DoOnFileProgress


//------------------------------------------------------------------------------
// on copy temp file progress
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnCopyTempFileProgress(Progress: double;
  ProgressPhase: TFXCProgressPhase; var Cancel: boolean);
begin
  Cancel := False;
  if Assigned(FOnCopyTempFileProgress) then
  begin
    FOnCopyTempFileProgress(Self, Progress, ProgressPhase, Cancel);
  end;
end;// DoOnFileProgress


// confirm overwrite prompt
procedure TBaseArchiver.DoOnConfirmOverwrite(SourceFileName: TFXCString;
  var DestFileName: TFXCString; var Confirm: boolean);
{$IFDEF DIALOGS}
var
  mr: TModalResult;
{$ENDIF}
begin
  if Assigned(FOnConfirmOverwrite) then
    FOnConfirmOverwrite(Self, SourceFileName, DestFileName, Confirm)
{$IFDEF DIALOGS}
  else
  begin
    mr      := MessageDlg(Format(SConfirmOverwrite, [DestFileName, SourceFileName]),
      mtConfirmation, [mbYes, mbYesToAll, mbNo, mbNoToAll], 0);
    Confirm := False;
    case mr of
      mrYes:
        Confirm := True;
      mrYesToAll:
      begin
        Confirm := True;
        FOptions.OverwriteMode := omAlways;
      end;
      mrNo:
        Confirm := False;
      mrNoToAll:
      begin
        Confirm := False;
        FOptions.OverwriteMode := omNever;
      end;
    end;
  end; // else
{$ENDIF}
end;// DoOnConfirmOverwrite


//------------------------------------------------------------------------------
// confirm file processing
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnConfirmProcessFile(const FileName: TFXCString;
  Operation: TFXCProcessOperation; var Confirm: boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmProcessFile) then
    FOnConfirmProcessFile(Self, FileName, Operation, Confirm);
end;// DoOnConfirmProcessFileEvent


//------------------------------------------------------------------------------
// occurs when password needed
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnPassword(const FileName: TFXCString;
  var NewPassword: AnsiString; var SkipFile: boolean);
var
  lFileName: string;
  lPassword: string;

{$IFDEF DIALOGS}
function GetAveCharSize(Canvas: TCanvas): TPoint;
var
 I: Integer;
 Buffer: array[0..51] of Char;
begin
 for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
 for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
 GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
 Result.X := Result.X div 52;
end;

function PasswordInputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        PasswordChar := '*';
        Text := Value;
        SelectAll;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;
{$ENDIF}

begin
  if Assigned(FOnPassword) then
    FOnPassword(Self, FileName, NewPassword, SkipFile)
{$IFDEF DIALOGS}
  else
  begin
    lFileName := FileName;
    lPassword := '';
    SkipFile := (not PasswordInputQuery(Format(SPasswordTitle, [lFileName]), SPasswordPrompt, lPassword));
    if not SkipFile then NewPassword := AnsiString(lPassword);
  end;
{$ELSE}
  else
    raise EFXCException.Create(00067);
{$ENDIF}
end;// DoOnPasswordEvent


//------------------------------------------------------------------------------
// on file processing fialure
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnProcessFileFailure(const FileName: TFXCString;
  Operation: TFXCProcessOperation; NativeError: integer; ErrorCode: integer;
  const ErrorMessage: TFXCString; var Action: TFXCAction);
{$IFDEF DIALOGS}
var
  mResult: word;
{$ENDIF}
begin
  Action := fxaIgnore;
  if Assigned(FOnProcessFileFailure) then
    FOnProcessFileFailure(Self, FileName, Operation, NativeError, ErrorCode,
      ErrorMessage, Action)
  else
  begin
{$IFDEF DIALOGS}
    if (NativeError = 00045) then
      Action := fxaAbort
    else
    begin
      // fixes error if file name is too long - 2.71; 
      if NativeError = 00061 then
        mResult := MessageDlg(ErrorMessage, mtConfirmation,
          [mbAbort, mbIgnore], 0)
      else
        mResult := MessageDlg(Format(SOnProcessFileFailure, [ErrorMessage]),
          mtConfirmation, [mbAbort, mbRetry, mbIgnore], 0);
      case mResult of
        mrRetry:
          Action := fxaRetry;
        mrIgnore:
          Action := fxaIgnore;
        else
          Action := fxaAbort;
      end;
    end;
{$ENDIF}
  end;
end;// DoOnFileProcessFailure


//------------------------------------------------------------------------------
// on request blank volume
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnRequestBlankVolume(VolumeNumber: integer;
  var VolumeFileName: TFXCString; var Cancel: boolean);
begin
  if (VolumeNumber < 0) then
    VolumeNumber := 0;
  // prepare default volume name
  MakeDefaultVolumeName(VolumeFileName, VolumeNumber, False);

  Cancel := False;
  if Assigned(FOnRequestBlankVolume) then
    FOnRequestBlankVolume(Self, VolumeNumber + 1, VolumeFileName, Cancel)
{$IFDEF DIALOGS}
  else
  begin
    if (SpanningMode = smSpanning) then
    begin
      Cancel := MessageDlg(Format(SOnRequestBlankDisk, [VolumeNumber + 1]),
        mtConfirmation, [mbOK, mbCancel], 0) <> mrOk;
    end
    else
      Cancel := False;
  end;
{$ENDIF}

end;// DoOnRequestBlankVolume


//------------------------------------------------------------------------------
// on request 1st volume
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnRequestFirstVolume(var VolumeFileName: TFXCString;
  var Cancel: boolean);
var
  OldVolumeFileName: TFXCString;
begin
  OldVolumeFileName := VolumeFileName;
  // prepare default volume name
  MakeDefaultVolumeName(VolumeFileName, 0, not Assigned(FOnRequestFirstVolume));

  Cancel := False;
  if Assigned(FOnRequestFirstVolume) then
    FOnRequestFirstVolume(Self, VolumeFileName, Cancel)
{$IFDEF DIALOGS}
  else
  begin
    if (SpanningMode = smSpanning) then
    begin
      Cancel := MessageDlg(SOnRequestFirstDisk, mtConfirmation,
        [mbOK, mbCancel], 0) <> mrOk;
      MakeDefaultVolumeName(OldVolumeFileName, 0, True);
      VolumeFileName := OldVolumeFileName;
    end
    else
      Cancel := (not FXCFileExists(VolumeFileName)) and (FileOpenMode <> fmCreate);
  end;
{$ELSE}
  else
    Cancel := (not FXCFileExists(VolumeFileName)) and (FileOpenMode <> fmCreate);
{$ENDIF}

end;// DoOnRequestFirstVolume


//------------------------------------------------------------------------------
// on request last volume
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnRequestLastVolume(var VolumeFileName: TFXCString;
  var Cancel: boolean);
{$IFDEF DIALOGS}
var
  OldVolumeFileName: TFXCString;
  bWrongFileOnDisk:  boolean;
{$ENDIF}
begin
{$IFDEF DIALOGS}
  OldVolumeFileName := VolumeFileName;
{$ENDIF}
  // prepare default volume name
  MakeDefaultVolumeName(VolumeFileName, -1, True);

  Cancel := False;
  if Assigned(FOnRequestLastVolume) then
    FOnRequestLastVolume(Self, VolumeFileName, Cancel)
{$IFDEF DIALOGS}
  else
  begin
    if (SpanningMode = smSpanning) then
    begin
      bWrongFileOnDisk := False;
      repeat
        if (not bWrongFileOnDisk) then
          Cancel := MessageDlg(SOnRequestLastDisk, mtConfirmation,
            [mbOK, mbCancel], 0) <> mrOk
        else
          Cancel := MessageDlg(Format(SWrongDiskRequestLastDisk, [VolumeFileName]),
            mtConfirmation, [mbOK, mbCancel], 0) <> mrOk;
        bWrongFileOnDisk := not FXCFileExists(VolumeFileName);
      until (Cancel or (not bWrongFileOnDisk));
    end
    else
      Cancel := not FXCFileExists(VolumeFileName);
  end;
{$ELSE}
  else
    Cancel := (not FXCFileExists(VolumeFileName)) and (FileOpenMode <> fmCreate);
{$ENDIF}
end;// DoOnRequestLastVolume


//------------------------------------------------------------------------------
// on request Middle volume
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnRequestMiddleVolume(VolumeNumber: integer;
  var VolumeFileName: TFXCString; var Cancel: boolean);
var
  OldVolumeFileName: TFXCString;
begin
  OldVolumeFileName := VolumeFileName;
  // prepare default volume name
  MakeDefaultVolumeName(VolumeFileName, VolumeNumber, not
    Assigned(FOnRequestMiddleVolume));

  Cancel := False;
  if Assigned(FOnRequestMiddleVolume) then
    FOnRequestMiddleVolume(Self, VolumeNumber + 1, VolumeFileName, Cancel)
{$IFDEF DIALOGS}
  else
  begin
    if (SpanningMode = smSpanning) then
      Cancel := MessageDlg(Format(SOnRequestMiddleDisk, [VolumeNumber + 1]),
        mtConfirmation, [mbOK, mbCancel], 0) <> mrOk
    else
      Cancel := not FXCFileExists(VolumeFileName);
    MakeDefaultVolumeName(OldVolumeFileName, VolumeNumber, True);
    VolumeFileName := OldVolumeFileName;
  end;
{$ELSE}
  else
    Cancel := (not FXCFileExists(VolumeFileName)) and (FileOpenMode <> fmCreate);
{$ENDIF}
end;// DoOnRequestMiddleVolume


//------------------------------------------------------------------------------
// on disk full
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnDiskFull(VolumeNumber: integer;
  const VolumeFileName: TFXCString; var Cancel: boolean);
{$IFDEF DIALOGS}
var
  RequestedVolumeSize: int64;
  FreeDriveSpace:      int64;
  VolumeNumberInfo:    TFXCVolumeNumberInfo;
{$ENDIF}
begin
  Cancel := False;
  if Assigned(FOnDiskFull) then
    FOnDiskFull(Self, VolumeNumber + 1, VolumeFileName, Cancel)
{$IFDEF DIALOGS}
  else
  begin
    if (SpanningMode = smSpanning) then
    begin
      VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
      VolumeNumberInfo.VolumeNumber := VolumeNumber;
      try
        RequestedVolumeSize := FSpanningOptions.GetVolumeSize(VolumeNumberInfo);
      finally
        VolumeNumberInfo.Free;
      end;
      FreeDriveSpace := GetFreeDriveSpace(VolumeFileName);
      Cancel := MessageDlg(Format(SOnDiskFull, [RequestedVolumeSize, FreeDriveSpace]),
        mtConfirmation, [mbOK, mbCancel], 0) <> mrOk;
    end
    else
      raise EFXCException.Create(00065);
//      Cancel := True;
  end;
{$ELSE}
  else
      raise EFXCException.Create(00066);
//    Cancel := True;
{$ENDIF}
end;// DoOnDiskFull


//------------------------------------------------------------------------------
// on process volume failure
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnProcessVolumeFailure(Operation: TFXCProcessOperation;
  VolumeNumber: integer; const VolumeFileName: TFXCString; var Cancel: boolean);
begin
  if Assigned(FOnProcessVolumeFailure) then
    FOnProcessVolumeFailure(Self, Operation, VolumeNumber, VolumeFileName, Cancel);
end;// DoOnProcessVolumeFailure


//------------------------------------------------------------------------------
// on store file
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnStoreFile(var FileName: TFXCString;
  var FileAttr: Integer; var Comment: AnsiString; const OriginalFileName: TFXCString);
begin
  if Assigned(FOnStoreFile) then
  begin
    FileName := FXCStringReplace(FileName, '/', '\', [rfReplaceAll]);
    FOnStoreFile(Self, FileName, FileAttr, Comment, OriginalFileName);
    FileName := FXCStringReplace(FileName, '\', '/', [rfReplaceAll]);
  end;
end;// DoOnStoreFile


//------------------------------------------------------------------------------
// on extract file
//------------------------------------------------------------------------------
procedure TBaseArchiver.DoOnExtractFile(var FileName: TFXCString;
  var FileAttr: longword; const Comment: AnsiString);
begin
  FileName := FXCStringReplace(FileName, '/', '\', [rfReplaceAll]);
  if Assigned(FOnExtractFile) then
    FOnExtractFile(Self, FileName, FileAttr, Comment);
end;// DoOnExtractFile


//------------------------------------------------------------------------------
// create
//------------------------------------------------------------------------------
constructor TBaseArchiver.Create(AOwner: TComponent);
begin
  DMHandle    := nil;
  FCompressedStream := nil;
  FNoProgress := False;
  FOptions    := TFXCOptions.Create;
  CompressionLevel := clFastest;
  FActive     := False;
  FFileMasks  := TFXCStringList.Create;
  FExclusionMasks := TFXCStringList.Create;
  
  {$IFDEF FC_VERSION}
  FFXCCryptoAlgorithm := caRijndael_128;
  FUseInitVector := False;
  {$ENDIF}
  FNoCompressionMasks := TFXCStringList.Create;
  FExtractCorruptedFiles := False;
  FOpenCorruptedArchives := True;
  FSpanningMode := smNone;
  FSpanningOptions := TFXCSpanningOptions.Create;
  FZip64Mode  := zmAuto;
  FVolumeNumberInfo := TFXCVolumeNumberInfo.Create;
  FSuppressPasswordEvent := False;
{$IFDEF FXCUNICODE}
  //FUnicodeFilenames := True;
  FUnicodeFilenames := False;   // For compact size (auto unicode)
{$ENDIF}
  inherited Create(AOwner);
end;


//------------------------------------------------------------------------------
// destroy
//------------------------------------------------------------------------------
destructor TBaseArchiver.Destroy;
begin
  CloseArchive;
  FVolumeNumberInfo.Free;
  FOptions.Free;
  FFileMasks.Free;
  FExclusionMasks.Free;
  FNoCompressionMasks.Free;
  FSpanningOptions.Free;
  inherited Destroy;
end;


//------------------------------------------------------------------------------
// creates / opens archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.OpenArchive;
var
  Handle: integer;
begin
  FUpdated := False;
  if (not FInMemory and (FXCExtractFileName(FileName) = '')) then
    raise EFXCException.Create(0009, Self);

  if (FInMemory or (not FXCFileExists(FileName))) then
    FileOpenMode := fmCreate
  else
  begin
    // detects read-only
    Handle := FileOpen(FileName, fmOpenReadWrite or fmShareDenyWrite);
    if (Handle <> -1) then
    begin
      FileOpenMode := fmOpenReadWrite or fmShareDenyWrite;
      FileClose(Handle);
    end
    else
      // read-only file
      FileOpenMode := fmOpenRead or fmShareDenyNone;
  end;

  // open/create archive
  OpenArchive(FileOpenMode);
end;// OpenArchive()

procedure TBaseArchiver.OpenArchive(Mode: word);
begin
  FUpdated := False;
  if ((not FInMemory) and (FXCExtractFileName(FileName) = '')) then
    raise EFXCException.Create(00010, Self);

  if ((Mode <> fmCreate) and (not FXCFileExists(FileName)) and
    (SpanningMode <> smSpanning) and (not IsRemovableDrive(FileName))) then
    raise EFXCException.Create(00012, [FileName], Self);

  if ((Mode <> fmCreate) and FInMemory) then
    raise EFXCException.Create(00028, [FileName], Self);

  // force save changes
  CloseArchive;
  // archive is stored in file
  IsCustomStream := False;
  // store mode
  FileOpenMode   := Mode;
  InternalOpenArchive;
end;// OpenArchive(Mode)


procedure TBaseArchiver.OpenArchive(Stream: TStream; Create: boolean);
begin
  if (SpanningMode <> smNone) then
    raise EFXCException.Create(00051, Self);

  // force save changes
  CloseArchive;
  // custom archive stream
  IsCustomStream := True;
  // save empty dir
  if (Create) then
    Stream.Size := 0;
  // create dir manager
  DMHandle := TFXCDirManager.Create(Stream, Create, Self);
  // open file
  FCompressedStream := Stream;
  // save empty dir?
  if (Create) then
    DMHandle.SaveDir(True)
  else
    // load dir from stream
    DMHandle.LoadDir;
  // indicate that archive is opened
  FActive := True;
  // after open
  DoAfterOpen;
end;// OpenArchive (Stream)


//------------------------------------------------------------------------------
// close archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.CloseArchive;
begin
  if (not FActive) then
  begin
    if ((FCompressedStream <> nil) and (not IsCustomStream)) then
      FCompressedStream.Free;
    FCompressedStream := nil;
    // clear dirs
    if (DMHandle <> nil) then
      DMHandle.Free;
    DMHandle := nil;
  end
  else
  begin
    // force save changes
    try
      ForceUpdate;
      if (FCompressedStream <> nil) then
        if (FCompressedStream.Size = DMHandle.StubSize) then
          DMHandle.SaveDir(True);
    except
    end;
    FActive := False;
    // close file/memory stream
    if (not IsCustomStream) then
      if (FCompressedStream <> nil) then
      begin
        FCompressedStream.Free;
      end;
    FCompressedStream := nil;
    // clear dirs
    DMHandle.Free;
    DMHandle := nil;
  end;
end;// CloseArchive


//------------------------------------------------------------------------------
// start transaction
//------------------------------------------------------------------------------
procedure TBaseArchiver.BeginUpdate;
var
  i: integer;
begin
  // 2.71 fix problem with infinite looping when multiple transactions are used for multi volume archives
  if (FSpanningMode <> smNone) and FUpdated then
    raise EFXCException.Create(00062);

  if (FUpdateCount = 0) then
  begin
    Active := True;
    // read-only?
    if (((FileOpenMode and (fmCreate or fmOpenReadWrite or fmOpenWrite)) = 0) and (not IsCustomStream)) then
      raise EFXCException.Create(32, Self);
    // clear modified
    for i := 0 to DMHandle.CDir.Count - 1 do
      DMHandle.CDir.ItemsPtr[i]^.Modified := False;
    // copy Dir -> RBDir (for Rollback and Commit)
    DMHandle.RBCDir.Assign(DMHandle.CDir);
  end;
  Inc(FUpdateCount);
end;// BeginUpdate


//------------------------------------------------------------------------------
// save changes
//------------------------------------------------------------------------------
procedure TBaseArchiver.EndUpdate;
begin
  if (FUpdateCount > 0) then
  begin
    if (FUpdateCount > 1) then
      Dec(FUpdateCount)
    else
    begin
      // 2.71
      FUpdated := True;
      ForceUpdate;
      // avoid errors on the following read operations
      if (SpanningMode <> smNone) and (FileOpenMode = fmCreate) then
        FileOpenMode := fmOpenReadWrite or fmShareDenyWrite;
    end;
  end;
end;// EndUpdate


//------------------------------------------------------------------------------
// cancel changes
//------------------------------------------------------------------------------
procedure TBaseArchiver.CancelUpdate;
begin
  if (FUpdateCount > 0) then
  begin
    FUpdateCount := 0;
  end;
end;// CancelUpdate

type
  THackMemoryStream = class(TMemoryStream)
  public
    function CopyFrom(Source: TStream; Count: Int64): Int64;
end;

{ THackMemoryStream }

function THackMemoryStream.CopyFrom(Source: TStream; Count: Int64): Int64;
var
  BufSize, N: Integer;
  Buffer: PByte;
  Copied : Int64;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;

  if (Count > 10000) then
    BufSize := 10000
  else
    BufSize := Count;

  Result := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
      if (Count >= 10000) then
        BufSize := 10000
      else
        BufSize := Count;
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

//------------------------------------------------------------------------------
// add stream data to archive as a file
//------------------------------------------------------------------------------
procedure TBaseArchiver.InternalAddFromStream(const FileName: TFXCString;
  Stream: TStream; CopyToBuffer: Boolean; DestroyStream: Boolean; Position: Int64;
  Count: Int64; Attr: integer; DateTime: TDateTime);
var
  DirIndex, OldPosition: Int64;
  MemStream:  THackMemoryStream;
  fTime:      integer;
  SourceSize: int64;
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;
  // don't call progress handlers
  FProgressEnabled := False;
  // start transaction
  BeginUpdate;
  try
    try
      DirIndex := AddFileInit(FileName, '', False, Attr, DateTime);
      if (DirIndex >= 0) then
      begin
        with DMHandle.CDir.ItemsPtr[DirIndex]^.CentralDir do
        begin
          externalAttr := Attr;
          if (DateTime = 0) then
            DateTime := Now;
          if (DateTime <> 0) then
          begin
            fTime := DateTimeToFileDate(DateTime);
            lastModTime := LongRec(fTime).Lo;
            lastModDate := LongRec(fTime).Hi;
          end;
        end;

        // store stream
        if (not CopyToBuffer) then
        begin
          DMHandle.CDir.ItemsPtr[DirIndex]^.Stream   := Stream;
          DMHandle.CDir.ItemsPtr[DirIndex]^.Position := Position;
          DMHandle.CDir.ItemsPtr[DirIndex]^.bDestroyStream := DestroyStream;
        end
        else
        begin
          MemStream := THackMemoryStream.Create;
          try
            MemStream.Size  := Count;
            MemStream.Position := 0;
            OldPosition     := Stream.Position;
            Stream.Position := Position;
            MemStream.CopyFrom(Stream, Count);
            // free old stream, if any
            if DMHandle.CDir.ItemsPtr[DirIndex]^.bDestroyStream then
              if DMHandle.CDir.ItemsPtr[DirIndex]^.Stream <> nil then
                DMHandle.CDir.ItemsPtr[DirIndex]^.Stream.Free;
            // store stream
            DMHandle.CDir.ItemsPtr[DirIndex]^.Stream := MemStream;
            DMHandle.CDir.ItemsPtr[DirIndex]^.Position := 0;
            DMHandle.CDir.ItemsPtr[DirIndex]^.bDestroyStream := True;
            Stream.Position := OldPosition;
          except
            if (DMHandle.CDir.ItemsPtr[DirIndex]^.Stream <> nil) then
              DMHandle.CDir.ItemsPtr[DirIndex]^.Stream.Free;
            DMHandle.CDir.ItemsPtr[DirIndex]^.Stream := nil;
            raise;
          end;
        end;
      end;

      // 2.74: fix progress
      // store source size
      SourceSize := Stream.Size;
      if (SourceSize < $FFFFFFFF) and (FZip64Mode <> zmAlways) then
        DMHandle.CDir.ItemsPtr[DirIndex]^.centralDir.unCompSize := SourceSize
      else
      begin
        DMHandle.CDir.ItemsPtr[DirIndex]^.bHugeFile := True;
        DMHandle.CDir.ItemsPtr[DirIndex]^.centralDir.uncompSize := $FFFFFFFF;
        DMHandle.CDir.ItemsPtr[DirIndex]^.Zip64ExtInfo.uncompSize := SourceSize;
      end;
      // end 2.74 fix progress

    except
      CancelUpdate;
      raise;
    end;
  finally
    // save changes
    EndUpdate;
  end;
end;// AddFromStream

//------------------------------------------------------------------------------
// add stream data to archive as a file - public interface
//------------------------------------------------------------------------------
procedure TBaseArchiver.AddFromStream;
begin
  InternalAddFromStream(FileName, Stream, CopyToBuffer, False, Position, Count, Attr, DateTime);
end;


//------------------------------------------------------------------------------
// add buffer data to archive as a file
//------------------------------------------------------------------------------
procedure TBaseArchiver.AddFromBuffer(const FileName: TFXCString; const Buffer;
  Count: integer; Attr: integer = faArchive; DateTime: TDateTime = 0);
var
  MemStream: TMemoryStream;
begin
  // changed in 3.0
  MemStream := TMemoryStream.Create;
  MemStream.Write(Buffer, Count);
  InternalAddFromStream(FileName, MemStream, False, True, 0, Count, Attr, DateTime);
end;


//------------------------------------------------------------------------------
// adds TFXCString data to archive as a file
//------------------------------------------------------------------------------
procedure TBaseArchiver.AddFromString(const FileName: TFXCString;
  const Text: string; Attr: integer = faArchive; DateTime: TDateTime = 0);
var
  StrStream: TStringStream;
begin
  // changed to  3.0
  StrStream := TStringStream.Create(Text);
  InternalAddFromStream(FileName, StrStream, False, True, 0, StrStream.Size, Attr, DateTime);
end;


//------------------------------------------------------------------------------
// extracts file from archive to stream
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractToStream(const FileName: TFXCString; Stream: TStream);
var
  OldRecurse: boolean;
  ItemNo: integer;
begin
  // checks that archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  // don't call progress handlers
  FProgressEnabled := True;

  OldRecurse := FOptions.Recurse;
  try
    FOptions.Recurse := False;
    // find file to extract
    // GetFileCendralDirItemNo added in 2.71
    FProcessedFileNo := 0;
    DoOnOverallProgress(0, poExtract, ppStart, FProgressCancel);
    //DoOnFileProgress(DMHandle.CDir.Items[ItemNo].SrcFileName, 0,
    //   poExtract, ppStart, FProgressCancel);

    if GetFileCendralDirItemNo(FileName, ItemNo) then
    begin
      Self.FProcessedFileCount := 1;    
      ExtractItem(ItemNo, Stream)
    end
    else
      raise EFXCException.Create(00015, [FileName], Self);

    DoOnFileProgress(DMHandle.CDir.Items[ItemNo].SrcFileName, 100,
       poExtract, ppEnd, FProgressCancel);
    DoOnOverallProgress(100, poExtract, ppEnd, FProgressCancel);


  finally
    FOptions.Recurse := OldRecurse;
  end;
end;// ExtractToStream


//------------------------------------------------------------------------------
// extracts file from archive to buffer
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractToBuffer(const FileName: TFXCString; var Buffer;
  Count: integer; StartPosition: Int64);
var
  // F: TFXCArchiveItem;
  MemStream:  TMemoryStream;
  OldRecurse: boolean;
  ItemNo:     integer;
begin
  // checks that archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  // don't call progress handlers
  FProgressEnabled := False;

  OldRecurse := FOptions.Recurse;
  try
    FOptions.Recurse := False;
    // find file to extract
    if GetFileCendralDirItemNo(FileName, ItemNo) then
    begin
      MemStream := TMemoryStream.Create;
      try
        MemStream.Position := 0;
        ExtractItem(ItemNo, MemStream, Count, StartPosition);
        if (StartPosition = 0) then
          MemStream.Position := 0;
        MemStream.ReadBuffer(Buffer, Count);
      finally
        MemStream.Free;
      end;
    end
    else
      raise EFXCException.Create(00015, [FileName], Self);
  finally
    FOptions.Recurse := OldRecurse;
  end;
end;// ExtractToBuffer


//------------------------------------------------------------------------------
// extracts file from archive to TFXCString
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractToString(const FileName: TFXCString; var Text: string);
var
  StrStream:  TStringStream;
  OldRecurse: boolean;
  ItemNo:     integer;
begin
  // checks that archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  // don't call progress handlers
  FProgressEnabled := False;

  OldRecurse := FOptions.Recurse;
  try
    FOptions.Recurse := False;
    // find file to extract
    if GetFileCendralDirItemNo(FileName, ItemNo) then
    begin
      StrStream := TStringStream.Create('');
      try
        StrStream.Position := 0;
        ExtractItem(ItemNo, StrStream);
        StrStream.Position := 0;
        Text := StrStream.DataString;
      finally
        StrStream.Free;
      end;
    end
    else
      raise EFXCException.Create(00015, [FileName], Self);
  finally
    FOptions.Recurse := OldRecurse;
  end;
end;// ExtractToString


//-------------------------------------------------------------
// faster method than CopyFrom, also allows for progressbar
// Introduced this faster method that dramatically increases archiving speed, especially
// for big files
// Thanks to Sanjay!
//------------------------------------------------------------------------------
function TBaseArchiver.FastFileAppend(const OuTFXCFileStream, InFileStream: TFXCFileStream;
  const FileName: TFXCString): boolean;
const
  BufSize = 1024 * 1024;
var
  Size, sizeBuf: integer;
  sizeWritten: integer;
  Buffer:    Pointer;
  SizeDone, SizeFile: int64;
  retResult: boolean;
begin
  retResult := True;
  sizeBuf   := BufSize;
  OuTFXCFileStream.Position := OuTFXCFileStream.Size;
  SizeFile  := InFileStream.Size;
  InFileStream.Position := 0;
  SizeDone  := 0;
  GetMem(Buffer, BufSize);
  try
    if (FProgressEnabled) then
    begin
      DoOnFileProgress(FileName, 0, poMakeSFX, ppStart, FProgressCancel);
      DoOnOverallProgress(0, poMakeSFX, ppStart, FProgressCancel);
    end;
    FProgressMax := sizeFile;
    repeat
      FProgress := SizeDone;
      if (FProgressEnabled) then
      begin
        DoOnFileProgress(FileName, FProgress / FProgressMax * 100.0, poMakeSFX,
          ppProcess, FProgressCancel);
        if (FProgressCancel) then
          break;
        DoOnOverallProgress(FProgress / FProgressMax * 100.0, poMakeSFX,
          ppProcess, FProgressCancel);
        if (FProgressCancel) then
          break;
      end;
      size     := InFileStream.Read(Buffer^, sizeBuf);
      SizeDone := SizeDone + size;
      if size <> sizeBuf then
        if SizeDone < sizeFile then
        begin
          retResult := False;
          Break;
        end;
      sizeWritten := OuTFXCFileStream.Write(Buffer^, size);
      if size <> sizeWritten then
      begin
        retResult := False;
        Break;
      end;
    until SizeDone >= sizeFile;
    if (FProgressEnabled) then
    begin
      DoOnFileProgress(FileName, 100, poMakeSFX, ppEnd, FProgressCancel);
      DoOnOverallProgress(100, poMakeSFX, ppEnd, FProgressCancel);
    end;
  finally
    FreeMem(Buffer);
    Result := retResult;
  end;
end;


//------------------------------------------------------------------------------
// make SFX
//------------------------------------------------------------------------------
procedure TBaseArchiver.MakeSFX(const SFXFileName: TFXCString);
var
  ArchiveStream: TStream;
  StubStream, DestStream: TFXCFileStream;
  SFXDirManager: TFXCDirManager;
  bSFXStubAlreadyExists: boolean;
begin
  if (SFXStub = '') then
    raise EFXCException.Create(00017, Self);
  if (not FXCFileExists(SFXStub)) then
    raise EFXCException.Create(00018, [SFXStub], Self);
  if (SpanningMode <> smNone) then
    raise EFXCException.Create(00049, Self);

  // save changes
  if (InUpdate) then
    ForceUpdate;

  ArchiveStream := FCompressedStream;
  StubStream    := nil;
  DestStream    := nil;
  try
    // open stub
    StubStream := TFXCFileStream.Create(SFXStub, fmOpenRead or fmShareDenyNone);
    // open destination SFX
    DestStream := TFXCFileStream.Create(SFXFileName, fmCreate);
    // if not active - open archive file
    if (FActive) then
    begin
      ArchiveStream := FCompressedStream;
      bSFXStubAlreadyExists := DMHandle.IsSFXArchive;
    end
    else
    begin
      ArchiveStream := TFXCFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
      // open directory with loading headers
      SFXDirManager := TFXCDirManager.Create(ArchiveStream, False, Self);
      try
        SFXDirManager.LoadDir;
        bSFXStubAlreadyExists := SFXDirManager.IsSFXArchive;
      finally
        SFXDirManager.Free;
      end;
      ArchiveStream.Position := 0;
    end;

    if (bSFXStubAlreadyExists) then
      raise EFXCException.Create(00050, Self);

    // copy stub
    DestStream.CopyFrom(StubStream, StubStream.Size);
    // append archive
    FProgressEnabled := True;
    if not FastFileAppend(DestStream, TFXCFileStream(ArchiveStream), FFileName) then
      raise EFXCException.Create(00056, Self);
    // update central dir end
    // open directory with loading headers
    SFXDirManager := TFXCDirManager.Create(DestStream, False, Self);
    SFXDirManager.LoadDir;
    //   SFXDirManager.ApplyStubOffset;
    SFXDirManager.SaveDir(False);
    SFXDirManager.Free;
  finally
    if (not FActive) then
      ArchiveStream.Free;
    StubStream.Free;
    DestStream.Free;
  end;
end;// MakeSFX


//------------------------------------------------------------------------------
// finds a file (first match) (params)
//------------------------------------------------------------------------------
function TBaseArchiver.FindFirst(var F: TFXCArchiveItem): boolean;
begin
  F.Handle.UseProperties := True;
  F.Handle.FFindAttr := FOptions.SearchAttr;
  F.Handle.ItemNo := 0;
  Result := InternalFind(F);
end;// FindFirst (lists)


//------------------------------------------------------------------------------
// finds a file (first match) (params)
//------------------------------------------------------------------------------
function TBaseArchiver.FindFirst(const FileMask: TFXCString; var F: TFXCArchiveItem;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = ''): boolean;
var
  Mask:   TFXCString;
  drvlen: integer;
begin
  drvlen := Length(ExtractFileDrive(FileMask));
  if ((drvlen > 0) and (Options.StorePath <> spFullPathWithDrive)) then

    Mask := Copy(FileMask, drvlen + 1, Length(FileMask) - drvlen)
  else
    Mask := FileMask;
  Mask := FXCStringReplace(Mask, '\', '/', [rfReplaceAll]);

  Move(TFXCPChar(Mask)^, F.Handle.CFindMask[0], ByteLength(Mask));
  F.Handle.CFindMask[Length(Mask)] := #0;
  F.Handle.ExclusionMask := ExclusionMask;
  F.Handle.UseProperties := False;
  // file mask or file name?
  F.Handle.FWildCards := (Pos('*', Mask) > 0) or (Pos('?', Mask) > 0);
  F.Handle.FFindAttr := SearchAttr;
  F.Handle.ItemNo := 0;
  Result := InternalFind(F);
end;// FindFirst (params)


//------------------------------------------------------------------------------
// finds a file (next match)
//------------------------------------------------------------------------------
function TBaseArchiver.FindNext(var F: TFXCArchiveItem): boolean;
begin
  Inc(F.Handle.ItemNo);
  Result := InternalFind(F);
end;// FindNext


//------------------------------------------------------------------------------
// rename file within archive
//------------------------------------------------------------------------------
procedure TBaseArchiver.RenameFile(const OldName, NewName: TFXCString);
var
  F1, F2: TFXCArchiveItem;

  procedure RenameItem(F: TFXCArchiveItem);
  var
    s, old, new: TFXCString;
    I, UnicodeDataLen, UnicodeFileNameLen: Integer;
    P: PByte;
  begin
    s := DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Name;
    if not (DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified and (DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Operation = poRename)) then
      DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.OldName := s;
    old := FXCStringReplace(OldName, '\', '/', [rfReplaceAll]);
    new := FXCStringReplace(NewName, '\', '/', [rfReplaceAll]);
    s   := FXCStringReplace(s, old, new, [rfReplaceAll, rfIgnoreCase]);
    DMHandle.CDir.SetItemName(F.Handle.ItemNo, s);
    DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.CentralDir.nameLength := Length(s);
    // special case for several sequentially applied operations within one transaction
    if ((not DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified) or
      (DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Operation = poChangeAttr) or
      (DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Operation = poChangeComment)) then
    begin
      DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified  := True;
      DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Operation := poRename;
    end;
    // update unicode extra fields
    {$IFDEF FXCUNICODE}

    // 5.06 fix:   if FUnicodeFilenames then
    begin
      // find unicode extra field
      for I := 0 to Length(DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.ExtraFields) - 1 do
      begin
        if DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.ExtraFields[I].headerID = UnicodeExtraFieldHeaderID then
        begin
          FreeMem(DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.ExtraFields[I].pData);
          UnicodeFileNameLen := Length(New);
          UnicodeDataLen := SizeOf(UnicodeAdditionalSignature) + SizeOf(UnicodeFileNameLen) + Length(New) * 2;
          DMHandle.CDir.Items[F.Handle.ItemNo].ExtraFields[I].dataSize := UnicodeDataLen;
          DMHandle.CDir.Items[F.Handle.ItemNo].ExtraFields[I].pData := AllocMem(SizeOf(Integer) + SizeOf(Integer) + Length(New) * 2);
          P := DMHandle.CDir.Items[F.Handle.ItemNo].ExtraFields[Length(DMHandle.CDir.Items[F.Handle.ItemNo].ExtraFields) - 1].pData;
          Move(UnicodeAdditionalSignature, P^, SizeOf(UnicodeAdditionalSignature));
          Inc(P, 4);
          Move(UnicodeFileNameLen, P^, SizeOf(UnicodeFileNameLen));
          Inc(P, 4);
          {$IFDEF FC_VERSION}
          // crypting file Name ? (unicode extra field)
          if (((DMHandle.CentralDirEnd.signature = FXCCentralDirEndSignature) or
            (DMHandle.CDir.Items[F.Handle.ItemNo].CentralDir.extractVersion = FXCVersion)) and
            ((DMHandle.CDir.Items[F.Handle.ItemNo].CentralDir.genPurposeFlag and $0001) = 1)) then
          begin
            // encrypt file name
            FXCInternalEncryptBuffer(caRijndael_256, PByte(PWideChar(New)), Length(New) * 2,
              AnsiString(IntToStr(FXCCentralDirEndSignature)));
          end;
          {$ENDIF}
          Move(PWideChar(New)^, P^, UnicodeFileNameLen*2);
        end;
      end;
    end;
    {$ENDIF}
  end;

begin
  // checks that archive is open
  CheckInactive;

  if (FindFirst(OldName, F1)) then
  begin
    BeginUpdate;
    try
      RenameItem(F1);
      // is folder renamed?
      if (DMHandle.CDir.ItemsPtr[F1.Handle.ItemNo]^.CentralDir.externalAttr and
        faDirectory <> 0) then
        // rename all files in folder
        if (FindFirst(DMHandle.CDir.ItemsPtr[F1.Handle.ItemNo]^.OldName +
          '*.*', F2)) then
          repeat
            RenameItem(F2);
          until (not FindNext(F2));
      EndUpdate;
    except
      CancelUpdate;
      raise;
    end;
  end
  else
    raise EFXCException.Create(00030, [OldName], Self);
end;// RenameFile


//------------------------------------------------------------------------------
// change external file attributes
//------------------------------------------------------------------------------
procedure TBaseArchiver.ChangeFilesAttr(const FileMask: TFXCString; NewAttr: longword);
var
  F: TFXCArchiveItem;
begin
  // checks that archive is open
  CheckInactive;

  if (FindFirst(FileMask, F)) then
  begin
    BeginUpdate;
    try
      repeat
        with DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^ do
        begin
          if ((CentralDir.ExternalAttr and faDirectory) <> 0) then
            CentralDir.ExternalAttr := NewAttr or faDirectory
          else
            CentralDir.ExternalAttr := NewAttr;
          if (not Modified) then
          begin
            Modified  := True;
            Operation := poChangeAttr;
          end;
        end;
      until (not FindNext(F));
      EndUpdate;
    except
      CancelUpdate;
      raise;
    end;
  end
  else
    raise EFXCException.Create(00031, [FileMask], Self);
end;// ChangeFilesAttr


//------------------------------------------------------------------------------
// change internal file attributes
//------------------------------------------------------------------------------
procedure TBaseArchiver.ChangeFilesIntAttr(const FileMask: TFXCString; NewAttr: longword);
var
  F: TFXCArchiveItem;
begin
  // checks that archive is open
  CheckInactive;

  if (FindFirst(FileMask, F)) then
  begin
    BeginUpdate;
    try
      repeat
        DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.CentralDir.internalAttr := NewAttr;
        if (not DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified) then
        begin
          DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified := True;
          DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Operation := poChangeAttr;
        end;
      until (not FindNext(F));
      EndUpdate;
    except
      CancelUpdate;
      raise;
    end;
  end
  else
    raise EFXCException.Create(00038, [FileMask], Self);
end;// ChangeFilesIntAttr


//------------------------------------------------------------------------------
// changes files comment
//------------------------------------------------------------------------------
procedure TBaseArchiver.ChangeFilesComment(const FileMask: TFXCString;
  const NewComment: AnsiString);
var
  F: TFXCArchiveItem;
begin
  // checks that archive is open
  CheckInactive;

  if (FindFirst(FileMask, F)) then
  begin
    BeginUpdate;
    try
      repeat
        DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Comment := NewComment;
        DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.CentralDir.commentLength :=
          Length(NewComment);

        if (not DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified) then
        begin
          DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Modified := True;
          DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Operation := poChangeComment;
        end;
      until (not FindNext(F));
      EndUpdate;
    except
      CancelUpdate;
      raise;
    end;
  end
  else
    raise EFXCException.Create(00031, [FileMask], Self);
end;// ChangeFilesComment


//------------------------------------------------------------------------------
// checks whether the password is valid for specified file in archive
//------------------------------------------------------------------------------
function TBaseArchiver.IsFilePasswordValid(const FileName: TFXCString;
  const Password: AnsiString): boolean;
var
  F:      TFXCArchiveItem;
  FKey:   TZipKey;
  fh:     TFXCFileHeader;
  fileHeader: TFXCFileHeader;
  fcrc32: longword;
  lfh:    TFXCZipFileHeader;
  OldPassword: AnsiString;
  pCrypter : ^TAESCryptoTransform;
  VolumeNumberInfo: TFXCVolumeNumberInfo;
begin
  // checks that archive is open
  CheckInactive;

  // by default
  Result := False;

  // find file in archive
  if (FindFirst(FileName, F)) then
  begin
    // file is encrypted?
    if (F.Encrypted) then
    begin
     VolumeNumberInfo := TFXCVolumeNumberInfo.Create;
     try
        // get volume number
      if (DMHandle.CDir.Items[F.Handle.ItemNo].CentralDir.diskNumberStart = $FFFF) then
        if (DMHandle.FZip64) then
          VolumeNumberInfo.VolumeNumber :=
           DMHandle.CDir.Items[F.Handle.ItemNo].Zip64ExtInfo.diskNumberStart
        else
           VolumeNumberInfo.VolumeNumber := 0
      else
        VolumeNumberInfo.VolumeNumber :=
          DMHandle.CDir.Items[F.Handle.ItemNo].CentralDir.diskNumberStart;
      if (DMHandle.CentralDirEnd.diskNumber = $FFFF) then
        if (DMHandle.FZip64) then
          VolumeNumberInfo.LastVolumeNumber :=
            DMHandle.Zip64CentralDirEndLocator.totalNumberOfDisks - 1
        else
          VolumeNumberInfo.LastVolumeNumber := 0
      else
        VolumeNumberInfo.LastVolumeNumber := DMHandle.CentralDirEnd.diskNumber;

      // request volume
      OpenVolume(VolumeNumberInfo);


      // seek to the local file header beginning
      if (DMHandle.CDir.Items[F.Handle.ItemNo].CentralDir.relOffsetLH < $FFFFFFFF) then
        FCompressedStream.Position :=
          DMHandle.CDir.Items[F.Handle.ItemNo].CentralDir.relOffsetLH
      else
        FCompressedStream.Position :=
          DMHandle.CDir.Items[F.Handle.ItemNo].Zip64ExtInfo.relOffsetLH;

      // skip local file header
      //     FCompressedStream.Seek(FXCZipFileHeaderSize, soFromCurrent);
      FCompressedStream.Read(lfh, FXCZipFileHeaderSize);

      FCompressedStream.Seek(lfh.nameLength + lfh.extraLength, soFromCurrent);

      // Zip or FXC?
      if (DMHandle.CDir.Items[F.Handle.ItemNo].CompressionMethod = ZIP_None) or
        (DMHandle.CDir.Items[F.Handle.ItemNo].CompressionMethod = ZIP_ZLIB) then
      begin
        //--- ZIP
        
      end
      else
      begin
        // FXC
        {$IFDEF FC_VERSION}
        // load header
        if (FCompressedStream.Read(FileHeader, FXCFileHeaderSize) <>
          FXCFileHeaderSize) then
          exit;

        Move(FileHeader, fh, sizeof(FileHeader));
        // decryption of control block
        FXCInternalDecryptBuffer(
          TFXCCryptoAlgorithm(fh.EncryptionAlgorithm),
          @fh.ControlBlock,
          @fh.ControlBlock,
          sizeof(fh.ControlBlock),
          Password,
          DMHandle.CDir.Items[F.Handle.ItemNo].UseOldRijndael);
        // check crc
        fcrc32 := $FFFFFFFF;
        UpdateCRC32(
          @fh.ControlBlock,
          sizeof(fh.ControlBlock),
          fcrc32);
        // password is valid
        Result := not ((not fcrc32) <> fh.ControlBlockCrc32);
        {$ENDIF}
      end;
     finally
       VolumeNumberInfo.Free;
     end
    end
    else
      Result := True;
  end
  else
    raise EFXCException.Create(00042, [FileName], Self);
end;// IsFilePasswordValid


//------------------------------------------------------------------------------
// adds files (uses list properties)
//------------------------------------------------------------------------------
procedure TBaseArchiver.AddFiles;
var
  EMasks: TFXCStringList;
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  // add files
  BeginUpdate;
  EMasks := TFXCStringList.Create;
  try
    // assign Exclusion masks
    EMasks.Assign(FExclusionMasks);
    // exlcude also archive from adding it to itself
    EMasks.Add(GetFullFileName('', FFileName, FCurrentDir));
    try
      InternalAddFiles(FFileMasks, FOptions.SearchAttr, EMasks, False, FOptions.Recurse);
      EndUpdate;
    except
      on E: Exception do
      begin
        CancelUpdate;
        raise;
      end;
    end;
  finally
    EMasks.Free;
  end;
end;// AddFiles (list)


//------------------------------------------------------------------------------
// adds files (uses file mask, attributes and exclusion mask)
//------------------------------------------------------------------------------
procedure TBaseArchiver.AddFiles(const FileMask: TFXCString;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
var
  FMasks, EMasks: TFXCStringList;
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  FMasks := TFXCStringList.Create;
  EMasks := TFXCStringList.Create;
  FMasks.Add(FileMask);
  if (ExclusionMask <> '') then
    EMasks.Add(ExclusionMask);
  // exlcude archive from adding it to itself
  EMasks.Add(GetFullFileName('', FFileName, FCurrentDir));
  if (VolumeFileName <> '') and (VolumeFileName <> FFileName) then
    EMasks.Add(GetFullFileName('', VolumeFileName, FCurrentDir));
  try
    // add files
    BeginUpdate;
    try
      InternalAddFiles(FMasks, SearchAttr, EMasks, False, FOptions.Recurse);
      EndUpdate;
    except
      CancelUpdate;
      raise;
    end;
  finally
    FMasks.Free;
    EMasks.Free;
  end;
end;// AddFiles (params)


//------------------------------------------------------------------------------
// moves files (uses list properties)
//------------------------------------------------------------------------------
procedure TBaseArchiver.MoveFiles;
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  // move files
  BeginUpdate;
  try
    InternalAddFiles(FFileMasks, FOptions.SearchAttr, FExclusionMasks,
      True, FOptions.Recurse);
    EndUpdate;
  except
    CancelUpdate;
    raise;
  end;
end;// MoveFiles (list)


//------------------------------------------------------------------------------
// moves files (uses file mask, attributes and exclusion mask)
//------------------------------------------------------------------------------
procedure TBaseArchiver.MoveFiles(const FileMask: TFXCString;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
var
  FMasks, EMasks: TFXCStringList;
begin
  // checks that archive is open
  CheckInactive;

  FMasks := TFXCStringList.Create;
  EMasks := TFXCStringList.Create;
  FMasks.Add(FileMask);
  if (ExclusionMask <> '') then
    EMasks.Add(ExclusionMask);
  try
    // move files
    BeginUpdate;
    try
      InternalAddFiles(FMasks, SearchAttr, EMasks, True, FOptions.Recurse);
      EndUpdate;
    except
      CancelUpdate;
      raise;
    end;
  finally
    FMasks.Free;
    EMasks.Free;
  end;
end;// MoveFiles (params)


//------------------------------------------------------------------------------
// deletes files (uses list properties)
//------------------------------------------------------------------------------
procedure TBaseArchiver.DeleteFiles;
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  // tag files
  TagFiles;

  // delete tagged files
  BeginUpdate;
  try
    ProcessTaggedFiles(poDelete);
    EndUpdate;
  except
    CancelUpdate;
    raise;
  end;
end;// DeleteFiles (list)


//------------------------------------------------------------------------------
// deletes files (uses file mask, attributes and exclusion mask)
//------------------------------------------------------------------------------
procedure TBaseArchiver.DeleteFiles(const FileMask: TFXCString;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  // tag files
  TagFiles(FileMask, SearchAttr, ExclusionMask);

  // delete tagged files
  BeginUpdate;
  try
    ProcessTaggedFiles(poDelete);
    EndUpdate;
  except
    CancelUpdate;
    raise;
  end;
end;// DeleteFiles (params)


//------------------------------------------------------------------------------
// updates files (uses list properties)
//------------------------------------------------------------------------------
procedure TBaseArchiver.UpdateFiles;
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  // tag files
  TagFiles;

  // update tagged files
  BeginUpdate;
  try
    ProcessTaggedFiles(poUpdate);
    EndUpdate;
  except
    CancelUpdate;
    raise;
  end;
end;// UpdateFiles (list)


//------------------------------------------------------------------------------
// updates files (uses file mask, attributes and exclusion mask)
//------------------------------------------------------------------------------
procedure TBaseArchiver.UpdateFiles(const FileMask: TFXCString;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
begin
  // checks that archive is open
  CheckInactive;
  CheckModifySpanning;

  // tag files
  TagFiles(FileMask, SearchAttr, ExclusionMask);

  // update tagged files
  BeginUpdate;
  try
    ProcessTaggedFiles(poUpdate);
    EndUpdate;
  except
    CancelUpdate;
    raise;
  end;
end;// UpdateFiles (params)


//------------------------------------------------------------------------------
// tests files (uses list properties)
//------------------------------------------------------------------------------
procedure TBaseArchiver.TestFiles;
begin
  // checks that archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  // tag files
  TagFiles;

  // test tagged files
  ProcessTaggedFiles(poTest);
end;// TestFiles (list)


//------------------------------------------------------------------------------
// tests files (uses file mask, attributes and exclusion mask)
//------------------------------------------------------------------------------
procedure TBaseArchiver.TestFiles(const FileMask: TFXCString;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
begin
  // checks thgat archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  // tag files
  TagFiles(FileMask, SearchAttr, ExclusionMask);

  // test tagged files
  ProcessTaggedFiles(poTest);
end;// TestFiles (params)


//------------------------------------------------------------------------------
// input file is specified by FileName property
// output file by default will be the same as input
//------------------------------------------------------------------------------
procedure TBaseArchiver.RepairArchive(const OutputFileName: TFXCString = '');
var
  bExtractCorrupted: boolean;
  bOpenCorruptedArchives: boolean;
  newFileName: TFXCString;
  resFileName: TFXCString;
  tempFileName: TFXCString;
  newArchive: TBaseArchiver;
  fs:   TFXCFileStream;
  i, n: integer;
begin
  if (FActive) then
    raise EFXCException.Create(00037, Self);
  newFileName := OutputFileName;
  if (OutputFileName = '') then
    newFileName := GetTempFileName;
  try
    if (OutputFileName = '') then
    begin
      resFileName := FileName;
      fs := TFXCFileStream.Create(resFileName, fmOpenReadWrite or fmShareExclusive);
      fs.Free;
      FXCDeleteFile(TFXCPChar(newFileName));
      fs := TFXCFileStream.Create(newFileName, fmCreate);
      fs.Free;
      FXCDeleteFile(TFXCPChar(newFileName));
    end
    else
    begin
      resFileName := OutputFileName;
      FXCDeleteFile(TFXCPChar(resFileName));
      fs := TFXCFileStream.Create(resFileName, fmCreate);
      fs.Free;
      FXCDeleteFile(TFXCPChar(resFileName));
    end;
  except
    raise EFXCException.Create(00036, [resFileName], self);
    Exit;
  end;
  newArchive := TBaseArchiver.Create(nil);
  bExtractCorrupted := ExtractCorruptedFiles;
  try
    bOpenCorruptedArchives := FOpenCorruptedArchives;
    FOpenCorruptedArchives := True;
    OpenArchive(fmOpenRead and fmShareExclusive);
    ExtractCorruptedFiles := True;
    newArchive.FileName := newFileName;
    newArchive.isZIPFormat := isZIPFormat;
    newArchive.OpenArchive(fmCreate);
    //   newArchive.BeginUpdate;
    tempFileName := GetTempFileName;
    fs := TFXCFileStream.Create(tempFileName, fmCreate);
    try
      for i := 0 to DMHandle.CDir.Count - 1 do
      begin
        if ((DMHandle.CDir.Items[i].CentralDir.externalAttr and faDirectory) <> 0) then
        begin
          // directory
          n := newArchive.DMHandle.CDir.Count;
          newArchive.DMHandle.CDir.SetCount(n + 1);
          newArchive.DMHandle.CDir.SetItem(n, DMHandle.CDir.Items[i]);
        end // extracting directory
        else
        begin
          fs.Size := 0;
          // file
          try
            ExtractToStream(DMHandle.CDir.Items[i].Name, fs);
            newArchive.Password   := Password;
            
            {$IFDEF FC_VERSION}
            newArchive.FFXCCryptoAlgorithm := Self.FFXCCryptoAlgorithm;
            {$ENDIF}
            newArchive.FCompressionAlgorithm := FCompressionAlgorithm;
            newArchive.FCompressionMode := FCompressionMode;
            newArchive.FCompressionMethod := Self.FCompressionMethod;

            newArchive.AddFromStream(DMHandle.CDir.Items[i].Name, fs);
            newArchive.ChangeFilesAttr(DMHandle.CDir.Items[i].Name,
              DMHandle.CDir.Items[i].CentralDir.externalAttr);

            newArchive.ChangeFilesIntAttr(DMHandle.CDir.Items[i].Name,
              DMHandle.CDir.Items[i].CentralDir.internalAttr);
          except
            continue;
          end;
        end; // extracting file
      end; // scanning items in the corrupted archive
    finally
      FOpenCorruptedArchives := bOpenCorruptedArchives;
    end;
  finally
    //  newArchive.EndUpdate;
    fs.Free;
    FXCDeleteFile(TFXCPChar(tempFileName));
    CloseArchive;
    newArchive.Free;
    ExtractCorruptedFiles := bExtractCorrupted;
  end;
  if (OutputFileName = '') then
  begin
    FXCDeleteFile(TFXCPChar(FileName));
    FXCCopyFile(TFXCPChar(newFileName), TFXCPChar(FileName), False);
  end;

end;


//------------------------------------------------------------------------------
// extracts files (uses list properties)
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractFiles;
begin
  // checks that archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  // tag files
  TagFiles;

  // extract tagged files
  ProcessTaggedFiles(poExtract);
end;// ExtractFiles (list)


//------------------------------------------------------------------------------
// extract files (uses file mask, attributes and exclusion mask)
//------------------------------------------------------------------------------
procedure TBaseArchiver.ExtractFiles(const FileMask: TFXCString;
  SearchAttr: integer = DefaultSearchAttr; const ExclusionMask: TFXCString = '');
var
  F: TFXCArchiveItem;
begin
  // checks that archive is open
  CheckInactive;
  // checks that archive is not in update
  CheckInUpdate;

  (* added *)
  // if only one file needed, then speed up
  if (not FOptions.Recurse)
    and (not ((Pos('*', FileMask) > 0) or (Pos('?', FileMask) > 0)))
    and (ExclusionMask = '')
    and (SearchAttr = DefaultSearchAttr) then
  begin
     if FindFirst(FileMask, F, SearchAttr, ExclusionMask) then
     begin
       // tag file
       DMHandle.CDir.ItemsPtr[F.Handle.ItemNo]^.Tagged := True;
       // extract tagged files
       ProcessTaggedFiles(poExtract, F.Handle.ItemNo);
     end;
  end
  else
  begin
    // tag files
    TagFiles(FileMask, SearchAttr, ExclusionMask);

    // extract tagged files
    ProcessTaggedFiles(poExtract);
  end;
end;// ExtractFiles (params)


//------------------------------------------------------------------------------
// IsValidArchiveFile
//------------------------------------------------------------------------------
function TBaseArchiver.IsValidArchiveFile: boolean;
var
  FOldOpenCorruptedArchives: boolean;
  FOldSpanningMode: TFXCSpanningMode;
begin
  Result := False;
  // checks that archive is not open
  if (not FActive) then
    if (FileExists(FFileName)) then
    begin
      FCompressedStream := TFXCFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
      try
        DMHandle := TFXCDirManager.Create(FCompressedStream, False, Self);
        FOldOpenCorruptedArchives := FOpenCorruptedArchives;
        FOpenCorruptedArchives := False;
        try
          if (DMHandle.HasCentralDirEnd) then
            Result := True
          else begin
            FOldSpanningMode := FSpanningMode;
            FSpanningMode    := smSplitting;
            try

              try
                DMHandle.LoadDir;
                Result := True;
              except
                FSpanningMode := smNone;
                try
                  DMHandle.LoadDir;
                  Result := (DMHandle.CDir.Count > 0);
                except
                  Result := False;
                end;
              end;
            finally
              if (Result) then
                try
                  DMHandle.Free;
                  DMHandle := nil;
                  FOpenCorruptedArchives := True;
                  OpenArchive(fmOpenRead or fmShareDenyNone);
                  try
                    TestFiles('*.*');
                  finally
                    CloseArchive;
                  end;
                except
                  Result := False;
                end;
              FSpanningMode := FOldSpanningMode;
            end;
          end;
        finally
          FOpenCorruptedArchives := FOldOpenCorruptedArchives;
          if (DMHandle <> nil) then
            DMHandle.Free;
          DMHandle := nil;
        end;
      finally
        FCompressedStream.Free;
        FCompressedStream := nil;
      end;
    end
    else
      Result := False
  else
    Result := True;
end;// IsValidArchiveFile



{$IFDEF FC_VERSION}
//------------------------------------------------------------------------------
// FillRandomBuffer
//------------------------------------------------------------------------------
procedure TBaseArchiver.FillRandomBuffer(buf: PByteArray; BufSize: Integer);
var
s: ShortString;
i:integer;
begin
  randomize;
  for i:=1 to length(s) do
    s[i] := AnsiChar(Random(MAXINT) mod 256);
  with TRandom_LFSR.Create(s, 400, False, nil) do
  try
    Buffer(buf^, BufSize);
  finally
   Free;
  end;
end;//FillRandomBuffer


//------------------------------------------------------------------------------
// GenerateIV
//------------------------------------------------------------------------------
procedure TBaseArchiver.GenerateIV;
begin
  //DoSeed(FInitVector, Length(FInitVector));
  //DoBuffer(FInitVector, Length(FInitVector));
  FillRandomBuffer(@FInitVector[0], FXC_MAX_IVECTOR_SIZE);
  FUseInitVector := true;
end;// GenerateIV

//------------------------------------------------------------------------------
// SetInitVector
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetInitVector(IV: Pointer; Size: Integer);
begin
  FillChar(FInitVector, MaxInitVectorSize,0);
  Move(IV^, FInitVector[0], min(size, MaxInitVectorSize));
end;

//------------------------------------------------------------------------------
// GetIVByte
//------------------------------------------------------------------------------
function TBaseArchiver.GetIVByte(Index: Integer): Byte;
begin
 if (Index < 0) or (Index >= MaxInitVectorSize) then
  raise EFXCException.Create(00063, [Index,MaxInitVectorSize], Self);
 Result := FInitVector[Index];
end;//GetIVByte

//------------------------------------------------------------------------------
// SetIVByte
//------------------------------------------------------------------------------
procedure TBaseArchiver.SetIVByte(Index: Integer; const Value: Byte);
begin
 if (Index < 0) or (Index >= MaxInitVectorSize) then
  raise EFXCException.Create(00064, [Index,MaxInitVectorSize], Self);
 FInitVector[Index] := Value;
end;//SetIVByte

////////////////////////////////////////////////////////////////////////////////

//   TFlexCompress

////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
// set compressioon method
//------------------------------------------------------------------------------
procedure TFlexCompress.SetCompMethod;
begin
  if (FCompressionMode = 0) then
    Self.FCompressionMethod := FXC_None
  else
  begin
    if (FCompressionAlgorithm = caZLIB) then
      Self.FCompressionMethod := FXC_ZLIB
    else
    if (FCompressionAlgorithm = caBZIP) then
      Self.FCompressionMethod := FXC_BZIP
    else
    if (FCompressionAlgorithm = caPPM) then
      Self.FCompressionMethod := FXC_PPM;
  end;
end; //SetCompMethod


//------------------------------------------------------------------------------
// set compression algorithm
//------------------------------------------------------------------------------
procedure TFlexCompress.SetCompressionAlgorithm(newAlgorithm: TFXCCompressionAlgorithm);
begin
  FCompressionAlgorithm := newAlgorithm;
end;

//------------------------------------------------------------------------------
// set crypto algorithm
//------------------------------------------------------------------------------
procedure TFlexCompress.SetPassword(const Value : AnsiString);
begin
  Self.FPassword := Value;   
end;


//------------------------------------------------------------------------------
// create
//------------------------------------------------------------------------------
constructor TFlexCompress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  isZIPFormat := False;
  FZip64Mode  := zmAuto;
  FCompressionAlgorithm := caZLIB;
end;


//------------------------------------------------------------------------------
// destroy
//------------------------------------------------------------------------------
destructor TFlexCompress.Destroy;
begin
  inherited Destroy;
end;

{$ENDIF}









function TBaseArchiver.GetFileCendralDirItemNo(FileName: TFXCString;
  out ItemNo: integer): boolean;
var
  F: TFXCArchiveItem;
begin
  Result := DMHandle.CDir.FileExists(FileName, ItemNo);
  if not Result then
  begin
    Result := FindFirst(FileName, F);
    ItemNo := F.Handle.ItemNo;
  end;
end;

{  TNullStream  }

procedure TNullStream.SetSize(NewSize: Longint);
begin
//
end;
{$IFDEF D6H}
procedure TNullStream.SetSize(const NewSize: Int64);
begin
//
end;
{$ENDIF}

function TNullStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

{$IFDEF D6H}
function TNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;
{$ENDIF}


function TNullStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;


function TNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;


{$IFDEF FXCUNICODE}

{ TUnicodeExtendedInfo }

function TUnicodeExtendedInfo.Load(Data: PByte; Size: Integer): Boolean;
var
  MS: TMemoryStream;
begin
  Result := False;
  MS := TMemoryStream.Create;
  try
    MS.Write(Data^, Size);
    Result := LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

function TUnicodeExtendedInfo.LoadFromStream(Stream: TStream): Boolean;
var
  NameLen: Integer;
begin
  Result := False;
  Stream.Position := 0;
  Stream.Read(FAdditionalSignature, 4);
  if FAdditionalSignature <> UnicodeAdditionalSignature then
    Exit;
  Stream.Read(NameLen, 4);
  SetLength(FName, NameLen);
  Stream.Read(PWideChar(FName)^, NameLen * 2);
  Result := True;
end;

procedure TUnicodeExtendedInfo.SaveToStream(Stream: TStream);
var
  Len: Integer;
begin
  Stream.Write(UnicodeAdditionalSignature, 4);
  Len := Length(FName);
  Stream.Write(Len, 4);
  Stream.Write(PWideChar(FName)^, Length(FName) * 2);
end;

{$ENDIF}

function TBaseArchiver.WriteToStreamWithOnDiskFull(Stream: TStream;
  var Buffer; Count: Integer): Boolean;
var
  Cancel: Boolean;
  SavePos: Int64;
begin
  SavePos := Stream.Position;
  while Stream.Write(Buffer, Count) <> Count do
  begin
    Cancel := True;
    DoOnDiskFull(-1, FFileName, Cancel);
    // exit if cancel
    if Cancel then
    begin
      FProgressCancel := True;
      Result := False;
      Exit;
    end;
    // if not cancel, try again
    Stream.Position := SavePos;
  end;
  Result := True;
end;

constructor TFXCCryptoTransform.Create;
begin
  inherited Create;
end;

procedure TFXCCryptoTransform.Initialize(mode : TCryptoTransformMode; const item : TDirItem);
begin
  Self.FTransformMode := mode;
end;

(* Implementation of the TAESCryptoTransform class  - BEGIN *)

constructor TAESCryptoTransform.Create(const Strength : integer; Crypt_version : word; ActualCompMethod : word);
begin
  inherited Create;
  Self.FKeyLengthBytes := Trunc(Strength / 8);
  Self.FVersion := Crypt_version;
  Self.FActualCompressionMethod := ActualCompMethod;
  Self.FAuthenticationCodeSize := 10;
  Self.FPHash := FindHash_by_Name('SHA1');
  Self.FKeyGenerator := nil;
  Self.FCryptor := nil;
  Self.FHashKey:=nil;
  Self.FEncryptionKey:=nil;
  Self.FPasswordVerificationValue:=nil;
  Self.FSaltValue := nil;
  Self.FAuthenticationCode := nil;
end;

destructor TAESCryptoTransform.Destroy;
begin
   if (Self.FKeyGenerator <> nil) then
     Self.FKeyGenerator.Destroy;
   if (Self.FHashKey <> nil) then
     FreeMem(Self.FHashKey);
   if (Self.FEncryptionKey <> nil) then
     FreeMem(Self.FEncryptionKey);
   if (Self.FPasswordVerificationValue <> nil) then
     FreeMem(Self.FPasswordVerificationValue);
   if (Self.FSaltValue <> nil) then
     FreeMem(Self.FSaltValue);
   if (Self.FAuthenticationCode <> nil) then
     FreeMem(Self.FAuthenticationCode);
   if (Self.FCryptor <> nil) then
     Self.FCryptor.Destroy;
 
end;

procedure TAESCryptoTransform.Initialize(Mode : TCryptoTransformMode; const Item : TDirItem);
begin
  inherited Initialize(Mode, Item);
end;

        //returns new AESCryptoTransform object based on the information from the AES extra field
function TAESCryptoTransform.CreateAESEncryption(const KeyLengthBits : integer; VersionNumber : Word; CompressionMethod : Word) : TAESCryptoTransform;
begin
    Result := TAESCryptoTransform.Create(KeyLengthBits div 8, VersionNumber, CompressionMethod);
end;

procedure TAESCryptoTransform.GenerateKey(const Password : AnsiString);
begin
  Self.FPassword := Password;
  if (Self.FTransformMode = Decryption) and (Self.FSaltValue <> nil) then
  begin
      if (Self.FKeyGenerator <> nil) then
         Self.FKeyGenerator.Destroy;
      Self.FKeyGenerator := TPBKDF2.Create(Self.FPassword, Self.FSaltValue, Self.FKeyLengthBytes div 2, 1000)
  end
  else
    begin
      if (Self.FKeyGenerator <> nil) then
         Self.FKeyGenerator.Destroy;
      Self.FKeyGenerator := TPBKDF2.Create(Self.FPassword, Self.FKeyLengthBytes div 2, 1000);
      Self.FSaltValue := Self.FKeyGenerator.GetSaltValue;
    end;
  Self.UpdateKeys();
end;

procedure TAESCryptoTransform.UpdateKeys;
var
  Keys : PByteArray;
  i: Integer;

begin
  if (Self.FKeyGenerator <> nil) then
         Self.FKeyGenerator.Destroy;
  Self.FKeyGenerator := TPBKDF2.Create(Self.FPassword, Self.FSaltValue, Self.FKeyLengthBytes div 2, 1000);
  Self.FKeyGenerator.GetKeyBytes(Self.FKeyLengthBytes*2 + 2, Keys);
  if (Self.FEncryptionKey <> nil) then
     FreeMem(Self.FEncryptionKey);
  GetMem(Self.FEncryptionKey, Self.FKeyLengthBytes);

  if (Self.FHashKey <> nil) then
     FreeMem(Self.FHashKey);
  GetMem(Self.FHashKey, Self.FKeyLengthBytes);

  if (Self.FPasswordVerificationValue <> nil) then
     FreeMem(Self.FPasswordVerificationValue);
  GetMem(Self.FPasswordVerificationValue, 2);

  for i := 0 to Self.FKeyLengthBytes -1 do
  begin
      Self.FEncryptionKey^[i] := Keys^[i];
      Self.FHashKey^[i] := Keys^[i+Self.FKeyLengthBytes];
  end;

  FXChmac.hmac_init(Self.FHMAC_Context, Self.FPHash, Self.FHashKey, Self.FKeyLengthBytes);

  if (Self.FCryptor <> nil) then
     Self.FCryptor.Destroy;
  Self.FCryptor := TCipher_Rijndael.Create(Self.FEncryptionKey, Self.FKeyLengthBytes);
  Self.FCryptor.Mode := cmCTR;

  Self.FPasswordVerificationValue^[0] := keys^[Self.FKeyLengthBytes*2];
  Self.FPasswordVerificationValue^[1] := keys^[Self.FKeyLengthBytes*2+1];

  FreeMem(Keys);
end;

function TAESCryptoTransform.CheckPassword(const Password : AnsiString; Item : TDirItem) : Boolean;
var KeyGen : TPBKDF2;
    PasswordValidation : PByteArray;
    Temp : PByteArray;
begin
  //generated by the given password
  keyGen := TPBKDF2.Create(Password, Self.FSaltValue, Self.FKeyLengthBytes div 2, 1000);
  keyGen.GetKeyBytes(Self.FKeyLengthBytes * 2 + 2, Temp);

  GetMem(PasswordValidation, 2);

  PasswordValidation[0] := Temp[Self.FKeyLengthBytes*2];
  PasswordValidation[1] := Temp[Self.FKeyLengthBytes*2+1];

  //if validation values are equal
  if (Self.FPasswordVerificationValue[0] = PasswordValidation[0]) and (Self.FPasswordVerificationValue[1] = PasswordValidation[1]) then
        Result := true
  else
        Result := false;
  KeyGen.Destroy;
  FreeMem(Temp);
  FreeMem(PasswordValidation);
end;

function TAESCryptoTransform.IsDirItemCorrupted(const Item : TDirItem; Crc32 : Cardinal) : Boolean;
var I : Integer;
    Digest : THashDigest;
begin

  if Self.FVersion = 1 then
      Result := (Crc32 <> Item.CentralDir.crc32)
  else
  begin
     //check authentication codes

     hmac_final(Self.FHMAC_Context, Digest);

     for i := 0 to Self.FAuthenticationCodeSize - 1 do
     begin
        if Digest[i] <> Self.FAuthenticationCode[i] then
        begin
           Result := true;
           exit;
        end
        else
          Result := false;
     end;
  end;
end;

function TAESCryptoTransform.GetKey() : PByteArray;
begin
  Result := Self.FEncryptionKey;
end;

function TAESCryptoTransform.GetPassword() : AnsiString;
begin
  Result := Self.FPassword;
end;


procedure TAESCryptoTransform.Reset();
begin
   SetLength(Self.FPassword, 0);
   Self.FKeyLengthBytes := 0;
   Self.FEncryptionKey := nil;
   Self.FHashKey := nil;
   Self.FKeyGenerator := nil;
   Self.FPasswordVerificationValue := nil;
   Self.FSaltValue := nil;
   Self.FCryptor := nil;
end;


function TAESCryptoTransform.DecryptBuffer(const InputBuffer : PAnsiChar; InputOffset : Longint; InputCount : Integer; var OutputBuffer : PAnsiChar; OutputOffset : Longint) : Longint;
var ToCrypt : PAnsiChar;
    i : Integer;
begin

   hmac_updateXL(Self.FHMAC_Context, InputBuffer, InputCount);

   GetMem(ToCrypt, InputCount);

   for i := 0 to InputCount - 1 do
      ToCrypt[i] :=  InputBuffer[i + InputOffset];

   Self.FCryptor.DecodeBuffer(ToCrypt^, ToCrypt^, InputCount);

   for i := 0 to InputCount - 1 do
      OutputBuffer[i + OutputOffset] := ToCrypt[i];

   FreeMem(ToCrypt);

   Result := InputCount;
end;

function TAESCryptoTransform.EncryptBuffer(const InputBuffer : PAnsiChar; InputOffset : Longint; InputCount : Integer; var OutputBuffer : PAnsiChar; OutputOffset : Longint) : Longint;
var ToCrypt : PAnsiChar;
    i : Integer;
begin
   GetMem(ToCrypt, InputCount);

   for i := 0 to InputCount - 1  do
      ToCrypt[i] := InputBuffer[i + InputOffset];

   Self.FCryptor.EncodeBuffer(ToCrypt^, ToCrypt^, InputCount);

   for i := 0 to inputCount - 1 do
      OutputBuffer[i + OutputOffset] :=  ToCrypt[i];

   hmac_updateXL(Self.FHMAC_Context, ToCrypt, InputCount);

   FreeMem(ToCrypt);

   Result := InputCount;

end;

function TAESCryptoTransform.GetExtraFieldData() : TFXCExtraFieldDataBlock;
var Strength : Word;
    ExtraField : TFXCExtraFieldDataBlock;
    HeaderID : Word;
    VendorID : Array [0..1] of Byte;
    TempStream : TStream;
    StrengthCode : Byte;
begin
  Strength := Self.FKeyLengthBytes * 8;
  case Strength of
  128:
    StrengthCode := 1;
  192:
    StrengthCode := 2;
  256:
    StrengthCode := 3;
  end;

  VendorID[0] := Ord('A');
  VendorID[1] := Ord('E');

  ExtraField.headerID := $9901;
  ExtraField.dataSize := 7;
  ExtraField.pData :=  AllocMem(7);

  TempStream := TMemoryStream.Create;

  TempStream.Write(Self.FVersion, 2);
  TempStream.Write(VendorID, 2);

  TempStream.Write(StrengthCode, 1);

  TempStream.Write(Self.FActualCompressionMethod, 2);
  TempStream.Seek(0, 0);
  TempStream.Read(ExtraField.pData^, 7);

  TempStream.Destroy;

  Result := ExtraField;
end;

function TAESCryptoTransform.GetFileStorageStartBlock() : PByte;
var TempStream : TMemoryStream;
    Buffer : PByte;
begin
   TempStream := TMemoryStream.Create;
   TempStream.Write(Self.FSaltValue^, Self.FKeyLengthBytes div 2);
   TempStream.Write(Self.FPasswordVerificationValue^, 2);
   GetMem(Buffer,Integer(TempStream.Size));
   TempStream.Seek(0, 0);
   TempStream.Read(Buffer^, Integer(TempStream.Size));

   TempStream.Destroy;

   Result := Buffer;

end;

procedure TAESCryptoTransform.LoadFileStorageStartBlock(Stream : TStream; Offset : Int64);
var
    SaltSize : Integer;
begin
   SaltSize := Self.FKeyLengthBytes div 2;
   Stream.Seek(Offset, soFromCurrent);
   Stream.Read(Self.FSaltValue^, SaltSize);
   //Salt value changed, need to rebuild keys
   Self.UpdateKeys;
   Stream.Read(Self.FPasswordVerificationValue^, 2);
end;

function TAESCryptoTransform.GetFileStorageStartBlockSize : Longint;
begin
  Result := Self.FKeyLengthBytes div 2 + 2;
end;


function TAESCryptoTransform.GetFileStorageEndBlock : PByte;
var
    i : Integer;
    MAC: THashDigest;
    Res : PByte;
begin

  if Self.FVersion <> 1 then
  begin
    //Calculate authenticationCode

    hmac_final(Self.FHMAC_Context, MAC);

    GetMem(Self.FAuthenticationCode, Self.FAuthenticationCodeSize);
    GetMem(Res, Self.FAuthenticationCodeSize);

    for i := 0 to Self.FAuthenticationCodeSize-1 do
    begin
       Self.FAuthenticationCode[i] := MAC[i];
       PByte(PAnsiChar(Res) + i)^ := Byte(MAC[i]);
    end;

    //return it
    Result := Res;
    Exit;
  end;
  //in AE-1 mode the authentication code is not used
  Result := nil;
end;


procedure TAESCryptoTransform.LoadFileStorageEndBlock(stream : TStream; offset : Int64);
begin
  if Self.FVersion <> 1 then
  begin
     Stream.Seek(Offset, soFromCurrent);
     if (Self.FAuthenticationCode <> nil) then
        FreeMem(Self.FAuthenticationCode);
     GetMem(Self.FAuthenticationCode, Self.FAuthenticationCodeSize);
     stream.Read(Self.FAuthenticationCode^, Self.FAuthenticationCodeSize);
  end;
end;

function TAESCryptoTransform.GetFileStorageEndBlockSize() : Longint;
begin
  if Self.FVersion = 1 then
    Result := 0
  else
    Result := Self.FAuthenticationCodeSize;
end;
(* Implementation of the TAESCryptoTransform class  - END *)

initialization

  InitializeCriticalSection(FCSect);







finalization
  DeleteCriticalSection(FCSect);

end.

{$IFDEF FXC_SET_WARNINGS_ON}
 {$WARNINGS ON}
 {$UNDEF FXC_SET_WARNINGS_ON}
{$ENDIF}


