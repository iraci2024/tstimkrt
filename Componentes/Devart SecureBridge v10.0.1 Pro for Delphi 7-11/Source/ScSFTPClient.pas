
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSFTPClient;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
{$IFNDEF NEXTGEN}
  Contnrs,
{$ENDIF}
  ScCLRClasses, ScConsts, ScBridge, ScTypes, ScUtils, ScThread,
  ScSSHClient, ScSSHChannel, ScSFTPPacket, ScSFTPUtils;

const
  DEFAULT_BLOCK_SIZE = 64 * 1024; // 64Kb
  DEFAULT_PIPELINE_LENGTH = 32;

type
  /// ! syns with TSFTPInternalOperation !
  TScSFTPOperation = (opOpeningFile, opOpeningDir,
    opClosingHandle, opReadingFile, opReadingDir, opWritingFile,
    opRemovingFile, opRenamingFile, opMakingDirectory, opRemovingDirectory,
    opRetrievingFileAttrs, opSettingFileAttrs, opReadingLink, opCreatingLink,
    opRetrievingAbsolutePath, opBlocking, opUnBlocking, opTextSeek,
    opCheckingFile, opQueryingAvailableSpace, opQueryingUserDirectory,
    opRequestExtension, opCopyingRemoteFile, opUnknown);
  TScSFTPOperations = set of TScSFTPOperation;

  TSFTPInternalOperation = (_opOpeningFile, _opOpeningDir,
    _opClosingHandle, _opReadingFile, _opReadingDir, _opWritingFile,
    _opRemovingFile, _opRenamingFile, _opMakingDirectory, _opRemovingDirectory,
    _opRetrievingFileAttrs, _opSettingFileAttrs, _opReadingLink, _opCreatingLink,
    _opRetrievingAbsolutePath, _opBlocking, _opUnBlocking, _opTextSeek,
    _opCheckingFile, _opQueryingAvailableSpace, _opQueryingUserDirectory,
    _opRequestExtension, _opCopyingRemoteFile, _opUnknown,
    _opRetrievingSymbolicFileAttrs, _opRetrievingHandleAttrs,
    _opSettingHandleAttrs, _opCreatingSymLink);
  TSFTPInternalOperations = set of TSFTPInternalOperation;

  TScSFTPOpenFileEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle) of object;

  TScSFTPBeforeReadDataEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle; FileOffset: Int64;
    Count: integer; var Cancel: boolean) of object;

  TScSFTPBeforeWriteDataEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle; FileOffset: Int64;
    var Buffer: TBytes; var Offset, Count: integer; EOF: boolean; var Cancel: boolean) of object;

  TScSFTPDataEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle; FileOffset: Int64;
    const Buffer: TBytes; Offset, Count: integer; EOF: boolean) of object;

  TScSFTPDataChangeEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle; FileOffset: Int64;
    var Buffer: TBytes; var Offset, Count: integer; EOF: boolean) of object;

  TScSFTPDirectoryListEvent = procedure(Sender: TObject;
    const Path: string; const Handle: TScSFTPFileHandle;
    FileInfo: TScSFTPFileInfo; EOF: boolean) of object;

  TScSFTPFileAttributesEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle;
    FileAttributes: TScSFTPFileAttributes) of object;

  TScSFTPFileNameEvent = procedure(Sender: TObject;
    const SrcFileName, DestFileName: string) of object;

  TScSFTPReplyCheckFileEvent = procedure(Sender: TObject;
    const FileName: string; const Handle: TScSFTPFileHandle;
    CheckFileReplyExtension: TScCheckFileReplyExtension) of object;

  TScSFTPReplySpaceAvailableEvent = procedure(Sender: TObject; const Path: string;
    SpaceAvailableReplyExtension: TScSpaceAvailableReplyExtension) of object;

  TScSFTPReplyExtensionEvent = procedure(Sender: TObject; const ExtName: string;
    Extension: TScSFTPExtension) of object;

  TScSFTPSuccessEvent = procedure(Sender: TObject; Operation: TScSFTPOperation;
    const FileName: string; const Handle: TScSFTPFileHandle;
    const Message: string) of object;

  TScSFTPErrorEvent = procedure(Sender: TObject; Operation: TScSFTPOperation;
    const FileName: string; const Handle: TScSFTPFileHandle;
    ErrorCode: integer; const ErrorMessage: string; var Fail: boolean) of object;

  TScSFTPVersionSelectEvent = procedure(Sender: TObject;
    const Versions: TScSFTPVersions; var Version: TScSFTPVersion) of object;

  TScSFTPCreateLocalFileEvent = procedure(Sender: TObject;
    const LocalFileName, RemoteFileName: string; Attrs: TScSFTPFileAttributes;
    var Handle: THandle) of object;

  TScSFTPSetRemoteFileAttributesEvent = procedure(Sender: TObject;
    const LocalFileName, RemoteFileName: string; Attrs: TScSFTPFileAttributes) of object;

  TScSFTPServerProperties = class
  protected
    FFilenameCharset: string;
    FFilenameCharsetAvailable: boolean;
    FNewline: string;
    FNewlineAvailable: boolean;
    FSupportedExtension: TScSFTPSupportedExtension;
    FSupportedExtensionAvailable: boolean;
    FSupportedAcls: TScSFTPSupportedAclExtension;
    FSupportedAclsAvailable: boolean;
    FVendor: TScSFTPVendorExtension;
    FVendorAvailable: boolean;
    FVersions: TScSFTPVersionsExtension;
    FVersionsAvailable: boolean;
    procedure Clear;

  public
    constructor Create;
    destructor Destroy; override;

    property FilenameCharset: string read FFilenameCharset;
    property FilenameCharsetAvailable: boolean read FFilenameCharsetAvailable;
    property Newline: string read FNewline write FNewline;
    property NewlineAvailable: boolean read FNewlineAvailable write FNewlineAvailable;
    property SupportedExtension: TScSFTPSupportedExtension read FSupportedExtension;
    property SupportedExtensionAvailable: boolean read FSupportedExtensionAvailable;
    property SupportedAcls: TScSFTPSupportedAclExtension read FSupportedAcls;
    property SupportedAclsAvailable: boolean read FSupportedAclsAvailable;
    property Vendor: TScSFTPVendorExtension read FVendor;
    property VendorAvailable: boolean read FVendorAvailable;
    property Versions: TScSFTPVersionsExtension read FVersions;
    property VersionsAvailable: boolean read FVersionsAvailable;
  end;

  TScSFTPLoadingFileInfo = class
    Attrs: TScSFTPFileAttributes;
    SrcFileName: string;
    DestFileName: string;
    OverwriteFile: boolean;
    Stream: TStream;
    IsStreamOwner: boolean;
    Buffer: TBytes;
    LoadedCount: integer;
    FileOffset: Int64;
    EOF: boolean;
    IsCanceled: boolean;

    destructor Destroy; override;
    procedure TryCloseStream;
  end;

  TScSFTPPacketInfo = record
    FileOffset: Int64;
    Offset: integer;
    Count: integer;
    IsLoaded: boolean;
    IsEOF: boolean;
  end;

  TScSharedObject = class
  private
    FRefCount: integer;
  public
    constructor Create;
    procedure AddRef;
    procedure Release;
  end;

  TScSFTPBufferInfo = class(TScSharedObject)
    Buffer: TBytes;
    EOF: boolean;
    IsCanceled: boolean;
    PacketInfos: array of TScSFTPPacketInfo;

    procedure ReIndexInfos(Index, NewCount: integer);
    procedure MoveData(StartOffset, Diff: integer);
    procedure SetEOF(StartOffset: integer);
    function GetCount: integer;
    function IsLoaded: boolean;
  end;

  TScSFTPFileItem = class(TScSharedObject)
    FileName: string;
    Handle: TScSFTPFileHandle;
    EOF: boolean;
    IsTextMode: boolean;

    DownloadingFileInfo: TScSFTPLoadingFileInfo;
    UploadingFileInfo: TScSFTPLoadingFileInfo;

    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

  TScSFTPRequestInfo = class
  private
    FBufferInfo: TScSFTPBufferInfo;
    procedure SetBufferInfo(Value: TScSFTPBufferInfo);

  public
    RequestID: integer;
    Operation: TSFTPInternalOperation;
    FileItem: TScSFTPFileItem;
    Path: string;
    RetrievingFileAttrs: TScSFTPFileAttributes;
    DirectoryList: TCRObjectList;
    ReplyExtension: TScSFTPCustomExtension;
    PacketIndex: integer;

    constructor Create(ID: integer; Op: TSFTPInternalOperation; AFileItem: TScSFTPFileItem);
    destructor Destroy; override;
    property BufferInfo: TScSFTPBufferInfo read FBufferInfo write SetBufferInfo;
  end;

  TScSFTPHashAlgorithm =
    (sftphaMD5, sftphaSHA1, sftphaSHA224, sftphaSHA256, sftphaSHA384, sftphaSHA512, sftphaCRC32);
  TScSFTPHashAlgorithms = set of TScSFTPHashAlgorithm;

const
  ScSFTPHashAlgorithmName: array [TScSFTPHashAlgorithm] of string =
    ('md5', 'sha1', 'sha224', 'sha256', 'sha384', 'sha512', 'crc32');
  ScSFTPAllHashAlgorithms = [sftphaMD5, sftphaSHA1, sftphaSHA224, sftphaSHA256, sftphaSHA384, sftphaSHA512, sftphaCRC32];

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSFTPClient = class(TComponent)
  private
    FSSHSubSystem: TScSSHSubSystem;
    FNonBlocking: boolean;
    FPacketWriter: TSFTPPacketWriter;
    FLock: TCriticalSection;
    FVersion: TScSFTPVersion;
    FServerVersion: integer;
    FRequestID: integer;
    FServerProperties: TScSFTPServerProperties;
    FFileList: TCRObjectList;
    FSentRequests: TCRObjectList;
    FReadBlockSize: integer;
    FWriteBlockSize: integer;
    FReceivedFilename: string;
    FReceivedHomeDirectory: string;
    FTempEOLBuffer: TBytes;
    FUseUnicode: boolean;
    FEOF: boolean;
    FPipelineLength: integer;
    FInitialized: boolean;

    FEventsCallMode: TScEventCallMode;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnOpenFile: TScSFTPOpenFileEvent;
    FOnData: TScSFTPDataEvent;
    FBeforeReadData: TScSFTPBeforeReadDataEvent;
    FBeforeWriteDataEx: TScSFTPBeforeWriteDataEvent;
    FAfterWriteData: TScSFTPDataEvent;
    FBeforeWriteData: TScSFTPDataChangeEvent;
    FOnDirectoryList: TScSFTPDirectoryListEvent;
    FOnFileAttributes: TScSFTPFileAttributesEvent;
    FOnFileName: TScSFTPFileNameEvent;
    FOnReplyCheckFile: TScSFTPReplyCheckFileEvent;
    FOnReplySpaceAvailable: TScSFTPReplySpaceAvailableEvent;
    FOnReplyExtension: TScSFTPReplyExtensionEvent;
    FOnSuccess: TScSFTPSuccessEvent;
    FOnError: TScSFTPErrorEvent;
    FOnVersionSelect: TScSFTPVersionSelectEvent;
    FOnCreateLocalFile: TScSFTPCreateLocalFileEvent;
    FOnSetRemoteFileAttributes: TScSFTPSetRemoteFileAttributesEvent;

    procedure SetEventsCallMode(Value: TScEventCallMode);
    function GetSSHClient: TScSSHClient;
    procedure SetSSHClient(Value: TScSSHClient);
    function GetTimeout: integer;
    procedure SetTimeout(Value: integer);
    procedure SetNonBlocking(Value: boolean);
    procedure SetVersion(Value: TScSFTPVersion);
    function GetServerVersion: TScSFTPVersion;
    function GetActive: boolean;
    function FindFileItem(const Handle: TScSFTPFileHandle): TScSFTPFileItem;
    function FindRequestById(RequestID: integer): TScSFTPRequestInfo;
    procedure Transmit;
    procedure TransmitAndProcess;
    function InitRequest(Operation: TSFTPInternalOperation; FileItem: TScSFTPFileItem): TScSFTPRequestInfo;
    function InitRequestByName(Operation: TSFTPInternalOperation; const AName: string): TScSFTPRequestInfo;
    function InitRequestByHandle(Operation: TSFTPInternalOperation; const Handle: TScSFTPFileHandle): TScSFTPRequestInfo;
    function NewRequestID: integer;
    function CreateFileWithAttrs(const Source, Destination: string;
      Attrs: TScSFTPFileAttributes; Overwrite: boolean): THandleStream;
    function DecodeBytes(const Data: TBytes): string;
    function EncodeString(const Str: string): TBytes;
    function RemoveCRLFString(const Source: TBytes; SrcOffset, SrcCount: integer; var Dest: TBytes; const DestEOL: Byte): integer;
    function ChangeEOLString(const Source: TBytes; SrcOffset, SrcCount: integer; var Dest: TBytes; const SrcEOL, DestEOF: Byte): integer;
    function AddCRLFString(const Source: TBytes; SrcOffset, SrcCount: integer; var Dest: TBytes; const SrcEOL: Byte): integer;
    procedure CheckInactive;

    procedure InitOpenFileRequest(RequestInfo: TScSFTPRequestInfo;
      const FileName: string; const Modes: TScSFTPFileOpenModes; Attributes: TScSFTPFileAttributes); overload;
    procedure InitOpenFileRequest(RequestInfo: TScSFTPRequestInfo;
      const FileName: string; const Mode: TScSFTPFileOpenMode; const Flags: TScSFTPFileOpenFlags;
      const BlockModes: TScSFTPBlockModes; const Access: TScSFTPDesiredAccess;
      Attributes: TScSFTPFileAttributes); overload;

    procedure OpenAndDownloadFile(const SourceName, DestinationName: string; Destination: TStream; Overwrite: boolean; FileOffset: Int64);
    procedure InternalDownload(const Handle: TScSFTPFileHandle; DownloadingFileInfo: TScSFTPLoadingFileInfo; LastOperation: TSFTPInternalOperation);
    procedure OpenAndUploadFile(Source: TStream; const SourceName, DestinationName: string; Overwrite: boolean; FileOffset: Int64);
    procedure InternalUpload(const Handle: TScSFTPFileHandle; UploadingFileInfo: TScSFTPLoadingFileInfo; LastOperation: TSFTPInternalOperation);

    procedure InternalReadFile(FileItem: TScSFTPFileItem; BufferInfo: TScSFTPBufferInfo);
    procedure InternalWriteFile(FileItem: TScSFTPFileItem; BufferInfo: TScSFTPBufferInfo);
    procedure ProcessPipelineResult(Count: integer; var LastException: Exception);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ProcessResult;
    procedure DoAsyncReceive(Sender: TObject);
    procedure AsyncProcessResult;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;
    procedure Disconnect;

    procedure CopyRemoteFile(const Source, Destination: string; Overwrite: boolean);

    procedure DownloadFile(const Source, Destination: string; Overwrite: boolean; FileOffset: Int64 = 0);
    procedure DownloadToStream(const SourceName: string; Destination: TStream; FileOffset: Int64 = 0);
    procedure UploadFile(const Source, Destination: string; Overwrite: boolean; FileOffset: Int64 = 0);
    procedure UploadFromStream(Source: TStream; const DestinationName: string; Overwrite: boolean; FileOffset: Int64 = 0);

    function OpenFile(const FileName: string; const Modes: TScSFTPFileOpenModes;
      Attributes: TScSFTPFileAttributes = nil): TScSFTPFileHandle; overload;
    function OpenFile(const FileName: string;
      const Mode: TScSFTPFileOpenMode; const Flags: TScSFTPFileOpenFlags = [];
      const BlockModes: TScSFTPBlockModes = []; const Access: TScSFTPDesiredAccess = [];
      Attributes: TScSFTPFileAttributes = nil): TScSFTPFileHandle; overload;
    function OpenDirectory(const Path: string): TScSFTPFileHandle;
    procedure CloseHandle(const Handle: TScSFTPFileHandle);

    function ReadFile(const Handle: TScSFTPFileHandle; FileOffset: Int64;
      var Buffer: TBytes; Offset, Count: integer): integer;
    procedure WriteFile(const Handle: TScSFTPFileHandle; FileOffset: Int64;
      const Buffer: TBytes; Offset, Count: integer; EOF: boolean = False);

    procedure ReadDirectory(const Handle: TScSFTPFileHandle);
    procedure ReadDirectoryToList(const Handle: TScSFTPFileHandle; List: TCRObjectList);
    procedure RemoveFile(const FileName: string);
    procedure RenameFile(const OldPath, NewPath: string; Flags: TScSFTPRenameFlags = []);
    procedure MakeDirectory(const Path: string; Attributes: TScSFTPFileAttributes = nil);
  {$HPPEMIT '#ifdef RemoveDirectory'}
  {$HPPEMIT '#undef RemoveDirectory'}
  {$HPPEMIT '#endif'}
    procedure RemoveDirectory(const Path: string);
    procedure RetrieveAttributes(Attrs: TScSFTPFileAttributes; const Path: string;
      SymbolicLinks: boolean = False; const Flags: TScSFTPAttributes = []);
    procedure RetrieveAttributesByHandle(Attrs: TScSFTPFileAttributes;
      const Handle: TScSFTPFileHandle; const Flags: TScSFTPAttributes = []);
    procedure SetAttributes(const Path: string; Attributes: TScSFTPFileAttributes);
    procedure SetAttributesByHandle(const Handle: TScSFTPFileHandle; Attributes: TScSFTPFileAttributes);
    function RetrieveAbsolutePath(const Path: string;
      Control: TScSFTPRealpathControl = rcNoCheck; ComposePath: TStringList = nil): string;
    function ReadSymbolicLink(const Path: string): string;
    procedure CreateLink(const LinkPath, TargetPath: string; Symbolic: boolean = True);
    procedure Block(const Handle: TScSFTPFileHandle; Offset, Count: Int64; BlockModes: TScSFTPBlockModes);
    procedure UnBlock(const Handle: TScSFTPFileHandle; Offset, Count: Int64);

    function TextSeek(const Handle: TScSFTPFileHandle; LineNumber: Int64): boolean;
    procedure CheckFile(const FileName: string; StartOffset, Length: Int64;
      BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
    procedure CheckFileByHandle(const Handle: TScSFTPFileHandle; StartOffset, Length: Int64;
      BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
    procedure CheckFileEx(const FileName: string; HashAlgorithm: TScSFTPHashAlgorithm;
      StartOffset, Length: Int64; BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
    procedure CheckFileByHandleEx(const Handle: TScSFTPFileHandle; HashAlgorithm: TScSFTPHashAlgorithm;
      StartOffset, Length: Int64; BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);

    procedure QueryAvailableSpace(const Path: string; ReplyExtension: TScSpaceAvailableReplyExtension = nil);
    function QueryUserHomeDirectory(const Username: string): string;
    procedure RequestExtension(const ExtName: string; const ExtData: TBytes;
      ReplyExtension: TScSFTPExtension = nil); overload;
    procedure RequestExtension(Extension: TScSFTPExtension;
      ReplyExtension: TScSFTPExtension = nil); overload;

  {$HPPEMIT '#ifdef EOF'}
  {$HPPEMIT '#undef EOF'}
  {$HPPEMIT '#endif'}
    function EOF: boolean; overload;
    function EOF(const Handle: TScSFTPFileHandle): boolean; overload;

    property Active: boolean read GetActive;
    property ServerProperties: TScSFTPServerProperties read FServerProperties;
    property ServerVersion: TScSFTPVersion read GetServerVersion;

  published
    property SSHClient: TScSSHClient read GetSSHClient write SetSSHClient;
    property Timeout: integer read GetTimeout write SetTimeout default DEFAULT_TIMEOUT;
    property Version: TScSFTPVersion read FVersion write SetVersion default vSFTP3;
    property ReadBlockSize: integer read FReadBlockSize write FReadBlockSize default DEFAULT_BLOCK_SIZE;
    property WriteBlockSize: integer read FWriteBlockSize write FWriteBlockSize default DEFAULT_BLOCK_SIZE;
    property PipelineLength: integer read FPipelineLength write FPipelineLength default DEFAULT_PIPELINE_LENGTH;
    property UseUnicode: boolean read FUseUnicode write FUseUnicode default True;

    property NonBlocking: boolean read FNonBlocking write SetNonBlocking default False;
    property EventsCallMode: TScEventCallMode read FEventsCallMode write SetEventsCallMode default ecAsynchronous;

    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;

    property OnError: TScSFTPErrorEvent read FOnError write FOnError;
    property OnSuccess: TScSFTPSuccessEvent read FOnSuccess write FOnSuccess;
    property OnOpenFile: TScSFTPOpenFileEvent read FOnOpenFile write FOnOpenFile;
    property OnData: TScSFTPDataEvent read FOnData write FOnData;
    property BeforeReadData: TScSFTPBeforeReadDataEvent read FBeforeReadData write FBeforeReadData;
    property BeforeWriteDataEx: TScSFTPBeforeWriteDataEvent read FBeforeWriteDataEx write FBeforeWriteDataEx;
    property AfterWriteData: TScSFTPDataEvent read FAfterWriteData write FAfterWriteData;
    property BeforeWriteData: TScSFTPDataChangeEvent read FBeforeWriteData write FBeforeWriteData;
    property OnDirectoryList: TScSFTPDirectoryListEvent read FOnDirectoryList write FOnDirectoryList;
    property OnFileAttributes: TScSFTPFileAttributesEvent read FOnFileAttributes write FOnFileAttributes;
    property OnFileName: TScSFTPFileNameEvent read FOnFileName write FOnFileName;
    property OnReplyCheckFile: TScSFTPReplyCheckFileEvent read FOnReplyCheckFile write FOnReplyCheckFile;
    property OnReplySpaceAvailable: TScSFTPReplySpaceAvailableEvent read FOnReplySpaceAvailable write FOnReplySpaceAvailable;
    property OnReplyExtension: TScSFTPReplyExtensionEvent read FOnReplyExtension write FOnReplyExtension;
    property OnVersionSelect: TScSFTPVersionSelectEvent read FOnVersionSelect write FOnVersionSelect;
    property OnCreateLocalFile: TScSFTPCreateLocalFileEvent read FOnCreateLocalFile write FOnCreateLocalFile;
    property OnSetRemoteFileAttributes: TScSFTPSetRemoteFileAttributesEvent
      read FOnSetRemoteFileAttributes write FOnSetRemoteFileAttributes;
  end;

var
  EOL: string = {$IFDEF MSWINDOWS}#13#10{$ELSE}#10{$ENDIF};
  {$EXTERNALSYM EOL}

implementation

uses
{$IFDEF VER17P}
  Types,
{$ENDIF}
  Math, ScFunctions, ScDECUtil,
  ScSSHUtils, ScReaderWriter, ScSFTPConsts;

const
  READ_METADATA_SIZE = 13;
  WRITE_METADATA_SIZE = 29;

const
  SFTP_OPERATION: array [TSFTPInternalOperation] of TScSFTPOperation =
    (opOpeningFile, opOpeningDir,
    opClosingHandle, opReadingFile, opReadingDir, opWritingFile,
    opRemovingFile, opRenamingFile, opMakingDirectory, opRemovingDirectory,
    opRetrievingFileAttrs, opSettingFileAttrs, opReadingLink, opCreatingLink,
    opRetrievingAbsolutePath, opBlocking, opUnBlocking, opTextSeek,
    opCheckingFile, opQueryingAvailableSpace, opQueryingUserDirectory,
    opRequestExtension, opCopyingRemoteFile, opUnknown,
    opRetrievingFileAttrs, opRetrievingFileAttrs,
    opSettingFileAttrs, opCreatingLink);

  SFTP_OPERATION_TYPE: array [TSFTPInternalOperation] of byte =
    (SSH_FXP_OPEN, SSH_FXP_OPENDIR,
    SSH_FXP_CLOSE, SSH_FXP_READ, SSH_FXP_READDIR, SSH_FXP_WRITE,
    SSH_FXP_REMOVE, SSH_FXP_RENAME, SSH_FXP_MKDIR, SSH_FXP_RMDIR,
    SSH_FXP_LSTAT, SSH_FXP_SETSTAT, SSH_FXP_READLINK, SSH_FXP_LINK,
    SSH_FXP_REALPATH, SSH_FXP_BLOCK, SSH_FXP_UNBLOCK, SSH_FXP_EXTENDED,
    SSH_FXP_EXTENDED, SSH_FXP_EXTENDED, SSH_FXP_EXTENDED,
    SSH_FXP_EXTENDED, SSH_FXP_EXTENDED, 0,
    SSH_FXP_STAT, SSH_FXP_FSTAT, SSH_FXP_FSETSTAT, SSH_FXP_SYMLINK);

{ TScSFTPServerProperties }

constructor TScSFTPServerProperties.Create;
begin
  inherited;

  FSupportedExtension := TScSFTPSupportedExtension.Create;
  FSupportedAcls := TScSFTPSupportedAclExtension.Create;
  FVendor := TScSFTPVendorExtension.Create;
  FVersions := TScSFTPVersionsExtension.Create;
end;

destructor TScSFTPServerProperties.Destroy;
begin
  FSupportedExtension.Free;
  FSupportedAcls.Free;
  FVendor.Free;
  FVersions.Free;

  inherited;
end;

procedure TScSFTPServerProperties.Clear;
begin
  FFilenameCharset := '';
  FFilenameCharsetAvailable := False;
  FNewline := '';
  FNewlineAvailable := False;
  TScSFTPExtensionUtils.Clear(FSupportedExtension);
  FSupportedExtensionAvailable := False;
  TScSFTPExtensionUtils.Clear(FSupportedAcls);
  FSupportedAclsAvailable := False;
  TScSFTPExtensionUtils.Clear(FVendor);
  FVendorAvailable := False;
  TScSFTPExtensionUtils.Clear(FVersions);
  FVersionsAvailable := False;
end;

{ TScSFTPLoadingFileInfo }

procedure FreeHandleStream(var Stream: TStream);
begin
  if (Stream <> nil) and not (Stream is TFileStream) and (Stream is THandleStream) and (integer(THandleStream(Stream).Handle) >= 0) then
    FileClose(THandleStream(Stream).Handle);
  Stream.Free;
  Stream := nil;
end;

destructor TScSFTPLoadingFileInfo.Destroy;
begin
  if IsStreamOwner then
    FreeHandleStream(Stream);

  Attrs.Free;

  inherited;
end;

procedure TScSFTPLoadingFileInfo.TryCloseStream;
begin
  if IsStreamOwner then
    FreeHandleStream(Stream);
end;

{ TScSharedObject }

constructor TScSharedObject.Create;
begin
  inherited;

  AddRef;
end;

procedure TScSharedObject.AddRef;
begin
{$IFDEF AUTOREFCOUNT}
  __ObjAddRef;
{$ENDIF}

  Inc(FRefCount);
end;

procedure TScSharedObject.Release;
begin
  if Assigned(Self) then begin
    Assert(FRefCount > 0, ClassName + '.Free RefCount = ' + IntToStr(FRefCount));

    if FRefCount = 1 then
    {$IFNDEF AUTOREFCOUNT}
      Free
    {$ENDIF}
    else
      Dec(FRefCount);

  {$IFDEF AUTOREFCOUNT}
    __ObjRelease;
  {$ENDIF}
  end;
end;

{ TScSFTPBufferInfo }

procedure TScSFTPBufferInfo.ReIndexInfos(Index, NewCount: integer);
var
  Len: integer;
begin
  if PacketInfos[Index].Count <= NewCount then
    raise EScError.Create(seInvalidInputArgs);

  Len := Length(PacketInfos);
  SetLength(PacketInfos, Len + 1);
  PacketInfos[Len].FileOffset := PacketInfos[Index].FileOffset + NewCount;
  PacketInfos[Len].Offset := PacketInfos[Index].Offset + NewCount;
  PacketInfos[Len].Count := PacketInfos[Index].Count - NewCount;
  PacketInfos[Len].IsLoaded := False;
  PacketInfos[Len].IsEOF := False;
  PacketInfos[Index].Count := NewCount;
end;

procedure TScSFTPBufferInfo.MoveData(StartOffset, Diff: integer);
var
  MovedCount: integer;
  LoadedDataOffset: integer;
  i: integer;
begin
  LoadedDataOffset := -1;

  for i := 0 to Length(PacketInfos) - 1 do begin
    if PacketInfos[i].Offset > StartOffset then begin // move data only after StartOffset
      if ((LoadedDataOffset = -1) or (PacketInfos[i].Offset < LoadedDataOffset)) and PacketInfos[i].IsLoaded then
        LoadedDataOffset := PacketInfos[i].Offset;

      PacketInfos[i].Offset := PacketInfos[i].Offset + Diff;
    end;
  end;

  if LoadedDataOffset = -1 then
    Exit;

  MovedCount := 0;

  for i := 0 to Length(PacketInfos) - 1 do begin
    if (PacketInfos[i].Offset >= LoadedDataOffset) and (PacketInfos[i].IsLoaded or not PacketInfos[i].IsEOF) then
      Inc(MovedCount, PacketInfos[i].Count);
  end;

  if MovedCount > 0 then begin
    if (LoadedDataOffset + Diff + MovedCount) > Length(Buffer) then
      SetLength(Buffer, LoadedDataOffset + Diff + MovedCount);
    Move(Buffer[LoadedDataOffset], Buffer[LoadedDataOffset + Diff], MovedCount);
  end;
end;

procedure TScSFTPBufferInfo.SetEOF(StartOffset: integer);
var
  i: integer;
begin
  EOF := True;

  for i := 0 to Length(PacketInfos) - 1 do
    if PacketInfos[i].Offset >= StartOffset then // mark packets begining from StartOffset
      PacketInfos[i].IsEOF := True;
end;

function TScSFTPBufferInfo.GetCount: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to Length(PacketInfos) - 1 do
    if PacketInfos[i].IsLoaded then
      Inc(Result, PacketInfos[i].Count);
end;

function TScSFTPBufferInfo.IsLoaded: boolean;
var
  i: integer;
begin
  Result := True;

  for i := 0 to Length(PacketInfos) - 1 do
    if not PacketInfos[i].IsLoaded and not PacketInfos[i].IsEOF then begin
      Result := False;
      Exit;
    end;
end;

{ TScSFTPFileItem }

constructor TScSFTPFileItem.Create(const FileName: string);
begin
  inherited Create;
  Self.FileName := FileName;
end;

destructor TScSFTPFileItem.Destroy;
begin
  DownloadingFileInfo.Free;
  UploadingFileInfo.Free;

  inherited;
end;

{ TScSFTPRequestInfo }

constructor TScSFTPRequestInfo.Create(ID: integer; Op: TSFTPInternalOperation; AFileItem: TScSFTPFileItem);
begin
  inherited Create;

  RequestID := ID;
  Operation := Op;
  FileItem := AFileItem;
  if FileItem <> nil then
    FileItem.AddRef;
end;

destructor TScSFTPRequestInfo.Destroy;
begin
  if FileItem <> nil then
    FileItem.Release;

  if FBufferInfo <> nil then
    FBufferInfo.Release;

  inherited;
end;

procedure TScSFTPRequestInfo.SetBufferInfo(Value: TScSFTPBufferInfo);
begin
  FBufferInfo := Value;
  if Value <> nil then
    Value.AddRef;
end;

{ TScSFTPClient }

constructor TScSFTPClient.Create(AOwner: TComponent);
begin
  inherited;

  FEventsCallMode := ecAsynchronous;

  FPacketWriter := TSFTPPacketWriter.Create;
  FServerProperties := TScSFTPServerProperties.Create;
  FFileList := TCRObjectList.Create;
  FSentRequests := TCRObjectList.Create;
  FLock := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;

  FReadBlockSize := DEFAULT_BLOCK_SIZE;
  FWriteBlockSize := DEFAULT_BLOCK_SIZE;

  FSSHSubSystem := TScSSHSubSystem.Create(nil);
  FSSHSubSystem.Subsystem := 'sftp';
  FSSHSubSystem.Timeout := DEFAULT_TIMEOUT;
  FVersion := vSFTP3;
  FUseUnicode := True;
  FPipelineLength := DEFAULT_PIPELINE_LENGTH;
end;

destructor TScSFTPClient.Destroy;
begin
  DisposeAsyncEvent(AsyncProcessResult);

  FSSHSubSystem.Free;
  FPacketWriter.Free;
  FServerProperties.Free;
  FSentRequests.Free;
  FFileList.Free;
  FLock.Free;

  inherited;
end;

procedure TScSFTPClient.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSFTPClient) then begin
    TScSFTPClient(Dest).SSHClient := SSHClient;
    TScSFTPClient(Dest).Timeout := Timeout;
    TScSFTPClient(Dest).Version := Version;
    TScSFTPClient(Dest).ReadBlockSize := ReadBlockSize;
    TScSFTPClient(Dest).WriteBlockSize := WriteBlockSize;
    TScSFTPClient(Dest).PipelineLength := PipelineLength;
    TScSFTPClient(Dest).UseUnicode := UseUnicode;
    TScSFTPClient(Dest).NonBlocking := NonBlocking;
    TScSFTPClient(Dest).EventsCallMode := EventsCallMode;
  end
  else
    inherited;
end;

function TScSFTPClient.FindFileItem(const Handle: TScSFTPFileHandle): TScSFTPFileItem;
var
  i: integer;
begin
  for i := 0 to FFileList.Count - 1 do begin
    Result := TObject(FFileList.Items[i]) as TScSFTPFileItem;
    if (Length(Result.Handle) = Length(Handle)) and (MemCompare(@Result.Handle[0], @Handle[0], Length(Handle)) = 0) then begin
      FFileList.Move(i, 0);
      Exit;
    end;
  end;

  raise EScError.Create(seInvalidFileHandle);
end;

function TScSFTPClient.FindRequestById(RequestID: integer): TScSFTPRequestInfo;
var
  i: integer;
begin
  for i := 0 to FSentRequests.Count - 1 do begin
    Result := TObject(FSentRequests.Items[i]) as TScSFTPRequestInfo;
    if Result.RequestID = RequestID then
      Exit;
  end;

  raise EScError.Create(seUnexpectedPacket);
end;

function TScSFTPClient.DecodeBytes(const Data: TBytes): string;
begin
  Result := TScSFTPUtils.DecodeBytes(Data, FUseUnicode);
end;

function TScSFTPClient.EncodeString(const Str: string): TBytes;
begin
  Result := TScSFTPUtils.EncodeString(Str, FUseUnicode);
end;

function TScSFTPClient.RemoveCRLFString(const Source: TBytes; SrcOffset, SrcCount: integer; var Dest: TBytes; const DestEOL: Byte): integer;
var
  SrcEnd: integer;
begin
  if Length(Dest) < SrcCount then
    SetLength(Dest, SrcCount);

  Result := 0;
  SrcEnd := SrcOffset + SrcCount;
  while SrcOffset < SrcEnd do begin
    if (SrcOffset < SrcEnd - 1) and (Source[SrcOffset] = CR) and (Source[SrcOffset + 1] = LF) then begin
      Dest[Result] := DestEOL;
      Inc(SrcOffset, 2);
      Inc(Result);
    end
    else begin
      Dest[Result] := Source[SrcOffset];
      Inc(SrcOffset);
      Inc(Result);
    end;
  end;
end;

function TScSFTPClient.ChangeEOLString(const Source: TBytes; SrcOffset, SrcCount: integer; var Dest: TBytes; const SrcEOL, DestEOF: Byte): integer;
var
  SrcEnd: integer;
begin
  if Length(Dest) < SrcCount then
    SetLength(Dest, SrcCount);

  Result := 0;
  SrcEnd := SrcOffset + SrcCount;
  while SrcOffset < SrcEnd do begin
    if (SrcOffset < SrcEnd - 1) and (Source[SrcOffset] = CR) and (Source[SrcOffset + 1] = LF) then begin
      Dest[Result] := CR;
      Dest[Result + 1] := LF;
      Inc(SrcOffset, 2);
      Inc(Result, 2);
    end
    else
      if Source[SrcOffset] = SrcEOL then begin
        Dest[Result] := DestEOF;
        Inc(SrcOffset);
        Inc(Result);
      end
      else begin
        Dest[Result] := Source[SrcOffset];
        Inc(SrcOffset);
        Inc(Result);
      end;
  end;
end;

function TScSFTPClient.AddCRLFString(const Source: TBytes; SrcOffset, SrcCount: integer; var Dest: TBytes; const SrcEOL: Byte): integer;
var
  SrcEnd: integer;
begin
  if Length(Dest) < (SrcCount * 2) then
    SetLength(Dest, SrcCount * 2);

  Result := 0;
  SrcEnd := SrcOffset + SrcCount;
  while SrcOffset < SrcEnd do begin
    if (SrcOffset < SrcEnd - 1) and (Source[SrcOffset] = CR) and (Source[SrcOffset + 1] = LF) then begin
      Dest[Result] := CR;
      Dest[Result + 1] := LF;
      Inc(SrcOffset, 2);
      Inc(Result, 2);
    end
    else
      if Source[SrcOffset] = SrcEOL then begin
        Dest[Result] := CR;
        Dest[Result + 1] := LF;
        Inc(SrcOffset);
        Inc(Result, 2);
      end
      else begin
        Dest[Result] := Source[SrcOffset];
        Inc(SrcOffset);
        Inc(Result);
      end;
  end;
end;

procedure TScSFTPClient.ProcessResult;

  procedure CheckOperation(Operation: TSFTPInternalOperation; const AllowedOperations: TSFTPInternalOperations);
  begin
    if not (Operation in AllowedOperations) then
      raise EScError.Create(seUnexpectedPacket);
  end;

var
  PacketReader: TSFTPPacketReader;
  TmpBuf: TBytes;
  PacketLength: integer;
  PacketType: SFTPPacketType;
  Operation: TSFTPInternalOperation;
  RequestId, AServerVersion, ErrorCode: integer;
  ErrorMessage: string;
  cnt, i: integer;
  ExtName: string;
  ExtData: TBytes;
  FileInfo: TScSFTPFileInfo;
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
  SelectedVersion: TScSFTPVersion;
  PacketWr: TSSHDataStream;
  Fail: boolean;
  Offset, RequestCount, ReadCount, ConvertedCount: integer;
  EOF: boolean;
  TmpExtension: TScSFTPCustomExtension;
begin
  PacketReader := TSFTPPacketReader.Create(nil);
  try
    PacketReader.CheckAndReallocBuffer(4);
    TmpBuf := PacketReader.Image;
    if FSSHSubSystem.ReadBuffer(TmpBuf, 0, 4) < 4 then
      if not FInitialized and not FSSHSubSystem.Connected then
        raise EScError.Create(seServerNotSupportSFTP)
      else
        raise EScError.Create(seConnectionTimeout);

    PacketLength := PacketReader.ReadInt32;
    if (PacketLength > MAX_SFTP_PACKET_LENGTH) or (PacketLength < 1) then
      raise EScError.Create(seInvalidMessageLength);

    PacketReader.CheckAndReallocBuffer(PacketLength);
    TmpBuf := PacketReader.Image;
    if FSSHSubSystem.ReadBuffer(TmpBuf, PacketReader.Offset, PacketLength) < PacketLength then
      raise EScError.Create(seConnectionTimeout);

    PacketType := PacketReader.ReadPacketType;

    if not FInitialized then begin
      if PacketType <> SSH_FXP_VERSION then
        raise EScError.Create(seUnexpectedPacketType);

      AServerVersion := PacketReader.ReadInt32;
      if (AServerVersion > 6) or (AServerVersion < 0) then
        raise EScError.Create(seUnknownSFTPServerVersion);
      if (AServerVersion < 3) and (PacketReader.Rest > 0) then
        raise EScError.Create(seInvalidMessage);

      FServerVersion := Min(AServerVersion, integer(FVersion));
      SetLength(ExtData, 0);

      while PacketReader.Rest > 0 do begin
        ExtName := Encoding.Default.GetString(PacketReader.ReadString);
        ExtData := PacketReader.ReadString;

        if ExtName = 'newline' then begin
          FServerProperties.FNewline := Encoding.Default.GetString(ExtData);
          FServerProperties.FNewlineAvailable := True;
        end
        else if (ExtName = 'supported') or (ExtName = 'supported2') then begin
          TScSFTPExtensionUtils.ParseExtension(FServerProperties.FSupportedExtension, ExtName, ExtData);
          FServerProperties.FSupportedExtensionAvailable := True;
        end
        else if ExtName = 'acl-supported' then begin
          TScSFTPExtensionUtils.ParseExtension(FServerProperties.FSupportedAcls, ExtName, ExtData);
          FServerProperties.FSupportedAclsAvailable := True;
        end
        else if ExtName = 'vendor-id' then begin
          TScSFTPExtensionUtils.ParseExtension(FServerProperties.FVendor, ExtName, ExtData);
          FServerProperties.FVendorAvailable := True;
        end
        else if ExtName = 'versions' then begin
          TScSFTPExtensionUtils.ParseExtension(FServerProperties.FVersions, ExtName, ExtData);
          FServerProperties.FVersionsAvailable := True;

          if Assigned(OnVersionSelect) then begin
            SelectedVersion := TScSFTPVersion(FServerVersion);
            OnVersionSelect(Self, FServerProperties.FVersions.Versions, SelectedVersion);
            if SelectedVersion <> TScSFTPVersion(FServerVersion) then begin
              PacketWr := TSSHDataStream.Create;
              try
                PacketWr.WriteAStr(IntToStr(integer(SelectedVersion)));
                RequestExtension('version-select', PacketWr.ToBytes);
                FServerVersion := integer(SelectedVersion);
              finally
                PacketWr.Free;
              end;
            end;
          end;
        end
        else if ExtName = 'filename-charset' then begin
          FServerProperties.FFilenameCharset := Encoding.Default.GetString(ExtData);
          FServerProperties.FFilenameCharsetAvailable := True;
        end;
      end;

      FInitialized := True;
    end
    else begin
      ErrorCode := SSH_FX_OK;
      RequestId := PacketReader.ReadInt32;
      RequestInfo := FindRequestById(RequestId);
      FileItem := RequestInfo.FileItem;
      Operation := RequestInfo.Operation;

      try
        case PacketType of
          SSH_FXP_STATUS: begin
            ErrorCode := PacketReader.ReadInt32;
            if FServerVersion >= 3 then begin
              ErrorMessage := DecodeBytes(PacketReader.ReadString);
              PacketReader.ReadString; // language tag
            end
            else
              if ErrorCode = SSH_FX_OK then
                ErrorMessage := 'OK'
              else
                ErrorMessage := 'FAILURE';

            if (ErrorCode = SSH_FX_EOF) and (Operation in [_opReadingFile, _opReadingDir, _opTextSeek]) then begin
              Assert(FileItem <> nil);
              FileItem.EOF := True;
              FEOF := True;

              case Operation of
                _opReadingFile: begin
                  RequestInfo.BufferInfo.SetEOF(RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Offset);

                  try
                    if Assigned(OnData) then
                      OnData(Self, FileItem.FileName, FileItem.Handle,
                        RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].FileOffset,
                        nil, 0, 0, True);
                  finally
                    if FileItem.DownloadingFileInfo <> nil then
                      FileItem.DownloadingFileInfo.EOF := True;
                  end;

                  if FileItem.DownloadingFileInfo = nil then
                    if NonBlocking and not RequestInfo.BufferInfo.IsLoaded then
                      InternalReadFile(FileItem, RequestInfo.BufferInfo);
                end;

                _opReadingDir: begin
                  if Assigned(OnDirectoryList) and (RequestInfo.DirectoryList = nil) then
                    OnDirectoryList(Self, FileItem.FileName, FileItem.Handle, nil, True);
                end;

                _opTextSeek: begin
                  Fail := False;
                  if Assigned(OnError) then
                    OnError(Self, SFTP_OPERATION[Operation], FileItem.FileName, FileItem.Handle, ErrorCode, ErrorMessage, Fail);
                end;
              else
                Assert(False);
              end;
            end
            else
            if ErrorCode = SSH_FX_OK then begin
              if (Operation = _opWritingFile) and Assigned(AfterWriteData) then
                AfterWriteData(Self, FileItem.FileName, FileItem.Handle,
                  RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].FileOffset,
                  RequestInfo.BufferInfo.Buffer,
                  RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Offset,
                  RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Count,
                  RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].IsEOF)
              else
              if Assigned(OnSuccess) then begin
                if FileItem <> nil then begin
                  if Operation = _opClosingHandle then begin
                    if FileItem.UploadingFileInfo <> nil then
                      FileItem.UploadingFileInfo.TryCloseStream;
                    if FileItem.DownloadingFileInfo <> nil then
                      FileItem.DownloadingFileInfo.TryCloseStream;
                  end;

                  OnSuccess(Self, SFTP_OPERATION[Operation], FileItem.FileName, FileItem.Handle, ErrorMessage);
                end
                else
                  OnSuccess(Self, SFTP_OPERATION[Operation], RequestInfo.Path, nil, ErrorMessage);
              end;

              if Operation = _opWritingFile then begin
                if FileItem.UploadingFileInfo <> nil then begin
                  FileItem.UploadingFileInfo.LoadedCount := FileItem.UploadingFileInfo.LoadedCount + RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Count;
                  FileItem.UploadingFileInfo.FileOffset := FileItem.UploadingFileInfo.FileOffset + RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Count;
                end
                else
                  if NonBlocking then
                    InternalWriteFile(FileItem, RequestInfo.BufferInfo);
              end;
            end
            else begin
              if (Operation = _opRetrievingHandleAttrs) and (FileItem <> nil) and (FileItem.DownloadingFileInfo <> nil) then
                // None
              else begin
                Fail := True;
                if Assigned(OnError) then begin
                  if FileItem <> nil then
                    OnError(Self, SFTP_OPERATION[Operation], FileItem.FileName, FileItem.Handle, ErrorCode, ErrorMessage, Fail)
                  else
                    OnError(Self, SFTP_OPERATION[Operation], RequestInfo.Path, nil, ErrorCode, ErrorMessage, Fail);

                  if not NonBlocking and Fail then
                    raise EScSFTPHandledError.Create(ErrorCode, ErrorMessage);
                end
                else
                  if not NonBlocking and Fail then
                    raise EScSFTPError.Create(ErrorCode, ErrorMessage);
              end;
            end;
          end;

          SSH_FXP_HANDLE: begin
            CheckOperation(Operation, [_opOpeningFile, _opOpeningDir]);
            Assert(FileItem <> nil);

            FEOF := False;
            FileItem.EOF := False;
            FileItem.Handle := PacketReader.ReadString;
            if Assigned(OnOpenFile) then
              OnOpenFile(Self, FileItem.FileName, FileItem.Handle);
          end;

          SSH_FXP_DATA: begin
            CheckOperation(Operation, [_opReadingFile]);
            Assert(FileItem <> nil);

            Offset := RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Offset;
            RequestCount := RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Count;
            ReadCount := PacketReader.ReadString(RequestInfo.BufferInfo.Buffer, Offset, RequestCount);
            if ReadCount < RequestCount then
              RequestInfo.BufferInfo.ReIndexInfos(RequestInfo.PacketIndex, ReadCount);

            if (ReadCount > 0) and FileItem.IsTextMode and FServerProperties.FNewlineAvailable and
              (FServerProperties.FNewline <> '') and (FServerProperties.FNewline <> EOL) then begin
              if FServerProperties.FNewline = CRLF then
                ConvertedCount := RemoveCRLFString(RequestInfo.BufferInfo.Buffer, Offset, ReadCount, FTempEOLBuffer, Byte(EOL[1]))
              else
              if EOL = CRLF then
                ConvertedCount := AddCRLFString(RequestInfo.BufferInfo.Buffer, Offset, ReadCount, FTempEOLBuffer, Byte(FServerProperties.FNewline[1]))
              else
                ConvertedCount := ChangeEOLString(RequestInfo.BufferInfo.Buffer, Offset, ReadCount, FTempEOLBuffer, Byte(FServerProperties.FNewline[1]), Byte(EOL[1]));

              if ReadCount <> ConvertedCount then
                RequestInfo.BufferInfo.MoveData(Offset, ConvertedCount - ReadCount);

              Buffer.BlockCopy(FTempEOLBuffer, 0, RequestInfo.BufferInfo.Buffer, Offset, ConvertedCount);
            end
            else
              ConvertedCount := ReadCount;

            RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].Count := ConvertedCount;
            RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].IsLoaded := True;

            if PacketReader.Rest > 0 then
              EOF := PacketReader.ReadBool
            else
              EOF := False;

            if EOF then
              RequestInfo.BufferInfo.SetEOF(Offset);

            FEOF := RequestInfo.BufferInfo.EOF;
            FileItem.EOF := EOF;

            try
              if Assigned(OnData) then
                OnData(Self, FileItem.FileName, FileItem.Handle,
                  RequestInfo.BufferInfo.PacketInfos[RequestInfo.PacketIndex].FileOffset,
                  RequestInfo.BufferInfo.Buffer, Offset, ConvertedCount, EOF);
            finally
              if FileItem.DownloadingFileInfo <> nil then begin
                FileItem.DownloadingFileInfo.LoadedCount := FileItem.DownloadingFileInfo.LoadedCount + ConvertedCount;
                FileItem.DownloadingFileInfo.FileOffset := FileItem.DownloadingFileInfo.FileOffset + ReadCount;
                FileItem.DownloadingFileInfo.EOF := RequestInfo.BufferInfo.EOF;
              end;
            end;

            if FileItem.DownloadingFileInfo = nil then
              if NonBlocking and not RequestInfo.BufferInfo.IsLoaded then
                InternalReadFile(FileItem, RequestInfo.BufferInfo);
          end;

          SSH_FXP_NAME: begin
            CheckOperation(Operation, [_opReadingDir, _opReadingLink, _opRetrievingAbsolutePath, _opQueryingUserDirectory]);
            Assert((Operation <> _opReadingDir) or (FileItem <> nil));

            FileInfo := TScSFTPFileInfo.Create;
            try
              cnt := PacketReader.ReadInt32;
              for i := 1 to cnt do begin
                FileInfo.Filename := DecodeBytes(PacketReader.ReadString);
                if FServerVersion <= 3 then
                  FileInfo.Longname := DecodeBytes(PacketReader.ReadString);

                TScSFTPFileAttributesUtils.ReadFromPacket(FileInfo.Attributes, PacketReader, FServerVersion);

                if Operation = _opReadingDir then begin
                  if RequestInfo.DirectoryList <> nil then begin
                    RequestInfo.DirectoryList.Add(FileInfo);
                    FileInfo := TScSFTPFileInfo.Create;
                  end
                  else
                  if Assigned(OnDirectoryList) then
                    OnDirectoryList(Self, FileItem.FileName, FileItem.Handle, FileInfo, False);
                end;
              end;

              case Operation of
                _opReadingDir: begin
                  if PacketReader.Rest > 0 then
                    FileItem.EOF := PacketReader.ReadBool
                  else
                    FileItem.EOF := False;

                  FEOF := FileItem.EOF;

                  if FileItem.EOF and Assigned(OnDirectoryList) and (RequestInfo.DirectoryList = nil) then
                    OnDirectoryList(Self, FileItem.FileName, FileItem.Handle, nil, True);

                  if NonBlocking and not FileItem.EOF then
                    ReadDirectory(FileItem.Handle);
                end;

                _opReadingLink, _opRetrievingAbsolutePath: begin
                  FReceivedFilename := FileInfo.Filename;
                  if Assigned(OnFileName) then
                    OnFileName(Self, RequestInfo.Path, FReceivedFilename);
                end;

                _opQueryingUserDirectory: begin
                  FReceivedHomeDirectory := FileInfo.Filename;
                  if Assigned(OnFileName) then
                    OnFileName(Self, RequestInfo.Path, FReceivedHomeDirectory);
                end;
              else
                Assert(False);
              end;
            finally
              FileInfo.Free;
            end;
          end;

          SSH_FXP_ATTRS: begin
            CheckOperation(Operation, [_opRetrievingFileAttrs, _opRetrievingSymbolicFileAttrs, _opRetrievingHandleAttrs]);
            Assert(RequestInfo.RetrievingFileAttrs <> nil);
            TScSFTPFileAttributesUtils.ReadFromPacket(RequestInfo.RetrievingFileAttrs, PacketReader, FServerVersion);

            if Assigned(OnFileAttributes) then
              if FileItem <> nil then
                OnFileAttributes(Self, FileItem.FileName, FileItem.Handle, RequestInfo.RetrievingFileAttrs)
              else
                OnFileAttributes(Self, RequestInfo.Path, nil, RequestInfo.RetrievingFileAttrs);

            if (FileItem <> nil) and not FileItem.IsTextMode and (aTextHint in RequestInfo.RetrievingFileAttrs.ValidAttributes) then
              FileItem.IsTextMode := RequestInfo.RetrievingFileAttrs.TextHint = thKnownText;
          end;

          SSH_FXP_EXTENDED_REPLY: begin
            CheckOperation(Operation, [_opRequestExtension, _opCheckingFile, _opQueryingAvailableSpace, _opQueryingUserDirectory]);

            TmpExtension := RequestInfo.ReplyExtension;
            try
              case Operation of
                _opRequestExtension: begin
                  if TmpExtension = nil then
                    TmpExtension := TScSFTPExtension.Create;

                  TScSFTPExtensionUtils.ParseExtension(TmpExtension, RequestInfo.Path, PacketReader.ReadAll);
                  if Assigned(OnReplyExtension) then
                    OnReplyExtension(Self, RequestInfo.Path, TmpExtension as TScSFTPExtension);
                end;

                _opCheckingFile: begin
                  if TmpExtension = nil then
                    TmpExtension := TScCheckFileReplyExtension.Create;

                  if FServerVersion = 5 then
                    ExtName := 'md5-hash'
                  else
                    ExtName := 'check-file';

                  TScSFTPExtensionUtils.ParseExtension(TmpExtension, ExtName, PacketReader.ReadAll);
                  if Assigned(OnReplyCheckFile) then
                    if FileItem <> nil then
                      OnReplyCheckFile(Self, FileItem.FileName, FileItem.Handle, TmpExtension as TScCheckFileReplyExtension)
                    else
                      OnReplyCheckFile(Self, RequestInfo.Path, nil, TmpExtension as TScCheckFileReplyExtension);
                end;

                _opQueryingAvailableSpace: begin
                  if TmpExtension = nil then
                    TmpExtension := TScSpaceAvailableReplyExtension.Create;

                  TScSFTPExtensionUtils.ParseExtension(TmpExtension, 'space-available', PacketReader.ReadAll);
                  if Assigned(OnReplySpaceAvailable) then
                    OnReplySpaceAvailable(Self, RequestInfo.Path, TmpExtension as TScSpaceAvailableReplyExtension);
                end;

                _opQueryingUserDirectory: begin
                  ExtName := Encoding.Default.GetString(PacketReader.ReadString);
                  if ExtName <> 'home-directory' then
                    raise EScError.Create(seUnknownReplyExtensionType);

                  FReceivedHomeDirectory := DecodeBytes(PacketReader.ReadString);
                  if Assigned(OnFileName) then
                    OnFileName(Self, RequestInfo.Path, FReceivedHomeDirectory);
                end;
              else
                Assert(False);
              end;
            finally
              if RequestInfo.ReplyExtension = nil then
                TmpExtension.Free;
            end;
          end;

        else
          raise EScError.Create(seUnexpectedPacketType);
        end;

      finally
        if RequestInfo <> nil then begin
          FSentRequests.Remove(RequestInfo);

          if ((Operation in [_opOpeningFile, _opOpeningDir]) and (ErrorCode <> SSH_FX_OK)) or
            ((Operation = _opClosingHandle) and (ErrorCode = SSH_FX_OK)) then
            if (FileItem <> nil) and (FileItem.FRefCount = 1) and (FFileList.IndexOf(FileItem) >= 0){if was Disconnected} then begin
              FFileList.Remove(FileItem);
              FileItem := nil;
            end;
        end;
      end;

      if NonBlocking and (FileItem <> nil) then begin
        if (FileItem.DownloadingFileInfo <> nil) then
          InternalDownload(FileItem.Handle, FileItem.DownloadingFileInfo, Operation)
        else
        if FileItem.UploadingFileInfo <> nil then
          InternalUpload(FileItem.Handle, FileItem.UploadingFileInfo, Operation);
      end;
    end;
  finally
    PacketReader.Free;
  end;
end;

procedure TScSFTPClient.DoAsyncReceive(Sender: TObject);
begin
  if not FInitialized then
    Exit;

  case FEventsCallMode of
    ecDirectly:
      AsyncProcessResult;
    ecAsynchronous:
      HandleEventAsync(AsyncProcessResult);
    ecSynchronous:
      SynchronizeWithMainThread(AsyncProcessResult);
  else
    Assert(False);
  end;
end;

procedure TScSFTPClient.AsyncProcessResult;
var
  Fail: boolean;
begin
  try
    if FSSHSubSystem.InCount > 0 then
      ProcessResult;
  except
    on E: Exception do
      if Assigned(OnError) then begin
        Fail := False;
        OnError(Self, opUnknown, '', nil, SSH_FX_FAILURE, E.Message, Fail);
      end;
  end;
end;

function TScSFTPClient.GetActive: boolean;
begin
  Assert(FSSHSubSystem <> nil);
  Result := FSSHSubSystem.Connected;
end;

procedure TScSFTPClient.Initialize;
var
  Written: integer;
begin
  if Active then
    raise EScError.Create(seSFTPClientActive);

  if FSSHSubSystem.Client <> nil then begin
    if FSSHSubSystem.Client.Options.SocketReceiveBufferSize = DEFAULT_SOCKET_BUFFER_SIZE then
      FSSHSubSystem.Client.Options.SocketReceiveBufferSize := ReadBlockSize;
    if FSSHSubSystem.Client.Options.SocketSendBufferSize = DEFAULT_SOCKET_BUFFER_SIZE then
      FSSHSubSystem.Client.Options.SocketSendBufferSize := WriteBlockSize;
  end;

  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  FSSHSubSystem.EventsCallMode := ecDirectly;
  FSSHSubSystem.OnConnect := OnConnect;
  FSSHSubSystem.OnDisconnect := OnDisconnect;
  if NonBlocking then
    FSSHSubSystem.OnAsyncReceive := DoAsyncReceive
  else
    FSSHSubSystem.OnAsyncReceive := nil;

  FServerProperties.Clear;
  FSentRequests.Clear;
  FFileList.Clear;
  FRequestID := 0;
  FInitialized := False;

  try
    FSSHSubSystem.Connect;

    FPacketWriter.InitPacket(SSH_FXP_INIT);
    FPacketWriter.WriteInt32(integer(FVersion));
    Written := FSSHSubSystem.WriteBuffer(FPacketWriter.PacketData, 0, FPacketWriter.PacketLength);
    if Written < FPacketWriter.PacketLength then
      raise EScError.Create(seCannotSendData);

    ProcessResult;
  except
    FSSHSubSystem.Disconnect;
    raise;
  end;
end;

procedure TScSFTPClient.Disconnect;
begin
  try
    FSSHSubSystem.Disconnect;
  finally
    FServerProperties.Clear;
    FSentRequests.Clear;
    FFileList.Clear;
  end;
end;

function TScSFTPClient.EOF: boolean;
begin
  if FFileList.Count > 0 then
    Result := FEOF
  else
    Result := True;
end;

function TScSFTPClient.EOF(const Handle: TScSFTPFileHandle): boolean;
begin
  Result := FindFileItem(Handle).EOF;
end;

function TScSFTPClient.NewRequestID: integer;
begin
  Inc(FRequestID);
  Result := FRequestID;
end;

function TScSFTPClient.InitRequestByName(Operation: TSFTPInternalOperation;
  const AName: string): TScSFTPRequestInfo;
var
  FileItem: TScSFTPFileItem;
begin
  if Operation in [_opOpeningFile, _opOpeningDir] then begin
    FileItem := TScSFTPFileItem.Create(AName);
    FFileList.Add(FileItem);
  end
  else
    FileItem := nil;

  Result := InitRequest(Operation, FileItem);
  Result.Path := AName;
end;

function TScSFTPClient.InitRequestByHandle(Operation: TSFTPInternalOperation;
  const Handle: TScSFTPFileHandle): TScSFTPRequestInfo;
var
  FileItem: TScSFTPFileItem;
begin
  FileItem := FindFileItem(Handle);
  Result := InitRequest(Operation, FileItem);
end;

function TScSFTPClient.InitRequest(Operation: TSFTPInternalOperation;
  FileItem: TScSFTPFileItem): TScSFTPRequestInfo;
begin
  FLock.Enter;

  Result := TScSFTPRequestInfo.Create(NewRequestID, Operation, FileItem);
  FSentRequests.Add(Result);

  FPacketWriter.InitPacket(SFTP_OPERATION_TYPE[Operation]);
  FPacketWriter.WriteInt32(Result.RequestID);
end;

procedure TScSFTPClient.Transmit;
var
  PacketLength: integer;
  Written: integer;
begin
  try
    Written := FSSHSubSystem.WriteBuffer(FPacketWriter.PacketData, 0, FPacketWriter.PacketLength);
    PacketLength := FPacketWriter.PacketLength;
  finally
    FLock.Leave;
  end;

  if Written < PacketLength then
    raise EScError.Create(seCannotSendData);
end;

procedure TScSFTPClient.TransmitAndProcess;
begin
  Transmit;
  if not NonBlocking then
    ProcessResult;
end;

function TScSFTPClient.CreateFileWithAttrs(const Source, Destination: string;
  Attrs: TScSFTPFileAttributes; Overwrite: boolean): THandleStream;
var
  Handle: THandle;
{$IFDEF MSWINDOWS}
  dwCreation, dwFlags: cardinal;
{$ELSE}
  Mode: cardinal;
{$ENDIF}
begin
  if Assigned(OnCreateLocalFile) then begin
    Handle := THandle(-1);
    OnCreateLocalFile(Self, Destination, Source, Attrs, Handle);
  end
  else begin
  {$IFDEF MSWINDOWS}
    if aAttrs in Attrs.ValidAttributes then begin
      dwFlags := 0;
      if faReadonly in Attrs.Attrs then
        dwFlags := dwFlags or FILE_ATTRIBUTE_READONLY;
      if faSystem in Attrs.Attrs then
        dwFlags := dwFlags or FILE_ATTRIBUTE_SYSTEM;
      if faHidden in Attrs.Attrs then
        dwFlags := dwFlags or FILE_ATTRIBUTE_HIDDEN;
      if faArchive in Attrs.Attrs then
        dwFlags := dwFlags or FILE_ATTRIBUTE_ARCHIVE;
      if faCompressed in Attrs.Attrs then
        dwFlags := dwFlags or FILE_ATTRIBUTE_COMPRESSED;
    end
    else
      dwFlags := FILE_ATTRIBUTE_NORMAL;

    if Overwrite then
      dwCreation := CREATE_ALWAYS
    else
      dwCreation := CREATE_NEW;

  {$IFNDEF FPC}
    Handle := CreateFile(PChar(Destination), GENERIC_READ or GENERIC_WRITE, 0,
      nil, dwCreation, dwFlags, 0);
  {$ELSE}
    Handle := CreateFileW(PWideChar(ScFunctions.UTF8Decode(Destination)), GENERIC_READ or GENERIC_WRITE, 0,
      nil, dwCreation, dwFlags, 0);
  {$ENDIF}
  {$ELSE}
    if Overwrite then
      Mode := 0
    else
      Mode := {$IFDEF POSIX}fmExclusive{$ELSE}0{$ENDIF};

    Handle := FileCreate(Destination, Mode {$IFDEF POSIX}, FileAccessRights{$ENDIF});
  {$ENDIF}
  end;

  if integer(Handle) < 0 then
    RaiseLastOSError;
  Result := THandleStream.Create(Handle);
end;

procedure TScSFTPClient.DownloadFile(const Source, Destination: string; Overwrite: boolean; FileOffset: Int64 = 0);
begin
  if (Source = '') or (Destination = '') then
    raise EScError.Create(seInvalidInputArgs);

  OpenAndDownloadFile(Source, Destination, nil, Overwrite, FileOffset);
end;

procedure TScSFTPClient.DownloadToStream(const SourceName: string; Destination: TStream; FileOffset: Int64 = 0);
begin
  if (SourceName = '') or (Destination = nil) then
    raise EScError.Create(seInvalidInputArgs);

  OpenAndDownloadFile(SourceName, '', Destination, False, FileOffset);
end;

procedure TScSFTPClient.OpenAndDownloadFile(const SourceName, DestinationName: string; Destination: TStream;
  Overwrite: boolean; FileOffset: Int64);
var
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
  BlockSize: integer;
begin
  RequestInfo := InitRequestByName(_opOpeningFile, SourceName);
  try
    InitOpenFileRequest(RequestInfo, SourceName, [foRead], nil);

    FileItem := RequestInfo.FileItem;
    Assert(FileItem <> nil);

    FileItem.DownloadingFileInfo := TScSFTPLoadingFileInfo.Create;
    FileItem.DownloadingFileInfo.Stream := Destination;
    FileItem.DownloadingFileInfo.IsStreamOwner := Destination = nil;
    FileItem.DownloadingFileInfo.SrcFileName := SourceName;
    FileItem.DownloadingFileInfo.DestFileName := DestinationName;
    FileItem.DownloadingFileInfo.OverwriteFile := Overwrite;
    FileItem.DownloadingFileInfo.FileOffset := FileOffset;
    if FileItem.DownloadingFileInfo.Stream <> nil then
      FileItem.DownloadingFileInfo.Stream.Position := FileItem.DownloadingFileInfo.FileOffset;

    if FServerProperties.SupportedExtensionAvailable and
      (FServerProperties.SupportedExtension.MaxReadSize > 0) and
      (FServerProperties.SupportedExtension.MaxReadSize < FReadBlockSize) then
      BlockSize := FServerProperties.SupportedExtension.MaxReadSize - READ_METADATA_SIZE
    else
      BlockSize := FReadBlockSize - READ_METADATA_SIZE;

    if NonBlocking then
      SetLength(FileItem.DownloadingFileInfo.Buffer, BlockSize)
    else
      SetLength(FileItem.DownloadingFileInfo.Buffer, BlockSize * FPipelineLength);
  finally
    TransmitAndProcess;
  end;

  if NonBlocking then
    Exit;

  try
    if FileItem.DownloadingFileInfo.Stream = nil then begin
      InternalDownload(FileItem.Handle, FileItem.DownloadingFileInfo, _opOpeningFile);
      InternalDownload(FileItem.Handle, FileItem.DownloadingFileInfo, _opRetrievingHandleAttrs);
    end;

    while not FileItem.DownloadingFileInfo.EOF and not FileItem.DownloadingFileInfo.IsCanceled do
      InternalDownload(FileItem.Handle, FileItem.DownloadingFileInfo, _opReadingFile);

    InternalDownload(FileItem.Handle, FileItem.DownloadingFileInfo, _opReadingFile); // CloseHandle
  except
    on Exception do begin
      if FFileList.IndexOf(FileItem) >= 0 then
        CloseHandle(FileItem.Handle);
      raise;
    end;
  end;
end;

procedure TScSFTPClient.InternalDownload(const Handle: TScSFTPFileHandle;
  DownloadingFileInfo: TScSFTPLoadingFileInfo; LastOperation: TSFTPInternalOperation);
begin
  if LastOperation = _opOpeningFile then begin
    if DownloadingFileInfo.Stream = nil then begin
      DownloadingFileInfo.Attrs := TScSFTPFileAttributes.Create;
      RetrieveAttributesByHandle(DownloadingFileInfo.Attrs, Handle);
    end
    else // downloading from stream
      ReadFile(Handle, DownloadingFileInfo.FileOffset, DownloadingFileInfo.Buffer, 0, Length(DownloadingFileInfo.Buffer));
  end
  else
  if LastOperation = _opRetrievingHandleAttrs then begin
    Assert(DownloadingFileInfo.Stream = nil);
    try
      if (DownloadingFileInfo.FileOffset > 0) and FileExists(DownloadingFileInfo.DestFileName) then
        DownloadingFileInfo.Stream := TFileStream.Create(DownloadingFileInfo.DestFileName, fmOpenReadWrite or fmShareExclusive)
      else
        DownloadingFileInfo.Stream := CreateFileWithAttrs(DownloadingFileInfo.SrcFileName, DownloadingFileInfo.DestFileName, DownloadingFileInfo.Attrs, DownloadingFileInfo.OverwriteFile);

      DownloadingFileInfo.Stream.Position := DownloadingFileInfo.FileOffset;
      DownloadingFileInfo.Stream.Size := DownloadingFileInfo.Stream.Position;
    except
      CloseHandle(Handle);
      raise;
    end;

    ReadFile(Handle, DownloadingFileInfo.FileOffset, DownloadingFileInfo.Buffer, 0, Length(DownloadingFileInfo.Buffer));
  end
  else
  if LastOperation = _opReadingFile then begin
    Assert(DownloadingFileInfo.Stream <> nil);
    if DownloadingFileInfo.LoadedCount > 0 then begin
      DownloadingFileInfo.Stream.WriteBuffer(DownloadingFileInfo.Buffer[0], DownloadingFileInfo.LoadedCount);
      DownloadingFileInfo.LoadedCount := 0;
    end;

    if not DownloadingFileInfo.EOF and not DownloadingFileInfo.IsCanceled then
      ReadFile(Handle, DownloadingFileInfo.FileOffset, DownloadingFileInfo.Buffer, 0, Length(DownloadingFileInfo.Buffer))
    else
      CloseHandle(Handle);
  end;
end;

procedure TScSFTPClient.UploadFile(const Source, Destination: string; Overwrite: boolean; FileOffset: Int64 = 0);
begin
  if (Source = '') or (Destination = '') then
    raise EScError.Create(seInvalidInputArgs);

  OpenAndUploadFile(nil, Source, Destination, Overwrite, FileOffset);
end;

procedure TScSFTPClient.UploadFromStream(Source: TStream; const DestinationName: string; Overwrite: boolean; FileOffset: Int64 = 0);
begin
  if (Source = nil) or (DestinationName = '') then
    raise EScError.Create(seInvalidInputArgs);

  OpenAndUploadFile(Source, '', DestinationName, Overwrite, FileOffset);
end;

procedure TScSFTPClient.OpenAndUploadFile(Source: TStream; const SourceName, DestinationName: string;
  Overwrite: boolean; FileOffset: Int64);
var
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
  OpenModes: TScSFTPFileOpenModes;
  BlockSize: integer;
  FileSize: Int64;
begin
  RequestInfo := InitRequestByName(_opOpeningFile, DestinationName);
  try
    if Overwrite then
      OpenModes := [foWrite, foCreate, foTrunc]
    else
      OpenModes := [foWrite, foCreate, foExcl];
    InitOpenFileRequest(RequestInfo, DestinationName, OpenModes, nil);

    FileItem := RequestInfo.FileItem;
    Assert(FileItem <> nil);
    FileItem.UploadingFileInfo := TScSFTPLoadingFileInfo.Create;

    if Source = nil then begin
      FileItem.UploadingFileInfo.Stream := TFileStream.Create(SourceName, fmOpenRead or fmShareDenyWrite);
      FileItem.UploadingFileInfo.IsStreamOwner := True;
    end
    else begin
      FileItem.UploadingFileInfo.Stream := Source;
      FileItem.UploadingFileInfo.IsStreamOwner := False;
    end;

    FileItem.UploadingFileInfo.Stream.Position := FileOffset;

    FileItem.UploadingFileInfo.SrcFileName := SourceName;
    FileItem.UploadingFileInfo.DestFileName := DestinationName;
    FileItem.UploadingFileInfo.FileOffset := FileOffset;

    BlockSize := FWriteBlockSize - WRITE_METADATA_SIZE;
    FileSize := FileItem.UploadingFileInfo.Stream.Size - FileItem.UploadingFileInfo.Stream.Position;
    FileItem.UploadingFileInfo.EOF := FileSize = 0;

    if NonBlocking then
      SetLength(FileItem.UploadingFileInfo.Buffer, Min(BlockSize, FileSize))
    else
      SetLength(FileItem.UploadingFileInfo.Buffer, Min(BlockSize * FPipelineLength, FileSize));
  finally
    TransmitAndProcess;
  end;

  if NonBlocking then
    Exit;

  try
    if not FileItem.UploadingFileInfo.EOF then // file is empty
      InternalUpload(FileItem.Handle, FileItem.UploadingFileInfo, _opOpeningFile);

    while not FileItem.UploadingFileInfo.EOF and not FileItem.UploadingFileInfo.IsCanceled do
      InternalUpload(FileItem.Handle, FileItem.UploadingFileInfo, _opWritingFile);

    InternalUpload(FileItem.Handle, FileItem.UploadingFileInfo, _opWritingFile); // CloseHandle
  except
    on Exception do begin
      if FFileList.IndexOf(FileItem) >= 0 then
        CloseHandle(FileItem.Handle);
      raise;
    end;
  end;
end;

procedure TScSFTPClient.InternalUpload(const Handle: TScSFTPFileHandle;
  UploadingFileInfo: TScSFTPLoadingFileInfo; LastOperation: TSFTPInternalOperation);
var
  Count: integer;
begin
  if (LastOperation = _opOpeningFile) and Assigned(OnSetRemoteFileAttributes) then begin
    if UploadingFileInfo.Attrs = nil then
      UploadingFileInfo.Attrs := TScSFTPFileAttributes.Create;
    OnSetRemoteFileAttributes(Self, UploadingFileInfo.SrcFileName, UploadingFileInfo.DestFileName, UploadingFileInfo.Attrs);
    SetAttributesByHandle(Handle, UploadingFileInfo.Attrs);
  end
  else
  if LastOperation in [_opOpeningFile, _opSettingHandleAttrs, _opWritingFile] then begin
    if (UploadingFileInfo.Stream <> nil) and (UploadingFileInfo.Stream.Position <> UploadingFileInfo.Stream.Size) and
      not UploadingFileInfo.IsCanceled
    then begin
      Count := UploadingFileInfo.Stream.Read(UploadingFileInfo.Buffer[0], Length(UploadingFileInfo.Buffer));
      if Count = 0 then
        raise EScError.Create(seStreamReadError);

      UploadingFileInfo.EOF := UploadingFileInfo.Stream.Position = UploadingFileInfo.Stream.Size;
      WriteFile(Handle, UploadingFileInfo.FileOffset, UploadingFileInfo.Buffer, 0, Count, UploadingFileInfo.EOF);
    end
    else begin
      UploadingFileInfo.EOF := True;
      CloseHandle(Handle);
    end;
  end;
end;

procedure TScSFTPClient.InitOpenFileRequest(RequestInfo: TScSFTPRequestInfo;
  const FileName: string; const Modes: TScSFTPFileOpenModes; Attributes: TScSFTPFileAttributes);
var
  Mode: TScSFTPFileOpenMode;
  Flags: TScSFTPFileOpenFlags;
  Access: TScSFTPDesiredAccess;
  BlockModes: TScSFTPBlockModes;
  SupportedExt: TScSFTPSupportedExtension;
begin
  if FServerVersion >= 5 then begin
    ConvertOpenModesVer3ToVer5(Modes, Mode, Flags, BlockModes, Access);
    InitOpenFileRequest(RequestInfo, FileName, Mode, Flags, BlockModes, Access, Attributes);
    Exit;
  end;

  FPacketWriter.WriteAsString(EncodeString(FileName));
  FPacketWriter.WriteInt32(ConvertFileOpenModesToSFTPValue(Modes, FServerVersion));
  if Attributes <> nil then begin
    if FServerProperties.SupportedExtensionAvailable then
      SupportedExt := FServerProperties.SupportedExtension
    else
      SupportedExt := nil;
    TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FServerVersion, SupportedExt);
  end
  else
    TScSFTPFileAttributesUtils.WriteToPacketDefaultValues(FPacketWriter, ftUnknown, FServerVersion);

  RequestInfo.FileItem.IsTextMode := foText in Modes;
end;

procedure TScSFTPClient.InitOpenFileRequest(RequestInfo: TScSFTPRequestInfo;
  const FileName: string; const Mode: TScSFTPFileOpenMode; const Flags: TScSFTPFileOpenFlags;
  const BlockModes: TScSFTPBlockModes; const Access: TScSFTPDesiredAccess;
  Attributes: TScSFTPFileAttributes);
var
  Modes: TScSFTPFileOpenModes;
  SupportedExt: TScSFTPSupportedExtension;
  OpenFlags: integer;
  UseBlockModes: boolean;
begin
  if FServerVersion < 5 then begin
    ConvertOpenModesVer5ToVer3(Mode, Flags, Access, Modes);
    InitOpenFileRequest(RequestInfo, FileName, Modes, Attributes);
    Exit;
  end;

  if FServerProperties.SupportedExtensionAvailable then
    SupportedExt := FServerProperties.SupportedExtension
  else
    SupportedExt := nil;

  UseBlockModes := True;

  OpenFlags := ConvertFileOpenFlagsToSFTPValue(Flags);
  if SupportedExt <> nil then begin
    TScSFTPSupportedExtensionUtils.CheckSupportedOpenFlags(SupportedExt, OpenFlags);

    UseBlockModes := SupportedExt.IsOpenBlockSetAvailable;
    if UseBlockModes then
      TScSFTPSupportedExtensionUtils.CheckSupportedOpenBlockSet(SupportedExt, BlockModes);
  end;

  OpenFlags := OpenFlags or ConvertFileOpenModeToSFTPValue(Mode);
  if UseBlockModes then
    OpenFlags := OpenFlags or ConvertBlockModesToSFTPValue(BlockModes);

  FPacketWriter.WriteAsString(EncodeString(FileName));
  FPacketWriter.WriteInt32(ConvertAceMaskToSFTPValue(Access));
  FPacketWriter.WriteInt32(OpenFlags);
  if Attributes <> nil then
    TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FServerVersion, SupportedExt)
  else
    TScSFTPFileAttributesUtils.WriteToPacketDefaultValues(FPacketWriter, ftUnknown, FServerVersion);

  RequestInfo.FileItem.IsTextMode := ofTextMode in Flags;
end;

function TScSFTPClient.OpenFile(const FileName: string; const Modes: TScSFTPFileOpenModes;
  Attributes: TScSFTPFileAttributes = nil): TScSFTPFileHandle;
var
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
begin
  RequestInfo := InitRequestByName(_opOpeningFile, FileName);
  try
    InitOpenFileRequest(RequestInfo, FileName, Modes, Attributes);
    FileItem := RequestInfo.FileItem;
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then begin
    Assert(FileItem <> nil);
    Result := FileItem.Handle;
  end
  else
    Result := nil;
end;

function TScSFTPClient.OpenFile(const FileName: string;
  const Mode: TScSFTPFileOpenMode; const Flags: TScSFTPFileOpenFlags = [];
  const BlockModes: TScSFTPBlockModes = []; const Access: TScSFTPDesiredAccess = [];
  Attributes: TScSFTPFileAttributes = nil): TScSFTPFileHandle;
var
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
begin
  RequestInfo := InitRequestByName(_opOpeningFile, FileName);
  try
    InitOpenFileRequest(RequestInfo, FileName, Mode, Flags, BlockModes, Access, Attributes);
    FileItem := RequestInfo.FileItem;
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then begin
    Assert(FileItem <> nil);
    Result := FileItem.Handle;
  end
  else
    Result := nil;
end;

function TScSFTPClient.OpenDirectory(const Path: string): TScSFTPFileHandle;
var
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
begin
  RequestInfo := InitRequestByName(_opOpeningDir, Path);
  try
    FPacketWriter.WriteAsString(EncodeString(Path));
    FileItem := RequestInfo.FileItem;
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then
    Result := FileItem.Handle
  else
    Result := nil;
end;

procedure TScSFTPClient.CloseHandle(const Handle: TScSFTPFileHandle);
begin
  InitRequestByHandle(_opClosingHandle, Handle);
  try
    FPacketWriter.WriteAsString(Handle);
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.ReadDirectory(const Handle: TScSFTPFileHandle);
var
  RequestInfo: TScSFTPRequestInfo;
begin
  RequestInfo := InitRequestByHandle(_opReadingDir, Handle);
  try
    FPacketWriter.WriteAsString(Handle);
    RequestInfo.FileItem.EOF := False;
    FEOF := False;
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.ReadDirectoryToList(const Handle: TScSFTPFileHandle; List: TCRObjectList);
var
  FileItem: TScSFTPFileItem;
  RequestInfo: TScSFTPRequestInfo;
begin
  if NonBlocking then
    raise EScError.Create(seOperationNotAllowedWhenNonBlocking);

  if List = nil then
    raise EScError.Create(seInvalidInputArgs);

  List.Clear;
  FileItem := FindFileItem(Handle);

  while not FileItem.EOF do begin
    RequestInfo := InitRequest(_opReadingDir, FileItem);
    try
      RequestInfo.DirectoryList := List;

      FPacketWriter.WriteAsString(Handle);
    finally
      TransmitAndProcess;
    end;
  end;
end;


procedure TScSFTPClient.ProcessPipelineResult(Count: integer; var LastException: Exception);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    try
      ProcessResult;
    except
      on E: EScSFTPError do
        if LastException = nil then
          LastException := EScSFTPError.Create(E.SFTPErrorCode, E.Message);
      on EScError do
        raise;
      on E: Exception do
        if LastException = nil then
          LastException := Exception.Create(E.Message);
    end;
end;

function TScSFTPClient.ReadFile(const Handle: TScSFTPFileHandle; FileOffset: Int64;
  var Buffer: TBytes; Offset, Count: integer): integer;
var
  FileItem: TScSFTPFileItem;
  BufferInfo: TScSFTPBufferInfo;
  MaxBlockSize, BlockSize, RemainedWindowSize: integer;
  PacketCount: integer;
  i: integer;
begin
  Result := 0;
  if Count = 0 then
    Exit;

  if (Buffer = nil) and not NonBlocking then
    raise EScError.Create(seInvalidInputArgs);

  if (Buffer <> nil) and (Length(Buffer) < (Offset + Count)) then
    raise EScError.Create(seInvalidInputArgs);

  if Buffer = nil then begin
    SetLength(Buffer, Count);
    Offset := 0;
  end;

  if FServerProperties.SupportedExtensionAvailable and
    (FServerProperties.SupportedExtension.MaxReadSize > 0) and
    (FServerProperties.SupportedExtension.MaxReadSize < FReadBlockSize) then
    MaxBlockSize := FServerProperties.SupportedExtension.MaxReadSize - READ_METADATA_SIZE
  else
    MaxBlockSize := FReadBlockSize - READ_METADATA_SIZE;

  FEOF := False;
  FileItem := FindFileItem(Handle);
  BufferInfo := TScSFTPBufferInfo.Create;
  try
    BufferInfo.Buffer := Buffer;
    PacketCount := (Count + MaxBlockSize - 1) div MaxBlockSize;
    SetLength(BufferInfo.PacketInfos, PacketCount);
    BlockSize := MaxBlockSize;

    for i := 0 to PacketCount - 1 do begin
      if (FileItem.DownloadingFileInfo <> nil) and not NonBlocking then begin
        if i = 0 then begin
          RemainedWindowSize := FSSHSubSystem.GetRemainedWindowSize mod FReadBlockSize;
          if RemainedWindowSize = 0 then
            BlockSize := MaxBlockSize
          else
          if RemainedWindowSize <= READ_METADATA_SIZE then
            BlockSize := MaxBlockSize + (READ_METADATA_SIZE - RemainedWindowSize)
          else
          if BlockSize > RemainedWindowSize - READ_METADATA_SIZE then begin
            if Count > MaxBlockSize - (RemainedWindowSize - READ_METADATA_SIZE) then
              Dec(Count, MaxBlockSize - (RemainedWindowSize - READ_METADATA_SIZE));
            BlockSize := RemainedWindowSize - READ_METADATA_SIZE;
          end;
        end
        else
          BlockSize := MaxBlockSize;
      end;

      BufferInfo.PacketInfos[i].FileOffset := FileOffset;
      BufferInfo.PacketInfos[i].Offset := Offset;
      BufferInfo.PacketInfos[i].Count := Min(Count, BlockSize);
      BufferInfo.PacketInfos[i].IsLoaded := False;
      BufferInfo.PacketInfos[i].IsEOF := False;
      Dec(Count, BlockSize);
      Inc(Offset, BlockSize);
      Inc(FileOffset, BlockSize);
    end;

    repeat
      InternalReadFile(FileItem, BufferInfo);
    until NonBlocking or BufferInfo.IsLoaded or BufferInfo.IsCanceled; // To finish loadind when server returns less data

    if (FileItem.DownloadingFileInfo <> nil) and FileItem.DownloadingFileInfo.IsCanceled then
      raise EScError.Create(seFileDownloadInterrupted);

    if NonBlocking then
      Result := 0
    else begin
      Buffer := BufferInfo.Buffer;
      Result := BufferInfo.GetCount;
    end;
  finally
    BufferInfo.Release;
  end;
end;

procedure TScSFTPClient.InternalReadFile(FileItem: TScSFTPFileItem; BufferInfo: TScSFTPBufferInfo);
var
  LastException: Exception;
  RequestInfo: TScSFTPRequestInfo;
  Cancel: boolean;
  i, No: integer;
begin
  Assert(FileItem <> nil);
  Assert(BufferInfo <> nil);

  Cancel := False;
  LastException := nil;
  No := 0;
  i := 0;
  while i < Length(BufferInfo.PacketInfos) do begin
    if not BufferInfo.PacketInfos[i].IsLoaded and not BufferInfo.PacketInfos[i].IsEOF then begin
      if Assigned(BeforeReadData) then
        BeforeReadData(Self, FileItem.FileName, FileItem.Handle, BufferInfo.PacketInfos[i].FileOffset,
          BufferInfo.PacketInfos[i].Count, Cancel);

      if Cancel then begin
        BufferInfo.IsCanceled := True;
        if FileItem.DownloadingFileInfo <> nil then
          FileItem.DownloadingFileInfo.IsCanceled := True;
        break;
      end;

      RequestInfo := InitRequest(_opReadingFile, FileItem);
      try
        FPacketWriter.WriteAsString(FileItem.Handle);
        FPacketWriter.WriteInt64(BufferInfo.PacketInfos[i].FileOffset);
        FPacketWriter.WriteInt32(BufferInfo.PacketInfos[i].Count);

        RequestInfo.BufferInfo := BufferInfo;
        RequestInfo.PacketIndex := i;
        Inc(No);
      finally
        Transmit;
      end;

      if NonBlocking then
        Exit;
    end;

    if No >= FPipelineLength then begin
      ProcessPipelineResult(No, LastException);
      No := 0;
    end;

    Inc(i);
  end;

  if NonBlocking then
    Exit;

  ProcessPipelineResult(No, LastException);

  if LastException <> nil then
    raise LastException;
end;

procedure TScSFTPClient.WriteFile(const Handle: TScSFTPFileHandle; FileOffset: Int64;
  const Buffer: TBytes; Offset, Count: integer; EOF: boolean = False);
var
  FileItem: TScSFTPFileItem;
  BufferInfo: TScSFTPBufferInfo;
  BlockSize: integer;
  PacketCount: integer;
  i: integer;
begin
  if Count = 0 then
    Exit;

  if Buffer = nil then
    raise EScError.Create(seInvalidInputArgs);

  FEOF := EOF;
  FileItem := FindFileItem(Handle);
  BufferInfo := TScSFTPBufferInfo.Create;
  try
    BufferInfo.Buffer := Buffer;

    if Assigned(BeforeWriteData) then
      BeforeWriteData(Self, FileItem.FileName, FileItem.Handle, FileOffset,
        BufferInfo.Buffer, Offset, Count, EOF);

    BlockSize := FWriteBlockSize - WRITE_METADATA_SIZE;
    PacketCount := (Count + BlockSize - 1) div BlockSize;
    SetLength(BufferInfo.PacketInfos, PacketCount);

    for i := 0 to PacketCount - 1 do begin
      BufferInfo.PacketInfos[i].FileOffset := FileOffset;
      BufferInfo.PacketInfos[i].Offset := Offset;
      BufferInfo.PacketInfos[i].Count := Min(Count, BlockSize);
      BufferInfo.PacketInfos[i].IsLoaded := False;
      BufferInfo.PacketInfos[i].IsEOF := False;
      Dec(Count, BlockSize);
      Inc(Offset, BlockSize);
      Inc(FileOffset, BlockSize);
    end;

    BufferInfo.PacketInfos[PacketCount - 1].IsEOF := EOF;
    InternalWriteFile(FileItem, BufferInfo);

    if (FileItem.UploadingFileInfo <> nil) and FileItem.UploadingFileInfo.IsCanceled then
      raise EScError.Create(seFileUploadInterrupted);
  finally
    BufferInfo.Release;
  end;
end;

procedure TScSFTPClient.InternalWriteFile(FileItem: TScSFTPFileItem; BufferInfo: TScSFTPBufferInfo);
var
  LastException: Exception;
  RequestInfo: TScSFTPRequestInfo;
  Cancel: boolean;
  i, No: integer;
begin
  Assert(FileItem <> nil);
  Assert(BufferInfo <> nil);

  Cancel := False;
  LastException := nil;
  No := 0;
  i := 0;
  while i < Length(BufferInfo.PacketInfos) do begin
    if not BufferInfo.PacketInfos[i].IsLoaded then begin
      if Assigned(BeforeWriteDataEx) then
        BeforeWriteDataEx(Self, FileItem.FileName, FileItem.Handle, BufferInfo.PacketInfos[i].FileOffset,
          BufferInfo.Buffer, BufferInfo.PacketInfos[i].Offset, BufferInfo.PacketInfos[i].Count, EOF, Cancel);

      if Cancel then begin
        BufferInfo.IsCanceled := True;
        if FileItem.UploadingFileInfo <> nil then
          FileItem.UploadingFileInfo.IsCanceled := True;
        break;
      end;

      BufferInfo.PacketInfos[i].IsLoaded := True;
      RequestInfo := InitRequest(_opWritingFile, FileItem);
      try
        FPacketWriter.WriteAsString(FileItem.Handle);
        FPacketWriter.WriteInt64(BufferInfo.PacketInfos[i].FileOffset);
        FPacketWriter.WriteAsString(TValueArr(BufferInfo.Buffer), BufferInfo.PacketInfos[i].Offset, BufferInfo.PacketInfos[i].Count);

        RequestInfo.BufferInfo := BufferInfo;
        RequestInfo.PacketIndex := i;
        Inc(No);
      finally
        Transmit;
      end;

      if NonBlocking then
        Exit;
    end;

    if No >= FPipelineLength then begin
      ProcessPipelineResult(No, LastException);
      No := 0;
    end;

    Inc(i);
  end;

  if NonBlocking then
    Exit;

  ProcessPipelineResult(No, LastException);

  if LastException <> nil then
    raise LastException;
end;

procedure TScSFTPClient.RemoveFile(const FileName: string);
begin
  InitRequestByName(_opRemovingFile, FileName);
  try
    FPacketWriter.WriteAsString(EncodeString(FileName));
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.RenameFile(const OldPath, NewPath: string; Flags: TScSFTPRenameFlags = []);
begin
  if FServerVersion < 2 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  InitRequestByName(_opRenamingFile, OldPath);
  try
    FPacketWriter.WriteAsString(EncodeString(OldPath));
    FPacketWriter.WriteAsString(EncodeString(NewPath));
    if FServerVersion >= 5 then
      FPacketWriter.WriteInt32(ConvertRenameFlagsToSFTPValue(Flags));
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.MakeDirectory(const Path: string; Attributes: TScSFTPFileAttributes = nil);
var
  SupportedExt: TScSFTPSupportedExtension;
begin
  InitRequestByName(_opMakingDirectory, Path);
  try
    FPacketWriter.WriteAsString(EncodeString(Path));
    if Attributes <> nil then begin
      if FServerProperties.SupportedExtensionAvailable then
        SupportedExt := FServerProperties.SupportedExtension
      else
        SupportedExt := nil;
      TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FServerVersion, SupportedExt);
    end
    else
      TScSFTPFileAttributesUtils.WriteToPacketDefaultValues(FPacketWriter, ftUnknown, FServerVersion);
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.RemoveDirectory(const Path: string);
begin
  InitRequestByName(_opRemovingDirectory, Path);
  try
    FPacketWriter.WriteAsString(EncodeString(Path));
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.RetrieveAttributes(Attrs: TScSFTPFileAttributes; const Path: string;
  SymbolicLinks: boolean = False; const Flags: TScSFTPAttributes = []);
var
  Op: TSFTPInternalOperation;
  RequestInfo: TScSFTPRequestInfo;
begin
  if Attrs = nil then
    raise EScError.Create(seInvalidInputArgs);

  if SymbolicLinks then
    Op := _opRetrievingSymbolicFileAttrs
  else
    Op := _opRetrievingFileAttrs;

  RequestInfo := InitRequestByName(Op, Path);
  try
    RequestInfo.RetrievingFileAttrs := Attrs;

    FPacketWriter.WriteAsString(EncodeString(Path));
    if FServerVersion >= 4 then
      FPacketWriter.WriteInt32(ConvertSFTPAttributesToSFTPValue(Flags, FServerVersion));
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.RetrieveAttributesByHandle(Attrs: TScSFTPFileAttributes;
  const Handle: TScSFTPFileHandle; const Flags: TScSFTPAttributes = []);
var
  RequestInfo: TScSFTPRequestInfo;
begin
  if Attrs = nil then
    raise EScError.Create(seInvalidInputArgs);

  RequestInfo := InitRequestByHandle(_opRetrievingHandleAttrs, Handle);
  try
    RequestInfo.RetrievingFileAttrs := Attrs;

    FPacketWriter.WriteAsString(Handle);
    if FServerVersion >= 4 then
      FPacketWriter.WriteInt32(ConvertSFTPAttributesToSFTPValue(Flags, FServerVersion));
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.SetAttributes(const Path: string; Attributes: TScSFTPFileAttributes);
var
  SupportedExt: TScSFTPSupportedExtension;
begin
  if Attributes = nil then
    raise EScError.Create(seInvalidInputArgs);

  InitRequestByName(_opSettingFileAttrs, Path);
  try
    FPacketWriter.WriteAsString(EncodeString(Path));
    if FServerProperties.SupportedExtensionAvailable then
      SupportedExt := FServerProperties.SupportedExtension
    else
      SupportedExt := nil;
    TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FServerVersion, SupportedExt);
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.SetAttributesByHandle(const Handle: TScSFTPFileHandle; Attributes: TScSFTPFileAttributes);
var
  SupportedExt: TScSFTPSupportedExtension;
begin
  if Attributes = nil then
    raise EScError.Create(seInvalidInputArgs);

  InitRequestByHandle(_opSettingHandleAttrs, Handle);
  try
    FPacketWriter.WriteAsString(Handle);
    if FServerProperties.SupportedExtensionAvailable then
      SupportedExt := FServerProperties.SupportedExtension
    else
      SupportedExt := nil;
    TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FServerVersion, SupportedExt);
  finally
    TransmitAndProcess;
  end;
end;

function TScSFTPClient.RetrieveAbsolutePath(const Path: string;
  Control: TScSFTPRealpathControl = rcNoCheck; ComposePath: TStringList = nil): string;
var
  i: integer;
begin
  InitRequestByName(_opRetrievingAbsolutePath, Path);
  try
    FPacketWriter.WriteAsString(EncodeString(Path));

    if FServerVersion >= 6 then begin
      FPacketWriter.WriteByte(ConvertRealpathControlToSFTPValue(Control));

      if ComposePath <> nil then
        for i := 0 to ComposePath.Count - 1 do
          FPacketWriter.WriteAsString(EncodeString(ComposePath[i]));
    end;
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then
    Result := FReceivedFilename
  else
    Result := '';
end;

function TScSFTPClient.ReadSymbolicLink(const Path: string): string;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  InitRequestByName(_opReadingLink, Path);
  try
    FPacketWriter.WriteAsString(EncodeString(Path));
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then
    Result := FReceivedFilename
  else
    Result := '';
end;

procedure TScSFTPClient.CreateLink(const LinkPath, TargetPath: string; Symbolic: boolean = True);
var
  Op: TSFTPInternalOperation;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerVersion < 6 then
    Op := _opCreatingSymLink
  else
    Op := _opCreatingLink;

  InitRequestByName(Op, LinkPath);
  try
    FPacketWriter.WriteAsString(EncodeString(LinkPath));
    FPacketWriter.WriteAsString(EncodeString(TargetPath));
    if FServerVersion >= 6 then
      FPacketWriter.WriteBool(Symbolic);
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.Block(const Handle: TScSFTPFileHandle; Offset, Count: Int64;
  BlockModes: TScSFTPBlockModes);
begin
  if FServerVersion < 6 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerProperties.SupportedExtensionAvailable then begin
    if not FServerProperties.SupportedExtension.IsBlockSetAvailable then
      Exit;

    TScSFTPSupportedExtensionUtils.CheckSupportedBlockSet(FServerProperties.SupportedExtension, BlockModes);
  end;

  InitRequestByHandle(_opBlocking, Handle);
  try
    FPacketWriter.WriteAsString(Handle);
    FPacketWriter.WriteInt64(Offset);
    FPacketWriter.WriteInt64(Count);
    FPacketWriter.WriteInt32(ConvertBlockModesToSFTPValue(BlockModes));
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.UnBlock(const Handle: TScSFTPFileHandle; Offset, Count: Int64);
begin
  if FServerVersion < 6 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  InitRequestByHandle(_opUnBlocking, Handle);
  try
    FPacketWriter.WriteAsString(Handle);
    FPacketWriter.WriteInt64(Offset);
    FPacketWriter.WriteInt64(Count);
  finally
    TransmitAndProcess;
  end;
end;

function TScSFTPClient.TextSeek(const Handle: TScSFTPFileHandle; LineNumber: Int64): boolean;
var
  RequestInfo: TScSFTPRequestInfo;
  FileItem: TScSFTPFileItem;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  RequestInfo := InitRequestByHandle(_opTextSeek, Handle);
  try
    FPacketWriter.WriteAStr('text-seek');
    FPacketWriter.WriteAsString(Handle);
    FPacketWriter.WriteInt64(LineNumber);
    FileItem := RequestInfo.FileItem;
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then
    Result := not FileItem.EOF
  else
    Result := True;
end;

procedure TScSFTPClient.CheckFile(const FileName: string; StartOffset, Length: Int64;
  BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
var
  RequestInfo: TScSFTPRequestInfo;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerVersion = 5 then begin
    if FServerProperties.SupportedExtensionAvailable then
      TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'md5-hash');

    RequestInfo := InitRequestByName(_opCheckingFile, FileName);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('md5-hash');
      FPacketWriter.WriteAsString(EncodeString(FileName));
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteAStr('');
    finally
      TransmitAndProcess;
    end;
  end
  else begin
    if FServerProperties.SupportedExtensionAvailable then
      TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'check-file-name');

    RequestInfo := InitRequestByName(_opCheckingFile, FileName);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('check-file-name');
      FPacketWriter.WriteAsString(EncodeString(FileName));
      FPacketWriter.WriteAStr('sha1,md5');
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteInt32(BlockSize);
    finally
      TransmitAndProcess;
    end;
  end;
end;

procedure TScSFTPClient.CheckFileByHandle(const Handle: TScSFTPFileHandle; StartOffset, Length: Int64;
  BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
var
  RequestInfo: TScSFTPRequestInfo;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerVersion = 5 then begin
    if FServerProperties.SupportedExtensionAvailable then
      TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'md5-hash-handle');

    RequestInfo := InitRequestByHandle(_opCheckingFile, Handle);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('md5-hash-handle');
      FPacketWriter.WriteAsString(Handle);
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteAStr('');
    finally
      TransmitAndProcess;
    end;
  end
  else begin
    if FServerProperties.SupportedExtensionAvailable then
      TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'check-file-handle');

    RequestInfo := InitRequestByHandle(_opCheckingFile, Handle);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('check-file-handle');
      FPacketWriter.WriteAsString(Handle);
      FPacketWriter.WriteAStr('sha1,md5');
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteInt32(BlockSize);
    finally
      TransmitAndProcess;
    end;
  end;
end;

procedure TScSFTPClient.CheckFileEx(const FileName: string;
  HashAlgorithm: TScSFTPHashAlgorithm; StartOffset, Length: Int64;
  BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
var
  RequestInfo: TScSFTPRequestInfo;
  ExtensionFound: boolean;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerVersion = 5 then  begin
    if HashAlgorithm <> sftphaMD5 then
      raise EScError.Create(seUnsupportedSFTPOperation);

    if FServerProperties.SupportedExtensionAvailable then
      TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'md5-hash');

    RequestInfo := InitRequestByName(_opCheckingFile, FileName);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('md5-hash');
      FPacketWriter.WriteAsString(EncodeString(FileName));
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteAStr('');
    finally
      TransmitAndProcess;
    end;
  end
  else begin
    ExtensionFound := False;

    if FServerProperties.SupportedExtensionAvailable then
      if FServerProperties.SupportedExtension.SupportedExtensionNames.Count > 0 then
        ExtensionFound := (FServerProperties.SupportedExtension.SupportedExtensionNames.IndexOf('check-file-name') >= 0) or
                          (FServerProperties.SupportedExtension.SupportedExtensionNames.IndexOf('check-file') >= 0);

    if not ExtensionFound then
      raise EScError.Create(seUnsupportedExtensionName);

    RequestInfo := InitRequestByName(_opCheckingFile, FileName);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('check-file-name');
      FPacketWriter.WriteAsString(EncodeString(FileName));
      FPacketWriter.WriteAStr(ScSFTPHashAlgorithmName[HashAlgorithm]);
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteInt32(BlockSize);
    finally
      TransmitAndProcess;
    end;
  end;
end;

procedure TScSFTPClient.CheckFileByHandleEx(const Handle: TScSFTPFileHandle;
  HashAlgorithm: TScSFTPHashAlgorithm; StartOffset, Length: Int64;
  BlockSize: integer; ReplyExtension: TScCheckFileReplyExtension = nil);
var
  RequestInfo: TScSFTPRequestInfo;
  ExtensionFound: boolean;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerVersion = 5 then
  begin
    if HashAlgorithm <> sftphaMD5 then
      raise EScError.Create(seUnsupportedSFTPOperation);

    if FServerProperties.SupportedExtensionAvailable then
      TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'md5-hash-handle');

    RequestInfo := InitRequestByHandle(_opCheckingFile, Handle);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('md5-hash-handle');
      FPacketWriter.WriteAsString(Handle);
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteAStr('');
    finally
      TransmitAndProcess;
    end;
  end
  else begin
    ExtensionFound := False;

    if FServerProperties.SupportedExtensionAvailable then
      if FServerProperties.SupportedExtension.SupportedExtensionNames.Count > 0 then
        ExtensionFound := (FServerProperties.SupportedExtension.SupportedExtensionNames.IndexOf('check-file-handle') >= 0) or
                          (FServerProperties.SupportedExtension.SupportedExtensionNames.IndexOf('check-file') >= 0);

    if not ExtensionFound then
      raise EScError.Create(seUnsupportedExtensionName);

    RequestInfo := InitRequestByHandle(_opCheckingFile, Handle);
    try
      RequestInfo.ReplyExtension := ReplyExtension;

      FPacketWriter.WriteAStr('check-file-handle');
      FPacketWriter.WriteAsString(Handle);
      FPacketWriter.WriteAStr(ScSFTPHashAlgorithmName[HashAlgorithm]);
      FPacketWriter.WriteInt64(StartOffset);
      FPacketWriter.WriteInt64(Length);
      FPacketWriter.WriteInt32(BlockSize);
    finally
      TransmitAndProcess;
    end;
  end;
end;

procedure TScSFTPClient.QueryAvailableSpace(const Path: string; ReplyExtension: TScSpaceAvailableReplyExtension = nil);
var
  RequestInfo: TScSFTPRequestInfo;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerProperties.SupportedExtensionAvailable then
    TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'space-available');

  RequestInfo := InitRequestByName(_opQueryingAvailableSpace, Path);
  try
    RequestInfo.ReplyExtension := ReplyExtension;

    FPacketWriter.WriteAStr('space-available');
    FPacketWriter.WriteAsString(EncodeString(Path));
  finally
    TransmitAndProcess;
  end;
end;

function TScSFTPClient.QueryUserHomeDirectory(const Username: string): string;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerProperties.SupportedExtensionAvailable then
    TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, 'home-directory');

  InitRequestByName(_opQueryingUserDirectory, Username);
  try
    FPacketWriter.WriteAStr('home-directory');
    FPacketWriter.WriteAsString(EncodeString(Username));
  finally
    TransmitAndProcess;
  end;

  if not NonBlocking then
    Result := FReceivedHomeDirectory
  else
    Result := '';
end;

procedure TScSFTPClient.RequestExtension(const ExtName: string; const ExtData: TBytes;
  ReplyExtension: TScSFTPExtension = nil);
var
  RequestInfo: TScSFTPRequestInfo;
begin
  if FServerVersion < 3 then
    raise EScError.Create(seUnsupportedSFTPOperation);

  if FServerProperties.SupportedExtensionAvailable then
    TScSFTPSupportedExtensionUtils.CheckExtensionName(FServerProperties.SupportedExtension, ExtName);

  RequestInfo := InitRequestByName(_opRequestExtension, ExtName);
  try
    RequestInfo.ReplyExtension := ReplyExtension;

    FPacketWriter.WriteAStr(ExtName);
    if Length(ExtData) > 0 then
      FPacketWriter.WriteBuf(ExtData);
  finally
    TransmitAndProcess;
  end;
end;

procedure TScSFTPClient.RequestExtension(Extension: TScSFTPExtension;
  ReplyExtension: TScSFTPExtension = nil);
begin
  if Extension = nil then
    raise EScError.Create(seInvalidInputArgs);

  RequestExtension(Extension.Name, Extension.Data, ReplyExtension);
end;

procedure TScSFTPClient.CopyRemoteFile(const Source, Destination: string; Overwrite: boolean);
begin
  InitRequestByName(_opCopyingRemoteFile, Source);
  try
    FPacketWriter.WriteAStr('copy-file');
    FPacketWriter.WriteAsString(EncodeString(Source));
    FPacketWriter.WriteAsString(EncodeString(Destination));
    FPacketWriter.WriteBool(Overwrite);
  finally
    TransmitAndProcess;
  end;
end;

function TScSFTPClient.GetServerVersion: TScSFTPVersion;
begin
  Result := TScSFTPVersion(FServerVersion);
end;

function TScSFTPClient.GetSSHClient: TScSSHClient;
begin
  Result := FSSHSubSystem.Client;
end;

procedure TScSFTPClient.CheckInactive;
begin
  if Active then
    raise EScError.Create(seClientOpened);
end;

procedure TScSFTPClient.SetSSHClient(Value: TScSSHClient);
begin
  if Value <> FSSHSubSystem.Client then begin
    CheckInactive;
    FSSHSubSystem.Client := Value;
  end;
end;

function TScSFTPClient.GetTimeout: integer;
begin
  Result := FSSHSubSystem.Timeout;
end;

procedure TScSFTPClient.SetTimeout(Value: integer);
begin
  FSSHSubSystem.Timeout := Value;
end;

procedure TScSFTPClient.SetNonBlocking(Value: boolean);
begin
  if Value <> FNonBlocking then begin
    CheckInactive;
    FNonBlocking := Value;
  end;
end;

procedure TScSFTPClient.SetEventsCallMode(Value: TScEventCallMode);
begin
  if Value <> FEventsCallMode then begin
    CheckInactive;
    FEventsCallMode := Value;
  end;
end;

procedure TScSFTPClient.SetVersion(Value: TScSFTPVersion);
begin
  if Value <> FVersion then begin
    CheckInactive;
    FVersion := Value;
  end;
end;

end.

