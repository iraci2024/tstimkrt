
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSFTPServer;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, ScTypes, ScBridge, ScSSHUtils, ScSSHChannel,
  ScSFTPPacket, ScSFTPUtils, ScSFTPConsts;

type
  TScSFTPServerGetFullPathEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo; var Path: string) of object;

  TScSFTPServerOpenEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo) of object;
  TScSFTPServerCloseEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo) of object;

  TScSFTPServerOpenFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const FileName: string; const OpenAttributes: TScSFTPFileOpenAttributes;
    out Data: TObject; var Error: TScSFTPError) of object;

  TScSFTPServerCloseFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; var Error: TScSFTPError) of object;

  TScSFTPServerReadFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; Offset: Int64; Count: cardinal;
    var Buffer: TBytes; var Read: cardinal; var Error: TScSFTPError) of object;

  TScSFTPServerWriteFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; Offset: Int64; const Buffer: TBytes; Count: integer;
    var Error: TScSFTPError) of object;

  TScSFTPServerRemoveFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const FileName: string; var Error: TScSFTPError) of object;

  TScSFTPServerRenameFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const OldName, NewName: string; const Flags: TScSFTPRenameFlags;
    var Error: TScSFTPError) of object;

  TScSFTPServerMakeDirectoryEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; var Error: TScSFTPError) of object;

  TScSFTPServerRemoveDirectoryEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; var Error: TScSFTPError) of object;

  TScSFTPServerOpenDirectoryEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; out Data: TObject; var Error: TScSFTPError) of object;

  TScSFTPServerReadDirectoryEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; FileInfo: TScSFTPFileInfo; var Error: TScSFTPError) of object;

  TScSFTPServerRetrieveAttributesEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; FollowSymLink: boolean; const ReqAttrs: TScSFTPAttributes;
    Attributes: TScSFTPFileAttributes; var Error: TScSFTPError) of object;

  TScSFTPServerRetrieveAttributesByHandleEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; const ReqAttrs: TScSFTPAttributes;
    Attributes: TScSFTPFileAttributes; var Error: TScSFTPError) of object;

  TScSFTPServerSetAttributesEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; Attributes: TScSFTPFileAttributes;
    var Error: TScSFTPError) of object;

  TScSFTPServerSetAttributesByHandleEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; Attributes: TScSFTPFileAttributes;
    var Error: TScSFTPError) of object;

  TScSFTPServerReadSymbolicLinkEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; out SymbolicName: string;
    var Error: TScSFTPError) of object;

  TScSFTPServerCreateLinkEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const LinkPath, TargetPath: string; Symbolic: boolean;
    var Error: TScSFTPError) of object;

  TScSFTPServerGetAbsolutePathEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    const Path: string; const Control: TScSFTPRealpathControl; ComposePathList: TStringList;
    out AbsolutePath: string; out FileType: TScSFTPFileType; var Error: TScSFTPError) of object;

  TScSFTPServerBlockFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; const Offset, Len: Int64; const BlockModes: TScSFTPBlockModes;
    var Error: TScSFTPError) of object;

  TScSFTPServerUnBlockFileEvent = procedure(
    Sender: TObject; SFTPSessionInfo: TScSFTPSessionInfo;
    Data: TObject; const Offset, Len: Int64; var Error: TScSFTPError) of object;

  TScOnRequestFileSecurityAttributes = procedure (Sender: TObject;
    Attributes: TScSFTPFileAttributes; const Path: string; SecurityDescriptor: Pointer) of object;

  TScHandle = class
  protected
    FHandle: THandle;
    FFullFileName: string;
    FInAppendMode: boolean;
    FInTextMode: boolean;
    FRWStarted: boolean;
  public
    constructor Create(Handle: THandle);
    property Handle: THandle read FHandle write FHandle;
    property FullFileName: string read FFullFileName;
  end;

  TScSearchRec = class
  protected
    FSearchRec: TSearchRec;
    FPath: string;
    FIsBof: boolean;
  public
    constructor Create(SearchRec: TSearchRec);
    property SearchRec: TSearchRec read FSearchRec write FSearchRec;
  end;

  TScSFTPHandleInfo = record
    Id: Int64;
    Data: TObject;
    IsDirectory: boolean;
  end;
  TScSFTPHandleInfoArray = array of TScSFTPHandleInfo;

  TScSFTPHandleInfoList = class
  private
    FItems: TScSFTPHandleInfoArray;
    FCount: integer;
    FLastHandleNo: Int64;
    procedure Grow;
    function GetItem(Index: integer): TScSFTPHandleInfo;
    procedure SetItem(Index: integer; const HandleInfo: TScSFTPHandleInfo);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Data: TObject; IsDirectory: boolean): TScSFTPFileHandle;
    procedure Delete(Index: integer);
    function IndexOf(const SFTPHandle: TScSFTPFileHandle): integer;
    property Count: integer read FCount;
    property Items[Index: integer]: TScSFTPHandleInfo read GetItem write SetItem; default;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSFTPServer = class(TComponent)
  private
    FUseUnicode: boolean;
    FDefaultRootPath: string;

    FOnGetFullPath: TScSFTPServerGetFullPathEvent;
    FOnOpen: TScSFTPServerOpenEvent;
    FOnClose: TScSFTPServerCloseEvent;
    FOnOpenFile: TScSFTPServerOpenFileEvent;
    FOnCloseFile: TScSFTPServerCloseFileEvent;
    FOnReadFile: TScSFTPServerReadFileEvent;
    FOnWriteFile: TScSFTPServerWriteFileEvent;
    FOnRemoveFile: TScSFTPServerRemoveFileEvent;
    FOnRenameFile: TScSFTPServerRenameFileEvent;
    FOnMakeDirectory: TScSFTPServerMakeDirectoryEvent;
    FOnRemoveDirectory: TScSFTPServerRemoveDirectoryEvent;
    FOnOpenDirectory: TScSFTPServerOpenDirectoryEvent;
    FOnReadDirectory: TScSFTPServerReadDirectoryEvent;
    FOnRetrieveAttributes: TScSFTPServerRetrieveAttributesEvent;
    FOnRetrieveAttributesByHandle: TScSFTPServerRetrieveAttributesByHandleEvent;
    FOnSetAttributes: TScSFTPServerSetAttributesEvent;
    FOnSetAttributesByHandle: TScSFTPServerSetAttributesByHandleEvent;
    FOnReadSymbolicLink: TScSFTPServerReadSymbolicLinkEvent;
    FOnCreateLink: TScSFTPServerCreateLinkEvent;
    FOnGetAbsolutePath: TScSFTPServerGetAbsolutePathEvent;
    FOnBlockFile: TScSFTPServerBlockFileEvent;
    FOnUnBlockFile: TScSFTPServerUnBlockFileEvent;
    FOnRequestFileSecurityAttributes: TScOnRequestFileSecurityAttributes;

    procedure DoRequestFileSecurityAttributes(Attributes: TScSFTPFileAttributes; const Path: string; SecurityDescriptor: Pointer);
  protected
    function ExtractPathDrive(const Path: string): string;

  public
    constructor Create(AOwner: TComponent); override;

    function GetCanonicalPath(const Path: string): string; virtual;
    function GetFullPath(SFTPSessionInfo: TScSFTPSessionInfo; const Path: string): string; virtual;

    procedure DefaultGetFullPath(SFTPSessionInfo: TScSFTPSessionInfo; var Path: string);
    procedure DefaultOpenFile(SFTPSessionInfo: TScSFTPSessionInfo;
      const FileName: string; const OpenAttributes: TScSFTPFileOpenAttributes;
      out Data: TObject; var Error: TScSFTPError); virtual;
    procedure DefaultCloseFile(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; var Error: TScSFTPError); virtual;
    procedure DefaultReadFile(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; Offset: Int64; Count: cardinal;
      var Buffer: TBytes; var Read: cardinal; var Error: TScSFTPError); virtual;
    procedure DefaultWriteFile(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; Offset: Int64; const Buffer: TBytes; Count: integer;
      var Error: TScSFTPError); virtual;
    procedure DefaultRemoveFile(SFTPSessionInfo: TScSFTPSessionInfo;
      const FileName: string; var Error: TScSFTPError); virtual;
    procedure DefaultRenameFile(SFTPSessionInfo: TScSFTPSessionInfo;
      const OldName, NewName: string; const Flags: TScSFTPRenameFlags;
      var Error: TScSFTPError); virtual;
    procedure DefaultMakeDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; var Error: TScSFTPError); virtual;
    procedure DefaultRemoveDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; var Error: TScSFTPError); virtual;
    procedure DefaultOpenDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; out Data: TObject; var Error: TScSFTPError); virtual;
    procedure DefaultReadDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; FileInfo: TScSFTPFileInfo; var Error: TScSFTPError); virtual;
    procedure DefaultRetrieveAttributes(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; FollowSymLink: boolean; const ReqAttrs: TScSFTPAttributes;
      Attributes: TScSFTPFileAttributes; var Error: TScSFTPError); virtual;
    procedure DefaultRetrieveAttributesByHandle(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; const ReqAttrs: TScSFTPAttributes;
      Attributes: TScSFTPFileAttributes; var Error: TScSFTPError); virtual;
    procedure DefaultSetAttributes(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; Attributes: TScSFTPFileAttributes; var Error: TScSFTPError); virtual;
    procedure DefaultSetAttributesByHandle(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; Attributes: TScSFTPFileAttributes; var Error: TScSFTPError); virtual;
    procedure DefaultReadSymbolicLink(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; out SymbolicName: string; var Error: TScSFTPError); virtual;
    procedure DefaultCreateLink(SFTPSessionInfo: TScSFTPSessionInfo;
      const LinkPath, TargetPath: string; Symbolic: boolean;
      var Error: TScSFTPError); virtual;
    procedure DefaultGetAbsolutePath(SFTPSessionInfo: TScSFTPSessionInfo;
      const Path: string; const Control: TScSFTPRealpathControl; ComposePathList: TStringList;
      out AbsolutePath: string; out FileType: TScSFTPFileType; var Error: TScSFTPError); virtual;
    procedure DefaultBlockFile(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; const Offset, Len: Int64;
      const BlockModes: TScSFTPBlockModes; var Error: TScSFTPError); virtual;
    procedure DefaultUnBlockFile(SFTPSessionInfo: TScSFTPSessionInfo;
      Data: TObject; const Offset, Len: Int64; var Error: TScSFTPError); virtual;

  published
    property DefaultRootPath: string read FDefaultRootPath write FDefaultRootPath;
    property UseUnicode: boolean read FUseUnicode write FUseUnicode default True;

    property OnGetFullPath: TScSFTPServerGetFullPathEvent read FOnGetFullPath write FOnGetFullPath;
    property OnOpen: TScSFTPServerOpenEvent read FOnOpen write FOnOpen;
    property OnClose: TScSFTPServerCloseEvent read FOnClose write FOnClose;
    property OnOpenFile: TScSFTPServerOpenFileEvent read FOnOpenFile write FOnOpenFile;
    property OnCloseFile: TScSFTPServerCloseFileEvent read FOnCloseFile write FOnCloseFile;
    property OnReadFile: TScSFTPServerReadFileEvent read FOnReadFile write FOnReadFile;
    property OnWriteFile: TScSFTPServerWriteFileEvent read FOnWriteFile write FOnWriteFile;
    property OnRemoveFile: TScSFTPServerRemoveFileEvent read FOnRemoveFile write FOnRemoveFile;
    property OnRenameFile: TScSFTPServerRenameFileEvent read FOnRenameFile write FOnRenameFile;
    property OnMakeDirectory: TScSFTPServerMakeDirectoryEvent read FOnMakeDirectory write FOnMakeDirectory;
    property OnRemoveDirectory: TScSFTPServerRemoveDirectoryEvent read FOnRemoveDirectory write FOnRemoveDirectory;
    property OnOpenDirectory: TScSFTPServerOpenDirectoryEvent read FOnOpenDirectory write FOnOpenDirectory;
    property OnReadDirectory: TScSFTPServerReadDirectoryEvent read FOnReadDirectory write FOnReadDirectory;
    property OnRetrieveAttributes: TScSFTPServerRetrieveAttributesEvent read FOnRetrieveAttributes write FOnRetrieveAttributes;
    property OnRetrieveAttributesByHandle: TScSFTPServerRetrieveAttributesByHandleEvent read FOnRetrieveAttributesByHandle write FOnRetrieveAttributesByHandle;
    property OnSetAttributes: TScSFTPServerSetAttributesEvent read FOnSetAttributes write FOnSetAttributes;
    property OnSetAttributesByHandle: TScSFTPServerSetAttributesByHandleEvent read FOnSetAttributesByHandle write FOnSetAttributesByHandle;
    property OnReadSymbolicLink: TScSFTPServerReadSymbolicLinkEvent read FOnReadSymbolicLink write FOnReadSymbolicLink;
    property OnCreateLink: TScSFTPServerCreateLinkEvent read FOnCreateLink write FOnCreateLink;
    property OnGetAbsolutePath: TScSFTPServerGetAbsolutePathEvent read FOnGetAbsolutePath write FOnGetAbsolutePath;
    property OnBlockFile: TScSFTPServerBlockFileEvent read FOnBlockFile write FOnBlockFile;
    property OnUnBlockFile: TScSFTPServerUnBlockFileEvent read FOnUnBlockFile write FOnUnBlockFile;
    property OnRequestFileSecurityAttributes: TScOnRequestFileSecurityAttributes read FOnRequestFileSecurityAttributes write FOnRequestFileSecurityAttributes;
  end;

  TScSFTPServerHandler = class(TScSSHSubSystemProcessor)
  private
    FInitialized: boolean;
    FPacketReader: TSFTPPacketReader;
    FPacketWriter: TSFTPPacketWriter;
    FHandleInfoList: TScSFTPHandleInfoList;
    FRWBuf: TBytes;
    FSFTPServer: TScSFTPServer;
    FSessionInfo: TScSFTPSessionInfo;

  {$IFDEF MSWINDOWS}
    FCoInitializeThreadID: cardinal;
  {$ENDIF}
    FNeedCallOnClose: boolean;
    FInitializationErrorMessage: string;

    procedure TransmitResponse;
    procedure RespondWithStatus(const RequestId: integer; const Error: TScSFTPError);
    function DecodeBytes(const Data: TBytes): string;
    function EncodeString(const Str: string): TBytes;
    procedure FindFileByHandle(const SFTPHandle: TScSFTPFileHandle;
      out Data: TObject; out Error: TScSFTPError);
    procedure FindDirByHandle(const SFTPHandle: TScSFTPFileHandle;
      out Data: TObject; out Error: TScSFTPError);

    procedure DoCloseFile(Data: TObject; out Error: TScSFTPError);
    procedure DoSetAttributesByHandle(Data: TObject;
      Attributes: TScSFTPFileAttributes; out Error: TScSFTPError);
    procedure DoSetAttributes(const Path: string;
      Attributes: TScSFTPFileAttributes; out Error: TScSFTPError);

    procedure ProcessOpenFileRequest;
    procedure ProcessCloseFileRequest;
    procedure ProcessReadFileRequest;
    procedure ProcessWriteFileRequest;
    procedure ProcessRemoveFileRequest;
    procedure ProcessRenameFileRequest;
    procedure ProcessMakeDirRequest;
    procedure ProcessRemoveDirRequest;
    procedure ProcessOpenDirRequest;
    procedure ProcessReadDirRequest;
    procedure ProcessRetrieveFileAttributesRequest(FollowSymLink: boolean);
    procedure ProcessRetrieveHandleAttributesRequest;
    procedure ProcessSetFileAttributesRequest;
    procedure ProcessSetHandleAttributesRequest;
    procedure ProcessReadLinkRequest;
    procedure ProcessCreateLinkRequest;
    procedure ProcessRealPathRequest;
    procedure ProcessBlockRequest;
    procedure ProcessUnBlockRequest;
    procedure ProcessExtensionRequest;

  public
    constructor Create(Channel: TScSSHChannel; SFTPServer: TScSFTPServer; ConnectionInfo: TScSSHClientInfo);
    destructor Destroy; override;

    procedure ProcessRequest; override;
    procedure Init; override;
    procedure Close; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  ShlObj, ActiveX, ComObj,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Fcntl, Posix.Unistd, Posix.Stdio,
{$ENDIF}
{$IFDEF UNIX}
  BaseUnix, UnixType,
{$ENDIF}
{$IFDEF VER16P}
  System.Types,
{$ENDIF}
  Math, ScCLRClasses, ScFunctions, ScConsts, ScUtils;

{$IFDEF MSWINDOWS}
type
  IShellLink = {$IFDEF IS_UNICODE}IShellLinkW{$ELSE}IShellLinkA{$ENDIF};
{$ENDIF}

const
  READ_METADATA_SIZE = 13;
  MAX_SFTP_READ_FILE_BLOCK_SIZE = 64 * 1024 - READ_METADATA_SIZE;
  MAX_SFTP_READ_DIR_BLOCK_SIZE = 64 * 1024 - (READ_METADATA_SIZE + 1);

{ TScHandle }

constructor TScHandle.Create(Handle: THandle);
begin
  inherited Create;
  FHandle := Handle;
end;

{ TScSearchRec }

constructor TScSearchRec.Create(SearchRec: TSearchRec);
begin
  inherited Create;
  FSearchRec := SearchRec;
  FIsBof := True;
end;

{ TScSFTPHandleInfoList }

constructor TScSFTPHandleInfoList.Create;
begin
  inherited;
  FLastHandleNo := 0;
end;

destructor TScSFTPHandleInfoList.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

procedure TScSFTPHandleInfoList.Grow;
var
  NewLength: integer;
begin
  NewLength := Length(FItems) * 2;
  if NewLength = 0 then
    NewLength := 16;
  SetLength(FItems, NewLength);
end;

function TScSFTPHandleInfoList.Add(Data: TObject; IsDirectory: boolean): TScSFTPFileHandle;
var
  i: integer;
begin
  if Data <> nil then
    for i := 0 to FCount - 1 do
      if FItems[i].Data = Data then begin
        Result := BitConverter.GetBytes(FItems[i].Id);
        Exit;
      end;

  if FCount >= Length(FItems) then
    Grow;

  Inc(FLastHandleNo);
  FItems[FCount].Id := FLastHandleNo;
  FItems[FCount].Data := Data;
  FItems[FCount].IsDirectory := IsDirectory;
  Result := BitConverter.GetBytes(FLastHandleNo);
  Inc(FCount);
end;

procedure TScSFTPHandleInfoList.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EScError.CreateFmt(SListIndexError, [Index], seInternalError);

  if (FItems[Index].Data is TScHandle) or (FItems[Index].Data is TScSearchRec) then
    FItems[Index].Data.Free;

  if Index <> FCount - 1 then
    FItems[Index] := FItems[FCount - 1];
  Dec(FCount);
end;

function TScSFTPHandleInfoList.IndexOf(const SFTPHandle: TScSFTPFileHandle): integer;
var
  i: integer;
  Id: Int64;
begin
  Result := -1;
  if Length(SFTPHandle) = sizeof(Int64) then begin
    Id := BitConverter.ToInt64(SFTPHandle, 0);
    for i := 0 to FCount - 1 do
      if FItems[i].Id = Id then begin
        Result := i;
        break;
      end;
  end;
end;

function TScSFTPHandleInfoList.GetItem(Index: integer): TScSFTPHandleInfo;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EScError.CreateFmt(SListIndexError, [Index], seInternalError);

  Result := FItems[Index];
end;

procedure TScSFTPHandleInfoList.SetItem(Index: integer; const HandleInfo: TScSFTPHandleInfo);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EScError.CreateFmt(SListIndexError, [Index], seInternalError);

  FItems[Index] := HandleInfo;
end;

{ TScSFTPServerHandler }

constructor TScSFTPServerHandler.Create(Channel: TScSSHChannel; SFTPServer: TScSFTPServer; ConnectionInfo: TScSSHClientInfo);
begin
  inherited Create(Channel);

{$IFDEF MSWINDOWS}
  FCoInitializeThreadID := 0;
{$ENDIF}
  FNeedCallOnClose := False;
  FInitializationErrorMessage := '';

  FSFTPServer := SFTPServer;
  Assert(FSFTPServer <> nil);
  SetLength(FRWBuf, 32);
  FHandleInfoList := TScSFTPHandleInfoList.Create;
  FPacketReader := TSFTPPacketReader.Create(nil);
  FPacketWriter := TSFTPPacketWriter.Create;

  FSessionInfo := TScSFTPSessionInfo.Create(ConnectionInfo);
  if FSessionInfo.HomePath = '' then
    if SFTPServer.DefaultRootPath <> '' then
      FSessionInfo.HomePath := SFTPServer.DefaultRootPath
    else
      FSessionInfo.HomePath := GetCurrentDir;

  if FSessionInfo.HomePath <> PathDelim then
    FSessionInfo.HomePath := ExcludeTrailingBackslash(FSessionInfo.HomePath);

  FSessionInfo.UseUnicode := SFTPServer.UseUnicode;
  FSessionInfo.EOL := {$IFDEF MSWINDOWS}#$0D#$0A{$ELSE}#$0A{$ENDIF};
end;

destructor TScSFTPServerHandler.Destroy;
begin
  Close;

  FPacketWriter.Free;
  FPacketReader.Free;
  FHandleInfoList.Free;
  FSessionInfo.Free;

  inherited;
end;

function TScSFTPServerHandler.DecodeBytes(const Data: TBytes): string;
begin
  Result := TScSFTPUtils.DecodeBytes(Data, FSessionInfo.UseUnicode);
end;

function TScSFTPServerHandler.EncodeString(const Str: string): TBytes;
begin
  Result := TScSFTPUtils.EncodeString(Str, FSessionInfo.UseUnicode);
end;

procedure TScSFTPServerHandler.TransmitResponse;
var
  Written: integer;
begin
  Written := FChannel.WriteBuffer(FPacketWriter.PacketData, 0, FPacketWriter.PacketLength);
  if Written < FPacketWriter.PacketLength then
    raise EScError.Create(seCannotSendClientData);
end;

procedure TScSFTPServerHandler.RespondWithStatus(const RequestId: integer;
  const Error: TScSFTPError);
begin
  FPacketWriter.InitPacket(SSH_FXP_STATUS);
  FPacketWriter.WriteInt32(RequestId);
  FPacketWriter.WriteInt32(SFTPErrorCodes[Error.ErrorCode]);

  if FSessionInfo.Version >= 3 then begin
    //D process errors - e.g. file_doesnot_exists - for open, remove, rename, etc.
    if Error.ErrorMessage = '' then
      FPacketWriter.WriteAsString(EncodeString(SFTPErrorMessages[Error.ErrorCode]))
    else
      FPacketWriter.WriteAsString(EncodeString(Error.ErrorMessage));
    FPacketWriter.WriteAsString(EncodeString('')); // language tag
  end;

  TransmitResponse;
end;

procedure TScSFTPServerHandler.FindFileByHandle(const SFTPHandle: TScSFTPFileHandle;
  out Data: TObject; out Error: TScSFTPError);
var
  Index: integer;
begin
  Index := FHandleInfoList.IndexOf(SFTPHandle);

  Data := nil;
  if Index = -1 then
    InitError(Error, erNoSuchFile)
  else
    if FHandleInfoList[Index].IsDirectory then
      InitError(Error, erFailure, 'File is a directory')
    else begin
      InitError(Error, erOk);
      Data := FHandleInfoList[Index].Data;
    end;
end;

procedure TScSFTPServerHandler.FindDirByHandle(const SFTPHandle: TScSFTPFileHandle;
  out Data: TObject; out Error: TScSFTPError);
var
  Index: integer;
begin
  Index := FHandleInfoList.IndexOf(SFTPHandle);

  Data := nil;
  if Index = -1 then
    InitError(Error, erNoSuchFile)
  else
    if not FHandleInfoList[Index].IsDirectory then
      InitError(Error, erFailure, 'Not a directory')
    else begin
      InitError(Error, erOk);
      Data := FHandleInfoList[Index].Data;
    end;
end;

procedure TScSFTPServerHandler.ProcessOpenFileRequest;
var
  RequestId: integer;
  FileName: string;
  Modes: TScSFTPFileOpenModes;
  dwFlags: integer;
  OpenAttributes: TScSFTPFileOpenAttributes;
  Data: TObject;
  Error, Error2: TScSFTPError;
  SFTPHandle: TScSFTPFileHandle;
begin
  SetLength(SFTPHandle, 0);
  RequestId := FPacketReader.ReadInt32;
  FileName := DecodeBytes(FPacketReader.ReadString);
  if FSessionInfo.Version <= 4 then begin
    Modes := ConvertSFTPValueToFileOpenModes(FPacketReader.ReadInt32);
    ConvertOpenModesVer3ToVer5(Modes, OpenAttributes.Mode, OpenAttributes.Flags,
      OpenAttributes.BlockModes, OpenAttributes.DesiredAccess);
  end
  else begin
    OpenAttributes.DesiredAccess := ConvertSFTPValueToAceMask(FPacketReader.ReadInt32);
    dwFlags := FPacketReader.ReadInt32;
    OpenAttributes.Mode := ConvertSFTPValueToFileOpenMode(dwFlags);
    OpenAttributes.Flags := ConvertSFTPValueToFileOpenFlags(dwFlags);
    OpenAttributes.BlockModes := ConvertSFTPValueToBlockModes(dwFlags);
  end;

  OpenAttributes.Attributes := TScSFTPFileAttributes.Create;
  try
    TScSFTPFileAttributesUtils.ReadFromPacket(OpenAttributes.Attributes, FPacketReader, FSessionInfo.Version);

    Data := nil;
    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnOpenFile) then
        FSFTPServer.OnOpenFile(FSFTPServer, FSessionInfo, FileName, OpenAttributes, Data, Error)
      else
        FSFTPServer.DefaultOpenFile(FSessionInfo, FileName, OpenAttributes, Data, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;

    if Error.ErrorCode = erOk then begin
      DoSetAttributesByHandle(Data, OpenAttributes.Attributes, Error);

      if Error.ErrorCode = erOk then
        SFTPHandle := FHandleInfoList.Add(Data, False)
      else begin
        DoCloseFile(Data, Error2);
        if (Data is TScHandle) or (Data is TScSearchRec) then
          Data.Free;
      end;
    end;

    if Error.ErrorCode = erOk then begin
      FPacketWriter.InitPacket(SSH_FXP_HANDLE);
      FPacketWriter.WriteInt32(RequestId);
      FPacketWriter.WriteAsString(SFTPHandle);
      TransmitResponse;
    end
    else
      RespondWithStatus(RequestId, Error);
  finally
    OpenAttributes.Attributes.Free;
  end;
end;

procedure TScSFTPServerHandler.DoCloseFile(Data: TObject; out Error: TScSFTPError);
begin
  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnCloseFile) then
      FSFTPServer.OnCloseFile(FSFTPServer, FSessionInfo, Data, Error)
    else
      FSFTPServer.DefaultCloseFile(FSessionInfo, Data, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;
end;

procedure TScSFTPServerHandler.ProcessCloseFileRequest;
var
  RequestId: integer;
  SFTPHandle: TScSFTPFileHandle;
  Index: integer;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  SFTPHandle := FPacketReader.ReadString;

  Index := FHandleInfoList.IndexOf(SFTPHandle);
  if Index = -1 then begin
    InitError(Error, erNoSuchFile);
  end
  else begin
    DoCloseFile(FHandleInfoList[Index].Data, Error);
    FHandleInfoList.Delete(Index);
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessReadFileRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  Offset: Int64;
  Len: cardinal;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;
  Offset := FPacketReader.ReadInt64;
  Len := Min(cardinal(FPacketReader.ReadInt32), MAX_SFTP_READ_FILE_BLOCK_SIZE);
  if cardinal(Length(FRWBuf)) < Len then
    SetLength(FRWBuf, Len);

  FindFileByHandle(Handle, Data, Error);

  if Error.ErrorCode = erOk then begin
    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnReadFile) then
        FSFTPServer.OnReadFile(FSFTPServer, FSessionInfo, Data, Offset, Len, FRWBuf, Len, Error)
      else
        FSFTPServer.DefaultReadFile(FSessionInfo, Data, Offset, Len, FRWBuf, Len, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;
  end;

  if Error.ErrorCode = erOk then begin
    FPacketWriter.InitPacket(SSH_FXP_DATA);
    FPacketWriter.WriteInt32(RequestId);
    FPacketWriter.WriteAsString(TValueArr(FRWBuf), 0, Len);
    TransmitResponse;
  end
  else
    RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessWriteFileRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  Offset: Int64;
  Len: integer;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;
  Offset := FPacketReader.ReadInt64;
  Len := FPacketReader.Rest - 4 { Len };
  if Length(FRWBuf) < Len then
    SetLength(FRWBuf, Len);
  Len := FPacketReader.ReadString(FRWBuf, 0, Length(FRWBuf));

  FindFileByHandle(Handle, Data, Error);

  if Error.ErrorCode = erOk then begin
    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnWriteFile) then
        FSFTPServer.OnWriteFile(FSFTPServer, FSessionInfo, Data, Offset, FRWBuf, Len, Error)
      else
        FSFTPServer.DefaultWriteFile(FSessionInfo, Data, Offset, FRWBuf, Len, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessRemoveFileRequest;
var
  RequestId: integer;
  FileName: string;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  FileName := DecodeBytes(FPacketReader.ReadString);

  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnRemoveFile) then
      FSFTPServer.OnRemoveFile(FSFTPServer, FSessionInfo, FileName, Error)
    else
      FSFTPServer.DefaultRemoveFile(FSessionInfo, FileName, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessRenameFileRequest;
var
  RequestId: integer;
  OldPath, NewPath: string;
  Flags: TScSFTPRenameFlags;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  OldPath := DecodeBytes(FPacketReader.ReadString);
  NewPath := DecodeBytes(FPacketReader.ReadString);
  if FSessionInfo.Version >= 5 then
    Flags := ConvertSFTPValueToRenameFlags(FPacketReader.ReadInt32)
  else
    Flags := [];

  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnRenameFile) then
      FSFTPServer.OnRenameFile(FSFTPServer, FSessionInfo, OldPath, NewPath, Flags, Error)
    else
      FSFTPServer.DefaultRenameFile(FSessionInfo, OldPath, NewPath, Flags, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessMakeDirRequest;
var
  RequestId: integer;
  Path: string;
  Attributes: TScSFTPFileAttributes;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);

  Attributes := TScSFTPFileAttributes.Create;
  try
    TScSFTPFileAttributesUtils.ReadFromPacket(Attributes, FPacketReader, FSessionInfo.Version);

    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnMakeDirectory) then
        FSFTPServer.OnMakeDirectory(FSFTPServer, FSessionInfo, Path, Error)
      else
        FSFTPServer.DefaultMakeDirectory(FSessionInfo, Path, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;

    if Error.ErrorCode = erOk then
      DoSetAttributes(Path, Attributes, Error);

    RespondWithStatus(RequestId, Error);
  finally
    Attributes.Free;
  end;
end;

procedure TScSFTPServerHandler.ProcessRemoveDirRequest;
var
  RequestId: integer;
  Path: string;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);

  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnRemoveDirectory) then
      FSFTPServer.OnRemoveDirectory(FSFTPServer, FSessionInfo, Path, Error)
    else
      FSFTPServer.DefaultRemoveDirectory(FSessionInfo, Path, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessOpenDirRequest;
var
  RequestId: integer;
  Path: string;
  Data: TObject;
  Error: TScSFTPError;
  SFTPHandle: TScSFTPFileHandle;
begin
  SetLength(SFTPHandle, 0);
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);

  Data := nil;
  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnOpenDirectory) then
      FSFTPServer.OnOpenDirectory(FSFTPServer, FSessionInfo, Path, Data, Error)
    else
      FSFTPServer.DefaultOpenDirectory(FSessionInfo, Path, Data, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;

  if Error.ErrorCode = erOk then begin
    SFTPHandle := FHandleInfoList.Add(Data, True);

    FPacketWriter.InitPacket(SSH_FXP_HANDLE);
    FPacketWriter.WriteInt32(RequestId);
    FPacketWriter.WriteAsString(SFTPHandle);
    TransmitResponse;
  end
  else
    RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessReadDirRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  FileInfo: TScSFTPFileInfo;
  FileInfoPacketWriter: TSFTPPacketWriter;
  Count: integer;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;

  FileInfoPacketWriter := TSFTPPacketWriter.Create(1024);
  FileInfo := TScSFTPFileInfo.Create;
  try
    Count := 0;
    FindDirByHandle(Handle, Data, Error);

    if Error.ErrorCode = erOk then begin
      repeat
        InitError(Error, erFailure);
        try
          if Assigned(FSFTPServer.OnRequestFileSecurityAttributes) then
            TScSFTPFileAttributesUtils.SetRequestFileSecurityAttributes(FileInfo.Attributes, FSFTPServer.DoRequestFileSecurityAttributes);

          if Assigned(FSFTPServer.OnReadDirectory) then
            FSFTPServer.OnReadDirectory(FSFTPServer, FSessionInfo, Data, FileInfo, Error)
          else
            FSFTPServer.DefaultReadDirectory(FSessionInfo, Data, FileInfo, Error);
        except
          on E: Exception do
            InitError(Error, erFailure, E.Message);
        end;

        if Error.ErrorCode = erOk then begin
          FileInfoPacketWriter.WriteAsString(EncodeString(FileInfo.FileName));
          if FSessionInfo.Version <= 3 then
            FileInfoPacketWriter.WriteAsString(EncodeString(FileInfo.LongName));
          TScSFTPFileAttributesUtils.WriteToPacket(FileInfo.Attributes, FileInfoPacketWriter, FSessionInfo.Version, nil);
          Inc(Count);
        end;
      until (Error.ErrorCode <> erOk) or (FileInfoPacketWriter.DataLength >= MAX_SFTP_READ_DIR_BLOCK_SIZE);
    end;

    if (Error.ErrorCode = erOk) or ((Error.ErrorCode = erEof) and (Count > 0)) then begin
      FPacketWriter.InitPacket(SSH_FXP_NAME);
      FPacketWriter.WriteInt32(RequestId);
      FPacketWriter.WriteInt32(Count);
      FPacketWriter.WriteBuf(TValueArr(FileInfoPacketWriter.Data), 0, FileInfoPacketWriter.DataLength);
      if (Error.ErrorCode = erEof) and (FSessionInfo.Version >= 6) then
        FPacketWriter.WriteBool(True);
      TransmitResponse;
    end
    else
      RespondWithStatus(RequestId, Error);
  finally
    FileInfo.Free;
    FileInfoPacketWriter.Free;
  end;
end;

procedure TScSFTPServerHandler.ProcessRetrieveFileAttributesRequest(FollowSymLink: boolean);
var
  RequestId: integer;
  Path: string;
  Flags: integer;
  ReqAttrs: TScSFTPAttributes;
  Attributes: TScSFTPFileAttributes;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);
  if FSessionInfo.Version >= 4 then
    Flags := FPacketReader.ReadInt32
  else
    Flags := 0;
  ReqAttrs := ConvertSFTPValueToSFTPAttributes(Flags);

  Attributes := TScSFTPFileAttributes.Create;
  try
    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnRetrieveAttributes) then
        FSFTPServer.OnRetrieveAttributes(FSFTPServer, FSessionInfo, Path, FollowSymLink, ReqAttrs, Attributes, Error)
      else
        FSFTPServer.DefaultRetrieveAttributes(FSessionInfo, Path, FollowSymLink, ReqAttrs, Attributes, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;

    if Error.ErrorCode = erOk then begin
      FPacketWriter.InitPacket(SSH_FXP_ATTRS);
      FPacketWriter.WriteInt32(RequestId);
      TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FSessionInfo.Version, nil);
      TransmitResponse;
    end
    else
      RespondWithStatus(RequestId, Error);
  finally
    Attributes.Free;
  end;
end;

procedure TScSFTPServerHandler.ProcessRetrieveHandleAttributesRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  Flags: integer;
  ReqAttrs: TScSFTPAttributes;
  Attributes: TScSFTPFileAttributes;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;
  if FSessionInfo.Version >= 4 then
    Flags := FPacketReader.ReadInt32
  else
    Flags := 0;
  ReqAttrs := ConvertSFTPValueToSFTPAttributes(Flags);

  Attributes := TScSFTPFileAttributes.Create;
  try
    FindFileByHandle(Handle, Data, Error);

    if Error.ErrorCode = erOk then begin
      InitError(Error, erFailure);
      try
        if Assigned(FSFTPServer.OnRetrieveAttributesByHandle) then
          FSFTPServer.OnRetrieveAttributesByHandle(FSFTPServer, FSessionInfo, Data, ReqAttrs, Attributes, Error)
        else
          FSFTPServer.DefaultRetrieveAttributesByHandle(FSessionInfo, Data, ReqAttrs, Attributes, Error);
      except
        on E: Exception do
          InitError(Error, erFailure, E.Message);
      end;
    end;

    if Error.ErrorCode = erOk then begin
      FPacketWriter.InitPacket(SSH_FXP_ATTRS);
      FPacketWriter.WriteInt32(RequestId);
      TScSFTPFileAttributesUtils.WriteToPacket(Attributes, FPacketWriter, FSessionInfo.Version, nil);
      TransmitResponse;
    end
    else
      RespondWithStatus(RequestId, Error);
  finally
    Attributes.Free;
  end;
end;

procedure TScSFTPServerHandler.DoSetAttributes(const Path: string;
  Attributes: TScSFTPFileAttributes; out Error: TScSFTPError);
begin
  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnSetAttributes) then
      FSFTPServer.OnSetAttributes(FSFTPServer, FSessionInfo, Path, Attributes, Error)
    else
      FSFTPServer.DefaultSetAttributes(FSessionInfo, Path, Attributes, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;
end;

procedure TScSFTPServerHandler.ProcessSetFileAttributesRequest;
var
  RequestId: integer;
  Path: string;
  Attributes: TScSFTPFileAttributes;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);

  Attributes := TScSFTPFileAttributes.Create;
  try
    TScSFTPFileAttributesUtils.ReadFromPacket(Attributes, FPacketReader, FSessionInfo.Version);
    DoSetAttributes(Path, Attributes, Error);
    RespondWithStatus(RequestId, Error);
  finally
    Attributes.Free;
  end;
end;

procedure TScSFTPServerHandler.DoSetAttributesByHandle(Data: TObject;
  Attributes: TScSFTPFileAttributes; out Error: TScSFTPError);
begin
  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnSetAttributesByHandle) then
      FSFTPServer.OnSetAttributesByHandle(FSFTPServer, FSessionInfo, Data, Attributes, Error)
    else
      FSFTPServer.DefaultSetAttributesByHandle(FSessionInfo, Data, Attributes, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;
end;

procedure TScSFTPServerHandler.ProcessSetHandleAttributesRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  Attributes: TScSFTPFileAttributes;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;

  Attributes := TScSFTPFileAttributes.Create;
  try
    TScSFTPFileAttributesUtils.ReadFromPacket(Attributes, FPacketReader, FSessionInfo.Version);
    FindFileByHandle(Handle, Data, Error);

    if Error.ErrorCode = erOk then
      DoSetAttributesByHandle(Data, Attributes, Error);

    RespondWithStatus(RequestId, Error);
  finally
    Attributes.Free;
  end;
end;

procedure TScSFTPServerHandler.ProcessReadLinkRequest;
var
  RequestId: integer;
  Path, Link: string;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);

  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnReadSymbolicLink) then
      FSFTPServer.OnReadSymbolicLink(FSFTPServer, FSessionInfo, Path, Link, Error)
    else
      FSFTPServer.DefaultReadSymbolicLink(FSessionInfo, Path, Link, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;

  if Error.ErrorCode = erOk then begin
    FPacketWriter.InitPacket(SSH_FXP_NAME);
    FPacketWriter.WriteInt32(RequestId);
    FPacketWriter.WriteInt32(1);
    FPacketWriter.WriteAsString(EncodeString(Link));
    if FSessionInfo.Version <= 3 then
      FPacketWriter.WriteAsString(EncodeString(Link));
    TScSFTPFileAttributesUtils.WriteToPacketDefaultValues(FPacketWriter, ftSymlink, FSessionInfo.Version);
    TransmitResponse;
  end
  else
    RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessCreateLinkRequest;
var
  RequestId: integer;
  LinkPath, TargetPath: string;
  Symbolic: boolean;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  LinkPath := DecodeBytes(FPacketReader.ReadString);
  TargetPath := DecodeBytes(FPacketReader.ReadString);
  if FSessionInfo.Version >= 6 then
    Symbolic := FPacketReader.ReadBool
  else
    Symbolic := True;

  InitError(Error, erFailure);
  try
    if Assigned(FSFTPServer.OnCreateLink) then
      FSFTPServer.OnCreateLink(FSFTPServer, FSessionInfo, LinkPath, TargetPath, Symbolic, Error)
    else
      FSFTPServer.DefaultCreateLink(FSessionInfo, LinkPath, TargetPath, Symbolic, Error);
  except
    on E: Exception do
      InitError(Error, erFailure, E.Message);
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessRealPathRequest;
var
  RequestId: integer;
  Path, AbsolutePath: string;
  Control: TScSFTPRealpathControl;
  ComposePathList: TStringList;
  FileType: TScSFTPFileType;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Path := DecodeBytes(FPacketReader.ReadString);
  ComposePathList := TStringList.Create;
  try
    if FSessionInfo.Version >= 6 then begin
      if FPacketReader.Rest > 0 then
        Control := ConvertSFTPValueToRealpathControl(FPacketReader.ReadByte)
      else
        Control := rcNoCheck;

      while FPacketReader.Rest > 0 do
        ComposePathList.Add(DecodeBytes(FPacketReader.ReadString));
    end
    else
      Control := rcStatAlways;

    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnGetAbsolutePath) then
        FSFTPServer.OnGetAbsolutePath(FSFTPServer, FSessionInfo, Path, Control, ComposePathList, AbsolutePath, FileType, Error)
      else
        FSFTPServer.DefaultGetAbsolutePath(FSessionInfo, Path, Control, ComposePathList, AbsolutePath, FileType, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;

    if Error.ErrorCode = erOk then begin
      FPacketWriter.InitPacket(SSH_FXP_NAME);
      FPacketWriter.WriteInt32(RequestId);
      FPacketWriter.WriteInt32(1);
      FPacketWriter.WriteAsString(EncodeString(AbsolutePath));
      if FSessionInfo.Version <= 3 then
        FPacketWriter.WriteAsString(EncodeString(AbsolutePath));
      TScSFTPFileAttributesUtils.WriteToPacketDefaultValues(FPacketWriter, FileType, FSessionInfo.Version);
      TransmitResponse;
    end
    else
      RespondWithStatus(RequestId, Error);
  finally
    ComposePathList.Free;
  end;
end;

procedure TScSFTPServerHandler.ProcessBlockRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  Offset, Len: Int64;
  BlockModes: TScSFTPBlockModes;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;
  Offset := FPacketReader.ReadInt64;
  Len := FPacketReader.ReadInt64;
  BlockModes := ConvertSFTPValueToBlockModes(FPacketReader.ReadInt32);

  FindFileByHandle(Handle, Data, Error);

  if Error.ErrorCode = erOk then begin
    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnBlockFile) then
        FSFTPServer.OnBlockFile(FSFTPServer, FSessionInfo, Data, Offset, Len, BlockModes, Error)
      else
        FSFTPServer.DefaultBlockFile(FSessionInfo, Data, Offset, Len, BlockModes, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessUnBlockRequest;
var
  RequestId: integer;
  Handle: TScSFTPFileHandle;
  Offset, Len: Int64;
  Data: TObject;
  Error: TScSFTPError;
begin
  RequestId := FPacketReader.ReadInt32;
  Handle := FPacketReader.ReadString;
  Offset := FPacketReader.ReadInt64;
  Len := FPacketReader.ReadInt64;

  FindFileByHandle(Handle, Data, Error);

  if Error.ErrorCode = erOk then begin
    InitError(Error, erFailure);
    try
      if Assigned(FSFTPServer.OnUnBlockFile) then
        FSFTPServer.OnUnBlockFile(FSFTPServer, FSessionInfo, Data, Offset, Len, Error)
      else
        FSFTPServer.DefaultUnBlockFile(FSessionInfo, Data, Offset, Len, Error);
    except
      on E: Exception do
        InitError(Error, erFailure, E.Message);
    end;
  end;

  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessExtensionRequest;
var
  RequestId: integer;
  ExtName: string;
//  ExtData: TBytes;
  ver: string;
  verI: integer;
  Error: TScSFTPError;
begin
//  SetLength(ExtData, 0);
  RequestId := FPacketReader.ReadInt32;
  ExtName := DecodeBytes(FPacketReader.ReadString);
  if ExtName = 'version-select' then begin
    ver := DecodeBytes(FPacketReader.ReadString);
    verI := StrToIntDef(ver, 0);
    if (verI >= 3) and (verI <= 6) then begin
      TScSFTPSessionInfoUtils.SetVersion(FSessionInfo, verI);
      InitError(Error, erOk);
      RespondWithStatus(RequestId, Error);
      Exit;
    end;
  end
  else
    {ExtData := }FPacketReader.ReadAll;

  InitError(Error, erOpUnsupported);
  RespondWithStatus(RequestId, Error);
end;

procedure TScSFTPServerHandler.ProcessRequest;
var
  TmpBuf: TBytes;
  PacketLength: integer;
  PacketType: SFTPPacketType;
  Error: TScSFTPError;
begin
  FPacketReader.Reset;
  FPacketReader.CheckAndReallocBuffer(4);
  TmpBuf := FPacketReader.Image;
  if FChannel.ReadBuffer(TmpBuf, 0, 4) < 4 then
    raise EScError.Create(seConnectionTimeout);

  PacketLength := FPacketReader.ReadInt32;
  if (PacketLength > MAX_SFTP_PACKET_LENGTH) or (PacketLength < 1) then
    raise EScError.Create(seInvalidMessageLength);

  FPacketReader.CheckAndReallocBuffer(PacketLength);
  TmpBuf := FPacketReader.Image;
  if FChannel.ReadBuffer(TmpBuf, FPacketReader.Offset, PacketLength) < PacketLength then
    raise EScError.Create(seConnectionTimeout);

  PacketType := FPacketReader.ReadPacketType;

  if not FInitialized then begin
    if PacketType <> SSH_FXP_INIT then
      raise EScError.Create(seUnexpectedPacketType);

    TScSFTPSessionInfoUtils.SetVersion(FSessionInfo, FPacketReader.ReadInt32);

    try
      if Assigned(FSFTPServer.OnOpen) then
        FSFTPServer.OnOpen(FSFTPServer, FSessionInfo);

    except
      on E: Exception do
        FInitializationErrorMessage := E.Message;
    end;

    FNeedCallOnClose := True;

    if (FSessionInfo.Version > 6) or (FSessionInfo.Version < 1) then
      raise EScError.Create(seUnknownSFTPClientVersion);

    FPacketWriter.InitPacket(SSH_FXP_VERSION);
    FPacketWriter.WriteInt32(FSessionInfo.Version);

    if FSessionInfo.Version >= 3 then begin
      FPacketWriter.WriteAsString(EncodeString('versions'));
      FPacketWriter.WriteAsString(EncodeString('3,4,5,6'));
      FPacketWriter.WriteAsString(EncodeString('newline'));
      FPacketWriter.WriteAsString(EncodeString(FSessionInfo.EOL));
    end;

    TransmitResponse;
    FInitialized := True;
    Exit;
  end;

  if FInitializationErrorMessage <> '' then begin
    InitError(Error, erFailure, FInitializationErrorMessage);
    RespondWithStatus(FPacketReader.ReadInt32, Error);

    raise EScError.CreateFmt(SSFTPInitializationFail, [FInitializationErrorMessage], seSFTPInitializationFail);
  end;

  case PacketType of
    SSH_FXP_OPEN:
      ProcessOpenFileRequest;
    SSH_FXP_CLOSE:
      ProcessCloseFileRequest;
    SSH_FXP_READ:
      ProcessReadFileRequest;
    SSH_FXP_WRITE:
      ProcessWriteFileRequest;
    SSH_FXP_REMOVE:
      ProcessRemoveFileRequest;
    SSH_FXP_RENAME:
      ProcessRenameFileRequest;
    SSH_FXP_MKDIR:
      ProcessMakeDirRequest;
    SSH_FXP_RMDIR:
      ProcessRemoveDirRequest;
    SSH_FXP_OPENDIR:
      ProcessOpenDirRequest;
    SSH_FXP_READDIR:
      ProcessReadDirRequest;
    SSH_FXP_STAT:
      ProcessRetrieveFileAttributesRequest(True);
    SSH_FXP_LSTAT:
      ProcessRetrieveFileAttributesRequest(False);
    SSH_FXP_FSTAT:
      ProcessRetrieveHandleAttributesRequest;
    SSH_FXP_SETSTAT:
      ProcessSetFileAttributesRequest;
    SSH_FXP_FSETSTAT:
      ProcessSetHandleAttributesRequest;
    SSH_FXP_READLINK:
      ProcessReadLinkRequest;
    SSH_FXP_SYMLINK, SSH_FXP_LINK:
      ProcessCreateLinkRequest;
    SSH_FXP_REALPATH:
      ProcessRealPathRequest;
    SSH_FXP_BLOCK:
      ProcessBlockRequest;
    SSH_FXP_UNBLOCK:
      ProcessUnBlockRequest;
    SSH_FXP_EXTENDED:
      ProcessExtensionRequest;
  else
    raise EScError.Create(seUnexpectedPacketType);
  end;
end;

procedure TScSFTPServerHandler.Init;
begin
{$IFDEF MSWINDOWS}
  if FCoInitializeThreadID = 0 then
    if CoInitialize(nil) in [S_OK, S_FALSE] then
      FCoInitializeThreadID := GetCurrentThreadId;
{$ENDIF}
end;

procedure TScSFTPServerHandler.Close;
var
  Error: TScSFTPError;
begin
  try
    while FHandleInfoList.Count > 0 do begin
      DoCloseFile(FHandleInfoList[0].Data, Error);
      FHandleInfoList.Delete(0);
    end;

    if FNeedCallOnClose then begin
      FNeedCallOnClose := False;

      try
        if Assigned(FSFTPServer.OnClose) then
          FSFTPServer.OnClose(FSFTPServer, FSessionInfo);
      except
      end;
    end;

    FChannel.Disconnect;
  finally
  {$IFDEF MSWINDOWS}
    // Because Close is called also from destructor, which is called
    // from another thread as which call Init;
    if FCoInitializeThreadID = GetCurrentThreadId then begin
      FCoInitializeThreadID := 0;
      CoUninitialize();
    end;
  {$ENDIF}
  end;
end;

{ TScSFTPServer }

constructor TScSFTPServer.Create(AOwner: TComponent);
begin
  inherited;

  FUseUnicode := True;
  FDefaultRootPath := '';
end;

function TScSFTPServer.ExtractPathDrive(const Path: string): string;
begin
  Result := '';
  if Path = '~' then
    Result := '~' + PathDelim
  else
  if Length(Path) >= 2 then begin
    if Path[2] = DriveDelim then // c:
      Result := Path[1] + DriveDelim + PathDelim
    else
    if ((Path[1] = '\') or (Path[1] = '/') or (Path[1] = '~')) and
       ((Path[2] = '\') or (Path[2] = '/')) then // \\ or ~\
      Result := Path[1] + PathDelim;
  end;

{$IFNDEF MSWINDOWS}
  if (Result = '') and (Length(Path) > 0) and (Path[1] = PathDelim) then
    Result := PathDelim;
{$ENDIF}
end;

function TScSFTPServer.GetCanonicalPath(const Path: string): string;
var
  Prefix, TmpPath: string;
  IsAbsolutePath: boolean;
  SubDirs: TStringList;
  i, j: integer;
begin
  TmpPath := Path;
  for i := 1 to Length(TmpPath) do
    if TmpPath[i] = {$IFDEF MSWINDOWS}'/'{$ELSE}'\'{$ENDIF} then
      TmpPath[i] := PathDelim;

  Prefix := ExtractPathDrive(TmpPath);
  IsAbsolutePath := Prefix <> '';
  if IsAbsolutePath then
    TmpPath := Copy(TmpPath, Length(Prefix) + 1, Length(TmpPath));

  SubDirs := Split(TmpPath, PathDelim);
  try
    for i := 0 to SubDirs.Count - 1 do begin
      if SubDirs[i] = ' ' then
        SubDirs[i] := ''
      else
      if SubDirs[i] = '.' then
        SubDirs[i] := ''
      else
      if SubDirs[i] = '..' then begin
        j := i - 1;

        while (j >= 0) and ((SubDirs[j] = '') or (SubDirs[j] = '..')) do
          Dec(j);

        if j >= 0 then begin
          SubDirs[j] := '';
          SubDirs[i] := '';
        end
        else
        if IsAbsolutePath then
          SubDirs[i] := '';
      end;
    end;

    Result := '';
    for i := 0 to SubDirs.Count - 1 do begin
      if SubDirs[i] <> '' then begin
        if Result <> '' then
          Result := Result + PathDelim;
        Result := Result + SubDirs[i];
      end;
    end;

    if Prefix <> '' then
      Result := Prefix + Result;
  finally
    SubDirs.Free;
  end;
end;

function TScSFTPServer.GetFullPath(SFTPSessionInfo: TScSFTPSessionInfo; const Path: string): string;
begin
  Result := Path;
  if Assigned(OnGetFullPath) then
    OnGetFullPath(Self, SFTPSessionInfo, Result)
  else
    DefaultGetFullPath(SFTPSessionInfo, Result);

  if Result = '' then
    raise EScError.Create(sePathNotAllowed);

  if Result <> PathDelim then
    Result := ExcludeTrailingBackslash(Result);
end;

procedure TScSFTPServer.DefaultGetFullPath(SFTPSessionInfo: TScSFTPSessionInfo; var Path: string);
var
  Drive: string;
begin
  Drive := ExtractPathDrive(Path);
  if Drive = '~' + PathDelim then begin
    Drive := '';
    Delete(Path, 1, 1);
  end;

  if Drive = '' then
    if (Path = '\') or (Path = '/') then
      Path := SFTPSessionInfo.HomePath
    else
    if (Length(Path) > 0) and ((Path[1] = '\') or (Path[1] = '/')) then
      Path := SFTPSessionInfo.HomePath + Path
    else
      Path := IncludeTrailingBackslash(SFTPSessionInfo.HomePath) + Path;

  Path := GetCanonicalPath(Path);
end;

{$IFDEF UNIX}
const
  INVALID_HANDLE_VALUE = THandle(-1);

function GetLastError: integer;
begin
  Result := fpgeterrno;
end;
{$ENDIF}

procedure TScSFTPServer.DefaultOpenFile(SFTPSessionInfo: TScSFTPSessionInfo;
  const FileName: string; const OpenAttributes: TScSFTPFileOpenAttributes;
  out Data: TObject; var Error: TScSFTPError);
{$IFNDEF VER15P}
{$IFDEF MSWINDOWS}
const
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;
{$ENDIF}
{$ENDIF}
var
{$IFDEF MSWINDOWS}
  dwDesiredAccess, dwShareMode, dwCreationDisposition, dwFlagsAndAttributes: DWORD;
{$ELSE}
  Flags, Mode: integer;
  AFullFileName: AnsiString;
{$ENDIF}
  FullFileName: string;
  Handle: THandle;
begin
  FullFileName := GetFullPath(SFTPSessionInfo, FileName);
{$IFDEF MSWINDOWS}
  dwDesiredAccess := ConvertAceMaskToSFTPValue(OpenAttributes.DesiredAccess);
  dwShareMode := 0;
  if not (bmRead in OpenAttributes.BlockModes) then
    dwShareMode := dwShareMode or FILE_SHARE_READ;
  if not (bmWrite in OpenAttributes.BlockModes) then
    dwShareMode := dwShareMode or FILE_SHARE_WRITE;
  if not (bmDelete in OpenAttributes.BlockModes) then
    dwShareMode := dwShareMode or FILE_SHARE_DELETE;

  case OpenAttributes.Mode of
    fmCreateNew:
      dwCreationDisposition := CREATE_NEW;
    fmCreateOrTruncate:
      dwCreationDisposition := CREATE_ALWAYS;
    fmOpenExisting:
      dwCreationDisposition := OPEN_EXISTING;
    fmOpenOrCreate:
      dwCreationDisposition := OPEN_ALWAYS;
    fmTruncateExisting:
      dwCreationDisposition := TRUNCATE_EXISTING;
  else
    dwCreationDisposition := 0;
    Assert(False);
  end;

  dwFlagsAndAttributes := FILE_ATTRIBUTE_NORMAL;
  if ofDeleteOnClose in OpenAttributes.Flags then
    dwFlagsAndAttributes := dwFlagsAndAttributes or FILE_FLAG_DELETE_ON_CLOSE;
  if ofNoFollow in OpenAttributes.Flags then
    dwFlagsAndAttributes := dwFlagsAndAttributes or FILE_FLAG_OPEN_REPARSE_POINT;

  Handle := CreateFile(PChar(FullFileName),
    dwDesiredAccess, dwShareMode, nil, dwCreationDisposition, dwFlagsAndAttributes, 0);
{$ELSE}
  Flags := 0;
  if ((amReadData in OpenAttributes.DesiredAccess) or (amReadAttributes in OpenAttributes.DesiredAccess)) and
    ((amWriteData in OpenAttributes.DesiredAccess) or (amWriteAttributes in OpenAttributes.DesiredAccess)) then
    Flags := O_RDWR
  else if (amReadData in OpenAttributes.DesiredAccess) or (amReadAttributes in OpenAttributes.DesiredAccess) then
    Flags := O_RDONLY
  else if (amWriteData in OpenAttributes.DesiredAccess) or (amWriteAttributes in OpenAttributes.DesiredAccess) then
    Flags := O_WRONLY;

  if (ofAppendData in OpenAttributes.Flags) or (ofAppendDataAtomic in OpenAttributes.Flags) then
    Flags := Flags or O_APPEND;

  case OpenAttributes.Mode of
    fmCreateNew:
      Flags := Flags or O_CREAT or O_EXCL;
    fmCreateOrTruncate:
      Flags := Flags or O_CREAT or O_TRUNC;
    fmOpenExisting:
      ;
    fmOpenOrCreate:
      Flags := Flags or O_CREAT;
    fmTruncateExisting:
      Flags := Flags or O_TRUNC;
  else
    Assert(False);
  end;

  if (OpenAttributes.Attributes <> nil) and (aPermissions in OpenAttributes.Attributes.ValidAttributes) then
    Mode := ConvertFilePermissionsToSFTPValue(OpenAttributes.Attributes.Permissions)
  else
    Mode := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;

  AFullFileName := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(FullFileName));
  Handle := THandle({$IFDEF POSIX}__open{$ELSE}fpOpen{$ENDIF}(PAnsiChar(AFullFileName), Flags, Mode));
{$ENDIF}

  if Handle <> INVALID_HANDLE_VALUE then begin
    Data := TScHandle.Create(Handle);
    TScHandle(Data).FFullFileName := FullFileName;
    TScHandle(Data).FInAppendMode := (ofAppendData in OpenAttributes.Flags) or (ofAppendDataAtomic in OpenAttributes.Flags);
    TScHandle(Data).FInTextMode := ofTextMode in OpenAttributes.Flags;
    TScHandle(Data).FRWStarted := False;
    InitError(Error, erOk);
  end
  else begin
    Data := nil;
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
  end;
end;

procedure TScSFTPServer.DefaultCloseFile(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; var Error: TScSFTPError);
begin
  if Data <> nil then
    if Data is TScSearchRec then begin
      SysUtils.FindClose(TScSearchRec(Data).FSearchRec);
      InitError(Error, erOk);
    end
    else
    if Data is TScHandle then begin
      FileClose(TScHandle(Data).FHandle);
      InitError(Error, erOk);
    end;
end;

procedure TScSFTPServer.DefaultReadFile(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; Offset: Int64; Count: cardinal;
  var Buffer: TBytes; var Read: cardinal; var Error: TScSFTPError);
begin
  if Data is TScHandle then begin
    if not TScHandle(Data).FInTextMode or not TScHandle(Data).FRWStarted then begin
      if TScHandle(Data).FInTextMode then
        Offset := 0; // set to beginning

      if FileSeek(NativeInt(TScHandle(Data).Handle), Offset, 0) = -1 then begin
        InitError(Error, erFailure, SysErrorMessage(GetLastError));
        Exit;
      end;
    end;

    TScHandle(Data).FRWStarted := True;

    Read := cardinal(FileRead(TScHandle(Data).Handle, Buffer[0], Count));

    if Read = cardinal(-1) then
      InitError(Error, erFailure, SysErrorMessage(GetLastError))
    else if Read = 0 then
      InitError(Error, erEof)
    else
      InitError(Error, erOk);
  end
  else
    InitError(Error, erFailure);
end;

procedure TScSFTPServer.DefaultWriteFile(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; Offset: Int64; const Buffer: TBytes; Count: integer; var Error: TScSFTPError);
var
  Origin: integer;
  Written: integer;
begin
  if Data is TScHandle then begin
    if not TScHandle(Data).FInTextMode or not TScHandle(Data).FRWStarted then begin
      if TScHandle(Data).FInTextMode then
        Offset := 0; // set to beginning

      if TScHandle(Data).FInAppendMode then begin
        Offset := 0;
        Origin := 2; {soEnd}
      end
      else
        Origin := 0;

      if FileSeek(NativeInt(TScHandle(Data).Handle), Offset, Origin) = -1 then begin
        InitError(Error, erFailure, SysErrorMessage(GetLastError));
        Exit;
      end;
    end;

    TScHandle(Data).FRWStarted := True;

    Written := integer(FileWrite(TScHandle(Data).Handle, Buffer[0], Count));

    if Written = -1 then
      InitError(Error, erFailure, SysErrorMessage(GetLastError))
    else if Written < Count then
      InitError(Error, erEof)
    else
      InitError(Error, erOk);
  end
  else
    InitError(Error, erFailure);
end;

procedure TScSFTPServer.DefaultRemoveFile(SFTPSessionInfo: TScSFTPSessionInfo;
  const FileName: string; var Error: TScSFTPError);
begin
  if SysUtils.DeleteFile(GetFullPath(SFTPSessionInfo, FileName)) then
    InitError(Error, erOk)
  else
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
end;

procedure TScSFTPServer.DefaultRenameFile(SFTPSessionInfo: TScSFTPSessionInfo;
  const OldName, NewName: string; const Flags: TScSFTPRenameFlags;
  var Error: TScSFTPError);
var
  FullOldName, FullNewName: string;
begin
  if rfAtomic in Flags then begin
    InitError(Error, erOpUnsupported);
    Exit;
  end;

  FullOldName := GetFullPath(SFTPSessionInfo, OldName);
  FullNewName := GetFullPath(SFTPSessionInfo, NewName);

  if (rfOverwrite in Flags) and FileExists(FullOldName) and FileExists(FullNewName) then
    SysUtils.DeleteFile(FullNewName);

  if SysUtils.RenameFile(FullOldName, FullNewName) then
    InitError(Error, erOk)
  else
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
end;

procedure TScSFTPServer.DefaultMakeDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; var Error: TScSFTPError);
begin
  if CreateDir(GetFullPath(SFTPSessionInfo, Path)) then
    InitError(Error, erOk)
  else
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
end;

procedure TScSFTPServer.DefaultRemoveDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; var Error: TScSFTPError);
begin
  if RemoveDir(GetFullPath(SFTPSessionInfo, Path)) then
    InitError(Error, erOk)
  else
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
end;

procedure TScSFTPServer.DefaultOpenDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; out Data: TObject; var Error: TScSFTPError);
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  FullPath := GetFullPath(SFTPSessionInfo, Path);
  if FindFirst(IncludeTrailingBackslash(FullPath) + '*', faAnyFile, SearchRec) = 0 then begin
    Data := TScSearchRec.Create(SearchRec);
    TScSearchRec(Data).FPath := FullPath;
    InitError(Error, erOk);
  end
  else begin
    Data := nil;
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
  end;
end;

procedure TScSFTPServer.DefaultReadDirectory(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; FileInfo: TScSFTPFileInfo; var Error: TScSFTPError);
begin
  if Data is TScSearchRec then begin
    if TScSearchRec(Data).FIsBof then
      TScSearchRec(Data).FIsBof := False
    else
    if FindNext(TScSearchRec(Data).FSearchRec) <> 0 then begin
      InitError(Error, erEof);
      Exit;
    end;

    FileInfo.FillInfo(TScSearchRec(Data).FPath, TScSearchRec(Data).FSearchRec);
    InitError(Error, erOk);
  end
  else
    InitError(Error, erFailure);
end;

procedure TScSFTPServer.DefaultRetrieveAttributes(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; FollowSymLink: boolean; const ReqAttrs: TScSFTPAttributes;
  Attributes: TScSFTPFileAttributes; var Error: TScSFTPError);
var
  SearchRec: TSearchRec;
  DestPath: string;
{$IFDEF MSWINDOWS}
  IObj: IUnknown;
  ILink: IShellLink;
  IPFile: IPersistFile;
  fd: TWin32FindData;
  ws: WideString;
  pPath: PChar;
{$ENDIF}
begin
  DestPath := GetFullPath(SFTPSessionInfo, Path);
{$IFDEF MSWINDOWS}
  if FollowSymLink then
    try
      IObj := CreateComObject(CLSID_ShellLink);
      ILink := IObj as IShellLink;
      IPFile := IObj as IPersistFile;

      ws := WideString(DestPath);
      if IPFile.Load(POleStr(ws), 0) = S_OK then begin
        pPath := Marshal.AllocHGlobal(MAX_PATH * sizeof(char));
        if ILink.GetPath(pPath, MAX_PATH, fd, 0) = S_OK then
          DestPath := string(pPath);
        Marshal.FreeHGlobal(pPath);
      end;
    except
    end;
{$ENDIF} // processed in RequestInfo for UNIX

{$IFDEF MSWINDOWS}
  if (Length(DestPath) = 2) and (DestPath[2] = DriveDelim) then // c:
    DestPath := IncludeTrailingBackslash(DestPath) + '*.*';
{$ENDIF}

  if FindFirst(DestPath, faAnyFile, SearchRec) = 0 then begin
    try
      TScSFTPFileAttributesUtils.RequestInfo(Attributes, DestPath, SearchRec, ReqAttrs, FollowSymLink);
    finally
      SysUtils.FindClose(SearchRec);
    end;
    InitError(Error, erOk);
  end
  else
    InitError(Error, erNoSuchFile);
end;

procedure TScSFTPServer.DefaultRetrieveAttributesByHandle(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; const ReqAttrs: TScSFTPAttributes;
  Attributes: TScSFTPFileAttributes; var Error: TScSFTPError);
begin
  if Data is TScHandle then begin
    TScSFTPFileAttributesUtils.RequestInfoByHandle(Attributes, TScHandle(Data).Handle, ReqAttrs);
    InitError(Error, erOk);
  end
  else
    InitError(Error, erFailure);
end;

procedure TScSFTPServer.DefaultSetAttributes(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; Attributes: TScSFTPFileAttributes; var Error: TScSFTPError);
begin
  if TScSFTPFileAttributesUtils.SetToFile(Attributes, GetFullPath(SFTPSessionInfo, Path)) then
    InitError(Error, erOk)
  else
    InitError(Error, erFailure, SysErrorMessage(GetLastError));
end;

procedure TScSFTPServer.DefaultSetAttributesByHandle(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; Attributes: TScSFTPFileAttributes; var Error: TScSFTPError);
begin
  if Data is TScHandle then begin
    if TScSFTPFileAttributesUtils.SetToFileByHandle(Attributes, TScHandle(Data).FFullFileName, TScHandle(Data).Handle) then
      InitError(Error, erOk)
    else
      InitError(Error, erFailure, SysErrorMessage(GetLastError));
  end
  else
    InitError(Error, erFailure);
end;

{$IFDEF VER15P}
{$DEFINE SC_USE_SYMLINK}
{$ENDIF}
{$IFDEF UNIX}
{$DEFINE SC_USE_SYMLINK}
{$ENDIF}

procedure TScSFTPServer.DefaultReadSymbolicLink(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; out SymbolicName: string; var Error: TScSFTPError);
var
  FullPath: string;
{$IFDEF MSWINDOWS}
  IObj: IUnknown;
  ILink: IShellLink;
  IPFile: IPersistFile;
  fd: TWin32FindData;
  ws: WideString;
  pPath: PChar;
{$ENDIF}
begin
  SymbolicName := '';
  FullPath := GetFullPath(SFTPSessionInfo, Path);
  if not FileExists(FullPath) and (ExtractFileExt(FullPath) = '') then
    FullPath := ChangeFileExt(FullPath, '.lnk');

{$IFDEF SC_USE_SYMLINK}
  if FileGetSymLinkTarget(FullPath, SymbolicName) then
    InitError(Error, erOk)
  else
{$ENDIF}
  {$IFDEF MSWINDOWS}
    if FileExists(FullPath) then begin
      InitError(Error, erFailure);
      try
        IObj := CreateComObject(CLSID_ShellLink);
        ILink := IObj as IShellLink;
        IPFile := IObj as IPersistFile;

        ws := WideString(FullPath);
        if IPFile.Load(POleStr(ws), 0) = S_OK then begin
          pPath := Marshal.AllocHGlobal(MAX_PATH * sizeof(char));
          try
            if ILink.GetPath(pPath, MAX_PATH, fd, 0) = S_OK then begin
              SymbolicName := string(pPath);
              SymbolicName := ExtractRelativePath(SFTPSessionInfo.HomePath, SymbolicName);
              InitError(Error, erOk);
              Exit;
            end;
          finally
            Marshal.FreeHGlobal(pPath);
          end;
        end;
      finally
        if Error.ErrorCode <> erOk then
          InitError(Error, erFailure, SysErrorMessage(GetLastError));
      end;
    end
    else
  {$ENDIF}
      InitError(Error, erNoSuchFile);
end;

procedure TScSFTPServer.DefaultCreateLink(SFTPSessionInfo: TScSFTPSessionInfo;
  const LinkPath, TargetPath: string; Symbolic: boolean; var Error: TScSFTPError);
var
  FullLinkPath, FullTargetPath: string;
{$IFDEF MSWINDOWS}
  IObj: IUnknown;
  ILink: IShellLink;
  IPFile: IPersistFile;
  ws: WideString;
{$ENDIF}
begin
  FullLinkPath := GetFullPath(SFTPSessionInfo, LinkPath);
  FullTargetPath := GetFullPath(SFTPSessionInfo, TargetPath);

  if not Symbolic then
    InitError(Error, erOpUnsupported)
  else
{$IFDEF SC_USE_SYMLINK}
  if FileCreateSymLink(FullLinkPath, FullTargetPath) then
    InitError(Error, erOk)
  else
{$ENDIF}
{$IFDEF MSWINDOWS}
  begin
    if ExtractFileExt(FullLinkPath) = '' then
      FullLinkPath := ChangeFileExt(FullLinkPath, '.lnk');

    if not FileExists(FullLinkPath) then begin
      InitError(Error, erFailure);
      try
        IObj := CreateComObject(CLSID_ShellLink);
        ILink := IObj as IShellLink;
        IPFile := IObj as IPersistFile;

        if ILink.SetPath(PChar(FullTargetPath)) <> S_OK then
          Exit;
        ws := WideString(FullLinkPath);
        if IPFile.Save(POleStr(ws), False) <> S_OK then
          Exit;

        InitError(Error, erOk);
      finally
        if Error.ErrorCode <> erOk then
          InitError(Error, erFailure, SysErrorMessage(GetLastError));
      end;
    end
    else
      InitError(Error, erFailure, 'File already exists');
  end;
{$ELSE}
  InitError(Error, erFailure);
{$ENDIF}
end;

procedure TScSFTPServer.DefaultGetAbsolutePath(SFTPSessionInfo: TScSFTPSessionInfo;
  const Path: string; const Control: TScSFTPRealpathControl; ComposePathList: TStringList;
  out AbsolutePath: string; out FileType: TScSFTPFileType; var Error: TScSFTPError);
var
  Drive, FullPath, ComposePath: string;
  HomePath: string;
  i: integer;
begin
  AbsolutePath := GetCanonicalPath(Path);
  for i := 0 to ComposePathList.Count - 1 do begin
    ComposePath := ComposePathList[i];
    if ComposePath <> '' then begin
      if (ExtractPathDrive(ComposePath) <> '') or (ComposePath[1] = '\') or (ComposePath[1] = '/') then begin
        if i > 0 then
          Break
        else
          AbsolutePath := GetCanonicalPath(ComposePath);
      end
      else
        AbsolutePath := IncludeTrailingBackslash(AbsolutePath) + GetCanonicalPath(ComposePath);
    end;
  end;

  FullPath := GetFullPath(SFTPSessionInfo, AbsolutePath);
  Drive := ExtractPathDrive(AbsolutePath);

  if Drive = '' then begin
    AbsolutePath := IncludeTrailingBackslash(FullPath);
    HomePath := IncludeTrailingBackslash(SFTPSessionInfo.HomePath);

    Drive := ExtractPathDrive(HomePath);
    if Drive <> '' then begin
      Delete(HomePath, 1, Length(Drive));
      Delete(AbsolutePath, 1, Length(Drive));
    end;

    AbsolutePath := ExtractRelativePath(HomePath, AbsolutePath);
    AbsolutePath := ExcludeTrailingBackslash(AbsolutePath);
    if AbsolutePath = '..' then
      AbsolutePath := '';
  end
  else
  if AbsolutePath <> PathDelim then
    AbsolutePath := ExcludeTrailingBackslash(AbsolutePath);

  if AbsolutePath = '' then
    AbsolutePath := PathDelim
  else
  if (AbsolutePath[1] <> PathDelim) and ((Length(AbsolutePath) < 2) or (AbsolutePath[2] <> DriveDelim)) then
    AbsolutePath := PathDelim + AbsolutePath;

  if DirectoryExists(FullPath) then begin
    FileType := ftDirectory;
    InitError(Error, erOk);
  end
  else
  if FileExists(FullPath) then begin
    FileType := ftFile;
    InitError(Error, erOk);
  end
  else begin
    FileType := ftUnknown;
    if Control <> rcStatAlways then
      InitError(Error, erOk)
    else
      InitError(Error, erNoSuchFile);
  end;

  for i := 1 to Length(AbsolutePath) do
    if AbsolutePath[i] = '\' then
      AbsolutePath[i] := '/';
end;

procedure TScSFTPServer.DefaultBlockFile(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; const Offset, Len: Int64; const BlockModes: TScSFTPBlockModes;
  var Error: TScSFTPError);
begin
  if Data is TScHandle then
    InitError(Error, erOpUnsupported)
  else
    InitError(Error, erFailure);
end;

procedure TScSFTPServer.DefaultUnBlockFile(SFTPSessionInfo: TScSFTPSessionInfo;
  Data: TObject; const Offset, Len: Int64; var Error: TScSFTPError);
begin
  if Data is TScHandle then
    InitError(Error, erOpUnsupported)
  else
    InitError(Error, erFailure);
end;

procedure TScSFTPServer.DoRequestFileSecurityAttributes(Attributes: TScSFTPFileAttributes;
  const Path: string; SecurityDescriptor: Pointer);
begin
  if Assigned(FOnRequestFileSecurityAttributes) then
    FOnRequestFileSecurityAttributes(Self, Attributes, Path, SecurityDescriptor);
end;

end.

