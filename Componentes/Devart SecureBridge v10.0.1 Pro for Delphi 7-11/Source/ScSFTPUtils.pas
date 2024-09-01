
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSFTPUtils;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysStat,
{$ENDIF}
{$IFDEF UNIX}
  BaseUnix, UnixType,
{$ENDIF}
{$IFNDEF NEXTGEN}
  Contnrs,
{$ENDIF}
  Classes, SysUtils,
  ScCLRClasses, ScTypes, ScUtils, ScSSHUtils, ScSFTPPacket, ScSFTPConsts;

type
  TScSFTPVersion = (vSFTP0, vSFTP1, vSFTP2, vSFTP3, vSFTP4, vSFTP5, vSFTP6);
  TScSFTPVersions = set of TScSFTPVersion;

  TScSFTPRenameFlag = (rfOverwrite, rfAtomic, rfNative);
  TScSFTPRenameFlags = set of TScSFTPRenameFlag;

  TScSFTPRealpathControl = (rcNoCheck, rcStatIf, rcStatAlways);

  TScSFTPFileOpenModeItem = (foRead, foWrite, foAppend, foCreate, foTrunc,
    foExcl, foText);
  TScSFTPFileOpenModes = set of TScSFTPFileOpenModeItem;

  TScSFTPFileOpenMode = (fmCreateNew, fmCreateOrTruncate, fmOpenExisting,
    fmOpenOrCreate, fmTruncateExisting);

  TScSFTPFileOpenFlag = (ofAppendData, ofAppendDataAtomic, ofTextMode, ofNoFollow,
    ofDeleteOnClose, ofAccessAudit, ofAccessBackup, ofBackupStream, ofOverrideOwner);
  TScSFTPFileOpenFlags = set of TScSFTPFileOpenFlag;
  
  TScSFTPBlockMode = (bmRead, bmWrite, bmDelete, bmAdvisory);
  TScSFTPBlockModes = set of TScSFTPBlockMode;

  TScSFTPFileType = (ftFile, ftDirectory, ftSymlink, ftSpecial, ftUnknown,
    ftSocket, ftCharDevice, ftBlockDevice, ftFifo);

  TScSFTPFilePermission = (pR_USR, pW_USR, pX_USR, pR_GRP, pW_GRP, pX_GRP,
    pR_OTH, pW_OTH, pX_OTH, pS_UID, pS_GID, pS_VTX);
  TScSFTPFilePermissions = set of TScSFTPFilePermission;

  TScSFTPAclFlag = (aclControlIncluded, aclControlPresent, aclControlInherited,
    aclAuditAlarmIncluded, aclAuditAlarmInherited);
  TScSFTPAclFlags = set of TScSFTPAclFlag;

  TScSFTPAceType = (atAccessAllowed, atAccessDenied, atSystemAudit, atSystemAlarm);

  TScSFTPAceFlag = (afFileInherit, afDirectoryInherit, afNo_PropagateInherit,
    afInheritOnly, afSuccessfulAccess, afFailedAccess, afIdentifierGroup);
  TScSFTPAceFlags = set of TScSFTPAceFlag;

  TScSFTPAceMaskItem = (amReadData, amListDirectory, amWriteData, amAddFile,
    amAppendData, amAddSubdirectory, amReadNamedAttrs, amWriteNamedAttrs,
    amExecute, amDeleteChild, amReadAttributes, amWriteAttributes,
    amDelete, amReadAcl, amWriteAcl, amWriteOwner, amSynchronize);
  TScSFTPAceMask = set of TScSFTPAceMaskItem;

  TScSFTPDesiredAccessItem = TScSFTPAceMaskItem;
  TScSFTPDesiredAccess = set of TScSFTPDesiredAccessItem;

  TScSFTPSupportedAcl = (saAllow, saDeny, saAudit, saAlarm,
    saInheritAccess, saInheritAuditAlarm);
  TScSFTPSupportedAcls = set of TScSFTPSupportedAcl;

  TScSFTPFileAttr = (faReadonly, faSystem, faHidden, faCaseInsensitive,
    faArchive, faEncrypted, faCompressed, faSparse, faAppendOnly, faImmutable,
    faSync, faTranslationError);
  TScSFTPFileAttrs = set of TScSFTPFileAttr;

  TScSFTPTextHint = (thKnownText, thGuessedText, thKnownBinary, thGuessedBinary);

  TScSFTPAttribute = (aSize, aAllocationSize, aOwnerGroup, aPermissions,
    aAccessTime, aCreateTime, aModifyTime, aChangeAttrTime, aSubsecondTimes,
    aAcl, aAttrs, aTextHint, aMimeType, aLinkCount, aUntranslatedName, aExtended);
  TScSFTPAttributes = set of TScSFTPAttribute;

  TScSFTPFileAttributes = class;

  TScRequestFileSecurityAttributes = procedure (
    Attributes: TScSFTPFileAttributes; const Path: string; SecurityDescriptor: Pointer) of object;

  TScSFTPFileHandle = TBytes;

  TScSFTPError = record
    ErrorCode: TScSFTPErrorCode;
    ErrorMessage: string;
  end;

  TScSFTPFileOpenAttributes = record
    Mode: TScSFTPFileOpenMode;
    Flags: TScSFTPFileOpenFlags;
    BlockModes: TScSFTPBlockModes;
    DesiredAccess: TScSFTPDesiredAccess;
    Attributes: TScSFTPFileAttributes;
  end;

  TScSFTPACEs = class(TScCollection)
  public
    constructor Create(AOwner: TPersistent);
  end;

  TScSFTPACEItem = class(TScCollectionItem)
  private
    FAceType: TScSFTPAceType;
    FAceFlags: TScSFTPAceFlags;
    FAceMask: TScSFTPAceMask;
    FWho: string;
  {$IFDEF MSWINDOWS}
    FSid: PSID;
  {$ENDIF}
    procedure SetWho(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  {$IFDEF MSWINDOWS}
    function GetSid: PSID;
  {$ENDIF}
  public
    destructor Destroy; override;
  published
    property AceType: TScSFTPAceType read FAceType write FAceType;
    property AceFlags: TScSFTPAceFlags read FAceFlags write FAceFlags;
    property AceMask: TScSFTPAceMask read FAceMask write FAceMask;
    property Who: string read FWho write SetWho;
  end;

  TScSFTPCustomExtension = class(TPersistent)
  private
    FName: string;
    FData: TBytes;
    procedure SetData(const Value: TBytes);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear; virtual;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); virtual;

    property Data: TBytes read FData write SetData;
  public
    property Name: string read FName write FName;
  end;

  TScSFTPExtension = class(TScSFTPCustomExtension)
  public
    property Data;
  end;

  TScFilenameTranslationControlExtension = class(TScSFTPCustomExtension)
  private
    FDoTranslate: Boolean;
    procedure SetDoTranslate(Value: Boolean);
  public
    constructor Create;
    property DoTranslate: Boolean read FDoTranslate write SetDoTranslate;
    property Data;
  end;

  TScCheckFileReplyExtension = class(TScSFTPCustomExtension)
  private
    FHashAlgorithm: string;
    FHashSize: integer;
    FHash: TBytes;
    function GetHashesCount: integer;
    function GetHash(Index: Integer): TBytes;
  protected
    procedure Clear; override;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); override;

  public
    property HashAlgorithm: string read FHashAlgorithm;
    property HashesCount: integer read GetHashesCount;
    property Hashes[Index: Integer]: TBytes read GetHash;
    property HashSize: Integer read FHashSize;
    property HashData: TBytes read FHash;
  end;

  TScSpaceAvailableReplyExtension = class(TScSFTPCustomExtension)
  private
    FBytesOnDevice: Int64;
    FUnusedBytesOnDevice: Int64;
    FBytesAvailableToUser: Int64;
    FUnusedBytesAvailableToUser: Int64;
    FBytesPerAllocationUnit: Integer;
  protected
    procedure Clear; override;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); override;

  public
    property BytesOnDevice: Int64 read FBytesOnDevice;
    property UnusedBytesOnDevice: Int64 read FUnusedBytesOnDevice;
    property BytesAvailableToUser: Int64 read FBytesAvailableToUser;
    property UnusedBytesAvailableToUser: Int64 read FUnusedBytesAvailableToUser;
    property BytesPerAllocationUnit: Integer read FBytesPerAllocationUnit;
  end;

  TScSFTPSupportedExtension = class(TScSFTPCustomExtension)
  private
    FSupportedAttributeMask: integer;
    FSupportedAttributeBits: integer;
    FSupportedOpenFlags: integer;
    FSupportedAccessMask: integer;
    FMaxReadSize: integer;
    FSupportedOpenBlockVector: word;
    FSupportedBlockVector: word;
    FAttribExtensionNames: TStringList;
    FExtensionNames: TStringList;
    FRaiseError: boolean;

    function GetSupportedAttributes: TScSFTPAttributes;
    function GetSupportedAttributeBits: TScSFTPFileAttrs;
    function GetSupportedOpenFlags: TScSFTPFileOpenFlags;
    function GetSupportedBlockModes: TScSFTPBlockModes;
    function GetSupportedAccessMask: TScSFTPAceMask;
    function InternalSupportedBlockSet(const BlockModes: TScSFTPBlockModes;
      SupportedBlockVector: integer): boolean;
    procedure CheckSupportedFeature(const Mask: integer; var Value: integer);

  protected
    procedure Clear; override;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); override;

    procedure CheckSupportedAttributes(var Value: integer);
    procedure CheckSupportedAttributeBits(var Value: integer);
    procedure CheckSupportedOpenFlags(var Value: integer);
    procedure CheckSupportedAccessMask(var Value: integer);
    procedure CheckSupportedOpenBlockSet(const BlockModes: TScSFTPBlockModes);
    procedure CheckSupportedBlockSet(const BlockModes: TScSFTPBlockModes);
    procedure CheckAttribExtensionName(const Name: string);
    procedure CheckExtensionName(const Name: string);

  public
    constructor Create;
    destructor Destroy; override;

    function IsSupportedOpenBlockSet(const BlockModes: TScSFTPBlockModes): boolean;
    function IsSupportedBlockSet(const BlockModes: TScSFTPBlockModes): boolean;
    function IsOpenBlockSetAvailable: boolean;
    function IsBlockSetAvailable: boolean;

    property MaxReadSize: integer read FMaxReadSize;
    property SupportedAttributes: TScSFTPAttributes read GetSupportedAttributes;
    property SupportedAttributeBits: TScSFTPFileAttrs read GetSupportedAttributeBits;
    property SupportedOpenFlags: TScSFTPFileOpenFlags read GetSupportedOpenFlags;
    property SupportedBlockModes: TScSFTPBlockModes read GetSupportedBlockModes;
    property SupportedAccessMask: TScSFTPAceMask read GetSupportedAccessMask;
    property SupportedAttribExtensionNames: TStringList read FAttribExtensionNames;
    property SupportedExtensionNames: TStringList read FExtensionNames;
    property RaiseError: boolean read FRaiseError write FRaiseError default True;
  end;

  TScSFTPSupportedAclExtension = class(TScSFTPCustomExtension)
  private
    FSupportedAcls: TScSFTPSupportedAcls;
  protected
    procedure Clear; override;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); override;

  public
    property SupportedAcls: TScSFTPSupportedAcls read FSupportedAcls;
  end;

  TScSFTPVendorExtension = class(TScSFTPCustomExtension)
  private
    FProductBuildNumber: Int64;
    FProductName: string;
    FProductVersion: string;
    FVendorName: string;
  protected
    procedure Clear; override;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); override;

  public
    property ProductBuildNumber: Int64 read FProductBuildNumber;
    property ProductName: string read FProductName;
    property ProductVersion: string read FProductVersion;
    property VendorName: string read FVendorName;
  end;

  TScSFTPVersionsExtension = class(TScSFTPCustomExtension)
  private
    FVersions: TScSFTPVersions;
    FStrVersions: string;
  protected
    procedure Clear; override;
    procedure ParseExtension(const ExtName: string; const ExtData: TBytes); override;

  public
    property AsString: string read FStrVersions;
    property Versions: TScSFTPVersions read FVersions;
  end;

{$IFDEF UNIX}
  _stat = stat;
{$ENDIF}

  TScSFTPFileAttributes = class(TPersistent)
  private
    FFileType: TScSFTPFileType;
    FSize: Int64;
    FAllocationSize: Int64;
    FAclFlags: TScSFTPAclFlags;
    FACEs: TScSFTPACEs;
    FAttrs: TScSFTPFileAttrs;
    FGID: integer;
    FUID: integer;
    FGroup: string;
    FOwner: string;
    FAccessTime: TDateTime;
    FCreateTime: TDateTime;
    FModifyTime: TDateTime;
    FChangeAttrTime: TDateTime;
    FPermissions: TScSFTPFilePermissions;
    FTextHint: TScSFTPTextHint;
    FMimeType: string;
    FLinkCount: integer;
    FUntranslatedName: string;
    FExtendedAttributes: TCRObjectList;
    FValidAttributes: TScSFTPAttributes;
  {$IFDEF MSWINDOWS}
    FOSid: PSID;
    FGSid: PSID;
  {$ENDIF}
    FRequestFileSecurityAttributes: TScRequestFileSecurityAttributes;

    procedure SetSize(Value: Int64);
    procedure SetAllocationSize(Value: Int64);
    procedure SetAclFlags(const Value: TScSFTPAclFlags);
    procedure SetACEs(Value: TScSFTPACEs);
    procedure SetAttrs(const Value: TScSFTPFileAttrs);
    procedure SetGID(Value: integer);
    procedure SetUID(Value: integer);
    procedure SetGroup(const Value: string);
    procedure SetOwner(const Value: string);
    procedure SetAccessTime(Value: TDateTime);
    procedure SetCreateTime(Value: TDateTime);
    procedure SetModifyTime(Value: TDateTime);
    procedure SetChangeAttrTime(Value: TDateTime);
    procedure SetPermissions(const Value: TScSFTPFilePermissions);
    procedure SetTextHint(Value: TScSFTPTextHint);
    procedure SetMimeType(const Value: string);
    procedure SetLinkCount(Value: integer);
    procedure SetUntranslatedName(const Value: string);
    procedure SetExtendedAttributes(Value: TCRObjectList);
    procedure OnChanged(Sender: TObject);

    procedure ParseTime(const DateTime: TDateTime; out Second: Int64; out NSec: Integer);
    function Int64ToDateTime(const Second: Int64; const NSec: Integer): TDateTime;
    function ConvertACLToSFTPBuffer(Version: integer; SupportedExtension: TScSFTPSupportedExtension): TBytes;
    procedure ConvertSFTPBufferToACL(const Value: TBytes; Version: integer);
  {$IFDEF MSWINDOWS}
    function DirsCount(const Path: string): integer;
    procedure RequestSecurityInfo(const Path: string; SecDescr: Pointer; const ReqAttrs: TScSFTPAttributes);
    function SetSecurityInfo(const Handle: THandle): boolean;
  {$ELSE}
    procedure RequestSysInfo(StatBuf: _stat; const ReqAttrs: TScSFTPAttributes);
  {$ENDIF}

  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure Clear;
    class procedure WriteToPacketDefaultValues(SftpPacket: TSFTPPacketWriter;
      const FileType: TScSFTPFileType; Version: integer);
    procedure WriteToPacket(SftpPacket: TSFTPPacketWriter; Version: integer; SupportedExtension: TScSFTPSupportedExtension);
    procedure ReadFromPacket(SftpPacket: TSFTPPacketReader; Version: integer);

    procedure RequestInfo(const Path: string; const FileRec: TSearchRec;
      const ReqAttrs: TScSFTPAttributes; const FollowSymLink: boolean);
    procedure RequestInfoByHandle(const Handle: THandle; const ReqAttrs: TScSFTPAttributes);
    function SetToFile(const Path: string): boolean;
    function SetToFileByHandle(const Path: string; const Handle: THandle): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function GetAttributesAsLongname: string;

    property FileType: TScSFTPFileType read FFileType write FFileType;
    property Size: Int64 read FSize write SetSize;
    property AllocationSize: Int64 read FAllocationSize write SetAllocationSize;
    property AclFlags: TScSFTPAclFlags read FAclFlags write SetAclFlags default [aclControlPresent];
    property ACEs: TScSFTPACEs read FACEs write SetACEs;
    property Attrs: TScSFTPFileAttrs read FAttrs write SetAttrs;
    property GID: integer read FGID write SetGID;
    property UID: integer read FUID write SetUID;
    property Group: string read FGroup write SetGroup;
    property Owner: string read FOwner write SetOwner;
    property AccessTime: TDateTime read FAccessTime write SetAccessTime;
    property CreateTime: TDateTime read FCreateTime write SetCreateTime;
    property ModifyTime: TDateTime read FModifyTime write SetModifyTime;
    property ChangeAttrTime: TDateTime read FChangeAttrTime write SetChangeAttrTime;
    property Permissions: TScSFTPFilePermissions read FPermissions write SetPermissions;
    property TextHint: TScSFTPTextHint read FTextHint write SetTextHint;
    property MimeType: string read FMimeType write SetMimeType;
    property LinkCount: integer read FLinkCount write SetLinkCount;
    property UntranslatedName: string read FUntranslatedName write SetUntranslatedName;
    property ExtendedAttributes: TCRObjectList read FExtendedAttributes write SetExtendedAttributes;
    property ValidAttributes: TScSFTPAttributes read FValidAttributes write FValidAttributes;

  {$IFDEF MSWINDOWS}
    property OSid: PSID read FOSid;
    property GSid: PSID read FGSid;
  {$ENDIF}
  end;

  TScSFTPFileInfo = class(TPersistent)
  private
    FFilename: string;
    FLongname: string;
    FAttributes: TScSFTPFileAttributes;
    function GetLongname: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure FillInfo(const Path: string; const FileRec: TSearchRec);

    property Filename: string read FFilename write FFilename;
    property Longname: string read GetLongname write FLongname;
    property Attributes: TScSFTPFileAttributes read FAttributes;
  end;

  EScSFTPError = class(EScError)
  protected
    FSFTPErrorCode: integer;
  public
    constructor Create(SFTPErrorCode: integer; const ErrorMessage: string);
    property SFTPErrorCode: integer read FSFTPErrorCode;
  end;

  EScSFTPHandledError = class(EScSFTPError);
  {$EXTERNALSYM EScSFTPHandledError}

  TScSFTPUtils = class
  public
    class function DecodeBytes(const Data: TBytes; const UseUnicode: boolean): string;
    class function EncodeString(const Str: string; const UseUnicode: boolean): TBytes;
  end;

  TScSFTPExtensionUtils = class
  public
    class procedure Clear(Obj: TScSFTPCustomExtension);
    class procedure ParseExtension(Obj: TScSFTPCustomExtension;
      const ExtName: string; const ExtData: TBytes);
  end;

  TScSFTPSupportedExtensionUtils = class
  public
    class procedure CheckSupportedOpenFlags(Obj: TScSFTPSupportedExtension;
      var Value: integer);
    class procedure CheckExtensionName(Obj: TScSFTPSupportedExtension;
      const Name: string);
    class procedure CheckSupportedOpenBlockSet(Obj: TScSFTPSupportedExtension;
      const BlockModes: TScSFTPBlockModes);
    class procedure CheckSupportedBlockSet(Obj: TScSFTPSupportedExtension;
      const BlockModes: TScSFTPBlockModes);
  end;

  TScSFTPFileAttributesUtils = class
  public
    class procedure WriteToPacketDefaultValues(SftpPacket: TSFTPPacketWriter;
      const FileType: TScSFTPFileType; Version: integer);
    class procedure WriteToPacket(Obj: TScSFTPFileAttributes;
      SftpPacket: TSFTPPacketWriter; Version: integer; SupportedExtension: TScSFTPSupportedExtension);
    class procedure ReadFromPacket(Obj: TScSFTPFileAttributes;
      SftpPacket: TSFTPPacketReader; Version: integer);

    class procedure RequestInfo(Obj: TScSFTPFileAttributes; const Path: string;
      const FileRec: TSearchRec; const ReqAttrs: TScSFTPAttributes; const FollowSymLink: boolean);
    class procedure RequestInfoByHandle(Obj: TScSFTPFileAttributes;
      const Handle: THandle; const ReqAttrs: TScSFTPAttributes);
    class function SetToFile(Obj: TScSFTPFileAttributes; const Path: string): boolean;
    class function SetToFileByHandle(Obj: TScSFTPFileAttributes;
      const Path: string; const Handle: THandle): boolean;

    class procedure SetRequestFileSecurityAttributes(Obj: TScSFTPFileAttributes; RequestFileSecurityAttributes: TScRequestFileSecurityAttributes);
  end;

procedure InitError(var Error: TScSFTPError; const Code: TScSFTPErrorCode; const Message: string = '');

procedure ConvertOpenModesVer3ToVer5(const OpenModes: TScSFTPFileOpenModes;
  out Mode: TScSFTPFileOpenMode; out Flags: TScSFTPFileOpenFlags;
  out BlockModes: TScSFTPBlockModes; out DesiredAccess: TScSFTPDesiredAccess);
procedure ConvertOpenModesVer5ToVer3(const Mode: TScSFTPFileOpenMode; const Flags: TScSFTPFileOpenFlags;
  const Access: TScSFTPDesiredAccess; out Modes: TScSFTPFileOpenModes);
function ConvertSFTPAttributesToSFTPValue(const Attributes: TScSFTPAttributes; ProtocolVersion: integer): integer;
function ConvertSFTPValueToSFTPAttributes(Value: integer): TScSFTPAttributes;
function ConvertFileOpenModesToSFTPValue(const FileOpenModes: TScSFTPFileOpenModes; ProtocolVersion: integer): integer;
function ConvertSFTPValueToFileOpenModes(Value: integer): TScSFTPFileOpenModes;
function ConvertFileOpenModeToSFTPValue(const FileOpenMode: TScSFTPFileOpenMode): integer;
function ConvertSFTPValueToFileOpenMode(Value: integer): TScSFTPFileOpenMode;
function ConvertBlockModesToSFTPValue(const BlockModes: TScSFTPBlockModes): integer;
function ConvertSFTPValueToBlockModes(Value: integer): TScSFTPBlockModes;
function ConvertFileOpenFlagsToSFTPValue(const FileOpenFlags: TScSFTPFileOpenFlags): integer;
function ConvertSFTPValueToFileOpenFlags(Value: integer): TScSFTPFileOpenFlags;
function ConvertSupportedAclsToSFTPValue(const SupportedAcls: TScSFTPSupportedAcls): integer;
function ConvertSFTPValueToSupportedAcls(Value: integer): TScSFTPSupportedAcls;
function ConvertFileTypeToSFTPValue(const FileType: TScSFTPFileType; ProtocolVersion: integer): byte;
function ConvertSFTPValueToFileType(Value: byte): TScSFTPFileType;
function ConvertFilePermissionsToSFTPValue(const FilePermissions: TScSFTPFilePermissions): integer;
function ConvertSFTPValueToFilePermissions(Value: integer): TScSFTPFilePermissions;
function ConvertAclFlagsToSFTPValue(const AclFlags: TScSFTPAclFlags): integer;
function ConvertSFTPValueToAclFlags(Value: integer): TScSFTPAclFlags;
function ConvertAceTypeToSFTPValue(const AceType: TScSFTPAceType): integer;
function ConvertSFTPValueToAceType(Value: integer): TScSFTPAceType;
function ConvertAceFlagsToSFTPValue(const AceFlags: TScSFTPAceFlags): integer;
function ConvertSFTPValueToAceFlags(Value: integer): TScSFTPAceFlags;
function ConvertAceMaskToSFTPValue(const AceMask: TScSFTPAceMask): integer;
function ConvertSFTPValueToAceMask(Value: integer): TScSFTPAceMask;
function ConvertFileAttrsToSFTPValue(const FileAttrs: TScSFTPFileAttrs; ProtocolVersion: integer): integer;
function ConvertSFTPValueToFileAttrs(Value: integer): TScSFTPFileAttrs;
function ConvertTextHintToSFTPValue(const TextHint: TScSFTPTextHint): byte;
function ConvertSFTPValueToTextHint(Value: byte): TScSFTPTextHint;
function ConvertRenameFlagsToSFTPValue(const RenameFlags: TScSFTPRenameFlags): integer;
function ConvertSFTPValueToRenameFlags(Value: integer): TScSFTPRenameFlags;
function ConvertRealpathControlToSFTPValue(const RealpathControl: TScSFTPRealpathControl): byte;
function ConvertSFTPValueToRealpathControl(Value: byte): TScSFTPRealpathControl;

{$IFDEF UNIX}
function FileCreateSymLink(const Link, Target: string): Boolean;
function FileGetSymLinkTarget(const FileName: string; var TargetName: string): Boolean;
{$ENDIF}

{$IFDEF MSWINDOWS}

{$MINENUMSIZE 4}
type
  MULTIPLE_TRUSTEE_OPERATION = (NO_MULTIPLE_TRUSTEE, TRUSTEE_IS_IMPERSONATE);
  {$EXTERNALSYM MULTIPLE_TRUSTEE_OPERATION}

  TRUSTEE_FORM = (TRUSTEE_IS_SID, TRUSTEE_IS_NAME, TRUSTEE_BAD_FORM,
    TRUSTEE_IS_OBJECTS_AND_SID, TRUSTEE_IS_OBJECTS_AND_NAME);
  {$EXTERNALSYM TRUSTEE_FORM}

  TRUSTEE_TYPE = (TRUSTEE_IS_UNKNOWN, TRUSTEE_IS_USER, TRUSTEE_IS_GROUP,
    TRUSTEE_IS_DOMAIN, TRUSTEE_IS_ALIAS, TRUSTEE_IS_WELL_KNOWN_GROUP,
    TRUSTEE_IS_DELETED, TRUSTEE_IS_INVALID, TRUSTEE_IS_COMPUTER);
  {$EXTERNALSYM TRUSTEE_TYPE}

  PTRUSTEE = ^TRUSTEE;
  {$EXTERNALSYM PTRUSTEE}
  TRUSTEE = record
    pMultipleTrustee: PTRUSTEE;
    MultipleTrusteeOperation: MULTIPLE_TRUSTEE_OPERATION;
    TrusteeForm: TRUSTEE_FORM;
    TrusteeType: TRUSTEE_TYPE;
    ptstrName: PAnsiChar;
  end;
  {$EXTERNALSYM TRUSTEE}

  PACL_SIZE_INFORMATION = ^ACL_SIZE_INFORMATION;
  {$EXTERNALSYM PACL_SIZE_INFORMATION}
  ACL_SIZE_INFORMATION = record
    AceCount: DWORD;
    AclBytesInUse: DWORD;
    AclBytesFree: DWORD;
  end;
  {$EXTERNALSYM ACL_SIZE_INFORMATION}

  ACE_HEADER = record
    AceType: byte;
    AceFlags: byte;
    AceSize: shortint;
  end;
  {$EXTERNALSYM ACE_HEADER}

  PACCESS_ACE = ^ACCESS_ALLOWED_ACE;
  {$EXTERNALSYM PACCESS_ACE}
  ACCESS_ALLOWED_ACE = record
    Header: ACE_HEADER;
    Mask: integer;
    SidStart: PSID;
  end;
  {$EXTERNALSYM ACCESS_ALLOWED_ACE}

const
  ACETYPE_ACCESS_ALLOWED          = 0;
  {$EXTERNALSYM ACETYPE_ACCESS_ALLOWED}
  ACETYPE_ACCESS_DENIED           = 1;
  {$EXTERNALSYM ACETYPE_ACCESS_DENIED}
  ACETYPE_SYSTEM_AUDIT            = 2;
  {$EXTERNALSYM ACETYPE_SYSTEM_AUDIT}
  ACETYPE_SYSTEM_ALARM            = 3;
  {$EXTERNALSYM ACETYPE_SYSTEM_ALARM}
  ACETYPE_ACCESS_ALLOWED_COMPOUND = 4;
  {$EXTERNALSYM ACETYPE_ACCESS_ALLOWED_COMPOUND}
  ACETYPE_ACCESS_ALLOWED_OBJECT   = 5;
  {$EXTERNALSYM ACETYPE_ACCESS_ALLOWED_OBJECT}
  ACETYPE_ACCESS_DENIED_OBJECT    = 6;
  {$EXTERNALSYM ACETYPE_ACCESS_DENIED_OBJECT}
  ACETYPE_SYSTEM_AUDIT_OBJECT     = 7;
  {$EXTERNALSYM ACETYPE_SYSTEM_AUDIT_OBJECT}
  ACETYPE_SYSTEM_ALARM_OBJECT     = 8;
  {$EXTERNALSYM ACETYPE_SYSTEM_ALARM_OBJECT}

  ACEFLAG_OBJECT_INHERIT_ACE         = $01;
  {$EXTERNALSYM ACEFLAG_OBJECT_INHERIT_ACE}
  ACEFLAG_CONTAINER_INHERIT_ACE      = $02;
  {$EXTERNALSYM ACEFLAG_CONTAINER_INHERIT_ACE}
  ACEFLAG_NO_PROPAGATE_INHERIT_ACE   = $04;
  {$EXTERNALSYM ACEFLAG_NO_PROPAGATE_INHERIT_ACE}
  ACEFLAG_INHERIT_ONLY_ACE           = $08;
  {$EXTERNALSYM ACEFLAG_INHERIT_ONLY_ACE}
  ACEFLAG_INHERITED_ACE              = $10;
  {$EXTERNALSYM ACEFLAG_INHERITED_ACE}
  ACEFLAG_SUCCESSFUL_ACCESS_ACE_FLAG = $40;
  {$EXTERNALSYM ACEFLAG_SUCCESSFUL_ACCESS_ACE_FLAG}
  ACEFLAG_FAILED_ACCESS_ACE_FLAG     = $80;
  {$EXTERNALSYM ACEFLAG_FAILED_ACCESS_ACE_FLAG}

  ACL_REVISION = 2;
  {$EXTERNALSYM ACL_REVISION}

  SE_FILE_OBJECT = 1;
  {$EXTERNALSYM SE_FILE_OBJECT}
  REQ_SEC_INFO = OWNER_SECURITY_INFORMATION + GROUP_SECURITY_INFORMATION + DACL_SECURITY_INFORMATION;
  {$EXTERNALSYM REQ_SEC_INFO}

  FILE_READ_DATA          = $0001;
  {$EXTERNALSYM FILE_READ_DATA}
  FILE_WRITE_DATA         = $0002;
  {$EXTERNALSYM FILE_WRITE_DATA}
  FILE_APPEND_DATA        = $0004;
  {$EXTERNALSYM FILE_APPEND_DATA}
  FILE_READ_PROPERTIES    = $0008;
  {$EXTERNALSYM FILE_READ_PROPERTIES}
  FILE_WRITE_PROPERTIES   = $0010;
  {$EXTERNALSYM FILE_WRITE_PROPERTIES}
  FILE_EXECUTE            = $0020;
  {$EXTERNALSYM FILE_EXECUTE}
  FILE_TRAVERSE           = $0020;
  {$EXTERNALSYM FILE_TRAVERSE}
  FILE_DELETE_CHILD       = $0040;
  {$EXTERNALSYM FILE_DELETE_CHILD}
  FILE_READ_ATTRIBUTES    = $0080;
  {$EXTERNALSYM FILE_READ_ATTRIBUTES}
  FILE_WRITE_ATTRIBUTES   = $0100;
  {$EXTERNALSYM FILE_WRITE_ATTRIBUTES}
  FILE_GENERIC_READ       = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_PROPERTIES or SYNCHRONIZE;
  {$EXTERNALSYM FILE_GENERIC_READ}
  FILE_GENERIC_WRITE      = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_PROPERTIES or FILE_APPEND_DATA or SYNCHRONIZE;
  {$EXTERNALSYM FILE_GENERIC_WRITE}
  FILE_GENERIC_EXECUTE    = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or SYNCHRONIZE;
  {$EXTERNALSYM FILE_GENERIC_EXECUTE}
  FILE_ALL_ACCESS         = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  {$EXTERNALSYM FILE_ALL_ACCESS}
{$IFDEF FPC}
  GENERIC_EXECUTE         = $20000000;
{$ENDIF}

  WinAceTypeValues: array[TScSFTPAceType] of integer = (
    ACETYPE_ACCESS_ALLOWED, ACETYPE_ACCESS_DENIED,
    ACETYPE_SYSTEM_AUDIT, ACETYPE_SYSTEM_ALARM);

  WinAceFlagValues: array[TScSFTPAceFlag] of integer = (
    ACEFLAG_OBJECT_INHERIT_ACE, ACEFLAG_CONTAINER_INHERIT_ACE,
    ACEFLAG_NO_PROPAGATE_INHERIT_ACE, ACEFLAG_INHERIT_ONLY_ACE,
    ACEFLAG_SUCCESSFUL_ACCESS_ACE_FLAG, ACEFLAG_FAILED_ACCESS_ACE_FLAG,
    ACEFLAG_INHERITED_ACE);

const
  secur32 = 'secur32.dll';

function PathCanonicalize(pszBuf: PChar; pszPath: PChar): BOOL; stdcall; external 'shlwapi.dll' name {$IFDEF IS_UNICODE}'PathCanonicalizeW'{$ELSE}'PathCanonicalizeA'{$ENDIF};
{$EXTERNALSYM PathCanonicalize}

type
  TBuildTrusteeWithSid = procedure (var _Trustee: TRUSTEE; pSidIn: PSID); stdcall;
  {$EXTERNALSYM TBuildTrusteeWithSid}
  TGetEffectiveRightsFromAcl = function (_pacl: PACL; const _Trustee: TRUSTEE; var pAccessRights: ACCESS_MASK): DWORD; stdcall;
  {$EXTERNALSYM TGetEffectiveRightsFromAcl}
  TConvertSidToStringSid = function (Sid: PSID; var StringSid: Pointer): BOOL; stdcall;
  {$EXTERNALSYM TConvertSidToStringSid}
  TGetUserNameEx = function (NameFormat: integer; lpNameBuffer: PChar; var nSize: DWORD): BOOL; stdcall;
  {$EXTERNALSYM TGetUserNameEx}
  TGetSecurityInfo = function (handle: THandle; ObjectType: DWORD; SecurityInfo: SECURITY_INFORMATION;
    ppsidOwner: PSID; ppsidGroup: PSID; ppDacl: PACL; ppSacl: PACL;
    var pSecurityDescriptor: PSecurityDescriptor): DWORD; stdcall;
  {$EXTERNALSYM TGetSecurityInfo}
  TWinSetSecurityInfo = function (handle: THandle; ObjectType: DWORD; SecurityInfo: SECURITY_INFORMATION;
    psidOwner: PSID; psidGroup: PSID; pDacl: PACL; pSacl: PACL): DWORD; stdcall;
  {$EXTERNALSYM TWinSetSecurityInfo}
{$IFDEF FPC}
  TAddAccessAllowedAceEx = function (var pAcl: TACL; dwAceRevision: DWORD;
    AceFlags: DWORD; AccessMask: DWORD; pSid: PSID): BOOL; stdcall;
  TAddAccessDeniedAceEx = function (var pAcl: TACL; dwAceRevision: DWORD;
    ACEFlags: DWORD; AccessMask: DWORD; pSid: PSID): BOOL; stdcall;
  TAddAuditAccessAceEx = function (var pAcl: TACL; dwAceRevision: DWORD;
    AceFlags: DWORD; dwAccessMask: DWORD; pSid: Pointer; bAuditSuccess, bAuditFailure: BOOL): BOOL; stdcall;
{$ENDIF}

function ConvertWinValueToAceType(Value: integer): TScSFTPAceType;
function ConvertWinValueToAceFlags(Value: integer): TScSFTPAceFlags;

{$ENDIF}

implementation

uses
{$IFDEF POSIX}
  System.Types, Posix.Pwd, Posix.Grp, Posix.SysTime, Posix.SysTypes,
  Posix.Time, Posix.Unistd, Posix.Utime, Posix.Fcntl,
{$ENDIF}
{$IFDEF DARWIN}
  MacTypes, CFBase, CFString,
{$ENDIF}
{$IFDEF UNIX}
  pwd, grp,
{$ENDIF}
  ScConsts, ScHash, ScFunctions;

{$IFDEF VER7P}
var
  SFTPFormatSettings: TFormatSettings;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  BuildTrusteeWithSid: TBuildTrusteeWithSid;
  GetEffectiveRightsFromAcl: TGetEffectiveRightsFromAcl;
  ConvertSidToStringSid: TConvertSidToStringSid;
  GetUserNameEx: TGetUserNameEx;
  GetSecurityInfo: TGetSecurityInfo;
  WinSetSecurityInfo: TWinSetSecurityInfo;
{$IFDEF FPC}
  AddAccessAllowedAceEx: TAddAccessAllowedAceEx;
  AddAccessDeniedAceEx: TAddAccessDeniedAceEx;
  AddAuditAccessAceEx: TAddAuditAccessAceEx;
{$ENDIF}
{$ENDIF}

const
  AttributeValues: array[TScSFTPAttribute] of integer = (
    SSH_FILEXFER_ATTR_SIZE, SSH_FILEXFER_ATTR_ALLOCATION_SIZE,
    SSH_FILEXFER_ATTR_OWNERGROUP, SSH_FILEXFER_ATTR_PERMISSIONS,
    SSH_FILEXFER_ATTR_ACCESSTIME, SSH_FILEXFER_ATTR_CREATETIME,
    SSH_FILEXFER_ATTR_MODIFYTIME, SSH_FILEXFER_ATTR_CTIME,
    SSH_FILEXFER_ATTR_SUBSECOND_TIMES, SSH_FILEXFER_ATTR_ACL,
    SSH_FILEXFER_ATTR_BITS, SSH_FILEXFER_ATTR_TEXT_HINT,
    SSH_FILEXFER_ATTR_MIME_TYPE, SSH_FILEXFER_ATTR_LINK_COUNT,
    SSH_FILEXFER_ATTR_UNTRANSLATED_NAME, Integer(SSH_FILEXFER_ATTR_EXTENDED));

  FileOpenModeV3Values: array[TScSFTPFileOpenModeItem] of integer = (
    SSH_FXF_READ, SSH_FXF_WRITE, SSH_FXF_APPEND, SSH_FXF_CREAT, SSH_FXF_TRUNC,
    SSH_FXF_EXCL, SSH_FXF_TEXT);

  FileOpenModeV5Values: array[TScSFTPFileOpenMode] of integer = (
    SSH_FXF_CREATE_NEW, SSH_FXF_CREATE_TRUNCATE, SSH_FXF_OPEN_EXISTING,
    SSH_FXF_OPEN_OR_CREATE, SSH_FXF_TRUNCATE_EXISTING);

  FileOpenFlagValues: array[TScSFTPFileOpenFlag] of integer = (
    SSH_FXF_APPEND_DATA, SSH_FXF_APPEND_DATA_ATOMIC, SSH_FXF_TEXT_MODE,
    SSH_FXF_NOFOLLOW, SSH_FXF_DELETE_ON_CLOSE, SSH_FXF_ACCESS_AUDIT_ALARM_INFO,
    SSH_FXF_ACCESS_BACKUP, SSH_FXF_BACKUP_STREAM, SSH_FXF_OVERRIDE_OWNER);

  BlockModeValues: array[TScSFTPBlockMode] of integer = (
    SSH_FXF_BLOCK_READ, SSH_FXF_BLOCK_WRITE, SSH_FXF_BLOCK_DELETE,
    SSH_FXF_BLOCK_ADVISORY);

  SupportedAclValues: array[TScSFTPSupportedAcl] of integer = (
    SSH_ACL_CAP_ALLOW, SSH_ACL_CAP_DENY, SSH_ACL_CAP_AUDIT, SSH_ACL_CAP_ALARM,
    SSH_ACL_CAP_INHERIT_ACCESS, SSH_ACL_CAP_INHERIT_AUDIT_ALARM);

  FileTypeValues: array[TScSFTPFileType] of byte = (
    SSH_FILEXFER_TYPE_REGULAR, SSH_FILEXFER_TYPE_DIRECTORY,
    SSH_FILEXFER_TYPE_SYMLINK, SSH_FILEXFER_TYPE_SPECIAL,
    SSH_FILEXFER_TYPE_UNKNOWN, SSH_FILEXFER_TYPE_SOCKET,
    SSH_FILEXFER_TYPE_CHAR_DEVICE, SSH_FILEXFER_TYPE_BLOCK_DEVICE,
    SSH_FILEXFER_TYPE_FIFO);

  FilePermissionValues: array[TScSFTPFilePermission] of integer = (
    S_IRUSR, S_IWUSR, S_IXUSR, S_IRGRP, S_IWGRP, S_IXGRP,
    S_IROTH, S_IWOTH, S_IXOTH, S_ISUID, S_ISGID, S_ISVTX);

  SFTPAclFlagValues: array[TScSFTPAclFlag] of integer = (
    SFX_ACL_CONTROL_INCLUDED, SFX_ACL_CONTROL_PRESENT, SFX_ACL_CONTROL_INHERITED,
    SFX_ACL_AUDIT_ALARM_INCLUDED, SFX_ACL_AUDIT_ALARM_INHERITED);

  SFTPAceTypeValues: array[TScSFTPAceType] of integer = (
    ACE4_ACCESS_ALLOWED_ACE_TYPE, ACE4_ACCESS_DENIED_ACE_TYPE,
    ACE4_SYSTEM_AUDIT_ACE_TYPE, ACE4_SYSTEM_ALARM_ACE_TYPE);

  SFTPAceFlagValues: array[TScSFTPAceFlag] of integer = (
    ACE4_FILE_INHERIT_ACE, ACE4_DIRECTORY_INHERIT_ACE,
    ACE4_NO_PROPAGATE_INHERIT_ACE, ACE4_INHERIT_ONLY_ACE,
    ACE4_SUCCESSFUL_ACCESS_ACE_FLAG, ACE4_FAILED_ACCESS_ACE_FLAG,
    ACE4_IDENTIFIER_GROUP);

  SFTPAceMaskValues: array[TScSFTPAceMaskItem] of integer = (
    ACE4_READ_DATA, ACE4_LIST_DIRECTORY, ACE4_WRITE_DATA, ACE4_ADD_FILE,
    ACE4_APPEND_DATA, ACE4_ADD_SUBDIRECTORY, ACE4_READ_NAMED_ATTRS,
    ACE4_WRITE_NAMED_ATTRS, ACE4_EXECUTE, ACE4_DELETE_CHILD,
    ACE4_READ_ATTRIBUTES, ACE4_WRITE_ATTRIBUTES, ACE4_DELETE, ACE4_READ_ACL,
    ACE4_WRITE_ACL, ACE4_WRITE_OWNER, ACE4_SYNCHRONIZE);

  FileAttrValues: array[TScSFTPFileAttr] of integer = (
    SSH_FILEXFER_ATTR_FLAGS_READONLY, SSH_FILEXFER_ATTR_FLAGS_SYSTEM,
    SSH_FILEXFER_ATTR_FLAGS_HIDDEN, SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE,
    SSH_FILEXFER_ATTR_FLAGS_ARCHIVE, SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED,
    SSH_FILEXFER_ATTR_FLAGS_COMPRESSED, SSH_FILEXFER_ATTR_FLAGS_SPARSE,
    SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY, SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE,
    SSH_FILEXFER_ATTR_FLAGS_SYNC, SSH_FILEXFER_ATTR_FLAGS_TRANSLATION_ERR);

  TextHintValues: array[TScSFTPTextHint] of byte = (
    SSH_FILEXFER_ATTR_KNOWN_TEXT, SSH_FILEXFER_ATTR_GUESSED_TEXT,
    SSH_FILEXFER_ATTR_KNOWN_BINARY, SSH_FILEXFER_ATTR_GUESSED_BINARY);

  RenameFlagValues: array[TScSFTPRenameFlag] of integer = (
    SSH_FXF_RENAME_OVERWRITE, SSH_FXF_RENAME_ATOMIC, SSH_FXF_RENAME_NATIVE);

  RealpathControlValues: array[TScSFTPRealpathControl] of byte = (
    SSH_FXP_REALPATH_NO_CHECK, SSH_FXP_REALPATH_STAT_IF, SSH_FXP_REALPATH_STAT_ALWAYS);

  _faSparse     = $0200;
  _faCompressed = $0800;
  _faEncrypted  = $4000;

  OSFileAttrValues: array[TScSFTPFileAttr] of cardinal = (
    SysUtils.faReadonly, {$IFDEF FPC}4{$ELSE}SysUtils.faSysFile{$ENDIF},
    {$IFDEF FPC}2{$ELSE}SysUtils.faHidden{$ENDIF}, 0,
    SysUtils.faArchive, _faEncrypted, _faCompressed, _faSparse,
    0, 0, 0, 0);

{$IFDEF MSWINDOWS}
const
  faSymLink = $00000400; // Available on POSIX and Vista and above

{$IFNDEF FPC}
  SidTypeUser   = 1;
  SidTypeGroup  = 2;
  SidTypeDomain = 3;
  SidTypeAlias  = 4;
{$ENDIF}
  NameSamCompatible = 2;
  NameUserPrincipal = 8;

var
  hAdvapi32Lib: HMODULE = 0;
  hSecur32Lib: HMODULE = 0;

function NotLink: integer;
begin
  raise Exception.Create('Function is not linked');
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
end;

procedure LoadFuncs;
  function GetProc(hLib: HMODULE; Name: string): FARPROC;
  begin
    if hLib > 0 then
      Result := GetProcAddress(hLib, PChar(Name))
    else
      Result := @NotLink;

    if Result = nil then
      Result := @NotLink;
  end;

begin
  hAdvapi32Lib := LoadLibrary(PChar(advapi32));
  BuildTrusteeWithSid := GetProc(hAdvapi32Lib, 'BuildTrusteeWithSidA');
  GetEffectiveRightsFromAcl := GetProc(hAdvapi32Lib, 'GetEffectiveRightsFromAclA');
  ConvertSidToStringSid := GetProc(hAdvapi32Lib, 'ConvertSidToStringSidA');
  GetSecurityInfo := GetProc(hAdvapi32Lib, 'GetSecurityInfo');
  WinSetSecurityInfo := GetProc(hAdvapi32Lib, 'SetSecurityInfo');
{$IFDEF FPC}
  AddAccessAllowedAceEx := GetProc(hAdvapi32Lib, 'AddAccessAllowedAceEx');
  AddAccessDeniedAceEx := GetProc(hAdvapi32Lib, 'AddAccessDeniedAceEx');
  AddAuditAccessAceEx := GetProc(hAdvapi32Lib, 'AddAuditAccessAceEx');
{$ENDIF}

  hSecur32Lib := LoadLibrary(PChar(secur32));
  GetUserNameEx := GetProc(hSecur32Lib, {$IFDEF IS_UNICODE}'GetUserNameExW'{$ELSE}'GetUserNameExA'{$ENDIF});
end;

procedure FreeFuncs;
begin
  if hAdvapi32Lib <> 0 then begin
    FreeLibrary(hAdvapi32Lib);
    hAdvapi32Lib := 0;
  end;

  if hSecur32Lib <> 0 then begin
    FreeLibrary(hSecur32Lib);
    hSecur32Lib := 0;
  end;
end;
{$ENDIF} //MSWINDOWS

{$IFDEF MSWINDOWS}
function InternalFileTimeToDateTime(Time: TFileTime): TDateTime;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
function InternalFileTimeToDateTime(Time: time_t): TDateTime;
{$ENDIF POSIX}

  function InternalEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond: Word): TDateTime;
  var
    LTime: TDateTime;
    Success: Boolean;
  begin
    Result := 0;
    Success := TryEncodeDate(AYear, AMonth, ADay, Result);
    if Success then
    begin
      Success := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
      if Success then
        if Result >= 0 then
          Result := Result + LTime
        else
          Result := Result - LTime
    end;
  end;

{$IFDEF MSWINDOWS}
var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := 0;
  FileTimeToLocalFileTime(Time, LFileTime);

  if FileTimeToSystemTime(LFileTime, SysTime) then
    Result := InternalEncodeDateTime(SysTime.wYear, SysTime.wMonth, SysTime.wDay,
      SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  LDecTime: tm;
begin
  Result := 0;

  if localtime_r(Time, LDecTime) <> nil then
    Result := InternalEncodeDateTime(LDecTime.tm_year + 1900, LDecTime.tm_mon + 1,
      LDecTime.tm_mday, LDecTime.tm_hour, LDecTime.tm_min, LDecTime.tm_sec, 0);
end;
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
function InternalDateTimeToFileTime(const DateTime: TDateTime; var FileTime: TFileTime): boolean;
var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  DateTimeToSystemTime(DateTime, SysTime);
  Result := SystemTimeToFileTime(SysTime, LFileTime);
  if Result then
    Result := LocalFileTimeToFileTime(LFileTime, FileTime);
end;
{$ELSE}
function InternalDateTimeToFileTime(const DateTime: TDateTime; var FileTime: time_t): boolean;
begin
  FileTime := DateTimeToFileDate(DateTime);
  Result := FileTime <> 0;
end;
{$ENDIF}

{$IFDEF UNIX} // Copy form Delphi XE2
function IsRelativePath(const Path: string): Boolean;
var
  L: Integer;
begin
  L := Length(Path);
  Result := (L > 0) and (Path[1] <> PathDelim)
    {$IFDEF MSWINDOWS}and (L > 1) and (Path[2] <> ':'){$ENDIF MSWINDOWS};
end;

function FileCreateSymLink(const Link, Target: string): Boolean;
begin
  if (Target = '') or (Link = '') then
    Exit(False);

  Result := fpsymlink(PAnsiChar(ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Target))), PAnsiChar(ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Link)))) = 0;
end;

{$IFDEF MACOS}

function TCFString_AsString(Value: CFStringRef): string;
var
  Range: CFRange;
  Tmp: TCharArray;
begin
  if Value = nil then Exit('');
  Range := CFRangeMake(0, CFStringGetLength(Value));
  if Range.Length > 0 then
  begin
    SetLength(Tmp, Range.Length);
    CFStringGetCharacters(Value, Range, UniCharPtr(Tmp));
    Result := string(Tmp);
  end
  else
    Result := EmptyStr;
end;

function FileSystemStringToString(const Source: PAnsiChar; var Dest: string): Boolean;
var
  StringRef: CFStringRef;
  MutableStringRef: CFMutableStringRef;
begin
  Result := False;
  StringRef := CFStringCreateWithFileSystemRepresentation(kCFAllocatorDefault, Source);
  if StringRef = nil then
    Exit;
  try
    MutableStringRef := CFStringCreateMutableCopy(kCFAllocatorDefault, CFStringGetLength(StringRef), StringRef);
  finally
    CFRelease(StringRef);
  end;
  if MutableStringRef = nil then
    Exit;
  try
    CFStringNormalize(MutableStringRef, kCFStringNormalizationFormKC);
    CFStringFold(MutableStringRef, kCFCompareLocalized, nil);
    Dest := TCFString_AsString(CFStringRef(MutableStringRef));
    Result := True;
  finally
    CFRelease(MutableStringRef);
  end;
end;

{$ENDIF}

function FileGetSymLinkTarget(const FileName: string; var TargetName: string): Boolean;
var
  Buffer: PAnsiChar;
  BufferLen, Size: Integer;
  SearchRec: TSearchRec;
  SearchName: string;
  LFileName: AnsiString;
begin
  Result := False;

  if (FileName = '') then
    Exit;

  LFileName := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(FileName));
  BufferLen := MAX_PATH;

  while True do
  begin
    System.GetMem(Buffer, BufferLen * sizeof(AnsiChar) + 1);
    try
      Size := fpreadlink(PAnsiChar(LFileName), Buffer, BufferLen);

      if (Size = -1) or (Size = 0) then
        Exit
      else if Size = BufferLen then
        Inc(BufferLen, BufferLen)
      else if Size < BufferLen then
      begin
        Buffer[Size] := #0;
      {$IFDEF LINUX}
        TargetName := Buffer;
      {$ENDIF}
      {$IFDEF LINUX_BSD}
        TargetName := Buffer;
      {$ENDIF}
      {$IFDEF MACOS}
        if not FileSystemStringToString(Buffer, TargetName) then
          Exit;
      {$ENDIF}

        if IsRelativePath(TargetName) and ((Length(TargetName) > 0) and (TargetName[1] <> '~')) then
          SearchName := IncludeTrailingBackslash(ExtractFilePath(FileName)) + TargetName
        else
          SearchName := TargetName;

        if FindFirst(SearchName, faAnyFile, SearchRec) = 0 then
        begin
        //  SymLinkRec.TargetName := TargetName;
        //  SymLinkRec.Attr := SearchRec.Attr;
        //  SymLinkRec.Size := SearchRec.Size;
        //  SymLinkRec.PathOnly := SearchRec.PathOnly;
        //  SymLinkRec.Mode := SearchRec.Mode;
        //  SymLinkRec.Time := SearchRec.Time;
          FindClose(SearchRec);
          Result := True;
        end;

        Break;
      end;
    finally
      System.FreeMem(Buffer);
    end;
  end;
end;
{$ENDIF}

procedure ConvertOpenModesVer3ToVer5(const OpenModes: TScSFTPFileOpenModes;
  out Mode: TScSFTPFileOpenMode; out Flags: TScSFTPFileOpenFlags;
  out BlockModes: TScSFTPBlockModes; out DesiredAccess: TScSFTPDesiredAccess);
begin
  if foCreate in OpenModes then begin
    if foExcl in OpenModes then
      Mode := fmCreateNew
    else if foTrunc in OpenModes then
      Mode := fmCreateOrTruncate
    else
      Mode := fmOpenOrCreate;
  end
  else
  if foTrunc in OpenModes then
    Mode := fmTruncateExisting
  else
    Mode := fmOpenExisting;

  Flags := [];
  DesiredAccess := [];
  BlockModes := [bmDelete];


  if foRead in OpenModes then begin
    Include(DesiredAccess, amReadData);
    Include(DesiredAccess, amReadAttributes);
  end;
  if (foWrite in OpenModes) or (foAppend in OpenModes) then begin
    Include(DesiredAccess, amWriteData);
    Include(DesiredAccess, amWriteAttributes);
    Include(BlockModes, bmWrite);
  end;
  if foAppend in OpenModes then begin
    Include(DesiredAccess, amAppendData);
    Include(Flags, ofAppendData);
  end;

  if foText in OpenModes then
    Include(Flags, ofTextMode);
end;

procedure ConvertOpenModesVer5ToVer3(const Mode: TScSFTPFileOpenMode; const Flags: TScSFTPFileOpenFlags;
  const Access: TScSFTPDesiredAccess; out Modes: TScSFTPFileOpenModes);
begin
  Modes := [];

  if (amReadData in Access) or (amReadAttributes in Access) then
    Include(Modes, foRead);
  if (amWriteData in Access) or (amWriteAttributes in Access) then
    Include(Modes, foWrite);
  if (ofAppendData in Flags) or (ofAppendDataAtomic in Flags) then
    Include(Modes, foAppend);

  if ofTextMode in Flags then
    Include(Modes, foText);

  case Mode of
    fmCreateNew: begin
      Include(Modes, foCreate);
      Include(Modes, foExcl);
    end;
    fmCreateOrTruncate: begin
      Include(Modes, foCreate);
      Include(Modes, foTrunc);
    end;
    fmOpenOrCreate:
      Include(Modes, foCreate);
    fmTruncateExisting:
      Include(Modes, foTrunc);
  end;
end;

{ SFTP Convertings }

function ConvertSFTPAttributesToSFTPValue(const Attributes: TScSFTPAttributes; ProtocolVersion: integer): integer;
var
  Item: TScSFTPAttribute;
begin
  Result := 0;
  for Item := Low(TScSFTPAttribute) to High(TScSFTPAttribute) do
    if Item in Attributes then
      Result := Result or AttributeValues[Item];

  case ProtocolVersion of
    0..3: begin
      if aOwnerGroup in Attributes then
        Result := Result or SSH_FILEXFER_ATTR_UIDGID;
      if aModifyTime in Attributes then
        Result := Result or SSH_FILEXFER_ATTR_ACMODTIME;

      Result := Result and AllowedAttributesVersion3;
    end;
    4:
      Result := Result and AllowedAttributesVersion4;
    5:
      Result := Result and AllowedAttributesVersion5;
    6:
      Result := Result and AllowedAttributesVersion6;
  end;
end;

function ConvertSFTPValueToSFTPAttributes(Value: integer): TScSFTPAttributes;
var
  Item: TScSFTPAttribute;
begin
  Result := [];
  for Item := Low(TScSFTPAttribute) to High(TScSFTPAttribute) do
    if (Value and AttributeValues[Item]) <> 0 then
      Include(Result, Item);

  if (Value and SSH_FILEXFER_ATTR_UIDGID) <> 0 then
    Include(Result, aOwnerGroup);
end;

function ConvertFileOpenModesToSFTPValue(const FileOpenModes: TScSFTPFileOpenModes; ProtocolVersion: integer): integer;
var
  Item: TScSFTPFileOpenModeItem;
begin
  Result := 0;
  for Item := Low(TScSFTPFileOpenModeItem) to High(TScSFTPFileOpenModeItem) do
    if Item in FileOpenModes then
      Result := Result or FileOpenModeV3Values[Item];

  if ProtocolVersion < 4 then
    Result := Result and not FileOpenModeV3Values[foText];
end;

function ConvertSFTPValueToFileOpenModes(Value: integer): TScSFTPFileOpenModes;
var
  Item: TScSFTPFileOpenModeItem;
begin
  Result := [];
  for Item := Low(TScSFTPFileOpenModeItem) to High(TScSFTPFileOpenModeItem) do
    if (Value and FileOpenModeV3Values[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertFileOpenModeToSFTPValue(const FileOpenMode: TScSFTPFileOpenMode): integer;
begin
  Result := FileOpenModeV5Values[FileOpenMode];
end;

function ConvertSFTPValueToFileOpenMode(Value: integer): TScSFTPFileOpenMode;
var
  Item: TScSFTPFileOpenMode;
begin
  Value := Value and SSH_FXF_ACCESS_DISPOSITION;
  for Item := Low(TScSFTPFileOpenMode) to High(TScSFTPFileOpenMode) do
    if Value = FileOpenModeV5Values[Item] then begin
      Result := Item;
      Exit;
    end;

  Result := TScSFTPFileOpenMode(-1);
end;

function ConvertBlockModesToSFTPValue(const BlockModes: TScSFTPBlockModes): integer;
var
  Item: TScSFTPBlockMode;
begin
  Result := 0;
  for Item := Low(TScSFTPBlockMode) to High(TScSFTPBlockMode) do
    if Item in BlockModes then
      Result := Result or BlockModeValues[Item];
end;

function ConvertSFTPValueToBlockModes(Value: integer): TScSFTPBlockModes;
var
  Item: TScSFTPBlockMode;
begin
  Result := [];
  for Item := Low(TScSFTPBlockMode) to High(TScSFTPBlockMode) do
    if (Value and BlockModeValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertFileOpenFlagsToSFTPValue(const FileOpenFlags: TScSFTPFileOpenFlags): integer;
var
  Item: TScSFTPFileOpenFlag;
begin
  Result := 0;
  for Item := Low(TScSFTPFileOpenFlag) to High(TScSFTPFileOpenFlag) do
    if Item in FileOpenFlags then
      Result := Result or FileOpenFlagValues[Item];
end;

function ConvertSFTPValueToFileOpenFlags(Value: integer): TScSFTPFileOpenFlags;
var
  Item: TScSFTPFileOpenFlag;
begin
  Result := [];
  for Item := Low(TScSFTPFileOpenFlag) to High(TScSFTPFileOpenFlag) do
    if (Value and FileOpenFlagValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertSupportedAclsToSFTPValue(const SupportedAcls: TScSFTPSupportedAcls): integer;
var
  Item: TScSFTPSupportedAcl;
begin
  Result := 0;
  for Item := Low(TScSFTPSupportedAcl) to High(TScSFTPSupportedAcl) do
    if Item in SupportedAcls then
      Result := Result or SupportedAclValues[Item];
end;

function ConvertSFTPValueToSupportedAcls(Value: integer): TScSFTPSupportedAcls;
var
  Item: TScSFTPSupportedAcl;
begin
  Result := [];
  for Item := Low(TScSFTPSupportedAcl) to High(TScSFTPSupportedAcl) do
    if (Value and SupportedAclValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertFileTypeToSFTPValue(const FileType: TScSFTPFileType; ProtocolVersion: integer): byte;
begin
  if (ProtocolVersion <= 4) and (FileType in [ftSocket, ftCharDevice, ftBlockDevice, ftFifo]) then
    Result := FileTypeValues[ftUnknown]
  else
    Result := FileTypeValues[FileType];
end;

function ConvertSFTPValueToFileType(Value: byte): TScSFTPFileType;
var
  Item: TScSFTPFileType;
begin
  if Value = 0 then begin
    Result := ftUnknown;
    Exit;
  end;

  for Item := Low(TScSFTPFileType) to High(TScSFTPFileType) do
    if Value = FileTypeValues[Item] then begin
      Result := Item;
      Exit;
    end;

  Result := TScSFTPFileType(-1);
end;

function ConvertFilePermissionsToSFTPValue(const FilePermissions: TScSFTPFilePermissions): integer;
var
  Item: TScSFTPFilePermission;
begin
  Result := 0;
  for Item := Low(TScSFTPFilePermission) to High(TScSFTPFilePermission) do
    if Item in FilePermissions then
      Result := Result or FilePermissionValues[Item];
end;

function ConvertSFTPValueToFilePermissions(Value: integer): TScSFTPFilePermissions;
var
  Item: TScSFTPFilePermission;
begin
  Result := [];
  for Item := Low(TScSFTPFilePermission) to High(TScSFTPFilePermission) do
    if (Value and FilePermissionValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertAclFlagsToSFTPValue(const AclFlags: TScSFTPAclFlags): integer;
var
  Item: TScSFTPAclFlag;
begin
  Result := 0;
  for Item := Low(TScSFTPAclFlag) to High(TScSFTPAclFlag) do
    if Item in AclFlags then
      Result := Result or SFTPAclFlagValues[Item];
end;

function ConvertSFTPValueToAclFlags(Value: integer): TScSFTPAclFlags;
var
  Item: TScSFTPAclFlag;
begin
  Result := [];
  for Item := Low(TScSFTPAclFlag) to High(TScSFTPAclFlag) do
    if (Value and SFTPAclFlagValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertAceTypeToSFTPValue(const AceType: TScSFTPAceType): integer;
begin
  Result := SFTPAceTypeValues[AceType];
end;

function ConvertSFTPValueToAceType(Value: integer): TScSFTPAceType;
var
  Item: TScSFTPAceType;
begin
  for Item := Low(TScSFTPAceType) to High(TScSFTPAceType) do
    if Value = SFTPAceTypeValues[Item] then begin
      Result := Item;
      Exit;
    end;

  Result := Low(TScSFTPAceType);
end;

function ConvertAceFlagsToSFTPValue(const AceFlags: TScSFTPAceFlags): integer;
var
  Item: TScSFTPAceFlag;
begin
  Result := 0;
  for Item := Low(TScSFTPAceFlag) to High(TScSFTPAceFlag) do
    if Item in AceFlags then
      Result := Result or SFTPAceFlagValues[Item];
end;

function ConvertSFTPValueToAceFlags(Value: integer): TScSFTPAceFlags;
var
  Item: TScSFTPAceFlag;
begin
  Result := [];
  for Item := Low(TScSFTPAceFlag) to High(TScSFTPAceFlag) do
    if (Value and SFTPAceFlagValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertAceMaskToSFTPValue(const AceMask: TScSFTPAceMask): integer;
var
  Item: TScSFTPAceMaskItem;
begin
  Result := 0;
  for Item := Low(TScSFTPAceMaskItem) to High(TScSFTPAceMaskItem) do
    if Item in AceMask then
      Result := Result or SFTPAceMaskValues[Item];
end;

function ConvertSFTPValueToAceMask(Value: integer): TScSFTPAceMask;
var
  Item: TScSFTPAceMaskItem;
begin
  Result := [];
  for Item := Low(TScSFTPAceMaskItem) to High(TScSFTPAceMaskItem) do
    if (Value and SFTPAceMaskValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertFileAttrsToSFTPValue(const FileAttrs: TScSFTPFileAttrs; ProtocolVersion: integer): integer;
var
  Item: TScSFTPFileAttr;
begin
  Result := 0;
  for Item := Low(TScSFTPFileAttr) to High(TScSFTPFileAttr) do
    if Item in FileAttrs then
      Result := Result or FileAttrValues[Item];

  if ProtocolVersion < 6 then
    Result := Result and (not SSH_FILEXFER_ATTR_FLAGS_TRANSLATION_ERR);
end;

function ConvertSFTPValueToFileAttrs(Value: integer): TScSFTPFileAttrs;
var
  Item: TScSFTPFileAttr;
begin
  Result := [];
  for Item := Low(TScSFTPFileAttr) to High(TScSFTPFileAttr) do
    if (Value and FileAttrValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertTextHintToSFTPValue(const TextHint: TScSFTPTextHint): byte;
begin
  Result := TextHintValues[TextHint];
end;

function ConvertSFTPValueToTextHint(Value: byte): TScSFTPTextHint;
var
  Item: TScSFTPTextHint;
begin
  for Item := Low(TScSFTPTextHint) to High(TScSFTPTextHint) do
    if Value = TextHintValues[Item] then begin
      Result := Item;
      Exit;
    end;

  Result := TScSFTPTextHint(-1);
end;

function ConvertRenameFlagsToSFTPValue(const RenameFlags: TScSFTPRenameFlags): integer;
var
  Item: TScSFTPRenameFlag;
begin
  Result := 0;
  for Item := Low(TScSFTPRenameFlag) to High(TScSFTPRenameFlag) do
    if Item in RenameFlags then
      Result := Result or RenameFlagValues[Item];
end;

function ConvertSFTPValueToRenameFlags(Value: integer): TScSFTPRenameFlags;
var
  Item: TScSFTPRenameFlag;
begin
  Result := [];
  for Item := Low(TScSFTPRenameFlag) to High(TScSFTPRenameFlag) do
    if (Value and RenameFlagValues[Item]) <> 0 then
      Include(Result, Item);
end;

function ConvertRealpathControlToSFTPValue(const RealpathControl: TScSFTPRealpathControl): byte;
begin
  Result := RealpathControlValues[RealpathControl];
end;

function ConvertSFTPValueToRealpathControl(Value: byte): TScSFTPRealpathControl;
var
  Item: TScSFTPRealpathControl;
begin
  for Item := Low(TScSFTPRealpathControl) to High(TScSFTPRealpathControl) do
    if Value = RealpathControlValues[Item] then begin
      Result := Item;
      Exit;
    end;

  Result := Low(TScSFTPRealpathControl);
end;

function ConvertFileAttrsToOSValue(const FileAttrs: TScSFTPFileAttrs): cardinal;
var
  Item: TScSFTPFileAttr;
begin
  Result := 0;
  for Item := Low(TScSFTPFileAttr) to High(TScSFTPFileAttr) do
    if Item in FileAttrs then
      Result := Result or OSFileAttrValues[Item];
end;

function ConvertOSValueToFileAttrs(Value: cardinal): TScSFTPFileAttrs;
var
  Item: TScSFTPFileAttr;
begin
  Result := [];
  for Item := Low(TScSFTPFileAttr) to High(TScSFTPFileAttr) do
    if (Value and OSFileAttrValues[Item]) <> 0 then
      Include(Result, Item);
end;

{$IFDEF MSWINDOWS}
{ Win Convertings }

function ConvertAceTypeToWinValue(const AceType: TScSFTPAceType): integer;
begin
  Result := WinAceTypeValues[AceType];
end;

function ConvertWinValueToAceType(Value: integer): TScSFTPAceType;
var
  Item: TScSFTPAceType;
begin
  for Item := Low(TScSFTPAceType) to High(TScSFTPAceType) do
    if Value = WinAceTypeValues[Item] then begin
      Result := Item;
      Exit;
    end;

  Result := Low(TScSFTPAceType);
end;

function ConvertAceFlagsToWinValue(const AceFlags: TScSFTPAceFlags): integer;
var
  Item: TScSFTPAceFlag;
begin
  Result := 0;
  for Item := Low(TScSFTPAceFlag) to High(TScSFTPAceFlag) do
    if Item in AceFlags then
      Result := Result or WinAceFlagValues[Item];
end;

function ConvertWinValueToAceFlags(Value: integer): TScSFTPAceFlags;
var
  Item: TScSFTPAceFlag;
begin
  Result := [];
  for Item := Low(TScSFTPAceFlag) to High(TScSFTPAceFlag) do
    if (Value and WinAceFlagValues[Item]) <> 0 then
      Include(Result, Item);
end;
{$ENDIF}

{ TScSFTPError }

procedure InitError(var Error: TScSFTPError; const Code: TScSFTPErrorCode; const Message: string = '');
begin
  Error.ErrorCode := Code;
  Error.ErrorMessage := Message;
end;

{ EScSFTPError }

constructor EScSFTPError.Create(SFTPErrorCode: integer; const ErrorMessage: string);
begin
  inherited CreateFmt(SSFTPServerError, [ErrorMessage], seSFTPServerError);
  FSFTPErrorCode := SFTPErrorCode;
end;

{ TScSFTPACEItem }

{$IFDEF MSWINDOWS}
function GetAccountSid(AccountName: string): PSID;
var
  OwnerType: SID_NAME_USE;
  SizeNeededS, SizeNeededD: DWORD;
  DomainName: Pointer;
  p: integer;
begin
  SizeNeededS := 0;
  SizeNeededD := 0;

  p := Pos('@', AccountName);
  if p > 0 then
    AccountName := Copy(AccountName, p + 1, Length(AccountName)) + '\' + Copy(AccountName, 1, p - 1);

  if LookupAccountName(nil, PChar(AccountName), nil, SizeNeededS, nil, SizeNeededD, OwnerType) or
    (GetLastError = ERROR_INSUFFICIENT_BUFFER) then begin
    DomainName := Marshal.AllocHGlobal(SizeNeededD * sizeof(char));
    try
      Result := Marshal.AllocHGlobal(SizeNeededS);
      if not LookupAccountName(nil, PChar(AccountName), Result, SizeNeededS, DomainName, SizeNeededD, OwnerType) then begin
        Marshal.FreeHGlobal(Result);
        Result := nil;
      end;
    finally
      Marshal.FreeHGlobal(DomainName);
    end;
  end
  else
    Result := nil;
end;
{$ENDIF}

destructor TScSFTPACEItem.Destroy;
begin
{$IFDEF MSWINDOWS}
  Marshal.FreeHGlobal(FSid);
{$ENDIF}
  inherited;
end;

procedure TScSFTPACEItem.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSFTPACEItem) then
  begin
    TScSFTPACEItem(Dest).AceType := AceType;
    TScSFTPACEItem(Dest).AceFlags := AceFlags;
    TScSFTPACEItem(Dest).AceMask := AceMask;
    TScSFTPACEItem(Dest).Who := Who;
  end
  else
    inherited;
end;

procedure TScSFTPACEItem.SetWho(const Value: string);
begin
  if FWho <> Value then begin
    FWho := Value;
  {$IFDEF MSWINDOWS}
    Marshal.FreeHGlobal(FSid);
    FSid := nil;
  {$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}
function TScSFTPACEItem.GetSid: PSID;
begin
  if (FSid = nil) and (FWho <> '') then
    FSid := GetAccountSid(FWho);

  Result := FSid;
end;
{$ENDIF}

{ TScSFTPACEs }

constructor TScSFTPACEs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSFTPACEItem);
end;

{ TScSFTPCustomExtension }

procedure TScSFTPCustomExtension.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSFTPCustomExtension) then begin
    TScSFTPCustomExtension(Dest).ParseExtension(Name, Data);
    TScSFTPCustomExtension(Dest).Data := Data;
  end
  else
    inherited;
end;

procedure TScSFTPCustomExtension.Clear;
begin
  FName := '';
  FData := nil;
end;

procedure TScSFTPCustomExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
begin
  FName := ExtName;
  FData := ExtData;
end;

procedure TScSFTPCustomExtension.SetData(const Value: TBytes);
begin
  SetLength(FData, Length(Value));
  if Length(Value) > 0 then
    Buffer.BlockCopy(Value, 0, FData, 0, Length(FData));
end;

{ TScFilenameTranslationControlExtension }

constructor TScFilenameTranslationControlExtension.Create;
begin
  inherited;

  FName := 'filename-translation-control';
  SetLength(FData, 1);
  FData[0] := 0;
end;

procedure TScFilenameTranslationControlExtension.SetDoTranslate(Value: Boolean);
begin
  FDoTranslate := Value;
  if Value then
    FData[0] := 1
  else
    FData[0] := 0;
end;

{ TScCheckFileReplyExtension }

procedure TScCheckFileReplyExtension.Clear;
begin
  inherited;
  FHashAlgorithm := '';
  FHashSize := 0;
  FHash := nil;
end;

procedure TScCheckFileReplyExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
var
  ExtPacket: TSFTPPacketReader;
begin
  inherited ParseExtension(ExtName, ExtData);

  ExtPacket := TSFTPPacketReader.Create(ExtData);
  try
    if ExtName = 'check-file' then begin
      FHashAlgorithm := LowerCase(Encoding.Default.GetString(ExtPacket.ReadString));

      if FHashAlgorithm = 'sha1' then
        FHashSize := 20
      else if FHashAlgorithm = 'md5' then
        FHashSize := 16
      else
        raise EScError.Create(seInvalidHashAlgorithm);

      FHash := ExtPacket.ReadAll;
    end
    else
    if ExtName = 'md5-hash' then begin
      ExtPacket.ReadString; // Name = 'md5-hash'
      FHashAlgorithm := 'md5';
      FHash := ExtPacket.ReadString;
      FHashSize := Length(FHash);
    end
    else
      raise EScError.Create(seUnknownReplyExtensionType);
  finally
    ExtPacket.Free;
  end;
end;

function TScCheckFileReplyExtension.GetHashesCount: integer;
begin
  if FHashSize = 0 then
    Result := 0
  else
    Result := Length(FData) div FHashSize;
end;

function TScCheckFileReplyExtension.GetHash(Index: Integer): TBytes;
begin
  if (Index < 0) or (Index >= HashesCount) then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(Result, FHashSize);
  Buffer.BlockCopy(FHash, FHashSize * Index, Result, 0, Length(Result));
end;

{ TScSpaceAvailableReplyExtension }

procedure TScSpaceAvailableReplyExtension.Clear;
begin
  inherited;
  FBytesOnDevice := 0;
  FUnusedBytesOnDevice := 0;
  FBytesAvailableToUser := 0;
  FUnusedBytesAvailableToUser := 0;
  FBytesPerAllocationUnit := 0;
end;

procedure TScSpaceAvailableReplyExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
var
  ExtPacket: TSFTPPacketReader;
begin
 inherited ParseExtension('space-available', ExtData);

  ExtPacket := TSFTPPacketReader.Create(ExtData);
  try
    FBytesOnDevice := ExtPacket.ReadInt64;
    FUnusedBytesOnDevice := ExtPacket.ReadInt64;
    FBytesAvailableToUser := ExtPacket.ReadInt64;
    FUnusedBytesAvailableToUser := ExtPacket.ReadInt64;

    if ExtPacket.Rest = 2 then
      FBytesPerAllocationUnit := ExtPacket.ReadInt16
    else
      FBytesPerAllocationUnit := ExtPacket.ReadInt32;
  finally
    ExtPacket.Free;
  end;
end;

{ TScSFTPSupportedExtension }

constructor TScSFTPSupportedExtension.Create;
begin
  inherited;
  FAttribExtensionNames := TStringList.Create;
  FExtensionNames := TStringList.Create;
  FRaiseError := True;
end;

destructor TScSFTPSupportedExtension.Destroy;
begin
  FAttribExtensionNames.Free;
  FExtensionNames.Free;

  inherited;
end;

procedure TScSFTPSupportedExtension.Clear;
begin
  inherited;

  FSupportedAttributeMask := 0;
  FSupportedAttributeBits := 0;
  FSupportedOpenFlags := 0;
  FSupportedAccessMask := 0;
  FMaxReadSize := 0;
  FSupportedOpenBlockVector := 0;
  FSupportedBlockVector := 0;
  FAttribExtensionNames.Clear;
  FExtensionNames.Clear;
end;

procedure TScSFTPSupportedExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
var
  ExtPacket: TSFTPPacketReader;
  i, cnt: integer;
begin
  inherited ParseExtension(ExtName, ExtData);

  ExtPacket := TSFTPPacketReader.Create(ExtData);
  try
    FSupportedAttributeMask := ExtPacket.ReadInt32;
    FSupportedAttributeBits := ExtPacket.ReadInt32;
    FSupportedOpenFlags := ExtPacket.ReadInt32;
    FSupportedAccessMask := ExtPacket.ReadInt32;
    FMaxReadSize := ExtPacket.ReadInt32;

    FAttribExtensionNames.Clear;
    FExtensionNames.Clear;

    if ExtName = 'supported' then begin
      FSupportedOpenBlockVector := $FFFF;
      FSupportedBlockVector := $FFFF;

      while ExtPacket.Rest > 0 do
        FExtensionNames.Add(Encoding.Default.GetString(ExtPacket.ReadString));
    end
    else if ExtName = 'supported2' then begin
      FSupportedOpenBlockVector := ExtPacket.ReadInt16;
      FSupportedBlockVector := ExtPacket.ReadInt16;

      cnt := ExtPacket.ReadInt32;
      for i := 1 to cnt do
        FAttribExtensionNames.Add(Encoding.Default.GetString(ExtPacket.ReadString));

      cnt := ExtPacket.ReadInt32;
      for i := 1 to cnt do
        FExtensionNames.Add(Encoding.Default.GetString(ExtPacket.ReadString));
    end
    else
      raise EScError.Create(seUnknownReplyExtensionType);
  finally
    ExtPacket.Free;
  end;
end;

function TScSFTPSupportedExtension.GetSupportedAttributes: TScSFTPAttributes;
begin
  Result := ConvertSFTPValueToSFTPAttributes(FSupportedAttributeMask);
end;

function TScSFTPSupportedExtension.GetSupportedAttributeBits: TScSFTPFileAttrs;
begin
  Result := ConvertSFTPValueToFileAttrs(FSupportedAttributeBits);
end;

function TScSFTPSupportedExtension.GetSupportedOpenFlags: TScSFTPFileOpenFlags;
begin
  Result := ConvertSFTPValueToFileOpenFlags(FSupportedOpenFlags);
end;

function TScSFTPSupportedExtension.GetSupportedBlockModes: TScSFTPBlockModes;
begin
  Result := ConvertSFTPValueToBlockModes(FSupportedOpenFlags);
end;

function TScSFTPSupportedExtension.GetSupportedAccessMask: TScSFTPAceMask;
begin
  Result := ConvertSFTPValueToAceMask(FSupportedAccessMask);
end;

function TScSFTPSupportedExtension.InternalSupportedBlockSet(
  const BlockModes: TScSFTPBlockModes; SupportedBlockVector: integer): boolean;
const
  BlockModeBits: array[TScSFTPBlockMode] of integer = ($01, $02, $04, $08);
var
  Item: TScSFTPBlockMode;
  Shift: integer;
begin
  if SupportedBlockVector = 1 then begin
    // https://tools.ietf.org/html/draft-ietf-secsh-filexfer-13
    // 5.4.  Supported Features
    // a server supporting no locking at all would set only bit zero, giving 0x0001.
    Result := True;
    Exit;
  end;

  Shift := 0;
  for Item := Low(TScSFTPBlockMode) to High(TScSFTPBlockMode) do
    if Item in BlockModes then
      Shift := Shift or BlockModeBits[Item];

  Result := ((SupportedBlockVector shr Shift) and 1) = 1;
end;

function TScSFTPSupportedExtension.IsSupportedOpenBlockSet(const BlockModes: TScSFTPBlockModes): boolean;
begin
  Result := InternalSupportedBlockSet(BlockModes, FSupportedOpenBlockVector);
end;

function TScSFTPSupportedExtension.IsSupportedBlockSet(const BlockModes: TScSFTPBlockModes): boolean;
begin
  Result := InternalSupportedBlockSet(BlockModes, FSupportedBlockVector);
end;

function TScSFTPSupportedExtension.IsOpenBlockSetAvailable: boolean;
begin
  Result := (FSupportedOpenBlockVector <> 0) and (FSupportedOpenBlockVector <> 1);
end;

function TScSFTPSupportedExtension.IsBlockSetAvailable: boolean;
begin
  Result := (FSupportedOpenBlockVector <> 0) and (FSupportedBlockVector <> 1);
end;

procedure TScSFTPSupportedExtension.CheckSupportedFeature(const Mask: integer; var Value: integer);
begin
  if RaiseError then begin
    if (not Mask and Value) <> 0 then
      raise EScError.Create(seUnsupportedAttribute);
  end
  else
    Value := Value and Mask;
end;

procedure TScSFTPSupportedExtension.CheckSupportedAttributes(var Value: integer);
begin
  CheckSupportedFeature(FSupportedAttributeMask, Value);
end;

procedure TScSFTPSupportedExtension.CheckSupportedAttributeBits(var Value: integer);
begin
  CheckSupportedFeature(FSupportedAttributeBits, Value);
end;

procedure TScSFTPSupportedExtension.CheckSupportedOpenFlags(var Value: integer);
begin
  CheckSupportedFeature(FSupportedOpenFlags, Value);
end;

procedure TScSFTPSupportedExtension.CheckSupportedAccessMask(var Value: integer);
begin
  CheckSupportedFeature(FSupportedAccessMask, Value);
end;

procedure TScSFTPSupportedExtension.CheckSupportedOpenBlockSet(const BlockModes: TScSFTPBlockModes);
begin
  if RaiseError and not IsSupportedOpenBlockSet(BlockModes) then
    raise EScError.Create(seUnsupportedBlockMode);
end;

procedure TScSFTPSupportedExtension.CheckSupportedBlockSet(const BlockModes: TScSFTPBlockModes);
begin
  if RaiseError and not IsSupportedBlockSet(BlockModes) then
    raise EScError.Create(seUnsupportedBlockMode);
end;

procedure TScSFTPSupportedExtension.CheckAttribExtensionName(const Name: string);
begin
  if RaiseError then
    if (FAttribExtensionNames.Count > 0) and (FAttribExtensionNames.IndexOf(Name) = -1) then
      raise EScError.Create(seUnsupportedExtensionName);
end;

procedure TScSFTPSupportedExtension.CheckExtensionName(const Name: string);
begin
  if RaiseError then
    if (FExtensionNames.Count > 0) and (FExtensionNames.IndexOf(Name) = -1) then
      raise EScError.Create(seUnsupportedExtensionName);
end;

{ TScSFTPSupportedAclExtension }

procedure TScSFTPSupportedAclExtension.Clear;
begin
  inherited;

  FSupportedAcls := [];
end;

procedure TScSFTPSupportedAclExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
var
  ExtPacket: TSFTPPacketReader;
begin
  inherited ParseExtension(ExtName, ExtData);

  ExtPacket := TSFTPPacketReader.Create(ExtData);
  try
    FSupportedAcls := ConvertSFTPValueToSupportedAcls(ExtPacket.ReadInt32);
  finally
    ExtPacket.Free;
  end;
end;

{ TScSFTPVendorExtension }

procedure TScSFTPVendorExtension.Clear;
begin
  inherited;

  FVendorName := '';
  FProductName := '';
  FProductVersion := '';
  FProductBuildNumber := 0;
end;

procedure TScSFTPVendorExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
var
  ExtPacket: TSFTPPacketReader;
begin
  inherited ParseExtension(ExtName, ExtData);

  ExtPacket := TSFTPPacketReader.Create(ExtData);
  try
    FVendorName := Encoding.UTF8.GetString(ExtPacket.ReadString);
    FProductName := Encoding.UTF8.GetString(ExtPacket.ReadString);
    FProductVersion := Encoding.UTF8.GetString(ExtPacket.ReadString);
    FProductBuildNumber := ExtPacket.ReadInt64;
  finally
    ExtPacket.Free;
  end;
end;

{ TScSFTPVersionsExtension }

procedure TScSFTPVersionsExtension.Clear;
begin
  inherited;

  FVersions := [];
  FStrVersions := '';
end;

procedure TScSFTPVersionsExtension.ParseExtension(const ExtName: string; const ExtData: TBytes);
var
  StartPos, Val: integer;
  Ver: string;
  i: integer;
begin
  inherited ParseExtension(ExtName, ExtData);

  FStrVersions := Encoding.Default.GetString(ExtData);
  FVersions := [];
  StartPos := 1;
  for i := 1 to Length(FStrVersions) + 1 do begin
    if (i > Length(FStrVersions)) or (FStrVersions[i] = ',') then begin
      Ver := Copy(FStrVersions, StartPos, i - StartPos);
      if TryStrToInt(Ver, Val) then
        Include(FVersions, TScSFTPVersion(Val));

      StartPos := i + 1;
    end;
  end;
end;

{ TScSFTPFileAttributes }

constructor TScSFTPFileAttributes.Create;
begin
  inherited;

  FAclFlags := [aclControlPresent];
  FACEs := TScSFTPACEs.Create(nil);
  FACEs.OnChanged := OnChanged;
  FExtendedAttributes := TCRObjectList.Create;
  FValidAttributes := [];
  FFileType := ftUnknown;
end;

destructor TScSFTPFileAttributes.Destroy;
begin
  FACEs.Free;
  FExtendedAttributes.Free;

  inherited;
end;

procedure TScSFTPFileAttributes.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSFTPFileAttributes) then begin
    TScSFTPFileAttributes(Dest).FFileType := FFileType;
    TScSFTPFileAttributes(Dest).FSize := FSize;
    TScSFTPFileAttributes(Dest).FAllocationSize := FAllocationSize;
    TScSFTPFileAttributes(Dest).FAclFlags := FAclFlags;
    TScSFTPFileAttributes(Dest).FACEs.Assign(FACEs);
    TScSFTPFileAttributes(Dest).FAttrs := FAttrs;
    TScSFTPFileAttributes(Dest).FGID := FGID;
    TScSFTPFileAttributes(Dest).FUID := FUID;
    TScSFTPFileAttributes(Dest).FGroup := FGroup;
    TScSFTPFileAttributes(Dest).FOwner := FOwner;
    TScSFTPFileAttributes(Dest).FAccessTime := FAccessTime;
    TScSFTPFileAttributes(Dest).FCreateTime := FCreateTime;
    TScSFTPFileAttributes(Dest).FModifyTime := FModifyTime;
    TScSFTPFileAttributes(Dest).FChangeAttrTime := FChangeAttrTime;
    TScSFTPFileAttributes(Dest).FPermissions := FPermissions;
    TScSFTPFileAttributes(Dest).FTextHint := FTextHint;
    TScSFTPFileAttributes(Dest).FMimeType := FMimeType;
    TScSFTPFileAttributes(Dest).FLinkCount := FLinkCount;
    TScSFTPFileAttributes(Dest).FUntranslatedName := FUntranslatedName;
    TScSFTPFileAttributes(Dest).FValidAttributes := FValidAttributes;
  {$IFNDEF NEXTGEN}
    TScSFTPFileAttributes(Dest).FExtendedAttributes.Assign(FExtendedAttributes);
  {$ENDIF}
  end
  else
    inherited;
end;

procedure TScSFTPFileAttributes.ParseTime(const DateTime: TDateTime;
  out Second: Int64; out NSec: Integer);
var
  t: TDateTime;
  MSec: Int64;
begin
  t := DateTime - UnixDateDelta;
  Second := Trunc(t * SecsPerDay);
  MSec := Round(t * MSecsPerDay) - Second * 1000;
  NSec := Integer(MSec) * 1000000;
end;

function TScSFTPFileAttributes.Int64ToDateTime(const Second: Int64; const NSec: Integer): TDateTime;
var
  MSec: Integer;
begin
  MSec := Round(NSec / 1000000);
  Result := {$IFNDEF LINUX}{$IFNDEF LINUX_BSD}{$IFNDEF CPU64}Extended{$ENDIF}{$ENDIF}{$ENDIF}(Second / SecsPerDay) +
    {$IFNDEF LINUX}{$IFNDEF LINUX_BSD}{$IFNDEF CPU64}Extended{$ENDIF}{$ENDIF}{$ENDIF}(MSec / MSecsPerDay);
  Result := Result + UnixDateDelta;
end;

function TScSFTPFileAttributes.ConvertACLToSFTPBuffer(Version: integer;
  SupportedExtension: TScSFTPSupportedExtension): TBytes;
var
  SFTPPacket: TSFTPPacketWriter;
  AceMaskVal: integer;
  i: integer;
begin
  SFTPPacket := TSFTPPacketWriter.Create;
  try
    if Version >= 6 then // AclFlags
      SFTPPacket.WriteInt32(ConvertAclFlagsToSFTPValue(FAclFlags));

    // ACEs
    SFTPPacket.WriteInt32(ACEs.Count);
    for i := 0 to ACEs.Count - 1 do
      with ACEs.Items[i] as TScSFTPACEItem do begin
        SFTPPacket.WriteInt32(ConvertAceTypeToSFTPValue(AceType));
        SFTPPacket.WriteInt32(ConvertAceFlagsToSFTPValue(AceFlags));

        AceMaskVal := ConvertAceMaskToSFTPValue(AceMask);
        if SupportedExtension <> nil then
          SupportedExtension.CheckSupportedAccessMask(AceMaskVal);
        SFTPPacket.WriteInt32(AceMaskVal);

        SFTPPacket.WriteWStr(WideString(Who));
      end;

    Result := SFTPPacket.ToBytes;
  finally
    SFTPPacket.Free;
  end;
end;

procedure TScSFTPFileAttributes.ConvertSFTPBufferToACL(const Value: TBytes; Version: integer);
var
  SFTPPacket: TSFTPPacketReader;
  ACEItem: TScSFTPACEItem;
  i, Count: integer;
begin
  SFTPPacket := TSFTPPacketReader.Create(Value);
  try
    if Version >= 6 then // AclFlags
      FAclFlags := ConvertSFTPValueToAclFlags(SFTPPacket.ReadInt32);

    // ACEs
    ACEs.Clear;
    Count := SFTPPacket.ReadInt32;
    for i := 1 to Count do begin
      ACEItem := ACEs.Add as TScSFTPACEItem;

      ACEItem.AceType := ConvertSFTPValueToAceType(SFTPPacket.ReadInt32);
      ACEItem.AceFlags := ConvertSFTPValueToAceFlags(SFTPPacket.ReadInt32);
      ACEItem.AceMask := ConvertSFTPValueToAceMask(SFTPPacket.ReadInt32);
      ACEItem.Who := Encoding.UTF8.GetString(SFTPPacket.ReadString);
    end;
  finally
    SFTPPacket.Free;
  end;
end;

class procedure TScSFTPFileAttributes.WriteToPacketDefaultValues(SftpPacket: TSFTPPacketWriter;
  const FileType: TScSFTPFileType; Version: integer);
begin
  SftpPacket.WriteInt32(0);
  if Version > 3 then
    SftpPacket.WriteByte(ConvertFileTypeToSFTPValue(FileType, Version));
end;

procedure TScSFTPFileAttributes.WriteToPacket(SftpPacket: TSFTPPacketWriter;
  Version: integer; SupportedExtension: TScSFTPSupportedExtension);
var
  CurValidAttributes: TScSFTPAttributes;
  ValidAttributesValue, AttrsValue: integer;
  PosixPermissions: integer;
  Second: Int64;
  NSec: Integer;
  i: Integer;
begin
  ValidAttributesValue := ConvertSFTPAttributesToSFTPValue(ValidAttributes, Version);
  if SupportedExtension <> nil then
    if SupportedExtension.FSupportedAttributeMask <> 0 then
      SupportedExtension.CheckSupportedAttributes(ValidAttributesValue);

  SftpPacket.WriteInt32(ValidAttributesValue);
  CurValidAttributes := ConvertSFTPValueToSFTPAttributes(ValidAttributesValue);

  if Version > 3 then
    SftpPacket.WriteByte(ConvertFileTypeToSFTPValue(FFileType, Version));

  if aSize in CurValidAttributes then
    SftpPacket.WriteInt64(Size);

  if (Version >= 6) and (aAllocationSize in CurValidAttributes) then
    SftpPacket.WriteInt64(AllocationSize);

  if aOwnerGroup in CurValidAttributes then begin
    if Version <= 3 then begin
      SftpPacket.WriteInt32(UID);
      SftpPacket.WriteInt32(GID);
    end
    else begin
      SftpPacket.WriteWStr(WideString(Owner));
      SftpPacket.WriteWStr(WideString(Group));
    end;
  end;

  if aPermissions in CurValidAttributes then begin
    case FFileType of
      ftFile:
        PosixPermissions := S_IFREG;
      ftDirectory:
        PosixPermissions := S_IFDIR;
      ftSymlink:
        PosixPermissions := S_IFLNK;
    else
      PosixPermissions := 0;
    end;
    PosixPermissions := PosixPermissions or ConvertFilePermissionsToSFTPValue(FPermissions);
    SftpPacket.WriteInt32(PosixPermissions);
  end;

  if Version <= 3 then begin
    if (aAccessTime in CurValidAttributes) or (aModifyTime in CurValidAttributes) then begin
      if AccessTime = 0 then
        AccessTime := Now;
      ParseTime(AccessTime, Second, NSec);
      SftpPacket.WriteInt32(integer(Second));

      if ModifyTime = 0 then
        ModifyTime := Now;
      ParseTime(ModifyTime, Second, NSec);
      SftpPacket.WriteInt32(integer(Second));
    end;
  end
  else begin
    if aAccessTime in CurValidAttributes then begin
      ParseTime(AccessTime, Second, NSec);
      SftpPacket.WriteInt64(Second);
      if aSubsecondTimes in CurValidAttributes then
        SftpPacket.WriteInt32(NSec);
    end;
    if aCreateTime in CurValidAttributes then begin
      ParseTime(CreateTime, Second, NSec);
      SftpPacket.WriteInt64(Second);
      if aSubsecondTimes in CurValidAttributes then
        SftpPacket.WriteInt32(NSec);
    end;
    if aModifyTime in CurValidAttributes then begin
      ParseTime(ModifyTime, Second, NSec);
      SftpPacket.WriteInt64(Second);
      if aSubsecondTimes in CurValidAttributes then
        SftpPacket.WriteInt32(NSec);
    end;
    if (aChangeAttrTime in CurValidAttributes) and (Version >= 6) then begin
      ParseTime(ChangeAttrTime, Second, NSec);
      SftpPacket.WriteInt64(Second);
      if aSubsecondTimes in CurValidAttributes then
        SftpPacket.WriteInt32(NSec);
    end;
  end;

  if (Version >= 4) and (aAcl in CurValidAttributes) then
    SftpPacket.WriteAsString(ConvertACLToSFTPBuffer(Version, SupportedExtension));

  if (Version >= 5) and (aAttrs in CurValidAttributes) then begin
    AttrsValue := ConvertFileAttrsToSFTPValue(FAttrs, Version);
    if SupportedExtension <> nil then
      if SupportedExtension.FSupportedAttributeBits <> 0 then
        SupportedExtension.CheckSupportedAttributeBits(AttrsValue);

    SftpPacket.WriteInt32(AttrsValue); // attrib-bits
    if Version >= 6 then
      SftpPacket.WriteInt32(AttrsValue); // attrib-bits-valid
  end;

  if Version >= 6 then begin
    if aTextHint in CurValidAttributes then
      SftpPacket.WriteByte(ConvertTextHintToSFTPValue(TextHint));
    if aMimeType in CurValidAttributes then
      SftpPacket.WriteAStr(MimeType);
    if aLinkCount in CurValidAttributes then
      SftpPacket.WriteInt32(LinkCount);
    if aUntranslatedName in CurValidAttributes then
      SftpPacket.WriteAStr(UntranslatedName);
  end;

  if aExtended in CurValidAttributes then begin
    SftpPacket.WriteInt32(ExtendedAttributes.Count);
    for i := 0 to ExtendedAttributes.Count - 1 do
      with TObject(ExtendedAttributes.Items[i]) as TScSFTPExtension do begin
        if SupportedExtension <> nil then
          SupportedExtension.CheckAttribExtensionName(Name);
        SftpPacket.WriteAStr(Name);
        SftpPacket.WriteAsString(Data);
      end;
  end;
end;

procedure TScSFTPFileAttributes.ReadFromPacket(SftpPacket: TSFTPPacketReader; Version: integer);
var
  ValidAttrs: Integer;
  PosixPermissions: integer;
  Second: Int64;
  NSec: Integer;
  ACLBuffer: TBytes;
  ExtAttr: TScSFTPExtension;
  i, Count: Integer;
begin
  ValidAttrs := SftpPacket.ReadInt32;
  case Version of
    0..3:
      if (ValidAttrs and not AllowedAttributesVersion3) <> 0 then
        raise EScError.Create(seUnsupportedFileAttrData);
    4:
      if (ValidAttrs and not AllowedAttributesVersion4) <> 0 then
        raise EScError.Create(seUnsupportedFileAttrData);
    5:
      if (ValidAttrs and not AllowedAttributesVersion5) <> 0 then
        raise EScError.Create(seUnsupportedFileAttrData);
    6:
      if (ValidAttrs and not AllowedAttributesVersion6) <> 0 then
        raise EScError.Create(seUnsupportedFileAttrData);
  end;
  FValidAttributes := ConvertSFTPValueToSFTPAttributes(ValidAttrs);

  if Version > 3 then begin
    FFileType := ConvertSFTPValueToFileType(SftpPacket.ReadByte);
    if FFileType = TScSFTPFileType(-1) then
      raise EScError.Create(seUnknownFileType);
  end
  else
    FFileType := ftUnknown;

  if FValidAttributes = [] then
    Exit;

  if aSize in FValidAttributes then
    FSize := SftpPacket.ReadInt64;

  if aAllocationSize in FValidAttributes then
    FAllocationSize := SftpPacket.ReadInt64;

  if aOwnerGroup in FValidAttributes then begin
    if Version <= 3 then begin
      FUID := SftpPacket.ReadInt32;
      FGID := SftpPacket.ReadInt32;
    end
    else begin
      FOwner := Encoding.UTF8.GetString(SftpPacket.ReadString);
      FGroup := Encoding.UTF8.GetString(SftpPacket.ReadString);
    end;
  end;

  if aPermissions in FValidAttributes then begin
    PosixPermissions := SftpPacket.ReadInt32;
    FPermissions := ConvertSFTPValueToFilePermissions(PosixPermissions);

    if FFileType = ftUnknown then begin
      PosixPermissions := PosixPermissions and S_IFMT;
      if PosixPermissions = S_IFREG then
        FFileType := ftFile
      else
      if PosixPermissions = S_IFDIR then
        FFileType := ftDirectory
      else
      if PosixPermissions = S_IFLNK then
        FFileType := ftSymlink;
    end;
  end;

  if Version <= 3 then begin
    if (aAccessTime in FValidAttributes) or (aModifyTime in FValidAttributes) then begin
      Second := SftpPacket.ReadInt32;
      FAccessTime := Int64ToDateTime(Second, 0);
      Second := SftpPacket.ReadInt32;
      FModifyTime := Int64ToDateTime(Second, 0);
      FValidAttributes := FValidAttributes + [aAccessTime, aModifyTime];
    end;
  end
  else begin
    if aAccessTime in FValidAttributes then begin
      Second := SftpPacket.ReadInt64;
      if aSubsecondTimes in FValidAttributes then
        NSec := SftpPacket.ReadInt32
      else
        NSec := 0;
      FAccessTime := Int64ToDateTime(Second, NSec);
    end;
    if aCreateTime in FValidAttributes then begin
      Second := SftpPacket.ReadInt64;
      if aSubsecondTimes in FValidAttributes then
        NSec := SftpPacket.ReadInt32
      else
        NSec := 0;
      FCreateTime := Int64ToDateTime(Second, NSec);
    end;
    if aModifyTime in FValidAttributes then begin
      Second := SftpPacket.ReadInt64;
      if aSubsecondTimes in FValidAttributes then
        NSec := SftpPacket.ReadInt32
      else
        NSec := 0;
      FModifyTime := Int64ToDateTime(Second, NSec);
    end;
    if aChangeAttrTime in FValidAttributes then begin
      Second := SftpPacket.ReadInt64;
      if aSubsecondTimes in FValidAttributes then
        NSec := SftpPacket.ReadInt32
      else
        NSec := 0;
      FChangeAttrTime := Int64ToDateTime(Second, NSec);
    end;
  end;

  if aAcl in FValidAttributes then begin
    SetLength(ACLBuffer, 0);
    ACLBuffer := SftpPacket.ReadString;
    ConvertSFTPBufferToACL(ACLBuffer, Version);
  end;

  if aAttrs in FValidAttributes then begin
    FAttrs := ConvertSFTPValueToFileAttrs(SftpPacket.ReadInt32);
    if Version >= 6 then
      FAttrs := ConvertSFTPValueToFileAttrs(SftpPacket.ReadInt32);
  end;

  if aTextHint in FValidAttributes then begin
    FTextHint := ConvertSFTPValueToTextHint(SftpPacket.ReadByte);
    if FTextHint = TScSFTPTextHint(-1) then
      raise EScError.Create(seUnknownTextHintValue);
  end;

  if aMimeType in FValidAttributes then
    FMimeType := Encoding.Default.GetString(SftpPacket.ReadString);

  if aLinkCount in FValidAttributes then
    FLinkCount := SftpPacket.ReadInt32;

  if aUntranslatedName in FValidAttributes then
    FUntranslatedName := Encoding.Default.GetString(SftpPacket.ReadString);

  if aExtended in FValidAttributes then begin
    ExtendedAttributes.Clear;
    Count := SftpPacket.ReadInt32;
    for i := 1 to Count do begin
      ExtAttr := TScSFTPExtension.Create;
      ExtendedAttributes.Add(ExtAttr);
      ExtAttr.ParseExtension(Encoding.Default.GetString(SftpPacket.ReadString),
        SftpPacket.ReadString);
    end;
  end;
end;

function TScSFTPFileAttributes.GetAttributesAsLongname: string;
var
{$IFNDEF VER7P}
  OldTimeSeparator: Char;
  OldShortDateFormat: string;
  OldLongTimeFormat: string;
{$ENDIF}
  s: string;
begin
  s := StringOfChar('-', 10);

  if FFileType = ftDirectory then
    s[1] := 'd';

  if aPermissions in ValidAttributes then begin
    if pR_USR in FPermissions then
      s[2] := 'r';
    if pW_USR in FPermissions then
      s[3] := 'w';
    if pX_USR in FPermissions then
      s[4] := 'x';
    if pR_GRP in FPermissions then
      s[5] := 'r';
    if pW_GRP in FPermissions then
      s[6] := 'w';
    if pX_GRP in FPermissions then
      s[7] := 'x';
    if pR_OTH in FPermissions then
      s[8] := 'r';
    if pW_OTH in FPermissions then
      s[9] := 'w';
    if pX_OTH in FPermissions then
      s[10] := 'x';
  end;

  if aLinkCount in ValidAttributes then
    s := s + ' ' + Format('%3d', [FLinkCount])
  else
    s := s + ' ' + Format('%3d', [1]);

  if aOwnerGroup in ValidAttributes then begin
    s := s + ' ' + FOwner + StringOfChar(' ', 8 - Length(FOwner));
    s := s + ' ' + FGroup + StringOfChar(' ', 8 - Length(FGroup));
  end
  else begin
    s := s + ' ' + StringOfChar(' ', 8);
    s := s + ' ' + StringOfChar(' ', 8);
  end;

  if aSize in ValidAttributes then
    s := s + ' ' + Format('%8d', [FSize])
  else
    s := s + ' ' + Format('%8d', [0]);

  if aModifyTime in ValidAttributes then begin
  {$IFDEF VER7P}
    s := s + ' ' + DateTimeToStr(FModifyTime, SFTPFormatSettings);
  {$ELSE}
    OldTimeSeparator := {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}TimeSeparator;
    OldShortDateFormat := {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortDateFormat;
    OldLongTimeFormat := {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}LongTimeFormat;
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}TimeSeparator := ':';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortDateFormat := 'mmm dd';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}LongTimeFormat := 'hh:mm';
    try
      s := s + ' ' + DateTimeToStr(FModifyTime);
    finally
      {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}TimeSeparator := OldTimeSeparator;
      {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortDateFormat := OldShortDateFormat;
      {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}LongTimeFormat := OldLongTimeFormat;
    end;
  {$ENDIF}
  end
  else
    s := s + ' ' + StringOfChar(' ', 12);

  Result := s;
end;

procedure TScSFTPFileAttributes.Clear;
begin
  FFileType := ftUnknown;
  FSize := 0;
  FAllocationSize := 0;
  FAccessTime := 0;
  FCreateTime := 0;
  FModifyTime := 0;
  FChangeAttrTime := 0;
  FLinkCount := 0;
  FUID := 0;
  FOwner := '';
  FGID := 0;
  FGroup := '';
  FPermissions := [];
  FAclFlags := [aclControlPresent];
  FACEs.Clear;
  FAttrs := [];
  FMimeType := '';
  FUntranslatedName := '';
  FExtendedAttributes.Clear;

  FValidAttributes := [];
end;

{$IFDEF MSWINDOWS}
function TScSFTPFileAttributes.DirsCount(const Path: string): integer;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(IncludeTrailingBackslash(Path) + '*.*', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) <> 0 then
        Inc(Result);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TScSFTPFileAttributes.RequestSecurityInfo(const Path: string; SecDescr: Pointer; const ReqAttrs: TScSFTPAttributes);

  function ConvertSID2UID(const Sid: PSID): integer;
  var
    PStrSid: Pointer;
    StrSid: string;
    i: integer;
  begin
    if not ConvertSidToStringSid(Sid, PStrSid) then begin
      Result := 0;
      Exit;
    end;

    StrSid := string(AnsiString(PAnsiChar(PStrSid)));
    LocalFree(HLOCAL(PStrSid));
    i := Length(StrSid);
    while i > 0 do begin
      if not CharInSet(StrSid[i], ['0'..'9']) then
        break;
      Dec(i);
    end;

    StrSid := Copy(StrSid, i + 1, Length(StrSid));
    Result := 10000 + StrToIntDef(StrSid, 0);
  end;

  function GetAccountName(const Sid: PSID): string;
  var
    SizeU, SizeD: DWORD;
    User, Domain: PChar;
    OwnerType: SID_NAME_USE;
  begin
    SizeU := MAX_PATH;
    SizeD := MAX_PATH;
    GetMem(User, MAX_PATH * sizeof(char));
    GetMem(Domain, MAX_PATH * sizeof(char));
    try
      if LookupAccountSID(nil, Sid, User, SizeU, Domain, SizeD, OwnerType) then
        if ((OwnerType = SidTypeUser) or (OwnerType = SidTypeGroup)) and (SizeD > 0) then
          Result := string(User) + '@' + string(Domain)
        else
          Result := string(User)
      else
        Result := '';
    finally
      FreeMem(User);
      FreeMem(Domain);
    end;
  end;

var
  IsDefaulted, DaclPresent: BOOL;
  SizeNeeded: DWORD;
  UserName: string;
  PUserName: PChar;
  pDacl: PACL;
  Trust: TRUSTEE;
  AMask: ACCESS_MASK;
  FilePermissions: TScSFTPFilePermissions;
  pAclInfo: PACL_SIZE_INFORMATION;
  AclInfo: ACL_SIZE_INFORMATION;
  pAce: Pointer;
  ACEItem: TScSFTPACEItem;
  i: integer;
begin
  FOSid := nil;
  FGSid := nil;

  try
    if GetSecurityDescriptorOwner(SecDescr, FOSid, {$IFDEF FPC}@{$ENDIF}IsDefaulted) then begin
      if (aOwnerGroup in ReqAttrs) or (ReqAttrs = []) then begin
        UID := ConvertSID2UID(FOSid);
        Owner := GetAccountName(FOSid);
      end;
    end
    else begin
      PUserName := Marshal.AllocHGlobal(MAX_PATH * sizeof(char));
      try
        SizeNeeded := MAX_PATH;
        if GetUserNameEx(NameUserPrincipal, PUserName, SizeNeeded) then
          UserName := string(PUserName)
        else
          if GetUserName(PUserName, SizeNeeded) then
            UserName := string(PUserName)
          else
            UserName := '';
      finally
        Marshal.FreeHGlobal(PUserName);
      end;

      if UserName <> '' then begin
        FOSid := GetAccountSid(UserName);
        if FOSid <> nil then
          if (aOwnerGroup in ReqAttrs) or (ReqAttrs = []) then begin
            UID := ConvertSID2UID(FOSid);
            Owner := UserName;
          end;
      end;
    end;

    if GetSecurityDescriptorGroup(SecDescr, FGSid, {$IFDEF FPC}@{$ENDIF}IsDefaulted) then begin
      if (aOwnerGroup in ReqAttrs) or (ReqAttrs = []) then begin
        GID := ConvertSID2UID(FGSid);
        Group := GetAccountName(FGSid);
      end;
    end;

    Permissions := [];

    if (aPermissions in ReqAttrs) or (aAcl in ReqAttrs) or (ReqAttrs = []) then begin
      if Assigned(FRequestFileSecurityAttributes) then
        FRequestFileSecurityAttributes(Self, Path, SecDescr)
      else
        if GetSecurityDescriptorDacl(SecDescr, {$IFDEF FPC}@{$ENDIF}DaclPresent, pDacl, {$IFDEF FPC}@{$ENDIF}IsDefaulted)
          and DaclPresent then begin

          if (aPermissions in ReqAttrs) or (ReqAttrs = []) then begin
            FilePermissions := [];

            if FOSID <> nil then begin
              BuildTrusteeWithSid(Trust, FOSID);
              if GetEffectiveRightsFromAcl(pDacl, Trust, AMask) = NO_ERROR then begin
                if ((AMask and GENERIC_READ) = GENERIC_READ) or ((AMask and FILE_GENERIC_READ) = FILE_GENERIC_READ) then
                  Include(FilePermissions, pR_USR);
                if ((AMask and GENERIC_WRITE) = GENERIC_WRITE) or ((AMask and FILE_GENERIC_WRITE) = FILE_GENERIC_WRITE) then
                  Include(FilePermissions, pW_USR);
                if ((AMask and GENERIC_EXECUTE) = GENERIC_EXECUTE) or ((AMask and FILE_GENERIC_EXECUTE) = FILE_GENERIC_EXECUTE) then
                  Include(FilePermissions, pX_USR);

                Permissions := FilePermissions;
              end;
            end;

            if FGSID <> nil then begin
              BuildTrusteeWithSid(Trust, FGSID);
              if GetEffectiveRightsFromAcl(pDacl, Trust, AMask) = NO_ERROR then begin
                if ((AMask and GENERIC_READ) = GENERIC_READ) or ((AMask and FILE_GENERIC_READ) = FILE_GENERIC_READ) then begin
                  Include(FilePermissions, pR_GRP);
                  Include(FilePermissions, pR_OTH);
                end;
                if ((AMask and GENERIC_WRITE) = GENERIC_WRITE) or ((AMask and FILE_GENERIC_WRITE) = FILE_GENERIC_WRITE) then begin
                  Include(FilePermissions, pW_GRP);
                  Include(FilePermissions, pW_OTH);
                end;
                if ((AMask and GENERIC_EXECUTE) = GENERIC_EXECUTE) or ((AMask and FILE_GENERIC_EXECUTE) = FILE_GENERIC_EXECUTE) then begin
                  Include(FilePermissions, pX_GRP);
                  Include(FilePermissions, pX_OTH);
                end;

                Permissions := FilePermissions;
              end;
            end;
          end;

          if (aAcl in ReqAttrs) or (ReqAttrs = []) then begin
            pAclInfo := @AclInfo;
            if (pDacl <> nil) and GetAclInformation(pDacl{$IFNDEF FPC}^{$ENDIF}, pAclInfo, sizeof(ACL_SIZE_INFORMATION), AclSizeInformation) then begin
              for i := 0 to integer(AclInfo.AceCount) - 1 do begin
                if GetACE(pDacl{$IFNDEF FPC}^{$ENDIF}, i, pAce) then begin
                  if PACCESS_ACE(pAce).Header.AceType in [ACETYPE_ACCESS_ALLOWED, ACETYPE_ACCESS_DENIED, ACETYPE_SYSTEM_AUDIT] then begin
                    ACEItem := FACEs.Add as TScSFTPACEItem;
                    ACEItem.AceType := ConvertWinValueToAceType(PACCESS_ACE(pAce).Header.AceType);
                    ACEItem.AceFlags := ConvertWinValueToAceFlags(PACCESS_ACE(pAce).Header.AceFlags);
                    ACEItem.AceMask := ConvertSFTPValueToAceMask(PACCESS_ACE(pAce).Mask);
                    ACEItem.Who := GetAccountName(@PACCESS_ACE(pAce).SidStart);
                  end;
                end;
              end;
            end;
          end;
        end;
    end;
  finally
    if UserName <> '' then begin
      Marshal.FreeHGlobal(FOSid);
      FOSid := nil;
    end;
  end;
end;

function TScSFTPFileAttributes.SetSecurityInfo(const Handle: THandle): boolean;
var
  Sid: PSID;
  pDacl: PACL;
  AclSize: DWORD;
  AccessFlags, AccessMask: DWORD;
  i: integer;
begin
  Result := False;

  if aOwnerGroup in ValidAttributes then begin
    Sid := GetAccountSid(FOwner);
    if Sid <> nil then
      try
        if WinSetSecurityInfo(Handle, SE_FILE_OBJECT, OWNER_SECURITY_INFORMATION, Sid, nil, nil, nil) <> 0 then
          Exit;
      finally
        Marshal.FreeHGlobal(Sid);
      end;

    Sid := GetAccountSid(FGroup);
    if Sid <> nil then
      try
        if WinSetSecurityInfo(Handle, SE_FILE_OBJECT, GROUP_SECURITY_INFORMATION, nil, Sid, nil, nil) <> 0 then
          Exit;
      finally
        Marshal.FreeHGlobal(Sid);
      end;
  end;

  if aAcl in ValidAttributes then begin
    if not (aclControlPresent in FAclFlags) then
      if WinSetSecurityInfo(Handle, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, nil, nil) <> 0 then
        Exit;

    if FACEs.Count > 0 then begin
      AclSize := sizeof(ACL) + (sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD)) * FACEs.Count;
      for i := 0 to FACEs.Count - 1 do
        AclSize := AclSize + GetLengthSid((FACEs.Items[i] as TScSFTPACEItem).GetSid);
      AclSize := (AclSize + sizeof(DWORD) - 1) and $FFFFFFFC; //align to DWORD

      pDacl := Marshal.AllocHGlobal(AclSize);
      try
        if not InitializeAcl(pDacl^, AclSize, ACL_REVISION) then
          Exit;

        for i := 0 to FACEs.Count - 1 do begin
          with FACEs.Items[i] as TScSFTPACEItem do begin
            AccessFlags := ConvertAceFlagsToWinValue(AceFlags);
            AccessMask := ConvertAceMaskToSFTPValue(AceMask);

            case AceType of
              atAccessAllowed:
                if not AddAccessAllowedAceEx(pDacl^, ACL_REVISION, AccessFlags, AccessMask, GetSid) then
                  Exit;
              atAccessDenied:
                if not AddAccessDeniedAceEx(pDacl^, ACL_REVISION, AccessFlags, AccessMask, GetSid) then
                  Exit;
              atSystemAudit:
                if not AddAuditAccessAceEx(pDacl^, ACL_REVISION, AccessFlags, AccessMask, GetSid, False, False) then
                  Exit;
            end;
          end;
        end;

        if WinSetSecurityInfo(Handle, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, pDacl, nil) <> 0 then
          Exit;
      finally
        Marshal.FreeHGlobal(pDacl);
      end;
    end;
  end;

  Result := True;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
procedure TScSFTPFileAttributes.RequestSysInfo(StatBuf: _stat; const ReqAttrs: TScSFTPAttributes);
{$IFNDEF IOS}
{$IFNDEF ANDROID}
var
  _PPasswd: Ppasswd;
  _PGroup: Pgroup;
{$ENDIF}
{$ENDIF}
begin
  if (aSize in ReqAttrs) or (ReqAttrs = []) then
    Size := StatBuf.st_size;

  if (aAllocationSize in ReqAttrs) or (ReqAttrs = []) then begin
    AllocationSize := Int64(StatBuf.st_blocks) * StatBuf.st_blksize;
    if AllocationSize = 0 then
      AllocationSize := StatBuf.st_size;
  end;

  if (aLinkCount in ReqAttrs) or (ReqAttrs = []) then
    LinkCount := StatBuf.st_nlink;

  if (aOwnerGroup in ReqAttrs) or (ReqAttrs = []) then begin
    UID := StatBuf.st_uid;
  {$IFNDEF IOS}
  {$IFNDEF ANDROID}
    _PPasswd := {$IFDEF POSIX}getpwuid{$ELSE}fpgetpwuid{$ENDIF}(UID);
    if _PPasswd <> nil then
      Owner := string(_PPasswd.pw_name)
    else
  {$ENDIF}
  {$ENDIF}
      Owner := IntToStr(UID);

    GID := StatBuf.st_gid;
  {$IFNDEF IOS}
  {$IFNDEF ANDROID}
    _PGroup := {$IFDEF POSIX}getgrgid{$ELSE}fpgetgrgid{$ENDIF}(GID);
    if _PGroup <> nil then
      Group := string(_PGroup.gr_name)
    else
  {$ENDIF}
  {$ENDIF}
      Group := IntToStr(GID);
  end;

  if (aPermissions in ReqAttrs) or (ReqAttrs = []) then
    Permissions := ConvertSFTPValueToFilePermissions(StatBuf.st_mode);

  if (aAccessTime in ReqAttrs) or (ReqAttrs = []) then
    AccessTime := FileDateToDateTime(StatBuf.st_atime);
  if (aModifyTime in ReqAttrs) or (ReqAttrs = []) then
    ModifyTime := FileDateToDateTime(StatBuf.st_mtime);
{$IFNDEF FPC}
{$IFDEF MACOS}
  if (aCreateTime in ReqAttrs) or (ReqAttrs = []) then
    CreateTime := FileDateToDateTime({$IFDEF POSIX}StatBuf.st_birthtime{$ELSE}StatBuf.__unused2[0]{$ENDIF});
{$ENDIF}
{$ENDIF}
  if (aChangeAttrTime in ReqAttrs) or (ReqAttrs = []) then
    ChangeAttrTime := FileDateToDateTime(StatBuf.st_ctime);
end;
{$ENDIF}

procedure TScSFTPFileAttributes.RequestInfo(const Path: string; const FileRec: TSearchRec;
  const ReqAttrs: TScSFTPAttributes; const FollowSymLink: boolean);
var
{$IFDEF MSWINDOWS}
  SecDescr: PSecurityDescriptor;
  SizeNeeded: DWORD;
  FileAttrs: DWORD;
{$ELSE}
  APath: AnsiString;
  StatBuf: _stat;
{$ENDIF}
begin
  Clear;

  if (FileRec.Attr and faDirectory) <> 0 then
    FileType := ftDirectory
  else
  if (FileRec.Attr and {$IFDEF FPC}$00000400{$ELSE}faSymLink{$ENDIF}) <> 0 then
    FileType := ftSymlink
  else
    FileType := ftFile;

  if aSubsecondTimes in ReqAttrs then
    Include(FValidAttributes, aSubsecondTimes);

{$IFDEF MSWINDOWS}
  if (aSize in ReqAttrs) or (ReqAttrs = []) then begin
    Size := Int64(FileRec.FindData.nFileSizeHigh) shl 32 + Int64(FileRec.FindData.nFileSizeLow);
    if (aAllocationSize in ReqAttrs) or (ReqAttrs = []) then
      AllocationSize := Size;
  end;

  if (aAccessTime in ReqAttrs) or (ReqAttrs = []) then
    AccessTime := InternalFileTimeToDateTime(FileRec.FindData.ftLastAccessTime);
  if (aModifyTime in ReqAttrs) or (ReqAttrs = []) then
    ModifyTime := InternalFileTimeToDateTime(FileRec.FindData.ftLastWriteTime);
  if (aCreateTime in ReqAttrs) or (ReqAttrs = []) then
    CreateTime := InternalFileTimeToDateTime(FileRec.FindData.ftCreationTime);

  if (aLinkCount in ReqAttrs) or (ReqAttrs = []) then
    if FileType = ftDirectory then
      LinkCount := DirsCount(Path)
    else
      LinkCount := 1;

  if (aAttrs in ReqAttrs) or (ReqAttrs = []) then begin
    FileAttrs := GetFileAttributes(PChar(Path));
    if FileAttrs <> DWORD(INVALID_HANDLE_VALUE) then
      Attrs := ConvertOSValueToFileAttrs(FileAttrs) + [faCaseInsensitive];
  end;

  if (([aOwnerGroup, aPermissions, aAcl] * ReqAttrs) <> []) or (ReqAttrs = []) then begin
    SizeNeeded := 0;
    GetFileSecurity(PChar(Path), REQ_SEC_INFO, nil, 0, {$IFDEF FPC}@{$ENDIF}SizeNeeded);
    if SizeNeeded > 0 then begin
      SecDescr := Marshal.AllocHGlobal(SizeNeeded);
      try
        if GetFileSecurity(PChar(Path), REQ_SEC_INFO, SecDescr, SizeNeeded, {$IFDEF FPC}@{$ENDIF}SizeNeeded) then
          RequestSecurityInfo(Path, SecDescr, ReqAttrs);
      finally
        Marshal.FreeHGlobal(SecDescr);
      end;
    end;
  end;
{$ELSE}

  if (aSize in ReqAttrs) or (ReqAttrs = []) then
    Size := FileRec.Size;

  if (aAttrs in ReqAttrs) or (ReqAttrs = []) then
    Attrs := ConvertOSValueToFileAttrs(FileRec.Attr);

  APath := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Path));
  if (FollowSymLink and ({$IFDEF POSIX}stat{$ELSE}fpstat{$ENDIF}(PAnsiChar(APath), StatBuf) = 0)) or
     (not FollowSymLink and ({$IFDEF POSIX}lstat{$ELSE}fplstat{$ENDIF}(PAnsiChar(APath), StatBuf) = 0)) then
    RequestSysInfo(StatBuf, ReqAttrs);
{$ENDIF}
end;

{$IFDEF FPC}
const
  INVALID_FILE_SIZE = cardinal(-1);
{$ENDIF}

procedure TScSFTPFileAttributes.RequestInfoByHandle(const Handle: THandle; const ReqAttrs: TScSFTPAttributes);
var
{$IFDEF MSWINDOWS}
  SecDescr: PSecurityDescriptor;
  FileSizeLow, FileSizeHigh: DWORD;
  CreateFileTime, AccessFileTime, ModifyFileTime: TFileTime;
  FileInformation: TByHandleFileInformation;
{$ELSE}
  StatBuf: _stat;
{$ENDIF}
begin
  Clear;

  FileType := ftFile;

  if aSubsecondTimes in ReqAttrs then
    Include(FValidAttributes, aSubsecondTimes);

{$IFDEF MSWINDOWS}
  if GetFileInformationByHandle(Handle, FileInformation) then begin
    if (aSize in ReqAttrs) or (ReqAttrs = []) then begin
      Size := Int64(FileInformation.nFileSizeHigh) shl 32 + Int64(FileInformation.nFileSizeLow);
      if (aAllocationSize in ReqAttrs) or (ReqAttrs = []) then
        AllocationSize := Size;
    end;

    if (aAccessTime in ReqAttrs) or (ReqAttrs = []) then
      AccessTime := InternalFileTimeToDateTime(FileInformation.ftLastAccessTime);
    if (aModifyTime in ReqAttrs) or (ReqAttrs = []) then
      ModifyTime := InternalFileTimeToDateTime(FileInformation.ftLastWriteTime);
    if (aCreateTime in ReqAttrs) or (ReqAttrs = []) then
      CreateTime := InternalFileTimeToDateTime(FileInformation.ftCreationTime);

    if (aLinkCount in ReqAttrs) or (ReqAttrs = []) then
      LinkCount := FileInformation.nNumberOfLinks;

    if (aAttrs in ReqAttrs) or (ReqAttrs = []) then
      Attrs := ConvertOSValueToFileAttrs(FileInformation.dwFileAttributes) + [faCaseInsensitive];
  end
  else begin
    if (aSize in ReqAttrs) or (ReqAttrs = []) then begin
      FileSizeLow := GetFileSize(Handle, @FileSizeHigh);
      if FileSizeLow <> INVALID_FILE_SIZE then begin
        Size := Int64(FileSizeHigh) shl 32 + Int64(FileSizeLow);
        if (aAllocationSize in ReqAttrs) or (ReqAttrs = []) then
          AllocationSize := Size;
      end;
    end;

    if (([aAccessTime, aModifyTime, aCreateTime] * ReqAttrs) <> []) or (ReqAttrs = []) then
      if GetFileTime(Handle, @CreateFileTime, @AccessFileTime, @ModifyFileTime) then begin
        if (aAccessTime in ReqAttrs) or (ReqAttrs = []) then
          AccessTime := InternalFileTimeToDateTime(AccessFileTime);
        if (aModifyTime in ReqAttrs) or (ReqAttrs = []) then
          ModifyTime := InternalFileTimeToDateTime(ModifyFileTime);
        if (aCreateTime in ReqAttrs) or (ReqAttrs = []) then
          CreateTime := InternalFileTimeToDateTime(CreateFileTime);
      end;
  end;

  if (([aOwnerGroup, aPermissions, aAcl] * ReqAttrs) <> []) or (ReqAttrs = []) then begin
    if GetSecurityInfo(Handle, SE_FILE_OBJECT, REQ_SEC_INFO, nil, nil, nil, nil, SecDescr) = 0 then
      try
        RequestSecurityInfo('', SecDescr, ReqAttrs);
      finally
        LocalFree(HLOCAL(SecDescr));
      end;
  end;
{$ELSE}
  if {$IFDEF POSIX}fstat{$ELSE}fpfstat{$ENDIF}(Handle, StatBuf) = 0 then
    RequestSysInfo(StatBuf, ReqAttrs);
{$ENDIF}
end;

function TScSFTPFileAttributes.SetToFile(const Path: string): boolean;
var
  Handle: THandle;
{$IFDEF MSWINDOWS}
  DesiredAccess, Flags: DWORD;
{$ELSE}
  APath: AnsiString;
{$ENDIF}
begin
  if ValidAttributes = [] then begin
    Result := True;
    Exit;
  end;

  Result := False;

{$IFDEF MSWINDOWS}
  if DirectoryExists(Path) then
    Flags := FILE_FLAG_BACKUP_SEMANTICS
  else
    Flags := FILE_ATTRIBUTE_NORMAL;

  DesiredAccess := FILE_GENERIC_READ or FILE_GENERIC_WRITE;
  if aAcl in ValidAttributes then
    DesiredAccess := DesiredAccess or WRITE_DAC;
  if aOwnerGroup in ValidAttributes then
    DesiredAccess := DesiredAccess or WRITE_OWNER;

  Handle := CreateFile(PChar(Path), DesiredAccess, FILE_SHARE_READ, nil, OPEN_EXISTING, Flags, 0);
  if Handle = INVALID_HANDLE_VALUE then
    Exit;

  try
    Result := SetToFileByHandle(Path, Handle);
  finally
    CloseHandle(Handle);
  end;
{$ELSE}
  if not DirectoryExists(Path) then begin
    APath := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Path));
    Handle := THandle({$IFDEF POSIX}__open{$ELSE}fpOpen{$ENDIF}(PAnsiChar(APath), O_RDWR, S_IRUSR or S_IWUSR));
    if Handle = THandle(-1) then
      Exit;
  end
  else
    Handle := 0;

  Result := SetToFileByHandle(Path, Handle);
{$ENDIF}
end;

function TScSFTPFileAttributes.SetToFileByHandle(const Path: string; const Handle: THandle): boolean;
var
{$IFDEF MSWINDOWS}
  FileAttrs: DWORD;
  CreateFileTime, AccessFileTime, ModifyFileTime: TFileTime;
  pCreateFileTime, pAccessFileTime, pModifyFileTime: PFileTime;
{$ELSE}
  APath, AOwner, AGroup: AnsiString;
  StatBuf: _stat;
  TimeBuf: utimbuf;
  Flags: integer;
{$IFNDEF IOS}
{$IFNDEF ANDROID}
  PasswdP: Ppasswd;
  GroupP: Pgroup;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  if ValidAttributes = [] then begin
    Result := True;
    Exit;
  end;

  Result := False;

  if not DirectoryExists(Path) then begin
    if aAllocationSize in ValidAttributes then begin
      if FileSeek(NativeInt(Handle), AllocationSize, 0) = -1 then
        Exit;
    {$IFDEF MSWINDOWS}
      if not SetEndOfFile(Handle) then
        Exit;
    {$ELSE}
      if {$IFDEF POSIX}ftruncate{$ELSE}fpftruncate{$ENDIF}(Handle, AllocationSize) = -1 then
        Exit;
    {$ENDIF}

      if aSize in ValidAttributes then begin
        if Size > AllocationSize then //TODO: Result := SSH_FX_INVALID_PARAMETER
          Exit;
        if Size < AllocationSize then
          FileSeek(NativeInt(Handle), Size, 0);
      end;
    end
    else
    if aSize in ValidAttributes then begin
      if FileSeek(NativeInt(Handle), Size, 0) = -1 then
        Exit;
    {$IFDEF MSWINDOWS}
      if not SetEndOfFile(Handle) then
    {$ELSE}
      if {$IFDEF POSIX}ftruncate{$ELSE}fpftruncate{$ENDIF}(Handle, Size) = -1 then
        Exit;
    {$ENDIF}
    end;
  end;

{$IFDEF MSWINDOWS}
  if aAttrs in ValidAttributes then begin
    FileAttrs := ConvertFileAttrsToOSValue(Attrs);
    if FileAttrs <> 0 then
      if not SetFileAttributes(PChar(Path), FileAttrs) then
        Exit;
  end;

  if ([aCreateTime, aAccessTime, aModifyTime] * ValidAttributes) <> [] then begin
    pCreateFileTime := nil;
    pAccessFileTime := nil;
    pModifyFileTime := nil;
    if aCreateTime in ValidAttributes then begin
      if not InternalDateTimeToFileTime(CreateTime, CreateFileTime) then
        Exit;
      pCreateFileTime := @CreateFileTime;
    end;

    if aAccessTime in ValidAttributes then begin
      if not InternalDateTimeToFileTime(AccessTime, AccessFileTime) then
        Exit;
      pAccessFileTime := @AccessFileTime;
    end;

    if aModifyTime in ValidAttributes then begin
      if not InternalDateTimeToFileTime(ModifyTime, ModifyFileTime) then
        Exit;
      pModifyFileTime := @ModifyFileTime;
    end;

    if not SetFileTime(Handle, pCreateFileTime, pAccessFileTime, pModifyFileTime) then
      Exit;
  end;

  if not SetSecurityInfo(Handle) then
    Exit;
{$ELSE}
  APath := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Path));

  if ([aAccessTime, aModifyTime] * ValidAttributes) <> [] then begin
    if {$IFDEF POSIX}stat{$ELSE}fpstat{$ENDIF}(PAnsiChar(APath), StatBuf) <> 0 then
      Exit;

    if aAccessTime in ValidAttributes then
      InternalDateTimeToFileTime(AccessTime, TimeBuf.actime)
    else
      TimeBuf.actime := StatBuf.st_atime;

    if aModifyTime in ValidAttributes then
      InternalDateTimeToFileTime(ModifyTime, TimeBuf.modtime)
    else
      TimeBuf.modtime := StatBuf.st_mtime;

    if {$IFDEF POSIX}utime{$ELSE}fputime{$ENDIF}(PAnsiChar(APath), {$IFNDEF POSIX}@{$ENDIF}TimeBuf) <> 0 then
      Exit;
  end;

{$IFDEF POSIX}
  if aAttrs in ValidAttributes then begin
    if faReadonly in Attrs then
      FileSetReadOnly(Path, True);
  end;
{$ENDIF}

  if aPermissions in ValidAttributes then begin
    Flags := ConvertFilePermissionsToSFTPValue(Permissions);
    if {$IFDEF POSIX}chmod{$ELSE}fpchmod{$ENDIF}(PAnsiChar(APath), Flags) <> 0 then
      Exit;
  end;

  if aOwnerGroup in ValidAttributes then begin
    if (UID = 0) and (Owner <> '') then begin
      AOwner := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Owner));
    {$IFNDEF IOS}
    {$IFNDEF ANDROID}
      PasswdP := {$IFDEF POSIX}getpwnam{$ELSE}fpgetpwnam{$ENDIF}(PAnsiChar(AOwner));
      if PasswdP <> nil then
        UID := PasswdP.pw_uid;
    {$ENDIF}
    {$ENDIF}
    end;

    if (GID = 0) and (Group <> '') then begin
      AGroup := ScFunctions.UTF8Encode({$IFDEF FPC}WideString{$ENDIF}(Group));
    {$IFNDEF IOS}
    {$IFNDEF ANDROID}
      GroupP := {$IFDEF POSIX}getgrnam{$ELSE}fpgetgrnam{$ENDIF}(PAnsiChar(AGroup));
      if GroupP <> nil then
        GID := GroupP.gr_gid;
    {$ENDIF}
    {$ENDIF}
    end;

    if {$IFDEF POSIX}lchown{$ELSE}fpchown{$ENDIF}(PAnsiChar(APath), UID, GID) <> 0 then
      Exit;
  end;
{$ENDIF}

  Result := True; //TODO: process and return errors
end;

procedure TScSFTPFileAttributes.OnChanged(Sender: TObject);
begin
  if Sender = FACEs then
    Include(FValidAttributes, aAcl);
end;

procedure TScSFTPFileAttributes.SetSize(Value: Int64);
begin
  FSize := Value;
  Include(FValidAttributes, aSize);
end;

procedure TScSFTPFileAttributes.SetAllocationSize(Value: Int64);
begin
  FAllocationSize := Value;
  Include(FValidAttributes, aAllocationSize);
end;

procedure TScSFTPFileAttributes.SetGID(Value: integer);
begin
  FGID := Value;
  Include(FValidAttributes, aOwnerGroup);
end;

procedure TScSFTPFileAttributes.SetUID(Value: integer);
begin
  FUID := Value;
  Include(FValidAttributes, aOwnerGroup);
end;

procedure TScSFTPFileAttributes.SetGroup(const Value: string);
begin
  FGroup := Value;
  Include(FValidAttributes, aOwnerGroup);
end;

procedure TScSFTPFileAttributes.SetOwner(const Value: string);
begin
  FOwner := Value;
  Include(FValidAttributes, aOwnerGroup);
end;

procedure TScSFTPFileAttributes.SetPermissions(const Value: TScSFTPFilePermissions);
begin
  FPermissions := Value;
  Include(FValidAttributes, aPermissions);
end;

procedure TScSFTPFileAttributes.SetAccessTime(Value: TDateTime);
begin
  FAccessTime := Value;
  Include(FValidAttributes, aAccessTime);
end;

procedure TScSFTPFileAttributes.SetCreateTime(Value: TDateTime);
begin
  FCreateTime := Value;
  Include(FValidAttributes, aCreateTime);
end;

procedure TScSFTPFileAttributes.SetModifyTime(Value: TDateTime);
begin
  FModifyTime := Value;
  Include(FValidAttributes, aModifyTime);
end;

procedure TScSFTPFileAttributes.SetChangeAttrTime(Value: TDateTime);
begin
  FChangeAttrTime := Value;
  Include(FValidAttributes, aChangeAttrTime);
end;

procedure TScSFTPFileAttributes.SetAclFlags(const Value: TScSFTPAclFlags);
begin
  FAclFlags := Value;
  Include(FValidAttributes, aAcl);
end;

procedure TScSFTPFileAttributes.SetACEs(Value: TScSFTPACEs);
begin
  if FACEs <> Value then begin
    FACEs.Assign(Value);
    Include(FValidAttributes, aAcl);
  end;
end;

procedure TScSFTPFileAttributes.SetAttrs(const Value: TScSFTPFileAttrs);
begin
  FAttrs := Value;
  Include(FValidAttributes, aAttrs);
end;

procedure TScSFTPFileAttributes.SetTextHint(Value: TScSFTPTextHint);
begin
  FTextHint := Value;
  Include(FValidAttributes, aTextHint);
end;

procedure TScSFTPFileAttributes.SetMimeType(const Value: string);
begin
  FMimeType := Value;
  Include(FValidAttributes, aMimeType);
end;

procedure TScSFTPFileAttributes.SetLinkCount(Value: integer);
begin
  FLinkCount := Value;
  Include(FValidAttributes, aLinkCount);
end;

procedure TScSFTPFileAttributes.SetUntranslatedName(const Value: string);
begin
  FUntranslatedName := Value;
  Include(FValidAttributes, aUntranslatedName);
end;

procedure TScSFTPFileAttributes.SetExtendedAttributes(Value: TCRObjectList);
begin
{$IFNDEF NEXTGEN}
  if FExtendedAttributes <> Value then begin
    FExtendedAttributes.Assign(Value);
    Include(FValidAttributes, aExtended);
  end;
{$ENDIF}
end;

{ TScSFTPFileInfo }

constructor TScSFTPFileInfo.Create;
begin
  inherited;
  FAttributes := TScSFTPFileAttributes.Create;
end;

destructor TScSFTPFileInfo.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

procedure TScSFTPFileInfo.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSFTPFileInfo) then begin
    TScSFTPFileInfo(Dest).FFilename := FFilename;
    TScSFTPFileInfo(Dest).FLongname := FLongname;
    TScSFTPFileInfo(Dest).FAttributes.Assign(FAttributes);
  end
  else
    inherited;
end;

function TScSFTPFileInfo.GetLongname: string;
begin
  Result := FLongname;
  if Result = '' then
    Result := FAttributes.GetAttributesAsLongname + ' ' + FFilename;
end;

procedure TScSFTPFileInfo.FillInfo(const Path: string; const FileRec: TSearchRec);
var
  FullFileName: string;
begin
  FFilename := FileRec.Name;
  FLongname := '';

  FullFileName := IncludeTrailingBackslash(Path) + FileRec.Name;
  FAttributes.RequestInfo(FullFileName, FileRec, [], False);
end;

{ TScSFTPUtils }

class function TScSFTPUtils.DecodeBytes(const Data: TBytes; const UseUnicode: boolean): string;
begin
  if UseUnicode then
    Result := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Data))
  else
    Result := Encoding.Default.GetString(Data);
end;

class function TScSFTPUtils.EncodeString(const Str: string; const UseUnicode: boolean): TBytes;
var
  WStr: WideString;
begin
  SetLength(Result, 0);

  if Length(Str) > 0 then begin
    if UseUnicode then begin
      WStr := WideString(Str);
      Result := Encoding.UTF8.GetBytes(WStr);
    end
    else
      Result := Encoding.Default.GetBytes(Str);
  end;
end;

{ TScSFTPExtensionUtils }

class procedure TScSFTPExtensionUtils.Clear(Obj: TScSFTPCustomExtension);
begin
  Obj.Clear;
end;

class procedure TScSFTPExtensionUtils.ParseExtension(Obj: TScSFTPCustomExtension;
  const ExtName: string; const ExtData: TBytes);
begin
  Obj.ParseExtension(ExtName, ExtData);
end;

{ TScSFTPSupportedExtensionUtils }

class procedure TScSFTPSupportedExtensionUtils.CheckSupportedOpenFlags(
  Obj: TScSFTPSupportedExtension; var Value: integer);
begin
  Obj.CheckSupportedOpenFlags(Value);
end;

class procedure TScSFTPSupportedExtensionUtils.CheckExtensionName(
  Obj: TScSFTPSupportedExtension; const Name: string);
begin
  Obj.CheckExtensionName(Name);
end;

class procedure TScSFTPSupportedExtensionUtils.CheckSupportedOpenBlockSet(
  Obj: TScSFTPSupportedExtension; const BlockModes: TScSFTPBlockModes);
begin
  Obj.CheckSupportedOpenBlockSet(BlockModes);
end;

class procedure TScSFTPSupportedExtensionUtils.CheckSupportedBlockSet(
  Obj: TScSFTPSupportedExtension; const BlockModes: TScSFTPBlockModes);
begin
  Obj.CheckSupportedBlockSet(BlockModes);
end;

{ TScSFTPFileAttributesUtils }

class procedure TScSFTPFileAttributesUtils.WriteToPacketDefaultValues(
  SftpPacket: TSFTPPacketWriter; const FileType: TScSFTPFileType; Version: integer);
begin
  TScSFTPFileAttributes.WriteToPacketDefaultValues(SftpPacket, FileType, Version);
end;

class procedure TScSFTPFileAttributesUtils.WriteToPacket(
  Obj: TScSFTPFileAttributes; SftpPacket: TSFTPPacketWriter; Version: integer;
  SupportedExtension: TScSFTPSupportedExtension);
begin
  Obj.WriteToPacket(SftpPacket, Version, SupportedExtension);
end;

class procedure TScSFTPFileAttributesUtils.ReadFromPacket(
  Obj: TScSFTPFileAttributes; SftpPacket: TSFTPPacketReader; Version: integer);
begin
  Obj.ReadFromPacket(SftpPacket, Version);
end;

class procedure TScSFTPFileAttributesUtils.RequestInfo(Obj: TScSFTPFileAttributes;
  const Path: string; const FileRec: TSearchRec; const ReqAttrs: TScSFTPAttributes;
  const FollowSymLink: boolean);
begin
  Obj.RequestInfo(Path, FileRec, ReqAttrs, FollowSymLink);
end;

class procedure TScSFTPFileAttributesUtils.RequestInfoByHandle(Obj: TScSFTPFileAttributes;
  const Handle: THandle; const ReqAttrs: TScSFTPAttributes);
begin
  Obj.RequestInfoByHandle(Handle, ReqAttrs);
end;

class function TScSFTPFileAttributesUtils.SetToFile(Obj: TScSFTPFileAttributes;
  const Path: string): boolean;
begin
  Result := Obj.SetToFile(Path);
end;

class function TScSFTPFileAttributesUtils.SetToFileByHandle(Obj: TScSFTPFileAttributes;
  const Path: string; const Handle: THandle): boolean;
begin
  Result := Obj.SetToFileByHandle(Path, Handle);
end;

class procedure TScSFTPFileAttributesUtils.SetRequestFileSecurityAttributes(Obj: TScSFTPFileAttributes; RequestFileSecurityAttributes: TScRequestFileSecurityAttributes);
begin
  Obj.FRequestFileSecurityAttributes := RequestFileSecurityAttributes;
end;

initialization
{$IFDEF VER7P}
  with SFTPFormatSettings do
  begin
    TimeSeparator := ':';
    ShortDateFormat := 'mmm dd';
    LongTimeFormat := 'hh:mm';
{$ELSE}
  begin
{$ENDIF}
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[1] := 'Jan';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[2] := 'Feb';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[3] := 'Mar';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[4] := 'Apr';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[5] := 'May';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[6] := 'Jun';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[7] := 'Jul';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[8] := 'Aug';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[9] := 'Sep';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[10] := 'Oct';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[11] := 'Nov';
    {$IFDEF FPC}DefaultFormatSettings.{$ENDIF}ShortMonthNames[12] := 'Dec';
  end;

{$IFDEF MSWINDOWS}
  LoadFuncs;

finalization
  FreeFuncs;
{$ENDIF}

end.

