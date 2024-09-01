
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScFTPListParser;

interface

uses
  Classes, SysUtils, Types,
  ScCLRClasses, ScTypes, ScFunctions, ScUtils, ScFTPUtils;

type
  TScFTPListItemClass = class of TScFTPListItem;
  TScFTPListParserClass = class of TScFTPListParser;

  TScFTPFileType = (fftDirectory, fftFile, fftSymLink, fftSymLinkDir,
    fftBlockDevice, fftCharDevice, fftFifo, fftSocket);

  TScFTPListType = (ltList, ltNameList, ltMList);

  TScFTPListItem = class(TCollectionItem)
  protected
    FData: string;
    FSize: Int64;
    FOwnerName: string;
    FGroupName: string;
    FFileName: string;
    FLocalFileName: string;
    FModifiedDate: TDateTime;
    FModifiedDateGMT: TDateTime;
    FModifiedAvailable: boolean;
    FSizeAvailable: boolean;
    FBlockSize: integer;
    FNumberBlocks: integer;
    FFileType: TScFTPFileType;
    FPermissions: string;
    FPermissionsDisplay: string;
    procedure SetFileName(const Value: string);

  public
    constructor Create(Owner: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    property Data: string read FData write FData;
    property Size: Int64 read FSize write FSize;
    property OwnerName: string read FOwnerName write FOwnerName;
    property GroupName: string read FGroupName write FGroupName;
    property FileName: string read FFileName write SetFileName;
    property LocalFileName: string read FLocalFileName write FLocalFileName;
    property FileType: TScFTPFileType read FFileType write FFileType;
    property ModifiedDate: TDateTime read FModifiedDate write FModifiedDate;
    property ModifiedDateGMT: TDateTime read FModifiedDateGMT write FModifiedDateGMT;
    property ModifiedAvailable: boolean read FModifiedAvailable write FModifiedAvailable;
    property SizeAvailable: boolean read FSizeAvailable write FSizeAvailable;
    property BlockSize: integer read FBlockSize write FBlockSize;
    property NumberBlocks: integer read FNumberBlocks write FNumberBlocks;
    property Permissions: string read FPermissions write FPermissions;
    property PermissionsDisplay: string read FPermissionsDisplay write FPermissionsDisplay;
  end;

  TScFTPDirectoryListing = class(TCollection)
  private
    function GetItem(Index: integer): TScFTPListItem;
    procedure SetItem(Index: integer; Value: TScFTPListItem);
  public
    constructor Create; reintroduce;
    function Add: TScFTPListItem;
    function IndexOf(Item: TScFTPListItem): integer;
    property Items[Index: integer]: TScFTPListItem read GetItem write SetItem; default;
  end;

  TScFTPListParser = class
  protected
    class function GetItemClassType: TScFTPListItemClass; virtual;
    class function CreateItem(Owner: TScFTPDirectoryListing): TScFTPListItem;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; virtual;

    class function IsIEFile(const Str: string): boolean;
    class function IsTotalLine(const Data: string): boolean;

  public
    class function Identitifier: string; virtual;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; virtual;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; virtual;
  end;

  TScFTPListHeaderParser = class(TScFTPListParser)
  protected
    class function IsHeader(const Data: string): boolean; virtual;
    class function IsFooter(const Data: string): boolean; virtual;
  public
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_NListParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_CreationDateListItem = class(TScFTPListItem)
  protected
    FCreationDate: TDateTime;
  public
    constructor Create(Owner: TCollection); override;
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
  end;

  TScFTP_MListItem = class(TScFTP_CreationDateListItem)
  protected
    FAttributes: TScWin32Attributes;
    FAttributesAvailable: boolean;
    FCreationDateGMT: TDateTime;
    FLastAccessDate: TDateTime;
    FLastAccessDateGMT: TDateTime;
    FLinkedItemName: string;
    FUniqueID: string;
    function GetFact(const Name: string): string;
  public
    constructor Create(Owner: TCollection); override;
    destructor Destroy; override;

    property CreationDateGMT: TDateTime read FCreationDateGMT write FCreationDateGMT;
    property LastAccessDate: TDateTime read FLastAccessDate write FLastAccessDate;
    property LastAccessDateGMT: TDateTime read FLastAccessDateGMT write FLastAccessDateGMT;

    property UniqueID: string read FUniqueID write FUniqueID;
    property Facts[const Name: string]: string read GetFact;
    property Attributes: TScWin32Attributes read FAttributes;
    property AttributesAvailable: boolean read FAttributesAvailable write FAttributesAvailable;
    property LinkedItemName: string read FLinkedItemName write FLinkedItemName;
  end;

  TScFTP_MListParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseFacts(const Data: string; Strings: TStrings): string;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_AS400Parser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_XecomMicroRTOSListItem = class(TScFTPListItem)
  protected
    FMemStart: cardinal;
    FMemEnd: cardinal;
  public
    constructor Create(Owner: TCollection); override;
    property MemStart: cardinal read FMemStart write FMemStart;
    property MemEnd: cardinal read FMemEnd write FMemEnd;
  end;

  TScFTP_XercomMicroRTOSParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean;  override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_WinQVNetParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_WindowsNTParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function IsSubDirContentBanner(const Value: string): boolean;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_WfFTPParser = class(TScFTPListHeaderParser)
  protected
    class function IsHeader(const Data: string): boolean; override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_VxWorksParser = class(TScFTPListHeaderParser)
  protected
    class function IsHeader(const Data: string): boolean;  override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_SimpleListItem = class(TScFTPListItem)
  public
    constructor Create(Owner: TCollection); override;
  end;

  TScFTPVSEPQDisposition = (
    pqdAppendable,
    pqdProcessAndDelete,
    pqdHoldUntilReleased,
    pqdProcessAndKeep,
    pqdLeaveUntilReleased,
    pqdErrorHoldUntilDK,
    pqdGetOrErrorHoldUntilDK,
    pqdJobProcessing,
    pqdSpoolOutputToInputD,
    pqdSurpressOutputSpooling,
    pqdSpoolOutputToTape);

  TScFTP_VSEPowerQueueListItem = class(TScFTPListItem)
  protected
    FDisposition: TScFTPVSEPQDisposition;
    FPriority: integer;
    FNumberRecs: integer;
  public
    property NumberRecs: integer read FNumberRecs write FNumberRecs;
    property Disposition: TScFTPVSEPQDisposition read FDisposition write FDisposition;
    property Priority: integer read FPriority write FPriority;
  end;

  TScFTP_VSESublibraryListItem = class(TScFTPListItem)
  protected
    FNumberRecs: integer;
    FCreationDate: TDateTime;
  public
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property NumberRecs: integer read FNumberRecs write FNumberRecs;
  end;

  TScFTP_VSESublibraryParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VSERootDirParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VSELibraryParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VSEVSAMCatalogParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VSEVTOCListItem = class(TScFTPListItem)
  public
    constructor Create(Owner: TCollection); override;
  end;

  TScFTP_VSEVTOCParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VSEPowerQueueParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VMSListItem = class(TScFTPListItem)
  protected
    FSystemPermissions: string;
    FOwnerPermissions: string;
    FGroupPermissions: string;
    FWorldPermissions: string;
    FVersion: integer;
  public
    property SystemPermissions: string read FSystemPermissions write FSystemPermissions;
    property OwnerPermissions: string read FOwnerPermissions write FOwnerPermissions;
    property GroupPermissions: string read FGroupPermissions write FGroupPermissions;
    property WorldPermissions: string read FWorldPermissions write FWorldPermissions;
    property Version: integer read FVersion write FVersion;
  end;

  TScFTP_VMSParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsVMSHeader(const Data: string): boolean;
    class function IsVMSFooter(const Data: string): boolean;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_RecListItem = class(TScFTPListItem)
  protected
    FRecLength: integer;
    FRecFormat: string;
    FNumberRecs: integer;
  public
    property RecLength: integer read FRecLength write FRecLength;
    property RecFormat: string read FRecFormat write FRecFormat;
    property NumberRecs: integer read FNumberRecs write FNumberRecs;
  end;

  TScFTP_VMVirtualReaderListItem = class(TScFTPListItem)
  protected
    FNumberRecs: integer;
  public
    constructor Create(Owner: TCollection); override;
    property NumberRecs: integer read FNumberRecs write FNumberRecs;
  end;

  TScFTP_VMCMSParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VMBFSParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_VirtualReaderParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_UnixPermListItem = class(TScFTPListItem)
  protected
    FGroupPermissions: string;
    FOwnerPermissions: string;
    FOtherPermissions: string;
  public
    property OwnerPermissions: string read FOwnerPermissions write FOwnerPermissions;
    property GroupPermissions: string read FGroupPermissions write FGroupPermissions;
    property OtherPermissions: string read FOtherPermissions write FOtherPermissions;
  end;

  TScFTP_UnixBaseListItem = class(TScFTP_UnixPermListItem)
  protected
    FInode: integer;
    FLinkCount: integer;
    FLinkedItemName: string;
  public
    property Inode: integer read FInode write FInode;
    property LinkCount: integer read FLinkCount write FLinkCount;
    property LinkedItemName: string read FLinkedItemName write FLinkedItemName;
  end;

  TScFTP_UnitreeListItem = class(TScFTP_UnixBaseListItem)
  protected
    FMigrated: boolean;
    FFileFamily: string;
  public
    property Migrated: boolean read FMigrated write FMigrated;
    property FileFamily: string read FFileFamily write FFileFamily;
  end;

  TScFTP_UnixParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsUnixStr(const Data: string): boolean;
    class function IsUnitreeBanner(const Data: string): boolean;
    class function IsSubDirContentBanner(const Value: string): boolean;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_UnitreeParser = class(TScFTP_UnixParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_UnisysClearpathListItem = class(TScFTP_CreationDateListItem)
  protected
    FFileKind: string;
  public
    property FileKind: string read FFileKind write FFileKind;
  end;

  TScFTP_UnisysClearpathParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_TSXPlusParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_TOPS20Parser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_TandemGuardianListItem = class(TScFTPListItem)
  protected
    FCode: cardinal;
  public
    property Code: cardinal read FCode write FCode;
  end;

  TScFTP_TandemGuardianParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_SuperTCPListItem = class(TScFTPListItem)
  protected
    FShortFileName: string;
  public
    property ShortFileName: string read FShortFileName write FShortFileName;
  end;

  TScFTP_SuperTCPParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsValidWin32FileName(const FileName: string): boolean;
    class function IsValidMSDOSFileName(const FileName: string): boolean;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_StratusVOSListItem = class(TScFTPListItem)
  protected
    FAccess: string;
    FFileFormat: string;
    FLinkedItemName: string;
  public
    property Access: string read FAccess write FAccess;
    property FileFormat: string read FFileFormat write FFileFormat;
    property LinkedItemName: string read FLinkedItemName write FLinkedItemName;
  end;

  TScFTP_StratusVOSParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsValidFileEntry(const Line: string): boolean;
    class function IsValidDirEntry(const Line: string): boolean;
    class function IsFilesHeader(const Line: string): boolean;
    class function IsDirsHeader(const Line: string): boolean;
    class function IsLinksHeader(const Line: string): boolean;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_SterCommEntUxListItem = class(TScFTPListItem)
  protected
    FProtocolFlags: string;
    FProtocolIndicator: string;
  public
    property ProtocolFlags: string read FProtocolFlags write FProtocolFlags;
    property ProtocolIndicator: string read FProtocolIndicator write FProtocolIndicator;
  end;

  TScFTP_SterComEntParser = class(TScFTPListHeaderParser)
  protected
    class function IsFooter(const Data: string): boolean; override;
  end;

  TScFTP_SterCommEntUxParser = class(TScFTP_SterComEntParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function IsValidFlags(const Value: string): boolean;
    class function IsValidPattern(const Raw: string; const Patterns: array of string): boolean;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_SterCommEntUxNSParser = class(TScFTP_SterComEntParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function IsValidDate(const StrDate: string): boolean;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_SterCommEntUxRootParser = class(TScFTP_SterComEntParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_SterCommExpOS390ListItem = class(TScFTPListItem)
  protected
    FRecFormat: string;
    FRecLength: integer;
  public
    property RecFormat: string read FRecFormat write FRecFormat;
    property RecLength: integer read FRecLength write FRecLength;
  end;

  TScFTP_SterCommExpOS390Parser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_PCTCPFtpsrvParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_PCNFSDParser = class(TScFTPListParser)
  protected
    class function CheckLine(const Data: string): boolean;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_DOSBaseListItem = class(TScFTPListItem)
  protected
    FAttributes: TScDOSAttributes;
    procedure SetAttributes(Attributes: TScDOSAttributes);
  public
    constructor Create(Owner: TCollection); override;
    destructor Destroy; override;
    property Attributes: TScDOSAttributes read FAttributes write SetAttributes;
  end;

  TScFTP_OS2Parser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsValidAttr(const Attr: string): boolean;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_NetwarePSUDosParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_NetwarePSUNFSParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_NovellNetwareParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function IsNovellPattern(Value: string): boolean;
    class function IsValidNovellPermissions(const Value: string): boolean;
    class function ExtractNovellPermissions(const Value: string): string;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_NCSAforMACOSParser = class(TScFTP_NListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_NCSAforDOSParser = class(TScFTPListHeaderParser)
  protected
    class function IsHeader(const Data: string): boolean; override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_MVSListItem = class(TScFTP_RecListItem)
  protected
    FMigrated: boolean;
    FVolume: string;
    FUnits: string;
    FOrganization: string;
    FNumberExtents: integer;
    FNumberTracks: integer;
  public
    constructor Create(Owner: TCollection); override;
    property Migrated: boolean read FMigrated write FMigrated;
    property Volume: string read FVolume write FVolume;
    property Units: string read FUnits write FUnits;
    property Organization: string read FOrganization write FOrganization;
    property NumberExtents: integer read FNumberExtents write FNumberExtents;
    property NumberTracks: integer read FNumberTracks write FNumberTracks;
  end;

  TScFTPMVSJESJobStatus = (jsJESNotApplicable, jsJESReceived, jsJESHold, jsJESRunning, jsJESOuptutAvailable);

  TScFTP_MVSJESListItem = class(TScFTPListItem)
  protected
    FJobStatus: TScFTPMVSJESJobStatus;
    FJobSpoolFiles: integer;
    FDetails: TStrings;
  public
    constructor Create(Owner: TCollection); override;
    destructor Destroy; override;
    property Details: TStrings read FDetails;
    property JobStatus: TScFTPMVSJESJobStatus read FJobStatus write FJobStatus;
    property JobSpoolFiles: integer read FJobSpoolFiles write FJobSpoolFiles;
  end;

  TScFTP_MVSParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function MVSStrToDate(const Value: string): TDateTime;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_MVSPartitionedDataSetParser = class(TScFTPListHeaderParser)
  protected
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_MVSJESInterface1Parser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_MVSJESInterface2Parser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
    class function Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean; override;
  end;

  TScFTP_MusicParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_MPEiXListItem = class(TScFTP_RecListItem)
  protected
    FLimit: cardinal;
  public
    constructor Create(Owner: TCollection); override;
    property Limit: cardinal read FLimit write FLimit;
  end;

  TScFTP_MPEiXBaseParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsExtHeader(Cols: TStrings): boolean;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_MPEiXParser = class(TScFTP_MPEiXBaseParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function IsHeader(const Data: string): boolean;  override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_MPEiXWithPOSIXParser = class(TScFTP_MPEiXBaseParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function IsHeader(const Data: string): boolean;  override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_MicrowareOS9ListItem = class(TScFTPListItem)
  protected
    FOwnerPermissions: string;
    FPublicPermissions: string;
    FMiscPermissions: string;
    FSector: cardinal;
  public
    property OwnerPermissions: string read FOwnerPermissions write FOwnerPermissions;
    property PublicPermissions: string read FPublicPermissions write FPublicPermissions;
    property MiscPermissions: string read FMiscPermissions write FMiscPermissions;
    property Sector: cardinal read FSector write FSector;
  end;

  TScFTP_MicrowareOS9Parser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean;  override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_IEFTPGatewayLsLongListItem = class(TScFTPListItem)
  protected
    FSenderAccount: string;
    FSenderUserID: string;
    FSenderUserClass: string;
  public
    property SenderAccount: string read FSenderAccount write FSenderAccount;
    property SenderUserID: string read FSenderUserID write FSenderUserID;
    property SenderUserClass: string read FSenderUserClass write FSenderUserClass;
  end;

  TScFTP_IEFTPGatewayLsFileNameListItem = class(TScFTP_SimpleListItem)
  protected
    FOriginalFileName: string;
  public
    property OriginalFileName: string read FOriginalFileName write FOriginalFileName;
  end;

  TScFTP_IEFTPGatewayLSLibraryListItem = class(TScFTP_UnixPermListItem)
  protected
    FAccount: string;
  public
    property Account: string read FAccount write FAccount;
  end;

  TScFTP_IEFTPGatewayLSLongParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_IEFTPGatewayLSShortParser = class(TScFTP_NListParser)
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_IEFTPGatewayLSFileNameParser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
    class function GetItemClassType: TScFTPListItemClass; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_IEFTPGatewayLSLibraryParser = class(TScFTPListHeaderParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function IsHeader(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
  end;

  TScFTP_KA9QParser = class(TScFTPListHeaderParser)
  protected
    class function IsHeader(const Data: string): boolean; override;
    class function IsFooter(const Data: string): boolean; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_HellSoftParser = class(TScFTP_NovellNetwareParser)
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_EPLFListItem = class(TScFTPListItem)
  protected
    FUniqueID: string;
  public
    property UniqueID: string read FUniqueID write FUniqueID;
  end;

  TScFTP_EPLFParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_DistinctTCPIPParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_CiscoIOSParser = class(TScFTP_NListParser)
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_ChameleonNewtParser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_BullGCOS8Parser = class(TScFTPListParser)
  protected
    class function GetItemClassType: TScFTPListItemClass; override;
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

  TScFTP_BullGCOS7Parser = class(TScFTPListParser)
  protected
    class function ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean; override;
  public
    class function Identitifier: string; override;
    class function CheckIfListing(Listing: TStrings; const SysDescript: string = ''): boolean; override;
  end;

function TryParseListing(Listing: TStrings; Dir: TScFTPDirectoryListing;
  const SysDescript: string; const ListType: TScFTPListType; out Format: string): boolean;

implementation

var
  PARSER_LIST: array[0..55] of TScFTPListParserClass = (
    TScFTP_AS400Parser, TScFTP_XercomMicroRTOSParser, TScFTP_WinQVNetParser,
    TScFTP_WindowsNTParser, TScFTP_WfFTPParser, TScFTP_VxWorksParser,
    TScFTP_VSELibraryParser, TScFTP_VSEPowerQueueParser, TScFTP_VSERootDirParser,
    TScFTP_VSESublibraryParser, TScFTP_VSEVSAMCatalogParser, TScFTP_VSEVTOCParser,
    TScFTP_VMSParser, TScFTP_VirtualReaderParser, TScFTP_VMBFSParser,
    TScFTP_VMCMSParser, TScFTP_UnixParser, TScFTP_UnitreeParser, TScFTP_UnisysClearpathParser,
    TScFTP_TSXPlusParser, TScFTP_TOPS20Parser, TScFTP_TandemGuardianParser,
    TScFTP_SuperTCPParser, TScFTP_StratusVOSParser, TScFTP_SterCommEntUxParser,
    TScFTP_SterCommEntUxNSParser, TScFTP_SterCommEntUxRootParser, TScFTP_SterCommExpOS390Parser,
    TScFTP_PCTCPFtpsrvParser, TScFTP_PCNFSDParser, TScFTP_OS2Parser,
    TScFTP_NetwarePSUDosParser, TScFTP_NetwarePSUNFSParser, TScFTP_NovellNetwareParser,
    TScFTP_NCSAforMACOSParser, TScFTP_NCSAforDOSParser, TScFTP_MVSParser,
    TScFTP_MVSPartitionedDataSetParser, TScFTP_MVSJESInterface1Parser,
    TScFTP_MVSJESInterface2Parser, TScFTP_MusicParser, TScFTP_MPEiXParser,
    TScFTP_MPEiXWithPOSIXParser, TScFTP_MicrowareOS9Parser, TScFTP_IEFTPGatewayLSLongParser,
    TScFTP_IEFTPGatewayLSShortParser, TScFTP_IEFTPGatewayLSFileNameParser,
    TScFTP_IEFTPGatewayLSLibraryParser, TScFTP_KA9QParser, TScFTP_HellSoftParser,
    TScFTP_EPLFParser, TScFTP_DistinctTCPIPParser, TScFTP_CiscoIOSParser,
    TScFTP_ChameleonNewtParser, TScFTP_BullGCOS8Parser, TScFTP_BullGCOS7Parser
  );

function TryParseListing(Listing: TStrings; Dir: TScFTPDirectoryListing;
  const SysDescript: string; const ListType: TScFTPListType; out Format: string): boolean;
var
  ParserClass: TScFTPListParserClass;
  i: integer;
begin
  Dir.Clear;

  case ListType of
    ltMList: begin
      Format := 'MLST';
      Result := TScFTP_MListParser.Parse(Listing, Dir);
    end;
    ltNameList: begin
      Format := 'NLST';
      Result := TScFTP_NListParser.Parse(Listing, Dir);
    end;
    ltList: begin
      ParserClass := nil;
      for i := Low(PARSER_LIST) to High(PARSER_LIST) do begin
        if PARSER_LIST[i].CheckIfListing(Listing, SysDescript) then begin
          ParserClass := PARSER_LIST[i];
          Break;
        end;
      end;

      Result := Assigned(ParserClass);
      if Result then begin
        Format := ParserClass.Identitifier;
        Result := ParserClass.Parse(Listing, Dir);
      end
      else
        Format := '';
    end;
  else
    Result := False;
    Assert(False);
  end;
end;

{ TScFTPListItem }

constructor TScFTPListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);

  FData := '';
  FFileType := fftFile;
  FSize := 0;
  FModifiedDate := 0.0;
  FFileName := '';
  FLocalFileName := '';
  FModifiedAvailable := True;
  FSizeAvailable := True;
end;

procedure TScFTPListItem.Assign(Source: TPersistent);
var
  Src: TScFTPListItem;
begin
  if Source is TScFTPListItem then begin
    Src := TScFTPListItem(Source);
    FData := Src.FData;
    FSize := Src.FSize;
    FOwnerName := Src.FOwnerName;
    FGroupName := Src.FGroupName;
    FFileName := Src.FFileName;
    FLocalFileName := Src.FLocalFileName;
    FFileType := Src.FFileType;
    FModifiedDate := Src.FModifiedDate;
    FModifiedDateGMT := Src.FModifiedDateGMT;
    FModifiedAvailable := Src.FModifiedAvailable;
    FSizeAvailable := Src.FSizeAvailable;
    FBlockSize := Src.FBlockSize;
    FNumberBlocks := Src.FNumberBlocks;
    FPermissions := Src.FPermissions;
    FPermissionsDisplay := Src.FPermissionsDisplay;
  end
  else
    inherited Assign(Source);
end;

procedure TScFTPListItem.SetFileName(const Value: string);
var
  i: integer;
begin
  if (FLocalFileName = '') or AnsiSameText(FFileName, FLocalFileName) then begin
    FFileName := Value;

    for i := 1 to Length(Value) do begin
      if CharInSet(Value[i], ['a'..'z']) then begin
        FLocalFileName := Value;
        Exit;
      end;
    end;

    FLocalFileName := LowerCase(Value);
  end
  else
    FFileName := Value;
end;

{ TScFTPDirectoryListing }

constructor TScFTPDirectoryListing.Create;
begin
  inherited Create(TScFTPListItem);
end;

function TScFTPDirectoryListing.Add: TScFTPListItem;
begin
  Result := TScFTPListItem(inherited Add);
end;

function TScFTPDirectoryListing.IndexOf(Item: TScFTPListItem): integer;
Var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Item = Items[i] then begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;

function TScFTPDirectoryListing.GetItem(Index: integer): TScFTPListItem;
begin
  Result := TScFTPListItem(inherited Items[Index]);
end;

procedure TScFTPDirectoryListing.SetItem(Index: integer; Value: TScFTPListItem);
begin
  inherited Items[Index] := Value;
end;

{ TScFTPListParser }

class function TScFTPListParser.Identitifier: string;
begin
  Result := '';
end;

class function TScFTPListParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTPListItem;
end;

class function TScFTPListParser.CreateItem(Owner: TScFTPDirectoryListing): TScFTPListItem;
begin
  Result := GetItemClassType.Create(Owner);
end;

class function TScFTPListParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
begin
  Result := False;
end;

class function TScFTPListParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
begin
  Result := False;
end;

class function TScFTPListParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  NewItem: TScFTPListItem;
  i: integer;
begin
  for i := 0 to Listing.Count - 1 do begin
    if Listing[i] <> '' then begin
      NewItem := CreateItem(Dir);
      NewItem.Data := Listing[i];
      ParseLine(NewItem);
    end;
  end;

  Result := True;
end;

class function TScFTPListParser.IsIEFile(const Str: string): boolean;
begin
  Result := (Length(Str) >= 4) and AnsiSameText(Copy(Str, Length(Str) - 3, 4), '._IE');
end;

class function TScFTPListParser.IsTotalLine(const Data: string): boolean;
begin
  Result := (Data <> '') and
    (Data[Length(Data)] <> ':') and
    (AnsiSameText(Copy(Data, 1, 5), 'TOTAL') or
    AnsiSameText(Copy(Data, 1, 6), 'GESAMT') or // German
    AnsiSameText(Copy(Data, 1, 9), 'INSGESAMT') or // German HPUX
    AnsiSameText(Copy(Data, 1, Length(KoreanTotal)), KoreanTotal) or
    AnsiSameText(Copy(Data, 1, Length(ChineseTotal)), ChineseTotal) or
    AnsiSameText(Copy(Data, 1, Length(JapaneseTotal)), JapaneseTotal));
end;

{ TScFTPListHeaderParser }

class function TScFTPListHeaderParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  i: integer;
begin
  Result := False;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if (Trim(Str) <> '') and not IsSpaceLine(Str) then begin
      Result := IsHeader(Str);
      Break;
    end;
  end;
end;

class function TScFTPListHeaderParser.IsHeader(const Data: string): boolean;
begin
  Result := False;
end;

class function TScFTPListHeaderParser.IsFooter(const Data: string): boolean;
begin
  Result := False;
end;

class function TScFTPListHeaderParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  Str: string;
  StartLine: integer;
  NewItem: TScFTPListItem;
  i: integer;
begin
  Result := True;
  if Listing.Count = 0 then
    Exit;

  StartLine := 0;
  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if IsHeader(Str) or (Trim(Str) = '') or IsSpaceLine(Str) then
      StartLine := i + 1
    else
      Break;
  end;

  for i := StartLine to Listing.Count - 1 do begin
    Str := Listing[i];

    if (Trim(Str) <> '') and not IsSpaceLine(Str) and not IsFooter(Str) then begin
      NewItem := CreateItem(Dir);
      NewItem.Data := Str;
      ParseLine(NewItem);
    end;
  end;
end;

{ TScFTP_SimpleListItem }

constructor TScFTP_SimpleListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FModifiedAvailable := False;
  FSizeAvailable := False;
end;

{ TScFTP_NListParser }

class function TScFTP_NListParser.Identitifier: string;
begin
  Result := 'NLST';
end;

class function TScFTP_NListParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SimpleListItem;
end;

class function TScFTP_NListParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
begin
  Result := False;
end;

class function TScFTP_NListParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
begin
  Item.FileName := Item.Data;
  Result := True;
end;

{ TScFTP_CreationDateListItem }

constructor TScFTP_CreationDateListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FModifiedAvailable := False;
  FSizeAvailable := False;
end;

{ TScFTP_MListItem }

constructor TScFTP_MListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FAttributesAvailable := False;
  FAttributes := TScWin32Attributes.Create;
end;

destructor TScFTP_MListItem.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function TScFTP_MListItem.GetFact(const Name: string): string;
var
  Facts: TStrings;
  TmpData: string;
begin
  Facts := TStringList.Create;
  try
    TmpData := FData;
    TmpData := ExtractFirstWord(TmpData, ' ');
    repeat
      Facts.Add(ExtractFirstWord(TmpData, ';'));
    until TmpData = '';

    Result := Facts.Values[Name];
  finally
    Facts.Free;
  end;
end;

{ TScFTP_MListParser }

class function TScFTP_MListParser.Identitifier: string;
begin
  Result := 'MLST';
end;

class function TScFTP_MListParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_MListItem;
end;

class function TScFTP_MListParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
begin
  Result := False;
end;

class function TScFTP_MListParser.ParseFacts(const Data: string; Strings: TStrings): string;
var
  TmpData: string;
  Facts, CharSet: string;
  SrcEncoding: Encoding;
begin
  Facts := Data;
  TmpData := ExtractFirstWord(Facts, ' ');
  repeat
    Strings.Add(ExtractFirstWord(TmpData, ';'));
  until TmpData = '';

  if Length(Facts) = 0 then begin
    Result := '';
    Exit;
  end;

  CharSet := Strings.Values['charset'];
  if CharSet = '' then
    CharSet := 'UTF-8';

  try
    SrcEncoding := EncodingByCharset(CharSet);
    if SrcEncoding = nil then
      SrcEncoding := Encoding.Default;

    if SrcEncoding <> Encoding.Default then
      Result := SrcEncoding.GetString(Encoding.Default.GetBytes(Facts))
    else
      Result := Facts;
  except
    Result := Facts;
  end;
end;

class function TScFTP_MListParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;

  function MListStrToGMTDateTime(TimeStamp: string): TDateTime;
  var
    Year, Month, Day, Hour, Min, Sec, MSec: integer;
  begin
    if TimeStamp <> '' then begin
      Year := StrToIntDef(Copy(TimeStamp, 1, 4), 0);
      Month := StrToIntDef(Copy(TimeStamp, 5, 2), 0);
      Day := StrToIntDef(Copy(TimeStamp, 7, 2), 0);

      Hour := StrToIntDef(Copy(TimeStamp, 9, 2), 0);
      Min := StrToIntDef(Copy(TimeStamp, 11, 2), 0);
      Sec := StrToIntDef(Copy(TimeStamp, 13, 2), 0);
      ExtractFirstWord(TimeStamp, '.');
      MSec := StrToIntDef(TimeStamp, 0);
      Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
    end
    else
      Result := 0;
  end;

const
  Sslink = 'OS.unix=slink:';
  Sblk = 'OS.unix=blk-';
  Schr = 'OS.unix=chr-';

var
  TmpData: string;
  Lexems: TStrings;
begin
  Lexems := TStringList.Create;
  try
    Item.FileName := ParseFacts(Item.Data, Lexems);
    Item.LocalFileName := Item.FileName;

    TmpData := Lexems.Values['type'];
    if (TmpData = 'cdir') or (TmpData = 'pdir') or (TmpData = 'dir') then
      Item.FileType := fftDirectory
    else
    if TmpData = 'OS.unix=slink' then
      Item.FileType := fftSymLink
    else
    if TmpData = 'OS.unix=socket' then
      Item.FileType := fftSocket
    else
    if TmpData = 'OS.unix=blk' then
      Item.FileType := fftBlockDevice
    else
    if TmpData = 'OS.unix=chr' then
      Item.FileType := fftCharDevice
    else
    if TmpData = 'OS.unix=fifo' then
      Item.FileType := fftFifo
    else begin
      if TmpData <> '' then begin
        if Copy(TmpData, 1, Length(Sslink)) = Sslink then begin
           Item.FileType := fftSymLink;
           ExtractFirstWord(TmpData, ':');
           TScFTP_MListItem(Item).LinkedItemName := TmpData;
        end
        else
        if Copy(TmpData, 1, Length(Sblk)) = Sblk then
          Item.FileType := fftBlockDevice
        else
        if Copy(TmpData, 1, Length(Schr)) = Schr then
          Item.FileType := fftCharDevice
        else
          Item.FileType := fftFile;
      end
      else
        Item.FileType := fftFile;
    end;

    TmpData := Lexems.Values['modify'];
    if TmpData <> '' then begin
      Item.ModifiedDateGMT := MListStrToGMTDateTime(TmpData);
      Item.ModifiedDate := Item.ModifiedDateGMT + GetLocalTimeZoneOffset;
      Item.ModifiedAvailable := True;
    end
    else
      Item.ModifiedAvailable := False;

    TmpData := Lexems.Values['create'];
    if TmpData <> '' then begin
      TScFTP_MListItem(Item).CreationDateGMT := MListStrToGMTDateTime(TmpData);
      TScFTP_MListItem(Item).CreationDate := TScFTP_MListItem(Item).CreationDateGMT + GetLocalTimeZoneOffset;
    end;

    TmpData := Lexems.Values['windows.lastaccesstime'];
    if TmpData <> '' then begin
      TScFTP_MListItem(Item).LastAccessDateGMT := MListStrToGMTDateTime(TmpData);
      TScFTP_MListItem(Item).LastAccessDate := TScFTP_MListItem(Item).LastAccessDateGMT + GetLocalTimeZoneOffset;
    end;

    TmpData := Lexems.Values['size'];
    if TmpData <> '' then begin
      Item.Size := StrToInt64Def(TmpData, 0);
      Item.SizeAvailable := True;
    end
    else
      Item.SizeAvailable := False;

    if not Item.SizeAvailable and (Item.FileType = fftDirectory) then begin
      TmpData := Lexems.Values['sizd'];
      if TmpData <> '' then begin
        Item.Size := StrToInt64Def(TmpData, 0);
        Item.SizeAvailable := True;
      end;
    end;

    TmpData := Lexems.Values['perm'];
    if TmpData <> '' then begin
      Item.Permissions := TmpData;
      Item.PermissionsDisplay := TmpData;
    end
    else begin
      TmpData := Lexems.Values['UNIX.mode'];
      if TmpData <> '' then begin
        if IsNumeric(TmpData) then begin
          PermissionsToStr(StrToIntDef(TmpData, 0), TmpData);
          case Item.FileType of
            fftFile:
              TmpData := '-' + TmpData;
            fftDirectory:
              TmpData := 'd' + TmpData;
            fftSymLink,
            fftSymLinkDir:
              TmpData := 'l' + TmpData;
            fftBlockDevice:
              TmpData := 'b' + TmpData;
            fftCharDevice:
              TmpData := 'c' + TmpData;
            fftFifo:
              TmpData := 'p' + TmpData;
            fftSocket:
              TmpData := 's' + TmpData;
           end;
        end;
        Item.Permissions := TmpData;
        Item.PermissionsDisplay := TmpData;
      end;
    end;

    TScFTP_MListItem(Item).UniqueID := Lexems.Values['unique'];

    TmpData := Lexems.Values['win32.ea'];
    if TmpData <> '' then begin
      ExtractFirstWord(TmpData, 'x');
      TScFTP_MListItem(Item).AttributesAvailable := True;
      TScFTP_MListItem(Item).Attributes.FileAttributes := StrToIntDef('$' + TmpData, 0);
    end;

    Result := True;
  finally
    Lexems.Free;
  end;
end;

{ TScFTP_AS400Parser }

class function TScFTP_AS400Parser.Identitifier: string;
begin
  Result := 'AS400';
end;

class function TScFTP_AS400Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if Lexems.Count > 4 then
      Result := ((Lexems[4] <> '') and (Lexems[4][1] = '*')) or (Lexems[4] = 'DIR');
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_AS400Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Size, Date, Time, ObjType, FileName: string;
  DateDelim: string;
  Year, Month, Day: integer;
  t: integer;
  p: integer;
begin
  try
    Item.ModifiedAvailable := False;
    Item.SizeAvailable := False;

    TmpData := Item.Data;
    Item.OwnerName := ExtractFirstWord(TmpData, ' ');

    TmpData := TrimLeft(TmpData);
    if (TmpData <> '') and CharInSet(TmpData[1], ['0'..'9']) then begin
      p := Pos(' ', TmpData);
      if (p > 9) or (p = 0) then begin
        Size := Copy(TmpData, 1, 9);
        Delete(TmpData, 1, 9);
      end
      else
        Size := ExtractFirstWord(TmpData, ' ');

      Item.Size := StrToInt64Def(Size, 0);
      Item.SizeAvailable := True;
      TmpData := TrimLeft(TmpData);
    end;

    if (TmpData <> '') and CharInSet(TmpData[1], ['0'..'9']) then begin
      Date := Trim(Copy(TmpData, 1, 8));
      Delete(TmpData, 1, 8);

      if (TmpData <> '') and (TmpData[1] <> ' ') then
        Date := Date + ExtractFirstWord(TmpData, ' ');

      if Date <> '' then begin
        DateDelim := DetectFirstDelim(Date);

        if DateDelim <> '' then begin
          Day := StrToIntDef(ExtractFirstWord(Date, DateDelim), 0);
          Month := StrToIntDef(ExtractFirstWord(Date, DateDelim), 0);
          Year := StrToIntDef(ExtractFirstWord(Date, DateDelim), 0);
          if Month > 12 then begin
            t := Month;
            Month := Day;
            Day := t;
          end;

          if Day > 31 then begin
            t := Day;
            Day := Year;
            Year := t;
          end;

          Year := CheckYear4(Year);
          Item.ModifiedDate := EncodeDate(Year, Month, Day);
        end
        else
          Item.ModifiedDate := 0;

        Item.ModifiedAvailable := True;
      end;

      Time := Trim(Copy(TmpData, 1, 8));
      Delete(TmpData, 1, 8);

      if (TmpData <> '') and (TmpData[1] <> ' ') then
        Time := Time + ExtractFirstWord(TmpData, ' ');

      if Time <> '' then
        Item.ModifiedDate := Item.ModifiedDate + StrToTime(Time, HHMMSS);
    end;

    TmpData := Trim(TmpData);
    p := Pos(' ', TmpData);
    if (p > 11) or (p = 0) then begin
      ObjType := Copy(TmpData, 1, 11);
      Delete(TmpData, 1, 11);
    end
    else
      ObjType := ExtractFirstWord(TmpData, ' ');

    if (ObjType = '*DIR') or (ObjType = '*DDIR') or (ObjType = '*LIB') or (ObjType = '*FLR') then begin
      Item.FileType := fftDirectory;
      if (TmpData <> '') and (TmpData[Length(TmpData)] = '/') then
        TmpData := ExtractFirstWord(TmpData, '/');
    end;

    Item.FileName := TrimLeft(TmpData);
    if Item.FileName = '' then begin
      Item.FileName := Item.OwnerName;
      Item.OwnerName := '';
    end;

    FileName := Item.FileName;
    repeat
      TmpData := ExtractFirstWord(FileName, '/');
    until FileName = '';

    Item.LocalFileName := LowerCase(TmpData);

    Result := True;
  except
    Result := False;
  end;
end;

{ TScFTP_XercomMicroRTOSParser }

class function TScFTP_XercomMicroRTOSParser.Identitifier: string;
begin
  Result := 'Xercom Micro RTOS';
end;

class function TScFTP_XercomMicroRTOSParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_XecomMicroRTOSListItem;
end;

class function TScFTP_XercomMicroRTOSParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count = 5 then
      Result := (Cols[0] = 'Start') and
        (Cols[1] = 'End') and
        (Cols[2] = 'length') and
        (Cols[3] = 'File') and
        (Cols[4] = 'name');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_XercomMicroRTOSParser.IsFooter(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count = 7 then
      Result := (Cols[0] = '**') and
        (Cols[1] = 'Total') and
        IsNumeric(Cols[2]) and
        (Cols[3] = 'files,') and
        IsNumeric(Cols[4]) and
        (Cols[5] = 'bytes') and
        (Cols[6] = '**');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_XercomMicroRTOSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
begin
  TmpData := TrimLeft(Item.Data);
  TScFTP_XecomMicroRTOSListItem(Item).MemStart := StrToIntDef('$' + ExtractFirstWord(TmpData, ' '), 0);
  TmpData := TrimLeft(TmpData);
  TScFTP_XecomMicroRTOSListItem(Item).MemEnd := StrToIntDef('$' + ExtractFirstWord(TmpData, ' '),0);

  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);
  Item.FileName := TrimLeft(TmpData);

  Result := True;
end;

{ TScFTP_XecomMicroRTOSListItem }

constructor TScFTP_XecomMicroRTOSListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FModifiedAvailable := False;
end;

{ TScFTP_WinQVNetParser }

class function TScFTP_WinQVNetParser.Identitifier: string;
begin
  Result := 'WinQVT/NET';
end;

class function TScFTP_WinQVNetParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  Result := (Copy(Str, 38, 1) = '-') and
    (Copy(Str, 41, 1) = '-') and
    (Copy(Str, 49, 1) = ':') and
    IsDate(Copy(Str, 36, 10), MMDDYY) and
    (Copy(Str, 46, 1) = ' ') and
    IsTime(Copy(Str, 47, 5), HHMMSS);
end;

class function TScFTP_WinQVNetParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, FileName: string;
begin
  TmpData := Copy(Item.Data, 1, 12);
  FileName := ExtractFirstWord(TmpData, '.');
  TmpData := Trim(TmpData);
  if TmpData <> '' then
    FileName := FileName + '.' + ExtractFirstWord(TmpData, ' ');
  Item.FileName := ExtractFirstWord(FileName, '/');

  TmpData := Item.Data;
  if Pos('/', Copy(TmpData, 1, 13)) > 0 then
    Item.FileType := fftDirectory;

  Delete(TmpData, 1, 13);
  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToDate(ExtractFirstWord(TmpData, ' '), MMDDYY);
  TmpData := Trim(TmpData);
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(TmpData, HHMMSS);

  Result := True;
end;

{ TScFTP_WindowsNTParser }

class function TScFTP_WindowsNTParser.Identitifier: string;
begin
  Result := 'Windows NT';
end;

class function TScFTP_WindowsNTParser.IsSubDirContentBanner(const Value: string): boolean;
begin
  Result := (Value <> '') and (Value[Length(Value)] = ':') and not IsValidEmulUnixPermissions(Value);
end;

class function TScFTP_WindowsNTParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  Size, Dir: string;
  sl: TStrings;
  i: integer;
begin
  Result := False;
  Dir := '';

  for i := 0 to Listing.Count - 1 do begin
    Str := UpperCase(Listing[i]);

    if (Str <> '') and not IsSubDirContentBanner(Str) then begin
      if Pos(' <DIR> ', Str) in [22..26] then
        Dir := '<DIR>';

      if IsDate(Copy(Str, 1, 8), MMDDYY) or IsDate(Str, YYMMDD) or IsDate(Str, MMDDYY) then begin
        Size := Copy(Str, 20, 19);
        if (Length(Size) = 19) and IsNumeric(Size[17]) and (Size[18] = ' ') then
          SetLength(Size, 17);
        Size := StringReplace(TrimLeft(Size), ',', '', [rfReplaceAll]);

        if (Dir = '<DIR>') or ((Dir = '') and (StrToInt64Def(Size, -1) <> -1)) then begin
          Result := False;
          sl := TStringList.Create;
          try
            Split(sl, Str, ' ', True);

            if sl.Count > 4 then
              if (sl[2] = 'F') or (sl[2] = 'D') then
                Result := IsNumeric(sl[4]) or (sl[4] = '-');
          finally
            sl.Free;
          end;

          Result := not Result;
        end;
      end;
    end;
  end;
end;

class function TScFTP_WindowsNTParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
  FileName, Date: string;
  MarkerPos: integer;
begin
  if (Item.Data <> '') and IsNumeric(Copy(Item.Data, 1, 4)) and not IsNumeric(Copy(Item.Data, 5, 1)) then begin
    Date := Copy(Item.Data, 1, 4) + '/' +
      Copy(Item.Data, 6, 2) + '/' +
      Copy(Item.Data, 9, 2) + ' ';

    TmpData := Trim(Copy(Item.Data, 11, MaxInt));
    Date := Date + ExtractFirstWord(TmpData, ' ');
    try
      Item.ModifiedDate := StrToDate(ExtractFirstWord(Date, ' '), YYMMDD);
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Date, HHMMSS);
    except
      Item.ModifiedDate := 0.0;
    end;
  end
  else begin
    TmpData := Item.Data;
    Date := ExtractFirstWord(TmpData, ' ');
    TmpData := TrimLeft(TmpData);
    Date := Date + ' ' + ExtractFirstWord(TmpData, ' ');
    try
      Item.ModifiedDate := StrToDate(ExtractFirstWord(Date, ' '), MMDDYY);
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Date, HHMMSS);
    except
      Item.ModifiedDate := 0.0;
    end;
  end;

  MarkerPos := 1;
  while True do begin
    TmpData := Trim(TmpData);
    Lexem := ExtractFirstWord(TmpData, ' ');

    if Pos(',', Lexem) <> 0 then
      Lexem := StringReplace(Lexem, ',', '', [rfReplaceAll]);

    if AnsiSameText(Lexem, '<DIR>') then begin
      Item.FileType := fftDirectory;
      if (TmpData <> '') and (Copy(TmpData, 1, 17) = '                 ') then
        MarkerPos := 18
      else if (TmpData <> '') and (Copy(TmpData, 1, 9) = '         ') then
        MarkerPos := 10;

      Item.SizeAvailable := False;
      Break;
    end
    else begin
      if not AnsiSameText(Lexem, 'AM') then begin
        if AnsiSameText(Lexem, 'PM') then
          Item.ModifiedDate := Item.ModifiedDate + EncodeTime(12, 0, 0, 0)
        else begin
          Item.FileType := fftFile;
          Item.Size := StrToInt64Def(Lexem, 0);
          Break;
        end;
      end;
    end;
  end;

  Item.LocalFileName := TmpData;
  FileName := Copy(TmpData, MarkerPos, MaxInt);
  if Path <> '' then begin
    Item.LocalFileName := FileName;
    FileName := Path + '\' + FileName;
    if (Length(FileName) >= 2) and (Copy(FileName, 1, 2) = '.\') then
      Delete(FileName, 1, Length('.\'));
  end;

  Item.FileName := FileName;

  Result := True;
end;

class function TScFTP_WindowsNTParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  Str, PathSpec: string;
  NewItem: TScFTPListItem;
  i: integer;
begin
  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if Str <> '' then begin
      if IsSubDirContentBanner(Str) then
        PathSpec := Copy(Str, 1, Length(Str) - 1)
      else begin
        NewItem := CreateItem(Dir);
        NewItem.Data := Str;
        Result := ParseLine(NewItem, PathSpec);
        if not Result then begin
          FreeAndNil(NewItem);
          Exit;
        end
      end;
    end;
  end;

  Result := True;
end;

{ TScFTP_WfFTPParser }

class function TScFTP_WfFTPParser.Identitifier: string;
begin
  Result := 'WfFTP';
end;

class function TScFTP_WfFTPParser.IsHeader(const Data: string): boolean;
const
  SVolume = ' Volume - drive ';
  SDirectory = ' Directory of ';
begin
  Result := (Data <> '') and ((Copy(Data, 1, Length(SVolume)) = SVolume) or (Copy(Data, 1, Length(SDirectory)) = SDirectory));
end;

class function TScFTP_WfFTPParser.IsFooter(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := (Pos('bytes - Total size', Data) > 1) or
    (Pos('bytes - Contiguous free space', Data) > 1) or
    (Pos('bytes - Available free space', Data) > 1);

  if Result then
    Exit;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count = 6 then
      Result := (Cols[0] = 'File') and
        (Cols[1] = 'Name') and
        (Cols[2] = 'Size') and
        (Cols[3] = 'Date') and
        (Cols[4] = 'Day') and
        (Cols[5] = 'Time');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_WfFTPParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
begin
  Item.FileType := fftFile;

  TmpData := Item.Data;
  Item.FileName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToDate(ExtractFirstWord(TmpData, ' '), MMDDYY);
  TmpData := TrimLeft(TmpData);
  ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(ExtractFirstWord(TmpData, ' '), HHMMSS);

  Result := True;
end;

{ TScFTP_VxWorksParser }

class function TScFTP_VxWorksParser.Identitifier: string;
begin
  Result := 'Wind River VxWorks';
end;

class function TScFTP_VxWorksParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count > 3 then
      Result := (Cols[0] = 'size') and
        (Cols[1] = 'date') and
        (Cols[2] = 'time') and
        (Cols[3] = 'name');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_VxWorksParser.IsFooter(const Data: string): boolean;
begin
  Result := (Data <> '') and AnsiSameText(Copy(Data, 1, 5), 'value');
end;

class function TScFTP_VxWorksParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
begin
  TmpData := Trim(Item.Data);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToDate(ExtractFirstWord(TmpData, ' '), Month_DD_YY);

  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(ExtractFirstWord(TmpData, ' '), HHMMSS);

  if (Length(TmpData) >= 5) and (Copy(TmpData, Length(TmpData) - 4, 5) = '<DIR>') then begin
    Item.FileType := fftDirectory;
    TmpData := Copy(TmpData, 1, Length(TmpData) - 5);
  end;

  Item.FileName := Trim(TmpData);

  Result := True;
end;

{ TScFTP_VSERootDirParser }

class function TScFTP_VSERootDirParser.Identitifier: string;
begin
  Result := 'VSE:  Root Directory';
end;

class function TScFTP_VSERootDirParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SimpleListItem;
end;

class function TScFTP_VSERootDirParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  ExtractFirstWord(Str, ' ');
  Str := Trim(Str);
  Result := (Str = '<Directory>') or (Str = '<VSE VTOC>') or
    (Str = '<Library>') or (Str = '<Power Queues>') or
    (Str = '<VSAM Catalog>') or (Str = 'Entry Seq VSAM');
end;

class function TScFTP_VSERootDirParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
begin
  TmpData := Item.Data;
  Item.FileName := ExtractFirstWord(TmpData, ' ');
  TmpData := Trim(TmpData);
  if TmpData = 'Entry Seq VSAM' then
    Item.FileType := fftFile
  else
    Item.FileType  := fftDirectory;

  Result := True;
end;

{ TScFTP_VSEVTOCParser }

class function TScFTP_VSEVTOCParser.Identitifier: string;
begin
  Result := 'VSE:  VTOC';
end;

class function TScFTP_VSEVTOCParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_VSEVTOCListItem;
end;

class function TScFTP_VSEVTOCParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if Lexems.Count = 5 then
      Result := (Pos(Lexems[4], 'SDVIU') > 0) and IsNumeric(Lexems[3]);
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_VSEVTOCParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);

    Item.FileName := Lexems[0];
    Item.ModifiedDate := StrToDate(Lexems[1], YYMMDD) + StrToTime(Lexems[2], HHMMSS);
    Item.FileType := fftFile;
    Item.SizeAvailable := False;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VSEPowerQueueParser }

class function TScFTP_VSEPowerQueueParser.Identitifier: string;
begin
  Result := 'VSE:  PowerQueue';
end;

class function TScFTP_VSEPowerQueueParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_VSEPowerQueueListItem;
end;

class function TScFTP_VSEPowerQueueParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
const
  PowerQueue_Dispositions: array [1..11] of Char =
    ('A', 'D', 'H', 'K', 'L', 'X', 'Y', '*', 'I', 'N', 'T');
var
  Lexems: TStrings;
  i, cnt: integer;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if (Lexems.Count = 6) or (Lexems.Count = 7) then begin
      cnt := 0;
      for i := 1 to Length(Lexems[0]) do begin
        if Lexems[0][i] = '.' then
          Inc(cnt);
      end;

      if cnt = 2 then
        Result := IsNumeric(Lexems[1]) and IsNumeric(Lexems[2]) and IsNumeric(Lexems[3]) and IsNumeric(Lexems[4]);

      if Result then
        Result := (Lexems[5] <> '') and (Pos(Lexems[5][1], PowerQueue_Dispositions) <> 0);
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_VSEPowerQueueParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  Disposition: TScFTPVSEPQDisposition;
begin
  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);

    if Lexems.Count > 0 then
      Item.FileName := Lexems[0];

    if Lexems.Count > 1 then begin
      Item.Size := StrToInt64Def(Lexems[1], 0);
      TScFTP_VSEPowerQueueListItem(Item).NumberRecs := Item.Size;
    end;

    if Lexems.Count > 4 then
      TScFTP_VSEPowerQueueListItem(Item).Priority := StrToIntDef(Lexems[4], 0);

    if (Lexems.Count > 5) and (Lexems[5] <> '') then begin
      case Lexems[5][1] of
        'A': Disposition := pqdAppendable;
        'D': Disposition := pqdProcessAndDelete;
        'H': Disposition := pqdHoldUntilReleased;
        'K': Disposition := pqdProcessAndKeep;
        'L': Disposition := pqdLeaveUntilReleased;
        'X': Disposition := pqdErrorHoldUntilDK;
        'Y': Disposition := pqdGetOrErrorHoldUntilDK;
        '*': Disposition := pqdJobProcessing;
        'I': Disposition := pqdSpoolOutputToInputD;
        'N': Disposition := pqdSurpressOutputSpooling;
        'T': Disposition := pqdSpoolOutputToTape;
      else
        Disposition := pqdProcessAndDelete;
      end;

      TScFTP_VSEPowerQueueListItem(Item).Disposition := Disposition;
    end;

    if Lexems.Count > 6 then
      Item.OwnerName := Lexems[6];

    Item.FileType := fftFile;
    Item.ModifiedAvailable := False;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VSEVSAMCatalogParser }

class function TScFTP_VSEVSAMCatalogParser.Identitifier: string;
begin
  Result := 'VSE:  VSAM Catalog';
end;

class function TScFTP_VSEVSAMCatalogParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if Lexems.Count = 5 then
      Result := (Pos(Lexems[4], 'EKR') > 0) and IsNumeric(Lexems[3]);
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_VSEVSAMCatalogParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);

    Item.FileName := Lexems[0];
    Item.ModifiedDate := StrToDate(Lexems[1], YYMMDD) + StrToTime(Lexems[2], HHMMSS);
    Item.Size := StrToInt64Def(Lexems[3], 0);
    Item.FileType := fftFile;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VSELibraryParser }

class function TScFTP_VSELibraryParser.Identitifier: string;
begin
  Result := 'VSE:  Library';
end;

class function TScFTP_VSELibraryParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  ExtractFirstWord(Str, ' ');
  Str := TrimLeft(Str);
  Str := ExtractFirstWord(Str, '>') + '>';
  Result := Str = '<Sub Library>';
end;

class function TScFTP_VSELibraryParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Lexems: TStrings;
begin
  TmpData := Item.Data;

  Item.FileName := ExtractFirstWord(TmpData, ' ');
  ExtractFirstWord(TmpData, '>');

  Lexems := TStringList.Create;
  try
    Split(Lexems, TmpData, ' ', True);

    if Lexems.Count > 0 then
      Item.Size := StrToInt64Def(Lexems[0], 0);

    if Lexems.Count > 1 then
      Item.NumberBlocks := StrToIntDef(Lexems[1], 0);

    if Lexems.Count > 2 then
      Item.ModifiedDate := StrToDate(Lexems[2], YYMMDD);

    if Lexems.Count > 3 then
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[3], HHMMSS);

    Item.FileType := fftDirectory;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VSESublibraryParser }

class function TScFTP_VSESublibraryParser.Identitifier: string;
begin
  Result := 'VSE:  Sublibrary';
end;

class function TScFTP_VSESublibraryParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_VSESublibraryListItem;
end;

class function TScFTP_VSESublibraryParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  if Length(Str) > 2 then
    Result := (Str[Length(Str) - 1] = ' ') and CharInSet(Str[Length(Str)], ['F', 'S']);

  if Result then begin
    Lexems := TStringList.Create;
    try
      Split(Lexems, Str, ' ', True);

      Result := (Lexems.Count > 4) and
        (Pos('/', Lexems[3]) > 0) and
        (Pos(':', Lexems[4]) > 0) and
        CharInSet(Lexems[Lexems.Count - 1][1], ['F', 'S']);
    finally
      Lexems.Free;
    end;
  end;
end;

class function TScFTP_VSESublibraryParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Lexems: TStrings;
begin
  if Length(Item.Data) < 2 then begin
    Result := False;
    Exit;
  end;

  Lexems := TStringList.Create;
  try
    TmpData := Copy(Item.Data, 1, Length(Item.Data) - 1);
    Split(Lexems, TmpData, ' ', True);

    if Lexems.Count > 0 then
      Item.FileName := Lexems[0];

    if Lexems.Count > 1 then begin
      Item.Size := StrToInt64Def(Lexems[1], 0);
      TScFTP_VSESublibraryListItem(Item).NumberRecs := Item.Size;
    end;

    if Lexems.Count > 2 then
      Item.NumberBlocks := StrToIntDef(Lexems[2], 0);

    if Lexems.Count > 3 then
      TScFTP_VSESublibraryListItem(Item).CreationDate := StrToDate(Lexems[3], YYMMDD);

    if Lexems.Count > 4 then
      TScFTP_VSESublibraryListItem(Item).CreationDate := TScFTP_VSESublibraryListItem(Item).CreationDate + StrToTime(Lexems[4], HHMMSS);

    if Lexems.Count > 5 then
      Item.ModifiedDate := StrToDate(Lexems[5], YYMMDD)
    else
      Item.ModifiedDate := StrToDate(Lexems[3], YYMMDD);

    if Lexems.Count > 6 then
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[6], HHMMSS)
    else
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[4], HHMMSS);

    Item.FileType := fftFile;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VSEVTOCListItem }

constructor TScFTP_VSEVTOCListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FSizeAvailable := False;
end;

{ TScFTP_VMSParser }

class function TScFTP_VMSParser.Identitifier: string;
begin
  Result := 'VMS';
end;

class function TScFTP_VMSParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_VMSListItem;
end;

class function TScFTP_VMSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  i: integer;
begin
  Result := False;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if Str <> '' then begin
      if Length(Str) > 1 then begin
        Result := IsVMSHeader(Str);
        if not Result then begin
          Str := ExtractFirstWord(Str, ' ');
          ExtractFirstWord(Str, ';');
          Result := IsNumeric(Str);
        end;
      end;

      Break;
    end;
  end;
end;

class function TScFTP_VMSParser.IsVMSFooter(const Data: string): boolean;
const
  STotalOf = 'TOTAL OF ';
  SGrandTotalOf = 'GRAND TOTAL OF ';
var
  UData: string;
begin
  UData := UpperCase(Data);
  Result := (UData <> '') and
    ((Copy(UData, 1, Length(STotalOf)) = STotalOf) or
    (Copy(UData, 1, Length(SGrandTotalOf)) = SGrandTotalOf));

  if Result then begin
    Result := (Pos(' FILE', UData) > 9);
    if not Result then
      Result := ExtractFirstWord(UData, ' ') = '*.*;';
  end;
end;

class function TScFTP_VMSParser.IsVMSHeader(const Data: string): boolean;
begin
  Result := (Data <> '') and (Data[Length(Data)] = ']') and (Pos(':[', Data) > 0);
end;

class function TScFTP_VMSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
const
  VMS_BLOCK_SIZE = 512;

var
  TmpData, TmpData2, FirstWord: string;
  Year, Month, Day: integer;
  Lexems: TStrings;
  OwnerIdx: integer;
  IsError: boolean;
begin
  TmpData2 := StringReplace(Item.Data, #9, ' ', [rfReplaceAll]);
  TmpData := ExtractFirstWord(TmpData2, ';');
  Item.LocalFileName := LowerCase(TmpData);

  FirstWord := ExtractFirstWord(TmpData2, ' ');
  TScFTP_VMSListItem(Item).Version := StrToIntDef(FirstWord, 0);
  TmpData := TmpData + ';' + FirstWord;

  if (Pos('.DIR;', TmpData) > 0) then begin
    Item.FileType := fftDirectory;
    if Pos(']', TmpData) = 0 then
      FirstWord := ''
    else
      FirstWord := ExtractFirstWord(TmpData, ']') + ']';
    Item.FileName := FirstWord + ExtractFirstWord(TmpData, '.');
    Item.LocalFileName := LowerCase(Item.FileName);
  end
  else begin
    Item.FileType := fftFile;
    Item.FileName := TmpData;
  end;

  if Path <> '' then
    Item.FileName := Path + Item.FileName;

  Lexems := TStringList.Create;
  try
    Split(Lexems, TmpData2, ' ', True);
    OwnerIdx := 3;

    if Lexems.Count > 0 then begin
      TmpData := Lexems[0];
      TmpData := ExtractFirstWord(TmpData, '/');

      IsError := False;
      if IsNumeric(TmpData) then begin
        Item.NumberBlocks := StrToIntDef(TmpData, 0);
        Item.BlockSize := VMS_BLOCK_SIZE;
        Item.Size := StrToInt64Def(TmpData, 0) * VMS_BLOCK_SIZE;
      end
      else begin
        if (Lexems[0] = '') or (Lexems[0][1] <> '[') then begin
          if (Lexems[0] = '') or not CharInSet(Lexems[0][1], ['0'..'9']) then begin
            IsError := True;
            Item.ModifiedAvailable := False;
            Item.SizeAvailable := False;
          end;
        end
        else
          OwnerIdx := 0;
      end;

      if not IsError then begin
        if OwnerIdx > 0 then begin
          if Lexems.Count > 1 then begin
            TmpData := Lexems[1];
            Day := StrToIntDef(ExtractFirstWord(TmpData, '-'), 1);
            Month := StrToMonth(ExtractFirstWord(TmpData, '-'));
            Year := StrToIntDef(ExtractFirstWord(TmpData, ' '), 1989);
            Item.ModifiedDate := EncodeDate(Year, Month, Day);
          end;

          if Lexems.Count > 2 then begin
            if Pos(':', Lexems[2]) = 0 then
              Dec(OwnerIdx)
            else
              Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[2], HHMMSS);
          end;
        end;

        if Lexems.Count > OwnerIdx then begin
          TmpData := Lexems[OwnerIdx];
          ExtractFirstWord(TmpData, '[');
          TmpData := ExtractFirstWord(TmpData,']');
          Item.GroupName := Trim(ExtractFirstWord(TmpData, ','));
          Item.OwnerName := Trim(TmpData);
        end;

        if Lexems.Count > (OwnerIdx + 1) then begin
          TmpData := Lexems[OwnerIdx + 1];
          ExtractFirstWord(TmpData, '(');
          TmpData := ExtractFirstWord(TmpData, ')');
          Item.Permissions := TmpData;
          Item.PermissionsDisplay := '(' + TmpData + ')';
          TScFTP_VMSListItem(Item).SystemPermissions := Trim(ExtractFirstWord(TmpData, ','));
          TScFTP_VMSListItem(Item).OwnerPermissions := Trim(ExtractFirstWord(TmpData, ','));
          TScFTP_VMSListItem(Item).GroupPermissions := Trim(ExtractFirstWord(TmpData, ','));
          TScFTP_VMSListItem(Item).WorldPermissions := Trim(TmpData);
        end;
      end;
    end;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

class function TScFTP_VMSParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
const
  VMS_RELPATH_PREFIX = '[.';

var
  NewItem: TScFTPListItem;
  StartLine, EndLine: integer;
  Str: string;
  RootPath, RelPath: string;
  i: integer;
begin
  RootPath := '';
  RelPath := '';
  StartLine := 0;
  EndLine := Listing.Count - 1;

  for i := 0 to EndLine do begin
    Str := Listing[i];

    if Trim(Str) = '' then
      Inc(StartLine)
    else begin
      if IsVMSHeader(Str) then begin
        RootPath := ExtractFirstWord(Str, ']') + '.';
        Inc(StartLine);
      end;

      Break;
    end;
  end;

  for i := EndLine downto StartLine do begin
    Str := Listing[i];

    if (Trim(Str) = '') or IsVMSFooter(Str) then
      Dec(EndLine)
    else
      Break;
  end;

  for i := StartLine to EndLine do begin
    Str := Listing[i];

    if Trim(Str) <> '' then begin
      if IsVMSHeader(Str) then
        RelPath := VMS_RELPATH_PREFIX + Copy(Str, Length(RootPath) + 1, MaxInt)
      else
      if not ((Str <> '') and (Str[1] = ' ') and (Pos(';', Str) = 0)) then begin
        NewItem := CreateItem(Dir);
        NewItem.Data := UnfoldLines(Listing, i);
        Result := ParseLine(NewItem, RelPath);
        if not Result then begin
          FreeAndNil(NewItem);
          Exit;
        end;
      end;
    end;
  end;

  Result := True;
end;

{ TScFTP_VMCMSParser }

class function TScFTP_VMCMSParser.Identitifier: string;
begin
  Result := 'VM/CMS';
end;

class function TScFTP_VMCMSParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_RecListItem;
end;

class function TScFTP_VMCMSParser.IsHeader(const Data: string): boolean;
begin
  Result := Trim(Data) = 'Filename FileType  Fm Format Lrecl  Records Blocks Date      Time'
end;

class function TScFTP_VMCMSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str, SubStr: string;
begin
  Result := inherited CheckIfListing(Listing, SysDescript);

  if not Result then begin
    if Listing.Count = 0 then
      Exit;

    Str := Listing[0];
    SubStr := Trim(Copy(Str, 19, 3));
    if CharInSet(SubStr[1], ['A'..'V']) and CharInSet(SubStr[2], ['0'..'6']) then begin
      SubStr := Trim(Copy(Str, 22, 3));
      Result := (SubStr = 'F') or (SubStr = 'V') or (SubStr = 'DIR');
      if Result then
        Result := IsDate(Trim(Copy(Str, 52, 10)), MMDDYY);
    end
    else begin
      Result := (SubStr = 'F') or (SubStr = 'V') or (SubStr = 'DIR');
      if Result then begin
        Result := (Copy(Str, 56, 1) = '/') and (Copy(Str, 59, 1) = '/');
        if not Result then begin
          Result := (Copy(Str, 58, 1) = '-') and (Copy(Str, 61, 1) = '-');
          if not Result then
            Result := (Copy(Str, 48, 1) = '-') and (Copy(Str, 51, 1) = '-');
        end;
      end;
    end;
  end;
end;

class function TScFTP_VMCMSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Lexems: TStrings;
  RecPos, RecLen: integer;
  RecNoPos, RecNoLen: integer;
  BlocksPos, BlocksLen: integer;
  ColPos: integer;
begin
  Item.FileName := Trim(Copy(Item.Data, 1, 8));

  if Item.Data[9] = ' ' then
    TmpData := Trim(Copy(Item.Data, 10, 9))
  else
    TmpData := Trim(Copy(Item.Data, 9, 9));
  if TmpData <> '' then
    Item.FileName := Item.FileName + '.' + TmpData;

  TmpData := Trim(Copy(Item.Data, 19, 3));
  if CharInSet(TmpData[1], ['A'..'V']) and CharInSet(TmpData[2], ['0'..'6']) then begin
    TmpData := Trim(Copy(Item.Data, 23, 3));
    RecPos := 30;
    RecLen := 7;
    RecNoPos := 37;
    RecNoLen := 7;
    BlocksPos := 44;
    BlocksLen := 8;
    ColPos := 52;
  end
  else begin
    RecPos := 22;
    RecLen := 9;
    RecNoPos := 31;
    RecNoLen := 11;
    BlocksPos := 42;
    BlocksLen := 11;
    if (Copy(Item.Data, 48, 1) = '-') and (Copy(Item.Data, 51, 1) = '-') then
      ColPos := 44
    else
      ColPos := 54;
  end;

  TScFTP_RecListItem(Item).RecFormat := TmpData;
  if TScFTP_RecListItem(Item).RecFormat = 'DIR' then begin
    Item.FileType := fftDirectory;
    TScFTP_RecListItem(Item).RecLength := 0;
  end
  else begin
    Item.FileType := fftFile;
    TScFTP_RecListItem(Item).RecLength := StrToIntDef(Copy(Item.Data, RecPos, RecLen), 0);

    TmpData := Trim(Copy(Item.Data, RecNoPos, RecNoLen));
    TScFTP_RecListItem(Item).NumberRecs := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);

    Item.NumberBlocks := StrToIntDef(Trim(Copy(Item.Data, BlocksPos, BlocksLen)), 0);
    Item.Size := Int64(TScFTP_RecListItem(Item).RecLength) * TScFTP_RecListItem(Item).NumberRecs;

    if TScFTP_RecListItem(Item).RecFormat = 'V' then begin
      if Item.Size > Int64(Item.NumberBlocks) * 4096 then
        Item.Size := Int64(Item.NumberBlocks) * 4096;
    end
    else
    if TScFTP_RecListItem(Item).RecFormat = 'DIR' then
      Item.SizeAvailable := False;
  end;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Trim(Copy(Item.Data, ColPos, MaxInt)), ' ', True);

    if Lexems.Count > 0 then begin
      if (Lexems[0] <> '') and IsNumeric(Copy(Lexems[0], 1, 3)) then
        Item.ModifiedDate := StrToDate(Lexems[0], YYMMDD)
      else
        Item.ModifiedDate := StrToDate(Lexems[0], MMDDYY);

      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[1], HHMMSS);
      if (Lexems.Count > 2) and (Lexems[2] <> '-') then
        Item.OwnerName := Lexems[2];
    end;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VMBFSParser }

class function TScFTP_VMBFSParser.Identitifier: string;
begin
  Result := 'VM/BFS';
end;

class function TScFTP_VMBFSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  if (Listing[0] <> '') and (Listing[0][Length(Listing[0])] <> '''') then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if Lexems.Count > 4 then begin
      if not IsDate(Lexems[0], MMDDYY) then
        Exit;

      Result := CharInSet(Lexems[2][1], ['F', 'D']);
      if Result then
        Result := IsNumeric(Lexems[4]) or (Lexems[4] <> '-');
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_VMBFSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  TmpData: string;
begin
  TmpData := Item.Data;
  Lexems := TStringList.Create;
  try
    Split(Lexems, ExtractFirstWord(TmpData, #39), ' ', True);

    Item.FileName := TmpData;
    if (Item.FileName <> '') and (Item.FileName[Length(Item.FileName)] = '''') then
      Item.FileName := Copy(Item.FileName, 1, Length(Item.FileName) - 1);

    if Lexems.Count > 0 then
      Item.ModifiedDate := StrToDate(Lexems[0], MMDDYY);

    if Lexems.Count > 1 then
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[1], HHMMSS);

    if Lexems.Count > 2 then
      if Lexems[2] = 'D' then
        Item.FileType := fftDirectory
      else
        Item.FileType := fftFile;

    if Lexems.Count > 4 then
      if IsNumeric(Lexems[3]) then begin
        Item.Size := StrToInt64Def(Lexems[4], 0);
        Item.SizeAvailable := True;
      end
      else
        Item.SizeAvailable := False;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VirtualReaderParser }

class function TScFTP_VirtualReaderParser.Identitifier: string;
begin
  Result := 'VM Virtual Reader';
end;

class function TScFTP_VirtualReaderParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_VMVirtualReaderListItem;
end;

class function TScFTP_VirtualReaderParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if (Lexems.Count > 2) and (Length(Lexems[0]) = 4) and IsNumeric(Lexems[0]) then
      Result := (Length(Lexems[2]) = 8) and (IsNumeric(Lexems[2]));
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_VirtualReaderParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);

    if Lexems.Count > 5 then
      Item.FileName := Lexems[5];
    if Lexems.Count > 6 then
      Item.FileName := Item.FileName + '.' + Lexems[6];
    if Lexems.Count > 2 then
      TScFTP_VMVirtualReaderListItem(Item).NumberRecs := StrToIntDef(Lexems[2], 0);
    if Lexems.Count > 3 then
      Item.ModifiedDate := StrToDate(Lexems[3], YYMMDD);
    if Lexems.Count > 4 then
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[1], HHMMSS);
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_VMVirtualReaderListItem }

constructor TScFTP_VMVirtualReaderListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FSizeAvailable := False;
end;

{ TScFTP_UnixParser }

class function TScFTP_UnixParser.Identitifier: string;
begin
  Result := 'Unix';
end;

class function TScFTP_UnixParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_UnixBaseListItem;
end;

class function TScFTP_UnixParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  sl: TStrings;
  Str: string;
  i: integer;
begin
  Result := False;

  sl := TStringList.Create;
  try
    for i := 0 to Listing.Count - 1 do begin
      Str := Listing[i];

      if Str <> '' then begin
        if IsUnixStr(Str) then begin
          Split(sl, Str, ' ', True);
          Result := (sl.Count > 4) and ((sl[4] = 'AR') or (sl[4] = 'DK'));
          if not Result then
            Result := IsUnitreeBanner(Str);

          if Identitifier <> TScFTP_UnitreeParser.Identitifier then
            Result := not Result;
          Break;
        end;

        if not (IsTotalLine(Str) or IsSubDirContentBanner(Str)) then
          Break;
      end;
    end;
  finally
    sl.Free;
  end;
end;

class function TScFTP_UnixParser.IsSubDirContentBanner(const Value: string): boolean;
begin
  Result := (Value <> '') and (Value[Length(Value)] = ':') and not IsValidEmulUnixPermissions(Value);
end;

class function TScFTP_UnixParser.IsUnixStr(const Data: string): boolean;
var
  sl: TStrings;
  UData: string;
begin
  UData := UpperCase(Data);
  Result := IsValidEmulUnixPermissions(Data);

  if Result then begin
    if CharInSet(UData[1], ['C', 'B']) then
      Exit;

    sl := TStringList.Create;
    try
      Split(sl, UData, ' ', True);

      if sl.Count > 9 then begin
        Result := (sl[9] <> 'AM') and (sl[9] <> 'PM');
        if Result then
          Result := not ((Pos(':', sl[8]) = 0) and (StrToMonth(sl[6]) > 0));
      end;
    finally
      sl.Free;
    end;
  end
  else begin
    sl := TStringList.Create;
    try
      Split(sl, UData, ' ', True);

      if (sl.Count > 3) and IsNumeric(sl[0]) then begin
        Result := IsValidEmulUnixPermissions(sl[1]);
        if not Result then
          Result := IsNumeric(sl[1]) and IsValidEmulUnixPermissions(sl[2]);
      end;
    finally
      sl.Free;
    end;
  end;
end;

class function TScFTP_UnixParser.IsUnitreeBanner(const Data: string): boolean;
begin
  Result := (Length(Data) >= 4{/().}) and (Data[1] = '/') and (Copy(Data, Length(Data) - 1, 2) = ').') and (Pos('(', Data) > 0);
end;

class function TScFTP_UnixParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;

  procedure DeleteSuffix(var Str: string; const Suffix: string);
  var
    p: integer;
  begin
    p := Length(Str) - Length(Suffix) + 1;
    if AnsiPos(Suffix, Str) = p then
      Delete(Str, p, Length(Suffix));
  end;

const
  MonthNames: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

var
  TmpData, Lexem: string;
  sl: TStrings;
  HasYear, HasTime: boolean;
  IsGOSwitches: boolean;
  Year, Month, TmpMonth, Day, Hour, Min, Sec, MSec: word;
  CurYear, CurMonth, CurDay: word;
  BonkedYear, DayStr, FileName, Size, LinkedItemName: string;
  p: integer;
begin
  DecodeDate(Now, Year, Month, Day);
  DecodeTime(Now, Hour, Min, Sec, MSec);
  TmpData := TrimLeft(Item.Data);

  TScFTP_UnixBaseListItem(Item).Inode := 0;
  TScFTP_UnixBaseListItem(Item).LinkCount := 0;
  Item.NumberBlocks := 0;
  Item.GroupName := '';
  Item.OwnerName := '';

  if not IsValidEmulUnixPermissions(GetFirstWord(TmpData, ' ')) then begin
    TmpData := TrimLeft(TmpData);
    TScFTP_UnixBaseListItem(Item).Inode := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);

    // Blocks
    TmpData := TrimLeft(TmpData);
    if not IsValidEmulUnixPermissions(GetFirstWord(TmpData, ' ')) then
      Item.NumberBlocks := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);
  end;

  // Permissions
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Item.Permissions := Copy(Lexem, 1, 10);
  Item.PermissionsDisplay := Item.Permissions;
  TScFTP_UnixBaseListItem(Item).OwnerPermissions := Copy(Lexem, 2, 3);
  TScFTP_UnixBaseListItem(Item).GroupPermissions := Copy(Lexem, 5, 3);
  TScFTP_UnixBaseListItem(Item).OtherPermissions := Copy(Lexem, 8, 3);

  Item.FileType := fftFile;
  if Lexem <> '' then
    case UpperCase(Copy(Lexem, 1, 1))[1] of
      'D':
        Item.FileType := fftDirectory;
      'L':
        Item.FileType := fftSymLink;
      'B':
        Item.FileType := fftBlockDevice;
      'C':
        Item.FileType := fftCharDevice;
      'P':
        Item.FileType := fftFifo;
      'S':
        Item.FileType := fftSocket;
    end;

  // Count
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if not AnsiSameText(Lexem, 'folder') then begin
    if (Length(Lexem) > 3) and (Lexem[1] = '0') then
      TmpData := Copy(Lexem, 2, MaxInt) + ' ' + TrimLeft(TmpData)
    else
      TScFTP_UnixBaseListItem(Item).LinkCount := StrToIntDef(Lexem, 0);

    IsGOSwitches := False;
    sl := TStringList.Create;
    try
      Split(sl, Item.Data, ' ', True);

      if sl.Count > 2 then begin
        if IsNumeric(sl[0]) then
          sl.Delete(0);
        if IsNumeric(sl[0]) then
          sl.Delete(0);

        if (sl.Count > 5) and (StrToMonth(sl[3]) > 0) then
          IsGOSwitches := IsNumeric(sl[4]) and (IsNumeric(sl[5]) or (AnsiPos(':', sl[5]) > 0));
      end;
    finally
      sl.Free;
    end;

    if not IsGOSwitches then begin
      // Owner
      TmpData := TrimLeft(TmpData);
      Item.OwnerName := ExtractFirstWord(TmpData, ' ');

      // Group
      TmpData := TrimLeft(TmpData);
      Item.GroupName := ExtractFirstWord(TmpData, ' ');
    end;
  end;

  // Size
  TmpData := TrimLeft(TmpData);
  if (TmpData <> '') and IsAlpha(TmpData[1]) and (Identitifier <> TScFTP_UnitreeParser.Identitifier) then begin
    Size := Item.GroupName;
    Item.GroupName := Item.OwnerName;
    Item.OwnerName := '';

    if not IsNumeric(Size) then begin
      Item.OwnerName := Item.GroupName;
      Item.GroupName := '';

      repeat
        if Item.GroupName <> '' then
          Item.GroupName := Item.GroupName + ' ';

        Item.GroupName := Item.GroupName + Size;
        Item.OwnerName := Item.GroupName;
        TmpData := TrimLeft(TmpData);
        Size := ExtractFirstWord(TmpData, ' ');
      until IsNumeric(Size);
    end;
  end
  else begin
    Lexem := ExtractFirstWord(TmpData, ' ');
    if AnsiPos(',', Lexem) > 0 then begin
      TmpData := TrimLeft(TmpData);
      ExtractFirstWord(TmpData, ' ');
      Size := '';
    end
    else
      Size := Lexem;

    TmpData := TrimLeft(TmpData);
    if Size = 'AR' then begin
      if Item is TScFTP_UnitreeListItem then begin
        TScFTP_UnitreeListItem(Item).Migrated := True;
        TScFTP_UnitreeListItem(Item).FileFamily := ExtractFirstWord(TmpData, ' ');
      end;

      TmpData := TrimLeft(TmpData);
      Size := ExtractFirstWord(TmpData, ' ');
    end
    else
    if Size = 'DK' then begin
      if Item is TScFTP_UnitreeListItem then
        TScFTP_UnitreeListItem(Item).FileFamily := ExtractFirstWord(TmpData, ' ');

      TmpData := TrimLeft(TmpData);
      Size := ExtractFirstWord(TmpData, ' ');
    end;
  end;

  Item.Size := StrToInt64Def(Size, 0);

  // Month
  HasTime := False;
  TmpData := TrimLeft(TmpData);
  if AnsiPos(ChineseYear, TmpData) > 0 then begin
    Year := StrToInt(ExtractFirstWord(TmpData, ChineseYear));
    TmpData := TrimLeft(TmpData);
    Hour := 0;
    Min := 0;
    Sec := 0;
    MSec := 999;
    HasYear := False;
  end
  else
    HasYear := True;

  if AnsiPos(ChineseDay, TmpData) > 0 then begin
    Month := StrToInt(ExtractFirstWord(TmpData, ChineseMonth));
    TmpData := TrimLeft(TmpData);
    Day := StrToInt(ExtractFirstWord(TmpData, ChineseDay));
    TmpData := TrimLeft(TmpData);
    if HasYear then begin
      Lexem := ExtractFirstWord(TmpData, ' ');
      HasTime := True;
    end;

    HasYear := False;
  end
  else begin
    BonkedYear := ExtractFirstWord(TmpData, ' ');
    BonkedYear := StringReplace(BonkedYear, '-', ' ', [rfReplaceAll]);
    BonkedYear := StringReplace(BonkedYear, '/', ' ', [rfReplaceAll]);
    TmpData := BonkedYear + ' ' + TmpData;
    Lexem := ExtractFirstWord(TmpData, ' ');

    if (Length(Lexem) > 3) and IsNumeric(Lexem) then begin
      Year := StrToIntDef(Lexem, Year);
      Lexem := ExtractFirstWord(TmpData, ' ');
    end;

    TmpData := TrimLeft(TmpData);
    if (Lexem <> '') and (Lexem[Length(Lexem)] = '.') then
      Delete(Lexem, Length(Lexem), 1);

    DeleteSuffix(Lexem, KoreanMonth);
    DeleteSuffix(Lexem, KoreanEUCMonth);
    DeleteSuffix(Lexem, JapaneseMonth);

    if IsNumeric(Lexem) then begin
      Month := StrToIntDef(Lexem, Month);
      Lexem := GetFirstWord(TmpData, ' ');
      if (Lexem <> '') and (Lexem[Length(Lexem)] = ',') then
        Delete(Lexem, Length(Lexem), 1);
      if (Lexem <> '') and (Lexem[Length(Lexem)] = '.') then
        Delete(Lexem, Length(Lexem), 1);

      TmpMonth := StrToMonth(Lexem);
      if TmpMonth > 0 then begin
        Day := Month;
        Lexem := ExtractFirstWord(TmpData, ' ');
        TmpData := TrimLeft(TmpData);
        Month := TmpMonth;
      end
      else begin
        if Month > 12 then begin
          Day := Month;
          Lexem := ExtractFirstWord(TmpData, ' ');
          TmpData := TrimLeft(TmpData);
          Month := StrToIntDef(Lexem, Month);
        end
        else begin
          // Day
          Lexem := ExtractFirstWord(TmpData, ' ');
          TmpData := TrimLeft(TmpData);
          DeleteSuffix(Lexem, KoreanDay);
          DeleteSuffix(Lexem, JapaneseDay);
          Day := StrToIntDef(Lexem, Day);
        end;
      end;

      HasYear := True;
    end
    else begin
      Month := StrToMonth(Lexem);
      if Month = 0 then begin
        Year := StrToIntDef(ExtractFirstWord(Lexem, '.'), Year);
        Month := StrToIntDef(ExtractFirstWord(Lexem, '.'), 0);
        Day := StrToInt(Lexem);
        HasYear := False;
      end
      else begin
        // Day
        Lexem := ExtractFirstWord(TmpData, ' ');
        TmpData := TrimLeft(TmpData);
        DeleteSuffix(Lexem, KoreanDay);
        DeleteSuffix(Lexem, JapaneseDay);
        Day := StrToIntDef(Lexem, Day);
        HasYear := True;
      end;
    end;
  end;

  // Year
  if HasYear then begin
    Lexem := ExtractFirstWord(TmpData, ' ');
    DeleteSUffix(Lexem, JapaneseYear);

    if AnsiPos(':', Lexem) = 0 then begin
      Year := StrToIntDef(Lexem, Year);
      Hour := 0;
      Min := 0;
      Sec := 0;
      MSec := 999;
    end
    else
      HasTime := True;
  end;

  // Time
  if HasTime then begin
    Year := DetectYear(Day, Month);
    Hour := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);

    if (AnsiPos(':', Lexem) > 0) and IsNumeric(GetFirstWord(TmpData, ' ')) then begin
      Min := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);
      Sec := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);
      MSec := StrToIntDef(ExtractFirstWord(Lexem,':'), 999);
      Lexem := ExtractFirstWord(TmpData, ' ');
      Year := StrToIntDef(Lexem, Year);
    end
    else begin
      Min := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);
      Sec := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);
      MSec := StrToIntDef(ExtractFirstWord(Lexem, ' '), 999);
    end;
  end;

  if (Month = 2) and (Day = 29) and not IsLeapYear(Year) then
    Item.ModifiedDate := EncodeDate(Year, Month, Day - 1) + EncodeTime(Hour, Min, Sec, MSec)
  else
    Item.ModifiedDate := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);

  DayStr := IntToStr(Day);
  if Length(DayStr) < 2 then
    DayStr := '0' + DayStr;

  // correct year
  DecodeDate(Now, CurYear, CurMonth, CurDay);
  if (Year < CurYear) and ((Now - Item.ModifiedDate) > 90) and
     (Pos(IntToStr(Month) + '  ' + IntToStr(Year), Item.Data) = 0) and
     (Pos(IntToStr(Month) + ' ' + DayStr + '  ' + IntToStr(Year), Item.Data) = 0) and
     (Pos(MonthNames[Month] + '  ' + IntToStr(Year), Item.Data) = 0) and
     (Pos(MonthNames[Month] + ' ' + DayStr + '  ' + IntToStr(Year), Item.Data) = 0)
  then
    if IncMonth(Item.ModifiedDate, 12) <= (Now + 7) then begin
      Inc(Year);
      Item.ModifiedDate := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
    end;

  // Name
  FileName := TmpData;

  if Item.FileType = fftSymLink then begin
    p := AnsiPos(' -> ', FileName);
    LinkedItemName := Copy(FileName, p + 4, Length(FileName) - p - 3);
    FileName := Copy(FileName, 1, p - 1);

    if (LinkedItemName <> '') and (LinkedItemName[Length(LinkedItemName)] = '/') then begin
      Item.FileType := fftSymLinkDir;
      LinkedItemName := Copy(LinkedItemName, 1, Length(LinkedItemName) - 1);
    end;
    TScFTP_UnixBaseListItem(Item).LinkedItemName := LinkedItemName;
  end;

  if CharInSet(FileName[Length(FileName)], ['/', '*']) then
    FileName := Copy(FileName, 1, Length(FileName) - 1);

  if Path <> '' then begin
    Item.LocalFileName := FileName;
    FileName := Path + '/' + FileName;
    if (Length(FileName) >= 2) and (FileName[1] = '.') and (FileName[2] = '/') then begin
      Delete(FileName, 1, Length('./'));
      if (Length(FileName) >= 1) and (FileName[1] = '/') then
        Delete(FileName, 1, Length('/'));
    end;
  end;

  Item.FileName := FileName;
  Result := True;
end;

class function TScFTP_UnixParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  Str: string;
  Path: string;
  NewItem: TScFTPListItem;
  i: integer;
begin
  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if not ((Str = '') or IsTotalLine(Str) or IsUnitreeBanner(Str) or AnsiSameText(Copy(Str, 1, 8), '/bin/ls:')) then begin
      if not IsUnixStr(Str) and IsSubDirContentBanner(Str) then
        Path := Copy(Str, 1, Length(Str) - 1)
      else begin
        NewItem := CreateItem(Dir);
        NewItem.Data := Str;
        Result := ParseLine(NewItem, Path);
        if not Result then begin
          FreeAndNil(NewItem);
          Exit;
        end;
      end;
    end;
  end;

  Result := True;
end;

{ TScFTP_UnitreeParser }

class function TScFTP_UnitreeParser.Identitifier: string;
begin
  Result := 'Unitree';
end;

class function TScFTP_UnitreeParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_UnitreeListItem;
end;

{ TScFTP_UnisysClearpathParser }

class function TScFTP_UnisysClearpathParser.Identitifier: string;
begin
  Result := 'Unisys Clearpath';
end;

class function TScFTP_UnisysClearpathParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_UnisysClearpathListItem;
end;

class function TScFTP_UnisysClearpathParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);
    if Cols.Count > 2 then
      Result := (Cols[0] = 'Report') and (Cols[1] = 'for:');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_UnisysClearpathParser.IsFooter(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);
    if Cols.Count = 4 then
      if (Cols[1] = 'Files') or (Cols[1] = 'File') then
        Result := (Cols[3] = 'Octets') or (Cols[3] = 'Octet');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_UnisysClearpathParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  Item.FileType := fftFile;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);

    if Lexems.Count > 4 then begin
      Item.FileName := Lexems[0];
      TScFTP_UnisysClearpathListItem(Item).FileKind := Lexems[1];

      if IsNumeric(Lexems[2]) then begin
        Item.Size := StrToInt64Def(Lexems[2], 0);
        Item.SizeAvailable := True;

        if IsDate(Lexems[3], MMDDYY) then begin
          TScFTP_UnisysClearpathListItem(Item).CreationDate := StrToDate(Lexems[3], MMDDYY);
          if IsTime(Lexems[4], HHMMSS) then begin
            TScFTP_UnisysClearpathListItem(Item).CreationDate := TScFTP_UnisysClearpathListItem(Item).CreationDate + StrToTime(Lexems[4], HHMMSS);
            Result := True;
          end;
        end;
      end;

      Lexems.Clear;
      Split(Lexems, Item.FileName, '/', True);
      if Lexems.Count > 0 then
        Item.LocalFileName := Lexems[Lexems.Count - 1]
      else
        Result := False;
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_UnisysClearpathParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  NewItem: TScFTPListItem;
  Str: string;
  i: integer;
begin
  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if (Trim(Str) <> '') and (Str[1] <> ' ') and not (IsHeader(Str) or IsFooter(Str)) then begin
      NewItem := CreateItem(Dir);
      NewItem.Data := UnfoldLines(Listing, i);
      Result := ParseLine(NewItem);
      if not Result then begin
        FreeAndNil(NewItem);
        Exit;
      end;
    end;
  end;

  Result := True;
end;

{ TScFTP_TSXPlusParser }

class function TScFTP_TSXPlusParser.Identitifier: string;
begin
  Result := 'TSX+';
end;

class function TScFTP_TSXPlusParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SimpleListItem;
end;

class function TScFTP_TSXPlusParser.IsHeader(const Data: string): boolean;
begin
  Result := False;
end;

class function TScFTP_TSXPlusParser.IsFooter(const Data: string): boolean;
var
  TmpData, Lexem: string;
begin
  Result := False;
  TmpData := Data;
  Lexem := ExtractFirstWord(TmpData, '[');
  if TmpData = '' then
    Exit;

  if TrimRight(Lexem) = 'Directory' then begin
    ExtractFirstWord(TmpData, ']');
    if TmpData = '' then
      Exit;

    TmpData := TrimLeft(TmpData);
    if (TmpData <> '') and (TmpData[1] = '/') then begin
      Delete(TmpData, 1, 1);

      if Pos('Files', TmpData) > 0 then begin
        ExtractFirstWord(TmpData, '/');
        if TmpData = '' then
          Exit;

        Result := Pos('Block', TmpData) > 0;
      end;
    end;
  end;
end;

class function TScFTP_TSXPlusParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  i: integer;
begin
  Result := False;

  for i := Listing.Count - 1 downto 0 do begin
    if (Listing[i] <> '') and IsFooter(Listing[i]) then begin
      Result := True;
      Break;
    end;
  end;
end;

class function TScFTP_TSXPlusParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  NewItem: TScFTPListItem;
  TmpData, Ext: string;
begin
  Result := True;
  TmpData := TrimLeft(Item.Data);

  Item.FileName := ExtractFirstWord(TmpData, '.');
  Ext := ExtractFirstWord(TmpData, ' ');
  if Ext = 'dsk' then
    Item.FileType := fftDirectory
  else begin
    Item.FileType := fftFile;
    Item.FileName := Item.FileName + '.' + Ext;
  end;

  TmpData := TrimLeft(TmpData);
  Item.NumberBlocks := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimRight(TmpData);
  if TmpData <> '' then begin
    NewItem := CreateItem(Item.Collection as TScFTPDirectoryListing);
    NewItem.Data := TmpData;
    Result := ParseLine(NewItem, Path);
    if not Result then begin
      FreeAndNil(NewItem);
      Exit;
    end;

    NewItem.Data := Item.Data;
  end;
end;

{ TScFTP_TOPS20Parser }

class function TScFTP_TOPS20Parser.Identitifier: string;
begin
  Result := 'TOPS20';
end;

class function TScFTP_TOPS20Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_CreationDateListItem;
end;

class function TScFTP_TOPS20Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
  Str, Lexem: string;
  i: integer;
begin
  Result := GetFirstWord(SysDescript, ' ') = 'TOPS20';
  if Result then
    Exit;

  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ',', False);

    if Lexems.Count = 0 then
      Exit;

    if PatternsCount(';', Lexems[0]) = 2 then begin
      if Lexems.Count > 3 then begin
        Lexem := Lexems[2];
        Result := IsDate(ExtractFirstWord(Lexem, ' '), DDMonthYY);
        if Result then begin
          Result := IsTime(Lexem, HHMMSS);

          if Result then begin
            Lexem := Lexems[3];
            Result := IsDate(ExtractFirstWord(Lexem, ' '), DDMonthYY);
            if Result then
              Result := IsTime(Lexem, HHMMSS);
          end;
        end;
      end;
    end;

    Str := Listing[0];
    if (Pos(':<', Str) > 0) and (Pos(':<', Str) < Pos('>', Str)) then begin
      Result := True;
      for i := 1 to Listing.Count - 1 do begin
        Lexems.Clear;
        Result := Pos(' ', Listing[i]) = 0;
        if Result then begin
          Split(Lexems, Listing[i], '.', False);
          if Lexems.Count = 3 then
            Result := IsNumeric(Lexems[2]);
        end;

        if not Result then
          Break;
      end;
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_TOPS20Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;

  function StripBuild(const FileName: string): string;
  var
    i, p: integer;
  begin
    p := 0;
    for i := Length(FileName) downto 1 do begin
      if FileName[i] = '.' then begin
        p := i;
        Break;
      end;
    end;

    if p = 0 then
      Result := FileName
    else
      Result := Copy(FileName, 1, p - 1);
  end;

var
  TmpData: string;
begin
  TmpData := Item.Data;

  if (Pos(':<', TmpData) > 0) and (Pos('>', TmpData) = Length(TmpData)) then begin
    Item.FileName := TmpData;
    Item.FileType := fftDirectory;
    ExtractFirstWord(TmpData, ':<');
    TmpData := ExtractFirstWord(TmpData, '>');
    Item.LocalFileName := LowerCase(ExtractFirstWord(TmpData, '.'));
    Item.ModifiedAvailable := False;
    Item.SizeAvailable := False;
    Result := True;
    Exit;
  end;

  if (TmpData <> '') and (TmpData[1] = '<') then begin
    Item.FileName := ExtractFirstWord(TmpData, ';');
    ExtractFirstWord(TmpData, ';');
    ExtractFirstWord(TmpData, ',');
    ExtractFirstWord(TmpData, ',');

    TScFTP_CreationDateListItem(Item).CreationDate := StrToDate(ExtractFirstWord(TmpData, ' '), DDMonthYY);
    TScFTP_CreationDateListItem(Item).CreationDate := TScFTP_CreationDateListItem(Item).CreationDate + StrToTime(Trim(ExtractFirstWord(TmpData, ',')), HHMMSS);
    Item.ModifiedDate := StrToDate(ExtractFirstWord(TmpData, ' '), DDMonthYY);
    Item.ModifiedDate := Item.ModifiedDate + StrToTime(Trim(TmpData), HHMMSS);

    TmpData := LowerCase(Item.FileName);
    ExtractFirstWord(TmpData, '>');

    if Pos('.DIRECTORY.', TmpData) > 0 then begin
      Item.FileType := fftDirectory;
      Item.LocalFileName := ExtractFirstWord(TmpData, '.');
    end
    else
      Item.LocalFileName := StripBuild(TmpData);
  end
  else begin
    Item.FileName := TmpData;
    Item.LocalFileName := LowerCase(StripBuild(TmpData));
    Item.ModifiedAvailable := False;
    Item.SizeAvailable := False;
    if Pos('.DIRECTORY.', TmpData) > 0 then
      Item.FileType := fftDirectory;
  end;

  Result := True;
end;

{ TScFTP_TandemGuardianParser }

class function TScFTP_TandemGuardianParser.Identitifier: string;
begin
  Result := 'Tandem NonStop Guardian';
end;

class function TScFTP_TandemGuardianParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_TandemGuardianListItem;
end;

class function TScFTP_TandemGuardianParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count = 7 then
      Result := (Cols[0] = 'File') and
        (Cols[1] = 'Code') and
        (Cols[2] = 'EOF') and
        (Cols[3] = 'Last') and
        (Cols[4] = 'Modification') and
        (Cols[5] = 'Owner') and
        (Cols[6] = 'RWEP')
  finally
    Cols.Free;
  end;
end;

class function TScFTP_TandemGuardianParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
  Year, Month, Day: integer;
begin
  Result := True;

  TmpData := Trim(Item.Data);
  Item.FileType := fftFile;
  Item.FileName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  TScFTP_TandemGuardianListItem(Item).Code := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);
  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Day := StrToIntDef(ExtractFirstWord(Lexem, '-'), 1);
  Month := StrToMonth(ExtractFirstWord(Lexem, '-'));
  Year := StrToIntDef(ExtractFirstWord(Lexem, ' '), 1989);
  Year := CheckYear4(Year);
  Item.ModifiedDate := EncodeDate(Year, Month, Day);

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

  TmpData := TrimLeft(TmpData);
  Item.GroupName := ExtractFirstWord(TmpData, ',');
  TmpData := TrimLeft(TmpData);
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');

  Item.Permissions := UnquoteStr(TmpData);
  Item.PermissionsDisplay := '"' + Item.Permissions + '"';
end;

{ TScFTP_SuperTCPParser }

class function TScFTP_SuperTCPParser.Identitifier: string;
begin
   Result := 'SuperTCP';
end;

class function TScFTP_SuperTCPParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SuperTCPListItem;
end;

class function TScFTP_SuperTCPParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str, Lexem: string;
  i: integer;
begin
  Result := False;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];
    Result := IsValidMSDOSFileName(ExtractFirstWord(Str, ' '));
    if not Result then
      Exit;

    Str := TrimLeft(Str);
    Lexem := ExtractFirstWord(Str, ' ');
    Result := (Lexem = '<DIR>') or IsNumeric(Lexem);
    if not Result then
      Exit;

    Str := TrimLeft(Str);
    Result := IsDate(ExtractFirstWord(Str, ' '), MMDDYY);
    if Result then begin
      Str := TrimLeft(Str);
      Result := IsTime(ExtractFirstWord(Str, ' '), HHMMSS);
    end;

    if Result then begin
      if Str <> '' then
        Result := IsValidWin32FileName(Str);
    end;

    if not Result then
      Break;
  end;
end;

class function TScFTP_SuperTCPParser.IsValidMSDOSFileName(const FileName: string): boolean;
const
  VALID_DOS_CHARS =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrtstuvwxyz0123456789_$~!#%&-{}()@''' + Char(180);

var
  FName, Ext: string;
  i: integer;
begin
  Result := False;
  if (FileName = '.') or (FileName = '..') then begin
    Result := True;
    Exit;
  end;

  Ext := FileName;
  FName := ExtractFirstWord(Ext, '.');
  if (Length(FName) > 0) and (Length(FName) < 9) then begin
    for i := 1 to Length(FName) do begin
      if AnsiPos(FName[i], VALID_DOS_CHARS) = 0 then
        Exit;
    end;

    for i := 1 to Length(Ext) do begin
      if AnsiPos(Ext[i], VALID_DOS_CHARS) = 0 then
        Exit;
    end;

    Result := True;
  end;
end;

class function TScFTP_SuperTCPParser.IsValidWin32FileName(const FileName: string): boolean;
const
  WIN32_INVALID_CHARS = '"*/:<>?\|' + #0;
  WIN32_INVALID_LAST = ' .';

var
  i: integer;
begin
  Result := False;
  if (FileName = '.') or (FileName = '..') then begin
    Result := True;
    Exit;
  end;

  if Length(FileName) > 0 then begin
    if AnsiPos(FileName[Length(FileName)], WIN32_INVALID_LAST) > 0 then
      Exit;

    for i := 1 to Length(FileName) do begin
      if AnsiPos(FileName[i], WIN32_INVALID_CHARS) > 0 then
        Exit;
    end;

    Result := True;
  end;
end;

class function TScFTP_SuperTCPParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
begin
  TmpData := Item.Data;

  Lexem := ExtractFirstWord(TmpData, ' ');
  Item.FileName := Lexem;
  TScFTP_SuperTCPListItem(Item).ShortFileName := Lexem;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if Lexem = '<DIR>' then begin
    Item.FileType := fftDirectory;
    Item.SizeAvailable := False;
  end
  else begin
    Item.FileType := fftFile;
    Result := IsNumeric(Lexem);
    if not Result then
      Exit;
    Item.Size := StrToInt64Def(Lexem, 0);
  end;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if IsDate(Lexem, MMDDYY) then
    Item.ModifiedDate := StrToDate(Lexem, MMDDYY)
  else begin
    Result := False;
    Exit;
  end;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Result := IsTime(Lexem, HHMMSS);
  if Result then
    Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

  if TmpData <> '' then
    Item.FileName := TmpData;
end;

{ TScFTP_StratusVOSParser }

class function TScFTP_StratusVOSParser.Identitifier: string;
begin
  Result := 'Stratus VOS';
end;

class function TScFTP_StratusVOSParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_StratusVOSListItem;
end;

class function TScFTP_StratusVOSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  FileType: TScFTPFileType;
  i: integer;
begin
  Result := False;
  FileType := fftFile;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if Str <> '' then begin
      if IsFilesHeader(Str) then
        FileType := fftFile
      else
      if IsDirsHeader(Str) then
        FileType := fftDirectory
      else
      if IsLinksHeader(Str) then
        FileType := fftSymLink
      else
        case FileType of
          fftFile:
            if not IsValidFileEntry(Str) then
              Exit;
          fftDirectory:
            if not IsValidDirEntry(Str) then
              Exit;
        end;
    end;
  end;

  Result := True;
end;

class function TScFTP_StratusVOSParser.IsDirsHeader(const Line: string): boolean;
const
  SDirs = 'Dirs: ';
begin
  Result := (Line <> '') and (Copy(Line, 1, Length(SDirs)) = SDirs);
end;

class function TScFTP_StratusVOSParser.IsFilesHeader(const Line: string): boolean;
const
  SFiles = 'Files: ';
  SBlocks = 'Blocks: ';
begin
  Result := (Line <> '') and (Copy(Line, 1, Length(SFiles)) = SFiles) and (Pos(SBlocks, Line) > 8);
end;

class function TScFTP_StratusVOSParser.IsLinksHeader(const Line: string): boolean;
const
  SLinks = 'Links: ';
begin
  Result := (Line <> '') and (Copy(Line, 1, Length(SLinks)) = SLinks);
end;

class function TScFTP_StratusVOSParser.IsValidDirEntry(const Line: string): boolean;
var
  Str, StrDate: string;
begin
  Result := False;
  Str := Line;
  if (Str <> '') and (Str[1] = ' ') then
    Delete(Str, 1, 1);
  if Length(ExtractFirstWord(Str, ' ')) <> 1 then
    Exit;

  Str := TrimLeft(Str);
  if not IsNumeric(ExtractFirstWord(Str, ' ')) then
    Exit;

  Str := TrimLeft(Str);
  StrDate := ExtractFirstWord(Str, ' ');
  if not IsDate(StrDate, YYMMDD) then
    Exit;

  Str := TrimLeft(Str);
  StrDate := ExtractFirstWord(Str, ' ');
  Result := IsTime(StrDate, HHMMSS);
end;

class function TScFTP_StratusVOSParser.IsValidFileEntry(const Line: string): boolean;
var
  Str, StrDate: string;
begin
  Result := False;
  Str := Line;
  if (Str <> '') and (Str[1] = ' ') then
    Delete(Str, 1, 1);
  if Length(ExtractFirstWord(Str, ' ')) <> 1 then
    Exit;

  Str := TrimLeft(Str);
  if not IsNumeric(ExtractFirstWord(Str, ' ')) then
    Exit;

  Str := TrimLeft(Str);
  StrDate := ExtractFirstWord(Str, ' ');
  if (StrDate = '') or not IsNumeric(Copy(StrDate, 1, 2)) then begin
    Str := TrimLeft(Str);
    StrDate := ExtractFirstWord(Str, ' ');
  end;
  if not IsDate(StrDate, YYMMDD) then
    Exit;

  Str := TrimLeft(Str);
  Result := IsTime(ExtractFirstWord(Str, ' '), HHMMSS);
end;

class function TScFTP_StratusVOSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
begin
  Result := False;

  case Item.FileType of
    fftFile: begin
      TmpData := Item.Data;
      if (TmpData <> '') and (TmpData[1] = ' ') then
        Delete(TmpData, 1, 1);
      TScFTP_StratusVOSListItem(Item).FAccess := ExtractFirstWord(TmpData, ' ');
      Item.Permissions := TScFTP_StratusVOSListItem(Item).Access;
      Item.PermissionsDisplay := Item.Permissions;
      if Length(TScFTP_StratusVOSListItem(Item).FAccess) <> 1 then begin
        TScFTP_StratusVOSListItem(Item).FAccess := '';
        Exit;
      end;

      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsNumeric(Lexem) then
        Exit;
      Item.NumberBlocks := StrToIntDef(Lexem, 0);
      Item.Size := Int64(Item.NumberBlocks) * 4096;
      Item.SizeAvailable := True;

      TmpData := TrimLeft(TmpData);
      TScFTP_StratusVOSListItem(Item).FileFormat := ExtractFirstWord(TmpData, ' ');

      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsDate(Lexem, YYMMDD) then
        Exit;
      Item.ModifiedDate := StrToDate(Lexem, YYMMDD);

      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsTime(Lexem, HHMMSS) then
        Exit;
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

      Item.FileName := TrimLeft(TmpData);
    end;

    fftDirectory: begin
      TmpData := Item.Data;
      if (TmpData <> '') and (TmpData[1] = ' ') then
        Delete(TmpData, 1, 1);

      TScFTP_StratusVOSListItem(Item).FAccess := ExtractFirstWord(TmpData, ' ');
      if Length(TScFTP_StratusVOSListItem(Item).FAccess) <> 1 then begin
        TScFTP_StratusVOSListItem(Item).FAccess := '';
        Exit;
      end;

      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsNumeric(Lexem) then
        Exit;
      Item.NumberBlocks := StrToIntDef(Lexem, 0);
      Item.Size := Int64(Item.NumberBlocks) * 4096;
      Item.SizeAvailable := True;

      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsDate(Lexem, YYMMDD) then
        Exit;
      Item.ModifiedDate := StrToDate(Lexem, YYMMDD);
      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsTime(Lexem, HHMMSS) then
        Exit;
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

      Item.FileName := TrimLeft(TmpData);
    end;

    fftSymLink: begin
      TmpData := Item.Data;

      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsDate(Lexem, YYMMDD) then
        Exit;
      Item.ModifiedDate := StrToDate(Lexem, YYMMDD);

      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if not IsTime(Lexem, HHMMSS) then
        Exit;
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

      TmpData := TrimLeft(TmpData);
      Item.FileName := TrimRight(ExtractFirstWord(TmpData, '->'));

      TScFTP_StratusVOSListItem(Item).LinkedItemName := Trim(TmpData);
      Item.SizeAvailable := False;
    end;
  end;

  Result := True;
end;

class function TScFTP_StratusVOSParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  NewItem: TScFTPListItem;
  FileType: TScFTPFileType;
  IsContinuedLine: boolean;
  Str, Line: string;
  i: integer;
begin
  Result := False;
  FileType := fftFile;
  IsContinuedLine := False;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    if Str <> '' then begin
      if IsFilesHeader(Str) then
        FileType := fftFile
      else
      if IsDirsHeader(Str) then
        FileType := fftDirectory
      else
      if IsLinksHeader(Str) then
        FileType := fftSymLink
      else
      if FileType <> fftSymLink then begin
        NewItem := CreateItem(Dir);
        NewItem.FileType := FileType;
        NewItem.Data := Str;
        if not ParseLine(NewItem) then begin
          FreeAndNil(NewItem);
          Exit;
        end;
      end
      else
      if not IsContinuedLine then begin
        Line := TrimRight(Str);
        if (Length(Line) >= 2) and (Copy(Line, Length(Line) - 1, 2) = '->') then
          IsContinuedLine := True
        else begin
          NewItem := CreateItem(Dir);
          NewItem.FileType := FileType;
          NewItem.Data := Line;
          if not ParseLine(NewItem) then begin
            FreeAndNil(NewItem);
            Exit;
          end;
        end;
      end
      else begin
        if (Str <> '') and (Str[1] = '+') then
          Delete(Str, 1, 1);
        Line := Line + Str;
        IsContinuedLine := False;

        if i < (Listing.Count - 2) then begin
          if (Listing[i + 1] <> '') and (Listing[i + 1][1] = '+') then
            IsContinuedLine := True
          else begin
            NewItem := CreateItem(Dir);
            NewItem.FileType := FileType;
            NewItem.Data := Line;
            if not ParseLine(NewItem) then begin
              FreeAndNil(NewItem);
              Exit;
            end;
          end;
        end
        else begin
          NewItem := CreateItem(Dir);
          NewItem.FileType := FileType;
          NewItem.Data := Line;
          if not ParseLine(NewItem) then begin
            FreeAndNil(NewItem);
            Exit;
          end;
        end;
      end;
    end;
  end;

  Result := True;
end;

{ TScFTP_SterCommEntUxParser }

class function TScFTP_SterCommEntUxParser.Identitifier: string;
begin
  Result := 'CONNECT:Enterprise for UNIX';
end;

class function TScFTP_SterCommEntUxParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SterCommEntUxListItem;
end;

class function TScFTP_SterCommEntUxParser.IsValidFlags(const Value: string): boolean;
var
  i: integer;
begin
  Result := False;
  if Value = '' then
    Exit;

  for i := 1 to Length(Value) do begin
    if Pos(Value[i], 'ACDEGIMNPRTUXS- ') = 0 then
      Exit;
  end;

  Result := True;
end;

class function TScFTP_SterCommEntUxParser.IsValidPattern(const Raw: string; const Patterns: array of string): boolean;
var
  i: integer;
begin
  Result := False;
  if Raw = '' then
    Exit;

  if (Length(Raw) = 3) and (Raw = '---') then begin
    Result := True;
    Exit;
  end;

  for i := Low(Patterns) to High(Patterns) do begin
    if Raw = Patterns[i] then begin
      Result := True;
      Exit;
    end;
  end;
end;

class function TScFTP_SterCommEntUxParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
const
  Protocol_Flags: array [0..6] of string =
    ('A', 'B', 'F', 'G', 'H', 'Q', 'W');
  Protocol_Ext_Flags: array [0..7] of string =
    ('TCP', 'BSC', 'FTP', 'FTP', 'HTTP', 'ASY', 'AS2', 'FTS');

var
  Str, Lexem: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];

  Lexem := ExtractFirstWord(Str, ' ');
  if (Length(Lexem) = 13) and IsValidFlags(Copy(Lexem, 1, 10)) then
    Result := IsValidPattern(Copy(Lexem, 11, 3), Protocol_Flags);

  if Result then begin
    Str := ExtractFirstWord(Str, ' ');
    if Length(Str) = 1 then
      Result := IsValidPattern(Str, Protocol_Flags)
    else
      Result := IsValidPattern(Str, Protocol_Ext_Flags);
  end;
end;

class function TScFTP_SterCommEntUxParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
  Year, Month, Day, Hour, Min, Sec: word;
begin
  DecodeDate(Now, Year, Month, Day);
  Hour := 0;
  Min := 0;
  Sec := 0;

  TmpData := TrimLeft(Item.Data);
  TScFTP_SterCommEntUxListItem(Item).ProtocolFlags := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  TScFTP_SterCommEntUxListItem(Item).ProtocolIndicator := ExtractFirstWord(TmpData, ' ');
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');

  while True do begin
    TmpData := TrimLeft(TmpData);
    Lexem := ExtractFirstWord(TmpData, ' ');
    if TmpData <> '' then begin
      if IsNumeric(TmpData[1]) then
        Break;
      Item.Size := StrToInt64Def(Lexem, 0);
    end;
  end;

  Month := StrToMonth(Lexem);
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Day := StrToIntDef(Lexem, Day);
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if Pos(':', Lexem) > 0 then begin
    Year := DetectYear(Day, Month);
    Hour := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);
    Min := StrToIntDef(ExtractFirstWord(Lexem, ':'), 0);
  end
  else
    Year := StrToIntDef(Lexem, Year);

  Item.FileName := TmpData;
  Item.ModifiedDate := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, 0);

  Result := True;
end;

{ TScFTP_SterCommEntUxNSParser }

class function TScFTP_SterCommEntUxNSParser.Identitifier: string;
begin
  Result := TScFTP_SterCommEntUxParser.Identitifier + '$$';
end;

class function TScFTP_SterCommEntUxNSParser.IsValidDate(const StrDate: string): boolean;
var
  TmpStr, DatePart: string;
  Month, Day, Hour, Min: word;
begin
  TmpStr := StrDate;
  DatePart := ExtractFirstWord(TmpStr, '-');
  Month := StrToIntDef(Copy(DatePart, 3, 2), 0);
  Result := (Month > 0) and (Month < 13);
  if not Result then
    Exit;

  Day := StrToIntDef(Copy(DatePart, 5, 2), 0);
  Result := (Day > 0) and (Day <= 31);
  if not Result then
    Exit;

  Hour := StrToIntDef(Copy(TmpStr, 1, 2), 0);
  Result := (Hour > 0) and (Hour <= 24);
  if not Result then
    Exit;

  Min := StrToIntDef(Copy(TmpStr, 3, 2), 0);
  Result := Min < 60;
end;

class function TScFTP_SterCommEntUxNSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
const
  Data_Flags: array [0..2] of string =
    ('BIN', 'ASC', 'EBC');
  Protocol_Flags: array [0..7] of string =
    ('TCP', 'BSC', 'FTP', 'FTP', 'HTTP', 'ASY', 'AS2', 'FTS');

var
  Str, Flags: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  if IsFooter(Str) then begin
    Result := True;
    Exit;
  end;

  if Pos('>', Str) > 0 then begin
    ExtractFirstWord(Str, '>');
    if (Str <> '') and (Str[1] = '+') then
      Delete(Str, 1, 1);

    Str := TrimLeft(Str);
    if IsValidDate(ExtractFirstWord(Str, ' ')) then begin
      if Length(Str) < 7 then
        Flags := Str
      else
        Flags := Copy(Str, Length(Str) - 6, 7);

      if TScFTP_SterCommEntUxParser.IsValidPattern(Copy(Flags, 1, 3), Protocol_Flags) and
         TScFTP_SterCommEntUxParser.IsValidPattern(Copy(Flags, 5, 3), Data_Flags) then
      begin
        Str := Copy(Str, 1, Length(Str) - 7);
        Result := TScFTP_SterCommEntUxParser.IsValidFlags(Str);
      end;
    end;
  end;
end;

class function TScFTP_SterCommEntUxNSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Year, Month, Day, Hour, Min: word;
begin
  TmpData := Item.Data;
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  ExtractFirstWord(TmpData, '<');
  Item.FileName := ExtractFirstWord(TmpData, '>');

  if (TmpData <> '') and (TmpData[1] = '+') then
    Delete(TmpData, 1, 1);
  TmpData := TrimLeft(TmpData);
  TmpData := Copy(TmpData, 1, 11);
  Year := StrToIntDef(Copy(TmpData, 1, 2), 0);
  Year := CheckYear4(Year);
  Month := StrToIntDef(Copy(TmpData, 3, 2), 0);
  Day := StrToIntDef(Copy(TmpData, 5, 2), 0);

  if (TmpData <> '') and (TmpData[1] = '+') then
    Delete(TmpData, 1, 1);
  ExtractFirstWord(TmpData, '-');
  Item.ModifiedDate := EncodeDate(Year, Month, Day);
  Hour := StrToIntDef(Copy(TmpData, 1, 2), 0);
  Min := StrToIntDef(Copy(TmpData, 3, 2), 0);
  Item.ModifiedDate := Item.ModifiedDate + EncodeTime(Hour, Min, 0, 0);

  Result := True;
end;

{ TScFTP_SterCommEntUxRootParser }

class function TScFTP_SterCommEntUxRootParser.Identitifier: string;
begin
  Result := TScFTP_SterCommEntUxParser.Identitifier + ' ROOT';
end;

class function TScFTP_SterCommEntUxRootParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SimpleListItem;
end;

class function TScFTP_SterCommEntUxRootParser.IsFooter(const Data: string): boolean;
const
  STotal = 'Total number of Mailboxes = ';
begin
  Result := (Data <> '') and (Copy(Data, 1, Length(STotal)) = STotal);
end;

class function TScFTP_SterCommEntUxRootParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  if IsFooter(Str) then begin
    Result := True;
    Exit;
  end;

  if (Length(Str) >= 26) and
     (Str[1] = 'd') and (Copy(Str, 2, 3) = '   ') and
     (Str[5] <> ' ') and (Copy(Str, 6, 3) = '   ') and
     (Str[9] <> ' ') and (Copy(Str, 10, 3) = '   ') and
     (Str[13] <> ' ') and (Copy(Str, 14, 2) = '  ') and
     (Str[16] <> ' ') and (Copy(Str, 17, 2) = '  ') and
     (Str[19] <> ' ') and (Copy(Str, 20, 2) = '  ') and
     (Str[22] <> ' ') and (Copy(Str, 23, 2) = '  ') and
     (Str[25] <> ' ') and (Str[26] = ' ')
  then
    Result := True;
end;

class function TScFTP_SterCommEntUxRootParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
begin
  Item.FileName := Copy(Item.Data, 27, MaxInt);
  Item.FileType := fftDirectory;
  Result := True;
end;

{ TScFTP_SterComEntParser }

class function TScFTP_SterComEntParser.IsFooter(const Data: string): boolean;
var
  UData: string;
begin
  UData := UpperCase(Data);
  Result := (Pos('TOTAL NUMBER OF ', UData) > 0) and
    (Pos(' BATCH', UData) > 0) and (Pos('LISTED:', UData) > 0);
end;

{ TScFTP_SterCommExpOS390Parser }

class function TScFTP_SterCommExpOS390Parser.Identitifier: string;
begin
  Result := 'Connect:Express for OS/390';
end;

class function TScFTP_SterCommExpOS390Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_SterCommExpOS390ListItem;
end;

class function TScFTP_SterCommExpOS390Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  if IsTotalLine(Str) then
    Exit;

  if Length(Str) >= 3 then
    if CharInSet(Str[2], ['D', 'F']) and (Str[3] = ' ') then begin
      Result := True;
      Exit;
    end;

  if Length(Str) >= 5 then
    if CharInSet(Str[4], ['0', '1', '2']) and (Str[5] = ' ') then begin
      Result := True;
      Exit;
    end;
end;

class function TScFTP_SterCommExpOS390Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);
    if (Lexems.Count > 3) and (Lexems[3] <> '-') then
      TScFTP_SterCommExpOS390ListItem(Item).RecFormat := Lexems[3];

    if Lexems.Count > 4 then
      TScFTP_SterCommExpOS390ListItem(Item).RecLength := StrToInt64Def(Lexems[4], 0);

    if Lexems.Count > 5 then
      Item.BlockSize := StrToInt64Def(Lexems[5], 0);

    if Lexems.Count > 6 then
      Item.FileName := Lexems[6];
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_PCTCPFtpsrvParser }

class function TScFTP_PCTCPFtpsrvParser.Identitifier: string;
begin
  Result := 'PC/TCP ftpsrv.exe';
end;

class function TScFTP_PCTCPFtpsrvParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  Result := (Str = '<dir>') or IsNumeric(Trim(Copy(Str, 1, 10)));
  if Result then
    Result := Trim(Copy(Str, 11, 19)) <> '';

  if Result then
    Result := StrToDay(Trim(Copy(Str, 31, 7))) > 0;

  if Result then
    Result := StrToMonth(Copy(Str, 38, 3)) > 0;
  if Result then
    Result := StrToIntDef(Copy(Str, 42, 2), 0) > 0;
  if Result then
    Result := IsTime(Copy(Str, 45, 8), HHMMSS);
  if Result then
    Result := IsNumeric(Trim(Copy(Str, 54, 4)));
end;

class function TScFTP_PCTCPFtpsrvParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Lexem: string;
  Year, Month, Day: word;
begin
  Result := False;
  TmpData := TrimLeft(Item.Data);
  Lexem := ExtractFirstWord(TmpData, ' ');

  if Lexem = '<dir>' then begin
    Item.FileType := fftDirectory;
    Item.SizeAvailable := False;
  end
  else
  if IsNumeric(Lexem) then begin
    Item.Size := StrToInt64Def(Lexem,0);
    Item.SizeAvailable := True;
  end
  else
    Exit;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if Lexem = '' then
    Exit
  else
    Item.FileName := Lexem;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if StrToDay(Lexem) < 1 then
    Exit;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Month := StrToMonth(Lexem);
  if Month < 1 then
    Exit;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Day := StrToIntDef(Lexem,0);
  if Day = 0 then
    Exit;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if not IsTime(Lexem, HHMMSS) then
    Exit;
  Item.ModifiedDate := StrToTime(Lexem, HHMMSS);

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Year := StrToIntDef(Lexem, $FFFF);
  if Year = $FFFF then
    Exit;
  Year := CheckYear4(Year);
  Item.ModifiedDate := Item.ModifiedDate + EncodeDate(Year, Month, Day);
  Item.ModifiedAvailable := True;

  Result := True;
end;

{ TScFTP_PCNFSDParser }

class function TScFTP_PCNFSDParser.Identitifier: string;
begin
  Result := 'PC-NFSD';
end;

class function TScFTP_PCNFSDParser.CheckLine(const Data: string): boolean;
var
  Lexems: TStrings;
  Lexem: string;
  i: integer;
begin
  Result := False;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Data, ' ', True);

    if Lexems.Count > 3 then begin
      i := Lexems.Count - 1;
      Lexem := Lexems[i];
      if (Lexem <> '') and CharInSet(Lexem[Length(Lexem)], ['a', 'p']) then begin
        Lexem := ExtractFirstWord(Lexem, 'a');
        Lexem := ExtractFirstWord(Lexem, 'p');
        if IsTime(Lexem, HHMMSS) then begin
          Dec(i);
          Lexem := Lexems[i];
          if IsDate(Lexem, MMDDYY) then begin
            Dec(i);
            if IsNumeric(Lexem) or (Lexem = '<dir>') then
              Result := (i = 0) or (i = 1);
          end;
        end;
      end;
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_PCNFSDParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Listing.Count - 1 do begin
    Result := CheckLine(Listing[i]);
    if Result then
      Break;
  end;
end;

class function TScFTP_PCNFSDParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  Lexem: string;
  i: integer;
begin
  Result := False;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);

    if Lexems.Count > 3 then begin
      Item.FileName := Lexems[0];
      if Length(Lexems[1]) < 4 then begin
        Item.FFileName := Item.FFileName + '.' + Lexems[1];
        i := 2;
      end
      else
        i := 1;

      Lexem := Lexems[i];
      Item.Size := DetectFirstNumber(Lexem, -1);
      if (Item.Size <> -1) or (Lexem = '<dir>') then begin
        if Lexem = '<dir>' then begin
          Item.FileType := fftDirectory;
          Item.SizeAvailable := False;
        end;
        Inc(i);

        if IsDate(Lexem, MMDDYY) then begin
          Item.ModifiedDate := StrToDate(Lexem, MMDDYY);

          Inc(i);
          Lexem := Lexems[i];
          if (Lexem <> '') and CharInSet(Lexem[Length(Lexem)], ['a', 'p']) then begin
            Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);
            Result := True;
          end;
        end;
      end;
    end;
  finally
    Lexems.Free;
  end;
end;

{ TScFTP_DOSBaseListItem }

constructor TScFTP_DOSBaseListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FAttributes := TScDOSAttributes.Create;
end;

destructor TScFTP_DOSBaseListItem.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

procedure TScFTP_DOSBaseListItem.SetAttributes(Attributes: TScDOSAttributes);
begin
  FAttributes.Assign(Attributes);
end;

{ TScFTP_OS2Parser }

class function TScFTP_OS2Parser.Identitifier: string;
begin
  Result := 'OS/2';
end;

class function TScFTP_OS2Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_DOSBaseListItem;
end;

class function TScFTP_OS2Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str, Lexem: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := TrimLeft(Listing[0]);
  if not IsNumeric(ExtractFirstWord(Str, ' ')) then
    Exit;

  while True do begin
    Str := TrimLeft(Str);
    Lexem := ExtractFirstWord(Str, ' ');
    if Lexem = 'DIR' then begin
      Str := TrimLeft(Str);
      Lexem := ExtractFirstWord(Str, ' ');
    end;

    if IsDate(Lexem, MMDDYY) then
      Break;

    if not IsValidAttr(Lexem) then
      Exit;
  end;

  if (Length(Str) = 0) or (Copy(Str, 1, 2) <> '  ') then
    Exit;

  if (Length(Str) >= 3) and (Str[3] = ' ') then
    Exit;

  Str := TrimLeft(Str);
  Result := IsTime(ExtractFirstWord(Str, ' '), HHMMSS);
end;

class function TScFTP_OS2Parser.IsValidAttr(const Attr: string): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 1 to Length(Attr) do begin
    Result := CharInSet(Attr[i], ['R', 'A', 'S', 'H']);
    if not Result then
      Break;
  end;
end;

class function TScFTP_OS2Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem, Size: string;
begin
  Result := False;
  TmpData := TrimLeft(Item.Data);
  Size := ExtractFirstWord(TmpData, ' ');
  Item.Size := StrToInt64Def(Size, 0);

  while True do begin
    TmpData := TrimLeft(TmpData);
    Lexem := ExtractFirstWord(TmpData, ' ');
    if (Size = '0') and (Lexem = 'DIR') then begin
      Item.FileType := fftDirectory;
      TmpData := TrimLeft(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
    end;

    if IsDate(Lexem, MMDDYY) then begin
      Item.ModifiedDate := StrToDate(Lexem, MMDDYY);
      Break;
    end;

    TScFTP_DOSBaseListItem(Item).Attributes.AddAttribute(Lexem);
    if TmpData = '' then
      Exit;
  end;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if IsTime(Lexem, HHMMSS) then
    Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

  Delete(TmpData, 1, 1);
  Item.FileName := TmpData;

  Result := True;
end;

{ TScFTP_NetwarePSUDosParser }

class function TScFTP_NetwarePSUDosParser.Identitifier: string;
begin
  Result := 'Novell Netware Print Services for Unix:  DOS Namespace';
end;

class function TScFTP_NetwarePSUDosParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  Perms: string;
begin
  Result := True;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  if (Str <> '') and CharInSet(Str[1], ['d', 'D', '-']) then begin
    Perms := TScFTP_NovellNetwareParser.ExtractNovellPermissions(Copy(Str, 1, 12));
    Result := (Length(Perms) = 8) and
      TScFTP_NovellNetwareParser.IsValidNovellPermissions(Perms) and
      TScFTP_NovellNetwareParser.IsNovellPattern(Str);
  end;
end;

class function TScFTP_NetwarePSUDosParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  ModifiedDate, ModifiedTime: string;
begin
  TmpData := Item.Data;

  if (TmpData <> '') and (UpperCase(TmpData[1]) = 'D') then
    Item.FileType := fftDirectory
  else
    Item.FileType := fftFile;
  Delete(TmpData, 1, 1);
  TmpData := TrimLeft(TmpData);

  Item.Permissions := TScFTP_NovellNetwareParser.ExtractNovellPermissions(ExtractFirstWord(TmpData, ' '));
  Item.PermissionsDisplay := '[' + Item.Permissions + ']';

  TmpData := TrimLeft(TmpData);
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  ModifiedDate := Copy(ExtractFirstWord(TmpData, ' '), 1, 3);
  TmpData := TrimLeft(TmpData);
  ModifiedDate := ModifiedDate + ' ' + ExtractFirstWord(TmpData, ' ');
  ModifiedDate := ExtractFirstWord(ModifiedDate, ',');
  TmpData := TrimLeft(TmpData);
  ModifiedDate := ModifiedDate + ' ' + ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToDate(ModifiedDate, MonthDDYY);

  ModifiedTime := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  ModifiedTime := ModifiedTime + ExtractFirstWord(TmpData, ' ');
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(ModifiedTime, HHMMSS);

  Item.FileName := TmpData;

  Result := True;
end;

{ TScFTP_NetwarePSUNFSParser }

class function TScFTP_NetwarePSUNFSParser.Identitifier: string;
begin
  Result := 'Novell Netware Print Services for Unix:  NFS Namespace';
end;

class function TScFTP_NetwarePSUNFSParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_UnixBaseListItem;
end;

class function TScFTP_NetwarePSUNFSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
  Str, Lexem: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  if (Length(Str) > 9) and (Str[10] = ' ') then
    Delete(Str, 10, 1);

  if IsValidUnixPermissions(Str) then begin
    Lexems := TStringList.Create;
    try
      Split(Lexems, Str, ' ', True);

      if (Lexems.Count > 9) and ((Lexems[9] = 'AM') or (Lexems[9] = 'PM')) then begin
        Lexem := Lexems[6];
        Lexem := ExtractFirstWord(Lexem, ',');
        if IsNumeric(Lexem) and IsNumeric(Lexems[7]) and (Length(Lexems[8]) >= 3) and (Lexems[8][3] = ':') then
          Result := StrToMonth(Lexems[5]) > 0;
      end;
    finally
      Lexems.Free;
    end;
  end;
end;

class function TScFTP_NetwarePSUNFSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
begin
  TmpData := Item.Data;
  Lexem := ExtractFirstWord(TmpData, ' ');
  if not IsNumeric(GetFirstWord(TmpData, ' ')) then
    Lexem := Lexem + ExtractFirstWord(TmpData, ' ');

  if (Lexem <> '') and (Lexem[1] = '-') then
    Item.FileType := fftFile
  else
    Item.FileType := fftDirectory;

  TScFTP_UnixBaseListItem(Item).OwnerPermissions := Copy(Lexem, 2, 3);
  TScFTP_UnixBaseListItem(Item).GroupPermissions := Copy(Lexem, 5, 3);
  TScFTP_UnixBaseListItem(Item).OtherPermissions := Copy(Lexem, 8, 3);
  Item.Permissions := Lexem;
  Item.PermissionsDisplay := Lexem;

  TScFTP_UnixBaseListItem(Item).LinkCount := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  Item.GroupName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Lexem := UpperCase(ExtractFirstWord(TmpData, ' ')) + ' ';
  TmpData := TrimLeft(TmpData);
  Lexem := TrimRight(Lexem) + ' ' + ExtractFirstWord(TmpData, ' ');
  Lexem := ExtractFirstWord(Lexem, ',');
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ') + ' ' + Lexem;
  Item.ModifiedDate := StrToDate(Lexem, YYMonthDD);
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Lexem := Lexem + ExtractFirstWord(TmpData, ' ');
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

  if AnsiPos(' -> ', TmpData) > 0 then begin
    Item.FileName := ExtractFirstWord(TmpData, ' -> ');
    TScFTP_UnixBaseListItem(Item).LinkedItemName := TmpData;
  end
  else
    Item.FileName := TmpData;
  Item.LocalFileName := Item.FileName;

  Result := True;
end;

{ TScFTP_NovellNetwareParser }

class function TScFTP_NovellNetwareParser.Identitifier: string;
begin
  Result := 'Novell Netware';
end;

class function TScFTP_NovellNetwareParser.IsNovellPattern(Value: string): boolean;
var
  sl: TStrings;
begin
  if (Length(Value) > 1) and (Value[2] = '[') then
    Insert(' ', Value, 2);

  sl := TStringList.Create;
  try
    Split(sl, Value, ' ', True);
     Result := (sl.Count > 8) and IsNumeric(sl[6]) and IsTime(sl[7], HHMMSS) and
       (AnsiSameText(sl[8], 'AM') or AnsiSameText(sl[8], 'PM'));
  finally
    sl.Free;
  end;
end;

class function TScFTP_NovellNetwareParser.IsValidNovellPermissions(const Value: string): boolean;
var
  i: integer;
begin
  Result := False;
  if Value = '' then
    Exit;

  for i := 1 to Length(Value) do begin
    if not CharInSet(Value[i], ['-', 'R', 'W', 'C', 'E', 'A', 'F', 'M', 'S']) then
      Exit;
  end;

  Result := True;
end;

class function TScFTP_NovellNetwareParser.ExtractNovellPermissions(const Value: string): string;
var
  OpenPos, ClosePos: integer;
begin
  Result := '';
  OpenPos := Pos('[', Value);
  ClosePos := Pos(']', Value);
  if (OpenPos <> 0) and (ClosePos <> 0) and (OpenPos < ClosePos) then
    Result := Trim(Copy(Value, OpenPos + 1, ClosePos - OpenPos - 1));
end;

class function TScFTP_NovellNetwareParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  Perms: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  if IsTotalLine(Listing[0]) then
    if Listing.Count > 1 then
      Str := Listing[1]
    else
      Exit
  else
    Str := Listing[0];

  if Str <> '' then
    if CharInSet(Str[1], ['d', 'D', '-', ' ']) then begin
      Perms := ExtractNovellPermissions(Str);
      Result := (Length(Perms) = 8) and IsValidNovellPermissions(Perms) and not IsNovellPattern(Str);
    end;
end;

class function TScFTP_NovellNetwareParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  TmpData, Lexem: string;
  Year, CurMonth, Month, Day, Hour, Min, Sec, MSec: word;
  i, NameIdx: integer;
  FileName: string;
begin
  DecodeDate(Now, Year, Month, Day);
  DecodeTime(Now, Hour, Min, Sec, MSec);
  CurMonth := Month;

  if (Item.Data <> '') and (UpperCase(Item.Data[1]) = 'D') then
    Item.FileType := fftDirectory
  else
    Item.FileType := fftFile;

  TmpData := Item.Data;
  Item.Permissions := ExtractNovellPermissions(TmpData);
  Item.PermissionsDisplay := '[' + Item.Permissions + ']';

  ExtractFirstWord(TmpData, '] ');
  if TmpData <> '' then begin
    if TmpData[1] = ' ' then begin
      Delete(TmpData, 1, 1);
      ExtractFirstWord(TmpData, ' ');
    end;

    Lexems := TStringList.Create;
    try
      Split(Lexems, TmpData, ' ', True);

      if Lexems.Count > 4 then begin
        Item.OwnerName := Lexems[0];
        Item.Size := StrToInt64Def(Lexems[1], 0);
        Month := StrToMonth(Lexems[2]);
        if Month < 1 then
          Month := CurMonth;

        NameIdx := 5;
        Day := StrToIntDef(Lexems[3], Day);
        if Pos(':', Lexems[4]) = 0 then begin
          Year := StrToIntDef(Lexems[4], Year);
          Year := CheckYear4(Year);
          Hour := 0;
          Min := 0;
          if (Lexems.Count > 5) and (Pos(':', Lexems[5]) > 0) then begin
            Lexem := Lexems[5];
            Hour := StrToIntDef(ExtractFirstWord(Lexem, ':'), Hour);
            Min := StrToIntDef(ExtractFirstWord(Lexem, ':'), Min);
            NameIdx := 6;
          end;
        end
        else begin
          Year := DetectYear(Day, Month);
          Lexem := Lexems[4];
          Hour := StrToIntDef(ExtractFirstWord(Lexem, ':'), Hour);
          Min := StrToIntDef(ExtractFirstWord(Lexem, ':'), Min);
        end;

        Item.ModifiedDate := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, 0, 0);
        FileName := '';
        for i := NameIdx to Lexems.Count - 1 do
          FileName := FileName + ' ' + Lexems[i];
        Delete(FileName, 1, 1);
        Item.FileName := FileName;
        Item.LocalFileName := FileName;
      end;
    finally
      Lexems.Free;
    end;
  end;

  Result := True;
end;

class function TScFTP_NovellNetwareParser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  NewItem: TScFTPListItem;
  i: integer;
begin
  Result := True;
  if Listing.Count = 0 then
    Exit;

  for i := 0 to Listing.Count - 1 do begin
    if (i = 0) and IsTotalLine(Listing[0]) then
      Continue;

    NewItem := CreateItem(Dir);
    NewItem.Data := Listing[i];
    ParseLine(NewItem);
  end;
end;

{ TScFTP_NCSAforMACOSParser }

class function TScFTP_NCSAforMACOSParser.Identitifier: string;
begin
  Result := 'NCSA for MACOS';
end;

class function TScFTP_NCSAforMACOSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
const
  SMACConnect = ' MAC-OS TCP/Connect';
begin
  Result := (SysDescript = 'MAC NCSA') or
    ((SysDescript <> '') and (Copy(SysDescript, 1, Length(SMACConnect)) = SMACConnect));
end;

class function TScFTP_NCSAforMACOSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
begin
  Result := True;
  try
    if CharInSet(Item.Data[Length(Item.Data)], ['/', '\']) then begin
      Item.FileType := fftDirectory;
      Item.FileName := Copy(Item.Data, 1, Length(Item.Data) - 1);
    end
    else begin
      Item.FileType := fftFile;
      Item.FileName := Item.Data;
    end;

  except
    Result := False;
  end;
end;

{ TScFTP_NCSAforDOSParser }

class function TScFTP_NCSAforDOSParser.Identitifier: string;
begin
  Result := 'NCSA for MS-DOS (CU/TCP)';
end;

class function TScFTP_NCSAforDOSParser.IsHeader(const Data: string): boolean;
begin
  Result := False;
end;

class function TScFTP_NCSAforDOSParser.IsFooter(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := False;

  Cols := TStringList.Create;
  try
    Split(Cols, StringReplace(Data, '-', ' ', [rfReplaceAll]), ' ', True);

    while Cols.Count > 2 do
      Cols.Delete(0);

    if Cols.Count = 2 then
      Result := (Cols[0] = 'Bytes') and (Cols[1] = 'Available');
  finally
    Cols.Free;
  end;
end;

class function TScFTP_NCSAforDOSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
  Str: string;
  i: integer;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  for i := 0 to Listing.Count - 2 do begin
    Lexems := TStringList.Create;
    try
      Str := Listing[i];
      Split(Lexems, Str, ' ', True);

      if Lexems.Count = 4 then
        Result := ((Lexems[1] = '<DIR>') or IsNumeric(Lexems[1])) and
          IsDate(Lexems[2], MMDDYY) and IsTime(Lexems[3], HHMMSS) and
          (not IsDate(Copy(Str, 36, 10), MMDDYY) or
          (Copy(Str, 46, 1) <> ' ') or not IsTime(Copy(Str, 47, 5), HHMMSS));
    finally
      Lexems.Free;
    end;

    if not Result then
      Break;
  end;

  Result := IsFooter(Listing[Listing.Count - 1]);
end;

class function TScFTP_NCSAforDOSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
begin
  TmpData := Item.Data;
  Item.FileName := ExtractFirstWord(TmpData, ' ');
  TmpData := Trim(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');

  if Lexem = '<DIR>' then begin
    Item.FileType := fftDirectory;
    Item.SizeAvailable := False;
  end
  else begin
    Item.FileType := fftFile;
    Item.Size := StrToInt64Def(Lexem, 0);
  end;

  if TmpData <> '' then begin
    TmpData := Trim(TmpData);
    Lexem := ExtractFirstWord(TmpData, ' ');
    if Lexem <> '' then begin
      Item.ModifiedDate := StrToDate(Lexem, MMDDYY);
      TmpData := Trim(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if Lexem <> '' then
        Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);
    end;
  end;

  Result := True;
end;

{ TScFTP_MVSParser }

class function TScFTP_MVSParser.Identitifier: string;
begin
  Result := 'MVS';
end;

class function TScFTP_MVSParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_MVSListItem;
end;

class function TScFTP_MVSParser.IsHeader(const Data: string): boolean;
var
  VolPos, UnitPos, RefPos, ExtPos, UsedPos,
  LreclPos, BlkSzPos, DsorgPos, DsnamePos: integer;
begin
  VolPos := Pos('Volume', Data);
  UnitPos := Pos('Unit', Data);
  RefPos := Pos('Referred', Data);
  if RefPos = 0 then
    RefPos := Pos('Date', Data);
  ExtPos := Pos('Ext', Data);
  UsedPos := Pos('Used', Data);
  LreclPos := Pos('Lrecl', Data);
  BlkSzPos := Pos('BlkSz', Data);
  DsorgPos := Pos('Dsorg', Data);
  DsnamePos := Pos('Dsname', Data);

  Result := (VolPos > 0) and (UnitPos > VolPos) and
    (RefPos > UnitPos) and (ExtPos > RefPos) and
    (UsedPos > ExtPos) and (LreclPos > UsedPos) and
    (BlkSzPos > LreclPos) and (DsorgPos > BlkSzPos) and
    (DsnamePos > DsorgPos);
end;

class function TScFTP_MVSParser.MVSStrToDate(const Value: string): TDateTime;
var
  TmpData: string;
  Year, Month, Day: integer;
  CurYear, CurMonth, CurDay: word;
begin
  DecodeDate(Now, CurYear, CurMonth, CurDay);
  TmpData := Value;

  if AnsiPos('/', TmpData) = 3 then begin
    Year := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurYear);
    if (Year < 13) and (Year > 0) then begin
      Month := Year;
      Day := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurDay);
      Year := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurYear);
    end
    else begin
      Month := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurMonth);
      Day := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurDay);
    end;
  end
  else begin
    Year := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurYear);
    Month := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurMonth);
    Day := StrToIntDef(ExtractFirstWord(TmpData, '/'), CurDay);
  end;

  Year := CheckYear4(Year);
  Result := EncodeDate(Year, Month, Day);
end;

class function TScFTP_MVSParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  Lexem: string;
  IsPseudoDir: boolean;
  i, p: integer;
begin
  Result := True;

  Item.ModifiedAvailable := False;
  Item.SizeAvailable := False;

  if Item.Data = '' then
    Exit;

  TScFTP_MVSListItem(Item).Migrated := (Copy(Item.Data, 1, 8) = 'Migrated') or (UpperCase(Copy(Item.Data, 1, 6)) = 'MIGRAT');
  IsPseudoDir := Copy(Item.Data, 1, 16) = 'Pseudo Directory';
  if IsPseudoDir then
    Item.FileType := fftDirectory;

  if not TScFTP_MVSListItem(Item).Migrated and not IsPseudoDir and (Copy(Item.Data, 1, 5) <> 'Error') then begin
    Lexems := TStringList.Create;
    try
      Split(Lexems, Item.Data, ' ', True);

      if Lexems.Count > 0 then
        TScFTP_MVSListItem(Item).Volume := Lexems[0];
      if Lexems.Count > 1 then
        TScFTP_MVSListItem(Item).Units := Lexems[1];

      if Lexems.Count > 2 then begin
        Lexem := Lexems[2];
        if (Lexem <> '') and CharInSet(Lexem[1], ['0'..'9']) then begin
          if Length(Lexem) > 10 then begin
            Lexems.Insert(3, Copy(Lexem, 11, MaxInt));
            Lexems[2] := Copy(Lexem, 1, 10);
          end;
          Item.ModifiedDate := MVSStrToDate(Lexems[2]);
          Item.ModifiedAvailable := True;
        end;
      end;

      if Lexems.Count > 3 then
        TScFTP_MVSListItem(Item).NumberExtents := StrToIntDef(Lexems[3], 0);

      if Lexems.Count > 4 then
        TScFTP_MVSListItem(Item).NumberTracks := StrToIntDef(Lexems[4], 0);

      if Lexems.Count > 5 then
        TScFTP_MVSListItem(Item).RecFormat := Lexems[5];

      if Lexems.Count > 6 then
        TScFTP_MVSListItem(Item).RecLength := StrToIntDef(Lexems[6], 0);

      if Lexems.Count > 7 then
        Item.BlockSize := StrToIntDef(Lexems[7], 0);

      if Lexems.Count > 8 then begin
        TScFTP_MVSListItem(Item).Organization := Lexems[8];

        if (TScFTP_MVSListItem(Item).Organization = 'PO') or (TScFTP_MVSListItem(Item).Organization = 'PO-E') then
          Item.FileType := fftDirectory
        else
          Item.FileType := fftFile;
      end;
    finally
      Lexems.Free;
    end;
  end;

  if Item.Data[Length(Item.Data)] = '''' then begin
    i := Pos('''', Item.Data) + 1;
    Item.FileName := Copy(Item.Data, i, Length(Item.Data) - i - 1);
  end
  else begin
    p := 0;
    for i := Length(Item.Data) downto 1 do begin
      if Item.Data[i] = ' ' then begin
        p := i;
        Break;
      end;
    end;

    Item.FileName := Copy(Item.Data, p + 1, MaxInt);
  end;
end;

{ TScFTP_MVSPartitionedDataSetParser }

class function TScFTP_MVSPartitionedDataSetParser.Identitifier: string;
begin
  Result := 'MVS:  Partitioned Data Set';
end;

class function TScFTP_MVSPartitionedDataSetParser.IsHeader(const Data: string): boolean;
var
  NamePos, SizePos: integer;
begin
  NamePos := Pos('Name', Data);
  SizePos := Pos('Size', Data);
  Result := (NamePos > 0) and (SizePos > NamePos);
end;

class function TScFTP_MVSPartitionedDataSetParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  Lexem: string;
begin
  Item.ModifiedAvailable := False;
  Item.SizeAvailable := False;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);
    if Lexems.Count > 0 then begin
      Item.FileName := Lexems[0];

      if Lexems.Count > 4 then begin
        Lexem := Lexems[3];
        if (Lexem <> '') and CharInSet(Lexem[1], ['0'..'9']) and (Pos('/', Lexem) > 0) then begin
          Item.ModifiedDate := TScFTP_MVSParser.MVSStrToDate(Lexem);
          Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexems[4], HHMMSS);
          Item.ModifiedAvailable := True;
        end;
      end;
    end;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_MVSJESInterface1Parser }

const
  SNo_Jobs_Found = 'No jobs found on JES queue';

class function TScFTP_MVSJESInterface1Parser.Identitifier: string;
begin
  Result := 'MVS:  JES Queue Interface 1';
end;

class function TScFTP_MVSJESInterface1Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_MVSJESListItem;
end;

class function TScFTP_MVSJESInterface1Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
  Lexem: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);
    Result := Lexems.Count > 2;

    if Result then begin
      Lexem := Trim(Lexems[2]);
      Result := (Lexem = 'INPUT') or (Lexem = 'HELD') or (Lexem = 'ACTIVE') or (Lexem = 'OUTPUT');
    end;

    if Result and (Lexems.Count > 3) then
      Result := IsNumeric(Lexems[3]) or ((Length(Lexems[3]) > 0) and (Lexems[3][1] = '-'));

    if not Result then
      Result := (Listing[0] = SNo_Jobs_Found);
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_MVSJESInterface1Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, FirstWord: string;
begin
  Item.ModifiedAvailable := False;
  Item.SizeAvailable := False;

  TmpData := Item.Data;
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Item.FileName := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);

  FirstWord := ExtractFirstWord(TmpData, ' ');
  if FirstWord = 'INPUT' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESReceived
  else
  if FirstWord = 'HELD' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESHold
  else
  if FirstWord = 'ACTIVE' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESRunning
  else
  if FirstWord = 'OUTPUT' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESOuptutAvailable;

  TmpData := TrimLeft(TmpData);
  TScFTP_MVSJESListItem(Item).JobSpoolFiles := StrToIntDef(ExtractFirstWord(TmpData, ' '), 0);

  Result := True;
end;

class function TScFTP_MVSJESInterface1Parser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  NewItem: TScFTPListItem;
  i: integer;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  if Listing[0] <> SNo_Jobs_Found then begin
    for i := 0 to Listing.Count - 1 do begin
      NewItem := CreateItem(Dir);
      NewItem.Data := Listing[i];
      ParseLine(NewItem);
    end;
  end
  else
    Result := True;
end;

{ TScFTP_MVSJESInterface2Parser }

const
  SMVS_Header = 'JOBNAME  JOBID    OWNER    STATUS CLASS';

class function TScFTP_MVSJESInterface2Parser.Identitifier: string;
begin
  Result := 'MVS:  JES Queue Interface 2';
end;

class function TScFTP_MVSJESInterface2Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_MVSJESListItem;
end;

class function TScFTP_MVSJESInterface2Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
begin
  Result := False;
  if Listing.Count > 0 then
    Result := (Listing[0] = SMVS_Header);
end;

class function TScFTP_MVSJESInterface2Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, No: string;
  SubStr: string;
  SpoolPos, TmpPos: integer;
begin
  Item.ModifiedAvailable := False;
  Item.SizeAvailable := False;
  Item.FileName := Trim(Copy(Item.Data, 10, 8));
  Item.OwnerName := Trim(Copy(Item.Data, 19, 7));
  if IsSpaceLine(Item.OwnerName) then
    Item.OwnerName := '';

  SubStr := Trim(Copy(Item.Data, 28, 7));
  if SubStr = 'INPUT' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESReceived
  else
  if SubStr = 'HELD' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESHold
  else
  if SubStr = 'ACTIVE' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESRunning
  else
  if SubStr = 'OUTPUT' then
    TScFTP_MVSJESListItem(Item).JobStatus := jsJESOuptutAvailable;

  TmpData := Trim(Copy(Item.Data, 35, MaxInt));
  SpoolPos := Pos(' spool', TmpData);
  if SpoolPos = 0 then begin
    Result := False;
    Exit;
  end;

  No := '';
  for TmpPos := SpoolPos - 1 downto 1 do begin
    if TmpData[TmpPos] = ' ' then
      Break;
    No := TmpData[TmpPos] + No;
  end;

  TScFTP_MVSJESListItem(Item).JobSpoolFiles := StrToIntDef(No, 0);
  Result := True;
end;

class function TScFTP_MVSJESInterface2Parser.Parse(Listing: TStrings; Dir: TScFTPDirectoryListing): boolean;
var
  NewItem: TScFTPListItem;
  Str: string;
  IsDetail: boolean;
  i: integer;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  IsDetail := False;
  for i := 0 to Listing.Count - 1 do begin
    if (i = 0) and (Listing[0] = SMVS_Header) then
      Continue;

    Str := Listing[i];

    if IsDetail then begin
      if Dir.Count > 0 then
        TScFTP_MVSJESListItem(Dir.Items[Dir.Count - 1]).Details.Add(Str);
    end
    else
    if Str = '--------' then
      IsDetail := True
    else begin
      NewItem := CreateItem(Dir);
      NewItem.Data := Str;
      ParseLine(NewItem);
    end;
  end;

  Result := True;
end;

{ TScFTP_MVSListItem }

constructor TScFTP_MVSListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FSizeAvailable := False;
end;

{ TScFTP_MVSJESListItem }

constructor TScFTP_MVSJESListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FJobStatus := jsJESNotApplicable;
  FDetails := TStringList.Create;
end;

destructor TScFTP_MVSJESListItem.Destroy;
begin
  FDetails.Free;
  inherited;
end;

{ TScFTP_MusicParser }

class function TScFTP_MusicParser.Identitifier: string;
begin
  Result := 'MUSIC/SP';
end;

class function TScFTP_MusicParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_RecListItem;
end;

class function TScFTP_MusicParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    Result := (Cols.Count > 7) and
      ((Cols[0] = 'File') and
      (Cols[1] = 'name') and
      (Cols[2] = 'RlenRf') and
      (Cols[3] = 'Size') and
      (Cols[4] = 'Read') and
      (Cols[5] = 'Write') and
      (Cols[6] = 'By') and
      ((Cols[7] = 'Attrbs') and
      (Cols.Count > 8) and
      (Cols[8] = '#Recs') or
      (Cols[7] = 'Attrbs#Recs')));
  finally
    Cols.Free;
  end;
end;

class function TScFTP_MusicParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem, RecFormat: string;
  Day, Month, Year: integer;
  i, p: integer;
begin
  TmpData := Item.Data;

  Item.FileName := ExtractFirstWord(TmpData, ' ');
  if (Item.FileName <> '') and (Item.FileName[Length(Item.FileName)] = '\') then begin
    Item.FileType := fftDirectory;
    Item.FileName := Copy(Item.FileName, 1, Length(Item.FileName) - 1);
  end;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  TScFTP_RecListItem(Item).RecLength := DetectFirstNumber(Lexem, 0);

  // Skip number
  p := 1;
  for i := 1 to Length(Lexem) do begin
    p := i;
    if not CharInSet(Lexem[i], ['0'..'9', ',']) then
      Break;
  end;

  RecFormat := Copy(Lexem, p, Length(Lexem));
  if (RecFormat <> '') and (RecFormat[1] = '<') then
    Delete(RecFormat, 1, 1);
  if (RecFormat <> '') and (RecFormat[Length(RecFormat)] = '>') then
    RecFormat := ExtractFirstWord(RecFormat, '>');

  TScFTP_RecListItem(Item).RecFormat := RecFormat;
  if TScFTP_RecListItem(Item).RecFormat = 'DIR' then
    Item.FileType := fftDirectory;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Item.Size := DetectFirstNumber(Lexem, 0) * 1024;

  TmpData := TrimLeft(TmpData);
  ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Day := StrToIntDef(Copy(Lexem, 1, 2), 1);
  Month := StrToMonth(Copy(Lexem, 3, 3));
  Year := CheckYear4(StrToIntDef(Copy(Lexem, 6, MaxInt), 0));
  Item.ModifiedDate := EncodeDate(Year, Month, Day);
  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(ExtractFirstWord(TmpData, ' '), HHMMSS);

  TmpData := TrimLeft(TmpData);
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  if Pos(' ', TmpData) = 0 then
    TScFTP_RecListItem(Item).NumberRecs := StrToIntDef(TmpData, 0);

  Result := True;
end;

{ TScFTP_MPEiXBaseParser }

class function TScFTP_MPEiXBaseParser.Identitifier: string;
begin
  Result := 'MPE/iX:  ';
end;

class function TScFTP_MPEiXBaseParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_MPEiXListItem;
end;

class function TScFTP_MPEiXBaseParser.IsExtHeader(Cols: TStrings): boolean;
begin
  Result := (Cols.Count > 3) and
    (Cols[0] = 'SIZE') and
    (Cols[1] = 'TYP') and
    (Cols[2] = 'EOF') and
    (Cols[3] = 'LIMIT');

  if Result and (Cols.Count = 8) then
    Result := (Cols[4] = 'R/B') and
      (Cols[5] = 'SECTORS') and
      (Cols[6] = '#X') and
      (Cols[7] = 'MX');

  if not Result and (Cols.Count = 3) then
    Result := (Pos('@', Cols[0]) > 0) and
      (Cols[1] = 'not') and
      (Cols[2] = 'found');
end;

{ TScFTP_MPEiXParser }

class function TScFTP_MPEiXParser.Identitifier: string;
begin
  Result := inherited Identitifier + 'LISTF';
end;

class function TScFTP_MPEiXParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
  AccountPos, GroupPos: integer;
begin
  AccountPos := Pos('ACCOUNT= ', Data);
  if AccountPos = 0 then
    AccountPos := Pos('ACCOUNT = ', Data);
  GroupPos := Pos('GROUP= ', Data);
  if GroupPos = 0 then
    GroupPos := Pos('GROUP = ', Data);

  Result := (AccountPos > 0) and (GroupPos > AccountPos);
  if Result then
    Exit;

  Cols := TStringList.Create;
  try
    Split(Cols, StringReplace(Data, '-', ' ', [rfReplaceAll]), ' ', True);
    Result := (Cols.Count > 3) and
      (Cols[0] = 'FILENAME') and
      (Cols[1] = 'CODE') and
      (Cols[2] = 'LOGICAL') and
      (Cols[3] = 'RECORD');

    if Result and (Cols.Count = 5) then
      Result := Cols[4] = 'SPACE';
    if not Result then
      Result := IsExtHeader(Cols);
  finally
    Cols.Free;
  end;
end;

class function TScFTP_MPEiXParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  TmpData: string;
begin
  Lexems := TStringList.Create;
  try
    Item.FileName := Trim(Copy(Item.Data, 1, 8));
    TmpData := Copy(Item.Data, 8, MaxInt);
    if (TmpData <> '') and (TmpData[1] <> ' ') then
      ExtractFirstWord(TmpData, ' ');

    Split(Lexems, TmpData, ' ', True);
    if Lexems.Count > 1 then
      Item.Size := DetectFirstNumber(Lexems[1], 0);
    if Lexems.Count > 2 then
      TScFTP_MPEiXListItem(Item).RecFormat := Lexems[2];
    if Lexems.Count > 3 then
      TScFTP_MPEiXListItem(Item).NumberRecs := StrToInt64Def(Lexems[3], 0);
    if Lexems.Count > 4 then
      TScFTP_MPEiXListItem(Item).Limit := StrToInt64Def(Lexems[4], 0);

    Item.FileType := fftFile;
    Item.ModifiedAvailable := False;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_MPEiXWithPOSIXParser }

class function TScFTP_MPEiXWithPOSIXParser.Identitifier: string;
begin
  Result := inherited Identitifier + 'With POSIX';
end;

class function TScFTP_MPEiXWithPOSIXParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Result := Pos('PATH= ', Data) > 0;
  if Result then
    Exit;

  Cols := TStringList.Create;
  try
    Split(Cols, StringReplace(Data, '-', ' ', [rfReplaceAll]), ' ', True);
    Result := (Cols.Count = 5) and
      (Cols[0] = 'CODE') and
      (Cols[1] = 'LOGICAL') and
      (Cols[2] = 'RECORD') and
      (Cols[3] = 'SPACE') and
      (Cols[4] = 'FILENAME');

    if not Result then
      Result := IsExtHeader(Cols);
  finally
    Cols.Free;
  end;
end;

class function TScFTP_MPEiXWithPOSIXParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);
    if Lexems.Count > 0 then
      Item.Size := DetectFirstNumber(Lexems[0], 0);
    if Lexems.Count > 1 then
      TScFTP_MPEiXListItem(Item).RecFormat := Lexems[1];
    if Lexems.Count > 2 then
      TScFTP_MPEiXListItem(Item).NumberRecs := StrToInt64Def(Lexems[2], 0);
    if Lexems.Count > 3 then
      TScFTP_MPEiXListItem(Item).Limit := StrToInt64Def(Lexems[3], 0);
    if Lexems.Count > 8 then
      Item.FileName := Lexems[8];

    if (Item.FileName <> '') and (Item.FileName[Length(Item.FileName)] = '/') then begin
      Item.FileType := fftDirectory;
      Item.FileName := Copy(Item.FileName, 1, Length(Item.FileName) - 1);
    end
    else
      Item.FileType := fftFile;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_MPEiXListItem }

constructor TScFTP_MPEiXListItem.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FModifiedAvailable := False;
end;

{ TScFTP_MicrowareOS9Parser }

class function TScFTP_MicrowareOS9Parser.Identitifier: string;
begin
  Result := 'MicroWare OS-9';
end;

class function TScFTP_MicrowareOS9Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_MicrowareOS9ListItem;
end;

class function TScFTP_MicrowareOS9Parser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Cols := TStringList.Create;
  try
    Result := False;
    Split(Cols, Data, ' ', True);

    if Cols.Count > 2 then begin
      Result := (Cols[0] = 'Directory') and (Cols[1] = 'of') and
        (PatternsCount(':', Cols[Cols.Count - 1]) = 2);

      if not Result then
        Result := (Cols.Count = 7) and
          (Cols[0] = 'Owner') and
          (Cols[1] = 'Last') and
          (Cols[2] = 'modified') and
          (Cols[3] = 'Attributes') and
          (Cols[4] = 'Sector') and
          (Cols[5] = 'Bytecount') and
          (Cols[6] = 'Name');
    end;
  finally
    Cols.Free;
  end;
end;

class function TScFTP_MicrowareOS9Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
  Perms: string;
begin
  TmpData := TrimLeft(Item.Data);

  Item.OwnerName := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToDate(ExtractFirstWord(TmpData, ' '), YYMMDD);

  TmpData := TrimLeft(TmpData);
  ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Perms := ExtractFirstWord(TmpData, ' ');
  if (Perms <> '') and (LowerCase(Perms[1]) = 'd') then
    Item.FileType := fftDirectory
  else
    Item.FileType := fftFile;

  Item.Permissions := Perms;
  Item.PermissionsDisplay := Perms;
  TScFTP_MicrowareOS9ListItem(Item).MiscPermissions := Copy(Perms, 1, 2);
  TScFTP_MicrowareOS9ListItem(Item).PublicPermissions := Copy(Perms, 3, 3);
  TScFTP_MicrowareOS9ListItem(Item).OwnerPermissions := Copy(Perms, 5, 3);

  TmpData := TrimLeft(TmpData);
  TScFTP_MicrowareOS9ListItem(Item).Sector := StrToInt64Def('$' + ExtractFirstWord(TmpData, ' '), 0);
  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);
  Item.FileName := TmpData;

  Result := True;
end;

{ TScFTP_IEFTPGatewayLSLongParser }

class function TScFTP_IEFTPGatewayLSLongParser.Identitifier: string;
begin
   Result := 'IE-FTPListStyleLong';
end;

class function TScFTP_IEFTPGatewayLSLongParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_IEFTPGatewayLsLongListItem;
end;

class function TScFTP_IEFTPGatewayLSLongParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count >= 6 then
      Result := (Cols[0] = 'Filename') and (Cols[1] = '(MSGKEY)') and
        (Cols[2] = 'Sender') and (Cols[3] = 'Class') and
        (Cols[4] = 'Size') and (Cols[5] = 'Date') and
        (Cols[6] = 'Time')
    else
      Result := False;
  finally
    Cols.Free;
  end;
end;

class function TScFTP_IEFTPGatewayLSLongParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  Lexem: string;
  Day, Month, Year: integer;
  Hour, Min, Sec: integer;
begin
  Item.FileType := fftFile;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Item.Data, ' ', True);
    Item.FileName := Lexems[0];
    TScFTP_IEFTPGatewayLsLongListItem(Item).SenderAccount := Lexems[1];
    TScFTP_IEFTPGatewayLsLongListItem(Item).SenderUserID := Lexems[2];
    TScFTP_IEFTPGatewayLsLongListItem(Item).SenderUserClass := Lexems[3];
    Item.Size := StrToInt64Def(Lexems[4], 0);
    Item.SizeAvailable := True;

    Lexem := Lexems[5];
    Year := CheckYear4(StrToInt(Copy(Lexem, 1, 2)));
    Month := StrToInt(Copy(Lexem, 3, 2));
    Day := StrToInt(Copy(Lexem, 5, 2));
    Item.ModifiedDate := EncodeDate(Year, Month, Day);

    Lexem := Lexems[6];
    Hour := StrToInt(Copy(Lexem, 1, 2));
    Min := StrToInt(Copy(Lexem, 3, 2));
    Sec := StrToInt(Copy(Lexem, 5, 2));
    Item.ModifiedDate := Item.ModifiedDate + EncodeTime(Hour, Min, Sec, 0);
    Item.ModifiedAvailable := True;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_IEFTPGatewayLSFileNameParser }

class function TScFTP_IEFTPGatewayLSFileNameParser.Identitifier: string;
begin
  Result := 'IE-FTPListStyleFileName';
end;

class function TScFTP_IEFTPGatewayLSFileNameParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_IEFTPGatewayLsFileNameListItem;
end;

class function TScFTP_IEFTPGatewayLSFileNameParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  i: integer;
begin
  Result := False;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];
    Result := IsIEFile(ExtractFirstWord(Str, ' '));
    if Result then begin
      Str := TrimLeft(Str);
      Result := Str <> '';
    end;

    if not Result then
      Break;
  end;
end;

class function TScFTP_IEFTPGatewayLSFileNameParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
begin
  Item.FileType := fftFile;
  TmpData := Item.Data;
  Item.FileName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  TScFTP_IEFTPGatewayLsFileNameListItem(Item).OriginalFileName := UnquoteStr(ExtractFirstWord(TmpData, ' '));

  Result := True;
end;

{ TScFTP_IEFTPGatewayLSShortParser }

class function TScFTP_IEFTPGatewayLSShortParser.Identitifier: string;
begin
  Result := 'IE-FTPListStyleShort';
end;

class function TScFTP_IEFTPGatewayLSShortParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to Listing.Count - 1 do begin
    Result := IsIEFile(Listing[i]);
    if not Result then
      break;
  end;
end;

{ TScFTP_IEFTPGatewayLSLibraryParser }

class function TScFTP_IEFTPGatewayLSLibraryParser.Identitifier: string;
begin
  Result := 'IE-FTPListStyleLibrary';
end;

class function TScFTP_IEFTPGatewayLSLibraryParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_IEFTPGatewayLSLibraryListItem;
end;

class function TScFTP_IEFTPGatewayLSLibraryParser.IsHeader(const Data: string): boolean;
var
  Cols: TStrings;
begin
  Cols := TStringList.Create;
  try
    Split(Cols, Data, ' ', True);

    if Cols.Count >= 6 then
      Result := (Cols[0] = 'Access') and (Cols[1] = 'Owner') and
        (Cols[2] = 'Account') and (Cols[3] = 'Size') and
        (Cols[4] = 'Last') and (Cols[5] = 'updated') and
        (Cols[6] = 'Name')
    else
      Result := False;
  finally
    Cols.Free;
  end;
end;

class function TScFTP_IEFTPGatewayLSLibraryParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData: string;
begin
  TmpData := Item.Data;

  Item.FileType := fftFile;
  TScFTP_IEFTPGatewayLSLibraryListItem(Item).OwnerPermissions := Copy(TmpData, 2, 3);
  TScFTP_IEFTPGatewayLSLibraryListItem(Item).GroupPermissions := Copy(TmpData, 5, 3);
  TScFTP_IEFTPGatewayLSLibraryListItem(Item).OtherPermissions := Copy(TmpData, 8, 3);

  Delete(TmpData, 1, 10);
  Item.OwnerName := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  TScFTP_IEFTPGatewayLSLibraryListItem(Item).Account := ExtractFirstWord(TmpData, ' ');

  TmpData := TrimLeft(TmpData);
  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);

  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToDate(ExtractFirstWord(TmpData, ' '), YYMMDD);
  TmpData := TrimLeft(TmpData);
  Item.ModifiedDate := StrToTime(ExtractFirstWord(TmpData, ' '), HHMMSS);

  Delete(TmpData, 1, 1);
  Item.FileName := TmpData;

  Result := True;
end;

{ TScFTP_KA9QParser }

class function TScFTP_KA9QParser.Identitifier: string;
begin
  Result := 'KA9Q';
end;

class function TScFTP_KA9QParser.IsFooter(const Data: string): boolean;
var
  Cols: TStrings;
  Col1: string;
begin
  if Data = '#' then begin
    Result := True;
    Exit;
  end;

  Result := False;
  Cols := TStringList.Create;
  try
    Split(Cols, StringReplace(Data, '-', ' ', [rfReplaceAll]), ' ', True);
    if Cols.Count > 1 then begin
      Col1 := Cols[1];
      Result := (Col1 = 'files.') or (Col1 = 'file.') or (Col1 = 'files') or (Col1 = 'file');
    end;
  finally
    Cols.Free;
  end;
end;

class function TScFTP_KA9QParser.IsHeader(const Data: string): boolean;
begin
  Result := False;
end;

class function TScFTP_KA9QParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if Lexems.Count > 2 then begin
      if (Lexems[0] <> '') and (Lexems[0][Length(Lexems[0])] = '/') then
        Result := (PatternsCount(':', Lexems[1]) = 1) and IsTime(Lexems[1], HHMMSS) and
          (PatternsCount('/', Lexems[2]) = 2) and IsDate(Lexems[2], MMDDYY)
      else
        Result := (Lexems.Count > 3) and
          (DetectFirstNumber(Lexems[1], -1) > -1) and
          (PatternsCount(':', Lexems[2]) = 1) and IsTime(Lexems[2], HHMMSS) and
          (PatternsCount('/', Lexems[3]) = 2) and IsDate(Lexems[3], MMDDYY);
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_KA9QParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
  NewItem: TScFTPListItem;
begin
  TmpData := Item.Data;

  Lexem := ExtractFirstWord(TmpData, ' ');
  if Lexem <> '' then begin
    if Lexem[Length(Lexem)] = '/' then begin
      Item.FileName := ExtractFirstWord(Lexem, '/');
      Item.FileType := fftDirectory;
      Item.SizeAvailable := False;
    end
    else begin
      Item.FileName := Lexem;
      Item.FileType := fftFile;

      TmpData := Trim(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      Item.Size := DetectFirstNumber(Lexem, 0);
    end;

    TmpData := Trim(TmpData);
    Lexem := ExtractFirstWord(TmpData, ' ');
    if Lexem <> '' then begin
      Item.ModifiedDate := StrToTime(Lexem, HHMMSS);

      TmpData := Trim(TmpData);
      Lexem := ExtractFirstWord(TmpData, ' ');
      if Lexem <> '' then begin
        Item.ModifiedDate := Item.ModifiedDate + StrToDate(Lexem, MMDDYY);

        TmpData := Trim(TmpData);
        if TmpData <> '' then begin
          NewItem := CreateItem(Item.Collection as TScFTPDirectoryListing);
          NewItem.Data := TmpData;
          ParseLine(NewItem, Path);
        end;
      end;
    end;
  end;

  Result := True;
end;

{ TScFTP_HellSoftParser }

class function TScFTP_HellSoftParser.Identitifier: string;
begin
  Result := 'Hellsoft';
end;

class function TScFTP_HellSoftParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
  Perms: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  if IsTotalLine(Listing[0]) then begin
    if Listing.Count > 1 then begin
      Str := Listing[1];
      Result := True;
    end;
  end
  else begin
    Str := Listing[0];
    Result := True;
  end;

  if Result then begin
    Result := Str <> '';
    if Result then begin
      Result := CharInSet(Str[1], ['d', 'D', '-']);
      if Result then begin
        Perms := ExtractNovellPermissions(Copy(Str, 1, 12));
        Result := (Length(Perms) = 7) and IsValidNovellPermissions(Perms);
      end;
    end;
  end;
end;

{ TScFTP_EPLFParser }

class function TScFTP_EPLFParser.Identitifier: string;
begin
  Result := 'EPLF';
end;

class function TScFTP_EPLFParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_EPLFListItem;
end;

class function TScFTP_EPLFParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := Listing.Count > 0;

  if Result then begin
    Str := Listing[0];
    Result := (Length(Str) > 2) and (Str[1] = '+') and (Pos(#9, Str) > 0);
  end;
end;

class function TScFTP_EPLFParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Lexems: TStrings;
  TmpData, Lexem: string;
  FileName: string;
  Date: double;
  i: integer;
begin
  Lexems := TStringList.Create;
  try
    FileName := Copy(Item.Data, 2, MaxInt);
    TmpData := ExtractFirstWord(FileName, #9);
    repeat
      Lexems.Add(ExtractFirstWord(TmpData, ','));
    until TmpData = '';

    Item.FileName := FileName;

    for i := 0 to Lexems.Count - 1 do begin
      Lexem := Lexems[i];

      if Lexem = '/' then
        Item.FileType := fftDirectory
      else
      if Lexem = 'r' then
        Item.FileType := fftFile
      else
      if Length(Lexem) > 0 then begin
        case Lexem[1] of
          's':
            Item.Size := StrToInt64Def(Copy(Lexem, 2, MaxInt), 0);
          'm': begin
            Lexem := Copy(Lexem, 2, MaxInt);
            Date := StrToInt64(Lexem)/(24 * 60 * 60) + UnixDateDelta;
            Item.ModifiedDate := Date + GetLocalTimeZoneOffset;
            Item.ModifiedDateGMT := Date;
          end;
          'i':
            TScFTP_EPLFListItem(Item).UniqueID := Copy(Lexem, 2, MaxInt);
          'u':
            if (Length(Lexem) > 1) and (Lexem[2] = 'p') then begin
              Item.Permissions := Copy(Lexem, 3, MaxInt);
              Item.PermissionsDisplay := Item.Permissions;
            end;
        end;
      end;
    end;
  finally
    Lexems.Free;
  end;

  Result := True;
end;

{ TScFTP_DistinctTCPIPParser }

class function TScFTP_DistinctTCPIPParser.Identitifier: string;
begin
  Result := 'Distinct TCP/IP';
end;

class function TScFTP_DistinctTCPIPParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_DOSBaseListItem;
end;

class function TScFTP_DistinctTCPIPParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Lexems: TStrings;
  Lexem0: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Lexems := TStringList.Create;
  try
    Split(Lexems, Listing[0], ' ', True);

    if Lexems.Count > 2 then begin
      Lexem0 := Lexems[0];
      Result := (Length(Lexem0) = 5) and CharInSet(Lexem0[1], ['-', 'd']) and
        IsNumeric(Lexems[1]) and (StrToMonth(Lexems[2]) > 0);

      if Result then
        Result := CharInSet(Lexem0[1], ['w', 'a', 's', 'h', '-', 'd']) and
                  CharInSet(Lexem0[2], ['w', 'a', 's', 'h', '-', 'd']) and
                  CharInSet(Lexem0[3], ['w', 'a', 's', 'h', '-', 'd']) and
                  CharInSet(Lexem0[4], ['w', 'a', 's', 'h', '-', 'd']) and
                  CharInSet(Lexem0[5], ['w', 'a', 's', 'h', '-', 'd']);
    end;
  finally
    Lexems.Free;
  end;
end;

class function TScFTP_DistinctTCPIPParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Date: string;
  FileAttributes: string;
begin
  Result := False;
  TmpData := TrimLeft(Item.Data);

  TScFTP_DOSBaseListItem(Item).Attributes.ReadOnly := True;
  FileAttributes := ExtractFirstWord(TmpData, ' ');
  TScFTP_DOSBaseListItem(Item).Attributes.AddAttribute(FileAttributes);
  TmpData := TrimLeft(TmpData);
  if (FileAttributes <> '') and (LowerCase(FileAttributes[1]) = 'd') then
    Item.FileType := fftDirectory;

  Item.Size := StrToInt64Def(ExtractFirstWord(TmpData, ' '), 0);
  TmpData := TrimLeft(TmpData);

  Date := ExtractFirstWord(TmpData, ' ');
  if StrToMonth(Date) = 0 then
    Exit;
  TmpData := TrimLeft(TmpData);

  Date := Date + ' ' + ExtractFirstWord(TmpData, ' ');
  Date := StringReplace(Date, ',', ' ', [rfReplaceAll]);
  Item.ModifiedDate := StrToDate(Date, MonthDDYY);

  TmpData := TrimLeft(TmpData);
  Date := ExtractFirstWord(TmpData, ' ');
  if not IsTime(Date, HHMMSS) then
    Exit;
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(Date, HHMMSS);
  Item.ModifiedDateGMT := Item.ModifiedDate;
  Item.ModifiedDate := Item.ModifiedDate + GetLocalTimeZoneOffset;

  Item.FileName := TrimLeft(TmpData);
  Result := True;
end;

{ TScFTP_CiscoIOSParser }

class function TScFTP_CiscoIOSParser.Identitifier: string;
begin
  Result := 'Cisco IOS';
end;

class function TScFTP_CiscoIOSParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
begin
  Result := (SysDescript <> '') and (AnsiSameText(Copy(SysDescript, 1, Length(Identitifier)), Identitifier));
end;

{ TScFTP_ChameleonNewtParser }

class function TScFTP_ChameleonNewtParser.Identitifier: string;
begin
  Result := 'NetManage Chameleon/Newt';
end;

class function TScFTP_ChameleonNewtParser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_DOSBaseListItem;
end;

class function TScFTP_ChameleonNewtParser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str, Lexem: string;
  Day: integer;
  i: integer;
begin
  Result := False;

  for i := 0 to Listing.Count - 1 do begin
    Str := Listing[i];

    ExtractFirstWord(Str, ' ');
    Str := TrimLeft(Str);
    Lexem := ExtractFirstWord(Str, ' ');
    Result := (Lexem = '<DIR>') or IsNumeric(Lexem);
    if not Result then begin
      Exit;
    end;
    Str := TrimLeft(Str);

    Result := StrToMonth(ExtractFirstWord(Str, ' ')) > 0;
    if not Result then
      Exit;

    Str := TrimLeft(Str);
    Day := StrToIntDef(ExtractFirstWord(Str, ' '), 0);
    Result := (Day > 0) and (Day <= 31);
    if not Result then
      Exit;

    Str := TrimLeft(Str);
    Result := IsNumeric(ExtractFirstWord(Str, ' '));
    if not Result then
      Exit;

    Str := TrimLeft(Str);
    Result := IsTime(ExtractFirstWord(Str, ' '), HHMMSS);
    if not Result then
      Exit;

    repeat
      Str := TrimLeft(Str);
      if Str = '' then
        Break;
      Result := TScFTP_OS2Parser.IsValidAttr(ExtractFirstWord(Str, ' '));
    until not Result;
  end;
end;

class function TScFTP_ChameleonNewtParser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  TmpData, Lexem: string;
  Day, Month, Year: integer;
begin
  TmpData := Item.Data;

  Item.FileName := ExtractFirstWord(TmpData, ' ');
  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  if Lexem = '<DIR>' then begin
    Item.FileType := fftDirectory;
    Item.SizeAvailable := False;
  end
  else begin
    Item.FileType := fftFile;
    Result := IsNumeric(Lexem);
    if not Result then
      Exit;
    Item.Size := StrToInt64Def(Lexem, 0);
  end;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Month := StrToMonth(Lexem);
  Result := Month > 0;
  if not Result then
    Exit;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Day := StrToInt64Def(Lexem, 0);
  Result := (Day > 0) and (Day <= 31);
  if not Result then
    Exit;

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Result := IsNumeric(Lexem);
  if not Result then
    Exit;
  Year := CheckYear4(StrToIntDef(Lexem, 0));
  Item.ModifiedDate := EncodeDate(Year, Month, Day);

  TmpData := TrimLeft(TmpData);
  Lexem := ExtractFirstWord(TmpData, ' ');
  Result := IsTime(Lexem, HHMMSS);
  if not Result then
    Exit;
  Item.ModifiedDate := Item.ModifiedDate + StrToTime(Lexem, HHMMSS);

  repeat
    TmpData := TrimLeft(TmpData);
    if TmpData = '' then
      Break;
    Lexem := ExtractFirstWord(TmpData, ' ');
    Result := TScFTP_DOSBaseListItem(Item).FAttributes.AddAttribute(Lexem);
  until not Result;
end;

{ TScFTP_BullGCOS8Parser }

class function TScFTP_BullGCOS8Parser.Identitifier: string;
begin
  Result := 'Bull GCOS8';
end;

class function TScFTP_BullGCOS8Parser.GetItemClassType: TScFTPListItemClass;
begin
  Result := TScFTP_UnixPermListItem;
end;

class function TScFTP_BullGCOS8Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  Result := (Length(Str) > 59) and
    CharInSet(Str[1], ['d', '-']) and
    (Str[2] = ' ') and
    CharInSet(Str[3], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Str[4], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Str[5], ['t', 's', 'r', 'w', 'x', '-']) and
    (Str[6] = ' ') and
    CharInSet(Str[7], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Str[8], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Str[9], ['t', 's', 'r', 'w', 'x', '-']) and
    (Str[10] = ' ') and
    CharInSet(Str[11], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Str[12], ['t', 's', 'r', 'w', 'x', '-']) and
    CharInSet(Str[13], ['t', 's', 'r', 'w', 'x', '-']) and
    (Str[14] = ' ') and
    IsNumeric(Str[25]) and
    (Str[26] = ' ');
end;

class function TScFTP_BullGCOS8Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Date: string;
begin
  if Length(Item.Data) > 0 then begin
    if Item.Data[1] = 'd' then
      Item.FileType := fftDirectory
    else
      Item.FileType := fftFile;

    Item.FileName := Copy(Item.Data, 60, Length(Item.Data));
    TScFTP_UnixPermListItem(Item).OwnerPermissions := Copy(Item.Data, 3, 3);
    TScFTP_UnixPermListItem(Item).GroupPermissions := Copy(Item.Data, 7, 3);
    TScFTP_UnixPermListItem(Item).OtherPermissions := Copy(Item.Data, 11, 3);
    Item.Permissions := Copy(Item.Data, 1, 13);
    Item.PermissionsDisplay := Item.Permissions;
    Item.Size := StrToInt64Def(Copy(Item.Data, 15, 11), 0);
    Item.OwnerName := Trim(Copy(Item.Data, 46, 14));

    Item.ModifiedDate := StrToDate(Copy(Item.Data, 27, 8), MMDDYY);
    Date := Copy(Item.Data, 36, 8);
    if Length(Trim(Date)) > 0 then
      Item.ModifiedDate := Item.ModifiedDate + StrToTime(Date, HHMMSS);
  end;

  Result := True;
end;

{ TScFTP_BullGCOS7Parser }

class function TScFTP_BullGCOS7Parser.Identitifier: string;
begin
  Result := 'Bull GCOS7';
end;

class function TScFTP_BullGCOS7Parser.CheckIfListing(Listing: TStrings;
  const SysDescript: string = ''): boolean;
var
  Str: string;
begin
  Result := False;
  if Listing.Count = 0 then
    Exit;

  Str := Listing[0];
  Result := (Length(Str) >= 55) and
    CharInSet(Str[1], ['-', 'd']) and
    (Str[2] = ' ') and
    CharInSet(Str[3], ['-', 'd', 's', 'm']) and
    (Str[4] = ' ') and
    (Str[24] = ' ') and
    (Str[25] <> ' ') and
    CharInSet(Str[46], ['0'..'9', ' ']) and
    CharInSet(Str[47], ['0'..'9', ' ']) and
    CharInSet(Str[48], [',', ' ']) and
    (Str[49] = ' ') and
    CharInSet(Str[50], ['0'..'9', ' ']) and
    CharInSet(Str[51], ['0'..'9', ' ']) and
    CharInSet(Str[52], ['0'..'9', ' ']) and
    CharInSet(Str[53], ['0'..'9', ' ']) and
    (Str[54] = ' ') and
    (Str[55] <> ' ');
end;

class function TScFTP_BullGCOS7Parser.ParseLine(Item: TScFTPListItem; const Path: string = ''): boolean;
var
  Date: string;
begin
  if Item.Data[1] = 'd' then
    Item.FileType := fftDirectory
  else
    Item.FileType := fftFile;

  Item.FileName := Copy(Item.Data, 55, MaxInt);
  Date := StringReplace(Copy(Item.Data, 42, 12), ',', '', [rfReplaceAll]);
  if Trim(Date) <> '' then
    Item.ModifiedDate := StrToDate(Date, MonthDDYY);
  Item.OwnerName := Trim(Copy(Item.Data, 25, 17));

  Result := True;
end;

end.

