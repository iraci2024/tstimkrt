{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressOfficeCore Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxOLEDocument;

interface

{$I cxVer.Inc}

uses
  RTLConsts, Windows, ActiveX, Math, Classes, Generics.Collections,
  Generics.Defaults,
  cxClasses, SysUtils, dxCore, dxCoreClasses;

const
  PID_DICTIONARY = 0;
{$EXTERNALSYM PID_DICTIONARY}
  PID_CODEPAGE = 1; // VT_I2
{$EXTERNALSYM PID_CODEPAGE}
  PID_TITLE = 2; // VT_LPSTR
{$EXTERNALSYM PID_TITLE}
  PID_SUBJECT = 3; // VT_LPSTR
{$EXTERNALSYM PID_SUBJECT}
  PID_AUTHOR = 4; // VT_LPSTR
{$EXTERNALSYM PID_AUTHOR}
  PID_KEYWORDS = 5; // VT_LPSTR
{$EXTERNALSYM PID_KEYWORDS}
  PID_COMMENTS = 6; // VT_LPSTR
{$EXTERNALSYM PID_COMMENTS}
  PID_TEMPLATE = 7; // VT_LPSTR
{$EXTERNALSYM PID_TEMPLATE}
  PID_LASTAUTHOR = 8; // VT_LPSTR
{$EXTERNALSYM PID_LASTAUTHOR}
  PID_REVNUMBER = 9; // VT_LPSTR
{$EXTERNALSYM PID_REVNUMBER}
  PID_EDITTIME = 10; // VT_FILETIME
{$EXTERNALSYM PID_EDITTIME}
  PID_LASTPRINTED = 11; // VT_FILETIME
{$EXTERNALSYM PID_LASTPRINTED}
  PID_CREATE_DTM = 12; // VT_FILETIME
{$EXTERNALSYM PID_CREATE_DTM}
  PID_LASTSAVE_DTM = 13; // VT_FILETIME
{$EXTERNALSYM PID_LASTSAVE_DTM}
  PID_PAGECOUNT = 14; // VT_I4
{$EXTERNALSYM PID_PAGECOUNT}
  PID_WORDCOUNT = 15; // VT_I4
{$EXTERNALSYM PID_WORDCOUNT}
  PID_CHARCOUNT = 16; // VT_I4
{$EXTERNALSYM PID_CHARCOUNT}
  PID_THUMBNAIL = 17; // VT_CF
{$EXTERNALSYM PID_THUMBNAIL}
  PID_APPNAME = 18; // VT_LPSTR
{$EXTERNALSYM PID_APPNAME}
  PID_SECURITY = 19; // VT_I4
{$EXTERNALSYM PID_SECURITY}
  CP_WINUNICODE = $04B0;
{$EXTERNALSYM CP_WINUNICODE}
  FMTID_SummaryInformation: TGUID = '{F29F85E0-4FF9-1068-AB91-08002B27B3D9}';
{$EXTERNALSYM FMTID_SummaryInformation}
  FMTID_DocSummaryInformation: TGUID = '{D5CDD502-2E9C-101B-9397-08002B2CF9AE}';
{$EXTERNALSYM FMTID_DocSummaryInformation}

type
  EdxOLEDocument = class(Exception);
  TdxOLEDocument = class;

  // common types
  TdxOLEDocumentMode = (dmReading, dmWriting);

  TdxOLEDocumentDirectoryEntryType = (ET_EMPTY, ET_STORAGE, ET_STREAM,
    ET_LOCKBYTES, ET_PROPERTY, ET_ROOT);

  TdxOLEDocumentDirectoryEntryColor = (EC_BLACK, EC_RED);

  { TdxOLEDocumentDirectoryEntry }

  TdxOLEDocumentDirectoryEntry = class(TPersistent)
  strict private
    function GetIsEmpty: Boolean;
    function GetSectorSize: Integer;
    function GetSize: Integer;
    function GetSmallStream: Boolean;
  protected
    FChild: TdxOLEDocumentDirectoryEntry;
    FColor: TdxOLEDocumentDirectoryEntryColor;
    FCreationTime: TTimeStamp;
    FData: TStream;
    FDocument: TdxOLEDocument;
    FEntryType: TdxOLEDocumentDirectoryEntryType;
    FID: TGUID;
    FInitialized: Boolean;
    FLeft: TdxOLEDocumentDirectoryEntry;
    FModificationTime: TTimeStamp;
    FName: WideString;
    FParent: TdxOLEDocumentDirectoryEntry;
    FRight: TdxOLEDocumentDirectoryEntry;
    FSectorID: LongInt;
    FSibling: TdxOLEDocumentDirectoryEntry;
    FSize: LongInt;
    procedure AddSibling(AValue: TdxOLEDocumentDirectoryEntry);
    procedure InitializeLinks;
    procedure ReadFromStream(AStream: TStream);
    procedure WriteDataToStream(AStream: TStream);
    procedure WriteHeaderToStream(AStream: TStream);

    property SectorSize: Integer read GetSectorSize;
    property SmallStream: Boolean read GetSmallStream;
  public
    constructor Create(AOwner: TdxOLEDocument;
      AParent: TdxOLEDocumentDirectoryEntry); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function ChildByName(const AName: WideString): TdxOLEDocumentDirectoryEntry;

    property Color: TdxOLEDocumentDirectoryEntryColor read FColor write FColor;
    property Child: TdxOLEDocumentDirectoryEntry read FChild;
    property CreationTime: TTimeStamp read FCreationTime;
    property Data: TStream read FData;
    property Document: TdxOLEDocument read FDocument;
    property EntryType: TdxOLEDocumentDirectoryEntryType read FEntryType;
    property FirstChild: TdxOLEDocumentDirectoryEntry read FChild;
    property ID: TGUID read FID;
    property IsEmpty: Boolean read GetIsEmpty;
    property Left: TdxOLEDocumentDirectoryEntry read FLeft;
    property ModificationTime: TTimeStamp read FModificationTime;
    property Name: WideString read FName;
    property Parent: TdxOLEDocumentDirectoryEntry read FParent;
    property Right: TdxOLEDocumentDirectoryEntry read FRight;
    property SectorID: LongInt read FSectorID;
    property Size: Integer read GetSize;
  end;

  TdxOLEDocumentSector = array of Integer;

  { TdxOLEDocument }

  TdxOLEDocument = class(TPersistent)
  public const
    RootDirName = 'Root Entry';
  private
    FDirEntries: TcxObjectList;
    procedure FillIndexes(AStartIndex, ACount: Integer; AMiniFAT: Boolean);
    procedure FillFAT(AEntry: TdxOLEDocumentDirectoryEntry;
      var AStartIndex: Integer);
    function GetActuallySourceStream(ASmallStream: Boolean): TStream;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxOLEDocumentDirectoryEntry;
    function GetRoot: TdxOLEDocumentDirectoryEntry;
    function GetSectorSize(ASmallStream: Boolean): Integer;
  protected
    FByteOrder: Word;
    FCLSID: TGUID;
    FDIFAT: TdxOLEDocumentSector;
    FDIFATCount: LongInt;
    FDIFATFirstSector: LongInt;
    FDirEntryCount: LongInt;
    FDirEntryFirstSector: LongInt;
    FFAT: TdxOLEDocumentSector;
    FFATCount: LongInt;
    FFATInHeader: array [0 .. 108] of Integer;
    FMiniFAT: TdxOLEDocumentSector;
    FMiniFATCount: LongInt;
    FMiniFATFirstSector: LongInt;
    FMiniSectorCutoffSize: LongInt;
    FMiniSectorShift: Word;
    FMode: TdxOLEDocumentMode;
    FRootEntry: TdxOLEDocumentDirectoryEntry;
    FSectorShift: Word;
    FStream: TStream;
    FTreeLocked: Boolean;
    FTransaction: LongInt;
    FVersion: Integer;
    function CreateDataStream(ADirEntry: TdxOLEDocumentDirectoryEntry): TStream;
    function CreateSectorsChain(ASectorID: Integer;
      AUseMiniFAT: Boolean = False): TList;
    procedure Check(AValue: Boolean);
    procedure CheckRead(ASize: Integer; var AData; APosition: Integer = -1);
    procedure GetSector(ASectorID: Integer; var ASector: TdxOLEDocumentSector);
    procedure InitializeItemsLinks;
    procedure InitializeRootStorage;
    procedure PrepareDataForCommit;
    procedure PrepareFATForCommit;
    procedure ReadData(AStream: TStream);
    procedure ReadDirectory(AStream: TStream);
    procedure ReadFATSector(ASectorID: Integer;
      var FATSector: TdxOLEDocumentSector);
    procedure WriteData(AStream: TStream);

    property ActuallySourceStream[ASmallStream: Boolean]: TStream
      read GetActuallySourceStream;
    property SectorSize[ASmallStream: Boolean]: Integer read GetSectorSize;
    property Root: TdxOLEDocumentDirectoryEntry read GetRoot;
    property TreeLocked: Boolean read FTreeLocked write FTreeLocked;
  public
    constructor Create(AStream: TStream; AMode: TdxOLEDocumentMode);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Commit;
    function CreateDirEntry(const AName: WideString;
      AEntryType: TdxOLEDocumentDirectoryEntryType = ET_EMPTY;
      AParent: TdxOLEDocumentDirectoryEntry = nil;
      ACheckForExistence: Boolean = False): TdxOLEDocumentDirectoryEntry;
    function CreateStream(AParent: TdxOLEDocumentDirectoryEntry;
      const AName: WideString): TStream;
    function FindByName(const AName: WideString;
      AParent: TdxOLEDocumentDirectoryEntry = nil)
      : TdxOLEDocumentDirectoryEntry;
    function IndexOf(AEntry: TdxOLEDocumentDirectoryEntry): Integer;
    function StreamByName(const AName: WideString;
      AParent: TdxOLEDocumentDirectoryEntry = nil): TStream;

    property CLSID: TGUID read FCLSID;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxOLEDocumentDirectoryEntry
      read GetItem; default;
    property Mode: TdxOLEDocumentMode read FMode;
    property Stream: TStream read FStream;
    property Version: Integer read FVersion;
  end;

  { TdxOLEDocumentProperty }

  TdxOLEDocumentProperty = class
  strict private
    FID: Cardinal;
    FValue: Variant;
  public
    constructor Create(ID: Cardinal);
    //
    property ID: Cardinal read FID;
    property Value: Variant read FValue write FValue;
  end;

  { TdxOLEDocumentPropertySetSection }

  TdxOLEDocumentPropertySetSection = class(TdxFastObjectList)
  strict private
  type
{$REGION 'Internal Types'}
    TPropertyEntry = record
      ID: Cardinal;
      Offset: Cardinal;
    end;

    TPropertyEntries = array of TPropertyEntry;
{$ENDREGION}
  strict private
    FClassID: TGUID;

    function CreateEncoding(ACodePage: Word): TEncoding;
    function GetItem(Index: Integer): TdxOLEDocumentProperty;
    //
    function ReadCodePageString(AStream: TStream): Variant;
    function ReadFileTime(AStream: TStream): Variant;
    function ReadVariant(AStream: TStream): Variant;
    procedure WriteCodePageString(AStream: TStream; const AValue: string);
    procedure WriteFileTime(AStream: TStream; const AValue: TDateTime);
    procedure WriteVariant(AStream: TStream; ID: Cardinal;
      const AValue: Variant);
  protected
    procedure ReadData(AStream: TStream);
    procedure WriteData(AStream: TStream);
  public
    constructor Create(const AClassID: TGUID);
    procedure Add(ID: Cardinal; const AValue: Variant);
    function Find(ID: Cardinal; out AItem: TdxOLEDocumentProperty): Boolean;
    function Get(ID: Cardinal; const ADefaultValue: Variant): Variant;
    procedure Remove(ID: Cardinal);
    //
    property ClassID: TGUID read FClassID;
    property Items[Index: Integer]: TdxOLEDocumentProperty
      read GetItem; default;
  end;

  { TdxOLEDocumentPropertySet }

  TdxOLEDocumentPropertySet = class
  strict private
  const
    OS_WIN16 = $0000;
    OS_MAC = $0001;
    OS_WIN32 = $0002;
  strict private
    FClassID: TGUID;
    FSections: TdxFastObjectList;

    function GetSection(Index: Integer): TdxOLEDocumentPropertySetSection;
    function GetSectionCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AClassID: TGUID): TdxOLEDocumentPropertySetSection;
    procedure Clear;
    function Find(const AClassID: TGUID;
      out ASection: TdxOLEDocumentPropertySetSection): Boolean;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    //
    property ClassID: TGUID read FClassID write FClassID;
    property SectionCount: Integer read GetSectionCount;
    property Sections[Index: Integer]: TdxOLEDocumentPropertySetSection
      read GetSection;
  end;

function dxIsOLEStream(AStream: TStream): Boolean;

implementation

uses
  cxVariants, Variants;

const
  oleSignature = Int64($E11AB1A1E011CFD0); // header signature
  oleDIFATSECTOR = Integer($FFFFFFFC); // double inDIRect FAT
  oleFATSector = Integer($FFFFFFFD); // special block beginning
  oleEndOfChain = Integer($FFFFFFFE); // end of chain
  oleUnused = Integer($FFFFFFFF); // unused
  oleEmpty = Integer($00000000); // empty
  oleDLLVersion = Integer($0003003E); // specification version
  olePlatformOrder = Word($FFFE); // order for intel platform
  oleSectorsInMasterFAT = 109; // sectors in master FAT from header
  oleBlockIDPerBigBlock = 128; // id cound in big block
  oleMaxBlockIDInBigBlock = 127; // id cound in big block
  oleContinueFATItem = 126; // id continue DIF block item
  oleBigBlockShift = 9; // big block shift
  oleSmallBlockShift = 6; // small block shift
  oleReservedSectorCount = 2; // header and directory sectors
  oleMiniSectorCutOffSize = Integer($000001000); // max mini-sector size

  oleBigBlockSize = 1 shl oleBigBlockShift;
  oleSmallBlockSize = 1 shl oleSmallBlockShift;
  oleSectorSize: array [Boolean] of Integer = (oleSmallBlockSize,
    oleBigBlockSize);

  oleRoot: WideString = TdxOLEDocument.RootDirName + #0;
  oleInvalidName: WideString = '';

const
  sdxErrorInvalidOLEDocument = 'The stream is not valid OLE Document.';
  sdxErrorInvalidOLEPropertySet = 'The stream is not valid OLE Property Set.';
  sdxErrorUnsupportedProperty = 'The %d property was not supported';

type

  { TOLEDocumentStreamReader }

  TOLEDocumentStreamReader = class(TStream)
  private
    FPosition: Integer;
    FSectorsChain: TList;
    FSectorSize: Integer;
    FSize: Integer;
    FSource: TStream;
    procedure ValidatePosition;
  protected
    procedure Initialize(ADirEntry: TdxOLEDocumentDirectoryEntry);
  public
    constructor Create(ASource: TStream; ASectorsChain: TList;
      ASize, ASectorSize: Integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;

    property Source: TStream read FSource;
  end;

function dxIsOLEStream(AStream: TStream): Boolean;
var
  APosition, ASignature: Int64;
begin
  APosition := AStream.Position;
  Result := AStream.Size > oleBigBlockSize * 2;
  if Result then
  begin
    AStream.ReadBuffer(ASignature, SizeOf(ASignature));
    Result := ASignature = oleSignature;
  end;
  AStream.Position := APosition;
end;

function AlignBySector(AStreamSize: Integer): Integer;
begin
  Result := RoundDiv(AStreamSize,
    oleSectorSize[AStreamSize > oleMiniSectorCutOffSize]) * oleSectorSize
    [AStreamSize > oleMiniSectorCutOffSize];
end;

procedure AlignStream(AStream: TStream; ASectorSize: Integer);
var
  ALeft: Integer;
  ABuffer: Pointer;
begin
  ALeft := RoundDiv(AStream.Size, ASectorSize) * ASectorSize - AStream.Size;
  if ALeft = 0 then
    Exit;
  GetMem(ABuffer, ALeft);
  FillChar(ABuffer^, ALeft, $FF);
  AStream.WriteBuffer(ABuffer^, ALeft);
  FreeMem(ABuffer);
end;

function CopyStreamBySectors(ASource, ADest: TStream;
  ASectorSize: Integer): Integer;
begin
  ASource.Position := 0;
  ADest.CopyFrom(ASource, ASource.Size);
  Result := RoundDiv(ASource.Size, ASectorSize);
  AlignStream(ADest, ASectorSize);
end;

function CompareDirEntry(AEntry1,
  AEntry2: TdxOLEDocumentDirectoryEntry): Integer;
begin
  Result := WideCompareStr(AEntry1.Name, AEntry2.Name);
end;

procedure FillInteger(var AData; ACount, APattern: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    PIntegerArray(@AData)^[I] := APattern;
end;

procedure AllocateSectors(var AData: TdxOLEDocumentSector;
  const ACount: Integer);
begin
  if ACount <= 0 then
    Exit;
  SetLength(AData, ACount * oleBlockIDPerBigBlock);
  FillInteger(AData[0], Length(AData), oleUnused);
end;

procedure WriteInteger(AStream: TStream; const AValue: Integer);
begin
  AStream.WriteBuffer(AValue, SizeOf(Integer));
end;

{ TOLEDocumentStreamReader }

constructor TOLEDocumentStreamReader.Create(ASource: TStream;
  ASectorsChain: TList; ASize, ASectorSize: Integer);
begin
  FSource := ASource;
  FSectorsChain := ASectorsChain;
  FSize := ASize;
  FSectorSize := ASectorSize;
  FPosition := 0;
end;

destructor TOLEDocumentStreamReader.Destroy;
begin
  FSectorsChain.Free;
  inherited Destroy;
end;

function TOLEDocumentStreamReader.Read(var Buffer; Count: Integer): LongInt;
var
  ADest: PByte;
  ASize: Integer;
begin
  if FSectorsChain.Count = 0 then
  begin
    Result := Source.Read(Buffer, Count);
    Exit;
  end;
  Result := 0;
  ADest := @Buffer;
  while (Count > 0) and (FPosition < FSize) do
  begin
    ASize := Min(FSectorSize - FPosition mod FSectorSize, Count);
    ValidatePosition;
    Inc(Result, FSource.Read(ADest^, ASize));
    Inc(ADest, ASize);
    Dec(Count, ASize);
    Inc(FPosition, ASize);
  end;
  ValidatePosition;
end;

function TOLEDocumentStreamReader.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (FSectorsChain <> nil) and (FSectorsChain.Count = 0) then
  begin
    Result := Source.Seek(Offset, Origin);
    Exit;
  end;
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      FPosition := FPosition + Offset;
    soEnd:
      FPosition := FSize + Offset;
  end;
  FPosition := Min(FSize, Max(0, FPosition));
  ValidatePosition;
  Result := FPosition;
end;

function TOLEDocumentStreamReader.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := -1;
end;

procedure TOLEDocumentStreamReader.Initialize
  (ADirEntry: TdxOLEDocumentDirectoryEntry);
begin
  FPosition := 0;
  FSize := ADirEntry.Size;
  FSectorSize := ADirEntry.SectorSize;
  FSource := ADirEntry.Document.ActuallySourceStream[ADirEntry.SmallStream];
  FSectorsChain := ADirEntry.Document.CreateSectorsChain(ADirEntry.SectorID,
    ADirEntry.SmallStream);
end;

procedure TOLEDocumentStreamReader.ValidatePosition;
var
  AChain, AOffset: Integer;
begin
  AChain := FPosition div FSectorSize;
  AOffset := FPosition mod FSectorSize;
  if FSectorsChain = nil then
    Exit;
  if AChain >= FSectorsChain.Count then
  begin
    AChain := FSectorsChain.Count - 1;
    AOffset := FSectorSize;
  end;
  FSource.Position := Integer(FSectorsChain[AChain]) * FSectorSize + AOffset;
  // 202 203 12928
end;

{ TdxOLEDocumentDirectoryEntry }

constructor TdxOLEDocumentDirectoryEntry.Create(AOwner: TdxOLEDocument;
  AParent: TdxOLEDocumentDirectoryEntry);
begin
  FDocument := AOwner;
  FColor := EC_RED;
  FParent := AParent;
  if Document <> nil then
    FData := Document.CreateDataStream(Self);
end;

destructor TdxOLEDocumentDirectoryEntry.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TdxOLEDocumentDirectoryEntry.Assign(ASource: TPersistent);
var
  AEntry: TdxOLEDocumentDirectoryEntry;
begin
  if ASource is TdxOLEDocumentDirectoryEntry then
  begin
    AEntry := TdxOLEDocumentDirectoryEntry(ASource);
    FName := AEntry.Name;
    FEntryType := AEntry.EntryType;
    FColor := AEntry.Color;
    if (EntryType <> ET_ROOT) and not AEntry.IsEmpty then
    begin
      AEntry.Data.Position := 0;
      FData.CopyFrom(AEntry.Data, AEntry.Size);
    end;
    FID := AEntry.ID;
    FCreationTime := AEntry.CreationTime;
    FModificationTime := AEntry.FModificationTime;
    if FName <> '' then
      FSize := AEntry.Size;
    TdxNativeInt(FLeft) := AEntry.Document.IndexOf(AEntry.Left);
    TdxNativeInt(FRight) := AEntry.Document.IndexOf(AEntry.Right);
    TdxNativeInt(FChild) := AEntry.Document.IndexOf(AEntry.Child);
  end;
end;

function TdxOLEDocumentDirectoryEntry.ChildByName(const AName: WideString)
  : TdxOLEDocumentDirectoryEntry;

  procedure CheckItem(const AItem: TdxOLEDocumentDirectoryEntry);
  begin
    if Result <> nil then
      Exit;
    if WideSameText(AItem.Name, AName) then
      Result := AItem;
    if AItem.Left <> nil then
      CheckItem(AItem.Left);
    if AItem.Right <> nil then
      CheckItem(AItem.Right);
  end;

begin
  Result := nil;
  if AName <> '' then
    CheckItem(Child);
end;

procedure TdxOLEDocumentDirectoryEntry.AddSibling
  (AValue: TdxOLEDocumentDirectoryEntry);
var
  ANode: TdxOLEDocumentDirectoryEntry;
begin
  if AValue = Self then
    Exit;
  ANode := Self;
  while ANode.FSibling <> nil do
  begin
    if AValue = ANode then
      Exit;
    ANode := ANode.FSibling;
  end;
  ANode.FSibling := AValue;
end;

procedure TdxOLEDocumentDirectoryEntry.InitializeLinks;

  procedure CheckNode(var ANode: TdxOLEDocumentDirectoryEntry;
    const AParent: TdxOLEDocumentDirectoryEntry);
  begin
    if (Integer(ANode) >= 0) and (TdxNativeInt(ANode) < Document.Count) then
    begin
      ANode := Document[TdxNativeInt(ANode)];
      ANode.FParent := AParent;
    end
    else
      ANode := nil;
  end;

begin
  CheckNode(FChild, Self);
  CheckNode(FLeft, Parent);
  CheckNode(FRight, Parent);
end;

procedure TdxOLEDocumentDirectoryEntry.ReadFromStream(AStream: TStream);
var
  AName: array [0 .. 64 - 1] of Byte;
begin
  AStream.ReadBuffer(AName, SizeOf(AName));
  AStream.ReadBuffer(FSize, SizeOf(Word));
  FName := PWideChar(@AName);
  AStream.ReadBuffer(FEntryType, SizeOf(Byte));
  if FName = '' then
    FEntryType := ET_EMPTY;
  AStream.ReadBuffer(FColor, SizeOf(Byte));
  AStream.ReadBuffer(FLeft, SizeOf(Integer));
  AStream.ReadBuffer(FRight, SizeOf(Integer));
  AStream.ReadBuffer(FChild, SizeOf(Integer));
  AStream.ReadBuffer(FID, SizeOf(TGUID));
  AStream.Position := AStream.Position + SizeOf(Integer);
  AStream.ReadBuffer(FCreationTime, SizeOf(TTimeStamp));
  AStream.ReadBuffer(FModificationTime, SizeOf(TTimeStamp));
  AStream.ReadBuffer(FSectorID, SizeOf(Integer));
  AStream.ReadBuffer(FSize, SizeOf(Integer));
  FSize := Max(0, FSize);
  AStream.Position := AStream.Position + SizeOf(Integer);
  if (FSize = 0) or not(EntryType in [ET_ROOT, ET_STREAM, ET_PROPERTY]) then
    Exit;
  TOLEDocumentStreamReader(FData).Initialize(Self);
end;

procedure TdxOLEDocumentDirectoryEntry.WriteDataToStream(AStream: TStream);
begin
  if SmallStream or ((Size <= 0) and (EntryType = ET_STREAM)) then
    Exit;
  CopyStreamBySectors(Data, AStream, oleBigBlockSize);
end;

procedure TdxOLEDocumentDirectoryEntry.WriteHeaderToStream(AStream: TStream);

  function CheckIndex(AIndex: Integer): Integer;
  begin
    Result := AIndex;
    if FEntryType = ET_EMPTY then
      Result := 0;
  end;

var
  L: Word;
  AName: array [0 .. 64 - 1] of Byte;
begin
  FillChar(AName, SizeOf(AName), 0);
  L := Min(Length(FName) * 2, SizeOf(AName) - 2);
  Move(FName[1], AName[0], L);
  Inc(L, 2);
  AStream.WriteBuffer(AName, SizeOf(AName));
  AStream.WriteBuffer(L, SizeOf(Word));
  AStream.WriteBuffer(FEntryType, SizeOf(Byte));
  AStream.WriteBuffer(FColor, SizeOf(Byte));
  WriteInteger(AStream, CheckIndex(Document.IndexOf(Left)));
  WriteInteger(AStream, CheckIndex(Document.IndexOf(Right)));
  if FEntryType = ET_ROOT then
    WriteInteger(AStream, 1)
  else
    WriteInteger(AStream, CheckIndex(Document.IndexOf(Child)));
  AStream.WriteBuffer(FID, SizeOf(TGUID));
  WriteInteger(AStream, 0);
  AStream.WriteBuffer(FCreationTime, SizeOf(TTimeStamp));
  AStream.WriteBuffer(FModificationTime, SizeOf(TTimeStamp));
  WriteInteger(AStream, FSectorID);
  WriteInteger(AStream, Data.Size);
  WriteInteger(AStream, 0);
end;

function TdxOLEDocumentDirectoryEntry.GetIsEmpty: Boolean;
begin
  Result := (Length(FName) = 0) or (FSize = 0) or
    not(EntryType in [ET_ROOT, ET_STORAGE, ET_STREAM, ET_PROPERTY]);
end;

function TdxOLEDocumentDirectoryEntry.GetSectorSize: Integer;
begin
  Result := Document.SectorSize[SmallStream];
end;

function TdxOLEDocumentDirectoryEntry.GetSize: Integer;
begin
  if Document.Mode <> dmReading then
    FSize := FData.Size;
  Result := FSize;
end;

function TdxOLEDocumentDirectoryEntry.GetSmallStream: Boolean;
begin
  Result := (Size < Document.FMiniSectorCutoffSize) and
    not(EntryType in [ET_ROOT, ET_STORAGE]);
end;

{ TdxOLEDocument }

constructor TdxOLEDocument.Create(AStream: TStream; AMode: TdxOLEDocumentMode);
begin
  FMode := AMode;
  FStream := AStream;
  FDirEntries := TcxObjectList.Create;
  if AMode = dmReading then
    ReadData(AStream)
  else
    InitializeRootStorage;
end;

destructor TdxOLEDocument.Destroy;
begin
  FDirEntries.Free;
  inherited Destroy;
end;

procedure TdxOLEDocument.Assign(ASource: TPersistent);
var
  I: Integer;
  ADocument: TdxOLEDocument;
begin
  if ASource is TdxOLEDocument then
  begin
    TreeLocked := True;
    try
      ADocument := TdxOLEDocument(ASource);
      FCLSID := ADocument.CLSID;
      for I := 0 to ADocument.Count - 1 do
        CreateDirEntry('').Assign(ADocument[I]);
      InitializeItemsLinks;
    finally
      TreeLocked := False
    end;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxOLEDocument.Commit;
begin
  if FMode <> dmWriting then
    Exit;
  PrepareDataForCommit;
  PrepareFATForCommit;
  WriteData(Stream)
end;

function TdxOLEDocument.CreateDirEntry(const AName: WideString;
  AEntryType: TdxOLEDocumentDirectoryEntryType = ET_EMPTY;
  AParent: TdxOLEDocumentDirectoryEntry = nil;
  ACheckForExistence: Boolean = False): TdxOLEDocumentDirectoryEntry;

  procedure LinkValue(var AField: TdxOLEDocumentDirectoryEntry;
    const ALink, ARelatedLink: TdxOLEDocumentDirectoryEntry);
  begin
    AField := ALink;
    if ARelatedLink.Color = EC_RED then
      ALink.Color := EC_BLACK;
  end;

var
  ANode: TdxOLEDocumentDirectoryEntry;
begin
  if ACheckForExistence then
  begin
    Result := FindByName(AName, AParent);
    if Result <> nil then
      Exit;
  end;
  Result := TdxOLEDocumentDirectoryEntry.Create(Self, AParent);
  if AEntryType = ET_ROOT then
    FRootEntry := Result;
  Result.FEntryType := AEntryType;
  Result.FName := AName;
  if Count = 0 then // Set Root color
    Result.Color := EC_BLACK;
  FDirEntries.Add(Result);
  if (Mode = dmReading) or TreeLocked or (Result = Root) or
    (AEntryType = ET_EMPTY) then
    Exit;

  if AParent = nil then
    AParent := Root
  else if AParent.FChild <> nil then
    AParent.FChild.AddSibling(Result);

  if AParent.Child = nil then
    LinkValue(AParent.FChild, Result, AParent)
  else
  begin
    ANode := AParent.FChild;
    if (AParent <> Root) and (ANode <> nil) and (ANode.Left = ANode.Right) then
    begin
      AParent.FChild := Result;
      Result.FLeft := ANode;
    end
    else if ANode.Right = nil then
      LinkValue(ANode.FRight, Result, ANode)
    else if ANode.Left = nil then
      LinkValue(ANode.FLeft, Result, ANode)
    else
    begin
      while ANode.Right <> nil do
        ANode := ANode.Right;
      LinkValue(ANode.FRight, Result, ANode);
    end;
  end;
end;

function TdxOLEDocument.CreateStream(AParent: TdxOLEDocumentDirectoryEntry;
  const AName: WideString): TStream;
begin
  Result := StreamByName(AName, AParent);
  if Result = nil then
    Result := CreateDirEntry(AName, ET_STREAM, AParent).Data;
end;

function TdxOLEDocument.FindByName(const AName: WideString;
  AParent: TdxOLEDocumentDirectoryEntry = nil): TdxOLEDocumentDirectoryEntry;
begin
  if SameText(AName, RootDirName) then
  begin
    Result := FRootEntry;
    Exit;
  end;
  if AParent = nil then
    AParent := FRootEntry;
  Result := AParent.FChild;
  while Result <> nil do
  begin
    if WideSameText(Result.Name, AName) then
      Exit;
    Result := Result.FSibling;
  end;
  Result := nil;
end;

function TdxOLEDocument.IndexOf(AEntry: TdxOLEDocumentDirectoryEntry): Integer;
begin
  Result := FDirEntries.IndexOf(AEntry);
end;

function TdxOLEDocument.StreamByName(const AName: WideString;
  AParent: TdxOLEDocumentDirectoryEntry = nil): TStream;
var
  AEntry: TdxOLEDocumentDirectoryEntry;
begin
  AEntry := FindByName(AName, AParent);
  if AEntry <> nil then
    Result := AEntry.Data
  else
    Result := nil;
end;

function TdxOLEDocument.CreateDataStream(ADirEntry
  : TdxOLEDocumentDirectoryEntry): TStream;
begin
  if FMode = dmReading then
    Result := TOLEDocumentStreamReader.Create(Stream, nil, 0,
      1 shl FSectorShift)
  else
    Result := TMemoryStream.Create;
end;

function TdxOLEDocument.CreateSectorsChain(ASectorID: Integer;
  AUseMiniFAT: Boolean = False): TList;
var
  FAT: TdxOLEDocumentSector;
  // B: PIntegerArray;
  AReader: TOLEDocumentStreamReader;
begin
  Result := TList.Create;
  if not AUseMiniFAT then
    try
      if ASectorID = -2 then
        Exit;
      repeat
        Result.Add(Pointer(ASectorID + 1));
        ReadFATSector(ASectorID div oleBlockIDPerBigBlock, FAT);
        ASectorID := FAT[ASectorID mod oleBlockIDPerBigBlock];
      until ASectorID = oleEndOfChain;
    except
      Result.Free;
      raise;
    end
  else
  begin
    AReader := TOLEDocumentStreamReader.Create(Stream,
      CreateSectorsChain(FMiniFATFirstSector),
      FMiniFATCount * SectorSize[False], SectorSize[False]);
    try
      { AReader.Position := 0;
        B := AllocMem(FMiniFATCount * SectorSize[False]);
        AReader.ReadBuffer(B^, AReader.Size); }
      AReader.Position := ASectorID * SizeOf(Integer);
      while ASectorID <> oleEndOfChain do
      begin
        Result.Add(Pointer(ASectorID));
        AReader.Position := ASectorID * SizeOf(Integer);
        AReader.ReadBuffer(ASectorID, SizeOf(Integer))
      end;
    finally
      AReader.Free;
    end;
  end;
end;

procedure TdxOLEDocument.Check(AValue: Boolean);
begin
  if not AValue then
    raise EdxOLEDocument.Create(sdxErrorInvalidOLEDocument);
end;

procedure TdxOLEDocument.CheckRead(ASize: Integer; var AData;
  APosition: Integer = -1);
begin
  if APosition <> -1 then
  begin
    Stream.Position := APosition;
    Check(Stream.Position = APosition);
  end;
  Check(Stream.Read(AData, ASize) = ASize)
end;

procedure TdxOLEDocument.GetSector(ASectorID: Integer;
  var ASector: TdxOLEDocumentSector);
begin
  SetLength(ASector, (1 shl FSectorShift) div SizeOf(Integer));
  CheckRead(1 shl FSectorShift, ASector[0], (ASectorID + 1) shl FSectorShift);
end;

procedure TdxOLEDocument.InitializeItemsLinks;

  procedure RestoreTree(AChild, AParent: TdxOLEDocumentDirectoryEntry);
  begin
    if (AChild = nil) or AChild.FInitialized then
      Exit;
    AChild.FInitialized := True;
    AChild.FParent := AParent;
    if AParent.FChild <> nil then
      AParent.FChild.AddSibling(AChild);
    RestoreTree(AChild.FLeft, AParent);
    RestoreTree(AChild.FRight, AParent);
    RestoreTree(AChild.FChild, AChild);
  end;

var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].InitializeLinks;
  RestoreTree(FRootEntry.FChild, FRootEntry);
end;

procedure TdxOLEDocument.InitializeRootStorage;
begin
  FVersion := oleDLLVersion;
  FByteOrder := olePlatformOrder;
  FSectorShift := oleBigBlockShift;
  FMiniSectorShift := oleSmallBlockShift;
  FFATCount := 1;
  FDirEntryFirstSector := 0;
  FMiniSectorCutoffSize := oleMiniSectorCutOffSize;
  FMiniFATFirstSector := oleEndOfChain;
  FMiniFATCount := oleEmpty;
  FDIFATFirstSector := oleEndOfChain;
  FDIFATCount := oleEmpty;
  FillInteger(FFATInHeader, Length(FFATInHeader), oleUnused);
end;

procedure TdxOLEDocument.PrepareDataForCommit;
var
  I, ABigBlocksCount: Integer;
begin
  FDirEntryCount := RoundDiv(Count, 4);
  ABigBlocksCount := 0;
  Root.Data.Size := 0;
  // Count sectors for streams
  for I := 1 to Count - 1 do
    if Items[I].SmallStream then
      FMiniFATCount := FMiniFATCount + CopyStreamBySectors(Items[I].Data,
        Root.Data, oleSmallBlockSize)
    else
      Inc(ABigBlocksCount, RoundDiv(Items[I].Size, oleBigBlockSize));
  FMiniFATCount := RoundDiv(FMiniFATCount, oleBlockIDPerBigBlock);
  ABigBlocksCount := 1 { Header } + FDirEntryCount { directory } +
    FMiniFATCount { miniFAT } + RoundDiv(Root.Size, oleBigBlockSize)
  { mini blocks stream } + ABigBlocksCount;
  FFATCount := RoundDiv(ABigBlocksCount, oleMaxBlockIDInBigBlock);
end;

procedure TdxOLEDocument.PrepareFATForCommit;
var
  I, ASector: Integer;
  AIndexInFAT: array [Boolean] of Integer;
begin
  // Master FAT and DIFAT
  ASector := FDirEntryCount; // sectors for header and dir entry
  if FFATCount > oleSectorsInMasterFAT then
  begin
    FDIFATCount := RoundDiv(FFATCount - oleSectorsInMasterFAT,
      oleMaxBlockIDInBigBlock);
    FDIFATFirstSector := ASector;
    AllocateSectors(FDIFAT, FDIFATCount);
    Inc(ASector, FDIFATCount);
  end;
  for I := 0 to Min(FFATCount, oleSectorsInMasterFAT) - 1 do
    FFATInHeader[I] := ASector + I;
  Inc(ASector, Min(FFATCount, oleSectorsInMasterFAT));
  for I := 0 to FFATCount - oleSectorsInMasterFAT + FDIFATCount - 2 do
    if (I + 1) mod oleBlockIDPerBigBlock = 0 then
      FDIFAT[I] := FDIFATFirstSector + (I + 1) div oleBlockIDPerBigBlock
    else
    begin
      FDIFAT[I] := ASector;
      Inc(ASector);
    end;
  if FMiniFATCount > 0 then
    FMiniFATFirstSector := ASector;
  Inc(ASector, FMiniFATCount);
  // Fill FAT
  AllocateSectors(FMiniFAT, FMiniFATCount);
  AllocateSectors(FFAT, FFATCount);
  FillIndexes(FDirEntryFirstSector, FDirEntryCount, False); // FAT for directory
  FillInteger(FFAT[FDIFATFirstSector], FDIFATCount, oleDIFATSECTOR);
  FillIndexes(FMiniFATFirstSector, FMiniFATCount, False); // FAT for miniFAT
  FillInteger(FFAT[FDirEntryCount + FDIFATCount], FFATCount, oleFATSector);
  AIndexInFAT[False] := ASector;
  AIndexInFAT[True] := 0;
  for I := 0 to Count - 1 do
    FillFAT(Items[I], AIndexInFAT[Items[I].SmallStream]);
end;

procedure TdxOLEDocument.ReadData(AStream: TStream);
var
  ASignature: Int64;
begin
  // read header
  AStream.ReadBuffer(ASignature, SizeOf(ASignature));
  Check(ASignature = oleSignature);
  Check(AStream.Read(FCLSID, SizeOf(TGUID)) = SizeOf(TGUID));
  AStream.ReadBuffer(FVersion, SizeOf(Integer));
  AStream.ReadBuffer(FByteOrder, SizeOf(Word));
  AStream.ReadBuffer(FSectorShift, SizeOf(Word));
  AStream.ReadBuffer(FMiniSectorShift, SizeOf(Word));
  Check((FMiniSectorShift > 0) and (FSectorShift > FMiniSectorShift));
  AStream.Position := AStream.Position + SizeOf(Word) + SizeOf(Integer) +
    SizeOf(Integer); // skip reserved
  AStream.ReadBuffer(FFATCount, SizeOf(Integer));
  AStream.ReadBuffer(FDirEntryFirstSector, SizeOf(Integer));
  AStream.ReadBuffer(FTransaction, SizeOf(Integer));
  AStream.ReadBuffer(FMiniSectorCutoffSize, SizeOf(Integer));
  AStream.ReadBuffer(FMiniFATFirstSector, SizeOf(Integer));
  AStream.ReadBuffer(FMiniFATCount, SizeOf(Integer));
  AStream.ReadBuffer(FDIFATFirstSector, SizeOf(Integer));
  AStream.ReadBuffer(FDIFATCount, SizeOf(Integer));
  Check(AStream.Read(FFATInHeader, SizeOf(FFATInHeader))
    = SizeOf(FFATInHeader));
  //
  ReadDirectory(AStream);
  InitializeItemsLinks;
end;

procedure TdxOLEDocument.ReadDirectory(AStream: TStream);
var
  AList: TList;
  AEntry: TdxOLEDocumentDirectoryEntry;
  AStreamReader: TOLEDocumentStreamReader;
begin
  AList := CreateSectorsChain(FDirEntryFirstSector);
  TreeLocked := True;
  AStreamReader := TOLEDocumentStreamReader.Create(Stream, AList,
    AList.Count shl FSectorShift, 1 shl FSectorShift);
  try
    AStreamReader.Position := 0;
    while AStreamReader.Position < AStreamReader.Size do
    begin
      AEntry := CreateDirEntry('');
      AEntry.ReadFromStream(AStreamReader);
      if AEntry.EntryType = ET_ROOT then
        FRootEntry := AEntry;
    end;
  finally
    AStreamReader.Free;
    TreeLocked := False;
  end;
end;

procedure TdxOLEDocument.ReadFATSector(ASectorID: Integer;
  var FATSector: TdxOLEDocumentSector);
var
  I, ADIFStart: Integer;
  ADIFSector: TdxOLEDocumentSector;
const
  DIFSectorPos = oleMaxBlockIDInBigBlock * SizeOf(Integer);
begin
  if ASectorID >= oleSectorsInMasterFAT then
  begin
    ASectorID := ASectorID - oleSectorsInMasterFAT;
    ADIFStart := FDIFATFirstSector;
    for I := 0 to ASectorID div oleMaxBlockIDInBigBlock - 1 do
    begin
      Stream.Position := (ADIFStart + 1) shl oleBigBlockShift + DIFSectorPos;
      Stream.ReadBuffer(ADIFStart, SizeOf(ADIFStart));
    end;
    GetSector(ADIFStart, ADIFSector);
    ASectorID := ADIFSector[ASectorID mod oleMaxBlockIDInBigBlock];
  end
  else
    ASectorID := FFATInHeader[ASectorID];
  GetSector(ASectorID, FATSector);
end;

procedure TdxOLEDocument.WriteData(AStream: TStream);
var
  I: Integer;
  ASignature: Int64;
begin
  // Header
  ASignature := oleSignature;
  AStream.WriteBuffer(ASignature, SizeOf(Int64));
  Check(AStream.Write(FCLSID, SizeOf(TGUID)) = SizeOf(TGUID));
  WriteInteger(AStream, FVersion);
  AStream.WriteBuffer(FByteOrder, SizeOf(Word));
  AStream.WriteBuffer(FSectorShift, SizeOf(Word));
  AStream.WriteBuffer(FMiniSectorShift, SizeOf(Word));
  I := 0;
  AStream.Write(I, SizeOf(Word)); // todo: reserved;
  WriteInteger(AStream, 0); // todo: reserved;
  WriteInteger(AStream, 0); // todo: reserved;
  WriteInteger(AStream, FFATCount);
  WriteInteger(AStream, FDirEntryFirstSector);
  WriteInteger(AStream, FTransaction);
  WriteInteger(AStream, FMiniSectorCutoffSize);
  WriteInteger(AStream, FMiniFATFirstSector);
  WriteInteger(AStream, FMiniFATCount);
  WriteInteger(AStream, FDIFATFirstSector);
  WriteInteger(AStream, FDIFATCount);
  Check(AStream.Write(FFATInHeader, SizeOf(FFATInHeader))
    = SizeOf(FFATInHeader));
  // DirEntry, DIFAT, FAT, MiniFAT, Data
  for I := 0 to Count - 1 do
    Items[I].WriteHeaderToStream(AStream);
  AlignStream(AStream, oleBigBlockSize);
  if FDIFATCount > 0 then
    AStream.WriteBuffer(FDIFAT[0], FDIFATCount * oleBigBlockSize);
  Stream.WriteBuffer(FFAT[0], FFATCount * oleBigBlockSize);
  if FMiniFATCount > 0 then
    AStream.WriteBuffer(FMiniFAT[0], FMiniFATCount * oleBigBlockSize);
  for I := 0 to Count - 1 do
    Items[I].WriteDataToStream(AStream);
end;

procedure TdxOLEDocument.FillIndexes(AStartIndex, ACount: Integer;
  AMiniFAT: Boolean);
var
  FAT: PIntegerArray;
begin
  if ACount <= 0 then
    Exit;
  FAT := @FFAT[0];
  if AMiniFAT then
    FAT := @FMiniFAT[0];
  repeat
    Dec(ACount);
    if ACount = 0 then
      FAT^[AStartIndex] := oleEndOfChain
    else
      FAT^[AStartIndex] := AStartIndex + 1;
    Inc(AStartIndex);
  until ACount = 0;
end;

procedure TdxOLEDocument.FillFAT(AEntry: TdxOLEDocumentDirectoryEntry;
  var AStartIndex: Integer);
var
  ACount: Integer;
begin
  if (AEntry.EntryType = ET_ROOT) and (AEntry.Size = 0) then
  begin
    AEntry.FSectorID := -2;
    Exit;
  end;
  AEntry.FSectorID := 0;
  if AEntry.Size <= 0 then
    Exit;
  AEntry.FSectorID := AStartIndex;
  ACount := RoundDiv(AEntry.Size, AEntry.SectorSize);
  if ACount >= 1 then
    FillIndexes(AStartIndex, ACount, AEntry.SmallStream);
  Inc(AStartIndex, ACount);
end;

function TdxOLEDocument.GetActuallySourceStream(ASmallStream: Boolean): TStream;
begin
  Result := Stream;
  if ASmallStream then
    Result := Items[0].Data;
end;

function TdxOLEDocument.GetCount: Integer;
begin
  Result := FDirEntries.Count;
end;

function TdxOLEDocument.GetItem(AIndex: Integer): TdxOLEDocumentDirectoryEntry;
begin
  Result := FDirEntries[AIndex] as TdxOLEDocumentDirectoryEntry;
end;

function TdxOLEDocument.GetRoot: TdxOLEDocumentDirectoryEntry;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[0];
end;

function TdxOLEDocument.GetSectorSize(ASmallStream: Boolean): Integer;
begin
  if ASmallStream then
    Result := 1 shl FMiniSectorShift
  else
    Result := 1 shl FSectorShift;
end;

{ TdxOLEDocumentProperty }

constructor TdxOLEDocumentProperty.Create(ID: Cardinal);
begin
  FID := ID;
end;

{ TdxOLEDocumentPropertySetSection }

constructor TdxOLEDocumentPropertySetSection.Create(const AClassID: TGUID);
begin
  inherited Create;
  FClassID := AClassID;
end;

procedure TdxOLEDocumentPropertySetSection.Add(ID: Cardinal;
  const AValue: Variant);
var
  AItem: TdxOLEDocumentProperty;
begin
  if not Find(ID, AItem) then
  begin
    AItem := TdxOLEDocumentProperty.Create(ID);
    inherited Add(AItem);
  end;
  AItem.Value := AValue;
end;

function TdxOLEDocumentPropertySetSection.Find(ID: Cardinal;
  out AItem: TdxOLEDocumentProperty): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ID = ID then
    begin
      AItem := Items[I];
      Exit(True);
    end;

  Result := False;
end;

function TdxOLEDocumentPropertySetSection.Get(ID: Cardinal;
  const ADefaultValue: Variant): Variant;
var
  AProperty: TdxOLEDocumentProperty;
begin
  if Find(ID, AProperty) then
  begin
    Result := AProperty.Value;
    if VarIsNull(Result) or VarIsEmpty(Result) then
      Result := ADefaultValue;
  end
  else
    Result := ADefaultValue;
end;

procedure TdxOLEDocumentPropertySetSection.Remove(ID: Cardinal);
var
  AItem: TdxOLEDocumentProperty;
begin
  if Find(ID, AItem) then
    inherited Remove(AItem);
end;

procedure TdxOLEDocumentPropertySetSection.ReadData(AStream: TStream);
var
  AOffset: Int64;
  APropertyCount: Integer;
  APropertyEntries: TPropertyEntries;
  I: Integer;
begin
  Clear;
  AOffset := AStream.Position;
  ReadCardinalFunc(AStream); // Section size
  APropertyCount := ReadCardinalFunc(AStream);

  SetLength(APropertyEntries, APropertyCount);
  for I := 0 to APropertyCount - 1 do
  begin
    APropertyEntries[I].ID := ReadCardinalFunc(AStream);
    APropertyEntries[I].Offset := ReadCardinalFunc(AStream);
  end;

  Capacity := APropertyCount;
  for I := 0 to APropertyCount - 1 do
  begin
    AStream.Position := AOffset + APropertyEntries[I].Offset;
    Add(APropertyEntries[I].ID, ReadVariant(AStream));
  end;
end;

procedure TdxOLEDocumentPropertySetSection.WriteData(AStream: TStream);

  procedure WriteEntries(AEntries: TPropertyEntries);
  var
    I: Integer;
  begin
    for I := 0 to Length(AEntries) - 1 do
    begin
      WriteCardinalProc(AStream, AEntries[I].ID);
      WriteCardinalProc(AStream, AEntries[I].Offset);
    end;
  end;

var
  ABaseOffset: Int64;
  APrevOffset: Int64;
  AProperty: TdxOLEDocumentProperty;
  APropertyEntries: TPropertyEntries;
  ATableOffset: Int64;
  I: Integer;
begin
  SetLength(APropertyEntries, Count);

  ABaseOffset := AStream.Position;
  WriteCardinalProc(AStream, 0); // Section size
  WriteCardinalProc(AStream, Count);

  ATableOffset := AStream.Position;
  WriteEntries(APropertyEntries);

  for I := 0 to Count - 1 do
  begin
    AProperty := Items[I];
    APropertyEntries[I].ID := AProperty.ID;
    APropertyEntries[I].Offset := AStream.Position - ABaseOffset;
    WriteVariant(AStream, AProperty.ID, AProperty.Value);
  end;

  APrevOffset := AStream.Position;
  AStream.Position := ABaseOffset;
  WriteCardinalProc(AStream, APrevOffset - ABaseOffset);
  AStream.Position := ATableOffset;
  WriteEntries(APropertyEntries);
  AStream.Position := APrevOffset;
end;

function TdxOLEDocumentPropertySetSection.ReadFileTime
  (AStream: TStream): Variant;
var
  ASysTime: TSystemTime;
  ATime: TFileTime;
begin
  AStream.ReadBuffer(ATime, SizeOf(ATime));
  FileTimeToSystemTime(ATime, ASysTime);
  Result := SystemTimeToDateTime(ASysTime);
end;

function TdxOLEDocumentPropertySetSection.CreateEncoding(ACodePage: Word)
  : TEncoding;
begin
  if ACodePage = CP_WINUNICODE then
    Result := TUnicodeEncoding.Create
  else
    Result := TEncoding.GetEncoding(ACodePage);
end;

function TdxOLEDocumentPropertySetSection.GetItem(Index: Integer)
  : TdxOLEDocumentProperty;
begin
  Result := TdxOLEDocumentProperty(List[Index]);
end;

function TdxOLEDocumentPropertySetSection.ReadCodePageString
  (AStream: TStream): Variant;
var
  ABytes: TBytes;
  ACodePage: Word;
  AProperty: TdxOLEDocumentProperty;
  ASize: Cardinal;
begin
  if Find(PID_CODEPAGE, AProperty) then
    ACodePage := AProperty.Value
  else
    ACodePage := CP_ACP;

  ASize := ReadCardinalFunc(AStream);
  if ACodePage = CP_WINUNICODE then
    ASize := ASize * 2;

  SetLength(ABytes, ASize);
  if ASize > 0 then
{$IFDEF DELPHIXE3}
    AStream.ReadBuffer(ABytes, ASize);
{$ELSE}
    AStream.ReadBuffer(ABytes[0], ASize);
{$ENDIF}
  with CreateEncoding(ACodePage) do
    try
      Result := GetString(ABytes);
    finally
      Free;
    end;

  Result := TrimRight(Result);
end;

function TdxOLEDocumentPropertySetSection.ReadVariant(AStream: TStream)
  : Variant;
var
  AType: Cardinal;
begin
  AType := ReadCardinalFunc(AStream);
  case AType of
    VT_EMPTY:
      Result := Empty;
    VT_NULL:
      Result := Null;
    VT_I1:
      Result := ReadByteFunc(AStream);
    VT_I2:
      Result := ReadSmallIntFunc(AStream);
    VT_I4:
      Result := ReadIntegerFunc(AStream);
    VT_I8:
      Result := ReadLargeIntFunc(AStream);
    VT_R4:
      Result := ReadSingleFunc(AStream);
    VT_R8:
      Result := ReadDoubleFunc(AStream);
    VT_BSTR, VT_LPSTR:
      Result := ReadCodePageString(AStream);
    VT_FILETIME:
      Result := ReadFileTime(AStream);
  else
    Result := Null;
  end;
end;

procedure TdxOLEDocumentPropertySetSection.WriteCodePageString(AStream: TStream;
  const AValue: string);
var
  AByteCount: Cardinal;
  ABytes: TBytes;
  ACodePage: Word;
  APaddingSize: Cardinal;
  AProperty: TdxOLEDocumentProperty;
begin

  if Find(PID_CODEPAGE, AProperty) then
    ACodePage := AProperty.Value
  else
    ACodePage := CP_ACP;

  with CreateEncoding(ACodePage) do
    try
      ABytes := GetBytes(AValue);
    finally
      Free;
    end;

  AByteCount := Length(ABytes);
  if AByteCount > 0 then
  begin
    APaddingSize := AByteCount mod 4;
    if APaddingSize = 0 then
      APaddingSize := 4;

    if ACodePage = CP_WINUNICODE then
      WriteCardinalProc(AStream, (AByteCount + APaddingSize) div 2)
    else
      WriteCardinalProc(AStream, (AByteCount + APaddingSize));

{$IFDEF DELPHIXE3}
    AStream.WriteBuffer(ABytes, AByteCount);
{$ELSE}
    AStream.WriteBuffer(ABytes[0], AByteCount);
{$ENDIF}
    while APaddingSize > 0 do
    begin
      WriteByteProc(AStream, 0);
      Dec(APaddingSize);
    end;
  end
  else
    WriteCardinalProc(AStream, 0);
end;

procedure TdxOLEDocumentPropertySetSection.WriteFileTime(AStream: TStream;
  const AValue: TDateTime);
var
  AFileTime: TFileTime;
  ASystemTime: TSystemTime;
begin
  DateTimeToSystemTime(AValue, ASystemTime);
  SystemTimeToFileTime(ASystemTime, AFileTime);
  AStream.WriteBuffer(AFileTime, SizeOf(AFileTime));
end;

procedure TdxOLEDocumentPropertySetSection.WriteVariant(AStream: TStream;
  ID: Cardinal; const AValue: Variant);
begin
  case ID of
    // VT_I2
    PID_CODEPAGE:
      begin
        WriteCardinalProc(AStream, VT_I2);
        WriteSmallIntProc(AStream, AValue);
      end;

    // VT_LPSTR
    PID_SUBJECT, PID_TITLE, PID_AUTHOR, PID_KEYWORDS, PID_COMMENTS,
      PID_TEMPLATE, PID_LASTAUTHOR, PID_REVNUMBER, PID_APPNAME:
      begin
        WriteCardinalProc(AStream, VT_LPSTR);
        WriteCodePageString(AStream, AValue);
      end;

    // VT_FILETIME
    PID_CREATE_DTM, PID_EDITTIME, PID_LASTPRINTED, PID_LASTSAVE_DTM:
      begin
        WriteCardinalProc(AStream, VT_FILETIME);
        WriteFileTime(AStream, AValue);
      end;

    // VT_I4
    PID_SECURITY, PID_PAGECOUNT, PID_WORDCOUNT, PID_CHARCOUNT:
      begin
        WriteCardinalProc(AStream, VT_I4);
        WriteIntegerProc(AStream, AValue);
      end;

  else
    raise EdxOLEDocument.CreateFmt(sdxErrorUnsupportedProperty, [ID]);
  end;
end;

{ TdxOLEDocumentPropertySet }

constructor TdxOLEDocumentPropertySet.Create;
begin
  FSections := TdxFastObjectList.Create;
end;

destructor TdxOLEDocumentPropertySet.Destroy;
begin
  FreeAndNil(FSections);
  inherited;
end;

function TdxOLEDocumentPropertySet.Add(const AClassID: TGUID)
  : TdxOLEDocumentPropertySetSection;
begin
  Result := TdxOLEDocumentPropertySetSection.Create(AClassID);
  FSections.Add(Result);
end;

procedure TdxOLEDocumentPropertySet.Clear;
begin
  FSections.Clear;
end;

function TdxOLEDocumentPropertySet.Find(const AClassID: TGUID;
  out ASection: TdxOLEDocumentPropertySetSection): Boolean;
var
  I: Integer;
begin
  for I := 0 to SectionCount - 1 do
    if IsEqualGUID(Sections[I].ClassID, AClassID) then
    begin
      ASection := Sections[I];
      Exit(True);
    end;

  Result := False;
end;

procedure TdxOLEDocumentPropertySet.LoadFromStream(AStream: TStream);
var
  ASection: TdxOLEDocumentPropertySetSection;
  ASectionCount: Integer;
  ASectionTableOffsets: array of Cardinal;
  ASectionTableKeys: array of TGUID;
  I: Integer;
begin
  Clear;

  if ReadWordFunc(AStream) <> $FFFE then // ByteOrder
    raise EdxOLEDocument.Create(sdxErrorInvalidOLEPropertySet);
  if ReadWordFunc(AStream) <> 0 then // Format
    raise EdxOLEDocument.Create(sdxErrorInvalidOLEPropertySet);

  ReadIntegerFunc(AStream); // OSVersion
  AStream.ReadBuffer(FClassID, SizeOf(FClassID));
  ASectionCount := ReadIntegerFunc(AStream);
  if ASectionCount < 0 then
    raise EdxOLEDocument.Create(sdxErrorInvalidOLEPropertySet);

  SetLength(ASectionTableKeys, ASectionCount);
  SetLength(ASectionTableOffsets, ASectionCount);
  for I := 0 to ASectionCount - 1 do
  begin
    AStream.ReadBuffer(ASectionTableKeys[I], SizeOf(TGUID));
    ASectionTableOffsets[I] := ReadCardinalFunc(AStream);
  end;

  for I := 0 to ASectionCount - 1 do
  begin
    AStream.Position := ASectionTableOffsets[I];
    ASection := Add(ASectionTableKeys[I]);
    ASection.ReadData(AStream);
  end;
end;

procedure TdxOLEDocumentPropertySet.SaveToStream(AStream: TStream);
var
  APrevPosition: Int64;
  ASection: TdxOLEDocumentPropertySetSection;
  ASectionTableOffset: Int64;
  ASectionTableOffsets: array of Cardinal;
  ASectionTableKeys: array of TGUID;
  I: Integer;
begin
  WriteWordProc(AStream, $FFFE);
  WriteWordProc(AStream, 0);

  WriteIntegerProc(AStream, (OS_WIN32 shl 16) or $0A04);
  AStream.WriteBuffer(FClassID, SizeOf(FClassID));
  WriteIntegerProc(AStream, SectionCount);

  SetLength(ASectionTableKeys, SectionCount);
  SetLength(ASectionTableOffsets, SectionCount);
  ASectionTableOffset := AStream.Position;
  for I := 0 to SectionCount - 1 do
  begin
    AStream.WriteBuffer(ASectionTableKeys[I], SizeOf(TGUID));
    AStream.WriteBuffer(ASectionTableOffsets[I], SizeOf(Cardinal));
  end;

  for I := 0 to SectionCount - 1 do
  begin
    ASection := Sections[I];
    ASectionTableKeys[I] := ASection.ClassID;
    ASectionTableOffsets[I] := AStream.Position;
    ASection.WriteData(AStream);
  end;

  APrevPosition := AStream.Position;
  AStream.Position := ASectionTableOffset;
  for I := 0 to SectionCount - 1 do
  begin
    AStream.WriteBuffer(ASectionTableKeys[I], SizeOf(TGUID));
    AStream.WriteBuffer(ASectionTableOffsets[I], SizeOf(Cardinal));
  end;
  AStream.Position := APrevPosition;
end;

function TdxOLEDocumentPropertySet.GetSection(Index: Integer)
  : TdxOLEDocumentPropertySetSection;
begin
  Result := TdxOLEDocumentPropertySetSection(FSections.List[Index]);
end;

function TdxOLEDocumentPropertySet.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

end.
