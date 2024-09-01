{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressPDFViewer }
{ }
{ Copyright (c) 2015-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL }
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

unit dxPDFBase;

{$I cxVer.inc}
{$IFNDEF DELPHIXE3}
{$M-}
{$ENDIF}

interface

uses
  Types, SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections,
  cxVariants, dxCore, dxCoreClasses,
  dxGenerics;

const
  dxPDFInvalidValue = -MaxInt; // for internal use

type
  TdxPDFBase = class;
  TdxPDFWriter = class;

  TdxPDFBaseType = (otArray, otBoolean, otDouble, otCommand, otCommandName,
    otDictionary, otIndirectReference, otIndirectObject, otInteger, otName,
    otNull, otStream, otString, otComment, otStreamElement, otObjectStream);
  // for internal use

  TdxPDFObjectIndex = class(TDictionary<Integer, TObject>);
  TdxPDFByteStringDictionary = class(TDictionary<Byte, string>);
  TdxPDFIntegerIntegerDictionary = class(TDictionary<Integer, Integer>);
  TdxPDFIntegerDoubleDictionary = class(TDictionary<Integer, Double>);
  TdxPDFIntegerList = class(TdxIntegerList);
  TdxPDFIntegerStringDictionary = class(TDictionary<Integer, string>);
  TdxPDFStringIntegerDictionary = class(TDictionary<string, Integer>);
  TdxPDFStringSmallIntegerDictionary = class(TDictionary<string, SmallInt>);
  TdxPDFStringStringDictionary = class(TDictionary<string, string>);
  TdxPDFWordDictionary = class(TDictionary<string, Word>);

  { IdxPDFEncryptionInfo }

  IdxPDFEncryptionInfo = interface // for internal use
    ['{DDECCF86-0499-45F3-9ED8-ED81624CE7EA}']
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
    function Encrypt(const AData: TBytes; ANumber: Integer): TBytes;
    function EncryptMetadata: Boolean;
  end;

  { TdxPDFSortedIntegerStringDictionary }

  TdxPDFSortedIntegerStringDictionary = class(TdxPDFIntegerStringDictionary)
  // for internal use
  public type
{$REGION 'Types'}
    TSortedKeys = class(TEnumerable<Integer>)
    strict private
      FDictionary: TdxPDFIntegerStringDictionary;
    public
      constructor Create(ADictionary: TdxPDFIntegerStringDictionary);
      function DoGetEnumerator: TEnumerator<Integer>; override;
    end;

    TSortedKeysEnumerator = class(TEnumerator<Integer>)
    strict private
      FIndex: Integer;
      FKeys: TIntegerDynArray;
    protected
      function DoGetCurrent: Integer; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(ADictionary: TdxPDFIntegerStringDictionary);
    end;
{$ENDREGION}
  strict private
    FSortedKeys: TSortedKeys;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    //
    property SortedKeys: TSortedKeys read FSortedKeys;
  end;

  { TdxPDFSmallIntegerDictionary }

  TdxPDFSmallIntegerDictionary = class(TDictionary<SmallInt, SmallInt>)
  // for internal use
  public
    procedure Assign(ASource: TdxPDFSmallIntegerDictionary);
  end;

  { IdxPDFDocumentSharedObjectListener }

  IdxPDFDocumentSharedObjectListener = interface // for internal use
    ['{430AD1E4-E008-46D1-B211-A00F56023C8E}']
    procedure DestroyHandler(Sender: TdxPDFBase);
  end;

  { TdxPDFReferencedObject }

  TdxPDFReferencedObject = class(TObject)
  strict private
    FReferenceCount: Integer;
  strict protected
    property ReferenceCount: Integer read FReferenceCount;
  protected
    procedure ResetReferenceCount;
  public
    procedure Reference; // for internal use
    procedure Release; // for internal use
  end;

  { TdxPDFBase }

  TdxPDFBase = class(TdxPDFReferencedObject)
  strict private
    FGeneration: Integer;
    FNumber: Integer;
  protected
    class function GetObjectType: TdxPDFBaseType; virtual; // for internal use
    procedure Write(AWriter: TdxPDFWriter); virtual; // for internal use
  public
    constructor Create; overload; virtual; // for internal use
    constructor Create(ANumber, AGeneration: Integer); overload; virtual;
    // for internal use

    property Generation: Integer read FGeneration write FGeneration;
    // for internal use
    property Number: Integer read FNumber write FNumber; // for internal use
    property ObjectType: TdxPDFBaseType read GetObjectType; // for internal use
  end;

  { TdxPDFFactory<T> }

  TdxPDFFactory<T> = class // for internal use
  strict private
    FClasses: TDictionary<string, T>;
  protected
    function ContainsKey(const AKey: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetClass(const AKey: string; out AClass: T): Boolean;
    procedure Register(const AKey: string; AClass: T);
    procedure UnregisterClass(const AKey: string);
  end;

  { TdxPDFObjectList<T> }

  TdxPDFObjectList<T: TdxPDFReferencedObject> = class(TList<T>)
  protected
    procedure Notify(const Item: T; Action: TCollectionNotification); override;
  end;

  TdxPDFReferencedObjects = class(TdxPDFObjectList<TdxPDFReferencedObject>);
  // for internal use

  { TdxPDFBaseReferences }

  TdxPDFBaseReferences = class // for internal use
  strict private
    FItems: TDictionary<Integer, TdxPDFReferencedObject>;
    FMaxKey: Integer;

    function GetCount: Integer;
    function GetKeys: TEnumerable<Integer>;
    function GetItem(AKey: Integer): TdxPDFReferencedObject;
  protected
    property Items: TDictionary<Integer, TdxPDFReferencedObject> read FItems;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(const AKey: Integer): Boolean;
    function TryGetValue(const AKey: Integer;
      out AValue: TdxPDFReferencedObject): Boolean;
    procedure Add(const AKey: Integer; AValue: TdxPDFReferencedObject);
    procedure Clear;
    procedure Remove(const AKey: Integer);
    procedure TrimExcess;

    property Count: Integer read GetCount;
    property Item[Key: Integer]: TdxPDFReferencedObject read GetItem; default;
    property Keys: TEnumerable<Integer> read GetKeys;
    property MaxKey: Integer read FMaxKey;
  end;

  { TdxPDFBaseList }

  TdxPDFBaseList = class(TdxPDFObjectList<TdxPDFBase>); // for internal use

  { TdxPDFUniqueReferences }

  TdxPDFUniqueReferences = class // for internal use
  strict private
    FLock: TRtlCriticalSection;
    FDictionary: TDictionary<string, TdxPDFBase>;
    FNumbers: TDictionary<Integer, TdxPDFBase>;
    FReferences: TdxPDFBaseList;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(const AID: string; ANumber: Integer): Boolean;
    function ContainsValue(AValue: TdxPDFBase): Boolean;
    function TryGetValue(AKey: Integer; out AValue: TdxPDFBase): Boolean;
    procedure Add(const AID: string; ANumber: Integer; AObject: TdxPDFBase);
    procedure Remove(const AID: string); overload;
    procedure Clear;
  end;

  TdxPDFStringReferencedObjectDictionary = class
    (TDictionary<string, TdxPDFReferencedObject>);

  { TdxPDFReferencedObjectDictionary }

  TdxPDFReferencedObjectDictionary = class // for internal use
  strict private
    FItems: TdxPDFStringReferencedObjectDictionary;
    function GetCount: Integer;
    procedure Changed(Sender: TObject; const Item: TdxPDFReferencedObject;
      Action: TCollectionNotification);
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(const AKey: string): Boolean;
    function TryGetValue(const AKey: string;
      out AValue: TdxPDFReferencedObject): Boolean;
    procedure Add(const AKey: string; AValue: TdxPDFReferencedObject);
    procedure AddOrSetValue(const AKey: string; AValue: TdxPDFReferencedObject);
    procedure Clear;
    procedure Remove(const AKey: string);
    procedure TrimExcess;

    property Count: Integer read GetCount;
    property Items: TdxPDFStringReferencedObjectDictionary read FItems;
  end;

  { TdxPDFCustomStream }

  TdxPDFCustomStream = class // for internal use
  strict private
    FFreeStream: Boolean;
    FReader: TcxReader;
    FWriter: TcxWriter;
    function GetPosition: Int64;
    function GetSize: Int64; inline;
    procedure SetPosition(const AValue: Int64);
  strict protected
    FStream: TStream;
  protected
    property Reader: TcxReader read FReader;
    property Writer: TcxWriter read FWriter;
  public
    constructor Create; overload; virtual;
    constructor Create(AStream: TStream; AFreeStream: Boolean = True);
      overload; virtual;
    destructor Destroy; override;

    function ReadArray(ALength: Integer): TBytes; inline;
    function ReadByte: SmallInt; inline;
    function ReadFixed: Single; inline;
    function ReadInt: Integer; inline;
    function ReadLong: Int64; inline;
    function ReadOffset(ALength: Integer): Integer; inline;
    function ReadShort: SmallInt;
    function ReadShortArray(ALength: Integer): TSmallIntDynArray; inline;
    function ReadString(ALength: Integer): string; inline;
    function ReadUShort: Word;
    function ToAlignedArray: TBytes;

    procedure WriteArray(const AArray: TBytes); overload;
    procedure WriteArray(const AArray: TBytes; ACount: Integer); overload;
    procedure WriteByte(AValue: Byte);
    procedure WriteEmptyArray(ALength: Integer);
    procedure WriteFixed(AValue: Single);
    procedure WriteDouble(AValue: Double);
    procedure WriteInt(AValue: Integer);
    procedure WriteLong(AValue: Int64);
    procedure WriteShort(AValue: SmallInt);
    procedure WriteShortArray(const AArray: TSmallIntDynArray);
    procedure WriteShortArrayEx(const AArray: TSmallIntDynArray);
    procedure WriteSpace;
    procedure WriteString(const AValue: string);

    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
  end;

  { TdxPDFDocumentCustomStream }

  TdxPDFDocumentCustomStream = class(TdxPDFCustomStream) // for internal use
  protected
    procedure WriteCloseBracket;
    procedure WriteHexadecimalString(const AValue: TBytes);
    procedure WriteOpenBracket;

    property Stream: TStream read FStream;
  end;

  { TdxPDFMemoryStream }

  TdxPDFMemoryStream = class(TdxPDFDocumentCustomStream) // for internal use
  strict private
    function GetData: TBytes; inline;
    function GetStream: TBytesStream;
  protected
    property Stream: TBytesStream read GetStream;
  public
    constructor Create; overload; override;
    constructor Create(const AData: TBytes); overload;
    destructor Destroy; override;
    property Data: TBytes read GetData;
  end;

  { TdxPDFWriter }

  TdxPDFWriter = class // for internal use
  strict private
    FCurrentObjectNumber: Integer;
    FEncryptionInfo: IdxPDFEncryptionInfo;
    FFreeStream: Boolean;
    FStream: TdxPDFDocumentCustomStream;
  protected
    function CreateEncryptionProvider: IdxPDFEncryptionInfo; virtual;
  public
    constructor Create(AStream: TStream; AFreeStream: Boolean); overload;
    constructor Create(AStream: TdxPDFDocumentCustomStream;
      AFreeStream: Boolean); overload;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function Encrypt(const ABytes: TBytes): TBytes;

    procedure BeginWriteObject(ANumber: Integer);
    procedure EndWriteObject;

    procedure WriteByte(AValue: Byte);
    procedure WriteBytes(const AValue: TBytes); overload;
    procedure WriteBytes(const AValue: TBytes; ACount: Integer); overload;
    procedure WriteCloseBracket;
    procedure WriteCloseDictionary;
    procedure WriteDouble(AValue: Double);
    procedure WriteName(const AValue: string);
    procedure WriteInteger(AValue: Integer);
    procedure WriteOpenBracket;
    procedure WriteOpenDictionary;
    procedure WriteSpace;

    procedure WriteLineFeed;
    procedure WriteHexadecimalString(const AValue: TBytes);
    procedure WriteString(const AValue: string; AWriteCrLf: Boolean = False);
    procedure WriteStringValue(const AValue: TBytes);

    property EncryptionInfo: IdxPDFEncryptionInfo read FEncryptionInfo;
    property Stream: TdxPDFDocumentCustomStream read FStream;
  end;

  { TdxPDFBaseStream }

  TdxPDFBaseStream = class abstract(TdxPDFBase)
  strict private
    FData: TBytes;
  protected
    procedure SetData(const AData: TBytes);
  public
    constructor Create(const AData: TBytes); virtual;
    destructor Destroy; override;

    property Data: TBytes read FData;
  end;

  { TdxPDFDefinedSymbols }

  TdxPDFDefinedSymbols = class
  public const
{$REGION 'public const'}
    Bell = 7;
    Backslash = 92;
    Backspace = 8;
    CarriageReturn = 13;
    Comment = Byte('%');
    DigitEnd = Byte('9');
    DigitStart = Byte('0');
    EndArray = Byte(']');
    EndObject = Byte('>');
    EndString = Byte(')');
    ExclamationMark = Byte('!');
    FormFeed = 12;
    HexDigitEnd = Byte('F');
    HexDigitStart = Byte('A');
    HorizontalTab = 9;
    Minus = Byte('-');
    LeftBracket = Byte('{');
    LineFeed = 10;
    LowercaseHexDigitEnd = Byte('f');
    LowercaseHexDigitStart = Byte('a');
    NameIdentifier = Byte('/');
    Null = 0;
    NumberSign = Byte('#');
    Period = Byte('.');
    Plus = Byte('+');
    RightBracket = Byte('}');
    Space = Byte(' ');
    StartArray = Byte('[');
    StartString = Byte('(');
    StartObject = Byte('<');
{$ENDREGION}
  end;

function dxPDFIsDoubleValid(AValue: Double): Boolean;
function dxPDFIsIntegerValid(AValue: Integer): Boolean;

function dxPDFFloatToStr(const AValue: Double): string;
function dxPDFRoundFloat(const AValue: Double): Double;

implementation

uses
  Math, StrUtils, dxTypeHelpers, dxStringHelper;

function dxPDFIsDoubleValid(AValue: Double): Boolean;
begin
  Result := not SameValue(AValue, dxPDFInvalidValue, 1);
end;

function dxPDFIsIntegerValid(AValue: Integer): Boolean;
begin
  Result := dxPDFIsDoubleValid(AValue);
end;

function dxPDFFloatToStr(const AValue: Double): string;
var
  AFormat: TFormatSettings;
begin
  FillChar(AFormat, SizeOf(AFormat), 0);
  AFormat.DecimalSeparator := '.';
  Result := FormatFloat('0.######', AValue, AFormat);
end;

function dxPDFRoundFloat(const AValue: Double): Double;
begin
  Result := RoundTo(AValue, -6);
end;

{ TdxPDFReferencedObject }

procedure TdxPDFReferencedObject.Reference;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TdxPDFReferencedObject.ResetReferenceCount;
begin
  InterlockedExchange(FReferenceCount, 0);
end;

procedure TdxPDFReferencedObject.Release;
begin
  if FReferenceCount <= 1 then
    Free
  else
    InterlockedDecrement(FReferenceCount);
end;

{ TdxPDFBase }

constructor TdxPDFBase.Create;
begin
  inherited Create;
  FGeneration := dxPDFInvalidValue;
  FNumber := dxPDFInvalidValue;
end;

constructor TdxPDFBase.Create(ANumber, AGeneration: Integer);
begin
  Create;
  FNumber := ANumber;
  FGeneration := AGeneration;
end;

class function TdxPDFBase.GetObjectType: TdxPDFBaseType;
begin
  Result := otNull;
end;

procedure TdxPDFBase.Write(AWriter: TdxPDFWriter);
begin
  // do nothing
end;

{ TdxPDFFactory<T> }

constructor TdxPDFFactory<T>.Create;
begin
  inherited Create;
  FClasses := TDictionary<string, T>.Create;
end;

destructor TdxPDFFactory<T>.Destroy;
begin
  FreeAndNil(FClasses);
  inherited Destroy;
end;

function TdxPDFFactory<T>.TryGetClass(const AKey: string;
  out AClass: T): Boolean;
begin
  Result := FClasses.TryGetValue(AKey, AClass);
end;

procedure TdxPDFFactory<T>.Register(const AKey: string; AClass: T);
begin
  if not ContainsKey(AKey) then
    FClasses.Add(AKey, AClass);
end;

procedure TdxPDFFactory<T>.UnregisterClass(const AKey: string);
begin
  if FClasses.ContainsKey(AKey) then
    FClasses.Remove(AKey);
end;

function TdxPDFFactory<T>.ContainsKey(const AKey: string): Boolean;
begin
  Result := FClasses.ContainsKey(AKey);
end;

{ TdxPDFObjectList }

procedure TdxPDFObjectList<T>.Notify(const Item: T;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded:
      Item.Reference;
    cnRemoved:
      Item.Release;
  end;
end;

{ TdxPDFBaseReferences }

constructor TdxPDFBaseReferences.Create;
begin
  inherited Create;
  FItems := TDictionary<Integer, TdxPDFReferencedObject>.Create;
  FMaxKey := -1;
end;

destructor TdxPDFBaseReferences.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxPDFBaseReferences.ContainsKey(const AKey: Integer): Boolean;
begin
  Result := FItems.ContainsKey(AKey);
end;

function TdxPDFBaseReferences.TryGetValue(const AKey: Integer;
  out AValue: TdxPDFReferencedObject): Boolean;
begin
  AValue := nil;
  Result := FItems.TryGetValue(AKey, AValue);
end;

procedure TdxPDFBaseReferences.Add(const AKey: Integer;
  AValue: TdxPDFReferencedObject);
begin
  FItems.Add(AKey, AValue);
  AValue.Reference;
  FMaxKey := Max(FMaxKey, AKey);
end;

procedure TdxPDFBaseReferences.Clear;
var
  AValue: TdxPDFReferencedObject;
begin
  for AValue in FItems.Values do
    AValue.Release;
  FItems.Clear;
end;

procedure TdxPDFBaseReferences.Remove(const AKey: Integer);
begin
  FItems.Items[AKey].Release;
  FItems.Remove(AKey);
end;

procedure TdxPDFBaseReferences.TrimExcess;
begin
  FItems.TrimExcess;
end;

function TdxPDFBaseReferences.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPDFBaseReferences.GetKeys: TEnumerable<Integer>;
begin
  Result := FItems.Keys;
end;

function TdxPDFBaseReferences.GetItem(AKey: Integer): TdxPDFReferencedObject;
begin
  if not FItems.TryGetValue(AKey, Result) then
    Result := nil;
end;

{ TdxPDFUniqueReferences }

constructor TdxPDFUniqueReferences.Create;
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FDictionary := TDictionary<string, TdxPDFBase>.Create;
  FNumbers := TDictionary<Integer, TdxPDFBase>.Create;
  FReferences := TdxPDFBaseList.Create;
end;

destructor TdxPDFUniqueReferences.Destroy;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FNumbers);
  FreeAndNil(FReferences);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxPDFUniqueReferences.ContainsKey(const AID: string;
  ANumber: Integer): Boolean;
begin
  Result := FDictionary.ContainsKey(AID) or FNumbers.ContainsKey(ANumber);
end;

function TdxPDFUniqueReferences.ContainsValue(AValue: TdxPDFBase): Boolean;
begin
  Result := FDictionary.ContainsValue(AValue);
end;

function TdxPDFUniqueReferences.TryGetValue(AKey: Integer;
  out AValue: TdxPDFBase): Boolean;
begin
  Result := FNumbers.TryGetValue(AKey, AValue);
end;

procedure TdxPDFUniqueReferences.Add(const AID: string; ANumber: Integer;
  AObject: TdxPDFBase);
begin
  EnterCriticalSection(FLock);
  try
    if not FDictionary.ContainsKey(AID) then
    begin
      FDictionary.Add(AID, AObject);
      if dxPDFIsIntegerValid(ANumber) then
        FNumbers.AddOrSetValue(ANumber, AObject);
      FReferences.Add(AObject);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxPDFUniqueReferences.Remove(const AID: string);
var
  AObject: TdxPDFBase;
begin
  if FDictionary.TryGetValue(AID, AObject) then
  begin
    FNumbers.Remove(AObject.Number);
    FDictionary.Remove(AID);
    FReferences.Remove(AObject)
  end;
end;

procedure TdxPDFUniqueReferences.Clear;
begin
  FDictionary.Clear;
  FNumbers.Clear;
  FReferences.Clear;
end;

{ TdxPDFReferencedObjectDictionary }

constructor TdxPDFReferencedObjectDictionary.Create;
begin
  inherited Create;
  FItems := TdxPDFStringReferencedObjectDictionary.Create;
  FItems.OnValueNotify := Changed;
end;

destructor TdxPDFReferencedObjectDictionary.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxPDFReferencedObjectDictionary.ContainsKey
  (const AKey: string): Boolean;
begin
  Result := FItems.ContainsKey(AKey);
end;

function TdxPDFReferencedObjectDictionary.TryGetValue(const AKey: string;
  out AValue: TdxPDFReferencedObject): Boolean;
begin
  Result := FItems.TryGetValue(AKey, AValue);
end;

procedure TdxPDFReferencedObjectDictionary.Add(const AKey: string;
  AValue: TdxPDFReferencedObject);
begin
  FItems.Add(AKey, AValue);
end;

procedure TdxPDFReferencedObjectDictionary.AddOrSetValue(const AKey: string;
  AValue: TdxPDFReferencedObject);
begin
  FItems.AddOrSetValue(AKey, AValue);
end;

procedure TdxPDFReferencedObjectDictionary.Clear;
begin
  FItems.Clear;
end;

procedure TdxPDFReferencedObjectDictionary.Remove(const AKey: string);
begin
  FItems.Remove(AKey);
end;

procedure TdxPDFReferencedObjectDictionary.TrimExcess;
begin
  FItems.TrimExcess;
end;

function TdxPDFReferencedObjectDictionary.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TdxPDFReferencedObjectDictionary.Changed(Sender: TObject;
  const Item: TdxPDFReferencedObject; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Item.Reference;
    cnRemoved:
      Item.Release;
  end;
end;

{ TdxPDFCustomStream }

constructor TdxPDFCustomStream.Create;
begin
  inherited Create;
  FFreeStream := True;
end;

constructor TdxPDFCustomStream.Create(AStream: TStream;
  AFreeStream: Boolean = True);
begin
  inherited Create;
  FFreeStream := AFreeStream;
  FStream := AStream;
  FReader := TcxReader.Create(FStream);
  FWriter := TcxWriter.Create(FStream);
end;

destructor TdxPDFCustomStream.Destroy;
begin
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  if FFreeStream then
    FreeAndNil(FStream);
  inherited Destroy;
end;

function TdxPDFCustomStream.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

function TdxPDFCustomStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

procedure TdxPDFCustomStream.SetPosition(const AValue: Int64);
begin
  FStream.Position := AValue;
end;

function TdxPDFCustomStream.ReadByte: SmallInt;
begin
  if Reader.Stream.Position < Size then
    Result := Reader.ReadByte
  else
    Result := -1;
end;

function TdxPDFCustomStream.ReadShort: SmallInt;
begin
  Result := ReadByte shl 8 + ReadByte;
end;

function TdxPDFCustomStream.ReadUShort: Word;
begin
  Result := ReadShort;
end;

function TdxPDFCustomStream.ToAlignedArray: TBytes;
var
  ALength, AFactor: Integer;
begin
  FStream.Position := 0;
  ALength := FStream.Size;
  AFactor := ALength mod 4;
  SetLength(Result, ALength + IfThen(AFactor > 0, 4 - AFactor, 0));
  FStream.Read(Result[0], ALength);
end;

function TdxPDFCustomStream.ReadInt: Integer;
begin
  Result := (ReadByte shl 24) + (ReadByte shl 16) + (ReadByte shl 8) + ReadByte;
end;

function TdxPDFCustomStream.ReadLong: Int64;
begin
  Result := Reader.ReadLargeInt;
end;

function TdxPDFCustomStream.ReadOffset(ALength: Integer): Integer;
begin
  case ALength of
    2:
      Result := ReadUShort;
    3:
      Result := (ReadByte shl 16) + (ReadByte shl 8) + ReadByte;
    4:
      Result := ReadInt;
  else
    Result := ReadByte;
  end;
end;

function TdxPDFCustomStream.ReadArray(ALength: Integer): TBytes;
var
  AArray: TBytes;
begin
  SetLength(AArray, ALength);
  FStream.Read(AArray[0], ALength);
  Result := AArray;
end;

function TdxPDFCustomStream.ReadShortArray(ALength: Integer): TSmallIntDynArray;
var
  I: Integer;
begin
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := ReadShort;
end;

function TdxPDFCustomStream.ReadString(ALength: Integer): string;
var
  ABuilder: TStringBuilder;
  I: Integer;
begin
  ABuilder := TdxStringBuilderManager.Get(ALength);
  try
    for I := 0 to ALength - 1 do
      ABuilder.Append(Char(ReadByte));
    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

function TdxPDFCustomStream.ReadFixed: Single;
begin
  Result := ReadInt / 65536;
end;

procedure TdxPDFCustomStream.WriteFixed(AValue: Single);
begin
  WriteInt(Trunc(AValue * 65536));
end;

procedure TdxPDFCustomStream.WriteDouble(AValue: Double);
begin
  WriteString(dxPDFFloatToStr(AValue));
end;

procedure TdxPDFCustomStream.WriteByte(AValue: Byte);
begin
  Writer.WriteByte(AValue);
end;

procedure TdxPDFCustomStream.WriteShort(AValue: SmallInt);
begin
  WriteByte((AValue and $FF00) shr 8);
  WriteByte(AValue and $FF);
end;

procedure TdxPDFCustomStream.WriteInt(AValue: Integer);
begin
  WriteByte((AValue and $FF000000) shr 24);
  WriteByte((AValue and $FF0000) shr 16);
  WriteByte((AValue and $FF00) shr 8);
  WriteByte(AValue and $FF);
end;

procedure TdxPDFCustomStream.WriteLong(AValue: Int64);
begin
  WriteByte((AValue and $FF00000000000000) shr 56);
  WriteByte((AValue and $FF000000000000) shr 48);
  WriteByte((AValue and $FF00000000) shr 32);
  WriteByte((AValue and $FF000000) shr 24);
  WriteByte((AValue and $FF0000) shr 16);
  WriteByte((AValue and $FF0000) shr 16);
  WriteByte((AValue and $FF00) shr 8);
  WriteByte(AValue and $FF);
end;

procedure TdxPDFCustomStream.WriteEmptyArray(ALength: Integer);
var
  I: Integer;
begin
  for I := 0 to ALength - 1 do
    WriteByte(0);
end;

procedure TdxPDFCustomStream.WriteArray(const AArray: TBytes);
begin
  WriteArray(AArray, Length(AArray));
end;

procedure TdxPDFCustomStream.WriteArray(const AArray: TBytes; ACount: Integer);
begin
  FStream.WriteBuffer(AArray[0], ACount);
end;

procedure TdxPDFCustomStream.WriteShortArray(const AArray: TSmallIntDynArray);
var
  AValue: SmallInt;
begin
  for AValue in AArray do
    WriteShort(AValue);
end;

procedure TdxPDFCustomStream.WriteShortArrayEx(const AArray: TSmallIntDynArray);
var
  I: Integer;
begin
  for I := 0 to Length(AArray) - 1 do
    WriteShort(AArray[I]);
end;

procedure TdxPDFCustomStream.WriteSpace;
begin
  WriteByte(Byte(' '));
end;

procedure TdxPDFCustomStream.WriteString(const AValue: string);
var
  C: Char;
begin
  for C in AValue do
    WriteByte(Byte(C));
end;

{ TdxPDFMemoryStream }

constructor TdxPDFMemoryStream.Create;
var
  AData: TBytes;
begin
  Create(TdxByteArray.Resize(AData, 0));
end;

constructor TdxPDFMemoryStream.Create(const AData: TBytes);
begin
  Create(TBytesStream.Create(AData));
end;

destructor TdxPDFMemoryStream.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TdxPDFMemoryStream.GetData: TBytes;
begin
  SetLength(Result, Size);
  cxCopyData(Stream.Bytes, Result, Size);
end;

function TdxPDFMemoryStream.GetStream: TBytesStream;
begin
  Result := TBytesStream(FStream);
end;

{ TdxPDFDocumentCustomStream }

procedure TdxPDFDocumentCustomStream.WriteCloseBracket;
begin
  WriteByte(Byte(']'));
end;

procedure TdxPDFDocumentCustomStream.WriteHexadecimalString
  (const AValue: TBytes);
var
  B: Byte;
  L, ALength, I, J, K: Integer;
  ABytes: TBytes;
begin
  L := Length(AValue);
  ALength := L * 2 + 2;
  SetLength(ABytes, ALength);
  ABytes[0] := 60;
  ABytes[ALength - 1] := 62;
  J := 0;
  for I := 0 to L - 1 do
  begin
    B := AValue[I];
    K := B shr 4;
    Inc(J);
    ABytes[J] := IfThen(K > 9, K + $37, K + $30);
    K := B and 15;
    Inc(J);
    ABytes[J] := IfThen(K > 9, K + $37, K + $30);
  end;
  WriteArray(ABytes);
end;

procedure TdxPDFDocumentCustomStream.WriteOpenBracket;
begin
  WriteByte(Byte('['));
end;

{ TdxPDFWriter }

constructor TdxPDFWriter.Create(AStream: TdxPDFDocumentCustomStream;
  AFreeStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FFreeStream := AFreeStream;
  FCurrentObjectNumber := dxPDFInvalidValue;
  FEncryptionInfo := CreateEncryptionProvider;
end;

constructor TdxPDFWriter.Create(AStream: TStream; AFreeStream: Boolean);
begin
  Create(TdxPDFDocumentCustomStream.Create(AStream, AFreeStream), True);
end;

destructor TdxPDFWriter.Destroy;
begin
  if FFreeStream then
    FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TdxPDFWriter.BeforeDestruction;
begin
  inherited;
  FEncryptionInfo := nil;
end;

function TdxPDFWriter.Encrypt(const ABytes: TBytes): TBytes;
begin
  if EncryptionInfo <> nil then
  begin
    dxTestCheck(FCurrentObjectNumber > 0,
      'TdxPDFWriter.Encrypt failed: object number is invalid');
    Result := EncryptionInfo.Encrypt(ABytes, FCurrentObjectNumber);
  end
  else
    Result := ABytes;
end;

procedure TdxPDFWriter.BeginWriteObject(ANumber: Integer);
begin
  dxTestCheck(FCurrentObjectNumber < 0, 'Recursion detected');
  FCurrentObjectNumber := ANumber;
  WriteString(IntToStr(ANumber) + ' 0 obj', True);
end;

procedure TdxPDFWriter.EndWriteObject;
begin
  WriteLineFeed;
  WriteString('endobj', True);
  FCurrentObjectNumber := dxPDFInvalidValue;
end;

procedure TdxPDFWriter.WriteByte(AValue: Byte);
begin
  FStream.WriteByte(AValue);
end;

procedure TdxPDFWriter.WriteBytes(const AValue: TBytes);
begin
  FStream.WriteArray(AValue);
end;

procedure TdxPDFWriter.WriteBytes(const AValue: TBytes; ACount: Integer);
begin
  FStream.WriteArray(AValue, ACount);
end;

procedure TdxPDFWriter.WriteCloseBracket;
begin
  FStream.WriteCloseBracket;
end;

procedure TdxPDFWriter.WriteCloseDictionary;
begin
  WriteString('>> ');
end;

procedure TdxPDFWriter.WriteDouble(AValue: Double);
begin
  FStream.WriteDouble(AValue);
end;

procedure TdxPDFWriter.WriteName(const AValue: string);

  function ReplaceSpecialChars(const S: string): string;
  begin
    Result := ReplaceStr(S, ' ', '#20');
    Result := ReplaceStr(Result, '/', '#2F');
  end;

begin
  FStream.WriteString('/');
  FStream.WriteString(ReplaceSpecialChars(AValue));
end;

procedure TdxPDFWriter.WriteInteger(AValue: Integer);
begin
  WriteString(IntToStr(AValue), False);
end;

procedure TdxPDFWriter.WriteOpenBracket;
begin
  FStream.WriteOpenBracket;
end;

procedure TdxPDFWriter.WriteOpenDictionary;
begin
  WriteString('<<');
end;

procedure TdxPDFWriter.WriteSpace;
begin
  FStream.WriteSpace;
end;

procedure TdxPDFWriter.WriteLineFeed;
begin
  WriteStringToStream(FStream.Stream, dxCRLF);
end;

procedure TdxPDFWriter.WriteHexadecimalString(const AValue: TBytes);

  procedure DoWriteByte(ABytes: TBytes; AIndex: Integer; B: Byte);
  begin
    ABytes[AIndex] := Byte(IfThen(B > 9, B + $37, B + $30));
  end;

var
  ABytes: TBytes;
  B: Byte;
  I, L, J: Integer;
begin
  L := Length(AValue);
  SetLength(ABytes, L * 2 + 2);
  ABytes[0] := TdxPDFDefinedSymbols.StartObject;
  ABytes[L * 2 + 2 - 1] := TdxPDFDefinedSymbols.EndObject;
  J := 0;
  for I := 0 to L - 1 do
  begin
    B := AValue[I];
    Inc(J);
    DoWriteByte(ABytes, J, B shr 4);
    Inc(J);
    DoWriteByte(ABytes, J, B and 15);
  end;
  WriteBytes(ABytes);
end;

procedure TdxPDFWriter.WriteString(const AValue: string;
  AWriteCrLf: Boolean = False);
begin
  WriteStringToStream(FStream.Stream, dxStringToAnsiString(AValue));
  if AWriteCrLf then
    WriteLineFeed;
end;

procedure TdxPDFWriter.WriteStringValue(const AValue: TBytes);

  function IsSpecialCharacter(var B: Byte): Boolean;
  begin
    case B of
      Ord(')'), Ord('('), Ord('\'):
        Result := True;

      Ord(#10):
        begin
          B := Ord('n');
          Result := True;
        end;

      Ord(#13):
        begin
          B := Ord('r');
          Result := True;
        end;

    else
      Result := False;
    end;
  end;

var
  I: Integer;
  ACode: Byte;
begin
  WriteByte(Ord('('));
  for I := Low(AValue) to High(AValue) do
  begin
    ACode := AValue[I];
    if IsSpecialCharacter(ACode) then
      WriteByte(Ord('\'));
    WriteByte(ACode);
  end;
  WriteByte(Ord(')'));
end;

function TdxPDFWriter.CreateEncryptionProvider: IdxPDFEncryptionInfo;
begin
  Result := nil;
end;

{ TdxPDFBaseStream }

constructor TdxPDFBaseStream.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
end;

destructor TdxPDFBaseStream.Destroy;
begin
  SetLength(FData, 0);
  inherited Destroy;
end;

procedure TdxPDFBaseStream.SetData(const AData: TBytes);
begin
  FData := AData;
end;

{ TdxPDFSmallIntegerDictionary }

procedure TdxPDFSmallIntegerDictionary.Assign
  (ASource: TdxPDFSmallIntegerDictionary);
var
  AKey: SmallInt;
begin
  Clear;
  for AKey in ASource.Keys do
    Add(AKey, ASource.Items[AKey]);
  TrimExcess;
end;

{ TdxPDFSortedIntegerStringDictionary }

procedure TdxPDFSortedIntegerStringDictionary.AfterConstruction;
begin
  inherited;
  FSortedKeys := TSortedKeys.Create(Self);
end;

destructor TdxPDFSortedIntegerStringDictionary.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSortedKeys);
end;

{ TdxPDFSortedIntegerStringDictionary.TSortedKeys }

constructor TdxPDFSortedIntegerStringDictionary.TSortedKeys.Create
  (ADictionary: TdxPDFIntegerStringDictionary);
begin
  FDictionary := ADictionary;
end;

function TdxPDFSortedIntegerStringDictionary.TSortedKeys.DoGetEnumerator
  : TEnumerator<Integer>;
begin
  Result := TSortedKeysEnumerator.Create(FDictionary);
end;

{ TdxPDFSortedIntegerStringDictionary.TSortedKeysEnumerator }

constructor TdxPDFSortedIntegerStringDictionary.TSortedKeysEnumerator.Create
  (ADictionary: TdxPDFIntegerStringDictionary);
var
  AKey: Integer;
  AKeyIndex: Integer;
begin
  FIndex := -1;
  SetLength(FKeys, ADictionary.Keys.Count);
  AKeyIndex := 0;
  for AKey in ADictionary.Keys do
  begin
    FKeys[AKeyIndex] := AKey;
    Inc(AKeyIndex);
  end;
  TArray.Sort<Integer>(FKeys);
end;

function TdxPDFSortedIntegerStringDictionary.TSortedKeysEnumerator.
  DoGetCurrent: Integer;
begin
  Result := FKeys[FIndex];
end;

function TdxPDFSortedIntegerStringDictionary.TSortedKeysEnumerator.
  DoMoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < Length(FKeys);
end;

end.
