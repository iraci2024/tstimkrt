{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressMemData }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSMEMDATA AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxmdaset;

interface

{$I cxVer.inc}

uses
{$IFNDEF DELPHI22}
  DBPlatform,
{$ENDIF}
  Generics.Defaults, Generics.Collections,
  Types, DB, SqlTimSt, Classes, SysUtils, Controls,
  dxCore, dxCoreClasses;

type
  TdxCustomMemData = class;
  TdxMemFields = class;
  TMemBlobData = AnsiString;
  TdxMemDataBuffer = TRecordBuffer;

{$IFDEF DELPHI17}
  TdxMemDataFieldList = TList<TField>;
{$ELSE}
  TdxMemDataFieldList = TList;
{$ENDIF}

  TdxIntegerList = class(TList)
  private
    function GetItem(AIndex: Integer): TdxNativeInt;
    procedure SetItem(AIndex: Integer; AValue: TdxNativeInt);
  public
    function Add(AValue: TdxNativeInt): Integer;
    function IndexOf(AValue: TdxNativeInt): Integer;
    procedure Insert(AIndex: Integer; AValue: TdxNativeInt);
    property Items[Index: Integer]: TdxNativeInt read GetItem
      write SetItem; default;
  end;

  TdxMemField = class
  private
    FField: TField;
    FDataType: TFieldType;
    FDataSize: Integer;
    FOffSet: Integer;
    FValueOffSet: Integer;
    FNextAutoIncValue: Integer;
    FOwner: TdxMemFields;
    FIndex: Integer;
    FNeedAutoInc: Boolean;

    function CompareRecordBuffers(ARecordBuffer1, ARecordBuffer2: TRecordBuffer;
      AIsCaseInsensitive: Boolean): Integer;

    function DataPointer(AIndex, AOffset: Integer): TRecordBuffer;
    procedure InternalAddValue(AValue: TRecordBuffer);

    function GetValues(AIndex: Integer): TRecordBuffer;
    function GetHasValue(AIndex: Integer): Boolean;
    function GetHasValues(AIndex: Integer): AnsiChar;
    procedure SetHasValue(AIndex: Integer; AValue: Boolean);
    procedure SetHasValues(AIndex: Integer; AValue: AnsiChar);

    procedure SetAutoIncValue(const AValue, ADataPointer: TRecordBuffer);

    function GetDataSet: TdxCustomMemData;
    function GetMemFields: TdxMemFields;

    property HasValue[AIndex: Integer]: Boolean read GetHasValue
      write SetHasValue;
  protected
    procedure CreateField(AField: TField); virtual;

    function GetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer): Boolean;
    procedure SetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer);

    property MemFields: TdxMemFields read GetMemFields;
  public
    constructor Create(AOwner: TdxMemFields);

    procedure AddValue(AValue: TRecordBuffer);
    // deprecated 'Use DataSet.AppendRecord method instead';
    procedure InsertValue(AIndex: Integer; ABuffer: TRecordBuffer);
    // deprecated 'Use DataSet.InsertRecord method instead';
    function GetDataFromBuffer(ABuffer: TRecordBuffer): TRecordBuffer;
    function GetHasValueFromBuffer(ABuffer: TRecordBuffer): AnsiChar;
    function GetValueFromBuffer(ARecordBuffer: TRecordBuffer): TRecordBuffer;

    // For the guys from AQA.
    property OffSet: Integer read FValueOffSet;

    property DataSet: TdxCustomMemData read GetDataSet;
    property Field: TField read FField;
    property Index: Integer read FIndex;
    property Values[AIndex: Integer]: TRecordBuffer read GetValues;
    property HasValues[AIndex: Integer]: AnsiChar read GetHasValues
      write SetHasValues;
  end;

  TdxMemFields = class
  private
    FItems: TList<TdxMemField>;
    FFieldsDictionary: TDictionary<TField, TdxMemField>;
    FCalcFields: TList;
    FDataSet: TdxCustomMemData;
    FValues: TList;
    FValuesSize: Integer;

    function GetRecordCount: Integer;
    function GetItem(AIndex: Integer): TdxMemField;
  protected
    function Add(AField: TField): TdxMemField;
    procedure Clear;
    function AllocateRecordMemory(ANewRecordIndex: Integer = -1): TRecordBuffer;
    procedure DeleteRecord(AIndex: Integer);
    procedure InsertRecord(const ABuffer: TRecordBuffer; AIndex: Integer;
      AAppend: Boolean);

    procedure AddField(AField: TField);
    procedure RemoveField(AField: TField);
  public
    constructor Create(ADataSet: TdxCustomMemData);
    destructor Destroy; override;

    procedure GetBuffer(Buffer: TRecordBuffer; AIndex: Integer);
    procedure SetBuffer(Buffer: TRecordBuffer; AIndex: Integer);
    function GetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer;
      Field: TField): Boolean;
    procedure SetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer;
      Field: TField);
    function GetCount: Integer;
    function IndexOf(AField: TField): TdxMemField;

    function GetValue(mField: TdxMemField; AIndex: Integer): TRecordBuffer;
    function GetHasValue(mField: TdxMemField; AIndex: Integer): AnsiChar;
    procedure SetValue(mField: TdxMemField; AIndex: Integer;
      ABuffer: TRecordBuffer);
    procedure SetHasValue(mField: TdxMemField; AIndex: Integer;
      AValue: AnsiChar);

    // For the guys from AQA.
    property Values: TList read FValues;

    property DataSet: TdxCustomMemData read FDataSet;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxMemField read GetItem;
    property RecordCount: Integer read GetRecordCount;
  end;

  PdxRecInfo = ^TdxRecInfo;

  TdxRecInfo = packed record
    Bookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  { TdxMemBlobStream }

  TdxMemBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TdxCustomMemData;
    FRecordBuffer: TRecordBuffer;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TdxCustomMemData }

  TdxSortOption = (soDesc, soCaseInsensitive);
  TdxSortOptions = set of TdxSortOption;

  TdxMemIndex = class(TCollectionItem)
  private
    fIsDirty: Boolean;
    FField: TField;
    FSortOptions: TdxSortOptions;
    fLoadedFieldName: string;
    fFieldName: string;
    FValues: TList;
    FValueIndexes: TdxIntegerList;

    procedure SetIsDirty(Value: Boolean);
    procedure DeleteRecord(ABuffer: TRecordBuffer);
    procedure UpdateRecord(ABuffer: TRecordBuffer);
    procedure SetFieldName(Value: string);
    procedure SetSortOptions(Value: TdxSortOptions);
    procedure SetFieldNameAfterMemdataLoaded;
  protected
    function GetMemData: TdxCustomMemData;
    procedure Prepare;
    function GotoNearest(const ABuffer: TRecordBuffer;
      ALocateOptions: TLocateOptions; out AIndex: Integer): Boolean;

    property IsDirty: Boolean read fIsDirty write SetIsDirty;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property MemData: TdxCustomMemData read GetMemData;
  published
    property FieldName: string read fFieldName write SetFieldName;
    property SortOptions: TdxSortOptions read FSortOptions write SetSortOptions;
  end;

  TdxMemIndexes = class(TCollection)
  private
    FMemData: TdxCustomMemData;
  protected
    function GetOwner: TPersistent; override;
    procedure SetIsDirty;
    procedure DeleteRecord(ABuffer: TRecordBuffer);
    procedure UpdateRecord(ABuffer: TRecordBuffer);
    procedure RemoveField(AField: TField);
    procedure CheckFields;
    procedure AfterMemdataLoaded;
  public
    function Add: TdxMemIndex;
    function GetIndexByField(AField: TField): TdxMemIndex;

    property MemData: TdxCustomMemData read FMemData;
  end;

  TdxMemPersistentOption = (poNone, poActive, poLoad);

  TdxMemPersistent = class(TPersistent)
  private
    FStream: TMemoryStream;
    FOption: TdxMemPersistentOption;
    FMemData: TdxCustomMemData;

    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AMemData: TdxCustomMemData);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveData;
    procedure LoadData;

    function HasData: Boolean;

    property MemData: TdxCustomMemData read FMemData;
  published
    property Option: TdxMemPersistentOption read FOption write FOption
      default poActive;
  end;

  TdxMemSortingInfo = record // for internal use only
    MemField: TdxMemField;
    Options: TdxSortOptions;
  end;

  TdxMemSortingInfos = array of TdxMemSortingInfo;

  { TdxCustomMemData }

  TdxCustomMemData = class(TDataSet)
  private
    FActive: Boolean;
    FData: TdxMemFields;
    FRecBufSize: Integer;
    FRecInfoOfs: Integer;
    FCurRec: Integer;
    FFilterCurRec: Integer;
    FBookMarks: TdxIntegerList;
    FBlobList: TList;
    FFilterList: TdxIntegerList;
    FLastBookmark: Integer;
    FSaveChanges: Boolean;
    FReadOnly: Boolean;
    FRecIdField: TField;
    FSortOptions: TdxSortOptions;
    FSortedFieldNames: string;
    FSortedField: TField;
    FSortingInfos: TdxMemSortingInfos;
    FLoadFlag: Boolean;
    FDelimiterChar: Char;
    FIsFiltered: Boolean;
    FGotoNearestMin: Integer;
    FGotoNearestMax: Integer;
    FProgrammedFilter: Boolean;
    FIndexes: TdxMemIndexes;
    FPersistent: TdxMemPersistent;

    function GetCalcBuffer: TdxMemDataBuffer;
    function GetRecIdMemField: TdxMemField;

    function AllocBufferForFieldValue(AValue: Variant; AField: TField): Pointer;
    function AllocBufferForField(AField: TField): Pointer;
    function GetSortOptions: TdxSortOptions;
    procedure PopulateValueList(const AList: TList);
    procedure SetSortedFields(Value: string);
    procedure SetSortOptions(Value: TdxSortOptions);
    procedure SetIndexes(Value: TdxMemIndexes);
    procedure SetPersistent(Value: TdxMemPersistent);
    function GetActiveRecBuf(var ARecordBuffer: TRecordBuffer): Boolean;
    procedure DoSort(ASortedList, ASynchronizationLists: TList;
      AmdField: TdxMemField; ASortOptions: TdxSortOptions); overload;
    procedure DoSort(ASortedList, ASynchronizationLists: TList;
      ASortingInfos: TdxMemSortingInfos); overload;
    procedure MakeSort;
    procedure CreateRecIDField;
    function GetStringLength(AFieldType: TFieldType;
      const ABuffer: Pointer): Integer;
    function InternalSetRecNo(const Value: Integer): Integer;
    function InternalLocate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Integer;
    procedure UpdateRecordFilteringAndSorting(AIsMakeSort: Boolean);
    function InternalIsFiltering: Boolean;

    property CalcBuffer: TdxMemDataBuffer read GetCalcBuffer;
    property IsBinaryDataLoading: Boolean read FLoadFlag;
    property RecIdMemField: TdxMemField read GetRecIdMemField;
  protected
    function ActiveBuffer: TdxMemDataBuffer; inline;

    procedure InitializeBlobData(ABuffer: TRecordBuffer);
    procedure FinalizeBlobData(ABuffer: TRecordBuffer);

    function GetRecordData(ARecordBuffer: TRecordBuffer): TRecordBuffer;

    function GetBlobInfo(ADataBuffer: TRecordBuffer; AOffset: Integer;
      out ABlobData: TRecordBuffer; out ABlobSize: Integer): Boolean; overload;
    function GetBlobInfo(ARecordBuffer: TRecordBuffer; AField: TField;
      out ABlobData: TRecordBuffer; out ABlobSize: Integer): Boolean; overload;
    function GetBlobPointer(ADataBuffer: TRecordBuffer; AOffset: Integer)
      : TRecordBuffer;
    function GetBlobData(ADataBuffer: TRecordBuffer; AOffset: Integer)
      : TMemBlobData; overload;
    function GetBlobData(ARecordBuffer: TRecordBuffer; AField: TField)
      : TMemBlobData; overload;
    procedure SetInternalBlobData(ADataBuffer: TRecordBuffer; AOffset: Integer;
      AValue: TRecordBuffer; ASize: Integer); overload;
    procedure SetInternalBlobData(ADataBuffer: TRecordBuffer; AOffset: Integer;
      AValue: TMemBlobData); overload;
    procedure SetBlobData(ADataBuffer: TRecordBuffer; AOffset: Integer;
      const AValue: TRecordBuffer; ASize: Integer); overload;
    procedure SetBlobData(ARecordBuffer: TRecordBuffer; AField: TField;
      const AValue: TMemBlobData); overload;
    procedure SetBlobData(ARecordBuffer: TRecordBuffer; AField: TField;
      const AValue: TRecordBuffer; ASize: Integer); overload;
    function GetActiveBlobData(Field: TField): TMemBlobData;
    procedure GetMemBlobData(Buffer: TRecordBuffer);
    procedure SetMemBlobData(Buffer: TRecordBuffer);
    procedure BlobClear;

    procedure Loaded; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
{$IFDEF DELPHI17}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
{$ENDIF}
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(ABuffer: Pointer; AAppend: Boolean); override;
{$IFDEF DELPHI17}
    procedure InternalAddRecord(ABuffer: TRecordBuffer;
      AAppend: Boolean); override;
{$ENDIF}
    procedure InternalInsert; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer;
      Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
{$IFDEF DELPHI17}
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
{$ENDIF}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer;
      NativeFormat: Boolean); override;
    function GetStateFieldValue(State: TDataSetState; Field: TField)
      : Variant; override;

    procedure DoAfterCancel; override;
    procedure DoAfterClose; override;
    procedure DoAfterInsert; override;
    procedure DoAfterOpen; override;
    procedure DoAfterPost; override;
    procedure DoBeforeClose; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforePost; override;
    procedure DoOnNewRecord; override;
  protected
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    procedure GetCalcFields(Buffer: TRecordBuffer); override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetCanModify: Boolean; override;
    procedure SetFiltered(Value: Boolean); override;

    function GetAnsiStringValue(const ABuffer: TRecordBuffer): AnsiString;
    function GetWideStringValue(const ABuffer: TRecordBuffer): WideString;
    function GetIntegerValue(const ABuffer: TRecordBuffer;
      ADataType: TFieldType): Integer;
    function GetLargeIntValue(const ABuffer: TRecordBuffer): Largeint;
    function GetFloatValue(const ABuffer: TRecordBuffer): Double;
    function GetExtendedValue(const ABuffer: TRecordBuffer): Extended;
    function GetCurrencyValue(const ABuffer: TRecordBuffer): Currency;
    function GetDateTimeValue(const ABuffer: TRecordBuffer; AField: TField)
      : TDateTime;
    function GetTimeStampValue(const ABuffer: TRecordBuffer; AField: TField)
      : TSQLTimeStamp;
    function GetVariantValue(const ABuffer: TRecordBuffer;
      AField: TField): Variant;

    function InternalCompareValues(const ABuffer1, ABuffer2: Pointer;
      AMemField: TdxMemField; AIsCaseInsensitive: Boolean;
      ACount: Integer = -1): Integer;
    function CompareValues(const ABuffer1, ABuffer2: TRecordBuffer;
      AMemField: TdxMemField; ASortOptions: TdxSortOptions): Integer; overload;
    function CompareValues(const ABuffer1, ABuffer2: TRecordBuffer;
      AMemField: TdxMemField): Integer; overload;
    function CompareValues(const ABuffer1, ABuffer2: TRecordBuffer;
      AField: TField): Integer; overload;

    function InternalGotoNearest(AValues: TList; AField: TField;
      const ABuffer: TRecordBuffer; ASortOptions: TdxSortOptions;
      ALocateOptions: TLocateOptions; out AIndex: Integer): Boolean; overload;
    function InternalGotoNearest(AValues: TList;
      ACompareInfos: TdxMemSortingInfos; const ARecordBuffer: TRecordBuffer)
      : Integer; overload;
    function GotoNearest(const AValueBuffer: TRecordBuffer;
      ALocateOptions: TLocateOptions; out AIndex: Integer): Boolean; overload;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure InternalAddFilterRecord;
    procedure MakeRecordSort;
    procedure UpdateFilterRecord; virtual;

    procedure CloseBlob(Field: TField); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; {$IFDEF DELPHI18}var
      {$ENDIF} Buffer: TValueBuffer): Boolean; override;
    function GetFieldData(Field: TField; {$IFDEF DELPHI18}var
      {$ENDIF} Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function GetCurrentRecord(Buffer: TRecordBuffer): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark)
      : Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode)
      : TStream; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;

    // streaming
    procedure LoadFromTextFile(const AFileName: string); dynamic;
    procedure SaveToTextFile(const AFileName: string); dynamic;
    procedure LoadFromStrings(AStrings: TStrings); dynamic;
    procedure SaveToStrings(AStrings: TStrings); dynamic;
    procedure LoadFromBinaryFile(const AFileName: string); dynamic;
    procedure SaveToBinaryFile(const AFileName: string); dynamic;
    procedure LoadFromStream(AStream: TStream); dynamic;
    procedure SaveToStream(AStream: TStream); dynamic;
    procedure AddFieldsFromDataSet(ADataSet: TDataSet;
      AOwner: TComponent = nil);
    procedure CreateFieldsFromBinaryFile(const AFileName: string);
    procedure CreateFieldsFromStream(Stream: TStream);
    procedure CreateFieldsFromDataSet(ADataSet: TDataSet;
      AOwner: TComponent = nil);
    procedure LoadFromDataSet(DataSet: TDataSet);
    procedure CopyFromDataSet(DataSet: TDataSet);

    function GetRecNoByFieldValue(Value: Variant; FieldName: string)
      : Integer; virtual;
    function SupportedFieldType(AType: TFieldType): Boolean; virtual;
    procedure FillBookMarks;
    procedure MoveCurRecordTo(AIndex: Integer);
    procedure UpdateFilters; virtual;
    { if failed return -1, in other case the record count with the same value }
    function GetValueCount(AFieldName: string; AValue: Variant): Integer;
    procedure SetFilteredRecNo(Value: Integer);

    // Again for the guys from AQA. Hi Atanas :-)
    property CurRec: Integer read FCurRec write FCurRec;

    property BlobFieldCount;
    property BlobList: TList read FBlobList;
    // FilterList made public - so we can set the list of filtered records
    // when ProgrammedFilter is True, the developer is responsible to set the list
    property FilterList: TdxIntegerList read FFilterList;
    // ProgrammedFilter - for faster setting of the filers. This avoids calling OnFilterRecord
    property ProgrammedFilter: Boolean read FProgrammedFilter
      write FProgrammedFilter;

    property RecIdField: TField read FRecIdField;
    property IsLoading: Boolean read FLoadFlag write FLoadFlag;
    property Data: TdxMemFields read FData;
    property DelimiterChar: Char read FDelimiterChar write FDelimiterChar;
    property Filter;

    property Indexes: TdxMemIndexes read FIndexes write SetIndexes;
    property Persistent: TdxMemPersistent read FPersistent write SetPersistent;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property SortOptions: TdxSortOptions read GetSortOptions
      write SetSortOptions;
    property SortedField: string read FSortedFieldNames write SetSortedFields
      stored False;
    property SortedFields: string read FSortedFieldNames write SetSortedFields;
  end;

  TdxMemData = class(TdxCustomMemData)
  published
    property Active;
    property Indexes;
    property Persistent;
    property ReadOnly;
    property SortOptions;
    property SortedField;
    property SortedFields;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;

    property OnFilterRecord;
  end;

procedure DateTimeToMemDataValue(Value: TDateTime; pt: TRecordBuffer;
  Field: TField);
function VariantToMemDataValue(const AValue: Variant; AMemDataValue: Pointer;
  AField: TField): Boolean;

const
  MemDataVer = 1.91;

implementation

uses
  Variants, FmtBcd, ActiveX, Windows, DbConsts, DBCommon, Contnrs, Math,
  StrUtils,
  cxVariants, cxDB, cxDataConsts;

type
  TFieldAccess = class(TField);

const
  MemDataVerString = 'Ver';
  ftStrings = [ftString, ftWideString, ftGuid];

function ValueBufferToRecordBuffer(ABuffer: TValueBuffer): TRecordBuffer;
begin
{$IFDEF DELPHI17}
  Result := @ABuffer[0];
{$ELSE}
  Result := ABuffer;
{$ENDIF}
end;

{
  function RecordBufferToValueBuffer(ABuffer: TRecordBuffer): TValueBuffer;
  begin
  Result := ABuffer;
  end;
}

function GetFieldValue(AField: TField): Variant;
begin
  if AField.IsNull then
    Result := Null
  else
    case AField.DataType of
      ftWideString:
        Result := AField.AsString; // Borland bug with WideString
    else
      Result := AField.Value;
    end;
end;

procedure SetFieldValue(ASrcField, ADestField: TField);
begin
  if ASrcField.IsNull then
    ADestField.Value := Null
  else
    case ASrcField.DataType of
      ftLargeInt:
        TLargeintField(ADestField).Value := TLargeintField(ASrcField).Value;
        // for D6
    else
      ADestField.Value := ASrcField.Value;
    end;
end;

function GetCharSize(AFieldType: TFieldType): Integer;
begin
  case AFieldType of
    ftString, ftGuid:
      Result := 1;
    ftWideString:
      Result := 2;
  else
    Result := 0;
  end;
end;

function GetDataSize(AValue: Variant; AField: TField): Integer; overload;
var
  ADataSize: Integer;
begin
  if AField.DataType in ftStrings then
  begin
    if not VarIsNull(AValue) then
      ADataSize := Length(AValue)
    else
      ADataSize := AField.Size;
    Result := (ADataSize + 1) * GetCharSize(AField.DataType)
  end
  else
    Result := AField.DataSize;
end;

function GetDataSize(AField: TField): Integer; overload;
begin
  Result := GetDataSize(Null, AField);
end;

function StrLen(const S: Pointer; AFieldType: TFieldType): Integer;
var
  B: PByte;
  W: PWord;
begin
  Result := 0;
  case AFieldType of
    ftWideString:
      begin
        W := S;
        while W^ <> 0 do
        begin
          Inc(Result);
          Inc(W);
        end;
      end;

    ftString, ftGuid:
      begin
        B := S;
        while B^ <> 0 do
        begin
          Inc(Result);
          Inc(B);
        end;
      end;
  end;
end;

function AllocBuferForString(ALength: Integer; AFieldType: TFieldType): Pointer;
begin
  Result := AllocMem((ALength + 1) * GetCharSize(AFieldType));
end;

procedure CopyChars(ASource, ADest: Pointer; AMaxCharCount: Integer;
  AFieldType: TFieldType);
var
  ACharSize: Integer;
  ASize: Integer;
begin
  ACharSize := GetCharSize(AFieldType);
  ASize := Min(StrLen(ASource, AFieldType), AMaxCharCount) * ACharSize;
  cxCopyData(ASource, ADest, ASize);
  cxZeroMemory(ShiftPointer(ADest, ASize), ACharSize);
end;

procedure DateTimeToMemDataValue(Value: TDateTime; pt: TRecordBuffer;
  Field: TField);
var
  TimeStamp: TTimeStamp;
  Data: TDateTimeRec;
  DataSize: Integer;
begin
  TimeStamp := DateTimeToTimeStamp(Value);
  DataSize := 4;
  case Field.DataType of
    ftDate:
      Data.Date := TimeStamp.Date;
    ftTime:
      Data.Time := TimeStamp.Time;
  else
    begin
      Data.DateTime := TimeStampToMSecs(TimeStamp);
      DataSize := 8;
    end;
  end;
  Move(Data, pt^, DataSize);
end;

type
  TFakeDataSet = class(TDataSet)
  private
    FBuffer: Pointer;
  protected
    function GetFieldData(Field: TField; {$IFDEF DELPHI18}var
      {$ENDIF} Buffer: TValueBuffer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
  end;

function TFakeDataSet.GetFieldData(Field: TField; {$IFDEF DELPHI18}var
  {$ENDIF} Buffer: TValueBuffer): Boolean;
begin
  Result := FBuffer <> nil;
  if Buffer <> nil then
    cxCopyData(FBuffer, Buffer, Field.DataSize);
end;

procedure TFakeDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  cxCopyData(Buffer, FBuffer, Field.DataSize);
end;

function VariantToMemDataValue(const AValue: Variant; AMemDataValue: Pointer;
  AField: TField): Boolean;
var
  AAnsiString: AnsiString;
  AWideString: WideString;
  AFakeDataSet: TFakeDataSet;
  AFakeField: TField;
begin
  Result := AMemDataValue <> nil;
  if Result then
    case AField.DataType of
      ftString, ftGuid:
        begin
          AAnsiString := dxVariantToAnsiString(AValue);
          CopyChars(PAnsiChar(AAnsiString), AMemDataValue, MaxInt,
            AField.DataType);
        end;
      ftWideString:
        begin
          AWideString := AValue;
          CopyChars(PWideChar(AWideString), AMemDataValue, MaxInt,
            AField.DataType);
        end;
    else
{$WARNINGS OFF}
      AFakeDataSet := TFakeDataSet.Create(nil);
{$WARNINGS ON}
      try
        AFakeField := TFieldClass(AField.ClassType).Create(AFakeDataSet);
        AFakeField.FieldName := AField.FieldName;
        AFakeField.DataSet := AFakeDataSet;
        AFakeDataSet.FBuffer := AMemDataValue;
        AFakeDataSet.InitFieldDefsFromFields;
        AFakeDataSet.BindFields(True);
        AFakeField.Value := AValue;
      finally
        AFakeDataSet.Free;
      end;
    end;
end;

function GetNoByFieldType(FieldType: TFieldType): Integer;
const
  dxFieldType: array [TFieldType] of Integer = (-1, // ftUnknown
    1, // ftString
    2, // ftSmallint
    3, // ftInteger
    4, // ftWord,
    5, // ftBoolean,
    6, // ftFloat,
    7, // ftCurrency,
    8, // ftBCD,
    9, // ftDate,
    10, // ftTime,
    11, // ftDateTime,
    -1, // ftBytes,
    -1, // ftVarBytes,
    12, // ftAutoInc,
    13, // ftBlob,
    14, // ftMemo,
    15, // ftGraphic,
    16, // ftFmtMemo,
    17, // ftParadoxOle,
    18, // ftDBaseOle,
    19, // ftTypedBinary
    -1, // ftCursor
    -1, // ftFixedChar
    20, // ftWideString
    21, // ftLargeInt
    -1, // ftADT
    -1, // ftArray
    -1, // ftReference
    -1, // ftDataSet
    -1, // ftOraBlob
    -1, // ftOraClob
    -1, // ftVariant
    -1, // ftInterface
    -1, // ftIDispatch
    22, // ftGuid
    23, // ftTimeStamp
    24 // ftFMTBcd
    , -1, // ftFixedWideChar
    25, // ftWideMemo
    -1, // ftOraTimeStamp
    -1 // ftOraInterval
    , 26, // ftLongWord
    27, // ftShortint
    28, // ftByte
    29, // ftExtended
    -1, // ftConnection
    -1, // ftParams
    -1 // ftStream
    , -1, // ftTimeStampOffset
    -1, // ftObject,
    -1 // ftSingle
    );
begin
  Result := dxFieldType[FieldType];
end;

const
  SupportFieldCount = 29;

function GetFieldTypeByNo(No: Integer): TFieldType;
const
  dxFieldType: array [1 .. SupportFieldCount] of TFieldType = (ftString,
    ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD,
    ftDate, ftTime, ftDateTime, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftWideString, ftLargeInt, ftGuid,
    ftTimeStamp, ftFMTBcd, ftWideMemo, ftLongWord, ftShortint, ftByte,
    ftExtended);
begin
  if (No < 1) or (No > SupportFieldCount) then
    Result := ftUnknown
  else
    Result := dxFieldType[No];
end;

function GetValidName(AOwner: TComponent; AName: string): string;
var
  I: Integer;
begin
  for I := 1 to Length(AName) do
    if not(dxCharInSet(AName[I], ['A' .. 'z']) or dxCharInSet(AName[I],
      ['0' .. '9'])) then
      AName[I] := '_';
  if dxCharInSet(AName[1], ['0' .. '9']) then
    AName := '_' + AName;

  Result := AName;

  I := 0;
  while AOwner.FindComponent(Result) <> nil do
  begin
    Result := AName + IntToStr(I);
    Inc(I);
  end
end;

procedure HandleException(ASender: TObject);
begin
  if Assigned(ApplicationHandleException) then
    ApplicationHandleException(ASender);
end;

type
  TdxBaseFieldType = (bftBlob, bftString, bftOrdinal);

  TdxFieldStreamer = class
  protected
    FField: TField;
  public
    property Field: TField read FField;
  end;

  TdxFieldReader = class(TdxFieldStreamer)
  private
    fFieldName: string;
    FBuffer: TRecordBuffer;
    FDataSize: Integer;
    FFieldSize: Integer;
    FFieldTypeNo: Integer;
    FDataType: TFieldType;
    BlobData: TMemBlobData;

    FRecordFieldSize: Integer;
    FHasValue: Byte;

    function GetHasValue: Boolean;
    procedure SetHasValue(Value: Boolean);

    function ReadFieldSize(AStream: TStream): Boolean;

    property HasValue: Boolean read GetHasValue write SetHasValue;
  protected
    function GetDataSize(AReadingDataSize: Integer): Integer; virtual;
    function GetFieldSize(AReadingDataSize: Integer): Integer; virtual;
  public
    constructor Create(AFieldName: string; AField: TField; ADataSize: Integer;
      AFieldTypeNo: Integer); virtual;
    destructor Destroy; override;

    procedure CreateField(AMemData: TdxCustomMemData); virtual;
    function ReadFieldValue(AStream: TStream; AVerNo: Double): Boolean;
      virtual; abstract;

    property FieldName: string read fFieldName;
    property FieldTypeNo: Integer read FFieldTypeNo;
    property DataType: TFieldType read FDataType;
  end;

  TdxFieldReaderClass = class of TdxFieldReader;

  { TdxReadBlobField }

  TdxBlobFieldReader = class(TdxFieldReader)
  private
    function ReadBlobFieldValue(AStream: TStream): Boolean;
  public
    function ReadFieldValue(AStream: TStream; AVerNo: Double): Boolean;
      override;
  end;

  { TdxReadStringField }

  TdxStringFieldReader = class(TdxFieldReader)
  private
    function ReadString(AStream: TStream): Boolean;
    function ReadStringFieldValue(AStream: TStream): Boolean;
  protected
    function GetDataSize(AReadingDataSize: Integer): Integer; override;
    function GetFieldSize(AReadingDataSize: Integer): Integer; override;
  public
    procedure CreateField(AMemData: TdxCustomMemData); override;
    function ReadFieldValue(AStream: TStream; AVerNo: Double): Boolean;
      override;
  end;

  { TdxReadStringFieldVer190 (1.90) }

  TdxStringFieldReaderVer190 = class(TdxStringFieldReader)
  public
    function ReadFieldValue(AStream: TStream; AVerNo: Double): Boolean;
      override;
  end;

  { TdxReadStringFieldVer191 (1.91) }

  TdxStringFieldReaderVer191 = class(TdxStringFieldReaderVer190)
  protected
    function GetDataSize(AReadingDataSize: Integer): Integer; override;
    function GetFieldSize(AReadingDataSize: Integer): Integer; override;
  end;

  { TdxReadOrdinalField }

  TdxOrdinalFieldReader = class(TdxFieldReader)
  private
    function ReadOrdinalFieldValue(AStream: TStream): Boolean;
  public
    function ReadFieldValue(AStream: TStream; AVerNo: Double): Boolean;
      override;
  end;

  { TdxFieldWriter }

  TdxFieldWriter = class(TdxFieldStreamer)
  protected
    FMemData: TdxCustomMemData;
    procedure WriteFieldValue(AStream: TStream; AMemField: TdxMemField;
      ARecordIndex: Integer); virtual; abstract;

    property MemData: TdxCustomMemData read FMemData;
  public
    constructor Create(AMemData: TdxCustomMemData; AField: TField); virtual;
  end;

  TdxFieldWriterClass = class of TdxFieldWriter;

  { TdxBlobFieldWriter }

  TdxBlobFieldWriter = class(TdxFieldWriter)
  protected
    procedure WriteFieldValue(AStream: TStream; AMemField: TdxMemField;
      ARecordIndex: Integer); override;
  end;

  { TdxStringFieldWriter }

  TdxStringFieldWriter = class(TdxFieldWriter)
  protected
    procedure WriteFieldValue(AStream: TStream; AMemField: TdxMemField;
      ARecordIndex: Integer); override;
  end;

  { TdxOrdinalFieldWriter }

  TdxOrdinalFieldWriter = class(TdxFieldWriter)
    procedure WriteFieldValue(AStream: TStream; AMemField: TdxMemField;
      ARecordIndex: Integer); override;
  end;

  { TdxMemDataStreamer }

  TdxMemDataStreamer = class
  protected
    FStream: TStream;
    FMemData: TdxCustomMemData;
    FFields: TList;
    FFieldStreamers: TObjectList;

    function BaseFieldType(AFieldType: TFieldType): TdxBaseFieldType;
    function FieldCount: Integer;
    function FieldStreamersCount: Integer;
    procedure PopulateFieldList;
    function GetField(Index: Integer): TField;
    function GetFieldStreamersByField(AField: TField): TdxFieldStreamer;
    function MemDataField(AField: TField): TdxMemField;

    property Fields[Index: Integer]: TField read GetField;
  public
    constructor Create(AMemData: TdxCustomMemData; AStream: TStream); virtual;
    destructor Destroy; override;

    property Stream: TStream read FStream;
    property MemData: TdxCustomMemData read FMemData;
  end;

  { TdxMemDataStreamReader }

  TdxMemDataStreamReader = class(TdxMemDataStreamer)
  private
    FVerNo: Double;

    function GetFieldReader(Index: Integer): TdxFieldReader;
    function GetFieldReaderClass(AFieldTypeNo: Integer): TdxFieldReaderClass;
    function GetFieldReadersByField(AField: TField): TdxFieldReader;
  protected
    procedure AddRecord;
    function ReadVerNoFromStream: Boolean;
    function ReadFieldsFromStream: Boolean;
    function ReadRecordFromStream: Boolean;

    property FieldReaders[Index: Integer]: TdxFieldReader read GetFieldReader;
    property FieldReadersByField[Field: TField]: TdxFieldReader
      read GetFieldReadersByField;
    property VerNo: Double read FVerNo;
  public
    constructor Create(AMemData: TdxCustomMemData; AStream: TStream); override;

    procedure CreateFields(AMemData: TdxCustomMemData);
    procedure LoadData;
  end;

  { TdxMemDataStreamWriter }

  TdxMemDataStreamWriter = class(TdxMemDataStreamer)
  private
    function GetFieldWriterClass(AFieldType: TFieldType): TdxFieldWriterClass;
    function GetFieldWritersByField(AField: TField): TdxFieldWriter;

    procedure WriteMemDataVersion;
    procedure WriteFields;
    procedure WriteRecord(ARecordIndex: Integer);

    property FieldWritersByField[Field: TField]: TdxFieldWriter
      read GetFieldWritersByField;
  public
    procedure SaveData;
  end;

  { TdxMemDataStreamer }

constructor TdxMemDataStreamer.Create(AMemData: TdxCustomMemData;
  AStream: TStream);
begin
  inherited Create;
  FMemData := AMemData;
  FStream := AStream;
  FFields := TList.Create;
  FFieldStreamers := TObjectList.Create;
end;

destructor TdxMemDataStreamer.Destroy;
begin
  FreeAndNil(FFieldStreamers);
  FreeAndNil(FFields);
  inherited Destroy;
end;

function TdxMemDataStreamer.BaseFieldType(AFieldType: TFieldType)
  : TdxBaseFieldType;
begin
  if (MemData.GetFieldClass(AFieldType) <> nil) and
    MemData.GetFieldClass(AFieldType).IsBlob then
    Result := bftBlob
  else if AFieldType in ftStrings then
    Result := bftString
  else
    Result := bftOrdinal;
end;

function TdxMemDataStreamer.FieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TdxMemDataStreamer.FieldStreamersCount: Integer;
begin
  Result := FFieldStreamers.Count;
end;

procedure TdxMemDataStreamer.PopulateFieldList;
var
  I: Integer;
begin
  for I := 0 to MemData.FieldCount - 1 do
    if not MemData.Fields[I].Lookup and not MemData.Fields[I].Calculated then
      FFields.Add(MemData.Fields[I]);
end;

function TdxMemDataStreamer.GetField(Index: Integer): TField;
begin
  Result := TField(FFields[Index]);
end;

function TdxMemDataStreamer.GetFieldStreamersByField(AField: TField)
  : TdxFieldStreamer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldStreamersCount - 1 do
    if TdxFieldStreamer(FFieldStreamers[I]).Field = AField then
    begin
      Result := TdxFieldStreamer(FFieldStreamers[I]);
      Break;
    end;
end;

function TdxMemDataStreamer.MemDataField(AField: TField): TdxMemField;
begin
  Result := MemData.Data.IndexOf(AField);
end;

{ TdxReadOrdinalField }

function TdxOrdinalFieldReader.ReadFieldValue(AStream: TStream;
  AVerNo: Double): Boolean;
begin
  if AVerNo > 0 then
    AStream.Read(FHasValue, 1);
  Result := ReadOrdinalFieldValue(AStream);
end;

function TdxOrdinalFieldReader.ReadOrdinalFieldValue(AStream: TStream): Boolean;
begin
  if Field <> nil then
    Result := ReadBufferFromStream(AStream, FBuffer, FDataSize)
  else
  begin
    AStream.Position := AStream.Position + FDataSize;
    Result := AStream.Position <= AStream.Size;
  end;
end;

{ TdxMemPersistent }
procedure TdxMemPersistent.Assign(Source: TPersistent);
begin
  if (Source is TdxMemPersistent) then
  begin
    Option := TdxMemPersistent(Source).Option;
    FStream.LoadFromStream(TdxMemPersistent(Source).FStream);
  end
  else
    inherited;
end;

constructor TdxMemPersistent.Create(AMemData: TdxCustomMemData);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FOption := poActive;
  FMemData := AMemData;
end;

destructor TdxMemPersistent.Destroy;
begin
  FStream.Free;

  inherited Destroy;
end;

procedure TdxMemPersistent.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  if (csWriting in MemData.ComponentState) and MemData.Active then
    SaveData;

  Filer.DefineBinaryProperty('Data', ReadData, WriteData, HasData);
end;

procedure TdxMemPersistent.ReadData(Stream: TStream);
begin
  FStream.Clear;
  FStream.LoadFromStream(Stream);
end;

procedure TdxMemPersistent.WriteData(Stream: TStream);
begin
  FStream.SaveToStream(Stream);
end;

function TdxMemPersistent.HasData: Boolean;
begin
  Result := FStream.Size > 0;
end;

procedure TdxMemPersistent.LoadData;
begin
  if HasData then
  begin
    FStream.Position := 0;
    FMemData.LoadFromStream(FStream);
  end;
end;

procedure TdxMemPersistent.SaveData;
begin
  FStream.Clear;
  if FMemData.RecordCount > 0 then
    FMemData.SaveToStream(FStream);
end;

{ TdxMemFields }
constructor TdxMemFields.Create(ADataSet: TdxCustomMemData);
begin
  inherited Create;
  FDataSet := ADataSet;
  FItems := TList<TdxMemField>.Create;
  FFieldsDictionary := TDictionary<TField, TdxMemField>.Create;
  FCalcFields := TList.Create;
end;

destructor TdxMemFields.Destroy;
begin
  Clear;
  FreeAndNil(FFieldsDictionary);
  FreeAndNil(FItems);
  FreeAndNil(FCalcFields);

  inherited Destroy;
end;

procedure TdxMemFields.Clear;
var
  I: Integer;
begin
  if FValues <> nil then
  begin
    for I := FValues.Count - 1 downto 0 do
      DeleteRecord(I);
    FreeAndNil(FValues);
  end;
  for I := 0 to FItems.Count - 1 do
    Items[I].Free;
  FItems.Clear;
  FCalcFields.Clear;
  FFieldsDictionary.Clear;
end;

function TdxMemFields.AllocateRecordMemory(ANewRecordIndex: Integer = -1)
  : TRecordBuffer;
begin
  Result := AllocMem(FValuesSize);
  if ANewRecordIndex = -1 then
    FValues.Add(Result)
  else
    FValues.Insert(ANewRecordIndex, Result);
end;

procedure TdxMemFields.DeleteRecord(AIndex: Integer);
begin
  FreeMem(Pointer(FValues[AIndex]));
  FValues.Delete(AIndex);
end;

function TdxMemFields.Add(AField: TField): TdxMemField;
begin
  Result := TdxMemField.Create(self);
  FItems.Add(Result);
  FFieldsDictionary.Add(AField, Result);
  Result.CreateField(AField);
end;

function TdxMemFields.GetItem(AIndex: Integer): TdxMemField;
begin
  Result := FItems[AIndex];
end;

function TdxMemFields.IndexOf(AField: TField): TdxMemField;
begin
  Result := nil;
  if FFieldsDictionary.ContainsKey(AField) then
    Result := FFieldsDictionary[AField];
end;

function TdxMemFields.GetValue(mField: TdxMemField; AIndex: Integer)
  : TRecordBuffer;
begin
  Result := mField.Values[AIndex];
end;

function TdxMemFields.GetHasValue(mField: TdxMemField; AIndex: Integer)
  : AnsiChar;
begin
  Result := mField.GetHasValues(AIndex);
end;

procedure TdxMemFields.SetValue(mField: TdxMemField; AIndex: Integer;
  ABuffer: TRecordBuffer);
begin
  mField.SetHasValue(AIndex, ABuffer <> nil);
  if ABuffer = nil then
    Exit;
  cxCopyData(ABuffer, mField.Values[AIndex], mField.FDataSize);
end;

procedure TdxMemFields.SetHasValue(mField: TdxMemField; AIndex: Integer;
  AValue: AnsiChar);
begin
  mField.SetHasValues(AIndex, AValue);
end;

function TdxMemFields.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TdxMemFields.GetBuffer(Buffer: TRecordBuffer; AIndex: Integer);
begin
  cxCopyData(Pointer(FValues[AIndex]), Buffer, FValuesSize);
end;

procedure TdxMemFields.SetBuffer(Buffer: TRecordBuffer; AIndex: Integer);
begin
  if AIndex = -1 then
    Exit;
  cxCopyData(Buffer, Pointer(FValues[AIndex]), FValuesSize);
end;

function TdxMemFields.GetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer;
  Field: TField): Boolean;
var
  mField: TdxMemField;
begin
  mField := IndexOf(Field);
  Result := (mField <> nil) and mField.GetActiveBuffer(AActiveBuffer, ABuffer);
end;

procedure TdxMemFields.SetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer;
  Field: TField);
var
  mField: TdxMemField;
begin
  mField := IndexOf(Field);
  if mField <> nil then
    mField.SetActiveBuffer(AActiveBuffer, ABuffer);
end;

function TdxMemFields.GetRecordCount: Integer;
begin
  if (FValues = nil) then
    Result := 0
  else
    Result := FValues.Count;
end;

procedure TdxMemFields.InsertRecord(const ABuffer: TRecordBuffer;
  AIndex: Integer; AAppend: Boolean);
var
  I: Integer;
  AData: TRecordBuffer;
  mField: TdxMemField;
begin
  AIndex := IfThen(AAppend, -1, Max(AIndex, 0));
  AData := AllocateRecordMemory(AIndex);
  cxCopyData(ABuffer, AData, FValuesSize);

  for I := 0 to FItems.Count - 1 do
  begin
    mField := Items[I];
    if mField.FNeedAutoInc then
      mField.SetAutoIncValue(mField.GetValueFromBuffer(ABuffer),
        mField.GetDataFromBuffer(AData));
  end;
end;

procedure TdxMemFields.AddField(AField: TField);
begin
  if IndexOf(AField) = nil then
    Add(AField);
end;

procedure TdxMemFields.RemoveField(AField: TField);
var
  mField: TdxMemField;
begin
  mField := IndexOf(AField);
  if mField <> nil then
  begin
    FFieldsDictionary.Remove(AField);
    FItems.Delete(mField.Index);
    mField.Free;
  end;
end;

{ TdxMemBlobStream }

constructor TdxMemBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);

  procedure PrepareMemory;
  var
    ASize: Integer;
    AData: TRecordBuffer;
  begin
    if Mode = bmWrite then
    begin
      Clear;
      FModified := True;
    end
    else
    begin
      FDataSet.GetBlobInfo(FRecordBuffer, FField, AData, ASize);
      SetSize(ASize);
      cxCopyData(AData, Memory, ASize);
      // inherited Write(AData^, ASize);
      // Position := 0;
    end;
  end;

begin
  inherited Create;
  FMode := Mode;
  FField := Field;
  FDataSet := TdxCustomMemData(FField.DataSet);
  if not FDataSet.GetActiveRecBuf(FRecordBuffer) then
    Exit;
  if not FField.Modified and (Mode <> bmRead) then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not(FDataSet.State in dsEditModes) then
      DatabaseError(SNotEditing);
  end;
  FOpened := True;
  PrepareMemory;
end;

destructor TdxMemBlobStream.Destroy;
begin
  if FModified then
  begin
    FDataSet.SetBlobData(FRecordBuffer, FField, Memory, Size);
    if FOpened then
      FField.Modified := True;
    try
      FDataSet.DataEvent(deFieldChange, TdxNativeInt(FField));
    except
      HandleException(self);
    end;
  end;
  inherited;
end;

function TdxMemBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FOpened then
  begin
    Result := inherited Write(Buffer, Count);
    FModified := True;
  end
  else
    Result := 0;
end;

{ TdxReadStringField }

procedure TdxStringFieldReader.CreateField(AMemData: TdxCustomMemData);
begin
  inherited;
  FField.Size := FFieldSize;
end;

function TdxStringFieldReader.ReadFieldValue(AStream: TStream;
  AVerNo: Double): Boolean;
begin
  Result := True;
  // For compatibility with the previous version
  // For some reason we increased the size of string length by one
  // Here we should increase it by one as well
  if ReadFieldSize(AStream) then
  begin
    HasValue := FRecordFieldSize > 1;
    Result := ReadStringFieldValue(AStream);
  end;
end;

function TdxStringFieldReader.GetDataSize(AReadingDataSize: Integer): Integer;
begin
  Result := AReadingDataSize;
  if FDataType = ftWideString then
    Result := (AReadingDataSize + 1) * GetCharSize(FDataType);
end;

function TdxStringFieldReader.GetFieldSize(AReadingDataSize: Integer): Integer;
begin
  Result := AReadingDataSize;
  if FDataType = ftString then
    Dec(Result);
end;

function TdxStringFieldReader.ReadString(AStream: TStream): Boolean;
var
  ATempBuffer: Pointer;
  ACharCount: Integer;
begin
  if FRecordFieldSize > FFieldSize then
    ACharCount := FFieldSize
  else
    ACharCount := FRecordFieldSize;

  if Field <> nil then
  begin
    ATempBuffer := AllocBuferForString(FFieldSize, FDataType);
    try
      ReadBufferFromStream(AStream, ATempBuffer,
        ACharCount * GetCharSize(FDataType));
      AStream.Position := AStream.Position + (FRecordFieldSize - ACharCount) *
        GetCharSize(FDataType);
      Result := AStream.Position <= AStream.Size;
      CopyChars(ATempBuffer, FBuffer, FFieldSize, FDataType);
    finally
      FreeMem(ATempBuffer);
    end;
  end
  else
  begin
    AStream.Position := AStream.Position + ACharCount * GetCharSize(FDataType);
    Result := AStream.Position <= AStream.Size;
  end;
end;

function TdxStringFieldReader.ReadStringFieldValue(AStream: TStream): Boolean;
begin
  Result := True;
  case FDataType of
    ftString, ftGuid:
      Result := ReadString(AStream);
    ftWideString:
      if HasValue then
      begin
        AStream.Position := AStream.Position + 1;
        // for compatibilities with previous versions
        Result := ReadString(AStream);
      end;
  end;
end;

{ TdxCustomMemData }
constructor TdxCustomMemData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TdxMemFields.Create(self);
  FData.FDataSet := self;
  FBookMarks := TdxIntegerList.Create;
  FBlobList := TList.Create;
  FFilterList := TdxIntegerList.Create;
  FDelimiterChar := dxTAB;

  FGotoNearestMin := -1;
  FGotoNearestMax := -1;

  FIndexes := TdxMemIndexes.Create(TdxMemIndex);
  FIndexes.FMemData := self;
  FPersistent := TdxMemPersistent.Create(self);

  CreateRecIDField;
end;

destructor TdxCustomMemData.Destroy;
begin
  Close;

  FreeAndNil(FIndexes);
  BlobClear;
  FreeAndNil(FBlobList);
  FreeAndNil(FBookMarks);
  FreeAndNil(FFilterList);
  FreeAndNil(FData);
  FreeAndNil(FPersistent);

  inherited Destroy;
end;

procedure TdxCustomMemData.CreateRecIDField;
begin
  if (FRecIdField <> nil) then
    Exit;
  FRecIdField := TIntegerField.Create(self);
  with FRecIdField do
  begin
    FieldName := 'RecId';
    DataSet := self;
    Name := self.Name + FieldName;
    Calculated := True;
    Visible := False;
  end;
end;

procedure TdxCustomMemData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not(csLoading in ComponentState) and not(csDestroying in ComponentState)
  then
  begin
    if (AComponent is TField) then
      case Operation of
        opInsert:
          if TField(AComponent).DataSet = self then
            FData.AddField(TField(AComponent));
        opRemove:
          begin
            if (FRecIdField = AComponent) then
              FRecIdField := nil;
            FData.RemoveField(TField(AComponent));
            Indexes.RemoveField(TField(AComponent));
          end;
      end;
  end;
  inherited Notification(AComponent, Operation);
end;

function TdxCustomMemData.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  AIndex: Integer;
begin
  Result := Bookmark <> nil;
  if Result then
  begin
    AIndex := FBookMarks.IndexOf(PInteger(Bookmark)^);
    Result := (AIndex > -1) and (AIndex < Data.RecordCount);
    if FIsFiltered then
      Result := FFilterList.IndexOf(AIndex + 1) > -1;
  end;
end;

function TdxCustomMemData.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
const
  RetCodes: array [Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
var
  r1, r2: Integer;
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    r1 := ReadInteger(Bookmark1);
    r2 := ReadInteger(Bookmark2);
    if r1 = r2 then
      Result := 0
    else
    begin
      if FSortedField <> nil then
      begin
        r1 := FBookMarks.IndexOf(r1);
        r2 := FBookMarks.IndexOf(r2);
      end;
      if r1 > r2 then
        Result := 1
      else
        Result := -1;
    end;
  end;
end;

function TdxCustomMemData.GetStringLength(AFieldType: TFieldType;
  const ABuffer: Pointer): Integer;
begin
  if ABuffer <> nil then
    Result := StrLen(ABuffer, AFieldType)
  else
    Result := 0;
end;

function TdxCustomMemData.InternalLocate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Integer;

  function CompareLocate_SortCaseSensitive: Boolean;
  begin
    Result := ((loCaseInsensitive in Options) and
      (soCaseInsensitive in SortOptions)) or
      (not(loCaseInsensitive in Options) and
      not(soCaseInsensitive in SortOptions));
  end;

  function AllocBufferByVariant(AValue: Variant; AField: TField): Pointer;
  begin
    if VarIsNull(AValue) then
      Result := nil
    else
      Result := AllocBufferForFieldValue(AValue, AField);
  end;

  function CompareLocStr(AmField: TdxMemField; buf1, buf2: TRecordBuffer;
    AStSize: Integer): Integer;
  var
    ATempBuffer: Pointer;
    fStr2Len: Integer;
  begin
    Result := -1;
    fStr2Len := GetStringLength(AmField.FDataType, buf2);
    if fStr2Len = AStSize then
      Result := InternalCompareValues(buf1, buf2, AmField, loCaseInsensitive
        in Options)
    else if (loPartialKey in Options) and (fStr2Len > AStSize) and (AStSize > 0)
    then
    begin
      ATempBuffer := AllocBuferForString(AStSize, AmField.FDataType);
      CopyChars(buf2, ATempBuffer, AStSize, AmField.FDataType);
      Result := InternalCompareValues(buf1, ATempBuffer, AmField,
        loCaseInsensitive in Options);
      FreeMem(ATempBuffer);
    end;
  end;

  function LocateByIndexField(AIndex: TdxMemIndex; AField: TField;
    AValue: Variant): Integer;
  var
    FStSize: Integer;
    mField: TdxMemField;
    ABuf: TRecordBuffer;
  begin
    ABuf := AllocBufferByVariant(AValue, AField);
    try
      VariantToMemDataValue(AValue, ABuf, AField);
      if AIndex = nil then
      begin
        if not GotoNearest(ABuf, Options, Result) and
          not(loPartialKey in Options) then
          Result := -1;
      end
      else
      begin
        if not AIndex.GotoNearest(ABuf, Options, Result) then
          Result := -1;
      end;

      if (Result > -1) then
      begin
        mField := FData.IndexOf(AField);
        if AField.DataType in ftStrings then
        begin
          FStSize := GetStringLength(AField.DataType, ABuf);
          if CompareLocStr(mField, ABuf, mField.Values[Result], FStSize) <> 0
          then
            Result := -1;
        end
        else
        begin
          if (InternalCompareValues(ABuf, mField.Values[Result], mField, False)
            <> 0) then
            Result := -1;
        end;
      end;
    finally
      FreeMem(ABuf);
    end;
  end;

  procedure PrepareLocate;
  begin
    CheckBrowseMode;
    CursorPosChanged;
    UpdateCursorPos;
  end;

  function GetLocateValue(AKeyValues: Variant; AIndex: Integer): Variant;
  begin
    if VarIsArray(AKeyValues) then
      Result := AKeyValues[AIndex]
    else
      Result := AKeyValues;
  end;

  function IsSortedByField(AField: TField): Boolean;
  begin
    Result := (AField = FSortedField) or
      (Indexes.GetIndexByField(AField) <> nil);
  end;

  function GetIndexBySortedField(AField: TField; AKeyValues: Variant): Integer;
  begin
    if AField = FSortedField then
      Result := LocateByIndexField(nil, AField, AKeyValues)
    else
      Result := LocateByIndexField(Indexes.GetIndexByField(AField), AField,
        AKeyValues);
  end;

  procedure CheckFields(AFieldList: TdxMemDataFieldList);
  var
    I: Integer;
  begin
    if AFieldList.Count = 0 then
      raise EdxException.CreateFmt(SFieldNotFound, [KeyFields]);
    for I := 0 to AFieldList.Count - 1 do
      if AFieldList[I] = nil then
        raise EdxException.CreateFmt(SFieldNotFound, [KeyFields])
      else if FData.IndexOf(TField(AFieldList[I])) = nil then
        DatabaseErrorFmt(SBadFieldType, [TField(AFieldList[I]).FieldName]);
  end;

var
  buf: TRecordBuffer;
  AValueList, AmFieldList: TList;
  AFieldList: TdxMemDataFieldList;
  StartId: Integer;
  AField: TField;
  I, j, k, RealRec, RealRecordCount: Integer;
  StSize: Integer;
  IsIndexed: Boolean;
  AKeyValues, AValue: Variant;
begin
  Result := -1;
  PrepareLocate;

  AFieldList := TdxMemDataFieldList.Create;
  try
    GetFieldList(AFieldList, KeyFields);
    CheckFields(AFieldList);
    if (RecordCount = 0) then
      Exit;

    AField := FindField(KeyFields);

    if (AField = nil) and not VarIsArray(KeyValues) then
      Exit;

    if (AField <> nil) and VarIsArray(KeyValues) then
      AKeyValues := KeyValues[0]
    else
      AKeyValues := KeyValues;

    if (AField <> nil) and not FIsFiltered and
      CompareLocate_SortCaseSensitive and IsSortedByField(AField) then
    begin
      Result := GetIndexBySortedField(AField, AKeyValues);
      Exit;
    end;

    AValueList := TList.Create;
    AmFieldList := TList.Create;
    try
      try
        for I := 0 to AFieldList.Count - 1 do
        begin
          AField := TField(AFieldList[I]);
          AValue := GetLocateValue(AKeyValues, I);
          buf := AllocBufferByVariant(AValue, AField);
          AValueList.Add(buf);
          VariantToMemDataValue(AValue, buf, AField);
          AmFieldList.Add(FData.IndexOf(AField));
        end;

        StartId := 0;
        IsIndexed := False;
        if not FIsFiltered then
        begin
          RealRecordCount := FData.RecordCount - 1;
          if CompareLocate_SortCaseSensitive and not VarIsArray(KeyValues) and
            IsSortedByField(TField(AFieldList[0])) then
          begin
            StartId := GetIndexBySortedField(TField(AFieldList[0]), AKeyValues);
            IsIndexed := True;
          end;
        end
        else
          RealRecordCount := FFilterList.Count - 1;

        if StartId > -1 then
        begin
          for I := StartId to RealRecordCount do
          begin
            if not FIsFiltered then
              RealRec := I
            else
              RealRec := Integer(FFilterList[I]) - 1;
            j := 0;
            for k := 0 to AFieldList.Count - 1 do
              if (TField(AFieldList[k]) <> nil) then
              begin
                if (AValueList[k] = nil) then
                begin
                  if TdxMemField(AmFieldList[k]).HasValue[RealRec] then
                    j := -1;
                end
                else
                begin
                  if (TField(AFieldList[k]).DataType in ftStrings) and
                    (Options <> []) then
                  begin
                    StSize := GetStringLength(TField(AFieldList[k]).DataType,
                      TRecordBuffer(AValueList[k]));
                    j := CompareLocStr(TdxMemField(AmFieldList[k]),
                      TRecordBuffer(AValueList[k]), TdxMemField(AmFieldList[k])
                      .Values[RealRec], StSize)
                  end
                  else
                    j := InternalCompareValues(TRecordBuffer(AValueList[k]),
                      TdxMemField(AmFieldList[k]).Values[RealRec],
                      TdxMemField(AmFieldList[k]), loCaseInsensitive
                      in Options);
                end;
                if IsIndexed and (k = 0) and (j <> 0) then
                begin
                  RealRec := -1;
                  Break;
                end;
                if j <> 0 then
                  Break;
              end;

            if RealRec = -1 then
              Break;
            if j = 0 then
            begin
              Result := I;
              Break;
            end;
          end;
        end;
      finally
        for I := 0 to AValueList.Count - 1 do
          FreeMem(Pointer(AValueList[I]));
      end;
    finally
      AmFieldList.Free;
      AValueList.Free;
    end;
  finally
    AFieldList.Free;
  end;
end;

function TdxCustomMemData.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  AIndex: Integer;
begin
  AIndex := InternalLocate(KeyFields, KeyValues, Options);
  Result := AIndex > -1;
  if Result then
  begin
    Inc(AIndex);
    if RecNo <> AIndex then
      RecNo := AIndex
    else
      Resync([]);
  end;
end;

procedure AddStrings(AStrings: TStrings; S: string);
var
  P: Integer;
begin
  repeat
    P := Pos(';', S);
    if P = 0 then
    begin
      AStrings.Add(S);
      Break;
    end
    else
    begin
      AStrings.Add(Copy(S, 1, P - 1));
      Delete(S, 1, P);
    end;
  until False;
end;

function TdxCustomMemData.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;

  function GetLookupValue(AField: TField; ALookupIndex: Integer): Variant;
  var
    mField: TdxMemField;
  begin
    if (AField = nil) then
      Result := Null
    else
    begin
      if not(AField is TBlobField) then
      begin
        mField := FData.IndexOf(AField);
        if (mField <> nil) and mField.HasValue[ALookupIndex] then
          Result := GetVariantValue(mField.Values[ALookupIndex], AField)
        else
          Result := Null;
      end
      else
        Result := GetBlobData(FBlobList[ALookupIndex], AField.OffSet);
    end;
  end;

var
  FLookupIndex: Integer;
  I: Integer;
  AStrings: TStrings;
begin
  FLookupIndex := InternalLocate(KeyFields, KeyValues, []);
  if (FLookupIndex > -1) then
  begin
    if FIsFiltered then
      FLookupIndex := Integer(FFilterList[FLookupIndex]) - 1;
    I := Pos(';', ResultFields);
    if (I < 1) then
      Result := GetLookupValue(FindField(ResultFields), FLookupIndex)
    else
    begin
      AStrings := TStringList.Create;
      try
        AddStrings(AStrings, ResultFields);
        Result := VarArrayCreate([0, AStrings.Count - 1], varVariant);
        for I := 0 to AStrings.Count - 1 do
          Result[I] := GetLookupValue(FindField(AStrings[I]), FLookupIndex);
      finally
        AStrings.Free;
      end;
    end;
  end
  else
    Result := Null;
end;

function TdxCustomMemData.GetRecNoByFieldValue(Value: Variant;
  FieldName: string): Integer;
begin
  Result := InternalLocate(FieldName, Value, []);
  if Result > -1 then
    Inc(Result);
end;

function TdxCustomMemData.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result := GetNoByFieldType(AType) <> -1;
end;

function TdxCustomMemData.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := inherited GetFieldClass(FieldType);
end;

procedure TdxCustomMemData.InternalOpen;
var
  I: Integer;
begin
  for I := 0 to FieldCount - 1 do
    if not SupportedFieldType(Fields[I].DataType) then
    begin
      DatabaseErrorFmt('Unsupported field type: %s', [Fields[I].FieldName]);
      Exit;
    end;

  FillBookMarks;

  FCurRec := -1;
  FFilterCurRec := -1;

  FRecInfoOfs := 0;
  for I := 0 to FieldCount - 1 do
    if not Fields[I].IsBlob then
      Inc(FRecInfoOfs, GetDataSize(Fields[I]) + 1);

  FRecBufSize := FRecInfoOfs + SizeOf(TdxRecInfo);
  BookmarkSize := SizeOf(Integer);

  InternalInitFieldDefs;

  if IsDefaultFields(self) then
    CreateFields;

  for I := 0 to FieldCount - 1 do
    if not Fields[I].IsBlob then
      FData.Add(Fields[I]);

  FData.FValues := TList.Create;
  BindFields(True);
  FActive := True;
  MakeSort;
  Indexes.CheckFields;
end;

procedure TdxCustomMemData.InternalClose;
begin
  FData.Clear;
  FBookMarks.Clear;
  FFilterList.Clear;
  BlobClear;
  FSortedField := nil;

  if IsDefaultFields(self) then
    DestroyFields;

  FLastBookmark := 0;
  FCurRec := -1;
  FFilterCurRec := -1;

  FActive := False;
end;

function TdxCustomMemData.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

procedure TdxCustomMemData.InternalInitFieldDefs;
var
  I: Integer;
begin
  FieldDefs.Clear;
  for I := 0 to FieldCount - 1 do
    with Fields[I] do
      if Calculated or Lookup then
        FData.FCalcFields.Add(Fields[I])
      else
        FieldDefs.Add(FieldName, DataType, Size, Required);
end;

procedure TdxCustomMemData.InternalHandleException;
begin
  HandleException(self);
end;

procedure TdxCustomMemData.InternalGotoBookmark(Bookmark: Pointer);
var
  AIndex, IndexF: Integer;
begin
  AIndex := FBookMarks.IndexOf(PInteger(Bookmark)^);
  if AIndex > -1 then
  begin
    if FIsFiltered then
    begin
      IndexF := FFilterList.IndexOf(AIndex + 1);
      if IndexF > -1 then
      begin
        FFilterCurRec := IndexF;
        FCurRec := AIndex;
      end;
    end
    else
      FCurRec := AIndex
  end
  else
    DatabaseError('Bookmark not found');
end;

procedure TdxCustomMemData.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(@PdxRecInfo(Buffer + FRecInfoOfs).Bookmark);
end;

function TdxCustomMemData.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PdxRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

procedure TdxCustomMemData.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PdxRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

procedure TdxCustomMemData.GetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  PInteger(Data)^ := PdxRecInfo(Buffer + FRecInfoOfs).Bookmark;
end;

{$IFDEF DELPHI17}

procedure TdxCustomMemData.GetBookmarkData(Buffer: TRecordBuffer;
  Data: TBookmark);
begin
  Assert(Length(Data) = BookmarkSize);
  GetBookmarkData(Buffer, @Data[0]);
end;
{$ENDIF}

procedure TdxCustomMemData.SetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  PdxRecInfo(Buffer + FRecInfoOfs).Bookmark := PInteger(Data)^;
end;

{$IFDEF DELPHI17}

procedure TdxCustomMemData.SetBookmarkData(Buffer: TRecordBuffer;
  Data: TBookmark);
begin
  Assert(Length(Data) = BookmarkSize);
  SetBookmarkData(Buffer, @Data[0]);
end;
{$ENDIF}

function TdxCustomMemData.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;
begin
  if ActiveBuffer <> nil then
  begin
    cxCopyData(ActiveBuffer, Buffer, RecordSize);
    Result := True;
  end
  else
    Result := False;
end;

function TdxCustomMemData.GetRecordSize: Word;
begin
  Result := FRecInfoOfs;
end;

procedure TdxCustomMemData.Loaded;
begin
  inherited Loaded;
  Indexes.AfterMemdataLoaded;
  if Active and (Persistent.Option = poLoad) then
    Persistent.LoadData;
end;

function TdxCustomMemData.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize + BlobFieldCount * SizeOf(Pointer));
  InitializeBlobData(GetRecordData(Result));
end;

procedure TdxCustomMemData.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FinalizeBlobData(GetRecordData(Buffer));
  FreeMem(Buffer);
  Buffer := nil;
end;

function TdxCustomMemData.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;

  function CalculateRecNo(var ARecNo: Integer): TGetResult;
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        if ARecNo >= RecordCount - 1 then
          Result := grEOF
        else
          Inc(ARecNo);
      gmPrior:
        if ARecNo <= 0 then
          Result := grBOF
        else
          Dec(ARecNo);
      gmCurrent:
        if (ARecNo < 0) or (ARecNo >= RecordCount) then
          Result := grError;
    end
  end;

begin
  if (FData = nil) then
  begin
    Result := grError;
    Exit;
  end;
  if FData.RecordCount < 1 then
    Result := grEOF
  else
  begin
    if not FIsFiltered then
      Result := CalculateRecNo(FCurRec)
    else
    begin
      Result := CalculateRecNo(FFilterCurRec);
      if (Result = grOK) then
        FCurRec := Integer(FFilterList[FFilterCurRec]) - 1
      else
        FCurRec := -1;
    end;
    if Result = grOK then
    begin
      FData.GetBuffer(Buffer, FCurRec);
      with PdxRecInfo(Buffer + FRecInfoOfs)^ do
      begin
        BookmarkFlag := bfCurrent;
        Bookmark := Integer(FBookMarks[FCurRec])
      end;
      GetMemBlobData(Buffer);
      GetCalcFields(Buffer);
    end
    else if (Result = grError) and DoCheck then
      DatabaseError('No Records');
  end;
end;

procedure TdxCustomMemData.InternalInitRecord(Buffer: TRecordBuffer);
begin
  cxZeroMemory(Buffer, FRecInfoOfs);
  FinalizeBlobData(GetRecordData(Buffer));
  InitializeBlobData(GetRecordData(Buffer));
end;

function TdxCustomMemData.GetActiveRecBuf(var ARecordBuffer
  : TRecordBuffer): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        ARecordBuffer := nil
      else
        ARecordBuffer := ActiveBuffer;
    dsEdit, dsInsert:
      ARecordBuffer := ActiveBuffer;
    dsCalcFields:
      ARecordBuffer := CalcBuffer;
  else
    ARecordBuffer := nil;
  end;
  Result := ARecordBuffer <> nil;
end;

function TdxCustomMemData.GetFieldData(Field: TField; {$IFDEF DELPHI18}var
  {$ENDIF} Buffer: TValueBuffer): Boolean;
var
  RecBuf: TRecordBuffer;
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;

  if Field.IsBlob then
    Result := Length(GetBlobData(RecBuf, Field)) > 0
  else
    Result := FData.GetActiveBuffer(RecBuf,
      ValueBufferToRecordBuffer(Buffer), Field);
end;

function TdxCustomMemData.GetFieldData(Field: TField; {$IFDEF DELPHI18}var
  {$ENDIF} Buffer: TValueBuffer; NativeFormat: Boolean): Boolean;
begin
  if (Field.DataType = ftWideString) then
    Result := GetFieldData(Field, Buffer)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat)
end;

procedure TdxCustomMemData.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  RecBuf: TRecordBuffer;
begin
  if not(State in dsWriteModes) then
    DatabaseError(SNotEditing, self);
  if not GetActiveRecBuf(RecBuf) then
    Exit;

  Field.Validate(Buffer);

  FData.SetActiveBuffer(RecBuf, ValueBufferToRecordBuffer(Buffer), Field);

  if not(State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, TdxNativeInt(Field));
end;

procedure TdxCustomMemData.SetFieldData(Field: TField; Buffer: TValueBuffer;
  NativeFormat: Boolean);
begin
  if (Field.DataType = ftWideString) then
    SetFieldData(Field, Buffer)
  else
    inherited SetFieldData(Field, Buffer, NativeFormat)
end;

function TdxCustomMemData.GetStateFieldValue(State: TDataSetState;
  Field: TField): Variant;
var
  mField: TdxMemField;
begin
  if (State = dsOldValue) and Modified and (self.State = dsEdit) then
  begin
    mField := FData.IndexOf(Field);
    if mField.HasValue[self.CurRec] then
      Result := GetVariantValue(mField.Values[self.CurRec], Field)
    else
      Result := Null;
  end
  else
    Result := inherited GetStateFieldValue(State, Field);
end;

procedure TdxCustomMemData.InternalFirst;
begin
  FCurRec := -1;
  FFilterCurRec := -1;
end;

procedure TdxCustomMemData.InternalLast;
begin
  if not FIsFiltered then
    FCurRec := FData.RecordCount
  else
  begin
    FFilterCurRec := RecordCount;
    FCurRec := FData.RecordCount;
  end;
end;

procedure TdxCustomMemData.DoAfterCancel;
begin
  if not IsBinaryDataLoading then
    inherited DoAfterCancel;
end;

procedure TdxCustomMemData.DoAfterClose;
begin
  if not IsBinaryDataLoading then
    inherited DoAfterClose;
end;

procedure TdxCustomMemData.DoAfterInsert;
begin
  if not IsBinaryDataLoading then
    inherited DoAfterInsert;
end;

procedure TdxCustomMemData.DoAfterOpen;
begin
  if not IsBinaryDataLoading then
  begin
    if (Persistent.Option = poActive) then
      Persistent.LoadData;
    inherited DoAfterOpen;
  end;
end;

procedure TdxCustomMemData.DoAfterPost;
begin
  if not IsBinaryDataLoading then
    inherited DoAfterPost;
end;

procedure TdxCustomMemData.DoBeforeClose;
begin
  if not IsBinaryDataLoading then
    inherited DoBeforeClose;
end;

procedure TdxCustomMemData.DoBeforeInsert;
begin
  if not IsBinaryDataLoading then
    inherited;
end;

procedure TdxCustomMemData.DoBeforeOpen;
begin
  if not IsBinaryDataLoading then
    inherited;
end;

procedure TdxCustomMemData.DoBeforePost;
begin
  if not IsBinaryDataLoading then
    inherited DoBeforePost;
end;

procedure TdxCustomMemData.DoOnNewRecord;
begin
  if not IsBinaryDataLoading then
    inherited DoOnNewRecord;
end;

procedure TdxCustomMemData.InternalAddFilterRecord;
var
  I: Integer;
begin
  if InternalIsFiltering then
  begin
    I := FFilterCurRec;
    if I < 0 then
      I := 0;
    if (I >= FFilterList.Count) then
    begin
      if (FCurRec = -1) then
        FCurRec := 0;
      FFilterList.Add(FCurRec + 1);
      FFilterCurRec := FFilterList.Count - 1;
    end
    else
    begin
      FFilterList.Insert(I, FCurRec + 1);
      FFilterCurRec := I;
      Inc(I);
      while I < FFilterList.Count do
      begin
        FFilterList[I] := FFilterList[I] + 1;
        Inc(I);
      end;
    end;
  end;
end;

function CompareRecordBuffersByFields(ARecordBuffer1, ARecordBuffer2: Pointer;
  ASortingInfos: TdxMemSortingInfos): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(ASortingInfos) - 1 do
  begin
    if ASortingInfos[I].MemField <> nil then
      Result := ASortingInfos[I].MemField.CompareRecordBuffers(ARecordBuffer1,
        ARecordBuffer2, soCaseInsensitive in ASortingInfos[I].Options);
    if Result <> 0 then
    begin
      if soDesc in ASortingInfos[I].Options then
        Result := -Result;
      Break;
    end;
  end;
end;

var
  ACompareOptions: TdxMemSortingInfos;

function CompareValuesByFields(ARecordBuffer1, ARecordBuffer2: Pointer)
  : Integer;
begin
  Result := CompareRecordBuffersByFields(ARecordBuffer1, ARecordBuffer2,
    ACompareOptions);
end;

procedure TdxCustomMemData.MakeRecordSort;
var
  NewCurRec, ATestIndex: Integer;

  function GetRecordBuffer(ARecordIndex: Integer): TRecordBuffer;
  begin
    Result := FData.FValues[ARecordIndex];
  end;

  procedure ExchangeLists;
  var
    I, Index, AMovedCount: Integer;
  begin
    if FIsFiltered and (FFilterList.Count > 0) then
    begin
      AMovedCount := 0;
      if FCurRec < NewCurRec then
      begin
        for I := FCurRec + 1 to NewCurRec do
        begin
          Index := FFilterList.IndexOf(I + 1);
          if Index > -1 then
          begin
            FFilterList[Index] := FFilterList[Index] - 1;
            Inc(AMovedCount);
          end;
        end;
      end
      else
      begin
        for I := FCurRec - 1 downto NewCurRec do
        begin
          Index := FFilterList.IndexOf(I + 1);
          if Index > -1 then
          begin
            FFilterList[Index] := FFilterList[Index] + 1;
            Dec(AMovedCount);
          end;
        end;
      end;
      FFilterList[FFilterCurRec] := NewCurRec + 1;
      if AMovedCount <> 0 then
      begin
        FFilterList.Move(FFilterCurRec, FFilterCurRec + AMovedCount);
        FFilterCurRec := FFilterCurRec + AMovedCount;
      end;
    end;
    FData.FValues.Move(FCurRec, NewCurRec);
    FBookMarks.Move(FCurRec, NewCurRec);
    if FBlobList.Count > 0 then
      FBlobList.Move(FCurRec, NewCurRec);
    FCurRec := NewCurRec;
  end;

begin
  if IsBinaryDataLoading or not FActive or (FData.RecordCount < 2) then
    Exit;
  if FSortedField <> nil then
  begin
    if (FCurRec > 0) and (CompareRecordBuffersByFields(GetRecordBuffer(FCurRec),
      GetRecordBuffer(FCurRec - 1), FSortingInfos) = -1) then
      FGotoNearestMax := FCurRec - 1
    else if (FCurRec < FData.RecordCount - 1) and
      (CompareRecordBuffersByFields(GetRecordBuffer(FCurRec),
      GetRecordBuffer(FCurRec + 1), FSortingInfos) = 1) then
      FGotoNearestMin := FCurRec + 1;

    NewCurRec := InternalGotoNearest(FData.FValues, FSortingInfos,
      GetRecordBuffer(FCurRec));

    FGotoNearestMax := -1;
    FGotoNearestMin := -1;
    if NewCurRec = -1 then
    begin
      if FCurRec = 0 then
        ATestIndex := 1
      else
        ATestIndex := 0;
      if CompareRecordBuffersByFields(GetRecordBuffer(FCurRec),
        GetRecordBuffer(ATestIndex), FSortingInfos) = -1 then
        NewCurRec := ATestIndex
      else
        NewCurRec := FData.RecordCount - 1;
    end;
    if NewCurRec = -1 then
      NewCurRec := 0;
    if (FCurRec < NewCurRec) and
      (CompareRecordBuffersByFields(GetRecordBuffer(NewCurRec),
      GetRecordBuffer(FCurRec), FSortingInfos) = 1) then
      NewCurRec := NewCurRec - 1;
    if NewCurRec = -1 then
      NewCurRec := 0;
    if NewCurRec = FData.RecordCount then
      NewCurRec := FData.RecordCount - 1;
    ExchangeLists;
  end;
end;

procedure TdxCustomMemData.UpdateRecordFilteringAndSorting
  (AIsMakeSort: Boolean);
begin
  if (FSortedField <> nil) and AIsMakeSort then
    MakeRecordSort;
  UpdateFilterRecord;
  if State = dsEdit then
    Indexes.UpdateRecord(Data.FValues[FCurRec])
  else
    Indexes.SetIsDirty;
end;

function TdxCustomMemData.InternalIsFiltering: Boolean;
begin
  Result := Assigned(OnFilterRecord) and Filtered;
end;

procedure TdxCustomMemData.InternalPost;
var
  AIsMakeSort: Boolean;
begin
  inherited InternalPost;
  FSaveChanges := True;
  AIsMakeSort := FSortedField <> nil;
  if State = dsEdit then
  begin
    AIsMakeSort := AIsMakeSort and
      (CompareRecordBuffersByFields(FData.FValues[FCurRec], ActiveBuffer,
      FSortingInfos) <> 0);
    FData.SetBuffer(ActiveBuffer, FCurRec);
  end
  else
  begin
    Inc(FLastBookmark);
    FCurRec := Max(FCurRec, 0);

    if BlobFieldCount > 0 then
      FBlobList.Insert(FCurRec, nil);

    FData.InsertRecord(ActiveBuffer, FCurRec, False);
    FBookMarks.Add(FLastBookmark);
    InternalAddFilterRecord;
  end;

  if BlobFieldCount > 0 then
    SetMemBlobData(ActiveBuffer);

  UpdateRecordFilteringAndSorting(AIsMakeSort);
end;

procedure TdxCustomMemData.InternalInsert;
var
  buf: TRecordBuffer;
  Value: Integer;
  mField: TdxMemField;
begin
  if FRecIdField <> nil then
  begin
    mField := FData.IndexOf(FRecIdField);
    if mField <> nil then
    begin
      buf := mField.GetDataFromBuffer(ActiveBuffer);
      Value := mField.FNextAutoIncValue;
      WriteByte(buf, 1);
      WriteInteger(buf, Value, 1);
    end;
  end;
end;

procedure TdxCustomMemData.InternalAddRecord(ABuffer: Pointer;
  AAppend: Boolean);
begin
  FSaveChanges := True;
  Inc(FLastBookmark);
  if AAppend then
    InternalLast;
  FData.InsertRecord(ABuffer, FCurRec, AAppend);
  FBookMarks.Add(FLastBookmark);

  if BlobFieldCount > 0 then
  begin
    if AAppend then
      FBlobList.Add(nil)
    else
      FBlobList.Insert(Max(FCurRec, 0), nil);
    SetMemBlobData(ABuffer);
  end;

  InternalAddFilterRecord;

  UpdateRecordFilteringAndSorting(True);
end;

{$IFDEF DELPHI17}

procedure TdxCustomMemData.InternalAddRecord(ABuffer: TRecordBuffer;
  AAppend: Boolean);
begin
  InternalAddRecord(Pointer(ABuffer), AAppend);
end;
{$ENDIF}

procedure TdxCustomMemData.InternalDelete;
var
  I: Integer;
  P: TRecordBuffer;
begin
  FSaveChanges := True;
  Indexes.DeleteRecord(FData.FValues.List[FCurRec]);
  FData.DeleteRecord(FCurRec);
  FBookMarks.Delete(FCurRec);

  if BlobFieldCount > 0 then
  begin
    P := FBlobList[FCurRec];
    if P <> nil then
    begin
      FinalizeBlobData(P);
      FreeMem(Pointer(FBlobList[FCurRec]));
    end;
    FBlobList.Delete(FCurRec);
  end;

  if not FIsFiltered then
  begin
    if FCurRec >= FData.RecordCount then
      Dec(FCurRec);
  end
  else
  begin
    FFilterList.Delete(FFilterCurRec);
    if FFilterCurRec < FFilterList.Count then
      for I := FFilterCurRec to FFilterList.Count - 1 do
        FFilterList[I] := FFilterList[I] - 1;
    if FFilterCurRec >= RecordCount then
      Dec(FFilterCurRec);
    if (FFilterCurRec > -1) then
      FCurRec := FFilterList[FFilterCurRec]
    else
      FCurRec := -1;
  end;
end;

procedure TdxCustomMemData.GetCalcFields(Buffer: TRecordBuffer);
begin
  if (RecIdField <> nil) and (CalcFieldsSize > RecIdField.DataSize + 1) or InternalCalcFields
  then
  begin
{$WARNINGS OFF}
    inherited GetCalcFields(Buffer);
{$WARNINGS ON}
  end;
end;

function TdxCustomMemData.GetRecordCount: Integer;
begin
  if Not FIsFiltered then
    Result := FData.RecordCount
  else
    Result := FFilterList.Count;
end;

function TdxCustomMemData.GetRecNo: Integer;
begin
  if State <> dsCalcFields then
    UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result := 1
  else
  begin
    if Not FIsFiltered then
      Result := FCurRec + 1
    else
      Result := FFilterCurRec + 1;
  end;
end;

function TdxCustomMemData.InternalSetRecNo(const Value: Integer): Integer;
begin
  if not FIsFiltered then
    Result := Value - 1
  else
  begin
    FFilterCurRec := Value - 1;
    Result := Integer(FFilterList[FFilterCurRec]) - 1;
  end;
end;

procedure TdxCustomMemData.SetRecNo(Value: Integer);
var
  NewCurRec: Integer;
begin
  if Active then
    CheckBrowseMode;
  if (Value > 0) and (Value <= FData.RecordCount) then
  begin
    NewCurRec := InternalSetRecNo(Value);
    if (NewCurRec <> FCurRec) then
    begin
      DoBeforeScroll;
      FCurRec := NewCurRec;
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end;
end;

procedure TdxCustomMemData.SetFilteredRecNo(Value: Integer);
var
  Index: Integer;
begin
  Index := FFilterList.IndexOf(Value);
  if Index >= 0 then
    SetRecNo(Index + 1);
end;

function TdxCustomMemData.GetCanModify: Boolean;
begin
  Result := not FReadOnly or IsBinaryDataLoading;
end;

procedure TdxCustomMemData.ClearCalcFields(Buffer: TRecordBuffer);
var
  I: Integer;
  mField: TdxMemField;
begin
  if (Data.FCalcFields.Count < 2) or (State = dsCalcFields) then
    Exit;
  for I := 1 to Data.FCalcFields.Count - 1 do
  begin
    mField := FData.IndexOf(TField(FData.FCalcFields[I]));
    WriteByte(Buffer, 0, mField.FOffSet);
  end;
end;

procedure TdxCustomMemData.SetFiltered(Value: Boolean);
var
  AOldFiltered: Boolean;
begin
  AOldFiltered := Filtered;
  inherited SetFiltered(Value);
  if AOldFiltered <> Filtered then
    UpdateFilters;
end;

function TdxCustomMemData.GetAnsiStringValue(const ABuffer: TRecordBuffer)
  : AnsiString;
begin
  Result := AnsiString(PAnsiChar(ABuffer));
end;

function TdxCustomMemData.GetWideStringValue(const ABuffer: TRecordBuffer)
  : WideString;
begin
  Result := WideString(PWideChar(ABuffer));
end;

function TdxCustomMemData.GetVariantValue(const ABuffer: TRecordBuffer;
  AField: TField): Variant;
var
  AFakeDataSet: TFakeDataSet;
  AFakeField: TField;
begin
  case AField.DataType of
    ftString, ftGuid:
      Result := GetAnsiStringValue(ABuffer);
    ftWideString:
      Result := GetWideStringValue(ABuffer);
  else
{$WARNINGS OFF}
    AFakeDataSet := TFakeDataSet.Create(nil);
{$WARNINGS ON}
    try
      AFakeField := TFieldClass(AField.ClassType).Create(AFakeDataSet);
      AFakeField.FieldName := AField.FieldName;
      AFakeField.DataSet := AFakeDataSet;
      AFakeDataSet.FBuffer := ABuffer;
      AFakeDataSet.InitFieldDefsFromFields;
      AFakeDataSet.BindFields(True);
      Result := AFakeField.Value;
    finally
      AFakeDataSet.Free;
    end;
  end;
end;

function TdxCustomMemData.GetIntegerValue(const ABuffer: TRecordBuffer;
  ADataType: TFieldType): Integer;

type
  PData = ^Data;

  Data = record
    case Integer of
      0:
        (Small: Smallint);
      1:
        (W: Word);
      2:
        (Long: Integer);
      3:
        (Short: ShortInt);
      4:
        (B: Byte)
  end;

var
  ptr: PData;
begin
  Assert(ABuffer <> nil);
  ptr := PData(@ABuffer[0]);
  case ADataType of
    ftSmallint:
      Result := ptr.Small;
    ftWord:
      Result := ptr.W;
    ftShortint:
      Result := ptr.Short;
    ftByte:
      Result := ptr.B;
  else
    Result := ptr.Long;
  end;
end;

function TdxCustomMemData.GetLargeIntValue(const ABuffer: TRecordBuffer)
  : Largeint;
begin
  Move(ABuffer^, Result, SizeOf(Largeint));
end;

function TdxCustomMemData.GetFloatValue(const ABuffer: TRecordBuffer): Double;
begin
  Move(ABuffer^, Result, SizeOf(Double));
end;

function TdxCustomMemData.GetExtendedValue(const ABuffer: TRecordBuffer)
  : Extended;
begin
  Move(ABuffer^, Result, SizeOf(Extended));
end;

function TdxCustomMemData.GetCurrencyValue(const ABuffer: TRecordBuffer)
  : System.Currency;
begin
  Move(ABuffer^, Result, SizeOf(System.Currency));
end;

function TdxCustomMemData.GetDateTimeValue(const ABuffer: TRecordBuffer;
  AField: TField): TDateTime;
begin
{$WARNINGS OFF}
  DataConvert(AField, ABuffer, @Result, False);
{$WARNINGS ON}
end;

function TdxCustomMemData.GetTimeStampValue(const ABuffer: TRecordBuffer;
  AField: TField): TSQLTimeStamp;
begin
{$WARNINGS OFF}
  DataConvert(AField, ABuffer, @Result, False);
{$WARNINGS ON}
end;

function TdxCustomMemData.CompareValues(const ABuffer1, ABuffer2: TRecordBuffer;
  AMemField: TdxMemField; ASortOptions: TdxSortOptions): Integer;
begin
  Result := InternalCompareValues(ABuffer1, ABuffer2, AMemField,
    soCaseInsensitive in ASortOptions);
end;

function TdxCustomMemData.CompareValues(const ABuffer1, ABuffer2: TRecordBuffer;
  AMemField: TdxMemField): Integer;
begin
  Result := CompareValues(ABuffer1, ABuffer2, AMemField, FSortOptions);
end;

function TdxCustomMemData.CompareValues(const ABuffer1, ABuffer2: TRecordBuffer;
  AField: TField): Integer;
begin
  Result := CompareValues(ABuffer1, ABuffer2, Data.IndexOf(AField));
end;

function TdxCustomMemData.InternalCompareValues(const ABuffer1,
  ABuffer2: Pointer; AMemField: TdxMemField; AIsCaseInsensitive: Boolean;
  ACount: Integer = -1): Integer;

  function CompareStrings: Integer;
  const
    AIgnoreCaseFlag: array [Boolean] of Cardinal = (0, NORM_IGNORECASE);
  var
    AFlags: Cardinal;
  begin
    AFlags := AIgnoreCaseFlag[AIsCaseInsensitive];
    case AMemField.FDataType of
      ftString, ftGuid:
        Result := CompareStringA(LOCALE_USER_DEFAULT, AFlags, ABuffer1, ACount,
          ABuffer2, ACount) - 2;
      ftWideString:
        Result := CompareStringW(LOCALE_USER_DEFAULT, AFlags, ABuffer1, ACount,
          ABuffer2, ACount) - 2;
    else
      Result := 0;
    end;
    if (Result <> 0) then
      Result := Result div abs(Result);
  end;

var
  AInt1, AInt2: Integer;
  ADouble1, ADouble2: Double;
  ABool1, ABool2: WordBool;
  ALargeint1, ALargeint2: Largeint;
  ADWord1, ADWord2: LongWord;
  AExtended1, AExtended2: Extended;
  AVar1, AVar2: Variant;
begin
  if (ABuffer1 = nil) or (ABuffer2 = nil) then
  begin
    if ABuffer1 = ABuffer2 then
      Result := 0
    else if ABuffer1 = nil then
      Result := -1
    else
      Result := 1;
    Exit;
  end;
  case AMemField.FDataType of
    ftString, ftWideString, ftGuid:
      Result := CompareStrings;
    ftShortint, ftByte, ftSmallint, ftInteger, ftWord, ftAutoInc:
      begin
        AInt1 := GetIntegerValue(ABuffer1, AMemField.FDataType);
        AInt2 := GetIntegerValue(ABuffer2, AMemField.FDataType);
        if AInt1 > AInt2 then
          Result := 1
        else if AInt1 < AInt2 then
          Result := -1
        else
          Result := 0;
      end;
    ftLargeInt:
      begin
        ALargeint1 := GetLargeIntValue(ABuffer1);
        ALargeint2 := GetLargeIntValue(ABuffer2);
        if ALargeint1 > ALargeint2 then
          Result := 1
        else if ALargeint1 < ALargeint2 then
          Result := -1
        else
          Result := 0;
      end;
    ftFloat, ftCurrency:
      begin
        ADouble1 := GetFloatValue(ABuffer1);
        ADouble2 := GetFloatValue(ABuffer2);
        if ADouble1 > ADouble2 then
          Result := 1
        else if ADouble1 < ADouble2 then
          Result := -1
        else
          Result := 0;
      end;
    ftDate, ftTime, ftDateTime:
      begin
        ADouble1 := GetDateTimeValue(ABuffer1, AMemField.FField);
        ADouble2 := GetDateTimeValue(ABuffer2, AMemField.FField);
        if ADouble1 > ADouble2 then
          Result := 1
        else if ADouble1 < ADouble2 then
          Result := -1
        else
          Result := 0;
      end;
    ftBoolean:
      begin
        ABool1 := ReadBoolean(ABuffer1);
        ABool2 := ReadBoolean(ABuffer2);
        if ABool1 > ABool2 then
          Result := 1
        else if ABool1 < ABool2 then
          Result := -1
        else
          Result := 0;
      end;
    ftLongWord:
      begin
        ADWord1 := GetIntegerValue(ABuffer1, AMemField.FDataType);
        ADWord2 := GetIntegerValue(ABuffer2, AMemField.FDataType);
        if ADWord1 > ADWord2 then
          Result := 1
        else if ADWord1 < ADWord2 then
          Result := -1
        else
          Result := 0;
      end;
    ftExtended:
      begin
        AExtended1 := GetExtendedValue(ABuffer1);
        AExtended2 := GetExtendedValue(ABuffer2);
        if AExtended1 > AExtended2 then
          Result := 1
        else if AExtended1 < AExtended2 then
          Result := -1
        else
          Result := 0;
      end;
    ftBCD, ftTimeStamp, ftFMTBcd: // complex
      begin
        AVar1 := GetVariantValue(ABuffer1, AMemField.FField);
        AVar2 := GetVariantValue(ABuffer2, AMemField.FField);
        Result := VarCompare(AVar1, AVar2);
      end;
  else
    Result := 0;
  end;
end;

function TdxCustomMemData.GetCalcBuffer: TdxMemDataBuffer;
begin
  Result := TdxMemDataBuffer(inherited CalcBuffer);
end;

function TdxCustomMemData.GetRecIdMemField: TdxMemField;
begin
  Result := Data.IndexOf(RecIdField)
end;

function TdxCustomMemData.AllocBufferForFieldValue(AValue: Variant;
  AField: TField): Pointer;
begin
  Result := AllocMem(GetDataSize(AValue, AField));
end;

function TdxCustomMemData.AllocBufferForField(AField: TField): Pointer;
begin
  Result := AllocMem(GetDataSize(Null, AField));
end;

function TdxCustomMemData.GetSortOptions: TdxSortOptions;
begin
  Result := FSortOptions;
end;

procedure TdxCustomMemData.PopulateValueList(const AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  AList.Capacity := FData.FValues.Count;
  for I := 0 to FData.FValues.Count - 1 do
    AList.Add(FData.FValues[I]);
end;

procedure TdxCustomMemData.SetSortedFields(Value: string);
begin
  if FSortedFieldNames <> Value then
  begin
    FSortedFieldNames := Value;
    MakeSort;
  end
end;

procedure TdxCustomMemData.SetSortOptions(Value: TdxSortOptions);
begin
  if (FSortOptions <> Value) then
  begin
    FSortOptions := Value;
    MakeSort;
  end;
end;

procedure TdxCustomMemData.SetIndexes(Value: TdxMemIndexes);
begin
  FIndexes.Assign(Value);
end;

procedure TdxCustomMemData.SetPersistent(Value: TdxMemPersistent);
begin
  FPersistent.Assign(Value);
end;

procedure TdxCustomMemData.MakeSort;
var
  ASynchronizationLists: TList;
  AFieldNames: TStringList;
  I, j: Integer;
  AFieldName: string;
  AColonPos: Integer;
begin
  FSortedField := nil;
  SetLength(FSortingInfos, 0);
  if IsBinaryDataLoading or not FActive or (FSortedFieldNames = '') then
    Exit;

  AFieldNames := TStringList.Create;
  try
    AFieldNames.Delimiter := ';';
    AFieldNames.DelimitedText := FSortedFieldNames;
    SetLength(FSortingInfos, AFieldNames.Count + 1);
    for I := 0 to AFieldNames.Count - 1 do
    begin
      FSortingInfos[I].Options := FSortOptions;
      AFieldName := AFieldNames[I];
      AColonPos := Pos(':', AFieldName);
      if AColonPos > 0 then
      begin
        for j := AColonPos to Length(AFieldName) do
        begin
          case AFieldName[j] of
            'n', 'N':
              Include(FSortingInfos[I].Options, soCaseInsensitive);
            'd', 'D':
              Include(FSortingInfos[I].Options, soDesc);
            'a', 'A':
              Exclude(FSortingInfos[I].Options, soDesc);
          end;
        end;
        AFieldName := Copy(AFieldName, 1, AColonPos - 1);
      end;
      FSortingInfos[I].MemField := Data.IndexOf(FindField(AFieldName));
    end;
    FSortingInfos[AFieldNames.Count].Options := FSortOptions;
    FSortingInfos[AFieldNames.Count].MemField := RecIdMemField;
  finally
    AFieldNames.Free;
  end;

  for I := 0 to Length(FSortingInfos) - 2 do
    if FSortingInfos[I].MemField <> nil then
    begin
      FSortedField := FSortingInfos[I].MemField.Field;
      Break;
    end;

  if FSortedField <> nil then
  begin
    UpdateCursorPos;
    ASynchronizationLists := TList.Create;
    try
      ASynchronizationLists.Add(FBookMarks);
      if FBlobList.Count > 0 then
        ASynchronizationLists.Add(FBlobList);
      DoSort(FData.FValues, ASynchronizationLists, FSortingInfos);
    finally
      ASynchronizationLists.Free;
    end;
    UpdateFilters;
    if not FIsFiltered then
      SetRecNo(FCurRec + 1);
    if Active then
      Resync([]);
  end;
end;

procedure TdxCustomMemData.DoSort(ASortedList, ASynchronizationLists: TList;
  AmdField: TdxMemField; ASortOptions: TdxSortOptions);
var
  ASortingInfos: TdxMemSortingInfos;
begin
  SetLength(ASortingInfos, 2);

  ASortingInfos[0].MemField := AmdField;
  ASortingInfos[0].Options := ASortOptions;
  ASortingInfos[1].MemField := RecIdMemField;
  ASortingInfos[1].Options := ASortOptions;
  DoSort(ASortedList, ASynchronizationLists, ASortingInfos);
end;

procedure TdxCustomMemData.DoSort(ASortedList, ASynchronizationLists: TList;
  ASortingInfos: TdxMemSortingInfos);

  procedure SynchronizeList(ADataList: TcxDoublyLinkedDataIndexedList;
    AList: TList);
  var
    ATempList: TList;
    I: Integer;
    AItem: TcxDoublyLinkedIndexedData;
  begin
    ATempList := TList.Create;
    try
      ATempList.Capacity := AList.Count;
      AItem := TcxDoublyLinkedIndexedData(ADataList.Last);
      for I := AList.Count - 1 downto 0 do
      begin
        ATempList.List[I] := AList.List[AItem.Index];
        AItem := TcxDoublyLinkedIndexedData(AItem.Prev);
      end;
      for I := 0 to AList.Count - 1 do
        AList.List[I] := ATempList.List[I];
    finally
      ATempList.Free;
    end;
  end;

var
  ADataList: TcxDoublyLinkedDataIndexedList;
  I: Integer;
begin
  ACompareOptions := ASortingInfos;
  ADataList := TcxDoublyLinkedDataIndexedList.Create;
  try
    ADataList.PopulateFromList(ASortedList);
    ADataList.Sort(CompareValuesByFields);
    ADataList.PopulateToList(ASortedList);
    if ASynchronizationLists <> nil then
      for I := 0 to ASynchronizationLists.Count - 1 do
        SynchronizeList(ADataList, TList(ASynchronizationLists[I]));
  finally
    ADataList.Free;
  end;
end;

function TdxCustomMemData.InternalGotoNearest(AValues: TList; AField: TField;
  const ABuffer: TRecordBuffer; ASortOptions: TdxSortOptions;
  ALocateOptions: TLocateOptions; out AIndex: Integer): Boolean;
var
  AMemField: TdxMemField;

  function CompareValues(AIndex: Integer; APartial: Boolean = False): Integer;
  var
    ABuffer2: TRecordBuffer;
  begin
    ABuffer2 := AMemField.GetValueFromBuffer(AValues[AIndex]);
    if APartial and (AMemField.FDataType in ftStrings) then
      Result := InternalCompareValues(ABuffer, ABuffer2, AMemField,
        soCaseInsensitive in ASortOptions,
        GetStringLength(AMemField.FDataType, ABuffer))
    else
      Result := self.CompareValues(ABuffer, ABuffer2, AMemField, ASortOptions);
  end;

var
  AMin, AMax, cmp, ADirection: Integer;
  APartial: Boolean;
begin
  Result := False;
  AIndex := -1;
  AMemField := Data.IndexOf(AField);
  if (AValues.Count = 0) or (AMemField = nil) then
    Exit;

  if FGotoNearestMin = -1 then
    AMin := 0
  else
    AMin := FGotoNearestMin;
  if FGotoNearestMax = -1 then
    AMax := AValues.Count - 1
  else
    AMax := FGotoNearestMax;

  ADirection := IfThen(soDesc in ASortOptions, -1, 1);
  APartial := loPartialKey in ALocateOptions;

  if (soDesc in ASortOptions) then
  begin
    cmp := CompareValues(AMax, APartial);
    if cmp <= 0 then
      AIndex := AMax;
  end
  else
  begin
    cmp := CompareValues(AMin, APartial);
    if cmp <= 0 then
      AIndex := AMin;
  end;

  if AIndex = -1 then
  begin
    repeat
      if ((AMax - AMin) = 1) then
      begin
        if (AMin = AIndex) then
          AMin := AMax;
        if (AMax = AIndex) then
          AMax := AMin;
      end;
      AIndex := AMin + ((AMax - AMin) div 2);
      cmp := CompareValues(AIndex, APartial) * ADirection;
      case cmp of
        0:
          AMin := AMax;
        1:
          AMin := AIndex;
        -1:
          AMax := AIndex;
      end;
    until (AMin = AMax);
  end;

  case cmp of
    0:
      begin
        while ((AIndex > 0) and (CompareValues(AIndex - 1, APartial) = 0)) do
          Dec(AIndex);
        Result := CompareValues(AIndex) = 0;
      end;
    1:
      begin
        case ADirection of
          1:
            if (AIndex < AValues.Count - 1) then
              Inc(AIndex);
          -1:
            while (AIndex < AValues.Count - 1) and
              (CompareValues(AIndex, APartial) = -1) do
              Inc(AIndex);
        end;
      end;
  end;
end;

function TdxCustomMemData.InternalGotoNearest(AValues: TList;
  ACompareInfos: TdxMemSortingInfos;
  const ARecordBuffer: TRecordBuffer): Integer;

  function CompareValues(ARecordIndex: Integer): Integer;
  var
    ARecordBuffer2: TRecordBuffer;
    I: Integer;
  begin
    ARecordBuffer2 := AValues[ARecordIndex];
    Result := 0;
    for I := 0 to Length(ACompareInfos) - 1 do
    begin
      if ACompareInfos[I].MemField <> nil then
        Result := ACompareInfos[I].MemField.CompareRecordBuffers(ARecordBuffer,
          ARecordBuffer2, soCaseInsensitive in ACompareInfos[I].Options);
      if Result <> 0 then
      begin
        if soDesc in ACompareInfos[I].Options then
          Result := -Result;
        Break;
      end;
    end;
  end;

var
  AMin, AMax, cmp: Integer;
begin
  Result := -1;

  if FGotoNearestMin <> -1 then
  begin
    AMin := FGotoNearestMin;
    if CompareValues(AMin) <= 0 then
      Result := AMin;
  end
  else
    AMin := 0;

  if FGotoNearestMax <> -1 then
  begin
    AMax := FGotoNearestMax;
    if CompareValues(AMax) >= 0 then
      Result := AMax;
  end
  else
    AMax := AValues.Count - 1;

  if Result = -1 then
  begin
    repeat
      if (AMax - AMin) = 1 then
      begin
        if AMin = Result then
          AMin := AMax;
        if AMax = Result then
          AMax := AMin;
      end;
      Result := AMin + ((AMax - AMin) div 2);
      cmp := CompareValues(Result);
      case cmp of
        0:
          AMin := AMax;
        1:
          AMin := Result;
        -1:
          AMax := Result;
      end;
    until (AMin = AMax);
    case cmp of
      0:
        while ((Result > 0) and (CompareValues(Result - 1) = 0)) do
          Dec(Result);
      1:
        if Result < AValues.Count - 1 then
          Inc(Result);
    end;
  end;
end;

function TdxCustomMemData.GotoNearest(const AValueBuffer: TRecordBuffer;
  ALocateOptions: TLocateOptions; out AIndex: Integer): Boolean;
begin
  AIndex := -1;
  Result := False;
  if IsBinaryDataLoading then
    Exit;

  if (FSortedField <> nil) and (FData.FValues.Count > 0) then
    Result := InternalGotoNearest(FData.FValues, FSortedField, AValueBuffer,
      FSortOptions, ALocateOptions, AIndex);
end;

procedure TdxCustomMemData.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  inherited SetOnFilterRecord(Value);
  UpdateFilters;
end;

procedure TdxCustomMemData.UpdateFilterRecord;
var
  Accepted: Boolean;
begin
  if not InternalIsFiltering then
    Exit;
  Accepted := True;
  OnFilterRecord(self, Accepted);
  if not Accepted and (FFilterCurRec > -1) and
    (FFilterCurRec < FFilterList.Count) then
  begin
    FFilterList.Delete(FFilterCurRec);
    FIsFiltered := True;
  end;
end;

procedure TdxCustomMemData.UpdateFilters;
var
  Accepted, OldControlsDisabled: Boolean;
  ACount: Integer;
begin
  if not Active then
    Exit;
  OldControlsDisabled := ControlsDisabled;
  if not OldControlsDisabled then
    DisableControls;

  if not FProgrammedFilter then
  begin
    FFilterList.Clear;
    if InternalIsFiltering then
    begin
      FIsFiltered := False;
      First;
      ACount := 1;
      while not EOF do
      begin
        Accepted := True;
        OnFilterRecord(self, Accepted);
        if Accepted then
          FFilterList.Add(ACount);
        Inc(ACount);
        Next;
      end;
    end;
  end;

  ClearBuffers;

  FIsFiltered := FProgrammedFilter or
    ((FFilterList.Count <> FData.RecordCount) and (FFilterList.Count > 0)) or
    InternalIsFiltering;
  if (FIsFiltered) then
  begin
    if (RecordCount > 0) then
      RecNo := 1;
    if FFilterCurRec >= FFilterList.Count then
      FFilterCurRec := FFilterList.Count - 1;
    Resync([]);
  end
  else
    First;

  if not OldControlsDisabled then
    EnableControls;
end;

function TdxCustomMemData.GetValueCount(AFieldName: string;
  AValue: Variant): Integer;
var
  ABuf: TRecordBuffer;
  I: Integer;
  AMemField: TdxMemField;
  AField: TField;
begin
  Result := -1;
  AField := FindField(AFieldName);
  if (AField = nil) then
    Exit;

  AMemField := FData.IndexOf(AField);
  if not VarIsEmpty(AValue) and not VarIsNull(AValue) then
  begin
    ABuf := AllocBufferForField(AField);
    try
      if VariantToMemDataValue(AValue, ABuf, AField) and (AMemField <> nil) then
      begin
        Result := 0;
        for I := 0 to FData.RecordCount - 1 do
          if CompareValues(ABuf, AMemField.Values[I], AMemField) = 0 then
            Inc(Result);
      end;
    finally
      FreeMem(ABuf);
    end;
  end
  else
  begin
    Result := 0;
    for I := 0 to FData.RecordCount - 1 do
      if not AMemField.HasValue[I] then
        Inc(Result);
  end;
end;

procedure TdxCustomMemData.FillBookMarks;
var
  I: Integer;
begin
  FBookMarks.Clear;
  for I := 1 to FData.RecordCount do
    FBookMarks.Add(I);
  FLastBookmark := FData.RecordCount;
end;

procedure TdxCustomMemData.MoveCurRecordTo(AIndex: Integer);
var
  I, FRealRec, FRealIndex: Integer;
begin
  if (AIndex > 0) and (AIndex <= RecordCount) and (RecNo <> AIndex) then
  begin
    if not FIsFiltered then
    begin
      FRealRec := FCurRec;
      FRealIndex := AIndex - 1;
    end
    else
    begin
      FRealRec := Integer(FFilterList[FFilterCurRec]) - 1;
      FRealIndex := Integer(FFilterList[AIndex - 1]) - 1;
    end;
    FData.FValues.Move(FRealRec, FRealIndex);
    FBookMarks.Move(FRealRec, FRealIndex);
    if FBlobList.Count > 0 then
      FBlobList.Move(FRealRec, FRealIndex);
    if FIsFiltered then
    begin
      if RecNo < AIndex then
      begin
        for I := RecNo to AIndex - 1 do
          FFilterList[I] := FFilterList[I] - 1;
      end
      else
      begin
        for I := RecNo - 2 downto AIndex - 1 do
          FFilterList[I] := FFilterList[I] + 1;
      end;
      FFilterList[FFilterCurRec] := FRealIndex + 1;
      FFilterList.Move(FFilterCurRec, AIndex - 1);
    end;
    SetRecNo(AIndex);
  end;
end;

procedure TdxCustomMemData.LoadFromTextFile(const AFileName: string);
var
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  try
    AStrings.LoadFromFile(AFileName);
    LoadFromStrings(AStrings);
  finally
    AStrings.Free;
  end;
end;

procedure TdxCustomMemData.SaveToTextFile(const AFileName: string);
var
  AStrings: TStringList;
begin
  if not Active then
    Exit;

  AStrings := TStringList.Create;
  try
    SaveToStrings(AStrings);
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

procedure TdxCustomMemData.LoadFromStrings(AStrings: TStrings);

  procedure ParseString(S: string; AStrigns: TStrings);
  var
    ADelimiterPos, AOffset, ACount: Integer;
    AParsedString: string;
  begin
    AStrigns.Clear;
    AOffset := 1;
    repeat
      ADelimiterPos := PosEx(FDelimiterChar, S, AOffset);
      if ADelimiterPos = 0 then
        ACount := MaxInt
      else
        ACount := ADelimiterPos - AOffset;
      AParsedString := Copy(S, AOffset, ACount);
      AStrigns.Add(AParsedString);
      AOffset := ADelimiterPos + 1;
    until ADelimiterPos = 0;
  end;

var
  AValues: TStringList;
  I, j: Integer;
  AFields: TList;
  AField: TField;
  AError: Boolean;
begin
  if (AStrings.Count = 0) then
    Exit;

  AError := False;
  DisableControls;
  try
    FLoadFlag := True;
    try
      Close;
      Open;
      AValues := TStringList.Create;
      AFields := TList.Create;
      try

        for I := 0 to AStrings.Count - 1 do
        begin
          ParseString(AStrings[I], AValues);
          if I = 0 then
          begin
            for j := 0 to AValues.Count - 1 do
            begin
              AField := FindField(AValues[j]);
              if (AField <> nil) and (AField.Calculated or AField.Lookup or
                AField.IsBlob) then
                AField := nil;
              AFields.Add(AField);
            end;
          end
          else if AFields.Count = AValues.Count then
          begin
            Append;
            for j := 0 to AValues.Count - 1 do
              if (AFields[j] <> nil) and (AValues[j] <> '') then
                TField(AFields[j]).Text := AValues[j];
            Post;
          end
          else
            AError := True;
        end;
      finally
        AFields.Free;
        AValues.Free;
      end;
    finally
      FLoadFlag := False;
    end;
    First;
    MakeSort;
  finally
    EnableControls;
  end;
  if AError then
    raise EdxException.Create(cxGetResourceString(@cxSDataInvalidStreamFormat));
end;

procedure TdxCustomMemData.SaveToStrings(AStrings: TStrings);
type
  FieldDataType = (fdtValue, fdtName);

  function GetFieldData(AFields: TList; AType: FieldDataType): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to AFields.Count - 1 do
    begin
      if I <> 0 then
        Result := Result + FDelimiterChar;
      case AType of
        fdtValue:
          Result := Result + TField(AFields[I]).Text;
        fdtName:
          Result := Result + TField(AFields[I]).FieldName;
      end;
    end;
  end;

var
  I: Integer;
  ABookMark: TBookmark;
  AList: TList;
begin
  if not Active then
    Exit;

  AList := TList.Create;
  try
    DisableControls;
    ABookMark := GetBookmark;
    try
      for I := 0 to FieldCount - 1 do
        if not Fields[I].Calculated and not Fields[I].Lookup and not Fields[I].IsBlob
        then
          AList.Add(Fields[I]);
      AStrings.Add(GetFieldData(AList, fdtName));
      First;
      while not EOF do
      begin
        AStrings.Add(GetFieldData(AList, fdtValue));
        Next;
      end;
      GotoBookmark(ABookMark);
    finally
      FreeBookmark(ABookMark);
      EnableControls;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxCustomMemData.CreateFieldsFromBinaryFile(const AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    CreateFieldsFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomMemData.CreateFieldsFromStream(Stream: TStream);
var
  AMemStreamReader: TdxMemDataStreamReader;
begin
  Close;
  AMemStreamReader := TdxMemDataStreamReader.Create(self, Stream);
  try
    AMemStreamReader.CreateFields(self);
  finally
    AMemStreamReader.Free;
  end;
end;

procedure TdxCustomMemData.LoadFromStream(AStream: TStream);
var
  AMemReader: TdxMemDataStreamReader;
begin
  DisableControls;
  try
    FLoadFlag := True;
    try
      Close;
      Open;
      AMemReader := TdxMemDataStreamReader.Create(self, AStream);
      try
        AMemReader.LoadData;
      finally
        AMemReader.Free;
      end;
    finally
      FLoadFlag := False;
    end;
    FillBookMarks;
    MakeSort;
    UpdateFilters;
    if not FIsFiltered then
      First;
    Refresh;
  finally
    EnableControls;
  end;
end;

procedure TdxCustomMemData.LoadFromBinaryFile(const AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomMemData.SaveToStream(AStream: TStream);
var
  AMemDataStreamWriter: TdxMemDataStreamWriter;
begin
  if not Active then
    Exit;
  CheckBrowseMode;
  AMemDataStreamWriter := TdxMemDataStreamWriter.Create(self, AStream);
  try
    AMemDataStreamWriter.SaveData;
  finally
    AMemDataStreamWriter.Free;
  end;
end;

procedure TdxCustomMemData.SaveToBinaryFile(const AFileName: string);
var
  AMemoryStream: TMemoryStream;
begin
  if not Active then
    Exit;

  AMemoryStream := TMemoryStream.Create;
  try
    SaveToStream(AMemoryStream);
    AMemoryStream.SaveToFile(AFileName);
  finally
    AMemoryStream.Free;
  end;
end;

function TdxCustomMemData.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TdxMemBlobStream.Create(TBlobField(Field), Mode);
end;

procedure TdxCustomMemData.CloseBlob(Field: TField);
begin
  if (FBlobList <> nil) and (FCurRec >= 0) and (FCurRec < RecordCount) and
    (State in dsEditModes) then
    SetBlobData(ActiveBuffer, Field, GetBlobData(FBlobList[FCurRec],
      Field.OffSet))
  else
    SetBlobData(ActiveBuffer, Field, '');
end;

procedure TdxCustomMemData.BlobClear;
var
  I: Integer;
  P: TRecordBuffer;
begin
  if BlobFieldCount > 0 then
    for I := 0 to FBlobList.Count - 1 do
    begin
      P := FBlobList[I];
      if P <> nil then
      begin
        FinalizeBlobData(P);
        FreeMem(FBlobList[I]);
      end;
    end;
  FBlobList.Clear;
end;

function TdxCustomMemData.ActiveBuffer: TdxMemDataBuffer;
begin
  Result := TdxMemDataBuffer(inherited ActiveBuffer);
end;

procedure TdxCustomMemData.InitializeBlobData(ABuffer: TRecordBuffer);
begin
  if BlobFieldCount = 0 then
    Exit;
  cxZeroMemory(ABuffer, BlobFieldCount * SizeOf(TdxNativeInt));
end;

procedure TdxCustomMemData.FinalizeBlobData(ABuffer: TRecordBuffer);
var
  I: Integer;
  ptr: TRecordBuffer;
begin
  if BlobFieldCount = 0 then
    Exit;
  for I := 0 to BlobFieldCount - 1 do
  begin
    ptr := ShiftPointer(ABuffer, I * SizeOf(TdxNativeInt));
    ptr := ReadPointer(ptr);
    FreeMem(ptr);
  end;
end;

function TdxCustomMemData.GetRecordData(ARecordBuffer: TRecordBuffer)
  : TRecordBuffer;
begin
  Result := ShiftPointer(ARecordBuffer, FRecBufSize);
end;

function TdxCustomMemData.GetBlobInfo(ADataBuffer: TRecordBuffer;
  AOffset: Integer; out ABlobData: TRecordBuffer;
  out ABlobSize: Integer): Boolean;
var
  APtr: TRecordBuffer;
begin
  APtr := GetBlobPointer(ADataBuffer, AOffset);
  Result := APtr <> nil;
  if Result then
  begin
    ABlobSize := ReadInteger(APtr);
    ABlobData := ShiftPointer(APtr, SizeOf(ABlobSize));
  end
  else
  begin
    ABlobSize := 0;
    ABlobData := nil;
  end;
end;

function TdxCustomMemData.GetBlobInfo(ARecordBuffer: TRecordBuffer;
  AField: TField; out ABlobData: TRecordBuffer; out ABlobSize: Integer)
  : Boolean;
begin
  Result := GetBlobInfo(GetRecordData(ARecordBuffer), AField.OffSet, ABlobData,
    ABlobSize);
end;

function TdxCustomMemData.GetBlobPointer(ADataBuffer: TRecordBuffer;
  AOffset: Integer): TRecordBuffer;
var
  APtr: TRecordBuffer;
begin
  Result := nil;
  if ADataBuffer <> nil then
  begin
    APtr := ShiftPointer(ADataBuffer, AOffset * SizeOf(TRecordBuffer));
    Result := ReadPointer(APtr);
  end;
end;

function TdxCustomMemData.GetBlobData(ADataBuffer: TRecordBuffer;
  AOffset: Integer): TMemBlobData;
var
  ASize: Integer;
  AData: TRecordBuffer;
begin
  Result := '';
  if GetBlobInfo(ADataBuffer, AOffset, AData, ASize) and (ASize > 0) then
  begin
    SetLength(Result, ASize);
    cxCopyData(AData, @Result[1], ASize);
  end;
end;

function TdxCustomMemData.GetBlobData(ARecordBuffer: TRecordBuffer;
  AField: TField): TMemBlobData;
begin
  Result := GetBlobData(GetRecordData(ARecordBuffer), AField.OffSet);
end;

procedure TdxCustomMemData.SetInternalBlobData(ADataBuffer: TRecordBuffer;
  AOffset: Integer; AValue: TRecordBuffer; ASize: Integer);
var
  ABufPtr, ADataPtr: TRecordBuffer;
begin
  ABufPtr := ShiftPointer(ADataBuffer, AOffset * SizeOf(TRecordBuffer));
  ADataPtr := GetBlobPointer(ABufPtr, 0);
  if ADataPtr <> nil then
  begin
    FreeMem(ADataPtr);
    ADataPtr := nil;
  end;
  if ASize > 0 then
  begin
    ADataPtr := AllocMem(ASize + SizeOf(TRecordBuffer));
    WriteInteger(ADataPtr, ASize);
    cxCopyData(AValue, ADataPtr, 0, SizeOf(ASize), ASize);
  end;
  WritePointer(ABufPtr, ADataPtr);
end;

procedure TdxCustomMemData.SetInternalBlobData(ADataBuffer: TRecordBuffer;
  AOffset: Integer; AValue: TMemBlobData);
begin
  SetInternalBlobData(ADataBuffer, AOffset, TRecordBuffer(AValue),
    Length(AValue));
end;

procedure TdxCustomMemData.SetBlobData(ADataBuffer: TRecordBuffer;
  AOffset: Integer; const AValue: TRecordBuffer; ASize: Integer);
begin
  if (GetRecordData(ActiveBuffer) <> ADataBuffer) or (State = dsFilter) then
    Exit;
  SetInternalBlobData(ADataBuffer, AOffset, AValue, ASize);
end;

procedure TdxCustomMemData.SetBlobData(ARecordBuffer: TRecordBuffer;
  AField: TField; const AValue: TMemBlobData);
begin
  SetBlobData(GetRecordData(ARecordBuffer), AField.OffSet,
    TRecordBuffer(AValue), Length(AValue));
end;

procedure TdxCustomMemData.SetBlobData(ARecordBuffer: TRecordBuffer;
  AField: TField; const AValue: TRecordBuffer; ASize: Integer);
begin
  SetBlobData(GetRecordData(ARecordBuffer), AField.OffSet, AValue, ASize);
end;

function TdxCustomMemData.GetActiveBlobData(Field: TField): TMemBlobData;
var
  I: Integer;
begin
  Result := '';
  I := FCurRec;
  if (I < 0) and (RecordCount > 0) then
    I := 0
  else if I >= RecordCount then
    I := RecordCount - 1;
  if (I >= 0) and (I < RecordCount) then
  begin
    if FIsFiltered then
      I := Integer(FFilterList[FFilterCurRec]) - 1;
    Result := GetBlobData(FBlobList[I], Field.OffSet);
  end;
end;

procedure TdxCustomMemData.GetMemBlobData(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  if BlobFieldCount > 0 then
  begin
    if (FCurRec >= 0) and (FCurRec < FData.RecordCount) then
    begin
      for I := 0 to BlobFieldCount - 1 do
        SetInternalBlobData(GetRecordData(Buffer), I,
          GetBlobData(FBlobList[FCurRec], I))
    end;
  end;
end;

procedure TdxCustomMemData.SetMemBlobData(Buffer: TRecordBuffer);
var
  P: TRecordBuffer;
  I, Pos: Integer;
begin
  if BlobFieldCount > 0 then
  begin
    Pos := FCurRec;
    if (Pos < 0) and (FData.RecordCount > 0) then
      Pos := 0
    else if Pos >= FData.RecordCount then
      Pos := FData.RecordCount - 1;
    if (Pos >= 0) and (Pos < FData.RecordCount) then
    begin
      P := FBlobList[Pos];
      if P = nil then
      begin
        P := AllocMem(BlobFieldCount * SizeOf(Pointer));
        InitializeBlobData(P);
      end;
      for I := 0 to BlobFieldCount - 1 do
        SetInternalBlobData(P, I, GetBlobData(GetRecordData(Buffer), I));
      FBlobList[Pos] := P;
    end;
  end;
end;

procedure TdxCustomMemData.CreateFieldsFromDataSet(ADataSet: TDataSet;
  AOwner: TComponent);
begin
  if (ADataSet <> nil) and (ADataSet.FieldCount > 0) then
  begin
    Close;
    while FieldCount > 1 do
      Fields[FieldCount - 1].Free;
    AddFieldsFromDataSet(ADataSet, AOwner);
  end;
end;

procedure TdxCustomMemData.AddFieldsFromDataSet(ADataSet: TDataSet;
  AOwner: TComponent);

  procedure AssignFieldProperties(ADestField, ASrcField: TField);
  begin
    ADestField.DisplayLabel := ASrcField.DisplayLabel;
    if TFieldAccess(ASrcField).GetDefaultWidth <> ASrcField.DisplayWidth then
      ADestField.DisplayWidth := ASrcField.DisplayWidth;
    ADestField.EditMask := ASrcField.EditMask;
    ADestField.FieldName := ASrcField.FieldName;
    ADestField.Visible := ASrcField.Visible;

    // custom properties
    if (ADestField is TStringField) or (ADestField is TBlobField) then
      ADestField.Size := ASrcField.Size;
    if ADestField is TBlobField then
      TBlobField(ADestField).BlobType := TBlobField(ASrcField).BlobType;
    if ADestField is TNumericField then
    begin
      TNumericField(ADestField).EditFormat := TNumericField(ASrcField)
        .EditFormat;
      TNumericField(ADestField).DisplayFormat := TNumericField(ASrcField)
        .DisplayFormat;
      if ADestField is TFloatField then
      begin
        TFloatField(ADestField).Currency := TFloatField(ASrcField).Currency;
        TFloatField(ADestField).Precision := TFloatField(ASrcField).Precision;
      end;
    end;
    if ADestField is TDateTimeField then
      TDateTimeField(ADestField).DisplayFormat := TDateTimeField(ASrcField)
        .DisplayFormat;

    ADestField.Calculated := ASrcField.Calculated;
    ADestField.Lookup := ASrcField.Lookup;
    if ASrcField.Lookup then
    begin
      ADestField.KeyFields := ASrcField.KeyFields;
      ADestField.LookupDataSet := ASrcField.LookupDataSet;
      ADestField.LookupKeyFields := ASrcField.LookupKeyFields;
      ADestField.LookupResultField := ASrcField.LookupResultField;
    end;
  end;

  function IsFieldExist(const AFieldName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Fields.Count - 1 do
    begin
      Result := SameText(Fields[I].FieldName, AFieldName);
      if Result then
        Break;
    end;
  end;

var
  AField: TField;
  I: Integer;
begin
  if AOwner = nil then
    AOwner := self;

  if ADataSet.FieldCount > 0 then
  begin
    for I := 0 to ADataSet.FieldCount - 1 do
    begin
      if SupportedFieldType(ADataSet.Fields[I].DataType) and
        ((RecIdField = nil) or (CompareText(ADataSet.Fields[I].FieldName,
        RecIdField.FieldName) <> 0)) and
        not IsFieldExist(ADataSet.Fields[I].FieldName) then
      begin
        AField := DefaultFieldClasses[ADataSet.Fields[I].DataType]
          .Create(AOwner);
        AField.Name := GetValidName(self, Name + ADataSet.Fields[I].FieldName);
        AssignFieldProperties(AField, ADataSet.Fields[I]);
        AField.DataSet := self;
      end;
    end;
  end
  else
  begin
    ADataSet.FieldDefs.Update;
    for I := 0 to ADataSet.FieldDefs.Count - 1 do
    begin
      if SupportedFieldType(ADataSet.FieldDefs[I].DataType) and
        not IsFieldExist(ADataSet.FieldDefs[I].Name) then
      begin
        AField := DefaultFieldClasses[ADataSet.Fields[I].DataType]
          .Create(AOwner);
        AField.Name := Name + ADataSet.FieldDefs[I].Name;
        AField.FieldName := ADataSet.FieldDefs[I].Name;
        if (AField is TStringField) or (AField is TBlobField) then
          AField.Size := ADataSet.FieldDefs[I].Size;
        AField.DataSet := self;
      end;
    end;
  end;
end;

procedure TdxCustomMemData.CopyFromDataSet(DataSet: TDataSet);
begin
  Close;
  CreateFieldsFromDataSet(DataSet);
  LoadFromDataSet(DataSet);
end;

procedure TdxCustomMemData.LoadFromDataSet(DataSet: TDataSet);

  function CanAssignTo(ASource, ADestination: TFieldType): Boolean;
  begin
    Result := ASource = ADestination;
    if not Result then
      Result := (ASource = ftAutoInc) and (ADestination = ftInteger);
  end;

  procedure SetAutoIncFlag;
  var
    I: Integer;
  begin
    for I := 1 to Data.FItems.Count - 1 do
    begin
      if Data.Items[I].FDataType = ftAutoInc then
        Data.Items[I].FNeedAutoInc := True;
    end;
  end;

var
  I: Integer;
  AField: TField;
  mField: TdxMemField;
  AFields: array of TField;
begin
  if (DataSet = nil) or (DataSet.FieldCount = 0) or not DataSet.Active then
    Exit;
  if FieldCount < 2 then
    CreateFieldsFromDataSet(DataSet);
  DataSet.DisableControls;
  DisableControls;
  try
    DataSet.First;
    Open;

    SetLength(AFields, DataSet.FieldCount);
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      AField := FindField(DataSet.Fields[I].FieldName);
      if (AField <> nil) and CanAssignTo(DataSet.Fields[I].DataType,
        AField.DataType) then
        AFields[I] := AField
      else
        AFields[I] := nil;
    end;

    while not DataSet.EOF do
    begin
      Append;
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        AField := AFields[I];
        if AField <> nil then
        begin
          SetFieldValue(DataSet.Fields[I], AField);
          if AField.DataType = ftAutoInc then
          begin
            mField := Data.IndexOf(AField);
            mField.FNeedAutoInc := False;
            if mField.FNextAutoIncValue <= AField.AsInteger then
              mField.FNextAutoIncValue := AField.AsInteger + 1;
          end;
        end;
      end;
      Post;
      DataSet.Next;
    end;

    SetAutoIncFlag;
  finally
    EnableControls;
    DataSet.EnableControls;
  end;
end;

{ TdxReadStringFieldVer191 (1.91) }

function TdxStringFieldReaderVer191.GetDataSize(AReadingDataSize
  : Integer): Integer;
begin
  Result := (AReadingDataSize + 1) * GetCharSize(FDataType);
end;

function TdxStringFieldReaderVer191.GetFieldSize(AReadingDataSize
  : Integer): Integer;
begin
  Result := AReadingDataSize;
end;

{ TdxMemIndexes }
function TdxMemIndexes.GetOwner: TPersistent;
begin
  Result := FMemData;
end;

procedure TdxMemIndexes.SetIsDirty;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxMemIndex(Items[I]).IsDirty := True;
end;

procedure TdxMemIndexes.DeleteRecord(ABuffer: TRecordBuffer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxMemIndex(Items[I]).DeleteRecord(ABuffer);
end;

procedure TdxMemIndexes.UpdateRecord(ABuffer: TRecordBuffer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxMemIndex(Items[I]).UpdateRecord(ABuffer);
end;

procedure TdxMemIndexes.RemoveField(AField: TField);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (TdxMemIndex(Items[I]).FField = AField) then
    begin
      TdxMemIndex(Items[I]).FField := nil;
      TdxMemIndex(Items[I]).IsDirty := True;
    end;
end;

procedure TdxMemIndexes.CheckFields;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    TdxMemIndex(Items[I]).FField :=
      FMemData.FindField(TdxMemIndex(Items[I]).FieldName);
    TdxMemIndex(Items[I]).IsDirty := True;
  end;
end;

procedure TdxMemIndexes.AfterMemdataLoaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TdxMemIndex(Items[I]).SetFieldNameAfterMemdataLoaded;
end;

function TdxMemIndexes.Add: TdxMemIndex;
begin
  Result := TdxMemIndex(inherited Add);
end;

function TdxMemIndexes.GetIndexByField(AField: TField): TdxMemIndex;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (TdxMemIndex(Items[I]).FField = AField) then
    begin
      Result := TdxMemIndex(Items[I]);
      Break;
    end;
end;

{ TdxReadStringFieldVer190 (1.90) }

function TdxStringFieldReaderVer190.ReadFieldValue(AStream: TStream;
  AVerNo: Double): Boolean;
begin
  Result := True;
  AStream.Read(FHasValue, 1);
  if HasValue then
  begin
    ReadFieldSize(AStream);
    Result := ReadString(AStream);
  end;
end;

{ TdxMemIndex }
constructor TdxMemIndex.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  fIsDirty := True;
  FValues := TList.Create;
  FValueIndexes := TdxIntegerList.Create;
end;

destructor TdxMemIndex.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FValueIndexes);

  inherited Destroy;
end;

procedure TdxMemIndex.Assign(Source: TPersistent);
begin
  if Source is TdxMemIndex then
  begin
    FieldName := TdxMemIndex(Source).FieldName;
    SortOptions := TdxMemIndex(Source).SortOptions;
  end
  else
    inherited Assign(Source);
end;

procedure TdxMemIndex.Prepare;
var
  I: Integer;
  mField: TdxMemField;
  ASynchronizationLists: TList;
begin
  if not IsDirty or (FField = nil) then
    Exit;

  FValueIndexes.Clear;
  mField := MemData.Data.IndexOf(FField);
  if mField <> nil then
  begin
    MemData.PopulateValueList(FValues);
    FValueIndexes.Capacity := FValues.Capacity;
    for I := 0 to FValues.Count - 1 do
      FValueIndexes.Add(I);
    ASynchronizationLists := TList.Create;
    try
      ASynchronizationLists.Add(FValueIndexes);
      MemData.DoSort(FValues, ASynchronizationLists, mField, SortOptions);
    finally
      ASynchronizationLists.Free;
    end;
    IsDirty := False;
  end;
end;

function TdxMemIndex.GotoNearest(const ABuffer: TRecordBuffer;
  ALocateOptions: TLocateOptions; out AIndex: Integer): Boolean;
begin
  Result := False;
  Prepare;
  if IsDirty then
    Exit;
  Result := MemData.InternalGotoNearest(FValues, FField, ABuffer, SortOptions,
    ALocateOptions, AIndex);
  if Result then
    AIndex := Integer(FValueIndexes[AIndex]);
end;

procedure TdxMemIndex.SetIsDirty(Value: Boolean);
begin
  if not Value and (FField = nil) then
    Value := True;
  if (fIsDirty <> Value) then
  begin
    fIsDirty := Value;
    if (Value) then
      FValues.Clear;
  end;
end;

procedure TdxMemIndex.DeleteRecord(ABuffer: TRecordBuffer);
begin
  IsDirty := True;
end;

{
  procedure TdxMemIndex.UpdateRecord(ABuffer: TRecordBuffer);
  var
  i, Index: Integer;
  mField: TdxMemField;
  begin
  if fIsDirty then
  exit;
  i := FValues.IndexOf(ABuffer);
  if i > -1 then
  begin
  Index := GetMemData.Data.FValues.IndexOf(FValueS[i]);
  if Index > - 1 then
  begin
  mField := GetMemData.Data.IndexOf(fField);
  if ((Index = 0)
  or (GetMemData.InternalCompareValues(mField.Values[Index - 1],
  mField.Values[Index], mField, soCaseinsensitive in SortOptions) <= 0))
  and ((Index = GetMemData.RecordCount - 1)
  or (GetMemData.InternalCompareValues(mField.Values[Index],
  mField.Values[Index + 1], mField, soCaseinsensitive in SortOptions) <= 0)) then
  exit;
  end;
  end;
  fIsDirty := True;
  end;
}

procedure TdxMemIndex.UpdateRecord(ABuffer: TRecordBuffer);
var
  AValueIndex: Integer;
  AIndex, APrevIndex, ANextIndex: Integer;
  mField: TdxMemField;
begin
  if fIsDirty then
    Exit;
  AValueIndex := FValues.IndexOf(ABuffer);
  if AValueIndex > -1 then
  begin
    AIndex := FValueIndexes[AValueIndex];
    if AIndex <> MemData.Data.FValues.IndexOf(ABuffer) then
    begin
      fIsDirty := True;
      Exit;
    end;

    if AValueIndex > 0 then
      APrevIndex := FValueIndexes[AValueIndex - 1]
    else
      APrevIndex := -1;
    if AValueIndex < MemData.RecordCount - 1 then
      ANextIndex := FValueIndexes[AValueIndex + 1]
    else
      ANextIndex := -1;

    mField := MemData.Data.IndexOf(FField);
    if ((APrevIndex = -1) or
      (MemData.InternalCompareValues(mField.Values[APrevIndex],
      mField.Values[AIndex], mField, soCaseInsensitive in SortOptions) <= 0))
      and ((ANextIndex = -1) or
      (MemData.InternalCompareValues(mField.Values[AIndex],
      mField.Values[ANextIndex], mField, soCaseInsensitive in SortOptions) <= 0))
    then
      Exit;
  end;
  fIsDirty := True;
end;

procedure TdxMemIndex.SetFieldName(Value: string);
var
  AField: TField;
begin
  if (MemData <> nil) and (csLoading in MemData.ComponentState) then
  begin
    fLoadedFieldName := Value;
    Exit;
  end;
  if (CompareText(fFieldName, Value) <> 0) then
  begin
    AField := MemData.FieldByName(Value);
    if AField <> nil then
    begin
      fFieldName := AField.FieldName;
      FField := AField;
      IsDirty := True;
    end;
  end;
end;

procedure TdxMemIndex.SetSortOptions(Value: TdxSortOptions);
begin
  if (SortOptions <> Value) then
  begin
    FSortOptions := Value;
    IsDirty := True;
  end;
end;

procedure TdxMemIndex.SetFieldNameAfterMemdataLoaded;
begin
  if (fLoadedFieldName <> '') then
    FieldName := fLoadedFieldName;
end;

function TdxMemIndex.GetMemData: TdxCustomMemData;
begin
  Result := TdxMemIndexes(Collection).FMemData;
end;

{ TdxMemField }

constructor TdxMemField.Create(AOwner: TdxMemFields);
begin
  inherited Create;
  FOwner := AOwner;
  FIndex := FOwner.FItems.Count;
end;

procedure TdxMemField.CreateField(AField: TField);
var
  I: Integer;
  mField: TdxMemField;
  AIsRecId: Boolean;
begin
  FField := AField;
  FDataType := Field.DataType;
  FDataSize := GetDataSize(AField);
  AIsRecId := AField = DataSet.RecIdField;
  FNeedAutoInc := AIsRecId or (FDataType = ftAutoInc);
  if FIndex = 0 then
  begin
    FOffSet := 0;
    FOwner.FValuesSize := 0;
  end
  else
  begin
    mField := FOwner.Items[Index - 1];
    FOffSet := mField.FOffSet + mField.FDataSize + 1;
  end;
  FValueOffSet := FOffSet + 1;
  Inc(FOwner.FValuesSize, FDataSize + 1);
  FNextAutoIncValue := 1;
  for I := 0 to DataSet.RecordCount - 1 do
    InternalAddValue(nil);
end;

function TdxMemField.GetActiveBuffer(AActiveBuffer,
  ABuffer: TRecordBuffer): Boolean;
var
  AData: Pointer;
begin
  AData := GetDataFromBuffer(AActiveBuffer);
  Result := ReadByte(AData) <> 0;
  AData := ShiftPointer(AData, SizeOf(Byte));
  if (ABuffer <> nil) and Result then
  begin
    if Field.DataType in ftStrings then
      CopyChars(AData, ABuffer, FDataSize, FDataType)
    else
      cxCopyData(AData, ABuffer, FDataSize);
  end;
end;

procedure TdxMemField.SetActiveBuffer(AActiveBuffer, ABuffer: TRecordBuffer);
var
  AData: Pointer;
begin
  AData := GetDataFromBuffer(AActiveBuffer);
  if ABuffer <> nil then
  begin
    WriteByte(AData, 1);
    AData := ShiftPointer(AData, SizeOf(Byte));
    if FDataType in ftStrings then
      CopyChars(ABuffer, AData, Field.Size, FDataType)
    else
      cxCopyData(ABuffer, AData, FDataSize);
  end
  else
    WriteByte(AData, 0);
end;

procedure TdxMemField.SetAutoIncValue(const AValue, ADataPointer
  : TRecordBuffer);
var
  ANewValue: Integer;
begin
  if (AValue <> nil) then
    ANewValue := ReadInteger(AValue)
  else
    ANewValue := -1;
  if (AValue <> nil) and (FNextAutoIncValue <= ANewValue) then
    FNextAutoIncValue := ANewValue + 1
  else
  begin
    if not DataSet.IsBinaryDataLoading or (AValue = nil) then
    begin
      WriteByte(ADataPointer, 1);
      WriteInteger(ADataPointer, FNextAutoIncValue, 1);
      Inc(FNextAutoIncValue);
    end;
  end;
end;

procedure TdxMemField.AddValue(AValue: TRecordBuffer);
begin
  if FIndex = 0 then
    InsertValue(FOwner.FValues.Count, AValue)
  else
    InsertValue(FOwner.FValues.Count - 1, AValue);
end;

procedure TdxMemField.InsertValue(AIndex: Integer; ABuffer: TRecordBuffer);
begin
  if AIndex = FOwner.FValues.Count then
    FOwner.AllocateRecordMemory;
  InternalAddValue(ABuffer);
end;

function TdxMemField.GetDataFromBuffer(ABuffer: TRecordBuffer): TRecordBuffer;
begin
  Result := ShiftPointer(ABuffer, FOffSet);
end;

function TdxMemField.GetHasValueFromBuffer(ABuffer: TRecordBuffer): AnsiChar;
begin
  Result := AnsiChar(ReadByte(ABuffer, FOffSet));
end;

function TdxMemField.GetValueFromBuffer(ARecordBuffer: TRecordBuffer)
  : TRecordBuffer;
begin
  if GetHasValueFromBuffer(ARecordBuffer) <> #0 then
    Result := ShiftPointer(ARecordBuffer, FValueOffSet)
  else
    Result := nil;
end;

function TdxMemField.CompareRecordBuffers(ARecordBuffer1, ARecordBuffer2
  : TRecordBuffer; AIsCaseInsensitive: Boolean): Integer;
begin
  Result := DataSet.InternalCompareValues(GetValueFromBuffer(ARecordBuffer1),
    GetValueFromBuffer(ARecordBuffer2), self, AIsCaseInsensitive);
end;

function TdxMemField.DataPointer(AIndex, AOffset: Integer): TRecordBuffer;
begin
  Result := ShiftPointer(FOwner.FValues[AIndex], AOffset);
end;

procedure TdxMemField.InternalAddValue(AValue: TRecordBuffer);
var
  AData: Pointer;
begin
  AData := GetDataFromBuffer(FOwner.Values.Last);
  if AValue = nil then
    WriteByte(AData, 0)
  else
  begin
    WriteByte(AData, 1);
    cxCopyData(AValue, AData, 0, SizeOf(Byte), FDataSize);
  end;
  if FNeedAutoInc then
    SetAutoIncValue(AValue, AData);
end;

function TdxMemField.GetValues(AIndex: Integer): TRecordBuffer;
begin
  if HasValue[AIndex] then
    Result := DataPointer(AIndex, FValueOffSet)
  else
    Result := nil;
end;

function TdxMemField.GetHasValue(AIndex: Integer): Boolean;
begin
  Result := HasValues[AIndex] <> #0;
end;

function TdxMemField.GetHasValues(AIndex: Integer): AnsiChar;
begin
  Result := AnsiChar(ReadByte(DataPointer(AIndex, FOffSet)));
end;

procedure TdxMemField.SetHasValue(AIndex: Integer; AValue: Boolean);
const
  AValues: array [Boolean] of AnsiChar = (#0, #1);
begin
  HasValues[AIndex] := AValues[AValue];
end;

procedure TdxMemField.SetHasValues(AIndex: Integer; AValue: AnsiChar);
begin
  WriteByte(DataPointer(AIndex, FOffSet), Byte(AValue));
end;

function TdxMemField.GetDataSet: TdxCustomMemData;
begin
  Result := MemFields.DataSet;
end;

function TdxMemField.GetMemFields: TdxMemFields;
begin
  Result := FOwner;
end;

{ TdxFieldWriter }

constructor TdxFieldWriter.Create(AMemData: TdxCustomMemData; AField: TField);
begin
  inherited Create;
  FMemData := AMemData;
  FField := AField;
end;

{ TdxBlobFieldWriter }

procedure TdxBlobFieldWriter.WriteFieldValue(AStream: TStream;
  AMemField: TdxMemField; ARecordIndex: Integer);
var
  ABlobLength: Integer;
  ABlobData: AnsiString;
begin
  ABlobData := MemData.GetBlobData(MemData.FBlobList[ARecordIndex],
    Field.OffSet);
  ABlobLength := Length(ABlobData);
  WriteIntegerToStream(AStream, ABlobLength);
  if (ABlobLength > 0) then
    WriteStringToStream(AStream, ABlobData);
end;

{ TdxStringFieldWriter }

procedure TdxStringFieldWriter.WriteFieldValue(AStream: TStream;
  AMemField: TdxMemField; ARecordIndex: Integer);
var
  AStrLength: Integer;
begin
  WriteCharToStream(AStream, AMemField.HasValues[ARecordIndex]);
  if AMemField.HasValue[ARecordIndex] then
  begin
    AStrLength := MemData.GetStringLength(Field.DataType,
      AMemField.Values[ARecordIndex]);
    WriteIntegerToStream(AStream, AStrLength);
    WriteBufferToStream(AStream, AMemField.Values[ARecordIndex],
      AStrLength * GetCharSize(Field.DataType));
  end;
end;

{ TdxOrdinalFieldWriter }

procedure TdxOrdinalFieldWriter.WriteFieldValue(AStream: TStream;
  AMemField: TdxMemField; ARecordIndex: Integer);
begin
  WriteCharToStream(AStream, AMemField.HasValues[ARecordIndex]);
  WriteBufferToStream(AStream, AMemField.Values[ARecordIndex],
    AMemField.FDataSize);
end;

{ TdxMemDataStreamWriter }

procedure TdxMemDataStreamWriter.WriteMemDataVersion;
begin
  WriteStringToStream(Stream, MemDataVerString);
  WriteDoubleToStream(Stream, MemDataVer);
end;

function TdxMemDataStreamWriter.GetFieldWriterClass(AFieldType: TFieldType)
  : TdxFieldWriterClass;
begin
  case BaseFieldType(AFieldType) of
    bftBlob:
      Result := TdxBlobFieldWriter;
    bftString:
      Result := TdxStringFieldWriter;
  else { bftOrdinal }
    Result := TdxOrdinalFieldWriter;
  end;
end;

function TdxMemDataStreamWriter.GetFieldWritersByField(AField: TField)
  : TdxFieldWriter;
begin
  Result := TdxFieldWriter(GetFieldStreamersByField(AField));
end;

procedure TdxMemDataStreamWriter.WriteFields;
var
  I: Integer;
begin
  WriteIntegerToStream(Stream, FieldCount);
  for I := 0 to FieldCount - 1 do
  begin
    if Fields[I].DataType in ftStrings then
      WriteIntegerToStream(Stream, Fields[I].Size)
    else
      WriteIntegerToStream(Stream, GetDataSize(Fields[I]));

    WriteSmallIntToStream(Stream, GetNoByFieldType(Fields[I].DataType));
    WriteSmallIntToStream(Stream, Length(Fields[I].FieldName) + 1);
    WriteStringToStream(Stream, dxStringToAnsiString(Fields[I].FieldName));

    // lines below for compability with Win32 version.
    // there was a bug on saving unneeded byte
    WriteCharToStream(Stream, #0);

    FFieldStreamers.Add(GetFieldWriterClass(Fields[I].DataType).Create(MemData,
      Fields[I]));
  end;
end;

procedure TdxMemDataStreamWriter.WriteRecord(ARecordIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FieldCount - 1 do
    FieldWritersByField[Fields[I]].WriteFieldValue(Stream,
      MemDataField(Fields[I]), ARecordIndex);
end;

procedure TdxMemDataStreamWriter.SaveData;
var
  I: Integer;
begin
  WriteMemDataVersion;
  PopulateFieldList;
  WriteFields;

  for I := 0 to MemData.FData.RecordCount - 1 do
    WriteRecord(I);
end;

function TdxIntegerList.Add(AValue: TdxNativeInt): Integer;
begin
  Result := inherited Add(Pointer(AValue));
end;

function TdxIntegerList.IndexOf(AValue: TdxNativeInt): Integer;
begin
  Result := inherited IndexOf(Pointer(AValue));
end;

procedure TdxIntegerList.Insert(AIndex: Integer; AValue: TdxNativeInt);
begin
  inherited Insert(AIndex, Pointer(AValue));
end;

function TdxIntegerList.GetItem(AIndex: Integer): TdxNativeInt;
begin
  Result := TdxNativeInt(inherited Items[AIndex]);
end;

procedure TdxIntegerList.SetItem(AIndex: Integer; AValue: TdxNativeInt);
begin
  inherited Items[AIndex] := Pointer(AValue);
end;

{ TdxMemDataStreamReader }

constructor TdxMemDataStreamReader.Create(AMemData: TdxCustomMemData;
  AStream: TStream);
begin
  inherited;
  FVerNo := -1;
end;

function TdxMemDataStreamReader.GetFieldReader(Index: Integer): TdxFieldReader;
begin
  Result := TdxFieldReader(FFieldStreamers[Index]);
end;

function TdxMemDataStreamReader.GetFieldReaderClass(AFieldTypeNo: Integer)
  : TdxFieldReaderClass;
var
  Var1: TFieldType;
begin
  Var1 :=
    GetFieldTypeByNo(AFieldTypeNo);
  case BaseFieldType(Var1) of
    bftBlob:
      Result := TdxBlobFieldReader;
    bftString:
      if VerNo < 1.85 then
        Result := TdxStringFieldReader
      else if VerNo < 1.905 then
        Result := TdxStringFieldReaderVer190
      else
        Result := TdxStringFieldReaderVer191;
  else { bftOrdinal }
    Result := TdxOrdinalFieldReader;
  end;
end;

function TdxMemDataStreamReader.GetFieldReadersByField(AField: TField)
  : TdxFieldReader;
begin
  Result := TdxFieldReader(GetFieldStreamersByField(AField));
end;

procedure TdxMemDataStreamReader.AddRecord;
var
  ARecordCount: Integer;
  P: TRecordBuffer;
  I: Integer;
  AFieldReader: TdxFieldReader;
begin
  MemData.Data.AllocateRecordMemory;
  ARecordCount := MemData.RecordCount;
  if MemData.RecIdMemField <> nil then
    MemData.RecIdMemField.InternalAddValue(@ARecordCount);

  if MemData.BlobFieldCount > 0 then
  begin
    P := AllocMem(MemData.BlobFieldCount * SizeOf(TRecordBuffer));
    MemData.InitializeBlobData(P);
    MemData.FBlobList.Add(P);
  end;
  for I := 0 to FieldCount - 1 do
  begin
    AFieldReader := GetFieldReadersByField(Fields[I]);

    if not Fields[I].IsBlob then
    begin
      if (AFieldReader <> nil) and AFieldReader.HasValue then
        MemDataField(Fields[I]).InternalAddValue(AFieldReader.FBuffer)
      else
        MemDataField(Fields[I]).InternalAddValue(nil);
    end
    else
    begin
      if (MemData.FBlobList.Last <> nil) and (AFieldReader <> nil) then
        MemData.SetInternalBlobData(MemData.FBlobList.Last,
          AFieldReader.Field.OffSet, AFieldReader.BlobData);
    end;
  end;
end;

function TdxMemDataStreamReader.ReadVerNoFromStream: Boolean;
var
  ABuf: Array [0 .. Length(MemDataVerString)] of AnsiChar;
begin
  Result := Stream.Read(ABuf, Length(MemDataVerString))
    = Length(MemDataVerString);
  ABuf[Length(MemDataVerString)] := #0;
  if Result then
  begin
    if ABuf = MemDataVerString then
    begin
      Result := Stream.Read(FVerNo, SizeOf(Double)) = SizeOf(Double);
      if FVerNo < 1 then
        FVerNo := 1;
    end
    else
    begin
      Stream.Position := 0;
      FVerNo := 0;
    end;
  end;
end;

function TdxMemDataStreamReader.ReadFieldsFromStream: Boolean;
var
  I, AFieldSize, Count: Integer;
  AFieldTypeNo, AFieldNameLength: Smallint;
  ABuf: Array [0 .. 255] of AnsiChar;
begin
  Result := False;
  Stream.Read(Count, 4);
  for I := 0 to Count - 1 do
  begin
    if (Stream.Read(AFieldSize, 4) < 4) then
      Exit;
    if (Stream.Read(AFieldTypeNo, 2) < 2) then
      Exit;
    if (Stream.Read(AFieldNameLength, 2) < 2) then
      Exit;
    if (AFieldNameLength > 255) then
      raise EdxException.Create
        (cxGetResourceString(@cxSDataInvalidStreamFormat));
    if (Stream.Read(ABuf, AFieldNameLength) < AFieldNameLength) then
      Exit;
    FFieldStreamers.Add(GetFieldReaderClass(AFieldTypeNo).Create(string(ABuf),
      MemData.FindField(string(ABuf)), AFieldSize, AFieldTypeNo));
  end;
  Result := (Stream.Position <= Stream.Size) and (FieldStreamersCount > 0);
end;

function TdxMemDataStreamReader.ReadRecordFromStream: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FieldStreamersCount - 1 do
  begin
    Result := FieldReaders[I].ReadFieldValue(Stream, VerNo);
    if not Result then
      Break;
  end;
end;

procedure TdxMemDataStreamReader.CreateFields(AMemData: TdxCustomMemData);
var
  I: Integer;
begin
  if ReadVerNoFromStream and ReadFieldsFromStream then
  begin
    for I := 0 to FieldStreamersCount - 1 do
      FieldReaders[I].CreateField(AMemData);
  end;
end;

procedure TdxMemDataStreamReader.LoadData;
begin
  if not ReadVerNoFromStream or not ReadFieldsFromStream then
    Exit;
  PopulateFieldList;
  while (Stream.Position < Stream.Size) and ReadRecordFromStream do
    AddRecord
end;

{ TdxReadBlobField }

function TdxBlobFieldReader.ReadFieldValue(AStream: TStream;
  AVerNo: Double): Boolean;
begin
  Result := True;
  if ReadFieldSize(AStream) then
  begin
    HasValue := FRecordFieldSize > 0;
    Result := ReadBlobFieldValue(AStream);
  end;
end;

function TdxBlobFieldReader.ReadBlobFieldValue(AStream: TStream): Boolean;
begin
  if Field <> nil then
  begin
    BlobData := '';
    if Length(BlobData) < FRecordFieldSize then
      SetLength(BlobData, FRecordFieldSize);
    Result := AStream.Read(TRecordBuffer(BlobData)^, FRecordFieldSize)
      = FRecordFieldSize;
  end
  else
  begin
    AStream.Position := AStream.Position + FRecordFieldSize;
    Result := AStream.Position <= AStream.Size;
  end;
end;

{ TdxReadField }
constructor TdxFieldReader.Create(AFieldName: string; AField: TField;
  ADataSize: Integer; AFieldTypeNo: Integer);
begin
  inherited Create;
  fFieldName := AFieldName;
  FField := AField;
  FFieldTypeNo := AFieldTypeNo;
  FDataType := GetFieldTypeByNo(AFieldTypeNo);
  FDataSize := GetDataSize(ADataSize);
  FFieldSize := GetFieldSize(ADataSize);
  FBuffer := nil;
  if (Field <> nil) then
  begin
    FBuffer := AllocMem(FDataSize);
    HasValue := True;
  end;
end;

destructor TdxFieldReader.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TdxFieldReader.GetHasValue: Boolean;
begin
  Result := FHasValue = 1;
end;

procedure TdxFieldReader.SetHasValue(Value: Boolean);
begin
  if Value then
    FHasValue := 1
  else
    FHasValue := 0;
end;

function TdxFieldReader.ReadFieldSize(AStream: TStream): Boolean;
begin
  Result := AStream.Read(FRecordFieldSize, SizeOf(Integer)) = SizeOf(Integer);
  if FRecordFieldSize > AStream.Size then
    FRecordFieldSize := AStream.Size;
end;

procedure TdxFieldReader.CreateField(AMemData: TdxCustomMemData);
begin
  if (Field <> nil) or (DataType = ftUnknown) then
    Exit;
  FField := AMemData.GetFieldClass(DataType).Create(AMemData);
  FField.FieldName := FieldName;
  FField.DataSet := AMemData;
  FField.Name := GetValidName(AMemData, AMemData.Name + Field.FieldName);
  FField.Calculated := False;
end;

function TdxFieldReader.GetDataSize(AReadingDataSize: Integer): Integer;
begin
  Result := AReadingDataSize;
end;

function TdxFieldReader.GetFieldSize(AReadingDataSize: Integer): Integer;
begin
  Result := AReadingDataSize;
end;

initialization

GroupDescendentsWith(TdxCustomMemData, TControl);

end.
