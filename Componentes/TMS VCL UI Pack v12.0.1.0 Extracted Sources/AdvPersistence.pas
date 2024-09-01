{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvPersistence;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LCLLIB}
  fgl,
  {$IFNDEF MSWINDOWS}
  LCLIntF,
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF LCLLIB}
  Generics.Collections,
  {$ENDIF}
  Classes, TypInfo, Variants, SysUtils,
  AdvTypes,
  AdvJSONReader,
  AdvJSONWriter
  {$IFDEF FNCLIB}
  {$IFDEF WEBLIB}
  ,WEBLib.JSON
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,JSON
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,JSON
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fpjson
  {$ENDIF}
  {$ENDIF}
  ;

type
  TStreamEx = TStream;

  IAdvPersistence = interface
  ['{363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}']
    procedure SaveSettingsToFile(AFileName: string; AAppearanceOnly: Boolean = False);
    procedure LoadSettingsFromFile(AFileName: string);
    procedure SaveSettingsToStream(AStream: TStreamEx; AAppearanceOnly: Boolean = False);
    procedure LoadSettingsFromStream(AStream: TStreamEx);
    function CanSaveProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
    function CanLoadProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
  end;

  IAdvBaseListIO = interface
  ['{FAB1D21E-D798-4CE0-B17B-9D75E4456AB4}']
    function GetItemClass: TClass;
  end;

  IAdvBasePersistenceIO = interface
    ['{91DEAFC3-8932-45F4-A3ED-5AAA0C0E9250}']
    function CreateObject(const AClassName: string; const ABaseClass: TClass): TObject;
  end;

  IAdvBaseCollectionIO = interface
  ['{90FDF257-7362-411D-B7F6-E2BEE2265016}']
    function AddItem(const AObject: TObject): TCollectionItem;
  end;

  IAdvPersistenceIO = interface(IAdvBasePersistenceIO)
  ['{11B625F8-447A-4AE5-BB88-5ECDEA979AF7}']
    function NeedsObjectReference(const AClass: TClass): Boolean;
    function GetObjectReference(const AObject: TObject): string;
    function FindObject(const AReference: string): TObject;
    procedure FixOwners(const AObject: TObject; const AObjectList: TObject);
  end;

  EAdvReaderException = class(Exception)
  end;

  {$IFDEF WEBLIB}
  TAdvPropertyInfo = TTypeMemberProperty;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TAdvPropertyInfo = PPropInfo;
  {$ENDIF}

  TAdvObjectDictionary = class(TDictionary<string, TObject>);
  TAdvObjectList = class(TObjectList<TObject>);
  TAdvStringList = class(TList<string>);
  TAdvIntegerList = class(TList<Integer>);
  TAdvDoubleList = class(TList<Double>);

  TAdvObjectArray = array of TObject;

  TAdvWriterCustomWritePropertyEvent = procedure(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AWriter: TAdvJSONWriter; var ACanWrite: Boolean) of object;
  TAdvWriterCustomIsAssignablePropertyEvent = procedure(AObject: TObject; APropertyName: string; var AIsAssignable: Boolean) of object;

  TAdvExcludePropertyListArray = array of string;

  TAdvWriter = class
  private
    FWriter: TAdvJSONWriter;
    FIOReference: TObject;
    FOnCustomWriteProperty: TAdvWriterCustomWritePropertyEvent;
    FRootObject: TObject;
    FExcludeProperties: TAdvExcludePropertyListArray;
    FOnCustomIsAssignableProperty: TAdvWriterCustomIsAssignablePropertyEvent;
    procedure SetRootObject(const Value: TObject);
    procedure SetExcludeProperties(
      const Value: TAdvExcludePropertyListArray);
    procedure SetIOReference(const Value: TObject);
    property Writer: TAdvJSONWriter read FWriter;
    procedure WritePropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
    procedure WriteProperties(AObject: TObject);
    procedure WriteProperty(AObject: TObject; AProp: TAdvPropertyInfo);
    procedure WriteGenericObjectList(AList: TAdvObjectList);
    procedure WriteGenericStringList(AList: TAdvStringList);
    procedure WriteGenericIntegerList(AList: TAdvIntegerList);
    procedure WriteGenericDoubleList(AList: TAdvDoubleList);
    procedure WriteStrings(AList: TStrings);
    procedure WriteGenericDictionary(ADictionary: TAdvObjectDictionary);
    procedure WriteCollection(ACollection: TCollection);
    procedure WriteList(AList: TList);
    procedure WriteBitmap(ABitmap: TAdvBitmap);
    procedure WriteSingleObject(AObject: TObject);
    procedure WriteObject(AObject: TObject);
  public
    constructor Create(AStream: TStreamEx);
    destructor Destroy; override;
    procedure Write(AObject: TObject);
    procedure WriteArray(AName: string; AArray: TAdvObjectArray);
    property JSONWriter: TAdvJSONWriter read FWriter;
    property IOReference: TObject read FIOReference write SetIOReference;
    property RootObject: TObject read FRootObject write SetRootObject;
    property ExcludeProperties: TAdvExcludePropertyListArray read FExcludeProperties write SetExcludeProperties;
    property OnCustomWriteProperty: TAdvWriterCustomWritePropertyEvent read FOnCustomWriteProperty write FOnCustomWriteProperty;
    property OnCustomIsAssignableProperty: TAdvWriterCustomIsAssignablePropertyEvent read FOnCustomIsAssignableProperty write FOnCustomIsAssignableProperty;
  end;

  TAdvReaderCustomReadPropertyEvent = procedure(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AReader: TAdvJSONReader; var ACanRead: Boolean) of object;

  TAdvObjectReference = class
  public
    Instance: TObject;
    Prop: TAdvPropertyInfo;
    Id: string;
    constructor Create(AInstance: TObject; AProp: TAdvPropertyInfo; const AId: string);
  end;

  TAdvObjectReferences = TObjectList<TAdvObjectReference>;

  TAdvReader = class
  private
    FReferences: TAdvObjectReferences;
    FReader: TAdvJSONReader;
    FIOReference: TObject;
    FOnCustomReadProperty: TAdvReaderCustomReadPropertyEvent;
    FRootObject: TObject;
    FExcludeProperties: TAdvExcludePropertyListArray;
    FOnCustomIsAssignableProperty: TAdvWriterCustomIsAssignablePropertyEvent;
    function ReadSingleObject(ABaseClass: TClass): TObject; overload;
    procedure SetRootObject(const Value: TObject);
    procedure SetExcludeProperties(
      const Value: TAdvExcludePropertyListArray);
    procedure SetIOReference(const Value: TObject);
    property Reader: TAdvJSONReader read FReader;
    procedure ReadSingleObject(AObject: TObject); overload;
    procedure ReadProperties(AObject: TObject);
    procedure ReadProperty(AObject: TObject; AProp: TAdvPropertyInfo);
    procedure ReadPropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
    procedure ReadExistingObject(AObject: TObject);
    procedure ReadGenericStringList(AList: TAdvStringList);
    procedure ReadGenericDoubleList(AList: TAdvDoubleList);
    procedure ReadGenericIntegerList(AList: TAdvIntegerList);
    procedure ReadStrings(AList: TStrings);
    procedure ReadGenericObjectList(AList: TAdvObjectList);    
    procedure ReadGenericDictionary(ADictionary: TAdvObjectDictionary);
    procedure ReadCollection(ACollection: TCollection);
    procedure ReadList(AList: TList);
    procedure ReadBitmap(ABitmap: TAdvBitmap);
    procedure ReadObject(AObject: TObject);
  public
    constructor Create(AStream: TStreamEx);
    destructor Destroy; override;
    function Read(AClass: TClass): TObject; overload;
    procedure Read(AObject: TObject); overload;
    function ReadArray(AName: string): TAdvObjectArray; overload;
    property JSONReader: TAdvJSONReader read FReader;
    property IOReference: TObject read FIOReference write SetIOReference;
    property RootObject: TObject read FRootObject write SetRootObject;
    property ExcludeProperties: TAdvExcludePropertyListArray read FExcludeProperties write SetExcludeProperties;
    property OnCustomReadProperty: TAdvReaderCustomReadPropertyEvent read FOnCustomReadProperty write FOnCustomReadProperty;
    property OnCustomIsAssignableProperty: TAdvWriterCustomIsAssignablePropertyEvent read FOnCustomIsAssignableProperty write FOnCustomIsAssignableProperty;
    procedure SolveReferences;
  end;

  {$IFDEF WEBLIB}
  PTypeInfo = TypInfo.TTypeInfo;
  {$ELSE}
  PTypeInfo = TypInfo.PTypeInfo;
  {$ENDIF}

  TAdvObjectPersistence = class
  public
    class function SaveObjectToString(AObject: TObject): string;
    class procedure LoadObjectFromString(AObject: TObject; AString: string);
  end;

  TAdvPersistence = class
  public class var
    ClassTypeVariable: string;
  private
    class var FOnCustomReadProperty: TAdvReaderCustomReadPropertyEvent;
    class var FOnCustomWriteProperty: TAdvWriterCustomWritePropertyEvent;
    class var FRootObject: TObject;
    class var FExcludeProperties: TAdvExcludePropertyListArray;
    class var FIOReference: TObject;
    class procedure DoCustomReadProperty(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AReader: TAdvJSONReader; var ACanRead: Boolean);
    class procedure DoCustomWriteProperty(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AWriter: TAdvJSONWriter; var ACanWrite: Boolean);
  public
    class procedure SaveSettingsToFile(AObject: TObject; AFileName: string);
    class procedure LoadSettingsFromFile(AObject: TObject; AFileName: string);
    class procedure SaveSettingsToStream(AObject: TObject; AStream: TStreamEx);
    class procedure LoadSettingsFromStream(AObject: TObject; AStream: TStreamEx);
    class procedure GetEnumValues(AValues: TStrings; APropInfo: TAdvPropertyInfo);
    class function CreateObject(const AClassName: string; BaseClass: TClass): TObject;
    class function GetPropInfoDataTypeInfo(APropInfo: TAdvPropertyInfo): PTypeInfo;
    class function GetPropInfoDataTypeInfoClassType(APropInfo: TAdvPropertyInfo): TClass;
    class function GetPropInfoType(APropInfo: TAdvPropertyInfo): TTypeKind; virtual;
    class function GetPropInfoName(APropInfo: TAdvPropertyInfo): string; virtual;
    class function GetPropInfoTypeName(APropInfo: TAdvPropertyInfo): string;
    class function GetEnumName(ATypeInfo: PTypeInfo; AValue: Integer): string;
    class function IsWriteOnly(APropInfo: TAdvPropertyInfo): Boolean; virtual;
    class function IsReadOnly(APropInfo: TAdvPropertyInfo): Boolean; virtual;
    class function IsAssignableProperty(AObject: TObject; APropInfo: TAdvPropertyInfo): Boolean; virtual;
    class function IsColor(APropertyName: string): Boolean; virtual;
    class function IsStrokeKind(APropertyName: string): Boolean; virtual;
    class function IsFillKind(APropertyName: string): Boolean; virtual;
    class function IsDate(APropertyName: string): Boolean; virtual;
    class function IsDateTime(APropertyName: string): Boolean; virtual;
    class function IsTime(APropertyName: string): Boolean; virtual;
    class function IsGenericList(AClass: TClass; AType: string = ''): Boolean; virtual;
    class function IsGenericDictionary(AClass: TClass): Boolean; virtual;
    class function IsCollection(AClass: TClass): Boolean; virtual;
    class function IsComponent(AClass: TClass): Boolean; virtual;
    class function IsControl(AClass: TClass): Boolean; virtual;
    class function IsList(AClass: TClass): Boolean; virtual;
    class function IsDescendingClass(AClass: TClass; AClassParentList: array of string): Boolean; virtual;
    class function IsBitmap(AClass: TClass): Boolean; virtual;
    class function IsStrings(AClass: TClass): Boolean; virtual;
    class property OnCustomWriteProperty: TAdvWriterCustomWritePropertyEvent read FOnCustomWriteProperty write FOnCustomWriteProperty;
    class property OnCustomReadProperty: TAdvReaderCustomReadPropertyEvent read FOnCustomReadProperty write FOnCustomReadProperty;
    class property RootObject: TObject read FRootObject write FRootObject;
    class property ExcludeProperties: TAdvExcludePropertyListArray read FExcludeProperties write FExcludeProperties;
    class property IOReference: TObject read FIOReference write FIOReference;
  end;

  {$IFDEF FNCLIB}
  TAdvJSONToClassPropertyType = (cptUndefined, cptString, cptBoolean, cptDateTime, cptObject, cptDouble, cptInteger,
    cptInteger64, cptObjectArray, cptStringArray, cptBooleanArray, cptDateTimeArray, cptDoubleArray, cptIntegerArray, cptInteger64Array);

  TAdvJSONToClassBaseClass = (cbcNone, cbcPersistent);

  TAdvJSONToClassOptions = record
    SortProperties: Boolean;
    DelphiCasing: Boolean;
    RemoveSpecialCharacters: Boolean;
    AddConstructor: Boolean;
    AddDestructor: Boolean;
    AddAssign: Boolean;
    AddUnit: Boolean;
    BaseClass: TAdvJSONToClassBaseClass;
    AddImplementation: Boolean;
  end;

  TAdvJSONToClassExportEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(var APropertyName: string; var APropertyType: TAdvJSONToClassPropertyType){$IFDEF LCLLIB} of object{$ENDIF};

  TAdvJSONToClassProperty = class
  private
    FName: string;
    FType: TAdvJSONToClassPropertyType;
    function FixKeyWord(AValue: string): string;
  public
    function IsObject: Boolean;
    function DelphiName(ACallBack: TAdvJSONToClassExportEvent; AOptions: TAdvJSONToClassOptions; AFixKeyWord: Boolean = True): string;
    function DelphiType(ACallBack: TAdvJSONToClassExportEvent; AOptions: TAdvJSONToClassOptions): string;
    constructor Create(AName: string; AType: TAdvJSONToClassPropertyType);
    property Name: string read FName write FName;
    property &Type: TAdvJSONToClassPropertyType read FType write FType;
  end;

  TAdvJSONToClassProperties = class(TObjectList<TAdvJSONToClassProperty>);

  TAdvJSONToClassItem = class
  private
    FProperties: TAdvJSONToClassProperties;
    FParentProperty: TAdvJSONToClassProperty;
    function GetProperties: TAdvJSONToClassProperties;
  public
    function GetClassName(ACallBack: TAdvJSONToClassExportEvent; AOptions: TAdvJSONToClassOptions): string;
    constructor Create(AParentProperty: TAdvJSONToClassProperty);
    destructor Destroy; override;
    property Properties: TAdvJSONToClassProperties read GetProperties;
    property ParentProperty: TAdvJSONToClassProperty read FParentProperty;
  end;

  TAdvJSONToClassItems = class(TObjectList<TAdvJSONToClassItem>);

  TAdvJSONToClass = class
  private
    class var FClasses: TAdvJSONToClassItems;
  protected
    class procedure JSONValueToClass(AParentProperty: TAdvJSONToClassProperty; AJSONValue: TJSONValue);
    class function JSONValueToPropertyType(AJSONValue: TJSONValue): TAdvJSONToClassPropertyType;
  public
    class function ExportToDelphi(AJSONString: string; AOptions: TAdvJSONToClassOptions; ACallBack: TAdvJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphi(AJSONString: string; ACallBack: TAdvJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphi(AJSONValue: TJSONValue; AOptions: TAdvJSONToClassOptions; ACallBack: TAdvJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphi(AJSONValue: TJSONValue; ACallBack: TAdvJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphiFromFile(AFileName: string; ACallBack: TAdvJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphiFromFile(AFileName: string; AOptions: TAdvJSONToClassOptions; ACallBack: TAdvJSONToClassExportEvent = nil): string; overload;
  end;
  {$ENDIF}

var
  ExcludePropertyList: array[0..52] of string = (
     'Align',
     'AllowFocus',
     'Anchors',
     'BevelEdges',
     'BevelInner',
     'BevelKind',
     'BevelOuter',
     'BevelWidth',
     'BiDiMode',
     'PictureContainer',
     'BorderSpacing',
     'CanParentFocus',
     'ClipChildren',
     'ClipParent',
     'Constraints',
     'Ctl3D',
     'DisableFocusEffect',
     'DoubleBuffered',
     'DragCursor',
     'DragKind',
     'DragMode',
     'Enabled',
     'EnableDragHighLight',
     'Height',
     'Hint',
     'HitTest',
     'Locked',
     'Margins',
     'Name',
     'Opacity',
     'Padding',
     'ParentBiDiMode',
     'ParentColor',
     'ParentCtl3D',
     'ParentDoubleBuffered',
     'ParentFont',
     'ParentShowHint',
     'PopupMenu',
     'Position',
     'RotationAngle',
     'RotationCenter',
     'Scale',
     'ShowHint',
     'Size',
     'StyleElements',
     'StyleName',
     'TabOrder',
     'TabStop',
     'Tag',
     'Touch',
     'TouchTargetExpansion',
     'Visible',
     'Width');

{$IFDEF FNCLIB}
function DefaultJSONToClassOptions: TAdvJSONToClassOptions;
{$ENDIF}

implementation

uses
  {$IFDEF FMXLIB}
  UITypes,
  {$ENDIF}
  DateUtils,
  StrUtils,
  Controls,
  Graphics,
  {$IFNDEF LCLLIB}
  Generics.Defaults,
  {$ENDIF}
  AdvUtils;

const
  {$IFDEF FMXLIB}
  gcNull = $00000000;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  gcNull = -1;
  {$ENDIF}

type
  {$IFDEF FMXLIB}
  TAdvPersistenceColor = TAlphaColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TAdvPersistenceColor = TColor;
  {$ENDIF}

{$IFDEF FNCLIB}
type
  {$IFNDEF LCLWEBLIB}
  TAdvJSONToClassPropertyNameComparer = class(TComparer<TAdvJSONToClassProperty>)
  public
    function Compare(const Left, Right: TAdvJSONToClassProperty): Integer; override;
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF FMXLIB}
  TControlClass = class of TControl;
{$ENDIF}
{$IFDEF CMNWEBLIB}
  TCustomControlClass = class of TCustomControl;
{$ENDIF}

{$IFNDEF WEBLIB}
{$IFNDEF FMXMOBILE}
{$IFNDEF LCLLIB}
type
  {$HINTS OFF}
  {$IF COMPILERVERSION < 26}
  TSymbolNameBase = string[255];
  TSymbolName = type TSymbolNameBase;
  {$IFEND}
  {$HINTS ON}
  PSymbolName = ^TSymbolName;
{$ENDIF}
{$IFDEF LCLLIB}
type
  PSymbolName = ^ShortString;
{$ENDIF}

function GetShortStringString(const ShortStringPointer: PSymbolName): string;
begin
  Result := string(ShortStringPointer^);
end;
{$ENDIF}
{$IFDEF FMXMOBILE}
function GetShortStringString(const ShortStringPointer: PByte): string;
var
  ShortStringLength: Byte;
  FirstShortStringCharacter: MarshaledAString;
  ConvertedLength: Cardinal;
  UnicodeCharacters: array[Byte] of Char;
begin
  if not Assigned(ShortStringPointer) then
    Result := ''
  else
  begin
    ShortStringLength := ShortStringPointer^;
    if ShortStringLength = 0 then
      Result := ''
    else
    begin
      FirstShortStringCharacter := MarshaledAString(ShortStringPointer+1);
      ConvertedLength := UTF8ToUnicode(
          UnicodeCharacters,
          Length(UnicodeCharacters),
          FirstShortStringCharacter,
          ShortStringLength
        );

      ConvertedLength := ConvertedLength-1;
      SetString(Result, UnicodeCharacters, ConvertedLength);
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

function GetTypeInfoEx(APropInfo: TAdvPropertyInfo): PTypeInfo;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF};
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.typeinfo;
  {$ENDIF}
end;

function GetColorRed(AColor: TAdvPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).R;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetRValue(AColor);
  {$ENDIF}
end;

function GetColorGreen(AColor: TAdvPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).G;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetGValue(AColor);
  {$ENDIF}
end;

function GetColorBlue(AColor: TAdvPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).B;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetBValue(AColor);
  {$ENDIF}
end;

function HTMLToColorEx(AHTML: string): TAdvPersistenceColor;

function HexVal(s:string): Integer;
var
  i,j: Integer;
  i1, i2: Integer;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  {$IFDEF ZEROSTRINGINDEX}
  i1 := 0;
  i2 := 1;
  {$ELSE}
  i1 := 1;
  i2 := 2;
  {$ENDIF}

  if s[i1] >= 'A' then
    i := ord(s[i1]) - ord('A') + 10
  else
    i := ord(s[i1]) - ord('0');

  if s[i2] >= 'A' then
    j := ord(s[i2]) - ord('A') + 10
  else
    j := ord(s[i2]) - ord('0');

  Result := i shl 4 + j;
end;

{$IFDEF CMNWEBLIB}
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(AHTML,2,2));
  g := Hexval(Copy(AHTML,4,2)) shl 8;
  b := Hexval(Copy(AHTML,6,2)) shl 16;
  Result :=  b + g + r;
{$ENDIF}

{$IFDEF FMXLIB}
const
  Alpha = TAdvPersistenceColor($FF000000);
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(AHTML,2,2)) shl 16;
  g := Hexval(Copy(AHTML,4,2)) shl 8;
  b := Hexval(Copy(AHTML,6,2));
  Result := Alpha or TAdvPersistenceColor(b + g + r);
{$ENDIF}
end;

function ColorToHTMLEx(AColor: TAdvPersistenceColor): string;
const
  HTMLHexColor = '#RRGGBB';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';
var
  c: TAdvPersistenceColor;
  i: Integer;
begin
  {$IFDEF ZEROSTRINGINDEX}
  i := 0;
  {$ELSE}
  i := 1;
  {$ENDIF}

  {$IFDEF FMXLIB}
  c := AColor;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  c := ColorToRGB(AColor);
  {$ENDIF}
  Result := HtmlHexColor;
  Result[1 + i] := HexDigit[GetColorRed(c) shr 4];
  Result[2 + i] := HexDigit[GetColorRed(c) and $F];
  Result[3 + i] := HexDigit[GetColorGreen(c) shr 4];
  Result[4 + i] := HexDigit[GetColorGreen(c) and $F];
  Result[5 + i] := HexDigit[GetColorBlue(c) shr 4];
  Result[6 + i] := HexDigit[GetColorBlue(c) and $F];
end;

{$IFDEF FNCLIB}
function DefaultJSONToClassOptions: TAdvJSONToClassOptions;
begin
  Result.AddUnit := False;
  Result.AddImplementation := False;
  Result.AddConstructor := False;
  Result.AddDestructor := False;
  Result.AddAssign := False;
  Result.BaseClass := cbcNone;
  Result.SortProperties := True;
  Result.DelphiCasing := True;
  Result.RemoveSpecialCharacters := True;
end;
{$ENDIF}

{ TAdvWriter }

constructor TAdvWriter.Create(AStream: TStreamEx);
begin
  FWriter := TAdvJSONWriter.Create(AStream);
end;

destructor TAdvWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TAdvWriter.SetExcludeProperties(
  const Value: TAdvExcludePropertyListArray);
begin
  FExcludeProperties := Value;
  TAdvPersistence.ExcludeProperties := FExcludeProperties;
end;

procedure TAdvWriter.SetIOReference(const Value: TObject);
begin
  FIOReference := Value;
  TAdvPersistence.IOReference := FIOReference;
end;

procedure TAdvWriter.SetRootObject(const Value: TObject);
begin
  FRootObject := Value;
  TAdvPersistence.RootObject := FRootObject;
end;

procedure TAdvWriter.WriteGenericDictionary(
  ADictionary: TAdvObjectDictionary);
var
  {$IFDEF LCLLIB}
  k: Integer;
  key: string;
  {$ENDIF}
  {$IFNDEF LCLLIB}
  key: string;
  {$ENDIF}
begin
  Writer.WriteBeginArray;
  {$IFDEF LCLLIB}
  for k := 0 to ADictionary.Count - 1 do
  begin
    key := ADictionary.Keys[k];
  {$ENDIF}
  {$IFNDEF LCLLIB}
  for key in ADictionary.Keys do
  begin
  {$ENDIF}
    Writer.WriteBeginObject;
    Writer.WriteName(key);
    WriteSingleObject(ADictionary[key]);
    Writer.WriteEndObject;
  end;
  Writer.WriteEndArray;
end;

procedure TAdvWriter.WriteGenericDoubleList(AList: TAdvDoubleList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteDouble(AList[I]);
  Writer.WriteEndArray;
end;

procedure TAdvWriter.WriteGenericIntegerList(AList: TAdvIntegerList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteInteger(AList[I]);
  Writer.WriteEndArray;
end;

procedure TAdvWriter.WriteGenericObjectList(AList: TAdvObjectList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    WriteSingleObject(AList[I]);
  Writer.WriteEndArray;
end;

procedure TAdvWriter.WriteGenericStringList(AList: TAdvStringList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteString(AList[I]);
  Writer.WriteEndArray;
end;

procedure TAdvWriter.Write(AObject: TObject);
begin
  WriteObject(AObject);
end;

procedure TAdvWriter.WriteArray(AName: string; AArray: TAdvObjectArray);
var
  I: Integer;
begin
  Writer.WriteBeginObject;
  Writer.WriteName(AName);
  Writer.WriteBeginArray;
  for I := 0 to Length(AArray) - 1 do
    WriteSingleObject(AArray[I]);
  Writer.WriteEndArray;
  Writer.WriteEndObject;
end;

procedure TAdvWriter.WriteBitmap(ABitmap: TAdvBitmap);
{$IFNDEF WEBLIB}
var
  ms: TMemoryStream;
{$ENDIF}
begin
  if IsBitmapEmpty(ABitmap) then
  begin
    FWriter.WriteString('');
    Exit;
  end;

 {$IFNDEF WEBLIB}
  ms := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(ms);
    ms.Position := 0;
    FWriter.WriteString(TAdvUtils.SaveStreamToHexStr(ms));
  finally
    ms.Free;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  FWriter.WriteString(ABitmap.Data);
  {$ENDIF}
end;

procedure TAdvWriter.WriteCollection(ACollection: TCollection);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to ACollection.Count - 1 do
    WriteSingleObject(ACollection.Items[I]);
  Writer.WriteEndArray;
end;

procedure TAdvWriter.WriteList(AList: TList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    WriteSingleObject(TObject(AList[I]));
  Writer.WriteEndArray;
end;

procedure TAdvWriter.WriteObject(AObject: TObject);
var
  b: IAdvPersistenceIO;
begin
  if AObject = nil then
    Writer.WriteNull
  else
  begin
    if TAdvPersistence.IsGenericList(AObject.ClassType, 'String') then
      WriteGenericStringList(TAdvStringList(AObject))
    else if TAdvPersistence.IsGenericList(AObject.ClassType, 'Integer') then
      WriteGenericIntegerList(TAdvIntegerList(AObject))
    else if TAdvPersistence.IsGenericList(AObject.ClassType, 'Double') then
      WriteGenericDoubleList(TAdvDoubleList(AObject))
    else if TAdvPersistence.IsGenericList(AObject.ClassType) then
      WriteGenericObjectList(TAdvObjectList(AObject))
    else if TAdvPersistence.IsGenericDictionary(AObject.ClassType) then
      WriteGenericDictionary(TAdvObjectDictionary(AObject))
    else if TAdvPersistence.IsList(AObject.ClassType) then
      WriteList(TList(AObject))
    else if TAdvPersistence.IsCollection(AObject.ClassType) then
      WriteCollection(TCollection(AObject))
    else if TAdvPersistence.IsBitmap(AObject.ClassType) then
      WriteBitmap(TAdvBitmap(AObject))
    else if TAdvPersistence.IsDescendingClass(AObject.ClassType, ['TStrings']) then
      WriteStrings(TStrings(AObject))
    else
    begin
      if Assigned(IOReference) and Supports(IOReference, IAdvPersistenceIO, b) then
      begin
        if b.NeedsObjectReference(AObject.ClassType) then
          Writer.WriteString(b.GetObjectReference(AObject))
        else
          WriteSingleObject(AObject);
      end
      else
        WriteSingleObject(AObject);
    end;
  end;
end;

procedure TAdvWriter.WriteSingleObject(AObject: TObject);
begin
  Writer.WriteBeginObject;
  if TAdvPersistence.ClassTypeVariable <> '' then
  begin
    Writer.WriteName(TAdvPersistence.ClassTypeVariable);
    Writer.WriteString(AObject.ClassName);
  end;
  WriteProperties(AObject);
  Writer.WriteEndObject;
end;

procedure TAdvWriter.WriteStrings(AList: TStrings);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteString(AList[I]);
  Writer.WriteEndArray;
end;

{$HINTS OFF}
procedure TAdvWriter.WritePropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
var
  cn: string;
  pName: string;
  en: string;
  k: TTypeKind;
  p: TAdvPropertyInfo;
  o: TObject;
  v: TMethod;
  c: TAdvPersistenceColor;
begin
  if TAdvPersistence.IsWriteOnly(APropInfo) then
  begin
    Writer.WriteNull;
    Exit;
  end;

  o := AInstance;
  p := APropInfo;
  k := TAdvPersistence.GetPropInfoType(p);
  pName := TAdvPersistence.GetPropInfoName(p);

  case k of
    tkInteger:
    begin
      cn := TAdvPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') then
      begin
        if GetOrdProp(o, p) = gcNull then
          Writer.WriteString('gcNull')
        else
        begin
          c := TAdvPersistenceColor(GetOrdProp(o, p));
          Writer.WriteString(ColorToHTMLEx(c))
        end;
      end
      else
        Writer.WriteInteger(GetOrdProp(o, p));
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}: Writer.WriteString(GetStrProp(o, p));
    tkEnumeration:
      if TAdvPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
        Writer.WriteBoolean(Boolean(GetOrdProp(o, p)))
      else
        Writer.WriteInteger(GetOrdProp(o, p));
    {$IFDEF LCLLIB}
    tkBool: Writer.WriteBoolean(Boolean(GetOrdProp(o, p)));
    {$ENDIF}
    {$IFDEF WEBLIB}
    tkBool: Writer.WriteBoolean(GetBoolProp(o, p));
    {$ENDIF}
    tkFloat: Writer.WriteDouble(GetFloatProp(o, p));
    {$IFNDEF WEBLIB}
    tkInt64: Writer.WriteInteger(GetInt64Prop(o, p));
    {$ENDIF}
    tkSet: Writer.WriteInteger(GetOrdProp(o, p));
    tkMethod:
    begin
      v := GetMethodProp(o, p);
      if v.Code = nil then
        Writer.WriteNull
      else
      begin
        if Assigned(TAdvPersistence.RootObject) then
          Writer.WriteString(TAdvPersistence.RootObject.MethodName(v.Code))
        else
          Writer.WriteNull;
      end;
    end
    else
    begin
      en := TAdvPersistence.GetEnumName(TypeInfo(TTypeKind), Integer(k));
      Writer.WriteNull;
      //raise EAdvReaderException.CreateFmt('Cannot write property %s with type %s', [pName, en]);
    end;
  end;

  if (o is TFont) and (pName = 'Size') then
  begin
    Writer.WriteName('IsFMX');
    Writer.WriteBoolean({$IFDEF FMXLIB}True{$ELSE}False{$ENDIF});
  end;
end;
{$HINTS ON}

procedure TAdvWriter.WriteProperties(AObject: TObject);
var
  {$IFNDEF WEBLIB}
  ci: Pointer;
  c: Integer;
  pl: PPropList;
  {$ENDIF}
  {$IFDEF WEBLIB}
  ci: TTypeInfoClass;
  a: TTypeMemberPropertyDynArray;
  {$ENDIF}
  I: Integer;
begin
  if Assigned(AObject) then
  begin
    {$IFNDEF WEBLIB}
    ci := AObject.ClassInfo;
    c := GetPropList(ci, tkAny, nil);
    GetMem(pl, c * SizeOf(TAdvPropertyInfo));
    {$ENDIF}
    {$IFDEF WEBLIB}
    ci := TypeInfo(AObject);
    {$ENDIF}
    try
      {$IFNDEF WEBLIB}
      GetPropList(ci, tkAny, pl);
      for I := 0 to c - 1 do
        WriteProperty(AObject, pl^[i]);
      {$ENDIF}
      {$IFDEF WEBLIB}
      a := GetPropList(ci, tkAny);
      for I := 0 to Length(a) - 1 do
        WriteProperty(AObject, a[I]);
      {$ENDIF}
    finally
      {$IFNDEF WEBLIB}
      FreeMem(pl);
      {$ENDIF}
    end;
  end;
end;

procedure TAdvWriter.WriteProperty(AObject: TObject; AProp: TAdvPropertyInfo);
var
  pName: string;
  k: TTypeKind;
  b, a, ap: Boolean;
  p: IAdvPersistence;
  o: TObject;
begin
  if not Assigned(AProp) then
    Exit;

  pName := TAdvPersistence.GetPropInfoName(AProp);
  k := TAdvPersistence.GetPropInfoType(AProp);

  b := TAdvUtils.IndexOfTextInArray(pName, TAdvPersistence.ExcludeProperties) = -1;
  if Supports(AObject, IAdvPersistence, p) then
    b := p.CanSaveProperty(AObject, pName, k);

  if b then
  begin
    a := True;
    if Assigned(OnCustomWriteProperty) then
      OnCustomWriteProperty(AObject, pName, k, Writer, a);

    if a then
    begin
      Writer.WriteName(pName);

      if k in [tkClass] then
      begin
        o := GetObjectProp(AObject, pName);

        ap := TAdvPersistence.IsAssignableProperty(AObject, AProp);

        if Assigned(OnCustomIsAssignableProperty) then
          OnCustomIsAssignableProperty(AObject, pName, ap);

        if ap then
        begin
          if o is TComponent then
            Writer.WriteString((o as TComponent).Name)
          else
            Writer.WriteString('');
        end
        else
          WriteObject(o);
      end
      else
        WritePropInfoValue(AObject, AProp);
    end;
  end;
end;

{ TAdvReader }

constructor TAdvReader.Create(AStream: TStreamEx);
begin
  FReader := TAdvJSONReader.Create(AStream);
  FReferences := TAdvObjectReferences.Create(true);
end;

destructor TAdvReader.Destroy;
begin
  FReader.Free;
  FReferences.Free;
  inherited;
end;

function TAdvReader.ReadSingleObject(ABaseClass: TClass): TObject;
var
  cn: string;
  b: IAdvBasePersistenceIO;
  p: IAdvPersistenceIO;
begin
  Reader.ReadBeginObject;
  if TAdvPersistence.ClassTypeVariable <> '' then
  begin
    if not Reader.HasNext or (Reader.ReadName <> TAdvPersistence.ClassTypeVariable) then
      raise EAdvReaderException.Create('"'+TAdvPersistence.ClassTypeVariable+'" property not found in Object descriptor.');
    cn := Reader.ReadString;
  end;

  if cn = '' then
    cn := ABaseClass.ClassName;

  if Assigned(FIOReference) then
  begin
    Result := nil;
    if Supports(FIOReference, IAdvBasePersistenceIO, b) then
      Result := b.CreateObject(cn, ABaseClass)
    else if Supports(FIOReference, IAdvPersistenceIO, p) then
      Result := p.CreateObject(cn, ABaseClass);
  end
  else
    Result := TAdvPersistence.CreateObject(cn, ABaseClass);
    
  try
    ReadProperties(Result);
    Reader.ReadEndObject;
  except
    Result.Free;
    raise;
  end;
end;

procedure TAdvReader.ReadExistingObject(AObject: TObject);
begin
  if Assigned(AObject) then
  begin
    Reader.ReadBeginObject;
    if TAdvPersistence.ClassTypeVariable <> '' then
    begin
      if not Reader.HasNext or (Reader.ReadName <> TAdvPersistence.ClassTypeVariable) then
        raise EAdvReaderException.Create('"'+TAdvPersistence.ClassTypeVariable+'" property not found in Object descriptor.');

      Reader.ReadString;
    end;

    ReadProperties(AObject);
    Reader.ReadEndObject;
  end
  else
    Reader.ReadNull;
end;

procedure TAdvReader.ReadGenericDictionary(ADictionary: TAdvObjectDictionary);
var
  obj: TObject;
  k: string;
  c: TClass;
  i: IAdvBaseListIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(ADictionary, IAdvBaseListIO, i) then
    c := i.GetItemClass;

  cl := False;
  ADictionary.Clear;
  Reader.ReadBeginArray;
  if not Reader.HasNext then
    ADictionary.Clear
  else
  begin
    while Reader.HasNext do
    begin
      if not Reader.IsNull then
      begin
        Reader.ReadBeginObject;
        k := Reader.ReadName;
        obj := ReadSingleObject(c);
        if Assigned(obj) then
        begin
          if not cl then
          begin
            ADictionary.Clear;
            cl := True;
          end;

          ADictionary.Add(k, obj);
        end;
        Reader.ReadEndObject;
      end
      else
        Reader.SkipValue;
    end;
  end;
  Reader.ReadEndArray;
end;

procedure TAdvReader.ReadGenericStringList(AList: TAdvStringList);
var
  obj: string;
begin
  AList.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    if not Reader.IsNull then
    begin
      obj := Reader.ReadString;
      AList.Add(obj);
    end
    else
      Reader.SkipValue;
  end;
  Reader.ReadEndArray;
end;

procedure TAdvReader.ReadGenericIntegerList(AList: TAdvIntegerList);
var
  obj: Integer;
begin
  AList.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    if not Reader.IsNull then
    begin
      obj := Reader.ReadInteger;
      AList.Add(obj);
    end
    else
      Reader.SkipValue;
  end;
  Reader.ReadEndArray;
end;

procedure TAdvReader.ReadGenericDoubleList(AList: TAdvDoubleList);
var
  obj: Double;
begin
  AList.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    if not Reader.IsNull then
    begin
      obj := Reader.ReadDouble;
      AList.Add(obj);
    end
    else
      Reader.SkipValue;
  end;
  Reader.ReadEndArray;
end;

procedure TAdvReader.ReadGenericObjectList(AList: TAdvObjectList);
var
  obj: TObject;
  b: IAdvPersistenceIO;
  c: TClass;
  i: IAdvBaseListIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(AList, IAdvBaseListIO, i) then
    c := i.GetItemClass;

  cl := False;
  Reader.ReadBeginArray;
  if not Reader.HasNext then
    AList.Clear
  else
  begin
    while Reader.HasNext do
    begin
      if not Reader.IsNull then
      begin
        obj := ReadSingleObject(c);
        if Assigned(obj) then
        begin
          if not cl then
          begin
            AList.Clear;
            cl := True;
          end;

          if Assigned(IOReference) and Supports(IOReference, IAdvPersistenceIO, b) then
            b.FixOwners(obj, AList);

          AList.Add(obj);
        end;
      end
      else
        Reader.SkipValue;
    end;
  end;
  Reader.ReadEndArray;
end;

function TAdvReader.Read(AClass: TClass): TObject;
begin
  Result := ReadSingleObject(AClass);
end;

procedure TAdvReader.Read(AObject: TObject);
begin
  ReadObject(AObject);
end;

procedure TAdvReader.ReadBitmap(ABitmap: TAdvBitmap);
var
  s: string;
  {$IFNDEF WEBLIB}
  ms: TMemoryStream;
  {$ENDIF}
begin
  if Reader.IsNull then
    Exit;

  s := Reader.ReadString;
  if s <> '' then
  begin
    {$IFNDEF WEBLIB}
    ms := TMemoryStream.Create;
    try
      TAdvUtils.LoadStreamFromHexStr(s, ms);
      ms.Position := 0;
      ABitmap.LoadFromStream(ms);
    finally
      ms.Free;
    end;
    {$ELSE}
    ABitmap.Data := s;
    {$ENDIF}
  end;
end;

procedure TAdvReader.ReadCollection(ACollection: TCollection);
var
  obj: TObject;
  c: TClass;
  i: IAdvBaseListIO;
  ii: IAdvBaseCollectionIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(ACollection, IAdvBaseListIO, i) then
    c := i.GetItemClass;

  Supports(ACollection, IAdvBaseCollectionIO, ii);

  cl := False;

  Reader.ReadBeginArray;
  if not Reader.HasNext then
    ACollection.Clear
  else
  begin
    while Reader.HasNext do
    begin
      if not Reader.IsNull then
      begin
        obj := ReadSingleObject(c);
        if Assigned(obj) then
        begin
          if not cl then
          begin
            ACollection.Clear;
            cl := True;
          end;

          try
            if obj is TPersistent then
            begin
              if Assigned(ii) then
                ii.AddItem(obj).Assign(obj as TPersistent)
              else
                ACollection.Add.Assign(obj as TPersistent);
            end;
          finally
            obj.Free;
          end;
        end;
      end
      else
        Reader.SkipValue;
    end;
  end;
  Reader.ReadEndArray;
end;

function TAdvReader.ReadArray(AName: string): TAdvObjectArray;
var
  Name: string;
begin
  Reader.ReadBeginObject;
  while Reader.HasNext do
  begin
    if not Reader.IsNull then
    begin
      Name := Reader.ReadName;
      if Name = AName then
      begin
        Reader.ReadBeginArray;
        while Reader.HasNext do
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := ReadSingleObject(TObject);
        end;
        Reader.ReadEndArray;
      end
      else
        Reader.SkipValue;
    end
    else
      Reader.SkipValue;
  end;
end;

procedure TAdvReader.ReadList(AList: TList);
var
  obj: TObject;
  b: IAdvPersistenceIO;
  c: TClass;
  i: IAdvBaseListIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(AList, IAdvBaseListIO, i) then
    c := i.GetItemClass;

  cl := False;

  Reader.ReadBeginArray;
  if not Reader.HasNext then
    AList.Clear
  else
  begin
    while Reader.HasNext do
    begin
      if not Reader.IsNull then
      begin
        obj := ReadSingleObject(c);
        if Assigned(obj) then
        begin
          if not cl then
          begin
            AList.Clear;
            cl := True;
          end;

          if Assigned(IOReference) and Supports(IOReference, IAdvPersistenceIO, b) then
            b.FixOwners(obj, AList);
          AList.Add(obj);
        end;
      end
      else
        Reader.SkipValue;
    end;
  end;
  Reader.ReadEndArray;
end;

procedure TAdvReader.ReadObject(AObject: TObject);
begin
  if AObject = nil then
    Reader.ReadNull
  else
  begin
    if TAdvPersistence.IsGenericList(AObject.ClassType, 'String') then
      ReadGenericStringList(TAdvStringList(AObject))
    else if TAdvPersistence.IsGenericList(AObject.ClassType, 'Double') then
      ReadGenericDoubleList(TAdvDoubleList(AObject))
    else if TAdvPersistence.IsGenericList(AObject.ClassType, 'Integer') then
      ReadGenericIntegerList(TAdvIntegerList(AObject))
    else if TAdvPersistence.IsGenericList(AObject.ClassType) then
      ReadGenericObjectList(TAdvObjectList(AObject))
    else if TAdvPersistence.IsGenericDictionary(AObject.ClassType) then
      ReadGenericDictionary(TAdvObjectDictionary(AObject))
    else if TAdvPersistence.IsList(AObject.ClassType) then
      ReadList(TList(AObject))
    else if TAdvPersistence.IsCollection(AObject.ClassType) then
      ReadCollection(TCollection(AObject))
    else if TAdvPersistence.IsBitmap(AObject.ClassType) then
      ReadBitmap(TAdvBitmap(AObject))
    else if TAdvPersistence.IsDescendingClass(AObject.ClassType, ['TStrings']) then
      ReadStrings(TStrings(AObject))
    else
      ReadSingleObject(AObject);
  end;
end;

procedure TAdvReader.ReadProperties(AObject: TObject);
var
  Prop: TAdvPropertyInfo;
begin
  while Reader.HasNext do
  begin
    if not Reader.IsNull then
    begin
      Prop := nil;
      if Assigned(AObject) then
        Prop := GetPropInfo(AObject, Reader.ReadName);
      if Assigned(Prop) then
        ReadProperty(AObject, Prop)
      else
        Reader.SkipValue;
    end
    else
      Reader.SkipValue;
  end;
end;

procedure TAdvReader.ReadProperty(AObject: TObject; AProp: TAdvPropertyInfo);
var
  pName: string;
  ct: TClass;
  b: Boolean;
  p: IAdvPersistence;
  pio: IAdvPersistenceIO;
  k: TTypeKind;
  a, ap: Boolean;
  o: TObject;
  n: string;
begin
  if not Assigned(AProp) then
    Exit;

  k := TAdvPersistence.GetPropInfoType(AProp);
  pName := TAdvPersistence.GetPropInfoName(AProp);

  b := TAdvUtils.IndexOfTextInArray(pName, TAdvPersistence.ExcludeProperties) = -1;
  if Supports(AObject, IAdvPersistence, p) then
    b := p.CanLoadProperty(AObject, pName, k);

  if b then
  begin
    a := True;
    if Assigned(OnCustomReadProperty) then
      OnCustomReadProperty(AObject, pName, k, Reader, a);

    if a then
    begin
      if k in [tkClass] then
      begin
        ct := TAdvPersistence.GetPropInfoDataTypeInfoClassType(AProp);
        if TAdvPersistence.IsGenericList(ct, 'String') then
          ReadGenericStringList(TAdvStringList(GetObjectProp(AObject, pName)))
        else if TAdvPersistence.IsGenericList(ct) then
          ReadGenericObjectList(TAdvObjectList(GetObjectProp(AObject, pName)))
        else if TAdvPersistence.IsGenericDictionary(ct) then
          ReadGenericDictionary(TAdvObjectDictionary(GetObjectProp(AObject, pName)))
        else if TAdvPersistence.IsList(ct) then
          ReadList(TList(GetObjectProp(AObject, pName)))
        else if TAdvPersistence.IsCollection(ct) then
          ReadCollection(TCollection(GetObjectProp(AObject, pName)))
        else if TAdvPersistence.IsBitmap(ct) then
          ReadBitmap(TAdvBitmap(GetObjectProp(AObject, pName)))
        else if TAdvPersistence.IsDescendingClass(ct, ['TStrings']) then
          ReadStrings(TStrings(GetObjectProp(AObject, pName)))
        else
        begin
          a := False;
          if Assigned(IOReference) and Supports(IOReference, IAdvPersistenceIO, pio) then
            a := pio.NeedsObjectReference(ct);

          if a then
          begin
            if Reader.IsNull then
            begin
              Reader.ReadNull;
              SetObjectProp(AObject, pName, nil);
            end
            else
              FReferences.Add(TAdvObjectReference.Create(AObject, AProp, Reader.ReadString));
          end
          else
          begin
            o := GetObjectProp(AObject, pName);

            ap := TAdvPersistence.IsAssignableProperty(AObject, AProp);
            if Assigned(OnCustomIsAssignableProperty) then
              OnCustomIsAssignableProperty(AObject, pName, ap);

            if ap then
            begin
              n := Reader.ReadString;
              if Assigned(FRootObject) and (FRootObject is TComponent) then
                SetObjectProp(AObject, pName, (FRootObject as TComponent).FindComponent(n));
            end
            else
              ReadExistingObject(o);
          end;
        end;
      end
      else
        ReadPropInfoValue(AObject, AProp);
    end;
  end
  else
    Reader.SkipValue;
end;

{$HINTS OFF}
procedure TAdvReader.ReadPropInfoValue(AInstance: TObject; const APropInfo: TAdvPropertyInfo);
var
  pName, cn, cnv, en: string;
  k: TTypeKind;
  p: TAdvPropertyInfo;
  o: TObject;
  i: Integer;
  s: string;
  b: Boolean;
  d: Double;
  ii: Int64;
  v: string;
  m: TMethod;
  bsz: Boolean;
begin
  if TAdvPersistence.IsWriteOnly(APropInfo) or Reader.IsNull then
  begin
    Reader.ReadNull;        
    Exit;
  end;
 
  o := AInstance;
  p := APropInfo;
  pName := TAdvPersistence.GetPropInfoName(p);
  k := TAdvPersistence.GetPropInfoType(p);

  case k of
    tkInteger:
    begin
      cn := TAdvPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') then
      begin
        cnv := Reader.ReadString;
        if not TAdvPersistence.IsReadOnly(p) then
        begin
          if cnv = 'gcNull' then
            SetOrdProp(o, pName, gcNull)
          else
          begin
            {$RANGECHECKS OFF}
            SetOrdProp(o, pName, HTMLToColorEx(cnv));
            {$RANGECHECKS ON}
          end;
        end;
      end
      else
      begin
        i := Reader.ReadInteger;
        if not TAdvPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, i);
      end;
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}: 
    begin
      s := Reader.ReadString;
      if not TAdvPersistence.IsReadOnly(p) then
        SetStrProp(o, p, s);
    end;
    tkEnumeration:
      if TAdvPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
      begin
        b := Reader.ReadBoolean;
        if not TAdvPersistence.IsReadOnly(p) then        
          SetOrdProp(o, p, Integer(b))
      end
      else
      begin
        i := Reader.ReadInteger;
        if not TAdvPersistence.IsReadOnly(p) then        
          SetOrdProp(o, p, i);
      end;
    {$IFDEF LCLWEBLIB}
    tkBool:
    begin
      b := Reader.ReadBoolean;
      if not TAdvPersistence.IsReadOnly(p) then
      begin
        {$IFDEF LCLLIB}
        SetOrdProp(o, p, Integer(b))
        {$ELSE}
        SetBoolProp(o, p, b)
        {$ENDIF}
      end;
    end;
    {$ENDIF}
    tkFloat:
    begin
      d := Reader.ReadDouble;
      if not TAdvPersistence.IsReadOnly(p) then
        SetFloatProp(o, p, d)
    end;
    {$IFNDEF WEBLIB}
    tkInt64:
    begin
      ii := Reader.ReadInt64;
      if not TAdvPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, ii)
    end;
    {$ENDIF}
    tkSet:
    begin
      i := Reader.ReadInteger;
      if not TAdvPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, i);
    end;
    tkMethod:
    begin
      m.Data := nil;
      m.Code := nil;
      if Reader.IsNull then
        Reader.ReadNull
      else
      begin
        if Assigned(TAdvPersistence.RootObject) then
        begin
          v := Reader.ReadString;
          m.Code := TAdvPersistence.RootObject.MethodAddress(v);
          if m.Code <> nil then
            m.Data := TAdvPersistence.RootObject;
        end
        else
          Reader.ReadNull;
      end;

      SetMethodProp(o, p, m);
    end
    else
    begin
      en := TAdvPersistence.GetEnumName(TypeInfo(TTypeKind), Integer(k));
      Reader.ReadNull;
      //raise EAdvReaderException.CreateFmt('Cannot read property %s with type %s', [pName, en]);
    end;
  end;

  if (o is TFont) and (pName = 'Size') then
  begin
    if Reader.HasNext then
    begin
      s := Reader.PeekName;
      if s = 'IsFMX' then
      begin
        Reader.ReadName;
        bsz := Reader.ReadBoolean;
        if bsz then
        begin
          {$IFNDEF FMXLIB}
          (o as TFont).Size := Round(((o as TFont).Size / 96) * 72);
          {$ENDIF}
        end
        else
        begin
          {$IFDEF FMXLIB}
          (o as TFont).Size := ((o as TFont).Size / 72) * 96;
          {$ENDIF}
        end;
      end;
    end;
  end;
end;
{$HINTS ON}

procedure TAdvReader.ReadSingleObject(AObject: TObject);
begin
  Reader.ReadBeginObject;
  if TAdvPersistence.ClassTypeVariable <> '' then
  begin
    if not Reader.HasNext or (Reader.ReadName <> TAdvPersistence.ClassTypeVariable) then
      raise EAdvReaderException.Create('"'+TAdvPersistence.ClassTypeVariable+'" property not found in Object descriptor.');
    Reader.ReadString;
  end;

  try
    ReadProperties(AObject);
    Reader.ReadEndObject;
  except
    raise;
  end;
end;

procedure TAdvReader.ReadStrings(AList: TStrings);
var
  obj: string;
begin
  AList.Clear;
  Reader.ReadBeginArray;
  while Reader.HasNext do
  begin
    if not Reader.IsNull then
    begin
      obj := Reader.ReadString;
      AList.Add(obj);
    end
    else
      Reader.SkipValue;
  end;
  Reader.ReadEndArray;
end;

procedure TAdvReader.SetExcludeProperties(
  const Value: TAdvExcludePropertyListArray);
begin
  FExcludeProperties := Value;
  TAdvPersistence.ExcludeProperties := FExcludeProperties;
end;

procedure TAdvReader.SetIOReference(const Value: TObject);
begin
  FIOReference := Value;
  TAdvPersistence.IOReference := FIOReference;
end;

procedure TAdvReader.SetRootObject(const Value: TObject);
begin
  FRootObject := Value;
  TAdvPersistence.RootObject := FRootObject;
end;

procedure TAdvReader.SolveReferences;
var
  b: IAdvPersistenceIO;
  r: Integer;
  rf: TAdvObjectReference;
  o: TObject;
begin
  if Assigned(IOReference) and Supports(IOReference, IAdvPersistenceIO, b) then
  begin
    for r := 0 to FReferences.Count - 1 do
    begin
      rf := FReferences[r];
      o := b.FindObject(rf.Id);
      SetObjectProp(rf.Instance, rf.Prop, o);
    end;
  end;
end;

{ TAdvObjectReference }

constructor TAdvObjectReference.Create(AInstance: TObject;
  AProp: TAdvPropertyInfo; const AId: string);
begin
  Instance := AInstance;
  Prop := AProp;
  Id := AId;
end;

{ TAdvPersistence }

class procedure TAdvPersistence.LoadSettingsFromFile(AObject: TObject; AFileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    {$IFNDEF WEBLIB}
    ms.LoadFromFile(AFileName);
    {$ENDIF}
    LoadSettingsFromStream(AObject, ms);
  finally
    ms.Free;
  end;
end;

class procedure TAdvPersistence.LoadSettingsFromStream(AObject: TObject; AStream: TStreamEx);
var
  Reader: TAdvReader;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
begin
  AStream.Position := 0;
  Reader := TAdvReader.Create(AStream);
  t := FormatSettings.ThousandSeparator;
  d := FormatSettings.DecimalSeparator;
  try
    Reader.IOReference := TAdvPersistence.IOReference;
    Reader.RootObject := TAdvPersistence.RootObject;
    Reader.OnCustomReadProperty := DoCustomReadProperty;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    Reader.Read(AObject);
  finally
    FormatSettings.DecimalSeparator := d;
    FormatSettings.ThousandSeparator := t;
    Reader.Free;
  end;
end;

class procedure TAdvPersistence.SaveSettingsToFile(AObject: TObject;
  AFileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveSettingsToStream(AObject, ms);
    ms.SaveToFile(AFileName);
  finally
    ms.Free;
  end;
end;

class procedure TAdvPersistence.SaveSettingsToStream(AObject: TObject;
  AStream: TStreamEx);
var
  Writer: TAdvWriter;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
begin
  Writer := TAdvWriter.Create(AStream);
  t := FormatSettings.ThousandSeparator;
  d := FormatSettings.DecimalSeparator;
  try
    Writer.IOReference := TAdvPersistence.IOReference;
    Writer.RootObject := TAdvPersistence.RootObject;
    Writer.OnCustomWriteProperty := DoCustomWriteProperty;
    FormatSettings.DecimalSeparator := '.';
    FormatSettings.ThousandSeparator := ',';
    Writer.Write(AObject);
  finally
    FormatSettings.DecimalSeparator := d;
    FormatSettings.ThousandSeparator := t;
    Writer.Free;
  end;
end;

{$HINTS OFF}
class procedure TAdvPersistence.GetEnumValues(AValues: TStrings; APropInfo: TAdvPropertyInfo);
var
  p: PTypeInfo;
  {$IFNDEF WEBLIB}
  su: PTypeData;
  {$IFNDEF LCLLIB}
  ct: PPTypeInfo;
  {$ENDIF}
  {$IFDEF LCLLIB}
  ct: PTypeInfo;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  pi: TTypeInfoInteger;
  ps: PTypeInfo;
  {$ENDIF}
  I: Integer;
  k: TTypeKind;
begin
  p := GetTypeInfoEx(APropInfo);
  {$IFNDEF WEBLIB}
  su := GetTypeData(p);
  if Assigned(su) then
  begin
    if p{$IFDEF LCLLIB}^{$ENDIF}.Kind = tkSet then
    begin
      ct := su^.CompType;
      if Assigned(ct) then
      begin
        k := ct^.Kind;
        case k of
          tkEnumeration:
          begin
            su := GetTypeData(ct{$IFNDEF LCLLIB}^{$ENDIF});
            for i := su^.MinValue to su^.MaxValue do
              AValues.Add(TAdvPersistence.GetEnumName(ct{$IFNDEF LCLLIB}^{$ENDIF},i));
          end;
        end;
      end;
    end
    else
    begin
      for i := su^.MinValue to su^.MaxValue do
        AValues.Add(TAdvPersistence.GetEnumName(p,i));
    end;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  if Assigned(p) and (p is TTypeInfoSet) then
    p := TTypeInfoSet(p).comptype;

  if Assigned(p) and (p is TTypeInfoInteger) then
  begin
   pi := TTypeInfoInteger(p);
   for i := pi.MinValue to pi.MaxValue do
     AValues.Add(TAdvPersistence.GetEnumName(p, i));
  end;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoDataTypeInfoClassType(APropInfo: TAdvPropertyInfo): TClass;
{$IFDEF WEBLIB}
var
  t: PTypeInfo;
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  Result := GetTypeData(APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF})^.ClassType
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := nil;
  if Assigned(APropInfo) and Assigned(APropInfo.typeinfo) then
  begin
    t := APropInfo.typeinfo;
    asm
      if (t.class){
        return t.class.ClassType();
      }
    end;
  end;
  {$ENDIF}
end;
{$HINTS ON}

class function TAdvPersistence.GetPropInfoDataTypeInfo(
  APropInfo: TAdvPropertyInfo): PTypeInfo;
begin
  {$IFNDEF WEBLIB}
  Result := GetTypeData(APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF})^.BaseType{$IFNDEF LCLLIB}^{$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := nil;
  if Assigned(APropInfo) then
    Result := APropInfo.typeinfo;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoName(APropInfo: TAdvPropertyInfo): string;
begin
  {$IFNDEF WEBLIB}
  Result := GetShortStringString(@APropInfo{$IFDEF LCLLIB}^{$ENDIF}.Name);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.name;
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoType(APropInfo: TAdvPropertyInfo): TTypeKind;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.PropType^{$IFNDEF LCLLIB}^{$ENDIF}.Kind;
  {$ENDIF}
  {$IFDEF WEBLIB}
  if Assigned(APropInfo.typeinfo) then
    Result := APropInfo.typeinfo.kind
  else
    Result := tkUnknown;
  {$ENDIF}
end;

class procedure TAdvPersistence.DoCustomReadProperty(AObject: TObject;
  APropertyName: string; APropertyKind: TTypeKind; AReader: TAdvJSONReader;
  var ACanRead: Boolean);
begin
  if Assigned(OnCustomReadProperty) then
    OnCustomReadProperty(AObject, APropertyName, APropertyKind, AReader, ACanRead);
end;

class procedure TAdvPersistence.DoCustomWriteProperty(AObject: TObject;
  APropertyName: string; APropertyKind: TTypeKind; AWriter: TAdvJSONWriter;
  var ACanWrite: Boolean);
begin
  if Assigned(OnCustomWriteProperty) then
    OnCustomWriteProperty(AObject, APropertyName, APropertyKind, AWriter, ACanWrite);
end;

class function TAdvPersistence.GetEnumName(ATypeInfo: PTypeInfo; AValue: Integer): string;
begin
  {$IFNDEF WEBLIB}
  Result := TypInfo.GetEnumName(ATypeInfo, AValue);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := TTypeInfoEnum(ATypeInfo).EnumType.IntToName[AValue];
  {$ENDIF}
end;

class function TAdvPersistence.GetPropInfoTypeName(APropInfo: TAdvPropertyInfo): string;
begin
  {$IFNDEF WEBLIB}
  Result := GetShortStringString(@APropInfo{$IFDEF LCLLIB}^{$ENDIF}.PropType^.Name);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := '';
  if Assigned(APropInfo.typeinfo) then
    Result := APropInfo.typeinfo.name;
  {$ENDIF}
end;

class function TAdvPersistence.IsList(AClass: TClass): boolean;
begin
  Result := IsDescendingClass(AClass, ['TList']);
end;

class function TAdvPersistence.IsAssignableProperty(AObject: TObject;
  APropInfo: TAdvPropertyInfo): Boolean;
var
  oProp: TObject;
  k: TTypeKind;
  pName: string;
begin
  Result := False;
  k := GetPropInfoType(APropInfo);
  if k in [tkClass] then
  begin
    pName := GetPropInfoName(APropInfo);
    oProp := GetObjectProp(AObject, pName);
    Result := (Assigned(oProp) and IsComponent(oProp.ClassType)) or not Assigned(oProp);
  end;
end;

class function TAdvPersistence.IsBitmap(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TBitmap', 'TPicture', 'TAdvBitmap'{$IFDEF WEBLIB}, 'TGraphic'{$ENDIF}]);
end;

class function TAdvPersistence.IsReadOnly(
  APropInfo: TAdvPropertyInfo): Boolean;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.SetProc = nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.setter = '';
  {$ENDIF}
end;

class function TAdvPersistence.IsStrings(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TStrings']);
end;

class function TAdvPersistence.IsStrokeKind(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAdvGraphicsStrokeKind');
end;

class function TAdvPersistence.IsTime(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TTime');
end;

class function TAdvPersistence.IsWriteOnly(APropInfo: TAdvPropertyInfo): Boolean;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.GetProc = nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.getter = '';
  {$ENDIF}
end;

class function TAdvPersistence.IsCollection(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TCollection']);
end;

class function TAdvPersistence.IsColor(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAlphaColor') or (APropertyName = 'TColor') or (APropertyName = 'TGraphicsColor');
end;

class function TAdvPersistence.IsComponent(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TComponent', 'TAdvCustomComponent']);
end;

class function TAdvPersistence.IsControl(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TControl']);
end;

class function TAdvPersistence.IsDate(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TDate');
end;

class function TAdvPersistence.IsDateTime(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TDateTime');
end;

class function TAdvPersistence.IsDescendingClass(AClass: TClass;
  AClassParentList: array of string): Boolean;
var
  cn: string;
  I: Integer;
begin
  if not Assigned(AClass) then
    Exit(False);
  repeat
    cn := AClass.ClassName;
    for I := 0 to Length(AClassParentList) - 1 do
    begin
      if (cn = AClassParentList[I]) then
        Exit(True);
    end;
    AClass := AClass.ClassParent;
  until not Assigned(AClass);
  Result := False;
end;

class function TAdvPersistence.IsFillKind(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAdvGraphicsFillKind');
end;

class function TAdvPersistence.IsGenericDictionary(AClass: TClass): Boolean;
var
  cn: string;
begin
  if not Assigned(AClass) then
    Exit(False);
  repeat
    cn := AClass.ClassName;
    if AnsiStartsStr('TDictionary<', cn) or AnsiStartsStr('TObjectDictionary<', cn) then
      Exit(True);
    AClass := AClass.ClassParent;
  until not Assigned(AClass);
  Result := False;
end;

class function TAdvPersistence.IsGenericList(AClass: TClass; AType: string = ''): Boolean;
var
  cn: string;
begin
  if not Assigned(AClass) then
    Exit(False);
  repeat
    cn := AClass.ClassName;
    if AnsiStartsStr('TList<', cn) or AnsiStartsStr('TObjectList<', cn) then
    begin
      if (AType = '') or ((AType <> '') and (Pos(LowerCase(AType), LowerCase(cn)) > 0) or (Pos(LowerCase(AType), LowerCase(cn)) > 0)) then
        Exit(True);
    end;
    AClass := AClass.ClassParent;
  until not Assigned(AClass);
  Result := False;
end;

class function TAdvPersistence.CreateObject(const AClassName: string;
  BaseClass: TClass): TObject;
var
  ObjType: TPersistentClass;
begin
  ObjType := GetClass(AClassName);
  if ObjType = nil then
    raise EAdvReaderException.CreateFmt('Type "%s" not found', [AClassName]);
  if not ObjType.InheritsFrom(TObject) then
    raise EAdvReaderException.Create('Type "%s" is not an class type');
  if BaseClass <> nil then
    if not ObjType.InheritsFrom(BaseClass) then
      raise EAdvReaderException.CreateFmt('Type "%s" does not inherit from %s',
        [AClassName, BaseClass.ClassName]);

  {$IFDEF CMNWEBLIB}
  if ObjType.InheritsFrom(TCustomControl) then
    Result := TCustomControlClass(ObjType).Create(nil)
  {$ENDIF}
  {$IFDEF FMXLIB}
  if ObjType.InheritsFrom(TControl) then
    Result := TControlClass(ObjType).Create(nil)
  {$ENDIF}
  else if ObjType.InheritsFrom(TComponent) then
    Result := TComponentClass(ObjType).Create(nil)
  else if ObjType.InheritsFrom(TCollectionItem) then
    Result := TCollectionItemClass(ObjType).Create(nil)
  else if ObjType.InheritsFrom(TPersistent) then
    Result := TPersistentClass(ObjType).Create
  else
    raise EAdvReaderException.CreateFmt('Type "%s" not supported', [AClassName]);
end;

{ TAdvObjectPersistence }

class procedure TAdvObjectPersistence.LoadObjectFromString(AObject: TObject; AString: string);
var
  ms: TStringStream;
begin
  ms := TStringStream.Create(AString{$IFDEF WEBLIB}, TEncoding.UTF8{$ENDIF});
  try
    TAdvPersistence.LoadSettingsFromStream(AObject, ms);
  finally
    ms.Free;
  end;
end;

class function TAdvObjectPersistence.SaveObjectToString(
  AObject: TObject): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(''{$IFDEF WEBLIB}, TEncoding.UTF8{$ENDIF});
  try
    TAdvPersistence.SaveSettingsToStream(AObject, ss);
    ss.Position := 0;
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

{$IFDEF FNCLIB}

{ TAdvJSONToClass }

class procedure TAdvJSONToClass.JSONValueToClass(AParentProperty: TAdvJSONToClassProperty; AJSONValue: TJSONValue);
var
  v: TJSONValue;
  n: string;
  p: TAdvJSONToClassProperty;
  sz: Integer;
  K: Integer;
  t: TAdvJSONToClassPropertyType;
  ci: TAdvJSONToClassItem;
begin
  if AJSONValue is TJSONObject then
  begin
    ci := TAdvJSONToClassItem.Create(AParentProperty);
    FClasses.Add(ci);

    sz := TAdvUtils.GetJSONObjectSize(AJSONValue as TJSONObject);
    for k := 0 to sz - 1 do
    begin
      n := TAdvUtils.GetJSONObjectName(AJSONValue as TJSONObject, k);
      v := TAdvUtils.GetJSONValue(AJSONValue, n);

      p := nil;
      if Assigned(v) then
      begin
        t := JSONValueToPropertyType(v);
        if t <> cptUndefined then
        begin
          p := TAdvJSONToClassProperty.Create(n, t);
          ci.Properties.Add(p);
        end;
      end;

      JSONValueToClass(p, v);
    end;
  end
  else if AJSONValue is TJSONArray then
  begin
    if TAdvUtils.GetJSONArraySize(AJSONValue as TJSONArray) > 0 then
    begin
      t := JSONValueToPropertyType(AJSONValue);
      v := TAdvUtils.GetJSONArrayItem(AJSONValue as TJSONArray, 0);
      if not Assigned(AParentProperty) and (t <> cptUndefined) then
      begin
        ci := TAdvJSONToClassItem.Create(nil);
        p := TAdvJSONToClassProperty.Create('Items', t);
        ci.Properties.Add(p);
        FClasses.Add(ci);
        AParentProperty := p;
      end;

      JSONValueToClass(AParentProperty, v);
    end
    else
    begin
      ci := TAdvJSONToClassItem.Create(AParentProperty);
      FClasses.Add(ci);
    end;
  end
  else
  begin

  end;
end;

class function TAdvJSONToClass.JSONValueToPropertyType(
  AJSONValue: TJSONValue): TAdvJSONToClassPropertyType;
var
  s: string;
  dt: TDateTime;
  b: Boolean;
  v: TJSONValue;
  p: TAdvJSONToClassPropertyType;
  d: Double;
  i: Integer;
  i64: Int64;
begin
  Result := cptUndefined;
  if not Assigned(AJSONValue) then
    Exit;

  if AJSONValue is TJSONObject then
    Result := cptObject
  else if AJSONValue is TJSONArray then
  begin
    Result := cptObjectArray;
    if TAdvUtils.GetJSONArraySize(AJSONValue as TJSONArray) > 0 then
    begin
      v := TAdvUtils.GetJSONArrayItem(AJSONValue as TJSONArray, 0);
      if Assigned(v) then
      begin
        p := JSONValueToPropertyType(v);
        case p of
          cptString: Result := cptStringArray;
          cptBoolean: Result := cptBooleanArray;
          cptDateTime: Result := cptDateTimeArray;
          cptObject: Result := cptObjectArray;
          cptDouble: Result := cptDoubleArray;
          cptInteger: Result := cptIntegerArray;
          cptInteger64: Result := cptInteger64Array;
        end;
      end;
    end;
  end
  else if AJSONValue is TJSONNumber then
  begin
    s := AJSONValue.Value;
    if TryStrToInt(s, i) then
      Result := cptInteger
    else if TryStrToInt64(s, i64) then
      Result := cptInteger64
    else if not TAdvUtils.TryStrToFloatDot(s, d) then
      Result := cptString
    else
      Result := cptDouble;
  end
  {$IFDEF LCLLIB}
  else if AJSONValue is TJSONBoolean then
    Result := cptBoolean
  {$ENDIF}
  {$IFNDEF LCLLIB}
  else if (AJSONValue is TJSONTrue) or (AJSONValue is TJSONFalse) then
    Result := cptBoolean
  {$ENDIF}
  else if AJSONValue is TJSONString then
  begin
    s := AJSONValue.Value;

    if TAdvUtils.TryStrToFloatDot(s, d) then
      Result := cptString
    else if TAdvUtils.IsDate(s, dt) or {$IFDEF FNCLIB}(TAdvUtils.ISOToDateTime(s, True) > 0) or{$ENDIF} TryStrToDateTime(s, dt) then
      Result := cptDateTime
    else if TryStrToBool(s, b) then
      Result := cptBoolean
    else
      Result:= cptString;
  end;
end;

class function TAdvJSONToClass.ExportToDelphiFromFile(AFileName: string; ACallBack: TAdvJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphiFromFile(AFileName, DefaultJSONToClassOptions, ACallBack);
end;

class function TAdvJSONToClass.ExportToDelphi(AJSONString: string; ACallBack: TAdvJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphi(AJSONString, DefaultJSONToClassOptions, ACallBack);
end;

class function TAdvJSONToClass.ExportToDelphi(AJSONValue: TJSONValue; ACallBack: TAdvJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphi(AJSONValue.ToString, DefaultJSONToClassOptions, ACallBack);
end;

class function TAdvJSONToClass.ExportToDelphiFromFile(AFileName: string; AOptions: TAdvJSONToClassOptions; ACallBack: TAdvJSONToClassExportEvent = nil): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    Result := ExportToDelphi(sl.Text, AOptions, ACallBack);
  finally
    sl.Free;
  end;
end;

function CompareNameVal(const Item1, Item2: TAdvJSONToClassProperty): Integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
end;

class function TAdvJSONToClass.ExportToDelphi(AJSONValue: TJSONValue; AOptions: TAdvJSONToClassOptions; ACallBack: TAdvJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphi(AJSONValue.ToString, AOptions, ACallBack);
end;

class function TAdvJSONToClass.ExportToDelphi(AJSONString: string; AOptions: TAdvJSONToClassOptions; ACallBack: TAdvJSONToClassExportEvent = nil): string;
var
  root: TJSONValue;
  I, K: Integer;
  c: TAdvJSONToClassItem;
  sl: TStringList;
  p: TAdvJSONToClassProperty;
  ind: string;
  cn: string;
  slRendered: TStringList;

  procedure SortList(const AList: TAdvJSONToClassProperties);
  {$IFNDEF LCLWEBLIB}
  var
    cmp: TAdvJSONToClassPropertyNameComparer;
  {$ENDIF}
  begin
    {$IFNDEF LCLWEBLIB}
    cmp := TAdvJSONToClassPropertyNameComparer.Create;
    try
      AList.Sort(cmp);
    finally
      cmp.Free;
    end;
    {$ENDIF}
    {$IFDEF LCLLIB}
    AList.Sort(@CompareNameVal);
    {$ENDIF}
    {$IFDEF WEBLIB}
    AList.Sort(TComparer<TAdvJSONToClassProperty>.Construct(
      function(const ALeft, ARight: TAdvJSONToClassProperty): Integer
      begin
        Result := CompareText(ALeft.Name, ARight.Name);
      end));
    {$ENDIF}
  end;
begin
  root := TAdvUtils.ParseJSON(AJSONString);
  if Assigned(root) then
  begin
    try
      FClasses := TAdvJSONToClassItems.Create;
      try
        JSONValueToClass(nil, root);

        sl := TStringList.Create;
        slRendered := TStringList.Create;
        try
          {$IFNDEF LCLLIB}
          {$HINTS OFF}
          {$IF COMPILERVERSION > 30}
          sl.TrailingLineBreak := False;
          {$IFEND}
          {$HINTS ON}
          {$ENDIF}

          ind := '';
          if AOptions.AddUnit then
          begin
            sl.Add('unit MyDelphiUnit;');
            sl.Add('');
            sl.Add('interface');
            sl.Add('');
            sl.Add('uses');
            sl.Add('  Classes, System.Generics.Collections;');
            sl.Add('');
            sl.Add('type');
            ind := '  ';
          end;

          for I := FClasses.Count - 1 downto 0 do
          begin
            c := FClasses[I];
            cn := c.GetClassName(ACallBack, AOptions);
            if slRendered.IndexOf(cn) = -1 then
            begin
              case AOptions.BaseClass of
                cbcNone: sl.Add(ind + cn + ' = class');
                cbcPersistent: sl.Add(ind + cn + ' = class(TPersistent)');
              end;

              if AOptions.SortProperties then
                SortList(c.Properties);


              if c.Properties.Count > 0 then
              begin
                sl.Add(ind + 'private');
                for K := 0 to c.Properties.Count - 1 do
                begin
                  p := c.Properties[K];
                  sl.Add(ind + '  F' + p.DelphiName(ACallBack, AOptions, False) + ': ' + p.DelphiType(ACallBack, AOptions) + ';');
                end;
              end;

              if AOptions.AddConstructor or AOptions.AddDestructor or AOptions.AddAssign then
              begin
                sl.Add(ind + 'public');

                if AOptions.AddAssign then
                begin
                  case AOptions.BaseClass of
                    cbcNone: sl.Add(ind + '  procedure Assign(Source: TObject);');
                    cbcPersistent: sl.Add(ind + '  procedure Assign(Source: TPersistent); override;');
                  end;
                end;

                if AOptions.AddConstructor then
                  sl.Add(ind + '  constructor Create;');

                if AOptions.AddDestructor then
                  sl.Add(ind + '  destructor Destroy; override;');
              end;

              if c.Properties.Count > 0 then
              begin
                sl.Add(ind + 'published');
                for K := 0 to c.Properties.Count - 1 do
                begin
                  p := c.Properties[K];
                  sl.Add(ind + '  property ' + p.DelphiName(ACallBack, AOptions) + ': ' + p.DelphiType(ACallBack, AOptions) + ' read F' + p.DelphiName(ACallBack, AOptions, False) + ' write F' + p.DelphiName(ACallBack, AOptions, False) + ';');
                end;
              end;

              sl.Add(ind + 'end;');
              sl.Add('');

              slRendered.Add(cn);
            end;
          end;

          slRendered.Clear;

          if AOptions.AddUnit then
          begin
            sl.Add('implementation');
            sl.Add('');
            if AOptions.AddImplementation then
            begin
              slRendered.Clear;

              for I := FClasses.Count - 1 downto 0 do
              begin
                c := FClasses[I];

                cn := c.GetClassName(ACallBack,AOptions);
                if slRendered.IndexOf(cn) = -1 then
                begin
                  if AOptions.AddConstructor or AOptions.AddDestructor or AOptions.AddAssign then
                  begin
                    sl.Add('{ ' + c.GetClassName(ACallBack, AOptions)  + ' }');
                    sl.Add('');

                    if AOptions.AddAssign then
                    begin
                      case AOptions.BaseClass of
                        cbcNone: sl.Add('procedure ' + cn + '.Assign(Source: TObject);');
                        cbcPersistent: sl.Add('procedure ' + cn + '.Assign(Source: TPersistent);');
                      end;

                      sl.Add('begin');
                      sl.Add('  if (Source is ' + cn + ') then');
                      sl.Add('  begin');
                      for K := 0 to c.Properties.Count - 1 do
                      begin
                        p := c.Properties[K];
                        if (p.&Type = cptObject) then
                          sl.Add('    F' + p.DelphiName(ACallBack, AOptions, False) + '.Assign((Source as ' + cn + ').' + p.DelphiName(ACallBack, AOptions, True) + ');')
                        else if not p.IsObject then
                          sl.Add('    F' + p.DelphiName(ACallBack, AOptions, False) + ' := (Source as ' + cn + ').' + p.DelphiName(ACallBack, AOptions, True) + ';')
                        else
                          sl.Add('    //F' + p.DelphiName(ACallBack, AOptions, False) + ' := (Source as ' + cn + ').' + p.DelphiName(ACallBack, AOptions, True) + '; //implement list copy');
                      end;
                      sl.Add('  end;');
                      sl.Add('end;');
                      sl.Add('');
                    end;

                    if AOptions.AddConstructor then
                    begin
                      sl.Add('constructor ' + cn + '.Create;');
                      sl.Add('begin');
                      sl.Add('  inherited;');
                      for K := 0 to c.Properties.Count - 1 do
                      begin
                        p := c.Properties[K];
                        if p.IsObject then
                          sl.Add('  F' + p.DelphiName(ACallBack, AOptions, False) + ' := ' + p.DelphiType(ACallBack, AOptions) + '.Create;');
                      end;
                      sl.Add('end;');
                      sl.Add('');
                    end;

                    if AOptions.AddDestructor then
                    begin
                      sl.Add('destructor ' + cn + '.Destroy;');
                      sl.Add('begin');
                      for K := 0 to c.Properties.Count - 1 do
                      begin
                        p := c.Properties[K];
                        if p.IsObject then
                          sl.Add('  F' + p.DelphiName(ACallBack, AOptions, False) + '.Free;');
                      end;
                      sl.Add('  inherited;');
                      sl.Add('end;');
                      sl.Add('');
                    end;

                    slRendered.Add(cn);
                  end;
                end;
              end;
            end;

            sl.Add('end.');
          end;

          while sl.Count > 0 do
          begin
            if sl[sl.Count - 1] = '' then
              sl.Delete(sl.Count - 1)
            else
              Break;
          end;

          Result := sl.Text;
        finally
          slRendered.Free;
          sl.Free;
        end;

      finally
        FClasses.Free;
      end;
    finally
      root.Free;
    end;
  end;
end;

{ TAdvJSONToClassItem }

constructor TAdvJSONToClassItem.Create(
  AParentProperty: TAdvJSONToClassProperty);
begin
  FParentProperty := AParentProperty;
end;

destructor TAdvJSONToClassItem.Destroy;
begin
  if Assigned(FProperties) then
    FProperties.Free;
  inherited;
end;

function TAdvJSONToClassItem.GetClassName(ACallBack: TAdvJSONToClassExportEvent; AOptions: TAdvJSONToClassOptions): string;
begin
  Result := 'TMyDelphiClass';
  if Assigned(ParentProperty) then
    Result := 'T' + ParentProperty.DelphiName(ACallBack, AOptions, False);
end;

function TAdvJSONToClassItem.GetProperties: TAdvJSONToClassProperties;
begin
  if not Assigned(FProperties) then
    FProperties := TAdvJSONToClassProperties.Create;

  Result := FProperties;
end;

{ TAdvJSONToClassProperty }

constructor TAdvJSONToClassProperty.Create(AName: string;
  AType: TAdvJSONToClassPropertyType);
begin
  FName := AName;
  FType := AType;
end;

function TAdvJSONToClassProperty.DelphiName(ACallBack: TAdvJSONToClassExportEvent; AOptions: TAdvJSONToClassOptions; AFixKeyWord: Boolean = True): string;
var
  n, s: string;
  c: Char;
  sl: TStringList;
  I: Integer;
  p: string;
  pt: TAdvJSONToClassPropertyType;
begin
  Result := '';
  if Length(Name) > 0 then
  begin
    p := Name;
    pt := &Type;

    if Assigned(ACallBack) then
      ACallBack(p, pt);

    sl := TStringList.Create;
    try

      if AOptions.RemoveSpecialCharacters then
      begin
        s := p;
        p := '';

        for c in s do
        begin
          if TAdvUtils.IsLetterOrDigit(c) then
            p := p + c
          else
            p := p + '_'
        end;
      end;

      TAdvUtils.Split('_', p, sl);

      for I := 0 to sl.Count - 1 do
      begin
        n := sl[I];

        if Length(n) > 0 then
        begin
          if AOptions.DelphiCasing then
            n := UpperCase(n[{$IFNDEF ZEROSTRINGINDEX}1{$ELSE}0{$ENDIF}]) + Copy(n, 2, Length(n) - 1);
        end;

        Result := Result + n;
      end;

      if AFixKeyWord then
        Result := FixKeyWord(Result);

    finally
      sl.Free;
    end;
  end;
end;

function TAdvJSONToClassProperty.DelphiType(ACallBack: TAdvJSONToClassExportEvent; AOptions: TAdvJSONToClassOptions): string;
var
  n: string;
  t: TAdvJSONToClassPropertyType;
begin
  Result := '';

  n := Name;
  t := FType;
  if Assigned(ACallBack) then
    ACallBack(n, t);

  case t of
    cptUndefined: Result := 'UNDEFINED';
    cptString: Result := 'string';
    cptBoolean: Result := 'Boolean';
    cptDateTime: Result := 'TDateTime';
    cptObject: Result := 'T' + DelphiName(ACallback, AOptions, False);
    cptDouble: Result := 'Double';
    cptInteger: Result := 'Integer';
    cptInteger64: Result := 'Int64';
    cptObjectArray: Result := 'TObjectList<' + 'T' + DelphiName(ACallback, AOptions, False) + '>';
    cptDoubleArray: Result := 'TList<Double>';
    cptStringArray: Result := 'TList<string>';
    cptBooleanArray: Result := 'TList<Boolean>';
    cptDateTimeArray: Result := 'TList<TDateTime>';
    cptIntegerArray: Result := 'TList<Integer>';
    cptInteger64Array: Result := 'TList<Int64>';
  end;
end;

function TAdvJSONToClassProperty.FixKeyWord(AValue: string): string;
const
  a: array[0..64] of string = ('and','array', 'as', 'asm', 'begin',
                               'case', 'class', 'const', 'constructor', 'destructor',
                               'dispinterface', 'div',	'do',	'downto',	'else',
                               'end',	'except',	'exports',	'file',	'finalization',
                               'finally',	'for',	'function',	'goto',	'if',
                               'implementation',	'in',	'inherited',	'initialization',	'inline',
                               'interface',	'is',	'label',	'library', 'mod',
                               'nil',	'not',	'object',	'of',	'or',
                               'out',	'packed',	'procedure',	'program', 'property',
                               'raise',	'record',	'repeat',	'resourcestring',	'set',
                               'shl',	'shr',	'string',	'then',	'threadvar',
                               'to',	'try',	'type',	'unit',	'until',
                               'uses',	'var',	'while',	'with',	'xor');
begin
  Result := AValue;
  if TAdvUtils.IndexOfTextInArray(UpperCase(AValue), a) <> -1 then
    Result := '&' + AValue;
end;

function TAdvJSONToClassProperty.IsObject: Boolean;
begin
  Result := &Type in [cptObject, cptObjectArray, cptStringArray, cptBooleanArray, cptDateTimeArray, cptDoubleArray, cptIntegerArray, cptInteger64Array];
end;

{ TAdvJSONToClassPropertyNameComparer }

{$IFNDEF LCLWEBLIB}
function TAdvJSONToClassPropertyNameComparer.Compare(const Left,
  Right: TAdvJSONToClassProperty): Integer;
begin
  Result := CompareText(Left.Name, Right.Name);
end;
{$ENDIF}
{$ENDIF}


initialization
begin
  TAdvPersistence.ClassTypeVariable := '$type';
end;

end.


