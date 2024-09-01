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

unit FMX.TMSFNCPersistence;

{$I FMX.TMSFNCDefines.inc}

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
  FMX.TMSFNCTypes,
  FMX.TMSFNCJSONReader,
  FMX.TMSFNCJSONWriter
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

  ITMSFNCPersistence = interface
  ['{363F04AF-B8A7-4C47-A2D6-8ED9C44CEFF6}']
    procedure SaveSettingsToFile(AFileName: string; AAppearanceOnly: Boolean = False);
    procedure LoadSettingsFromFile(AFileName: string);
    procedure SaveSettingsToStream(AStream: TStreamEx; AAppearanceOnly: Boolean = False);
    procedure LoadSettingsFromStream(AStream: TStreamEx);
    function CanSaveProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
    function CanLoadProperty(AObject: TObject; APropertyName: string; APropertyType: TTypeKind): Boolean;
  end;

  ITMSFNCBaseListIO = interface
  ['{FAB1D21E-D798-4CE0-B17B-9D75E4456AB4}']
    function GetItemClass: TClass;
  end;

  ITMSFNCBasePersistenceIO = interface
    ['{91DEAFC3-8932-45F4-A3ED-5AAA0C0E9250}']
    function CreateObject(const AClassName: string; const ABaseClass: TClass): TObject;
  end;

  ITMSFNCBaseCollectionIO = interface
  ['{90FDF257-7362-411D-B7F6-E2BEE2265016}']
    function AddItem(const AObject: TObject): TCollectionItem;
  end;

  ITMSFNCPersistenceIO = interface(ITMSFNCBasePersistenceIO)
  ['{11B625F8-447A-4AE5-BB88-5ECDEA979AF7}']
    function NeedsObjectReference(const AClass: TClass): Boolean;
    function GetObjectReference(const AObject: TObject): string;
    function FindObject(const AReference: string): TObject;
    procedure FixOwners(const AObject: TObject; const AObjectList: TObject);
  end;

  ETMSFNCReaderException = class(Exception)
  end;

  {$IFDEF WEBLIB}
  TTMSFNCPropertyInfo = TTypeMemberProperty;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCPropertyInfo = PPropInfo;
  {$ENDIF}

  TTMSFNCObjectDictionary = class(TDictionary<string, TObject>);
  TTMSFNCObjectList = class(TObjectList<TObject>);
  TTMSFNCStringList = class(TList<string>);
  TTMSFNCIntegerList = class(TList<Integer>);
  TTMSFNCDoubleList = class(TList<Double>);

  TTMSFNCObjectArray = array of TObject;

  TTMSFNCWriterCustomWritePropertyEvent = procedure(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AWriter: TTMSFNCJSONWriter; var ACanWrite: Boolean) of object;
  TTMSFNCWriterCustomIsAssignablePropertyEvent = procedure(AObject: TObject; APropertyName: string; var AIsAssignable: Boolean) of object;

  TTMSFNCExcludePropertyListArray = array of string;

  TTMSFNCWriter = class
  private
    FWriter: TTMSFNCJSONWriter;
    FIOReference: TObject;
    FOnCustomWriteProperty: TTMSFNCWriterCustomWritePropertyEvent;
    FRootObject: TObject;
    FExcludeProperties: TTMSFNCExcludePropertyListArray;
    FOnCustomIsAssignableProperty: TTMSFNCWriterCustomIsAssignablePropertyEvent;
    procedure SetRootObject(const Value: TObject);
    procedure SetExcludeProperties(
      const Value: TTMSFNCExcludePropertyListArray);
    procedure SetIOReference(const Value: TObject);
    property Writer: TTMSFNCJSONWriter read FWriter;
    procedure WritePropInfoValue(AInstance: TObject; const APropInfo: TTMSFNCPropertyInfo);
    procedure WriteProperties(AObject: TObject);
    procedure WriteProperty(AObject: TObject; AProp: TTMSFNCPropertyInfo);
    procedure WriteGenericObjectList(AList: TTMSFNCObjectList);
    procedure WriteGenericStringList(AList: TTMSFNCStringList);
    procedure WriteGenericIntegerList(AList: TTMSFNCIntegerList);
    procedure WriteGenericDoubleList(AList: TTMSFNCDoubleList);
    procedure WriteStrings(AList: TStrings);
    procedure WriteGenericDictionary(ADictionary: TTMSFNCObjectDictionary);
    procedure WriteCollection(ACollection: TCollection);
    procedure WriteList(AList: TList);
    procedure WriteBitmap(ABitmap: TTMSFNCBitmap);
    procedure WriteSingleObject(AObject: TObject);
    procedure WriteObject(AObject: TObject);
  public
    constructor Create(AStream: TStreamEx);
    destructor Destroy; override;
    procedure Write(AObject: TObject);
    procedure WriteArray(AName: string; AArray: TTMSFNCObjectArray);
    property JSONWriter: TTMSFNCJSONWriter read FWriter;
    property IOReference: TObject read FIOReference write SetIOReference;
    property RootObject: TObject read FRootObject write SetRootObject;
    property ExcludeProperties: TTMSFNCExcludePropertyListArray read FExcludeProperties write SetExcludeProperties;
    property OnCustomWriteProperty: TTMSFNCWriterCustomWritePropertyEvent read FOnCustomWriteProperty write FOnCustomWriteProperty;
    property OnCustomIsAssignableProperty: TTMSFNCWriterCustomIsAssignablePropertyEvent read FOnCustomIsAssignableProperty write FOnCustomIsAssignableProperty;
  end;

  TTMSFNCReaderCustomReadPropertyEvent = procedure(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AReader: TTMSFNCJSONReader; var ACanRead: Boolean) of object;

  TTMSFNCObjectReference = class
  public
    Instance: TObject;
    Prop: TTMSFNCPropertyInfo;
    Id: string;
    constructor Create(AInstance: TObject; AProp: TTMSFNCPropertyInfo; const AId: string);
  end;

  TTMSFNCObjectReferences = TObjectList<TTMSFNCObjectReference>;

  TTMSFNCReader = class
  private
    FReferences: TTMSFNCObjectReferences;
    FReader: TTMSFNCJSONReader;
    FIOReference: TObject;
    FOnCustomReadProperty: TTMSFNCReaderCustomReadPropertyEvent;
    FRootObject: TObject;
    FExcludeProperties: TTMSFNCExcludePropertyListArray;
    FOnCustomIsAssignableProperty: TTMSFNCWriterCustomIsAssignablePropertyEvent;
    function ReadSingleObject(ABaseClass: TClass): TObject; overload;
    procedure SetRootObject(const Value: TObject);
    procedure SetExcludeProperties(
      const Value: TTMSFNCExcludePropertyListArray);
    procedure SetIOReference(const Value: TObject);
    property Reader: TTMSFNCJSONReader read FReader;
    procedure ReadSingleObject(AObject: TObject); overload;
    procedure ReadProperties(AObject: TObject);
    procedure ReadProperty(AObject: TObject; AProp: TTMSFNCPropertyInfo);
    procedure ReadPropInfoValue(AInstance: TObject; const APropInfo: TTMSFNCPropertyInfo);
    procedure ReadExistingObject(AObject: TObject);
    procedure ReadGenericStringList(AList: TTMSFNCStringList);
    procedure ReadGenericDoubleList(AList: TTMSFNCDoubleList);
    procedure ReadGenericIntegerList(AList: TTMSFNCIntegerList);
    procedure ReadStrings(AList: TStrings);
    procedure ReadGenericObjectList(AList: TTMSFNCObjectList);    
    procedure ReadGenericDictionary(ADictionary: TTMSFNCObjectDictionary);
    procedure ReadCollection(ACollection: TCollection);
    procedure ReadList(AList: TList);
    procedure ReadBitmap(ABitmap: TTMSFNCBitmap);
    procedure ReadObject(AObject: TObject);
  public
    constructor Create(AStream: TStreamEx);
    destructor Destroy; override;
    function Read(AClass: TClass): TObject; overload;
    procedure Read(AObject: TObject); overload;
    function ReadArray(AName: string): TTMSFNCObjectArray; overload;
    property JSONReader: TTMSFNCJSONReader read FReader;
    property IOReference: TObject read FIOReference write SetIOReference;
    property RootObject: TObject read FRootObject write SetRootObject;
    property ExcludeProperties: TTMSFNCExcludePropertyListArray read FExcludeProperties write SetExcludeProperties;
    property OnCustomReadProperty: TTMSFNCReaderCustomReadPropertyEvent read FOnCustomReadProperty write FOnCustomReadProperty;
    property OnCustomIsAssignableProperty: TTMSFNCWriterCustomIsAssignablePropertyEvent read FOnCustomIsAssignableProperty write FOnCustomIsAssignableProperty;
    procedure SolveReferences;
  end;

  {$IFDEF WEBLIB}
  PTypeInfo = TypInfo.TTypeInfo;
  {$ELSE}
  PTypeInfo = TypInfo.PTypeInfo;
  {$ENDIF}

  TTMSFNCObjectPersistence = class
  public
    class function SaveObjectToString(AObject: TObject): string;
    class procedure LoadObjectFromString(AObject: TObject; AString: string);
  end;

  TTMSFNCPersistence = class
  public class var
    ClassTypeVariable: string;
  private
    class var FOnCustomReadProperty: TTMSFNCReaderCustomReadPropertyEvent;
    class var FOnCustomWriteProperty: TTMSFNCWriterCustomWritePropertyEvent;
    class var FRootObject: TObject;
    class var FExcludeProperties: TTMSFNCExcludePropertyListArray;
    class var FIOReference: TObject;
    class procedure DoCustomReadProperty(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AReader: TTMSFNCJSONReader; var ACanRead: Boolean);
    class procedure DoCustomWriteProperty(AObject: TObject; APropertyName: string; APropertyKind: TTypeKind; AWriter: TTMSFNCJSONWriter; var ACanWrite: Boolean);
  public
    class procedure SaveSettingsToFile(AObject: TObject; AFileName: string);
    class procedure LoadSettingsFromFile(AObject: TObject; AFileName: string);
    class procedure SaveSettingsToStream(AObject: TObject; AStream: TStreamEx);
    class procedure LoadSettingsFromStream(AObject: TObject; AStream: TStreamEx);
    class procedure GetEnumValues(AValues: TStrings; APropInfo: TTMSFNCPropertyInfo);
    class function CreateObject(const AClassName: string; BaseClass: TClass): TObject;
    class function GetPropInfoDataTypeInfo(APropInfo: TTMSFNCPropertyInfo): PTypeInfo;
    class function GetPropInfoDataTypeInfoClassType(APropInfo: TTMSFNCPropertyInfo): TClass;
    class function GetPropInfoType(APropInfo: TTMSFNCPropertyInfo): TTypeKind; virtual;
    class function GetPropInfoName(APropInfo: TTMSFNCPropertyInfo): string; virtual;
    class function GetPropInfoTypeName(APropInfo: TTMSFNCPropertyInfo): string;
    class function GetEnumName(ATypeInfo: PTypeInfo; AValue: Integer): string;
    class function IsWriteOnly(APropInfo: TTMSFNCPropertyInfo): Boolean; virtual;
    class function IsReadOnly(APropInfo: TTMSFNCPropertyInfo): Boolean; virtual;
    class function IsAssignableProperty(AObject: TObject; APropInfo: TTMSFNCPropertyInfo): Boolean; virtual;
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
    class property OnCustomWriteProperty: TTMSFNCWriterCustomWritePropertyEvent read FOnCustomWriteProperty write FOnCustomWriteProperty;
    class property OnCustomReadProperty: TTMSFNCReaderCustomReadPropertyEvent read FOnCustomReadProperty write FOnCustomReadProperty;
    class property RootObject: TObject read FRootObject write FRootObject;
    class property ExcludeProperties: TTMSFNCExcludePropertyListArray read FExcludeProperties write FExcludeProperties;
    class property IOReference: TObject read FIOReference write FIOReference;
  end;

  {$IFDEF FNCLIB}
  TTMSFNCJSONToClassPropertyType = (cptUndefined, cptString, cptBoolean, cptDateTime, cptObject, cptDouble, cptInteger,
    cptInteger64, cptObjectArray, cptStringArray, cptBooleanArray, cptDateTimeArray, cptDoubleArray, cptIntegerArray, cptInteger64Array);

  TTMSFNCJSONToClassBaseClass = (cbcNone, cbcPersistent);

  TTMSFNCJSONToClassOptions = record
    SortProperties: Boolean;
    DelphiCasing: Boolean;
    RemoveSpecialCharacters: Boolean;
    AddConstructor: Boolean;
    AddDestructor: Boolean;
    AddAssign: Boolean;
    AddUnit: Boolean;
    BaseClass: TTMSFNCJSONToClassBaseClass;
    AddImplementation: Boolean;
  end;

  TTMSFNCJSONToClassExportEvent = {$IFNDEF LCLLIB}reference to {$ENDIF}procedure(var APropertyName: string; var APropertyType: TTMSFNCJSONToClassPropertyType){$IFDEF LCLLIB} of object{$ENDIF};

  TTMSFNCJSONToClassProperty = class
  private
    FName: string;
    FType: TTMSFNCJSONToClassPropertyType;
    function FixKeyWord(AValue: string): string;
  public
    function IsObject: Boolean;
    function DelphiName(ACallBack: TTMSFNCJSONToClassExportEvent; AOptions: TTMSFNCJSONToClassOptions; AFixKeyWord: Boolean = True): string;
    function DelphiType(ACallBack: TTMSFNCJSONToClassExportEvent; AOptions: TTMSFNCJSONToClassOptions): string;
    constructor Create(AName: string; AType: TTMSFNCJSONToClassPropertyType);
    property Name: string read FName write FName;
    property &Type: TTMSFNCJSONToClassPropertyType read FType write FType;
  end;

  TTMSFNCJSONToClassProperties = class(TObjectList<TTMSFNCJSONToClassProperty>);

  TTMSFNCJSONToClassItem = class
  private
    FProperties: TTMSFNCJSONToClassProperties;
    FParentProperty: TTMSFNCJSONToClassProperty;
    function GetProperties: TTMSFNCJSONToClassProperties;
  public
    function GetClassName(ACallBack: TTMSFNCJSONToClassExportEvent; AOptions: TTMSFNCJSONToClassOptions): string;
    constructor Create(AParentProperty: TTMSFNCJSONToClassProperty);
    destructor Destroy; override;
    property Properties: TTMSFNCJSONToClassProperties read GetProperties;
    property ParentProperty: TTMSFNCJSONToClassProperty read FParentProperty;
  end;

  TTMSFNCJSONToClassItems = class(TObjectList<TTMSFNCJSONToClassItem>);

  TTMSFNCJSONToClass = class
  private
    class var FClasses: TTMSFNCJSONToClassItems;
  protected
    class procedure JSONValueToClass(AParentProperty: TTMSFNCJSONToClassProperty; AJSONValue: TJSONValue);
    class function JSONValueToPropertyType(AJSONValue: TJSONValue): TTMSFNCJSONToClassPropertyType;
  public
    class function ExportToDelphi(AJSONString: string; AOptions: TTMSFNCJSONToClassOptions; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphi(AJSONString: string; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphi(AJSONValue: TJSONValue; AOptions: TTMSFNCJSONToClassOptions; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphi(AJSONValue: TJSONValue; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphiFromFile(AFileName: string; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string; overload;
    class function ExportToDelphiFromFile(AFileName: string; AOptions: TTMSFNCJSONToClassOptions; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string; overload;
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
     'BitmapContainer',
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
function DefaultJSONToClassOptions: TTMSFNCJSONToClassOptions;
{$ENDIF}

implementation

uses
  {$IFDEF FMXLIB}
  UITypes,
  {$ENDIF}
  DateUtils,
  StrUtils,
  FMX.Controls,
  FMX.Graphics,
  {$IFNDEF LCLLIB}
  Generics.Defaults,
  {$ENDIF}
  FMX.TMSFNCUtils;

const
  {$IFDEF FMXLIB}
  gcNull = $00000000;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  gcNull = -1;
  {$ENDIF}

type
  {$IFDEF FMXLIB}
  TTMSFNCPersistenceColor = TAlphaColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TTMSFNCPersistenceColor = TColor;
  {$ENDIF}

{$IFDEF FNCLIB}
type
  {$IFNDEF LCLWEBLIB}
  TTMSFNCJSONToClassPropertyNameComparer = class(TComparer<TTMSFNCJSONToClassProperty>)
  public
    function Compare(const Left, Right: TTMSFNCJSONToClassProperty): Integer; override;
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

function GetTypeInfoEx(APropInfo: TTMSFNCPropertyInfo): PTypeInfo;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.PropType{$IFNDEF LCLLIB}^{$ENDIF};
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.typeinfo;
  {$ENDIF}
end;

function GetColorRed(AColor: TTMSFNCPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).R;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetRValue(AColor);
  {$ENDIF}
end;

function GetColorGreen(AColor: TTMSFNCPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).G;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetGValue(AColor);
  {$ENDIF}
end;

function GetColorBlue(AColor: TTMSFNCPersistenceColor): Byte;
begin
  {$IFDEF FMXLIB}
  Result := TAlphaColorRec(AColor).B;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  AColor := ColorToRGB(AColor);
  Result := GetBValue(AColor);
  {$ENDIF}
end;

function HTMLToColorEx(AHTML: string): TTMSFNCPersistenceColor;

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
  Alpha = TTMSFNCPersistenceColor($FF000000);
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(AHTML,2,2)) shl 16;
  g := Hexval(Copy(AHTML,4,2)) shl 8;
  b := Hexval(Copy(AHTML,6,2));
  Result := Alpha or TTMSFNCPersistenceColor(b + g + r);
{$ENDIF}
end;

function ColorToHTMLEx(AColor: TTMSFNCPersistenceColor): string;
const
  HTMLHexColor = '#RRGGBB';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';
var
  c: TTMSFNCPersistenceColor;
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
function DefaultJSONToClassOptions: TTMSFNCJSONToClassOptions;
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

{ TTMSFNCWriter }

constructor TTMSFNCWriter.Create(AStream: TStreamEx);
begin
  FWriter := TTMSFNCJSONWriter.Create(AStream);
end;

destructor TTMSFNCWriter.Destroy;
begin
  FWriter.Free;
  inherited;
end;

procedure TTMSFNCWriter.SetExcludeProperties(
  const Value: TTMSFNCExcludePropertyListArray);
begin
  FExcludeProperties := Value;
  TTMSFNCPersistence.ExcludeProperties := FExcludeProperties;
end;

procedure TTMSFNCWriter.SetIOReference(const Value: TObject);
begin
  FIOReference := Value;
  TTMSFNCPersistence.IOReference := FIOReference;
end;

procedure TTMSFNCWriter.SetRootObject(const Value: TObject);
begin
  FRootObject := Value;
  TTMSFNCPersistence.RootObject := FRootObject;
end;

procedure TTMSFNCWriter.WriteGenericDictionary(
  ADictionary: TTMSFNCObjectDictionary);
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

procedure TTMSFNCWriter.WriteGenericDoubleList(AList: TTMSFNCDoubleList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteDouble(AList[I]);
  Writer.WriteEndArray;
end;

procedure TTMSFNCWriter.WriteGenericIntegerList(AList: TTMSFNCIntegerList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteInteger(AList[I]);
  Writer.WriteEndArray;
end;

procedure TTMSFNCWriter.WriteGenericObjectList(AList: TTMSFNCObjectList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    WriteSingleObject(AList[I]);
  Writer.WriteEndArray;
end;

procedure TTMSFNCWriter.WriteGenericStringList(AList: TTMSFNCStringList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteString(AList[I]);
  Writer.WriteEndArray;
end;

procedure TTMSFNCWriter.Write(AObject: TObject);
begin
  WriteObject(AObject);
end;

procedure TTMSFNCWriter.WriteArray(AName: string; AArray: TTMSFNCObjectArray);
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

procedure TTMSFNCWriter.WriteBitmap(ABitmap: TTMSFNCBitmap);
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
    FWriter.WriteString(TTMSFNCUtils.SaveStreamToHexStr(ms));
  finally
    ms.Free;
  end;
  {$ENDIF}
  {$IFDEF WEBLIB}
  FWriter.WriteString(ABitmap.Data);
  {$ENDIF}
end;

procedure TTMSFNCWriter.WriteCollection(ACollection: TCollection);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to ACollection.Count - 1 do
    WriteSingleObject(ACollection.Items[I]);
  Writer.WriteEndArray;
end;

procedure TTMSFNCWriter.WriteList(AList: TList);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    WriteSingleObject(TObject(AList[I]));
  Writer.WriteEndArray;
end;

procedure TTMSFNCWriter.WriteObject(AObject: TObject);
var
  b: ITMSFNCPersistenceIO;
begin
  if AObject = nil then
    Writer.WriteNull
  else
  begin
    if TTMSFNCPersistence.IsGenericList(AObject.ClassType, 'String') then
      WriteGenericStringList(TTMSFNCStringList(AObject))
    else if TTMSFNCPersistence.IsGenericList(AObject.ClassType, 'Integer') then
      WriteGenericIntegerList(TTMSFNCIntegerList(AObject))
    else if TTMSFNCPersistence.IsGenericList(AObject.ClassType, 'Double') then
      WriteGenericDoubleList(TTMSFNCDoubleList(AObject))
    else if TTMSFNCPersistence.IsGenericList(AObject.ClassType) then
      WriteGenericObjectList(TTMSFNCObjectList(AObject))
    else if TTMSFNCPersistence.IsGenericDictionary(AObject.ClassType) then
      WriteGenericDictionary(TTMSFNCObjectDictionary(AObject))
    else if TTMSFNCPersistence.IsList(AObject.ClassType) then
      WriteList(TList(AObject))
    else if TTMSFNCPersistence.IsCollection(AObject.ClassType) then
      WriteCollection(TCollection(AObject))
    else if TTMSFNCPersistence.IsBitmap(AObject.ClassType) then
      WriteBitmap(TTMSFNCBitmap(AObject))
    else if TTMSFNCPersistence.IsDescendingClass(AObject.ClassType, ['TStrings']) then
      WriteStrings(TStrings(AObject))
    else
    begin
      if Assigned(IOReference) and Supports(IOReference, ITMSFNCPersistenceIO, b) then
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

procedure TTMSFNCWriter.WriteSingleObject(AObject: TObject);
begin
  Writer.WriteBeginObject;
  if TTMSFNCPersistence.ClassTypeVariable <> '' then
  begin
    Writer.WriteName(TTMSFNCPersistence.ClassTypeVariable);
    Writer.WriteString(AObject.ClassName);
  end;
  WriteProperties(AObject);
  Writer.WriteEndObject;
end;

procedure TTMSFNCWriter.WriteStrings(AList: TStrings);
var
  I: Integer;
begin
  Writer.WriteBeginArray;
  for I := 0 to AList.Count - 1 do
    Writer.WriteString(AList[I]);
  Writer.WriteEndArray;
end;

{$HINTS OFF}
procedure TTMSFNCWriter.WritePropInfoValue(AInstance: TObject; const APropInfo: TTMSFNCPropertyInfo);
var
  cn: string;
  pName: string;
  en: string;
  k: TTypeKind;
  p: TTMSFNCPropertyInfo;
  o: TObject;
  v: TMethod;
  c: TTMSFNCPersistenceColor;
begin
  if TTMSFNCPersistence.IsWriteOnly(APropInfo) then
  begin
    Writer.WriteNull;
    Exit;
  end;

  o := AInstance;
  p := APropInfo;
  k := TTMSFNCPersistence.GetPropInfoType(p);
  pName := TTMSFNCPersistence.GetPropInfoName(p);

  case k of
    tkInteger:
    begin
      cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') then
      begin
        if GetOrdProp(o, p) = gcNull then
          Writer.WriteString('gcNull')
        else
        begin
          c := TTMSFNCPersistenceColor(GetOrdProp(o, p));
          Writer.WriteString(ColorToHTMLEx(c))
        end;
      end
      else
        Writer.WriteInteger(GetOrdProp(o, p));
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}: Writer.WriteString(GetStrProp(o, p));
    tkEnumeration:
      if TTMSFNCPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
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
        if Assigned(TTMSFNCPersistence.RootObject) then
          Writer.WriteString(TTMSFNCPersistence.RootObject.MethodName(v.Code))
        else
          Writer.WriteNull;
      end;
    end
    else
    begin
      en := TTMSFNCPersistence.GetEnumName(TypeInfo(TTypeKind), Integer(k));
      Writer.WriteNull;
      //raise ETMSFNCReaderException.CreateFmt('Cannot write property %s with type %s', [pName, en]);
    end;
  end;

  if (o is TFont) and (pName = 'Size') then
  begin
    Writer.WriteName('IsFMX');
    Writer.WriteBoolean({$IFDEF FMXLIB}True{$ELSE}False{$ENDIF});
  end;
end;
{$HINTS ON}

procedure TTMSFNCWriter.WriteProperties(AObject: TObject);
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
    GetMem(pl, c * SizeOf(TTMSFNCPropertyInfo));
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

procedure TTMSFNCWriter.WriteProperty(AObject: TObject; AProp: TTMSFNCPropertyInfo);
var
  pName: string;
  k: TTypeKind;
  b, a, ap: Boolean;
  p: ITMSFNCPersistence;
  o: TObject;
begin
  if not Assigned(AProp) then
    Exit;

  pName := TTMSFNCPersistence.GetPropInfoName(AProp);
  k := TTMSFNCPersistence.GetPropInfoType(AProp);

  b := TTMSFNCUtils.IndexOfTextInArray(pName, TTMSFNCPersistence.ExcludeProperties) = -1;
  if Supports(AObject, ITMSFNCPersistence, p) then
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

        ap := TTMSFNCPersistence.IsAssignableProperty(AObject, AProp);

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

{ TTMSFNCReader }

constructor TTMSFNCReader.Create(AStream: TStreamEx);
begin
  FReader := TTMSFNCJSONReader.Create(AStream);
  FReferences := TTMSFNCObjectReferences.Create(true);
end;

destructor TTMSFNCReader.Destroy;
begin
  FReader.Free;
  FReferences.Free;
  inherited;
end;

function TTMSFNCReader.ReadSingleObject(ABaseClass: TClass): TObject;
var
  cn: string;
  b: ITMSFNCBasePersistenceIO;
  p: ITMSFNCPersistenceIO;
begin
  Reader.ReadBeginObject;
  if TTMSFNCPersistence.ClassTypeVariable <> '' then
  begin
    if not Reader.HasNext or (Reader.ReadName <> TTMSFNCPersistence.ClassTypeVariable) then
      raise ETMSFNCReaderException.Create('"'+TTMSFNCPersistence.ClassTypeVariable+'" property not found in Object descriptor.');
    cn := Reader.ReadString;
  end;

  if cn = '' then
    cn := ABaseClass.ClassName;

  if Assigned(FIOReference) then
  begin
    Result := nil;
    if Supports(FIOReference, ITMSFNCBasePersistenceIO, b) then
      Result := b.CreateObject(cn, ABaseClass)
    else if Supports(FIOReference, ITMSFNCPersistenceIO, p) then
      Result := p.CreateObject(cn, ABaseClass);
  end
  else
    Result := TTMSFNCPersistence.CreateObject(cn, ABaseClass);
    
  try
    ReadProperties(Result);
    Reader.ReadEndObject;
  except
    Result.Free;
    raise;
  end;
end;

procedure TTMSFNCReader.ReadExistingObject(AObject: TObject);
begin
  if Assigned(AObject) then
  begin
    Reader.ReadBeginObject;
    if TTMSFNCPersistence.ClassTypeVariable <> '' then
    begin
      if not Reader.HasNext or (Reader.ReadName <> TTMSFNCPersistence.ClassTypeVariable) then
        raise ETMSFNCReaderException.Create('"'+TTMSFNCPersistence.ClassTypeVariable+'" property not found in Object descriptor.');

      Reader.ReadString;
    end;

    ReadProperties(AObject);
    Reader.ReadEndObject;
  end
  else
    Reader.ReadNull;
end;

procedure TTMSFNCReader.ReadGenericDictionary(ADictionary: TTMSFNCObjectDictionary);
var
  obj: TObject;
  k: string;
  c: TClass;
  i: ITMSFNCBaseListIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(ADictionary, ITMSFNCBaseListIO, i) then
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

procedure TTMSFNCReader.ReadGenericStringList(AList: TTMSFNCStringList);
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

procedure TTMSFNCReader.ReadGenericIntegerList(AList: TTMSFNCIntegerList);
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

procedure TTMSFNCReader.ReadGenericDoubleList(AList: TTMSFNCDoubleList);
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

procedure TTMSFNCReader.ReadGenericObjectList(AList: TTMSFNCObjectList);
var
  obj: TObject;
  b: ITMSFNCPersistenceIO;
  c: TClass;
  i: ITMSFNCBaseListIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(AList, ITMSFNCBaseListIO, i) then
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

          if Assigned(IOReference) and Supports(IOReference, ITMSFNCPersistenceIO, b) then
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

function TTMSFNCReader.Read(AClass: TClass): TObject;
begin
  Result := ReadSingleObject(AClass);
end;

procedure TTMSFNCReader.Read(AObject: TObject);
begin
  ReadObject(AObject);
end;

procedure TTMSFNCReader.ReadBitmap(ABitmap: TTMSFNCBitmap);
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
      TTMSFNCUtils.LoadStreamFromHexStr(s, ms);
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

procedure TTMSFNCReader.ReadCollection(ACollection: TCollection);
var
  obj: TObject;
  c: TClass;
  i: ITMSFNCBaseListIO;
  ii: ITMSFNCBaseCollectionIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(ACollection, ITMSFNCBaseListIO, i) then
    c := i.GetItemClass;

  Supports(ACollection, ITMSFNCBaseCollectionIO, ii);

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

function TTMSFNCReader.ReadArray(AName: string): TTMSFNCObjectArray;
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

procedure TTMSFNCReader.ReadList(AList: TList);
var
  obj: TObject;
  b: ITMSFNCPersistenceIO;
  c: TClass;
  i: ITMSFNCBaseListIO;
  cl: Boolean;
begin
  c := TObject;
  if Supports(AList, ITMSFNCBaseListIO, i) then
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

          if Assigned(IOReference) and Supports(IOReference, ITMSFNCPersistenceIO, b) then
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

procedure TTMSFNCReader.ReadObject(AObject: TObject);
begin
  if AObject = nil then
    Reader.ReadNull
  else
  begin
    if TTMSFNCPersistence.IsGenericList(AObject.ClassType, 'String') then
      ReadGenericStringList(TTMSFNCStringList(AObject))
    else if TTMSFNCPersistence.IsGenericList(AObject.ClassType, 'Double') then
      ReadGenericDoubleList(TTMSFNCDoubleList(AObject))
    else if TTMSFNCPersistence.IsGenericList(AObject.ClassType, 'Integer') then
      ReadGenericIntegerList(TTMSFNCIntegerList(AObject))
    else if TTMSFNCPersistence.IsGenericList(AObject.ClassType) then
      ReadGenericObjectList(TTMSFNCObjectList(AObject))
    else if TTMSFNCPersistence.IsGenericDictionary(AObject.ClassType) then
      ReadGenericDictionary(TTMSFNCObjectDictionary(AObject))
    else if TTMSFNCPersistence.IsList(AObject.ClassType) then
      ReadList(TList(AObject))
    else if TTMSFNCPersistence.IsCollection(AObject.ClassType) then
      ReadCollection(TCollection(AObject))
    else if TTMSFNCPersistence.IsBitmap(AObject.ClassType) then
      ReadBitmap(TTMSFNCBitmap(AObject))
    else if TTMSFNCPersistence.IsDescendingClass(AObject.ClassType, ['TStrings']) then
      ReadStrings(TStrings(AObject))
    else
      ReadSingleObject(AObject);
  end;
end;

procedure TTMSFNCReader.ReadProperties(AObject: TObject);
var
  Prop: TTMSFNCPropertyInfo;
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

procedure TTMSFNCReader.ReadProperty(AObject: TObject; AProp: TTMSFNCPropertyInfo);
var
  pName: string;
  ct: TClass;
  b: Boolean;
  p: ITMSFNCPersistence;
  pio: ITMSFNCPersistenceIO;
  k: TTypeKind;
  a, ap: Boolean;
  o: TObject;
  n: string;
begin
  if not Assigned(AProp) then
    Exit;

  k := TTMSFNCPersistence.GetPropInfoType(AProp);
  pName := TTMSFNCPersistence.GetPropInfoName(AProp);

  b := TTMSFNCUtils.IndexOfTextInArray(pName, TTMSFNCPersistence.ExcludeProperties) = -1;
  if Supports(AObject, ITMSFNCPersistence, p) then
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
        ct := TTMSFNCPersistence.GetPropInfoDataTypeInfoClassType(AProp);
        if TTMSFNCPersistence.IsGenericList(ct, 'String') then
          ReadGenericStringList(TTMSFNCStringList(GetObjectProp(AObject, pName)))
        else if TTMSFNCPersistence.IsGenericList(ct) then
          ReadGenericObjectList(TTMSFNCObjectList(GetObjectProp(AObject, pName)))
        else if TTMSFNCPersistence.IsGenericDictionary(ct) then
          ReadGenericDictionary(TTMSFNCObjectDictionary(GetObjectProp(AObject, pName)))
        else if TTMSFNCPersistence.IsList(ct) then
          ReadList(TList(GetObjectProp(AObject, pName)))
        else if TTMSFNCPersistence.IsCollection(ct) then
          ReadCollection(TCollection(GetObjectProp(AObject, pName)))
        else if TTMSFNCPersistence.IsBitmap(ct) then
          ReadBitmap(TTMSFNCBitmap(GetObjectProp(AObject, pName)))
        else if TTMSFNCPersistence.IsDescendingClass(ct, ['TStrings']) then
          ReadStrings(TStrings(GetObjectProp(AObject, pName)))
        else
        begin
          a := False;
          if Assigned(IOReference) and Supports(IOReference, ITMSFNCPersistenceIO, pio) then
            a := pio.NeedsObjectReference(ct);

          if a then
          begin
            if Reader.IsNull then
            begin
              Reader.ReadNull;
              SetObjectProp(AObject, pName, nil);
            end
            else
              FReferences.Add(TTMSFNCObjectReference.Create(AObject, AProp, Reader.ReadString));
          end
          else
          begin
            o := GetObjectProp(AObject, pName);

            ap := TTMSFNCPersistence.IsAssignableProperty(AObject, AProp);
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
procedure TTMSFNCReader.ReadPropInfoValue(AInstance: TObject; const APropInfo: TTMSFNCPropertyInfo);
var
  pName, cn, cnv, en: string;
  k: TTypeKind;
  p: TTMSFNCPropertyInfo;
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
  if TTMSFNCPersistence.IsWriteOnly(APropInfo) or Reader.IsNull then
  begin
    Reader.ReadNull;        
    Exit;
  end;
 
  o := AInstance;
  p := APropInfo;
  pName := TTMSFNCPersistence.GetPropInfoName(p);
  k := TTMSFNCPersistence.GetPropInfoType(p);

  case k of
    tkInteger:
    begin
      cn := TTMSFNCPersistence.GetPropInfoTypeName(p);
      if (cn = 'TAlphaColor') or (cn = 'TColor') or (cn = 'TGraphicsColor') then
      begin
        cnv := Reader.ReadString;
        if not TTMSFNCPersistence.IsReadOnly(p) then
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
        if not TTMSFNCPersistence.IsReadOnly(p) then
          SetOrdProp(o, p, i);
      end;
    end;
    {$IFNDEF WEBLIB}tkWChar, tkLString, tkUString,{$ENDIF}tkChar, tkString{$IFDEF LCLLIB},tkAString{$ENDIF}: 
    begin
      s := Reader.ReadString;
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetStrProp(o, p, s);
    end;
    tkEnumeration:
      if TTMSFNCPersistence.GetPropInfoDataTypeInfo(p) = TypeInfo(Boolean) then
      begin
        b := Reader.ReadBoolean;
        if not TTMSFNCPersistence.IsReadOnly(p) then        
          SetOrdProp(o, p, Integer(b))
      end
      else
      begin
        i := Reader.ReadInteger;
        if not TTMSFNCPersistence.IsReadOnly(p) then        
          SetOrdProp(o, p, i);
      end;
    {$IFDEF LCLWEBLIB}
    tkBool:
    begin
      b := Reader.ReadBoolean;
      if not TTMSFNCPersistence.IsReadOnly(p) then
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
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetFloatProp(o, p, d)
    end;
    {$IFNDEF WEBLIB}
    tkInt64:
    begin
      ii := Reader.ReadInt64;
      if not TTMSFNCPersistence.IsReadOnly(p) then
        SetOrdProp(o, p, ii)
    end;
    {$ENDIF}
    tkSet:
    begin
      i := Reader.ReadInteger;
      if not TTMSFNCPersistence.IsReadOnly(p) then
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
        if Assigned(TTMSFNCPersistence.RootObject) then
        begin
          v := Reader.ReadString;
          m.Code := TTMSFNCPersistence.RootObject.MethodAddress(v);
          if m.Code <> nil then
            m.Data := TTMSFNCPersistence.RootObject;
        end
        else
          Reader.ReadNull;
      end;

      SetMethodProp(o, p, m);
    end
    else
    begin
      en := TTMSFNCPersistence.GetEnumName(TypeInfo(TTypeKind), Integer(k));
      Reader.ReadNull;
      //raise ETMSFNCReaderException.CreateFmt('Cannot read property %s with type %s', [pName, en]);
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

procedure TTMSFNCReader.ReadSingleObject(AObject: TObject);
begin
  Reader.ReadBeginObject;
  if TTMSFNCPersistence.ClassTypeVariable <> '' then
  begin
    if not Reader.HasNext or (Reader.ReadName <> TTMSFNCPersistence.ClassTypeVariable) then
      raise ETMSFNCReaderException.Create('"'+TTMSFNCPersistence.ClassTypeVariable+'" property not found in Object descriptor.');
    Reader.ReadString;
  end;

  try
    ReadProperties(AObject);
    Reader.ReadEndObject;
  except
    raise;
  end;
end;

procedure TTMSFNCReader.ReadStrings(AList: TStrings);
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

procedure TTMSFNCReader.SetExcludeProperties(
  const Value: TTMSFNCExcludePropertyListArray);
begin
  FExcludeProperties := Value;
  TTMSFNCPersistence.ExcludeProperties := FExcludeProperties;
end;

procedure TTMSFNCReader.SetIOReference(const Value: TObject);
begin
  FIOReference := Value;
  TTMSFNCPersistence.IOReference := FIOReference;
end;

procedure TTMSFNCReader.SetRootObject(const Value: TObject);
begin
  FRootObject := Value;
  TTMSFNCPersistence.RootObject := FRootObject;
end;

procedure TTMSFNCReader.SolveReferences;
var
  b: ITMSFNCPersistenceIO;
  r: Integer;
  rf: TTMSFNCObjectReference;
  o: TObject;
begin
  if Assigned(IOReference) and Supports(IOReference, ITMSFNCPersistenceIO, b) then
  begin
    for r := 0 to FReferences.Count - 1 do
    begin
      rf := FReferences[r];
      o := b.FindObject(rf.Id);
      SetObjectProp(rf.Instance, rf.Prop, o);
    end;
  end;
end;

{ TTMSFNCObjectReference }

constructor TTMSFNCObjectReference.Create(AInstance: TObject;
  AProp: TTMSFNCPropertyInfo; const AId: string);
begin
  Instance := AInstance;
  Prop := AProp;
  Id := AId;
end;

{ TTMSFNCPersistence }

class procedure TTMSFNCPersistence.LoadSettingsFromFile(AObject: TObject; AFileName: string);
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

class procedure TTMSFNCPersistence.LoadSettingsFromStream(AObject: TObject; AStream: TStreamEx);
var
  Reader: TTMSFNCReader;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
begin
  AStream.Position := 0;
  Reader := TTMSFNCReader.Create(AStream);
  t := FormatSettings.ThousandSeparator;
  d := FormatSettings.DecimalSeparator;
  try
    Reader.IOReference := TTMSFNCPersistence.IOReference;
    Reader.RootObject := TTMSFNCPersistence.RootObject;
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

class procedure TTMSFNCPersistence.SaveSettingsToFile(AObject: TObject;
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

class procedure TTMSFNCPersistence.SaveSettingsToStream(AObject: TObject;
  AStream: TStreamEx);
var
  Writer: TTMSFNCWriter;
  {$IFDEF WEBLIB}
  d, t: string;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  d, t: Char;
  {$ENDIF}
begin
  Writer := TTMSFNCWriter.Create(AStream);
  t := FormatSettings.ThousandSeparator;
  d := FormatSettings.DecimalSeparator;
  try
    Writer.IOReference := TTMSFNCPersistence.IOReference;
    Writer.RootObject := TTMSFNCPersistence.RootObject;
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
class procedure TTMSFNCPersistence.GetEnumValues(AValues: TStrings; APropInfo: TTMSFNCPropertyInfo);
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
              AValues.Add(TTMSFNCPersistence.GetEnumName(ct{$IFNDEF LCLLIB}^{$ENDIF},i));
          end;
        end;
      end;
    end
    else
    begin
      for i := su^.MinValue to su^.MaxValue do
        AValues.Add(TTMSFNCPersistence.GetEnumName(p,i));
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
     AValues.Add(TTMSFNCPersistence.GetEnumName(p, i));
  end;
  {$ENDIF}
end;

class function TTMSFNCPersistence.GetPropInfoDataTypeInfoClassType(APropInfo: TTMSFNCPropertyInfo): TClass;
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

class function TTMSFNCPersistence.GetPropInfoDataTypeInfo(
  APropInfo: TTMSFNCPropertyInfo): PTypeInfo;
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

class function TTMSFNCPersistence.GetPropInfoName(APropInfo: TTMSFNCPropertyInfo): string;
begin
  {$IFNDEF WEBLIB}
  Result := GetShortStringString(@APropInfo{$IFDEF LCLLIB}^{$ENDIF}.Name);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.name;
  {$ENDIF}
end;

class function TTMSFNCPersistence.GetPropInfoType(APropInfo: TTMSFNCPropertyInfo): TTypeKind;
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

class procedure TTMSFNCPersistence.DoCustomReadProperty(AObject: TObject;
  APropertyName: string; APropertyKind: TTypeKind; AReader: TTMSFNCJSONReader;
  var ACanRead: Boolean);
begin
  if Assigned(OnCustomReadProperty) then
    OnCustomReadProperty(AObject, APropertyName, APropertyKind, AReader, ACanRead);
end;

class procedure TTMSFNCPersistence.DoCustomWriteProperty(AObject: TObject;
  APropertyName: string; APropertyKind: TTypeKind; AWriter: TTMSFNCJSONWriter;
  var ACanWrite: Boolean);
begin
  if Assigned(OnCustomWriteProperty) then
    OnCustomWriteProperty(AObject, APropertyName, APropertyKind, AWriter, ACanWrite);
end;

class function TTMSFNCPersistence.GetEnumName(ATypeInfo: PTypeInfo; AValue: Integer): string;
begin
  {$IFNDEF WEBLIB}
  Result := TypInfo.GetEnumName(ATypeInfo, AValue);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := TTypeInfoEnum(ATypeInfo).EnumType.IntToName[AValue];
  {$ENDIF}
end;

class function TTMSFNCPersistence.GetPropInfoTypeName(APropInfo: TTMSFNCPropertyInfo): string;
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

class function TTMSFNCPersistence.IsList(AClass: TClass): boolean;
begin
  Result := IsDescendingClass(AClass, ['TList']);
end;

class function TTMSFNCPersistence.IsAssignableProperty(AObject: TObject;
  APropInfo: TTMSFNCPropertyInfo): Boolean;
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

class function TTMSFNCPersistence.IsBitmap(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TBitmap', 'TPicture', 'TTMSFNCBitmap'{$IFDEF WEBLIB}, 'TGraphic'{$ENDIF}]);
end;

class function TTMSFNCPersistence.IsReadOnly(
  APropInfo: TTMSFNCPropertyInfo): Boolean;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.SetProc = nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.setter = '';
  {$ENDIF}
end;

class function TTMSFNCPersistence.IsStrings(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TStrings']);
end;

class function TTMSFNCPersistence.IsStrokeKind(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TTMSFNCGraphicsStrokeKind');
end;

class function TTMSFNCPersistence.IsTime(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TTime');
end;

class function TTMSFNCPersistence.IsWriteOnly(APropInfo: TTMSFNCPropertyInfo): Boolean;
begin
  {$IFNDEF WEBLIB}
  Result := APropInfo^.GetProc = nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := APropInfo.getter = '';
  {$ENDIF}
end;

class function TTMSFNCPersistence.IsCollection(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TCollection']);
end;

class function TTMSFNCPersistence.IsColor(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TAlphaColor') or (APropertyName = 'TColor') or (APropertyName = 'TGraphicsColor');
end;

class function TTMSFNCPersistence.IsComponent(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TComponent', 'TTMSFNCCustomComponent']);
end;

class function TTMSFNCPersistence.IsControl(AClass: TClass): Boolean;
begin
  Result := IsDescendingClass(AClass, ['TControl']);
end;

class function TTMSFNCPersistence.IsDate(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TDate');
end;

class function TTMSFNCPersistence.IsDateTime(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TDateTime');
end;

class function TTMSFNCPersistence.IsDescendingClass(AClass: TClass;
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

class function TTMSFNCPersistence.IsFillKind(APropertyName: string): Boolean;
begin
  Result := (APropertyName = 'TTMSFNCGraphicsFillKind');
end;

class function TTMSFNCPersistence.IsGenericDictionary(AClass: TClass): Boolean;
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

class function TTMSFNCPersistence.IsGenericList(AClass: TClass; AType: string = ''): Boolean;
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

class function TTMSFNCPersistence.CreateObject(const AClassName: string;
  BaseClass: TClass): TObject;
var
  ObjType: TPersistentClass;
begin
  ObjType := GetClass(AClassName);
  if ObjType = nil then
    raise ETMSFNCReaderException.CreateFmt('Type "%s" not found', [AClassName]);
  if not ObjType.InheritsFrom(TObject) then
    raise ETMSFNCReaderException.Create('Type "%s" is not an class type');
  if BaseClass <> nil then
    if not ObjType.InheritsFrom(BaseClass) then
      raise ETMSFNCReaderException.CreateFmt('Type "%s" does not inherit from %s',
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
    raise ETMSFNCReaderException.CreateFmt('Type "%s" not supported', [AClassName]);
end;

{ TTMSFNCObjectPersistence }

class procedure TTMSFNCObjectPersistence.LoadObjectFromString(AObject: TObject; AString: string);
var
  ms: TStringStream;
begin
  ms := TStringStream.Create(AString{$IFDEF WEBLIB}, TEncoding.UTF8{$ENDIF});
  try
    TTMSFNCPersistence.LoadSettingsFromStream(AObject, ms);
  finally
    ms.Free;
  end;
end;

class function TTMSFNCObjectPersistence.SaveObjectToString(
  AObject: TObject): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(''{$IFDEF WEBLIB}, TEncoding.UTF8{$ENDIF});
  try
    TTMSFNCPersistence.SaveSettingsToStream(AObject, ss);
    ss.Position := 0;
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

{$IFDEF FNCLIB}

{ TTMSFNCJSONToClass }

class procedure TTMSFNCJSONToClass.JSONValueToClass(AParentProperty: TTMSFNCJSONToClassProperty; AJSONValue: TJSONValue);
var
  v: TJSONValue;
  n: string;
  p: TTMSFNCJSONToClassProperty;
  sz: Integer;
  K: Integer;
  t: TTMSFNCJSONToClassPropertyType;
  ci: TTMSFNCJSONToClassItem;
begin
  if AJSONValue is TJSONObject then
  begin
    ci := TTMSFNCJSONToClassItem.Create(AParentProperty);
    FClasses.Add(ci);

    sz := TTMSFNCUtils.GetJSONObjectSize(AJSONValue as TJSONObject);
    for k := 0 to sz - 1 do
    begin
      n := TTMSFNCUtils.GetJSONObjectName(AJSONValue as TJSONObject, k);
      v := TTMSFNCUtils.GetJSONValue(AJSONValue, n);

      p := nil;
      if Assigned(v) then
      begin
        t := JSONValueToPropertyType(v);
        if t <> cptUndefined then
        begin
          p := TTMSFNCJSONToClassProperty.Create(n, t);
          ci.Properties.Add(p);
        end;
      end;

      JSONValueToClass(p, v);
    end;
  end
  else if AJSONValue is TJSONArray then
  begin
    if TTMSFNCUtils.GetJSONArraySize(AJSONValue as TJSONArray) > 0 then
    begin
      t := JSONValueToPropertyType(AJSONValue);
      v := TTMSFNCUtils.GetJSONArrayItem(AJSONValue as TJSONArray, 0);
      if not Assigned(AParentProperty) and (t <> cptUndefined) then
      begin
        ci := TTMSFNCJSONToClassItem.Create(nil);
        p := TTMSFNCJSONToClassProperty.Create('Items', t);
        ci.Properties.Add(p);
        FClasses.Add(ci);
        AParentProperty := p;
      end;

      JSONValueToClass(AParentProperty, v);
    end
    else
    begin
      ci := TTMSFNCJSONToClassItem.Create(AParentProperty);
      FClasses.Add(ci);
    end;
  end
  else
  begin

  end;
end;

class function TTMSFNCJSONToClass.JSONValueToPropertyType(
  AJSONValue: TJSONValue): TTMSFNCJSONToClassPropertyType;
var
  s: string;
  dt: TDateTime;
  b: Boolean;
  v: TJSONValue;
  p: TTMSFNCJSONToClassPropertyType;
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
    if TTMSFNCUtils.GetJSONArraySize(AJSONValue as TJSONArray) > 0 then
    begin
      v := TTMSFNCUtils.GetJSONArrayItem(AJSONValue as TJSONArray, 0);
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
    else if not TTMSFNCUtils.TryStrToFloatDot(s, d) then
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

    if TTMSFNCUtils.TryStrToFloatDot(s, d) then
      Result := cptString
    else if TTMSFNCUtils.IsDate(s, dt) or {$IFDEF FNCLIB}(TTMSFNCUtils.ISOToDateTime(s, True) > 0) or{$ENDIF} TryStrToDateTime(s, dt) then
      Result := cptDateTime
    else if TryStrToBool(s, b) then
      Result := cptBoolean
    else
      Result:= cptString;
  end;
end;

class function TTMSFNCJSONToClass.ExportToDelphiFromFile(AFileName: string; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphiFromFile(AFileName, DefaultJSONToClassOptions, ACallBack);
end;

class function TTMSFNCJSONToClass.ExportToDelphi(AJSONString: string; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphi(AJSONString, DefaultJSONToClassOptions, ACallBack);
end;

class function TTMSFNCJSONToClass.ExportToDelphi(AJSONValue: TJSONValue; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphi(AJSONValue.ToString, DefaultJSONToClassOptions, ACallBack);
end;

class function TTMSFNCJSONToClass.ExportToDelphiFromFile(AFileName: string; AOptions: TTMSFNCJSONToClassOptions; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string;
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

function CompareNameVal(const Item1, Item2: TTMSFNCJSONToClassProperty): Integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
end;

class function TTMSFNCJSONToClass.ExportToDelphi(AJSONValue: TJSONValue; AOptions: TTMSFNCJSONToClassOptions; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string;
begin
  Result := ExportToDelphi(AJSONValue.ToString, AOptions, ACallBack);
end;

class function TTMSFNCJSONToClass.ExportToDelphi(AJSONString: string; AOptions: TTMSFNCJSONToClassOptions; ACallBack: TTMSFNCJSONToClassExportEvent = nil): string;
var
  root: TJSONValue;
  I, K: Integer;
  c: TTMSFNCJSONToClassItem;
  sl: TStringList;
  p: TTMSFNCJSONToClassProperty;
  ind: string;
  cn: string;
  slRendered: TStringList;

  procedure SortList(const AList: TTMSFNCJSONToClassProperties);
  {$IFNDEF LCLWEBLIB}
  var
    cmp: TTMSFNCJSONToClassPropertyNameComparer;
  {$ENDIF}
  begin
    {$IFNDEF LCLWEBLIB}
    cmp := TTMSFNCJSONToClassPropertyNameComparer.Create;
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
    AList.Sort(TComparer<TTMSFNCJSONToClassProperty>.Construct(
      function(const ALeft, ARight: TTMSFNCJSONToClassProperty): Integer
      begin
        Result := CompareText(ALeft.Name, ARight.Name);
      end));
    {$ENDIF}
  end;
begin
  root := TTMSFNCUtils.ParseJSON(AJSONString);
  if Assigned(root) then
  begin
    try
      FClasses := TTMSFNCJSONToClassItems.Create;
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

{ TTMSFNCJSONToClassItem }

constructor TTMSFNCJSONToClassItem.Create(
  AParentProperty: TTMSFNCJSONToClassProperty);
begin
  FParentProperty := AParentProperty;
end;

destructor TTMSFNCJSONToClassItem.Destroy;
begin
  if Assigned(FProperties) then
    FProperties.Free;
  inherited;
end;

function TTMSFNCJSONToClassItem.GetClassName(ACallBack: TTMSFNCJSONToClassExportEvent; AOptions: TTMSFNCJSONToClassOptions): string;
begin
  Result := 'TMyDelphiClass';
  if Assigned(ParentProperty) then
    Result := 'T' + ParentProperty.DelphiName(ACallBack, AOptions, False);
end;

function TTMSFNCJSONToClassItem.GetProperties: TTMSFNCJSONToClassProperties;
begin
  if not Assigned(FProperties) then
    FProperties := TTMSFNCJSONToClassProperties.Create;

  Result := FProperties;
end;

{ TTMSFNCJSONToClassProperty }

constructor TTMSFNCJSONToClassProperty.Create(AName: string;
  AType: TTMSFNCJSONToClassPropertyType);
begin
  FName := AName;
  FType := AType;
end;

function TTMSFNCJSONToClassProperty.DelphiName(ACallBack: TTMSFNCJSONToClassExportEvent; AOptions: TTMSFNCJSONToClassOptions; AFixKeyWord: Boolean = True): string;
var
  n, s: string;
  c: Char;
  sl: TStringList;
  I: Integer;
  p: string;
  pt: TTMSFNCJSONToClassPropertyType;
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
          if TTMSFNCUtils.IsLetterOrDigit(c) then
            p := p + c
          else
            p := p + '_'
        end;
      end;

      TTMSFNCUtils.Split('_', p, sl);

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

function TTMSFNCJSONToClassProperty.DelphiType(ACallBack: TTMSFNCJSONToClassExportEvent; AOptions: TTMSFNCJSONToClassOptions): string;
var
  n: string;
  t: TTMSFNCJSONToClassPropertyType;
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

function TTMSFNCJSONToClassProperty.FixKeyWord(AValue: string): string;
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
  if TTMSFNCUtils.IndexOfTextInArray(UpperCase(AValue), a) <> -1 then
    Result := '&' + AValue;
end;

function TTMSFNCJSONToClassProperty.IsObject: Boolean;
begin
  Result := &Type in [cptObject, cptObjectArray, cptStringArray, cptBooleanArray, cptDateTimeArray, cptDoubleArray, cptIntegerArray, cptInteger64Array];
end;

{ TTMSFNCJSONToClassPropertyNameComparer }

{$IFNDEF LCLWEBLIB}
function TTMSFNCJSONToClassPropertyNameComparer.Compare(const Left,
  Right: TTMSFNCJSONToClassProperty): Integer;
begin
  Result := CompareText(Left.Name, Right.Name);
end;
{$ENDIF}
{$ENDIF}


initialization
begin
  TTMSFNCPersistence.ClassTypeVariable := '$type';
end;

end.


