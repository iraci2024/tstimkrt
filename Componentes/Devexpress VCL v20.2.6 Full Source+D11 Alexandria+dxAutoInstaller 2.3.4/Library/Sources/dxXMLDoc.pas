{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressCore Library }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL }
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

unit dxXMLDoc;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Classes, SysUtils, Windows, StrUtils, AnsiStrings, dxCore, dxCoreClasses,
  dxCustomTree, Generics.Defaults, Generics.Collections,
  dxHash, dxHashUtils;

type
  TdxXMLNode = class;
  TdxXMLNodeAttribute = class;
  TdxXMLNodeAttributes = class;
  TdxXMLDocument = class;

  TdxXMLString = type AnsiString;

  { EdxXMLUnexpectedToken }

  EdxXMLUnexpectedToken = class(EdxException)
  public
    constructor Create(const AToken, AStringForParsing: string);
  end;

  TdxXMLEncoding = (dxxeNone, dxxeUTF8, dxxeWindows);
  TdxXMLTokenID = (ttUnknown, ttEqual, ttTagHeaderBegin, ttTagHeaderEnd,
    ttTagEnd, ttTagFooter, ttComment);

  TdxXMLToken = packed record
    Buffer: PAnsiChar;
    BufferLengthInChars: Integer;
    TokenType: TdxXMLTokenID;
  end;

  { TdxXMLDateTime }

  TdxXMLDateTime = record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Millisecond: Word;
    IsUTC: Boolean;

    class function Create(const ASource: TDateTime; AIsUTC: Boolean = False)
      : TdxXMLDateTime; static;
    procedure Assign(const ASource: TDateTime; AIsUTC: Boolean = False);
    procedure Clear;
    procedure Parse(const S: string);
    function ToDateTime: TDateTime;
    function ToString: string;
  end;

  { TdxXMLSharedStrings }

  TdxXMLSharedStrings = class
  strict private
  type

    TAnsiStringValue = class
    public
      Hash: Cardinal;
      Next: TAnsiStringValue;
      Value: AnsiString;

      constructor Create(P: PAnsiChar; L: Integer; AHash: Cardinal;
        AValue: PAnsiString = nil);
    end;

    TStringValue = class
    public
      Hash: Cardinal;
      Next: TStringValue;
      Value: string;

      constructor Create(P: PChar; L: Integer; AHash: Cardinal;
        AValue: PString = nil);
    end;

    TAnsiStringValueArray = array of TAnsiStringValue;
    TStringValueArray = array of TStringValue;

  strict private
    FTableA: TAnsiStringValueArray;
    FTableS: TStringValueArray;
    FTableSize: Cardinal;

    function DoAdd(P: PAnsiChar; L: Integer; AValue: PAnsiString = nil)
      : AnsiString; overload;
    function DoAdd(P: PChar; L: Integer; AValue: PString = nil)
      : string; overload;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AValue: AnsiString): AnsiString; overload;
    function Add(const AValue: string): string; overload;
    function Add(const AValue: TdxXMLToken): AnsiString; overload;
    procedure Clear;
  end;

  { TdxXMLParser }

  TdxXMLParser = class
  public const
    NameScopeDelimiter = TdxXMLString(':');
  protected
    FDocument: TdxXMLDocument;
    FData: PAnsiChar;
    FDataLength: Integer;
    FEncoding: TdxXMLEncoding;
    FEncodingCodePage: Integer;
    FSharedStrings: TdxXMLSharedStrings;

    function DecodeValue(const S: AnsiString): string; virtual;
    function NextToken(out AToken: TdxXMLToken): Boolean; overload;
    function NextToken(var P: PAnsiChar; var C: Integer;
      out AToken: TdxXMLToken): Boolean; overload;
    procedure ParseDocumentHeader; virtual;
    procedure ParseEncoding; virtual;
    function ParseNodeHeader(ANode: TdxXMLNode): TdxXMLNode; virtual;
    procedure ParseNodeValue(ANode: TdxXMLNode;
      ATagHeaderEndCursor, ACursor: PAnsiChar); virtual;
    function Share(const AToken: TdxXMLToken): AnsiString; overload; inline;
    function Share(const AValue: string): string; overload; inline;
    procedure SkipTag;
    function TokenToString(const AToken: TdxXMLToken): AnsiString;
  public
    constructor Create(ADocument: TdxXMLDocument;
      AShareStrings: Boolean = True);
    destructor Destroy; override;
    procedure Parse(AScan: PAnsiChar; ACount: Integer);
    //
    property Document: TdxXMLDocument read FDocument;
  end;

  { TdxXMLHelper }

  TdxXMLHelper = class
  protected
    class function GetServiceCharacter(ACharCount: Integer; P: PWord;
      out L: Integer; out C: Char): Boolean;
    class function IsBoolean(const S: string): Boolean;
    class function IsEncodedCharacter(const S: string;
      APosition, ALength: Integer; out ACode: Integer): Boolean;
  public
    class function ExtractNameScope(const S: TdxXMLString): TdxXMLString;
    class function ExtractNameWithoutNameScope(const S: TdxXMLString)
      : TdxXMLString;
    class function IsPreserveSpacesNeeded(const S: string): Boolean;

    class function DecodeBoolean(const S: string): Boolean;
    class function DecodeString(const S: string): string;
    class function EncodeBoolean(const Value: Boolean): string;
    class function EncodeString(const S: string;
      ARemoveBreakLines: Boolean): string;
  end;

  { TdxXMLNodeAttribute }

  TdxXMLNodeAttribute = class(TcxDoublyLinkedObject)
  strict private
    FName: TdxXMLString;
    FValue: string;

    function GetValue: TdxXMLString; inline;
    function GetValueAsBoolean: Boolean; inline;
    function GetValueAsDateTime: TDateTime;
    function GetValueAsFloat: Double;
    function GetValueAsInt64: Int64;
    function GetValueAsInteger: Integer;
    procedure SetValue(const AValue: TdxXMLString); inline;
    procedure SetValueAsBoolean(AValue: Boolean); inline;
    procedure SetValueAsDateTime(const Value: TDateTime);
    procedure SetValueAsFloat(const AValue: Double);
    procedure SetValueAsInt64(const Value: Int64);
    procedure SetValueAsInteger(AValue: Integer);
  protected
    procedure Assign(ASource: TdxXMLNodeAttribute);
    procedure WriteData(AStream: TStream);
  public
    property Name: TdxXMLString read FName write FName;
    property Value: TdxXMLString read GetValue write SetValue;
    property ValueAsBoolean: Boolean read GetValueAsBoolean
      write SetValueAsBoolean;
    property ValueAsDateTime: TDateTime read GetValueAsDateTime
      write SetValueAsDateTime;
    property ValueAsFloat: Double read GetValueAsFloat write SetValueAsFloat;
    property ValueAsInt64: Int64 read GetValueAsInt64 write SetValueAsInt64;
    property ValueAsInteger: Integer read GetValueAsInteger
      write SetValueAsInteger;
    property ValueAsString: string read FValue write FValue;
  end;

  { TdxXMLNodeAttributes }

  TdxXMLNodeAttributes = class(TcxDoublyLinkedObjectList)
  strict private
    FNode: TdxXMLNode;

    function GetFirst: TdxXMLNodeAttribute;
    function GetLast: TdxXMLNodeAttribute;
  protected
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
    function GetAttr(const AAttrName: TdxXMLString): TdxXMLNodeAttribute;
    procedure WriteData(AStream: TStream);
  public
    constructor Create(ANode: TdxXMLNode);
    function Add(const AttrName: TdxXMLString): TdxXMLNodeAttribute;
      reintroduce; overload;
    function Add(const AttrName: TdxXMLString; AValue: Boolean)
      : TdxXMLNodeAttribute; reintroduce; overload;
    function Add(const AttrName: TdxXMLString; AValue: Integer)
      : TdxXMLNodeAttribute; reintroduce; overload;
    function Add(const AttrName: TdxXMLString; const AValue: Double)
      : TdxXMLNodeAttribute; reintroduce; overload;
    function Add(const AttrName: TdxXMLString; const AValue: Int64)
      : TdxXMLNodeAttribute; reintroduce; overload;
    function Add(const AttrName: TdxXMLString; const AValue: string)
      : TdxXMLNodeAttribute; reintroduce; overload;
    function Add(const AttrName: TdxXMLString; const AValue: TdxXMLString)
      : TdxXMLNodeAttribute; reintroduce; overload;
    procedure Assign(const ASource: TdxXMLNodeAttributes);
    procedure Delete(const AAttrName: TdxXMLString); reintroduce;
    function Exists(const AAttrName: TdxXMLString): Boolean;
    function Find(const AAttrName: TdxXMLString;
      out AAttr: TdxXMLNodeAttribute): Boolean;

    function GetValue(const AAttrName: TdxXMLString;
      const ADefaultValue: TdxXMLString = ''): TdxXMLString;
    function GetValueAsBoolean(const AAttrName: TdxXMLString;
      ADefaultValue: Boolean = False): Boolean;
    function GetValueAsDateTime(const AAttrName: TdxXMLString;
      const ADefaultValue: TDateTime = 0): TDateTime; overload;
    function GetValueAsDateTime(const AAttrName: TdxXMLString;
      out ADateTime: TdxXMLDateTime): Boolean; overload;
    function GetValueAsDefaultBoolean(const AAttrName: TdxXMLString)
      : TdxDefaultBoolean;
    function GetValueAsFloat(const AAttrName: TdxXMLString;
      const ADefaultValue: Double = 0): Double;
    function GetValueAsInt64(const AAttrName: TdxXMLString;
      const ADefaultValue: Int64 = 0): Int64;
    function GetValueAsInteger(const AAttrName: TdxXMLString;
      ADefaultValue: Integer = 0): Integer;
    function GetValueAsString(const AAttrName: TdxXMLString;
      const ADefaultValue: string = ''): string; inline;
    procedure SetValue(const AAttrName: TdxXMLString;
      const AValue: TdxXMLString);
    procedure SetValueAsBoolean(const AAttrName: TdxXMLString; AValue: Boolean);
    procedure SetValueAsDefaultBoolean(const AAttrName: TdxXMLString;
      AValue: TdxDefaultBoolean);
    procedure SetValueAsFloat(const AAttrName: TdxXMLString;
      const AValue: Double);
    procedure SetValueAsInt64(const AAttrName: TdxXMLString;
      const AValue: Int64);
    procedure SetValueAsInteger(const AAttrName: TdxXMLString; AValue: Integer);
    procedure SetValueAsString(const AAttrName: TdxXMLString;
      const AValue: string);
    procedure SetValueAsDateTime(const AAttrName: TdxXMLString;
      const AValue: TDateTime); overload;
    procedure SetValueAsDateTime(const AAttrName: TdxXMLString;
      const AValue: TdxXMLDateTime); overload;

    property First: TdxXMLNodeAttribute read GetFirst; // for internal use
    property Last: TdxXMLNodeAttribute read GetLast; // for internal use
    property Node: TdxXMLNode read FNode; // for internal use
  end;

  { TdxXMLNodes }

  TdxXMLNodes = class(TcxDoublyLinkedObjectList);

  { TdxXMLNode }

  TdxXMLNodeForEachProc = reference to procedure(ANode: TdxXMLNode;
    AUserData: Pointer);

  TdxXMLNodeClass = class of TdxXMLNode;

  TdxXMLNode = class(TcxDoublyLinkedObject)
  strict private
    FAttributes: TdxXMLNodeAttributes;
    FParent: TdxXMLNode;
    FText: string;

    function GetChildValue(const AName: TdxXMLString): string;
    function GetCount: Integer;
    function GetFirst: TdxXMLNode; inline;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TdxXMLNode;
    function GetLevel: Integer;
    function GetNameAsString: string; inline;
    function GetNameScope: TdxXMLString;
    function GetNameWithoutNameScope: TdxXMLString;
    function GetNext: TdxXMLNode; inline;
    function GetText: TdxXMLString; inline;
    function GetTextAsBoolean: Boolean; inline;
    function GetTextAsDateTime: TDateTime; inline;
    procedure SetText(const Value: TdxXMLString); inline;
    procedure SetTextAsBoolean(const Value: Boolean); inline;
    procedure SetTextAsDateTime(const Value: TDateTime); inline;
  protected
    FChildren: TdxXMLNodes;
    FName: TdxXMLString;

    procedure CheckTextEncoding; virtual;
    procedure ChildrenNeeded; inline;
    function CreateAttributes: TdxXMLNodeAttributes; virtual;
    function GetIsEmpty: Boolean; virtual;
    function GetNodeClass: TdxXMLNodeClass; virtual;
    function HasData: Boolean;
    function TextIsPreserveSpaceMode: Boolean;
    procedure ReadData(AStream: TStream; const AVersion: Cardinal = 0); virtual;
    procedure WriteAttributes(AStream: TStream); virtual;
    procedure WriteChildren(AStream: TStream; AAutoIndent: Boolean); virtual;
    procedure WriteContent(AStream: TStream; AAutoIndent: Boolean); virtual;
    procedure WriteData(AStream: TStream; AAutoIndent: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddChild(const ATagName: TdxXMLString): TdxXMLNode;
    procedure Assign(ASource: TdxXMLNode);
    procedure Clear; virtual;
    function HasAttribute(const AAttrName: TdxXMLString): Boolean;
    procedure SetAttribute(const AttrName: TdxXMLString; const AValue: Variant);

    function FindChild(const AName: TdxXMLString): TdxXMLNode; overload;
    function FindChild(const AName: TdxXMLString; out ANode: TdxXMLNode)
      : Boolean; overload; virtual;
    function FindChild(const ANames: array of TdxXMLString;
      out ANode: TdxXMLNode; ACanCreate: Boolean = False): Boolean; overload;
    procedure ForEach(AProc: TdxXMLNodeForEachProc; AUserData: Pointer = nil);

    property Attributes: TdxXMLNodeAttributes read FAttributes;
    // for internal use
    property Name: TdxXMLString read FName;
    property NameAsString: string read GetNameAsString;
    property NameScope: TdxXMLString read GetNameScope;
    property NameWithoutNameScope: TdxXMLString read GetNameWithoutNameScope;
    property Text: TdxXMLString read GetText write SetText;
    property TextAsBoolean: Boolean read GetTextAsBoolean
      write SetTextAsBoolean;
    property TextAsDateTime: TDateTime read GetTextAsDateTime
      write SetTextAsDateTime;
    property TextAsString: string read FText write FText;

    property ChildValues[const AName: TdxXMLString]: string read GetChildValue;
    property Count: Integer read GetCount;
    property First: TdxXMLNode read GetFirst; // for internal use
    property HasChildren: Boolean read GetHasChildren;
    property Index: Integer read GetIndex;
    property IsEmpty: Boolean read GetIsEmpty;
    property Items[Index: Integer]: TdxXMLNode read GetItem; default;
    // for internal use
    property Level: Integer read GetLevel;
    property Next: TdxXMLNode read GetNext; // for internal use
    property Parent: TdxXMLNode read FParent; // for internal use
  end;

  { TdxXMLDocument }

  TdxXMLDocument = class(TcxInterfacedPersistent)
  strict private
    FAutoIndent: Boolean;
    FEncoding: TdxXMLString;
    FRoot: TdxXMLNode;
    FStandalone: TdxXMLString;
    FVersion: TdxXMLString;
  protected
    function CreateParser: TdxXMLParser; virtual;
    function CreateRootNode: TdxXMLNode; virtual;
    function GetFooterText: TdxXMLString; virtual;
    function GetHeaderText: TdxXMLString; virtual;
    procedure Parse(const P: PAnsiChar; ACount: Integer); virtual;
  public
    constructor Create(AOwner: TPersistent = nil); override;
    constructor CreateEx(const AFileName: TFileName);
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    function AddChild(const ATagName: TdxXMLString): TdxXMLNode;
    function FindChild(const AName: TdxXMLString; out ANode: TdxXMLNode)
      : Boolean; overload;
    function FindChild(const ANames: array of TdxXMLString)
      : TdxXMLNode; overload;
    function FindChild(const ANames: array of TdxXMLString;
      out ANode: TdxXMLNode): Boolean; overload;
    procedure ForEach(const ANames: array of TdxXMLString;
      AProc: TdxXMLNodeForEachProc; AUserData: Pointer = nil);

    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromString(const AString: AnsiString);
    procedure SaveToFile(const AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);

    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
    property Encoding: TdxXMLString read FEncoding write FEncoding;
    property Root: TdxXMLNode read FRoot; // for internal use
    property Standalone: TdxXMLString read FStandalone write FStandalone;
    property Version: TdxXMLString read FVersion write FVersion;
  end;

  { TdxXMLPackableNode }

  TdxXMLPackableNode = class(TdxXMLNode)
  strict private
    FPackedData: TMemoryStream;
  protected
    procedure CheckPacked;
    function CreateAttributes: TdxXMLNodeAttributes; override;
    function GetIsEmpty: Boolean; override;
    function GetNodeClass: TdxXMLNodeClass; override;
    procedure WriteData(AStream: TStream; AAutoIndent: Boolean); override;
  public
    destructor Destroy; override;
    function AddChild(const ATagName: TdxXMLString): TdxXMLPackableNode;
      reintroduce;
    function FindChild(const AName: TdxXMLString; out ANode: TdxXMLPackableNode)
      : Boolean; reintroduce; overload;
    function FindChild(const ANames: array of TdxXMLString): TdxXMLPackableNode;
      reintroduce; overload;
    function FindChild(const ANames: array of TdxXMLString;
      out ANode: TdxXMLPackableNode): Boolean; reintroduce; overload;
    procedure Pack;
  end;

  { TdxXMLPackableNodeAttributes }

  TdxXMLPackableNodeAttributes = class(TdxXMLNodeAttributes)
  public
    function Add: TcxDoublyLinkedObject; override;
  end;

  { TdxXMLPackableDocument }

  TdxXMLPackableDocument = class(TdxXMLDocument)
  strict private
    function GetRoot: TdxXMLPackableNode;
  protected
    function CreateRootNode: TdxXMLNode; override;
  public
    property Root: TdxXMLPackableNode read GetRoot;
  end;

function dxStringToXMLString(const AValue: string): TdxXMLString; inline;
function dxXMLStringToString(const AValue: TdxXMLString): string; inline;

implementation

uses
  RTLConsts, SysConst, Math, dxStringHelper;

const
  sdxDefaultXMLVersion = '1.0';

  sAttributeEncoding = 'encoding';
  sAttributeVersion = 'version';
  sEncodingUTF8 = 'UTF-8';
  sEncodingWindows = 'Windows-';

  sXMLCDATABegin = AnsiString('<![CDATA[');
  sXMLCDATAEnd = AnsiString(']]>');

  sXMLSpaceModeAttr = AnsiString('xml:space');
  sXMLSpaceModePreserve = AnsiString('preserve');

  sXMLBoolValues: array [Boolean] of string = ('false', 'true');

type

  { TdxXMLServiceCharMapItem }

  TdxXMLServiceCharMapItem = record
    Char: Char;
    Replacement: string;
  end;

const
  XMLServiceCharMapCount = 8;
  XMLServiceCharMap: array [0 .. XMLServiceCharMapCount - 1]
    of TdxXMLServiceCharMapItem = ((Char: #9; Replacement: '&#9;'), (Char: #10;
    Replacement: '&#10;'), (Char: #13; Replacement: '&#13;'), (Char: '"';
    Replacement: '&quot;'), (Char: #39; Replacement: '&apos;'), (Char: '<';
    Replacement: '&lt;'), (Char: '>'; Replacement: '&gt;'), (Char: '&';
    Replacement: '&amp;'));

function FindDataInMemory(const AData, AMem: PByte;
  ADataSize, AMemSize, AMemOffset: Integer; out AOffset: Integer): Boolean;
var
  P: PByte;
  C: Integer;
begin
  Result := False;
  P := PByte(TdxNativeInt(AMem) + AMemOffset);
  C := AMemSize - AMemOffset;
  while C >= ADataSize do
  begin
    Result := (PByteArray(P)^[0] = PByteArray(AData)^[0]) and
      (PByteArray(P)^[ADataSize - 1] = PByteArray(AData)^[ADataSize - 1]) and
      CompareMem(P, AData, ADataSize);
    if Result then
    begin
      AOffset := AMemSize - C;
      Break;
    end;
    Dec(C);
    Inc(P);
  end;
end;

function FindStringInMemoryA(const S: AnsiString; AMem: PByte;
  AMemSize, AMemOffset: Integer; out AOffset: Integer): Boolean;
begin
  Result := FindDataInMemory(@S[1], AMem, Length(S), AMemSize,
    AMemOffset, AOffset);
end;

function dxStringToXMLString(const AValue: string): TdxXMLString; inline;
begin
  Result := dxStringToAnsiString(AValue, CP_UTF8);
end;

function dxXMLStringToString(const AValue: TdxXMLString): string; inline;
begin
  Result := dxAnsiStringToString(AValue, CP_UTF8);
end;

procedure WriteIndent(AStream: TStream; ALevel: Integer);
var
  C: AnsiChar;
begin
  C := dxTab;
  while ALevel > 0 do
  begin
    AStream.WriteBuffer(C, SizeOf(C));
    Dec(ALevel);
  end;
end;

procedure WriteString(AStream: TStream; const AString: TdxXMLString);
begin
  if Length(AString) > 0 then
    AStream.WriteBuffer(AString[1], Length(AString));
end;

{ EdxXMLUnexpectedToken }

constructor EdxXMLUnexpectedToken.Create(const AToken,
  AStringForParsing: string);
begin
  inherited CreateFmt('Unexpected token was founded ("%s" in "%s")',
    [IfThen(AToken <> #0, AToken, '#0'), AStringForParsing]);
end;

{ TdxXMLDateTime }

procedure TdxXMLDateTime.Assign(const ASource: TDateTime;
  AIsUTC: Boolean = False);
begin
  DecodeDate(ASource, Year, Month, Day);
  DecodeTime(ASource, Hour, Minute, Second, Millisecond);
  IsUTC := AIsUTC;
end;

procedure TdxXMLDateTime.Clear;
begin
  Year := 0;
  Month := 0;
  Day := 0;
  Hour := 0;
  Minute := 0;
  Second := 0;
  Millisecond := 0;
  IsUTC := False;
end;

class function TdxXMLDateTime.Create(const ASource: TDateTime; AIsUTC: Boolean)
  : TdxXMLDateTime;
begin
  Result.Assign(ASource, AIsUTC);
end;

procedure TdxXMLDateTime.Parse(const S: string);

  function GetNextPart(out ADelimiter: Char; var AIndex: Integer): string;
  var
    I: Integer;
  begin
    for I := AIndex to Length(S) do
    begin
      ADelimiter := S[I];
      if not CharInSet(ADelimiter, ['0' .. '9']) then
      begin
        Result := Copy(S, AIndex, I - AIndex);
        AIndex := I + 1;
        Exit;
      end;
    end;
    Result := Copy(S, AIndex, MaxInt);
    AIndex := Length(S) + 1;
    ADelimiter := #0;
  end;

  function GetNextPartAndCheckDelimiter(const AExpectedDelimiter: Char;
    var AIndex: Integer): string;
  var
    C: Char;
  begin
    Result := GetNextPart(C, AIndex);
    if C <> AExpectedDelimiter then
      raise EdxXMLUnexpectedToken.Create(C, S);
  end;

var
  ADelim: Char;
  AIndex: Integer;
  AOffsetHour: Word;
  AOffsetMinutes: Word;
  ASign: Integer;
  AValue: string;
begin
  Clear;
  AIndex := 1;
  Year := StrToIntDef(GetNextPartAndCheckDelimiter('-', AIndex), 0);
  Month := StrToIntDef(GetNextPartAndCheckDelimiter('-', AIndex), 0);
  Day := StrToIntDef(GetNextPart(ADelim, AIndex), 0);

  if ADelim = 'T' then
  begin
    Hour := StrToIntDef(GetNextPartAndCheckDelimiter(':', AIndex), 0);
    Minute := StrToIntDef(GetNextPartAndCheckDelimiter(':', AIndex), 0);
    Second := StrToIntDef(GetNextPart(ADelim, AIndex), 0);

    if ADelim = '.' then
    begin
      AValue := GetNextPart(ADelim, AIndex);
      Millisecond := Round(1000 * StrToIntDef(AValue, 0) / IntPower(10,
        Length(AValue)));
    end;
  end;

  case ADelim of
    'Z':
      begin
        GetNextPart(ADelim, AIndex);
        IsUTC := True;
      end;

    '+', '-':
      begin
        ASign := IfThen(ADelim = '-', -1, 1);
        AOffsetHour := StrToIntDef(GetNextPart(ADelim, AIndex), 0);
        if ADelim = ':' then
          AOffsetMinutes := StrToIntDef(GetNextPart(ADelim, AIndex), 0)
        else
          AOffsetMinutes := 0;

        Assign(ToDateTime - ASign * EncodeTime(AOffsetHour, AOffsetMinutes, 0,
          0), True);
      end;
  end;

  if ADelim <> #0 then
    raise EdxXMLUnexpectedToken.Create(ADelim, S);

end;

function TdxXMLDateTime.ToDateTime: TDateTime;
begin
  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second,
    Millisecond);
end;

function TdxXMLDateTime.ToString: string;
begin
  Result := FormatDateTime('yyyy-mm-dd''T''hh:nn:ss.zzz', ToDateTime) +
    IfThen(IsUTC, 'Z');
end;

{ TdxXMLSharedStrings }

constructor TdxXMLSharedStrings.Create;
begin
  inherited Create;
  FTableSize := MaxWord;
  SetLength(FTableA, FTableSize);
  SetLength(FTableS, FTableSize);
end;

destructor TdxXMLSharedStrings.Destroy;
begin
  Clear;
  SetLength(FTableA, 0);
  SetLength(FTableS, 0);
  inherited Destroy;
end;

function TdxXMLSharedStrings.Add(const AValue: TdxXMLToken): AnsiString;
begin
  Result := DoAdd(AValue.Buffer, AValue.BufferLengthInChars);
end;

function TdxXMLSharedStrings.Add(const AValue: AnsiString): AnsiString;
begin
  Result := DoAdd(PAnsiChar(AValue), Length(AValue), @AValue);
end;

function TdxXMLSharedStrings.Add(const AValue: string): string;
begin
  Result := DoAdd(PWideChar(AValue), Length(AValue), @AValue);
end;

procedure TdxXMLSharedStrings.Clear;
var
  AItemA: TAnsiStringValue;
  AItemS: TStringValue;
  ATempItemA: TAnsiStringValue;
  ATempItemS: TStringValue;
  I: Integer;
begin
  for I := 0 to FTableSize - 1 do
  begin
    AItemA := FTableA[I];
    FTableA[I] := nil;
    while AItemA <> nil do
    begin
      ATempItemA := AItemA;
      AItemA := AItemA.Next;
      ATempItemA.Free;
    end;
  end;
  for I := 0 to FTableSize - 1 do
  begin
    AItemS := FTableS[I];
    FTableS[I] := nil;
    while AItemS <> nil do
    begin
      ATempItemS := AItemS;
      AItemS := AItemS.Next;
      ATempItemS.Free;
    end;
  end;
end;

function TdxXMLSharedStrings.DoAdd(P: PAnsiChar; L: Integer;
  AValue: PAnsiString = nil): AnsiString;
var
  AIndex: Integer;
  AItem: TAnsiStringValue;
  AHash: Cardinal;
begin
  AHash := dxDotNetHash(PByte(P), L);
  AIndex := AHash mod FTableSize;
  AItem := FTableA[AIndex];
  if AItem = nil then
  begin
    AItem := TAnsiStringValue.Create(P, L, AHash, AValue);
    FTableA[AIndex] := AItem;
  end
  else
    repeat
      if (AItem.Hash = AHash) and (Length(AItem.Value) = L) then
      begin
        if CompareMem(PAnsiChar(AItem.Value), P, L) then
          Break;
      end;

      if AItem.Next = nil then
      begin
        AItem.Next := TAnsiStringValue.Create(P, L, AHash, AValue);
        AItem := AItem.Next;
        Break;
      end;
      AItem := AItem.Next;
    until False;

  Result := AItem.Value;
end;

function TdxXMLSharedStrings.DoAdd(P: PChar; L: Integer;
  AValue: PString): string;
var
  AIndex: Integer;
  AItem: TStringValue;
  AHash: Cardinal;
begin
  AHash := dxDotNetHash(PByte(P), L * SizeOf(Char));
  AIndex := AHash mod FTableSize;
  AItem := FTableS[AIndex];
  if AItem = nil then
  begin
    AItem := TStringValue.Create(P, L, AHash, AValue);
    FTableS[AIndex] := AItem;
  end
  else
    repeat
      if (AItem.Hash = AHash) and (Length(AItem.Value) = L) then
      begin
        if CompareMem(PChar(AItem.Value), P, L * SizeOf(Char)) then
          Break;
      end;

      if AItem.Next = nil then
      begin
        AItem.Next := TStringValue.Create(P, L, AHash, AValue);
        AItem := AItem.Next;
        Break;
      end;
      AItem := AItem.Next;
    until False;

  Result := AItem.Value;
end;

{ TdxXMLSharedStrings.TValue }

constructor TdxXMLSharedStrings.TAnsiStringValue.Create(P: PAnsiChar;
  L: Integer; AHash: Cardinal; AValue: PAnsiString);
begin
  Hash := AHash;
  if AValue <> nil then
    Value := AValue^
  else
    SetString(Value, P, L);
end;

{ TdxXMLSharedStrings.TStringValue }

constructor TdxXMLSharedStrings.TStringValue.Create(P: PChar; L: Integer;
  AHash: Cardinal; AValue: PString = nil);
begin
  Hash := AHash;
  if AValue <> nil then
    Value := AValue^
  else
    SetString(Value, P, L);
end;

{ TdxXMLParser }

constructor TdxXMLParser.Create(ADocument: TdxXMLDocument;
  AShareStrings: Boolean);
begin
  inherited Create;
  FDocument := ADocument;
  if AShareStrings then
    FSharedStrings := TdxXMLSharedStrings.Create;
end;

destructor TdxXMLParser.Destroy;
begin
  FreeAndNil(FSharedStrings);
  inherited Destroy;
end;

procedure TdxXMLParser.Parse(AScan: PAnsiChar; ACount: Integer);
var
  ANode: TdxXMLNode;
  ATagHeaderEndCursor: PAnsiChar;
  AToken: TdxXMLToken;
begin
  FData := AScan;
  FDataLength := ACount;

  ANode := Document.Root;
  ANode.Clear;

  ATagHeaderEndCursor := nil;
  while NextToken(AToken) do
  begin
    case AToken.TokenType of
      ttTagHeaderBegin:
        if (FDataLength > 0) and (FData^ = '?') then
          ParseDocumentHeader
        else if (FDataLength > 0) and (FData^ = '!') then
          SkipTag
        else
        begin
          ParseNodeValue(ANode, ATagHeaderEndCursor,
            FData - AToken.BufferLengthInChars);
          ANode := ParseNodeHeader(ANode.AddChild(''));
          if ANode = nil then
            Break;
          ATagHeaderEndCursor := FData;
        end;

      ttTagFooter:
        begin
          if not ANode.HasChildren then
            ParseNodeValue(ANode, ATagHeaderEndCursor,
              FData - AToken.BufferLengthInChars);
          ANode := ANode.Parent;
          if ANode = nil then
            Break;
          ATagHeaderEndCursor := nil;
        end;
    end;
  end;
end;

function TdxXMLParser.DecodeValue(const S: AnsiString): string;
begin
  case FEncoding of
    dxxeWindows:
      Result := dxAnsiStringToString(S, FEncodingCodePage);
    dxxeUTF8:
      Result := dxAnsiStringToString(S, CP_UTF8);
  else
    Result := dxAnsiStringToString(S);
  end;
  Result := TdxXMLHelper.DecodeString(Result);
end;

function TdxXMLParser.NextToken(out AToken: TdxXMLToken): Boolean;
begin
  Result := NextToken(FData, FDataLength, AToken);
end;

function TdxXMLParser.NextToken(var P: PAnsiChar; var C: Integer;
  out AToken: TdxXMLToken): Boolean;

  function IsSpace(const A: AnsiChar): LongBool; inline;
  begin
    Result := A in dxSpaceCharacters;
  end;

  function IsQuot(const A: AnsiChar): LongBool; inline;
  begin
    Result := (A = '"') or (A = #39);
  end;

  function IsTagDelimiter(const A: AnsiChar): LongBool; inline;
  begin
    Result := (A = '<') or (A = '>');
  end;

  function IsDelimiter(const A: AnsiChar): LongBool; inline;
  begin
    Result := (A = '=') or (A = '/') or IsTagDelimiter(A) or IsQuot(A) or
      IsSpace(A);
  end;

  procedure MoveToNextSymbol;
  begin
    if C > 0 then
    begin
      Inc(P);
      Dec(C);
    end;
  end;

  procedure MoveUntilQuotOrTag(AQuot: AnsiChar);
  begin
    while (C > 0) and (P^ <> AQuot) and not IsTagDelimiter(P^) do
    begin
      Inc(P);
      Dec(C);
    end;
  end;

  procedure MoveUntilDelimiter;
  begin
    while (C > 0) and not IsDelimiter(P^) do
    begin
      Inc(P);
      Dec(C);
    end;
  end;

  procedure SkipSpaces;
  begin
    while (C > 0) and IsSpace(P^) do
    begin
      Inc(P);
      Dec(C);
    end;
  end;

  procedure PutSpecialToken(AType: TdxXMLTokenID; ALength: Integer);
  begin
    AToken.Buffer := P;
    AToken.BufferLengthInChars := ALength;
    AToken.TokenType := AType;
    Dec(C, ALength);
    Inc(P, ALength);
  end;

  function CheckForCommentToken(out ALength: Integer): Boolean;

    function DoCheck(const AStartID, AFinishID: AnsiString): Boolean;
    var
      LS, LF: Integer;
    begin
      Result := False;
      LS := Length(AStartID);
      LF := Length(AFinishID);
      if (C > LS + LF) and CompareMem(P, @AStartID[1], LS) then
      begin
        Result := FindStringInMemoryA(AFinishID, PByte(P), C, LS, ALength);
        if Result then
          Inc(ALength, LF);
      end;
    end;

  begin
    Result := DoCheck('<!--', '-->') or DoCheck(sXMLCDATABegin, sXMLCDATAEnd);
  end;

  function CheckForSpecialToken: Boolean;
  var
    ALength: Integer;
  begin
    case P^ of
      '<':
        if (C > 1) and (PAnsiChar(P + 1)^ = '/') then
          PutSpecialToken(ttTagFooter, 2)
        else if (C > 1) and (PAnsiChar(P + 1)^ = '!') and
          CheckForCommentToken(ALength) then
          PutSpecialToken(ttComment, ALength)
        else
          PutSpecialToken(ttTagHeaderBegin, 1);

      '/', '?':
        if (C > 1) and (PAnsiChar(P + 1)^ = '>') then
          PutSpecialToken(ttTagEnd, 2);
      '=':
        PutSpecialToken(ttEqual, 1);
      '>':
        PutSpecialToken(ttTagHeaderEnd, 1);
    end;
    Result := AToken.TokenType <> ttUnknown;
  end;

var
  AQuot: AnsiChar;
begin
  SkipSpaces;
  AToken.TokenType := ttUnknown;
  AToken.BufferLengthInChars := 0;
  Result := C > 0;
  if Result then
  begin
    if IsQuot(P^) then
    begin
      AQuot := P^;
      MoveToNextSymbol;
      AToken.Buffer := P;
      MoveUntilQuotOrTag(AQuot);
      AToken.BufferLengthInChars := NativeUInt(P) - NativeUInt(AToken.Buffer);
      if P^ = AQuot then
        MoveToNextSymbol;
    end
    else if not CheckForSpecialToken then
    begin
      if IsDelimiter(P^) then
      begin
        AToken.Buffer := P;
        AToken.BufferLengthInChars := 1;
        MoveToNextSymbol;
      end
      else
      begin
        AToken.Buffer := P;
        MoveUntilDelimiter;
        AToken.BufferLengthInChars := NativeUInt(P) - NativeUInt(AToken.Buffer);
      end;
    end;
  end;
end;

procedure TdxXMLParser.ParseDocumentHeader;
var
  AAttr: TdxXMLNodeAttribute;
  ANode: TdxXMLNode;
begin
  ANode := TdxXMLNode.Create;
  try
    ParseNodeHeader(ANode);

    if ANode.Attributes.Find(sAttributeEncoding, AAttr) then
      Document.Encoding := AAttr.Value
    else
      Document.Encoding := sEncodingUTF8;

    if ANode.Attributes.Find(sAttributeVersion, AAttr) then
      Document.Version := AAttr.Value
    else
      Document.Version := sdxDefaultXMLVersion;

    ParseEncoding;
  finally
    ANode.Free;
  end;
end;

procedure TdxXMLParser.ParseEncoding;
var
  AEncodingValue: string;
begin
  AEncodingValue := dxAnsiStringToString(Document.Encoding);
  if SameText(AEncodingValue, sEncodingUTF8) then
    FEncoding := dxxeUTF8
  else if SameText(Copy(AEncodingValue, 1, Length(sEncodingWindows)),
    sEncodingWindows) then
  begin
    FEncoding := dxxeWindows;
    FEncodingCodePage :=
      StrToIntDef(Copy(AEncodingValue, Length(sEncodingWindows) + 1,
      MaxInt), 0);
  end
  else
    FEncoding := dxxeNone;
end;

function TdxXMLParser.ParseNodeHeader(ANode: TdxXMLNode): TdxXMLNode;
var
  AToken: TdxXMLToken;
  ATokenIndex: Integer;
begin
  ATokenIndex := 0;
  Result := ANode.Parent;
  while NextToken(AToken) do
  begin
    case AToken.TokenType of
      ttTagEnd:
        Break;
      ttTagHeaderBegin, ttComment:
        Exit(nil);
      ttTagHeaderEnd:
        begin
          Result := ANode;
          Break;
        end;
    else
      begin
        if (ATokenIndex > 3) then
          ATokenIndex := 1;
        if (ATokenIndex = 0) then
          ANode.FName := Share(AToken);
        if (ATokenIndex = 2) and (AToken.TokenType <> ttEqual) then
          ATokenIndex := 1;
        if (ATokenIndex = 1) then
          ANode.Attributes.Add(Share(AToken), '');
        if (ATokenIndex = 3) then
          ANode.Attributes.Last.ValueAsString :=
            Share(DecodeValue(TokenToString(AToken)));
      end;
    end;
    Inc(ATokenIndex);
  end;
end;

procedure TdxXMLParser.ParseNodeValue(ANode: TdxXMLNode;
  ATagHeaderEndCursor, ACursor: PAnsiChar);
var
  ALength: Integer;
  AValue: AnsiString;
  S1, S2: PAnsiChar;
begin
  if ATagHeaderEndCursor <> nil then
  begin
    S2 := ACursor - 1;
    S1 := ATagHeaderEndCursor;
    ALength := NativeUInt(S2) - NativeUInt(S1) + 1;

    if not ANode.TextIsPreserveSpaceMode then
    begin
      while (S1^ < ' ') and (ALength > 0) do
      begin
        Dec(ALength);
        Inc(S1);
      end;
      while (S2^ < ' ') and (ALength > 0) do
      begin
        Dec(ALength);
        Dec(S2);
      end;
    end;

    if (ALength >= Length(sXMLCDATABegin) + Length(sXMLCDATAEnd)) then
      if CompareMem(S1, @sXMLCDATABegin[1], Length(sXMLCDATABegin)) then
      begin
        Inc(S1, Length(sXMLCDATABegin));
        Dec(ALength, Length(sXMLCDATABegin) + Length(sXMLCDATAEnd));
      end;

    if ALength > 0 then
    begin
      SetString(AValue, S1, ALength);
      ANode.TextAsString := DecodeValue(AValue);
    end;
  end;
end;

function TdxXMLParser.Share(const AToken: TdxXMLToken): AnsiString;
begin
  if FSharedStrings <> nil then
    Result := FSharedStrings.Add(AToken)
  else
    Result := TokenToString(AToken);
end;

function TdxXMLParser.Share(const AValue: string): string;
begin
  if FSharedStrings <> nil then
    Result := FSharedStrings.Add(AValue)
  else
    Result := AValue;
end;

procedure TdxXMLParser.SkipTag;
var
  ANode: TdxXMLNode;
begin
  ANode := TdxXMLNode.Create;
  try
    ParseNodeHeader(ANode);
  finally
    ANode.Free;
  end;
end;

function TdxXMLParser.TokenToString(const AToken: TdxXMLToken): AnsiString;
begin
  SetString(Result, AToken.Buffer, AToken.BufferLengthInChars);
end;

{ TdxXMLHelper }

class function TdxXMLHelper.ExtractNameScope(const S: TdxXMLString)
  : TdxXMLString;
begin
  Result := Copy(S, 1, LastDelimiter(TdxXMLParser.NameScopeDelimiter, S));
end;

class function TdxXMLHelper.ExtractNameWithoutNameScope(const S: TdxXMLString)
  : TdxXMLString;
var
  AIndex: Integer;
begin
  AIndex := LastDelimiter(TdxXMLParser.NameScopeDelimiter, S);
  if AIndex > 0 then
    Result := Copy(S, AIndex + 1, MaxInt)
  else
    Result := S;
end;

class function TdxXMLHelper.DecodeBoolean(const S: string): Boolean;
var
  AValue: Integer;
begin
  if TryStrToInt(S, AValue) then
    Result := AValue <> 0
  else
    Result := SameText(S, sXMLBoolValues[True]);
end;

class function TdxXMLHelper.DecodeString(const S: string): string;
var
  ACode: Integer;
  ALength: Integer;
  AReplacement: Char;
  ASecondPassNeeded: Boolean;
  I, J, L: Integer;
begin
  Result := S;
  repeat
    I := 1;
    J := 1;
    ASecondPassNeeded := False;
    ALength := Length(Result);
    while I <= ALength do
    begin
      if IsEncodedCharacter(Result, I, ALength, ACode) and (ACode <= MaxByte)
      then
      begin
        ASecondPassNeeded := ASecondPassNeeded or (ACode = $5F);
        Result[J] := Char(ACode);
        Inc(I, 6);
      end
      else if (Result[I] = '&') and GetServiceCharacter(ALength - I + 1,
        @Result[I], L, AReplacement) then
      begin
        ASecondPassNeeded := ASecondPassNeeded or (AReplacement = '&');
        Result[J] := AReplacement;
        Inc(I, L - 1);
      end
      else
        Result[J] := Result[I];

      Inc(I);
      Inc(J);
    end;
    if I <> J then
      SetLength(Result, J - 1);
  until not ASecondPassNeeded;
end;

class function TdxXMLHelper.EncodeBoolean(const Value: Boolean): string;
begin
  Result := sXMLBoolValues[Value];
end;

class function TdxXMLHelper.EncodeString(const S: string;
  ARemoveBreakLines: Boolean): string;

  function CheckServiceChar(const C: Char; out AReplacement: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to XMLServiceCharMapCount - 1 do
      if XMLServiceCharMap[I].Char = C then
      begin
        AReplacement := XMLServiceCharMap[I].Replacement;
        Exit(True);
      end;
  end;

  function EncodeChar(const AChar: Char): string;
  begin
    Result := '_x' + IntToHex(Word(AChar), 4) + '_';
  end;

var
  ABuilder: TStringBuilder;
  AChar: Char;
  ALength: Integer;
  AReplacement: string;
  I, X: Integer;
begin
  Result := S;

  ABuilder := TdxStringBuilderManager.Get(MulDiv(Length(Result), 3, 2));
  try
    ALength := Length(Result);
    for I := 1 to ALength do
    begin
      AChar := Result[I];
      if ARemoveBreakLines and ((AChar = #13) or (AChar = #10)) then
        ABuilder.Append(' ')
      else

        if CheckServiceChar(AChar, AReplacement) then
        ABuilder.Append(AReplacement)
      else

        if Word(AChar) <= $1F then
        ABuilder.Append(EncodeChar(AChar))
      else

        if IsEncodedCharacter(Result, I, ALength, X) then
        ABuilder.Append(EncodeChar('_'))
      else
        ABuilder.Append(AChar);
    end;
    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

class function TdxXMLHelper.GetServiceCharacter(ACharCount: Integer; P: PWord;
  out L: Integer; out C: Char): Boolean;
var
  I: Integer;
  S: string;
begin
  Result := False;

  for I := 0 to XMLServiceCharMapCount - 1 do
  begin
    S := XMLServiceCharMap[I].Replacement;
    L := Length(S);
    if (L <= ACharCount) and CompareMem(P, @S[1], L * SizeOf(Char)) then
    begin
      C := XMLServiceCharMap[I].Char;
      Exit(True);
    end;
  end;

  if (ACharCount > 1) and (PWord(TdxNativeUInt(P) + SizeOf(Word))^ = $23) then
  begin
    I := 0;
    L := 2;
    Inc(P, 2);
    Dec(ACharCount, 2);
    while ACharCount > 0 do
    begin
      if P^ = $3B then
      begin
        Result := InRange(I, 0, MaxWord);
        if Result then
        begin
          C := Char(I);
          Inc(L);
        end;
        Break;
      end;
      if not InRange(P^, $30, $39) then
        Exit(False);
      I := I * 10 + (P^ - $30);
      Dec(ACharCount);
      Inc(L);
      Inc(P);
    end;
  end;
end;

class function TdxXMLHelper.IsBoolean(const S: string): Boolean;
begin
  Result := SameText(sXMLBoolValues[False], S) or
    SameText(sXMLBoolValues[True], S);
end;

class function TdxXMLHelper.IsEncodedCharacter(const S: string;
  APosition, ALength: Integer; out ACode: Integer): Boolean;
begin
  Result := (APosition <= ALength - 6) and (S[APosition] = '_') and
    (S[APosition + 1] = 'x') and (S[APosition + 6] = '_') and
    TryStrToInt('$' + Copy(S, APosition + 2, 4), ACode);
end;

class function TdxXMLHelper.IsPreserveSpacesNeeded(const S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    if dxCharInSet(S[1], dxSpaceCharacters) or
      dxCharInSet(S[L], dxSpaceCharacters) then
      Exit(True);
    for I := 1 to Length(S) do
    begin
      if dxCharInSet(S[I], dxBreakLineCharacters) then
        Exit(True);
    end;
  end;
  Result := False;
end;

{ TdxXMLNodeAttribute }

procedure TdxXMLNodeAttribute.Assign(ASource: TdxXMLNodeAttribute);
begin
  FName := ASource.FName;
  FValue := ASource.FValue;
end;

procedure TdxXMLNodeAttribute.WriteData(AStream: TStream);
begin
  WriteString(AStream, Name);
  WriteString(AStream, '="');
  WriteString(AStream, dxStringToXMLString(TdxXMLHelper.EncodeString
    (FValue, True)));
  WriteString(AStream, '"');
end;

function TdxXMLNodeAttribute.GetValueAsBoolean: Boolean;
begin
  Result := TdxXMLHelper.DecodeBoolean(ValueAsString);
end;

function TdxXMLNodeAttribute.GetValueAsFloat: Double;
begin
  Result := dxStrToFloat(ValueAsString);
end;

function TdxXMLNodeAttribute.GetValueAsInt64: Int64;
begin
  if not TryStrToInt64(ValueAsString, Result) then
  begin
    if TdxXMLHelper.IsBoolean(FValue) then
      Result := Ord(ValueAsBoolean)
    else
      raise EConvertError.CreateResFmt(@SInvalidInteger, [FValue]);
  end;
end;

function TdxXMLNodeAttribute.GetValueAsInteger: Integer;
begin
  if not TryStrToInt(ValueAsString, Result) then
  begin
    if TdxXMLHelper.IsBoolean(FValue) then
      Result := Ord(ValueAsBoolean)
    else
      raise EConvertError.CreateResFmt(@SInvalidInteger, [FValue]);
  end;
end;

function TdxXMLNodeAttribute.GetValue: TdxXMLString;
begin
  Result := dxStringToXMLString(FValue);
end;

procedure TdxXMLNodeAttribute.SetValueAsBoolean(AValue: Boolean);
begin
  ValueAsInteger := Ord(AValue);
end;

procedure TdxXMLNodeAttribute.SetValueAsFloat(const AValue: Double);
begin
  ValueAsString := dxFloatToStr(AValue);
end;

procedure TdxXMLNodeAttribute.SetValueAsInt64(const Value: Int64);
begin
  ValueAsString := IntToStr(Value);
end;

procedure TdxXMLNodeAttribute.SetValueAsInteger(AValue: Integer);
begin
  ValueAsString := IntToStr(AValue);
end;

procedure TdxXMLNodeAttribute.SetValue(const AValue: TdxXMLString);
begin
  FValue := dxXMLStringToString(AValue);
end;

function TdxXMLNodeAttribute.GetValueAsDateTime: TDateTime;
var
  AValue: TdxXMLDateTime;
begin
  AValue.Parse(ValueAsString);
  Result := AValue.ToDateTime;
end;

procedure TdxXMLNodeAttribute.SetValueAsDateTime(const Value: TDateTime);
var
  ADateTime: TdxXMLDateTime;
begin
  ADateTime.Assign(Value);
  ValueAsString := ADateTime.ToString;
end;

{ TdxXMLNodeAttributes }

constructor TdxXMLNodeAttributes.Create(ANode: TdxXMLNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString)
  : TdxXMLNodeAttribute;
begin
  Result := TdxXMLNodeAttribute(Add);
  Result.Name := AttrName;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString; AValue: Boolean)
  : TdxXMLNodeAttribute;
begin
  Result := Add(AttrName);
  Result.ValueAsBoolean := AValue;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString; AValue: Integer)
  : TdxXMLNodeAttribute;
begin
  Result := Add(AttrName);
  Result.ValueAsInteger := AValue;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString;
  const AValue: TdxXMLString): TdxXMLNodeAttribute;
begin
  Result := Add(AttrName);
  Result.Value := AValue;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString;
  const AValue: Double): TdxXMLNodeAttribute;
begin
  Result := Add(AttrName);
  Result.ValueAsFloat := AValue;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString;
  const AValue: Int64): TdxXMLNodeAttribute;
begin
  Result := Add(AttrName);
  Result.ValueAsInt64 := AValue;
end;

function TdxXMLNodeAttributes.Add(const AttrName: TdxXMLString;
  const AValue: string): TdxXMLNodeAttribute;
begin
  Result := Add(AttrName);
  Result.ValueAsString := AValue;
end;

procedure TdxXMLNodeAttributes.Assign(const ASource: TdxXMLNodeAttributes);
var
  AAttr: TdxXMLNodeAttribute;
begin
  Clear;
  AAttr := ASource.First;
  while AAttr <> nil do
  begin
    TdxXMLNodeAttribute(Add).Assign(AAttr);
    AAttr := TdxXMLNodeAttribute(AAttr.Next);
  end;
end;

procedure TdxXMLNodeAttributes.Delete(const AAttrName: TdxXMLString);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Remove(AAttr);
end;

function TdxXMLNodeAttributes.Exists(const AAttrName: TdxXMLString): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := Find(AAttrName, AAttr);
end;

function TdxXMLNodeAttributes.Find(const AAttrName: TdxXMLString;
  out AAttr: TdxXMLNodeAttribute): Boolean;
begin
  Result := False;
  AAttr := First;
  while AAttr <> nil do
  begin
    Result := SameText(AAttr.Name, AAttrName);
    if Result then
      Break;
    AAttr := TdxXMLNodeAttribute(AAttr.Next);
  end;
end;

function TdxXMLNodeAttributes.GetValue(const AAttrName, ADefaultValue
  : TdxXMLString): TdxXMLString;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Result := AAttr.Value
  else
    Result := ADefaultValue;
end;

function TdxXMLNodeAttributes.GetValueAsBoolean(const AAttrName: TdxXMLString;
  ADefaultValue: Boolean = False): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Result := AAttr.ValueAsBoolean
  else
    Result := ADefaultValue;
end;

function TdxXMLNodeAttributes.GetValueAsDateTime(const AAttrName: TdxXMLString;
  const ADefaultValue: TDateTime = 0): TDateTime;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Result := AAttr.ValueAsDateTime
  else
    Result := ADefaultValue;
end;

function TdxXMLNodeAttributes.GetValueAsDateTime(const AAttrName: TdxXMLString;
  out ADateTime: TdxXMLDateTime): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := Find(AAttrName, AAttr);
  if Result then
    ADateTime.Parse(AAttr.ValueAsString);
end;

function TdxXMLNodeAttributes.GetValueAsDefaultBoolean(const AAttrName
  : TdxXMLString): TdxDefaultBoolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Result := TdxDefaultBoolean(Ord(AAttr.ValueAsBoolean))
  else
    Result := bDefault;
end;

function TdxXMLNodeAttributes.GetValueAsFloat(const AAttrName: TdxXMLString;
  const ADefaultValue: Double = 0): Double;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Result := AAttr.ValueAsFloat
  else
    Result := ADefaultValue;
end;

function TdxXMLNodeAttributes.GetValueAsInt64(const AAttrName: TdxXMLString;
  const ADefaultValue: Int64 = 0): Int64;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := ADefaultValue;
  if Find(AAttrName, AAttr) then
    try
      Result := AAttr.ValueAsInt64;
    except
      Result := ADefaultValue;
    end;
end;

function TdxXMLNodeAttributes.GetValueAsInteger(const AAttrName: TdxXMLString;
  ADefaultValue: Integer = 0): Integer;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := ADefaultValue;
  if Find(AAttrName, AAttr) then
    try
      Result := AAttr.ValueAsInteger;
    except
      Result := ADefaultValue;
    end;
end;

function TdxXMLNodeAttributes.GetValueAsString(const AAttrName: TdxXMLString;
  const ADefaultValue: string = ''): string;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if Find(AAttrName, AAttr) then
    Result := AAttr.ValueAsString
  else
    Result := ADefaultValue;
end;

procedure TdxXMLNodeAttributes.SetValue(const AAttrName, AValue: TdxXMLString);
begin
  GetAttr(AAttrName).Value := AValue;
end;

procedure TdxXMLNodeAttributes.SetValueAsBoolean(const AAttrName: TdxXMLString;
  AValue: Boolean);
begin
  GetAttr(AAttrName).ValueAsBoolean := AValue;
end;

procedure TdxXMLNodeAttributes.SetValueAsDateTime(const AAttrName: TdxXMLString;
  const AValue: TdxXMLDateTime);
begin
  GetAttr(AAttrName).ValueAsString := AValue.ToString;
end;

procedure TdxXMLNodeAttributes.SetValueAsDefaultBoolean(const AAttrName
  : TdxXMLString; AValue: TdxDefaultBoolean);
begin
  if AValue = bDefault then
    Delete(AAttrName)
  else
    SetValueAsBoolean(AAttrName, AValue = bTrue);
end;

procedure TdxXMLNodeAttributes.SetValueAsFloat(const AAttrName: TdxXMLString;
  const AValue: Double);
begin
  GetAttr(AAttrName).ValueAsFloat := AValue;
end;

procedure TdxXMLNodeAttributes.SetValueAsInteger(const AAttrName: TdxXMLString;
  AValue: Integer);
begin
  GetAttr(AAttrName).ValueAsInteger := AValue;
end;

procedure TdxXMLNodeAttributes.SetValueAsInt64(const AAttrName: TdxXMLString;
  const AValue: Int64);
begin
  GetAttr(AAttrName).ValueAsInt64 := AValue;
end;

procedure TdxXMLNodeAttributes.SetValueAsString(const AAttrName: TdxXMLString;
  const AValue: string);
begin
  GetAttr(AAttrName).ValueAsString := AValue;
end;

procedure TdxXMLNodeAttributes.SetValueAsDateTime(const AAttrName: TdxXMLString;
  const AValue: TDateTime);
begin
  GetAttr(AAttrName).ValueAsDateTime := AValue;
end;

function TdxXMLNodeAttributes.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TdxXMLNodeAttribute.Create;
end;

function TdxXMLNodeAttributes.GetAttr(const AAttrName: TdxXMLString)
  : TdxXMLNodeAttribute;
begin
  if not Find(AAttrName, Result) then
    Result := Add(AAttrName);
end;

procedure TdxXMLNodeAttributes.WriteData(AStream: TStream);
var
  AAttribute: TdxXMLNodeAttribute;
begin
  AAttribute := First;
  while AAttribute <> nil do
  begin
    WriteString(AStream, ' ');
    AAttribute.WriteData(AStream);
    AAttribute := TdxXMLNodeAttribute(AAttribute.Next);
  end;
end;

function TdxXMLNodeAttributes.GetFirst: TdxXMLNodeAttribute;
begin
  Result := TdxXMLNodeAttribute(inherited First);
end;

function TdxXMLNodeAttributes.GetLast: TdxXMLNodeAttribute;
begin
  Result := TdxXMLNodeAttribute(inherited Last);
end;

{ TdxXMLNode }

constructor TdxXMLNode.Create;
begin
  inherited Create;
  FAttributes := CreateAttributes;
end;

destructor TdxXMLNode.Destroy;
begin
  if (Parent <> nil) and (Parent.FChildren <> nil) then
    Parent.FChildren.Extract(Self);
  FreeAndNil(FAttributes);
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TdxXMLNode.AddChild(const ATagName: TdxXMLString): TdxXMLNode;
begin
  ChildrenNeeded;
  Result := GetNodeClass.Create;
  FChildren.DoAdd(Result);
  Result.FParent := Self;
  Result.FName := ATagName;
end;

procedure TdxXMLNode.Assign(ASource: TdxXMLNode);
var
  I: Integer;
begin
  Clear;
  FName := ASource.Name;
  Attributes.Assign(ASource.Attributes);
  for I := 0 to ASource.Count - 1 do
    AddChild('').Assign(ASource[I]);
end;

procedure TdxXMLNode.Clear;
begin
  Attributes.Clear;
  FreeAndNil(FChildren);
end;

function TdxXMLNode.HasAttribute(const AAttrName: TdxXMLString): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := Attributes.Find(AAttrName, AAttr);
end;

procedure TdxXMLNode.SetAttribute(const AttrName: TdxXMLString;
  const AValue: Variant);
begin
  Attributes.SetValueAsString(AttrName, dxVariantToString(AValue));
end;

function TdxXMLNode.FindChild(const AName: TdxXMLString;
  out ANode: TdxXMLNode): Boolean;
begin
  ANode := First;
  while (ANode <> nil) and not SameText(AName, ANode.Name) do
    ANode := ANode.Next;
  Result := ANode <> nil;
end;

function TdxXMLNode.FindChild(const AName: TdxXMLString): TdxXMLNode;
begin
  if not FindChild(AName, Result) then
    Result := nil;
end;

function TdxXMLNode.FindChild(const ANames: array of TdxXMLString;
  out ANode: TdxXMLNode; ACanCreate: Boolean = False): Boolean;
var
  ATempNode: TdxXMLNode;
  I: Integer;
begin
  ANode := Self;
  for I := 0 to Length(ANames) - 1 do
  begin
    ATempNode := ANode.FindChild(ANames[I]);
    if (ATempNode = nil) and ACanCreate then
      ATempNode := ANode.AddChild(ANames[I]);
    ANode := ATempNode;
    if ANode = nil then
      Break;
  end;
  Result := (Length(ANames) > 0) and (ANode <> nil);
end;

procedure TdxXMLNode.ForEach(AProc: TdxXMLNodeForEachProc;
  AUserData: Pointer = nil);
var
  ANode: TdxXMLNode;
begin
  ANode := First;
  while ANode <> nil do
  begin
    AProc(ANode, AUserData);
    ANode := ANode.Next;
  end;
end;

procedure TdxXMLNode.CheckTextEncoding;
begin
  if TdxXMLHelper.IsPreserveSpacesNeeded(FText) then
    Attributes.SetValue(sXMLSpaceModeAttr, sXMLSpaceModePreserve)
  else
    Attributes.Delete(sXMLSpaceModeAttr);
end;

procedure TdxXMLNode.ChildrenNeeded;
begin
  if FChildren = nil then
    FChildren := TdxXMLNodes.Create;
end;

function TdxXMLNode.CreateAttributes: TdxXMLNodeAttributes;
begin
  Result := TdxXMLNodeAttributes.Create(Self);
end;

function TdxXMLNode.GetIsEmpty: Boolean;
begin
  Result := (Attributes.Count = 0) and not HasChildren and (Length(FText) = 0);
end;

function TdxXMLNode.GetNodeClass: TdxXMLNodeClass;
begin
  Result := TdxXMLNode;
end;

function TdxXMLNode.HasData: Boolean;
begin
  Result := (First <> nil) or (Length(FText) > 0);
end;

function TdxXMLNode.TextIsPreserveSpaceMode: Boolean;
begin
  Result := SameText(Attributes.GetValue(sXMLSpaceModeAttr),
    sXMLSpaceModePreserve);
end;

procedure TdxXMLNode.ReadData(AStream: TStream; const AVersion: Cardinal = 0);
begin
  // do nothing
end;

procedure TdxXMLNode.WriteAttributes(AStream: TStream);
begin
  Attributes.WriteData(AStream);
end;

procedure TdxXMLNode.WriteChildren(AStream: TStream; AAutoIndent: Boolean);
var
  ANode: TdxXMLNode;
begin
  ANode := First;
  while ANode <> nil do
  begin
    ANode.WriteData(AStream, AAutoIndent);
    ANode := ANode.Next;
  end;
end;

procedure TdxXMLNode.WriteData(AStream: TStream; AAutoIndent: Boolean);
begin
  if AAutoIndent and not Parent.TextIsPreserveSpaceMode then
    WriteIndent(AStream, Level);
  CheckTextEncoding;
  WriteString(AStream, '<' + Name);
  WriteAttributes(AStream);

  if HasData then
  begin
    WriteString(AStream, '>');
    WriteContent(AStream, AAutoIndent);
    WriteString(AStream, '</' + Name + '>');
  end
  else
    WriteString(AStream, '/>');

  if AAutoIndent then
    WriteString(AStream, dxCRLF);
end;

procedure TdxXMLNode.WriteContent(AStream: TStream; AAutoIndent: Boolean);
begin
  if FText <> '' then
    WriteString(AStream,
      dxStringToXMLString(TdxXMLHelper.EncodeString(FText, False)));
  if HasChildren then
  begin
    if AAutoIndent and ((FText = '') or not TextIsPreserveSpaceMode) then
      WriteString(AStream, dxCRLF);
    WriteChildren(AStream, AAutoIndent);
    if AAutoIndent then
      WriteIndent(AStream, Level);
  end;
end;

function TdxXMLNode.GetChildValue(const AName: TdxXMLString): string;
var
  ANode: TdxXMLNode;
begin
  if FindChild(AName, ANode) then
    Result := ANode.TextAsString
  else
    Result := '';
end;

function TdxXMLNode.GetCount: Integer;
begin
  if FChildren <> nil then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TdxXMLNode.GetFirst: TdxXMLNode;
begin
  if FChildren <> nil then
    Result := TdxXMLNode(FChildren.First)
  else
    Result := nil;
end;

function TdxXMLNode.GetHasChildren: Boolean;
begin
  Result := First <> nil;
end;

function TdxXMLNode.GetIndex: Integer;
var
  ANode: TdxXMLNode;
begin
  if Parent <> nil then
  begin
    Result := 0;
    ANode := Parent.First;
    while ANode <> nil do
    begin
      if ANode = Self then
        Exit(Result);
      ANode := ANode.Next;
      Inc(Result);
    end;
  end;
  Result := -1;
end;

function TdxXMLNode.GetItem(Index: Integer): TdxXMLNode;
var
  AIndex: Integer;
  ANode: TdxXMLNode;
begin
  AIndex := 0;
  ANode := First;
  while ANode <> nil do
  begin
    if AIndex = Index then
      Exit(ANode);
    ANode := ANode.Next;
    Inc(AIndex);
  end;
  raise EListError.CreateFmt(SListIndexError, [Index]);
end;

function TdxXMLNode.GetLevel: Integer;
var
  ANode: TdxXMLNode;
begin
  Result := -1;
  ANode := Parent;
  while ANode <> nil do
  begin
    Inc(Result);
    ANode := ANode.Parent;
  end;
end;

function TdxXMLNode.GetNameAsString: string;
begin
  Result := dxXMLStringToString(Name);
end;

function TdxXMLNode.GetNameScope: TdxXMLString;
begin
  Result := TdxXMLHelper.ExtractNameScope(Name);
end;

function TdxXMLNode.GetNameWithoutNameScope: TdxXMLString;
begin
  Result := TdxXMLHelper.ExtractNameWithoutNameScope(Name);
end;

function TdxXMLNode.GetNext: TdxXMLNode;
begin
  Result := TdxXMLNode(inherited Next);
end;

function TdxXMLNode.GetTextAsBoolean: Boolean;
begin
  Result := TdxXMLHelper.DecodeBoolean(FText);
end;

function TdxXMLNode.GetTextAsDateTime: TDateTime;
var
  ADateTime: TdxXMLDateTime;
begin
  ADateTime.Parse(TextAsString);
  Result := ADateTime.ToDateTime;
end;

function TdxXMLNode.GetText: TdxXMLString;
begin
  Result := dxStringToXMLString(FText);
end;

procedure TdxXMLNode.SetText(const Value: TdxXMLString);
begin
  FText := dxXMLStringToString(Value);
end;

procedure TdxXMLNode.SetTextAsBoolean(const Value: Boolean);
begin
  FText := TdxXMLHelper.EncodeBoolean(Value);
end;

procedure TdxXMLNode.SetTextAsDateTime(const Value: TDateTime);
var
  ADateTime: TdxXMLDateTime;
begin
  ADateTime.Assign(Value);
  FText := ADateTime.ToString;
end;

{ TdxXMLDocument }

constructor TdxXMLDocument.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FRoot := CreateRootNode;
  FVersion := sdxDefaultXMLVersion;
  FEncoding := sEncodingUTF8;
end;

constructor TdxXMLDocument.CreateEx(const AFileName: TFileName);
begin
  Create(nil);
  LoadFromFile(AFileName);
end;

destructor TdxXMLDocument.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TdxXMLDocument.BeginUpdate;
begin
end;

procedure TdxXMLDocument.EndUpdate;
begin
end;

function TdxXMLDocument.AddChild(const ATagName: TdxXMLString): TdxXMLNode;
begin
  Result := Root.AddChild(ATagName);
end;

function TdxXMLDocument.FindChild(const AName: TdxXMLString;
  out ANode: TdxXMLNode): Boolean;
begin
  Result := Root.FindChild(AName, ANode);
end;

function TdxXMLDocument.FindChild(const ANames: array of TdxXMLString;
  out ANode: TdxXMLNode): Boolean;
begin
  Result := Root.FindChild(ANames, ANode);
end;

procedure TdxXMLDocument.ForEach(const ANames: array of TdxXMLString;
  AProc: TdxXMLNodeForEachProc; AUserData: Pointer = nil);
var
  ANode: TdxXMLNode;
begin
  if FindChild(ANames, ANode) then
    ANode.ForEach(AProc, AUserData);
end;

function TdxXMLDocument.FindChild(const ANames: array of TdxXMLString)
  : TdxXMLNode;
begin
  if not FindChild(ANames, Result) then
    Result := nil;
end;

procedure TdxXMLDocument.LoadFromFile(const AFileName: TFileName);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxXMLDocument.LoadFromStream(AStream: TStream);
var
  ABuffer: PAnsiChar;
  ABufferSize: Integer;
begin
  BeginUpdate;
  try
    Root.Clear;
    ABufferSize := AStream.Size - AStream.Position;
    if ABufferSize > 0 then
    begin
      ABuffer := AllocMem(ABufferSize);
      try
        AStream.ReadBuffer(ABuffer^, ABufferSize);
        Parse(ABuffer, ABufferSize);
      finally
        FreeMem(ABuffer);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxXMLDocument.LoadFromString(const AString: AnsiString);
begin
  BeginUpdate;
  try
    Root.Clear;
    Parse(PAnsiChar(AString), Length(AString));
  finally
    EndUpdate;
  end;
end;

procedure TdxXMLDocument.SaveToFile(const AFileName: TFileName);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxXMLDocument.SaveToStream(AStream: TStream);
begin
  FEncoding := sEncodingUTF8;
  WriteString(AStream, GetHeaderText);
  Root.WriteChildren(AStream, AutoIndent);
  WriteString(AStream, GetFooterText);
end;

function TdxXMLDocument.CreateParser: TdxXMLParser;
begin
  Result := TdxXMLParser.Create(Self);
end;

function TdxXMLDocument.CreateRootNode: TdxXMLNode;
begin
  Result := TdxXMLNode.Create;
end;

function TdxXMLDocument.GetFooterText: TdxXMLString;
begin
  Result := '';
end;

function TdxXMLDocument.GetHeaderText: TdxXMLString;
begin
  Result := '';
  if (Version <> '') or (Encoding <> '') or (Standalone <> '') then
  begin
    Result := '<?xml';
    if Length(Version) > 0 then
      Result := Result + ' version="' + Version + '"';
    if Length(Encoding) > 0 then
      Result := Result + ' encoding="' + Encoding + '"';
    if Length(Standalone) > 0 then
      Result := Result + ' standalone="' + Standalone + '"';
    Result := Result + '?>' + dxCRLF;
  end;
end;

procedure TdxXMLDocument.Parse(const P: PAnsiChar; ACount: Integer);
var
  AParser: TdxXMLParser;
begin
  AParser := CreateParser;
  try
    AParser.Parse(P, ACount);
  finally
    AParser.Free;
  end;
end;

{ TdxXMLPackableNode }

destructor TdxXMLPackableNode.Destroy;
begin
  FreeAndNil(FPackedData);
  inherited Destroy;
end;

function TdxXMLPackableNode.AddChild(const ATagName: TdxXMLString)
  : TdxXMLPackableNode;
begin
  Result := TdxXMLPackableNode(inherited AddChild(ATagName));
end;

function TdxXMLPackableNode.FindChild(const AName: TdxXMLString;
  out ANode: TdxXMLPackableNode): Boolean;
begin
  Result := FindChild(AName, TdxXMLNode(ANode));
end;

function TdxXMLPackableNode.FindChild(const ANames: array of TdxXMLString)
  : TdxXMLPackableNode;
begin
  Result := TdxXMLPackableNode(FindChild(ANames));
end;

function TdxXMLPackableNode.FindChild(const ANames: array of TdxXMLString;
  out ANode: TdxXMLPackableNode): Boolean;
begin
  Result := FindChild(ANames, TdxXMLNode(ANode));
end;

procedure TdxXMLPackableNode.Pack;
begin
  if not IsEmpty then
  begin
    FPackedData := TMemoryStream.Create;
    inherited WriteData(FPackedData, False);
    Clear;
  end;
end;

procedure TdxXMLPackableNode.CheckPacked;
begin
  if FPackedData <> nil then
    raise EInvalidOperation.Create(ClassName);
end;

function TdxXMLPackableNode.CreateAttributes: TdxXMLNodeAttributes;
begin
  Result := TdxXMLPackableNodeAttributes.Create(Self);
end;

function TdxXMLPackableNode.GetIsEmpty: Boolean;
begin
  Result := inherited GetIsEmpty and (FPackedData = nil);
end;

function TdxXMLPackableNode.GetNodeClass: TdxXMLNodeClass;
begin
  Result := TdxXMLPackableNode;
end;

procedure TdxXMLPackableNode.WriteData(AStream: TStream; AAutoIndent: Boolean);
begin
  if FPackedData <> nil then
    AStream.WriteBuffer(FPackedData.Memory^, FPackedData.Size)
  else
    inherited WriteData(AStream, AAutoIndent);
end;

{ TdxXMLPackableNodeAttributes }

function TdxXMLPackableNodeAttributes.Add: TcxDoublyLinkedObject;
begin
  TdxXMLPackableNode(Node).CheckPacked;
  Result := inherited Add;
end;

{ TdxXMLPackableDocument }

function TdxXMLPackableDocument.CreateRootNode: TdxXMLNode;
begin
  Result := TdxXMLPackableNode.Create;
end;

function TdxXMLPackableDocument.GetRoot: TdxXMLPackableNode;
begin
  Result := TdxXMLPackableNode(inherited Root);
end;

end.
