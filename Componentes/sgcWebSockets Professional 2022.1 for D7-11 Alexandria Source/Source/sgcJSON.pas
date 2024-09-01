{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcJSON;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
{$IFDEF NEXTGEN} System.Generics.Collections{$ELSE}Contnrs{$ENDIF};

type

  TsgcJSON = class;
  TsgcObjectJSON = class;
  TsgcObjectJSONList = class;
{$IFDEF SGC_JSON_INTF}
  IsgcJSON = interface;
  IsgcObjectJSON = interface;
{$ELSE}
  IsgcJSON = TsgcJSON;
  IsgcObjectJSON = TsgcObjectJSON;
{$ENDIF}
  TsgcJSONtype = (sgcJSONUndefined, sgcJSONObject, sgcJSONList, sgcJSONString,
    sgcJSONNumber, sgcJSONBoolean, sgcJSONNull);

{$IFDEF SGC_JSON_INTF}

  IsgcObjectJSON = interface(IInterface)
    ['{DF1B6784-B501-46BC-A679-E4CEACBD9022}']

    function GetJSONObject: IsgcJSON;
    function GetJSONType: TsgcJSONtype;
    procedure SetJSONObject(const Value: IsgcJSON);
    procedure SetJSONType(const Value: TsgcJSONtype);
    property JSONObject: IsgcJSON read GetJSONObject write SetJSONObject;
    function GetNode(const aName: String): IsgcObjectJSON;
    procedure SetNode(const aName: String; const aValue: IsgcObjectJSON);
    function GetItem(const i: Integer): IsgcObjectJSON;
    procedure SetItem(const i: Integer; const aValue: IsgcObjectJSON);
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    function GetCount: Integer;
    function GetName: String;
    procedure SetName_(const Value: String);

    procedure Clear;

    property Item[const i: Integer]: IsgcObjectJSON read GetItem write SetItem;
    property Node[const aName: String]: IsgcObjectJSON read GetNode
      write SetNode;
    property Count: Integer read GetCount;
    property JSONType: TsgcJSONtype read GetJSONType write SetJSONType;
    property Name: String read GetName write SetName_;
    property Value: Variant read GetValue write SetValue;
  end;

  IsgcJSON = interface(IInterface)
    ['{76C5903F-99BE-4E3C-85C8-413B6B278C3F}']

    { json items }
    function GetCount: Integer;
    function GetNode(const aName: String): IsgcObjectJSON;
    procedure SetNode(const aName: String; const aValue: IsgcObjectJSON);
    function GetItem(i: Integer): IsgcObjectJSON;
    procedure SetItem(i: Integer; const aValue: IsgcObjectJSON);
    function GetIsArray: Boolean;
    procedure SetIsArray(const Value: Boolean);
    property Node[const aName: String]: IsgcObjectJSON read GetNode
      write SetNode;
    property Item[i: Integer]: IsgcObjectJSON read GetItem write SetItem;
    property Count: Integer read GetCount;
    property IsArray: Boolean read GetIsArray write SetIsArray;
    { json items }

    { json objects }
    function AddPair(const aName, aValue: String): IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Integer)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Int64)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Double)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Boolean)
      : IsgcObjectJSON; overload;
    function AddObject(const aName: string; const aValue: string = '')
      : IsgcObjectJSON;
    function AddArray(const aName: string; const aValue: string = '')
      : IsgcObjectJSON;
    { json objects }

    { clear objects }
    procedure Clear;
    { clear objects }

    { read json text }
    procedure Read(aText: String);
    { read json text }

    { write json text }
    function GetText: String;
    property Text: String read GetText;
    { write json text }
  end;
{$ENDIF}
{$IFDEF SGC_JSON_INTF}

  TInterfacedComponent = class(TComponent, IInterface)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    procedure FreeInstance; override;
  private
    FRefCount: Integer;
    FDestroyObject: Boolean;
  end;
{$ENDIF}
{$IFDEF SGC_JSON_INTF}

  TsgcObjectJSON = class(TInterfacedComponent, IsgcObjectJSON)
{$ELSE}
  TsgcObjectJSON = class(TComponent)
{$ENDIF}
  private
    FQuoted: Boolean;
    FValue: Variant;
    FName: String;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);

    { sgcJSONObject }
  private
    FJSONObject: IsgcJSON;
    FJSONType: TsgcJSONtype;
  protected
    function GetCount: Integer;
    function GetJSONObject: IsgcJSON;
    function GetJSONType: TsgcJSONtype;
    function GetName: String;
    procedure SetJSONObject(const Value: IsgcJSON);
    procedure SetJSONType(const Value: TsgcJSONtype);
    procedure SetName_(const Value: String);
  public
    constructor Create(aOwner: TComponent); override;
  public
    procedure Clear;
  public
    property JSONObject: IsgcJSON read GetJSONObject write SetJSONObject;

  protected
    function GetNode(const aName: String): IsgcObjectJSON;
    procedure SetNode(const aName: String; const aValue: IsgcObjectJSON);
    function GetItem(const i: Integer): IsgcObjectJSON;
    procedure SetItem(const i: Integer; const aValue: IsgcObjectJSON);
  public
    property Item[const i: Integer]: IsgcObjectJSON read GetItem write SetItem;
    property Node[const aName: String]: IsgcObjectJSON read GetNode
      write SetNode;
    { sgcJSONObject }

  public
    property Count: Integer read GetCount;
    property JSONType: TsgcJSONtype read GetJSONType write SetJSONType;
    property Name: String read GetName write SetName_;
    property Value: Variant read GetValue write SetValue;
  end;

  TsgcObjectJSONList = class({$IFDEF NEXTGEN}TList<TsgcObjectJSON>{$ELSE}TObjectList{$ENDIF})
  protected
    function GetNode(const aName: String): TsgcObjectJSON;
    function GetItem(i: Integer): TsgcObjectJSON;
    procedure SetNode(const aName: String; const aValue: TsgcObjectJSON);
    procedure SetItem(i: Integer; const aValue: TsgcObjectJSON);
  public
    property Node[const aName: String]: TsgcObjectJSON read GetNode
      write SetNode;
    property Item[i: Integer]: TsgcObjectJSON read GetItem write SetItem;
  public
    destructor Destroy; override;
{$IFDEF NEXTGEN}
  private
    FOwnsObjects: Boolean;
  public
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
{$ENDIF}
  end;

{$IFDEF SGC_JSON_INTF}

  TsgcJSON = class(TInterfacedComponent, IsgcJSON)
{$ELSE}
  TsgcJSON = class(TComponent)
{$ENDIF}
    { create }
  protected
    function DoCreateObject(var aText: String; var Pos: Integer): Boolean;
    function DoCreateList(var aText: String; var Pos: Integer): Boolean;
    { create }

    { read }
  protected
    function DoReadObject(var aText: String; var Pos: Integer): Boolean;
    function DoReadList(var aText: String; var Pos: Integer): Boolean;
    function DoReadValue(var aText: String; var Pos: Integer): Boolean;
    function DoReadString(var aText: String; var Pos: Integer): Boolean;
    function DoReadChar(var aText: String; var Pos: Integer): Boolean;
    function DoReadInteger(var aText: String; var Pos: Integer): Boolean;
    function DoReadNumber(var aText: String; var Pos: Integer): Boolean;
  protected
    procedure DoSpaces(var aText: String; var Pos: Integer);
    { read }

    { get }
  protected
    function DoGetObject(aObject: TsgcObjectJSON;
      const aIsArray: Boolean = False): String;
    function DoGetString(aObject: TsgcObjectJSON;
      const aIsArray: Boolean = False): String;
    function DoGetNumber(aObject: TsgcObjectJSON;
      const aIsArray: Boolean = False): String;
    function DoGetBoolean(aObject: TsgcObjectJSON;
      const aIsArray: Boolean = False): String;
    function DoGetNull(aObject: TsgcObjectJSON;
      const aIsArray: Boolean = False): String;
    function DoGetList(aObject: TsgcObjectJSON): String;
    { get }

    { json items }
  private
    FIsArray: Boolean;
  protected
    function GetCount: Integer; virtual;
    function GetNode(const aName: String): IsgcObjectJSON; virtual;
    procedure SetNode(const aName: String;
      const aValue: IsgcObjectJSON); virtual;
    function GetItem(i: Integer): IsgcObjectJSON; virtual;
    procedure SetItem(i: Integer; const aValue: IsgcObjectJSON); virtual;
    function GetIsArray: Boolean; virtual;
    procedure SetIsArray(const Value: Boolean); virtual;
  public
    property Node[const aName: String]: IsgcObjectJSON read GetNode
      write SetNode;
    property Item[i: Integer]: IsgcObjectJSON read GetItem write SetItem;
    property Count: Integer read GetCount;
    property IsArray: Boolean read GetIsArray write SetIsArray;
    { json items }

    { AddPair }
  protected
    function DoAdd(const aName: String; aValue: Variant; aType: TsgcJSONtype)
      : IsgcObjectJSON; overload;
  public
    function AddPair(const aName, aValue: String): IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Integer)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Int64)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Double)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Boolean)
      : IsgcObjectJSON; overload;
    function AddPair(const aName: String): IsgcObjectJSON; overload;
    function AddObject(const aName: string; const aValue: string = '')
      : IsgcObjectJSON;
    function AddArray(const aName: string; const aValue: string = '')
      : IsgcObjectJSON;
    { AddPair }

    { data }
  private
    FData: TsgcObjectJSONList;
    function GetData: TsgcObjectJSONList;
  protected
    property Data: TsgcObjectJSONList read GetData write FData;
  public
    procedure Clear;
    { data }

    { JSON object }
  private
    FJSONName: String;
    FJSONType: TsgcJSONtype;
    FJSONValue: Variant;
    procedure DoJSONStart(const aName: String);
    procedure DoJSONValue(const aValue: String);
    procedure DoJSONEnd;
  protected
    function GetJSONValue(const aJSON: TsgcObjectJSON;
      aIsArray: Boolean = False): string; virtual;
    { JSON object }

    { parse }
  protected
    function GetText: String;
  public
    procedure Read(aText: String);
    property Text: String read GetText;
    { parse }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { property }
  private
    FEscapeStrings: Boolean;
    function EscapeString(const aValue: string): string;
  public
    property EscapeStrings: Boolean read FEscapeStrings write FEscapeStrings;
    { propertye }
  end;

  TsgcOwnerComponent = Class(TComponent)
  private
    FInstance: IsgcJSON;
  protected
    property Instance: IsgcJSON read FInstance write FInstance;
  End;

procedure SetJSONClass(const aClass: TComponentClass);
function GetJSONInstance(aOwner: TComponent = nil): IsgcJSON;
procedure FreeJSONInstance(aInstance: IsgcJSON);

implementation

uses
  sgcBase_Helpers, sgcWebSocket_Helpers;

var
  oJSONClass: TComponentClass = TsgcJSON;
  oJSONList: TThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF};

procedure SetJSONClass(const aClass: TComponentClass);
begin
{$IFDEF SGC_JSON_INTF}
  oJSONClass := aClass;
{$ELSE}
  raise Exception.Create('Unsupported method.');
{$ENDIF}
end;

function GetJSONInstance(aOwner: TComponent = nil): IsgcJSON;
var
  oOwnerComponent: TsgcOwnerComponent;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := nil;

  if Assigned(oJSONClass) then
  begin
    // ... if hasn't owner create a dummy component and
    // ... save in a list
    oOwnerComponent := nil;
    if not Assigned(aOwner) then
    begin
      oOwnerComponent := TsgcOwnerComponent.Create(nil);
      aOwner := oOwnerComponent;
    end;

    // ... create instance
    result := oJSONClass.Create(aOwner) as IsgcJSON;

    // ... save to list
    if Assigned(oOwnerComponent) then
    begin
      oOwnerComponent.Instance := result;
      oList := oJSONList.LockList;
      Try
        oList.Add(oOwnerComponent);
      Finally
        oJSONList.UnlockList;
      End;
    end;
  end;
end;

procedure FreeJSONInstance(aInstance: IsgcJSON);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := oJSONList.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcOwnerComponent(oList[i]).Instance = aInstance then
      begin
{$IFDEF NEXTGEN}
        TsgcOwnerComponent(oList[i]).DisposeOf;
{$ELSE}
        TsgcOwnerComponent(oList[i]).Free;
{$ENDIF}
        oList.Delete(i);
        exit;
      end;
    end;
  Finally
    oJSONList.UnlockList;
  End;
  // ... free if not in list
{$IFDEF NEXTGEN}
  TComponent(aInstance).DisposeOf;
{$ELSE}
  TComponent(aInstance).Free;
{$ENDIF}
end;

function sgcStringEnd(const aText: String): Integer;
begin
  result := Length(aText){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF};
end;

function TsgcJSON.DoAdd(const aName: String; aValue: Variant;
  aType: TsgcJSONtype): IsgcObjectJSON;
var
  oJSON: TsgcObjectJSON;
begin
  inherited;
  result := Node[aName];

  if not Assigned(result) then
  begin
    oJSON := TsgcObjectJSON.Create(self);
    if EscapeStrings then
      oJSON.Name := EscapeString(aName)
    else
      oJSON.Name := aName;
    oJSON.JSONType := aType;
    if aType = sgcJSONBoolean then
      oJSON.Value := LowerCase(aValue)
    else if aType = sgcJSONNull then
      oJSON.Value := ''
    else if aType = sgcJSONObject then
      oJSON.JSONObject.Read(aValue)
    else if aType = sgcJSONList then
      oJSON.JSONObject.Read(aValue)
    else if aType = sgcJSONString then
    begin
      if EscapeStrings then
        oJSON.Value := EscapeString(aValue)
      else
        oJSON.Value := aValue
    end
    else
      oJSON.Value := aValue;
    if aType = sgcJSONString then
    begin
{$IFDEF LAZARUS}
      if (LeftStr(WideString(aValue), 1) = '{') or
        (LeftStr(WideString(aValue), 1) = '[') then
{$ELSE}
      if (LeftStr(aValue, 1) = '{') or (LeftStr(aValue, 1) = '[') then
{$ENDIF}
        oJSON.FQuoted := False;
    end;

    Data.Add(oJSON);
    result := oJSON;
  end
  else
    Node[aName].Value := aValue;
end;

function TsgcJSON.DoCreateList(var aText: String; var Pos: Integer): Boolean;
var
  oJSON: TsgcObjectJSON;
begin
  oJSON := TsgcObjectJSON.Create(self);
  oJSON.Name := FJSONName;
  oJSON.JSONType := sgcJSONList;
  result := TsgcJSON(oJSON.JSONObject).DoReadList(aText, Pos);

  Data.Add(oJSON);

  FJSONName := '';
end;

function TsgcJSON.DoCreateObject(var aText: String; var Pos: Integer): Boolean;
var
  oJSON: TsgcObjectJSON;
begin
  oJSON := TsgcObjectJSON.Create(self);
  oJSON.Name := FJSONName;
  result := TsgcJSON(oJSON.JSONObject).DoReadObject(aText, Pos);

  Data.Add(oJSON);

  FJSONName := '';
end;

procedure TsgcJSON.DoJSONEnd;
var
  oComp: TsgcObjectJSON;
begin
  if (FJSONType <> sgcJSONObject) and (FJSONType <> sgcJSONList) then
  begin
    oComp := TsgcObjectJSON.Create(self);
    oComp.Name := FJSONName;
    oComp.JSONType := FJSONType;
    oComp.Value := FJSONValue;

    Data.Add(oComp);

    FJSONName := '';
  end;
end;

function TsgcJSON.DoReadObject(var aText: String; var Pos: Integer): Boolean;
var
  vPos: Integer;
  vStart: Integer;
begin
  vPos := Pos;
  DoSpaces(aText, Pos);
  result := False;
  if ((Pos <= sgcStringEnd(aText)) and
    (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '{')) then
  begin
    inc(Pos);
    while Pos <= sgcStringEnd(aText) do
    begin
      DoSpaces(aText, Pos);
      if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '}' then
      begin
        inc(Pos);
        result := True;
        break;
      end;
      vStart := Pos;
      if not(DoReadString(aText, Pos)) then
        break;
      DoJSONStart(Copy(aText, vStart + 1, Pos - (vStart + 1) - 1));
      DoSpaces(aText, Pos);
      if not((Pos <= sgcStringEnd(aText)) and
        (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = ':')) then
        break
      else
        inc(Pos);
      DoSpaces(aText, Pos);
      if not(DoReadValue(aText, Pos)) then
        break;
      DoJSONEnd;
      DoSpaces(aText, Pos);
      if ((Pos <= sgcStringEnd(aText)) and
        (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = ',')) then
        inc(Pos);
    end;
    if not(result) then
      Pos := vPos;
  end;
end;

procedure TsgcJSON.Read(aText: String);
var
  vPos: Integer;
begin
  Clear;

  vPos := 1;

  IsArray := False;
  if LeftStr(aText, 1) = '[' then
  begin
    IsArray := True;
    DoReadList(aText, vPos);
  end
  else
    DoReadObject(aText, vPos);
end;

procedure TsgcJSON.DoSpaces(var aText: String; var Pos: Integer);
begin
  while Pos <= sgcStringEnd(aText) do
  begin
    if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '#' then
    begin
      while ((Pos <= sgcStringEnd(aText)) and
        (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] <> #10)) do
        inc(Pos)
    end
    else
    begin
      case aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] of
        ' ', #9, #12, #13, #10:
          inc(Pos);
      else
        break;
      end;
    end;
  end;
end;

function TsgcJSON.DoReadList(var aText: String; var Pos: Integer): Boolean;
var
  vPos: Integer;
  oJSON: TsgcObjectJSON;
begin
  vPos := Pos;
  DoSpaces(aText, Pos);
  result := False;
  if ((Pos <= sgcStringEnd(aText)) and
    (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '[')) then
  begin
    inc(Pos);
    while Pos <= sgcStringEnd(aText) do
    begin
      DoSpaces(aText, Pos);
      if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = ']' then
      begin
        inc(Pos);
        result := True;
        break;
      end;

      FJSONName := '';

      if not(DoReadValue(aText, Pos)) then
        break;

      if (FJSONType <> sgcJSONObject) and (FJSONType <> sgcJSONList) then
      begin
        oJSON := TsgcObjectJSON.Create(self);
        oJSON.Name := FJSONName;
        oJSON.JSONType := FJSONType;
        oJSON.Value := FJSONValue;
        Data.Add(oJSON);
      end;

      DoSpaces(aText, Pos);
      if ((Pos <= sgcStringEnd(aText)) and
        (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = ',')) then
        inc(Pos);
    end;
    if not(result) then
      Pos := vPos;
  end;
end;

function TsgcJSON.DoReadValue(var aText: String; var Pos: Integer): Boolean;
var
  vPos: Integer;
begin
  DoSpaces(aText, Pos);
  result := False;
  vPos := Pos;
  if Pos <= sgcStringEnd(aText) then
  begin
    case aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] of
      '"':
        begin
          FJSONType := sgcJSONString;
          result := DoReadString(aText, Pos);
          DoJSONValue(Copy(aText, vPos + 1, Pos - (vPos + 1) - 1));
        end;
      '+', '-', '0' .. '9':
        begin
          FJSONType := sgcJSONNumber;
          result := DoReadNumber(aText, Pos);
          DoJSONValue(Copy(aText, vPos, Pos - vPos));
        end;
      '{':
        begin
          FJSONType := sgcJSONObject;
          result := DoCreateObject(aText, Pos);
        end;
      '[':
        begin
          FJSONType := sgcJSONList;
          result := DoCreateList(aText, Pos);
        end;
      't', 'T':
        begin
          FJSONType := sgcJSONBoolean;
          result := LowerCase(Copy(String(aText), Pos, 4)) = 'true';
          DoJSONValue('true');
          Pos := Pos + 4;
        end;
      'f', 'F':
        begin
          FJSONType := sgcJSONBoolean;
          result := LowerCase(Copy(String(aText), Pos, 5)) = 'false';
          DoJSONValue('false');
          Pos := Pos + 5;
        end;
      'n', 'N':
        begin
          FJSONType := sgcJSONNull;
          result := LowerCase(Copy(String(aText), Pos, 4)) = 'null';
          Pos := Pos + 4;
          DoJSONValue('null');
        end;
    end;
  end;
end;

function TsgcJSON.DoReadString(var aText: String; var Pos: Integer): Boolean;
var
  vPos: Integer;
begin
  vPos := Pos;
  DoSpaces(aText, Pos);
  result := False;
  if Pos <= sgcStringEnd(aText) then
  begin
    if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '"' then
    begin
      inc(Pos);
      while Pos <= sgcStringEnd(aText) do
      begin
        if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '"' then
        begin
          if not((aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} - 1] = '\')
            and (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} + 1] <> '}')
            and (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} + 1] <> ']')
            and (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} + 1] <> ','))
          then
          begin
            inc(Pos);
            result := True;
            break;
          end
          else
            inc(Pos);
        end;
        if not(DoReadChar(aText, Pos)) then
          break;
      end;
      if not(result) then
        Pos := vPos;
    end
    else if UpperCase(MidStr(aText, Pos{$IFDEF SGC_ZEROBASEDSTRINGS} -
      1{$ENDIF}, 4)) = 'TRUE' then
    begin
      Pos := Pos + 4;
      result := True;
    end
    else if UpperCase(MidStr(aText, Pos{$IFDEF SGC_ZEROBASEDSTRINGS} -
      1{$ENDIF}, 5)) = 'FALSE' then
    begin
      Pos := Pos + 5;
      result := True;
    end;
  end;
end;

function TsgcJSON.DoReadChar(var aText: String; var Pos: Integer): Boolean;
var
  vPos: Integer;
begin
  vPos := Pos;
  if Pos <= sgcStringEnd(aText) then
  begin
    result := True;
    if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '\' then
    begin
      inc(Pos);
      if Pos <= sgcStringEnd(aText) then
      begin
        if aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = 'u' then
        begin
          if Pos + 4 <= sgcStringEnd(aText) then
          begin
            inc(Pos, 5);
          end
          else
          begin
            result := False;
            Pos := vPos;
          end;
        end
        else
          inc(Pos);
      end
      else
      begin
        result := False;
        Pos := vPos;
      end;
    end
    else
      inc(Pos);
  end
  else
    result := False;
end;

function TsgcJSON.DoReadInteger(var aText: String; var Pos: Integer): Boolean;
begin
  result := False;
  while Pos <= sgcStringEnd(aText) do
  begin
    if ((aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] >= '0') and
      (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] <= '9')) then
    begin
      result := True;
      inc(Pos);
    end
    else
      break;
  end;
end;

function TsgcJSON.DoReadNumber(var aText: String; var Pos: Integer): Boolean;
var
  vPos: Integer;
begin
  vPos := Pos;
  DoSpaces(aText, Pos);
  result := False;
  if Pos <= sgcStringEnd(aText) then
  begin
    if ((aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '+') or
      (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '-')) then
      inc(Pos);
    if DoReadInteger(aText, Pos) then
    begin
      if ((Pos <= sgcStringEnd(aText)) and
        (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '.')) then
      begin
        inc(Pos);
        if not(DoReadInteger(aText, Pos)) then
        begin
          Pos := vPos;
          exit;
        end;
      end;
      if ((Pos <= sgcStringEnd(aText)) and
        ((aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = 'e') or
        (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = 'E'))) then
      begin
        inc(Pos);
        if ((Pos <= sgcStringEnd(aText)) and
          ((aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '+') or
          (aText[Pos{$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF}] = '-'))) then
          inc(Pos);
        if not(DoReadInteger(aText, Pos)) then
        begin
          Pos := vPos;
          exit;
        end;
      end;
      result := True;
    end;
  end;
end;

procedure TsgcJSON.DoJSONStart(const aName: String);
begin
  FJSONName := aName;
  FJSONType := sgcJSONUndefined;
  FJSONValue := '';
end;

procedure TsgcJSON.DoJSONValue(const aValue: String);
begin
  FJSONValue := aValue;
end;

function TsgcJSON.GetText: String;
var
  i: Integer;
  oJSON: TsgcObjectJSON;
begin
  for i := 0 to Data.Count - 1 do
  begin
    if i = 0 then
    begin
      if IsArray then
        result := '['
      else
        result := '{';
    end;

    oJSON := TsgcObjectJSON(Data.Items[i]);

    result := result + GetJSONValue(oJSON, IsArray);

    if i = Data.Count - 1 then
    begin
      if IsArray then
        result := result + ']'
      else
        result := result + '}'
    end
    else if RightStr(result, 1) <> ',' then
      result := result + ',';
  end;

  if result = '' then
  begin
    if IsArray then
      result := '[]'
    else
      result := '{}';
  end;
end;

function TsgcJSON.DoGetBoolean(aObject: TsgcObjectJSON;
  const aIsArray: Boolean = False): String;
begin
  if aIsArray then
    result := String(aObject.Value)
  else
    result := '"' + aObject.Name + '":' + String(aObject.Value);
end;

function TsgcJSON.DoGetList(aObject: TsgcObjectJSON): String;
var
  i: Integer;
begin
  result := '';
  if aObject.Name <> '' then
    result := '"' + aObject.Name + '":';
  result := result + '[';
  for i := 0 to aObject.Count - 1 do
  begin
    if i > 0 then
      result := result + ',';
    result := result + GetJSONValue(TsgcObjectJSON(aObject.Item[i]), True);
  end;
  result := result + ']';
end;

function TsgcJSON.DoGetNull(aObject: TsgcObjectJSON;
  const aIsArray: Boolean = False): String;
begin
  if aIsArray then
    result := String(aObject.Value)
  else
    result := '"' + aObject.Name + '":null';
end;

function TsgcJSON.DoGetNumber(aObject: TsgcObjectJSON;
  const aIsArray: Boolean = False): String;
begin
  if aIsArray then
    result := sgcStringReplace(String(aObject.Value), ',', '.')
  else
    result := '"' + aObject.Name + '":' + sgcStringReplace
      (String(aObject.Value), ',', '.');
end;

function TsgcJSON.DoGetObject(aObject: TsgcObjectJSON;
  const aIsArray: Boolean = False): String;
begin
  if aIsArray then
  begin
    if aObject.JSONObject.Text <> '' then
      result := aObject.JSONObject.Text;
  end
  else
  begin
    if aObject.JSONObject.Text <> '' then
      result := '"' + aObject.Name + '":' + aObject.JSONObject.Text
    else
      result := '"' + aObject.Name + '":""';
  end;
end;

function TsgcJSON.DoGetString(aObject: TsgcObjectJSON;
  const aIsArray: Boolean = False): String;
var
  vValue: String;
begin
  vValue := String(aObject.Value);
  // if not ((LeftStr(vValue, 1) = '{') or (LeftStr(vValue, 1) = '[') or (LeftStr(vValue, 1) = '"')) then
  if (LeftStr(vValue, 1) <> '"') and (aObject.FQuoted = True) then
    // if not (LeftStr(vValue, 1) = '"') then
    vValue := '"' + vValue + '"';

  if aIsArray then
    result := vValue
  else
    result := '"' + aObject.Name + '":' + vValue;
end;

function TsgcJSON.GetJSONValue(const aJSON: TsgcObjectJSON;
  aIsArray: Boolean = False): string;
begin
  result := '';
  case aJSON.JSONType of
    sgcJSONString:
      result := DoGetString(aJSON, aIsArray);
    sgcJSONNumber:
      result := DoGetNumber(aJSON, aIsArray);
    sgcJSONBoolean:
      result := DoGetBoolean(aJSON, aIsArray);
    sgcJSONNull:
      result := DoGetNull(aJSON, aIsArray);
    sgcJSONList:
      result := DoGetList(aJSON);
    sgcJSONObject:
      result := DoGetObject(aJSON, aIsArray);
  end;
end;

function TsgcObjectJSONList.GetNode(const aName: String): TsgcObjectJSON;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if TsgcObjectJSON(Items[i]).Name = aName then
    begin
      result := TsgcObjectJSON(Items[i]);
      break;
    end;
  end;
end;

function TsgcObjectJSONList.GetItem(i: Integer): TsgcObjectJSON;
begin
  if i < Count then
    result := TsgcObjectJSON(Items[i])
  else
    result := nil;
end;

procedure TsgcObjectJSONList.SetItem(i: Integer; const aValue: TsgcObjectJSON);
begin
  if i < Count then
    Items[i] := TsgcObjectJSON(aValue);
end;

procedure TsgcObjectJSONList.SetNode(const aName: String;
  const aValue: TsgcObjectJSON);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if TsgcObjectJSON(Items[i]).Name = aName then
    begin
      Items[i] := TsgcObjectJSON(aValue);
      break;
    end;
  end;
end;

destructor TsgcObjectJSONList.Destroy;
var
  i: Integer;
begin
  if not OwnsObjects then
  begin
    for i := Count - 1 Downto 0 do
    begin
{$IFDEF NEXTGEN}
      Item[i].DisposeOf;
{$ELSE}
      Item[i].Free;
{$ENDIF}
    end;
  end;
  inherited;
end;

constructor TsgcObjectJSON.Create(aOwner: TComponent);
begin
  inherited;
  FQuoted := True;
end;

procedure TsgcObjectJSON.Clear;
begin
  FJSONObject := nil;
end;

function TsgcObjectJSON.GetCount: Integer;
begin
  result := 0;
  if Assigned(FJSONObject) then
    result := JSONObject.Count;
end;

function TsgcObjectJSON.GetItem(const i: Integer): IsgcObjectJSON;
begin
  result := JSONObject.GetItem(i);
end;

function TsgcObjectJSON.GetJSONObject: IsgcJSON;
begin
  if not Assigned(FJSONObject) then
  begin
    FJSONObject := TsgcJSON.Create(self);
    if JSONType <> sgcJSONList then
      JSONType := sgcJSONObject;
  end;
  result := FJSONObject;
end;

function TsgcObjectJSON.GetJSONType: TsgcJSONtype;
begin
  result := FJSONType;
end;

function TsgcObjectJSON.GetName: String;
begin
  result := FName;
end;

function TsgcObjectJSON.GetNode(const aName: String): IsgcObjectJSON;
begin
  result := JSONObject.GetNode(aName);
end;

function TsgcObjectJSON.GetValue: Variant;
begin
  if JSONType = sgcJSONObject then
    result := JSONObject.Text
  else if JSONType = sgcJSONString then
    result := FValue
  else if JSONType = sgcJSONList then
  begin
    JSONObject.IsArray := True;
    result := JSONObject.Text;
  end
  else
    result := FValue;
end;

procedure TsgcObjectJSON.SetItem(const i: Integer;
  const aValue: IsgcObjectJSON);
begin
  JSONObject.SetItem(i, aValue);
end;

procedure TsgcObjectJSON.SetJSONObject(const Value: IsgcJSON);
begin
  FJSONObject := Value;
end;

procedure TsgcObjectJSON.SetJSONType(const Value: TsgcJSONtype);
begin
  FJSONType := Value;
end;

procedure TsgcObjectJSON.SetName_(const Value: String);
begin
  FName := Value;
end;

procedure TsgcObjectJSON.SetNode(const aName: String;
  const aValue: IsgcObjectJSON);
begin
  JSONObject.SetNode(aName, aValue);
end;

procedure TsgcObjectJSON.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

constructor TsgcJSON.Create(aOwner: TComponent);
begin
  inherited;
  IsArray := False;
end;

destructor TsgcJSON.Destroy;
begin
  Clear;
  inherited;
end;

function TsgcJSON.AddPair(const aName: String; const aValue: Boolean)
  : IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONBoolean);
end;

function TsgcJSON.AddPair(const aName: String; const aValue: Double)
  : IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcJSON.AddPair(const aName: String; const aValue: Integer)
  : IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcJSON.AddPair(const aName, aValue: String): IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONString);
end;

function TsgcJSON.AddArray(const aName: string; const aValue: string = '')
  : IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONList);
end;

function TsgcJSON.AddObject(const aName: string; const aValue: string = '')
  : IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONObject);
end;

function TsgcJSON.AddPair(const aName: String; const aValue: Int64)
  : IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcJSON.AddPair(const aName: String): IsgcObjectJSON;
begin
  result := DoAdd(aName, '', sgcJSONNull);
end;

procedure TsgcJSON.Clear;
begin
  sgcFree(FData);
end;

function TsgcJSON.GetCount: Integer;
begin
  result := Data.Count;
end;

function TsgcJSON.GetData: TsgcObjectJSONList;
begin
  if not Assigned(FData) then
  begin
    FData := TsgcObjectJSONList.Create;
    FData.OwnsObjects := False;
  end;
  result := FData;
end;

function TsgcJSON.GetItem(i: Integer): IsgcObjectJSON;
begin
  result := Data.GetItem(i);
end;

function TsgcJSON.GetIsArray: Boolean;
begin
  result := FIsArray;
end;

function TsgcJSON.GetNode(const aName: String): IsgcObjectJSON;
begin
  result := Data.GetNode(aName);
end;

procedure TsgcJSON.SetItem(i: Integer; const aValue: IsgcObjectJSON);
begin
  Data.SetItem(i, TsgcObjectJSON(aValue));
end;

procedure TsgcJSON.SetIsArray(const Value: Boolean);
begin
  FIsArray := Value;
end;

procedure TsgcJSON.SetNode(const aName: String; const aValue: IsgcObjectJSON);
begin
  Data.SetNode(aName, TsgcObjectJSON(aValue));
end;

function TsgcJSON.EscapeString(const aValue: string): string;

  procedure AddChars(const aChars: string; var Dest: string;
    var AIndex: Integer);
  begin
    Insert(aChars, Dest, AIndex);
    Delete(Dest, AIndex + 2, 1);
    inc(AIndex, 2);
  end;

  procedure AddUnicodeChars(const aChars: string; var Dest: string;
    var AIndex: Integer); 
  begin
    Insert(aChars, Dest, AIndex);
    Delete(Dest, AIndex + 6, 1);
    inc(AIndex, 6);
  end;

var
  i, j: Integer;
  vChar: Char;
begin
  result := aValue;
  j := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF};
  for i := {$IFDEF SGC_ZEROBASEDSTRINGS}0{$ELSE}1{$ENDIF} to Length
    (aValue){$IFDEF SGC_ZEROBASEDSTRINGS} - 1{$ENDIF} do
  begin
    vChar := aValue[i];
    case vChar of
      '/', '\', '"':
        begin
          Insert('\', result, j);
          inc(j, 2);
        end;
      #8:
        begin
          AddChars('\b', result, j);
        end;
      #9:
        begin
          AddChars('\t', result, j);
        end;
      #10:
        begin
          AddChars('\n', result, j);
        end;
      #12:
        begin
          AddChars('\f', result, j);
        end;
      #13:
        begin
          AddChars('\r', result, j);
        end;
      #0 .. #7, #11, #14 .. #31:
        begin
          AddUnicodeChars('\u' + IntToHex(Word(vChar), 4), result, j);
        end
    else
      begin
        if Word(vChar) > 127 then
          AddUnicodeChars('\u' + IntToHex(Word(vChar), 4), result, j)
        else
          inc(j);
      end;
    end;
  end;
end;

{$IFDEF SGC_JSON_INTF}

procedure TInterfacedComponent.BeforeDestruction;
begin
  if FRefCount = 0 then
    inherited;
end;

destructor TInterfacedComponent.Destroy;
begin
  if FRefCount = 0 then
    inherited
  else
    FDestroyObject := True;
end;

procedure TInterfacedComponent.FreeInstance;
begin
  if FRefCount = 0 then
    inherited;
end;

function TInterfacedComponent._AddRef: Integer;
begin
  result := -1;
  inc(FRefCount);
end;

function TInterfacedComponent._Release: Integer;
begin
  result := -1;
  Dec(FRefCount);
  if (FRefCount = 0) and FDestroyObject then
{$IFNDEF NEXTGEN}Free; {$ENDIF}
end;
{$ENDIF}

initialization

oJSONList := TThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF}.Create;

finalization

sgcFree(oJSONList);

end.
