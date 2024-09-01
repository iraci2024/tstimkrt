{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com


 XSuperObject is a library written by Onur YILDIZ, sources:
   https://github.com/onryldz/x-superobject
***************************************************************************}


unit sgcJSON_XSuperObject;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
  sgcJSON,
  XSuperJSON, XSuperObject,
  {$IFDEF NEXTGEN} System.Generics.Collections{$ELSE}Contnrs{$ENDIF};

type

  TsgcObjectXSOJSON = class(TInterfacedComponent, IsgcObjectJSON)
  private
    FJSONValue: ISuperObject;
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
    property Node[const aName: String]: IsgcObjectJSON read GetNode write SetNode;
  { sgcJSONObject }

  private
    FIsArray: Boolean;
  protected
    property IsArray: Boolean read FIsArray write FIsArray;
  public
    property Count: Integer read GetCount;
    property JSONType: TsgcJSONtype read GetJSONType write SetJSONType;
    property Name: String read GetName write SetName_;
    property Value: Variant read GetValue write SetValue;
  end;

  TsgcObjectXSOJSONList = class({$IFDEF NEXTGEN}<TsgcObjectXSOJSON>{$ELSE}TObjectList{$ENDIF})
  protected
    function GetNode(const aName: String): TsgcObjectXSOJSON;
    function GetItem(i: Integer): TsgcObjectXSOJSON;
    procedure SetNode(const aName: String; const aValue: TsgcObjectXSOJSON);
    procedure SetItem(i: Integer; const aValue: TsgcObjectXSOJSON);
  public
    property Node[const aName: String]: TsgcObjectXSOJSON read GetNode write
        SetNode;
    property Item[i: Integer]: TsgcObjectXSOJSON read GetItem write SetItem;
  {$IFDEF NEXTGEN}
  private
    FOwnsObjects: Boolean;
  public
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  {$ENDIF}
  end;



  TsgcXSOJSON = class(TInterfacedComponent, IsgcJSON)
    { json items }
  private
    FIsArray: Boolean;
  protected
    function GetCount: Integer; virtual;
    function GetNode(const aName: String): IsgcObjectJSON; virtual;
    procedure SetNode(const aName: String; const aValue: IsgcObjectJSON); virtual;
    function GetItem(i: integer): IsgcObjectJSON; virtual;
    procedure SetItem(i: integer; const aValue: IsgcObjectJSON); virtual;
    function GetIsArray: Boolean; virtual;
    procedure SetIsArray(const Value: Boolean); virtual;
  public
    property Node[const aName: String]: IsgcObjectJSON read GetNode write SetNode;
    property Item[i: integer]: IsgcObjectJSON read GetItem write SetItem;
    property Count: Integer read GetCount;
    property IsArray: Boolean read GetIsArray write SetIsArray;
    { json items }

    { json objects }
  protected
    function DoAdd(const aName: String; aValue: Variant; aType: TsgcJSONtype): TsgcObjectXSOJSON;
  public
    function AddPair(const aName, aValue: String): IsgcObjectJSON; overload;
    function AddPair(const aName: String; const aValue: Integer): IsgcObjectJSON;
        overload;
    function AddPair(const aName: String; const aValue: Int64): IsgcObjectJSON;
        overload;
    function AddPair(const aName: String; const aValue: Double): IsgcObjectJSON;
        overload;
    function AddPair(const aName: String; const aValue: Boolean): IsgcObjectJSON;
        overload;
    function AddObject(const aName: String; const aValue: string = ''):
        IsgcObjectJSON;
    function AddArray(const aName: String; const aValue: string = ''):
        IsgcObjectJSON;
    { json objects }

    { data }
  private
    FData: TsgcObjectXSOJSONList;
    function GetData: TsgcObjectXSOJSONList;
  protected
    property Data: TsgcObjectXSOJSONList read GetData write FData;
  public
    procedure Clear;
    { data }

  { parse }
  private
    FJSONObject: ISuperObject;
    function GetJSONObject: ISuperObject;
  protected
    property JSONObject: ISuperObject read GetJSONObject;
  protected
    function GetText: String;
  protected
    procedure DoReadArray(aText: String); virtual;
    procedure DoReadObject(aText: String); virtual;
  public
    procedure Read(aText: String);
    property Text: String read GetText;
  { parse }

  { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor }
  end;



implementation

uses sgcWebSocket_Helpers;

function TsgcObjectXSOJSONList.GetNode(const aName: String):
    TsgcObjectXSOJSON;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if TsgcObjectXSOJSON(Items[i]).Name = aName then
    begin
      result := TsgcObjectXSOJSON(Items[i]);
      break;
    end;
  end;
end;

function TsgcObjectXSOJSONList.GetItem(i: Integer): TsgcObjectXSOJSON;
begin
  if i < Count then
    Result := TsgcObjectXSOJSON(Items[i])
  else
    Result := nil;
end;

procedure TsgcObjectXSOJSONList.SetItem(i: Integer; const aValue:
    TsgcObjectXSOJSON);
begin
  if i < Count then
    Items[i] := TsgcObjectXSOJSON(aValue);
end;

procedure TsgcObjectXSOJSONList.SetNode(const aName: String; const aValue:
    TsgcObjectXSOJSON);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if TsgcObjectXSOJSON(Items[i]).Name = aName then
    begin
      Items[i] := TsgcObjectXSOJSON(aValue);
      break;
    end;
  end;
end;

function TsgcXSOJSON.AddObject(const aName: String; const aValue: string = ''):
    IsgcObjectJSON;
begin
  if aValue = '' then
    result := DoAdd(aName, '{}', sgcJSONObject)
  else
    result := DoAdd(aName, aValue, sgcJSONObject);
end;

procedure TsgcXSOJSON.Clear;
begin
  inherited;
  sgcFree(FData);
  FJSONObject := nil;
end;

constructor TsgcXSOJSON.Create(aOwner: TComponent);
begin
  inherited;
  IsArray := False;
end;

destructor TsgcXSOJSON.Destroy;
begin
  Clear;
  inherited;
end;

function TsgcXSOJSON.AddArray(const aName: String; const aValue: string = ''):
    IsgcObjectJSON;
begin
  if aValue = '' then
    result := DoAdd(aName, '[]', sgcJSONList)
  else
    result := DoAdd(aName, aValue, sgcJSONList);
end;

function TsgcXSOJSON.AddPair(const aName, aValue: String): IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONString);
end;

function TsgcXSOJSON.AddPair(const aName: String; const aValue: Integer):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcXSOJSON.AddPair(const aName: String; const aValue: Double):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcXSOJSON.AddPair(const aName: String; const aValue: Boolean):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONBoolean);
end;

function TsgcXSOJSON.AddPair(const aName: String; const aValue: Int64):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcXSOJSON.DoAdd(const aName: String; aValue: Variant; aType:
    TsgcJSONtype): TsgcObjectXSOJSON;
var
  oJSON: TsgcObjectXSOJSON;
begin
  result := Data.Node[aName];
  if not Assigned(result) then
  begin
    if IsArray then
      case aType of
        sgcJSONUndefined: JSONObject.AsArray.V[StrToInt(aName)] := aValue;
        sgcJSONObject: JSONObject.AsArray.O[StrToInt(aName)] := SO(aValue);
        sgcJSONList: JSONObject.AsArray.O[StrToInt(aName)] := SO(aValue);
        sgcJSONString: JSONObject.AsArray.S[StrToInt(aName)] := aValue;
        sgcJSONNumber: JSONObject.AsArray.F[StrToInt(aName)] := aValue;
        sgcJSONBoolean: JSONObject.AsArray.B[StrToInt(aName)] := aValue;
        sgcJSONNull: JSONObject.AsArray.Null[StrToInt(aName)] := aValue;
      end
    else
      case aType of
        sgcJSONUndefined: JSONObject.V[aName] := aValue;
        sgcJSONObject: JSONObject.O[aName] := SO(aValue);
        sgcJSONList: JSONObject.V[aName] := SA(aValue);
        sgcJSONString: JSONObject.S[aName] := aValue;
        sgcJSONNumber: JSONObject.F[aName] := aValue;
        sgcJSONBoolean: JSONObject.B[aName] := aValue;
        sgcJSONNull: JSONObject.Null[aName] := aValue;
      end;
    oJSON := TsgcObjectXSOJSON.Create(self);
    oJSON.FJSONValue := JSONObject;
    oJSON.Name := aName;
    oJSON.JSONType := aType;
    oJSON.IsArray := IsArray;

    Data.Add(oJSON);
    result := oJSON;
  end
  else
    Data.Node[aName].Value := aValue;
end;

procedure TsgcXSOJSON.DoReadArray(aText: String);
var
  i: integer;
  oMember: IMember;
begin
  FJSONObject := SO(aText);

  if Assigned(FJSONObject) then
  begin
    if FJSONObject.DataType = dtArray then
    begin
      i := 0;
      for oMember in FJSONObject.AsArray do
      begin
        case oMember.DataType of
          dtObject:
            AddObject(IntToStr(i), oMember.ToString);
          dtArray:
            AddArray(IntToStr(i), oMember.ToString);
          else
            AddPair(IntToStr(i), oMember.AsString);
        end;
        inc(i);
      end;
    end;
  end;
end;

procedure TsgcXSOJSON.DoReadObject(aText: String);
var
  i: integer;
  vName : String;
begin
  FJSONObject := SO(aText);
  if Assigned(FJSONObject) then
  begin
    if FJSONObject.DataType = dtObject then
    begin
      FJSONObject.First;
      for i := 0 to FJSONObject.Count - 1 do
      begin
        vName := FJSONObject.GetCurrentKey;
        case FJSONObject[vName].DataType of
          dtObject:
            AddObject(vName).JSONObject.Read(FJSONObject[vName].ToString);
          else
            AddPair(vName, FJSONObject[vName].AsString);
        end;
        FJSONObject.Next;
      end;
    end;
  end;
end;

function TsgcXSOJSON.GetCount: Integer;
begin
  Result := Data.Count;
end;

function TsgcXSOJSON.GetData: TsgcObjectXSOJSONList;
begin
  if not Assigned(FData) then
  begin
    FData := TsgcObjectXSOJSONList.Create;
    FData.OwnsObjects := False;
  end;
  Result := FData;
end;

function TsgcXSOJSON.GetItem(i: integer): IsgcObjectJSON;
begin
  result := Data.GetItem(i);
end;


function TsgcXSOJSON.GetJSONObject: ISuperObject;
begin
  if not Assigned(FJSONObject) then
    FJSONObject := SO;
  result := FJSONObject;
end;

function TsgcXSOJSON.GetIsArray: Boolean;
begin
  Result := FIsArray;
end;


function TsgcXSOJSON.GetNode(const aName: String): IsgcObjectJSON;
begin
  result := Data.GetNode(aName);
end;

function TsgcXSOJSON.GetText: String;
begin
  Result := JSONObject.AsJSON;
end;

procedure TsgcXSOJSON.Read(aText: String);
begin
  Clear;

  if LeftStr(aText, 1) = '[' then
  begin
    IsArray := True;
    DoReadArray(aText);
  end
  else
    DoReadObject(aText);
end;

procedure TsgcXSOJSON.SetItem(i: integer; const aValue: IsgcObjectJSON);
begin
  Data.GetItem(i).Value := aValue;
end;

procedure TsgcXSOJSON.SetIsArray(const Value: Boolean);
begin
  FIsArray := Value;
end;

procedure TsgcXSOJSON.SetNode(const aName: String; const aValue:
    IsgcObjectJSON);
begin
  Data.GetNode(aName).Value := aValue;
end;

procedure TsgcObjectXSOJSON.Clear;
begin
  IsArray := False;
  FJSONObject := nil;
end;

function TsgcObjectXSOJSON.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FJSONObject) then
    result := JSONObject.Count;
end;

function TsgcObjectXSOJSON.GetItem(const i: Integer): IsgcObjectJSON;
begin
  result := JSONObject.GetItem(i);
end;

function TsgcObjectXSOJSON.GetJSONObject: IsgcJSON;
begin
  if not Assigned(FJSONObject) then
  begin
    FJSONObject := TsgcXSOJSON.Create(self);
    if JSONType = sgcJSONList then
      TsgcXSOJSON(FJSONObject).FJSONObject := FJSONValue.AsArray.O[StrToInt(Name)]
    else
    begin
      JSONType := sgcJSONObject;
      if Assigned(FJSONValue) then
      begin
        case FJSONValue.Ancestor[Name].DataType of
          dtObject:
            TsgcXSOJSON(FJSONObject).FJSONObject := FJSONValue.O[Name]
          else
            TsgcXSOJSON(FJSONObject).FJSONObject := nil;
        end;
      end;
    end;
  end;
  Result := FJSONObject;
end;

function TsgcObjectXSOJSON.GetJSONType: TsgcJSONtype;
begin
  Result := FJSONtype;
end;

function TsgcObjectXSOJSON.GetName: String;
begin
  Result := FName;
end;

function TsgcObjectXSOJSON.GetNode(const aName: String): IsgcObjectJSON;
begin
  result := JSONObject.GetNode(aName);
end;

function TsgcObjectXSOJSON.GetValue: Variant;
begin
  if IsArray then
    case FJSONValue.AsArray.Ancestor[StrToInt(Name)].DataType of
      dtObject:
        Result := FJSONValue.AsArray.O[StrToInt(Name)].AsJSON;
      dtArray:
        Result := FJSONValue.AsArray.A[StrToInt(Name)].AsJSON;
      dtString:
        Result := FJSONValue.AsArray.S[StrToInt(Name)];
      else
        Result := FJSONValue.AsArray.V[StrToInt(Name)];
    end
  else
    case FJSONValue[Name].DataType of
      dtObject:
        Result := FJSONValue[Name].AsObject.AsJSON;
      dtArray:
        Result := FJSONValue[Name].AsArray.AsJSON;
      dtString:
        Result := FJSONValue[Name].AsString;
      dtNil:
        Result := '';
      else
        Result := FJSONValue[Name].AsVariant;
    end;
end;

procedure TsgcObjectXSOJSON.SetItem(const i: Integer; const aValue:
    IsgcObjectJSON);
begin
  JSONObject.SetItem(i, TsgcObjectXSOJSON(aValue));
end;

procedure TsgcObjectXSOJSON.SetJSONObject(const Value: IsgcJSON);
begin
  FJSONObject := Value;
end;

procedure TsgcObjectXSOJSON.SetJSONType(const Value: TsgcJSONtype);
begin
  FJSONtype := Value;
end;

procedure TsgcObjectXSOJSON.SetName_(const Value: String);
begin
  FName := Value;
end;

procedure TsgcObjectXSOJSON.SetNode(const aName: String; const aValue:
    IsgcObjectJSON);
begin
  JSONObject.SetNode(aName, TsgcObjectXSOJSON(aValue));
end;

procedure TsgcObjectXSOJSON.SetValue(const Value: Variant);
begin
  if IsArray then
  case FJSONValue.AsArray.Ancestor[StrToInt(Name)].DataType of
    dtObject:
      FJSONValue.AsArray.O[StrToInt(Name)] := SO(Value);
    dtArray:
      FJSONValue.AsArray.A[StrToInt(Name)] := SA(Value);
    dtString:
      FJSONValue.AsArray.S[StrToInt(Name)] := Value;
    else
      FJSONValue.AsArray.V[StrToInt(Name)] := Value;
  end
  else
    FJSONValue[Name].AsVariant := Value;
end;

end.
