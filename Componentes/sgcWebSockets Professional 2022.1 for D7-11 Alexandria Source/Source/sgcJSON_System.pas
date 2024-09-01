{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}


unit sgcJSON_System;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
  sgcJSON,
  System.JSON,
  {$IFDEF NEXTGEN} System.Generics.Collections{$ELSE}Contnrs{$ENDIF};

type

  TsgcObjectSystemJSON = class(TInterfacedComponent, IsgcObjectJSON)
  private
    FJSONValue: TObject;
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
    FIsObject: Boolean;
  protected
    property IsArray: Boolean read FIsArray write FIsArray;
    property IsObject: Boolean read FIsObject write FIsObject;
  public
    property Count: Integer read GetCount;
    property JSONType: TsgcJSONtype read GetJSONType write SetJSONType;
    property Name: String read GetName write SetName_;
    property Value: Variant read GetValue write SetValue;
  end;

  TsgcObjectSystemJSONList = class({$IFDEF NEXTGEN}<TsgcObjectSystemJSON>{$ELSE}TObjectList{$ENDIF})
  protected
    function GetNode(const aName: String): TsgcObjectSystemJSON;
    function GetItem(i: Integer): TsgcObjectSystemJSON;
    procedure SetNode(const aName: String; const aValue: TsgcObjectSystemJSON);
    procedure SetItem(i: Integer; const aValue: TsgcObjectSystemJSON);
  public
    property Node[const aName: String]: TsgcObjectSystemJSON read GetNode write
        SetNode;
    property Item[i: Integer]: TsgcObjectSystemJSON read GetItem write SetItem;
  {$IFDEF NEXTGEN}
  private
    FOwnsObjects: Boolean;
  public
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  {$ENDIF}
  end;



  TsgcSystemJSON = class(TInterfacedComponent, IsgcJSON)
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
    function DoAdd(const aName: String; aValue: Variant; aType: TsgcJSONtype): TsgcObjectSystemJSON;
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
        IsgcObjectJSON; overload;
    function AddArray(const aName: String; const aValue: string = ''):
        IsgcObjectJSON; overload;
    { json objects }

    { data }
  private
    FData: TsgcObjectSystemJSONList;
    function GetData: TsgcObjectSystemJSONList;
  protected
    property Data: TsgcObjectSystemJSONList read GetData write FData;
  public
    procedure Clear;
    { data }

  { parse }
  private
    FJSONValue: TJSONValue;
    FJSONName: String;
    FJSONObject: TObject;
  protected
    function GetJSONValue: TJSONValue;
    function GetJSONObject: TObject;
    property JSONValue: TJSONValue read GetJSONValue;
    property JSONObject: TObject read GetJSONObject;
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

function RemoveDoubleQuotes(const aText: String): String;
begin
  result := aText;
  if LeftStr(result, 1) = '"' then
    result := MidStr(result, 2, Length(result));
  if RightStr(aText, 1) = '"' then
    result := MidStr(result, 1, Length(result) - 1);
end;


function GetJSONPairByName(const aJSONObject: TJSONObject; const aName: String): TJSONPair;
var
  i: integer;
begin
  result := nil;
  for i := 0 to aJSONObject.Count - 1 do
  begin
    if RemoveDoubleQuotes(aJSONObject.Pairs[i].JsonString.ToString) = aName then
    begin
      result := aJSONObject.Pairs[i];
      break;
    end;
  end;
end;


function TsgcObjectSystemJSONList.GetNode(const aName: String):
    TsgcObjectSystemJSON;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if TsgcObjectSystemJSON(Items[i]).Name = aName then
    begin
      result := TsgcObjectSystemJSON(Items[i]);
      break;
    end;
  end;
end;

function TsgcObjectSystemJSONList.GetItem(i: Integer): TsgcObjectSystemJSON;
begin
  if i < Count then
    Result := TsgcObjectSystemJSON(Items[i])
  else
    Result := nil;
end;

procedure TsgcObjectSystemJSONList.SetItem(i: Integer; const aValue:
    TsgcObjectSystemJSON);
begin
  if i < Count then
    Items[i] := TsgcObjectSystemJSON(aValue);
end;

procedure TsgcObjectSystemJSONList.SetNode(const aName: String; const aValue:
    TsgcObjectSystemJSON);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if TsgcObjectSystemJSON(Items[i]).Name = aName then
    begin
      Items[i] := TsgcObjectSystemJSON(aValue);
      break;
    end;
  end;
end;

function TsgcSystemJSON.AddObject(const aName: String; const aValue: string =
    ''): IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONObject)
end;

procedure TsgcSystemJSON.Clear;
begin
  inherited;
  sgcFree(FData);
  sgcFree(FJSONValue);
  FJSONObject := nil;
end;

constructor TsgcSystemJSON.Create(aOwner: TComponent);
begin
  inherited;
  IsArray := False;
end;

destructor TsgcSystemJSON.Destroy;
begin
  Clear;
  inherited;
end;

function TsgcSystemJSON.AddArray(const aName: String; const aValue: string =
    ''): IsgcObjectJSON;
begin
  result := DoAdd(aName, aValue, sgcJSONList)
end;

function TsgcSystemJSON.AddPair(const aName, aValue: String): IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONString);
end;

function TsgcSystemJSON.AddPair(const aName: String; const aValue: Integer):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcSystemJSON.AddPair(const aName: String; const aValue: Double):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcSystemJSON.AddPair(const aName: String; const aValue: Boolean):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONBoolean);
end;

function TsgcSystemJSON.AddPair(const aName: String; const aValue: Int64):
    IsgcObjectJSON;
begin
  DoAdd(aName, aValue, sgcJSONNumber);
end;

function TsgcSystemJSON.DoAdd(const aName: String; aValue: Variant; aType:
    TsgcJSONtype): TsgcObjectSystemJSON;
var
  oJSON: TsgcObjectSystemJSON;
  oJSONObject: TJSONObject;
begin
  oJSONObject := nil;

  result := Data.Node[aName];
  if not Assigned(result) then
  begin
    if GetJSONPairByName(TJSONObject(JSONObject), aName) = nil then
    begin
      oJSONObject := nil;
      if IsArray then
        TJSONArray(JSONObject).Add(String(aValue))
      else
      begin
        case aType of
          sgcJSONObject:
            begin
              if aValue = '' then
              begin
                oJSONObject := TJSONObject.Create;
                TJSONObject(JSONObject).AddPair(aName, oJSONObject);
              end
              else
                TJSONObject(JSONObject).AddPair(aName, TJSONObject.ParseJSONValue(aValue));
            end;
          sgcJSONList:
            begin
              if aValue = '' then
              begin
                oJSONObject := TJSONObject(TJSONArray.Create);
                TJSONObject(JSONObject).AddPair(aName, oJSONObject);
              end
              else
                TJSONObject(JSONObject).AddPair(aName, TJSONArray(TJSONObject.ParseJSONValue(aValue)));
            end
          else
            TJSONObject(JSONObject).AddPair(aName, aValue);
        end;
      end;
    end;

    oJSON := TsgcObjectSystemJSON.Create(self);
    if Assigned(oJSONObject) then
    begin
      oJSON.FJSONValue := oJSONObject;
      oJSON.IsObject := True;
    end
    else
    begin
      oJSON.FJSONValue := JSONObject;
      oJSON.IsObject := False;
    end;
    oJSON.Name := aName;
    oJSON.JSONType := aType;
    oJSON.IsArray := IsArray;

    Data.Add(oJSON);
    result := oJSON;
  end
  else
    Data.Node[aName].Value := aValue;
end;

procedure TsgcSystemJSON.DoReadArray(aText: String);
var
  i: integer;
  oJSONValue: TJSONValue;
  oObject: TsgcObjectSystemJSON;
begin
  for i := 0 to TJSONArray(FJSONObject).Count - 1 do
  begin
    oJSONValue := TJSONArray(FJSONObject).Items[i];
    if Assigned(oJSONValue) then
    begin
      oObject := TsgcObjectSystemJSON.Create(self);
      oObject.FJSONValue := JSONObject;

      if oJSONValue.ClassType = TJSONTrue then
        oObject.JSONType := sgcJSONBoolean
      else if oJSONValue.ClassType = TJSONNull then
        oObject.JSONType := sgcJSONNULL
      else if oJSONValue.ClassType = TJSONObject then
        oObject.JSONType := sgcJSONObject
      else if oJSONValue.ClassType = TJSONArray then
        oObject.JSONType := sgcJSONList
      else if oJSONValue.ClassType = TJSONNumber then
        oObject.JSONType := sgcJSONNumber
      else
        oObject.JSONType := sgcJSONString;

      oObject.Name := IntToStr(i);
      oObject.Value := oJSONValue.ToString;
      oObject.IsArray := True;

      Data.Add(oObject)
    end;
  end;
end;

procedure TsgcSystemJSON.DoReadObject(aText: String);
var
  i: integer;
  oJSONValue: TJSONPair;
  vName: String;
begin
  for i := 0 to TJSONObject(FJSONObject).Count - 1 do
  begin
    oJSONValue := TJSONObject(FJSONObject).Pairs[i];
    if Assigned(oJSONValue) then
    begin
      vName := RemoveDoubleQuotes(oJSONValue.JsonString.ToString);

      if oJSONValue.JsonValue.ClassType = TJSONTrue then
      begin
        if oJSONValue.JsonValue.ToString = 'true' then
          AddPair(vName, True)
        else
          AddPair(vName, False);
      end
      else if oJSONValue.JsonValue.ClassType = TJSONNull then
        AddPair(vName, 'null')
      else if oJSONValue.JsonValue.ClassType = TJSONObject then
        AddObject(vName).JSONObject.Read(oJSONValue.JsonValue.ToJSON)
      else if oJSONValue.JsonValue.ClassType = TJSONArray then
        AddArray(vName, oJSONValue.JsonValue.ToJSON)
      else if oJSONValue.JsonValue.ClassType = TJSONNumber then
        AddPair(vName, StrToFloat(oJSONValue.JsonValue.ToString))
      else
        AddPair(vName, oJSONValue.JsonValue.ToString);
    end;
  end;
end;

function TsgcSystemJSON.GetCount: Integer;
begin
  Result := Data.Count;
end;

function TsgcSystemJSON.GetData: TsgcObjectSystemJSONList;
begin
  if not Assigned(FData) then
  begin
    FData := TsgcObjectSystemJSONList.Create;
    FData.OwnsObjects := False;
  end;
  Result := FData;
end;

function TsgcSystemJSON.GetItem(i: integer): IsgcObjectJSON;
begin
  result := Data.GetItem(i);
end;


function TsgcSystemJSON.GetJSONObject: TObject;
begin
  if not Assigned(FJSONObject) then
  begin
    if IsArray then
      FJSONObject := TJSONArray.Create
    else
      FJSONObject := TJSONObject.create;
  end;
  result := FJSONObject;
end;

function TsgcSystemJSON.GetJSONValue: TJSONValue;
begin
  if not Assigned(FJSONValue) then
    FJSONValue := TJSONValue.create;
  result := FJSONValue;
end;

function TsgcSystemJSON.GetIsArray: Boolean;
begin
  Result := FIsArray;
end;

function TsgcSystemJSON.GetNode(const aName: String): IsgcObjectJSON;
begin
  result := Data.GetNode(aName);
end;

function TsgcSystemJSON.GetText: String;
var
  oJSONPair: TJSONPair;
begin
  if IsArray then
    Result := TJSONArray(JSONObject).ToString
  else
  begin
    if FJSONName <> '' then
    begin
      oJSONPair := GetJSONPairByName(TJSONObject(JSONObject), FJSONName);
      if Assigned(oJSONPair) then
      begin
        if oJSONPair.JsonValue.ClassType = TJSONObject then
          result := oJSONPair.JsonValue.ToJSON
        else if oJSONPair.JsonValue.ClassType = TJSONArray then
          result := oJSONPair.JsonValue.ToJSON
        else
          result := oJSONPair.JsonValue.Value;
      end
      else
        Result := TJSONObject(JSONObject).ToString;
    end
    else
      Result := TJSONObject(JSONObject).ToString
  end;
end;

procedure TsgcSystemJSON.Read(aText: String);
begin
  Clear;

  FJSONObject := TJSONObject.ParseJSONValue(aText);

  if Assigned(FJSONObject) then
  begin
    if FJSONObject.ClassType = TJSONArray then
    begin
      IsArray := True;
      DoReadArray(aText)
    end
    else if FJSONObject.ClassType = TJSONObject then
    begin
      IsArray := False;
      DoReadObject(aText)
    end;
  end;
end;


procedure TsgcSystemJSON.SetItem(i: integer; const aValue: IsgcObjectJSON);
begin
  Data.SetItem(i, TsgcObjectSystemJSON(aValue));
end;

procedure TsgcSystemJSON.SetIsArray(const Value: Boolean);
begin
  FIsArray := Value;
end;

procedure TsgcSystemJSON.SetNode(const aName: String; const aValue:
    IsgcObjectJSON);
begin
  Data.SetNode(aName, TsgcObjectSystemJSON(aValue));
end;

procedure TsgcObjectSystemJSON.Clear;
begin
  FJSONObject := nil;
end;

function TsgcObjectSystemJSON.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FJSONObject) then
    result := JSONObject.Count;
end;

function TsgcObjectSystemJSON.GetItem(const i: Integer): IsgcObjectJSON;
begin
  result := JSONObject.GetItem(i);
end;

function TsgcObjectSystemJSON.GetJSONObject: IsgcJSON;
begin
  if not Assigned(FJSONObject) then
  begin
    FJSONObject := TsgcSystemJSON.Create(self);
    if JSONType = sgcJSONList then
      TsgcSystemJSON(FJSONObject).FJSONObject := TJSONArray(FJSONValue)
    else
    begin
      JSONType := sgcJSONObject;
      TsgcSystemJSON(FJSONObject).FJSONObject := TJSONObject(FJSONValue);
    end;
    TsgcSystemJSON(FJSONObject).FJSONName := Name;
  end;
  Result := FJSONObject;
end;


function TsgcObjectSystemJSON.GetJSONType: TsgcJSONtype;
begin
  Result := FJSONtype;
end;

function TsgcObjectSystemJSON.GetName: String;
begin
  Result := FName;
end;

function TsgcObjectSystemJSON.GetNode(const aName: String): IsgcObjectJSON;
begin
  result := JSONObject.GetNode(aName);
end;

function TsgcObjectSystemJSON.GetValue: Variant;
var
  oJSONPair: TJSONPair;
begin
  if IsArray then
  begin
    if TJSONArray(FJSONValue).Items[StrToInt(Name)].ClassType = TJSONObject then
      Result := TJSONArray(FJSONValue).Items[StrToInt(Name)].ToJSON
    else if TJSONArray(FJSONValue).Items[StrToInt(Name)].ClassType = TJSONArray then
      Result := TJSONArray(FJSONValue).Items[StrToInt(Name)].ToJSON
    else
      Result := TJSONArray(FJSONValue).Items[StrToInt(Name)].Value
  end
  else
  begin
    Result := '';
    if IsObject then
      result := TJSONObject(FJSONValue).ToJSON
    else
    begin
      oJSONPair := GetJSONPairByName(TJSONObject(FJSONValue), Name);
      if Assigned(oJSONPair) then
      begin
        if oJSONPair.JsonValue.ClassType = TJSONObject then
          result := oJSONPair.JsonValue.ToJSON
        else if oJSONPair.JsonValue.ClassType = TJSONArray then
          result := oJSONPair.JsonValue.ToJSON
        else
          result := oJSONPair.JsonValue.Value;
      end;
    end
  end;
end;

procedure TsgcObjectSystemJSON.SetItem(const i: Integer; const aValue:
    IsgcObjectJSON);
begin
  JSONObject.SetItem(i, TsgcObjectSystemJSON(aValue));
end;

procedure TsgcObjectSystemJSON.SetJSONObject(const Value: IsgcJSON);
begin
  FJSONObject := Value;
end;

procedure TsgcObjectSystemJSON.SetJSONType(const Value: TsgcJSONtype);
begin
  FJSONtype := Value;
end;

procedure TsgcObjectSystemJSON.SetName_(const Value: String);
begin
  FName := Value;
end;

procedure TsgcObjectSystemJSON.SetNode(const aName: String; const aValue:
    IsgcObjectJSON);
begin
  JSONObject.SetNode(aName, TsgcObjectSystemJSON(aValue));
end;

procedure TsgcObjectSystemJSON.SetValue(const Value: Variant);
begin
  if Assigned(FJSONValue) then
  begin
    if FJSONValue.ClassType = TJSONString then
    begin
      TJSONString(FJSONValue).Free;
      FJSONValue := TJSONString.Create(Value);
    end
    else if FJSONValue.ClassType = TJSONNumber then
    begin
      TJSONNumber(FJSONValue).Free;
      FJSONValue := TJSONNumber.Create(Value);
    end
    else if FJSONValue.ClassType = TJSONPair then
    begin
      TJSONPair(FJSONValue).JsonValue.Free;
      TJSONPair(FJSONValue).JsonValue := TJSONString.Create(Value);
    end;
  end;
end;

end.
