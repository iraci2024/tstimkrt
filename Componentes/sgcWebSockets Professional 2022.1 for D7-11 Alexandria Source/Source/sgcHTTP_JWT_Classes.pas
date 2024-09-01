{ ***************************************************************************
  sgcJWT component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_JWT_Classes;

interface

{$I sgcVer.inc}
{$IFDEF SGC_JWT}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders{$ELSE}IdSSLOpenSSLHeaders{$ENDIF},
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // sgc
  sgcHTTP_JWT_Types, sgcJSON, sgcBase_Helpers;

type

  TsgcHTTP_JWTBase_JSON = class(TPersistent)
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON;
  protected
    function GetJSONText: string; virtual;
    procedure DoBuildJSON; virtual;
  private
    FKeyValue: TStringList;
    function GetKeyValue: TStringList;
    procedure DoBuildJSONKeyValue;
  public
    procedure ClearKeyValues;
    procedure AddKeyValue(const aKey, aValue: string); overload;
    procedure AddKeyValue(const aKey: string; aValue: Boolean); overload;
  public
    property JSONText: string read GetJSONText;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type
  TsgcHTTP_JWTHeader = class(TsgcHTTP_JWTBase_JSON)
  private
    Falg: TsgcHTTP_JWT_Algorithm;
    Fkid: string;
    Ftyp: string;
  protected
    procedure DoBuildJSON; override;
  public
    constructor Create; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property alg: TsgcHTTP_JWT_Algorithm read Falg write Falg;
    property kid: string read Fkid write Fkid;
    property typ: string read Ftyp write Ftyp;
  end;

type
  TsgcHTTP_JWTPayload = class(TsgcHTTP_JWTBase_JSON)
  private
    Faud: string;
    Fexp: Int64;
    Fiat: Int64;
    Fnbf: Int64;
    Fiss: string;
    Fsub: string;
  private
    Fjti: string;
  protected
    procedure DoBuildJSON; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property aud: string read Faud write Faud;
    property exp: Int64 read Fexp write Fexp;
    property iat: Int64 read Fiat write Fiat;
    property nbf: Int64 read Fnbf write Fnbf;
    property iss: string read Fiss write Fiss;
    property jti: string read Fjti write Fjti;
    property sub: string read Fsub write Fsub;
  end;

  TsgcHTTP_JWT_Base = class

  end;

{$ENDIF}

implementation

{$IFDEF SGC_JWT}

constructor TsgcHTTP_JWTBase_JSON.Create;
begin
  inherited;
end;

destructor TsgcHTTP_JWTBase_JSON.Destroy;
begin
  sgcFree(FKeyValue);
  sgcFree(FJSON);
  inherited;
end;

procedure TsgcHTTP_JWTBase_JSON.AddKeyValue(const aKey, aValue: string);
begin
  GetKeyValue.Add(aKey + '=' + aValue);
end;

procedure TsgcHTTP_JWTBase_JSON.AddKeyValue(const aKey: string;
  aValue: Boolean);
begin
  if aValue then
    GetKeyValue.Add(aKey + '=true')
  else
    GetKeyValue.Add(aKey + '=false');
end;

procedure TsgcHTTP_JWTBase_JSON.ClearKeyValues;
begin
  GetKeyValue.Clear;
end;

procedure TsgcHTTP_JWTBase_JSON.DoBuildJSON;
begin
  JSON.Clear;
end;

procedure TsgcHTTP_JWTBase_JSON.DoBuildJSONKeyValue;
var
  i: integer;
  vInt: Int64;
begin
  for i := 0 to GetKeyValue.Count - 1 do
  begin
    if GetKeyValue.ValueFromIndex[i] = 'true' then
      JSON.AddPair(GetKeyValue.Names[i], true)
    else if GetKeyValue.ValueFromIndex[i] = 'false' then
      JSON.AddPair(GetKeyValue.Names[i], false)
    else if TryStrToInt64(GetKeyValue.ValueFromIndex[i], vInt) then
      JSON.AddPair(GetKeyValue.Names[i], vInt)
    else
      JSON.AddPair(GetKeyValue.Names[i],
        sgcEncodeJSON(GetKeyValue.ValueFromIndex[i]));
  end;
end;

function TsgcHTTP_JWTBase_JSON.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(nil);
  Result := FJSON;
end;

function TsgcHTTP_JWTBase_JSON.GetJSONText: string;
begin
  DoBuildJSON;
  DoBuildJSONKeyValue;
  Result := JSON.Text;
end;

function TsgcHTTP_JWTBase_JSON.GetKeyValue: TStringList;
begin
  if not Assigned(FKeyValue) then
    FKeyValue := TStringList.Create;
  Result := FKeyValue;
end;

constructor TsgcHTTP_JWTHeader.Create;
begin
  inherited;
  typ := 'JWT';
end;

procedure TsgcHTTP_JWTHeader.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWTHeader then
  begin
    alg := TsgcHTTP_JWTHeader(aSource).alg;
    typ := TsgcHTTP_JWTHeader(aSource).typ;
    kid := TsgcHTTP_JWTHeader(aSource).kid;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_JWTHeader.DoBuildJSON;
begin
  inherited;
  JSON.AddPair('alg', GetAlgorithmString(alg));
  JSON.AddPair('typ', sgcEncodeJSON(typ));
  if kid <> '' then
    JSON.AddPair('kid', sgcEncodeJSON(kid));
end;

procedure TsgcHTTP_JWTPayload.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP_JWTPayload then
  begin
    aud := TsgcHTTP_JWTPayload(aSource).aud;
    exp := TsgcHTTP_JWTPayload(aSource).exp;
    iat := TsgcHTTP_JWTPayload(aSource).iat;
    iss := TsgcHTTP_JWTPayload(aSource).iss;
    sub := TsgcHTTP_JWTPayload(aSource).sub;
    nbf := TsgcHTTP_JWTPayload(aSource).nbf;
    jti := TsgcHTTP_JWTPayload(aSource).jti;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTP_JWTPayload.DoBuildJSON;
begin
  inherited;
  if iss <> '' then
    JSON.AddPair('iss', sgcEncodeJSON(iss));
  if sub <> '' then
    JSON.AddPair('sub', sgcEncodeJSON(sub));
  if aud <> '' then
    JSON.AddPair('aud', sgcEncodeJSON(aud));
  if jti <> '' then
    JSON.AddPair('jti', sgcEncodeJSON(jti));
  if iat > 0 then
    JSON.AddPair('iat', iat);
  if exp > 0 then
    JSON.AddPair('exp', exp);
  if nbf > 0 then
    JSON.AddPair('nbf', nbf);
end;

{$ENDIF}

initialization


finalization

end.
