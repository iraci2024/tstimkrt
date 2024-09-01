
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScMD5SHA1CSP;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF SBRIDGE}
  CRTypes, CRHashAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLTypes, TdsBridge;
{$ELSE}
  TdsUtilsUni, TdsSSLTypesUni, TdsBridgeUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScHashAlgorithm, ScUtils, ScSSLTypes, ScBridge;
{$ENDIF}

type
  TMD5SHA1CryptoServiceProvider = class(THashAlgorithm)
  private
    FMD5: THashAlgorithm;
    FSHA1: THashAlgorithm;
    FProtocol: TScSSLProtocol;
    FMasterKey: TBytes;
    procedure SetMasterKey(const Value: TBytes);

  protected
    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Initialize; override;
    class function GetHashSize: Integer; override;

    property Protocol: TScSSLProtocol read FProtocol write FProtocol;
    property MasterKey: TBytes read FMasterKey write SetMasterKey;
  end;

implementation

uses
{$IFNDEF SBRIDGE}
  CLRClasses, CRDECUtil, CRHash,
{$IFNDEF UNIDACPRO}
  TdsSSL3HandshakeLayer;
{$ELSE}
  TdsSSL3HandshakeLayerUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScDECUtil, ScHash,
  ScSSL3HandshakeLayer;
{$ENDIF}

{ TMD5SHA1CryptoServiceProvider }

constructor TMD5SHA1CryptoServiceProvider.Create;
begin
  inherited;

  FProtocol := spTls1;
  FMD5 := THash_MD5.Create;
  FSHA1 := THash_SHA1.Create;
end;

destructor TMD5SHA1CryptoServiceProvider.Destroy;
begin
  FMD5.Free;
  FSHA1.Free;

  if Length(FMasterKey) > 0 then
    FillChar(FMasterKey[0], Length(FMasterKey), 0);

  inherited;
end;

procedure TMD5SHA1CryptoServiceProvider.SetMasterKey(const Value: TBytes);
begin
  SetLength(FMasterKey, Length(Value));
  if Length(Value) > 0 then
    Move(Value[0], FMasterKey[0], Length(Value));
end;

class function TMD5SHA1CryptoServiceProvider.GetHashSize: Integer;
begin
  Result := 36;
end;

procedure TMD5SHA1CryptoServiceProvider.Initialize;
begin
  FMD5.Initialize;
  FSHA1.Initialize;
end;

procedure TMD5SHA1CryptoServiceProvider.HashCore(const Data: TValueArr; Offset, Count: Integer);
begin
  FMD5.TransformBlock(Data, Offset, Count, Data, Offset);
  FSHA1.TransformBlock(Data, Offset, Count, Data, Offset);
end;

function TMD5SHA1CryptoServiceProvider.HashFinal: TBytes;
var
  FinalMD5, FinalSHA1: THashAlgorithm;
begin
  if FProtocol = spSsl3 then begin
    FinalMD5 := TSsl3HandshakeMac.Create(haMD5, FMD5, FMasterKey);
    FinalSHA1 := TSsl3HandshakeMac.Create(haSHA1, FSHA1, FMasterKey);
  end
  else begin
    FinalMD5 := FMD5;
    FinalSHA1 := FSHA1;
  end;

  try
    SetLength(Result, 36);
    FinalMD5.TransformFinalBlock(nil, 0, 0);
    FinalSHA1.TransformFinalBlock(nil, 0, 0);

    Buffer.BlockCopy(FinalMD5.Hash, 0, Result, 0, 16);
    Buffer.BlockCopy(FinalSHA1.Hash, 0, Result, 16, 20);

  finally
    if FProtocol = spSsl3 then begin
      FinalMD5.Free;
      FinalSHA1.Free;
    end;
  end;
end;

end.

