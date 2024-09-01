{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressMapControl }
{ }
{ Copyright (c) 2013-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL }
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

unit dxMapControlHttpRequest;

interface

{$I cxVer.inc}

uses
  Classes, Forms, IdGlobal, IdURI, IdHTTPHeaderInfo, dxHttpRequest,
  dxHttpIndyRequest;

type
  TdxMapControlConnectionProtocol = (cpDefault, cpHTTP, cpHTTPS);
  TdxMapControlHttpRequestMode = TdxHttpRequestMode; // for internal use
  TdxMapControlHttpRequestEvent = TdxHttpRequestEvent; // for internal use

  TdxMapControlHttpRequest = class(TdxHttpIndyRequest) // for internal use
  protected
    function DoGet(const AUri: string; AResponseContent: TStream)
      : Boolean; override;
  end;

const
  dxMapControlDefaultConnectionProtocol
    : TdxMapControlConnectionProtocol = cpHTTP;

function GetContent(const AUri: string; AResponseContent: TStream): Boolean;
// for internal use
function dxParamsEncode(const ASrc: string): string; // for internal use
function dxMapControlHttpDefaultProxyInfo: TIdProxyConnectionInfo; // #doc'd
function dxMapControlGetActualConnectionProtocol
  (AValue: TdxMapControlConnectionProtocol): TdxMapControlConnectionProtocol;
// for internal use

implementation

uses
  SysUtils, dxWinInet;

function IsSecureConnection(const AUri: string): Boolean;
begin
  Result := Pos('https://', LowerCase(AUri)) = 1;
end;

function dxMapControlHttpDefaultProxyInfo: TIdProxyConnectionInfo;
begin
  Result := dxHttpIndyDefaultProxyInfo;
end;

function GetContent(const AUri: string; AResponseContent: TStream): Boolean;
begin
  try
    if IsSecureConnection(AUri) then
      Result := TdxHttpHelper.GetStream(Application.Title, AUri, '',
        AResponseContent)
    else
      Result := dxHttpIndyRequest.GetContent(AUri, AResponseContent);
  except
    on Exception do
      Result := False;
  end;
end;

function dxParamsEncode(const ASrc: string): string;
begin
  Result := dxHttpIndyRequest.dxParamsEncode(ASrc);
end;

function dxMapControlGetActualConnectionProtocol
  (AValue: TdxMapControlConnectionProtocol): TdxMapControlConnectionProtocol;
begin
  case AValue of
    cpDefault:
      Result := dxMapControlDefaultConnectionProtocol
  else
    Result := AValue;
  end;
end;

{ TdxMapControlHttpRequest }

function TdxMapControlHttpRequest.DoGet(const AUri: string;
  AResponseContent: TStream): Boolean;
begin
  if IsSecureConnection(AUri) then
    Result := TdxHttpHelper.GetStream(Application.Title, AUri, '',
      AResponseContent, function(const AUri: string): Boolean
      begin
        DoRequest(mhrmRead, 0);
        Result := IsCancelled;
      end)
  else
    Result := inherited DoGet(AUri, AResponseContent);
end;

end.
