{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_Classes, sgcHTTP_Client;

type
  TsgcHTTPAPIExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

  TsgcHTTPAPI_client = class(TsgcHTTPComponentClient_Base)
    { properties }
  private
    FLog: Boolean;
    FLogFileName: String;
    FTLSOptions: TsgcIdHTTPTLS_Options;
    function GetTLSOptions: TsgcIdHTTPTLS_Options;
  protected
    property Log: Boolean read FLog write FLog;
    property LogFileName: String read FLogFileName write FLogFileName;
  public
    property TLSOptions: TsgcIdHTTPTLS_Options read GetTLSOptions write FTLSOptions;
    { properties }

    { destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { destrucgtor }

    { methods }
  protected
    function DoGet(const aURL: String; const aHeaders: TStrings = nil)
      : string; virtual;
    function DoPost(const aURL, aBody: String; const aHeaders: TStrings = nil)
      : string; virtual;
    function DoDelete(const aURL: String; const aHeaders: TStrings = nil; const
        aBody: string = ''): string; virtual;
    function DoPatch(const aURL, aBody: String; const aHeaders: TStrings = nil):
        string; virtual;
    function DoPut(const aURL, aBody: String; const aHeaders: TStrings = nil):
        string; virtual;
  public
    function Get(const aURL: String; const aHeaders: TStrings = nil): String;
    function Post(const aURL, aBody: String;
      const aHeaders: TStrings = nil): String;
    function Delete(const aURL: String; const aHeader: TStrings = nil; const aBody:
        string = ''): String;
    function Patch(const aURL, aBody: String; const aHeaders: TStrings = nil):
        String;
    function Put(const aURL, aBody: String; const aHeaders: TStrings = nil): String;
    { methods }

    { events }
  private
    FOnHTTPAPIException: TsgcHTTPAPIExceptionEvent;
  public
    property OnHTTPAPIException: TsgcHTTPAPIExceptionEvent
      read FOnHTTPAPIException write FOnHTTPAPIException;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
{$IFDEF SGC_INDY}sgcIdHTTP{$ELSE}IdHTTP{$ENDIF},
  sgcBase_Helpers, sgcWebSocket_Types, StrUtils;

constructor TsgcHTTPAPI_client.Create(aOwner: TComponent);
begin
  inherited;
  TLSOptions.Version := tls1_2;
end;

destructor TsgcHTTPAPI_client.Destroy;
begin
  sgcFree(FTLSOptions);
  inherited;
end;

function TsgcHTTPAPI_client.Delete(const aURL: String; const aHeader: TStrings
    = nil; const aBody: string = ''): String;
begin
  Result := DoDelete(aURL, aHeader, aBody);
end;

function TsgcHTTPAPI_client.DoDelete(const aURL: String; const aHeaders:
    TStrings = nil; const aBody: string = ''): string;
var
  oHTTP: TsgcIdHTTP;
begin
  Result := '';
  Try
    oHTTP := TsgcIdHTTP.Create(nil);
    Try
      oHTTP.TLSOptions := TLSOptions;
      oHTTP.Log := Log;
      oHTTP.LogOptions.FileName := LogFileName;
      if Assigned(aHeaders) then
        oHTTP.Request.CustomHeaders.Text := aHeaders.Text;
{$IFDEF INDY10_6_0_5122}
{$IFDEF SGC_INDY_LIB}
      if aBody <> '' then
        Result := oHTTP.Delete(aURL, aBody)
      else
        Result := oHTTP.Delete(aURL);
{$ELSE}
      Result := oHTTP.Delete(aURL);
{$ENDIF}
{$ELSE}
{$IFDEF INDY10_5_5}
      Result := '';
      oHTTP.Delete(aURL);
{$ENDIF}
{$ENDIF}
    Finally
      sgcFree(oHTTP);
    End;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      E.Message := E.Message + '. ' + E.ErrorMessage;
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
    On E: Exception do
    begin
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
  end;
end;

function TsgcHTTPAPI_client.DoGet(const aURL: String;
  const aHeaders: TStrings = nil): string;
var
  oHTTP: TsgcIdHTTP;
begin
  Result := '';
  Try
    oHTTP := TsgcIdHTTP.Create(nil);
    Try
      oHTTP.TLSOptions := TLSOptions;
      oHTTP.Log := Log;
      oHTTP.LogOptions.FileName := LogFileName;
      if Assigned(aHeaders) then
        oHTTP.Request.CustomHeaders.Text := aHeaders.Text;
      Result := oHTTP.Get(aURL);
    Finally
      sgcFree(oHTTP);
    End;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      E.Message := E.Message + '. ' + E.ErrorMessage;
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
    On E: Exception do
    begin
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
  end;
end;

function TsgcHTTPAPI_client.DoPatch(const aURL, aBody: String; const aHeaders:
    TStrings = nil): string;
var
  oHTTP: TsgcIdHTTP;
  oRequest: TStringStream;
begin
  Result := '';
  Try
    oHTTP := TsgcIdHTTP.Create(nil);
    Try
      oHTTP.TLSOptions := TLSOptions;
      oHTTP.Log := Log;
      oHTTP.LogOptions.FileName := LogFileName;
      if Assigned(aHeaders) then
        oHTTP.Request.CustomHeaders.Text := aHeaders.Text;
      oRequest := TStringStream.Create(aBody);
      Try
        {$IFDEF INDY10_6_0_5122}
        Result := oHTTP.Patch(aURL, oRequest);
        {$ELSE}
        raise Exception.Create('Indy version PATCH method not supported');
        {$ENDIF}
      Finally
        sgcFree(oRequest);
      End;
    Finally
      sgcFree(oHTTP);
    End;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      E.Message := E.Message + '. ' + E.ErrorMessage;
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
    On E: Exception do
    begin
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
  end;
end;

function TsgcHTTPAPI_client.DoPost(const aURL, aBody: String;
  const aHeaders: TStrings = nil): string;
var
  oHTTP: TsgcIdHTTP;
  oRequest: TStringStream;
begin
  Result := '';
  Try
    oHTTP := TsgcIdHTTP.Create(nil);
    Try
      oHTTP.TLSOptions := TLSOptions;
      oHTTP.Log := Log;
      oHTTP.LogOptions.FileName := LogFileName;
      if Assigned(aHeaders) then
        oHTTP.Request.CustomHeaders.Text := aHeaders.Text;
      oRequest := TStringStream.Create(aBody);
      Try
        Result := oHTTP.Post(aURL, oRequest);
      Finally
        sgcFree(oRequest);
      End;
    Finally
      sgcFree(oHTTP);
    End;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      E.Message := E.Message + '. ' + E.ErrorMessage;
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
    On E: Exception do
    begin
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
  end;
end;

function TsgcHTTPAPI_client.DoPut(const aURL, aBody: String; const aHeaders:
    TStrings = nil): string;
var
  oHTTP: TsgcIdHTTP;
  oRequest: TStringStream;
begin
  Result := '';
  Try
    oHTTP := TsgcIdHTTP.Create(nil);
    Try
      oHTTP.TLSOptions := TLSOptions;
      oHTTP.Log := Log;
      oHTTP.LogOptions.FileName := LogFileName;
      if Assigned(aHeaders) then
        oHTTP.Request.CustomHeaders.Text := aHeaders.Text;
      oRequest := TStringStream.Create(aBody);
      Try
        Result := oHTTP.Put(aURL, oRequest);
      Finally
        sgcFree(oRequest);
      End;
    Finally
      sgcFree(oHTTP);
    End;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      E.Message := E.Message + '. ' + E.ErrorMessage;
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
    On E: Exception do
    begin
      if Assigned(FOnHTTPAPIException) then
        FOnHTTPAPIException(self, E)
      else
        raise;
    end;
  end;
end;

function TsgcHTTPAPI_client.Get(const aURL: String;
  const aHeaders: TStrings = nil): String;
begin
  Result := DoGet(aURL, aHeaders);
end;

function TsgcHTTPAPI_client.GetTLSOptions: TsgcIdHTTPTLS_Options;
begin
  if not Assigned(FTLSOptions) then
    FTLSOptions := TsgcIdHTTPTLS_Options.Create;
  Result := FTLSOptions;
end;

function TsgcHTTPAPI_client.Patch(const aURL, aBody: String; const aHeaders:
    TStrings = nil): String;
begin
  Result := DoPatch(aURL, aBody, aHeaders);
end;

function TsgcHTTPAPI_client.Post(const aURL, aBody: String;
  const aHeaders: TStrings = nil): String;
begin
  Result := DoPost(aURL, aBody, aHeaders);
end;

function TsgcHTTPAPI_client.Put(const aURL, aBody: String; const aHeaders:
    TStrings = nil): String;
begin
  Result := DoPut(aURL, aBody, aHeaders);
end;

{$ENDIF}

end.
