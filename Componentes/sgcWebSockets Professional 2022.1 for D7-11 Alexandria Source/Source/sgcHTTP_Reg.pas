{***************************************************************************
 sgcHTTP component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcHTTP_Reg;

interface

{$I sgcVer.inc}

{$IFDEF SGC_HTTP}

uses
  Classes,
  {$IFDEF LAZARUS}
  PropEdits, ComponentEditors,
  {$ELSE}
  {$IFNDEF APPMETHOD}
  {$IFDEF SGC_DESIGN_PACKAGE}
  DesignEditors, DesignIntf,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  sgcHTTP;

  procedure Register;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

procedure Register;
begin
  Classes.RegisterComponents('SGC WebSockets HTTP',
    [
      {$IFDEF SGC_OAUTH}TsgcHTTP_OAuth2_Client{$ENDIF}
      {$IFDEF SGC_OAUTH_SERVER}, TsgcHTTP_OAuth2_Server{$ENDIF}
      {$IFDEF SGC_JWT}, TsgcHTTP_JWT_Client{$ENDIF}
      {$IFDEF SGC_JWT_SERVER}, TsgcHTTP_JWT_Server{$ENDIF}
      {$IFDEF SGC_AWS}, TsgcHTTPAWS_SQS_Client{$ENDIF}
      {$IFDEF SGC_GOOGLE_CLOUD}
        , TsgcHTTPGoogleCloud_PubSub_Client
        , TsgcHTTPGoogleCloud_Calendar_Client
      {$ENDIF}
      {$IFDEF SGC_EDT_ENT}
      {$IFDEF SGC_HTTP2}, TsgcHTTP2Client{$ENDIF}
      {$ENDIF}
    ]
  );
end;

{$ENDIF}

end.
