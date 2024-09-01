{***************************************************************************
 sgcHTTP component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcHTTP_Classes;

interface

{$I sgcVer.inc}

{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Classes, sgcTCP_Classes;

type

  TsgcOnAuthToken = procedure(Sender: TObject;
    const TokenType, Token, Data: String) of object;
  TsgcOnAuthTokenError = procedure(Sender: TObject;
    const Error, ErrorDescription, Data: String) of object;


  TsgcHTTPComponent_Base = class(TsgcComponent_Base)
  end;

  TsgcHTTPComponentClient_Base = class(TsgcHTTPComponent_Base)
  end;

  TsgcHTTPComponentClientAuthToken_Base = class(TsgcHTTPComponentClient_Base)
    { events }
  private
    FOnAuthTokenError: TsgcOnAuthTokenError;
    FOnAuthToken: TsgcOnAuthToken;
  protected
    procedure DoAuthTokenEvent(const aTokenType, aToken, aData: String); virtual;
    procedure DoAuthTokenErrorEvent(const aError, aErrorDescription, aData:
        String); virtual;
  public
    property OnAuthToken: TsgcOnAuthToken read FOnAuthToken write FOnAuthToken;
    property OnAuthTokenError: TsgcOnAuthTokenError read FOnAuthTokenError
      write FOnAuthTokenError;
    { events }
  end;

  TsgcHTTPComponentServer_Base = class(TsgcHTTPComponent_Base)
  end;

  TsgcHTTPComponentServerAuthToken_Base = class(TsgcHTTPComponentServer_Base)
  end;

  TsgcHTTPProxy_Options = class(TsgcTCPProxy_Options)

  end;




{$ENDIF}

implementation

procedure TsgcHTTPComponentClientAuthToken_Base.DoAuthTokenErrorEvent(const
    aError, aErrorDescription, aData: String);
begin
  if Assigned(FOnAuthTokenError) then
    FOnAuthTokenError(self, aError, aErrorDescription, aData);
end;

procedure TsgcHTTPComponentClientAuthToken_Base.DoAuthTokenEvent(const
    aTokenType, aToken, aData: String);
begin
  if Assigned(FOnAuthToken) then
    FOnAuthToken(self, aTokenType, aToken, aData);
end;

{$IFDEF SGC_HTTP}

{$ENDIF}


end.
