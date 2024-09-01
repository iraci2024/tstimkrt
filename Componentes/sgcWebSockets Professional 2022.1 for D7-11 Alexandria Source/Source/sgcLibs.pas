{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcLibs;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes,
  sgcLib_RCON_Client
{$IFDEF SGC_EDT_PRO}
    , sgcHTTP_API_Cryptohopper
{$ENDIF}
{$IFDEF SGC_TELEGRAM}
    , sgcLib_Telegram_Client
{$ENDIF}
    ;

Type

{$IFDEF SGC_TELEGRAM}
  TsgcTDLib_Telegram = class(TsgcTDLib_Telegram_Client)
  published
    property OnBeforeReadEvent;
    property OnConnectionStatus;
    property OnAuthorizationStatus;
    property OnAuthenticationCode;
    property OnAuthenticationPassword;
    property OnRegisterUser;
    property OnMessageText;
    property OnMessageDocument;
    property OnMessagePhoto;
    property OnMessageVideo;
    property OnNewChat;
    property OnNewCallbackQuery;
    property OnEvent;
    property OnException;

    property Telegram;
    property Version;
  end;
{$ENDIF}

  TsgcLib_RCON = class(TsgcLib_RCON_Client)
  published
    property OnConnect;
    property OnDisconnect;
    property OnAuthenticate;
    property OnResponse;
    property OnException;

    property RCON_Options;
    property NotifyEvents;
    property Version;
  end;

{$IFDEF SGC_EDT_PRO}
  TsgcHTTP_Cryptohopper = class(TsgcHTTP_API_Cryptohopper)
  published
    property OnWebhook;

    property CryptohopperOptions;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}

implementation

end.
