{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIWWebSocket;

interface

{$I sgcVer.inc}

{$IFDEF IWIX}
uses
  Classes,
  sgcIWWebSocket_Client;

type

  TsgcIWWebSocketClient = class(TsgcIWWSClient)
  published

    property JSOpen;
    property JSMessage;
    property JSClose;
    property JSError;

    property Host;
    property Port;
    property TLS;
    property Parameters;
    property URL;

    property Transports;

    property OnAsyncConnect;
    property OnAsyncMessage;
    property OnAsyncDisconnect;
    property OnAsyncError;

    property OnAsyncEmulation;

    property Version;
  End;
{$ENDIF}


implementation

end.
