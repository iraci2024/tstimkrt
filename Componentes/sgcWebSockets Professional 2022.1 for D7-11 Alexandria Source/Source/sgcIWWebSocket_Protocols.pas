{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIWWebSocket_Protocols;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

uses
  Classes,
  sgcIWWebSocket_Protocol_sgc_Client, sgcIWWebSocket_Protocol_Dataset_Client;

type
  TsgcIWWSPClient_sgc = class(TsgcIWWSProtocol_sgc_Client)
  published
    property OnAsyncConnect;
    property OnAsyncDisconnect;
    property OnAsyncMessage;
    property OnAsyncError;
    property OnAsyncSubscription;
    property OnAsyncUnSubscription;

    property Client;

    property Version;
  end;

  TsgcIWWSPClient_Dataset = class(TsgcIWWSProtocol_Dataset_Client)
  published
    property OnAsyncConnect;
    property OnAsyncDisconnect;
    property OnAsyncMessage;
    property OnAsyncError;
    property OnAsyncSubscription;
    property OnAsyncUnSubscription;

    property OnAfterNewRecord;
    property OnAfterUpdateRecord;
    property OnAfterDeleteRecord;

    property Client;
    property Dataset;

    property Version;
  end;

{$ENDIF}

{$ENDIF}

implementation

end.
