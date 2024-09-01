{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server_APIs;

interface

{$I sgcVer.inc}

{$IFDEF SGC_APIS}

uses
  sgcWebSocket_Server_API_SocketIO, sgcWebSocket_Server_API_RTCMultiConnection;

type

  TsgcWSAPIServer_SocketIO = class(TsgcWSServer_API_SocketIO)
  published
    property OnConnect;
    property OnMessage;
    property OnError;
    property OnException;
    property OnDisconnect;
    property OnSocketIOJSONMessage;

    property Server;
    property SocketIO;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPIServer_RTCMultiConnection = class(TsgcWSServer_API_RTCMultiConnection)
  published
//    property OnConnect;
//    property OnMessage;
//    property OnError;
//    property OnException;
//    property OnDisconnect;

    property Server;
    property RTCMultiConnection;

//    property RawMessages;

    property Version;
  end;

{$ENDIF}

implementation

end.
