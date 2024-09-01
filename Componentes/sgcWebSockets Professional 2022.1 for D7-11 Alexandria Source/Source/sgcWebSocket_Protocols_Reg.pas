{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocols_Reg;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

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
  {$IFDEF SGC_EDT_PRO}
  {$IFDEF IWIX}
  {$IFNDEF APPMETHOD}
  sgcIWWebSocket_Protocols,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  sgcWebSocket_Protocols;

  procedure Register;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

procedure Register;
begin
  Classes.RegisterComponents('SGC WebSockets Protocols',
    [
      TsgcWSPClient_MQTT
      , TsgcWSPClient_WAMP2
      , TsgcWSPClient_STOMP, TsgcWSPClient_STOMP_RabbitMQ
      , TsgcWSPClient_STOMP_ActiveMQ

      {$IFDEF SGC_EDT_PRO}
      {$IFDEF IWIX}
      {$IFNDEF APPMETHOD}
      , TsgcIWWSPClient_sgc, TsgcIWWSPClient_Dataset
      {$ENDIF}
      {$ENDIF}
      , TsgcWSPServer_broker, TsgcWSPClient_broker
      , TsgcWSPServer_sgc, TsgcWSPClient_sgc
      , TsgcWSPServer_Dataset, TsgcWSPClient_Dataset
      , TsgcWSPServer_Files, TsgcWSPClient_Files
      , TsgcWSPServer_Presence, TsgcWSPClient_Presence
      , TsgcWSPServer_WebRTC, TsgcWSPServer_AppRTC
      , TsgcWSPServer_WAMP, TsgcWSPClient_WAMP
      {$ENDIF}
      {$IFDEF SGC_AMQP}
      , TsgcWSPClient_AMQP
      {$ENDIF}
    ]
  );
end;

{$ENDIF}

end.
