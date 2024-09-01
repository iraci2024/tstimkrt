{***************************************************************************
 sgcIoT component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIoT_Reg;

interface

{$I sgcVer.inc}

{$IFDEF SGC_IOT}

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
  sgcIoT;
{$ENDIF}

  procedure Register;


implementation

procedure Register;
begin
  {$IFDEF SGC_IOT}
  Classes.RegisterComponents('SGC WebSockets IoT',
    [
      TsgcIoTAmazon_MQTT_Client,
      TsgcIoTAzure_MQTT_Client
    ]
  );
  {$ENDIF}
end;


end.
