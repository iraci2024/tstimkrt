{***************************************************************************
 sgcP2P component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcP2P_Reg;

interface

{$I sgcVer.inc}

{$IFDEF SGC_P2P}

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
  sgcP2P;
{$ENDIF}

  procedure Register;


implementation

procedure Register;
begin
  {$IFDEF SGC_P2P}
  Classes.RegisterComponents('SGC WebSockets P2P',
    [
      TsgcUDPCLient
      {$IFDEF SGC_EDT_PRO}
      , TsgcUDPServer
      {$ENDIF}
      {$IFDEF SGC_STUN}
      , TsgcSTUNClient
      {$IFDEF SGC_EDT_PRO}
      , TsgcSTUNServer
      {$ENDIF}
      {$ENDIF}
      {$IFDEF SGC_TURN}
      , TsgcTURNClient
      , TsgcTURNServer
      {$ENDIF}
      {$IFDEF SGC_ICE}
      , TsgcICEClient
      {$ENDIF}
    ]
  );
  {$ENDIF}
end;


end.
