{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Reg;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  {$IFDEF LAZARUS}
  PropEdits, ComponentEditors,
  {$ELSE}
  {$IFNDEF APPMETHOD}
  {$IFDEF SGC_DESIGN_PACKAGE}
  DesignEditors, DesignIntf,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF IWIX}
  {$IFNDEF APPMETHOD}
  sgcIWWebSocket,
  {$ENDIF}
  {$ENDIF}
  sgcWebSocket;

  procedure Register;

implementation

{$IFDEF D2007}
uses
  //-->start sgc_package_reg
  {$IFNDEF BCB}
  {$IFNDEF LAZARUS}
  {$IFDEF SGC_PACKAGE_REG}
  sgcPackage_Reg,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  //<--end sgc_package_reg
  {$IFDEF SGC_DESIGN_PACKAGE}
  ToolsAPI,
  {$ENDIF}
  Windows,
  sgcBase_Const;
{$ENDIF}

//-->start sgc_package_reg
{$IFNDEF BCB}
{$IFNDEF LAZARUS}
{$IFDEF D2007}
{$IFDEF SGC_PACKAGE_REG}
var
  NotifierIndex: Integer = -1;
  PackageReg: TsgcCheckPackage;
  IOTA: TIDEIOTAsgcWebSockets;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
//<--end sgc_package_reg

procedure Register;
begin
  Classes.RegisterComponents('SGC WebSockets',
    [
      TsgcWebSocketClient
      {$IFDEF SGC_WINHTTP}
      ,TsgcWebSocketClient_WinHTTP
      {$ENDIF}
      {$IFDEF IWIX}
      {$IFNDEF APPMETHOD}
      ,TsgcIWWebSocketClient
      {$ENDIF}
      {$ENDIF}
      {$IFDEF SGC_EDT_PRO}
      ,TsgcWebSocketServer
      ,TsgcWebSocketHTTPServer
      ,TsgcWebSocketProxyServer
      ,TsgcWebSocketLoadBalancerServer
      {$IFDEF SGC_EDT_ENT}
      {$IFDEF SGC_HTTPAPI}
      ,TsgcWebSocketServer_HTTPAPI
      {$ENDIF}
      {$ENDIF}
      {$IFDEF SGC_QUIC}
      ,TsgcQUICClient
      {$ENDIF}
      {$ENDIF}
    ]
  );
  {$IFNDEF LAZARUS}
  {$IFDEF D2007}
  {$IFDEF SGC_DESIGN_PACKAGE}
  ForceDemandLoadState(dlDisable);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  //-->start sgc_package_reg
  {$IFNDEF BCB}
  {$IFNDEF LAZARUS}
  {$IFDEF D2007}
  {$IFDEF SGC_PACKAGE_REG}
  PackageReg := TsgcCheckPackage.Create('sgcWebSockets');
  IOTA := TIDEIOTAsgcWebSockets.Create;
  IOTA.CheckPackage := PackageReg;
  NotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(IOTA);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  //<--end sgc_package_reg
end;

{$IFDEF D2007}
{$IFDEF SGC_DESIGN_PACKAGE}
procedure RegisterSplashScreen;
var
  oBitmap: HBITMAP;
  vPackage: String;
begin
  if Assigned(SplashScreenServices) then
  begin
    oBitmap := LoadBitmap(FindResourceHInstance(HInstance), 'TSGCWEBSOCKETCLIENT');
    Try
      vPackage := CS_APPLICATION_NAME + ' ' + CS_EDITION + ' ' + CS_LICENSE + ' ' + CS_VERSION;
      SplashScreenServices.AddPluginBitmap(vPackage, oBitmap, False, 'Registered');
    Finally
      DeleteObject(oBitmap);
    End;
  end;
end;

initialization
  RegisterSplashScreen;

finalization
  //-->start sgc_package_reg
  {$IFNDEF BCB}
  {$IFNDEF LAZARUS}
  {$IFDEF D2007}
  {$IFDEF SGC_PACKAGE_REG}
  if Assigned(PackageReg) then FreeAndNil(PackageReg);
  if NotifierIndex <> -MAXWORD then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(NotifierIndex);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  //<--end sgc_package_reg
{$ENDIF}
{$ENDIF}

end.
