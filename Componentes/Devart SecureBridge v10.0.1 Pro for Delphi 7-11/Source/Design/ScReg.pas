
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Register
//////////////////////////////////////////////////

{$I SB.inc}

unit ScReg;

interface

procedure Register;
{$IFDEF VER10P}
procedure RegisterSplashScreen(ProductName: string; ProductVersion: string; HProductIco: cardinal; IsTrial: boolean; LicensedType: string);
procedure RegisterAboutBox(ProductName: string; ProductVersion: string; ProductPage:string; AboutDescription: string; HProductIco: cardinal;
                           IsTrial: boolean; LicensedType: string; Edition: string);
procedure UnregisterAboutBox;
{$ENDIF}

implementation

{$IFNDEF FPC}
  {$IFDEF VER28P}
    {$R ScDesign28p.res}
  {$ELSE}
    {$R ScDesign.res}
    {$IFDEF VER10P}
      {$R ScDesign10p.res}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes,
  ScBridge, ScSSHClient, ScSSHChannel,
  ScSSLClient, ScSSLServer, ScTCPServer,
  ScCryptoAPIStorage, ScSFTPClient, ScCMS, ScHttp,
  ScFTPClient, ScSMTPClient,
  ScWebSocketClient, ScSignalRHubConnection
{$IFNDEF STD}
  , ScSSHServer, ScSFTPServer
{$ENDIF}
  {$IFDEF VER10P}, ToolsAPI, SysUtils, Windows{$ENDIF};

{$I SecureBridgeVer.inc}

{$IFDEF VER10P}
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;
{$ENDIF}

{$IFDEF VER10P}
procedure RegisterSplashScreen(ProductName: string; ProductVersion: string; HProductIco: cardinal; IsTrial: boolean; LicensedType: string);
begin
  SplashScreenServices.AddPluginBitmap(ProductName + ' ' + ProductVersion, HProductIco, IsTrial, LicensedType);
end;

procedure RegisterAboutBox(ProductName: string; ProductVersion: string; ProductPage:string; AboutDescription: string; HProductIco: cardinal;
                           IsTrial: boolean; LicensedType: string; Edition: string);
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(ProductName + ' ' + ProductVersion, AboutDescription,
  HProductIco, IsTrial, LicensedType, Edition);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;
{$ENDIF}

procedure Register;
begin
{$IFDEF VER10P}
  RegisterSplashScreen('Devart SecureBridge Components',
                       SecureBridgeVersion,
                       LoadBitmap(HInstance, {$IFDEF VER10}'SPLASHBL'{$ENDIF}
                                             {$IFDEF VER11}'SPLASHWH'{$ENDIF}
                                             {$IFDEF VER12}'SPLASHWH'{$ENDIF}
                                             {$IFDEF VER14P}'SPLASHBL'{$ENDIF}),
                       False, 'Licensed'
                      );
  RegisterAboutBox('Devart SecureBridge Components',
                   SecureBridgeVersion,
                   'http://www.devart.com/sbridge/',
                   'Devart SecureBridge Components' + #13#10 +
                   'Copyright 2007 - 2021 Devart. All rights reserved.' + #13#10 +
                   'Web: www.devart.com/sbridge/' + #13#10 +
                   'Support: www.devart.com/sbridge/support.html',
                   LoadBitmap(HInstance, 'ABOUT'),
                   False, 'Licensed'
                   , 'Standard edition');
{$ENDIF}

  RegisterComponents('SecureBridge', [TScSSHClient]);
  RegisterComponents('SecureBridge', [TScSSHChannel]);
  RegisterComponents('SecureBridge', [TScSSHShell]);
  RegisterComponents('SecureBridge', [TScSFTPClient]);
{$IFNDEF STD}
  RegisterComponents('SecureBridge', [TScSSHServer]);
  RegisterComponents('SecureBridge', [TScSFTPServer]);
{$ENDIF}
  RegisterComponents('SecureBridge', [TScSSLClient]);
  RegisterComponents('SecureBridge', [TScSSLServerConnection]);
  RegisterComponents('SecureBridge', [TScMemoryStorage]);
  RegisterComponents('SecureBridge', [TScFileStorage]);
{$IFDEF MSWINDOWS}
  RegisterComponents('SecureBridge', [TScRegStorage]);
{$ENDIF}
{$IFNDEF LINUX}
{$IFNDEF LINUX_BSD}
{$IFNDEF ANDROID}
  RegisterComponents('SecureBridge', [TScCryptoAPIStorage]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
  RegisterComponents('SecureBridge', [TScCMSProcessor]);
  RegisterComponents('SecureBridge', [TScFTPClient]);
  RegisterComponents('SecureBridge', [TScHttpWebRequest]);
  RegisterComponents('SecureBridge', [TScWebSocketClient]);
  RegisterComponents('SecureBridge', [TScHubConnection]);
  RegisterComponents('SecureBridge', [TScSMTPClient]);
  RegisterComponents('SecureBridge', [TScTCPServer]);
end;

initialization
{$IFDEF FPC}
  {$I ScDesign.lrs}
{$ENDIF}

{$IFDEF VER10P}
finalization
  UnregisterAboutBox;
{$ENDIF}

end.
