unit SSHServerSrv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  SyncObjs, ScSSHUtils, ScBridge, ScSSHServer;

type
  TSSHServerSrv = class(TService)
    ScSSHServer: TScSSHServer;
    ScFileStorage: TScFileStorage;
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceExecute(Sender: TService);
  private
    FLockLog: TCriticalSection;
    FNeedForLog: Boolean;
    FLogFilename: string;

    procedure Log(Message: string);
    procedure LoadState;
    procedure StopSSHServer;

    procedure ScSSHServerAfterClientConnect(Sender: TObject;
      ClientInfo: TScSSHClientInfo);
    procedure ScSSHServerAfterClientDisconnect(Sender: TObject;
      ClientInfo: TScSSHClientInfo);
    procedure ScSSHServerClientError(Sender: TObject;
      ClientInfo: TScSSHClientInfo; E: Exception);
    procedure ScSSHServerBeforeChannelConnect(Sender: TObject;
      ChannelInfo: TscSSHChannelInfo; var Direct: Boolean);
    procedure ScSSHServerAfterChannelDisconnect(Sender: TObject;
      ChannelInfo: TScSSHChannelInfo);
    procedure ScSSHServerBeforeShellConnect(Sender: TObject;
      ClientInfo: TScSSHClientInfo);
    procedure ScSSHServerAfterShellDisconnect(Sender: TObject;
      ClientInfo: TScSSHClientInfo);
    procedure ScSSHServerChannelError(Sender: TObject;
      ChannelInfo: TScSSHChannelInfo; E: Exception);
    procedure ScSSHServerRemotePortForwardingRequest(Sender: TObject;
      ClientInfo: TScSSHClientInfo; const Host: String;
      const Port: Integer; var Allow: Boolean);
    procedure ScSSHServerCancelRemotePortForwardingRequest(Sender: TObject;
      ClientInfo: TScSSHClientInfo; const Host: String;
      const Port: Integer);
    procedure ScSSHServerError(Sender: TObject; E: Exception);

  public
    function GetServiceController: TServiceController; override;
  end;

var
  SSHServer: TSSHServerSrv;

implementation

{$R *.DFM}

uses
  WinSock, Registry, ScCLRClasses, ScTypes, ScUtils;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SSHServer.Controller(CtrlCode);
end;

function TSSHServerSrv.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSSHServerSrv.LoadState;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Devart\SecureBridge\SSHServer', False) then begin
      
        if ValueExists('Storage Path') then
          ScFileStorage.Path := ReadString('Storage Path');
        if ValueExists('ListenAddress') then
          ScSSHServer.Options.ListenAddress := ReadString('ListenAddress');
        if ValueExists('ServerPort') then
          ScSSHServer.Port := ReadInteger('ServerPort');
        if ValueExists('Banner') then
          ScSSHServer.Options.Banner := ReadString('Banner');

        if ValueExists('Log file') then
          FLogFilename := ReadString('Log file')
        else
          FLogFilename := '';
        if ValueExists('Log events') then
          FNeedForLog := ReadBool('Log events') and (FLogFilename <> '')
        else
          FNeedForLog := False;

        if ValueExists('ServerCipher_3DES') then
          if ReadBool('ServerCipher_3DES') then
            ScSSHServer.Ciphers := ScSSHServer.Ciphers + [saTripleDES_cbc, saTripleDES_ctr]
          else
            ScSSHServer.Ciphers := ScSSHServer.Ciphers - [saTripleDES_cbc, saTripleDES_ctr];

        if ValueExists('ServerCipher_Blowfish') then
          if ReadBool('ServerCipher_Blowfish') then
            ScSSHServer.Ciphers := ScSSHServer.Ciphers + [saBlowfish_cbc, saBlowfish_ctr]
          else
            ScSSHServer.Ciphers := ScSSHServer.Ciphers - [saBlowfish_cbc, saBlowfish_ctr];

        if ValueExists('ServerCipher_AES128') then
          if ReadBool('ServerCipher_AES128') then
            ScSSHServer.Ciphers := ScSSHServer.Ciphers + [saAES128_cbc, saAES128_ctr]
          else
            ScSSHServer.Ciphers := ScSSHServer.Ciphers - [saAES128_cbc, saAES128_ctr];

        if ValueExists('ServerCipher_AES192') then
          if ReadBool('ServerCipher_AES192') then
            ScSSHServer.Ciphers := ScSSHServer.Ciphers + [saAES192_cbc, saAES192_ctr]
          else
            ScSSHServer.Ciphers := ScSSHServer.Ciphers - [saAES192_cbc, saAES192_ctr];

        if ValueExists('ServerCipher_AES256') then
          if ReadBool('ServerCipher_AES256') then
            ScSSHServer.Ciphers := ScSSHServer.Ciphers + [saAES256_cbc, saAES256_ctr]
          else
            ScSSHServer.Ciphers := ScSSHServer.Ciphers - [saAES256_cbc, saAES256_ctr];

        if ValueExists('ServerCipher_Cast128') then
          if ReadBool('ServerCipher_Cast128') then
            ScSSHServer.Ciphers := ScSSHServer.Ciphers + [saCast128_cbc, saCast128_ctr]
          else
            ScSSHServer.Ciphers := ScSSHServer.Ciphers - [saCast128_cbc, saCast128_ctr];

        if ValueExists('SSHServerAuthentication_PublicKey') then
          if ReadBool('SSHServerAuthentication_PublicKey') then
            ScSSHServer.Authentications := ScSSHServer.Authentications + [atPublicKey]
          else
            ScSSHServer.Authentications := ScSSHServer.Authentications - [atPublicKey];

        if ValueExists('SSHServerAuthentication_Password') then
          if ReadBool('SSHServerAuthentication_Password') then
            ScSSHServer.Authentications := ScSSHServer.Authentications + [atPassword]
          else
            ScSSHServer.Authentications := ScSSHServer.Authentications - [atPassword];

        if ValueExists('RSAKeyName') then
          ScSSHServer.KeyNameRSA := ReadString('RSAKeyName');
        if ScSSHServer.KeyNameRSA <> '' then
          ScSSHServer.HostKeyAlgorithms := ScSSHServer.HostKeyAlgorithms + [aaRSA]
        else
          ScSSHServer.HostKeyAlgorithms := ScSSHServer.HostKeyAlgorithms - [aaRSA];

        if ValueExists('DSAKeyName') then
          ScSSHServer.KeyNameDSA := ReadString('DSAKeyName');
        if ScSSHServer.KeyNameDSA <> '' then
          ScSSHServer.HostKeyAlgorithms := ScSSHServer.HostKeyAlgorithms + [aaDSA]
        else
          ScSSHServer.HostKeyAlgorithms := ScSSHServer.HostKeyAlgorithms - [aaDSA];

        if ValueExists('ClientAliveCountMax') then
          ScSSHServer.Options.ClientAliveCountMax := ReadInteger('ClientAliveCountMax');
        if ValueExists('ClientAliveInterval') then
          ScSSHServer.Options.ClientAliveInterval := ReadInteger('ClientAliveInterval');
        if ValueExists('MaxStartups') then
          ScSSHServer.Options.MaxStartups := ReadInteger('MaxStartups');
        if ValueExists('ServerTimeout') then
          ScSSHServer.Timeout := ReadInteger('ServerTimeout');
      end;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TSSHServerSrv.ServiceCreate(Sender: TObject);
begin
  FLockLog := TCriticalSection.Create;
end;

procedure TSSHServerSrv.ServiceDestroy(Sender: TObject);
begin
  FLockLog.Free;
end;

procedure TSSHServerSrv.ServiceStart(Sender: TService; var Started: Boolean);
begin
  LoadState;

  if FNeedForLog then begin
    ScSSHServer.AfterClientConnect := ScSSHServerAfterClientConnect;
    ScSSHServer.AfterClientDisconnect := ScSSHServerAfterClientDisconnect;
    ScSSHServer.OnClientError := ScSSHServerClientError;
    ScSSHServer.BeforeChannelConnect := ScSSHServerBeforeChannelConnect;
    ScSSHServer.AfterChannelDisconnect := ScSSHServerAfterChannelDisconnect;
    ScSSHServer.BeforeShellConnect := ScSSHServerBeforeShellConnect;
    ScSSHServer.AfterShellDisconnect := ScSSHServerAfterShellDisconnect;
    ScSSHServer.OnChannelError := ScSSHServerChannelError;
    ScSSHServer.OnRemotePortForwardingRequest := ScSSHServerRemotePortForwardingRequest;
    ScSSHServer.OnCancelRemotePortForwardingRequest := ScSSHServerCancelRemotePortForwardingRequest;
    ScSSHServer.OnError := ScSSHServerError;
  end
  else begin
    ScSSHServer.AfterClientConnect := nil;
    ScSSHServer.AfterClientDisconnect := nil;
    ScSSHServer.OnClientError := nil;
    ScSSHServer.BeforeChannelConnect := nil;
    ScSSHServer.AfterChannelDisconnect := nil;
    ScSSHServer.BeforeShellConnect := nil;
    ScSSHServer.AfterShellDisconnect := nil;
    ScSSHServer.OnChannelError := nil;
    ScSSHServer.OnRemotePortForwardingRequest := nil;
    ScSSHServer.OnCancelRemotePortForwardingRequest := nil;
    ScSSHServer.OnError := nil;
  end;

  Log('Starting SSH server.');
  try
    ScSSHServer.Active := True;
    Started := ScSSHServer.Active;
  except
    on e: Exception do begin
      Log('Error on starting SSH server: ' + E.Message);
      raise;
    end;
  end;

  Log('SSH server started.');
end;

procedure TSSHServerSrv.StopSSHServer;
begin
  try
    ScSSHServer.Active := False;
  finally
    Log('SSH server stopped.');
  end;
end;

procedure TSSHServerSrv.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  StopSSHServer;
  Stopped := not ScSSHServer.Active;
end;

procedure TSSHServerSrv.ServiceExecute(Sender: TService);
begin
  while not Terminated do
    ServiceThread.ProcessRequests(True);

  StopSSHServer;
end;

procedure TSSHServerSrv.Log(Message: string);
var
  fs: TFileStream;
  Buf: TBytes;
begin
  if not FNeedForLog then
    Exit;

  fs := nil;
  FLockLog.Enter;
  try
    SetLength(Buf, 0);
    try
      fs := TFileStream.Create(FLogFilename, fmOpenReadWrite + fmShareDenyNone);
    except
      fs := TFileStream.Create(FLogFilename, fmCreate);
    end;

    fs.Seek(0, soFromEnd);
    Message := TimeToStr(Now) + #9 + Message + #13#10;
    Buf := Encoding.Default.GetBytes(Message);
    fs.Write(Buf[0], Length(Buf));
  finally
    fs.Free;
    FLockLog.Leave;
  end;
end;

procedure TSSHServerSrv.ScSSHServerAfterClientConnect(Sender: TObject;
  ClientInfo: TScSSHClientInfo);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' successfully connected.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerAfterClientDisconnect(Sender: TObject;
  ClientInfo: TScSSHClientInfo);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' disconnected.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerClientError(Sender: TObject;
  ClientInfo: TScSSHClientInfo; E: Exception);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' connection error: ' + E.Message;
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerBeforeChannelConnect(Sender: TObject;
  ChannelInfo: TScSSHChannelInfo; var Direct: Boolean);
var
  s, pf: string;
begin
  if ChannelInfo.Remote then
    pf := ' remote port forwarding from '
  else
    pf := ' local port forwarding to ';

  s := 'IP ' + ChannelInfo.Client.TCPConnection.GetRemoteIP +
    ' User ' + ChannelInfo.Client.User + pf +
    ChannelInfo.DestHost + ':' + IntToStr(ChannelInfo.DestPort) + ' established.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerAfterChannelDisconnect(Sender: TObject;
  ChannelInfo: TScSSHChannelInfo);
var
  s, pf: string;
begin
  if ChannelInfo.Remote then
    pf := ' remote port forwarding from '
  else
    pf := ' local port forwarding to ';

  s := 'IP ' + ChannelInfo.Client.TCPConnection.GetRemoteIP +
    ' User ' + ChannelInfo.Client.User + pf +
    ChannelInfo.DestHost + ':' + IntToStr(ChannelInfo.DestPort) + ' closed.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerBeforeShellConnect(Sender: TObject;
  ClientInfo: TScSSHClientInfo);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' session opened.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerAfterShellDisconnect(Sender: TObject;
  ClientInfo: TScSSHClientInfo);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' session closed.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerChannelError(Sender: TObject;
  ChannelInfo: TScSSHChannelInfo; E: Exception);
var
  s, pf: string;
begin
  if ChannelInfo.Remote then
    pf := ' error on remote port forwarding from '
  else
    pf := ' error on local port forwarding to ';

  s := 'IP ' + ChannelInfo.Client.TCPConnection.GetRemoteIP +
    ' User ' + ChannelInfo.Client.User + pf +
    ChannelInfo.DestHost + ':' + IntToStr(ChannelInfo.DestPort) + ': ' + E.Message;
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerRemotePortForwardingRequest(
  Sender: TObject; ClientInfo: TScSSHClientInfo; const Host: String;
  const Port: Integer; var Allow: Boolean);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' request to remote port forwarding from ' +
    Host + ':' + IntToStr(Port) + '.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerCancelRemotePortForwardingRequest(
  Sender: TObject; ClientInfo: TScSSHClientInfo; const Host: String;
  const Port: Integer);
var
  s: string;
begin
  s := 'IP ' + ClientInfo.TCPConnection.GetRemoteIP +
    ' User ' + ClientInfo.User + ' request to cancel remote port forwarding from ' +
    Host + ':' + IntToStr(Port) + '.';
  Log(s);
end;

procedure TSSHServerSrv.ScSSHServerError(Sender: TObject; E: Exception);
begin
  Log('Server error: ' + E.Message);
end;

end.

