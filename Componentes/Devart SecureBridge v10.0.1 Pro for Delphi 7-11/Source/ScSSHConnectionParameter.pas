
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSHConnectionParameter;

interface

uses
  Classes, ScVio, ScUtils, ScSSHUtils, ScRNG, ScBridge;

type
  // Fill the properties of ConnectionParameter object before you start the connection.
  TSshConnectionParameters = class (TPersistent)
  private
    FRandom: IScRandom;
    FProtocol: TScSSHProtocol;
    FKeyExchangeAlgorithms: string;
    FAllowedShell: Boolean;
    FPreferableHostKeyAlgorithms: string;
    FCompressionClientAlgorithms: string;
    FCompressionServerAlgorithms: string;
    FAuthenticationType: TScSSHAuthentication;
    FServerAuthenticationsType: TScSSHAuthentications;
    FUserName: string;
    FPassword: string;
    FTerminalName: string;
    FTerminalWidth: Integer;
    FTerminalHeight: Integer;
    FTerminalPixelWidth: Integer;
    FTerminalPixelHeight: Integer;
    FCheckMACError: Boolean;
    FWindowSize: Integer;
    FMaxPacketSize: Integer;
    FRekeyLimitSize: Int64;
    FAliveCountMax: integer;
    FAliveInterval: integer;
    FBanner: string;
    FOnBanner: TScBannerEvent;
    FOnServerKeyValidation: TScServerKeyValidationEvent;
    FClientKey: TScKey;
    FServerKey: TScKey;
    FHostKeys: array [TScAsymmetricAlgorithm] of TScKey;
    FOnCheckUserPass: TScCheckUserPass;
    FOnCheckUserKey: TScCheckUserKey;
    FConnectionInfo: TScSSHClientInfo;
    FClientVersion: string;
    FServerVersion: string;
    FMsgIgnoreRate: Integer;
    FIPVersion: TIPVersion;

    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnError: TScErrorEvent;
    FOnRemotePortForwardingRequest: TScOnRemotePortForwardingRequest;
    FOnCancelRemotePortForwardingRequest: TScOnCancelRemotePortForwardingRequest;
    FOnEstablishPortForwarding: TScOnEstablishPortForwarding;
    FOnEstablishRemotePortForwarding: TScOnEstablishRemotePortForwarding;
    FOnCancelRemotePortForwarding: TScOnEstablishRemotePortForwarding;
    FOnEstablishSession: TScOnEstablishSession;
    FOnOpenSession: TScOnOpenSession;
    FOnAuthenticationPrompt: TScAuthenticationPromptEvent;
    FOnGetDHEGroup: TScOnGetDHEGroup;

    function GetHostKeys(Index: TScAsymmetricAlgorithm): TScKey;
    procedure SetHostKeys(Index: TScAsymmetricAlgorithm; Value: TScKey);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;
    destructor Destroy; override;

    property Protocol: TScSSHProtocol read FProtocol write FProtocol;
    property KeyExchangeAlgorithms: string read FKeyExchangeAlgorithms write FKeyExchangeAlgorithms;
    property AllowedShell: Boolean read FAllowedShell write FAllowedShell;
    property PreferableHostKeyAlgorithms: string read FPreferableHostKeyAlgorithms write FPreferableHostKeyAlgorithms;
    property CompressionClientAlgorithms: string read FCompressionClientAlgorithms write FCompressionClientAlgorithms;
    property CompressionServerAlgorithms: string read FCompressionServerAlgorithms write FCompressionServerAlgorithms;
    property AuthenticationType: TScSSHAuthentication read FAuthenticationType write FAuthenticationType;
    property ServerAuthenticationsType: TScSSHAuthentications read FServerAuthenticationsType write FServerAuthenticationsType;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property TerminalName: string read FTerminalName write FTerminalName;
    property TerminalWidth: Integer read FTerminalWidth write FTerminalWidth;
    property TerminalHeight: Integer read FTerminalHeight write FTerminalHeight;
    property TerminalPixelWidth: Integer read FTerminalPixelWidth write FTerminalPixelWidth;
    property TerminalPixelHeight: Integer read FTerminalPixelHeight write FTerminalPixelHeight;
    property Random: IScRandom read FRandom write FRandom;
    property CheckMACError: Boolean read FCheckMACError write FCheckMACError;
    property WindowSize: Integer read FWindowSize write FWindowSize;
    property MaxPacketSize: Integer read FMaxPacketSize write FMaxPacketSize;
    property RekeyLimitSize: Int64 read FRekeyLimitSize write FRekeyLimitSize;
    property AliveCountMax: integer read FAliveCountMax write FAliveCountMax;
    property AliveInterval: integer read FAliveInterval write FAliveInterval;
    property Banner: string read FBanner write FBanner;
    property OnBanner: TScBannerEvent read FOnBanner write FOnBanner;
    property OnServerKeyValidation: TScServerKeyValidationEvent read FOnServerKeyValidation write FOnServerKeyValidation;
    property ClientKey: TScKey read FClientKey write FClientKey;
    property ServerKey: TScKey read FServerKey write FServerKey;
    property HostKeys[Index: TScAsymmetricAlgorithm]: TScKey read GetHostKeys write SetHostKeys;
    property OnCheckUserPass: TScCheckUserPass read FOnCheckUserPass write FOnCheckUserPass;
    property OnCheckUserKey: TScCheckUserKey read FOnCheckUserKey write FOnCheckUserKey;
    property ConnectionInfo: TScSSHClientInfo read FConnectionInfo write FConnectionInfo;
    property ClientVersion: string read FClientVersion write FClientVersion;
    property ServerVersion: string read FServerVersion write FServerVersion;
    property MsgIgnoreRate: Integer read FMsgIgnoreRate write FMsgIgnoreRate;
    property IPVersion: TIPVersion read FIPVersion write FIPVersion;

    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property OnError: TScErrorEvent read FOnError write FOnError;
    property OnRemotePortForwardingRequest: TScOnRemotePortForwardingRequest
      read FOnRemotePortForwardingRequest write FOnRemotePortForwardingRequest;
    property OnCancelRemotePortForwardingRequest: TScOnCancelRemotePortForwardingRequest
      read FOnCancelRemotePortForwardingRequest write FOnCancelRemotePortForwardingRequest;
    property OnEstablishPortForwarding: TScOnEstablishPortForwarding
      read FOnEstablishPortForwarding write FOnEstablishPortForwarding;
    property OnEstablishRemotePortForwarding: TScOnEstablishRemotePortForwarding
      read FOnEstablishRemotePortForwarding write FOnEstablishRemotePortForwarding;
    property OnCancelRemotePortForwarding: TScOnEstablishRemotePortForwarding
      read FOnCancelRemotePortForwarding write FOnCancelRemotePortForwarding;
    property OnEstablishSession: TScOnEstablishSession read FOnEstablishSession write FOnEstablishSession;
    property OnOpenSession: TScOnOpenSession read FOnOpenSession write FOnOpenSession;

    property OnAuthenticationPrompt: TScAuthenticationPromptEvent read FOnAuthenticationPrompt write FOnAuthenticationPrompt;
    property OnGetDHEGroup: TScOnGetDHEGroup read FOnGetDHEGroup write FOnGetDHEGroup;
  end;

implementation

uses
  ScAlgorithmSupport, ScConsts, ScFunctions;

constructor TSshConnectionParameters.Create;
begin
  inherited;

  FProtocol := pSSH2;
  FAuthenticationType := atPassword;
  FTerminalName := 'vt100';
  FTerminalWidth := 80;
  FTerminalHeight := 25;
  FTerminalPixelWidth := 640;
  FTerminalPixelHeight := 480;

  FKeyExchangeAlgorithms := '';
  FPreferableHostKeyAlgorithms := ''; //CipherFactory.PublicKeyAlgorithmToSSH2Name(pkaDSA);
  FMaxPacketSize := SSH_CHANNEL_PACKET_SIZE;
  FWindowSize := 8 * SSH_CHANNEL_PACKET_SIZE;
  FCheckMACError := True;
  FRekeyLimitSize := 1024 * 1024 * 1024; // 1G
  FAliveCountMax := 3;
  FAliveInterval := 0;
  FAllowedShell := True;
end;

destructor TSshConnectionParameters.Destroy;
begin
  FPassword := '';
  inherited;
end;

procedure TSshConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TSshConnectionParameters) then begin
    TSshConnectionParameters(Dest).FRandom := FRandom;
    TSshConnectionParameters(Dest).FKeyExchangeAlgorithms := FKeyExchangeAlgorithms;
    TSshConnectionParameters(Dest).FAllowedShell := FAllowedShell;
    TSshConnectionParameters(Dest).FPreferableHostKeyAlgorithms := FPreferableHostKeyAlgorithms;
    TSshConnectionParameters(Dest).FCompressionClientAlgorithms := FCompressionClientAlgorithms;
    TSshConnectionParameters(Dest).FCompressionServerAlgorithms := FCompressionServerAlgorithms;
    TSshConnectionParameters(Dest).FAuthenticationType := FAuthenticationType;
    TSshConnectionParameters(Dest).FServerAuthenticationsType := FServerAuthenticationsType;
    TSshConnectionParameters(Dest).FTerminalWidth := FTerminalWidth;
    TSshConnectionParameters(Dest).FTerminalHeight := FTerminalHeight;
    TSshConnectionParameters(Dest).FTerminalPixelWidth := FTerminalPixelWidth;
    TSshConnectionParameters(Dest).FTerminalPixelHeight := FTerminalPixelHeight;
    TSshConnectionParameters(Dest).FMaxPacketSize := FMaxPacketSize;
    TSshConnectionParameters(Dest).FPassword := FPassword;
    TSshConnectionParameters(Dest).FProtocol := FProtocol;
    TSshConnectionParameters(Dest).FTerminalName := FTerminalName;
    TSshConnectionParameters(Dest).FUserName := FUserName;
    TSshConnectionParameters(Dest).FWindowSize := FWindowSize;
    TSshConnectionParameters(Dest).FCheckMACError := FCheckMACError;
    TSshConnectionParameters(Dest).FRekeyLimitSize := FRekeyLimitSize;
    TSshConnectionParameters(Dest).FAliveCountMax := FAliveCountMax;
    TSshConnectionParameters(Dest).FAliveInterval := FAliveInterval;
    TSshConnectionParameters(Dest).FBanner := FBanner;
    TSshConnectionParameters(Dest).FOnBanner := FOnBanner;
    TSshConnectionParameters(Dest).FOnServerKeyValidation := FOnServerKeyValidation;
    TSshConnectionParameters(Dest).FClientKey := FClientKey;
    TSshConnectionParameters(Dest).FServerKey := FServerKey;
    TSshConnectionParameters(Dest).FHostKeys := FHostKeys;
    TSshConnectionParameters(Dest).FOnCheckUserPass := FOnCheckUserPass;
    TSshConnectionParameters(Dest).FOnCheckUserKey := FOnCheckUserKey;
    TSshConnectionParameters(Dest).FConnectionInfo := FConnectionInfo;
    TSshConnectionParameters(Dest).FClientVersion := FClientVersion;
    TSshConnectionParameters(Dest).FServerVersion := FServerVersion;
    TSshConnectionParameters(Dest).FMsgIgnoreRate := FMsgIgnoreRate;
    TSshConnectionParameters(Dest).FIPVersion := FIPVersion;

    TSshConnectionParameters(Dest).FAfterConnect := FAfterConnect;
    TSshConnectionParameters(Dest).FAfterDisconnect := FAfterDisconnect;
    TSshConnectionParameters(Dest).FOnError := FOnError;
    TSshConnectionParameters(Dest).FOnRemotePortForwardingRequest := FOnRemotePortForwardingRequest;
    TSshConnectionParameters(Dest).FOnCancelRemotePortForwardingRequest := FOnCancelRemotePortForwardingRequest;
    TSshConnectionParameters(Dest).FOnEstablishPortForwarding := FOnEstablishPortForwarding;
    TSshConnectionParameters(Dest).FOnEstablishRemotePortForwarding := FOnEstablishRemotePortForwarding;
    TSshConnectionParameters(Dest).FOnCancelRemotePortForwarding := FOnCancelRemotePortForwarding;
    TSshConnectionParameters(Dest).FOnEstablishSession := FOnEstablishSession;
    TSshConnectionParameters(Dest).FOnOpenSession := FOnOpenSession;
    TSshConnectionParameters(Dest).FOnAuthenticationPrompt := FOnAuthenticationPrompt;
    TSshConnectionParameters(Dest).FOnGetDHEGroup := FOnGetDHEGroup;
  end
  else
    inherited;
end;

function TSshConnectionParameters.GetHostKeys(Index: TScAsymmetricAlgorithm): TScKey;
begin
  Result := FHostKeys[Index];
end;

procedure TSshConnectionParameters.SetHostKeys(Index: TScAsymmetricAlgorithm; Value: TScKey);
begin
  if Value <> nil then begin
    Value.Ready := True;
    if Value.Algorithm <> Index then
      raise EScError.CreateFmt(SBadKeyAlgorithm, [Value.KeyName, CipherFactory.PublicKeyAlgorithmToSSH2Name(Index)], seBadKeyAlgorithm);
  end;
  FHostKeys[Index] := Value;
end;

end.

