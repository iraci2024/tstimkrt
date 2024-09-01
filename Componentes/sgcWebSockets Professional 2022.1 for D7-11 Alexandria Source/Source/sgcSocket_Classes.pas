{ ***************************************************************************
  sgcSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcSocket_Classes;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // indy
  {$IFDEF SGC_INDY}sgcIdLogFile{$ELSE}IdLogFile{$ENDIF},
  // sgc
  sgcBase_Classes,
  // sgcWebSocket
  sgcWebSocket_Classes_SyncObjs, sgcWebSocket_Types;

type

  TsgcBytes = array of Byte;

  TsgcSocketKeepAlive = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FTime: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
    property Time: Integer read FTime write FTime;
  end;

  TsgcSocketConnection_Base = class
    { critical section }
  protected
    FCS: TsgcCriticalSection;
    FCSArray: Array [0 .. 5] of TsgcCriticalSection;
  protected
    procedure DoEnterCS; overload; virtual;
    procedure DoEnterCS(const aIndex: Integer); overload; virtual;
    procedure DoLeaveCS; overload; virtual;
    procedure DoLeaveCS(const aIndex: Integer); overload; virtual;
    { critical section }

    { connection }
  protected
    function GetActive: Boolean; virtual; abstract;
  public
    property Active: Boolean read GetActive;
    { connection }

    { properties }
  private
    FIsInternal: Boolean;
  protected
    FGuid: String;
    FRecBytes: Int64;
    FSendBytes: Int64;
    FIP: String;
    FLocalIP: String;
    FLocalPort: Integer;
    FPort: Integer;
    FIPVersion: TwsIPVersion;
    FTransport: TwsTransport;
    function GetGuid: String; virtual;
    function GetIP: String; virtual; abstract;
    function GetLocalIP: String; virtual; abstract;
    function GetLocalPort: Integer; virtual; abstract;
    function GetPort: Integer; virtual; abstract;
    function GetIPVersion: TwsIPVersion; virtual; abstract;
    procedure SetTransport(const Value: TwsTransport); virtual;
  protected
    property IsInternal: Boolean read FIsInternal write FIsInternal;
  public
    property Guid: String read GetGuid;
    property IP: String read GetIP;
    property LocalIP: String read GetLocalIP;
    property LocalPort: Integer read GetLocalPort;
    property Port: Integer read GetPort;
    property IPVersion: TwsIPVersion read GetIPVersion;
    property RecBytes: Int64 read FRecBytes write FRecBytes;
    property SendBytes: Int64 read FSendBytes write FSendBytes;
    property Transport: TwsTransport read FTransport write SetTransport;
    { properties }

    { disconnect }
  private
    FDisconnected: Boolean;
  public
    property Disconnected: Boolean read FDisconnected write FDisconnected;
    { disconnect }

    { socket fields }
  private
    FRawException: Exception;
  public
    property RawException: Exception read FRawException write FRawException;
    { socket fields }

    { user session data }
  private
    FData: TObject;
  public
    property Data: TObject read FData write FData;
    { user session data }

    { constructor }
  protected
    FFreed: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Free;
    { constructor }
  end;

  TsgcSocketConnection = class(TsgcSocketConnection_Base)

  end;

  TsgcSocketComponent_Base = class(TsgcComponent_Base)
    { critical sections }
  private
    FCSArray: Array [0 .. 5] of TsgcCriticalSection;
    FCS: TsgcCriticalSection;

  protected
    procedure DoEnterCS; overload; virtual;
    procedure DoEnterCS(const aIndex: Integer); overload; virtual;
    procedure DoLeaveCS; overload; virtual;
    procedure DoLeaveCS(const aIndex: Integer); overload; virtual;
    function DoAssignedCS: Boolean; overload; virtual;
    function DoAssignedCS(const aIndex: Integer): Boolean; overload; virtual;
    { critical sections }

    { properties }
  protected
    FNotifyEvents: TwsNotifyEvent;
  protected
    procedure SetNotifyEvents(const Value: TwsNotifyEvent); virtual;
  protected
    property NotifyEvents: TwsNotifyEvent read FNotifyEvents
      write SetNotifyEvents;
    { properties }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { methods }
  protected
    procedure DoDestroyConnection(aConnection: TsgcSocketConnection); virtual;
    { methods }
  end;


  TsgcSocketLogFile = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcSocketThrottle = class(TPersistent)
  private
    FBitsPerSec: Integer;
    FEnabled: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property BitsPerSec: Integer read FBitsPerSec write FBitsPerSec;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TsgcSocketOpenSSL_Options = class(TPersistent)
  private
    FAPIVersion: TwsOpenSSLAPI;
    FLibPathCustom: string;
    FLibPath: TwsOpenSSLLibPath;
    FUnixSymLinks: TwsOpenSSLSymLinks;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property APIVersion: TwsOpenSSLAPI read FAPIVersion write FAPIVersion;
    property LibPathCustom: string read FLibPathCustom write FLibPathCustom;
    property LibPath: TwsOpenSSLLibPath read FLibPath write FLibPath;
    property UnixSymLinks: TwsOpenSSLSymLinks read FUnixSymLinks write
        FUnixSymLinks;
  end;

  TsgcSocketSChannel_Options = class(TPersistent)
  private
    FCertHash: String;
    FCertStoreName: TsgcSChannelCertStoreName;
    FCertStorePath: TsgcSChannelCertStorePath;
    FCipherList: string;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property CertStoreName: TsgcSChannelCertStoreName read FCertStoreName write
        FCertStoreName;
    property CertStorePath: TsgcSChannelCertStorePath read FCertStorePath write
        FCertStorePath;
    property CertHash: String read FCertHash write FCertHash;
    property CipherList: string read FCipherList write FCipherList;
  end;

  TsgcSocketTLS_Options_Base = class(TPersistent)
  private
    FCertFile: String;
    FKeyFile: String;
    FPassword: {$IFDEF INDY10_6}String{$ELSE}AnsiString{$ENDIF};
    FRootCertFile: String;
    FVerifyCertificate: Boolean;
    FVerifyDepth: Integer;
    FVersion: TwsTLSVersions;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property CertFile: String read FCertFile write FCertFile;
    property KeyFile: String read FKeyFile write FKeyFile;
    property Password:
{$IFDEF INDY10_6}String{$ELSE}AnsiString{$ENDIF} read FPassword write FPassword;
    property RootCertFile: String read FRootCertFile write FRootCertFile;
    property VerifyCertificate: Boolean read FVerifyCertificate
      write FVerifyCertificate;
    property VerifyDepth: Integer read FVerifyDepth write FVerifyDepth;
    property Version: TwsTLSVersions read FVersion write FVersion;
  end;

  TsgcOpenSSLClient_Options = class(TsgcSocketOpenSSL_Options)

  end;

  TsgcSChannelClient_Options = class(TsgcSocketSChannel_Options)

  end;

  TsgcSocketTLS_Options = class(TsgcSocketTLS_Options_Base)
  private
    FALPNProtocols: TStringList;
    FIOHandler: TwsTLSIOHandler;
    FOpenSSL_Options: TsgcOpenSSLClient_Options;
    FSChannel_Options: TsgcSChannelClient_Options;
    procedure SetALPNProtocols(const Value: TStringList);
    procedure SetOpenSSL_Options(const Value: TsgcOpenSSLClient_Options);
    procedure SetSChannel_Options(const Value: TsgcSChannelClient_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ALPNProtocols: TStringList read FALPNProtocols
      write SetALPNProtocols;
    property IOHandler: TwsTLSIOHandler read FIOHandler write FIOHandler;
    property OpenSSL_Options: TsgcOpenSSLClient_Options read FOpenSSL_Options
      write SetOpenSSL_Options;
    property SChannel_Options: TsgcSChannelClient_Options read FSChannel_Options write SetSChannel_Options;
  end;

  TsgcSocketProxy_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FHost: String;
    FPassword: String;
    FPort: Integer;
    FProxyType: TwsProxy;
    FUsername: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: String read FHost write FHost;
    property Password: String read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property ProxyType: TwsProxy read FProxyType write FProxyType;
    property Username: String read FUsername write FUsername;
  end;

  TsgcSocketWatchDog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
  protected
    FAttempts: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Attempts: Integer read FAttempts write FAttempts;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
  end;

  TsgcSocketHeartBeat_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
  end;



implementation

uses
  sgcBase_Helpers;

procedure TsgcSocketKeepAlive.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketKeepAlive then
  begin
    Enabled := TsgcSocketKeepAlive(aSource).Enabled;
    Interval := TsgcSocketKeepAlive(aSource).Interval;
    Time := TsgcSocketKeepAlive(aSource).Time;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketConnection_Base.Create;
var
  i: Integer;
begin
  FCS := TsgcCriticalSection.Create;
  for i := 0 to Length(FCSArray) - 1 do
    FCSArray[i] := TsgcCriticalSection.Create;
  FData := nil;
  FRecBytes := 0;
  FSendBytes := 0;
end;

destructor TsgcSocketConnection_Base.Destroy;
var
  i: integer;
begin
  // sgcFree(FData); // user data, don't try to free
  sgcFree(FCS);
  for i := 0 to Length(FCSArray) - 1 do
    sgcFree(FCSArray[i]);
  inherited;
end;

procedure TsgcSocketConnection_Base.DoEnterCS;
begin
  FCS.Acquire;
end;

procedure TsgcSocketConnection_Base.DoEnterCS(const aIndex: Integer);
begin
  FCSArray[aIndex].Acquire;
end;

procedure TsgcSocketConnection_Base.DoLeaveCS;
begin
  FCS.Release;
end;

procedure TsgcSocketConnection_Base.DoLeaveCS(const aIndex: Integer);
begin
  FCSArray[aIndex].Release;
end;

procedure TsgcSocketConnection_Base.Free;
begin
  if Self <> nil then
  begin
    FFreed := True;

    Destroy;
  end;
end;

function TsgcSocketConnection_Base.GetGuid: String;
begin
  if Assigned(Self) then
  begin
    if FGuid = '' then
      FGuid := NewGuid;
    Result := FGuid;
  end;
end;

procedure TsgcSocketConnection_Base.SetTransport(const Value: TwsTransport);
begin
  FTransport := Value;
end;

procedure TsgcSocketComponent_Base.DoEnterCS;
begin
  FCS.Acquire;
end;

procedure TsgcSocketComponent_Base.DoEnterCS(const aIndex: Integer);
begin
  FCSArray[aIndex].Acquire;
end;

procedure TsgcSocketComponent_Base.DoLeaveCS;
begin
  FCS.Release;
end;

procedure TsgcSocketComponent_Base.DoLeaveCS(const aIndex: Integer);
begin
  FCSArray[aIndex].Release;
end;

procedure TsgcSocketComponent_Base.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  FNotifyEvents := Value;
end;

constructor TsgcSocketComponent_Base.Create(aOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FCS := TsgcCriticalSection.Create;
  for i := 0 to Length(FCSArray) - 1 do
    FCSArray[i] := TsgcCriticalSection.Create;
{$IFDEF CONSOLE}
  FNotifyEvents := neNoSync;
{$ELSE}
  {$IFDEF MSWINDOWS}
  if IsLibrary then
    FNotifyEvents := neNoSync
  else
    FNotifyEvents := neAsynchronous;
  {$ELSE}
  FNotifyEvents := neAsynchronous;
  {$ENDIF}
{$ENDIF}
end;

destructor TsgcSocketComponent_Base.Destroy;
var
  i: Integer;
begin
  sgcFree(FCS);
  for i := 0 to Length(FCSArray) - 1 do
    sgcFree(FCSArray[i]);
  inherited;
end;

function TsgcSocketComponent_Base.DoAssignedCS: Boolean;
begin
  Result := Assigned(FCS);
end;

function TsgcSocketComponent_Base.DoAssignedCS(const aIndex: Integer): Boolean;
begin
  Result := Assigned(FCSArray[aIndex]);
end;

procedure TsgcSocketComponent_Base.DoDestroyConnection(aConnection:
    TsgcSocketConnection);
begin
  if Assigned(aConnection) then
    aConnection.FFreed := True;
  sgcFree(aConnection);
end;

procedure TsgcSocketLogFile.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketLogFile then
  begin
    FEnabled := TsgcSocketLogFile(aSource).Enabled;
    FileName := TsgcSocketLogFile(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketLogFile.Create;
begin
  Enabled := False;
end;

constructor TsgcSocketThrottle.Create;
begin
  inherited;
  Enabled := False;
end;

procedure TsgcSocketThrottle.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketThrottle then
  begin
    FEnabled := TsgcSocketThrottle(aSource).Enabled;
    BitsPerSec := TsgcSocketThrottle(aSource).BitsPerSec;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketOpenSSL_Options.Create;
begin
  inherited;
  APIVersion := oslAPI_1_0;
  LibPath := oslpNone;
  LibPathCustom := '';
end;

procedure TsgcSocketOpenSSL_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketOpenSSL_Options then
  begin
    APIVersion := TsgcSocketOpenSSL_Options(aSource).APIVersion;
    LibPath := TsgcSocketOpenSSL_Options(aSource).LibPath;
    LibPathCustom := TsgcSocketOpenSSL_Options(aSource).LibPathCustom;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketTLS_Options_Base.Create;
begin
  inherited;
  VerifyCertificate := False;
  VerifyDepth := 0;
end;

procedure TsgcSocketTLS_Options_Base.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketTLS_Options_Base then
  begin
    CertFile := TsgcSocketTLS_Options_Base(aSource).CertFile;
    KeyFile := TsgcSocketTLS_Options_Base(aSource).KeyFile;
    Password := TsgcSocketTLS_Options_Base(aSource).Password;
    RootCertFile := TsgcSocketTLS_Options_Base(aSource).RootCertFile;
    Version := TsgcSocketTLS_Options_Base(aSource).Version;
    VerifyCertificate := TsgcSocketTLS_Options_Base(aSource).VerifyCertificate;
    VerifyDepth := TsgcSocketTLS_Options_Base(aSource).VerifyDepth;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketTLS_Options.Create;
begin
  inherited;
  FOpenSSL_Options := TsgcOpenSSLClient_Options.Create;
  FSChannel_Options := TsgcSChannelClient_Options.Create;
  FALPNProtocols := TStringList.Create;
  IOHandler := iohOpenSSL;
end;

destructor TsgcSocketTLS_Options.Destroy;
begin
  sgcFree(FSChannel_Options);
  sgcFree(FOpenSSL_Options);
  sgcFree(FALPNProtocols);
  inherited;
end;

procedure TsgcSocketTLS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketTLS_Options then
  begin
    IOHandler := TsgcSocketTLS_Options(aSource).IOHandler;
    ALPNProtocols := TsgcSocketTLS_Options(aSource).ALPNProtocols;
    OpenSSL_Options := TsgcSocketTLS_Options(aSource).OpenSSL_Options;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcSocketTLS_Options.SetALPNProtocols(const Value: TStringList);
begin
  if Assigned(FALPNProtocols) then
    FALPNProtocols.Assign(Value);
end;

procedure TsgcSocketTLS_Options.SetOpenSSL_Options(const Value
  : TsgcOpenSSLClient_Options);
begin
  FOpenSSL_Options.Assign(Value);
end;

procedure TsgcSocketTLS_Options.SetSChannel_Options(const Value:
    TsgcSChannelClient_Options);
begin
  FSChannel_Options.Assign(Value);
end;

constructor TsgcSocketProxy_Options.Create;
begin
  inherited;
  ProxyType := pxyHTTP;
end;

procedure TsgcSocketProxy_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketProxy_Options then
  begin
    Enabled := TsgcSocketProxy_Options(aSource).Enabled;
    Host := TsgcSocketProxy_Options(aSource).Host;
    Port := TsgcSocketProxy_Options(aSource).Port;
    Username := TsgcSocketProxy_Options(aSource).Username;
    Password := TsgcSocketProxy_Options(aSource).Password;
    ProxyType := TsgcSocketProxy_Options(aSource).ProxyType;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketWatchDog_Options.Create;
begin
  inherited;
  Enabled := False;
  Interval := 60;
  Attempts := 0;
end;

procedure TsgcSocketWatchDog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketWatchDog_Options then
  begin
    Enabled := TsgcSocketWatchDog_Options(aSource).Enabled;
    Interval := TsgcSocketWatchDog_Options(aSource).Interval;
    Attempts := TsgcSocketWatchDog_Options(aSource).Attempts;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketHeartBeat_Options.Create;
begin
  inherited;
  FEnabled := False;
  FInterval := 60;
end;

procedure TsgcSocketHeartBeat_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketHeartBeat_Options then
  begin
    Enabled := TsgcSocketHeartBeat_Options(aSource).Enabled;
    Interval := TsgcSocketHeartBeat_Options(aSource).Interval;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSocketSChannel_Options.Create;
begin
  inherited;
  FCertHash := '';
  FCipherList := '';
end;

procedure TsgcSocketSChannel_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSocketSChannel_Options then
  begin
    CertHash := TsgcSocketSChannel_Options(aSource).CertHash;
    CertStoreName := TsgcSocketSChannel_Options(aSource).CertStoreName;
    CertStorePath := TsgcSocketSChannel_Options(aSource).CertStorePath;
    CipherList := TsgcSocketSChannel_Options(aSource).CipherList;
  end
  else
    inherited Assign(aSource);
end;

end.
