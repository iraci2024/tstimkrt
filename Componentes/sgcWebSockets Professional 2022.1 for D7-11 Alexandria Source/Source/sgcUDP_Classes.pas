{ ***************************************************************************
  sgcUDP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  ***************************************************************************5 / }

unit sgcUDP_Classes;

interface

{$I sgcVer.inc}
{$IFDEF SGC_UDP}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcSocket_Classes, sgcSocket_Classes_Indy, sgcWebSocket_Types,
  sgcBase_Helpers,
  sgcBase_Classes,
  // sgcWebSocket
  sgcWebSocket_Classes_SyncObjs, sgcWebSocket_Classes_Queues;

type
  TsgcUDPSocketExceptionEvent = procedure(Sender: TObject; const E: Exception)
    of object;
  TsgcUDPRetransmissionRetryEvent = procedure(Sender: TObject; const Bytes:
      TBytes; const aIPAddress: string; aPort: Word) of object;
  TsgcUDPRetransmissionTimeoutEvent = procedure(Sender: TObject; const
      aBytes: TBytes; const aIPAddress: string; aPort: Word) of object;

  TsgcUDPSocket = class;

  TsgcUDPConnectEvent = procedure(Sender: TObject; Socket: TsgcUDPSocket)
    of object;
  TsgcUDPDisconnectEvent = procedure(Sender: TObject; Socket: TsgcUDPSocket)
    of object;
  TsgcUDPReadEvent = procedure(Sender: TObject; Socket: TsgcUDPSocket;
    Bytes: TsgcBytes) of object;
  TsgcUDPExceptionEvent = procedure(Sender: TObject; Socket: TsgcUDPSocket;
    E: Exception) of object;

  TsgcUDPKeepAlive = class(TsgcSocketKeepAlive)

  end;

  TsgcIdUDPLogFile = class(TsgcIdLogFileBase)

  end;

  TsgcUDPRetransmissionItem = class(TsgcQueueItemBase)
  private
    FBytes: TBytes;
    FIPAddress: string;
    FMaxRetries: Integer;
    FNextRetry: TDateTime;
    FPort: Word;
    FRetries: Integer;
    FRTO: Integer;
    procedure SetBytes(const Value: TBytes);
  public
    constructor Create; override;
  public
    property Bytes: TBytes read FBytes write SetBytes;
    property IPAddress: string read FIPAddress write FIPAddress;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property NextRetry: TDateTime read FNextRetry write FNextRetry;
    property Port: Word read FPort write FPort;
    property Retries: Integer read FRetries write FRetries;
    property RTO: Integer read FRTO write FRTO;
  end;

  TsgcUDPRetransmissionQueue = class(TsgcQueueBase)
  end;

  TsgcUDPRetransmissions = class
    { timer }
  private
    FTimer: TsgcTimer;
    function GetTimer: TsgcTimer;
  protected
    procedure OnTimerEvent(Sender: TObject); virtual;
    procedure OnExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    property Timer: TsgcTimer read GetTimer write FTimer;
    { timer }

    { queue }
  private
    FQueue: TsgcUDPRetransmissionQueue;
    function GetEnabled: Boolean;
    function GetQueue: TsgcUDPRetransmissionQueue;
    procedure SetEnabled(const Value: Boolean);
  protected
    property Queue: TsgcUDPRetransmissionQueue read GetQueue write FQueue;
    { queue }

    { constructor / destructor }
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  protected
    procedure DoRetransmissions;
  public
    procedure AddItem(const aItem: TsgcUDPRetransmissionItem);
    function DeleteItem(const aID: String): Boolean;
    { methods }

    { properties }
  private
    FMaxRetries: Integer;
    FRTO: Integer;
  public
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RTO: Integer read FRTO write FRTO;
    { properties }

    { events }
  private
    FOnRetry: TsgcUDPRetransmissionRetryEvent;
    FOnTimeout: TsgcUDPRetransmissionTimeoutEvent;
    FOnException: TsgcTimerOnException;
  public
    property OnRetry: TsgcUDPRetransmissionRetryEvent read FOnRetry
      write FOnRetry;
    property OnTimeout: TsgcUDPRetransmissionTimeoutEvent read FOnTimeout
      write FOnTimeout;
    property OnException: TsgcTimerOnException read FOnException
      write FOnException;
    { events }
  end;

  TsgcUDPRetransmission_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FMaxRetries: Integer;
    FRTO: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RTO: Integer read FRTO write FRTO;
  end;

  TsgcUDPSocket_Base = class(TsgcSocketConnection)
    { frame }
  private
    FUDPEndOfFrameScanBuffer: TtcpEOFScanBuffer;
  protected
    FUDPEndOfFrame: TBytes;
  public
    procedure AddUDPEndOfFrame(aByte: Byte);
    procedure ClearUDPEndOfFrame;
  public
    property UDPEndOfFrameScanBuffer: TtcpEOFScanBuffer
      read FUDPEndOfFrameScanBuffer write FUDPEndOfFrameScanBuffer;
    { frame }

    { fields }
  protected
    FIP: string;
    FLocalIP: string;
    FPort: Integer;
    FLocalPort: Integer;
    FIPVersion: TwsIPVersion;
  public
    property IP: string read FIP write FIP;
    property LocalIP: string read FLocalIP write FLocalIP;
    property Port: Integer read FPort write FPort;
    property LocalPort: Integer read FLocalPort write FLocalPort;
    property IPVersion: TwsIPVersion read FIPVersion write FIPVersion;
    { fields }

    { destructor }
  public
    destructor Destroy; override;
    { destructor }

    { log file }
  protected
    FLogFile: TsgcIdUDPLogFile;
  protected
    procedure DoLogSendString(const aValue: string); virtual;
    procedure DoLogRecvString(const aValue: string); virtual;
    { log file }

    { log data }
  protected
    procedure DoLogData(const aBuffer: TBytes); virtual;
  public
    procedure LogData(const aBuffer: TBytes);
    { log data }

    { write data }
  protected
    procedure DoWriteData(const aIPAddress: string; aPort: Word; const aValue:
        string); overload; virtual;
    procedure DoWriteData(const aIPAddress: string; aPort: Word; const aValue:
        TBytes); overload; virtual;
  protected
    procedure DoWriteData(const aValue: string); overload; virtual;
    procedure DoWriteData(const aValue: TBytes); overload; virtual;
  public
    procedure WriteData(const aIPAddress: string; aPort: Word; const aValue:
        string); overload;
    procedure WriteData(const aIPAddress: string; aPort: Word; const aValue:
        TBytes); overload;
  public
    procedure WriteData(const aValue: string); overload;
    procedure WriteData(const aValue: TBytes); overload;
    { write data }

    { events }
  private
    FOnException: TsgcUDPSocketExceptionEvent;
  protected
    procedure DoException(const E: Exception);
  protected
    property OnException: TsgcUDPSocketExceptionEvent read FOnException
      write FOnException;
    { events }
  end;

  TsgcUDPSocket = class(TsgcUDPSocket_Base)
  protected
    FMsgRead: TBytes;
  public
    property MsgRead: TBytes read FMsgRead write FMsgRead;

  public
    procedure Assign(const aSource: TsgcUDPSocket); virtual;
  end;

  TsgcUDPComponent_Base = class(TsgcSocketComponent_Base)

  end;

  TsgcUDPLogFile = class(TsgcSocketLogFile)

  end;

  TsgcUDPThrottle = class(TsgcSocketThrottle)

  end;

  TsgcUDPOpenSSL_Options = class(TsgcSocketOpenSSL_Options)

  end;

  TsgcUDPTLS_Options_Base = class(TsgcSocketTLS_Options_Base)

  end;

  TsgcUDPTLS_Options = class(TsgcSocketTLS_Options)

  end;

  TsgcUDPProxy_Options = class(TsgcSocketProxy_Options)

  end;

  TsgcUDPWatchDog_Options = class(TsgcSocketWatchDog_Options)

  end;

  TsgcUDPNotifyObject = class
  private
    FBytes: TBytes;
    FSocket: TsgcUDPSocket;
    FRawException: Exception;
    procedure SetBytes(const Value: TBytes);
    procedure SetRawException(const Value: Exception);
    procedure SetSocket(const Value: TsgcUDPSocket);
  public
    destructor Destroy; override;
  public
    property Bytes: TBytes read FBytes write SetBytes;
    property Socket: TsgcUDPSocket read FSocket write SetSocket;
    property RawException: Exception read FRawException write SetRawException;
  end;

  TsgcUDPObjectList = class
    (TsgcThreadList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF})
  private
    procedure DoAdd(Item: Pointer);
  public
    procedure AddNotifyObject(aSocket: TsgcUDPSocket); overload;
    procedure AddNotifyObject(aSocket: TsgcUDPSocket;
      aException: Exception); overload;
    procedure AddNotifyObject(aSocket: TsgcUDPSocket; aBytes: TBytes); overload;
  public
    procedure DeleteAll(aFreeSocket: Boolean = False);
  end;

  TsgcUDPComponent = class(TsgcUDPComponent_Base)
  private
    FOnUDPConnect: TsgcUDPConnectEvent;
    FOnUDPDisconnect: TsgcUDPDisconnectEvent;
    FOnUDPRead: TsgcUDPReadEvent;
    FOnUDPException: TsgcUDPExceptionEvent;
{$IFNDEF SGC_EVENT_DISPATCH}
  private
    FNotifyUDPConnect: TsgcUDPObjectList;
    FNotifyUDPDisconnect: TsgcUDPObjectList;
    FNotifyUDPRead: TsgcUDPObjectList;
    FNotifyUDPException: TsgcUDPObjectList;
  private
    FAsyncUDPConnect: Boolean;
    FAsyncUDPDisconnect: Boolean;
    FAsyncUDPRead: Boolean;
    FAsyncUDPException: Boolean;
    procedure DoAsyncUDPConnect;
    procedure DoAsyncUDPDisconnect;
    procedure DoAsyncUDPRead;
    procedure DoAsyncUDPException;
  private
    function GetNotifyUDPConnect: TsgcUDPObjectList;
    function GetNotifyUDPDisconnect: TsgcUDPObjectList;
    function GetNotifyUDPRead: TsgcUDPObjectList;
    function GetNotifyUDPException: TsgcUDPObjectList;
  private
    property NotifyUDPConnect: TsgcUDPObjectList read GetNotifyUDPConnect;
    property NotifyUDPDisconnect: TsgcUDPObjectList read GetNotifyUDPDisconnect;
    property NotifyUDPRead: TsgcUDPObjectList read GetNotifyUDPRead;
    property NotifyUDPException: TsgcUDPObjectList read GetNotifyUDPException;
{$ENDIF}
  protected
    procedure DoNotifyUDPConnect(const aSocket: TsgcUDPSocket); virtual;
    procedure DoNotifyUDPDisconnect(const aSocket: TsgcUDPSocket); virtual;
    procedure DoNotifyUDPRead(const aSocket: TsgcUDPSocket); virtual;
    procedure DoNotifyUDPException(const aSocket: TsgcUDPSocket); virtual;
  protected
    procedure DoEventUDPConnect(const aSocket: TsgcUDPSocket); virtual;
    procedure DoEventUDPDisconnect(const aSocket: TsgcUDPSocket); virtual;
    procedure DoEventUDPRead(const aSocket: TsgcUDPSocket;
      const Bytes: TBytes); virtual;
    procedure DoEventUDPException(const aSocket: TsgcUDPSocket;
      const aException: Exception); virtual;
  protected
    procedure DoException(const aSocket: TsgcUDPSocket;
      const aException: Exception); virtual;
  protected
    property OnUDPConnect: TsgcUDPConnectEvent read FOnUDPConnect
      write FOnUDPConnect;
    property OnUDPDisconnect: TsgcUDPDisconnectEvent read FOnUDPDisconnect
      write FOnUDPDisconnect;
    property OnUDPRead: TsgcUDPReadEvent read FOnUDPRead write FOnUDPRead;
    property OnUDPException: TsgcUDPExceptionEvent read FOnUDPException
      write FOnUDPException;
  public
    destructor Destroy; override;
  public
    procedure EnterCS;
    procedure LeaveCS;
  end;

  TsgcUDPHeartBeat_Options = class(TsgcSocketHeartBeat_Options)

  end;

{$IFDEF SGC_DEBUG}
procedure DoLog(const aObject: TObject; const aSocket: TsgcUDPSocket_Base;
  const aMethod: String = ''; const aParams: String = '');
{$ENDIF}

{$ENDIF}

implementation

{$IFDEF SGC_UDP}

uses
  DateUtils,
  sgcWebSocket_Helpers
{$IFDEF SGC_DEBUG}{$IFDEF SGC_LOGGERPRO}, LoggerPro.GlobalLogger{$ELSE},
  siAuto{$ENDIF}{$ENDIF}
{$IFDEF SGC_EDT_PRO}, sgcUDP_Server{$ENDIF}, sgcUDP_Client;

{$IFDEF SGC_DEBUG}

procedure DoLog(const aObject: TObject; const aSocket: TsgcUDPSocket_Base;
  const aMethod: String = ''; const aParams: String = '');
var
  vText: String;
begin
  vText := '[' + aMethod + ']';
  if Assigned(aSocket) then
    vText := vText + ': ' + DecodeBase64(aSocket.Guid);
  if aParams <> '' then
    vText := vText + ' ' + aParams;

{$IFDEF SGC_LOGGERPRO}
  Log.Debug(vText, '');
{$ELSE}
  siMain.LogMessage('[ClassName]: ' + aObject.className + ' ' + vText);
{$ENDIF}
end;
{$ENDIF}

destructor TsgcUDPSocket_Base.Destroy;
begin
  inherited;
end;

procedure TsgcUDPSocket_Base.AddUDPEndOfFrame(aByte: Byte);
var
  vLength: Integer;
begin
  vLength := Length(FUDPEndOfFrame);
  SetLength(FUDPEndOfFrame, vLength + 1);
  FUDPEndOfFrame[vLength] := aByte;
end;

procedure TsgcUDPSocket_Base.ClearUDPEndOfFrame;
begin
  SetLength(FUDPEndOfFrame, 0);
end;

procedure TsgcUDPSocket_Base.DoException(const E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(self, E);
end;

procedure TsgcUDPSocket_Base.DoLogRecvString(const aValue: string);
begin
  if Assigned(FLogFile) then
  begin
    FLogFile.EnterCS;
    Try
      FLogFile.Open;
      Try
        FLogFile.LogRecvString(aValue);
      Finally
        FLogFile.Close;
      End;
    Finally
      FLogFile.LeaveCS;
    End;
  end;
end;

procedure TsgcUDPSocket_Base.DoLogSendString(const aValue: string);
begin
  if Assigned(FLogFile) then
  begin
    FLogFile.EnterCS;
    Try
      FLogFile.Open;
      Try
        FLogFile.LogSendString(aValue);
      Finally
        FLogFile.Close;
      End;
    Finally
      FLogFile.LeaveCS;
    End;
  end;
end;

procedure TsgcUDPSocket_Base.DoLogData(const aBuffer: TBytes);
begin
  if Length(aBuffer) > 0 then
    DoLogRecvString(sgcBytesToStringRaw(aBuffer));
end;

procedure TsgcUDPSocket_Base.DoWriteData(const aValue: string);
begin
  if Assigned(FLogFile) then
    DoLogSendString(aValue);
end;

procedure TsgcUDPSocket_Base.DoWriteData(const aValue: TBytes);
begin
  if Assigned(FLogFile) then
    DoLogSendString(sgcBytesToStringRaw(aValue));
end;

procedure TsgcUDPSocket_Base.DoWriteData(const aIPAddress: string; aPort: Word;
    const aValue: string);
begin
  if Assigned(FLogFile) then
    DoLogSendString(aValue);
end;

procedure TsgcUDPSocket_Base.DoWriteData(const aIPAddress: string; aPort: Word;
    const aValue: TBytes);
begin
  if Assigned(FLogFile) then
    DoLogSendString(sgcBytesToStringRaw(aValue));
end;

procedure TsgcUDPSocket_Base.LogData(const aBuffer: TBytes);
begin
  Try
    DoLogData(aBuffer);
  Except
    On E: Exception do
      DoException(E);
  End;
end;

procedure TsgcUDPSocket_Base.WriteData(const aValue: string);
begin
  Try
    DoWriteData(aValue);
  Except
    On E: Exception do
      DoException(E);
  End;
end;

procedure TsgcUDPSocket_Base.WriteData(const aValue: TBytes);
begin
  Try
    DoWriteData(aValue);
  Except
    On E: Exception do
      DoException(E);
  End;
end;

procedure TsgcUDPSocket_Base.WriteData(const aIPAddress: string; aPort: Word;
    const aValue: string);
begin
  Try
    DoWriteData(aIPAddress, aPort, aValue);
  Except
    On E: Exception do
      DoException(E);
  End;
end;

procedure TsgcUDPSocket_Base.WriteData(const aIPAddress: string; aPort: Word;
    const aValue: TBytes);
begin
  Try
    DoWriteData(aIPAddress, aPort, aValue);
  Except
    On E: Exception do
      DoException(E);
  End;
end;

destructor TsgcUDPComponent.Destroy;
begin
{$IFNDEF SGC_EVENT_DISPATCH}
  sgcFree(FNotifyUDPConnect);
  if Assigned(FNotifyUDPDisconnect) then
    FNotifyUDPDisconnect.DeleteAll(True);
  sgcFree(FNotifyUDPDisconnect);
  sgcFree(FNotifyUDPRead);
  sgcFree(FNotifyUDPException);
{$ENDIF}
  inherited;
end;

{$IFNDEF SGC_EVENT_DISPATCH}

procedure TsgcUDPComponent.DoAsyncUDPConnect;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyUDPConnect) then
    exit;

  oList := FNotifyUDPConnect.LockList;
  Try
    if FAsyncUDPConnect then
      exit;
    FAsyncUDPConnect := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventUDPConnect(TsgcUDPNotifyObject(oList.Items[0]).Socket);
        if not Assigned(FNotifyUDPConnect) then
          exit;
        if oList.Count > 0 then
{$IFDEF NEXTGEN}
          TObject(oList.Items[0]).DisposeOf;
{$ELSE}
          TObject(oList.Items[0]).Free;
{$ENDIF}
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncUDPConnect := False;
    End;
  Finally
    if Assigned(FNotifyUDPConnect) then
      FNotifyUDPConnect.UnLockList;
  End;
end;

procedure TsgcUDPComponent.DoAsyncUDPDisconnect;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyUDPDisconnect) then
    exit;

  oList := FNotifyUDPDisconnect.LockList;
  Try
    if FAsyncUDPDisconnect then
      exit;
    FAsyncUDPDisconnect := True;
    Try
      while oList.Count > 0 do
      begin
        Try
          DoEventUDPDisconnect(TsgcUDPNotifyObject(oList.Items[0]).Socket);
          if not Assigned(FNotifyUDPDisconnect) then
            exit;
          if Assigned(TsgcUDPNotifyObject(oList.Items[0]).Socket) then
            if not TsgcUDPNotifyObject(oList.Items[0]).Socket.FFreed then
              Try
                DoDestroyConnection(TsgcUDPNotifyObject(oList.Items[0]).Socket);
              Except
                // todo: handle exception
              End;
        Finally
          if Assigned(FNotifyUDPDisconnect) then
          begin
            if oList.Count > 0 then
{$IFDEF NEXTGEN}
              TObject(oList.Items[0]).DisposeOf;
{$ELSE}
              TObject(oList.Items[0]).Free;
{$ENDIF}
            if oList.Count > 0 then
              oList.Delete(0);
          end;
        end;
      end;
    Finally
      FAsyncUDPDisconnect := False;
    End;
  Finally
    if Assigned(FNotifyUDPDisconnect) then
      FNotifyUDPDisconnect.UnLockList;
  End;
end;

procedure TsgcUDPComponent.DoAsyncUDPException;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyUDPException) then
    exit;

  oList := FNotifyUDPException.LockList;
  Try
    if FAsyncUDPException then
      exit;
    FAsyncUDPException := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventUDPException(TsgcUDPNotifyObject(oList.Items[0]).Socket,
          TsgcUDPNotifyObject(oList.Items[0]).RawException);
        if not Assigned(FNotifyUDPException) then
          exit;
        if oList.Count > 0 then
{$IFDEF NEXTGEN}
          TObject(oList.Items[0]).DisposeOf;
{$ELSE}
          TObject(oList.Items[0]).Free;
{$ENDIF}
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncUDPException := False;
    End;
  Finally
    if Assigned(FNotifyUDPException) then
      FNotifyUDPException.UnLockList;
  End;
end;

procedure TsgcUDPComponent.DoAsyncUDPRead;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyUDPRead) then
    exit;

  oList := FNotifyUDPRead.LockList;
  Try
    if FAsyncUDPRead then
      exit;
    FAsyncUDPRead := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventUDPRead(TsgcUDPNotifyObject(oList.Items[0]).Socket,
          TsgcUDPNotifyObject(oList.Items[0]).Bytes);
        if not Assigned(FNotifyUDPRead) then
          exit;
        if oList.Count > 0 then
{$IFDEF NEXTGEN}
          TObject(oList.Items[0]).DisposeOf;
{$ELSE}
          TObject(oList.Items[0]).Free;
{$ENDIF}
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncUDPRead := False;
    End;
  Finally
    if Assigned(FNotifyUDPRead) then
      FNotifyUDPRead.UnLockList;
  End;
end;

function TsgcUDPComponent.GetNotifyUDPConnect: TsgcUDPObjectList;
begin
  if not Assigned(FNotifyUDPConnect) then
    FNotifyUDPConnect := TsgcUDPObjectList.Create;
  Result := FNotifyUDPConnect;
end;

function TsgcUDPComponent.GetNotifyUDPDisconnect: TsgcUDPObjectList;
begin
  if not Assigned(FNotifyUDPDisconnect) then
    FNotifyUDPDisconnect := TsgcUDPObjectList.Create;
  Result := FNotifyUDPDisconnect;
end;

function TsgcUDPComponent.GetNotifyUDPException: TsgcUDPObjectList;
begin
  if not Assigned(FNotifyUDPException) then
    FNotifyUDPException := TsgcUDPObjectList.Create;
  Result := FNotifyUDPException;
end;

function TsgcUDPComponent.GetNotifyUDPRead: TsgcUDPObjectList;
begin
  if not Assigned(FNotifyUDPRead) then
    FNotifyUDPRead := TsgcUDPObjectList.Create;
  Result := FNotifyUDPRead;
end;

{$ENDIF}

procedure TsgcUDPComponent.DoEventUDPConnect(const aSocket: TsgcUDPSocket);
begin
  if Assigned(FOnUDPConnect) then
    FOnUDPConnect(self, aSocket);
end;

procedure TsgcUDPComponent.DoEventUDPDisconnect(const aSocket: TsgcUDPSocket);
begin
  if Assigned(FOnUDPDisconnect) then
    FOnUDPDisconnect(self, aSocket);
end;

procedure TsgcUDPComponent.DoEventUDPException(const aSocket: TsgcUDPSocket;
  const aException: Exception);
begin
  if Assigned(FOnUDPException) then
    FOnUDPException(self, aSocket, aException);
end;

procedure TsgcUDPComponent.DoEventUDPRead(const aSocket: TsgcUDPSocket;
  const Bytes: TBytes);
begin
  inherited;
  if Assigned(FOnUDPRead) then
    FOnUDPRead(self, aSocket, TsgcBytes(Bytes));
end;

procedure TsgcUDPComponent.DoException(const aSocket: TsgcUDPSocket;
  const aException: Exception);
begin
  Try
    if Assigned(FOnUDPException) then
    begin
      if Assigned(aSocket) then
      begin
        aSocket.RawException := aException;
        DoNotifyUDPException(aSocket);
      end
      else
        DoEventUDPException(nil, aException);
    end
  Except
    // nothing
  end;
end;

procedure TsgcUDPComponent.DoNotifyUDPConnect(const aSocket: TsgcUDPSocket);
begin
  if IsDestroying then
    exit;

  if Assigned(aSocket) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventUDPConnect(aSocket);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventUDPConnect(aSocket);
            end);
{$ELSE}
          NotifyUDPConnect.AddNotifyObject(aSocket);
          NotifyMethod(DoAsyncUDPConnect);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventUDPConnect(aSocket);
            end);
{$ELSE}
          NotifyUDPConnect.AddNotifyObject(aSocket);
          SynchronizeMethod(DoAsyncUDPConnect);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcUDPComponent.DoNotifyUDPDisconnect(const aSocket: TsgcUDPSocket);
begin
  if IsDestroying then
    exit;

  if Assigned(aSocket) then
  begin
    case NotifyEvents of
      neNoSync:
        begin
          DoEventUDPDisconnect(aSocket);
          DoDestroyConnection(aSocket);
        end;
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventUDPDisconnect(aSocket);
              DoDestroyConnection(aSocket);
            end);
{$ELSE}
          NotifyUDPDisconnect.AddNotifyObject(aSocket);
          NotifyMethod(DoAsyncUDPDisconnect);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventUDPDisconnect(aSocket);
              DoDestroyConnection(aSocket);
            end);
{$ELSE}
          NotifyUDPDisconnect.AddNotifyObject(aSocket);
          SynchronizeMethod(DoAsyncUDPDisconnect);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcUDPComponent.DoNotifyUDPException(const aSocket: TsgcUDPSocket);
begin
  if IsDestroying then
    exit;

  if Assigned(aSocket) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventUDPException(aSocket, aSocket.RawException);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventUDPException(aSocket, aSocket.MsgException,
                aSocket.RawException);
            end);
{$ELSE}
          NotifyUDPException.AddNotifyObject(aSocket, aSocket.RawException);
          NotifyMethod(DoAsyncUDPException);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventUDPException(aSocket, aSocket.RawException);
            end);
{$ELSE}
          NotifyUDPException.AddNotifyObject(aSocket, aSocket.RawException);
          SynchronizeMethod(DoAsyncUDPException);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcUDPComponent.DoNotifyUDPRead(const aSocket: TsgcUDPSocket);
begin
  if IsDestroying then
    exit;

  if Assigned(aSocket) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventUDPRead(aSocket, aSocket.MsgRead);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventUDPRead(aSocket, aSocket.MsgRead);
            end);
{$ELSE}
          NotifyUDPRead.AddNotifyObject(aSocket, aSocket.MsgRead);
          NotifyMethod(DoAsyncUDPRead);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventMessage(aSocket, aSocket.MsgRead);
            end);
{$ELSE}
          NotifyUDPRead.AddNotifyObject(aSocket, aSocket.MsgRead);
          SynchronizeMethod(DoAsyncUDPRead);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcUDPComponent.EnterCS;
begin
  DoEnterCS;
end;

procedure TsgcUDPComponent.LeaveCS;
begin
  DoLeaveCS;
end;

destructor TsgcUDPNotifyObject.Destroy;
begin
  sgcFree(FRawException);
  sgcFree(FSocket);
  inherited;
end;

procedure TsgcUDPNotifyObject.SetBytes(const Value: TBytes);
begin
  SetLength(FBytes, Length(Value));
  sgcMove(Value[0], FBytes[0], Length(Value));
end;

procedure TsgcUDPNotifyObject.SetRawException(const Value: Exception);
begin
  FRawException := Exception(Value.ClassType.Create);
  FRawException.Message := Value.Message;
end;

procedure TsgcUDPNotifyObject.SetSocket(const Value: TsgcUDPSocket);
begin
{$IFDEF SGC_EDT_PRO}
  if Value.ClassType = TsgcUDPSocketServer then
    FSocket := TsgcUDPSocket(TsgcUDPSocketServerClass.Create)
  else
{$ENDIF}
    if Value.ClassType = TsgcUDPSocketClient then
      FSocket := TsgcUDPSocket(TsgcUDPSocketClientClass.Create);
  FSocket.Assign(Value);
end;

procedure TsgcUDPObjectList.AddNotifyObject(aSocket: TsgcUDPSocket);
var
  oComponent: TsgcUDPNotifyObject;
begin
  oComponent := TsgcUDPNotifyObject.Create;
  oComponent.Socket := aSocket;

  DoAdd(oComponent);
end;

procedure TsgcUDPObjectList.AddNotifyObject(aSocket: TsgcUDPSocket;
aException: Exception);
var
  oComponent: TsgcUDPNotifyObject;
begin
  oComponent := TsgcUDPNotifyObject.Create;
  oComponent.Socket := aSocket;
  oComponent.RawException := aException;

  DoAdd(oComponent);
end;

procedure TsgcUDPObjectList.AddNotifyObject(aSocket: TsgcUDPSocket;
aBytes: TBytes);
var
  oComponent: TsgcUDPNotifyObject;
begin
  oComponent := TsgcUDPNotifyObject.Create;
  oComponent.Socket := aSocket;
  oComponent.Bytes := aBytes;

  DoAdd(oComponent);
end;

procedure TsgcUDPObjectList.DeleteAll(aFreeSocket: Boolean = False);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF};
  oObject: TsgcUDPNotifyObject;
begin
  oList := LockList;
  Try
    while oList.Count > 0 do
    begin
      oObject := TsgcUDPNotifyObject(oList.Items[0]);
      if aFreeSocket then
      begin
        if Assigned(oObject.Socket) then
        begin
          if not oObject.Socket.FFreed then
          begin
            Try
              oObject.Socket.Free;
            Except
              // todo: handle exception
            End;
          end;
        end;
      end;
      sgcFree(oObject);
      oList.Delete(0);
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcUDPObjectList.DoAdd(Item: Pointer);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcUDPNotifyObject>{$ENDIF};
begin
  oList := LockList;
  Try
    oList.Add(Item);
  Finally
    UnLockList;
  End;
end;

procedure TsgcUDPSocket.Assign(const aSource: TsgcUDPSocket);
begin
  if Assigned(aSource) then
  begin
    FIP := aSource.IP;
    FLocalIP := aSource.LocalIP;
    FPort := aSource.Port;
    FLocalPort := aSource.LocalPort;
    FIPVersion := aSource.IPVersion;
    FGuid := aSource.Guid;
    FLogFile := aSource.FLogFile;
    FOnException := aSource.OnException;
  end;
end;

constructor TsgcUDPRetransmission_Options.Create;
begin
  inherited;
  Enabled := False;
  MaxRetries := 7;
  RTO := 500;
end;

procedure TsgcUDPRetransmission_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcUDPRetransmission_Options then
  begin
    Enabled := TsgcUDPRetransmission_Options(aSource).Enabled;
    MaxRetries := TsgcUDPRetransmission_Options(aSource).MaxRetries;
    RTO := TsgcUDPRetransmission_Options(aSource).RTO;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcUDPRetransmissions.Create;
begin
  inherited;
end;

destructor TsgcUDPRetransmissions.Destroy;
begin
  sgcFree(FTimer);
  sgcFree(FQueue);
  inherited;
end;

procedure TsgcUDPRetransmissions.AddItem(const aItem
  : TsgcUDPRetransmissionItem);
begin
  aItem.RTO := RTO;
  aItem.NextRetry := IncMillisecond(Now, RTO);
  Queue.Add(aItem);
end;

function TsgcUDPRetransmissions.DeleteItem(const aID: String): Boolean;
begin
  Result := Queue.DeleteItem(aID);
end;

procedure TsgcUDPRetransmissions.DoRetransmissions;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oItem: TsgcUDPRetransmissionItem;
begin
  oList := Queue.LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      oItem := TsgcUDPRetransmissionItem(oList[i]);
      if Now >= oItem.NextRetry then
      begin
        oItem.Retries := oItem.Retries + 1;
        oItem.RTO := oItem.RTO * 2;
        oItem.NextRetry := IncMillisecond(Now, oItem.RTO);
        if oItem.Retries >= oItem.MaxRetries then
        begin
          if Assigned(FOnTimeout) then
            FOnTimeout(self, oItem.Bytes, oItem.IPAddress, oItem.Port);

          sgcFree(oItem);
          oList.Delete(i);
        end
        else
        begin
          if Assigned(FOnRetry) then
            FOnRetry(self, oItem.Bytes, oItem.IPAddress, oItem.Port);
        end;
      end;
    end;
  Finally
    Queue.UnLockList;
  End;
end;

function TsgcUDPRetransmissions.GetEnabled: Boolean;
begin
  Result := Timer.Enabled;
end;

function TsgcUDPRetransmissions.GetQueue: TsgcUDPRetransmissionQueue;
begin
  if not Assigned(FQueue) then
  begin
    FQueue := TsgcUDPRetransmissionQueue.Create;
    FQueue.OwnObjects := True;
  end;
  Result := FQueue;
end;

function TsgcUDPRetransmissions.GetTimer: TsgcTimer;
begin
  if not Assigned(FTimer) then
  begin
    FTimer := TsgcTimer.Create;
    FTimer.Interval := 10;
    FTimer.OnTimer := OnTimerEvent;
    FTimer.OnException := OnExceptionEvent;
  end;
  Result := FTimer;
end;

procedure TsgcUDPRetransmissions.OnExceptionEvent(Sender: TObject;
E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(self, E);
end;

procedure TsgcUDPRetransmissions.OnTimerEvent(Sender: TObject);
begin
  DoRetransmissions;
end;

procedure TsgcUDPRetransmissions.SetEnabled(const Value: Boolean);
begin
  Timer.Enabled := True;
end;

constructor TsgcUDPRetransmissionItem.Create;
begin
  inherited;
  MaxRetries := 7;
  NextRetry := 0;
  Retries := 0;
  RTO := 0;
end;

procedure TsgcUDPRetransmissionItem.SetBytes(const Value: TBytes);
begin
  SetLength(FBytes, Length(Value));
  sgcMove(Value[0], FBytes[0], Length(FBytes));
end;

initialization

// -->start sgc_trial
{$IFDEF SGC_TRIAL}
if ((Now > EncodeDate(2022, 5, 28)) and (FormatDateTime('s', Now) = '0')) then
begin
  raise Exception.Create
    (DecodeBase64
    ('VGhpcyBkZW1vIHZlcnNpb24gY2FuIG9ubHkgcnVuIGZvciBhIGxpbWl0ZWQgdGltZSBwZXJpb2QuIFBsZWFzZSB2aXNpdCB3d3cuZXNlZ2VjZS5jb20gdG8gcHVyY2hhc2UgeW91ciBjb3B5IG9mIHRoZSBsaWJyYXJ5Lg==')
    );
end;
{$ENDIF}
// <--end sgc_trial

finalization

{$ENDIF}

end.
