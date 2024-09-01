{ ***************************************************************************
  sgcTCP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcTCP_Classes;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcBase_Classes, sgcBase_Helpers, sgcSocket_Classes,
  // websocket
  sgcWebSocket_Classes_SyncObjs, sgcWebSocket_Helpers, sgcWebSocket_Types;

type
  TsgcTCPConnection = class;

  TsgcTCPConnectEvent = procedure(Connection: TsgcTCPConnection) of object;
  TsgcTCPDisconnectEvent = procedure(Connection: TsgcTCPConnection) of object;
  TsgcTCPMessageEvent = procedure(Connection: TsgcTCPConnection;
    const Text: string) of object;
  TsgcTCPBinaryEvent = procedure(Connection: TsgcTCPConnection;
    const Data: TMemoryStream) of object;
  TsgcTCPExceptionEvent = procedure(Connection: TsgcTCPConnection; E: Exception)
    of object;

  TsgcTCPKeepAlive = class(TsgcSocketKeepAlive)

  end;

  TsgcTCPConnection_Base = class(TsgcSocketConnection)
    { protocol fields }
  private
    FMsgReceived: String;
    FMsgBinaryReceived: TMemoryStream;
    FMsgException: String;
  protected
    function GetMsgBinaryReceived: TMemoryStream;
  public
    property MsgReceived: String read FMsgReceived write FMsgReceived;
    property MsgBinaryReceived: TMemoryStream read GetMsgBinaryReceived
      write FMsgBinaryReceived;
    property MsgException: String read FMsgException write FMsgException;
    { protocol fields }

    { disconnect }
  protected
    FMustDisconnect: Boolean;
    procedure DoDisconnectIfActive; virtual;
    { disconnect }

    { frame }
  private
    FTCPEndOfFrameScanBuffer: TtcpEOFScanBuffer;
  protected
    FTCPEndOfFrame: TBytes;
  public
    procedure AddTCPEndOfFrame(aByte: Byte);
    procedure ClearTCPEndOfFrame;
  public
    property TCPEndOfFrameScanBuffer: TtcpEOFScanBuffer
      read FTCPEndOfFrameScanBuffer write FTCPEndOfFrameScanBuffer;
    { frame }

    { constructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor }
  end;

  TsgcTCPConnection = class(TsgcTCPConnection_Base)

  end;

  TsgcTCPNotifyObject = class
  private
    FConnection: TsgcTCPConnection;
    FInt: Integer;
    FRawException: Exception;
    FStream: TMemoryStream;
    FText: String;
    FText1: String;
    procedure SetRawException(const Value: Exception);
  protected
    function GetStream: TMemoryStream;
  public
    destructor Destroy; override;
  public
    property Connection: TsgcTCPConnection read FConnection write FConnection;
    property Int: Integer read FInt write FInt;
    property RawException: Exception read FRawException write SetRawException;
    property Stream: TMemoryStream read GetStream write FStream;
    property Text: String read FText write FText;
    property Text1: String read FText1 write FText1;
  end;

  TsgcTCPObjectList = class
    (TsgcThreadList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF})
  private
    procedure DoAdd(Item: Pointer);
  public
    procedure AddNotifyObject(aConnection: TsgcTCPConnection); overload;
    procedure AddNotifyObject(aConnection: TsgcTCPConnection;
      aParam: String); overload;
    procedure AddNotifyObject(aConnection: TsgcTCPConnection; aParam: String;
      aParam1: String); overload;
    procedure AddNotifyObject(aConnection: TsgcTCPConnection;
      aParam: Integer); overload;
    procedure AddNotifyObject(aConnection: TsgcTCPConnection;
      aParam: TMemoryStream); overload;
    procedure AddNotifyObject(aConnection: TsgcTCPConnection;
      aStream: TMemoryStream; aOpCode: TOpCode; aBoolean: Boolean); overload;
    procedure AddNotifyObject(aConnection: TsgcTCPConnection; aParam: String;
      aException: Exception); overload;
  public
    procedure DeleteAll(aFreeConnection: Boolean = False);
  end;

  TsgcTCPComponent_Base = class(TsgcSocketComponent_Base)

  end;

  TsgcTCPLogFile = class(TsgcSocketLogFile)

  end;

  TsgcTCPThrottle = class(TsgcSocketThrottle)

  end;

  TsgcTCPOpenSSL_Options = class(TsgcSocketOpenSSL_Options)

  end;

  TsgcTCPTLS_Options_Base = class(TsgcSocketTLS_Options_Base)

  end;

  TsgcTCPTLS_Options = class(TsgcSocketTLS_Options)

  end;

  TsgcTCPProxy_Options = class(TsgcSocketProxy_Options)

  end;

  TsgcTCPWatchDog_Options = class(TPersistent)
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

  TsgcTCPComponent = class(TsgcTCPComponent_Base)
    { events }
  private
    FQueueMessages: Boolean;
    FQueueMessage: TsgcTCPObjectList;
    FQueueBinary: TsgcTCPObjectList;
  private
    FOnConnect: TsgcTCPConnectEvent;
    FOnDisconnect: TsgcTCPDisconnectEvent;
    FOnMessage: TsgcTCPMessageEvent;
    FOnBinary: TsgcTCPBinaryEvent;
    FOnException: TsgcTCPExceptionEvent;
{$IFNDEF SGC_EVENT_DISPATCH}
  private
    FNotifyConnect: TsgcTCPObjectList;
    FNotifyDisconnect: TsgcTCPObjectList;
    FNotifyMessage: TsgcTCPObjectList;
    FNotifyBinary: TsgcTCPObjectList;
    FNotifyException: TsgcTCPObjectList;
  private
    FAsyncConnect: Boolean;
    FAsyncDisconnect: Boolean;
    FAsyncMessage: Boolean;
    FAsyncBinary: Boolean;
    FAsyncException: Boolean;
    procedure DoAsyncConnect;
    procedure DoAsyncDisconnect;
    procedure DoAsyncMessage;
    procedure DoAsyncBinary;
    procedure DoAsyncException;
  private
    function GetNotifyConnect: TsgcTCPObjectList;
    function GetNotifyDisconnect: TsgcTCPObjectList;
    function GetNotifyMessage: TsgcTCPObjectList;
    function GetNotifyBinary: TsgcTCPObjectList;
    function GetNotifyException: TsgcTCPObjectList;
  private
    property NotifyConnect: TsgcTCPObjectList read GetNotifyConnect;
    property NotifyDisconnect: TsgcTCPObjectList read GetNotifyDisconnect;
    property NotifyMessage: TsgcTCPObjectList read GetNotifyMessage;
    property NotifyBinary: TsgcTCPObjectList read GetNotifyBinary;
    property NotifyException: TsgcTCPObjectList read GetNotifyException;
{$ENDIF}
  private
    function GetQueueMessage: TsgcTCPObjectList;
    function GetQueueBinary: TsgcTCPObjectList;
  protected
    property QueueMessage: TsgcTCPObjectList read GetQueueMessage;
    property QueueBinary: TsgcTCPObjectList read GetQueueBinary;
  protected
    procedure DoEventConnect(const aConnection: TsgcTCPConnection); virtual;
    procedure DoEventDisconnect(const aConnection: TsgcTCPConnection); virtual;
    procedure DoEventMessage(const aConnection: TsgcTCPConnection;
      const Text: string); virtual;
    procedure DoEventBinary(const aConnection: TsgcTCPConnection;
      const Data: TMemoryStream); virtual;
    procedure DoEventException(const aConnection: TsgcTCPConnection;
      const Error: String; const aException: Exception); virtual;
  protected
    procedure DoNotifyConnect(const aConnection: TsgcTCPConnection); virtual;
    procedure DoNotifyDisconnect(const aConnection: TsgcTCPConnection); virtual;
    procedure DoNotifyMessage(const aConnection: TsgcTCPConnection); virtual;
    procedure DoNotifyBinary(const aConnection: TsgcTCPConnection); virtual;
    procedure DoNotifyException(const aConnection: TsgcTCPConnection); overload; virtual;
    procedure DoNotifyException(const Error: STring; const aException: Exception);
        overload; virtual;
  protected
    procedure DoException(const aConnection: TsgcTCPConnection;
      const aMsgException: String; const aException: Exception = nil); virtual;
  protected
    FQueueProcessing: Boolean;
    procedure SetQueueMessages(const Value: Boolean); virtual;
    procedure DoProcessQueue; virtual;
    procedure DoClearQueue; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure QueueClear;
    property QueueMessages: Boolean read FQueueMessages write SetQueueMessages;
  protected
    property OnConnect: TsgcTCPConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TsgcTCPDisconnectEvent read FOnDisconnect
      write FOnDisconnect;
    property OnMessage: TsgcTCPMessageEvent read FOnMessage write FOnMessage;
    property OnBinary: TsgcTCPBinaryEvent read FOnBinary write FOnBinary;
    property OnException: TsgcTCPExceptionEvent read FOnException
      write FOnException;
    { events }
  end;

  TsgcTCPHeartBeat_Options = class(TsgcSocketHeartBeat_Options)

  end;

{$IFDEF SGC_DEBUG}

procedure DoLog(const aObject: TObject; const aConnection: TsgcTCPConnection;
  const aMethod: String = ''; const aParams: String = '');
{$ENDIF}

implementation

{$IFDEF SGC_DEBUG}

uses
  {$IFDEF SGC_LOGGERPRO}LoggerPro.GlobalLogger{$ELSE}siAuto{$ENDIF};

procedure DoLog(const aObject: TObject; const aConnection: TsgcTCPConnection;
  const aMethod: String = ''; const aParams: String = '');
var
  vText: String;
begin
  vText := '[' + aMethod + ']';
  if Assigned(aConnection) then
    vText := vText + ': ' + aConnection.Guid;
  if aParams <> '' then
    vText := vText + ' ' + aParams;

  {$IFDEF SGC_LOGGERPRO}
  Log.Debug(vText, '');
  {$ELSE}
  siMain.LogMessage('[ClassName]: ' + aObject.className + ' ' + vText);
  {$ENDIF}
end;
{$ENDIF}

constructor TsgcTCPConnection_Base.Create;
begin
  inherited;
  FFreed := False;
end;

destructor TsgcTCPConnection_Base.Destroy;
begin
  sgcFree(FMsgBinaryReceived);
  inherited;
end;

procedure TsgcTCPConnection_Base.AddTCPEndOfFrame(aByte: Byte);
var
  vLength: Integer;
begin
  vLength := Length(FTCPEndOfFrame);
  SetLength(FTCPEndOfFrame, vLength + 1);
  FTCPEndOfFrame[vLength] := aByte;
end;

procedure TsgcTCPConnection_Base.ClearTCPEndOfFrame;
begin
  SetLength(FTCPEndOfFrame, 0);
end;

procedure TsgcTCPConnection_Base.DoDisconnectIfActive;
begin
  // nothing
end;

function TsgcTCPConnection_Base.GetMsgBinaryReceived: TMemoryStream;
begin
  if not Assigned(FMsgBinaryReceived) then
    FMsgBinaryReceived := TMemoryStream.Create;
  Result := FMsgBinaryReceived;
end;

constructor TsgcTCPWatchDog_Options.Create;
begin
  inherited;
  Enabled := False;
  Interval := 60;
  Attempts := 0;
end;

procedure TsgcTCPWatchDog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcTCPWatchDog_Options then
  begin
    Enabled := TsgcTCPWatchDog_Options(aSource).Enabled;
    Interval := TsgcTCPWatchDog_Options(aSource).Interval;
    Attempts := TsgcTCPWatchDog_Options(aSource).Attempts;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcTCPComponent.DoNotifyDisconnect(const aConnection
  : TsgcTCPConnection);
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    case NotifyEvents of
      neNoSync:
        begin
          DoEventDisconnect(aConnection);
          DoDestroyConnection(aConnection);
        end;
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventDisconnect(aConnection);
              DoDestroyConnection(aConnection);
            end);
{$ELSE}
          NotifyDisconnect.AddNotifyObject(aConnection);
          NotifyMethod(DoAsyncDisconnect);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventDisconnect(aConnection);
              DoDestroyConnection(aConnection);
            end);
{$ELSE}
          NotifyDisconnect.AddNotifyObject(aConnection);
          SynchronizeMethod(DoAsyncDisconnect);
{$ENDIF}
        end;
    end;
  end;
end;

constructor TsgcTCPComponent.Create(aOwner: TComponent);
begin
  inherited;
  FQueueMessages := False;
end;

destructor TsgcTCPComponent.Destroy;
begin
{$IFNDEF SGC_EVENT_DISPATCH}
  sgcFree(FNotifyConnect);
  if Assigned(FNotifyDisconnect) then
    FNotifyDisconnect.DeleteAll(True);
  sgcFree(FNotifyDisconnect);
  sgcFree(FNotifyMessage);
  sgcFree(FNotifyException);
  sgcFree(FNotifyBinary);
{$ENDIF}
  sgcFree(FQueueMessage);
  sgcFree(FQueueBinary);
  inherited;
end;

{$IFNDEF SGC_EVENT_DISPATCH}

procedure TsgcTCPComponent.DoAsyncConnect;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyConnect) then
    exit;

  oList := FNotifyConnect.LockList;
  Try
    if FAsyncConnect then
      exit;
    FAsyncConnect := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventConnect(TsgcTCPNotifyObject(oList.Items[0]).Connection);
        if not Assigned(FNotifyConnect) then
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
      FAsyncConnect := False;
    End;
  Finally
    if Assigned(FNotifyConnect) then
      FNotifyConnect.UnLockList;
  End;
end;

procedure TsgcTCPComponent.DoAsyncDisconnect;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyDisconnect) then
    exit;

  oList := FNotifyDisconnect.LockList;
  Try
    if FAsyncDisconnect then
      exit;
    FAsyncDisconnect := True;
    Try
      while oList.Count > 0 do
      begin
        Try
          DoEventDisconnect(TsgcTCPNotifyObject(oList.Items[0]).Connection);
          if not Assigned(FNotifyDisconnect) then
            exit;
          if Assigned(TsgcTCPNotifyObject(oList.Items[0]).Connection) then
            if not TsgcTCPNotifyObject(oList.Items[0]).Connection.FFreed then
              Try
                DoDestroyConnection(TsgcTCPNotifyObject(oList.Items[0])
                  .Connection);
              Except
                // todo: handle exception
              End;
        Finally
          if Assigned(FNotifyDisconnect) then
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
      FAsyncDisconnect := False;
    End;
  Finally
    if Assigned(FNotifyDisconnect) then
      FNotifyDisconnect.UnLockList;
  End;
end;

procedure TsgcTCPComponent.DoAsyncMessage;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyMessage) then
    exit;

  oList := FNotifyMessage.LockList;
  Try
    if FAsyncMessage then
      exit;
    FAsyncMessage := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventMessage(TsgcTCPNotifyObject(oList.Items[0]).Connection,
          TsgcTCPNotifyObject(oList.Items[0]).Text);
        if not Assigned(FNotifyMessage) then
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
      FAsyncMessage := False;
    End;
  Finally
    if Assigned(FNotifyMessage) then
      FNotifyMessage.UnLockList;
  End;
end;

procedure TsgcTCPComponent.DoAsyncBinary;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyBinary) then
    exit;

  oList := FNotifyBinary.LockList;
  Try
    if FAsyncBinary then
      exit;
    FAsyncBinary := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventBinary(TsgcTCPNotifyObject(oList.Items[0]).Connection,
          TsgcTCPNotifyObject(oList.Items[0]).Stream);
        if not Assigned(FNotifyBinary) then
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
      FAsyncBinary := False;
    End;
  Finally
    if Assigned(FNotifyBinary) then
      FNotifyBinary.UnLockList;
  End;
end;

procedure TsgcTCPComponent.DoAsyncException;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyException) then
    exit;

  oList := FNotifyException.LockList;
  Try
    if FAsyncException then
      exit;
    FAsyncException := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventException(TsgcTCPNotifyObject(oList.Items[0]).Connection,
          TsgcTCPNotifyObject(oList.Items[0]).Text,
          TsgcTCPNotifyObject(oList.Items[0]).RawException);
        if not Assigned(FNotifyException) then
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
      FAsyncException := False;
    End;
  Finally
    if Assigned(FNotifyException) then
      FNotifyException.UnLockList;
  End;
end;

function TsgcTCPComponent.GetNotifyConnect: TsgcTCPObjectList;
begin
  if not Assigned(FNotifyConnect) then
    FNotifyConnect := TsgcTCPObjectList.Create;
  Result := FNotifyConnect;
end;

function TsgcTCPComponent.GetNotifyDisconnect: TsgcTCPObjectList;
begin
  if not Assigned(FNotifyDisconnect) then
    FNotifyDisconnect := TsgcTCPObjectList.Create;
  Result := FNotifyDisconnect;
end;

function TsgcTCPComponent.GetNotifyMessage: TsgcTCPObjectList;
begin
  if not Assigned(FNotifyMessage) then
    FNotifyMessage := TsgcTCPObjectList.Create;
  Result := FNotifyMessage;
end;

function TsgcTCPComponent.GetNotifyBinary: TsgcTCPObjectList;
begin
  if not Assigned(FNotifyBinary) then
    FNotifyBinary := TsgcTCPObjectList.Create;
  Result := FNotifyBinary;
end;

function TsgcTCPComponent.GetNotifyException: TsgcTCPObjectList;
begin
  if not Assigned(FNotifyException) then
    FNotifyException := TsgcTCPObjectList.Create;
  Result := FNotifyException;
end;
{$ENDIF}

procedure TsgcTCPComponent.DoClearQueue;
begin
  if Assigned(FQueueMessage) then
    FQueueMessage.Clear;

  if Assigned(FQueueBinary) then
    FQueueBinary.Clear;
end;

procedure TsgcTCPComponent.DoNotifyConnect(const aConnection
  : TsgcTCPConnection);
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventConnect(aConnection);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventConnect(aConnection);
            end);
{$ELSE}
          NotifyConnect.AddNotifyObject(aConnection);
          NotifyMethod(DoAsyncConnect);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventConnect(aConnection);
            end);
{$ELSE}
          NotifyConnect.AddNotifyObject(aConnection);
          SynchronizeMethod(DoAsyncConnect);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcTCPComponent.DoNotifyMessage(const aConnection
  : TsgcTCPConnection);
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventMessage(aConnection, aConnection.MsgReceived);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventMessage(aConnection, aConnection.MsgReceived);
            end);
{$ELSE}
          NotifyMessage.AddNotifyObject(aConnection, aConnection.MsgReceived);
          NotifyMethod(DoAsyncMessage);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventMessage(aConnection, aConnection.MsgReceived);
            end);
{$ELSE}
          NotifyMessage.AddNotifyObject(aConnection, aConnection.MsgReceived);
          SynchronizeMethod(DoAsyncMessage);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcTCPComponent.DoNotifyBinary(const aConnection: TsgcTCPConnection);
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventBinary(aConnection, aConnection.MsgBinaryReceived);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventBinary(aConnection, aConnection.MsgBinaryReceived);
            end);
{$ELSE}
          NotifyBinary.AddNotifyObject(aConnection,
            aConnection.MsgBinaryReceived);
          NotifyMethod(DoAsyncBinary);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventBinary(aConnection, aConnection.MsgBinaryReceived);
            end);
{$ELSE}
          NotifyBinary.AddNotifyObject(aConnection,
            aConnection.MsgBinaryReceived);
          SynchronizeMethod(DoAsyncBinary);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcTCPComponent.DoEventBinary(const aConnection: TsgcTCPConnection;
const Data: TMemoryStream);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventBinary');
{$ENDIF}
  if Assigned(FOnBinary) then
  begin
    if QueueMessages then
      QueueBinary.AddNotifyObject(aConnection, Data)
    else
      FOnBinary(aConnection, Data);
  end;
end;

procedure TsgcTCPComponent.DoEventConnect(const aConnection: TsgcTCPConnection);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventConnect');
{$ENDIF}
  if Assigned(FOnConnect) then
    FOnConnect(aConnection);
end;

procedure TsgcTCPComponent.DoEventDisconnect(const aConnection
  : TsgcTCPConnection);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventDisconnect');
{$ENDIF}
  if Assigned(FOnDisconnect) then
    FOnDisconnect(aConnection);
end;

procedure TsgcTCPComponent.DoEventException(const aConnection
  : TsgcTCPConnection; const Error: String; const aException: Exception);
var
  vException: Exception;
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventException', '[Error]: ' + Error);
{$ENDIF}
  if Assigned(FOnException) then
  begin
    if not Assigned(aException) then
    begin
      vException := Exception.Create(Error);
      Try
        FOnException(aConnection, vException);
      Finally
        sgcFree(vException);
      End;
    end
    else
    begin
      if aException.Message = '' then
        aException.Message := Error;
      FOnException(aConnection, aException);
    end;
  end;
end;

procedure TsgcTCPComponent.DoEventMessage(const aConnection: TsgcTCPConnection;
const Text: string);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
{$ENDIF}
  if Assigned(FOnMessage) then
  begin
    if QueueMessages then
      QueueMessage.AddNotifyObject(aConnection, Text)
    else
      FOnMessage(aConnection, Text);
  end;
end;

procedure TsgcTCPComponent.DoException(const aConnection: TsgcTCPConnection;
const aMsgException: String; const aException: Exception = nil);
begin
  Try
    if Assigned(FOnException) then
    begin
      if Assigned(aConnection) then
      begin
        aConnection.MsgException := aMsgException;
        aConnection.RawException := aException;
        DoNotifyException(aConnection);
        // ... disconnect on error
        aConnection.DoDisconnectIfActive;
      end
      else
        DoNotifyException(aMsgException, aException);
    end
    else
    begin
      if Assigned(aConnection) then
      begin
        // ... disconnect on error
        aConnection.DoDisconnectIfActive;
      end;
    end;
  Except
    // nothing
  end;
end;

procedure TsgcTCPComponent.DoNotifyException(const aConnection
  : TsgcTCPConnection);
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventException(aConnection, aConnection.MsgException,
          aConnection.RawException);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventException(aConnection, aConnection.MsgException,
                aConnection.RawException);
            end);
{$ELSE}
          NotifyException.AddNotifyObject(aConnection, aConnection.MsgException,
            aConnection.RawException);
          NotifyMethod(DoAsyncException);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventException(aConnection, aConnection.MsgException,
                aConnection.RawException);
            end);
{$ELSE}
          NotifyException.AddNotifyObject(aConnection, aConnection.MsgException,
            aConnection.RawException);
          SynchronizeMethod(DoAsyncException);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcTCPComponent.DoNotifyException(const Error: STring; const
    aException: Exception);
begin
  if IsDestroying then
    exit;

  case NotifyEvents of
    neNoSync:
      DoEventException(nil, Error, aException);
    neAsynchronous:
      begin
{$IFDEF SGC_EVENT_DISPATCH}
        TThread.Queue(nil,
          procedure
          begin
            DoEventException(nil, Error, aException);
          end);
{$ELSE}
        NotifyException.AddNotifyObject(nil, Error, aException);
        NotifyMethod(DoAsyncException);
{$ENDIF}
      end;
    neSynchronous:
      begin
{$IFDEF SGC_EVENT_DISPATCH}
        TThread.Synchronize(nil,
          procedure
          begin
            DoEventException(nil, Error, aException);
          end);
{$ELSE}
        NotifyException.AddNotifyObject(nil, Error, aException);
        SynchronizeMethod(DoAsyncException);
{$ENDIF}
      end;
  end;
end;

procedure TsgcTCPComponent.DoProcessQueue;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  FQueueProcessing := True;
  Try
    { messages }
    oList := QueueMessage.LockList;
    Try
      While oList.Count > 0 do
      begin
        DoEventMessage(TsgcTCPNotifyObject(oList.Items[0]).Connection,
          TsgcTCPNotifyObject(oList.Items[0]).Text);
{$IFDEF NEXTGEN}
        TObject(oList.Items[0]).DisposeOf;
{$ELSE}
        TObject(oList.Items[0]).Free;
{$ENDIF}
        oList.Delete(0);
      end;
    Finally
      QueueMessage.UnLockList;
    End;

    { binary }
    oList := QueueBinary.LockList;
    Try
      while oList.Count > 0 do
      begin
        DoEventBinary(TsgcTCPNotifyObject(oList.Items[0]).Connection,
          TsgcTCPNotifyObject(oList.Items[0]).Stream);
{$IFDEF NEXTGEN}
        TObject(oList.Items[0]).DisposeOf;
{$ELSE}
        TObject(oList.Items[0]).Free;
{$ENDIF}
        oList.Delete(0);
      end;
    Finally
      QueueBinary.UnLockList;
    End;
  Finally
    FQueueProcessing := False;
  End;
end;

function TsgcTCPComponent.GetQueueBinary: TsgcTCPObjectList;
begin
  if not Assigned(FQueueBinary) then
    FQueueBinary := TsgcTCPObjectList.Create;
  Result := FQueueBinary;
end;

function TsgcTCPComponent.GetQueueMessage: TsgcTCPObjectList;
begin
  if not Assigned(FQueueMessage) then
    FQueueMessage := TsgcTCPObjectList.Create;
  Result := FQueueMessage;
end;

procedure TsgcTCPComponent.QueueClear;
begin
  DoClearQueue;
end;

procedure TsgcTCPComponent.SetQueueMessages(const Value: Boolean);
begin
  if not FQueueProcessing then
  begin
    FQueueMessages := Value;
    if not FQueueMessages then
      DoProcessQueue;
  end;
end;

destructor TsgcTCPNotifyObject.Destroy;
begin
  RawException := nil;
  sgcFree(FStream);
  inherited;
end;

function TsgcTCPNotifyObject.GetStream: TMemoryStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;
  Result := FStream;
end;

procedure TsgcTCPNotifyObject.SetRawException(const Value: Exception);
begin
  FRawException := Value;
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection;
aParam: String);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := aParam;

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection;
aParam: Integer);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := '';
  oComponent.Int := aParam;

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection;
aParam: TMemoryStream);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Stream.LoadFromStream(aParam);

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection;
aParam: String; aParam1: String);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := aParam;
  oComponent.Text1 := aParam1;

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection;
aStream: TMemoryStream; aOpCode: TOpCode; aBoolean: Boolean);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Stream.LoadFromStream(aStream);
  oComponent.Int := Ord(aOpCode);
  oComponent.Text1 := '0';
  if aBoolean then
    oComponent.Text1 := '1';

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.AddNotifyObject(aConnection: TsgcTCPConnection;
aParam: String; aException: Exception);
var
  oComponent: TsgcTCPNotifyObject;
begin
  oComponent := TsgcTCPNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := aParam;
  oComponent.RawException := aException;

  DoAdd(oComponent);
end;

procedure TsgcTCPObjectList.DeleteAll(aFreeConnection: Boolean = False);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
  oObject: TsgcTCPNotifyObject;
begin
  oList := LockList;
  Try
    while oList.Count > 0 do
    begin
      oObject := TsgcTCPNotifyObject(oList.Items[0]);
      if aFreeConnection then
      begin
        if Assigned(oObject.Connection) then
        begin
          if not oObject.Connection.FFreed then
          begin
            Try
              oObject.Connection.Free;
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

procedure TsgcTCPObjectList.DoAdd(Item: Pointer);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcTCPNotifyObject>{$ENDIF};
begin
  oList := LockList;
  Try
    oList.Add(Item);
  Finally
    UnLockList;
  End;
end;

end.
