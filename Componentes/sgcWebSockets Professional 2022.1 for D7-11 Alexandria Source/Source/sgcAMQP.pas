{ ***************************************************************************
  sgcAMQP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcAMQP;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // sgc
  sgcAMQP_Classes, sgcAMQP_Const, sgcBase_Helpers;

type
  TsgcAMQPBeforeReadFrameEvent = procedure(Sender: TObject;
    const aFrame: TsgcAMQPFrame; var Handled: Boolean) of object;
  TsgcAMQPWriteBytesEvent = procedure(Sender: TObject; const aBytes: TBytes)
    of object;
  TsgcAMQPAuthenticationEvent = procedure(Sender: TObject;
    aMechanisms: TsgcAMQPAuthentications; var Mechanism: TsgcAMQPAuthentication;
    var User, Password: string) of object;
  TsgcAMQPChallengeEvent = procedure(Sender: TObject; const aChallenge: String;
    var Challenge: String) of object;
  TsgcAMQPConnectEvent = procedure(Sender: TObject) of object;
  TsgcAMQPHeartBeatEvent = procedure(Sender: TObject) of object;
  TsgcAMQPDisconnectEvent = procedure(Sender: TObject;
    const aClose: TsgcAMQPFramePayload_Method_ConnectionClose; aAck: Boolean)
    of object;
  TsgcAMQPChannelOpenEvent = procedure(Sender: TObject; const aChannel: string)
    of object;
  TsgcAMQPChannelCloseEvent = procedure(Sender: TObject; const aChannel: string;
    const aChannelClose: TsgcAMQPFramePayload_Method_ChannelClose;
    aAck: Boolean) of object;
  TsgcAMQPChannelFlowEvent = procedure(Sender: TObject; const aChannel: string;
    aFlow: Boolean; aAck: Boolean) of object;
  TsgcAMQPExchangeDeclareEvent = procedure(Sender: TObject;
    const aChannel, aExchange: string) of object;
  TsgcAMQPExchangeDeleteEvent = procedure(Sender: TObject;
    const aChannel, aExchange: string) of object;
  TsgcAMQPQueueDeclareEvent = procedure(Sender: TObject;
    const aChannel, aQueue: string; aMessageCount, aConsumerCount: Integer)
    of object;
  TsgcAMQPQueueBindEvent = procedure(Sender: TObject;
    const aChannel, aQueue, aExchange: string) of object;
  TsgcAMQPQueueUnBindEvent = procedure(Sender: TObject;
    const aChannel, aQueue, aExchange: string) of object;
  TsgcAMQPQueuePurgeEvent = procedure(Sender: TObject;
    const aChannel, aQueue: string; aMessageCount: Integer) of object;
  TsgcAMQPQueueDeleteEvent = procedure(Sender: TObject;
    const aChannel, aQueue: string; aMessageCount: Integer) of object;
  TsgcAMQPBasicQoSEvent = procedure(Sender: TObject; const aChannel: string;
    const aQoS: TsgcAMQPFramePayload_Method_BasicQoS) of object;
  TsgcAMQPBasicConsumeEvent = procedure(Sender: TObject; const aChannel,
      aConsumerTag: string) of object;
  TsgcAMQPBasicCancelConsumeEvent = procedure(Sender: TObject;
    const aChannel, aConsumerTag: string) of object;
  TsgcAMQPBasicReturnEvent = procedure(Sender: TObject; const aChannel: string;
    const aReturn: TsgcAMQPFramePayload_Method_BasicReturn;
    const aContent: TsgcAMQPMessageContent) of object;
  TsgcAMQPBasicDeliverEvent = procedure(Sender: TObject; const aChannel: string;
    const aDeliver: TsgcAMQPFramePayload_Method_BasicDeliver;
    const aContent: TsgcAMQPMessageContent) of object;
  TsgcAMQPBasicGetOkEvent = procedure(Sender: TObject; const aChannel: string;
    const aGetOk: TsgcAMQPFramePayload_Method_BasicGetOk;
    const aContent: TsgcAMQPMessageContent) of object;
  TsgcAMQPBasicGetEmptyEvent = procedure(Sender: TObject;
    const aChannel: string) of object;
  TsgcAMQPBasicRecoverOkEvent = procedure(Sender: TObject;
    const aChannel: string) of object;
  TsgcAMQPTransactionOkEvent = procedure(Sender: TObject;
    const aChannel: string; aTransaction: TsgcAMQPTransaction) of object;
  TsgcAMQPExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  TsgcAMQPRead = (amqpReadWait, amqpReadWaitData);

  TsgcAMQPHeartBeat = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
  end;

  TsgcAMQPOptions = class(TPersistent)
  private
    FLocale: string;
    FMaxChannels: Word;
    FMaxFrameSize: Cardinal;
    FVirtualHost: string;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Locale: string read FLocale write FLocale;
    property MaxChannels: Word read FMaxChannels write FMaxChannels;
    property MaxFrameSize: Cardinal read FMaxFrameSize write FMaxFrameSize;
    property VirtualHost: string read FVirtualHost write FVirtualHost;
  end;

  TsgcAMQPNegotiatedValues = class
  private
    FHeartBeat: Word;
    FLocale: String;
    FMaxChannels: Word;
    FMaxFrameSize: Cardinal;
  public
    property HeartBeat: Word read FHeartBeat write FHeartBeat;
    property Locale: String read FLocale write FLocale;
    property MaxChannels: Word read FMaxChannels write FMaxChannels;
    property MaxFrameSize: Cardinal read FMaxFrameSize write FMaxFrameSize;
  end;

  TsgcAMQP = class(TComponent)
    { helpers }
  protected
    function GetChannel(const aChannel: string;
      aRaiseIfNotFound: Boolean = True): TsgcAMQPChannelThreadItem; virtual;
    { helpers }

    { channel threads }
  private
    FChannels: TsgcAMQPChannelThreads;
    function GetChannels: TsgcAMQPChannelThreads;
  protected
    procedure OnChannelExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property Channels: TsgcAMQPChannelThreads read GetChannels write FChannels;
    { channel threads }

    { wait }
  protected
    procedure DoInitWaitAMQPMethod(const aChannel: string; aMethods:
        TsgcAMQPMethods);
    function DoWaitAMQPMethod(const aChannel: string; const aTimeout: Integer =
        CS_AMQP_DEFAULT_TIMEOUT; aMethodsFalse: TsgcAMQPMethods = []): Boolean;
    { wait }

    { read }
  private
    FBytes: TBytes;
    // ... connection
  private
    FConnectionClose: TsgcAMQPFramePayload_Method_ConnectionClose;
    function GetConnectionClose: TsgcAMQPFramePayload_Method_ConnectionClose;
  protected
    FConnected: Boolean;
  protected
    property ConnectionClose: TsgcAMQPFramePayload_Method_ConnectionClose
      read GetConnectionClose write FConnectionClose;
  private
    procedure DoRead_ConnClose(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionClose);
    procedure DoRead_ConnCloseOk(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionCloseOk);
    // ... channels
  private
    procedure DoRead_ChannFlow(aChannelId: Word;
      const aPayload: TsgcAMQPFramePayload_Method_ChannelFlow);
    procedure DoRead_ChannFlowOk(aChannelId: Word;
      const aPayload: TsgcAMQPFramePayload_Method_ChannelFlowOk);
    procedure DoRead_ChannClose(aChannelId: Word;
      const aPayload: TsgcAMQPFramePayload_Method_ChannelClose);
    procedure DoRead_ChannCloseOk(aChannelId: Word;
      const aPayload: TsgcAMQPFramePayload_Method_ChannelCloseOk);
  protected
    procedure OnReadFrameEvent(Sender: TObject); virtual;
    procedure DoReadFrameMethod(const aFrame: TsgcAMQPFrame;
      const aMethod: TsgcAMQPFramePayloadType_Method); virtual;
    procedure DoReadFrameHeader(const aFrame: TsgcAMQPFrame;
      const aHeader: TsgcAMQPFramePayloadType_ContentHeader); virtual; abstract;
    procedure DoReadFrameBody(const aFrame: TsgcAMQPFrame;
      const aBody: TsgcAMQPFramePayloadType_ContentBody); virtual; abstract;
    procedure DoReadFrameHeartBeat(const aFrame: TsgcAMQPFrame); virtual;
  protected
    procedure DoRead(const aBytes: TBytes); virtual;
  public
    procedure Read(const aData: TMemoryStream); overload;
    procedure Read(const aBytes: TBytes); overload;
    { read }

    { write }
    // ... connection
  private
    procedure DoWrite_ConnClose(aReplyCode: Word; const aReplyText: string;
      aFailClassId: TsgcAMQPClass; aFailMethodId: TsgcAMQPMethod);
    procedure DoWrite_ConnCloseOk;
  public
    procedure Ping;
  public
    procedure Close(aReplyCode: Word; const aReplyText: string;
      aFailClassId: TsgcAMQPClass; aFailMethodId: TsgcAMQPMethod);
    function CloseEx(aReplyCode: Word; const aReplyText: string;
      aFailClassId: TsgcAMQPClass; aFailMethodId: TsgcAMQPMethod;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... channels
  private
    procedure DoWrite_ChannClose(aChannelId: Word; aReplyCode: Word;
      const aReplyText: string; aFailClassId: TsgcAMQPClass;
      aFailMethodId: TsgcAMQPMethod);
    procedure DoWrite_ChannCloseOk;
    procedure DoWrite_ChannFlow(aChannelId: Word; aActive: Boolean);
    procedure DoWrite_ChannFlowOk(aChannelId: Word; aActive: Boolean);
  public
    procedure CloseChannel(const aChannel: string; aReplyCode: Word;
      const aReplyText: string; aFailClassId: TsgcAMQPClass;
      aFailMethodId: TsgcAMQPMethod);
    function CloseChannelEx(const aChannel: string; aReplyCode: Word;
      const aReplyText: string; aFailClassId: TsgcAMQPClass;
      aFailMethodId: TsgcAMQPMethod;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure EnableChannel(const aChannel: string);
    function EnableChannelEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure DisableChannel(const aChannel: string);
    function DisableChannelEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    { write }

    { properties }
  private
    FHeartBeat: TsgcAMQPHeartBeat;
    FAMQPOptions: TsgcAMQPOptions;
    FAMQPNegotiatedValues: TsgcAMQPNegotiatedValues;
    function GetAMQPNegotiatedValues: TsgcAMQPNegotiatedValues;
    procedure SetAMQPOptions(const Value: TsgcAMQPOptions);
    procedure SetHeartBeat(const Value: TsgcAMQPHeartBeat);
  public
    property AMQPOptions: TsgcAMQPOptions read FAMQPOptions
      write SetAMQPOptions;
    property HeartBeat: TsgcAMQPHeartBeat read FHeartBeat write SetHeartBeat;
    property AMQPNegotiatedValues: TsgcAMQPNegotiatedValues
      read GetAMQPNegotiatedValues write FAMQPNegotiatedValues;
    { properties }

    { constructor / destructor }
  protected
    procedure DoClear; virtual;
  public
    procedure Clear;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { events }
  protected
    FOnAMQPBeforeReadFrame: TsgcAMQPBeforeReadFrameEvent;
    FOnAMQPChannelOpen: TsgcAMQPChannelOpenEvent;
    FOnAMQPChannelClose: TsgcAMQPChannelCloseEvent;
    FOnAMQPDisconnect: TsgcAMQPDisconnectEvent;
    FOnAMQPHeartBeat: TsgcAMQPHeartBeatEvent;
    FOnAMQPWriteBytes: TsgcAMQPWriteBytesEvent;
    FOnAMQPChannelFlow: TsgcAMQPChannelFlowEvent;
    FOnAMQPException: TsgcAMQPExceptionEvent;
  protected
    procedure DoWriteBytes(const aBytes: TBytes); virtual;
    procedure DoAMQPExceptionEvent(E: Exception); virtual;
  public
    property OnAMQPBeforeReadFrame: TsgcAMQPBeforeReadFrameEvent
      read FOnAMQPBeforeReadFrame write FOnAMQPBeforeReadFrame;
    property OnAMQPChannelClose: TsgcAMQPChannelCloseEvent
      read FOnAMQPChannelClose write FOnAMQPChannelClose;
    property OnAMQPChannelFlow: TsgcAMQPChannelFlowEvent read FOnAMQPChannelFlow
      write FOnAMQPChannelFlow;
    property OnAMQPDisconnect: TsgcAMQPDisconnectEvent read FOnAMQPDisconnect
      write FOnAMQPDisconnect;
    property OnAMQPHeartBeat: TsgcAMQPHeartBeatEvent read FOnAMQPHeartBeat
      write FOnAMQPHeartBeat;
    property OnAMQPWriteBytes: TsgcAMQPWriteBytesEvent read FOnAMQPWriteBytes
      write FOnAMQPWriteBytes;
    property OnAMQPChannelOpen: TsgcAMQPChannelOpenEvent read FOnAMQPChannelOpen
      write FOnAMQPChannelOpen;
    property OnAMQPException: TsgcAMQPExceptionEvent read FOnAMQPException
      write FOnAMQPException;
    { events }
  end;

implementation

uses
  sgcAMQP_Helpers;

constructor TsgcAMQP.Create(aOwner: TComponent);
begin
  inherited;
  FHeartBeat := TsgcAMQPHeartBeat.Create;
  FAMQPOptions := TsgcAMQPOptions.Create;
end;

destructor TsgcAMQP.Destroy;
begin
  sgcFree(FAMQPOptions);
  sgcFree(FHeartBeat);
  sgcFree(FChannels);
  sgcFree(FConnectionClose);
  sgcFree(FAMQPNegotiatedValues);
  inherited;
end;

procedure TsgcAMQP.Clear;
begin
  DoClear;
end;

procedure TsgcAMQP.DoClear;
begin
  sgcFree(FConnectionClose);
  sgcFree(FAMQPNegotiatedValues);
end;

procedure TsgcAMQP.Close(aReplyCode: Word; const aReplyText: string;
  aFailClassId: TsgcAMQPClass; aFailMethodId: TsgcAMQPMethod);
begin
  DoWrite_ConnClose(aReplyCode, aReplyText, aFailClassId, aFailMethodId);
end;

procedure TsgcAMQP.CloseChannel(const aChannel: string; aReplyCode: Word;
  const aReplyText: string; aFailClassId: TsgcAMQPClass;
  aFailMethodId: TsgcAMQPMethod);
begin
  DoWrite_ChannClose(StrToInt(GetChannel(aChannel).ID), aReplyCode, aReplyText,
    aFailClassId, aFailMethodId)
end;

function TsgcAMQP.CloseChannelEx(const aChannel: string; aReplyCode: Word;
  const aReplyText: string; aFailClassId: TsgcAMQPClass;
  aFailMethodId: TsgcAMQPMethod;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpChannCloseOk]);

  CloseChannel(aChannel, aReplyCode, aReplyText, aFailClassId, aFailMethodId);

  Result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

function TsgcAMQP.CloseEx(aReplyCode: Word; const aReplyText: string;
  aFailClassId: TsgcAMQPClass; aFailMethodId: TsgcAMQPMethod;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod('', [amqpConnCloseOk]);

  Close(aReplyCode, aReplyText, aFailClassId, aFailMethodId);

  Result := DoWaitAMQPMethod('', aTimeout);
end;

procedure TsgcAMQP.DisableChannel(const aChannel: string);
begin
  DoWrite_ChannFlow(StrToInt(GetChannel(aChannel).ID), False)
end;

function TsgcAMQP.DisableChannelEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpChannFlowOk]);

  DisableChannel(aChannel);

  Result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP.DoAMQPExceptionEvent(E: Exception);
begin
  if Assigned(FOnAMQPException) then
    FOnAMQPException(self, E);

  if FConnected then
  begin
    if E.ClassType = TsgcAMQPException then
      Close(TsgcAMQPException(E).Code, E.Message, amqpClassNone, amqpMethodNone);
  end;
end;

procedure TsgcAMQP.DoInitWaitAMQPMethod(const aChannel: string; aMethods:
    TsgcAMQPMethods);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := GetChannel(aChannel);
  oChannel.WaitRequest.Initialize;
  oChannel.WaitRequest.Methods := aMethods;
end;

procedure TsgcAMQP.Ping;
var
  oFrame: TsgcAMQPFrame;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameHeartBeat;
    oFrame.Header.Channel := 0;
    oFrame.Header.Size := 0;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.DoRead(const aBytes: TBytes);
var
  i, n: Integer;
  vBytes: TBytes;
  vOffset: Integer;
  oFrame: TsgcAMQPFrame;
begin
  Try
    // ... copy remaining bytes if required
    if Length(FBytes) > 0 then
    begin
      SetLength(vBytes, Length(aBytes) + Length(FBytes));
      sgcMove(FBytes[0], vBytes[0], Length(FBytes));
      sgcMove(aBytes[0], vBytes[Length(FBytes)], Length(aBytes));
      SetLength(FBytes, 0);
    end
    else
      vBytes := aBytes;

    // ... read bytes
    vOffset := 0;
    n := Length(vBytes);
    While (n - vOffset) > 7 do
    begin
      oFrame := TsgcAMQPFrame.Create;
      oFrame.MaxSize := AMQPNegotiatedValues.MaxFrameSize;
      oFrame.OnReadFrame := OnReadFrameEvent;
      i := oFrame.Read(vBytes, vOffset);
      vOffset := vOffset + i;

      // add frame to channel read thread
      if i > 0 then
        Channels.AddFrame(oFrame)
      else
        sgcFree(oFrame);

      // end of read
      if vOffset = n then
        exit
        // more bytes are required
      else if i = 0 then
      begin
        SetLength(FBytes, Length(vBytes) - vOffset);
        sgcMove(vBytes[vOffset], FBytes[0], Length(FBytes));
        exit;
      end
    end;
  Except
    On E: Exception do
      DoAMQPExceptionEvent(E);
  End;
end;

procedure TsgcAMQP.DoReadFrameHeartBeat(const aFrame: TsgcAMQPFrame);
begin
  if aFrame.Header.Size > 0 then
    DoRaiseAMQPException(CS_AMQP_ERROR_FRAME);
end;

procedure TsgcAMQP.DoReadFrameMethod(const aFrame: TsgcAMQPFrame;
  const aMethod: TsgcAMQPFramePayloadType_Method);
begin
  case aMethod.ClassId of
    amqpClassConnection:
      case aMethod.MethodId of
        amqpConnClose:
          DoRead_ConnClose(TsgcAMQPFramePayload_Method_ConnectionClose
            (aMethod.Payload));
        amqpConnCloseOk:
          DoRead_ConnCloseOk(TsgcAMQPFramePayload_Method_ConnectionCloseOk
            (aMethod.Payload));
      end;
    amqpClassChannel:
      case aMethod.MethodId of
        amqpChannFlow:
          DoRead_ChannFlow(aFrame.Header.Channel,
            TsgcAMQPFramePayload_Method_ChannelFlow(aMethod.Payload));
        amqpChannFlowOk:
          DoRead_ChannFlowOk(aFrame.Header.Channel,
            TsgcAMQPFramePayload_Method_ChannelFlowOk(aMethod.Payload));
        amqpChannClose:
          DoRead_ChannClose(aFrame.Header.Channel,
            TsgcAMQPFramePayload_Method_ChannelClose(aMethod.Payload));
        amqpChannCloseOk:
          DoRead_ChannCloseOk(aFrame.Header.Channel,
            TsgcAMQPFramePayload_Method_ChannelCloseOk(aMethod.Payload));
      end;
    amqpClassExchange:
      ;
    amqpClassQueue:
      ;
    amqpClassBasic:
      ;
    amqpClassTx:
      ;
  end;
end;

procedure TsgcAMQP.DoRead_ChannClose(aChannelId: Word;
  const aPayload: TsgcAMQPFramePayload_Method_ChannelClose);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  DoWrite_ChannCloseOk;

  oChannel := Channels.GetChannel(aChannelId);
  if Assigned(oChannel) then
  begin
    oChannel.ChannelClose := aPayload;
    oChannel.MethodId := amqpChannClose;

    if Assigned(FOnAMQPChannelClose) then
      FOnAMQPChannelClose(self, oChannel.Channel, aPayload, False);
  end;
end;

procedure TsgcAMQP.DoRead_ChannCloseOk(aChannelId: Word;
  const aPayload: TsgcAMQPFramePayload_Method_ChannelCloseOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aChannelId);
  if Assigned(oChannel) then
  begin
    oChannel.MethodId := amqpChannCloseOk;
    if Assigned(FOnAMQPChannelClose) then
      FOnAMQPChannelClose(self, oChannel.Channel, oChannel.ChannelClose, True);
    oChannel.Terminate;
  end;
end;

procedure TsgcAMQP.DoRead_ChannFlow(aChannelId: Word;
  const aPayload: TsgcAMQPFramePayload_Method_ChannelFlow);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  DoWrite_ChannFlowOk(aChannelId, aPayload.Active);

  oChannel := Channels.GetChannel(aChannelId);
  if Assigned(oChannel) then
  begin
    oChannel.Flow := aPayload.Active;

    if Assigned(FOnAMQPChannelFlow) then
      FOnAMQPChannelFlow(self, oChannel.Channel, aPayload.Active, False);
  end;
end;

procedure TsgcAMQP.DoRead_ChannFlowOk(aChannelId: Word;
  const aPayload: TsgcAMQPFramePayload_Method_ChannelFlowOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aChannelId);
  if Assigned(oChannel) then
  begin
    oChannel.Flow := aPayload.Active;
    if Assigned(FOnAMQPChannelFlow) then
      FOnAMQPChannelFlow(self, oChannel.Channel, aPayload.Active, True);
  end;
end;

procedure TsgcAMQP.DoRead_ConnClose(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionClose);
begin
  DoWrite_ConnCloseOk;

  ConnectionClose.ReplyCode := aPayload.ReplyCode;
  ConnectionClose.ReplyText := aPayload.ReplyText;
  ConnectionClose.FailClassId := aPayload.FailClassId;
  ConnectionClose.FailMethodId := aPayload.FailMethodId;

  if Assigned(FOnAMQPDisconnect) then
    FOnAMQPDisconnect(self, ConnectionClose, False);
end;

procedure TsgcAMQP.DoRead_ConnCloseOk(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionCloseOk);
begin
  if Assigned(FOnAMQPDisconnect) then
    FOnAMQPDisconnect(self, ConnectionClose, True);
end;

function TsgcAMQP.DoWaitAMQPMethod(const aChannel: string; const aTimeout:
    Integer = CS_AMQP_DEFAULT_TIMEOUT; aMethodsFalse: TsgcAMQPMethods = []):
    Boolean;
var
  oChannel: TsgcAMQPChannelThreadItem;
  vStart: Cardinal;
begin
  Result := False;

  oChannel := GetChannel(aChannel);
  Try
    // ... wait response
    vStart := sgcGetTicks;
    if aTimeout > 0 then
    begin
      repeat
        if not FConnected then
          exit;
        if oChannel.WaitRequest.Terminated then
        begin
          Result := not (oChannel.WaitRequest.Method in aMethodsFalse);
          break;
        end;
        sleep(1);
      until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeout);
    end;
  Finally
    oChannel.WaitRequest.Initialize;
  End;
end;

procedure TsgcAMQP.DoWriteBytes(const aBytes: TBytes);
begin
  if Assigned(FOnAMQPWriteBytes) then
    FOnAMQPWriteBytes(self, aBytes);
end;

procedure TsgcAMQP.DoWrite_ChannClose(aChannelId: Word; aReplyCode: Word;
  const aReplyText: string; aFailClassId: TsgcAMQPClass;
  aFailMethodId: TsgcAMQPMethod);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ChannelClose;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := aChannelId;
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ChannelClose.Create;

    oPayload.ReplyCode := aReplyCode;
    oPayload.ReplyText := aReplyText;
    oPayload.FailClassId := aFailClassId;
    oPayload.FailMethodId := aFailMethodId;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.DoWrite_ChannCloseOk;
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ChannelCloseOk;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ChannelCloseOk.Create;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.DoWrite_ChannFlow(aChannelId: Word; aActive: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ChannelFlow;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oFrame.Header.Channel := aChannelId;
    oPayload := TsgcAMQPFramePayload_Method_ChannelFlow.Create;
    oPayload.Active := aActive;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.DoWrite_ChannFlowOk(aChannelId: Word; aActive: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ChannelFlowOk;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oFrame.Header.Channel := aChannelId;
    oPayload := TsgcAMQPFramePayload_Method_ChannelFlowOk.Create;
    oPayload.Active := aActive;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.DoWrite_ConnClose(aReplyCode: Word; const aReplyText: string;
  aFailClassId: TsgcAMQPClass; aFailMethodId: TsgcAMQPMethod);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ConnectionClose;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ConnectionClose.Create;
    oPayload.ReplyCode := aReplyCode;
    oPayload.ReplyText := aReplyText;
    oPayload.FailClassId := aFailClassId;
    oPayload.FailMethodId := aFailMethodId;

    ConnectionClose.ReplyCode := aReplyCode;
    ConnectionClose.ReplyText := aReplyText;
    ConnectionClose.FailClassId := aFailClassId;
    ConnectionClose.FailMethodId := aFailMethodId;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.DoWrite_ConnCloseOk;
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ConnectionCloseOk;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ConnectionCloseOk.Create;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP.EnableChannel(const aChannel: string);
begin
  DoWrite_ChannFlow(StrToInt(GetChannel(aChannel).ID), True)
end;

function TsgcAMQP.EnableChannelEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpChannFlowOk]);

  EnableChannel(aChannel);

  Result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

function TsgcAMQP.GetAMQPNegotiatedValues: TsgcAMQPNegotiatedValues;
begin
  if not Assigned(FAMQPNegotiatedValues) then
  begin
    FAMQPNegotiatedValues := TsgcAMQPNegotiatedValues.Create;
    if HeartBeat.Enabled then
      FAMQPNegotiatedValues.HeartBeat := HeartBeat.Interval
    else
      FAMQPNegotiatedValues.HeartBeat := 0;
    FAMQPNegotiatedValues.MaxChannels := AMQPOptions.MaxChannels;
    FAMQPNegotiatedValues.MaxFrameSize := AMQPOptions.MaxFrameSize;
    FAMQPNegotiatedValues.Locale := AMQPOptions.Locale;
  end;
  Result := FAMQPNegotiatedValues;
end;

function TsgcAMQP.GetChannel(const aChannel: string;
  aRaiseIfNotFound: Boolean = True): TsgcAMQPChannelThreadItem;
begin
  Result := Channels.GetChannel(aChannel);
  if not Assigned(Result) then
    raise Exception.CreateFmt(S_AMQP_ERROR_CHANNEL_NOT_EXISTS, [aChannel]);
end;

function TsgcAMQP.GetChannels: TsgcAMQPChannelThreads;
begin
  if not Assigned(FChannels) then
  begin
    FChannels := TsgcAMQPChannelThreads.Create;
    FChannels.OnException := OnChannelExceptionEvent;
  end;
  Result := FChannels;
end;

function TsgcAMQP.GetConnectionClose
  : TsgcAMQPFramePayload_Method_ConnectionClose;
begin
  if not Assigned(FConnectionClose) then
    FConnectionClose := TsgcAMQPFramePayload_Method_ConnectionClose.Create;
  Result := FConnectionClose;
end;

procedure TsgcAMQP.OnChannelExceptionEvent(Sender: TObject; E: Exception);
begin
  if Assigned(FOnAMQPException) then
    FOnAMQPException(self, E);
end;

procedure TsgcAMQP.OnReadFrameEvent(Sender: TObject);
var
  vHandled: Boolean;
  oFrame: TsgcAMQPFrame;
begin
  oFrame := TsgcAMQPFrame(Sender);

  vHandled := False;
  if Assigned(FOnAMQPBeforeReadFrame) then
  begin
    FOnAMQPBeforeReadFrame(self, oFrame, vHandled);
    if vHandled then
      exit;
  end;

  case oFrame.Header._Type of
    amqpFrameMethod:
      DoReadFrameMethod(oFrame, oFrame.Payload_Method);
    amqpFrameHeader:
      DoReadFrameHeader(oFrame, oFrame.Payload_ContentHeader);
    amqpFrameBody:
      DoReadFrameBody(oFrame, oFrame.Payload_ContentBody);
    amqpFrameHeartBeat:
      begin
        if HeartBeat.Enabled then
        begin
          DoReadFrameHeartBeat(oFrame);
          if Assigned(FOnAMQPHeartBeat) then
            FOnAMQPHeartBeat(self);
        end;
      end;
  end;
end;

procedure TsgcAMQP.Read(const aData: TMemoryStream);
begin
  DoRead(sgcReadAMQPBytes(aData, aData.Size));
end;

procedure TsgcAMQP.Read(const aBytes: TBytes);
begin
  DoRead(aBytes);
end;

procedure TsgcAMQP.SetAMQPOptions(const Value: TsgcAMQPOptions);
begin
  if Assigned(FAMQPOptions) then
    FAMQPOptions.Assign(Value);
end;

procedure TsgcAMQP.SetHeartBeat(const Value: TsgcAMQPHeartBeat);
begin
  if Assigned(FHeartBeat) then
    FHeartBeat.Assign(Value);
end;

procedure TsgcAMQPHeartBeat.Assign(aSource: TPersistent);
begin
  if aSource is TsgcAMQPHeartBeat then
  begin
    Enabled := TsgcAMQPHeartBeat(aSource).Enabled;
    Interval := TsgcAMQPHeartBeat(aSource).Interval;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcAMQPOptions.Create;
begin
  inherited;
  FVirtualHost := CS_AMQP_DEFAULT_VIRTUAL_HOST;
  FMaxChannels := CS_AMQP_DEFAULT_MAX_CHANNELS;
  FMaxFrameSize := CS_AMQP_DEFAULT_MAX_FRAME_SIZE;
  FLocale := CS_AMQP_DEFAULT_LOCALE;
end;

procedure TsgcAMQPOptions.Assign(aSource: TPersistent);
begin
  if aSource is TsgcAMQPOptions then
  begin
    MaxChannels := TsgcAMQPOptions(aSource).MaxChannels;
    MaxFrameSize := TsgcAMQPOptions(aSource).MaxFrameSize;
    Locale := TsgcAMQPOptions(aSource).Locale;
    VirtualHost := TsgcAMQPOptions(aSource).VirtualHost;
  end
  else
    inherited Assign(aSource);
end;

end.
