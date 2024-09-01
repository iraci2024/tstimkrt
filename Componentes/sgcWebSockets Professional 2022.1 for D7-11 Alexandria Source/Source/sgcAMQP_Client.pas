{ ***************************************************************************
  sgcAMQP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcAMQP_Client;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // sgc
  sgcAMQP, sgcAMQP_Classes, sgcAMQP_Const, sgcBase_Helpers;

type
  TsgcAMQP_Client = class(TsgcAMQP)
    { read }
    // ... connection
  protected
    procedure DoRead_ConnStart(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionStart); virtual;
    procedure DoRead_ConnSecure(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionSecure); virtual;
    procedure DoRead_ConnTune(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionTune); virtual;
    procedure DoRead_ConnOpenOk(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionOpenOk); virtual;
    // ... channel
  protected
    procedure DoRead_ChannOpenOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_ChannelOpenOk); virtual;
    // ... exchange
  protected
    procedure DoRead_ExchDeclareOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_ExchangeDeclareOk); virtual;
    procedure DoRead_ExchDeleteOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_ExchangeDeleteOk); virtual;
    // ... queue
    procedure DoRead_QueueDeclareOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_QueueDeclareOk); virtual;
    procedure DoRead_QueueBindOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_QueueBindOk); virtual;
    procedure DoRead_QueueUnBindOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_QueueUnBindOk); virtual;
    procedure DoRead_QueuePurgeOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_QueuePurgeOk); virtual;
    procedure DoRead_QueueDeleteOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_QueueDeleteOk); virtual;
    // ... basic
  protected
    procedure DoRead_BasicQoSOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicQoSOk); virtual;
    procedure DoRead_BasicConsumeOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicConsumeOk); virtual;
    procedure DoRead_BasicCancelOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicCancelOk); virtual;
    procedure DoRead_BasicReturn(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicReturn); virtual;
    procedure DoRead_BasicDeliver(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicDeliver); virtual;
    procedure DoRead_BasicGetOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicGetOk); virtual;
    procedure DoRead_BasicGetEmpty(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicGetEmpty); virtual;
    procedure DoRead_BasicRecoverOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_BasicRecoverOk); virtual;
    // ... tx
  protected
    procedure DoRead_TxSelectOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_TxSelectOk); virtual;
    procedure DoRead_TxCommitOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_TxCommitOk); virtual;
    procedure DoRead_TxRollbackOk(const aFrame: TsgcAMQPFrame;
      const aPayload: TsgcAMQPFramePayload_Method_TxRollbackOk); virtual;
  protected
    procedure DoMessageContentEvent(const aChannel: TsgcAMQPChannelThreadItem);
  protected
    procedure DoReadFrameMethod(const aFrame: TsgcAMQPFrame;
      const aMethod: TsgcAMQPFramePayloadType_Method); override;
    procedure DoReadFrameHeader(const aFrame: TsgcAMQPFrame;
      const aHeader: TsgcAMQPFramePayloadType_ContentHeader); override;
    procedure DoReadFrameBody(const aFrame: TsgcAMQPFrame;
      const aBody: TsgcAMQPFramePayloadType_ContentBody); override;
    { read }

    { write }
    // ... connection
  private
    procedure DoWrite_ConnSecureOk(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionSecure);
    procedure DoWrite_ConnStartOk(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionStart);
    procedure DoWrite_ConnTuneOk(const aPayload
      : TsgcAMQPFramePayload_Method_ConnectionTune);
    procedure DoWrite_ConnOpen;
  public
    procedure Connect;
    procedure Disconnect;
    // ... channel
  private
    procedure DoWrite_ChannOpen(aChannelId: Word);
  public
    procedure OpenChannel(const aChannel: string);
    function OpenChannelEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... exchange
  private
    procedure DoWrite_ExchDeclare(const aChannel: TsgcAMQPChannelThreadItem;
      const aExchange, aExchangeType: string; aPassive, aDurable, aAutoDelete,
      aInternal, aNoWait: Boolean);
    procedure DoWrite_ExchDelete(const aChannel: TsgcAMQPChannelThreadItem;
      const aExchange: string; aIfUnused, aNoWait: Boolean);
  public
    procedure DeclareExchange(const aChannel, aExchange, aExchangeType: string;
      aPassive, aDurable, aAutoDelete, aInternal, aNoWait: Boolean);
    function DeclareExchangeEx(const aChannel, aExchange, aExchangeType: string;
      aPassive, aDurable, aAutoDelete, aInternal, aNoWait: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure DeleteExchange(const aChannel, aExchange: string;
      aIfUnused, aNoWait: Boolean);
    function DeleteExchangeEx(const aChannel, aExchange: string;
      aIfUnused, aNoWait: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... queue
  private
    procedure DoWrite_QueueDeclare(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue: string; aPassive, aDurable, aExclusive, aAutoDelete,
      aNoWait: Boolean);
    procedure DoWrite_QueueBind(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue, aExchange, aRoutingKey: string; aNoWait: Boolean);
    procedure DoWrite_QueueUnBind(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue, aExchange, aRoutingKey: string);
    procedure DoWrite_QueuePurge(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue: string; aNoWait: Boolean);
    procedure DoWrite_QueueDelete(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue: string; aIfUnused, aIfEmpty, aNoWait: Boolean);
  public
    procedure DeclareQueue(const aChannel, aQueue: string;
      aPassive, aDurable, aExclusive, aAutoDelete, aNoWait: Boolean);
    function DeclareQueueEx(const aChannel, aQueue: string;
      aPassive, aDurable, aExclusive, aAutoDelete, aNoWait: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure BindQueue(const aChannel, aQueue, aExchange, aRoutingKey: string;
      aNoWait: Boolean);
    function BindQueueEx(const aChannel, aQueue, aExchange, aRoutingKey: string;
      aNoWait: Boolean; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure UnBindQueue(const aChannel, aQueue, aExchange,
      aRoutingKey: string);
    function UnBindQueueEx(const aChannel, aQueue, aExchange,
      aRoutingKey: string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT)
      : Boolean;
    procedure PurgeQueue(const aChannel, aQueue: string; aNoWait: Boolean);
    function PurgeQueueEx(const aChannel, aQueue: string; aNoWait: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure DeleteQueue(const aChannel, aQueue: string;
      aIfUnused, aIfEmpty, aNoWait: Boolean);
    function DeleteQueueEx(const aChannel, aQueue: string;
      aIfUnused, aIfEmpty, aNoWait: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... basic
  private
    procedure DoWriteContentHeader(const aChannel: TsgcAMQPChannelThreadItem;
      aClassId: TsgcAMQPClass; const aHeader: TsgcAMQPBasicProperties;
      const aBody: TStream; var aBytes: TBytes);
    procedure DoWriteContentBody(const aChannel: TsgcAMQPChannelThreadItem;
      const aData: TStream; var aBytes: TBytes);
  private
    procedure DoWrite_BasicQoS(const aChannel: TsgcAMQPChannelThreadItem;
      aPrefetchSize: Cardinal; aPrefetchCount: Word; aGlobal: Boolean);
    procedure DoWrite_BasicConsume(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue, aConsumerTag: string; aNoLocal, aNoAck, aExclusive,
      aNoWait: Boolean);
    procedure DoWrite_BasicCancel(const aChannel: TsgcAMQPChannelThreadItem;
      const aConsumerTag: string; aNoWait: Boolean);
    procedure DoWrite_BasicPublish(const aChannel: TsgcAMQPChannelThreadItem;
      const aExchange, aRoutingKey: string;
      const aHeader: TsgcAMQPBasicProperties; const aBody: TStream;
      aMandatory, aImmediate: Boolean);
    procedure DoWrite_BasicGet(const aChannel: TsgcAMQPChannelThreadItem;
      const aQueue: string; aNoAck: Boolean);
    procedure DoWrite_BasicAck(const aChannel: TsgcAMQPChannelThreadItem;
      const aDeliveryTag: UInt64; aMultiple: Boolean);
    procedure DoWrite_BasicReject(const aChannel: TsgcAMQPChannelThreadItem;
      const aDeliveryTag: UInt64; aRequeue: Boolean);
    procedure DoWrite_BasicRecoverAsync(const aChannel
      : TsgcAMQPChannelThreadItem; aRequeue: Boolean);
    procedure DoWrite_BasicRecover(const aChannel: TsgcAMQPChannelThreadItem;
      aRequeue: Boolean);
  public
    procedure SetQoS(const aChannel: string; aPrefetchSize: Cardinal;
      aPrefetchCount: Word; aGlobal: Boolean);
    function SetQoSEx(const aChannel: string; aPrefetchSize: Cardinal;
      aPrefetchCount: Word; aGlobal: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure Consume(const aChannel, aQueue, aConsumerTag: string;
      aNoLocal, aNoAck, aExclusive, aNoWait: Boolean);
    function ConsumeEx(const aChannel, aQueue, aConsumerTag: string; aNoLocal,
        aNoAck, aExclusive: Boolean; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT):
        Boolean;
    procedure CancelConsume(const aChannel, aConsumerTag: string;
      aNoWait: Boolean);
    function CancelConsumeEx(const aChannel, aConsumerTag: string; aTimeout:
        Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey: string;
      const aHeader: TsgcAMQPBasicProperties; const aMessage: string;
      aMandatory: Boolean = False; aImmediate: Boolean = False); overload;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey: string; const
        aHeader: TsgcAMQPBasicProperties; const aMessage: TStream; aMandatory,
        aImmediate: Boolean); overload;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey,
      aMessage: string; aMandatory: Boolean = False;
      aImmediate: Boolean = False); overload;
    procedure PublishMessage(const aChannel, aExchange, aRoutingKey: string; const
        aMessage: TStream; aMandatory: Boolean = false; aImmediate: Boolean =
        false); overload;
    procedure GetMessage(const aChannel, aQueue: string; aNoAck: Boolean);
    function GetMessageEx(const aChannel, aQueue: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure AckMessage(const aChannel: string; aDeliveryTag: UInt64;
      aMultiple: Boolean);
    procedure RejectMessage(const aChannel: string; aDeliveryTag: UInt64;
      aRequeue: Boolean);
    procedure RecoverAsync(const aChannel: string; aRequeue: Boolean);
    procedure Recover(const aChannel: string; aRequeue: Boolean);
    function RecoverEx(const aChannel: string; aRequeue: Boolean;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    // ... tx
  private
    procedure DoWrite_TxSelect(const aChannel: TsgcAMQPChannelThreadItem);
    procedure DoWrite_TxCommit(const aChannel: TsgcAMQPChannelThreadItem);
    procedure DoWrite_TxRollback(const aChannel: TsgcAMQPChannelThreadItem);
  public
    procedure SelectTransaction(const aChannel: string);
    function SelectTransactionEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure CommitTransaction(const aChannel: string);
    function CommitTransactionEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    procedure RollbackTransaction(const aChannel: string);
    function RollbackTransactionEx(const aChannel: string;
      aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
    { write }

    { events }
  private
    FOnAMQPAuthentication: TsgcAMQPAuthenticationEvent;
    FOnAMQPBasicCancelConsume: TsgcAMQPBasicCancelConsumeEvent;
    FOnAMQPChallenge: TsgcAMQPChallengeEvent;
    FOnAMQPConnect: TsgcAMQPConnectEvent;
    FOnAMQPBasicConsume: TsgcAMQPBasicConsumeEvent;
    FOnAMQPBasicDeliver: TsgcAMQPBasicDeliverEvent;
    FOnAMQPBasicGetEmpty: TsgcAMQPBasicGetEmptyEvent;
    FOnAMQPBasicGetOk: TsgcAMQPBasicGetOkEvent;
    FOnAMQPExchangeDeclare: TsgcAMQPExchangeDeclareEvent;
    FOnAMQPExchangeDelete: TsgcAMQPExchangeDeleteEvent;
    FOnAMQPBasicQoS: TsgcAMQPBasicQoSEvent;
    FOnAMQPBasicReturn: TsgcAMQPBasicReturnEvent;
    FOnAMQPQueueBind: TsgcAMQPQueueBindEvent;
    FOnAMQPQueueDeclare: TsgcAMQPQueueDeclareEvent;
    FOnAMQPQueueDelete: TsgcAMQPQueueDeleteEvent;
    FOnAMQPQueuePurge: TsgcAMQPQueuePurgeEvent;
    FOnAMQPQueueUnBind: TsgcAMQPQueueUnBindEvent;
    FOnAMQPBasicRecoverOk: TsgcAMQPBasicRecoverOkEvent;
    FOnAMQPTransactionOk: TsgcAMQPTransactionOkEvent;
  public
    property OnAMQPAuthentication: TsgcAMQPAuthenticationEvent
      read FOnAMQPAuthentication write FOnAMQPAuthentication;
    property OnAMQPBasicCancelConsume: TsgcAMQPBasicCancelConsumeEvent
      read FOnAMQPBasicCancelConsume write FOnAMQPBasicCancelConsume;
    property OnAMQPChallenge: TsgcAMQPChallengeEvent read FOnAMQPChallenge
      write FOnAMQPChallenge;
    property OnAMQPConnect: TsgcAMQPConnectEvent read FOnAMQPConnect
      write FOnAMQPConnect;
    property OnAMQPBasicConsume: TsgcAMQPBasicConsumeEvent
      read FOnAMQPBasicConsume write FOnAMQPBasicConsume;
    property OnAMQPBasicDeliver: TsgcAMQPBasicDeliverEvent
      read FOnAMQPBasicDeliver write FOnAMQPBasicDeliver;
    property OnAMQPBasicGetEmpty: TsgcAMQPBasicGetEmptyEvent
      read FOnAMQPBasicGetEmpty write FOnAMQPBasicGetEmpty;
    property OnAMQPBasicGetOk: TsgcAMQPBasicGetOkEvent read FOnAMQPBasicGetOk
      write FOnAMQPBasicGetOk;
    property OnAMQPExchangeDeclare: TsgcAMQPExchangeDeclareEvent
      read FOnAMQPExchangeDeclare write FOnAMQPExchangeDeclare;
    property OnAMQPExchangeDelete: TsgcAMQPExchangeDeleteEvent
      read FOnAMQPExchangeDelete write FOnAMQPExchangeDelete;
    property OnAMQPBasicQoS: TsgcAMQPBasicQoSEvent read FOnAMQPBasicQoS
      write FOnAMQPBasicQoS;
    property OnAMQPBasicReturn: TsgcAMQPBasicReturnEvent read FOnAMQPBasicReturn
      write FOnAMQPBasicReturn;
    property OnAMQPQueueBind: TsgcAMQPQueueBindEvent read FOnAMQPQueueBind
      write FOnAMQPQueueBind;
    property OnAMQPQueueUnBind: TsgcAMQPQueueUnBindEvent read FOnAMQPQueueUnBind
      write FOnAMQPQueueUnBind;
    property OnAMQPBasicRecoverOk: TsgcAMQPBasicRecoverOkEvent
      read FOnAMQPBasicRecoverOk write FOnAMQPBasicRecoverOk;
    property OnAMQPQueueDeclare: TsgcAMQPQueueDeclareEvent
      read FOnAMQPQueueDeclare write FOnAMQPQueueDeclare;
    property OnAMQPQueueDelete: TsgcAMQPQueueDeleteEvent read FOnAMQPQueueDelete
      write FOnAMQPQueueDelete;
    property OnAMQPQueuePurge: TsgcAMQPQueuePurgeEvent read FOnAMQPQueuePurge
      write FOnAMQPQueuePurge;
    property OnAMQPTransactionOk: TsgcAMQPTransactionOkEvent
      read FOnAMQPTransactionOk write FOnAMQPTransactionOk;
    { events }
  end;

implementation

uses
  sgcAMQP_Helpers;

procedure TsgcAMQP_Client.AckMessage(const aChannel: string;
  aDeliveryTag: UInt64; aMultiple: Boolean);
begin
  DoWrite_BasicAck(GetChannel(aChannel), aDeliveryTag, aMultiple);
end;

procedure TsgcAMQP_Client.BindQueue(const aChannel, aQueue, aExchange,
  aRoutingKey: string; aNoWait: Boolean);
begin
  DoWrite_QueueBind(GetChannel(aChannel), aQueue, aExchange,
    aRoutingKey, aNoWait);
end;

function TsgcAMQP_Client.BindQueueEx(const aChannel, aQueue, aExchange,
  aRoutingKey: string; aNoWait: Boolean;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpQueueBindOk]);

  BindQueue(aChannel, aQueue, aExchange, aRoutingKey, aNoWait);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.CancelConsume(const aChannel, aConsumerTag: string;
  aNoWait: Boolean);
begin
  DoWrite_BasicCancel(GetChannel(aChannel), aConsumerTag, aNoWait);
end;

function TsgcAMQP_Client.CancelConsumeEx(const aChannel, aConsumerTag: string;
    aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpBasicCancelOk]);

  CancelConsume(aChannel, aConsumerTag, False);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.CommitTransaction(const aChannel: string);
begin
  DoWrite_TxCommit(GetChannel(aChannel));
end;

function TsgcAMQP_Client.CommitTransactionEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpTxCommitOk]);

  CommitTransaction(aChannel);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.Connect;
var
  vBytes: TBytes;
begin
  FConnected := True;

  SetLength(vBytes, 8);

  vBytes[0] := Ord('A');
  vBytes[1] := Ord('M');
  vBytes[2] := Ord('Q');
  vBytes[3] := Ord('P');
  vBytes[4] := 0;
  vBytes[5] := CS_AMQP_MAJOR_VERSION;
  vBytes[6] := CS_AMQP_MINOR_VERSION;
  vBytes[7] := CS_AMQP_RELEASE_VERSION;

  DoWriteBytes(vBytes);
end;

procedure TsgcAMQP_Client.Consume(const aChannel, aQueue, aConsumerTag: string;
  aNoLocal, aNoAck, aExclusive, aNoWait: Boolean);
begin
  DoWrite_BasicConsume(GetChannel(aChannel), aQueue, aConsumerTag, aNoLocal,
    aNoAck, aExclusive, aNoWait);
end;

function TsgcAMQP_Client.ConsumeEx(const aChannel, aQueue, aConsumerTag:
    string; aNoLocal, aNoAck, aExclusive: Boolean; aTimeout: Integer =
    CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpBasicConsumeOk]);

  Consume(aChannel, aQueue, aConsumerTag, aNoLocal, aNoAck, aExclusive, False);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.DeclareExchange(const aChannel, aExchange,
  aExchangeType: string; aPassive, aDurable, aAutoDelete, aInternal,
  aNoWait: Boolean);
begin
  DoWrite_ExchDeclare(GetChannel(aChannel), aExchange, aExchangeType, aPassive,
    aDurable, aNoWait, aAutoDelete, aInternal);
end;

function TsgcAMQP_Client.DeclareExchangeEx(const aChannel, aExchange,
  aExchangeType: string; aPassive, aDurable, aAutoDelete, aInternal,
  aNoWait: Boolean; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpExchDeclareOk]);

  DeclareExchange(aChannel, aExchange, aExchangeType, aPassive, aDurable,
    aNoWait, aAutoDelete, aInternal);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.DeclareQueue(const aChannel, aQueue: string;
  aPassive, aDurable, aExclusive, aAutoDelete, aNoWait: Boolean);
begin
  DoWrite_QueueDeclare(GetChannel(aChannel), aQueue, aPassive, aDurable,
    aExclusive, aAutoDelete, aNoWait);
end;

function TsgcAMQP_Client.DeclareQueueEx(const aChannel, aQueue: string;
  aPassive, aDurable, aExclusive, aAutoDelete, aNoWait: Boolean;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpQueueDeclareOk]);

  DeclareQueue(aChannel, aQueue, aPassive, aDurable, aExclusive,
    aAutoDelete, aNoWait);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.DeleteExchange(const aChannel, aExchange: string;
  aIfUnused, aNoWait: Boolean);
begin
  DoWrite_ExchDelete(GetChannel(aChannel), aExchange, aIfUnused, aNoWait);
end;

function TsgcAMQP_Client.DeleteExchangeEx(const aChannel, aExchange: string;
  aIfUnused, aNoWait: Boolean;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpExchDeleteOk]);

  DeleteExchange(aChannel, aExchange, aIfUnused, aNoWait);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.DeleteQueue(const aChannel, aQueue: string;
  aIfUnused, aIfEmpty, aNoWait: Boolean);
begin
  DoWrite_QueueDelete(GetChannel(aChannel), aQueue, aIfUnused,
    aIfEmpty, aNoWait);
end;

function TsgcAMQP_Client.DeleteQueueEx(const aChannel, aQueue: string;
  aIfUnused, aIfEmpty, aNoWait: Boolean;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpQueueDeleteOk]);

  DeleteQueue(aChannel, aQueue, aIfUnused, aIfEmpty, aNoWait);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.Disconnect;
begin
  FConnected := False;
end;

procedure TsgcAMQP_Client.DoMessageContentEvent(const aChannel
  : TsgcAMQPChannelThreadItem);
begin
  if aChannel.MessageContent.Payload.ClassType = TsgcAMQPFramePayload_Method_BasicReturn
  then
  begin
    if Assigned(FOnAMQPBasicReturn) then
      FOnAMQPBasicReturn(self, aChannel.Channel,
        TsgcAMQPFramePayload_Method_BasicReturn
        (aChannel.MessageContent.Payload), aChannel.MessageContent);
  end
  else if aChannel.MessageContent.Payload.ClassType = TsgcAMQPFramePayload_Method_BasicDeliver
  then
  begin
    if Assigned(FOnAMQPBasicDeliver) then
      FOnAMQPBasicDeliver(self, aChannel.Channel,
        TsgcAMQPFramePayload_Method_BasicDeliver
        (aChannel.MessageContent.Payload), aChannel.MessageContent);
  end
  else if aChannel.MessageContent.Payload.ClassType = TsgcAMQPFramePayload_Method_BasicGetOk
  then
  begin
    if Assigned(FOnAMQPBasicGetOk) then
      FOnAMQPBasicGetOk(self, aChannel.Channel,
        TsgcAMQPFramePayload_Method_BasicGetOk(aChannel.MessageContent.Payload),
        aChannel.MessageContent);
  end;
end;

procedure TsgcAMQP_Client.DoReadFrameBody(const aFrame: TsgcAMQPFrame;
  const aBody: TsgcAMQPFramePayloadType_ContentBody);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if not Assigned(oChannel.MessageContent.Payload) then
      DoRaiseAMQPException(CS_AMQP_ERROR_UNEXPECTED_FRAME)
    else if not Assigned(oChannel.MessageContent.Header) then
      DoRaiseAMQPException(CS_AMQP_ERROR_UNEXPECTED_FRAME);

    oChannel.MessageContent.Body := aBody;
    DoMessageContentEvent(oChannel);
  end;
end;

procedure TsgcAMQP_Client.DoReadFrameHeader(const aFrame: TsgcAMQPFrame;
  const aHeader: TsgcAMQPFramePayloadType_ContentHeader);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if not Assigned(oChannel.MessageContent.Payload) then
      DoRaiseAMQPException(CS_AMQP_ERROR_UNEXPECTED_FRAME);

    oChannel.MessageContent.Header := aHeader;
    if aHeader.BodySize = 0 then
      DoMessageContentEvent(oChannel);
  end;
end;

procedure TsgcAMQP_Client.DoReadFrameMethod(const aFrame: TsgcAMQPFrame;
  const aMethod: TsgcAMQPFramePayloadType_Method);
begin
  inherited;

  case aMethod.ClassId of
    amqpClassConnection:
      case aMethod.MethodId of
        amqpConnStart:
          DoRead_ConnStart(TsgcAMQPFramePayload_Method_ConnectionStart
            (aMethod.Payload));
        amqpConnSecure:
          DoRead_ConnSecure(TsgcAMQPFramePayload_Method_ConnectionSecure
            (aMethod.Payload));
        amqpConnTune:
          DoRead_ConnTune(TsgcAMQPFramePayload_Method_ConnectionTune
            (aMethod.Payload));
        amqpConnOpenOk:
          DoRead_ConnOpenOk(TsgcAMQPFramePayload_Method_ConnectionOpenOk
            (aMethod.Payload));
      end;
    amqpClassChannel:
      case aMethod.MethodId of
        amqpChannOpenOk:
          DoRead_ChannOpenOk(aFrame, TsgcAMQPFramePayload_Method_ChannelOpenOk
            (aMethod.Payload));
      end;
    amqpClassExchange:
      case aMethod.MethodId of
        amqpExchDeclareOk:
          DoRead_ExchDeclareOk(aFrame,
            TsgcAMQPFramePayload_Method_ExchangeDeclareOk(aMethod.Payload));
        amqpExchDeleteOk:
          DoRead_ExchDeleteOk(aFrame,
            TsgcAMQPFramePayload_Method_ExchangeDeleteOk(aMethod.Payload));
      end;
    amqpClassQueue:
      case aMethod.MethodId of
        amqpQueueDeclareOk:
          DoRead_QueueDeclareOk(aFrame,
            TsgcAMQPFramePayload_Method_QueueDeclareOk(aMethod.Payload));
        amqpQueueBindOk:
          DoRead_QueueBindOk(aFrame, TsgcAMQPFramePayload_Method_QueueBindOk
            (aMethod.Payload));
        amqpQueueUnBindOk:
          DoRead_QueueUnBindOk(aFrame, TsgcAMQPFramePayload_Method_QueueUnBindOk
            (aMethod.Payload));
        amqpQueuePurgeOk:
          DoRead_QueuePurgeOk(aFrame, TsgcAMQPFramePayload_Method_QueuePurgeOk
            (aMethod.Payload));
        amqpQueueDeleteOk:
          DoRead_QueueDeleteOk(aFrame, TsgcAMQPFramePayload_Method_QueueDeleteOk
            (aMethod.Payload));
      end;
    amqpClassBasic:
      case aMethod.MethodId of
        amqpBasicQosOk:
          DoRead_BasicQoSOk(aFrame, TsgcAMQPFramePayload_Method_BasicQoSOk
            (aMethod.Payload));
        amqpBasicConsumeOk:
          DoRead_BasicConsumeOk(aFrame,
            TsgcAMQPFramePayload_Method_BasicConsumeOk(aMethod.Payload));
        amqpBasicCancelOk:
          DoRead_BasicCancelOk(aFrame, TsgcAMQPFramePayload_Method_BasicCancelOk
            (aMethod.Payload));
        amqpBasicReturn:
          DoRead_BasicReturn(aFrame, TsgcAMQPFramePayload_Method_BasicReturn
            (aMethod.Payload));
        amqpBasicDeliver:
          DoRead_BasicDeliver(aFrame, TsgcAMQPFramePayload_Method_BasicDeliver
            (aMethod.Payload));
        amqpBasicGetOk:
          DoRead_BasicGetOk(aFrame, TsgcAMQPFramePayload_Method_BasicGetOk
            (aMethod.Payload));
        amqpBasicGetEmpty:
          DoRead_BasicGetEmpty(aFrame, TsgcAMQPFramePayload_Method_BasicGetEmpty
            (aMethod.Payload));
        amqpBasicRecoverOk:
          DoRead_BasicRecoverOk(aFrame,
            TsgcAMQPFramePayload_Method_BasicRecoverOk(aMethod.Payload));
      end;
    amqpClassTx:
      case aMethod.MethodId of
        amqpTxSelectOk:
          DoRead_TxSelectOk(aFrame, TsgcAMQPFramePayload_Method_TxSelectOk
            (aMethod.Payload));
        amqpTxCommitOk:
          DoRead_TxCommitOk(aFrame, TsgcAMQPFramePayload_Method_TxCommitOk
            (aMethod.Payload));
        amqpTxRollbackOk:
          DoRead_TxRollbackOk(aFrame, TsgcAMQPFramePayload_Method_TxRollbackOk
            (aMethod.Payload));
      end;
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicCancelOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicCancelOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPBasicCancelConsume) then
      FOnAMQPBasicCancelConsume(self, oChannel.Channel, aPayload.ConsumerTag);
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicConsumeOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicConsumeOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPBasicConsume) then
      FOnAMQPBasicConsume(self, oChannel.Channel, aPayload.ConsumerTag);
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicDeliver(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicDeliver);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    oChannel.MessageContent.Clear;
    oChannel.MessageContent.Payload := aPayload;
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicGetEmpty(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicGetEmpty);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPBasicGetEmpty) then
      FOnAMQPBasicGetEmpty(self, oChannel.Channel);
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicGetOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicGetOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    oChannel.MessageContent.Clear;
    oChannel.MessageContent.Payload := aPayload;
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicQoSOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicQoSOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPBasicQoS) then
      FOnAMQPBasicQoS(self, oChannel.Channel, oChannel.QoS);
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicRecoverOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicRecoverOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPBasicRecoverOk) then
      FOnAMQPBasicRecoverOk(self, oChannel.Channel);
  end;
end;

procedure TsgcAMQP_Client.DoRead_BasicReturn(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_BasicReturn);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    oChannel.MessageContent.Clear;
    oChannel.MessageContent.Payload := aPayload;
  end;
end;

procedure TsgcAMQP_Client.DoRead_ChannOpenOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_ChannelOpenOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    oChannel.MethodId := amqpChannOpenOk;

    if Assigned(FOnAMQPChannelOpen) then
      FOnAMQPChannelOpen(self, oChannel.Channel);
  end;
end;

procedure TsgcAMQP_Client.DoRead_ConnOpenOk(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionOpenOk);
begin
  if Assigned(FOnAMQPConnect) then
    FOnAMQPConnect(self);
end;

procedure TsgcAMQP_Client.DoRead_ConnSecure(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionSecure);
begin
  DoWrite_ConnSecureOk(aPayload);
end;

procedure TsgcAMQP_Client.DoRead_ConnStart(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionStart);
begin
  if aPayload.VersionMajor > CS_AMQP_MAJOR_VERSION then
    DoRaiseAMQPException(CS_AMQP_ERROR_CLOSE_CONNECTION)
  else if aPayload.VersionMinor > CS_AMQP_MINOR_VERSION then
    DoRaiseAMQPException(CS_AMQP_ERROR_CLOSE_CONNECTION);

  DoWrite_ConnStartOk(aPayload);
end;

procedure TsgcAMQP_Client.DoRead_ConnTune(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionTune);
begin
  DoWrite_ConnTuneOk(aPayload);
  DoWrite_ConnOpen;
end;

procedure TsgcAMQP_Client.DoRead_ExchDeclareOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_ExchangeDeclareOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPExchangeDeclare) then
      FOnAMQPExchangeDeclare(self, oChannel.Channel,
        oChannel.Request.ExchangeDeclare);
  end;
end;

procedure TsgcAMQP_Client.DoRead_ExchDeleteOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_ExchangeDeleteOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPExchangeDelete) then
      FOnAMQPExchangeDelete(self, oChannel.Channel,
        oChannel.Request.ExchangeDelete);
  end;
end;

procedure TsgcAMQP_Client.DoRead_QueueBindOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_QueueBindOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPQueueBind) then
      FOnAMQPQueueBind(self, oChannel.Channel, oChannel.Request.QueueBindQueue,
        oChannel.Request.QueueBindExchange);
  end;
end;

procedure TsgcAMQP_Client.DoRead_QueueDeclareOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_QueueDeclareOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPQueueDeclare) then
      FOnAMQPQueueDeclare(self, oChannel.Channel, aPayload.Queue,
        aPayload.MessageCount, aPayload.ConsumerCount);
  end;
end;

procedure TsgcAMQP_Client.DoRead_QueueDeleteOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_QueueDeleteOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPQueueDelete) then
      FOnAMQPQueueDelete(self, oChannel.Channel, oChannel.Request.QueueDelete,
        aPayload.MessageCount);
  end;
end;

procedure TsgcAMQP_Client.DoRead_QueuePurgeOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_QueuePurgeOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPQueuePurge) then
      FOnAMQPQueuePurge(self, oChannel.Channel, oChannel.Request.QueuePurge,
        aPayload.MessageCount);
  end;
end;

procedure TsgcAMQP_Client.DoRead_QueueUnBindOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_QueueUnBindOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPQueueUnBind) then
      FOnAMQPQueueUnBind(self, oChannel.Channel,
        oChannel.Request.QueueUnBindQueue,
        oChannel.Request.QueueUnBindExchange);
  end;
end;

procedure TsgcAMQP_Client.DoRead_TxCommitOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_TxCommitOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPTransactionOk) then
      FOnAMQPTransactionOk(self, oChannel.Channel, amqpTransactionCommit);
  end;
end;

procedure TsgcAMQP_Client.DoRead_TxRollbackOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_TxRollbackOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPTransactionOk) then
      FOnAMQPTransactionOk(self, oChannel.Channel, amqpTransactionRollback);
  end;
end;

procedure TsgcAMQP_Client.DoRead_TxSelectOk(const aFrame: TsgcAMQPFrame;
  const aPayload: TsgcAMQPFramePayload_Method_TxSelectOk);
var
  oChannel: TsgcAMQPChannelThreadItem;
begin
  oChannel := Channels.GetChannel(aFrame.Header.Channel);
  if Assigned(oChannel) then
  begin
    if Assigned(FOnAMQPTransactionOk) then
      FOnAMQPTransactionOk(self, oChannel.Channel, amqpTransactionSelect);
  end;
end;

procedure TsgcAMQP_Client.DoWriteContentBody(const aChannel
  : TsgcAMQPChannelThreadItem; const aData: TStream; var aBytes: TBytes);
var
  oFrame: TsgcAMQPFrame;
  vOffset: Int64;
  vSize: Int64;
begin
  vOffset := 0;
  aData.Position := 0;
  while vOffset < aData.Size do
  begin
    vSize := AMQPNegotiatedValues.MaxFrameSize - CS_AMQP_FRAME_HEADER_SIZE;
    if vSize > aData.Size - aData.Position then
      vSize := aData.Size - aData.Position;
    aData.Position := vOffset;
    oFrame := TsgcAMQPFrame.Create;
    Try
      oFrame.Header.Channel := StrToInt(aChannel.ID);
      oFrame.Header._Type := amqpFrameBody;
      oFrame.Header.Size := vSize;
      oFrame.Payload_ContentBody.Data := aData;
      oFrame.Payload_ContentBody.DataSize := vSize;

      sgcWriteBytes(oFrame.Write, aBytes);
    Finally
      sgcFree(oFrame);
    End;
    vOffset := vOffset + vSize;
  end;
end;

procedure TsgcAMQP_Client.DoWriteContentHeader(const aChannel
  : TsgcAMQPChannelThreadItem; aClassId: TsgcAMQPClass;
  const aHeader: TsgcAMQPBasicProperties; const aBody: TStream;
  var aBytes: TBytes);
var
  oFrame: TsgcAMQPFrame;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameHeader;
    oFrame.Payload_ContentHeader.ClassId := aClassId;
    oFrame.Payload_ContentHeader.Header := aHeader;
    oFrame.Payload_ContentHeader.BodySize := aBody.Size;

    sgcWriteBytes(oFrame.Write, aBytes);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicAck(const aChannel
  : TsgcAMQPChannelThreadItem; const aDeliveryTag: UInt64; aMultiple: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicAck;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicAck.Create;
    oPayload.DeliveryTag := aDeliveryTag;
    oPayload.Multiple := aMultiple;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicCancel(const aChannel
  : TsgcAMQPChannelThreadItem; const aConsumerTag: string; aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicCancel;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicCancel.Create;
    oPayload.ConsumerTag := aConsumerTag;
    oPayload.NoWait := aNoWait;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicConsume(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue, aConsumerTag: string;
  aNoLocal, aNoAck, aExclusive, aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicConsume;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicConsume.Create;
    oPayload.Queue := aQueue;
    oPayload.ConsumerTag := aConsumerTag;
    oPayload.NoLocal := aNoLocal;
    oPayload.NoAck := aNoAck;
    oPayload.Exclusive := aExclusive;
    oPayload.NoWait := aNoWait;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicGet(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue: string; aNoAck: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicGet;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicGet.Create;
    oPayload.Queue := aQueue;
    oPayload.NoAck := aNoAck;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicPublish(const aChannel
  : TsgcAMQPChannelThreadItem; const aExchange, aRoutingKey: string;
  const aHeader: TsgcAMQPBasicProperties; const aBody: TStream;
  aMandatory, aImmediate: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicPublish;
  vBytes: TBytes;
begin
  // ... basic publish
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicPublish.Create;
    oPayload.Exchange := aExchange;
    oPayload.RoutingKey := aRoutingKey;
    oPayload.Mandatory := aMandatory;
    oPayload.Immediate := aImmediate;
    oFrame.Payload_Method.Payload := oPayload;

    sgcWriteBytes(oFrame.Write, vBytes);
  Finally
    sgcFree(oFrame);
  End;

  // ... content header
  DoWriteContentHeader(aChannel, amqpClassBasic, aHeader, aBody, vBytes);

  // ... content body
  DoWriteContentBody(aChannel, aBody, vBytes);

  // ... send message
  DoWriteBytes(vBytes);
end;

procedure TsgcAMQP_Client.DoWrite_BasicQoS(const aChannel
  : TsgcAMQPChannelThreadItem; aPrefetchSize: Cardinal; aPrefetchCount: Word;
  aGlobal: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicQoS;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicQoS.Create;
    oPayload.PrefetchSize := aPrefetchSize;
    oPayload.PrefetchCount := aPrefetchCount;
    oPayload.Global := aGlobal;
    oFrame.Payload_Method.Payload := oPayload;

    aChannel.QoS := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicRecover(const aChannel
  : TsgcAMQPChannelThreadItem; aRequeue: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicRecover;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicRecover.Create;
    oPayload.Requeue := aRequeue;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicRecoverAsync(const aChannel
  : TsgcAMQPChannelThreadItem; aRequeue: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicRecoverAsync;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicRecoverAsync.Create;
    oPayload.Requeue := aRequeue;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_BasicReject(const aChannel
  : TsgcAMQPChannelThreadItem; const aDeliveryTag: UInt64; aRequeue: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_BasicReject;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_BasicReject.Create;
    oPayload.DeliveryTag := aDeliveryTag;
    oPayload.Requeue := aRequeue;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ChannOpen(aChannelId: Word);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ChannelOpen;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := aChannelId;
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ChannelOpen.Create;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ConnOpen;
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ConnectionOpen;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ConnectionOpen.Create;
    oPayload.VirtualHost := AMQPOptions.VirtualHost;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ConnSecureOk(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionSecure);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ConnectionSecureOk;
  vChallenge: string;
begin
  vChallenge := '';
  if Assigned(FOnAMQPChallenge) then
    FOnAMQPChallenge(self, aPayload.Challenge, vChallenge);

  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ConnectionSecureOk.Create;
    oPayload.Challenge := vChallenge;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ConnStartOk(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionStart);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ConnectionStartOk;
  vMechanisms: TsgcAMQPAuthentications;
  vMechanism: TsgcAMQPAuthentication;
  vUser, vPassword: string;
  oLocales: TStringList;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ConnectionStartOk.Create;

    vMechanism := amqpAuthAnnonymous;
    if aPayload.Mechanisms <> '' then
    begin
      vMechanisms := sgcGetAMQPAuthentications(aPayload.Mechanisms);
      if amqpAuthAMQPlain in vMechanisms then
        vMechanism := amqpAuthAMQPlain
      else if amqpAuthPlain in vMechanisms then
        vMechanism := amqpAuthPlain;
      if vMechanism <> amqpAuthAnnonymous then
      begin
        if Assigned(FOnAMQPAuthentication) then
          FOnAMQPAuthentication(self, vMechanisms, vMechanism, vUser, vPassword)
      end;
    end;

    oLocales := TStringList.Create;
    Try
      oLocales.Delimiter := ' ';
      oLocales.DelimitedText := aPayload.Locales;
      if oLocales.IndexOf(AMQPOptions.Locale) > -1 then
        AMQPNegotiatedValues.Locale := AMQPOptions.Locale
      else
        AMQPNegotiatedValues.Locale := CS_AMQP_DEFAULT_LOCALE;
      oPayload.Locale := AMQPNegotiatedValues.Locale;
    Finally
      sgcFree(oLocales);
    End;
    oPayload.Mechanism := sgcGetAMQPAuthenticationValue(vMechanism);
    oPayload.User := vUser;
    oPayload.Password := vPassword;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ConnTuneOk(const aPayload
  : TsgcAMQPFramePayload_Method_ConnectionTune);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ConnectionTuneOk;
begin
  // set tune values
  if HeartBeat.Enabled then
  begin
    if HeartBeat.Interval < aPayload.HeartBeat then
      AMQPNegotiatedValues.HeartBeat := HeartBeat.Interval
    else
      AMQPNegotiatedValues.HeartBeat := aPayload.HeartBeat;
  end
  else
    AMQPNegotiatedValues.HeartBeat := 0;
  if AMQPOptions.MaxChannels < aPayload.ChannelMax then
    AMQPNegotiatedValues.MaxChannels := AMQPOptions.MaxChannels
  else
    AMQPNegotiatedValues.MaxChannels := aPayload.ChannelMax;
  if AMQPOptions.MaxFrameSize < aPayload.FrameMax then
    AMQPNegotiatedValues.MaxFrameSize := AMQPOptions.MaxFrameSize
  else
    AMQPNegotiatedValues.MaxFrameSize := aPayload.FrameMax;

  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ConnectionTuneOk.Create;
    oPayload.ChannelMax := AMQPNegotiatedValues.MaxChannels;
    oPayload.FrameMax := AMQPNegotiatedValues.MaxFrameSize;
    oPayload.HeartBeat := AMQPNegotiatedValues.HeartBeat;

    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ExchDeclare(const aChannel
  : TsgcAMQPChannelThreadItem; const aExchange, aExchangeType: string;
  aPassive, aDurable, aAutoDelete, aInternal, aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ExchangeDeclare;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ExchangeDeclare.Create;
    oPayload.Exchange := aExchange;
    oPayload.ExchangeType := aExchangeType;
    oPayload.Passive := aPassive;
    oPayload.Durable := aDurable;
    oPayload.NoWait := aNoWait;
    oPayload.AutoDelete := aAutoDelete;
    oPayload.Internal := aInternal;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);

    if not aNoWait then
      aChannel.Request.ExchangeDeclare := aExchange;
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_ExchDelete(const aChannel
  : TsgcAMQPChannelThreadItem; const aExchange: string;
  aIfUnused, aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_ExchangeDelete;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_ExchangeDelete.Create;
    oPayload.Exchange := aExchange;
    oPayload.IfUnused := aIfUnused;
    oPayload.NoWait := aNoWait;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);

    if not aNoWait then
      aChannel.Request.ExchangeDelete := aExchange;
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_QueueBind(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue, aExchange, aRoutingKey: string;
  aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_QueueBind;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_QueueBind.Create;
    oPayload.Queue := aQueue;
    oPayload.Exchange := aExchange;
    oPayload.RoutingKey := aRoutingKey;
    oPayload.NoWait := aNoWait;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);

    if not aNoWait then
    begin
      aChannel.Request.QueueBindQueue := aQueue;
      aChannel.Request.QueueBindExchange := aExchange;
    end;
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_QueueDeclare(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue: string;
  aPassive, aDurable, aExclusive, aAutoDelete, aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_QueueDeclare;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_QueueDeclare.Create;
    oPayload.Queue := aQueue;
    oPayload.Passive := aPassive;
    oPayload.Durable := aDurable;
    oPayload.Exclusive := aExclusive;
    oPayload.NoWait := aNoWait;
    oPayload.AutoDelete := aAutoDelete;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_QueueDelete(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue: string;
  aIfUnused, aIfEmpty, aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_QueueDelete;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_QueueDelete.Create;
    oPayload.Queue := aQueue;
    oPayload.IfUnused := aIfUnused;
    oPayload.IfEmpty := aIfEmpty;
    oPayload.NoWait := aNoWait;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);

    if not aNoWait then
      aChannel.Request.QueueDelete := aQueue;
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_QueuePurge(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue: string; aNoWait: Boolean);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_QueuePurge;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_QueuePurge.Create;
    oPayload.Queue := aQueue;
    oPayload.NoWait := aNoWait;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);

    if not aNoWait then
      aChannel.Request.QueuePurge := aQueue;
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_QueueUnBind(const aChannel
  : TsgcAMQPChannelThreadItem; const aQueue, aExchange, aRoutingKey: string);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_QueueUnBind;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_QueueUnBind.Create;
    oPayload.Queue := aQueue;
    oPayload.Exchange := aExchange;
    oPayload.RoutingKey := aRoutingKey;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_TxCommit(const aChannel
  : TsgcAMQPChannelThreadItem);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_TxCommit;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_TxCommit.Create;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_TxRollback(const aChannel
  : TsgcAMQPChannelThreadItem);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_TxRollback;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_TxRollback.Create;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.DoWrite_TxSelect(const aChannel
  : TsgcAMQPChannelThreadItem);
var
  oFrame: TsgcAMQPFrame;
  oPayload: TsgcAMQPFramePayload_Method_TxSelect;
begin
  oFrame := TsgcAMQPFrame.Create;
  Try
    oFrame.Header.Channel := StrToInt(aChannel.ID);
    oFrame.Header._Type := amqpFrameMethod;
    oPayload := TsgcAMQPFramePayload_Method_TxSelect.Create;
    oFrame.Payload_Method.Payload := oPayload;

    DoWriteBytes(oFrame.Write);
  Finally
    sgcFree(oFrame);
  End;
end;

procedure TsgcAMQP_Client.GetMessage(const aChannel, aQueue: string;
  aNoAck: Boolean);
begin
  DoWrite_BasicGet(GetChannel(aChannel), aQueue, aNoAck);
end;

function TsgcAMQP_Client.GetMessageEx(const aChannel, aQueue: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpBasicGetOk, amqpBasicGetEmpty]);

  GetMessage(aChannel, aQueue, False);

  result := DoWaitAMQPMethod(aChannel, aTimeout, [amqpBasicGetEmpty]);
end;

procedure TsgcAMQP_Client.OpenChannel(const aChannel: string);
var
  vChannelId: Word;
begin
  vChannelId := Channels.AddChannel(aChannel);
  if vChannelId > 0 then
    DoWrite_ChannOpen(vChannelId)
  else
    raise Exception.CreateFmt(S_AMQP_ERROR_CHANNEL_ALREADY_EXISTS, [aChannel]);
end;

function TsgcAMQP_Client.OpenChannelEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
//  DoInitWaitAMQPMethod(aChannel, [amqpChannOpenOk]); // assigned internally

  OpenChannel(aChannel);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.PublishMessage(const aChannel, aExchange,
    aRoutingKey: string; const aHeader: TsgcAMQPBasicProperties; const
    aMessage: TStream; aMandatory, aImmediate: Boolean);
begin
  if Assigned(aMessage) then
    aMessage.Position := 0;

  DoWrite_BasicPublish(GetChannel(aChannel), aExchange, aRoutingKey, aHeader,
    aMessage, aMandatory, aImmediate);
end;

procedure TsgcAMQP_Client.PublishMessage(const aChannel, aExchange,
  aRoutingKey: string; const aHeader: TsgcAMQPBasicProperties;
  const aMessage: string; aMandatory: Boolean = False;
  aImmediate: Boolean = False);
var
  oStream: TStringStream;
begin
  oStream := TStringStream.Create(aMessage{$IFDEF D2009},
    TEncoding.UTF8{$ENDIF});
  Try
    PublishMessage(aChannel, aExchange, aRoutingKey, aHeader, oStream,
      aMandatory, aImmediate);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcAMQP_Client.PublishMessage(const aChannel, aExchange, aRoutingKey,
  aMessage: string; aMandatory: Boolean = False; aImmediate: Boolean = False);
var
  oStream: TStringStream;
begin
  oStream := TStringStream.Create(aMessage{$IFDEF D2009},
    TEncoding.UTF8{$ENDIF});
  Try
    PublishMessage(aChannel, aExchange, aRoutingKey, nil, oStream, aMandatory,
      aImmediate);
  Finally
    sgcFree(oStream);
  End;
end;

procedure TsgcAMQP_Client.PublishMessage(const aChannel, aExchange,
    aRoutingKey: string; const aMessage: TStream; aMandatory: Boolean = false;
    aImmediate: Boolean = false);
begin
  PublishMessage(aChannel, aExchange, aRoutingKey, nil, aMessage, aMandatory,
    aImmediate);
end;

procedure TsgcAMQP_Client.PurgeQueue(const aChannel, aQueue: string;
  aNoWait: Boolean);
begin
  DoWrite_QueuePurge(GetChannel(aChannel), aQueue, aNoWait);
end;

function TsgcAMQP_Client.PurgeQueueEx(const aChannel, aQueue: string;
  aNoWait: Boolean; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpQueuePurgeOk]);

  PurgeQueue(aChannel, aQueue, aNoWait);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.Recover(const aChannel: string; aRequeue: Boolean);
begin
  DoWrite_BasicRecover(GetChannel(aChannel), aRequeue);
end;

procedure TsgcAMQP_Client.RecoverAsync(const aChannel: string;
  aRequeue: Boolean);
begin
  DoWrite_BasicRecoverAsync(GetChannel(aChannel), aRequeue);
end;

function TsgcAMQP_Client.RecoverEx(const aChannel: string; aRequeue: Boolean;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpBasicRecoverOk]);

  Recover(aChannel, aRequeue);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.RejectMessage(const aChannel: string;
  aDeliveryTag: UInt64; aRequeue: Boolean);
begin
  DoWrite_BasicReject(GetChannel(aChannel), aDeliveryTag, aRequeue);
end;

procedure TsgcAMQP_Client.RollbackTransaction(const aChannel: string);
begin
  DoWrite_TxRollback(GetChannel(aChannel));
end;

function TsgcAMQP_Client.RollbackTransactionEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpTxRollbackOk]);

  RollbackTransaction(aChannel);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.SelectTransaction(const aChannel: string);
begin
  DoWrite_TxSelect(GetChannel(aChannel));
end;

function TsgcAMQP_Client.SelectTransactionEx(const aChannel: string;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpTxSelectOk]);

  SelectTransaction(aChannel);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.SetQoS(const aChannel: string;
  aPrefetchSize: Cardinal; aPrefetchCount: Word; aGlobal: Boolean);
begin
  DoWrite_BasicQoS(GetChannel(aChannel), aPrefetchSize, aPrefetchCount,
    aGlobal);
end;

function TsgcAMQP_Client.SetQoSEx(const aChannel: string;
  aPrefetchSize: Cardinal; aPrefetchCount: Word; aGlobal: Boolean;
  aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpBasicQosOk]);

  SetQoS(aChannel, aPrefetchSize, aPrefetchCount, aGlobal);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

procedure TsgcAMQP_Client.UnBindQueue(const aChannel, aQueue, aExchange,
  aRoutingKey: string);
begin
  DoWrite_QueueUnBind(GetChannel(aChannel), aQueue, aExchange, aRoutingKey);
end;

function TsgcAMQP_Client.UnBindQueueEx(const aChannel, aQueue, aExchange,
  aRoutingKey: string; aTimeout: Integer = CS_AMQP_DEFAULT_TIMEOUT): Boolean;
begin
  DoInitWaitAMQPMethod(aChannel, [amqpQueueUnBindOk]);

  UnBindQueue(aChannel, aQueue, aExchange, aRoutingKey);

  result := DoWaitAMQPMethod(aChannel, aTimeout);
end;

end.
