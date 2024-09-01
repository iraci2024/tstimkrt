{ ***************************************************************************
  sgcHTTP Amazon component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_Amazon_SQS;

interface

{$I sgcVer.inc}
{$IFDEF SGC_AWS}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_Amazon_AWS, sgcWebSocket_Classes_Queues;

type

  TsgcOnAWSSQSResponse = procedure(Sender: TObject; const RawResponse: String;
    var Handled: Boolean) of object;
  TsgcOnAWSSQSBeforeRequest = procedure(Sender: TObject; var URL: String;
    var Handled: Boolean) of object;
  TsgcOnAWSSQSError = procedure(Sender: TObject;
    const Error_Code, Error_Description, RawError: String) of object;

  TsgcAWSSQSAttribute = (sqsatAll, sqsatPolicy, sqsatVisibilityTimeout,
    sqsatMaximumMessageSize, sqsatMessageRetentionPeriod,
    sqsatApproximateNumberOfMessages,
    sqsatApproximateNumberOfMessagesNotVisible, sqsatCreatedTimestamp,
    sqsatLastModifiedTimestamp, sqsatQueueArn,
    sqsatApproximateNumberOfMessagesDelayed, sqsatDelaySeconds,
    sqsatReceiveMessageWaitTimeSeconds, sqsatRedrivePolicy, sqsatFifoQueue,
    sqsatContentBasedDeduplication, sqsatKmsMasterKeyId,
    sqsatKmsDataKeyReusePeriodSeconds);
  TsgcAWSSQSAttributes = array of TsgcAWSSQSAttribute;

  TsgcAWSSQSActionName = (sqsanAddPermission, sqsanChangeMessageVisibility,
    sqsanDeleteMessage, sqsanDeleteQueue, sqsanGetQueueAttributes,
    sqsanGetQueueUrl, sqsanListDeadLetterSourceQueues, sqsanPurgeQueue,
    sqsanReceiveMessage, sqsanRemovePermission, sqsanSendMessage,
    sqsanSetQueueAttributes);
  TsgcAWSSQSActionNames = array of TsgcAWSSQSActionName;

  TsgcSQSAttribute = class(TsgcQueueItemBase)
  private
    FAttributeValue: String;
    function GetAttributeName: String;
    procedure SetAttributeName(const Value: String);
  public
    property AttributeName: String read GetAttributeName write SetAttributeName;
    property AttributeValue: String read FAttributeValue write FAttributeValue;
  end;

  TsgcSQSAttributes = class(TsgcQueue)
  private
  public
    procedure AddSQSAttribute(const aName, aValue: String); overload;
    procedure AddSQSAttribute(const aAttribute: TsgcAWSSQSAttribute;
      aValue: String); overload;
  end;

  TsgcSQSMessageAttributeItem = class(TsgcQueueItemBase)
  private
    FAttributeType: String;
    FAttributeValue: String;
    function GetAttributeName: String;
    procedure SetAttributeName(const Value: String);
  public
    property AttributeName: String read GetAttributeName write SetAttributeName;
    property AttributeType: String read FAttributeType write FAttributeType;
    property AttributeValue: String read FAttributeValue write FAttributeValue;
  end;

  TsgcSQSMessageAttributes = class(TsgcQueue)
  public
    procedure AddSQSAttribute(const aName, aType, aValue: String);
  end;

  TsgcSQSSendMessageRequest = class(TsgcQueueItemBase)
  private
    FDelaySeconds: Integer;
    FMessageBody: String;
    FMessageDeduplicationId: String;
    FMessageGroupId: String;
    FMessageAttributes: TsgcSQSMessageAttributes;
    FMessageSystemAttributes: TsgcSQSMessageAttributes;
    function GetMessageAttributes: TsgcSQSMessageAttributes;
    function GetMessageSystemAttributes: TsgcSQSMessageAttributes;
    procedure SetMessageAttributes(const Value: TsgcSQSMessageAttributes);
    procedure SetMessageSystemAttributes(const Value: TsgcSQSMessageAttributes);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property DelaySeconds: Integer read FDelaySeconds write FDelaySeconds;
    property MessageAttributes: TsgcSQSMessageAttributes
      read GetMessageAttributes write SetMessageAttributes;
    property MessageSystemAttributes: TsgcSQSMessageAttributes
      read GetMessageSystemAttributes write SetMessageSystemAttributes;
    property MessageBody: String read FMessageBody write FMessageBody;
    property MessageDeduplicationId: String read FMessageDeduplicationId
      write FMessageDeduplicationId;
    property MessageGroupId: String read FMessageGroupId write FMessageGroupId;
  end;

  TsgcSQSSendMessageRequests = class(TsgcQueue)
  end;

  TsgcSQSSendMessageResponse = class(TsgcQueueItemBase)
  private
    FMD5OfMessageAttributes: String;
    FMD5OfMessageBody: String;
    FMD5OfMessageSystemAttributes: String;
    FMessageId: String;
    FSequenceNumber: String;
  public
    property MD5OfMessageAttributes: String read FMD5OfMessageAttributes
      write FMD5OfMessageAttributes;
    property MD5OfMessageBody: String read FMD5OfMessageBody
      write FMD5OfMessageBody;
    property MD5OfMessageSystemAttributes: String
      read FMD5OfMessageSystemAttributes write FMD5OfMessageSystemAttributes;
    property MessageId: String read FMessageId write FMessageId;
    property SequenceNumber: String read FSequenceNumber write FSequenceNumber;
  end;

  TsgcSQSSendMessageResponses = class(TsgcQueue)
  end;

  TsgcSQSSendMessage = class(TsgcQueueItemBase)
  private
    FRequest: TsgcSQSSendMessageRequest;
    FResponse: TsgcSQSSendMessageResponse;
    function GetRequest: TsgcSQSSendMessageRequest;
    function GetResponse: TsgcSQSSendMessageResponse;
  public
    destructor Destroy; override;
  public
    property Request: TsgcSQSSendMessageRequest read GetRequest write FRequest;
    property Response: TsgcSQSSendMessageResponse read GetResponse
      write FResponse;
  end;

  TsgcSQSReceiveMessageRequest = class(TsgcQueueItemBase)
  private
    FAttributes: TsgcAWSSQSAttributes;
    FMaxNumberOfMessages: Integer;
    FMessageAttributeName: TStringList;
    FReceiveRequestAttemptId: String;
    FVisibilityTimeout: Integer;
    FWaitTimeSeconds: Integer;
    function GetMessageAttributeName: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Attributes: TsgcAWSSQSAttributes read FAttributes
      write FAttributes;
    property MaxNumberOfMessages: Integer read FMaxNumberOfMessages
      write FMaxNumberOfMessages;
    property MessageAttributeName: TStringList read GetMessageAttributeName
      write FMessageAttributeName;
    property ReceiveRequestAttemptId: String read FReceiveRequestAttemptId
      write FReceiveRequestAttemptId;
    property VisibilityTimeout: Integer read FVisibilityTimeout
      write FVisibilityTimeout;
    property WaitTimeSeconds: Integer read FWaitTimeSeconds
      write FWaitTimeSeconds;
  end;

  TsgcSQSReceiveMessageResponse = class(TsgcQueueItemBase)
  private
    FAttributes: TsgcSQSAttributes;
    FBody: String;
    FMD5OfBody: String;
    FMD5OfMessageAttributes: String;
    FMessageAttributes: TsgcSQSMessageAttributes;
    FReceiptHandle: string;
    function GetAttributes: TsgcSQSAttributes;
    function GetMessageAttributes: TsgcSQSMessageAttributes;
    function GetMessageId: String;
    procedure SetMessageId(const Value: String);
    procedure SetReceiptHandle(const Value: string);
  public
    destructor Destroy; override;
  public
    property Attributes: TsgcSQSAttributes read GetAttributes write FAttributes;
    property Body: String read FBody write FBody;
    property MD5OfBody: String read FMD5OfBody write FMD5OfBody;
    property MD5OfMessageAttributes: String read FMD5OfMessageAttributes
      write FMD5OfMessageAttributes;
    property MessageAttributes: TsgcSQSMessageAttributes
      read GetMessageAttributes write FMessageAttributes;
    property MessageId: String read GetMessageId write SetMessageId;
    property ReceiptHandle: string read FReceiptHandle write SetReceiptHandle;
  end;

  TsgcSQSReceiveMessageResponses = class(TsgcQueue)
  end;

  TsgcSQSReceiveMessage = class(TsgcQueueItemBase)
  private
    FRequest: TsgcSQSReceiveMessageRequest;
    FResponse: TsgcSQSReceiveMessageResponse;
    function GetRequest: TsgcSQSReceiveMessageRequest;
    function GetResponse: TsgcSQSReceiveMessageResponse;
  public
    destructor Destroy; override;
  public
    property Request: TsgcSQSReceiveMessageRequest read GetRequest
      write FRequest;
    property Response: TsgcSQSReceiveMessageResponse read GetResponse
      write FResponse;
  end;

  TsgcSQSDeleteMessageBatchRequestItem = class(TsgcQueueItemBase)
  private
    FReceiptHandle: String;
  public
    property ReceiptHandle: String read FReceiptHandle write FReceiptHandle;
  end;

  TsgcSQSDeleteMessageBatchRequestItems = class(TsgcQueue)
  public
    procedure AddSQSDeleteMessage(const aId, aReceiptHandle: String);
  end;

  TsgcSQSChangeMessageVisibilityBatchRequest = class(TsgcQueueItemBase)
  private
    FReceiptHandle: String;
    FVisibilityTimeout: Integer;
  public
    property ReceiptHandle: String read FReceiptHandle write FReceiptHandle;
    property VisibilityTimeout: Integer read FVisibilityTimeout
      write FVisibilityTimeout;
  end;

  TsgcSQSChangeMessageVisibilityBatchRequests = class(TsgcQueue)
  public
    procedure AddSQSChangeVisibility(const aId, aReceiptHandle: String;
      aVisibilityTimeout: Integer);
  end;

  TsgcSQSTag = class(TsgcQueueItemBase)
  private
    FValue: String;
    function GetKey: String;
    procedure SetKey(const Value: String);
  public
    property Key: String read GetKey write SetKey;
    property Value: String read FValue write FValue;
  end;

  TsgcSQSTags = class(TsgcQueue)
  public
    procedure AddSQSTag(const Key, Value: String);
  end;

  TsgcSQSPermission = class(TsgcQueueItemBase)
  private
    FActionName: string;
    FAWSAccountId: String;
  public
    property ActionName: string read FActionName write FActionName;
    property AWSAccountId: String read FAWSAccountId write FAWSAccountId;
  end;

  TsgcSQSPermissions = class(TsgcQueue)
  public
    procedure AddSQSPermission(const ActionName: String;
      const AWSAccountId: String); overload;
    procedure AddSQSPermission(const ActionName: TsgcAWSSQSActionName;
      const AWSAccountId: String); overload;
  end;

  TsgcSQSQueueItem = class(TsgcQueueItemBase)
  private
    FURL: String;
  public
    property URL: String read FURL write FURL;
  end;

  TsgcSQSQueue = class(TsgcQueue)
  public
    procedure AddSQSQueue(const aQueue, aURL: String);
    function GetSQSQueue(const aQueue: String): TsgcSQSQueueItem;
    function GetURLFromQueue(const aQueueName: String): String;
  end;

  TsgcSQSCreateQueue = class(TsgcQueueItemBase)
  private
    FAttributes: TsgcSQSAttributes;
    FTags: TsgcSQSTags;
    function GetAttributes: TsgcSQSAttributes;
    function GetQueueName: String;
    function GetTags: TsgcSQSTags;
    procedure SetQueueName(const Value: String);
  public
    destructor Destroy; override;
  public
    property Attributes: TsgcSQSAttributes read GetAttributes write FAttributes;
    property QueueName: String read GetQueueName write SetQueueName;
    property Tags: TsgcSQSTags read GetTags write FTags;
  end;

  TsgcHTTP_Amazon_AWS_SQS_Client = class(TsgcHTTP_Amazon_AWS_Client)
    { from TsgcHTTP_Amazon_AWS_Client }
  protected
    function DoBeforeGET_Request(var aURL: String): Boolean; override;
    { from TsgcHTTP_Amazon_AWS_Client }

    { helpers }
  private
    FParams: TStringList;
    procedure AddParam(const aParam, aValue: String); overload;
    procedure AddParam(const aParam: String; aValue: Integer); overload;
    procedure ClearParams(const aAction: String);
    function GetParams: TStringList;
  private
    function GetFullURL(const aEndPoint: String = ''): String;
  protected
    property Params: TStringList read GetParams;
    { helpers }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { queues }
  private
    FQueues: TsgcSQSQueue;
    function GetQueues: TsgcSQSQueue;
  protected
    property Queues: TsgcSQSQueue read GetQueues write FQueues;
    { queues }

    { methods }
  private
    procedure DoProcessError(const aError: String);
  protected
    function DoSendMessage(const aURL: String;
      const aRequest: TsgcSQSSendMessageRequest;
      var aResponse: TsgcSQSSendMessageResponse): Boolean; virtual;
    function DoSendMessageBatch(const aURL: String;
      const aRequests: TsgcSQSSendMessageRequests;
      var aResponses: TsgcSQSSendMessageResponses): Boolean; virtual;
  protected
    function DoDeleteMessage(const aURL, aReceiptHandle: String)
      : Boolean; virtual;
    procedure DoDeleteMessageBatch(const aURL: String;
      aDeleteBatchItems: TsgcSQSDeleteMessageBatchRequestItems); virtual;
  protected
    function DoChangeMessageVisibility(const aURL, aReceiptHandle: String;
      VisibilityTimeout: Integer): Boolean; virtual;
    function DoChangeMessageVisibilityBatch(const aURL: String;
      const aRequests: TsgcSQSChangeMessageVisibilityBatchRequests;
      var aResponseIds: TStringList): Boolean;
  protected
    function DoReceiveMessage(const aURL: String;
      const aRequest: TsgcSQSReceiveMessageRequest;
      var aResponses: TsgcSQSReceiveMessageResponses): Boolean; virtual;
  protected
    function DoGetQueueURL(const aQueueName: String): String; virtual;
    function DoCreateQueue(const aQueue: TsgcSQSCreateQueue): string; virtual;
    function DoDeleteQueue(const aURL: String): Boolean; virtual;
    function DoListQueues(var Queues: TStringList;
      const aQueueNamePrefix: String): Boolean; virtual;
    function DoListQueueTags(const aURL: String; var Tags: TsgcSQSTags)
      : Boolean; virtual;
    function DoGetQueueAttributes(const aURL: String;
      var Attributes: TsgcSQSAttributes): Boolean; virtual;
    function DoSetQueueAttributes(const aURL: String;
      const aAttributes: TsgcSQSAttributes): Boolean; virtual;
    function DoPurgueQueue(const aURL: String): Boolean; virtual;
    function DoTagQueue(const aURL: String; aTags: TsgcSQSTags): Boolean; virtual;
    function DoUnTagQueue(const aURL: String; aTags: TStringList): Boolean; virtual;
  protected
    function DoAddPermission(const aURL, aLabel: String;
      const aPermissions: TsgcSQSPermissions): Boolean; virtual;
    function DoRemovePermission(const aURL, aLabel: String): Boolean; virtual;
  protected
    function DoListDeadLetterSourceQueues(const aURL: String;
      aQueueURLs: TStringList): Boolean;
  public
    function SendMessage(const aQueueName, aMessageBody: String;
      aDelaySeconds: Integer = 0): Boolean; overload;
    function SendMessage(const aQueueName: String;
      aRequest: TsgcSQSSendMessageRequest;
      var aResponse: TsgcSQSSendMessageResponse): Boolean; overload;
    procedure SendMessageBatch(const aQueueName: String;
      aRequests: TsgcSQSSendMessageRequests;
      var aResponses: TsgcSQSSendMessageResponses);
  public
    function ReceiveMessage(const aQueueName: String;
      var aResponses: TsgcSQSReceiveMessageResponses): Boolean; overload;
    function ReceiveMessage(const aQueueName: String;
      const aRequest: TsgcSQSReceiveMessageRequest;
      var aResponses: TsgcSQSReceiveMessageResponses): Boolean; overload;
  public
    function ChangeMessageVisibility(const aQueueName, aReceiptHandle: String;
      VisibilityTimeout: Integer): Boolean;
    function ChangeMessageVisibilityBatch(const aQueueName: String;
      const aRequests: TsgcSQSChangeMessageVisibilityBatchRequests;
      var aResponseIds: TStringList): Boolean;
  public
    function DeleteMessage(const aQueueName, aReceiptHandle: String): Boolean;
    procedure DeleteMessageBatch(const aQueueName: String;
      aDeleteBatchItems: TsgcSQSDeleteMessageBatchRequestItems);
  public
    function GetQueueURL(const aQueueName: String): String;
    function CreateQueue(const aQueueName: String): string; overload;
    function CreateQueue(const aQueue: TsgcSQSCreateQueue): string; overload;
    function DeleteQueue(const aQueueName: String): Boolean;
    function ListQueues(var Queues: TStringList;
      const aQueueNamePrefix: String = ''): Boolean;
    function ListQueueTags(const aQueueName: String;
      var Tags: TsgcSQSTags): Boolean;
    function GetQueueAttributes(const aQueueName: String;
      var Attributes: TsgcSQSAttributes): Boolean;
    function SetQueueAttributes(const aQueueName: String;
      const aAttributes: TsgcSQSAttributes): Boolean;
    function PurgueQueue(const aQueueName: String): Boolean;
    function TagQueue(const aQueueName: String; aTags: TsgcSQSTags): Boolean;
        overload;
    function TagQueue(const aQueueName: String; aKey, aValue: String): Boolean;
        overload;
    function UnTagQueue(const aQueueName: String; aTags: TStringList): Boolean;
        overload;
    function UnTagQueue(const aQueueName: String; aKey: String): Boolean; overload;
  public
    function AddPermission(const aQueueName, aLabel: String;
      const aPermissions: TsgcSQSPermissions): Boolean;
    function RemovePermission(const aQueueName, aLabel: String): Boolean;
  public
    function ListDeadLetterSourceQueues(const aQueueName: String;
      aQueueURLs: TStringList): Boolean;
    { methods }

    { events }
  private
    FOnSQSBeforeRequest: TsgcOnAWSSQSBeforeRequest;
    FOnSQSError: TsgcOnAWSSQSError;
    FOnSQSResponse: TsgcOnAWSSQSResponse;
  protected
    function DoSQSResponse(const aRawResponse: String): Boolean; virtual;
    function DoSQSBeforeRequest(var aURL: String): Boolean; virtual;
    procedure DoSQSErrorEvent(const aError_Code, aError_Description,
      aRawError: String); virtual;
  public
    property OnSQSResponse: TsgcOnAWSSQSResponse read FOnSQSResponse
      write FOnSQSResponse;
    property OnSQSError: TsgcOnAWSSQSError read FOnSQSError write FOnSQSError;
    property OnSQSBeforeRequest: TsgcOnAWSSQSBeforeRequest
      read FOnSQSBeforeRequest write FOnSQSBeforeRequest;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_AWS}

uses
  StrUtils,
  // sgc
  sgcBase_Helpers;

function EncodeURIComponent(const aValue: string): {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF};
const
  HexMap: {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF} = '0123456789ABCDEF';

  function IsSafeChar(ch: Integer): Boolean;
  begin
    if (ch >= 48) and (ch <= 57) then Result := True // 0-9
    else if (ch >= 65) and (ch <= 90) then Result := True // A-Z
    else if (ch >= 97) and (ch <= 122) then Result := True // a-z
// fail on SQS    
//    else if (ch = 33) then Result := True // !
//    else if (ch >= 39) and (ch <= 42) then Result := True // '()*
    else if (ch >= 45) and (ch <= 46) then Result := True // -.
    else if (ch = 95) then Result := True // _
    else if (ch = 126) then Result := True // ~
    else Result := False;
  end;
var
  i, j: Integer;
  vUTF8: {$IFDEF NEXTGEN}String{$ELSE}AnsiString{$ENDIF};
begin
  Result := '';

  {$IFDEF NEXTGEN}
  vUTF8 := String(UTF8Encode(aValue));
  {$ELSE}
  vUTF8 := UTF8Encode(aValue);
  {$ENDIF}

  i := 1;
  j := 1;
  SetLength(Result, Length(vUTF8) * 3); // space to %xx encode every byte
  while i <= Length(vUTF8) do
  begin
    if IsSafeChar(Ord(vUTF8[i])) then
    begin
      Result[j] := vUTF8[i];
      Inc(j);
    end
    else
    begin
      Result[j] := '%';
      Result[j + 1] := HexMap[(Ord(vUTF8[i]) shr 4) + 1];
      Result[j + 2] := HexMap[(Ord(vUTF8[i]) and 15) + 1];
      Inc(j,3);
    end;
    Inc(i);
  end;

  SetLength(Result, j-1);
end;

function ConvertHTMLEntities(const aValue: string): string;
begin
  result := aValue;
  result := sgcStringReplace(result, '&lt;', '<');
  result := sgcStringReplace(result, '&gt;', '>');
  result := sgcStringReplace(result, '&quot;', '"');
  result := sgcStringReplace(result, '&amp;', '&');  
end;

function GetAWSSQSAttributeName(aAttribute: TsgcAWSSQSAttribute): String;
begin
  case aAttribute of
    sqsatAll:
      result := 'All';
    sqsatPolicy:
      result := 'Policy';
    sqsatVisibilityTimeout:
      result := 'VisibilityTimeout';
    sqsatMaximumMessageSize:
      result := 'MaximumMessageSize';
    sqsatMessageRetentionPeriod:
      result := 'MessageRetentionPeriod';
    sqsatApproximateNumberOfMessages:
      result := 'ApproximateNumberOfMessages';
    sqsatApproximateNumberOfMessagesNotVisible:
      result := 'ApproximateNumberOfMessagesNotVisible';
    sqsatCreatedTimestamp:
      result := 'CreatedTimestamp';
    sqsatLastModifiedTimestamp:
      result := 'LastModifiedTimestamp';
    sqsatQueueArn:
      result := 'QueueArn';
    sqsatApproximateNumberOfMessagesDelayed:
      result := 'ApproximateNumberOfMessagesDelayed';
    sqsatDelaySeconds:
      result := 'DelaySeconds';
    sqsatReceiveMessageWaitTimeSeconds:
      result := 'ReceiveMessageWaitTimeSeconds';
    sqsatRedrivePolicy:
      result := 'RedrivePolicy';
    sqsatFifoQueue:
      result := 'FifoQueue';
    sqsatContentBasedDeduplication:
      result := 'ContentBasedDeduplication';
    sqsatKmsMasterKeyId:
      result := 'KmsMasterKeyId';
    sqsatKmsDataKeyReusePeriodSeconds:
      result := 'KmsDataKeyReusePeriodSeconds';
  else
    result := '';
  end;
end;

destructor TsgcHTTP_Amazon_AWS_SQS_Client.Destroy;
begin
  sgcFree(FQueues);
  sgcFree(FParams);
  inherited;
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.AddParam(const aParam, aValue: String);
begin
  Params.Add(aParam + '=' + aValue);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.AddPermission(const aQueueName,
  aLabel: String; const aPermissions: TsgcSQSPermissions): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoAddPermission(vURL, aLabel, aPermissions);
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.ClearParams(const aAction: String);
begin
  Params.Clear;
  Params.Add('Action=' + aAction);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.CreateQueue(const aQueueName
  : String): string;
var
  oQueue: TsgcSQSCreateQueue;
begin
  oQueue := TsgcSQSCreateQueue.Create;
  Try
    oQueue.QueueName := aQueueName;
    result := DoCreateQueue(oQueue);
  Finally
    sgcFree(oQueue);
  End;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.CreateQueue(const aQueue
  : TsgcSQSCreateQueue): string;
begin
  result := DoCreateQueue(aQueue);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DeleteMessage(const aQueueName,
  aReceiptHandle: String): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoDeleteMessage(vURL, aReceiptHandle);
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.DeleteMessageBatch(const aQueueName
  : String; aDeleteBatchItems: TsgcSQSDeleteMessageBatchRequestItems);
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  DoDeleteMessageBatch(vURL, aDeleteBatchItems);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DeleteQueue(const aQueueName
  : String): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoDeleteQueue(vURL);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoAddPermission(const aURL,
  aLabel: String; const aPermissions: TsgcSQSPermissions): Boolean;
var
  i: Integer;
  vResult, vError: String;
begin
  result := False;

  ClearParams('AddPermission');

  for i := 0 to aPermissions.Count - 1 do
  begin
    AddParam('AWSAccountId.' + IntToStr(i + 1),
      TsgcSQSPermission(aPermissions.Item[i]).AWSAccountId);
    AddParam('ActionName.' + IntToStr(i + 1),
      TsgcSQSPermission(aPermissions.Item[i]).ActionName);
  end;

  AddParam('Label', aLabel);

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      result := True;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoBeforeGET_Request
  (var aURL: String): Boolean;
begin
  result := DoSQSBeforeRequest(aURL);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoCreateQueue
  (const aQueue: TsgcSQSCreateQueue): string;
var
  vResult, vError: String;
  i: Integer;
begin
  result := '';
  ClearParams('CreateQueue');

  for i := 0 to aQueue.Attributes.Count - 1 do
  begin
    AddParam('Attribute.' + IntToStr(i + 1) + '.Name',
      TsgcSQSAttribute(aQueue.Attributes.Item[i]).AttributeName);
    AddParam('Attribute.' + IntToStr(i + 1) + '.Value',
      TsgcSQSAttribute(aQueue.Attributes.Item[i]).AttributeValue);
  end;
  AddParam('QueueName', aQueue.QueueName);
  for i := 0 to aQueue.Attributes.Count - 1 do
  begin
    AddParam('Tag.' + IntToStr(i + 1) + '.Name',
      TsgcSQSTag(aQueue.Tags.Item[i]).Key);
    AddParam('Tag.' + IntToStr(i + 1) + '.Value',
      TsgcSQSTag(aQueue.Tags.Item[i]).Value);
  end;

  GetURL(GetFullURL, vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      result := sgcGetNodeValue(vResult, 'QueueUrl');
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoDeleteMessage(const aURL,
  aReceiptHandle: String): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('DeleteMessage');
  AddParam('ReceiptHandle', URIEncode(aReceiptHandle));

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      if sgcGetNodeValue(vResult, 'RequestId') <> '' then
        result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.DoDeleteMessageBatch
  (const aURL: String;
  aDeleteBatchItems: TsgcSQSDeleteMessageBatchRequestItems);
var
  i: Integer;
  vResult, vError: String;
  oItem: TsgcSQSDeleteMessageBatchRequestItem;
begin
  ClearParams('DeleteMessageBatch');
  for i := 1 to aDeleteBatchItems.Count do
  begin
    oItem := TsgcSQSDeleteMessageBatchRequestItem
      (aDeleteBatchItems.Item[i - 1]);
    if Assigned(oItem) then
    begin
      AddParam('DeleteMessageBatchRequestEntry.' + IntToStr(i) + '.Id',
        URIEncode(oItem.ID));
      AddParam('DeleteMessageBatchRequestEntry.' + IntToStr(i) +
        '.ReceiptHandle', URIEncode(oItem.ReceiptHandle));
    end;
    if i > 9 then
      break;
  end;

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      sgcGetNodeValue(vResult, 'RequestId')
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoDeleteQueue
  (const aURL: String): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('DeleteQueue');
  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      result := True;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoGetQueueURL(const aQueueName
  : String): String;
var
  vError: String;
begin
  ClearParams('GetQueueUrl');
  AddParam('QueueName', aQueueName);
  GetURL(GetFullURL, result, vError);
  if result <> '' then
  begin
    if DoSQSResponse(result) then
    begin
      result := sgcGetNodeValue(result, 'QueueUrl');
      if result <> '' then
        Queues.AddSQSQueue(aQueueName, result);
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoGetQueueAttributes(const aURL: String;
  var Attributes: TsgcSQSAttributes): Boolean;
var
  vResult, vError: String;
  oList: TStringList;
  i: Integer;
  vName: string;
  vValue: string;
begin
  result := False;

  ClearParams('GetQueueAttributes');
  AddParam('AttributeName.1', 'All');
  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      oList := TStringList.Create;
      Try
        sgcGetNodeList(vResult, 'Attribute', oList);
        for i := 0 to oList.Count - 1 do
        begin
          vName := sgcGetNodeValue(oList[i], 'Name');
          vValue := sgcGetNodeValue(oList[i], 'Value');
          if vName <> '' then
            Attributes.AddSQSAttribute(vName, vValue);
        end;
      Finally
        sgcFree(oList);
      End;
      result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoListQueues(var Queues: TStringList;
  const aQueueNamePrefix: String): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('ListQueues');
  if aQueueNamePrefix <> '' then
    AddParam('QueueNamePrefix', aQueueNamePrefix);
  GetURL(GetFullURL, vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      sgcGetNodeList(vResult, 'QueueUrl', Queues);
      result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoListQueueTags(const aURL: String;
  var Tags: TsgcSQSTags): Boolean;
var
  i: Integer;
  oList: TStringList;
  vKey: string;
  vResult, vError: string;
  vValue: string;
begin
  result := False;

  ClearParams('ListQueueTags');
  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      oList := TStringList.Create;
      Try
        sgcGetNodeList(vResult, 'Tag', oList);
        for i := 0 to oList.Count - 1 do
        begin
          vKey := sgcGetNodeValue(oList[i], 'Key');
          vValue := sgcGetNodeValue(oList[i], 'Value');
          if vKey <> '' then
            Tags.AddSQSTag(vKey, vValue);
        end;
        result := True;
      Finally
        sgcFree(oList);
      End;
    end;
  end
  else
    DoProcessError(vError);
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.DoProcessError(const aError: String);
var
  vCode: string;
  vMessage: string;
begin
  if aError = '' then
    exit;

  vCode := sgcGetNodeValue(aError, 'Code');
  vMessage := sgcGetNodeValue(aError, 'Message');
  DoSQSErrorEvent(vCode, vMessage, aError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoPurgueQueue
  (const aURL: String): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('PurgeQueue');

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      if sgcGetNodeValue(vResult, 'RequestId') <> '' then
        result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoReceiveMessage(const aURL: String;
  const aRequest: TsgcSQSReceiveMessageRequest;
  var aResponses: TsgcSQSReceiveMessageResponses): Boolean;
var
  i, j: Integer;
  oList, oAttributes: TStringList;
  oResponse: TsgcSQSReceiveMessageResponse;
  vResult, vError: String;
begin
  result := False;

  ClearParams('ReceiveMessage');
  if ((aRequest.MaxNumberOfMessages >= 1) and
    (aRequest.MaxNumberOfMessages <= 10)) then
    AddParam('MaxNumberOfMessages', aRequest.MaxNumberOfMessages);
  if aRequest.ReceiveRequestAttemptId <> '' then
    AddParam('ReceiveRequestAttemptId', aRequest.ReceiveRequestAttemptId);
  if aRequest.VisibilityTimeout > 0 then
    AddParam('VisibilityTimeout', aRequest.VisibilityTimeout);
  if aRequest.WaitTimeSeconds > 0 then
    AddParam('WaitTimeSeconds', aRequest.WaitTimeSeconds);
  for i := 0 to High(aRequest.Attributes) do
    AddParam('AttributeName.' + IntToStr(i + 1),
      GetAWSSQSAttributeName(aRequest.Attributes[i]));
  for i := 0 to aRequest.MessageAttributeName.Count - 1 do
    AddParam('MessageAttributeName.' + IntToStr(i + 1),
      aRequest.MessageAttributeName[i]);

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      oList := TStringList.Create;
      Try
        sgcGetNodeList(vResult, 'Message', oList);
        for i := 0 to oList.Count - 1 do
        begin
          oResponse := TsgcSQSReceiveMessageResponse.Create;

          oResponse.MessageId := sgcGetNodeValue(oList[i], 'MessageId');
{$IFDEF D2009}
{$IFDEF NEXTGEN}
{$WARNINGS Off}
          oResponse.Body :=
            UTF8ToString(sgcGetNodeValue(oList[i], 'Body'));
{$WARNINGS On}
{$ELSE}
          oResponse.Body :=
            UTF8ToString(RawByteString(sgcGetNodeValue(oList[i], 'Body')));
{$ENDIF}
{$ELSE}
          oResponse.Body := UTF8Decode(sgcGetNodeValue(oList[i], 'Body'));
{$ENDIF}
          oResponse.Body := ConvertHTMLEntities(oResponse.Body);
          oResponse.MD5OfBody := sgcGetNodeValue(oList[i], 'MD5OfBody');
          oResponse.MD5OfMessageAttributes :=
            sgcGetNodeValue(oList[i], 'MD5OfMessageAttributes');
          oResponse.MessageId := sgcGetNodeValue(oList[i], 'MessageId');
          oResponse.ReceiptHandle := sgcGetNodeValue(oList[i], 'ReceiptHandle');

          oAttributes := TStringList.Create;
          Try
            sgcGetNodeList(oList[i], 'Attribute', oAttributes);
            for j := 0 to oAttributes.Count - 1 do
              oResponse.Attributes.AddSQSAttribute
                (sgcGetNodeValue(oAttributes[i], 'Name'),
                sgcGetNodeValue(oAttributes[i], 'Value'));
          Finally
            sgcFree(oAttributes);
          End;

          oAttributes := TStringList.Create;
          Try
            sgcGetNodeList(oList[i], 'MessageAttribute', oAttributes);
            for j := 0 to oAttributes.Count - 1 do
              oResponse.MessageAttributes.AddSQSAttribute
                (sgcGetNodeValue(oAttributes[i], 'Name'),
                sgcGetNodeValue(oAttributes[i], 'Type'),
                sgcGetNodeValue(oAttributes[i], 'Value'));
          Finally
            sgcFree(oAttributes);
          End;

          aResponses.AddItem(oResponse);
        end;
        result := True;
      Finally
        sgcFree(oList);
      End;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoRemovePermission(const aURL,
  aLabel: String): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('RemovePermission');
  AddParam('Label', aLabel);

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      result := True;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoSendMessage(const aURL: String;
  const aRequest: TsgcSQSSendMessageRequest;
  var aResponse: TsgcSQSSendMessageResponse): Boolean;
var
  i: Integer;
  vResult, vError: String;
begin
  result := False;

  ClearParams('SendMessage');
  AddParam('MessageBody', aRequest.MessageBody);
  if ((aRequest.DelaySeconds > 0) and (aRequest.DelaySeconds <= 900)) then
    AddParam('DelaySeconds', aRequest.DelaySeconds);
  if aRequest.MessageDeduplicationId <> '' then
    AddParam('MessageDeduplicationId', aRequest.MessageDeduplicationId);
  if aRequest.MessageGroupId <> '' then
    AddParam('MessageGroupId', aRequest.MessageGroupId);
  if aRequest.MessageAttributes.Count > 0 then
  begin
    for i := 1 to aRequest.MessageAttributes.Count do
    begin
      AddParam('MessageAttribute.' + IntToStr(i) + '.Name',
        TsgcSQSMessageAttributeItem(aRequest.MessageAttributes.Item[i - 1])
        .AttributeName);
      AddParam('MessageAttribute.' + IntToStr(i) + '.Value.StringValue',
        TsgcSQSMessageAttributeItem(aRequest.MessageAttributes.Item[i - 1])
        .AttributeValue);
      AddParam('MessageAttribute.' + IntToStr(i) + '.Value.DataType',
        TsgcSQSMessageAttributeItem(aRequest.MessageAttributes.Item[i - 1])
        .AttributeType);
    end;
  end;
  if aRequest.MessageSystemAttributes.Count > 0 then
  begin
    for i := 1 to aRequest.MessageSystemAttributes.Count do
    begin
      AddParam('MessageSystemAttribute.' + IntToStr(i) + '.Name',
        TsgcSQSMessageAttributeItem(aRequest.MessageSystemAttributes.Item[i - 1])
        .AttributeName);
      AddParam('MessageSystemAttribute.' + IntToStr(i) + 'Value..StringValue',
        TsgcSQSMessageAttributeItem(aRequest.MessageSystemAttributes.Item[i - 1])
        .AttributeValue);
      AddParam('MessageSystemAttribute.' + IntToStr(i) + 'Value..DataType',
        TsgcSQSMessageAttributeItem(aRequest.MessageSystemAttributes.Item[i - 1])
        .AttributeType);
    end;
  end;

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      aResponse.MD5OfMessageBody := sgcGetNodeValue(vResult,
        'MD5OfMessageBody');
      aResponse.MD5OfMessageAttributes := sgcGetNodeValue(vResult,
        'MD5OfMessageAttributes');
      aResponse.MD5OfMessageSystemAttributes :=
        sgcGetNodeValue(vResult, 'MD5OfMessageSystemAttributes');
      aResponse.MessageId := sgcGetNodeValue(vResult, 'MessageId');
      aResponse.SequenceNumber := sgcGetNodeValue(vResult, 'SequenceNumber');

      result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoSendMessageBatch(const aURL: String;
  const aRequests: TsgcSQSSendMessageRequests;
  var aResponses: TsgcSQSSendMessageResponses): Boolean;
var
  i, j: Integer;
  vResult, vError: String;
  oRequest: TsgcSQSSendMessageRequest;
  oResponse: TsgcSQSSendMessageResponse;
  oList: TStringList;
begin
  result := False;

  ClearParams('SendMessageBatch');
  for i := 1 to aRequests.Count do
  begin
    oRequest := TsgcSQSSendMessageRequest(aRequests.Item[i - 1]);
    AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) + '.Id',
      oRequest.ID);
    AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) + '.MessageBody',
      oRequest.MessageBody);
    if ((oRequest.DelaySeconds > 0) and (oRequest.DelaySeconds <= 900)) then
      AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) + '.DelaySeconds',
        oRequest.DelaySeconds);
    if oRequest.MessageDeduplicationId <> '' then
      AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
        '.MessageDeduplicationId', oRequest.MessageDeduplicationId);
    if oRequest.MessageGroupId <> '' then
      AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
        '.MessageGroupId', oRequest.MessageGroupId);
    if oRequest.MessageAttributes.Count > 0 then
    begin
      for j := 1 to oRequest.MessageAttributes.Count do
      begin
        AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
          '.MessageAttribute.' + IntToStr(j) + '.Name',
          TsgcSQSMessageAttributeItem(oRequest.MessageAttributes.Item[j - 1])
          .AttributeName);
        AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
          '.MessageAttribute.' + IntToStr(j) + '.Value.StringValue',
          TsgcSQSMessageAttributeItem(oRequest.MessageAttributes.Item[j - 1])
          .AttributeValue);
        AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
          '.MessageAttribute.' + IntToStr(j) + '.Value.DataType',
          TsgcSQSMessageAttributeItem(oRequest.MessageAttributes.Item[j - 1])
          .AttributeType);
      end;
    end;
    if oRequest.MessageSystemAttributes.Count > 0 then
    begin
      for j := 1 to oRequest.MessageSystemAttributes.Count do
      begin
        AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
          '.MessageSystemAttribute.' + IntToStr(j) + '.Name',
          TsgcSQSMessageAttributeItem(oRequest.MessageSystemAttributes.Item[j - 1])
          .AttributeName);
        AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
          '.MessageSystemAttribute.' + IntToStr(j) + '.Value.StringValue',
          TsgcSQSMessageAttributeItem(oRequest.MessageSystemAttributes.Item[j - 1])
          .AttributeValue);
        AddParam('SendMessageBatchRequestEntry.' + IntToStr(i) +
          '.MessageSystemAttribute.' + IntToStr(j) + '.Value.DataType',
          TsgcSQSMessageAttributeItem(oRequest.MessageSystemAttributes.Item[j - 1])
          .AttributeType);
      end;
    end;

    if i > 9 then
      break;
  end;

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      oList := TStringList.Create;
      Try
        sgcGetNodeList(vResult, 'SendMessageBatchResultEntry', oList);
        for i := 0 to oList.Count - 1 do
        begin
          oResponse := TsgcSQSSendMessageResponse.Create;

          oResponse.ID := sgcGetNodeValue(oList[i], 'Id');
          oResponse.MD5OfMessageBody := sgcGetNodeValue(oList[i],
            'MD5OfMessageBody');
          oResponse.MD5OfMessageAttributes :=
            sgcGetNodeValue(oList[i], 'MD5OfMessageAttributes');
          oResponse.MD5OfMessageSystemAttributes :=
            sgcGetNodeValue(oList[i], 'MD5OfMessageSystemAttributes');
          oResponse.MessageId := sgcGetNodeValue(oList[i], 'MessageId');
          oResponse.SequenceNumber := sgcGetNodeValue(oList[i],
            'SequenceNumber');

          aResponses.AddItem(oResponse);
        end;
      Finally
        sgcFree(oList);
      end;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoSetQueueAttributes(const aURL: String;
  const aAttributes: TsgcSQSAttributes): Boolean;
var
  i: Integer;
  vResult, vError: String;
begin
  result := False;

  ClearParams('SetQueueAttributes');
  for i := 0 to aAttributes.Count - 1 do
  begin
    AddParam('Attribute.' + IntToStr(i + 1) + '.Name',
      TsgcSQSAttribute(aAttributes.Item[i]).AttributeName);
    AddParam('Attribute.' + IntToStr(i + 1) + '.Value',
      TsgcSQSAttribute(aAttributes.Item[i]).AttributeValue);
  end;

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      result := True;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoSQSBeforeRequest
  (var aURL: String): Boolean;
begin
  result := False;
  if Assigned(FOnSQSBeforeRequest) then
  begin
    FOnSQSBeforeRequest(self, aURL, result);
    result := not result;
  end
  else
    result := True;
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.DoSQSErrorEvent(const aError_Code,
  aError_Description, aRawError: String);
begin
  if Assigned(FOnSQSError) then
    FOnSQSError(self, aError_Code, aError_Description, aRawError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoSQSResponse(const aRawResponse
  : String): Boolean;
var
  vHandled: Boolean;
begin
  result := True;
  if Assigned(FOnSQSResponse) then
  begin
    vHandled := False;
    FOnSQSResponse(self, aRawResponse, vHandled);
    result := not vHandled;
  end;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoTagQueue(const aURL: String; aTags:
    TsgcSQSTags): Boolean;
var
  i: Integer;
  vResult, vError: String;
  oTag: TsgcSQSTag;
begin
  result := False;

  ClearParams('TagQueue');
  for i := 1 to aTags.Count do
  begin
    oTag := TsgcSQSTag(aTags.Item[i - 1]);
    if Assigned(oTag) then
    begin
      AddParam('Tag.' + IntToStr(i) + '.Key', URIEncode(oTag.Key));
      AddParam('Tag.' + IntToStr(i) + '.Value', URIEncode(oTag.Value));
    end;
  end;

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      if sgcGetNodeValue(vResult, 'RequestId') <> '' then
        result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoUnTagQueue(const aURL: String; aTags:
    TStringList): Boolean;
var
  i: Integer;
  vResult, vError: String;
begin
  result := False;

  ClearParams('UntagQueue');
  for i := 1 to aTags.Count do
    AddParam('TagKey.' + IntToStr(i), URIEncode(aTags[i - 1]));

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      if sgcGetNodeValue(vResult, 'RequestId') <> '' then
        result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function SortParams(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result := CompareStr(List[Index1], List[Index2]);
end;

constructor TsgcHTTP_Amazon_AWS_SQS_Client.Create(aOwner: TComponent);
begin
  inherited;
  AWSOptions.Service := 'sqs';
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.AddParam(const aParam: String;
  aValue: Integer);
begin
  AddParam(aParam, IntToStr(aValue));
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ChangeMessageVisibility
  (const aQueueName, aReceiptHandle: String;
  VisibilityTimeout: Integer): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoChangeMessageVisibility(vURL, aReceiptHandle, VisibilityTimeout);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ChangeMessageVisibilityBatch
  (const aQueueName: String;
  const aRequests: TsgcSQSChangeMessageVisibilityBatchRequests;
  var aResponseIds: TStringList): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoChangeMessageVisibilityBatch(vURL, aRequests, aResponseIds);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoChangeMessageVisibility(const aURL,
  aReceiptHandle: String; VisibilityTimeout: Integer): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('ChangeMessageVisibility');
  AddParam('ReceiptHandle', URIEncode(aReceiptHandle));
  AddParam('VisibilityTimeout', VisibilityTimeout);

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
      result := True;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoChangeMessageVisibilityBatch
  (const aURL: String;
  const aRequests: TsgcSQSChangeMessageVisibilityBatchRequests;
  var aResponseIds: TStringList): Boolean;
var
  i: Integer;
  vResult, vError: String;
begin
  result := False;

  ClearParams('ChangeMessageVisibilityBatch');

  for i := 0 to aRequests.Count - 1 do
  begin
    AddParam('ChangeMessageVisibilityBatchRequestEntry.' + IntToStr(i + 1) +
      '.Id', TsgcSQSChangeMessageVisibilityBatchRequest(aRequests.Item[i]).ID);
    AddParam('ChangeMessageVisibilityBatchRequestEntry.' + IntToStr(i + 1) +
      '.ReceiptHandle', TsgcSQSChangeMessageVisibilityBatchRequest
      (aRequests.Item[i]).ReceiptHandle);
    AddParam('ChangeMessageVisibilityBatchRequestEntry.' + IntToStr(i + 1) +
      '.VisibilityTimeout', TsgcSQSChangeMessageVisibilityBatchRequest
      (aRequests.Item[i]).VisibilityTimeout);
  end;

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      aResponseIds.Clear;
      sgcGetNodeList(vResult, 'Id', aResponseIds);
      result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.DoListDeadLetterSourceQueues
  (const aURL: String; aQueueURLs: TStringList): Boolean;
var
  vResult, vError: String;
begin
  result := False;

  ClearParams('ListDeadLetterSourceQueues');

  GetURL(GetFullURL(aURL), vResult, vError);
  if vResult <> '' then
  begin
    if DoSQSResponse(vResult) then
    begin
      aQueueURLs.Clear;
      sgcGetNodeList(vResult, 'QueueUrl', aQueueURLs);
      result := True;
    end;
  end
  else
    DoProcessError(vError);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.GetFullURL(const aEndPoint
  : String = ''): String;
var
  i: Integer;
  oList: TStringList;
begin
  if aEndPoint <> '' then
    result := aEndPoint + '?'
  else
    result := EndPoint + '?';

  oList := TStringList.Create;
  Try
    oList.CaseSensitive := True;
    for i := 0 to Params.Count - 1 do
      oList.Add(Params.Names[i]);
    oList.CustomSort(SortParams);

    for i := 0 to oList.Count - 1 do
    begin
      if i > 0 then
        result := result + '&';
      result := result + oList[i] + '=' + String(EncodeURIComponent(Params.Values[oList[i]]));
    end;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.GetParams: TStringList;
begin
  if not Assigned(FParams) then
    FParams := TStringList.Create;
  result := FParams;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.GetQueueAttributes(const aQueueName
  : String; var Attributes: TsgcSQSAttributes): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  Attributes.Clear;
  result := DoGetQueueAttributes(vURL, Attributes);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.GetQueues: TsgcSQSQueue;
begin
  if not Assigned(FQueues) then
    FQueues := TsgcSQSQueue.Create;
  result := FQueues;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.GetQueueURL(const aQueueName
  : String): String;
begin
  result := Queues.GetURLFromQueue(aQueueName);
  if result = '' then
    result := DoGetQueueURL(aQueueName);
  if result <> '' then
  begin
    if RightStr(result, 1) <> '/' then
      result := result + '/';
  end;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ListDeadLetterSourceQueues
  (const aQueueName: String; aQueueURLs: TStringList): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoListDeadLetterSourceQueues(vURL, aQueueURLs);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ListQueues(var Queues: TStringList;
  const aQueueNamePrefix: String = ''): Boolean;
begin
  Queues.Clear;

  result := DoListQueues(Queues, aQueueNamePrefix);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ListQueueTags(const aQueueName: String;
  var Tags: TsgcSQSTags): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  Tags.Clear;
  result := DoListQueueTags(vURL, Tags);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.PurgueQueue(const aQueueName
  : String): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoPurgueQueue(vURL);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ReceiveMessage(const aQueueName: String;
  var aResponses: TsgcSQSReceiveMessageResponses): Boolean;
var
  oRequest: TsgcSQSReceiveMessageRequest;
begin
  oRequest := TsgcSQSReceiveMessageRequest.Create;
  Try
    result := ReceiveMessage(aQueueName, oRequest, aResponses);
  Finally
    sgcFree(oRequest);
  End;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.ReceiveMessage(const aQueueName: String;
  const aRequest: TsgcSQSReceiveMessageRequest;
  var aResponses: TsgcSQSReceiveMessageResponses): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoReceiveMessage(vURL, aRequest, aResponses);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.RemovePermission(const aQueueName,
  aLabel: String): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoRemovePermission(vURL, aLabel);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.SendMessage(const aQueueName,
  aMessageBody: String; aDelaySeconds: Integer = 0): Boolean;
var
  vURL: String;
  oRequest: TsgcSQSSendMessageRequest;
  oResponse: TsgcSQSSendMessageResponse;
begin
  vURL := GetQueueURL(aQueueName);

  oRequest := TsgcSQSSendMessageRequest.Create;
  Try
    oRequest.MessageBody := aMessageBody;
    oRequest.DelaySeconds := aDelaySeconds;
    oResponse := TsgcSQSSendMessageResponse.Create;
    Try
      result := DoSendMessage(vURL, oRequest, oResponse);
    Finally
      sgcFree(oResponse);
    End;
  Finally
    sgcFree(oRequest);
  End;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.SendMessage(const aQueueName: String;
  aRequest: TsgcSQSSendMessageRequest;
  var aResponse: TsgcSQSSendMessageResponse): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoSendMessage(vURL, aRequest, aResponse);
end;

procedure TsgcHTTP_Amazon_AWS_SQS_Client.SendMessageBatch(const aQueueName
  : String; aRequests: TsgcSQSSendMessageRequests;
  var aResponses: TsgcSQSSendMessageResponses);
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  DoSendMessageBatch(vURL, aRequests, aResponses);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.SetQueueAttributes(const aQueueName
  : String; const aAttributes: TsgcSQSAttributes): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoSetQueueAttributes(vURL, aAttributes);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.TagQueue(const aQueueName: String;
    aTags: TsgcSQSTags): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoTagQueue(vURL, aTags);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.TagQueue(const aQueueName: String;
    aKey, aValue: String): Boolean;
var
  oTags: TsgcSQSTags;
begin
  oTags := TsgcSQSTags.Create;
  Try
    oTags.AddSQSTag(aKey, aValue);
    result := TagQueue(aQueueName, oTags);
  Finally
    sgcFree(oTags);
  End;
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.UnTagQueue(const aQueueName: String;
    aTags: TStringList): Boolean;
var
  vURL: String;
begin
  vURL := GetQueueURL(aQueueName);

  result := DoUnTagQueue(vURL, aTags);
end;

function TsgcHTTP_Amazon_AWS_SQS_Client.UnTagQueue(const aQueueName: String;
    aKey: String): Boolean;
var
  oTags: TStringList;
begin
  oTags := TStringList.Create;
  Try
    oTags.Add(aKey);
    result := UnTagQueue(aQueueName, oTags);
  Finally
    sgcFree(oTags);
  End;
end;

procedure TsgcSQSQueue.AddSQSQueue(const aQueue, aURL: String);
var
  oQueue: TsgcSQSQueueItem;
begin
  oQueue := TsgcSQSQueueItem(GetItem(aQueue));
  if not Assigned(oQueue) then
  begin
    oQueue := TsgcSQSQueueItem.Create;
    oQueue.ID := aQueue;
    oQueue.URL := aURL;
    AddItem(oQueue);
  end;
end;

function TsgcSQSQueue.GetSQSQueue(const aQueue: String): TsgcSQSQueueItem;
begin
  result := TsgcSQSQueueItem(GetItem(aQueue));
end;

function TsgcSQSQueue.GetURLFromQueue(const aQueueName: String): String;
var
  oQueue: TsgcSQSQueueItem;
begin
  result := '';
  oQueue := GetSQSQueue(aQueueName);
  if Assigned(oQueue) then
    result := oQueue.URL;
end;

procedure TsgcSQSDeleteMessageBatchRequestItems.AddSQSDeleteMessage(const aId,
  aReceiptHandle: String);
var
  oItem: TsgcSQSDeleteMessageBatchRequestItem;
begin
  oItem := TsgcSQSDeleteMessageBatchRequestItem(GetItem(aId));
  if not Assigned(oItem) then
  begin
    oItem := TsgcSQSDeleteMessageBatchRequestItem.Create;
    oItem.ID := aId;
    oItem.ReceiptHandle := aReceiptHandle;
    AddItem(oItem);
  end;
end;

procedure TsgcSQSTags.AddSQSTag(const Key, Value: String);
var
  oTag: TsgcSQSTag;
begin
  oTag := TsgcSQSTag(GetItem(Key));
  if not Assigned(oTag) then
  begin
    oTag := TsgcSQSTag.Create;
    oTag.ID := Key;
    oTag.Value := Value;
    AddItem(oTag);
  end;
end;

function TsgcSQSTag.GetKey: String;
begin
  result := ID;
end;

procedure TsgcSQSTag.SetKey(const Value: String);
begin
  ID := Value;
end;

function TsgcSQSMessageAttributeItem.GetAttributeName: String;
begin
  result := ID;
end;

procedure TsgcSQSMessageAttributeItem.SetAttributeName(const Value: String);
begin
  ID := Value;
end;

procedure TsgcSQSMessageAttributes.AddSQSAttribute(const aName, aType,
  aValue: String);
var
  oItem: TsgcSQSMessageAttributeItem;
begin
  oItem := TsgcSQSMessageAttributeItem(GetItem(aName));
  if not Assigned(oItem) then
  begin
    oItem := TsgcSQSMessageAttributeItem.Create;
    oItem.AttributeName := aName;
    oItem.AttributeType := aType;
    oItem.AttributeValue := aValue;
    AddItem(oItem);
  end;
end;

constructor TsgcSQSSendMessageRequest.Create;
begin
  inherited;
  DelaySeconds := 0;
end;

destructor TsgcSQSSendMessageRequest.Destroy;
begin
  sgcFree(FMessageSystemAttributes);
  sgcFree(FMessageAttributes);
  inherited;
end;

function TsgcSQSSendMessageRequest.GetMessageAttributes
  : TsgcSQSMessageAttributes;
begin
  if not Assigned(FMessageAttributes) then
    FMessageAttributes := TsgcSQSMessageAttributes.Create;
  result := FMessageAttributes;
end;

function TsgcSQSSendMessageRequest.GetMessageSystemAttributes
  : TsgcSQSMessageAttributes;
begin
  if not Assigned(FMessageSystemAttributes) then
    FMessageSystemAttributes := TsgcSQSMessageAttributes.Create;
  result := FMessageSystemAttributes;
end;

procedure TsgcSQSSendMessageRequest.SetMessageAttributes
  (const Value: TsgcSQSMessageAttributes);
var
  i: Integer;
begin
  MessageAttributes.Clear;

  for i := 0 to Value.Count - 1 do
  begin
    MessageAttributes.AddSQSAttribute(TsgcSQSMessageAttributeItem(Value.Item[i])
      .AttributeName, TsgcSQSMessageAttributeItem(Value.Item[i]).AttributeType,
      TsgcSQSMessageAttributeItem(Value.Item[i]).AttributeValue);
  end;
end;

procedure TsgcSQSSendMessageRequest.SetMessageSystemAttributes
  (const Value: TsgcSQSMessageAttributes);
var
  i: Integer;
begin
  MessageSystemAttributes.Clear;

  for i := 0 to Value.Count - 1 do
  begin
    MessageSystemAttributes.AddSQSAttribute
      (TsgcSQSMessageAttributeItem(Value.Item[i]).AttributeName,
      TsgcSQSMessageAttributeItem(Value.Item[i]).AttributeType,
      TsgcSQSMessageAttributeItem(Value.Item[i]).AttributeValue);
  end;
end;

destructor TsgcSQSSendMessage.Destroy;
begin
  sgcFree(FRequest);
  sgcFree(FResponse);
  inherited;
end;

function TsgcSQSSendMessage.GetRequest: TsgcSQSSendMessageRequest;
begin
  if not Assigned(FRequest) then
    FRequest := TsgcSQSSendMessageRequest.Create;
  result := FRequest;
end;

function TsgcSQSSendMessage.GetResponse: TsgcSQSSendMessageResponse;
begin
  if not Assigned(FResponse) then
    FResponse := TsgcSQSSendMessageResponse.Create;
  result := FResponse;
end;

destructor TsgcSQSReceiveMessage.Destroy;
begin
  sgcFree(FRequest);
  sgcFree(FResponse);
  inherited;
end;

function TsgcSQSReceiveMessage.GetRequest: TsgcSQSReceiveMessageRequest;
begin
  if not Assigned(FRequest) then
    FRequest := TsgcSQSReceiveMessageRequest.Create;
  result := FRequest;
end;

function TsgcSQSReceiveMessage.GetResponse: TsgcSQSReceiveMessageResponse;
begin
  if not Assigned(FResponse) then
    FResponse := TsgcSQSReceiveMessageResponse.Create;
  result := FResponse;
end;

destructor TsgcSQSReceiveMessageResponse.Destroy;
begin
  sgcFree(FAttributes);
  inherited;
end;

function TsgcSQSReceiveMessageResponse.GetAttributes: TsgcSQSAttributes;
begin
  if not Assigned(FAttributes) then
    FAttributes := TsgcSQSAttributes.Create;
  result := FAttributes;
end;

function TsgcSQSReceiveMessageResponse.GetMessageAttributes
  : TsgcSQSMessageAttributes;
begin
  if not Assigned(FMessageAttributes) then
    FMessageAttributes := TsgcSQSMessageAttributes.Create;
  result := FMessageAttributes;
end;

function TsgcSQSReceiveMessageResponse.GetMessageId: String;
begin
  result := ID;
end;

procedure TsgcSQSReceiveMessageResponse.SetMessageId(const Value: String);
begin
  ID := Value;
end;

procedure TsgcSQSReceiveMessageResponse.SetReceiptHandle(const Value: string);
begin
  FReceiptHandle := Value;
end;

constructor TsgcSQSReceiveMessageRequest.Create;
begin
  inherited;
  MaxNumberOfMessages := 1;
end;

destructor TsgcSQSReceiveMessageRequest.Destroy;
begin
  sgcFree(FMessageAttributeName);
  inherited;
end;

function TsgcSQSReceiveMessageRequest.GetMessageAttributeName: TStringList;
begin
  if not Assigned(FMessageAttributeName) then
    FMessageAttributeName := TStringList.Create;
  result := FMessageAttributeName;
end;

function TsgcSQSAttribute.GetAttributeName: String;
begin
  result := ID;
end;

procedure TsgcSQSAttribute.SetAttributeName(const Value: String);
begin
  ID := Value;
end;

procedure TsgcSQSAttributes.AddSQSAttribute(const aName, aValue: String);
var
  oItem: TsgcSQSAttribute;
begin
  oItem := TsgcSQSAttribute(GetItem(aName));
  if not Assigned(oItem) then
  begin
    oItem := TsgcSQSAttribute.Create;
    oItem.AttributeName := aName;
    oItem.AttributeValue := aValue;
    AddItem(oItem);
  end;
end;

procedure TsgcSQSAttributes.AddSQSAttribute(const aAttribute
  : TsgcAWSSQSAttribute; aValue: String);
var
  vName: String;
begin
  vName := GetAWSSQSAttributeName(aAttribute);
  if vName <> '' then
    AddSQSAttribute(vName, aValue);
end;

destructor TsgcSQSCreateQueue.Destroy;
begin
  sgcFree(FAttributes);
  sgcFree(FTags);
  inherited;
end;

function TsgcSQSCreateQueue.GetAttributes: TsgcSQSAttributes;
begin
  if not Assigned(FAttributes) then
    FAttributes := TsgcSQSAttributes.Create;
  result := FAttributes;
end;

function TsgcSQSCreateQueue.GetQueueName: String;
begin
  result := ID;
end;

function TsgcSQSCreateQueue.GetTags: TsgcSQSTags;
begin
  if not Assigned(FTags) then
    FTags := TsgcSQSTags.Create;
  result := FTags;
end;

procedure TsgcSQSCreateQueue.SetQueueName(const Value: String);
begin
  ID := Value;
end;

procedure TsgcSQSPermissions.AddSQSPermission(const ActionName
  : TsgcAWSSQSActionName; const AWSAccountId: String);
var
  vActionName: String;
begin
  case ActionName of
    sqsanAddPermission:
      vActionName := 'AddPermission';
    sqsanChangeMessageVisibility:
      vActionName := 'ChangeMessageVisibility';
    sqsanDeleteMessage:
      vActionName := 'DeleteMessage';
    sqsanDeleteQueue:
      vActionName := 'DeleteQueue';
    sqsanGetQueueAttributes:
      vActionName := 'GetQueueAttributes';
    sqsanGetQueueUrl:
      vActionName := 'GetQueueUrl';
    sqsanListDeadLetterSourceQueues:
      vActionName := 'ListDeadLetterSourceQueues';
    sqsanPurgeQueue:
      vActionName := 'PurgeQueue';
    sqsanReceiveMessage:
      vActionName := 'ReceiveMessage';
    sqsanRemovePermission:
      vActionName := 'RemovePermission';
    sqsanSendMessage:
      vActionName := 'SendMessage';
    sqsanSetQueueAttributes:
      vActionName := 'SetQueueAttributes';
  end;

  AddSQSPermission(vActionName, AWSAccountId);
end;

procedure TsgcSQSPermissions.AddSQSPermission(const ActionName: String;
  const AWSAccountId: String);
var
  oPermission: TsgcSQSPermission;
begin
  oPermission := TsgcSQSPermission(GetItem(ActionName + '_' + ActionName));
  if not Assigned(oPermission) then
  begin
    oPermission := TsgcSQSPermission.Create;
    oPermission.ID := ActionName + '_' + ActionName;
    oPermission.ActionName := ActionName;
    oPermission.AWSAccountId := AWSAccountId;
    AddItem(oPermission);
  end;
end;

procedure TsgcSQSChangeMessageVisibilityBatchRequests.AddSQSChangeVisibility
  (const aId, aReceiptHandle: String; aVisibilityTimeout: Integer);
var
  oItem: TsgcSQSChangeMessageVisibilityBatchRequest;
begin
  oItem := TsgcSQSChangeMessageVisibilityBatchRequest(GetItem(aId));
  if not Assigned(oItem) then
  begin
    oItem := TsgcSQSChangeMessageVisibilityBatchRequest.Create;
    oItem.ID := aId;
    oItem.ReceiptHandle := aReceiptHandle;
    oItem.VisibilityTimeout := aVisibilityTimeout;
    AddItem(oItem);
  end;
end;

{$ENDIF}

end.
