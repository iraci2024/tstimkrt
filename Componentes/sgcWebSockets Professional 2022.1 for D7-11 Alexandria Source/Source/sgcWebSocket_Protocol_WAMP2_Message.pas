{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_WAMP2_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Protocol_Base_Message;

type

  TsgcWSMessageWAMP2 = class(TsgcWSMessage_Base)
  { helpers }
  private
    function DoReadMethodId: Integer;
    function GetRequestId: Int64;
  { helpers }

  { constructor }
  public
    constructor Create(aOwner: TComponent); override;
  { constructor }

  { from TsgcWSMessage_Base }
  public
    procedure Clear(aForceClear: Boolean = False); override;
    procedure Read(const aMessage: string); override;
    function Write: string; override;
  { from TsgcWSMessage_Base }

  { properties }
  private
    FArguments: String;
    FArgumentsKw: String;
    FAuthentication: String;
    FRequestId: Int64;
    FSubscriptionId: Int64;
    FErrorMethodId: Integer;
    FErrorRequestId: Int64;
    FErrorDetails: String;
    FErrorURI: String;
    FDetails: String;
    FSessionId: Int64;
    FTopicUri: string;
    FMethodId: Integer;
    FOptions: String;
    FPublicationId: Int64;
    FReason: String;
  public
    property Arguments: String read FArguments write FArguments;
    property ArgumentsKw: String read FArgumentsKw write FArgumentsKw;
    property Authentication: String read FAuthentication write FAuthentication;
    property RequestId: Int64 read FRequestId write FRequestId;
    property SubscriptionId: Int64 read FSubscriptionId write FSubscriptionId;
    property ErrorMethodId: Integer read FErrorMethodId write FErrorMethodId;
    property ErrorRequestId: Int64 read FErrorRequestId write FErrorRequestId;
    property ErrorDetails: String read FErrorDetails write FErrorDetails;
    property ErrorURI: String read FErrorURI write FErrorURI;
    property Details: String read FDetails write FDetails;
    property SessionId: Int64 read FSessionId write FSessionId;
    property TopicUri: string read FTopicUri write FTopicUri;
    property MethodId: Integer read FMethodId write FMethodId;
    property Options: String read FOptions write FOptions;
    property PublicationId: Int64 read FPublicationId write FPublicationId;
    property Reason: String read FReason write FReason;
  { properties }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Helpers, sgcWebSocket_Const, sgcJSON;

const
  CS_NULL = 'null';

constructor TsgcWSMessageWAMP2.Create(aOwner: TComponent);
begin
  inherited;
  JSON.IsArray := True;
end;

procedure TsgcWSMessageWAMP2.Clear(aForceClear: Boolean = False);
begin
  if (not FIsWriting and not FIsReading) or (aForceClear = True) then
  begin
    RequestId := 0;
    SubscriptionId := 0;
    ErrorMethodId := 0;
    ErrorRequestId := 0;
    ErrorDetails := '';
    ErrorURI := '';
    Details := '';
    SessionId := 0;
    TopicUri := '';
    MethodId := 0;
    Reason := '';
    PublicationId := 0;
    Arguments := '';
    ArgumentsKw := '';
    Options := '';
    Authentication := '';
    inherited;
  end;
end;

function TsgcWSMessageWAMP2.DoReadMethodId: Integer;
begin
  Result := -1;

  if JSON.count > 0 then
  begin
    if JSON.Item[0] <> nil then
      if JSON.Item[0].JSONType = sgcJSONNumber then
        Result := JSON.Item[0].Value;
  end;
end;

function TsgcWSMessageWAMP2.GetRequestId: Int64;
begin
  if RequestId = 0 then
    Result := StrToInt64(FormatDateTime('yymmddhhnnsszzz', Now))
  else
    Result := RequestId;
end;

procedure TsgcWSMessageWAMP2.Read(const aMessage: string);
begin
  DoEnterRead(aMessage);


  MethodId := DoReadMethodId;

  case MethodId of
    CS_WAMP2_WELCOME:
      begin
        SessionId := DoReadJSONValue(1);
        Details := DoReadJSONValue(2);
      end;
    CS_WAMP2_CHALLENGE:
      begin
        Authentication := DoReadJSONValue(1);
        Details := DoReadJSONValue(2);
      end;
    CS_WAMP2_ABORT:
      begin
        Details := DoReadJSONValue(1);
        Reason := DoReadJSONValue(2);
      end;
    CS_WAMP2_GOODBYE:
      begin
        Details := DoReadJSONValue(1);
        Reason := DoReadJSONValue(2);
      end;
    CS_WAMP2_SUBSCRIBED:
      begin
        RequestId := DoReadJSONValue(1);
        SubscriptionId := DoReadJSONValue(2);
      end;
    CS_WAMP2_UNSUBSCRIBED:
      begin
        RequestId := DoReadJSONValue(1);
      end;
    CS_WAMP2_REGISTERED:
      begin
        RequestId := DoReadJSONValue(1);
        SubscriptionId := DoReadJSONValue(2);
      end;
    CS_WAMP2_UNREGISTERED:
      begin
        RequestId := DoReadJSONValue(1);
        SubscriptionId := DoReadJSONValue(2);
      end;
    CS_WAMP2_EVENT:
      begin
        SubscriptionId := DoReadJSONValue(1);
        PublicationId := DoReadJSONValue(2);
        Details := DoReadJSONValue(3);
        Arguments := DoReadJSONValue(4);
        ArgumentsKw := DoReadJSONValue(5);
      end;
    CS_WAMP2_ERROR:
      begin
        ErrorMethodId := DoReadJSONValue(1);
        ErrorRequestId := DoReadJSONValue(2);
        ErrorDetails := DoReadJSONValue(3);
        ErrorUri := DoReadJSONValue(4);
      end;
    CS_WAMP2_PUBLISHED:
      begin
        RequestId := DoReadJSONValue(1);
        PublicationId := DoReadJSONValue(2);
      end;
    CS_WAMP2_RESULT:
      begin
        RequestId := DoReadJSONValue(1);
        Details := DoReadJSONValue(2);
        Arguments := DoReadJSONValue(3);
        ArgumentsKw := DoReadJSONValue(4);
      end;
  end;

  inherited;
end;

function TsgcWSMessageWAMP2.Write: string;
begin
  DoEnterWrite;

  case MethodId of
    CS_WAMP2_HELLO:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', TopicUri);
        DoWriteJSONValue('3', Details);
      end;
    CS_WAMP2_ABORT:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', Details);
        DoWriteJSONValue('3', Reason);
      end;
    CS_WAMP2_AUTHENTICATE:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', Authentication);
        DoWriteJSONValue('3', Details);
      end;
    CS_WAMP2_GOODBYE:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', Details);
        DoWriteJSONValue('3', Reason);
      end;
    CS_WAMP2_SUBSCRIBE:
      begin
        DoWriteJSONValue('1', MethodId);

        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', Options);
        DoWriteJSONValue('4', TopicUri);
      end;
    CS_WAMP2_UNSUBSCRIBE:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', SubscriptionId);
      end;
    CS_WAMP2_PUBLISH:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', Options);
        DoWriteJSONValue('4', TopicUri);
        if (Arguments <> '') then
        begin
          DoWriteJSONValue('5', Arguments);
          if (ArgumentsKw <> '') then
            DoWriteJSONValue('6', ArgumentsKw);
        end
      end;
    CS_WAMP2_CALL:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', Options);
        DoWriteJSONValue('4', TopicUri);
        if (Arguments <> '') then
        begin
          DoWriteJSONValue('5', Arguments);
          if (ArgumentsKw <> '') then
            DoWriteJSONValue('6', ArgumentsKw);
        end
      end;
    CS_WAMP2_REGISTER:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', Options);
        DoWriteJSONValue('4', TopicUri);
      end;
    CS_WAMP2_UNREGISTER:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', SubscriptionId);
      end;
    CS_WAMP2_INVOCATION:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', Details);
        if (Arguments <> '') then
        begin
          DoWriteJSONValue('4', Arguments);
          if (ArgumentsKw <> '') then
            DoWriteJSONValue('5', ArgumentsKw);
        end
      end;
    CS_WAMP2_INVOCATION_ERROR:
      begin
        DoWriteJSONValue('1', CS_WAMP2_ERROR);
        DoWriteJSONValue('2', CS_WAMP2_INVOCATION);
        DoWriteJSONValue('3', GetRequestId);
        DoWriteJSONValue('4', Details);
        DoWriteJSONValue('5', TopicUri);
        if (Arguments <> '') then
        begin
          DoWriteJSONValue('6', Arguments);
          if (ArgumentsKw <> '') then
            DoWriteJSONValue('7', ArgumentsKw);
        end
      end;
    CS_WAMP2_YIELD:
      begin
        DoWriteJSONValue('1', MethodId);
        DoWriteJSONValue('2', GetRequestId);
        DoWriteJSONValue('3', Options);
        if (Arguments <> '') then
        begin
          DoWriteJSONValue('4', Arguments);
          if (ArgumentsKw <> '') then
            DoWriteJSONValue('5', ArgumentsKw);
        end
      end;
  end;
  result := inherited Write;
end;

{$ENDIF}

end.
