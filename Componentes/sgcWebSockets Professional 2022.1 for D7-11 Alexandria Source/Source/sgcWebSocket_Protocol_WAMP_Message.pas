{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_WAMP_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Protocol_Base_Message;

type
  TsgcWSMessageWAMP = class(TsgcWSMessage_Base)
  { helpers }
  protected
    function DoReadTypeId: Integer;
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
    FArguments: string;
    FCallId: string;
    FCallResult: string;
    FEligible: String;
    FErrorDesc: String;
    FErrorDetails: String;
    FErrorURI: String;
    FEvent: String;
    FExclude: string;
    FPrefix: string;
    FPrefixURI: string;
    FProcUri: String;
    FSessionId: string;
    FTopicUri: string;
    FTypeId: Integer;
    FProtocolVersion: string;
    FServerIdent: string;
  public
    property Arguments: string read FArguments write FArguments;
    property CallId: string read FCallId write FCallId;
    property CallResult: string read FCallResult write FCallResult;
    property Eligible: String read FEligible write FEligible;
    property ErrorDesc: String read FErrorDesc write FErrorDesc;
    property ErrorDetails: String read FErrorDetails write FErrorDetails;
    property ErrorURI: String read FErrorURI write FErrorURI;
    property Event: String read FEvent write FEvent;
    property Exclude: string read FExclude write FExclude;
    property Prefix: string read FPrefix write FPrefix;
    property PrefixURI: string read FPrefixURI write FPrefixURI;
    property ProcUri: String read FProcUri write FProcUri;
    property SessionId: string read FSessionId write FSessionId;
    property TopicUri: string read FTopicUri write FTopicUri;
    property TypeId: Integer read FTypeId write FTypeId;
    property ProtocolVersion: string read FProtocolVersion write FProtocolVersion;
    property ServerIdent: string read FServerIdent write FServerIdent;
  { properties }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcBase_Const,
  sgcWebSocket_Helpers, sgcWebSocket_Const, sgcJSON;

const
  CS_NULL = 'null';

constructor TsgcWSMessageWAMP.Create(aOwner: TComponent);
begin
  inherited;
  JSON.IsArray := True;
end;

procedure TsgcWSMessageWAMP.Clear(aForceClear: Boolean = False);
begin
  if (not FIsWriting and not FIsReading) or (aForceClear = True) then
  begin
    Arguments := '';
    CallId := '';
    CallResult := '';
    Eligible := '';
    ErrorDesc := '';
    ErrorDetails := '';
    ErrorURI := '';
    Event := '';
    Exclude := '';
    Prefix := '';
    PrefixURI := '';
    ProcUri := '';
    SessionId := '';
    TopicUri := '';
    TypeId := 0;
    ProtocolVersion := '';
    ServerIdent := '';
    inherited;
  end;
end;

function TsgcWSMessageWAMP.DoReadTypeId: Integer;
begin
  Result := -1;

  if JSON.count > 0 then
  begin
    if JSON.Item[0] <> nil then
      if JSON.Item[0].JSONType = sgcJSONNumber then
        Result := JSON.Item[0].Value;
  end;
end;

procedure TsgcWSMessageWAMP.Read(const aMessage: string);
begin
  DoEnterRead(aMessage);

  TypeId := DoReadTypeId;

  case TypeId of
    CS_WAMP_WELCOME:
      begin
        SessionId := DoReadJSONValue(1);
        ProtocolVersion := DoReadJSONValue(2);
        ServerIdent := DoReadJSONValue(3);
      end;
    CS_WAMP_PREFIX:
      begin
        Prefix := DoReadJSONValue(1);
        PrefixURI := DoReadJSONValue(2);
      end;
    CS_WAMP_CALL:
      begin
        CallId := DoReadJSONValue(1);
        ProcURI := DoReadJSONValue(2);
        Arguments := DoReadJSONValue(3);
      end;
    CS_WAMP_CALLRESULT:
      begin
        CallId := DoReadJSONValue(1);
        CallResult := DoReadJSONValue(2);
      end;
    CS_WAMP_CALLERROR:
      begin
        CallId := DoReadJSONValue(1);
        ErrorURI := DoReadJSONValue(2);
        ErrorDesc := DoReadJSONValue(3);
        ErrorDetails := DoReadJSONValue(4);
      end;
    CS_WAMP_SUBSCRIBE:
      begin
        TopicURI := DoReadJSONValue(1);
      end;
    CS_WAMP_UNSUBSCRIBE:
      begin
        TopicURI := DoReadJSONValue(1);
      end;
    CS_WAMP_PUBLISH:
      begin
        TopicURI := DoReadJSONValue(1);
        Event := DoReadJSONValue(2);
        Exclude := DoReadJSONValue(3);
        Eligible := DoReadJSONValue(4);
      end;
    CS_WAMP_EVENT:
      begin
        TopicURI := DoReadJSONValue(1);
        Event := DoReadJSONValue(2);
      end;
    // ... custom events
    CS_WAMP_CALL_PROGRESS_RESULT:
      begin
        CallId := DoReadJSONValue(1);
        CallResult := DoReadJSONValue(2);
      end;
    CS_WAMP_CALL_CANCEL:
      CallId := DoReadJSONValue(1);
  end;

  inherited;
end;

function TsgcWSMessageWAMP.Write: string;
begin
  DoEnterWrite;

  DoWriteJSONValue('0', TypeId);
  case TypeId of
    CS_WAMP_WELCOME:
      begin
        DoWriteJSONValue('1', SessionId);
        DoWriteJSONValue('2', CS_WAMP_VERSION);
        DoWriteJSONValue('3', CS_APPLICATION_NAME + ' ' + CS_VERSION);
      end;
    CS_WAMP_PREFIX:
      begin
        DoWriteJSONValue('1', Prefix);
        DoWriteJSONValue('2', PrefixURI);
      end;
    CS_WAMP_CALL:
      begin
        DoWriteJSONValue('1', CallId);
        DoWriteJSONValue('2', ProcURI);
        if Arguments <> '' then
          DoWriteJSONValue('3', Arguments)
        else
          DoWriteJSONValue('3', CS_NULL);
      end;
    CS_WAMP_CALLRESULT:
      begin
        DoWriteJSONValue('1', CallId);
        if CallResult <> '' then
          DoWriteJSONValue('2', CallResult)
        else
          DoWriteJSONValue('2', CS_NULL);
      end;
    CS_WAMP_CALLERROR:
      begin
        DoWriteJSONValue('1', CallId);
        DoWriteJSONValue('2', ErrorURI);
        DoWriteJSONValue('3', ErrorDesc);
        if ErrorDetails <> '' then
          DoWriteJSONValue('4', ErrorDetails);
      end;
    CS_WAMP_SUBSCRIBE:
      begin
        DoWriteJSONValue('1', TopicURI);
      end;
    CS_WAMP_UNSUBSCRIBE:
      begin
        DoWriteJSONValue('1', TopicURI);
      end;
    CS_WAMP_PUBLISH:
      begin
        DoWriteJSONValue('1', TopicURI);
        if Event = '' then
          DoWriteJSONValue('2', CS_NULL)
        else
          DoWriteJSONValue('2', Event);
        DoWriteJSONValue('3', Eligible);
        DoWriteJSONValue('4', Exclude);
      end;
    CS_WAMP_EVENT:
      begin
        DoWriteJSONValue('1', TopicURI);
        DoWriteJSONValue('2', Event);
      end;
    // ... custom events
    CS_WAMP_CALL_PROGRESS_RESULT:
      begin
        DoWriteJSONValue('1', CallId);
        if CallResult <> '' then
          DoWriteJSONValue('2', CallResult)
        else
          DoWriteJSONValue('2', CS_NULL);
      end;
    CS_WAMP_CALL_CANCEL:
      DoWriteJSONValue('1', CallId);
  end;
  result := inherited Write;
end;

{$ENDIF}

end.
