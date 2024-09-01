{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_sgc_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Protocol_Base_Message, sgcJSON, sgcWebSocket_Types,
  sgcWebSocket_Classes;

type

  TsgcWSAcknowledgment = procedure(Connection: TsgcWSConnection; const Id:
      string) of object;

  TsgcWSQoSItem = class
  private
    FGuid: String;
    FDate: TDateTime;
    FID: string;
    FText: String;
    procedure SetID(const Value: string);
  public
    property Guid: String read FGuid write FGuid;
    property Date: TDateTime read FDate write FDate;
    property ID: string read FID write SetID;
    property Text: String read FText write FText;
  end;


  TsgcWSQoSList = class({$IFDEF NEXTGEN}TList<TsgcWSQoSItem>{$ELSE}TObjectList{$ENDIF})

  end;

  TsgcWSQoS_Options = class(TPersistent)
  private
    FLevel: TWSQoS;
    FInterval: Integer;
    FTimeout: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Level: TWSQoS read FLevel write FLevel;
    property Interval: Integer read FInterval write FInterval;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TsgcWSMessage = class(TsgcWSMessage_Base)
  private
    Ferrorcode: Integer;
    Ferrordata: string;
    Ferrormessage: string;
    FGuid: String;
    Fid: string;
    FJSONResult: IsgcJSON;
    Fmethod: string;
    Fparams: string;
    Fresult: string;
    FText: String;
    Fversion: String;
    FQoS: String;
    FQueue: String;
    FChannel: String;
    function GetJSONResult: IsgcJSON;
  protected
    procedure DoReadMethods; virtual;
  public
    constructor Create(aOwner: TCOmponent); override;
  public
    procedure Clear(aForceClear: Boolean = False); override;
  public
    procedure Read(const aMessage: String); override;
    function Write: string; override;
  public
  { custom }
  public
    property Text: String read FText write FText;
    property Channel: String read FChannel write FChannel;
    property Guid: String read FGuid write FGuid;
    property QoS: String read FQoS write FQoS;
    property Queue: String read FQueue write FQueue;
  { custom }

  { json-rpc }
  public
    property errorcode: Integer read Ferrorcode write Ferrorcode;
    property errordata: string read Ferrordata write Ferrordata;
    property errormessage: string read Ferrormessage write Ferrormessage;
    property id: string read Fid write Fid;
    property JSONResult: IsgcJSON read GetJSONResult write FJSONResult;
    property method: string read Fmethod write Fmethod;
    property params: string read Fparams write Fparams;
    property result: string read Fresult write Fresult;
    property version: String read Fversion;
  { json-rpc }
  end;


{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers;

constructor TsgcWSMessage.Create(aOwner: TCOmponent);
begin
  inherited;
  Fversion := CS_JSONRPC_VERSION;
end;

procedure TsgcWSMessage.Clear(aForceClear: Boolean = False);
begin
  if (not FIsWriting and not FIsReading) or (aForceClear = True) then
  begin
    Text := '';
    Channel := '';
    errorcode := 0;
    errordata := '';
    errormessage := '';
    Guid := '';
    id := '';
    method := '';
    params := '';
    result := '';
    QoS := '';
    queue := '';
    JSONResult.Clear;
    inherited;
  end;
end;

procedure TsgcWSMessage.DoReadMethods;
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    if ((method = CS_SGC_BROADCAST) or (method = CS_SGC_MESSAGE)) then
    begin
      oJSON.Read(params);
      if oJSON.Node[CS_MESSAGE] <> nil then
        Text := oJSON.Node[CS_MESSAGE].Value;
      if oJSON.Node[CS_CHANNEL] <> nil then
        Channel := oJSON.Node[CS_CHANNEL].Value;
    end
    else if ((method = CS_SGC_PUBLISH) or (method = CS_SGC_EVENT)) then
    begin
      oJSON.Read(params);
      if oJSON.Node[CS_MESSAGE] <> nil then
        Text := oJSON.Node[CS_MESSAGE].Value;
      if oJSON.Node[CS_CHANNEL] <> nil then
        Channel := oJSON.Node[CS_CHANNEL].Value;
      if oJSON.Node[CS_QOS] <> nil then
        QoS := oJSON.Node[CS_QOS].Value;
      if oJSON.Node[CS_QUEUE] <> nil then
        Queue := oJSON.Node[CS_QUEUE].Value;
    end
    else if ((method = CS_SGC_SUBSCRIBE) or (method = CS_SGC_UNSUBSCRIBE)) then
    begin
      oJSON.Read(params);
      if oJSON.Node[CS_CHANNEL] <> nil then
        Channel := oJSON.Node[CS_CHANNEL].Value;
    end
    else if method = CS_SGC_UNSUBSCRIBE_ALL then
    begin
      oJSON.Read(params);
      if oJSON.Node[CS_CHANNEL] <> nil then
        Channel := oJSON.Node[CS_CHANNEL].Value;
    end
    else if ((method = CS_SGC_TRANSACTION) or (method = CS_SGC_COMMIT) or (method = CS_SGC_ROLLBACK)) then
    begin
      oJSON.Read(params);
      if oJSON.Node[CS_CHANNEL] <> nil then
        Channel := oJSON.Node[CS_CHANNEL].Value;
    end
    else
    begin
      oJSON.Read(params);
      if oJSON.Node[CS_QOS] <> nil then
        QoS := oJSON.Node[CS_QOS].Value;
      if oJSON.Node[CS_QUEUE] <> nil then
        Queue := oJSON.Node[CS_QUEUE].Value;
      if oJSON.Node[CS_CHANNEL] <> nil then
        Channel := oJSON.Node[CS_CHANNEL].Value;
    end;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

function TsgcWSMessage.GetJSONResult: IsgcJSON;
begin
  if not Assigned(FJSONResult) then
    FJSONResult := GetJSONInstance(self);
  Result := FJSONResult;
end;

procedure TsgcWSMessage.Read(const aMessage: String);
begin
  Try
    DoEnterRead(aMessage);
  Except
    On E: Exception do
    begin
      DoLeaveRead;
      raise;
    end;
  End;

  if DoReadJSONValue(CS_JSON_RPC) <> CS_JSONRPC_VERSION then
    raise Exception.Create(S_ERROR_DECODING_MESSAGE);

  // ... result
  if JSON.Node[CS_RESULT] <> nil then
  begin
    result := DoReadJSONValue(CS_RESULT);
    method := DoReadJSONValue(CS_METHOD, JSON.Node[CS_RESULT]);
    text := DoReadJSONValue(CS_MESSAGE, JSON.Node[CS_RESULT]);
    channel := DoReadJSONValue(CS_CHANNEL, JSON.Node[CS_RESULT]);
  end
  // ... error
  else if JSON.Node[CS_ERROR] <> nil then
  begin
    method := '';
    errorcode := DoReadJSONValue(CS_CODE, JSON.Node[CS_ERROR]);
    errormessage := DoReadJSONValue(CS_MESSAGE, JSON.Node[CS_ERROR]);
    errordata := DoReadJSONValue(CS_DATA, JSON.Node[CS_ERROR]);
  end
  else
  // ... method
  begin
    method := DoReadJSONValue(CS_METHOD);
    params := DoReadJSONValue(CS_PARAMS);
    // ... read methods
    DoReadMethods;
  end;
  if QoS = '' then
    QoS := DoReadJSONValue(CS_QOS);
  id := DoReadJSONValue(CS_ID);

  inherited;
end;

function TsgcWSMessage.Write: string;
var
  oJSON: IsgcObjectJSON;
begin
  DoEnterWrite;

  DoWriteJSONValue(CS_JSON_RPC, version);
  // ... error
  if errorcode <> 0  then
  begin
    oJSON := DoAddJSONObject(CS_ERROR);
    DoWriteJSONValue(CS_CODE, errorcode, oJSON);
    DoWriteJSONValue(CS_MESSAGE, errormessage, oJSON);
    if errordata <> '' then
      DoWriteJSONValue(CS_DATA, errordata, oJSON);
  end
  // ... result
  else if JSONResult.Count > 0 then
    DoAddJSONObject(CS_RESULT, JSONResult.Text)
  else if Fresult <> '' then
  begin
    // ... subscription
    if (method = CS_SGC_SUBSCRIBE) or (method = CS_SGC_UNSUBSCRIBE) then
    begin
      oJSON := DoAddJSONObject(CS_RESULT);
      DoWriteJSONValue(CS_METHOD, method, oJSON);
      DoWriteJSONValue(CS_CHANNEL, channel, oJSON);
    end
    else if method = CS_SGC_UNSUBSCRIBE_ALL then
    begin
      oJSON := DoAddJSONObject(CS_RESULT);
      DoWriteJSONValue(CS_METHOD, method, oJSON);
      DoWriteJSONValue(CS_CHANNEL, '', oJSON);
    end
    // ... broadcast
    else if ((method = CS_SGC_BROADCAST) or (method = CS_SGC_MESSAGE))  then
    begin
      oJSON := DoAddJSONObject(CS_RESULT);
      DoWriteJSONValue(CS_METHOD, CS_SGC_MESSAGE, oJSON);
      DoWriteJSONValue(CS_CHANNEL, channel, oJSON);
      DoWriteJSONValue(CS_MESSAGE, text, oJSON);
    end
    // ... publish
    else if ((method = CS_SGC_PUBLISH) or (method = CS_SGC_EVENT))  then
    begin
      oJSON := DoAddJSONObject(CS_RESULT);
      DoWriteJSONValue(CS_METHOD, method, oJSON);
      DoWriteJSONValue(CS_CHANNEL, channel, oJSON);
      DoWriteJSONValue(CS_MESSAGE, text, oJSON);
      if QoS <> '' then
        DoWriteJSONValue(CS_QOS, QoS, oJSON);
      if Queue <> '' then
        DoWriteJSONValue(CS_QUEUE, Queue, oJSON);
    end
    // ... session
    else if method = CS_SGC_SESSION  then
    begin
      oJSON := DoAddJSONObject(CS_RESULT);
      DoWriteJSONValue(CS_METHOD, method, oJSON);
      DoWriteJSONValue(CS_MESSAGE, text, oJSON);
    end
    // ... pubrec
    else if method = CS_SGC_PUBREC then
    begin
      oJSON := DoAddJSONObject(CS_RESULT);
      DoWriteJSONValue(CS_METHOD, method, oJSON);
    end
    else
      DoWriteJSONValue(CS_RESULT, Fresult);
  end
  else
  // ... method
  begin
    DoWriteJSONValue(CS_METHOD, method);
    if params <> '' then
      DoWriteJSONValue(CS_PARAMS, params)
    else if ((Text <> '') or (channel <> '') or (QoS <> '')) then
    begin
      oJSON := DoAddJSONObject(CS_PARAMS);
      if text <> '' then
        DoWriteJSONValue(CS_MESSAGE, Text, oJSON);
      if Channel <> '' then
        DoWriteJSONValue(CS_CHANNEL, Channel, oJSON);
      if QoS <> '' then
        DoWriteJSONValue(CS_QOS, QoS, oJSON);
      if Queue <> '' then
        DoWriteJSONValue(CS_QUEUE, Queue, oJSON);        
    end;
  end;
  if QoS <> '' then
    DoWriteJSONValue(CS_QOS, QoS);
  if id <> '' then
    DoWriteJSONValue(CS_ID, id);

  result := inherited Write;
end;

procedure TsgcWSQoSItem.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TsgcWSQoS_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSQoS_Options then
  begin
    Level := TsgcWSQoS_Options(aSource).Level;
    Interval := TsgcWSQoS_Options(aSource).Interval;
    Timeout := TsgcWSQoS_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
