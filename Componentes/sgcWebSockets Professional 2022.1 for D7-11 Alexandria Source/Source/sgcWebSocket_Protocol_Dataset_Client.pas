{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Dataset_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, DB,
  // sgc
  sgcWebSocket_Protocol_sgc_Client, sgcWebSocket_Types,
  sgcWebSocket_Protocol_dataset_message, sgcWebSocket_HTTPResponse,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_sgc_Message, sgcJSON;

type

  TsgcWSProtocol_dataset_client = class(TsgcWSProtocol_sgc_client)

  { TsgcWSMessageDataset }
  private
    FWSMessageDataset: TsgcWSMessageDataset;
    function GetWSMessageDataset: TsgcWSMessageDataset;
  protected
    property WSMessageDataset: TsgcWSMessageDataset read GetWSMessageDataset write
        FWSMessageDataset;
  { TsgcWSMessageDataset }

  { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; aCode: Integer);
        override;
  { from TsgcWSComponent }

  { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
  { from TsgcWSProtocol }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  { subscription }
  private
    FAutoSubscribe: Boolean;
  public
    procedure Subscribe_all;
    procedure UnSubscribe_all;
  public
    property AutoSubscribe: Boolean read FAutoSubscribe write FAutoSubscribe;
  { subscription }

  { dataset }
  private
    FDataSet: TDataSet;
  protected
    procedure SetDataSet(const Value: TDataSet);
  public
    property DataSet: TDataSet read FDataSet write SetDataSet;
  { dataset }

  { events }
  private
    FDSBeforePost: TDataSetNotifyEvent;
    FDSBeforeDelete: TDataSetNotifyEvent;
    FDSAfterPost: TDataSetNotifyEvent;
    FDSNewRecord: TDataSetNotifyEvent;
    FDSAfterEdit: TDataSetNotifyEvent;
    FDSAfterCancel: TDataSetNotifyEvent;
  protected
    procedure OnUpdateRecordEvent(Sender: TDataSet); virtual;
    procedure OnDeleteRecordEvent(Sender: TDataSet); virtual;
    procedure OnAfterPostEvent(Sender: TDataSet); virtual;
    procedure OnAfterEditEvent(Sender: TDataSet); virtual;
    procedure OnNewRecordEvent(Sender: TDataSet); virtual;
    procedure OnAfterCancelEvent(Sender: TDataSet); virtual;
  { events }

  { methods }
  private
    FSynchronizing: Boolean;
  protected
    procedure DoWriteData(aMessage: TsgcWSMessage; const aMethod: String; const
        aChannel: String = ''; const aGuid: String = ''); virtual;
  public
    procedure Synchronize;
    procedure GetMetaData;
  { methods }

  { events }
  private
    FOnBeforeDeleteRecord: TsgcWSBeforeRecordEvent;
    FOnBeforeNewRecord: TsgcWSBeforeRecordEvent;
    FOnBeforeUpdateRecord: TsgcWSBeforeRecordEvent;
    FOnAfterDeleteRecord: TsgcWSAfterRecordEvent;
    FOnAfterNewRecord: TsgcWSAfterRecordEvent;
    FOnAfterUpdateRecord: TsgcWSAfterRecordEvent;
  private
    FOnBeforeDatasetUpdate: TsgcWSBeforeDatasetUpdateEvent;
  private
    FOnAfterSynchronize: TNotifyEvent;
    FOnBeforeSynchronize: TNotifyEvent;
  private
    FOnMetaData: TsgcWSMetaDataEvent;
  private
    procedure OnBeforeNewRecordEvent(aConnection: TsgcWSConnection; const Dataset:
        TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
    procedure OnBeforeUpdateRecordEvent(aConnection: TsgcWSConnection; const
        Dataset: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
    procedure OnBeforeDeleteRecordEvent(aConnection: TsgcWSConnection; const
        Dataset: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
    procedure OnAfterNewRecordEvent(aConnection: TsgcWSConnection; const DataSet:
        TDataset);
    procedure OnAfterUpdateRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
    procedure OnAfterDeleteRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
  private
    procedure OnBeforeDatasetUpdateEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON; var Handled: Boolean);
  private
    procedure OnBeforeSynchronizeEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON);
    procedure OnAfterSynchronizeEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON);
  private
    procedure OnMetaDataEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON);
    procedure SetOnBeforeSynchronize(const Value: TNotifyEvent);
  protected
    procedure DoBeforeNewRecordEvent(const aConnection: TsgcWSConnection; const
        Dataset: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
        virtual;
    procedure DoBeforeUpdateRecordEvent(const aConnection: TsgcWSConnection; const
        Dataset: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
        virtual;
    procedure DoBeforeDeleteRecordEvent(const aConnection: TsgcWSConnection; const
        Dataset: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
        virtual;
    procedure DoAfterNewRecordEvent(aConnection: TsgcWSConnection; const DataSet:
        TDataset); virtual;
    procedure DoAfterUpdateRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset); virtual;
    procedure DoAfterDeleteRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset); virtual;
  protected
    procedure DoBeforeDatasetUpdateEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON; var Handled: Boolean); virtual;
  protected
    procedure DoBeforeSynchronizeEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON); virtual;
    procedure DoAfterSynchronizeEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON); virtual;
  public
    property OnBeforeNewRecord: TsgcWSBeforeRecordEvent read FOnBeforeNewRecord write
        FOnBeforeNewRecord;
    property OnBeforeUpdateRecord: TsgcWSBeforeRecordEvent read FOnBeforeUpdateRecord
        write FOnBeforeUpdateRecord;
    property OnBeforeDeleteRecord: TsgcWSBeforeRecordEvent read FOnBeforeDeleteRecord
        write FOnBeforeDeleteRecord;
    property OnAfterNewRecord: TsgcWSAfterRecordEvent read FOnAfterNewRecord write
        FOnAfterNewRecord;
    property OnAfterUpdateRecord: TsgcWSAfterRecordEvent read FOnAfterUpdateRecord
        write FOnAfterUpdateRecord;
    property OnAfterDeleteRecord: TsgcWSAfterRecordEvent read FOnAfterDeleteRecord
        write FOnAfterDeleteRecord;
  public
    property OnBeforeDatasetUpdate: TsgcWSBeforeDatasetUpdateEvent read
        FOnBeforeDatasetUpdate write FOnBeforeDatasetUpdate;
  public
    property OnBeforeSynchronize: TNotifyEvent read FOnBeforeSynchronize write
        SetOnBeforeSynchronize;
    property OnAfterSynchronize: TNotifyEvent read FOnAfterSynchronize write
        FOnAfterSynchronize;
  public
    property OnMetaData: TsgcWSMetaDataEvent read FOnMetaData write FOnMetaData;
  { events }

  { properties }
  private
    FNotifyUpdates: Boolean;
    FApplyUpdates: Boolean;
    FAutoEscapeText: Boolean;
    FEncodeBase64: Boolean;
    FFormatSettings: TsgcWSDatasetFormatSettings;
    FUpdateMode: TwsDatasetUpdateMode;
    procedure SetFormatSettings(const Value: TsgcWSDatasetFormatSettings);
  public
    property NotifyUpdates: Boolean read FNotifyUpdates write FNotifyUpdates;
    property ApplyUpdates: Boolean read FApplyUpdates write FApplyUpdates;
    property AutoEscapeText: Boolean read FAutoEscapeText write FAutoEscapeText;
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
    property FormatSettings: TsgcWSDatasetFormatSettings read FFormatSettings write
        SetFormatSettings;
    property UpdateMode: TwsDatasetUpdateMode read FUpdateMode write FUpdateMode;
    { properties }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses sgcWebSocket_Helpers, sgcWebSocket_Const;

constructor TsgcWSProtocol_dataset_client.Create(aOwner: TComponent);
begin
  inherited;
  FSynchronizing := False;
  FProtocol := CS_PROTOCOL_DATASET;
  FFormatSettings := TsgcWSDataSetFormatSettings.Create;
  ApplyUpdates := True;
  NotifyUpdates := True;
  UpdateMode := upWhereAll;
  AutoEscapeText := False;
  EncodeBase64 := False;
end;

destructor TsgcWSProtocol_dataset_client.Destroy;
begin
  FDataset := nil;
  sgcFree(FFormatSettings);
  inherited;
end;

procedure TsgcWSProtocol_dataset_client.DoAfterDeleteRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  if Assigned(FOnAfterDeleteRecord) then FOnAfterDeleteRecord(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_client.DoAfterNewRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  if Assigned(FOnAfterNewRecord) then FOnAfterNewRecord(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_client.DoAfterSynchronizeEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON);
begin

end;

procedure TsgcWSProtocol_dataset_client.DoAfterUpdateRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  if Assigned(FOnAfterUpdateRecord) then FOnAfterUpdateRecord(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_client.DoBeforeDatasetUpdateEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeDatasetUpdate) then
    FOnBeforeDatasetUpdate(aConnection, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.DoBeforeDeleteRecordEvent(const
    aConnection: TsgcWSConnection; const Dataset: TDataset; const JSON:
    IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeDeleteRecord) then
    FOnBeforeDeleteRecord(aConnection, Dataset, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.DoBeforeNewRecordEvent(const
    aConnection: TsgcWSConnection; const Dataset: TDataset; const JSON:
    IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeNewRecord) then
    FOnBeforeNewRecord(aConnection, Dataset, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.DoBeforeSynchronizeEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON);
begin

end;

procedure TsgcWSProtocol_dataset_client.DoBeforeUpdateRecordEvent(const
    aConnection: TsgcWSConnection; const Dataset: TDataset; const JSON:
    IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeUpdateRecord) then
    FOnBeforeUpdateRecord(aConnection, Dataset, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  inherited;
  if AutoSubscribe then
    Subscribe_all;
end;

procedure TsgcWSProtocol_dataset_client.DoEventDisconnect(aConnection:
    TsgcWSConnection; aCode: Integer);
begin
//  Dataset := nil;
  inherited;
end;

procedure TsgcWSProtocol_dataset_client.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
  {$ENDIF}

  // ... custom message
  if DoRawMessage(aConnection, Text) then exit;

  // ... queue
  if QueueMessages then
  begin
    QueueMessage.AddNotifyObject(aConnection, Text);
    exit;
  end;

  // ... process message
  WSMessage.Read(Text);

  if WSMessage.Method = CS_SGC_DATASET  then
  begin
    if ApplyUpdates then
    begin
      FSynchronizing := True;
      Try
        WSMessageDataset.Read(aConnection, Text);
      Finally
        FSynchronizing := False;
      End;
    end;
  end
  else if WSMessage.method = CS_SGC_DATASET_SYNCHRONIZE_START then
  begin
    if Assigned(FOnBeforeSynchronize) then
      FOnBeforeSynchronize(self)
  end
  else if WSMessage.method = CS_SGC_DATASET_SYNCHRONIZE_END then
  begin
    if Assigned(FOnAfterSynchronize) then
      FOnAfterSynchronize(self)
  end
  else if WSMessage.method = CS_SGC_METADATA then
      WSMessageDataSet.Read(aConnection, Text)
  else
    inherited;
end;

procedure TsgcWSProtocol_dataset_client.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  if AutoSubscribe then
    Subscribe_all;
end;

procedure TsgcWSProtocol_dataset_client.DoWriteData(aMessage: TsgcWSMessage;
    const aMethod: String; const aChannel: String = ''; const aGuid: String =
    '');
begin
  if not NotifyUpdates then exit;

  WSMessageDataSet.DoEnterWrite;

  WSMessageDataSet.method := aMethod;
  WSMessageDataSet.Channel := aChannel;
  WSMessageDataSet.Guid := aGuid;

  DoWriteRawData(WSMessageDataSet.Write);
end;

function TsgcWSProtocol_dataset_client.GetWSMessageDataset:
    TsgcWSMessageDataset;
begin
  if not Assigned(FWSMessageDataset) then
  begin
    FWSMessageDataset := TsgcWSMessageDataset.Create(self);
    FWSMessageDataset.OnBeforeNewRecord := OnBeforeNewRecordEvent;
    FWSMessageDataset.OnBeforeUpdateRecord := OnBeforeUpdateRecordEvent;
    FWSMessageDataset.OnBeforeDeleteRecord := OnBeforeDeleteRecordEvent;
    FWSMessageDataset.OnAfterNewRecord := OnAfterNewRecordEvent;
    FWSMessageDataset.OnAfterUpdateRecord := OnAfterUpdateRecordEvent;
    FWSMessageDataset.OnAfterDeleteRecord := OnAfterDeleteRecordEvent;
    FWSMessageDataset.OnBeforeDatasetUpdate := OnBeforeDatasetUpdateEvent;
    FWSMessageDataset.OnBeforeSynchronize := OnBeforeSynchronizeEvent;
    FWSMessageDataset.OnAfterSynchronize := OnAfterSynchronizeEvent;

    FWSMessageDataset.OnMetaData := OnMetaDataEvent;

    FWSMessageDataset.UpdateMode := UpdateMode;
    FWSMessageDataSet.AutoEscapeText := AutoEscapeText;
    FWSMessageDataSet.EncodeBase64 := EncodeBase64;

    FWSMessageDataSet.FormatSettings.Assign(FormatSettings);
  end;
  Result := FWSMessageDataset;
end;

procedure TsgcWSProtocol_dataset_client.GetMetaData;
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_DATASET_METADATA;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_dataset_client.OnAfterCancelEvent(Sender: TDataSet);
begin
  if NotifyUpdates then
    QueueMessages := False;

  if Assigned(FDSAfterCancel) then
    FDSAfterCancel(Sender);
end;

procedure TsgcWSProtocol_dataset_client.OnAfterDeleteRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterDeleteRecordEvent(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_client.OnAfterEditEvent(Sender: TDataSet);
begin
  if NotifyUpdates then
    QueueMessages := True;

  if Assigned(FDSAfterEdit) then
    FDSAfterEdit(Sender);
end;

procedure TsgcWSProtocol_dataset_client.OnAfterNewRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterNewRecordEvent(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_client.OnNewRecordEvent(Sender: TDataSet);
begin
  if NotifyUpdates then
    QueueMessages := True;

  if Assigned(FDSNewRecord) then
    FDSNewRecord(Sender);
end;

procedure TsgcWSProtocol_dataset_client.OnAfterPostEvent(Sender: TDataSet);
begin
  if NotifyUpdates then
    QueueMessages := False;

  if Assigned(FDSAfterPost) then
    FDSAfterPost(Sender);
end;

procedure TsgcWSProtocol_dataset_client.OnAfterSynchronizeEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON);
begin
  DoAfterSynchronizeEvent(aConnection, JSON);
end;

procedure TsgcWSProtocol_dataset_client.OnAfterUpdateRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterUpdateRecordEvent(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_client.OnBeforeDatasetUpdateEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  DoBeforeDatasetUpdateEvent(aConnection, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.OnBeforeDeleteRecordEvent(aConnection:
    TsgcWSConnection; const Dataset: TDataset; const JSON: IsgcObjectJSON; var
    Handled: Boolean);
begin
  DoBeforeDeleteRecordEvent(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.OnBeforeNewRecordEvent(aConnection:
    TsgcWSConnection; const Dataset: TDataset; const JSON: IsgcObjectJSON; var
    Handled: Boolean);
begin
  DoBeforeNewRecordEvent(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.OnBeforeSynchronizeEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON);
begin
  DoBeforeSynchronizeEvent(aConnection, JSON);
end;

procedure TsgcWSProtocol_dataset_client.OnBeforeUpdateRecordEvent(aConnection:
    TsgcWSConnection; const Dataset: TDataset; const JSON: IsgcObjectJSON; var
    Handled: Boolean);
begin
  DoBeforeUpdateRecordEvent(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_client.OnDeleteRecordEvent(Sender: TDataSet);
begin
  if Assigned(FDSBeforeDelete) then FDSBeforeDelete(Sender);

  if not FSynchronizing then
    DoWriteData(WSMessageDataSet, CS_SGC_DATASET, CS_SGC_DATASET_DELETE);
end;

procedure TsgcWSProtocol_dataset_client.OnMetaDataEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON);
begin
  if Assigned(FOnMetaData) then
    FOnMetaData(aConnection, JSON);
end;

procedure TsgcWSProtocol_dataset_client.OnUpdateRecordEvent(Sender: TDataSet);
begin
  if Assigned(FDSBeforePost) then FDSBeforePost(Sender);

  if not FSynchronizing then
  begin
    if Sender.State in [dsInsert] then
      DoWriteData(WSMessageDataSet, CS_SGC_DATASET, CS_SGC_DATASET_NEW)
    else
      DoWriteData(WSMessageDataSet, CS_SGC_DATASET, CS_SGC_DATASET_UPDATE);
  end;
end;

procedure TsgcWSProtocol_dataset_client.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    FDSBeforePost := nil;
    FDSBeforeDelete := nil;
    FDSNewRecord := nil;
    FDSAfterEdit := nil;
    FDSAfterPost := nil;
    FDSAfterCancel := nil;
    if Assigned(FDataSet) then
    begin
      FDSBeforePost := FDataSet.BeforePost;
      FDSBeforeDelete := FDataSet.BeforeDelete;
      FDSNewRecord := FDataSet.OnNewRecord;
      FDSAfterEdit := FDataSet.AfterEdit;
      FDSAfterPost := FDataSet.AfterPost;
      FDSAfterCancel := FDataSet.AfterCancel;
      FDataSet.BeforePost := OnUpdateRecordEvent;
      FDataSet.BeforeDelete := OnDeleteRecordEvent;
      FDataSet.AfterPost := OnAfterPostEvent;
      FDataSet.OnNewRecord := OnNewRecordEvent;
      FDataSet.AfterEdit := OnAfterEditEvent;
      FDataSet.AfterCancel := OnAfterCancelEvent;
    end;

    WSMessageDataSet.DataSet := Value;
  end;
end;

procedure TsgcWSProtocol_dataset_client.SetFormatSettings(const Value:
    TsgcWSDatasetFormatSettings);
begin
  if Assigned(FFormatSettings) then
    FFormatSettings.Assign(Value);
end;

procedure TsgcWSProtocol_dataset_client.SetOnBeforeSynchronize(const Value:
    TNotifyEvent);
begin
  FOnBeforeSynchronize := Value;
end;

procedure TsgcWSProtocol_dataset_client.Subscribe_all;
begin
  subscribe(CS_SGC_DATASET_NEW);
  subscribe(CS_SGC_DATASET_UPDATE);
  subscribe(CS_SGC_DATASET_DELETE);
end;

procedure TsgcWSProtocol_dataset_client.Synchronize;
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_DATASET_SYNCHRONIZE;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_dataset_client.UnSubscribe_all;
begin
  unsubscribe(CS_SGC_DATASET_NEW);
  unsubscribe(CS_SGC_DATASET_UPDATE);
  unsubscribe(CS_SGC_DATASET_DELETE);
end;

{$ENDIF}

end.
