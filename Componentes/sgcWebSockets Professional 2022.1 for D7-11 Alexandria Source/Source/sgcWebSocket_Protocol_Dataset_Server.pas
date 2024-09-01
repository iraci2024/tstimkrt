{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Dataset_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, DB,
  // sgc
  sgcWebSocket_Protocol_sgc_Message, sgcWebSocket_Protocol_dataset_message,
  sgcWebSocket_Protocol_sgc_server, sgcWebSocket_HTTPResponse,
  sgcWebSocket_Classes, sgcWebSocket_Types, sgcJSON;

type


  TsgcWSProtocol_dataset_server = class(TsgcWSProtocol_sgc_server)
    { from TsgcWSProtocol_sgc }
  protected
    FWSMessage: TsgcWSMessage;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessage; override;
  protected
    procedure DoProcessMessage(aConnection: TsgcWSConnection; const aText: String);
        override;
    { from TsgcWSProtocol_sgc }

    { TsgcWSMessageDataset }
  private
    FWSMessageDataset: TsgcWSMessageDataset;
    function GetWSMessageDataset: TsgcWSMessageDataset;
  protected
    property WSMessageDataset: TsgcWSMessageDataset read GetWSMessageDataset write
        FWSMessageDataset;
    { TsgcWSMessageDataset }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection; Data:
        TMemoryStream); override;
    { from TsgcWSComponent }

    { methods }
  private
    procedure DoAutoSynchronize(aConnection: TsgcWSConnection);
  protected
    procedure DoSynchronizeStart(aConnection: TsgcWSConnection); virtual;
    procedure DoSynchronizeEnd(aConnection: TsgcWSConnection); virtual;
    procedure DoSynchronize(aConnection: TsgcWSConnection); virtual;
    procedure DoMetaData(aConnection: TsgcWSConnection); virtual;
  public
    procedure MetaData(aConnection: TsgcWSConnection);
    procedure Synchronize(aConnection: TsgcWSConnection);
  public
    procedure BroadcastRecord;
    { methods}

    { internal events }
  private
    FDSAfterPost: TDataSetNotifyEvent;
    FDSBeforeDelete: TDataSetNotifyEvent;
    FDSAfterRefresh: TDataSetNotifyEvent;
  protected
    procedure OnUpdateRecordEvent(Sender: TDataSet); virtual;
    procedure OnDeleteRecordEvent(Sender: TDataSet); virtual;
    procedure OnAfterRefreshEvent(Sender: TDataSet); virtual;
    { internal events }

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
    procedure OnBeforeNewRecordEvent(aConnection: TsgcWSConnection; const DataSet:
        TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
    procedure OnBeforeUpdateRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
    procedure OnBeforeDeleteRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
    procedure OnAfterNewRecordEvent(aConnection: TsgcWSConnection; const DataSet:
        TDataset);
    procedure OnAfterUpdateRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
    procedure OnAfterDeleteRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
  private
    procedure OnBeforeDatasetUpdateEvent(aConnection: TsgcWSConnection; const JSON:
        IsgcObjectJSON; var Handled: Boolean);
  protected
    procedure DoBeforeNewRecordEvent(aConnection: TsgcWSConnection; DataSet:
        TDataset; JSON: IsgcObjectJSON; var Handled: Boolean); virtual;
    procedure DoBeforeUpdateRecordEvent(aConnection: TsgcWSConnection; DataSet:
        TDataset; JSON: IsgcObjectJSON; var Handled: Boolean); virtual;
    procedure DoBeforeDeleteRecordEvent(aConnection: TsgcWSConnection; DataSet:
        TDataset; JSON: IsgcObjectJSON; var Handled: Boolean); virtual;
    procedure DoAfterNewRecordEvent(aConnection: TsgcWSConnection; DataSet:
        TDataset); virtual;
    procedure DoAfterUpdateRecordEvent(aConnection: TsgcWSConnection; DataSet:
        TDataset); virtual;
    procedure DoAfterDeleteRecordEvent(aConnection: TsgcWSConnection; DataSet:
        TDataset); virtual;
  protected
    procedure DoBeforeDatasetUpdateEvent(aConnection: TsgcWSConnection; JSON:
        IsgcObjectJSON; var Handled: Boolean);
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
  { events }


    { from sgcWebSocket_Protocol_sgc }
  protected
    procedure DoBroadCast(aMessage: TsgcWSMessage; const aChannel: String; const
        Exclude: String = ''; const Include: String = ''); override;
    { from sgcWebSocket_Protocol_sgc }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { DataSet }
  private
    FDataSet: TDataSet;
  protected
    procedure SetDataSet(const Value: TDataSet);
  public
    property DataSet: TDataSet read FDataSet write SetDataSet;
    { DataSet }

    { properties }
  private
    FNotifyUpdates: Boolean;
    FNotifyDeletes: Boolean;
    FApplyUpdates: Boolean;
    FAutoEscapeText: Boolean;
    FAutoSynchronize: Boolean;
    FEncodeBase64: Boolean;
    FFormatSettings: TsgcWSDatasetFormatSettings;
    FUpdateMode: TwsDatasetUpdateMode;
    procedure SetFormatSettings(const Value: TsgcWSDatasetFormatSettings);
  public
    property AutoSynchronize: Boolean read FAutoSynchronize write FAutoSynchronize;
    property NotifyUpdates: Boolean read FNotifyUpdates write FNotifyUpdates;
    property NotifyDeletes: Boolean read FNotifyDeletes write FNotifyDeletes;
    property ApplyUpdates: Boolean read FApplyUpdates write FApplyUpdates;
    property AutoEscapeText: Boolean read FAutoEscapeText write FAutoEscapeText;
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
    property FormatSettings: TsgcWSDatasetFormatSettings read FFormatSettings write
        SetFormatSettings;
    property UpdateMode: TwsDatasetUpdateMode read FUpdateMode write FUpdateMode;
    { properties }

  end;



  TsgcWSProtocol_JS_dataset = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;


  TsgcWSProtocol_HTML_dataset = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses sgcWebSocket_Helpers, sgcWebSocket_Const, sgcWebSocket_Resources;

function TsgcWSProtocol_JS_dataset.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_DATASET_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_dataset.GetFileName: string;
begin
  Result := CS_JS_DATASET_ESEGECE_COM;
end;

constructor TsgcWSProtocol_dataset_server.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_DATASET;
  FFormatSettings := TsgcWSDataSetFormatSettings.Create;
  AutoSynchronize := True;
  NotifyUpdates := True;
  NotifyDeletes := True;
  ApplyUpdates := True;
  UpdateMode := upWhereAll;
  AutoEscapeText := False;
  EncodeBase64 := False;
end;

destructor TsgcWSProtocol_dataset_server.Destroy;
begin
  FDataSet := nil;
  sgcFree(FFormatSettings);
  inherited;
end;

procedure TsgcWSProtocol_dataset_server.BroadcastRecord;
begin
  DoBroadCast(WSMessageDataSet, CS_SGC_DATASET_UPDATE)
end;

procedure TsgcWSProtocol_dataset_server.DoAfterDeleteRecordEvent(aConnection:
    TsgcWSConnection; DataSet: TDataset);
begin
  if Assigned(FOnAfterDeleteRecord) then
    FOnAfterDeleteRecord(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_server.DoAfterNewRecordEvent(aConnection:
    TsgcWSConnection; DataSet: TDataset);
begin
  if Assigned(FOnAfterNewRecord) then
    FOnAfterNewRecord(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_server.DoAfterUpdateRecordEvent(aConnection:
    TsgcWSConnection; DataSet: TDataset);
begin
  if Assigned(FOnAfterUpdateRecord) then
    FOnAfterUpdateRecord(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_server.DoAutoSynchronize(aConnection:
    TsgcWSConnection);
begin
  if AutoSynchronize then
  begin
    DoMetaData(aConnection);
    DoSynchronize(aConnection);
  end;
end;

procedure TsgcWSProtocol_dataset_server.DoBeforeDeleteRecordEvent(aConnection:
    TsgcWSConnection; DataSet: TDataset; JSON: IsgcObjectJSON; var Handled:
    Boolean);
begin
  if Assigned(FOnBeforeDeleteRecord) then
    FOnBeforeDeleteRecord(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.DoBeforeNewRecordEvent(aConnection:
    TsgcWSConnection; DataSet: TDataset; JSON: IsgcObjectJSON; var Handled:
    Boolean);
begin
  if Assigned(FOnBeforeNewRecord) then
    FOnBeforeNewRecord(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.DoBeforeUpdateRecordEvent(aConnection:
    TsgcWSConnection; DataSet: TDataset; JSON: IsgcObjectJSON; var Handled:
    Boolean);
begin
  if Assigned(FOnBeforeUpdateRecord) then
    FOnBeforeUpdateRecord(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.DoBroadCast(aMessage: TsgcWSMessage;
    const aChannel: String; const Exclude: String = ''; const Include: String =
    '');
begin
  WSMessageDataSet.DoEnterWrite;

  WSMessageDataSet.Method := CS_SGC_DATASET;
  WSMessageDataSet.Channel := aChannel;
  WSMessageDataSet.Guid := Guid;
  WSMessageDataSet.Text := '';
  inherited;
end;

procedure TsgcWSProtocol_dataset_server.DoBeforeDatasetUpdateEvent(aConnection:
    TsgcWSConnection; JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeDatasetUpdate) then
    FOnBeforeDatasetUpdate(aConnection, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.DoEventBinary(const aConnection:
    TsgcWSConnection; Data: TMemoryStream);
begin
  inherited;
end;

procedure TsgcWSProtocol_dataset_server.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  // ... initialize
  DoInitialize(aConnection);
  // ... notify connection only if not Assigned Broker
  if not Assigned(Broker) then
    inherited;
end;

procedure TsgcWSProtocol_dataset_server.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  DoAutoSynchronize(aConnection);
end;

procedure TsgcWSProtocol_dataset_server.DoMetaData(aConnection:
    TsgcWSConnection);
begin
  if DataSet.Active then
  begin
    WSMessageDataset.DoEnterWrite;
    Try
      WSMessageDataset.method := CS_SGC_DATASET_METADATA;
      WSMessageDataSet.Channel := '';
      DoWriteMessageText(aConnection, WSMessageDataset.Write);
    Finally
      WSMessageDataset.DoLeaveWrite;
    End;
  end;
end;

procedure TsgcWSProtocol_dataset_server.DoProcessMessage(aConnection:
    TsgcWSConnection; const aText: String);
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoProcessMessage', '[Text]: ' + aText);
  {$ENDIF}

  if ApplyUpdates then
    WSMessageDataSet.Read(aConnection, aText);

  if FWSMessage.method = CS_SGC_DATASET_SYNCHRONIZE then
    DoSynchronize(aConnection)
  else if FWSMessage.method = CS_SGC_DATASET_METADATA then
    DoMetaData(aConnection)
  else
    inherited;
end;

procedure TsgcWSProtocol_dataset_server.DoSynchronize(aConnection:
    TsgcWSConnection);
begin
  DoSynchronizeStart(aConnection);
  Try
    DataSet.First;
    while not DataSet.EOF do
    begin
      WSMessageDataset.DoEnterWrite;
      Try
        WSMessageDataset.method := CS_SGC_DATASET;
        WSMessageDataSet.Channel := CS_SGC_DATASET_UPDATE; // ... maybe already exists
        DoWriteMessageText(aConnection, WSMessageDataset.Write);
      Finally
        WSMessageDataset.DoLeaveWrite;
      End;
      DataSet.Next;
    end;
  Finally
    DoSynchronizeEnd(aConnection);
  End;
end;

procedure TsgcWSProtocol_dataset_server.DoSynchronizeEnd(aConnection:
    TsgcWSConnection);
begin
  WSMessageDataset.DoEnterWrite;
  Try
    WSMessageDataset.method := CS_SGC_DATASET_SYNCHRONIZE_END;
    WSMessageDataset.Result := CS_SGC_DATASET_SYNCHRONIZE_START;
    WSMessageDataSet.Channel := '';
    DoWriteMessageText(aConnection, WSMessageDataset.Write);
  Finally
    WSMessageDataset.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_dataset_server.DoSynchronizeStart(aConnection:
    TsgcWSConnection);
begin
  WSMessageDataset.DoEnterWrite;
  Try
    WSMessageDataset.method := CS_SGC_DATASET_SYNCHRONIZE_START;
    WSMessageDataSet.Channel := '';
    DoWriteMessageText(aConnection, WSMessageDataset.Write);
  Finally
    WSMessageDataset.DoLeaveWrite;
  End;
end;

function TsgcWSProtocol_dataset_server.GetWSMessageByConnection(const
    aConnection: TsgcWSConnection): TsgcWSMessage;
begin
  result := inherited GetWSMessageByConnection(aConnection);

  FWSMessage := result;
end;

function TsgcWSProtocol_dataset_server.GetWSMessageDataset:
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

    FWSMessageDataset.UpdateMode := UpdateMode;
    FWSMessageDataSet.AutoEscapeText := AutoEscapeText;
    FWSMessageDataSet.EncodeBase64 := EncodeBase64;

    FWSMessageDataSet.FormatSettings.Assign(FormatSettings);
  end;
  Result := FWSMessageDataset;
end;

procedure TsgcWSProtocol_dataset_server.MetaData(aConnection: TsgcWSConnection);
begin
  DoMetaData(aConnection);
end;

procedure TsgcWSProtocol_dataset_server.OnAfterDeleteRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterDeleteRecordEvent(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_server.OnAfterNewRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterNewRecordEvent(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_server.OnAfterRefreshEvent(Sender: TDataSet);
begin
  if Assigned(FDSAfterRefresh) then
    FDSAfterRefresh(Sender);

  if NotifyUpdates then
  begin
    if UpdateMode = upRefreshAll then
      DoBroadCast(WSMessageDataSet, CS_SGC_DATASET_UPDATE);
  end;
end;

procedure TsgcWSProtocol_dataset_server.OnAfterUpdateRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterUpdateRecordEvent(aConnection, DataSet);
end;

procedure TsgcWSProtocol_dataset_server.OnBeforeDeleteRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset; const JSON: IsgcObjectJSON; var
    Handled: Boolean);
begin
  DoBeforeDeleteRecordEvent(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.OnBeforeNewRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset; const JSON: IsgcObjectJSON; var
    Handled: Boolean);
begin
  DoBeforeNewRecordEvent(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.OnBeforeUpdateRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset; const JSON: IsgcObjectJSON; var
    Handled: Boolean);
begin
  DoBeforeUpdateRecordEvent(aConnection, DataSet, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.OnBeforeDatasetUpdateEvent(aConnection:
    TsgcWSConnection; const JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  DoBeforeDatasetUpdateEvent(aConnection, JSON, Handled);
end;

procedure TsgcWSProtocol_dataset_server.OnUpdateRecordEvent(Sender: TDataSet);
begin
  if Assigned(FDSAfterPost) then
    FDSAfterPost(Sender);

  if NotifyUpdates then
    DoBroadCast(WSMessageDataSet, CS_SGC_DATASET_UPDATE);
end;

procedure TsgcWSProtocol_dataset_server.OnDeleteRecordEvent(Sender: TDataSet);
begin
  if Assigned(FDSBeforeDelete) then
    FDSBeforeDelete(Sender);

  if NotifyDeletes then
    DoBroadCast(WSMessageDataSet, CS_SGC_DATASET_DELETE);
end;

procedure TsgcWSProtocol_dataset_server.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    FDSAfterPost := nil;
    FDSBeforeDelete := nil;
    FDSAfterRefresh := nil;
    if Assigned(FDataSet) then
    begin
      FDSAfterPost := FDataSet.AfterPost;
      FDSBeforeDelete := FDataSet.BeforeDelete;
      FDSAfterRefresh := FDataSet.AfterRefresh;
      FDataSet.AfterPost := OnUpdateRecordEvent;
      FDataSet.BeforeDelete := OnDeleteRecordEvent;
      FDataSet.AfterRefresh := OnAfterRefreshEvent;
    end;

    WSMessageDataset.DataSet := Value;
  end;
end;

procedure TsgcWSProtocol_dataset_server.SetFormatSettings(const Value:
    TsgcWSDatasetFormatSettings);
begin
  if Assigned(FFormatSettings) then
    FFormatSettings.Assign(Value);
end;

procedure TsgcWSProtocol_dataset_server.Synchronize(aConnection:
    TsgcWSConnection);
begin
  DoSynchronize(aConnection);
end;

class function TsgcWSProtocol_HTML_dataset.GetFileName: string;
begin
  Result := CS_HTML_DATASET_ESEGECE_COM;
end;

function TsgcWSProtocol_HTML_dataset.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_HTML_DATASET_ESEGECE_COM);
end;

initialization
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_dataset);
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_dataset);

{$ENDIF}

end.
