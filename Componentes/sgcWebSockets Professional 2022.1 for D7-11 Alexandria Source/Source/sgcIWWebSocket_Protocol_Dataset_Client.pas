{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIWWebSocket_Protocol_Dataset_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

uses
  Classes,
  sgcIWWebSocket_Protocol_Base, DB,
  sgcWebSocket_Protocol_Dataset_Message, 
  sgcIWWebSocket_Protocol_sgc_Client, sgcWebSocket_Classes;

type
  TsgcIWWSProtocol_Dataset_Client = class(TsgcIWWSProtocol_sgc_client)

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
    procedure DoNotifyMessage(const aMessage: String); override;
  { from TsgcWSComponent }

  public
    constructor Create(aOwner: TComponent); override;

  { subscription }
  public
    procedure Subscribe_New;
    procedure Subscribe_Update;
    procedure Subscript_Delete;
  { subscription }

  { dataset }
  private
    FDataSet: TDataSet;
  protected
    procedure SetDataSet(const Value: TDataSet);
  public
    property DataSet: TDataSet read FDataSet write SetDataSet;

  { events }
  private
    FOnAfterDeleteRecord: TsgcWSAfterRecordEvent;
    FOnAfterNewRecord: TsgcWSAfterRecordEvent;
    FOnAfterUpdateRecord: TsgcWSAfterRecordEvent;
  private
    procedure OnAfterNewRecordEvent(aConnection: TsgcWSConnection; const DataSet:
        TDataset);
    procedure OnAfterUpdateRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
    procedure OnAfterDeleteRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
  protected
    procedure DoAfterNewRecordEvent(aConnection: TsgcWSConnection; const DataSet:
        TDataset);
    procedure DoAfterUpdateRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
    procedure DoAfterDeleteRecordEvent(aConnection: TsgcWSConnection; const
        DataSet: TDataset);
  published
    property OnAfterNewRecord: TsgcWSAfterRecordEvent read FOnAfterNewRecord write
        FOnAfterNewRecord;
    property OnAfterUpdateRecord: TsgcWSAfterRecordEvent read FOnAfterUpdateRecord
        write FOnAfterUpdateRecord;
    property OnAfterDeleteRecord: TsgcWSAfterRecordEvent read FOnAfterDeleteRecord
        write FOnAfterDeleteRecord;
  { events }
  end;

{$ENDIF}

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

uses sgcWebSocket_Const;

constructor TsgcIWWSProtocol_Dataset_Client.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_DATASET;
end;

procedure TsgcIWWSProtocol_Dataset_Client.DoAfterDeleteRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  if Assigned(FOnAfterDeleteRecord) then FOnAfterDeleteRecord(aConnection, DataSet);
end;

procedure TsgcIWWSProtocol_Dataset_Client.DoAfterNewRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  if Assigned(FOnAfterNewRecord) then FOnAfterNewRecord(aConnection, DataSet);
end;

procedure TsgcIWWSProtocol_Dataset_Client.DoAfterUpdateRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  if Assigned(FOnAfterUpdateRecord) then FOnAfterUpdateRecord(aConnection, DataSet);
end;

procedure TsgcIWWSProtocol_Dataset_Client.DoNotifyMessage(const aMessage:
    String);
begin
  WSMessage.Read(aMessage);
  if WSMessage.Method = CS_DATASET then
    WSMessageDataset.Read(aMessage)
  else
    inherited;
end;

function TsgcIWWSProtocol_Dataset_Client.GetWSMessageDataset:
    TsgcWSMessageDataset;
begin
  if not Assigned(FWSMessageDataset) then
  begin
    FWSMessageDataset := TsgcWSMessageDataset.Create(self);
    FWSMessageDataset.OnAfterNewRecord := OnAfterNewRecordEvent;
    FWSMessageDataset.OnAfterUpdateRecord := OnAfterUpdateRecordEvent;
    FWSMessageDataset.OnAfterDeleteRecord := OnAfterDeleteRecordEvent;
  end;
  Result := FWSMessageDataset;
end;

procedure TsgcIWWSProtocol_Dataset_Client.OnAfterDeleteRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterDeleteRecordEvent(aConnection, DataSet);
end;

procedure TsgcIWWSProtocol_Dataset_Client.OnAfterNewRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterNewRecordEvent(aConnection, DataSet);
end;

procedure TsgcIWWSProtocol_Dataset_Client.OnAfterUpdateRecordEvent(aConnection:
    TsgcWSConnection; const DataSet: TDataset);
begin
  DoAfterUpdateRecordEvent(aConnection, DataSet);
end;

procedure TsgcIWWSProtocol_Dataset_Client.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    WSMessageDataSet.DataSet := Value;
  end;
end;

procedure TsgcIWWSProtocol_Dataset_Client.Subscribe_New;
begin
  Subscribe(CS_SGC_DATASET_NEW);
end;

procedure TsgcIWWSProtocol_Dataset_Client.Subscribe_Update;
begin
  Subscribe(CS_SGC_DATASET_UPDATE);
end;

procedure TsgcIWWSProtocol_Dataset_Client.Subscript_Delete;
begin
  Subscribe(CS_SGC_DATASET_DELETE);
end;

{$ENDIF}

{$ENDIF}

end.
