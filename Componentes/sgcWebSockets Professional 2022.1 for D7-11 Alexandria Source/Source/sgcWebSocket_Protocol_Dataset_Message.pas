{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Dataset_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, DB, Variants, TypInfo, StrUtils,
  // sgc
  sgcJSON, sgcWebSocket_Protocol_sgc_message, sgcWebSocket_Types,
  sgcWebSocket_Classes;

type

  TsgcFields = array of Variant;

  TsgcWSMetaDataEvent = procedure(Connection: TsgcWSConnection; const JSON:
      IsgcObjectJSON) of object;
  TsgcWSBeforeRecordEvent = procedure(Connection: TsgcWSConnection; const
      Dataset: TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean) of
      object;
  TsgcWSAfterRecordEvent = procedure(Connection: TsgcWSConnection; const Dataset:
      TDataset) of object;
  TsgcWSBeforeDatasetUpdateEvent = procedure(Connection: TsgcWSConnection; const
      JSON: IsgcObjectJSON; var Handled: Boolean) of object;
  TsgcWSSynchronizeEvent = procedure(Connection: TsgcWSConnection; const JSON:
      IsgcObjectJSON) of object;


  TsgcWSDatasetFormatSettings = class(TPersistent)
  private
    FDateSeparator: Char;
    FDecimalSeparator: Char;
    FShortDateFormat: String;
    FThousandSeparator: Char;
    FTimeSeparator: Char;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property DateSeparator: Char read FDateSeparator write FDateSeparator;
    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
    property ShortDateFormat: String read FShortDateFormat write FShortDateFormat;
    property ThousandSeparator: Char read FThousandSeparator write
        FThousandSeparator;
    property TimeSeparator: Char read FTimeSeparator write FTimeSeparator;
  end;

  TsgcWSMessageDataset = class(TsgcWSMessage)
    { helpers }
  private
    function Escape(const aText: String): String;
    function UnEscape(const aText: String): String;
  private
    function GetBlobAsString(aField: TField): String;
    { helpers }

    { delta changes }
  private
    FDSBeforeInsertEvent: TDataSetNotifyEvent;
    FDSBeforeEditEvent: TDataSetNotifyEvent;
    FDSAfterPostEvent: TDataSetNotifyEvent;
  private
    FFieldsValue: TsgcFields;
    FDeltaFields: TStringList;
  protected
    procedure OnBeforeInsertEvent(DataSet: TDataSet);
    procedure OnBeforeEditEvent(DataSet: TDataSet);
    procedure OnAfterPostEvent(DataSet: TDataSet);
  protected
    function GetDeltaFields: TStringList;
    { delta changes }

    { dataset }
  private
    FDataSet: TDataSet;
  protected
    procedure SetDataSet(const Value: TDataSet); virtual;
    function GetDatasetName: String;
  public
    property DataSet: TDataSet read FDataSet write SetDataSet;
    { dataset }

    { format settings }
  private
    FFS: TFormatSettings;
    FFormatSettings: TsgcWSDatasetFormatSettings;
    function GetFormatSettings: TsgcWSDatasetFormatSettings;
    procedure SetFormatSettings(const Value: TsgcWSDatasetFormatSettings);
  protected
    function GetFS: TFormatSettings;
  public
    property FormatSettings: TsgcWSDatasetFormatSettings read GetFormatSettings
        write SetFormatSettings;
    { format settings }

  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoJSONRecord; virtual;
  protected
    function FindRecord: Boolean; virtual;
    procedure DoAssignRecord; virtual;
    procedure DoDeleteRecord; virtual;
    procedure DoNewRecord; virtual;
    procedure DoUpdateRecord; virtual;
    procedure DoRefreshRecord; virtual;
  protected
    function DoBeforeDatasetUpdate: Boolean; virtual;
  protected
    function DatasetNodeExists(const aFieldName: String): Boolean;
    function GetJSONValue(const aFieldName: String): Variant;
  public
  public
    procedure Read(aConnection: TsgcWSConnection; const aMessage: String);
        reintroduce; overload;
    function Write: string; override;

    { key fields }
  private
    FKeyFields: TFields;
    procedure DoFreeKeyFields;
    procedure DoAddKeyField(aField: TField);
    function GetKeyFields: TFields;
  protected
    procedure DoKeyFields;
    function GetKeyFieldsAsString: string;
    function GetKeyValuesAsVariant: Variant;
  public
    property KeyFields: TFields read GetKeyFields write FKeyFields;
    { key fields }

    { synchronize }
  protected
    procedure DoReadBeforeSynchronize; virtual;
    procedure DoReadAfterSynchronize; virtual;
    procedure DoWriteBeforeSynchronize; virtual;
    procedure DoWriteAfterSynchronize; virtual;
    { synchronize }


    { metadata }
  protected
    procedure DoWriteMetaData; virtual;
    procedure DoReadMetaData; virtual;
    { metadata }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { properties }
  private
    FUpdateMode: TwsDatasetUpdateMode;
    FAutoEscapeText: Boolean;
    FEncodeBase64: Boolean;
  public
    property UpdateMode: TwsDatasetUpdateMode read FUpdateMode write FUpdateMode;
    property AutoEscapeText: Boolean read FAutoEscapeText write FAutoEscapeText;
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
    { properties }

    { events }
  private
    FOnBeforeNewRecord: TsgcWSBeforeRecordEvent;
    FOnBeforeDeleteRecord: TsgcWSBeforeRecordEvent;
    FOnBeforeUpdateRecord: TsgcWSBeforeRecordEvent;
    FOnAfterDeleteRecord: TsgcWSAfterRecordEvent;
    FOnAfterNewRecord: TsgcWSAfterRecordEvent;
    FOnAfterUpdateRecord: TsgcWSAfterRecordEvent;
  private
    FOnMetaData: TsgcWSMetaDataEvent;
  private
    FOnBeforeDatasetUpdate: TsgcWSBeforeDatasetUpdateEvent;
  private
    FOnBeforeSynchronize: TsgcWSSynchronizeEvent;
    FOnAfterSynchronize: TsgcWSSynchronizeEvent;
  protected
    procedure DoBeforeNewRecordEvent(const Dataset: TDataset; const JSON:
        IsgcObjectJSON; var Handled: Boolean); virtual;
    procedure DoBeforeUpdateRecordEvent(const Dataset: TDataset; const JSON:
        IsgcObjectJSON; var Handled: Boolean); virtual;
    procedure DoBeforeDeleteRecordEvent(const Dataset: TDataset; const JSON:
        IsgcObjectJSON; var Handled: Boolean); virtual;
    procedure DoAfterNewRecordEvent; virtual;
    procedure DoAfterUpdateRecordEvent; virtual;
    procedure DoAfterDeleteRecordEvent; virtual;
  protected
    procedure DoMetaDataEvent(const JSON: IsgcObjectJSON); virtual;
  protected
    procedure DoBeforeSynchronizeEvent(const JSON: IsgcObjectJSON); virtual;
    procedure DoAfterSynchronizeEvent(const JSON: IsgcObjectJSON); virtual;
  public
    property OnBeforeNewRecord: TsgcWSBeforeRecordEvent read FOnBeforeNewRecord write
        FOnBeforeNewRecord;
    property OnBeforeUpdateRecord: TsgcWSBeforeRecordEvent read FOnBeforeUpdateRecord
        write FOnBeforeUpdateRecord;
    property OnBeforeDeleteRecord: TsgcWSBeforeRecordEvent read FOnBeforeDeleteRecord
        write FOnBeforeDeleteRecord;
    property OnAfterDeleteRecord: TsgcWSAfterRecordEvent read FOnAfterDeleteRecord
        write FOnAfterDeleteRecord;
    property OnAfterNewRecord: TsgcWSAfterRecordEvent read FOnAfterNewRecord write
        FOnAfterNewRecord;
    property OnAfterUpdateRecord: TsgcWSAfterRecordEvent read FOnAfterUpdateRecord
        write FOnAfterUpdateRecord;
  public
    property OnMetaData: TsgcWSMetaDataEvent read FOnMetaData write FOnMetaData;
  public
    property OnBeforeSynchronize: TsgcWSSynchronizeEvent read FOnBeforeSynchronize
        write FOnBeforeSynchronize;
    property OnAfterSynchronize: TsgcWSSynchronizeEvent read FOnAfterSynchronize
        write FOnAfterSynchronize;
  public
    property OnBeforeDatasetUpdate: TsgcWSBeforeDatasetUpdateEvent read
        FOnBeforeDatasetUpdate write FOnBeforeDatasetUpdate;
    { events }

  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcBase_Helpers, sgcWebSocket_Helpers;

constructor TsgcWSMessageDataset.Create(aOwner: TComponent);
begin
  inherited;
  AutoEscapeText := False;
  EncodeBase64 := False;
end;

destructor TsgcWSMessageDataset.Destroy;
begin
  FDataSet := nil;
  FFieldsValue := nil;
  DoFreeKeyFields;
  sgcFree(FFormatSettings);
  sgcFree(FDeltaFields);
  inherited;
end;

procedure TsgcWSMessageDataset.DoAddKeyField(aField: TField);
var
  oField: TField;
begin
  oField := TField.Create(self);
  oField.FieldName := aField.FieldName;

  KeyFields.Add(oField);
end;

procedure TsgcWSMessageDataset.DoAfterDeleteRecordEvent;
begin
  if Assigned(FOnAfterDeleteRecord) then
    FOnAfterDeleteRecord(FWSConnection, DataSet);
end;

procedure TsgcWSMessageDataset.DoAfterNewRecordEvent;
begin
  if Assigned(FOnAfterNewRecord) then
    FOnAfterNewRecord(FWSConnection, DataSet);
end;

procedure TsgcWSMessageDataset.DoAfterUpdateRecordEvent;
begin
  if Assigned(FOnAfterUpdateRecord) then
    FOnAfterUpdateRecord(FWSConnection, DataSet);
end;

procedure TsgcWSMessageDataset.DoAssignRecord;
var
  i: Integer;
  vDateTime: string;
  vValue: Variant;
  vText: string;
  oStringStream: TsgcStringStream;
  oStream: TMemoryStream;
  vBase64: Boolean;
  vNumber: Double;
begin
  vBase64 := DoReadJSONValue(CS_DATASET_ENCODE_BASE64, JSON.Node[CS_RESULT]);

  // ... apply updates
  for i := 0 to DataSet.Fields.count - 1 do
  begin
    if DatasetNodeExists(DataSet.Fields[i].FieldName) then
    begin
      case DataSet.Fields[i].DataType of
        ftDateTime, ftDate, ftTimeStamp:
          begin
            vDateTime := GetJSONValue(DataSet.Fields[i].FieldName);
            if vDateTime <> '' then
            begin
              if vDateTime = '0' then
                DataSet.Fields[i].Clear
              else
              begin
                case UpdateMode of
                  upWhereAll:
                    DataSet.Fields[i].AsDateTime := StrToDateTime(vDateTime, GetFS);
                  upWhereChanged:
                    begin
                      if DataSet.Fields[i].AsDateTime <> StrToDateTime(vDateTime) then
                        DataSet.Fields[i].AsDateTime := StrToDateTime(vDateTime, GetFS);
                    end;
                end;
              end;
            end;
          end;
        ftString, ftWideString, ftMemo{$IFDEF D2007}, ftWideMemo{$ENDIF}:
          begin
            vText := GetJSONValue(DataSet.Fields[i].FieldName);
            if vBase64 then
              vText := sgcBase_Helpers.DecodeBase64(vText);
            if AutoEscapeText then
              vText := UnEscape(vText);

            case UpdateMode of
              upWhereAll:
                DataSet.Fields[i].AsString := vText;
              upWhereChanged:
                begin
                  if DataSet.Fields[i].AsString <> vText then
                    DataSet.Fields[i].AsString := vText;
                end;
            end;
          end;
        ftBlob:
          begin
            case UpdateMode of
              upWhereAll:
                begin
                  oStringStream := TsgcStringStream.Create(String(GetJSONValue(DataSet.Fields[i].FieldName)));
                  Try
                    TBlobField(DataSet.Fields[i]).LoadFromStream(oStringStream);
                  Finally
                    sgcFree(oStringStream);
                  End;
                end;
              upWhereChanged:
                begin
                  oStream := TMemoryStream.Create;
                  Try
                    TBlobField(DataSet.Fields[i]).SaveToStream(oStream);
                    vText := sgcBase_Helpers.EncodeBase64(oStream);
                    if vText <> GetJSONValue(DataSet.Fields[i].FieldName) then
                    begin
                      oStringStream := TsgcStringStream.Create(String(GetJSONValue(DataSet.Fields[i].FieldName)));
                      Try
                        TBlobField(DataSet.Fields[i]).LoadFromStream(oStringStream);
                      Finally
                        sgcFree(oStringStream);
                      End;
                    end;
                  Finally
                    sgcFree(oStream);
                  End;
                end;
            end;
          end;
        ftFloat, ftCurrency:
          begin
            if sgcContainsText(String(GetJSONValue(DataSet.Fields[i].FieldName)), ',') then
              vNumber := StrToFloat(String(GetJSONValue(DataSet.Fields[i].FieldName)), GetFS)
            else if sgcContainsText(String(GetJSONValue(DataSet.Fields[i].FieldName)), '.') then
              vNumber := StrToFloat(String(GetJSONValue(DataSet.Fields[i].FieldName)), GetFS)
            else
              vNumber := GetJSONValue(DataSet.Fields[i].FieldName);
            case UpdateMode of
              upWhereAll:
                DataSet.Fields[i].Value := vNumber;
              upWhereChanged:
                begin
                  if DataSet.Fields[i].Value <> vNumber then
                    DataSet.Fields[i].Value := vNumber;
                end;
            end;
          end
        else
        begin
          case UpdateMode of
            upWhereAll:
              DataSet.Fields[i].Value := GetJSONValue(DataSet.Fields[i].FieldName);
            upWhereChanged:
              begin
                vValue := GetJSONValue(DataSet.Fields[i].FieldName);
                if DataSet.Fields[i].Value <> vValue then
                  DataSet.Fields[i].Value := vValue;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TsgcWSMessageDataset.DoBeforeDeleteRecordEvent(const Dataset:
    TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeDeleteRecord) then
    FOnBeforeDeleteRecord(FWSConnection, Dataset, JSON, Handled);
end;

procedure TsgcWSMessageDataset.DoBeforeNewRecordEvent(const Dataset: TDataset;
    const JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeNewRecord) then
    FOnBeforeNewRecord(FWSConnection, Dataset, JSON, Handled);
end;

procedure TsgcWSMessageDataset.DoBeforeUpdateRecordEvent(const Dataset:
    TDataset; const JSON: IsgcObjectJSON; var Handled: Boolean);
begin
  if Assigned(FOnBeforeUpdateRecord) then
    FOnBeforeUpdateRecord(FWSConnection, Dataset, JSON, Handled);
end;

procedure TsgcWSMessageDataset.DoDeleteRecord;
var
  vHandled: Boolean;
begin
  vHandled := False;
  DoBeforeDeleteRecordEvent(DataSet, JSON.Node[CS_RESULT].Node[CS_DATASET], vHandled);
  if not vHandled then
  begin
    if UpdateMode = upRefreshAll then
      DoRefreshRecord
    else if FindRecord then
      DataSet.Delete;
    DoAfterDeleteRecordEvent;
  end;
end;

procedure TsgcWSMessageDataset.DoJSONRecord;
var
  i: Integer;
  oJSONObject: IsgcObjectJSON;
  vWrite: Boolean;
  vText: String;
begin
  id := ''; // clear id

  JSONResult.Clear;
  JSONResult.AddPair(CS_METHOD, CS_SGC_DATASET);
  JSONResult.AddPair(CS_CHANNEL, Channel);
  if Channel = CS_SGC_DATASET_UPDATE then
    JSONResult.AddPair(CS_UPDATE_MODE, TwsDatasetUpdateMode_String[Ord(UpdateMode)]);
  JSONResult.AddPair(CS_DATASET_NAME, Dataset.Name);
  JSONResult.AddPair(CS_DATASET_RECNO, Dataset.RecNo);
  JSONResult.AddPair(CS_DATASET_ENCODE_BASE64, EncodeBase64);
  oJSONObject := JSONResult.AddObject(CS_DATASET);

  for i := 0 to DataSet.Fields.count - 1 do
  begin
    vWrite := True;
    if (UpdateMode = upWhereChanged) and (Channel = CS_SGC_DATASET_UPDATE) and (GetDeltaFields.Count > 0) then
      vWrite := GetDeltaFields.IndexOf(DataSet.Fields[i].FieldName) > -1;
    if vWrite then
    begin
      case DataSet.Fields[i].DataType of
        ftInteger, ftSmallint, ftWord, ftAutoInc, ftLargeInt:
          DoWriteJSONValue(DataSet.Fields[i].FieldName,
            DataSet.Fields[i].AsInteger, oJSONObject);
        ftFloat, ftCurrency:
          DoWriteJSONValue(DataSet.Fields[i].FieldName,
            FormatFloat('0.00', DataSet.Fields[i].AsFloat, GetFS), oJSONObject);
        ftBoolean:
          DoWriteJSONValue(DataSet.Fields[i].FieldName,
            DataSet.Fields[i].AsBoolean, oJSONObject);
        ftDateTime, ftDate, ftTimeStamp:
          if DataSet.Fields[i].IsNull then
            DoWriteJSONValue(DataSet.Fields[i].FieldName,
              '0', oJSONObject)
          else
          begin
            DoWriteJSONValue(DataSet.Fields[i].FieldName,
              DateTimeToStr(DataSet.Fields[i].AsFloat, GetFS),
              oJSONObject);
          end;
        ftBlob:
          begin
            if DataSet.Fields[i].IsNull then
              DoWriteJSONValue(DataSet.Fields[i].FieldName,
                '', oJSONObject)
            else
              DoWriteJSONValue(DataSet.Fields[i].FieldName,
                GetBlobAsString(DataSet.Fields[i]), oJSONObject);
          end
        else
        begin
          vText := DataSet.Fields[i].AsString;
          if EncodeBase64 then
            vText := sgcBase_Helpers.EncodeBase64(vText);
          if AutoEscapeText then
            vText := Escape(vText);
          DoWriteJSONValue(DataSet.Fields[i].FieldName, vText, oJSONObject);
        end;
      end;
    end;
  end;
end;

procedure TsgcWSMessageDataset.DoKeyFields;
var
  i: Integer;
begin
  KeyFields.Clear;
  if Assigned(DataSet) then
  begin
    for i := 0 to DataSet.Fields.count - 1 do
    begin
      if pfInKey in DataSet.Fields[i].ProviderFlags then
        DoAddKeyField(DataSet.Fields[i]);
    end;
  end;
end;

procedure TsgcWSMessageDataset.DoWriteMetaData;
var
  i: Integer;
  oJSONObject, oJSONField: IsgcObjectJSON;
begin
  JSONResult.Clear;
  JSONResult.AddPair(CS_METHOD, CS_SGC_METADATA);
  JSONResult.AddPair(CS_CHANNEL, Channel);
  JSONResult.AddPair(CS_DATASET_NAME, Dataset.Name);
  oJSONObject := JSONResult.AddObject(CS_METADATA);

  for i := 0 to DataSet.Fields.count - 1 do
  begin
    oJSONField := oJSONObject.JSONObject.AddObject(CS_FIELD + IntToStr(i));
    DoWriteJSONValue(CS_FIELDNAME, DataSet.Fields[i].FieldName, oJSONField);
    DoWriteJSONValue(CS_DATATYPE, GetEnumName(TypeInfo(TFieldType),integer(Dataset.Fields[i].DataType)), oJSONField);
    DoWriteJSONValue(CS_DATASIZE, Dataset.Fields[i].DataSize, oJSONField);
    DoWriteJSONValue(CS_KEYFIELD, pfInKey in Dataset.Fields[i].ProviderFlags, oJSONField);
  end;
end;

procedure TsgcWSMessageDataset.DoNewRecord;
var
  vHandled: Boolean;
begin
  vHandled := False;
  DoBeforeNewRecordEvent(DataSet, JSON.Node[CS_RESULT].Node[CS_DATASET], vHandled);
  if not vHandled then
  begin
    if UpdateMode = upRefreshAll then
      DoRefreshRecord
    else
    begin
      DataSet.Append;
      DoAssignRecord;
      DataSet.Post;
    end;
    DoAfterNewRecordEvent;
  end;
end;

procedure TsgcWSMessageDataset.DoReadMetaData;
begin
  if JSON.Node[CS_RESULT].Node[CS_METADATA] <> nil then
    DoMetaDataEvent(JSON.Node[CS_RESULT].Node[CS_METADATA]);
end;

procedure TsgcWSMessageDataset.DoUpdateRecord;
var
  vHandled: Boolean;
begin
  if FindRecord then
  begin
    vHandled := False;
    DoBeforeUpdateRecordEvent(DataSet, JSON.Node[CS_RESULT].Node[CS_DATASET], vHandled);
    if not vHandled then
    begin
      if UpdateMode = upRefreshAll then
        DoRefreshRecord
      else
      begin
        DataSet.Edit;
        DoAssignRecord;
        DataSet.Post;
      end;
      DoAfterUpdateRecordEvent;
    end;
  end
  else
    DoNewRecord;
end;

function TsgcWSMessageDataset.FindRecord: Boolean;
var
  vValues: Variant;
begin
  vValues := GetKeyValuesAsVariant;
  result := DataSet.Locate(GetKeyFieldsAsString, VarArrayOf(vValues), []);
end;

function TsgcWSMessageDataset.GetDeltaFields: TStringList;
begin
  if not Assigned(FDeltaFields) then
  begin
    FDeltaFields := TStringList.Create;
    FDeltaFields.Duplicates := dupIgnore;
  end;
  Result := FDeltaFields;
end;

function TsgcWSMessageDataset.GetJSONValue(const aFieldName: String): Variant;
begin
  result := VarNull;
  if DatasetNodeExists(aFieldName) then
    result := JSON.Node[CS_RESULT].Node[CS_DATASET].Node[aFieldName].Value
end;

function TsgcWSMessageDataset.GetKeyFields: TFields;
begin
  if not Assigned(FKeyFields) then
  begin
    FKeyFields := TFields.Create(nil);
    DoKeyFields;
  end;
  result := FKeyFields;
end;

function TsgcWSMessageDataset.GetKeyFieldsAsString: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to KeyFields.count - 1 do
  begin
    if result = '' then
      result := KeyFields.Fields[i].FieldName
    else
      result := result + ',' + KeyFields.Fields[i].FieldName;
  end;
end;

function TsgcWSMessageDataset.GetKeyValuesAsVariant: Variant;
var
  i: Integer;
begin
  result := '';
  for i := 0 to KeyFields.count - 1 do
  begin
    if result = '' then
      result := GetJSONValue(KeyFields.Fields[i].FieldName)
    else
      result := result + ',' + GetJSONValue(KeyFields.Fields[i].FieldName);
  end;
end;

function TsgcWSMessageDataset.DatasetNodeExists(const aFieldName: String):
    Boolean;
begin
  result := False;
  if JSON.Node[CS_RESULT] <> nil then
  begin
    if JSON.Node[CS_RESULT].Node[CS_DATASET] <> nil then
    begin
      if JSON.Node[CS_RESULT].Node[CS_DATASET].Node[aFieldName] <> nil then
        result := True;
    end;
  end;
end;

procedure TsgcWSMessageDataset.DoAfterSynchronizeEvent(const JSON:
    IsgcObjectJSON);
begin
  if Assigned(FOnAfterSynchronize) then FOnAfterSynchronize(FWSConnection, JSON);
end;

function TsgcWSMessageDataset.DoBeforeDatasetUpdate: Boolean;
begin
  result := False;
  if Assigned(FOnBeforeDatasetUpdate) then
    FOnBeforeDatasetUpdate(FWSConnection, JSON.Node[CS_RESULT], result);
end;

procedure TsgcWSMessageDataset.DoBeforeSynchronizeEvent(const JSON:
    IsgcObjectJSON);
begin
  if Assigned(FOnBeforeSynchronize) then FOnBeforeSynchronize(FWSConnection, JSON);
end;

procedure TsgcWSMessageDataset.DoFreeKeyFields;
var
  i: Integer;
begin
  if Assigned(FKeyFields) then
  begin
    for i := FKeyFields.Count - 1 Downto 0 do
      FKeyFields.Remove(FKeyFields[i]);
  end;
  sgcFree(FKeyFields);
end;

procedure TsgcWSMessageDataset.DoMetaDataEvent(const JSON: IsgcObjectJSON);
begin
  if Assigned(FOnMetaData) then FOnMetaData(FWSConnection, JSON);
end;

procedure TsgcWSMessageDataset.DoReadAfterSynchronize;
begin
  if JSON.Node[CS_RESULT] <> nil then
    DoAfterSynchronizeEvent(JSON.Node[CS_RESULT]);
end;

procedure TsgcWSMessageDataset.DoReadBeforeSynchronize;
begin
  if JSON.Node[CS_RESULT] <> nil then
    DoBeforeSynchronizeEvent(JSON.Node[CS_RESULT]);
end;

procedure TsgcWSMessageDataset.DoRefreshRecord;
begin
  if Assigned(Dataset) then
  begin
    Dataset.DisableControls;
    Try
      Dataset.Refresh;
    Finally
      Dataset.EnableControls;
    End;
  end;
end;

procedure TsgcWSMessageDataset.DoWriteAfterSynchronize;
begin
  JSONResult.Clear;
  JSONResult.AddPair(CS_METHOD, CS_SGC_DATASET_SYNCHRONIZE_END);
  JSONResult.AddPair(CS_DATASET_NAME, Dataset.Name);
end;

procedure TsgcWSMessageDataset.DoWriteBeforeSynchronize;
begin
  JSONResult.Clear;
  JSONResult.AddPair(CS_METHOD, CS_SGC_DATASET_SYNCHRONIZE_START);
  JSONResult.AddPair(CS_DATASET_NAME, Dataset.Name);
  JSONResult.AddPair(CS_DATASET_RECORDCOUNT, Dataset.RecordCount);
end;

function TsgcWSMessageDataset.Escape(const aText: String): String;
begin
  result := sgcStringReplace(aText, '{', '\{');
  result := sgcStringReplace(result, '}', '\}');
  result := sgcStringReplace(result, '[', '\[');
  result := sgcStringReplace(result, ']', '\]');
end;

function TsgcWSMessageDataset.GetBlobAsString(aField: TField): String;
var
  oStream: TMemoryStream;
begin
  if aField.IsNull then
    result := ''
  else
  begin
    oStream := TMemoryStream.Create;
    Try
      TBlobField(aField).SaveToStream(oStream);
      oStream.Position := 0;
      result := sgcBase_Helpers.EncodeBase64(oStream);
    Finally
      sgcFree(oStream);
    End;
  end;
end;

function TsgcWSMessageDataset.GetDatasetName: String;
begin
  Result := '';
  if Assigned(Dataset) then
    Result := Dataset.Name;
end;

function TsgcWSMessageDataset.GetFormatSettings: TsgcWSDatasetFormatSettings;
begin
  if not Assigned(FFormatSettings) then
    FFormatSettings := TsgcWSDataSetFormatSettings.Create;
  Result := FFormatSettings;
end;

function TsgcWSMessageDataset.GetFS: TFormatSettings;
begin
  FFS.DecimalSeparator := FormatSettings.DecimalSeparator;
  FFS.ThousandSeparator := FormatSettings.ThousandSeparator;
  FFS.DateSeparator := FormatSettings.DateSeparator;
  FFS.TimeSeparator := FormatSettings.TimeSeparator;
  FFS.ShortDateFormat := FormatSettings.ShortDateFormat;

  Result := FFS;
end;

procedure TsgcWSMessageDataset.OnAfterPostEvent(DataSet: TDataSet);
var
  i: Integer;
begin
  for i := 0 to DataSet.FieldCount - 1 do
  begin
    if DataSet.Fields[i].DataType = ftBlob then
    begin
      if FFieldsValue[i] <> GetBlobAsString(DataSet.fields[i]) then
        GetDeltaFields.Add(DataSet.Fields[i].FieldName);
    end
    else if FFieldsValue[i] <> DataSet.fields[i].Value then
      GetDeltaFields.Add(DataSet.Fields[i].FieldName)
  end;

  if Assigned(FDSAfterPostEvent) then FDSAfterPostEvent(DataSet);
end;

procedure TsgcWSMessageDataset.OnBeforeEditEvent(DataSet: TDataSet);
var
  i: Integer;
begin
  if Assigned(FDSBeforeEditEvent) then FDSBeforeEditEvent(DataSet);

  // ... get key fields for delta changes
  GetDeltaFields.Clear;
  for i := 0 to GetKeyFields.Count - 1 do
    GetDeltaFields.Add(GetKeyFields.Fields[i].FieldName);

  SetLength(FFieldsValue, DataSet.Fieldcount);
  for i := 0 to DataSet.Fieldcount - 1 do
  begin
    if DataSet.Fields[i].DataType = ftBlob then
      FFieldsValue[i] := GetBlobAsString(DataSet.fields[i])
    else
      FFieldsValue[i] := DataSet.fields[i].Value;
  end;
end;

procedure TsgcWSMessageDataset.OnBeforeInsertEvent(DataSet: TDataSet);
var
  i: Integer;
begin
  if Assigned(FDSBeforeInsertEvent) then FDSBeforeInsertEvent(DataSet);

  // ... get key fields for delta changes
  GetDeltaFields.Clear;
  for i := 0 to GetKeyFields.Count - 1 do
    GetDeltaFields.Add(GetKeyFields.Fields[i].FieldName);

  SetLength(FFieldsValue, DataSet.Fieldcount);
  for i := 0 to DataSet.Fieldcount - 1 do
    FFieldsValue[i] := DataSet.fields[i].Value;
end;

procedure TsgcWSMessageDataset.Read(aConnection: TsgcWSConnection; const
    aMessage: String);
begin
  FWSConnection := aConnection;

  // ... enter read
  DoEnterRead(aMessage);

  Channel := '';
  if JSON.Node[CS_RESULT] <> nil then
  begin
    Method := DoReadJSONValue(CS_METHOD, JSON.Node[CS_RESULT]);
    Channel := DoReadJSONValue(CS_CHANNEL, JSON.Node[CS_RESULT]);

    if Assigned(DataSet) then
    begin
      // ... synchronize start
      if method = CS_SGC_DATASET_SYNCHRONIZE_START then
      begin
        if not DoBeforeDatasetUpdate then
          DoReadBeforeSynchronize;
      end
      // ... synchronize end
      else if method = CS_SGC_DATASET_SYNCHRONIZE_END then
      begin
        if not DoBeforeDatasetUpdate then
          DoReadAfterSynchronize;
      end
      // ... metadata
      else if Method = CS_SGC_METADATA then
      begin
        if not DoBeforeDatasetUpdate then
          DoReadMetaData;
      end
      // ... dataset
      else if Method = CS_SGC_DATASET then
      begin
        if not DoBeforeDatasetUpdate then
        begin
          if Channel = CS_SGC_DATASET_NEW then
            DoNewRecord
          else if Channel = CS_SGC_DATASET_UPDATE then
            DoUpdateRecord
          else if Channel = CS_SGC_DATASET_DELETE then
            DoDeleteRecord
        end;
      end;
    end;
  end;
  inherited Read(aMessage);
end;

procedure TsgcWSMessageDataset.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    DoFreeKeyFields;
    sgcFree(FDeltaFields);
    FDataSet := Value;
    if UpdateMode = upWhereChanged then
    begin
      FDSBeforeInsertEvent := FDataSet.BeforeInsert;
      FDSBeforeEditEvent := FDataSet.BeforeEdit;
      FDSAfterPostEvent := FDataSet.AfterPost;

      FDataSet.BeforeInsert := OnBeforeInsertEvent;
      FDataSet.BeforeEdit := OnBeforeEditEvent;
      FDataSet.AfterPost := OnAfterPostEvent;
    end;
  end;
end;

procedure TsgcWSMessageDataset.SetFormatSettings(const Value:
    TsgcWSDatasetFormatSettings);
begin
  FormatSettings.Assign(Value);

end;

function TsgcWSMessageDataset.UnEscape(const aText: String): String;
begin
  result := sgcStringReplace(aText, '\{', '{');
  result := sgcStringReplace(result, '\}', '}');
  result := sgcStringReplace(result, '\[', '[');
  result := sgcStringReplace(result, '\]', ']');
end;

function TsgcWSMessageDataset.Write: string;
begin
  DoEnterWrite;
  if method = CS_SGC_DATASET_METADATA then
    DoWriteMetaData
  else if method = CS_SGC_DATASET_SYNCHRONIZE_START then
    DoWriteBeforeSynchronize
  else if method = CS_SGC_DATASET_SYNCHRONIZE_END then
    DoWriteAfterSynchronize
  else if method = CS_SGC_DATASET then
    DoJSONRecord;
  result := inherited Write;
end;

constructor TsgcWSDatasetFormatSettings.Create;
begin
  inherited;
  DecimalSeparator := ',';
  ThousandSeparator := '.';
  DateSeparator := '/';
  TimeSeparator := ':';
  ShortDateFormat := 'dd/mm/yyyy hh:nn:ss:zzz';
end;

procedure TsgcWSDatasetFormatSettings.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSDatasetFormatSettings then
  begin
    DecimalSeparator := TsgcWSDatasetFormatSettings(aSource).DecimalSeparator;
    ThousandSeparator := TsgcWSDatasetFormatSettings(aSource).ThousandSeparator;
    DateSeparator := TsgcWSDatasetFormatSettings(aSource).DateSeparator;
    TimeSeparator := TsgcWSDatasetFormatSettings(aSource).TimeSeparator;
    ShortDateFormat := TsgcWSDatasetFormatSettings(aSource).ShortDateFormat;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
