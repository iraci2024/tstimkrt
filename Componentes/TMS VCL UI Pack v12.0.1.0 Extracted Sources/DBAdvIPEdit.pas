{************************************************************************}
{ TDBADVIPEDIT component                                                 }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2023                                            }
{            Email : info@tmssoftware.com                                }
{            Web : http://www.tmssoftware.com                            }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit dbadvipedit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvIPEdit, DB, DBCtrls;

type
  TUpdateRecordEvent = procedure(Sender: TObject; var Value: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvIPEdit = class(TAdvIPEdit)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FOldState: TDataSetState;
    FClearOnInsert: Boolean;
    FIsEditing: Boolean;
    FFieldReadOnly: Boolean;
    FFocused: Boolean;
    FShowFieldName: boolean;
    FOnUpdateRecord: TUpdateRecordEvent;
    procedure SetFocused(Value: Boolean);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure ActiveChange(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure WMClear(var Message: TWMClear); message WM_CLEAR;
    procedure CMExit(var Message: TWMNoParams); message CM_EXIT;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure ResetMaxLength;
    procedure SetShowFieldName(const Value: boolean);
    procedure UpdateFieldName;
    function GetField: TField;
  protected
    { Protected declarations }
    procedure DataUpdate(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure OctetKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure OctetKeyPress(Sender: TObject; var Key: Char); override;
    procedure OctetExit(Sender: TObject); override;
    function DoEditCanModify: Boolean; virtual;
    procedure DoUpdateRecord(var Value: string); virtual;
    property DataLink: TFieldDataLink read FDataLink;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    { Published declarations }
    property ClearOnInsert: Boolean read FClearOnInsert write FClearOnInsert default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ShowFieldName: boolean read FShowFieldName write SetShowFieldName default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  end;


implementation

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;

{ TDBAdvIPEdit }

procedure TDBAdvIPEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and
       (F.DataType in [ftString, ftWideString]) and
       (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TDBAdvIPEdit.Change;
begin
  FDataLink.Modified;
  inherited;
end;

procedure TDBAdvIPEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

procedure TDBAdvIPEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBAdvIPEdit.OctetExit(Sender: TObject);
var
  s: string;
  domod: boolean;
begin
  if not FDataLink.ReadOnly then
  begin
    s := IPAddress;

    domod := Modified;
    DoUpdateRecord(s);

    IPAddress := s;
    Modified := domod;
    try
      FDataLink.UpdateRecord;     { tell data link to update database }
    except
      on Exception do
      begin
        SetFocus;   // if it failed, don't let focus leave
        raise;
      end;
    end;
  end;

  inherited;
end;

procedure TDBAdvIPEdit.OctetKeyPress(Sender: TObject; var Key: Char);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if not (Key = #27) and not ((Key = #13) and ReturnIsTab) and
    (not ((Key = #3) and (GetKeyState(VK_CONTROL) and $8000 = $8000))) then
    if not DoEditCanModify then
    begin
      Key := #0;
      Exit;
    end;

  if (Key = #8) and not DoEditCanMOdify then
    Exit;

  inherited OctetKeyPress(Sender, Key);

  {$IFNDEF DELPHI_UNICODE}
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and (Key <> '.') and
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  if (Key >= #32) and (FDataLink.Field <> nil) and (Key <> '.') and
  {$ENDIF}
    not FDataLink.Field.IsValidChar(Key) or (FDataLink.ReadOnly) then
  begin
    MessageBeep(0);
    Key := #0;
  end;

  case Key of
    ^H, ^V, ^X, #32..#255:
      begin
        FIsEditing := true;
        FDataLink.Edit;
      end;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDBAdvIPEdit.OctetKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;

  if DoEditCanModify then
    DataUpdate(Self);
end;

procedure TDBAdvIPEdit.CMExit(var Message: TWMNoParams);
var
  s: string;
  domod: boolean;
begin
  if not FDataLink.ReadOnly then
  begin
    s := IPAddress;
    domod := Modified;
    DoUpdateRecord(s);
    IPAddress := s;
    Modified := domod;
    try
      FDataLink.UpdateRecord;     { tell data link to update database }
    except
      on Exception do
      begin
        SetFocus;   // if it failed, don't let focus leave
        raise;
      end;
    end;
  end;

  inherited;
end;

procedure TDBAdvIPEdit.SetFocused(Value: boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    FDataLink.Reset;
  end;
end;

procedure TDBAdvIPEdit.CMEnter(var Message: TWMNoParams);
begin
  SetFocused(True);
  inherited;
  if Assigned(DataSource) and not FDataLink.CanModify then
  begin
    inherited ReadOnly := true;
    FFieldReadOnly := true;
  end;

  if Assigned(DataSource) and FDataLink.CanModify and FFieldReadOnly then
  begin
    inherited ReadOnly := false;
    FFieldReadOnly := false;
  end;
end;

constructor TDBAdvIPEdit.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  ControlStyle := ControlStyle + [csReplicatable];
  FClearOnInsert := False;
  FFieldReadOnly := False;
end;

procedure TDBAdvIPEdit.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if FIsEditing and (FDataLink.DataSet.State = dsEdit) and Assigned(OnChange) then
    Exit;

  FIsEditing := false;

  if Assigned(FDataLink.Field) and
     not (FClearOnInsert and (FDataLink.DataSet.State = dsInsert) and
     (FOldState <> dsInsert))  then
  begin
    if FDataLink.field.DataType in [ftString, ftWideString] then
    begin
      if FFocused and FDataLink.CanModify then
        IPAddress := FDataLink.Field.Text
      else
        IPAddress := FDataLink.Field.DisplayText;
    end
    else
    if FDataLink.field.DataType in [ftInteger, ftCurrency {$IFDEF DELPHIXE11_LVL} , ftLongWord, ftLargeInt {$ENDIF}] then
    begin
      {$IFDEF DELPHIXE11_LVL}
      IntIPAddress := FDataLink.Field.AsLongWord;
      {$ELSE}
      IntIPAddress := FDataLink.Field.AsInteger;
      {$ENDIF}
    end;

    Modified := False;
  end;

  if (FDataLink.DataSet.State = dsInsert) and FClearOnInsert
    and (FOldState <> dsInsert) then
  begin
    IPAddress := '0.0.0.0';
  end;

  FOldState := FDataLink.DataSet.State;
end;

procedure TDBAdvIPEdit.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
     not (FClearOnInsert and (FDataLink.DataSet.State = dsInsert)) then
  begin
    if FDataLink.field.DataType in [ftString, ftWideString] then
      FDataLink.Field.Text := IPAddress
    else
    if FDataLink.field.DataType in [ftInteger, ftCurrency {$IFDEF DELPHIXE11_LVL} , ftLongWord, ftLargeint {$ENDIF}] then
      {$IFDEF DELPHIXE11_LVL}
      FDataLink.Field.AsLongWord := IntIPAddress;
      {$ELSE}
      FDataLink.Field.AsInteger := IntIPAddress;
      {$ENDIF}
  end;
end;

destructor TDBAdvIPEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBAdvIPEdit.DoUpdateRecord(var Value: string);
begin
  if Assigned(OnUpdateRecord) then
    OnUpdateRecord(Self, Value);
end;

function TDBAdvIPEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvIPEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvIPEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBAdvIPEdit.GetReadOnly: Boolean;
begin
  if Assigned(DataSource) then
    Result := FDataLink.ReadOnly or inherited ReadOnly
  else
    Result := inherited ReadOnly;
end;

procedure TDBAdvIPEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
  UpdateFieldName;
end;

procedure TDBAdvIPEdit.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvIPEdit.SetReadOnly(Value: Boolean);
begin
  inherited ReadOnly := Value;
  FDataLink.ReadOnly := Value;
end;

procedure TDBAdvIPEdit.SetShowFieldName(const Value: boolean);
begin
  if (FShowFieldName <> Value) then
  begin
    FShowFieldName := Value;
    UpdateFieldName;
  end;
end;

procedure TDBAdvIPEdit.WMCut(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TDBAdvIPEdit.WMPaste(var Message: TMessage);
begin
  if not FDataLink.Readonly then
   begin
    if FDataLink.Edit then
      inherited;
   end;
end;

procedure TDBAdvIPEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBAdvIPEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if (Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    if not DoEditCanModify then
    begin
      key := 0;
      Exit;
    end
    else
      FDataLink.Modified;
  end;

  if FDataLink.ReadOnly and (key = VK_DELETE) then
    Key := 0;

  inherited KeyDown(Key, Shift);

  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDBAdvIPEdit.KeyPress(var Key: Char);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if not (Key = #27) and not ((Key = #13) and ReturnIsTab) and
    (not ((Key = #3) and (GetKeyState(VK_CONTROL) and $8000 = $8000))) then
    if not DoEditCanMOdify then
    begin
      Key := #0;
      Exit;
    end;

  if (Key = #8) and not DoEditCanMOdify then
    Exit;

  inherited KeyPress(Key);

  {$IFNDEF DELPHI_UNICODE}
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and (Key <> '.') and
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  if (Key >= #32) and (FDataLink.Field <> nil) and (Key <> '.') and
  {$ENDIF}
    not FDataLink.Field.IsValidChar(Key) or (FDataLink.ReadOnly) then
  begin
    MessageBeep(0);
    Key := #0;
  end;

  case Key of
    ^H, ^V, ^X, #32..#255:
      begin
        FIsEditing := true;
        FDataLink.Edit;
      end;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDBAdvIPEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if DoEditCanModify then
    DataUpdate(Self);
end;

procedure TDBAdvIPEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
end;

procedure TDBAdvIPEdit.UpdateFieldName;
var
  fld: TField;
begin
  if ShowFieldName then
  begin
    if Assigned(FDataLink) and Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active or not DataSource.Enabled then
        LabelCaption := '';

      if FDataLink.DataSet.Active and DataSource.Enabled and (DataField <> '') then
      begin
        fld := FDataLink.Dataset.FieldByName(DataField);
        if Assigned(fld) then
          LabelCaption := fld.FieldName;
      end;
    end;
  end;
end;

procedure TDBAdvIPEdit.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active or not DataSource.Enabled then
        Text := '';

      UpdateFieldName;
    end
    else
      Text := '';
  end;
end;

function TDBAdvIPEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBAdvIPEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

function TDBAdvIPEdit.DoEditCanModify: Boolean;
begin
  if Assigned(DataSource) then
  begin
    Result := FDataLink.CanModify;

    if Result then
    begin
      if not FDataLink.DataSource.AutoEdit and (FDataLink.DataSet.State = dsBrowse) then
        Result := false;
    end;

  end
  else
    Result := true;
end;

procedure TDBAdvIPEdit.WMClear(var Message: TWMClear);
begin
  if FDataLink.Edit then
    inherited;
end;


end.
