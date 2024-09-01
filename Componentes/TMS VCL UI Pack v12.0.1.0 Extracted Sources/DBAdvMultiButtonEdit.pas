{**************************************************************************}
{ TDBAdvMultiButtonEdit component                                          }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2023                                                         }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : https://www.tmssoftware.com                                      }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit DBAdvMultiButtonEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvEdit, DB, DBCtrls, AdvMultiButtonEdit, DBAdvEd;

{$I TMSDEFS.INC}

type

  TDBAdvMultiButtonEdit = class(TAdvMultiButtonEdit)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FShowFieldName: boolean;
    FOldState: TDataSetState;
    FIsEditing: Boolean;
    FClearOnInsert: Boolean;
    FFocused: Boolean;
    FOnUpdateRecord: TUpdateRecordEvent;

    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure ResetMaxLength;
    procedure UpdateFieldName;
    procedure SetShowFieldName(const Value: boolean);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    function GetField: TField;
    procedure SetFocused(Value: Boolean);
  protected
    procedure DataUpdate(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DoEditChange(Sender: TObject); override;
    procedure DoUpdateRecord(var Value: string); virtual;
    function EditCanModify: Boolean; virtual;
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeypress(Sender: TObject; var Key: Char); override;
    procedure DoEditEnter(Sender: TObject); override;
    procedure DoEditExit(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;

  published
    property ClearOnInsert: Boolean read FClearOnInsert write FClearOnInsert default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ShowFieldName: boolean read FShowFieldName write SetShowFieldName default False;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  end;

  TDBAdvTouchSpinEdit = class(TAdvTouchSpinEdit)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FShowFieldName: boolean;
    FOldState: TDataSetState;
    FIsEditing: Boolean;
    FClearOnInsert: Boolean;
    FFocused: Boolean;
    FOnUpdateRecord: TUpdateRecordEvent;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetShowFieldName(const Value: boolean);
    procedure ResetMaxLength;
    procedure UpdateFieldName;
    procedure SetFocused(Value: Boolean);
  protected
    procedure DataUpdate(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DoEditChange(Sender: TObject); override;
    procedure DoClickButton(ButtonIndex: integer); override;
    procedure DoUpdateRecord(var Value: string); virtual;
    function EditCanModify: Boolean; virtual;
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeypress(Sender: TObject; var Key: Char); override;
    procedure DoEditEnter(Sender: TObject); override;
    procedure DoEditExit(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ShowFieldName: boolean read FShowFieldName write SetShowFieldName default False;
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

{ TDBAdvMultiButtonEdit }

procedure TDBAdvMultiButtonEdit.ActiveChange(Sender: TObject);
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

procedure TDBAdvMultiButtonEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

constructor TDBAdvMultiButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TDBAdvMultiButtonEdit.DataChange(Sender: TObject);
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
    case FDataLink.Field.Alignment of
    taLeftJustify: EditAlign := eaLeft;
    taRightJustify: EditAlign := eaRight;
    taCenter: EditAlign := eaCenter;
    end;

    if (FDataLink.Field.DataType in [ftString , ftWideString]) and (MaxLength = 0) then
      MaxLength := FDataLink.Field.Size;

    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
      Text := FDataLink.Field.DisplayText;

    Edit.Modified := False;
  end;

  if (FDataLink.DataSet.State = dsInsert) and FClearOnInsert
    and (FOldState <> dsInsert) then
  begin
    Text := '';
  end;

  FOldState := FDataLink.DataSet.State;
end;

procedure TDBAdvMultiButtonEdit.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
     not (FClearOnInsert and (FDataLink.DataSet.State = dsInsert)) then
  begin
    FDataLink.Field.Text := Text;
  end;
end;

destructor TDBAdvMultiButtonEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBAdvMultiButtonEdit.DoEditChange(Sender: TObject);
begin
  FDataLink.Modified;
  inherited;
end;

procedure TDBAdvMultiButtonEdit.DoEditEnter(Sender: TObject);
begin
  SetFocused(True);
  inherited;
end;

procedure TDBAdvMultiButtonEdit.DoEditExit(Sender: TObject);
var
  s: string;
  domod: boolean;
begin
  if not FDataLink.ReadOnly then
  begin
    s := Text;
    domod := Edit.Modified;
    DoUpdateRecord(s);
    Text := s;
    Edit.Modified := domod;
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

procedure TDBAdvMultiButtonEdit.DoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if (Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    if not EditCanModify then
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

procedure TDBAdvMultiButtonEdit.DoEditKeypress(Sender: TObject; var Key: Char);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if not (Key = #27) and not ((Key = #13) and ReturnIsTab) and
    (not ((Key = #3) and (GetKeyState(VK_CONTROL) and $8000 = $8000))) then
    if not EditCanMOdify then
    begin
      Key := #0;
      Exit;
    end;

  if (Key = #8) and not EditCanMOdify then
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
        Edit.SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDBAdvMultiButtonEdit.DoUpdateRecord(var Value: string);
begin
  if Assigned(OnUpdateRecord) then
    OnUpdateRecord(Self, Value);
end;

function TDBAdvMultiButtonEdit.EditCanModify: Boolean;
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

function TDBAdvMultiButtonEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBAdvMultiButtonEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvMultiButtonEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBAdvMultiButtonEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBAdvMultiButtonEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBAdvMultiButtonEdit.ResetMaxLength;
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

procedure TDBAdvMultiButtonEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
  UpdateFieldName;
end;

procedure TDBAdvMultiButtonEdit.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvMultiButtonEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    FDataLink.Reset;
  end;
end;

procedure TDBAdvMultiButtonEdit.SetShowFieldName(const Value: boolean);
begin
  if (FShowFieldName <> Value) then
  begin
    FShowFieldName := Value;
    UpdateFieldName;
  end;
end;

function TDBAdvMultiButtonEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TDBAdvMultiButtonEdit.UpdateFieldName;
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

{ TDBAdvTouchSpinEdit }

procedure TDBAdvTouchSpinEdit.ActiveChange(Sender: TObject);
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

constructor TDBAdvTouchSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TDBAdvTouchSpinEdit.DataChange(Sender: TObject);
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
    case FDataLink.Field.Alignment of
    taLeftJustify: EditAlign := eaLeft;
    taRightJustify: EditAlign := eaRight;
    taCenter: EditAlign := eaCenter;
    end;

    if (FDataLink.Field.DataType in [ftString , ftWideString]) and (MaxLength = 0) then
      MaxLength := FDataLink.Field.Size;

    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
      Text := FDataLink.Field.DisplayText;

    Edit.Modified := False;
  end;

  if (FDataLink.DataSet.State = dsInsert) and FClearOnInsert
    and (FOldState <> dsInsert) then
  begin
    Text := '';
  end;

  FOldState := FDataLink.DataSet.State;
end;

procedure TDBAdvTouchSpinEdit.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and
     not (FClearOnInsert and (FDataLink.DataSet.State = dsInsert)) then
  begin
    FDataLink.Field.Text := Text;
  end;
end;

destructor TDBAdvTouchSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBAdvTouchSpinEdit.DoClickButton(ButtonIndex: integer);
begin
  if not FDataLink.ReadOnly then
  begin
    FDataLink.Modified;
    FDataLink.Edit;
    inherited;
  end;
end;

procedure TDBAdvTouchSpinEdit.DoEditChange(Sender: TObject);
begin
  FDataLink.Modified;
  inherited;
end;

procedure TDBAdvTouchSpinEdit.DoEditEnter(Sender: TObject);
begin
  SetFocused(True);
  inherited;
end;

procedure TDBAdvTouchSpinEdit.DoEditExit(Sender: TObject);
var
  s: string;
  domod: boolean;
begin
  if not FDataLink.ReadOnly then
  begin
    s := Text;
    domod := Edit.Modified;
    DoUpdateRecord(s);
    Text := s;
    Edit.Modified := domod;
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

procedure TDBAdvTouchSpinEdit.DoEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if (Key = VK_DELETE) or (Key = VK_BACK) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    if not EditCanModify then
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

procedure TDBAdvTouchSpinEdit.DoEditKeypress(Sender: TObject; var Key: Char);
begin
  if not Assigned(DataSource) then
  begin
    inherited;
    Exit;
  end;

  if not (Key = #27) and not ((Key = #13) and ReturnIsTab) and
    (not ((Key = #3) and (GetKeyState(VK_CONTROL) and $8000 = $8000))) then
    if not EditCanMOdify then
    begin
      Key := #0;
      Exit;
    end;

  if (Key = #8) and not EditCanMOdify then
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
        Edit.SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDBAdvTouchSpinEdit.DoUpdateRecord(var Value: string);
begin
  if Assigned(OnUpdateRecord) then
    OnUpdateRecord(Self, Value);
end;

function TDBAdvTouchSpinEdit.EditCanModify: Boolean;
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

function TDBAdvTouchSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvTouchSpinEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBAdvTouchSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBAdvTouchSpinEdit.ResetMaxLength;
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

procedure TDBAdvTouchSpinEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
  UpdateFieldName;
end;

procedure TDBAdvTouchSpinEdit.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBAdvTouchSpinEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    FDataLink.Reset;
  end;
end;

procedure TDBAdvTouchSpinEdit.SetShowFieldName(const Value: boolean);
begin
  if (FShowFieldName <> Value) then
  begin
    FShowFieldName := Value;
    UpdateFieldName;
  end;
end;

procedure TDBAdvTouchSpinEdit.UpdateFieldName;
begin

end;

end.
