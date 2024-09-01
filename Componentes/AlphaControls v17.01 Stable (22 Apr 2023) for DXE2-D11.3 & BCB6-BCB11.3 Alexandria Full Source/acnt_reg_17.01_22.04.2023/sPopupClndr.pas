unit sPopupClndr;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  {$IFDEF DELPHI6UP}Variants, {$ENDIF}
  sMonthCalendar, sConst, sPanel, acntUtils, sCustomComboEdit, ExtCtrls;


type
  TsPopupCalendar = class(TForm)
    sMonthCalendar1: TsMonthCalendar;
    procedure sToolButton3Click(Sender: TObject);
    procedure sToolButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CalendarClick(ARow, ACol: integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    CurrentPPI: integer;
    FCloseUp: TNotifyEvent;
    procedure KeyPress(var Key: Char); override;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  public
    FEditor: TsCustomComboEdit;
    function IsValidDate(ADate: TDateTime): boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseUp;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    property FCalendar: TsMonthCalendar read sMonthCalendar1 write sMonthCalendar1;
    property OnCloseUp: TNotifyEvent read FCloseUp write FCloseUp;
  end;


implementation

{$R *.dfm}

uses
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sToolEdit, sSkinManager, sGraphUtils, sMessages, sStyleSimply;


function TsPopupCalendar.GetValue: Variant;
begin
  if csDesigning in ComponentState then
    Result := VarFromDateTime(SysUtils.Date)
  else
    Result := VarFromDateTime(FCalendar.CalendarDate);
end;


procedure TsPopupCalendar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (FCalendar <> nil) and (Key <> #0) then
    FCalendar.FGrid.KeyPress(Key);
end;


procedure TsPopupCalendar.Loaded;
begin
  if DefaultManager <> nil then
    Scaled := DefaultManager.Options.ScaleMode = smVCL
  else
    Scaled := True;

  inherited;
  Height := Height + Integer(TsDateEdit(FEditor).ShowTodayBtn) * 16;
end;


procedure TsPopupCalendar.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then
    try
      if (Trim(ReplaceStr(VarToStr(Value), {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DateSeparator, '')) = '') or VarIsNull(Value) or VarIsEmpty(Value) then
        FCalendar.CalendarDate := VarToDateTime(SysUtils.Date)
      else
        FCalendar.CalendarDate := VarToDateTime(Value);
    except
      FCalendar.CalendarDate := VarToDateTime(SysUtils.Date);
    end;
end;


procedure TsPopupCalendar.sToolButton3Click(Sender: TObject);
begin
  Application.Minimize;
end;


procedure TsPopupCalendar.sToolButton1Click(Sender: TObject);
begin
  Close;
end;


procedure TsPopupCalendar.CloseUp;
begin
  if Assigned(FCloseUp) then
    FCloseUp(FEditor);
end;


constructor TsPopupCalendar.Create(AOwner: TComponent);
begin
  FEditor := TsCustomComboEdit(AOwner);
  CurrentPPI := 96;
  inherited;
end;


destructor TsPopupCalendar.Destroy;
begin
  inherited;
end;


procedure TsPopupCalendar.FormShow(Sender: TObject);
begin
  sMonthCalendar1.FDragBar.Cursor := crDefault;
  if (DefaultManager <> nil) and DefaultManager.Active then
    Color := DefaultManager.GetGlobalColor
  else
    Color := clBtnFace;

  if Assigned(FEditor) then
    sMonthCalendar1.ShowCurrentDate := TsDateEdit(FEditor).ShowCurrentDate;
end;


type
  TAccessGrid = class(TsCalendGrid);

procedure TsPopupCalendar.CalendarClick(ARow, ACol: integer);
var
  CanAccept: boolean;
  d: TDateTime;
begin
  CanAccept := True;
  if FEditor <> nil then begin
    d := sMonthCalendar1.CellDate[ACol, ARow];
    if not IsValidDate(d) then
      Exit;

    if not sMonthCalendar1.Initializing {and (TsCustomDateEdit(FEditor).Date <> d) {and Assigned(TsDateEdit(FEditor).OnAcceptDate)} then
      if sMonthCalendar1.DimUnacceptedCells then
        CanAccept := TAccessGrid(sMonthCalendar1.FGrid).CellsStates[ARow][ACol] = 1
      else
        if Assigned(TsDateEdit(FEditor).OnAcceptDate) then
          TsDateEdit(FEditor).OnAcceptDate(FEditor, d, CanAccept);

    if CanAccept then begin
      if TsCustomDateEdit(FEditor).Date <> d then begin
        TsCustomDateEdit(FEditor).Date := d;
        TsCustomDateEdit(FEditor).Modified := True;
        if Assigned(TsCustomDateEdit(FEditor).OnChange) then
          TsCustomDateEdit(FEditor).OnChange(TsCustomDateEdit(FEditor));
      end;
      Close;
      // Clear a global variable with visible calendar
      if sPopupCalendar = Self then
        sPopupCalendar := nil;

      if Assigned(FEditor) and FEditor.Visible and FEditor.Enabled then begin
        if FEditor.CanFocus and IsWindowVisible(FEditor.Handle) then
          FEditor.SetFocus;

        CloseUp;
        if FEditor.Focused and FEditor.AutoSelect then
          FEditor.SelectAll;

        Exit; // Closed already
      end;
    end;
  end;
  if CanAccept then
    Close;
end;


procedure TsPopupCalendar.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  sMonthCalendar1.FGrid.Clicked := False;
  // Clear a global variable with visible calendar
  if sPopupCalendar = Self then
    sPopupCalendar := nil;

  inherited;
end;


type
  TAccessCalendar = class(TsMonthCalendar);

procedure TsPopupCalendar.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  with TAccessCalendar(sMonthCalendar1) do
    CanClose := (PopMenu = nil) or (PopMenu.PopupComponent = nil);
end;


procedure TsPopupCalendar.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETSCALE:
          Message.Result := CurrentPPI;

        AC_SETSCALE:
          CurrentPPI := Message.LParam;

        AC_GETFONTINDEX: if sMonthCalendar1.SkinData.SkinManager <> nil then begin

          PacPaintInfo(Message.LParam)^.FontIndex := sMonthCalendar1.SkinData.SkinManager.SkinCommonInfo.Sections[ssPanelLow];
          Message.Result := 1;
          Exit;
        end;
      end;

    WM_ERASEBKGND:
      if Assigned(DefaultManager) and DefaultManager.Active then
        Exit;
  end;
  inherited;
end;


function TsPopupCalendar.IsValidDate(ADate: TDateTime): boolean;
begin
  with TsCustomDateEdit(FEditor) do
    if (MinDate <> 0) and (ADate < MinDate) then
      Result := False
    else
      if (MaxDate <> 0) and (ADate > MaxDate) then
        Result := False
      else
        Result := True;
end;

end.
