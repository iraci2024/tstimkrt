unit sMonthCalendar;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses Windows, Classes, Controls, SysUtils, Graphics, buttons, grids, messages, StdCtrls, forms, comctrls, Menus,
  {$IFNDEF DELPHI5} types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  acntUtils, sPanel, sGraphUtils, sConst, sMessages, sDateUtils, sSpeedButton, sDefaults;


type
{$IFNDEF NOTFORHELP}
  TGetCellParams = procedure(Sender: TObject; Date: TDatetime; AFont: TFont; var Background: TColor) of object;
  TDayCellParams = procedure(Sender: TObject; Num: integer; AFont: TFont; var Background: TColor) of object;
  TDateMouseEvent = procedure(Sender: TObject; Date: TDatetime) of object;
  TsMonthCalendar = class;
  TStatesRow = array of byte;
  TStatesRows = array of TStatesRow;


  TsCalendGrid = class(TDrawGrid)
  private
    FMouseDate: TDateTime;
    FOwner: TsMonthCalendar;
    FCellsStates: TStatesRows;
    procedure WMSize         (var Message: TWMSize ); message WM_SIZE;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure SetMouseDate(const Value: TDateTime);
  protected
    MouseCell: TPoint;
    TrySelectCol,
    TrySelectRow: integer;
    function CellsStates: TStatesRows;
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    Clicked: boolean;
    procedure Click; override;
    procedure UpdateStates;
    function CurrentMonth(aDate: TDateTime): boolean;
    procedure KeyPress(var Key: Char); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure Paint; override;
    procedure CreateWnd; override;
    constructor Create(AOwner: TComponent); override;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure SetMouseCell(const p: TPoint);
    property MouseDate: TDateTime read FMouseDate write SetMouseDate;
    property GridLineWidth;
    property DefaultColWidth;
    property DefaultRowHeight;
  end;

  TacCalendarStates = set of (csMonthChanging, csCmpLoading);
  TOnAcceptDate = procedure(Sender: TObject; var aDate: TDateTime; var CanAccept: boolean) of object;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsMonthCalendar = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    FReadOnly,
    FUpdating,
    FAnimated,
    FShowTitle,
    FShowWeeks,
    FShowTodayBtn,
    FShowYearBtns,
    FShowMonthBtns,
    FUseCurrentDate,
    FShowCurrentDate,
    FShowSelectAlways,
    FAllowChangeMonth,
    FDimUnacceptedCells,
    FTravellingSelection,
    FSelectAllowedDateOnly: boolean;

    FDate,
    FMinDate,
    FMaxDate: TDateTime;

    FOnDrawDay,
    FOnGetCellParams: TGetCellParams;

    FOnGetDayOfWeekParams: TDayCellParams;

    FTitleHeight,
    FTitleBtnsWidth: integer;

    FOnDateMouseEnter,
    FOnDateMouseLeave: TDateMouseEvent;

    FWeekendColor: TColor;
    FWeekends:     TDaysOfWeek;
    FOnChange:     TNotifyEvent;
    FStartOfWeek:  TCalDayOfWeek;
    FOnAcceptDate: TOnAcceptDate;

    function GetCellText(ACol, ARow: Integer): string;
    function GetDayNum  (ACol, ARow: Integer): integer;
    function GetDateElement(Index: Integer): Integer;

    function IsWeekend(ACol: Integer): Boolean;
    procedure CalendarUpdate(DayOnly: Boolean);
    function StoreCalendarDate: Boolean;
    function FirstDay: integer;
    procedure TopPanelDblClick(Sender: TObject);
    procedure DoChangeAnimated(ForwDirection: boolean; OldBmp: TBitmap);

    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends    (Value: TDaysOfWeek);
    procedure SetStartOfWeek (Value: TCalDayOfWeek);
    procedure SetDateElement (Index: Integer; Value: Integer);

    procedure UpdateTitleHeight;
    procedure SetBoolean(const Index: Integer; const Value: boolean);
    procedure SetDate(const Index: Integer; const Value: TDateTime);
    procedure SetCalendarDate(const Value: TDateTime);
    procedure SetTitleHeight(const Value: integer);
    procedure SetTitleBtnsWidth(const Value: integer);
    procedure SetCalendarChanged(const Value: boolean);
  protected
    PopMenu: TPopupMenu;
    FMonthOffset: Integer;
    ShownDate: TDateTime;
    FCalendarChanged: boolean;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick (Sender: TObject);
    procedure NextYearBtnClick (Sender: TObject);
    procedure MenuClick        (Sender: TObject);
    procedure TodayClick       (Sender: TObject);
    procedure TitleClick       (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MakePopup;
    procedure UpdateProps;
    procedure UpdateNavBtns;
    procedure Change;
    function DaysThisMonth: Integer;
    function ThisDayNum (ACol, ARow: integer): integer;
    procedure ChangeMonth(Delta: Integer; AllowAnimation: boolean = False);
    procedure ChangeYear (Delta: Integer; AllowAnimation: boolean = False);
    procedure GoToShownDate(ToForward: boolean; AllowAnimation: boolean = False);
  public
    Initializing: boolean;
    State: TacCalendarStates;
    FGrid: TsCalendGrid;
    FDragBar: TsDragBar;
    FCaptBtn,
    FTodayBtn: TsSpeedButton;
    FBtns: array [0..3] of TsTimerSpeedButton;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    function MousePosToDate(X, Y: Integer): TDateTime;
    function IsValidDate(Date: TDateTime): boolean;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar;
    procedure WndProc(var Message: TMessage); override;
    function GetCellDate(ACol, ARow: Integer): TDateTime;
    procedure RefreshAcceptedDates;
    property CellDate[ACol, ARow: Integer]: TDateTime read GetCellDate;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
    property OnDrawDay: TGetCellParams read FOnDrawDay write FOnDrawDay;
    property MaxDate: TDateTime Index 1 read FMaxDate write SetDate;
    property MinDate: TDateTime Index 2 read FMinDate write SetDate;
    property SelectAllowedDateOnly: boolean read FSelectAllowedDateOnly write FSelectAllowedDateOnly default True;
    property CalendarChanged: boolean read FCalendarChanged write SetCalendarChanged;
{$ENDIF} // NOTFORHELP
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property BorderWidth default 3;
    property BevelWidth  default 5;
    property Width  default 178;
    property Height default 139;
{$ENDIF} // NOTFORHELP
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDateMouseEnter: TDateMouseEvent read FOnDateMouseEnter write FOnDateMouseEnter;
    property OnDateMouseLeave: TDateMouseEvent read FOnDateMouseLeave write FOnDateMouseLeave;
    property OnAcceptDate: TOnAcceptDate read FOnAcceptDate write FOnAcceptDate;
{:@event}
    property OnGetCellParams: TGetCellParams read FOnGetCellParams write FOnGetCellParams;
    property OnGetDayOfWeekParams: TDayCellParams read FOnGetDayOfWeekParams write FOnGetDayOfWeekParams;
    property Animated: boolean read FAnimated write FAnimated default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property StartOfWeek: TCalDayOfWeek read FStartOfWeek write SetStartOfWeek default dowLocaleDefault;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default DefWeekends;

    property CalendarDate: TDateTime read FDate write SetCalendarDate stored StoreCalendarDate;

    property Year:  Integer index 1 read GetDateElement write SetDateElement stored False;
    property Day:   Integer index 3 read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;

    property TitleHeight: integer read FTitleHeight write SetTitleHeight default 18;
    property TitleBtnsWidth: integer read FTitleBtnsWidth write SetTitleBtnsWidth default 18;

    property AllowChangeMonth: boolean index 0 read FAllowChangeMonth write SetBoolean default True;
    property ShowCurrentDate:  boolean index 1 read FShowCurrentDate  write SetBoolean default True;
    property ShowSelectAlways: boolean index 2 read FShowSelectAlways write SetBoolean default True;
    property ShowTitle:        boolean index 3 read FShowTitle        write SetBoolean default True;
    property ShowTodayBtn:     boolean index 4 read FShowTodayBtn     write SetBoolean default False;
    property ShowWeeks:        boolean index 5 read FShowWeeks        write SetBoolean default False;
    property ShowYearBtns:     boolean index 6 read FShowYearBtns     write SetBoolean default True;
    property ShowMonthBtns:    boolean index 7 read FShowMonthBtns    write SetBoolean default True;
    property UseCurrentDate:   boolean index 8 read FUseCurrentDate   write SetBoolean default True;
    property DimUnacceptedCells: boolean index 9 read FDimUnacceptedCells write SetBoolean default True;

    property TravellingSelection: boolean read FTravellingSelection write FTravellingSelection default True;
  end;


var
  s_WeeksTitle: acString = '¹/w';
  s_Today: acString;

{$IFNDEF NOTFORHELP}
function acChangeMonth(aDelta: Integer; var aDate: TDateTime): Boolean;
function acChangeYear (aDelta: Integer; var aDate: TDateTime): Boolean;
{$ENDIF} // NOTFORHELP

implementation

uses
  ExtCtrls, math,
  {$IFDEF DELPHI6UP} DateUtils, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  acgpUtils, sCommonData, sSkinProps, sSkinManager, sPopupClndr, sVclUtils, sToolEdit, sStyleSimply;


function WeekNum(const ADate: TDateTime): Word;
var
  Y, M, D: Word;
  dtTmp: TDateTime;
  A: array [0..1] of char;
begin
  DecodeDate(ADate, Y, M, D);
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTWEEKOFYEAR, A, SizeOf(A));
{$IFDEF DELPHI6UP}
  if Ord(A[0]) - Ord(ZeroChar) = 0 then begin
{$ENDIF}
    dtTmp := EnCodeDate(Y, 1, 1);
    Result := (Trunc(ADate - dtTmp) + (DayOfWeek(dtTmp) - 1)) div 7 + 1;
{$IFDEF DELPHI6UP}
  end
  else
    DecodeDateWeek(ADate, Y, Result, D);
{$ENDIF}
  if Result <> 0 then
    Result := Result - 1;
end;


const
  CaptArray: array [boolean{RTL}, 0..3] of Char = (('7', '3', '4', '8'), ('8', '4', '3', '7'));

constructor TsMonthCalendar.Create(AOwner: TComponent);
var
  i: integer;
  sp: TsPanel;
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsMonthCalendar;
  SkinData.FUpdating := False;

  FShowTitle := True;
  FShowTodayBtn := False;
  FShowSelectAlways := True;
  FTravellingSelection := True;
  PopMenu := nil;

  FDragBar := TsDragBar.Create(Self);
  FDragBar.OnDblClick := TopPanelDblClick;
  FDragBar.OnMouseDown := TitleClick;

  FDragBar.Align := alTop;
  FDragBar.Top := -1;
  FAnimated := True;
  FAllowChangeMonth := True;
  FTitleHeight := 18;
  FTitleBtnsWidth := 18;
  FCalendarChanged := False;
  FDimUnacceptedCells := True;
  ShownDate := 0;

  FMaxDate := 0;
  FMinDate := 0;

  sp := TsPanel.Create(Self);
  with sp do begin
    Align := alTop;
    Height := 3;
    Caption := '';
    BevelOuter := bvNone;
    Parent := Self;
  end;

  FGrid := TsCalendGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.ParentColor := True;
  FGrid.OnDblClick := OnDblClick;
  FGrid.OnClick := OnClick;
  BorderWidth := 3;
  BevelWidth := 1;
  Caption := s_Space;
  FShowCurrentDate := True;
  FSelectAllowedDateOnly := True;

  FShowYearBtns := True;
  FShowMonthBtns := True;

  FUseCurrentDate := True;
  FStartOfWeek := dowLocaleDefault;
  FWeekends := DefWeekends;
  FWeekendColor := clRed;
  FDate := Date;
  Width  := 178;
  Height := 139;

  for i := 0 to 3 do begin
    FBtns[i] := TsTimerSpeedButton.Create(Self);
    FBtns[i].Parent := FDragBar;
    FBtns[i].Margin := 2;
    FBtns[i].Flat := True;
    FBtns[i].Caption := CaptArray[BidiMode = bdRightToLeft, i];
    FBtns[i].Font.Name := s_Webdings;
  end;
  FBtns[0].Align := alLeft;
  FBtns[0].OnClick := PrevYearBtnClick;

  FBtns[1].Align := alLeft;
  FBtns[1].OnClick := PrevMonthBtnClick;

  FBtns[2].Align := alRight;
  FBtns[2].OnClick := NextMonthBtnClick;

  FBtns[3].Align := alRight;
  FBtns[3].OnClick := NextYearBtnClick;

  FBtns[1].Left := 20;
  FBtns[2].Left := 80;
  FBtns[3].Left := 100;

  FCaptBtn := TsSpeedButton.Create(Self);
  FCaptBtn.Enabled := False;
  FCaptBtn.Align := alClient;
  FCaptBtn.Flat := True;
  FCaptBtn.Caption := '';
  FCaptBtn.DisabledKind := [];
  FCaptBtn.SkinData.SkinSection := s_Transparent;
  FCaptBtn.Parent := FDragBar;

  FDragBar.Parent := Self;

  FTodayBtn := TsSpeedButton.Create(Self);
  FTodayBtn.Visible := False;
  FTodayBtn.Align := alBottom;
  UpdateTitleHeight;

  FTodayBtn.Flat := True;
  FTodayBtn.Caption := s_Today + s_Space + DateToLongStr(Date);

  FTodayBtn.OnClick := TodayClick;
  FTodayBtn.Parent := Self;
end;


procedure TsMonthCalendar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


function TsMonthCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;


function MakeNewDate(Y, M, D: word): TDateTime;
begin
{$IFDEF DELPHI7UP}
  TryEncodeDate(Y, M, D, Result);
{$ELSE}
  Result := EncodeDate(Y, M, D);
{$ENDIF}
end;


function TsMonthCalendar.GetCellDate(ACol, ARow: Integer): TDateTime;
var
  DayNum, Y, M: integer;
begin
  DayNum := ThisDayNum(ACol, ARow);
  Y := Year;
  M := Month;
  if DayNum < 1 then begin
    if M = 1 then begin // Get previous month
      dec(Y);
      M := 12;
    end
    else
      dec(M);

    Result := MakeNewDate(Y, M, DaysPerMonth(Y, M) + DayNum);
  end
  else
    if DayNum > DaysThisMonth then begin
      inc(M);
      if M > 12 then begin
        inc(Y);
        M := 1;
      end;
      Result := MakeNewDate(Y, M, DayNum - DaysThisMonth);
    end
    else
      Result := MakeNewDate(Y, M, DayNum);
end;


function TsMonthCalendar.GetCellText(ACol, ARow: Integer): string;
var
  DayNum: Integer;
  Y, M: Word;
begin
  if ARow = 0 then
    if ShowWeeks and (ACol = 0) then
      Result := s_WeeksTitle
    else
      Result := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ShortDayNames[(FirstDay + ACol - Integer(ShowWeeks) + 1) mod 7 + 1]
  else
    if ShowWeeks and (ACol = 0) then begin
      if ARow <= FGrid.RowCount - 1 then begin
        Y := Year;
        M := Month;
        if ARow = 1 then
          DayNum := FMonthOffset + 7 - Integer(ShowWeeks)
        else
          DayNum := FMonthOffset + 1 - Integer(ShowWeeks) + (ARow - 1) * 7 + 1;

        if FirstDay = 0 then
          dec(DayNum); // If Monday - first

        if DayNum <= 0 then begin
          if M = 1 then begin // Previous year
            M := 12;
            dec(Y);
          end
          else
            dec(M);

          DayNum := DaysPerMonth(Y, M);
        end
        else
          if DayNum > DaysThisMonth then begin
            DayNum := DayNum - DaysThisMonth;
            inc(M);
            if M > 12 then begin
              inc(Y);
              M := 1;
            end;
          end;

        Result := IntToStr(WeekNum(EncodeDate(Y, M, DayNum)) + 1);
      end;
    end
    else
      Result := IntToStr(GetDayNum(ACol, ARow));
end;


function TsMonthCalendar.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;


function TsMonthCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ShownDate, AYear, AMonth, ADay);
  case Index of
    1:   Result := AYear;
    2:   Result := AMonth;
    3:   Result := ADay;
    else Result := -1;
  end;
end;


function TsMonthCalendar.GetDayNum(ACol, ARow: Integer): integer;
var
  DayNum, Y, M: integer;
begin
  DayNum := ThisDayNum(ACol, ARow);
  if DayNum < 1 then begin
    Y := Year;
    M := Month;
    if ARow <= 2 then begin
      if M = 1 then begin // Get previous month
        dec(Y);
        M := 12;
      end
      else
        dec(M);

      Result := DaysPerMonth(Y, M) + DayNum;
    end
    else begin
      if M = 12 then begin // Get previous month
        inc(Y);
        M := 1;
      end
      else
        inc(M);

      Result := DaysPerMonth(Y, M) + DayNum;
    end;
  end
  else
    if DayNum > DaysThisMonth then
      Result := DayNum - DaysThisMonth
    else
      Result := DayNum;
end;


procedure TsMonthCalendar.SetDate(const Index: Integer; const Value: TDateTime);
begin
  case Index of
    1: if FMaxDate <> Value then begin
      FMaxDate := Value;
      if Value <> 0 then
        if not (csLoading in ComponentState) and (CalendarDate > Value) then
          CalendarDate := Value;

      FGrid.UpdateStates;
    end;

    2: if FMinDate <> Value then begin
      FMinDate := Value;
      if Value <> 0 then
        if not (csLoading in ComponentState) and (CalendarDate < Value) then
          CalendarDate := Value;

      FGrid.UpdateStates;
    end;
  end;
  CalendarChanged := True;
//  UpdateCalendar;
end;


procedure TsMonthCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then begin
    DecodeDate(ShownDate, AYear, AMonth, ADay);
    case Index of
      1:
        if AYear <> Value then begin
          AYear := Value;
          CalendarChanged := True;
        end
        else
          Exit;

      2:
        if (Value <= 12) and (Value <> AMonth) then begin
          AMonth := Value;
          CalendarChanged := True;
          if ADay > DaysPerMonth(Year, Value) then
            ADay := DaysPerMonth(Year, Value);
        end
        else
          Exit;

      3:
        if (Value <= DaysThisMonth) and (Value <> ADay) then
          ADay := Value
        else
          Exit

      else
        Exit;
    end;
    ShownDate := MakeNewDate(AYear, AMonth, ADay);
    if ShownDate <> 0 then begin
      FUseCurrentDate := False;
      if FTravellingSelection then begin
        FDate := ShownDate;
        Change;
      end;
      if not Initializing then begin
        Initializing := True;
        CalendarUpdate(Index = 3);
        Initializing := False;
      end;
    end
    else
      ShownDate := FDate;
  end;
end;


procedure TsMonthCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsMonthCalendar.SetWeekends(Value: sConst.TDaysOfWeek);
begin
  if Value <> FWeekends then begin
    FWeekends := Value;
    if not (csLoading in ComponentState) then
      UpdateCalendar;
  end;
end;


procedure TsMonthCalendar.GoToShownDate(ToForward: boolean; AllowAnimation: boolean = False);
{$IFDEF DELPHI6UP}
var
  b: boolean;
  OldBmp: TBitmap;
//  RepaintNeeded: boolean;
//  PrevMonth,
  AYear, AMonth, ADay: word;
{$ENDIF}
begin
  if FTravellingSelection then
    CalendarDate := ShownDate;
{$IFDEF DELPHI6UP}
  b := FAnimated and AllowAnimation and ((SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation);
//  PrevMonth := Month;
  if (MinDate <> 0) and (ShownDate < MinDate) then begin
    ShownDate := MinDate;
    DecodeDate(ShownDate, AYear, AMonth, ADay);
    b := False;
//    RepaintNeeded := PrevMonth = AMonth;
  end
  else
    if (MaxDate <> 0) and (ShownDate > MaxDate) then begin
      ShownDate := MaxDate;
      DecodeDate(ShownDate, AYear, AMonth, ADay);
      b := False;
//      RepaintNeeded := PrevMonth = AMonth;
    end;

//  RepaintNeeded := True;

//  if not RepaintNeeded then
//    Exit;

  if b then begin
    CalendarChanged := True;
    UpdateCalendar;
    OldBmp := CreateBmp32(FGrid);
    FGrid.PaintTo(OldBmp.Canvas, 0, 0);
    DoChangeAnimated(ToForward, OldBmp);
  end
  else
{$ENDIF} // DELPHI6UP
  begin
    CalendarChanged := True;
    UpdateCalendar;
  end;
end;


function TsMonthCalendar.IsWeekend(ACol: Integer): Boolean;
begin
  Result := TCalDayOfWeek((FirstDay + ACol - Integer(ShowWeeks)) mod 7) in FWeekends;
end;


procedure TsMonthCalendar.SetStartOfWeek(Value: TCalDayOfWeek);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    if not (csLoading in ComponentState) then
      UpdateCalendar;
  end;
end;


procedure TsMonthCalendar.SetTitleBtnsWidth(const Value: integer);
begin
  if FTitleBtnsWidth <> Value then begin
    FTitleBtnsWidth := Value;
    if not (csLoading in ComponentState) then
      UpdateNavBtns;
  end;
end;


procedure TsMonthCalendar.SetTitleHeight(const Value: integer);
begin
  if FTitleHeight <> Value then begin
    FTitleHeight := Value;
    UpdateTitleHeight;
  end;
end;


function acChangeMonth(aDelta: Integer; var aDate: TDateTime): Boolean;
var
  AYear, AMonth, ADay: Word;
  NewDate: TDateTime;
  CurDay: Integer;
begin
  DecodeDate(aDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if aDelta > 0 then
    ADay := DaysPerMonth(AYear, AMonth)
  else
    ADay := 1;

  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + aDelta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := DaysPerMonth(AYear, AMonth);

  aDate := MakeNewDate(AYear, AMonth, ADay);
  Result := aDate <> 0;
end;


function acChangeYear(aDelta: Integer; var aDate: TDateTime): Boolean;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(aDate, AYear, AMonth, ADay); 
  if IsLeapYear(aYear) and (aMonth = 2) and (aDay = 29) then
    aDay := 28;

  aDate := MakeNewDate(AYear + aDelta, AMonth, ADay);
  Result := aDate <> 0;
end;


procedure TsMonthCalendar.ChangeMonth(Delta: Integer; AllowAnimation: boolean = False);
begin
  if FAllowChangeMonth then
    if acChangeMonth(Delta, ShownDate) then begin
      State := State + [csMonthChanging];
      GoToShownDate(Delta > 0, AllowAnimation);
      State := State - [csMonthChanging];
    end;
end;


procedure TsMonthCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;


procedure TsMonthCalendar.NextMonth;
begin
  ChangeMonth(1);
end;


procedure TsMonthCalendar.NextYear;
begin
  ChangeYear(1);
end;


procedure TsMonthCalendar.PrevYear;
begin
  ChangeYear(-1);
end;


procedure TsMonthCalendar.CalendarUpdate(DayOnly: Boolean);
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDateTime;
  d1, d2: integer;
begin
  if [csLoading, csDestroying] * ComponentState = [] then begin
    FUpdating := True;
    try
      DecodeDate(ShownDate, AYear, AMonth, ADay);
      FirstDate := MakeNewDate(AYear, AMonth, 1);
      if FirstDate <> 0 then begin
        FMonthOffset := 2 - (DayOfWeek(FirstDate) - (FirstDay + 1) + 7) mod 7;
        if FMonthOffset in [1, 2] then
          FMonthOffset := FMonthOffset - 7;

        d1 := (ADay - FMonthOffset) mod 7 + Integer(ShowWeeks);
        d2 := (ADay - FMonthOffset) div 7 + 1;
        FGrid.MoveColRow(d1, d2, False, False);

        if FCalendarChanged then begin
          CalendarChanged := False;
          FGrid.UpdateStates;
          if not DayOnly then begin
            FCaptBtn.Caption := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[AMonth] + s_Space + IntToStr(AYear);
            if FGrid.HandleAllocated then
              RedrawWindow(FGrid.Handle, nil, 0, RDWA_ALLNOW);
          end;
        end;
      end;
    finally
      FUpdating := False;
    end;
  end;
end;


procedure TsMonthCalendar.UpdateCalendar;
begin
  CalendarUpdate(False);
end;


function TsMonthCalendar.FirstDay: integer;
var
  A: array [0..1] of char;
begin
  if FStartOfWeek = dowLocaleDefault then begin
    GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, A, SizeOf(A));
    Result := Ord(A[0]) - Ord(ZeroChar);
  end
  else
    Result := Ord(FStartOfWeek);
end;


procedure TsMonthCalendar.PrevYearBtnClick(Sender: TObject);
begin
  ChangeYear(-1, True);
end;


procedure TsMonthCalendar.RefreshAcceptedDates;
begin
  FGrid.UpdateStates;
  FGrid.Repaint;
end;


procedure TsMonthCalendar.NextYearBtnClick(Sender: TObject);
begin
  ChangeYear(1, True);
end;


procedure TsMonthCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  ChangeMonth(-1, True);
end;


procedure TsMonthCalendar.NextMonthBtnClick(Sender: TObject);
begin
  ChangeMonth(1, True);
end;


procedure TsMonthCalendar.TopPanelDblClick(Sender: TObject);
begin
  CalendarDate := Date;
end;


procedure TsMonthCalendar.WndProc(var Message: TMessage);
var
  i, fHeight: integer;
begin
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            if (PopMenu <> nil) and SkinData.SkinManager.SkinnedPopups then
              Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, False);

            UpdateCalendar;
            FGrid.Repaint;
            if ShowTitle then
              SendMessage(FDragBar.Handle, Message.Msg, Message.WParam, Message.LParam);
          end;

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then begin
            if (PopMenu <> nil) and SkinData.SkinManager.SkinnedPopups then
              Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, True);

            UpdateCalendar;
            FGrid.Repaint;
            if ShowTitle then
              SendMessage(FDragBar.Handle, Message.Msg, Message.WParam, Message.LParam);
          end;
      end;

    CM_FONTCHANGED:
      if (Skindata.SkinManager <> nil) and (Skindata.SkinManager.Options.ScaleMode <> smVCL) then begin
        fHeight := Skindata.SkinManager.ScaleInt(-13);
        for i := 0 to 3 do
          FBtns[i].Font.Height := fHeight;
      end;

    CM_BIDIMODECHANGED:
      for i := 0 to 3 do
        FBtns[i].Caption := CaptArray[BidiMode = bdRightToLeft, i];
  end;
end;


procedure TsMonthCalendar.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    0: if FAllowChangeMonth <> Value then begin
      FAllowChangeMonth := Value;
      UpdateNavBtns;
      if Value then
        FDragBar.Cursor := crHandPoint
      else
        FDragBar.Cursor := Cursor;

      Invalidate;
    end;

    1: if FShowCurrentDate <> Value then begin
      FShowCurrentDate := Value;
      SkinData.Invalidate;
    end;

    2: if FShowSelectAlways <> Value then begin
      FShowSelectAlways := Value;
      CalendarUpdate(False);//True);
    end;

    3: if FShowTitle <> Value then begin
      FShowTitle := Value;
      FDragBar.Visible := Value;
      if Value then
        FDragBar.Parent := Self
      else
        FDragBar.Parent := nil;
    end;

    4: if FShowTodayBtn <> Value then begin
      FShowTodayBtn := Value;
      if Value then
        FTodayBtn.Parent := Self
      else
        FTodayBtn.Parent := nil;

      CalendarUpdate(False);
      FTodayBtn.SkinData.Invalidate;
    end;

    5: if FShowWeeks <> Value then begin
      FShowWeeks := Value;
      CalendarChanged := True;
      FGrid.DefaultColWidth := FGrid.Width div (7 + integer(ShowWeeks));
      UpdateProps;
      Invalidate;
    end;

    6: if FShowYearBtns <> Value then begin
      FShowYearBtns := Value;
      UpdateNavBtns;
    end;

    7: if FShowMonthBtns <> Value then begin
      FShowMonthBtns := Value;
      UpdateNavBtns;
    end;

    8: if Value <> FUseCurrentDate then begin
      FUseCurrentDate := Value;
      if Value then begin
        CalendarChanged := True;
        FDate := Date;
        CalendarUpdate(True);
      end;
    end;

    9: if FDimUnacceptedCells <> Value then begin
      CalendarChanged := True;
      FDimUnacceptedCells := Value;
      CalendarUpdate(True);
      SkinData.Invalidate;
    end;
  end;
end;


procedure TsMonthCalendar.SetCalendarChanged(const Value: boolean);
begin
  if ShownDate <> 0 then
    FCalendarChanged := Value;
end;


procedure TsMonthCalendar.SetCalendarDate(const Value: TDateTime);
var
  Y, M, D: Word;
  FullUpdate: boolean;
begin
  if (FDate <> Value) and IsValidDate(Value) then begin
    DecodeDate(Value, Y, M, D);
    if not FAllowChangeMonth and ((Y <> Year) or (M <> Month)) then
      Exit;

    FullUpdate := (Y <> Year) or (M <> Month);
    if M <> Month then
      State := State + [csMonthChanging];

    FDate := MakeNewDate(Y, M, D);
    if FDate <> 0 then begin
      FDate := Value;
      ShownDate := FDate;
      Change;
      if FullUpdate then
        CalendarChanged := True;

      if not (csMonthChanging in State) then begin
        CalendarUpdate(not FullUpdate);
        State := State - [csMonthChanging];
      end
      else
        CalendarUpdate(not FullUpdate);
    end;
  end;
end;


type
  TAccessCommonData = class(TsCommonData);

procedure TsMonthCalendar.Loaded;
begin
  inherited;
end;


procedure TsMonthCalendar.CreateWnd;
var
  i: integer;
begin
  inherited;
  ShownDate := FDate;
  if FShowtodayBtn then
    FTodayBtn.Visible := True;

  ShownDate := FDate;
  FCalendarChanged := True;
  UpdateProps;
  UpdateNavBtns;
  if Assigned(FDragBar) and FDragBar.Visible then begin
    TAccessCommonData(FDragBar.SkinData).FSkinManager := TAccessCommonData(SkinData).FSkinManager;
    for i := 0 to 3 do
      TAccessCommonData(FBtns[i].SkinData).FSkinManager := TAccessCommonData(SkinData).FSkinManager;
  end;
end;


procedure TsMonthCalendar.MakePopup;
var
  y, m, d: word;
  si: TMenuItem;
  miRoot: TMenuItem;
  yName, mName: string;
  i, minYear, minMonth, maxYear, maxMonth: integer;

  procedure MakeMenusForYear(aMinYear, aMaxYear: integer; Items: TMenuItem);
  var
    i, j, minM, maxM: integer;
    mi: TMenuItem;
  begin
    for i := aMinYear to aMaxYear do begin
      if (i < minYear) or (i > maxYear) then
        Continue;

      mi := TMenuItem.Create(Self);
      mi.Tag := i;
      mi.Caption := IntToStr(i);
      if Items = PopMenu.Items then
        mi.Caption := mi.Caption;

      if i = Year then begin
        mi.Default := True;
        mi.Checked := True;
      end;

      minM := minMonth;
      if minMonth > 1 then
        minMonth := 1;

      if i = maxYear then begin
        maxM := maxMonth;
        if maxMonth < 12 then
          maxMonth := 12;
      end
      else
        maxM := 12;

      for j := minM to maxM do begin
        si := TMenuItem.Create(Self);
        si.Caption := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[j];
        si.Tag := j;
        if mi.Default and (j = Month) then begin
          si.Default := True;
          si.Checked := True;
        end;
        si.OnClick := MenuClick;
        mi.Add(si);
      end;
      Items.Add(mi);
    end;
  end;

  procedure CheckItem(AItem: TMenuItem);
  var
    i: integer;
    b: boolean;
  begin
    if AItem.Count = 0 then begin
      AItem.Checked := (AItem.Parent.Caption = yName) and (AItem.Caption = mName);
      AItem.Default := AItem.Checked;
    end
    else begin
      b := False;
      for i := 0 to AItem.Count - 1 do begin
        CheckItem(AItem.Items[i]);
        if AItem.Items[i].Checked then
          b := True;
      end;
      AItem.Checked := b;
      AItem.Default := b;
    end;
  end;

begin
  mName := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[Month];
  yName := IntToStr(Year);
  PopMenu.Free;

  PopMenu := TPopupMenu.Create(Self);
  PopMenu.AutoHotkeys := maManual;

  minYear := (Year - 4) - 10 * 5;
  minMonth := 1;
  if MinDate <> 0 then begin
    DecodeDate(MinDate, y, m, d);
    if (y > minYear) or (y = minYear) and (m > minMonth) then begin // new start values
      minYear := y;
      minMonth := m;
    end;
  end;

  maxYear := (Year - 4) + 50;
  maxMonth := 12;

  if MaxDate <> 0 then begin
    DecodeDate(MaxDate, y, m, d);
    if (y < maxYear) or (y = maxYear) and (m < maxMonth) then begin // new finish values
      maxYear := y;
      maxMonth := m;
    end;
  end;

  i := minYear;
  while i < (Year - 4) do begin
    miRoot := TMenuItem.Create(Self);
    miRoot.Caption := IntToStr(i) + '..' + IntToStr(min(Year - 5, i + 9));
    MakeMenusForYear(i, min(Year - 5, i + 9), miRoot);
    PopMenu.Items.Add(miRoot);
    inc(i, 10);
  end;
  if MaxYear = MinYear then begin
    if MaxMonth - MinMonth > 0 then
      for i := MinMonth to MaxMonth do begin
        si := TMenuItem.Create(Self);
        si.Caption := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}LongMonthNames[i];
        si.Tag := i;
        if i = Month then begin
          si.Default := True;
          si.Checked := True;
        end;
        si.OnClick := MenuClick;
        PopMenu.Items.Add(si);
      end
  end
  else
    MakeMenusForYear(Year - 4, Year + 5, PopMenu.Items);

  i := Year + 6;
  while i < maxYear do begin
    miRoot := TMenuItem.Create(Self);
    miRoot.Caption := IntToStr(i) + '..' + IntToStr(min(maxYear, i + 9));
    MakeMenusForYear(i, min(maxYear, i + 9), miRoot);
    PopMenu.Items.Add(miRoot);
    inc(i, 10);
  end;

  if SkinData.Skinned and SkinData.SkinManager.SkinnedPopups then
    Skindata.SkinManager.SkinableMenus.HookPopupMenu(PopMenu, True);
end;


procedure TsMonthCalendar.MenuClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(Sender);
  Initializing := True;
  Year := mi.Parent.Tag;
  Initializing := False;
  CalendarChanged := True;
  Month := mi.Tag;
end;


function TsMonthCalendar.ThisDayNum(ACol, ARow: integer): integer;
begin
  Result := FMonthOffset + ACol - Integer(ShowWeeks) + (ARow - 1) * 7;
end;


procedure TsMonthCalendar.TitleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FAllowChangeMonth then begin
    MakePopup;
    P := acMousePos;
    inc(SysPopupCount);
    PopMenu.PopupComponent := Self;
    PopMenu.Popup(P.X, P.Y);
    PopMenu.PopupComponent := nil;
    dec(SysPopupCount);
  end;
end;


procedure TsMonthCalendar.TodayClick(Sender: TObject);
begin
  CalendarDate := Date;
  if Parent is TsPopupCalendar then
    TsPopupCalendar(Parent).CalendarClick(FGrid.Row, FGrid.Col);
end;


procedure TsMonthCalendar.UpdateProps;
begin
  Initializing := True;
  if FShowTodayBtn then
    FTodayBtn.Parent := Self
  else
    FTodayBtn.Parent := nil;

  if FGrid <> nil then begin
    FGrid.ColCount := 7 + integer(ShowWeeks);
    FGrid.FixedCols := integer(ShowWeeks);
    UpdateCalendar;
  end;
  Initializing := False;
end;


procedure TsMonthCalendar.UpdateTitleHeight;
var
  i, Size: integer;
begin
  Size := FTitleHeight;
  FDragBar.Height := Size;
  FTodayBtn.Height := Size;
  for i := 0 to 3 do
    FBtns[i].Width := Size;
end;


procedure TsMonthCalendar.UpdateNavBtns;
var
  i, bWidth: integer;
begin
  FBtns[0].Visible := FAllowChangeMonth and ShowYearBtns;
  FBtns[1].Visible := FAllowChangeMonth and ShowMonthBtns;
  FBtns[2].Visible := FAllowChangeMonth and ShowMonthBtns;
  FBtns[3].Visible := FAllowChangeMonth and ShowYearBtns;
  bWidth := FTitleBtnsWidth;
  for i := 0 to 3 do
    FBtns[i].Width := bWidth;
end;


function TsMonthCalendar.MousePosToDate(X, Y: Integer): TDateTime;
var
  ACol, ARow: integer;
begin
  if not PtInRect(FGrid.BoundsRect, Point(X, Y)) then
    Result := 0
  else begin
    FGrid.MouseToCell(X - FGrid.Left, Y - FGrid.Top, ACol, ARow);
    if ARow = 0 then
      Result := 0
    else
      Result := GetCellDate(ACol, ARow);
  end;
end;


function TsMonthCalendar.IsValidDate(Date: TDateTime): boolean;
begin
  if ((MinDate <> 0) and (Date < MinDate)) or ((MaxDate <> 0) and (Date > MaxDate)) then
    Result := False
  else
    Result := True;
end;


function TsCalendGrid.CellsStates: TStatesRows;
begin
  if Length(FCellsStates) = 0 then
    UpdateStates;

  Result := FCellsStates;
end;


procedure TsCalendGrid.Click;
var
  DayNum: integer;
  CanAccept: boolean;
  TheCellText: string;
  NewDate: TDateTime;
begin
  if not FOwner.FUpdating then begin
    inherited Click;
    if not FOwner.ShowWeeks or (TrySelectCol > 0) then begin
      NewDate := FOwner.CellDate[TrySelectCol, TrySelectRow];
      if FOwner.DimUnacceptedCells then
        CanAccept := (CellsStates[TrySelectRow, TrySelectCol] = 1)
      else begin
        CanAccept := True;
        if Assigned(FOwner.OnAcceptDate) then
          FOwner.OnAcceptDate(FOwner, NewDate, CanAccept);
      end;

      if CanAccept then begin
        TheCellText := FOwner.CellText[TrySelectCol, TrySelectRow];
        if TheCellText <> '' then begin
          DayNum := FOwner.ThisDayNum(TrySelectCol, TrySelectRow);
          if (FOwner.FAllowChangeMonth or ((DayNum >= 1) and (DayNum <= FOwner.DaysThisMonth))) then begin
            if not (FOwner.Parent is TsPopupCalendar) then begin
              if not CurrentMonth(FOwner.CellDate[TrySelectCol, TrySelectRow]) then
                if DayNum < 1 then
                  FOwner.PrevMonth
                else
                  if DayNum > FOwner.DaysThisMonth then
                    FOwner.NextMonth;

              FOwner.Day := StrToInt(TheCellText);
            end;
            FOwner.Initializing := True;
            FOwner.CalendarDate := FOwner.ShownDate;
            FOwner.Initializing := False;
          end
          else
            FOwner.UpdateCalendar;
        end;
      end;
    end;
    if Assigned(FOwner.OnClick) then
      FOwner.OnClick(FOwner);
  end;
end;


constructor TsCalendGrid.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TsMonthCalendar(AOwner);
  Ctl3D := False;
  BorderStyle := bsNone;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 8;
  ScrollBars := ssNone;
  MouseCell := Point(-1, -1);
  MouseDate := 0;
  Options := [];
  Align := alClient;
  Tag := ExceptTag;
end;


procedure TsCalendGrid.CreateWnd;
begin
  inherited;
  DefaultDrawing := False;
  Clicked := False;
end;


function TsCalendGrid.CurrentMonth(aDate: TDateTime): boolean;
var
  Y, M, d: word;
begin
  DecodeDate(aDate, Y, M, d);
  Result := (Y = FOwner.Year) and (M = FOwner.Month);
end;


procedure TsCalendGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  R: TRect;
  c: TColor;
  Y, M, D: word;
  Flags: Cardinal;
  TheText: string;
  aDate: TDateTime;
  BGInfo: TacBGInfo;
  RealNum, DayNum: integer;
  bDimmed, CanAccept: boolean;

  function IsToday: boolean;
  begin
    if (ARow > 2) and (RealNum < 10) or (ARow > 4) and (RealNum < 16) or (ARow < 3) and (RealNum > 20) or FOwner.ShowWeeks and (ACol < 1) then
      Result := False
    else
      Result := (ARow <> 0) and (TheText <> '') and (EncodeDate(FOwner.Year, FOwner.Month, StrToInt(TheText)) = Date)
  end;

begin
  if ARow < RowCount - 1 then begin
    TheText := FOwner.CellText[ACol, ARow];
    RealNum := FOwner.GetDayNum(ACol, ARow);
    R := ARect;
    if ACol = 6 + Integer(FOwner.ShowWeeks) then
      R.Right := Width;

    if ARow = 6 then
      R.Bottom := Height;

    BGInfo.PleaseDraw := False;
    BGInfo.Offset := MkPoint;
    GetBGInfo(@BGInfo, FOwner);
    if (BGInfo.BgType = btCache) and not BGInfo.Bmp.Empty then
      BitBlt(Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top + BGInfo.Offset.Y, SRCCOPY)
    else
      FillDC(Canvas.Handle, R, BGInfo.Color);

    R := ARect;
    Canvas.Font.Assign(Font);
    if gdSelected in AState then begin
      DecodeDate(FOwner.FDate, Y, M, D);
      if (Y <> FOwner.Year) or (M <> FOwner.Month) then
        AState := AState - [gdSelected];

      if not FOwner.ShowSelectAlways and not Focused then
        AState := AState - [gdSelected];
    end;

    if FOwner.ShowCurrentDate and (gdSelected in AState) then begin
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clBlack;
    end
    else
      Canvas.Font.Style := [];

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Style := [];
    if not (FOwner.ShowWeeks and (ACol = 0)) and FOwner.ShowCurrentDate and IsToday and (ACol <> 7 + Integer(FOwner.ShowWeeks)) then begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style := psClear;
      if FOwner.SkinData.Skinned then
        Canvas.Brush.Color := BlendColors(FOwner.WeekendColor, FOwner.SkinData.SkinManager.GetGlobalColor, 77)
      else
        Canvas.Brush.Color := BlendColors(FOwner.WeekendColor, ColortoRGB(clBtnFace), 77);

      acgpRoundRect(Canvas.Handle, R.Left, ARect.Top, WidthOf(R), HeightOf(ARect), 3, 0, TColor($FF000000 or Cardinal(Canvas.Brush.Color)), clNone);
      Canvas.Font.Color := clWhite;
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Style := [fsBold];
    end
    else begin
      if FOwner.IsWeekend(ACol{, ARow}) then
        if FOwner.SkinData.Skinned and (FOwner.WeekendColor = clRed) then
          if FOwner.SkinData.CommonSkinData.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color = 0 then
            Canvas.Font.Color := BlendColors(clRed, FOwner.SkinData.CommonSkinData.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color, 196)
          else
            Canvas.Font.Color := BlendColors(clRed, FOwner.SkinData.CommonSkinData.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color, 127) // acColorToRGB(slBtnRedText)
        else
          Canvas.Font.Color := FOwner.WeekendColor
      else
        with FOwner.SkinData do
          if FOwner.ShowWeeks and (ACol = 0) then begin
            if ARow = 0 then begin
              Canvas.Font.Name := 'Small Fonts';
              Canvas.Font.Size := 6;
            end
            else
              Canvas.Font.Style := [fsBold];

            if Skinned and Assigned(SkinManager) then
              Canvas.Font.Color := BlendColors(FOwner.SkinData.CommonSkinData.gd[FOwner.SkinData.SkinIndex].Props[0].Color, FOwner.SkinData.CommonSkinData.gd[FOwner.SkinData.SkinIndex].Props[0].FontColor.Color, 196)
            else
              Canvas.Font.Color := clGrayText;

            Canvas.Pen.Style := psSolid;
            Canvas.Pen.Color := Canvas.Font.Color;
            acgpUtils.acgpDrawLine(Canvas.Handle, ARect.Right - 1, ARect.Top, ARect.Right - 1, ARect.Bottom, TColor($FF000000 or Cardinal(Canvas.Font.Color)));
            dec(ARect.Right, 3);
          end
          else
            if Skinned and Assigned(SkinManager) and (SkinManager.GetGlobalFontColor <> clFuchsia) then
              with CommonSkinData do
                if Skinned then
                  if (gd[SkinIndex].BorderIndex < 0) and (gd[SkinIndex].Props[0].Transparency = 100) then
                    Canvas.Font.Color := GetControlFontColor(Parent, SkinManager)
                  else
                    Canvas.Font.Color := gd[SkinIndex].Props[0].FontColor.Color
                else
                  Canvas.Font.Color := SkinManager.GetGlobalFontColor
            else
              Canvas.Font.Color := clWindowText;

      if ARow <> 0 then begin
        DayNum := FOwner.ThisDayNum(ACol, ARow);
        if (DayNum <= 0) or (DayNum > FOwner.DaysThisMonth) then
          if FOwner.SkinData.Skinned then
            Canvas.Font.Color := BlendColors(Canvas.Font.Color, FOwner.SkinData.SkinManager.GetGlobalColor, 127)
          else
            Canvas.Font.Color := BlendColors(ColorToRGB(Canvas.Font.Color), ColorToRGB(FOwner.Color), 127);
      end;
    end;

    CanAccept := True;
    if (ARow > 0) and (TheText <> '') and not (FOwner.ShowWeeks and (ACol = 0)) then begin
      aDate := FOwner.GetCellDate(ACol, ARow);
      bDimmed := False;
      if FOwner.DimUnacceptedCells {and Assigned(FOwner.OnAcceptDate)} then begin
        CanAccept := (CellsStates[ARow][ACol] = 1);
        if not CanAccept then begin
          Canvas.Font.Style := [];
//          Canvas.Font.Color  := SysColorToSkin(clGrayText,  FOwner.SkinData.SkinManager);
          Canvas.Brush.Color := acColorToRGB{SysColorToSkin}(clBtnShadow, FOwner.SkinData.SkinManager);
          bDimmed := True
        end;
      end;

      if Assigned(FOwner.FOnDrawDay) or Assigned(FOwner.FOnGetCellParams) then begin
        c := clFuchsia;
        if Assigned(FOwner.FOnDrawDay) then
          FOwner.FOnDrawDay(TsPopupCalendar(FOwner.Parent).FEditor, aDate, Canvas.Font, c)
        else begin
          FOwner.FOnGetCellParams(Self, aDate, Canvas.Font, c);
          Canvas.Font.Color := acColorToRGB(Canvas.Font.Color);
        end;
        if c <> clFuchsia then begin
          Canvas.Brush.Color := acColorToRGB(c);
          Canvas.Brush.Style := bsSolid;
        end;
      end;

      if (not FOwner.IsValidDate(aDate) or not CanAccept) {and not bDimmed} then
        Canvas.Font.Color := BlendColors(acColorToRGB(Canvas.Font.Color), acColorToRGB(Canvas.Brush.Color), iff(bDimmed, 128, 77));
    end;

    if (ARow = 0) and Assigned(FOwner.FOnGetDayOfWeekParams) then
      if (ACol > 0) or not FOwner.ShowWeeks then begin
        c := Canvas.Brush.Color;
        FOwner.FOnGetDayOfWeekParams(TsPopupCalendar(FOwner.Parent).FEditor, ACol - integer(FOwner.ShowWeeks), Canvas.Font, c);
        Canvas.Brush.Color := c;
      end;

    Flags := DT_VCENTER or DT_CENTER or DT_ALPHATEXT * Cardinal(FOwner.SkinData.TranspMode <> TM_OPAQUE);
    if TheText = '' then
      Exit;

    if gdSelected in AState then begin
      Canvas.Font.Style := [fsBold];
      InflateRect(R, -2, -2);
      if FOwner.SkinData.Skinned then begin
        PaintItem(FOwner.SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection], BGInfoToCI(@BGInfo),
                  True, integer(Focused or FOwner.Focused), R, Point(0, 0), Canvas.Handle, FOwner.SkinData.CommonSkinData);

        Canvas.Font.Color := FOwner.SkinData.SkinManager.GetHighLightFontColor(Focused or FOwner.Focused);
      end
      else begin
        FillDC(Canvas.Handle, R, clHighlight);
        Canvas.Font.Color := clHighlightText;
      end;
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psClear;
    end;
    if not CanAccept or (FOwner.SkinData.TranspMode <= 0) then
      Canvas.TextRect(ARect, ARect.Left + (ARect.Right - ARect.Left - Canvas.TextWidth(TheText)) div 2,
                      ARect.Top + (ARect.Bottom - ARect.Top - Canvas.TextHeight(TheText)) div 2, TheText)
    else
      acDrawText(Canvas.Handle, TheText, ARect, Flags);

    if not FOwner.SkinData.Skinned then
      if (gdSelected in AState) and (Canvas.Brush.Style <> bsSolid) and Focused then begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Style := psClear;
        DrawFocusRect(Canvas.Handle, R);
      end;
  end;
end;


procedure TsCalendGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  d: TDateTime;
  CanAccept: boolean;
begin
  case Key of
    VK_UP, VK_DOWN: begin
      Shift := Shift - [ssCtrl];
      if VK_UP = Key then begin
        if FOwner.IsValidDate(FOwner.CalendarDate - 7) then
          FOwner.CalendarDate := FOwner.CalendarDate - 7;
      end
      else
        if FOwner.IsValidDate(FOwner.CalendarDate + 7) then
          FOwner.CalendarDate := FOwner.CalendarDate + 7;

      Exit;
    end;

    VK_LEFT, VK_SUBTRACT:
      if Shift = [] then begin
        if FOwner.IsValidDate(FOwner.CalendarDate - 1) then
          FOwner.CalendarDate := FOwner.CalendarDate - 1;

        Exit;
      end;

    VK_RIGHT, VK_ADD:
      if Shift = [] then begin
        if FOwner.IsValidDate(FOwner.CalendarDate + 1) then
          FOwner.CalendarDate := FOwner.CalendarDate + 1;

        Exit;
      end;

    VK_NEXT: begin
      if ssCtrl in Shift then
        FOwner.NextYear
      else
        FOwner.NextMonth;

      Exit;
    end;

    VK_PRIOR: begin
      if ssCtrl in Shift then
        FOwner.PrevYear
      else
        FOwner.PrevMonth;

      Exit;
    end;

    VK_RETURN:
      if FOwner.Parent is TsPopupCalendar then
        with TsPopupCalendar(FOwner.Parent) do begin
          if FEditor <> nil then begin
            d := sMonthCalendar1.CalendarDate;
            if not TsPopupCalendar(FOwner.Parent).IsValidDate(d) then
              Exit;

            CanAccept := True;
            if {not FInitializing and }Assigned(TsDateEdit(FEditor).OnAcceptDate) then
              if FOwner.DimUnacceptedCells then
                CanAccept := (CellsStates[Row][Col] = 1)
              else
                TsDateEdit(FEditor).OnAcceptDate(FEditor, d, CanAccept);

            if CanAccept then begin
              TsCustomDateEdit(FEditor).Date := d;
              if Assigned(TsCustomDateEdit(FEditor).OnChange) then
                TsCustomDateEdit(FEditor).OnChange(TsCustomDateEdit(FEditor));
            end;
            FEditor.SetFocus;
            if FEditor.AutoSelect then
              FEditor.SelectAll;
          end;
          Close;
          CloseUp;
        end;

    VK_ESCAPE:
      if FOwner.Parent is TsPopupCalendar then
        with TsPopupCalendar(FOwner.Parent) do begin
          if FEditor.Visible then
            FEditor.SetFocus;

          Close;
          CloseUp;
        end;
  end;
  inherited KeyDown(Key, Shift);
end;


procedure TsCalendGrid.KeyPress(var Key: Char);
begin
  if CharInSet(AnsiChar(Key), ['T', 't']) then begin
    FOwner.CalendarDate := Trunc(Now);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;


procedure TsCalendGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;


procedure TsCalendGrid.Paint;
begin
  if [csDestroying, csLoading] * ComponentState = [] then
    inherited;
end;


function TsCalendGrid.SelectCell(ACol, ARow: Integer): Boolean;
var
  NewDate: TDateTime;
  CanAccept: boolean;
begin
  TrySelectCol := ACol;
  TrySelectRow := ARow;
  if not FOwner.FUpdating and FOwner.FReadOnly or (FOwner.CellText[ACol, ARow] = '') then
    Result := False
  else begin
    NewDate := FOwner.GetCellDate(ACol, ARow);
    if FOwner.FSelectAllowedDateOnly and (FOwner.MinDate <> 0) and (NewDate < FOwner.MinDate) or (FOwner.MaxDate <> 0) and (NewDate > FOwner.MaxDate) then
      Result := False
    else
      if not FOwner.Initializing then begin
        if FOwner.DimUnacceptedCells then
          CanAccept := (CellsStates[ARow][ACol] = 1)
        else begin
          CanAccept := True;
          if Assigned(FOwner.OnAcceptDate) then
            FOwner.OnAcceptDate(FOwner, NewDate, CanAccept);
        end;

        if CanAccept then
          Result := inherited SelectCell(ACol, ARow)
        else
          Result := False;
      end
      else
        Result := True;
  end;
end;


procedure TsCalendGrid.SetMouseCell(const p: TPoint);
begin
  if (MouseCell.X <> p.X) or (MouseCell.Y <> p.Y) then begin
    MouseCell := p;
    if (FOwner.ShowWeeks and (p.X = 0)) or (p.Y <= 0) or (HeightOf(CellRect(MouseCell.X, MouseCell.Y)) < DefaultRowHeight) then
      MouseDate := 0
    else
      MouseDate := FOwner.GetCellDate(MouseCell.X, MouseCell.Y);
  end;
end;


procedure TsCalendGrid.SetMouseDate(const Value: TDateTime);
begin
  if Value <> MouseDate then begin
    if (FMouseDate <> 0) and Assigned(FOwner.FOnDateMouseLeave) then
      FOwner.FOnDateMouseLeave(FOwner, FMouseDate);

    FMouseDate := Value;
    if (FMouseDate <> 0) and Assigned(FOwner.FOnDateMouseEnter) then
      FOwner.FOnDateMouseEnter(FOwner, FMouseDate);
  end;
end;


procedure TsCalendGrid.UpdateStates;
var
  d: TDateTime;
  CanAccept: boolean;
  x, y, c, r, minX, minY, maxY: integer;
begin
  c := ColCount;
  r := RowCount;
  SetLength(FCellsStates, r);
  if FOwner.DimUnacceptedCells {and Assigned(FOwner.OnAcceptDate)} then begin
    minY := 1;
    maxY := 6;
    for y := 0 to r - 1 do begin
      SetLength(FCellsStates[y], c);
      if (y < minY) or (y > maxY) then
        FillMemory(@(FCellsStates[y][0]), c, 1)
      else begin
        minX := c - 7;
        for x := minX to c - 1 do begin
          d := FOwner.GetCellDate(x, y);
          if (FOwner.MinDate <> 0) and (d < FOwner.MinDate) then
            CanAccept := False
          else
            if (FOwner.MaxDate <> 0) and (d > FOwner.MaxDate) then
              CanAccept := False
            else begin
              CanAccept := True;
              if Assigned(FOwner.OnAcceptDate) then
                if (c <> 8) or (x <> 0) then
                  if FOwner.Parent is TsPopupCalendar then
                    TsDateEdit(TsPopupCalendar(FOwner.Parent).FEditor).OnAcceptDate(TsDateEdit(TsPopupCalendar(FOwner.Parent).FEditor), d, CanAccept)
                  else
                    FOwner.OnAcceptDate(FOwner, d, CanAccept);
            end;

          FCellsStates[y][x] := integer(CanAccept);
        end;
      end;
    end;
  end
  else
    for y := 0 to r - 1 do begin
      SetLength(FCellsStates[y], c);
      FillMemory(@(FCellsStates[y][0]), c, 1);
    end;
end;


procedure TsCalendGrid.WMMouseActivate(var Message: TMessage);
begin
  if FOwner.Parent is TsPopupCalendar then
    Message.Result := MA_NOACTIVATE
  else
    inherited;
end;


procedure TsCalendGrid.WMSize(var Message: TWMSize);
begin
  DefaultColWidth  := Message.Width  div (7 + integer(FOwner.ShowWeeks));
  DefaultRowHeight := Message.Height div 7;
end;


{$IFNDEF DELPHI6UP}
  {$HINTS OFF}
type
  TAccessStdGrid = class(TCustomControl)
  private
    FAnchor:        TGridCoord;
    FBorderStyle:   TBorderStyle;
    FCanEditModify: Boolean;
    FColCount:      Longint;
    FColWidths,
    FTabStops:      Pointer;
    FCurrent:       TGridCoord;
  end;
{$ENDIF}


procedure TsCalendGrid.WndProc(var Message: TMessage);
var
  DC: HDC;
  R: TRect;
  BGInfo: TacBGInfo;
  SaveIndex, X, Y: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_ERASEBKGND, CM_MOUSEENTER: ;

    WM_TIMER: ; // Do not scroll when mouse pressed and moved

    CM_MOUSELEAVE: MouseDate := 0;

    WM_MOUSEMOVE: begin
      MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
      SetMouseCell(Point(X, Y));
      MouseMove(GetShiftState, TWMMouse(Message).XPos, TWMMouse(Message).YPos);
    end;

    WM_LBUTTONDBLCLK: begin
      inherited;
      if Assigned(FOwner.OnDblClick) and (csDoubleClicks in FOwner.ControlStyle) then
        if FOwner.Parent is TsPopupCalendar then begin
          if TsPopupCalendar(FOwner.Parent).FEditor = nil then
            FOwner.OnDblClick(FOwner)
        end
        else
          FOwner.OnDblClick(FOwner);
    end;

    WM_PAINT: begin
      inherited;
      // Filling BG vert line
      if TWMPAINT(Message).DC = 0 then
        DC := GetDC(Handle)
      else
        DC := TWMPAINT(Message).DC;

      SaveIndex := SaveDC(DC);
      try
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, FOwner);
        if BiDiMode = bdLeftToRight then
          R := Rect(ColCount * DefaultColWidth, 0, Width, Height) // Right BG line filling
        else
          R := Rect(0, 0, ClientWidth - ColCount * DefaultColWidth, Height); // Left BG line filling

        if BGInfo.BgType = btCache then
          BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), BGInfo.Bmp.Canvas.Handle, Left + R.Left + BGInfo.Offset.X, Top + R.Top + BGInfo.Offset.Y, SRCCOPY)
        else
          FillDC(DC, R, BGInfo.Color);
      finally
        RestoreDC(DC, SaveIndex);
        if TWMPAINT(Message).DC = 0 then
          ReleaseDC(Handle, DC);
      end;
    end;

    WM_LBUTTONDOWN: begin
      if not (FOwner.Parent is TsPopupCalendar) then
        inherited
      else
        if FOwner.Parent.Visible then
          if not Clicked then begin
            Clicked := True;
            MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
            if not FOwner.ShowWeeks or (X > 0) then
              if (Y > 0) and FOwner.IsValidDate(FOwner.GetCellDate(X, Y)) then begin // If not a day name
{$IFDEF DELPHI6UP}
                FocusCell(X, Y, False);
{$ELSE}
                TAccessStdGrid(Self).FCurrent.X := X;
                TAccessStdGrid(Self).FCurrent.Y := Y;
                Click;
{$ENDIF}
                Invalidate;
              end;

            Clicked := False;
          end;
      end;

    WM_LBUTTONUP: begin
      inherited;
      if FOwner.Parent is TsPopupCalendar then
        if FOwner.Parent.Visible and not Clicked then begin
          Clicked := True;
          MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
          TsPopupCalendar(FOwner.Parent).CalendarClick(Y, X);
          Clicked := False;
        end;
    end

    else inherited;
  end;
end;


procedure TsCalendGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARow, ACol: integer;
begin
  MouseToCell(X, Y, ACol, ARow);
  if ARow <> 7 then begin
    inherited;
    if Assigned(FOwner.OnMouseDown) then
      FOwner.OnMouseDown(FOwner, Button, Shift, X + Left, Y + Top);
  end;
end;


procedure TsCalendGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FOwner.OnMouseUp) then
    FOwner.OnMouseUp(FOwner, Button, Shift, X + Left, Y + Top);
end;


procedure TsCalendGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOwner.OnMouseMove) then
    FOwner.OnMouseMove(FOwner, Shift, X + Left, Y + Top);
end;


procedure TsMonthCalendar.DoChangeAnimated(ForwDirection: boolean; OldBmp: TBitmap);
var
  CacheBmp, NewBmp: TBitmap;
  Position: Integer;
  lTicks: DWord;
  FPos: real;
  DC: hdc;
begin
  FDragBar.Repaint;
  NewBmp := CreateBmp32(FGrid);
  FGrid.PaintTo({$IFDEF DELPHI6UP}NewBmp.Canvas{$ELSE}NewBmp.Canvas.Handle{$ENDIF}, 0, 0);
  CacheBmp := CreateBmp32(FGrid);
  FPos := 0;
  while Round(FPos) < FGrid.Width do begin
    lTicks := GetTickCount;
    FPos := FPos + (FGrid.Width - FPos) / 5;
    Position := Round(FPos);
    BitBlt(CacheBmp.Canvas.Handle, -Position * acMinusPlus[ForwDirection], 0, CacheBmp.Width, CacheBmp.Height, OldBmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(CacheBmp.Canvas.Handle, (FGrid.Width - Position) * acMinusPlus[ForwDirection], 0, CacheBmp.Width, CacheBmp.Height, NewBmp.Canvas.Handle, 0, 0, SRCCOPY);
    DC := GetDC(FGrid.Handle);
    try
      BitBlt(DC, 0, 0, CacheBmp.Width, CacheBmp.Height, CacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      ReleaseDC(FGrid.Handle, DC);
    end;
    WaitTicks(lTicks);
  end;
  OldBmp.Free;
  NewBmp.Free;
  CacheBmp.Free;
end;


procedure TsMonthCalendar.ChangeYear(Delta: Integer; AllowAnimation: boolean = False);
begin
  if FAllowChangeMonth then begin
    if TravellingSelection and IsLeapYear(Year) and (Month = 2) and (Day = 29) then
      Day := 28;

    ShownDate := MakeNewDate(Year + Delta, Month, Day);
    if ShownDate <> 0 then begin
      State := State + [csMonthChanging];
      GoToShownDate(Delta > 0, AllowAnimation);
      State := State - [csMonthChanging];
    end
    else
      ShownDate := FDate;
  end;
end;


{$IFNDEF WIN64}
var
  Lib: HModule = 0;
  ResStringRec: TResStringRec;


initialization
  Lib := LoadLibrary(comctl32);
  if Lib <> 0 then begin
  {$IFDEF DELPHI5}
    ResStringRec.Module := @ACNativeInt(Lib);
  {$ELSE}
    ResStringRec.Module := @Lib;
  {$ENDIF}
    // Search a localization of "Today:"
    ResStringRec.Identifier := {$IFDEF DELPHI_XE2} 4432 {$ELSE} 4163 {$ENDIF};
    s_Today := LoadResString(@ResStringRec);
    FreeLibrary(Lib);
    if s_Today = '' then
      s_Today := 'Today: ';
  end;

finalization
{$ENDIF} // WIN64

end.
