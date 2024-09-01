{*************************************************************************}
{ TMS TAdvDurationEdit component                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2020                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvDurationEdit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Classes, Controls, Dialogs,
  AdvEdit, AdvStyleIF, Forms, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First version

type
  TAdvDurationSpan = (dsYear, dsMonth, dsDay, dsHour, dsMinute, dsSecond, dsMilliSecond);

  TAdvDurationNumCustomMaskEdit = class(TAdvCustomMaskEdit)
  private
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
  end;

  TAdvDurationEditUnits = class(TPersistent)
  private
    FDay: string;
    FHour: string;
    FMillisecond: string;
    FMinute: string;
    FMonth: string;
    FSecond: string;
    FYear: string;
    FOnChanged: TNotifyEvent;
    procedure SetDay(const Value: string);
    procedure SetHour(const Value: string);
    procedure SetMillisecond(const Value: string);
    procedure SetMinute(const Value: string);
    procedure SetMonth(const Value: string);
    procedure SetSecond(const Value: string);
    procedure SetYear(const Value: string);
  protected
    procedure DoChanged; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Day: string read FDay write SetDay;
    property Hour: string read FHour write SetHour;
    property Millisecond: string read FMillisecond write SetMillisecond;
    property Minute: string read FMinute write SetMinute;
    property Month: string read FMonth write SetMonth;
    property Second: string read FSecond write SetSecond;
    property Year: string read FYear write SetYear;
  end;

  TAdvDurationEdit = class(TAdvCustomMaskEdit)
  private
    FAutoWidth: Boolean;
    FDurationDay: SmallInt;
    FDurationFirst: TAdvDurationSpan;
    FDurationHour: SmallInt;
    FDurationLast: TAdvDurationSpan;
    FDurationMilliSecond: SmallInt;
    FDurationMinute: SmallInt;
    FDurationMonth: SmallInt;
    FDurationSecond: SmallInt;
    FDurationText: string;
    FDurationYear: SmallInt;
    FMinWidth: Integer;
    FSegments: array[0..6] of TAdvDurationNumCustomMaskEdit;
    FSeparators: array[0..6] of TStaticText;
    FSeparatorWidth: Integer;
    FUnits: TAdvDurationEditUnits;
    FOnUnitsChanged: TNotifyEvent;
    FOnDurationSpanChange: TNotifyEvent;
    FOnDurationChange: TNotifyEvent;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetDurationValueString(ASpan: TAdvDurationSpan): string;
    function GetFlatEx: boolean;
    function GetReadOnlyEx: boolean;
    function GetUnitFromDurationSpan(ADurationSpan: TAdvDurationSpan): string;
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetDurationDay(const Value: SmallInt);
    procedure SetDurationFirst(const Value: TAdvDurationSpan);
    procedure SetDurationHour(const Value: SmallInt);
    procedure SetDurationLast(const Value: TAdvDurationSpan);
    procedure SetDurationMilliSecond(const Value: SmallInt);
    procedure SetDurationMinute(const Value: SmallInt);
    procedure SetDurationMonth(const Value: SmallInt);
    procedure SetDurationSecond(const Value: SmallInt);
    procedure SetDurationText;
    procedure SetDurationValue(ADurationSpan: TAdvDurationSpan; AddValue: integer);
    procedure SetDurationYear(const Value: SmallInt);
    procedure SetFlatEx(const Value: boolean);
    procedure SetReadOnlyEx(const Value: boolean);
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  protected
    procedure CalculateValues(AValue: TAdvDurationSpan);
    procedure CreateSubControls;
    procedure DestroySubControls;
    procedure DecrementDurationValue(ADurationSpan: TAdvDurationSpan);
    procedure DoUnitsChanged; virtual;
    procedure DoOnDurationChange; virtual;
    procedure DoOnDurationSpanChange; virtual;
    procedure DurationToText;
    procedure IncrementDurationValue(ADurationSpan: TAdvDurationSpan);
    procedure InitControls;
    procedure SegmentChange(Sender: TObject);
    procedure SegmentExit(Sender: TObject);
    procedure SegmentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SegmentKeyPress(Sender: TObject; var Key: Char);
    procedure SegmentKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetSeparatorCaptions;
    procedure SetEditRect;
    procedure UnitsChanged(Sender: TObject);
    procedure UpdateSubControls;
    procedure UpdateText(ADurationSpan: TAdvDurationSpan);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetDuration(ABeginDateTime: TDateTime; AEndDateTime: TDateTime);
  published
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;
    property Duration: string read FDurationText;
    property DurationDay: SmallInt read FDurationDay write SetDurationDay default 0;
    property DurationFirst: TAdvDurationSpan read FDurationFirst write SetDurationFirst default dsHour;
    property DurationHour: SmallInt read FDurationHour write SetDurationHour default 0;
    property DurationLast: TAdvDurationSpan read FDurationLast write SetDurationLast default dsSecond;
    property DurationMinute: SmallInt read FDurationMinute write SetDurationMinute default 0;
    property DurationMilliSecond: SmallInt read FDurationMilliSecond write SetDurationMilliSecond default 0;
    property DurationMonth: SmallInt read FDurationMonth write SetDurationMonth default 0;
    property DurationSecond: SmallInt read FDurationSecond write SetDurationSecond default 0;
    property DurationYear: SmallInt read FDurationYear write SetDurationYear default 0;
    property Units: TAdvDurationEditUnits read FUnits write FUnits;

    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    {$IFDEF DELPHIXE_LVL}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: boolean read GetReadOnlyEx write SetReadOnlyEx;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Visible;
    property AutoTab;
    property CanUndo;
    property BorderColor;
    property DisabledBorder;
    property DisabledColor;
    property Flat: boolean read GetFlatEx write SetFlatEx;
    property FlatLineColor;
    property FlatParentColor;
    property ShowModified;
    property FocusLabel;
    property LabelCaption;
    property LabelAlwaysEnabled;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelFont;
    property ModifiedColor;
    property ReturnIsTab;
    property SoftBorder;
    property Version;

    property OnMaskComplete;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDurationChange: TNotifyEvent read FOnDurationChange write FOnDurationChange;
    property OnDurationSpanChange: TNotifyEvent read FOnDurationSpanChange write FOnDurationSpanChange;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    {$IFDEF DELPHIXE_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnitsChanged: TNotifyEvent read FOnUnitsChanged write FOnUnitsChanged;
  end;

implementation
uses
  Math, DateUtils;

const
  Ctrl_Codes = [vk_back, vk_tab, vk_return];
  Numeric_Codes = [ord('0')..ord('9')];

{ TAdvDurationEdit }

procedure TAdvDurationEdit.CalculateValues(AValue: TAdvDurationSpan);
var
  i: Integer;
begin
  i := Ord(FDurationFirst);

  if Ord(AValue) < i then
  begin
    while I > Ord(AValue) do
    begin
      case TAdvDurationSpan(I) of
        dsMonth:
        begin
          if FDurationMonth > 11 then
          begin
            DurationYear := FDurationMonth div 12;
            DurationMonth := FDurationMonth mod 12;
          end;
        end;
        dsDay:
        begin
          if FDurationDay > 30 then
          begin
            if (AValue = dsYear) and (FDurationDay > 364) then
            begin
              FDurationYear := FDurationDay div 365;

              FDurationMonth := (FDurationDay mod 365) div 30;
              DurationDay := (FDurationDay mod 365) mod 30;

              Inc(I);
            end
            else
            begin
              FDurationMonth := FDurationDay div 30;
              DurationDay := FDurationDay mod 30;
            end;
          end;
        end;
        dsHour:
        begin
          if FDurationHour > 23 then
          begin
            FDurationDay := FDurationHour div 24;
            DurationHour := FDurationHour mod 24;
          end;
        end;
        dsMinute:
        begin
          if FDurationMinute > 59 then
          begin
            FDurationHour := FDurationMinute div 60;
            DurationMinute := FDurationMinute mod 60;
          end;
        end;
        dsSecond:
        begin
          if FDurationSecond > 59 then
          begin
            FDurationMinute := FDurationSecond div 60;
            DurationSecond := FDurationSecond mod 60;
          end;
        end;
        dsMilliSecond:
        begin
          if FDurationMilliSecond > 999 then
          begin
            FDurationSecond := FDurationMilliSecond div 1000;
            DurationMilliSecond := FDurationMilliSecond mod 1000;
          end;
        end;
      end;
      Dec(I);
    end;
  end
  else
  begin
    while I < Ord(AValue) do
    begin
      case TAdvDurationSpan(I) of
        dsYear:
        begin
          if FDurationYear > 0 then
          begin
            if Ord(AValue) <= Ord(dsDay) then
            begin
              FDurationDay := FDurationDay + DurationYear * 365 + (DurationYear div 4);

              if FDurationDay > 9999 then
                FDurationDay := 0
              else
              begin
                FDurationYear := 0;
                Inc(I);
              end;
            end
            else
            begin
              FDurationMonth := FDurationMonth + DurationYear * 12;

              if FDurationMonth > 9999 then
                FDurationMonth := 0
              else
                FDurationYear := 0;
            end;
          end;
        end;
        dsMonth:
        begin
          if FDurationMonth > 0 then
          begin
            FDurationDay := FDurationDay + FDurationMonth * 30;

            if FDurationDay > 9999 then
              FDurationDay := 0
            else
              FDurationMonth := 0;
          end;
        end;
        dsDay:
        begin
          if FDurationDay > 0 then
          begin
            FDurationHour := FDurationHour + FDurationDay * 24;

            if FDurationHour > 9999 then
              FDurationHour := 0
            else
              FDurationDay := 0;
          end;
        end;
        dsHour:
        begin
          if FDurationHour > 23 then
          begin
            FDurationMinute := FDurationMinute + FDurationHour * 60;

            if FDurationMinute > 9999 then
              FDurationMinute := 0
            else
              FDurationHour := 0;
          end;
        end;
        dsMinute:
        begin
          if FDurationMinute > 59 then
          begin
            FDurationSecond := FDurationSecond + FDurationMinute * 60;

            if FDurationSecond > 9999 then
              FDurationSecond := 0
            else
              FDurationMinute := 0;
          end;
        end;
        dsSecond:
        begin
          if FDurationSecond > 59 then
          begin
            FDurationMilliSecond := FDurationMilliSecond + FDurationSecond * 1000;

            if FDurationMilliSecond > 9999 then
              FDurationMilliSecond := 0
            else
              FDurationSecond := 0;
          end;
        end;
      end;
      Inc(I);
    end;
  end;
end;

procedure TAdvDurationEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdateSubControls;
  if HandleAllocated then
    SetEditRect;
end;

procedure TAdvDurationEdit.CMEnabledChanged(var Msg: TMessage);
var
  i: Integer;
begin
  inherited;
  if HandleAllocated then
  begin
    for i := 1 to Length(FSegments) - 1 do
    begin
      if Assigned(FSegments[i]) then
        FSegments[i].Enabled := Enabled;
    end;
  end;
end;

procedure TAdvDurationEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateSubControls;
  if HandleAllocated then
    SetEditRect;
end;

constructor TAdvDurationEdit.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  Units := TAdvDurationEditUnits.Create;

  Units.Year := 'Y';
  Units.Month := 'M';
  Units.Day := 'D';
  Units.Hour := 'h';
  Units.Minute := 'm';
  Units.Second := 's';
  Units.Millisecond := 'ms';

  Units.OnChanged := UnitsChanged;

  FDurationText := '00h 00m 00s';

  FDurationFirst := dsHour;
  FDurationLast := dsSecond;

  FSeparatorWidth := 8;

  for i := 0 to 6 do
  begin
    FSegments[i] := nil;
    FSeparators[i] := nil;
  end;

  FMinWidth := Width;
end;

procedure TAdvDurationEdit.CreateSubControls;
var
  i: integer;
begin
  for i := Ord(FDurationFirst) to Ord(FDurationLast) do
  begin
    if not Assigned(FSegments[i]) then
    begin
      FSegments[i] := TAdvDurationNumCustomMaskEdit.Create(Self);
      FSegments[i].Parent := Self;
      FSegments[i].BorderStyle := bsNone;
      FSegments[i].Top := 2;

      FSegments[i].OnKeyUp := SegmentKeyUp;
      FSegments[i].OnKeyDown := SegmentKeyDown;
      FSegments[i].OnKeyPress := SegmentKeyPress;
      FSegments[i].OnChange := SegmentChange;
      FSegments[i].OnExit := SegmentExit;

      FSegments[i].Tag := i;
      FSegments[i].PopupMenu := PopupMenu;
      FSegments[i].Alignment := taCenter;
    end;

    if I = Ord(FDurationFirst) then
      FSegments[i].MaxLength := 4
    else if (I = 6) then
      FSegments[i].MaxLength := 3
    else
      FSegments[i].MaxLength := 2;

    if not Assigned(FSeparators[i]) then
    begin
      FSeparators[i] := TStaticText.Create(Self);
      FSeparators[i].Parent := Self;
      FSeparators[i].Caption := GetUnitFromDurationSpan(TAdvDurationSpan(i));
    end;
      FSeparators[i].Top := 2;
  end;
end;

procedure TAdvDurationEdit.CreateWnd;
var
  Chg: TNotifyEvent;
begin
  inherited;

  Text := '';

  Chg := OnChange;
  OnChange := nil;

  Width := Width - 1;
  Width := Width + 1;

  InitControls;

  OnChange := Chg;
end;

procedure TAdvDurationEdit.DecrementDurationValue(ADurationSpan: TAdvDurationSpan);
begin
  case ADurationSpan of
    dsMonth:
    begin
      if FDurationMonth <= 0 then
      begin
        DurationMonth := 11;
        Exit;
      end;
    end;
    dsDay:
    begin
      if FDurationDay <= 0 then
      begin
        DurationDay := 30;
        Exit;
      end;
    end;
    dsHour:
    begin
      if FDurationHour <= 0 then
      begin
        DurationHour := 23;
        Exit;
      end;
    end;
    dsMinute:
    begin
      if FDurationMinute <= 0 then
      begin
        DurationMinute := 59;
        Exit;
      end;
    end;
    dsSecond:
    begin
      if FDurationSecond <= 0 then
      begin
        DurationSecond := 59;
        Exit;
      end;
    end;
    dsMilliSecond:
    begin
      if FDurationMilliSecond <= 0 then
      begin
        DurationMilliSecond := 999;
        Exit;
      end;
    end;
  end;

  SetDurationValue(ADurationSpan, -1);
end;

destructor TAdvDurationEdit.Destroy;
begin
  Units.Free;
  DestroySubControls;

  inherited;
end;

procedure TAdvDurationEdit.DestroySubControls;
var
  i: integer;
begin
  for i := 0 to Length(FSegments) - 1 do
  begin
    if Assigned(FSegments[i]) then
    begin
      FSegments[i].Free;
      FSegments[i] := nil;
    end;
    if Assigned(FSeparators[i]) then
    begin
      FSeparators[i].Free;
      FSeparators[i] := nil;
    end
  end;
end;

procedure TAdvDurationEdit.DoOnDurationChange;
begin
  if Assigned(OnDurationChange) then
    OnDurationChange(Self);
end;

procedure TAdvDurationEdit.DoOnDurationSpanChange;
begin
  if Assigned(OnDurationSpanChange) then
    OnDurationSpanChange(Self);
end;

procedure TAdvDurationEdit.DoUnitsChanged;
begin
  if Assigned(OnUnitsChanged) then
    OnUnitsChanged(Self);
end;

procedure TAdvDurationEdit.DurationToText;
var
  I: Integer;
begin
  if Assigned(FSegments[Ord(FDurationFirst)]) then
  begin
    for I := Ord(FDurationFirst) to Ord(FDurationLast) do
    begin
      FSegments[I].Text := GetDurationValueString(TAdvDurationSpan(I))
    end;
  end;
  SetDurationText;
end;

function TAdvDurationEdit.GetDurationValueString(ASpan: TAdvDurationSpan): string;
begin
  case ASpan of
    dsYear:
    begin
      Result := IntToStr(FDurationYear);

      while (Length(Result) < 4) do
        Result := '0' + Result;
    end;
    dsMonth:
    begin
      Result:= IntToStr(FDurationMonth);

      while (Length(Result) < 2) do
        Result := '0' + Result;
    end;
    dsDay:
    begin
      Result:= IntToStr(FDurationDay);

      while (Length(Result) < 2) do
        Result := '0' + Result;
    end;
    dsHour:
    begin
      Result:= IntToStr(FDurationHour);

      while (Length(Result) < 2) do
        Result := '0' + Result;
    end;
    dsMinute:
    begin
      Result:= IntToStr(FDurationMinute);

      while (Length(Result) < 2) do
        Result := '0' + Result;
    end;
    dsSecond:
    begin
      Result:= IntToStr(FDurationSecond);

      while (Length(Result) < 2) do
        Result := '0' + Result;
    end;
    dsMilliSecond:
    begin
      Result:= IntToStr(FDurationMilliSecond);

      while (Length(Result) < 3) do
        Result := '0' + Result;
    end;
  end;
end;

function TAdvDurationEdit.GetFlatEx: boolean;
begin
  Result := inherited Flat;
end;

function TAdvDurationEdit.GetReadOnlyEx: boolean;
begin
  Result := inherited ReadOnly;
end;

function TAdvDurationEdit.GetUnitFromDurationSpan(
  ADurationSpan: TAdvDurationSpan): string;
begin
  case ADurationSpan of
    dsYear: Result := Units.Year;
    dsMonth: Result := Units.Month;
    dsDay: Result := Units.Day;
    dsHour: Result := Units.Hour;
    dsMinute: Result := Units.Minute;
    dsSecond: Result := Units.Second;
    dsMilliSecond: Result := Units.Millisecond;
  end;
end;

procedure TAdvDurationEdit.IncrementDurationValue(ADurationSpan: TAdvDurationSpan);
begin
  case ADurationSpan of
    dsMonth:
    begin
      if not (FDurationFirst = dsMonth) and (FDurationMonth >= 11) then
      begin
        DurationMonth := 0;
        Exit;
      end;
    end;
    dsDay:
    begin
      if not (FDurationFirst = dsDay) and (FDurationDay >= 30) then
      begin
        DurationDay := 0;
        Exit;
      end;
    end;
    dsHour:
    begin
      if not (FDurationFirst = dsHour) and (FDurationHour >= 23) then
      begin
        DurationHour := 0;
        Exit;
      end;
    end;
    dsMinute:
    begin
      if not (FDurationFirst = dsMinute) and (FDurationMinute >= 59) then
      begin
        DurationMinute := 0;
        Exit;
      end;
    end;
    dsSecond:
    begin
      if not (FDurationFirst = dsSecond) and (FDurationSecond >= 59) then
      begin
        DurationSecond := 0;
        Exit;
      end;
    end;
    dsMilliSecond:
    begin
      if not (FDurationFirst = dsMilliSecond) and (FDurationMilliSecond >= 999) then
      begin
        DurationMilliSecond := 0;
        Exit;
      end;
    end;
  end;

  SetDurationValue(ADurationSpan, 1);
end;

procedure TAdvDurationEdit.InitControls;
begin
  DestroySubControls;
  CreateSubControls;
  UpdateSubControls;
  DurationToText;
end;

procedure TAdvDurationEdit.Loaded;
begin
  inherited;
  SetEditRect;
  Width := Width + 1;
  Width := Width - 1;
end;

procedure TAdvDurationEdit.SegmentChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvDurationEdit.SegmentExit(Sender: TObject);
var
  val: Integer;
  tg: Integer;
begin
  val := StrToInt((Sender as TAdvDurationNumCustomMaskEdit).Text);
  tg := (Sender as TAdvDurationNumCustomMaskEdit).Tag;

  case tg of
    0: DurationYear :=  val;
    1: DurationMonth := val;
    2: DurationDay := val;
    3: DurationHour := val;
    4: DurationMinute := val;
    5: DurationSecond := val;
    6: DurationMilliSecond := val;
  end;

  (Sender as TAdvDurationNumCustomMaskEdit).Text := GetDurationValueString(TAdvDurationSpan(tg));
end;

procedure TAdvDurationEdit.SegmentKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  span: TAdvDurationSpan;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);

  if (Key = VK_DOWN) or (Key = VK_UP) then
  begin
    span := TAdvDurationSpan((Sender as TAdvDurationNumCustomMaskEdit).Tag);

    if Key = VK_UP then
      IncrementDurationValue(span)
    else
      DecrementDurationValue(span);

    (Sender as TAdvDurationNumCustomMaskEdit).Text := GetDurationValueString(span);
  end;
end;

procedure TAdvDurationEdit.SegmentKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TAdvDurationEdit.SegmentKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  tg: integer;
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);

  tg := (Sender as TAdvDurationNumCustomMaskEdit).Tag;

  if (Length((Sender as TAdvDurationNumCustomMaskEdit).Text) = (Sender as TAdvDurationNumCustomMaskEdit).MaxLength) and ((Sender as TAdvDurationNumCustomMaskEdit).SelStart = (Sender as TAdvDurationNumCustomMaskEdit).MaxLength)  then
  begin
    if tg < Ord(FDurationLast) then
    begin
      FSegments[tg + 1].SetFocus;
    end;
  end;

  if (Key = VK_RIGHT) or (Key = VK_SPACE) then
  begin
    if tg < Ord(FDurationLast) then
    begin
      FSegments[tg + 1].SetFocus;
    end;
  end;

  if (Key = VK_LEFT) and ((Sender as TAdvDurationNumCustomMaskEdit).SelStart = 0) then
  begin
    if Length((Sender as TAdvDurationNumCustomMaskEdit).Text) = 0 then
      FSegments[tg].Text := '0';
  end;

  if (Key = VK_LEFT) and ((Sender as TAdvDurationNumCustomMaskEdit).SelStart = 0) then
  begin
    if tg > Ord(FDurationFirst) then
    begin
      FSegments[tg - 1].SetFocus;
    end
    else
    begin
      SetFocus;
    end;
  end;
end;

procedure TAdvDurationEdit.SetAutoWidth(const Value: Boolean);
begin
  if Value <> FAutoWidth then
  begin
    FAutoWidth := Value;
    if FAutoWidth then
      Width := FMinWidth;
  end;
end;

procedure TAdvDurationEdit.SetDuration(ABeginDateTime, AEndDateTime: TDateTime);
var
  t1, t2: TDateTime;
begin
  if ABeginDateTime > AEndDateTime then
  begin
    t1 := ABeginDateTime;
    t2 := AEndDateTime;
  end
  else
  begin
    t1 := AEndDateTime;
    t2 := ABeginDateTime;
  end;

  if Ord(FDurationFirst) < Ord(dsYear)  then
  begin
    DurationYear := YearsBetween(t2, t1);
    t1 := IncYear(t1, 0 - FDurationYear);
  end;
  if Ord(FDurationFirst) <= Ord(dsMonth)  then
  begin
    DurationMonth := MonthsBetween(t2, t1);
    t1 := IncMonth(t1, 0 - FDurationMonth);
  end;
  if Ord(FDurationFirst) <= Ord(dsDay)  then
  begin
    DurationDay := DaysBetween(t2, t1);
    t1 := IncDay(t1, 0 - FDurationDay);
  end;
  if Ord(FDurationFirst) <= Ord(dsHour)  then
  begin
    DurationHour := HoursBetween(t2, t1);
    t1 := IncHour(t1, 0 - FDurationHour);
  end;
  if Ord(FDurationFirst) <= Ord(dsMinute)  then
  begin
    DurationMinute := MinutesBetween(t2, t1);
    t1 := IncMinute(t1, 0 - FDurationMinute);
  end;
  if Ord(FDurationFirst) <= Ord(dsSecond)  then
  begin
    DurationSecond := SecondsBetween(t2, t1);
    t1 := IncSecond(t1, 0 - FDurationSecond);
  end;
  if Ord(FDurationFirst) <= Ord(dsMilliSecond)  then
    DurationMilliSecond := MilliSecondsBetween(t2, t1);
end;

procedure TAdvDurationEdit.SetDurationDay(const Value: SmallInt);
begin
  if (FDurationDay <> Value) and (((Value < 31) and (Value >= 0)) or (FDurationFirst = dsDay)) then
  begin
    FDurationDay := Value;
    UpdateText(dsDay);
  end;
end;

procedure TAdvDurationEdit.SetDurationFirst(const Value: TAdvDurationSpan);
begin
  if FDurationFirst <> Value then
  begin
    CalculateValues(Value);
    FDurationFirst := Value;
    InitControls;
    DoOnDurationSpanChange;
  end;
end;

procedure TAdvDurationEdit.SetDurationHour(const Value: SmallInt);
begin
  if (FDurationHour <> Value) and (((Value < 24) and (Value >= 0)) or (FDurationFirst = dsHour)) then
  begin
    FDurationHour := Value;
    UpdateText(dsHour);
  end;
end;

procedure TAdvDurationEdit.SetDurationLast(const Value: TAdvDurationSpan);
begin
  if FDurationLast <> Value then
  begin
    FDurationLast := Value;
    InitControls;
    DoOnDurationSpanChange;
  end;
end;

procedure TAdvDurationEdit.SetDurationMilliSecond(const Value: SmallInt);
begin
  if (FDurationMilliSecond <> Value) and (((Value < 1000) and (Value >= 0)) or (FDurationFirst = dsMilliSecond)) then
  begin
    FDurationMilliSecond := Value;
    UpdateText(dsMilliSecond);
  end;
end;

procedure TAdvDurationEdit.SetDurationMinute(const Value: SmallInt);
begin
  if (FDurationMinute <> Value) and (((Value < 60) and (Value >= 0)) or (FDurationFirst = dsMinute)) then
  begin
    FDurationMinute := Value;
    UpdateText(dsMinute);
  end;
end;

procedure TAdvDurationEdit.SetDurationMonth(const Value: SmallInt);
begin
  if (FDurationMonth <> Value) and (((Value < 12) and (Value >= 0)) or (FDurationFirst = dsMonth)) then
  begin
    FDurationMonth := Value;
    UpdateText(dsMonth);
  end;
end;

procedure TAdvDurationEdit.SetDurationSecond(const Value: SmallInt);
begin
  if (FDurationSecond <> Value) and (((Value < 60) and (Value >= 0)) or (FDurationFirst = dsSecond)) then
  begin
    FDurationSecond := Value;
    UpdateText(dsSecond);
  end;
end;

procedure TAdvDurationEdit.SetDurationText;
var
 s: string;
 i: integer;
begin
  I := Ord(FDurationFirst);
  s := '';

  while I <= Ord(FDurationLast) do
  begin
    s:= s + GetDurationValueString(TAdvDurationSpan(I)) + GetUnitFromDurationSpan(TAdvDurationSpan(I)) + ' ';
    Inc(I);
  end;

  Delete(s,s.Length, 1);

  FDurationText := s;
end;

procedure TAdvDurationEdit.SetDurationValue(ADurationSpan: TAdvDurationSpan;
  AddValue: integer);
begin
  case ADurationSpan of
    dsYear: DurationYear := FDurationYear + AddValue;
    dsMonth: DurationMonth := FDurationMonth + AddValue;
    dsDay: DurationDay := FDurationDay + AddValue;
    dsHour: DurationHour := FDurationHour + AddValue;
    dsMinute: DurationMinute := FDurationMinute + AddValue;
    dsSecond: DurationSecond := FDurationSecond + AddValue;
    dsMilliSecond: DurationMilliSecond := FDurationMilliSecond + AddValue;
  end;
end;

procedure TAdvDurationEdit.SetDurationYear(const Value: SmallInt);
begin
  if (FDurationYear <> Value) and (Value >= 0) then
  begin
    FDurationYear := Value;
    UpdateText(dsYear);
  end;
end;

procedure TAdvDurationEdit.SetEditRect;
var
  Loc: TRect;
  num: Integer;
begin
  num := Ord(FDurationLast) - Ord(FDurationFirst) + 1;
  if (num > 0) and HandleAllocated then
  begin
    SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
    Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
    Loc.Right := -3 + (ClientWidth - FSeparatorWidth * num) div num;

    if (BorderStyle = bsNone) then
    begin
      Loc.Top := 4;
      if Flat then
        Loc.Left := 4
      else
        Loc.Left := 2;
    end
    else
    begin
      Loc.Top := 1;
      Loc.Left := 1;
    end;

    SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  end;
end;

procedure TAdvDurationEdit.SetFlatEx(const Value: boolean);
var
  i: integer;
begin
  inherited Flat := Value;

  if HandleAllocated then
  begin
    for i := 1 to Length(FSegments) - 1 do
    begin
      if Assigned(FSegments[i]) then
      begin
        FSegments[i].Flat := Value;
        if Value then
        begin
          FSegments[i].Top := 4;
          FSegments[i].Height := Self.Height - 4;
        end
        else
        begin
          FSegments[i].Top := 2;
          FSegments[i].Height := Self.Height;
        end;
      end;
    end;
    for I := 0 to Length(FSeparators) - 1 do
    begin
      if Assigned(FSeparators[i]) then
      begin
        if Value then
          FSeparators[i].Top := 4
        else
          FSeparators[i].Top := 2;
      end;
    end;
  end;
end;

procedure TAdvDurationEdit.SetReadOnlyEx(const Value: boolean);
var
  i: integer;
begin
  inherited ReadOnly := Value;

  if HandleAllocated then
  begin
    for i := 1 to Length(FSegments) - 1 do
    begin
      if Assigned(FSegments[i]) then
        FSegments[i].ReadOnly := Value;
    end;
  end;
end;

procedure TAdvDurationEdit.SetSeparatorCaptions;
var
  I: Integer;
begin
  for I := Ord(dsYear) to Ord(dsMilliSecond) do
  begin
    FSeparators[i].Caption := GetUnitFromDurationSpan(TAdvDurationSpan(i));
  end;
end;

procedure TAdvDurationEdit.UnitsChanged(Sender: TObject);
begin
  SetSeparatorCaptions;
  UpdateSubControls;
  SetDurationText;

  DoUnitsChanged;
end;

procedure TAdvDurationEdit.UpdateSubControls;
var
  w,i: integer;
  SegLeft: Integer;
  tw: Integer;
  FTmpCanvas: TCanvas;
  FHDC: HDC;
begin
  SegLeft := 2;

  if Assigned(FSegments[Ord(FDurationFirst)]) and HandleAllocated then
  begin
    for i := Ord(FDurationFirst) to Ord(FDurationLast) do
    begin
      FTmpCanvas := TCanvas.Create;
      try
        FHDC := GetDC(0);
        FTmpCanvas.Handle := FHDC;
        FTmpCanvas.Font := Font;
        if (i = Ord(FDurationFirst)) then
          w := FTmpCanvas.TextWidth('33333')
        else if (i = 6) then
          w := FTmpCanvas.TextWidth('3333')
        else
          w := FTmpCanvas.TextWidth('333');
        tw := FTmpCanvas.TextWidth(FSeparators[i].Caption);
        ReleaseDC(0,FHDC)
      finally
        FTmpCanvas.Free;
      end;

      FSegments[i].Width := w;
      FSegments[i].Height := ClientHeight - 2;

      if Flat then
      begin
        FSegments[i].Height := Self.Height - 4;
      end
      else
      begin
        FSegments[i].Height := Self.Height;
      end;

      FSegments[i].Color := Color;
      if BorderStyle = bsNone then
        FSegments[i].Top := 4
      else
        FSegments[i].Top := 2;

      if I = Ord(FDurationFirst) then
        FSegments[i].Alignment := taRightJustify
      else
        FSegments[i].Alignment := taCenter;

      FSegments[i].Left := SegLeft;
      SegLeft := SegLeft + w - 1;
      if I = Ord(FDurationFirst) then
        SegLeft := SegLeft + 3;
      FSeparators[i].Left := SegLeft;
      SegLeft := SegLeft + tw;

      FSegments[i].PopupMenu := PopupMenu;

      if I = Ord(FDurationFirst) then
      FSegments[i].Alignment := taRightJustify
      else
        FSegments[i].Alignment := taCenter;
      FSegments[i].ReadOnly := ReadOnly;
    end;

    FMinWidth := SegLeft + 10;
    if FAutoWidth then
    begin
      Width := FMinWidth;
    end;
  end;

  for I := 0 to Length(FSeparators) - 1 do
  begin
    if Assigned(FSeparators[i]) then
    begin
      if Flat then
        FSeparators[i].Top := 4
      else
        FSeparators[i].Top := 2;
    end;
  end;
end;

procedure TAdvDurationEdit.UpdateText(ADurationSpan: TAdvDurationSpan);
begin
  if Assigned(FSegments[Ord(ADurationSpan)]) then
    FSegments[Ord(ADurationSpan)].Text := GetDurationValueString(ADurationSpan);
  SetDurationText;
  DoOnDurationChange;
end;

procedure TAdvDurationEdit.WMChar(var Msg: TWMKey);
begin
  Msg.Result := 1
end;

procedure TAdvDurationEdit.WMKillFocus(var Message: TMessage);
var
  I: integer;
begin
  for I := Ord(FDurationFirst) to Ord(FDurationLast) do
  begin
    if Assigned(FSegments[i]) then
      FSegments[i].Visible := True;
    if Assigned(FSeparators[i]) then
      FSeparators[i].Visible := True;
  end;
  Text := '';
end;

procedure TAdvDurationEdit.WMSetFocus(var Msg: TWMSetFocus);
var
  I: Integer;
begin
  for I := Ord(FDurationFirst) to Ord(FDurationLast) do
  begin
    if Assigned(FSegments[i]) and FSegments[i].Visible then
    begin
      FSegments[i].SetFocus;
      Exit;
    end;
  end;
end;

{ TAdvDurationNumCustomMaskEdit }

procedure TAdvDurationNumCustomMaskEdit.WMChar(var Msg: TWMKey);
begin
  if (Msg.CharCode in Numeric_Codes + Ctrl_Codes) then
    inherited
  else
    Msg.Result := 1;
end;

{ TAdvDurationEditUnits }

procedure TAdvDurationEditUnits.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TAdvDurationEditUnits.SetDay(const Value: string);
begin
  if FDay <> Value then
  begin
    FDay := Value;
    DoChanged
  end;
end;

procedure TAdvDurationEditUnits.SetHour(const Value: string);
begin
  if FHour <> Value then
  begin
    FHour := Value;
    DoChanged
  end;
end;

procedure TAdvDurationEditUnits.SetMillisecond(const Value: string);
begin
  if FMillisecond <> Value then
  begin
    FMillisecond := Value;
    DoChanged
  end;
end;

procedure TAdvDurationEditUnits.SetMinute(const Value: string);
begin
  if FMinute <> Value then
  begin
    FMinute := Value;
    DoChanged;
  end;
end;

procedure TAdvDurationEditUnits.SetMonth(const Value: string);
begin
  if FMonth <> Value then
  begin
    FMonth := Value;
    DoChanged
  end;
end;

procedure TAdvDurationEditUnits.SetSecond(const Value: string);
begin
  if FSecond <> Value then
  begin
    FSecond := Value;
    DoChanged
  end;
end;

procedure TAdvDurationEditUnits.SetYear(const Value: string);
begin
  if FYear <> Value then
  begin
    FYear := Value;
    DoChanged
  end;
end;

end.
