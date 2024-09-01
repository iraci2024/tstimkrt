unit sCurrEdit;
{$I sDefs.inc}

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Mask,
  {$IFNDEF ALITE}sCalcUnit, {$ENDIF}
  sCustomComboEdit, acntUtils, sConst;


type
{$IFNDEF NOTFORHELP}
  TsCustomNumEdit = class(TsCustomComboEdit)
  private
    FFocused,
    FBeepOnError,
    FCheckOnExit,
    FFlatButtons,
    FFormatOnEditing,
    FFormatting: boolean;

    FCanvas: TControlCanvas;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: string;
    FOnValidating: boolean;
    FShowZeroIfEmpty: boolean;
    FAutoValueCorrect: boolean;
    function GetAsInteger: Int64;
    function GetText: string;
    function GetDisplayFormat: string;
    function IsFormatStored: boolean;
    procedure SetShowZeroIfEmpty(const Value: boolean);

    procedure SetFocused       (AValue: boolean);
    procedure SetBeepOnError   (AValue: boolean);
    procedure SetDisplayFormat (const AValue: string);
    procedure SetDecimalPlaces (AValue: Cardinal);
    procedure SetMaxValue      (AValue: Extended);
    procedure SetMinValue      (AValue: Extended);
    procedure SetText          (const AValue: string);

    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit (var Message: TCMExit ); message CM_EXIT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
{$IFNDEF FPC}
  {$IFNDEF ALITE}
    procedure CalcWindowClose(Sender: TObject; var Action: TCloseAction);
  {$ENDIF}
{$ENDIF}
  protected
    FValue,
    FMinValue,
    FMaxValue: Extended;

    function TextToValText(const AValue: string): string;
    function CheckValue(NewValue: Extended): Extended; virtual;
    function GetValue: Extended; virtual;
    function IsDefaultValue: boolean; override;
    procedure Change; override;
    procedure ReformatEditText;
    procedure DataChanged;
    function DefFormat: string;
    procedure SetValue(AValue: Extended); virtual;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): boolean;
    function FormatDisplayText(AValue: Extended): string;
    function GetDisplayText: string; virtual;
    procedure Reset; override;
    procedure CheckRange;
    procedure UpdateData(FromPaste: boolean = False);
    function CanCalc: boolean; virtual;
    property Formatting: boolean read FFormatting;
    property Alignment default taRightJustify;
    property BeepOnError: boolean read FBeepOnError write SetBeepOnError default True;
    property CheckOnExit: boolean read FCheckOnExit write FCheckOnExit default False;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces default 2;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat stored IsFormatStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    procedure PopupWindowShow; override;
    procedure PopupWindowClose; override;
    property ClickKey;
    procedure PaintText; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure WndProc(var Message: TMessage); override;
    property AsInteger: Int64 read GetAsInteger;
    property DisplayText: string read GetDisplayText;
    property DroppedDown;
    property Value: Extended read GetValue write SetValue;
  published
    property ButtonMode;
    property DirectInput;
    property AutoValueCorrect: boolean read FAutoValueCorrect write FAutoValueCorrect default True;
    property FlatButtons: boolean read FFlatButtons write FFlatButtons default False;
    property ShowZeroIfEmpty: boolean read FShowZeroIfEmpty write SetShowZeroIfEmpty default True;
    property PopupWidth  default 216;
    property PopupHeight default 136;
    property TextHint;
  end;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsCalcEdit = class(TsCustomNumEdit)
  public
    property AsInteger;
{$IFNDEF NOTFORHELP}
    constructor Create(AOwner: TComponent); override;
  published
    property ClickKey;
    property AutoSelect;
    property BeepOnError;
    property DirectInput;
    property DragCursor;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property OnValidateError;
    property Alignment;
    property CheckOnExit;
    property DecimalPlaces;
    property DisplayFormat;
    property MaxValue;
    property MinValue;
    property Text;
    property Value;
  end;

{$IFNDEF NOTFORHELP}
var
  s_ValueBounds: acString = 'The value is out of min/max bounds';
{$ENDIF} // NOTFORHELP

implementation

uses
  {$IFDEF DELPHI7UP} RTLConsts, {$ENDIF}
  sAlphaGraph, sVclUtils, sMessages, sGraphUtils, sCommonData, sGlyphUtils, sSkinManager;


constructor TsCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Alignment := taRightJustify;
  FDisplayFormat := DefFormat;
  MaxLength := 0;
  FDecimalPlaces := 2;
  FBeepOnError := True;
  FFlatButtons := False;
  FShowZeroIfEmpty := True;
  FFormatOnEditing := False;
  FAutoValueCorrect := True;
  FOnValidating := False;
  inherited Text := '';
  DataChanged;
  PopupWidth := 216;
  PopupHeight := 136;
end;


destructor TsCustomNumEdit.Destroy;
begin
  if FPopupWindow <> nil then
    FreeAndNil(FPopupWindow);

  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);

  inherited Destroy;
end;


function TsCustomNumEdit.DefFormat: string;
begin
  Result := '### ### ##0.00;-### ### ##0.00;0';
end;


function TsCustomNumEdit.IsDefaultValue: boolean;
begin
  Result := Value = 0;
end;


function TsCustomNumEdit.IsFormatStored: boolean;
begin
  Result := DisplayFormat <> DefFormat;
end;


function TsCustomNumEdit.IsValidChar(Key: Char): boolean;
var
  S: string;
  SelStart, SelStop, DecPos: Integer;
  RetValue: Extended;
begin
  Result := False;
  if (Key = CharMinus) and (MinValue >= 0) and (MaxValue <> MinValue) then
    Exit;

  S := EditText;
  GetSel(SelStart, SelStop);
  System.Delete(S, SelStart + 1, SelStop - SelStart);
  System.Insert(Key, S, SelStart + 1);
  S := TextToValText(S);
  DecPos := Pos({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator, S);
  if DecPos > 0 then begin
    SelStart := Pos('E', UpperCase(S));
    if (SelStart > DecPos) then
      DecPos := SelStart - DecPos
    else
      DecPos := Length(S) - DecPos;

    if DecPos > Integer(FDecimalPlaces) then
      Exit;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;


procedure TsCustomNumEdit.KeyPress(var Key: Char);
var
  p: DWord;
begin
  if CanCalc and Assigned(FPopupWindow) and IsWindowVisible(FPopupWindow.Handle) then begin
{$IFNDEF ALITE}
    TsCalcForm(FPopupWindow).OnKeyPress(FPopupWindow, Key);
{$ENDIF}
    Key := #0;
  end
  else begin
    if CharInSet(AnsiChar(Key), [s_Dot, s_Comma] - [{$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ThousandSeparator]) then
      Key := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator;

    inherited KeyPress(Key);
    if not CharInSet(Key, [#3, #22]) then begin // Ctrl-C, Ctrl-V
      if CanCalc and (CharInSet(Key, [#43{'+'}{, #45{'-'}, #47{'/'}, #42{'*'}, #37{'%'}]) or ((Key = #45) and (Pos('E', UpperCase(Text)) < 1))) then begin
        p := ACNativeUInt(SendMessage(Handle, EM_GETSEL, 0, 0)) mod $10000;
        if p = 0 then begin
          if (Key <> #45) then
            Key := #0;

          Exit;
        end;

        if (Key = #45) and (Value = 0) {or not ShowButton} then
          Exit;

        DoClick;
{$IFNDEF ALITE}
        if Assigned(FPopupWindow) and IsWindowVisible(FPopupWindow.Handle) then
          TsCalcForm(FPopupWindow).OnKeyPress(FPopupWindow, Key);
{$ENDIF}

        Key := #0;
      end
      else
        if not CharInSet(Key, [#8]) and not IsValidChar(Key) then begin
          if BeepOnError then
            MessageBeep(0);

          Key := #0;
        end
        else
          if Key = #27 then begin
            Reset;
            Key := #0;
          end;
    end;
  end;
end;


procedure TsCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;


procedure TsCustomNumEdit.SetBeepOnError(AValue: boolean);
begin
  if FBeepOnError <> AValue then
    FBeepOnError := AValue;
end;


procedure TsCustomNumEdit.SetDisplayFormat(const AValue: string);
begin
  if DisplayFormat <> AValue then begin
    FDisplayFormat := AValue;
    SkinData.Invalidate;
    DataChanged;
  end;
end;


function TsCustomNumEdit.GetDisplayFormat: string;
begin
  Result := FDisplayFormat;
end;


procedure TsCustomNumEdit.SetFocused(AValue: boolean);
begin
  if FFocused <> AValue then begin
    FFocused := AValue;
    if not Focused or not AutoSelect then
      SkinData.Invalidate;

    FFormatting := True;
    try
      DataChanged;
    finally
      FFormatting := False;
    end;
  end;
end;


procedure TsCustomNumEdit.SetDecimalPlaces(AValue: Cardinal);
begin
  if FDecimalPlaces <> AValue then begin
    FDecimalPlaces := AValue;
    DataChanged;
    SkinData.Invalidate;
  end;
end;


function TsCustomNumEdit.FormatDisplayText(AValue: Extended): string;
begin
  if DisplayFormat <> '' then begin
    Result := FormatFloat(DisplayFormat, AValue);
    while (Length(Result) > 0) and (Result[1] = s_Space) do
      Delete(Result, 1, 1); // Del leading spaces
  end
  else
    Result := FloatToStr(AValue);
end;


function TsCustomNumEdit.GetDisplayText: string;
var
  s: string;
begin
  s := Text;
  if not Focused or ButtonMode then
    if Text = '' then
      if FShowZeroIfEmpty then
        Result := ZeroChar
      else
        Result := ''
    else
      Result := FormatDisplayText(StrToFloat(TextToValText(Text)))
  else
    Result := EditText;
end;


procedure TsCustomNumEdit.Clear;
begin
  Text := '';
end;


procedure TsCustomNumEdit.DataChanged;
var
  EditFormat: string;
begin
  if EditMask = '' then begin
    EditFormat := ZeroChar;
    if FDecimalPlaces > 0 then
      EditFormat := EditFormat + s_Dot + MakeStr(CharDiez, FDecimalPlaces);

    if not FShowZeroIfEmpty and (FValue = 0) then begin
      EditText := '';
      SendMessage(Handle, WM_SETTEXT, 0, 0);
    end
    else
      EditText := FormatFloat(EditFormat, FValue);
  end;
end;


function TsCustomNumEdit.CheckValue(NewValue: Extended): Extended;

  procedure DoMinMaxError;
  begin
    if not FOnValidating then begin
      FOnValidating := True;
      if Assigned(OnValidateError) then
        OnValidateError(Self, s_ValueBounds)
      else begin
        if not (csDesigning in ComponentState) and CanFocus then
          SetFocus;

        raise EDBEditError.Create(s_ValueBounds);
      end;
      FOnValidating := False;
    end;
  end;

begin
  Result := NewValue;
  if FMaxValue <> FMinValue then
    if FMaxValue > FMinValue then begin
      if NewValue < FMinValue then
        if AutoValueCorrect then begin
          Result := FMinValue;
          Modified := False;
        end
        else
          DoMinMaxError
      else
        if NewValue > FMaxValue then
          if AutoValueCorrect then begin
            Result := FMaxValue;
            Modified := False;
          end
          else
            DoMinMaxError;
    end
    else
      if FMaxValue = 0 then begin
        if NewValue < FMinValue then
          if AutoValueCorrect then begin
            Result := FMinValue;
            Modified := False;
          end
          else
            DoMinMaxError;
      end
      else
        if FMinValue = 0 then
          if NewValue > FMaxValue then
            if AutoValueCorrect then begin
              Result := FMaxValue;
              Modified := False;
            end
            else
              DoMinMaxError;
end;


procedure TsCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)));
end;


procedure TsCustomNumEdit.UpdateData(FromPaste: boolean = False);
var
  s: string;
  Minus: integer;
  OldValue, NewValue: Extended;
begin
  s := Text;
  if pos(CharMinus, s) = 1 then begin
    Delete(s, 1, 1);
    Minus := -1
  end
  else
    Minus := 1;

  if FromPaste then begin
    OldValue := FValue;
    NewValue := 0;
    if not AnyStrToCurr(s, NewValue) then begin
      if Assigned(OnValidateError) then
        OnValidateError(Self, EditText)
{$IFDEF DELPHI7UP}
      else
        raise EInvalidOperation.CreateResFmt(@SMaskErr, [EditText])
{$ENDIF};

      EditText := FormatDisplayText(OldValue); // Return prev text
    end
    else
      FValue := CheckValue(NewValue * Minus);
  end
  else
    FValue := CheckValue(StrToFloat(TextToValText(ZeroChar + s)) * Minus);
end;


function TsCustomNumEdit.GetValue: Extended;
begin
  if not (csDesigning in ComponentState) then
    try
      UpdateData(False);
    except
      FValue := FMinValue;
    end;

  Result := FValue;
end;


procedure TsCustomNumEdit.SetValue(AValue: Extended);
begin
  if not (csLoading in ComponentState) then
    FValue := CheckValue(AValue)
  else
    FValue := AValue;

  DataChanged;
  SkinData.Invalidate;
end;


function TsCustomNumEdit.GetAsInteger: Int64;
begin
  Result := Trunc(Value);
end;


procedure TsCustomNumEdit.SetMinValue(AValue: Extended);
begin
  if FMinValue <> AValue then begin
    FMinValue := AValue;
    Value := FValue;
  end;
end;


procedure TsCustomNumEdit.SetShowZeroIfEmpty(const Value: boolean);
begin
  if FShowZeroIfEmpty <> Value then begin
    FShowZeroIfEmpty := Value;
    if not Value and (FValue = 0) then begin
      EditText := '';
      DataChanged;
      SendMessage(Handle, WM_SETTEXT, 0, 0);
    end;
    SkinData.Invalidate(True);
  end;
end;


procedure TsCustomNumEdit.SetMaxValue(AValue: Extended);
begin
  if FMaxValue <> AValue then begin
    FMaxValue := AValue;
    Value := FValue;
  end;
end;


function TsCustomNumEdit.GetText: string;
begin
  Result := inherited Text;
  Result := DelChars(Result, CharQuest);
  Result := DelChars(Result, #13);
  Result := DelChars(Result, #10);
end;


function TsCustomNumEdit.TextToValText(const AValue: string): string;
begin
  Result:= StringReplace(AValue, s_Space, '', [rfIgnoreCase, rfReplaceAll]);
  if {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator <> {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ThousandSeparator then
    Result := DelChars(Result, {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ThousandSeparator);

  if ({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator <> s_Dot) and ({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ThousandSeparator <> s_Dot) then
    Result := ReplaceStr(Result, s_Dot, {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator);

  if ({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator <> s_Comma) and ({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}ThousandSeparator <> s_Comma) then
    Result := ReplaceStr(Result, s_Comma, {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator);

  if Result = '' then begin
    if FShowZeroIfEmpty then
      Result := ZeroChar
  end
  else
    if Result = CharMinus then
      Result := '-0';
end;


procedure TsCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then begin
    if AValue <> '' then
      FValue := CheckValue(StrToFloat(TextToValText(AValue)))
    else
      FValue := 0;

    DataChanged;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomNumEdit.ReformatEditText;
var
  S: string;
  IsEmpty: boolean;
  OldLen, SelStart, SelStop: Integer;
begin
  FFormatting := True;
  try
    S := inherited Text;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = CharMinus);
    if HandleAllocated then
      GetSel(SelStart, SelStop);

    if not IsEmpty then
      S := TextToValText(S);

    S := FormatFloatStr(S, Pos(s_Comma, DisplayFormat) > 0);
    inherited Text := S;
    if HandleAllocated and (GetFocus = Handle) and not (csDesigning in ComponentState) then begin
      Inc(SelStart, Length(S) - OldLen);
      SetCursor(SelStart);
    end;
  finally
    FFormatting := False;
  end;
end;


function TsCustomNumEdit.CanCalc: boolean;
begin
  Result := True;
end;


procedure TsCustomNumEdit.Change;
begin
  if not FFormatting then begin
    if FFormatOnEditing and FFocused then
      ReformatEditText;

    inherited Change;
  end;
end;


procedure TsCustomNumEdit.WMPaste(var Message: TMessage);
var
  S: string;
begin
  S := EditText;
  try
    inherited;
    UpdateData(True);
  except
    EditText := S;
    SelectAll;
    if CanFocus then
      SetFocus;

    if BeepOnError then
      MessageBeep(0);
  end;
end;


procedure TsCustomNumEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  if FFormatOnEditing then
    ReformatEditText;

  inherited;
end;


procedure TsCustomNumEdit.CMExit(var Message: TCMExit);
begin
  try
//    CheckRange;
    UpdateData(False);
//    Modified := False; Should be reset if AutoCorrection was made only
  except
    SelectAll;
    if CanFocus then
      SetFocus;

    raise;
  end;
  SetFocused(False);
  SetCursor(0);
  DoExit;
end;


procedure TsCustomNumEdit.PopupWindowClose;
begin
  inherited;
  dec(SysPopupCount);
end;


procedure TsCustomNumEdit.PopupWindowShow;
begin
  if CanCalc then begin
    FadingForbidden := True;
{$IFNDEF ALITE}
    FreeAndNil(FPopupWindow);
    FPopupWindow := TsCalcForm.Create(Self);
    with TsCalcForm(FPopupWindow) do begin
      BorderStyle := bsNone;
      Font.Assign(Self.Font);
      if SkinData.SkinManager <> nil then begin
        Font.Height := Font.Height * 96 div SkinData.CommonSkinData.PPI; // Get original size
        if SkinData.SkinManager.IsActive and (SkinData.SkinManager.Options.ScaleMode = smVCL) and (GetControlPPI(Self) = Screen.PixelsPerInch) then
          Font.Height := ScaleInt(Font.Height, SkinData);
      end;
      Position := poDefault;
      FPrecision := 16;
      FEditor := Self;
      FMainPanel.BorderStyle := bsNone;
      FDisplayPanel.BorderStyle := bsNone;
      SetText(Text);
      OnClose := CalcWindowClose;
    end;
{$ENDIF}
    inc(SysPopupCount);
    inherited;
    FadingForbidden := False;
  end;
end;


procedure TsCustomNumEdit.PaintText;
var
  R: TRect;
  aText: acString;
  al: TAlignment;
  Flags: Cardinal;
  TranspMode: byte;
begin
  SkinData.FCacheBmp.Canvas.Font.Assign(Font);
  TranspMode := GetTransMode(SkinData);
  R := TextRect;
  if TranspMode = TM_FULL then
    dec(R.Left, 2);

  if Focused and not ButtonMode then
    al := taLeftJustify
  else
    al := BidiAlign[IsRightToLeft, Alignment];

  Flags := DT_TOP or DT_NOPREFIX or DT_ALPHATEXT * Cardinal(SkinData.TranspMode = TM_FULL);
  if not IsDefaultValue or (Value = 0) and (TextHint = '') then begin
    if PasswordChar <> #0 then
      acFillString(aText, Length(Text), iff(PasswordChar = '*', CharBullet, acChar(PasswordChar)))
    else
      aText := DisplayText;

    acWriteTextEx(SkinData.FCacheBMP.Canvas, PacChar(aText), True, R, Flags or GetStringFlags(Self, al), SkinData, ControlIsActive(SkinData));
  end
  else
    if not SkinData.FFocused then
      TextHintOut(SkinData, R, TextHint, Flags)
    else
      Exit;

  if ButtonMode and ShowFocus and Focused and CanShowFocus(SkinData) then begin
    InflateRect(R, 1 + integer(not AllowBtnStyle), 0);
    acDrawFocusRect(SkinData.FCacheBmp.Canvas, R);
  end;
  if SkinData.TranspMode = TM_PARTIAL then
    FillAlphaRect(SkinData.FCacheBmp, R);
end;


procedure TsCustomNumEdit.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN:
          if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and Assigned(FPopupWindow) then
            AlphaBroadCast(FPopupWindow, Message);

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then
            if Assigned(FPopupWindow) then
              AlphaBroadCast(FPopupWindow, Message);
      end;
  end;
end;


constructor TsCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefBmpID := iBTN_CALC;
end;


{$IFNDEF FPC}
  {$IFNDEF ALITE}
procedure TsCustomNumEdit.CalcWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
end;
  {$ENDIF}
{$ENDIF}

end.
