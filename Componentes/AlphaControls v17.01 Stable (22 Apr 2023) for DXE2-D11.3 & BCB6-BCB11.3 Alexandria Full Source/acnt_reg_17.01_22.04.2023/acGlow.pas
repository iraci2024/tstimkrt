unit acGlow;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Controls, Classes, Forms, SysUtils, Graphics, Messages,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  sConst, acntTypes, sSkinManager, sCommonData;

type
  TacSizeChanging = (scIncrease, scDecrease, scNotChanged);

  TacCircleEffectData = record
    SizeChanging: TacSizeChanging;
    SkinData: TsCommonData;
    AnimProc: TacAnimProc;

    RadiusFinish: integer;
    RadiusStart: integer;

    Center: TPoint;
    State: integer;
    Color: TColor;

    AlphaFinish: byte;
    AlphaStart: byte;
  end;


function ShowGlow(const RealRect: TRect; const SkinSection, Name: string; const Margin, Alpha: integer; WndHandle: HWND; SkinData: TsCommonData = nil): integer; overload;
function ShowGlow(const RealRect: TRect; const MaskIndexes: array of integer; const Margin, Alpha: integer; WndHandle: HWND; SkinData: TsCommonData = nil): integer; overload;
procedure HideGlow(var ID: integer; DoAnimation: boolean = False);
procedure HideCircle(var ID: integer);
procedure RepaintGlow(ID: integer);
procedure SetGlowAlpha(ID: integer; aAlpha: byte);
procedure ClearGlows(ManualFreeing: boolean = False; SkipCircles: boolean = False);
// Circle click procedures
procedure ShowCircleIfNeeded(const SkinData: TsCommonData; Alpha: byte = MaxByte; DoAnimation: boolean = False);
function CreateCircleEffect(Data: TacCircleEffectData): integer;
procedure StartLEAnimation(Data: TacCircleEffectData; AllowAnimation: boolean);
function CircleClickMode(Ctrl: TControl; sd: TsCommonData; EventProperty: TacAnimatEvents): boolean;
function MinCircleRadius(sd: TsCommonData): integer;
function MaxCircleRadius(sd: TsCommonData): integer;

function UpdateCircle(Data: TObject; iIteration: integer): boolean;

var
  bGlowingDestroying: boolean = False;
  iMinCircleRadius: integer = 17;
  iMaxCircleRadius: integer = 23;


implementation

uses
  math,
{$IFNDEF ALITE}
  acSlider,
{$ENDIF}  
  sGradient, sStyleSimply, acntUtils, sTrackBar, sGraphUtils, sAlphaGraph, sVCLUtils, acThdTimer, acAnimation, sMessages;


type
  TacLayeredEffect = class(TObject)
  private
    FSkinData: TsCommonData;
    procedure SetSkinData(const Value: TsCommonData);
  protected
    AlphaBmp: TBitmap;
  public
    State: integer;
    FWndHandle: HWND;
    Wnd: TacGlowForm;
    Destroyed: boolean;
    OldWndProc: TWndMethod;
    SkinManager: TsSkinManager;
    property SkinData: TsCommonData read FSkinData write SetSkinData;
    procedure Repaint; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure NewWndProc(var Message: TMessage);
  end;


  TacGlowEffect = class(TacLayeredEffect)
  public
    FAlpha,
    Margin,
    MaskIndex: integer;
    FRealRect: TRect;
    procedure Repaint; override;
    procedure Show(RealRect: TRect; const Alpha: integer; WndHandle: HWND);
  end;


  TacCircleEffect = class(TacLayeredEffect)
  public
    AlphaFinish: byte;
    RadiusStart,
    RadiusFinish: integer;
    Color: TColor;
    Center: TPoint;
    RealAlpha: real;
    RealRadius: real;
    BackBmp: TBitmap;
    SizeChanging: TacSizeChanging;
    constructor Create; override;
    destructor Destroy; override;
    procedure Repaint; override;
    procedure Show;
  end;


  TacLayeredEffects = array of TacLayeredEffect;


var
  acgEffects: TacLayeredEffects;


function ShowGlow(const RealRect: TRect; const SkinSection, Name: string; const Margin, Alpha: integer; WndHandle: HWND; SkinData: TsCommonData = nil): integer;
var
  NewGlow: TacGlowEffect;
  mi: integer;
begin
  Result := -1;
  if acLayered and (SkinData <> nil) and (GetCapture = 0) then begin
    NewGlow := TacGlowEffect.Create;
    NewGlow.SkinData := SkinData;
    NewGlow.SkinManager := SkinData.SkinManager;
    if SkinData.CommonSkinData.gd[SkinData.SkinIndex].GlowCount > 0 then
      if SkinSection <> '' then
        mi := NewGlow.SkinManager.GetMaskIndex(SkinSection, Name + ZeroChar, SkinData.CommonSkinData)
      else
        if SkinData <> nil then
          mi := NewGlow.SkinManager.GetMaskIndex(SkinData.SkinIndex, Name + ZeroChar, SkinData.CommonSkinData)
        else
          mi := -1
    else
      mi := -1;

    if (mi >= 0) or ((SkinData.OuterEffects.Visibility = ovMouseHovered) and (SkinData.CommonSkinData.gd[SkinData.SkinIndex].OuterMode > 0)) then begin
      Result := Length(acgEffects);
      SetLength(acgEffects, Result + 1);
      acgEffects[Result] := NewGlow;
      NewGlow.Margin := Margin;
      NewGlow.MaskIndex := mi;
      NewGlow.Show(RealRect, Alpha, WndHandle)
    end
    else begin
      Result := -1;
      FreeAndNil(NewGlow);
    end;
  end;
end;


function ShowGlow(const RealRect: TRect; const MaskIndexes: array of integer; const Margin, Alpha: integer; WndHandle: HWND; SkinData: TsCommonData = nil): integer; overload;
var
  NewGlow: TacGlowEffect;
begin
  if acLayered and (SkinData <> nil) and (GetCapture = 0) and (Length(MaskIndexes) > 0) and (MaskIndexes[0] >= 0) then begin
    NewGlow := TacGlowEffect.Create;
    NewGlow.SkinData := SkinData;
    Result := Length(acgEffects);
    SetLength(acgEffects, Result + 1);
    acgEffects[Result] := NewGlow;
    NewGlow.Margin := Margin;
    NewGlow.MaskIndex := MaskIndexes[0];
    NewGlow.Show(RealRect, Alpha, WndHandle)
  end
  else
    Result := -1;
end;


procedure InitCircleEffect(GlowID: integer; Data: TacCircleEffectData);
begin
  with TacCircleEffect(acgEffects[GlowID]) do begin
    SizeChanging := Data.SizeChanging;
    SkinData := Data.SkinData;
    Center := Data.Center;
    RadiusFinish := Data.RadiusFinish;
    RadiusStart := Data.RadiusStart;
    AlphaFinish := Data.AlphaFinish;
    Color := Data.Color;
    State := Data.State;

    if RealAlpha = 0 then
      RealRadius := 0
    else
      RealRadius := RadiusStart;

    RealAlpha := Data.AlphaStart;
  end;
end;


function CreateCircleEffect(Data: TacCircleEffectData): integer;
begin
  if acLayered and (Data.SkinData <> nil) then begin
    Result := Length(acgEffects);
    SetLength(acgEffects, Result + 1);

    acgEffects[Result] := TacGlowEffect(TacCircleEffect.Create);
    InitCircleEffect(Result, Data);
    TacCircleEffect(acgEffects[Result]).Show;
  end
  else
    Result := -1;
end;


procedure CorrectColor(var Color: TColor; Alpha: byte);
begin
  if Color = -1 then begin // If not specified in the skin
    Color := acColorToRGB(clBtnText);
    Color := Color or (Alpha shl 24);
  end;
end;


const
  AlphaNormal = $11;
  AlphaDownCheck = $66;
  AlphaDownTrack = $22;


procedure ShowCircleIfNeeded(const SkinData: TsCommonData; Alpha: byte = MaxByte; DoAnimation: boolean = False);
var
  p: TPoint;
  WndHandle: HWND;
  ActiveRect: TRect;
  PrevState: integer;
  ge: TacCircleEffect;
  Data: TacCircleEffectData;

  function GetColorNdx: integer;
  begin
    case SkinData.COC of
      COC_TsListBox, COC_TsGroupBox:
        Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssCheckBox];

      COC_TsSlider, COC_TsTrackBar:
        Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssTrackBar]

      else
        Result := SkinData.SkinIndex;
    end;
  end;

  function BackColor: TColor;
  begin
    Result := SkinData.CommonSkinData.gd[GetColorNdx].Props[1].LightColor;
    CorrectColor(Result, AlphaNormal);
  end;

begin
  with SkinData do
    if SkinManager.Effects.AllowGlowing and SkinManager.IsValidSkinIndex(SkinData.SkinIndex) and not SkinManager.Options.NoMouseHover {and (GetCapture = 0) }then begin
      WndHandle := OwnerHandle;
      p := acMousePos;
      if (TrySendMessage(WndHandle, SM_ALPHACMD, MakeWParam(0, AC_GETMOUSEAREA), LParam(@ActiveRect)) <> 0) then
        if PtInRect(ActiveRect, p) and FMouseAbove then begin
          if not SkinData.SkinManager.Effects.AllowAnimation then
            DoAnimation := False;

          // Check if effect exists already
          if (SkinData.CircleID >= 0) then
            if acgEffects[SkinData.CircleID] is TacCircleEffect then
              ge := TacCircleEffect(acgEffects[SkinData.CircleID])
            else begin
              HideGlow(SkinData.CircleID);
              ge := nil;
            end
          else
            ge := nil;

          // Receive current state
          if (CircleID < 0) or not (csLButtonDown in FOwnerControl.ControlState) then
            Data.State := 1
          else
            Data.State := 2;

          // Prev state
          if ge = nil then
            PrevState := 0
          else
            PrevState := ge.State;

          if Data.State <> PrevState then begin
            // Init
            Data.SkinData := SkinData;
            Data.Center.X := ActiveRect.Left + WidthOf(ActiveRect) div 2;
            Data.Center.Y := ActiveRect.Top + HeightOf(ActiveRect) div 2;
            Data.Color := SkinData.CommonSkinData.gd[GetColorNdx].Props[Data.State].LightColor;

            case Data.State of
              1: begin // Mouse hover
                if CircleID < 0 then begin // Make an effect
//                  Alpha := iff(DoAnimation, 0, MaxByte);
                end;
                case Data.SkinData.COC of
                  COC_TsTrackBar: begin
                    Data.RadiusFinish := MinCircleRadius(Data.SkinData);
                    if PrevState = 0 then begin
                      Data.RadiusStart := 0;
                      Data.SizeChanging := scIncrease;
                    end
                    else begin
                      Data.RadiusStart := MaxCircleRadius(Data.SkinData);
                      Data.SizeChanging := scDecrease;
                    end;
                    Data.AlphaFinish := MaxByte;
                    Data.AlphaStart := MaxByte;
                  end;

                  COC_TsSlider: begin
                    Data.RadiusFinish := WidthOf(ActiveRect) * 3 div 5;
                    if PrevState = 0 then begin
                      Data.RadiusStart := 0;
                      Data.SizeChanging := scIncrease;
                    end
                    else begin
                      Data.RadiusStart := Data.RadiusFinish * 3 div 2;
                      Data.SizeChanging := scNotChanged;
                    end;
                    Data.AlphaFinish := MaxByte;
                    Data.AlphaStart := MaxByte;
                  end;

                  COC_TsListBox: begin
                    Data.RadiusFinish := MaxCircleRadius(Data.SkinData) * 4 div 5;
                    if PrevState = 0 then begin
                      Data.RadiusStart := 0;
                      Data.SizeChanging := scIncrease;
                    end
                    else begin
                      Data.Color := SkinData.CommonSkinData.gd[GetColorNdx].Props[2].LightColor; // Continue with MouseDown color
                      CorrectColor(Data.Color, AlphaDownCheck);
                      if ge <> nil then
                        Data.RadiusStart := Round(ge.RealRadius)
                      else
                        Data.RadiusStart := 0;

                      Data.SizeChanging := scIncrease;
                    end;
                    Data.AlphaFinish := MaxByte;
                    Data.AlphaStart := MaxByte;
                  end

                  else begin // CheckBox
                    Data.RadiusFinish := MaxCircleRadius(Data.SkinData);
                    if PrevState = 0 then begin
                      Data.RadiusStart := 10;
                      Data.SizeChanging := scIncrease;
                    end
                    else begin
                      Data.Color := SkinData.CommonSkinData.gd[GetColorNdx].Props[2].LightColor; // Continue with MouseDown color
                      CorrectColor(Data.Color, AlphaDownCheck);
                      if ge <> nil then
                        Data.RadiusStart := Round(ge.RealRadius)
                      else
                        Data.RadiusStart := 0;

                      Data.SizeChanging := scIncrease;
                    end;
                    Data.AlphaFinish := MaxByte;
                    Data.AlphaStart := MaxByte;
                  end;
                end;
                CorrectColor(Data.Color, AlphaNormal);
              end;

              2: begin // Click
                if ge = nil then
                  Exit;

                Data.SizeChanging := scIncrease;
                case Data.SkinData.COC of
                  COC_TsTrackBar: begin
                    Data.RadiusFinish := MaxCircleRadius(Data.SkinData);
                    Data.RadiusStart := MinCircleRadius(Data.SkinData);
                    Data.AlphaFinish := MaxByte;
                    Data.AlphaStart := MaxByte;
                    CorrectColor(Data.Color, AlphaDownTrack);
                  end;

                  COC_TsSlider: begin
                    Data.RadiusStart := WidthOf(ActiveRect) * 3 div 5;
                    Data.RadiusFinish := Data.RadiusStart * 3 div 2;
                    Data.AlphaFinish := MaxByte;
                    Data.AlphaStart := MaxByte;
                    CorrectColor(Data.Color, AlphaDownTrack);
                  end;

                  COC_TsListBox: begin
                    Data.RadiusFinish := MaxCircleRadius(Data.SkinData) * 4 div 5;
                    Data.RadiusStart := 0;
                    if ge.BackBmp <> nil then
                      FreeAndNil(ge.BackBmp);

                    // Prepare back bitmap
                    ge.BackBmp := MakeShadow(BackColor, Data.RadiusFinish, 0, 0, 0, Data.RadiusFinish div 4, False, True);
                    Data.AlphaFinish := iff(DoAnimation, 0, MaxByte);
                    Data.AlphaStart := MaxByte;
                  end

                  else begin // CheckBox
                    Data.RadiusFinish := MaxCircleRadius(Data.SkinData);
                    Data.RadiusStart := 0;
                    if ge.BackBmp <> nil then
                      FreeAndNil(ge.BackBmp);

                    // Prepare back bitmap
                    ge.BackBmp := MakeShadow(BackColor, Data.RadiusFinish, 0, 0, 0, Data.RadiusFinish div 4, False, True);
                    Data.AlphaFinish := iff(DoAnimation, 0, MaxByte);
                    Data.AlphaStart := MaxByte;
                  end;
                  CorrectColor(Data.Color, AlphaDownCheck);
                end;
              end;
            end;
            Data.AnimProc := UpdateCircle;
            if DoAnimation then
              StartLEAnimation(Data, True)
            else
              CircleID := CreateCircleEffect(Data);
          end;
        end
        else
          HideGlow(CircleID, DoAnimation);
    end;
end;


procedure HideGlow(var ID: integer; DoAnimation: boolean = False);
var
  sd: TsCommonData;
  i: integer;
begin
  if IsValidIndex(ID, Length(acgEffects)) and (acgEffects[ID] <> nil) then begin
    sd := acgEffects[ID].SkinData;
    if acgEffects[ID] is TacCircleEffect then
      DoAnimation := False;

    if DoAnimation and sd.SkinManager.Effects.AllowAnimation and not Application.Terminated and ((sd.AnimTimer = nil) or (sd.AnimTimer.ThreadType = TT_LAYEREDEFFECT)) then begin
      i := GetNewTimer(sd.AnimTimer, sd.FOwnerControl, 1);
      if sd.AnimTimer.State <> 0 then begin // Started already
        sd.AnimTimer.InitData(sd, i, UpdateGlowing_CB, 0);
        sd.AnimTimer.TimeHandler;
      end;
    end
    else begin
      if (acgEffects[ID].Wnd <> nil) then begin
        acgEffects[ID].Wnd.WindowProc := acgEffects[ID].OldWndProc;
        bGlowingDestroying := True;
        if (csDestroying in acgEffects[ID].SkinManager.ComponentState) or Application.Terminated then
          FreeAndNil(acgEffects[ID].Wnd)
        else begin
          acgEffects[ID].Wnd.Release;
          acgEffects[ID].Wnd := nil;
        end;
        bGlowingDestroying := False;
      end;

      if (ID >= 0) and (acgEffects[ID] <> nil) and not (acgEffects[ID] is TacCircleEffect) then begin
        acgEffects[ID].Free;
        acgEffects[ID] := nil;
      end;

      ID := -1;
      if sd.AnimTimer <> nil then
        StopTimer(sd);
    end;
  end;
end;


procedure HideCircle(var ID: integer);
var
  sd: TsCommonData;
begin
  if IsValidIndex(ID, Length(acgEffects)) and (acgEffects[ID] <> nil) then begin
    sd := acgEffects[ID].SkinData;
    if (acgEffects[ID].Wnd <> nil) then begin
      acgEffects[ID].Wnd.WindowProc := acgEffects[ID].OldWndProc;
      bGlowingDestroying := True;
      if (csDestroying in acgEffects[ID].SkinManager.ComponentState) or Application.Terminated then
        FreeAndNil(acgEffects[ID].Wnd)
      else begin
        acgEffects[ID].Wnd.Release;
        acgEffects[ID].Wnd := nil;
      end;
      bGlowingDestroying := False;
    end;
    ID := -1;
    if sd.CircleTimer <> nil then
      StopCircleTimer(sd);
  end;
end;


procedure SetGlowAlpha(ID: integer; aAlpha: byte);
begin
  if IsValidIndex(ID, Length(acgEffects)) and (acgEffects[ID] is TacGlowEffect) then begin
    TacGlowEffect(acgEffects[ID]).FAlpha := AAlpha;
    SetFormBlendValue(acgEffects[ID].Wnd.Handle, acgEffects[ID].AlphaBmp, AAlpha);
  end;
end;


procedure RepaintGlow(ID: integer);
begin
  if IsValidIndex(ID, Length(acgEffects)) and (acgEffects[ID] <> nil) then // Update glow
    acgEffects[ID].Repaint;
end;


procedure ClearGlows(ManualFreeing: boolean = False; SkipCircles: boolean = False);
var
  i, l: integer;
begin
  if not bGlowingDestroying then begin
    l := Length(acgEffects) - 1;
    if not SkipCircles then begin
      for i := 0 to l do
        if acgEffects[i] <> nil then begin
          if ManualFreeing then
            if acgEffects[i].Wnd <> nil then begin
              acgEffects[i].Wnd.WindowProc := acgEffects[i].OldWndProc;
              bGlowingDestroying := True;
              FreeAndNil(acgEffects[i].Wnd);
              bGlowingDestroying := False;
            end;

          if acgEffects[i].SkinData <> nil then
            if acgEffects[i].SkinData.GlowID = i then
              acgEffects[i].SkinData.GlowID := -1
            else
              if acgEffects[i].SkinData.CircleID = i then
                acgEffects[i].SkinData.CircleID := -1;

          acgEffects[i].Free;
        end;

      SetLength(acgEffects, 0);
    end
    else begin
      for i := 0 to l do
        if (acgEffects[i] <> nil) and (not SkipCircles or not (acgEffects[i] is TacCircleEffect)) then begin
          if ManualFreeing then
            if acgEffects[i].Wnd <> nil then begin
              acgEffects[i].Wnd.WindowProc := acgEffects[i].OldWndProc;
              bGlowingDestroying := True;
              FreeAndNil(acgEffects[i].Wnd);
              bGlowingDestroying := False;
            end;

          if acgEffects[i].SkinData <> nil then
            if acgEffects[i].SkinData.CircleID = i then
              acgEffects[i].SkinData.CircleID := -1{
            else
              if acgEffects[i].SkinData.GlowID = i then
                acgEffects[i].SkinData.GlowID := -1};

          acgEffects[i].Free;
          acgEffects[i] := nil;
        end;
    end;
  end;
end;


function UpdateCircle(Data: TObject; iIteration: integer): boolean; // Timer procedure
var
  wRect: TRect;
  OldAlpha: byte;
  OldSize: integer;
  Handle: THandle;
  sd: TsCommonData;
  ge: TacCircleEffect;
  OldCenter: TPoint;
begin
  Result := False;
  sd := TsCommonData(Data);
  if Assigned(sd.CircleTimer) and not sd.CircleTimer.Destroyed then begin
    Handle := sd.OwnerHandle;
    if sd.CircleID < 0 then
      ShowCircleIfNeeded(sd, 0);

    if (sd.CircleID >= 0) and (acgEffects[sd.CircleID] is TacCircleEffect) then begin // Effect may be not created for some reasons
      ge := TacCircleEffect(acgEffects[sd.CircleID]);
      // Receive coords of work area on the screen
      TrySendMessage(Handle, SM_ALPHACMD, MakeWParam(0, AC_GETMOUSEAREA), LParam(@wRect));
      with sd.CircleTimer do
        if PtInRect(wRect, acMousePos) or (csLButtonDown in sd.FOwnerControl.ControlState) then begin // If Mouse in active area still
          OldAlpha := Round(ge.RealAlpha);
          OldSize := Round(ge.RealRadius);
          OldCenter := ge.Center;
          Result := True;
          if not PtInRect(wRect, acMousePos) then begin // If dragged, set max size and alpha immediately
            ge.RealRadius := ge.RadiusFinish;
            ge.RealAlpha := ge.AlphaFinish;
          end;

          // Calc center point
          ge.Center.X := wRect.Left + WidthOf(wRect) div 2;
          ge.Center.y := wRect.Top + HeightOf(wRect) div 2;
          case ge.SizeChanging of {csLButtonDown in sd.FOwnerControl.ControlState then begin} // If must grow
            scIncrease: begin // Size increased
              // Calc new size
              if (sd.COC in [COC_TsCheckBox, COC_TsRadioButton, COC_TsListBox, COC_TsGroupBox]) {and (csLButtonDown in sd.FOwnerControl.ControlState)} then
                if csLButtonDown in sd.FOwnerControl.ControlState then begin
                  ge.RealRadius := ge.RealRadius + (ge.RadiusFinish - ge.RealRadius) / 7; // slower
                  if ge.AlphaFinish = 0 then
                    ge.RealAlpha := ge.RealAlpha - 5
                  else
                    ge.RealAlpha := ge.RealAlpha + 5;
                end
                else begin
                  ge.RealRadius := ge.RealRadius + (ge.RadiusFinish - ge.RealRadius) / 5; // slower
                  if ge.AlphaFinish = MaxByte then
                    ge.RealAlpha := ge.RealAlpha - 15
                  else
                    ge.RealAlpha := ge.RealAlpha + 15;
                end
              else begin
                ge.RealRadius := ge.RealRadius + (ge.RadiusFinish - ge.RealRadius) / 3;
                ge.RealAlpha := ge.RealAlpha + (ge.AlphaFinish - ge.RealAlpha) / 3;
              end;
              if ge.RealAlpha > MaxByte then
                ge.RealAlpha := MaxByte
              else
                if ge.RealAlpha < 0 then
                  ge.RealAlpha := 0;

              if ge.RealRadius >= ge.RadiusFinish - 1 then begin // Finish
                ge.RealRadius := ge.RadiusFinish;
                Result := False;
                StopCircleTimer(sd);
                if (sd.COC in [COC_TsCheckBox, COC_TsRadioButton, COC_TsListBox, COC_TsGroupBox]) and (ge.State = 1) and not (csLButtonDown in sd.FOwnerControl.ControlState) then begin
                  ge.Color := sd.CommonSkinData.gd[sd.SkinManager.SkinCommonInfo.Sections[ssCheckBox]].Props[1].LightColor;
                  CorrectColor(ge.Color, iff(sd.CircleTimer.State = 2, AlphaDownCheck, AlphaNormal));
                end;
              end;
            end;

            scDecrease: begin // If reduce size
              ge.RealRadius := ge.RealRadius - (ge.RealRadius - ge.RadiusFinish) / 3;
              ge.RealAlpha := max(0, ge.RealAlpha - (ge.RealAlpha - ge.AlphaFinish) / 3);

              if abs(ge.RealRadius - ge.RadiusStart) < 1 then begin // Finish
                ge.RealRadius := ge.RadiusStart;
                Result := False;
                StopCircleTimer(sd);
              end;
            end

            else begin // Not changed
              ge.RealRadius := ge.RadiusFinish;

              if (sd.COC in [COC_TsCheckBox, COC_TsRadioButton, COC_TsListBox, COC_TsGroupBox]) then
                if csLButtonDown in sd.FOwnerControl.ControlState then begin
                  ge.RealAlpha := ge.RealAlpha + (ge.AlphaFinish - ge.RealAlpha) / 7;
                end
                else begin
                  ge.RealAlpha := ge.RealAlpha + (ge.AlphaFinish - ge.RealAlpha) / 5;
                end
              else begin
                ge.RealAlpha := ge.RealAlpha + (ge.AlphaFinish - ge.RealAlpha) / 3;
              end;
              if ge.RealAlpha > ge.AlphaFinish then // Finish
                ge.RealAlpha := ge.AlphaFinish;
            end;
          end;
          if (OldSize <> Round(ge.RealRadius)) or (OldAlpha <> Round(ge.RealAlpha)) or (OldCenter.X <> ge.Center.X) or (OldCenter.Y <> ge.Center.Y) or (ge.AlphaBmp = nil) or (ge.AlphaBmp.Canvas.Brush.Color <> ge.Color) then begin
            if ge.AlphaBmp <> nil then
              ge.AlphaBmp.Canvas.Brush.Color := ge.Color;

            ge.Show;
          end;
        end
        else begin // Mouse leaved a work area
          Result := False;
          HideGlow(sd.CircleID);
        end;
    end;
  end;
end;


function CircleClickMode(Ctrl: TControl; sd: TsCommonData; EventProperty: TacAnimatEvents): boolean;
begin
  Result := (Ctrl <> nil) and not (csDesigning in Ctrl.ComponentState) and Ctrl.Enabled and EventEnabled(aeClick, EventProperty) and
            (sd.SkinManager <> nil) and (sd.SkinManager.AnimEffects.Buttons.ClickEffect <> ceWaveOut);
end;


procedure StartLEAnimation(Data: TacCircleEffectData; AllowAnimation: boolean);
begin
  if Data.SkinData.Skinned then begin
    if not SkinIsChanging(Data.SkinData) and AllowAnimation and Data.SkinData.SkinManager.Effects.AllowAnimation then begin // Animated
      with Data.SkinData do begin
        if CircleID < 0 then begin
          CircleID := CreateCircleEffect(Data); // Make invisible
        end
        else
          InitCircleEffect(CircleID, Data);
        // Start animation
        if CircleTimer = nil then
          CircleTimer := TacThreadedTimer.Create(FOwnerControl)
        else
          CircleTimer.OldState := Data.State;

        CircleTimer.Enabled := False;
        CircleTimer.Iterations := MaxInt; // Unlimited
        CircleTimer.Iteration := MaxInt div 2;

        if (CircleTimer.State <= 0) and (TacCircleEffect(acgEffects[CircleID]).RealRadius = 0) then
          TacCircleEffect(acgEffects[CircleID]).RealRadius := TacCircleEffect(acgEffects[CircleID]).RadiusStart;

        if (CircleTimer.State <= 0) and (TacCircleEffect(acgEffects[CircleID]).RealAlpha = 0) then
          TacCircleEffect(acgEffects[CircleID]).RealAlpha := Data.AlphaStart;

        CircleTimer.State := Data.State;
        CircleTimer.AnimData := Data.SkinData;
        case Data.SkinData.COC of
          COC_TsTrackBar, COC_TsSlider:
            if Data.State = 1 then
              CircleTimer.Interval := acTimerInterval
            else
              CircleTimer.Interval := 3 * acTimerInterval;

          else
            CircleTimer.Interval := 2 * acTimerInterval;
        end;

        CircleTimer.AnimProc := Data.AnimProc;
        CircleTimer.Enabled  := True;
        CircleTimer.TimeHandler;
      end;
    end
    else begin
      Data.SkinData.CircleID := CreateCircleEffect(Data); // Without animation
      TacCircleEffect(acgEffects[Data.SkinData.CircleID]).RealAlpha := Data.AlphaStart;
    end;
    if Data.SkinData.CircleTimer <> nil then
      Data.SkinData.CircleTimer.ThreadType := TT_LAYEREDEFFECT;
  end;
end;


function MinCircleRadius(sd: TsCommonData): integer;
begin
  Result := ScaleInt(iMinCircleRadius, sd);
end;


function MaxCircleRadius(sd: TsCommonData): integer;
begin
  Result := ScaleInt(iMaxCircleRadius, sd);
end;


procedure TacGlowEffect.Repaint;
var
  DC: hdc;
  h: HWND;
  op: byte;
  oNdx: integer;
  NewPos: TPoint;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  Coloring: TacColoring;
  FBlend: TBlendFunction;
  R, oRect, GlowRect, OutRect, MaxWidths: TRect;

  procedure CheckShadowSize;
  var
    Ndx: integer;
  begin
    with SkinData, CommonSkinData do
      if (OuterEffects.Visibility = ovMouseHovered) and (gd[SkinIndex].OuterMode > 0) then begin
        oNdx := gd[SkinIndex].OuterMask;
        if (oNdx < 0) and (gd[SkinIndex].OuterMode < 3) then begin // If common effect
          Ndx := gd[SkinIndex].OuterMode - 1;
          if oe[Ndx].Source = 0 then
            Exit;

          with oe[Ndx] do begin
            oNdx := Mask;
            oRect := Rect(OffsetL, OffsetT, OffsetR, OffsetB);
            op := Opacity;
          end;
          if oNdx < 0 then
            Exit;
        end
        else begin
          oRect := gd[SkinIndex].OuterOffset;
          op := gd[SkinIndex].OuterOpacity;
        end;
        MaxWidths.Left   := max(MaxWidths.Left,   oRect.Left);
        MaxWidths.Top    := max(MaxWidths.Top,    oRect.Top);
        MaxWidths.Right  := max(MaxWidths.Right,  oRect.Right);
        MaxWidths.Bottom := max(MaxWidths.Bottom, oRect.Bottom);
      end
      else begin
        oRect := MkRect;
        op := 0;
        oNdx := -1;
      end;
  end;

  procedure CheckOverlappedRects;
  var
    R: TRect;
    Wnd: THandle;
  begin
    if SkinData.FOwnerControl is TWinControl then begin
      Wnd := TWinControl(SkinData.FOwnerControl).Handle;
      while Wnd <> 0 do begin
        Wnd := GetWindow(Wnd, GW_HWNDPREV);
        if (Wnd <> 0) and IsWindowVisible(Wnd) then begin
          GetWindowRect(Wnd, R);
          if PtInRect(FRealRect, R.TopLeft) or PtInRect(FRealRect, R.BottomRight) or PtInRect(FRealRect, Point(R.Left, R.Bottom)) or PtInRect(FRealRect, Point(R.Right, R.Top)) then begin
            OffsetRect(R, -FRealRect.Left + MaxWidths.Left, -FRealRect.Top + MaxWidths.Top);
            FillRect32(AlphaBmp, R, 0, 0);
          end;
        end
        else
          Break;
      end;
    end;
  end;

begin
  InitBlendData(FBlend, MaxByte);
  if Wnd = nil then begin
    Wnd := TacGlowForm.CreateNew(nil);
    Wnd.ControlStyle := Wnd.ControlStyle - [csCaptureMouse];
    OldWndProc := Wnd.WindowProc;
    Wnd.WindowProc := NewWndProc;
  end;
  if (GetWindowLong(FWndHandle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0) or (SkinData.FOwnerObject = nil) then
    h := HWND_TOPMOST
  else begin
    h := GetNextWindow(FWndHandle, GW_HWNDPREV);
    if h = 0 then
      h := FWndHandle;
  end;
  if FSkinData.FOwnerControl is TWinControl then begin
    GetWindowRect(TWinControl(FSkinData.FOwnerControl).Handle, oRect);
    DC := GetWindowDC(TWinControl(FSkinData.FOwnerControl).Handle);
    try
      GetClipBox(DC, OutRect);
      if IsRectEmpty(OutRect) then
        R := oRect
      else begin
        R.Left := oRect.Left + OutRect.Left;
        R.Top := oRect.Top + OutRect.Top;
        R.Right := oRect.Left + OutRect.Right;
        R.Bottom := oRect.Top + OutRect.Bottom;
      end;
    finally
      ReleaseDC(TWinControl(FSkinData.FOwnerControl).Handle, DC);
    end;
  end
  else
    R := FRealRect;

  if MaskIndex >= 0 then begin // Glow effect is available
    MaxWidths.Left   := SkinData.CommonSkinData.ma[MaskIndex].WL - Margin;
    MaxWidths.Top    := SkinData.CommonSkinData.ma[MaskIndex].WT - Margin;
    MaxWidths.Right  := SkinData.CommonSkinData.ma[MaskIndex].WR - Margin;
    MaxWidths.Bottom := SkinData.CommonSkinData.ma[MaskIndex].WB - Margin;
  end
  else
    MaxWidths := MkRect;

  CheckShadowSize;
  FBmpSize.cx := WidthOf (R, True) + MaxWidths.Left + MaxWidths.Right;
  FBmpSize.cy := HeightOf(R, True) + MaxWidths.Top + MaxWidths.Bottom;

  NewPos := MkPoint(R.Left - MaxWidths.Left, R.Top - MaxWidths.Top);

  Wnd.Width := FBmpSize.cx;
  Wnd.Height := FBmpSize.cy;
  Wnd.HandleNeeded;
  if Wnd.HandleAllocated then begin
    // Prepare Bmp
    if AlphaBmp = nil then
      AlphaBmp := CreateBmp32(FBmpSize.cx, FBmpSize.cy)
    else begin
      AlphaBmp.Width := FBmpSize.cx;
      AlphaBmp.Height := FBmpSize.cy;
    end;
    FillDC(AlphaBmp.Canvas.Handle, MkRect(AlphaBmp), 0);
    // Paint shadow if needed
    if oNdx >= 0 then begin
      OutRect.Left := MaxWidths.Left - oRect.Left;
      OutRect.Top := MaxWidths.Top - oRect.Top;
      OutRect.Right := FBmpSize.cx - MaxWidths.Right + oRect.Right;
      OutRect.Bottom := FBmpSize.cy - MaxWidths.Bottom + oRect.Bottom;
      DrawSkinRect32Ex(AlphaBmp, OutRect, EmptyCI, SkinData.CommonSkinData.ma[oNdx], 0, oRect, op);
    end;
    // Paint glow
    if MaskIndex >= 0 then begin
      GlowRect.Left := MaxWidths.Left - SkinData.CommonSkinData.ma[MaskIndex].WL + Margin;
      GlowRect.Top := MaxWidths.Top - SkinData.CommonSkinData.ma[MaskIndex].WT + Margin;
      GlowRect.Right := FBmpSize.cx - MaxWidths.Right + SkinData.CommonSkinData.ma[MaskIndex].WR - Margin;
      GlowRect.Bottom := FBmpSize.cy - MaxWidths.Bottom + SkinData.CommonSkinData.ma[MaskIndex].WB - Margin;
      DrawSkinRect(AlphaBmp, GlowRect, EmptyCI, SkinData.CommonSkinData.ma[MaskIndex], min(integer(SkinData.FFocused), SkinData.CommonSkinData.ma[MaskIndex].ImageCount - 1), False);
    end;
    CheckOverlappedRects;

    // Change color if needed
    if SendAMessage(SkinData.OwnerHandle, AC_GETCOLORTONE, LParam(@Coloring)) = 0 then
      Coloring.ColorToneBG := clNone;

    UpdateBmpColors(AlphaBmp, SkinData, False, integer(ControlIsActive(SkinData)), Coloring.ColorToneBG);

    SetWindowLong(Wnd.Handle, GWL_EXSTYLE, GetWindowLong(Wnd.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TRANSPARENT);
    Wnd.Left := NewPos.X;
    Wnd.Top := NewPos.Y;
    FBmpTopLeft := MkPoint;
    FBlend.SourceConstantAlpha := FAlpha;
    DC := GetDC(0);
    try
      UpdateLayeredWindow(Wnd.Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, 0, @FBlend, ULW_ALPHA);
    finally
      ReleaseDC(0, DC);
    end;
    SetWindowPos(Wnd.Handle, h, NewPos.X, NewPos.Y, 0, 0,
      SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_NOSIZE {or SWP_NOZORDER {Preventing of windows positions changing});
  end;
end;


procedure TacGlowEffect.Show(RealRect: TRect; const Alpha: integer; WndHandle: HWND);
begin
  if Wnd = nil then begin
    FRealRect := RealRect;
    FAlpha := Alpha;
    FWndHandle := WndHandle;
    Repaint;
  end;
end;


constructor TacLayeredEffect.Create;
begin
  Wnd := nil;
  Destroyed := False;
end;


destructor TacLayeredEffect.Destroy;
begin
  Destroyed := True;
  if Wnd <> nil then begin
    Wnd.WindowProc := OldWndProc;
    if (csDestroying in SkinManager.ComponentState) or Application.Terminated then
      FreeAndNil(Wnd)
    else
      Wnd.Release;

    Wnd := nil;
  end;
  if AlphaBmp <> nil then
    FreeAndNil(AlphaBmp);

  inherited;
end;


procedure TacLayeredEffect.NewWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_NCHITTEST:
      Message.Result := HTTRANSPARENT;
    else
      OldWndProc(Message);
  end;
end;


procedure TacLayeredEffect.SetSkinData(const Value: TsCommonData);
begin
  FSkinData := Value;
  SkinManager := FSkinData.SkinManager;
end;


constructor TacCircleEffect.Create;
begin
  BackBmp := nil;
  inherited;
end;


destructor TacCircleEffect.Destroy;
begin
  FreeAndNil(BackBmp);
  if (SkinData.COC = COC_TsTrackBar) and (SkinData.FOwnerControl is TsTrackBar) then
    TsTrackBar(SkinData.FOwnerControl).HideToolTip;

  inherited;
end;


procedure TacCircleEffect.Repaint;
var
  wRect: TRect;
begin
  TrySendMessage(SkinData.OwnerHandle, SM_ALPHACMD, MakeWParam(0, AC_GETMOUSEAREA), LParam(@wRect));
  with SkinData.CircleTimer do
    begin // If Mouse in active area still
//      if not PtInRect(wRect, acMousePos) then // If dragged, set max size immediately
//        ge.RealRadius := ge.RadiusFinish;
      // Calc center point
      Center.X := wRect.Left + WidthOf(wRect) div 2;
      Center.y := wRect.Top + HeightOf(wRect) div 2;
      Show;
    end;
end;


procedure TacCircleEffect.Show;
var
  DC: hdc;
  h: HWND;
  R: TRect;
  Alpha: byte;
  Bmp: TBitmap;

  NewPos: TPoint;
  FBmpSize: TSize;
  Form: TCustomForm;
  FBmpTopLeft: TPoint;
  Data: TacToolTipData;
  FBlend: TBlendFunction;
  wSize, hSize, rMin, rMax, Size: integer;
begin
  if Wnd = nil then begin
    Wnd := TacGlowForm.CreateNew(nil);
    Wnd.ControlStyle := Wnd.ControlStyle - [csCaptureMouse];
    OldWndProc := Wnd.WindowProc;
    Wnd.WindowProc := NewWndProc;
  end;
  Wnd.HandleNeeded;
  if Wnd.HandleAllocated then begin
    SetWindowLong(Wnd.Handle, GWL_EXSTYLE, GetWindowLong(Wnd.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TRANSPARENT);
    Size := Round(RealRadius);
    Alpha := Round(RealAlpha);
    // Prepare Bmp
    FreeAndNil(AlphaBmp);
    if Size > 1 then begin
      AlphaBmp := MakeShadow(Color, Size, 0, 0, 0, Size div 5, False, True);
      if BackBmp <> nil then begin
        Bmp := AlphaBmp;
        AlphaBmp := CreateBmpLike(BackBmp);
        BitBlt(AlphaBmp.Canvas.Handle, 0, 0, AlphaBmp.Width, AlphaBmp.Height, BackBmp.Canvas.Handle, 0, 0, SRCCOPY);
        PaintBmpRect32(AlphaBmp, Bmp, MkRect(Bmp), Point((AlphaBmp.Width - Bmp.Width) div 2, (AlphaBmp.Height - Bmp.Height) div 2), Alpha);
        Bmp.Free;
      end;
      FBmpSize.cx := AlphaBmp.Width;
      FBmpSize.cy := AlphaBmp.Height;

{$IFNDEF ALITE}
      if SkinData.COC in [COC_TsSlider] then begin
        if SkinData.FOwnerControl is TsSlider then begin
          R := TsSlider(SkinData.FOwnerControl).ThumbRect;
          wSize := WidthOf(R);
          hSize := HeightOf(R);

          Bmp := CreateBmpLike(SkinData.FCacheBmp);
          BitBlt(Bmp.Canvas.Handle, R.Left + (wSize - AlphaBmp.Width) div 2, R.Top + (hSize - AlphaBmp.Height) div 2, AlphaBmp.Width, AlphaBmp.Height, AlphaBmp.Canvas.Handle, 0, 0, SRCCOPY);
          TsSlider(SkinData.FOwnerControl).PaintThumb(Bmp, TsSlider(SkinData.FOwnerControl).SliderOn);
          Bmp.Free;
        end;
      end;
{$ENDIF}

      NewPos.X := Center.X - FBmpSize.cx div 2;
      NewPos.Y := Center.Y - FBmpSize.cy div 2;

      Wnd.SetBounds(NewPos.X, NewPos.Y, FBmpSize.cx, FBmpSize.cy);
{
      Wnd.Width := FBmpSize.cx;
      Wnd.Height := FBmpSize.cy;
      Wnd.Left := NewPos.X;
      Wnd.Top := NewPos.Y;
}
      if AlphaBmp = nil then
        Exit;

      FBmpTopLeft := MkPoint;

      InitBlendData(FBlend, MaxByte);
      DC := GetDC(0);
      try
        UpdateLayeredWindow(Wnd.Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, 0, @FBlend, ULW_ALPHA);
      finally
        ReleaseDC(0, DC);
      end;

      Form := GetParentForm(SkinData.FOwnerControl);
      if (Form <> nil) or (GetWindowLong(Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0) then
        h := HWND_TOPMOST
      else begin
        h := GetNextWindow(Form.Handle, GW_HWNDPREV);
        if h = 0 then
          h := FWndHandle;
      end;

      SetWindowPos(Wnd.Handle, h, NewPos.X, NewPos.Y, 0, 0,
        SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_NOSIZE {or SWP_NOZORDER {Preventing of windows positions changing});

      FreeAndNil(AlphaBmp);

      if (SkinData.COC = COC_TsTrackBar) and (SkinData.FOwnerControl is TsTrackBar) then begin // Hint info
        Data.SrcPoint := Center;
        Data.Alpha := MaxByte;
        rMin := MinCircleRadius(SkinData);
        rMax := MaxCircleRadius(SkinData);
        if State = 2 then
          Data.Distance := rMax
        else
          Data.Distance := min(Size, rMin) + (rMax - rMin);

        dec(Data.Distance, rMax div 5 - 1);
        TsTrackBar(SkinData.FOwnerControl).ShowToolTip(Data);
      end;
    end;
  end;
end;


initialization

finalization
  ClearGlows;

end.
