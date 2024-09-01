unit sStyleSimply;
{$I sDefs.inc}

interface

uses
  Windows, Graphics, Classes, Controls, SysUtils, Forms, StdCtrls, Messages,
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  acntTypes, acntUtils, sConst, sSkinProps;


type
  TacSection = (
    ssMenuItem, ssCheckBox, ssSpeedButton, ssSpeedButton_Small, ssToolButton, ssTransparent, ssGroupBox, ssDivider, ssWebBtn, // <- Transparent sections
    ssButton, ssComboNoEdit, ssButtonHuge, ssToolBar, ssEdit, ssGauge, ssSelection, ssStatusBar, ssPanel, ssPanelLow, ssTrackBar,
    ssProgressV, ssDragBar, ssGripH, ssGripV, ssProgressH, ssTabSheet, ssComboBox, ssCaption,
    ssMainMenu, ssSplitter, ssComboBtn, ssFormTitle, ssHint,
    ssDividerV, ssPageControl, ssTabTop, ssBarPanel, ssBarTitle, ssDialog, ssForm,
    ssScrollBtnLeft, ssScrollBtnTop, ssScrollBtnRight, ssScrollBtnBottom, ssScrollBar1H, ssScrollBar1V, ssScrollBar2H, ssScrollBar2V,
    ssScrollSliderH, ssScrollSliderV,
    ssSlider_Off, ssSlider_On, ssThumb_On, ssThumb_Off,
    // Auxiliary
    ssMenuCaption, ssDialogTitle, ssMenuButton, ssMenuLine, ssFormHeader,
    ssTBBtn, ssTBMenuBtn, ssTBTab, ssColHeader, ssColHeaderA, ssColHeaderL, ssColHeaderR, ssUpDown
  );


  TacTitleGlyph =
    (tgCloseAlone,
     tgClose, tgMax, tgMin, tgNormal, tgHelp,
     tgSmallClose, tgSmallMax, tgSmallMin, tgSmallNormal, tgSmallHelp, tgUser, tgNone);

  TacTitleGlowArray = array [tgCloseAlone..tgSmallClose] of TacIntArray;

  TacDefSectionIndex = (dsiEdit, dsiButton);

const
  acGlowedGlyphs = [tgCloseAlone..tgNormal];

  acAuxReplacement: array[ssMenuCaption..ssUpDown] of TacSection = (
    ssToolBar, ssFormTitle, ssSpeedButton, ssMenuLine, ssFormHeader,
    ssToolButton, ssTBMenuBtn, ssTabTop, ssButton, ssColHeader, ssColHeader, ssColHeader, ssButton
  );


  acSectNames: array [TacSection] of string = (
    s_MenuItem, s_CheckBox, s_SpeedButton, s_SpeedButton_Small, s_ToolButton, s_Transparent, s_GroupBox, s_Divider, s_WebBtn,
    s_Button, s_ComboNoEdit, s_ButtonHuge, s_ToolBar, s_Edit, s_Gauge, s_Selection, s_StatusBar, s_Panel, s_PanelLow, s_TrackBar,
    s_ProgressV, s_DragBar, s_GripH, s_GripV, s_ProgressH, s_TabSheet, s_ComboBox, s_Caption,
    s_MainMenu, s_Splitter, s_ComboBtn, s_FormTitle, s_Hint,
    s_DividerV, s_PageControl, s_TabTop, s_BarPanel, s_BarTitle, s_Dialog, s_Form,
    s_ScrollBtnLeft, s_ScrollBtnTop, s_ScrollBtnRight, s_ScrollBtnBottom, s_ScrollBar1H, s_ScrollBar1V, s_ScrollBar2H, s_ScrollBar2V,
    s_ScrollSliderH, s_ScrollSliderV,
    s_Slider_Off, s_Slider_On, s_Thumb_On, s_Thumb_Off,
    // Auxiliary
    s_MenuCaption, s_DialogTitle, s_MenuButton, s_MenuLine, s_FormHeader,
    s_TBBtn, s_TBMenuBtn, s_TBTab, s_ColHeader, s_ColHeaderA, s_ColHeaderL, s_ColHeaderR, s_UpDown
  );


  acTitleGlyphs: array [TacTitleGlyph] of string =
    (s_BorderIconCloseAlone,
     s_BorderIconClose, s_BorderIconMaximize, s_BorderIconMinimize, s_BorderIconNormalize, s_BorderIconHelp,
     s_SmallIconClose, s_SmallIconMaximize, s_SmallIconMinimize, s_SmallIconNormalize, s_SmallIconHelp, s_TitleButtonMask, '');


  ProgArray:  array [boolean] of string = (s_ProgVert, s_ProgHorz);
  ThickArray: array [boolean] of string = (s_TickVert, s_TickHorz);
                                          //   0        1            2           3           4          5          6          7            8            9            10        11       12        13            14           15           16           17           18
  acDefSections: array [0..18] of string = (s_Edit, s_Button, s_Transparent, s_ToolBar, s_CheckBox, s_ComboBox, s_Gauge, s_Slider_On, s_GroupBox, s_PageControl, s_TabSheet, s_Panel, s_Form, s_PanelLow, s_SpeedButton, s_Splitter, s_StatusBar, s_TrackBar, s_ComboNoEdit);

type
  TacConstElementData = record
    SkinIndex,
    MaskIndex,
    GlyphIndex: integer;
    BGIndex: array [0..ac_MaxPropsIndex] of integer;
    SkinSection: string;
  end;


  TacTrackBarData = record
    SkinIndex,
    MaskIndex,
    GlyphIndex: integer;
    BGIndex: array [0..ac_MaxPropsIndex] of integer;
    TickIndex,
    ProgIndex,
    SliderIndex: integer;
  end;

  TacIntSections  = array [TacSection]   of integer;
  TacSideElements = array [TacSide]      of TacConstElementData;
  TacBoolElements = array [boolean]      of TacConstElementData;
  TacAllTabs      = array [TacTabLayout] of TacSideElements;


  TacFormCallBack = procedure(SkinProvider: TComponent; Data: integer = 0);

procedure CopyExForms(SkinManager: TComponent);
procedure LockForms  (SkinManager: TComponent);
procedure UnLockForms(SkinManager: TComponent; Repaint: boolean = True);
procedure AppBroadCastS(var Message);
procedure SendToHooked (var Message);
procedure IntSkinForm  (Form: TCustomForm);
procedure IntUnSkinForm(Form: TCustomForm);
procedure IterateForms(sm: TComponent; FormCallBack: TacFormCallBack; Data: integer = 0);

function SectionInArray(const Sections: TacIntSections; const Value: integer; RangeMin: TacSection = ssMenuItem; RangeMax: TacSection = ssWebBtn): boolean;


var
  AppIcon, AppIconLarge: TIcon;
  HookedComponents: array of TComponent;


implementation

uses
  ShellAPI, math,
  sVclUtils, sMessages, sCommonData, sSkinProvider, sSkinManager, acDials, sGradient;


function SectionInArray(const Sections: TacIntSections; const Value: integer; RangeMin: TacSection = ssMenuItem; RangeMax: TacSection = ssWebBtn): boolean;
var
  Section: TacSection;
begin
  for Section := RangeMin to RangeMax do
    if Sections[Section] = Value then begin
      Result := True;
      Exit;
    end;

  Result := False;
end;


procedure IterateForms(sm: TComponent; FormCallBack: TacFormCallBack; Data: integer = 0);
var
  i: integer;
  sp: TsSkinProvider;
begin
  i := 0;
  while i <= Length(HookedComponents) - 1 do begin
    if HookedComponents[i] is TCustomForm then
      with TForm(HookedComponents[i]) do
        if (WindowState <> wsMinimized) and (FormStyle <> fsMDIChild) and (Parent = nil) and HandleAllocated and Visible then begin
          sp := TsSkinProvider(SendAMessage(TForm(HookedComponents[i]), AC_GETPROVIDER));
          if (sp <> nil) and (sp.SkinData.SkinManager = sm) then
            FormCallBack(sp, Data);
        end;

    inc(i);
  end;
end;


type
  TAccessSkinProvider = class(TsSkinProvider);

procedure CopyExFormsCB(sp: TComponent; Data: integer);
begin
  with TAccessSkinProvider(sp) do begin
    if BorderForm <> nil then
      with BorderForm do
        if AForm <> nil then begin
          TAccessSkinProvider(sp).FormState := TAccessSkinProvider(sp).FormState or FS_CHANGING;
          if CoverBmp <> nil then
            FreeAndNil(CoverBmp);

          CoverBmp := GetFormImage(TAccessSkinProvider(sp));
          TAccessSkinProvider(sp).FormState := TAccessSkinProvider(sp).FormState and not FS_CHANGING;
          TAccessSkinProvider(sp).SkinData.BGChanged := True;
        end;

    TAccessSkinProvider(sp).FInAnimation := True;
  end;
end;


procedure CopyExForms(SkinManager: TComponent);
begin
  with TsSkinManager(SkinManager) do
    if not IsIconic(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}) and
        ([csDesigning, csLoading, csReading] * ComponentState = []) then begin

      if IsDefault and (AnimEffects.SkinChanging.Active) then
        InAnimationProcess := True;

      IterateForms(TsSkinManager(SkinManager), CopyExFormsCB);
    end;
end;


procedure LockFormsCB(sp: TComponent; Data: integer);
var
  Alpha: integer;
begin
  with TAccessSkinProvider(sp) do begin
    FInAnimation := True;
    FormState := FormState or FS_LOCKED;
    if BorderForm <> nil then
      with BorderForm do begin
        if (AForm <> nil) and not Locked then begin
          if CoverBmp <> nil then begin
{$IFDEF DELPHI7UP}
            if Form.AlphaBlend then
              Alpha := Form.AlphaBlendValue
            else
{$ENDIF}
              Alpha := MaxByte;


            ExBorderShowing := True;
            SetWindowRgn(AForm.Handle, 0, False);
            SetFormBlendValue(AForm.Handle, CoverBmp, Alpha);
            SetWindowPos(AForm.Handle, TopWndAfter, AForm.Left, AForm.Top, 0, 0, SWPA_SHOWZORDER);
//            ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
//            AForm.Show;
//            AForm.Left := AForm.Left + 200;
//            RedrawWindow(AForm.Handle, nil, 0, RDWA_ALLNOW);
//            SetFormBlendValue(AForm.Handle, CoverBmp, Alpha);
          end;
//          SetWindowRgn(AForm.Handle, MakeRgn, False);
        end;

//    if Form.BorderStyle <> bsNone then
        Form.Perform(WM_SETREDRAW, 0, 0);
        ExBorderShowing := False;
      end
    else
      Form.Perform(WM_SETREDRAW, 0, 0);
  end;
end;


procedure LockForms(SkinManager: TComponent);
begin
  with TsSkinManager(SkinManager) do
    if not IsIconic(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}) then begin
      if IsDefault and (AnimEffects.SkinChanging.Active) then
        InAnimationProcess := True;

      IterateForms(SkinManager, LockFormsCB);
    end;
end;


var
  iLatestTitleHeight: integer = 0;


procedure AllowRedrawCB(sp: TComponent; Data: integer);
begin
  if TAccessSkinProvider(sp).FormState and FS_LOCKED <> 0 then
    TAccessSkinProvider(sp).Form.Perform(WM_SETREDRAW, 1, 0);
end;


procedure DoRepaintCB(sp: TComponent; Data: integer);
var
  ActWnd: hwnd;
  AlphaValue: byte;
begin
  with TAccessSkinProvider(sp), SkinData.SkinManager, AnimEffects.SkinChanging do
    if (Form.WindowState <> wsMinimized) and (Form.ParentWindow = 0) then
      if FormState and FS_LOCKED <> 0 then begin
        FInAnimation := False;
        if Data = 1 then begin
          if Active then begin
{$IFDEF DELPHI7UP}
            if Form.AlphaBlend then
              AlphaValue := Form.AlphaBlendValue
            else
{$ENDIF}
              AlphaValue := MaxByte;

            if DrawNonClientArea then
              case Mode of
                atAero: AnimShowControl(Form, Time, AlphaValue, atcAero)
                else    AnimShowControl(Form, Time, AlphaValue, atcFade)
              end
            else
              AnimShowControl(Form, 0);
          end;
          FormState := FormState and not FS_CHANGING;
          if (BorderForm <> nil) and (Form.WindowState = wsMaximized) then // Title height may be changed
            if iLatestTitleHeight <> ActualTitleHeight then begin // If height of title is really changed
              iLatestTitleHeight := ActualTitleHeight;
              ActWnd := GetActiveWindow;
              SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) and not WS_VISIBLE);
              ShowWindow(Form.Handle, SW_SHOWMAXIMIZED);
              SetWindowLong(Form.Handle, GWL_STYLE, GetWindowLong(Form.Handle, GWL_STYLE) or WS_VISIBLE);
              if ActWnd <> 0 then
                SetActiveWindow(ActWnd);
            end;

          if DrawNonClientArea then begin
            if {(Win32MajorVersion < 6) or }not AeroIsEnabled then
              SetWindowRgn(Form.Handle, 0, False); // Fixing of Aero bug

            sSkinProvider.FillArOR(TAccessSkinProvider(sp)); // Update rgn data for skin
            sSkinProvider.UpdateRgn(TAccessSkinProvider(sp), True, True); // Update of native borders
            RedrawWindow(Form.Handle, nil, 0, RDWA_ALLNOW); // Redraw main form under layered form
            if SkinData.TranspMode > 0 then
              IterateControls(Form, 0, RepaintOpaque);

            if (BorderForm <> nil) and not BorderForm.Locked then begin
              SetWindowRgn(BorderForm.AForm.Handle, BorderForm.MakeRgn, False); // Avoiding a flickering
              BorderForm.UpdateExBordersPos;
            end;
          end
          else
            RedrawWindow(Form.Handle, nil, 0, RDWA_ALLNOW);

          FInAnimation := False;
        end
        else begin
          FormState := FormState and not FS_CHANGING;
          SendMessage(Form.Handle, WM_SETREDRAW, 1, 0);
          if Data = 1 then
            RedrawWindow(Form.Handle, nil, 0, RDWA_ALLNOW);
        end;
        FormState := FormState and not FS_LOCKED;
      end;
end;


procedure UnlockForms(SkinManager: TComponent; Repaint: boolean = True);
begin
  if not IsIconic(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}) then begin
    if TsSkinManager(SkinManager).IsDefault then
      InAnimationProcess := False;

    IterateForms(TsSkinManager(SkinManager), AllowRedrawCB);
    IterateForms(TsSkinManager(SkinManager), DoRepaintCB, integer(Repaint));
  end;
end;


procedure AppBroadCastS(var Message);
var
  i: integer;
begin
  i := Application.ComponentCount - 1;
  while i >= 0 do begin // Order has been changed (Z-order is valuable now in skins changing or activating)
    if i >= Application.ComponentCount then
      Exit;
    // Sending a message to all forms (non-MDIChild first)
    if Application.Components[i] is TForm then
      with TForm(Application.Components[i]), TMessage(Message) do
        if FormStyle <> fsMDIChild then
          if ([csDestroying, csLoading] * ComponentState = []) and not WndIsSkinned(Handle) then begin
            // Form must be handled first
            SendMessage(Handle, Msg, WParam, LParam);
            AlphaBroadCast(TWinControl(Application.Components[i]), Message);
            // Message sending to client if form is a MDIForm
            if FormStyle = fsMDIForm then
              SendMessage(ClientHandle, Msg, WParam, LParam);
          end;

    dec(i);
  end;
  SendToHooked(Message);
end;


procedure SendToHooked(var Message);
var
  i: integer;
begin
  i := 0;
  while i < Length(HookedComponents) do begin
    if HookedComponents[i] is TForm then
      with TMessage(Message), TForm(HookedComponents[i]) do
        if not (csDestroying in ComponentState) then begin
          SendMessage(Handle, Msg, WParam, LParam);
          AlphaBroadCast(TForm(HookedComponents[i]), Message);
          if FormStyle = fsMDIForm then
            SendMessage(ClientHandle, Msg, WParam, LParam);
        end;

    inc(i);
  end;
  if acSupportedList <> nil then
    for i := 0 to acSupportedList.Count - 1 do
      if acSupportedList[i] <> nil then
        with TMessage(Message), TacProvider(acSupportedList[i]) do
          if (ListSW <> nil) and IsWindowVisible(ListSW.CtrlHandle) then
            SendMessage(ListSW.CtrlHandle, Msg, WParam, LParam);
end;


procedure IntSkinForm(Form: TCustomForm);
begin
  SetLength(HookedComponents, Length(HookedComponents) + 1);
  HookedComponents[Length(HookedComponents) - 1] := Form;
end;


procedure IntUnSkinForm(Form: TCustomForm);
var
  i, l: integer;
begin
  l := Length(HookedComponents) - 1;
  for i := 0 to l do
    if HookedComponents[i] = Form then begin
      HookedComponents[i] := HookedComponents[l];
      HookedComponents[l] := nil;
    end;

  if l >= 0 then
    if HookedComponents[l] = nil then
      SetLength(HookedComponents, l)
end;


initialization
  AppIcon      := GetIconForFile(Application.ExeName, SHGFI_SMALLICON);
  AppIconLarge := GetIconForFile(Application.ExeName, SHGFI_LARGEICON);


finalization
  if Assigned(AppIcon) then
    FreeAndNil(AppIcon);

  if Assigned(AppIconLarge) then
    FreeAndNil(AppIconLarge);

  SetLength(HookedComponents, 0);

end.





