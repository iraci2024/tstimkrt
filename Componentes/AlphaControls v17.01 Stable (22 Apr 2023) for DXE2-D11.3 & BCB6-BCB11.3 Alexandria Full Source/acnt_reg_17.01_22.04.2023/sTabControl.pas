unit sTabControl;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, CommCtrl,
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  {$IFNDEF DELPHI5} types, {$ENDIF}
  acSBUtils, sCommonData, sConst;

type
{$IFNDEF NOTFORHELP}
  TacTabData = record
    GlyphRect,
    TextRect,
    FocusRect:      TRect;
    TextSize:       TSize;
    TextPos:        TPoint;
  end;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsTabControl = class(TTabControl)
  private
    procedure SetCurItem(const Value: integer);
  protected
    TabsBG: TBitmap;
    BtnSW: TacSpinWnd;
    FCurItem: integer;
    FCommonData: TsCommonData;
    ChangedSkinSection: string;
    function GetClRect: TRect;
    function TabsRect: TRect;
    function TabsBGRect: TRect;
    function TabRow(TabIndex: integer): integer;
    procedure CheckUpDown;
    function PageRect: TRect;
    function IsLeftToRight: boolean;

    function StdTabRect(Index: integer): TRect;
    function SkinTabRect(Index: integer; Active: boolean): TRect;

    function GetTabUnderMouse(p: TPoint): integer;
    procedure TrySetNewTab(aIndex: integer);

    procedure DrawStdTab(aTabIndex: Integer; State: integer; DC: hdc);
    procedure DrawSkinTabs(Bmp: TBitmap; PaintActive: boolean);
    procedure DrawSkinTab(Index: Integer; State: integer; Bmp: TBitmap; OffsetPoint: TPoint); overload;
    procedure DrawSkinTab(Index: Integer; State: integer; DC: hdc); overload;

    procedure InitTabContentData(Canvas: TCanvas; ATabIndex: integer; BmpRect: TRect; State: integer; var Data: TacTabData);

    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure RepaintTab(i, State: integer; TabDC: hdc = 0);

    procedure AC_WMNCPaint(var Message: TWMPaint);
    procedure AC_WMPaint(var Message: TWMPaint);
    procedure PrepareCache;
  public
    procedure AfterConstruction; override;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    property CurItem: integer read FCurItem write SetCurItem default -1;
  published
    property SkinData: TsCommonData read FCommonData write FCommonData;
  end;
{$ENDIF} // NOTFORHELP


implementation

uses
  math, ExtCtrls,
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  acAlphaImageList, sAlphaGraph, sStyleSimply, sSkinProps, sGraphUtils, acntUtils, sMessages, sVCLUtils,
  sSkinManager, sThirdParty;


procedure TsTabControl.PrepareCache;
var
  DC: hdc;
  R: TRect;
  CI: TCacheInfo;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
{$ENDIF}
begin
  InitCacheBmp(SkinData);
  FCommonData.FCacheBmp.Canvas.Lock;
  R := PageRect;
  if SkinData.Skinned then begin
    if Tabs.Count > 0 then
      DrawSkinTabs(FCommonData.FCacheBmp, False);

    CI := GetParentCache(SkinData);
    PaintItem(SkinData.SkinIndex, CI, False, 0, R, Point(Left + R.Left, Top + R.Top), SkinData.FCacheBmp, SkinData.CommonSkinData);
    SkinData.PaintOuterEffects(Self, MkPoint);
    if TabIndex >= 0 then begin
      R := SkinTabRect(TabIndex, True);
      DrawSkinTab(TabIndex, 2, FCommonData.FCacheBmp.Canvas.Handle);
    end;
  end
  else begin
    DC := FCommonData.FCacheBmp.Canvas.Handle;
    CI := EmptyCI;
    if Style = tsTabs then
{$IFDEF DELPHI7UP}
      if acThemesEnabled then begin
        // Transparent part drawing
        Details := acThemeServices.GetElementDetails(ttBody);
        acThemeServices.DrawParentBackground(Handle, DC, @Details, True);
        acThemeServices.DrawElement(DC, Details, MkRect(Width, Height));
        // Page painting
        case TabPosition of
          tpTop: inc(R.Right, 2);
        end;
        Details := acThemeServices.GetElementDetails(ttPane);
        acThemeServices.DrawParentBackground(Handle, DC, @Details, False);
        acThemeServices.DrawElement(DC, Details, R);
      end
      else
{$ENDIF}
      begin
        R := PageRect;
        FillDC(DC, ClientRect, ColorToRGB(Color));
        DrawEdge(DC, R, EDGE_RAISED, BF_RECT);
      end
    else
      FillDC(DC, R, Color);

    FCommonData.FCacheBmp.Canvas.Unlock;
    if Tabs.Count > 0 then
      DrawSkinTabs(FCommonData.FCacheBmp, True);
  end;
  SkinData.BGChanged := False;
end;


procedure TsTabControl.AC_WMNCPaint(var Message: TWMPaint);
var
  DC, SavedDC: hdc;
  R: TRect;
begin
  if not InUpdating(SkinData) then begin
    if (Message.Unused = 1) or InAnimationProcess or (SkinData.CtrlSkinState and ACS_PRINTING <> 0) then begin
      DC := Message.DC;
      SavedDC := 0;
    end
    else begin
      DC := GetDC(Handle);
      SavedDC := SaveDC(DC);
    end;
    try
      if SkinData.BGChanged then
        PrepareCache;

      if (Tabs.Count > 0) and (TabIndex >= 0) then begin
        R := PageRect;
        ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      end;
      CopyWinControlCache(Self, SkinData, MkRect, MkRect(Self), DC, False);
    finally
      if DC <> Message.DC then begin
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end;
  end
end;


procedure TsTabControl.AC_WMPaint(var Message: TWMPaint);
var
  DC, SavedDC: hdc;
  R: TRect;
begin
  if not InUpdating(SkinData) then begin
    if (Message.Unused = 1) or InAnimationProcess or (SkinData.CtrlSkinState and ACS_PRINTING <> 0) then begin
      DC := Message.DC;
      SavedDC := 0;
    end
    else begin
      DC := GetDC(Handle);
      SavedDC := SaveDC(DC);
    end;
    try
      if SkinData.BGChanged then
        PrepareCache;

      if (Tabs.Count > 0) and (TabIndex >= 0) and ((Message.DC = 0) or (Message.DC <> SkinData.PrintDC)) then begin
        R := TabsRect;
        ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      end;

      CopyWinControlCache(Self, SkinData, MkRect, MkRect(Self), DC, False);
      SetParentUpdated(Self);
      sVCLUtils.PaintControls(DC, Self, True, MkPoint);
    finally
      if DC <> Message.DC then begin
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end;
  end
end;


procedure TsTabControl.AfterConstruction;
begin
  inherited AfterConstruction;
  SkinData.Loaded;
  CheckUpDown;
end;


procedure TsTabControl.CheckUpDown;
var
  Wnd: HWND;
  sp: TacSkinParams;
begin
  if not (csLoading in ComponentState) and not (csCreating in ControlState) then
    if FCommonData.Skinned and HandleAllocated then begin
      Wnd := FindWindowEx(Handle, 0, 'msctls_updown32', nil);
      FreeAndNil(BtnSW);
      if Wnd <> 0 then begin
        sp.SkinSection := s_UpDown;
        sp.Control := nil;
        BtnSW := TacSpinWnd.Create(Wnd, nil, SkinData.SkinManager, sp);
      end;
    end;
end;


procedure TsTabControl.CNDrawItem(var Message: TWMDrawItem);
begin
//
end;


constructor TsTabControl.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsTabControl;
  inherited Create(AOwner);
  TabsBG := CreateBmp32;
  BtnSW := nil;
  FCurItem := -1;
end;


destructor TsTabControl.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(BtnSW);
  TabsBG.Free;
  inherited Destroy;
end;


procedure TsTabControl.DrawSkinTab(Index, State: integer; Bmp: TBitmap; OffsetPoint: TPoint);
var
  rText, aRect, R: TRect;
  VertFont: TLogFont;
  bFont, cFont: hfont;
  ItemData: {$IFDEF TNTUNICODE}TTCItemW{$ELSE}TTCItem{$ENDIF};
  TabsCovering, TabSkinIndex, TabState: integer;
  i, h, w, iHeight, iWidth: integer;
  Buffer: array [0..4095] of AcChar;
  TempBmp: Graphics.TBitmap;
  SavedDC, MaskDC: hdc;
  ImgList: HImageList;
  lCaption: ACString;
  TabSection: string;
  TranspMode: byte;
  pFont: PLogFontA;
  CI: TCacheInfo;
  Flags: Cardinal;
  Font: TFont;

  procedure MakeVertFont(Orient: integer);
  var
    st: integer;
  begin
    Font := TFont.Create;
    Font.Assign(Bmp.Canvas.Font);
    pFont := PLogFontA(@VertFont);
    GetObject(Bmp.Canvas.Handle, SizeOf(TLogFont), pFont);
    VertFont.lfEscapement := Orient;
    VertFont.lfHeight := Bmp.Canvas.Font.Height;
    VertFont.lfStrikeOut := integer(fsStrikeOut in Bmp.Canvas.Font.Style);
    VertFont.lfItalic := integer(fsItalic in Bmp.Canvas.Font.Style);
    VertFont.lfUnderline := integer(fsUnderline	in Bmp.Canvas.Font.Style);
    VertFont.lfWeight := FW_NORMAL;
    VertFont.lfCharSet := Bmp.Canvas.Font.Charset;

    VertFont.lfWidth := 0;
    Vertfont.lfOutPrecision := OUT_DEFAULT_PRECIS;
    VertFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    VertFont.lfOrientation := VertFont.lfEscapement;
    VertFont.lfPitchAndFamily := Default_Pitch;
    VertFont.lfQuality := Default_Quality;
    if Font.Name <> s_MSSansSerif then
      StrPCopy(VertFont.lfFaceName, Font.Name)
    else
      VertFont.lfFaceName := s_Arial;

    Bmp.Canvas.Font.Handle := CreateFontIndirect(VertFont);
    st := integer(State > 0);
    if TabSkinIndex >= 0 then
      Bmp.Canvas.Font.Color := SkinData.CommonSkinData.gd[TabSkinIndex].Props[st].FontColor.Color
    else
      Bmp.Canvas.Font.Color := Font.Color;
  end;

  procedure KillVertFont;
  begin
    if Font <> nil then begin
      Bmp.Canvas.Font.Assign(Font);
      FreeAndNil(Font);
    end;
  end;

begin
  if Index >= 0 then begin
    bFont := ACNativeUInt(SendMessage(Handle, WM_GETFONT, 0, 0));
    cFont := SelectObject(Bmp.Canvas.Handle, bFont);

    R := SkinTabRect(Index, Index = TabIndex);
    if (State = 1) and (R.Left < 0) then
      Exit;

    rText := SkinTabRect(Index, (State = 2) and (Index = TabIndex));
    aRect := rText;

    ItemData.mask := TCIF_IMAGE or TCIF_STATE or TCIF_TEXT;
    ItemData.dwStateMask := TCIF_STATE;
    ItemData.pszText := Buffer;
    ItemData.cchTextMax := SizeOf(Buffer);

  {$IFDEF TNTUNICODE}
    SendMessage(Handle, TCM_GETITEMW, Index, LPARAM(@ItemData));
  {$ELSE}
    SendMessage(Handle, TCM_GETITEM, Index, LPARAM(@ItemData));
  {$ENDIF}
    lCaption := Buffer;
    TabsCovering := 0;
    // Tabs drawing
    with SkinData do
      if SkinData.SkinManager.IsActive and (CommonSkinData.Tabs[tlSingle][asTop].SkinIndex >= 0) then begin // new style
        TabState := State;
        case Style of
          tsTabs: begin
            TabSkinIndex := CommonSkinData.Tabs[tlSingle][acTabSides[TabPosition]].SkinIndex;
            TabSection   := CommonSkinData.Tabs[tlSingle][acTabSides[TabPosition]].SkinSection;
            TabsCovering := CommonSkinData.TabsCovering;
          end;

          tsButtons: begin
            TabSection := s_Button;
            TabSkinIndex := SkinData.SkinManager.GetSkinIndex(TabSection);
          end

          else begin
            TabSection := s_ToolButton;
            TabSkinIndex := SkinData.SkinManager.GetSkinIndex(TabSection);
          end;
        end;
        if TabSkinIndex >= 0 then begin
          if CommonSkinData.gd[TabSkinIndex].States <= TabState then
            TabState := CommonSkinData.gd[TabSkinIndex].States - 1;

          TempBmp := CreateBmp32(aRect);
          try
            if (State = 2) and (Index = TabIndex) then
              with OffsetPoint do begin
                // Restore BG for Active tab
                BitBlt(TempBmp.Canvas.Handle, aRect.Left + x, aRect.Top + y, TempBmp.Width, TempBmp.Height,
                       SkinData.FCacheBmp.Canvas.Handle, aRect.Left, aRect.Top, SRCCOPY);
                OffsetRect(R, X, Y);
                BitBlt(TempBmp.Canvas.Handle, 0, 0, TempBmp.Width, TempBmp.Height,
                       SkinData.FCacheBmp.Canvas.Handle, SkinTabRect(Index, Index = TabIndex).Left,
                       SkinTabRect(Index, Index = TabIndex).Top, SRCCOPY);
                // Paint active tab
                BitBlt(Bmp.Canvas.Handle, aRect.Left + x, aRect.Top + y, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
                CI := MakeCacheInfo(TempBmp);
                PaintItem(TabSkinIndex, CI, True, TabState, MkRect(TempBmp), Point(0, 0), Bmp, CommonSkinData);
              end
            else begin
              CI := MakeCacheInfo(SkinData.FCacheBmp);
              if State = 1 then
                CI.X := 0;

              PaintItem(TabSkinIndex, CI, True, TabState, MkRect(TempBmp), aRect.TopLeft, TempBmp, CommonSkinData);
              SavedDC := SaveDC(Bmp.Canvas.Handle);
              R := PageRect;
              if TabPosition in [tpLeft, tpTop] then
                ExcludeClipRect(Bmp.Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);

              BitBlt(Bmp.Canvas.Handle, aRect.Left + OffsetPoint.x, aRect.Top + OffsetPoint.y, TempBmp.Width, TempBmp.Height, TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
              RestoreDC(Bmp.Canvas.Handle, SavedDC);
            end;
          finally
            FreeAndNil(TempBmp);
          end;
        end;
      end
      else begin
        TabState := 0;
        TabSkinIndex := -1;
      end;
    // End of tabs drawing
    if not (OwnerDraw and Assigned(OnDrawTab)) then begin
      // Drawing of the tab content
      OffsetRect(rText, OffsetPoint.x, OffsetPoint.y);
      R := rText;
      InflateRect(R, -3, -3);
      ImgList := SendMessage(Handle, TCM_GETIMAGELIST, 0, 0);
      Bmp.Canvas.Font.Assign(Self.Font);
      TranspMode := GetTransMode(SkinData);
      Flags := DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_ALPHATEXT * Cardinal(TranspMode = 2);
      if UseRightToLeftReading then
        Flags := Flags or DT_RTLREADING;

      if TabPosition in [tpTop, tpBottom] then begin
        if TabsCovering > 0 then
          InflateRect(R, -TabsCovering * 2, 0);
      end
      else
        if TabsCovering > 0 then
          InflateRect(R, 0, -TabsCovering * 2);

      if (Images <> nil) and (State > 0) then
        SetImagesState(Images, State);

       case TabPosition of
        tpTop, tpBottom: begin
          if (ImgList <> 0) and (ItemData.iImage >= 0) then begin
            ImageList_GetIconSize(ImgList, w, h);
            Images.Draw(Bmp.Canvas, rText.Left + (WidthOf(rText) - (acTextWidth(Bmp.Canvas, lCaption) + Images.Width + 8)) div 2,
                        rText.Top + (HeightOf(rText) - Images.Height) div 2, ItemData.iImage, True);
            inc(rText.Left, w);
          end;
          R := rText;
          acWriteTextEx(Bmp.Canvas, PacChar(lCaption), True, rText, Flags, TabSkinIndex, TabState <> 0, SkinData.SkinManager);
        end;

        tpLeft: begin
          Bmp.Canvas.Brush.Style := bsClear;
          MakeVertFont(-2700);
          with acTextExtent(bmp.Canvas, lCaption) do begin
            h := cx;
            w := cy;
          end;
          if (ImgList <> 0) and (ItemData.iImage >= 0) then begin
            ImageList_GetIconSize(ImgList, iWidth, iHeight);
            if Index = TabIndex then
              OffsetRect(rText, 2, 0);

            i := rText.Bottom - (HeightOf(rText) - (iHeight + 4 + h)) div 2 - iHeight;
            Images.Draw(Bmp.Canvas, rText.Left + (WidthOf(rText) - Images.Width) div 2, i, ItemData.iImage, Enabled);
            Bmp.Canvas.Brush.Style := bsClear;
            acTextRect(bmp.Canvas, rText, rText.Left + (WidthOf(rText) - w) div 2, i - 4, lCaption);
            InflateRect(rText, (w - WidthOf(rText)) div 2, (h - HeightOf(rText)) div 2 + 2);
            OffsetRect(rText, 0, - (4 + h) div 2);
          end
          else begin
            Bmp.Canvas.Brush.Style := bsClear;
            acTextRect(Bmp.Canvas, rText, rText.Left + (WidthOf(rText) - w) div 2, rText.Bottom - (HeightOf(rText) - h) div 2, lCaption);
            InflateRect(rText, (w - WidthOf(rText)) div 2, (h - HeightOf(rText)) div 2);
          end;
          KillVertFont;
        end;

        tpRight: begin
          Bmp.Canvas.Brush.Style := bsClear;
          MakeVertFont(-900);
          OffsetRect(rText, -2, -1);
          with acTextExtent(bmp.Canvas, lCaption) do begin
            h := cx;
            w := cy;
          end;
          if (ImgList <> 0) and (ItemData.iImage >= 0) then begin
            ImageList_GetIconSize(ImgList, iWidth, iHeight);
            if Index = TabIndex then
              OffsetRect(rText, 2, 0);

            i := rText.Top + (HeightOf(rText) - (iHeight + 4 + h)) div 2;
            Images.Draw(Bmp.Canvas, rText.Left + (WidthOf(rText) - Images.Width) div 2, i, ItemData.iImage, Enabled);
            Bmp.Canvas.Brush.Style := bsClear;
            acTextRect(Bmp.Canvas, rText, rText.Left + (WidthOf(rText) - w) div 2 + Bmp.Canvas.TextHeight(lCaption), i + 4 + iHeight, lCaption);
            InflateRect(rText, (w - WidthOf(rText)) div 2, (h - HeightOf(rText)) div 2 + 2);
            OffsetRect(rText, 0, + (4 + iHeight) div 2);
          end
          else begin
            Bmp.Canvas.Brush.Style := bsClear;
            acTextRect(Bmp.Canvas, rText, rText.Left + (WidthOf(rText) + w) div 2, rText.Top + (HeightOf(rText) - h) div 2, lCaption);
            InflateRect(rText, (w - WidthOf(rText)) div 2, (h - HeightOf(rText)) div 2 + 2);
          end;
          KillVertFont;
        end;
      end;
      if TranspMode = 1 then
        FillAlphaRect(Bmp, rText);

      if (Images <> nil) and (State > 0) then
        SetImagesState(Images, 0);
    end
    else begin
      Bmp.Canvas.Lock;
      R := SkinTabRect(Index, Index = TabIndex);
      if Bmp <> SkinData.FCacheBmp then begin
        aRect :=TabsRect;
        TempBmp := CreateBmp32(aRect);
        MaskDC := CreateCompatibleDC(0);
        try
          TempBmp.Canvas.Lock;
          TempBmp.Canvas.FillRect(aRect);
          Canvas.Handle := TempBmp.Canvas.Handle;
          BitBlt(TempBmp.Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          OnDrawTab(Self, Index, R, Index = TabIndex);
          TempBmp.Canvas.Unlock;
          SelectObject(MaskDC, TempBmp.MaskHandle);
          BitBlt(Bmp.Canvas.Handle, 0, 0, WidthOf(R), HeightOf(R), TempBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
        finally
          DeleteDC(MaskDC);
          FreeAndNil(TempBmp);
        end;
      end
      else begin
        Canvas.Handle := Bmp.Canvas.Handle;
        OnDrawTab(Self, Index, R, Index = TabIndex);
      end;
      Bmp.Canvas.Unlock;
    end;
    SelectObject(Bmp.Canvas.Handle, cFont);
  end;
end;


procedure TsTabControl.DrawSkinTab(Index, State: integer; DC: hdc);
var
  aRect: TRect;
  TempBmp: TBitmap;
begin
  if Index >= 0 then begin
    aRect := SkinTabRect(Index, State = 2);
    TempBmp := CreateBmp32(aRect);
    DrawSkinTab(Index, State, TempBmp, Point(-aRect.Left, -aRect.Top));
    BitBlt(DC, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(TempBmp);
  end;
end;


procedure TsTabControl.DrawSkinTabs(Bmp: TBitmap; PaintActive: boolean);
var
  i: integer;
  SavedDC: hdc;
  CI: TCacheInfo;
  bSkinned: boolean;
  R, RTabsBG: TRect;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) then begin
    R := TabsRect;
    Bmp.Canvas.Lock;
    bSkinned := SkinData.Skinned;
    if bSkinned then
      CI := GetParentCache(FCommonData)
    else begin
      CI.Ready := False;
      CI.FillColor := Color;
      CI.Bmp := nil;
    end;
    // Copy rect from parent or just fill
    if TabIndex >= 0 then begin
      RTabsBG := RectsAnd(PageRect, SkinTabRect(TabIndex, True));
      if not IsRectEmpty(RTabsBG) then
        if not ci.Ready then
          FillDC(Bmp.Canvas.Handle, RTabsBG, CI.FillColor)
        else
          BitBlt(Bmp.Canvas.Handle, RTabsBG.Left, RTabsBG.Top, WidthOf(RTabsBG), HeightOf(RTabsBG),
                 ci.Bmp.Canvas.Handle, ci.X + Left + RTabsBG.Left, ci.Y + Top + RTabsBG.Top, SRCCOPY);
    end;
    if not ci.Ready then
{$IFDEF DELPHI7UP}
      if not bSkinned and acThemesEnabled then begin
        Details := acThemeServices.GetElementDetails(ttBody);
        SavedDC := SaveDC(Bmp.Canvas.Handle);
        try
          IntersectClipRect(Bmp.Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
          acThemeServices.DrawParentBackground(Handle, Bmp.Canvas.Handle, @Details, False);
        finally
          RestoreDC(Bmp.Canvas.Handle, SavedDC);
        end;
      end
      else
{$ENDIF}
        FillDC(Bmp.Canvas.Handle, R, CI.FillColor)
    else
      BitBlt(Bmp.Canvas.Handle, R.Left, R.Top, min(WidthOf(R), ci.Bmp.Width), min(HeightOf(R), ci.Bmp.Height),
             ci.Bmp.Canvas.Handle, ci.X + Left + R.Left, ci.Y + Top + R.Top, SRCCOPY);

    if Bmp = SkinData.FCacheBmp then begin
      RTabsBG := TabsBGRect;
      TabsBG.Width  := WidthOf (RTabsBG);
      TabsBG.Height := HeightOf(RTabsBG);
      BitBlt(TabsBG.Canvas.Handle, 0, 0, TabsBG.Width, TabsBG.Height, Bmp.Canvas.Handle, RTabsBG.Left, RTabsBG.Top, SRCCOPY);
      FillAlphaRect(TabsBG, MkRect(TabsBG), MaxByte);
    end;
    // Draw tabs
    SavedDC := SaveDC(Bmp.Canvas.Handle);
    try
      if Tabs.Count > 0 then
        if bSkinned then begin
          for i := Tabs.Count - 1 downto 0 do
            if i <> TabIndex then
              DrawSkinTab(i, integer(CurItem = i), Bmp.Canvas.Handle);

          if PaintActive and (TabIndex >= 0) then // Draw active tab
            DrawSkinTab(TabIndex, 2, Bmp.Canvas.Handle);
        end
        else begin
          for i := 0 to Tabs.Count - 1 do
            if i <> TabIndex then
              DrawStdTab(i, integer(CurItem = i), Bmp.Canvas.Handle);

          if PaintActive and (TabIndex >= 0) then // Draw active tab
            DrawStdTab(TabIndex, 2, Bmp.Canvas.Handle);
        end;
    finally
      RestoreDC(Bmp.Canvas.Handle, SavedDC);
    end;
    Bmp.Canvas.UnLock;
  end;
end;


procedure TsTabControl.DrawStdTab(aTabIndex, State: integer; DC: hdc);
const
  AntiPosArray: array [TTabPosition] of integer = (BF_BOTTOM, BF_TOP, BF_RIGHT, BF_LEFT);
  Spacing = 3;
  FixOffset = 6;
var
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  Tab: TThemedTab;
  ToolBtn: TThemedToolBar;
  Btn: TThemedButton;
  tp: TTabPosition;
{$ENDIF}
  MaskDC: hdc;
  Flags: Cardinal;
  Bmp,
  TempBmp: TBitmap;
//  TranspMode: byte;
  lCaption: ACString;
  dContent: TacTabData;
  aRect, R, rTmp: TRect;
begin
  if aTabIndex >= 0 then begin
    R := StdTabRect(aTabIndex);
    if State = 2 then
      InflateRect(R, 2, 2);

    Bmp := CreateBmp32(R);
    try
      Bmp.Canvas.Font.Assign(Font);
      SelectObject(Bmp.Canvas.Handle, Bmp.Canvas.Font.Handle);
      Bmp.Canvas.Brush.Style := bsClear;
      aRect := MkRect(Bmp);
{$IFDEF DELPHI7UP}
      if acThemesEnabled then begin
        case Style of
          tsTabs: begin
            tp := TabPosition;
            if TabPosition <> tpTop then begin // Others tabs are not supported by API
              if not IsLeftToRight then
                case TabPosition of
                  tpLeft: tp := tpRight;
                  tpRight: tp := tpLeft;
                end;

              case State of
                0: if tp = tpBottom then
                     Details := acThemeServices.GetElementDetails(ttTabItemNormal)
                   else
                     Details := acThemeServices.GetElementDetails(ttbButtonNormal);

                1: if tp = tpBottom then
                     Details := acThemeServices.GetElementDetails(ttTabItemHot)
                   else
                     Details := acThemeServices.GetElementDetails(ttbButtonHot);

                2: Details := acThemeServices.GetElementDetails(ttPane);
              end
            end
            else begin // Draw buttons with a special offset (tabs emulation)
              case State of
                0:   Tab := ttTabItemNormal;
                1:   Tab := ttTabItemHot;
                else Tab := ttTabItemSelected;
              end;
              Details := acThemeServices.GetElementDetails(Tab);
            end;
            with acThemeServices, Bmp do // Draw tab
              case tp of
                tpTop:    DrawElement(Bmp.Canvas.Handle, Details, aRect);
                tpBottom: DrawElement(Bmp.Canvas.Handle, Details, Rect(0, -FixOffset, Width, Height));
                tpLeft:   DrawElement(Bmp.Canvas.Handle, Details,  MkRect(Width + FixOffset, Height));
                tpRight:  DrawElement(Bmp.Canvas.Handle, Details, Rect(-FixOffset, 0, Width, Height));
              end;

            Details.Part := -1;
          end;

          tsButtons: begin
            case State of
              0:   Btn := tbPushButtonNormal;
              1:   Btn := tbPushButtonHot
              else Btn := tbPushButtonPressed;
            end;
            Details := acThemeServices.GetElementDetails(Btn);
          end

          else begin
            if R.Left > 6 then begin // Draw a separator
              rTmp := Rect(R.Left - 6, R.Top, R.Left, R.Bottom);
              DrawEdge(DC, rTmp, EDGE_ETCHED, BF_LEFT);
            end;
            case State of
              0: begin
                FillDC(Bmp.Canvas.Handle, MkRect(Bmp), Color); { Fill background }
                ToolBtn := ttbButtonNormal;
              end;

              1: begin
                FillDC(Bmp.Canvas.Handle, MkRect(Bmp), Color); { Fill background }
                ToolBtn := ttbButtonHot;
              end

              else
                ToolBtn := ttbButtonPressed;
            end;
            Details := acThemeServices.GetElementDetails(ToolBtn);
          end;
        end;

        if Details.Part <> -1 then
          acThemeServices.DrawElement(Bmp.Canvas.Handle, Details, aRect); // Draw tab
      end
      else
{$ENDIF}
      begin // Draw without themes (very od style)
        FillDC(Bmp.Canvas.Handle, aRect, Color);
        case Style of
          tsTabs:
            DrawEdge(Bmp.Canvas.Handle, aRect, EDGE_RAISED, BF_RECT and not AntiPosArray[TabPosition]);

          tsButtons:
            case State of
              0, 1: DrawEdge(Bmp.Canvas.Handle, aRect, EDGE_RAISED, BF_RECT);
              2:    DrawEdge(Bmp.Canvas.Handle, aRect, EDGE_SUNKEN, BF_RECT);
            end;

          else begin
            if R.Left > 6 then begin // Draw a separator
              rTmp := Rect(R.Left - 6, R.Top, R.Left, R.Bottom);
              DrawEdge(DC, rTmp, EDGE_ETCHED, BF_LEFT);
            end;
            case State of
              1:
                Frame3D(Bmp.Canvas, aRect, ColorToRGB(clWhite), ColorToRGB(clBtnShadow), 1);

              2: begin
                FillDC(Bmp.Canvas.Handle, aRect, ColorToRGB(cl3DLight));
                DrawEdge(Bmp.Canvas.Handle, aRect, EDGE_SUNKEN, BF_RECT);
              end;
            end;
          end;
        end;
      end;
      if not OwnerDraw then begin
        // Drawing of tab content
        Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or DT_WORD_ELLIPSIS;
        if UseRightToLeftReading then
          Flags := Flags or DT_RTLREADING or DT_NOCLIP;

        lCaption := Tabs[aTabIndex];
        // Init transp mode
//        TranspMode := GetTransMode(SkinData, aTabIndex, State);
        // Get coordinates for tab content
        InitTabContentData(Bmp.Canvas, aTabIndex, aRect, State, dContent);
        // Draw glyph if rect is not empty
        if not IsRectEmpty(dContent.GlyphRect) then begin
          SetImagesPPI(Images, SkinData.CommonSkinData.PPI);
          Bmp.Canvas.Font.Color := ColorToRGB(Font.Color);
          if State > 0 then
            SetImagesState(Images, State);

          Images.Draw(Bmp.Canvas, dContent.GlyphRect.Left, dContent.GlyphRect.Top, aTabIndex, True);
          if State > 0 then
            SetImagesState(Images, 0);
        end;
        Bmp.Canvas.Brush.Style := bsClear;
        acWriteText(Bmp.Canvas, PacChar(lCaption), True, dContent.TextRect, Flags);
        // Paint focus rect
        if not IsRectEmpty(dContent.FocusRect) then begin
          Bmp.Canvas.Pen.Color := clWindowFrame;
          Bmp.Canvas.Brush.Color := clBtnFace;
          Bmp.Canvas.DrawFocusRect(dContent.FocusRect);
        end;
      end
      else
        if Assigned(OnDrawTab) then begin
          aRect :=TabsRect;
          TempBmp := CreateBmp32(aRect);
          MaskDC := CreateCompatibleDC(0);
          try
            TempBmp.Canvas.Lock;
            TempBmp.Canvas.FillRect(aRect);
            Canvas.Handle := TempBmp.Canvas.Handle;
            BitBlt(TempBmp.Canvas.Handle, R.Left, R.Top, WidthOf(R), HeightOf(R), Bmp.Canvas.Handle, 0, 0, SRCCOPY);
            OnDrawTab(Self, aTabIndex, R, State <> 0);
            TempBmp.Canvas.Unlock;
            SelectObject(MaskDC, TempBmp.MaskHandle);
            BitBlt(Bmp.Canvas.Handle, 0, 0, WidthOf(R), HeightOf(R), TempBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
            BitBlt(DC, R.Left, R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          finally
            DeleteDC(MaskDC);
            FreeAndNil(TempBmp);
          end;
        end;

      BitBlt(DC, R.Left, R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY); // Copy Tab to DC
    finally
      Bmp.Free;
    end;
  end;
end;


function TsTabControl.GetClRect: TRect;
begin
  Result := MkRect(Self);
  SendMessage(Handle, TCM_ADJUSTRECT, 0, LPARAM(@Result));
  Inc(Result.Top, 2);
end;


function TsTabControl.GetTabUnderMouse(p: TPoint): integer;
var
  i: integer;
  R: TRect;
begin
  Result := -1;
  for i := 0 to Tabs.Count - 1 do begin
    R := SkinTabRect(i, False);
    if PtInRect(R, p) then begin
      Result := i;
      Exit;
    end;
  end;
end;


procedure TsTabControl.InitTabContentData(Canvas: TCanvas; ATabIndex: integer; BmpRect: TRect; State: integer; var Data: TacTabData);
var
  sIndex, TabsCovering, i, ContentSize, iGlyphSpace, imgWidth, imgHeight: integer;
  IsVertical: boolean;
  lCaption: ACString;
  GlyphSize: TSize;
  ta: TAlignment;
  R: TRect;

  procedure OffsetContent(LeftDX, RightDX, DY: integer);
  begin
    OffsetRect(Data.GlyphRect,  LeftDX, DY);
    OffsetRect(Data.TextRect,   LeftDX, DY);
  end;

  function GetTextSize(aRect: TRect): TSize;
  begin
    if lCaption <> '' then begin
      R := aRect;
      acDrawText(Canvas.Handle, lCaption, R, DT_CALCRECT or DT_SINGLELINE);
      Result := MkSize(R);
    end
    else
      Result := MkSize;
  end;

begin
  lCaption := Tabs[ATabIndex];
  // Size of glyph
  if (Images <> nil) and IsValidIndex(ATabIndex, GetImageCount(Images)) then begin
    GlyphSize.cx := GetImageWidth (Images, ATabIndex, GetPPI(SkinData));
    GlyphSize.cy := GetImageHeight(Images, ATabIndex, GetPPI(SkinData));
  end
  else
    GlyphSize := MkSize;

  // Flags and variables initializing
  iGlyphSpace := integer((lCaption <> '') and (GlyphSize.cx <> 0)) * SkinData.CommonSkinData.Spacing;
  TabsCovering := FCommonData.CommonSkinData.TabsCovering * 2;

  IsVertical := TabPosition in [tpLeft, tpRight];
  ta := taCenter;

  if IsVertical then begin
    imgWidth  := HeightOf(BmpRect);
    imgHeight := WidthOf (BmpRect);
    ChangeI(GlyphSize.cx, GlyphSize.cy);
  end
  else begin
    imgWidth  := WidthOf (BmpRect);
    imgHeight := HeightOf(BmpRect);
  end;

  Data.TextSize := GetTextSize(Rect(0, 0, imgWidth - GlyphSize.cx - iGlyphSpace, 0)); // Size of text
  ContentSize := GlyphSize.cx + iGlyphSpace + Data.TextSize.cx;
  case ta of
    taLeftJustify:  Data.GlyphRect.Left := 0;
    taRightJustify: Data.GlyphRect.Left := imgWidth - ContentSize
    else            Data.GlyphRect.Left := max(0, (imgWidth - ContentSize) div 2);
  end;
  inc(Data.GlyphRect.Left, TabsCovering);
  Data.GlyphRect.Top := (imgHeight - GlyphSize.cy) div 2;

  Data.GlyphRect.Right := Data.GlyphRect.Left + GlyphSize.cx;
  Data.GlyphRect.Bottom := Data.GlyphRect.Top + GlyphSize.cy;
  // Text rect
  Data.TextRect.Left := Data.GlyphRect.Right + iGlyphSpace;
  Data.TextRect.Top := (imgHeight - Data.TextSize.cy) div 2 + (imgHeight - Data.TextSize.cy) mod 2;

  Data.TextRect.Right := Data.TextRect.Left + Data.TextSize.cx;
  Data.TextRect.Bottom := Data.TextRect.Top + Data.TextSize.cy;
  sIndex := SkinData.CommonSkinData.Tabs[tlSingle][acTabSides[TabPosition]].SkinIndex;
  // Update active tab offsets
  if State = 2 then begin
    case ta of // Correction of tab rect
      taLeftJustify:  OffsetContent(2, -2, 0);
      taRightJustify: OffsetContent(-2, 2, 0)
      else ;
    end;
    i := 1 + integer(CanClickShift(sIndex, SkinData.CommonSkinData));
    if not IsLeftToRight then
      OffsetContent(0, 0, acMinusPlus[TabPosition in [tpBottom,  tpLeft]] * i)
    else
      OffsetContent(0, 0, acMinusPlus[TabPosition in [tpBottom, tpRight]] * i);
  end;
  // Mirror rects
  if not IsLeftToRight then
    if not IsVertical then begin
      ReflectRect(Data.GlyphRect, BmpRect, True);
      ReflectRect(Data.TextRect,  BmpRect, True);
    end;
  // Focus rect
  if (Focused or FCommonData.FFocused) and (State = 2) and (Data.TextSize.cx <> 0) then begin
    Data.FocusRect := Data.TextRect;
    InflateRect(Data.FocusRect, 3, 0);
    inc(Data.FocusRect.Bottom, 3);
  end
  else
    Data.FocusRect := MkRect;
  // Rotate rects
  if IsVertical then begin
    Data.GlyphRect := RotateRect(Data.GlyphRect, False, imgWidth);
    Data.TextRect  := RotateRect(Data.TextRect,  False, imgWidth);
  end;
  if IsVertical then begin
    Data.TextPos.X := Data.TextRect.Left + (WidthOf(Data.TextRect) - Data.TextSize.cy) div 2;
    Data.TextPos.Y := Data.TextRect.Bottom;
  end
  else begin
    Data.TextPos.X := Data.TextRect.Left;
    Data.TextPos.Y := Data.TextRect.Top;
  end;
end;


function TsTabControl.IsLeftToRight: boolean;
begin
  Result := (csDesigning in ComponentState) or (BiDiMode <> bdRightToLeft);
end;


procedure TsTabControl.Loaded;
begin
  inherited Loaded;
  SkinData.Loaded;
  CheckUpDown;
end;


function TsTabControl.PageRect: TRect;
begin
  Result := MkRect(Self);
  if Tabs.Count > 0 then
    case TabPosition of
      tpTop:    Result.Top    := GetClRect.Top    - TopOffset;
      tpBottom: Result.Bottom := GetClRect.Bottom + BottomOffset;
      tpLeft:   Result.Left   := GetClRect.Left   - LeftOffset;
      tpRight:  Result.Right  := GetClRect.Right  + RightOffset;
    end;
end;


procedure TsTabControl.RepaintTab(i, State: integer; TabDC: hdc);
var
  DC, SavedDC: hdc;
  R: TRect;
begin
  if TabDC = 0 then
    DC := GetDC(Handle)
  else
    DC := TabDC;

  SavedDC := SaveDC(DC);
  try
    if TabIndex <> i then begin // Exclude active bigger tabrect
      R := SkinTabRect(TabIndex, True);
      ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    end;
    R := PageRect; // Exclude body rectangle
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    if SkinData.Skinned then
      DrawSkinTab(i, State, DC)
    else
      DrawStdTab(i, State, DC);
  finally
    RestoreDC(DC, SavedDC);
    if TabDC = 0 then
      ReleaseDC(Handle, DC);
  end;
end;


procedure TsTabControl.SetCurItem(const Value: integer);
var
  Old: integer;
begin
  if FCurItem <> Value then begin
    Old := FCurItem;
    FCurItem := Value;
    if IsValidIndex(Value, Tabs.Count) then
      if Value <> TabIndex then
        RepaintTab(Value, 1); // Repaint new hot tab

    if IsValidIndex(Old, Tabs.Count) then
      if Old <> TabIndex then
        RepaintTab(Old, 0); // Repaint old tab in normal state
  end;
end;


function TsTabControl.SkinTabRect(Index: integer; Active: boolean): TRect;
var
  aTabsCovering: integer;
begin
  Result := MkRect;
  if IsValidIndex(Index, Tabs.Count) then begin
    Result := StdTabRect(Index);
    if (Style <> tsTabs) or (Result.Left = Result.Right) then
      Exit;

    aTabsCovering := 0;
    if Style = tsTabs then
      aTabsCovering := SkinData.CommonSkinData.TabsCovering;

    if Active then
      dec(Result.Bottom, 1)
    else begin
      inc(Result.Bottom, 3);
      dec(Result.Right, 1);
    end;

    case TabPosition of
      tpTop: begin
        InflateRect(Result, 2 * Integer(Active), 2 * Integer(Active));
        inc(Result.Bottom, 1);
      end;

      tpBottom: begin
        InflateRect(Result, 2 * Integer(Active), Integer(Active));
        dec(Result.Top, 2);
        if Active then
          inc(Result.Bottom)
        else
          dec(Result.Bottom, 3);
      end;

      tpLeft: begin
        InflateRect(Result, 0, 1);
        inc(Result.Right, 2);
        if Active then
          InflateRect(Result, 1, 1)
        else begin
          dec(Result.Bottom, 4);
          inc(Result.Right, 2);
        end;
      end;

      tpRight: begin
        InflateRect(Result, 1, 0);
        OffsetRect(Result, -1, -1);
        if Active then begin
          InflateRect(Result, 1, 1);
          inc(Result.Bottom, 3);
        end
        else
          dec(Result.Bottom, 2);
      end;
    end;
    if TabPosition in [tpTop, tpBottom] then begin
      if aTabsCovering > 0 then begin
        OffsetRect(Result, aTabsCovering, 0);
        InflateRect(Result, aTabsCovering, 0);
      end;
    end
    else
      if aTabsCovering > 0 then begin
        OffsetRect(Result, 0, aTabsCovering);
        InflateRect(Result, 0, aTabsCovering);
      end;
  end;
end;


function TsTabControl.StdTabRect(Index: integer): TRect;
begin
  Result := TabRect(Index);
  if not IsLeftToRight and (TabPosition in [tpTop, tpBottom]) then
    ReflectRect(Result, Width, Height, TabPosition in [tpTop, tpBottom]);
end;


function TsTabControl.TabRow(TabIndex: integer): integer;
var
  h, w, rCount: integer;
  R, tR: TRect;
begin
  rCount := TabCtrl_GetRowCount(Handle);
  if rCount > 1 then begin
    R := TabRect(TabIndex);
    tR := TabsRect;
    w := WidthOf(R) + 1;
    h := HeightOf(R) + 1;
    case TabPosition of
      tpTop:   Result := (R.Bottom + h div 2) div h;
      tpLeft:  Result := (R.Right +  w div 2) div w;
      tpRight: Result := rCount - (R.Right - tR.Left + w div 2) div w + 1
      else     Result := rCount - (R.Bottom - tR.Top + h div 2) div h + 1;
    end;
  end
  else
    Result := 1;
end;


function TsTabControl.TabsBGRect: TRect;
var
  R: TRect;
begin
  if Tabs.Count > 0 then begin
    Result := MkRect(Width, Height);
    AdjustClientRect(R);
    case TabPosition of
      tpTop:    Result.Bottom := R.Top    - TopOffset;
      tpBottom: Result.Top    := R.Bottom + BottomOffset;
      tpLeft:   Result.Right  := R.Left   - LeftOffset
      else      Result.Left   := R.Right  + RightOffset
    end;
  end
  else
    Result := MkRect;
end;


function TsTabControl.TabsRect: TRect;
begin
  Result := MkRect(Self);
  if Tabs.Count > 0 then begin
    case TabPosition of
      tpTop:    Result.Bottom := GetClRect.Top    - TopOffset;
      tpBottom: Result.Top    := GetClRect.Bottom + BottomOffset;
      tpLeft:   Result.Right  := GetClRect.Left   - LeftOffset;
      tpRight:  Result.Left   := GetClRect.Right  + RightOffset;
    end;
  end;
end;


procedure TsTabControl.TrySetNewTab(aIndex: integer);
var
  MHdr: TNMHdr;
  Result: HRESULT;
begin
  if (aIndex <> TabIndex) and CanChange and CanShowTab(aIndex) then begin
    MHdr.hwndFrom := Handle;
    MHdr.code := TCN_SELCHANGING;
    Result := Perform(CN_NOTIFY, WParam(Handle), LPAram(@MHdr));
    if Result = 0 then begin
      SkinData.BGChanged := True;
      TabIndex := aIndex;
      MHdr.code := TCN_SELCHANGE;
      Perform(CN_NOTIFY, WParam(Handle), LPAram(@MHdr));
    end;
  end;
end;


procedure TsTabControl.WndProc(var Message: TMessage);
var
  R: TRect;
  p: TPoint;
  aMsg: TMsg;
  NewItem: integer;
  PS: TPaintStruct;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins supported

      AC_REMOVESKIN: begin
        if Message.LParam = LPARAM(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          CheckUpDown;
          if BtnSW <> nil then
            FreeAndNil(BtnSW);

          SkinData.Invalidate;
        end;
        AlphaBroadcast(Self, Message);
      end;

      AC_REFRESH:
        if RefreshNeeded(SkinData, Message) then begin
          CommonMessage(Message, FCommonData);
          SkinData.Invalidate;
          CheckUpDown;
          AlphaBroadcast(Self, Message);
          Exit;
        end;

      AC_SETNEWSKIN:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          AlphaBroadcast(Self, Message);
          Exit;
        end;

      AC_PREPARING: begin
        Message.Result := LRESULT(FCommonData.FUpdating);
        Exit;
      end;

      AC_ENDPARENTUPDATE:
        if SkinData.Skinned then
          if FCommonData.FUpdating then begin
            if not InUpdating(FCommonData, True) then
              RedrawWindow(Handle, nil, 0, RDWA_NOCHILDREN);

            SetParentUpdated(Self);
            Exit;
          end;

      AC_GETOUTRGN: begin
        PRect(Message.LParam)^ := PageRect;
        OffsetRect(PRect(Message.LParam)^, Left, Top);
        Exit;
      end;

      AC_PREPARECACHE: begin
        if not InUpdating(SkinData) and SkinData.Skinned then
          PrepareCache;

        Exit;
      end;

      AC_GETBG:
        if SkinData.Skinned then begin
          InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
          Exit;
        end;

      AC_GETDEFSECTION: begin
        Message.Result := 9 + 1;
        Exit;
      end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          Message.Result := FCommonData.SkinManager.GetSkinIndex(s_PageControl + sTabPositions[TabPosition]) + 1;

        Exit;
      end

      else
        if CommonMessage(Message, FCOmmonData) then
          Exit;
    end;
    if Assigned(FCommonData) then begin
      if CommonWndProc(Message, FCommonData) and (Message.Msg = SM_ALPHACMD) then
        Exit;

      if Message.Msg <> SM_ALPHACMD then
        case Message.Msg of
          WM_MOUSELEAVE, CM_MOUSELEAVE:
            if ([csDesigning, csDestroying] * ComponentState = []) and (CurItem >= 0) and HotTrack then begin
              inherited;
              CurItem := -1;
              Exit;
            end;

          WM_MOUSEMOVE:
            if ([csDesigning, csDestroying] * ComponentState = []) then begin
              if (FCommonData.CtrlSkinState and ACS_LOCKED = 0) and (HotTrack or (Style <> tsTabs)) then begin
                if (DefaultManager <> nil) and not (csDesigning in DefaultManager.ComponentState) then
                  DefaultManager.ActiveControl := Handle;

                p.x := TCMHitTest(Message).XPos;
                p.y := TCMHitTest(Message).YPos;
                NewItem := GetTabUnderMouse(p);
                if IsValidIndex(NewItem, Tabs.Count) then begin // If tab is hovered
                  if Assigned(OnMouseMove) then
                    OnMouseMove(Self, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);

                  if not SkinData.Skinned or not SkinData.SkinManager.Effects.AllowAnimation then begin
                    FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_LOCKED;
                    inherited;
                    FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_LOCKED;
                    PeekMessage(aMsg, Handle, WM_PAINT, WM_PAINT, PM_REMOVE); // Prevent an automatic repainting
                    Application.ProcessMessages;
                  end
                  else
                    inherited;

                  CurItem := NewItem;
                end
                else
                  CurItem := -1;
              end
              else
                inherited;

              Exit;
            end;

          WM_LBUTTONUP, WM_LBUTTONDOWN:
            if not (csDesigning in ComponentState) and Enabled then begin
              p.x := TCMHitTest(Message).XPos;
              p.y := TCMHitTest(Message).YPos;
              NewItem := GetTabUnderMouse(p);
              if NewItem >= 0 then begin // If tab is pressed
                if Message.Msg = WM_LBUTTONDOWN then begin
                  MouseDown(mbLeft, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);
                  TrySetNewTab(NewItem);
                end
                else
                  if Message.Msg = WM_LBUTTONUP then
                    MouseUp(mbLeft, GetShiftState, TCMHitTest(Message).XPos, TCMHitTest(Message).YPos);

                Exit;
              end;
            end;

          WM_PRINT: begin
            SkinData.Updating := False;
            AC_WMPaint(TWMPaint(Message));
            Exit;
          end;

          WM_NCPAINT:
            if IsWindowVisible(Handle) then begin
              if not InAnimationProcess then
                AC_WMNCPaint(TWMPaint(Message));

              Message.Result := 0;
              Exit;
            end;

          WM_ERASEBKGND:
            if IsWindowVisible(Handle) then begin
              if not InAnimationProcess then
                AC_WMPaint(TWMPaint(Message));

              Message.Result := 0;
              Exit;
            end;

          WM_PAINT: begin
            if not (csDestroying in ComponentState) and (Parent <> nil) then // Background update
              InvalidateRect(Handle, nil, True); // Background update (for repaint of graphic controls and for tansheets refreshing)

            BeginPaint(Handle, PS);
            EndPaint(Handle, PS);
            Exit;
          end;

          CM_VISIBLECHANGED, WM_MOUSEWHEEL:
            if FCommonData.Skinned and ([csReading, csLoading] * ComponentState = []) then begin
              FCommonData.BGChanged := True;
              Repaint;
              if not FCommonData.Updating then
                FCommonData.BGChanged := False;
            end;

          WM_SETFOCUS, CM_ENTER, WM_KILLFOCUS, CM_EXIT:
            if FCommonData.Skinned and TabStop then begin
              FCommonData.FFocused := (Message.Msg = CM_ENTER) or (Message.Msg = WM_SETFOCUS);
              FCommonData.FMouseAbove := False;
              FCommonData.BGChanged := True;
              if ([csReading, csLoading] * ComponentState = []) and not (csCreating in ControlState) then
                RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
            end;

          CM_MOUSEENTER:
            if not FCommonData.FFocused and not (csDesigning in ComponentState) then
              FCommonData.FMouseAbove := (Message.Msg = CM_MOUSEENTER);

          WM_WINDOWPOSCHANGING:
            FCommonData.BGChanged := True;
        end;
  end;
  inherited;
  case Message.Msg of
    WM_NCHITTEST:
      if PtInRect(TabsRect, ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos))) then
        if (csDesigning in ComponentState) or (GetTabUnderMouse(ScreenToClient(Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos))) >= 0) then
          Message.Result := HTCLIENT
        else
          Message.Result := HTTRANSPARENT;

    WM_HSCROLL:
      if not (csLoading in ComponentState) then begin
        FCommonData.BGChanged := True;
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME);
      end;

    CN_NOTIFY:
      case TWMNotify(Message).NMHdr^.code of
        TCN_SELCHANGE: begin
          SkinData.BGChanged := True;
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
        end;
      end;

    WM_SIZE: begin
      CheckUpDown;
      GetWindowRect(Handle, R);
      SkinData.BGChanged := True;
      RedrawWindow(Handle, nil, 0, RDWA_NOCHILDREN);
    end;

    TCM_SETCURSEL:
      if [csReading, csLoading] * ComponentState = [] then begin
        SkinData.BGChanged := True;
        RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
      end;

    WM_MOVE:
      if [csReading, csLoading] * ComponentState = [] then
        RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
  end;
end;

end.
