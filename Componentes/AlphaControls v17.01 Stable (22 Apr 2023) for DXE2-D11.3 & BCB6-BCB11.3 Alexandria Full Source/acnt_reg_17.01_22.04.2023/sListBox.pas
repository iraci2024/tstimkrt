unit sListBox;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  StdCtrls, controls, classes, forms, graphics, messages, windows, sysutils,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  {$IFDEF TNTUNICODE} TntStdCtrls, TntClasses, {$ENDIF}
  acSBUtils, sConst, sDefaults, sCommonData;


type
{$IFNDEF NOTFORHELP}
  TBeforeDrawItemEvent = procedure(aDC: hdc; aIndex: Integer; var aRect: TRect) of object;

{$IFDEF TNTUNICODE}
  TsCustomListBox = class(TTntListBox)
{$ELSE}
  TsCustomListBox = class(TListBox)
{$ENDIF}
  private
    FOnMouseEnter,
    FOnMouseLeave: TNotifyEvent;
    FBoundLabel: TsBoundLabel;
    FOnVScroll: TNotifyEvent;
    FullPaint: boolean;
    FOnBeforeItemDraw: TBeforeDrawItemEvent;
    FLeftPadding: byte;
    FItemsCount: Integer;
    FItemIndex: Integer;
    FOnItemIndexChanged: TNotifyEvent;
    FOnItemCountChanged: TNotifyEvent;
    FShowLines: boolean;
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure CNDrawItem  (var Message: TWMDrawItem);   message CN_DRAWITEM;
    procedure WMPaint     (var Message: TWMPaint);      message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMPaint);      message WM_ERASEBKGND;
    procedure WMPrint     (var Message: TWMPaint);
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
{$IFDEF DELPHI6UP}
    function GetItemTextDirect(AIndex: integer): acString;
{$ENDIF}
    function FontStored: boolean;
    function ColorStored: boolean;
    procedure SetLeftPadding(const Value: byte);
    procedure SetShowLines(const Value: boolean);
  protected
    FAutoHideScroll: boolean;
    FCommonData: TsScrollWndData;
    FAutoCompleteDelay: Word;
    FDisabledKind: TsDisabledKind;
    procedure SetAutoHideScroll(const Value: boolean);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF}); override;
    procedure Change; virtual;
    procedure SetItemIndex(const Value: Integer); {$IFDEF DELPHI6UP}override;{$ENDIF}
  public
    ListSW: TacScrollWnd;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
{$IFNDEF DELPHI6UP}
    function Count: integer;
{$ENDIF}
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property AutoCompleteDelay: Word read FAutoCompleteDelay write FAutoCompleteDelay default 500;
    property AutoHideScroll: boolean read FAutoHideScroll write SetAutoHideScroll default True;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property LeftPadding: byte read FLeftPadding write SetLeftPadding default 0;
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
    property ShowLines: boolean read FShowLines write SetShowLines default False;
    property OnBeforeItemDraw: TBeforeDrawItemEvent read FOnBeforeItemDraw write FOnBeforeItemDraw;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnVScroll:    TNotifyEvent read FOnVScroll    write FOnVScroll;
    property Color stored ColorStored;
    property OnItemIndexChanged: TNotifyEvent read FOnItemIndexChanged write FOnItemIndexChanged;
    property OnItemCountChanged: TNotifyEvent read FOnItemCountChanged write FOnItemCountChanged;
end;
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsListBox = class(TsCustomListBox);


implementation

uses math, ExtCtrls,
  sVCLUtils, sMessages, sGraphUtils, sAlphaGraph, acntUtils, sStyleSimply, sSkinManager;


procedure TsCustomListBox.Change;
begin
  if Assigned(FOnItemIndexChanged) then FOnItemIndexChanged(Self);
end;


procedure TsCustomListBox.ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
{$IFNDEF D2005}
  if not (Style in [lbOwnerDrawFixed, lbOwnerDrawVariable]) then
    ItemHeight := MulDiv(ItemHeight, M, D);
{$ENDIF}
end;


procedure TsCustomListBox.CNCommand(var AMessage: TWMCommand);
begin

  inherited;
  if (AMessage.NotifyCode = LBN_SELCHANGE) and (FItemIndex <> ItemIndex) then
  begin
    FItemIndex := ItemIndex;
    Change;
  end;
end;

procedure TsCustomListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  XOffset: integer;
  OldDC: hdc;
begin
  if SkinData.Skinned then
    with Message.DrawItemStruct^ do begin
      State := TOwnerDrawState(LongRec(itemState).Lo);
      if (SkinData.PrintDC <> hDC) {$IFDEF DELPHI6UP}and Canvas.HandleAllocated{$ENDIF} then
        OldDC := Canvas.Handle
      else
        OldDC := 0;

      Canvas.Handle := hDC;
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      if Message.Result = 0 then begin // If received not from WM_PAINT handler
        if (Columns = 0) and (ListSW <> nil) and (ListSW.sBarHorz <> nil) and FullPaint then
          XOffset := ListSW.sBarHorz.ScrollInfo.nPos
        else
          XOffset := 0;

        OffsetRect(rcItem, XOffset, 0);
        State := State + [odReserved1];
//        if Focused and (integer(Message.DrawItemStruct^.itemID) = ItemIndex) and (odSelected in State) then
//          State := State + [odFocused];
      end;
      DrawItem(integer(itemID), rcItem, State);
      if SkinData.PrintDC <> hDC then
        Canvas.Handle := OldDC;
    end
  else
    inherited;
end;


constructor TsCustomListBox.Create(AOwner: TComponent);
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsListBox;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FAutoCompleteDelay := 500;
  FAutoHideScroll := True;
  FLeftPadding := 0;
  FDisabledKind := DefDisabledKind;
  FShowLines := False;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  DoubleBuffered := False;

  FItemsCount := 0;
  FItemIndex := -1;
end;


procedure TsCustomListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Style = lbStandard then
    Params.Style := Params.Style or LBS_OWNERDRAWFIXED and not LBS_OWNERDRAWVARIABLE;

  if not FAutoHideScroll then
    Params.Style := Params.Style or LBS_DISABLENOSCROLL;
end;


procedure TsCustomListBox.CreateWnd;
begin
  inherited;
  FCommonData.Loaded(False);
  if HandleAllocated then
    RefreshEditScrolls(SkinData, ListSW);
end;


destructor TsCustomListBox.Destroy;
begin
  FreeAndNil(ListSW);
  FreeAndNil(FCommonData);
  FreeAndNil(FBoundLabel);
  inherited Destroy;
end;


procedure TsCustomListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  i, w, h, XOffset, sNdx, l: integer;
  bItemFocused, bSelected: boolean;
  Bmp: Graphics.TBitmap;
  DrawStyle: Cardinal;
  TmpColor: TColor;
  fs: TFontStyles;
  CI: TCacheInfo;
  SavedData: hdc;
  aRect: TRect;
  s: PACChar;

  procedure ShowLine(Canvas: TCanvas; ARect: TRect);
  begin
    if ShowLines then begin
      Canvas.Pen.Color := SkinData.SkinManager.Palette[pcGrid];
      acPaintLine(Canvas.Handle, 0, ARect.Bottom - 1, Width, ARect.Bottom - 1);
    end;
  end;

begin
  if Index >= 0 then begin
    DrawStyle := DT_NOPREFIX or DT_EXPANDTABS or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP or DT_ALPHATEXT * Cardinal(SkinData.TranspMode = 2);
    if UseRightToLeftReading then
      DrawStyle := DrawStyle or DT_RTLREADING or DT_RIGHT;

{$IFDEF DELPHI6UP}
  {$IFDEF TNTUNICODE}
    s := PacChar(TTntStrings(GetItemTextDirect(Index)));
  {$ELSE}
    s := PacChar(GetItemTextDirect(Index));
  {$ENDIF}
{$ELSE}
    s := PacChar({$IFDEF TNTUNICODE}TTntStrings{$ENDIF}(Items)[Index]);
{$ENDIF}
    l := Items.Count;
    bSelected := odSelected in State;
    if SkinData.Skinned then begin
      w := WidthOf (Rect, True);
      h := HeightOf(Rect, True);
      if (w > 0) and (h > 0) then begin
        Bmp := CreateBmp32(w, h);
        if (BorderStyle <> bsNone) and (ListSW <> nil) then
          CI := MakeCacheInfo(SkinData.FCacheBmp, ListSW.cxLeftEdge, ListSW.cyTopEdge)
        else
          CI := MakeCacheInfo(SkinData.FCacheBmp);

        if (Columns = 0) and (ListSW <> nil) and (ListSW.sBarHorz <> nil) then
          XOffset := ListSW.sBarHorz.ScrollInfo.nPos
        else
          XOffset := 0;

        if odReserved1 in State then
          BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, SkinData.FCacheBmp.Canvas.Handle, Rect.Left + CI.X - 2 * XOffset, Rect.Top + CI.Y, SRCCOPY)
        else
          BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, SkinData.FCacheBmp.Canvas.Handle, Rect.Left + CI.X - XOffset, Rect.Top + CI.Y, SRCCOPY);

        bItemFocused := (odFocused in State) or Focused;
        if Assigned(OnDrawItem) then begin
          BitBlt(Canvas.Handle, Rect.Left - XOffset, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          if bSelected then begin
            Canvas.Brush.Color := SkinData.SkinManager.GetHighLightColor(bItemFocused);
            Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(bItemFocused);
          end
          else begin
            Canvas.Brush.Color := Color;
            Canvas.Font.Color := Font.Color;
          end;
          fs := Canvas.Font.Style;
          if Assigned(FOnBeforeItemDraw) then
            FOnBeforeItemDraw(Canvas.Handle, Index, Rect);

          OnDrawItem(Self, Index, Rect, State);
          ShowLine(Canvas, Rect);
          Canvas.Font.Style := fs;
          FreeAndNil(Bmp);
          Exit;
        end
        else begin
          ShowLine(Bmp.Canvas, MkRect(Bmp));
          if bSelected then begin
            sNdx := SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection];
            if sNdx < 0 then
              FillDC(Bmp.Canvas.Handle, MkRect(Bmp), SkinData.SkinManager.GetHighLightColor(bItemFocused))
            else
              PaintItem(sNdx, CI, True, integer(bItemFocused), MkRect(Bmp), Rect.TopLeft, Bmp, SkinData.CommonSkinData)
          end
          else
            sNdx := -1;

          if IsValidIndex(Index, l) then begin
            Bmp.Canvas.Font.Assign(Font);
            aRect := MkRect(Bmp);
            InflateRect(aRect, -1, 0);
            if (ListSW <> nil) and (ListSW.cxLeftEdge < integer(BorderStyle = bsSingle) * (1 + integer(Ctl3D))) then
              inc(aRect.Left);

            if sNdx < 0 then begin
              if bSelected then
                Bmp.Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(bItemFocused)
              else
                if not SkinData.CustomFont then
                  with SkinData.CommonSkinData.gd[SkinData.SkinIndex] do begin
                    i := integer(ControlIsActive(SkinData) and (States > 1));
                    if (SkinData.SkinSection = '') or not NeedParentFont(SkinData.SkinManager, SkinData.SkinIndex, i) then
                      Bmp.Canvas.Font.Color := Props[i].FontColor.Color
                    else
                      Bmp.Canvas.Font.Color := GetFontColor(Parent, SkinData.SkinIndex, SkinData.SkinManager, i);
                  end
                else
                  Bmp.Canvas.Font.Color := Font.Color;

              if (odDisabled in State) and Enabled then
                Bmp.Canvas.Font.Color := BlendColors(Bmp.Canvas.Font.Color, Color, DefBlendDisabled);

              Bmp.Canvas.Brush.Style := bsClear;
              if Assigned(FOnBeforeItemDraw) then
                FOnBeforeItemDraw(Bmp.Canvas.Handle, Index, aRect);

              AcDrawText(Bmp.Canvas.Handle, s, aRect, DrawStyle);
            end
            else begin
              Bmp.Canvas.Brush.Style := bsClear;
              if Assigned(FOnBeforeItemDraw) then begin
                with SkinData.CommonSkinData.gd[sNdx] do begin
                  i := integer(ControlIsActive(SkinData) and (States > 1));
                  Bmp.Canvas.Font.Color := Props[i].FontColor.Color
                end;
                FOnBeforeItemDraw(Bmp.Canvas.Handle, Index, aRect);
              end;
              acWriteTextEx(Bmp.Canvas, PacChar(s), not (odDisabled in State), aRect, DrawStyle, sNdx, bItemFocused, SkinData.SkinManager);
            end;
            if SkinData.TranspMode = 1 then
              FillAlphaRect(Bmp, aRect);

            if bItemFocused and (sNdx < 0) and bSelected then begin
              InflateRect(aRect, 1, 0);
              DrawFocusRect(Bmp.Canvas.Handle, aRect);
            end;
          end;
          if not Enabled then
            BmpDisabledKind(Bmp, DisabledKind, Parent, CI, Rect.TopLeft);
        end;
        BitBlt(Canvas.Handle, Rect.Left - XOffset, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        FreeAndNil(Bmp);
      end;
    end
    else begin
      Canvas.Font.Assign(Font);
      if bSelected then begin
        TmpColor := ColorToRGB(clHighLight);
        Canvas.Font.Color := ColorToRGB(clHighLightText);
      end
      else begin
        TmpColor := ColorToRGB(Color);
        Canvas.Font.Color := ColorToRGB(Font.Color);
      end;
      FillDC(Canvas.Handle, Rect, TmpColor);
      Canvas.Brush.Color := TmpColor;
      if odDisabled in State then
        Canvas.Font.Color := BlendColors(ColortoRGB(Canvas.Font.Color), TmpColor, DefBlendDisabled);

      if Assigned(FOnBeforeItemDraw) then begin
        SavedData := SaveDC(Canvas.Handle);
        FOnBeforeItemDraw(Canvas.Handle, Index, Rect);
      end
      else
        SavedData := 0;

      if Assigned(OnDrawItem) then begin
        OnDrawItem(Self, Index, Rect, State);
        if odFocused in State then begin
          InflateRect(Rect, 1, 0);
          DrawFocusRect(Canvas.Handle, Rect);
        end;
      end
      else begin
        InflateRect(Rect, -1, 0);
        if IsValidIndex(Index, Items.Count) then begin
          Canvas.Brush.Style := bsClear;
          AcDrawText(Canvas.Handle, s, Rect, DrawStyle);
          if odFocused in State then begin
            InflateRect(Rect, 1, 0);
            DrawFocusRect(Canvas.Handle, Rect);
          end;
        end;
      end;
      ShowLine(Canvas, Rect);
      if Assigned(FOnBeforeItemDraw) and (SavedData <> 0) then
        RestoreDc(Canvas.Handle, SavedData);
    end;
  end;
end;


function TsCustomListBox.ColorStored: boolean;
begin
  Result := not SkinData.Skinned or SkinData.CustomColor;
end;


function TsCustomListBox.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


{$IFDEF DELPHI6UP}
function TsCustomListBox.GetItemTextDirect(AIndex: integer): acString;
var
  Len: Integer;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := DoGetData(AIndex)
  else begin
    Len := TrySendMessage(Handle, LB_GETTEXTLEN, AIndex, 0);
    if Len <> LB_ERR then begin
      SetLength(Result, Len);
      if Len <> 0 then begin
        Len := TrySendMessage(Handle, LB_GETTEXT, AIndex, LPARAM(PChar(Result)));
        SetLength(Result, Len);
      end;
    end;
  end;
end;
{$ENDIF}


procedure TsCustomListBox.SetAutoHideScroll(const Value: boolean);
begin
  if FAutoHideScroll <> Value then begin
    FAutoHideScroll := Value;
    if not (csLoading in ComponentState) then
      RecreateWnd;
  end;
end;


procedure TsCustomListBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsCustomListBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  if FItemIndex <> ItemIndex then
    begin
      FItemIndex := ItemIndex;
      Change;
    end;
end;

procedure TsCustomListBox.SetLeftPadding(const Value: byte);
begin
  if FLeftPadding <> Value then begin
    FLeftPadding := Value;
    if not (csLoading in ComponentState) then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
  end;
end;


procedure TsCustomListBox.SetShowLines(const Value: boolean);
begin
  if FShowLines <> Value then begin
    FShowLines := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomListBox.WMEraseBkGnd(var Message: TWMPaint);
var
  R: TRect;
  h: integer;
  DC, SavedDC: HDC;
begin
  if SkinData.Skinned then begin
    if not SkinData.FUpdating then begin
      if SkinData.BGChanged then
        PrepareCache(SkinData);

      if TWMPaint(Message).DC = 0 then
        DC := GetDC(Handle)
      else
        DC := TWMPaint(Message).DC;

      SavedDC := SaveDC(DC);
      try
        if Items.Count > 0 then begin
          R := ItemRect(Items.Count - 1);
          // Left rect
          if Columns > 0 then begin
            h := ClientHeight - ClientHeight mod ItemHeight;
            ExcludeClipRect(DC, 0, 0, R.Left, h);
          end;
          // Top rect
          ExcludeClipRect(DC, 0, 0, R.Right, R.Bottom);
        end;
        if ListSW = nil then
          BitBlt(DC, 0, 0, ClientWidth, ClientHeight, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          BitBlt(DC, 0, 0, ClientWidth, ClientHeight, SkinData.FCacheBmp.Canvas.Handle, ListSW.cxLeftEdge, ListSW.cyTopEdge, SRCCOPY);
      finally
        RestoreDC(DC, SavedDC);
      end;

      if TWMPaint(Message).DC = 0 then
        ReleaseDC(Handle, DC);
    end;
    Message.Result := 1;
  end
  else
    inherited;
end;


procedure TsCustomListBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  inc(Message.CalcSize_Params.rgrc[0].Left, FLeftPadding);
end;


procedure TsCustomListBox.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
  if SkinData.Skinned then
    if InUpdating(SkinData) then begin
      BeginPaint(Handle, PS);
      EndPaint(Handle, PS);
    end
    else begin
      if SkinData.CtrlSkinState and ACS_PRINTING = 0 then // Prevent ghost painting when printing
        inherited;

      if Items.Count = 0 then
        WMEraseBkGnd(Message);
    end
  else
    inherited;
end;


procedure TsCustomListBox.WMPrint(var Message: TWMPaint);
var
  CurNdx, bwl, bwt: integer;
  MemBmp: Graphics.TBitmap;
  PS: TPaintStruct;
  DC: hdc;

  procedure PaintListBox;
  var
    R: TRect;
    DrawItemMsg: TWMDrawItem;
    DrawItemStruct: TDrawItemStruct;
    MaxBottom, MaxRight, i: Integer;
    MeasureItemStruct: TMeasureItemStruct;
  begin
    FullPaint := True;
    R := MkRect(Width, 0);
    MaxBottom := 0;
    MaxRight := 0;
    for i := max(0, TopIndex) to Items.Count - 1 do begin
      R := ItemRect(i);
      if RectIsVisible(ClientRect, R) then begin
        MaxBottom := Max(R.Bottom, MaxBottom);
        MaxRight := Max(R.Right, MaxRight);
        DrawItemMsg.Msg := CN_DRAWITEM;
        DrawItemMsg.DrawItemStruct := @DrawItemStruct;
        DrawItemMsg.Ctl := Handle;
        DrawItemMsg.Result := 1;
        DrawItemStruct.CtlType := ODT_LISTBOX;
        DrawItemStruct.itemAction := ODA_DRAWENTIRE;
        if MultiSelect then begin
          DrawItemStruct.itemState := iff(Selected[i], ODS_SELECTED, 0);
          if (ItemIndex = i) and Focused then
            DrawItemStruct.itemState := DrawItemStruct.itemState or ODS_FOCUS;
        end
        else
          if CurNdx = i then begin
            DrawItemStruct.itemState := ODS_SELECTED;
            if Focused then
              DrawItemStruct.itemState := DrawItemStruct.itemState or ODS_FOCUS;
          end
          else
            DrawItemStruct.itemState := 0;

        DrawItemStruct.hDC := MemBmp.Canvas.Handle;
        DrawItemStruct.CtlID := Handle;
        DrawItemStruct.hwndItem := Handle;
        DrawItemStruct.rcItem := R;

        MeasureItemStruct.itemWidth := WidthOf(R);
        MeasureItemStruct.itemHeight := ItemHeight;
        if IsValidIndex(i, Items.Count) and (GetItemData(i) <> LB_ERR) then
          DrawItemStruct.itemData := ACNativeUInt(Pointer(Items.Objects[I]))
        else
          DrawItemStruct.itemData := 0;

        DrawItemStruct.itemID := I;
        Dispatch(DrawItemMsg);
      end
      else
        Break;
    end;
    // Copy BG under last item
    BitBlt(MemBmp.Canvas.Handle, R.Left, R.Bottom, WidthOf(R), ClientHeight - R.Bottom, FCommonData.FCacheBmp.Canvas.Handle, R.Left + bwl, R.Bottom + bwt, SRCCOPY);
    // Bottom line
    BitBlt(MemBmp.Canvas.Handle, 0, MaxBottom, ClientWidth, ClientHeight - MaxBottom, FCommonData.FCacheBmp.Canvas.Handle, bwl, MaxBottom + bwt, SRCCOPY);

    if R.Right < ClientWidth then // Copy BG at the right
      BitBlt(MemBmp.Canvas.Handle, R.Right, 0, Width - bwl - R.Right, ClientHeight, FCommonData.FCacheBmp.Canvas.Handle, R.Right + bwl, bwt, SRCCOPY);

    FullPaint := False;
  end;

begin
  if SkinData.Skinned or (Style in [lbOwnerDrawVariable]) then begin
    BeginPaint(Handle, PS);
    if not InUpdating(SkinData) then begin
      if (BorderStyle <> bsNone) and (ListSW <> nil) then begin
        bwl := iff(Ctl3d, ListSW.cxLeftEdge, 1);
        bwt := iff(Ctl3d, ListSW.cyTopEdge, 1);
      end
      else begin
        bwl := 0;
        bwt := 0;
      end;
      MemBmp := CreateBmp32(Self);
      MemBmp.Canvas.Lock;
      Canvas.Handle := MemBmp.Canvas.Handle;
      Canvas.Lock;
      Canvas.Font.Assign(Font);
      CurNdx := ItemIndex;
      PaintListBox;
      Canvas.UnLock;
      Canvas.Handle := 0;

      if Message.DC <> 0 then
        DC := Message.DC
      else
        DC := GetDC(Handle);

      BitBlt(DC, 0, 0, Width, Height, MemBmp.Canvas.Handle, 0, 0, SRCCOPY);
      MemBmp.Free;
      if Message.DC <> DC then
        ReleaseDC(Handle, DC);
    end;
    EndPaint(Handle, PS);
  end
  else
    inherited;
end;


procedure TsCustomListBox.WndProc(var Message: TMessage);
var
  M: TMessage;
  bw: integer;
  DC: hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD: begin
      case Message.WParamHi of
        AC_CTRLHANDLED:
          Message.Result := 1;

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
            FreeAndNil(ListSW);

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then begin
            if HandleAllocated then
              RefreshEditScrolls(SkinData, ListSW);

            CommonMessage(Message, FCommonData);
            if not InAnimationProcess then
              RedrawWindow(Handle, nil, 0, RDWA_REPAINT);
          end;

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            if HandleAllocated and IsWindowVisible(Handle) then
              RefreshEditScrolls(SkinData, ListSW);
          end;

        AC_GETCONTROLCOLOR:
          if not FCommonData.Skinned then
            Message.Result := ColorToRGB(Color);

        AC_ENDUPDATE:
          Perform(CM_INVALIDATE, 0, 0);

        AC_GETDEFSECTION: begin
          Message.Result := 0 + 1;
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if (FCommonData <> nil) and (FCommonData.SkinManager <> nil) then
            Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssEdit] + 1;

          Exit;
        end;

        AC_SETSCALE: begin
          CommonMessage(Message, FCommonData);
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
          Exit;
        end;

        AC_SETGLASSMODE: begin
          CommonMessage(Message, FCommonData);
          Exit;
        end;
      end;
      if SkinData.Skinned then // Do not call it again
        Exit;
    end;

    CM_MOUSEENTER, CM_MOUSELEAVE: begin
      CommonWndProc(Message, FCommonData);
      inherited;
      if Enabled and not (csDesigning in ComponentState) then
        case SkinData.FMouseAbove of
          True:  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
          False: if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
        end;

      Exit;
    end;

    WM_PRINT:
      if (Style <> lbStandard) and (TWMPaint(Message).DC <> 0) then begin
        Canvas.Handle := TWMPaint(Message).DC;
        M := MakeMessage(WM_PAINT, Message.WParam, Message.LParam, 0);
        WMPrint(TWMPaint(M));
        Ac_NCDraw(ListSW, Handle, -1, TWMPaint(Message).DC);
        Exit;
      end;


    LB_ADDSTRING, LB_INSERTSTRING, LB_DELETESTRING:
      begin
        inherited;
        if (Message.Result >= 0) then
          begin
            FItemsCount := Items.Count;
            if Assigned(FOnItemCountChanged) then FOnItemCountChanged(Self);
          end;
        Exit;
      end;

    LB_RESETCONTENT:
      begin
        FItemsCount := Items.Count;
        inherited;
        if (FItemsCount <> Items.Count) then
          begin
            FItemsCount := Items.Count;
            if Assigned(FOnItemCountChanged) then FOnItemCountChanged(Self);
          end;
        Exit;
      end;
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then begin
    case Message.Msg of
      WM_NCPAINT:
        if (FLeftPadding > 0) and HandleAllocated and IsWindowVisible(Handle) then begin
          DC := GetWindowDC(Handle);
          try
            bw := iff(BorderStyle <> bsNone, 1 + integer(Ctl3d), 0);
            FillDC(DC, Rect(bw, bw, bw + FLeftPadding, Height - bw), Color);
          finally
            ReleaseDC(Handle, DC);
          end;
        end;
    end;
    inherited;
  end
  else begin
    case Message.Msg of
      WM_VSCROLL, WM_HSCROLL: begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
        SkinData.FUpdating := True;
      end;

      WM_MOUSEWHEEL:
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      WM_MOUSEWHEEL: begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        InvalidateRect(Handle, nil, True);
      end;

      WM_VSCROLL, WM_HSCROLL: begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        SkinData.FUpdating := False;
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ERASE);
      end;

      WM_ENABLE:
        if Visible and not (csLoading in ComponentState) then begin
          FCommonData.BGChanged := True;
          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ERASE);
        end;

      WM_SIZE:
        if Visible and not (csLoading in ComponentState) {$IFDEF DELPHI7UP} and Canvas.HandleAllocated {$ENDIF} then begin
          Perform(WM_NCPAINT, 0, 0);
          Perform(WM_ERASEBKGND, 0, 0);
        end;

    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


{$IFNDEF DELPHI6UP}
function TsCustomListBox.Count: integer;
begin
  Result := Items.Count;
end;
{$ENDIF}


procedure TsCustomListBox.Loaded;
begin
  inherited;
  FCommonData.Loaded(False);
  if FLeftPadding > 0 then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

  if HandleAllocated then
    RefreshEditScrolls(SkinData, ListSW);
end;

end.

