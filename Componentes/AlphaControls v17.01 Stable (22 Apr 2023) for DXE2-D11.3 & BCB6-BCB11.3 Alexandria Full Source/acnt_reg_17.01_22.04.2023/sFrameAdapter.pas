unit sFrameAdapter;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  acntTypes, sSkinManager, sConst, sCommondata, acSBUtils, sDefaults;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsFrameAdapter = class(TComponent)
  private
    FThirdParty: TsThirdPartyList;
    FAllowSkin3rdParty: boolean;
{$IFNDEF NOTFORHELP}
  protected
    bRecFlag,
    FOwnThirdLists: boolean;

    Frame: TFrame;
    ListSW: TacScrollWnd;
    OldWndProc: TWndMethod;
    FCommonData: TsScrollWndData;
    function PrepareCache: boolean;
    procedure OurPaintHandler(aDC: hdc);
    procedure AC_WMPaint(const aDC: hdc);
    procedure NewWndProc(var Message: TMessage);
  public
    ThirdLists: TStringLists;
    function GetFrame: TFrame;
    procedure Loaded; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function OwnThirdLists: boolean;
{$ENDIF} // NOTFORHELP
  published
    property AllowSkin3rdParty: boolean read FAllowSkin3rdParty write FAllowSkin3rdParty default DefFrameAllowSkin3rdParty;
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
    property ThirdParty: TsThirdPartyList read FThirdParty write FThirdParty;
  end;


{$IFNDEF NOTFORHELP}
procedure UpdateAdapterThirdNames(fra: TsFrameAdapter);
{$ENDIF}

implementation

uses
  menus,
  sVclUtils, sGraphUtils, acntUtils, sMessages, sStyleSimply, sSpeedButton;


const
  sWinControlForm = 'TWinControlForm';


procedure UpdateAdapterThirdNames(fra: TsFrameAdapter);
var
  i: integer;
begin
  if Assigned(fra) then
    for i := 0 to High(acThirdNames) do
      fra.ThirdParty.SetString(i, fra.ThirdLists[i].Text);
end;


procedure TsFrameAdapter.AC_WMPaint(const aDC: hdc);
var
  DC, SavedDC: hdc;
  PS: TPaintStruct;
begin
//  if not (csDestroying in Frame.ComponentState) and (Frame.Parent <> nil) and not IsCached(FCommonData) then
//    InvalidateRect(Frame.Handle, nil, True); // Background update (for repaint of graphic controls and for a frame refreshing)
  BeginPaint(Frame.Handle, PS);
  if aDC = 0 then
    DC := GetDC(Frame.Handle)
  else
    DC := aDC;

  SavedDC := SaveDC(DC);
  try
    if not InUpdating(FCommonData) then
      OurPaintHandler(DC);
  finally
    RestoreDC(DC, SavedDC);
    if aDC = 0 then
      ReleaseDC(Frame.Handle, DC);

    EndPaint(Frame.Handle, PS);
  end;
end;


procedure TsFrameAdapter.AfterConstruction;
begin
  inherited;
  if Assigned(Frame) and not (csDesigning in ComponentState) then begin
    Load3rdNames(ThirdParty, ThirdLists, FOwnThirdLists);
    FCommonData.InitCommonProp;
    if SkinData.SkinManager <> nil then
      Frame.Perform(SM_ALPHACMD, AC_SETNEWSKIN_HI, LParam(SkinData.SkinManager));
  end;
end;


constructor TsFrameAdapter.Create(AOwner: TComponent);
var
  i: integer;
begin
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsFrameAdapter;
  FAllowSkin3rdParty := DefFrameAllowSkin3rdParty;

  FThirdParty := TsThirdPartyList.Create;
  SetLength(ThirdLists, High(acThirdNames) + 1);
  for i := 0 to High(acThirdNames) do begin
    ThirdLists[i] := TStringList.Create;
    {$IFDEF DELPHI6UP}
    ThirdLists[i].CaseSensitive := True;
    {$ENDIF}
  end;

  inherited Create(AOwner);
  bRecFlag := False;
  if not (AOwner is TCustomFrame) then begin
    Frame := nil;
    Raise Exception.Create(acs_FrameAdapterError1);
  end;
  Frame := TFrame(AOwner);
  FCommonData.FOwnerControl := TControl(AOwner);
  if (Frame <> nil) and not CtrlIsSkinned(Frame) then begin
    OldWndProc := Frame.WindowProc;
    Frame.WindowProc := NewWndProc;
  end;
  FCommonData.InitCommonProp;
end;


destructor TsFrameAdapter.Destroy;
var
  i: integer;
begin
  if [csDesigning, csLoading] * ComponentState = [] then
    DelObjLink(Self{Frame}, GetParentForm(Frame));

  FreeAndNil(ListSW);
  if (Frame <> nil) and Assigned(OldWndProc) then
    Frame.WindowProc := OldWndProc;

  Frame := nil;
  FreeAndNil(FCommonData);

  for i := 0 to Length(ThirdLists) - 1 do
    if ThirdLists[i] <> nil then
      FreeAndNil(ThirdLists[i]);

  SetLength(ThirdLists, 0);
  FreeAndNil(FThirdParty);

  inherited Destroy;
end;


function TsFrameAdapter.GetFrame: TFrame;
begin
  Result := Frame;
end;


type
  TAccessCommonData = class(TsCommonData);


procedure TsFrameAdapter.Loaded;
var
  i: integer;
begin
  inherited Loaded;
  Load3rdNames(ThirdParty, ThirdLists, FOwnThirdLists);
  if Assigned(Frame) and WndIsSkinned(Frame.Handle) and Assigned(SkinData) and Assigned(SkinData.SkinManager) then
    with SkinData, SkinManager, SkinData.CommonSkinData do begin
      UpdateIndexes;
      if IsActive then
        if (csDesigning in Frame.ComponentState) and // Updating of form color in design-time
             (Frame.Parent.ClassName = sWinControlForm) and
               IsValidSkinIndex(SkinManager.SkinCommonInfo.IndexGlobalInfo) then
{$IFDEF SKININDESIGN}
          TacAccessControl(Frame.Parent).Color := gd[SkinCommonInfo.IndexGlobalInfo].Props[0].Color
{$ENDIF}
        else begin
          // Popups initialization
          for i := 0 to Frame.ComponentCount - 1 do
            if (Frame.Components[i] is TPopupMenu) and SkinnedPopups then
              SkinableMenus.HookPopupMenu(TPopupMenu(Frame.Components[i]), True);

   			  if [csDesigning, csLoading] * ComponentState = [] then
            AddObjLink(Self, GetParentForm(Frame));

          if Skinned and (srThirdParty in SkinningRules) then
            AddToAdapter(Frame);

          if not TAccessCommonData(SkinData).GlassModeInit then
            UpdateGlassMode(Frame);
        end;

      FCommonData.InitCommonProp;
    end;
end;


procedure TsFrameAdapter.NewWndProc(var Message: TMessage);
var
  b, bSendBGChanged: boolean;
  m: TMessage;
  i: integer;
begin
{$IFDEF LOGGED}
  if Frame.Tag = 3 then
    AddToLog(Message);
{$ENDIF}
{$IFNDEF SKININDESIGN}
  if csDesigning in ComponentState then
    OldWndProc(Message)
  else
{$ENDIF}
  begin
    case Message.Msg of
      WM_DESTROY:
        if [csDesigning, csLoading] * ComponentState = [] then
          DelObjLink(Self{Frame}, GetParentForm(Frame));

      SM_ALPHACMD:
        case Message.WParamHi of
          AC_CTRLHANDLED: begin
            Message.Result := 1;
            Exit;
          end;

          AC_SETNEWSKIN:
            if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
              CommonWndProc(Message, FCommonData);
              AlphaBroadCast(Frame, Message);
              if Assigned(SkinData.SkinManager) then
                with SkinData, SkinManager, SkinData.CommonSkinData, SkinCommonInfo do
                  if (csDesigning in Frame.ComponentState) then begin
{$IFDEF SKININDESIGN}
                    if SkinCommonInfo.IndexGlobalInfo >= 0 then begin // Updating of form color in design-time
                      TacAccessControl(Frame.Parent).Color := gd[IndexGlobalInfo].Props[0].Color;
                      TacAccessControl(Frame.Parent).Font.Color := gd[IndexGlobalInfo].Props[0].FontColor.Color;
                    end;
{$ENDIF}
                  end
                  else
                    for i := 0 to Frame.ComponentCount - 1 do
                      if (Frame.Components[i] is TPopupMenu) and SkinnedPopups then
                        SkinableMenus.HookPopupMenu(TPopupMenu(Frame.Components[i]), Active);

              Exit;
            end;

          AC_REFRESH:
            if RefreshNeeded(SkinData, Message) then begin
              CommonWndProc(Message, FCommonData);
              FCommonData.UpdateIndexes;
              if Message.WParamLo <> 1 then
                AlphaBroadcast(Frame, Message);

              if Assigned(FCommonData.SkinManager) then
                with SkinData, SkinManager, SkinData.CommonSkinData do
                  if (csDesigning in Frame.ComponentState) then begin // Updating of form color in design-time
{$IFDEF SKININDESIGN}
                    if SkinCommonInfo.IndexGlobalInfo >= 0 then begin
                      TacAccessControl(Frame.Parent).Color := gd[SkinCommonInfo.IndexGlobalInfo].Props[0].Color;
                      TacAccessControl(Frame.Parent).Font.Color := gd[SkinCommonInfo.IndexGlobalInfo].Props[0].FontColor.Color;
                    end;
{$ENDIF}
                  end
                  else
                    if IsWindowVisible(Frame.Handle) then begin
                      RedrawWindow(Frame.Handle, nil, 0, RDWA_ALLNOW);
                      RefreshScrolls(SkinData, ListSW);
                    end;

              SetClassLong(Frame.Handle, GCL_HBRBACKGROUND, Integer(SkinData.SkinManager.Brushes[pcMainColor]));
              Exit;
            end;

          AC_STOPFADING: begin
            AlphaBroadcast(Frame, Message);
            Exit;
          end;

          AC_SETSCALE: begin
            CommonMessage(Message, FCommonData);
            Exit;
          end;

          AC_BEFORESCROLL:
            if GetBoolMsg(Frame, AC_CHILDCHANGED) or FCommonData.RepaintIfMoved then
              Frame.Perform(WM_SETREDRAW, 0, 0);

          AC_AFTERSCROLL:
            if (TrySendMessage(Frame.Handle, SM_ALPHACMD, MakeWParam(AC_CHILDCHANGED, 1{Kill effects}), 0) = 1) or FCommonData.RepaintIfMoved then begin
//            if GetBoolMsg(Frame, AC_CHILDCHANGED) or FCommonData.RepaintIfMoved then begin
              Frame.Perform(WM_SETREDRAW, 1, 0);
              RedrawWindow(Frame.Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);//RDWA_ALLNOW);
            end;

          AC_REMOVESKIN:
            if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
              if ListSW <> nil then
                FreeAndNil(ListSW);

              CommonWndProc(Message, FCommonData);
              if (csDesigning in Frame.ComponentState) then begin // Updating of form color in design-time
{$IFDEF SKININDESIGN}
                if Assigned(Frame.Parent) then begin
                  TacAccessControl(Frame.Parent).Color := clBtnFace;
                  TacAccessControl(Frame.Parent).Font.Color := clWindowText;
                end;
{$ENDIF}
              end
              else
                if not Application.Terminated then begin
                  SetClassLong(Frame.Handle, GCL_HBRBACKGROUND, Integer(GetSysColorBrush(COLOR_BTNFACE)));
                  SetWindowPos(Frame.Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
                  if IsWindowVisible(Frame.Handle) then begin
                    RedrawWindow(Frame.Handle, nil, 0, RDWA_ALLNOW);
                    Application.ProcessMessages; // Repaint graphic controls
                  end;
                end;

              AlphaBroadcast(Frame, Message);
              Exit;
            end;

          AC_PREPARECACHE: begin
            if not InUpdating(SkinData) and not PrepareCache then
              SkinData.Updating := True;

            Exit;
          end;

          AC_GETDEFSECTION: begin
            Message.Result := 2 + 1;
            Exit;
          end;

          AC_GETDEFINDEX: begin
{$IFNDEF ALITE}
            if SkinData.SkinSection <> '' then begin
              SkinData.UpdateIndexes;
              Message.Result := SkinData.SkinIndex + 1;
            end
            else
{$ENDIF}
              if SkinData.SkinManager <> nil then
                Message.Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssTransparent] + 1;

            Exit;
          end;

          AC_GETSKININDEX: begin
            PacSectionInfo(Message.LParam)^.siSkinIndex := FCommonData.SkinIndex;
            Exit;
          end;

          AC_GETSKINDATA: begin
            Message.Result := LResult(SkinData);
            Exit;
          end;

          AC_GETCONTROLCOLOR: if not SkinData.Skinned then begin
            Message.Result := Frame.Color;
            Exit;
          end;

          AC_SETGLASSMODE: begin
            CommonMessage(Message, FCommonData);
            Exit;
          end;
        end;

      WM_SIZE:
        SkinData.BGChanged := True;
    end;

    if (csDestroying in ComponentState) or
         (csDestroying in Frame.ComponentState) or
           not FCommonData.Skinned or
             not ((SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive) then begin
      if SkinData.SkinManager <> nil then
        if SkinData.SkinManager.IsActive then
          case Message.Msg of
            CM_SHOWINGCHANGED, CM_VISIBLECHANGED:
              if Assigned(Frame) and Frame.HandleAllocated then begin
                SkinData.UpdateIndexes;
                m := MakeMessage(SM_ALPHACMD, AC_SETNEWSKIN_HI, LPARAM(SkinData.SkinManager), 0);
                AlphaBroadCast(Frame, m);
              end;
          end;

      OldWndProc(Message);
    end
    else
      case Message.Msg of
        SM_ALPHACMD:
          case Message.WParamHi of
            AC_CHILDCHANGED:
              with SkinData, CommonSkinData do begin
                if (SkinIndex < 0) or not Assigned(SkinManager) then
                  Message.LParam := 0
                else
                  with gd[SkinIndex].Props[0] do
                    Message.LParam := LPARAM((GradientPercent + ImagePercent > 0) or RepaintIfMoved);

                Message.Result := Message.LParam;
                Exit;
              end;

            AC_UPDATING: begin
              FCommonData.Updating := Message.WParamLo = 1;
              for i := 0 to Frame.ControlCount - 1 do
                Frame.Controls[i].Perform(Message.Msg, Message.WParam, Message.LParam);

              Exit;
            end;

            AC_ENDPARENTUPDATE: begin
              if FCommonData.FUpdating {$IFDEF D2006} and ([csRecreating, csAligning] * Frame.ControlState = []) {$ENDIF} then begin
                if not InUpdating(FCommonData, True) then begin
                  b := SkinData.BGChanged and Assigned(Frame.OnResize);
                  if SkinData.BGChanged then
                    PrepareCache;

                  RedrawWindow(Frame.Handle, nil, 0, RDWA_ALLNOW);
                  if b then
                    Frame.OnResize(Frame);

                  SetParentUpdated(Frame);
                end;
              end;
              Exit;
            end;

            AC_PREPARECACHE: begin
              if not PrepareCache then
                SkinData.Updating := True;

              Message.Result := 1;
              Exit;
            end;

            AC_GETCONTROLCOLOR:
              if SkinData.Skinned then
                with SkinData, CommonSkinData do
                  case gd[Skinindex].Props[0].Transparency of
                    0:
                      Message.Result := LRESULT(gd[Skinindex].Props[0].Color);

                    100:
                      if Frame.Parent <> nil then begin
                        Message.Result := Frame.Parent.Perform(SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, 0);
                        if Message.Result = clFuchsia {if AlphaMessage not supported} then
                          Message.Result := LRESULT(TacAccessControl(Frame.Parent).Color)
                      end
                      else
                        Message.Result := LRESULT(ColorToRGB(Frame.Color));

                    else begin
                      if Frame.Parent <> nil then
                        Message.Result := Frame.Parent.Perform(SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, 0)
                      else
                        Message.Result := LRESULT(ColorToRGB(Frame.Color));
                      // Mixing of colors
                      C1.C := TColor(Message.Result);
                      with gd[Skinindex].Props[0] do begin
                        C2.C := Color;
                        C1.R := ((C1.R - C2.R) * Transparency + C2.R shl 8) shr 8;
                        C1.G := ((C1.G - C2.G) * Transparency + C2.G shl 8) shr 8;
                        C1.B := ((C1.B - C2.B) * Transparency + C2.B shl 8) shr 8;
                      end;
                      Message.Result := LRESULT(C1.C);
                    end
                  end
              else
                if Assigned(Frame) then
                  Message.LParam := LPARAM(ColorToRGB(TacAccessControl(Frame).Color));
            else
              try
                if CommonMessage(Message, SkinData) then
                  Exit;
              except
                Exit;
              end;
          end;

        WM_CREATE, WM_NCCREATE: begin
          OldWndProc(Message);
          if Frame.HandleAllocated then
            SkinData.InitCommonProp;
        end;

        WM_VSCROLL, WM_HSCROLL: begin
          OldWndProc(Message);
          UpdateScrolls(ListSW, True);
        end;

        CM_MOUSEENTER:
          if not (csDesigning in ComponentState) then begin
            OldWndProc(Message);
            for i := 0 to Frame.ControlCount - 1 do
              if (Frame.Controls[i] is TsSpeedButton) and
                   (Frame.Controls[i] <> Pointer(Message.LParam)) and
                     TsSpeedButton(Frame.Controls[i]).SkinData.FMouseAbove then
                Frame.Controls[i].Perform(CM_MOUSELEAVE, 0, 0);

            if DefaultManager <> nil then
              DefaultManager.ActiveControl := Frame.Handle;
          end;

        CM_VISIBLECHANGED: begin
          FCommonData.BGChanged := True;
          OldWndProc(Message);
          if Assigned(SkinData.SkinManager) then
            Frame.Perform(SM_ALPHACMD, AC_REFRESH_HI, {0);//,} LParam(SkinData.SkinManager));
        end;

        WM_WINDOWPOSCHANGED:
          if not bRecFlag then begin
            bRecFlag := True;
            bSendBGChanged := ((Message.Msg = WM_SIZE) and (FCommonData.BGType <> BGT_NONE)) or FCommonData.RepaintIfMoved;
            FCommonData.BGChanged := FCommonData.BGChanged or bSendBGChanged;
            if bSendBGChanged then begin
              m := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
              Frame.BroadCast(m);
            end;
            if Message.Msg <> WM_MOVE then
              InvalidateRect(Frame.Handle, nil, True);

            OldWndProc(Message);
            bRecFlag := False;
          end;

        WM_SIZE, WM_MOVE: begin
          bSendBGChanged := ((Message.Msg = WM_SIZE) and (FCommonData.BGType <> BGT_NONE)) or FCommonData.RepaintIfMoved;
          FCommonData.BGChanged := FCommonData.BGChanged or bSendBGChanged;
          if bSendBGChanged then begin
            m := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
            Frame.BroadCast(m);
          end;
          if Message.Msg <> WM_MOVE then
            InvalidateRect(Frame.Handle, nil, True);

          OldWndProc(Message);
        end;

        WM_PARENTNOTIFY:
          if (Message.WParamLo in [WM_CREATE, WM_DESTROY]) and not (csLoading in ComponentState) and not (csCreating in Frame.ControlState) then begin
            if (Message.WParamLo = WM_CREATE) and (srThirdParty in SkinData.SkinManager.SkinningRules) then begin
//              if not OwnThirdLists or SearchSupport(ThirdLists, Frame.ClassName) then
      			  if [csDesigning, csLoading] * ComponentState = [] then
	              AddObjLink(Self, GetParentForm(Frame));

              AddToAdapter(Frame);
//              if not TAccessCommonData(SkinData).GlassModeInit then
//                UpdateGlassMode(Frame);
            end;
            OldWndProc(Message);
            UpdateScrolls(ListSW, False);
          end
          else
            OldWndProc(Message);

        CM_SHOWINGCHANGED: begin
          CommonWndProc(Message, SkinData);
          OldWndProc(Message);
          RefreshScrolls(SkinData, ListSW);
          if srThirdParty in SkinData.SkinManager.SkinningRules then begin
//            if not OwnThirdLists or SearchSupport(ThirdLists, Frame.ClassName) then
  	     	  if [csDesigning, csLoading] * ComponentState = [] then
              AddObjLink(Self, GetParentForm(Frame));

            AddToAdapter(Frame);
          end;
          if IsWindowVisible(Frame.Handle) then begin
            if not TAccessCommonData(SkinData).GlassModeInit then
              UpdateGlassMode(Frame);

            SkinData.Updating := False;
            Frame.Repaint;
            SetParentUpdated(Frame);
          end;
        end;

        WM_PAINT:
          if (csDesigning in Frame.ComponentState) or (Frame.Parent = nil) or (csDestroying in ComponentState) then
            OldWndProc(Message)
          else begin
            AC_WMPaint(TWMPaint(Message).DC);
            Message.Result := 0;
          end;

        WM_PRINT:
          if FCommonData.Skinned then begin
            FCommonData.Updating := False;
            if ControlIsReady(Frame) then begin
              if ListSW = nil then
                RefreshScrolls(SkinData, ListSW);

              FCommonData.BGChanged := True;
              Ac_NCDraw(ListSW, Frame.Handle, -1, TWMPaint(Message).DC); // Scrolls painting

              if (ListSW <> nil) and (ListSW.sBarVert <> nil) then begin
                if ListSW.sBarVert.fScrollVisible and (Frame.BidiMode = bdRightToLeft) then
                  MoveWindowOrg(TWMPaint(Message).DC, GetScrollMetric(ListSW.sbarVert, SM_SCROLLWIDTH), 0);

                if ListSW.sBarHorz <> nil then
                  IntersectClipRect(TWMPaint(Message).DC, 0, 0,
                          SkinData.FCacheBmp.Width  - ListSW.cxLeftEdge - ListSW.cxRightEdge - integer(ListSW.sBarVert.fScrollVisible) * GetScrollMetric(ListSW.sBarVert, SM_SCROLLWIDTH),
                          SkinData.FCacheBmp.Height - ListSW.cyTopEdge - ListSW.cyBottomEdge - integer(ListSW.sBarHorz.fScrollVisible) * GetScrollMetric(ListSW.sBarHorz, SM_SCROLLWIDTH));
              end;

              OurPaintHandler(TWMPaint(Message).DC);
              if Assigned(Frame.OnResize) then
                Frame.OnResize(Frame);
            end;
          end
          else
            OldWndProc(Message);

        WM_NCPAINT:
          if csDesigning in Frame.ComponentState then
            OldWndProc(Message)
          else
            if not InUpdating(FCommonData) then
              Message.Result := 0;

        WM_ERASEBKGND:
          if (csDesigning in Frame.ComponentState) or not Frame.Showing then
            OldWndProc(Message)
          else
            if not InAnimationProcess then begin
              if not InUpdating(FCommonData) and not FCommonData.BGChanged then
// Desc 6                if csRecreating in Frame.ControlState then
//                  SkinData.FUpdating := True
//                else
                  OurPaintHandler(TWMPaint(Message).DC);

              Message.Result := 1;
            end;

        else
          OldWndProc(Message);
      end;
  end;
end;


procedure TsFrameAdapter.OurPaintHandler(aDC: hdc);
var
  R: TRect;
  i: integer;
  Changed: boolean;
  DC, SavedDC: hdc;
  BGInfo: TacBGInfo;

  procedure FillBG(aRect: TRect);
  begin
    FillDC(DC, aRect, GetBGColor(SkinData, 0))
  end;

begin
  if (aDC <> 0) and (not InAnimationProcess or (aDC = SkinData.PrintDC)) then begin
    DC := aDC;
    SavedDC := SaveDC(DC);
    if not InUpdating(FCommonData) then begin
      Changed := True;
      if (SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex < 0) and (FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Transparency = 100) and not HaveOuterEffects(SkinData) then begin // Parent BG is taken
        BGInfo.DrawDC := 0;
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, Frame.Parent);
        if (BGInfo.BgType = btCache) and (BGInfo.Bmp <> nil) then begin
          BitBlt(DC, 0, 0, Frame.Width, Frame.Height, BGInfo.Bmp.Canvas.Handle, Frame.Left + BGInfo.Offset.X, Frame.Top + BGInfo.Offset.Y, SRCCOPY);
          InitCacheBmp(SkinData);
          BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Frame.Width, Frame.Height, BGInfo.Bmp.Canvas.Handle, Frame.Left + BGInfo.Offset.X, Frame.Top + BGInfo.Offset.Y, SRCCOPY);
        end
        else begin
          SavedDC := SaveDC(DC);
          ExcludeControls(DC, Frame, 0, 0);
          FillBG(MkRect(Frame.Width, Frame.Height));
          RestoreDC(DC, SavedDC);
        end
      end
      else begin
        Changed := FCommonData.BGChanged or FCommonData.HalfVisible;
        i := GetClipBox(DC, R);
        if (i = 0){ or IsRectEmpty(R) {is not redrawn while resizing }then
          Exit;

        if SkinData.RepaintIfMoved and (Frame.Parent <> nil) then
          FCommonData.HalfVisible := (WidthOf(R, True) <> Frame.Width) or (HeightOf(R, True) <> Frame.Height)
        else
          FCommonData.HalfVisible := False;

        if Changed then
          if not PrepareCache then begin
            RestoreDC(DC, SavedDC);
            Exit;
          end;

        CopyWinControlCache(Frame, FCommonData, MkRect, MkRect(Frame), DC, True);
      end;
      sVCLUtils.PaintControls(DC, Frame, Changed, MkPoint, {0, }SkinData.PrintDC <> aDC);
      SetParentUpdated(Frame);
    end;
    RestoreDC(DC, SavedDC);
  end;
end;


function TsFrameAdapter.OwnThirdLists: boolean;
begin
  Result := FOwnThirdLists;
end;


function TsFrameAdapter.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
begin
//  SkinData.UpdateIndexes;
  if Frame.Parent <> nil then
    GetBGInfo(@BGInfo, Frame.Parent)
  else
    BGInfo.BgType := btNotReady;

  if BGInfo.BgType <> btNotReady then begin
    InitCacheBmp(SkinData);
    SkinData.FCacheBmp.Canvas.Font.Assign(Frame.Font);
    PaintItem(SkinData, BGInfoToCI(@BGInfo), False, 0, MkRect(Frame), MkPoint(Frame), SkinData.FCacheBMP, False);
    SkinData.PaintOuterEffects(Frame, MkPoint);
    SkinData.BGChanged := False;
    Result := True;
  end
  else begin
    Result := False;
    SkinData.Updating := True;
  end;
end;

end.


