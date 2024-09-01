unit sSkinMenus;
{$I sDefs.inc}
 //{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  {$IFDEF DELPHI6UP}  Types,      {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes,    {$ENDIF}
  {$IFDEF LOGGED}     sDebugMsgs, {$ENDIF}
  {$IFDEF TNTUNICODE} TntMenus,   {$ENDIF}
  acntTypes, sConst;


type
  TsMenuItemType = (smCaption, smDivider, smNormal, smTopLine);
  TsMenuManagerDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState; ItemType: TsMenuItemType) of object;


  TacMenuSupport = class(TPersistent)
  private
    FCustomFont,
    FUseExtraLine:   boolean;

    FFont,
    FExtraLineFont:  TFont;

    FAlphaBlend:     byte;
    FExtraLineWidth: integer;
    FIcoLineSkin:    TsSkinSection;
    FSkinnedShadows: boolean;
    FShowAccelCharAlways: boolean;
    procedure SetExtraLineFont(const Value: TFont);
    procedure SetCustomFont(const Value: boolean);
    procedure SetFont(const Value: TFont);

    function FontStored: boolean;
    function ExtraFontStored: boolean;
    function LineSkinStored: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AlphaBlend:     byte          read FAlphaBlend     write FAlphaBlend       default MaxByte - 1;
    property ExtraLineFont:  TFont         read FExtraLineFont  write SetExtraLineFont  stored ExtraFontStored;
    property ExtraLineWidth: integer       read FExtraLineWidth write FExtraLineWidth   default 32;
    property IcoLineSkin:    TsSkinSection read FIcoLineSkin    write FIcoLineSkin      stored LineSkinStored;
    property Font:           TFont         read FFont           write SetFont           stored FontStored;
    property CustomFont:     boolean       read FCustomFont     write SetCustomFont     default False;
    property UseExtraLine:   boolean       read FUseExtraLine   write FUseExtraLine     default False;
    property SkinnedShadows: boolean       read FSkinnedShadows write FSkinnedShadows   default True;
    property ShowAccelCharAlways: boolean  read FShowAccelCharAlways write FShowAccelCharAlways default False;
  end;


  TMenuItemData = record
    Item: TMenuItem;
    R: TRect;
  end;

  PacMenuInfo = ^TacMenuInfo;

  TacMenuInfo = record
    FirstItem: TMenuItem;
    Bmp:       TBitmap;
    Wnd:       hwnd;
    HaveExtraLine,
    ItemsIterated: boolean;
  end;


{$IFNDEF FPC}
  TsSkinableMenus = class(TPersistent)
  private
    FMargin,
    FSpacing,
    FBevelWidth,
    FBorderWidth,
    FSkinBorderWidth: integer;

    Form: TForm;
    PPI: integer;
    it: TsMenuItemType;
    FCaptionFont: TFont;
    FAlignment: TAlignment;
    procedure SetCaptionFont(const Value: TFont);
    function GetCaptionFont: TFont;
    function GetMargin: integer;
    function GetSpacing: integer;
    function GetBorderWidth: integer;
  protected
    ArOR: TRects;
    sd: TacObject;
    FOnDrawItem: TsMenuManagerDrawItemEvent;

    Pressed,
    FActive,
    BorderDrawing: boolean;

    FOwner: TComponent;

    function ParentHeight (aCanvas: TCanvas; Item: TMenuItem): integer;
    function GetItemHeight(aCanvas: TCanvas; Item: TMenuItem): integer;
    function ParentWidth  (aCanvas: TCanvas; Item: TMenuItem): integer;
    function GetItemWidth (aCanvas: TCanvas; Item: TMenuItem; const mi: PacMenuInfo): integer;
    procedure PaintDivider(aCanvas: TCanvas; aRect: TRect; Item: TMenuItem; MenuBmp: TBitmap; const mi: PacMenuInfo);
    procedure PaintCaption(aCanvas: TCanvas; aRect: TRect; Item: TMenuItem; BG:      TBitmap; const mi: PacMenuInfo);
    function IsRTL(Item: TMenuItem): boolean;
    function IsDivText(Item: TMenuItem): boolean;
    function ItemRect (Item: TMenuItem; aRect: TRect): TRect;
    function CursorMarginH: integer;
    function CursorMarginV: integer;
    procedure UpdateFont(aCanvas: TCanvas; aItem: TMenuItem; aBold: boolean; aCaption: boolean = False);

    function IsTopLine(Item: TMenuItem): boolean;
    procedure sMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure sAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    function PrepareMenuBG(mi: PacMenuInfo; Item: TMenuItem; Width, Height: integer; Wnd: hwnd = 0): TBitmap;

    procedure sMeasureLineItem     (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure sAdvancedDrawLineItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

    procedure SetActive(const Value: boolean);
    function NoBorder: boolean;
    function GlyphSize(Item: TMenuItem; Common: boolean = False): TSize;
    function RealBorderWidth: integer;
    function LastItem(Item: TMenuItem): boolean;
    function IsPopupItem(Item: TMenuItem): boolean;

    function ExtraWidth(const mip: PacMenuInfo): integer;
  public
    MenuProvider: TObject;
    procedure UpdateMenus;
    procedure InitItems(A: boolean);
    procedure DrawWndBorder(Wnd: hWnd; MenuBmp: TBitmap);
    function GetMenuInfo(Item: TMenuItem; aCanvas: TCanvas; aWnd: hwnd = 0): PacMenuInfo;
    procedure InitItem(Item: TMenuItem; A: boolean);
    procedure HookItem(MenuItem: TMenuItem; AActive: boolean);
    procedure InitMenuLine(Menu: TMainMenu; A: boolean);
    procedure HookPopupMenu(Menu: TPopupMenu; Active: boolean);
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write FAlignment;
    property BevelWidth: integer read FBevelWidth write FBevelWidth default 0;
    property BorderWidth: integer read GetBorderWidth write FBorderWidth default 3;
    property CaptionFont: TFont read GetCaptionFont write SetCaptionFont;
    property SkinBorderWidth: integer read FSkinBorderWidth write FSkinBorderWidth;
    property Margin: integer read GetMargin write FMargin default 2;
    property Spacing: integer read GetSpacing write FSpacing default 8;
    property OnDrawItem: TsMenuManagerDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;
{$ENDIF}


function GetFirstItem(Item: TMenuItem): TMenuItem;
procedure DeleteUnusedBmps(DeleteAll: boolean);
function ChildIconPresent: boolean;
{$IFNDEF FPC}
procedure ClearCache;
{$ENDIF}


var
  MDISkinProvider: TObject;
  acCanHookMenu: boolean = False;
  CustomMenuFont: TFont = nil;
  iDefIcoWidth: integer = 16;


const
  s_SysMenu = 'acSysMenu';


implementation

uses
  math, StdCtrls, ImgList,
  {$IFDEF TNTUNICODE} TntWindows, {$ENDIF}
  sDefaults, sStyleSimply, sSkinProvider, sMaskData, sSkinProps, sThirdParty, sGraphUtils, acntUtils,
  sAlphaGraph, sSkinManager, sVclUtils, sMessages, sGlyphUtils, acAlphaImageList;


const
  s_DontForget = 'Use OnGetExtraLineData event';


var
  Measuring: boolean = False;
  MenuInfoArray: array of TacMenuInfo;
  AlignToInt: array [TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);

  // Temp data
  IcoLineWidth: integer = 0;
  ExtraCaption, ExtraSection: string;
  ExtraGlyph: TBitmap;

  
function ChildIconPresent: boolean;
begin
  if MDISkinProvider <> nil then
    with TsSkinProvider(MDISkinProvider), Form do
      Result := (Form <> nil) and (FormStyle = fsMDIForm) and (ActiveMDIChild <> nil) and
                (ActiveMDIChild.WindowState = wsMaximized) and (biSystemMenu in ActiveMDIChild.BorderIcons) and
                Assigned(ActiveMDIChild.Icon)
  else
    Result := False;
end;


function GetFirstItem(Item: TMenuItem): TMenuItem;
var
  i: integer;
begin
  for i := 0 to Item.Parent.Count - 1 do
    if Item.Parent.Items[i].Visible then begin
      Result := Item.Parent.Items[i];
      Exit;
    end;

  Result := nil;
end;


procedure DeleteUnusedBmps(DeleteAll: boolean);
var
  i, l: integer;
begin
  l := Length(MenuInfoArray);
  for i := 0 to l - 1 do
    MenuInfoArray[i].Bmp.Free;

  SetLength(MenuInfoArray, 0);
end;


function Breaked(MenuItem: TMenuItem): boolean;
var
  i: integer;
begin
  Result := False;
{$IFNDEF FPC}
  for i := 1 to MenuItem.MenuIndex do
    if MenuItem.Parent.Items[i].Break <> mbNone then begin
      Result := True;
      Exit;
    end;
{$ENDIF}
end;


{$IFNDEF FPC}
constructor TsSkinableMenus.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FActive := False;
  FCaptionFont := TFont.Create;
  BorderDrawing := False;
  FBorderWidth := 3;
  FBevelWidth := 0;
  PPI := 96;
  FSpacing := 8;
  FMargin := 2;
  sd := nil;
end;


type
  TAccessMenuItem = class(TMenuItem);

procedure TsSkinableMenus.sAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var
  Wnd: hwnd;
  aMsg: TMSG;
  Item: TMenuItem;
  mi: PacMenuInfo;
  sm: TsSkinManager;
  DrawStyle: Cardinal;
  bSelected, RTL: boolean;
  Text, TabText: acString;
  ItemBmp, BGImage: TBitmap;
  ItemData: TacMenuItemData;
  DrawData: TacDrawGlyphData;
  iRect, R, gRect, cRect, clipBox: TRect;
  ExtWidth, bWidth, i, j, c, l, w, h: integer;

  function TextRect: TRect;
  begin
    Result := MkRect(ItemBmp);
    inc(Result.Left, Margin * 2 + GlyphSize(Item, True).cx + Spacing + iRect.Left);
    if RTL then
      ReflectRect(Result, WidthOf(iRect), 0, True);
  end;

  function ShortCutRect(const s: acString): TRect;
  var
    tr, R: TRect;
  begin
    Result := MkRect(ItemBmp);
    tR := MkRect(1, 0);
    acDrawText(ItemBmp.Canvas.Handle, PacChar(Text), tR, DT_EXPANDTABS or DT_SINGLELINE or DT_CALCRECT);
    R := iRect;
    dec(R.Right, 2 * TacSkinData(sd).Spacing);
    Result.Left := R.Right - WidthOf(tr);
    Result.Right := R.Right;
    if RTL then
      ReflectRect(Result, WidthOf(aRect), 0, True);
  end;

  function IsTopVisible(Item: TMenuItem): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to Item.Parent.Count - 1 do
      if Item.Parent.Items[i].Visible then
        if Item.Parent.Items[i] = Item then begin
          Result := True;
          Break;
        end;
  end;

  function IsBtmVisible(Item: TMenuItem): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to Item.Parent.Count - 1 do
      if Item.Parent.Items[Item.Parent.Count - 1 - i].Visible then
        if Item.Parent.Items[Item.Parent.Count - 1 - i] = Item then begin
          Result := True;
          Break;
        end;
  end;

  procedure DrawServiceGlyph(gIndex: integer);
  var
    gR: TRect;
    h, w: integer;
  begin
    h := GetImageHeight(acCharImages, gIndex, PPI);
    w := GetImageWidth(acCharImages, gIndex, PPI);
    gR.Top := (ItemBmp.Height - h) div 2;
    gR.Bottom := gR.Top + h;

    gR.Left := (IcoLineWidth - w) div 2 + iRect.Left;
    gR.Right := gR.Left + w;
    if RTL then
      ReflectRect(gR, WidthOf(iRect), 0, True);

    SetImagesPPI(acCharImages, PPI);
    if DrawData.CurrentState > 0 then begin
      if DrawData.CurrentState > 0 then
        SetImagesState(acCharImages, DrawData.CurrentState);

      acCharImages.Draw(ItemBmp.Canvas, gR.Left, gR.Top, gIndex, True);
      if DrawData.CurrentState > 0 then
        SetImagesState(acCharImages, 0);
    end
    else
      acCharImages.Draw(ItemBmp.Canvas, gR.Left, gR.Top, gIndex, True);
  end;

begin
  if (FOwner <> nil) and TsSkinManager(FOwner).Active then begin
    bWidth := RealBorderWidth;
    GetClipBox(ACanvas.Handle, clipBox);
    Item := TMenuItem(Sender);
    sm := TsSkinManager(FOwner);
    if (Item = Item.Parent[0]) or (sd = nil) then begin
      Form := TForm(GetOwnerForm(Item.GetParentMenu));
      if Form <> nil then
        sd := sm.CommonSkinData(GetControlPPI(Form))
      else
        sd := sm.CommonSkinData;

      PPI := TacSkinData(sd).PPI;
    end;

    if NoBorder then begin
      if clipBox.Right - ARect.Right < 10 then
        ARect.Right := clipBox.Right;

      if clipBox.Bottom - ARect.Bottom < 10 then
        ARect.Bottom := clipBox.Bottom;
    end;
    // Get RTL value from parent menu
    RTL := IsRTL(Item);
    if TempControl <> nil then begin
      if ShowHintStored then
        Application.ShowHint := AppShowHint;

      SendAMessage(TControl(TempControl), WM_MOUSELEAVE);
      TempControl := nil;
    end;
    ACanvas.Lock;
    try
      if IsNT then
        Wnd := WindowFromDC(ACanvas.Handle)
      else
        Wnd := 0;

      mi := GetMenuInfo(Item, ACanvas, Wnd);
      R := MkRect(mi.Bmp);
      BGImage := mi.Bmp; // !
{
      if IsNT and (Wnd <> 0) then begin // Draw when WM_NCPAINT is not received
        NewDC := GetWindowDC(Wnd);
        if (BGImage <> nil) and (BGImage.Canvas.Handle <> 0) then
          try

            if IsTopVisible(Item) then begin // First item
              BitBlt(NewDC, 0, 0, BGImage.Width, bWidth, BGImage.Canvas.Handle, 0, 0, SRCCOPY);
              // Left border
//              if not IsDivText(Item) then
//                BitBlt(NewDC, 0, 0, ExtraWidth(mi) * Br + max(SkinBorderWidth, bWidth), mi.Bmp.Height, BGImage.Canvas.Handle, 0, 0, SRCCOPY);
            end;
            if IsBtmVisible(Item) then // Last item
              BitBlt(NewDC, 0, BGImage.Height - BorderWidth, BGImage.Width, BorderWidth, BGImage.Canvas.Handle, 0, BGImage.Height - BorderWidth, SRCCOPY);
            // Right border
            BitBlt(NewDC, BGImage.Width - BorderWidth, aRect.Top + BorderWidth, BorderWidth, HeightOf(aRect),
                   BGImage.Canvas.Handle, BGImage.Width - BorderWidth, aRect.Top + BorderWidth, SRCCOPY);
          finally
            ReleaseDC(Wnd, NewDC);
          end;
      end;
}
      if (Wnd = 0) and (Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF} <> 0) then
        if not PeekMessage(aMsg, Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}, WM_DRAWMENUBORDER, WM_DRAWMENUBORDER2, PM_NOREMOVE) then
          PostMessage(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}, WM_DRAWMENUBORDER, 0, LParam(Item));

      if (Wnd <> 0) and (BGImage <> nil) and (Item = GetFirstItem(Item)) then
        DrawWndBorder(Wnd, BGImage);

      if Item.IsLine then begin
        PaintDivider(aCanvas, aRect, Item, BGImage, mi);
        ACanvas.UnLock;
        Exit;
      end
      else
        if IsDivText(Item) then begin
          mi := GetMenuInfo(Item, {WidthOf(R), HeightOf(R), }ACanvas, Wnd);
          PaintCaption(aCanvas, aRect, Item, BGImage, mi);
          ACanvas.UnLock;
          Exit;
        end;

      it := smNormal;
      if BGImage = nil then begin
        ACanvas.UnLock;
        Exit;
      end;

      i := sm.CommonSkinData.MenuItem.SkinIndex;
      // Check for multi-columned menus...
      if Item.MenuIndex < Item.Parent.Count - 1 then begin
{$IFNDEF FPC}
        if not mi.ItemsIterated and (Item.Break = mbBarBreak{mbNone}) then begin // Paint vert divider as a line
          if i >= 0 then
            C := TacSkinData(sd).gd[i].Props[0].FontColor.Color
          else
            C := clGray;

          FillDC(BGImage.Canvas.Handle, Rect(aRect.Left, 0, aRect.Left + 1, BGImage.Height), C);
        end;
        if Item.Parent.Items[Item.MenuIndex + 1].Break <> mbNone then // If item is bottom in column
          BitBlt(ACanvas.Handle, aRect.Left, aRect.Bottom, WidthOf(aRect), BGImage.Height - 2 * bWidth - aRect.Bottom, BGImage.Canvas.Handle, aRect.Left + bWidth, aRect.Bottom + bWidth, SrcCopy);
{$ENDIF}
      end
      else begin
        if aRect.Bottom < BGImage.Height - 2 * bWidth then // Last item
          BitBlt(ACanvas.Handle, aRect.Left, aRect.Bottom, WidthOf(aRect), BGImage.Height - 2 * bWidth - aRect.Bottom, BGImage.Canvas.Handle, aRect.Left + bWidth, aRect.Bottom + bWidth, SrcCopy);

        mi.ItemsIterated := True;
      end;
{$IFNDEF FPC}
//      if Item.Break <> mbNone then //
//        BitBlt(ACanvas.Handle, aRect.Left - 4, aRect.Top, 4, BGImage.Height - 6, BGImage.Canvas.Handle, aRect.Left - 1, aRect.Top + bWidth, SrcCopy);
{$ENDIF}
      ExtWidth := ExtraWidth(mi);
      ItemBmp := CreateBmp32(aRect);
      iRect := MkRect(ItemBmp);
      if ExtWidth > 0  then
        if RTL then begin
          if BGImage.Width - aRect.Right <= 2 * bWidth then
            dec(iRect.Right, ExtWidth)
        end
        else
          if aRect.Left <= bWidth then
            inc(iRect.Left, ExtWidth);

      // Copy BG
      BitBlt(ItemBmp.Canvas.Handle, 0, 0, ItemBmp.Width, ItemBmp.Height, BGImage.Canvas.Handle, aRect.Left + bWidth{ + iRect.Left}, aRect.Top + bWidth, SRCCOPY);
      bSelected := Item.Enabled and (odSelected in State);
      // Draw MenuItem
      if sm.IsValidSkinIndex(i) and (bSelected or (TacSkinData(sd).gd[i].Props[0].Transparency < 100)) then
        PaintItem(i, MakeCacheInfo(BGImage, bWidth, bWidth), True, integer(bSelected), iRect, Point(aRect.Left + iRect.Left, aRect.Top), ItemBmp.Canvas.Handle, TsSkinManager(FOwner).CommonSkinData);

      // Data for a glyph drawing
      DrawData.CurrentState := integer(Item.Enabled and (odSelected in State));
      ItemBmp.Canvas.Font.Color := TacSkinData(sd).gd[i].Props[DrawData.CurrentState].FontColor.Color;
      // Check/radio glyph drawing
      if odChecked in State then
        if Item.Bitmap.Empty and ((Item.GetImageList = nil) or (Item.ImageIndex < 0)) then begin
          if Item.RadioItem then begin
            if TsSkinManager(FOwner).ButtonsOptions.OldGlyphsMode then
              j := sm.GetMaskIndex(sm.SkinCommonInfo.IndexGLobalInfo, s_RadioButtonChecked, TacSkinData(sd))
            else
              j := -1;

            if j < 0 then
              DrawServiceGlyph(5);
          end
          else begin
            j := sm.GetMaskIndex(sm.SkinCommonInfo.IndexGLobalInfo, s_CheckGlyph, TacSkinData(sd));
            if j < 0 then
              DrawServiceGlyph(6);
          end;
          if j >= 0 then begin
            w := TacSkinData(sd).ma[j].Width;
            h := TacSkinData(sd).ma[j].Height;
            cRect.Top := (HeightOf(aRect) - h) div 2;
            cRect.Left := max(0, (IcoLineWidth{GlyphSizeCX} - w) div 2{ + Margin}) + iRect.Left;
            cRect.Right := cRect.Left + w;
            cRect.Bottom := cRect.Top + h;
            if RTL then
              ReflectRect(cRect, WidthOf(iRect), 0, True);

            DrawSkinGlyph(ItemBmp, cRect.TopLeft, integer(Item.Enabled and (odSelected in State)), 1, TacSkinData(sd).ma[j], MakeCacheInfo(ItemBmp))
          end;
        end;

      // Glyph drawing
      if not Item.Bitmap.Empty or (Item.GetImageList <> nil) and IsValidIndex(Item.ImageIndex, GetImageCount(Item.GetImageList)) then begin
        if not Item.Bitmap.Empty then begin
          w := Item.Bitmap.Width;
          h := Item.Bitmap.Height;
          DrawData.Images := nil;
          DrawData.Glyph := Item.Bitmap;
          gRect.Left := (GlyphSize(Item).cx + Margin + Spacing - w) div 2 + iRect.Left;// - BorderWidth;
        end
        else begin
          w := GetImageWidth (Item.GetImageList, Item.ImageIndex, PPI);
          h := GetImageHeight(Item.GetImageList, Item.ImageIndex, PPI);
          DrawData.ImageIndex := Item.ImageIndex;
          DrawData.Images := Item.GetImageList;
          DrawData.Glyph := nil;
          gRect.Left := (IcoLineWidth - w) div 2 + iRect.Left;// - BorderWidth;
        end;
        gRect.Top := (ItemBmp.Height - h) div 2;
        gRect.Bottom := gRect.Top + h;
        gRect.Right := gRect.Left + w;
        if RTL then
          ReflectRect(gRect, WidthOf(iRect), 0, True);

        DrawData.ImgRect := gRect;
        DrawData.CommonSkinData := TacSkinData(sd);

        DrawData.DisabledGlyphKind := [];
        DrawData.Enabled := Item.Enabled;
        DrawData.Blend := 0;
        DrawData.Down := False;

        if odChecked in State then begin
          j := TsSkinManager(FOwner).SkinCommonInfo.Sections[ssSpeedButton_Small];
          if j >= 0 then begin
            InflateRect(gRect, 1, 1);
            PaintItem(j, MakeCacheInfo(ItemBmp, bWidth, bWidth), True, 2, gRect, MkPoint, ItemBmp, TsSkinManager(FOwner).CommonSkinData);
            InflateRect(gRect, -1, -1);
          end;
        end;
        DrawData.Reflected := False;
        DrawData.Grayed := (dgGrayed in DefDisabledGlyphKind) and not Item.Enabled or DrawData.CommonSkinData.FOwner.Effects.DiscoloredGlyphs and not (odSelected in State);
        DrawData.DstBmp := ItemBmp;
        DrawData.PPI := PPI;
        DrawData.Canvas := ItemBmp.Canvas;
        DrawData.BGColor := DrawData.CommonSkinData.gd[i].Props[DrawData.CurrentState].Color;
        DrawData.NumGlyphs := 1;
        acDrawGlyphEx(DrawData);
      end
      else
        if (Item.GetParentMenu <> nil) and (pos(s_SysMenu, Item.GetParentMenu.Name) = 1) then
          case Item.Tag of
            SC_MINIMIZE: DrawServiceGlyph(iServCharMin);
            SC_MAXIMIZE: DrawServiceGlyph(iServCharMax);
            SC_RESTORE:  DrawServiceGlyph(iServCharRest);
            SC_CLOSE:    DrawServiceGlyph(iServCharClose);
            SC_MOVE:     DrawServiceGlyph(7);
          end;

      UpdateFont(ItemBmp.Canvas, Item, odDefault in State, False); // False
      // Text writing
      ItemData.Font := ItemBmp.Canvas.Font;
      if Assigned(TsSkinManager(FOwner).OnGetPopupItemData) then begin
        c := ItemBmp.Canvas.Font.Color;
        TsSkinManager(FOwner).OnGetPopupItemData(Item, State, ItemData);
        SelectObject(ItemBmp.Canvas.Handle, ItemBmp.Canvas.Font.Handle);
        if ItemBmp.Canvas.Font.Color <> c then
          i := -1;
      end;
      R := TextRect;
{$IFDEF TNTUNICODE}
      if Sender is TTntMenuItem then
        Text := TTntMenuItem(Sender).Caption
      else
{$ENDIF}
        Text := Item.Caption;

      c := pos(#9, Text);
      DrawStyle := 0;
      if c <> 0 then begin
        l := Length(Text) - c;
        TabText := copy(Text, c + 1, l);
        Delete(Text, c, l + 1);
      end
      else
        if (Text <> '') and (Text[1] = #8) then begin
          Delete(Text, 1, 1);
          Text := Text + '      ';
          DrawStyle := AlignToInt[taRightJustify];
        end
        else
          DrawStyle := AlignToInt[Alignment];

      DrawStyle := DrawStyle or DT_EXPANDTABS or DT_VCENTER or DT_NOCLIP or Cardinal(iff(pos(#13, Text) > 0, DT_WORDBREAK, DT_SINGLELINE));
      if RTL then
        DrawStyle := DrawStyle or DT_RIGHT or DT_RTLREADING;

      if (odNoAccel in State) and not sm.MenuSupport.ShowAccelCharAlways then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;

      InflateRect(R, 0, -Margin);
      sGraphUtils.acWriteTextEx(ItemBmp.Canvas, PacChar(Text), True, R, DrawStyle, i, Item.Enabled and ((odSelected in State) or (odHotLight in State)), FOwner);
      Text := ShortCutToText(TMenuItem(Sender).ShortCut);
      if Text <> '' then begin
        DrawStyle := DT_SINGLELINE or DT_VCENTER or DT_LEFT;
        R := ShortCutRect(Text);
        sGraphUtils.acWriteTextEx(ItemBmp.Canvas, PacChar(Text), True, R, DrawStyle, i, Item.Enabled and ((odSelected in State) or (odHotLight in State)), FOwner);
      end;
      if TabText <> '' then begin
        DrawStyle := DT_SINGLELINE or DT_VCENTER or DT_RIGHT;
        dec(R.Right, 8);
        sGraphUtils.acWriteTextEx(ItemBmp.Canvas, PacChar(TabText), True, R, DrawStyle, i, Item.Enabled and ((odSelected in State) or (odHotLight in State)), FOwner);
      end;
      if Assigned(FOnDrawItem) then
        FOnDrawItem(Item, ItemBmp.Canvas, MkRect(ItemBmp), State, it);

      if not Item.Enabled then begin
        R := aRect;
        OffsetRect(R, bWidth, bWidth);
        BlendTransRectangle(ItemBmp, 0, 0, BGImage, R, DefBlendDisabled);
      end;
      BitBlt(ACanvas.Handle, aRect.Left, aRect.Top, ItemBmp.Width, ItemBmp.Height, ItemBmp.Canvas.Handle, 0, 0, SrcCopy);
      FreeAndNil(ItemBmp);
    finally
      if Assigned(Item.OnDrawItem) then
        Item.OnDrawItem(Item, ACanvas, ARect, odSelected in State);

      ACanvas.UnLock;
    end;
  end;
end;


procedure TsSkinableMenus.InitItems(A: boolean);
var
  i: integer;

  procedure ProcessComponent(c: TComponent);
  var
    i: integer;
  begin
    if c <> nil then begin
      if c is TMainMenu then begin
        InitMenuLine(TMainMenu(c), A);
        for i := 0 to TMainMenu(c).Items.Count - 1 do
          HookItem(TMainMenu(c).Items[i], A and TsSkinManager(FOwner).SkinnedPopups);
      end
      else
        if c is TPopupMenu then
          HookPopupMenu(TPopupMenu(c), A and TsSkinManager(FOwner).SkinnedPopups)
        else
          if (c is TMenuItem) and not (TMenuItem(c).GetParentMenu is TMainMenu) then
            HookItem(TMenuItem(c), A and TsSkinManager(FOwner).SkinnedPopups);

      for i := 0 to c.ComponentCount - 1 do
        ProcessComponent(c.Components[i]);
    end;
  end;

begin
  FActive := A;
  if not (csDesigning in Fowner.ComponentState) then
    for i := 0 to Application.ComponentCount - 1 do
      ProcessComponent(Application.Components[i]);
end;


procedure TsSkinableMenus.HookItem(MenuItem: TMenuItem; AActive: boolean);
var
  i: integer;

  procedure HookSubItems(Item: TMenuItem);
  var
    i: integer;
  begin
    for i := 0 to Item.Count - 1 do begin
      if AActive then begin
        if not IsTopLine(Item.Items[i]) then begin
          if not Assigned(Item.Items[i].OnAdvancedDrawItem) or acIgnoreMenuOwnerDraw then
            Item.Items[i].OnAdvancedDrawItem := sAdvancedDrawItem;

          if not Assigned(Item.Items[i].OnMeasureItem) or acIgnoreMenuOwnerDraw then
            Item.Items[i].OnMeasureItem := sMeasureItem;
        end;
      end
      else begin
        if addr(Item.Items[i].OnAdvancedDrawItem) = addr(TsSkinableMenus.sAdvancedDrawItem) then
          Item.Items[i].OnAdvancedDrawItem := nil;

        if addr(Item.Items[i].OnMeasureItem) = addr(TsSkinableMenus.sMeasureItem) then
          Item.Items[i].OnMeasureItem := nil;
      end;
      HookSubItems(Item.Items[i]);
    end;
  end;

begin
  for i := 0 to MenuItem.Count - 1 do begin
    if AActive then begin
      if not IsTopLine(MenuItem.Items[i]) then begin
        if not Assigned(MenuItem.Items[i].OnAdvancedDrawItem) or acIgnoreMenuOwnerDraw then
          MenuItem.Items[i].OnAdvancedDrawItem := sAdvancedDrawItem;

        if not Assigned(MenuItem.Items[i].OnMeasureItem) or acIgnoreMenuOwnerDraw then
          MenuItem.Items[i].OnMeasureItem := sMeasureItem;
      end;
    end
    else begin
      if addr(MenuItem.Items[i].OnAdvancedDrawItem) = addr(TsSkinableMenus.sAdvancedDrawItem) then
        MenuItem.Items[i].OnAdvancedDrawItem := nil;

      if addr(MenuItem.Items[i].OnMeasureItem) = addr(TsSkinableMenus.sMeasureItem) then
        MenuItem.Items[i].OnMeasureItem := nil;
    end;
    HookSubItems(MenuItem.Items[i]);
  end;
end;


procedure TsSkinableMenus.SetActive(const Value: boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    InitItems(Value);
  end
end;


procedure TsSkinableMenus.sMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  R: TRect;
  i: integer;
  Text: acString;
  Item: TMenuItem;
  mi: PacMenuInfo;
  ItemData: TacMenuItemData;
begin
  if not (csDestroying in TComponent(Sender).ComponentState) then begin
    acCanHookMenu := True;
    Item := TMenuItem(Sender);
    if (Item = Item.Parent[0]) or (sd = nil) then begin
      Form := TForm(GetOwnerForm(Item.GetParentMenu));
      if Form <> nil then
        sd := TsSkinManager(FOwner).CommonSkinData(GetControlPPI(Form))
      else
        sd := TsSkinManager(FOwner).CommonSkinData;

      PPI := TacSkinData(sd).PPI;
    end;
    mi := GetMenuInfo(Item, nil);
    if Item.Caption = cLineCaption then
      it := smDivider
    else
      if IsdivText(Item) then
        it := smCaption
      else
        it := smNormal;

    if (mi <> nil) and (mi.FirstItem = nil) then begin // If not defined still
      mi.FirstItem := Item.Parent.Items[0];
      if not Measuring and not (csDesigning in TsSkinManager(FOwner).ComponentState) then begin
//        if mi.FirstItem.Name <> s_SkinSelectItemName then begin
        Measuring := True;
        if ExtraGlyph <> nil then
          FreeAndNil(ExtraGlyph);

        mi.HaveExtraLine := True;
        if Assigned(TsSkinManager(FOwner).OnGetMenuExtraLineData) then begin
          ExtraSection := s_MenuExtraLine;
          ExtraCaption := s_DontForget;
          TsSkinManager(FOwner).OnGetMenuExtraLineData(mi.FirstItem, ExtraSection, ExtraCaption, ExtraGlyph, mi.HaveExtraLine);
          ExtraCaption := DelChars(ExtraCaption, '&');
        end;
        if not mi.HaveExtraLine and Assigned(ExtraGlyph) then
          FreeAndNil(ExtraGlyph);

        Measuring := False;
//        end
//        else
//          mi.HaveExtraLine := False;
      end;
    end;
    UpdateFont(ACanvas, Item, Item.Default, False);//, False);
    case it of
      smDivider:
        R := MkRect(1, 0);

      smCaption: begin
{$IFDEF TNTUNICODE}
        if Item is TTntMenuItem then
          Text := TTntMenuItem(Item).Caption
        else
{$ENDIF}
          Text := Item.Caption;

        R := MkRect(acTextWidth(ACanvas, Text), 0);
      end

      else begin
{$IFDEF TNTUNICODE}
        if Item is TTntMenuItem then
          Text := TTntMenuItem(Item).Caption + iff(ShortCutToText(TTntMenuItem(Item).ShortCut) = '', '', ShortCutToText(TTntMenuItem(Item).ShortCut))
        else
{$ENDIF}
          Text := Item.Caption + iff(ShortCutToText(Item.ShortCut) = '', '', ShortCutToText(Item.ShortCut));

        R := MkRect(800, 0);
      end;
    end;

    ItemData.Font := ACanvas.Font;
    if Assigned(TsSkinManager(FOwner).OnGetPopupItemData) then begin
      TsSkinManager(FOwner).OnGetPopupItemData(Item, [], ItemData);
      SelectObject(ACanvas.Handle, ACanvas.Font.Handle);
    end;

    if Text = '' then
      Width := Margin * 3 + GlyphSize(Item).cx * 2 + Spacing
    else begin
{$IFDEF TNTUNICODE}
      if Sender is TTntMenuItem then
        Tnt_DrawTextW(ACanvas.Handle, PacChar(Text), Length(Text), R, DT_EXPANDTABS or DT_NOCLIP or DT_CALCRECT or DT_WORDBREAK)
      else
{$ENDIF}
        acDrawText(ACanvas.Handle, PacChar(Text), R, DT_EXPANDTABS or DT_NOCLIP or DT_CALCRECT or DT_WORDBREAK);

      Width := Margin * 3 + WidthOf(R) + GlyphSize(Item).cx * 2 + Spacing;

      if NoBorder then
        dec(Width, 2 * BorderWidth);
    end;
    if mi.HaveExtraLine and (not Breaked(Item) or (Item.Parent.Items[0] = Item)) then
      inc(Width, ExtraWidth(mi));

    Height := GetItemHeight(aCanvas, Item);

    for i := 0 to Item.Count - 1 do // Remeasure subitems
      TAccessMenuItem(Item.Items[i]).MenuChanged(True);
  end;
end;


destructor TsSkinableMenus.Destroy;
begin
  FOwner := nil;
  FreeAndNil(FCaptionFont);
  inherited Destroy;
end;


procedure TsSkinableMenus.UpdateFont(aCanvas: TCanvas; aItem: TMenuItem; aBold: boolean; aCaption: boolean = False);
var
  bCanScale: boolean;
  f: TacAccessControl;
begin
  aCanvas.Font.PixelsPerInch := 96;
  bCanScale := True;
  if Assigned(CustomMenuFont) then
    aCanvas.Font.Assign(CustomMenuFont)
  else
    if aCaption then
      aCanvas.Font.Assign(CaptionFont)
    else
      if Assigned(Screen.MenuFont) then begin
        aCanvas.Font.Assign(Screen.MenuFont);
        bCanScale := True;
      end;

  f := TacAccessControl(GetOwnerForm(aItem.GetParentMenu));
  if f = nil then
    f := TacAccessControl(Application.MainForm);

  if f <> nil then begin
    if not Assigned(CustomMenuFont) then
      aCanvas.Font.Charset := f.Font.Charset;

    if bCanScale then
      aCanvas.Font.Height := aCanvas.Font.Height * CurrentPPI(TsSkinManager(FOwner), f) div 96;
  end;
  if aBold then
    aCanvas.Font.Style := [fsBold];
end;


// Refresh list of all MenuItems in the program
procedure TsSkinableMenus.UpdateMenus;
begin
  SetActive(TsSkinManager(FOwner).IsActive);
end;


// Return height of the menu panel
function TsSkinableMenus.ParentHeight(aCanvas: TCanvas; Item: TMenuItem): integer;
var
  i, ret: integer;
begin
  ret := 0;
  Result := 0;
  for i := 0 to Item.Parent.Count - 1 do
    if Item.Parent.Items[i].Visible then
      if Item.Parent.Items[i].Break <> mbNone then begin
        Result := max(Result, ret);
        ret := GetItemHeight(aCanvas, Item.Parent.Items[i]);
      end
      else
        inc(ret, GetItemHeight(aCanvas, Item.Parent.Items[i]));

  Result := max(Result, ret);
end;


// Return height of the current MenuItem
function TsSkinableMenus.GetBorderWidth: integer;
begin
  if sd is TacSkinData then
    Result := TacSkinData(sd).ScaleIntValue(FBorderWidth)
  else
    Result := FBorderWidth;
end;


function TsSkinableMenus.GetCaptionFont: TFont;
begin
  if Assigned(CustomMenuFont) then
    Result := CustomMenuFont
  else
    if Assigned(Screen.MenuFont) then
      Result := Screen.MenuFont
    else
      Result := nil;
end;


function TsSkinableMenus.GetItemHeight(aCanvas: TCanvas; Item: TMenuItem): integer;
var
  R: TRect;
  Text: acString;
  IsDivider: boolean;
begin
  IsDivider  := Item.Caption = cLineCaption;
  if not IsDivider then begin
{$IFDEF TNTUNICODE}
    if Item is TTntMenuItem then
      Text := TTntMenuItem(Item).Caption
    else
{$ENDIF}
      Text := Item.Caption;

    if not IsDivText(Item) then
      Text := Text + iff(ShortCutToText(Item.ShortCut) = '', '', ShortCutToText(Item.ShortCut));
  end;
  if IsDivider then
    Result := 2
  else begin
    R := MkRect(800, 0);
    UpdateFont(ACanvas, Item, Item.Default, False);//, False);
    acDrawText(ACanvas.Handle, PacChar(Text), R, DT_EXPANDTABS or DT_NOCLIP or DT_CALCRECT or DT_WORDBREAK);
    if IsDivText(Item) then
      Result := HeightOf(R) + 2 * (Margin + 1) + 1
    else
      Result := Maxi(HeightOf(R), GlyphSize(Item).cy) + 2 * (Margin + 1);
  end;
end;


function TsSkinableMenus.IsDivText(Item: TMenuItem): boolean;
begin
  Result := (copy(Item.Caption, 1, 1) = cMenuCaption) and (copy(Item.Caption, length(Item.Caption), 1) = cMenuCaption);
end;


function TsSkinableMenus.IsTopLine(Item: TMenuItem): boolean;
var
  i: integer;
  m: TMenu;
begin
  Result := False;
  m := Item.GetParentMenu;
  if m is TMainMenu then
    for i := 0 to m.Items.Count - 1 do
      if m.Items[i] = Item then begin
        Result := True;
        Exit;
      end;
end;


function TsSkinableMenus.CursorMarginH: integer;
begin
  Result := RealBorderWidth;
end;


function TsSkinableMenus.CursorMarginV: integer;
begin
  Result := 0;
end;


function TsSkinableMenus.ItemRect(Item: TMenuItem; aRect: TRect): TRect;
begin
  Result := aRect;
  if Item.Parent.Items[0] = Item then
    Result.Top := Result.Top + CursorMarginV;

  if Item.Parent.Items[Item.Parent.Count - 1] = Item then
    Result.Bottom := Result.Bottom - CursorMarginV;

  Result.Left := Result.Left + CursorMarginH;
  Result.Right := Result.Right - CursorMarginH;
end;


procedure TsSkinableMenus.PaintDivider(aCanvas: TCanvas; aRect: TRect; Item: TMenuItem; MenuBmp: TBitmap; const mi: PacMenuInfo);
var
  Form: TForm;
  nRect: TRect;
  CI: TCacheInfo;
  sd: TacSkinData;
  TempBmp: TBitmap;
  sm: TsSkinManager;
  SkinIndex, IcoLineNdx, ExtWidth, OffsLeft, OffsRight, IcoWidth: integer;
begin
  sm := TsSkinManager(FOwner);
  Form := TForm(GetOwnerForm(Item.GetParentMenu));
  if Form <> nil then
    sd := sm.CommonSkinData(GetControlPPI(Form))
  else
    sd := sm.CommonSkinData;

  with sd do begin
    nRect := aRect;
    ExtWidth := ExtraWidth(mi);
    SkinIndex := sm.SkinCommonInfo.Sections[ssDividerV];

    IcoLineNdx := sm.GetSkinIndex(sm.MenuSupport.IcoLineSkin);
    if (IcoLineNdx >= 0) and ((gd[IcoLineNdx].Props[0].Transparency < 100) or (gd[IcoLineNdx].BorderIndex >= 0)) then
      IcoWidth := IcoLineWidth + TacSkinData(sd).Spacing
    else
      IcoWidth := 0;

    if IsRTL(Item) then begin
      OffsLeft  := 0;
      OffsRight := ExtWidth + IcoWidth;
    end
    else begin
      OffsRight := 0;
      OffsLeft  := ExtWidth + IcoWidth;
    end;

    OffsetRect(nRect, -nRect.Left + Margin + OffsLeft, -nRect.Top);
    dec(nRect.Right, Margin + OffsRight);

    if (sm <> nil) and sm.IsActive and (SkinIndex >= 0) and (gd[SkinIndex].BorderIndex >= 0) then begin
      TempBmp := CreateBmp32(aRect);
      if MenuBmp <> nil then
        BitBlt(TempBmp.Canvas.Handle, 0, 0, WidthOf(aRect), HeightOf(aRect), MenuBmp.Canvas.Handle, aRect.Left + RealBorderWidth, aRect.Top + RealBorderWidth, SRCCOPY);

      CI := MakeCacheInfo(TempBmp);
      DrawSkinRect(TempBmp, nRect, CI, ma[gd[SkinIndex].BorderIndex], 0, True);
      BitBlt(aCanvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), TempBmp.Canvas.Handle, 0, 0, SRCCOPY);
      FreeAndnil(TempBmp);
    end
    else begin
      BitBlt(aCanvas.Handle, aRect.Left, aRect.Top, WidthOf(aRect), HeightOf(aRect), MenuBmp.Canvas.Handle, aRect.Left + RealBorderWidth, aRect.Top + RealBorderWidth, SRCCOPY);
      aCanvas.Pen.Color := iff(sm.Palette[pcBorder] <> clFuchsia, sm.Palette[pcBorder], clBtnShadow);
      aCanvas.Pen.Style := psSolid;
      acPaintLine(aCanvas.Handle, nRect.Left, aRect.Top, nRect.Right, aRect.Top);
    end;
  end;
end;


procedure TsSkinableMenus.PaintCaption(aCanvas: TCanvas; aRect: TRect; Item: TMenuItem; BG: TBitmap; const mi: PacMenuInfo);
var
  R: TRect;
  s: acString;
  RTL: boolean;
  ItemBmp: TBitmap;
  i, Flags, Offset, ExtWidth: integer;
begin
  ItemBmp := CreateBmp32(aRect);
  Offset := RealBorderWidth;
  BitBlt(ItemBmp.Canvas.Handle, 0, 0, ItemBmp.Width, ItemBmp.Height, BG.Canvas.Handle, aRect.Left + Offset, aRect.Top + Offset, SRCCOPY);
  ExtWidth := ExtraWidth(mi);
  RTL := IsRTL(Item);
  R := Rect(0, 0, ItemBmp.Width - ExtWidth, ItemBmp.Height);
  if not RTL and (ExtWidth > 0) then
    OffsetRect(R, ExtWidth, 0);

  i := TsSkinManager(FOwner).SkinCommonInfo.Sections[ssMenuCaption];
  if TsSkinManager(FOwner).IsValidSkinIndex(i) then
    PaintItem(i, MakeCacheInfo(BG, 1 + RealBorderWidth, 1 + RealBorderWidth), True, 0, R, Point(aRect.Left + R.Left, aRect.Top), ItemBmp, TsSkinManager(FOwner).CommonSkinData);

  // Text writing
  UpdateFont(ItemBmp.Canvas, Item, False, True);//, False);
  s := Copy(Item.Caption, 2, Length(Item.Caption) - 2);
  Flags := DT_WORDBREAK or DT_VCENTER or DT_CENTER or DT_HIDEPREFIX;
  InflateRect(R, 0, -Margin -2);
  SelectObject(ItemBmp.Canvas.Handle, ItemBmp.Canvas.Font.Handle);
  acWriteTextEx(ItemBmp.Canvas, PacChar(s), True, R, Flags, i, False);
  BitBlt(ACanvas.Handle, aRect.Left, aRect.Top, ItemBmp.Width, ItemBmp.Height, ItemBmp.Canvas.Handle, 0, 0, SrcCopy);
  FreeAndNil(ItemBmp);
end;


procedure TsSkinableMenus.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;


type
  TAccessProvider = class(TsSkinProvider);


procedure TsSkinableMenus.sAdvancedDrawLineItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var
{$IFDEF TNTUNICODE}
  ws: WideString;
{$ENDIF}
  Bmp: TBitmap;
  ci: TCacheInfo;
  R, gRect: TRect;
  Item: TMenuItem;
  Flags: cardinal;
  i, l, t: integer;
  sp: TAccessProvider;
  ItemSelected: boolean;
  Images: TCustomImageList;
  DrawData: TacDrawGlyphData;

  function TextRect: TRect;
  begin
    Result := aRect;
    inc(Result.Left, Margin);
    dec(Result.Right, Margin);
  end;

  function ShortCutRect: TRect;
  begin
    Result := aRect;
    Result.Left := WidthOf(TextRect);
  end;

begin
  if (Self = nil) or (FOwner = nil) then
    inherited
  else begin
    Item := TMenuItem(Sender);
    sp := TAccessProvider(MenuProvider);
    if sp = nil then
      try
        sp := TAccessProvider(SendAMessage(Item.GetParentMenu.WindowHandle, AC_GETPROVIDER));
      except
      end;

    if (sp = nil) and (MDISkinProvider <> nil) then
      sp := TAccessProvider(MDISkinProvider);

    if sp = nil then
      inherited
    else
      if sp.SkinData.FCacheBmp <> nil then begin
        // Calc rectangle for painting and defining a Canvas
        Bmp := CreateBmp32(aRect);
        gRect := MkRect(Bmp);
        // BG for menu item
        CI := MakeCacheInfo(sp.MenuLineBmp);
        // Calc real offset to menu item in cache
        if ACanvas = sp.SkinData.FCacheBmp.Canvas then begin // If paint in form cache
          if sp.BorderForm <> nil then
            t := aRect.Top - sp.CaptionHeight(True) - sp.ShadowSize.Top
          else
            if GetWindowLong(sp.Form.Handle, GWL_STYLE) and WS_CAPTION <> 0 then
              t := aRect.Top - SysCaptHeight(sp) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False)
            else
              t := 0;

          l := aRect.Left - sp.ShadowSize.Left - SysBorderWidth(sp.Form.Handle, sp.BorderForm);
        end
        else begin // If procedure was called by system (DRAWITEM)
          if sp.BorderForm <> nil then
            t := aRect.Top - sp.CaptionHeight(True) + DiffTitle(sp.BorderForm) + integer(sp.FSysExHeight) * 4
          else
            if GetWindowLong(sp.Form.Handle, GWL_STYLE) and WS_CAPTION <> 0 then
              t := aRect.Top - SysCaptHeight(sp) + DiffTitle(sp.BorderForm) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False)
            else
              t := 0;

          l := aRect.Left - SysBorderWidth(sp.Form.Handle, sp.BorderForm) + DiffBorder(sp.BorderForm);
        end;
        // Skin index for menu item
        i := TsSkinManager(FOwner).CommonSkinData.MenuItem.SkinIndex;
        ItemSelected := Item.Enabled and ((odSelected in State) or (odHotLight in State));
        if TsSkinManager(FOwner).IsValidSkinIndex(i) then
          PaintItem(i, ci, True, integer(ItemSelected), gRect, Point(l, t), Bmp, TsSkinManager(FOwner).CommonSkinData);

        gRect.Left := 0;
        gRect.Right := 0;
        Images := nil;
        if not Item.Bitmap.Empty then begin
          gRect.Top := (HeightOf(ARect) - GlyphSize(Item).cy) div 2;
          if SysLocale.MiddleEast and (Item.GetParentMenu.BiDiMode = bdRightToLeft) then
            gRect.Left := Bmp.Width - 3 - Item.Bitmap.Width
          else
            gRect.Left := 3;

          gRect.Right := gRect.Left + Item.Bitmap.Width + 3;
          if not Item.Enabled then
            OffsetRect(gRect, -gRect.Left + 3, -gRect.Top + 1);
        
          Bmp.Canvas.Draw(gRect.Left, gRect.Top, Item.Bitmap);
        end
        else begin
          Images := Item.GetImageList;
          if (Item.GetImageList <> nil) and (Item.ImageIndex >= 0) then begin
            gRect.Top := (HeightOf(ARect) - Item.GetImageList.Height) div 2;

            if SysLocale.MiddleEast and (Item.GetParentMenu.BiDiMode = bdRightToLeft) then
              gRect.Left := Bmp.Width - 3 - Item.GetImageList.Width
            else
              gRect.Left := 3;

            gRect.Right := gRect.Left + Item.GetImageList.Width + 3;
          end
          else
            gRect.Right := gRect.Left;
        end;
        if gRect.Right <> gRect.Left then begin
          DrawData.Blend := 0;
          DrawData.SkinIndex := i;
          DrawData.CommonSkinData := sp.SkinData.CommonSkinData;
          if (Item.Bitmap <> nil) and (Item.Bitmap.Height <> 0) then
            DrawData.NumGlyphs := max(1, Item.Bitmap.Width div Item.Bitmap.Height)
          else
            if Images <> nil then
              DrawData.NumGlyphs := max(1, Images.Width div Images.Height);

          DrawData.ImageIndex := Item.ImageIndex;
          DrawData.CurrentState := integer(ItemSelected);
          DrawData.Down := False;
          DrawData.Grayed := DrawData.CommonSkinData.FOwner.Effects.DiscoloredGlyphs;
          DrawData.Enabled := Item.Enabled;
          DrawData.Reflected := False;
          DrawData.Glyph := nil;
          DrawData.DstBmp := Bmp;
          DrawData.ImgRect := gRect;
          DrawData.Canvas := Bmp.Canvas;
          DrawData.PPI := PPI;
          if DrawData.CommonSkinData.FOwner.IsValidSkinIndex(DrawData.ImageIndex) then
            DrawData.BGColor := DrawData.CommonSkinData.gd[DrawData.ImageIndex].Props[0].Color
          else
            DrawData.BGColor := $FFFFFF;

          DrawData.Images := Images;
          DrawData.Glyph := Item.Bitmap;
          DrawData.DisabledGlyphKind := [dgBlended];
          acDrawGlyphEx(DrawData);
        end;

        // Text writing
        UpdateFont(Bmp.Canvas, Item, odDefault in State, False);//, True);
        R := TextRect;
        if SysLocale.MiddleEast and (Item.GetParentMenu.BiDiMode = bdRightToLeft) then
          R.Left := R.Left - WidthOf(gRect)
        else
          R.Left := R.Left + WidthOf(gRect);

        if Bmp <> nil then
          OffsetRect(R, -TextRect.Left + 2, -R.Top);

        i := TsSkinManager(FOwner).SkinCommonInfo.Sections[ssMenuLine];
        Flags := DT_CENTER or DT_EXPANDTABS or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP;
        if sp.Form.UseRightToLeftReading then
          Flags := Flags or DT_RTLREADING;

        if (odNoAccel in State) and not TsSkinManager(FOwner).MenuSupport.ShowAccelCharAlways then
          Flags := Flags + DT_HIDEPREFIX;
{$IFDEF TNTUNICODE}
        if Sender is TTntMenuItem then begin
          ws := WideString(TTntMenuItem(Sender).Caption);
          sGraphUtils.WriteTextExW(Bmp.Canvas, PWideChar(ws), True, R, Flags or AlignToInt[Alignment], i, ItemSelected, FOwner);
        end
        else
{$ENDIF}
          if sp.BorderForm <> nil then
{$IFDEF TNTUNICODE}
            sGraphUtils.WriteTextEx(Bmp.Canvas, PChar(Item.Caption), True, R, Flags or AlignToInt[Alignment], i, ItemSelected, FOwner)
{$ELSE}
            WriteText32(Bmp, PacChar(Item.Caption), True, R, Flags or AlignToInt[Alignment], i, integer(ItemSelected), TsSkinManager(FOwner){$IFDEF TNTUNICODE}, True{$ENDIF})
{$ENDIF}
          else
            sGraphUtils.WriteTextEx(Bmp.Canvas, PChar(Item.Caption), True, R, Flags or AlignToInt[Alignment], i, ItemSelected, FOwner);

        if Assigned(FOnDrawItem) then
          FOnDrawItem(Item, Bmp.Canvas, gRect, State, smTopLine);

        if Assigned(Bmp) then begin
          if not Item.Enabled then begin
            R := Rect(l, t, l + Bmp.Width, t + Bmp.Width);
            SumBmpRect(Bmp, sp.MenuLineBmp, DefBlendDisabled, R, MkPoint);
          end;
          BitBlt(ACanvas.Handle, aRect.Left, aRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
          FreeAndNil(Bmp);
        end;
      end;
  end;
end;


procedure TsSkinableMenus.sMeasureLineItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  W: integer;
  Menu: TMenu;
  Text: acString;
  Item: TMenuItem;
begin
  Item := TMenuItem(Sender);
  Height := GetSystemMetrics(SM_CYMENU) - 1;
  UpdateFont(ACanvas, Item, False, False);//, True);
  Text := ReplaceStr(Item.Caption, '&', '');
  W := ACanvas.TextWidth(Text);
  Menu := Item.GetParentMenu;
  if Assigned(Menu.Images) and (Item.ImageIndex >= 0) then begin
    if sd = nil then
      Form := TForm(GetOwnerForm(Item.GetParentMenu));
      if Form <> nil then
        sd := TsSkinManager(FOwner).CommonSkinData(GetControlPPI(Form))
      else
        sd := TsSkinManager(FOwner).CommonSkinData;

    inc(W, GetImageHeight(Menu.Images, Item.ImageIndex, TacSkinData(sd).ScalePercent) + 6)
  end
  else
    if not Item.Bitmap.Empty then
      inc(W, Item.Bitmap.Width + 6);

  // If last item (for a MDIChild buttons drawing)
  if LastItem(Item) then
    inc(W, 40);

  Width := W;
end;


procedure TsSkinableMenus.InitItem(Item: TMenuItem; A: boolean);
begin
  if A then
    if not IsTopLine(Item) then begin
      if TsSkinManager(FOwner).SkinnedPopups then begin
        if not Assigned(Item.OnAdvancedDrawItem) or acIgnoreMenuOwnerDraw then
          Item.OnAdvancedDrawItem := sAdvancedDrawItem;

        if not Assigned(Item.OnMeasureItem) or acIgnoreMenuOwnerDraw then
          Item.OnMeasureItem := sMeasureItem;
      end;
    end
    else begin
      Item.OnAdvancedDrawItem := sAdvancedDrawLineItem;
      Item.OnMeasureItem := sMeasureLineItem;
    end
  else begin
    if addr(Item.OnAdvancedDrawItem) = addr(TsSkinableMenus.sAdvancedDrawItem) then
      Item.OnAdvancedDrawItem := nil;

    if addr(Item.OnMeasureItem) = addr(TsSkinableMenus.sMeasureItem) then
      Item.OnMeasureItem := nil;
  end;
end;


procedure TsSkinableMenus.InitMenuLine(Menu: TMainMenu; A: boolean);
var
  i: integer;
begin
  if Assigned(Menu) and not (csDestroying in Menu.ComponentState) then begin
    for i := 0 to Menu.Items.Count - 1 do
      if A then begin
        if TsSkinManager(FOwner).Active then begin
          if not TsSkinManager(FOwner).SkinnedPopups then
            Exit;

          Menu.Items[i].OnAdvancedDrawItem := sAdvancedDrawLineItem;
          Menu.Items[i].OnMeasureItem := sMeasureLineItem;
        end;
      end
      else begin
        if addr(Menu.Items[i].OnAdvancedDrawItem) = addr(TsSkinableMenus.sAdvancedDrawLineItem) then
          Menu.Items[i].OnAdvancedDrawItem := nil;

        if addr(Menu.Items[i].OnMeasureItem) = addr(TsSkinableMenus.sMeasureLineItem) then
          Menu.Items[i].OnMeasureItem := nil;
      end;

    if Menu.Items.Count > 0 then
      Menu.OwnerDraw := A;
  end;
end;


procedure TsSkinableMenus.HookPopupMenu(Menu: TPopupMenu; Active: boolean);
var
  i: integer;

  procedure HookSubItems(Item: TMenuItem);
  var
    i: integer;
  begin
    for i := 0 to Item.Count - 1 do begin
      if Active then begin
        if not Assigned(Item.Items[i].OnAdvancedDrawItem) or acIgnoreMenuOwnerDraw then
          Item.Items[i].OnAdvancedDrawItem := sAdvancedDrawItem;

        if not Assigned(Item.Items[i].OnMeasureItem) or acIgnoreMenuOwnerDraw then
          Item.Items[i].OnMeasureItem := sMeasureItem;
      end
      else begin
        if addr(Item.Items[i].OnAdvancedDrawItem) = addr(TsSkinableMenus.sAdvancedDrawItem) then
          Item.Items[i].OnAdvancedDrawItem := nil;

        if addr(Item.Items[i].OnMeasureItem) = addr(TsSkinableMenus.sMeasureItem) then
          Item.Items[i].OnMeasureItem := nil;
      end;
      HookSubItems(Item.Items[i]);
    end;
  end;

begin
  Active := Active and TsSkinManager(Self.FOwner).IsActive;
  Menu.OwnerDraw := Active and TsSkinManager(Self.FOwner).SkinnedPopups;
  if Active then
    Menu.MenuAnimation := Menu.MenuAnimation + [maNone]
  else
    Menu.MenuAnimation := Menu.MenuAnimation - [maNone];

  for i := 0 to Menu.Items.Count - 1 do begin
    if Active then begin
      if not Assigned(Menu.Items[i].OnAdvancedDrawItem) or acIgnoreMenuOwnerDraw then
        Menu.Items[i].OnAdvancedDrawItem := sAdvancedDrawItem;

      if not Assigned(Menu.Items[i].OnMeasureItem) or acIgnoreMenuOwnerDraw then
        Menu.Items[i].OnMeasureItem := sMeasureItem;
    end
    else begin
      if addr(Menu.Items[i].OnAdvancedDrawItem) = addr(TsSkinableMenus.sAdvancedDrawItem) then
        Menu.Items[i].OnAdvancedDrawItem := nil;

      if addr(Menu.Items[i].OnMeasureItem) = addr(TsSkinableMenus.sMeasureItem) then
        Menu.Items[i].OnMeasureItem := nil;
    end;
    HookSubItems(Menu.Items[i]);
  end;
end;


function TsSkinableMenus.LastItem(Item: TMenuItem): boolean;
begin
  Result := False;
end;


function TsSkinableMenus.NoBorder: boolean;
begin
  with TsSkinManager(FOwner) do
    Result := (CommonSkinData.gd[SkinCommonInfo.Sections[ssMainMenu]].BorderIndex < 0) and
              (CommonSkinData.gd[SkinCommonInfo.Sections[ssMainMenu]].BorderRadius <= 0);
end;


function TsSkinableMenus.IsPopupItem(Item: TMenuItem): boolean;
begin
  Result := Item.GetParentMenu is TPopupMenu;
end;


function TsSkinableMenus.IsRTL(Item: TMenuItem): boolean;
begin
  if SysLocale.MiddleEast then
    Result := Item.GetParentMenu.BiDiMode = bdRightToLeft
  else
    Result := False;
end;


procedure ClearCache;
begin
  DeleteUnusedBmps(True);
end;


procedure TsSkinableMenus.DrawWndBorder(Wnd: hWnd; MenuBmp: TBitmap);
var
  DC: hdc;
  l, i: integer;
  rgn, subRgn: hrgn;
begin
  if not BorderDrawing then
    if IsNT and (MenuBmp <> nil) then begin
      SendMessage(Wnd, SM_ALPHACMD, AC_DROPPEDDOWN shl 16, 0);
      BorderDrawing := True;

      if Length(ArOR) > 0 then begin
        l := Length(ArOR);
        rgn := CreateRectRgn(0, 0, MenuBmp.Width, MenuBmp.Height);
        if l > 0 then
          for i := 0 to l - 1 do begin
            subrgn := CreateRectRgn(ArOR[i].Left, ArOR[i].Top, ArOR[i].Right, ArOR[i].Bottom);
            CombineRgn(rgn, rgn, subrgn, RGN_DIFF);
            DeleteObject(subrgn);
          end
        else begin
          subrgn := CreateRectRgn(0, 0, 1, 1);
          CombineRgn(rgn, rgn, subrgn, RGN_DIFF);
          DeleteObject(subrgn);
        end;
        SetWindowRgn(Wnd, rgn, False);//True);
      end;

      if (SendMessage(Wnd, SM_ALPHACMD, AC_UPDATING_HI, 0) = 0) then begin
        // Draw border (fix for Extraline)
        DC := GetWindowDC(Wnd);
        BitBltBorder(DC, 0, 0, MenuBmp.Width, MenuBmp.Height, MenuBmp.Canvas.Handle, 0, 0, 3);
        ReleaseDC(Wnd, DC);
      end;
      BorderDrawing := False;
    end;
end;


function TsSkinableMenus.ExtraWidth(const mip: PacMenuInfo): integer;
begin
  if mip <> nil then
    with TsSkinManager(FOwner) do
      if MenuSupport.UseExtraLine and mip.HaveExtraLine then
        Result := MenuSupport.ExtraLineWidth * PPI div 96
      else
        Result := 0
  else
    Result := 0;
end;


function TsSkinableMenus.GetItemWidth(aCanvas: TCanvas; Item: TMenuItem; const mi: PacMenuInfo): integer;
var
  Text: string;
  R: TRect;
begin
  UpdateFont(ACanvas, Item, Item.Default, False);//, False);
  case it of
    smDivider:
      Result := Margin * 3 + ACanvas.TextWidth(Text) + GlyphSize(Item).cx * 2 + Spacing;

    smCaption: begin
      Text := cLineCaption + Item.Caption + cLineCaption;
      Result := Margin * 3 + ACanvas.TextWidth(Text) + GlyphSize(Item).cx * 2 + Spacing;
    end

    else begin
      Text := Item.Caption + iff(ShortCutToText(Item.ShortCut) = '', '', ShortCutToText(Item.ShortCut));
      R := MkRect(1, 0);
      DrawText(ACanvas.Handle, PChar(Text), Length(Text), R, DT_EXPANDTABS or DT_SINGLELINE or DT_NOCLIP or DT_CALCRECT);
      Result := WidthOf(R) + Margin * 3 + GlyphSize(Item).cx * 2 + Spacing;
    end;
  end;
  if mi.HaveExtraLine then
    inc(Result, ExtraWidth(mi));
end;


function TsSkinableMenus.ParentWidth(aCanvas: TCanvas; Item: TMenuItem): integer;
var
  i, OldRes, w, h: integer;
  it: TMenuItem;
  R: TRect;
begin
  Result := 0;
  OldRes := 0;
  if GetClipBox(aCanvas.Handle, R) <> NULLREGION then
    Result := WidthOf(R)
  else begin
    for i := 0 to Item.Parent.Count - 1 do
      if Item.Parent.Items[i].Visible then begin
        it := Item.Parent.Items[i];
        if it.Break <> mbNone then begin
          inc(OldRes, Result + 4);
          Result := 0;
        end;
        w := 0;
        h := 0;
        sMeasureItem(it, aCanvas, w, h);
        Result := max(Result, w + 12);
      end;

    inc(Result, OldRes);
  end;
end;


function TsSkinableMenus.PrepareMenuBG(mi: PacMenuInfo; Item: TMenuItem; Width, Height: integer; Wnd: hwnd = 0): TBitmap;
var
  R, gRect, rGlyph: TRect;
  bw, i, j, w, Marg: integer;
  VertFont: TLogFont;
  ItemBmp: TBitmap;
  pFont: PLogFontA;
  ExtraSize: TSize;
  f: TCustomForm;
  CI: TCacheInfo;
  Form: TForm;
  sd: TacSkinData;
  MaskBmp: TBitmap;

  procedure MakeVertFont(Orient: integer);
  begin
    with TsSkinManager(FOwner) do begin
      mi.Bmp.Canvas.Font.Assign(MenuSupport.ExtraLineFont);
      f := GetOwnerForm(Item.GetParentMenu);
      if f <> nil then
        mi.Bmp.Canvas.Font.Charset := f.Font.Charset;

      pFont := PLogFontA(@VertFont);
      StrPCopy(VertFont.lfFaceName, MenuSupport.ExtraLineFont.Name);
      GetObject(mi.Bmp.Canvas.Handle, SizeOf(TLogFont), pFont);
      VertFont.lfEscapement := Orient;
      VertFont.lfHeight     := MenuSupport.ExtraLineFont.Height * PPI div 96;
      VertFont.lfStrikeOut  := integer(fsStrikeOut  in MenuSupport.ExtraLineFont.Style);
      VertFont.lfItalic     := integer(fsItalic     in MenuSupport.ExtraLineFont.Style);
      VertFont.lfUnderline  := integer(fsUnderline	in MenuSupport.ExtraLineFont.Style);

      if fsBold in MenuSupport.ExtraLineFont.Style then
        VertFont.lfWeight := FW_BOLD
      else
        VertFont.lfWeight := FW_NORMAL;

      VertFont.lfCharSet         := MenuSupport.ExtraLineFont.Charset;
      VertFont.lfWidth           := 0;
      Vertfont.lfOutPrecision    := OUT_DEFAULT_PRECIS;
      VertFont.lfClipPrecision   := CLIP_DEFAULT_PRECIS;
      VertFont.lfOrientation     := VertFont.lfEscapement;
      VertFont.lfPitchAndFamily  := Default_Pitch;
      VertFont.lfQuality         := ANTIALIASED_QUALITY;
      mi.Bmp.Canvas.Font.Handle := CreateFontIndirect(VertFont);
      mi.Bmp.Canvas.Font.Color  := sd.gd[j].Props[0].FontColor.Color;
    end;
  end;

begin
  Result := nil;
  with TsSkinManager(FOwner) do begin
    Form := TForm(GetOwnerForm(Item.GetParentMenu));
    if Form <> nil then
      sd := CommonSkinData(GetControlPPI(Form))
    else
      sd := CommonSkinData;

    if not (csDesigning in ComponentState) then
      if Item.Parent.Items[0].Name <> s_SkinSelectItemName then begin
        if ExtraGlyph <> nil then
          FreeAndNil(ExtraGlyph);

        mi.HaveExtraLine := True;
        if Assigned(OnGetMenuExtraLineData) then begin
          ExtraSection := s_MenuExtraLine;
          ExtraCaption := s_DontForget;
          OnGetMenuExtraLineData(Item.Parent.Items[0], ExtraSection, ExtraCaption, ExtraGlyph, mi.HaveExtraLine);
          ExtraCaption := DelChars(ExtraCaption, '&');
        end;
        if not mi.HaveExtraLine and Assigned(ExtraGlyph) then
          FreeAndNil(ExtraGlyph);
      end
      else
        mi.HaveExtraLine := False;

    mi.FirstItem := Item.Parent.Items[0];
    mi.Wnd := Wnd;
    mi.Bmp := CreateBmp32(Width, Height);
    mi.Bmp.Canvas.Lock;
    mi.ItemsIterated := False;
    gRect := MkRect(Width, Height);
    i := SkinCommonInfo.Sections[ssMainMenu];
    // Draw Menu
    IcoLineWidth := GlyphSize(Item, True).cx + Margin + Spacing;
    if IsValidSkinIndex(i) then begin
      bw := FSkinBorderWidth;
      if bw < 0 then begin
        j := sd.gd[i].BorderIndex;
        if j >= 0 then begin
          FSkinBorderWidth := min(sd.ma[j].WL, 3);
          if FSkinBorderWidth <= 0 then
            FSkinBorderWidth := 3;
        end
        else
          FSkinBorderWidth := max(0, sd.gd[i].BorderRadius div 2 - 4);

        bw := FSkinBorderWidth;
      end;

      // Background
      PaintItemBG(i, EmptyCI, 0, gRect, MkPoint, mi.Bmp, TsSkinManager(FOwner).CommonSkinData);
      ci := MakeCacheInfo(mi.Bmp);
      // Ico line painting
      j := GetSkinIndex(MenuSupport.IcoLineSkin);
      if j >= 0 then begin // Ico line
        ItemBmp := CreateBmp32(IcoLineWidth, Mi.Bmp.Height - bw * 2);
        PaintItem(j, ci, True, 0, MkRect(ItemBmp), Point(bw + ExtraWidth(mi), bw), ITemBmp, TsSkinManager(FOwner).CommonSkinData);
        BitBlt(mi.Bmp.Canvas.Handle, bw + ExtraWidth(mi), bw, ItemBmp.Width, ItemBmp.Height, ItemBmp.Canvas.Handle, 0, 0, SrcCopy);
        FreeAndNil(ItemBmp);
      end;
      // Border
      if IsValidImgIndex(sd.gd[i].BorderIndex) then
        DrawSkinRect(mi.Bmp, gRect, MakeCacheInfo(mi.Bmp), sd.ma[sd.gd[i].BorderIndex], 0, False)
      else
        TryPaintBorder(mi.Bmp, gRect, sd.gd[i], 0, CI, nil, sd.PPI, nil);

      ExtraSize := MkSize(MenuSupport.ExtraLineWidth * PPI div 96, mi.Bmp.Height - bw * 2);
      R := MkRect(ExtraSize);
      // Extra line painting
      if MenuSupport.UseExtraLine and mi.HaveExtraLine then begin
        j := GetSkinIndex(ExtraSection);
        if j >= 0 then begin // Extra line
          ItemBmp := CreateBmp32(ExtraSize.cx, ExtraSize.cy);
          PaintItem(j, ci, True, 0, R, Point(bw, bw), ItemBmp, TsSkinManager(FOwner).CommonSkinData);
          BitBlt(mi.Bmp.Canvas.Handle, bw, bw, ItemBmp.Width, ItemBmp.Height, ItemBmp.Canvas.Handle, 0, 0, SrcCopy);
          FreeAndNil(ItemBmp);
        end;
      end;

      if SysLocale.MiddleEast and (Item.GetParentMenu.BiDiMode = bdRightToLeft) then
        StretchBlt(mi.Bmp.Canvas.Handle, mi.Bmp.Width, 0, -mi.Bmp.Width, mi.Bmp.Height,
                   mi.Bmp.Canvas.Handle, 0, 0, mi.Bmp.Width, mi.Bmp.Height, SRCCOPY);

      if MenuSupport.UseExtraLine and mi.HaveExtraLine then begin
        Marg := 12;
        if ExtraGlyph <> nil then begin
          inc(Marg, ExtraGlyph.Height + 8);
          rGlyph.Left := bw + (ExtraWidth(mi) - ExtraGlyph.Width) div 2;
          rGlyph.Bottom := ExtraSize.cy - TacSkinData(sd).Spacing;
          rGlyph.Right := rGlyph.Left + ExtraGlyph.Width;
          rGlyph.Top := rGlyph.Bottom - ExtraGlyph.Height;
          if SysLocale.MiddleEast and (Item.GetParentMenu.BiDiMode = bdRightToLeft) then
            ReflectRect(rGlyph, Width, Height, True);

          ExtraGlyph.HandleType := bmDIB;
          if ExtraGlyph.PixelFormat <> pf32bit then
            mi.Bmp.Canvas.Draw(rGlyph.Left, rGlyph.Top, ExtraGlyph)
          else
            CopyBmp32(rGlyph, MkRect(ExtraGlyph), mi.Bmp, ExtraGlyph, EmptyCI, False, clNone, 0, False);
        end;
        if ExtraCaption <> '' then begin
          MakeVertFont(-2700);
          w := mi.Bmp.Canvas.TextHeight(ExtraCaption);
          mi.Bmp.Canvas.Brush.Style := bsClear;
          OffsetRect(R, bw, 0);
          if SysLocale.MiddleEast and (Item.GetParentMenu.BiDiMode = bdRightToLeft) then
            ReflectRect(R, Width, Height, True);

          mi.Bmp.Canvas.TextRect(R, R.Left + (WidthOf(R) - w) div 2, mi.Bmp.Height - Marg, ExtraCaption);
        end;
        if Assigned(ExtraGlyph) then
          FreeAndNil(ExtraGlyph);
      end;
      // Prepare array of trans. px
      SetLength(ArOR, 0);
      i := GetMaskIndex(i, s_BordersMask, TacSkinData(sd));
      if IsValidImgIndex(i) then begin
        AddRgn(ArOR, mi.Bmp.Width, sd.ma[i], 0, False);
        AddRgn(ArOR, mi.Bmp.Width, sd.ma[i], mi.Bmp.Height - sd.ma[i].WB, True);
      end
      else
        with sd.gd[SkinCommonInfo.Sections[ssMainMenu]] do
          if BorderRadius > 0 then begin
            MaskBmp := GetRoundMask(BorderRadius);
            AddRgn(ArOR, MaskBmp, MkRect(MaskBmp), mi.Bmp.Width, BorderRadius, BorderRadius, BorderRadius, BorderRadius,
                   0, False, False, TColor($FFFFFFFF));

            AddRgn(ArOR, MaskBmp, MkRect(MaskBmp), mi.Bmp.Width, BorderRadius, BorderRadius, BorderRadius, BorderRadius,
                   mi.Bmp.Height - BorderRadius, True, False, TColor($FFFFFFFF));
          end;
    end;
    mi.Bmp.Canvas.UnLock;
  end;
end;


function TsSkinableMenus.RealBorderWidth: integer;
begin
  Result := FBorderWidth;
end;


function TsSkinableMenus.GetMargin: integer;
begin
  if (FOwner is TsSkinManager) and TsSkinManager(FOwner).IsActive and (sd is TacSkinData) then
    Result := TacSkinData(sd).ScaleIntValue(FMargin)
  else
    Result := FMargin;
end;


function TsSkinableMenus.GetMenuInfo(Item: TMenuItem; aCanvas: TCanvas; aWnd: hwnd = 0): PacMenuInfo;
var
  i, l, w, h: integer;
  fi: TMenuItem;
  R: TRect;
begin
  Result := nil;
  l := Length(MenuInfoArray);
  if Item <> nil then begin
    fi := Item.Parent.Items[0];
    for i := 0 to l - 1 do
      if MenuInfoArray[i].FirstItem = fi then begin
        // Found
        Result := @MenuInfoArray[i];
        Break;
      end;

    if Result = nil then begin
      SetLength(MenuInfoArray, Length(MenuInfoArray) + 1);
      Result := @(MenuInfoArray[Length(MenuInfoArray) - 1]);
    end;
    // If not found but BG is required already
    if (Result.Bmp = nil) and (aCanvas <> nil) then begin
      if aWnd <> 0 then
        if GetWindowRect(aWnd, R) then begin
          w := WidthOf(R, True);
          h := HeightOf(R, True);
        end
        else begin
          w := 0;
          h := 0;
        end
      else begin
        w := ParentWidth(ACanvas, Item) + BorderWidth * 2{ + 4} {Fix for some systems where Wnd = 0};
        h := ParentHeight(ACanvas, Item) + BorderWidth * 2;
      end;
      PrepareMenuBG(Result, fi, W, H, aWnd);
    end;
  end
  else
    if aWnd <> 0 then
      for i := 0 to l - 1 do
        if MenuInfoArray[i].Wnd = aWnd then begin
          Result := @MenuInfoArray[i];
          Exit;
        end;
end;


function TsSkinableMenus.GetSpacing: integer;
begin
  if sd is TacSkinData then
    Result := TacSkinData(sd).ScaleIntValue(FSpacing)
  else
    Result := FSpacing;
end;
{$ENDIF}


function TsSkinableMenus.GlyphSize(Item: TMenuItem; Common: boolean = False): TSize;
var
  mi: TMenu;
  i: integer;
begin
  Result := MkSize;
  if Common then begin
    mi := Item.GetParentMenu;
    if Assigned(mi) and Assigned(mi.Images) then
      Result := MkSize(GetImageWidth(mi.Images, -1, PPI), GetImageHeight(mi.Images, -1, PPI))
//      Result := MkSize(GetImageWidth(mi.Images, Item.ImageIndex, PPI), GetImageHeight(mi.Images, Item.ImageIndex, PPI))
    else
      if not Item.Bitmap.Empty then
        Result := MkSize(Item.Bitmap)
      else begin
        i := iDefIcoWidth * PPI div 96;
        Result := MkSize(i, i);
      end
  end
  else
    if not Item.Bitmap.Empty then
      Result := MkSize(Item.Bitmap)
    else
      if Assigned(Item.Parent) and (Item.Parent.SubMenuImages <> nil) then
        Result := MkSize(GetImageWidth(Item.Parent.SubMenuImages, -1, PPI))
      else begin
        mi := Item.GetParentMenu;
        if Assigned(mi) and Assigned(mi.Images) then
          Result := MkSize(GetImageWidth(mi.Images, Item.ImageIndex, PPI), GetImageHeight(mi.Images, Item.ImageIndex, PPI))
        else begin
          i := iDefIcoWidth * PPI div 96;
          Result := MkSize(i, i);
        end;
      end;
end;


constructor TacMenuSupport.Create;
begin
  FUseExtraLine := False;
  FExtraLineWidth := 32;
  FExtraLineFont := TFont.Create;
  FAlphaBlend := MaxByte - 1;
  FFont := TFont.Create;
  FFont.PixelsPerInch := 96;
  FFont.Size := 8;
  FSkinnedShadows := True;
  FShowAccelCharAlways := False;
end;


destructor TacMenuSupport.Destroy;
begin
  FreeAndNil(FExtraLineFont);
  FreeAndNil(FFont);
  if CustomFont then
    FreeAndNil(CustomMenuFont);

  inherited;
end;


function TacMenuSupport.ExtraFontStored: boolean;
begin
  Result := IsCustomFont(nil, FExtraLineFont);
end;


function TacMenuSupport.FontStored: boolean;
begin
  Result := IsCustomFont(nil, FFont);
end;


function TacMenuSupport.LineSkinStored: boolean;
begin
  Result := FIcoLineSkin <> s_MenuIcoLine;
end;


procedure TacMenuSupport.SetCustomFont(const Value: boolean);
var
  sCustomName: string;
begin
  FCustomFont := Value;
  if (DefaultManager <> nil) then
    case DefaultManager.Fonts.MainMode of
      fmCustom:   sCustomName := DefaultManager.Fonts.MainFont;
      fmFromSkin: sCustomName := DefaultManager.SkinCommonInfo.MainFont;
    end;

  if Value or (sCustomName <> '') then begin
    if CustomMenuFont = nil then
      CustomMenuFont := TFont.Create;

    CustomMenuFont.PixelsPerInch := 96;
    CustomMenuFont.Assign(Font);
    CustomMenuFont.Name := sCustomName;
  end
  else
    FreeAndNil(CustomMenuFont);
end;


procedure TacMenuSupport.SetExtraLineFont(const Value: TFont);
begin
  FExtraLineFont.Assign(Value);
end;


procedure TacMenuSupport.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if CustomMenuFont = nil then
    CustomMenuFont := TFont.Create;

  CustomMenuFont.Assign(Font);
end;


initialization

finalization
{$IFNDEF FPC}
  ClearCache;
{$ENDIF}
  if CustomMenuFont <> nil then
    FreeaNdNil(CustomMenuFont);

end.
