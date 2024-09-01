unit sLabel;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ImgList,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, TntStdCtrls, TntGraphics, {$ENDIF}
  {$IFNDEF ALITE} sHtmlParse, {$ENDIF}
  {$IFDEF FPC} LMessages, {$ENDIF}
  sDefaults, sConst, sMessages, sSkinManager;


type
{$IFNDEF NOTFORHELP}
  TsShadowMode = (smNone, smCustom, smSkin1);
  TsKindType = (ktStandard, ktCustom, ktSkin);
{$ENDIF}


{$IFNDEF NOTFORHELP}
  TsCustomLabel = class(TCustomLabel)
  private
    FOnMouseLeave,
    FOnMouseEnter: TNotifyEvent;
    FSkinSection: string;

    TranspMode,
    FSkinIndex: integer;
    FSkinManager: TsSkinManager;

    FSavedParentFont,
    FTransparentForMouse: boolean;
{$IFDEF TNTUNICODE}
    function GetCaption: TWideCaption;
    function IsCaptionStored: Boolean;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    function IsHintStored: Boolean;
    procedure SetHint(const Value: WideString);
    procedure CMHintShow  (var Message: TMessage);      message CM_HINTSHOW;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    function GetLabelText: WideString; reintroduce; virtual;
{$ENDIF}
    procedure SetSkinSection(const Value: string);
    procedure SetSkinManager(const Value: TsSkinManager);
    function GetSkinManager: TsSkinManager;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCurrentFont: TFont; virtual;
    function TextColor: TColor; virtual;
    function SkinIndex: integer;
    procedure WndProc(var Message: TMessage); override;
    function ManagerStored: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure Paint; override;
{$IFNDEF FPC}
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
{$ENDIF}
    property Font;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property SkinManager: TsSkinManager read GetSkinManager write SetSkinManager stored ManagerStored;
    property SkinSection: string read FSkinSection write SetSkinSection;
  {$IFDEF TNTUNICODE}
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  {$ELSE}
    property Caption;
  {$ENDIF}
{$IFDEF D2005}
    property EllipsisPosition;
{$ENDIF}
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar default DefShowAccelChar;
    property ShowHint;
    property Transparent default True;
    property TransparentForMouse: boolean read FTransparentForMouse write FTransparentForMouse default False;
{$IFDEF D2010}
    property Touch;
{$ENDIF}
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  TsClassSkinData = record
    CustomColor,
    CustomFont: boolean;
  end;

  TLinkClickEvent = procedure(Sender: TObject; var ALink: string; var DefaultAction: boolean) of object;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsLabel = class(TsCustomLabel)
{$IFNDEF NOTFORHELP}
  private
    FUseSkinColor: boolean;
    procedure SetUseSkinColor(const Value: boolean);
  protected
    function TextColor: TColor; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Font;
{$ENDIF} // NOTFORHELP
    property UseSkinColor: boolean read FUseSkinColor write SetUseSkinColor default True;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsImgLabel = class(TsLabel)
{$IFNDEF NOTFORHELP}
  private
    FSpacing: integer;
    procedure SetInteger(const Index, Value: integer);
  protected
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImageIndex: integer;
    function ImageExists: boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawIntText(ACanvas: TCanvas; var Rect: TRect; Flags: Longint);
    procedure AdjustBounds; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
{$ENDIF} // NOTFORHELP
    property ImageIndex: integer index 0 read FImageIndex write SetInteger default -1;
    property Spacing:    integer index 1 read FSpacing    write SetInteger default 4;

    property Images: TCustomImageList read FImages write SetImages;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFNDEF ALITE}
  TsHTMLLabel = class(TsLabel)
  {$IFNDEF NOTFORHELP}
  private
    HotLink,
    PressedLink: integer;
    FUseHTML,

    InSizing,
    LinkClicking: boolean;

    Links: TacLinks;
    FShowMode: TsWindowShowMode;
    FOnLinkClick: TLinkClickEvent;
    FFitToWidth: boolean;
    function GetPlainCaption: acString;
    procedure SetUseHTML(const Value: boolean);
    procedure SetFitToWidth(const Value: boolean);
    procedure BufferedRepaint;
  public
    procedure DoClick(LinkIndex: integer);
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  {$ENDIF} // NOTFORHELP
  published
    property PlainCaption: acString read GetPlainCaption;
    property FitToWidth: boolean read FFitToWidth write SetFitToWidth default False;
    property UseHTML: boolean read FUseHTML write SetUseHTML default True;
    property ShowMode: TsWindowShowMode read FShowMode write FShowMode default soDefault;
    {:@event}
    property OnLinkClick: TLinkClickEvent read FOnLinkClick write FOnLinkClick;
  end;
{$ENDIF}


{$IFNDEF NOTFORHELP}
  TsEditLabel = class({$IFNDEF ALITE}TsHTMLLabel{$ELSE}TsLabel{$ENDIF})
  public
    BoundLabel: TObject;
    procedure Click; override;
    procedure DblClick; override;
    constructor InternalCreate(BoundStruct: TObject);
    destructor Destroy; override;
    function TextColor: TColor; override;
  end;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsWebLabel = class(TsLabel)
{$IFNDEF NOTFORHELP}
  private
    FNormalFont: TFont;
    procedure SetHoverFont (const Value: TFont);
    procedure SetNormalFont(const Value: TFont);
  protected
    FHoverFont: TFont;
    FURL: string;
    FShowMode: TsWindowShowMode;
    function TextColor: TColor; override;
    function GetCurrentFont: TFont; override;
    procedure ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF}); override;
    procedure CMParentFontChanged(var Message: TMessage);       message CM_PARENTFONTCHANGED;
    procedure CMMouseEnter       (var Message: TMessage);       message CM_MOUSEENTER;
    procedure CMMouseLeave       (var Message: TMessage);       message CM_MOUSELEAVE;
    procedure WMEraseBkGnd       (var Message: TWMLButtonDown); message WM_ERASEBKGND;
  public
    MouseAbove: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Font: TFont read FNormalFont write SetNormalFont;
    property Cursor default crHandPoint;
{$ENDIF} // NOTFORHELP
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property ShowMode: TsWindowShowMode read FShowMode write FShowMode default soDefault;
    property URL: string read FURL write FURL;
  end;


  TsKind = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FColor: TColor;
    FOwner: TControl;
    FKindType: TsKindType;
    procedure SetKindType(const Value: TsKindType);
    procedure SetColor(const Value: TColor);
  public
    constructor Create(AOwner: TControl);
  published
{$ENDIF} // NOTFORHELP
    property KindType: TsKindType read FKindType write SetKindType default ktSkin;
    property Color: TColor read FColor write SetColor default clWhite;
  end;


{$IFNDEF NOTFORHELP}
  TsLabelFX = class;

  TShadowData = record
    Blur,
    Offset,
    Alpha: integer;
    Color: TColor;
  end;


  TacOffsetKeeper = class(TPersistent)
  private
    FRightBottom, FLeftTop: integer;
  published
    property LeftTop: integer read FLeftTop write FLeftTop stored True;
    property RightBottom: integer read FRightBottom write FRightBottom stored True;
  end;
{$ENDIF} // NOTFORHELP


  TsShadow = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FDistance,
    FBlurCount: Integer;

    FBuffered: Boolean;
    FColor: TColor;
    ParentControl: TsLabelFX;
    FMode: TsShadowMode;
    FAlphaValue: byte;
    FOffsetKeeper: TacOffsetKeeper;
    procedure UpdateRGB;
    procedure SetAlphaValue(const Value: byte);
    procedure SetColor     (const Value: TColor);
    procedure SetMode      (const Value: TsShadowMode);
    procedure SetInteger   (const Index, Value: integer);
  public
    sr, sg, sb: Integer;
    constructor Create(Control: TsLabelFX);
    destructor Destroy; override;
    property ShadowBuffered: Boolean read FBuffered write FBuffered default False;
  published
    property OffsetKeeper: TacOffsetKeeper read FOffsetKeeper write FOffsetKeeper;
{$ENDIF} // NOTFORHELP
    property AlphaValue: byte read FAlphaValue write SetAlphaValue default MaxByte;
    property Color: TColor read FColor write SetColor default clBlack;
    property Mode: TsShadowMode read FMode write SetMode default smSkin1;
    property BlurCount: integer Index 0 read FBlurCount write SetInteger default 4;
    property Distance:  integer Index 1 read FDistance  write SetInteger default 1;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  // Used shading algorithm by Gennady Ermakovets (gen@godeltech.com)
  TsLabelFX = class(TsCustomLabel)
{$IFNDEF NOTFORHELP}
  private
    FAngle,
    OffsTopLeft,
    FMaskBitsSize: Integer;

    IntPosChanging,
    FNeedInvalidate: boolean;

    FMask: TBitmap;
    FMaskBits: Pointer;
    FShadow: TsShadow;
    FKind: TsKind;
    PrevCtrlWidth: integer;
    procedure SetAngle(const Value: integer);
  protected
    function TextColor: TColor; override;
  {$IFNDEF FPC}
    procedure AdjustBounds; override;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure WndProc (var Message: TMessage); override;
  {$IFNDEF FPC}
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  {$ENDIF}
    function CurrentShadowData: TShadowData;
    function CurrentFontColor: TColor;
    function ShadowExists: boolean;
    procedure UpdatePosition;
  published
    property Font;
{$ENDIF} // NOTFORHELP
    property Angle: integer read FAngle write SetAngle;
    property Kind: TsKind read FKind write FKind;
    property Shadow: TsShadow read FShadow write FShadow;
  end;


(* ///////////////////////////////////////////////////////////////////////
TsStickyLabel component was based on the TGMSStickyLabel control code

GMSStickyLabel v1.1 July/5/97        by Glenn Shukster & Jacques Scoatarin

  GMS COMPUTING INC.                 Phone         (905)771-6458
  53 COLVIN CRES.                    Fax                   -6819
  THORNHILL, ONT.                    Compuserve:       72734,123
  CANADA  L4J 2N7                    InternetId:Gms@Shaw.wave.ca
                                     http://members.tor.shaw.wave.ca/~gms/

  Jacques Scoatarin                  Phone (357)2-492591
  52 Athalassis Ave, (flat 202)      InternetId:j.scoatarin@cytanet.com.cy
  Nicosia, Cyprus
/////////////////////////////////////////////////////////////////////// *)
{$IFNDEF NOTFORHELP}
  TAlignTo = (altLeft, altTop, altBottom, altRight, altTopCenter, altBottomCenter);
{$ENDIF}
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsStickyLabel = class(TsLabel)
{$IFNDEF NOTFORHELP}
  private
    FGap: Integer;
    FAlignTo: TAlignTo;
    FAttachTo: TControl;
    FRealigning: Boolean;
    FOldWinProc: TWndMethod;
    Procedure SetGap(Value: Integer);
    procedure SetAttachTo(Value: TControl);
    Procedure SetAlignTo(Value: TAlignTo);
    procedure NewWinProc(var Message: TMessage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Adjust(MoveLabel: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Font;
{$ENDIF} // NOTFORHELP
    property AlignTo: TAlignTo read FAlignTo write SetAlignTo default altLeft;
    property AttachTo: TControl read FAttachTo write SetAttachTo;
    Property Gap: Integer Read FGap write SetGap Default 2;
  end;


implementation

uses ShellAPI, math,
  {$IFDEF TNTUNICODE} TntWindows, {$ENDIF}
  sVCLUtils, sGraphUtils, acntUtils, sCommonData, sStyleSimply, acAlphaImageList, sThirdParty, acntTypes, sAlphaGraph;


var
  FontChanging: boolean;


function GetParentCache(Control: TControl): TCacheInfo;
var
  ParentBG: TacBGInfo;
begin
  ParentBG.PleaseDraw := False;
  GetBGInfo(@ParentBG, Control.Parent);
  Result := BGInfoToCI(@ParentBG);
end;


constructor TsCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkinManager := nil;
  FSkinIndex := -1;
  FSavedParentFont := False;
  FTransparentForMouse := False;
  ShowAccelChar := DefShowAccelChar;
  Transparent := True;
end;


{$IFNDEF FPC}
{$IFDEF TNTUNICODE}
procedure TsCustomLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  if (SkinManager <> nil) and SkinManager.IsActive then begin
    Canvas.Font.Assign(Font);
    Canvas.Font.Color := TextColor;
    Font.Color := Canvas.Font.Color;
  end;
  if not TntLabel_DoDrawText(Self, Rect, Flags, GetLabelText) then
    inherited;
end;
{$ELSE}
procedure TsCustomLabel.DoDrawText(var Rect: TRect; Flags: Longint);
  {$IFDEF D2005}
const
  Ellipsis: array [TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  NewRect: TRect;
  DText, Text: string;
  Height, Delim: Integer;
  {$ELSE}
var
  Text: string;
  {$ENDIF}
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + s_Space;

  if not ShowAccelChar then
    Flags := Flags or DT_NOPREFIX;

  Flags := DrawTextBiDiModeFlags(Flags) or DT_ALPHATEXT * integer((TranspMode = TM_FULL) and Transparent);
  Canvas.Font := GetCurrentFont;
{$IFDEF D2005}
  if (EllipsisPosition <> epNone) and not AutoSize then begin
    DText := Text;
    Flags := Flags and not DT_EXPANDTABS;
    Flags := Flags or Ellipsis[EllipsisPosition];
    if WordWrap and (EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
      repeat
        NewRect := Rect;
        Dec(NewRect.Right, Canvas.TextWidth(s_Ellipsis));
        acDrawText(Canvas.Handle, DText, NewRect, Flags or DT_CALCRECT);
        Height := NewRect.Bottom - NewRect.Top;
        if (Height > ClientHeight) and (Height > Canvas.Font.Height) then begin
          Delim := LastDelimiter(s_Space + #9, Text);
          if Delim = 0 then
            Delim := Length(Text);

          Dec(Delim);
  {$IF NOT DEFINED(CLR)}
          if ByteType(Text, Delim) = mbLeadByte then
            Dec(Delim);
  {$IFEND}
          Text := Copy(Text, 1, Delim);
          DText := Text + s_Ellipsis;
          if Text = '' then
            Break;
        end
        else
          Break;
      until False;

    if Text <> '' then
      Text := DText;
  end;
{$ENDIF}
  Canvas.Font.Color := ColorToRGB(TextColor);
  acDrawText(Canvas.Handle, PChar(Text), {Length(Text), }Rect, Flags);
end;
{$ENDIF}
{$ENDIF}


function TsCustomLabel.GetCurrentFont: TFont;
begin
  Result := inherited Font;
end;


{$IFDEF TNTUNICODE}
function TsCustomLabel.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;


function TsCustomLabel.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;


procedure TsCustomLabel.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
  AdjustBounds;
end;


function TsCustomLabel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;


function TsCustomLabel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;


procedure TsCustomLabel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;


procedure TsCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  TntLabel_CMDialogChar(Self, Message, Caption);
end;


procedure TsCustomLabel.CMHintShow(var Message: TMessage);
begin
  ProcessCMHintShowMsg(Message);
  inherited;
end;


function TsCustomLabel.GetLabelText: WideString;
begin
  Result := Caption;
end;
{$ENDIF}


procedure TsCustomLabel.Paint;
var
  Bmp: TBitmap;
begin
  if not (csDestroying in ComponentState) then begin
    if (SkinIndex >= 0) and (SkinManager <> nil) and SkinManager.IsActive then
      PaintItem(SkinIndex, GetParentCache(Self), True, 0, MkRect(Self), Point(Left, Top), Canvas.Handle, SkinManager.CommonSkinData);

    inherited;
    if (TranspMode = TM_PARTIAL) or not Transparent then begin
      Bmp := CreateBmp32(Self);
      BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, 0, 0, SRCCOPY);
      FillAlphaRect(Bmp, MkRect(Bmp));
      BitBlt(Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      Bmp.Free;
    end;
    FSkinIndex := -1;
  end;
end;


var
  acLabelFontChanging: boolean = False;

procedure TsCustomLabel.WndProc(var Message: TMessage);
var
  bRect: TRect;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    CM_HITTEST:
      if FTransparentForMouse then begin
        Message.Result := HTNOWHERE;
        Exit;
      end;

    CM_PARENTBIDIMODECHANGED, CM_TEXTCHANGED, CM_FONTCHANGED: begin
      if ([csDestroying, csLoading] * ComponentState = []) and (Parent <> nil) and not acLabelFontChanging then begin
        if not FontChanging then begin
          acLabelFontChanging := True;
{$IFNDEF FPC}
          if AutoSize then
            case Alignment of
              taRightJustify:
                if BiDiMode = bdLeftToRight then begin
                  bRect := BoundsRect;
                  Width := MulDiv(Width, 3, 2);
                  AdjustBounds;
                  Left := bRect.Left + WidthOf(bRect) - Width;
                end
                else
                  AdjustBounds
              else
                AdjustBounds;
            end;
{$ENDIF}
          if not InAnimationProcess then
            Invalidate;

          acLabelFontChanging := False;
        end;
      end;
      Exit;
    end;

    CM_MOUSEENTER:
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);
  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETGLASSMODE: begin
          TranspMode := Message.LParamHi;
          Message.Result := MakeLResult(Message.LParamLo, TranspMode);
          if (TranspMode = TM_FULL) and Transparent and AutoSize then // If GDI+ text writing
            AdjustBounds;
        end;

        AC_REMOVESKIN:
          if [csDestroying] * ComponentState = [] then begin
            if (SkinManager <> nil) and (csDestroying in SkinManager.ComponentState) then
              FSkinManager := nil;

            if FSavedParentFont then
              ParentFont := True;

            FSkinIndex := -1;
            Invalidate;
          end;

        AC_REFRESH:
          if ((Message.WParamLo = 0) or (Message.WParamLo = FSkinIndex)) and ([csDesigning, csDestroying] * ComponentState = []) then begin
            if ParentFont then
              FSavedParentFont := True;

            FSkinIndex := -1;
            Invalidate;
          end;

        AC_GETFONTINDEX:
          if (SkinManager <> nil) and SkinManager.Active and (Parent <> nil) then
            Message.Result := GetFontIndex(Parent, PacPaintInfo(Message.LParam));

        AC_ISOPAQUE:
          if (SkinManager <> nil) and SkinManager.IsActive then
            Message.Result := 1
          else
            Message.Result := integer(not (csOpaque in ControlStyle));

        AC_FONTSCHANGED:
          SkinManager.UpdateFontName(Self);
      end;

    CM_MOUSELEAVE:
      if Assigned(FOnMouseLeave) then
        FOnMouseLeave(Self);
  end;
end;


function TsCustomLabel.ManagerStored: boolean;
begin
  Result := FSkinManager <> nil;
end;


procedure TsCustomLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = SkinManager then
      SkinManager := nil;
end;


procedure TsCustomLabel.SetSkinSection(const Value: string);
begin
  if FSkinSection <> Value then begin
    FSkinIndex := -1;
    FSkinSection := Value;
    Repaint;
  end;
end;


function TsCustomLabel.SkinIndex: integer;
begin
  if (SkinManager <> nil) and SkinManager.IsActive and (SkinSection <> '') then begin
    if FSkinIndex < 0 then
      FSkinIndex := SkinManager.GetSkinIndex(SkinSection)
  end
  else
    FSkinIndex := -1;

  Result := FSkinIndex;
end;


function TsCustomLabel.TextColor: TColor;
begin
  if (SkinManager <> nil) and SkinManager.IsActive then
    Result := SkinManager.Palette[pcLabelText]
  else
    Result := Font.Color;

  if not Enabled then
    Result := BlendColors(Result, SkinManager.GetGlobalColor, DefBlendDisabled);
end;


procedure TsCustomLabel.SetSkinManager(const Value: TsSkinManager);
begin
  if FSkinManager <> Value then begin
    FSkinManager := Value;
    Repaint;
  end;
end;


function TsCustomLabel.GetSkinManager: TsSkinManager;
begin
  if FSkinManager <> nil then
    Result := FSkinManager
  else
    Result := DefaultManager;
end;

type TAccessControl = class(TWinControl);

procedure TsCustomLabel.Loaded;
begin
  inherited;
  UpdateGlassMode(Self, False);
  if SkinManager <> nil then
    SkinManager.UpdateFontName(Self);
end;


procedure TsWebLabel.ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  Font.Height := MulDiv(Font.Height, M, D);
  HoverFont.Height := MulDiv(HoverFont.Height, M, D);
end;


procedure TsWebLabel.Click;
begin
  inherited;
{$WARNINGS OFF}
  if FURL <> '' then
    {$IFDEF TNTUNICODE}ShellExecuteW{$ELSE}ShellExecute{$ENDIF}(Application.{$IFDEF FPC}MainFormHandle{$ELSE}Handle{$ENDIF}, 'open', PacChar(FURL), nil, nil, ord(FShowMode));
end;


procedure TsWebLabel.CMMouseEnter(var Message: TMessage);
var
  R: TRect;
begin
  inherited;
  if Enabled then begin
    MouseAbove := True;
    if AutoSize then begin
      Invalidate;
      Update;
{$IFNDEF FPC}
      AdjustBounds;
{$ENDIF}
    end;
    R := BoundsRect;
    InvalidateRect(Parent.Handle, @R, True);
  end;
end;


procedure TsWebLabel.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  inherited WndProc(Message);
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_FONTSCHANGED:
          if not ParentFont then begin
            FNormalFont.Assign(TacAccessControl(Self).Font);
            FHoverFont.Name := FNormalFont.Name;
            FHoverFont.Size := Font.Size;
          end;
      end;
  end;
end;


procedure TsWebLabel.CMMouseLeave(var Message: TMessage);
var
  R: TRect;
begin
  inherited;
  if Enabled then begin
    MouseAbove := False;
    if AutoSize then begin
      Invalidate;
      Update;
{$IFNDEF FPC}
      AdjustBounds;
{$ENDIF}
    end;
    R := BoundsRect;
    InvalidateRect(Parent.Handle, @R, True);
  end;
end;


procedure TsWebLabel.CMParentFontChanged(var Message: TMessage);
var
  c: TColor;
begin
  inherited;
  if ParentFont and Assigned(Parent) then begin
    C := FNormalFont.Color;
    FNormalFont.Assign(TacAccessControl(Parent).Font);
    FHoverFont.Name := Font.Name;
    FHoverFont.Size := Font.Size;
    FNormalFont.Color := C;
  end;
end;


constructor TsWebLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowMode := soDefault;
  FHoverFont := TFont.Create;
  FNormalFont := TFont.Create;
  FNormalFont.OnChange := inherited Font.OnChange;
  Cursor := crHandPoint;
  ControlStyle := ControlStyle + [csOpaque];
  Transparent := True;
end;


destructor TsWebLabel.Destroy;
begin
  FreeAndNil(FHoverFont);
  FreeAndNil(FNormalFont);
  inherited Destroy;
end;


function TsWebLabel.GetCurrentFont: TFont;
begin
  if MouseAbove and not (csDesigning in ComponentState) then
    Result := FHoverFont
  else
    Result := FNormalFont;
end;


procedure TsWebLabel.Loaded;
begin
  inherited Loaded;
  inherited Font.Assign(FNormalFont);
end;


procedure TsWebLabel.SetHoverFont(const Value: TFont);
begin
  FHoverFont.Assign(Value);
end;


procedure TsWebLabel.SetNormalFont(const Value: TFont);
begin
  inherited Font.Assign(Value);
  FNormalFont.Assign(Value);
  FontChanging := True;
  Paint;
  FontChanging := False;
end;


function TsWebLabel.TextColor: TColor;
var
  DefManager: TsSkinManager;
begin
  DefManager := SkinManager;
  if MouseAbove and not (csDesigning in ComponentState) and Enabled then
    if UseSkinColor and (DefManager <> nil) and DefManager.Active then
      Result := DefManager.Palette[pcWebTextHot]
    else
      Result := FHoverFont.Color // TCTop
  else
    if not (csDestroying in ComponentState) and
         (DefManager <> nil) and
           DefManager.Active and
             UseSkinColor {$IFNDEF SKININDESIGN}and
               not ((csDesigning in ComponentState) and (GetOwnerFrame(Self) <> nil)){$ENDIF} then begin
      Result := DefManager.Palette[pcWebText]
    end
    else
      Result := ColorToRGB(FNormalFont.Color);

  if not Enabled then
    if DefManager <> nil then
      Result := BlendColors(Result, DefManager.GetGlobalColor, DefBlendDisabled)
    else
      Result := BlendColors(Result, TacAccessControl(Parent).Color, DefBlendDisabled);
end;


procedure TsWebLabel.WMEraseBkGnd(var Message: TWMLButtonDown);
begin
  //
end;


procedure TsEditLabel.Click;
begin
  inherited;
  if (BoundLabel <> nil) and TsBoundLabel(BoundLabel).AllowClick then
    TsBoundLabel(BoundLabel).DoClick(TsBoundLabel(BoundLabel).FOwnerControl);
end;


procedure TsEditLabel.DblClick;
begin
  inherited;
  if (BoundLabel <> nil) and TsBoundLabel(BoundLabel).AllowClick then
    TsBoundLabel(BoundLabel).DoClick(TsBoundLabel(BoundLabel).FOwnerControl);
end;


destructor TsEditLabel.Destroy;
begin
  if BoundLabel <> nil then begin
    TsBoundLabel(BoundLabel).FActive := False;
    TsBoundLabel(BoundLabel).FTheLabel := nil;
  end;
  inherited Destroy;
end;


constructor TsEditLabel.InternalCreate(BoundStruct: TObject);
begin
  inherited Create(nil);
  BoundLabel := BoundStruct;
  if (BoundLabel <> nil) and (TsBoundLabel(BoundLabel).FCommonData <> nil) then
    SkinManager := TsBoundLabel(BoundLabel).FCommonData.SkinManager;
end;


function TsEditLabel.TextColor: TColor;
begin
  Result := inherited TextColor;
end;


{$IFDEF DELPHI_10BERLIN}
const
  RecOffs1 = 8 * SizeOf(TObject) + SizeOf(TWndMethod) + 10 * SizeOf(integer) + 2 * SizeOf(TControlStyle) +
    10 * SizeOf(Boolean) + 7 * SizeOf(TAlign{1}) + SizeOf(TColor) + 2 * SizeOf(TCursor) + SizeOf(string) +
    SizeOf(TWinControlClass) + 21 * SizeOf(TCanResizeEvent);

type
  TacScaledControl = class(TComponent)
  private
{$HINTS OFF}
    Offset: array [0..RecOffs1 - 1] of byte;
    FOnGesture: TGestureEvent;

    FHelpType: THelpType;
    FHelpKeyword: string;
    FHelpContext: THelpContext;
    FCustomHint: TCustomHint;
    FParentCustomHint: Boolean;
    FText: PChar;
    FStyleElements: TStyleElements;
{$HINTS ON}
    InScaling: boolean;
  end;
{$ENDIF}

procedure TsStickyLabel.Adjust(MoveLabel: boolean);
var
  IntAlign: TAlignTo;
  AGap, iNewTop, iNewLeft: Integer;
  MoveRelativeTo, Mover: TControl;
begin
  if FAttachTo <> nil then
    if ([csLoading] * ComponentState = []) and ([csLoading] * FAttachTo.ComponentState = []) and ([csLoading] * Parent.ComponentState = [])
    {$IFDEF DELPHI_10BERLIN} and not TacScaledControl(FAttachTo).InScaling and not TacScaledControl(Self).InScaling {$ENDIF} then begin
      FRealigning := True;
      AGap := FGap * GetControlPPI(Self) div 96;
      if MoveLabel then begin
        MoveRelativeTo := FAttachTo;
        Mover := Self;
        IntAlign := FAlignTo;
      end
      else begin
        MoveRelativeTo := Self;
        Mover := FAttachTo;
        IntAlign := altRight;
        case FAlignTo of
          altTop:         IntAlign := altBottom;
          altRight:       IntAlign := altLeft;
          altBottom:      IntAlign := altTop;
          altTopCenter:   IntAlign := altBottomCenter;
          altBottomCenter:IntAlign := altTopCenter;
        end;
      end;
      case IntAlign of
        altLeft: begin
          iNewTop :=  MoveRelativeTo.Top + (MoveRelativeTo.Height - Mover.Height) div 2;
          iNewLeft := MoveRelativeTo.Left - Mover.Width - AGap;
        end;

        altRight: begin
          iNewTop :=  MoveRelativeTo.Top + (MoveRelativeTo.Height - Mover.Height) div 2;
          iNewLeft := MoveRelativeTo.Left + MoveRelativeTo.Width + AGap;
        end;

        altTop: begin
          iNewTop := MoveRelativeTo.Top - Mover.Height - AGap;
          iNewLeft := MoveRelativeTo.Left;
        end;

        altTopCenter: begin
          iNewTop := MoveRelativeTo.Top - Mover.Height - AGap;
          iNewLeft := MoveRelativeTo.Left - (Mover.Width - MoveRelativeTo.Width) div 2;
        end;

        altBottomCenter:begin
          iNewTop := MoveRelativeTo.Top + MoveRelativeTo.Height + AGap;
          iNewLeft := MoveRelativeTo.Left - (Mover.Width - MoveRelativeTo.Width) div 2;
        end

        else begin
          iNewTop := MoveRelativeTo.Top + MoveRelativeTo.Height + AGap;
          iNewLeft := MoveRelativeTo.Left;
        end;
      end;
      if MoveLabel then
        Mover.SetBounds(iNewLeft, iNewTop, Mover.Width, Mover.Height);

      FRealigning := False;
    end;
end;


constructor TsStickyLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGap := 2;
  FRealigning := False;
end;


destructor TsStickyLabel.Destroy;
begin
  SetAttachTo(nil);
  inherited Destroy;
end;


procedure TsStickyLabel.Loaded;
begin
  inherited;
  if FAttachTo <> nil then
    Adjust(True);
end;

procedure TsStickyLabel.NewWinProc(var Message: TMessage);
begin
  if not (csDestroying in ComponentState) then
    if Assigned(FAttachTo) and not FRealigning then begin
      FRealigning := True;
      try
        case(Message.Msg) of
          CM_ENABLEDCHANGED:
            Enabled := FAttachTo.Enabled;

          CM_VISIBLECHANGED:
            Visible := FAttachTo.Visible;

          WM_SIZE, WM_MOVE, WM_WINDOWPOSCHANGED:
            Adjust(True);
        end;
      finally
        FRealigning := False;
      end;
    end;

  if Assigned(FOldWinProc) then
    FOldWinProc(Message);
end;


procedure TsStickyLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAttachTo) then
    SetAttachTo(nil);

  inherited Notification(AComponent, Operation);
end;


procedure TsStickyLabel.SetAlignTo(Value: TAlignTo);
begin
  if FAlignTo <> Value then begin
    FAlignTo := Value;
    Adjust(True);
  end;
end;


procedure TsStickyLabel.SetAttachTo(Value: TControl);
begin
  if Value <> FAttachTo then begin
    if Assigned(FAttachTo) then
      FAttachTo.WindowProc := FOldWinProc;

    FAttachTo := Value;
    if Assigned(Value) then begin
      Adjust(True);
      Enabled := FAttachTo.Enabled;
      Visible := FAttachTo.Visible;
      FOldWinProc := FAttachTo.WindowProc;
      FAttachTo.WindowProc := NewWinProc;
    end;
  end;
end;


procedure TsStickyLabel.SetGap(Value: Integer);
begin
  if FGap <> Value then begin
    FGap := Value;
    Adjust(True);
  end;
end;


procedure TsStickyLabel.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REFLECTLAYOUT:
          case AlignTo of
            altLeft:  AlignTo := altRight;
            altRight: AlignTo := altLeft;
          end;
        else
          inherited;
      end;

    WM_WINDOWPOSCHANGED:
      if not (csDestroying in ComponentState) and Assigned(FAttachTo) and not FRealigning then begin
        FRealigning := True;
        try
          Adjust(True);
        finally
          FRealigning := False;
        end;
      end
      else
        inherited;
    else
      inherited;
  end;
end;


constructor TsShadow.Create(Control: TsLabelFX);
begin
  FColor := clBlack;
  FBlurCount := 4;
  FDistance := 1;
  FMode := smSkin1;
  FAlphaValue := MaxByte;
  ParentControl := Control;
  FOffsetKeeper := TacOffsetKeeper.Create;
end;


destructor TsShadow.Destroy;
begin
  FreeAndNil(FOffsetKeeper);
  inherited;
end;


procedure TsShadow.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      ParentControl.UpdatePosition;
      ParentControl.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FBlurCount, Value);
    1: ChangeProp(FDistance, Value);
  end;
end;


procedure TsShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    UpdateRGB;
    ParentControl.Invalidate;
  end;
end;


procedure TsShadow.SetMode(const Value: TsShadowMode);
begin
  if FMode <> Value then begin
    FMode := Value;
    UpdateRGB;
    ParentControl.UpdatePosition;
    ParentControl.Invalidate;
  end;
end;


procedure TsShadow.UpdateRGB;
begin
  with TsColor(ColorToRGB(FColor)) do begin
    sr := R;
    sg := G;
    sb := B;
  end;
end;


procedure TsShadow.SetAlphaValue(const Value: byte);
begin
  if FAlphaValue <> Value then begin
    FAlphaValue := Value;
    ParentControl.Invalidate;
  end;
end;


{$IFNDEF FPC}
procedure TsLabelFX.AdjustBounds;
var
  R: TRect;
  Text: acString;
  AAlignment: TAlignment;
  Size, ActSize, OldSize: TSize;
  X, Y, OffsTopLeft, OffsRightBottom: integer;
begin
  if ([csReading, csLoading] * ComponentState = []) and (Parent <> nil) then begin
    AAlignment := Alignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);

    with CurrentShadowData do begin
      OffsTopLeft     := min(0, Offset - Blur);
      OffsRightBottom := max(0, Offset + Blur);
    end;
    X := Left;
    Y := Top;
    with Shadow.OffsetKeeper do
      if AutoSize then begin
        Text := GetLabelText;
        if (Text = '') or ShowAccelChar and (Text[1] = '&') and (Text[2] = #0) then
          Text := Text + s_Space;

        if Align in [alClient, alTop, alBottom] then
          R := MkRect(Width, 0)
        else
          R := MkRect(MaxInt, 0);

        DoDrawText(R, DT_EXPANDTABS or DT_CALCRECT or DT_NOPREFIX or TextWrapping[WordWrap]);
        Size := MkSize(R);

        with Size do begin
          ActSize := Size;
          inc(cx, - OffsTopLeft + OffsRightBottom);
          inc(cy, - OffsTopLeft + OffsRightBottom);
          OldSize := MkSize(Width + LeftTop - RightBottom, Height + LeftTop - RightBottom);
          case AAlignment of
            taLeftJustify:  X := X - LeftTop + OffsTopLeft;
            taCenter:       X := X - (LeftTop - OffsTopLeft - RightBottom + OffsRightBottom) div 2;
            taRightJustify: begin
              if PrevCtrlWidth = 0 then
                PrevCtrlWidth := Width;

              X := PrevCtrlWidth;
              X := Left + X;
              X := X - Size.cx;
              PrevCtrlWidth := Size.cx;
            end;
          end;
          case Layout of
            tlTop:    Y := Y - LeftTop + OffsTopLeft;
            tlCenter: Y := Y - (LeftTop - OffsTopLeft - RightBottom + OffsRightBottom) div 2;
            tlBottom: Y := Y + OffsTopLeft + Height - RightBottom - OldSize.cy;
          end;
          if Layout = tlBottom then
            Y := Y + OffsTopLeft + Height - RightBottom - ActSize.cy;

          SetBounds(X, Y, cx, cy);
        end;
      end
      else begin
        OldSize.cx := Width  + LeftTop - RightBottom;
        OldSize.cy := Height + LeftTop - RightBottom;
        case AAlignment of
          taLeftJustify:  X := X - LeftTop + OffsTopLeft;
          taCenter:       X := X - (LeftTop - OffsTopLeft - RightBottom + OffsRightBottom) div 2;
          taRightJustify: X := X + OffsTopLeft + Width - RightBottom - OldSize.cx;
        end;

        case Layout of
          tlTop:    Y := Y - LeftTop + OffsTopLeft;
          tlCenter: Y := Y - (LeftTop - OffsTopLeft - RightBottom + OffsRightBottom) div 2;
          tlBottom: Y := Y + OffsTopLeft + Height - RightBottom - OldSize.cy;
        end;
        inc(OldSize.cx, - OffsTopLeft + OffsRightBottom);
        inc(OldSize.cy, - OffsTopLeft + OffsRightBottom);
        SetBounds(X, Y, OldSize.cx, OldSize.cy);
      end;
  end;
end;
{$ENDIF}


constructor TsLabelFX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkinIndex := -1;
  FShadow := TsShadow.Create({AOwner, }Self);
  FKind := TsKind.Create(Self);
  FMask := CreateBmp32;
  PrevCtrlWidth := 0;
  FMaskBits := nil;
  IntPosChanging := False;
  FNeedInvalidate := True;
end;


function TsLabelFX.CurrentFontColor: TColor;
begin
  case FKind.KindType of
    ktStandard:
      Result := ColorToRGB(Font.Color);

    ktCustom:
      Result := ColorToRGB(Kind.Color);

    else {ktSkin}
      if (SkinManager <> nil) and SkinManager.IsActive then
        with SkinManager, SkinCommonInfo do begin
          if SkinSection <> '' then
            FSkinIndex := GetSkinIndex(SkinSection);

          if ShadowExists then begin
            if FSkinIndex < 0 then
                if IsValidSkinIndex(IndexGlobalInfo) and (CommonSkinData.gd[IndexGlobalInfo].Props[0].FontColor.Left <> -1) then
                  Result := ColorToRGB(CommonSkinData.gd[IndexGlobalInfo].Props[0].FontColor.Left)
                else
                  Result := ColorToRGB(Kind.Color)
            else
              Result := ColorToRGB(CommonSkinData.gd[FSkinIndex].Props[0].FontColor.Color);
          end
          else
            Result := GetFontColor(Self, FSkinIndex, SkinManager);
        end
      else
        Result := ColorToRGB(Kind.Color);
  end;
end;


function TsLabelFX.CurrentShadowData: TShadowData;
begin
  case FShadow.Mode of
    smCustom: begin
      Result.Color  := FShadow.Color;
      Result.Blur   := FShadow.BlurCount;
      Result.Offset := FShadow.FDistance;
      Result.Alpha  := FShadow.AlphaValue;
    end;

    smSkin1:
      with SkinManager do begin
        if (SkinManager <> nil) and Active and (CommonSkinData.Shadow1Blur >= 0) then begin
          Result.Color  := SkinCommonInfo.Shadow1Color;
          Result.Blur   := CommonSkinData.Shadow1Blur;
          Result.Offset := CommonSkinData.Shadow1Offset;
        end
        else begin
          Result.Color  := FShadow.Color;
          Result.Blur   := FShadow.BlurCount;
          Result.Offset := FShadow.FDistance;
        end;
        Result.Alpha := FShadow.AlphaValue;
      end

    else begin
      Result.Blur   := 0;
      Result.Color  := 0;
      Result.Offset := 0;
      Result.Alpha  := 0;
    end;
  end;
end;


destructor TsLabelFX.Destroy;
begin
  FreeAndNil(FShadow);
  FreeAndNil(FMask);
  FreeAndNil(FKind);
  FreeMem(FMaskBits);
  inherited;
end;


procedure DrawDisabledText(DC: hdc; Text: PacChar; aRect: TRect; Flags: integer; Angle: integer; Offset: TPoint);
begin
  OffsetRect(aRect, 1, 1);
  SetTextColor(DC, ColorToRGB(clBtnHighlight));
  if Angle = 0 then
    acDrawText(DC, Text, aRect, Flags)
  else
    acExtTextOut(DC, aRect.Left + Offset.X, aRect.Top + Offset.Y, 0, @aRect, PacChar(Text), Length(Text), nil);

  OffsetRect(aRect, -1, -1);
  SetTextColor(DC, ColorToRGB(clBtnShadow));
  if Angle = 0 then
    acDrawText(DC, PacChar(Text), aRect, Flags)
  else
    acExtTextOut(DC, aRect.Left + Offset.X, aRect.Top + Offset.Y, 0, @aRect, PacChar(Text), Length(Text), nil);
end;


{$IFNDEF FPC}
procedure TsLabelFX.DoDrawText(var Rect: TRect; Flags: Longint);
const
  LB_BORDER = 3;
var
  sP: TPoint;
  Size: TSize;
  invert: byte;
  DstRect: TRect;
  Text: acString;
  rcos, rsin: real;
  DoShadow: boolean;
  MaskOffs, D0, D: PByte;
  ShadowData: TShadowData;
  iPad, OffsRightBottom, ShadowOffset: Integer;
  offs_North, offs_South, offs_West, offs_East: PByte;
  DeltaD, W, H, nW, nH, mWidth, mHeight: Integer;

  procedure AddMask;
  var
    DeltaS, bWidth, y, x: Integer;
    MaskOffs, S0, S: PByte;
  begin // Fill mask
    MaskOffs := PByte(ACNativeInt(FMaskBits) + W + 1);
    bWidth := FMask.Width - 1;
    if InitLine(FMask, Pointer(S0), DeltaS) then
      for y := 0 to FMask.Height - 1 do begin
        S := Pointer(PAnsiChar(S0) + DeltaS * Y);
        for x := 0 to bWidth do begin
          if S^ <> 0 then
            MaskOffs^ := MaxByte;

          S := PByte(ACNativeInt(S) + 4);
          MaskOffs := PByte(ACNativeInt(MaskOffs) + 1);
        end;
        MaskOffs := PByte(ACNativeInt(MaskOffs) + 2);
      end;
  end;

  procedure PrepareMask;
  var
    i, y, x: integer;
  begin
    if FNeedInvalidate or not FShadow.FBuffered then begin
      // Clear a mask BG (black = 0)
      FMask.Width := WidthOf(Rect, True);
      FMask.Height := HeightOf(Rect, True);
      FMask.Canvas.Brush.Color := 0;
      FMask.Canvas.FillRect(MkRect(FMask));

      OffsetRect(DstRect, ShadowData.Offset, ShadowData.Offset);
      // Text props
      // Draw a white mask text
      if Angle = 0 then begin
        FMask.Canvas.Font.Assign(Font);
{$IFDEF DELPHI_XE}
//        if FMask.Canvas.Font.Quality = fqDefault then
//          FMask.Canvas.Font.Quality := fqAntialiased;
{$ENDIF}
        FMask.Canvas.Font.Color := clWhite;
        acDrawText(FMask.Canvas.Handle, Text, DstRect, Flags);
      end
      else begin
        MakeAngledFont(FMask.Canvas.Handle, Font, Angle * 10);
        SetTextColor(FMask.Canvas.Handle, ColorToRGB(clWhite));
        acExtTextOut(FMask.Canvas.Handle, DstRect.Left + sP.X, DstRect.Top + sP.Y, 0, @DstRect, PacChar(Text), Length(Text), nil);
      end;

      mWidth := FMask.Width;
      mHeight := FMask.Height;
      W := mWidth + 2;
      H := mHeight + 2;
      if FMaskBitsSize < W * H * 2 then begin
        FMaskBitsSize := W * H * 2;
        ReallocMem(FMaskBits, FMaskBitsSize);
      end;
      FillChar(PChar(FMaskBits)^, W * H * 2, 0);
      // Blur the mask
      for i := 1 to ShadowData.Blur do begin
        MaskOffs := PByte(ACNativeInt(FMaskBits) + W + 1);
        AddMask;
        offs_North := PByte(ACNativeInt(MaskOffs) - W);
        offs_South := PByte(ACNativeInt(MaskOffs) + W);
        offs_West  := PByte(ACNativeInt(MaskOffs) - 1);
        offs_East  := PByte(ACNativeInt(MaskOffs) + 1);
        for y := 0 to H - 3 do begin
          for x := 0 to W - 3 do begin
            MaskOffs^ := (offs_North^ + offs_South^ + offs_West^ + offs_East^) shr 2;
            MaskOffs   := PByte(ACNativeInt(MaskOffs)   + 1);
            offs_North := PByte(ACNativeInt(offs_North) + 1);
            offs_South := PByte(ACNativeInt(offs_South) + 1);
            offs_West  := PByte(ACNativeInt(offs_West)  + 1);
            offs_East  := PByte(ACNativeInt(offs_East)  + 1);
          end;
          MaskOffs   := PByte(ACNativeInt(MaskOffs)   + 2);
          offs_North := PByte(ACNativeInt(offs_North) + 2);
          offs_South := PByte(ACNativeInt(offs_South) + 2);
          offs_West  := PByte(ACNativeInt(offs_West)  + 2);
          offs_East  := PByte(ACNativeInt(offs_East)  + 2);
        end;
      end;

      MaskOffs := PByte(ACNativeInt(FMaskBits) + mWidth + 3);
      if Transparent then
        BitBlt(FMask.Canvas.Handle, 0, 0, mWidth, mHeight, Canvas.Handle, 0, 0, SRCCOPY)
      else
        if (TranspMode = TM_FULL) and Transparent then
          FillRect32(FMask, MkRect(mWidth, mHeight), ColorToRGB(Color))
        else
          FillDC(FMask.Canvas.Handle, MkRect(mWidth, mHeight), ColorToRGB(Color));

      if InitLine(FMask, Pointer(D0), DeltaD) then
        with FShadow do
          for y := 0 to mHeight - 1 do begin
            D := Pointer(PAnsiChar(D0) + DeltaD * Y);
            for x := 0 to mWidth - 1 do begin
              MaskOffs^ := MaskOffs^ * ShadowData.Alpha shr 8;
              invert := not MaskOffs^;
              case invert of
                MaxByte: D := PByte(ACNativeInt(D) + 2); // Just skip transparent part
                0: begin // Non-transparent
                  D^ := sb;
                  D := PByte(ACNativeInt(D) + 1);
                  D^ := sg;
                  D := PByte(ACNativeInt(D) + 1);
                  D^ := sr;
                end
                else begin
                  D^ := (D^ * invert + sb * MaskOffs^) shr 8;
                  D := PByte(ACNativeInt(D) + 1);
                  D^ := (D^ * invert + sg * MaskOffs^) shr 8;
                  D := PByte(ACNativeInt(D) + 1);
                  D^ := (D^ * invert + sr * MaskOffs^) shr 8;
                end;
              end;
              D := PByte(ACNativeInt(D) + 2);
              MaskOffs := PByte(ACNativeInt(MaskOffs) + 1);
            end;
            MaskOffs := PByte(ACNativeInt(MaskOffs) + 2);
          end;

      FNeedInvalidate := False;
    end; // Need Invalidate
  end;

begin
  if not (csLoading in ComponentState) then begin
    // Prepare text
    Text := GetLabelText;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and (Text[1] = '&') and (Text[2] = #0)) then
      Text := Text + s_Space;

    // Prepare flags
    Flags := DrawTextBiDiModeFlags(Flags) or DT_NOCLIP or Longint(TextWrapping[WordWrap]) or DT_ALPHATEXT * integer((TranspMode = TM_FULL) and Transparent);
    if not ShowAccelChar then
      Flags := Flags or DT_NOPREFIX;

    Canvas.Font.Assign(Font);
    if Angle <> 0 then begin
      MakeAngledFont(Canvas.Handle, Font, Angle * 10);
      SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
    end;

    DstRect := MkRect(WidthOf(Rect));
    acDrawText(Canvas.Handle, Text, DstRect, Flags or DT_CALCRECT);
    Size := MkSize(WidthOf(DstRect), HeightOf(DstRect));

    // Calc size and offset of text
    rcos := acCos(Angle);
    rsin := acSin(Angle);

    nW := Round(Size.cx * abs(rcos) + Size.cy * abs(rsin));
    nH := Round(Size.cx * abs(rsin) + Size.cy * abs(rcos));

    if Flags and DT_CALCRECT = 0 then begin
      sP.X := Round((Size.cx - Size.cx * rcos - Size.cy * rsin) / 2) + (nW - Size.cx) div 2 + 1;
      sP.Y := Round((Size.cy * (1 - rcos)     + Size.cx * rsin) / 2) + (nH - Size.cy) div 2 + 1;
    end;

    DoShadow := ((FShadow.Mode <> smNone) or (FSkinIndex >= 0) and (SkinManager <> nil) and SkinManager.IsActive);
    if DoShadow then begin
      ShadowData := CurrentShadowData;
      with Shadow, TsColor(ColorToRGB(ShadowData.Color)) do begin
        sr := R;
        sg := G;
        sb := B;
      end;
      ShadowOffset := ShadowData.Offset;
      OffsTopLeft := Min(0, ShadowData.Offset - ShadowData.Blur);
      OffsRightBottom := Max(0, ShadowData.Offset + ShadowData.Blur);
      iPad := OffsRightBottom + OffsTopLeft;
    end
    else begin
      iPad := 0;
      OffsTopLeft := 0;
      ShadowOffset := 0;
      OffsRightBottom := 0;
    end;

    // Positioning
    if AutoSize then begin
      DstRect.Left := Rect.Left - OffsTopLeft;
      DstRect.Top := Rect.Top - OffsTopLeft;
    end
    else begin
      case Alignment of
        taLeftJustify:  DstRect.Left := Rect.Left - OffsTopLeft;
        taCenter:       DstRect.Left := (Rect.Right - nW - iPad) div 2 + ShadowOffset;
        taRightJustify: DstRect.Left := Rect.Right - nW - OffsRightBottom;
      end;
      case Layout of
        tlTop:    DstRect.Top := Rect.Top - OffsTopLeft;
        tlCenter: DstRect.Top := (Height - nH - iPad) div 2 + ShadowOffset;
        tlBottom: DstRect.Top := Height - nH - OffsRightBottom;
      end;
    end;
    DstRect.Right := DstRect.Left + nW;
    DstRect.Bottom := DstRect.Top + nH;
    if Flags and DT_CALCRECT = 0 then
      if not Enabled then
        DrawDisabledText(Canvas.Handle, PacChar(Text), DstRect, Flags, Angle, sP)
      else begin
        if DoShadow then begin // If not standard kind
          PrepareMask;
          BitBlt(Canvas.Handle, 0, 0, FMask.Width, FMask.Height, FMask.Canvas.Handle, 0, 0, SRCCOPY);
          OffsetRect(DstRect, -ShadowOffset, -ShadowOffset);
        end;
        if Angle <> 0 then begin
          MakeAngledFont(Canvas.Handle, Font, Angle * 10);
          SetTextColor(Canvas.Handle, CurrentFontColor);
          {$IFDEF TNTUNICODE}ExtTextOutW{$ELSE}ExtTextOut{$ENDIF}(Canvas.Handle, DstRect.Left + sP.X, DstRect.Top + sP.Y, 0, @DstRect, PacChar(Text), Length(Text), nil);
        end
        else begin
          SetTextColor(Canvas.Handle, CurrentFontColor);
          acDrawText(Canvas.Handle, Text, DstRect, Flags);
        end;
      end
    else
      Rect := MkRect(WidthOf(DstRect), HeightOf(DstRect));
  end
  else
    inherited;
end;
{$ENDIF}


procedure TsLabelFX.Invalidate;
begin
  if WordWrap and (Angle <> 0) then
    Angle := 0;

  inherited;
end;


procedure TsLabelFX.SetAngle(const Value: integer);
begin
  if FAngle <> Value then begin
    FAngle := Value;
    if (Value <> 0) and WordWrap then
      WordWrap := False;

{$IFNDEF FPC}
    if AutoSize then begin
      AdjustBounds;
      Repaint;
    end
    else
{$ENDIF}
      Invalidate;
  end;
end;


function TsLabelFX.ShadowExists: boolean;
begin
  Result := Shadow.Mode <> smNone;
end;


function TsLabelFX.TextColor: TColor;
begin
  if (SkinManager <> nil) and SkinManager.IsActive and (Kind.KindType = ktSkin) then
    Result := SkinManager.Palette[pcLabelText]
  else
    if Kind.KindType = ktCustom then
      Result := Kind.Color
    else
      Result := clWindowText;

  if not Enabled then
    Result := BlendColors(Result, SkinManager.GetGlobalColor, DefBlendDisabled);
end;


procedure TsLabelFX.UpdatePosition;
var
  CurrentOffset1, CurrentOffset2: integer;
begin
  if not (csLoading in ComponentState) and ((DefaultManager = nil) or ([msScaleChanging, msAnimScaling, msFormScaling] * DefaultManager.ManagerState = [])) then begin
    with CurrentShadowData do begin
      CurrentOffset1 := min(0, Offset - Blur);
      CurrentOffset2 := max(0, Offset + Blur);
    end;
    if (CurrentOffset1 <> Shadow.OffsetKeeper.LeftTop) or (CurrentOffset2 <> Shadow.OffsetKeeper.RightBottom) then begin
      IntPosChanging := True;
{$IFNDEF FPC}
      AdjustBounds;
{$ENDIF}
      Shadow.OffsetKeeper.LeftTop := CurrentOffset1;
      Shadow.OffsetKeeper.RightBottom := CurrentOffset2;
      IntPosChanging := False;
    end;
  end;
end;


procedure TsLabelFX.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN, AC_SETNEWSKIN:
          if not (csDestroying in ComponentState) then
            UpdatePosition;
      end;

    WM_WINDOWPOSCHANGED, WM_SIZE, WM_MOVE:
      if (FShadow <> nil) and not IntPosChanging then
        with CurrentShadowData do begin
          Shadow.OffsetKeeper.LeftTop := min(0, Offset - Blur);
          Shadow.OffsetKeeper.RightBottom := Max(0, Offset + Blur);
        end;
  end;
end;


constructor TsKind.Create(AOwner: TControl);
begin
  FKindType := ktSkin;
  FColor := clWhite;
  FOwner := AOwner
end;


procedure TsKind.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    FOwner.Invalidate
  end
end;


procedure TsKind.SetKindType(const Value: TsKindType);
begin
  if FKindType <> Value then begin
    FKindType := Value;
    FOwner.Invalidate
  end
end;


constructor TsLabel.Create(AOwner: TComponent);
begin
  inherited;
  FUseSkinColor := True;
end;


procedure TsLabel.SetUseSkinColor(const Value: boolean);
begin
  if FUseSkinColor <> Value then begin
    FUseSkinColor := Value;
    if not (csLoading in ComponentState) then
      Repaint;
  end;
end;


function TsLabel.TextColor: TColor;
begin
  if FUseSkinColor and (SkinManager <> nil) and SkinManager.Active then
    Result := GetFontColor(Self, SkinIndex, SkinManager)
  else
    Result := Font.Color;

  if not Enabled then
    Result := BlendColors(Result, GetControlColor(Parent), DefBlendDisabled);
end;


{$IFNDEF ALITE}
procedure TsHTMLLabel.DoClick(LinkIndex: integer);
var
  b: boolean;
  s: string;
begin
  b := True;
  s := Links[LinkIndex].URL;
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, s, b);

  if (s <> '') and b then
    ShellExecute(Application.Handle, 'open', PChar(s), nil, nil, ord(FShowMode));
end;


procedure TsHTMLLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  iLink: integer;
  Bmp: TBitmap;
  Html: TsHtml;
  R: TRect;
begin
  if FUseHTML then begin
    if Caption <> '' then begin
      Html := TsHtml.Create;
      Bmp := CreateBmp32(Width, Height);
      Bmp.Canvas.Font.Assign(Font);
      Bmp.Canvas.Brush.Style := bsClear;
      Bmp.Canvas.Font.Color := TextColor;
      try
        HTML.SkinManager := SkinManager;
        HTML.TranspMode := TranspMode;

        if HotLink >= 0 then
          iLink := Links[HotLink].Ndx
        else
          iLink := -1;

        if Flags and DT_CALCRECT <> 0 then begin
          R := Rect;
          if AutoSize and not FFitToWidth then
            if not (Align in [alClient, alTop, alBottom]) then
              R.Right := R.Left + MaxWord
            else
              R.Right := R.Left + Width;

          HTML.Init(Bmp, Caption, R, iLink, PressedLink);
          Rect := HTML.HTMLText(Links, True);
        end
        else begin
          HTML.Alignment := Alignment;
          HTML.BidiMode := BidiMode;
          HTML.Layout := Layout;
          HTML.Init(Bmp, Caption, Rect, iLink, PressedLink);
          BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
          HTML.HTMLText(Links);
          BitBlt(Canvas.Handle, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        end;
      finally
        Bmp.Free;
        Html.Free;
      end;
    end;
  end
  else
    inherited;
end;


procedure TsHTMLLabel.WndProc(var Message: TMessage);
var
  ActNdx: integer;

  function ActiveLink(InArray: boolean): integer;
  var
    i: integer;
    mPos: TPoint;
  begin
    mPos := ScreenToClient(acMousePos);
    for i := 0 to Length(Links) - 1 do
      if PtInRect(Links[i].Bounds, mPos) then begin
        if InArray then
          Result := Links[i].Ndx
        else
          Result := i;

        Exit;
      end;

    Result := -1;
  end;

begin
  case Message.Msg of
    CM_TEXTCHANGED:
      if ((FocusControl <> nil) and (csLoading in FocusControl.ComponentState)) or not (Visible or (csDesigning in ComponentState)) then
        Exit;

    WM_LBUTTONDOWN: begin
      if HotLink >= 0 then begin
        PressedLink := HotLink;
        BufferedRepaint;
      end;
    end;

    WM_LBUTTONUP:
      if not LinkClicking and (PressedLink >= 0) then begin
        LinkClicking := True;
        if HotLink >= 0 then
          DoClick(PressedLink);

        PressedLink := -1;
        LinkClicking := False;
        BufferedRepaint;
      end;

    WM_MOUSEMOVE: begin
      ActNdx := ActiveLink(False);
      if ActNdx >= 0 then begin
        if HotLink <> ActNdx then begin
          HotLink := ActNdx;
          Cursor := crHandPoint;
          BufferedRepaint;
        end;
      end
      else
        if HotLink >= 0 then begin
          HotLink := -1;
          Cursor := crDefault;
          BufferedRepaint;
        end;
    end;

    WM_MOUSELEAVE, CM_MOUSELEAVE:
      if (HotLink >= 0) or (PressedLink >= 0) then begin
        HotLink := -1;
        PressedLink := -1;
        Cursor := crDefault;
        BufferedRepaint;
      end;
  end;
  inherited;
  case Message.Msg of
    WM_WINDOWPOSCHANGED, WM_SIZE:
      if AutoSize and (Align in [alClient, alTop, alBottom]) then
        if not InSizing then begin
          InSizing := True;
          AdjustBounds;
          InSizing := False;
        end;
  end;
end;


procedure TsHTMLLabel.BufferedRepaint;
var
  DC: hdc;
  TmpBmp: TBitmap;
  ParentBG: TacBGInfo;
begin
  if (Visible or (csDesigning in ComponentState) and not (csNoDesignVisible in ControlStyle)) and (Parent <> nil) and Parent.HandleAllocated then
    if (csOpaque in ControlStyle) or (DefaultManager <> nil) and DefaultManager.Active then begin
      TmpBmp := CreateBmp32(Self);
      DC := GetDC(Parent.Handle);
      IntersectClipRect(DC, Left, Top, Left + Width, Top + Height);
      try
        TmpBmp.Canvas.Lock;
        if not (csOpaque in ControlStyle) then begin // Transparent
          ParentBG.PleaseDraw := False;
          GetBGInfo(@ParentBG, Parent);
          case ParentBG.BgType of
            btCache: if ParentBG.Bmp <> nil then
              BitBlt(TmpBmp.Canvas.Handle, 0, 0, Width, Height, ParentBG.Bmp.Canvas.Handle, Left + ParentBG.Offset.X, Top + ParentBG.Offset.Y, SRCCOPY);

            btFill:
              FillDC(TmpBmp.Canvas.Handle, MkRect(Self), ColorToRGB(ParentBG.Color));
          end;
        end;

        TAccessControl(Parent).PaintControls(TmpBmp.Canvas.Handle, Self);
        BitBlt(DC, 0, 0, Width, Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
        TmpBmp.Canvas.UnLock;
      finally
        ReleaseDC(Parent.Handle, DC);
        TmpBmp.Free;
      end;
    end
    else begin
      Invalidate;
      Update;
    end;
end;


constructor TsHTMLLabel.Create(AOwner: TComponent);
begin
  inherited;
  FUseHTML := True;
  FFitToWidth := False;
  HotLink := -1;
  PressedLink := -1;
  InSizing := False;
  LinkClicking := False;
  FShowMode := soDefault;
end;


function TsHTMLLabel.GetPlainCaption: acString;
begin
  Result := GetPlainText(Caption);
end;


procedure TsHTMLLabel.SetFitToWidth(const Value: boolean);
begin
  if FFitToWidth <> Value then begin
    FFitToWidth := Value;
    if not (csLoading in ComponentState) and AutoSize then
      AdjustSize;
  end;
end;


procedure TsHTMLLabel.SetUseHTML(const Value: boolean);
begin
  if FUseHTML <> Value then begin
    FUseHTML := Value;
    Repaint;
  end;
end;
{$ENDIF}


procedure TsImgLabel.AdjustBounds;
var
  iPPI, h, w: integer;
begin
  inherited;
  if ([csReading, csLoading] * ComponentState = []) and AutoSize and ImageExists then begin
    iPPI := GetControlPPI(Self);
    h := maxi(Height, GetImageHeight(FImages, FImageIndex, iPPI));
    w := Width + FSpacing + GetImageWidth(FImages, FImageIndex, iPPI);
    SetBounds(Left, Top, w, h);
  end;
end;


constructor TsImgLabel.Create(AOwner: TComponent);
begin
  inherited;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageIndex := -1;
  FSpacing := 4;
end;


destructor TsImgLabel.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  inherited;
end;


procedure TsImgLabel.DrawIntText(ACanvas: TCanvas; var Rect: TRect; Flags: Integer);
{$IFDEF D2005}
const
  Ellipsis: array [TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  NewRect: TRect;
  DText, Text: string;
  Height, Delim: Integer;
{$ELSE}
var
  Text: string;
{$ENDIF}
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + s_Space;

  if not ShowAccelChar then
    Flags := Flags or DT_NOPREFIX;

  Flags := DrawTextBiDiModeFlags(Flags);// or DT_ALPHATEXT * integer((TranspMode = TM_FULL) and Transparent);
  ACanvas.Font := GetCurrentFont;
{$IFDEF D2005}
  if (EllipsisPosition <> epNone) and not AutoSize then begin
    DText := Text;
    Flags := Flags and not DT_EXPANDTABS;
    Flags := Flags or Ellipsis[EllipsisPosition];
    if WordWrap and (EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
      repeat
        NewRect := Rect;
        Dec(NewRect.Right, ACanvas.TextWidth(s_Ellipsis));
        acDrawText(ACanvas.Handle, DText, NewRect, Flags or DT_CALCRECT);
        Height := NewRect.Bottom - NewRect.Top;
        if (Height > ClientHeight) and (Height > ACanvas.Font.Height) then begin
          Delim := LastDelimiter(s_Space + #9, Text);
          if Delim = 0 then
            Delim := Length(Text);

          Dec(Delim);
{$IF NOT DEFINED(CLR)}
          if ByteType(Text, Delim) = mbLeadByte then
            Dec(Delim);
{$IFEND}
          Text := Copy(Text, 1, Delim);
          DText := Text + s_Ellipsis;
          if Text = '' then
            Break;
        end
        else
          Break;
      until False;

    if Text <> '' then
      Text := DText;
  end;
{$ENDIF}
  ACanvas.Font.Color := TextColor;
  acDrawText(ACanvas.Handle, PacChar(Text), Rect, Flags);
end;


function TsImgLabel.ImageExists: boolean;
begin
  Result := (FImages <> nil) and IsValidIndex(FImageIndex, GetImageCount(FImages));
end;


procedure TsImgLabel.ImageListChange(Sender: TObject);
begin
  if AutoSize then
    AdjustBounds;

  Repaint;
end;


procedure TsImgLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TsImgLabel.Paint;
var
  Rect, CalcRect, GlyphRect: TRect;
  p, h, w, ndx: integer;
  DrawStyle: Longint;
  sd: TacSkinData;
  Bmp: TBitmap;
  C: TColor;
begin
  if not (csDestroying in ComponentState) then begin
    Bmp := CreateBmp32(Self);
    BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
    if (SkinIndex >= 0) and (SkinManager <> nil) and SkinManager.IsActive then
      PaintItem(SkinIndex, GetParentCache(Self), True, 0, MkRect(Self), Point(Left, Top), Bmp, DefaultManager.CommonSkinData);

    with Bmp.Canvas do begin
      Rect := ClientRect;
      if not Transparent then begin
        Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(Rect)
      end;
      if ImageExists then begin
        sd := GetSkinData(nil, GetControlPPI(Self));
        p := sd.PPI;
        h := GetImageHeight(Images, FImageIndex, p);
        w := GetImageWidth (Images, FImageIndex, p);
        GlyphRect.Top := (Height - h) div 2;
        GlyphRect.Bottom := GlyphRect.Top + h;
        GlyphRect.Left := 0;
        GlyphRect.Right := w;
        Bmp.Canvas.Font.Color := TextColor;
        SetImagesPPI(Images, p);
        if (Images is TsAlphaImageList) and (SkinManager <> nil) and SkinManager.IsActive and SkinManager.Effects.DiscoloredGlyphs then begin
          if NeedParentFont(SkinManager, SkinIndex, 0) then
            ndx := GetFontIndex(Parent, SkinIndex, SkinManager, 0)
          else
            ndx := SkinIndex;

          if ndx >= 0 then
            C := SkinManager.CommonSkinData.gd[ndx].Props[0].Color
          else
            C := 0;

          DrawAlphaImgList(Images, Bmp, GlyphRect.Left, GlyphRect.Top, ImageIndex, 0, C, 0, 1, False, p);
        end
        else
          Images.Draw(Bmp.Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, True);
      end
      else
        GlyphRect := MkRect;

      if Caption <> '' then begin
        Brush.Style := bsClear;
        Bmp.Canvas.Font.Assign(Font);
        if GlyphRect.Right <> 0 then
          Rect.Left := GlyphRect.Right + FSpacing;

        DrawStyle := DT_EXPANDTABS or TextWrapping[WordWrap] or acAlignmentArray[Alignment] or DT_ALPHATEXT * integer((TranspMode = 2) and Transparent);
        if Layout <> tlTop then begin
          CalcRect := Rect;
          DrawIntText(Bmp.Canvas, CalcRect, DrawStyle or DT_CALCRECT);
          if Layout = tlBottom then
            OffsetRect(Rect, 0, Height - CalcRect.Bottom)
          else
            OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
        end;
        DrawIntText(Bmp.Canvas, Rect, DrawStyle);
        if (TranspMode = TM_PARTIAL) or not Transparent then
          FillAlphaRect(Bmp, Rect);
      end;
    end;
    BitBlt(Canvas.Handle, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
  end;
end;


procedure TsImgLabel.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if AutoSize then
      AdjustBounds;

    Repaint;
  end;
end;


procedure TsImgLabel.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    if AutoSize then
      AdjustBounds;
  end;
end;


procedure TsImgLabel.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in ComponentState) then begin
        if AutoSize then
          AdjustBounds;

        Repaint;
      end;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FImageIndex, Value);
    1: ChangeProp(FSpacing, Value);
  end;
end;


initialization
  Classes.RegisterClasses([TsEditLabel])

finalization

end.
