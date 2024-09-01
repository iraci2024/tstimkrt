{***************************************************************************}
{ TAdvGraphicCheckLabel component                                           }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright � 2014 - 2021                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvGraphicCheckLabel;

{$I TMSDEFS.INC}

interface

{$R AdvGraphicCheckLabel.res}

uses
  Windows, Classes, Types, Messages, Controls, Forms, Dialogs, SysUtils, AdvGDIP,
  Graphics
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with runtime enabling/disabling control
  // v1.1.0.0 : New : ShowFocus property added
  //          : New : ReturnIsTab property added
  //          : New : Transparent property added
  // v1.1.1.0 : Improved: Support for High DPI (Text height, center images)
  // v1.2.0.0 : New : LabelSettings.DisabledPicture property added
  // v1.2.0.1 : Fixed : Invalidate after checklabel is enabled/disabled
  // v1.3.0.0 : New : Alignment property added
  // v1.4.0.0 : New : AutoSize property added

type

  TLabelSettings =  class(TPersistent)
  private
    FPicture: TAdvGdipPicture;
    FText: string;
    FFont: TFont;
    FHoverPicture: TAdvGdipPicture;
    FDisabledPicture: TAdvGdipPicture;
    FOnChange: TNotifyEvent;
    FPictureColor: TColor;
    FHoverFontColor: TColor;
    FHoverFontStyle: TFontStyles;
    procedure SetPicture(const Value: TAdvGdipPicture);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetHoverPicture(const Value: TAdvGdipPicture);
    procedure SetPictureColor(const Value: TColor);
    procedure SetDisabledPicture(const Value: TAdvGdipPicture);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DisabledPicture: TAdvGdipPicture read FDisabledPicture write SetDisabledPicture;
    property HoverFontStyle: TFontStyles read FHoverFontStyle write FHoverFontStyle;
    property HoverFontColor: TColor read FHoverFontColor write FHoverFontColor default clNone;
    property HoverPicture: TAdvGdipPicture read FHoverPicture write SetHoverPicture;
    property Font: TFont read FFont write SetFont;
    property Picture: TAdvGdipPicture read FPicture write SetPicture;
    property PictureColor: TColor read FPictureColor write SetPictureColor;
    property Text: string read FText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCheckKind = (ckCheck, ckLike, ckAvailable, ckWish, ckBookmark, ckFavorite, ckInCart, ckFollowing, ckCustom);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGraphicCheckLabel = class(TCustomControl)
  private
    FStateChecked: TLabelSettings;
    FStateUnChecked: TLabelSettings;
    FChecked: boolean;
    FHasMouse: boolean;
    FOldCursor: TCursor;
    FKind: TCheckKind;
    FDesignTime: boolean;
    FShowFocus: boolean;
    FReturnIsTab: boolean;
    FTransparent: boolean;
    FAlignment: TLeftRight;
    FAutoSize: boolean;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetStateChecked(const Value: TLabelSettings);
    procedure SetStateUnChecked(const Value: TLabelSettings);
    procedure SetChecked(const Value: boolean);
    procedure SetKind(const Value: TCheckKind);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetShowFocus(const Value: boolean);
    procedure SetTransparent(const Value: boolean);
    procedure SetAlignment(const Value: TLeftRight);
    procedure SetAutoSizeEx(const Value: boolean);
  protected
    function GetActiveRect(Canvas: TCanvas; ASettings: TLabelSettings): TRect; virtual;
    function GetTextRect: TRect; virtual;
    function GetPicRect: TRect; virtual;
    function InActiveRect(x,y: integer): boolean; virtual;
    procedure LoadPictureSet(AName, CaptionNormal, CaptionActive: string);
    procedure LoadPredefined;
    procedure SettingsChanged(Sender: TObject);
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoDraw(Canvas: TCanvas; ASettings: TLabelSettings; Measure: boolean): TSize; virtual;
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetVersionNr: Integer; virtual;
    procedure Toggle; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure UpdateSize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Alignment: TLeftRight read FAlignment write SetAlignment default taRightJustify;
    property Anchors;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx;
    property Checked: boolean read FChecked write SetChecked default false;
    property Enabled;
    property Hint;

    property Kind: TCheckKind read FKind write SetKind default ckCheck;

    {$IFDEF DELPHIXE_LVL}
    property Margins;
    property Padding;
    {$ENDIF}
    property PopupMenu;

    property ReturnIsTab: boolean read FReturnIsTab write FReturnIsTab default false;
    property ShowFocus: boolean read FShowFocus write SetShowFocus default true;
    property ShowHint;
    property StateChecked: TLabelSettings read FStateChecked write SetStateChecked;
    property StateUnChecked: TLabelSettings read FStateUnChecked write SetStateUnChecked;

    property TabOrder;
    property TabStop;
    property Transparent: boolean read FTransparent write SetTransparent default false;
    property Version: string read GetVersion write SetVersion;

    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnExit;
    property OnEndDrag;
    property OnEnter;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    {$IFDEF DELPHIXE_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
  end;


implementation

uses
  Math, ActiveX;

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

function GetDPIScale(AControl: TControl; ACanvas: TCanvas): single;
var
  {$IFDEF DELPHIXE10_LVL}
  frm: TCustomForm;
  {$ENDIF}
  {$IFNDEF DELPHIXE10_LVL}
  FHDC: HDC;
  {$ENDIF}
begin
  {$IFDEF DELPHIXE10_LVL}
  Result := 1;
  frm := GetParentForm(AControl);
  if Assigned(frm) and (frm is TForm) and (frm as TForm).Scaled then
  begin
    Result := TForm(frm).Monitor.PixelsPerInch / TForm(frm).PixelsPerInch;
  end;
  {$ENDIF}

  {$IFNDEF DELPHIXE10_LVL}
  if Assigned(ACanvas) then
  begin
    Result := GetDeviceCaps(ACanvas.Handle, LOGPIXELSY) / 96;
  end
  else
  begin
    FHDC := GetDC(0);
    Result := GetDeviceCaps(FHDC, LOGPIXELSX) / 96;
    ReleaseDC(0, FHDC);
  end;
  {$ENDIF}
end;


procedure DrawPictureWithColor(g: TGPGraphics; Pic: TAdvGdipPicture; PicClr: TGPColor; DR: TRect);
var
  Attr: TGPImageAttributes;
  ColorMatrix: TColorMatrix;
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  RF: TGPRectF;
  CR: TRect;
  s: Integer;
  rc, gc, bc: Double;
  hr: HResult;

begin
  CR := DR;

  //InflateRect(CR, -2, -2);

  s := Min(CR.Right - CR.Left, CR.Bottom - CR.Top);
  RF := MakeRect(CR.Left + (CR.Right - CR.Left - s) div 2, CR.Top + (CR.Bottom - CR.Top - s) div 2, s, s);

  if not Assigned(Pic) or Pic.Empty then
    Exit;

  ms := TMemoryStream.Create;
  pic.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;
  // Create IStream* from global memory

  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      rc := ADVGDIP.GetRed(PicClr) / 255;
      gc := ADVGDIP.GetGreen(PicClr)/ 255;
      bc := ADVGDIP.GetBlue(PicClr)/ 255;

      // transformed image color
      FillChar(ColorMatrix, Sizeof(ColorMatrix), 0);

      ColorMatrix[3,3] := 1; // <- original A

      ColorMatrix[4,0] := rc; // <- desired R
      ColorMatrix[4,1] := gc; // <- desired G
      ColorMatrix[4,2] := bc; // <- desired B

      Attr := TGPImageAttributes.Create;
      Attr.SetColorMatrix(ColorMatrix);

      RF.X := DR.Left;
      RF.Y := DR.Top;
      RF.Width := Img.Width;
      RF.Height := Img.Height;

      g.DrawImage(img, RF, 0, 0, integer(Img.Width), integer(Img.Height), UnitPixel, Attr);

      Img.Free;
      attr.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
end;


{ TLabelSettings }

procedure TLabelSettings.Assign(Source: TPersistent);
begin
  if (Source is TLabelSettings) then
  begin
    FText := (Source as TLabelSettings).Text;
    FPicture.Assign((Source as TLabelSettings).Picture);
    FDisabledPicture.Assign((Source as TLabelSettings).DisabledPicture);
    FHoverPicture.Assign((Source as TLabelSettings).HoverPicture);
    FPictureColor := (Source as TLabelSettings).PictureColor;
    FFont.Assign((Source as TLabelSettings).Font);
    FHoverFontStyle := (Source as TLabelSettings).HoverFontStyle;
    FHoverFontColor := (Source as TLabelSettings).HoverFontColor;
  end;
end;

procedure TLabelSettings.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TLabelSettings.Create;
begin
  inherited;
  FText := '';
  FPicture := TAdvGdipPicture.Create;
  FPicture.OnChange := FontChanged;
  FDisabledPicture := TAdvGdipPicture.Create;
  FDisabledPicture.OnChange := FontChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FHoverFontStyle := [];
  FHoverFontColor := clNone;
  FHoverPicture := TAdvGdipPicture.Create;
  FHoverPicture.OnChange := FontChanged;
  FPictureColor := clNone;
end;

destructor TLabelSettings.Destroy;
begin
  FFont.Free;
  FPicture.Free;
  FHoverPicture.Free;
  FDisabledPicture.Free;
  inherited;
end;

procedure TLabelSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TLabelSettings.SetDisabledPicture(const Value: TAdvGdipPicture);
begin
  FDisabledPicture.Assign(Value);
end;

procedure TLabelSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TLabelSettings.SetHoverPicture(const Value: TAdvGdipPicture);
begin
  FHoverPicture.Assign(Value);
  Changed;
end;

procedure TLabelSettings.SetPicture(const Value: TAdvGdipPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

procedure TLabelSettings.SetPictureColor(const Value: TColor);
begin
  if (FPictureColor <> Value) then
  begin
    FPictureColor := Value;
    Changed;
  end;
end;

procedure TLabelSettings.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TAdvGraphicCheckLabel }

procedure TAdvGraphicCheckLabel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvGraphicCheckLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvGraphicCheckLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if FHasMouse then
  begin
    FHasMouse := false;
    Invalidate;
    Cursor := FOldCursor;
  end;
end;

constructor TAdvGraphicCheckLabel.Create(AOwner: TComponent);
begin
  inherited;
  FStateChecked := TLabelSettings.Create;
  FStateChecked.OnChange := SettingsChanged;
  FStateUnChecked := TLabelSettings.Create;
  FStateUnChecked.OnChange := SettingsChanged;
  FShowFocus := True;
  FReturnIsTab := False;
  FHasMouse := False;
  FChecked := False;
  Height := 20;
  Width := 80;
  FAlignment := taRightJustify;
  DoubleBuffered := true;
  Canvas.Brush.Style := bsClear;
  LoadPictureSet('CHECK', '','');

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvGraphicCheckLabel.CreateParams(var Params: TCreateParams);
begin
  inherited;

end;

procedure TAdvGraphicCheckLabel.CreateWnd;
begin
  inherited;
  if FDesignTime then
    FStateUnChecked.Text := Name;
end;

destructor TAdvGraphicCheckLabel.Destroy;
begin
  FStateChecked.Free;
  FStateUnChecked.Free;
  inherited;
end;

function TAdvGraphicCheckLabel.DoDraw(Canvas: TCanvas; ASettings: TLabelSettings; Measure: boolean): TSize;
var
  R: TRect;
  pic: TAdvGdipPicture;
  x,y,h: integer;
  gp: TGPGraphics;
  col: TGPColor;
  DPIScale: single;
begin
  Result.cx := 0;
  Result.cy := 0;
  h := 0;

  R := GetClientRect;
  DPIScale := GetDPIScale(Self, Canvas);

  pic := ASettings.Picture;

  if not Enabled and not ASettings.DisabledPicture.Empty then
    pic := ASettings.DisabledPicture;

  if Enabled and FHasMouse and Assigned(ASettings.HoverPicture) and not ASettings.HoverPicture.Empty then
    pic := ASettings.HoverPicture;

  if Assigned(pic) and not pic.Empty then
  begin
     if Alignment = taRightJustify then
       x := 0
     else
       x := r.Right - pic.Width;

    //cPicH := Round(DPIScale * pic.Height);
    //cPicW := Round(DPIScale * pic.Width);

    y := (r.Bottom - r.Top - pic.Height) div 2;
    // do color transform here?

    if not Measure then
    begin

      if ASettings.PictureColor <> clNone then
      begin
        gp := TGPGraphics.Create(Canvas.Handle);
        col := MakeColor(0,ColorToRGB(ASettings.PictureColor));
        DrawPictureWithColor(gp,pic,col,Rect(x,y,pic.Width,y + pic.Height));
        gp.Free;
      end
      else
      begin
        Canvas.Draw(x,y,pic);
      end;

    end;

    h := pic.Height;

   if Alignment = taRightJustify then
      R.Left := R.Left + pic.Width + 4
   else
      R.Right := R.Right - pic.Width - 4
  end;

  Canvas.Font.Assign(ASettings.Font);
  Canvas.Font.Height := Round(DPIScale * ASettings.Font.Height);
  Canvas.Font.Color := ASettings.Font.Color;

  if FHasMouse then
  begin
    Canvas.Font.Style := ASettings.HoverFontStyle;
    if ASettings.HoverFontColor <> clNone then
      Canvas.Font.Color := ASettings.HoverFontColor;
  end;

  Canvas.Brush.Style := bsClear;

  if not Measure then
    DrawText(Canvas.Handle, PChar(ASettings.Text),Length(ASettings.Text), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE)
  else
  begin
    R.Right := $FFFF;
    R.Bottom := $FFFF;
    Result.cy := DrawText(Canvas.Handle, PChar(ASettings.Text),Length(ASettings.Text), R, DT_LEFT or DT_TOP or DT_SINGLELINE or DT_CALCRECT);
    Result.cx := r.Right;
    Result.cy := Max(result.cy,h);
  end;

  if FShowFocus and (GetFocus = Handle) and not Measure then
  begin
    R.Left := R.Left - 2;
    Canvas.Brush.Style := bsSolid;
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

procedure TAdvGraphicCheckLabel.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TAdvGraphicCheckLabel.DoExit;
begin
  inherited;
  Invalidate;
end;

function TAdvGraphicCheckLabel.GetActiveRect(Canvas: TCanvas;
  ASettings: TLabelSettings): TRect;
var
  wp,h,hp,ht: integer;
  R: TRect;
begin
  Canvas.Font.Assign(ASettings.Font);

  R := ClientRect;
  h := R.Bottom - R.Top;
  hp := 0;
  wp := 0;

  if Assigned(ASettings.Picture) and not ASettings.Picture.Empty and (Alignment = taRightJustify) then
  begin
    hp := ASettings.Picture.Height;
    wp := ASettings.Picture.Width + 4;
  end;

  R.Bottom := 0;

  ht := DrawText(Canvas.Handle, PChar(ASettings.Text),Length(ASettings.Text), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_CALCRECT);

  ht := Max(ht,hp);
  ht := (h - ht) div 2;

  R.Bottom := R.Top + h - ht;
  R.Top := R.Top + ht;
  R.Right := R.Right + wp;
  Result := R;
end;

function TAdvGraphicCheckLabel.GetPicRect: TRect;
var
  R: TRect;
  wp,hp,h: integer;
begin
  R := ClientRect;
  h := R.Bottom - R.Top;
  hp := 0;
  wp := 0;

  if Assigned(FStateChecked.Picture) then
  begin
    hp := FStateChecked.Picture.Height;
    wp := FStateChecked.Picture.Width + 4;
  end;

  hp := (h - hp) div 2;

  R.Bottom := R.Top + h - hp;
  R.Top := R.Top + hp;
  R.Left := R.Right - wp;
  Result := R;
end;

function TAdvGraphicCheckLabel.GetTextRect: TRect;
begin
  if Checked then
    Result := GetActiveRect(Canvas, FStateChecked)
  else
    Result := GetActiveRect(Canvas, FStateUnChecked);
end;

function TAdvGraphicCheckLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvGraphicCheckLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvGraphicCheckLabel.InActiveRect(x, y: integer): boolean;
begin
  if Alignment = taRightJustify then
    Result := PtInRect(GetTextRect,Point(X,Y))
  else
  begin
    Result := PtInRect(GetTextRect,Point(X,Y)) or PtInRect(GetPicRect, Point(X,Y));
  end;
end;

procedure TAdvGraphicCheckLabel.KeyPress(var Key: Char);
begin
  inherited;
  if Enabled and (Key = #32) then
  begin
    Toggle;
    Click;
  end;

  if (Key = #13) then
    if FReturnIsTab  then
      PostMessage(Handle, WM_KEYDOWN, VK_TAB, 0)
    else
      if Enabled then
      begin
        Toggle;
        Click;
      end;
end;

procedure TAdvGraphicCheckLabel.Loaded;
begin
  inherited;
  if (FStateChecked.Text = '') then
    FStateChecked.Text := FStateUnChecked.Text;
end;

procedure TAdvGraphicCheckLabel.LoadPictureSet(AName: string; CaptionNormal, CaptionActive: string);
begin
  FStateUnChecked.Picture.LoadFromResourceName(Hinstance, 'GC_' + AName);
  FStateChecked.Picture.LoadFromResourceName(Hinstance, 'GC_' + AName+'_ACTIVE');
  if (CaptionNormal <> '') then
    FStateUnChecked.Text := CaptionNormal;
  if (CaptionActive <> '') then
    FStateChecked.Text := CaptionActive;
end;

procedure TAdvGraphicCheckLabel.LoadPredefined;
begin
  case Kind of
    ckCheck: LoadPictureSet('CHECK', '','');
    ckLike: LoadPictureSet('LIKE','Like','Unlike');
    ckAvailable: LoadPictureSet('AVAILABLE','Not available','Available');
    ckWish: LoadPictureSet('WISH','Add to wishlisht','In wishlist');
    ckBookmark: LoadPictureSet('BOOKMARK','Bookmark','Bookmarked');
    ckFavorite: LoadPictureSet('FAVORITE','Favorite','Favorited');
    ckInCart: LoadPictureSet('INCART','Add to cart','In cart');
    ckFollowing: LoadPictureSet('FOLLOWING','Follow','Following');
  end;
end;

procedure TAdvGraphicCheckLabel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if TabStop then
    SetFocus;
end;

procedure TAdvGraphicCheckLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if InActiveRect(X,Y)then
  begin
    if not FHasMouse then
    begin
      FHasMouse := true;
      Invalidate;
      FOldCursor := Cursor;
      Cursor := crHandPoint;
    end;
  end
  else
  begin
    if FHasMouse then
    begin
      FHasMouse := false;
      Invalidate;
      Cursor := FOldCursor;
    end;
  end;
end;

procedure TAdvGraphicCheckLabel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TAdvGraphicCheckLabel.Paint;
begin
//  inherited;
  if Checked then
    DoDraw(Canvas, FStateChecked, false)
  else
    DoDraw(Canvas, FStateUnChecked, false);
end;

procedure TAdvGraphicCheckLabel.SetAlignment(const Value: TLeftRight);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvGraphicCheckLabel.SetAutoSizeEx(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    UpdateSize;
  end;
end;

procedure TAdvGraphicCheckLabel.SetChecked(const Value: boolean);
begin
  if (FChecked <> Value) then
  begin
    FChecked := Value;
    Invalidate;
  end;
end;

procedure TAdvGraphicCheckLabel.SetEnabled(Value: Boolean);
begin
  inherited;
  Invalidate;
end;

procedure TAdvGraphicCheckLabel.SetKind(const Value: TCheckKind);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;

    if not ((csLoading in ComponentState) or (csReading in ComponentState)) then
    begin
      LoadPredefined;
    end;
  end;
end;

procedure TAdvGraphicCheckLabel.SetShowFocus(const Value: boolean);
begin
  if (FShowFocus <> Value) then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TAdvGraphicCheckLabel.SetStateChecked(const Value: TLabelSettings);
begin
  FStateChecked.Assign(Value);
end;

procedure TAdvGraphicCheckLabel.SetStateUnChecked(const Value: TLabelSettings);
begin
  FStateUnChecked.Assign(Value);
end;

procedure TAdvGraphicCheckLabel.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvGraphicCheckLabel.SetTransparent(const Value: boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TAdvGraphicCheckLabel.SetVersion(const Value: string);
begin
  // read only streamed property
end;

procedure TAdvGraphicCheckLabel.Toggle;
begin
  Checked := not Checked;
end;

procedure TAdvGraphicCheckLabel.UpdateSize;
var
  sz: TSize;
begin
  if AutoSize then
  begin
    sz := DoDraw(Canvas, FStateChecked, true);
    Width := sz.cx;
    Height := sz.cy;
  end;
end;

procedure TAdvGraphicCheckLabel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if FTransparent then
    Message.Result := 1
  else
    inherited;
end;

procedure TAdvGraphicCheckLabel.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if Enabled and (InActiveRect(Message.XPos, Message.YPos)) then
    Toggle;
  inherited;
end;

procedure TAdvGraphicCheckLabel.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  dbl: boolean;
  p: TPoint;
  i: integer;
begin
  if not FTransparent then
  begin
    inherited;
    Exit;
  end;

  if Assigned(Parent) then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      dbl := Parent.DoubleBuffered;
      {$IFDEF DELPHI_UNICODE}
      if (Parent is TCustomForm) then
      {$ENDIF}
        Parent.DoubleBuffered := false;
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      Parent.DoubleBuffered := dbl;
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, 0);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;


end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.
