{***************************************************************************}
{ TAdvToolTip component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2021                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : https://www.tmssoftware.com                              }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvToolTip;

{$I TMSDEFS.INC}
{$M+}

interface

uses
  Windows, Classes, Messages, Controls, Graphics, StdCtrls, Buttons, ImgList, Forms, Types
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 3; // Build nr.

  // Version history
  // 1.0.0.0  : First release
  // 1.0.0.1  : Improved : Rendering when large icons are used
  // 1.0.0.2  : Improved : Rendering in Shape tsRectangle
  // 1.0.0.3  : Fixed : Issue with restoring control border color

type

  TAdvToolTipPosition = (ttTopRight, ttTopLeft, ttTopCenter, ttRight, ttBottomLeft, ttBottomCenter, ttBottomRight, ttCustom);

  TAdvToolTipShape = (tsBalloon, tsRectangle);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvToolTip = class(TComponent)
  private
    FBorderColor: TColor;
    FImages: TCustomImageList;
    FMaxWidth: integer;
    FColor: TColor;
    FControlBorderColor: TColor;
    FFont: TFont;
    FX: integer;
    FY: integer;
    FImageIndex: TImageIndex;
    FPosition: TAdvToolTipPosition;
    FRounding: integer;
    FShape: TAdvToolTipShape;
    FMargin: integer;
    procedure SetFont(const Value: TFont);
    function GetVersion: string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: integer;
  published
    property ControlBorderColor: TColor read FControlBorderColor write FControlBorderColor default clRed;
    property BorderColor: TColor read FBorderColor write FBorderColor default clRed;
    property Color: TColor read FColor write FColor default clRed;
    property Font: TFont read FFont write SetFont;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
    property Images: TCustomImageList read FImages write FImages;
    property Margin: integer read FMargin write FMargin default 0;
    property MaxWidth: integer read FMaxWidth write FMaxWidth default 0;
    property Position: TAdvToolTipPosition read FPosition write FPosition default ttRight;
    property Rounding: integer read FRounding write FRounding default 2;
    property Shape: TAdvToolTipShape read FShape write FShape default tsBalloon;
    property X: integer read FX write FX default 0;
    property Y: integer read FY write FY default 0;
    property Version: string read GetVersion;
  end;

  TAdvToolTipWindow = class(TCustomControl)
  private
    FInternalToolTip: TAdvToolTip;
    FControl: TControl;
    FColor: TColor;
    FText: string;
    FToolTip: TAdvToolTip;
    FBorderSet: boolean;
    FOldBorderColor: TColor;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetColor(const Value: TColor);
    procedure SetText(const Value: string);
    function ToolTipInfo: TAdvToolTip;
  protected
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure DoSetRegion;
    procedure DoMeasure; virtual;
    procedure DoPosition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(Control: TControl; ToolTip: TAdvToolTip = nil);
    procedure Hide;
  published
    property Color: TColor read FColor write SetColor default clRed;
    property Font;
    property Text: string read FText write SetText;
    property OnClick;
    property Visible;
  end;

  TAdvControlValidator = class(TCollectionItem)
  private
    FControl: TControl;
    FTag: integer;
    FText: string;
  public
  published
    property Control: TControl read FControl write FControl;
    property Text:  string read FText write FText;
    property Tag: integer read FTag write FTag;
  end;

  TAdvControlValidators = class(TOwnedCollection)
  end;

  TAdvFormValidators = class(TComponent)
  private
    FValidators: TAdvControlValidators;
    FToolTip: TAdvToolTip;
    procedure SetValidators(const Value: TAdvControlValidators);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ToolTip: TAdvToolTip read FToolTip write FToolTip;
    property Validators: TAdvControlValidators read FValidators write SetValidators;
  end;


procedure ShowValidator(AControl: TControl; AText: string; AToolTip: TAdvToolTip = nil);
procedure HideValidator(AControl: TControl);


implementation

uses
  AdvGDIP, SysUtils, TypInfo;

const
  BalloonSize = 8;

var
  ControlList: TList;

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TValidator = class(TObject)
  private
    FControl: TControl;
    FToolTipWindow: TAdvToolTipWindow;
  published
    property Control: TControl read FControl write FControl;
    property ToolTipWindow: TAdvToolTipWindow read FToolTipWindow write FToolTipWindow;
  end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvBadge }

procedure TAdvToolTipWindow.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TAdvToolTipWindow.Create(AOwner: TComponent);
begin
  inherited;
  Width := 20;
  Height := 20;
  FColor := clRed;
  Text := '1';
  Font.Color := clWhite;
  DoubleBuffered := true;
  Visible := false;
  FInternalToolTip := TAdvToolTip.Create(Self);
  FOldBorderColor := clNone;
  FBorderSet := false;
  FControl := nil;
end;

procedure TAdvToolTipWindow.CreateWnd;
begin
  inherited;
  DoMeasure;
end;

destructor TAdvToolTipWindow.Destroy;
begin
  if Assigned(FControl) and (FBorderSet) then
  begin
    SetOrdProp(FControl,'BorderColor', FOldBorderColor);
    FControl.Invalidate;
  end;

  inherited;
end;

procedure TAdvToolTipWindow.DoMeasure;
var
  graphics: TGPGraphics;
  gpfont: TGPFont;
  sf: TGPStringFormat;
  layoutr, sizer: TGPRectF;
  NewWidth, NewHeight: integer;
  s: string;
  tt: TAdvToolTip;
begin
  if HandleAllocated then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    gpfont := graphics.MakeFont(ToolTipInfo.Font);

    sf := TGPStringFormat.Create(0);

    s := ' ' + Text + ' ';

    tt := ToolTipInfo;

    if tt.MaxWidth > 0 then
    begin
      layoutr.X := 0;
      layoutr.Y := 0;
      layoutr.Width := tt.MaxWidth;
      layoutr.Height := 1000;
      graphics.MeasureString(s, Length(s), gpFont, layoutr, sf, sizer)
    end
    else
      graphics.MeasureString(s, Length(s), gpFont, MakePointF(0, 0), sf, sizer);

    NewWidth := Round(sizer.Width + 4 + tt.Margin * 2);
    NewHeight := Round(sizer.Height + sizer.Height/5 + tt.Margin * 2);

    if NewWidth < NewHeight then
      NewWidth := NewHeight;

    if (tt.Shape = tsBalloon) and not (tt.Position = ttRight) then
      NewHeight := NewHeight + BALLOONSIZE;

    if (tt.Shape = tsBalloon) and (tt.Position = ttRight) then
      NewWidth := NewWidth + BALLOONSIZE;

    if (tt.ImageIndex >= 0) and Assigned(tt.Images) then
    begin
      NewWidth := NewWidth + tt.Images.Width;
      if NewHeight < tt.Images.Height + 6 then
        NewHeight := tt.Images.Height + 6;
    end;

    sf.Free;
    gpFont.Free;
    graphics.Free;

    if (NewWidth <> Width) or (NewHeight <> Height) then
    begin
      Width := NewWidth;
      Height := NewHeight;
    end;

    DoSetRegion;
    DoPosition;
  end;
end;

procedure TAdvToolTipWindow.DoPosition;
var
  tt: TAdvToolTip;
  p: TControl;
  pt: TPoint;
begin
  if Assigned(FControl) then
  begin
    tt := ToolTipInfo;

    p := FControl;
    pt := Point(0,0);

    repeat
       pt.X := pt.X + p.Left;
       pt.Y := pt.Y + p.Top;
      p := p.Parent;
    until (p is TCustomForm);

    if tt.Position = ttCustom then
    begin
      Top := pt.X + tt.X;
      Left := pt.Y + tt.Y;
    end;

    if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
    begin
      Top := pt.Y + FControl.Height;
    end;

    if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
    begin
      Top := pt.Y - Height;
    end;

    if tt.Position in [ttRight] then
    begin
      Top := pt.Y + (FControl.Height - Height) div 2;
      Left := pt.X + FControl.Width;
    end;

    if tt.Position in [ttBottomLeft, ttTopLeft] then
    begin
      Left := pt.X;
    end;

    if tt.Position in [ttBottomRight, ttTopRight] then
    begin
      Left := pt.X + FControl.Width - Width;
    end;

    if tt.Position in [ttBottomCenter, ttTopCenter] then
    begin
      Left := pt.X + (FControl.Width - Width) div 2;
    end;

  end;
end;

procedure TAdvToolTipWindow.DoSetRegion;
var
  wrgn, rgn, arrrgn: THandle;
  pts: array[0..3] of TPoint;
  tt: TAdvToolTip;
  bst,bsb,bsr: integer;
  x,y: integer;
begin

  SetWindowRgn(Handle, 0, false);

  bsb := 0;
  bst := 0;
  bsr := 0;

  tt := ToolTipInfo;
  if tt.Shape = tsBalloon then
  begin
    if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
      bsb := BALLOONSIZE;

    if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
      bst := BALLOONSIZE;

    if tt.Position in [ttRight] then
      bsr := BALLOONSIZE;
  end;

  rgn := CreateRoundRectRgn(bsr, bsb, Width + 1 + bsr, Height + 1 + bsb - bst, tt.Rounding, tt.Rounding);

  if tt.Shape = tsBalloon then
  begin
    x := 0;
    y := 0;

    if tt.Position in [ttTopLeft, ttBottomLeft] then
      x := BALLOONSIZE;

    if tt.Position in [ttTopCenter, ttBottomCenter] then
      x := Width div 2;

    if tt.Position in [ttTopRight, ttBottomRight] then
      x := Width - 2 * BALLOONSIZE;

    if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
      y := Height - BALLOONSIZE;

    if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
      y := 0;

    if tt.Position = ttRight then
    begin
      pts[0].X := 0;
      pts[0].Y := Height div 2;

      pts[1].X := BALLOONSIZE + 2;
      pts[1].Y := Height div 2 - BALLOONSIZE div 2  - 1;

      pts[2].X := BALLOONSIZE + 2;
      pts[2].Y := Height div 2 + BALLOONSIZE div 2 + 1;

      pts[3].X := 0;
      pts[3].Y := 0;
    end
    else
    begin
      pts[0].X := x;
      pts[0].Y := y;

      pts[1].X := x;
      pts[1].Y := y + BALLOONSIZE;

      pts[2].X := x + BALLOONSIZE;

      if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
        pts[2].Y := y + BALLOONSIZE
      else
        pts[2].Y := y;

      pts[3].X := 0;
      pts[3].Y := 0;
    end;

    arrrgn := CreatePolygonRgn(pts, 3, WINDING);

    wrgn := CreateRectRgn(0, 0, 0, 0);
    CombineRgn(wrgn, rgn, arrrgn, RGN_OR );
    SetWindowRgn(Handle, wrgn, true);
    DeleteObject(wrgn);
    DeleteObject(arrrgn);
  end
  else
    SetWindowRgn(Handle, rgn, true);

  DeleteObject(rgn);
end;

procedure TAdvToolTipWindow.Hide;
begin

end;

procedure DrawRoundRectangle(graphic: TGPGraphics; R: TRect; Radius: Integer; Clr: TColor; BorderColor: TColor);
var
  path: TGPGraphicsPath;
  l, t, w, h, d: Integer;
  gpbrush: TGPBrush;
  gppen: TGPPen;
begin
  if not Assigned(graphic) then
    Exit;

  path := TGPGraphicsPath.Create;
  try
    l := R.Left;
    t := R.Top;
    w := R.Right;
    h := R.Bottom;
    d := Radius shl 1;
    path.AddArc(l, t, d, d, 180, 90); // topleft
    path.AddLine(l + radius, t, l + w - radius, t); // top
    path.AddArc(l + w - d, t, d, d, 270, 90); // topright
    path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
    path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
    path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
    path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
    path.AddLine(l, t + h - radius, l, t + radius); // left
    path.CloseFigure();

    gpbrush := TGPSolidBrush.Create(MakeColor(255, Clr));
    gppen := nil;
    if (BorderColor <> clNone) and (BorderColor <> Clr) then
      gppen := TGPPen.Create(MakeColor(255,BorderColor));

    try
      graphic.FillPath(gpbrush,path);

      if Assigned(gppen) then
        graphic.DrawPath(gppen, path);
    finally
      if Assigned(gppen) then
        gppen.Free;
      gpbrush.Free;
    end;

  finally
    path.Free;
  end;
end;

procedure TAdvToolTipWindow.Paint;
var
  graphics: TGPGraphics;
  gpbrush: TGPBrush;
  gpfont: TGPFont;
  gppen: TGPPen;
  dr: TGPRectF;
  stringformat: TGPStringFormat;
  tt: TAdvToolTip;
  bsb, bst, bsr, bs: integer;

begin
  tt := ToolTipInfo;

  bsb := 0;
  bst := 0;
  bsr := 0;
  bs := 0;

  if tt.Shape = tsBalloon then
  begin
    bs := BALLOONSIZE;
    if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
      bst :=  BALLOONSIZE;

    if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
      bsb := BALLOONSIZE;

    if tt.Position in [ttRight] then
      bsr := BALLOONSIZE;
  end;

  graphics := TGPGraphics.Create(Canvas.Handle);

  try
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    DrawRoundRectangle(graphics, Rect(1 + bsr,1 + bst,Width - 2 - bsr, Height - 2 - bs + bsr), tt.Rounding, tt.Color, tt.BorderColor);

    if tt.Shape = tsBalloon then
    begin
      gpbrush := TGPSolidBrush.Create(MakeColor(255, tt.Color));

      dr.Height := BALLOONSIZE;
      dr.Width := BALLOONSIZE;

      if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
      begin
        case tt.Position of
          ttBottomLeft: dr.X := BALLOONSIZE;
          ttBottomCenter: dr.X := Width div 2;
          ttBottomRight: dr.X := Width - BALLOONSIZE * 2;
        end;

        dr.Y := 0;
        dr.Height := BALLOONSIZE + 2;
      end;

      if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
      begin
        case tt.Position of
          ttTopLeft: dr.X := BALLOONSIZE;
          ttTopCenter: dr.X := Width div 2;
          ttTopRight: dr.X := Width - BALLOONSIZE * 2;
        end;

        dr.Y := Height - BALLOONSIZE - 2;
      end;

      if tt.Position in [ttRight] then
      begin
        dr.X := 0;
        dr.Y := (Height - BALLOONSIZE) div 2;
        dr.Width := BALLOONSIZE + 2;
        dr.Height := BALLOONSIZE;
      end;

      graphics.FillRectangle(gpbrush, dr);
      gpbrush.Free;

      if (tt.BorderColor <> tt.Color) and (tt.BorderColor <> clNone) then
      begin
        gppen := TGPPen.Create(MakeColor(255, tt.BorderColor));
        try
          if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
          begin
            graphics.DrawLine(gppen,dr.X, dr.Y + 1, dr.X, dr.Y + dr.Height - 1);
            graphics.DrawLine(gppen,dr.X + BALLOONSIZE, dr.Y + 1, dr.X, dr.Y + dr.Height - 1);
          end;

          if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
          begin
            graphics.DrawLine(gppen,dr.X, dr.Y, dr.X, dr.Y + dr.Height - 1);
            graphics.DrawLine(gppen,dr.X, dr.Y, dr.X + BALLOONSIZE, dr.Y + dr.Height - 1);
          end;

          if tt.Position in [ttRight] then
          begin
            graphics.DrawLine(gppen,dr.X, Height div 2, dr.X + BALLOONSIZE + 1, Height div 2 - BALLOONSIZE div 2);
            graphics.DrawLine(gppen,dr.X, Height div 2, dr.X + BALLOONSIZE + 1, Height div 2 + BALLOONSIZE div 2);
          end;

        finally
          gppen.Free;
        end;
      end;
    end;

    gpbrush := TGPSolidBrush.Create(MakeColor(255, tt.Font.Color));

    gpfont := graphics.MakeFont(tt.Font);

    stringFormat := TGPStringFormat.Create(0);

    try
      if tt.Position in [ttTopLeft, ttTopCenter, ttTopRight] then
      begin
        dr.X := 0;
        dr.Y := 0;
        dr.Width := Width;
        dr.Height := Height - bsb;
      end;

      if tt.Position in [ttBottomLeft, ttBottomCenter, ttBottomRight] then
      begin
        dr.X := 0;
        dr.Y := bst;
        dr.Width := Width;
        dr.Height := Height - bst;
      end;

      if tt.Position in [ttRight] then
      begin
        dr.X := bsr;
        dr.Y := 0;
        dr.Width := Width - bsr;
        dr.Height := Height;
      end;

      if (tt.ImageIndex >= 0) and Assigned(tt.Images) then
      begin
        tt.Images.Draw(Canvas, Round(dr.X + 4), Round(dr.Y + 4), tt.ImageIndex);
        dr.X := dr.X + tt.Images.Width;
        dr.Width := dr.Width - tt.Images.Width;
      end;

      stringFormat.SetAlignment(StringAlignmentCenter);
      stringFormat.SetLineAlignment(StringAlignmentCenter);

      graphics.DrawString(Text, Length(Text), gpFont, dr, stringformat, gpbrush);

    finally
      stringformat.Free;
      gpbrush.Free;
      gpfont.Free;
    end;

  finally
    graphics.Free;
  end;
end;

procedure TAdvToolTipWindow.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TAdvToolTipWindow.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;

    DoMeasure;

    Invalidate;
  end;
end;

procedure TAdvToolTipWindow.Show(Control: TControl; ToolTip: TAdvToolTip = nil);
var
  tt: TAdvToolTip;
begin
  FToolTip := ToolTip;
  FControl := Control;
  if Assigned(Control) then
  begin
    if not Assigned(Parent) then
      Parent := GetParentForm(Control);

    DoMeasure;

    tt := ToolTipInfo;

    if (tt.BorderColor <> clNone) and IsPublishedProp(FControl,'BorderColor') and not Visible then
    begin
      FBorderSet := true;
      FOldBorderColor := GetOrdProp(FControl,'BorderColor');
      SetOrdProp(FCOntrol, 'BorderColor',tt.BorderColor);
    end;

    Visible := true;
  end;
end;

function TAdvToolTipWindow.ToolTipInfo: TAdvToolTip;
begin
  if Assigned(FToolTip) then
    Result := FToolTip
  else
    Result := FInternalToolTip;
end;

procedure TAdvToolTipWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvToolTipWindow.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  dbl: boolean;
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) then
  begin
    DC := Message.DC;
    if (DC <> 0) then
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


{ TAdvToolTip }

procedure TAdvToolTip.Assign(Source: TPersistent);
begin
  if (Source is TAdvToolTip) then
  begin
    FControlBorderColor := (Source as TAdvToolTip).ControlBorderColor;
    FBorderColor := (Source as TAdvToolTip).BorderColor;
    FColor := (Source as TAdvToolTip).Color;
    FFont.Assign((Source as TAdvToolTip).Font);
    FImageIndex := (Source as TAdvToolTip).ImageIndex;
    FImages := (Source as TAdvToolTip).Images;
    FMargin := (Source as TAdvToolTip).Margin;
    FMaxWidth := (Source as TAdvToolTip).MaxWidth;
    FPosition := (Source as TAdvToolTip).Position;
    FRounding := (Source as TAdvToolTip).Rounding;
    FShape := (Source as TAdvToolTip).Shape;
    FX := (Source as TAdvToolTip).X;
    FY := (Source as TAdvToolTip).Y;
  end;
end;

constructor TAdvToolTip.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clRed;
  FControlBorderColor := clRed;
  FBorderColor := clRed;
  FX := 0;
  FY := 0;
  FFont := TFont.Create;
  FFont.Color := clWhite;
  FImageIndex := -1;
  FImages := nil;
  FRounding := 2;
  FShape := tsBalloon;
  FPosition := ttRight;
end;

destructor TAdvToolTip.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TAdvToolTip.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvToolTip.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvToolTip.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TAdvToolTip.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;


procedure ShowControlToolTip(AText: string; AControl: TControl; AToolTip: TAdvToolTip);
begin

end;

{ TAdvFormValidators }

constructor TAdvFormValidators.Create(AOwner: TComponent);
begin
  inherited;
  FValidators := TAdvControlValidators.Create(Self, TAdvControlValidator);
end;

destructor TAdvFormValidators.Destroy;
begin
  FValidators.Free;
  inherited;
end;

procedure TAdvFormValidators.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FToolTip) then
    FToolTip := nil;

  if (Operation = opRemove)  then
  begin
    for i := 0 to FValidators.Count - 1 do
    begin
      if (FValidators.Items[i] as TAdvControlValidator).Control = AComponent then
        (FValidators.Items[i] as TAdvControlValidator).Control := nil;
    end;
  end;

end;

procedure TAdvFormValidators.SetValidators(const Value: TAdvControlValidators);
begin
  FValidators.Assign(Value);
end;


procedure ShowValidator(AControl: TControl; AText: string; AToolTip: TAdvToolTip = nil);
var
  v: TValidator;
  i: integer;
begin

  v := nil;
  for i := 0 to ControlList.Count - 1 do
  begin
    if TValidator(ControlList.Items[i]).Control = AControl then
    begin
      v := TValidator(ControlList.Items[i]);
    end;
  end;

  if not Assigned(v) then
  begin
    v := TValidator.Create;
    v.ToolTipWindow := TAdvToolTipWindow.Create(AControl);
    v.Control := AControl;
    ControlLIst.Add(v);
  end;

  v.ToolTipWindow.Text := AText;
  v.ToolTipWindow.Show(AControl, AToolTip);
end;

procedure HideValidator(AControl: TControl);
var
  i: integer;
begin
  for i := ControlList.Count - 1 downto 0 do
  begin
    if TValidator(ControlList.Items[i]).Control = AControl then
    begin
      TValidator(ControlList.Items[i]).ToolTipWindow.Free;
      TValidator(ControlList.Items[i]).Free;

      ControlList.Delete(i);
    end;
  end;
end;

procedure DestroyValidators;
var
  i: integer;
begin
  for i := 0 to ControlList.Count - 1 do
  begin
    TValidator(ControlList.Items[i]).Free;
  end;
end;

initialization
  ControlList := TList.Create;


finalization
  DestroyValidators;
  ControlList.Free;

end.
