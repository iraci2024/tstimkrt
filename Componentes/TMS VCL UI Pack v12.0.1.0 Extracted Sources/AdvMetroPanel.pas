{***************************************************************************}
{ TAdvMetroPanel component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014 - 2020                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvMetroPanel;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Types, Graphics, SysUtils, Controls, AdvStyleIF,
  ImgList, PictureContainer
  {$IFDEF DELPHIXE2_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  DEFAULT_CAPTIONHEIGHT = 26;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Design-time issue when resizing and copy & paste



type
  TAnchorClick = procedure(Sender: TObject; Text: string) of object;
  TCaptionAlignment = (caLeft, caCenter, caRight);

  TMetroPanelAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBackgroundTextColor: TColor;
    FBackgroundColor: TColor;
    FTextColor: TColor;
    FColor: TColor;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBackgroundTextColor(const Value: TColor);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledTextColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default $00B0A374;
    property TextColor: TColor read FTextColor write SetTextColor default clWhite;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property BackgroundTextColor: TColor read FBackgroundTextColor write SetBackgroundTextColor default clBlack;

    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGray;
  end;

  TAdvMetroPanel = class(TCustomControl, ITMSTones)
  private
    FCaptionRect: TRect;
    FCaptionImageRect: TRect;
    FCaptionTextRect: TRect;
    FHTMLRect: TRect;
    FHTMLText: TStringList;
    FImages: TCustomImageList;
    FOnAnchorClick: TAnchorClick;
    FPanelRect: TRect;
    FPictureContainer: TPictureContainer;
    FAppearance: TMetroPanelAppearance;
    FCaption: TCaption;
    FCaptionAlign: TCaptionAlignment;
    FCaptionImageIndex: Integer;
    function AnchorTextAt(X, Y: Integer): string;
    procedure AppearanceChanged(Sender: TObject);
    procedure CalculateRects;
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure SetHTMLText(const Value: TStringList);
    procedure SetImages(const Value: TCustomImageList);
    procedure setPictureContainer(const Value: TPictureContainer);
    procedure SetVersion(const Value: string);
    procedure SetAppearance(const Value: TMetroPanelAppearance);
    procedure SetCaption(const Value: TCaption);
    procedure SetCaptionAlign(const Value: TCaptionAlignment);
    procedure SetCaptionImageIndex(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColorTones(ATones: TColorTones);
  published
    property Align;
    property Anchors;
    property Appearance: TMetroPanelAppearance read FAppearance write SetAppearance;
    property Caption: TCaption read FCaption write SetCaption;
    property CaptionAlign: TCaptionAlignment read FCaptionAlign write SetCaptionAlign default caCenter;
    property CaptionImageIndex: Integer read FCaptionImageIndex write SetCaptionImageIndex default 0;
    property Constraints;
    property Cursor;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height default 200;
    property HTMLText: TStringList read FHTMLText write SetHTMLText;
    property Images: TCustomImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FPictureContainer write setPictureContainer;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Top;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property Width default 300;

    property OnAnchorClick: TAnchorClick read FOnAnchorClick write FOnAnchorClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    {$IFDEF DELPHI2007_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  ComObj, Commctrl, ShellApi;

{$I HTMLEngo.pas}

{ TAdvMetroPanel }

function TAdvMetroPanel.AnchorTextAt(X, Y: Integer): string;
var
  xs, ys, HyperLinks, MouseLink: Integer;
  a, s, str, focusanchor: string;
  res: boolean;
  r: TRect;
begin
  Result := '';
  a := '';

  str := FHTMLText.Text;

  res := HTMLDrawEx(Canvas, str, FHTMLRect, FImages, X, Y, -1, -1, 1, True, False, False, False, False, False, True, True, 1.0,
           clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys, HyperLinks, MouseLink, r, nil, FPictureContainer, 0);

  if res then
    Result := a;
end;

procedure TAdvMetroPanel.AppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvMetroPanel.CalculateRects;
var
  bm: TBitmap;
begin
  FCaptionRect.Left := 0;
  FCaptionRect.Right := Width;
  FCaptionRect.Top := 0;
  FCaptionRect.Bottom := DEFAULT_CAPTIONHEIGHT;

  FCaptionImageRect.Right := 0;
  if Assigned(FImages) and (CaptionImageIndex < FImages.Count) and (FCaptionImageIndex >= 0) then
  begin
    bm := TBitmap.Create;
    try
      FImages.GetBitmap(FCaptionImageIndex, bm);
      FCaptionImageRect.Left := 5;
      FCaptionImageRect.Right := bm.Width + 5;
      FCaptionImageRect.Top := (DEFAULT_CAPTIONHEIGHT - bm.Height) div 2;
      FCaptionImageRect.Bottom := DEFAULT_CAPTIONHEIGHT - (DEFAULT_CAPTIONHEIGHT - bm.Height) div 2;
    finally
      bm.Free;
    end;
  end;

  FCaptionTextRect.Left := FCaptionImageRect.Right + 5;
  FCaptionTextRect.Right := Width - 5;
  FCaptionTextRect.Top := 0;
  FCaptionTextRect.Bottom := DEFAULT_CAPTIONHEIGHT;

  FPanelRect.Top := FCaptionRect.Bottom;
  FPanelRect.Bottom := Height;
  FPanelRect.Left := 0;
  FPanelRect.Right := Width;

  FHTMLRect.Left := 5;
  FHTMLRect.Right := Width - 5;
  FHTMLRect.Top := FPanelRect.Top + 5;
  FHTMLRect.Bottom := FPanelRect.Bottom - 5;
end;

constructor TAdvMetroPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  DoubleBuffered := True;
  FHTMLText := TStringList.Create;
  FHTMLText.OnChange := AppearanceChanged;
  Caption := 'Caption';
  FCaptionAlign := caCenter;
  Width := 300;
  Height := 200;
  FCaptionRect.Left := 0;
  FCaptionRect.Right := 300;
  FCaptionRect.Top := 0;
  FCaptionRect.Bottom := 26;

  FPanelRect.Top := 26;
  FPanelRect.Bottom := 200;
  FPanelRect.Left := 0;
  FPanelRect.Right := 300;

  FCaptionTextRect.Left := 5;
  FCaptionTextRect.Right := 295;
  FCaptionTextRect.Top := 0;
  FCaptionTextRect.Bottom := 26;

  FHTMLRect.Left := 5;
  FHTMLRect.Right := 295;
  FHTMLRect.Top := 31;
  FHTMLRect.Bottom := 195;

  FAppearance := TMetroPanelAppearance.Create;
  FAppearance.FOnChange := AppearanceChanged;
end;

procedure TAdvMetroPanel.CreateWnd;
begin
  inherited;
  CalculateRects;
end;

destructor TAdvMetroPanel.Destroy;
begin
  FAppearance.Destroy;
  FHTMLText.Destroy;
  inherited;
end;

function TAdvMetroPanel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvMetroPanel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvMetroPanel.Loaded;
begin
  inherited;
  CalculateRects;
end;

procedure TAdvMetroPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if AnchorTextAt(X, Y) <> '' then
  begin
    if Assigned(FOnAnchorclick) then
      FOnAnchorclick(Self, AnchorTextAt(X, Y));
  end;
end;

procedure TAdvMetroPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
   if AnchorTextAt(X, Y) <> '' then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TAdvMetroPanel.Paint;
var
  r, rout: TRect;
  text, a, s, focusAnchor: string;
  xs, ys, HyperLinks, MouseLink: Integer;
begin
  if not Enabled then
  begin
    r := FCaptionRect;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Appearance.DisabledColor;
    Canvas.Pen.Color := Appearance.DisabledColor;
    Canvas.FillRect(r);
    r := FCaptionImageRect;
    if Assigned(FImages) and (CaptionImageIndex < FImages.Count) and (FCaptionImageIndex >= 0) then
    begin
      FImages.Draw(Canvas, r.Left, r.Top, FCaptionImageIndex);
    end;
    Canvas.Font.Color := Appearance.DisabledTextColor;
    text := Caption;
    r := FCaptionTextRect;

    {$IFNDEF DELPHIXE_LVL}
    Canvas.TextOut(r.Left, r.Top, text);
    {$ENDIF}
    {$IFDEF DELPHIXE_LVL}
    case FCaptionAlign of
      caLeft: Canvas.TextRect(r, text, [tfLeft, tfSingleLine, tfVerticalCenter]);
      caCenter: Canvas.TextRect(r, text, [tfCenter, tfSingleLine, tfVerticalCenter]);
      caRight: Canvas.TextRect(r, text, [tfRight, tfSingleLine, tfVerticalCenter]);
    end;
    {$ENDIF}

    Canvas.Brush.Color := Appearance.BackgroundColor;
    r := FPanelRect;
    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
    Canvas.Font.Color := Appearance.BackgroundTextColor;
    r := FHTMLRect;
    HTMLDrawEx(Canvas, FHTMLText.Text, r, FImages, -1, -1, -1, -1, 1, False, False, False, False, False, False, True, True, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys , HyperLinks, MouseLink, rout, nil, FPictureContainer, 0);
  end
  else 
  begin
    r := FCaptionRect;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Appearance.Color;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Appearance.Color;
    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
    r := FCaptionImageRect;
    if Assigned(FImages) and (CaptionImageIndex < FImages.Count) and (FCaptionImageIndex >= 0) then
    begin
      FImages.Draw(Canvas, r.Left, r.Top, FCaptionImageIndex);
    end;
    Canvas.Font.Color := Appearance.TextColor;
    text := Caption;
    r := FCaptionTextRect;
    {$IFNDEF DELPHIXE_LVL}
    Canvas.TextOut(r.Left, r.Top, text);
    {$ENDIF}
    {$IFDEF DELPHIXE_LVL}
    case FCaptionAlign of
      caLeft: Canvas.TextRect(r, text, [tfLeft, tfSingleLine, tfVerticalCenter]);
      caCenter: Canvas.TextRect(r, text, [tfCenter, tfSingleLine, tfVerticalCenter]);
      caRight: Canvas.TextRect(r, text, [tfRight, tfSingleLine, tfVerticalCenter]);
    end;
    {$ENDIF}
    Canvas.Brush.Color := Appearance.BackgroundColor;
    r:= FPanelRect;
    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
    Canvas.Font.Color := Appearance.BackgroundTextColor;
    r := FHTMLRect;
    HTMLDrawEx(Canvas, FHTMLText.Text, r, FImages, -1, -1, -1, -1, 1, False, False, False, False, False, False, True, True, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys , HyperLinks, MouseLink, rout, nil, FPictureContainer, 0);
  end;
end;

procedure TAdvMetroPanel.Resize;
begin
  inherited;
  CalculateRects;
  Invalidate;
end;

procedure TAdvMetroPanel.SetAppearance(const Value: TMetroPanelAppearance);
begin
  FAppearance.Assign(Value);
  Invalidate;
end;

procedure TAdvMetroPanel.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvMetroPanel.SetCaptionAlign(const Value: TCaptionAlignment);
begin
  if FCaptionAlign <> Value then
  begin
    FCaptionAlign := Value;
    Invalidate;
  end;
end;

procedure TAdvMetroPanel.SetCaptionImageIndex(const Value: Integer);
begin
  if FCaptionImageIndex <> Value then
  begin
    FCaptionImageIndex := Value;
    CalculateRects;
    Invalidate;
  end;
end;

procedure TAdvMetroPanel.SetColorTones(ATones: TColorTones);
begin
  Appearance.BackgroundColor := ATones.Background.BrushColor;
  Appearance.BackgroundTextColor := ATones.Background.TextColor;
  Appearance.Color := ATones.Selected.BrushColor;
  Appearance.TextColor := ATones.Selected.TextColor;
  Appearance.DisabledColor := ATones.Disabled.BrushColor;
  Appearance.DisabledTextColor := ATones.Disabled.TextColor;

  Invalidate;
end;

procedure TAdvMetroPanel.SetHTMLText(const Value: TStringList);
begin
  FHTMLText.Assign(Value);
  CalculateRects;
  Invalidate;
end;

procedure TAdvMetroPanel.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TAdvMetroPanel.setPictureContainer(const Value: TPictureContainer);
begin
  FPictureContainer := Value;
  Invalidate;
end;

procedure TAdvMetroPanel.SetVersion(const Value: string);
begin

end;

{ TMetroPanelAppearance }

procedure TMetroPanelAppearance.Assign(Source: TPersistent);
begin
  if (Source is TMetroPanelAppearance) then
  begin
    FColor := (Source as TMetroPanelAppearance).Color;
    FTextColor := (Source as TMetroPanelAppearance).TextColor;

    FDisabledColor := (Source as TMetroPanelAppearance).DisabledColor;
    FDisabledTextColor := (Source as TMetroPanelAppearance).DisabledTextColor;

    FBackgroundColor := (Source as TMetroPanelAppearance).BackgroundColor;
    FBackgroundTextColor := (Source as TMetroPanelAppearance).BackgroundTextColor;
  end
  else
    inherited;
end;

procedure TMetroPanelAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TMetroPanelAppearance.Create;
begin
  inherited;
  FColor := $00B0A374;
  FTextColor := clWhite;
  FBackgroundColor := clWhite;
  FBackgroundTextColor := clBlack;
  FDisabledColor := clSilver;
  FDisabledTextColor := clGray;
end;

procedure TMetroPanelAppearance.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TMetroPanelAppearance.SetBackgroundTextColor(const Value: TColor);
begin
  if FBackgroundTextColor <> Value then
  begin
    FBackgroundTextColor := Value;
    Changed;
  end;
end;

procedure TMetroPanelAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TMetroPanelAppearance.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TMetroPanelAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    Changed;
  end;
end;

procedure TMetroPanelAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

end.
