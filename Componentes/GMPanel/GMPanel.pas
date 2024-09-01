unit GMPanel;

{ ******************************************************************** }
{ TGMPanel                                                             }
{ ******************************************************************** }
{ Date : 2006-10-30                                                    }
{ Coding : Gerard Maheu                                                }
{                                                     }
{                                                                      }
{ Comments to : gerard1@mailcity.com                                   }
{ ******************************************************************** }
{ Properties :                                                         }
{    Title with or without section                                     }
{    Shadow, opaque or semi-transparent                                }
{    Picture or gradient on background                                 }
{    Panel semi-transparent                                            }
{    Panel of differents shapes                                        }
{    Predefined themes                                                 }
{ ******************************************************************** }
{ COPYING THE SOFTWARE                                                 }
{ You can freely copy the component provided and re-distribute         }
{ royalty free.                                                        }
{                                                                      }
{ MODIFIED VERSIONS OF THE SOFTWARE                                    }
{ Feel free to update, revise, modify or improve the component.        }
{ If you do so, PLEASE submit to the author any improvements or        }
{ additions you made for future inclusion in the general distribution  }
{ of the component by the author.                                      }
{ ******************************************************************** }
{ MODIFICATIONS :   (Please, leave all the above sections.)            }
{ Date  : Date of modification                                         }
{ By    : Your name                                                    }
{ Email :                                                              }
{ Description:                                                         }
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
{ ******************************************************************** }

//TODO: avec picture ou gradient, ca branle !  voir pour ajouter un masque
//      lorsqu'on a une forme autre que rectangle, pour conserver la forme.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Math;

const
  CM_NEEDBACK = CM_BASE + 101;
  NB_PIXELS   = 25; // Grandeur minimum pour la section de titre, et
                    // grandeur maximum pour les coins des polygones.

type
  TPanelType       = (ptRectangle, ptRoundRect, ptTitleRoundRect, ptOctogone, ptHexagoneLeft, ptHexagoneRight);

  TTitlePosition   = (tpTop, tpLeft, tpBottom);

  THorizontalAlign = (haCenter, haRight, haLeft,   haNone);

  TVerticalAlign   = (vaCenter, vaTop,   vaBottom, vaNone);

  TShadowPosition  = (spTopLeft, spTopRight, spBottomLeft, spBottomRight);

  TPictureStyle    = (psTile, psCenter, psStretch);

  TGradientType    = (gtHorizontal, gtVertical);

  TTransparentType = (gttNone, gttLeft, gttRight, gttUp, gttDown);

  TThemeType       = (ttNote, ttBlue, ttPurple, ttGreen, ttYellow, ttCream, ttRed, ttCustom);

  TTransparent = class(TPersistent)
  private
    FActive: Boolean;
    FPercent: Integer;
    FType: TTransparentType;
    FOnChange: TNotifyEvent;
    procedure SetTransparentActive(const Value: Boolean);
    procedure SetTransparentPercent(Value: Integer);
    procedure SetTransparentType(const Value: TTransparentType);
  protected
    procedure ChangeOccured; dynamic;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active: Boolean  read FActive  write SetTransparentActive;
    property Percent: Integer read FPercent write SetTransparentPercent;
    property GradientType: TTransparentType read FType write SetTransparentType;
  end;

  TTitle = class(TPersistent)
  private
    FText : String;                     // Texte du titre.
    FFont: TFont;                       // Police du texte.
    FTop: Integer;                      // Position verticale du texte si FVAlign = vaNone.
    FLeft: Integer;                     // Position horizontale du texte si FHAlign = haNone.
    FVAlign: TVerticalAlign;            // Alignement vertical du titre.
    FHAlign: THorizontalAlign;          // Alignement horizontal du titre.
    FSectionVisible : Boolean;          // Section de titre visible ou non.
    FSectionPosition: TTitlePosition;   // Position de la section de titre.
    FSectionSize: Integer;              // Dimension de la section de titre.
    FSectionColor: TColor;              // Couleur de fon de la section de titre.
    FTransparent: TTransparent;
    FOnChange: TNotifyEvent;
    procedure SetText(const Value: String);
    procedure SetFont(const Value: TFont);
    procedure SetTop(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetSectionColor(const Value: TColor);
    procedure SetSectionPosition(const Value: TTitlePosition);
    procedure SetSectionSize(const Value: Integer);
    procedure SetSectionVisible(const Value: Boolean);
    procedure SetVAlign(const Value: TVerticalAlign);
    procedure SetHAlign(const Value: THorizontalAlign);
    procedure TitleChange(Sender: TObject);
  protected
    procedure ChangeOccured; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Text : String read FText write SetText;
    property Font : TFont read FFont write SetFont;
    property Top  : Integer read FTop write SetTop;
    property Left : Integer read FLeft write SetLeft;
    property VerticalAlign: TVerticalAlign read FVAlign write SetVAlign;
    property HorizontalAlign: THorizontalAlign read FHAlign write SetHAlign;
    property SectionVisible : Boolean read FSectionVisible write SetSectionVisible;
    property SectionPosition : TTitlePosition read FSectionPosition write SetSectionPosition;
    property SectionSize : Integer read FSectionSize write SetSectionSize;
    property SectionColor : TColor read FSectionColor write SetSectionColor;
    property Transparent: TTransparent read FTransparent write FTransparent;
  end;

  TShadow = class(TPersistent)
  private
    FVisible      : Boolean;         // Ombrage visible ou non.
    FColor        : TColor;          // Couleur de l'ombre.
    FDepth        : Integer;         // Profondeur de l'ombre.
    FSpace        : Integer;         // Espace de l'ombre à partir du bord du panneau.
    FPosition     : TShadowPosition; // Position de l'ombre.
    FTransparency : Integer;         // Pourcentage de transparence
    FOnChange: TNotifyEvent;
    procedure SetVisible(Value : Boolean);
    procedure SetColor(Value : TColor);
    procedure SetDepth(Value : Integer);
    procedure SetSpace(Value : Integer);
    procedure SetPosition(Value : TShadowPosition);
    procedure SetTransparency(Value : Integer);
  protected
    procedure ChangeOccured; dynamic;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Visible : Boolean read FVisible write SetVisible;
    property Color   : TColor  read FColor   write SetColor;
    property Depth   : Integer read FDepth   write SetDepth;
    property Space   : Integer read FSpace   write SetSpace;
    property Position: TShadowPosition read FPosition write SetPosition;
    property Transparency : Integer read FTransparency write SetTransparency;
  end;

  TGMPanelPicture = class(TPersistent)
  private
    FPicture: TBitMap;
    FStyle : TPictureStyle;
    FTransparent: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetPicture(const Value : TBitMap);
    procedure SetPictureStyle(const Value : TPictureStyle);
    procedure SetPictureTransparent(const Value : Boolean);
  protected
    procedure ChangeOccured; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Picture: TBitMap read FPicture write SetPicture;
    property Style: TPictureStyle read FStyle write SetPictureStyle;
    property Transparent: Boolean read FTransparent write SetPictureTransparent;
  end;

  TGradient = class(TPersistent)
  private
    FActive: Boolean;
    FPercent: Integer;
    FColor_1: TColor;
    FColor_2: TColor;
    FColor_3: TColor;
    FType: TGradientType;
    FOnChange: TNotifyEvent;
    procedure SetGradientActive(const Value: Boolean);
    procedure SetGradientPercent(Value: integer);
    procedure SetGradientColor_1(const Value: TColor);
    procedure SetGradientColor_2(const Value: TColor);
    procedure SetGradientColor_3(const Value: TColor);
    procedure SetGradientType(const Value: TGradientType);
  protected
    procedure ChangeOccured; dynamic;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active: Boolean  read FActive  write SetGradientActive;
    property Percent: Integer read FPercent write SetGradientPercent;
    property Color_1: TColor  read FColor_1 write SetGradientColor_1;
    property Color_2: TColor  read FColor_2 write SetGradientColor_2;
    property Color_3: TColor  read FColor_3 write SetGradientColor_3;
    property GradientType: TGradientType read FType write SetGradientType;
  end;

  TGMPanel = class(TCustomPanel)
  private
    FTitle: TTitle;
    FShadow: TShadow;
    FPicture: TGMPanelPicture;
    FBackBitmap : TBitmap;
    FGradient : TGradient;
    FPanelType: TPanelType;
    FRoundRectDegree: Integer;
    FTheme: TThemeType;
    FTransparent: TTransparent;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_EraseBkGnd;
    procedure DrawSection(const Rect: TRect);
    procedure DrawShadow(var Rect: TRect);
    procedure DrawPanel(var PanelRect: TRect);
    procedure DrawTitle(var Rect: TRect);
    procedure FontChanged(Sender: TObject);
    procedure TitleChanged(Sender: TObject);
    procedure ShadowChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure GradientChanged(Sender: TObject);
    procedure TransparentChange(Sender: TObject);
    procedure GetBackGround(Parent: TWinControl);
    procedure WMSIZE(var Message : TMessage); message WM_SIZE;
    procedure WMMOVE(Var Message : TMessage); message WM_MOVE;
    procedure CMNEEDBACK(Var Message : TMessage); message CM_NEEDBACK;
    procedure SetPanelType(const Value: TPanelType);
    procedure SetRoundRectDegree(const Value: integer);
    procedure SetTheme(const Value: TThemeType);
    procedure DoGradient(CL, CT, CW, CH : Integer);
    procedure DrawGradient(GradientRect: TRect);
    procedure DrawRectTransparent(TheRect: TRect; TransPercent: Integer; RectColor : TColor; TransparentType: TTransparentType;IsShadow : Boolean);
    procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor; Width: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure RePaint; override;
    procedure Refresh;
    procedure Update; override;
  published
    // Rendre published les propriétés qu'on veut.
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    // Nouvelles propriétés.
    property Title: TTitle read FTitle write FTitle;
    property Shadow: TShadow read FShadow write FShadow;
    property Gradient : TGradient read FGradient write FGradient;
    property Picture: TGMPanelPicture read FPicture write FPicture;
    property PanelType: TPanelType read FPanelType write SetPanelType;
    property RoundRectDegree : Integer read FRoundRectDegree write SetRoundRectDegree;
    property Theme: TThemeType read FTheme write SetTheme;
    property Transparent: TTransparent read FTransparent write FTransparent;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [TGMPanel]);
end;

{ --------------------------------------------------------------------
  Méthode : DrawBitmapTile.
  But     :
  -------------------------------------------------------------------- }
procedure DrawBitmapTile(B:TBitmap; C:TCanvas; AR:TRect);
var
  PB,PBP : TBitMap;
  X,Y  : integer;
begin
  PB  := TBitMap.Create;
  PBP := TBitMap.Create;
  PB.Assign(B);
  PB.Dormant;             // Free up GDI resources
  PB.FreeImage;           // Free up Memory.
  try
    if not PB.Empty then
      begin
        y := AR.Top;
        while y < AR.Bottom do
          begin
            x := AR.Left;
            while x < AR.Right do
              begin
                if x + PB.Width <= AR.Right then
                begin
                  if y + PB.Height <= AR.Bottom then
                  begin
                    C.Draw(x, y,PB )
                  end
                  else
                  begin
                    PBP.Assign(B);
                    PBP.Height:=(AR.Bottom-y);
                    C.Draw(x, y,PBP);
                    PBP.Dormant;             // Free up GDI resources
                    PBP.FreeImage;           // Free up Memory.
                  end;
                end
                else
                begin
                  if y + PB.Height<=AR.Bottom then
                    begin
                      PBP.Assign(B);
                      PBP.Width := (AR.Right-x);
                      C.Draw(x, y,PBP);
                      PBP.Dormant;             // Free up GDI resources
                      PBP.FreeImage;           // Free up Memory.
                    end
                  else
                    begin
                      PBP.Assign(B);
                      PBP.Width  := (AR.Right-x);
                      PBP.Height := (AR.Bottom-y);
                      C.Draw(x, y,PBP);
                      PBP.Dormant;             // Free up GDI resources
                      PBP.FreeImage;           // Free up Memory.
                    end;
                end;

                x := x + PB.Width;
              end;
            y := y + PB.Height;
         end;
      end;
  finally
    PB.Free;
    PBP.Free;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : DrawBitmapCenter.
  But     :
  -------------------------------------------------------------------- }
procedure DrawBitmapCenter(B:TBitmap; C:TCanvas; AR:TRect);
var
  PB   : TBitMap;
  X,Y  : integer;
begin
  PB:=TBitMap.Create;
  PB.Assign(B);
  PB.Dormant;             // Free up GDI resources
  PB.FreeImage;           // Free up Memory.
  try
    if not PB.Empty then
      begin
        if PB.Width<(AR.Right-AR.Left) then
          begin
            X:=AR.Left+((AR.Right-PB.Width-AR.Left) div 2);
          end
        else
          begin
            X := AR.Left;
            PB.Width:=AR.Right-AR.Left;
          end;

        if PB.Height<(AR.Bottom-AR.Top) then
          begin
            Y:=AR.Top+((AR.Bottom-PB.Height-AR.Top) div 2);
          end
        else
          begin
            y := AR.Top;
            PB.Height:=AR.Bottom-AR.Top;
          end;
        C.Draw(x, y,PB);
      end;
  finally
    PB.Free;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : DrawBitmapStretch.
  But     :
  -------------------------------------------------------------------- }
procedure DrawBitmapStretch(B:TBitmap; C:TCanvas; AR:TRect);
var
  PB   : TBitMap;
begin
  PB:=TBitMap.Create;
  PB.Assign(B);
  PB.Dormant;             // Free up GDI resources
  PB.FreeImage;           // Free up Memory.
  try
    if not PB.Empty then
      C.StretchDraw(AR,PB);
  finally
    PB.Free;
  end;
end;

{ ******************************************************************** }
{ ****************************** TGMPanel **************************** }
{ ******************************************************************** }

{ --------------------------------------------------------------------
  Méthode : Create.
  But     :
  -------------------------------------------------------------------- }
constructor TGMPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csOpaque]; // Panel transparent.

  FTitle               := TTitle.Create;
  FTitle.Font.OnChange := FontChanged;
  FTitle.OnChange      := TitleChanged;

  FShadow              := TShadow.Create;
  FShadow.OnChange     := ShadowChanged;

  FPicture             := TGMPanelPicture.Create;
  FPicture.OnChange    := PictureChanged;

  FGradient            := TGradient.Create;
  FGradient.OnChange   := GradientChanged;

  FTransparent         := TTransparent.Create;
  FTransparent.OnChange:= TransparentChange;

  FTheme               := ttCustom;

  FBackBitmap := TBitmap.Create;
  FBackBitmap.PixelFormat:=pf24bit;

  PanelType       := ptRectangle;
  RoundRectDegree := 20;

  BevelInner  := bvNone;
  BevelOuter  := bvNone;
  BevelWidth  := 1;
  BorderWidth := 0;

  FullRepaint := True;
end;

{ --------------------------------------------------------------------
  Méthode : CreateParams.
  But     : Avoir un panneau transparent.
  -------------------------------------------------------------------- }
procedure TGMPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

{ --------------------------------------------------------------------
  Méthode : Frame3D.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor,
  BottomColor: TColor; Width: Integer);

  procedure DoRect;
    // Largement inspiré de TCustomPanel.Paint du unit ExtCtrls.
    var
      Delta : Integer; // Dimension des coins du polygone.
    begin
      Delta := Min(NB_PIXELS, (Rect.Bottom div 4)); // Un maximum de 25 pixels.

      case FPanelType of
        ptRectangle :
          begin
            Canvas.Pen.Color := TopColor;
            Canvas.PolyLine([Point(Rect.Left, Rect.Bottom),
                             Point(Rect.Left, Rect.Top),
                             Point(Rect.Right, Rect.Top)]);
            Canvas.Pen.Color := BottomColor;
            Canvas.PolyLine([Point(Rect.Right, Rect.Top),
                             Point(Rect.Right, Rect.Bottom),
                             Point(Rect.Left-1, Rect.Bottom)]);
          end;
        ptRoundRect :
          begin
            Canvas.Pen.Color := TopColor;
            // Côté gauche.
            Canvas.MoveTo(Rect.Left, Rect.Bottom - (FRoundRectDegree div 2));
            Canvas.LineTo(Rect.Left, Rect.Top + (FRoundRectDegree div 2));
            // Coin supérieur gauche.
            Canvas.Arc(Rect.Left, Rect.Top, Rect.Left+FRoundRectDegree, Rect.Top+FRoundRectDegree, Rect.Left+(FRoundRectDegree div 2), Rect.Top, Rect.Left, Rect.Top+(FRoundRectDegree div 2));
            // Côté du haut.
            Canvas.MoveTo(Rect.Left + (FRoundRectDegree div 2), Rect.Top);
            Canvas.LineTo(Rect.Right - (FRoundRectDegree div 2), Rect.Top);
            // Coin supérieur droit.
            Canvas.Arc(Rect.Right-FRoundRectDegree, Rect.Top, Rect.Right, Rect.Top+FRoundRectDegree, Rect.Right, Rect.Top + (FRoundRectDegree div 2), Rect.Right - (FRoundRectDegree div 2), Rect.Top);

            Canvas.Pen.Color := BottomColor;
            // Côté droit.
            Canvas.MoveTo(Rect.Right, Rect.Top + (FRoundRectDegree div 2));
            Canvas.LineTo(Rect.Right, Rect.Bottom - (FRoundRectDegree div 2));
            // Coin inférieur droit.
            Canvas.Arc(Rect.Right-FRoundRectDegree, Rect.Bottom - FRoundRectDegree, Rect.Right, Rect.Bottom, Rect.Right - (FRoundRectDegree div 2), Rect.Bottom, Rect.Right, Rect.Bottom - (FRoundRectDegree div 2));
            // Côté du bas.
            Canvas.MoveTo(Rect.Right - (FRoundRectDegree div 2), Rect.Bottom);
            Canvas.LineTo(Rect.Left-1 + (FRoundRectDegree div 2), Rect.Bottom);
            // Coin inférieur gauche.
            Canvas.Arc(Rect.Left-1, Rect.Bottom - FRoundRectDegree, Rect.Left-1+FRoundRectDegree, Rect.Bottom, Rect.Left-1, Rect.Bottom-(FRoundRectDegree div 2), Rect.Left-1+(FRoundRectDegree div 2), Rect.Bottom );
          end;
        ptTitleRoundRect :
          begin
            if FTitle.SectionVisible then
            begin
              if FTitle.SectionPosition = tpTop then
              begin
                Canvas.Pen.Color := TopColor;
                // Côté gauche.
                Canvas.MoveTo(Rect.Left, Rect.Bottom);
                Canvas.LineTo(Rect.Left, Rect.Top + (FRoundRectDegree div 2));
                // Coin supérieur gauche.
                Canvas.Arc(Rect.Left, Rect.Top, Rect.Left+FRoundRectDegree, Rect.Top+FRoundRectDegree, Rect.Left+(FRoundRectDegree div 2), Rect.Top, Rect.Left, Rect.Top+(FRoundRectDegree div 2));
                // Côté du haut.
                Canvas.MoveTo(Rect.Left + (FRoundRectDegree div 2), Rect.Top);
                Canvas.LineTo(Rect.Right - (FRoundRectDegree div 2), Rect.Top);

                Canvas.Pen.Color := BottomColor;
                // Coin supérieur droit.
                Canvas.Arc(Rect.Right-FRoundRectDegree, Rect.Top, Rect.Right, Rect.Top+FRoundRectDegree, Rect.Right, Rect.Top + (FRoundRectDegree div 2), Rect.Right - (FRoundRectDegree div 2), Rect.Top);
                // Côté droit.
                Canvas.MoveTo(Rect.Right, Rect.Top + (FRoundRectDegree div 2));
                Canvas.LineTo(Rect.Right, Rect.Bottom);
                Canvas.LineTo(Rect.Left-1, Rect.Bottom);
              end
              else
              if FTitle.SectionPosition = tpBottom then
              begin
                Canvas.Pen.Color := TopColor;
                // Coin inférieur gauche.
                Canvas.Arc(Rect.Left, Rect.Bottom - FRoundRectDegree, Rect.Left+FRoundRectDegree, Rect.Bottom, Rect.Left, Rect.Bottom-(FRoundRectDegree div 2), Rect.Left+(FRoundRectDegree div 2), Rect.Bottom );
                // Côté gauche.
                Canvas.MoveTo(Rect.Left, Rect.Bottom - (FRoundRectDegree div 2));
                Canvas.LineTo(Rect.Left, Rect.Top);
                // Coté du haut.
                Canvas.LineTo(Rect.Right, Rect.Top);

                Canvas.Pen.Color := BottomColor;
                // Côté droit.
                Canvas.LineTo(Rect.Right, Rect.Bottom - (FRoundRectDegree div 2));
                // Coin inférieur droit.
                Canvas.Arc(Rect.Right-FRoundRectDegree, Rect.Bottom - FRoundRectDegree, Rect.Right, Rect.Bottom, Rect.Right - (FRoundRectDegree div 2), Rect.Bottom, Rect.Right, Rect.Bottom - (FRoundRectDegree div 2));
                // Côté du bas.
                Canvas.MoveTo(Rect.Right - (FRoundRectDegree div 2), Rect.Bottom);
                Canvas.LineTo(Rect.Left-1 + (FRoundRectDegree div 2), Rect.Bottom);
              end
              else // FTitlePosition = tpLeft.
              begin
                Canvas.Pen.Color := TopColor;
                // Coin inférieur gauche.
                Canvas.Arc(Rect.Left, Rect.Bottom - FRoundRectDegree, Rect.Left+FRoundRectDegree, Rect.Bottom, Rect.Left, Rect.Bottom-(FRoundRectDegree div 2), Rect.Left+(FRoundRectDegree div 2), Rect.Bottom );
                // Côté gauche.
                Canvas.MoveTo(Rect.Left, Rect.Bottom - (FRoundRectDegree div 2));
                Canvas.LineTo(Rect.Left, Rect.Top + (FRoundRectDegree div 2));
                // Coin supérieur gauche.
                Canvas.Arc(Rect.Left, Rect.Top, Rect.Left+FRoundRectDegree, Rect.Top+FRoundRectDegree, Rect.Left+(FRoundRectDegree div 2), Rect.Top, Rect.Left, Rect.Top+(FRoundRectDegree div 2));
                // Côté du haut.
                Canvas.MoveTo(Rect.Left + (FRoundRectDegree div 2), Rect.Top);
                Canvas.LineTo(Rect.Right, Rect.Top);

                Canvas.Pen.Color := BottomColor;
                // Côté droit.
                Canvas.LineTo(Rect.Right, Rect.Bottom);
                // Côté du bas.
                Canvas.LineTo(Rect.Left-1 + (FRoundRectDegree div 2), Rect.Bottom);
              end;
            end
            else
            begin // Pas de section de titre, donc c'est un rectangle ordinaire.
              Canvas.Pen.Color := TopColor;
              Canvas.PolyLine([Point(Rect.Left, Rect.Bottom),
                               Point(Rect.Left, Rect.Top),
                               Point(Rect.Right, Rect.Top)]);
              Canvas.Pen.Color := BottomColor;
              Canvas.PolyLine([Point(Rect.Right, Rect.Top),
                               Point(Rect.Right, Rect.Bottom),
                               Point(Rect.Left-1, Rect.Bottom)]);
            end;
          end;
        ptOctogone  :
          begin
            Canvas.Pen.Color := TopColor;
            Canvas.PolyLine([Point(Rect.Right - Delta, Rect.Top),
                             Point(Rect.Left + Delta, Rect.Top),
                             Point(Rect.Left, Delta),
                             Point(Rect.Left, Rect.Bottom - Delta),
                             Point(Rect.Left + Delta, Rect.Bottom)]);
            Canvas.Pen.Color := BottomColor;
            Canvas.PolyLine([Point(Rect.Left-1 + Delta, Rect.Bottom),
                             Point(Rect.Right - Delta, Rect.Bottom),
                             Point(Rect.Right, Rect.Bottom - Delta),
                             Point(Rect.Right, Rect.Top + Delta),
                             Point(Rect.Right - Delta, Rect.Top)]);
          end;
        ptHexagoneLeft :
          begin
            Canvas.Pen.Color := TopColor;
            Canvas.PolyLine([Point(Rect.Right, Rect.Top),
                             Point(Rect.Left + Delta, Rect.Top),
                             Point(Rect.Left, Rect.Top + Delta),
                             Point(Rect.Left, Rect.Bottom)]);
            Canvas.Pen.Color := BottomColor;
            Canvas.PolyLine([Point(Rect.Left-1, Rect.Bottom),
                             Point(Rect.Right - Delta, Rect.Bottom),
                             Point(Rect.Right, Rect.Bottom - Delta),
                             Point(Rect.Right, Rect.Top)]);

          end;
        ptHexagoneRight :
          begin
            Canvas.Pen.Color := TopColor;
            Canvas.PolyLine([Point(Rect.Right - Delta, Rect.Top),
                             Point(Rect.Left, Rect.Top),
                             Point(Rect.Left, Rect.Bottom - Delta),
                             Point(Rect.Left + Delta, Rect.Bottom)]);
            Canvas.Pen.Color := BottomColor;
            Canvas.PolyLine([Point(Rect.Left - 1 + Delta, Rect.Bottom),
                             Point(Rect.Right, Rect.Bottom),
                             Point(Rect.Right, Rect.Top + Delta),
                             Point(Rect.Right - Delta, Rect.Top)]);
          end;
      end; // Case...
    end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

{ --------------------------------------------------------------------
  Méthode : Paint.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.Paint;
var
  Rect: TRect;
  PictureRect: TRect;
  PanelRect: TRect;
  Flags: LongInt;
begin
  Rect := GetClientRect;

  // Dessiner l'ombre et ajuste les dimensions du Rect en le diminuant
  // selon la grosseur de l'ombre.
  if Shadow.Visible then
    DrawShadow(Rect);
  PanelRect := Rect; // Mémorise les dimensions sans l'ombre, s'il y a lieu.

  // Dessiner le panneau.
  DrawPanel(Rect);

  // Dessiner le gradient.
  if FGradient.Active then
    DrawGradient(Rect);

  // Dessiner l'image en background.
  if not FPicture.Picture.Empty then
  begin
    PictureRect := Rect;
    FPicture.Picture.Transparent := FPicture.Transparent;

    if FTitle.SectionVisible then
    begin
      if FTitle.SectionPosition = tpTop then
        PictureRect.Top := PictureRect.Top + FTitle.SectionSize
      else
      if FTitle.SectionPosition = tpBottom then
        PictureRect.Bottom := PictureRect.Bottom - FTitle.SectionSize
      else // FTitlePosition = tpLeft.
        PictureRect.Left := PictureRect.Left + FTitle.SectionSize;
    end;

    case FPicture.Style of
      psTile    : DrawBitmapTile(FPicture.Picture, Canvas, PictureRect);
      psCenter  : DrawBitmapCenter(FPicture.Picture, Canvas, PictureRect);
      psStretch : DrawBitmapStretch(FPicture.Picture, Canvas, PictureRect);
    end;
  end; // if not FPicture.Picture.Empty then...

  if FTransparent.Active then
    DrawRectTransparent(PanelRect, FTransparent.FPercent, Color, FTransparent.FType, False);

  // Dessine la section de titre.
  if Title.SectionVisible then
    DrawTitle(Rect);

  // Écrire le titre.
  if Trim(FTitle.Text) > '' then
  begin
    Canvas.Font.Assign(FTitle.Font);
    Canvas.Brush.Style := bsClear; // Transparent.
    Canvas.Pen.Style := psClear;   // Transparent.

    if (FTitle.VerticalAlign = vaNone) and (FTitle.HorizontalAlign = haNone) then
      Canvas.TextOut(FTitle.Left, FTitle.Top, FTitle.Text)
    else
    begin
      Flags := DT_SINGLELINE;
      case FTitle.VerticalAlign of
        vaCenter : Flags := Flags or DT_VCENTER;
        vaTop    : Flags := Flags or DT_TOP;
        vaBottom : Flags := Flags or DT_BOTTOM;
        vaNone   : Rect.Top := FTitle.Top; // FTitle.Top sera utilisé.
      end;
      case FTitle.HorizontalAlign of
        haCenter : Flags := Flags or DT_CENTER;
        haRight  : Flags := Flags or DT_RIGHT;
        haLeft   : Flags := Flags or DT_LEFT;
        haNone   : Rect.Left := FTitle.Left; // FTitle.Left sera utilisé.
      end;

      DrawText(Canvas.Handle, PChar(FTitle.Text), -1, Rect, Flags);
    end;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : WMEraseBkGnd.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

{ --------------------------------------------------------------------
  Méthode : SetParent.
  But     : Faire en sorte que le panel soit transparent.
            Nécessaire pour que ca marche.
  -------------------------------------------------------------------- }
procedure TGMPanel.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if Parent <> nil then
    SetWindowLong(Parent.Handle, GWL_STYLE,
      GetWindowLong(Parent.Handle, GWL_STYLE) and not WS_ClipChildren);
end;

{ --------------------------------------------------------------------
  Méthode : RePaint.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.RePaint;
begin
  inherited Repaint;
end;

{ --------------------------------------------------------------------
  Méthode : Refresh.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.Refresh;
begin
  inherited Refresh;
end;

{ --------------------------------------------------------------------
  Méthode : Update.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.Update;
begin
  inherited Update;
end;

{ --------------------------------------------------------------------
  Méthode : Invalidate.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.Invalidate;
//var
//  Rect: TRect;
begin
//  Rect := BoundsRect;

//  if (Parent <> nil) and Parent.HandleAllocated then
//    InvalidateRect(Parent.Handle, @Rect, True)
//  else

  inherited Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : Destroy.
  But     :
  -------------------------------------------------------------------- }
destructor TGMPanel.Destroy;
begin
  inherited;

  FTitle.Free;
  FShadow.Free;
  FBackBitmap.Free;
end;

{ --------------------------------------------------------------------
  Méthode : DoGradient.
  But     : Afficher un gradiant de couleurs sur le panneau.
  Params. : Section affectée délimitée par
            CL = Client left,
            CT = Client top,
            CW = Client width,
            CH = Client height.
  -------------------------------------------------------------------- }
procedure TGMPanel.DoGradient(CL, CT, CW, CH: Integer);
var
  H, D0, H1,
  S, S1, T,
  R, G, B,
  R0, G0, B0: Integer;
  Coefficient : Integer;

  function EX(A: Integer): Integer;
  begin
    Result := A div Coefficient;
  end;
  //
  procedure DWord_Int(C_X: TColor; var Rx, Gx, Bx: Integer);
  var
    Y1, Y2, Y3: Integer;
  begin
    Y1 := GetRValue(C_X);
    Y2 := GetGValue(C_X);
    Y3 := GetBValue(C_X);
    Rx := Y1 * Coefficient;
    Gx := Y2 * Coefficient;
    Bx := Y3 * Coefficient;
  end;
  //
  procedure DO_FILL(X1, X2, X3, X4: Integer);
  begin
    with Canvas do
    begin
      Brush.Color := RGB((EX(R) + EX(R0 div S * S1)),
        (EX(G) + EX(G0 div S * S1)),
        (EX(B) + EX(B0 div S * S1))
        );

      FillRect(RECT(X1, X2, X3, X4));
    end;
  end;
  //
  procedure GPart(CxL, CxT, CxW, CxH, Dx0: Integer);
  var
    Sx1: Integer;
  begin
    if FGradient.GradientType = gtHorizontal then
      T := CxW
    else
      T := CxH;
    if T * Coefficient <= Dx0 then
      H := 1
    else
      H := ((T + EX(Dx0)) - 1) * Coefficient div Dx0;
//      H := ((T + EX(Dx0))) * Coefficient div Dx0;
    S := T div H; // get number of steps MAX

    // now do the FILLING of the AREA
    H1 := H; // for all - but the last one
    for Sx1 := 0 to S do
    begin
      S1 := Sx1; // so the DO_FILL will see the VAR.
      if ((Sx1 + 1) * H) > T then
//      if ((Sx1) * H) > T then
        H1 := T - (Sx1 * H);
      if FGradient.GradientType = gtHorizontal then
        DO_FILL(CxL + (Sx1 * H), CxT, CxL + ((Sx1 * H) + H1), CxT + CxH)
      else
        DO_FILL(CxL, CxT + (Sx1 * H), CxL + CxW, CxT + ((Sx1 * H) + H1));
    end;
  end;
  //
  function Get_Colors(FCol, TCol: TColor): Boolean;
  begin
    Result := False;
    if FCol = TCol then
      EXIT;
    //   RED GREEN BLUE
    DWord_Int(ColorToRGB(FCol), R, G, B);
    //   get MAX difference
    DWord_Int(ColorToRGB(TCol), R0, G0, B0);
    R0 := R0 - R; // get Color-Diff RED
    G0 := G0 - G; //                GREEN
    B0 := B0 - B; //                BLUE
    D0 := ABS(R0); // get Max-Color-Difference
    if ABS(G0) > D0 then
      D0 := ABS(G0);
    if ABS(B0) > D0 then
      D0 := ABS(B0);
    Result := TRUE;
  end;
  //
  function XH(I, Proz: Integer): Integer; // PART height
  begin
    Result := (I * Proz) div 100;
    if Result = 0 then
      Result := 1;
  end;
begin
  Coefficient := 100;

  // Up to now - just a few general settings
  if FGradient.Percent = 100 then
  begin
    if Get_Colors(FGradient.Color_1, FGradient.Color_2) then
      GPart(CL, CT, CW, CH, D0);
  end
  else
  begin
    if Get_Colors(FGradient.Color_1, FGradient.Color_2) then
      if FGradient.GradientType = gtHorizontal then
        GPart(CL, CT, XH(CW, FGradient.Percent), CH, D0)
      else
        GPart(CL, CT, CW, XH(CH, FGradient.Percent), D0);
    if Get_Colors(FGradient.Color_2, FGradient.Color_3) then
      if FGradient.GradientType = gtHorizontal then
        GPart(CL + XH(CW, FGradient.Percent), CT,
          CW - XH(CW, FGradient.Percent), CH, D0)
      else
        GPart(CL, CT + XH(CH, FGradient.Percent),
          CW, CH - XH(CH, FGradient.Percent), D0);
  end;
end;

{ --------------------------------------------------------------------
  Méthode : DrawGradient.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.DrawGradient(GradientRect: TRect);
var
  Dim : Integer;
begin
  if (FTitle.SectionVisible) then
  begin
    Dim := FTitle.SectionSize;

    with GradientRect do
    begin
      if FTitle.SectionPosition = tpTop then
        DoGradient(Left, Top+Dim, Right-Left, Bottom-Dim-Top)
      else
      if FTitle.SectionPosition = tpBottom then
        DoGradient(Left, Top, Right-Left, Bottom-Dim-Top)
      else // if FTitlePosition = tpLeft
        DoGradient(Left+Dim, Top, Right-Dim-Left, Bottom-Top);
    end; // with GradientRect do
  end
  else
    DoGradient(GradientRect.Left, GradientRect.Top, GradientRect.Right-GradientRect.Left, GradientRect.Bottom-GradientRect.Top);
end;

{ --------------------------------------------------------------------
  Méthode : DrawPanel.
  But     : Dessine le panneau.
  Param.  : TRect délimitant le panneau.
  -------------------------------------------------------------------- }
procedure TGMPanel.DrawPanel(var PanelRect: TRect);
var
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;
begin
  // Bordure 3D.
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, PanelRect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, PanelRect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, PanelRect, TopColor, BottomColor, BevelWidth);
  end;

  Canvas.Brush.Color := Color;
  Canvas.Pen.Color   := Color;
  DrawSection(PanelRect);
end;

{ --------------------------------------------------------------------
  Méthode : DrawRectTransparent.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.DrawRectTransparent(TheRect: TRect; TransPercent: Integer;
  RectColor : TColor; TransparentType: TTransparentType;IsShadow : Boolean);
var
  FForeBitmap : TBitmap;
  p1,p2       : pByteArray;
  i,j         : Integer;

  function Bias(PValue, PBias: Double): Double;
  begin
    //Invalid values means not bias calculation
    if (PBias <= 1) and (PBias >= -1) and (PValue >= 0) and (PValue <= 1) then
    begin
      // a Bias of 0 is a linear relationship. Let's save some time here
      if PBias = 0 then
      begin
        Result := PValue;
        exit;
      end;
      //PBias ranges from 1 through 0 to -1. Actual bias should be between 0 and 1
      if PBias >= 0 then
      begin
        //Positive bias
        Result := Power(PValue, 1 - PBias);
      end
      else
      begin
        //mirrored positive bias
        Result := 1 - power(1 - PValue, 1 + PBias);
      end;
    end
    else
    begin
      Result := PValue;
    end;
  end;
  function CalculateTransFade(X, Y, Trans : Integer): Integer;
  var
    TransFade : Integer;
  begin
    case TransparentType of
      // Bitmap 24 bit (3 bytes par pixel pour R G et B)
      gttLeft : TransFade := 100 - Round(Bias(((X div 3)/FBackBitmap.Width), (Trans/100)) * (100-Trans));
      gttRight: TransFade := 100 - Round(Bias((1 - (X div 3) / FBackBitmap.Width), (Trans/100)) * (100 - Trans));
      gttUp   : TransFade := 100 - Round(Bias((Y / FBackBitmap.Height), (Trans/100)) * (100 - Trans));
      gttDown : TransFade := 100 - Round(Bias((1 - Y / FBackBitmap.Height), (Trans/100)) * (100 - Trans));
    else
      TransFade := Trans; //gttNone
    end; // case...

    if TransFade < 0 then
      Result := 0
    else
    if TransFade > 100 then
      Result := 100
    else
      Result := TransFade;
  end;
begin
  FForeBitmap := TBitmap.Create;

  FForeBitmap.PixelFormat := pf24bit;
  FForeBitmap.Width       := FBackBitmap.Width;
  FForeBitmap.Height      := FBackBitmap.Height;

  if (IsShadow) then
  begin
    FForeBitmap.Canvas.Brush.Color := RectColor;
    FForeBitmap.Canvas.Brush.Style := bsSolid;
    FForeBitmap.Canvas.Pen.Style   := psClear;
    FForeBitmap.Canvas.FillRect(TheRect);
  end
  else
  begin
//    if (FGradient.Active) or not(FPicture.Picture.Empty) then
//    begin
      FForeBitmap.Canvas.CopyRect(Rect(0,0,FBackBitmap.Width,FBackBitmap.Height), Canvas, Rect(0,0,FBackBitmap.Width,FBackBitmap.Height));
//    end
//    else
//    begin
//      FForeBitmap.Canvas.Brush.Color := RectColor;
//      FForeBitmap.Canvas.Brush.Style := bsSolid;
//      FForeBitmap.Canvas.Pen.Style   := psClear;
//      FForeBitmap.Canvas.FillRect(TheRect);
//    end;
  end;

  // Jumelle les deux bitmap pour simuler la transparence.
  for i := 0 to FBackBitmap.Height-1 do
  begin
    p1 := FBackBitmap.ScanLine[i];
    p2 := FForeBitmap.ScanLine[i];

    for j := 0 to (3 * FBackBitmap.Width)-1 do // bitmap 24 Bit (3 Bytes per pixel pour R G et B)
    begin
      p2[j] := p2[j]+ (CalculateTransFade(j, i, TransPercent) * (p1[j]-p2[j]) div 100);
    end;
  end;

  Canvas.Pen.Style    := psClear;
  Canvas.Brush.Bitmap := FForeBitmap;
  DrawSection(TheRect);
  FForeBitmap.Free;
end;

{ --------------------------------------------------------------------
  Méthode : DrawSection.
  But     : Dessine une section selon le type de panel voulu.
  Param.  : TRect délimitant la section.
  -------------------------------------------------------------------- }
procedure TGMPanel.DrawSection(const Rect: TRect);
var
  Delta : Integer; // Dimension des coins du polygone.
begin
  Delta := Min(NB_PIXELS, (Rect.Bottom div 4)); // Un maximum de 25 pixels.

  case FPanelType of
    ptRectangle : Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    ptRoundRect : Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, FRoundRectDegree, FRoundRectDegree);
    ptTitleRoundRect :
                  begin
                    if FTitle.SectionVisible then
                    begin
                      Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, FRoundRectDegree, FRoundRectDegree);
                      // Le côté opposé au titre est carré. On couvre donc la partie arrondie qu'on ne veut pas.
                      case FTitle.SectionPosition of
                        tpTop    : Canvas.Rectangle(Rect.Left, Rect.Bottom - Delta - 2, Rect.Right, Rect.Bottom);
                        tpBottom : Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Top + Delta + 2);
                        else
                          Canvas.Rectangle(Rect.Right - Delta - 2, Rect.Top, Rect.Right, Rect.Bottom);
                      end;
                    end
                    else
                      Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
                  end;
    ptOctogone  : Canvas.Polygon([Point(Rect.Left, Delta),
                  Point(Rect.Left, Rect.Bottom - Delta),
                  Point(Rect.Left + Delta, Rect.Bottom),
                  Point(Rect.Right - Delta, Rect.Bottom),
                  Point(Rect.Right, Rect.Bottom - Delta),
                  Point(Rect.Right, Rect.Top + Delta),
                  Point(Rect.Right - Delta, Rect.Top),
                  Point(Rect.Left + Delta, Rect.Top),
                  Point(Rect.Left, Delta)]);
    ptHexagoneLeft : Canvas.Polygon([Point(Rect.Left + Delta, Rect.Top),
                  Point(Rect.Left, Rect.Top + Delta),
                  Point(Rect.Left, Rect.Bottom),
                  Point(Rect.Right - Delta, Rect.Bottom),
                  Point(Rect.Right, Rect.Bottom - Delta),
                  Point(Rect.Right, Rect.Top),
                  Point(Rect.Left + Delta, Rect.Top)]);
    ptHexagoneRight : Canvas.Polygon([Point(Rect.Left, Rect.Top),
                  Point(Rect.Left, Rect.Bottom - Delta),
                  Point(Rect.Left + Delta, Rect.Bottom),
                  Point(Rect.Right, Rect.Bottom),
                  Point(Rect.Right, Rect.Top + Delta),
                  Point(Rect.Right - Delta, Rect.Top),
                  Point(Rect.Left, Rect.Top)]);
  end;
end;

{ --------------------------------------------------------------------
  Méthode : DrawShadow.
  But     : Dessine l'ombre.
  Param.  : TRect délimitant le panneau au complet.
  Retour  : TRect est modifié pour obtenir les dimensions disponibles
            pour le panneau utilisable.
  -------------------------------------------------------------------- }
procedure TGMPanel.DrawShadow(var Rect: TRect);
var
  ShadowRect  : TRect;
begin
  Canvas.Brush.Color := Shadow.Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color   := Shadow.Color;

  ShadowRect := Rect;
  case FShadow.Position of
    spTopLeft     : begin
                      ShadowRect.Bottom := ShadowRect.Bottom - Shadow.Space;
                      ShadowRect.Right  := ShadowRect.Right - Shadow.Space;
                    end;
    spTopRight    : begin
                      ShadowRect.Bottom := ShadowRect.Bottom - Shadow.Space;
                      ShadowRect.Left   := ShadowRect.Left + Shadow.Space;
                    end;
    spBottomLeft  : begin
                      ShadowRect.Top  := ShadowRect.Top + Shadow.Space;
                      ShadowRect.Right:= ShadowRect.Right - Shadow.Space;
                    end;
    spBottomRight : begin
                      ShadowRect.Top  := ShadowRect.Top + Shadow.Space;
                      ShadowRect.Left := ShadowRect.Left + Shadow.Space;
                    end;
  end;

  if Shadow.Transparency = 0 then
  begin
    DrawSection(ShadowRect);
  end
  else
  // Ombre transparente.
  begin
    DrawRectTransparent(ShadowRect, Shadow.Transparency, Shadow.Color, gttNone, True)
  end;

  // Retire les dimensions de l'ombre, pour le Rect disponible pour
  // le panneau.
  case FShadow.Position of
    spTopLeft     : begin
                      Rect.Left   := ShadowRect.Left + Shadow.Depth;
                      Rect.Top    := ShadowRect.Top + Shadow.Depth;
                    end;
    spTopRight    : begin
                      Rect.Right  := ShadowRect.Right - Shadow.Depth;
                      Rect.Top    := ShadowRect.Top + Shadow.Depth;
                    end;
    spBottomLeft  : begin
                      Rect.Left   := ShadowRect.Left + Shadow.Depth;
                      Rect.Bottom := ShadowRect.Bottom - Shadow.Depth;
                    end;
    spBottomRight : begin
                      Rect.Right  := ShadowRect.Right - Shadow.Depth;
                      Rect.Bottom := ShadowRect.Bottom - Shadow.Depth;
                    end;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : DrawTitle.
  But     : Dessin de la section du titre.
  -------------------------------------------------------------------- }
procedure TGMPanel.DrawTitle(var Rect: TRect);
var
  Delta : Integer; // Dimension des coins du polygone.
begin
  Delta := Min(NB_PIXELS, (Rect.Bottom div 4)); // Un maximum de 25 pixels.

  // Sépare la section pour le titre.
  case FTitle.SectionPosition of
    tpTop : Rect.Bottom := Rect.Top  + FTitle.SectionSize + 1;
    tpLeft: Rect.Right  := Rect.Left + FTitle.SectionSize + 1;
    else    Rect.Top    := Rect.Bottom - FTitle.SectionSize - 1;
  end;

  Canvas.Brush.Color := FTitle.SectionColor;
  Canvas.Pen.Color   := FTitle.SectionColor;

  case FPanelType of
    ptRectangle : Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    ptRoundRect : begin
                    Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, FRoundRectDegree, FRoundRectDegree);
                    // On veut la base du titre carré.
                    case FTitle.SectionPosition of
                      tpTop    : Canvas.Rectangle(Rect.Left, Rect.Top + (FTitle.SectionSize div 2), Rect.Right, Rect.Bottom);
                      tpBottom : Canvas.Rectangle(Rect.Left, Rect.Bottom - FTitle.SectionSize, Rect.Right, Rect.Bottom - (FTitle.SectionSize div 2));
                      tpLeft   : Canvas.Rectangle(Rect.Left + (FTitle.SectionSize div 2), Rect.Top, Rect.Right, Rect.Bottom);
                    end;
                  end;
    ptTitleRoundRect :
                  begin
                    Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, FRoundRectDegree, FRoundRectDegree);

                    // Le côté opposé au titre est carré. On couvre donc la partie arrondie qu'on ne veut pas.
                    case FTitle.SectionPosition of
                      tpTop    : Canvas.Rectangle(Rect.Left, Rect.Top + Delta, Rect.Right, Rect.Bottom);
                      tpBottom : Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom - 10);
                      else
                        Canvas.Rectangle(Rect.Left + 10, Rect.Top, Rect.Right, Rect.Bottom);
                    end;

                    // On veut la base du titre carré.
                    case FTitle.SectionPosition of
                      tpTop    : Canvas.Rectangle(Rect.Left, Rect.Top + (FTitle.SectionSize div 2), Rect.Right, Rect.Bottom);
                      tpBottom : Canvas.Rectangle(Rect.Left, Rect.Bottom - FTitle.SectionSize, Rect.Right, Rect.Bottom - (FTitle.SectionSize div 2));
                      tpLeft   : Canvas.Rectangle(Rect.Left + (FTitle.SectionSize div 2), Rect.Top, Rect.Right, Rect.Bottom);
                    end;
                  end;
    ptOctogone  :
                  case FTitle.SectionPosition of
                    tpTop :
                        // La section de titre arrive plus bas que l'angle du panneau.
                        Canvas.Polygon([Point(Rect.Left, FTitle.SectionSize),
                        Point(Rect.Right, FTitle.SectionSize),
                        Point(Rect.Right, Delta),
                        Point(Rect.Right - Delta, Rect.Top),
                        Point(Rect.Left + Delta, Rect.Top),
                        Point(Rect.Left, Delta),
                        Point(Rect.Left, FTitle.SectionSize)]);
                    tpBottom :
                        // La section de titre arrive plus haut que l'angle du panneau.
                        Canvas.Polygon([Point(Rect.Left, Rect.Bottom - FTitle.SectionSize),
                        Point(Rect.Right, Rect.Bottom - FTitle.SectionSize),
                        Point(Rect.Right, Rect.Bottom - Delta),
                        Point(Rect.Right - Delta, Rect.Bottom),
                        Point(Rect.Left + Delta, Rect.Bottom),
                        Point(Rect.Left, Rect.Bottom - Delta),
                        Point(Rect.Left, Rect.Bottom - FTitle.SectionSize)]);
                    tpLeft :
                        if ((Rect.Bottom div 4) = NB_PIXELS) and
                          (FTitle.SectionSize = NB_PIXELS) then
                          // La section de titre arrive vis-à-vis l'angle du panneau.
                          Canvas.Polygon([Point(Rect.Left + Delta, Rect.Bottom),
                          Point(Rect.Left, Rect.Bottom - Delta),
                          Point(Rect.Left, Rect.Top + Delta),
                          Point(Rect.Left + Delta, Rect.Top),
                          Point(Rect.Left + Delta, Rect.Bottom)])
                        else
                          // La section de titre arrive plus à droite que l'angle du panneau.
                          Canvas.Polygon([Point(Rect.Left + FTitle.SectionSize, Rect.Bottom),
                          Point(Rect.Left + Delta, Rect.Bottom),
                          Point(Rect.Left, Rect.Bottom - Delta),
                          Point(Rect.Left, Rect.Top + Delta),
                          Point(Rect.Left + Delta, Rect.Top),
                          Point(Rect.Left + FTitle.SectionSize, Rect.Top),
                          Point(Rect.Left + FTitle.SectionSize, Rect.Bottom)]);
                  end; //  case FTitle.SectionPosition of (ptOctogone)
    ptHexagoneLeft :
                  case FTitle.SectionPosition of
                    tpTop :
                        // La section de titre arrive plus bas que l'angle du panneau.
                        Canvas.Polygon([Point(Rect.Left + Delta, Rect.Top),
                        Point(Rect.Left, Rect.Top + Delta),
                        Point(Rect.Left, Rect.Top + FTitle.SectionSize),
                        Point(Rect.Right, Rect.Top + FTitle.SectionSize),
                        Point(Rect.Right, Rect.Top),
                        Point(Rect.Left + Delta, Rect.Top)]);
                    tpBottom :
                        // La section de titre arrive plus haut que l'angle du panneau.
                        Canvas.Polygon([Point(Rect.Left, Rect.Bottom),
                        Point(Rect.Right - Delta, Rect.Bottom),
                        Point(Rect.Right, Rect.Bottom - Delta),
                        Point(Rect.Right, Rect.Bottom - FTitle.SectionSize),
                        Point(Rect.Left, Rect.Bottom - FTitle.SectionSize),
                        Point(Rect.Left, Rect.Bottom)]);
                    tpLeft :
                        if ((Rect.Bottom div 4) = NB_PIXELS) and
                          (FTitle.SectionSize = NB_PIXELS) then
                          // La section de titre arrive vis-à-vis l'angle du panneau.
                          Canvas.Polygon([Point(Rect.Left, Rect.Bottom),
                          Point(Rect.Left + Delta, Rect.Bottom),
                          Point(Rect.Left + Delta, Rect.Top),
                          Point(Rect.Left, Rect.Top + Delta),
                          Point(Rect.Left, Rect.Bottom)])
                        else
                          // La section de titre arrive plus à droite que l'angle du panneau.
                          Canvas.Polygon([Point(Rect.Left, Rect.Bottom),
                          Point(Rect.Left + FTitle.SectionSize, Rect.Bottom),
                          Point(Rect.Left + FTitle.SectionSize, Rect.Top),
                          Point(Rect.Left + Delta, Rect.Top),
                          Point(Rect.Left, Rect.Top + Delta),
                          Point(Rect.Left, Rect.Bottom)]);
                  end; //  case FTitle.SectionPosition of (ptHexagoneLeft)
    ptHexagoneRight :
                  case FTitle.SectionPosition of
                    tpTop :
                        // La section de titre arrive plus bas que l'angle du panneau.
                        Canvas.Polygon([Point(Rect.Left, Rect.Top),
                        Point(Rect.Left, Rect.Top + FTitle.SectionSize),
                        Point(Rect.Right, Rect.Top + FTitle.SectionSize),
                        Point(Rect.Right, Rect.Top + Delta),
                        Point(Rect.Right - Delta, Rect.Top),
                        Point(Rect.Left, Rect.Top)]);
                    tpBottom :
                        // La section de titre arrive plus haut que l'angle du panneau.
                        Canvas.Polygon([Point(Rect.Left, Rect.Bottom - FTitle.SectionSize),
                        Point(Rect.Left, Rect.Bottom - Delta),
                        Point(Rect.Left + Delta, Rect.Bottom),
                        Point(Rect.Right, Rect.Bottom),
                        Point(Rect.Right, Rect.Bottom - FTitle.SectionSize),
                        Point(Rect.Left, Rect.Bottom - FTitle.SectionSize)]);
                    tpLeft :
                        if ((Rect.Bottom div 4) = NB_PIXELS) and
                          (FTitle.SectionSize = NB_PIXELS) then
                          // La section de titre arrive vis-à-vis l'angle du panneau.
                          Canvas.Polygon([Point(Rect.Left, Rect.Top),
                          Point(Rect.Left, Rect.Bottom - Delta),
                          Point(Rect.Left + Delta, Rect.Bottom),
                          Point(Rect.Left + Delta, Rect.Top),
                          Point(Rect.Left, Rect.Top)])
                        else
                          // La section de titre arrive plus à droite que l'angle du panneau.
                          Canvas.Polygon([Point(Rect.Left, Rect.Top),
                          Point(Rect.Left, Rect.Bottom - Delta),
                          Point(Rect.Left + Delta, Rect.Bottom),
                          Point(Rect.Left + FTitle.SectionSize, Rect.Bottom),
                          Point(Rect.Left + FTitle.SectionSize, Rect.Top),
                          Point(Rect.Left, Rect.Top)]);
                  end; //  case FTitle.SectionPosition of (ptHexagoneRight)
  end; // case FPanelType of

  if FTitle.FTransparent.Active then
    DrawRectTransparent(Rect, FTitle.FTransparent.FPercent, FTitle.FSectionColor, FTitle.FTransparent.FType, False);
end;

{ --------------------------------------------------------------------
  Méthode : FontChanged.
  But     : Forcer à redessiner le panneau lorsque la police est modifiée.
  -------------------------------------------------------------------- }
procedure TGMPanel.FontChanged(Sender: TObject);
begin
  FTheme := ttCustom;
  Repaint;
//  Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : WMMOVE.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.WMMOVE(var Message: TMessage);
begin
  inherited;

  PostMessage(Self.Handle, CM_NEEDBACK, 0, 0);

  Message.Result := 0;
end;

{ --------------------------------------------------------------------
  Méthode : WMSIZE.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.WMSIZE(var Message: TMessage);
begin
  if Assigned(FBackBitmap) then
  begin
    FBackBitmap.Width  := Width;
    FBackBitmap.Height := Height;
  end;

  PostMessage(Self.Handle, CM_NEEDBACK, 0, 0);
end;

{ --------------------------------------------------------------------
  Méthode : CMNEEDBACK.
  But     : Mettre le background dans FBackBitmap.
  -------------------------------------------------------------------- }
procedure TGMPanel.CMNEEDBACK(var Message: TMessage);
begin
  GetBackGround(Self.Parent);
end;

{ --------------------------------------------------------------------
  Méthode : GetBackGround.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.GetBackGround(Parent: TWinControl);
var
  ABitmap : TBitmap;
//  A1      : TBitmap;
begin
  FBackBitmap.Width  := Width;
  FBackBitmap.height := Height;

  if Assigned(Parent) then
  begin
    ABitmap := TBitmap.Create;
    ABitmap.PixelFormat := pf24bit;
    ABitmap.Width  := -Parent.ClientOrigin.x + ClientOrigin.x + Width;
    ABitmap.Height := -Parent.ClientOrigin.y + ClientOrigin.y + Height;
    ABitmap.Canvas.Brush.Color := TCustomForm(Parent).Color;
    ABitmap.Canvas.FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));
    SendMessage(Parent.Handle, WM_PAINT, ABitmap.Canvas.Handle, 0);
    Application.ProcessMessages;

//    A1 := TBitmap.Create;
//    A1.PixelFormat := pf24Bit;
//    A1.width       := FBackBitmap.Width;
//    A1.Height      := FBackBitmap.Height;
//    A1.Canvas.Draw(0, 0, FBackBitmap);
    FBackBitmap.Canvas.Draw(Parent.ClientOrigin.x - ClientOrigin.x, Parent.ClientOrigin.y - ClientOrigin.y, ABitmap);
//    ABitmap.Free;

    Repaint;
//Invalidate;
//    A1.free;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : SetRoundRectDegree.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.SetRoundRectDegree(const Value: integer);
begin
  FRoundRectDegree := Value;
  FTheme := ttCustom;
  
  Repaint;
//Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : SetTheme.
  But     : Appliquer le thème sélectionné.
  -------------------------------------------------------------------- }
procedure TGMPanel.SetTheme(const Value: TThemeType);
begin
  FTheme := Value;

  case FTheme of
    ttNote:
      begin
        Color                  := $00DDF2F2; // Beige pale.
        Font.Color             := clMaroon;        
        FTitle.SectionColor    := $00C0DEDE; // Beige foncé.
        FTitle.SectionPosition := tpLeft;
        FTitle.SectionVisible  := True;
        FTitle.SectionSize     := 60;
        FTitle.Font.Color      := clOlive;
        FShadow.Visible        := False;
      end;
    ttBlue:
      begin
        Color := $00E3DED5;
        FTitle.SectionColor := RGB(0, 128, 192);
        FShadow.Visible := false;
        FTitle.Font.Color := clWhite;
      end;
    ttPurple:
      begin
        Color := $00CBC5C8;
        FTitle.SectionColor := clPurple;
        FShadow.Visible := false;
        FTitle.Font.Color := clWhite;
      end;
    ttGreen:
      begin
        Color := $00B9CEBB;
        FTitle.SectionColor := clGreen;
        FShadow.Visible := false;
        FTitle.Font.Color := clWhite;
      end;
    ttYellow:
      begin
        Color := $00BED2D8;
        FTitle.SectionColor := $00B9FFFF;
        FTitle.Font.Color := clBlack;
        FShadow.Visible := false;
      end;
    ttCream:
      begin
        Color := clWhite;
        FTitle.SectionColor := $00EFFBFF;
        FTitle.Font.Color := $00A5703A;
        FShadow.Visible := false;
      end;
    ttRed:
      begin
        Color := $00E1DBFD;
        FTitle.SectionColor := $00CBBFFF;
        FShadow.Visible := True;
        FTitle.Font.Color := $005A07AD;
        FShadow.Depth := 4;
        FShadow.Space := 10;
      end;
  end;

  Repaint;
//Invalidate;

  // Ré-assigne car les affectations ci-haut remet FTheme = ttCustom.
  FTheme := Value;
end;

(*
{ --------------------------------------------------------------------
  Méthode : SetTransparent.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.SetTransparent(const Value: TTransparent);
begin
  FTransparency := Value;

//  if Value > 0 then
//    Gradient.Active := False; // Mutuellement exclusif.

  Repaint;
//Invalidate;
end;
*)

{ --------------------------------------------------------------------
  Méthode : SetPanelType.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.SetPanelType(const Value: TPanelType);
begin
  FPanelType := Value;
  FTheme := ttCustom;
  
  Repaint;
//Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : TitleChanged.
  But     : Forcer à redessiner le panneau lorsque le titre est modifié.
  -------------------------------------------------------------------- }
procedure TGMPanel.TitleChanged(Sender: TObject);
begin
  FTheme := ttCustom;
  Repaint;
//Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : ShadowChanged.
  But     : Forcer à redessiner le panneau lorsque l'ombre est modifié.
  -------------------------------------------------------------------- }
procedure TGMPanel.ShadowChanged(Sender: TObject);
begin
  FTheme := ttCustom;
  Repaint;
//Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : PictureChanged.
  But     : Forcer à redessiner le panneau lorsque l'image est modifiée.
  -------------------------------------------------------------------- }
procedure TGMPanel.PictureChanged(Sender: TObject);
begin
  FTheme := ttCustom;
  Repaint;
//Invalidate;
end;

{ --------------------------------------------------------------------
  Méthode : TransparentChange.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.TransparentChange(Sender: TObject);
begin  
  FTheme := ttCustom;
  Repaint;
end;

{ --------------------------------------------------------------------
  Méthode : GradientChanged.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanel.GradientChanged(Sender: TObject);
begin
  FTheme := ttCustom;

//  if Gradient.Active then
//    Transparency := 0;  // Mutuellement exclusif.

  Repaint;
//Invalidate;
end;

{ ******************************************************************** }
{ ****************************** TShadow ***************************** }
{ ******************************************************************** }

{ --------------------------------------------------------------------
  Méthode : ChangeOccured.
  But     : Indiquer qu'une modification a été apportée à l'ombre.
  -------------------------------------------------------------------- }
procedure TShadow.ChangeOccured;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ --------------------------------------------------------------------
  Méthode : Create.
  But     :
  -------------------------------------------------------------------- }
constructor TShadow.Create;
begin
  FColor        := clGray;
  FVisible      := False;
  FDepth        := 5;
  FSpace        := 5;
  FTransparency := 0;
  FPosition     := spBottomRight;
end;

{ --------------------------------------------------------------------
  Méthode : SetColor.
  But     :
  -------------------------------------------------------------------- }
procedure TShadow.SetColor(Value: TColor);
begin
  FColor := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetDepth.
  But     :
  -------------------------------------------------------------------- }
procedure TShadow.SetDepth(Value: Integer);
begin
  FDepth := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetPosition.
  But     :
  -------------------------------------------------------------------- }
procedure TShadow.SetPosition(Value: TShadowPosition);
begin
  FPosition := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetSpace.
  But     :
  -------------------------------------------------------------------- }
procedure TShadow.SetSpace(Value: Integer);
begin
  FSpace := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetTransparency.
  But     :
  -------------------------------------------------------------------- }
procedure TShadow.SetTransparency(Value: Integer);
begin
  if Value < 0 then
    FTransparency := 0
  else
  if Value > 100 then
    FTransparency := 100
  else
    FTransparency := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetVisible.
  But     :
  -------------------------------------------------------------------- }
procedure TShadow.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  ChangeOccured;
end;

{ ******************************************************************** }
{ ******************************* TTitle ***************************** }
{ ******************************************************************** }

{ --------------------------------------------------------------------
  Méthode : ChangeOccured.
  But     : Indiquer qu'une modification a été apportée au titre.
  -------------------------------------------------------------------- }
procedure TTitle.ChangeOccured;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ --------------------------------------------------------------------
  Méthode : Create.
  But     :
  -------------------------------------------------------------------- }
constructor TTitle.Create;
begin
  inherited;

  // Initialisations.
  FText            := '';
  FFont            := TFont.Create;
  FFont.Name       := 'Arial';
  FFont.Color      := clWhite;
  FTop             := 10;
  FLeft            := 10;
  FSectionVisible  := True;
  FSectionPosition := tpTop;
  FSectionSize     := NB_PIXELS;
  FSectionColor    := clTeal;
  FVAlign          := vaCenter;
  FHAlign          := haCenter;

  FTransparent     := TTransparent.Create;
  FTransparent.OnChange := TitleChange;
end;

{ --------------------------------------------------------------------
  Méthode : Destroy.
  But     :
  -------------------------------------------------------------------- }
destructor TTitle.Destroy;
begin
  inherited;

  FFont.Free;
end;

{ --------------------------------------------------------------------
  Méthode : SetFont.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetFont(const Value: TFont);
begin
  FFont := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetHAlign.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetHAlign(const Value: THorizontalAlign);
begin
  FHAlign := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetVAlign.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetVAlign(const Value: TVerticalAlign);
begin
  FVAlign := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetLeft.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetLeft(const Value: Integer);
begin
  FLeft := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetSectionColor.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetSectionColor(const Value: TColor);
begin
  FSectionColor := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetSectionPosition.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetSectionPosition(const Value: TTitlePosition);
begin
  FSectionPosition := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetSectionSize.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetSectionSize(const Value: Integer);
begin
  if Value < NB_PIXELS then
    FSectionSize := NB_PIXELS
  else
    FSectionSize := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetSectionVisible.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetSectionVisible(const Value: Boolean);
begin
  FSectionVisible := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetText.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetText(const Value: String);
begin
  FText := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetTop.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.SetTop(const Value: Integer);
begin
  FTop := Value;
  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : TitleChange.
  But     :
  -------------------------------------------------------------------- }
procedure TTitle.TitleChange(Sender: TObject);
begin
  ChangeOccured;
end;

{ TGMPanelPicture }

{ --------------------------------------------------------------------
  Méthode : ChangeOccured.
  But     : Indiquer qu'une modification a été apportée à l'image.
  -------------------------------------------------------------------- }
procedure TGMPanelPicture.ChangeOccured;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ --------------------------------------------------------------------
  Méthode : Create.
  But     :
  -------------------------------------------------------------------- }
constructor TGMPanelPicture.Create;
begin
  FPicture     := TBitmap.Create;
  FStyle       := psTile;
  FTransparent := False;
end;

{ --------------------------------------------------------------------
  Méthode : Destroy.
  But     :
  -------------------------------------------------------------------- }
destructor TGMPanelPicture.Destroy;
begin
  FPicture.Free;

  inherited;
end;

{ --------------------------------------------------------------------
  Méthode : SetPicture.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanelPicture.SetPicture(const Value: TBitMap);
begin
  if (Value = nil) or (Value.Height = 0) then
  begin
    FPicture.Height := 0;
    FPicture.Width  := 0;
  end
  else
  begin
    Value.Transparent := True;
    FPicture.Assign(Value);
  end;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetPictureStyle.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanelPicture.SetPictureStyle(const Value: TPictureStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    ChangeOccured;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : SetPictureTransparent.
  But     :
  -------------------------------------------------------------------- }
procedure TGMPanelPicture.SetPictureTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    ChangeOccured;
  end;
end;


{ TGradient }

{ --------------------------------------------------------------------
  Méthode : ChangeOccured.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.ChangeOccured;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ --------------------------------------------------------------------
  Méthode : Create.
  But     :
  -------------------------------------------------------------------- }
constructor TGradient.Create;
begin
  FActive  := False;
  FColor_1 := clBtnFace;
  FColor_2 := clWhite;
  FColor_3 := clAqua;
  FType    := gtHorizontal;
  FPercent := 100;
end;

{ --------------------------------------------------------------------
  Méthode : SetGradientActive.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.SetGradientActive(const Value: Boolean);
begin
  FActive := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetGradientColor_1.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.SetGradientColor_1(const Value: TColor);
begin
  FColor_1 := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetGradientColor_2.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.SetGradientColor_2(const Value: TColor);
begin
  FColor_2 := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetGradientColor_3.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.SetGradientColor_3(const Value: TColor);
begin
  FColor_3 := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetGradientPercent.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.SetGradientPercent(Value: integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > 100 then
    Value := 100;

  FPercent := Value;

  ChangeOccured;
end;

{ --------------------------------------------------------------------
  Méthode : SetGradientType.
  But     :
  -------------------------------------------------------------------- }
procedure TGradient.SetGradientType(const Value: TGradientType);
begin
  FType := Value;

  ChangeOccured;
end;


{ ******************************************************************** }
{ **************************** TTransparent ************************** }
{ ******************************************************************** }

{ --------------------------------------------------------------------
  Méthode : ChangeOccured.
  But     :
  -------------------------------------------------------------------- }
procedure TTransparent.ChangeOccured;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ --------------------------------------------------------------------
  Méthode : Create.
  But     :
  -------------------------------------------------------------------- }
constructor TTransparent.Create;
begin
  FActive  := False;
  FPercent := 50;
  FType    := gttNone;
end;

{ --------------------------------------------------------------------
  Méthode : SetTransparentActive.
  But     :
  -------------------------------------------------------------------- }
procedure TTransparent.SetTransparentActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    ChangeOccured;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : SetTransparentPercent.
  But     :
  -------------------------------------------------------------------- }
procedure TTransparent.SetTransparentPercent(Value: Integer);
begin
  if FPercent <> Value then
  begin
    FPercent := Value;
    ChangeOccured;
  end;
end;

{ --------------------------------------------------------------------
  Méthode : SetTransparentType.
  But     :
  -------------------------------------------------------------------- }
procedure TTransparent.SetTransparentType(const Value: TTransparentType);
begin
  if FType <> Value then
  begin
    FType := Value;
    ChangeOccured;
  end;
end;

end.

