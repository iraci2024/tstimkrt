{*************************************************************************}
{ TMS TAdvRichEditorRuler component                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2020                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : https://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorRuler;

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils, Classes, Types, Graphics, Controls, Dialogs, ComCtrls,
  AdvCustomControl, AdvRichEditor, AdvGraphics, AdvGraphicsTypes, AdvTypes,
  AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : First version
  // v1.0.0.1 : Fixed : Initial value for AutoWidth and AutoPosition set to True
  //          : Improved : AutoPosition and drawing for High DPI
  // v1.0.0.1 : Fixed : Position on creation
type
  TAdvRichEditorCustomRuler = class;

  TAdvRichEditorRulerPosition = (rpTop, rpBottom);
  TAdvRichEditorRulerTabType = (ttLeftTab, ttLeftIndent, ttRightIndent, ttHangingIndent, ttLeftMargin, ttRightMargin, ttNone);
  TAdvRichEditorRulerTabMove = (tmTickMark, tmStep, tmLabel, tmAll, tmPixel);

  TAdvRichEditorRulerTickMarks = class(TPersistent)
  private
    FColor: TAdvGraphicsColor;
    FContinuousLabelSteps: Boolean;
    FLabelStep: Integer;
    FMarginColor: TAdvGraphicsColor;
    FSize: Integer;
    FSpacing: Integer;
    FStep: Integer;
    FStepHeight: Integer;
    FOnChanged: TNotifyEvent;

    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetContinuousLabelSteps(const Value: Boolean);
    procedure SetLabelStep(const Value: Integer);
    procedure SetMarginColor(const Value: TAdvGraphicsColor);
    procedure SetSize(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetStep(const Value: Integer);
    procedure SetStepHeight(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TAdvGraphicsColor read FColor write SetColor default gcGray;   
    property ContinuousLabelSteps: Boolean read FContinuousLabelSteps write SetContinuousLabelSteps default False;
    property LabelStep: Integer read FLabelStep write SetLabelStep default 50;
    property MarginColor: TAdvGraphicsColor read FMarginColor write SetMarginColor default clNone;
    property Size: Integer read FSize write SetSize default 1;
    property Spacing: Integer read FSpacing write SetSpacing default 10;
    property Step: Integer read FStep write SetStep default 25;
    property StepHeight: Integer read FStepHeight Write SetStepHeight default 6;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TAdvRichEditorRulerTabItem = class(TCollectionItem)
  private
    FIndent: Integer;
    procedure SetIndent(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Indent: integer read FIndent write SetIndent default -1;
  end;

  TAdvRichEditorRulerTabCollection = class(TOwnedCollection)
  private
    FOwner: TAdvRichEditorCustomRuler;
    function GetItem(Index: Integer): TAdvRichEditorRulerTabItem;
    procedure SetItem(Index: Integer; const Value: TAdvRichEditorRulerTabItem);
  public
    constructor Create(AOwner: TAdvRichEditorCustomRuler);
    function Add: TAdvRichEditorRulerTabItem;
    function AddTab(AIndent: Integer = 0): integer; overload;
    function AddTab(ATabItem: TAdvRichEditorRulerTabItem): integer; overload;
    procedure EndUpdate; override;
    function Insert(Index: Integer): TAdvRichEditorRulerTabItem;
    property AdvRichEditorCustomRuler: TAdvRichEditorCustomRuler read FOwner;
    property Items[Index: Integer]: TAdvRichEditorRulerTabItem read GetItem write SetItem; default;
  end;

  TAdvRichEditorRulerTabEvent = procedure(Sender: TObject; Index: Integer; Indent: Integer) of object;
  TAdvRichEditorRulerEvent = procedure(Sender: TObject; Indent: Integer) of object;

  TAdvRichEditorCustomRuler = class(TAdvCustomControl, ITMSStyle)
  private
    FRichEditor: TAdvRichEditor;
    FAutoPosition: Boolean;
    FAutoWidth: Boolean;
    FBorderColor: TAdvGraphicsColor;
    FColor: TAdvGraphicsColor;
    FDesignTime: Boolean;
    FDPIScale: Single;
    FFontColorMargin: TAdvGraphicsColor;
    FHangingIndent: Integer;
    FIndentBorderColor: TAdvGraphicsColor;
    FIndentColor: TAdvGraphicsColor;
    FLeftIndent: Integer;
    FLeftIndentBottomGlyph: TAdvBitmap;
    FLeftIndentTopGlyph: TAdvBitmap;
    FLeftMargin: Integer;
    FMarginColor: TAdvGraphicsColor;
    FMouseDown: Boolean;
    FMouseInRuler: Boolean;
    FMoveTabIdx: Integer;
    FMoveTabPos: Integer;
    FMoveTabType: TAdvRichEditorRulerTabType;
    FMovingTabColor: TAdvGraphicsColor;
    FPaddingBottom: Integer;
    FPaddingLeft: Integer;
    FPaddingRight: Integer;
    FPaddingTop: Integer;
    FPosition: TAdvRichEditorRulerPosition;
    FRightIndent: Integer;
    FRightIndentGlyph: TAdvBitmap;
    FRightMargin: Integer;
    FRulerBorderWidth: Integer;
    FRulerBorderColor: TAdvGraphicsColor;
    FRulerColor: TAdvGraphicsColor;
    FShowHangingIndent: Boolean;
    FShowLeftIndent: Boolean;
    FShowLeftMargin: Boolean;
    FShowRightIndent: Boolean;
    FShowRightMargin: Boolean;
    FTabColor: TAdvGraphicsColor;
    FTabMove: TAdvRichEditorRulerTabMove;
    FTabs: TAdvRichEditorRulerTabCollection;
    FTabSize: Integer;
    FTickMarks: TAdvRichEditorRulerTickMarks;
    FTMSStyle: TTMSStyle;
    FZeroPos: Integer;
    FOnHangingIndentChange: TAdvRichEditorRulerEvent;
    FOnHangingIndentChanged: TAdvRichEditorRulerEvent;
    FOnLeftIndentChange: TAdvRichEditorRulerEvent;
    FOnLeftIndentChanged: TAdvRichEditorRulerEvent;
    FOnLeftMarginChange: TAdvRichEditorRulerEvent;
    FOnLeftMarginChanged: TAdvRichEditorRulerEvent;
    FOnRightIndentChange: TAdvRichEditorRulerEvent;
    FOnRightIndentChanged: TAdvRichEditorRulerEvent;
    FOnRightMarginChange: TAdvRichEditorRulerEvent;
    FOnRightMarginChanged: TAdvRichEditorRulerEvent;
    FOnTabAdded: TAdvRichEditorRulerTabEvent;
    FOnTabChange: TAdvRichEditorRulerTabEvent;
    FOnTabChanged: TAdvRichEditorRulerTabEvent;
    FOnTabRemove: TAdvRichEditorRulerTabEvent;
    FOnTickMarksChanged: TNotifyEvent;
    FOnTabModified: TAdvRichEditorRulerTabEvent;

    procedure DrawBackGroundColor(AGraphics: TAdvGraphics);
    procedure DrawBorder(AGraphics: TAdvGraphics);
    procedure DrawHangingIndent(AGraphics: TAdvGraphics);
    procedure DrawLabelStep(AGraphics: TAdvGraphics; AXPos: Integer; AText: string; AColor: TAdvGraphicsColor);
    procedure DrawLeftIndent(AGraphics: TAdvGraphics);
    procedure DrawLeftMargin(AGraphics: TAdvGraphics);
    procedure DrawRightIndent(AGraphics: TAdvGraphics);
    procedure DrawRightMargin(AGraphics: TAdvGraphics);
    procedure DrawRulerColor(AGraphics: TAdvGraphics);
    procedure DrawStep(AGraphics: TAdvGraphics; AXPos: Integer; AColor: TAdvGraphicsColor);
    procedure DrawTabs(AGraphics: TAdvGraphics);
    procedure DrawTick(AGraphics: TAdvGraphics; AXPos: Integer; AColor: TAdvGraphicsColor);
    procedure DrawTickMarks(AGraphics: TAdvGraphics);
    function GetActiveRect: TRect;
    function GetBoxRect: TRectF;
    function GetClosestNewTabVal(XPos: Integer): Integer;
    function GetClosestPoint(Xpos: Integer): Integer;
    function GetEnabledEx: Boolean;
    procedure GetIndentPath(ATabType: TAdvRichEditorRulerTabType; var APath: TAdvGraphicsPath); overload;
    procedure GetIndentPath(XPos: Single; ATabType: TAdvRichEditorRulerTabType; var APath: TAdvGraphicsPath); overload;
    function GetIndentPos(ATabType: TAdvRichEditorRulerTabType): Integer;
    function GetLeftMarginPos: Integer;
    function GetLeftMarginRect: TRect;
    function GetRightMarginPos: Integer;
    function GetRightMarginRect: TRect;
    function GetRulerBottom: Integer;
    function GetRulerLeft: Integer;
    function GetRulerTop: Integer;
    function GetWorkHeight: Integer;
    function GetWorkWidth: Integer;
    function PointInHangingBox(APoint: TPointF): Boolean;
    function PointInHangingIndent(APoint: TPointF): Boolean;
    function PointInLeftIndent(APoint: TPointF): Boolean;
    function PointInRightIndent(APoint: TPointF): Boolean;
    function PointInRuler(APoint: TPointF): Boolean;
    function PointInTabs(APoint: TPointF): Integer;
    function PointOnLeftMargin(APoint: TPointF): Boolean;
    function PointOnRightMargin(APoint: TPointF): Boolean;
    procedure SetRichEditor(const Value: TAdvRichEditor);
    procedure SetAutoPosition(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetBorderColor(const Value: TAdvGraphicsColor);
    procedure SetColor(const Value: TAdvGraphicsColor);
    procedure SetEnabledEx(const Value: Boolean);
    procedure SetFontColorMargin(const Value: TAdvGraphicsColor);
    procedure SetHangingIndent(const Value: Integer);
    procedure SetIndentBorderColor(const Value: TAdvGraphicsColor);
    procedure SetIndentColor(const Value: TAdvGraphicsColor);
    procedure SetIndentVal(Xpos: Integer; ATabType: TAdvRichEditorRulerTabType; AIndex: Integer = -1);
    procedure SetLeftIndent(const Value: Integer);
    procedure SetLeftIndentBottomGlyph(const Value: TAdvBitmap);
    procedure SetLeftIndentTopGlyph(const Value: TAdvBitmap);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetMarginColor(const Value: TAdvGraphicsColor);
    procedure SetPaddingBottom(const Value: Integer);
    procedure SetPaddingLeft(const Value: Integer);
    procedure SetPaddingRight(const Value: Integer);
    procedure SetPaddingTop(const Value: Integer);
    procedure SetPosition;
    procedure SetRightIndent(const Value: Integer);
    procedure SetRightIndentGlyph(const Value: TAdvBitmap);
    procedure SetRightMargin(const Value: Integer);
    procedure SetRulerBorderColor(const Value: TAdvGraphicsColor);
    procedure SetRulerBorderWidth(const Value: Integer);
    procedure SetRulerColor(const Value: TAdvGraphicsColor);
    procedure SetShowHangingIndent(const Value: Boolean);
    procedure SetShowLeftIndent(const Value: Boolean);
    procedure SetShowLeftMargin(const Value: Boolean);
    procedure SetShowRightIndent(const Value: Boolean);
    procedure SetShowRightMargin(const Value: Boolean);
    procedure SetSize;
    procedure SetTabColor(const Value: TAdvGraphicsColor);
    procedure SetTabMove(const Value: TAdvRichEditorRulerTabMove);
    procedure SetTabSize(const Value: Integer);
    procedure SetUIStyle(const Value: TTMSStyle);
    procedure SetMovingTabColor(const Value: TAdvGraphicsColor);
    procedure SyncTabs;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    {$IFNDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    {$IFDEF DELPHIXE10_LVL}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure HandleTabsModified(Index: Integer; Indent: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(Value: TWinControl); override;

    property AutoPosition: Boolean read FAutoPosition write SetAutoPosition default True;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    property BorderColor: TAdvGraphicsColor read FBorderColor write SetBorderColor default clNone;
    property Color: TAdvGraphicsColor read FColor write SetColor default gcWhite;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx default True;
    property FontColorMargin: TAdvGraphicsColor read FFontColorMargin write SetFontColorMargin default clNone;
    property HangingIndent: Integer read FHangingIndent write SetHangingIndent default 50;
    property IndentBorderColor: TAdvGraphicsColor read FIndentBorderColor write SetIndentBorderColor default clBlack;
    property IndentColor: TAdvGraphicsColor read FIndentColor write SetIndentColor default clWhite;
    property LeftIndent: Integer read FLeftIndent write SetLeftIndent default 50;
    property LeftIndentBottomGlyph: TAdvBitmap read FLeftIndentBottomGlyph write SetLeftIndentBottomGlyph;
    property LeftIndentTopGlyph: TAdvBitmap read FLeftIndentTopGlyph write SetLeftIndentTopGlyph;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 50;
    property MarginColor: TAdvGraphicsColor read FMarginColor write SetMarginColor default gcLightgray;
    property MovingTabColor: TAdvGraphicsColor read FMovingTabColor write SetMovingTabColor default gcLightgray;
    property PaddingBottom: Integer read FPaddingBottom write SetPaddingBottom default 7;
    property PaddingLeft: Integer read FPaddingLeft write SetPaddingLeft default 4;
    property PaddingRight: Integer read FPaddingRight write SetPaddingRight default 4;
    property PaddingTop: Integer read FPaddingTop write SetPaddingTop default 7;
    property Position: TAdvRichEditorRulerPosition read FPosition write FPosition default rpTop;
    property RichEditor: TAdvRichEditor read FRichEditor write SetRichEditor;
    property RightIndent: Integer read FRightIndent write SetRightIndent;
    property RightIndentGlyph: TAdvBitmap read FRightIndentGlyph write SetRightIndentGlyph;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 46;
    property RulerBorderColor: TAdvGraphicsColor read FRulerBorderColor write SetRulerBorderColor default clGray;
    property RulerBorderWidth: Integer read FRulerBorderWidth write SetRulerBorderWidth default 1;
    property RulerColor : TAdvGraphicsColor read FRulerColor write SetRulerColor default clNone;
    property ShowHangingIndent: Boolean read FShowHangingIndent write SetShowHangingIndent default False;
    property ShowLeftIndent: Boolean read FShowLeftIndent write SetShowLeftIndent default True;
    property ShowLeftMargin: Boolean read FShowLeftMargin write SetShowLeftMargin default True;
    property ShowRightIndent: Boolean read FShowRightIndent write SetShowRightIndent default True;
    property ShowRightMargin: Boolean read FShowRightMargin write SetShowRightMargin default True;
    property Tabs: TAdvRichEditorRulerTabCollection read FTabs write FTabs;
    property TabColor: TAdvGraphicsColor read FTabColor write SetTabColor default clBlack;
    property TabMove: TAdvRichEditorRulerTabMove read FTabMove write SetTabMove default tmAll;
    property TabSize: Integer read FTabSize write SetTabSize default 5;
    property TickMarks: TAdvRichEditorRulerTickMarks read FTickMarks write FTickMarks;
    property UIStyle: TTMSStyle read FTMSStyle write SetUIStyle default tsCustom;

    procedure DoHangingIndentChange(Indent: Integer); virtual;
    procedure DoHangingIndentChanged(Indent: Integer); virtual;
    procedure DoLeftIndentChange(Indent: Integer); virtual;
    procedure DoLeftIndentChanged(Indent: Integer); virtual;
    procedure DoLeftMarginChange(Indent: Integer); virtual;
    procedure DoLeftMarginChanged(Indent: Integer); virtual;
    procedure DoRightIndentChange(Indent: Integer); virtual;
    procedure DoRightIndentChanged(Indent: Integer); virtual;
    procedure DoRightMarginChange(Indent: Integer); virtual;
    procedure DoRightMarginChanged(Indent: Integer); virtual;
    procedure DoTabAdded(Index: Integer; Indent: Integer); virtual;
    procedure DoTabChange(Index: Integer; Indent: Integer); virtual;
    procedure DoTabChanged(Index: Integer; Indent: Integer); virtual;
    procedure DoTabModified(Index: Integer; Indent: Integer); virtual;
    procedure DoTabRemove(Index: Integer; Indent: Integer); virtual;
    procedure DoTickMarksChanged(Sender: TObject); virtual;
    procedure DoUpdate(Sender: TObject); virtual;

    property OnHangingIndentChange: TAdvRichEditorRulerEvent read FOnHangingIndentChange write FOnHangingIndentChange;
    property OnHangingIndentChanged: TAdvRichEditorRulerEvent read FOnHangingIndentChanged write FOnHangingIndentChanged;
    property OnLeftIndentChange: TAdvRichEditorRulerEvent read FOnLeftIndentChange write FOnLeftIndentChange;
    property OnLeftIndentChanged: TAdvRichEditorRulerEvent read FOnLeftIndentChanged write FOnLeftIndentChanged;
    property OnLeftMarginChange: TAdvRichEditorRulerEvent read FOnLeftMarginChange write FOnLeftMarginChange;
    property OnLeftMarginChanged: TAdvRichEditorRulerEvent read FOnLeftMarginChanged write FOnLeftMarginChanged;
    property OnRightIndentChange: TAdvRichEditorRulerEvent read FOnRightIndentChange write FOnRightIndentChange;
    property OnRightIndentChanged: TAdvRichEditorRulerEvent read FOnRightIndentChanged write FOnRightIndentChanged;
    property OnRightMarginChange: TAdvRichEditorRulerEvent read FOnRightMarginChange write FOnRightMarginChange;
    property OnRightMarginChanged: TAdvRichEditorRulerEvent read FOnRightMarginChanged write FOnRightMarginChanged;
    property OnTabAdded: TAdvRichEditorRulerTabEvent read FOnTabAdded write FOnTabAdded;
    property OnTabChange: TAdvRichEditorRulerTabEvent read FOnTabChange write FOnTabChange;
    property OnTabChanged: TAdvRichEditorRulerTabEvent read FOnTabChanged write FOnTabChanged;
    property OnTabModified: TAdvRichEditorRulerTabEvent read FOnTabModified write FOnTabModified;
    property OnTabRemove: TAdvRichEditorRulerTabEvent read FOnTabRemove write FOnTabRemove;
    property OnTickMarksChanged: TNotifyEvent read FOnTickMarksChanged write FOnTickMarksChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(AGraphics: TAdvGraphics; ARect: TRectF); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure UpdateRichEditor; virtual;
  end;

  TAdvRichEditorHorizontalRuler = class(TAdvRichEditorCustomRuler)
  published
    property AutoPosition;
    property AutoWidth;
    property BorderColor;
    property Color;
    property Enabled;
    property FontColorMargin;
    property HangingIndent;
    property IndentBorderColor;
    property IndentColor;
    property LeftIndent;
    property LeftIndentBottomGlyph;
    property LeftIndentTopGlyph;
    property LeftMargin;
    property MarginColor;
    property MovingTabColor;
    property PaddingBottom;
    property PaddingLeft;
    property PaddingRight;
    property PaddingTop;
    property Position;
    property RichEditor;
    property RightIndent;
    property RightIndentGlyph;
    property RightMargin;
    property RulerBorderColor;
    property RulerColor;
    property ShowHangingIndent;
    property ShowLeftIndent;
    property ShowLeftMargin;
    property ShowRightIndent;
    property ShowRightMargin;
    property TabColor;
    property TabMove;
    property Tabs;
    property TabSize;
    property TickMarks;
    property UIStyle;

    property OnLeftMarginChange;
    property OnLeftMarginChanged;
    property OnRightMarginChange;
    property OnRightMarginChanged;
    property OnLeftIndentChange;
    property OnLeftIndentChanged;
    property OnRightIndentChange;
    property OnRightIndentChanged;
    property OnHangingIndentChange;
    property OnHangingIndentChanged;
    property OnTabAdded;
    property OnTabModified;
    property OnTabChange;
    property OnTabChanged;
    property OnTabRemove;
  end;

implementation
uses
  Forms, System.UITypes;

{ TAdvRichEditorCustomRuler }

{$IFNDEF DELPHIXE10_LVL}
procedure TAdvRichEditorCustomRuler.ChangeScale(M, D: Integer);
{$ENDIF}
{$IFDEF DELPHIXE10_LVL}
procedure TAdvRichEditorCustomRuler.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ENDIF}
begin
  inherited;
  FDPIScale := GetDPIScale(Self, Canvas);
  Invalidate;
end;

constructor TAdvRichEditorCustomRuler.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  FMouseDown := False;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FMoveTabType := ttNone;
  FMoveTabIdx := -1;

  FDPIScale := 1;
  FPaddingBottom := 7;
  FPaddingLeft := 4;
  FPaddingRight := 4;
  FPaddingTop := 7;

  FZeroPos := 0;

  FTabMove := tmAll;

  FPosition := rpTop;

  FColor := gcWhite;
  FRulerColor := clNone;
  FBorderColor := clNone;
  FMarginColor := gcLightgray;
  FFontColorMargin := clNone;
  FTabColor := clBlack;
  FIndentColor := clWhite;
  FIndentBorderColor := clBlack;
  FRulerBorderColor := clGray;
  MovingTabColor := gcLightgray;

  FLeftIndentBottomGlyph := TAdvBitmap.Create;
  FLeftIndentTopGlyph := TAdvBitmap.Create;
  FRightIndentGlyph := TAdvBitmap.Create;

  FRulerBorderWidth := 1;

  FLeftMargin := 50;
  FRightMargin := 46;
  FLeftIndent := 0;
  FHangingIndent := 0;
  FRightIndent := 0;

  FShowLeftMargin := True;
  FShowRightMargin := True;
  ShowLeftIndent := True;
  ShowRightIndent := True;
  ShowHangingIndent := False;

  Tabs := TAdvRichEditorRulerTabCollection.Create(Self);
  TabSize := 5;

  FTickMarks := TAdvRichEditorRulerTickMarks.Create;
  FTickMarks.OnChanged := DoTickMarksChanged;

  ParentColor := True;

  if FDesignTime then
  begin
    SetComponentStyle(GetDefaultStyle(AOwner));
    ParentFont := SetParentFontForStyle(FTMSSTyle);
  end;

  FTMSSTyle := tsCustom;

  FZeroPos := FLeftMargin;

  Height := 30;
  Width := 400;


  FAutoWidth := True;
  FAutoPosition := True;

  if FDesignTime and Assigned(AOwner) and (AOwner is TCustomForm) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
      if (AOwner.Components[i] is TAdvRichEditor) then
      begin
        FRichEditor := AOwner.Components[i] as TAdvRichEditor;
        break;
      end;
  end;
end;

procedure TAdvRichEditorCustomRuler.CreateWnd;
begin
  inherited;
  FDPIScale := GetDPIScale(Self, Canvas);
end;

destructor TAdvRichEditorCustomRuler.Destroy;
begin
  if Assigned(FRichEditor) then
    FRichEditor.OnRulerUpdate := nil;

  FLeftIndentBottomGlyph.Free;
  FLeftIndentTopGlyph.Free;
  FRightIndentGlyph.Free;
  FTickMarks.Free;
  Tabs.Free;
  inherited;
end;

procedure TAdvRichEditorCustomRuler.DoHangingIndentChange(Indent: Integer);
begin
  if Assigned(OnHangingIndentChange) then
    OnHangingIndentChange(Self, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoHangingIndentChanged(Indent: Integer);
begin
  if Assigned(OnHangingIndentChanged) then
    OnHangingIndentChanged(Self, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoLeftIndentChange(Indent: Integer);
begin
  if Assigned(OnLeftIndentChange) then
    OnLeftIndentChange(Self, Indent);

  if Assigned(FRichEditor) then
    FRichEditor.SetSelectionIndentPos(Indent);
end;

procedure TAdvRichEditorCustomRuler.DoLeftIndentChanged(Indent: Integer);
begin
  if Assigned(OnLeftIndentChanged) then
    OnLeftIndentChanged(Self, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoLeftMarginChange(Indent: Integer);
begin
  if Assigned(OnLeftMarginChange) then
    OnLeftMarginChange(Self, Indent);

  if Assigned(FRichEditor) then
    FRichEditor.PageMargin.Left := Indent;
end;

procedure TAdvRichEditorCustomRuler.DoLeftMarginChanged(Indent: Integer);
begin
  if Assigned(OnLeftMarginChanged) then
    OnLeftMarginChanged(Self, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoRightIndentChange(Indent: Integer);
begin
  if Assigned(OnRightIndentChange) then
    OnRightIndentChange(Self, Indent);

  if Assigned(FRichEditor) then
    FRichEditor.SetSelectionRightIndent(Indent);
end;

procedure TAdvRichEditorCustomRuler.DoRightIndentChanged(Indent: Integer);
begin
  if Assigned(OnRightIndentChanged) then
    OnRightIndentChanged(Self, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoRightMarginChange(Indent: Integer);
begin
  if Assigned(OnRightMarginChange) then
    OnRightMarginChange(Self, Indent);

  if Assigned(FRichEditor) then
    FRichEditor.PageMargin.Right := Indent;
end;

procedure TAdvRichEditorCustomRuler.DoRightMarginChanged(Indent: Integer);
begin
  if Assigned(OnRightMarginChanged) then
    OnRightMarginChanged(Self, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoTabAdded(Index, Indent: Integer);
begin
  if Assigned(OnTabAdded) then
    OnTabAdded(Self, Index, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoTabChange(Index, Indent: Integer);
begin
  if Assigned(OnTabChange) then
    OnTabChange(Self, Index, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoTabChanged(Index, Indent: Integer);
begin
  if Assigned(OnTabChanged) then
    OnTabChanged(Self, Index, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoTabModified(Index, Indent: Integer);
begin
  if Assigned(OnTabModified) then
    OnTabModified(Self, Index, Indent);
end;

procedure TAdvRichEditorCustomRuler.DoTabRemove(Index, Indent: Integer);
begin
if Assigned(OnTabRemove) then
    OnTabRemove(Self, Index, Indent);
end;

procedure TAdvRichEditorCustomRuler.Draw(AGraphics: TAdvGraphics;
  ARect: TRectF);
begin
  DrawBackGroundColor(AGraphics);
  DrawRulerColor(AGraphics);

  // -- Draw Margin -- //
  DrawLeftMargin(AGraphics);
  DrawRightMargin(AGraphics);

  // -- Draw TickMarks -- //
  DrawTickMarks(AGraphics);

  // -- Draw Border -- //
  DrawBorder(AGraphics);

  // -- Draw Left Indent -- //
  DrawLeftIndent(AGraphics);
  // -- Draw Right Indent -- //
  DrawRightIndent(AGraphics);
  // -- Draw Hanging Indent -- //
  DrawHangingIndent(AGraphics);

  // -- Draw Tabs -- //
  DrawTabs(AGraphics);
end;

procedure TAdvRichEditorCustomRuler.DrawBackGroundColor(AGraphics: TAdvGraphics);
begin
  if ParentColor then
    AGraphics.Fill.Color := Parent.Brush.Color
  else
    AGraphics.Fill.Color := Color;
  AGraphics.Fill.Kind := gfkSolid;
  AGraphics.Stroke.Kind := gskNone;

  AGraphics.DrawRectangle(TRect.Create(ClientRect.Left, ClientRect.Top, ClientRect.Right + 1, ClientRect.Bottom + 1));
end;

procedure TAdvRichEditorCustomRuler.DrawBorder(AGraphics: TAdvGraphics);
begin
  AGraphics.Fill.Kind := gfkNone;
  if RulerBorderColor <> clNone then
  begin
    AGraphics.Stroke.Color := RulerBorderColor;
    if RulerBorderWidth > 0 then
    begin
      AGraphics.Stroke.Width := RulerBorderWidth;
      AGraphics.Stroke.Kind := gskSolid;
      AGraphics.DrawRectangle(GetRulerLeft, GetRulerTop, GetRulerLeft + GetWorkWidth, GetRulerBottom);
    end;
  end;

  if BorderColor <> clNone then
  begin
    AGraphics.Fill.Kind := gfkNone;
    AGraphics.Stroke.Kind := gskSolid;
    AGraphics.Stroke.Color := BorderColor;

    AGraphics.DrawRectangle(ClientRect);
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawHangingIndent(AGraphics: TAdvGraphics);
var
 p: TAdvGraphicsPath;
 x: Single;
begin
  if ShowHangingIndent then
  begin
    if IndentBorderColor <> clNone then
    begin
      if Enabled then
        AGraphics.Stroke.Color := IndentBorderColor
      else
        AGraphics.Stroke.Color := $00D4D4D4;

      AGraphics.Stroke.Width := Round(FDPIScale * 10) div 10;
      AGraphics.Stroke.Kind := gskSolid;
    end
    else
      AGraphics.Stroke.Kind := gskNone;

    x := GetRulerLeft + FZeroPos + HangingIndent;

    if Enabled then
      AGraphics.Fill.Color := IndentColor
    else
      AGraphics.Fill.Color := $00E0E0E0;
    AGraphics.Fill.Kind := gfkSolid;

    p := TAdvGraphicsPath.Create;
    GetIndentPath(x,ttHangingIndent, p);
    AGraphics.DrawPath(p,pdmPolygon);

    AGraphics.DrawRectangle(GetBoxRect);

    p.Free;
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawLabelStep(AGraphics: TAdvGraphics; AXPos: Integer; AText: string; AColor: TAdvGraphicsColor);
var
  h, w: Single;
  p: TPointF;
begin
  AGraphics.Font.Assign(Font);
  if Enabled then
    AGraphics.Font.Color := AColor
  else
    AGraphics.Font.Color := $00A0A0A0;

  h := AGraphics.CalculateTextHeight(AText);
  w := AGraphics.CalculateTextWidth(AText);

  p := TPointF.Create(AXPos - (w / 2) + 1, GetRulerTop + ((GetWorkHeight - h) / 2));

  AGraphics.DrawText(p,AText);
end;

procedure TAdvRichEditorCustomRuler.DrawLeftIndent(AGraphics: TAdvGraphics);
var
 p: TAdvGraphicsPath;
 x, y: Single;
begin
  if ShowLeftIndent then
  begin
    if ShowHangingIndent and Assigned(FLeftIndentTopGlyph) and not LeftIndentTopGlyph.Bitmap.Empty then
    begin
      x := GetIndentPos(ttLeftIndent);
      y := GetRulerTop + (GetWorkHeight / 2);

      AGraphics.DrawBitmap(x - FLeftIndentTopGlyph.Width / 2, y - FLeftIndentTopGlyph.Height, x + FLeftIndentTopGlyph.Width / 2, y, FLeftIndentTopGlyph);
    end
    else if not ShowHangingIndent and Assigned(FLeftIndentBottomGlyph) and not LeftIndentBottomGlyph.Bitmap.Empty then
    begin
      x := GetIndentPos(ttLeftIndent);
      y := GetRulerTop + (GetWorkHeight / 2);

      AGraphics.DrawBitmap(x - FLeftIndentBottomGlyph.Width / 2, y, x + FLeftIndentBottomGlyph.Width / 2, y + FLeftIndentBottomGlyph.Height, FLeftIndentBottomGlyph);
    end
    else
    begin
      if IndentBorderColor <> clNone then
      begin
        if Enabled then
          AGraphics.Stroke.Color := IndentBorderColor
        else
          AGraphics.Stroke.Color := $00D4D4D4;
        AGraphics.Stroke.Width := Round(FDPIScale * 10) div 10;
        AGraphics.Stroke.Kind := gskSolid;
      end
      else
        AGraphics.Stroke.Kind := gskNone;

      x := GetIndentPos(ttLeftIndent);

      if Enabled then
        AGraphics.Fill.Color := IndentColor
      else
        AGraphics.Fill.Color := $00E0E0E0;
      AGraphics.Fill.Kind := gfkSolid;

      p := TAdvGraphicsPath.Create;
      GetIndentPath(x,ttLeftIndent, p);
      AGraphics.DrawPath(p,pdmPolygon);

      p.Free;
    end;
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawLeftMargin(AGraphics: TAdvGraphics);
begin
  if FShowLeftMargin and (FLeftMargin > 0) then
  begin
    if Enabled then
      AGraphics.Fill.Color := MarginColor
    else
      AGraphics.Fill.Color := $00D4D4D4;

    AGraphics.Fill.Kind := gfkSolid;
    AGraphics.Stroke.Kind := gskNone;

    AGraphics.DrawRectangle(GetRulerLeft, GetRulerTop, GetLeftMarginPos + 1, GetRulerBottom);
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawRightIndent(AGraphics: TAdvGraphics);
var
 p: TAdvGraphicsPath;
 x,y: Single;
begin
  if ShowRightIndent then
  begin
    if Assigned(FRightIndentGlyph) and not FRightIndentGlyph.Bitmap.Empty then
    begin
      x := GetIndentPos(ttRightIndent);
      y := GetRulerTop + (GetWorkHeight / 2);

      AGraphics.DrawBitmap(x - FRightIndentGlyph.Width / 2, y, x + FRightIndentGlyph.Width / 2, y + FRightIndentGlyph.Height, FRightIndentGlyph);
    end
    else
    begin
      if IndentBorderColor <> clNone then
      begin
        if Enabled then
          AGraphics.Stroke.Color := IndentBorderColor
        else
          AGraphics.Stroke.Color := $00D4D4D4;
        AGraphics.Stroke.Width := Round(FDPIScale * 10) div 10;
        AGraphics.Stroke.Kind := gskSolid;
      end
      else
        AGraphics.Stroke.Kind := gskNone;

      x := GetIndentPos(ttRightIndent);

      if Enabled then
        AGraphics.Fill.Color := IndentColor
      else
        AGraphics.Fill.Color := $00E0E0E0;
      AGraphics.Fill.Kind := gfkSolid;

      p := TAdvGraphicsPath.Create;
      GetIndentPath(x,ttRightIndent, p);
      AGraphics.DrawPath(p,pdmPolygon);

      p.Free;
    end;
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawRightMargin(AGraphics: TAdvGraphics);
begin
  if FShowRightMargin and (FRightMargin > 0) then
  begin
    if Enabled then
      AGraphics.Fill.Color := MarginColor
    else
      AGraphics.Fill.Color := $00D4D4D4;

    AGraphics.Fill.Kind := gfkSolid;
    AGraphics.Stroke.Kind := gskNone;

    AGraphics.DrawRectangle(GetRightMarginPos, GetRulerTop, GetRulerLeft + GetWorkWidth, GetRulerBottom);
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawRulerColor(AGraphics: TAdvGraphics);
begin
  if RulerColor <> clNone then
  begin
    if Enabled then
      AGraphics.Fill.Color := RulerColor
    else
      AGraphics.Fill.Color := $00E0E0E0;

    AGraphics.Fill.Kind := gfkSolid;
    AGraphics.Stroke.Kind := gskNone;

    AGraphics.DrawRectangle(GetRulerLeft, GetRulerTop, GetRulerLeft + GetWorkWidth, GetRulerBottom);
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawStep(AGraphics: TAdvGraphics; AXPos: Integer; AColor: TAdvGraphicsColor);
var
  p,pt: TPointF;
begin
  if Enabled then
    AGraphics.Stroke.Color := AColor
  else
    AGraphics.Stroke.Color := $00A0A0A0;
  AGraphics.Stroke.Kind := gskSolid;
  AGraphics.Stroke.Width := FDPIScale;

  p := TPointF.Create(AXPos, GetRulerTop + (GetWorkHeight - Round(FDPIScale * TickMarks.StepHeight)) / 2);
  pt := TPointF.Create(AXPos, GetRulerTop +  (GetWorkHeight - Round(FDPIScale *TickMarks.StepHeight)) / 2 + Round(FDPIScale * TickMarks.StepHeight));

  AGraphics.DrawLine(p, pt);
end;

procedure TAdvRichEditorCustomRuler.DrawTabs(AGraphics: TAdvGraphics);
var
  p,pt: TPointF;
  I: integer;
  x: Single;
begin
  if Tabs.Count > 0 then
  begin
    AGraphics.Stroke.Kind := gskSolid;
    AGraphics.Stroke.Width := 1.5 * FDPIScale;

    x := GetRulerLeft + FZeroPos;

    for I := 0 to Tabs.Count - 1 do
    begin
      if Tabs[I].Indent >= 0 then
      begin
        if (FMoveTabIdx = I) then
        begin
          AGraphics.Stroke.Color := MovingTabColor;

          p := TPointF.Create(x + FMoveTabPos, GetRulerTop + GetWorkHeight - Round(TabSize * FDPIScale) - 1);
          pt := TPointF.Create(x + FMoveTabPos, GetRulerTop + GetWorkHeight);
          AGraphics.DrawLine(p, pt);

          p := TPointF.Create(x + FMoveTabPos + Round(TabSize * FDPIScale), GetRulerTop + GetWorkHeight);
          AGraphics.DrawLine(pt, p);
        end;

        if not ((FMoveTabIdx = I) and not FMouseInRuler) then
        begin
          if Enabled then
            AGraphics.Stroke.Color := TabColor
          else
            AGraphics.Stroke.Color := clBlack;

          p := TPointF.Create(x + Tabs[I].Indent, GetRulerTop + GetWorkHeight - Round(TabSize * FDPIScale) - 1);
          pt := TPointF.Create(x + Tabs[I].Indent, GetRulerTop + GetWorkHeight);
          AGraphics.DrawLine(p, pt);

          p := TPointF.Create(x + Tabs[I].Indent + Round(TabSize * FDPIScale), GetRulerTop + GetWorkHeight);
          AGraphics.DrawLine(pt, p);
        end;
      end;
    end;
  end;
end;

procedure TAdvRichEditorCustomRuler.DrawTick(AGraphics: TAdvGraphics; AXPos: Integer; AColor: TAdvGraphicsColor);
var
  s, h: Single;
begin
  AGraphics.Stroke.Kind := gskNone;
  AGraphics.Fill.Kind := gfkSolid;

  if Enabled then
  begin
    AGraphics.Fill.Color := AColor;
  end
  else
  begin
    AGraphics.Fill.Color := $00A0A0A0;
  end;

  s := TickMarks.Size * FDPIScale;
  h := GetRulerTop + GetWorkHeight / 2;

  if (AXPos + s) > (GetRulerLeft + GetWorkWidth) then
    AGraphics.DrawEllipse(AXPos - s, h - s, GetRulerLeft + GetWorkWidth, h + s)
  else
    AGraphics.DrawEllipse(AXPos - s, h - s, AXPos + s, h + s);
end;

procedure TAdvRichEditorCustomRuler.DrawTickMarks(AGraphics: TAdvGraphics);
var
  LMR: TRect;
  AR: TRect;
  RMR: TRect;
  I, c, LabelCount: Integer;
  start: Integer;
  fclr, clr: TAdvGraphicsColor;
  st: TAdvGraphicsSaveState;
begin
  AR := GetActiveRect;
  LMR := GetLeftMarginRect;
  RMR := GetRightMarginRect;
  c := 0;
  LabelCount := 0;

  st := AGraphics.SaveState;
  try
    AGraphics.ClipRect(TRectF.Create(GetRulerLeft, GetRulerTop, GetRulerLeft + GetWorkWidth, GetRulerBottom));

    if not TickMarks.ContinuousLabelSteps and (LeftMargin > 0) then
    begin
      c := 1;
      LabelCount := 1;

      if FontColorMargin <> clNone then
        fclr := FontColorMargin
      else
        fclr := Font.Color;

      if TickMarks.MarginColor <> clNone then
        clr := TickMarks.MarginColor
      else
        clr := TickMarks.Color;

      for I := LMR.Right - 1 downto LMR.Left do
      begin
        if (TickMarks.LabelStep > 0) and (c mod TickMarks.LabelStep = 0) then
        begin
          DrawLabelStep(AGraphics, I, IntToStr(LabelCount), fclr);
          Inc(LabelCount);
        end
        else if (TickMarks.Step > 0) and (c mod TickMarks.Step = 0) then
        begin
          DrawStep(AGraphics, I, clr);
        end
        else if (TickMarks.Spacing > 0) and (TickMarks.Size > 0) and (c mod TickMarks.Spacing = 0) then
        begin
          DrawTick(AGraphics, I, clr);
        end;
        Inc(c);
      end;

      LabelCount := 0;
      c := 0;
      start := AR.Left;
    end
    else
      start := LMR.Left;

    for I := start to RMR.Right do
    begin
      if ((I < AR.Left) or (I > AR.Right)) and (TickMarks.MarginColor <> clNone) then
        clr := TickMarks.MarginColor
      else
        clr := TickMarks.Color;

      if (TickMarks.LabelStep > 0) and (c mod TickMarks.LabelStep = 0) then
      begin
        if ((I < AR.Left) or (I > AR.Right)) and (FontColorMargin <> clNone) then
          clr := FontColorMargin
        else
          clr := Font.Color;
        DrawLabelStep(AGraphics, I, IntToStr(LabelCount), clr);
        Inc(LabelCount);
      end
      else if (TickMarks.Step > 0) and (c mod TickMarks.Step = 0) then
      begin
        DrawStep(AGraphics, I, clr);
      end
      else if (TickMarks.Spacing > 0) and (TickMarks.Size > 0) and (c mod TickMarks.Spacing = 0) then
      begin
        DrawTick(AGraphics, I, clr);
      end;
      Inc(c);
  end;
  finally
    AGraphics.RestoreState(st);
  end;
end;

function TAdvRichEditorCustomRuler.GetActiveRect: TRect;
begin
  Result := TRect.Create(GetLeftMarginPos, GetRulerTop, GetRightMarginPos, GetRulerBottom);
end;

function TAdvRichEditorCustomRuler.GetBoxRect: TRectF;
begin
  Result := TRectF.Create(GetRulerLeft + FZeroPos + HangingIndent - (FDPIScale * 4), GetRulerBottom, GetRulerLeft + FZeroPos + HangingIndent + (FDPIScale * 4) + 1, GetRulerBottom + (FDPIScale * 6));
end;

function TAdvRichEditorCustomRuler.GetClosestPoint(Xpos: Integer): Integer;
var
  x, d1, d2, diff: Integer;
begin
  Result := 0;
  if TabMove = tmPixel then
    Exit;

  x := Xpos - GetRulerLeft - FZeroPos;

  diff := 100000;

  if (TabMove = tmLabel) or (TabMove = tmAll) then
  begin
    d1 := x mod TickMarks.LabelStep;
    if x >= 0 then
      d2 := d1 - TickMarks.LabelStep
    else
      d2 := d1 + TickMarks.LabelStep;


    if (Abs(d1) < Abs(d2)) and (Abs(d1) < Abs(diff)) then
        diff := d1
    else if Abs(d2) < Abs(diff) then
      diff := d2;

    if TabMove = tmLabel then
    begin
      Result := diff;
      Exit;
    end;
  end;

  if (TabMove = tmStep) or (TabMove = tmAll) then
  begin
    d1 := x mod TickMarks.Step;
    if x >= 0 then
        d2 := d1 - TickMarks.Step
    else
      d2 := d1 + TickMarks.Step;

    if (Abs(d1) < Abs(d2)) and (Abs(d1) < Abs(diff)) then
        diff := d1
    else if Abs(d2) < Abs(diff) then
      diff := d2;

    if TabMove = tmStep then
    begin
      Result := diff;
      Exit;
    end;
  end;

  if (TabMove = tmTickMark) or (TabMove = tmAll) then
  begin
    d1 := x mod TickMarks.Spacing;
    if x >= 0 then
        d2 := d1 - TickMarks.Spacing
    else
      d2 := d1 + TickMarks.Spacing;

    if (Abs(d1) < Abs(d2)) and (Abs(d1) < Abs(diff)) then
        diff := d1
    else if Abs(d2) < Abs(diff) then
      diff := d2;
  end;

  Result := diff;
end;

function TAdvRichEditorCustomRuler.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvRichEditorCustomRuler.GetIndentPath(XPos: Single;
  ATabType: TAdvRichEditorRulerTabType; var APath: TAdvGraphicsPath);
begin
  APath.Clear;
  case ATabType of
    ttLeftIndent:
    begin
      if ShowHangingIndent then
      begin
        APath.AddLine(TPointF.Create(XPos - (FDPIScale * 4), GetRulerTop - 1),
          TPointF.Create(XPos - (FDPIScale * 4), GetRulerTop + (FDPIScale * 4) - 1));
        APath.AddLine(TPointF.Create(XPos - (FDPIScale * 4), GetRulerTop + (FDPIScale * 4) - 1),
          TPointF.Create(XPos, GetRulerTop + (FDPIScale * 8) - 1));
        APath.AddLine(TPointF.Create(XPos, GetRulerTop + (FDPIScale * 8) - 1),
          TPointF.Create(XPos + (FDPIScale * 4), GetRulerTop + (FDPIScale * 4) - 1));
        APath.AddLine(TPointF.Create(XPos + (FDPIScale * 4), GetRulerTop + (FDPIScale * 4) - 1),
          TPointF.Create(XPos + (FDPIScale * 4), GetRulerTop - 1));
        APath.ClosePath;
      end
      else
      begin
        GetIndentPath(XPos, ttRightIndent,APath);
      end;
    end;
    ttRightIndent, ttHangingIndent:
    begin
      APath.AddLine(TPointF.Create(XPos - (FDPIScale * 4), GetRulerBottom),
        TPointF.Create(XPos - (FDPIScale * 4), GetRulerBottom - (FDPIScale * 4)));
      APath.AddLine(TPointF.Create(XPos - (FDPIScale * 4), GetRulerBottom - (FDPIScale * 4)),
        TPointF.Create(XPos, GetRulerBottom - (FDPIScale * 8)));
      APath.AddLine(TPointF.Create(XPos, GetRulerBottom - (FDPIScale * 8)),
        TPointF.Create(XPos + (FDPIScale * 4), GetRulerBottom - (FDPIScale * 4)));
      APath.AddLine(TPointF.Create(XPos + ( FDPIScale * 4), GetRulerBottom - (FDPIScale * 4)),
        TPointF.Create(XPos + (FDPIScale * 4), GetRulerBottom));
      APath.ClosePath;
    end;
    else
    begin
      APath := nil;
    end;
  end;
end;

procedure TAdvRichEditorCustomRuler.GetIndentPath(ATabType: TAdvRichEditorRulerTabType;
  var APath: TAdvGraphicsPath);
begin
  GetIndentPath(GetIndentPos(ATabType), ATabType, APath);
end;

function TAdvRichEditorCustomRuler.GetIndentPos(ATabType: TAdvRichEditorRulerTabType): Integer;
var
 x: Integer;
begin
  x := GetRulerLeft + FZeroPos;
  case ATabType of
    ttLeftIndent:
      Result := x + FLeftIndent;
    ttRightIndent:
        Result := GetRightMarginPos - RightIndent;
    ttHangingIndent:
      Result := x + HangingIndent;
    else
      Result := -1;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetIndentVal(Xpos: Integer; ATabType: TAdvRichEditorRulerTabType; AIndex: Integer = -1);
var
 x: Integer;
 diff: Integer;
begin
  diff := GetClosestPoint(Xpos);

  x := GetRulerLeft + FZeroPos;
  case ATabType of
    ttLeftIndent:
      LeftIndent := Xpos - x - diff;
    ttRightIndent:
      RightIndent := GetRightMarginPos - (Xpos - diff);
    ttHangingIndent:
    begin
      if AIndex = -11 then
      begin
        if ((Xpos + (LeftIndent - HangingIndent) - x - diff) >= (0 - FZeroPos)) and ((Xpos - x - diff) >= (0 - FZeroPos)) and
        ((Xpos - diff) < (GetRightMarginPos - FRightIndent)) and ((Xpos + (LeftIndent - HangingIndent) - diff) < (GetRightMarginPos - FRightIndent))  then
        begin
          LeftIndent := Xpos + (LeftIndent - HangingIndent) - x - diff;
          HangingIndent := Xpos - x - diff;
        end;
      end
      else
        HangingIndent := Xpos - x - diff;
    end;
    ttLeftTab:
    begin
      if AIndex >= 0 then
        Tabs[AIndex].Indent := Xpos - FZeroPos - diff - GetRulerLeft;
    end;
    ttLeftMargin:
      LeftMargin := Xpos - GetRulerLeft - diff;
    ttRightMargin:
      RightMargin := GetWorkWidth - (Xpos - GetRulerLeft - diff);
  end;
end;

function TAdvRichEditorCustomRuler.GetWorkHeight: Integer;
begin
  Result := Height - Round(FDPIScale * FPaddingTop) - Round(FDPIScale * FPaddingBottom) - BorderWidth * 2;
end;

function TAdvRichEditorCustomRuler.GetWorkWidth: Integer;
begin
  Result := Width - Round(FDPIScale * FPaddingRight) - Round(FDPIScale * FPaddingLeft) - 2 * BorderWidth;
end;

procedure TAdvRichEditorCustomRuler.HandleTabsModified(Index: Integer; Indent: Integer);
begin
  SyncTabs;
  DoTabModified(Index, Indent);
end;

procedure TAdvRichEditorCustomRuler.Loaded;
begin
  inherited;
  SetPosition;
end;

procedure TAdvRichEditorCustomRuler.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  idx: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  pt := TPointF.Create(X,Y);

  if not FMouseDown then
  begin
    if PointInLeftIndent(pt) then
    begin
      FMoveTabType := ttLeftIndent;
    end
    else if PointInRightIndent(pt) then
    begin
      FMoveTabType := ttRightIndent;
    end
    else if PointInHangingIndent(pt) then
    begin
      FMoveTabType := ttHangingIndent;
      FMoveTabIdx := -10;
    end
    else if PointInHangingBox(pt) then
    begin
      FMoveTabType := ttHangingIndent;
      FMoveTabIdx := -11;
    end
    else if PointOnLeftMargin(pt) then
    begin
      FMoveTabType := ttLeftMargin;
    end
    else if PointOnRightMargin(pt) then
    begin
      FMoveTabType := ttRightMargin;
    end
    else if PointInRuler(pt) then
    begin
      idx := PointInTabs(pt);
      if idx >= 0 then
      begin
        FMoveTabType := ttLeftTab;
        FMoveTabIdx := idx;
        FMoveTabPos := Tabs[FMoveTabIdx].Indent;
      end
      else
      begin
        FMoveTabType := ttLeftTab;
        FMoveTabIdx := Tabs.AddTab(GetClosestNewTabVal(X));
        FMoveTabPos := Tabs[FMoveTabIdx].Indent;

        DoTabAdded(FMoveTabIdx, Tabs[FMoveTabIdx].Indent);
        HandleTabsModified(FMoveTabIdx, Tabs[FMoveTabIdx].Indent);
      end;
    end
    else
    begin
      FMoveTabType := ttNone;
      FMoveTabIdx := -1;
      FMoveTabPos := -1;
    end;

    Invalidate;
  end;

  FMouseDown := True;
end;

procedure TAdvRichEditorCustomRuler.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  pt: TPointF;
begin
  inherited MouseMove(Shift, X, Y);

  pt := TPointF.Create(X,Y);


  if FMouseDown and (FMoveTabType <> ttNone) then
  begin
    if PointInRuler(pt) then
      FMouseInRuler := True
    else
      FMouseInRuler := False;

    SetIndentVal(X,FMoveTabType, FMoveTabIdx);

    case FMoveTabType of
      ttLeftTab:
      begin
        DoTabChange(FMoveTabIdx, Tabs[FMoveTabIdx].Indent);
        HandleTabsModified(FMoveTabIdx, Tabs[FMoveTabIdx].Indent);
      end;
      ttLeftIndent: DoLeftIndentChange(FLeftIndent);
      ttRightIndent: DoRightIndentChange(FRightIndent);
      ttHangingIndent:
      begin
        if FMoveTabIdx = -11 then
          DoLeftIndentChange(FLeftIndent);
        DoHangingIndentChange(FHangingIndent);
      end;
      ttLeftMargin: DoLeftMarginChange(FLeftMargin);
      ttRightMargin: DoRightMarginChange(FRightMargin);
    end;
  end
  else
  begin
    if PointInLeftIndent(pt) or PointInRightIndent(pt) or PointInHangingIndent(pt) or PointInHangingBox(pt) then
      Cursor := crHandPoint
    else if PointOnLeftMargin(pt) or PointOnRightMargin(pt) then
      Cursor := crSizeWE
    else
      Cursor := crDefault;
  end;
end;

procedure TAdvRichEditorCustomRuler.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TabIndent: integer;
begin
  inherited;

  if FMouseDown and (FMoveTabType <> ttNone) then
  begin
    case FMoveTabType of
      ttLeftTab:
      begin
        if not PointInRuler(TPointF.Create(X,Y)) then
        begin
          TabIndent := Tabs[FMoveTabIdx].Indent;
          DoTabRemove(FMoveTabIdx, TabIndent);
          Tabs.Delete(FMoveTabIdx);

          HandleTabsModified(FMoveTabIdx, TabIndent);

          Invalidate;
        end
        else
        begin
          DoTabChanged(FMoveTabIdx, Tabs[FMoveTabIdx].Indent);
          HandleTabsModified(FMoveTabIdx, Tabs[FMoveTabIdx].Indent);
        end;
      end;
      ttLeftIndent:
      begin
        DoLeftIndentChanged(FLeftIndent);
      end;
      ttRightIndent:
      begin
        DoRightIndentChanged(FRightIndent);
      end;
      ttHangingIndent:
      begin
        if FMoveTabIdx = -11 then
        begin
          DoLeftIndentChanged(FLeftIndent);
        end;
        DoHangingIndentChanged(FHangingIndent);
      end;
      ttLeftMargin:
      begin
        DoLeftMarginChanged(FLeftMargin);
      end;
      ttRightMargin:
      begin
        DoRightMarginChanged(FRightMargin);
      end;
    end;

    FMoveTabType := ttNone;
    FMoveTabIdx := -1;
    FMoveTabPos := -1;

    Invalidate;
  end;

  FMouseDown := False;
end;

function TAdvRichEditorCustomRuler.GetRulerBottom: Integer;
begin
  Result := GetRulerTop + GetWorkHeight;
end;

function TAdvRichEditorCustomRuler.GetRulerLeft: Integer;
begin
  Result := BorderWidth + Round(FDPIScale * FPaddingLeft);
end;

function TAdvRichEditorCustomRuler.GetLeftMarginPos: Integer;
begin
  Result := GetRulerLeft + FLeftMargin;
end;

function TAdvRichEditorCustomRuler.GetLeftMarginRect: TRect;
begin
  Result := TRect.Create(GetRulerLeft, GetRulerTop, GetLeftMarginPos, GetRulerBottom);
end;

function TAdvRichEditorCustomRuler.GetClosestNewTabVal(XPos: Integer): Integer;
var
 diff: Integer;
begin
  diff := GetClosestPoint(Xpos);
  Result := Xpos - FZeroPos - diff - GetRulerLeft;
end;

function TAdvRichEditorCustomRuler.GetRightMarginPos: Integer;
begin
  Result := GetRulerLeft + GetWorkWidth - FRightMargin;
end;

function TAdvRichEditorCustomRuler.GetRightMarginRect: TRect;
begin
  Result := TRect.Create(GetRightMarginPos, GetRulerTop, GetRulerLeft + GetWorkWidth, GetRulerBottom);
end;

function TAdvRichEditorCustomRuler.GetRulerTop: Integer;
begin
  Result := Round(FDPIScale * FPaddingTop) + BorderWidth;
end;

procedure TAdvRichEditorCustomRuler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = RichEditor) then
    begin
      RichEditor := nil;
    end;
  end
  else if (AComponent = RichEditor) then
  begin
    SetPosition;
  end;
end;

procedure TAdvRichEditorCustomRuler.DoTickMarksChanged(Sender: TObject);
begin
  Invalidate;

  if Assigned(OnTickMarksChanged) then
    OnTickMarksChanged(Self);
end;

procedure TAdvRichEditorCustomRuler.DoUpdate(Sender: TObject);
var
  i: integer;
begin
  if Assigned(FRichEditor) then
  begin
    BeginUpdate;

    LeftIndent := FRichEditor.GetSelectionIndent;

    RightIndent := FRichEditor.GetSelectionRightIndent;

    LeftMargin := FRichEditor.PageMargin.Left;


    Tabs.Clear;
    for i := 0 to FRichEditor.TabPositions.Count - 1 do
    begin
      Tabs.AddTab(FRichEditor.TabPositions[i].Indent - Round(FDPIScale * PaddingLeft));
    end;

    EndUpdate;
  end;
end;

function TAdvRichEditorCustomRuler.PointInHangingBox(APoint: TPointF): Boolean;
begin
  Result := TAdvGraphics.PointInRect(TPoint.Create(Round(APoint.X), Round(APoint.Y)), GetBoxRect);
end;

function TAdvRichEditorCustomRuler.PointInHangingIndent(
  APoint: TPointF): Boolean;
var
  pth: TAdvGraphicsPath;
begin
  pth := TAdvGraphicsPath.Create;

  GetIndentPath(ttHangingIndent,pth);

  Result := TAdvGraphics.PointInPath(APoint, pth);

  pth.Free;
end;

function TAdvRichEditorCustomRuler.PointInLeftIndent(APoint: TPointF): Boolean;
var
  pth: TAdvGraphicsPath;
begin
  pth := TAdvGraphicsPath.Create;

  GetIndentPath(ttLeftIndent,pth);

  Result := TAdvGraphics.PointInPath(APoint, pth);

  pth.Free;
end;

function TAdvRichEditorCustomRuler.PointInRightIndent(APoint: TPointF): Boolean;
var
  pth: TAdvGraphicsPath;
begin
  pth := TAdvGraphicsPath.Create;

  GetIndentPath(ttRightIndent,pth);

  Result := TAdvGraphics.PointInPath(APoint, pth);

  pth.Free;
end;

function TAdvRichEditorCustomRuler.PointInRuler(APoint: TPointF): Boolean;
begin
  Result := TAdvGraphics.PointInRect(TPoint.Create(Round(APoint.X), Round(APoint.Y)), GetActiveRect);
end;

function TAdvRichEditorCustomRuler.PointInTabs(APoint: TPointF): Integer;
var
  I: Integer;
begin
  Result := -1;
  
  for I := 0 to Tabs.Count - 1 do
  begin
    if (APoint.X > GetRulerLeft + FZeroPos + Tabs[I].FIndent - 2 * FDPIScale) and (APoint.X < GetRulerLeft + FZeroPos + Tabs[I].FIndent + 2 * FDPIScale) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TAdvRichEditorCustomRuler.PointOnLeftMargin(APoint: TPointF): Boolean;
begin
  Result := ((APoint.X >= GetLeftMarginPos - (2 * FDPIScale)) and (APoint.X <= GetLeftMarginPos + (2 * FDPIScale))) and (GetRulerTop < APoint.Y) and (APoint.Y < GetRulerBottom);
end;

function TAdvRichEditorCustomRuler.PointOnRightMargin(APoint: TPointF): Boolean;
begin
  Result := ((APoint.X >= GetRightMarginPos - (2 * FDPIScale)) and (APoint.X <= GetRightMarginPos + (2 * FDPIScale))) and (GetRulerTop < APoint.Y) and (APoint.Y < GetRulerBottom);
end;

procedure TAdvRichEditorCustomRuler.SetRichEditor(
  const Value: TAdvRichEditor);
var
  i: integer;
begin
  FRichEditor := Value;
  if Assigned(Value) then
  begin
    SetAutoPosition(FAutoPosition);

    FRichEditor.OnRulerUpdate := DoUpdate;

    BeginUpdate;

    FRichEditor.PageMargin.Left := LeftMargin;
    FRichEditor.PageMargin.Right := RightMargin;
//    Tabs.Clear;

//    for i := 0 to FRichEditor.TabPositions.Count - 1 do
//    begin
//      Tabs.AddTab(FRichEditor.TabPositions[i].Indent - PaddingLeft);
//    end;

    FRichEditor.TabPositions.Clear;

    for I := 0 to Tabs.Count - 1 do
    begin
      FRichEditor.TabPositions.Add(Tabs[i].Indent + Round(FDPIScale * PaddingLeft));
    end;

    EndUpdate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetAutoPosition(const Value: Boolean);
begin
  if FAutoPosition <> Value then
  begin
    FAutoPosition := Value;
    SetPosition;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetAutoWidth(const Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    SetSize;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetBorderColor(
  const Value: TAdvGraphicsColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetColor(const Value: TAdvGraphicsColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSSTyle := AStyle;

  if not (AStyle = tsCustom) then
  begin
    case AStyle of
      tsTerminal:
      begin
        RulerColor := clWhite;
        RulerBorderColor := clGray;
        IndentColor := clHighlight;
        IndentBorderColor := clBlack;
        TabColor := clBlack;
        MarginColor := clMedGray;
        MovingTabColor := gcLightGray;
        TickMarks.Color := clBlack;
        TickMarks.MarginColor := clNone;
        Font.Color := clBlack;
        FontColorMargin := clNone;
      end;
      tsWindows7:
      begin
        RulerColor := clWhite;
        RulerBorderColor := $00ECB58C;
        IndentColor := RGB(251, 253, 255);
        IndentBorderColor := RGB(30, 57, 91);
        TabColor := RGB(30, 57, 91);
        MarginColor := RGB(223, 233, 245);
        MovingTabColor := RGB(186, 201, 219);
        TickMarks.Color := RGB(30, 57, 91);
        TickMarks.MarginColor := clNone;
        Font.Color := RGB(30, 57, 91);
        FontColorMargin := clNone;
      end;
      tsWindows8, tsWindows10:
      begin
        RulerColor := clWhite;
        RulerBorderColor := clSilver;
        IndentColor := $00F7F6F5;
        IndentBorderColor := clBlack;
        TabColor := clBlack;
        MarginColor := $00ECECEB;
        MovingTabColor := gcLightgray;
        TickMarks.Color := clBlack;
        TickMarks.MarginColor := clNone;
        Font.Color := clBlack;
        FontColorMargin := clNone;
      end;

      tsOffice2007Luna:
      begin
        RulerColor := clWhite;
        RulerBorderColor := $00DFD2C5;
        IndentColor := $00FEF6F0;
        IndentBorderColor := $008B4215;
        TabColor := $008B4215;
        MarginColor := $00FFDBBF ;
        MovingTabColor := $00A0BFCC;
        TickMarks.Color := $008B4215;
        TickMarks.MarginColor := clNone;
        Font.Color := $008B4215;
        FontColorMargin := clNone;
      end;
      tsOffice2007Obsidian:
      begin
        RulerColor := clWhite;
        RulerBorderColor := $00CAC7C6;
        IndentColor := TAdvGraphics.HTMLToColor('$EDEEEF');
        IndentBorderColor := clBlack;
        TabColor := clBlack;
        MarginColor := TAdvGraphics.HTMLToColor('$535353');
        MovingTabColor := $00A0BFCC;
        TickMarks.Color := clBlack;
        TickMarks.MarginColor := clWhite;
        Font.Color := clBlack;
        FontColorMargin := clWhite;
      end;
      tsOffice2007Silver:
      begin
        RulerColor := clWhite;
        RulerBorderColor := $00C6B0B1;
        IndentColor := $00F6F6F6;
        IndentBorderColor := $005C534C;
        TabColor := $005C534C;
        MarginColor := $00DDD4D0;
        MovingTabColor := $00AEC6C9;
        TickMarks.Color := $005C534C;
        TickMarks.MarginColor := clNone;
        Font.Color := $005C534C;
        FontColorMargin := clNone;
      end;

      tsOffice2010Blue:
      begin
        RulerColor := $00F7EBDF;
        RulerBorderColor := $00B3947B;
        IndentColor := $00E0CBBC;
        IndentBorderColor := $00AC7853;
        TabColor := $00946849;
        MarginColor := $00F0DAC8;
        MovingTabColor := clGray;
        TickMarks.Color := $005C3208;
        TickMarks.MarginColor := clNone;
        Font.Color := $005C3208;
        FontColorMargin := clNone;
      end;
      tsOffice2010Silver:
      begin
        RulerColor := $00F6F6F6;
        RulerBorderColor := $008F8780;
        IndentColor := $00C9C9C9;
        IndentBorderColor := $007D7A7C;
        TabColor := $006B6B6B;
        MarginColor := $00D6D1CD;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $005F5B5A;
        TickMarks.MarginColor := clNone;
        Font.Color := $005F5B5A;
        FontColorMargin := clNone;
      end;
      tsOffice2010Black:
      begin
        RulerColor := $00A7A7A7;
        RulerBorderColor := $004E4E4E;
        IndentColor := $00CDCDCD;
        IndentBorderColor := $007B7B7B;
        TabColor := $006B6B6B;
        MarginColor := $006F6F6F;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $002F2F2F;
        TickMarks.MarginColor := clNone;
        Font.Color := $002F2F2F;
        FontColorMargin := clNone;
      end;

      tsOffice2013White:
      begin
        RulerColor := clWhite;
        RulerBorderColor := clGray;
        IndentColor := clWhite;
        IndentBorderColor := $00666666;
        TabColor := $00666666;
        MarginColor := gcLightgray;
        MovingTabColor := clSilver;
        TickMarks.Color := $00666666;
        TickMarks.MarginColor := clNone;
        Font.Color := $00666666;
        FontColorMargin := clNone;
      end;
      tsOffice2013LightGray:
      begin
        RulerColor := $00FAFAFA;
        RulerBorderColor := clGray;
        IndentColor := $00F1F1F1;
        IndentBorderColor := $00666666;
        TabColor := $00666666;
        MarginColor := gcLightgray;
        MovingTabColor := clSilver;
        TickMarks.Color := $00666666;
        TickMarks.MarginColor := clNone;
        Font.Color := $00666666;
        FontColorMargin := clNone;
      end;
      tsOffice2013Gray:
      begin
        RulerColor := $00FAFAFA;
        RulerBorderColor := clGray;
        IndentColor := $00F3F3F3;
        IndentBorderColor := $00666666;
        TabColor := $00666666;
        MarginColor := gcLightgray;
        MovingTabColor := $00DEDEDE;
        TickMarks.Color := $00666666;
        TickMarks.MarginColor := clNone;
        Font.Color := $00666666;
        FontColorMargin := clNone;
      end;

      tsOffice2016White:
      begin
        RulerColor := clWhite;
        RulerBorderColor := $00E2E2E2;
        IndentColor := clWhite;
        IndentBorderColor := $00919191;
        TabColor := $00777777;
        MarginColor := $00E6E6E6;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $00BBBBBB;
        TickMarks.MarginColor := clNone;
        Font.Color := $00525252;
        FontColorMargin := clNone;
      end;
      tsOffice2016Gray:
      begin
        RulerColor := $00767676;
        RulerBorderColor := $00444444;
        IndentColor := $00B2B2B2;
        IndentBorderColor := $00444444;
        TabColor := $00E1E1E1;
        MarginColor := $00585858;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $00B2B2B2;
        TickMarks.MarginColor := clNone;
        Font.Color := clWhite;
        FontColorMargin := clNone;
      end;
      tsOffice2016Black:
      begin
        RulerColor := $003B3B3B;
        RulerBorderColor := $00505050;
        IndentColor := $00A0A0A0;
        IndentBorderColor := $000B0B0B;
        TabColor := $00ACACAC;
        MarginColor := $000B0B0B;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $00979797;
        TickMarks.MarginColor := clNone;
        Font.Color := clWhite;
        FontColorMargin := clNone;
      end;

      tsOffice2019White:
      begin
        RulerColor := clWhite;
        RulerBorderColor := $00E2E2E2;
        IndentColor := clWhite;
        IndentBorderColor := $00919191;
        TabColor := $00777777;
        MarginColor := $00E6E6E6;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $00BBBBBB;
        TickMarks.MarginColor := clNone;
        Font.Color := $00525252;
        FontColorMargin := clNone;
      end;
      tsOffice2019Gray:
      begin
        RulerColor := $00767676;
        RulerBorderColor := $00444444;
        IndentColor := $00B2B2B2;
        IndentBorderColor := $00444444;
        TabColor := $00E1E1E1;
        MarginColor := $00585858;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $00B2B2B2;
        TickMarks.MarginColor := clNone;
        Font.Color := clWhite;
        FontColorMargin := clNone;
      end;
      tsOffice2019Black:
      begin
        RulerColor := $003B3B3B;
        RulerBorderColor := $00505050;
        IndentColor := $00A0A0A0;
        IndentBorderColor := $000B0B0B;
        TabColor := $00ACACAC;
        MarginColor := $000B0B0B;
        MovingTabColor := gcLightgray;
        TickMarks.Color := $00979797;
        TickMarks.MarginColor := clNone;
        Font.Color := clWhite;
        FontColorMargin := clNone;
      end;
      else
      begin
        RulerColor := clWhite;
        RulerBorderColor := clGray;
        IndentColor := clWhite;
        IndentBorderColor := clBlack;
        TabColor := clBlack;
        MarginColor := gcLightgray;
        MovingTabColor := gcLightgray;
        TickMarks.Color := clGray;
        TickMarks.MarginColor := clNone;
        Font.Color := clWindowText;
        FontColorMargin := clNone;
      end;
    end;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetEnabledEx(const Value: Boolean);
begin
  inherited Enabled := Value;
  Invalidate;
end;

procedure TAdvRichEditorCustomRuler.SetFontColorMargin(
  const Value: TAdvGraphicsColor);
begin
  if FFontColorMargin <> Value then
  begin
    FFontColorMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetHangingIndent(const Value: Integer);
begin
  if (FHangingIndent <> Value) and (Value + FZeroPos + GetRulerLeft < (GetRightMarginPos - FRightIndent)) then
  begin
    if Value + FZeroPos > 0 then
      FHangingIndent := Value
    else
      FHangingIndent := 0 - FZeroPos;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetIndentBorderColor(
  const Value: TAdvGraphicsColor);
begin
  if FIndentBorderColor <> Value then
  begin
    FIndentBorderColor := Value;
    Invalidate;
  end;

end;

procedure TAdvRichEditorCustomRuler.SetIndentColor(
  const Value: TAdvGraphicsColor);
begin
  if FIndentColor <> Value then
  begin
    FIndentColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetLeftIndent(const Value: Integer);
begin
  if (FLeftIndent <> Value) and (Value + FZeroPos + GetRulerLeft < (GetRightMarginPos - FRightIndent)) then
  begin
    if Value + FZeroPos > 0 then
      FLeftIndent := Value
    else
      FLeftIndent := 0 - FZeroPos;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetLeftIndentBottomGlyph(const Value: TAdvBitmap);
begin
  FLeftIndentBottomGlyph.Assign(Value);
  Invalidate;
end;

procedure TAdvRichEditorCustomRuler.SetLeftIndentTopGlyph(const Value: TAdvBitmap);
begin
  FLeftIndentTopGlyph.Assign(Value);
  Invalidate;
end;

procedure TAdvRichEditorCustomRuler.SetLeftMargin(const Value: Integer);
begin
  if (FLeftMargin <> Value) and (Value + GetRulerLeft < GetRightMarginPos ) then
  begin
    if Value > 0 then
      FLeftMargin := Value
    else
      FLeftMargin := 0;

    if not TickMarks.FContinuousLabelSteps then
      FZeroPos := FLeftMargin
    else
      FZeroPos := 0;

    if Assigned(RichEditor) then
      FRichEditor.PageMargin.Left := Value;


    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetPaddingBottom(const Value: Integer);
begin
  if FPaddingBottom <> Value then
  begin
    FPaddingBottom := Value;

    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetPaddingLeft(const Value: Integer);
begin
  if FPaddingLeft <> Value then
  begin
    FPaddingLeft := Value;
    FZeroPos := FLeftMargin;

    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetPaddingRight(const Value: Integer);
begin
  if FPaddingRight <> Value then
  begin
    FPaddingRight := Value;

    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetPaddingTop(const Value: Integer);
begin
  if FPaddingTop <> Value then
  begin
    FPaddingTop := Value;

    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetParent(Value: TWinControl);
begin
  inherited;
  SetPosition;
end;

procedure TAdvRichEditorCustomRuler.SetMarginColor(
  const Value: TAdvGraphicsColor);
begin
  if FMarginColor <> Value then
  begin
    FMarginColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetMovingTabColor(
  const Value: TAdvGraphicsColor);
begin
  if FMovingTabColor <> Value then
  begin
    FMovingTabColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetPosition;
var
 diff: integer;
begin
  if Assigned(FRichEditor) and FAutoPosition then
  begin
    Left := FRichEditor.Left - Round(FDPIScale * FPaddingLeft);

    if FRichEditor.Align in [alTop, alBottom] then
      Align := FRichEditor.Align
    else if FRichEditor.Align = alClient then
    begin
      if Position = rpTop then
        Align := alTop
      else
        Align := alBottom;
    end;

    if Position = rpTop then
    begin
      if FRichEditor.Top - Height < 0 then
      begin
        diff := Height - FRichEditor.Top;
        FRichEditor.Top := diff;
        Top := 0;
      end
      else
        Top := FRichEditor.Top - Height;
    end
    else if Position = rpBottom then
      Top := FRichEditor.Top + FRichEditor.Height;

    SetSize;

    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetRightIndentGlyph(const Value: TAdvBitmap);
begin
  FRightIndentGlyph.Assign(Value);
  Invalidate;
end;

procedure TAdvRichEditorCustomRuler.SetRightIndent(const Value: Integer);
begin
  if (FRightIndent <> Value) and (FLeftIndent + FZeroPos + GetRulerLeft < (GetRightMarginPos - Value)) and (FHangingIndent  + FZeroPos + GetRulerLeft < (GetRightMarginPos - Value)) then
  begin
    FRightIndent := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetRightMargin(const Value: Integer);
begin
  if (FRightMargin <> Value) and (GetLeftMarginPos < GetWorkWidth - Value) then
  begin
    if Value >= 0 then
      FRightMargin := Value
    else
      FRightMargin := 0;

    if Assigned(RichEditor) then
      RichEditor.PageMargin.Right := Value;

    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetRulerBorderColor(
  const Value: TAdvGraphicsColor);
begin
  if FRulerBorderColor <> Value then
  begin
    FRulerBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetRulerBorderWidth(const Value: Integer);
begin
  if FRulerBorderWidth <> Value then
  begin
    if Value > 0 then
      FRulerBorderWidth := Value
    else
      FRulerBorderWidth := 0;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetRulerColor(
  const Value: TAdvGraphicsColor);
begin
  if FRulerColor <> Value then
  begin
    FRulerColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetShowHangingIndent(const Value: Boolean);
begin
  if FShowHangingIndent <> Value then
  begin
    FShowHangingIndent := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetShowLeftIndent(const Value: Boolean);
begin
  if FShowLeftIndent <> Value then
  begin
    FShowLeftIndent := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetShowLeftMargin(const Value: Boolean);
begin
  if FShowLeftMargin <> Value then
  begin
    FShowLeftMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetShowRightIndent(const Value: Boolean);
begin
  if FShowRightIndent <> Value then
  begin
    FShowRightIndent := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetShowRightMargin(const Value: Boolean);
begin
  if FShowRightMargin <> Value then
  begin
    FShowRightMargin := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetSize;
begin
  if FAutoWidth and Assigned(FRichEditor) then
  begin
    Width := FRichEditor.Width + Round(FDPIScale * FPaddingLeft) + Round(FDPIScale * FPaddingRight);
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetTabColor(const Value: TAdvGraphicsColor);
begin
  if FTabColor <> Value then
  begin
    FTabColor := Value;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetTabMove(const Value: TAdvRichEditorRulerTabMove);
begin
  if FTabMove <> Value then
    FTabMove := Value;
end;

procedure TAdvRichEditorCustomRuler.SetTabSize(const Value: Integer);
begin
  if FTabSize <> Value then
  begin
    if Value > 0 then
      FTabSize := Value
    else
      FTabSize := 0;
    Invalidate;
  end;
end;

procedure TAdvRichEditorCustomRuler.SetUIStyle(const Value: TTMSStyle);
begin
  SetComponentStyle(Value);
end;

procedure TAdvRichEditorCustomRuler.SyncTabs;
var
  i: integer;
begin
  if Assigned(FRichEditor) then
  begin
    FRicheditor.TabPositions.Clear;

    for i := 0 to Tabs.Count - 1 do
    begin
      FRicheditor.TabPositions.Add(Tabs[i].Indent + Round(FDPIScale * PaddingLeft));
    end;
  end;
end;

procedure TAdvRichEditorCustomRuler.UpdateRichEditor;
begin
  if Assigned(FRichEditor) then
  begin
    FRicheditor.PageMargin.Left := LeftMargin;
    FRichEditor.PageMargin.Right := RightMargin;
    SyncTabs;
  end;
end;

{ TTabCollection }

function TAdvRichEditorRulerTabCollection.Add: TAdvRichEditorRulerTabItem;
begin
  Result := TAdvRichEditorRulerTabItem(inherited Add);
end;

function TAdvRichEditorRulerTabCollection.AddTab(ATabItem: TAdvRichEditorRulerTabItem): integer;
begin
  Result := AddTab(ATabItem.Indent);
end;

function TAdvRichEditorRulerTabCollection.AddTab(AIndent: Integer): integer;
var
  ti: TAdvRichEditorRulerTabItem;
  I: Integer;
begin
  if Self.Count > 0 then
  begin
    for I := 0 to Self.Count - 1 do
    begin
      if (AIndent = Self.Items[I].FIndent) then
      begin
        Result := I;
        Exit;
      end
    end;
    I := Self.Count;
  end
  else
    I := 0;

  ti := Add;
  ti.FIndent := AIndent;
  Result := I;

  FOwner.Invalidate;
end;

constructor TAdvRichEditorRulerTabCollection.Create(AOwner: TAdvRichEditorCustomRuler);
begin
  inherited Create(AOwner, TAdvRichEditorRulerTabItem);
  FOwner := AOwner;
end;

procedure TAdvRichEditorRulerTabCollection.EndUpdate;
begin
  inherited;
  FOwner.Invalidate;
end;

function TAdvRichEditorRulerTabCollection.GetItem(Index: Integer): TAdvRichEditorRulerTabItem;
begin
  Result := TAdvRichEditorRulerTabItem(inherited Items[Index]);
end;

function TAdvRichEditorRulerTabCollection.Insert(Index: Integer): TAdvRichEditorRulerTabItem;
begin
  Result := TAdvRichEditorRulerTabItem(inherited Insert(Index));
end;

procedure TAdvRichEditorRulerTabCollection.SetItem(Index: Integer; const Value: TAdvRichEditorRulerTabItem);
begin
  inherited Items[Index] := Value;
end;

{ TRulerTickMarks }

procedure TAdvRichEditorRulerTickMarks.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorRulerTickMarks) then
  begin
    Color := (Source as TAdvRichEditorRulerTickMarks).Color;
    MarginColor := (Source as TAdvRichEditorRulerTickMarks).MarginColor;
    LabelStep := (Source as TAdvRichEditorRulerTickMarks).Size;
    Step := (Source as TAdvRichEditorRulerTickMarks).Step;
    Spacing := (Source as TAdvRichEditorRulerTickMarks).Spacing;
    Size := (Source as TAdvRichEditorRulerTickMarks).Size;
  end
  else
    inherited;
end;

procedure TAdvRichEditorRulerTickMarks.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

constructor TAdvRichEditorRulerTickMarks.Create;
begin
  inherited;
  FSize := 1;
  FSpacing := 10;
  FStep := 25;
  FStepHeight := 6;
  FLabelStep := 50;
  FColor := gcGray;
  FMarginColor := clNone;
  FContinuousLabelSteps := False;
end;

destructor TAdvRichEditorRulerTickMarks.Destroy;
begin

  inherited;
end;

procedure TAdvRichEditorRulerTickMarks.SetColor(const Value: TAdvGraphicsColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvRichEditorRulerTickMarks.SetContinuousLabelSteps(const Value: Boolean);
begin
  if FContinuousLabelSteps <> Value then
  begin
    FContinuousLabelSteps := Value;
    Changed;
  end;
end;

procedure TAdvRichEditorRulerTickMarks.SetLabelStep(const Value: Integer);
begin
  if FLabelStep <> Value then
  begin
    FLabelStep := Value;
    Changed;
  end;        
end;

procedure TAdvRichEditorRulerTickMarks.SetMarginColor(const Value: TAdvGraphicsColor);
begin
  if FMarginColor <> Value then
  begin
    FMarginColor := Value;
    Changed;
  end;
end;

procedure TAdvRichEditorRulerTickMarks.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TAdvRichEditorRulerTickMarks.SetSpacing(const Value: Integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TAdvRichEditorRulerTickMarks.SetStep(const Value: Integer);
begin
  if (FStep <> Value) then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TAdvRichEditorRulerTickMarks.SetStepHeight(const Value: Integer);
begin
  if FStepHeight <> Value then
  begin
    FStepHeight := Value;
    Changed;
  end;
end;

{ TTabItem }

procedure TAdvRichEditorRulerTabItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorRulerTabItem) then
  begin
    Indent := (Source as TAdvRichEditorRulerTabItem).Indent;
  end
  else
    inherited;
end;

procedure TAdvRichEditorRulerTabItem.SetIndent(const Value: integer);
begin
  begin
    FIndent := Value;
    (Collection as TAdvRichEditorRulerTabCollection).FOwner.Invalidate;
  end;
end;

end.
