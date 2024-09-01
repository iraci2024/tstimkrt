unit acntTypes;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  ExtCtrls,
  {$IFDEF TNTUNICODE} TntMenus, {$ENDIF}
{$IFNDEF NOJPG}
  {$IFDEF TINYJPG} acTinyJpg, {$ELSE} Jpeg, {$ENDIF}
{$ENDIF}
  Controls, Menus, Windows, Classes, Forms, Graphics, Messages,
  sConst;

{$IFNDEF NOTFORHELP}

type
{$IFNDEF NOTFORHELP}
  {$IFDEF DELPHI5}
  acInt64 = LongInt;
  {$ELSE}
  acInt64 = Int64;
  {$ENDIF}
{$ENDIF}

{$IFNDEF D2007}
  HPAINTBUFFER = THandle; // handle to a buffered paint context
  PBP_PAINTPARAMS = ^BP_PAINTPARAMS;
  BP_PAINTPARAMS = record
    cbSize: DWORD;
    dwFlags: DWORD; // BPPF_ flags
    prcExclude: PRect;
    pBlendFunction: PBLENDFUNCTION;
  end;
  _BP_PAINTPARAMS = BP_PAINTPARAMS;
  TBPPaintParams = BP_PAINTPARAMS;
  PBPPaintParams = ^TBPPaintParams;
{$ENDIF}

  TacUpdateFlags = (ufUpdateThumbs, ufUpdateScale, ufFormCreating, ufFormInitializing);
  TacOrientation = (aoHorizontal, aoVertical);

  TacCtrlArray = array of TControl;
  TacArrowsStyle = (arsSolid1, arsLines1, arsSolid2, arsLines2, arsSolid3, {arsLines3, }arsDefault);
  TacOpaqueMode = (omAuto, omForceOpaque, omAlphaChannel);
  TacSides = set of TacSide;
  PacSides = ^TacSides;

  TacObject = class(TObject);

  TacItemDrawData = record
    Font: TFont;
    Text,
    HtmlText: acString;
    ItemIndex,
    ImageIndex: integer;
    State: TOwnerDrawState;
  end;

  TacAnimData = record
    Iteration,
    Iterations: integer;
    Owner: TControl;
  end;

  TacAnimProc = function(Data: TObject; Iteration: integer): boolean;
  TacAnimProcEx = function(var Data: TacAnimData): boolean;

  TacItemDrawDataObj = class(TacObject)
  public
    ImageIndex: integer;
  end;

  TacTimer = class(TTimer)
  public
    FOwner: TObject;
  end;

  TacBounds = record
    BLeft, BTop, BWidth, BHeight: integer;
  end;

  TacIntValues = record
    Value0, Value1, Value2, Value3: integer;
  end;
  PacIntValues = ^TacIntValues;

  TSrcRect = packed record
    SLeft, STop, SRight, SBottom: {$IFDEF DELPHI_XE8}FixedInt{$ELSE}Longint{$ENDIF};
  end;

  TDstRect = packed record
    DLeft, DTop, DRight, DBottom: {$IFDEF DELPHI_XE8}FixedInt{$ELSE}Longint{$ENDIF};
  end;

  TacToolTipData = record
    Alpha: byte;
    SrcPoint: TPoint;
//    Position: TacSide; // (asLeft, asTop, asRight, asBottom)
    Distance: integer;
  end;

  TacAccentPolicy = packed record
    AccentState: Integer;
    AccentFlags: Integer;
    GradientColor: Integer;
    AnimationId: Integer;
  end;

  TacWinCompAttrData = packed record
    attribute: THandle;
    pData: Pointer;
    dataSize: ULONG;
  end;

  PacColoring = ^TacColoring;
  TacColoring = record
    ColorToneBG: TColor;
    ColorGlyph: TColor;
    ColorText: TColor;
  end;

  TacGetColoring = procedure(Sender: TObject; State: integer; var Coloring: TacColoring) of object;
  TacColorPreview = procedure(Sender: TObject; OriginColor, PreviousColor: TColor; var NewColor: TColor) of object;

  PacLayerPos = ^TacLayerPos;
  TacLayerPos = record
    Bounds: TacBounds;
    LayerBlend: byte;
  end;

  TStringLists = array of TStringList;
  TRects = array of Windows.TRect;
  TacJpegClass = {$IFDEF TINYJPG}TacTinyJPGImage{$ELSE}TJPEGImage{$ENDIF};
  TacMenuItem = {$IFDEF TNTUNICODE}TTntMenuItem{$ELSE}TMenuItem{$ENDIF};

  TacAccessControl = class(TControl)
  public
    property AutoSize;
    property ParentColor;
    property Color;
    property ParentFont;
    property PopupMenu;
    property Font;
    property Text;
  end;

  TacAccessCanvas = class({$IFDEF D2010}TCustomCanvas{$ELSE}TPersistent{$ENDIF})
  public
    FHandle: HDC;
  end;

  TacGlowForm = class(TCustomForm)
  public
    AlphaBmp: TBitmap;
    TransparentMouse: boolean;
    procedure AfterConstruction; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  end;

  TacButtonPaintData = record
    AColor1,
    AColor2,
    ABorderColor,
    AFontColor: TColor;
    AText: acString;
    ABevelWidth,
    ABorderWidth: integer;
    ATransparent: boolean;
  end;

  TacPaintButtonOptions = class;

  TacDataNormal = class(TPersistent)
  private
    FColor1,
    FColor2,
    FFontColor,
    FBorderColor: TColor;
    FOwner: TacPaintButtonOptions;
  public
    constructor Create(AOwner: TacPaintButtonOptions); virtual;
    procedure SetColor(const Index: Integer; const Value: TColor);
  published
    property Color1:      TColor index 0 read FColor1       write SetColor default clBtnFace;
    property Color2:      TColor index 1 read FColor2       write SetColor default cl3DLight;
    property FontColor:   TColor index 2 read FFontColor    write SetColor default clBtnText;
    property BorderColor: TColor index 3 read FBorderColor  write SetColor default clActiveBorder;
  end;

  TacDataActive = class(TacDataNormal)
  public
    constructor Create(AOwner: TacPaintButtonOptions); override;
  published
    property Color1 default clBtnHighlight;
    property Color2 default clBtnFace;
  end;

  TacDataPressed = class(TacDataNormal)
  public
    constructor Create(AOwner: TacPaintButtonOptions); override;
  published
    property Color1 default cl3DLight;
    property Color2 default cl3DLight;
  end;

{$ENDIF}

  TacPaintBorderData = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FChanged: boolean;
    FWidth: integer;
    FOnChange: TNotifyEvent;
    FColorPressed: TColor;
    FColorActive: TColor;
    FColorNormal: TColor;
    FRadiusTopLeft: integer;
    FRadiusTopRight: integer;
    FRadiusBottomLeft: integer;
    FRadiusBottomRight: integer;
    procedure SetWidth(const Value: integer);
    procedure SetColor(const Index: Integer; const Value: TColor);
    procedure SetRadius(const Index, Value: integer);
    procedure CheckChanged;
  public
    FSkinData,
    FOwnerObject: TObject;
    constructor Create(AOwnerObject, ASkinData: TObject; AChangeEvent: TNotifyEvent);
    procedure Invalidate;
    function IsChanged: boolean;
    function GetCorner(ACorner: integer): integer;
  published
{$ENDIF}
    property Width: integer read FWidth write SetWidth default -1;

    property ColorNormal:  TColor index 0 read FColorNormal  write SetColor default clNone;
    property ColorActive:  TColor index 1 read FColorActive  write SetColor default clNone;
    property ColorPressed: TColor index 2 read FColorPressed write SetColor default clNone;

    property RadiusTopLeft:     integer index 0 read FRadiusTopLeft     write SetRadius default -1;
    property RadiusTopRight:    integer index 1 read FRadiusTopRight    write SetRadius default -1;
    property RadiusBottomLeft:  integer index 2 read FRadiusBottomLeft  write SetRadius default -1;
    property RadiusBottomRight: integer index 3 read FRadiusBottomRight write SetRadius default -1;
  end;

{$IFNDEF NOTFORHELP}
  TacUpdateBorderProc = procedure(PaintBorderData: TacPaintBorderData; var OutCorners: TacIntValues; var OutSides: PacSides);

  TacPaintButtonOptions = class(TPersistent)
  private
    FBevelWidth,
    FBorderWidth: integer;
    FDataActive:  TacDataActive;
    FDataNormal:  TacDataNormal;
    FDataPressed: TacDataPressed;
    procedure SetInteger(const Index: Integer; const Value: integer);
  protected
    FOnInvalidate: TNotifyEvent;
  public
    constructor Create;
    function GetData(State: integer): TacDataNormal;
    destructor Destroy; override;
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
  published
    property BevelWidth:  integer index 0 read FBevelWidth  write SetInteger default 2;
    property BorderWidth: integer index 1 read FBorderWidth write SetInteger default 1;
    property DataActive:  TacDataActive  read FDataActive  write FDataActive;
    property DataNormal:  TacDataNormal  read FDataNormal  write FDataNormal;
    property DataPressed: TacDataPressed read FDataPressed write FDataPressed;
  end;

  TacPadding = class(TPersistent)
  private
    FTop,
    FLeft,
    FRight,
    FBottom: TacIntProperty;
    FControl: TControl;
    FOnChange: TNotifyEvent;
    procedure SetPadding(const Index: Integer; Value: TacIntProperty);
  public
    constructor Create(Control: TControl; ChangeEvent: TNotifyEvent);
  published
    property Left:   TacIntProperty index 0 read FLeft   write SetPadding default 0;
    property Top:    TacIntProperty index 1 read FTop    write SetPadding default 0;
    property Right:  TacIntProperty index 2 read FRight  write SetPadding default 0;
    property Bottom: TacIntProperty index 3 read FBottom write SetPadding default 0;
  end;


  TacAnimEventData = record
    DoOutput,
    DoLighting: boolean;
  end;
  PacAnimEventData = ^TacAnimEventData;

  TacAnimUpdateEvent = procedure(Sender: TObject; Data: PacAnimEventData) of object;

function acBounds(Left, Top, Width, Height: integer): TacBounds;

{$ENDIF}

implementation

uses
  sCommonData, sPageControl,
{$IFDEF LOGGED}
  sDebugMsgs,
{$ENDIF}
  SysUtils, math;

function acBounds(Left, Top, Width, Height: integer): TacBounds;
begin
  with Result do begin
    BLeft := Left;
    BTop := Top;
    BWidth := Width;
    BHeight := Height;
  end;
end;


procedure TacGlowForm.AfterConstruction;
begin
  inherited;
  AlphaBmp := nil;
  Tag := ExceptTag;
  BorderStyle := bsNone;
  TransparentMouse := True;
//  Name := 'GlowForm' + IntToStr(GetTickCount);
end;


procedure TacGlowForm.CreateWnd;
begin
  inherited;
//  Assert(HandleAllocated);
  if HandleAllocated then begin
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE);
    SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not NCS_DROPSHADOW);
  end;
end;


destructor TacGlowForm.Destroy;
begin
  if AlphaBmp <> nil then
    FreeAndNil(AlphaBmp);

  inherited;
end;


procedure TacGlowForm.Loaded;
begin
  Scaled := acCurrentScaleMode = smVCL;
  inherited;
end;


procedure TacGlowForm.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  if HandleAllocated then
    inherited;
end;


procedure TacGlowForm.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  if Tag = 2 then
    AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    // Removing of blinking
    CM_VISIBLECHANGED, WM_SHOWWINDOW:
      if (Message.WParam = 0) and not (csDestroying in ComponentState) then
        if HandleAllocated and IsWindowVisible(Handle) then // If hide
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_HIDE or SWP_NOMOVE or SWP_NOSIZE);

    WM_NCHITTEST: if TransparentMouse then
      Message.Result := HTTRANSPARENT;
  end;
  inherited;
end;


constructor TacPaintButtonOptions.Create;
begin
  FBorderWidth := 1;
  FBevelWidth := 2;
  FDataActive  := TacDataActive.Create(Self);
  FDataNormal  := TacDataNormal.Create(Self);
  FDataPressed := TacDataPressed.Create(Self);
end;


destructor TacPaintButtonOptions.Destroy;
begin
  FDataActive.Free;
  FDataNormal.Free;
  FDataPressed.Free;
  inherited;
end;


function TacPaintButtonOptions.GetData(State: integer): TacDataNormal;
begin
  case State of
    1: Result   := DataActive;
    2: Result   := DataPressed
    else Result := DataNormal;
  end;
end;


procedure TacPaintButtonOptions.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if Assigned(FOnInvalidate) then
        FOnInvalidate(Self);
    end;
  end;

begin
  case Index of
    0: ChangeProp(FBevelWidth, Value);
    1: ChangeProp(FBorderWidth, Value);
  end;
end;


constructor TacDataNormal.Create(AOwner: TacPaintButtonOptions);
begin
  FOwner        := AOwner;
  FColor1       := clBtnFace;
  FColor2       := cl3DLight;
  FFontColor    := clBtnText;
  FBorderColor := clActiveBorder;
end;


procedure TacDataNormal.SetColor(const Index: Integer; const Value: TColor);

  procedure ChangeProp(var Prop: TColor; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if Assigned(FOwner.FOnInvalidate) then
        FOwner.FOnInvalidate(FOwner);
    end;
  end;

begin
  case Index of
    0: ChangeProp(FColor1, Value);
    1: ChangeProp(FColor2, Value);
    2: ChangeProp(FFontColor, Value);
    3: ChangeProp(FBorderColor, Value);
  end;
end;


constructor TacDataActive.Create(AOwner: TacPaintButtonOptions);
begin
  inherited;
  Color1 := clBtnHighlight;
  Color2 := clBtnFace;
end;


constructor TacDataPressed.Create(AOwner: TacPaintButtonOptions);
begin
  inherited;
  Color1 := cl3DLight;
  Color2 := cl3DLight;
end;


constructor TacPadding.Create(Control: TControl; ChangeEvent: TNotifyEvent);
begin
  inherited Create;
  FControl := Control;
  FOnChange := ChangeEvent;
  FLeft   := 0;
  FRight  := 0;
  FTop    := 0;
  FBottom := 0;
end;


procedure TacPadding.SetPadding(const Index: Integer; Value: TacIntProperty);

  procedure ChangeProp(var Prop: TacIntProperty; Value: TacIntProperty);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in FControl.ComponentState) and Assigned(FOnChange) then
        FOnChange(FControl)
    end;
  end;

begin
  case Index of
    0: ChangeProp(FLeft,   Value);
    1: ChangeProp(FTop,    Value);
    2: ChangeProp(FRight,  Value);
    3: ChangeProp(FBottom, Value);
  end;
end;


procedure TacPaintBorderData.CheckChanged;
begin
  FChanged := (FWidth >= 0) or
    (FColorNormal <> clNone) or
    (FColorActive <> clNone) or
    (FColorPressed <> clNone) or
    (FRadiusTopLeft <> -1) or
    (FRadiusTopRight <> -1) or
    (FRadiusBottomLeft <> -1) or
    (FRadiusBottomRight <> -1);
end;


constructor TacPaintBorderData.Create;
begin
  FOnChange := AChangeEvent;
  FChanged := False;
  Width := -1;

  FRadiusTopLeft := -1;
  FRadiusTopRight := -1;
  FRadiusBottomLeft := -1;
  FRadiusBottomRight := -1;

  FColorNormal := clNone;
  FColorActive := clNone;
  FColorPressed := clNone;
  FOwnerObject := AOwnerObject;
  FSkinData := ASkinData;
end;


function TacPaintBorderData.GetCorner(ACorner: integer): integer;
var
  iDiff: integer;
begin
  case ACorner of
    0: Result := FRadiusTopLeft;
    1: Result := FRadiusTopRight;
    2: Result := FRadiusBottomLeft;
    else Result := FRadiusBottomRight
  end;
  if Result > 0 then
    if FSkindata is TsCommonData then
      with TsCommonData(FSkindata) do begin
        if CurrentPPI <> 96 then
          Result := ScaleInt(Result, TsCommonData(FSkindata));

        if (FOwnerControl <> nil) then
          with FOwnerControl do
            if Width > Height then begin
              case ACorner of
                0: iDiff := FRadiusBottomLeft;  // TopLeft
                1: iDiff := FRadiusBottomRight; // TopRight
                2: iDiff := FRadiusTopLeft      // BottomLeft
                else iDiff := FRadiusTopRight;  // BottomRight
              end;
              if iDiff < 0 then
                iDiff := 0;

              if Result > Height - iDiff then
                Result := max(Height div 2, Height - iDiff);
            end
            else begin
              case ACorner of
                0: iDiff := FRadiusTopRight;     // TopLeft
                1: iDiff := FRadiusTopLeft;      // TopRight
                2: iDiff := FRadiusBottomRight   // BottomLeft
                else iDiff := FRadiusBottomLeft; // BottomRight
              end;
              if Result > Width - iDiff then
                Result := max(Width div 2, Width - iDiff);
            end;
      end
    else
      if (FSkinData is TsTabSkinData) then
        with TsTabSkinData(FSkinData) do
          if (FPage <> nil) and (TsPageControl(FPage.PageControl) <> nil) then
            Result := ScaleInt(Result, TsPageControl(FPage.PageControl).SkinData);
end;


procedure TacPaintBorderData.Invalidate;
begin
  CheckChanged;
  if Assigned(FOnChange) then
    if (FOwnerObject is TComponent) and ([csLoading, csReading] * TComponent(FOwnerObject).ComponentState = []) then
      FOnChange(Self);
end;


function TacPaintBorderData.IsChanged: boolean;
begin
  CheckChanged;
  Result := FChanged;
end;


procedure TacPaintBorderData.SetColor(const Index: Integer; const Value: TColor);

  procedure ChangeProp(var Prop: TColor; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FColorNormal, Value);
    1: ChangeProp(FColorActive, Value);
    2: ChangeProp(FColorPressed, Value);
  end;
end;


procedure TacPaintBorderData.SetRadius(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FRadiusTopLeft, Value);
    1: ChangeProp(FRadiusTopRight, Value);
    2: ChangeProp(FRadiusBottomLeft, Value);
    3: ChangeProp(FRadiusBottomRight, Value);
  end;
end;


procedure TacPaintBorderData.SetWidth(const Value: integer);
begin
  if FWidth <> Value then begin
    FWidth := Value;
    Invalidate;
  end;
end;

end.




