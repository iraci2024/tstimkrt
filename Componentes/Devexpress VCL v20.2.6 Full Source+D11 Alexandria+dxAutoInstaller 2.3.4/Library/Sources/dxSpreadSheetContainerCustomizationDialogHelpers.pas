{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSpreadSheet }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxSpreadSheetContainerCustomizationDialogHelpers;

{$I cxVer.Inc}

interface

uses
  Windows, Types, Controls, Classes, Graphics, Generics.Defaults,
  Generics.Collections,
  dxGDIPlusClasses, cxGraphics, dxCoreGraphics, cxControls, dxCoreClasses;

type

  { TdxSpreadSheetGradientStopsEdit }

  TdxSpreadSheetGradientStopsEdit = class(TcxControl)
  strict private
  const
    StopSize = 12;
  strict private
    FAllowModifySchema: Boolean;
    FBrush: TdxGPBrush;
    FCaptureOffset: TPoint;
    FDragging: Boolean;
    FSelectedStopIndex: Integer;
    FStops: TdxRectList;

    FOnSelectionChanged: TNotifyEvent;

    procedure BrushChangeHandler(Sender: TObject);
    function GetSelectedStopColor: TdxAlphaColor;
    function GetTrackArea: TRect;
    function GetTrackAreaContent: TRect;
    procedure SetSelectedStopColor(const Value: TdxAlphaColor);
    procedure SetSelectedStopIndex(AValue: Integer);
  protected
    procedure BiDiModeChanged; override;
    procedure BoundsChanged; override;

    // Keyboard
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    // Mouse
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

    procedure Calculate;
    procedure DoPaint; override;
    procedure DrawFrame(ACanvas: TcxCanvas; R: TRect;
      ASelected: Boolean = False);
    procedure DrawGradient(ACanvas: TcxCanvas; R: TRect);
    procedure DrawStop(ACanvas: TcxCanvas; const R: TRect;
      AColor: TdxAlphaColor; ASelected: Boolean);
    procedure DrawStops(ACanvas: TcxCanvas);
    procedure DrawTrack(ACanvas: TcxCanvas);
    procedure MoveStop(const DeltaX: Integer);
    procedure MoveStopTo(const P: TPoint);
    function StopIndexAtPoint(const P: TPoint): Integer;
    function StopIndexByColorAndOffset(AOffset: Single;
      AColor: TdxAlphaColor): Integer;
    //
    property Stops: TdxRectList read FStops;
    property TrackArea: TRect read GetTrackArea;
    property TrackAreaContent: TRect read GetTrackAreaContent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add;
    procedure AddAtPoint(const P: TPoint);
    procedure DeleteSelected;
    //
    property AllowModifySchema: Boolean read FAllowModifySchema
      write FAllowModifySchema;
    property Brush: TdxGPBrush read FBrush;
    property SelectedStopColor: TdxAlphaColor read GetSelectedStopColor
      write SetSelectedStopColor;
    property SelectedStopIndex: Integer read FSelectedStopIndex
      write SetSelectedStopIndex;
  published
    property BiDiMode;
    property ParentBiDiMode;
    property OnDblClick;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
      write FOnSelectionChanged;
  end;

implementation

uses
  cxGeometry, SysUtils, cxLookAndFeelPainters, cxLookAndFeels, dxTypeHelpers,
  dxCore, Math;

{ TdxSpreadSheetGradientStopsEdit }

constructor TdxSpreadSheetGradientStopsEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowModifySchema := True;
  FBrush := TdxGPBrush.Create;
  FBrush.Style := gpbsGradient;
  FBrush.OnChange := BrushChangeHandler;
  FStops := TdxRectList.Create;
  FSelectedStopIndex := -1;
  ControlStyle := ControlStyle + [csCaptureMouse];
  TabStop := True;
  Keys := [kArrows];
end;

destructor TdxSpreadSheetGradientStopsEdit.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FStops);
  inherited Destroy;
end;

procedure TdxSpreadSheetGradientStopsEdit.Add;
begin
  if AllowModifySchema then
  begin
    Brush.GradientPoints.Add(0.5, 0);
    SelectedStopIndex := StopIndexByColorAndOffset(0.5, 0);
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.AddAtPoint(const P: TPoint);
begin
  Add;
  MoveStopTo(P);
end;

procedure TdxSpreadSheetGradientStopsEdit.DeleteSelected;
begin
  if AllowModifySchema and (SelectedStopIndex >= 0) then
  begin
    Brush.GradientPoints.Delete(SelectedStopIndex);
    SelectedStopIndex := SelectedStopIndex; // validate
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.BiDiModeChanged;
begin
  inherited;
  Calculate;
end;

procedure TdxSpreadSheetGradientStopsEdit.BoundsChanged;
begin
  inherited BoundsChanged;
  Calculate;
end;

procedure TdxSpreadSheetGradientStopsEdit.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if IsRightToLeft then
    Key := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key);

  case Key of
    VK_INSERT:
      Add;
    VK_DELETE:
      DeleteSelected;
    VK_LEFT:
      SelectedStopIndex := Max(SelectedStopIndex - 1, 0);
    VK_RIGHT:
      SelectedStopIndex := SelectedStopIndex + 1;
    VK_UP, VK_DOWN:
      MoveStop(IfThen(Key = VK_DOWN, -1, 1) * Max(1,
        Round((TrackArea.Width - StopSize) / 100)));
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.DblClick;
begin
  if SelectedStopIndex < 0 then
    AddAtPoint(CalcCursorPos);
  inherited DblClick;
end;

procedure TdxSpreadSheetGradientStopsEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  SetFocus;
  if (Button = mbLeft) and not(ssDouble in Shift) then
  begin
    SelectedStopIndex := StopIndexAtPoint(Point(X, Y));
    FDragging := (SelectedStopIndex >= 0) and AllowModifySchema;
    if FDragging then
    begin
      FCaptureOffset := cxPointOffset(Point(X, Y),
        cxRectCenter(Stops[SelectedStopIndex]), False);
      MouseCapture := True;
    end;
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.MouseMove(Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  if FDragging then
    MoveStopTo(cxPointOffset(Point(X, Y), FCaptureOffset, False));
end;

procedure TdxSpreadSheetGradientStopsEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  if FDragging then
  begin
    MouseCapture := False;
    FDragging := False;
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.Calculate;
var
  AStopBounds: TRect;
  AStopsArea: TRect;
  I: Integer;
begin
  Stops.Clear;
  AStopsArea := cxRectCenterHorizontally(TrackArea, TrackArea.Width - StopSize);
  AStopsArea.Bottom := Bounds.Bottom;
  AStopsArea.Top := Bounds.Top;
  for I := 0 to Brush.GradientPoints.Count - 1 do
  begin
    AStopBounds := cxRectBounds(AStopsArea.Left +
      Round(AStopsArea.Width * Brush.GradientPoints.Offsets[I]) -
      StopSize shr 1, AStopsArea.Top, StopSize, AStopsArea.Height);
    if IsRightToLeft then
      AStopBounds := TdxRightToLeftLayoutConverter.ConvertRect(AStopBounds,
        AStopsArea);
    Stops.Add(AStopBounds);
  end;
  Invalidate;
end;

procedure TdxSpreadSheetGradientStopsEdit.DoPaint;
begin
  inherited DoPaint;
  Canvas.FillRect(Bounds, LookAndFeelPainter.DefaultContentColor);
  DrawTrack(Canvas);
  DrawStops(Canvas);
end;

procedure TdxSpreadSheetGradientStopsEdit.DrawFrame(ACanvas: TcxCanvas;
  R: TRect; ASelected: Boolean = False);
const
  Color1Map: array [Boolean] of TColor = ($222222, $0022EF);
  Color2Map: array [Boolean] of TColor = (clWhite, $94E2FF);
begin
  ACanvas.FrameRect(R, Color1Map[ASelected]);
  R := cxRectInflate(R, -1);
  ACanvas.FrameRect(R, Color2Map[ASelected]);
  R := cxRectInflate(R, -1);
  cxDrawTransparencyCheckerboard(ACanvas, R, 4);
end;

procedure TdxSpreadSheetGradientStopsEdit.DrawGradient(ACanvas: TcxCanvas;
  R: TRect);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    dxGpRightToLeftDependentDraw(dxGPPaintCanvas, R,
      ACanvas.UseRightToLeftAlignment, procedure
      begin
        dxGPPaintCanvas.Rectangle(R, nil, Brush);
      end);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.DrawStop(ACanvas: TcxCanvas;
const R: TRect; AColor: TdxAlphaColor; ASelected: Boolean);
begin
  DrawFrame(ACanvas, R, ASelected);
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(cxRectInflate(R, -2));
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    try
      dxGPPaintCanvas.Rectangle(R, 0, AColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.DrawStops(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Stops.Count - 1 do
    DrawStop(ACanvas, Stops[I], Brush.GradientPoints.Colors[I],
      I = SelectedStopIndex);
end;

procedure TdxSpreadSheetGradientStopsEdit.DrawTrack(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    DrawFrame(ACanvas, TrackArea);
    ACanvas.IntersectClipRect(TrackAreaContent);
    DrawGradient(ACanvas, TrackArea);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.MoveStop(const DeltaX: Integer);
begin
  if SelectedStopIndex >= 0 then
    MoveStopTo(cxRectCenter(cxRectOffset(Stops[SelectedStopIndex], DeltaX, 0)));
end;

procedure TdxSpreadSheetGradientStopsEdit.MoveStopTo(const P: TPoint);
var
  AColor: TdxAlphaColor;
  AOffset: Single;
  ARect: TRect;
begin
  if AllowModifySchema and (SelectedStopIndex >= 0) then
  begin
    ARect := cxRectCenterHorizontally(TrackArea, TrackArea.Width - StopSize);
    AColor := Brush.GradientPoints.Colors[SelectedStopIndex];
    AOffset := Min(Max((P.X - ARect.Left) / ARect.Width, 0), 1);
    if IsRightToLeft then
      AOffset := 1 - AOffset;
    Brush.GradientPoints.Offsets[SelectedStopIndex] := AOffset;
    FSelectedStopIndex := StopIndexByColorAndOffset(AOffset, AColor);
  end;
end;

function TdxSpreadSheetGradientStopsEdit.StopIndexAtPoint
  (const P: TPoint): Integer;
var
  I: Integer;
begin
  for I := Stops.Count - 1 downto 0 do
  begin
    if cxRectPtIn(Stops[I], P) then
      Exit(I);
  end;
  Result := -1;
end;

function TdxSpreadSheetGradientStopsEdit.StopIndexByColorAndOffset
  (AOffset: Single; AColor: TdxAlphaColor): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Brush.GradientPoints.Count - 1 downto 0 do
  begin
    if SameValue(Brush.GradientPoints.Offsets[I], AOffset) and
      (Brush.GradientPoints.Colors[I] = AColor) then
      Exit(I);
  end;
end;

procedure TdxSpreadSheetGradientStopsEdit.BrushChangeHandler(Sender: TObject);
begin
  Calculate;
end;

function TdxSpreadSheetGradientStopsEdit.GetSelectedStopColor: TdxAlphaColor;
begin
  if SelectedStopIndex >= 0 then
    Result := Brush.GradientPoints.Colors[SelectedStopIndex]
  else
    Result := 0;
end;

function TdxSpreadSheetGradientStopsEdit.GetTrackArea: TRect;
begin
  Result := cxRectInflate(Bounds, 0, -2);
end;

function TdxSpreadSheetGradientStopsEdit.GetTrackAreaContent: TRect;
begin
  Result := cxRectInflate(TrackArea, -2);
end;

procedure TdxSpreadSheetGradientStopsEdit.SetSelectedStopColor
  (const Value: TdxAlphaColor);
begin
  if SelectedStopIndex >= 0 then
    Brush.GradientPoints.Colors[SelectedStopIndex] := Value;
end;

procedure TdxSpreadSheetGradientStopsEdit.SetSelectedStopIndex(AValue: Integer);
begin
  AValue := Min(Max(AValue, -1), Brush.GradientPoints.Count - 1);
  if SelectedStopIndex <> AValue then
  begin
    FSelectedStopIndex := AValue;
    dxCallNotify(OnSelectionChanged, Self);
    Invalidate;
  end;
end;

end.
