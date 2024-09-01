{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressGanttControl }
{ }
{ Copyright (c) 2020 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSGANTTCONTROL AND ALL }
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

unit dxGanttControlSplitter;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Windows, Messages, MultiMon, Types, Forms, Controls, Graphics,
  Dialogs,
  Classes, Themes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxControls, cxGraphics, cxGeometry, cxLookAndFeels,
  cxLookAndFeelPainters,
  dxGanttControlCustomClasses;

type
  TdxGanttControlSplitterController = class;
  TdxGanttControlSplitterOptions = class;

  { TdxGanttControlSplitterViewInfo }

  TdxGanttControlSplitterViewInfo = class
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    FOptions: TdxGanttControlSplitterOptions;
  protected
    procedure DoDraw; override;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo;
      AOptions: TdxGanttControlSplitterOptions); reintroduce;
    property Options: TdxGanttControlSplitterOptions read FOptions;
  end;

  { TdxGanttSplitterDragAndDropObject }

  TdxGanttSplitterDragAndDropObject = class(TdxGanttControlResizingObject)
  strict private
    function GetController: TdxGanttControlSplitterController; inline;
  protected
    procedure ApplyChanges(const P: TPoint); override;
    function CanDrop(const P: TPoint): Boolean; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragImageHeight: Integer; override;
    function GetDragImageWidth: Integer; override;
  public
    property Controller: TdxGanttControlSplitterController read GetController;
  end;

  { TdxGanttControlSplitterDragHelper }

  TdxGanttControlSplitterDragHelper = class(TdxGanttControlDragHelper)
  protected
    function CreateDragAndDropObject: TdxGanttControlDragAndDropObject;
      override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
  end;

  { TdxGanttControlSplitterController }

  TdxGanttControlSplitterController = class abstract
    (TdxGanttControlCustomController)
  strict private
    FOptions: TdxGanttControlSplitterOptions;
    function InternalGetVierInfo: TdxGanttControlSplitterViewInfo; inline;
  protected
    procedure ApplySize(const P: TPoint); virtual; abstract;
    function CalculateDragPoint(const P: TPoint): TPoint; virtual;
    function CreateDragHelper: TdxGanttControlDragHelper; override;
    function GetDesignHitTest(X: Integer; Y: Integer; Shift: TShiftState)
      : Boolean; override;
    property Options: TdxGanttControlSplitterOptions read FOptions;
    property ViewInfo: TdxGanttControlSplitterViewInfo read InternalGetVierInfo;
  public
    constructor Create(AControl: TdxGanttControlBase;
      AOptions: TdxGanttControlSplitterOptions); reintroduce;
  end;

  { TdxGanttControlSplitterOptions }

  TdxGanttControlSplitterOptions = class(TdxGanttControlCustomOptions)
  strict private
  const
    DefaultWidth = 5;
    DefaultMinSize = 50;
    MinWidth = 3;
  strict private
    FMinSize: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    procedure SetMinSize(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property MinSize: Integer read FMinSize write SetMinSize
      default DefaultMinSize;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default DefaultWidth;
  end;

implementation

uses
  Math,
  cxCustomCanvas,
  dxTypeHelpers,
  dxGanttControlCursors;

{ TdxGanttControlSplitterViewInfo }

constructor TdxGanttControlSplitterViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo;
  AOptions: TdxGanttControlSplitterOptions);
begin
  inherited Create(AOwner);
  FOptions := AOptions;
end;

procedure TdxGanttControlSplitterViewInfo.DoDraw;
var
  ABorders: TcxBorders;
begin
  if Bounds.Width < Bounds.Height then
    ABorders := [bLeft, bRight]
  else
    ABorders := [bTop, bBottom];
  LookAndFeelPainter.DrawScaledHeader(Canvas, Bounds, cxbsNormal, [], ABorders,
    ScaleFactor, False, False);
end;

function TdxGanttControlSplitterViewInfo.GetCurrentCursor(const P: TPoint;
  const ADefaultCursor: TCursor): TCursor;
begin
  Result := TdxGanttControlCursors.HSplit;
end;

{ TdxGanttSplitterDragAndDropObject }

procedure TdxGanttSplitterDragAndDropObject.ApplyChanges(const P: TPoint);
begin
  Controller.ApplySize(Controller.CalculateDragPoint(P));
  SetDesignerModified(Controller.Control);
end;

function TdxGanttSplitterDragAndDropObject.CanDrop(const P: TPoint): Boolean;
begin
  Result := True;
end;

procedure TdxGanttSplitterDragAndDropObject.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
var
  APos: TPoint;
begin
  Accepted := True;
  APos := Controller.CalculateDragPoint(P);
  ShowDragImage(APos);
end;

function TdxGanttSplitterDragAndDropObject.GetController
  : TdxGanttControlSplitterController;
begin
  Result := TdxGanttControlSplitterController(inherited Controller);
end;

function TdxGanttSplitterDragAndDropObject.GetDragImageHeight: Integer;
begin
  Result := Controller.ViewInfo.Bounds.Height;
end;

function TdxGanttSplitterDragAndDropObject.GetDragImageWidth: Integer;
begin
  Result := Controller.ViewInfo.Bounds.Width;
end;

{ TdxGanttControlSplitterDragHelper }

function TdxGanttControlSplitterDragHelper.CreateDragAndDropObject
  : TdxGanttControlDragAndDropObject;
begin
  Result := TdxGanttSplitterDragAndDropObject.Create(Controller);
end;

procedure TdxGanttControlSplitterDragHelper.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  Accepted := True;
end;

procedure TdxGanttControlSplitterDragHelper.DragOver(Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

function TdxGanttControlSplitterDragHelper.StartDragAndDrop
  (const P: TPoint): Boolean;
begin
  Result := True;
end;

{ TdxGanttControlSplitterController }

function TdxGanttControlSplitterController.CalculateDragPoint
  (const P: TPoint): TPoint;
var
  R: TRect;
begin
  Result.Y := ViewInfo.Bounds.Top;
  R := ViewInfo.Owner.Bounds;
  if ViewInfo.UseRightToLeftAlignment then
    Result.X := Min(R.Right - Options.MinSize, P.X)
  else
    Result.X := Max(R.Left + Options.MinSize, P.X);
end;

constructor TdxGanttControlSplitterController.Create
  (AControl: TdxGanttControlBase; AOptions: TdxGanttControlSplitterOptions);
begin
  inherited Create(AControl);
  FOptions := AOptions;
end;

function TdxGanttControlSplitterController.CreateDragHelper
  : TdxGanttControlDragHelper;
begin
  Result := TdxGanttControlSplitterDragHelper.Create(Self);
end;

function TdxGanttControlSplitterController.GetDesignHitTest(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function TdxGanttControlSplitterController.InternalGetVierInfo
  : TdxGanttControlSplitterViewInfo;
begin
  Result := TdxGanttControlSplitterViewInfo(inherited ViewInfo);
end;

{ TdxGanttControlSplitterOptions }

procedure TdxGanttControlSplitterOptions.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlSplitterOptions;
begin
  if Source is TdxGanttControlSplitterOptions then
  begin
    ASource := TdxGanttControlSplitterOptions(Source);
    Width := ASource.Width;
    Visible := ASource.Visible;
    MinSize := ASource.MinSize;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlSplitterOptions.DoReset;
begin
  FWidth := DefaultWidth;
  FVisible := True;
  FMinSize := DefaultMinSize;
end;

procedure TdxGanttControlSplitterOptions.SetMinSize(const Value: Integer);
begin
  if (FMinSize <> Value) and (FMinSize >= 0) then
  begin
    FMinSize := Value;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlSplitterOptions.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed([TdxGanttControlOptionsChangedType.Layout]);
  end;
end;

procedure TdxGanttControlSplitterOptions.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) and (Value >= MinWidth) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

end.
