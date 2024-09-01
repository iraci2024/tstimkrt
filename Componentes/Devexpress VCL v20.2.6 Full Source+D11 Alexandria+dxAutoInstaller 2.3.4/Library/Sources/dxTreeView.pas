{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEditors }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL }
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

unit dxTreeView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Controls, Generics.Collections, Generics.Defaults,
  Graphics, ImgList, Forms, StdCtrls,
  dxCoreClasses, dxCustomTree, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxGraphics, cxGeometry, cxClasses,
  dxCoreGraphics;

type
  TdxTreeViewHitTest = class;
  TdxTreeViewOptionsView = class;
  TdxTreeViewViewInfo = class;
  TdxTreeViewNodeViewInfo = class;
  TdxTreeViewNode = class;
  TdxCustomTreeView = class;

  TdxTreeViewChange = (tvcContent, tvcLayout, tvcStructure);
  TdxTreeViewChanges = set of TdxTreeViewChange;

  { TdxTreeViewNode }

  TdxTreeViewNode = class(TdxTreeCustomNode)
  strict private
    FCaption: string;
    FCheckState: TcxCheckBoxState;
    FHideCheckBox: Boolean;
    FSelected: Boolean;

    function GetChecked: Boolean;
    function GetFirst: TdxTreeViewNode; inline;
    function GetItem(Index: Integer): TdxTreeViewNode; inline;
    function GetLast: TdxTreeViewNode; inline;
    function GetNext: TdxTreeViewNode; inline;
    function GetParent: TdxTreeViewNode; inline;
    function GetPrev: TdxTreeViewNode; inline;
    function GetRoot: TdxTreeViewNode; inline;
    function GetTreeView: TdxCustomTreeView; inline;
    function GetSelected: Boolean; inline;
    procedure SetCaption(const AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckState(AValue: TcxCheckBoxState);
    procedure SetHideCheckBox(const AValue: Boolean);
    procedure SetSelected(AValue: Boolean);
  protected
    FNeedHint: Boolean;

    procedure DoNodeExpandStateChanged; override;
    function GetHintText: string; virtual;
    function GetVisibleLevel: Integer;
    function IsHidden: Boolean;
  public
    function AddChild(ACaption: string = ''; AData: Pointer = nil)
      : TdxTreeViewNode; inline;
    function AddChildFirst: TdxTreeViewNode; inline;
    function AddNode(ANode, ARelative: TdxTreeViewNode; AData: Pointer;
      AttachMode: TdxTreeNodeAttachMode): TdxTreeViewNode; inline;
    //
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property CheckState: TcxCheckBoxState read FCheckState write SetCheckState;
    property HideCheckBox: Boolean read FHideCheckBox write SetHideCheckBox;
    property Items[Index: Integer]: TdxTreeViewNode read GetItem; default;
    // for internal use
    property First: TdxTreeViewNode read GetFirst; // for internal use
    property Last: TdxTreeViewNode read GetLast; // for internal use
    property Next: TdxTreeViewNode read GetNext; // for internal use
    property Parent: TdxTreeViewNode read GetParent; // for internal use
    property Prev: TdxTreeViewNode read GetPrev; // for internal use
    property Root: TdxTreeViewNode read GetRoot; // for internal use
    property Selected: Boolean read GetSelected write SetSelected;
    property TreeView: TdxCustomTreeView read GetTreeView; // for internal use
  end;

  TdxTreeViewNodeClass = class of TdxTreeViewNode;

  { TdxCustomTreeViewHintHelper }

  TdxCustomTreeViewHintHelper = class(TcxControlHintHelper)
  private
    FOwnerControl: TdxCustomTreeView;
  protected
    function GetOwnerControl: TcxControl; override;
    function PtInCaller(const P: TPoint): Boolean; override;
  public
    constructor Create(AOwnerControl: TdxCustomTreeView); virtual;
  end;

  { TdxCustomTreeView }

  TdxTreeViewCustomDrawNodeEvent = procedure(Sender: TdxCustomTreeView;
    Canvas: TcxCanvas; NodeViewInfo: TdxTreeViewNodeViewInfo;
    var Handled: Boolean) of object;
  TdxTreeViewNodeAllowEvent = procedure(Sender: TdxCustomTreeView;
    Node: TdxTreeViewNode; var Allow: Boolean) of object;
  TdxTreeViewNodeEvent = procedure(Sender: TdxCustomTreeView;
    Node: TdxTreeViewNode) of object;

  TdxCustomTreeView = class(TcxControl, IdxTreeOwner)
  strict private
  const
    dxDefaultHeight = 200;
    dxDefaultWidth = 100;
  strict private
    FAbsoluteVisibleNodes: TdxFastList;
    FChanges: TdxTreeViewChanges;
    FFocusedNode: TdxTreeViewNode;
    FFocusNodeOnMouseUp: Boolean;
    FHighlightedText: string;
    FHintHelper: TdxCustomTreeViewHintHelper;
    FHitTest: TdxTreeViewHitTest;
    FHottrackItem: TdxTreeViewNode;
    FLockCount: Integer;
    FOptionsView: TdxTreeViewOptionsView;
    FPressedItem: TdxTreeViewNode;
    FRoot: TdxTreeViewNode;
    FViewInfo: TdxTreeViewViewInfo;

    FOnCollapsed: TdxTreeViewNodeEvent;
    FOnCollapsing: TdxTreeViewNodeAllowEvent;
    FOnCustomDrawNode: TdxTreeViewCustomDrawNodeEvent;
    FOnDeletion: TdxTreeViewNodeEvent;
    FOnExpanded: TdxTreeViewNodeEvent;
    FOnExpanding: TdxTreeViewNodeAllowEvent;
    FOnGetChildren: TdxTreeViewNodeEvent;
    FOnNodeStateChanged: TdxTreeViewNodeEvent;
    FOnSelectionChanged: TNotifyEvent;

    procedure SetFocusedNode(AValue: TdxTreeViewNode);
    procedure SetHighlightedText(AValue: string);
    procedure SetHottrackItem(AValue: TdxTreeViewNode);
    procedure SetOptionsView(AValue: TdxTreeViewOptionsView);
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure BoundsChanged; override;
    function CanShowHint: Boolean;
    procedure Changed(AChanges: TdxTreeViewChanges);
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckHint;
    function CheckRTLPosition(APosition: Integer): Integer;
    function CreateOptionsView: TdxTreeViewOptionsView; virtual;
    function CreateViewInfo: TdxTreeViewViewInfo; virtual;
    procedure DoCancelMode; override;
    function DoCustomDrawNode(ACanvas: TcxCanvas;
      ANodeViewInfo: TdxTreeViewNodeViewInfo): Boolean; virtual;
    procedure DoFocusNodeByMouse(ANode: TdxTreeViewNode); virtual;
    procedure DoNodeExpandStateChanged(ANode: TdxTreeViewNode); virtual;
    procedure DoPaint; override;
    procedure DoSelectionChanged; virtual;
    procedure FocusEnter; override;
    procedure FocusLeave; override;
    procedure FontChanged; override;
    function GetDefaultHeight: Integer; virtual;
    function GetDefaultWidth: Integer; virtual;
    function GetNodeClass: TdxTreeViewNodeClass; overload; virtual;
    procedure InitScrollBarsParameters; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure NodeStateChanged(ANode: TdxTreeViewNode); virtual;
    procedure ProcessChanges(AChanges: TdxTreeViewChanges); virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    function ShowFirstLevelNodes: Boolean; virtual;
    procedure UpdateAbsoluteVisibleNodes; virtual;
    procedure UpdateViewPort(const APoint: TPoint); virtual;

    // Keyboard
    function CheckFocusedObject: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    // Mouse
    procedure CalculateHitTest(X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    // IdxTreeOwner
    procedure BeforeDelete(Sender: TdxTreeCustomNode);
    function CanCollapse(Sender: TdxTreeCustomNode): Boolean; virtual;
    function CanExpand(Sender: TdxTreeCustomNode): Boolean; virtual;
    procedure Collapsed(Sender: TdxTreeCustomNode); virtual;
    procedure DeleteNode(Sender: TdxTreeCustomNode); virtual;
    procedure Expanded(Sender: TdxTreeCustomNode); virtual;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;
    function GetNodeClass(ARelativeNode: TdxTreeCustomNode)
      : TdxTreeCustomNodeClass; overload;
    function GetNodeBounds(ANode: TdxTreeCustomNode): TRect;
    function GetOwner: TPersistent; reintroduce;
    function HasHottrack: Boolean; virtual;
    procedure InvalidateNode(ANode: TdxTreeCustomNode);
    procedure LoadChildren(Sender: TdxTreeCustomNode);
    procedure TreeNotification(Sender: TdxTreeCustomNode;
      ANotification: TdxTreeNodeNotifications);

    property AbsoluteVisibleNodes: TdxFastList read FAbsoluteVisibleNodes;
    property FocusNodeOnMouseUp: Boolean read FFocusNodeOnMouseUp
      write FFocusNodeOnMouseUp;
    property HighlightedText: string read FHighlightedText
      write SetHighlightedText;
    property HintHelper: TdxCustomTreeViewHintHelper read FHintHelper;
    property HitTest: TdxTreeViewHitTest read FHitTest;
    property HottrackItem: TdxTreeViewNode read FHottrackItem
      write SetHottrackItem;
    property OptionsView: TdxTreeViewOptionsView read FOptionsView
      write SetOptionsView;
    property PressedItem: TdxTreeViewNode read FPressedItem;
    property ViewInfo: TdxTreeViewViewInfo read FViewInfo;

    property ScrollBars default ssVertical;
    property OnCollapsed: TdxTreeViewNodeEvent read FOnCollapsed
      write FOnCollapsed;
    property OnCollapsing: TdxTreeViewNodeAllowEvent read FOnCollapsing
      write FOnCollapsing;
    property OnCustomDrawNode: TdxTreeViewCustomDrawNodeEvent
      read FOnCustomDrawNode write FOnCustomDrawNode;
    property OnDeletion: TdxTreeViewNodeEvent read FOnDeletion
      write FOnDeletion;
    property OnExpanded: TdxTreeViewNodeEvent read FOnExpanded
      write FOnExpanded;
    property OnExpanding: TdxTreeViewNodeAllowEvent read FOnExpanding
      write FOnExpanding;
    property OnGetChildren: TdxTreeViewNodeEvent read FOnGetChildren
      write FOnGetChildren;
    property OnNodeStateChanged: TdxTreeViewNodeEvent read FOnNodeStateChanged
      write FOnNodeStateChanged;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
      write FOnSelectionChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ExpandTo(ANode: TdxTreeViewNode);
    procedure FullRefresh;
    function GetNodeAtPos(const P: TPoint; out ANode: TdxTreeViewNode): Boolean;
    procedure MakeVisible(ANode: TdxTreeViewNode);
    procedure ScrollBy(ADeltaX, ADeltaY: Integer);
    //
    property FocusedNode: TdxTreeViewNode read FFocusedNode
      write SetFocusedNode; // for internal use
    property Root: TdxTreeViewNode read FRoot; // for internal use
  end;

  { TdxTreeViewPersistent }

  TdxTreeViewPersistent = class(TPersistent)
  strict private
    FTreeView: TdxCustomTreeView;
  protected
    property TreeView: TdxCustomTreeView read FTreeView;
  public
    constructor Create(ATreeView: TdxCustomTreeView); virtual;
  end;

  { TdxTreeViewOptionsView }

  TdxTreeViewOptionsView = class(TdxTreeViewPersistent)
  strict private
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FItemHeight: Integer;
    FRowSelect: Boolean;
    FShowCheckBoxes: Boolean;
    FShowLines: Boolean;
    FShowRoot: Boolean;

    procedure ImagesChangeHandler(Sender: TObject);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetItemHeight(AValue: Integer);
    procedure SetRowSelect(AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetShowLines(AValue: Boolean);
    procedure SetShowRoot(const Value: Boolean);
  protected
    procedure Changed(AChanges: TdxTreeViewChanges);
    procedure ChangeScale(M, D: Integer); virtual;
  public
    constructor Create(ATreeView: TdxCustomTreeView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property RowSelect: Boolean read FRowSelect write SetRowSelect
      default False;
    property ShowCheckBoxes: Boolean read FShowCheckBoxes
      write SetShowCheckBoxes default False;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;
    property ShowRoot: Boolean read FShowRoot write SetShowRoot default True;

  end;

  { TdxTreeViewHitTest }

  TdxTreeViewHitTest = class(TdxTreeViewPersistent)
  strict private
    FHitAtCheckBox: Boolean;
    FHitAtExpandButton: Boolean;
    FHitAtImage: Boolean;
    FHitAtSelection: Boolean;
    FHitAtText: Boolean;
    FHitObject: TObject;
    FHitPoint: TPoint;

    function GetHitAtNode: Boolean;
    function GetHitObjectAsNode: TdxTreeViewNode;
  public
    procedure Reset;

    property HitAtCheckBox: Boolean read FHitAtCheckBox write FHitAtCheckBox;
    property HitAtExpandButton: Boolean read FHitAtExpandButton
      write FHitAtExpandButton;
    property HitAtImage: Boolean read FHitAtImage write FHitAtImage;
    property HitAtNode: Boolean read GetHitAtNode;
    property HitAtSelection: Boolean read FHitAtSelection write FHitAtSelection;
    property HitAtText: Boolean read FHitAtText write FHitAtText;
    property HitObject: TObject read FHitObject write FHitObject;
    property HitObjectAsNode: TdxTreeViewNode read GetHitObjectAsNode;
    // for internal use
    property HitPoint: TPoint read FHitPoint write FHitPoint;
  end;

  { TdxTreeViewCustomViewInfo }

  TdxTreeViewCustomViewInfo = class abstract(TdxTreeViewPersistent)
  strict private
    function GetPainter: TcxCustomLookAndFeelPainter; inline;
    function GetScaleFactor: TdxScaleFactor;
  protected
    FBounds: TRect;

    procedure RightToLeftConversion(ABounds: TRect); virtual;
  public
    procedure Calculate(const ABounds: TRect); virtual;
    procedure CalculateHitTest(AHitTest: TdxTreeViewHitTest); virtual; abstract;
    procedure Draw(ACanvas: TcxCanvas); virtual; abstract;
    //
    property Bounds: TRect read FBounds;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxTreeViewNodeViewInfo }

  TdxTreeViewNodeViewInfo = class(TdxTreeViewCustomViewInfo)
  strict private
  const
    ElementsOffset = 4;
  strict private
    FData: TdxTreeViewNode;
    FImageColorPalette: IdxColorPalette;

    function GetHeight: Integer; inline;
    function GetImages: TCustomImageList; inline;
    function GetLevelOffset: Integer;
    function GetSelectionRect: TRect;
  protected
    FCheckBoxRect: TRect;
    FExpandButtonRect: TRect;
    FImageRect: TRect;
    FLevelIndent: Integer;
    FSelectionRect: TRect;
    FTextRect: TRect;
    FTextWidth: Integer;
    FTreeLineHorz: TRect;
    FTreeLineVert: TRect;

    procedure AdjustTextRect; virtual;
    procedure CalculateTreeLines; virtual;
    procedure DrawCaption(ACanvas: TcxCanvas); virtual;
    procedure DrawCheckBox(ACanvas: TcxCanvas); virtual;
    procedure DrawExpandButton(ACanvas: TcxCanvas); virtual;
    procedure DrawSelection(ACanvas: TcxCanvas); virtual;
    procedure DrawTreeLine(ACanvas: TcxCanvas; const R: TRect);
    function GetTextColor(ASelected: Boolean): TColor; virtual;
    procedure RightToLeftConversion(ABounds: TRect); override;
  public
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateHitTest(AHitTest: TdxTreeViewHitTest); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    function HasCheckBox: Boolean; virtual;
    function HasExpandButton: Boolean; virtual;
    function HasHottrack: Boolean; virtual;
    function HasImage: Boolean; virtual;
    function HasSelection: Boolean; virtual;
    procedure SetData(ANode: TdxTreeViewNode);

    property CheckBoxRect: TRect read FCheckBoxRect;
    property ExpandButtonRect: TRect read FExpandButtonRect;
    property Height: Integer read GetHeight;
    property ImageColorPalette: IdxColorPalette read FImageColorPalette
      write FImageColorPalette; // for internal use
    property ImageRect: TRect read FImageRect;
    property Images: TCustomImageList read GetImages;
    property LevelIndent: Integer read FLevelIndent;
    property LevelOffset: Integer read GetLevelOffset;
    property Node: TdxTreeViewNode read FData; // for internal use
    property SelectionRect: TRect read GetSelectionRect;
    property TextRect: TRect read FTextRect;
    property TextWidth: Integer read FTextWidth;
    property TreeLineHorz: TRect read FTreeLineHorz;
    property TreeLineVert: TRect read FTreeLineVert;
  end;

  { TdxTreeViewViewInfo }

  TdxTreeViewViewInfo = class(TdxTreeViewCustomViewInfo)
  strict private
  const
    dxContentOffset = 1;
  strict private
    FContentRect: TRect;
    FContentSize: TSize;
    FNodeViewInfo: TdxTreeViewNodeViewInfo;
    FViewPort: TPoint;

    function GetAbsoluteVisibleNodes: TdxFastList; inline;
    function GetContentOffset: TPoint;
    function GetNumberOfNodesInContentRect: Integer;
    procedure SetViewPort(const AValue: TPoint);
  protected
    procedure CalculateContentWidth;
    function CalculateNodeWidth(ANode: TdxTreeViewNode): Integer;
    function CreateNodeViewInfo: TdxTreeViewNodeViewInfo; virtual;
    function GetBackgroundColor: TColor; virtual;
    procedure RightToLeftConversion(ABounds: TRect); override;
    //
    property AbsoluteVisibleNodes: TdxFastList read GetAbsoluteVisibleNodes;
  public
    constructor Create(ATreeView: TdxCustomTreeView); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateHitTest(AHitTest: TdxTreeViewHitTest); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetNodeAtPos(P: TPoint; out ANodeIndex: Integer): Boolean;
    //
    property BackgroundColor: TColor read GetBackgroundColor;
    property ContentRect: TRect read FContentRect;
    property ContentSize: TSize read FContentSize;
    property NodeViewInfo: TdxTreeViewNodeViewInfo read FNodeViewInfo;
    property NumberOfNodesInContentRect: Integer
      read GetNumberOfNodesInContentRect;
    property ViewPort: TPoint read FViewPort write SetViewPort;
  end;

  { TdxTreeView }

  TdxInternalTreeView = class(TdxCustomTreeView)
  published
    property Align;
    property BiDiMode;
    property BorderStyle default cxcbsDefault;
    property DragCursor;
    property DragMode;
    property LookAndFeel;
    property OptionsView;

    property OnCollapsed;
    property OnCollapsing;
    property OnCustomDrawNode;
    property OnContextPopup;
    property OnDblClick;
    property OnDeletion;
    property OnExpanded;
    property OnExpanding;
    property OnGetChildren;
    property OnKeyDown;
    property OnNodeStateChanged;
    property OnSelectionChanged;
  end;

implementation

uses
  SysUtils, Math, cxDrawTextUtils, RTLConsts;

const
  AInc: array [Boolean] of Integer = (-1, 1);

function CallNodeAllowEvent(ANode: TdxTreeViewNode;
  AEvent: TdxTreeViewNodeAllowEvent): Boolean;
begin
  Result := True;
  if Assigned(AEvent) then
    AEvent(ANode.TreeView, TdxTreeViewNode(ANode), Result);
end;

procedure CallNodeEvent(ANode: TdxTreeViewNode; AEvent: TdxTreeViewNodeEvent);
begin
  if Assigned(AEvent) then
    AEvent(ANode.TreeView, TdxTreeViewNode(ANode));
end;

{ TdxTreeViewNode }

function TdxTreeViewNode.AddChild(ACaption: string = ''; AData: Pointer = nil)
  : TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited AddChild);
  Result.Caption := ACaption;
  Result.Data := AData;
end;

function TdxTreeViewNode.AddChildFirst: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited AddChildFirst);
end;

function TdxTreeViewNode.AddNode(ANode, ARelative: TdxTreeViewNode;
  AData: Pointer; AttachMode: TdxTreeNodeAttachMode): TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited AddNode(ANode, ARelative, AData,
    AttachMode));
end;

procedure TdxTreeViewNode.DoNodeExpandStateChanged;
begin
  TreeView.DoNodeExpandStateChanged(Self);
end;

function TdxTreeViewNode.GetHintText: string;
begin
  Result := Caption;
end;

function TdxTreeViewNode.GetVisibleLevel: Integer;
var
  ANode: TdxTreeViewNode;
begin
  if TreeView.ShowFirstLevelNodes then
    Result := Level
  else
  begin
    Result := -1;
    ANode := Parent;
    while ANode <> nil do
    begin
      if not ANode.IsHidden then
        Inc(Result);
      ANode := ANode.Parent;
    end;
  end;
end;

function TdxTreeViewNode.IsHidden: Boolean;
begin
  Result := (Parent <> nil) and Parent.IsRoot and
    not TreeView.ShowFirstLevelNodes;
end;

function TdxTreeViewNode.GetChecked: Boolean;
begin
  Result := CheckState = cbsChecked;
end;

function TdxTreeViewNode.GetFirst: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited First);
end;

function TdxTreeViewNode.GetItem(Index: Integer): TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited Items[Index]);
end;

function TdxTreeViewNode.GetLast: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited Last);
end;

function TdxTreeViewNode.GetNext: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited Next);
end;

function TdxTreeViewNode.GetParent: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited Parent);
end;

function TdxTreeViewNode.GetPrev: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited Prev);
end;

function TdxTreeViewNode.GetRoot: TdxTreeViewNode;
begin
  Result := TdxTreeViewNode(inherited Root);
end;

function TdxTreeViewNode.GetTreeView: TdxCustomTreeView;
begin
  Result := TdxCustomTreeView(Owner);
end;

function TdxTreeViewNode.GetSelected: Boolean;
begin
  Result := FSelected or (TreeView.FocusedNode = Self);
end;

procedure TdxTreeViewNode.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Notify([tnData]);
  end;
end;

procedure TdxTreeViewNode.SetChecked(AValue: Boolean);
const
  ACheckState: array [Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
begin
  if AValue <> Checked then
    CheckState := ACheckState[AValue];
end;

procedure TdxTreeViewNode.SetCheckState(AValue: TcxCheckBoxState);
begin
  if FCheckState <> AValue then
  begin
    FCheckState := AValue;
    Notify([tnData]);
    TreeView.NodeStateChanged(Self);
  end;
end;

procedure TdxTreeViewNode.SetHideCheckBox(const AValue: Boolean);
begin
  if FHideCheckBox <> AValue then
  begin
    FHideCheckBox := AValue;
    Notify([tnData]);
  end;
end;

procedure TdxTreeViewNode.SetSelected(AValue: Boolean);
begin
  if AValue <> FSelected then
  begin
    FSelected := AValue;
    TreeView.InvalidateNode(Self);
    TreeView.DoSelectionChanged;
  end;
end;

{ TdxCustomTreeViewHintHelper }

constructor TdxCustomTreeViewHintHelper.Create(AOwnerControl
  : TdxCustomTreeView);
begin
  inherited Create;
  FOwnerControl := AOwnerControl;
end;

function TdxCustomTreeViewHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FOwnerControl;
end;

function TdxCustomTreeViewHintHelper.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(GetHintControl.Bounds, P);
end;

{ TdxCustomTreeView }

constructor TdxCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Keys := [kArrows];
  FRoot := GetNodeClass.Create(Self);
  FHitTest := TdxTreeViewHitTest.Create(Self);
  FHintHelper := TdxCustomTreeViewHintHelper.Create(Self);
  FAbsoluteVisibleNodes := TdxFastList.Create;
  FOptionsView := CreateOptionsView;
  FViewInfo := CreateViewInfo;
  BorderStyle := cxcbsDefault;
  ScrollBars := ssVertical;
  TabStop := True;
  Height := GetDefaultHeight;
  Width := GetDefaultWidth;
end;

destructor TdxCustomTreeView.Destroy;
begin
  FreeAndNil(FAbsoluteVisibleNodes);
  FreeAndNil(FOptionsView);
  FreeAndNil(FViewInfo);
  FreeAndNil(FHintHelper);
  FreeAndNil(FHitTest);
  FreeAndNil(FRoot);
  inherited Destroy;
end;

function TdxCustomTreeView.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomTreeView.BeforeDestruction;
begin
  inherited;
  Root.Clear;
end;

procedure TdxCustomTreeView.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomTreeView.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed([]);
end;

procedure TdxCustomTreeView.ExpandTo(ANode: TdxTreeViewNode);
begin
  BeginUpdate;
  try
    ANode := ANode.Parent;
    while ANode <> nil do
    begin
      ANode.Expanded := True;
      ANode := ANode.Parent;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomTreeView.FullRefresh;
begin
  Changed([tvcContent, tvcLayout, tvcStructure]);
end;

function TdxCustomTreeView.GetNodeAtPos(const P: TPoint;
  out ANode: TdxTreeViewNode): Boolean;
var
  ANodeIndex: Integer;
begin
  Result := ViewInfo.GetNodeAtPos(P, ANodeIndex);
  if Result then
    ANode := AbsoluteVisibleNodes[ANodeIndex]
  else
    ANode := nil;
end;

procedure TdxCustomTreeView.MakeVisible(ANode: TdxTreeViewNode);

  function CalculateMakeVisibleDelta(const ARect: TRect): Integer;
  begin
    if ARect.Top < 0 then
      Result := ARect.Top
    else if ARect.Bottom > cxRectHeight(ViewInfo.ContentRect) then
      Result := Min(ARect.Bottom - cxRectHeight(ViewInfo.ContentRect),
        ARect.Top)
    else
      Result := 0;
  end;

var
  ARect: TRect;
  ANodeIndex: Integer;
begin
  if ANode = nil then
    Exit;
  BeginUpdate;
  try
    ExpandTo(ANode);
    UpdateAbsoluteVisibleNodes;
    ANodeIndex := AbsoluteVisibleNodes.IndexOf(ANode);
    if ANodeIndex >= 0 then
    begin
      ARect := cxRectBounds(0, ANodeIndex * ViewInfo.NodeViewInfo.Height, 0,
        ViewInfo.NodeViewInfo.Height);
      ARect := cxRectOffset(ARect, ViewInfo.ViewPort, False);
      ScrollBy(0, CalculateMakeVisibleDelta(ARect));
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomTreeView.ScrollBy(ADeltaX, ADeltaY: Integer);
begin
  UpdateViewPort(cxPointOffset(ViewInfo.ViewPort, ADeltaX, ADeltaY));
  UpdateScrollBars;
end;

procedure TdxCustomTreeView.BoundsChanged;
begin
  inherited BoundsChanged;
  Changed([tvcLayout]);
end;

function TdxCustomTreeView.CanShowHint: Boolean;
begin
  Result := ShowHint and Application.Active;
end;

procedure TdxCustomTreeView.Changed(AChanges: TdxTreeViewChanges);
begin
  FChanges := FChanges + AChanges;
  if FLockCount = 0 then
  begin
    if not IsDestroying then
      ProcessChanges(FChanges);
    FChanges := [];
  end;
end;

procedure TdxCustomTreeView.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  OptionsView.ChangeScale(M, D);
end;

procedure TdxCustomTreeView.CheckHint;
var
  AHitNode: TdxTreeViewNode;
  R: TRect;
begin
  AHitNode := HitTest.HitObjectAsNode;
  if (AHitNode = nil) or not(AHitNode.FNeedHint and HitTest.HitAtText) then
    HintHelper.CancelHint
  else if HintHelper.HintableObject <> AHitNode then
  begin
    R := ViewInfo.NodeViewInfo.Bounds;
    R := cxRectOffset(R, HitTest.HitPoint.X, HitTest.HitPoint.Y +
      cxRectHeight(R));
    if UseRightToLeftAlignment then
      OffsetRect(R, -cxRectWidth(R), 0);
    HintHelper.ShowHint(R, R, AHitNode.Caption, False, AHitNode);
  end;
end;

function TdxCustomTreeView.CheckRTLPosition(APosition: Integer): Integer;
begin
  if UseRightToLeftAlignment then
    Result := -APosition
  else
    Result := APosition;
end;

function TdxCustomTreeView.CreateOptionsView: TdxTreeViewOptionsView;
begin
  Result := TdxTreeViewOptionsView.Create(Self);
end;

function TdxCustomTreeView.CreateViewInfo: TdxTreeViewViewInfo;
begin
  Result := TdxTreeViewViewInfo.Create(Self);
end;

procedure TdxCustomTreeView.DoCancelMode;
begin
  inherited DoCancelMode;
  if HasHottrack then
    HottrackItem := nil;
end;

function TdxCustomTreeView.DoCustomDrawNode(ACanvas: TcxCanvas;
  ANodeViewInfo: TdxTreeViewNodeViewInfo): Boolean;
begin
  Result := False;
  if Assigned(OnCustomDrawNode) then
    OnCustomDrawNode(Self, ACanvas, ANodeViewInfo, Result);
end;

procedure TdxCustomTreeView.DoFocusNodeByMouse(ANode: TdxTreeViewNode);
begin
  FocusedNode := ANode;
end;

procedure TdxCustomTreeView.DoNodeExpandStateChanged(ANode: TdxTreeViewNode);
begin
end;

procedure TdxCustomTreeView.DoPaint;
begin
  inherited DoPaint;
  ViewInfo.Draw(Canvas);
end;

procedure TdxCustomTreeView.DoSelectionChanged;
begin
  CallNotify(OnSelectionChanged, Self);
end;

procedure TdxCustomTreeView.FocusEnter;
begin
  InvalidateNode(FocusedNode);
  inherited FocusEnter;
end;

procedure TdxCustomTreeView.FocusLeave;
begin
  InvalidateNode(FocusedNode);
  inherited FocusLeave;
end;

procedure TdxCustomTreeView.FontChanged;
begin
  inherited FontChanged;
  Canvas.Font := Font;
end;

function TdxCustomTreeView.GetDefaultHeight: Integer;
begin
  Result := dxDefaultHeight;
end;

function TdxCustomTreeView.GetDefaultWidth: Integer;
begin
  Result := dxDefaultWidth;
end;

function TdxCustomTreeView.GetNodeClass: TdxTreeViewNodeClass;
begin
  Result := TdxTreeViewNode;
end;

procedure TdxCustomTreeView.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbHorizontal, 0, ViewInfo.ContentSize.cx - 1,
    ViewInfo.NodeViewInfo.Height, cxRectWidth(ViewInfo.ContentRect),
    CheckRTLPosition(ViewInfo.ViewPort.X), True, True);
  SetScrollBarInfo(sbVertical, 0, ViewInfo.ContentSize.cy - 1,
    ViewInfo.NodeViewInfo.Height, cxRectHeight(ViewInfo.ContentRect),
    ViewInfo.ViewPort.Y, True, True);
end;

procedure TdxCustomTreeView.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  FullRefresh;
end;

procedure TdxCustomTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (OptionsView <> nil) and (OptionsView.Images = AComponent) then
      OptionsView.Images := nil;
  end;
end;

procedure TdxCustomTreeView.NodeStateChanged(ANode: TdxTreeViewNode);
begin
  CallNodeEvent(ANode, OnNodeStateChanged);
end;

procedure TdxCustomTreeView.ProcessChanges(AChanges: TdxTreeViewChanges);
begin
  if tvcStructure in AChanges then
  begin
    UpdateAbsoluteVisibleNodes;
    Include(AChanges, tvcLayout);
  end;
  if tvcLayout in AChanges then
  begin
    ViewInfo.Calculate(ClientBounds);
    if UseRightToLeftAlignment then
      ViewInfo.RightToLeftConversion(ClientBounds);
    if ScrollBars in [ssBoth, ssHorizontal] then
      ViewInfo.CalculateContentWidth;
    UpdateViewPort(ViewInfo.ViewPort);
    UpdateScrollBars;
    Include(AChanges, tvcContent);
  end;
  if tvcContent in AChanges then
    Invalidate;
end;

procedure TdxCustomTreeView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);

  function GetScrollPos(AStartPos, AStep, AMax: Integer): Integer;
  begin
    if AScrollCode in [scLineDown, scLineUp] then
      AScrollPos := AStartPos + AInc[AScrollCode = scLineDown] * AStep;
    Result := AScrollPos;
  end;

var
  P: TPoint;
begin
  if AScrollCode = scEndScroll then
    Exit;
  if AScrollBarKind = sbVertical then
    P := Point(ViewInfo.ViewPort.X, GetScrollPos(ViewInfo.ViewPort.Y,
      ViewInfo.NodeViewInfo.Height, ViewInfo.ContentSize.cy - 1))
  else
    P := Point(GetScrollPos(CheckRTLPosition(ViewInfo.ViewPort.X),
      ViewInfo.NodeViewInfo.Height, ViewInfo.ContentSize.cx - 1),
      ViewInfo.ViewPort.Y);
  UpdateViewPort(P);
  UpdateScrollBars;
  Invalidate;
end;

function TdxCustomTreeView.ShowFirstLevelNodes: Boolean;
begin
  Result := True;
end;

procedure TdxCustomTreeView.UpdateAbsoluteVisibleNodes;

  procedure PopulateLevel(ANode: TdxTreeViewNode);
  begin
    ANode := ANode.First;
    while ANode <> nil do
    begin
      if ANode.Visible then
      begin
        if ShowFirstLevelNodes or not ANode.IsHidden then
          FAbsoluteVisibleNodes.Add(ANode);
        if ANode.Expanded then
          PopulateLevel(ANode);
      end;
      ANode := ANode.Next;
    end;
  end;

begin
  FAbsoluteVisibleNodes.Count := 0;
  PopulateLevel(Root);
end;

procedure TdxCustomTreeView.UpdateViewPort(const APoint: TPoint);
begin
  ViewInfo.ViewPort := APoint;
end;

function TdxCustomTreeView.CheckFocusedObject: Boolean;
begin
  Result := FocusedNode <> nil;
  if not Result and (AbsoluteVisibleNodes.Count > 0) then
    FocusedNode := AbsoluteVisibleNodes.First;
end;

procedure TdxCustomTreeView.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SelectNextNode(ASkipFactor: Integer);
  var
    AIndex: Integer;
  begin
    if CheckFocusedObject then
    begin
      AIndex := AbsoluteVisibleNodes.IndexOf(FocusedNode);
      AIndex := Max(0, Min(AIndex + ASkipFactor,
        AbsoluteVisibleNodes.Count - 1));
      FocusedNode := AbsoluteVisibleNodes[AIndex];
    end;
  end;

  procedure ExpandAll(ANode: TdxTreeViewNode);
  begin
    if not ANode.HasChildren then
      Exit;
    BeginUpdate;
    try
      ANode.Expanded := True;
      ANode := ANode.First;
      while ANode <> nil do
      begin
        ExpandAll(ANode);
        ANode := ANode.Next;
      end;
    finally
      EndUpdate;
    end;
  end;

begin
  inherited;

  case Key of
    VK_UP:
      SelectNextNode(-1);

    VK_DOWN:
      SelectNextNode(1);

    VK_NEXT:
      SelectNextNode(ViewInfo.NumberOfNodesInContentRect);

    VK_PRIOR:
      SelectNextNode(-ViewInfo.NumberOfNodesInContentRect);

    VK_SPACE:
      if CheckFocusedObject then
        FocusedNode.Checked := not FocusedNode.Checked;

    VK_HOME:
      if AbsoluteVisibleNodes.Count > 0 then
        FocusedNode := AbsoluteVisibleNodes.First;

    VK_END:
      if AbsoluteVisibleNodes.Count > 0 then
        FocusedNode := AbsoluteVisibleNodes.Last;

    VK_LEFT:
      if CheckFocusedObject then
      begin
        if FocusedNode.Expanded and CanCollapse(FocusedNode) then
          FocusedNode.Expanded := False
        else if (FocusedNode.Parent <> nil) and not FocusedNode.Parent.IsRoot
        then
          FocusedNode := FocusedNode.Parent;
      end;

    VK_RIGHT:
      if CheckFocusedObject then
      begin
        if not FocusedNode.Expanded then
          FocusedNode.Expanded := True
        else if FocusedNode.First <> nil then
          FocusedNode := FocusedNode.First;
      end;
    VK_ADD:
      if FocusedNode <> nil then
        FocusedNode.Expanded := True;
    VK_SUBTRACT:
      if FocusedNode <> nil then
        FocusedNode.Expanded := False;
    VK_MULTIPLY:
      begin
        ExpandAll(FocusedNode);
        MakeVisible(FocusedNode);
      end;
  end;
end;

procedure TdxCustomTreeView.CalculateHitTest(X, Y: Integer);
begin
  HitTest.Reset;
  HitTest.HitPoint := Point(X, Y);
  ViewInfo.CalculateHitTest(HitTest);
end;

procedure TdxCustomTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  SetFocus;
  CalculateHitTest(X, Y);
  FPressedItem := HitTest.HitObjectAsNode;
  HintHelper.MouseDown;
  if HitTest.HitAtNode and (ssLeft in Shift) then
  begin
    if HitTest.HitAtExpandButton or HitTest.HitAtText and (ssDouble in Shift)
    then
      HitTest.HitObjectAsNode.Expanded := not HitTest.HitObjectAsNode.Expanded
    else if not FocusNodeOnMouseUp then
      if HitTest.HitAtCheckBox then
      begin
        FocusedNode := HitTest.HitObjectAsNode;
        HitTest.HitObjectAsNode.Checked := not HitTest.HitObjectAsNode.Checked;
      end
      else if HitTest.HitAtSelection then
        FocusedNode := HitTest.HitObjectAsNode;
  end;
end;

procedure TdxCustomTreeView.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if HasHottrack then
    HottrackItem := nil;
end;

procedure TdxCustomTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AIsHitTestCalculated: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  AIsHitTestCalculated := False;
  if HasHottrack then
    if cxRectPtIn(ClientBounds, cxPoint(X, Y)) then
    begin
      CalculateHitTest(X, Y);
      AIsHitTestCalculated := True;
      HottrackItem := HitTest.HitObjectAsNode;
    end
    else
      HottrackItem := nil;
  if CanShowHint then
  begin
    if not AIsHitTestCalculated then
      CalculateHitTest(X, Y);
    CheckHint;
  end;
end;

procedure TdxCustomTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  CalculateHitTest(X, Y);
  if FocusNodeOnMouseUp and HitTest.HitAtNode and (Button = mbLeft) and
    (FPressedItem = HitTest.HitObject) then
  begin
    if HitTest.HitAtCheckBox then
    begin
      FocusedNode := HitTest.HitObjectAsNode;
      HitTest.HitObjectAsNode.Checked := not HitTest.HitObjectAsNode.Checked;
    end
    else if HitTest.HitAtSelection then
      DoFocusNodeByMouse(HitTest.HitObjectAsNode);
  end;
  FPressedItem := nil;
end;

procedure TdxCustomTreeView.BeforeDelete(Sender: TdxTreeCustomNode);
var
  ANode: TdxTreeViewNode;
  ASender: TdxTreeViewNode absolute Sender;
begin
  CallNodeEvent(ASender, OnDeletion);
  if FocusedNode = ASender then
  begin
    ANode := ASender.Next;
    if ANode = nil then
      ANode := ASender.Prev;
    if ANode = nil then
      ANode := ASender.Parent;
    FocusedNode := ANode;
  end;
end;

function TdxCustomTreeView.CanCollapse(Sender: TdxTreeCustomNode): Boolean;
var
  ASender: TdxTreeViewNode absolute Sender;
begin
  Result := CallNodeAllowEvent(ASender, OnCollapsing);
end;

function TdxCustomTreeView.CanExpand(Sender: TdxTreeCustomNode): Boolean;
var
  ASender: TdxTreeViewNode absolute Sender;
begin
  Result := CallNodeAllowEvent(ASender, OnExpanding);
end;

procedure TdxCustomTreeView.Collapsed(Sender: TdxTreeCustomNode);
var
  ASender: TdxTreeViewNode absolute Sender;
begin
  CallNodeEvent(ASender, OnCollapsed);
end;

procedure TdxCustomTreeView.DeleteNode(Sender: TdxTreeCustomNode);
begin
  Changed([tvcStructure]);
end;

procedure TdxCustomTreeView.Expanded(Sender: TdxTreeCustomNode);
var
  ASender: TdxTreeViewNode absolute Sender;
begin
  CallNodeEvent(ASender, OnExpanded);
end;

function TdxCustomTreeView.GetMainScrollBarsClass
  : TcxControlCustomScrollBarsClass;
begin
  if not IsPopupScrollBars then
    Result := TcxControlScrollBars
  else
    Result := inherited GetMainScrollBarsClass;
end;

function TdxCustomTreeView.GetNodeClass(ARelativeNode: TdxTreeCustomNode)
  : TdxTreeCustomNodeClass;
begin
  Result := GetNodeClass;
end;

function TdxCustomTreeView.GetNodeBounds(ANode: TdxTreeCustomNode): TRect;
var
  ANodeIndex: Integer;
begin
  Result := cxNullRect;
  if ANode <> nil then
  begin
    ANodeIndex := AbsoluteVisibleNodes.IndexOf(ANode);
    if ANodeIndex >= 0 then
    begin
      Result := cxRectBounds(0, ANodeIndex * ViewInfo.NodeViewInfo.Height,
        ClientWidth + ViewInfo.ViewPort.X, ViewInfo.NodeViewInfo.Height);
      Result := cxRectOffset(Result, ViewInfo.ContentRect.TopLeft);
      Result := cxRectOffset(Result, ViewInfo.ViewPort, False);
    end;
  end;
end;

function TdxCustomTreeView.GetOwner: TPersistent;
begin
  Result := Self;
end;

function TdxCustomTreeView.HasHottrack: Boolean;
begin
  Result := False;
end;

procedure TdxCustomTreeView.InvalidateNode(ANode: TdxTreeCustomNode);
var
  ABounds: TRect;
begin
  if ANode = nil then
    Exit;
  ABounds := GetNodeBounds(ANode);
  if cxRectIsEmpty(ABounds) then
    Exit;
  if cxRectIntersect(ClientBounds, ABounds) then
    InvalidateRect(ABounds, True);
end;

procedure TdxCustomTreeView.LoadChildren(Sender: TdxTreeCustomNode);
var
  ASender: TdxTreeViewNode absolute Sender;
begin
  CallNodeEvent(ASender, OnGetChildren);
end;

procedure TdxCustomTreeView.TreeNotification(Sender: TdxTreeCustomNode;
  ANotification: TdxTreeNodeNotifications);
var
  ASender: TdxTreeViewNode absolute Sender;
begin
  BeginUpdate;
  try
    if tnStructure in ANotification then
      Changed([tvcStructure]);
    if tnData in ANotification then
      Changed([tvcContent]);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomTreeView.SetFocusedNode(AValue: TdxTreeViewNode);
begin
  if FFocusedNode <> AValue then
  begin
    FFocusedNode := AValue;
    MakeVisible(FocusedNode);
    if not IsDestroying then
      DoSelectionChanged;
    Invalidate;
  end;
end;

procedure TdxCustomTreeView.SetHighlightedText(AValue: string);
begin
  if AValue <> HighlightedText then
  begin
    FHighlightedText := AValue;
    Changed([tvcContent]);
  end;
end;

procedure TdxCustomTreeView.SetHottrackItem(AValue: TdxTreeViewNode);
begin
  if FHottrackItem <> AValue then
  begin
    InvalidateNode(FHottrackItem);
    FHottrackItem := AValue;
    InvalidateNode(FHottrackItem);
  end;
end;

procedure TdxCustomTreeView.SetOptionsView(AValue: TdxTreeViewOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

{ TdxTreeViewPersistent }

constructor TdxTreeViewPersistent.Create(ATreeView: TdxCustomTreeView);
begin
  FTreeView := ATreeView;
end;

{ TdxTreeViewOptionsView }

constructor TdxTreeViewOptionsView.Create(ATreeView: TdxCustomTreeView);
begin
  inherited Create(ATreeView);
  FShowLines := True;
  FShowRoot := True;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChangeHandler;
end;

destructor TdxTreeViewOptionsView.Destroy;
begin
  FreeAndNil(FImagesChangeLink);
  inherited Destroy;
end;

procedure TdxTreeViewOptionsView.Assign(Source: TPersistent);
begin
  if Source is TdxTreeViewOptionsView then
  begin
    Images := TdxTreeViewOptionsView(Source).Images;
    RowSelect := TdxTreeViewOptionsView(Source).RowSelect;
    ShowCheckBoxes := TdxTreeViewOptionsView(Source).ShowCheckBoxes;
    ShowLines := TdxTreeViewOptionsView(Source).ShowLines;
    ShowRoot := TdxTreeViewOptionsView(Source).ShowRoot;
  end;
end;

procedure TdxTreeViewOptionsView.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Images := nil;
end;

procedure TdxTreeViewOptionsView.Changed(AChanges: TdxTreeViewChanges);
begin
  TreeView.Changed(AChanges);
end;

procedure TdxTreeViewOptionsView.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TdxTreeViewOptionsView.ImagesChangeHandler(Sender: TObject);
begin
  Changed([tvcContent]);
end;

procedure TdxTreeViewOptionsView.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FImagesChangeLink, TreeView);
  Changed([tvcLayout]);
end;

procedure TdxTreeViewOptionsView.SetItemHeight(AValue: Integer);
begin
  if AValue <> ItemHeight then
  begin
    FItemHeight := AValue;
    Changed([tvcLayout]);
  end;
end;

procedure TdxTreeViewOptionsView.SetRowSelect(AValue: Boolean);
begin
  if AValue <> RowSelect then
  begin
    FRowSelect := AValue;
    Changed([tvcContent, tvcLayout]);
  end;
end;

procedure TdxTreeViewOptionsView.SetShowCheckBoxes(AValue: Boolean);
begin
  if FShowCheckBoxes <> AValue then
  begin
    FShowCheckBoxes := AValue;
    Changed([tvcLayout]);
  end;
end;

procedure TdxTreeViewOptionsView.SetShowLines(AValue: Boolean);
begin
  if FShowLines <> AValue then
  begin
    FShowLines := AValue;
    Changed([tvcLayout]);
  end;
end;

procedure TdxTreeViewOptionsView.SetShowRoot(const Value: Boolean);
begin
  if FShowRoot <> Value then
  begin
    FShowRoot := Value;
    Changed([tvcLayout]);
  end;
end;

{ TdxTreeViewHitTest }

function TdxTreeViewHitTest.GetHitAtNode: Boolean;
begin
  Result := HitObject is TdxTreeViewNode;
end;

function TdxTreeViewHitTest.GetHitObjectAsNode: TdxTreeViewNode;
begin
  if HitAtNode then
    Result := TdxTreeViewNode(HitObject)
  else
    Result := nil;
end;

procedure TdxTreeViewHitTest.Reset;
begin
  FHitAtCheckBox := False;
  FHitAtSelection := False;
  FHitAtExpandButton := False;
  FHitAtImage := False;
  FHitAtText := False;
  FHitObject := nil;
  FHitPoint := cxNullPoint;
end;

{ TdxTreeViewCustomViewInfo }

procedure TdxTreeViewCustomViewInfo.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

procedure TdxTreeViewCustomViewInfo.RightToLeftConversion(ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, ABounds);
end;

function TdxTreeViewCustomViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := TreeView.LookAndFeelPainter;
end;

function TdxTreeViewCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TreeView.ScaleFactor;
end;

{ TdxTreeViewNodeViewInfo }

procedure TdxTreeViewNodeViewInfo.Calculate(const ABounds: TRect);

  function MeasureHeight(AImageSize, AExpandButtonSize,
    ACheckHeight: Integer): Integer;
  begin
    Result := 2 * ScaleFactor.Apply(cxTextOffset) +
      Max(Max(AImageSize, ACheckHeight), Max(AExpandButtonSize,
      TdxTextMeasurer.TextLineHeight(TreeView.Font)));
    Result := Max(Result, ScaleFactor.Apply(TreeView.OptionsView.ItemHeight));
  end;

  function PlaceArea(var R: TRect; AAreaWidth, AAreaHeight: Integer): TRect;
  begin
    Result := cxRectSetWidth(R, AAreaWidth);
    Result := cxRectCenterVertically(Result, AAreaHeight);
    R.Left := Result.Right + ScaleFactor.Apply(ElementsOffset);
  end;

var
  AExpandButtonSize: Integer;
  ACheckSize: TSize;
  AImageSize: TSize;
begin
  AImageSize := dxGetImageSize(Images, ScaleFactor);
  AExpandButtonSize := Painter.ScaledExpandButtonAreaSize(ScaleFactor);
  ACheckSize := Painter.ScaledCheckButtonAreaSize(ScaleFactor);

  FLevelIndent := AExpandButtonSize + ScaleFactor.Apply(ElementsOffset);
  FBounds := cxRectSetHeight(ABounds, MeasureHeight(AImageSize.cy,
    AExpandButtonSize, ACheckSize.cy));
  FTextRect := cxRectInflate(FBounds, -ScaleFactor.Apply(cxTextOffset));

  FExpandButtonRect := PlaceArea(FTextRect, AExpandButtonSize,
    AExpandButtonSize);

  if TreeView.OptionsView.ShowCheckBoxes then
    FCheckBoxRect := PlaceArea(FTextRect, ACheckSize.cx, ACheckSize.cy)
  else
    FCheckBoxRect := cxNullRect;

  if Images <> nil then
    FImageRect := PlaceArea(FTextRect, AImageSize.cx, AImageSize.cy)
  else
    FImageRect := cxNullRect;

  FSelectionRect := TextRect;
  if not cxRectIsEmpty(ImageRect) then
    FSelectionRect := cxRectUnion(ImageRect, SelectionRect);
  FSelectionRect := cxRectInflate(SelectionRect,
    ScaleFactor.Apply(cxTextOffset));
end;

procedure TdxTreeViewNodeViewInfo.CalculateHitTest
  (AHitTest: TdxTreeViewHitTest);
begin
  if PtInRect(Bounds, AHitTest.HitPoint) or TreeView.OptionsView.RowSelect and
    PtInRect(SelectionRect, AHitTest.HitPoint) then
  begin
    AHitTest.HitObject := Node;
    AHitTest.HitAtCheckBox := PtInRect(CheckBoxRect, AHitTest.HitPoint);
    AHitTest.HitAtExpandButton := PtInRect(ExpandButtonRect, AHitTest.HitPoint);
    AHitTest.HitAtImage := PtInRect(ImageRect, AHitTest.HitPoint);
    AHitTest.HitAtSelection := PtInRect(SelectionRect, AHitTest.HitPoint);
    AHitTest.HitAtText := PtInRect(TextRect, AHitTest.HitPoint);
  end;
end;

procedure TdxTreeViewNodeViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if TreeView.DoCustomDrawNode(ACanvas, Self) then
    Exit;
  if HasSelection or HasHottrack then
    DrawSelection(ACanvas);
  if HasExpandButton then
    DrawExpandButton(ACanvas);
  if HasCheckBox then
    DrawCheckBox(ACanvas);
  if HasImage then
    cxDrawImage(ACanvas, ImageRect, nil, Images, Node.ImageIndex, True,
      ImageColorPalette, ScaleFactor);
  DrawCaption(ACanvas);
end;

procedure TdxTreeViewNodeViewInfo.SetData(ANode: TdxTreeViewNode);
begin
  if FData <> ANode then
  begin
    FData := ANode;
    AdjustTextRect;
    CalculateTreeLines;
  end;
end;

procedure TdxTreeViewNodeViewInfo.AdjustTextRect;
var
  ATotalWidth, AWidth: Integer;
begin
  if Node <> nil then
  begin
    Node.FNeedHint := False;
    FTextWidth := TdxTextMeasurer.TextWidthTO(TreeView.Font, Node.Caption);
    if TreeView.UseRightToLeftAlignment then
      ATotalWidth := FTextRect.Right - Bounds.Left + LevelOffset
    else
      ATotalWidth := Bounds.Right - FTextRect.Left - LevelOffset;
    Inc(ATotalWidth, TreeView.CheckRTLPosition(TreeView.ViewInfo.ViewPort.X));
    AWidth := Min(ATotalWidth, FTextWidth);
    Node.FNeedHint := FTextWidth > ATotalWidth;
  end
  else if TreeView.UseRightToLeftAlignment then
    AWidth := FTextRect.Right - Bounds.Left
  else
    AWidth := Bounds.Right - FTextRect.Left;

  if TreeView.UseRightToLeftAlignment then
    FTextRect.Left := FTextRect.Right - AWidth
  else
    FTextRect.Right := FTextRect.Left + AWidth;
end;

procedure TdxTreeViewNodeViewInfo.CalculateTreeLines;
begin
end;

procedure TdxTreeViewNodeViewInfo.DrawCaption(ACanvas: TcxCanvas);
const
  AFlags: array [Boolean] of Cardinal = (CXTO_LEFT or CXTO_CENTER_VERTICALLY or
    CXTO_SINGLELINE or CXTO_END_ELLIPSIS, CXTO_RIGHT or
    CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or CXTO_END_ELLIPSIS);
var
  ARect: TRect;
  ASelStart: Integer;
begin
  ARect := TextRect;
  ASelStart := Pos(AnsiUpperCase(TreeView.HighlightedText),
    AnsiUpperCase(Node.Caption)) - 1;
  cxTextOut(ACanvas.Canvas, Node.Caption, ARect,
    AFlags[TreeView.UseRightToLeftAlignment], ASelStart,
    Length(TreeView.HighlightedText), ACanvas.Font, clHighlight,
    clHighlightText, 0, 0, 0, GetTextColor(HasSelection));
end;

procedure TdxTreeViewNodeViewInfo.DrawCheckBox(ACanvas: TcxCanvas);
begin
  Painter.DrawScaledCheckButton(ACanvas, CheckBoxRect, cxbsNormal,
    Node.CheckState, ScaleFactor);
end;

procedure TdxTreeViewNodeViewInfo.DrawExpandButton(ACanvas: TcxCanvas);
var
  AState: TcxExpandButtonState;
begin
  if TreeView.OptionsView.RowSelect and HasSelection then
    AState := cebsSelected
  else
    AState := cebsNormal;
  Painter.DrawScaledExpandButton(ACanvas, ExpandButtonRect, Node.Expanded,
    ScaleFactor, clDefault, AState);
end;

procedure TdxTreeViewNodeViewInfo.DrawSelection(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := SelectionRect;
  if not TreeView.OptionsView.RowSelect then
    if TreeView.UseRightToLeftAlignment then
      Dec(R.Left, LevelOffset)
    else
      Dec(R.Right, LevelOffset);
  ACanvas.FillRect(R, Painter.DefaultSelectionColor);
end;

procedure TdxTreeViewNodeViewInfo.DrawTreeLine(ACanvas: TcxCanvas;
  const R: TRect);
begin
  cxFillHalfToneRect(ACanvas.Canvas, R, TreeView.ViewInfo.BackgroundColor,
    Painter.DefaultTreeListTreeLineColor);
end;

function TdxTreeViewNodeViewInfo.GetTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := Painter.DefaultSelectionTextColor
  else
    Result := cxGetActualColor(Painter.DefaultEditorTextColor
      (not TreeView.Enabled), clWindowText);
end;

function TdxTreeViewNodeViewInfo.HasCheckBox: Boolean;
begin
  Result := not cxRectIsEmpty(CheckBoxRect) and not Node.HideCheckBox;
end;

function TdxTreeViewNodeViewInfo.HasExpandButton: Boolean;
begin
  Result := not cxRectIsEmpty(ExpandButtonRect) and Node.HasChildren;
end;

function TdxTreeViewNodeViewInfo.HasHottrack: Boolean;
begin
  Result := TreeView.HottrackItem = Node;
end;

function TdxTreeViewNodeViewInfo.HasImage: Boolean;
begin
  Result := not cxRectIsEmpty(ImageRect);
end;

function TdxTreeViewNodeViewInfo.HasSelection: Boolean;
begin
  Result := Node.Selected;
end;

procedure TdxTreeViewNodeViewInfo.RightToLeftConversion(ABounds: TRect);
begin
  inherited RightToLeftConversion(ABounds);
  FCheckBoxRect := TdxRightToLeftLayoutConverter.ConvertRect
    (CheckBoxRect, ABounds);
  FExpandButtonRect := TdxRightToLeftLayoutConverter.ConvertRect
    (ExpandButtonRect, ABounds);
  FImageRect := TdxRightToLeftLayoutConverter.ConvertRect(ImageRect, ABounds);
  FSelectionRect := TdxRightToLeftLayoutConverter.ConvertRect
    (SelectionRect, ABounds);
  FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(TextRect, ABounds);
  FLevelIndent := -LevelIndent;
end;

function TdxTreeViewNodeViewInfo.GetHeight: Integer;
begin
  Result := cxRectHeight(Bounds)
end;

function TdxTreeViewNodeViewInfo.GetImages: TCustomImageList;
begin
  Result := TreeView.OptionsView.Images;
end;

function TdxTreeViewNodeViewInfo.GetLevelOffset: Integer;
begin
  if Node <> nil then
  begin
    Result := Node.GetVisibleLevel * FLevelIndent;
    if not TreeView.OptionsView.ShowRoot then
      Dec(Result, FLevelIndent);
  end
  else
    Result := 0;
end;

function TdxTreeViewNodeViewInfo.GetSelectionRect: TRect;
begin
  if TreeView.OptionsView.RowSelect then
    Result := cxRectOffsetHorz(Bounds, -LevelOffset)
  else
    Result := FSelectionRect;
  if TreeView.UseRightToLeftAlignment then
    Inc(Result.Left, TreeView.ViewInfo.ViewPort.X)
  else
    Inc(Result.Right, TreeView.ViewInfo.ViewPort.X);
end;

{ TdxTreeViewViewInfo }

constructor TdxTreeViewViewInfo.Create(ATreeView: TdxCustomTreeView);
begin
  inherited;
  FNodeViewInfo := CreateNodeViewInfo;
end;

destructor TdxTreeViewViewInfo.Destroy;
begin
  FreeAndNil(FNodeViewInfo);
  inherited;
end;

procedure TdxTreeViewViewInfo.Calculate(const ABounds: TRect);
begin
  inherited;
  FContentRect := cxRectInflate(Bounds, -dxContentOffset);
  NodeViewInfo.Calculate(ContentRect);
  FContentSize := cxSize(cxRectWidth(ContentRect), AbsoluteVisibleNodes.Count *
    NodeViewInfo.Height);
end;

procedure TdxTreeViewViewInfo.CalculateHitTest(AHitTest: TdxTreeViewHitTest);
var
  ANodeIndex: Integer;
  APrevHitPoint: TPoint;
begin
  if GetNodeAtPos(AHitTest.HitPoint, ANodeIndex) then
  begin
    APrevHitPoint := AHitTest.HitPoint;
    NodeViewInfo.SetData(AbsoluteVisibleNodes[ANodeIndex]);
    try
      AHitTest.HitPoint := cxPointOffset(AHitTest.HitPoint,
        GetContentOffset, False);
      AHitTest.HitPoint := cxPointOffset(AHitTest.HitPoint,
        -NodeViewInfo.LevelOffset, -ANodeIndex * NodeViewInfo.Height);
      NodeViewInfo.CalculateHitTest(AHitTest);
      AHitTest.HitPoint := APrevHitPoint;
    finally
      NodeViewInfo.SetData(nil);
    end;
  end;
end;

procedure TdxTreeViewViewInfo.Draw(ACanvas: TcxCanvas);
var
  AOffset: TPoint;
  I: Integer;
  AFirstVisibleIndex: Integer;
  ALastVisibleIndex: Integer;
begin
  ACanvas.FillRect(Bounds, BackgroundColor);
  ACanvas.SaveState;
  try
    AOffset := GetContentOffset;
    AFirstVisibleIndex := Trunc(-AOffset.Y / NodeViewInfo.Height);
    ALastVisibleIndex :=
      Min(Ceil(AFirstVisibleIndex + NumberOfNodesInContentRect),
      AbsoluteVisibleNodes.Count - 1);
    MoveWindowOrg(ACanvas.Handle, AOffset.X, AOffset.Y + AFirstVisibleIndex *
      NodeViewInfo.Height);
    for I := AFirstVisibleIndex to ALastVisibleIndex do
    begin
      NodeViewInfo.SetData(AbsoluteVisibleNodes[I]);
      MoveWindowOrg(ACanvas.Handle, NodeViewInfo.LevelOffset, 0);
      if ACanvas.RectVisible(NodeViewInfo.Bounds) then
        NodeViewInfo.Draw(ACanvas);
      MoveWindowOrg(ACanvas.Handle, -NodeViewInfo.LevelOffset,
        NodeViewInfo.Height);
    end;
  finally
    NodeViewInfo.SetData(nil);
    ACanvas.RestoreState;
  end;
end;

function TdxTreeViewViewInfo.GetNodeAtPos(P: TPoint;
  out ANodeIndex: Integer): Boolean;
begin
  Result := False;
  if PtInRect(Bounds, P) then
  begin
    P := cxPointOffset(P, ViewPort);
    P := cxPointOffset(P, ContentRect.TopLeft, False);
    ANodeIndex := Trunc(P.Y / NodeViewInfo.Height);
    Result := InRange(ANodeIndex, 0, AbsoluteVisibleNodes.Count - 1);
  end;
end;

function TdxTreeViewViewInfo.GetNumberOfNodesInContentRect: Integer;
begin
  Result := cxRectHeight(ContentRect) div NodeViewInfo.Height
end;

procedure TdxTreeViewViewInfo.SetViewPort(const AValue: TPoint);
begin
  FViewPort.X := TreeView.CheckRTLPosition
    (Max(0, Min(AValue.X, ContentSize.cx - cxRectWidth(ContentRect))));
  FViewPort.Y :=
    Max(0, Min(AValue.Y, ContentSize.cy - cxRectHeight(ContentRect)));
end;

function TdxTreeViewViewInfo.GetAbsoluteVisibleNodes: TdxFastList;
begin
  Result := TreeView.AbsoluteVisibleNodes;
end;

function TdxTreeViewViewInfo.GetBackgroundColor: TColor;
begin
  Result := cxGetActualColor(Painter.DefaultEditorBackgroundColor
    (not TreeView.Enabled), clWindow);
end;

function TdxTreeViewViewInfo.GetContentOffset: TPoint;
begin
  Result := cxPointOffset(cxNullPoint, ViewPort, False);
end;

procedure TdxTreeViewViewInfo.CalculateContentWidth;
var
  I, AWidth: Integer;
begin
  AWidth := cxRectWidth(FContentRect);
  for I := 0 to TreeView.AbsoluteVisibleNodes.Count - 1 do
    AWidth := Max(AWidth,
      CalculateNodeWidth(TreeView.AbsoluteVisibleNodes.List[I]));
  NodeViewInfo.SetData(nil);
  FContentSize.cx := AWidth;
end;

function TdxTreeViewViewInfo.CalculateNodeWidth(ANode: TdxTreeViewNode)
  : Integer;
begin
  NodeViewInfo.SetData(ANode);
  if TreeView.UseRightToLeftAlignment then
    Result := Bounds.Right - NodeViewInfo.TextRect.Right +
      NodeViewInfo.TextWidth + ScaleFactor.Apply(cxTextOffset)
  else
    Result := NodeViewInfo.TextRect.Left + NodeViewInfo.TextWidth;
  Inc(Result, Abs(NodeViewInfo.LevelOffset));
end;

function TdxTreeViewViewInfo.CreateNodeViewInfo: TdxTreeViewNodeViewInfo;
begin
  Result := TdxTreeViewNodeViewInfo.Create(TreeView);
end;

procedure TdxTreeViewViewInfo.RightToLeftConversion(ABounds: TRect);
begin
  inherited RightToLeftConversion(ABounds);
  NodeViewInfo.RightToLeftConversion(ABounds);
end;

end.
