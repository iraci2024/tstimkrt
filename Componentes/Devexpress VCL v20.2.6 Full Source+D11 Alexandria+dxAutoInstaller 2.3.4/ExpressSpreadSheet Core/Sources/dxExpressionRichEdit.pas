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

unit dxExpressionRichEdit;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Windows, Messages, Controls, Graphics, ImgList,
  Generics.Defaults, Generics.Collections,
  dxCore, cxContainer, dxCoreGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  cxEdit, dxMessages,
  cxRichEdit, dxAutoCompleteWindow, cxListBox, cxGraphics;

type
  TdxExpressionRichEdit = class;
  TdxExpressionRichEditAutoCompleteWindow = class;

  { TdxExpressionRichEditSuggestion }

  TdxExpressionRichEditSuggestion = record
  strict private
    FCaption: string;
    FImageIndex: Integer;
    FData: Pointer;
  public
    constructor Create(const ACaption: string; AImageIndex: Integer = -1;
      AData: Pointer = nil);

    property Caption: string read FCaption;
    property ImageIndex: Integer read FImageIndex;
    property Data: Pointer read FData;
  end;

  TdxExpressionRichEditSuggestionList = class
    (TList<TdxExpressionRichEditSuggestion>);

  { TdxExpressionRichEditAutoCompleteWindowHintWindow }

  TdxExpressionRichEditAutoCompleteWindowHintWindow = class(THintWindow)
  public const
    MaxWidth = 150;
  strict private
    function InternalGetOwner: TdxExpressionRichEditAutoCompleteWindow; inline;
    function GetDrawTextFlags: Integer;
    function GetPainter: TcxCustomLookAndFeelPainter;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure NCPaint(DC: HDC); override;
    procedure Paint; override;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    property Owner: TdxExpressionRichEditAutoCompleteWindow
      read InternalGetOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  end;

  { TdxExpressionRichEditAutoCompleteWindow }

  TdxExpressionRichEditAutoCompleteWindow = class(TdxCustomAutoCompleteWindow)
  strict private
    FHintWindow: TdxExpressionRichEditAutoCompleteWindowHintWindow;

    function CreateHintWindow
      : TdxExpressionRichEditAutoCompleteWindowHintWindow;
    procedure InnerListBoxClickHandler(Sender: TObject);
    function GetHintText: string;
    function GetRichEdit: TdxExpressionRichEdit; inline;
  protected
    function CalculatePosition(const ASize: TSize): TPoint; override;
    function CalculateSize: TSize; override;
    function CreateInnerListBox: TdxCustomAutoCompleteInnerListBox; override;
    procedure DoHide; override;
    procedure DoShow; override;

    function HasSuggestions: Boolean;
    procedure UpdateHintWindow;
    procedure UpdateSuggestions;

    property RichEdit: TdxExpressionRichEdit read GetRichEdit;
    property HintWindow: TdxExpressionRichEditAutoCompleteWindowHintWindow
      read FHintWindow;
  public
    destructor Destroy; override;
  end;

  { TdxExpressionRichEditAutoCompleteWindowListBox }

  TdxExpressionRichEditAutoCompleteWindowListBox = class
    (TdxCustomAutoCompleteInnerListBox)
  strict private
    function InternalGetOwner: TdxExpressionRichEditAutoCompleteWindow; inline;
  protected
    function DoDrawItemImage(const R: TRect; AItem: TdxCustomListBoxItem;
      AState: TcxButtonState): Boolean;
    procedure DrawItemImage(const R: TRect; AItem: TdxCustomListBoxItem;
      AState: TcxButtonState); override;
    function IsSizeGripVisible: Boolean; override;
  public
    property Owner: TdxExpressionRichEditAutoCompleteWindow
      read InternalGetOwner;
  end;

  { TdxExpressionRichEdit }

  TdxExpressionRichEditGetSuggestionsEvent = procedure(Sender: TObject;
    AList: TdxExpressionRichEditSuggestionList) of object;
  TdxExpressionRichEditGetSuggestionHintEvent = procedure(Sender: TObject;
    AData: TObject; var AHintText: string) of object;
  TdxExpressionRichEditPostSuggestionEvent = procedure(Sender: TObject;
    const AText: string; AData: Pointer) of object;
  TdxExpressionRichEditDrawSuggestionImageEvent = procedure(Sender: TObject;
    ACanvas: TcxCanvas; const R: TRect; AItem: TdxCustomListBoxItem;
    AState: TcxButtonState; var AHandled: Boolean) of object;

  TdxExpressionRichEdit = class(TcxCustomRichEdit)
  strict private
    FAutoCompleteWindow: TdxExpressionRichEditAutoCompleteWindow;

    FOnGetSuggestions: TdxExpressionRichEditGetSuggestionsEvent;
    FOnGetSuggestionHint: TdxExpressionRichEditGetSuggestionHintEvent;
    FOnDrawSuggestionImage: TdxExpressionRichEditDrawSuggestionImageEvent;
    FOnPostSuggestion: TdxExpressionRichEditPostSuggestionEvent;

    procedure AutoCompleteWindowSelectItemHandler(Sender: TObject);
    function GetImages: TCustomImageList;
    function GetIsAutoCompleteWindowVisible: Boolean;
    function GetOnChange: TNotifyEvent;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure DXMContainerSetFocus(var Message: TMessage);
      message DXM_CONTAINERSETFOCUS;
  protected
    procedure Click; override;
    procedure DblClick; override;
    function GetInnerEditClass: TControlClass; override;
    procedure Initialize; override;

    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState)
      : Boolean; override;
    function CanKeyPressModifyEdit(Key: Char): Boolean; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    function ValidateKeyPress(var Key: Char): Boolean; override;

    function DoGetSuggestions: TdxExpressionRichEditSuggestionList;
    function DoGetSuggestionHint(AData: TObject): string;
    function DoDrawSuggestionImage(ACanvas: TcxCanvas; const R: TRect;
      AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean;
    function DoPostSuggestion: Boolean;

    function CalculateAutoCompleteWindowPosition(const ASize: TSize): TPoint;
    function CreateAutoCompleteWindow: TdxExpressionRichEditAutoCompleteWindow;
    function NeedShowAutoCompleteWindow(Key: Word; Shift: TShiftState): Boolean;
    procedure PopulateSuggestions(AInnerListBox
      : TdxCustomAutoCompleteInnerListBox);
    procedure UpdateSuggestionList;

    property AutoCompleteWindow: TdxExpressionRichEditAutoCompleteWindow
      read FAutoCompleteWindow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HideAutoCompleteWindow;
    procedure ShowAutoCompleteWindow;
    property IsAutoCompleteWindowVisible: Boolean
      read GetIsAutoCompleteWindowVisible;
  published
    property Images: TCustomImageList read GetImages write SetImages;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawSuggestionImage
      : TdxExpressionRichEditDrawSuggestionImageEvent
      read FOnDrawSuggestionImage write FOnDrawSuggestionImage;
    property OnGetSuggestions: TdxExpressionRichEditGetSuggestionsEvent
      read FOnGetSuggestions write FOnGetSuggestions;
    property OnGetSuggestionHint: TdxExpressionRichEditGetSuggestionHintEvent
      read FOnGetSuggestionHint write FOnGetSuggestionHint;
    property OnKeyPress;
    property OnPostSuggestion: TdxExpressionRichEditPostSuggestionEvent
      read FOnPostSuggestion write FOnPostSuggestion;
  end;

  { TdxExpressionRichInnerEdit }

  TdxExpressionRichInnerEdit = class(TcxRichInnerEdit)
  strict private
    function GetContainer: TdxExpressionRichEdit; inline;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  protected
    property Container: TdxExpressionRichEdit read GetContainer;
  end;

implementation

uses
  Math, Forms,
  cxGeometry, dxCharacters, dxTypeHelpers, cxRichEditUtils, cxDrawTextUtils,
  dxFormattedText;

type
  TdxCustomListBoxItemsAccess = class(TdxCustomListBoxItems);

  { TdxExpressionRichEditSuggestion }

constructor TdxExpressionRichEditSuggestion.Create(const ACaption: string;
  AImageIndex: Integer; AData: Pointer);
begin
  FCaption := ACaption;
  FImageIndex := AImageIndex;
  FData := AData;
end;

{ TdxExpressionRichEditAutoCompleteWindowHintWindow }

function TdxExpressionRichEditAutoCompleteWindowHintWindow.CalcHintRect
  (MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  AIndent: Integer;
  AFormattedText: TdxFormattedText;
begin
  Result := cxRect(0, 0, Min(MaxWidth, Self.MaxWidth), MaxInt);
  cxScreenCanvas.Font := Font;
  AFormattedText := TdxFormattedText.Create;
  try
    TdxFormattedTextConverters.Import(AFormattedText, AHint, Font);
    AFormattedText.CalculateLayout(cxScreenCanvas.Canvas, Font, Result,
      GetDrawTextFlags, Owner.ScaleFactor);
    Result := TRect.CreateSize(AFormattedText.TextSize);
  finally
    AFormattedText.Free;
  end;
  cxScreenCanvas.Dormant;
  AIndent := 2 * Owner.ScaleFactor.Apply(cxTextOffset);
  Inc(Result.Right, AIndent + 2);
  Inc(Result.Bottom, AIndent);
end;

function TdxExpressionRichEditAutoCompleteWindowHintWindow.
  GetDrawTextFlags: Integer;
begin
  Result := CXTO_WORDBREAK or CXTO_NOPREFIX;
  if IsRightToLeft then
    Result := Result or CXTO_RTLREADING;
end;

function TdxExpressionRichEditAutoCompleteWindowHintWindow.GetPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := Owner.Style.LookAndFeel.Painter;
end;

procedure TdxExpressionRichEditAutoCompleteWindowHintWindow.WMEraseBkgnd
  (var Message: TWmEraseBkgnd);
begin
  cxPaintCanvas.BeginPaint(Message.DC);
  try
    Painter.DrawHintBackground(cxPaintCanvas, ClientRect);
  finally
    cxPaintCanvas.EndPaint;
  end;
  Message.Result := 1;
end;

function TdxExpressionRichEditAutoCompleteWindowHintWindow.InternalGetOwner
  : TdxExpressionRichEditAutoCompleteWindow;
begin
  Result := TdxExpressionRichEditAutoCompleteWindow(inherited Owner);
end;

procedure TdxExpressionRichEditAutoCompleteWindowHintWindow.NCPaint(DC: HDC);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.FillRect(cxRectSetNullOrigin(BoundsRect),
      cxGetActualColor(Painter.GetHintBorderColor, clWindowFrame));
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxExpressionRichEditAutoCompleteWindowHintWindow.Paint;
var
  ATextRect: TRect;
  AFormattedText: TdxFormattedText;
begin
  cxPaintCanvas.BeginPaint(Canvas);
  try
    Painter.DrawHintBackground(cxPaintCanvas, ClientRect);

    cxPaintCanvas.Font := Font;
    cxPaintCanvas.Font.Color := Painter.ScreenTipGetTitleTextColor;
    cxPaintCanvas.Brush.Style := bsClear;

    AFormattedText := TdxFormattedText.Create;
    try
      TdxFormattedTextConverters.Import(AFormattedText, Text, Font);
      ATextRect := cxRectInflate(ClientRect,
        -Owner.ScaleFactor.Apply(cxTextOffset));
      AFormattedText.CalculateLayout(cxPaintCanvas.Canvas, cxPaintCanvas.Font,
        ATextRect, GetDrawTextFlags, Owner.ScaleFactor);
      AFormattedText.Draw(cxPaintCanvas.Canvas, ATextRect.TopLeft);
    finally
      AFormattedText.Free;
    end;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

{ TdxExpressionRichEditAutoCompleteWindow }

destructor TdxExpressionRichEditAutoCompleteWindow.Destroy;
begin
  FreeAndNil(FHintWindow);
  inherited Destroy;
end;

function TdxExpressionRichEditAutoCompleteWindow.CalculatePosition
  (const ASize: TSize): TPoint;
begin
  Result := RichEdit.CalculateAutoCompleteWindowPosition(ASize);
end;

function TdxExpressionRichEditAutoCompleteWindow.CalculateSize: TSize;
begin
  InnerListBox.LayoutChanged;
  Result := InnerListBox.CalculateContentSize(DisplayRowsCount);
  Result.Width := Result.Width + 2 * dxAutoCompleteWindowBorderSize;
  Result.Height := Result.Height + 2 * dxAutoCompleteWindowBorderSize
end;

function TdxExpressionRichEditAutoCompleteWindow.CreateInnerListBox
  : TdxCustomAutoCompleteInnerListBox;
begin
  Result := TdxExpressionRichEditAutoCompleteWindowListBox.Create(Self);
  Result.CircularKeyboardNavigation := False;
  Result.OnClick := InnerListBoxClickHandler;
end;

procedure TdxExpressionRichEditAutoCompleteWindow.UpdateHintWindow;
var
  AText: string;
  AHintSize: TSize;
  AMonitorRect: TRect;
  ARect: TRect;
  ARestSpace: Integer;
begin
  if TdxCustomListBoxItemsAccess(InnerListBox.Items).LockCount > 0 then
    Exit;
  AText := GetHintText;

  if AText <> '' then
  begin
    if FHintWindow = nil then
      FHintWindow := CreateHintWindow;

    ARect := cxRectInflate(BoundsRect, ScaleFactor.Apply(3), 0);
    AHintSize :=
      cxSize(FHintWindow.CalcHintRect
      (TdxExpressionRichEditAutoCompleteWindowHintWindow.MaxWidth, AText, nil));
    AMonitorRect := Monitor.BoundsRect;

    if IsRightToLeft then
    begin
      ARestSpace := ARect.Left - AMonitorRect.Left;
      if (AHintSize.cx > ARestSpace) and
        (ARestSpace < AMonitorRect.Right - ARect.Right) then
      begin
        AHintSize := cxSize(FHintWindow.CalcHintRect(AMonitorRect.Right -
          ARect.Right, AText, nil));
        ARect.Left := ARect.Right;
        ARect := cxRectSetSize(ARect, AHintSize);
      end
      else
      begin
        AHintSize := cxSize(FHintWindow.CalcHintRect(ARestSpace, AText, nil));
        ARect := cxRectSetRight(ARect, ARect.Left, AHintSize.cx);
        ARect := cxRectSetHeight(ARect, AHintSize.cy);
      end;
    end
    else
    begin
      ARestSpace := AMonitorRect.Right - ARect.Right;
      if (AHintSize.cx > ARestSpace) and
        (ARestSpace < ARect.Left - AMonitorRect.Left) then
      begin
        AHintSize :=
          cxSize(FHintWindow.CalcHintRect(ARect.Left - AMonitorRect.Left,
          AText, nil));
        ARect := cxRectSetRight(ARect, ARect.Left, AHintSize.cx);
        ARect := cxRectSetHeight(ARect, AHintSize.cy);
      end
      else
      begin
        AHintSize := cxSize(FHintWindow.CalcHintRect(ARestSpace, AText, nil));
        ARect.Left := ARect.Right;
        ARect := cxRectSetSize(ARect, AHintSize);
      end;
    end;

    FHintWindow.ActivateHint(ARect, AText);
  end
  else
    FreeAndNil(FHintWindow);
end;

procedure TdxExpressionRichEditAutoCompleteWindow.UpdateSuggestions;
var
  ASize: TSize;
  P: TPoint;
begin
  InnerListBox.BeginUpdate;
  try
    RichEdit.PopulateSuggestions(InnerListBox);
  finally
    InnerListBox.EndUpdate;
  end;
  if IsVisible then
  begin
    ASize := CalculateSize;
    P := BoundsRect.TopLeft;
    if IsRightToLeft then
      P.X := BoundsRect.Right - ASize.cx;
    CorrectBoundsWithDesktopWorkArea(P, ASize);
    SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
  end;
end;

procedure TdxExpressionRichEditAutoCompleteWindow.DoHide;
begin
  inherited DoHide;
  FreeAndNil(FHintWindow);
end;

procedure TdxExpressionRichEditAutoCompleteWindow.DoShow;
begin
  inherited DoShow;
  UpdateHintWindow;
end;

function TdxExpressionRichEditAutoCompleteWindow.CreateHintWindow
  : TdxExpressionRichEditAutoCompleteWindowHintWindow;
begin
  Result := TdxExpressionRichEditAutoCompleteWindowHintWindow.Create(Self);
  Result.BiDiMode := BiDiMode;
end;

procedure TdxExpressionRichEditAutoCompleteWindow.InnerListBoxClickHandler
  (Sender: TObject);
begin
  UpdateHintWindow;
end;

function TdxExpressionRichEditAutoCompleteWindow.GetHintText: string;
begin
  Result := '';
  if (InnerListBox.ItemIndex = -1) or
    (InnerListBox.Items[InnerListBox.ItemIndex].Data = nil) then
    Exit;
  Result := RichEdit.DoGetSuggestionHint
    (InnerListBox.Items[InnerListBox.ItemIndex].Data);
end;

function TdxExpressionRichEditAutoCompleteWindow.GetRichEdit
  : TdxExpressionRichEdit;
begin
  Result := TdxExpressionRichEdit(OwnerControl);
end;

function TdxExpressionRichEditAutoCompleteWindow.HasSuggestions: Boolean;
begin
  Result := InnerListBox.Count > 0;
end;

{ TdxExpressionRichEditAutoCompleteWindowListBox }

function TdxExpressionRichEditAutoCompleteWindowListBox.DoDrawItemImage
  (const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState)
  : Boolean;
begin
  Result := Owner.RichEdit.DoDrawSuggestionImage(Canvas, R, AItem, AState);
end;

procedure TdxExpressionRichEditAutoCompleteWindowListBox.DrawItemImage
  (const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  AImageRect: TRect;
begin
  if not DoDrawItemImage(R, AItem, AState) and
    IsImageAssigned(Images, AItem.ImageIndex) then
  begin
    AImageRect := cxRectCenter(R, ImageSize);
    cxDrawImage(Canvas.Handle, AImageRect, AImageRect, nil, Images,
      AItem.ImageIndex, idmNormal);
  end;
end;

function TdxExpressionRichEditAutoCompleteWindowListBox.InternalGetOwner
  : TdxExpressionRichEditAutoCompleteWindow;
begin
  Result := TdxExpressionRichEditAutoCompleteWindow(inherited Owner);
end;

function TdxExpressionRichEditAutoCompleteWindowListBox.
  IsSizeGripVisible: Boolean;
begin
  Result := False;
end;

{ TdxExpressionRichEdit }

constructor TdxExpressionRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoCompleteWindow := CreateAutoCompleteWindow;
end;

destructor TdxExpressionRichEdit.Destroy;
begin
  FreeAndNil(FAutoCompleteWindow);
  inherited Destroy;
end;

function TdxExpressionRichEdit.CanKeyDownModifyEdit(Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := inherited CanKeyDownModifyEdit(Key, Shift) and
    not NeedShowAutoCompleteWindow(Key, Shift);
end;

function TdxExpressionRichEdit.CanKeyPressModifyEdit(Key: Char): Boolean;
begin
  Result := not NeedShowAutoCompleteWindow(Ord(Key), KeyboardStateToShiftState)
    and inherited CanKeyPressModifyEdit(Key);
end;

procedure TdxExpressionRichEdit.DoEditKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if NeedShowAutoCompleteWindow(Key, Shift) then
    ShowAutoCompleteWindow;

  case Key of
    VK_UP, VK_DOWN:
      if AutoCompleteWindow.IsVisible then
      begin
        AutoCompleteWindow.ProcessNavigationKey(Key, Shift);
        Key := 0;
      end;
    VK_ESCAPE, VK_TAB, VK_LEFT, VK_RIGHT:
      HideAutoCompleteWindow;
    VK_RETURN:
      if (Shift = []) and DoPostSuggestion then
        Key := 0;
  end;

  if ([ssCtrl] = Shift) and dxCharInSet(Char(Key), ['1' .. '5', 'L', 'E', 'R',
    'J']) then
    Key := 0;
  inherited DoEditKeyDown(Key, Shift);
end;

function TdxExpressionRichEdit.ValidateKeyPress(var Key: Char): Boolean;
begin
  if NeedShowAutoCompleteWindow(Ord(Key), KeyboardStateToShiftState) then
    Key := #0;
  Result := inherited ValidateKeyPress(Key);
end;

function TdxExpressionRichEdit.DoGetSuggestions
  : TdxExpressionRichEditSuggestionList;
begin
  Result := TdxExpressionRichEditSuggestionList.Create;
  if Assigned(OnGetSuggestions) then
    OnGetSuggestions(Self, Result);
end;

function TdxExpressionRichEdit.DoGetSuggestionHint(AData: TObject): string;
begin
  Result := '';
  if Assigned(OnGetSuggestionHint) then
    OnGetSuggestionHint(Self, AData, Result);
end;

function TdxExpressionRichEdit.DoDrawSuggestionImage(ACanvas: TcxCanvas;
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState): Boolean;
begin
  Result := False;
  if Assigned(OnDrawSuggestionImage) then
    OnDrawSuggestionImage(Self, ACanvas, R, AItem, AState, Result);
end;

function TdxExpressionRichEdit.DoPostSuggestion: Boolean;
var
  AData: TObject;
  AText: string;
begin
  Result := AutoCompleteWindow.IsVisible;
  if not Result then
    Exit;
  AData := AutoCompleteWindow.SelectedObject;
  AText := AutoCompleteWindow.SelectedText;
  HideAutoCompleteWindow;
  if AData = nil then
    Exit;
  if Assigned(OnPostSuggestion) then
    OnPostSuggestion(Self, AText, AData);
end;

procedure TdxExpressionRichEdit.DXMContainerSetFocus(var Message: TMessage);
begin
  inherited;
  SetFocus;
end;

procedure TdxExpressionRichEdit.Click;
begin
  HideAutoCompleteWindow;
  inherited Click;
end;

procedure TdxExpressionRichEdit.DblClick;
begin
  HideAutoCompleteWindow;
  inherited DblClick;
end;

function TdxExpressionRichEdit.GetInnerEditClass: TControlClass;
begin
  Result := TdxExpressionRichInnerEdit;
end;

function TdxExpressionRichEdit.CalculateAutoCompleteWindowPosition
  (const ASize: TSize): TPoint;
begin
  Result := cxRichEditGetCharPosition(InnerRich.Handle, RichVersion, SelStart);
  Result.Y := Result.Y + cxTextExtent(SendMessage(InnerRich.Handle, WM_GETFONT,
    0, 0), dxMeasurePattern).cy;
  if IsRightToLeft then
    Result.X := Result.X - ASize.cx;
  Result := InnerRich.ClientToScreen(Result);
end;

function TdxExpressionRichEdit.CreateAutoCompleteWindow
  : TdxExpressionRichEditAutoCompleteWindow;
begin
  Result := TdxExpressionRichEditAutoCompleteWindow.Create(Self);
  Result.OnSelectItem := AutoCompleteWindowSelectItemHandler;
end;

procedure TdxExpressionRichEdit.HideAutoCompleteWindow;
begin
  FAutoCompleteWindow.ClosePopup;
  FAutoCompleteWindow.Visible := False;
end;

procedure TdxExpressionRichEdit.Initialize;
begin
  inherited Initialize;
  Properties.PlainText := True;
end;

function TdxExpressionRichEdit.NeedShowAutoCompleteWindow(Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := (Key = Ord(TdxCharacters.Space)) and (Shift = [ssCtrl]);
end;

procedure TdxExpressionRichEdit.PopulateSuggestions(AInnerListBox
  : TdxCustomAutoCompleteInnerListBox);
var
  AList: TList<TdxExpressionRichEditSuggestion>;
  I: Integer;
begin
  AInnerListBox.Clear;
  AList := DoGetSuggestions;
  try
    for I := 0 to AList.Count - 1 do
      AInnerListBox.AddItem(AList[I].Caption, AList[I].Data,
        AList[I].ImageIndex);
    if AList.Count > 0 then
      AInnerListBox.ItemIndex := 0;
  finally
    AList.Free;
  end;
end;

procedure TdxExpressionRichEdit.ShowAutoCompleteWindow;
begin
  FAutoCompleteWindow.UpdateSuggestions;
  if FAutoCompleteWindow.HasSuggestions then
    FAutoCompleteWindow.Popup(Self);
end;

procedure TdxExpressionRichEdit.UpdateSuggestionList;
begin
  if AutoCompleteWindow.Visible then
  begin
    AutoCompleteWindow.UpdateSuggestions;
    if not AutoCompleteWindow.HasSuggestions then
      HideAutoCompleteWindow;
  end;
end;

procedure TdxExpressionRichEdit.AutoCompleteWindowSelectItemHandler
  (Sender: TObject);
begin
  DoPostSuggestion;
end;

function TdxExpressionRichEdit.GetImages: TCustomImageList;
begin
  Result := AutoCompleteWindow.InnerListBox.Images;
end;

function TdxExpressionRichEdit.GetIsAutoCompleteWindowVisible: Boolean;
begin
  Result := AutoCompleteWindow.Visible;
end;

function TdxExpressionRichEdit.GetOnChange: TNotifyEvent;
begin
  Result := Properties.OnChange;
end;

procedure TdxExpressionRichEdit.SetImages(const Value: TCustomImageList);
begin
  AutoCompleteWindow.InnerListBox.Images := Value;
end;

procedure TdxExpressionRichEdit.SetOnChange(const Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

{ TdxExpressionRichInnerEdit }

function TdxExpressionRichInnerEdit.GetContainer: TdxExpressionRichEdit;
begin
  Result := TdxExpressionRichEdit(inherited Container);
end;

procedure TdxExpressionRichInnerEdit.WMChar(var Message: TWMChar);
begin
  inherited;
  if not(Message.CharCode in [0, VK_ESCAPE]) then
    Container.UpdateSuggestionList;
end;

end.
