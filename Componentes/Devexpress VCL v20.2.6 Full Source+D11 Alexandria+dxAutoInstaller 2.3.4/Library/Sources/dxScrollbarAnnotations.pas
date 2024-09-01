{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressCommonLibrary }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL }
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

unit dxScrollbarAnnotations;

{$I cxVer.inc}

interface

uses
  Windows, Forms, Messages, Types, SysUtils, Classes, Graphics,
  Generics.Defaults, Generics.Collections,
  dxCoreGraphics;

const
  dxFirstScrollbarAnnotationID = 0;
  dxFirstPredefinedScrollbarAnnotationID = 240;
  dxSelectedRowScrollbarAnnotationID = dxFirstPredefinedScrollbarAnnotationID;
  dxFocusedRowScrollbarAnnotationID = 241;
  dxSearchResultScrollbarAnnotationID = 242;
  dxErrorScrollbarAnnotationID = 243;
  dxLastScrollbarAnnotationID = 255;

type
  TdxScrollbarAnnotationAlignment = (saaNear, saaCenter, saaFar, saaClient);
  TdxScrollbarAnnotationKind = dxFirstScrollbarAnnotationID ..
    dxLastScrollbarAnnotationID;
  TdxScrollbarAnnotationKinds = set of TdxScrollbarAnnotationKind;

const
  dxAllScrollbarAnnotationKinds = [Low(TdxScrollbarAnnotationKind)
    .. High(TdxScrollbarAnnotationKind)];
  dxAllCustomScrollbarAnnotationKinds =
    [dxFirstScrollbarAnnotationID ..
    dxFirstPredefinedScrollbarAnnotationID - 1];

type
  TdxScrollbarAnnotations = class;
  TdxScrollbarAnnotationOptions = class;

  { IdxScrollbarAnnotationRenderer }

  IdxScrollbarAnnotationRenderer = interface // for internal use
    ['{0C20A750-FAFE-4A96-A0FB-DE73BCF98CF9}']
    procedure SetAnnotations(AAnnotations: TdxScrollbarAnnotations);
    procedure Invalidate(AAnnotationKinds
      : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
    procedure Update;
  end;

  TdxScrollbarAnnotationRowIndexList = class(TList<Integer>)
  public
{$IFNDEF DELPHIXE}
    function ToArray: TArray<Integer>;
{$ENDIF}
  end;

  TdxScrollbarAnnotationRowIndexLists = class
    (TObjectDictionary<TdxScrollbarAnnotationKind,
    TdxScrollbarAnnotationRowIndexList>);

  TdxPopulateCustomScrollbarAnnotationRowIndexList = procedure(Sender: TObject;
    AAnnotationIndex: Integer;
    ARowIndexList: TdxScrollbarAnnotationRowIndexList) of object;
  TdxGetScrollbarAnnotationHint = procedure(Sender: TObject;
    AAnnotationRowIndexLists: TdxScrollbarAnnotationRowIndexLists;
    var AHint: string) of object;

  TdxScrollbarAnnotationStyle = record
    Alignment: TdxScrollbarAnnotationAlignment;
    Color: TdxAlphaColor;
    MaxHeight: Integer;
    MinHeight: Integer;
    Offset: Integer;
    Width: Integer;
    function IsEqual(AStyle: TdxScrollbarAnnotationStyle): Boolean;
  end;

  TdxScrollbarAnnotations = class // for internal use
  strict private
    FChangedKinds: TdxScrollbarAnnotationKinds;
    FItemLists: TdxScrollbarAnnotationRowIndexLists;
    FLockCount: Integer;
    FRenderers: TList<IdxScrollbarAnnotationRenderer>;
    procedure Changed(AKinds: TdxScrollbarAnnotationKinds =
      dxAllScrollbarAnnotationKinds);
    function FindNearestRecordIndex(AKinds: TdxScrollbarAnnotationKinds;
      ACircular: Boolean; AGoForward: Boolean; out AIndex: Integer): Boolean;
    procedure SortChangedLists;
    procedure UpdateRenderers;
  protected
    procedure Add(AKind: TdxScrollbarAnnotationKind; ARecordIndex: Integer);
    procedure AddRange(AKind: TdxScrollbarAnnotationKind;
      ARecordIndices: TdxScrollbarAnnotationRowIndexList);
    procedure BeginUpdate;
    procedure Clear(AKind: TdxScrollbarAnnotationKind);
    procedure EndUpdate;
    function GetCurrentRecordIndex: Integer; virtual;
    function GetFirstRecordIndex: Integer;
    function GetLastRecordIndex: Integer;
    function GetOptions: TdxScrollbarAnnotationOptions; virtual;
    function GetDataPixelScrollSize: Integer; virtual;
    function GetRecordIndexByScrollableRecordIndex(AIndex: Integer)
      : Integer; virtual;
    function IsVisible(AKind: TdxScrollbarAnnotationKind): Boolean; virtual;
    procedure Populate(AKind: TdxScrollbarAnnotationKind;
      AList: TdxScrollbarAnnotationRowIndexList); virtual;
    procedure Refresh(AChangedAnnotationKinds
      : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds); virtual;
    procedure SetCurrentRecordIndex(AIndex: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRenderer(ARenderer: IdxScrollbarAnnotationRenderer);
    procedure Click(AAnnotationRowIndexLists
      : TdxScrollbarAnnotationRowIndexLists; var AHandled: Boolean); virtual;
    function GetDataScrollSize: Integer;
    procedure GetHint(AAnnotationRowIndexLists
      : TdxScrollbarAnnotationRowIndexLists; var AHint: string); virtual;
    function GetLastIndexInScrollBand(ARecordIndex: Integer): Integer; virtual;
    function GetRecordPixelScrollPosition(ARecordIndex: Integer)
      : Integer; virtual;
    function GetScrollableRecordCount: Integer; virtual;
    function GetScrollableRecordIndexByRecordIndex(AIndex: Integer)
      : Integer; virtual;
    function GetStyle(AKind: TdxScrollbarAnnotationKind)
      : TdxScrollbarAnnotationStyle; virtual;
    procedure InvalidateRenderers(AAnnotationKinds
      : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
    function IsRecordIndexBasedRendering: Boolean; virtual;
    function GoToNext(AKinds: TdxScrollbarAnnotationKinds =
      dxAllScrollbarAnnotationKinds; AGoForward: Boolean = True;
      AGoOnCycle: Boolean = False): Boolean;
    procedure RemoveRenderer(ARenderer: IdxScrollbarAnnotationRenderer);

    property ChangedKinds: TdxScrollbarAnnotationKinds read FChangedKinds;
    property CurrentRecordIndex: Integer read GetCurrentRecordIndex
      write SetCurrentRecordIndex;
    property ItemLists: TdxScrollbarAnnotationRowIndexLists read FItemLists;
  end;

  TdxCustomScrollbarAnnotation = class(TCollectionItem)
  strict private
    FStyle: TdxScrollbarAnnotationStyle;
    FVisible: Boolean;
    function GetAlignment: TdxScrollbarAnnotationAlignment;
    function GetColor: TdxAlphaColor;
    function GetMaxHeight: Integer;
    function GetMinHeight: Integer;
    function GetOffset: Integer;
    function GetWidth: Integer;
    function IsAlignmentStored: Boolean;
    function IsMaxHeightStored: Boolean;
    function IsMinHeightStored: Boolean;
    function IsOffsetStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetAlignment(const Value: TdxScrollbarAnnotationAlignment);
    procedure SetColor(const Value: TdxAlphaColor);
    procedure SetMaxHeight(Value: Integer);
    procedure SetMinHeight(Value: Integer);
    procedure SetOffset(Value: Integer);
    procedure SetStyle(const Value: TdxScrollbarAnnotationStyle);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Style: TdxScrollbarAnnotationStyle read FStyle write SetStyle;
  published
    property Alignment: TdxScrollbarAnnotationAlignment read GetAlignment
      write SetAlignment stored IsAlignmentStored;
    property Color: TdxAlphaColor read GetColor write SetColor
      default dxacDefault;
    property MaxHeight: Integer read GetMaxHeight write SetMaxHeight
      stored IsMaxHeightStored;
    property MinHeight: Integer read GetMinHeight write SetMinHeight
      stored IsMinHeightStored;
    property Offset: Integer read GetOffset write SetOffset
      stored IsOffsetStored;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
  end;

  TdxCustomScrollbarAnnotations = class(TCollection)
  strict private
    FOwner: TdxScrollbarAnnotationOptions;
  private
    function GetItem(Index: Integer): TdxCustomScrollbarAnnotation;
    procedure SetItem(Index: Integer;
      const Value: TdxCustomScrollbarAnnotation);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TdxScrollbarAnnotationOptions); virtual;
    function Add: TdxCustomScrollbarAnnotation;
    property Items[Index: Integer]: TdxCustomScrollbarAnnotation read GetItem
      write SetItem; default;
  end;

  TdxScrollbarAnnotationOptions = class(TPersistent) // for internal use
  strict private
    FStates: array [0 .. 4] of Boolean;
    FCustomScrollbarAnnotations: TdxCustomScrollbarAnnotations;
    procedure SetCustomScrollbarAnnotations(const Value
      : TdxCustomScrollbarAnnotations);
    function GetCustomScrollbarAnnotations: TdxCustomScrollbarAnnotations;
  protected
    procedure Changed; virtual;
    procedure CheckScrollbarAnnotations; virtual;
    function GetDefaultValue(Index: Integer): Boolean; virtual;
    function GetValue(Index: Integer): Boolean;
    function GetStyle(AKind: TdxScrollbarAnnotationKind)
      : TdxScrollbarAnnotationStyle; virtual;
    function IsVisible(AKind: Integer): Boolean; virtual;
    procedure RefreshScrollbarAnnotations(AChangedAnnotationKinds
      : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds); virtual;
    procedure SetValue(Index: Integer; Value: Boolean); virtual;

    property Active: Boolean index 0 read GetValue write SetValue;
    property CustomAnnotations: TdxCustomScrollbarAnnotations
      read GetCustomScrollbarAnnotations write SetCustomScrollbarAnnotations;
    property ShowErrors: Boolean index 4 read GetValue write SetValue;
    property ShowSearchResults: Boolean index 3 read GetValue write SetValue;
    property ShowFocusedRow: Boolean index 2 read GetValue write SetValue;
    property ShowSelectedRows: Boolean index 1 read GetValue write SetValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

var
  dxSelectedRowScrollbarAnnotationStyle: TdxScrollbarAnnotationStyle =
    (Alignment: saaFar; Color: dxacDefault; MaxHeight: 0; MinHeight: 2;
    Offset: 0; Width: 4;);

  dxFocusedRowScrollbarAnnotationStyle: TdxScrollbarAnnotationStyle =
    (Alignment: saaCenter; Color: dxacDefault; MaxHeight: 2; MinHeight: 2;
    Offset: 0; Width: 9;);

  dxSearchResultScrollbarAnnotationStyle: TdxScrollbarAnnotationStyle =
    (Alignment: saaCenter; Color: dxacDefault; MaxHeight: 2; MinHeight: 2;
    Offset: 0; Width: 11;);

  dxErrorScrollbarAnnotationStyle: TdxScrollbarAnnotationStyle =
    (Alignment: saaCenter; Color: dxacDefault; MaxHeight: 4; MinHeight: 4;
    Offset: 0; Width: 3;);

  dxCustomScrollbarAnnotationStyle: TdxScrollbarAnnotationStyle =
    (Alignment: saaNear; Color: dxacDefault; MaxHeight: 0; MinHeight: 2;
    Offset: 0; Width: 4;);

implementation

uses
  Math;

function dxGetDefaultScrollbarAnnotationStyle(AKind: TdxScrollbarAnnotationKind)
  : TdxScrollbarAnnotationStyle;
begin
  case AKind of
    dxSelectedRowScrollbarAnnotationID:
      Result := dxSelectedRowScrollbarAnnotationStyle;
    dxFocusedRowScrollbarAnnotationID:
      Result := dxFocusedRowScrollbarAnnotationStyle;
    dxSearchResultScrollbarAnnotationID:
      Result := dxSearchResultScrollbarAnnotationStyle;
    dxErrorScrollbarAnnotationID:
      Result := dxErrorScrollbarAnnotationStyle;
  else // Custom
    Result := dxCustomScrollbarAnnotationStyle;
  end;
end;

{ TdxScrollbarAnnotationRowIndexList }

{$IFNDEF DELPHIXE}

function TdxScrollbarAnnotationRowIndexList.ToArray: TArray<Integer>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Items[I];
end;
{$ENDIF}
{ TdxScrollbarAnnotationStyle }

function TdxScrollbarAnnotationStyle.IsEqual
  (AStyle: TdxScrollbarAnnotationStyle): Boolean;
begin
  Result := (Alignment = AStyle.Alignment) and (Color = AStyle.Color) and
    (MaxHeight = AStyle.MaxHeight) and (MinHeight = AStyle.MinHeight) and
    (Offset = AStyle.Offset) and (Width = AStyle.Width);
end;

{ TdxScrollbarAnnotations }

constructor TdxScrollbarAnnotations.Create;
begin
  inherited Create;
  FItemLists := TdxScrollbarAnnotationRowIndexLists.Create([doOwnsValues]);
  FRenderers := TList<IdxScrollbarAnnotationRenderer>.Create;
end;

destructor TdxScrollbarAnnotations.Destroy;
var
  I: Integer;
begin
  for I := FRenderers.Count - 1 downto 0 do
    FRenderers[I].SetAnnotations(nil);
  FreeAndNil(FRenderers);
  FreeAndNil(FItemLists);
  inherited Destroy;
end;

procedure TdxScrollbarAnnotations.AddRenderer
  (ARenderer: IdxScrollbarAnnotationRenderer);
begin
  if ARenderer <> nil then
    FRenderers.Add(ARenderer);
end;

procedure TdxScrollbarAnnotations.Click(AAnnotationRowIndexLists
  : TdxScrollbarAnnotationRowIndexLists; var AHandled: Boolean);
begin
end;

procedure TdxScrollbarAnnotations.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and (FChangedKinds <> []) then
  begin
    SortChangedLists;
    UpdateRenderers;
  end;
end;

function TdxScrollbarAnnotations.GetDataScrollSize: Integer;
begin
  if IsRecordIndexBasedRendering then
    Result := GetScrollableRecordCount
  else
    Result := GetDataPixelScrollSize;
end;

procedure TdxScrollbarAnnotations.GetHint(AAnnotationRowIndexLists
  : TdxScrollbarAnnotationRowIndexLists; var AHint: string);
begin
end;

function TdxScrollbarAnnotations.GetLastIndexInScrollBand
  (ARecordIndex: Integer): Integer;
begin
  Result := ARecordIndex;
end;

function TdxScrollbarAnnotations.GetRecordPixelScrollPosition
  (ARecordIndex: Integer): Integer;
begin
  Result := 0;
end;

function TdxScrollbarAnnotations.GetScrollableRecordCount: Integer;
begin
  Result := 0;
end;

function TdxScrollbarAnnotations.GetScrollableRecordIndexByRecordIndex
  (AIndex: Integer): Integer;
begin
  Result := AIndex;
end;

function TdxScrollbarAnnotations.GetStyle(AKind: TdxScrollbarAnnotationKind)
  : TdxScrollbarAnnotationStyle;
begin
  Result := GetOptions.GetStyle(AKind);
end;

procedure TdxScrollbarAnnotations.InvalidateRenderers(AAnnotationKinds
  : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
var
  I: Integer;
begin
  for I := 0 to FRenderers.Count - 1 do
    FRenderers[I].Invalidate(AAnnotationKinds);
end;

function TdxScrollbarAnnotations.IsRecordIndexBasedRendering: Boolean;
begin
  Result := True;
end;

function TdxScrollbarAnnotations.GoToNext
  (AKinds: TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds;
  AGoForward: Boolean = True; AGoOnCycle: Boolean = False): Boolean;
var
  ANextAnnotationRowIndex: Integer;
begin
  Result := FindNearestRecordIndex(AKinds, AGoOnCycle, AGoForward,
    ANextAnnotationRowIndex);
  if Result then
    CurrentRecordIndex := ANextAnnotationRowIndex;
end;

procedure TdxScrollbarAnnotations.RemoveRenderer
  (ARenderer: IdxScrollbarAnnotationRenderer);
begin
  FRenderers.Remove(ARenderer);
end;

procedure TdxScrollbarAnnotations.Add(AKind: TdxScrollbarAnnotationKind;
  ARecordIndex: Integer);
var
  AItems: TdxScrollbarAnnotationRowIndexList;
begin
  if not InRange(ARecordIndex, GetFirstRecordIndex, GetLastRecordIndex) then
    Exit;
  if not FItemLists.TryGetValue(AKind, AItems) then
  begin
    AItems := TdxScrollbarAnnotationRowIndexList.Create;
    FItemLists.Add(AKind, AItems);
  end;
  AItems.Add(ARecordIndex);
  Changed([AKind]);
end;

procedure TdxScrollbarAnnotations.AddRange(AKind: TdxScrollbarAnnotationKind;
  ARecordIndices: TdxScrollbarAnnotationRowIndexList);
var
  I: Integer;
begin
  for I := 0 to ARecordIndices.Count - 1 do
    Add(AKind, ARecordIndices[I]);
end;

procedure TdxScrollbarAnnotations.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxScrollbarAnnotations.Clear(AKind: TdxScrollbarAnnotationKind);
var
  AItems: TdxScrollbarAnnotationRowIndexList;
begin
  if FItemLists.TryGetValue(AKind, AItems) then
  begin
    AItems.Clear;
    Changed([AKind]);
  end;
end;

function TdxScrollbarAnnotations.GetCurrentRecordIndex: Integer;
begin
  Result := -1;
end;

function TdxScrollbarAnnotations.GetFirstRecordIndex: Integer;
begin
  Result := GetRecordIndexByScrollableRecordIndex(0);
end;

function TdxScrollbarAnnotations.GetLastRecordIndex: Integer;
begin
  Result := GetRecordIndexByScrollableRecordIndex(GetScrollableRecordCount - 1);
end;

function TdxScrollbarAnnotations.GetOptions: TdxScrollbarAnnotationOptions;
begin
  Result := nil;
end;

function TdxScrollbarAnnotations.GetDataPixelScrollSize: Integer;
begin
  Result := 0;
end;

function TdxScrollbarAnnotations.GetRecordIndexByScrollableRecordIndex
  (AIndex: Integer): Integer;
begin
  Result := AIndex;
end;

function TdxScrollbarAnnotations.IsVisible
  (AKind: TdxScrollbarAnnotationKind): Boolean;
begin
  Result := GetOptions.IsVisible(AKind);
end;

procedure TdxScrollbarAnnotations.Populate(AKind: TdxScrollbarAnnotationKind;
  AList: TdxScrollbarAnnotationRowIndexList);
begin
end;

procedure TdxScrollbarAnnotations.Changed
  (AKinds: TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
begin
  FChangedKinds := FChangedKinds + AKinds;
  if FLockCount = 0 then
  begin
    SortChangedLists;
    UpdateRenderers;
  end;
end;

function TdxScrollbarAnnotations.FindNearestRecordIndex
  (AKinds: TdxScrollbarAnnotationKinds; ACircular: Boolean; AGoForward: Boolean;
  out AIndex: Integer): Boolean;
var
  AKind: TdxScrollbarAnnotationKind;
  AList: TdxScrollbarAnnotationRowIndexList;
  AItemIndex: Integer;
  AFoundAnnotationRowIndex: Integer;
  ANextRowIndex: Integer;
begin
  AIndex := -1;
  if (AKinds <> []) and (ACircular or AGoForward and
    (CurrentRecordIndex < GetLastRecordIndex) or not AGoForward and
    (CurrentRecordIndex > GetFirstRecordIndex)) then
  begin
    if AGoForward then
      if CurrentRecordIndex < GetLastRecordIndex then
        ANextRowIndex := CurrentRecordIndex + 1
      else
        ANextRowIndex := GetFirstRecordIndex
    else if CurrentRecordIndex > GetFirstRecordIndex then
      ANextRowIndex := CurrentRecordIndex - 1
    else
      ANextRowIndex := GetLastRecordIndex;

    for AKind in FItemLists.Keys do
      if AKind in AKinds then
      begin
        AList := FItemLists[AKind];
        if AList.Count = 0 then
          Continue;
        if AList.BinarySearch(ANextRowIndex, AItemIndex) then
        begin
          AIndex := AList[AItemIndex];
          Break;
        end
        else
        begin
          if ACircular or AGoForward and (AItemIndex <= AList.Count - 1) or
            not AGoForward and (AItemIndex > 0) then
          begin
            if AGoForward then
              if AItemIndex > AList.Count - 1 then
                AFoundAnnotationRowIndex := AList.First
              else
                AFoundAnnotationRowIndex := AList[AItemIndex]
            else if AItemIndex <= 0 then
              AFoundAnnotationRowIndex := AList.Last
            else
              AFoundAnnotationRowIndex := AList[AItemIndex - 1];
            if AIndex = -1 then
              AIndex := AFoundAnnotationRowIndex
            else if (((AFoundAnnotationRowIndex > ANextRowIndex) and
              (AIndex < ANextRowIndex)) or
              ((AFoundAnnotationRowIndex < ANextRowIndex) and
              (AIndex > ANextRowIndex))) xor AGoForward then
              AIndex := Min(AIndex, AFoundAnnotationRowIndex)
            else
              AIndex := Max(AIndex, AFoundAnnotationRowIndex);
          end;
        end;
      end;
  end;
  Result := AIndex <> -1;
end;

procedure TdxScrollbarAnnotations.Refresh(AChangedAnnotationKinds
  : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
var
  AList: TdxScrollbarAnnotationRowIndexList;
  AKind: Integer;
begin
  BeginUpdate;
  try
    AList := TdxScrollbarAnnotationRowIndexList.Create;
    try
      for AKind := dxFirstScrollbarAnnotationID to
        dxLastScrollbarAnnotationID do
      begin
        if AKind in AChangedAnnotationKinds then
        begin
          Clear(AKind);
          if IsVisible(AKind) then
          begin
            Populate(AKind, AList);
            AddRange(AKind, AList);
            AList.Clear;
          end;
        end;
      end;
    finally
      AList.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxScrollbarAnnotations.SetCurrentRecordIndex(AIndex: Integer);
begin
end;

procedure TdxScrollbarAnnotations.SortChangedLists;
var
  AKind: TdxScrollbarAnnotationKind;
begin
  for AKind in FItemLists.Keys do
    if AKind in ChangedKinds then
      FItemLists[AKind].Sort;
end;

procedure TdxScrollbarAnnotations.UpdateRenderers;
var
  I: Integer;
begin
  for I := 0 to FRenderers.Count - 1 do
    FRenderers[I].Update;
  FChangedKinds := [];
end;

{ TdxCustomScrollbarAnnotation }

constructor TdxCustomScrollbarAnnotation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStyle := dxGetDefaultScrollbarAnnotationStyle(dxFirstScrollbarAnnotationID);
  FVisible := True;
end;

procedure TdxCustomScrollbarAnnotation.Assign(Source: TPersistent);
begin
  if Source is TdxCustomScrollbarAnnotation then
  begin
    Style := TdxCustomScrollbarAnnotation(Source).Style;
    Visible := TdxCustomScrollbarAnnotation(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

function TdxCustomScrollbarAnnotation.GetAlignment
  : TdxScrollbarAnnotationAlignment;
begin
  Result := FStyle.Alignment;
end;

function TdxCustomScrollbarAnnotation.GetColor: TdxAlphaColor;
begin
  Result := FStyle.Color;
end;

function TdxCustomScrollbarAnnotation.GetMaxHeight: Integer;
begin
  Result := FStyle.MaxHeight;
end;

function TdxCustomScrollbarAnnotation.GetMinHeight: Integer;
begin
  Result := FStyle.MinHeight;
end;

function TdxCustomScrollbarAnnotation.GetOffset: Integer;
begin
  Result := FStyle.Offset;
end;

function TdxCustomScrollbarAnnotation.GetWidth: Integer;
begin
  Result := FStyle.Width;
end;

function TdxCustomScrollbarAnnotation.IsAlignmentStored: Boolean;
begin
  Result := Alignment <> dxGetDefaultScrollbarAnnotationStyle
    (dxFirstScrollbarAnnotationID).Alignment;
end;

function TdxCustomScrollbarAnnotation.IsMaxHeightStored: Boolean;
begin
  Result := MaxHeight <> dxGetDefaultScrollbarAnnotationStyle
    (dxFirstScrollbarAnnotationID).MaxHeight;
end;

function TdxCustomScrollbarAnnotation.IsMinHeightStored: Boolean;
begin
  Result := MinHeight <> dxGetDefaultScrollbarAnnotationStyle
    (dxFirstScrollbarAnnotationID).MinHeight;
end;

function TdxCustomScrollbarAnnotation.IsOffsetStored: Boolean;
begin
  Result := Offset <> dxGetDefaultScrollbarAnnotationStyle
    (dxFirstScrollbarAnnotationID).Offset;
end;

function TdxCustomScrollbarAnnotation.IsWidthStored: Boolean;
begin
  Result := Width <> dxGetDefaultScrollbarAnnotationStyle
    (dxFirstScrollbarAnnotationID).Width;
end;

procedure TdxCustomScrollbarAnnotation.SetAlignment
  (const Value: TdxScrollbarAnnotationAlignment);
begin
  if Alignment <> Value then
  begin
    FStyle.Alignment := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetColor(const Value: TdxAlphaColor);
begin
  if Color <> Value then
  begin
    FStyle.Color := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetMaxHeight(Value: Integer);
begin
  Value := Max(0, Value);
  if MaxHeight <> Value then
  begin
    FStyle.MaxHeight := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetMinHeight(Value: Integer);
begin
  Value := Max(0, Value);
  if MinHeight <> Value then
  begin
    FStyle.MinHeight := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetOffset(Value: Integer);
begin
  if Offset <> Value then
  begin
    FStyle.Offset := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetStyle(const Value
  : TdxScrollbarAnnotationStyle);
begin
  if not FStyle.IsEqual(Value) then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TdxCustomScrollbarAnnotation.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if Width <> Value then
  begin
    FStyle.Width := Value;
    Changed(False);
  end;
end;

{ TdxCustomScrollbarAnnotations }

constructor TdxCustomScrollbarAnnotations.Create
  (AOwner: TdxScrollbarAnnotationOptions);
begin
  inherited Create(TdxCustomScrollbarAnnotation);
  FOwner := AOwner;
end;

function TdxCustomScrollbarAnnotations.Add: TdxCustomScrollbarAnnotation;
begin
  Result := TdxCustomScrollbarAnnotation(inherited Add);
end;

function TdxCustomScrollbarAnnotations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TdxCustomScrollbarAnnotations.Update(Item: TCollectionItem);
var
  AChangedKinds: TdxScrollbarAnnotationKinds;
begin
  if Item <> nil then
    AChangedKinds := [Item.Index]
  else
    AChangedKinds := dxAllCustomScrollbarAnnotationKinds;
  FOwner.RefreshScrollbarAnnotations(AChangedKinds);
end;

function TdxCustomScrollbarAnnotations.GetItem(Index: Integer)
  : TdxCustomScrollbarAnnotation;
begin
  Result := TdxCustomScrollbarAnnotation(inherited GetItem(Index));
end;

procedure TdxCustomScrollbarAnnotations.SetItem(Index: Integer;
  const Value: TdxCustomScrollbarAnnotation);
begin
  inherited SetItem(Index, Value);
end;

{ TdxScrollbarAnnotationOptions }

constructor TdxScrollbarAnnotationOptions.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := Low(FStates) to High(FStates) do
    FStates[I] := GetDefaultValue(I);
  FCustomScrollbarAnnotations := TdxCustomScrollbarAnnotations.Create(Self);
end;

destructor TdxScrollbarAnnotationOptions.Destroy;
begin
  FreeAndNil(FCustomScrollbarAnnotations);
  inherited Destroy;
end;

procedure TdxScrollbarAnnotationOptions.Assign(Source: TPersistent);
var
  AOptions: TdxScrollbarAnnotationOptions;
  I: Integer;
begin
  if Source is TdxScrollbarAnnotationOptions then
  begin
    AOptions := TdxScrollbarAnnotationOptions(Source);
    for I := Low(FStates) to High(FStates) do
      SetValue(I, AOptions.FStates[I]);
    CustomAnnotations := AOptions.CustomAnnotations;
  end
  else
    inherited;
end;

function TdxScrollbarAnnotationOptions.GetCustomScrollbarAnnotations
  : TdxCustomScrollbarAnnotations;
begin
  Result := FCustomScrollbarAnnotations;
end;

function TdxScrollbarAnnotationOptions.GetValue(Index: Integer): Boolean;
begin
  Result := FStates[Index];
end;

function TdxScrollbarAnnotationOptions.GetStyle
  (AKind: TdxScrollbarAnnotationKind): TdxScrollbarAnnotationStyle;
begin
  if AKind in dxAllCustomScrollbarAnnotationKinds then
    Result := CustomAnnotations[AKind].Style
  else
    Result := dxGetDefaultScrollbarAnnotationStyle(AKind);
end;

function TdxScrollbarAnnotationOptions.IsVisible(AKind: Integer): Boolean;
begin
  case AKind of
    dxFirstScrollbarAnnotationID .. dxFirstPredefinedScrollbarAnnotationID - 1:
      Result := (AKind < CustomAnnotations.Count) and CustomAnnotations
        [AKind].Visible;
    dxFirstPredefinedScrollbarAnnotationID .. dxErrorScrollbarAnnotationID:
      Result := FStates[AKind - dxFirstPredefinedScrollbarAnnotationID + 1]
  else
    Result := False;
  end;
end;

procedure TdxScrollbarAnnotationOptions.Changed;
begin
end;

procedure TdxScrollbarAnnotationOptions.CheckScrollbarAnnotations;
begin
end;

function TdxScrollbarAnnotationOptions.GetDefaultValue(Index: Integer): Boolean;
begin
  Result := Index > 0;
end;

procedure TdxScrollbarAnnotationOptions.RefreshScrollbarAnnotations
  (AChangedAnnotationKinds: TdxScrollbarAnnotationKinds =
  dxAllScrollbarAnnotationKinds);
begin
end;

procedure TdxScrollbarAnnotationOptions.SetCustomScrollbarAnnotations
  (const Value: TdxCustomScrollbarAnnotations);
begin
  FCustomScrollbarAnnotations.Assign(Value);
end;

procedure TdxScrollbarAnnotationOptions.SetValue(Index: Integer;
  Value: Boolean);
var
  ARefreshAnnotationKinds: TdxScrollbarAnnotationKinds;
begin
  if FStates[Index] <> Value then
  begin
    FStates[Index] := Value;
    if Index = 0 then
    begin
      CheckScrollbarAnnotations;
      ARefreshAnnotationKinds := dxAllScrollbarAnnotationKinds;
    end
    else
      ARefreshAnnotationKinds := [dxFirstPredefinedScrollbarAnnotationID -
        1 + Index];
    RefreshScrollbarAnnotations(ARefreshAnnotationKinds);
    Changed;
  end;
end;

end.
