{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2022                                      }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvResponsiveManager;

{$I TMSDEFS.INC}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

//{$DEFINE NOPP}

interface

uses
  Classes, AdvStateManager, AdvCustomControl, Types, AdvTypes
  {$IFNDEF LCLLIB}
  ,Generics.Defaults, Generics.Collections
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,fgl
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release

type
  TAdvCustomResponsiveManager = class;

  TAdvResponsiveManagerConstraintMode = (cmSize, cmString, cmBoolean, cmNumber);

  TAdvResponsiveManagerConstraint = class(TPersistent)
  private
    FWidth: Single;
    FHeight: Single;
    FStringValue: string;
    FMode: TAdvResponsiveManagerConstraintMode;
    FBooleanValue: Boolean;
    FNumberValue: Extended;
    function IsHeightStored: Boolean;
    function IsWidthStored: Boolean;
    function IsNumberValueStored: Boolean;
  protected
    property Mode: TAdvResponsiveManagerConstraintMode read FMode write FMode default cmSize;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: Single read FWidth write FWidth stored IsWidthStored nodefault;
    property Height: Single read FHeight write FHeight stored IsHeightStored nodefault;
    property StringValue: string read FStringValue write FStringValue;
    property NumberValue: Extended read FNumberValue write FNumberValue stored IsNumberValueStored nodefault;
    property BooleanValue: Boolean read FBooleanValue write FBooleanValue default False;
  end;

  TAdvResponsiveManagerSizeConstraint = record
    Width, Height: Single;
  end;

  TAdvResponsiveManagerItem = class(TAdvStateManagerItem)
  private
    FConstraint: TAdvResponsiveManagerConstraint;
    procedure SetConstraint(const Value: TAdvResponsiveManagerConstraint);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Constraint: TAdvResponsiveManagerConstraint read FConstraint write SetConstraint;
  end;

  TAdvResponsiveManagerItems = class(TAdvStateManagerItems)
  private
    function GetItemEx(Index: Integer): TAdvResponsiveManagerItem;
    procedure SetItemEx(Index: Integer; const Value: TAdvResponsiveManagerItem);
  protected
    function GetStateManagerStateClass: TCollectionItemClass; override;
  public
    function Add: TAdvResponsiveManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    function Insert(index: Integer): TAdvResponsiveManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    property Items[Index: Integer]: TAdvResponsiveManagerItem read GetItemEx write SetItemEx; default;
  end;

  TAdvResponsiveManagerResizeMode = (mrmWidthOnly, mrmHeightOnly, mrmWidthFirst, mrmHeightFirst);

  {$IFNDEF LCLWEBLIB}
  TAdvResponsiveManagerItemSizeComparer = class(TComparer<TAdvResponsiveManagerItem>)
  private
    FManager: TAdvCustomResponsiveManager;
  public
    function Compare(const Left, Right: TAdvResponsiveManagerItem): Integer; override;
  end;
  {$ENDIF}

  TAdvResponsiveManagerItemList = TList<TAdvResponsiveManagerItem>;

  TAdvResponsiveManagerPaintBox = class(TAdvCustomControl)
  private
    FManager: TAdvCustomResponsiveManager;
  public
    procedure Paint; override;
  end;

  TAdvCustomResponsiveManager = class(TAdvCustomStateManager)
  private
    FPaintBox: TAdvResponsiveManagerPaintBox;
    FPreviewManager:  TAdvCustomResponsiveManager;
    FOldResize: TNotifyEvent;
    FAutoLoadOnResize: Boolean;
    FBlockLoadConstraints: Boolean;
    FMode: TAdvResponsiveManagerResizeMode;
    function GetStates: TAdvResponsiveManagerItems;
    procedure SetStates(const Value: TAdvResponsiveManagerItems);
  protected
    function GetDocURL: string; override;
    function GetInstance: NativeUInt; override;
    function GetVersion: string; override;
    function GetSizeConstraint: TAdvResponsiveManagerSizeConstraint;
    function InternalFindState(AConstraint: TAdvResponsiveManagerConstraint): TAdvResponsiveManagerItem; virtual;
    function CreateStatesCollection: TAdvStateManagerItems; override;
    procedure InternalSetActiveState(AState: TAdvStateManagerItem); override;
    procedure SetConstraint(AState: TAdvResponsiveManagerItem); virtual;
    procedure InternalSaveToState(AState: TAdvStateManagerItem; ANew: Boolean); override;
    procedure BeforeAssignControl; override;
    procedure AfterAssignControl; override;
    procedure BeforeLoadState(AState: TAdvStateManagerItem); override;
    procedure DoResponsiveResize(Sender: TObject);
    procedure DoPreviewResize(Sender: TObject);
    procedure Recreate; virtual;

    property Mode: TAdvResponsiveManagerResizeMode read FMode write FMode default mrmWidthOnly;
    property BlockLoadConstraints: Boolean read FBlockLoadConstraints write FBlockLoadConstraints;
    property States: TAdvResponsiveManagerItems read GetStates write SetStates;
    property Version: string read GetVersion;
    property AutoLoadOnResize: Boolean read FAutoLoadOnResize write FAutoLoadOnResize default True;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); overload; override;

    procedure LoadState(AStringValue: string); overload; virtual;
    procedure LoadState(ABooleanValue: Boolean); overload; virtual;
    procedure LoadState(ANumberValue: Extended); overload; virtual;
    procedure LoadState; overload; virtual;

    function SaveToNewState: TAdvResponsiveManagerItem; overload; virtual;
    function SaveToNewState(AStringValue: string): TAdvResponsiveManagerItem; overload; virtual;
    function SaveToNewState(ABooleanValue: Boolean): TAdvResponsiveManagerItem; overload; virtual;
    function SaveToNewState(ANumberValue: Extended): TAdvResponsiveManagerItem; overload; virtual;

    function FindState(AStringValue: string): TAdvResponsiveManagerItem; overload; virtual;
    function FindState(ABooleanValue: Boolean): TAdvResponsiveManagerItem; overload; virtual;
    function FindState(ANumberValue: Extended): TAdvResponsiveManagerItem; overload; virtual;
    function FindState: TAdvResponsiveManagerItem; overload; virtual;

    procedure Preview; virtual;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvResponsiveManager = class(TAdvCustomResponsiveManager)
  public
    procedure RegisterRuntimeClasses; override;
  published
    property ActiveState;
    property AutoSave;
    property AutoLoadOnResize;
    property Version;
    property Mode;
    property States;
    property Control;
    property OnLoadStateCustom;
    property OnBeforeLoadControlState;
    property OnAfterLoadControlState;
    property OnBeforeLoadState;
    property OnAfterLoadState;
  end;

  {$IFNDEF FNCLIB}
  {$IFDEF WEBLIB}
  TResponsiveManager = class(TAdvResponsiveManager);
  {$ENDIF}
  {$ENDIF}

implementation

uses
  SysUtils, Math, Graphics, Forms, Controls,
  AdvUtils, AdvGraphics, AdvGraphicsTypes;

{$R 'AdvResponsiveManager.res'}

type
  TAdvResponsiveManagerPreviewForm = class(TForm);
  TCustomFormOpen = class(TCustomForm);
  {$IFNDEF FMXLIB}
  TControlOpen = class(TWinControl);
  {$ENDIF}

{ TAdvCustomResponsiveManager }

procedure TAdvCustomResponsiveManager.DoPreviewResize(Sender: TObject);
begin
  if Assigned(FPaintBox) and {$IFDEF VCLLIB}(FPaintBox.HandleAllocated) and {$ENDIF} (Sender is TCustomForm) {$IFDEF VCLLIB}and (Sender as TCustomForm).HandleAllocated{$ENDIF} then
  begin
    if Assigned(FPreviewManager) then
    begin
      case FPreviewManager.Mode of
        mrmWidthOnly, mrmWidthFirst: FPaintBox.SetBounds(0, 0, (Sender as TCustomForm).ClientWidth, 20);
        mrmHeightOnly, mrmHeightFirst: FPaintBox.SetBounds(0, 0, 20, (Sender as TCustomForm).ClientHeight);
      end;
    end;
  end;
end;

procedure TAdvCustomResponsiveManager.DoResponsiveResize(Sender: TObject);
begin
  if AutoLoadOnResize then
    LoadState;

  if Assigned(FOldResize) then
    FOldResize(Sender);
end;

procedure TAdvCustomResponsiveManager.AfterAssignControl;
begin
  inherited;
  if not IsDesigning then
  begin
    {$IFDEF FMXLIB}
    if Control is TControl then
    begin
      FOldResize := (Control as TControl).OnResize;
      (Control as TControl).OnResize := DoResponsiveResize;
    end
    {$ELSE}
    if Control is TWinControl then
    begin
      FOldResize := TControlOpen(Control).OnResize;
      TControlOpen(Control).OnResize := DoResponsiveResize;
    end
    {$ENDIF}
    else if Control is TCustomForm then
    begin
      FOldResize := TCustomFormOpen(Control).OnResize;
      TCustomFormOpen(Control).OnResize := DoResponsiveResize;
    end;
  end;
end;

procedure TAdvCustomResponsiveManager.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvCustomResponsiveManager then
  begin
    FAutoLoadOnResize := (Source as TAdvCustomResponsiveManager).AutoLoadOnResize;
    FMode := (Source as TAdvCustomResponsiveManager).Mode;
  end;
end;

procedure TAdvCustomResponsiveManager.BeforeAssignControl;
begin
  inherited;
  if Assigned(Control) and Assigned(FOldResize) and not IsDesigning then
  begin
    {$IFDEF FMXLIB}
    if Control is TControl then
      (Control as TControl).OnResize := FOldResize
    {$ELSE}
    if Control is TWinControl then
      TControlOpen(Control).OnResize := FOldResize
    {$ENDIF}
    else if Control is TCustomForm then
      TCustomFormOpen(Control).OnResize := FOldResize;
  end;
end;

procedure TAdvCustomResponsiveManager.BeforeLoadState(
  AState: TAdvStateManagerItem);
var
  c: TAdvStateManagerControl;
  r: TAdvResponsiveManagerItem;
begin
  inherited;

  if BlockLoadConstraints then
    Exit;

  c := Control;
  if Assigned(c) and (AState is TAdvResponsiveManagerItem) then
  begin
    r := AState as TAdvResponsiveManagerItem;
    if c is TCustomForm then
    begin
      (c as TCustomForm).ClientWidth := Round(r.Constraint.Width);
      (c as TCustomForm).ClientHeight := Round(r.Constraint.Height);
    end
    {$IFDEF WEBLIB}
    else if c is TWinControl then
    begin
      (c as TWinControl).Width := Round(r.Constraint.Width);
      (c as TWinControl).Height := Round(r.Constraint.Height);
    end;
    {$ELSE}
    else if c is TControl then
    begin
      (c as TControl).Width := Round(r.Constraint.Width);
      (c as TControl).Height := Round(r.Constraint.Height);
    end;
    {$ENDIF}
  end;
end;

constructor TAdvCustomResponsiveManager.Create(AOwner: TComponent);
begin
  inherited;
  FOldResize := nil;
  FMode := mrmWidthOnly;
  FAutoLoadOnResize := True;
end;

function TAdvCustomResponsiveManager.CreateStatesCollection: TAdvStateManagerItems;
begin
  Result := TAdvResponsiveManagerItems.Create(Self);
end;

function TAdvCustomResponsiveManager.FindState(
  AStringValue: string): TAdvResponsiveManagerItem;
var
  c: TAdvResponsiveManagerConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    c.Mode := cmString;
    c.StringValue := AStringValue;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function TAdvCustomResponsiveManager.FindState(
  ABooleanValue: Boolean): TAdvResponsiveManagerItem;
var
  c: TAdvResponsiveManagerConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    c.Mode := cmBoolean;
    c.BooleanValue := ABooleanValue;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function TAdvCustomResponsiveManager.FindState(
  ANumberValue: Extended): TAdvResponsiveManagerItem;
var
  c: TAdvResponsiveManagerConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    c.Mode := cmNumber;
    c.NumberValue := ANumberValue;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function TAdvCustomResponsiveManager.FindState: TAdvResponsiveManagerItem;
var
  c: TAdvResponsiveManagerConstraint;
  d: TAdvResponsiveManagerSizeConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    d := GetSizeConstraint;
    c.Mode := cmSize;
    c.Width := d.Width;
    c.Height := d.Height;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function CompareSizeVal(const Item1, Item2: TAdvResponsiveManagerItem): Integer;
var
  m: TAdvCustomResponsiveManager;
begin
  Result := -1;

  m := Item1.Manager as TAdvCustomResponsiveManager;


  case m.Mode of
    mrmWidthOnly, mrmWidthFirst:
    begin
      Result := CompareValue(Item1.Constraint.Width, Item2.Constraint.Width);
      if m.Mode = mrmWidthFirst then
      begin
        if Result = EqualsValue then
          Result := CompareValue(Item1.Constraint.Height, Item2.Constraint.Height);
      end;
    end;
    mrmHeightOnly, mrmHeightFirst:
    begin
      Result := CompareValue(Item1.Constraint.Height, Item2.Constraint.Height);
      if m.Mode = mrmHeightFirst then
      begin
        if Result = EqualsValue then
          Result := CompareValue(Item1.Constraint.Width, Item2.Constraint.Width);
      end;
    end;
  end;
end;

procedure SortList(const AList: TAdvResponsiveManagerItemList; const AManager: TAdvCustomResponsiveManager);
{$IFNDEF LCLWEBLIB}
var
  cmp: TAdvResponsiveManagerItemSizeComparer;
{$ENDIF}
begin
  {$IFNDEF LCLWEBLIB}
  cmp := TAdvResponsiveManagerItemSizeComparer.Create;
  cmp.FManager := AManager;
  try
    AList.Sort(cmp);
  finally
    cmp.Free;
  end;
  {$ENDIF}
  {$IFDEF LCLLIB}
  AList.Sort(@CompareSizeVal);
  {$ENDIF}
  {$IFDEF WEBLIB}
  AList.Sort(TComparer<TAdvResponsiveManagerItem>.Construct(
    function(const ALeft, ARight: TAdvResponsiveManagerItem): Integer
    begin
      Result := CompareSizeVal(ALeft, ARight);
    end));
  {$ENDIF}
end;

function TAdvCustomResponsiveManager.InternalFindState(
  AConstraint: TAdvResponsiveManagerConstraint): TAdvResponsiveManagerItem;
var
  I: Integer;
  Z: Single;
  CW, CH: Single;
  l: TAdvResponsiveManagerItemList;
  l2: TAdvResponsiveManagerItemList;
begin
  Result := nil;

  if States.Count = 0 then
    Exit;

  case AConstraint.Mode of
    cmSize:
    begin
      CW := AConstraint.Width;
      CH := AConstraint.Height;
      {$IFDEF VCLLIB}
      CW := TAdvUtils.MulDivSingle(CW, DesigntimeFormPixelsPerInch, Round(DesigntimeFormPixelsPerInch * TAdvUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch)));
      CH := TAdvUtils.MulDivSingle(CH, DesigntimeFormPixelsPerInch, Round(DesigntimeFormPixelsPerInch * TAdvUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch)));
      {$ENDIF}

      l := TAdvResponsiveManagerItemList.Create;
      l2 := TAdvResponsiveManagerItemList.Create;
      try
        for i := 0 to States.Count - 1 do
          l.Add(States[I]);

        if l.Count = 0 then
          Exit;

        SortList(l, Self);

        if Mode in [mrmWidthOnly, mrmWidthFirst] then
        begin
          for I := l.Count - 1 downto 0 do
          begin
            if CW >= l[I].Constraint.Width then
              l2.Add(l[I]);
          end;
        end
        else
        begin
          for I := l.Count - 1 downto 0 do
          begin
            if CH >= l[I].Constraint.Height then
              l2.Add(l[I]);
          end;
        end;

        SortList(l2, Self);

        if l2.Count = 0 then
        begin
          Result := l[0];

          for I := l.Count - 1 downto 0 do
          begin
            if Mode = mrmWidthFirst then
            begin
              if CH <= Result.Constraint.Height then
                Result := l[I];
            end
            else if Mode = mrmHeightFirst then
            begin
              if CW <= Result.Constraint.Width then
                Result := l[I];
            end;
          end;
        end
        else
        begin
          Result := l2[l2.Count - 1];

          if Mode = mrmWidthFirst then
          begin
            Z := Abs(CH - Result.Constraint.Height);
            for I := l2.Count - 1 downto 0 do
            begin
              if (Abs(CH - l2[I].Constraint.Height) < Z) then
              begin
                Result := l2[I];
                Z := Abs(CH - Result.Constraint.Height);
              end;
            end;
          end
          else if Mode = mrmHeightFirst then
          begin
            Z := Abs(CW - Result.Constraint.Width);
            for I := l2.Count - 1 downto 0 do
            begin
              if (Abs(CW - l2[I].Constraint.Width) < Z) then
              begin
                Result := l2[I];
                Z := Abs(CW - Result.Constraint.Width);
              end;
            end;
          end;
        end;

      finally
        l.Free;
        l2.Free;
      end;
    end;
    cmBoolean:
    begin
      Result := GetDefaultState as TAdvResponsiveManagerItem;
      for I := 0 to States.Count - 1 do
      begin
        if (States[I].Constraint.BooleanValue = AConstraint.BooleanValue) then
        begin
          Result := States[I];
          Break;
        end;
      end;
    end;
    cmNumber:
    begin
      Result := GetDefaultState as TAdvResponsiveManagerItem;
      for I := 0 to States.Count - 1 do
      begin
        if (States[I].Constraint.NumberValue = AConstraint.NumberValue) then
        begin
          Result := States[I];
          Break;
        end;
      end;
    end;
    cmString:
    begin
      Result := GetDefaultState as TAdvResponsiveManagerItem;
      for I := 0 to States.Count - 1 do
      begin
        if (States[I].Constraint.StringValue = AConstraint.StringValue) then
        begin
          Result := States[I];
          Break;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomResponsiveManager.InternalSaveToState(
  AState: TAdvStateManagerItem; ANew: Boolean);
begin
  inherited;
  if (AState is TAdvResponsiveManagerItem) then
    SetConstraint(AState as TAdvResponsiveManagerItem);
end;

procedure TAdvCustomResponsiveManager.InternalSetActiveState(
  AState: TAdvStateManagerItem);
var
  f: TAdvResponsiveManagerPreviewForm;
begin
  inherited;
  if Assigned(Self.Parent) and (Self.Parent is TAdvResponsiveManagerPreviewForm) then
  begin
    f := (Self.Parent as TAdvResponsiveManagerPreviewForm);
    f.Caption := 'Responsive Manager Preview';
    if (ActiveState >= 0) and (ActiveState <= States.Count - 1) then
      f.Caption := f.Caption + ' | Active State = ' + States[ActiveState].Name;
  end;  
end;

function TAdvCustomResponsiveManager.GetDocURL: string;
begin
  Result := TAdvBaseDocURL + 'tmsfnccore/components/ttmsfncresponsivemanager/';
end;

function TAdvCustomResponsiveManager.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TAdvCustomResponsiveManager.GetSizeConstraint: TAdvResponsiveManagerSizeConstraint;
var
  c: TAdvStateManagerControl;
begin
  Result.Width := -1;
  Result.Height := -1;

  c := Control;

  if Assigned(c) then
  begin
    if c is TCustomForm then
    begin
      Result.Width := (c as TCustomForm).ClientWidth;
      Result.Height := (c as TCustomForm).ClientHeight;
    end
    {$IFDEF WEBLIB}
    else if c is TWinControl then
    begin
      Result.Width := (c as TWinControl).Width;
      Result.Height := (c as TWinControl).Height;
    end;
    {$ELSE}
    else if c is TControl then
    begin
      Result.Width := (c as TControl).Width;
      Result.Height := (c as TControl).Height;
    end;
    {$ENDIF}
  end;
end;

function TAdvCustomResponsiveManager.GetStates: TAdvResponsiveManagerItems;
begin
  Result := TAdvResponsiveManagerItems(inherited States);
end;

procedure TAdvCustomResponsiveManager.SetConstraint(
  AState: TAdvResponsiveManagerItem);
var
  d: TAdvResponsiveManagerSizeConstraint;
begin
  if Assigned(AState) then
  begin
    d := GetSizeConstraint;
    AState.Constraint.Width := d.Width;
    AState.Constraint.Height := d.Height;
  end;
end;

procedure TAdvCustomResponsiveManager.SetStates(const Value: TAdvResponsiveManagerItems);
begin
  inherited States := Value;
end;

function TAdvCustomResponsiveManager.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCustomResponsiveManager.LoadState(AStringValue: string);
var
  c: TAdvResponsiveManagerConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    c.Mode := cmString;
    c.StringValue := AStringValue;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TAdvCustomResponsiveManager.LoadState(ABooleanValue: Boolean);
var
  c: TAdvResponsiveManagerConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    c.Mode := cmBoolean;
    c.BooleanValue := ABooleanValue;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TAdvCustomResponsiveManager.LoadState(ANumberValue: Extended);
var
  c: TAdvResponsiveManagerConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    c.Mode := cmNumber;
    c.NumberValue := ANumberValue;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TAdvCustomResponsiveManager.LoadState;
var
  c: TAdvResponsiveManagerConstraint;
  d: TAdvResponsiveManagerSizeConstraint;
begin
  c := TAdvResponsiveManagerConstraint.Create;
  try
    d := GetSizeConstraint;
    c.Mode := cmSize;
    c.Width := d.Width;
    c.Height := d.Height;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TAdvCustomResponsiveManager.Preview;
{$IFNDEF WEBLIB}
var
  f: TForm;
{$ENDIF}
begin
  {$IFNDEF WEBLIB}
  if Assigned(Control) then
  begin
    if Control is TCustomForm then
    begin
      f := TAdvResponsiveManagerPreviewForm.CreateNew(nil);
      try
        f.Name := (Control as TCustomForm).Name;
        f.Caption := 'Responsive Manager Preview';
        f.ClientWidth := (Control as TCustomForm).ClientWidth;
        f.ClientHeight := (Control as TCustomForm).ClientHeight;
        {$IFDEF FMXLIB}
        f.Position := TFormPosition.ScreenCenter;
        {$ELSE}
        f.Position := poScreenCenter;
        {$ENDIF}
        
        f.AfterConstruction;

        f.OnResize := DoPreviewResize;

        FPreviewManager := TAdvCustomResponsiveManager.Create(f);
        FPreviewManager.Assign(Self);
        FPreviewManager.Parent := f;
        FPreviewManager.Visible := False;
        FPreviewManager.Recreate;
        FPreviewManager.ActiveState := -1;
        FPreviewManager.Control := f;

        FPaintBox := TAdvResponsiveManagerPaintBox.Create(f);
        FPaintBox.FManager := FPreviewManager;
        FPaintBox.SetBounds(0, 0, f.ClientWidth, 20);
        FPaintBox.Parent := f;

        f.ShowModal;
      finally
        f.Free;
      end;
    end
    else
      raise Exception.Create('Preview cannot be started with ' + Control.Name);
  end;
  {$ENDIF}
end;

procedure TAdvCustomResponsiveManager.Recreate;

  {$IFNDEF WEBLIB}
  procedure CloneProperties(const Source: TAdvStateManagerControl; const Dest: TAdvStateManagerControl);
  var
    ms: TMemoryStream;
    w: TWriter;
    r: TReader;
  begin
    ms := TMemoryStream.Create;
    
    w := TWriter.Create(ms, 4096);
    try
      w.IgnoreChildren := True;
      w.WriteDescendent(Source, nil);
    finally
      w.Free;
    end;

    ms.Position := 0;

    r := TReader.Create(ms, 4096);
    try
      r.IgnoreChildren := True;
      {$IFDEF VCLLIB}
      Dest.Parent := Self;
      {$ENDIF}
      r.ReadRootComponent(Dest);
      {$IFDEF VCLLIB}
      Dest.Parent := nil;
      {$ENDIF}
    finally
      r.Free;
    end;    
  end;

  procedure AddControls(AControl: TAdvStateManagerControl);
  var
    cn: string;
    ct: TComponent;
    c, p: TAdvStateManagerControl;
    I: Integer;
  begin
    for I := 0 to AControl.ComponentCount - 1 do
    begin
      ct := AControl.Components[I];
      if Assigned(ct) and (ct is TAdvStateManagerControl) then
      begin
        if CanPersist(ct as TAdvStateManagerControl) then
        begin
          cn := ct.Name;
          if cn <> '' then
          begin
            c := TComponentClass(ct.ClassType).Create(Self.Parent) as TAdvStateManagerControl;
            c.Name := cn;
            CloneProperties(ct as TAdvStateManagerControl, c);

            if Assigned((ct as TAdvStateManagerControl).Parent) then
            begin
              if (ct as TAdvStateManagerControl).Parent is TCustomForm then
                (c as TAdvStateManagerControl).Parent := Self.Parent
              else
              begin
                p := FindControlByName((ct as TAdvStateManagerControl).Parent.Name, Self.Parent);
                if Assigned(p) {$IFNDEF FMXLIB}and (p is TWinControl){$ENDIF} then
                  (c as TAdvStateManagerControl).Parent := p{$IFNDEF FMXLIB} as TWinControl{$ENDIF};
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}
begin
  {$IFNDEF WEBLIB}
  if Assigned(Control) then
    AddControls(Control);
  {$ENDIF}
end;

function TAdvCustomResponsiveManager.SaveToNewState: TAdvResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);
end;

function TAdvCustomResponsiveManager.SaveToNewState(
  AStringValue: string): TAdvResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);

  Result.Constraint.StringValue := AStringValue;
end;

{ TAdvResponsiveManagerItems }

function TAdvResponsiveManagerItems.Add: TAdvResponsiveManagerItem;
begin
  Result := TAdvResponsiveManagerItem(inherited Add);
end;

function TAdvResponsiveManagerItems.GetItemEx(Index: Integer): TAdvResponsiveManagerItem;
begin
  Result := TAdvResponsiveManagerItem(inherited Items[Index]);
end;

function TAdvResponsiveManagerItems.GetStateManagerStateClass: TCollectionItemClass;
begin
  Result := TAdvResponsiveManagerItem;
end;

function TAdvResponsiveManagerItems.Insert(index: Integer): TAdvResponsiveManagerItem;
begin
  Result := TAdvResponsiveManagerItem(inherited Insert(Index));
end;

procedure TAdvResponsiveManagerItems.SetItemEx(Index: Integer; const Value: TAdvResponsiveManagerItem);
begin
  inherited SetItem(Index, Value);
end;

function TAdvCustomResponsiveManager.SaveToNewState(
  ANumberValue: Extended): TAdvResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);

  Result.Constraint.NumberValue := ANumberValue;
end;

function TAdvCustomResponsiveManager.SaveToNewState(
  ABooleanValue: Boolean): TAdvResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);

  Result.Constraint.BooleanValue := ABooleanValue;
end;

{ TAdvResponsiveManagerItem }

procedure TAdvResponsiveManagerItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TAdvResponsiveManagerItem) then
    FConstraint.Assign((Source as TAdvResponsiveManagerItem).Constraint);
end;

constructor TAdvResponsiveManagerItem.Create(Collection: TCollection);
var
  m: TAdvCustomStateManager;
begin
  FConstraint := TAdvResponsiveManagerConstraint.Create;
  inherited;

  m := Manager;
  if Assigned(m) and (m is TAdvCustomResponsiveManager) then
    (m as TAdvCustomResponsiveManager).SetConstraint(Self);
end;

destructor TAdvResponsiveManagerItem.Destroy;
begin
  FConstraint.Free;
  inherited;
end;

function TAdvResponsiveManagerItem.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
  Result := Result + ' [W=' + FloatToStr(Constraint.Width) + ', H=' + FloatToStr(Constraint.Height) + ']'
end;

procedure TAdvResponsiveManagerItem.SetConstraint(
  const Value: TAdvResponsiveManagerConstraint);
begin
  FConstraint.Assign(Value);
end;

{ TAdvResponsiveManagerItemComparer }

{$IFNDEF LCLWEBLIB}
function TAdvResponsiveManagerItemSizeComparer.Compare(const Left,
  Right: TAdvResponsiveManagerItem): Integer;
begin
  Result := CompareSizeVal(Left, Right);
end;
{$ENDIF}

{ TAdvResponsiveManager }

procedure TAdvResponsiveManager.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TAdvResponsiveManagerItem);
end;

{ TAdvResponsiveManagerConstraint }

procedure TAdvResponsiveManagerConstraint.Assign(Source: TPersistent);
begin
  if Source is TAdvResponsiveManagerConstraint then
  begin
    FStringValue := (Source as TAdvResponsiveManagerConstraint).StringValue;
    FNumberValue := (Source as TAdvResponsiveManagerConstraint).NumberValue;
    FBooleanValue := (Source as TAdvResponsiveManagerConstraint).BooleanValue;
    FWidth := (Source as TAdvResponsiveManagerConstraint).Width;
    FHeight := (Source as TAdvResponsiveManagerConstraint).Height;
  end;
end;

constructor TAdvResponsiveManagerConstraint.Create;
begin
  FWidth := 0;
  FHeight := 0;
  FStringValue := '';
  FBooleanValue := False;
  FNumberValue := 0;
end;

destructor TAdvResponsiveManagerConstraint.Destroy;
begin

  inherited;
end;

function TAdvResponsiveManagerConstraint.IsHeightStored: Boolean;
begin
  Result := Height <> 0;
end;

function TAdvResponsiveManagerConstraint.IsNumberValueStored: Boolean;
begin
  Result := NumberValue <> 0;
end;

function TAdvResponsiveManagerConstraint.IsWidthStored: Boolean;
begin
  Result := Width <> 0;
end;

{ TAdvResponsiveManagerPaintBox }

procedure TAdvResponsiveManagerPaintBox.Paint;
var
  g: TAdvGraphics;
  l: TAdvResponsiveManagerItemList;
  i: Integer;
  x, w, ww, y, h, hh: Single;
  r: TRectF;

const
  c: array[0..9] of TAdvGraphicsColor = (
  {$IFDEF FMXLIB}
  $FF57C0F5,
  $FFF71F56,
  $FF031B79,
  $FF954EBB,
  $FFEEC713,
  $FFF36042,
  $FF2773FC,
  $FFA1CF58,
  $FF00A9A6,
  $FF85E4F9
  {$ENDIF}
  {$IFNDEF FMXLIB}
  $F5C057,
  $561FF7,
  $791B03,
  $BB4E95,
  $13C7EE,
  $4260F3,
  $FC7327,
  $58CFA1,
  $A6A900,
  $F9E485
  {$ENDIF}
  );
begin
  if not Assigned(FManager) then
    Exit;

  l := TAdvResponsiveManagerItemList.Create;
  try
    for i := 0 to FManager.States.Count - 1 do
      l.Add(FManager.States[I]);

    if l.Count = 0 then
      Exit;

    SortList(l, FManager);

    x := 0;
    y := 0;

    g := TAdvGraphics.Create(Canvas);
    try
      g.Font.Color := gcWhite;

      for I := 0 to l.Count - 1 do
      begin
        if I < Length(c) then
        begin
          g.Fill.Color := c[I];
          g.Stroke.Color := c[I];
        end;

        case FManager.Mode of
          mrmWidthOnly, mrmWidthFirst:
          begin
            if I < l.Count - 1 then
            begin
              WW := l[I + 1].Constraint.Width;
              {$IFDEF VCLLIB}
              WW := WW * TAdvUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch);
              {$ENDIF}
              w := ww - x
            end
            else
              w := Width - x;

            r := RectF(x, 0, x + w, Height);

            if r.Left < Width then
            begin
              g.DrawRectangle(r);

              InflateRectEx(r, -2, -2);

              g.DrawText(r, l[I].Name, False, gtaLeading, gtaCenter, gttNone);
            end;

            x := x + w;

          end;
          mrmHeightOnly, mrmHeightFirst:
          begin
            if I < l.Count - 1 then
            begin
              HH := l[I + 1].Constraint.Height;
              {$IFDEF VCLLIB}
              HH := HH * TAdvUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch);
              {$ENDIF}
              h := HH - y
            end
            else
              h := Height - y;

            r := RectF(0, y, Width, y + h);

            if r.Top < Height then
            begin
              g.DrawRectangle(r);

              InflateRectEx(r, -2, -2);

              g.DrawText(r, l[I].Name, False, gtaLeading, gtaCenter, gttNone, 90);
            end;

            y := y + h;
          end;
        end;

      end;
    finally
      g.Free;
    end;
  finally
    l.Free;
  end;
end;

end.
