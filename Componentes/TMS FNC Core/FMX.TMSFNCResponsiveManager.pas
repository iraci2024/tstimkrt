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

unit FMX.TMSFNCResponsiveManager;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

//{$DEFINE NOPP}

interface

uses
  Classes, FMX.TMSFNCStateManager, FMX.TMSFNCCustomControl, Types, FMX.TMSFNCTypes
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
  TTMSFNCCustomResponsiveManager = class;

  TTMSFNCResponsiveManagerConstraintMode = (cmSize, cmString, cmBoolean, cmNumber);

  TTMSFNCResponsiveManagerConstraint = class(TPersistent)
  private
    FWidth: Single;
    FHeight: Single;
    FStringValue: string;
    FMode: TTMSFNCResponsiveManagerConstraintMode;
    FBooleanValue: Boolean;
    FNumberValue: Extended;
    function IsHeightStored: Boolean;
    function IsWidthStored: Boolean;
    function IsNumberValueStored: Boolean;
  protected
    property Mode: TTMSFNCResponsiveManagerConstraintMode read FMode write FMode default cmSize;
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

  TTMSFNCResponsiveManagerSizeConstraint = record
    Width, Height: Single;
  end;

  TTMSFNCResponsiveManagerItem = class(TTMSFNCStateManagerItem)
  private
    FConstraint: TTMSFNCResponsiveManagerConstraint;
    procedure SetConstraint(const Value: TTMSFNCResponsiveManagerConstraint);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property Constraint: TTMSFNCResponsiveManagerConstraint read FConstraint write SetConstraint;
  end;

  TTMSFNCResponsiveManagerItems = class(TTMSFNCStateManagerItems)
  private
    function GetItemEx(Index: Integer): TTMSFNCResponsiveManagerItem;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCResponsiveManagerItem);
  protected
    function GetStateManagerStateClass: TCollectionItemClass; override;
  public
    function Add: TTMSFNCResponsiveManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    function Insert(index: Integer): TTMSFNCResponsiveManagerItem;{$IFDEF WEBLIB}reintroduce;{$ENDIF}
    property Items[Index: Integer]: TTMSFNCResponsiveManagerItem read GetItemEx write SetItemEx; default;
  end;

  TTMSFNCResponsiveManagerResizeMode = (mrmWidthOnly, mrmHeightOnly, mrmWidthFirst, mrmHeightFirst);

  {$IFNDEF LCLWEBLIB}
  TTMSFNCResponsiveManagerItemSizeComparer = class(TComparer<TTMSFNCResponsiveManagerItem>)
  private
    FManager: TTMSFNCCustomResponsiveManager;
  public
    function Compare(const Left, Right: TTMSFNCResponsiveManagerItem): Integer; override;
  end;
  {$ENDIF}

  TTMSFNCResponsiveManagerItemList = TList<TTMSFNCResponsiveManagerItem>;

  TTMSFNCResponsiveManagerPaintBox = class(TTMSFNCCustomControl)
  private
    FManager: TTMSFNCCustomResponsiveManager;
  public
    procedure Paint; override;
  end;

  TTMSFNCCustomResponsiveManager = class(TTMSFNCCustomStateManager)
  private
    FPaintBox: TTMSFNCResponsiveManagerPaintBox;
    FPreviewManager:  TTMSFNCCustomResponsiveManager;
    FOldResize: TNotifyEvent;
    FAutoLoadOnResize: Boolean;
    FBlockLoadConstraints: Boolean;
    FMode: TTMSFNCResponsiveManagerResizeMode;
    function GetStates: TTMSFNCResponsiveManagerItems;
    procedure SetStates(const Value: TTMSFNCResponsiveManagerItems);
  protected
    function GetDocURL: string; override;
    function GetInstance: NativeUInt; override;
    function GetVersion: string; override;
    function GetSizeConstraint: TTMSFNCResponsiveManagerSizeConstraint;
    function InternalFindState(AConstraint: TTMSFNCResponsiveManagerConstraint): TTMSFNCResponsiveManagerItem; virtual;
    function CreateStatesCollection: TTMSFNCStateManagerItems; override;
    procedure InternalSetActiveState(AState: TTMSFNCStateManagerItem); override;
    procedure SetConstraint(AState: TTMSFNCResponsiveManagerItem); virtual;
    procedure InternalSaveToState(AState: TTMSFNCStateManagerItem; ANew: Boolean); override;
    procedure BeforeAssignControl; override;
    procedure AfterAssignControl; override;
    procedure BeforeLoadState(AState: TTMSFNCStateManagerItem); override;
    procedure DoResponsiveResize(Sender: TObject);
    procedure DoPreviewResize(Sender: TObject);
    procedure Recreate; virtual;

    property Mode: TTMSFNCResponsiveManagerResizeMode read FMode write FMode default mrmWidthOnly;
    property BlockLoadConstraints: Boolean read FBlockLoadConstraints write FBlockLoadConstraints;
    property States: TTMSFNCResponsiveManagerItems read GetStates write SetStates;
    property Version: string read GetVersion;
    property AutoLoadOnResize: Boolean read FAutoLoadOnResize write FAutoLoadOnResize default True;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); overload; override;

    procedure LoadState(AStringValue: string); overload; virtual;
    procedure LoadState(ABooleanValue: Boolean); overload; virtual;
    procedure LoadState(ANumberValue: Extended); overload; virtual;
    procedure LoadState; overload; virtual;

    function SaveToNewState: TTMSFNCResponsiveManagerItem; overload; virtual;
    function SaveToNewState(AStringValue: string): TTMSFNCResponsiveManagerItem; overload; virtual;
    function SaveToNewState(ABooleanValue: Boolean): TTMSFNCResponsiveManagerItem; overload; virtual;
    function SaveToNewState(ANumberValue: Extended): TTMSFNCResponsiveManagerItem; overload; virtual;

    function FindState(AStringValue: string): TTMSFNCResponsiveManagerItem; overload; virtual;
    function FindState(ABooleanValue: Boolean): TTMSFNCResponsiveManagerItem; overload; virtual;
    function FindState(ANumberValue: Extended): TTMSFNCResponsiveManagerItem; overload; virtual;
    function FindState: TTMSFNCResponsiveManagerItem; overload; virtual;

    procedure Preview; virtual;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCResponsiveManager = class(TTMSFNCCustomResponsiveManager)
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
  TResponsiveManager = class(TTMSFNCResponsiveManager);
  {$ENDIF}
  {$ENDIF}

implementation

uses
  SysUtils, Math, FMX.Graphics, FMX.Forms, FMX.Controls,
  FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes;

{$R 'TMSFNCResponsiveManager.res'}

type
  TTMSFNCResponsiveManagerPreviewForm = class(TForm);
  TCustomFormOpen = class(TCustomForm);
  {$IFNDEF FMXLIB}
  TControlOpen = class(TWinControl);
  {$ENDIF}

{ TTMSFNCCustomResponsiveManager }

procedure TTMSFNCCustomResponsiveManager.DoPreviewResize(Sender: TObject);
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

procedure TTMSFNCCustomResponsiveManager.DoResponsiveResize(Sender: TObject);
begin
  if AutoLoadOnResize then
    LoadState;

  if Assigned(FOldResize) then
    FOldResize(Sender);
end;

procedure TTMSFNCCustomResponsiveManager.AfterAssignControl;
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

procedure TTMSFNCCustomResponsiveManager.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TTMSFNCCustomResponsiveManager then
  begin
    FAutoLoadOnResize := (Source as TTMSFNCCustomResponsiveManager).AutoLoadOnResize;
    FMode := (Source as TTMSFNCCustomResponsiveManager).Mode;
  end;
end;

procedure TTMSFNCCustomResponsiveManager.BeforeAssignControl;
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

procedure TTMSFNCCustomResponsiveManager.BeforeLoadState(
  AState: TTMSFNCStateManagerItem);
var
  c: TTMSFNCStateManagerControl;
  r: TTMSFNCResponsiveManagerItem;
begin
  inherited;

  if BlockLoadConstraints then
    Exit;

  c := Control;
  if Assigned(c) and (AState is TTMSFNCResponsiveManagerItem) then
  begin
    r := AState as TTMSFNCResponsiveManagerItem;
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

constructor TTMSFNCCustomResponsiveManager.Create(AOwner: TComponent);
begin
  inherited;
  FOldResize := nil;
  FMode := mrmWidthOnly;
  FAutoLoadOnResize := True;
end;

function TTMSFNCCustomResponsiveManager.CreateStatesCollection: TTMSFNCStateManagerItems;
begin
  Result := TTMSFNCResponsiveManagerItems.Create(Self);
end;

function TTMSFNCCustomResponsiveManager.FindState(
  AStringValue: string): TTMSFNCResponsiveManagerItem;
var
  c: TTMSFNCResponsiveManagerConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
  try
    c.Mode := cmString;
    c.StringValue := AStringValue;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function TTMSFNCCustomResponsiveManager.FindState(
  ABooleanValue: Boolean): TTMSFNCResponsiveManagerItem;
var
  c: TTMSFNCResponsiveManagerConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
  try
    c.Mode := cmBoolean;
    c.BooleanValue := ABooleanValue;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function TTMSFNCCustomResponsiveManager.FindState(
  ANumberValue: Extended): TTMSFNCResponsiveManagerItem;
var
  c: TTMSFNCResponsiveManagerConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
  try
    c.Mode := cmNumber;
    c.NumberValue := ANumberValue;
    Result := InternalFindState(c);
  finally
    c.Free;
  end;
end;

function TTMSFNCCustomResponsiveManager.FindState: TTMSFNCResponsiveManagerItem;
var
  c: TTMSFNCResponsiveManagerConstraint;
  d: TTMSFNCResponsiveManagerSizeConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
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

function CompareSizeVal(const Item1, Item2: TTMSFNCResponsiveManagerItem): Integer;
var
  m: TTMSFNCCustomResponsiveManager;
begin
  Result := -1;

  m := Item1.Manager as TTMSFNCCustomResponsiveManager;


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

procedure SortList(const AList: TTMSFNCResponsiveManagerItemList; const AManager: TTMSFNCCustomResponsiveManager);
{$IFNDEF LCLWEBLIB}
var
  cmp: TTMSFNCResponsiveManagerItemSizeComparer;
{$ENDIF}
begin
  {$IFNDEF LCLWEBLIB}
  cmp := TTMSFNCResponsiveManagerItemSizeComparer.Create;
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
  AList.Sort(TComparer<TTMSFNCResponsiveManagerItem>.Construct(
    function(const ALeft, ARight: TTMSFNCResponsiveManagerItem): Integer
    begin
      Result := CompareSizeVal(ALeft, ARight);
    end));
  {$ENDIF}
end;

function TTMSFNCCustomResponsiveManager.InternalFindState(
  AConstraint: TTMSFNCResponsiveManagerConstraint): TTMSFNCResponsiveManagerItem;
var
  I: Integer;
  Z: Single;
  CW, CH: Single;
  l: TTMSFNCResponsiveManagerItemList;
  l2: TTMSFNCResponsiveManagerItemList;
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
      CW := TTMSFNCUtils.MulDivSingle(CW, DesigntimeFormPixelsPerInch, Round(DesigntimeFormPixelsPerInch * TTMSFNCUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch)));
      CH := TTMSFNCUtils.MulDivSingle(CH, DesigntimeFormPixelsPerInch, Round(DesigntimeFormPixelsPerInch * TTMSFNCUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch)));
      {$ENDIF}

      l := TTMSFNCResponsiveManagerItemList.Create;
      l2 := TTMSFNCResponsiveManagerItemList.Create;
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
      Result := GetDefaultState as TTMSFNCResponsiveManagerItem;
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
      Result := GetDefaultState as TTMSFNCResponsiveManagerItem;
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
      Result := GetDefaultState as TTMSFNCResponsiveManagerItem;
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

procedure TTMSFNCCustomResponsiveManager.InternalSaveToState(
  AState: TTMSFNCStateManagerItem; ANew: Boolean);
begin
  inherited;
  if (AState is TTMSFNCResponsiveManagerItem) then
    SetConstraint(AState as TTMSFNCResponsiveManagerItem);
end;

procedure TTMSFNCCustomResponsiveManager.InternalSetActiveState(
  AState: TTMSFNCStateManagerItem);
var
  f: TTMSFNCResponsiveManagerPreviewForm;
begin
  inherited;
  if Assigned(Self.Parent) and (Self.Parent is TTMSFNCResponsiveManagerPreviewForm) then
  begin
    f := (Self.Parent as TTMSFNCResponsiveManagerPreviewForm);
    f.Caption := 'Responsive Manager Preview';
    if (ActiveState >= 0) and (ActiveState <= States.Count - 1) then
      f.Caption := f.Caption + ' | Active State = ' + States[ActiveState].Name;
  end;  
end;

function TTMSFNCCustomResponsiveManager.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfnccore/components/ttmsfncresponsivemanager/';
end;

function TTMSFNCCustomResponsiveManager.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCCustomResponsiveManager.GetSizeConstraint: TTMSFNCResponsiveManagerSizeConstraint;
var
  c: TTMSFNCStateManagerControl;
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

function TTMSFNCCustomResponsiveManager.GetStates: TTMSFNCResponsiveManagerItems;
begin
  Result := TTMSFNCResponsiveManagerItems(inherited States);
end;

procedure TTMSFNCCustomResponsiveManager.SetConstraint(
  AState: TTMSFNCResponsiveManagerItem);
var
  d: TTMSFNCResponsiveManagerSizeConstraint;
begin
  if Assigned(AState) then
  begin
    d := GetSizeConstraint;
    AState.Constraint.Width := d.Width;
    AState.Constraint.Height := d.Height;
  end;
end;

procedure TTMSFNCCustomResponsiveManager.SetStates(const Value: TTMSFNCResponsiveManagerItems);
begin
  inherited States := Value;
end;

function TTMSFNCCustomResponsiveManager.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TTMSFNCCustomResponsiveManager.LoadState(AStringValue: string);
var
  c: TTMSFNCResponsiveManagerConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
  try
    c.Mode := cmString;
    c.StringValue := AStringValue;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TTMSFNCCustomResponsiveManager.LoadState(ABooleanValue: Boolean);
var
  c: TTMSFNCResponsiveManagerConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
  try
    c.Mode := cmBoolean;
    c.BooleanValue := ABooleanValue;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TTMSFNCCustomResponsiveManager.LoadState(ANumberValue: Extended);
var
  c: TTMSFNCResponsiveManagerConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
  try
    c.Mode := cmNumber;
    c.NumberValue := ANumberValue;
    InternalLoadState(InternalFindState(c));
  finally
    c.Free;
  end;
end;

procedure TTMSFNCCustomResponsiveManager.LoadState;
var
  c: TTMSFNCResponsiveManagerConstraint;
  d: TTMSFNCResponsiveManagerSizeConstraint;
begin
  c := TTMSFNCResponsiveManagerConstraint.Create;
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

procedure TTMSFNCCustomResponsiveManager.Preview;
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
      f := TTMSFNCResponsiveManagerPreviewForm.CreateNew(nil);
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

        FPreviewManager := TTMSFNCCustomResponsiveManager.Create(f);
        FPreviewManager.Assign(Self);
        FPreviewManager.Parent := f;
        FPreviewManager.Visible := False;
        FPreviewManager.Recreate;
        FPreviewManager.ActiveState := -1;
        FPreviewManager.Control := f;

        FPaintBox := TTMSFNCResponsiveManagerPaintBox.Create(f);
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

procedure TTMSFNCCustomResponsiveManager.Recreate;

  {$IFNDEF WEBLIB}
  procedure CloneProperties(const Source: TTMSFNCStateManagerControl; const Dest: TTMSFNCStateManagerControl);
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

  procedure AddControls(AControl: TTMSFNCStateManagerControl);
  var
    cn: string;
    ct: TComponent;
    c, p: TTMSFNCStateManagerControl;
    I: Integer;
  begin
    for I := 0 to AControl.ComponentCount - 1 do
    begin
      ct := AControl.Components[I];
      if Assigned(ct) and (ct is TTMSFNCStateManagerControl) then
      begin
        if CanPersist(ct as TTMSFNCStateManagerControl) then
        begin
          cn := ct.Name;
          if cn <> '' then
          begin
            c := TComponentClass(ct.ClassType).Create(Self.Parent) as TTMSFNCStateManagerControl;
            c.Name := cn;
            CloneProperties(ct as TTMSFNCStateManagerControl, c);

            if Assigned((ct as TTMSFNCStateManagerControl).Parent) then
            begin
              if (ct as TTMSFNCStateManagerControl).Parent is TCustomForm then
                (c as TTMSFNCStateManagerControl).Parent := Self.Parent
              else
              begin
                p := FindControlByName((ct as TTMSFNCStateManagerControl).Parent.Name, Self.Parent);
                if Assigned(p) {$IFNDEF FMXLIB}and (p is TWinControl){$ENDIF} then
                  (c as TTMSFNCStateManagerControl).Parent := p{$IFNDEF FMXLIB} as TWinControl{$ENDIF};
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

function TTMSFNCCustomResponsiveManager.SaveToNewState: TTMSFNCResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);
end;

function TTMSFNCCustomResponsiveManager.SaveToNewState(
  AStringValue: string): TTMSFNCResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);

  Result.Constraint.StringValue := AStringValue;
end;

{ TTMSFNCResponsiveManagerItems }

function TTMSFNCResponsiveManagerItems.Add: TTMSFNCResponsiveManagerItem;
begin
  Result := TTMSFNCResponsiveManagerItem(inherited Add);
end;

function TTMSFNCResponsiveManagerItems.GetItemEx(Index: Integer): TTMSFNCResponsiveManagerItem;
begin
  Result := TTMSFNCResponsiveManagerItem(inherited Items[Index]);
end;

function TTMSFNCResponsiveManagerItems.GetStateManagerStateClass: TCollectionItemClass;
begin
  Result := TTMSFNCResponsiveManagerItem;
end;

function TTMSFNCResponsiveManagerItems.Insert(index: Integer): TTMSFNCResponsiveManagerItem;
begin
  Result := TTMSFNCResponsiveManagerItem(inherited Insert(Index));
end;

procedure TTMSFNCResponsiveManagerItems.SetItemEx(Index: Integer; const Value: TTMSFNCResponsiveManagerItem);
begin
  inherited SetItem(Index, Value);
end;

function TTMSFNCCustomResponsiveManager.SaveToNewState(
  ANumberValue: Extended): TTMSFNCResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);

  Result.Constraint.NumberValue := ANumberValue;
end;

function TTMSFNCCustomResponsiveManager.SaveToNewState(
  ABooleanValue: Boolean): TTMSFNCResponsiveManagerItem;
begin
  Result := States.Add;
  if not IsDesignTime then
    InternalSaveToState(Result, True)
  else
    InternalSetActiveState(Result);

  Result.Constraint.BooleanValue := ABooleanValue;
end;

{ TTMSFNCResponsiveManagerItem }

procedure TTMSFNCResponsiveManagerItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TTMSFNCResponsiveManagerItem) then
    FConstraint.Assign((Source as TTMSFNCResponsiveManagerItem).Constraint);
end;

constructor TTMSFNCResponsiveManagerItem.Create(Collection: TCollection);
var
  m: TTMSFNCCustomStateManager;
begin
  FConstraint := TTMSFNCResponsiveManagerConstraint.Create;
  inherited;

  m := Manager;
  if Assigned(m) and (m is TTMSFNCCustomResponsiveManager) then
    (m as TTMSFNCCustomResponsiveManager).SetConstraint(Self);
end;

destructor TTMSFNCResponsiveManagerItem.Destroy;
begin
  FConstraint.Free;
  inherited;
end;

function TTMSFNCResponsiveManagerItem.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
  Result := Result + ' [W=' + FloatToStr(Constraint.Width) + ', H=' + FloatToStr(Constraint.Height) + ']'
end;

procedure TTMSFNCResponsiveManagerItem.SetConstraint(
  const Value: TTMSFNCResponsiveManagerConstraint);
begin
  FConstraint.Assign(Value);
end;

{ TTMSFNCResponsiveManagerItemComparer }

{$IFNDEF LCLWEBLIB}
function TTMSFNCResponsiveManagerItemSizeComparer.Compare(const Left,
  Right: TTMSFNCResponsiveManagerItem): Integer;
begin
  Result := CompareSizeVal(Left, Right);
end;
{$ENDIF}

{ TTMSFNCResponsiveManager }

procedure TTMSFNCResponsiveManager.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCResponsiveManagerItem);
end;

{ TTMSFNCResponsiveManagerConstraint }

procedure TTMSFNCResponsiveManagerConstraint.Assign(Source: TPersistent);
begin
  if Source is TTMSFNCResponsiveManagerConstraint then
  begin
    FStringValue := (Source as TTMSFNCResponsiveManagerConstraint).StringValue;
    FNumberValue := (Source as TTMSFNCResponsiveManagerConstraint).NumberValue;
    FBooleanValue := (Source as TTMSFNCResponsiveManagerConstraint).BooleanValue;
    FWidth := (Source as TTMSFNCResponsiveManagerConstraint).Width;
    FHeight := (Source as TTMSFNCResponsiveManagerConstraint).Height;
  end;
end;

constructor TTMSFNCResponsiveManagerConstraint.Create;
begin
  FWidth := 0;
  FHeight := 0;
  FStringValue := '';
  FBooleanValue := False;
  FNumberValue := 0;
end;

destructor TTMSFNCResponsiveManagerConstraint.Destroy;
begin

  inherited;
end;

function TTMSFNCResponsiveManagerConstraint.IsHeightStored: Boolean;
begin
  Result := Height <> 0;
end;

function TTMSFNCResponsiveManagerConstraint.IsNumberValueStored: Boolean;
begin
  Result := NumberValue <> 0;
end;

function TTMSFNCResponsiveManagerConstraint.IsWidthStored: Boolean;
begin
  Result := Width <> 0;
end;

{ TTMSFNCResponsiveManagerPaintBox }

procedure TTMSFNCResponsiveManagerPaintBox.Paint;
var
  g: TTMSFNCGraphics;
  l: TTMSFNCResponsiveManagerItemList;
  i: Integer;
  x, w, ww, y, h, hh: Single;
  r: TRectF;

const
  c: array[0..9] of TTMSFNCGraphicsColor = (
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

  l := TTMSFNCResponsiveManagerItemList.Create;
  try
    for i := 0 to FManager.States.Count - 1 do
      l.Add(FManager.States[I]);

    if l.Count = 0 then
      Exit;

    SortList(l, FManager);

    x := 0;
    y := 0;

    g := TTMSFNCGraphics.Create(Canvas);
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
              WW := WW * TTMSFNCUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch);
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
              HH := HH * TTMSFNCUtils.GetDPIScale(Self, DesigntimeFormPixelsPerInch);
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
