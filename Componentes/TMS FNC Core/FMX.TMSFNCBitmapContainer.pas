{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2022                               }
{            Email : info@tmssoftware.com                            }
{            Web : https//www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCBitmapContainer;

{$I FMX.TMSFNCDefines.inc}

interface

uses
  Classes, FMX.TMSFNCTypes, FMX.TMSFNCCustomComponent, FMX.Controls
  {$IFNDEF WEBLIB}
  {$IFNDEF LCLLIB}
  , Generics.Collections
  {$ENDIF}
  {$IFDEF LCLLIB}
  , fgl
  {$ENDIF}
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
  TTMSFNCBitmapContainer = class;

  TTMSFNCBitmapItem = class;

  ITMSFNCBitmapContainer = interface
    ['{ED26710D-395F-4971-8AC9-A31083BF2A3C}']
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
  end;

  ITMSFNCBitmapContainerGetItem = interface
    ['{98F65D59-B40C-4574-AF9C-3CA68E86AE10}']
    function ItemCount: Integer;
    function GetItem(AIndex: Integer): TTMSFNCBitmapItem;
  end;

  TTMSFNCBitmapItem = class(TCollectionItem)
  private
    FBitmap: TTMSFNCBitmap;
    FTag: NativeInt;
    FName: string;
    procedure SetBitmap(const Value: TTMSFNCBitmap);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure DoBitmapChanged(Sender: TObject); virtual;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
  published
    property Bitmap: TTMSFNCBitmap read FBitmap write SetBitmap;
    property Name: string read FName write FName;
    property Tag: NativeInt read FTag write FTag;
  end;

  TTMSFNCBitmapCollection = class(TOwnedCollection)
  private
    FOwner: TTMSFNCBitmapContainer;
    function GetItemEx(Index: Integer): TTMSFNCBitmapItem;
    procedure SetItemEx(Index: Integer; const Value: TTMSFNCBitmapItem);
  protected
    function GetBitmapItemClass: TCollectionItemClass; virtual;
  public
    constructor Create(AOwner: TTMSFNCBitmapContainer);
    function Add: TTMSFNCBitmapItem;
    function Insert(index: Integer): TTMSFNCBitmapItem;
    property Items[Index: Integer]: TTMSFNCBitmapItem read GetItemEx write SetItemEx; default;
  end;

  {$IFDEF WEBLIB}
  TControlList = class(TList);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TControlList = class(TList<TControl>);
  {$ENDIF}

  {$IFDEF FMXLIB}
  TWinControl = TControl;
  {$ENDIF}

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TTMSFNCBitmapContainer = class(TTMSFNCCustomComponent, ITMSFNCBitmapContainerGetItem)
  private
    FControls: TControlList;
    FItems: TTMSFNCBitmapCollection;
    FOnBitmapChanged: TNotifyEvent;
    procedure SetItems(const Value: TTMSFNCBitmapCollection);
    function GetBitmapName(AIndex: Integer): String;
    function GetBitmap(AIndex: Integer): TTMSFNCBitmap;
    function GetItems: TTMSFNCBitmapCollection;
  protected
    function GetInstance: NativeUInt; override;
    function GetVersion: string; override;
    function CreateItems: TTMSFNCBitmapCollection; virtual;
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoBitmapChanged(Sender: TObject);
    procedure InvalidateMembers(AControl: TWinControl); virtual;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed; virtual;
    procedure RegisterControl(AControl: TControl);
    procedure FindBitmap(i: Integer; ABitmap: TTMSFNCBitmap); overload; virtual;
    function GetItem(AIndex: Integer): TTMSFNCBitmapItem; virtual;
    function FindBitmap(s: string): TTMSFNCBitmap; overload; virtual;
    function ItemCount: Integer;
    function RandomBitmapName: string;
    function RandomBitmap: TTMSFNCBitmap;
    procedure AddFromURL({%H-}URL, {%H-}BitmapName: string); virtual;
    procedure AddFromResource(ResourceName, BitmapName: string; AInstance: NativeUInt); overload;
    procedure AddFromResource(ResourceName, BitmapName: string); overload;
    procedure AddFromFile(FileName, BitmapName: String);
    {$IFNDEF WEBLIB}
    procedure AddFromFolder(AFolder: string);
    {$ENDIF}
    property BitmapNames[AIndex: Integer]: String read GetBitmapName;
    property Bitmaps[AIndex: Integer]: TTMSFNCBitmap read GetBitmap;
  published
    property Items: TTMSFNCBitmapCollection read GetItems write SetItems;
    property Version: string read GetVersion;
    property OnBitmapChanged: TNotifyEvent read FOnBitmapChanged write FOnBitmapChanged;
  end;

implementation

uses
  TypInfo, FMX.Forms, SysUtils, FMX.TMSFNCUtils
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  ;

{$R 'TMSFNCBitmapContainer.res'}

{$IFDEF FMXLIB}
type
  TCustomFormHelper = class helper for TCustomForm
  private
    function GetControlsCount: Integer;
    function GetControls(AIndex: Integer): TWinControl;
  public
    property ControlsCount: Integer read GetControlsCount;
    property Controls[Index: Integer]: TWinControl read GetControls;
  end;
  {$ENDIF}

{ TTMSFNCBitmapItem }

procedure TTMSFNCBitmapItem.Assign(Source: TPersistent);
begin
  Name := (Source as TTMSFNCBitmapItem).Name;
  Tag := (Source as TTMSFNCBitmapItem).Tag;
  Bitmap.Assign((Source as TTMSFNCBitmapItem).Bitmap)
end;

constructor TTMSFNCBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TTMSFNCBitmap.Create;
  FBitmap.OnChange := DoBitmapChanged;
  FName := 'Item' + IntToStr(Collection.Count);
end;

destructor TTMSFNCBitmapItem.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TTMSFNCBitmapItem.DoBitmapChanged(Sender: TObject);
begin
  if (Collection is TTMSFNCBitmapCollection) then
  begin
    if Assigned((Collection as TTMSFNCBitmapCollection).FOwner) then
      (Collection as TTMSFNCBitmapCollection).FOwner.DoBitmapChanged(Self);
  end;
end;

function TTMSFNCBitmapItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  {$IFDEF WEBLIB}
  else
    Result := '';
  {$ENDIF}
  {$IFNDEF WEBLIB}
  else
    Result := inherited;
  {$ENDIF}
end;

procedure TTMSFNCBitmapItem.SetBitmap(const Value: TTMSFNCBitmap);
begin
  FBitmap.Assign(Value);
end;

{ TTMSFNCBitmapCollection }

function TTMSFNCBitmapCollection.Add: TTMSFNCBitmapItem;
begin
  Result := TTMSFNCBitmapItem(inherited Add);
end;

constructor TTMSFNCBitmapCollection.Create(AOwner: TTMSFNCBitmapContainer);
begin
  inherited Create(AOwner, GetBitmapItemClass);
  FOwner := AOwner;
end;

function TTMSFNCBitmapCollection.GetBitmapItemClass: TCollectionItemClass;
begin
  Result := TTMSFNCBitmapItem;
end;

function TTMSFNCBitmapCollection.GetItemEx(Index: Integer): TTMSFNCBitmapItem;
begin
  Result := TTMSFNCBitmapItem(inherited Items[Index]);
end;

function TTMSFNCBitmapCollection.Insert(index: Integer): TTMSFNCBitmapItem;
begin
  Result := TTMSFNCBitmapItem(inherited Insert(Index));
end;

procedure TTMSFNCBitmapCollection.SetItemEx(Index: Integer; const Value: TTMSFNCBitmapItem);
begin
  inherited SetItem(Index, Value);
end;

{ TTMSFNCBitmapContainer }

procedure TTMSFNCBitmapContainer.AddFromFile(FileName, BitmapName: string);
var
  bmpi: TTMSFNCBitmapItem;
begin
  bmpi := Items.Add;
  bmpi.Bitmap.LoadFromFile(FileName);
  bmpi.Name := BitmapName;
end;

{$IFNDEF WEBLIB}
procedure TTMSFNCBitmapContainer.AddFromFolder(AFolder: string);
var
  SR: TSearchRec;

  procedure AddToList(s: string);
  begin
    with Items.Add do
    begin
      try
        Bitmap.LoadFromFile(s);
        Name := ExtractFileName(s);
      except
        Bitmap.Assign(nil);
      end;
    end;
  end;

begin
  if FindFirst(AFolder,faAnyFile and not faDirectory,SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name);
    while FindNext(SR) = 0 do
      AddToList(ExtractFilePath(AFolder) + SR.Name);
  end;
  FindClose(SR);
end;
{$ENDIF}

procedure TTMSFNCBitmapContainer.AddFromURL(URL, BitmapName: string);
{$IFDEF WEBLIB}
var
  bmpi: TTMSFNCBitmapItem;
{$ENDIF}
begin
  {$IFDEF WEBLIB}
  bmpi := Self.Items.Add;
  bmpi.Bitmap.LoadFromURL(URL);
  bmpi.Name := BitmapName;
  {$ENDIF}
end;

procedure TTMSFNCBitmapContainer.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCBitmapContainer) then
  begin
    FItems.Assign((Source as TTMSFNCBitmapContainer).Items);
  end
  else
    inherited;
end;

procedure TTMSFNCBitmapContainer.AddFromResource(ResourceName, BitmapName: string; AInstance: NativeUInt);
var
  bmpi: TTMSFNCBitmapItem;
begin
  bmpi := Items.Add;
  bmpi.Bitmap.LoadFromResource(ResourceName, AInstance);
  bmpi.Name := BitmapName;
end;

procedure TTMSFNCBitmapContainer.AddFromResource(ResourceName, BitmapName: string);
begin
  AddFromResource(ResourceName, BitmapName, GetInstance);
end;

procedure TTMSFNCBitmapContainer.Changed;
var
  i: integer;
begin
  for i := 0 to FControls.Count - 1 do
  {$IFDEF FMXLIB}
    FControls.Items[i].Repaint;
  {$ENDIF}
  {$IFDEF CMNLIB}
    FControls.Items[i].Invalidate;
  {$ENDIF}
end;

constructor TTMSFNCBitmapContainer.Create(AOwner: TComponent);
begin
  inherited;
  FItems := CreateItems;
  FControls := TControlList.Create;
end;

constructor TTMSFNCBitmapContainer.Create;
begin
  Create(nil);
end;

function TTMSFNCBitmapContainer.CreateItems: TTMSFNCBitmapCollection;
begin
  Result := TTMSFNCBitmapCollection.Create(Self);
end;

destructor TTMSFNCBitmapContainer.Destroy;
begin
  FreeAndNil(FControls);
  FItems.Free;
  inherited;
end;

procedure TTMSFNCBitmapContainer.DoBitmapChanged(Sender: TObject);
var
  f: TCustomForm;
  I: Integer;
begin
  f := TTMSFNCUtils.GetParentForm(Self);
  if Assigned(f) then
  begin
    for I := 0 to f.ControlsCount - 1 do
      if f.Controls[I] is TWinControl then
        InvalidateMembers(f.Controls[I] as TWinControl);
  end;

  if Assigned(OnBitmapChanged) then
    OnBitmapChanged(Self);
end;

procedure TTMSFNCBitmapContainer.FindBitmap(i: Integer; ABitmap: TTMSFNCBitmap);
begin
  if (i >= 0) and (i <= Items.Count - 1) then
    ABitmap.Assign(Items[I].Bitmap);
end;

function TTMSFNCBitmapContainer.FindBitmap(s: string): TTMSFNCBitmap;
var
  i: Integer;
begin
  Result := nil;
  s := Uppercase(s);
  i := 1;
  while i <= Items.Count do
  begin
    if Uppercase(Items[i - 1].Name) = s then
    begin
      Result := Items[i - 1].Bitmap;
      Break;
    end;
    Inc(i);
  end;
end;

procedure TTMSFNCBitmapContainer.SetItems(const Value: TTMSFNCBitmapCollection);
begin
  FItems.Assign(Value);
end;

function TTMSFNCBitmapContainer.GetBitmap(AIndex: Integer): TTMSFNCBitmap;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= Items.Count - 1) then
    Result := Items[AIndex].Bitmap;
end;

function TTMSFNCBitmapContainer.GetBitmapName(AIndex: Integer): String;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex <= Items.Count - 1) then
    Result := Items[AIndex].Name;
end;

function TTMSFNCBitmapContainer.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

function TTMSFNCBitmapContainer.GetItem(AIndex: Integer): TTMSFNCBitmapItem;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= ItemCount - 1) then
    Result := Items[AIndex];
end;

function TTMSFNCBitmapContainer.GetItems: TTMSFNCBitmapCollection;
begin
  Result := FItems;
end;

function TTMSFNCBitmapContainer.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TTMSFNCBitmapContainer.InvalidateMembers(AControl: TWinControl);
var
  I: Integer;
begin
  if not Assigned(AControl) then
    Exit;

  if Assigned(GetPropInfo(AControl, 'BitmapContainer')) then
    AControl.Repaint;

  for I := 0 to AControl.ControlsCount - 1 do
  begin
    if AControl.Controls[I] is TWinControl then
      InvalidateMembers(AControl.Controls[I] as TWinControl);
  end;
end;

function TTMSFNCBitmapContainer.ItemCount: Integer;
begin
  Result := Items.Count;
end;

procedure TTMSFNCBitmapContainer.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
  inherited;
  if (AOperation = opRemove) and Assigned(FControls) then
  begin
    for i := FControls.Count - 1 downto 0 do
    begin
      if (FControls.Items[i] = AComponent) then
        FControls.Delete(i);
    end;
  end;
end;

function TTMSFNCBitmapContainer.RandomBitmap: TTMSFNCBitmap;
begin
  Result := Bitmaps[Random(ItemCount)];
end;

function TTMSFNCBitmapContainer.RandomBitmapName: string;
begin
  Result := BitmapNames[Random(ItemCount)];
end;

procedure TTMSFNCBitmapContainer.RegisterControl(
  AControl: TControl);
begin
  FControls.Add(AControl);
end;

procedure TTMSFNCBitmapContainer.RegisterRuntimeClasses;
begin
  RegisterClass(TTMSFNCBitmapContainer);
end;

{$IFDEF FMXLIB}

{ TCustomFormHelper }

function TCustomFormHelper.GetControls(AIndex: Integer): TWinControl;
var
  c: TFmxObject;
begin
  Result := nil;
  c := Children[AIndex];
  if c is TWinControl then
    Result := c as TWinControl;
end;

function TCustomFormHelper.GetControlsCount: Integer;
begin
  Result := ChildrenCount;
end;

{$ENDIF}

end.
