{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
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

unit AdvUndo;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvTypes;

type
  TAdvUndoStackItem = class(TCollectionItem)
  private
    FActionName: string;
    FState: TStream;
    FObj: TObject;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property ActionName: string read FActionName write FActionName;
    property Obj: TObject read FObj write FObj;
    property State: TStream read FState;
  end;

  TAdvUndoManager = class(TCollection)
  private
    FCurrent: integer;
    FObject: TObject;
    FMaxStackCount: Integer;
    function GetStackItem(Index: Integer): TAdvUndoStackItem;
  public
    function NextUndoAction: string;
    function NextRedoAction: string;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure Undo;
    procedure Redo;
    procedure ClearUndoStack;
    procedure PushState(const {%H-}AActionName: string);
    constructor Create(AObject: TObject);
    property MaxStackCount: Integer read FMaxStackCount write FMaxStackCount default 20;
    property Stack[Index: Integer]: TAdvUndoStackItem read GetStackItem; default;
  end;

implementation

uses
  SysUtils,
  AdvPersistence;

{ TAdvUndoStackItem }

constructor TAdvUndoStackItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := TMemoryStream.Create;
end;

destructor TAdvUndoStackItem.Destroy;
begin
  FState.Free;
  inherited;
end;

{ TAdvUndoManager }

function TAdvUndoManager.CanRedo: Boolean;
begin
  Result := (FCurrent < Count - 1) and (Count > 0);
end;

function TAdvUndoManager.CanUndo: Boolean;
begin
  Result := (FCurrent > 0) and (Count > 0);
end;

procedure TAdvUndoManager.ClearUndoStack;
begin
  Clear;
  FCurrent := -1;
  PushState('');
end;

constructor TAdvUndoManager.Create(AObject: TObject);
begin
  inherited Create(TAdvUndoStackItem);
  FObject := AObject;
  FMaxStackCount := 20;
  FCurrent := -1;
end;

function TAdvUndoManager.GetStackItem(Index: Integer): TAdvUndoStackItem;
begin
  Result := nil;
  if (Index > -1) and (Index < Count) then
    Result := TAdvUndoStackItem(Items[Index]);
end;

function TAdvUndoManager.NextUndoAction: string;
begin
  Result := '';
  if (FCurrent > -1) and (FCurrent < Count) then
    Result := TAdvUndoStackItem(Items[FCurrent]).ActionName;
end;

function TAdvUndoManager.NextRedoAction: string;
begin
  Result := '';
  if (FCurrent >= -1) and (FCurrent < Count - 1) then
    Result := TAdvUndoStackItem(Items[FCurrent + 1]).ActionName;
end;

procedure TAdvUndoManager.Undo;
var
  p: IAdvPersistence;
begin
  if (FCurrent > 0) and (FCurrent <= Count) then
  begin
    with TAdvUndoStackItem(Items[FCurrent - 1]) do
    begin
      FState.Position := 0;
      if Supports(FObject, IAdvPersistence, p) then
        p.LoadSettingsFromStream(FState)
      else
        TAdvPersistence.LoadSettingsFromStream(FObject, FState);
    end;
    Dec(FCurrent);
  end;
end;

procedure TAdvUndoManager.Redo;
var
  p: IAdvPersistence;
begin
  if (FCurrent >= -1) and (FCurrent < Count - 1) then
  begin
    with TAdvUndoStackItem(Items[FCurrent + 1]) do
    begin
      FState.Position := 0;
      if Supports(FObject, IAdvPersistence, p) then
        p.LoadSettingsFromStream(FState)
      else
        TAdvPersistence.LoadSettingsFromStream(FObject, FState);
    end;
    Inc(FCurrent);
  end;
end;

procedure TAdvUndoManager.PushState(const AActionName: string);
var
  p: IAdvPersistence;
  it: TCollectionItem;
begin
  while FCurrent < Count - 1 do
  begin
    it := Items[Count - 1];
    {$IFDEF FMXLIB}
    it.DisposeOf;
    {$ENDIF}
    {$IFNDEF FMXLIB}
    it.Free;
    {$ENDIF}
  end;

  with TAdvUndoStackItem(Add) do
  begin
    ActionName := AActionName;
    if Supports(FObject, IAdvPersistence, p) then
      p.SaveSettingsToStream(FState)
    else
      TAdvPersistence.SaveSettingsToStream(FObject, FState);
    Inc(FCurrent);
  end;
  if Count > MaxStackCount then
  begin
    Delete(0);
    dec(FCurrent);
  end;
end;

end.
