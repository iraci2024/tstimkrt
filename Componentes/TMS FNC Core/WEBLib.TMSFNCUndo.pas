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

unit WEBLib.TMSFNCUndo;

{$I WEBLib.TMSFNCDefines.inc}

interface

uses
  Classes, WEBLib.TMSFNCTypes;

type
  TTMSFNCUndoStackItem = class(TCollectionItem)
  private
    FActionName: string;
    FState: TStream;
    FObj: TObject;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property ActionName: string read FActionName write FActionName;
    property Obj: TObject read FObj write FObj;
    property State: TStream read FState;
  end;

  TTMSFNCUndoManager = class(TCollection)
  private
    FCurrent: integer;
    FObject: TObject;
    FMaxStackCount: Integer;
    function GetStackItem(Index: Integer): TTMSFNCUndoStackItem;
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
    property Stack[Index: Integer]: TTMSFNCUndoStackItem read GetStackItem; default;
  end;

implementation

uses
  SysUtils,
  WEBLib.TMSFNCPersistence;

{ TTMSFNCUndoStackItem }

constructor TTMSFNCUndoStackItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FState := TMemoryStream.Create;
end;

destructor TTMSFNCUndoStackItem.Destroy;
begin
  FState.Free;
  inherited;
end;

{ TTMSFNCUndoManager }

function TTMSFNCUndoManager.CanRedo: Boolean;
begin
  Result := (FCurrent < Count - 1) and (Count > 0);
end;

function TTMSFNCUndoManager.CanUndo: Boolean;
begin
  Result := (FCurrent > 0) and (Count > 0);
end;

procedure TTMSFNCUndoManager.ClearUndoStack;
begin
  Clear;
  FCurrent := -1;
  PushState('');
end;

constructor TTMSFNCUndoManager.Create(AObject: TObject);
begin
  inherited Create(TTMSFNCUndoStackItem);
  FObject := AObject;
  FMaxStackCount := 20;
  FCurrent := -1;
end;

function TTMSFNCUndoManager.GetStackItem(Index: Integer): TTMSFNCUndoStackItem;
begin
  Result := nil;
  if (Index > -1) and (Index < Count) then
    Result := TTMSFNCUndoStackItem(Items[Index]);
end;

function TTMSFNCUndoManager.NextUndoAction: string;
begin
  Result := '';
  if (FCurrent > -1) and (FCurrent < Count) then
    Result := TTMSFNCUndoStackItem(Items[FCurrent]).ActionName;
end;

function TTMSFNCUndoManager.NextRedoAction: string;
begin
  Result := '';
  if (FCurrent >= -1) and (FCurrent < Count - 1) then
    Result := TTMSFNCUndoStackItem(Items[FCurrent + 1]).ActionName;
end;

procedure TTMSFNCUndoManager.Undo;
var
  p: ITMSFNCPersistence;
begin
  if (FCurrent > 0) and (FCurrent <= Count) then
  begin
    with TTMSFNCUndoStackItem(Items[FCurrent - 1]) do
    begin
      FState.Position := 0;
      if Supports(FObject, ITMSFNCPersistence, p) then
        p.LoadSettingsFromStream(FState)
      else
        TTMSFNCPersistence.LoadSettingsFromStream(FObject, FState);
    end;
    Dec(FCurrent);
  end;
end;

procedure TTMSFNCUndoManager.Redo;
var
  p: ITMSFNCPersistence;
begin
  if (FCurrent >= -1) and (FCurrent < Count - 1) then
  begin
    with TTMSFNCUndoStackItem(Items[FCurrent + 1]) do
    begin
      FState.Position := 0;
      if Supports(FObject, ITMSFNCPersistence, p) then
        p.LoadSettingsFromStream(FState)
      else
        TTMSFNCPersistence.LoadSettingsFromStream(FObject, FState);
    end;
    Inc(FCurrent);
  end;
end;

procedure TTMSFNCUndoManager.PushState(const AActionName: string);
var
  p: ITMSFNCPersistence;
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

  with TTMSFNCUndoStackItem(Add) do
  begin
    ActionName := AActionName;
    if Supports(FObject, ITMSFNCPersistence, p) then
      p.SaveSettingsToStream(FState)
    else
      TTMSFNCPersistence.SaveSettingsToStream(FObject, FState);
    Inc(FCurrent);
  end;
  if Count > MaxStackCount then
  begin
    Delete(0);
    dec(FCurrent);
  end;
end;

end.
