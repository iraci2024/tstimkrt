{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2016 - 2022                               }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvCheckedTreeView;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvTreeView, AdvTreeViewData
  {$IFNDEF LCLLIB}
  ,AdvTypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Improved : Update initial look

type
  TAdvCheckedTreeViewNodes = class;

  TAdvCheckedTreeViewNodeValue = class(TAdvTreeViewNodeValue)
  public
    constructor Create(Collection: TCollection); override;
  end;

  TAdvCheckedTreeViewNodeValues = class(TAdvTreeViewNodeValues)
  protected
    function GetItemClass: TCollectionItemClass; override;
  end;

  TAdvCheckedTreeViewNode = class(TAdvTreeViewNode)
  protected
    function CreateNodeValues: TAdvTreeViewNodeValues; override;
    function CreateNodes: TAdvTreeViewNodes; override;
  end;

  TAdvTreeViewCheckedNodes = array of TAdvTreeViewNode;

  TAdvCheckedTreeViewNodes = class(TAdvTreeViewNodes)
  protected
    function GetItemClass: TCollectionItemClass; override;
    function CheckedNodesInternal(AColumn: Integer = 0; ARecurse: Boolean = True): TAdvTreeViewCheckedNodes; virtual;
  public
    function CheckedNodes(AColumn: Integer = 0; ARecurse: Boolean = True): TAdvTreeViewCheckedNodes; virtual;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  TAdvCheckedTreeView = class(TAdvTreeView)
  private
    function GetChecked(ANode: TAdvTreeViewNode): Boolean;
    procedure SetChecked(ANode: TAdvTreeViewNode; const Value: Boolean);
  protected
    function CreateNodes: TAdvTreeViewNodes; override;
    function GetVersion: string; override;
  public
    procedure InitSample; override;
    property Checked[ANode: TAdvTreeViewNode]: Boolean read GetChecked write SetChecked;
    function CheckedNodes(AColumn: Integer = 0; ARecurse: Boolean = True): TAdvTreeViewCheckedNodes; virtual;
  end;

implementation

uses
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  UITypes,
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  AdvUtils, AdvGraphicsTypes;

{ TAdvCheckedTreeView }

function TAdvCheckedTreeView.CheckedNodes(AColumn: Integer;
  ARecurse: Boolean): TAdvTreeViewCheckedNodes;
begin
  Result := TAdvCheckedTreeViewNodes(Nodes).CheckedNodes(AColumn, ARecurse);
end;

function TAdvCheckedTreeView.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvCheckedTreeViewNodes.Create(Self, nil);
end;

function TAdvCheckedTreeView.GetChecked(ANode: TAdvTreeViewNode): Boolean;
begin
  Result := False;
  if Assigned(ANode) then
    Result := ANode.Checked[0];
end;

function TAdvCheckedTreeView.GetVersion: string;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvCheckedTreeView.InitSample;
var
  pManagers, pSpecialists, pAssistants, pSub: TAdvTreeViewNode;
  c: TAdvTreeViewColumn;
  I: Integer;
  n: string;
begin
 BeginUpdate;

  Width := ScalePaintValue(350);

  ClearNodeList;
  Columns.Clear;
  Nodes.Clear;

  ResetToDefaultStyle;

  c := Columns.Add;
  c.Text := 'Name';
  c.HorizontalTextAlign := gtaLeading;

  c := Columns.Add;
  c.Text := 'Unit';
  c.HorizontalTextAlign := gtaTrailing;
  c.Width := ScalePaintValue(90);

  c := Columns.Add;
  c.Text := 'Status';
  c.HorizontalTextAlign := gtaCenter;
  c.Width := ScalePaintValue(80);

  pManagers := AddNode;
  pManagers.Text[0] := 'Managers';
  pManagers.CheckTypes[0] := tvntCheckBox;

  pSpecialists := AddNode;
  pSpecialists.Text[0] := 'Specialists';
  pSpecialists.CheckTypes[0] := tvntCheckBox;

  pAssistants := AddNode;
  pAssistants.Text[0] := 'Assistants';
  pAssistants.CheckTypes[0] := tvntCheckBox;

  for I := 0 to 7 do
  begin
    if I < 3 then
      pSub := AddNode(pManagers)
    else if I < 5  then
      pSub := AddNode(pSpecialists)
    else
      pSub := AddNode(pAssistants);

    pSub.CheckTypes[0] := tvntCheckBox;
    pSub.CheckTypes[1] := tvntNone;
    pSub.CheckTypes[2] := tvntNone;

    case Random(125) mod 7 of
      0: n := 'Liam';
      1: n := 'Fatma';
      2: n := 'Yusuf';
      3: n := 'Marie';
      4: n := 'Isabella';
      5: n := 'Omar';
      else
        n := 'Arthur';
    end;

    case Random(125) mod 7 of
      0: n := n + ' Andersson';
      1: n := n + ' Wang';
      2: n := n + ' Smith';
      3: n := n + ' Peeters';
      4: n := n + ' Gonzales';
      5: n := n + ' Moyo';
      else
        n := n + ' Ali';
    end;

    pSub.Text[0] := n;

    case Random(120) mod 7 of
      1: pSub.Text[1] := 'Research';
      2: pSub.Text[1] := 'Finance';
      3: pSub.Text[1] := 'Development';
      4: pSub.Text[1] := 'Sales';
      5: pSub.Text[1] := 'HR';
      6: pSub.Text[1] := 'Marketing';
      else
        pSub.Text[1] := 'Customers';
    end;

    case Random(120) mod 5 of
      0: pSub.Text[2] := 'Flight';
      1: pSub.Text[2] := 'On Leave';
      2: pSub.Text[2] := 'Abroad';
      else
        pSub.Text[2] := 'Office';
    end;
  end;

  pManagers.Expanded := True;
  pSpecialists.Expanded := True;

  GlobalFont.Name := 'Segoe UI';

  ColumnsAppearance.StretchAll := False;
  ColumnsAppearance.Stretch := True;
  ColumnsAppearance.StretchColumn := 0;

  EndUpdate;
end;

procedure TAdvCheckedTreeView.SetChecked(ANode: TAdvTreeViewNode;
  const Value: Boolean);
begin
  if Assigned(ANode) then
    ANode.Checked[0] := Value;
end;

{ TAdvCheckedTreeViewNode }

function TAdvCheckedTreeViewNode.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvCheckedTreeViewNodes.Create(TreeView, Self);
end;

function TAdvCheckedTreeViewNode.CreateNodeValues: TAdvTreeViewNodeValues;
begin
  Result := TAdvCheckedTreeViewNodeValues.Create(TreeView, Self);
end;

{ TAdvCheckedTreeViewNodes }

function TAdvCheckedTreeViewNodes.CheckedNodes(AColumn: Integer;
  ARecurse: Boolean): TAdvTreeViewCheckedNodes;
begin
  Result := CheckedNodesInternal(AColumn, ARecurse);
end;

function TAdvCheckedTreeViewNodes.CheckedNodesInternal(AColumn: Integer; ARecurse: Boolean): TAdvTreeViewCheckedNodes;
var
  I: Integer;
  td: TAdvTreeViewData;
  n: TAdvTreeViewNode;
  k: Integer;
  a: TAdvTreeViewCheckedNodes;
begin
  Result := nil;
  td := TreeView;
  if not Assigned(td) then
    Exit;

  for I := 0 to Count - 1 do
  begin
    n := Items[I];
    if TAdvCheckedTreeView(td).Checked[n] then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := n;
    end;

    if ARecurse then
    begin
      a := TAdvCheckedTreeViewNodes(n.Nodes).CheckedNodesInternal(AColumn, ARecurse);
      for K := 0 to Length(a) - 1 do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := a[K];
      end;
    end;
  end;
end;

function TAdvCheckedTreeViewNodes.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvCheckedTreeViewNode;
end;

{ TAdvCheckedTreeViewNodeValue }

constructor TAdvCheckedTreeViewNodeValue.Create(Collection: TCollection);
var
  t: TAdvTreeViewData;
begin
  inherited;
  t := TreeView;
  if Assigned(t) and t.IsDesigntime then
    CheckType := tvntCheckBox;
end;

{ TAdvCheckedTreeViewNodeValues }

function TAdvCheckedTreeViewNodeValues.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvCheckedTreeViewNodeValue;
end;

end.

