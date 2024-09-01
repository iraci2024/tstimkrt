{**************************************************************************}
{ TADVSTRINGGRID FORM CONTROL EDITLINK                                     }
{                                                                          }
{ written by TMS software                                                  }
{            copyright � 2000 - 2022                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}

unit frmctrllink;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, Graphics, Forms, SysUtils,
  Dialogs, AdvGrid;

type

  TSetEditorValueEvent = procedure(Sender: TObject; Grid: TAdvStringGrid; AValue: string) of object;
  TGetEditorValueEvent = procedure(Sender: TObject; Grid: TAdvStringGrid; var AValue: string) of object;
  TSetEditorProperties = procedure(Sender: TObject; Grid: TAdvStringGrid; AControl: TWinControl) of object;
  TSetEditorFocus = procedure(Sender: TObject; Grid: TAdvStringGrid; AControl: TWinControl) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFormControlEditLink = class(TEditLink)
  private
    FControl: TWinControl;
    FOnGetEditorValue: TGetEditorValueEvent;
    FOnSetEditorValue: TSetEditorValueEvent;
    FOnSetEditorProperties: TSetEditorProperties;
    FOnSetEditorFocus: TSetEditorFocus;
    procedure SetControl(const Value: TWinControl);
  protected
    procedure EditExit(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateEditor(AParent:TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
    procedure HideEditor; override;
    function GetEditorValue: string; override;
    procedure SetEditorValue(s: string); override;
    procedure SetFocus(Value: Boolean); override;
  published
    property Control: TWinControl read FControl write SetControl;
    property OnSetEditorValue: TSetEditorValueEvent read FOnSetEditorValue write FOnSetEditorValue;
    property OnSetEditorFocus: TSetEditorFocus read FOnSetEditorFocus write FOnSetEditorFocus;
    property OnGetEditorValue: TGetEditorValueEvent read FOnGetEditorValue write FOnGetEditorValue;
    property OnSetEditorProperties: TSetEditorProperties read FOnSetEditorProperties write FOnSetEditorProperties;
  end;


implementation

type
  TMyWinControl = class(TWinControl)
  published
  end;


{ TFormControlEditLink }

constructor TFormControlEditLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControl := nil;
end;

procedure TFormControlEditLink.CreateEditor(AParent: TWinControl);
begin
  if Assigned(FControl) then
    FControl.Parent := AParent;
end;

procedure TFormControlEditLink.DestroyEditor;
begin
  if Assigned(FControl) and (EditStyle = esPopup) then
  begin
    FControl.Parent := (Owner as TWinControl);
    FControl.Hide;
  end;
  inherited;
end;

procedure TFormControlEditLink.EditExit(Sender: TObject);
var
  pt: TPoint;
  grid: TAdvStringGrid;
begin
  grid := (GetParent as TAdvStringGrid);

  grid.HideInplaceEdit;
  FControl.Hide;

  GetCursorPos(pt);

  pt := grid.ScreenToClient(pt);

  if PtInRect(grid.ClientRect,pt) then
  begin
    if grid.CanFocus then
      grid.SetFocus;
  end;
end;

function TFormControlEditLink.GetEditControl: TWinControl;
begin
  if not Assigned(FControl) then
    raise Exception.Create('FormControlEditLink control not assigned');

  Result := FControl;
  TMyWinControl(FControl).OnExit := EditExit;
  TMyWinControl(FControl).OnKeyDown := EditKeyDown;
end;

function TFormControlEditLink.GetEditorValue: String;
begin
  if Assigned(FOnGetEditorValue) then
    FOnGetEditorValue(Self,Grid,Result);
end;

procedure TFormControlEditLink.HideEditor;
begin
  inherited;
end;

procedure TFormControlEditLink.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FControl) then
  begin
    FControl := nil;
  end;
end;

procedure TFormControlEditLink.SetControl(const Value: TWinControl);
begin
  FControl := Value;
  if EditStyle = esPopup then
  begin
    if Assigned(FControl) and (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      if PopupWidth = 0 then
        PopupWidth := FControl.Width;
      if PopupHeight = 0 then
        PopupHeight := FControl.Height;
    end;
  end;
end;

procedure TFormControlEditLink.SetEditorValue(s: String);
begin
  if Assigned(FOnSetEditorValue) then
    FOnSetEditorValue(Self,Grid,s);
end;

procedure TFormControlEditLink.SetFocus(Value: Boolean);
begin
  inherited;
  if Value then
    if Assigned(FOnSetEditorFocus) then
      FOnSetEditorFocus(Self,Grid,FControl);
end;

procedure TFormControlEditLink.SetProperties;
begin
  inherited;
  if Assigned(FOnSetEditorProperties) then
    FOnSetEditorProperties(Self,Grid,FControl);
end;

End.



