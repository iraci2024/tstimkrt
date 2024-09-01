{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
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

unit FMX.TMSFNCToggleButton;

{$I FMX.TMSFNCDefines.inc}
{$IFDEF LCLLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
  {$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, FMX.TMSFNCTypes, FMX.Controls, FMX.StdCtrls
  {$IFDEF VCLLIB}
  , Winapi.Messages, Vcl.ActnList
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
  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsWeb)]
  {$ENDIF}
  {$IFNDEF WEBLIB}
  TTMSFNCToggleButton = class(TButton)
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCToggleButton = class(TWebButton)
  {$ENDIF}
  {$IFDEF LCLWEBLIB}
  private
    FChecked: Boolean;
    FOnClick: TNotifyEvent;
  protected
    procedure DoClick;
    function GetIsPressed: Boolean;
    procedure SetIsPressed(Value: Boolean);
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property isPressed: Boolean read GetIsPressed write SetIsPressed;
  {$ENDIF}
  {$IFDEF FMXLIB}
  public
    constructor Create(AOwner: TComponent); override;
  {$ENDIF}
  {$IFDEF VCLLIB}
  private
    FOnClick: TNotifyEvent;
    FChecked: Boolean;
    FGroupIndex: Integer;
    procedure Toggle;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetGroupIndex(const Value: Integer);
    procedure TurnSiblingsOff;
  protected
    procedure DoClick;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetIsPressed: Boolean;
    procedure SetIsPressed(Value: Boolean);
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property isPressed: Boolean read GetIsPressed write SetIsPressed;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
  {$ENDIF}
  end;

implementation

uses
  TypInfo, FMX.Forms, SysUtils, FMX.TMSFNCUtils
  {$IFDEF VCLLIB}
  , Winapi.Windows
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Types
  {$ENDIF}
  {$IFDEF WEBLIB}
  , WEBLIB.Graphics
  {$ENDIF}
  {$IFDEF LCLLIB}
  , Graphics
  {$ENDIF}
  ;

{$IFDEF LCLWEBLIB}
constructor TTMSFNCToggleButton.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TTMSFNCToggleButton.Click;
begin
  FChecked := not FChecked;

  if FChecked then
    Color := clSkyBlue
  else
    Color := clNone;

  DoClick;
end;

procedure TTMSFNCToggleButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

function TTMSFNCToggleButton.GetIsPressed: Boolean;
begin
  Result := FChecked;
end;

procedure TTMSFNCToggleButton.SetIsPressed(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;

    if FChecked then
      Color := clSkyBlue
    else
      Color := clNone;

    Invalidate;
  end;
end;
{$ENDIF}

{$IFDEF FMXLIB}
constructor TTMSFNCToggleButton.Create(AOwner: TComponent);
begin
  inherited;
  StaysPressed := True;
end;
{$ENDIF}

{$IFDEF VCLLIB}
procedure TTMSFNCToggleButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TTMSFNCToggleButton.Toggle;
begin
  Checked := not FChecked;
  DoClick;
end;

procedure TTMSFNCToggleButton.SetButtonStyle(ADefault: Boolean);
begin
  { do nothing - avoid setting style to BS_PUSHBUTTON }
end;

procedure TTMSFNCToggleButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_PUSHLIKE  or BS_CHECKBOX;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TTMSFNCToggleButton.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
end;

procedure TTMSFNCToggleButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Toggle
  else
    inherited;
end;

function TTMSFNCToggleButton.GetIsPressed: Boolean;
begin
  Result := FChecked;
end;

procedure TTMSFNCToggleButton.SetIsPressed(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
    if Value then
      TurnSiblingsOff;
    if not ClicksDisabled then Click;
  end;
end;

procedure TTMSFNCToggleButton.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  if Checked then
    TurnSiblingsOff;
end;

procedure TTMSFNCToggleButton.TurnSiblingsOff;
var
  I: Integer;
  Sibling: TControl;
begin
  if (Parent <> nil) and (GroupIndex <> 0) then
    with Parent do
      for I := 0 to ControlCount - 1 do
      begin
        Sibling := Controls[I];
        if (Sibling <> Self) and (Sibling is TTMSFNCToggleButton) then
          with TTMSFNCToggleButton(Sibling) do
            if GroupIndex = Self.GroupIndex then
            begin
              if Assigned(Action) and
                 (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                TCustomAction(Action).Checked := False;
              SetChecked(False);
            end;
      end;
end;
{$ENDIF}

end.
