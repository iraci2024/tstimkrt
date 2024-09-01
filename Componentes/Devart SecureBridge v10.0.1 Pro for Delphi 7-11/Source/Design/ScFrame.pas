
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Base Frame
//////////////////////////////////////////////////

{$I SB.inc}

unit ScFrame;

interface

uses
  Controls, ComCtrls, Forms,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, ScEditor;

type
  TScFrameClass = class of TScFrame;

  TScFrame = class(TFrame)
    procedure FrameEnter(Sender: TObject);
  protected
    FModified: boolean;
    FEditor: TScEditorForm;
    FActivated: boolean; // To avoid duplicate call (for example, on TScFrame.FrameExit and PageControl.OnChanging events)

    function GetPage: TTabSheet;
    procedure DoActivate; virtual;
    procedure DoFinish; virtual;

  public
    function ActiveControl: TWinControl; virtual; // Return default control for this frame
    procedure Activate;
    procedure Finish;
    procedure ReActivate;

    property Activated: boolean read FActivated;
    property Page: TTabSheet read GetPage;
    property Editor: TScEditorForm read FEditor write FEditor;
    property Modified: boolean read FModified write FModified;

  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TScFrame }

function TScFrame.ActiveControl: TWinControl;
begin
  Result := nil;
end;

function TScFrame.GetPage: TTabSheet;
begin
  Result := Parent as TTabSheet;
end;

procedure TScFrame.DoActivate;
begin
end;

procedure TScFrame.DoFinish;
begin
end;

procedure TScFrame.Activate;
begin
  if not FActivated then
    ReActivate;
end;

procedure TScFrame.Finish;
begin
  if FActivated then
    DoFinish;
  FActivated := False;
end;

procedure TScFrame.FrameEnter(Sender: TObject);
begin
  Activate;
end;

procedure TScFrame.ReActivate;
begin
  DoActivate;
  FActivated := True;
end;

end.
