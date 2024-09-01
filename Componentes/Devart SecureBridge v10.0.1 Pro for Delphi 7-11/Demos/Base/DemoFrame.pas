unit DemoFrame;

interface

uses
  Classes, {$IFNDEF VER130}Variants,{$ENDIF}
  Forms, Windows, Messages, SysUtils, StdCtrls, Graphics, Controls, Dialogs,
  ComCtrls, ShellAPI, Buttons, ExtCtrls;

type
  TDemoFrame = class(TFrame)
  public
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure Activate; virtual;
  end;

  TDemoFrameClass = class of TDemoFrame;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TDemoFrame.Initialize;
begin
end;

procedure TDemoFrame.Finalize;
begin
end;

procedure TDemoFrame.Activate;
begin
end;

end.
