
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  SecureBridge About Window
//////////////////////////////////////////////////

unit SecureBridgeAbout;

interface
uses
  Jpeg, Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi;

type
  TSecureBridgeAboutForm = class(TForm)
    OKBtn: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbMail: TLabel;
    lbWeb: TLabel;
    Label10: TLabel;
    lbForum: TLabel;
    Image1: TImage;
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbForumClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SecureBridgeAboutForm: TSecureBridgeAboutForm;

procedure ShowAbout;

implementation

{$R *.dfm}

procedure ShowAbout;
begin
  with TSecureBridgeAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TSecureBridgeAboutForm.lbWebClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'http://www.devart.com/sbridge', '', '', SW_SHOW);
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TSecureBridgeAboutForm.lbMailClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'mailto:sbridge@devart.com', 'zxczxc', '', SW_SHOW);
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TSecureBridgeAboutForm.lbForumClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'http://forums.devart.com/viewforum.php?f=27', '', '', SW_SHOW);
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TSecureBridgeAboutForm.lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TSecureBridgeAboutForm.lbMailMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TSecureBridgeAboutForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
end;

end.
