{$I ..\Base\SBDemo.inc}

unit SSHServerMain;

interface

uses
  SysUtils, Classes,
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  SecureBridgeAbout, DemoForm, DemoBase;

type
  TSSHServerForm = class(TDemoForm)
    procedure lbAboutClick(Sender: TObject); override;
  private
    { Private declarations }
  protected
    //Product customization
    function ApplicationTitle: string; override;
    procedure RegisterDemos; override;
  public
    function ProductColor: TColor; override;
  end;

var
  fmSSHServerForm: TSSHServerForm;

implementation

uses
  SSHServerFrame;

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

function TSSHServerForm.ProductColor: TColor;
begin
  Result := $0000BBFF;
end;

function TSSHServerForm.ApplicationTitle: string;
begin
  Result := 'SecureBridge demos';
end;

procedure TSSHServerForm.RegisterDemos;
begin
  Demos.RegisterDemo('SSHServerFrame', 'SSH Server', 'SSHServerFrame', '', TSSHServerFrame, 3);
end;

procedure TSSHServerForm.lbAboutClick(Sender: TObject);
begin
  inherited;
  SecureBridgeAboutForm.ShowModal;
end;

end.
