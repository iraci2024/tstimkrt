{$I ..\Base\SBDemo.inc}

unit SSHClientDemoForm;

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
  TSSHClientForm = class(TDemoForm)
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
  SSHClientForm: TSSHClientForm;

implementation

uses
  SSHClientFrame;

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

function TSSHClientForm.ProductColor: TColor;
begin
  Result := $0000BBFF;
end;

function TSSHClientForm.ApplicationTitle: string;
begin
  Result := 'SecureBridge';
end;

procedure TSSHClientForm.RegisterDemos;
begin
  Demos.RegisterDemo('SSHClientFrame', 'SSH Client', 'SSHClientFrame', '', TfrSSHClientFrame, 3);
end;

procedure TSSHClientForm.lbAboutClick(Sender: TObject);
begin
  inherited;
  SecureBridgeAboutForm.ShowModal;
end;

end.
