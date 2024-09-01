{$I ..\Base\SBDemo.inc}

unit SFTPClientDemoForm;

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
  TSFTPClientForm = class(TDemoForm)
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
  SFTPClientForm: TSFTPClientForm;

implementation

uses
  SFTPClientFrame;

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

function TSFTPClientForm.ProductColor: TColor;
begin
  Result := $0000BBFF;
end;

function TSFTPClientForm.ApplicationTitle: string;
begin
  Result := 'SecureBridge demos';
end;

procedure TSFTPClientForm.RegisterDemos;
begin
  Demos.RegisterDemo('SFTPClientFrame', 'SFTP Client', 'SFTPClientFrame', '', TSFTPClientFrame, 0);
end;

procedure TSFTPClientForm.lbAboutClick(Sender: TObject);
begin
  inherited;
  SecureBridgeAboutForm.ShowModal;
end;

end.
