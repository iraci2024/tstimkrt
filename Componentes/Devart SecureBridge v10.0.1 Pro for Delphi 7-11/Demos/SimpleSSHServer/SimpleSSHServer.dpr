program SimpleSSHServer;

uses
  Forms,
  SecureBridgeAbout in '..\Base\SecureBridgeAbout.pas' {SecureBridgeAboutForm},
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SSHServerMain in 'SSHServerMain.pas' {fmSSHServerForm},
  SSHServerFrame in 'SSHServerFrame\SSHServerFrame.pas' {frSSHServerFrame},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSHServerForm, fmSSHServerForm);
  Application.CreateForm(TSecureBridgeAboutForm, SecureBridgeAboutForm);
  Application.Run;
end.
