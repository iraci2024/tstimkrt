program SSHServer;

uses
  Forms,
  SecureBridgeAbout in '..\Base\SecureBridgeAbout.pas' {SecureBridgeAboutForm},
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SSHServerMain in 'SSHServerMain.pas' {fmSSHServerForm},
  SSHServerFrame in 'SSHServerFrame\SSHServerFrame.pas' {frSSHServerFrame},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas',
  ScColFrame in 'SSHServerFrame\ScColFrame.pas' {ScColFrame: TFrame},
  ScKeysFrame in 'SSHServerFrame\ScKeysFrame.pas' {ScKeysFrame: TFrame},
  ScUsersFrame in 'SSHServerFrame\ScUsersFrame.pas' {ScUsersFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSHServerForm, fmSSHServerForm);
  Application.CreateForm(TSecureBridgeAboutForm, SecureBridgeAboutForm);
  Application.Run;
end.
