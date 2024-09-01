program SSHClient;

uses
  Forms,
  SecureBridgeAbout in '..\Base\SecureBridgeAbout.pas' {SecureBridgeAboutForm},
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SSHLocalPF in 'LocalPortForwarding\SSHLocalPF.pas' {frSSHLocalPF},
  SSHClientDemoForm in 'SSHClientDemoForm.pas' {SSHClientForm},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas',
  PromptForm in '..\Base\PromptForm.pas' {fmPrompt};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TSSHClientForm, SSHClientForm);
  Application.CreateForm(TSecureBridgeAboutForm, SecureBridgeAboutForm);
  Application.CreateForm(TfmPrompt, fmPrompt);
  Application.Run;
end.
