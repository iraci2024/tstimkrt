program SSHClient;

uses
  Forms,
  SecureBridgeAbout in '..\Base\SecureBridgeAbout.pas' {SecureBridgeAboutForm},
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SSHClientFrame in 'SSHClientFrame\SSHClientFrame.pas' {frSSHClientFrame},
  SSHClientDemoForm in 'SSHClientDemoForm.pas' {SSHClientForm},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas',
  PromptForm in '..\Base\PromptForm.pas' {fmPrompt},
  NewNameForm in '..\Base\NewNameForm.pas' {fmNewName},
  FileViewForm in '..\Base\FileViewForm.pas' {fmFileView},
  SettingsForm in 'SettingsForm.pas' {fmSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TSSHClientForm, SSHClientForm);
  Application.CreateForm(TSecureBridgeAboutForm, SecureBridgeAboutForm);
  Application.CreateForm(TfmPrompt, fmPrompt);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.CreateForm(TfmNewName, fmNewName);
  Application.CreateForm(TfmFileView, fmFileView);
  Application.Run;
end.
