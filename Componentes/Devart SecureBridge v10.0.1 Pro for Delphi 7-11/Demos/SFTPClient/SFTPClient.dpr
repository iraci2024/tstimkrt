program SFTPClient;

uses
  Forms,
  SecureBridgeAbout in '..\Base\SecureBridgeAbout.pas' {SecureBridgeAboutForm},
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SFTPClientFrame in 'SFTPClientFrame\SFTPClientFrame.pas' {SFTPClientFrame},
  SFTPClientDemoForm in 'SFTPClientDemoForm.pas' {SFTPClientForm},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas',
  PromptForm in '..\Base\PromptForm.pas' {fmPrompt},
  NewNameForm in '..\Base\NewNameForm.pas' {fmNewName},
  FileViewForm in '..\Base\FileViewForm.pas' {fmFileView};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TSFTPClientForm, SFTPClientForm);
  Application.CreateForm(TSecureBridgeAboutForm, SecureBridgeAboutForm);
  Application.CreateForm(TfmPrompt, fmPrompt);
  Application.CreateForm(TfmNewName, fmNewName);
  Application.CreateForm(TfmFileView, fmFileView);
  Application.Run;
end.
