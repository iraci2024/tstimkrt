program Advanced;

uses
  Forms,
  ProgressIndicator in 'ProgressIndicator.pas' {FormProgress},
  uMain in 'uMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TFormProgress, FormProgress);
  Application.Run;
end.
