program Basic;

uses
  Forms,
  uMain in 'uMain.pas' {fmMain},
  uProgress in 'uProgress.pas' {frmProgress};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.Run;
end.
