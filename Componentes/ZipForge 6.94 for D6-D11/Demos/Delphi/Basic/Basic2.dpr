program Basic2;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fmMain},
  uProgress in 'uProgress.pas' {frmProgress};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.Run;
end.
