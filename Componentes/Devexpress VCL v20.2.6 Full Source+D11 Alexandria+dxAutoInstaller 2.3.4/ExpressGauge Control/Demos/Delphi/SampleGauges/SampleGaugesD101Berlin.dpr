program SampleGaugesD101Berlin;

uses
  Forms,
  uSampleGauges in 'uSampleGauges.pas' {frmSampleGauges};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Sample Gauges ';
  Application.CreateForm(TfrmSampleGauges, frmSampleGauges);
  Application.Run;
end.
