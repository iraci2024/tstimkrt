program CarDashboardD101Berlin;

uses
  Forms,
  uCarDashboard in 'uCarDashboard.pas' {frmCarDashboard};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCarDashboard, frmCarDashboard);
  Application.Run;
end.
