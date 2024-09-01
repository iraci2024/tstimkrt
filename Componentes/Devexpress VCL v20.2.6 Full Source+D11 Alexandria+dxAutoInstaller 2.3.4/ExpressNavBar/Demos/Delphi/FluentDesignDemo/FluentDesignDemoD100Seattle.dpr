program FluentDesignDemoD100Seattle;

uses
  Forms,
  FluentDesignDemoMainForm in 'FluentDesignDemoMainForm.pas' {frmFluentDesignDemo: TdxFluentDesignForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}

{$R dxDPIAwareManifest.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFluentDesignDemo, frmFluentDesignDemo);
  Application.Run;
end.
