program ScrollbarAnnotationsDemoDXE5;

uses
  Forms,
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  ScrollbarAnnotationsDemoMain in 'ScrollbarAnnotationsDemoMain.pas' {ScrollbarAnnotationsDemoMainForm},
  CarsData in '..\Common\CarsData.pas' {dmCars: TDataModule},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  CustomAnnotationSettings in 'CustomAnnotationSettings.pas' {frmCustomAnnotationSettings};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TScrollbarAnnotationsDemoMainForm, ScrollbarAnnotationsDemoMainForm);
  Application.Run;
end.
