program ScrollbarAnnotationsDemoD101Berlin;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars: TDataModule},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  ScrollbarAnnotationsDemoMain in 'ScrollbarAnnotationsDemoMain.pas' {ScrollbarAnnotationsDemoMainForm},
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
