program ScrollbarAnnotationsDemoD100Seattle;

uses
  Forms,
  CustomAnnotationSettings in 'CustomAnnotationSettings.pas' {frmCustomAnnotationSettings},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  ScrollbarAnnotationsDemoMain in 'ScrollbarAnnotationsDemoMain.pas' {ScrollbarAnnotationsDemoMainForm},
  ScrollbarAnnotationsDemoData in 'ScrollbarAnnotationsDemoData.pas' {ScrollbarAnnotationsDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TScrollbarAnnotationsDemoDataDM, ScrollbarAnnotationsDemoDataDM);
  Application.CreateForm(TScrollbarAnnotationsDemoMainForm, ScrollbarAnnotationsDemoMainForm);
  Application.Run;
end.
