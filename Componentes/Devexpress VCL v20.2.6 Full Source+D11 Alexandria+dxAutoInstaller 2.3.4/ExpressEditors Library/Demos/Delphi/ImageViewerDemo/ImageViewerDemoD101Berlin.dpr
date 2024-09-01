program ImageViewerDemoD101Berlin;

uses
  Forms,
  BaseForm in '..\BaseForm.pas',
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  ImageViewerDemoMain in 'ImageViewerDemoMain.pas' {ImageViewerDemoMainForm},
  ImageViewerDemoResizeImage in 'ImageViewerDemoResizeImage.pas' {ImageViewerDemoResizeImageForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TImageViewerDemoMainForm, ImageViewerDemoMainForm);
  Application.CreateForm(TImageViewerDemoResizeImageForm, ImageViewerDemoResizeImageForm);
  Application.Run;
end.
