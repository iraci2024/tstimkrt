program FormattedLabelDemoDXE6;

uses
  Forms,
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  FormattedLabelDemoMain in 'FormattedLabelDemoMain.pas' {dxFormattedLabelDemoForm},
  HyperlinkDialog in 'HyperlinkDialog.pas' {fmHyperlinkDialog};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdxFormattedLabelDemoForm, dxFormattedLabelDemoForm);
  Application.Run;
end.
