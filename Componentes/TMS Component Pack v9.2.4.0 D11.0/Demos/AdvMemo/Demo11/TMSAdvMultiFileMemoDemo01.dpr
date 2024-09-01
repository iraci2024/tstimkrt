program TMSAdvMultiFileMemoDemo01;

uses
  Vcl.Forms,
  UAdvMultiFileMemoDemo01 in 'UAdvMultiFileMemoDemo01.pas' {TFAdvMultiFileMemoDemo01};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTFAdvMultiFileMemoDemo01, TFAdvMultiFileMemoDemo01);
  Application.Run;
end.
