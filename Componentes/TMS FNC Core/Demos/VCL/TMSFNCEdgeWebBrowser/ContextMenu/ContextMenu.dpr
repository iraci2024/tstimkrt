program ContextMenu;

uses
  Vcl.Forms,
  UContextMenu in 'UContextMenu.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
